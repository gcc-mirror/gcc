/* Loop splitting.
   Copyright (C) 2015-2019 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "fold-const.h"
#include "tree-cfg.h"
#include "tree-ssa.h"
#include "tree-ssa-loop-niter.h"
#include "tree-ssa-loop.h"
#include "tree-ssa-loop-manip.h"
#include "tree-into-ssa.h"
#include "tree-inline.h"
#include "tree-cfgcleanup.h"
#include "cfgloop.h"
#include "tree-scalar-evolution.h"
#include "gimple-iterator.h"
#include "gimple-pretty-print.h"
#include "cfghooks.h"
#include "gimple-fold.h"
#include "gimplify-me.h"

/* This file implements two kinds of loop splitting.

   One transformation of loops like:

   for (i = 0; i < 100; i++)
     {
       if (i < 50)
         A;
       else
         B;
     }

   into:

   for (i = 0; i < 50; i++)
     {
       A;
     }
   for (; i < 100; i++)
     {
       B;
     }

   */

/* Return true when BB inside LOOP is a potential iteration space
   split point, i.e. ends with a condition like "IV < comp", which
   is true on one side of the iteration space and false on the other,
   and the split point can be computed.  If so, also return the border
   point in *BORDER and the comparison induction variable in IV.  */

static tree
split_at_bb_p (class loop *loop, basic_block bb, tree *border, affine_iv *iv)
{
  gimple *last;
  gcond *stmt;
  affine_iv iv2;

  /* BB must end in a simple conditional jump.  */
  last = last_stmt (bb);
  if (!last || gimple_code (last) != GIMPLE_COND)
    return NULL_TREE;
  stmt = as_a <gcond *> (last);

  enum tree_code code = gimple_cond_code (stmt);

  /* Only handle relational comparisons, for equality and non-equality
     we'd have to split the loop into two loops and a middle statement.  */
  switch (code)
    {
      case LT_EXPR:
      case LE_EXPR:
      case GT_EXPR:
      case GE_EXPR:
	break;
      default:
	return NULL_TREE;
    }

  if (loop_exits_from_bb_p (loop, bb))
    return NULL_TREE;

  tree op0 = gimple_cond_lhs (stmt);
  tree op1 = gimple_cond_rhs (stmt);
  class loop *useloop = loop_containing_stmt (stmt);

  if (!simple_iv (loop, useloop, op0, iv, false))
    return NULL_TREE;
  if (!simple_iv (loop, useloop, op1, &iv2, false))
    return NULL_TREE;

  /* Make it so that the first argument of the condition is
     the looping one.  */
  if (!integer_zerop (iv2.step))
    {
      std::swap (op0, op1);
      std::swap (*iv, iv2);
      code = swap_tree_comparison (code);
      gimple_cond_set_condition (stmt, code, op0, op1);
      update_stmt (stmt);
    }
  else if (integer_zerop (iv->step))
    return NULL_TREE;
  if (!integer_zerop (iv2.step))
    return NULL_TREE;
  if (!iv->no_overflow)
    return NULL_TREE;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Found potential split point: ");
      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
      fprintf (dump_file, " { ");
      print_generic_expr (dump_file, iv->base, TDF_SLIM);
      fprintf (dump_file, " + I*");
      print_generic_expr (dump_file, iv->step, TDF_SLIM);
      fprintf (dump_file, " } %s ", get_tree_code_name (code));
      print_generic_expr (dump_file, iv2.base, TDF_SLIM);
      fprintf (dump_file, "\n");
    }

  *border = iv2.base;
  return op0;
}

/* Given a GUARD conditional stmt inside LOOP, which we want to make always
   true or false depending on INITIAL_TRUE, and adjusted values NEXTVAL
   (a post-increment IV) and NEWBOUND (the comparator) adjust the loop
   exit test statement to loop back only if the GUARD statement will
   also be true/false in the next iteration.  */

static void
patch_loop_exit (class loop *loop, gcond *guard, tree nextval, tree newbound,
		 bool initial_true)
{
  edge exit = single_exit (loop);
  gcond *stmt = as_a <gcond *> (last_stmt (exit->src));
  gimple_cond_set_condition (stmt, gimple_cond_code (guard),
			     nextval, newbound);
  update_stmt (stmt);

  edge stay = EDGE_SUCC (exit->src, EDGE_SUCC (exit->src, 0) == exit);

  exit->flags &= ~(EDGE_TRUE_VALUE | EDGE_FALSE_VALUE);
  stay->flags &= ~(EDGE_TRUE_VALUE | EDGE_FALSE_VALUE);

  if (initial_true)
    {
      exit->flags |= EDGE_FALSE_VALUE;
      stay->flags |= EDGE_TRUE_VALUE;
    }
  else
    {
      exit->flags |= EDGE_TRUE_VALUE;
      stay->flags |= EDGE_FALSE_VALUE;
    }
}

/* Give an induction variable GUARD_IV, and its affine descriptor IV,
   find the loop phi node in LOOP defining it directly, or create
   such phi node.  Return that phi node.  */

static gphi *
find_or_create_guard_phi (class loop *loop, tree guard_iv, affine_iv * /*iv*/)
{
  gimple *def = SSA_NAME_DEF_STMT (guard_iv);
  gphi *phi;
  if ((phi = dyn_cast <gphi *> (def))
      && gimple_bb (phi) == loop->header)
    return phi;

  /* XXX Create the PHI instead.  */
  return NULL;
}

/* Returns true if the exit values of all loop phi nodes can be
   determined easily (i.e. that connect_loop_phis can determine them).  */

static bool
easy_exit_values (class loop *loop)
{
  edge exit = single_exit (loop);
  edge latch = loop_latch_edge (loop);
  gphi_iterator psi;

  /* Currently we regard the exit values as easy if they are the same
     as the value over the backedge.  Which is the case if the definition
     of the backedge value dominates the exit edge.  */
  for (psi = gsi_start_phis (loop->header); !gsi_end_p (psi); gsi_next (&psi))
    {
      gphi *phi = psi.phi ();
      tree next = PHI_ARG_DEF_FROM_EDGE (phi, latch);
      basic_block bb;
      if (TREE_CODE (next) == SSA_NAME
	  && (bb = gimple_bb (SSA_NAME_DEF_STMT (next)))
	  && !dominated_by_p (CDI_DOMINATORS, exit->src, bb))
	return false;
    }

  return true;
}

/* This function updates the SSA form after connect_loops made a new
   edge NEW_E leading from LOOP1 exit to LOOP2 (via in intermediate
   conditional).  I.e. the second loop can now be entered either
   via the original entry or via NEW_E, so the entry values of LOOP2
   phi nodes are either the original ones or those at the exit
   of LOOP1.  Insert new phi nodes in LOOP2 pre-header reflecting
   this.  The loops need to fulfill easy_exit_values().  */

static void
connect_loop_phis (class loop *loop1, class loop *loop2, edge new_e)
{
  basic_block rest = loop_preheader_edge (loop2)->src;
  gcc_assert (new_e->dest == rest);
  edge skip_first = EDGE_PRED (rest, EDGE_PRED (rest, 0) == new_e);

  edge firste = loop_preheader_edge (loop1);
  edge seconde = loop_preheader_edge (loop2);
  edge firstn = loop_latch_edge (loop1);
  gphi_iterator psi_first, psi_second;
  for (psi_first = gsi_start_phis (loop1->header),
       psi_second = gsi_start_phis (loop2->header);
       !gsi_end_p (psi_first);
       gsi_next (&psi_first), gsi_next (&psi_second))
    {
      tree init, next, new_init;
      use_operand_p op;
      gphi *phi_first = psi_first.phi ();
      gphi *phi_second = psi_second.phi ();

      init = PHI_ARG_DEF_FROM_EDGE (phi_first, firste);
      next = PHI_ARG_DEF_FROM_EDGE (phi_first, firstn);
      op = PHI_ARG_DEF_PTR_FROM_EDGE (phi_second, seconde);
      gcc_assert (operand_equal_for_phi_arg_p (init, USE_FROM_PTR (op)));

      /* Prefer using original variable as a base for the new ssa name.
	 This is necessary for virtual ops, and useful in order to avoid
	 losing debug info for real ops.  */
      if (TREE_CODE (next) == SSA_NAME
	  && useless_type_conversion_p (TREE_TYPE (next),
					TREE_TYPE (init)))
	new_init = copy_ssa_name (next);
      else if (TREE_CODE (init) == SSA_NAME
	       && useless_type_conversion_p (TREE_TYPE (init),
					     TREE_TYPE (next)))
	new_init = copy_ssa_name (init);
      else if (useless_type_conversion_p (TREE_TYPE (next),
					  TREE_TYPE (init)))
	new_init = make_temp_ssa_name (TREE_TYPE (next), NULL,
				       "unrinittmp");
      else
	new_init = make_temp_ssa_name (TREE_TYPE (init), NULL,
				       "unrinittmp");

      gphi * newphi = create_phi_node (new_init, rest);
      add_phi_arg (newphi, init, skip_first, UNKNOWN_LOCATION);
      add_phi_arg (newphi, next, new_e, UNKNOWN_LOCATION);
      SET_USE (op, new_init);
    }
}

/* The two loops LOOP1 and LOOP2 were just created by loop versioning,
   they are still equivalent and placed in two arms of a diamond, like so:

               .------if (cond)------.
               v                     v
             pre1                   pre2
              |                      |
        .--->h1                     h2<----.
        |     |                      |     |
        |    ex1---.            .---ex2    |
        |    /     |            |     \    |
        '---l1     X            |     l2---'
                   |            |
                   |            |
                   '--->join<---'

   This function transforms the program such that LOOP1 is conditionally
   falling through to LOOP2, or skipping it.  This is done by splitting
   the ex1->join edge at X in the diagram above, and inserting a condition
   whose one arm goes to pre2, resulting in this situation:

               .------if (cond)------.
               v                     v
             pre1       .---------->pre2
              |         |            |
        .--->h1         |           h2<----.
        |     |         |            |     |
        |    ex1---.    |       .---ex2    |
        |    /     v    |       |     \    |
        '---l1   skip---'       |     l2---'
                   |            |
                   |            |
                   '--->join<---'


   The condition used is the exit condition of LOOP1, which effectively means
   that when the first loop exits (for whatever reason) but the real original
   exit expression is still false the second loop will be entered.
   The function returns the new edge cond->pre2.

   This doesn't update the SSA form, see connect_loop_phis for that.  */

static edge
connect_loops (class loop *loop1, class loop *loop2)
{
  edge exit = single_exit (loop1);
  basic_block skip_bb = split_edge (exit);
  gcond *skip_stmt;
  gimple_stmt_iterator gsi;
  edge new_e, skip_e;

  gimple *stmt = last_stmt (exit->src);
  skip_stmt = gimple_build_cond (gimple_cond_code (stmt),
				 gimple_cond_lhs (stmt),
				 gimple_cond_rhs (stmt),
				 NULL_TREE, NULL_TREE);
  gsi = gsi_last_bb (skip_bb);
  gsi_insert_after (&gsi, skip_stmt, GSI_NEW_STMT);

  skip_e = EDGE_SUCC (skip_bb, 0);
  skip_e->flags &= ~EDGE_FALLTHRU;
  new_e = make_edge (skip_bb, loop_preheader_edge (loop2)->src, 0);
  if (exit->flags & EDGE_TRUE_VALUE)
    {
      skip_e->flags |= EDGE_TRUE_VALUE;
      new_e->flags |= EDGE_FALSE_VALUE;
    }
  else
    {
      skip_e->flags |= EDGE_FALSE_VALUE;
      new_e->flags |= EDGE_TRUE_VALUE;
    }

  new_e->probability = profile_probability::likely ();
  skip_e->probability = new_e->probability.invert ();

  return new_e;
}

/* This returns the new bound for iterations given the original iteration
   space in NITER, an arbitrary new bound BORDER, assumed to be some
   comparison value with a different IV, the initial value GUARD_INIT of
   that other IV, and the comparison code GUARD_CODE that compares
   that other IV with BORDER.  We return an SSA name, and place any
   necessary statements for that computation into *STMTS.

   For example for such a loop:

     for (i = beg, j = guard_init; i < end; i++, j++)
       if (j < border)  // this is supposed to be true/false
         ...

   we want to return a new bound (on j) that makes the loop iterate
   as long as the condition j < border stays true.  We also don't want
   to iterate more often than the original loop, so we have to introduce
   some cut-off as well (via min/max), effectively resulting in:

     newend = min (end+guard_init-beg, border)
     for (i = beg; j = guard_init; j < newend; i++, j++)
       if (j < c)
         ...

   Depending on the direction of the IVs and if the exit tests
   are strict or non-strict we need to use MIN or MAX,
   and add or subtract 1.  This routine computes newend above.  */

static tree
compute_new_first_bound (gimple_seq *stmts, class tree_niter_desc *niter,
			 tree border,
			 enum tree_code guard_code, tree guard_init)
{
  /* The niter structure contains the after-increment IV, we need
     the loop-enter base, so subtract STEP once.  */
  tree controlbase = force_gimple_operand (niter->control.base,
					   stmts, true, NULL_TREE);
  tree controlstep = niter->control.step;
  tree enddiff;
  if (POINTER_TYPE_P (TREE_TYPE (controlbase)))
    {
      controlstep = gimple_build (stmts, NEGATE_EXPR,
				  TREE_TYPE (controlstep), controlstep);
      enddiff = gimple_build (stmts, POINTER_PLUS_EXPR,
			      TREE_TYPE (controlbase),
			      controlbase, controlstep);
    }
  else
    enddiff = gimple_build (stmts, MINUS_EXPR,
			    TREE_TYPE (controlbase),
			    controlbase, controlstep);

  /* Compute end-beg.  */
  gimple_seq stmts2;
  tree end = force_gimple_operand (niter->bound, &stmts2,
					true, NULL_TREE);
  gimple_seq_add_seq_without_update (stmts, stmts2);
  if (POINTER_TYPE_P (TREE_TYPE (enddiff)))
    {
      tree tem = gimple_convert (stmts, sizetype, enddiff);
      tem = gimple_build (stmts, NEGATE_EXPR, sizetype, tem);
      enddiff = gimple_build (stmts, POINTER_PLUS_EXPR,
			      TREE_TYPE (enddiff),
			      end, tem);
    }
  else
    enddiff = gimple_build (stmts, MINUS_EXPR, TREE_TYPE (enddiff),
			    end, enddiff);

  /* Compute guard_init + (end-beg).  */
  tree newbound;
  enddiff = gimple_convert (stmts, TREE_TYPE (guard_init), enddiff);
  if (POINTER_TYPE_P (TREE_TYPE (guard_init)))
    {
      enddiff = gimple_convert (stmts, sizetype, enddiff);
      newbound = gimple_build (stmts, POINTER_PLUS_EXPR,
			       TREE_TYPE (guard_init),
			       guard_init, enddiff);
    }
  else
    newbound = gimple_build (stmts, PLUS_EXPR, TREE_TYPE (guard_init),
			     guard_init, enddiff);

  /* Depending on the direction of the IVs the new bound for the first
     loop is the minimum or maximum of old bound and border.
     Also, if the guard condition isn't strictly less or greater,
     we need to adjust the bound.  */ 
  int addbound = 0;
  enum tree_code minmax;
  if (niter->cmp == LT_EXPR)
    {
      /* GT and LE are the same, inverted.  */
      if (guard_code == GT_EXPR || guard_code == LE_EXPR)
	addbound = -1;
      minmax = MIN_EXPR;
    }
  else
    {
      gcc_assert (niter->cmp == GT_EXPR);
      if (guard_code == GE_EXPR || guard_code == LT_EXPR)
	addbound = 1;
      minmax = MAX_EXPR;
    }

  if (addbound)
    {
      tree type2 = TREE_TYPE (newbound);
      if (POINTER_TYPE_P (type2))
	type2 = sizetype;
      newbound = gimple_build (stmts,
			       POINTER_TYPE_P (TREE_TYPE (newbound))
			       ? POINTER_PLUS_EXPR : PLUS_EXPR,
			       TREE_TYPE (newbound),
			       newbound,
			       build_int_cst (type2, addbound));
    }

  tree newend = gimple_build (stmts, minmax, TREE_TYPE (border),
			      border, newbound);
  return newend;
}

/* Checks if LOOP contains an conditional block whose condition
   depends on which side in the iteration space it is, and if so
   splits the iteration space into two loops.  Returns true if the
   loop was split.  NITER must contain the iteration descriptor for the
   single exit of LOOP.  */

static bool
split_loop (class loop *loop1)
{
  class tree_niter_desc niter;
  basic_block *bbs;
  unsigned i;
  bool changed = false;
  tree guard_iv;
  tree border = NULL_TREE;
  affine_iv iv;

  if (!single_exit (loop1)
      /* ??? We could handle non-empty latches when we split the latch edge
	 (not the exit edge), and put the new exit condition in the new block.
	 OTOH this executes some code unconditionally that might have been
	 skipped by the original exit before.  */
      || !empty_block_p (loop1->latch)
      || !easy_exit_values (loop1)
      || !number_of_iterations_exit (loop1, single_exit (loop1), &niter,
				     false, true)
      || niter.cmp == ERROR_MARK
      /* We can't yet handle loops controlled by a != predicate.  */
      || niter.cmp == NE_EXPR)
    return false;

  bbs = get_loop_body (loop1);

  if (!can_copy_bbs_p (bbs, loop1->num_nodes))
    {
      free (bbs);
      return false;
    }

  /* Find a splitting opportunity.  */
  for (i = 0; i < loop1->num_nodes; i++)
    if ((guard_iv = split_at_bb_p (loop1, bbs[i], &border, &iv)))
      {
	/* Handling opposite steps is not implemented yet.  Neither
	   is handling different step sizes.  */
	if ((tree_int_cst_sign_bit (iv.step)
	     != tree_int_cst_sign_bit (niter.control.step))
	    || !tree_int_cst_equal (iv.step, niter.control.step))
	  continue;

	/* Find a loop PHI node that defines guard_iv directly,
	   or create one doing that.  */
	gphi *phi = find_or_create_guard_phi (loop1, guard_iv, &iv);
	if (!phi)
	  continue;
	gcond *guard_stmt = as_a<gcond *> (last_stmt (bbs[i]));
	tree guard_init = PHI_ARG_DEF_FROM_EDGE (phi,
						 loop_preheader_edge (loop1));
	enum tree_code guard_code = gimple_cond_code (guard_stmt);

	/* Loop splitting is implemented by versioning the loop, placing
	   the new loop after the old loop, make the first loop iterate
	   as long as the conditional stays true (or false) and let the
	   second (new) loop handle the rest of the iterations.

	   First we need to determine if the condition will start being true
	   or false in the first loop.  */
	bool initial_true;
	switch (guard_code)
	  {
	    case LT_EXPR:
	    case LE_EXPR:
	      initial_true = !tree_int_cst_sign_bit (iv.step);
	      break;
	    case GT_EXPR:
	    case GE_EXPR:
	      initial_true = tree_int_cst_sign_bit (iv.step);
	      break;
	    default:
	      gcc_unreachable ();
	  }

	/* Build a condition that will skip the first loop when the
	   guard condition won't ever be true (or false).  */
	gimple_seq stmts2;
	border = force_gimple_operand (border, &stmts2, true, NULL_TREE);
	if (stmts2)
	  gsi_insert_seq_on_edge_immediate (loop_preheader_edge (loop1),
					    stmts2);
	tree cond = build2 (guard_code, boolean_type_node, guard_init, border);
	if (!initial_true)
	  cond = fold_build1 (TRUTH_NOT_EXPR, boolean_type_node, cond); 

	/* Now version the loop, placing loop2 after loop1 connecting
	   them, and fix up SSA form for that.  */
	initialize_original_copy_tables ();
	basic_block cond_bb;

	class loop *loop2 = loop_version (loop1, cond, &cond_bb,
					   profile_probability::always (),
					   profile_probability::always (),
					   profile_probability::always (),
					   profile_probability::always (),
					   true);
	gcc_assert (loop2);
	update_ssa (TODO_update_ssa);

	edge new_e = connect_loops (loop1, loop2);
	connect_loop_phis (loop1, loop2, new_e);

	/* The iterations of the second loop is now already
	   exactly those that the first loop didn't do, but the
	   iteration space of the first loop is still the original one.
	   Compute the new bound for the guarding IV and patch the
	   loop exit to use it instead of original IV and bound.  */
	gimple_seq stmts = NULL;
	tree newend = compute_new_first_bound (&stmts, &niter, border,
					       guard_code, guard_init);
	if (stmts)
	  gsi_insert_seq_on_edge_immediate (loop_preheader_edge (loop1),
					    stmts);
	tree guard_next = PHI_ARG_DEF_FROM_EDGE (phi, loop_latch_edge (loop1));
	patch_loop_exit (loop1, guard_stmt, guard_next, newend, initial_true);

	/* Finally patch out the two copies of the condition to be always
	   true/false (or opposite).  */
	gcond *force_true = as_a<gcond *> (last_stmt (bbs[i]));
	gcond *force_false = as_a<gcond *> (last_stmt (get_bb_copy (bbs[i])));
	if (!initial_true)
	  std::swap (force_true, force_false);
	gimple_cond_make_true (force_true);
	gimple_cond_make_false (force_false);
	update_stmt (force_true);
	update_stmt (force_false);

	free_original_copy_tables ();

	/* We destroyed LCSSA form above.  Eventually we might be able
	   to fix it on the fly, for now simply punt and use the helper.  */
	rewrite_into_loop_closed_ssa_1 (NULL, 0, SSA_OP_USE, loop1);

	changed = true;
	if (dump_file && (dump_flags & TDF_DETAILS))
	  fprintf (dump_file, ";; Loop split.\n");

	/* Only deal with the first opportunity.  */
	break;
      }

  free (bbs);
  return changed;
}

/* Another transformation of loops like:

   for (i = INIT (); CHECK (i); i = NEXT ())
     {
       if (expr (a_1, a_2, ..., a_n))  // expr is pure
         a_j = ...;  // change at least one a_j
       else
         S;          // not change any a_j
     }

   into:

   for (i = INIT (); CHECK (i); i = NEXT ())
     {
       if (expr (a_1, a_2, ..., a_n))
         a_j = ...;
       else
         {
           S;
           i = NEXT ();
           break;
         }
     }

   for (; CHECK (i); i = NEXT ())
     {
       S;
     }

   */

/* Data structure to hold temporary information during loop split upon
   semi-invariant conditional statement.  */
class split_info {
public:
  /* Array of all basic blocks in a loop, returned by get_loop_body().  */
  basic_block *bbs;

  /* All memory store/clobber statements in a loop.  */
  auto_vec<gimple *> memory_stores;

  /* Whether above memory stores vector has been filled.  */
  int need_init;

  /* Control dependencies of basic blocks in a loop.  */
  auto_vec<hash_set<basic_block> *> control_deps;

  split_info () : bbs (NULL),  need_init (true) { }

  ~split_info ()
    {
      if (bbs)
	free (bbs);

      for (unsigned i = 0; i < control_deps.length (); i++)
	delete control_deps[i];
    }
};

/* Find all statements with memory-write effect in LOOP, including memory
   store and non-pure function call, and keep those in a vector.  This work
   is only done one time, for the vector should be constant during analysis
   stage of semi-invariant condition.  */

static void
find_vdef_in_loop (struct loop *loop)
{
  split_info *info = (split_info *) loop->aux;
  gphi *vphi = get_virtual_phi (loop->header);

  /* Indicate memory store vector has been filled.  */
  info->need_init = false;

  /* If loop contains memory operation, there must be a virtual PHI node in
     loop header basic block.  */
  if (vphi == NULL)
    return;

  /* All virtual SSA names inside the loop are connected to be a cyclic
     graph via virtual PHI nodes.  The virtual PHI node in loop header just
     links the first and the last virtual SSA names, by using the last as
     PHI operand to define the first.  */
  const edge latch = loop_latch_edge (loop);
  const tree first = gimple_phi_result (vphi);
  const tree last = PHI_ARG_DEF_FROM_EDGE (vphi, latch);

  /* The virtual SSA cyclic graph might consist of only one SSA name, who
     is defined by itself.

       .MEM_1 = PHI <.MEM_2(loop entry edge), .MEM_1(latch edge)>

     This means the loop contains only memory loads, so we can skip it.  */
  if (first == last)
    return;

  auto_vec<gimple *> other_stores;
  auto_vec<tree> worklist;
  auto_bitmap visited;

  bitmap_set_bit (visited, SSA_NAME_VERSION (first));
  bitmap_set_bit (visited, SSA_NAME_VERSION (last));
  worklist.safe_push (last);

  do
    {
      tree vuse = worklist.pop ();
      gimple *stmt = SSA_NAME_DEF_STMT (vuse);

      /* We mark the first and last SSA names as visited at the beginning,
	 and reversely start the process from the last SSA name towards the
	 first, which ensures that this do-while will not touch SSA names
	 defined outside the loop.  */
      gcc_assert (gimple_bb (stmt)
		  && flow_bb_inside_loop_p (loop, gimple_bb (stmt)));

      if (gimple_code (stmt) == GIMPLE_PHI)
	{
	  gphi *phi = as_a <gphi *> (stmt);

	  for (unsigned i = 0; i < gimple_phi_num_args (phi); ++i)
	    {
	      tree arg = gimple_phi_arg_def (stmt, i);

	      if (bitmap_set_bit (visited, SSA_NAME_VERSION (arg)))
		worklist.safe_push (arg);
	    }
	}
      else
	{
	  tree prev = gimple_vuse (stmt);

	  /* Non-pure call statement is conservatively assumed to impact all
	     memory locations.  So place call statements ahead of other memory
	     stores in the vector with an idea of of using them as shortcut
	     terminators to memory alias analysis.  */
	  if (gimple_code (stmt) == GIMPLE_CALL)
	    info->memory_stores.safe_push (stmt);
	  else
	    other_stores.safe_push (stmt);

	  if (bitmap_set_bit (visited, SSA_NAME_VERSION (prev)))
	    worklist.safe_push (prev);
	}
    } while (!worklist.is_empty ());

    info->memory_stores.safe_splice (other_stores);
}

/* Two basic blocks have equivalent control dependency if one dominates to
   the other, and it is post-dominated by the latter.  Given a basic block
   BB in LOOP, find farest equivalent dominating basic block.  For BB, there
   is a constraint that BB does not post-dominate loop header of LOOP, this
   means BB is control-dependent on at least one basic block in LOOP.  */

static basic_block
get_control_equiv_head_block (struct loop *loop, basic_block bb)
{
  while (!bb->aux)
    {
      basic_block dom_bb = get_immediate_dominator (CDI_DOMINATORS, bb);

      gcc_checking_assert (dom_bb && flow_bb_inside_loop_p (loop, dom_bb));

      if (!dominated_by_p (CDI_POST_DOMINATORS, dom_bb, bb))
	break;

      bb = dom_bb;
    }
  return bb;
}

/* Given a BB in LOOP, find out all basic blocks in LOOP that BB is control-
   dependent on.  */

static hash_set<basic_block> *
find_control_dep_blocks (struct loop *loop, basic_block bb)
{
  /* BB has same control dependency as loop header, then it is not control-
     dependent on any basic block in LOOP.  */
  if (dominated_by_p (CDI_POST_DOMINATORS, loop->header, bb))
    return NULL;

  basic_block equiv_head = get_control_equiv_head_block (loop, bb);

  if (equiv_head->aux)
    {
      /* There is a basic block containing control dependency equivalent
	 to BB.  No need to recompute that, and also set this information
	 to other equivalent basic blocks.  */
      for (; bb != equiv_head;
	   bb = get_immediate_dominator (CDI_DOMINATORS, bb))
	bb->aux = equiv_head->aux;
      return (hash_set<basic_block> *) equiv_head->aux;
    }

  /* A basic block X is control-dependent on another Y iff there exists
     a path from X to Y, in which every basic block other than X and Y
     is post-dominated by Y, but X is not post-dominated by Y.

     According to this rule, traverse basic blocks in the loop backwards
     starting from BB, if a basic block is post-dominated by BB, extend
     current post-dominating path to this block, otherwise it is another
     one that BB is control-dependent on.  */

  auto_vec<basic_block> pdom_worklist;
  hash_set<basic_block> pdom_visited;
  hash_set<basic_block> *dep_bbs = new hash_set<basic_block>;

  pdom_worklist.safe_push (equiv_head);

  do
    {
      basic_block pdom_bb = pdom_worklist.pop ();
      edge_iterator ei;
      edge e;

      if (pdom_visited.add (pdom_bb))
	continue;

      FOR_EACH_EDGE (e, ei, pdom_bb->preds)
	{
	  basic_block pred_bb = e->src;

	  if (!dominated_by_p (CDI_POST_DOMINATORS, pred_bb, bb))
	    {
	      dep_bbs->add (pred_bb);
	      continue;
	    }

	  pred_bb = get_control_equiv_head_block (loop, pred_bb);

	  if (pdom_visited.contains (pred_bb))
	    continue;

	  if (!pred_bb->aux)
	    {
	      pdom_worklist.safe_push (pred_bb);
	      continue;
	    }

	  /* If control dependency of basic block is available, fast extend
	     post-dominating path using the information instead of advancing
	     forward step-by-step.  */
	  hash_set<basic_block> *pred_dep_bbs
			= (hash_set<basic_block> *) pred_bb->aux;

	  for (hash_set<basic_block>::iterator iter = pred_dep_bbs->begin ();
	       iter != pred_dep_bbs->end (); ++iter)
	    {
	      basic_block pred_dep_bb = *iter;

	      /* Basic blocks can either be in control dependency of BB, or
		 must be post-dominated by BB, if so, extend the path from
		 these basic blocks.  */
	      if (!dominated_by_p (CDI_POST_DOMINATORS, pred_dep_bb, bb))
		dep_bbs->add (pred_dep_bb);
	      else if (!pdom_visited.contains (pred_dep_bb))
		pdom_worklist.safe_push (pred_dep_bb);
	    }
	}
    } while (!pdom_worklist.is_empty ());

  /* Record computed control dependencies in loop so that we can reach them
     when reclaiming resources.  */
  ((split_info *) loop->aux)->control_deps.safe_push (dep_bbs);

  /* Associate control dependence with related equivalent basic blocks.  */
  for (equiv_head->aux = dep_bbs; bb != equiv_head;
       bb = get_immediate_dominator (CDI_DOMINATORS, bb))
    bb->aux = dep_bbs;

  return dep_bbs;
}

/* Forward declaration */

static bool
stmt_semi_invariant_p_1 (struct loop *loop, gimple *stmt,
			 const_basic_block skip_head,
			 hash_map<gimple *, bool> &stmt_stat);

/* Given STMT, memory load or pure call statement, check whether it is impacted
   by some memory store in LOOP, excluding trace starting from SKIP_HEAD (the
   trace is composed of SKIP_HEAD and those basic block dominated by it, always
   corresponds to one branch of a conditional statement).  If SKIP_HEAD is
   NULL, all basic blocks of LOOP are checked.  */

static bool
vuse_semi_invariant_p (struct loop *loop, gimple *stmt,
		       const_basic_block skip_head)
{
  split_info *info = (split_info *) loop->aux;
  tree rhs = NULL_TREE;
  ao_ref ref;
  gimple *store;
  unsigned i;

  /* Collect memory store/clobber statements if haven't done that.  */
  if (info->need_init)
    find_vdef_in_loop (loop);

  if (is_gimple_assign (stmt))
    rhs = gimple_assign_rhs1 (stmt);

  ao_ref_init (&ref, rhs);

  FOR_EACH_VEC_ELT (info->memory_stores, i, store)
    {
      /* Skip basic blocks dominated by SKIP_HEAD, if non-NULL.  */
      if (skip_head
	  && dominated_by_p (CDI_DOMINATORS, gimple_bb (store), skip_head))
	continue;

      if (!ref.ref || stmt_may_clobber_ref_p_1 (store, &ref))
	return false;
    }

  return true;
}

/* Suppose one condition branch, led by SKIP_HEAD, is not executed since
   certain iteration of LOOP, check whether an SSA name (NAME) remains
   unchanged in next iteration.  We call this characteristic semi-
   invariantness.  SKIP_HEAD might be NULL, if so, nothing excluded, all basic
   blocks and control flows in the loop will be considered.  Semi-invariant
   state of checked statement is cached in hash map STMT_STAT to avoid
   redundant computation in possible following re-check.  */

static inline bool
ssa_semi_invariant_p (struct loop *loop, tree name,
		      const_basic_block skip_head,
		      hash_map<gimple *, bool> &stmt_stat)
{
  gimple *def = SSA_NAME_DEF_STMT (name);
  const_basic_block def_bb = gimple_bb (def);

  /* An SSA name defined outside loop is definitely semi-invariant.  */
  if (!def_bb || !flow_bb_inside_loop_p (loop, def_bb))
    return true;

  return stmt_semi_invariant_p_1 (loop, def, skip_head, stmt_stat);
}

/* Check whether a loop iteration PHI node (LOOP_PHI) defines a value that is
   semi-invariant in LOOP.  Basic blocks dominated by SKIP_HEAD (if non-NULL),
   are excluded from LOOP.  */

static bool
loop_iter_phi_semi_invariant_p (struct loop *loop, gphi *loop_phi,
				const_basic_block skip_head)
{
  const_edge latch = loop_latch_edge (loop);
  tree name = gimple_phi_result (loop_phi);
  tree from = PHI_ARG_DEF_FROM_EDGE (loop_phi, latch);

  gcc_checking_assert (from);

  /* Loop iteration PHI node locates in loop header, and it has two source
     operands, one is an initial value coming from outside the loop, the other
     is a value through latch of the loop, which is derived in last iteration,
     we call the latter latch value.  From the PHI node to definition of latch
     value, if excluding branch trace starting from SKIP_HEAD, except copy-
     assignment or likewise, there is no other kind of value redefinition, SSA
     name defined by the PHI node is semi-invariant.

                         loop entry
                              |     .--- latch ---.
                              |     |             |
                              v     v             |
                  x_1 = PHI <x_0,  x_3>           |
                           |                      |
                           v                      |
              .------- if (cond) -------.         |
              |                         |         |
              |                     [ SKIP ]      |
              |                         |         |
              |                     x_2 = ...     |
              |                         |         |
              '---- T ---->.<---- F ----'         |
                           |                      |
                           v                      |
                  x_3 = PHI <x_1, x_2>            |
                           |                      |
                           '----------------------'

     Suppose in certain iteration, execution flow in above graph goes through
     true branch, which means that one source value to define x_3 in false
     branch (x_2) is skipped, x_3 only comes from x_1, and x_1 in next
     iterations is defined by x_3, we know that x_1 will never changed if COND
     always chooses true branch from then on.  */

  while (from != name)
    {
      /* A new value comes from a CONSTANT.  */
      if (TREE_CODE (from) != SSA_NAME)
	return false;

      gimple *stmt = SSA_NAME_DEF_STMT (from);
      const_basic_block bb = gimple_bb (stmt);

      /* A new value comes from outside the loop.  */
      if (!bb || !flow_bb_inside_loop_p (loop, bb))
	return false;

      from = NULL_TREE;

      if (gimple_code (stmt) == GIMPLE_PHI)
	{
	  gphi *phi = as_a <gphi *> (stmt);

	  for (unsigned i = 0; i < gimple_phi_num_args (phi); ++i)
	    {
	      if (skip_head)
		{
		  const_edge e = gimple_phi_arg_edge (phi, i);

		  /* Don't consider redefinitions in excluded basic blocks.  */
		  if (dominated_by_p (CDI_DOMINATORS, e->src, skip_head))
		    continue;
		}

	      tree arg = gimple_phi_arg_def (phi, i);

	      if (!from)
		from = arg;
	      else if (!operand_equal_p (from, arg, 0))
		/* There are more than one source operands that provide
		   different values to the SSA name, it is variant.  */
		return false;
	    }
	}
      else if (gimple_code (stmt) == GIMPLE_ASSIGN)
	{
	  /* For simple value copy, check its rhs instead.  */
	  if (gimple_assign_ssa_name_copy_p (stmt))
	    from = gimple_assign_rhs1 (stmt);
	}

      /* Any other kind of definition is deemed to introduce a new value
	 to the SSA name.  */
      if (!from)
	return false;
    }
  return true;
}

/* Check whether conditional predicates that BB is control-dependent on, are
   semi-invariant in LOOP.  Basic blocks dominated by SKIP_HEAD (if non-NULL),
   are excluded from LOOP.  Semi-invariant state of checked statement is cached
   in hash map STMT_STAT.  */

static bool
control_dep_semi_invariant_p (struct loop *loop, basic_block bb,
			      const_basic_block skip_head,
			      hash_map<gimple *, bool> &stmt_stat)
{
  hash_set<basic_block> *dep_bbs = find_control_dep_blocks (loop, bb);

  if (!dep_bbs)
    return true;

  for (hash_set<basic_block>::iterator iter = dep_bbs->begin ();
       iter != dep_bbs->end (); ++iter)
    {
      gimple *last = last_stmt (*iter);

      if (!last)
	return false;

      /* Only check condition predicates.  */
      if (gimple_code (last) != GIMPLE_COND
	  && gimple_code (last) != GIMPLE_SWITCH)
	return false;

      if (!stmt_semi_invariant_p_1 (loop, last, skip_head, stmt_stat))
	return false;
    }

  return true;
}

/* Check whether STMT is semi-invariant in LOOP, iff all its operands are
   semi-invariant, consequently, all its defined values are semi-invariant.
   Basic blocks dominated by SKIP_HEAD (if non-NULL), are excluded from LOOP.
   Semi-invariant state of checked statement is cached in hash map
   STMT_STAT.  */

static bool
stmt_semi_invariant_p_1 (struct loop *loop, gimple *stmt,
			 const_basic_block skip_head,
			 hash_map<gimple *, bool> &stmt_stat)
{
  bool existed;
  bool &invar = stmt_stat.get_or_insert (stmt, &existed);

  if (existed)
    return invar;

  /* A statement might depend on itself, which is treated as variant.  So set
     state of statement under check to be variant to ensure that.  */
  invar = false;

  if (gimple_code (stmt) == GIMPLE_PHI)
    {
      gphi *phi = as_a <gphi *> (stmt);

      if (gimple_bb (stmt) == loop->header)
	{
	  invar = loop_iter_phi_semi_invariant_p (loop, phi, skip_head);
	  return invar;
	}

      /* For a loop PHI node that does not locate in loop header, it is semi-
	 invariant only if two conditions are met.  The first is its source
	 values are derived from CONSTANT (including loop-invariant value), or
	 from SSA name defined by semi-invariant loop iteration PHI node.  The
	 second is its source incoming edges are control-dependent on semi-
	 invariant conditional predicates.  */
      for (unsigned i = 0; i < gimple_phi_num_args (phi); ++i)
	{
	  const_edge e = gimple_phi_arg_edge (phi, i);
	  tree arg = gimple_phi_arg_def (phi, i);

	  if (TREE_CODE (arg) == SSA_NAME)
	    {
	      if (!ssa_semi_invariant_p (loop, arg, skip_head, stmt_stat))
		return false;

	      /* If source value is defined in location from where the source
		 edge comes in, no need to check control dependency again
		 since this has been done in above SSA name check stage.  */
	      if (e->src == gimple_bb (SSA_NAME_DEF_STMT (arg)))
		continue;
	    }

	  if (!control_dep_semi_invariant_p (loop, e->src, skip_head,
					     stmt_stat))
	    return false;
	}
    }
  else
    {
      ssa_op_iter iter;
      tree use;

      /* Volatile memory load or return of normal (non-const/non-pure) call
	 should not be treated as constant in each iteration of loop.  */
      if (gimple_has_side_effects (stmt))
	return false;

      /* Check if any memory store may kill memory load at this place.  */
      if (gimple_vuse (stmt) && !vuse_semi_invariant_p (loop, stmt, skip_head))
	return false;

      /* Although operand of a statement might be SSA name, CONSTANT or
	 VARDECL, here we only need to check SSA name operands.  This is
	 because check on VARDECL operands, which involve memory loads,
	 must have been done prior to invocation of this function in
	 vuse_semi_invariant_p.  */
      FOR_EACH_SSA_TREE_OPERAND (use, stmt, iter, SSA_OP_USE)
	if (!ssa_semi_invariant_p (loop, use, skip_head, stmt_stat))
	  return false;
    }

  if (!control_dep_semi_invariant_p (loop, gimple_bb (stmt), skip_head,
				     stmt_stat))
    return false;

  /* Here we SHOULD NOT use invar = true, since hash map might be changed due
     to new insertion, and thus invar may point to invalid memory.  */
  stmt_stat.put (stmt, true);
  return true;
}

/* A helper function to check whether STMT is semi-invariant in LOOP.  Basic
   blocks dominated by SKIP_HEAD (if non-NULL), are excluded from LOOP.  */

static bool
stmt_semi_invariant_p (struct loop *loop, gimple *stmt,
		       const_basic_block skip_head)
{
  hash_map<gimple *, bool> stmt_stat;
  return stmt_semi_invariant_p_1 (loop, stmt, skip_head, stmt_stat);
}

/* Determine when conditional statement never transfers execution to one of its
   branch, whether we can remove the branch's leading basic block (BRANCH_BB)
   and those basic blocks dominated by BRANCH_BB.  */

static bool
branch_removable_p (basic_block branch_bb)
{
  edge_iterator ei;
  edge e;

  if (single_pred_p (branch_bb))
    return true;

  FOR_EACH_EDGE (e, ei, branch_bb->preds)
    {
      if (dominated_by_p (CDI_DOMINATORS, e->src, branch_bb))
	continue;

      if (dominated_by_p (CDI_DOMINATORS, branch_bb, e->src))
	continue;

       /* The branch can be reached from opposite branch, or from some
	  statement not dominated by the conditional statement.  */
      return false;
    }

  return true;
}

/* Find out which branch of a conditional statement (COND) is invariant in the
   execution context of LOOP.  That is: once the branch is selected in certain
   iteration of the loop, any operand that contributes to computation of the
   conditional statement remains unchanged in all following iterations.  */

static edge
get_cond_invariant_branch (struct loop *loop, gcond *cond)
{
  basic_block cond_bb = gimple_bb (cond);
  basic_block targ_bb[2];
  bool invar[2];
  unsigned invar_checks = 0;

  for (unsigned i = 0; i < 2; i++)
    {
      targ_bb[i] = EDGE_SUCC (cond_bb, i)->dest;

      /* One branch directs to loop exit, no need to perform loop split upon
	 this conditional statement.  Firstly, it is trivial if the exit branch
	 is semi-invariant, for the statement is just to break loop.  Secondly,
	 if the opposite branch is semi-invariant, it means that the statement
	 is real loop-invariant, which is covered by loop unswitch.  */
      if (!flow_bb_inside_loop_p (loop, targ_bb[i]))
	return NULL;
    }

  for (unsigned i = 0; i < 2; i++)
    {
      invar[!i] = false;

      if (!branch_removable_p (targ_bb[i]))
	continue;

      /* Given a semi-invariant branch, if its opposite branch dominates
	 loop latch, it and its following trace will only be executed in
	 final iteration of loop, namely it is not part of repeated body
	 of the loop.  Similar to the above case that the branch is loop
	 exit, no need to split loop.  */
      if (dominated_by_p (CDI_DOMINATORS, loop->latch, targ_bb[i]))
	continue;

      invar[!i] = stmt_semi_invariant_p (loop, cond, targ_bb[i]);
      invar_checks++;
    }

  /* With both branches being invariant (handled by loop unswitch) or
     variant is not what we want.  */
  if (invar[0] ^ !invar[1])
    return NULL;

  /* Found a real loop-invariant condition, do nothing.  */
  if (invar_checks < 2 && stmt_semi_invariant_p (loop, cond, NULL))
    return NULL;

  return EDGE_SUCC (cond_bb, invar[0] ? 0 : 1);
}

/* Calculate increased code size measured by estimated insn number if applying
   loop split upon certain branch (BRANCH_EDGE) of a conditional statement.  */

static int
compute_added_num_insns (struct loop *loop, const_edge branch_edge)
{
  basic_block cond_bb = branch_edge->src;
  unsigned branch = EDGE_SUCC (cond_bb, 1) == branch_edge;
  basic_block opposite_bb = EDGE_SUCC (cond_bb, !branch)->dest;
  basic_block *bbs = ((split_info *) loop->aux)->bbs;
  int num = 0;

  for (unsigned i = 0; i < loop->num_nodes; i++)
    {
      /* Do no count basic blocks only in opposite branch.  */
      if (dominated_by_p (CDI_DOMINATORS, bbs[i], opposite_bb))
	continue;

      num += estimate_num_insns_seq (bb_seq (bbs[i]), &eni_size_weights);
    }

  /* It is unnecessary to evaluate expression of the conditional statement
     in new loop that contains only invariant branch.  This expression should
     be constant value (either true or false).  Exclude code size of insns
     that contribute to computation of the expression.  */

  auto_vec<gimple *> worklist;
  hash_set<gimple *> removed;
  gimple *stmt = last_stmt (cond_bb);

  worklist.safe_push (stmt);
  removed.add (stmt);
  num -= estimate_num_insns (stmt, &eni_size_weights);

  do
    {
      ssa_op_iter opnd_iter;
      use_operand_p opnd_p;

      stmt = worklist.pop ();
      FOR_EACH_PHI_OR_STMT_USE (opnd_p, stmt, opnd_iter, SSA_OP_USE)
	{
	  tree opnd = USE_FROM_PTR (opnd_p);

	  if (TREE_CODE (opnd) != SSA_NAME || SSA_NAME_IS_DEFAULT_DEF (opnd))
	    continue;

	  gimple *opnd_stmt = SSA_NAME_DEF_STMT (opnd);
	  use_operand_p use_p;
	  imm_use_iterator use_iter;

	  if (removed.contains (opnd_stmt)
	      || !flow_bb_inside_loop_p (loop, gimple_bb (opnd_stmt)))
	    continue;

	  FOR_EACH_IMM_USE_FAST (use_p, use_iter, opnd)
	    {
	      gimple *use_stmt = USE_STMT (use_p);

	      if (!is_gimple_debug (use_stmt) && !removed.contains (use_stmt))
		{
		  opnd_stmt = NULL;
		  break;
		}
	    }

	  if (opnd_stmt)
	    {
	      worklist.safe_push (opnd_stmt);
	      removed.add (opnd_stmt);
	      num -= estimate_num_insns (opnd_stmt, &eni_size_weights);
	    }
	}
    } while (!worklist.is_empty ());

  gcc_assert (num >= 0);
  return num;
}

/* Find out loop-invariant branch of a conditional statement (COND) if it has,
   and check whether it is eligible and profitable to perform loop split upon
   this branch in LOOP.  */

static edge
get_cond_branch_to_split_loop (struct loop *loop, gcond *cond)
{
  edge invar_branch = get_cond_invariant_branch (loop, cond);
  if (!invar_branch)
    return NULL;

  /* When accurate profile information is available, and execution
     frequency of the branch is too low, just let it go.  */
  profile_probability prob = invar_branch->probability;
  if (prob.reliable_p ())
    {
      int thres = param_min_loop_cond_split_prob;

      if (prob < profile_probability::always ().apply_scale (thres, 100))
	return NULL;
    }

  /* Add a threshold for increased code size to disable loop split.  */
  if (compute_added_num_insns (loop, invar_branch) > param_max_peeled_insns)
    return NULL;

  return invar_branch;
}

/* Given a loop (LOOP1) with a loop-invariant branch (INVAR_BRANCH) of some
   conditional statement, perform loop split transformation illustrated
   as the following graph.

               .-------T------ if (true) ------F------.
               |                    .---------------. |
               |                    |               | |
               v                    |               v v
          pre-header                |            pre-header
               | .------------.     |                 | .------------.
               | |            |     |                 | |            |
               | v            |     |                 | v            |
             header           |     |               header           |
               |              |     |                 |              |
      .--- if (cond) ---.     |     |        .--- if (true) ---.     |
      |                 |     |     |        |                 |     |
  invariant             |     |     |    invariant             |     |
      |                 |     |     |        |                 |     |
      '---T--->.<---F---'     |     |        '---T--->.<---F---'     |
               |              |    /                  |              |
             stmts            |   /                 stmts            |
               |              F  T                    |              |
              / \             | /                    / \             |
     .-------*   *      [ if (cond) ]       .-------*   *            |
     |           |            |             |           |            |
     |         latch          |             |         latch          |
     |           |            |             |           |            |
     |           '------------'             |           '------------'
     '------------------------. .-----------'
             loop1            | |                   loop2
                              v v
                             exits

   In the graph, loop1 represents the part derived from original one, and
   loop2 is duplicated using loop_version (), which corresponds to the part
   of original one being splitted out.  In original latch edge of loop1, we
   insert a new conditional statement duplicated from the semi-invariant cond,
   and one of its branch goes back to loop1 header as a latch edge, and the
   other branch goes to loop2 pre-header as an entry edge.  And also in loop2,
   we abandon the variant branch of the conditional statement by setting a
   constant bool condition, based on which branch is semi-invariant.  */

static bool
do_split_loop_on_cond (struct loop *loop1, edge invar_branch)
{
  basic_block cond_bb = invar_branch->src;
  bool true_invar = !!(invar_branch->flags & EDGE_TRUE_VALUE);
  gcond *cond = as_a <gcond *> (last_stmt (cond_bb));

  gcc_assert (cond_bb->loop_father == loop1);

  if (dump_enabled_p ())
    dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, cond,
		     "loop split on semi-invariant condition at %s branch\n",
		     true_invar ? "true" : "false");

  initialize_original_copy_tables ();

  struct loop *loop2 = loop_version (loop1, boolean_true_node, NULL,
				     profile_probability::always (),
				     profile_probability::never (),
				     profile_probability::always (),
				     profile_probability::always (),
				     true);
  if (!loop2)
    {
      free_original_copy_tables ();
      return false;
    }

  basic_block cond_bb_copy = get_bb_copy (cond_bb);
  gcond *cond_copy = as_a<gcond *> (last_stmt (cond_bb_copy));

  /* Replace the condition in loop2 with a bool constant to let PassManager
     remove the variant branch after current pass completes.  */
  if (true_invar)
    gimple_cond_make_true (cond_copy);
  else
    gimple_cond_make_false (cond_copy);

  update_stmt (cond_copy);

  /* Insert a new conditional statement on latch edge of loop1, its condition
     is duplicated from the semi-invariant.  This statement acts as a switch
     to transfer execution from loop1 to loop2, when loop1 enters into
     invariant state.  */
  basic_block latch_bb = split_edge (loop_latch_edge (loop1));
  basic_block break_bb = split_edge (single_pred_edge (latch_bb));
  gimple *break_cond = gimple_build_cond (gimple_cond_code(cond),
					  gimple_cond_lhs (cond),
					  gimple_cond_rhs (cond),
					  NULL_TREE, NULL_TREE);

  gimple_stmt_iterator gsi = gsi_last_bb (break_bb);
  gsi_insert_after (&gsi, break_cond, GSI_NEW_STMT);

  edge to_loop1 = single_succ_edge (break_bb);
  edge to_loop2 = make_edge (break_bb, loop_preheader_edge (loop2)->src, 0);

  to_loop1->flags &= ~EDGE_FALLTHRU;
  to_loop1->flags |= true_invar ? EDGE_FALSE_VALUE : EDGE_TRUE_VALUE;
  to_loop2->flags |= true_invar ? EDGE_TRUE_VALUE : EDGE_FALSE_VALUE;

  update_ssa (TODO_update_ssa);

  /* Due to introduction of a control flow edge from loop1 latch to loop2
     pre-header, we should update PHIs in loop2 to reflect this connection
     between loop1 and loop2.  */
  connect_loop_phis (loop1, loop2, to_loop2);

  free_original_copy_tables ();

  rewrite_into_loop_closed_ssa_1 (NULL, 0, SSA_OP_USE, loop1);

  return true;
}

/* Traverse all conditional statements in LOOP, to find out a good candidate
   upon which we can do loop split.  */

static bool
split_loop_on_cond (struct loop *loop)
{
  split_info *info = new split_info ();
  basic_block *bbs = info->bbs = get_loop_body (loop);
  bool do_split = false;

  /* Allocate an area to keep temporary info, and associate its address
     with loop aux field.  */
  loop->aux = info;

  for (unsigned i = 0; i < loop->num_nodes; i++)
    bbs[i]->aux = NULL;

  for (unsigned i = 0; i < loop->num_nodes; i++)
    {
      basic_block bb = bbs[i];

      /* We only consider conditional statement, which be executed at most once
	 in each iteration of the loop.  So skip statements in inner loops.  */
      if ((bb->loop_father != loop) || (bb->flags & BB_IRREDUCIBLE_LOOP))
	continue;

      /* Actually this check is not a must constraint.  With it, we can ensure
	 conditional statement will always be executed in each iteration.  */
      if (!dominated_by_p (CDI_DOMINATORS, loop->latch, bb))
	continue;

      gimple *last = last_stmt (bb);

      if (!last || gimple_code (last) != GIMPLE_COND)
	continue;

      gcond *cond = as_a <gcond *> (last);
      edge branch_edge = get_cond_branch_to_split_loop (loop, cond);

      if (branch_edge)
	{
	  do_split_loop_on_cond (loop, branch_edge);
	  do_split = true;
	  break;
	}
    }

  delete info;
  loop->aux = NULL;

  return do_split;
}

/* Main entry point.  Perform loop splitting on all suitable loops.  */

static unsigned int
tree_ssa_split_loops (void)
{
  class loop *loop;
  bool changed = false;

  gcc_assert (scev_initialized_p ());

  calculate_dominance_info (CDI_POST_DOMINATORS);

  FOR_EACH_LOOP (loop, LI_INCLUDE_ROOT)
    loop->aux = NULL;

  /* Go through all loops starting from innermost.  */
  FOR_EACH_LOOP (loop, LI_FROM_INNERMOST)
    {
      if (loop->aux)
	{
	  /* If any of our inner loops was split, don't split us,
	     and mark our containing loop as having had splits as well.  */
	  loop_outer (loop)->aux = loop;
	  continue;
	}

      if (optimize_loop_for_size_p (loop))
	continue;

      if (split_loop (loop) || split_loop_on_cond (loop))
	{
	  /* Mark our containing loop as having had some split inner loops.  */
	  loop_outer (loop)->aux = loop;
	  changed = true;
	}
    }

  FOR_EACH_LOOP (loop, LI_INCLUDE_ROOT)
    loop->aux = NULL;

  clear_aux_for_blocks ();

  free_dominance_info (CDI_POST_DOMINATORS);

  if (changed)
    return TODO_cleanup_cfg;
  return 0;
}

/* Loop splitting pass.  */

namespace {

const pass_data pass_data_loop_split =
{
  GIMPLE_PASS, /* type */
  "lsplit", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  TV_LOOP_SPLIT, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_loop_split : public gimple_opt_pass
{
public:
  pass_loop_split (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_loop_split, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return flag_split_loops != 0; }
  virtual unsigned int execute (function *);

}; // class pass_loop_split

unsigned int
pass_loop_split::execute (function *fun)
{
  if (number_of_loops (fun) <= 1)
    return 0;

  return tree_ssa_split_loops ();
}

} // anon namespace

gimple_opt_pass *
make_pass_loop_split (gcc::context *ctxt)
{
  return new pass_loop_split (ctxt);
}
