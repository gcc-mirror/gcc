/* Loop splitting.
   Copyright (C) 2015-2017 Free Software Foundation, Inc.

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
#include "cfgloop.h"
#include "tree-scalar-evolution.h"
#include "gimple-iterator.h"
#include "gimple-pretty-print.h"
#include "cfghooks.h"
#include "gimple-fold.h"
#include "gimplify-me.h"

/* This file implements loop splitting, i.e. transformation of loops like

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
split_at_bb_p (struct loop *loop, basic_block bb, tree *border, affine_iv *iv)
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
  struct loop *useloop = loop_containing_stmt (stmt);

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
patch_loop_exit (struct loop *loop, gcond *guard, tree nextval, tree newbound,
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
find_or_create_guard_phi (struct loop *loop, tree guard_iv, affine_iv * /*iv*/)
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
easy_exit_values (struct loop *loop)
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
connect_loop_phis (struct loop *loop1, struct loop *loop2, edge new_e)
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
connect_loops (struct loop *loop1, struct loop *loop2)
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

  new_e->count = skip_bb->count;
  new_e->probability = profile_probability::likely ();
  new_e->count = skip_e->count.apply_probability (PROB_LIKELY);
  skip_e->count -= new_e->count;
  skip_e->probability = profile_probability::unlikely ();

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
compute_new_first_bound (gimple_seq *stmts, struct tree_niter_desc *niter,
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
split_loop (struct loop *loop1, struct tree_niter_desc *niter)
{
  basic_block *bbs;
  unsigned i;
  bool changed = false;
  tree guard_iv;
  tree border = NULL_TREE;
  affine_iv iv;

  bbs = get_loop_body (loop1);

  /* Find a splitting opportunity.  */
  for (i = 0; i < loop1->num_nodes; i++)
    if ((guard_iv = split_at_bb_p (loop1, bbs[i], &border, &iv)))
      {
	/* Handling opposite steps is not implemented yet.  Neither
	   is handling different step sizes.  */
	if ((tree_int_cst_sign_bit (iv.step)
	     != tree_int_cst_sign_bit (niter->control.step))
	    || !tree_int_cst_equal (iv.step, niter->control.step))
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

	/* FIXME: probabilities seems wrong here.  */
	struct loop *loop2 = loop_version (loop1, cond, &cond_bb,
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
	tree newend = compute_new_first_bound (&stmts, niter, border,
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

/* Main entry point.  Perform loop splitting on all suitable loops.  */

static unsigned int
tree_ssa_split_loops (void)
{
  struct loop *loop;
  bool changed = false;

  gcc_assert (scev_initialized_p ());
  FOR_EACH_LOOP (loop, LI_INCLUDE_ROOT)
    loop->aux = NULL;

  /* Go through all loops starting from innermost.  */
  FOR_EACH_LOOP (loop, LI_FROM_INNERMOST)
    {
      struct tree_niter_desc niter;
      if (loop->aux)
	{
	  /* If any of our inner loops was split, don't split us,
	     and mark our containing loop as having had splits as well.  */
	  loop_outer (loop)->aux = loop;
	  continue;
	}

      if (single_exit (loop)
	  /* ??? We could handle non-empty latches when we split
	     the latch edge (not the exit edge), and put the new
	     exit condition in the new block.  OTOH this executes some
	     code unconditionally that might have been skipped by the
	     original exit before.  */
	  && empty_block_p (loop->latch)
	  && !optimize_loop_for_size_p (loop)
	  && easy_exit_values (loop)
	  && number_of_iterations_exit (loop, single_exit (loop), &niter,
					false, true)
	  && niter.cmp != ERROR_MARK
	  /* We can't yet handle loops controlled by a != predicate.  */
	  && niter.cmp != NE_EXPR)
	{
	  if (split_loop (loop, &niter))
	    {
	      /* Mark our containing loop as having had some split inner
	         loops.  */
	      loop_outer (loop)->aux = loop;
	      changed = true;
	    }
	}
    }

  FOR_EACH_LOOP (loop, LI_INCLUDE_ROOT)
    loop->aux = NULL;

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
