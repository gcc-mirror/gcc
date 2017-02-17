/* Loop unswitching.
   Copyright (C) 2004-2017 Free Software Foundation, Inc.

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
#include "gimplify.h"
#include "tree-cfg.h"
#include "tree-ssa.h"
#include "tree-ssa-loop-niter.h"
#include "tree-ssa-loop.h"
#include "tree-into-ssa.h"
#include "cfgloop.h"
#include "params.h"
#include "tree-inline.h"
#include "gimple-iterator.h"
#include "cfghooks.h"
#include "tree-ssa-loop-manip.h"

/* This file implements the loop unswitching, i.e. transformation of loops like

   while (A)
     {
       if (inv)
         B;

       X;

       if (!inv)
	 C;
     }

   where inv is the loop invariant, into

   if (inv)
     {
       while (A)
	 {
           B;
	   X;
	 }
     }
   else
     {
       while (A)
	 {
	   X;
	   C;
	 }
     }

   Inv is considered invariant iff the values it compares are both invariant;
   tree-ssa-loop-im.c ensures that all the suitable conditions are in this
   shape.  */

static struct loop *tree_unswitch_loop (struct loop *, basic_block, tree);
static bool tree_unswitch_single_loop (struct loop *, int);
static tree tree_may_unswitch_on (basic_block, struct loop *);
static bool tree_unswitch_outer_loop (struct loop *);
static edge find_loop_guard (struct loop *);
static bool empty_bb_without_guard_p (struct loop *, basic_block);
static bool used_outside_loop_p (struct loop *, tree);
static void hoist_guard (struct loop *, edge);
static bool check_exit_phi (struct loop *);
static tree get_vop_from_header (struct loop *);

/* Main entry point.  Perform loop unswitching on all suitable loops.  */

unsigned int
tree_ssa_unswitch_loops (void)
{
  struct loop *loop;
  bool changed = false;

  /* Go through all loops starting from innermost.  */
  FOR_EACH_LOOP (loop, LI_FROM_INNERMOST)
    {
      if (!loop->inner)
	/* Unswitch innermost loop.  */
	changed |= tree_unswitch_single_loop (loop, 0);
      else
	changed |= tree_unswitch_outer_loop (loop);
    }

  if (changed)
    return TODO_cleanup_cfg;
  return 0;
}

/* Return TRUE if an SSA_NAME maybe undefined and is therefore
   unsuitable for unswitching.  STMT is the statement we are
   considering for unswitching and LOOP is the loop it appears in.  */

static bool
is_maybe_undefined (const tree name, gimple *stmt, struct loop *loop)
{
  /* The loop header is the only block we can trivially determine that
     will always be executed.  If the comparison is in the loop
     header, we know it's OK to unswitch on it.  */
  if (gimple_bb (stmt) == loop->header)
    return false;

  auto_bitmap visited_ssa;
  auto_vec<tree> worklist;
  worklist.safe_push (name);
  bitmap_set_bit (visited_ssa, SSA_NAME_VERSION (name));
  while (!worklist.is_empty ())
    {
      tree t = worklist.pop ();

      /* If it's obviously undefined, avoid further computations.  */
      if (ssa_undefined_value_p (t, true))
	return true;

      if (ssa_defined_default_def_p (t))
	continue;

      gimple *def = SSA_NAME_DEF_STMT (t);

      /* Check that all the PHI args are fully defined.  */
      if (gphi *phi = dyn_cast <gphi *> (def))
	{
	  for (unsigned i = 0; i < gimple_phi_num_args (phi); ++i)
	    {
	      tree t = gimple_phi_arg_def (phi, i);
	      /* If an SSA has already been seen, it may be a loop,
		 but we can continue and ignore this use.  Otherwise,
		 add the SSA_NAME to the queue and visit it later.  */
	      if (TREE_CODE (t) == SSA_NAME
		  && bitmap_set_bit (visited_ssa, SSA_NAME_VERSION (t)))
		worklist.safe_push (t);
	    }
	  continue;
	}

      /* Uses in stmts always executed when the region header executes
	 are fine.  */
      if (dominated_by_p (CDI_DOMINATORS, loop->header, gimple_bb (def)))
	continue;

      /* Handle calls and memory loads conservatively.  */
      if (!is_gimple_assign (def)
	  || (gimple_assign_single_p (def)
	      && gimple_vuse (def)))
	return true;

      /* Check that any SSA names used to define NAME are also fully
	 defined.  */
      use_operand_p use_p;
      ssa_op_iter iter;
      FOR_EACH_SSA_USE_OPERAND (use_p, def, iter, SSA_OP_USE)
	{
	  tree t = USE_FROM_PTR (use_p);
	  /* If an SSA has already been seen, it may be a loop,
	     but we can continue and ignore this use.  Otherwise,
	     add the SSA_NAME to the queue and visit it later.  */
	  if (bitmap_set_bit (visited_ssa, SSA_NAME_VERSION (t)))
	    worklist.safe_push (t);
	}
    }
  return false;
}

/* Checks whether we can unswitch LOOP on condition at end of BB -- one of its
   basic blocks (for what it means see comments below).  */

static tree
tree_may_unswitch_on (basic_block bb, struct loop *loop)
{
  gimple *last, *def;
  gcond *stmt;
  tree cond, use;
  basic_block def_bb;
  ssa_op_iter iter;

  /* BB must end in a simple conditional jump.  */
  last = last_stmt (bb);
  if (!last || gimple_code (last) != GIMPLE_COND)
    return NULL_TREE;
  stmt = as_a <gcond *> (last);

  /* To keep the things simple, we do not directly remove the conditions,
     but just replace tests with 0 != 0 resp. 1 != 0.  Prevent the infinite
     loop where we would unswitch again on such a condition.  */
  if (gimple_cond_true_p (stmt) || gimple_cond_false_p (stmt))
    return NULL_TREE;

  /* Condition must be invariant.  */
  FOR_EACH_SSA_TREE_OPERAND (use, stmt, iter, SSA_OP_USE)
    {
      def = SSA_NAME_DEF_STMT (use);
      def_bb = gimple_bb (def);
      if (def_bb
	  && flow_bb_inside_loop_p (loop, def_bb))
	return NULL_TREE;
      /* Unswitching on undefined values would introduce undefined
	 behavior that the original program might never exercise.  */
      if (is_maybe_undefined (use, stmt, loop))
	return NULL_TREE;
    }

  cond = build2 (gimple_cond_code (stmt), boolean_type_node,
		 gimple_cond_lhs (stmt), gimple_cond_rhs (stmt));

  return cond;
}

/* Simplifies COND using checks in front of the entry of the LOOP.  Just very
   simplish (sufficient to prevent us from duplicating loop in unswitching
   unnecessarily).  */

static tree
simplify_using_entry_checks (struct loop *loop, tree cond)
{
  edge e = loop_preheader_edge (loop);
  gimple *stmt;

  while (1)
    {
      stmt = last_stmt (e->src);
      if (stmt
	  && gimple_code (stmt) == GIMPLE_COND
	  && gimple_cond_code (stmt) == TREE_CODE (cond)
	  && operand_equal_p (gimple_cond_lhs (stmt),
			      TREE_OPERAND (cond, 0), 0)
	  && operand_equal_p (gimple_cond_rhs (stmt),
			      TREE_OPERAND (cond, 1), 0))
	return (e->flags & EDGE_TRUE_VALUE
		? boolean_true_node
		: boolean_false_node);

      if (!single_pred_p (e->src))
	return cond;

      e = single_pred_edge (e->src);
      if (e->src == ENTRY_BLOCK_PTR_FOR_FN (cfun))
	return cond;
    }
}

/* Unswitch single LOOP.  NUM is number of unswitchings done; we do not allow
   it to grow too much, it is too easy to create example on that the code would
   grow exponentially.  */

static bool
tree_unswitch_single_loop (struct loop *loop, int num)
{
  basic_block *bbs;
  struct loop *nloop;
  unsigned i, found;
  tree cond = NULL_TREE;
  gimple *stmt;
  bool changed = false;
  HOST_WIDE_INT iterations;

  /* Perform initial tests if unswitch is eligible.  */
  if (num == 0)
    {
      /* Do not unswitch in cold regions. */
      if (optimize_loop_for_size_p (loop))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, ";; Not unswitching cold loops\n");
	  return false;
	}

      /* The loop should not be too large, to limit code growth. */
      if (tree_num_loop_insns (loop, &eni_size_weights)
	  > (unsigned) PARAM_VALUE (PARAM_MAX_UNSWITCH_INSNS))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, ";; Not unswitching, loop too big\n");
	  return false;
	}

      /* If the loop is not expected to iterate, there is no need
	 for unswitching.  */
      iterations = estimated_loop_iterations_int (loop);
      if (iterations < 0)
        iterations = likely_max_loop_iterations_int (loop);
      if (iterations >= 0 && iterations <= 1)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, ";; Not unswitching, loop is not expected"
		     " to iterate\n");
	  return false;
	}
    }

  i = 0;
  bbs = get_loop_body (loop);
  found = loop->num_nodes;

  while (1)
    {
      /* Find a bb to unswitch on.  */
      for (; i < loop->num_nodes; i++)
	if ((cond = tree_may_unswitch_on (bbs[i], loop)))
	  break;

      if (i == loop->num_nodes)
	{
	  if (dump_file
	      && num > PARAM_VALUE (PARAM_MAX_UNSWITCH_LEVEL)
	      && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, ";; Not unswitching anymore, hit max level\n");

	  if (found == loop->num_nodes)
	    {
	      free (bbs);
	      return changed;
	    }
	  break;
	}

      cond = simplify_using_entry_checks (loop, cond);
      stmt = last_stmt (bbs[i]);
      if (integer_nonzerop (cond))
	{
	  /* Remove false path.  */
	  gimple_cond_set_condition_from_tree (as_a <gcond *> (stmt),
					       boolean_true_node);
	  changed = true;
	}
      else if (integer_zerop (cond))
	{
	  /* Remove true path.  */
	  gimple_cond_set_condition_from_tree (as_a <gcond *> (stmt),
					       boolean_false_node);
	  changed = true;
	}
      /* Do not unswitch too much.  */
      else if (num > PARAM_VALUE (PARAM_MAX_UNSWITCH_LEVEL))
	{
	  i++;
	  continue;
	}
      /* In nested tree_unswitch_single_loop first optimize all conditions
	 using entry checks, then discover still reachable blocks in the
	 loop and find the condition only among those still reachable bbs.  */
      else if (num != 0)
	{
	  if (found == loop->num_nodes)
	    found = i;
	  i++;
	  continue;
	}
      else
	{
	  found = i;
	  break;
	}

      update_stmt (stmt);
      i++;
    }

  if (num != 0)
    {
      basic_block *tos, *worklist;

      /* When called recursively, first do a quick discovery
	 of reachable bbs after the above changes and only
	 consider conditions in still reachable bbs.  */
      tos = worklist = XNEWVEC (basic_block, loop->num_nodes);

      for (i = 0; i < loop->num_nodes; i++)
	bbs[i]->flags &= ~BB_REACHABLE;

      /* Start with marking header.  */
      *tos++ = bbs[0];
      bbs[0]->flags |= BB_REACHABLE;

      /* Iterate: find everything reachable from what we've already seen
	 within the same innermost loop.  Don't look through false edges
	 if condition is always true or true edges if condition is
	 always false.  */
      while (tos != worklist)
	{
	  basic_block b = *--tos;
	  edge e;
	  edge_iterator ei;
	  int flags = 0;

	  if (EDGE_COUNT (b->succs) == 2)
	    {
	      gimple *stmt = last_stmt (b);
	      if (stmt
		  && gimple_code (stmt) == GIMPLE_COND)
		{
		  gcond *cond_stmt = as_a <gcond *> (stmt);
		  if (gimple_cond_true_p (cond_stmt))
		    flags = EDGE_FALSE_VALUE;
		  else if (gimple_cond_false_p (cond_stmt))
		    flags = EDGE_TRUE_VALUE;
		}
	    }

	  FOR_EACH_EDGE (e, ei, b->succs)
	    {
	      basic_block dest = e->dest;

	      if (dest->loop_father == loop
		  && !(dest->flags & BB_REACHABLE)
		  && !(e->flags & flags))
		{
		  *tos++ = dest;
		  dest->flags |= BB_REACHABLE;
		}
	    }
	}

      free (worklist);

      /* Find a bb to unswitch on.  */
      for (; found < loop->num_nodes; found++)
	if ((bbs[found]->flags & BB_REACHABLE)
	    && (cond = tree_may_unswitch_on (bbs[found], loop)))
	  break;

      if (found == loop->num_nodes)
	{
	  free (bbs);
	  return changed;
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, ";; Unswitching loop\n");

  initialize_original_copy_tables ();
  /* Unswitch the loop on this condition.  */
  nloop = tree_unswitch_loop (loop, bbs[found], cond);
  if (!nloop)
    {
      free_original_copy_tables ();
      free (bbs);
      return changed;
    }

  /* Update the SSA form after unswitching.  */
  update_ssa (TODO_update_ssa);
  free_original_copy_tables ();

  /* Invoke itself on modified loops.  */
  tree_unswitch_single_loop (nloop, num + 1);
  tree_unswitch_single_loop (loop, num + 1);
  free (bbs);
  return true;
}

/* Unswitch a LOOP w.r. to given basic block UNSWITCH_ON.  We only support
   unswitching of innermost loops.  COND is the condition determining which
   loop is entered -- the new loop is entered if COND is true.  Returns NULL
   if impossible, new loop otherwise.  */

static struct loop *
tree_unswitch_loop (struct loop *loop,
		    basic_block unswitch_on, tree cond)
{
  unsigned prob_true;
  edge edge_true, edge_false;

  /* Some sanity checking.  */
  gcc_assert (flow_bb_inside_loop_p (loop, unswitch_on));
  gcc_assert (EDGE_COUNT (unswitch_on->succs) == 2);
  gcc_assert (loop->inner == NULL);

  extract_true_false_edges_from_block (unswitch_on, &edge_true, &edge_false);
  prob_true = edge_true->probability;
  return loop_version (loop, unshare_expr (cond),
		       NULL, prob_true, REG_BR_PROB_BASE - prob_true, prob_true,
		       REG_BR_PROB_BASE - prob_true, false);
}

/* Unswitch outer loops by hoisting invariant guard on
   inner loop without code duplication.  */
static bool
tree_unswitch_outer_loop (struct loop *loop)
{
  edge exit, guard;
  HOST_WIDE_INT iterations;

  gcc_assert (loop->inner);
  if (loop->inner->next)
    return false;
  /* Accept loops with single exit only which is not from inner loop.  */
  exit = single_exit (loop);
  if (!exit || exit->src->loop_father != loop)
    return false;
  /* Check that phi argument of exit edge is not defined inside loop.  */
  if (!check_exit_phi (loop))
    return false;
  /* If the loop is not expected to iterate, there is no need
      for unswitching.  */
  iterations = estimated_loop_iterations_int (loop);
  if (iterations < 0)
    iterations = likely_max_loop_iterations_int (loop);
  if (iterations >= 0 && iterations <= 1)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, ";; Not unswitching, loop is not expected"
		 " to iterate\n");
      return false;
    }

  bool changed = false;
  while ((guard = find_loop_guard (loop)))
    {
      if (! changed)
	rewrite_virtuals_into_loop_closed_ssa (loop);
      hoist_guard (loop, guard);
      changed = true;
    }
  return changed;
}

/* Checks if the body of the LOOP is within an invariant guard.  If this
   is the case, returns the edge that jumps over the real body of the loop,
   otherwise returns NULL.  */

static edge
find_loop_guard (struct loop *loop)
{
  basic_block header = loop->header;
  edge guard_edge, te, fe;
  basic_block *body = NULL;
  unsigned i;
  tree use;
  ssa_op_iter iter;

  /* We check for the following situation:

     while (1)
       {
	 [header]]
         loop_phi_nodes;
	 something1;
	 if (cond1)
	   body;
	 nvar = phi(orig, bvar) ... for all variables changed in body;
	 [guard_end]
	 something2;
	 if (cond2)
	   break;
	 something3;
       }

     where:

     1) cond1 is loop invariant
     2) If cond1 is false, then the loop is essentially empty; i.e.,
	a) nothing in something1, something2 and something3 has side
	   effects
	b) anything defined in something1, something2 and something3
	   is not used outside of the loop.  */

  gcond *cond;
  do
    {
      if (single_succ_p (header))
	header = single_succ (header);
      else
	{
	  cond = dyn_cast <gcond *> (last_stmt (header));
	  if (! cond)
	    return NULL;
	  extract_true_false_edges_from_block (header, &te, &fe);
	  /* Make sure to skip earlier hoisted guards that are left
	     in place as if (true).  */
	  if (gimple_cond_true_p (cond))
	    header = te->dest;
	  else if (gimple_cond_false_p (cond))
	    header = fe->dest;
	  else
	    break;
	}
    }
  while (1);
  if (!flow_bb_inside_loop_p (loop, te->dest)
      || !flow_bb_inside_loop_p (loop, fe->dest))
    return NULL;

  if (just_once_each_iteration_p (loop, te->dest)
      || (single_succ_p (te->dest)
	  && just_once_each_iteration_p (loop, single_succ (te->dest))))
    {
      if (just_once_each_iteration_p (loop, fe->dest))
	return NULL;
      guard_edge = te;
    }
  else if (just_once_each_iteration_p (loop, fe->dest)
	   || (single_succ_p (fe->dest)
	       && just_once_each_iteration_p (loop, single_succ (fe->dest))))
    guard_edge = fe;
  else
    return NULL;

  /* Guard edge must skip inner loop.  */
  if (!dominated_by_p (CDI_DOMINATORS, loop->inner->header,
      guard_edge == fe ? te->dest : fe->dest))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Guard edge %d --> %d is not around the loop!\n",
		 guard_edge->src->index, guard_edge->dest->index);
      return NULL;
    }
  if (guard_edge->dest == loop->latch)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Guard edge destination is loop latch.\n");
      return NULL;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file,
	     "Considering guard %d -> %d in loop %d\n",
	     guard_edge->src->index, guard_edge->dest->index, loop->num);
  /* Check if condition operands do not have definitions inside loop since
     any bb copying is not performed.  */
  FOR_EACH_SSA_TREE_OPERAND (use, cond, iter, SSA_OP_USE)
    {
      gimple *def = SSA_NAME_DEF_STMT (use);
      basic_block def_bb = gimple_bb (def);
      if (def_bb
          && flow_bb_inside_loop_p (loop, def_bb))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "  guard operands have definitions"
				" inside loop\n");
	  return NULL;
	}
    }

  body = get_loop_body (loop);
  for (i = 0; i < loop->num_nodes; i++)
    {
      basic_block bb = body[i];
      if (bb->loop_father != loop)
	continue;
      if (bb->flags & BB_IRREDUCIBLE_LOOP)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Block %d is marked as irreducible in loop\n",
		      bb->index);
	  guard_edge = NULL;
	  goto end;
	}
      if (!empty_bb_without_guard_p (loop, bb))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "  block %d has side effects\n", bb->index);
	  guard_edge = NULL;
	  goto end;
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "  suitable to hoist\n");
end:
  if (body)
    free (body);
  return guard_edge;
}

/* Returns true if
   1) no statement in BB has side effects
   2) assuming that edge GUARD is always taken, all definitions in BB
      are noy used outside of the loop.
   KNOWN_INVARIANTS is a set of ssa names we know to be invariant, and
   PROCESSED is a set of ssa names for that we already tested whether they
   are invariant or not.  */

static bool
empty_bb_without_guard_p (struct loop *loop, basic_block bb)
{
  basic_block exit_bb = single_exit (loop)->src;
  bool may_be_used_outside = (bb == exit_bb
			      || !dominated_by_p (CDI_DOMINATORS, bb, exit_bb));
  tree name;
  ssa_op_iter op_iter;

  /* Phi nodes do not have side effects, but their results might be used
     outside of the loop.  */
  if (may_be_used_outside)
    {
      for (gphi_iterator gsi = gsi_start_phis (bb);
	   !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gphi *phi = gsi.phi ();
	  name = PHI_RESULT (phi);
	  if (virtual_operand_p (name))
	    continue;

	  if (used_outside_loop_p (loop, name))
	    return false;
	}
    }

  for (gimple_stmt_iterator gsi = gsi_start_bb (bb);
       !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      if (gimple_has_side_effects (stmt))
	return false;

      if (gimple_vdef(stmt))
	return false;

      FOR_EACH_SSA_TREE_OPERAND (name, stmt, op_iter, SSA_OP_DEF)
	{
	  if (may_be_used_outside
	      && used_outside_loop_p (loop, name))
	    return false;
	}
    }
  return true;
}

/* Return true if NAME is used outside of LOOP.  */

static bool
used_outside_loop_p (struct loop *loop, tree name)
{
  imm_use_iterator it;
  use_operand_p use;

  FOR_EACH_IMM_USE_FAST (use, it, name)
    {
      gimple *stmt = USE_STMT (use);
      if (!flow_bb_inside_loop_p (loop, gimple_bb (stmt)))
	return true;
    }

  return false;
}

/* Return argument for loop preheader edge in header virtual phi if any.  */

static tree
get_vop_from_header (struct loop *loop)
{
  for (gphi_iterator gsi = gsi_start_phis (loop->header);
       !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();
      if (!virtual_operand_p (gimple_phi_result (phi)))
	continue;
      return PHI_ARG_DEF_FROM_EDGE (phi, loop_preheader_edge (loop));
    }
  return NULL_TREE;
}

/* Move the check of GUARD outside of LOOP.  */

static void
hoist_guard (struct loop *loop, edge guard)
{
  edge exit = single_exit (loop);
  edge preh = loop_preheader_edge (loop);
  basic_block pre_header = preh->src;
  basic_block bb;
  edge te, fe, e, new_edge;
  gimple *stmt;
  basic_block guard_bb = guard->src;
  edge not_guard;
  gimple_stmt_iterator gsi;
  int flags = 0;
  bool fix_dom_of_exit;
  gcond *cond_stmt, *new_cond_stmt;

  bb = get_immediate_dominator (CDI_DOMINATORS, exit->dest);
  fix_dom_of_exit = flow_bb_inside_loop_p (loop, bb);
  gsi = gsi_last_bb (guard_bb);
  stmt = gsi_stmt (gsi);
  gcc_assert (gimple_code (stmt) == GIMPLE_COND);
  cond_stmt = as_a <gcond *> (stmt);
  extract_true_false_edges_from_block (guard_bb, &te, &fe);
  /* Insert guard to PRE_HEADER.  */
  if (!empty_block_p (pre_header))
    gsi = gsi_last_bb (pre_header);
  else
    gsi = gsi_start_bb (pre_header);
  /* Create copy of COND_STMT.  */
  new_cond_stmt = gimple_build_cond (gimple_cond_code (cond_stmt),
				     gimple_cond_lhs (cond_stmt),
				     gimple_cond_rhs (cond_stmt),
				     NULL_TREE, NULL_TREE);
  gsi_insert_after (&gsi, new_cond_stmt, GSI_NEW_STMT);
  /* Convert COND_STMT to true/false conditional.  */
  if (guard == te)
    gimple_cond_make_false (cond_stmt);
  else
    gimple_cond_make_true (cond_stmt);
  update_stmt (cond_stmt);
  /* Create new loop pre-header.  */
  e = split_block (pre_header, last_stmt (pre_header));
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "  Moving guard %i->%i (prob %i) to bb %i, "
	     "new preheader is %i\n",
	     guard->src->index, guard->dest->index, guard->probability,
	     e->src->index, e->dest->index);

  gcc_assert (loop_preheader_edge (loop)->src == e->dest);

  if (guard == fe)
    {
      e->flags = EDGE_TRUE_VALUE;
      flags |= EDGE_FALSE_VALUE;
      not_guard = te;
    }
  else
    {
      e->flags = EDGE_FALSE_VALUE;
      flags |= EDGE_TRUE_VALUE;
      not_guard = fe;
    }
  new_edge = make_edge (pre_header, exit->dest, flags);

  /* Determine the probability that we skip the loop.  Assume that loop has
     same average number of iterations regardless outcome of guard.  */
  new_edge->probability = guard->probability;
  int skip_count = guard->src->count
		   ? RDIV (guard->count * pre_header->count, guard->src->count)
		   : apply_probability (guard->count, new_edge->probability);

  if (skip_count > e->count)
    {
      fprintf (dump_file, "  Capping count; expect profile inconsistency\n");
      skip_count = e->count;
    }
  new_edge->count = skip_count;
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "  Estimated probability of skipping loop is %i\n",
	     new_edge->probability);

  /* Update profile after the transform:

     First decrease count of path from newly hoisted loop guard
     to loop header...  */
  e->count -= skip_count;
  e->probability = REG_BR_PROB_BASE - new_edge->probability;
  e->dest->count = e->count;
  e->dest->frequency = EDGE_FREQUENCY (e);

  /* ... now update profile to represent that original guard will be optimized
     away ...  */
  guard->probability = 0;
  guard->count = 0;
  not_guard->probability = REG_BR_PROB_BASE;
  /* This count is wrong (frequency of not_guard does not change),
     but will be scaled later.  */
  not_guard->count = guard->src->count;

  /* ... finally scale everything in the loop except for guarded basic blocks
     where profile does not change.  */
  basic_block *body = get_loop_body (loop);

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "  Scaling nonguarded BBs in loop:");
  for (unsigned int i = 0; i < loop->num_nodes; i++)
    {
      basic_block bb = body[i];
      if (!dominated_by_p (CDI_DOMINATORS, bb, not_guard->dest))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, " %i", bb->index);
          scale_bbs_frequencies_int (&bb, 1, e->probability, REG_BR_PROB_BASE);
  	}
    }

  if (fix_dom_of_exit)
    set_immediate_dominator (CDI_DOMINATORS, exit->dest, pre_header);
  /* Add NEW_ADGE argument for all phi in post-header block.  */
  bb = exit->dest;
  for (gphi_iterator gsi = gsi_start_phis (bb);
       !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();
      tree arg;
      if (virtual_operand_p (gimple_phi_result (phi)))
	{
	  arg = get_vop_from_header (loop);
	  if (arg == NULL_TREE)
	    /* Use exit edge argument.  */
	    arg =  PHI_ARG_DEF_FROM_EDGE (phi, exit);
	  add_phi_arg (phi, arg, new_edge, UNKNOWN_LOCATION);
	}
      else
	{
	  /* Use exit edge argument.  */
	  arg = PHI_ARG_DEF_FROM_EDGE (phi, exit);
	  add_phi_arg (phi, arg, new_edge, UNKNOWN_LOCATION);
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\n  guard hoisted.\n");

  free (body);
}

/* Return true if phi argument for exit edge can be used
   for edge around loop.  */

static bool
check_exit_phi (struct loop *loop)
{
  edge exit = single_exit (loop);
  basic_block pre_header = loop_preheader_edge (loop)->src;

  for (gphi_iterator gsi = gsi_start_phis (exit->dest);
       !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();
      tree arg;
      gimple *def;
      basic_block def_bb;
      if (virtual_operand_p (gimple_phi_result (phi)))
	continue;
      arg = PHI_ARG_DEF_FROM_EDGE (phi, exit);
      if (TREE_CODE (arg) != SSA_NAME)
	continue;
      def = SSA_NAME_DEF_STMT (arg);
      if (!def)
	continue;
      def_bb = gimple_bb (def);
      if (!def_bb)
	continue;
      if (!dominated_by_p (CDI_DOMINATORS, pre_header, def_bb))
	/* Definition inside loop!  */
	return false;
      /* Check loop closed phi invariant.  */
      if (!flow_bb_inside_loop_p (def_bb->loop_father, pre_header))
	return false;
    }
  return true;
}

/* Loop unswitching pass.  */

namespace {

const pass_data pass_data_tree_unswitch =
{
  GIMPLE_PASS, /* type */
  "unswitch", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  TV_TREE_LOOP_UNSWITCH, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_tree_unswitch : public gimple_opt_pass
{
public:
  pass_tree_unswitch (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_tree_unswitch, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return flag_unswitch_loops != 0; }
  virtual unsigned int execute (function *);

}; // class pass_tree_unswitch

unsigned int
pass_tree_unswitch::execute (function *fun)
{
  if (number_of_loops (fun) <= 1)
    return 0;

  return tree_ssa_unswitch_loops ();
}

} // anon namespace

gimple_opt_pass *
make_pass_tree_unswitch (gcc::context *ctxt)
{
  return new pass_tree_unswitch (ctxt);
}


