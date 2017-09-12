/* CFG cleanup for trees.
   Copyright (C) 2001-2017 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "tree-pass.h"
#include "ssa.h"
#include "diagnostic-core.h"
#include "fold-const.h"
#include "cfganal.h"
#include "cfgcleanup.h"
#include "tree-eh.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "tree-ssa-loop-manip.h"
#include "tree-dfa.h"
#include "tree-ssa.h"
#include "cfgloop.h"
#include "tree-scalar-evolution.h"
#include "gimple-match.h"
#include "gimple-fold.h"
#include "tree-ssa-loop-niter.h"


/* The set of blocks in that at least one of the following changes happened:
   -- the statement at the end of the block was changed
   -- the block was newly created
   -- the set of the predecessors of the block changed
   -- the set of the successors of the block changed
   ??? Maybe we could track these changes separately, since they determine
       what cleanups it makes sense to try on the block.  */
bitmap cfgcleanup_altered_bbs;

/* Remove any fallthru edge from EV.  Return true if an edge was removed.  */

static bool
remove_fallthru_edge (vec<edge, va_gc> *ev)
{
  edge_iterator ei;
  edge e;

  FOR_EACH_EDGE (e, ei, ev)
    if ((e->flags & EDGE_FALLTHRU) != 0)
      {
	if (e->flags & EDGE_COMPLEX)
	  e->flags &= ~EDGE_FALLTHRU;
	else
	  remove_edge_and_dominated_blocks (e);
	return true;
      }
  return false;
}

/* Convert a SWTCH with single non-default case to gcond and replace it
   at GSI.  */

static bool
convert_single_case_switch (gswitch *swtch, gimple_stmt_iterator &gsi)
{
  if (gimple_switch_num_labels (swtch) != 2)
    return false;

  tree index = gimple_switch_index (swtch);
  tree default_label = CASE_LABEL (gimple_switch_default_label (swtch));
  tree label = gimple_switch_label (swtch, 1);
  tree low = CASE_LOW (label);
  tree high = CASE_HIGH (label);

  basic_block default_bb = label_to_block_fn (cfun, default_label);
  basic_block case_bb = label_to_block_fn (cfun, CASE_LABEL (label));

  basic_block bb = gimple_bb (swtch);
  gcond *cond;

  /* Replace switch statement with condition statement.  */
  if (high)
    {
      tree lhs, rhs;
      generate_range_test (bb, index, low, high, &lhs, &rhs);
      cond = gimple_build_cond (LE_EXPR, lhs, rhs, NULL_TREE, NULL_TREE);
    }
  else
    cond = gimple_build_cond (EQ_EXPR, index,
			      fold_convert (TREE_TYPE (index), low),
			      NULL_TREE, NULL_TREE);

  gsi_replace (&gsi, cond, true);

  /* Update edges.  */
  edge case_edge = find_edge (bb, case_bb);
  edge default_edge = find_edge (bb, default_bb);

  case_edge->flags |= EDGE_TRUE_VALUE;
  default_edge->flags |= EDGE_FALSE_VALUE;
  return true;
}

/* Disconnect an unreachable block in the control expression starting
   at block BB.  */

static bool
cleanup_control_expr_graph (basic_block bb, gimple_stmt_iterator gsi,
			    bool first_p)
{
  edge taken_edge;
  bool retval = false;
  gimple *stmt = gsi_stmt (gsi);

  if (!single_succ_p (bb))
    {
      edge e;
      edge_iterator ei;
      bool warned;
      tree val = NULL_TREE;

      /* Try to convert a switch with just a single non-default case to
	 GIMPLE condition.  */
      if (gimple_code (stmt) == GIMPLE_SWITCH
	  && convert_single_case_switch (as_a<gswitch *> (stmt), gsi))
	stmt = gsi_stmt (gsi);

      fold_defer_overflow_warnings ();
      switch (gimple_code (stmt))
	{
	case GIMPLE_COND:
	  /* During a first iteration on the CFG only remove trivially
	     dead edges but mark other conditions for re-evaluation.  */
	  if (first_p)
	    {
	      val = const_binop (gimple_cond_code (stmt), boolean_type_node,
				 gimple_cond_lhs (stmt),
				 gimple_cond_rhs (stmt));
	      if (! val)
		bitmap_set_bit (cfgcleanup_altered_bbs, bb->index);
	    }
	  else
	    {
	      code_helper rcode;
	      tree ops[3] = {};
	      if (gimple_simplify (stmt, &rcode, ops, NULL, no_follow_ssa_edges,
				   no_follow_ssa_edges)
		  && rcode == INTEGER_CST)
		val = ops[0];
	    }
	  break;

	case GIMPLE_SWITCH:
	  val = gimple_switch_index (as_a <gswitch *> (stmt));
	  break;

	default:
	  ;
	}
      taken_edge = find_taken_edge (bb, val);
      if (!taken_edge)
	{
	  fold_undefer_and_ignore_overflow_warnings ();
	  return false;
	}

      /* Remove all the edges except the one that is always executed.  */
      warned = false;
      for (ei = ei_start (bb->succs); (e = ei_safe_edge (ei)); )
	{
	  if (e != taken_edge)
	    {
	      if (!warned)
		{
		  fold_undefer_overflow_warnings
		    (true, stmt, WARN_STRICT_OVERFLOW_CONDITIONAL);
		  warned = true;
		}

	      taken_edge->probability += e->probability;
	      taken_edge->count += e->count;
	      remove_edge_and_dominated_blocks (e);
	      retval = true;
	    }
	  else
	    ei_next (&ei);
	}
      if (!warned)
	fold_undefer_and_ignore_overflow_warnings ();
    }
  else
    taken_edge = single_succ_edge (bb);

  bitmap_set_bit (cfgcleanup_altered_bbs, bb->index);
  gsi_remove (&gsi, true);
  taken_edge->flags = EDGE_FALLTHRU;

  return retval;
}

/* Cleanup the GF_CALL_CTRL_ALTERING flag according to
   to updated gimple_call_flags.  */

static void
cleanup_call_ctrl_altering_flag (gimple *bb_end)
{
  if (!is_gimple_call (bb_end)
      || !gimple_call_ctrl_altering_p (bb_end))
    return;

  int flags = gimple_call_flags (bb_end);
  if (((flags & (ECF_CONST | ECF_PURE))
       && !(flags & ECF_LOOPING_CONST_OR_PURE))
      || (flags & ECF_LEAF))
    gimple_call_set_ctrl_altering (bb_end, false);
}

/* Try to remove superfluous control structures in basic block BB.  Returns
   true if anything changes.  */

static bool
cleanup_control_flow_bb (basic_block bb, bool first_p)
{
  gimple_stmt_iterator gsi;
  bool retval = false;
  gimple *stmt;

  /* If the last statement of the block could throw and now cannot,
     we need to prune cfg.  */
  retval |= gimple_purge_dead_eh_edges (bb);

  gsi = gsi_last_nondebug_bb (bb);
  if (gsi_end_p (gsi))
    return retval;

  stmt = gsi_stmt (gsi);

  /* Try to cleanup ctrl altering flag for call which ends bb.  */
  cleanup_call_ctrl_altering_flag (stmt);

  if (gimple_code (stmt) == GIMPLE_COND
      || gimple_code (stmt) == GIMPLE_SWITCH)
    {
      gcc_checking_assert (gsi_stmt (gsi_last_bb (bb)) == stmt);
      retval |= cleanup_control_expr_graph (bb, gsi, first_p);
    }
  else if (gimple_code (stmt) == GIMPLE_GOTO
	   && TREE_CODE (gimple_goto_dest (stmt)) == ADDR_EXPR
	   && (TREE_CODE (TREE_OPERAND (gimple_goto_dest (stmt), 0))
	       == LABEL_DECL))
    {
      /* If we had a computed goto which has a compile-time determinable
	 destination, then we can eliminate the goto.  */
      edge e;
      tree label;
      edge_iterator ei;
      basic_block target_block;

      gcc_checking_assert (gsi_stmt (gsi_last_bb (bb)) == stmt);
      /* First look at all the outgoing edges.  Delete any outgoing
	 edges which do not go to the right block.  For the one
	 edge which goes to the right block, fix up its flags.  */
      label = TREE_OPERAND (gimple_goto_dest (stmt), 0);
      if (DECL_CONTEXT (label) != cfun->decl)
	return retval;
      target_block = label_to_block (label);
      for (ei = ei_start (bb->succs); (e = ei_safe_edge (ei)); )
	{
	  if (e->dest != target_block)
	    remove_edge_and_dominated_blocks (e);
	  else
	    {
	      /* Turn off the EDGE_ABNORMAL flag.  */
	      e->flags &= ~EDGE_ABNORMAL;

	      /* And set EDGE_FALLTHRU.  */
	      e->flags |= EDGE_FALLTHRU;
	      ei_next (&ei);
	    }
	}

      bitmap_set_bit (cfgcleanup_altered_bbs, bb->index);
      bitmap_set_bit (cfgcleanup_altered_bbs, target_block->index);

      /* Remove the GOTO_EXPR as it is not needed.  The CFG has all the
	 relevant information we need.  */
      gsi_remove (&gsi, true);
      retval = true;
    }

  /* Check for indirect calls that have been turned into
     noreturn calls.  */
  else if (is_gimple_call (stmt)
	   && gimple_call_noreturn_p (stmt))
    {
      /* If there are debug stmts after the noreturn call, remove them
	 now, they should be all unreachable anyway.  */
      for (gsi_next (&gsi); !gsi_end_p (gsi); )
	gsi_remove (&gsi, true);
      if (remove_fallthru_edge (bb->succs))
	retval = true;
    }

  return retval;
}

/* Return true if basic block BB does nothing except pass control
   flow to another block and that we can safely insert a label at
   the start of the successor block.

   As a precondition, we require that BB be not equal to
   the entry block.  */

static bool
tree_forwarder_block_p (basic_block bb, bool phi_wanted)
{
  gimple_stmt_iterator gsi;
  location_t locus;

  /* BB must have a single outgoing edge.  */
  if (single_succ_p (bb) != 1
      /* If PHI_WANTED is false, BB must not have any PHI nodes.
	 Otherwise, BB must have PHI nodes.  */
      || gimple_seq_empty_p (phi_nodes (bb)) == phi_wanted
      /* BB may not be a predecessor of the exit block.  */
      || single_succ (bb) == EXIT_BLOCK_PTR_FOR_FN (cfun)
      /* Nor should this be an infinite loop.  */
      || single_succ (bb) == bb
      /* BB may not have an abnormal outgoing edge.  */
      || (single_succ_edge (bb)->flags & EDGE_ABNORMAL))
    return false;

  gcc_checking_assert (bb != ENTRY_BLOCK_PTR_FOR_FN (cfun));

  locus = single_succ_edge (bb)->goto_locus;

  /* There should not be an edge coming from entry, or an EH edge.  */
  {
    edge_iterator ei;
    edge e;

    FOR_EACH_EDGE (e, ei, bb->preds)
      if (e->src == ENTRY_BLOCK_PTR_FOR_FN (cfun) || (e->flags & EDGE_EH))
	return false;
      /* If goto_locus of any of the edges differs, prevent removing
	 the forwarder block for -O0.  */
      else if (optimize == 0 && e->goto_locus != locus)
	return false;
  }

  /* Now walk through the statements backward.  We can ignore labels,
     anything else means this is not a forwarder block.  */
  for (gsi = gsi_last_bb (bb); !gsi_end_p (gsi); gsi_prev (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);

      switch (gimple_code (stmt))
	{
	case GIMPLE_LABEL:
	  if (DECL_NONLOCAL (gimple_label_label (as_a <glabel *> (stmt))))
	    return false;
	  if (optimize == 0 && gimple_location (stmt) != locus)
	    return false;
	  break;

	  /* ??? For now, hope there's a corresponding debug
	     assignment at the destination.  */
	case GIMPLE_DEBUG:
	  break;

	default:
	  return false;
	}
    }

  if (current_loops)
    {
      basic_block dest;
      /* Protect loop headers.  */
      if (bb_loop_header_p (bb))
	return false;

      dest = EDGE_SUCC (bb, 0)->dest;
      /* Protect loop preheaders and latches if requested.  */
      if (dest->loop_father->header == dest)
	{
	  if (bb->loop_father == dest->loop_father)
	    {
	      if (loops_state_satisfies_p (LOOPS_HAVE_SIMPLE_LATCHES))
		return false;
	      /* If bb doesn't have a single predecessor we'd make this
		 loop have multiple latches.  Don't do that if that
		 would in turn require disambiguating them.  */
	      return (single_pred_p (bb)
		      || loops_state_satisfies_p
		      	   (LOOPS_MAY_HAVE_MULTIPLE_LATCHES));
	    }
	  else if (bb->loop_father == loop_outer (dest->loop_father))
	    return !loops_state_satisfies_p (LOOPS_HAVE_PREHEADERS);
	  /* Always preserve other edges into loop headers that are
	     not simple latches or preheaders.  */
	  return false;
	}
    }

  return true;
}

/* If all the PHI nodes in DEST have alternatives for E1 and E2 and
   those alternatives are equal in each of the PHI nodes, then return
   true, else return false.  */

static bool
phi_alternatives_equal (basic_block dest, edge e1, edge e2)
{
  int n1 = e1->dest_idx;
  int n2 = e2->dest_idx;
  gphi_iterator gsi;

  for (gsi = gsi_start_phis (dest); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();
      tree val1 = gimple_phi_arg_def (phi, n1);
      tree val2 = gimple_phi_arg_def (phi, n2);

      gcc_assert (val1 != NULL_TREE);
      gcc_assert (val2 != NULL_TREE);

      if (!operand_equal_for_phi_arg_p (val1, val2))
	return false;
    }

  return true;
}

/* Removes forwarder block BB.  Returns false if this failed.  */

static bool
remove_forwarder_block (basic_block bb)
{
  edge succ = single_succ_edge (bb), e, s;
  basic_block dest = succ->dest;
  gimple *label;
  edge_iterator ei;
  gimple_stmt_iterator gsi, gsi_to;
  bool can_move_debug_stmts;

  /* We check for infinite loops already in tree_forwarder_block_p.
     However it may happen that the infinite loop is created
     afterwards due to removal of forwarders.  */
  if (dest == bb)
    return false;

  /* If the destination block consists of a nonlocal label or is a
     EH landing pad, do not merge it.  */
  label = first_stmt (dest);
  if (label)
    if (glabel *label_stmt = dyn_cast <glabel *> (label))
      if (DECL_NONLOCAL (gimple_label_label (label_stmt))
	  || EH_LANDING_PAD_NR (gimple_label_label (label_stmt)) != 0)
	return false;

  /* If there is an abnormal edge to basic block BB, but not into
     dest, problems might occur during removal of the phi node at out
     of ssa due to overlapping live ranges of registers.

     If there is an abnormal edge in DEST, the problems would occur
     anyway since cleanup_dead_labels would then merge the labels for
     two different eh regions, and rest of exception handling code
     does not like it.

     So if there is an abnormal edge to BB, proceed only if there is
     no abnormal edge to DEST and there are no phi nodes in DEST.  */
  if (bb_has_abnormal_pred (bb)
      && (bb_has_abnormal_pred (dest)
	  || !gimple_seq_empty_p (phi_nodes (dest))))
    return false;

  /* If there are phi nodes in DEST, and some of the blocks that are
     predecessors of BB are also predecessors of DEST, check that the
     phi node arguments match.  */
  if (!gimple_seq_empty_p (phi_nodes (dest)))
    {
      FOR_EACH_EDGE (e, ei, bb->preds)
	{
	  s = find_edge (e->src, dest);
	  if (!s)
	    continue;

	  if (!phi_alternatives_equal (dest, succ, s))
	    return false;
	}
    }

  can_move_debug_stmts = MAY_HAVE_DEBUG_STMTS && single_pred_p (dest);

  basic_block pred = NULL;
  if (single_pred_p (bb))
    pred = single_pred (bb);

  /* Redirect the edges.  */
  for (ei = ei_start (bb->preds); (e = ei_safe_edge (ei)); )
    {
      bitmap_set_bit (cfgcleanup_altered_bbs, e->src->index);

      if (e->flags & EDGE_ABNORMAL)
	{
	  /* If there is an abnormal edge, redirect it anyway, and
	     move the labels to the new block to make it legal.  */
	  s = redirect_edge_succ_nodup (e, dest);
	}
      else
	s = redirect_edge_and_branch (e, dest);

      if (s == e)
	{
	  /* Create arguments for the phi nodes, since the edge was not
	     here before.  */
	  for (gphi_iterator psi = gsi_start_phis (dest);
	       !gsi_end_p (psi);
	       gsi_next (&psi))
	    {
	      gphi *phi = psi.phi ();
	      source_location l = gimple_phi_arg_location_from_edge (phi, succ);
	      tree def = gimple_phi_arg_def (phi, succ->dest_idx);
	      add_phi_arg (phi, unshare_expr (def), s, l);
	    }
	}
    }

  /* Move nonlocal labels and computed goto targets as well as user
     defined labels and labels with an EH landing pad number to the
     new block, so that the redirection of the abnormal edges works,
     jump targets end up in a sane place and debug information for
     labels is retained.  */
  gsi_to = gsi_start_bb (dest);
  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); )
    {
      tree decl;
      label = gsi_stmt (gsi);
      if (is_gimple_debug (label))
	break;
      decl = gimple_label_label (as_a <glabel *> (label));
      if (EH_LANDING_PAD_NR (decl) != 0
	  || DECL_NONLOCAL (decl)
	  || FORCED_LABEL (decl)
	  || !DECL_ARTIFICIAL (decl))
	{
	  gsi_remove (&gsi, false);
	  gsi_insert_before (&gsi_to, label, GSI_SAME_STMT);
	}
      else
	gsi_next (&gsi);
    }

  /* Move debug statements if the destination has a single predecessor.  */
  if (can_move_debug_stmts)
    {
      gsi_to = gsi_after_labels (dest);
      for (gsi = gsi_after_labels (bb); !gsi_end_p (gsi); )
	{
	  gimple *debug = gsi_stmt (gsi);
	  if (!is_gimple_debug (debug))
	    break;
	  gsi_remove (&gsi, false);
	  gsi_insert_before (&gsi_to, debug, GSI_SAME_STMT);
	}
    }

  bitmap_set_bit (cfgcleanup_altered_bbs, dest->index);

  /* Update the dominators.  */
  if (dom_info_available_p (CDI_DOMINATORS))
    {
      basic_block dom, dombb, domdest;

      dombb = get_immediate_dominator (CDI_DOMINATORS, bb);
      domdest = get_immediate_dominator (CDI_DOMINATORS, dest);
      if (domdest == bb)
	{
	  /* Shortcut to avoid calling (relatively expensive)
	     nearest_common_dominator unless necessary.  */
	  dom = dombb;
	}
      else
	dom = nearest_common_dominator (CDI_DOMINATORS, domdest, dombb);

      set_immediate_dominator (CDI_DOMINATORS, dest, dom);
    }

  /* Adjust latch infomation of BB's parent loop as otherwise
     the cfg hook has a hard time not to kill the loop.  */
  if (current_loops && bb->loop_father->latch == bb)
    bb->loop_father->latch = pred;

  /* And kill the forwarder block.  */
  delete_basic_block (bb);

  return true;
}

/* STMT is a call that has been discovered noreturn.  Split the
   block to prepare fixing up the CFG and remove LHS.
   Return true if cleanup-cfg needs to run.  */

bool
fixup_noreturn_call (gimple *stmt)
{
  basic_block bb = gimple_bb (stmt);
  bool changed = false;

  if (gimple_call_builtin_p (stmt, BUILT_IN_RETURN))
    return false;

  /* First split basic block if stmt is not last.  */
  if (stmt != gsi_stmt (gsi_last_bb (bb)))
    {
      if (stmt == gsi_stmt (gsi_last_nondebug_bb (bb)))
	{
	  /* Don't split if there are only debug stmts
	     after stmt, that can result in -fcompare-debug
	     failures.  Remove the debug stmts instead,
	     they should be all unreachable anyway.  */
	  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
	  for (gsi_next (&gsi); !gsi_end_p (gsi); )
	    gsi_remove (&gsi, true);
	}
      else
	{
	  split_block (bb, stmt);
	  changed = true;
	}
    }

  /* If there is an LHS, remove it, but only if its type has fixed size.
     The LHS will need to be recreated during RTL expansion and creating
     temporaries of variable-sized types is not supported.  Also don't
     do this with TREE_ADDRESSABLE types, as assign_temp will abort.
     Drop LHS regardless of TREE_ADDRESSABLE, if the function call
     has been changed into a call that does not return a value, like
     __builtin_unreachable or __cxa_pure_virtual.  */
  tree lhs = gimple_call_lhs (stmt);
  if (lhs
      && (should_remove_lhs_p (lhs)
	  || VOID_TYPE_P (TREE_TYPE (gimple_call_fntype (stmt)))))
    {
      gimple_call_set_lhs (stmt, NULL_TREE);

      /* We need to fix up the SSA name to avoid checking errors.  */
      if (TREE_CODE (lhs) == SSA_NAME)
	{
	  tree new_var = create_tmp_reg (TREE_TYPE (lhs));
	  SET_SSA_NAME_VAR_OR_IDENTIFIER (lhs, new_var);
	  SSA_NAME_DEF_STMT (lhs) = gimple_build_nop ();
	  set_ssa_default_def (cfun, new_var, lhs);
	}

      update_stmt (stmt);
    }

  /* Mark the call as altering control flow.  */
  if (!gimple_call_ctrl_altering_p (stmt))
    {
      gimple_call_set_ctrl_altering (stmt, true);
      changed = true;
    }

  return changed;
}

/* Return true if we want to merge BB1 and BB2 into a single block.  */

static bool
want_merge_blocks_p (basic_block bb1, basic_block bb2)
{
  if (!can_merge_blocks_p (bb1, bb2))
    return false;
  gimple_stmt_iterator gsi = gsi_last_nondebug_bb (bb1);
  if (gsi_end_p (gsi) || !stmt_can_terminate_bb_p (gsi_stmt (gsi)))
    return true;
  return bb1->count.ok_for_merging (bb2->count);
}


/* Tries to cleanup cfg in basic block BB.  Returns true if anything
   changes.  */

static bool
cleanup_tree_cfg_bb (basic_block bb)
{
  if (tree_forwarder_block_p (bb, false)
      && remove_forwarder_block (bb))
    return true;

  /* If there is a merge opportunity with the predecessor
     do nothing now but wait until we process the predecessor.
     This happens when we visit BBs in a non-optimal order and
     avoids quadratic behavior with adjusting stmts BB pointer.  */
  if (single_pred_p (bb)
      && want_merge_blocks_p (single_pred (bb), bb))
    /* But make sure we _do_ visit it.  When we remove unreachable paths
       ending in a backedge we fail to mark the destinations predecessors
       as changed.  */
    bitmap_set_bit (cfgcleanup_altered_bbs, single_pred (bb)->index);

  /* Merging the blocks may create new opportunities for folding
     conditional branches (due to the elimination of single-valued PHI
     nodes).  */
  else if (single_succ_p (bb)
	   && want_merge_blocks_p (bb, single_succ (bb)))
    {
      merge_blocks (bb, single_succ (bb));
      return true;
    }

  return false;
}

/* Iterate the cfg cleanups, while anything changes.  */

static bool
cleanup_tree_cfg_1 (void)
{
  bool retval = false;
  basic_block bb;
  unsigned i, n;

  /* Prepare the worklists of altered blocks.  */
  cfgcleanup_altered_bbs = BITMAP_ALLOC (NULL);

  /* During forwarder block cleanup, we may redirect edges out of
     SWITCH_EXPRs, which can get expensive.  So we want to enable
     recording of edge to CASE_LABEL_EXPR.  */
  start_recording_case_labels ();

  /* We cannot use FOR_EACH_BB_FN for the BB iterations below
     since the basic blocks may get removed.  */

  /* Start by iterating over all basic blocks looking for edge removal
     opportunities.  Do this first because incoming SSA form may be
     invalid and we want to avoid performing SSA related tasks such
     as propgating out a PHI node during BB merging in that state.  */
  n = last_basic_block_for_fn (cfun);
  for (i = NUM_FIXED_BLOCKS; i < n; i++)
    {
      bb = BASIC_BLOCK_FOR_FN (cfun, i);
      if (bb)
	retval |= cleanup_control_flow_bb (bb, true);
    }

  /* After doing the above SSA form should be valid (or an update SSA
     should be required).  */

  /* Continue by iterating over all basic blocks looking for BB merging
     opportunities.  */
  n = last_basic_block_for_fn (cfun);
  for (i = NUM_FIXED_BLOCKS; i < n; i++)
    {
      bb = BASIC_BLOCK_FOR_FN (cfun, i);
      if (bb)
	retval |= cleanup_tree_cfg_bb (bb);
    }

  /* Now process the altered blocks, as long as any are available.  */
  while (!bitmap_empty_p (cfgcleanup_altered_bbs))
    {
      i = bitmap_first_set_bit (cfgcleanup_altered_bbs);
      bitmap_clear_bit (cfgcleanup_altered_bbs, i);
      if (i < NUM_FIXED_BLOCKS)
	continue;

      bb = BASIC_BLOCK_FOR_FN (cfun, i);
      if (!bb)
	continue;

      retval |= cleanup_control_flow_bb (bb, false);
      retval |= cleanup_tree_cfg_bb (bb);
    }

  end_recording_case_labels ();
  BITMAP_FREE (cfgcleanup_altered_bbs);
  return retval;
}

static bool
mfb_keep_latches (edge e)
{
  return ! dominated_by_p (CDI_DOMINATORS, e->src, e->dest);
}

/* Remove unreachable blocks and other miscellaneous clean up work.
   Return true if the flowgraph was modified, false otherwise.  */

static bool
cleanup_tree_cfg_noloop (void)
{
  bool changed;

  timevar_push (TV_TREE_CLEANUP_CFG);

  /* Iterate until there are no more cleanups left to do.  If any
     iteration changed the flowgraph, set CHANGED to true.

     If dominance information is available, there cannot be any unreachable
     blocks.  */
  if (!dom_info_available_p (CDI_DOMINATORS))
    {
      changed = delete_unreachable_blocks ();
      calculate_dominance_info (CDI_DOMINATORS);
    }
  else
    {
      checking_verify_dominators (CDI_DOMINATORS);
      changed = false;
    }

  /* Ensure that we have single entries into loop headers.  Otherwise
     if one of the entries is becoming a latch due to CFG cleanup
     (from formerly being part of an irreducible region) then we mess
     up loop fixup and associate the old loop with a different region
     which makes niter upper bounds invalid.  See for example PR80549.
     This needs to be done before we remove trivially dead edges as
     we need to capture the dominance state before the pending transform.  */
  if (current_loops)
    {
      loop_p loop;
      unsigned i;
      FOR_EACH_VEC_ELT (*get_loops (cfun), i, loop)
	if (loop && loop->header)
	  {
	    basic_block bb = loop->header;
	    edge_iterator ei;
	    edge e;
	    bool found_latch = false;
	    bool any_abnormal = false;
	    unsigned n = 0;
	    /* We are only interested in preserving existing loops, but
	       we need to check whether they are still real and of course
	       if we need to add a preheader at all.  */
	    FOR_EACH_EDGE (e, ei, bb->preds)
	      {
		if (e->flags & EDGE_ABNORMAL)
		  {
		    any_abnormal = true;
		    break;
		  }
		if (dominated_by_p (CDI_DOMINATORS, e->src, bb))
		  {
		    found_latch = true;
		    continue;
		  }
		n++;
	      }
	    /* If we have more than one entry to the loop header
	       create a forwarder.  */
	    if (found_latch && ! any_abnormal && n > 1)
	      {
		edge fallthru = make_forwarder_block (bb, mfb_keep_latches,
						      NULL);
		loop->header = fallthru->dest;
		if (! loops_state_satisfies_p (LOOPS_NEED_FIXUP))
		  {
		    /* The loop updating from the CFG hook is incomplete
		       when we have multiple latches, fixup manually.  */
		    remove_bb_from_loops (fallthru->src);
		    loop_p cloop = loop;
		    FOR_EACH_EDGE (e, ei, fallthru->src->preds)
		      cloop = find_common_loop (cloop, e->src->loop_father);
		    add_bb_to_loop (fallthru->src, cloop);
		  }
	      }
	  }
    }

  changed |= cleanup_tree_cfg_1 ();

  gcc_assert (dom_info_available_p (CDI_DOMINATORS));
  compact_blocks ();

  checking_verify_flow_info ();

  timevar_pop (TV_TREE_CLEANUP_CFG);

  if (changed && current_loops)
    {
      /* Removing edges and/or blocks may make recorded bounds refer
         to stale GIMPLE stmts now, so clear them.  */
      free_numbers_of_iterations_estimates (cfun);
      loops_state_set (LOOPS_NEED_FIXUP);
    }

  return changed;
}

/* Repairs loop structures.  */

static void
repair_loop_structures (void)
{
  bitmap changed_bbs;
  unsigned n_new_loops;

  calculate_dominance_info (CDI_DOMINATORS);

  timevar_push (TV_REPAIR_LOOPS);
  changed_bbs = BITMAP_ALLOC (NULL);
  n_new_loops = fix_loop_structure (changed_bbs);

  /* This usually does nothing.  But sometimes parts of cfg that originally
     were inside a loop get out of it due to edge removal (since they
     become unreachable by back edges from latch).  Also a former
     irreducible loop can become reducible - in this case force a full
     rewrite into loop-closed SSA form.  */
  if (loops_state_satisfies_p (LOOP_CLOSED_SSA))
    rewrite_into_loop_closed_ssa (n_new_loops ? NULL : changed_bbs,
				  TODO_update_ssa);

  BITMAP_FREE (changed_bbs);

  checking_verify_loop_structure ();
  scev_reset ();

  timevar_pop (TV_REPAIR_LOOPS);
}

/* Cleanup cfg and repair loop structures.  */

bool
cleanup_tree_cfg (void)
{
  bool changed = cleanup_tree_cfg_noloop ();

  if (current_loops != NULL
      && loops_state_satisfies_p (LOOPS_NEED_FIXUP))
    repair_loop_structures ();

  return changed;
}

/* Tries to merge the PHI nodes at BB into those at BB's sole successor.
   Returns true if successful.  */

static bool
remove_forwarder_block_with_phi (basic_block bb)
{
  edge succ = single_succ_edge (bb);
  basic_block dest = succ->dest;
  gimple *label;
  basic_block dombb, domdest, dom;

  /* We check for infinite loops already in tree_forwarder_block_p.
     However it may happen that the infinite loop is created
     afterwards due to removal of forwarders.  */
  if (dest == bb)
    return false;

  /* Removal of forwarders may expose new natural loops and thus
     a block may turn into a loop header.  */
  if (current_loops && bb_loop_header_p (bb))
    return false;

  /* If the destination block consists of a nonlocal label, do not
     merge it.  */
  label = first_stmt (dest);
  if (label)
    if (glabel *label_stmt = dyn_cast <glabel *> (label))
      if (DECL_NONLOCAL (gimple_label_label (label_stmt)))
	return false;

  /* Record BB's single pred in case we need to update the father
     loop's latch information later.  */
  basic_block pred = NULL;
  if (single_pred_p (bb))
    pred = single_pred (bb);

  /* Redirect each incoming edge to BB to DEST.  */
  while (EDGE_COUNT (bb->preds) > 0)
    {
      edge e = EDGE_PRED (bb, 0), s;
      gphi_iterator gsi;

      s = find_edge (e->src, dest);
      if (s)
	{
	  /* We already have an edge S from E->src to DEST.  If S and
	     E->dest's sole successor edge have the same PHI arguments
	     at DEST, redirect S to DEST.  */
	  if (phi_alternatives_equal (dest, s, succ))
	    {
	      e = redirect_edge_and_branch (e, dest);
	      redirect_edge_var_map_clear (e);
	      continue;
	    }

	  /* PHI arguments are different.  Create a forwarder block by
	     splitting E so that we can merge PHI arguments on E to
	     DEST.  */
	  e = single_succ_edge (split_edge (e));
	}
      else
	{
	  /* If we merge the forwarder into a loop header verify if we
	     are creating another loop latch edge.  If so, reset
	     number of iteration information of the loop.  */
	  if (dest->loop_father->header == dest
	      && dominated_by_p (CDI_DOMINATORS, e->src, dest))
	    {
	      dest->loop_father->any_upper_bound = false;
	      dest->loop_father->any_likely_upper_bound = false;
	      free_numbers_of_iterations_estimates (dest->loop_father);
	    }
	}

      s = redirect_edge_and_branch (e, dest);

      /* redirect_edge_and_branch must not create a new edge.  */
      gcc_assert (s == e);

      /* Add to the PHI nodes at DEST each PHI argument removed at the
	 destination of E.  */
      for (gsi = gsi_start_phis (dest);
	   !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  gphi *phi = gsi.phi ();
	  tree def = gimple_phi_arg_def (phi, succ->dest_idx);
	  source_location locus = gimple_phi_arg_location_from_edge (phi, succ);

	  if (TREE_CODE (def) == SSA_NAME)
	    {
	      /* If DEF is one of the results of PHI nodes removed during
		 redirection, replace it with the PHI argument that used
		 to be on E.  */
	      vec<edge_var_map> *head = redirect_edge_var_map_vector (e);
	      size_t length = head ? head->length () : 0;
	      for (size_t i = 0; i < length; i++)
		{
		  edge_var_map *vm = &(*head)[i];
		  tree old_arg = redirect_edge_var_map_result (vm);
		  tree new_arg = redirect_edge_var_map_def (vm);

		  if (def == old_arg)
		    {
		      def = new_arg;
		      locus = redirect_edge_var_map_location (vm);
		      break;
		    }
		}
	    }

	  add_phi_arg (phi, def, s, locus);
	}

      redirect_edge_var_map_clear (e);
    }

  /* Update the dominators.  */
  dombb = get_immediate_dominator (CDI_DOMINATORS, bb);
  domdest = get_immediate_dominator (CDI_DOMINATORS, dest);
  if (domdest == bb)
    {
      /* Shortcut to avoid calling (relatively expensive)
	 nearest_common_dominator unless necessary.  */
      dom = dombb;
    }
  else
    dom = nearest_common_dominator (CDI_DOMINATORS, domdest, dombb);

  set_immediate_dominator (CDI_DOMINATORS, dest, dom);

  /* Adjust latch infomation of BB's parent loop as otherwise
     the cfg hook has a hard time not to kill the loop.  */
  if (current_loops && bb->loop_father->latch == bb)
    bb->loop_father->latch = pred;

  /* Remove BB since all of BB's incoming edges have been redirected
     to DEST.  */
  delete_basic_block (bb);

  return true;
}

/* This pass merges PHI nodes if one feeds into another.  For example,
   suppose we have the following:

  goto <bb 9> (<L9>);

<L8>:;
  tem_17 = foo ();

  # tem_6 = PHI <tem_17(8), tem_23(7)>;
<L9>:;

  # tem_3 = PHI <tem_6(9), tem_2(5)>;
<L10>:;

  Then we merge the first PHI node into the second one like so:

  goto <bb 9> (<L10>);

<L8>:;
  tem_17 = foo ();

  # tem_3 = PHI <tem_23(7), tem_2(5), tem_17(8)>;
<L10>:;
*/

namespace {

const pass_data pass_data_merge_phi =
{
  GIMPLE_PASS, /* type */
  "mergephi", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_MERGE_PHI, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_merge_phi : public gimple_opt_pass
{
public:
  pass_merge_phi (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_merge_phi, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_merge_phi (m_ctxt); }
  virtual unsigned int execute (function *);

}; // class pass_merge_phi

unsigned int
pass_merge_phi::execute (function *fun)
{
  basic_block *worklist = XNEWVEC (basic_block, n_basic_blocks_for_fn (fun));
  basic_block *current = worklist;
  basic_block bb;

  calculate_dominance_info (CDI_DOMINATORS);

  /* Find all PHI nodes that we may be able to merge.  */
  FOR_EACH_BB_FN (bb, fun)
    {
      basic_block dest;

      /* Look for a forwarder block with PHI nodes.  */
      if (!tree_forwarder_block_p (bb, true))
	continue;

      dest = single_succ (bb);

      /* We have to feed into another basic block with PHI
	 nodes.  */
      if (gimple_seq_empty_p (phi_nodes (dest))
	  /* We don't want to deal with a basic block with
	     abnormal edges.  */
	  || bb_has_abnormal_pred (bb))
	continue;

      if (!dominated_by_p (CDI_DOMINATORS, dest, bb))
	{
	  /* If BB does not dominate DEST, then the PHI nodes at
	     DEST must be the only users of the results of the PHI
	     nodes at BB.  */
	  *current++ = bb;
	}
      else
	{
	  gphi_iterator gsi;
	  unsigned int dest_idx = single_succ_edge (bb)->dest_idx;

	  /* BB dominates DEST.  There may be many users of the PHI
	     nodes in BB.  However, there is still a trivial case we
	     can handle.  If the result of every PHI in BB is used
	     only by a PHI in DEST, then we can trivially merge the
	     PHI nodes from BB into DEST.  */
	  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi);
	       gsi_next (&gsi))
	    {
	      gphi *phi = gsi.phi ();
	      tree result = gimple_phi_result (phi);
	      use_operand_p imm_use;
	      gimple *use_stmt;

	      /* If the PHI's result is never used, then we can just
		 ignore it.  */
	      if (has_zero_uses (result))
		continue;

	      /* Get the single use of the result of this PHI node.  */
  	      if (!single_imm_use (result, &imm_use, &use_stmt)
		  || gimple_code (use_stmt) != GIMPLE_PHI
		  || gimple_bb (use_stmt) != dest
		  || gimple_phi_arg_def (use_stmt, dest_idx) != result)
		break;
	    }

	  /* If the loop above iterated through all the PHI nodes
	     in BB, then we can merge the PHIs from BB into DEST.  */
	  if (gsi_end_p (gsi))
	    *current++ = bb;
	}
    }

  /* Now let's drain WORKLIST.  */
  bool changed = false;
  while (current != worklist)
    {
      bb = *--current;
      changed |= remove_forwarder_block_with_phi (bb);
    }
  free (worklist);

  /* Removing forwarder blocks can cause formerly irreducible loops
     to become reducible if we merged two entry blocks.  */
  if (changed
      && current_loops)
    loops_state_set (LOOPS_NEED_FIXUP);

  return 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_merge_phi (gcc::context *ctxt)
{
  return new pass_merge_phi (ctxt);
}

/* Pass: cleanup the CFG just before expanding trees to RTL.
   This is just a round of label cleanups and case node grouping
   because after the tree optimizers have run such cleanups may
   be necessary.  */

static unsigned int
execute_cleanup_cfg_post_optimizing (void)
{
  unsigned int todo = execute_fixup_cfg ();
  if (cleanup_tree_cfg ())
    {
      todo &= ~TODO_cleanup_cfg;
      todo |= TODO_update_ssa;
    }
  maybe_remove_unreachable_handlers ();
  cleanup_dead_labels ();
  if (group_case_labels ())
    todo |= TODO_cleanup_cfg;
  if ((flag_compare_debug_opt || flag_compare_debug)
      && flag_dump_final_insns)
    {
      FILE *final_output = fopen (flag_dump_final_insns, "a");

      if (!final_output)
	{
	  error ("could not open final insn dump file %qs: %m",
		 flag_dump_final_insns);
	  flag_dump_final_insns = NULL;
	}
      else
	{
	  int save_unnumbered = flag_dump_unnumbered;
	  int save_noaddr = flag_dump_noaddr;

	  flag_dump_noaddr = flag_dump_unnumbered = 1;
	  fprintf (final_output, "\n");
	  dump_enumerated_decls (final_output, dump_flags | TDF_NOUID);
	  flag_dump_noaddr = save_noaddr;
	  flag_dump_unnumbered = save_unnumbered;
	  if (fclose (final_output))
	    {
	      error ("could not close final insn dump file %qs: %m",
		     flag_dump_final_insns);
	      flag_dump_final_insns = NULL;
	    }
	}
    }
  return todo;
}

namespace {

const pass_data pass_data_cleanup_cfg_post_optimizing =
{
  GIMPLE_PASS, /* type */
  "optimized", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_CLEANUP_CFG, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_remove_unused_locals, /* todo_flags_finish */
};

class pass_cleanup_cfg_post_optimizing : public gimple_opt_pass
{
public:
  pass_cleanup_cfg_post_optimizing (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_cleanup_cfg_post_optimizing, ctxt)
  {}

  /* opt_pass methods: */
  virtual unsigned int execute (function *)
    {
      return execute_cleanup_cfg_post_optimizing ();
    }

}; // class pass_cleanup_cfg_post_optimizing

} // anon namespace

gimple_opt_pass *
make_pass_cleanup_cfg_post_optimizing (gcc::context *ctxt)
{
  return new pass_cleanup_cfg_post_optimizing (ctxt);
}


