/* CFG cleanup for trees.
   Copyright (C) 2001-2014 Free Software Foundation, Inc.

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
#include "tm.h"
#include "tree.h"
#include "tm_p.h"
#include "basic-block.h"
#include "diagnostic-core.h"
#include "flags.h"
#include "function.h"
#include "langhooks.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "tree-eh.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimple-ssa.h"
#include "tree-cfg.h"
#include "tree-phinodes.h"
#include "ssa-iterators.h"
#include "stringpool.h"
#include "tree-ssanames.h"
#include "tree-ssa-loop-manip.h"
#include "expr.h"
#include "tree-dfa.h"
#include "tree-ssa.h"
#include "tree-pass.h"
#include "except.h"
#include "cfgloop.h"
#include "hashtab.h"
#include "tree-ssa-propagate.h"
#include "tree-scalar-evolution.h"

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


/* Disconnect an unreachable block in the control expression starting
   at block BB.  */

static bool
cleanup_control_expr_graph (basic_block bb, gimple_stmt_iterator gsi)
{
  edge taken_edge;
  bool retval = false;
  gimple stmt = gsi_stmt (gsi);
  tree val;

  if (!single_succ_p (bb))
    {
      edge e;
      edge_iterator ei;
      bool warned;
      location_t loc;

      fold_defer_overflow_warnings ();
      loc = gimple_location (stmt);
      switch (gimple_code (stmt))
	{
	case GIMPLE_COND:
	  val = fold_binary_loc (loc, gimple_cond_code (stmt),
				 boolean_type_node,
			         gimple_cond_lhs (stmt),
				 gimple_cond_rhs (stmt));
	  break;

	case GIMPLE_SWITCH:
	  val = gimple_switch_index (stmt);
	  break;

	default:
	  val = NULL_TREE;
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
      if (taken_edge->probability > REG_BR_PROB_BASE)
	taken_edge->probability = REG_BR_PROB_BASE;
    }
  else
    taken_edge = single_succ_edge (bb);

  bitmap_set_bit (cfgcleanup_altered_bbs, bb->index);
  gsi_remove (&gsi, true);
  taken_edge->flags = EDGE_FALLTHRU;

  return retval;
}

/* Try to remove superfluous control structures in basic block BB.  Returns
   true if anything changes.  */

static bool
cleanup_control_flow_bb (basic_block bb)
{
  gimple_stmt_iterator gsi;
  bool retval = false;
  gimple stmt;

  /* If the last statement of the block could throw and now cannot,
     we need to prune cfg.  */
  retval |= gimple_purge_dead_eh_edges (bb);

  gsi = gsi_last_bb (bb);
  if (gsi_end_p (gsi))
    return retval;

  stmt = gsi_stmt (gsi);

  if (gimple_code (stmt) == GIMPLE_COND
      || gimple_code (stmt) == GIMPLE_SWITCH)
    retval |= cleanup_control_expr_graph (bb, gsi);
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

      /* First look at all the outgoing edges.  Delete any outgoing
	 edges which do not go to the right block.  For the one
	 edge which goes to the right block, fix up its flags.  */
      label = TREE_OPERAND (gimple_goto_dest (stmt), 0);
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
           && gimple_call_noreturn_p (stmt)
           && remove_fallthru_edge (bb->succs))
    retval = true;

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
      gimple stmt = gsi_stmt (gsi);

      switch (gimple_code (stmt))
	{
	case GIMPLE_LABEL:
	  if (DECL_NONLOCAL (gimple_label_label (stmt)))
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
      if (bb->loop_father->header == bb)
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
  gimple_stmt_iterator gsi;

  for (gsi = gsi_start_phis (dest); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple phi = gsi_stmt (gsi);
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
  gimple label;
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
  if (label
      && gimple_code (label) == GIMPLE_LABEL
      && (DECL_NONLOCAL (gimple_label_label (label))
	  || EH_LANDING_PAD_NR (gimple_label_label (label)) != 0))
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
	  for (gsi = gsi_start_phis (dest);
	       !gsi_end_p (gsi);
	       gsi_next (&gsi))
	    {
	      gimple phi = gsi_stmt (gsi);
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
      decl = gimple_label_label (label);
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
	  gimple debug = gsi_stmt (gsi);
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

/* STMT is a call that has been discovered noreturn.  Fixup the CFG
   and remove LHS.  Return true if something changed.  */

bool
fixup_noreturn_call (gimple stmt)
{
  basic_block bb = gimple_bb (stmt);
  bool changed = false;

  if (gimple_call_builtin_p (stmt, BUILT_IN_RETURN))
    return false;

  /* First split basic block if stmt is not last.  */
  if (stmt != gsi_stmt (gsi_last_bb (bb)))
    split_block (bb, stmt);

  changed |= remove_fallthru_edge (bb->succs);

  /* If there is LHS, remove it.  */
  if (gimple_call_lhs (stmt))
    {
      tree op = gimple_call_lhs (stmt);
      gimple_call_set_lhs (stmt, NULL_TREE);

      /* We need to remove SSA name to avoid checking errors.
	 All uses are dominated by the noreturn and thus will
	 be removed afterwards.
	 We proactively remove affected non-PHI statements to avoid
	 fixup_cfg from trying to update them and crashing.  */
      if (TREE_CODE (op) == SSA_NAME)
	{
	  use_operand_p use_p;
          imm_use_iterator iter;
	  gimple use_stmt;
	  bitmap_iterator bi;
	  unsigned int bb_index;

	  bitmap blocks = BITMAP_ALLOC (NULL);

          FOR_EACH_IMM_USE_STMT (use_stmt, iter, op)
	    {
	      if (gimple_code (use_stmt) != GIMPLE_PHI)
	        bitmap_set_bit (blocks, gimple_bb (use_stmt)->index);
	      else
		FOR_EACH_IMM_USE_ON_STMT (use_p, iter)
		  SET_USE (use_p, error_mark_node);
	    }
	  EXECUTE_IF_SET_IN_BITMAP (blocks, 0, bb_index, bi)
	    delete_basic_block (BASIC_BLOCK_FOR_FN (cfun, bb_index));
	  BITMAP_FREE (blocks);
	  release_ssa_name (op);
	}
      update_stmt (stmt);
      changed = true;
    }
  return changed;
}


/* Split basic blocks on calls in the middle of a basic block that are now
   known not to return, and remove the unreachable code.  */

static bool
split_bbs_on_noreturn_calls (void)
{
  bool changed = false;
  gimple stmt;
  basic_block bb;

  /* Detect cases where a mid-block call is now known not to return.  */
  if (cfun->gimple_df)
    while (vec_safe_length (MODIFIED_NORETURN_CALLS (cfun)))
      {
	stmt = MODIFIED_NORETURN_CALLS (cfun)->pop ();
	bb = gimple_bb (stmt);
	/* BB might be deleted at this point, so verify first
	   BB is present in the cfg.  */
	if (bb == NULL
	    || bb->index < NUM_FIXED_BLOCKS
	    || bb->index >= last_basic_block_for_fn (cfun)
	    || BASIC_BLOCK_FOR_FN (cfun, bb->index) != bb
	    || !gimple_call_noreturn_p (stmt))
	  continue;

	changed |= fixup_noreturn_call (stmt);
      }

  return changed;
}

/* Tries to cleanup cfg in basic block BB.  Returns true if anything
   changes.  */

static bool
cleanup_tree_cfg_bb (basic_block bb)
{
  bool retval = cleanup_control_flow_bb (bb);

  if (tree_forwarder_block_p (bb, false)
      && remove_forwarder_block (bb))
    return true;

  /* Merging the blocks may create new opportunities for folding
     conditional branches (due to the elimination of single-valued PHI
     nodes).  */
  if (single_succ_p (bb)
      && can_merge_blocks_p (bb, single_succ (bb)))
    {
      merge_blocks (bb, single_succ (bb));
      return true;
    }

  return retval;
}

/* Iterate the cfg cleanups, while anything changes.  */

static bool
cleanup_tree_cfg_1 (void)
{
  bool retval = false;
  basic_block bb;
  unsigned i, n;

  retval |= split_bbs_on_noreturn_calls ();

  /* Prepare the worklists of altered blocks.  */
  cfgcleanup_altered_bbs = BITMAP_ALLOC (NULL);

  /* During forwarder block cleanup, we may redirect edges out of
     SWITCH_EXPRs, which can get expensive.  So we want to enable
     recording of edge to CASE_LABEL_EXPR.  */
  start_recording_case_labels ();

  /* Start by iterating over all basic blocks.  We cannot use FOR_EACH_BB_FN,
     since the basic blocks may get removed.  */
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

      retval |= cleanup_tree_cfg_bb (bb);

      /* Rerun split_bbs_on_noreturn_calls, in case we have altered any noreturn
	 calls.  */
      retval |= split_bbs_on_noreturn_calls ();
    }

  end_recording_case_labels ();
  BITMAP_FREE (cfgcleanup_altered_bbs);
  return retval;
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
#ifdef ENABLE_CHECKING
      verify_dominators (CDI_DOMINATORS);
#endif
      changed = false;
    }

  changed |= cleanup_tree_cfg_1 ();

  gcc_assert (dom_info_available_p (CDI_DOMINATORS));
  compact_blocks ();

#ifdef ENABLE_CHECKING
  verify_flow_info ();
#endif

  timevar_pop (TV_TREE_CLEANUP_CFG);

  if (changed && current_loops)
    loops_state_set (LOOPS_NEED_FIXUP);

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

#ifdef ENABLE_CHECKING
  verify_loop_structure ();
#endif
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
  gimple label;
  basic_block dombb, domdest, dom;

  /* We check for infinite loops already in tree_forwarder_block_p.
     However it may happen that the infinite loop is created
     afterwards due to removal of forwarders.  */
  if (dest == bb)
    return false;

  /* If the destination block consists of a nonlocal label, do not
     merge it.  */
  label = first_stmt (dest);
  if (label
      && gimple_code (label) == GIMPLE_LABEL
      && DECL_NONLOCAL (gimple_label_label (label)))
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
      gimple_stmt_iterator gsi;

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

      s = redirect_edge_and_branch (e, dest);

      /* redirect_edge_and_branch must not create a new edge.  */
      gcc_assert (s == e);

      /* Add to the PHI nodes at DEST each PHI argument removed at the
	 destination of E.  */
      for (gsi = gsi_start_phis (dest);
	   !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  gimple phi = gsi_stmt (gsi);
	  tree def = gimple_phi_arg_def (phi, succ->dest_idx);
	  source_location locus = gimple_phi_arg_location_from_edge (phi, succ);

	  if (TREE_CODE (def) == SSA_NAME)
	    {
	      edge_var_map_vector *head;
	      edge_var_map *vm;
	      size_t i;

	      /* If DEF is one of the results of PHI nodes removed during
		 redirection, replace it with the PHI argument that used
		 to be on E.  */
	      head = redirect_edge_var_map_vector (e);
	      FOR_EACH_VEC_SAFE_ELT (head, i, vm)
		{
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
  true, /* has_execute */
  TV_TREE_MERGE_PHI, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_verify_ssa, /* todo_flags_finish */
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
	  gimple_stmt_iterator gsi;
	  unsigned int dest_idx = single_succ_edge (bb)->dest_idx;

	  /* BB dominates DEST.  There may be many users of the PHI
	     nodes in BB.  However, there is still a trivial case we
	     can handle.  If the result of every PHI in BB is used
	     only by a PHI in DEST, then we can trivially merge the
	     PHI nodes from BB into DEST.  */
	  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi);
	       gsi_next (&gsi))
	    {
	      gimple phi = gsi_stmt (gsi);
	      tree result = gimple_phi_result (phi);
	      use_operand_p imm_use;
	      gimple use_stmt;

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
  unsigned int todo = 0;
  if (cleanup_tree_cfg ())
    todo |= TODO_update_ssa;
  maybe_remove_unreachable_handlers ();
  cleanup_dead_labels ();
  group_case_labels ();
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
  true, /* has_execute */
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


