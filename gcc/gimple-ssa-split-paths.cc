/* Support routines for Splitting Paths to loop backedges
   Copyright (C) 2015-2025 Free Software Foundation, Inc.
   Contributed by Ajit Kumar Agarwal <ajitkum@xilinx.com>.

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
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "tree-cfg.h"
#include "cfganal.h"
#include "cfgloop.h"
#include "gimple-iterator.h"
#include "tracer.h"
#include "predict.h"
#include "gimple-ssa.h"
#include "tree-phinodes.h"
#include "ssa-iterators.h"
#include "fold-const.h"
#include "cfghooks.h"

/* Given LATCH, the latch block in a loop, see if the shape of the
   path reaching LATCH is suitable for being split by duplication.
   If so, return the block that will be duplicated into its predecessor
   paths.  Else return NULL.  */

static basic_block
find_block_to_duplicate_for_splitting_paths (basic_block latch)
{
  /* We should have simple latches at this point.  So the latch should
     have a single successor.  This implies the predecessor of the latch
     likely has the loop exit.  And it's that predecessor we're most
     interested in. To keep things simple, we're going to require that
     the latch have a single predecessor too.  */
  if (single_succ_p (latch) && single_pred_p (latch))
    {
      basic_block bb = get_immediate_dominator (CDI_DOMINATORS, latch);
      gcc_assert (single_pred_edge (latch)->src == bb);

      /* If BB has been marked as not to be duplicated, then honor that
	 request.  */
      if (ignore_bb_p (bb))
	return NULL;

      gimple *last = gsi_stmt (gsi_last_nondebug_bb (bb));
      /* The immediate dominator of the latch must end in a conditional.  */
      if (!last || gimple_code (last) != GIMPLE_COND)
	return NULL;

      /* We're hoping that BB is a join point for an IF-THEN-ELSE diamond
	 region.  Verify that it is.

	 First, verify that BB has two predecessors (each arm of the
	 IF-THEN-ELSE) and two successors (the latch and exit) and that
	 all edges are normal.  */
      if (EDGE_COUNT (bb->preds) == 2
	  && !(EDGE_PRED (bb, 0)->flags & EDGE_COMPLEX)
	  && !(EDGE_PRED (bb, 1)->flags & EDGE_COMPLEX)
	  && EDGE_COUNT (bb->succs) == 2
	  && !(EDGE_SUCC (bb, 0)->flags & EDGE_COMPLEX)
	  && !(EDGE_SUCC (bb, 1)->flags & EDGE_COMPLEX))
	{
	  /* Now verify that BB's immediate dominator ends in a
	     conditional as well.  */
	  basic_block bb_idom = get_immediate_dominator (CDI_DOMINATORS, bb);
	  gimple *last = gsi_stmt (gsi_last_nondebug_bb (bb_idom));
	  if (!last || gimple_code (last) != GIMPLE_COND)
	    return NULL;

	  /* And that BB's immediate dominator's successors are the
	     predecessors of BB or BB itself.  */
	  if (!(EDGE_PRED (bb, 0)->src == bb_idom
		|| find_edge (bb_idom, EDGE_PRED (bb, 0)->src))
	      || !(EDGE_PRED (bb, 1)->src == bb_idom
		   || find_edge (bb_idom, EDGE_PRED (bb, 1)->src)))
	    return NULL;

	  /* And that the predecessors of BB each have a single successor
	     or are BB's immediate domiator itself.  */
	  if (!(EDGE_PRED (bb, 0)->src == bb_idom
		|| single_succ_p (EDGE_PRED (bb, 0)->src))
	      || !(EDGE_PRED (bb, 1)->src == bb_idom
		   || single_succ_p (EDGE_PRED (bb, 1)->src)))
	    return NULL;

	  /* So at this point we have a simple diamond for an IF-THEN-ELSE
	     construct starting at BB_IDOM, with a join point at BB.  BB
	     pass control outside the loop or to the loop latch.

	     We're going to want to create two duplicates of BB, one for
	     each successor of BB_IDOM.  */
	  return bb;
	}
    }
  return NULL;
}

/* Return the number of non-debug statements in a block.  */
static unsigned int
count_stmts_in_block (basic_block bb)
{
  gimple_stmt_iterator gsi;
  unsigned int num_stmts = 0;

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      if (!is_gimple_debug (stmt))
	num_stmts++;
    }
  return num_stmts;
}

/* Return TRUE if CODE represents a tree code that is not likely to
   be easily if-convertable because it likely expands into multiple
   insns, FALSE otherwise.  */
static bool
poor_ifcvt_candidate_code (enum tree_code code)
{
  return (code == MIN_EXPR
	  || code == MAX_EXPR
	  || code == ABS_EXPR
	  || code == COND_EXPR
	  || code == CALL_EXPR);
}

/* Return TRUE if PRED of BB is an poor ifcvt candidate. */
static bool
poor_ifcvt_pred (basic_block pred, basic_block bb)
{
  /* If the edge count of the pred is not 1, then
     this is the predecessor from the if rather
     than middle one. */
  if (EDGE_COUNT (pred->succs) != 1)
    return false;

  /* Empty middle bb are never a poor ifcvt candidate. */
  if (empty_block_p (pred))
    return false;
  /* If BB's predecessors are single statement blocks where
     the output of that statement feed the same PHI in BB,
     it an ifcvt candidate. */
  gimple *stmt = last_and_only_stmt (pred);
  if (!stmt || gimple_code (stmt) != GIMPLE_ASSIGN)
    return true;
  tree_code code = gimple_assign_rhs_code (stmt);
  if (poor_ifcvt_candidate_code (code))
    return true;
  tree lhs = gimple_assign_lhs (stmt);
  gimple_stmt_iterator gsi;
  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple *phi = gsi_stmt (gsi);
      if (gimple_phi_arg_def (phi, 0) == lhs
	  || gimple_phi_arg_def (phi, 1) == lhs)
	return false;
    }
  return true;
}

/* Return TRUE if BB is a reasonable block to duplicate by examining
   its size, false otherwise.  BB will always be a loop latch block.

   Things to consider:

     We do not want to spoil if-conversion if at all possible.

     Most of the benefit seems to be from eliminating the unconditional
     jump rather than CSE/DCE opportunities.  So favor duplicating
     small latches.  A latch with just a conditional branch is ideal.

     CSE/DCE opportunties crop up when statements from the predecessors
     feed statements in the latch and allow statements in the latch to
     simplify.  */

static bool
is_feasible_trace (basic_block bb)
{
  basic_block pred1 = EDGE_PRED (bb, 0)->src;
  basic_block pred2 = EDGE_PRED (bb, 1)->src;
  int num_stmts_in_join = count_stmts_in_block (bb);
  int num_stmts_in_pred1
    = EDGE_COUNT (pred1->succs) == 1 ? count_stmts_in_block (pred1) : 0;
  int num_stmts_in_pred2
    = EDGE_COUNT (pred2->succs) == 1 ? count_stmts_in_block (pred2) : 0;

  /* Upper Hard limit on the number statements to copy.  */
  if (num_stmts_in_join
      >= param_max_jump_thread_duplication_stmts)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "Duplicating block %d would duplicate "
		 "too many statments: %d >= %d\n",
		 bb->index, num_stmts_in_join,
		 param_max_jump_thread_duplication_stmts);
      return false;
    }

  /* This is meant to catch cases that are likely opportunities for
     if-conversion.  */
  if (num_stmts_in_pred1 <= 1 && num_stmts_in_pred2 <= 1)
    {
      int num_phis = 0;
      /* The max number of PHIs that should be considered for an ifcvt
	 candidate.  */
      const int max_num_phis = 3;
      for (gphi_iterator si = gsi_start_phis (bb); ! gsi_end_p (si);
	  gsi_next (&si))
	{
	  num_phis++;
	  if (num_phis > max_num_phis)
	    break;
	}
      if (num_phis <= max_num_phis
	  && !poor_ifcvt_pred (pred1, bb)
	  && !poor_ifcvt_pred (pred2, bb))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file,
		     "Block %d appears to be a join point for "
		     "if-convertable bbs.\n",
		     bb->index);
	  return false;
	}
    }

  /* If the joiner has no PHIs with useful uses there is zero chance
     of CSE/DCE/jump-threading possibilities exposed by duplicating it.  */
  bool found_useful_phi = false;
  for (gphi_iterator si = gsi_start_phis (bb); ! gsi_end_p (si);
       gsi_next (&si))
    {
      gphi *phi = si.phi ();
      use_operand_p use_p;
      imm_use_iterator iter;
      FOR_EACH_IMM_USE_FAST (use_p, iter, gimple_phi_result (phi))
	{
	  gimple *stmt = USE_STMT (use_p);
	  if (is_gimple_debug (stmt))
	    continue;
	  /* If there's a use in the joiner this might be a CSE/DCE
	     opportunity, but not if the use is in a conditional
	     which makes this a likely if-conversion candidate.  */
	  if (gimple_bb (stmt) == bb
	      && (!is_gimple_assign (stmt)
		  || (TREE_CODE_CLASS (gimple_assign_rhs_code (stmt))
		      != tcc_comparison)))
	    {
	      found_useful_phi = true;
	      break;
	    }
	  /* If the use is on a loop header PHI and on one path the
	     value is unchanged this might expose a jump threading
	     opportunity.  */
	  if (gimple_code (stmt) == GIMPLE_PHI
	      && gimple_bb (stmt) == bb->loop_father->header
	      /* But for memory the PHI alone isn't good enough.  */
	      && ! virtual_operand_p (gimple_phi_result (stmt)))
	    {
	      bool found_unchanged_path = false;
	      for (unsigned i = 0; i < gimple_phi_num_args (phi); ++i)
		if (gimple_phi_arg_def (phi, i) == gimple_phi_result (stmt))
		  {
		    found_unchanged_path = true;
		    break;
		  }
	      /* If we found an unchanged path this can only be a threading
	         opportunity if we have uses of the loop header PHI result
		 in a stmt dominating the merge block.  Otherwise the
		 splitting may prevent if-conversion.  */
	      if (found_unchanged_path)
		{
		  use_operand_p use2_p;
		  imm_use_iterator iter2;
		  FOR_EACH_IMM_USE_FAST (use2_p, iter2, gimple_phi_result (stmt))
		    {
		      gimple *use_stmt = USE_STMT (use2_p);
		      if (is_gimple_debug (use_stmt))
			continue;
		      basic_block use_bb = gimple_bb (use_stmt);
		      if (use_bb != bb
			  && dominated_by_p (CDI_DOMINATORS, bb, use_bb))
			{
			  if (gcond *cond = dyn_cast <gcond *> (use_stmt))
			    if (gimple_cond_code (cond) == EQ_EXPR
				|| gimple_cond_code (cond) == NE_EXPR)
			      found_useful_phi = true;
			  break;
			}
		    }
		}
	      if (found_useful_phi)
		break;
	    }
	}
      if (found_useful_phi)
	break;
    }
  /* There is one exception namely a controlling condition we can propagate
     an equivalence from to the joiner.  */
  bool found_cprop_opportunity = false;
  basic_block dom = get_immediate_dominator (CDI_DOMINATORS, bb);
  gcond *cond = as_a <gcond *> (*gsi_last_bb (dom));
  if (gimple_cond_code (cond) == EQ_EXPR
      || gimple_cond_code (cond) == NE_EXPR)
    for (unsigned i = 0; i < 2; ++i)
      {
	tree op = gimple_op (cond, i);
	if (TREE_CODE (op) == SSA_NAME)
	  {
	    use_operand_p use_p;
	    imm_use_iterator iter;
	    FOR_EACH_IMM_USE_FAST (use_p, iter, op)
	      {
		if (is_gimple_debug (USE_STMT (use_p)))
		  continue;
		if (gimple_bb (USE_STMT (use_p)) == bb)
		  {
		    found_cprop_opportunity = true;
		    break;
		  }
	      }
	  }
	if (found_cprop_opportunity)
	  break;
      }

  if (! found_useful_phi && ! found_cprop_opportunity)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "Block %d is a join that does not expose CSE/DCE/jump-thread "
		 "opportunities when duplicated.\n",
		 bb->index);
      return false;
    }

  /* We may want something here which looks at dataflow and tries
     to guess if duplication of BB is likely to result in simplification
     of instructions in BB in either the original or the duplicate.  */
  return true;
}

/* If the immediate dominator of the latch of the loop is
   block with conditional branch, then the loop latch  is
   duplicated to its predecessors path preserving the SSA
   semantics.

   CFG before transformation.

              2
              |
              |
        +---->3
        |    / \
        |   /   \
        |  4     5
        |   \   /
        |    \ /
        |     6
        |    / \
        |   /   \
        |  8     7
        |  |     |
        ---+     E



    Block 8 is the latch.  We're going to make copies of block 6 (9 & 10)
    and wire things up so they look like this:

              2
              |
              |
        +---->3
        |    / \
        |   /   \
        |  4     5
        |  |     |
        |  |     |
        |  9    10
        |  |\   /|
        |  | \ / |
        |  |  7  |
        |  |  |  |
        |  |  E  |
        |  |     |
        |   \   /
        |    \ /
        +-----8


    Blocks 9 and 10 will get merged into blocks 4 & 5 respectively which
    enables CSE, DCE and other optimizations to occur on a larger block
    of code.   */

static bool
split_paths ()
{
  bool changed = false;

  loop_optimizer_init (LOOPS_NORMAL | LOOPS_HAVE_RECORDED_EXITS);
  initialize_original_copy_tables ();
  calculate_dominance_info (CDI_DOMINATORS);

  for (auto loop : loops_list (cfun, LI_FROM_INNERMOST))
    {
      /* Only split paths if we are optimizing this loop for speed.  */
      if (!optimize_loop_for_speed_p (loop))
	continue;

      /* See if there is a block that we can duplicate to split the
	 path to the loop latch.  */
      basic_block bb
	= find_block_to_duplicate_for_splitting_paths (loop->latch);

      /* BB is the merge point for an IF-THEN-ELSE we want to transform.

	 Essentially we want to create a duplicate of bb and redirect the
	 first predecessor of BB to the duplicate (leaving the second
	 predecessor as is.  This will split the path leading to the latch
	 re-using BB to avoid useless copying.  */
      if (bb && is_feasible_trace (bb))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file,
		     "Duplicating join block %d into predecessor paths\n",
		     bb->index);
	  basic_block pred0 = EDGE_PRED (bb, 0)->src;
	  if (EDGE_COUNT (pred0->succs) != 1)
	    pred0 = EDGE_PRED (bb, 1)->src;
	  transform_duplicate (pred0, bb);
	  changed = true;

	  /* If BB has an outgoing edge marked as IRREDUCIBLE, then
	     duplicating BB may result in an irreducible region turning
	     into a natural loop.

	     Long term we might want to hook this into the block
	     duplication code, but as we've seen with similar changes
	     for edge removal, that can be somewhat risky.  */
	  if (EDGE_SUCC (bb, 0)->flags & EDGE_IRREDUCIBLE_LOOP
	      || EDGE_SUCC (bb, 1)->flags & EDGE_IRREDUCIBLE_LOOP)
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		  fprintf (dump_file,
			   "Join block %d has EDGE_IRREDUCIBLE_LOOP set.  "
			   "Scheduling loop fixups.\n",
			   bb->index);
	      loops_state_set (LOOPS_NEED_FIXUP);
	    }
	}
    }

  loop_optimizer_finalize ();
  free_original_copy_tables ();
  return changed;
}

/* Main entry point for splitting paths.  Returns TODO_cleanup_cfg if any
   paths where split, otherwise return zero.  */

static unsigned int
execute_split_paths ()
{
  /* If we don't have at least 2 real blocks and backedges in the
     CFG, then there's no point in trying to perform path splitting.  */
  if (n_basic_blocks_for_fn (cfun) <= NUM_FIXED_BLOCKS + 1
      || !mark_dfs_back_edges ())
    return 0;

  bool changed = split_paths();
  if (changed)
    free_dominance_info (CDI_DOMINATORS);

  return changed ? TODO_cleanup_cfg : 0;
}

static bool
gate_split_paths ()
{
  return flag_split_paths;
}

namespace {

const pass_data pass_data_split_paths =
{
  GIMPLE_PASS, /* type */
  "split-paths", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_SPLIT_PATHS, /* tv_id */
  PROP_ssa, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa, /* todo_flags_finish */
};

class pass_split_paths : public gimple_opt_pass
{
   public:
    pass_split_paths (gcc::context *ctxt)
      : gimple_opt_pass (pass_data_split_paths, ctxt)
    {}
   /* opt_pass methods: */
  opt_pass * clone () final override { return new pass_split_paths (m_ctxt); }
  bool gate (function *) final override { return gate_split_paths (); }
  unsigned int execute (function *) final override
  {
    return execute_split_paths ();
  }

}; // class pass_split_paths

} // anon namespace

gimple_opt_pass *
make_pass_split_paths (gcc::context *ctxt)
{
  return new pass_split_paths (ctxt);
}
