/* Support routines for Splitting Paths to loop backedges
   Copyright (C) 2015-2016 Free Software Foundation, Inc.
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
#include "params.h"

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
	 IF-THEN-ELSE) and two successors (the latch and exit).  */
      if (EDGE_COUNT (bb->preds) == 2 && EDGE_COUNT (bb->succs) == 2)
	{
	  /* Now verify that BB's immediate dominator ends in a
	     conditional as well.  */
	  basic_block bb_idom = get_immediate_dominator (CDI_DOMINATORS, bb);
	  gimple *last = gsi_stmt (gsi_last_nondebug_bb (bb_idom));
	  if (!last || gimple_code (last) != GIMPLE_COND)
	    return NULL;

	  /* And that BB's immediate dominator's successors are the
	     predecessors of BB.  */
	  if (!find_edge (bb_idom, EDGE_PRED (bb, 0)->src)
	      || !find_edge (bb_idom, EDGE_PRED (bb, 1)->src))
	    return NULL;

	  /* And that the predecessors of BB each have a single successor.  */
	  if (!single_succ_p (EDGE_PRED (bb, 0)->src)
	      || !single_succ_p (EDGE_PRED (bb, 1)->src))
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
  int num_stmts_in_pred1 = count_stmts_in_block (pred1);
  int num_stmts_in_pred2 = count_stmts_in_block (pred2);

  /* This is meant to catch cases that are likely opportunities for
     if-conversion.  Essentially we look for the case where
     BB's predecessors are both single statement blocks where
     the output of that statement feed the same PHI in BB.  */
  if (num_stmts_in_pred1 == 1 && num_stmts_in_pred2 == 1)
    {
      gimple *stmt1 = last_and_only_stmt (pred1);
      gimple *stmt2 = last_and_only_stmt (pred2);

      if (stmt1 && stmt2
	  && gimple_code (stmt1) == GIMPLE_ASSIGN
	  && gimple_code (stmt2) == GIMPLE_ASSIGN)
	{
	  enum tree_code code1 = gimple_assign_rhs_code (stmt1);
	  enum tree_code code2 = gimple_assign_rhs_code (stmt2);

	  if (!poor_ifcvt_candidate_code (code1)
	      && !poor_ifcvt_candidate_code (code2))
	    {
	      tree lhs1 = gimple_assign_lhs (stmt1);
	      tree lhs2 = gimple_assign_lhs (stmt2);
	      gimple_stmt_iterator gsi;
	      for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
		{
		  gimple *phi = gsi_stmt (gsi);
		  if ((gimple_phi_arg_def (phi, 0) == lhs1
		       && gimple_phi_arg_def (phi, 1) == lhs2)
		      || (gimple_phi_arg_def (phi, 1) == lhs1
			  && gimple_phi_arg_def (phi, 0) == lhs2))
		    {
		      if (dump_file && (dump_flags & TDF_DETAILS))
			fprintf (dump_file,
				 "Block %d appears to be a join point for "
				 "if-convertable diamond.\n",
				 bb->index);
		      return false;
		    }
		}
	    }
	}
    }

  /* We may want something here which looks at dataflow and tries
     to guess if duplication of BB is likely to result in simplification
     of instructions in BB in either the original or the duplicate.  */

  /* Upper Hard limit on the number statements to copy.  */
  if (num_stmts_in_join
      >= PARAM_VALUE (PARAM_MAX_JUMP_THREAD_DUPLICATION_STMTS))
    return false;

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
  loop_p loop;

  loop_optimizer_init (LOOPS_NORMAL | LOOPS_HAVE_RECORDED_EXITS);
  initialize_original_copy_tables ();
  calculate_dominance_info (CDI_DOMINATORS);

  FOR_EACH_LOOP (loop, LI_FROM_INNERMOST)
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
   opt_pass * clone () { return new pass_split_paths (m_ctxt); }
   virtual bool gate (function *) { return gate_split_paths (); }
   virtual unsigned int execute (function *) { return execute_split_paths (); }

}; // class pass_split_paths

} // anon namespace

gimple_opt_pass *
make_pass_split_paths (gcc::context *ctxt)
{
  return new pass_split_paths (ctxt);
}
