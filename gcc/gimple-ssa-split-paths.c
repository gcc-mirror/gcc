/* Support routines for Splitting Paths to loop backedges
   Copyright (C) 2015 Free Software Foundation, Inc.
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
#include "cfganal.h"
#include "cfgloop.h"
#include "gimple-iterator.h"
#include "tracer.h"
#include "predict.h"

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
	     the predecessors of BB.  */
	  if (!find_edge (bb_idom, EDGE_PRED (bb, 0)->src)
	      || !find_edge (bb_idom, EDGE_PRED (bb, 1)->src))
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

/* Return TRUE if BB is a reasonable block to duplicate by examining
   its size, false otherwise.  BB will always be a loop latch block.

   Should this use the same tests as we do for jump threading?  */

static bool
is_feasible_trace (basic_block bb)
{
  int num_stmt = 0;
  gimple_stmt_iterator gsi;

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      if (!is_gimple_debug (stmt))
	num_stmt++;
    }

  /* We may want to limit how many statements we copy.  */
  if (num_stmt > 1)
    return true;

  return false;
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
