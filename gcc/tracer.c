/* The tracer pass for the GNU compiler.
   Contributed by Jan Hubicka, SuSE Labs.
   Adapted to work on GIMPLE instead of RTL by Robert Kidd, UIUC.
   Copyright (C) 2001-2014 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* This pass performs the tail duplication needed for superblock formation.
   For more information see:

     Design and Analysis of Profile-Based Optimization in Compaq's
     Compilation Tools for Alpha; Journal of Instruction-Level
     Parallelism 3 (2000) 1-25

   Unlike Compaq's implementation we don't do the loop peeling as most
   probably a better job can be done by a special pass and we don't
   need to worry too much about the code size implications as the tail
   duplicates are crossjumped again if optimizations are not
   performed.  */


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "fibheap.h"
#include "flags.h"
#include "params.h"
#include "coverage.h"
#include "tree-pass.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "tree-ssa.h"
#include "tree-inline.h"
#include "cfgloop.h"

static int count_insns (basic_block);
static bool ignore_bb_p (const_basic_block);
static bool better_p (const_edge, const_edge);
static edge find_best_successor (basic_block);
static edge find_best_predecessor (basic_block);
static int find_trace (basic_block, basic_block *);

/* Minimal outgoing edge probability considered for superblock formation.  */
static int probability_cutoff;
static int branch_ratio_cutoff;

/* A bit BB->index is set if BB has already been seen, i.e. it is
   connected to some trace already.  */
sbitmap bb_seen;

static inline void
mark_bb_seen (basic_block bb)
{
  unsigned int size = SBITMAP_SIZE (bb_seen);

  if ((unsigned int)bb->index >= size)
    bb_seen = sbitmap_resize (bb_seen, size * 2, 0);

  bitmap_set_bit (bb_seen, bb->index);
}

static inline bool
bb_seen_p (basic_block bb)
{
  return bitmap_bit_p (bb_seen, bb->index);
}

/* Return true if we should ignore the basic block for purposes of tracing.  */
static bool
ignore_bb_p (const_basic_block bb)
{
  gimple g;

  if (bb->index < NUM_FIXED_BLOCKS)
    return true;
  if (optimize_bb_for_size_p (bb))
    return true;

  /* A transaction is a single entry multiple exit region.  It must be
     duplicated in its entirety or not at all.  */
  g = last_stmt (CONST_CAST_BB (bb));
  if (g && gimple_code (g) == GIMPLE_TRANSACTION)
    return true;

  return false;
}

/* Return number of instructions in the block.  */

static int
count_insns (basic_block bb)
{
  gimple_stmt_iterator gsi;
  gimple stmt;
  int n = 0;

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      stmt = gsi_stmt (gsi);
      n += estimate_num_insns (stmt, &eni_size_weights);
    }
  return n;
}

/* Return true if E1 is more frequent than E2.  */
static bool
better_p (const_edge e1, const_edge e2)
{
  if (e1->count != e2->count)
    return e1->count > e2->count;
  if (e1->src->frequency * e1->probability !=
      e2->src->frequency * e2->probability)
    return (e1->src->frequency * e1->probability
	    > e2->src->frequency * e2->probability);
  /* This is needed to avoid changes in the decision after
     CFG is modified.  */
  if (e1->src != e2->src)
    return e1->src->index > e2->src->index;
  return e1->dest->index > e2->dest->index;
}

/* Return most frequent successor of basic block BB.  */

static edge
find_best_successor (basic_block bb)
{
  edge e;
  edge best = NULL;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->succs)
    if (!best || better_p (e, best))
      best = e;
  if (!best || ignore_bb_p (best->dest))
    return NULL;
  if (best->probability <= probability_cutoff)
    return NULL;
  return best;
}

/* Return most frequent predecessor of basic block BB.  */

static edge
find_best_predecessor (basic_block bb)
{
  edge e;
  edge best = NULL;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->preds)
    if (!best || better_p (e, best))
      best = e;
  if (!best || ignore_bb_p (best->src))
    return NULL;
  if (EDGE_FREQUENCY (best) * REG_BR_PROB_BASE
      < bb->frequency * branch_ratio_cutoff)
    return NULL;
  return best;
}

/* Find the trace using bb and record it in the TRACE array.
   Return number of basic blocks recorded.  */

static int
find_trace (basic_block bb, basic_block *trace)
{
  int i = 0;
  edge e;

  if (dump_file)
    fprintf (dump_file, "Trace seed %i [%i]", bb->index, bb->frequency);

  while ((e = find_best_predecessor (bb)) != NULL)
    {
      basic_block bb2 = e->src;
      if (bb_seen_p (bb2) || (e->flags & (EDGE_DFS_BACK | EDGE_COMPLEX))
	  || find_best_successor (bb2) != e)
	break;
      if (dump_file)
	fprintf (dump_file, ",%i [%i]", bb->index, bb->frequency);
      bb = bb2;
    }
  if (dump_file)
    fprintf (dump_file, " forward %i [%i]", bb->index, bb->frequency);
  trace[i++] = bb;

  /* Follow the trace in forward direction.  */
  while ((e = find_best_successor (bb)) != NULL)
    {
      bb = e->dest;
      if (bb_seen_p (bb) || (e->flags & (EDGE_DFS_BACK | EDGE_COMPLEX))
	  || find_best_predecessor (bb) != e)
	break;
      if (dump_file)
	fprintf (dump_file, ",%i [%i]", bb->index, bb->frequency);
      trace[i++] = bb;
    }
  if (dump_file)
    fprintf (dump_file, "\n");
  return i;
}

/* Look for basic blocks in frequency order, construct traces and tail duplicate
   if profitable.  */

static bool
tail_duplicate (void)
{
  fibnode_t *blocks = XCNEWVEC (fibnode_t, last_basic_block_for_fn (cfun));
  basic_block *trace = XNEWVEC (basic_block, n_basic_blocks_for_fn (cfun));
  int *counts = XNEWVEC (int, last_basic_block_for_fn (cfun));
  int ninsns = 0, nduplicated = 0;
  gcov_type weighted_insns = 0, traced_insns = 0;
  fibheap_t heap = fibheap_new ();
  gcov_type cover_insns;
  int max_dup_insns;
  basic_block bb;
  bool changed = false;

  /* Create an oversized sbitmap to reduce the chance that we need to
     resize it.  */
  bb_seen = sbitmap_alloc (last_basic_block_for_fn (cfun) * 2);
  bitmap_clear (bb_seen);
  initialize_original_copy_tables ();

  if (profile_info && flag_branch_probabilities)
    probability_cutoff = PARAM_VALUE (TRACER_MIN_BRANCH_PROBABILITY_FEEDBACK);
  else
    probability_cutoff = PARAM_VALUE (TRACER_MIN_BRANCH_PROBABILITY);
  probability_cutoff = REG_BR_PROB_BASE / 100 * probability_cutoff;

  branch_ratio_cutoff =
    (REG_BR_PROB_BASE / 100 * PARAM_VALUE (TRACER_MIN_BRANCH_RATIO));

  FOR_EACH_BB_FN (bb, cfun)
    {
      int n = count_insns (bb);
      if (!ignore_bb_p (bb))
	blocks[bb->index] = fibheap_insert (heap, -bb->frequency,
					    bb);

      counts [bb->index] = n;
      ninsns += n;
      weighted_insns += n * bb->frequency;
    }

  if (profile_info && flag_branch_probabilities)
    cover_insns = PARAM_VALUE (TRACER_DYNAMIC_COVERAGE_FEEDBACK);
  else
    cover_insns = PARAM_VALUE (TRACER_DYNAMIC_COVERAGE);
  cover_insns = (weighted_insns * cover_insns + 50) / 100;
  max_dup_insns = (ninsns * PARAM_VALUE (TRACER_MAX_CODE_GROWTH) + 50) / 100;

  while (traced_insns < cover_insns && nduplicated < max_dup_insns
         && !fibheap_empty (heap))
    {
      basic_block bb = (basic_block) fibheap_extract_min (heap);
      int n, pos;

      if (!bb)
	break;

      blocks[bb->index] = NULL;

      if (ignore_bb_p (bb))
	continue;
      gcc_assert (!bb_seen_p (bb));

      n = find_trace (bb, trace);

      bb = trace[0];
      traced_insns += bb->frequency * counts [bb->index];
      if (blocks[bb->index])
	{
	  fibheap_delete_node (heap, blocks[bb->index]);
	  blocks[bb->index] = NULL;
	}

      for (pos = 1; pos < n; pos++)
	{
	  basic_block bb2 = trace[pos];

	  if (blocks[bb2->index])
	    {
	      fibheap_delete_node (heap, blocks[bb2->index]);
	      blocks[bb2->index] = NULL;
	    }
	  traced_insns += bb2->frequency * counts [bb2->index];
	  if (EDGE_COUNT (bb2->preds) > 1
	      && can_duplicate_block_p (bb2)
	      /* We have the tendency to duplicate the loop header
	         of all do { } while loops.  Do not do that - it is
		 not profitable and it might create a loop with multiple
		 entries or at least rotate the loop.  */
	      && (!current_loops
		  || bb2->loop_father->header != bb2))
	    {
	      edge e;
	      basic_block copy;

	      nduplicated += counts [bb2->index];

	      e = find_edge (bb, bb2);

	      copy = duplicate_block (bb2, e, bb);
	      flush_pending_stmts (e);

	      add_phi_args_after_copy (&copy, 1, NULL);

	      /* Reconsider the original copy of block we've duplicated.
	         Removing the most common predecessor may make it to be
	         head.  */
	      blocks[bb2->index] =
		fibheap_insert (heap, -bb2->frequency, bb2);

	      if (dump_file)
		fprintf (dump_file, "Duplicated %i as %i [%i]\n",
			 bb2->index, copy->index, copy->frequency);

	      bb2 = copy;
	      changed = true;
	    }
	  mark_bb_seen (bb2);
	  bb = bb2;
	  /* In case the trace became infrequent, stop duplicating.  */
	  if (ignore_bb_p (bb))
	    break;
	}
      if (dump_file)
	fprintf (dump_file, " covered now %.1f\n\n",
		 traced_insns * 100.0 / weighted_insns);
    }
  if (dump_file)
    fprintf (dump_file, "Duplicated %i insns (%i%%)\n", nduplicated,
	     nduplicated * 100 / ninsns);

  free_original_copy_tables ();
  sbitmap_free (bb_seen);
  free (blocks);
  free (trace);
  free (counts);
  fibheap_delete (heap);

  return changed;
}

namespace {

const pass_data pass_data_tracer =
{
  GIMPLE_PASS, /* type */
  "tracer", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_execute */
  TV_TRACER, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa, /* todo_flags_finish */
};

class pass_tracer : public gimple_opt_pass
{
public:
  pass_tracer (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_tracer, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      return (optimize > 0 && flag_tracer && flag_reorder_blocks);
    }

  virtual unsigned int execute (function *);

}; // class pass_tracer

unsigned int
pass_tracer::execute (function *fun)
{
  bool changed;

  if (n_basic_blocks_for_fn (fun) <= NUM_FIXED_BLOCKS + 1)
    return 0;

  mark_dfs_back_edges ();
  if (dump_file)
    brief_dump_cfg (dump_file, dump_flags);

  /* Trace formation is done on the fly inside tail_duplicate */
  changed = tail_duplicate ();
  if (changed)
    {
      free_dominance_info (CDI_DOMINATORS);
      /* If we changed the CFG schedule loops for fixup by cleanup_cfg.  */
      if (current_loops)
	loops_state_set (LOOPS_NEED_FIXUP);
    }

  if (dump_file)
    brief_dump_cfg (dump_file, dump_flags);

  return changed ? TODO_cleanup_cfg : 0;
}
} // anon namespace

gimple_opt_pass *
make_pass_tracer (gcc::context *ctxt)
{
  return new pass_tracer (ctxt);
}
