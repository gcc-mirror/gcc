/* The tracer pass for the GNU compiler.
   Contributed by Jan Hubicka, SuSE Labs.
   Copyright (C) 2001, 2002 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING.  If not, write to the Free
   Software Foundation, 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.  */

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
#include "tree.h"
#include "rtl.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "output.h"
#include "cfglayout.h"
#include "fibheap.h"
#include "flags.h"
#include "params.h"
#include "profile.h"

static int count_insns		PARAMS ((basic_block));
static bool ignore_bb_p		PARAMS ((basic_block));
static bool better_p		PARAMS ((edge, edge));
static edge find_best_successor PARAMS ((basic_block));
static edge find_best_predecessor PARAMS ((basic_block));
static int find_trace		PARAMS ((basic_block, basic_block *));
static void tail_duplicate	PARAMS ((void));
static void layout_superblocks	PARAMS ((void));
static bool ignore_bb_p		PARAMS ((basic_block));

/* Minimal outgoing edge probability considered for superblock formation.  */
static int probability_cutoff;
static int branch_ratio_cutoff;

/* Return true if BB has been seen - it is connected to some trace
   already.  */

#define seen(bb) (RBI (bb)->visited || RBI (bb)->next)

/* Return true if we should ignore the basic block for purposes of tracing.  */
static bool
ignore_bb_p (bb)
     basic_block bb;
{
  if (bb->index < 0)
    return true;
  if (!maybe_hot_bb_p (bb))
    return true;
  return false;
}

/* Return number of instructions in the block.  */

static int
count_insns (bb)
     basic_block bb;
{
  rtx insn;
  int n = 0;

  for (insn = bb->head; insn != NEXT_INSN (bb->end); insn = NEXT_INSN (insn))
    if (active_insn_p (insn))
      n++;
  return n;
}

/* Return true if E1 is more frequent than E2.  */
static bool
better_p (e1, e2)
     edge e1, e2;
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
find_best_successor (bb)
     basic_block bb;
{
  edge e;
  edge best = NULL;

  for (e = bb->succ; e; e = e->succ_next)
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
find_best_predecessor (bb)
     basic_block bb;
{
  edge e;
  edge best = NULL;

  for (e = bb->pred; e; e = e->pred_next)
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
find_trace (bb, trace)
     basic_block bb;
     basic_block *trace;
{
  int i = 0;
  edge e;

  if (rtl_dump_file)
    fprintf (rtl_dump_file, "Trace seed %i [%i]", bb->index, bb->frequency);

  while ((e = find_best_predecessor (bb)) != NULL)
    {
      basic_block bb2 = e->src;
      if (seen (bb2) || (e->flags & (EDGE_DFS_BACK | EDGE_COMPLEX))
	  || find_best_successor (bb2) != e)
	break;
      if (rtl_dump_file)
	fprintf (rtl_dump_file, ",%i [%i]", bb->index, bb->frequency);
      bb = bb2;
    }
  if (rtl_dump_file)
    fprintf (rtl_dump_file, " forward %i [%i]", bb->index, bb->frequency);
  trace[i++] = bb;

  /* Follow the trace in forward direction.  */
  while ((e = find_best_successor (bb)) != NULL)
    {
      bb = e->dest;
      if (seen (bb) || (e->flags & (EDGE_DFS_BACK | EDGE_COMPLEX))
	  || find_best_predecessor (bb) != e)
	break;
      if (rtl_dump_file)
	fprintf (rtl_dump_file, ",%i [%i]", bb->index, bb->frequency);
      trace[i++] = bb;
    }
  if (rtl_dump_file)
    fprintf (rtl_dump_file, "\n");
  return i;
}

/* Look for basic blocks in frequency order, construct traces and tail duplicate
   if profitable.  */

static void
tail_duplicate ()
{
  fibnode_t *blocks = xcalloc (last_basic_block, sizeof (fibnode_t));
  basic_block *trace = xmalloc (sizeof (basic_block) * n_basic_blocks);
  int *counts = xmalloc (sizeof (int) * last_basic_block);
  int ninsns = 0, nduplicated = 0;
  gcov_type weighted_insns = 0, traced_insns = 0;
  fibheap_t heap = fibheap_new ();
  gcov_type cover_insns;
  int max_dup_insns;
  basic_block bb;

  if (profile_info.count_profiles_merged && flag_branch_probabilities)
    probability_cutoff = PARAM_VALUE (TRACER_MIN_BRANCH_PROBABILITY_FEEDBACK);
  else
    probability_cutoff = PARAM_VALUE (TRACER_MIN_BRANCH_PROBABILITY);
  probability_cutoff = REG_BR_PROB_BASE / 100 * probability_cutoff;

  branch_ratio_cutoff =
    (REG_BR_PROB_BASE / 100 * PARAM_VALUE (TRACER_MIN_BRANCH_RATIO));

  FOR_EACH_BB (bb)
    {
      int n = count_insns (bb);
      if (!ignore_bb_p (bb))
	blocks[bb->index] = fibheap_insert (heap, -bb->frequency,
					    bb);

      counts [bb->index] = n;
      ninsns += n;
      weighted_insns += n * bb->frequency;
    }

  if (profile_info.count_profiles_merged && flag_branch_probabilities)
    cover_insns = PARAM_VALUE (TRACER_DYNAMIC_COVERAGE_FEEDBACK);
  else
    cover_insns = PARAM_VALUE (TRACER_DYNAMIC_COVERAGE);
  cover_insns = (weighted_insns * cover_insns + 50) / 100;
  max_dup_insns = (ninsns * PARAM_VALUE (TRACER_MAX_CODE_GROWTH) + 50) / 100;

  while (traced_insns < cover_insns && nduplicated < max_dup_insns
         && !fibheap_empty (heap))
    {
      basic_block bb = fibheap_extract_min (heap);
      int n, pos;

      if (!bb)
	break;

      blocks[bb->index] = NULL;

      if (ignore_bb_p (bb))
	continue;
      if (seen (bb))
	abort ();

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
	  if (bb2->pred && bb2->pred->pred_next
	      && cfg_layout_can_duplicate_bb_p (bb2))
	    {
	      edge e = bb2->pred;
	      basic_block old = bb2;

	      while (e->src != bb)
		e = e->pred_next;
	      nduplicated += counts [bb2->index];
	      bb2 = cfg_layout_duplicate_bb (bb2, e);

	      /* Reconsider the original copy of block we've duplicated.
	         Removing the most common predecesor may make it to be
	         head.  */
	      blocks[old->index] =
		fibheap_insert (heap, -old->frequency, old);

	      if (rtl_dump_file)
		fprintf (rtl_dump_file, "Duplicated %i as %i [%i]\n",
			 old->index, bb2->index, bb2->frequency);
	    }
	  RBI (bb)->next = bb2;
	  RBI (bb2)->visited = 1;
	  bb = bb2;
	  /* In case the trace became infrequent, stop duplicating.  */
	  if (ignore_bb_p (bb))
	    break;
	}
      if (rtl_dump_file)
	fprintf (rtl_dump_file, " covered now %.1f\n\n",
		 traced_insns * 100.0 / weighted_insns);
    }
  if (rtl_dump_file)
    fprintf (rtl_dump_file, "Duplicated %i insns (%i%%)\n", nduplicated,
	     nduplicated * 100 / ninsns);

  free (blocks);
  free (trace);
  free (counts);
  fibheap_delete (heap);
}

/* Connect the superblocks into linear seuqence.  At the moment we attempt to keep
   the original order as much as possible, but the algorithm may be made smarter
   later if needed.  BB reordering pass should void most of the benefits of such
   change though.  */

static void
layout_superblocks ()
{
  basic_block end = ENTRY_BLOCK_PTR->succ->dest;
  basic_block bb = ENTRY_BLOCK_PTR->succ->dest->next_bb;

  while (bb != EXIT_BLOCK_PTR)
    {
      edge e, best = NULL;
      while (RBI (end)->next)
	end = RBI (end)->next;

      for (e = end->succ; e; e = e->succ_next)
	if (e->dest != EXIT_BLOCK_PTR
	    && e->dest != ENTRY_BLOCK_PTR->succ->dest
	    && !RBI (e->dest)->visited
	    && (!best || EDGE_FREQUENCY (e) > EDGE_FREQUENCY (best)))
	  best = e;

      if (best)
	{
	  RBI (end)->next = best->dest;
	  RBI (best->dest)->visited = 1;
	}
      else
	for (; bb != EXIT_BLOCK_PTR; bb=bb->next_bb)
	  {
	    if (!RBI (bb)->visited)
	      {
		RBI (end)->next = bb;
		RBI (bb)->visited = 1;
		break;
	      }
	  }
    }
}

/* Main entry point to this file.  */

void
tracer ()
{
  if (n_basic_blocks <= 1)
    return;
  cfg_layout_initialize ();
  mark_dfs_back_edges ();
  if (rtl_dump_file)
    dump_flow_info (rtl_dump_file);
  tail_duplicate ();
  layout_superblocks ();
  if (rtl_dump_file)
    dump_flow_info (rtl_dump_file);
  cfg_layout_finalize ();
  /* Merge basic blocks in duplicated traces.  */
  cleanup_cfg (CLEANUP_EXPENSIVE);
}
