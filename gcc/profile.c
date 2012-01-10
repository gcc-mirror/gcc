/* Calculate branch probabilities, and basic block execution counts.
   Copyright (C) 1990, 1991, 1992, 1993, 1994, 1996, 1997, 1998, 1999,
   2000, 2001, 2002, 2003, 2004, 2005, 2007, 2008, 2009, 2010, 2011, 2012
   Free Software Foundation, Inc.
   Contributed by James E. Wilson, UC Berkeley/Cygnus Support;
   based on some ideas from Dain Samples of UC Berkeley.
   Further mangling by Bob Manson, Cygnus Support.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* Generate basic block profile instrumentation and auxiliary files.
   Profile generation is optimized, so that not all arcs in the basic
   block graph need instrumenting. First, the BB graph is closed with
   one entry (function start), and one exit (function exit).  Any
   ABNORMAL_EDGE cannot be instrumented (because there is no control
   path to place the code). We close the graph by inserting fake
   EDGE_FAKE edges to the EXIT_BLOCK, from the sources of abnormal
   edges that do not go to the exit_block. We ignore such abnormal
   edges.  Naturally these fake edges are never directly traversed,
   and so *cannot* be directly instrumented.  Some other graph
   massaging is done. To optimize the instrumentation we generate the
   BB minimal span tree, only edges that are not on the span tree
   (plus the entry point) need instrumenting. From that information
   all other edge counts can be deduced.  By construction all fake
   edges must be on the spanning tree. We also attempt to place
   EDGE_CRITICAL edges on the spanning tree.

   The auxiliary files generated are <dumpbase>.gcno (at compile time)
   and <dumpbase>.gcda (at run time).  The format is
   described in full in gcov-io.h.  */

/* ??? Register allocation should use basic block execution counts to
   give preference to the most commonly executed blocks.  */

/* ??? Should calculate branch probabilities before instrumenting code, since
   then we can use arc counts to help decide which arcs to instrument.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "flags.h"
#include "output.h"
#include "regs.h"
#include "expr.h"
#include "function.h"
#include "basic-block.h"
#include "diagnostic-core.h"
#include "coverage.h"
#include "value-prof.h"
#include "tree.h"
#include "cfghooks.h"
#include "tree-flow.h"
#include "timevar.h"
#include "cfgloop.h"
#include "tree-pass.h"

#include "profile.h"

struct bb_info {
  unsigned int count_valid : 1;

  /* Number of successor and predecessor edges.  */
  gcov_type succ_count;
  gcov_type pred_count;
};

#define BB_INFO(b)  ((struct bb_info *) (b)->aux)


/* Counter summary from the last set of coverage counts read.  */

const struct gcov_ctr_summary *profile_info;

/* Collect statistics on the performance of this pass for the entire source
   file.  */

static int total_num_blocks;
static int total_num_edges;
static int total_num_edges_ignored;
static int total_num_edges_instrumented;
static int total_num_blocks_created;
static int total_num_passes;
static int total_num_times_called;
static int total_hist_br_prob[20];
static int total_num_branches;

/* Forward declarations.  */
static void find_spanning_tree (struct edge_list *);

/* Add edge instrumentation code to the entire insn chain.

   F is the first insn of the chain.
   NUM_BLOCKS is the number of basic blocks found in F.  */

static unsigned
instrument_edges (struct edge_list *el)
{
  unsigned num_instr_edges = 0;
  int num_edges = NUM_EDGES (el);
  basic_block bb;

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, NULL, next_bb)
    {
      edge e;
      edge_iterator ei;

      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  struct edge_info *inf = EDGE_INFO (e);

	  if (!inf->ignore && !inf->on_tree)
	    {
	      gcc_assert (!(e->flags & EDGE_ABNORMAL));
	      if (dump_file)
		fprintf (dump_file, "Edge %d to %d instrumented%s\n",
			 e->src->index, e->dest->index,
			 EDGE_CRITICAL_P (e) ? " (and split)" : "");
	      gimple_gen_edge_profiler (num_instr_edges++, e);
	    }
	}
    }

  total_num_blocks_created += num_edges;
  if (dump_file)
    fprintf (dump_file, "%d edges instrumented\n", num_instr_edges);
  return num_instr_edges;
}

/* Add code to measure histograms for values in list VALUES.  */
static void
instrument_values (histogram_values values)
{
  unsigned i, t;

  /* Emit code to generate the histograms before the insns.  */

  for (i = 0; i < VEC_length (histogram_value, values); i++)
    {
      histogram_value hist = VEC_index (histogram_value, values, i);
      switch (hist->type)
	{
	case HIST_TYPE_INTERVAL:
	  t = GCOV_COUNTER_V_INTERVAL;
	  break;

	case HIST_TYPE_POW2:
	  t = GCOV_COUNTER_V_POW2;
	  break;

	case HIST_TYPE_SINGLE_VALUE:
	  t = GCOV_COUNTER_V_SINGLE;
	  break;

	case HIST_TYPE_CONST_DELTA:
	  t = GCOV_COUNTER_V_DELTA;
	  break;

 	case HIST_TYPE_INDIR_CALL:
 	  t = GCOV_COUNTER_V_INDIR;
 	  break;

 	case HIST_TYPE_AVERAGE:
 	  t = GCOV_COUNTER_AVERAGE;
 	  break;

 	case HIST_TYPE_IOR:
 	  t = GCOV_COUNTER_IOR;
 	  break;

	default:
	  gcc_unreachable ();
	}
      if (!coverage_counter_alloc (t, hist->n_counters))
	continue;

      switch (hist->type)
	{
	case HIST_TYPE_INTERVAL:
	  gimple_gen_interval_profiler (hist, t, 0);
	  break;

	case HIST_TYPE_POW2:
	  gimple_gen_pow2_profiler (hist, t, 0);
	  break;

	case HIST_TYPE_SINGLE_VALUE:
	  gimple_gen_one_value_profiler (hist, t, 0);
	  break;

	case HIST_TYPE_CONST_DELTA:
	  gimple_gen_const_delta_profiler (hist, t, 0);
	  break;

 	case HIST_TYPE_INDIR_CALL:
 	  gimple_gen_ic_profiler (hist, t, 0);
  	  break;

	case HIST_TYPE_AVERAGE:
	  gimple_gen_average_profiler (hist, t, 0);
	  break;

	case HIST_TYPE_IOR:
	  gimple_gen_ior_profiler (hist, t, 0);
	  break;

	default:
	  gcc_unreachable ();
	}
    }
}


/* Computes hybrid profile for all matching entries in da_file.  
   
   CFG_CHECKSUM is the precomputed checksum for the CFG.  */

static gcov_type *
get_exec_counts (unsigned cfg_checksum, unsigned lineno_checksum)
{
  unsigned num_edges = 0;
  basic_block bb;
  gcov_type *counts;

  /* Count the edges to be (possibly) instrumented.  */
  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, NULL, next_bb)
    {
      edge e;
      edge_iterator ei;

      FOR_EACH_EDGE (e, ei, bb->succs)
	if (!EDGE_INFO (e)->ignore && !EDGE_INFO (e)->on_tree)
	  num_edges++;
    }

  counts = get_coverage_counts (GCOV_COUNTER_ARCS, num_edges, cfg_checksum,
				lineno_checksum, &profile_info);
  if (!counts)
    return NULL;

  if (dump_file && profile_info)
    fprintf(dump_file, "Merged %u profiles with maximal count %u.\n",
	    profile_info->runs, (unsigned) profile_info->sum_max);

  return counts;
}


static bool
is_edge_inconsistent (VEC(edge,gc) *edges)
{
  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, edges)
    {
      if (!EDGE_INFO (e)->ignore)
        {
          if (e->count < 0
	      && (!(e->flags & EDGE_FAKE)
	          || !block_ends_with_call_p (e->src)))
	    {
	      if (dump_file)
		{
		  fprintf (dump_file,
		  	   "Edge %i->%i is inconsistent, count"HOST_WIDEST_INT_PRINT_DEC,
			   e->src->index, e->dest->index, e->count);
		  dump_bb (e->src, dump_file, 0);
		  dump_bb (e->dest, dump_file, 0);
		}
              return true;
	    }
        }
    }
  return false;
}

static void
correct_negative_edge_counts (void)
{
  basic_block bb;
  edge e;
  edge_iterator ei;

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, NULL, next_bb)
    {
      FOR_EACH_EDGE (e, ei, bb->succs)
        {
           if (e->count < 0)
             e->count = 0;
        }
    }
}

/* Check consistency.
   Return true if inconsistency is found.  */
static bool
is_inconsistent (void)
{
  basic_block bb;
  bool inconsistent = false;
  FOR_EACH_BB (bb)
    {
      inconsistent |= is_edge_inconsistent (bb->preds);
      if (!dump_file && inconsistent)
	return true;
      inconsistent |= is_edge_inconsistent (bb->succs);
      if (!dump_file && inconsistent)
	return true;
      if (bb->count < 0)
        {
	  if (dump_file)
	    {
	      fprintf (dump_file, "BB %i count is negative "
		       HOST_WIDEST_INT_PRINT_DEC,
		       bb->index,
		       bb->count);
	      dump_bb (bb, dump_file, 0);
	    }
	  inconsistent = true;
	}
      if (bb->count != sum_edge_counts (bb->preds))
        {
	  if (dump_file)
	    {
	      fprintf (dump_file, "BB %i count does not match sum of incoming edges "
		       HOST_WIDEST_INT_PRINT_DEC" should be " HOST_WIDEST_INT_PRINT_DEC,
		       bb->index,
		       bb->count,
		       sum_edge_counts (bb->preds));
	      dump_bb (bb, dump_file, 0);
	    }
	  inconsistent = true;
	}
      if (bb->count != sum_edge_counts (bb->succs) &&
          ! (find_edge (bb, EXIT_BLOCK_PTR) != NULL && block_ends_with_call_p (bb)))
	{
	  if (dump_file)
	    {
	      fprintf (dump_file, "BB %i count does not match sum of outgoing edges "
		       HOST_WIDEST_INT_PRINT_DEC" should be " HOST_WIDEST_INT_PRINT_DEC,
		       bb->index,
		       bb->count,
		       sum_edge_counts (bb->succs));
	      dump_bb (bb, dump_file, 0);
	    }
	  inconsistent = true;
	}
      if (!dump_file && inconsistent)
	return true;
    }

  return inconsistent;
}

/* Set each basic block count to the sum of its outgoing edge counts */
static void
set_bb_counts (void)
{
  basic_block bb;
  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, NULL, next_bb)
    {
      bb->count = sum_edge_counts (bb->succs);
      gcc_assert (bb->count >= 0);
    }
}

/* Reads profile data and returns total number of edge counts read */
static int
read_profile_edge_counts (gcov_type *exec_counts)
{
  basic_block bb;
  int num_edges = 0;
  int exec_counts_pos = 0;
  /* For each edge not on the spanning tree, set its execution count from
     the .da file.  */
  /* The first count in the .da file is the number of times that the function
     was entered.  This is the exec_count for block zero.  */

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, NULL, next_bb)
    {
      edge e;
      edge_iterator ei;

      FOR_EACH_EDGE (e, ei, bb->succs)
	if (!EDGE_INFO (e)->ignore && !EDGE_INFO (e)->on_tree)
	  {
	    num_edges++;
	    if (exec_counts)
	      {
		e->count = exec_counts[exec_counts_pos++];
		if (e->count > profile_info->sum_max)
		  {
		    if (flag_profile_correction)
		      {
			static bool informed = 0;
			if (!informed)
		          inform (input_location,
			          "corrupted profile info: edge count exceeds maximal count");
			informed = 1;
		      }
		    else
		      error ("corrupted profile info: edge from %i to %i exceeds maximal count",
			     bb->index, e->dest->index);
		  }
	      }
	    else
	      e->count = 0;

	    EDGE_INFO (e)->count_valid = 1;
	    BB_INFO (bb)->succ_count--;
	    BB_INFO (e->dest)->pred_count--;
	    if (dump_file)
	      {
		fprintf (dump_file, "\nRead edge from %i to %i, count:",
			 bb->index, e->dest->index);
		fprintf (dump_file, HOST_WIDEST_INT_PRINT_DEC,
			 (HOST_WIDEST_INT) e->count);
	      }
	  }
    }

    return num_edges;
}

#define OVERLAP_BASE 10000

/* Compare the static estimated profile to the actual profile, and
   return the "degree of overlap" measure between them.

   Degree of overlap is a number between 0 and OVERLAP_BASE. It is
   the sum of each basic block's minimum relative weights between
   two profiles. And overlap of OVERLAP_BASE means two profiles are
   identical.  */

static int
compute_frequency_overlap (void)
{
  gcov_type count_total = 0, freq_total = 0;
  int overlap = 0;
  basic_block bb;

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, NULL, next_bb)
    {
      count_total += bb->count;
      freq_total += bb->frequency;
    }

  if (count_total == 0 || freq_total == 0)
    return 0;

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, NULL, next_bb)
    overlap += MIN (bb->count * OVERLAP_BASE / count_total,
		    bb->frequency * OVERLAP_BASE / freq_total);

  return overlap;
}

/* Compute the branch probabilities for the various branches.
   Annotate them accordingly.  

   CFG_CHECKSUM is the precomputed checksum for the CFG.  */

static void
compute_branch_probabilities (unsigned cfg_checksum, unsigned lineno_checksum)
{
  basic_block bb;
  int i;
  int num_edges = 0;
  int changes;
  int passes;
  int hist_br_prob[20];
  int num_branches;
  gcov_type *exec_counts = get_exec_counts (cfg_checksum, lineno_checksum);
  int inconsistent = 0;

  /* Very simple sanity checks so we catch bugs in our profiling code.  */
  if (!profile_info)
    return;
  if (profile_info->run_max * profile_info->runs < profile_info->sum_max)
    {
      error ("corrupted profile info: run_max * runs < sum_max");
      exec_counts = NULL;
    }

  if (profile_info->sum_all < profile_info->sum_max)
    {
      error ("corrupted profile info: sum_all is smaller than sum_max");
      exec_counts = NULL;
    }

  /* Attach extra info block to each bb.  */
  alloc_aux_for_blocks (sizeof (struct bb_info));
  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, NULL, next_bb)
    {
      edge e;
      edge_iterator ei;

      FOR_EACH_EDGE (e, ei, bb->succs)
	if (!EDGE_INFO (e)->ignore)
	  BB_INFO (bb)->succ_count++;
      FOR_EACH_EDGE (e, ei, bb->preds)
	if (!EDGE_INFO (e)->ignore)
	  BB_INFO (bb)->pred_count++;
    }

  /* Avoid predicting entry on exit nodes.  */
  BB_INFO (EXIT_BLOCK_PTR)->succ_count = 2;
  BB_INFO (ENTRY_BLOCK_PTR)->pred_count = 2;

  num_edges = read_profile_edge_counts (exec_counts);

  if (dump_file)
    fprintf (dump_file, "\n%d edge counts read\n", num_edges);

  /* For every block in the file,
     - if every exit/entrance edge has a known count, then set the block count
     - if the block count is known, and every exit/entrance edge but one has
     a known execution count, then set the count of the remaining edge

     As edge counts are set, decrement the succ/pred count, but don't delete
     the edge, that way we can easily tell when all edges are known, or only
     one edge is unknown.  */

  /* The order that the basic blocks are iterated through is important.
     Since the code that finds spanning trees starts with block 0, low numbered
     edges are put on the spanning tree in preference to high numbered edges.
     Hence, most instrumented edges are at the end.  Graph solving works much
     faster if we propagate numbers from the end to the start.

     This takes an average of slightly more than 3 passes.  */

  changes = 1;
  passes = 0;
  while (changes)
    {
      passes++;
      changes = 0;
      FOR_BB_BETWEEN (bb, EXIT_BLOCK_PTR, NULL, prev_bb)
	{
	  struct bb_info *bi = BB_INFO (bb);
	  if (! bi->count_valid)
	    {
	      if (bi->succ_count == 0)
		{
		  edge e;
		  edge_iterator ei;
		  gcov_type total = 0;

		  FOR_EACH_EDGE (e, ei, bb->succs)
		    total += e->count;
		  bb->count = total;
		  bi->count_valid = 1;
		  changes = 1;
		}
	      else if (bi->pred_count == 0)
		{
		  edge e;
		  edge_iterator ei;
		  gcov_type total = 0;

		  FOR_EACH_EDGE (e, ei, bb->preds)
		    total += e->count;
		  bb->count = total;
		  bi->count_valid = 1;
		  changes = 1;
		}
	    }
	  if (bi->count_valid)
	    {
	      if (bi->succ_count == 1)
		{
		  edge e;
		  edge_iterator ei;
		  gcov_type total = 0;

		  /* One of the counts will be invalid, but it is zero,
		     so adding it in also doesn't hurt.  */
		  FOR_EACH_EDGE (e, ei, bb->succs)
		    total += e->count;

		  /* Search for the invalid edge, and set its count.  */
		  FOR_EACH_EDGE (e, ei, bb->succs)
		    if (! EDGE_INFO (e)->count_valid && ! EDGE_INFO (e)->ignore)
		      break;

		  /* Calculate count for remaining edge by conservation.  */
		  total = bb->count - total;

		  gcc_assert (e);
		  EDGE_INFO (e)->count_valid = 1;
		  e->count = total;
		  bi->succ_count--;

		  BB_INFO (e->dest)->pred_count--;
		  changes = 1;
		}
	      if (bi->pred_count == 1)
		{
		  edge e;
		  edge_iterator ei;
		  gcov_type total = 0;

		  /* One of the counts will be invalid, but it is zero,
		     so adding it in also doesn't hurt.  */
		  FOR_EACH_EDGE (e, ei, bb->preds)
		    total += e->count;

		  /* Search for the invalid edge, and set its count.  */
		  FOR_EACH_EDGE (e, ei, bb->preds)
		    if (!EDGE_INFO (e)->count_valid && !EDGE_INFO (e)->ignore)
		      break;

		  /* Calculate count for remaining edge by conservation.  */
		  total = bb->count - total + e->count;

		  gcc_assert (e);
		  EDGE_INFO (e)->count_valid = 1;
		  e->count = total;
		  bi->pred_count--;

		  BB_INFO (e->src)->succ_count--;
		  changes = 1;
		}
	    }
	}
    }
  if (dump_file)
    {
      int overlap = compute_frequency_overlap ();
      dump_flow_info (dump_file, dump_flags);
      fprintf (dump_file, "Static profile overlap: %d.%d%%\n",
	       overlap / (OVERLAP_BASE / 100),
	       overlap % (OVERLAP_BASE / 100));
    }

  total_num_passes += passes;
  if (dump_file)
    fprintf (dump_file, "Graph solving took %d passes.\n\n", passes);

  /* If the graph has been correctly solved, every block will have a
     succ and pred count of zero.  */
  FOR_EACH_BB (bb)
    {
      gcc_assert (!BB_INFO (bb)->succ_count && !BB_INFO (bb)->pred_count);
    }

  /* Check for inconsistent basic block counts */
  inconsistent = is_inconsistent ();

  if (inconsistent)
   {
     if (flag_profile_correction)
       {
         /* Inconsistency detected. Make it flow-consistent. */
         static int informed = 0;
         if (informed == 0)
           {
             informed = 1;
             inform (input_location, "correcting inconsistent profile data");
           }
         correct_negative_edge_counts ();
         /* Set bb counts to the sum of the outgoing edge counts */
         set_bb_counts ();
         if (dump_file)
           fprintf (dump_file, "\nCalling mcf_smooth_cfg\n");
         mcf_smooth_cfg ();
       }
     else
       error ("corrupted profile info: profile data is not flow-consistent");
   }

  /* For every edge, calculate its branch probability and add a reg_note
     to the branch insn to indicate this.  */

  for (i = 0; i < 20; i++)
    hist_br_prob[i] = 0;
  num_branches = 0;

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, NULL, next_bb)
    {
      edge e;
      edge_iterator ei;

      if (bb->count < 0)
	{
	  error ("corrupted profile info: number of iterations for basic block %d thought to be %i",
		 bb->index, (int)bb->count);
	  bb->count = 0;
	}
      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  /* Function may return twice in the cased the called function is
	     setjmp or calls fork, but we can't represent this by extra
	     edge from the entry, since extra edge from the exit is
	     already present.  We get negative frequency from the entry
	     point.  */
	  if ((e->count < 0
	       && e->dest == EXIT_BLOCK_PTR)
	      || (e->count > bb->count
		  && e->dest != EXIT_BLOCK_PTR))
	    {
	      if (block_ends_with_call_p (bb))
		e->count = e->count < 0 ? 0 : bb->count;
	    }
	  if (e->count < 0 || e->count > bb->count)
	    {
	      error ("corrupted profile info: number of executions for edge %d-%d thought to be %i",
		     e->src->index, e->dest->index,
		     (int)e->count);
	      e->count = bb->count / 2;
	    }
	}
      if (bb->count)
	{
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    e->probability = (e->count * REG_BR_PROB_BASE + bb->count / 2) / bb->count;
	  if (bb->index >= NUM_FIXED_BLOCKS
	      && block_ends_with_condjump_p (bb)
	      && EDGE_COUNT (bb->succs) >= 2)
	    {
	      int prob;
	      edge e;
	      int index;

	      /* Find the branch edge.  It is possible that we do have fake
		 edges here.  */
	      FOR_EACH_EDGE (e, ei, bb->succs)
		if (!(e->flags & (EDGE_FAKE | EDGE_FALLTHRU)))
		  break;

	      prob = e->probability;
	      index = prob * 20 / REG_BR_PROB_BASE;

	      if (index == 20)
		index = 19;
	      hist_br_prob[index]++;

	      num_branches++;
	    }
	}
      /* As a last resort, distribute the probabilities evenly.
	 Use simple heuristics that if there are normal edges,
	 give all abnormals frequency of 0, otherwise distribute the
	 frequency over abnormals (this is the case of noreturn
	 calls).  */
      else if (profile_status == PROFILE_ABSENT)
	{
	  int total = 0;

	  FOR_EACH_EDGE (e, ei, bb->succs)
	    if (!(e->flags & (EDGE_COMPLEX | EDGE_FAKE)))
	      total ++;
	  if (total)
	    {
	      FOR_EACH_EDGE (e, ei, bb->succs)
		if (!(e->flags & (EDGE_COMPLEX | EDGE_FAKE)))
		  e->probability = REG_BR_PROB_BASE / total;
		else
		  e->probability = 0;
	    }
	  else
	    {
	      total += EDGE_COUNT (bb->succs);
	      FOR_EACH_EDGE (e, ei, bb->succs)
		e->probability = REG_BR_PROB_BASE / total;
	    }
	  if (bb->index >= NUM_FIXED_BLOCKS
	      && block_ends_with_condjump_p (bb)
	      && EDGE_COUNT (bb->succs) >= 2)
	    num_branches++;
	}
    }
  counts_to_freqs ();
  profile_status = PROFILE_READ;
  compute_function_frequency ();

  if (dump_file)
    {
      fprintf (dump_file, "%d branches\n", num_branches);
      if (num_branches)
	for (i = 0; i < 10; i++)
	  fprintf (dump_file, "%d%% branches in range %d-%d%%\n",
		   (hist_br_prob[i] + hist_br_prob[19-i]) * 100 / num_branches,
		   5 * i, 5 * i + 5);

      total_num_branches += num_branches;
      for (i = 0; i < 20; i++)
	total_hist_br_prob[i] += hist_br_prob[i];

      fputc ('\n', dump_file);
      fputc ('\n', dump_file);
    }

  free_aux_for_blocks ();
}

/* Load value histograms values whose description is stored in VALUES array
   from .gcda file.  

   CFG_CHECKSUM is the precomputed checksum for the CFG.  */

static void
compute_value_histograms (histogram_values values, unsigned cfg_checksum,
                          unsigned lineno_checksum)
{
  unsigned i, j, t, any;
  unsigned n_histogram_counters[GCOV_N_VALUE_COUNTERS];
  gcov_type *histogram_counts[GCOV_N_VALUE_COUNTERS];
  gcov_type *act_count[GCOV_N_VALUE_COUNTERS];
  gcov_type *aact_count;

  for (t = 0; t < GCOV_N_VALUE_COUNTERS; t++)
    n_histogram_counters[t] = 0;

  for (i = 0; i < VEC_length (histogram_value, values); i++)
    {
      histogram_value hist = VEC_index (histogram_value, values, i);
      n_histogram_counters[(int) hist->type] += hist->n_counters;
    }

  any = 0;
  for (t = 0; t < GCOV_N_VALUE_COUNTERS; t++)
    {
      if (!n_histogram_counters[t])
	{
	  histogram_counts[t] = NULL;
	  continue;
	}

      histogram_counts[t] =
	get_coverage_counts (COUNTER_FOR_HIST_TYPE (t),
			     n_histogram_counters[t], cfg_checksum,
			     lineno_checksum, NULL);
      if (histogram_counts[t])
	any = 1;
      act_count[t] = histogram_counts[t];
    }
  if (!any)
    return;

  for (i = 0; i < VEC_length (histogram_value, values); i++)
    {
      histogram_value hist = VEC_index (histogram_value, values, i);
      gimple stmt = hist->hvalue.stmt;

      t = (int) hist->type;

      aact_count = act_count[t];
      act_count[t] += hist->n_counters;

      gimple_add_histogram_value (cfun, stmt, hist);
      hist->hvalue.counters =  XNEWVEC (gcov_type, hist->n_counters);
      for (j = 0; j < hist->n_counters; j++)
	hist->hvalue.counters[j] = aact_count[j];
    }

  for (t = 0; t < GCOV_N_VALUE_COUNTERS; t++)
    free (histogram_counts[t]);
}

/* The entry basic block will be moved around so that it has index=1,
   there is nothing at index 0 and the exit is at n_basic_block.  */
#define BB_TO_GCOV_INDEX(bb)  ((bb)->index - 1)
/* When passed NULL as file_name, initialize.
   When passed something else, output the necessary commands to change
   line to LINE and offset to FILE_NAME.  */
static void
output_location (char const *file_name, int line,
		 gcov_position_t *offset, basic_block bb)
{
  static char const *prev_file_name;
  static int prev_line;
  bool name_differs, line_differs;

  if (!file_name)
    {
      prev_file_name = NULL;
      prev_line = -1;
      return;
    }

  name_differs = !prev_file_name || filename_cmp (file_name, prev_file_name);
  line_differs = prev_line != line;

  if (name_differs || line_differs)
    {
      if (!*offset)
	{
	  *offset = gcov_write_tag (GCOV_TAG_LINES);
	  gcov_write_unsigned (BB_TO_GCOV_INDEX (bb));
	  name_differs = line_differs=true;
	}

      /* If this is a new source file, then output the
	 file's name to the .bb file.  */
      if (name_differs)
	{
	  prev_file_name = file_name;
	  gcov_write_unsigned (0);
	  gcov_write_string (prev_file_name);
	}
      if (line_differs)
	{
	  gcov_write_unsigned (line);
	  prev_line = line;
	}
     }
}

/* Instrument and/or analyze program behavior based on program flow graph.
   In either case, this function builds a flow graph for the function being
   compiled.  The flow graph is stored in BB_GRAPH.

   When FLAG_PROFILE_ARCS is nonzero, this function instruments the edges in
   the flow graph that are needed to reconstruct the dynamic behavior of the
   flow graph.

   When FLAG_BRANCH_PROBABILITIES is nonzero, this function reads auxiliary
   information from a data file containing edge count information from previous
   executions of the function being compiled.  In this case, the flow graph is
   annotated with actual execution counts, which are later propagated into the
   rtl for optimization purposes.

   Main entry point of this file.  */

void
branch_prob (void)
{
  basic_block bb;
  unsigned i;
  unsigned num_edges, ignored_edges;
  unsigned num_instrumented;
  struct edge_list *el;
  histogram_values values = NULL;
  unsigned cfg_checksum, lineno_checksum;

  total_num_times_called++;

  flow_call_edges_add (NULL);
  add_noreturn_fake_exit_edges ();

  /* We can't handle cyclic regions constructed using abnormal edges.
     To avoid these we replace every source of abnormal edge by a fake
     edge from entry node and every destination by fake edge to exit.
     This keeps graph acyclic and our calculation exact for all normal
     edges except for exit and entrance ones.

     We also add fake exit edges for each call and asm statement in the
     basic, since it may not return.  */

  FOR_EACH_BB (bb)
    {
      int need_exit_edge = 0, need_entry_edge = 0;
      int have_exit_edge = 0, have_entry_edge = 0;
      edge e;
      edge_iterator ei;

      /* Functions returning multiple times are not handled by extra edges.
         Instead we simply allow negative counts on edges from exit to the
         block past call and corresponding probabilities.  We can't go
         with the extra edges because that would result in flowgraph that
	 needs to have fake edges outside the spanning tree.  */

      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  gimple_stmt_iterator gsi;
	  gimple last = NULL;

	  /* It may happen that there are compiler generated statements
	     without a locus at all.  Go through the basic block from the
	     last to the first statement looking for a locus.  */
	  for (gsi = gsi_last_nondebug_bb (bb);
	       !gsi_end_p (gsi);
	       gsi_prev_nondebug (&gsi))
	    {
	      last = gsi_stmt (gsi);
	      if (gimple_has_location (last))
		break;
	    }

	  /* Edge with goto locus might get wrong coverage info unless
	     it is the only edge out of BB.
	     Don't do that when the locuses match, so
	     if (blah) goto something;
	     is not computed twice.  */
	  if (last
	      && gimple_has_location (last)
	      && e->goto_locus != UNKNOWN_LOCATION
	      && !single_succ_p (bb)
	      && (LOCATION_FILE (e->goto_locus)
	          != LOCATION_FILE (gimple_location (last))
		  || (LOCATION_LINE (e->goto_locus)
		      != LOCATION_LINE (gimple_location (last)))))
	    {
	      basic_block new_bb = split_edge (e);
	      edge ne = single_succ_edge (new_bb);
	      ne->goto_locus = e->goto_locus;
	      ne->goto_block = e->goto_block;
	    }
	  if ((e->flags & (EDGE_ABNORMAL | EDGE_ABNORMAL_CALL))
	       && e->dest != EXIT_BLOCK_PTR)
	    need_exit_edge = 1;
	  if (e->dest == EXIT_BLOCK_PTR)
	    have_exit_edge = 1;
	}
      FOR_EACH_EDGE (e, ei, bb->preds)
	{
	  if ((e->flags & (EDGE_ABNORMAL | EDGE_ABNORMAL_CALL))
	       && e->src != ENTRY_BLOCK_PTR)
	    need_entry_edge = 1;
	  if (e->src == ENTRY_BLOCK_PTR)
	    have_entry_edge = 1;
	}

      if (need_exit_edge && !have_exit_edge)
	{
	  if (dump_file)
	    fprintf (dump_file, "Adding fake exit edge to bb %i\n",
		     bb->index);
	  make_edge (bb, EXIT_BLOCK_PTR, EDGE_FAKE);
	}
      if (need_entry_edge && !have_entry_edge)
	{
	  if (dump_file)
	    fprintf (dump_file, "Adding fake entry edge to bb %i\n",
		     bb->index);
	  make_edge (ENTRY_BLOCK_PTR, bb, EDGE_FAKE);
	  /* Avoid bbs that have both fake entry edge and also some
	     exit edge.  One of those edges wouldn't be added to the
	     spanning tree, but we can't instrument any of them.  */
	  if (have_exit_edge || need_exit_edge)
	    {
	      gimple_stmt_iterator gsi;
	      gimple first;
	      tree fndecl;

	      gsi = gsi_after_labels (bb);
	      gcc_checking_assert (!gsi_end_p (gsi));
	      first = gsi_stmt (gsi);
	      if (is_gimple_debug (first))
		{
		  gsi_next_nondebug (&gsi);
		  gcc_checking_assert (!gsi_end_p (gsi));
		  first = gsi_stmt (gsi);
		}
	      /* Don't split the bbs containing __builtin_setjmp_receiver
		 or __builtin_setjmp_dispatcher calls.  These are very
		 special and don't expect anything to be inserted before
		 them.  */
	      if (!is_gimple_call (first)
		  || (fndecl = gimple_call_fndecl (first)) == NULL
		  || DECL_BUILT_IN_CLASS (fndecl) != BUILT_IN_NORMAL
		  || (DECL_FUNCTION_CODE (fndecl) != BUILT_IN_SETJMP_RECEIVER
		      && (DECL_FUNCTION_CODE (fndecl)
			  != BUILT_IN_SETJMP_DISPATCHER)))
		{
		  if (dump_file)
		    fprintf (dump_file, "Splitting bb %i after labels\n",
			     bb->index);
		  split_block_after_labels (bb);
		}
	    }
	}
    }

  el = create_edge_list ();
  num_edges = NUM_EDGES (el);
  alloc_aux_for_edges (sizeof (struct edge_info));

  /* The basic blocks are expected to be numbered sequentially.  */
  compact_blocks ();

  ignored_edges = 0;
  for (i = 0 ; i < num_edges ; i++)
    {
      edge e = INDEX_EDGE (el, i);
      e->count = 0;

      /* Mark edges we've replaced by fake edges above as ignored.  */
      if ((e->flags & (EDGE_ABNORMAL | EDGE_ABNORMAL_CALL))
	  && e->src != ENTRY_BLOCK_PTR && e->dest != EXIT_BLOCK_PTR)
	{
	  EDGE_INFO (e)->ignore = 1;
	  ignored_edges++;
	}
    }

  /* Create spanning tree from basic block graph, mark each edge that is
     on the spanning tree.  We insert as many abnormal and critical edges
     as possible to minimize number of edge splits necessary.  */

  find_spanning_tree (el);

  /* Fake edges that are not on the tree will not be instrumented, so
     mark them ignored.  */
  for (num_instrumented = i = 0; i < num_edges; i++)
    {
      edge e = INDEX_EDGE (el, i);
      struct edge_info *inf = EDGE_INFO (e);

      if (inf->ignore || inf->on_tree)
	/*NOP*/;
      else if (e->flags & EDGE_FAKE)
	{
	  inf->ignore = 1;
	  ignored_edges++;
	}
      else
	num_instrumented++;
    }

  total_num_blocks += n_basic_blocks;
  if (dump_file)
    fprintf (dump_file, "%d basic blocks\n", n_basic_blocks);

  total_num_edges += num_edges;
  if (dump_file)
    fprintf (dump_file, "%d edges\n", num_edges);

  total_num_edges_ignored += ignored_edges;
  if (dump_file)
    fprintf (dump_file, "%d ignored edges\n", ignored_edges);


  /* Compute two different checksums. Note that we want to compute
     the checksum in only once place, since it depends on the shape
     of the control flow which can change during 
     various transformations.  */
  cfg_checksum = coverage_compute_cfg_checksum ();
  lineno_checksum = coverage_compute_lineno_checksum ();

  /* Write the data from which gcov can reconstruct the basic block
     graph and function line numbers  */

  if (coverage_begin_function (lineno_checksum, cfg_checksum))
    {
      gcov_position_t offset;

      /* Basic block flags */
      offset = gcov_write_tag (GCOV_TAG_BLOCKS);
      for (i = 0; i != (unsigned) (n_basic_blocks); i++)
	gcov_write_unsigned (0);
      gcov_write_length (offset);

      /* Keep all basic block indexes nonnegative in the gcov output.
	 Index 0 is used for entry block, last index is for exit
	 block.    */
      ENTRY_BLOCK_PTR->index = 1;
      EXIT_BLOCK_PTR->index = last_basic_block;

      /* Arcs */
      FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, EXIT_BLOCK_PTR, next_bb)
	{
	  edge e;
	  edge_iterator ei;

	  offset = gcov_write_tag (GCOV_TAG_ARCS);
	  gcov_write_unsigned (BB_TO_GCOV_INDEX (bb));

	  FOR_EACH_EDGE (e, ei, bb->succs)
	    {
	      struct edge_info *i = EDGE_INFO (e);
	      if (!i->ignore)
		{
		  unsigned flag_bits = 0;

		  if (i->on_tree)
		    flag_bits |= GCOV_ARC_ON_TREE;
		  if (e->flags & EDGE_FAKE)
		    flag_bits |= GCOV_ARC_FAKE;
		  if (e->flags & EDGE_FALLTHRU)
		    flag_bits |= GCOV_ARC_FALLTHROUGH;
		  /* On trees we don't have fallthru flags, but we can
		     recompute them from CFG shape.  */
		  if (e->flags & (EDGE_TRUE_VALUE | EDGE_FALSE_VALUE)
		      && e->src->next_bb == e->dest)
		    flag_bits |= GCOV_ARC_FALLTHROUGH;

		  gcov_write_unsigned (BB_TO_GCOV_INDEX (e->dest));
		  gcov_write_unsigned (flag_bits);
	        }
	    }

	  gcov_write_length (offset);
	}

      ENTRY_BLOCK_PTR->index = ENTRY_BLOCK;
      EXIT_BLOCK_PTR->index = EXIT_BLOCK;

      /* Line numbers.  */
      /* Initialize the output.  */
      output_location (NULL, 0, NULL, NULL);

      FOR_EACH_BB (bb)
	{
	  gimple_stmt_iterator gsi;
	  gcov_position_t offset = 0;

	  if (bb == ENTRY_BLOCK_PTR->next_bb)
	    {
	      expanded_location curr_location =
		expand_location (DECL_SOURCE_LOCATION (current_function_decl));
	      output_location (curr_location.file, curr_location.line,
			       &offset, bb);
	    }

	  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	    {
	      gimple stmt = gsi_stmt (gsi);
	      if (gimple_has_location (stmt))
		output_location (gimple_filename (stmt), gimple_lineno (stmt),
				 &offset, bb);
	    }

	  /* Notice GOTO expressions eliminated while constructing the CFG.  */
	  if (single_succ_p (bb)
	      && single_succ_edge (bb)->goto_locus != UNKNOWN_LOCATION)
	    {
	      expanded_location curr_location
		= expand_location (single_succ_edge (bb)->goto_locus);
	      output_location (curr_location.file, curr_location.line,
			       &offset, bb);
	    }

	  if (offset)
	    {
	      /* A file of NULL indicates the end of run.  */
	      gcov_write_unsigned (0);
	      gcov_write_string (NULL);
	      gcov_write_length (offset);
	    }
	}
    }

#undef BB_TO_GCOV_INDEX

  if (flag_profile_values)
    gimple_find_values_to_profile (&values);

  if (flag_branch_probabilities)
    {
      compute_branch_probabilities (cfg_checksum, lineno_checksum);
      if (flag_profile_values)
	compute_value_histograms (values, cfg_checksum, lineno_checksum);
    }

  remove_fake_edges ();

  /* For each edge not on the spanning tree, add counting code.  */
  if (profile_arc_flag
      && coverage_counter_alloc (GCOV_COUNTER_ARCS, num_instrumented))
    {
      unsigned n_instrumented;

      gimple_init_edge_profiler ();

      n_instrumented = instrument_edges (el);

      gcc_assert (n_instrumented == num_instrumented);

      if (flag_profile_values)
	instrument_values (values);

      /* Commit changes done by instrumentation.  */
      gsi_commit_edge_inserts ();
    }

  free_aux_for_edges ();

  VEC_free (histogram_value, heap, values);
  free_edge_list (el);
  coverage_end_function (lineno_checksum, cfg_checksum);
}

/* Union find algorithm implementation for the basic blocks using
   aux fields.  */

static basic_block
find_group (basic_block bb)
{
  basic_block group = bb, bb1;

  while ((basic_block) group->aux != group)
    group = (basic_block) group->aux;

  /* Compress path.  */
  while ((basic_block) bb->aux != group)
    {
      bb1 = (basic_block) bb->aux;
      bb->aux = (void *) group;
      bb = bb1;
    }
  return group;
}

static void
union_groups (basic_block bb1, basic_block bb2)
{
  basic_block bb1g = find_group (bb1);
  basic_block bb2g = find_group (bb2);

  /* ??? I don't have a place for the rank field.  OK.  Lets go w/o it,
     this code is unlikely going to be performance problem anyway.  */
  gcc_assert (bb1g != bb2g);

  bb1g->aux = bb2g;
}

/* This function searches all of the edges in the program flow graph, and puts
   as many bad edges as possible onto the spanning tree.  Bad edges include
   abnormals edges, which can't be instrumented at the moment.  Since it is
   possible for fake edges to form a cycle, we will have to develop some
   better way in the future.  Also put critical edges to the tree, since they
   are more expensive to instrument.  */

static void
find_spanning_tree (struct edge_list *el)
{
  int i;
  int num_edges = NUM_EDGES (el);
  basic_block bb;

  /* We use aux field for standard union-find algorithm.  */
  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, NULL, next_bb)
    bb->aux = bb;

  /* Add fake edge exit to entry we can't instrument.  */
  union_groups (EXIT_BLOCK_PTR, ENTRY_BLOCK_PTR);

  /* First add all abnormal edges to the tree unless they form a cycle. Also
     add all edges to EXIT_BLOCK_PTR to avoid inserting profiling code behind
     setting return value from function.  */
  for (i = 0; i < num_edges; i++)
    {
      edge e = INDEX_EDGE (el, i);
      if (((e->flags & (EDGE_ABNORMAL | EDGE_ABNORMAL_CALL | EDGE_FAKE))
	   || e->dest == EXIT_BLOCK_PTR)
	  && !EDGE_INFO (e)->ignore
	  && (find_group (e->src) != find_group (e->dest)))
	{
	  if (dump_file)
	    fprintf (dump_file, "Abnormal edge %d to %d put to tree\n",
		     e->src->index, e->dest->index);
	  EDGE_INFO (e)->on_tree = 1;
	  union_groups (e->src, e->dest);
	}
    }

  /* Now insert all critical edges to the tree unless they form a cycle.  */
  for (i = 0; i < num_edges; i++)
    {
      edge e = INDEX_EDGE (el, i);
      if (EDGE_CRITICAL_P (e) && !EDGE_INFO (e)->ignore
	  && find_group (e->src) != find_group (e->dest))
	{
	  if (dump_file)
	    fprintf (dump_file, "Critical edge %d to %d put to tree\n",
		     e->src->index, e->dest->index);
	  EDGE_INFO (e)->on_tree = 1;
	  union_groups (e->src, e->dest);
	}
    }

  /* And now the rest.  */
  for (i = 0; i < num_edges; i++)
    {
      edge e = INDEX_EDGE (el, i);
      if (!EDGE_INFO (e)->ignore
	  && find_group (e->src) != find_group (e->dest))
	{
	  if (dump_file)
	    fprintf (dump_file, "Normal edge %d to %d put to tree\n",
		     e->src->index, e->dest->index);
	  EDGE_INFO (e)->on_tree = 1;
	  union_groups (e->src, e->dest);
	}
    }

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, NULL, next_bb)
    bb->aux = NULL;
}

/* Perform file-level initialization for branch-prob processing.  */

void
init_branch_prob (void)
{
  int i;

  total_num_blocks = 0;
  total_num_edges = 0;
  total_num_edges_ignored = 0;
  total_num_edges_instrumented = 0;
  total_num_blocks_created = 0;
  total_num_passes = 0;
  total_num_times_called = 0;
  total_num_branches = 0;
  for (i = 0; i < 20; i++)
    total_hist_br_prob[i] = 0;
}

/* Performs file-level cleanup after branch-prob processing
   is completed.  */

void
end_branch_prob (void)
{
  if (dump_file)
    {
      fprintf (dump_file, "\n");
      fprintf (dump_file, "Total number of blocks: %d\n",
	       total_num_blocks);
      fprintf (dump_file, "Total number of edges: %d\n", total_num_edges);
      fprintf (dump_file, "Total number of ignored edges: %d\n",
	       total_num_edges_ignored);
      fprintf (dump_file, "Total number of instrumented edges: %d\n",
	       total_num_edges_instrumented);
      fprintf (dump_file, "Total number of blocks created: %d\n",
	       total_num_blocks_created);
      fprintf (dump_file, "Total number of graph solution passes: %d\n",
	       total_num_passes);
      if (total_num_times_called != 0)
	fprintf (dump_file, "Average number of graph solution passes: %d\n",
		 (total_num_passes + (total_num_times_called  >> 1))
		 / total_num_times_called);
      fprintf (dump_file, "Total number of branches: %d\n",
	       total_num_branches);
      if (total_num_branches)
	{
	  int i;

	  for (i = 0; i < 10; i++)
	    fprintf (dump_file, "%d%% branches in range %d-%d%%\n",
		     (total_hist_br_prob[i] + total_hist_br_prob[19-i]) * 100
		     / total_num_branches, 5*i, 5*i+5);
	}
    }
}
