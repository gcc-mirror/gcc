/* Calculate branch probabilities, and basic block execution counts.
   Copyright (C) 1990-2018 Free Software Foundation, Inc.
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
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "cgraph.h"
#include "coverage.h"
#include "diagnostic-core.h"
#include "cfganal.h"
#include "value-prof.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "dumpfile.h"
#include "cfgloop.h"

#include "profile.h"

/* Map from BBs/edges to gcov counters.  */
vec<gcov_type> bb_gcov_counts;
hash_map<edge,gcov_type> *edge_gcov_counts;

struct bb_profile_info {
  unsigned int count_valid : 1;

  /* Number of successor and predecessor edges.  */
  gcov_type succ_count;
  gcov_type pred_count;
};

#define BB_INFO(b)  ((struct bb_profile_info *) (b)->aux)


/* Counter summary from the last set of coverage counts read.  */

gcov_summary *profile_info;

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

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR_FOR_FN (cfun), NULL, next_bb)
    {
      edge e;
      edge_iterator ei;

      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  struct edge_profile_info *inf = EDGE_INFO (e);

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
  unsigned i;

  /* Emit code to generate the histograms before the insns.  */

  for (i = 0; i < values.length (); i++)
    {
      histogram_value hist = values[i];
      unsigned t = COUNTER_FOR_HIST_TYPE (hist->type);

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

 	case HIST_TYPE_INDIR_CALL:
 	case HIST_TYPE_INDIR_CALL_TOPN:
 	  gimple_gen_ic_profiler (hist, t, 0);
  	  break;

	case HIST_TYPE_AVERAGE:
	  gimple_gen_average_profiler (hist, t, 0);
	  break;

	case HIST_TYPE_IOR:
	  gimple_gen_ior_profiler (hist, t, 0);
	  break;

	case HIST_TYPE_TIME_PROFILE:
	  gimple_gen_time_profiler (t, 0);
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
  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR_FOR_FN (cfun), NULL, next_bb)
    {
      edge e;
      edge_iterator ei;

      FOR_EACH_EDGE (e, ei, bb->succs)
	if (!EDGE_INFO (e)->ignore && !EDGE_INFO (e)->on_tree)
	  num_edges++;
    }

  counts = get_coverage_counts (GCOV_COUNTER_ARCS, cfg_checksum,
				lineno_checksum);
  if (!counts)
    return NULL;

  return counts;
}

static bool
is_edge_inconsistent (vec<edge, va_gc> *edges)
{
  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, edges)
    {
      if (!EDGE_INFO (e)->ignore)
        {
          if (edge_gcov_count (e) < 0
	      && (!(e->flags & EDGE_FAKE)
	          || !block_ends_with_call_p (e->src)))
	    {
	      if (dump_file)
		{
		  fprintf (dump_file,
		  	   "Edge %i->%i is inconsistent, count%" PRId64,
			   e->src->index, e->dest->index, edge_gcov_count (e));
		  dump_bb (dump_file, e->src, 0, TDF_DETAILS);
		  dump_bb (dump_file, e->dest, 0, TDF_DETAILS);
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

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR_FOR_FN (cfun), NULL, next_bb)
    {
      FOR_EACH_EDGE (e, ei, bb->succs)
        {
           if (edge_gcov_count (e) < 0)
             edge_gcov_count (e) = 0;
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
  FOR_EACH_BB_FN (bb, cfun)
    {
      inconsistent |= is_edge_inconsistent (bb->preds);
      if (!dump_file && inconsistent)
	return true;
      inconsistent |= is_edge_inconsistent (bb->succs);
      if (!dump_file && inconsistent)
	return true;
      if (bb_gcov_count (bb) < 0)
        {
	  if (dump_file)
	    {
	      fprintf (dump_file, "BB %i count is negative "
		       "%" PRId64,
		       bb->index,
		       bb_gcov_count (bb));
	      dump_bb (dump_file, bb, 0, TDF_DETAILS);
	    }
	  inconsistent = true;
	}
      if (bb_gcov_count (bb) != sum_edge_counts (bb->preds))
        {
	  if (dump_file)
	    {
	      fprintf (dump_file, "BB %i count does not match sum of incoming edges "
		       "%" PRId64" should be %" PRId64,
		       bb->index,
		       bb_gcov_count (bb),
		       sum_edge_counts (bb->preds));
	      dump_bb (dump_file, bb, 0, TDF_DETAILS);
	    }
	  inconsistent = true;
	}
      if (bb_gcov_count (bb) != sum_edge_counts (bb->succs) &&
	  ! (find_edge (bb, EXIT_BLOCK_PTR_FOR_FN (cfun)) != NULL
	     && block_ends_with_call_p (bb)))
	{
	  if (dump_file)
	    {
	      fprintf (dump_file, "BB %i count does not match sum of outgoing edges "
		       "%" PRId64" should be %" PRId64,
		       bb->index,
		       bb_gcov_count (bb),
		       sum_edge_counts (bb->succs));
	      dump_bb (dump_file, bb, 0, TDF_DETAILS);
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
  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR_FOR_FN (cfun), NULL, next_bb)
    {
      bb_gcov_count (bb) = sum_edge_counts (bb->succs);
      gcc_assert (bb_gcov_count (bb) >= 0);
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

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR_FOR_FN (cfun), NULL, next_bb)
    {
      edge e;
      edge_iterator ei;

      FOR_EACH_EDGE (e, ei, bb->succs)
	if (!EDGE_INFO (e)->ignore && !EDGE_INFO (e)->on_tree)
	  {
	    num_edges++;
	    if (exec_counts)
	      edge_gcov_count (e) = exec_counts[exec_counts_pos++];
	    else
	      edge_gcov_count (e) = 0;

	    EDGE_INFO (e)->count_valid = 1;
	    BB_INFO (bb)->succ_count--;
	    BB_INFO (e->dest)->pred_count--;
	    if (dump_file)
	      {
		fprintf (dump_file, "\nRead edge from %i to %i, count:",
			 bb->index, e->dest->index);
		fprintf (dump_file, "%" PRId64,
			 (int64_t) edge_gcov_count (e));
	      }
	  }
    }

    return num_edges;
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
    {
      if (dump_file)
	fprintf (dump_file, "Profile info is missing; giving up\n");
      return;
    }

  bb_gcov_counts.safe_grow_cleared (last_basic_block_for_fn (cfun));
  edge_gcov_counts = new hash_map<edge,gcov_type>;

  /* Attach extra info block to each bb.  */
  alloc_aux_for_blocks (sizeof (struct bb_profile_info));
  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR_FOR_FN (cfun), NULL, next_bb)
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
  BB_INFO (EXIT_BLOCK_PTR_FOR_FN (cfun))->succ_count = 2;
  BB_INFO (ENTRY_BLOCK_PTR_FOR_FN (cfun))->pred_count = 2;

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
      FOR_BB_BETWEEN (bb, EXIT_BLOCK_PTR_FOR_FN (cfun), NULL, prev_bb)
	{
	  struct bb_profile_info *bi = BB_INFO (bb);
	  if (! bi->count_valid)
	    {
	      if (bi->succ_count == 0)
		{
		  edge e;
		  edge_iterator ei;
		  gcov_type total = 0;

		  FOR_EACH_EDGE (e, ei, bb->succs)
		    total += edge_gcov_count (e);
		  bb_gcov_count (bb) = total;
		  bi->count_valid = 1;
		  changes = 1;
		}
	      else if (bi->pred_count == 0)
		{
		  edge e;
		  edge_iterator ei;
		  gcov_type total = 0;

		  FOR_EACH_EDGE (e, ei, bb->preds)
		    total += edge_gcov_count (e);
		  bb_gcov_count (bb) = total;
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
		    total += edge_gcov_count (e);

		  /* Search for the invalid edge, and set its count.  */
		  FOR_EACH_EDGE (e, ei, bb->succs)
		    if (! EDGE_INFO (e)->count_valid && ! EDGE_INFO (e)->ignore)
		      break;

		  /* Calculate count for remaining edge by conservation.  */
		  total = bb_gcov_count (bb) - total;

		  gcc_assert (e);
		  EDGE_INFO (e)->count_valid = 1;
		  edge_gcov_count (e) = total;
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
		    total += edge_gcov_count (e);

		  /* Search for the invalid edge, and set its count.  */
		  FOR_EACH_EDGE (e, ei, bb->preds)
		    if (!EDGE_INFO (e)->count_valid && !EDGE_INFO (e)->ignore)
		      break;

		  /* Calculate count for remaining edge by conservation.  */
		  total = bb_gcov_count (bb) - total + edge_gcov_count (e);

		  gcc_assert (e);
		  EDGE_INFO (e)->count_valid = 1;
		  edge_gcov_count (e) = total;
		  bi->pred_count--;

		  BB_INFO (e->src)->succ_count--;
		  changes = 1;
		}
	    }
	}
    }

  total_num_passes += passes;
  if (dump_file)
    fprintf (dump_file, "Graph solving took %d passes.\n\n", passes);

  /* If the graph has been correctly solved, every block will have a
     succ and pred count of zero.  */
  FOR_EACH_BB_FN (bb, cfun)
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
         if (dump_enabled_p () && informed == 0)
           {
             informed = 1;
             dump_printf_loc (MSG_NOTE,
			      dump_location_t::from_location_t (input_location),
                              "correcting inconsistent profile data\n");
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

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR_FOR_FN (cfun), NULL, next_bb)
    {
      edge e;
      edge_iterator ei;

      if (bb_gcov_count (bb) < 0)
	{
	  error ("corrupted profile info: number of iterations for basic block %d thought to be %i",
		 bb->index, (int)bb_gcov_count (bb));
	  bb_gcov_count (bb) = 0;
	}
      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  /* Function may return twice in the cased the called function is
	     setjmp or calls fork, but we can't represent this by extra
	     edge from the entry, since extra edge from the exit is
	     already present.  We get negative frequency from the entry
	     point.  */
	  if ((edge_gcov_count (e) < 0
	       && e->dest == EXIT_BLOCK_PTR_FOR_FN (cfun))
	      || (edge_gcov_count (e) > bb_gcov_count (bb)
		  && e->dest != EXIT_BLOCK_PTR_FOR_FN (cfun)))
	    {
	      if (block_ends_with_call_p (bb))
		edge_gcov_count (e) = edge_gcov_count (e) < 0
				      ? 0 : bb_gcov_count (bb);
	    }
	  if (edge_gcov_count (e) < 0
	      || edge_gcov_count (e) > bb_gcov_count (bb))
	    {
	      error ("corrupted profile info: number of executions for edge %d-%d thought to be %i",
		     e->src->index, e->dest->index,
		     (int)edge_gcov_count (e));
	      edge_gcov_count (e) = bb_gcov_count (bb) / 2;
	    }
	}
      if (bb_gcov_count (bb))
	{
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    e->probability = profile_probability::probability_in_gcov_type
		(edge_gcov_count (e), bb_gcov_count (bb));
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

	      prob = e->probability.to_reg_br_prob_base ();
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
      else if (profile_status_for_fn (cfun) == PROFILE_ABSENT)
	{
	  int total = 0;

	  FOR_EACH_EDGE (e, ei, bb->succs)
	    if (!(e->flags & (EDGE_COMPLEX | EDGE_FAKE)))
	      total ++;
	  if (total)
	    {
	      FOR_EACH_EDGE (e, ei, bb->succs)
		if (!(e->flags & (EDGE_COMPLEX | EDGE_FAKE)))
		  e->probability
		    = profile_probability::guessed_always ().apply_scale (1, total);
		else
		  e->probability = profile_probability::never ();
	    }
	  else
	    {
	      total += EDGE_COUNT (bb->succs);
	      FOR_EACH_EDGE (e, ei, bb->succs)
		e->probability
		 = profile_probability::guessed_always ().apply_scale (1, total);
	    }
	  if (bb->index >= NUM_FIXED_BLOCKS
	      && block_ends_with_condjump_p (bb)
	      && EDGE_COUNT (bb->succs) >= 2)
	    num_branches++;
	}
    }

  /* If we have real data, use them!  */
  if (bb_gcov_count (ENTRY_BLOCK_PTR_FOR_FN (cfun))
      || !flag_guess_branch_prob)
    FOR_ALL_BB_FN (bb, cfun)
      bb->count = profile_count::from_gcov_type (bb_gcov_count (bb));
  /* If function was not trained, preserve local estimates including statically
     determined zero counts.  */
  else
    FOR_ALL_BB_FN (bb, cfun)
      if (!(bb->count == profile_count::zero ()))
        bb->count = bb->count.global0 ();

  bb_gcov_counts.release ();
  delete edge_gcov_counts;
  edge_gcov_counts = NULL;

  update_max_bb_count ();

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
  struct cgraph_node *node;

  for (t = 0; t < GCOV_N_VALUE_COUNTERS; t++)
    n_histogram_counters[t] = 0;

  for (i = 0; i < values.length (); i++)
    {
      histogram_value hist = values[i];
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

      histogram_counts[t] = get_coverage_counts (COUNTER_FOR_HIST_TYPE (t),
						 cfg_checksum,
						 lineno_checksum);
      if (histogram_counts[t])
	any = 1;
      act_count[t] = histogram_counts[t];
    }
  if (!any)
    return;

  for (i = 0; i < values.length (); i++)
    {
      histogram_value hist = values[i];
      gimple *stmt = hist->hvalue.stmt;

      t = (int) hist->type;

      aact_count = act_count[t];

      if (act_count[t])
        act_count[t] += hist->n_counters;

      gimple_add_histogram_value (cfun, stmt, hist);
      hist->hvalue.counters =  XNEWVEC (gcov_type, hist->n_counters);
      for (j = 0; j < hist->n_counters; j++)
        if (aact_count)
          hist->hvalue.counters[j] = aact_count[j];
        else
          hist->hvalue.counters[j] = 0;

      /* Time profiler counter is not related to any statement,
         so that we have to read the counter and set the value to
         the corresponding call graph node.  */
      if (hist->type == HIST_TYPE_TIME_PROFILE)
        {
	  node = cgraph_node::get (hist->fun->decl);
	  node->tp_first_run = hist->hvalue.counters[0];

          if (dump_file)
            fprintf (dump_file, "Read tp_first_run: %d\n", node->tp_first_run);
        }
    }

  for (t = 0; t < GCOV_N_VALUE_COUNTERS; t++)
    free (histogram_counts[t]);
}

/* Location triplet which records a location.  */
struct location_triplet
{
  const char *filename;
  int lineno;
  int bb_index;
};

/* Traits class for streamed_locations hash set below.  */

struct location_triplet_hash : typed_noop_remove <location_triplet>
{
  typedef location_triplet value_type;
  typedef location_triplet compare_type;

  static hashval_t
  hash (const location_triplet &ref)
  {
    inchash::hash hstate (0);
    if (ref.filename)
      hstate.add_int (strlen (ref.filename));
    hstate.add_int (ref.lineno);
    hstate.add_int (ref.bb_index);
    return hstate.end ();
  }

  static bool
  equal (const location_triplet &ref1, const location_triplet &ref2)
  {
    return ref1.lineno == ref2.lineno
      && ref1.bb_index == ref2.bb_index
      && ref1.filename != NULL
      && ref2.filename != NULL
      && strcmp (ref1.filename, ref2.filename) == 0;
  }

  static void
  mark_deleted (location_triplet &ref)
  {
    ref.lineno = -1;
  }

  static void
  mark_empty (location_triplet &ref)
  {
    ref.lineno = -2;
  }

  static bool
  is_deleted (const location_triplet &ref)
  {
    return ref.lineno == -1;
  }

  static bool
  is_empty (const location_triplet &ref)
  {
    return ref.lineno == -2;
  }
};




/* When passed NULL as file_name, initialize.
   When passed something else, output the necessary commands to change
   line to LINE and offset to FILE_NAME.  */
static void
output_location (hash_set<location_triplet_hash> *streamed_locations,
		 char const *file_name, int line,
		 gcov_position_t *offset, basic_block bb)
{
  static char const *prev_file_name;
  static int prev_line;
  bool name_differs, line_differs;

  location_triplet triplet;
  triplet.filename = file_name;
  triplet.lineno = line;
  triplet.bb_index = bb ? bb->index : 0;

  if (streamed_locations->add (triplet))
    return;

  if (!file_name)
    {
      prev_file_name = NULL;
      prev_line = -1;
      return;
    }

  name_differs = !prev_file_name || filename_cmp (file_name, prev_file_name);
  line_differs = prev_line != line;

  if (!*offset)
    {
      *offset = gcov_write_tag (GCOV_TAG_LINES);
      gcov_write_unsigned (bb->index);
      name_differs = line_differs = true;
    }

  /* If this is a new source file, then output the
     file's name to the .bb file.  */
  if (name_differs)
    {
      prev_file_name = file_name;
      gcov_write_unsigned (0);
      gcov_write_filename (prev_file_name);
    }
  if (line_differs)
    {
      gcov_write_unsigned (line);
      prev_line = line;
    }
}

/* Helper for qsort so edges get sorted from highest frequency to smallest.
   This controls the weight for minimal spanning tree algorithm  */
static int
compare_freqs (const void *p1, const void *p2)
{
  const_edge e1 = *(const const_edge *)p1;
  const_edge e2 = *(const const_edge *)p2;

  /* Critical edges needs to be split which introduce extra control flow.
     Make them more heavy.  */
  int m1 = EDGE_CRITICAL_P (e1) ? 2 : 1;
  int m2 = EDGE_CRITICAL_P (e2) ? 2 : 1;

  if (EDGE_FREQUENCY (e1) * m1 + m1 != EDGE_FREQUENCY (e2) * m2 + m2)
    return EDGE_FREQUENCY (e2) * m2 + m2 - EDGE_FREQUENCY (e1) * m1 - m1;
  /* Stabilize sort.  */
  if (e1->src->index != e2->src->index)
    return e2->src->index - e1->src->index;
  return e2->dest->index - e1->dest->index;
}

/* Instrument and/or analyze program behavior based on program the CFG.

   This function creates a representation of the control flow graph (of
   the function being compiled) that is suitable for the instrumentation
   of edges and/or converting measured edge counts to counts on the
   complete CFG.

   When FLAG_PROFILE_ARCS is nonzero, this function instruments the edges in
   the flow graph that are needed to reconstruct the dynamic behavior of the
   flow graph.  This data is written to the gcno file for gcov.

   When FLAG_BRANCH_PROBABILITIES is nonzero, this function reads auxiliary
   information from the gcda file containing edge count information from
   previous executions of the function being compiled.  In this case, the
   control flow graph is annotated with actual execution counts by
   compute_branch_probabilities().

   Main entry point of this file.  */

void
branch_prob (void)
{
  basic_block bb;
  unsigned i;
  unsigned num_edges, ignored_edges;
  unsigned num_instrumented;
  struct edge_list *el;
  histogram_values values = histogram_values ();
  unsigned cfg_checksum, lineno_checksum;

  total_num_times_called++;

  flow_call_edges_add (NULL);
  add_noreturn_fake_exit_edges ();

  hash_set <location_triplet_hash> streamed_locations;

  /* We can't handle cyclic regions constructed using abnormal edges.
     To avoid these we replace every source of abnormal edge by a fake
     edge from entry node and every destination by fake edge to exit.
     This keeps graph acyclic and our calculation exact for all normal
     edges except for exit and entrance ones.

     We also add fake exit edges for each call and asm statement in the
     basic, since it may not return.  */

  FOR_EACH_BB_FN (bb, cfun)
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
	  gimple *last = NULL;

	  /* It may happen that there are compiler generated statements
	     without a locus at all.  Go through the basic block from the
	     last to the first statement looking for a locus.  */
	  for (gsi = gsi_last_nondebug_bb (bb);
	       !gsi_end_p (gsi);
	       gsi_prev_nondebug (&gsi))
	    {
	      last = gsi_stmt (gsi);
	      if (!RESERVED_LOCATION_P (gimple_location (last)))
		break;
	    }

	  /* Edge with goto locus might get wrong coverage info unless
	     it is the only edge out of BB.
	     Don't do that when the locuses match, so
	     if (blah) goto something;
	     is not computed twice.  */
	  if (last
	      && gimple_has_location (last)
	      && !RESERVED_LOCATION_P (e->goto_locus)
	      && !single_succ_p (bb)
	      && (LOCATION_FILE (e->goto_locus)
	          != LOCATION_FILE (gimple_location (last))
		  || (LOCATION_LINE (e->goto_locus)
		      != LOCATION_LINE (gimple_location (last)))))
	    {
	      basic_block new_bb = split_edge (e);
	      edge ne = single_succ_edge (new_bb);
	      ne->goto_locus = e->goto_locus;
	    }
	  if ((e->flags & (EDGE_ABNORMAL | EDGE_ABNORMAL_CALL))
	       && e->dest != EXIT_BLOCK_PTR_FOR_FN (cfun))
	    need_exit_edge = 1;
	  if (e->dest == EXIT_BLOCK_PTR_FOR_FN (cfun))
	    have_exit_edge = 1;
	}
      FOR_EACH_EDGE (e, ei, bb->preds)
	{
	  if ((e->flags & (EDGE_ABNORMAL | EDGE_ABNORMAL_CALL))
	       && e->src != ENTRY_BLOCK_PTR_FOR_FN (cfun))
	    need_entry_edge = 1;
	  if (e->src == ENTRY_BLOCK_PTR_FOR_FN (cfun))
	    have_entry_edge = 1;
	}

      if (need_exit_edge && !have_exit_edge)
	{
	  if (dump_file)
	    fprintf (dump_file, "Adding fake exit edge to bb %i\n",
		     bb->index);
	  make_edge (bb, EXIT_BLOCK_PTR_FOR_FN (cfun), EDGE_FAKE);
	}
      if (need_entry_edge && !have_entry_edge)
	{
	  if (dump_file)
	    fprintf (dump_file, "Adding fake entry edge to bb %i\n",
		     bb->index);
	  make_edge (ENTRY_BLOCK_PTR_FOR_FN (cfun), bb, EDGE_FAKE);
	  /* Avoid bbs that have both fake entry edge and also some
	     exit edge.  One of those edges wouldn't be added to the
	     spanning tree, but we can't instrument any of them.  */
	  if (have_exit_edge || need_exit_edge)
	    {
	      gimple_stmt_iterator gsi;
	      gimple *first;

	      gsi = gsi_start_nondebug_after_labels_bb (bb);
	      gcc_checking_assert (!gsi_end_p (gsi));
	      first = gsi_stmt (gsi);
	      /* Don't split the bbs containing __builtin_setjmp_receiver
		 or ABNORMAL_DISPATCHER calls.  These are very
		 special and don't expect anything to be inserted before
		 them.  */
	      if (is_gimple_call (first)
		  && (gimple_call_builtin_p (first, BUILT_IN_SETJMP_RECEIVER)
		      || (gimple_call_flags (first) & ECF_RETURNS_TWICE)
		      || (gimple_call_internal_p (first)
			  && (gimple_call_internal_fn (first)
			      == IFN_ABNORMAL_DISPATCHER))))
		continue;

	      if (dump_file)
		fprintf (dump_file, "Splitting bb %i after labels\n",
			 bb->index);
	      split_block_after_labels (bb);
	    }
	}
    }

  el = create_edge_list ();
  num_edges = NUM_EDGES (el);
  qsort (el->index_to_edge, num_edges, sizeof (edge), compare_freqs);
  alloc_aux_for_edges (sizeof (struct edge_profile_info));

  /* The basic blocks are expected to be numbered sequentially.  */
  compact_blocks ();

  ignored_edges = 0;
  for (i = 0 ; i < num_edges ; i++)
    {
      edge e = INDEX_EDGE (el, i);

      /* Mark edges we've replaced by fake edges above as ignored.  */
      if ((e->flags & (EDGE_ABNORMAL | EDGE_ABNORMAL_CALL))
	  && e->src != ENTRY_BLOCK_PTR_FOR_FN (cfun)
	  && e->dest != EXIT_BLOCK_PTR_FOR_FN (cfun))
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
      struct edge_profile_info *inf = EDGE_INFO (e);

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

  total_num_blocks += n_basic_blocks_for_fn (cfun);
  if (dump_file)
    fprintf (dump_file, "%d basic blocks\n", n_basic_blocks_for_fn (cfun));

  total_num_edges += num_edges;
  if (dump_file)
    fprintf (dump_file, "%d edges\n", num_edges);

  total_num_edges_ignored += ignored_edges;
  if (dump_file)
    fprintf (dump_file, "%d ignored edges\n", ignored_edges);

  total_num_edges_instrumented += num_instrumented;
  if (dump_file)
    fprintf (dump_file, "%d instrumentation edges\n", num_instrumented);

  /* Compute two different checksums. Note that we want to compute
     the checksum in only once place, since it depends on the shape
     of the control flow which can change during 
     various transformations.  */
  cfg_checksum = coverage_compute_cfg_checksum (cfun);
  lineno_checksum = coverage_compute_lineno_checksum ();

  /* Write the data from which gcov can reconstruct the basic block
     graph and function line numbers (the gcno file).  */
  if (coverage_begin_function (lineno_checksum, cfg_checksum))
    {
      gcov_position_t offset;

      /* Basic block flags */
      offset = gcov_write_tag (GCOV_TAG_BLOCKS);
      gcov_write_unsigned (n_basic_blocks_for_fn (cfun));
      gcov_write_length (offset);

      /* Arcs */
      FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR_FOR_FN (cfun),
		      EXIT_BLOCK_PTR_FOR_FN (cfun), next_bb)
	{
	  edge e;
	  edge_iterator ei;

	  offset = gcov_write_tag (GCOV_TAG_ARCS);
	  gcov_write_unsigned (bb->index);

	  FOR_EACH_EDGE (e, ei, bb->succs)
	    {
	      struct edge_profile_info *i = EDGE_INFO (e);
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

		  gcov_write_unsigned (e->dest->index);
		  gcov_write_unsigned (flag_bits);
	        }
	    }

	  gcov_write_length (offset);
	}

      /* Line numbers.  */
      /* Initialize the output.  */
      output_location (&streamed_locations, NULL, 0, NULL, NULL);

      hash_set<int_hash <location_t, 0, 2> > seen_locations;

      FOR_EACH_BB_FN (bb, cfun)
	{
	  gimple_stmt_iterator gsi;
	  gcov_position_t offset = 0;

	  if (bb == ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb)
	    {
	      location_t loc = DECL_SOURCE_LOCATION (current_function_decl);
	      seen_locations.add (loc);
	      expanded_location curr_location = expand_location (loc);
	      output_location (&streamed_locations, curr_location.file,
			       curr_location.line, &offset, bb);
	    }

	  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	    {
	      gimple *stmt = gsi_stmt (gsi);
	      location_t loc = gimple_location (stmt);
	      if (!RESERVED_LOCATION_P (loc))
		{
		  seen_locations.add (loc);
		  output_location (&streamed_locations, gimple_filename (stmt),
				   gimple_lineno (stmt), &offset, bb);
		}
	    }

	  /* Notice GOTO expressions eliminated while constructing the CFG.
	     It's hard to distinguish such expression, but goto_locus should
	     not be any of already seen location.  */
	  location_t loc;
	  if (single_succ_p (bb)
	      && (loc = single_succ_edge (bb)->goto_locus)
	      && !RESERVED_LOCATION_P (loc)
	      && !seen_locations.contains (loc))
	    {
	      expanded_location curr_location = expand_location (loc);
	      output_location (&streamed_locations, curr_location.file,
			       curr_location.line, &offset, bb);
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

      gimple_init_gcov_profiler ();

      n_instrumented = instrument_edges (el);

      gcc_assert (n_instrumented == num_instrumented);

      if (flag_profile_values)
	instrument_values (values);

      /* Commit changes done by instrumentation.  */
      gsi_commit_edge_inserts ();
    }

  free_aux_for_edges ();

  values.release ();
  free_edge_list (el);
  coverage_end_function (lineno_checksum, cfg_checksum);
  if (flag_branch_probabilities && profile_info)
    {
      struct loop *loop;
      if (dump_file && (dump_flags & TDF_DETAILS))
	report_predictor_hitrates ();
      profile_status_for_fn (cfun) = PROFILE_READ;

      /* At this moment we have precise loop iteration count estimates.
	 Record them to loop structure before the profile gets out of date. */
      FOR_EACH_LOOP (loop, 0)
	if (loop->header->count > 0)
	  {
	    gcov_type nit = expected_loop_iterations_unbounded (loop);
	    widest_int bound = gcov_type_to_wide_int (nit);
	    loop->any_estimate = false;
	    record_niter_bound (loop, bound, true, false);
	  }
      compute_function_frequency ();
    }
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
  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR_FOR_FN (cfun), NULL, next_bb)
    bb->aux = bb;

  /* Add fake edge exit to entry we can't instrument.  */
  union_groups (EXIT_BLOCK_PTR_FOR_FN (cfun), ENTRY_BLOCK_PTR_FOR_FN (cfun));

  /* First add all abnormal edges to the tree unless they form a cycle. Also
     add all edges to the exit block to avoid inserting profiling code behind
     setting return value from function.  */
  for (i = 0; i < num_edges; i++)
    {
      edge e = INDEX_EDGE (el, i);
      if (((e->flags & (EDGE_ABNORMAL | EDGE_ABNORMAL_CALL | EDGE_FAKE))
	   || e->dest == EXIT_BLOCK_PTR_FOR_FN (cfun))
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

  /* And now the rest.  Edge list is sorted according to frequencies and
     thus we will produce minimal spanning tree.  */
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

  clear_aux_for_blocks ();
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
