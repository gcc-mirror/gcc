/* Calculate branch probabilities, and basic block execution counts.
   Copyright (C) 1990, 1991, 1992, 1993, 1994, 1996, 1997, 1998, 1999,
   2000, 2001, 2002, 2003  Free Software Foundation, Inc.
   Contributed by James E. Wilson, UC Berkeley/Cygnus Support;
   based on some ideas from Dain Samples of UC Berkeley.
   Further mangling by Bob Manson, Cygnus Support.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

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

   The auxiliary file generated is <dumpbase>.bbg. The format is
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
#include "toplev.h"
#include "coverage.h"
#include "value-prof.h"
#include "tree.h"

/* Additional information about the edges we need.  */
struct edge_info {
  unsigned int count_valid : 1;

  /* Is on the spanning tree.  */
  unsigned int on_tree : 1;

  /* Pretend this edge does not exist (it is abnormal and we've
     inserted a fake to compensate).  */
  unsigned int ignore : 1;
};

struct bb_info {
  unsigned int count_valid : 1;

  /* Number of successor and predecessor edges.  */
  gcov_type succ_count;
  gcov_type pred_count;
};

#define EDGE_INFO(e)  ((struct edge_info *) (e)->aux)
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
static int total_num_never_executed;
static int total_num_branches;

/* Forward declarations.  */
static void find_spanning_tree (struct edge_list *);
static rtx gen_edge_profiler (int);
static rtx gen_interval_profiler (struct histogram_value *, unsigned,
				  unsigned);
static rtx gen_pow2_profiler (struct histogram_value *, unsigned, unsigned);
static rtx gen_one_value_profiler (struct histogram_value *, unsigned,
				   unsigned);
static rtx gen_const_delta_profiler (struct histogram_value *, unsigned,
				     unsigned);
static unsigned instrument_edges (struct edge_list *);
static void instrument_values (unsigned, struct histogram_value *);
static void compute_branch_probabilities (void);
static void compute_value_histograms (unsigned, struct histogram_value *);
static gcov_type * get_exec_counts (void);
static basic_block find_group (basic_block);
static void union_groups (basic_block, basic_block);


/* Add edge instrumentation code to the entire insn chain.

   F is the first insn of the chain.
   NUM_BLOCKS is the number of basic blocks found in F.  */

static unsigned
instrument_edges (struct edge_list *el)
{
  unsigned num_instr_edges = 0;
  int num_edges = NUM_EDGES (el);
  basic_block bb;

  remove_fake_edges ();

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, NULL, next_bb)
    {
      edge e;

      for (e = bb->succ; e; e = e->succ_next)
	{
	  struct edge_info *inf = EDGE_INFO (e);

	  if (!inf->ignore && !inf->on_tree)
	    {
	      rtx edge_profile;

	      if (e->flags & EDGE_ABNORMAL)
		abort ();
	      if (rtl_dump_file)
		fprintf (rtl_dump_file, "Edge %d to %d instrumented%s\n",
			 e->src->index, e->dest->index,
			 EDGE_CRITICAL_P (e) ? " (and split)" : "");
	      edge_profile = gen_edge_profiler (num_instr_edges++);
	      insert_insn_on_edge (edge_profile, e);
	      rebuild_jump_labels (e->insns);
	    }
	}
    }

  total_num_blocks_created += num_edges;
  if (rtl_dump_file)
    fprintf (rtl_dump_file, "%d edges instrumented\n", num_instr_edges);
  return num_instr_edges;
}

/* Add code to measure histograms list of VALUES of length N_VALUES.  */
static void
instrument_values (unsigned n_values, struct histogram_value *values)
{
  rtx sequence;
  unsigned i, t;
  edge e;

  /* Emit code to generate the histograms before the insns.  */

  for (i = 0; i < n_values; i++)
    {
      e = split_block (BLOCK_FOR_INSN (values[i].insn),
		       PREV_INSN (values[i].insn));
      switch (values[i].type)
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

	default:
	  abort ();
	}
      if (!coverage_counter_alloc (t, values[i].n_counters))
	continue;

      switch (values[i].type)
	{
	case HIST_TYPE_INTERVAL:
	  sequence = gen_interval_profiler (values + i, t, 0);
	  break;

	case HIST_TYPE_POW2:
	  sequence = gen_pow2_profiler (values + i, t, 0);
	  break;

	case HIST_TYPE_SINGLE_VALUE:
	  sequence = gen_one_value_profiler (values + i, t, 0);
	  break;

	case HIST_TYPE_CONST_DELTA:
	  sequence = gen_const_delta_profiler (values + i, t, 0);
	  break;

	default:
	  abort ();
	}

      safe_insert_insn_on_edge (sequence, e);
    }
}


/* Computes hybrid profile for all matching entries in da_file.  */

static gcov_type *
get_exec_counts (void)
{
  unsigned num_edges = 0;
  basic_block bb;
  gcov_type *counts;

  /* Count the edges to be (possibly) instrumented.  */
  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, NULL, next_bb)
    {
      edge e;
      for (e = bb->succ; e; e = e->succ_next)
	if (!EDGE_INFO (e)->ignore && !EDGE_INFO (e)->on_tree)
	  num_edges++;
    }

  counts = get_coverage_counts (GCOV_COUNTER_ARCS, num_edges, &profile_info);
  if (!counts)
    return NULL;

  if (rtl_dump_file && profile_info)
    fprintf(rtl_dump_file, "Merged %u profiles with maximal count %u.\n",
	    profile_info->runs, (unsigned) profile_info->sum_max);

  return counts;
}


/* Compute the branch probabilities for the various branches.
   Annotate them accordingly.  */

static void
compute_branch_probabilities (void)
{
  basic_block bb;
  int i;
  int num_edges = 0;
  int changes;
  int passes;
  int hist_br_prob[20];
  int num_never_executed;
  int num_branches;
  gcov_type *exec_counts = get_exec_counts ();
  int exec_counts_pos = 0;

  /* Very simple sanity checks so we catch bugs in our profiling code.  */
  if (profile_info)
    {
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
    }

  /* Attach extra info block to each bb.  */

  alloc_aux_for_blocks (sizeof (struct bb_info));
  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, NULL, next_bb)
    {
      edge e;

      for (e = bb->succ; e; e = e->succ_next)
	if (!EDGE_INFO (e)->ignore)
	  BB_INFO (bb)->succ_count++;
      for (e = bb->pred; e; e = e->pred_next)
	if (!EDGE_INFO (e)->ignore)
	  BB_INFO (bb)->pred_count++;
    }

  /* Avoid predicting entry on exit nodes.  */
  BB_INFO (EXIT_BLOCK_PTR)->succ_count = 2;
  BB_INFO (ENTRY_BLOCK_PTR)->pred_count = 2;

  /* For each edge not on the spanning tree, set its execution count from
     the .da file.  */

  /* The first count in the .da file is the number of times that the function
     was entered.  This is the exec_count for block zero.  */

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, NULL, next_bb)
    {
      edge e;
      for (e = bb->succ; e; e = e->succ_next)
	if (!EDGE_INFO (e)->ignore && !EDGE_INFO (e)->on_tree)
	  {
	    num_edges++;
	    if (exec_counts)
	      {
		e->count = exec_counts[exec_counts_pos++];
		if (e->count > profile_info->sum_max)
		  {
		    error ("corrupted profile info: edge from %i to %i exceeds maximal count",
			   bb->index, e->dest->index);
		  }
	      }
	    else
	      e->count = 0;

	    EDGE_INFO (e)->count_valid = 1;
	    BB_INFO (bb)->succ_count--;
	    BB_INFO (e->dest)->pred_count--;
	    if (rtl_dump_file)
	      {
		fprintf (rtl_dump_file, "\nRead edge from %i to %i, count:",
			 bb->index, e->dest->index);
		fprintf (rtl_dump_file, HOST_WIDEST_INT_PRINT_DEC,
			 (HOST_WIDEST_INT) e->count);
	      }
	  }
    }

  if (rtl_dump_file)
    fprintf (rtl_dump_file, "\n%d edge counts read\n", num_edges);

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
		  gcov_type total = 0;

		  for (e = bb->succ; e; e = e->succ_next)
		    total += e->count;
		  bb->count = total;
		  bi->count_valid = 1;
		  changes = 1;
		}
	      else if (bi->pred_count == 0)
		{
		  edge e;
		  gcov_type total = 0;

		  for (e = bb->pred; e; e = e->pred_next)
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
		  gcov_type total = 0;

		  /* One of the counts will be invalid, but it is zero,
		     so adding it in also doesn't hurt.  */
		  for (e = bb->succ; e; e = e->succ_next)
		    total += e->count;

		  /* Seedgeh for the invalid edge, and set its count.  */
		  for (e = bb->succ; e; e = e->succ_next)
		    if (! EDGE_INFO (e)->count_valid && ! EDGE_INFO (e)->ignore)
		      break;

		  /* Calculate count for remaining edge by conservation.  */
		  total = bb->count - total;

		  if (! e)
		    abort ();
		  EDGE_INFO (e)->count_valid = 1;
		  e->count = total;
		  bi->succ_count--;

		  BB_INFO (e->dest)->pred_count--;
		  changes = 1;
		}
	      if (bi->pred_count == 1)
		{
		  edge e;
		  gcov_type total = 0;

		  /* One of the counts will be invalid, but it is zero,
		     so adding it in also doesn't hurt.  */
		  for (e = bb->pred; e; e = e->pred_next)
		    total += e->count;

		  /* Search for the invalid edge, and set its count.  */
		  for (e = bb->pred; e; e = e->pred_next)
		    if (!EDGE_INFO (e)->count_valid && !EDGE_INFO (e)->ignore)
		      break;

		  /* Calculate count for remaining edge by conservation.  */
		  total = bb->count - total + e->count;

		  if (! e)
		    abort ();
		  EDGE_INFO (e)->count_valid = 1;
		  e->count = total;
		  bi->pred_count--;

		  BB_INFO (e->src)->succ_count--;
		  changes = 1;
		}
	    }
	}
    }
  if (rtl_dump_file)
    dump_flow_info (rtl_dump_file);

  total_num_passes += passes;
  if (rtl_dump_file)
    fprintf (rtl_dump_file, "Graph solving took %d passes.\n\n", passes);

  /* If the graph has been correctly solved, every block will have a
     succ and pred count of zero.  */
  FOR_EACH_BB (bb)
    {
      if (BB_INFO (bb)->succ_count || BB_INFO (bb)->pred_count)
	abort ();
    }

  /* For every edge, calculate its branch probability and add a reg_note
     to the branch insn to indicate this.  */

  for (i = 0; i < 20; i++)
    hist_br_prob[i] = 0;
  num_never_executed = 0;
  num_branches = 0;

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, NULL, next_bb)
    {
      edge e;
      rtx note;

      if (bb->count < 0)
	{
	  error ("corrupted profile info: number of iterations for basic block %d thought to be %i",
		 bb->index, (int)bb->count);
	  bb->count = 0;
	}
      for (e = bb->succ; e; e = e->succ_next)
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
	      rtx insn = BB_END (bb);

	      while (GET_CODE (insn) != CALL_INSN
		     && insn != BB_HEAD (bb)
		     && keep_with_call_p (insn))
		insn = PREV_INSN (insn);
	      if (GET_CODE (insn) == CALL_INSN)
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
	  for (e = bb->succ; e; e = e->succ_next)
	    e->probability = (e->count * REG_BR_PROB_BASE + bb->count / 2) / bb->count;
	  if (bb->index >= 0
	      && any_condjump_p (BB_END (bb))
	      && bb->succ->succ_next)
	    {
	      int prob;
	      edge e;
	      int index;

	      /* Find the branch edge.  It is possible that we do have fake
		 edges here.  */
	      for (e = bb->succ; e->flags & (EDGE_FAKE | EDGE_FALLTHRU);
		   e = e->succ_next)
		continue; /* Loop body has been intentionally left blank.  */

	      prob = e->probability;
	      index = prob * 20 / REG_BR_PROB_BASE;

	      if (index == 20)
		index = 19;
	      hist_br_prob[index]++;

	      note = find_reg_note (BB_END (bb), REG_BR_PROB, 0);
	      /* There may be already note put by some other pass, such
		 as builtin_expect expander.  */
	      if (note)
		XEXP (note, 0) = GEN_INT (prob);
	      else
		REG_NOTES (BB_END (bb))
		  = gen_rtx_EXPR_LIST (REG_BR_PROB, GEN_INT (prob),
				       REG_NOTES (BB_END (bb)));
	      num_branches++;
	    }
	}
      /* Otherwise distribute the probabilities evenly so we get sane
	 sum.  Use simple heuristics that if there are normal edges,
	 give all abnormals frequency of 0, otherwise distribute the
	 frequency over abnormals (this is the case of noreturn
	 calls).  */
      else
	{
	  int total = 0;

	  for (e = bb->succ; e; e = e->succ_next)
	    if (!(e->flags & (EDGE_COMPLEX | EDGE_FAKE)))
	      total ++;
	  if (total)
	    {
	      for (e = bb->succ; e; e = e->succ_next)
		if (!(e->flags & (EDGE_COMPLEX | EDGE_FAKE)))
		  e->probability = REG_BR_PROB_BASE / total;
		else
		  e->probability = 0;
	    }
	  else
	    {
	      for (e = bb->succ; e; e = e->succ_next)
		total ++;
	      for (e = bb->succ; e; e = e->succ_next)
		e->probability = REG_BR_PROB_BASE / total;
	    }
	  if (bb->index >= 0
	      && any_condjump_p (BB_END (bb))
	      && bb->succ->succ_next)
	    num_branches++, num_never_executed;
	}
    }

  if (rtl_dump_file)
    {
      fprintf (rtl_dump_file, "%d branches\n", num_branches);
      fprintf (rtl_dump_file, "%d branches never executed\n",
	       num_never_executed);
      if (num_branches)
	for (i = 0; i < 10; i++)
	  fprintf (rtl_dump_file, "%d%% branches in range %d-%d%%\n",
		   (hist_br_prob[i] + hist_br_prob[19-i]) * 100 / num_branches,
		   5 * i, 5 * i + 5);

      total_num_branches += num_branches;
      total_num_never_executed += num_never_executed;
      for (i = 0; i < 20; i++)
	total_hist_br_prob[i] += hist_br_prob[i];

      fputc ('\n', rtl_dump_file);
      fputc ('\n', rtl_dump_file);
    }

  free_aux_for_blocks ();
}

/* Load value histograms for N_VALUES values whose description is stored
   in VALUES array from .da file.  */
static void
compute_value_histograms (unsigned n_values, struct histogram_value *values)
{
  unsigned i, j, t, any;
  unsigned n_histogram_counters[GCOV_N_VALUE_COUNTERS];
  gcov_type *histogram_counts[GCOV_N_VALUE_COUNTERS];
  gcov_type *act_count[GCOV_N_VALUE_COUNTERS];
  gcov_type *aact_count;
 
  for (t = 0; t < GCOV_N_VALUE_COUNTERS; t++)
    n_histogram_counters[t] = 0;

  for (i = 0; i < n_values; i++)
    n_histogram_counters[(int) (values[i].type)] += values[i].n_counters;

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
			     n_histogram_counters[t], NULL);
      if (histogram_counts[t])
	any = 1;
      act_count[t] = histogram_counts[t];
    }
  if (!any)
    return;

  for (i = 0; i < n_values; i++)
    {
      rtx hist_list = NULL_RTX;
      t = (int) (values[i].type);

      aact_count = act_count[t];
      act_count[t] += values[i].n_counters;
      for (j = values[i].n_counters; j > 0; j--)
	hist_list = alloc_EXPR_LIST (0, GEN_INT (aact_count[j - 1]), hist_list);
      hist_list = alloc_EXPR_LIST (0, copy_rtx (values[i].value), hist_list);
      hist_list = alloc_EXPR_LIST (0, GEN_INT (values[i].type), hist_list);
      REG_NOTES (values[i].insn) =
	      alloc_EXPR_LIST (REG_VALUE_PROFILE, hist_list,
			       REG_NOTES (values[i].insn));
    }

  for (t = 0; t < GCOV_N_VALUE_COUNTERS; t++)
    if (histogram_counts[t])
      free (histogram_counts[t]);
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
  unsigned n_values = 0;
  struct histogram_value *values = NULL;

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

      /* Functions returning multiple times are not handled by extra edges.
         Instead we simply allow negative counts on edges from exit to the
         block past call and corresponding probabilities.  We can't go
         with the extra edges because that would result in flowgraph that
	 needs to have fake edges outside the spanning tree.  */

      for (e = bb->succ; e; e = e->succ_next)
	{
	  if ((e->flags & (EDGE_ABNORMAL | EDGE_ABNORMAL_CALL))
	       && e->dest != EXIT_BLOCK_PTR)
	    need_exit_edge = 1;
	  if (e->dest == EXIT_BLOCK_PTR)
	    have_exit_edge = 1;
	}
      for (e = bb->pred; e; e = e->pred_next)
	{
	  if ((e->flags & (EDGE_ABNORMAL | EDGE_ABNORMAL_CALL))
	       && e->src != ENTRY_BLOCK_PTR)
	    need_entry_edge = 1;
	  if (e->src == ENTRY_BLOCK_PTR)
	    have_entry_edge = 1;
	}

      if (need_exit_edge && !have_exit_edge)
	{
	  if (rtl_dump_file)
	    fprintf (rtl_dump_file, "Adding fake exit edge to bb %i\n",
		     bb->index);
	  make_edge (bb, EXIT_BLOCK_PTR, EDGE_FAKE);
	}
      if (need_entry_edge && !have_entry_edge)
	{
	  if (rtl_dump_file)
	    fprintf (rtl_dump_file, "Adding fake entry edge to bb %i\n",
		     bb->index);
	  make_edge (ENTRY_BLOCK_PTR, bb, EDGE_FAKE);
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

#ifdef ENABLE_CHECKING
  verify_flow_info ();
#endif

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

  total_num_blocks += n_basic_blocks + 2;
  if (rtl_dump_file)
    fprintf (rtl_dump_file, "%d basic blocks\n", n_basic_blocks);

  total_num_edges += num_edges;
  if (rtl_dump_file)
    fprintf (rtl_dump_file, "%d edges\n", num_edges);

  total_num_edges_ignored += ignored_edges;
  if (rtl_dump_file)
    fprintf (rtl_dump_file, "%d ignored edges\n", ignored_edges);

  /* Write the data from which gcov can reconstruct the basic block
     graph.  */

  /* Basic block flags */
  if (coverage_begin_output ())
    {
      gcov_position_t offset;

      offset = gcov_write_tag (GCOV_TAG_BLOCKS);
      for (i = 0; i != (unsigned) (n_basic_blocks + 2); i++)
	gcov_write_unsigned (0);
      gcov_write_length (offset);
    }

   /* Keep all basic block indexes nonnegative in the gcov output.
      Index 0 is used for entry block, last index is for exit block.
      */
  ENTRY_BLOCK_PTR->index = -1;
  EXIT_BLOCK_PTR->index = last_basic_block;
#define BB_TO_GCOV_INDEX(bb)  ((bb)->index + 1)

  /* Arcs */
  if (coverage_begin_output ())
    {
      gcov_position_t offset;

      FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, EXIT_BLOCK_PTR, next_bb)
	{
	  edge e;

	  offset = gcov_write_tag (GCOV_TAG_ARCS);
	  gcov_write_unsigned (BB_TO_GCOV_INDEX (bb));

	  for (e = bb->succ; e; e = e->succ_next)
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

		  gcov_write_unsigned (BB_TO_GCOV_INDEX (e->dest));
		  gcov_write_unsigned (flag_bits);
	        }
	    }

	  gcov_write_length (offset);
	}
    }

  /* Line numbers.  */
  if (coverage_begin_output ())
    {
      char const *prev_file_name = NULL;
      gcov_position_t offset;

      FOR_EACH_BB (bb)
	{
	  rtx insn = BB_HEAD (bb);
	  int ignore_next_note = 0;

	  offset = 0;

	  /* We are looking for line number notes.  Search backward
	     before basic block to find correct ones.  */
	  insn = prev_nonnote_insn (insn);
	  if (!insn)
	    insn = get_insns ();
	  else
	    insn = NEXT_INSN (insn);

	  while (insn != BB_END (bb))
	    {
	      if (GET_CODE (insn) == NOTE)
		{
		  /* Must ignore the line number notes that
		     immediately follow the end of an inline function
		     to avoid counting it twice.  There is a note
		     before the call, and one after the call.  */
		  if (NOTE_LINE_NUMBER (insn)
		      == NOTE_INSN_REPEATED_LINE_NUMBER)
		    ignore_next_note = 1;
		  else if (NOTE_LINE_NUMBER (insn) <= 0)
		    /*NOP*/;
		  else if (ignore_next_note)
		    ignore_next_note = 0;
		  else
		    {
		      if (!offset)
			{
			  offset = gcov_write_tag (GCOV_TAG_LINES);
			  gcov_write_unsigned (BB_TO_GCOV_INDEX (bb));
			}

		      /* If this is a new source file, then output the
			 file's name to the .bb file.  */
		      if (!prev_file_name
			  || strcmp (NOTE_SOURCE_FILE (insn),
				     prev_file_name))
			{
			  prev_file_name = NOTE_SOURCE_FILE (insn);
			  gcov_write_unsigned (0);
			  gcov_write_string (prev_file_name);
			}
		      gcov_write_unsigned (NOTE_LINE_NUMBER (insn));
		    }
		}
	      insn = NEXT_INSN (insn);
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
  ENTRY_BLOCK_PTR->index = ENTRY_BLOCK;
  EXIT_BLOCK_PTR->index = EXIT_BLOCK;
#undef BB_TO_GCOV_INDEX

  if (flag_profile_values)
    {
      life_analysis (get_insns (), NULL, PROP_DEATH_NOTES);
      find_values_to_profile (&n_values, &values);
      allocate_reg_info (max_reg_num (), FALSE, FALSE);
    }

  if (flag_branch_probabilities)
    {
      compute_branch_probabilities ();
      if (flag_profile_values)
	compute_value_histograms (n_values, values);
    }

  /* For each edge not on the spanning tree, add counting code as rtl.  */
  if (profile_arc_flag
      && coverage_counter_alloc (GCOV_COUNTER_ARCS, num_instrumented))
    {
      unsigned n_instrumented = instrument_edges (el);

      if (n_instrumented != num_instrumented)
	abort ();

      if (flag_profile_values)
	instrument_values (n_values, values);

      /* Commit changes done by instrumentation.  */
      commit_edge_insertions_watch_calls ();
      allocate_reg_info (max_reg_num (), FALSE, FALSE);
    }

  remove_fake_edges ();
  free_aux_for_edges ();
  /* Re-merge split basic blocks and the mess introduced by
     insert_insn_on_edge.  */
  cleanup_cfg (profile_arc_flag ? CLEANUP_EXPENSIVE : 0);
  if (rtl_dump_file)
    dump_flow_info (rtl_dump_file);

  free_edge_list (el);
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
  if (bb1g == bb2g)
    abort ();

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
	  if (rtl_dump_file)
	    fprintf (rtl_dump_file, "Abnormal edge %d to %d put to tree\n",
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
	  if (rtl_dump_file)
	    fprintf (rtl_dump_file, "Critical edge %d to %d put to tree\n",
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
	  if (rtl_dump_file)
	    fprintf (rtl_dump_file, "Normal edge %d to %d put to tree\n",
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
  total_num_never_executed = 0;
  for (i = 0; i < 20; i++)
    total_hist_br_prob[i] = 0;
}

/* Performs file-level cleanup after branch-prob processing
   is completed.  */

void
end_branch_prob (void)
{
  if (rtl_dump_file)
    {
      fprintf (rtl_dump_file, "\n");
      fprintf (rtl_dump_file, "Total number of blocks: %d\n",
	       total_num_blocks);
      fprintf (rtl_dump_file, "Total number of edges: %d\n", total_num_edges);
      fprintf (rtl_dump_file, "Total number of ignored edges: %d\n",
	       total_num_edges_ignored);
      fprintf (rtl_dump_file, "Total number of instrumented edges: %d\n",
	       total_num_edges_instrumented);
      fprintf (rtl_dump_file, "Total number of blocks created: %d\n",
	       total_num_blocks_created);
      fprintf (rtl_dump_file, "Total number of graph solution passes: %d\n",
	       total_num_passes);
      if (total_num_times_called != 0)
	fprintf (rtl_dump_file, "Average number of graph solution passes: %d\n",
		 (total_num_passes + (total_num_times_called  >> 1))
		 / total_num_times_called);
      fprintf (rtl_dump_file, "Total number of branches: %d\n",
	       total_num_branches);
      fprintf (rtl_dump_file, "Total number of branches never executed: %d\n",
	       total_num_never_executed);
      if (total_num_branches)
	{
	  int i;

	  for (i = 0; i < 10; i++)
	    fprintf (rtl_dump_file, "%d%% branches in range %d-%d%%\n",
		     (total_hist_br_prob[i] + total_hist_br_prob[19-i]) * 100
		     / total_num_branches, 5*i, 5*i+5);
	}
    }
}


/* Output instructions as RTL to increment the edge execution count.  */

static rtx
gen_edge_profiler (int edgeno)
{
  rtx ref = coverage_counter_ref (GCOV_COUNTER_ARCS, edgeno);
  rtx tmp;
  enum machine_mode mode = GET_MODE (ref);
  rtx sequence;

  start_sequence ();
  ref = validize_mem (ref);

  tmp = expand_simple_binop (mode, PLUS, ref, const1_rtx,
			     ref, 0, OPTAB_WIDEN);

  if (tmp != ref)
    emit_move_insn (copy_rtx (ref), tmp);

  sequence = get_insns ();
  end_sequence ();
  return sequence;
}

/* Output instructions as RTL to increment the interval histogram counter.
   VALUE is the expression whose value is profiled.  TAG is the tag of the
   section for counters, BASE is offset of the counter position.  */

static rtx
gen_interval_profiler (struct histogram_value *value, unsigned tag,
		       unsigned base)
{
  unsigned gcov_size = tree_low_cst (TYPE_SIZE (GCOV_TYPE_NODE), 1);
  enum machine_mode mode = mode_for_size (gcov_size, MODE_INT, 0);
  rtx mem_ref, tmp, tmp1, mr, val;
  rtx sequence;
  rtx more_label = gen_label_rtx ();
  rtx less_label = gen_label_rtx ();
  rtx end_of_code_label = gen_label_rtx ();
  int per_counter = gcov_size / BITS_PER_UNIT;

  start_sequence ();

  if (value->seq)
    emit_insn (value->seq);

  mr = gen_reg_rtx (Pmode);

  tmp = coverage_counter_ref (tag, base);
  tmp = force_reg (Pmode, XEXP (tmp, 0));

  val = expand_simple_binop (value->mode, MINUS,
			     copy_rtx (value->value),
			     GEN_INT (value->hdata.intvl.int_start),
			     NULL_RTX, 0, OPTAB_WIDEN);

  if (value->hdata.intvl.may_be_more)
    do_compare_rtx_and_jump (copy_rtx (val), GEN_INT (value->hdata.intvl.steps),
			     GE, 0, value->mode, NULL_RTX, NULL_RTX, more_label);
  if (value->hdata.intvl.may_be_less)
    do_compare_rtx_and_jump (copy_rtx (val), const0_rtx, LT, 0, value->mode,
			     NULL_RTX, NULL_RTX, less_label);

  /* We are in range.  */
  tmp1 = expand_simple_binop (value->mode, MULT,
			      copy_rtx (val), GEN_INT (per_counter),
			      NULL_RTX, 0, OPTAB_WIDEN);
  tmp1 = expand_simple_binop (Pmode, PLUS, copy_rtx (tmp), tmp1, mr,
			      0, OPTAB_WIDEN);
  if (tmp1 != mr)
    emit_move_insn (copy_rtx (mr), tmp1);

  if (value->hdata.intvl.may_be_more
      || value->hdata.intvl.may_be_less)
    {
      emit_jump_insn (gen_jump (end_of_code_label));
      emit_barrier ();
    }

  /* Above the interval.  */
  if (value->hdata.intvl.may_be_more)
    {
      emit_label (more_label);
      tmp1 = expand_simple_binop (Pmode, PLUS, copy_rtx (tmp),
				  GEN_INT (per_counter * value->hdata.intvl.steps),
				  mr, 0, OPTAB_WIDEN);
      if (tmp1 != mr)
	emit_move_insn (copy_rtx (mr), tmp1);
      if (value->hdata.intvl.may_be_less)
	{
	  emit_jump_insn (gen_jump (end_of_code_label));
	  emit_barrier ();
	}
    }

  /* Below the interval.  */
  if (value->hdata.intvl.may_be_less)
    {
      emit_label (less_label);
      tmp1 = expand_simple_binop (Pmode, PLUS, copy_rtx (tmp),
		GEN_INT (per_counter * (value->hdata.intvl.steps
					+ (value->hdata.intvl.may_be_more ? 1 : 0))),
		mr, 0, OPTAB_WIDEN);
      if (tmp1 != mr)
	emit_move_insn (copy_rtx (mr), tmp1);
    }

  if (value->hdata.intvl.may_be_more
      || value->hdata.intvl.may_be_less)
    emit_label (end_of_code_label);

  mem_ref = validize_mem (gen_rtx_MEM (mode, mr));

  tmp = expand_simple_binop (mode, PLUS, copy_rtx (mem_ref), const1_rtx,
			     mem_ref, 0, OPTAB_WIDEN);

  if (tmp != mem_ref)
    emit_move_insn (copy_rtx (mem_ref), tmp);

  sequence = get_insns ();
  end_sequence ();
  rebuild_jump_labels (sequence);
  return sequence;
}

/* Output instructions as RTL to increment the power of two histogram counter.
   VALUE is the expression whose value is profiled.  TAG is the tag of the
   section for counters, BASE is offset of the counter position.  */

static rtx
gen_pow2_profiler (struct histogram_value *value, unsigned tag, unsigned base)
{
  unsigned gcov_size = tree_low_cst (TYPE_SIZE (GCOV_TYPE_NODE), 1);
  enum machine_mode mode = mode_for_size (gcov_size, MODE_INT, 0);
  rtx mem_ref, tmp, mr, uval;
  rtx sequence;
  rtx end_of_code_label = gen_label_rtx ();
  rtx loop_label = gen_label_rtx ();
  int per_counter = gcov_size / BITS_PER_UNIT;

  start_sequence ();

  if (value->seq)
    emit_insn (value->seq);

  mr = gen_reg_rtx (Pmode);
  tmp = coverage_counter_ref (tag, base);
  tmp = force_reg (Pmode, XEXP (tmp, 0));
  emit_move_insn (mr, tmp);

  uval = gen_reg_rtx (value->mode);
  emit_move_insn (uval, copy_rtx (value->value));

  /* Check for non-power of 2.  */
  if (value->hdata.pow2.may_be_other)
    {
      do_compare_rtx_and_jump (copy_rtx (uval), const0_rtx, LE, 0, value->mode,
			       NULL_RTX, NULL_RTX, end_of_code_label);
      tmp = expand_simple_binop (value->mode, PLUS, copy_rtx (uval),
				 constm1_rtx, NULL_RTX, 0, OPTAB_WIDEN);
      tmp = expand_simple_binop (value->mode, AND, copy_rtx (uval), tmp,
				 NULL_RTX, 0, OPTAB_WIDEN);
      do_compare_rtx_and_jump (tmp, const0_rtx, NE, 0, value->mode, NULL_RTX,
			       NULL_RTX, end_of_code_label);
    }

  /* Count log_2(value).  */
  emit_label (loop_label);

  tmp = expand_simple_binop (Pmode, PLUS, copy_rtx (mr), GEN_INT (per_counter), mr, 0, OPTAB_WIDEN);
  if (tmp != mr)
    emit_move_insn (copy_rtx (mr), tmp);

  tmp = expand_simple_binop (value->mode, ASHIFTRT, copy_rtx (uval), const1_rtx,
			     uval, 0, OPTAB_WIDEN);
  if (tmp != uval)
    emit_move_insn (copy_rtx (uval), tmp);

  do_compare_rtx_and_jump (copy_rtx (uval), const0_rtx, NE, 0, value->mode,
			   NULL_RTX, NULL_RTX, loop_label);

  /* Increase the counter.  */
  emit_label (end_of_code_label);

  mem_ref = validize_mem (gen_rtx_MEM (mode, mr));

  tmp = expand_simple_binop (mode, PLUS, copy_rtx (mem_ref), const1_rtx,
			     mem_ref, 0, OPTAB_WIDEN);

  if (tmp != mem_ref)
    emit_move_insn (copy_rtx (mem_ref), tmp);

  sequence = get_insns ();
  end_sequence ();
  rebuild_jump_labels (sequence);
  return sequence;
}

/* Output instructions as RTL for code to find the most common value.
   VALUE is the expression whose value is profiled.  TAG is the tag of the
   section for counters, BASE is offset of the counter position.  */

static rtx
gen_one_value_profiler (struct histogram_value *value, unsigned tag,
			unsigned base)
{
  unsigned gcov_size = tree_low_cst (TYPE_SIZE (GCOV_TYPE_NODE), 1);
  enum machine_mode mode = mode_for_size (gcov_size, MODE_INT, 0);
  rtx stored_value_ref, counter_ref, all_ref, stored_value, counter, all;
  rtx tmp, uval;
  rtx sequence;
  rtx same_label = gen_label_rtx ();
  rtx zero_label = gen_label_rtx ();
  rtx end_of_code_label = gen_label_rtx ();

  start_sequence ();

  if (value->seq)
    emit_insn (value->seq);

  stored_value_ref = coverage_counter_ref (tag, base);
  counter_ref = coverage_counter_ref (tag, base + 1);
  all_ref = coverage_counter_ref (tag, base + 2);
  stored_value = validize_mem (stored_value_ref);
  counter = validize_mem (counter_ref);
  all = validize_mem (all_ref);

  uval = gen_reg_rtx (mode);
  convert_move (uval, copy_rtx (value->value), 0);

  /* Check if the stored value matches.  */
  do_compare_rtx_and_jump (copy_rtx (uval), copy_rtx (stored_value), EQ,
			   0, mode, NULL_RTX, NULL_RTX, same_label);

  /* Does not match; check whether the counter is zero.  */
  do_compare_rtx_and_jump (copy_rtx (counter), const0_rtx, EQ, 0, mode,
			   NULL_RTX, NULL_RTX, zero_label);

  /* The counter is not zero yet.  */
  tmp = expand_simple_binop (mode, PLUS, copy_rtx (counter), constm1_rtx,
			     counter, 0, OPTAB_WIDEN);

  if (tmp != counter)
    emit_move_insn (copy_rtx (counter), tmp);

  emit_jump_insn (gen_jump (end_of_code_label));
  emit_barrier ();

  emit_label (zero_label);
  /* Set new value.  */
  emit_move_insn (copy_rtx (stored_value), copy_rtx (uval));

  emit_label (same_label);
  /* Increase the counter.  */
  tmp = expand_simple_binop (mode, PLUS, copy_rtx (counter), const1_rtx,
			     counter, 0, OPTAB_WIDEN);

  if (tmp != counter)
    emit_move_insn (copy_rtx (counter), tmp);

  emit_label (end_of_code_label);

  /* Increase the counter of all executions; this seems redundant given
     that ve have counts for edges in cfg, but it may happen that some
     optimization will change the counts for the block (either because
     it is unable to update them correctly, or because it will duplicate
     the block or its part).  */
  tmp = expand_simple_binop (mode, PLUS, copy_rtx (all), const1_rtx,
			     all, 0, OPTAB_WIDEN);

  if (tmp != all)
    emit_move_insn (copy_rtx (all), tmp);
  sequence = get_insns ();
  end_sequence ();
  rebuild_jump_labels (sequence);
  return sequence;
}

/* Output instructions as RTL for code to find the most common value of
   a difference between two evaluations of an expression.
   VALUE is the expression whose value is profiled.  TAG is the tag of the
   section for counters, BASE is offset of the counter position.  */

static rtx
gen_const_delta_profiler (struct histogram_value *value, unsigned tag,
			  unsigned base)
{
  struct histogram_value one_value_delta;
  unsigned gcov_size = tree_low_cst (TYPE_SIZE (GCOV_TYPE_NODE), 1);
  enum machine_mode mode = mode_for_size (gcov_size, MODE_INT, 0);
  rtx stored_value_ref, stored_value, tmp, uval;
  rtx sequence;

  start_sequence ();

  if (value->seq)
    emit_insn (value->seq);

  stored_value_ref = coverage_counter_ref (tag, base);
  stored_value = validize_mem (stored_value_ref);

  uval = gen_reg_rtx (mode);
  convert_move (uval, copy_rtx (value->value), 0);
  tmp = expand_simple_binop (mode, MINUS,
			     copy_rtx (uval), copy_rtx (stored_value),
			     NULL_RTX, 0, OPTAB_WIDEN);

  one_value_delta.value = tmp;
  one_value_delta.mode = mode;
  one_value_delta.seq = NULL_RTX;
  one_value_delta.insn = value->insn;
  one_value_delta.type = HIST_TYPE_SINGLE_VALUE;
  emit_insn (gen_one_value_profiler (&one_value_delta, tag, base + 1));

  emit_move_insn (copy_rtx (stored_value), uval);
  sequence = get_insns ();
  end_sequence ();
  rebuild_jump_labels (sequence);
  return sequence;
}
