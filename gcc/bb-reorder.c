/* Basic block reordering routines for the GNU compiler.
   Copyright (C) 2000, 2002, 2003 Free Software Foundation, Inc.

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

/* This (greedy) algorithm constructs traces in several rounds.
   The construction starts from "seeds".  The seed for the first round
   is the entry point of function.  When there are more than one seed
   that one is selected first that has the lowest key in the heap
   (see function bb_to_key).  Then the algorithm repeatedly adds the most
   probable successor to the end of a trace.  Finally it connects the traces.

   There are two parameters: Branch Threshold and Exec Threshold.
   If the edge to a successor of the actual basic block is lower than
   Branch Threshold or the frequency of the successor is lower than
   Exec Threshold the successor will be the seed in one of the next rounds.
   Each round has these parameters lower than the previous one.
   The last round has to have these parameters set to zero
   so that the remaining blocks are picked up.

   The algorithm selects the most probable successor from all unvisited
   successors and successors that have been added to this trace.
   The other successors (that has not been "sent" to the next round) will be
   other seeds for this round and the secondary traces will start in them.
   If the successor has not been visited in this trace it is added to the trace
   (however, there is some heuristic for simple branches).
   If the successor has been visited in this trace the loop has been found.
   If the loop has many iterations the loop is rotated so that the
   source block of the most probable edge going out from the loop
   is the last block of the trace.
   If the loop has few iterations and there is no edge from the last block of
   the loop going out from loop the loop header is duplicated.
   Finally, the construction of the trace is terminated.

   When connecting traces it first checks whether there is an edge from the
   last block of one trace to the first block of another trace.
   When there are still some unconnected traces it checks whether there exists
   a basic block BB such that BB is a successor of the last bb of one trace
   and BB is a predecessor of the first block of another trace. In this case,
   BB is duplicated and the traces are connected through this duplicate.
   The rest of traces are simply connected so there will be a jump to the
   beginning of the rest of trace.


   References:

   "Software Trace Cache"
   A. Ramirez, J. Larriba-Pey, C. Navarro, J. Torrellas and M. Valero; 1999
   http://citeseer.nj.nec.com/15361.html

*/

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "basic-block.h"
#include "flags.h"
#include "timevar.h"
#include "output.h"
#include "cfglayout.h"
#include "fibheap.h"
#include "target.h"

/* The number of rounds.  */
#define N_ROUNDS 4

/* Branch thresholds in thousandths (per mille) of the REG_BR_PROB_BASE.  */
static int branch_threshold[N_ROUNDS] = {400, 200, 100, 0};

/* Exec thresholds in thousandths (per mille) of the frequency of bb 0.  */
static int exec_threshold[N_ROUNDS] = {500, 200, 50, 0};

/* If edge frequency is lower than DUPLICATION_THRESHOLD per mille of entry
   block the edge destination is not duplicated while connecting traces.  */
#define DUPLICATION_THRESHOLD 100

/* Length of unconditional jump instruction.  */
static int uncond_jump_length;

/* Structure to hold needed information for each basic block.  */
typedef struct bbro_basic_block_data_def
{
  /* Which trace is the bb start of (-1 means it is not a start of a trace).  */
  int start_of_trace;

  /* Which trace is the bb end of (-1 means it is not an end of a trace).  */
  int end_of_trace;

  /* Which heap is BB in (if any)?  */
  fibheap_t heap;

  /* Which heap node is BB in (if any)?  */
  fibnode_t node;
} bbro_basic_block_data;

/* The current size of the following dynamic array.  */
static int array_size;

/* The array which holds needed information for basic blocks.  */
static bbro_basic_block_data *bbd;

/* To avoid frequent reallocation the size of arrays is greater than needed,
   the number of elements is (not less than) 1.25 * size_wanted.  */
#define GET_ARRAY_SIZE(X) ((((X) / 4) + 1) * 5)

/* Free the memory and set the pointer to NULL.  */
#define FREE(P) \
  do { if (P) { free (P); P = 0; } else { abort (); } } while (0)

/* Structure for holding information about a trace.  */
struct trace
{
  /* First and last basic block of the trace.  */
  basic_block first, last;

  /* The round of the STC creation which this trace was found in.  */
  int round;

  /* The length (i.e. the number of basic blocks) of the trace.  */
  int length;
};

/* Maximum frequency and count of one of the entry blocks.  */
int max_entry_frequency;
gcov_type max_entry_count;

/* Local function prototypes.  */
static void find_traces (int *, struct trace *);
static basic_block rotate_loop (edge, struct trace *, int);
static void mark_bb_visited (basic_block, int);
static void find_traces_1_round (int, int, gcov_type, struct trace *, int *,
				 int, fibheap_t *);
static basic_block copy_bb (basic_block, edge, basic_block, int);
static fibheapkey_t bb_to_key (basic_block);
static bool better_edge_p (basic_block, edge, int, int, int, int);
static void connect_traces (int, struct trace *);
static bool copy_bb_p (basic_block, int);
static int get_uncond_jump_length (void);

/* Find the traces for Software Trace Cache.  Chain each trace through
   RBI()->next.  Store the number of traces to N_TRACES and description of
   traces to TRACES.  */

static void
find_traces (int *n_traces, struct trace *traces)
{
  int i;
  edge e;
  fibheap_t heap;

  /* Insert entry points of function into heap.  */
  heap = fibheap_new ();
  max_entry_frequency = 0;
  max_entry_count = 0;
  for (e = ENTRY_BLOCK_PTR->succ; e; e = e->succ_next)
    {
      bbd[e->dest->index].heap = heap;
      bbd[e->dest->index].node = fibheap_insert (heap, bb_to_key (e->dest),
						    e->dest);
      if (e->dest->frequency > max_entry_frequency)
	max_entry_frequency = e->dest->frequency;
      if (e->dest->count > max_entry_count)
	max_entry_count = e->dest->count;
    }

  /* Find the traces.  */
  for (i = 0; i < N_ROUNDS; i++)
    {
      gcov_type count_threshold;

      if (rtl_dump_file)
	fprintf (rtl_dump_file, "STC - round %d\n", i + 1);

      if (max_entry_count < INT_MAX / 1000)
	count_threshold = max_entry_count * exec_threshold[i] / 1000;
      else
	count_threshold = max_entry_count / 1000 * exec_threshold[i];

      find_traces_1_round (REG_BR_PROB_BASE * branch_threshold[i] / 1000,
			   max_entry_frequency * exec_threshold[i] / 1000,
			   count_threshold, traces, n_traces, i, &heap);
    }
  fibheap_delete (heap);

  if (rtl_dump_file)
    {
      for (i = 0; i < *n_traces; i++)
	{
	  basic_block bb;
	  fprintf (rtl_dump_file, "Trace %d (round %d):  ", i + 1,
		   traces[i].round + 1);
	  for (bb = traces[i].first; bb != traces[i].last; bb = bb->rbi->next)
	    fprintf (rtl_dump_file, "%d [%d] ", bb->index, bb->frequency);
	  fprintf (rtl_dump_file, "%d [%d]\n", bb->index, bb->frequency);
	}
      fflush (rtl_dump_file);
    }
}

/* Rotate loop whose back edge is BACK_EDGE in the tail of trace TRACE
   (with sequential number TRACE_N).  */

static basic_block
rotate_loop (edge back_edge, struct trace *trace, int trace_n)
{
  basic_block bb;

  /* Information about the best end (end after rotation) of the loop.  */
  basic_block best_bb = NULL;
  edge best_edge = NULL;
  int best_freq = -1;
  gcov_type best_count = -1;
  /* The best edge is preferred when its destination is not visited yet
     or is a start block of some trace.  */
  bool is_preferred = false;

  /* Find the most frequent edge that goes out from current trace.  */
  bb = back_edge->dest;
  do
    {
      edge e;
      for (e = bb->succ; e; e = e->succ_next)
	if (e->dest != EXIT_BLOCK_PTR
	    && e->dest->rbi->visited != trace_n
	    && (e->flags & EDGE_CAN_FALLTHRU)
	    && !(e->flags & EDGE_COMPLEX))
	{
	  if (is_preferred)
	    {
	      /* The best edge is preferred.  */
	      if (!e->dest->rbi->visited
		  || bbd[e->dest->index].start_of_trace >= 0)
		{
		  /* The current edge E is also preferred.  */
		  int freq = EDGE_FREQUENCY (e);
		  if (freq > best_freq || e->count > best_count)
		    {
		      best_freq = freq;
		      best_count = e->count;
		      best_edge = e;
		      best_bb = bb;
		    }
		}
	    }
	  else
	    {
	      if (!e->dest->rbi->visited
		  || bbd[e->dest->index].start_of_trace >= 0)
		{
		  /* The current edge E is preferred.  */
		  is_preferred = true;
		  best_freq = EDGE_FREQUENCY (e);
		  best_count = e->count;
		  best_edge = e;
		  best_bb = bb;
		}
	      else
		{
		  int freq = EDGE_FREQUENCY (e);
		  if (!best_edge || freq > best_freq || e->count > best_count)
		    {
		      best_freq = freq;
		      best_count = e->count;
		      best_edge = e;
		      best_bb = bb;
		    }
		}
	    }
	}
      bb = bb->rbi->next;
    }
  while (bb != back_edge->dest);

  if (best_bb)
    {
      /* Rotate the loop so that the BEST_EDGE goes out from the last block of
	 the trace.  */
      if (back_edge->dest == trace->first)
	{
	  trace->first = best_bb->rbi->next;
	}
      else
	{
	  basic_block prev_bb;

	  for (prev_bb = trace->first;
	       prev_bb->rbi->next != back_edge->dest;
	       prev_bb = prev_bb->rbi->next)
	    ;
	  prev_bb->rbi->next = best_bb->rbi->next;

	  /* Try to get rid of uncond jump to cond jump.  */
	  if (prev_bb->succ && !prev_bb->succ->succ_next)
	    {
	      basic_block header = prev_bb->succ->dest;

	      /* Duplicate HEADER if it is a small block containing cond jump
		 in the end.  */
	      if (any_condjump_p (BB_END (header)) && copy_bb_p (header, 0))
		{
		  copy_bb (header, prev_bb->succ, prev_bb, trace_n);
		}
	    }
	}
    }
  else
    {
      /* We have not found suitable loop tail so do no rotation.  */
      best_bb = back_edge->src;
    }
  best_bb->rbi->next = NULL;
  return best_bb;
}

/* This function marks BB that it was visited in trace number TRACE.  */

static void
mark_bb_visited (basic_block bb, int trace)
{
  bb->rbi->visited = trace;
  if (bbd[bb->index].heap)
    {
      fibheap_delete_node (bbd[bb->index].heap, bbd[bb->index].node);
      bbd[bb->index].heap = NULL;
      bbd[bb->index].node = NULL;
    }
}

/* One round of finding traces. Find traces for BRANCH_TH and EXEC_TH i.e. do
   not include basic blocks their probability is lower than BRANCH_TH or their
   frequency is lower than EXEC_TH into traces (or count is lower than
   COUNT_TH).  It stores the new traces into TRACES and modifies the number of
   traces *N_TRACES. Sets the round (which the trace belongs to) to ROUND. It
   expects that starting basic blocks are in *HEAP and at the end it deletes
   *HEAP and stores starting points for the next round into new *HEAP.  */

static void
find_traces_1_round (int branch_th, int exec_th, gcov_type count_th,
		     struct trace *traces, int *n_traces, int round,
		     fibheap_t *heap)
{
  /* Heap for discarded basic blocks which are possible starting points for
     the next round.  */
  fibheap_t new_heap = fibheap_new ();

  while (!fibheap_empty (*heap))
    {
      basic_block bb;
      struct trace *trace;
      edge best_edge, e;
      fibheapkey_t key;

      bb = fibheap_extract_min (*heap);
      bbd[bb->index].heap = NULL;
      bbd[bb->index].node = NULL;

      if (rtl_dump_file)
	fprintf (rtl_dump_file, "Getting bb %d\n", bb->index);

      /* If the BB's frequency is too low send BB to the next round.  */
      if (round < N_ROUNDS - 1
	  && (bb->frequency < exec_th || bb->count < count_th
	      || probably_never_executed_bb_p (bb)))
	{
	  int key = bb_to_key (bb);
	  bbd[bb->index].heap = new_heap;
	  bbd[bb->index].node = fibheap_insert (new_heap, key, bb);

	  if (rtl_dump_file)
	    fprintf (rtl_dump_file,
		     "  Possible start point of next round: %d (key: %d)\n",
		     bb->index, key);
	  continue;
	}

      trace = traces + *n_traces;
      trace->first = bb;
      trace->round = round;
      trace->length = 0;
      (*n_traces)++;

      do
	{
	  int prob, freq;

	  /* The probability and frequency of the best edge.  */
	  int best_prob = INT_MIN / 2;
	  int best_freq = INT_MIN / 2;

	  best_edge = NULL;
	  mark_bb_visited (bb, *n_traces);
	  trace->length++;

	  if (rtl_dump_file)
	    fprintf (rtl_dump_file, "Basic block %d was visited in trace %d\n",
		     bb->index, *n_traces - 1);

	  /* Select the successor that will be placed after BB.  */
	  for (e = bb->succ; e; e = e->succ_next)
	    {
#ifdef ENABLE_CHECKING
	      if (e->flags & EDGE_FAKE)
		abort ();
#endif

	      if (e->dest == EXIT_BLOCK_PTR)
		continue;

	      if (e->dest->rbi->visited
		  && e->dest->rbi->visited != *n_traces)
		continue;

	      prob = e->probability;
	      freq = EDGE_FREQUENCY (e);

	      /* Edge that cannot be fallthru or improbable or infrequent
		 successor (ie. it is unsuitable successor).  */
	      if (!(e->flags & EDGE_CAN_FALLTHRU) || (e->flags & EDGE_COMPLEX)
		  || prob < branch_th || freq < exec_th || e->count < count_th)
		continue;

	      if (better_edge_p (bb, e, prob, freq, best_prob, best_freq))
		{
		  best_edge = e;
		  best_prob = prob;
		  best_freq = freq;
		}
	    }

	  /* If the best destination has multiple predecessors, and can be
	     duplicated cheaper than a jump, don't allow it to be added
	     to a trace.  We'll duplicate it when connecting traces.  */
	  if (best_edge && best_edge->dest->pred->pred_next
	      && copy_bb_p (best_edge->dest, 0))
	    best_edge = NULL;

	  /* Add all non-selected successors to the heaps.  */
	  for (e = bb->succ; e; e = e->succ_next)
	    {
	      if (e == best_edge
		  || e->dest == EXIT_BLOCK_PTR
		  || e->dest->rbi->visited)
		continue;

	      key = bb_to_key (e->dest);

	      if (bbd[e->dest->index].heap)
		{
		  /* E->DEST is already in some heap.  */
		  if (key != bbd[e->dest->index].node->key)
		    {
		      if (rtl_dump_file)
			{
			  fprintf (rtl_dump_file,
				   "Changing key for bb %d from %ld to %ld.\n",
				   e->dest->index,
				   (long) bbd[e->dest->index].node->key,
				   key);
			}
		      fibheap_replace_key (bbd[e->dest->index].heap,
					   bbd[e->dest->index].node, key);
		    }
		}
	      else
		{
		  fibheap_t which_heap = *heap;

		  prob = e->probability;
		  freq = EDGE_FREQUENCY (e);

		  if (!(e->flags & EDGE_CAN_FALLTHRU)
		      || (e->flags & EDGE_COMPLEX)
		      || prob < branch_th || freq < exec_th
		      || e->count < count_th)
		    {
		      if (round < N_ROUNDS - 1)
			which_heap = new_heap;
		    }

		  bbd[e->dest->index].heap = which_heap;
		  bbd[e->dest->index].node = fibheap_insert (which_heap,
								key, e->dest);

		  if (rtl_dump_file)
		    {
		      fprintf (rtl_dump_file,
			       "  Possible start of %s round: %d (key: %ld)\n",
			       (which_heap == new_heap) ? "next" : "this",
			       e->dest->index, (long) key);
		    }

		}
	    }

	  if (best_edge) /* Suitable successor was found.  */
	    {
	      if (best_edge->dest->rbi->visited == *n_traces)
		{
		  /* We do nothing with one basic block loops.  */
		  if (best_edge->dest != bb)
		    {
		      if (EDGE_FREQUENCY (best_edge)
			  > 4 * best_edge->dest->frequency / 5)
			{
			  /* The loop has at least 4 iterations.  If the loop
			     header is not the first block of the function
			     we can rotate the loop.  */

			  if (best_edge->dest != ENTRY_BLOCK_PTR->next_bb)
			    {
			      if (rtl_dump_file)
				{
				  fprintf (rtl_dump_file,
					   "Rotating loop %d - %d\n",
					   best_edge->dest->index, bb->index);
				}
			      bb->rbi->next = best_edge->dest;
			      bb = rotate_loop (best_edge, trace, *n_traces);
			    }
			}
		      else
			{
			  /* The loop has less than 4 iterations.  */

			  /* Check whether there is another edge from BB.  */
			  edge another_edge;
			  for (another_edge = bb->succ;
			       another_edge;
			       another_edge = another_edge->succ_next)
			    if (another_edge != best_edge)
			      break;

			  if (!another_edge && copy_bb_p (best_edge->dest,
							  !optimize_size))
			    {
			      bb = copy_bb (best_edge->dest, best_edge, bb,
					    *n_traces);
			    }
			}
		    }

		  /* Terminate the trace.  */
		  break;
		}
	      else
		{
		  /* Check for a situation

		    A
		   /|
		  B |
		   \|
		    C

		  where
		  EDGE_FREQUENCY (AB) + EDGE_FREQUENCY (BC)
		    >= EDGE_FREQUENCY (AC).
		  (i.e. 2 * B->frequency >= EDGE_FREQUENCY (AC) )
		  Best ordering is then A B C.

		  This situation is created for example by:

		  if (A) B;
		  C;

		  */

		  for (e = bb->succ; e; e = e->succ_next)
		    if (e != best_edge
			&& (e->flags & EDGE_CAN_FALLTHRU)
			&& !(e->flags & EDGE_COMPLEX)
			&& !e->dest->rbi->visited
			&& !e->dest->pred->pred_next
			&& e->dest->succ
			&& (e->dest->succ->flags & EDGE_CAN_FALLTHRU)
			&& !(e->dest->succ->flags & EDGE_COMPLEX)
			&& !e->dest->succ->succ_next
			&& e->dest->succ->dest == best_edge->dest
			&& 2 * e->dest->frequency >= EDGE_FREQUENCY (best_edge))
		      {
			best_edge = e;
			if (rtl_dump_file)
			  fprintf (rtl_dump_file, "Selecting BB %d\n",
				   best_edge->dest->index);
			break;
		      }

		  bb->rbi->next = best_edge->dest;
		  bb = best_edge->dest;
		}
	    }
	}
      while (best_edge);
      trace->last = bb;
      bbd[trace->first->index].start_of_trace = *n_traces - 1;
      bbd[trace->last->index].end_of_trace = *n_traces - 1;

      /* The trace is terminated so we have to recount the keys in heap
	 (some block can have a lower key because now one of its predecessors
	 is an end of the trace).  */
      for (e = bb->succ; e; e = e->succ_next)
	{
	  if (e->dest == EXIT_BLOCK_PTR
	      || e->dest->rbi->visited)
	    continue;

	  if (bbd[e->dest->index].heap)
	    {
	      key = bb_to_key (e->dest);
	      if (key != bbd[e->dest->index].node->key)
		{
		  if (rtl_dump_file)
		    {
		      fprintf (rtl_dump_file,
			       "Changing key for bb %d from %ld to %ld.\n",
			       e->dest->index,
			       (long) bbd[e->dest->index].node->key, key);
		    }
		  fibheap_replace_key (bbd[e->dest->index].heap,
				       bbd[e->dest->index].node,
				       key);
		}
	    }
	}
    }

  fibheap_delete (*heap);

  /* "Return" the new heap.  */
  *heap = new_heap;
}

/* Create a duplicate of the basic block OLD_BB and redirect edge E to it, add
   it to trace after BB, mark OLD_BB visited and update pass' data structures
   (TRACE is a number of trace which OLD_BB is duplicated to).  */

static basic_block
copy_bb (basic_block old_bb, edge e, basic_block bb, int trace)
{
  basic_block new_bb;

  new_bb = cfg_layout_duplicate_bb (old_bb, e);
  if (e->dest != new_bb)
    abort ();
  if (e->dest->rbi->visited)
    abort ();
  if (rtl_dump_file)
    fprintf (rtl_dump_file,
	     "Duplicated bb %d (created bb %d)\n",
	     old_bb->index, new_bb->index);
  new_bb->rbi->visited = trace;
  new_bb->rbi->next = bb->rbi->next;
  bb->rbi->next = new_bb;

  if (new_bb->index >= array_size || last_basic_block > array_size)
    {
      int i;
      int new_size;

      new_size = MAX (last_basic_block, new_bb->index + 1);
      new_size = GET_ARRAY_SIZE (new_size);
      bbd = xrealloc (bbd, new_size * sizeof (bbro_basic_block_data));
      for (i = array_size; i < new_size; i++)
	{
	  bbd[i].start_of_trace = -1;
	  bbd[i].end_of_trace = -1;
	  bbd[i].heap = NULL;
	  bbd[i].node = NULL;
	}
      array_size = new_size;

      if (rtl_dump_file)
	{
	  fprintf (rtl_dump_file,
		   "Growing the dynamic array to %d elements.\n",
		   array_size);
	}
    }

  return new_bb;
}

/* Compute and return the key (for the heap) of the basic block BB.  */

static fibheapkey_t
bb_to_key (basic_block bb)
{
  edge e;

  int priority = 0;

  /* Do not start in probably never executed blocks.  */
  if (probably_never_executed_bb_p (bb))
    return BB_FREQ_MAX;

  /* Prefer blocks whose predecessor is an end of some trace
     or whose predecessor edge is EDGE_DFS_BACK.  */
  for (e = bb->pred; e; e = e->pred_next)
    {
      if ((e->src != ENTRY_BLOCK_PTR && bbd[e->src->index].end_of_trace >= 0)
	  || (e->flags & EDGE_DFS_BACK))
	{
	  int edge_freq = EDGE_FREQUENCY (e);

	  if (edge_freq > priority)
	    priority = edge_freq;
	}
    }

  if (priority)
    /* The block with priority should have significantly lower key.  */
    return -(100 * BB_FREQ_MAX + 100 * priority + bb->frequency);
  return -bb->frequency;
}

/* Return true when the edge E from basic block BB is better than the temporary
   best edge (details are in function).  The probability of edge E is PROB. The
   frequency of the successor is FREQ.  The current best probability is
   BEST_PROB, the best frequency is BEST_FREQ.
   The edge is considered to be equivalent when PROB does not differ much from
   BEST_PROB; similarly for frequency.  */

static bool
better_edge_p (basic_block bb, edge e, int prob, int freq, int best_prob,
	       int best_freq)
{
  bool is_better_edge;

  /* The BEST_* values do not have to be best, but can be a bit smaller than
     maximum values.  */
  int diff_prob = best_prob / 10;
  int diff_freq = best_freq / 10;

  if (prob > best_prob + diff_prob)
    /* The edge has higher probability than the temporary best edge.  */
    is_better_edge = true;
  else if (prob < best_prob - diff_prob)
    /* The edge has lower probability than the temporary best edge.  */
    is_better_edge = false;
  else if (freq < best_freq - diff_freq)
    /* The edge and the temporary best edge  have almost equivalent
       probabilities.  The higher frequency of a successor now means
       that there is another edge going into that successor.
       This successor has lower frequency so it is better.  */
    is_better_edge = true;
  else if (freq > best_freq + diff_freq)
    /* This successor has higher frequency so it is worse.  */
    is_better_edge = false;
  else if (e->dest->prev_bb == bb)
    /* The edges have equivalent probabilities and the successors
       have equivalent frequencies.  Select the previous successor.  */
    is_better_edge = true;
  else
    is_better_edge = false;

  return is_better_edge;
}

/* Connect traces in array TRACES, N_TRACES is the count of traces.  */

static void
connect_traces (int n_traces, struct trace *traces)
{
  int i;
  bool *connected;
  int last_trace;
  int freq_threshold;
  gcov_type count_threshold;

  freq_threshold = max_entry_frequency * DUPLICATION_THRESHOLD / 1000;
  if (max_entry_count < INT_MAX / 1000)
    count_threshold = max_entry_count * DUPLICATION_THRESHOLD / 1000;
  else
    count_threshold = max_entry_count / 1000 * DUPLICATION_THRESHOLD;

  connected = xcalloc (n_traces, sizeof (bool));
  last_trace = -1;
  for (i = 0; i < n_traces; i++)
    {
      int t = i;
      int t2;
      edge e, best;
      int best_len;

      if (connected[t])
	continue;

      connected[t] = true;

      /* Find the predecessor traces.  */
      for (t2 = t; t2 > 0;)
	{
	  best = NULL;
	  best_len = 0;
	  for (e = traces[t2].first->pred; e; e = e->pred_next)
	    {
	      int si = e->src->index;

	      if (e->src != ENTRY_BLOCK_PTR
		  && (e->flags & EDGE_CAN_FALLTHRU)
		  && !(e->flags & EDGE_COMPLEX)
		  && bbd[si].end_of_trace >= 0
		  && !connected[bbd[si].end_of_trace]
		  && (!best
		      || e->probability > best->probability
		      || (e->probability == best->probability
			  && traces[bbd[si].end_of_trace].length > best_len)))
		{
		  best = e;
		  best_len = traces[bbd[si].end_of_trace].length;
		}
	    }
	  if (best)
	    {
	      best->src->rbi->next = best->dest;
	      t2 = bbd[best->src->index].end_of_trace;
	      connected[t2] = true;
	      if (rtl_dump_file)
		{
		  fprintf (rtl_dump_file, "Connection: %d %d\n",
			   best->src->index, best->dest->index);
		}
	    }
	  else
	    break;
	}

      if (last_trace >= 0)
	traces[last_trace].last->rbi->next = traces[t2].first;
      last_trace = t;

      /* Find the successor traces.  */
      while (1)
	{
	  /* Find the continuation of the chain.  */
	  best = NULL;
	  best_len = 0;
	  for (e = traces[t].last->succ; e; e = e->succ_next)
	    {
	      int di = e->dest->index;

	      if (e->dest != EXIT_BLOCK_PTR
		  && (e->flags & EDGE_CAN_FALLTHRU)
		  && !(e->flags & EDGE_COMPLEX)
		  && bbd[di].start_of_trace >= 0
		  && !connected[bbd[di].start_of_trace]
		  && (!best
		      || e->probability > best->probability
		      || (e->probability == best->probability
			  && traces[bbd[di].start_of_trace].length > best_len)))
		{
		  best = e;
		  best_len = traces[bbd[di].start_of_trace].length;
		}
	    }

	  if (best)
	    {
	      if (rtl_dump_file)
		{
		  fprintf (rtl_dump_file, "Connection: %d %d\n",
			   best->src->index, best->dest->index);
		}
	      t = bbd[best->dest->index].start_of_trace;
	      traces[last_trace].last->rbi->next = traces[t].first;
	      connected[t] = true;
	      last_trace = t;
	    }
	  else
	    {
	      /* Try to connect the traces by duplication of 1 block.  */
	      edge e2;
	      basic_block next_bb = NULL;
	      bool try_copy = false;

	      for (e = traces[t].last->succ; e; e = e->succ_next)
		if (e->dest != EXIT_BLOCK_PTR
		    && (e->flags & EDGE_CAN_FALLTHRU)
		    && !(e->flags & EDGE_COMPLEX)
		    && (!best || e->probability > best->probability))
		  {
		    edge best2 = NULL;
		    int best2_len = 0;

		    /* If the destination is a start of a trace which is only
		       one block long, then no need to search the successor
		       blocks of the trace.  Accept it.  */
		    if (bbd[e->dest->index].start_of_trace >= 0
			&& traces[bbd[e->dest->index].start_of_trace].length
			   == 1)
		      {
			best = e;
			try_copy = true;
			continue;
		      }

		    for (e2 = e->dest->succ; e2; e2 = e2->succ_next)
		      {
			int di = e2->dest->index;

			if (e2->dest == EXIT_BLOCK_PTR
			    || ((e2->flags & EDGE_CAN_FALLTHRU)
				&& !(e2->flags & EDGE_COMPLEX)
				&& bbd[di].start_of_trace >= 0
				&& !connected[bbd[di].start_of_trace]
				&& (EDGE_FREQUENCY (e2) >= freq_threshold)
				&& (e2->count >= count_threshold)
				&& (!best2
				    || e2->probability > best2->probability
				    || (e2->probability == best2->probability
					&& traces[bbd[di].start_of_trace].length
					   > best2_len))))
			  {
			    best = e;
			    best2 = e2;
			    if (e2->dest != EXIT_BLOCK_PTR)
			      best2_len = traces[bbd[di].start_of_trace].length;
			    else
			      best2_len = INT_MAX;
			    next_bb = e2->dest;
			    try_copy = true;
			  }
		      }
		  }

	      /* Copy tiny blocks always; copy larger blocks only when the
		 edge is traversed frequently enough.  */
	      if (try_copy
		  && copy_bb_p (best->dest,
				!optimize_size
				&& EDGE_FREQUENCY (best) >= freq_threshold
				&& best->count >= count_threshold))
		{
		  basic_block new_bb;

		  if (rtl_dump_file)
		    {
		      fprintf (rtl_dump_file, "Connection: %d %d ",
			       traces[t].last->index, best->dest->index);
		      if (!next_bb)
			fputc ('\n', rtl_dump_file);
		      else if (next_bb == EXIT_BLOCK_PTR)
			fprintf (rtl_dump_file, "exit\n");
		      else
			fprintf (rtl_dump_file, "%d\n", next_bb->index);
		    }

		  new_bb = copy_bb (best->dest, best, traces[t].last, t);
		  traces[t].last = new_bb;
		  if (next_bb && next_bb != EXIT_BLOCK_PTR)
		    {
		      t = bbd[next_bb->index].start_of_trace;
		      traces[last_trace].last->rbi->next = traces[t].first;
		      connected[t] = true;
		      last_trace = t;
		    }
		  else
		    break;	/* Stop finding the successor traces.  */
		}
	      else
		break;	/* Stop finding the successor traces.  */
	    }
	}
    }

  if (rtl_dump_file)
    {
      basic_block bb;

      fprintf (rtl_dump_file, "Final order:\n");
      for (bb = traces[0].first; bb; bb = bb->rbi->next)
	fprintf (rtl_dump_file, "%d ", bb->index);
      fprintf (rtl_dump_file, "\n");
      fflush (rtl_dump_file);
    }

  FREE (connected);
}

/* Return true when BB can and should be copied. CODE_MAY_GROW is true
   when code size is allowed to grow by duplication.  */

static bool
copy_bb_p (basic_block bb, int code_may_grow)
{
  int size = 0;
  int max_size = uncond_jump_length;
  rtx insn;
  int n_succ;
  edge e;

  if (!bb->frequency)
    return false;
  if (!bb->pred || !bb->pred->pred_next)
    return false;
  if (!cfg_layout_can_duplicate_bb_p (bb))
    return false;

  /* Avoid duplicating blocks which have many successors (PR/13430).  */
  n_succ = 0;
  for (e = bb->succ; e; e = e->succ_next)
    {
      n_succ++;
      if (n_succ > 8)
	return false;
    }

  if (code_may_grow && maybe_hot_bb_p (bb))
    max_size *= 8;

  for (insn = BB_HEAD (bb); insn != NEXT_INSN (BB_END (bb));
       insn = NEXT_INSN (insn))
    {
      if (INSN_P (insn))
	size += get_attr_length (insn);
    }

  if (size <= max_size)
    return true;

  if (rtl_dump_file)
    {
      fprintf (rtl_dump_file,
	       "Block %d can't be copied because its size = %d.\n",
	       bb->index, size);
    }

  return false;
}

/* Return the length of unconditional jump instruction.  */

static int
get_uncond_jump_length (void)
{
  rtx label, jump;
  int length;

  label = emit_label_before (gen_label_rtx (), get_insns ());
  jump = emit_jump_insn (gen_jump (label));

  length = get_attr_length (jump);

  delete_insn (jump);
  delete_insn (label);
  return length;
}

/* Reorder basic blocks.  The main entry point to this file.  FLAGS is
   the set of flags to pass to cfg_layout_initialize().  */

void
reorder_basic_blocks (unsigned int flags)
{
  int n_traces;
  int i;
  struct trace *traces;

  if (n_basic_blocks <= 1)
    return;

  if ((* targetm.cannot_modify_jumps_p) ())
    return;

  timevar_push (TV_REORDER_BLOCKS);

  cfg_layout_initialize (flags);

  set_edge_can_fallthru_flag ();
  mark_dfs_back_edges ();

  /* We are estimating the length of uncond jump insn only once since the code
     for getting the insn length always returns the minimal length now.  */
  if (uncond_jump_length == 0)
    uncond_jump_length = get_uncond_jump_length ();

  /* We need to know some information for each basic block.  */
  array_size = GET_ARRAY_SIZE (last_basic_block);
  bbd = xmalloc (array_size * sizeof (bbro_basic_block_data));
  for (i = 0; i < array_size; i++)
    {
      bbd[i].start_of_trace = -1;
      bbd[i].end_of_trace = -1;
      bbd[i].heap = NULL;
      bbd[i].node = NULL;
    }

  traces = xmalloc (n_basic_blocks * sizeof (struct trace));
  n_traces = 0;
  find_traces (&n_traces, traces);
  connect_traces (n_traces, traces);
  FREE (traces);
  FREE (bbd);

  if (rtl_dump_file)
    dump_flow_info (rtl_dump_file);

  cfg_layout_finalize ();

  timevar_pop (TV_REORDER_BLOCKS);
}
