/* Basic block reordering routines for the GNU compiler.
   Copyright (C) 2000-2024 Free Software Foundation, Inc.

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

/* This file contains the "reorder blocks" pass, which changes the control
   flow of a function to encounter fewer branches; the "partition blocks"
   pass, which divides the basic blocks into "hot" and "cold" partitions,
   which are kept separate; and the "duplicate computed gotos" pass, which
   duplicates blocks ending in an indirect jump.

   There are two algorithms for "reorder blocks": the "simple" algorithm,
   which just rearranges blocks, trying to minimize the number of executed
   unconditional branches; and the "software trace cache" algorithm, which
   also copies code, and in general tries a lot harder to have long linear
   pieces of machine code executed.  This algorithm is described next.  */

/* This (greedy) algorithm constructs traces in several rounds.
   The construction starts from "seeds".  The seed for the first round
   is the entry point of the function.  When there are more than one seed,
   the one with the lowest key in the heap is selected first (see bb_to_key).
   Then the algorithm repeatedly adds the most probable successor to the end
   of a trace.  Finally it connects the traces.

   There are two parameters: Branch Threshold and Exec Threshold.
   If the probability of an edge to a successor of the current basic block is
   lower than Branch Threshold or its count is lower than Exec Threshold,
   then the successor will be the seed in one of the next rounds.
   Each round has these parameters lower than the previous one.
   The last round has to have these parameters set to zero so that the
   remaining blocks are picked up.

   The algorithm selects the most probable successor from all unvisited
   successors and successors that have been added to this trace.
   The other successors (that has not been "sent" to the next round) will be
   other seeds for this round and the secondary traces will start from them.
   If the successor has not been visited in this trace, it is added to the
   trace (however, there is some heuristic for simple branches).
   If the successor has been visited in this trace, a loop has been found.
   If the loop has many iterations, the loop is rotated so that the source
   block of the most probable edge going out of the loop is the last block
   of the trace.
   If the loop has few iterations and there is no edge from the last block of
   the loop going out of the loop, the loop header is duplicated.

   When connecting traces, the algorithm first checks whether there is an edge
   from the last block of a trace to the first block of another trace.
   When there are still some unconnected traces it checks whether there exists
   a basic block BB such that BB is a successor of the last block of a trace
   and BB is a predecessor of the first block of another trace.  In this case,
   BB is duplicated, added at the end of the first trace and the traces are
   connected through it.
   The rest of traces are simply connected so there will be a jump to the
   beginning of the rest of traces.

   The above description is for the full algorithm, which is used when the
   function is optimized for speed.  When the function is optimized for size,
   in order to reduce long jumps and connect more fallthru edges, the
   algorithm is modified as follows:
   (1) Break long traces to short ones.  A trace is broken at a block that has
   multiple predecessors/ successors during trace discovery.  When connecting
   traces, only connect Trace n with Trace n + 1.  This change reduces most
   long jumps compared with the above algorithm.
   (2) Ignore the edge probability and count for fallthru edges.
   (3) Keep the original order of blocks when there is no chance to fall
   through.  We rely on the results of cfg_cleanup.

   To implement the change for code size optimization, block's index is
   selected as the key and all traces are found in one round.

   References:

   "Software Trace Cache"
   A. Ramirez, J. Larriba-Pey, C. Navarro, J. Torrellas and M. Valero; 1999
   http://citeseer.nj.nec.com/15361.html

*/

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "cfghooks.h"
#include "df.h"
#include "memmodel.h"
#include "optabs.h"
#include "regs.h"
#include "emit-rtl.h"
#include "output.h"
#include "expr.h"
#include "tree-pass.h"
#include "cfgrtl.h"
#include "cfganal.h"
#include "cfgbuild.h"
#include "cfgcleanup.h"
#include "bb-reorder.h"
#include "except.h"
#include "alloc-pool.h"
#include "fibonacci_heap.h"
#include "stringpool.h"
#include "attribs.h"
#include "common/common-target.h"

/* The number of rounds.  In most cases there will only be 4 rounds, but
   when partitioning hot and cold basic blocks into separate sections of
   the object file there will be an extra round.  */
#define N_ROUNDS 5

struct target_bb_reorder default_target_bb_reorder;
#if SWITCHABLE_TARGET
struct target_bb_reorder *this_target_bb_reorder = &default_target_bb_reorder;
#endif

#define uncond_jump_length \
  (this_target_bb_reorder->x_uncond_jump_length)

/* Branch thresholds in thousandths (per mille) of the REG_BR_PROB_BASE.  */
static const int branch_threshold[N_ROUNDS] = {400, 200, 100, 0, 0};

/* Exec thresholds in thousandths (per mille) of the count of bb 0.  */
static const int exec_threshold[N_ROUNDS] = {500, 200, 50, 0, 0};

/* If edge count is lower than DUPLICATION_THRESHOLD per mille of entry
   block the edge destination is not duplicated while connecting traces.  */
#define DUPLICATION_THRESHOLD 100

typedef fibonacci_heap <long, basic_block_def> bb_heap_t;
typedef fibonacci_node <long, basic_block_def> bb_heap_node_t;

/* Structure to hold needed information for each basic block.  */
struct bbro_basic_block_data
{
  /* Which trace is the bb start of (-1 means it is not a start of any).  */
  int start_of_trace;

  /* Which trace is the bb end of (-1 means it is not an end of any).  */
  int end_of_trace;

  /* Which trace is the bb in?  */
  int in_trace;

  /* Which trace was this bb visited in?  */
  int visited;

  /* Cached maximum frequency of interesting incoming edges.
     Minus one means not yet computed.  */
  int priority;

  /* Which heap is BB in (if any)?  */
  bb_heap_t *heap;

  /* Which heap node is BB in (if any)?  */
  bb_heap_node_t *node;
};

/* The current size of the following dynamic array.  */
static int array_size;

/* The array which holds needed information for basic blocks.  */
static bbro_basic_block_data *bbd;

/* To avoid frequent reallocation the size of arrays is greater than needed,
   the number of elements is (not less than) 1.25 * size_wanted.  */
#define GET_ARRAY_SIZE(X) ((((X) / 4) + 1) * 5)

/* Free the memory and set the pointer to NULL.  */
#define FREE(P) (gcc_assert (P), free (P), P = 0)

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

/* Maximum count of one of the entry blocks.  */
static profile_count max_entry_count;

/* Local function prototypes.  */
static void find_traces_1_round (int, profile_count, struct trace *, int *,
				 int, bb_heap_t **, int);
static basic_block copy_bb (basic_block, edge, basic_block, int);
static long bb_to_key (basic_block);
static bool better_edge_p (const_basic_block, const_edge, profile_probability,
			   profile_count, profile_probability, profile_count,
			   const_edge);
static bool copy_bb_p (const_basic_block, int);

/* Return the trace number in which BB was visited.  */

static int
bb_visited_trace (const_basic_block bb)
{
  gcc_assert (bb->index < array_size);
  return bbd[bb->index].visited;
}

/* This function marks BB that it was visited in trace number TRACE.  */

static void
mark_bb_visited (basic_block bb, int trace)
{
  bbd[bb->index].visited = trace;
  if (bbd[bb->index].heap)
    {
      bbd[bb->index].heap->delete_node (bbd[bb->index].node);
      bbd[bb->index].heap = NULL;
      bbd[bb->index].node = NULL;
    }
}

/* Check to see if bb should be pushed into the next round of trace
   collections or not.  Reasons for pushing the block forward are 1).
   If the block is cold, we are doing partitioning, and there will be
   another round (cold partition blocks are not supposed to be
   collected into traces until the very last round); or 2). There will
   be another round, and the basic block is not "hot enough" for the
   current round of trace collection.  */

static bool
push_to_next_round_p (const_basic_block bb, int round, int number_of_rounds,
		      profile_count count_th)
{
  bool there_exists_another_round;
  bool block_not_hot_enough;

  there_exists_another_round = round < number_of_rounds - 1;

  block_not_hot_enough = (bb->count < count_th
			  || probably_never_executed_bb_p (cfun, bb));

  if (there_exists_another_round
      && block_not_hot_enough)
    return true;
  else
    return false;
}

/* Find the traces for Software Trace Cache.  Chain each trace through
   RBI()->next.  Store the number of traces to N_TRACES and description of
   traces to TRACES.  */

static void
find_traces (int *n_traces, struct trace *traces)
{
  int i;
  int number_of_rounds;
  edge e;
  edge_iterator ei;
  bb_heap_t *heap = new bb_heap_t (LONG_MIN);

  /* Add one extra round of trace collection when partitioning hot/cold
     basic blocks into separate sections.  The last round is for all the
     cold blocks (and ONLY the cold blocks).  */

  number_of_rounds = N_ROUNDS - 1;

  /* Insert entry points of function into heap.  */
  max_entry_count = profile_count::zero ();
  FOR_EACH_EDGE (e, ei, ENTRY_BLOCK_PTR_FOR_FN (cfun)->succs)
    {
      bbd[e->dest->index].heap = heap;
      bbd[e->dest->index].node = heap->insert (bb_to_key (e->dest), e->dest);
      if (e->dest->count > max_entry_count)
	max_entry_count = e->dest->count;
    }

  /* Find the traces.  */
  for (i = 0; i < number_of_rounds; i++)
    {
      profile_count count_threshold;

      if (dump_file)
	fprintf (dump_file, "STC - round %d\n", i + 1);

      count_threshold = max_entry_count.apply_scale (exec_threshold[i], 1000);

      find_traces_1_round (REG_BR_PROB_BASE * branch_threshold[i] / 1000,
			   count_threshold, traces, n_traces, i, &heap,
			   number_of_rounds);
    }
  delete heap;

  if (dump_file)
    {
      for (i = 0; i < *n_traces; i++)
	{
	  basic_block bb;
	  fprintf (dump_file, "Trace %d (round %d):  ", i + 1,
		   traces[i].round + 1);
	  for (bb = traces[i].first;
	       bb != traces[i].last;
	       bb = (basic_block) bb->aux)
	    {
	      fprintf (dump_file, "%d [", bb->index);
	      bb->count.dump (dump_file);
	      fprintf (dump_file, "] ");
	    }
	  fprintf (dump_file, "%d [", bb->index);
	  bb->count.dump (dump_file);
	  fprintf (dump_file, "]\n");
	}
      fflush (dump_file);
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
  profile_count best_count = profile_count::uninitialized ();
  /* The best edge is preferred when its destination is not visited yet
     or is a start block of some trace.  */
  bool is_preferred = false;

  /* Find the most frequent edge that goes out from current trace.  */
  bb = back_edge->dest;
  do
    {
      edge e;
      edge_iterator ei;

      FOR_EACH_EDGE (e, ei, bb->succs)
	if (e->dest != EXIT_BLOCK_PTR_FOR_FN (cfun)
	    && bb_visited_trace (e->dest) != trace_n
	    && (e->flags & EDGE_CAN_FALLTHRU)
	    && !(e->flags & EDGE_COMPLEX))
	{
	  if (is_preferred)
	    {
	      /* The best edge is preferred.  */
	      if (!bb_visited_trace (e->dest)
		  || bbd[e->dest->index].start_of_trace >= 0)
		{
		  /* The current edge E is also preferred.  */
		  if (e->count () > best_count)
		    {
		      best_count = e->count ();
		      best_edge = e;
		      best_bb = bb;
		    }
		}
	    }
	  else
	    {
	      if (!bb_visited_trace (e->dest)
		  || bbd[e->dest->index].start_of_trace >= 0)
		{
		  /* The current edge E is preferred.  */
		  is_preferred = true;
		  best_count = e->count ();
		  best_edge = e;
		  best_bb = bb;
		}
	      else
		{
		  if (!best_edge || e->count () > best_count)
		    {
		      best_count = e->count ();
		      best_edge = e;
		      best_bb = bb;
		    }
		}
	    }
	}
      bb = (basic_block) bb->aux;
    }
  while (bb != back_edge->dest);

  if (best_bb)
    {
      /* Rotate the loop so that the BEST_EDGE goes out from the last block of
	 the trace.  */
      if (back_edge->dest == trace->first)
	{
	  trace->first = (basic_block) best_bb->aux;
	}
      else
	{
	  basic_block prev_bb;

	  for (prev_bb = trace->first;
	       prev_bb->aux != back_edge->dest;
	       prev_bb = (basic_block) prev_bb->aux)
	    ;
	  prev_bb->aux = best_bb->aux;

	  /* Try to get rid of uncond jump to cond jump.  */
	  if (single_succ_p (prev_bb))
	    {
	      basic_block header = single_succ (prev_bb);

	      /* Duplicate HEADER if it is a small block containing cond jump
		 in the end.  */
	      if (any_condjump_p (BB_END (header)) && copy_bb_p (header, 0)
		  && !CROSSING_JUMP_P (BB_END (header)))
		copy_bb (header, single_succ_edge (prev_bb), prev_bb, trace_n);
	    }
	}
    }
  else
    {
      /* We have not found suitable loop tail so do no rotation.  */
      best_bb = back_edge->src;
    }
  best_bb->aux = NULL;
  return best_bb;
}

/* One round of finding traces.  Find traces for BRANCH_TH and EXEC_TH i.e. do
   not include basic blocks whose probability is lower than BRANCH_TH or whose
   count is lower than EXEC_TH into traces (or whose count is lower than
   COUNT_TH).  Store the new traces into TRACES and modify the number of
   traces *N_TRACES.  Set the round (which the trace belongs to) to ROUND.
   The function expects starting basic blocks to be in *HEAP and will delete
   *HEAP and store starting points for the next round into new *HEAP.  */

static void
find_traces_1_round (int branch_th, profile_count count_th,
		     struct trace *traces, int *n_traces, int round,
		     bb_heap_t **heap, int number_of_rounds)
{
  /* Heap for discarded basic blocks which are possible starting points for
     the next round.  */
  bb_heap_t *new_heap = new bb_heap_t (LONG_MIN);
  bool for_size = optimize_function_for_size_p (cfun);

  while (!(*heap)->empty ())
    {
      basic_block bb;
      struct trace *trace;
      edge best_edge, e;
      long key;
      edge_iterator ei;

      bb = (*heap)->extract_min ();
      bbd[bb->index].heap = NULL;
      bbd[bb->index].node = NULL;

      if (dump_file)
	fprintf (dump_file, "Getting bb %d\n", bb->index);

      /* If the BB's count is too low, send BB to the next round.  When
	 partitioning hot/cold blocks into separate sections, make sure all
	 the cold blocks (and ONLY the cold blocks) go into the (extra) final
	 round.  When optimizing for size, do not push to next round.  */

      if (!for_size
	  && push_to_next_round_p (bb, round, number_of_rounds,
				   count_th))
	{
	  int key = bb_to_key (bb);
	  bbd[bb->index].heap = new_heap;
	  bbd[bb->index].node = new_heap->insert (key, bb);

	  if (dump_file)
	    fprintf (dump_file,
		     "  Possible start point of next round: %d (key: %d)\n",
		     bb->index, key);
	  continue;
	}

      trace = traces + *n_traces;
      trace->first = bb;
      trace->round = round;
      trace->length = 0;
      bbd[bb->index].in_trace = *n_traces;
      (*n_traces)++;

      do
	{
	  bool ends_in_call;

	  /* The probability and count of the best edge.  */
	  profile_probability best_prob = profile_probability::uninitialized ();
	  profile_count best_count = profile_count::uninitialized ();

	  best_edge = NULL;
	  mark_bb_visited (bb, *n_traces);
	  trace->length++;

	  if (dump_file)
	    fprintf (dump_file, "Basic block %d was visited in trace %d\n",
		     bb->index, *n_traces);

	  ends_in_call = block_ends_with_call_p (bb);

	  /* Select the successor that will be placed after BB.  */
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    {
	      gcc_assert (!(e->flags & EDGE_FAKE));

	      if (e->dest == EXIT_BLOCK_PTR_FOR_FN (cfun))
		continue;

	      if (bb_visited_trace (e->dest)
		  && bb_visited_trace (e->dest) != *n_traces)
		continue;

	      /* If partitioning hot/cold basic blocks, don't consider edges
		 that cross section boundaries.  */
	      if (BB_PARTITION (e->dest) != BB_PARTITION (bb))
		continue;

	      profile_probability prob = e->probability;
	      profile_count count = e->dest->count;

	      /* The only sensible preference for a call instruction is the
		 fallthru edge.  Don't bother selecting anything else.  */
	      if (ends_in_call)
		{
		  if (e->flags & EDGE_CAN_FALLTHRU)
		    {
		      best_edge = e;
		      best_prob = prob;
		      best_count = count;
		    }
		  continue;
		}

	      /* Edge that cannot be fallthru or improbable or infrequent
		 successor (i.e. it is unsuitable successor).  When optimizing
		 for size, ignore the probability and count.  */
	      if (!(e->flags & EDGE_CAN_FALLTHRU) || (e->flags & EDGE_COMPLEX)
		  || !prob.initialized_p ()
		  || ((prob.to_reg_br_prob_base () < branch_th
		      || e->count () < count_th) && (!for_size)))
		continue;

	      if (better_edge_p (bb, e, prob, count, best_prob, best_count,
				 best_edge))
		{
		  best_edge = e;
		  best_prob = prob;
		  best_count = count;
		}
	    }

	  /* If the best destination has multiple predecessors and can be
	     duplicated cheaper than a jump, don't allow it to be added to
	     a trace; we'll duplicate it when connecting the traces later.
	     However, we need to check that this duplication wouldn't leave
	     the best destination with only crossing predecessors, because
	     this would change its effective partition from hot to cold.  */
	  if (best_edge
	      && EDGE_COUNT (best_edge->dest->preds) >= 2
	      && copy_bb_p (best_edge->dest, 0))
	    {
	      bool only_crossing_preds = true;
	      edge e;
	      edge_iterator ei;
	      FOR_EACH_EDGE (e, ei, best_edge->dest->preds)
		if (e != best_edge && !(e->flags & EDGE_CROSSING))
		  {
		    only_crossing_preds = false;
		    break;
		  }
	      if (!only_crossing_preds)
		best_edge = NULL;
	    }

	  /* If the best destination has multiple successors or predecessors,
	     don't allow it to be added when optimizing for size.  This makes
	     sure predecessors with smaller index are handled before the best
	     destination.  It breaks long trace and reduces long jumps.

	     Take if-then-else as an example.
		A
	       / \
	      B   C
	       \ /
		D
	     If we do not remove the best edge B->D/C->D, the final order might
	     be A B D ... C.  C is at the end of the program.  If D's successors
	     and D are complicated, might need long jumps for A->C and C->D.
	     Similar issue for order: A C D ... B.

	     After removing the best edge, the final result will be ABCD/ ACBD.
	     It does not add jump compared with the previous order.  But it
	     reduces the possibility of long jumps.  */
	  if (best_edge && for_size
	      && (EDGE_COUNT (best_edge->dest->succs) > 1
		 || EDGE_COUNT (best_edge->dest->preds) > 1))
	    best_edge = NULL;

	  /* Add all non-selected successors to the heaps.  */
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    {
	      if (e == best_edge
		  || e->dest == EXIT_BLOCK_PTR_FOR_FN (cfun)
		  || bb_visited_trace (e->dest))
		continue;

	      key = bb_to_key (e->dest);

	      if (bbd[e->dest->index].heap)
		{
		  /* E->DEST is already in some heap.  */
		  if (key != bbd[e->dest->index].node->get_key ())
		    {
		      if (dump_file)
			{
			  fprintf (dump_file,
				   "Changing key for bb %d from %ld to %ld.\n",
				   e->dest->index,
				   (long) bbd[e->dest->index].node->get_key (),
				   key);
			}
		      bbd[e->dest->index].heap->replace_key
		        (bbd[e->dest->index].node, key);
		    }
		}
	      else
		{
		  bb_heap_t *which_heap = *heap;

		  profile_probability prob = e->probability;

		  if (!(e->flags & EDGE_CAN_FALLTHRU)
		      || (e->flags & EDGE_COMPLEX)
		      || !prob.initialized_p ()
		      || prob.to_reg_br_prob_base () < branch_th
		      || e->count () < count_th)
		    {
		      /* When partitioning hot/cold basic blocks, make sure
			 the cold blocks (and only the cold blocks) all get
			 pushed to the last round of trace collection.  When
			 optimizing for size, do not push to next round.  */

		      if (!for_size && push_to_next_round_p (e->dest, round,
							     number_of_rounds,
							     count_th))
			which_heap = new_heap;
		    }

		  bbd[e->dest->index].heap = which_heap;
		  bbd[e->dest->index].node = which_heap->insert (key, e->dest);

		  if (dump_file)
		    {
		      fprintf (dump_file,
			       "  Possible start of %s round: %d (key: %ld)\n",
			       (which_heap == new_heap) ? "next" : "this",
			       e->dest->index, (long) key);
		    }

		}
	    }

	  if (best_edge) /* Suitable successor was found.  */
	    {
	      if (bb_visited_trace (best_edge->dest) == *n_traces)
		{
		  /* We do nothing with one basic block loops.  */
		  if (best_edge->dest != bb)
		    {
		      if (best_edge->count ()
			  > best_edge->dest->count.apply_scale (4, 5))
			{
			  /* The loop has at least 4 iterations.  If the loop
			     header is not the first block of the function
			     we can rotate the loop.  */

			  if (best_edge->dest
			      != ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb)
			    {
			      if (dump_file)
				{
				  fprintf (dump_file,
					   "Rotating loop %d - %d\n",
					   best_edge->dest->index, bb->index);
				}
			      bb->aux = best_edge->dest;
			      bbd[best_edge->dest->index].in_trace =
							     (*n_traces) - 1;
			      bb = rotate_loop (best_edge, trace, *n_traces);
			    }
			}
		      else
			{
			  /* The loop has less than 4 iterations.  */

			  if (single_succ_p (bb)
			      && copy_bb_p (best_edge->dest,
			      		    optimize_edge_for_speed_p
			      		    (best_edge)))
			    {
			      bb = copy_bb (best_edge->dest, best_edge, bb,
					    *n_traces);
			      trace->length++;
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
		  AB->count () + BC->count () >= AC->count ().
		  (i.e. 2 * B->count >= AC->count )
		  Best ordering is then A B C.

		  When optimizing for size, A B C is always the best order.

		  This situation is created for example by:

		  if (A) B;
		  C;

		  */

		  FOR_EACH_EDGE (e, ei, bb->succs)
		    if (e != best_edge
			&& (e->flags & EDGE_CAN_FALLTHRU)
			&& !(e->flags & EDGE_COMPLEX)
			&& !bb_visited_trace (e->dest)
			&& single_pred_p (e->dest)
			&& !(e->flags & EDGE_CROSSING)
			&& single_succ_p (e->dest)
			&& (single_succ_edge (e->dest)->flags
			    & EDGE_CAN_FALLTHRU)
			&& !(single_succ_edge (e->dest)->flags & EDGE_COMPLEX)
			&& single_succ (e->dest) == best_edge->dest
			&& (e->dest->count * 2
			    >= best_edge->count () || for_size))
		      {
			best_edge = e;
			if (dump_file)
			  fprintf (dump_file, "Selecting BB %d\n",
				   best_edge->dest->index);
			break;
		      }

		  bb->aux = best_edge->dest;
		  bbd[best_edge->dest->index].in_trace = (*n_traces) - 1;
		  bb = best_edge->dest;
		}
	    }
	}
      while (best_edge);
      trace->last = bb;
      bbd[trace->first->index].start_of_trace = *n_traces - 1;
      if (bbd[trace->last->index].end_of_trace != *n_traces - 1)
	{
	  bbd[trace->last->index].end_of_trace = *n_traces - 1;
	  /* Update the cached maximum frequency for interesting predecessor
	     edges for successors of the new trace end.  */
	  FOR_EACH_EDGE (e, ei, trace->last->succs)
	    if (EDGE_FREQUENCY (e) > bbd[e->dest->index].priority)
	      bbd[e->dest->index].priority = EDGE_FREQUENCY (e);
	}

      /* The trace is terminated so we have to recount the keys in heap
	 (some block can have a lower key because now one of its predecessors
	 is an end of the trace).  */
      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  if (e->dest == EXIT_BLOCK_PTR_FOR_FN (cfun)
	      || bb_visited_trace (e->dest))
	    continue;

	  if (bbd[e->dest->index].heap)
	    {
	      key = bb_to_key (e->dest);
	      if (key != bbd[e->dest->index].node->get_key ())
		{
		  if (dump_file)
		    {
		      fprintf (dump_file,
			       "Changing key for bb %d from %ld to %ld.\n",
			       e->dest->index,
			       (long) bbd[e->dest->index].node->get_key (), key);
		    }
		  bbd[e->dest->index].heap->replace_key
		    (bbd[e->dest->index].node, key);
		}
	    }
	}
    }

  delete (*heap);

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

  new_bb = duplicate_block (old_bb, e, bb);
  BB_COPY_PARTITION (new_bb, old_bb);

  gcc_assert (e->dest == new_bb);

  if (dump_file)
    fprintf (dump_file,
	     "Duplicated bb %d (created bb %d)\n",
	     old_bb->index, new_bb->index);

  if (new_bb->index >= array_size
      || last_basic_block_for_fn (cfun) > array_size)
    {
      int i;
      int new_size;

      new_size = MAX (last_basic_block_for_fn (cfun), new_bb->index + 1);
      new_size = GET_ARRAY_SIZE (new_size);
      bbd = XRESIZEVEC (bbro_basic_block_data, bbd, new_size);
      for (i = array_size; i < new_size; i++)
	{
	  bbd[i].start_of_trace = -1;
	  bbd[i].end_of_trace = -1;
	  bbd[i].in_trace = -1;
	  bbd[i].visited = 0;
	  bbd[i].priority = -1;
	  bbd[i].heap = NULL;
	  bbd[i].node = NULL;
	}
      array_size = new_size;

      if (dump_file)
	{
	  fprintf (dump_file,
		   "Growing the dynamic array to %d elements.\n",
		   array_size);
	}
    }

  gcc_assert (!bb_visited_trace (e->dest));
  mark_bb_visited (new_bb, trace);
  new_bb->aux = bb->aux;
  bb->aux = new_bb;

  bbd[new_bb->index].in_trace = trace;

  return new_bb;
}

/* Compute and return the key (for the heap) of the basic block BB.  */

static long
bb_to_key (basic_block bb)
{
  edge e;
  edge_iterator ei;

  /* Use index as key to align with its original order.  */
  if (optimize_function_for_size_p (cfun))
    return bb->index;

  /* Do not start in probably never executed blocks.  */

  if (BB_PARTITION (bb) == BB_COLD_PARTITION
      || probably_never_executed_bb_p (cfun, bb))
    return BB_FREQ_MAX;

  /* Prefer blocks whose predecessor is an end of some trace
     or whose predecessor edge is EDGE_DFS_BACK.  */
  int priority = bbd[bb->index].priority;
  if (priority == -1)
    {
      priority = 0;
      FOR_EACH_EDGE (e, ei, bb->preds)
	{
	  if ((e->src != ENTRY_BLOCK_PTR_FOR_FN (cfun)
	       && bbd[e->src->index].end_of_trace >= 0)
	      || (e->flags & EDGE_DFS_BACK))
	    {
	      int edge_freq = EDGE_FREQUENCY (e);

	      if (edge_freq > priority)
		priority = edge_freq;
	    }
	}
      bbd[bb->index].priority = priority;
    }

  if (priority)
    /* The block with priority should have significantly lower key.  */
    return -(100 * BB_FREQ_MAX + 100 * priority + bb->count.to_frequency (cfun));

  return -bb->count.to_frequency (cfun);
}

/* Return true when the edge E from basic block BB is better than the temporary
   best edge (details are in function).  The probability of edge E is PROB. The
   count of the successor is COUNT.  The current best probability is
   BEST_PROB, the best count is BEST_COUNT.
   The edge is considered to be equivalent when PROB does not differ much from
   BEST_PROB; similarly for count.  */

static bool
better_edge_p (const_basic_block bb, const_edge e, profile_probability prob,
	       profile_count count, profile_probability best_prob,
	       profile_count best_count, const_edge cur_best_edge)
{
  bool is_better_edge;

  /* The BEST_* values do not have to be best, but can be a bit smaller than
     maximum values.  */
  profile_probability diff_prob = best_prob / 10;

  /* The smaller one is better to keep the original order.  */
  if (optimize_function_for_size_p (cfun))
    return !cur_best_edge
	   || cur_best_edge->dest->index > e->dest->index;

  /* Those edges are so expensive that continuing a trace is not useful
     performance wise.  */
  if (e->flags & (EDGE_ABNORMAL | EDGE_EH))
    return false;

  if (prob > best_prob + diff_prob
      || (!best_prob.initialized_p ()
	  && prob > profile_probability::guessed_never ()))
    /* The edge has higher probability than the temporary best edge.  */
    is_better_edge = true;
  else if (prob < best_prob - diff_prob)
    /* The edge has lower probability than the temporary best edge.  */
    is_better_edge = false;
  else
    {
      profile_count diff_count = best_count / 10;
      if (count < best_count - diff_count
	  || (!best_count.initialized_p ()
	      && count.nonzero_p ()))
	/* The edge and the temporary best edge  have almost equivalent
	   probabilities.  The higher countuency of a successor now means
	   that there is another edge going into that successor.
	   This successor has lower countuency so it is better.  */
	is_better_edge = true;
      else if (count > best_count + diff_count)
	/* This successor has higher countuency so it is worse.  */
	is_better_edge = false;
      else if (e->dest->prev_bb == bb)
	/* The edges have equivalent probabilities and the successors
	   have equivalent frequencies.  Select the previous successor.  */
	is_better_edge = true;
      else
	is_better_edge = false;
    }

  return is_better_edge;
}

/* Return true when the edge E is better than the temporary best edge
   CUR_BEST_EDGE.  If SRC_INDEX_P is true, the function compares the src bb of
   E and CUR_BEST_EDGE; otherwise it will compare the dest bb.
   BEST_LEN is the trace length of src (or dest) bb in CUR_BEST_EDGE.
   TRACES record the information about traces.
   When optimizing for size, the edge with smaller index is better.
   When optimizing for speed, the edge with bigger probability or longer trace
   is better.  */

static bool
connect_better_edge_p (const_edge e, bool src_index_p, int best_len,
		       const_edge cur_best_edge, struct trace *traces)
{
  int e_index;
  int b_index;
  bool is_better_edge;

  if (!cur_best_edge)
    return true;

  if (optimize_function_for_size_p (cfun))
    {
      e_index = src_index_p ? e->src->index : e->dest->index;
      b_index = src_index_p ? cur_best_edge->src->index
			      : cur_best_edge->dest->index;
      /* The smaller one is better to keep the original order.  */
      return b_index > e_index;
    }

  if (src_index_p)
    {
      e_index = e->src->index;

      /* We are looking for predecessor, so probabilities are not that
	 informative.  We do not want to connect A to B because A has
	 only one successor (probability is 100%) while there is edge
	 A' to B where probability is 90% but which is much more frequent.  */
      if (e->count () > cur_best_edge->count ())
	/* The edge has higher probability than the temporary best edge.  */
	is_better_edge = true;
      else if (e->count () < cur_best_edge->count ())
	/* The edge has lower probability than the temporary best edge.  */
	is_better_edge = false;
      else if (e->probability > cur_best_edge->probability)
	/* The edge has higher probability than the temporary best edge.  */
	is_better_edge = true;
      else if (e->probability < cur_best_edge->probability)
	/* The edge has lower probability than the temporary best edge.  */
	is_better_edge = false;
      else if (traces[bbd[e_index].end_of_trace].length > best_len)
	/* The edge and the temporary best edge have equivalent probabilities.
	   The edge with longer trace is better.  */
	is_better_edge = true;
      else
	is_better_edge = false;
    }
  else
    {
      e_index = e->dest->index;

      if (e->probability > cur_best_edge->probability)
	/* The edge has higher probability than the temporary best edge.  */
	is_better_edge = true;
      else if (e->probability < cur_best_edge->probability)
	/* The edge has lower probability than the temporary best edge.  */
	is_better_edge = false;
      else if (traces[bbd[e_index].start_of_trace].length > best_len)
	/* The edge and the temporary best edge have equivalent probabilities.
	   The edge with longer trace is better.  */
	is_better_edge = true;
      else
	is_better_edge = false;
    }

  return is_better_edge;
}

/* Connect traces in array TRACES, N_TRACES is the count of traces.  */

static void
connect_traces (int n_traces, struct trace *traces)
{
  int i;
  bool *connected;
  bool two_passes;
  int last_trace;
  int current_pass;
  int current_partition;
  profile_count count_threshold;
  bool for_size = optimize_function_for_size_p (cfun);

  count_threshold = max_entry_count.apply_scale (DUPLICATION_THRESHOLD, 1000);

  connected = XCNEWVEC (bool, n_traces);
  last_trace = -1;
  current_pass = 1;
  current_partition = BB_PARTITION (traces[0].first);
  two_passes = false;

  if (crtl->has_bb_partition)
    for (i = 0; i < n_traces && !two_passes; i++)
      if (BB_PARTITION (traces[0].first)
	  != BB_PARTITION (traces[i].first))
	two_passes = true;

  for (i = 0; i < n_traces || (two_passes && current_pass == 1) ; i++)
    {
      int t = i;
      int t2;
      edge e, best;
      int best_len;

      if (i >= n_traces)
	{
	  gcc_assert (two_passes && current_pass == 1);
	  i = 0;
	  t = i;
	  current_pass = 2;
	  if (current_partition == BB_HOT_PARTITION)
	    current_partition = BB_COLD_PARTITION;
	  else
	    current_partition = BB_HOT_PARTITION;
	}

      if (connected[t])
	continue;

      if (two_passes
	  && BB_PARTITION (traces[t].first) != current_partition)
	continue;

      connected[t] = true;

      /* Find the predecessor traces.  */
      for (t2 = t; t2 > 0;)
	{
	  edge_iterator ei;
	  best = NULL;
	  best_len = 0;
	  FOR_EACH_EDGE (e, ei, traces[t2].first->preds)
	    {
	      int si = e->src->index;

	      if (e->src != ENTRY_BLOCK_PTR_FOR_FN (cfun)
		  && (e->flags & EDGE_CAN_FALLTHRU)
		  && !(e->flags & EDGE_COMPLEX)
		  && bbd[si].end_of_trace >= 0
		  && !connected[bbd[si].end_of_trace]
		  && (BB_PARTITION (e->src) == current_partition)
		  && connect_better_edge_p (e, true, best_len, best, traces))
		{
		  best = e;
		  best_len = traces[bbd[si].end_of_trace].length;
		}
	    }
	  if (best)
	    {
	      best->src->aux = best->dest;
	      t2 = bbd[best->src->index].end_of_trace;
	      connected[t2] = true;

	      if (dump_file)
		{
		  fprintf (dump_file, "Connection: %d %d\n",
			   best->src->index, best->dest->index);
		}
	    }
	  else
	    break;
	}

      if (last_trace >= 0)
	traces[last_trace].last->aux = traces[t2].first;
      last_trace = t;

      /* Find the successor traces.  */
      while (1)
	{
	  /* Find the continuation of the chain.  */
	  edge_iterator ei;
	  best = NULL;
	  best_len = 0;
	  FOR_EACH_EDGE (e, ei, traces[t].last->succs)
	    {
	      int di = e->dest->index;

	      if (e->dest != EXIT_BLOCK_PTR_FOR_FN (cfun)
		  && (e->flags & EDGE_CAN_FALLTHRU)
		  && !(e->flags & EDGE_COMPLEX)
		  && bbd[di].start_of_trace >= 0
		  && !connected[bbd[di].start_of_trace]
		  && (BB_PARTITION (e->dest) == current_partition)
		  && connect_better_edge_p (e, false, best_len, best, traces))
		{
		  best = e;
		  best_len = traces[bbd[di].start_of_trace].length;
		}
	    }

	  if (for_size)
	    {
	      if (!best)
		/* Stop finding the successor traces.  */
		break;

	      /* It is OK to connect block n with block n + 1 or a block
		 before n.  For others, only connect to the loop header.  */
	      if (best->dest->index > (traces[t].last->index + 1))
		{
		  int count = EDGE_COUNT (best->dest->preds);

		  FOR_EACH_EDGE (e, ei, best->dest->preds)
		    if (e->flags & EDGE_DFS_BACK)
		      count--;

		  /* If dest has multiple predecessors, skip it.  We expect
		     that one predecessor with smaller index connects with it
		     later.  */
		  if (count != 1) 
		    break;
		}

	      /* Only connect Trace n with Trace n + 1.  It is conservative
		 to keep the order as close as possible to the original order.
		 It also helps to reduce long jumps.  */
	      if (last_trace != bbd[best->dest->index].start_of_trace - 1)
		break;

	      if (dump_file)
		fprintf (dump_file, "Connection: %d %d\n",
			 best->src->index, best->dest->index);

	      t = bbd[best->dest->index].start_of_trace;
	      traces[last_trace].last->aux = traces[t].first;
	      connected[t] = true;
	      last_trace = t;
	    }
	  else if (best)
	    {
	      if (dump_file)
		{
		  fprintf (dump_file, "Connection: %d %d\n",
			   best->src->index, best->dest->index);
		}
	      t = bbd[best->dest->index].start_of_trace;
	      traces[last_trace].last->aux = traces[t].first;
	      connected[t] = true;
	      last_trace = t;
	    }
	  else
	    {
	      /* Try to connect the traces by duplication of 1 block.  */
	      edge e2;
	      basic_block next_bb = NULL;
	      bool try_copy = false;

	      FOR_EACH_EDGE (e, ei, traces[t].last->succs)
		if (e->dest != EXIT_BLOCK_PTR_FOR_FN (cfun)
		    && (e->flags & EDGE_CAN_FALLTHRU)
		    && !(e->flags & EDGE_COMPLEX)
		    && (!best || e->probability > best->probability))
		  {
		    edge_iterator ei;
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

		    FOR_EACH_EDGE (e2, ei, e->dest->succs)
		      {
			int di = e2->dest->index;

			if (e2->dest == EXIT_BLOCK_PTR_FOR_FN (cfun)
			    || ((e2->flags & EDGE_CAN_FALLTHRU)
				&& !(e2->flags & EDGE_COMPLEX)
				&& bbd[di].start_of_trace >= 0
				&& !connected[bbd[di].start_of_trace]
				&& BB_PARTITION (e2->dest) == current_partition
				&& e2->count () >= count_threshold
				&& (!best2
				    || e2->probability > best2->probability
				    || (e2->probability == best2->probability
					&& traces[bbd[di].start_of_trace].length
					   > best2_len))))
			  {
			    best = e;
			    best2 = e2;
			    if (e2->dest != EXIT_BLOCK_PTR_FOR_FN (cfun))
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
		  && BB_PARTITION (best->src) == BB_PARTITION (best->dest)
		  && copy_bb_p (best->dest,
				optimize_edge_for_speed_p (best)
				&& (!best->count ().initialized_p ()
				    || best->count () >= count_threshold)))
		{
		  basic_block new_bb;

		  if (dump_file)
		    {
		      fprintf (dump_file, "Connection: %d %d ",
			       traces[t].last->index, best->dest->index);
		      if (!next_bb)
			fputc ('\n', dump_file);
		      else if (next_bb == EXIT_BLOCK_PTR_FOR_FN (cfun))
			fprintf (dump_file, "exit\n");
		      else
			fprintf (dump_file, "%d\n", next_bb->index);
		    }

		  new_bb = copy_bb (best->dest, best, traces[t].last, t);
		  traces[t].last = new_bb;
		  if (next_bb && next_bb != EXIT_BLOCK_PTR_FOR_FN (cfun))
		    {
		      t = bbd[next_bb->index].start_of_trace;
		      traces[last_trace].last->aux = traces[t].first;
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

  if (dump_file)
    {
      basic_block bb;

      fprintf (dump_file, "Final order:\n");
      for (bb = traces[0].first; bb; bb = (basic_block) bb->aux)
	fprintf (dump_file, "%d ", bb->index);
      fprintf (dump_file, "\n");
      fflush (dump_file);
    }

  FREE (connected);
}

/* Return true when BB can and should be copied. CODE_MAY_GROW is true
   when code size is allowed to grow by duplication.  */

static bool
copy_bb_p (const_basic_block bb, int code_may_grow)
{
  unsigned int size = 0;
  unsigned int max_size = uncond_jump_length;
  rtx_insn *insn;

  if (EDGE_COUNT (bb->preds) < 2)
    return false;
  if (!can_duplicate_block_p (bb))
    return false;

  /* Avoid duplicating blocks which have many successors (PR/13430).  */
  if (EDGE_COUNT (bb->succs) > 8)
    return false;

  if (code_may_grow && optimize_bb_for_speed_p (bb))
    max_size *= param_max_grow_copy_bb_insns;

  FOR_BB_INSNS (bb, insn)
    {
      if (INSN_P (insn))
	{
	  size += get_attr_min_length (insn);
	  if (size > max_size)
	    break;
	}
    }

  if (size <= max_size)
    return true;

  if (dump_file)
    {
      fprintf (dump_file,
	       "Block %d can't be copied because its size = %u.\n",
	       bb->index, size);
    }

  return false;
}

/* Return the length of unconditional jump instruction.  */

int
get_uncond_jump_length (void)
{
  unsigned int length;

  start_sequence ();
  rtx_code_label *label = emit_label (gen_label_rtx ());
  rtx_insn *jump = emit_jump_insn (targetm.gen_jump (label));
  length = get_attr_min_length (jump);
  end_sequence ();

  gcc_assert (length < INT_MAX);
  return length;
}

/* Create a forwarder block to OLD_BB starting with NEW_LABEL and in the
   other partition wrt OLD_BB.  */

static basic_block
create_eh_forwarder_block (rtx_code_label *new_label, basic_block old_bb)
{
  /* Split OLD_BB, so that EH pads have always only incoming EH edges,
     bb_has_eh_pred bbs are treated specially by DF infrastructure.  */
  old_bb = split_block_after_labels (old_bb)->dest;

  /* Put the new label and a jump in the new basic block.  */
  rtx_insn *label = emit_label (new_label);
  rtx_code_label *old_label = block_label (old_bb);
  rtx_insn *jump = emit_jump_insn (targetm.gen_jump (old_label));
  JUMP_LABEL (jump) = old_label;

  /* Create the new basic block and put it in last position.  */
  basic_block last_bb = EXIT_BLOCK_PTR_FOR_FN (cfun)->prev_bb;
  basic_block new_bb = create_basic_block (label, jump, last_bb);
  new_bb->aux = last_bb->aux;
  new_bb->count = old_bb->count;
  last_bb->aux = new_bb;

  emit_barrier_after_bb (new_bb);

  make_single_succ_edge (new_bb, old_bb, 0);

  /* Make sure the new basic block is in the other partition.  */
  unsigned new_partition = BB_PARTITION (old_bb);
  new_partition ^= BB_HOT_PARTITION | BB_COLD_PARTITION;
  BB_SET_PARTITION (new_bb, new_partition);

  return new_bb;
}

/* The common landing pad in block OLD_BB has edges from both partitions.
   Add a new landing pad that will just jump to the old one and split the
   edges so that no EH edge crosses partitions.  */

static void
sjlj_fix_up_crossing_landing_pad (basic_block old_bb)
{
  const unsigned lp_len = cfun->eh->lp_array->length ();
  edge_iterator ei;
  edge e;

  /* Generate the new common landing-pad label.  */
  rtx_code_label *new_label = gen_label_rtx ();
  LABEL_PRESERVE_P (new_label) = 1;

  /* Create the forwarder block.  */
  basic_block new_bb = create_eh_forwarder_block (new_label, old_bb);

  /* Create the map from old to new lp index and initialize it.  */
  unsigned *index_map = (unsigned *) alloca (lp_len * sizeof (unsigned));
  memset (index_map, 0, lp_len * sizeof (unsigned));

  /* Fix up the edges.  */
  for (ei = ei_start (old_bb->preds); (e = ei_safe_edge (ei)) != NULL; )
    if (e->src != new_bb && BB_PARTITION (e->src) == BB_PARTITION (new_bb))
      {
	rtx_insn *insn = BB_END (e->src);
	rtx note = find_reg_note (insn, REG_EH_REGION, NULL_RTX);

	gcc_assert (note != NULL);
	const unsigned old_index = INTVAL (XEXP (note, 0));

	/* Generate the new landing-pad structure.  */
	if (index_map[old_index] == 0)
	  {
	    eh_landing_pad old_lp = (*cfun->eh->lp_array)[old_index];
	    eh_landing_pad new_lp = gen_eh_landing_pad (old_lp->region);
	    new_lp->post_landing_pad = old_lp->post_landing_pad;
	    new_lp->landing_pad = new_label;
	    index_map[old_index] = new_lp->index;
	  }
	XEXP (note, 0) = GEN_INT (index_map[old_index]);

	/* Adjust the edge to the new destination.  */
	redirect_edge_succ (e, new_bb);
      }
    else
      ei_next (&ei);
}

/* The landing pad OLD_LP, in block OLD_BB, has edges from both partitions.
   Add a new landing pad that will just jump to the old one and split the
   edges so that no EH edge crosses partitions.  */

static void
dw2_fix_up_crossing_landing_pad (eh_landing_pad old_lp, basic_block old_bb)
{
  eh_landing_pad new_lp;
  edge_iterator ei;
  edge e;

  /* Generate the new landing-pad structure.  */
  new_lp = gen_eh_landing_pad (old_lp->region);
  new_lp->post_landing_pad = old_lp->post_landing_pad;
  new_lp->landing_pad = gen_label_rtx ();
  LABEL_PRESERVE_P (new_lp->landing_pad) = 1;

  /* Create the forwarder block.  */
  basic_block new_bb = create_eh_forwarder_block (new_lp->landing_pad, old_bb);

  /* Fix up the edges.  */
  for (ei = ei_start (old_bb->preds); (e = ei_safe_edge (ei)) != NULL; )
    if (e->src != new_bb && BB_PARTITION (e->src) == BB_PARTITION (new_bb))
      {
	rtx_insn *insn = BB_END (e->src);
	rtx note = find_reg_note (insn, REG_EH_REGION, NULL_RTX);

	gcc_assert (note != NULL);
	gcc_checking_assert (INTVAL (XEXP (note, 0)) == old_lp->index);
	XEXP (note, 0) = GEN_INT (new_lp->index);

	/* Adjust the edge to the new destination.  */
	redirect_edge_succ (e, new_bb);
      }
    else
      ei_next (&ei);
}


/* Ensure that all hot bbs are included in a hot path through the
   procedure. This is done by calling this function twice, once
   with WALK_UP true (to look for paths from the entry to hot bbs) and
   once with WALK_UP false (to look for paths from hot bbs to the exit).
   Returns the updated value of COLD_BB_COUNT and adds newly-hot bbs
   to BBS_IN_HOT_PARTITION.  */

static unsigned int
sanitize_hot_paths (bool walk_up, unsigned int cold_bb_count,
                    vec<basic_block> *bbs_in_hot_partition)
{
  /* Callers check this.  */
  gcc_checking_assert (cold_bb_count);

  /* Keep examining hot bbs while we still have some left to check
     and there are remaining cold bbs.  */
  vec<basic_block> hot_bbs_to_check = bbs_in_hot_partition->copy ();
  while (! hot_bbs_to_check.is_empty ()
         && cold_bb_count)
    {
      basic_block bb = hot_bbs_to_check.pop ();
      vec<edge, va_gc> *edges = walk_up ? bb->preds : bb->succs;
      edge e;
      edge_iterator ei;
      profile_probability highest_probability
				 = profile_probability::uninitialized ();
      profile_count highest_count = profile_count::uninitialized ();
      bool found = false;

      /* Walk the preds/succs and check if there is at least one already
         marked hot. Keep track of the most frequent pred/succ so that we
         can mark it hot if we don't find one.  */
      FOR_EACH_EDGE (e, ei, edges)
        {
          basic_block reach_bb = walk_up ? e->src : e->dest;

          if (e->flags & EDGE_DFS_BACK)
            continue;

	  /* Do not expect profile insanities when profile was not adjusted.  */
	  if (e->probability == profile_probability::never ()
	      || e->count () == profile_count::zero ())
	    continue;

          if (BB_PARTITION (reach_bb) != BB_COLD_PARTITION)
          {
            found = true;
            break;
          }
          /* The following loop will look for the hottest edge via
             the edge count, if it is non-zero, then fallback to
             the edge probability.  */
          if (!(e->count () > highest_count))
            highest_count = e->count ();
          if (!highest_probability.initialized_p ()
	      || e->probability > highest_probability)
            highest_probability = e->probability;
        }

      /* If bb is reached by (or reaches, in the case of !WALK_UP) another hot
         block (or unpartitioned, e.g. the entry block) then it is ok. If not,
         then the most frequent pred (or succ) needs to be adjusted.  In the
         case where multiple preds/succs have the same frequency (e.g. a
         50-50 branch), then both will be adjusted.  */
      if (found)
        continue;

      FOR_EACH_EDGE (e, ei, edges)
        {
          if (e->flags & EDGE_DFS_BACK)
            continue;
	  /* Do not expect profile insanities when profile was not adjusted.  */
	  if (e->probability == profile_probability::never ()
	      || e->count () == profile_count::zero ())
	    continue;
          /* Select the hottest edge using the edge count, if it is non-zero,
             then fallback to the edge probability.  */
          if (highest_count.initialized_p ())
            {
              if (!(e->count () >= highest_count))
                continue;
            }
          else if (!(e->probability >= highest_probability))
            continue;

          basic_block reach_bb = walk_up ? e->src : e->dest;

          /* We have a hot bb with an immediate dominator that is cold.
             The dominator needs to be re-marked hot.  */
          BB_SET_PARTITION (reach_bb, BB_HOT_PARTITION);
	  if (dump_file)
	    fprintf (dump_file, "Promoting bb %i to hot partition to sanitize "
		     "profile of bb %i in %s walk\n", reach_bb->index,
		     bb->index, walk_up ? "backward" : "forward");
          cold_bb_count--;

          /* Now we need to examine newly-hot reach_bb to see if it is also
             dominated by a cold bb.  */
          bbs_in_hot_partition->safe_push (reach_bb);
          hot_bbs_to_check.safe_push (reach_bb);
        }
    }
  hot_bbs_to_check.release ();

  return cold_bb_count;
}


/* Find the basic blocks that are rarely executed and need to be moved to
   a separate section of the .o file (to cut down on paging and improve
   cache locality).  Return a vector of all edges that cross.  */

static vec<edge>
find_rarely_executed_basic_blocks_and_crossing_edges (void)
{
  vec<edge> crossing_edges = vNULL;
  basic_block bb;
  edge e;
  edge_iterator ei;
  unsigned int cold_bb_count = 0;
  auto_vec<basic_block> bbs_in_hot_partition;

  propagate_unlikely_bbs_forward ();

  /* Mark which partition (hot/cold) each basic block belongs in.  */
  FOR_EACH_BB_FN (bb, cfun)
    {
      bool cold_bb = false;

      if (probably_never_executed_bb_p (cfun, bb))
        {
          cold_bb = true;

          /* Handle profile insanities created by upstream optimizations
             by also checking the incoming edge weights. If there is a non-cold
             incoming edge, conservatively prevent this block from being split
             into the cold section.  */
	  if (!bb->count.precise_p ())
	    FOR_EACH_EDGE (e, ei, bb->preds)
	      if (!probably_never_executed_edge_p (cfun, e))
		{
		  cold_bb = false;
		  break;
		}
        }
      if (cold_bb)
        {
          BB_SET_PARTITION (bb, BB_COLD_PARTITION);
          cold_bb_count++;
        }
      else
        {
          BB_SET_PARTITION (bb, BB_HOT_PARTITION);
          bbs_in_hot_partition.safe_push (bb);
        }
    }

  /* Ensure that hot bbs are included along a hot path from the entry to exit.
     Several different possibilities may include cold bbs along all paths
     to/from a hot bb. One is that there are edge weight insanities
     due to optimization phases that do not properly update basic block profile
     counts. The second is that the entry of the function may not be hot, because
     it is entered fewer times than the number of profile training runs, but there
     is a loop inside the function that causes blocks within the function to be
     above the threshold for hotness. This is fixed by walking up from hot bbs
     to the entry block, and then down from hot bbs to the exit, performing
     partitioning fixups as necessary.  */
  if (cold_bb_count)
    {
      mark_dfs_back_edges ();
      cold_bb_count = sanitize_hot_paths (true, cold_bb_count,
                                          &bbs_in_hot_partition);
      if (cold_bb_count)
        sanitize_hot_paths (false, cold_bb_count, &bbs_in_hot_partition);

      hash_set <basic_block> set;
      find_bbs_reachable_by_hot_paths (&set);
      FOR_EACH_BB_FN (bb, cfun)
	if (!set.contains (bb))
	  BB_SET_PARTITION (bb, BB_COLD_PARTITION);
    }

  /* The format of .gcc_except_table does not allow landing pads to
     be in a different partition as the throw.  Fix this by either
     moving the landing pads or inserting forwarder landing pads.  */
  if (cfun->eh->lp_array)
    {
      const bool sjlj
	= (targetm_common.except_unwind_info (&global_options) == UI_SJLJ);
      unsigned i;
      eh_landing_pad lp;

      FOR_EACH_VEC_ELT (*cfun->eh->lp_array, i, lp)
	{
	  bool all_same, all_diff;

	  if (lp == NULL
	      || lp->landing_pad == NULL_RTX
	      || !LABEL_P (lp->landing_pad))
	    continue;

	  all_same = all_diff = true;
	  bb = BLOCK_FOR_INSN (lp->landing_pad);
	  FOR_EACH_EDGE (e, ei, bb->preds)
	    {
	      gcc_assert (e->flags & EDGE_EH);
	      if (BB_PARTITION (bb) == BB_PARTITION (e->src))
		all_diff = false;
	      else
		all_same = false;
	    }

	  if (all_same)
	    ;
	  else if (all_diff)
	    {
	      int which = BB_PARTITION (bb);
	      which ^= BB_HOT_PARTITION | BB_COLD_PARTITION;
	      BB_SET_PARTITION (bb, which);
	    }
	  else if (sjlj)
	    sjlj_fix_up_crossing_landing_pad (bb);
	  else
	    dw2_fix_up_crossing_landing_pad (lp, bb);

	  /* There is a single, common landing pad in SJLJ mode.  */
	  if (sjlj)
	    break;
	}
    }

  /* Mark every edge that crosses between sections.  */
  FOR_EACH_BB_FN (bb, cfun)
    FOR_EACH_EDGE (e, ei, bb->succs)
      {
	unsigned int flags = e->flags;

        /* We should never have EDGE_CROSSING set yet.  */
	gcc_checking_assert ((flags & EDGE_CROSSING) == 0);

	if (e->src != ENTRY_BLOCK_PTR_FOR_FN (cfun)
	    && e->dest != EXIT_BLOCK_PTR_FOR_FN (cfun)
	    && BB_PARTITION (e->src) != BB_PARTITION (e->dest))
	  {
	    crossing_edges.safe_push (e);
	    flags |= EDGE_CROSSING;
	  }

	/* Now that we've split eh edges as appropriate, allow landing pads
	   to be merged with the post-landing pads.  */
	flags &= ~EDGE_PRESERVE;

	e->flags = flags;
      }

  return crossing_edges;
}

/* Set the flag EDGE_CAN_FALLTHRU for edges that can be fallthru.  */

static void
set_edge_can_fallthru_flag (void)
{
  basic_block bb;

  FOR_EACH_BB_FN (bb, cfun)
    {
      edge e;
      edge_iterator ei;

      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  e->flags &= ~EDGE_CAN_FALLTHRU;

	  /* The FALLTHRU edge is also CAN_FALLTHRU edge.  */
	  if (e->flags & EDGE_FALLTHRU)
	    e->flags |= EDGE_CAN_FALLTHRU;
	}

      /* If the BB ends with an invertible condjump all (2) edges are
	 CAN_FALLTHRU edges.  */
      if (EDGE_COUNT (bb->succs) != 2)
	continue;
      if (!any_condjump_p (BB_END (bb)))
	continue;

      rtx_jump_insn *bb_end_jump = as_a <rtx_jump_insn *> (BB_END (bb));
      if (!invert_jump (bb_end_jump, JUMP_LABEL (bb_end_jump), 0))
	continue;
      invert_jump (bb_end_jump, JUMP_LABEL (bb_end_jump), 0);
      EDGE_SUCC (bb, 0)->flags |= EDGE_CAN_FALLTHRU;
      EDGE_SUCC (bb, 1)->flags |= EDGE_CAN_FALLTHRU;
    }
}

/* If any destination of a crossing edge does not have a label, add label;
   Convert any easy fall-through crossing edges to unconditional jumps.  */

static void
add_labels_and_missing_jumps (vec<edge> crossing_edges)
{
  size_t i;
  edge e;

  FOR_EACH_VEC_ELT (crossing_edges, i, e)
    {
      basic_block src = e->src;
      basic_block dest = e->dest;
      rtx_jump_insn *new_jump;

      if (dest == EXIT_BLOCK_PTR_FOR_FN (cfun))
	continue;

      /* Make sure dest has a label.  */
      rtx_code_label *label = block_label (dest);

      /* Nothing to do for non-fallthru edges.  */
      if (src == ENTRY_BLOCK_PTR_FOR_FN (cfun))
	continue;
      if ((e->flags & EDGE_FALLTHRU) == 0)
	continue;

      /* If the block does not end with a control flow insn, then we
	 can trivially add a jump to the end to fixup the crossing.
	 Otherwise the jump will have to go in a new bb, which will
	 be handled by fix_up_fall_thru_edges function.  */
      if (control_flow_insn_p (BB_END (src)))
	continue;

      /* Make sure there's only one successor.  */
      gcc_assert (single_succ_p (src));

      new_jump = emit_jump_insn_after (targetm.gen_jump (label), BB_END (src));
      BB_END (src) = new_jump;
      JUMP_LABEL (new_jump) = label;
      LABEL_NUSES (label) += 1;

      emit_barrier_after_bb (src);

      /* Mark edge as non-fallthru.  */
      e->flags &= ~EDGE_FALLTHRU;
    }
}

/* Find any bb's where the fall-through edge is a crossing edge (note that
   these bb's must also contain a conditional jump or end with a call
   instruction; we've already dealt with fall-through edges for blocks
   that didn't have a conditional jump or didn't end with call instruction
   in the call to add_labels_and_missing_jumps).  Convert the fall-through
   edge to non-crossing edge by inserting a new bb to fall-through into.
   The new bb will contain an unconditional jump (crossing edge) to the
   original fall through destination.  */

static void
fix_up_fall_thru_edges (void)
{
  basic_block cur_bb;

  FOR_EACH_BB_FN (cur_bb, cfun)
    {
      edge succ1;
      edge succ2;
      edge fall_thru = NULL;
      edge cond_jump = NULL;

      fall_thru = NULL;
      if (EDGE_COUNT (cur_bb->succs) > 0)
	succ1 = EDGE_SUCC (cur_bb, 0);
      else
	succ1 = NULL;

      if (EDGE_COUNT (cur_bb->succs) > 1)
	succ2 = EDGE_SUCC (cur_bb, 1);
      else
	succ2 = NULL;

      /* Find the fall-through edge.  */

      if (succ1
	  && (succ1->flags & EDGE_FALLTHRU))
	{
	  fall_thru = succ1;
	  cond_jump = succ2;
	}
      else if (succ2
	       && (succ2->flags & EDGE_FALLTHRU))
	{
	  fall_thru = succ2;
	  cond_jump = succ1;
	}
      else if (succ2 && EDGE_COUNT (cur_bb->succs) > 2)
	fall_thru = find_fallthru_edge (cur_bb->succs);

      if (fall_thru && (fall_thru->dest != EXIT_BLOCK_PTR_FOR_FN (cfun)))
	{
	  /* Check to see if the fall-thru edge is a crossing edge.  */

	  if (fall_thru->flags & EDGE_CROSSING)
	    {
	      /* The fall_thru edge crosses; now check the cond jump edge, if
		 it exists.  */

	      bool cond_jump_crosses = true;
	      int invert_worked = 0;
	      rtx_insn *old_jump = BB_END (cur_bb);

	      /* Find the jump instruction, if there is one.  */

	      if (cond_jump)
		{
		  if (!(cond_jump->flags & EDGE_CROSSING))
		    cond_jump_crosses = false;

		  /* We know the fall-thru edge crosses; if the cond
		     jump edge does NOT cross, and its destination is the
		     next block in the bb order, invert the jump
		     (i.e. fix it so the fall through does not cross and
		     the cond jump does).  */

		  if (!cond_jump_crosses)
		    {
		      /* Find label in fall_thru block. We've already added
			 any missing labels, so there must be one.  */

		      rtx_code_label *fall_thru_label
			= block_label (fall_thru->dest);

		      if (old_jump && fall_thru_label)
			{
			  rtx_jump_insn *old_jump_insn
			    = dyn_cast <rtx_jump_insn *> (old_jump);
			  if (old_jump_insn)
			    invert_worked = invert_jump (old_jump_insn,
							 fall_thru_label, 0);
			}

		      if (invert_worked)
			{
			  fall_thru->flags &= ~EDGE_FALLTHRU;
			  cond_jump->flags |= EDGE_FALLTHRU;
			  update_br_prob_note (cur_bb);
			  std::swap (fall_thru, cond_jump);
			  cond_jump->flags |= EDGE_CROSSING;
			  fall_thru->flags &= ~EDGE_CROSSING;
			}
		    }
		}

	      if (cond_jump_crosses || !invert_worked)
		{
		  /* This is the case where both edges out of the basic
		     block are crossing edges. Here we will fix up the
		     fall through edge. The jump edge will be taken care
		     of later.  The EDGE_CROSSING flag of fall_thru edge
		     is unset before the call to force_nonfallthru
		     function because if a new basic-block is created
		     this edge remains in the current section boundary
		     while the edge between new_bb and the fall_thru->dest
		     becomes EDGE_CROSSING.  */

		  fall_thru->flags &= ~EDGE_CROSSING;
		  unsigned old_count = EDGE_COUNT (cur_bb->succs);
		  basic_block new_bb = force_nonfallthru (fall_thru);

		  if (new_bb)
		    {
		      new_bb->aux = cur_bb->aux;
		      cur_bb->aux = new_bb;

                      /* This is done by force_nonfallthru_and_redirect.  */
		      gcc_assert (BB_PARTITION (new_bb)
                                  == BB_PARTITION (cur_bb));

		      edge e = single_succ_edge (new_bb);
		      e->flags |= EDGE_CROSSING;
		      if (EDGE_COUNT (cur_bb->succs) > old_count)
			{
			  /* If asm goto has a crossing fallthrough edge
			     and at least one of the labels to the same bb,
			     force_nonfallthru can result in the fallthrough
			     edge being redirected and a new edge added for the
			     label or more labels to e->dest.  As we've
			     temporarily cleared EDGE_CROSSING flag on the
			     fallthrough edge, we need to restore it again.
			     See PR108596.  */
			  rtx_insn *j = BB_END (cur_bb);
			  gcc_checking_assert (JUMP_P (j)
					       && (asm_noperands (PATTERN (j))
						   > 0));
			  edge e2 = find_edge (cur_bb, e->dest);
			  if (e2)
			    e2->flags |= EDGE_CROSSING;
			}
		    }
		  else
		    {
		      /* If a new basic-block was not created; restore
			 the EDGE_CROSSING flag.  */
		      fall_thru->flags |= EDGE_CROSSING;
		    }

		  /* Add barrier after new jump */
		  emit_barrier_after_bb (new_bb ? new_bb : cur_bb);
		}
	    }
	}
    }
}

/* This function checks the destination block of a "crossing jump" to
   see if it has any crossing predecessors that begin with a code label
   and end with an unconditional jump.  If so, it returns that predecessor
   block.  (This is to avoid creating lots of new basic blocks that all
   contain unconditional jumps to the same destination).  */

static basic_block
find_jump_block (basic_block jump_dest)
{
  basic_block source_bb = NULL;
  edge e;
  rtx_insn *insn;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, jump_dest->preds)
    if (e->flags & EDGE_CROSSING)
      {
	basic_block src = e->src;

	/* Check each predecessor to see if it has a label, and contains
	   only one executable instruction, which is an unconditional jump.
	   If so, we can use it.  */

	if (LABEL_P (BB_HEAD (src)))
	  for (insn = BB_HEAD (src);
	       !INSN_P (insn) && insn != NEXT_INSN (BB_END (src));
	       insn = NEXT_INSN (insn))
	    {
	      if (INSN_P (insn)
		  && insn == BB_END (src)
		  && JUMP_P (insn)
		  && !any_condjump_p (insn))
		{
		  source_bb = src;
		  break;
		}
	    }

	if (source_bb)
	  break;
      }

  return source_bb;
}

/* Find all BB's with conditional jumps that are crossing edges;
   insert a new bb and make the conditional jump branch to the new
   bb instead (make the new bb same color so conditional branch won't
   be a 'crossing' edge).  Insert an unconditional jump from the
   new bb to the original destination of the conditional jump.  */

static void
fix_crossing_conditional_branches (void)
{
  basic_block cur_bb;
  basic_block new_bb;
  basic_block dest;
  edge succ1;
  edge succ2;
  edge crossing_edge;
  edge new_edge;
  rtx set_src;
  rtx old_label = NULL_RTX;
  rtx_code_label *new_label;

  FOR_EACH_BB_FN (cur_bb, cfun)
    {
      crossing_edge = NULL;
      if (EDGE_COUNT (cur_bb->succs) > 0)
	succ1 = EDGE_SUCC (cur_bb, 0);
      else
	succ1 = NULL;

      if (EDGE_COUNT (cur_bb->succs) > 1)
	succ2 = EDGE_SUCC (cur_bb, 1);
      else
	succ2 = NULL;

      /* We already took care of fall-through edges, so only one successor
	 can be a crossing edge.  */

      if (succ1 && (succ1->flags & EDGE_CROSSING))
	crossing_edge = succ1;
      else if (succ2 && (succ2->flags & EDGE_CROSSING))
	crossing_edge = succ2;

      if (crossing_edge)
	{
	  rtx_insn *old_jump = BB_END (cur_bb);

	  /* Check to make sure the jump instruction is a
	     conditional jump.  */

	  set_src = NULL_RTX;

	  if (any_condjump_p (old_jump))
	    {
	      if (GET_CODE (PATTERN (old_jump)) == SET)
		set_src = SET_SRC (PATTERN (old_jump));
	      else if (GET_CODE (PATTERN (old_jump)) == PARALLEL)
		{
		  set_src = XVECEXP (PATTERN (old_jump), 0,0);
		  if (GET_CODE (set_src) == SET)
		    set_src = SET_SRC (set_src);
		  else
		    set_src = NULL_RTX;
		}
	    }

	  if (set_src && (GET_CODE (set_src) == IF_THEN_ELSE))
	    {
	      rtx_jump_insn *old_jump_insn =
			as_a <rtx_jump_insn *> (old_jump);

	      if (GET_CODE (XEXP (set_src, 1)) == PC)
		old_label = XEXP (set_src, 2);
	      else if (GET_CODE (XEXP (set_src, 2)) == PC)
		old_label = XEXP (set_src, 1);

	      /* Check to see if new bb for jumping to that dest has
		 already been created; if so, use it; if not, create
		 a new one.  */

	      new_bb = find_jump_block (crossing_edge->dest);

	      if (new_bb)
		new_label = block_label (new_bb);
	      else
		{
		  basic_block last_bb;
		  rtx_code_label *old_jump_target;
		  rtx_jump_insn *new_jump;

		  /* Create new basic block to be dest for
		     conditional jump.  */

		  /* Put appropriate instructions in new bb.  */

		  new_label = gen_label_rtx ();
		  emit_label (new_label);

		  gcc_assert (GET_CODE (old_label) == LABEL_REF);
		  old_jump_target = old_jump_insn->jump_target ();
		  new_jump = as_a <rtx_jump_insn *>
		    (emit_jump_insn (targetm.gen_jump (old_jump_target)));
		  new_jump->set_jump_target (old_jump_target);

		  last_bb = EXIT_BLOCK_PTR_FOR_FN (cfun)->prev_bb;
		  new_bb = create_basic_block (new_label, new_jump, last_bb);
		  new_bb->aux = last_bb->aux;
		  last_bb->aux = new_bb;

		  emit_barrier_after_bb (new_bb);

		  /* Make sure new bb is in same partition as source
		     of conditional branch.  */
		  BB_COPY_PARTITION (new_bb, cur_bb);
		}

	      /* Make old jump branch to new bb.  */

	      redirect_jump (old_jump_insn, new_label, 0);

	      /* Remove crossing_edge as predecessor of 'dest'.  */

	      dest = crossing_edge->dest;

	      redirect_edge_succ (crossing_edge, new_bb);

	      /* Make a new edge from new_bb to old dest; new edge
		 will be a successor for new_bb and a predecessor
		 for 'dest'.  */

	      if (EDGE_COUNT (new_bb->succs) == 0)
		new_edge = make_single_succ_edge (new_bb, dest, 0);
	      else
		new_edge = EDGE_SUCC (new_bb, 0);

	      crossing_edge->flags &= ~EDGE_CROSSING;
	      new_edge->flags |= EDGE_CROSSING;
	    }
	}
    }
}

/* Find any unconditional branches that cross between hot and cold
   sections.  Convert them into indirect jumps instead.  */

static void
fix_crossing_unconditional_branches (void)
{
  basic_block cur_bb;
  rtx_insn *last_insn;
  rtx label;
  rtx label_addr;
  rtx_insn *indirect_jump_sequence;
  rtx_insn *jump_insn = NULL;
  rtx new_reg;
  rtx_insn *cur_insn;
  edge succ;

  FOR_EACH_BB_FN (cur_bb, cfun)
    {
      last_insn = BB_END (cur_bb);

      if (EDGE_COUNT (cur_bb->succs) < 1)
	continue;

      succ = EDGE_SUCC (cur_bb, 0);

      /* Check to see if bb ends in a crossing (unconditional) jump.  At
	 this point, no crossing jumps should be conditional.  */

      if (JUMP_P (last_insn)
	  && (succ->flags & EDGE_CROSSING))
	{
	  gcc_assert (!any_condjump_p (last_insn));

	  /* Make sure the jump is not already an indirect or table jump.  */

	  if (!computed_jump_p (last_insn)
	      && !tablejump_p (last_insn, NULL, NULL)
	      && asm_noperands (PATTERN (last_insn)) < 0)
	    {
	      /* We have found a "crossing" unconditional branch.  Now
		 we must convert it to an indirect jump.  First create
		 reference of label, as target for jump.  */

	      label = JUMP_LABEL (last_insn);
	      label_addr = gen_rtx_LABEL_REF (Pmode, label);
	      LABEL_NUSES (label) += 1;

	      /* Get a register to use for the indirect jump.  */

	      new_reg = gen_reg_rtx (Pmode);

	      /* Generate indirect the jump sequence.  */

	      start_sequence ();
	      emit_move_insn (new_reg, label_addr);
	      emit_indirect_jump (new_reg);
	      indirect_jump_sequence = get_insns ();
	      end_sequence ();

	      /* Make sure every instruction in the new jump sequence has
		 its basic block set to be cur_bb.  */

	      for (cur_insn = indirect_jump_sequence; cur_insn;
		   cur_insn = NEXT_INSN (cur_insn))
		{
		  if (!BARRIER_P (cur_insn))
		    BLOCK_FOR_INSN (cur_insn) = cur_bb;
		  if (JUMP_P (cur_insn))
		    jump_insn = cur_insn;
		}

	      /* Insert the new (indirect) jump sequence immediately before
		 the unconditional jump, then delete the unconditional jump.  */

	      emit_insn_before (indirect_jump_sequence, last_insn);
	      delete_insn (last_insn);

	      JUMP_LABEL (jump_insn) = label;
	      LABEL_NUSES (label)++;

	      /* Make BB_END for cur_bb be the jump instruction (NOT the
		 barrier instruction at the end of the sequence...).  */

	      BB_END (cur_bb) = jump_insn;
	    }
	}
    }
}

/* Update CROSSING_JUMP_P flags on all jump insns.  */

static void
update_crossing_jump_flags (void)
{
  basic_block bb;
  edge e;
  edge_iterator ei;

  FOR_EACH_BB_FN (bb, cfun)
    FOR_EACH_EDGE (e, ei, bb->succs)
      if (e->flags & EDGE_CROSSING)
	{
	  if (JUMP_P (BB_END (bb)))
	    CROSSING_JUMP_P (BB_END (bb)) = 1;
	  break;
	}
}

/* Reorder basic blocks using the software trace cache (STC) algorithm.  */

static void
reorder_basic_blocks_software_trace_cache (void)
{
  if (dump_file)
    fprintf (dump_file, "\nReordering with the STC algorithm.\n\n");

  int n_traces;
  int i;
  struct trace *traces;

  /* We are estimating the length of uncond jump insn only once since the code
     for getting the insn length always returns the minimal length now.  */
  if (uncond_jump_length == 0)
    uncond_jump_length = get_uncond_jump_length ();

  /* We need to know some information for each basic block.  */
  array_size = GET_ARRAY_SIZE (last_basic_block_for_fn (cfun));
  bbd = XNEWVEC (bbro_basic_block_data, array_size);
  for (i = 0; i < array_size; i++)
    {
      bbd[i].start_of_trace = -1;
      bbd[i].end_of_trace = -1;
      bbd[i].in_trace = -1;
      bbd[i].visited = 0;
      bbd[i].priority = -1;
      bbd[i].heap = NULL;
      bbd[i].node = NULL;
    }

  traces = XNEWVEC (struct trace, n_basic_blocks_for_fn (cfun));
  n_traces = 0;
  find_traces (&n_traces, traces);
  connect_traces (n_traces, traces);
  FREE (traces);
  FREE (bbd);
}

/* Order edges by execution frequency, higher first.  */

static int
edge_order (const void *ve1, const void *ve2)
{
  edge e1 = *(const edge *) ve1;
  edge e2 = *(const edge *) ve2;
  profile_count c1 = e1->count ();
  profile_count c2 = e2->count ();
  /* Since profile_count::operator< does not establish a strict weak order
     in presence of uninitialized counts, use 'max': this makes them appear
     as if having execution frequency less than any initialized count.  */
  profile_count m = c1.max (c2);
  return (m == c2) - (m == c1);
}

/* Reorder basic blocks using the "simple" algorithm.  This tries to
   maximize the dynamic number of branches that are fallthrough, without
   copying instructions.  The algorithm is greedy, looking at the most
   frequently executed branch first.  */

static void
reorder_basic_blocks_simple (void)
{
  if (dump_file)
    fprintf (dump_file, "\nReordering with the \"simple\" algorithm.\n\n");

  edge *edges = new edge[2 * n_basic_blocks_for_fn (cfun)];

  /* First, collect all edges that can be optimized by reordering blocks:
     simple jumps and conditional jumps, as well as the function entry edge.  */

  int n = 0;
  edges[n++] = EDGE_SUCC (ENTRY_BLOCK_PTR_FOR_FN (cfun), 0);

  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    {
      rtx_insn *end = BB_END (bb);

      if (computed_jump_p (end) || tablejump_p (end, NULL, NULL))
	continue;

      /* We cannot optimize asm goto.  */
      if (JUMP_P (end) && extract_asm_operands (end))
	continue;

      if (single_succ_p (bb))
	edges[n++] = EDGE_SUCC (bb, 0);
      else if (any_condjump_p (end))
	{
	  edge e0 = EDGE_SUCC (bb, 0);
	  edge e1 = EDGE_SUCC (bb, 1);
	  /* When optimizing for size it is best to keep the original
	     fallthrough edges.  */
	  if (e1->flags & EDGE_FALLTHRU)
	    std::swap (e0, e1);
	  edges[n++] = e0;
	  edges[n++] = e1;
	}
    }

  /* Sort the edges, the most desirable first.  When optimizing for size
     all edges are equally desirable.  */

  if (optimize_function_for_speed_p (cfun))
    gcc_stablesort (edges, n, sizeof *edges, edge_order);

  /* Now decide which of those edges to make fallthrough edges.  We set
     BB_VISITED if a block already has a fallthrough successor assigned
     to it.  We make ->AUX of an endpoint point to the opposite endpoint
     of a sequence of blocks that fall through, and ->AUX will be NULL
     for a block that is in such a sequence but not an endpoint anymore.

     To start with, everything points to itself, nothing is assigned yet.  */

  FOR_ALL_BB_FN (bb, cfun)
    {
      bb->aux = bb;
      bb->flags &= ~BB_VISITED;
    }

  EXIT_BLOCK_PTR_FOR_FN (cfun)->aux = 0;

  /* Now for all edges, the most desirable first, see if that edge can
     connect two sequences.  If it can, update AUX and BB_VISITED; if it
     cannot, zero out the edge in the table.  */

  for (int j = 0; j < n; j++)
    {
      edge e = edges[j];

      basic_block tail_a = e->src;
      basic_block head_b = e->dest;
      basic_block head_a = (basic_block) tail_a->aux;
      basic_block tail_b = (basic_block) head_b->aux;

      /* An edge cannot connect two sequences if:
	 - it crosses partitions;
	 - its src is not a current endpoint;
	 - its dest is not a current endpoint;
	 - or, it would create a loop.  */

      if (e->flags & EDGE_CROSSING
	  || tail_a->flags & BB_VISITED
	  || !tail_b
	  || (!(head_b->flags & BB_VISITED) && head_b != tail_b)
	  || tail_a == tail_b)
	{
	  edges[j] = 0;
	  continue;
	}

      tail_a->aux = 0;
      head_b->aux = 0;
      head_a->aux = tail_b;
      tail_b->aux = head_a;
      tail_a->flags |= BB_VISITED;
    }

  /* Put the pieces together, in the same order that the start blocks of
     the sequences already had.  The hot/cold partitioning gives a little
     complication: as a first pass only do this for blocks in the same
     partition as the start block, and (if there is anything left to do)
     in a second pass handle the other partition.  */

  basic_block last_tail = (basic_block) ENTRY_BLOCK_PTR_FOR_FN (cfun)->aux;

  int current_partition
    = BB_PARTITION (last_tail == ENTRY_BLOCK_PTR_FOR_FN (cfun)
		    ? EDGE_SUCC (ENTRY_BLOCK_PTR_FOR_FN (cfun), 0)->dest
		    : last_tail);
  bool need_another_pass = true;

  for (int pass = 0; pass < 2 && need_another_pass; pass++)
    {
      need_another_pass = false;

      FOR_EACH_BB_FN (bb, cfun)
	if ((bb->flags & BB_VISITED && bb->aux) || bb->aux == bb)
	  {
	    if (BB_PARTITION (bb) != current_partition)
	      {
		need_another_pass = true;
		continue;
	      }

	    last_tail->aux = bb;
	    last_tail = (basic_block) bb->aux;
	  }

      current_partition ^= BB_HOT_PARTITION | BB_COLD_PARTITION;
    }

  last_tail->aux = 0;

  /* Finally, link all the chosen fallthrough edges.  */

  for (int j = 0; j < n; j++)
    if (edges[j])
      edges[j]->src->aux = edges[j]->dest;

  delete[] edges;

  /* If the entry edge no longer falls through we have to make a new
     block so it can do so again.  */

  edge e = EDGE_SUCC (ENTRY_BLOCK_PTR_FOR_FN (cfun), 0);
  if (e->dest != ENTRY_BLOCK_PTR_FOR_FN (cfun)->aux)
    {
      force_nonfallthru (e);
      e->src->aux = ENTRY_BLOCK_PTR_FOR_FN (cfun)->aux;
    }
}

/* Reorder basic blocks.  The main entry point to this file.  */

static void
reorder_basic_blocks (void)
{
  gcc_assert (current_ir_type () == IR_RTL_CFGLAYOUT);

  if (n_basic_blocks_for_fn (cfun) <= NUM_FIXED_BLOCKS + 1)
    return;

  set_edge_can_fallthru_flag ();
  mark_dfs_back_edges ();

  switch (flag_reorder_blocks_algorithm)
    {
    case REORDER_BLOCKS_ALGORITHM_SIMPLE:
      reorder_basic_blocks_simple ();
      break;

    case REORDER_BLOCKS_ALGORITHM_STC:
      reorder_basic_blocks_software_trace_cache ();
      break;

    default:
      gcc_unreachable ();
    }

  relink_block_chain (/*stay_in_cfglayout_mode=*/true);

  if (dump_file)
    {
      if (dump_flags & TDF_DETAILS)
	dump_reg_info (dump_file);
      dump_flow_info (dump_file, dump_flags);
    }

  /* Signal that rtl_verify_flow_info_1 can now verify that there
     is at most one switch between hot/cold sections.  */
  crtl->bb_reorder_complete = true;
}

/* Determine which partition the first basic block in the function
   belongs to, then find the first basic block in the current function
   that belongs to a different section, and insert a
   NOTE_INSN_SWITCH_TEXT_SECTIONS note immediately before it in the
   instruction stream.  When writing out the assembly code,
   encountering this note will make the compiler switch between the
   hot and cold text sections.  */

void
insert_section_boundary_note (void)
{
  basic_block bb;
  bool switched_sections = false;
  int current_partition = 0;

  if (!crtl->has_bb_partition)
    return;

  FOR_EACH_BB_FN (bb, cfun)
    {
      if (!current_partition)
	current_partition = BB_PARTITION (bb);
      if (BB_PARTITION (bb) != current_partition)
	{
	  gcc_assert (!switched_sections);
          switched_sections = true;
          emit_note_before (NOTE_INSN_SWITCH_TEXT_SECTIONS, BB_HEAD (bb));
          current_partition = BB_PARTITION (bb);
	}
    }

  /* Make sure crtl->has_bb_partition matches reality even if bbpart finds
     some hot and some cold basic blocks, but later one of those kinds is
     optimized away.  */
  crtl->has_bb_partition = switched_sections;
}

namespace {

const pass_data pass_data_reorder_blocks =
{
  RTL_PASS, /* type */
  "bbro", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_REORDER_BLOCKS, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_reorder_blocks : public rtl_opt_pass
{
public:
  pass_reorder_blocks (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_reorder_blocks, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) final override
    {
      if (targetm.cannot_modify_jumps_p ())
	return false;
      return (optimize > 0
	      && (flag_reorder_blocks || flag_reorder_blocks_and_partition));
    }

  unsigned int execute (function *) final override;

}; // class pass_reorder_blocks

unsigned int
pass_reorder_blocks::execute (function *fun)
{
  basic_block bb;

  /* Last attempt to optimize CFG, as scheduling, peepholing and insn
     splitting possibly introduced more crossjumping opportunities.  */
  cfg_layout_initialize (CLEANUP_EXPENSIVE);

  reorder_basic_blocks ();
  cleanup_cfg (CLEANUP_EXPENSIVE | CLEANUP_NO_PARTITIONING);

  FOR_EACH_BB_FN (bb, fun)
    if (bb->next_bb != EXIT_BLOCK_PTR_FOR_FN (fun))
      bb->aux = bb->next_bb;
  cfg_layout_finalize ();

  FOR_EACH_BB_FN (bb, fun)
    df_recompute_luids (bb);
  return 0;
}

} // anon namespace

rtl_opt_pass *
make_pass_reorder_blocks (gcc::context *ctxt)
{
  return new pass_reorder_blocks (ctxt);
}

/* Duplicate a block (that we already know ends in a computed jump) into its
   predecessors, where possible.  Return whether anything is changed.  */
static bool
maybe_duplicate_computed_goto (basic_block bb, int max_size)
{
  /* Make sure that the block is small enough.  */
  rtx_insn *insn;
  FOR_BB_INSNS (bb, insn)
    if (INSN_P (insn))
      {
	max_size -= get_attr_min_length (insn);
	if (max_size < 0)
	   return false;
      }

  bool changed = false;
  edge e;
  edge_iterator ei;
  for (ei = ei_start (bb->preds); (e = ei_safe_edge (ei)); )
    {
      basic_block pred = e->src;

      /* Do not duplicate BB into PRED if we cannot merge a copy of BB
	 with PRED.  */
      if (!single_succ_p (pred)
	  || e->flags & EDGE_COMPLEX
	  || pred->index < NUM_FIXED_BLOCKS
	  || (JUMP_P (BB_END (pred)) && !simplejump_p (BB_END (pred)))
	  || (JUMP_P (BB_END (pred)) && CROSSING_JUMP_P (BB_END (pred))))
	{
	  ei_next (&ei);
	  continue;
	}

      if (dump_file)
	fprintf (dump_file, "Duplicating computed goto bb %d into bb %d\n",
		 bb->index, e->src->index);

      /* Remember if PRED can be duplicated; if so, the copy of BB merged
	 with PRED can be duplicated as well.  */
      bool can_dup_more = can_duplicate_block_p (pred);

      /* Make a copy of BB, merge it into PRED.  */
      basic_block copy = duplicate_block (bb, e, NULL);
      emit_barrier_after_bb (copy);
      reorder_insns_nobb (BB_HEAD (copy), BB_END (copy), BB_END (pred));
      merge_blocks (pred, copy);

      changed = true;

      /* Try to merge the resulting merged PRED into further predecessors.  */
      if (can_dup_more)
	maybe_duplicate_computed_goto (pred, max_size);
    }

  return changed;
}

/* Duplicate the blocks containing computed gotos.  This basically unfactors
   computed gotos that were factored early on in the compilation process to
   speed up edge based data flow.  We used to not unfactor them again, which
   can seriously pessimize code with many computed jumps in the source code,
   such as interpreters.  See e.g. PR15242.  */
static void
duplicate_computed_gotos (function *fun)
{
  /* We are estimating the length of uncond jump insn only once
     since the code for getting the insn length always returns
     the minimal length now.  */
  if (uncond_jump_length == 0)
    uncond_jump_length = get_uncond_jump_length ();

  /* Never copy a block larger than this.  */
  int max_size
    = uncond_jump_length * param_max_goto_duplication_insns;

  bool changed = false;

  /* Try to duplicate all blocks that end in a computed jump and that
     can be duplicated at all.  */
  basic_block bb;
  FOR_EACH_BB_FN (bb, fun)
    if (computed_jump_p (BB_END (bb)) && can_duplicate_block_p (bb))
      changed |= maybe_duplicate_computed_goto (bb, max_size);

  /* Some blocks may have become unreachable.  */
  if (changed)
    cleanup_cfg (0);

  /* Duplicating blocks will redirect edges and may cause hot blocks
    previously reached by both hot and cold blocks to become dominated
    only by cold blocks.  */
  if (changed)
    fixup_partitions ();
}

namespace {

const pass_data pass_data_duplicate_computed_gotos =
{
  RTL_PASS, /* type */
  "compgotos", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_REORDER_BLOCKS, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_duplicate_computed_gotos : public rtl_opt_pass
{
public:
  pass_duplicate_computed_gotos (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_duplicate_computed_gotos, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) final override;
  unsigned int execute (function *) final override;

}; // class pass_duplicate_computed_gotos

bool
pass_duplicate_computed_gotos::gate (function *fun)
{
  if (targetm.cannot_modify_jumps_p ())
    return false;
  return (optimize > 0
	  && flag_expensive_optimizations
	  && ! optimize_function_for_size_p (fun));
}

unsigned int
pass_duplicate_computed_gotos::execute (function *fun)
{
  duplicate_computed_gotos (fun);

  return 0;
}

} // anon namespace

rtl_opt_pass *
make_pass_duplicate_computed_gotos (gcc::context *ctxt)
{
  return new pass_duplicate_computed_gotos (ctxt);
}

/* This function is the main 'entrance' for the optimization that
   partitions hot and cold basic blocks into separate sections of the
   .o file (to improve performance and cache locality).  Ideally it
   would be called after all optimizations that rearrange the CFG have
   been called.  However part of this optimization may introduce new
   register usage, so it must be called before register allocation has
   occurred.  This means that this optimization is actually called
   well before the optimization that reorders basic blocks (see
   function above).

   This optimization checks the feedback information to determine
   which basic blocks are hot/cold, updates flags on the basic blocks
   to indicate which section they belong in.  This information is
   later used for writing out sections in the .o file.  Because hot
   and cold sections can be arbitrarily large (within the bounds of
   memory), far beyond the size of a single function, it is necessary
   to fix up all edges that cross section boundaries, to make sure the
   instructions used can actually span the required distance.  The
   fixes are described below.

   Fall-through edges must be changed into jumps; it is not safe or
   legal to fall through across a section boundary.  Whenever a
   fall-through edge crossing a section boundary is encountered, a new
   basic block is inserted (in the same section as the fall-through
   source), and the fall through edge is redirected to the new basic
   block.  The new basic block contains an unconditional jump to the
   original fall-through target.  (If the unconditional jump is
   insufficient to cross section boundaries, that is dealt with a
   little later, see below).

   In order to deal with architectures that have short conditional
   branches (which cannot span all of memory) we take any conditional
   jump that attempts to cross a section boundary and add a level of
   indirection: it becomes a conditional jump to a new basic block, in
   the same section.  The new basic block contains an unconditional
   jump to the original target, in the other section.

   For those architectures whose unconditional branch is also
   incapable of reaching all of memory, those unconditional jumps are
   converted into indirect jumps, through a register.

   IMPORTANT NOTE: This optimization causes some messy interactions
   with the cfg cleanup optimizations; those optimizations want to
   merge blocks wherever possible, and to collapse indirect jump
   sequences (change "A jumps to B jumps to C" directly into "A jumps
   to C").  Those optimizations can undo the jump fixes that
   partitioning is required to make (see above), in order to ensure
   that jumps attempting to cross section boundaries are really able
   to cover whatever distance the jump requires (on many architectures
   conditional or unconditional jumps are not able to reach all of
   memory).  Therefore tests have to be inserted into each such
   optimization to make sure that it does not undo stuff necessary to
   cross partition boundaries.  This would be much less of a problem
   if we could perform this optimization later in the compilation, but
   unfortunately the fact that we may need to create indirect jumps
   (through registers) requires that this optimization be performed
   before register allocation.

   Hot and cold basic blocks are partitioned and put in separate
   sections of the .o file, to reduce paging and improve cache
   performance (hopefully).  This can result in bits of code from the
   same function being widely separated in the .o file.  However this
   is not obvious to the current bb structure.  Therefore we must take
   care to ensure that: 1). There are no fall_thru edges that cross
   between sections; 2). For those architectures which have "short"
   conditional branches, all conditional branches that attempt to
   cross between sections are converted to unconditional branches;
   and, 3). For those architectures which have "short" unconditional
   branches, all unconditional branches that attempt to cross between
   sections are converted to indirect jumps.

   The code for fixing up fall_thru edges that cross between hot and
   cold basic blocks does so by creating new basic blocks containing
   unconditional branches to the appropriate label in the "other"
   section.  The new basic block is then put in the same (hot or cold)
   section as the original conditional branch, and the fall_thru edge
   is modified to fall into the new basic block instead.  By adding
   this level of indirection we end up with only unconditional branches
   crossing between hot and cold sections.

   Conditional branches are dealt with by adding a level of indirection.
   A new basic block is added in the same (hot/cold) section as the
   conditional branch, and the conditional branch is retargeted to the
   new basic block.  The new basic block contains an unconditional branch
   to the original target of the conditional branch (in the other section).

   Unconditional branches are dealt with by converting them into
   indirect jumps.  */

namespace {

const pass_data pass_data_partition_blocks =
{
  RTL_PASS, /* type */
  "bbpart", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_REORDER_BLOCKS, /* tv_id */
  PROP_cfglayout, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_partition_blocks : public rtl_opt_pass
{
public:
  pass_partition_blocks (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_partition_blocks, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) final override;
  unsigned int execute (function *) final override;

}; // class pass_partition_blocks

bool
pass_partition_blocks::gate (function *fun)
{
  /* The optimization to partition hot/cold basic blocks into separate
     sections of the .o file does not work well with linkonce or with
     user defined section attributes or with naked attribute.  Don't call
     it if either case arises.  */
  return (flag_reorder_blocks_and_partition
	  && optimize
	  /* See pass_reorder_blocks::gate.  We should not partition if
	     we are going to omit the reordering.  */
	  && optimize_function_for_speed_p (fun)
	  && !DECL_COMDAT_GROUP (current_function_decl)
	  && !lookup_attribute ("section", DECL_ATTRIBUTES (fun->decl))
	  && !lookup_attribute ("naked", DECL_ATTRIBUTES (fun->decl))
	  /* Workaround a bug in GDB where read_partial_die doesn't cope
	     with DIEs with DW_AT_ranges, see PR81115.  */
	  && !(in_lto_p && MAIN_NAME_P (DECL_NAME (fun->decl))));
}

unsigned
pass_partition_blocks::execute (function *fun)
{
  vec<edge> crossing_edges;

  if (n_basic_blocks_for_fn (fun) <= NUM_FIXED_BLOCKS + 1)
    return 0;

  df_set_flags (DF_DEFER_INSN_RESCAN);

  crossing_edges = find_rarely_executed_basic_blocks_and_crossing_edges ();
  if (!crossing_edges.exists ())
    /* Make sure to process deferred rescans and clear changeable df flags.  */
    return TODO_df_finish;

  crtl->has_bb_partition = true;

  /* Make sure the source of any crossing edge ends in a jump and the
     destination of any crossing edge has a label.  */
  add_labels_and_missing_jumps (crossing_edges);

  /* Convert all crossing fall_thru edges to non-crossing fall
     thrus to unconditional jumps (that jump to the original fall
     through dest).  */
  fix_up_fall_thru_edges ();

  /* If the architecture does not have conditional branches that can
     span all of memory, convert crossing conditional branches into
     crossing unconditional branches.  */
  if (!HAS_LONG_COND_BRANCH)
    fix_crossing_conditional_branches ();

  /* If the architecture does not have unconditional branches that
     can span all of memory, convert crossing unconditional branches
     into indirect jumps.  Since adding an indirect jump also adds
     a new register usage, update the register usage information as
     well.  */
  if (!HAS_LONG_UNCOND_BRANCH)
    fix_crossing_unconditional_branches ();

  update_crossing_jump_flags ();

  /* Clear bb->aux fields that the above routines were using.  */
  clear_aux_for_blocks ();

  crossing_edges.release ();

  /* ??? FIXME: DF generates the bb info for a block immediately.
     And by immediately, I mean *during* creation of the block.

	#0  df_bb_refs_collect
	#1  in df_bb_refs_record
	#2  in create_basic_block_structure

     Which means that the bb_has_eh_pred test in df_bb_refs_collect
     will *always* fail, because no edges can have been added to the
     block yet.  Which of course means we don't add the right 
     artificial refs, which means we fail df_verify (much) later.

     Cleanest solution would seem to make DF_DEFER_INSN_RESCAN imply
     that we also shouldn't grab data from the new blocks those new
     insns are in either.  In this way one can create the block, link
     it up properly, and have everything Just Work later, when deferred
     insns are processed.

     In the meantime, we have no other option but to throw away all
     of the DF data and recompute it all.  */
  if (fun->eh->lp_array)
    {
      df_finish_pass (true);
      df_scan_alloc (NULL);
      df_scan_blocks ();
      /* Not all post-landing pads use all of the EH_RETURN_DATA_REGNO
	 data.  We blindly generated all of them when creating the new
	 landing pad.  Delete those assignments we don't use.  */
      df_set_flags (DF_LR_RUN_DCE);
      df_analyze ();
    }

  /* Make sure to process deferred rescans and clear changeable df flags.  */
  return TODO_df_finish;
}

} // anon namespace

rtl_opt_pass *
make_pass_partition_blocks (gcc::context *ctxt)
{
  return new pass_partition_blocks (ctxt);
}
