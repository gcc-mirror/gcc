/* Calculate branch probabilities, and basic block execution counts.
   Copyright (C) 1990, 1991, 1992, 1993, 1994, 1996, 1997, 1998, 1999,
   2000, 2001  Free Software Foundation, Inc.
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

   The two auxiliary files generated are <dumpbase>.bb and
   <dumpbase>.bbg. The former contains the BB->linenumber
   mappings, and the latter describes the BB graph.

   The BB file contains line numbers for each block. For each basic
   block, a zero count is output (to mark the start of a block), then
   the line numbers of that block are listed. A zero ends the file
   too.

   The BBG file contains a count of the blocks, followed by edge
   information, for every edge in the graph. The edge information
   lists the source and target block numbers, and a bit mask
   describing the type of edge.

   The BB and BBG file formats are fully described in the gcov
   documentation.  */

/* ??? Register allocation should use basic block execution counts to
   give preference to the most commonly executed blocks.  */

/* ??? The .da files are not safe.  Changing the program after creating .da
   files or using different options when compiling with -fbranch-probabilities
   can result the arc data not matching the program.  Maybe add instrumented
   arc count to .bbg file?  Maybe check whether PFG matches the .bbg file?  */

/* ??? Should calculate branch probabilities before instrumenting code, since
   then we can use arc counts to help decide which arcs to instrument.  */

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "tree.h"
#include "flags.h"
#include "insn-config.h"
#include "output.h"
#include "regs.h"
#include "expr.h"
#include "function.h"
#include "toplev.h"
#include "ggc.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "gcov-io.h"
#include "target.h"
#include "profile.h"
#include "libfuncs.h"
#include "langhooks.h"

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

/* Keep all basic block indexes nonnegative in the gcov output.  Index 0
   is used for entry block, last block exit block.  */
#define BB_TO_GCOV_INDEX(bb)  ((bb) == ENTRY_BLOCK_PTR ? 0		\
			       : ((bb) == EXIT_BLOCK_PTR		\
				  ? last_basic_block + 1 : (bb)->index + 1))

/* Instantiate the profile info structure.  */

struct profile_info profile_info;

/* Name and file pointer of the output file for the basic block graph.  */

static FILE *bbg_file;

/* Name and file pointer of the input file for the arc count data.  */

static FILE *da_file;
static char *da_file_name;

/* Pointer of the output file for the basic block/line number map.  */
static FILE *bb_file;

/* Last source file name written to bb_file.  */

static char *last_bb_file_name;

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
static void find_spanning_tree PARAMS ((struct edge_list *));
static void init_edge_profiler PARAMS ((void));
static rtx gen_edge_profiler PARAMS ((int));
static void instrument_edges PARAMS ((struct edge_list *));
static void output_gcov_string PARAMS ((const char *, long));
static void compute_branch_probabilities PARAMS ((void));
static gcov_type * get_exec_counts PARAMS ((void));
static long compute_checksum PARAMS ((void));
static basic_block find_group PARAMS ((basic_block));
static void union_groups PARAMS ((basic_block, basic_block));

/* If nonzero, we need to output a constructor to set up the
   per-object-file data.  */
static int need_func_profiler = 0;

/* Add edge instrumentation code to the entire insn chain.

   F is the first insn of the chain.
   NUM_BLOCKS is the number of basic blocks found in F.  */

static void
instrument_edges (el)
     struct edge_list *el;
{
  int num_instr_edges = 0;
  int num_edges = NUM_EDGES (el);
  basic_block bb;
  remove_fake_edges ();

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, NULL, next_bb)
    {
      edge e = bb->succ;
      while (e)
	{
	  struct edge_info *inf = EDGE_INFO (e);
	  if (!inf->ignore && !inf->on_tree)
	    {
	      if (e->flags & EDGE_ABNORMAL)
		abort ();
	      if (rtl_dump_file)
		fprintf (rtl_dump_file, "Edge %d to %d instrumented%s\n",
			 e->src->index, e->dest->index,
			 EDGE_CRITICAL_P (e) ? " (and split)" : "");
	      need_func_profiler = 1;
	      insert_insn_on_edge (
			 gen_edge_profiler (total_num_edges_instrumented
					    + num_instr_edges++), e);
	    }
	  e = e->succ_next;
	}
    }

  profile_info.count_edges_instrumented_now = num_instr_edges;
  total_num_edges_instrumented += num_instr_edges;
  profile_info.count_instrumented_edges = total_num_edges_instrumented;

  total_num_blocks_created += num_edges;
  if (rtl_dump_file)
    fprintf (rtl_dump_file, "%d edges instrumented\n", num_instr_edges);

  commit_edge_insertions_watch_calls ();
}

/* Output STRING to bb_file, surrounded by DELIMITER.  */

static void
output_gcov_string (string, delimiter)
     const char *string;
     long delimiter;
{
  size_t temp;

  /* Write a delimiter to indicate that a file name follows.  */
  __write_long (delimiter, bb_file, 4);

  /* Write the string.  */
  temp = strlen (string) + 1;
  fwrite (string, temp, 1, bb_file);

  /* Append a few zeros, to align the output to a 4 byte boundary.  */
  temp = temp & 0x3;
  if (temp)
    {
      char c[4];

      c[0] = c[1] = c[2] = c[3] = 0;
      fwrite (c, sizeof (char), 4 - temp, bb_file);
    }

  /* Store another delimiter in the .bb file, just to make it easy to find
     the end of the file name.  */
  __write_long (delimiter, bb_file, 4);
}


/* Computes hybrid profile for all matching entries in da_file.
   Sets max_counter_in_program as a side effect.  */

static gcov_type *
get_exec_counts ()
{
  int num_edges = 0;
  basic_block bb;
  int okay = 1, i;
  int mismatch = 0;
  gcov_type *profile;
  char *function_name_buffer;
  int function_name_buffer_len;
  gcov_type max_counter_in_run;
  const char *name = IDENTIFIER_POINTER
		      (DECL_ASSEMBLER_NAME (current_function_decl));

  profile_info.max_counter_in_program = 0;
  profile_info.count_profiles_merged = 0;

  /* No .da file, no execution counts.  */
  if (!da_file)
    return 0;

  /* Count the edges to be (possibly) instrumented.  */

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, NULL, next_bb)
    {
      edge e;
      for (e = bb->succ; e; e = e->succ_next)
	if (!EDGE_INFO (e)->ignore && !EDGE_INFO (e)->on_tree)
	  num_edges++;
    }

  /* now read and combine all matching profiles.  */

  profile = xmalloc (sizeof (gcov_type) * num_edges);
  rewind (da_file);
  function_name_buffer_len = strlen (name) + 1;
  function_name_buffer = xmalloc (function_name_buffer_len + 1);

  for (i = 0; i < num_edges; i++)
    profile[i] = 0;

  while (1)
    {
      long magic, extra_bytes;
      long func_count;
      int i;

      if (__read_long (&magic, da_file, 4) != 0)
	break;

      if (magic != -123)
	{
	  okay = 0;
	  break;
	}

      if (__read_long (&func_count, da_file, 4) != 0)
	{
	  okay = 0;
	  break;
	}

      if (__read_long (&extra_bytes, da_file, 4) != 0)
	{
	  okay = 0;
	  break;
	}

      fseek (da_file, 4 + 8, SEEK_CUR);

      /* read the maximal counter.  */
      __read_gcov_type (&max_counter_in_run, da_file, 8);

      /* skip the rest of "statistics" emited by __bb_exit_func.  */
      fseek (da_file, extra_bytes - (4 + 8 + 8), SEEK_CUR);

      for (i = 0; i < func_count; i++)
	{
	  long arc_count;
	  long chksum;
	  int j;

	  if (__read_gcov_string
	      (function_name_buffer, function_name_buffer_len, da_file,
	       -1) != 0)
	    {
	      okay = 0;
	      break;
	    }

	  if (__read_long (&chksum, da_file, 4) != 0)
	    {
	      okay = 0;
	      break;
	    }

	  if (__read_long (&arc_count, da_file, 4) != 0)
	    {
	      okay = 0;
	      break;
	    }

	  if (strcmp (function_name_buffer, name) != 0)
	    {
	      /* skip */
	      if (fseek (da_file, arc_count * 8, SEEK_CUR) < 0)
		{
		  okay = 0;
		  break;
		}
	    }
	  else if (arc_count != num_edges
		   || chksum != profile_info.current_function_cfg_checksum)
	    okay = 0, mismatch = 1;
	  else
	    {
	      gcov_type tmp;

	      profile_info.max_counter_in_program += max_counter_in_run;
	      profile_info.count_profiles_merged++;

	      for (j = 0; j < arc_count; j++)
		if (__read_gcov_type (&tmp, da_file, 8) != 0)
		  {
		    okay = 0;
		    break;
		  }
		else
		  {
		    profile[j] += tmp;
		  }
	    }
	}

      if (!okay)
	break;

    }

  free (function_name_buffer);

  if (!okay)
    {
      if (mismatch)
	error
	  ("Profile does not match flowgraph of function %s (out of date?)",
	   current_function_name);
      else
	error (".da file corrupted");
      free (profile);
      return 0;
    }
  if (rtl_dump_file)
    {
      fprintf(rtl_dump_file, "Merged %i profiles with maximal count %i.\n",
	      profile_info.count_profiles_merged,
	      (int)profile_info.max_counter_in_program);
    }

  return profile;
}


/* Compute the branch probabilities for the various branches.
   Annotate them accordingly.  */

static void
compute_branch_probabilities ()
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

		  /* Seedgeh for the invalid edge, and set its count.  */
		  for (e = bb->pred; e; e = e->pred_next)
		    if (! EDGE_INFO (e)->count_valid && ! EDGE_INFO (e)->ignore)
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
      gcov_type total;
      rtx note;

      total = bb->count;
      if (total)
	{
	  for (e = bb->succ; e; e = e->succ_next)
	    {
		e->probability = (e->count * REG_BR_PROB_BASE + total / 2) / total;
		if (e->probability < 0 || e->probability > REG_BR_PROB_BASE)
		  {
		    error ("corrupted profile info: prob for %d-%d thought to be %d",
			   e->src->index, e->dest->index, e->probability);
		    e->probability = REG_BR_PROB_BASE / 2;
		  }
	    }
	  if (bb->index >= 0
	      && any_condjump_p (bb->end)
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

	      note = find_reg_note (bb->end, REG_BR_PROB, 0);
	      /* There may be already note put by some other pass, such
		 as builtin_expect expander.  */
	      if (note)
		XEXP (note, 0) = GEN_INT (prob);
	      else
		REG_NOTES (bb->end)
		  = gen_rtx_EXPR_LIST (REG_BR_PROB, GEN_INT (prob),
				       REG_NOTES (bb->end));
	      num_branches++;
	    }
	}
      /* Otherwise distribute the probabilities evenly so we get sane sum.
	 Use simple heuristics that if there are normal edges, give all abnormals
	 frequency of 0, otherwise distribute the frequency over abnormals
	 (this is the case of noreturn calls).  */
      else
	{
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
	      && any_condjump_p (bb->end)
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
  if (exec_counts)
    free (exec_counts);
}

/* Compute checksum for the current function.  */

#define CHSUM_HASH	500000003
#define CHSUM_SHIFT	2

static long
compute_checksum ()
{
  long chsum = 0;
  basic_block bb;

  FOR_EACH_BB (bb)
    {
      edge e;

      for (e = bb->succ; e; e = e->succ_next)
	{
	  chsum = ((chsum << CHSUM_SHIFT) + (BB_TO_GCOV_INDEX (e->dest) + 1)) % CHSUM_HASH;
	}

      chsum = (chsum << CHSUM_SHIFT) % CHSUM_HASH;
    }

  return chsum;
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
branch_prob ()
{
  basic_block bb;
  int i;
  int num_edges, ignored_edges;
  struct edge_list *el;
  const char *name = IDENTIFIER_POINTER
		       (DECL_ASSEMBLER_NAME (current_function_decl));

  profile_info.current_function_cfg_checksum = compute_checksum ();

  if (rtl_dump_file)
    fprintf (rtl_dump_file, "CFG checksum is %ld\n",
	profile_info.current_function_cfg_checksum);

  /* Start of a function.  */
  if (flag_test_coverage)
    output_gcov_string (name, (long) -2);

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
      rtx insn;
      edge e;

      /* Add fake edges from entry block to the call insns that may return
	 twice.  The CFG is not quite correct then, as call insn plays more
	 role of CODE_LABEL, but for our purposes, everything should be OK,
	 as we never insert code to the beggining of basic block.  */
      for (insn = bb->head; insn != NEXT_INSN (bb->end);
	   insn = NEXT_INSN (insn))
	{
	  if (GET_CODE (insn) == CALL_INSN
	      && find_reg_note (insn, REG_SETJMP, NULL))
	    {
	      if (GET_CODE (bb->head) == CODE_LABEL
		  || insn != NEXT_INSN (bb->head))
		{
		  e = split_block (bb, PREV_INSN (insn));
		  make_edge (ENTRY_BLOCK_PTR, e->dest, EDGE_FAKE);
		  break;
		}
	      else
		{
		  /* We should not get abort here, as call to setjmp should not
		     be the very first instruction of function.  */
		  if (bb == ENTRY_BLOCK_PTR)
		    abort ();
		  make_edge (ENTRY_BLOCK_PTR, bb, EDGE_FAKE);
		}
	    }
	}

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

  /* Output line number information about each basic block for
     GCOV utility.  */
  if (flag_test_coverage)
    {
      FOR_EACH_BB (bb)
	{
	  rtx insn = bb->head;
	  static int ignore_next_note = 0;

	  /* We are looking for line number notes.  Search backward before
	     basic block to find correct ones.  */
	  insn = prev_nonnote_insn (insn);
	  if (!insn)
	    insn = get_insns ();
	  else
	    insn = NEXT_INSN (insn);

	  /* Output a zero to the .bb file to indicate that a new
	     block list is starting.  */
	  __write_long (0, bb_file, 4);

	  while (insn != bb->end)
	    {
	      if (GET_CODE (insn) == NOTE)
		{
		  /* Must ignore the line number notes that immediately
		     follow the end of an inline function to avoid counting
		     it twice.  There is a note before the call, and one
		     after the call.  */
		  if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_REPEATED_LINE_NUMBER)
		    ignore_next_note = 1;
		  else if (NOTE_LINE_NUMBER (insn) > 0)
		    {
		      if (ignore_next_note)
			ignore_next_note = 0;
		      else
			{
			  /* If this is a new source file, then output the
			     file's name to the .bb file.  */
			  if (! last_bb_file_name
			      || strcmp (NOTE_SOURCE_FILE (insn),
					 last_bb_file_name))
			    {
			      if (last_bb_file_name)
				free (last_bb_file_name);
			      last_bb_file_name
				= xstrdup (NOTE_SOURCE_FILE (insn));
			      output_gcov_string (NOTE_SOURCE_FILE (insn),
						  (long)-1);
			    }
			  /* Output the line number to the .bb file.  Must be
			     done after the output_bb_profile_data() call, and
			     after the file name is written, to ensure that it
			     is correctly handled by gcov.  */
			  __write_long (NOTE_LINE_NUMBER (insn), bb_file, 4);
			}
		    }
		}
	      insn = NEXT_INSN (insn);
	    }
	}
      __write_long (0, bb_file, 4);
    }

  /* Create spanning tree from basic block graph, mark each edge that is
     on the spanning tree.  We insert as many abnormal and critical edges
     as possible to minimize number of edge splits necessary.  */

  find_spanning_tree (el);

  /* Fake edges that are not on the tree will not be instrumented, so
     mark them ignored.  */
  for (i = 0; i < num_edges; i++)
    {
      edge e = INDEX_EDGE (el, i);
      struct edge_info *inf = EDGE_INFO (e);
      if ((e->flags & EDGE_FAKE) && !inf->ignore && !inf->on_tree)
	{
	  inf->ignore = 1;
	  ignored_edges++;
	}
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

  /* Create a .bbg file from which gcov can reconstruct the basic block
     graph.  First output the number of basic blocks, and then for every
     edge output the source and target basic block numbers.
     NOTE: The format of this file must be compatible with gcov.  */

  if (flag_test_coverage)
    {
      int flag_bits;

      __write_gcov_string (name, strlen (name), bbg_file, -1);

      /* write checksum.  */
      __write_long (profile_info.current_function_cfg_checksum, bbg_file, 4);

      /* The plus 2 stands for entry and exit block.  */
      __write_long (n_basic_blocks + 2, bbg_file, 4);
      __write_long (num_edges - ignored_edges + 1, bbg_file, 4);

      FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, EXIT_BLOCK_PTR, next_bb)
	{
	  edge e;
	  long count = 0;

	  for (e = bb->succ; e; e = e->succ_next)
	    if (!EDGE_INFO (e)->ignore)
	      count++;
	  __write_long (count, bbg_file, 4);

	  for (e = bb->succ; e; e = e->succ_next)
	    {
	      struct edge_info *i = EDGE_INFO (e);
	      if (!i->ignore)
		{
		  flag_bits = 0;
		  if (i->on_tree)
		    flag_bits |= 0x1;
		  if (e->flags & EDGE_FAKE)
		    flag_bits |= 0x2;
		  if (e->flags & EDGE_FALLTHRU)
		    flag_bits |= 0x4;

		  __write_long (BB_TO_GCOV_INDEX (e->dest), bbg_file, 4);
		  __write_long (flag_bits, bbg_file, 4);
	        }
	    }
	}
      /* Emit fake loopback edge for EXIT block to maintain compatibility with
         old gcov format.  */
      __write_long (1, bbg_file, 4);
      __write_long (0, bbg_file, 4);
      __write_long (0x1, bbg_file, 4);

      /* Emit a -1 to separate the list of all edges from the list of
	 loop back edges that follows.  */
      __write_long (-1, bbg_file, 4);
    }

  if (flag_branch_probabilities)
    compute_branch_probabilities ();

  /* For each edge not on the spanning tree, add counting code as rtl.  */

  if (profile_arc_flag)
    {
      instrument_edges (el);
      allocate_reg_info (max_reg_num (), FALSE, FALSE);
    }

  remove_fake_edges ();
  /* Re-merge split basic blocks and the mess introduced by
     insert_insn_on_edge.  */
  cleanup_cfg (profile_arc_flag ? CLEANUP_EXPENSIVE : 0);
  if (rtl_dump_file)
    dump_flow_info (rtl_dump_file);

  free_aux_for_edges ();
  free_edge_list (el);
}

/* Union find algorithm implementation for the basic blocks using
   aux fields.  */

static basic_block
find_group (bb)
     basic_block bb;
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
union_groups (bb1, bb2)
     basic_block bb1, bb2;
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
find_spanning_tree (el)
     struct edge_list *el;
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
	   || e->dest == EXIT_BLOCK_PTR
	   )
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
      if ((EDGE_CRITICAL_P (e))
	  && !EDGE_INFO (e)->ignore
	  && (find_group (e->src) != find_group (e->dest)))
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
      if (find_group (e->src) != find_group (e->dest)
	  && !EDGE_INFO (e)->ignore)
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
init_branch_prob (filename)
  const char *filename;
{
  int len = strlen (filename);
  int i;

  if (flag_test_coverage)
    {
      char *data_file, *bbg_file_name;

      /* Open an output file for the basic block/line number map.  */
      data_file = (char *) alloca (len + 4);
      strcpy (data_file, filename);
      strcat (data_file, ".bb");
      if ((bb_file = fopen (data_file, "wb")) == 0)
	fatal_io_error ("can't open %s", data_file);

      /* Open an output file for the program flow graph.  */
      bbg_file_name = (char *) alloca (len + 5);
      strcpy (bbg_file_name, filename);
      strcat (bbg_file_name, ".bbg");
      if ((bbg_file = fopen (bbg_file_name, "wb")) == 0)
	fatal_io_error ("can't open %s", bbg_file_name);

      /* Initialize to zero, to ensure that the first file name will be
	 written to the .bb file.  */
      last_bb_file_name = 0;
    }

  da_file_name = (char *) xmalloc (len + 4);
  strcpy (da_file_name, filename);
  strcat (da_file_name, ".da");
  
  if (flag_branch_probabilities)
    {
      da_file = fopen (da_file_name, "rb");
      if (!da_file)
	warning ("file %s not found, execution counts assumed to be zero",
		 da_file_name);
    }

  if (profile_arc_flag)
    init_edge_profiler ();

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
end_branch_prob ()
{
  if (flag_test_coverage)
    {
      fclose (bb_file);
      fclose (bbg_file);
      unlink (da_file_name);
    }

  if (flag_branch_probabilities && da_file)
    fclose (da_file);

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

/* The label used by the edge profiling code.  */

static GTY(()) rtx profiler_label;

/* Initialize the profiler_label.  */

static void
init_edge_profiler ()
{
  /* Generate and save a copy of this so it can be shared.  */
  char buf[20];
  ASM_GENERATE_INTERNAL_LABEL (buf, "LPBX", 2);
  profiler_label = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (buf));
}

/* Output instructions as RTL to increment the edge execution count.  */

static rtx
gen_edge_profiler (edgeno)
     int edgeno;
{
  enum machine_mode mode = mode_for_size (GCOV_TYPE_SIZE, MODE_INT, 0);
  rtx mem_ref, tmp;
  rtx sequence;

  start_sequence ();

  tmp = force_reg (Pmode, profiler_label);
  tmp = plus_constant (tmp, GCOV_TYPE_SIZE / BITS_PER_UNIT * edgeno);
  mem_ref = validize_mem (gen_rtx_MEM (mode, tmp));

  set_mem_alias_set (mem_ref, new_alias_set ());

  tmp = expand_simple_binop (mode, PLUS, mem_ref, const1_rtx,
			     mem_ref, 0, OPTAB_WIDEN);

  if (tmp != mem_ref)
    emit_move_insn (copy_rtx (mem_ref), tmp);

  sequence = get_insns ();
  end_sequence ();
  return sequence;
}

/* Output code for a constructor that will invoke __bb_init_func, if
   this has not already been done.  */

void
output_func_start_profiler ()
{
  tree fnname, fndecl;
  char *name;
  char buf[20];
  const char *cfnname;
  rtx table_address;
  enum machine_mode mode = mode_for_size (GCOV_TYPE_SIZE, MODE_INT, 0);
  int save_flag_inline_functions = flag_inline_functions;

  /* It's either already been output, or we don't need it because we're
     not doing profile-edges.  */
  if (! need_func_profiler)
    return;

  need_func_profiler = 0;

  /* Synthesize a constructor function to invoke __bb_init_func with a
     pointer to this object file's profile block.  */

  /* Try and make a unique name given the "file function name".

     And no, I don't like this either.  */

  fnname = get_file_function_name ('I');
  cfnname = IDENTIFIER_POINTER (fnname);
  name = concat (cfnname, "GCOV", NULL);
  fnname = get_identifier (name);
  free (name);

  fndecl = build_decl (FUNCTION_DECL, fnname,
		       build_function_type (void_type_node, NULL_TREE));
  DECL_EXTERNAL (fndecl) = 0;

  /* It can be a static function as long as collect2 does not have
     to scan the object file to find its ctor/dtor routine.  */
  TREE_PUBLIC (fndecl) = ! targetm.have_ctors_dtors;

  TREE_USED (fndecl) = 1;

  DECL_RESULT (fndecl) = build_decl (RESULT_DECL, NULL_TREE, void_type_node);

  fndecl = (*lang_hooks.decls.pushdecl) (fndecl);
  rest_of_decl_compilation (fndecl, 0, 1, 0);
  announce_function (fndecl);
  current_function_decl = fndecl;
  DECL_INITIAL (fndecl) = error_mark_node;
  make_decl_rtl (fndecl, NULL);
  init_function_start (fndecl, input_filename, lineno);
  (*lang_hooks.decls.pushlevel) (0);
  expand_function_start (fndecl, 0);
  cfun->arc_profile = 0;

  /* Actually generate the code to call __bb_init_func.  */
  ASM_GENERATE_INTERNAL_LABEL (buf, "LPBX", 0);
  table_address = force_reg (Pmode,
			     gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (buf)));
  emit_library_call (gen_rtx_SYMBOL_REF (Pmode, "__bb_init_func"), LCT_NORMAL,
		     mode, 1, table_address, Pmode);

  expand_function_end (input_filename, lineno, 0);
  (*lang_hooks.decls.poplevel) (1, 0, 1);

  /* Since fndecl isn't in the list of globals, it would never be emitted
     when it's considered to be 'safe' for inlining, so turn off
     flag_inline_functions.  */
  flag_inline_functions = 0;

  rest_of_compilation (fndecl);

  /* Reset flag_inline_functions to its original value.  */
  flag_inline_functions = save_flag_inline_functions;

  if (! quiet_flag)
    fflush (asm_out_file);
  current_function_decl = NULL_TREE;

  if (targetm.have_ctors_dtors)
    (* targetm.asm_out.constructor) (XEXP (DECL_RTL (fndecl), 0),
				     DEFAULT_INIT_PRIORITY);
}

#include "gt-profile.h"
