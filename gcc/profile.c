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
#include "hashtab.h"

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

struct function_list
{
  struct function_list *next; 	/* next function */
  const char *name; 		/* function name */
  unsigned cfg_checksum;	/* function checksum */
  unsigned count_edges;	        /* number of intrumented edges  */
};

static struct function_list *functions_head = 0;
static struct function_list **functions_tail = &functions_head;

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
static char *bbg_file_name;

/* Name and file pointer of the input file for the arc count data.  */

static FILE *da_file;
static char *da_file_name;

/* The name of the count table. Used by the edge profiling code.  */
static GTY(()) rtx profiler_label;

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
static rtx gen_edge_profiler PARAMS ((int));
static void instrument_edges PARAMS ((struct edge_list *));
static void compute_branch_probabilities PARAMS ((void));
static hashval_t htab_counts_index_hash PARAMS ((const void *));
static int htab_counts_index_eq PARAMS ((const void *, const void *));
static void htab_counts_index_del PARAMS ((void *));
static void cleanup_counts_index PARAMS ((int));
static int index_counts_file PARAMS ((void));
static gcov_type * get_exec_counts PARAMS ((void));
static unsigned compute_checksum PARAMS ((void));
static basic_block find_group PARAMS ((basic_block));
static void union_groups PARAMS ((basic_block, basic_block));


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

struct section_reference
{
  long offset;
  int owns_summary;
  long *summary;
};

struct da_index_entry
{
  /* We hash by  */
  char *function_name;
  unsigned section;
  /* and store  */
  unsigned checksum;
  unsigned n_offsets;
  struct section_reference *offsets;
};

static hashval_t
htab_counts_index_hash (of)
     const void *of;
{
  const struct da_index_entry *entry = of;

  return htab_hash_string (entry->function_name) ^ entry->section;
}

static int
htab_counts_index_eq (of1, of2)
     const void *of1;
     const void *of2;
{
  const struct da_index_entry *entry1 = of1;
  const struct da_index_entry *entry2 = of2;

  return !strcmp (entry1->function_name, entry2->function_name)
	  && entry1->section == entry2->section;
}

static void
htab_counts_index_del (what)
     void *what;
{
  struct da_index_entry *entry = what;
  unsigned i;

  for (i = 0; i < entry->n_offsets; i++)
    {
      struct section_reference *act = entry->offsets + i;
      if (act->owns_summary)
	free (act->summary);
    }
  free (entry->function_name);
  free (entry->offsets);
  free (entry);
}

static char *counts_file_name;
static htab_t counts_file_index = NULL;

static void
cleanup_counts_index (close_file)
     int close_file;
{
  if (da_file && close_file)
    {
      fclose (da_file);
      da_file = NULL;
    }
  if (counts_file_name)
    free (counts_file_name);
  counts_file_name = NULL;
  if (counts_file_index)
    htab_delete (counts_file_index);
  counts_file_index = NULL;
}

static int
index_counts_file ()
{
  char *function_name_buffer = NULL;
  unsigned magic, version, ix, checksum;
  long *summary;

  if (!da_file)
    return 0;
  counts_file_index = htab_create (10, htab_counts_index_hash, htab_counts_index_eq, htab_counts_index_del);

  /* No .da file, no data.  */
  if (!da_file)
    return 0;

  /* Now index all profile sections.  */

  rewind (da_file);

  summary = NULL;

  if (gcov_read_unsigned (da_file, &magic) || magic != GCOV_DATA_MAGIC)
    {
      warning ("`%s' is not a gcov data file", da_file_name);
      goto cleanup;
    }
  if (gcov_read_unsigned (da_file, &version) || version != GCOV_VERSION)
    {
      char v[4], e[4];
      magic = GCOV_VERSION;
      
      for (ix = 4; ix--; magic >>= 8, version >>= 8)
	{
	  v[ix] = version;
	  e[ix] = magic;
	}
      warning ("`%s' is version `%.4s', expected version `%.4s'",
	       da_file_name, v, e);
      goto cleanup;
    }
  
  while (1)
    {
      unsigned tag, length;
      long offset;
      
      offset = gcov_save_position (da_file);
      if (gcov_read_unsigned (da_file, &tag)
	  || gcov_read_unsigned (da_file, &length))
	{
	  if (feof (da_file))
	    break;
	corrupt:;
	  warning ("`%s' is corrupted", da_file_name);
	  goto cleanup;
	}
      if (tag == GCOV_TAG_FUNCTION)
	{
	  if (gcov_read_string (da_file, &function_name_buffer, NULL)
	      || gcov_read_unsigned (da_file, &checksum))
	    goto corrupt;
	  continue;
	}
      if (tag == GCOV_TAG_PROGRAM_SUMMARY)
	{
	  if (length != GCOV_SUMMARY_LENGTH)
	    goto corrupt;

	  if (summary)
	    *summary = offset;
	  summary = NULL;
	}
      else
	{
	  if (function_name_buffer)
	    {
	      struct da_index_entry **slot, elt;
	      elt.function_name = function_name_buffer;
	      elt.section = tag;

	      slot = (struct da_index_entry **)
		htab_find_slot (counts_file_index, &elt, INSERT);
	      if (*slot)
		{
		  if ((*slot)->checksum != checksum)
		    {
		      warning ("profile mismatch for `%s'", function_name_buffer);
		      goto cleanup;
		    }
		  (*slot)->n_offsets++;
		  (*slot)->offsets = xrealloc ((*slot)->offsets,
					       sizeof (struct section_reference) * (*slot)->n_offsets);
		}
	      else
		{
		  *slot = xmalloc (sizeof (struct da_index_entry));
		  (*slot)->function_name = xstrdup (function_name_buffer);
		  (*slot)->section = tag;
		  (*slot)->checksum = checksum;
		  (*slot)->n_offsets = 1;
		  (*slot)->offsets = xmalloc (sizeof (struct section_reference));
		}
	      (*slot)->offsets[(*slot)->n_offsets - 1].offset = offset;
	      if (summary)
		(*slot)->offsets[(*slot)->n_offsets - 1].owns_summary = 0;
	      else
		{
		  summary = xmalloc (sizeof (long));
		  *summary = -1;
		  (*slot)->offsets[(*slot)->n_offsets - 1].owns_summary = 1;
		}
	      (*slot)->offsets[(*slot)->n_offsets - 1].summary = summary;
	    }
	}
      if (gcov_skip (da_file, length))
	goto corrupt;
    }

  free (function_name_buffer);

  return 1;

cleanup:
  cleanup_counts_index (1);
  if (function_name_buffer)
    free (function_name_buffer);
  return 0;
}

/* Computes hybrid profile for all matching entries in da_file.
   Sets max_counter_in_program as a side effect.  */

static gcov_type *
get_exec_counts ()
{
  unsigned num_edges = 0;
  basic_block bb;
  gcov_type *profile;
  gcov_type max_count;
  unsigned ix, i, tag, length, num;
  const char *name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (current_function_decl));
  struct da_index_entry *entry, what;
  struct section_reference *act;
  gcov_type count;
  struct gcov_summary summ;

  profile_info.max_counter_in_program = 0;
  profile_info.count_profiles_merged = 0;

  /* No .da file, no execution counts.  */
  if (!da_file)
    return NULL;
  if (!counts_file_index)
    abort ();

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

  for (ix = 0; ix < num_edges; ix++)
    profile[ix] = 0;

  what.function_name = (char *) name;
  what.section = GCOV_TAG_ARC_COUNTS;
  entry = htab_find (counts_file_index, &what);
  if (!entry)
    {
      warning ("No profile for function '%s' found.", name);
      goto cleanup;
    }
  
  if (entry->checksum != profile_info.current_function_cfg_checksum)
    {
      warning ("profile mismatch for `%s'", current_function_name);
      goto cleanup;
    }

  for (i = 0; i < entry->n_offsets; i++)
    {
      act = entry->offsets + i;

      /* Read arc counters.  */
      max_count = 0;
      gcov_resync (da_file, act->offset, 0);

      if (gcov_read_unsigned (da_file, &tag)
	  || gcov_read_unsigned (da_file, &length)
	  || tag != GCOV_TAG_ARC_COUNTS)
	{
	  /* We have already passed through file, so any error means
	     something is rotten.  */
	  abort ();
	}
      num = length / 8;

      if (num != num_edges)
	{
	  warning ("profile mismatch for `%s'", current_function_name);
	  goto cleanup;
	}
	  
      for (ix = 0; ix != num; ix++)
	{
	  if (gcov_read_counter (da_file, &count))
	    abort ();
	  if (count > max_count)
	    max_count = count;
	  profile[ix] += count;
	}

      /* Read program summary.  */
      if (*act->summary != -1)
	{
	  gcov_resync (da_file, *act->summary, 0);
	  if (gcov_read_unsigned (da_file, &tag)
	      || gcov_read_unsigned (da_file, &length)
	      || tag != GCOV_TAG_PROGRAM_SUMMARY
	      || gcov_read_summary (da_file, &summ))
	    abort ();
	  profile_info.count_profiles_merged += summ.runs;
	  profile_info.max_counter_in_program += summ.arc_sum_max;
	}
      else
	summ.runs = 0;
      if (!summ.runs)
	{
	  profile_info.count_profiles_merged++;
	  profile_info.max_counter_in_program += max_count;
	}
    }

  if (rtl_dump_file)
    {
      fprintf(rtl_dump_file, "Merged %i profiles with maximal count %i.\n",
	      profile_info.count_profiles_merged,
	      (int)profile_info.max_counter_in_program);
    }

  return profile;

cleanup:;
  free (profile);
  cleanup_counts_index (1);
  return NULL;
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
      /* Otherwise distribute the probabilities evenly so we get sane
	 sum.  Use simple heuristics that if there are normal edges,
	 give all abnormals frequency of 0, otherwise distribute the
	 frequency over abnormals (this is the case of noreturn
	 calls).  */
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

/* Compute checksum for the current function.  We generate a CRC32.  */

static unsigned
compute_checksum ()
{
  unsigned chksum = 0;
  basic_block bb;
  
  FOR_EACH_BB (bb)
    {
      edge e = NULL;
      
      do
	{
	  unsigned value = BB_TO_GCOV_INDEX (e ? e->dest : bb);
	  unsigned ix;

	  /* No need to use all bits in value identically, nearly all
	     functions have less than 256 blocks.  */
	  value ^= value << 16;
	  value ^= value << 8;
	  
	  for (ix = 8; ix--; value <<= 1)
	    {
	      unsigned feedback;

	      feedback = (value ^ chksum) & 0x80000000 ? 0x04c11db7 : 0;
	      chksum <<= 1;
	      chksum ^= feedback;
	    }
	  
	  e = e ? e->succ_next : bb->succ;
	}
      while (e);
    }

  return chksum;
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
    fprintf (rtl_dump_file, "CFG checksum is %u\n",
	profile_info.current_function_cfg_checksum);

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

  if (flag_test_coverage && bbg_file)
    {
      long offset;
      
      /* Announce function */
      if (gcov_write_unsigned (bbg_file, GCOV_TAG_FUNCTION)
	  || !(offset = gcov_reserve_length (bbg_file))
	  || gcov_write_string (bbg_file, name,
			     strlen (name))
	  || gcov_write_unsigned (bbg_file,
			    profile_info.current_function_cfg_checksum)
	  || gcov_write_length (bbg_file, offset))
	goto bbg_error;

      /* Basic block flags */
      if (gcov_write_unsigned (bbg_file, GCOV_TAG_BLOCKS)
	  || !(offset = gcov_reserve_length (bbg_file)))
	goto bbg_error;
      for (i = 0; i != n_basic_blocks + 2; i++)
	if (gcov_write_unsigned (bbg_file, 0))
	  goto bbg_error;
      if (gcov_write_length (bbg_file, offset))
	goto bbg_error;
      
      /* Arcs */
      FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, EXIT_BLOCK_PTR, next_bb)
	{
	  edge e;

	  if (gcov_write_unsigned (bbg_file, GCOV_TAG_ARCS)
	      || !(offset = gcov_reserve_length (bbg_file))
	      || gcov_write_unsigned (bbg_file, BB_TO_GCOV_INDEX (bb)))
	    goto bbg_error;

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

		  if (gcov_write_unsigned (bbg_file,
					   BB_TO_GCOV_INDEX (e->dest))
		      || gcov_write_unsigned (bbg_file, flag_bits))
		    goto bbg_error;
	        }
	    }
	  if (gcov_write_length (bbg_file, offset))
	    goto bbg_error;
	}

      /* Output line number information about each basic block for
     	 GCOV utility.  */
      {
	char const *prev_file_name = NULL;
	
	FOR_EACH_BB (bb)
	  {
	    rtx insn = bb->head;
	    int ignore_next_note = 0;
	    
	    offset = 0;
	    
	    /* We are looking for line number notes.  Search backward
	       before basic block to find correct ones.  */
	    insn = prev_nonnote_insn (insn);
	    if (!insn)
	      insn = get_insns ();
	    else
	      insn = NEXT_INSN (insn);

	    while (insn != bb->end)
	      {
		if (GET_CODE (insn) == NOTE)
		  {
		     /* Must ignore the line number notes that immediately
		     	follow the end of an inline function to avoid counting
		     	it twice.  There is a note before the call, and one
		     	after the call.  */
		    if (NOTE_LINE_NUMBER (insn)
			== NOTE_INSN_REPEATED_LINE_NUMBER)
		      ignore_next_note = 1;
		    else if (NOTE_LINE_NUMBER (insn) <= 0)
		      /*NOP*/;
		    else if (ignore_next_note)
		      ignore_next_note = 0;
		    else
		      {
			if (offset)
			  /*NOP*/;
			else if (gcov_write_unsigned (bbg_file, GCOV_TAG_LINES)
				 || !(offset = gcov_reserve_length (bbg_file))
				 || gcov_write_unsigned (bbg_file,
						   BB_TO_GCOV_INDEX (bb)))
			  goto bbg_error;
			/* If this is a new source file, then output
			   the file's name to the .bb file.  */
			if (!prev_file_name
			    || strcmp (NOTE_SOURCE_FILE (insn),
				       prev_file_name))
			  {
			    prev_file_name = NOTE_SOURCE_FILE (insn);
			    if (gcov_write_unsigned (bbg_file, 0)
				|| gcov_write_string (bbg_file, prev_file_name,
						      strlen (prev_file_name)))
			      goto bbg_error;
			  }
			if (gcov_write_unsigned (bbg_file, NOTE_LINE_NUMBER (insn)))
			  goto bbg_error;
		      }
		  }
		insn = NEXT_INSN (insn);
	      }
	    if (offset)
	      {
		if (gcov_write_unsigned (bbg_file, 0)
		    || gcov_write_string (bbg_file, NULL, 0)
		    || gcov_write_length (bbg_file, offset))
		  {
		  bbg_error:;
		    warning ("error writing `%s'", bbg_file_name);
		    fclose (bbg_file);
		    bbg_file = NULL;
		  }
	      }
	  }
      }
    }

  if (flag_branch_probabilities)
    compute_branch_probabilities ();

  /* For each edge not on the spanning tree, add counting code as rtl.  */

  if (cfun->arc_profile && profile_arc_flag)
    {
      struct function_list *item;
      
      instrument_edges (el);
      allocate_reg_info (max_reg_num (), FALSE, FALSE);

      /* ??? Probably should re-use the existing struct function.  */
      item = xmalloc (sizeof (struct function_list));
      
      *functions_tail = item;
      functions_tail = &item->next;
      
      item->next = 0;
      item->name = xstrdup (name);
      item->cfg_checksum = profile_info.current_function_cfg_checksum;
      item->count_edges = profile_info.count_edges_instrumented_now;
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
      /* Open the bbg output file.  */
      bbg_file_name = (char *) xmalloc (len + strlen (GCOV_GRAPH_SUFFIX) + 1);
      strcpy (bbg_file_name, filename);
      strcat (bbg_file_name, GCOV_GRAPH_SUFFIX);
      bbg_file = fopen (bbg_file_name, "wb");
      if (!bbg_file)
	fatal_io_error ("cannot open %s", bbg_file_name);

      if (gcov_write_unsigned (bbg_file, GCOV_GRAPH_MAGIC)
	  || gcov_write_unsigned (bbg_file, GCOV_VERSION))
	{
	  fclose (bbg_file);
	  fatal_io_error ("cannot write `%s'", bbg_file_name);
	}
    }

  da_file_name = (char *) xmalloc (len + strlen (GCOV_DATA_SUFFIX) + 1);
  strcpy (da_file_name, filename);
  strcat (da_file_name, GCOV_DATA_SUFFIX);
  
  if (flag_branch_probabilities)
    {
      da_file = fopen (da_file_name, "rb");
      if (!da_file)
	warning ("file %s not found, execution counts assumed to be zero",
		 da_file_name);
      if (counts_file_index && strcmp (da_file_name, counts_file_name))
       	cleanup_counts_index (0);
      if (index_counts_file ())
	counts_file_name = xstrdup (da_file_name);
    }

  if (profile_arc_flag)
    {
      /* Generate and save a copy of this so it can be shared.  */
      char buf[20];
      
      ASM_GENERATE_INTERNAL_LABEL (buf, "LPBX", 2);
      profiler_label = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (buf));
    }
  
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
      if (bbg_file)
	{
#if __GNUC__ && !CROSS_COMPILE && SUPPORTS_WEAK
	  /* If __gcov_init has a value in the compiler, it means we
	     are instrumenting ourselves. We should not remove the
	     counts file, because we might be recompiling
	     ourselves. The .da files are all removed during copying
	     the stage1 files.  */
	  extern void __gcov_init (void *)
	    __attribute__ ((weak));
	  
	  if (!__gcov_init)
	    unlink (da_file_name);
#else
	  unlink (da_file_name);
#endif
	  fclose (bbg_file);
	}
      else
	{
	  unlink (bbg_file_name);
	  unlink (da_file_name);
	}
    }

  if (da_file)
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

/* Write out the structure which libgcc uses to locate all the arc
   counters.  The structures used here must match those defined in
   gcov-io.h.  Write out the constructor to call __gcov_init.  */

void
create_profiler ()
{
  tree fields, field, value = NULL_TREE;
  tree ginfo_type;
  tree string_type;
  tree gcov_type, gcov_ptr_type;
  char name[20];
  char *ctor_name;
  tree structure, ctor;
  rtx structure_address;
  int save_flag_inline_functions = flag_inline_functions;

  if (!profile_info.count_instrumented_edges)
    return;
  
  string_type = build_pointer_type
    (build_qualified_type (char_type_node,  TYPE_QUAL_CONST));
  gcov_type = make_signed_type (GCOV_TYPE_SIZE);
  gcov_ptr_type
    = build_pointer_type (build_qualified_type
			  (gcov_type, TYPE_QUAL_CONST));
  
  ginfo_type = (*lang_hooks.types.make_type) (RECORD_TYPE);
  

  /* Version ident */
  fields = build_decl (FIELD_DECL, NULL_TREE, long_unsigned_type_node);
  value = tree_cons (fields, convert (long_unsigned_type_node, build_int_2
				      (GCOV_VERSION, 0)), value);
      
  /* NULL */
  field = build_decl (FIELD_DECL, NULL_TREE, build_pointer_type
		      (build_qualified_type
		       (ginfo_type, TYPE_QUAL_CONST)));
  TREE_CHAIN (field) = fields;
  fields = field;
  value = tree_cons (fields, null_pointer_node, value);
  
  /* Filename */
  {
    tree filename_string;
    char *filename;
    int filename_len;
    
    filename = getpwd ();
    filename = (filename && da_file_name[0] != '/'
		? concat (filename, "/", da_file_name, NULL)
		: da_file_name);
    filename_len = strlen (filename);
    filename_string = build_string (filename_len + 1, filename);
    if (filename != da_file_name)
      free (filename);
    TREE_TYPE (filename_string) = build_array_type
      (char_type_node, build_index_type
       (build_int_2 (filename_len, 0)));
    
    field = build_decl (FIELD_DECL, NULL_TREE, string_type);
    TREE_CHAIN (field) = fields;
    fields = field;
    value = tree_cons (fields, build1 (ADDR_EXPR, string_type,
				       filename_string), value);
  }
  
  /* Workspace */
  field = build_decl (FIELD_DECL, NULL_TREE, long_integer_type_node);
  TREE_CHAIN (field) = fields;
  fields = field;
  value = tree_cons (fields,
		     convert (long_integer_type_node, integer_zero_node),
		     value);
      
  /* function_info table */
  {
    struct function_list *item;
    int num_nodes = 0;
    tree array_value = NULL_TREE;
    tree finfo_type, finfo_ptr_type;
    tree name, checksum, arcs;
    
    finfo_type = (*lang_hooks.types.make_type) (RECORD_TYPE);
    name = build_decl (FIELD_DECL, NULL_TREE, string_type);
    checksum = build_decl (FIELD_DECL, NULL_TREE, unsigned_type_node);
    TREE_CHAIN (checksum) = name;
    arcs = build_decl (FIELD_DECL, NULL_TREE, unsigned_type_node);
    TREE_CHAIN (arcs) = checksum;
    finish_builtin_struct (finfo_type, "__function_info",
			   arcs, NULL_TREE);
    finfo_ptr_type = build_pointer_type
      (build_qualified_type (finfo_type, TYPE_QUAL_CONST));
    
    for (item = functions_head; item != 0; item = item->next, num_nodes++)
      {
	size_t name_len = strlen (item->name);
	tree finfo_value = NULL_TREE;
	tree fname = build_string (name_len + 1, item->name);
	
	TREE_TYPE (fname) = build_array_type
	  (char_type_node, build_index_type (build_int_2 (name_len, 0)));
	finfo_value = tree_cons (name, build1
				 (ADDR_EXPR, string_type,
				  fname), finfo_value);
	finfo_value = tree_cons (checksum, convert
				 (unsigned_type_node,
				  build_int_2 (item->cfg_checksum, 0)),
				 finfo_value);
	finfo_value = tree_cons (arcs, convert
				 (unsigned_type_node,
				  build_int_2 (item->count_edges, 0)),
				 finfo_value);
	array_value = tree_cons (NULL_TREE, build
				 (CONSTRUCTOR, finfo_type, NULL_TREE,
				  nreverse (finfo_value)), array_value);
      }

    /* Create constructor for array.  */
    if (num_nodes)
      {
	tree array_type;

	array_type = build_array_type (finfo_type, build_index_type
				       (build_int_2 (num_nodes - 1, 0)));
	array_value = build (CONSTRUCTOR, array_type,
			     NULL_TREE, nreverse (array_value));
	array_value = build1
	  (ADDR_EXPR, finfo_ptr_type, array_value);
      }
    else
      array_value = null_pointer_node;
    
    field = build_decl (FIELD_DECL, NULL_TREE, finfo_ptr_type);
    TREE_CHAIN (field) = fields;
    fields = field;
    value = tree_cons (fields, array_value, value);
    
    /* number of functions */
    field = build_decl (FIELD_DECL, NULL_TREE, unsigned_type_node);
    TREE_CHAIN (field) = fields;
    fields = field;
    value = tree_cons (fields, convert (unsigned_type_node, build_int_2
					(num_nodes, 0)), value);
  }
  
  /* arc count table */
  {
    tree counts_table = null_pointer_node;
    
    if (profile_info.count_instrumented_edges)
      {
	tree gcov_type_array_type
	  = build_array_type (gcov_type, build_index_type
			      (build_int_2 (profile_info.
					    count_instrumented_edges - 1, 0)));
	/* No values.  */
	counts_table
	  = build (VAR_DECL, gcov_type_array_type, NULL_TREE, NULL_TREE);
	TREE_STATIC (counts_table) = 1;
	DECL_NAME (counts_table) = get_identifier (XSTR (profiler_label, 0));
	assemble_variable (counts_table, 0, 0, 0);
	counts_table = build1 (ADDR_EXPR, gcov_ptr_type, counts_table);
      }
    
    field = build_decl (FIELD_DECL, NULL_TREE, gcov_ptr_type);
    TREE_CHAIN (field) = fields;
    fields = field;
    value = tree_cons (fields, counts_table, value);
  }
  
  /* number of arc counts */
  field = build_decl (FIELD_DECL, NULL_TREE, unsigned_type_node);
  TREE_CHAIN (field) = fields;
  fields = field;
  value = tree_cons (fields, convert
		     (unsigned_type_node,
		      build_int_2 (profile_info
				   .count_instrumented_edges, 0)),
		     value);
  
  finish_builtin_struct (ginfo_type, "__gcov_info", fields, NULL_TREE);
  structure = build (VAR_DECL, ginfo_type, NULL_TREE, NULL_TREE);
  DECL_INITIAL (structure)
    = build (CONSTRUCTOR, ginfo_type, NULL_TREE, nreverse (value));
  TREE_STATIC (structure) = 1;
  ASM_GENERATE_INTERNAL_LABEL (name, "LPBX", 0);
  DECL_NAME (structure) = get_identifier (name);
  
  /* Build structure.  */
  assemble_variable (structure, 0, 0, 0);

  /* Build the constructor function to invoke __gcov_init. */
  ctor_name = concat (IDENTIFIER_POINTER (get_file_function_name ('I')),
		      "_GCOV", NULL);
  ctor = build_decl (FUNCTION_DECL, get_identifier (ctor_name),
		     build_function_type (void_type_node, NULL_TREE));
  free (ctor_name);
  DECL_EXTERNAL (ctor) = 0;

  /* It can be a static function as long as collect2 does not have
     to scan the object file to find its ctor/dtor routine.  */
  TREE_PUBLIC (ctor) = ! targetm.have_ctors_dtors;
  TREE_USED (ctor) = 1;
  DECL_RESULT (ctor) = build_decl (RESULT_DECL, NULL_TREE, void_type_node);

  ctor = (*lang_hooks.decls.pushdecl) (ctor);
  rest_of_decl_compilation (ctor, 0, 1, 0);
  announce_function (ctor);
  current_function_decl = ctor;
  DECL_INITIAL (ctor) = error_mark_node;
  make_decl_rtl (ctor, NULL);
  init_function_start (ctor, input_filename, lineno);
  (*lang_hooks.decls.pushlevel) (0);
  expand_function_start (ctor, 0);
  cfun->arc_profile = 0;

  /* Actually generate the code to call __gcov_init.  */
  structure_address = force_reg (Pmode, gen_rtx_SYMBOL_REF
				 (Pmode, IDENTIFIER_POINTER
				  (DECL_NAME (structure))));
  emit_library_call (gen_rtx_SYMBOL_REF (Pmode, "__gcov_init"),
		     LCT_NORMAL, VOIDmode, 1,
		     structure_address, Pmode);

  expand_function_end (input_filename, lineno, 0);
  (*lang_hooks.decls.poplevel) (1, 0, 1);

  /* Since ctor isn't in the list of globals, it would never be emitted
     when it's considered to be 'safe' for inlining, so turn off
     flag_inline_functions.  */
  flag_inline_functions = 0;

  rest_of_compilation (ctor);

  /* Reset flag_inline_functions to its original value.  */
  flag_inline_functions = save_flag_inline_functions;

  if (! quiet_flag)
    fflush (asm_out_file);
  current_function_decl = NULL_TREE;

  if (targetm.have_ctors_dtors)
    (* targetm.asm_out.constructor) (XEXP (DECL_RTL (ctor), 0),
				     DEFAULT_INIT_PRIORITY);
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

#include "gt-profile.h"
