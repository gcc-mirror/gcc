/* Control flow graph manipulation code for GNU compiler.
   Copyright (C) 1987-2016 Free Software Foundation, Inc.

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

/* This file contains low level functions to manipulate the CFG and
   analyze it.  All other modules should not transform the data structure
   directly and use abstraction instead.  The file is supposed to be
   ordered bottom-up and should not contain any code dependent on a
   particular intermediate language (RTL or trees).

   Available functionality:
     - Initialization/deallocation
	 init_flow, clear_edges
     - Low level basic block manipulation
	 alloc_block, expunge_block
     - Edge manipulation
	 make_edge, make_single_succ_edge, cached_make_edge, remove_edge
	 - Low level edge redirection (without updating instruction chain)
	     redirect_edge_succ, redirect_edge_succ_nodup, redirect_edge_pred
     - Dumping and debugging
	 dump_flow_info, debug_flow_info, dump_edge_info
     - Allocation of AUX fields for basic blocks
	 alloc_aux_for_blocks, free_aux_for_blocks, alloc_aux_for_block
     - clear_bb_flags
     - Consistency checking
	 verify_flow_info
     - Dumping and debugging
	 print_rtl_with_bb, dump_bb, debug_bb, debug_bb_n

   TODO: Document these "Available functionality" functions in the files
   that implement them.
 */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "hard-reg-set.h"
#include "tree.h"
#include "cfghooks.h"
#include "df.h"
#include "cfganal.h"
#include "cfgloop.h" /* FIXME: For struct loop.  */
#include "dumpfile.h"


#define RDIV(X,Y) (((X) + (Y) / 2) / (Y))

/* Called once at initialization time.  */

void
init_flow (struct function *the_fun)
{
  if (!the_fun->cfg)
    the_fun->cfg = ggc_cleared_alloc<control_flow_graph> ();
  n_edges_for_fn (the_fun) = 0;
  ENTRY_BLOCK_PTR_FOR_FN (the_fun)
    = ggc_cleared_alloc<basic_block_def> ();
  ENTRY_BLOCK_PTR_FOR_FN (the_fun)->index = ENTRY_BLOCK;
  EXIT_BLOCK_PTR_FOR_FN (the_fun)
    = ggc_cleared_alloc<basic_block_def> ();
  EXIT_BLOCK_PTR_FOR_FN (the_fun)->index = EXIT_BLOCK;
  ENTRY_BLOCK_PTR_FOR_FN (the_fun)->next_bb
    = EXIT_BLOCK_PTR_FOR_FN (the_fun);
  EXIT_BLOCK_PTR_FOR_FN (the_fun)->prev_bb
    = ENTRY_BLOCK_PTR_FOR_FN (the_fun);
}

/* Helper function for remove_edge and clear_edges.  Frees edge structure
   without actually removing it from the pred/succ arrays.  */

static void
free_edge (function *fn, edge e)
{
  n_edges_for_fn (fn)--;
  ggc_free (e);
}

/* Free the memory associated with the edge structures.  */

void
clear_edges (struct function *fn)
{
  basic_block bb;
  edge e;
  edge_iterator ei;

  FOR_EACH_BB_FN (bb, fn)
    {
      FOR_EACH_EDGE (e, ei, bb->succs)
	free_edge (fn, e);
      vec_safe_truncate (bb->succs, 0);
      vec_safe_truncate (bb->preds, 0);
    }

  FOR_EACH_EDGE (e, ei, ENTRY_BLOCK_PTR_FOR_FN (fn)->succs)
    free_edge (fn, e);
  vec_safe_truncate (EXIT_BLOCK_PTR_FOR_FN (fn)->preds, 0);
  vec_safe_truncate (ENTRY_BLOCK_PTR_FOR_FN (fn)->succs, 0);

  gcc_assert (!n_edges_for_fn (fn));
}

/* Allocate memory for basic_block.  */

basic_block
alloc_block (void)
{
  basic_block bb;
  bb = ggc_cleared_alloc<basic_block_def> ();
  return bb;
}

/* Link block B to chain after AFTER.  */
void
link_block (basic_block b, basic_block after)
{
  b->next_bb = after->next_bb;
  b->prev_bb = after;
  after->next_bb = b;
  b->next_bb->prev_bb = b;
}

/* Unlink block B from chain.  */
void
unlink_block (basic_block b)
{
  b->next_bb->prev_bb = b->prev_bb;
  b->prev_bb->next_bb = b->next_bb;
  b->prev_bb = NULL;
  b->next_bb = NULL;
}

/* Sequentially order blocks and compact the arrays.  */
void
compact_blocks (void)
{
  int i;

  SET_BASIC_BLOCK_FOR_FN (cfun, ENTRY_BLOCK, ENTRY_BLOCK_PTR_FOR_FN (cfun));
  SET_BASIC_BLOCK_FOR_FN (cfun, EXIT_BLOCK, EXIT_BLOCK_PTR_FOR_FN (cfun));

  if (df)
    df_compact_blocks ();
  else
    {
      basic_block bb;

      i = NUM_FIXED_BLOCKS;
      FOR_EACH_BB_FN (bb, cfun)
	{
	  SET_BASIC_BLOCK_FOR_FN (cfun, i, bb);
	  bb->index = i;
	  i++;
	}
      gcc_assert (i == n_basic_blocks_for_fn (cfun));

      for (; i < last_basic_block_for_fn (cfun); i++)
	SET_BASIC_BLOCK_FOR_FN (cfun, i, NULL);
    }
  last_basic_block_for_fn (cfun) = n_basic_blocks_for_fn (cfun);
}

/* Remove block B from the basic block array.  */

void
expunge_block (basic_block b)
{
  unlink_block (b);
  SET_BASIC_BLOCK_FOR_FN (cfun, b->index, NULL);
  n_basic_blocks_for_fn (cfun)--;
  /* We should be able to ggc_free here, but we are not.
     The dead SSA_NAMES are left pointing to dead statements that are pointing
     to dead basic blocks making garbage collector to die.
     We should be able to release all dead SSA_NAMES and at the same time we should
     clear out BB pointer of dead statements consistently.  */
}

/* Connect E to E->src.  */

static inline void
connect_src (edge e)
{
  vec_safe_push (e->src->succs, e);
  df_mark_solutions_dirty ();
}

/* Connect E to E->dest.  */

static inline void
connect_dest (edge e)
{
  basic_block dest = e->dest;
  vec_safe_push (dest->preds, e);
  e->dest_idx = EDGE_COUNT (dest->preds) - 1;
  df_mark_solutions_dirty ();
}

/* Disconnect edge E from E->src.  */

static inline void
disconnect_src (edge e)
{
  basic_block src = e->src;
  edge_iterator ei;
  edge tmp;

  for (ei = ei_start (src->succs); (tmp = ei_safe_edge (ei)); )
    {
      if (tmp == e)
	{
	  src->succs->unordered_remove (ei.index);
	  df_mark_solutions_dirty ();
	  return;
	}
      else
	ei_next (&ei);
    }

  gcc_unreachable ();
}

/* Disconnect edge E from E->dest.  */

static inline void
disconnect_dest (edge e)
{
  basic_block dest = e->dest;
  unsigned int dest_idx = e->dest_idx;

  dest->preds->unordered_remove (dest_idx);

  /* If we removed an edge in the middle of the edge vector, we need
     to update dest_idx of the edge that moved into the "hole".  */
  if (dest_idx < EDGE_COUNT (dest->preds))
    EDGE_PRED (dest, dest_idx)->dest_idx = dest_idx;
  df_mark_solutions_dirty ();
}

/* Create an edge connecting SRC and DEST with flags FLAGS.  Return newly
   created edge.  Use this only if you are sure that this edge can't
   possibly already exist.  */

edge
unchecked_make_edge (basic_block src, basic_block dst, int flags)
{
  edge e;
  e = ggc_cleared_alloc<edge_def> ();
  n_edges_for_fn (cfun)++;

  e->src = src;
  e->dest = dst;
  e->flags = flags;

  connect_src (e);
  connect_dest (e);

  execute_on_growing_pred (e);
  return e;
}

/* Create an edge connecting SRC and DST with FLAGS optionally using
   edge cache CACHE.  Return the new edge, NULL if already exist.  */

edge
cached_make_edge (sbitmap edge_cache, basic_block src, basic_block dst, int flags)
{
  if (edge_cache == NULL
      || src == ENTRY_BLOCK_PTR_FOR_FN (cfun)
      || dst == EXIT_BLOCK_PTR_FOR_FN (cfun))
    return make_edge (src, dst, flags);

  /* Does the requested edge already exist?  */
  if (! bitmap_bit_p (edge_cache, dst->index))
    {
      /* The edge does not exist.  Create one and update the
	 cache.  */
      bitmap_set_bit (edge_cache, dst->index);
      return unchecked_make_edge (src, dst, flags);
    }

  /* At this point, we know that the requested edge exists.  Adjust
     flags if necessary.  */
  if (flags)
    {
      edge e = find_edge (src, dst);
      e->flags |= flags;
    }

  return NULL;
}

/* Create an edge connecting SRC and DEST with flags FLAGS.  Return newly
   created edge or NULL if already exist.  */

edge
make_edge (basic_block src, basic_block dest, int flags)
{
  edge e = find_edge (src, dest);

  /* Make sure we don't add duplicate edges.  */
  if (e)
    {
      e->flags |= flags;
      return NULL;
    }

  return unchecked_make_edge (src, dest, flags);
}

/* Create an edge connecting SRC to DEST and set probability by knowing
   that it is the single edge leaving SRC.  */

edge
make_single_succ_edge (basic_block src, basic_block dest, int flags)
{
  edge e = make_edge (src, dest, flags);

  e->probability = REG_BR_PROB_BASE;
  e->count = src->count;
  return e;
}

/* This function will remove an edge from the flow graph.  */

void
remove_edge_raw (edge e)
{
  remove_predictions_associated_with_edge (e);
  execute_on_shrinking_pred (e);

  disconnect_src (e);
  disconnect_dest (e);

  free_edge (cfun, e);
}

/* Redirect an edge's successor from one block to another.  */

void
redirect_edge_succ (edge e, basic_block new_succ)
{
  execute_on_shrinking_pred (e);

  disconnect_dest (e);

  e->dest = new_succ;

  /* Reconnect the edge to the new successor block.  */
  connect_dest (e);

  execute_on_growing_pred (e);
}

/* Redirect an edge's predecessor from one block to another.  */

void
redirect_edge_pred (edge e, basic_block new_pred)
{
  disconnect_src (e);

  e->src = new_pred;

  /* Reconnect the edge to the new predecessor block.  */
  connect_src (e);
}

/* Clear all basic block flags that do not have to be preserved.  */
void
clear_bb_flags (void)
{
  basic_block bb;

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR_FOR_FN (cfun), NULL, next_bb)
    bb->flags &= BB_FLAGS_TO_PRESERVE;
}

/* Check the consistency of profile information.  We can't do that
   in verify_flow_info, as the counts may get invalid for incompletely
   solved graphs, later eliminating of conditionals or roundoff errors.
   It is still practical to have them reported for debugging of simple
   testcases.  */
static void
check_bb_profile (basic_block bb, FILE * file, int indent, int flags)
{
  edge e;
  int sum = 0;
  gcov_type lsum;
  edge_iterator ei;
  struct function *fun = DECL_STRUCT_FUNCTION (current_function_decl);
  char *s_indent = (char *) alloca ((size_t) indent + 1);
  memset ((void *) s_indent, ' ', (size_t) indent);
  s_indent[indent] = '\0';

  if (profile_status_for_fn (fun) == PROFILE_ABSENT)
    return;

  if (bb != EXIT_BLOCK_PTR_FOR_FN (fun))
    {
      bool found = false;
      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  if (!(e->flags & EDGE_EH))
	    found = true;
	  sum += e->probability;
	}
      /* Only report mismatches for non-EH control flow. If there are only EH
	 edges it means that the BB ends by noreturn call.  Here the control
	 flow may just terminate.  */
      if (found)
	{
	  if (EDGE_COUNT (bb->succs) && abs (sum - REG_BR_PROB_BASE) > 100)
	    fprintf (file, "%s%sInvalid sum of outgoing probabilities %.1f%%\n",
		     (flags & TDF_COMMENT) ? ";; " : "", s_indent,
		     sum * 100.0 / REG_BR_PROB_BASE);
	  lsum = 0;
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    lsum += e->count;
	  if (EDGE_COUNT (bb->succs)
	      && (lsum - bb->count > 100 || lsum - bb->count < -100))
	    fprintf (file, "%s%sInvalid sum of outgoing counts %i, should be %i\n",
		     (flags & TDF_COMMENT) ? ";; " : "", s_indent,
		     (int) lsum, (int) bb->count);
	}
    }
    if (bb != ENTRY_BLOCK_PTR_FOR_FN (fun))
    {
      sum = 0;
      FOR_EACH_EDGE (e, ei, bb->preds)
	sum += EDGE_FREQUENCY (e);
      if (abs (sum - bb->frequency) > 100)
	fprintf (file,
		 "%s%sInvalid sum of incoming frequencies %i, should be %i\n",
		 (flags & TDF_COMMENT) ? ";; " : "", s_indent,
		 sum, bb->frequency);
      lsum = 0;
      FOR_EACH_EDGE (e, ei, bb->preds)
	lsum += e->count;
      if (lsum - bb->count > 100 || lsum - bb->count < -100)
	fprintf (file, "%s%sInvalid sum of incoming counts %i, should be %i\n",
		 (flags & TDF_COMMENT) ? ";; " : "", s_indent,
		 (int) lsum, (int) bb->count);
    }
  if (BB_PARTITION (bb) == BB_COLD_PARTITION)
    {
      /* Warn about inconsistencies in the partitioning that are
         currently caused by profile insanities created via optimization.  */
      if (!probably_never_executed_bb_p (fun, bb))
        fprintf (file, "%s%sBlock in cold partition with hot count\n",
                 (flags & TDF_COMMENT) ? ";; " : "", s_indent);
      FOR_EACH_EDGE (e, ei, bb->preds)
        {
          if (!probably_never_executed_edge_p (fun, e))
            fprintf (file,
                     "%s%sBlock in cold partition with incoming hot edge\n",
                     (flags & TDF_COMMENT) ? ";; " : "", s_indent);
        }
    }
}

void
dump_edge_info (FILE *file, edge e, int flags, int do_succ)
{
  basic_block side = (do_succ ? e->dest : e->src);
  bool do_details = false;
  
  if ((flags & TDF_DETAILS) != 0
      && (flags & TDF_SLIM) == 0)
    do_details = true;

  if (side->index == ENTRY_BLOCK)
    fputs (" ENTRY", file);
  else if (side->index == EXIT_BLOCK)
    fputs (" EXIT", file);
  else
    fprintf (file, " %d", side->index);

  if (e->probability && do_details)
    fprintf (file, " [%.1f%%] ", e->probability * 100.0 / REG_BR_PROB_BASE);

  if (e->count && do_details)
    {
      fputs (" count:", file);
      fprintf (file, "%" PRId64, e->count);
    }

  if (e->flags && do_details)
    {
      static const char * const bitnames[] =
	{
#define DEF_EDGE_FLAG(NAME,IDX) #NAME ,
#include "cfg-flags.def"
	  NULL
#undef DEF_EDGE_FLAG
	};
      bool comma = false;
      int i, flags = e->flags;

      gcc_assert (e->flags <= EDGE_ALL_FLAGS);
      fputs (" (", file);
      for (i = 0; flags; i++)
	if (flags & (1 << i))
	  {
	    flags &= ~(1 << i);

	    if (comma)
	      fputc (',', file);
	    fputs (bitnames[i], file);
	    comma = true;
	  }

      fputc (')', file);
    }
}

DEBUG_FUNCTION void
debug (edge_def &ref)
{
  /* FIXME (crowl): Is this desireable?  */
  dump_edge_info (stderr, &ref, 0, false);
  dump_edge_info (stderr, &ref, 0, true);
}

DEBUG_FUNCTION void
debug (edge_def *ptr)
{
  if (ptr)
    debug (*ptr);
  else
    fprintf (stderr, "<nil>\n");
}

/* Simple routines to easily allocate AUX fields of basic blocks.  */

static struct obstack block_aux_obstack;
static void *first_block_aux_obj = 0;
static struct obstack edge_aux_obstack;
static void *first_edge_aux_obj = 0;

/* Allocate a memory block of SIZE as BB->aux.  The obstack must
   be first initialized by alloc_aux_for_blocks.  */

static void
alloc_aux_for_block (basic_block bb, int size)
{
  /* Verify that aux field is clear.  */
  gcc_assert (!bb->aux && first_block_aux_obj);
  bb->aux = obstack_alloc (&block_aux_obstack, size);
  memset (bb->aux, 0, size);
}

/* Initialize the block_aux_obstack and if SIZE is nonzero, call
   alloc_aux_for_block for each basic block.  */

void
alloc_aux_for_blocks (int size)
{
  static int initialized;

  if (!initialized)
    {
      gcc_obstack_init (&block_aux_obstack);
      initialized = 1;
    }
  else
    /* Check whether AUX data are still allocated.  */
    gcc_assert (!first_block_aux_obj);

  first_block_aux_obj = obstack_alloc (&block_aux_obstack, 0);
  if (size)
    {
      basic_block bb;

      FOR_ALL_BB_FN (bb, cfun)
	alloc_aux_for_block (bb, size);
    }
}

/* Clear AUX pointers of all blocks.  */

void
clear_aux_for_blocks (void)
{
  basic_block bb;

  FOR_ALL_BB_FN (bb, cfun)
    bb->aux = NULL;
}

/* Free data allocated in block_aux_obstack and clear AUX pointers
   of all blocks.  */

void
free_aux_for_blocks (void)
{
  gcc_assert (first_block_aux_obj);
  obstack_free (&block_aux_obstack, first_block_aux_obj);
  first_block_aux_obj = NULL;

  clear_aux_for_blocks ();
}

/* Allocate a memory edge of SIZE as E->aux.  The obstack must
   be first initialized by alloc_aux_for_edges.  */

void
alloc_aux_for_edge (edge e, int size)
{
  /* Verify that aux field is clear.  */
  gcc_assert (!e->aux && first_edge_aux_obj);
  e->aux = obstack_alloc (&edge_aux_obstack, size);
  memset (e->aux, 0, size);
}

/* Initialize the edge_aux_obstack and if SIZE is nonzero, call
   alloc_aux_for_edge for each basic edge.  */

void
alloc_aux_for_edges (int size)
{
  static int initialized;

  if (!initialized)
    {
      gcc_obstack_init (&edge_aux_obstack);
      initialized = 1;
    }
  else
    /* Check whether AUX data are still allocated.  */
    gcc_assert (!first_edge_aux_obj);

  first_edge_aux_obj = obstack_alloc (&edge_aux_obstack, 0);
  if (size)
    {
      basic_block bb;

      FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR_FOR_FN (cfun),
		      EXIT_BLOCK_PTR_FOR_FN (cfun), next_bb)
	{
	  edge e;
	  edge_iterator ei;

	  FOR_EACH_EDGE (e, ei, bb->succs)
	    alloc_aux_for_edge (e, size);
	}
    }
}

/* Clear AUX pointers of all edges.  */

void
clear_aux_for_edges (void)
{
  basic_block bb;
  edge e;

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR_FOR_FN (cfun),
		  EXIT_BLOCK_PTR_FOR_FN (cfun), next_bb)
    {
      edge_iterator ei;
      FOR_EACH_EDGE (e, ei, bb->succs)
	e->aux = NULL;
    }
}

/* Free data allocated in edge_aux_obstack and clear AUX pointers
   of all edges.  */

void
free_aux_for_edges (void)
{
  gcc_assert (first_edge_aux_obj);
  obstack_free (&edge_aux_obstack, first_edge_aux_obj);
  first_edge_aux_obj = NULL;

  clear_aux_for_edges ();
}

DEBUG_FUNCTION void
debug_bb (basic_block bb)
{
  dump_bb (stderr, bb, 0, dump_flags);
}

DEBUG_FUNCTION basic_block
debug_bb_n (int n)
{
  basic_block bb = BASIC_BLOCK_FOR_FN (cfun, n);
  debug_bb (bb);
  return bb;
}

/* Dumps cfg related information about basic block BB to OUTF.
   If HEADER is true, dump things that appear before the instructions
   contained in BB.  If FOOTER is true, dump things that appear after.
   Flags are the TDF_* masks as documented in dumpfile.h.
   NB: With TDF_DETAILS, it is assumed that cfun is available, so
   that maybe_hot_bb_p and probably_never_executed_bb_p don't ICE.  */

void
dump_bb_info (FILE *outf, basic_block bb, int indent, int flags,
	      bool do_header, bool do_footer)
{
  edge_iterator ei;
  edge e;
  static const char * const bb_bitnames[] =
    {
#define DEF_BASIC_BLOCK_FLAG(NAME,IDX) #NAME ,
#include "cfg-flags.def"
      NULL
#undef DEF_BASIC_BLOCK_FLAG
    };
  const unsigned n_bitnames = sizeof (bb_bitnames) / sizeof (char *);
  bool first;
  char *s_indent = (char *) alloca ((size_t) indent + 1);
  memset ((void *) s_indent, ' ', (size_t) indent);
  s_indent[indent] = '\0';

  gcc_assert (bb->flags <= BB_ALL_FLAGS);

  if (do_header)
    {
      unsigned i;

      if (flags & TDF_COMMENT)
	fputs (";; ", outf);
      fprintf (outf, "%sbasic block %d, loop depth %d",
	       s_indent, bb->index, bb_loop_depth (bb));
      if (flags & TDF_DETAILS)
	{
	  struct function *fun = DECL_STRUCT_FUNCTION (current_function_decl);
	  fprintf (outf, ", count " "%" PRId64,
		   (int64_t) bb->count);
	  fprintf (outf, ", freq %i", bb->frequency);
	  if (maybe_hot_bb_p (fun, bb))
	    fputs (", maybe hot", outf);
	  if (probably_never_executed_bb_p (fun, bb))
	    fputs (", probably never executed", outf);
	}
      fputc ('\n', outf);

      if (flags & TDF_DETAILS)
	{
	  check_bb_profile (bb, outf, indent, flags);
	  if (flags & TDF_COMMENT)
	    fputs (";; ", outf);
	  fprintf (outf, "%s prev block ", s_indent);
	  if (bb->prev_bb)
	    fprintf (outf, "%d", bb->prev_bb->index);
	  else
	    fprintf (outf, "(nil)");
	  fprintf (outf, ", next block ");
	  if (bb->next_bb)
	    fprintf (outf, "%d", bb->next_bb->index);
	  else
	    fprintf (outf, "(nil)");

	  fputs (", flags:", outf);
	  first = true;
	  for (i = 0; i < n_bitnames; i++)
	    if (bb->flags & (1 << i))
	      {
		if (first)
		  fputs (" (", outf);
		else
		  fputs (", ", outf);
		first = false;
		fputs (bb_bitnames[i], outf);
	      }
	  if (!first)
	    fputc (')', outf);
	  fputc ('\n', outf);
	}

      if (flags & TDF_COMMENT)
	fputs (";; ", outf);
      fprintf (outf, "%s pred:      ", s_indent);
      first = true;
      FOR_EACH_EDGE (e, ei, bb->preds)
	{
	  if (! first)
	    {
	      if (flags & TDF_COMMENT)
		fputs (";; ", outf);
	      fprintf (outf, "%s            ", s_indent);
	    }
	  first = false;
	  dump_edge_info (outf, e, flags, 0);
	  fputc ('\n', outf);
	}
      if (first)
	fputc ('\n', outf);
    }

  if (do_footer)
    {
      if (flags & TDF_COMMENT)
	fputs (";; ", outf);
      fprintf (outf, "%s succ:      ", s_indent);
      first = true;
      FOR_EACH_EDGE (e, ei, bb->succs)
        {
	  if (! first)
	    {
	      if (flags & TDF_COMMENT)
		fputs (";; ", outf);
	      fprintf (outf, "%s            ", s_indent);
	    }
	  first = false;
	  dump_edge_info (outf, e, flags, 1);
	  fputc ('\n', outf);
	}
      if (first)
	fputc ('\n', outf);
    }
}

/* Dumps a brief description of cfg to FILE.  */

void
brief_dump_cfg (FILE *file, int flags)
{
  basic_block bb;

  FOR_EACH_BB_FN (bb, cfun)
    {
      dump_bb_info (file, bb, 0,
		    flags & (TDF_COMMENT | TDF_DETAILS),
		    true, true);
    }
}

/* An edge originally destinating BB of FREQUENCY and COUNT has been proved to
   leave the block by TAKEN_EDGE.  Update profile of BB such that edge E can be
   redirected to destination of TAKEN_EDGE.

   This function may leave the profile inconsistent in the case TAKEN_EDGE
   frequency or count is believed to be lower than FREQUENCY or COUNT
   respectively.  */
void
update_bb_profile_for_threading (basic_block bb, int edge_frequency,
				 gcov_type count, edge taken_edge)
{
  edge c;
  int prob;
  edge_iterator ei;

  bb->count -= count;
  if (bb->count < 0)
    {
      if (dump_file)
	fprintf (dump_file, "bb %i count became negative after threading",
		 bb->index);
      bb->count = 0;
    }

  /* Compute the probability of TAKEN_EDGE being reached via threaded edge.
     Watch for overflows.  */
  if (bb->frequency)
    prob = GCOV_COMPUTE_SCALE (edge_frequency, bb->frequency);
  else
    prob = 0;
  if (prob > taken_edge->probability)
    {
      if (dump_file)
	fprintf (dump_file, "Jump threading proved probability of edge "
		 "%i->%i too small (it is %i, should be %i).\n",
		 taken_edge->src->index, taken_edge->dest->index,
		 taken_edge->probability, prob);
      prob = taken_edge->probability;
    }

  /* Now rescale the probabilities.  */
  taken_edge->probability -= prob;
  prob = REG_BR_PROB_BASE - prob;
  bb->frequency -= edge_frequency;
  if (bb->frequency < 0)
    bb->frequency = 0;
  if (prob <= 0)
    {
      if (dump_file)
	fprintf (dump_file, "Edge frequencies of bb %i has been reset, "
		 "frequency of block should end up being 0, it is %i\n",
		 bb->index, bb->frequency);
      EDGE_SUCC (bb, 0)->probability = REG_BR_PROB_BASE;
      ei = ei_start (bb->succs);
      ei_next (&ei);
      for (; (c = ei_safe_edge (ei)); ei_next (&ei))
	c->probability = 0;
    }
  else if (prob != REG_BR_PROB_BASE)
    {
      int scale = RDIV (65536 * REG_BR_PROB_BASE, prob);

      FOR_EACH_EDGE (c, ei, bb->succs)
	{
	  /* Protect from overflow due to additional scaling.  */
	  if (c->probability > prob)
	    c->probability = REG_BR_PROB_BASE;
	  else
	    {
	      c->probability = RDIV (c->probability * scale, 65536);
	      if (c->probability > REG_BR_PROB_BASE)
		c->probability = REG_BR_PROB_BASE;
	    }
	}
    }

  gcc_assert (bb == taken_edge->src);
  taken_edge->count -= count;
  if (taken_edge->count < 0)
    {
      if (dump_file)
	fprintf (dump_file, "edge %i->%i count became negative after threading",
		 taken_edge->src->index, taken_edge->dest->index);
      taken_edge->count = 0;
    }
}

/* Multiply all frequencies of basic blocks in array BBS of length NBBS
   by NUM/DEN, in int arithmetic.  May lose some accuracy.  */
void
scale_bbs_frequencies_int (basic_block *bbs, int nbbs, int num, int den)
{
  int i;
  edge e;
  if (num < 0)
    num = 0;

  /* Scale NUM and DEN to avoid overflows.  Frequencies are in order of
     10^4, if we make DEN <= 10^3, we can afford to upscale by 100
     and still safely fit in int during calculations.  */
  if (den > 1000)
    {
      if (num > 1000000)
	return;

      num = RDIV (1000 * num, den);
      den = 1000;
    }
  if (num > 100 * den)
    return;

  for (i = 0; i < nbbs; i++)
    {
      edge_iterator ei;
      bbs[i]->frequency = RDIV (bbs[i]->frequency * num, den);
      /* Make sure the frequencies do not grow over BB_FREQ_MAX.  */
      if (bbs[i]->frequency > BB_FREQ_MAX)
	bbs[i]->frequency = BB_FREQ_MAX;
      bbs[i]->count = RDIV (bbs[i]->count * num, den);
      FOR_EACH_EDGE (e, ei, bbs[i]->succs)
	e->count = RDIV (e->count * num, den);
    }
}

/* numbers smaller than this value are safe to multiply without getting
   64bit overflow.  */
#define MAX_SAFE_MULTIPLIER (1 << (sizeof (int64_t) * 4 - 1))

/* Multiply all frequencies of basic blocks in array BBS of length NBBS
   by NUM/DEN, in gcov_type arithmetic.  More accurate than previous
   function but considerably slower.  */
void
scale_bbs_frequencies_gcov_type (basic_block *bbs, int nbbs, gcov_type num,
				 gcov_type den)
{
  int i;
  edge e;
  gcov_type fraction = RDIV (num * 65536, den);

  gcc_assert (fraction >= 0);

  if (num < MAX_SAFE_MULTIPLIER)
    for (i = 0; i < nbbs; i++)
      {
	edge_iterator ei;
	bbs[i]->frequency = RDIV (bbs[i]->frequency * num, den);
	if (bbs[i]->count <= MAX_SAFE_MULTIPLIER)
	  bbs[i]->count = RDIV (bbs[i]->count * num, den);
	else
	  bbs[i]->count = RDIV (bbs[i]->count * fraction, 65536);
	FOR_EACH_EDGE (e, ei, bbs[i]->succs)
	  if (bbs[i]->count <= MAX_SAFE_MULTIPLIER)
	    e->count = RDIV (e->count * num, den);
	  else
	    e->count = RDIV (e->count * fraction, 65536);
      }
   else
    for (i = 0; i < nbbs; i++)
      {
	edge_iterator ei;
	if (sizeof (gcov_type) > sizeof (int))
	  bbs[i]->frequency = RDIV (bbs[i]->frequency * num, den);
	else
	  bbs[i]->frequency = RDIV (bbs[i]->frequency * fraction, 65536);
	bbs[i]->count = RDIV (bbs[i]->count * fraction, 65536);
	FOR_EACH_EDGE (e, ei, bbs[i]->succs)
	  e->count = RDIV (e->count * fraction, 65536);
      }
}

/* Helper types for hash tables.  */

struct htab_bb_copy_original_entry
{
  /* Block we are attaching info to.  */
  int index1;
  /* Index of original or copy (depending on the hashtable) */
  int index2;
};

struct bb_copy_hasher : nofree_ptr_hash <htab_bb_copy_original_entry>
{
  static inline hashval_t hash (const htab_bb_copy_original_entry *);
  static inline bool equal (const htab_bb_copy_original_entry *existing,
			    const htab_bb_copy_original_entry * candidate);
};

inline hashval_t
bb_copy_hasher::hash (const htab_bb_copy_original_entry *data)
{
  return data->index1;
}

inline bool
bb_copy_hasher::equal (const htab_bb_copy_original_entry *data,
		       const htab_bb_copy_original_entry *data2)
{
  return data->index1 == data2->index1;
}

/* Data structures used to maintain mapping between basic blocks and
   copies.  */
static hash_table<bb_copy_hasher> *bb_original;
static hash_table<bb_copy_hasher> *bb_copy;

/* And between loops and copies.  */
static hash_table<bb_copy_hasher> *loop_copy;
static object_allocator<htab_bb_copy_original_entry> *original_copy_bb_pool;

/* Initialize the data structures to maintain mapping between blocks
   and its copies.  */
void
initialize_original_copy_tables (void)
{
  original_copy_bb_pool = new object_allocator<htab_bb_copy_original_entry>
    ("original_copy");
  bb_original = new hash_table<bb_copy_hasher> (10);
  bb_copy = new hash_table<bb_copy_hasher> (10);
  loop_copy = new hash_table<bb_copy_hasher> (10);
}

/* Free the data structures to maintain mapping between blocks and
   its copies.  */
void
free_original_copy_tables (void)
{
  gcc_assert (original_copy_bb_pool);
  delete bb_copy;
  bb_copy = NULL;
  delete bb_original;
  bb_copy = NULL;
  delete loop_copy;
  loop_copy = NULL;
  delete original_copy_bb_pool;
  original_copy_bb_pool = NULL;
}

/* Removes the value associated with OBJ from table TAB.  */

static void
copy_original_table_clear (hash_table<bb_copy_hasher> *tab, unsigned obj)
{
  htab_bb_copy_original_entry **slot;
  struct htab_bb_copy_original_entry key, *elt;

  if (!original_copy_bb_pool)
    return;

  key.index1 = obj;
  slot = tab->find_slot (&key, NO_INSERT);
  if (!slot)
    return;

  elt = *slot;
  tab->clear_slot (slot);
  original_copy_bb_pool->remove (elt);
}

/* Sets the value associated with OBJ in table TAB to VAL.
   Do nothing when data structures are not initialized.  */

static void
copy_original_table_set (hash_table<bb_copy_hasher> *tab,
			 unsigned obj, unsigned val)
{
  struct htab_bb_copy_original_entry **slot;
  struct htab_bb_copy_original_entry key;

  if (!original_copy_bb_pool)
    return;

  key.index1 = obj;
  slot = tab->find_slot (&key, INSERT);
  if (!*slot)
    {
      *slot = original_copy_bb_pool->allocate ();
      (*slot)->index1 = obj;
    }
  (*slot)->index2 = val;
}

/* Set original for basic block.  Do nothing when data structures are not
   initialized so passes not needing this don't need to care.  */
void
set_bb_original (basic_block bb, basic_block original)
{
  copy_original_table_set (bb_original, bb->index, original->index);
}

/* Get the original basic block.  */
basic_block
get_bb_original (basic_block bb)
{
  struct htab_bb_copy_original_entry *entry;
  struct htab_bb_copy_original_entry key;

  gcc_assert (original_copy_bb_pool);

  key.index1 = bb->index;
  entry = bb_original->find (&key);
  if (entry)
    return BASIC_BLOCK_FOR_FN (cfun, entry->index2);
  else
    return NULL;
}

/* Set copy for basic block.  Do nothing when data structures are not
   initialized so passes not needing this don't need to care.  */
void
set_bb_copy (basic_block bb, basic_block copy)
{
  copy_original_table_set (bb_copy, bb->index, copy->index);
}

/* Get the copy of basic block.  */
basic_block
get_bb_copy (basic_block bb)
{
  struct htab_bb_copy_original_entry *entry;
  struct htab_bb_copy_original_entry key;

  gcc_assert (original_copy_bb_pool);

  key.index1 = bb->index;
  entry = bb_copy->find (&key);
  if (entry)
    return BASIC_BLOCK_FOR_FN (cfun, entry->index2);
  else
    return NULL;
}

/* Set copy for LOOP to COPY.  Do nothing when data structures are not
   initialized so passes not needing this don't need to care.  */

void
set_loop_copy (struct loop *loop, struct loop *copy)
{
  if (!copy)
    copy_original_table_clear (loop_copy, loop->num);
  else
    copy_original_table_set (loop_copy, loop->num, copy->num);
}

/* Get the copy of LOOP.  */

struct loop *
get_loop_copy (struct loop *loop)
{
  struct htab_bb_copy_original_entry *entry;
  struct htab_bb_copy_original_entry key;

  gcc_assert (original_copy_bb_pool);

  key.index1 = loop->num;
  entry = loop_copy->find (&key);
  if (entry)
    return get_loop (cfun, entry->index2);
  else
    return NULL;
}
