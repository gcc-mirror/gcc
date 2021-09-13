/* Control flow graph manipulation code for GNU compiler.
   Copyright (C) 1987-2021 Free Software Foundation, Inc.

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
	 init_flow, free_cfg
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



/* Called once at initialization time.  */

void
init_flow (struct function *the_fun)
{
  if (!the_fun->cfg)
    the_fun->cfg = ggc_cleared_alloc<control_flow_graph> ();
  n_edges_for_fn (the_fun) = 0;
  the_fun->cfg->count_max = profile_count::uninitialized ();
  ENTRY_BLOCK_PTR_FOR_FN (the_fun)
    = alloc_block ();
  ENTRY_BLOCK_PTR_FOR_FN (the_fun)->index = ENTRY_BLOCK;
  EXIT_BLOCK_PTR_FOR_FN (the_fun)
    = alloc_block ();
  EXIT_BLOCK_PTR_FOR_FN (the_fun)->index = EXIT_BLOCK;
  ENTRY_BLOCK_PTR_FOR_FN (the_fun)->next_bb
    = EXIT_BLOCK_PTR_FOR_FN (the_fun);
  EXIT_BLOCK_PTR_FOR_FN (the_fun)->prev_bb
    = ENTRY_BLOCK_PTR_FOR_FN (the_fun);
  the_fun->cfg->edge_flags_allocated = EDGE_ALL_FLAGS;
  the_fun->cfg->bb_flags_allocated = BB_ALL_FLAGS;
}

/* Helper function for remove_edge and free_cffg.  Frees edge structure
   without actually removing it from the pred/succ arrays.  */

static void
free_edge (function *fn, edge e)
{
  n_edges_for_fn (fn)--;
  ggc_free (e);
}

/* Free basic block BB.  */

static void
free_block (basic_block bb)
{
   vec_free (bb->succs);
   bb->succs = NULL;
   vec_free (bb->preds);
   bb->preds = NULL;
   ggc_free (bb);
}

/* Free the memory associated with the CFG in FN.  */

void
free_cfg (struct function *fn)
{
  edge e;
  edge_iterator ei;
  basic_block next;

  for (basic_block bb = ENTRY_BLOCK_PTR_FOR_FN (fn); bb; bb = next)
    {
      next = bb->next_bb;
      FOR_EACH_EDGE (e, ei, bb->succs)
	free_edge (fn, e);
      free_block (bb);
    }

  gcc_assert (!n_edges_for_fn (fn));
  /* Sanity check that dominance tree is freed.  */
  gcc_assert (!fn->cfg->x_dom_computed[0] && !fn->cfg->x_dom_computed[1]);
  
  vec_free (fn->cfg->x_label_to_block_map);
  vec_free (basic_block_info_for_fn (fn));
  ggc_free (fn->cfg);
  fn->cfg = NULL;
}

/* Allocate memory for basic_block.  */

basic_block
alloc_block (void)
{
  basic_block bb;
  bb = ggc_cleared_alloc<basic_block_def> ();
  bb->count = profile_count::uninitialized ();
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
     We should be able to release all dead SSA_NAMES and at the same time we
     should clear out BB pointer of dead statements consistently.  */
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

  e->probability = profile_probability::uninitialized ();
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

  e->probability = profile_probability::always ();
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
  int flags_to_preserve = BB_FLAGS_TO_PRESERVE;
  if (current_loops
      && loops_state_satisfies_p (cfun, LOOPS_HAVE_MARKED_IRREDUCIBLE_REGIONS))
    flags_to_preserve |= BB_IRREDUCIBLE_LOOP;

  FOR_ALL_BB_FN (bb, cfun)
    bb->flags &= flags_to_preserve;
}

/* Check the consistency of profile information.  We can't do that
   in verify_flow_info, as the counts may get invalid for incompletely
   solved graphs, later eliminating of conditionals or roundoff errors.
   It is still practical to have them reported for debugging of simple
   testcases.  */
static void
check_bb_profile (basic_block bb, FILE * file, int indent)
{
  edge e;
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
      profile_probability sum = profile_probability::never ();
      int isum = 0;

      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  if (!(e->flags & (EDGE_EH | EDGE_FAKE)))
	    found = true;
	  sum += e->probability;
	  if (e->probability.initialized_p ())
	    isum += e->probability.to_reg_br_prob_base ();
	}
      /* Only report mismatches for non-EH control flow. If there are only EH
	 edges it means that the BB ends by noreturn call.  Here the control
	 flow may just terminate.  */
      if (found)
	{
	  if (sum.differs_from_p (profile_probability::always ()))
	    {
	      fprintf (file,
		       ";; %sInvalid sum of outgoing probabilities ",
		       s_indent);
	      sum.dump (file);
	      fprintf (file, "\n");
	    }
	  /* Probabilities caps to 100% and thus the previous test will never
	     fire if the sum of probabilities is too large.  */
	  else if (isum > REG_BR_PROB_BASE + 100)
	    {
	      fprintf (file,
		       ";; %sInvalid sum of outgoing probabilities %.1f%%\n",
		       s_indent, isum * 100.0 / REG_BR_PROB_BASE);
	    }
	}
    }
  if (bb != ENTRY_BLOCK_PTR_FOR_FN (fun))
    {
      profile_count sum = profile_count::zero ();
      FOR_EACH_EDGE (e, ei, bb->preds)
	sum += e->count ();
      if (sum.differs_from_p (bb->count))
	{
	  fprintf (file, ";; %sInvalid sum of incoming counts ",
		   s_indent);
	  sum.dump (file);
	  fprintf (file, ", should be ");
	  bb->count.dump (file);
	  fprintf (file, "\n");
	}
    }
  if (BB_PARTITION (bb) == BB_COLD_PARTITION)
    {
      /* Warn about inconsistencies in the partitioning that are
         currently caused by profile insanities created via optimization.  */
      if (!probably_never_executed_bb_p (fun, bb))
	fprintf (file, ";; %sBlock in cold partition with hot count\n",
		 s_indent);
      FOR_EACH_EDGE (e, ei, bb->preds)
        {
          if (!probably_never_executed_edge_p (fun, e))
            fprintf (file,
		     ";; %sBlock in cold partition with incoming hot edge\n",
		     s_indent);
        }
    }
}

void
dump_edge_info (FILE *file, edge e, dump_flags_t flags, int do_succ)
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

  if (e->probability.initialized_p () && do_details)
    {
      fprintf (file, " [");
      e->probability.dump (file);
      fprintf (file, "] ");
    }

  if (e->count ().initialized_p () && do_details)
    {
      fputs (" count:", file);
      e->count ().dump (file);
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
  fprintf (stderr, "<edge (%d -> %d)>\n",
	   ref.src->index, ref.dest->index);
  dump_edge_info (stderr, &ref, TDF_DETAILS, false);
  fprintf (stderr, "\n");
}

DEBUG_FUNCTION void
debug (edge_def *ptr)
{
  if (ptr)
    debug (*ptr);
  else
    fprintf (stderr, "<nil>\n");
}

static void
debug_slim (edge e)
{
  fprintf (stderr, "<edge 0x%p (%d -> %d)>", (void *) e,
	   e->src->index, e->dest->index);
}

DEFINE_DEBUG_VEC (edge)
DEFINE_DEBUG_HASH_SET (edge)

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
  debug_bb (bb, dump_flags);
}

DEBUG_FUNCTION basic_block
debug_bb_n (int n)
{
  basic_block bb = BASIC_BLOCK_FOR_FN (cfun, n);
  debug_bb (bb);
  return bb;
}

/* Print BB with specified FLAGS.  */

DEBUG_FUNCTION void
debug_bb (basic_block bb, dump_flags_t flags)
{
  dump_bb (stderr, bb, 0, flags);
}

/* Print basic block numbered N with specified FLAGS.  */

DEBUG_FUNCTION basic_block
debug_bb_n (int n, dump_flags_t flags)
{
  basic_block bb = BASIC_BLOCK_FOR_FN (cfun, n);
  debug_bb (bb, flags);
  return bb;
}

/* Dumps cfg related information about basic block BB to OUTF.
   If HEADER is true, dump things that appear before the instructions
   contained in BB.  If FOOTER is true, dump things that appear after.
   Flags are the TDF_* masks as documented in dumpfile.h.
   NB: With TDF_DETAILS, it is assumed that cfun is available, so
   that maybe_hot_bb_p and probably_never_executed_bb_p don't ICE.  */

void
dump_bb_info (FILE *outf, basic_block bb, int indent, dump_flags_t flags,
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

      fputs (";; ", outf);
      fprintf (outf, "%sbasic block %d, loop depth %d",
	       s_indent, bb->index, bb_loop_depth (bb));
      if (flags & TDF_DETAILS)
	{
	  struct function *fun = DECL_STRUCT_FUNCTION (current_function_decl);
	  if (bb->count.initialized_p ())
	    {
	      fputs (", count ", outf);
	      bb->count.dump (outf);
	    }
	  if (maybe_hot_bb_p (fun, bb))
	    fputs (", maybe hot", outf);
	  if (probably_never_executed_bb_p (fun, bb))
	    fputs (", probably never executed", outf);
	}
      fputc ('\n', outf);

      if (flags & TDF_DETAILS)
	{
	  check_bb_profile (bb, outf, indent);
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

      fputs (";; ", outf);
      fprintf (outf, "%s pred:      ", s_indent);
      first = true;
      FOR_EACH_EDGE (e, ei, bb->preds)
	{
	  if (! first)
	    {
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
      fputs (";; ", outf);
      fprintf (outf, "%s succ:      ", s_indent);
      first = true;
      FOR_EACH_EDGE (e, ei, bb->succs)
        {
	  if (! first)
	    {
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
brief_dump_cfg (FILE *file, dump_flags_t flags)
{
  basic_block bb;

  FOR_EACH_BB_FN (bb, cfun)
    {
      dump_bb_info (file, bb, 0, flags & TDF_DETAILS, true, true);
    }
}

/* An edge originally destinating BB of COUNT has been proved to
   leave the block by TAKEN_EDGE.  Update profile of BB such that edge E can be
   redirected to destination of TAKEN_EDGE.

   This function may leave the profile inconsistent in the case TAKEN_EDGE
   frequency or count is believed to be lower than COUNT
   respectively.  */
void
update_bb_profile_for_threading (basic_block bb, 
				 profile_count count, edge taken_edge)
{
  edge c;
  profile_probability prob;
  edge_iterator ei;

  if (bb->count < count)
    {
      if (dump_file)
	fprintf (dump_file, "bb %i count became negative after threading",
		 bb->index);
    }
  bb->count -= count;

  /* Compute the probability of TAKEN_EDGE being reached via threaded edge.
     Watch for overflows.  */
  if (bb->count.nonzero_p ())
    prob = count.probability_in (bb->count);
  else
    prob = profile_probability::never ();
  if (prob > taken_edge->probability)
    {
      if (dump_file)
	{
	  fprintf (dump_file, "Jump threading proved probability of edge "
		   "%i->%i too small (it is ",
		   taken_edge->src->index, taken_edge->dest->index);	
	  taken_edge->probability.dump (dump_file);
	  fprintf (dump_file, " should be ");
	  prob.dump (dump_file);
	  fprintf (dump_file, ")\n");
	}
      prob = taken_edge->probability.apply_scale (6, 8);
    }

  /* Now rescale the probabilities.  */
  taken_edge->probability -= prob;
  prob = prob.invert ();
  if (prob == profile_probability::never ())
    {
      if (dump_file)
	fprintf (dump_file, "Edge probabilities of bb %i has been reset, "
		 "count of block should end up being 0, it is non-zero\n",
		 bb->index);
      EDGE_SUCC (bb, 0)->probability = profile_probability::guessed_always ();
      ei = ei_start (bb->succs);
      ei_next (&ei);
      for (; (c = ei_safe_edge (ei)); ei_next (&ei))
	c->probability = profile_probability::guessed_never ();
    }
  else if (!(prob == profile_probability::always ()))
    {
      FOR_EACH_EDGE (c, ei, bb->succs)
	c->probability /= prob;
    }

  gcc_assert (bb == taken_edge->src);
}

/* Multiply all frequencies of basic blocks in array BBS of length NBBS
   by NUM/DEN, in profile_count arithmetic.  More accurate than previous
   function but considerably slower.  */
void
scale_bbs_frequencies_profile_count (basic_block *bbs, int nbbs,
				     profile_count num, profile_count den)
{
  int i;
  if (num == profile_count::zero () || den.nonzero_p ())
    for (i = 0; i < nbbs; i++)
      bbs[i]->count = bbs[i]->count.apply_scale (num, den);
}

/* Multiply all frequencies of basic blocks in array BBS of length NBBS
   by NUM/DEN, in profile_count arithmetic.  More accurate than previous
   function but considerably slower.  */
void
scale_bbs_frequencies (basic_block *bbs, int nbbs,
		       profile_probability p)
{
  int i;

  for (i = 0; i < nbbs; i++)
    bbs[i]->count = bbs[i]->count.apply_probability (p);
}

/* Data structures used to maintain mapping between basic blocks and
   copies.  */
typedef hash_map<int_hash<int, -1, -2>, int> copy_map_t;
static copy_map_t *bb_original;
static copy_map_t *bb_copy;

/* And between loops and copies.  */
static copy_map_t *loop_copy;

/* Initialize the data structures to maintain mapping between blocks
   and its copies.  */
void
initialize_original_copy_tables (void)
{
  bb_original = new copy_map_t (10);
  bb_copy = new copy_map_t (10);
  loop_copy = new copy_map_t (10);
}

/* Reset the data structures to maintain mapping between blocks and
   its copies.  */

void
reset_original_copy_tables (void)
{
  bb_original->empty ();
  bb_copy->empty ();
  loop_copy->empty ();
}

/* Free the data structures to maintain mapping between blocks and
   its copies.  */
void
free_original_copy_tables (void)
{
  delete bb_copy;
  bb_copy = NULL;
  delete bb_original;
  bb_original = NULL;
  delete loop_copy;
  loop_copy = NULL;
}

/* Return true iff we have had a call to initialize_original_copy_tables
   without a corresponding call to free_original_copy_tables.  */

bool
original_copy_tables_initialized_p (void)
{
  return bb_copy != NULL;
}

/* Removes the value associated with OBJ from table TAB.  */

static void
copy_original_table_clear (copy_map_t *tab, unsigned obj)
{
  if (!original_copy_tables_initialized_p ())
    return;

  tab->remove (obj);
}

/* Sets the value associated with OBJ in table TAB to VAL.
   Do nothing when data structures are not initialized.  */

static void
copy_original_table_set (copy_map_t *tab,
			 unsigned obj, unsigned val)
{
  if (!original_copy_tables_initialized_p ())
    return;

  tab->put (obj, val);
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
  gcc_assert (original_copy_tables_initialized_p ());

  int *entry = bb_original->get (bb->index);
  if (entry)
    return BASIC_BLOCK_FOR_FN (cfun, *entry);
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
  gcc_assert (original_copy_tables_initialized_p ());

  int *entry = bb_copy->get (bb->index);
  if (entry)
    return BASIC_BLOCK_FOR_FN (cfun, *entry);
  else
    return NULL;
}

/* Set copy for LOOP to COPY.  Do nothing when data structures are not
   initialized so passes not needing this don't need to care.  */

void
set_loop_copy (class loop *loop, class loop *copy)
{
  if (!copy)
    copy_original_table_clear (loop_copy, loop->num);
  else
    copy_original_table_set (loop_copy, loop->num, copy->num);
}

/* Get the copy of LOOP.  */

class loop *
get_loop_copy (class loop *loop)
{
  gcc_assert (original_copy_tables_initialized_p ());

  int *entry = loop_copy->get (loop->num);
  if (entry)
    return get_loop (cfun, *entry);
  else
    return NULL;
}
