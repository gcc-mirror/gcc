/* Control flow graph manipulation code for GNU compiler.
   Copyright (C) 1987, 1988, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002 Free Software Foundation, Inc.

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

/* This file contains low level functions to manipulate the CFG and
   analyze it.  All other modules should not transform the datastructure
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
 */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "rtl.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "regs.h"
#include "flags.h"
#include "output.h"
#include "function.h"
#include "except.h"
#include "toplev.h"
#include "tm_p.h"
#include "obstack.h"

/* The obstack on which the flow graph components are allocated.  */

struct obstack flow_obstack;
static char *flow_firstobj;

/* Number of basic blocks in the current function.  */

int n_basic_blocks;

/* First free basic block number.  */

int last_basic_block;

/* Number of edges in the current function.  */

int n_edges;

/* First edge in the deleted edges chain.  */

edge first_deleted_edge;
static basic_block first_deleted_block;

/* The basic block array.  */

varray_type basic_block_info;

/* The special entry and exit blocks.  */

struct basic_block_def entry_exit_blocks[2]
= {{NULL,			/* head */
    NULL,			/* end */
    NULL,			/* head_tree */
    NULL,			/* end_tree */
    NULL,			/* pred */
    NULL,			/* succ */
    NULL,			/* local_set */
    NULL,			/* cond_local_set */
    NULL,			/* global_live_at_start */
    NULL,			/* global_live_at_end */
    NULL,			/* aux */
    ENTRY_BLOCK,		/* index */
    NULL,			/* prev_bb */
    EXIT_BLOCK_PTR,		/* next_bb */
    0,				/* loop_depth */
    NULL,                       /* loop_father */
    0,				/* count */
    0,				/* frequency */
    0				/* flags */
  },
  {
    NULL,			/* head */
    NULL,			/* end */
    NULL,			/* head_tree */
    NULL,			/* end_tree */
    NULL,			/* pred */
    NULL,			/* succ */
    NULL,			/* local_set */
    NULL,			/* cond_local_set */
    NULL,			/* global_live_at_start */
    NULL,			/* global_live_at_end */
    NULL,			/* aux */
    EXIT_BLOCK,			/* index */
    ENTRY_BLOCK_PTR,		/* prev_bb */
    NULL,			/* next_bb */
    0,				/* loop_depth */
    NULL,                       /* loop_father */
    0,				/* count */
    0,				/* frequency */
    0				/* flags */
  }
};

void debug_flow_info			PARAMS ((void));
static void free_edge			PARAMS ((edge));

/* Called once at initialization time.  */

void
init_flow ()
{
  static int initialized;

  first_deleted_edge = 0;
  first_deleted_block = 0;
  n_edges = 0;

  if (!initialized)
    {
      gcc_obstack_init (&flow_obstack);
      flow_firstobj = (char *) obstack_alloc (&flow_obstack, 0);
      initialized = 1;
    }
  else
    {
      obstack_free (&flow_obstack, flow_firstobj);
      flow_firstobj = (char *) obstack_alloc (&flow_obstack, 0);
    }
}

/* Helper function for remove_edge and clear_edges.  Frees edge structure
   without actually unlinking it from the pred/succ lists.  */

static void
free_edge (e)
     edge e;
{
  n_edges--;
  memset (e, 0, sizeof *e);
  e->succ_next = first_deleted_edge;
  first_deleted_edge = e;
}

/* Free the memory associated with the edge structures.  */

void
clear_edges ()
{
  basic_block bb;
  edge e;

  FOR_EACH_BB (bb)
    {
      edge e = bb->succ;

      while (e)
	{
	  edge next = e->succ_next;

	  free_edge (e);
	  e = next;
	}

      bb->succ = NULL;
      bb->pred = NULL;
    }

  e = ENTRY_BLOCK_PTR->succ;
  while (e)
    {
      edge next = e->succ_next;

      free_edge (e);
      e = next;
    }

  EXIT_BLOCK_PTR->pred = NULL;
  ENTRY_BLOCK_PTR->succ = NULL;

  if (n_edges)
    abort ();
}

/* Allocate memory for basic_block.  */

basic_block
alloc_block ()
{
  basic_block bb;

  if (first_deleted_block)
    {
      bb = first_deleted_block;
      first_deleted_block = (basic_block) bb->succ;
      bb->succ = NULL;
    }
  else
    {
      bb = (basic_block) obstack_alloc (&flow_obstack, sizeof *bb);
      memset (bb, 0, sizeof *bb);
    }
  return bb;
}

/* Link block B to chain after AFTER.  */
void
link_block (b, after)
     basic_block b, after;
{
  b->next_bb = after->next_bb;
  b->prev_bb = after;
  after->next_bb = b;
  b->next_bb->prev_bb = b;
}

/* Unlink block B from chain.  */
void
unlink_block (b)
     basic_block b;
{
  b->next_bb->prev_bb = b->prev_bb;
  b->prev_bb->next_bb = b->next_bb;
}

/* Sequentially order blocks and compact the arrays.  */
void
compact_blocks ()
{
  int i;
  basic_block bb;
 
  i = 0;
  FOR_EACH_BB (bb)
    {
      BASIC_BLOCK (i) = bb;
      bb->index = i;
      i++;
    }

  if (i != n_basic_blocks)
    abort ();

  last_basic_block = n_basic_blocks;
}


/* Remove block B from the basic block array.  */

void
expunge_block (b)
     basic_block b;
{
  unlink_block (b);
  BASIC_BLOCK (b->index) = NULL;
  n_basic_blocks--;

  /* Invalidate data to make bughunting easier.  */
  memset (b, 0, sizeof *b);
  b->index = -3;
  b->succ = (edge) first_deleted_block;
  first_deleted_block = (basic_block) b;
}

/* Create an edge connecting SRC and DEST with flags FLAGS.  Return newly
   created edge.  Use this only if you are sure that this edge can't
   possibly already exist.  */

edge
unchecked_make_edge (src, dst, flags)
     basic_block src, dst;
     int flags;
{
  edge e;

  if (first_deleted_edge)
    {
      e = first_deleted_edge;
      first_deleted_edge = e->succ_next;
    }
  else
    {
      e = (edge) obstack_alloc (&flow_obstack, sizeof *e);
      memset (e, 0, sizeof *e);
    }
  n_edges++;

  e->succ_next = src->succ;
  e->pred_next = dst->pred;
  e->src = src;
  e->dest = dst;
  e->flags = flags;

  src->succ = e;
  dst->pred = e;

  return e;
}
/* Create an edge connecting SRC and DST with FLAGS optionally using
   edge cache CACHE.  Return the new edge, NULL if already exist.  */

edge
cached_make_edge (edge_cache, src, dst, flags)
     sbitmap *edge_cache;
     basic_block src, dst;
     int flags;
{
  int use_edge_cache;
  edge e;

  /* Don't bother with edge cache for ENTRY or EXIT, if there aren't that
     many edges to them, or we didn't allocate memory for it.  */
  use_edge_cache = (edge_cache
		    && src != ENTRY_BLOCK_PTR && dst != EXIT_BLOCK_PTR);

  /* Make sure we don't add duplicate edges.  */
  switch (use_edge_cache)
    {
    default:
      /* Quick test for non-existence of the edge.  */
      if (! TEST_BIT (edge_cache[src->index], dst->index))
	break;

      /* The edge exists; early exit if no work to do.  */
      if (flags == 0)
	return NULL;

      /* FALLTHRU */
    case 0:
      for (e = src->succ; e; e = e->succ_next)
	if (e->dest == dst)
	  {
	    e->flags |= flags;
	    return NULL;
	  }
      break;
    }

  e = unchecked_make_edge (src, dst, flags);

  if (use_edge_cache)
    SET_BIT (edge_cache[src->index], dst->index);

  return e;
}

/* Create an edge connecting SRC and DEST with flags FLAGS.  Return newly
   created edge or NULL if already exist.  */

edge
make_edge (src, dest, flags)
     basic_block src, dest;
     int flags;
{
  return cached_make_edge (NULL, src, dest, flags);
}

/* Create an edge connecting SRC to DEST and set probability by knowing
   that it is the single edge leaving SRC.  */

edge
make_single_succ_edge (src, dest, flags)
     basic_block src, dest;
     int flags;
{
  edge e = make_edge (src, dest, flags);

  e->probability = REG_BR_PROB_BASE;
  e->count = src->count;
  return e;
}

/* This function will remove an edge from the flow graph.  */

void
remove_edge (e)
     edge e;
{
  edge last_pred = NULL;
  edge last_succ = NULL;
  edge tmp;
  basic_block src, dest;

  src = e->src;
  dest = e->dest;
  for (tmp = src->succ; tmp && tmp != e; tmp = tmp->succ_next)
    last_succ = tmp;

  if (!tmp)
    abort ();
  if (last_succ)
    last_succ->succ_next = e->succ_next;
  else
    src->succ = e->succ_next;

  for (tmp = dest->pred; tmp && tmp != e; tmp = tmp->pred_next)
    last_pred = tmp;

  if (!tmp)
    abort ();
  if (last_pred)
    last_pred->pred_next = e->pred_next;
  else
    dest->pred = e->pred_next;

  free_edge (e);
}

/* Redirect an edge's successor from one block to another.  */

void
redirect_edge_succ (e, new_succ)
     edge e;
     basic_block new_succ;
{
  edge *pe;

  /* Disconnect the edge from the old successor block.  */
  for (pe = &e->dest->pred; *pe != e; pe = &(*pe)->pred_next)
    continue;
  *pe = (*pe)->pred_next;

  /* Reconnect the edge to the new successor block.  */
  e->pred_next = new_succ->pred;
  new_succ->pred = e;
  e->dest = new_succ;
}

/* Like previous but avoid possible duplicate edge.  */

edge
redirect_edge_succ_nodup (e, new_succ)
     edge e;
     basic_block new_succ;
{
  edge s;

  /* Check whether the edge is already present.  */
  for (s = e->src->succ; s; s = s->succ_next)
    if (s->dest == new_succ && s != e)
      break;

  if (s)
    {
      s->flags |= e->flags;
      s->probability += e->probability;
      if (s->probability > REG_BR_PROB_BASE)
	s->probability = REG_BR_PROB_BASE;
      s->count += e->count;
      remove_edge (e);
      e = s;
    }
  else
    redirect_edge_succ (e, new_succ);

  return e;
}

/* Redirect an edge's predecessor from one block to another.  */

void
redirect_edge_pred (e, new_pred)
     edge e;
     basic_block new_pred;
{
  edge *pe;

  /* Disconnect the edge from the old predecessor block.  */
  for (pe = &e->src->succ; *pe != e; pe = &(*pe)->succ_next)
    continue;

  *pe = (*pe)->succ_next;

  /* Reconnect the edge to the new predecessor block.  */
  e->succ_next = new_pred->succ;
  new_pred->succ = e;
  e->src = new_pred;
}

void
clear_bb_flags ()
{
  basic_block bb;

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, NULL, next_bb)
    bb->flags = 0;
}

void
dump_flow_info (file)
     FILE *file;
{
  int i;
  int max_regno = max_reg_num ();
  basic_block bb;
  static const char * const reg_class_names[] = REG_CLASS_NAMES;

  fprintf (file, "%d registers.\n", max_regno);
  for (i = FIRST_PSEUDO_REGISTER; i < max_regno; i++)
    if (REG_N_REFS (i))
      {
	enum reg_class class, altclass;

	fprintf (file, "\nRegister %d used %d times across %d insns",
		 i, REG_N_REFS (i), REG_LIVE_LENGTH (i));
	if (REG_BASIC_BLOCK (i) >= 0)
	  fprintf (file, " in block %d", REG_BASIC_BLOCK (i));
	if (REG_N_SETS (i))
	  fprintf (file, "; set %d time%s", REG_N_SETS (i),
		   (REG_N_SETS (i) == 1) ? "" : "s");
	if (regno_reg_rtx[i] != NULL && REG_USERVAR_P (regno_reg_rtx[i]))
	  fprintf (file, "; user var");
	if (REG_N_DEATHS (i) != 1)
	  fprintf (file, "; dies in %d places", REG_N_DEATHS (i));
	if (REG_N_CALLS_CROSSED (i) == 1)
	  fprintf (file, "; crosses 1 call");
	else if (REG_N_CALLS_CROSSED (i))
	  fprintf (file, "; crosses %d calls", REG_N_CALLS_CROSSED (i));
	if (regno_reg_rtx[i] != NULL
	    && PSEUDO_REGNO_BYTES (i) != UNITS_PER_WORD)
	  fprintf (file, "; %d bytes", PSEUDO_REGNO_BYTES (i));

	class = reg_preferred_class (i);
	altclass = reg_alternate_class (i);
	if (class != GENERAL_REGS || altclass != ALL_REGS)
	  {
	    if (altclass == ALL_REGS || class == ALL_REGS)
	      fprintf (file, "; pref %s", reg_class_names[(int) class]);
	    else if (altclass == NO_REGS)
	      fprintf (file, "; %s or none", reg_class_names[(int) class]);
	    else
	      fprintf (file, "; pref %s, else %s",
		       reg_class_names[(int) class],
		       reg_class_names[(int) altclass]);
	  }

	if (regno_reg_rtx[i] != NULL && REG_POINTER (regno_reg_rtx[i]))
	  fprintf (file, "; pointer");
	fprintf (file, ".\n");
      }

  fprintf (file, "\n%d basic blocks, %d edges.\n", n_basic_blocks, n_edges);
  FOR_EACH_BB (bb)
    {
      edge e;
      int sum;
      gcov_type lsum;

      fprintf (file, "\nBasic block %d: first insn %d, last %d, ",
	       bb->index, INSN_UID (bb->head), INSN_UID (bb->end));
      fprintf (file, "prev %d, next %d, ",
	       bb->prev_bb->index, bb->next_bb->index);
      fprintf (file, "loop_depth %d, count ", bb->loop_depth);
      fprintf (file, HOST_WIDEST_INT_PRINT_DEC, bb->count);
      fprintf (file, ", freq %i", bb->frequency);
      if (maybe_hot_bb_p (bb))
	fprintf (file, ", maybe hot");
      if (probably_never_executed_bb_p (bb))
	fprintf (file, ", probably never executed");
      fprintf (file, ".\n");

      fprintf (file, "Predecessors: ");
      for (e = bb->pred; e; e = e->pred_next)
	dump_edge_info (file, e, 0);

      fprintf (file, "\nSuccessors: ");
      for (e = bb->succ; e; e = e->succ_next)
	dump_edge_info (file, e, 1);

      fprintf (file, "\nRegisters live at start:");
      dump_regset (bb->global_live_at_start, file);

      fprintf (file, "\nRegisters live at end:");
      dump_regset (bb->global_live_at_end, file);

      putc ('\n', file);

      /* Check the consistency of profile information.  We can't do that
	 in verify_flow_info, as the counts may get invalid for incompletely
	 solved graphs, later elliminating of conditionals or roundoff errors.
	 It is still practical to have them reported for debugging of simple
	 testcases.  */
      sum = 0;
      for (e = bb->succ; e; e = e->succ_next)
	sum += e->probability;
      if (bb->succ && abs (sum - REG_BR_PROB_BASE) > 100)
	fprintf (file, "Invalid sum of outgoing probabilities %.1f%%\n",
		 sum * 100.0 / REG_BR_PROB_BASE);
      sum = 0;
      for (e = bb->pred; e; e = e->pred_next)
	sum += EDGE_FREQUENCY (e);
      if (abs (sum - bb->frequency) > 100)
	fprintf (file,
		 "Invalid sum of incomming frequencies %i, should be %i\n",
		 sum, bb->frequency);
      lsum = 0;
      for (e = bb->pred; e; e = e->pred_next)
	lsum += e->count;
      if (lsum - bb->count > 100 || lsum - bb->count < -100)
	fprintf (file, "Invalid sum of incomming counts %i, should be %i\n",
		 (int)lsum, (int)bb->count);
      lsum = 0;
      for (e = bb->succ; e; e = e->succ_next)
	lsum += e->count;
      if (bb->succ && (lsum - bb->count > 100 || lsum - bb->count < -100))
	fprintf (file, "Invalid sum of incomming counts %i, should be %i\n",
		 (int)lsum, (int)bb->count);
    }

  putc ('\n', file);
}

void
debug_flow_info ()
{
  dump_flow_info (stderr);
}

void
dump_edge_info (file, e, do_succ)
     FILE *file;
     edge e;
     int do_succ;
{
  basic_block side = (do_succ ? e->dest : e->src);

  if (side == ENTRY_BLOCK_PTR)
    fputs (" ENTRY", file);
  else if (side == EXIT_BLOCK_PTR)
    fputs (" EXIT", file);
  else
    fprintf (file, " %d", side->index);

  if (e->probability)
    fprintf (file, " [%.1f%%] ", e->probability * 100.0 / REG_BR_PROB_BASE);

  if (e->count)
    {
      fprintf (file, " count:");
      fprintf (file, HOST_WIDEST_INT_PRINT_DEC, e->count);
    }

  if (e->flags)
    {
      static const char * const bitnames[]
	= {"fallthru", "ab", "abcall", "eh", "fake", "dfs_back", "can_fallthru"};
      int comma = 0;
      int i, flags = e->flags;

      fputs (" (", file);
      for (i = 0; flags; i++)
	if (flags & (1 << i))
	  {
	    flags &= ~(1 << i);

	    if (comma)
	      fputc (',', file);
	    if (i < (int) ARRAY_SIZE (bitnames))
	      fputs (bitnames[i], file);
	    else
	      fprintf (file, "%d", i);
	    comma = 1;
	  }

      fputc (')', file);
    }
}

/* Simple routines to easily allocate AUX fields of basic blocks.  */

static struct obstack block_aux_obstack;
static void *first_block_aux_obj = 0;
static struct obstack edge_aux_obstack;
static void *first_edge_aux_obj = 0;

/* Allocate a memory block of SIZE as BB->aux.  The obstack must
   be first initialized by alloc_aux_for_blocks.  */

inline void
alloc_aux_for_block (bb, size)
     basic_block bb;
     int size;
{
  /* Verify that aux field is clear.  */
  if (bb->aux || !first_block_aux_obj)
    abort ();
  bb->aux = obstack_alloc (&block_aux_obstack, size);
  memset (bb->aux, 0, size);
}

/* Initialize the block_aux_obstack and if SIZE is nonzero, call
   alloc_aux_for_block for each basic block.  */

void
alloc_aux_for_blocks (size)
     int size;
{
  static int initialized;

  if (!initialized)
    {
      gcc_obstack_init (&block_aux_obstack);
      initialized = 1;
    }

  /* Check whether AUX data are still allocated.  */
  else if (first_block_aux_obj)
    abort ();
  first_block_aux_obj = (char *) obstack_alloc (&block_aux_obstack, 0);
  if (size)
    {
      basic_block bb;

      FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, NULL, next_bb)
	alloc_aux_for_block (bb, size);
    }
}

/* Clear AUX pointers of all blocks.  */

void
clear_aux_for_blocks ()
{
  basic_block bb;

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, NULL, next_bb)
    bb->aux = NULL;
}

/* Free data allocated in block_aux_obstack and clear AUX pointers
   of all blocks.  */

void
free_aux_for_blocks ()
{
  if (!first_block_aux_obj)
    abort ();
  obstack_free (&block_aux_obstack, first_block_aux_obj);
  first_block_aux_obj = NULL;

  clear_aux_for_blocks ();
}

/* Allocate a memory edge of SIZE as BB->aux.  The obstack must
   be first initialized by alloc_aux_for_edges.  */

inline void
alloc_aux_for_edge (e, size)
     edge e;
     int size;
{
  /* Verify that aux field is clear.  */
  if (e->aux || !first_edge_aux_obj)
    abort ();
  e->aux = obstack_alloc (&edge_aux_obstack, size);
  memset (e->aux, 0, size);
}

/* Initialize the edge_aux_obstack and if SIZE is nonzero, call
   alloc_aux_for_edge for each basic edge.  */

void
alloc_aux_for_edges (size)
     int size;
{
  static int initialized;

  if (!initialized)
    {
      gcc_obstack_init (&edge_aux_obstack);
      initialized = 1;
    }

  /* Check whether AUX data are still allocated.  */
  else if (first_edge_aux_obj)
    abort ();

  first_edge_aux_obj = (char *) obstack_alloc (&edge_aux_obstack, 0);
  if (size)
    {
      basic_block bb;

      FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, EXIT_BLOCK_PTR, next_bb)
	{
	  edge e;

	  for (e = bb->succ; e; e = e->succ_next)
	    alloc_aux_for_edge (e, size);
	}
    }
}

/* Clear AUX pointers of all edges.  */

void
clear_aux_for_edges ()
{
  basic_block bb;
  edge e;

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, EXIT_BLOCK_PTR, next_bb)
    {
      for (e = bb->succ; e; e = e->succ_next)
	e->aux = NULL;
    }
}

/* Free data allocated in edge_aux_obstack and clear AUX pointers
   of all edges.  */

void
free_aux_for_edges ()
{
  if (!first_edge_aux_obj)
    abort ();
  obstack_free (&edge_aux_obstack, first_edge_aux_obj);
  first_edge_aux_obj = NULL;

  clear_aux_for_edges ();
}
