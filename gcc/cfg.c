/* Control flow graph manipulation code for GNU compiler.
   Copyright (C) 1987, 1988, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.

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
 */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
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
#include "alloc-pool.h"

/* The obstack on which the flow graph components are allocated.  */

struct obstack flow_obstack;
static char *flow_firstobj;

/* Basic block object pool.  */

static alloc_pool bb_pool;

/* Edge object pool.  */

static alloc_pool edge_pool;

/* Number of basic blocks in the current function.  */

int n_basic_blocks;

/* First free basic block number.  */

int last_basic_block;

/* Number of edges in the current function.  */

int n_edges;

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
    { NULL, NULL },		/* dom */
    0,				/* count */
    0,				/* frequency */
    0,				/* flags */
    NULL			/* rbi */
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
    { NULL, NULL },		/* dom */
    0,				/* count */
    0,				/* frequency */
    0,				/* flags */
    NULL			/* rbi */
  }
};

void debug_flow_info (void);
static void free_edge (edge);

/* Called once at initialization time.  */

void
init_flow (void)
{
  static int initialized;

  n_edges = 0;

  if (!initialized)
    {
      gcc_obstack_init (&flow_obstack);
      flow_firstobj = obstack_alloc (&flow_obstack, 0);
      initialized = 1;
    }
  else
    {
      free_alloc_pool (bb_pool);
      free_alloc_pool (edge_pool);
      obstack_free (&flow_obstack, flow_firstobj);
      flow_firstobj = obstack_alloc (&flow_obstack, 0);
    }
  bb_pool = create_alloc_pool ("Basic block pool",
			       sizeof (struct basic_block_def), 100);
  edge_pool = create_alloc_pool ("Edge pool",
			       sizeof (struct edge_def), 100);
}

/* Helper function for remove_edge and clear_edges.  Frees edge structure
   without actually unlinking it from the pred/succ lists.  */

static void
free_edge (edge e)
{
  n_edges--;
  pool_free (edge_pool, e);
}

/* Free the memory associated with the edge structures.  */

void
clear_edges (void)
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
alloc_block (void)
{
  basic_block bb;
  bb = pool_alloc (bb_pool);
  memset (bb, 0, sizeof (*bb));
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
}

/* Sequentially order blocks and compact the arrays.  */
void
compact_blocks (void)
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
expunge_block (basic_block b)
{
  unlink_block (b);
  BASIC_BLOCK (b->index) = NULL;
  n_basic_blocks--;
  pool_free (bb_pool, b);
}

/* Create an edge connecting SRC and DEST with flags FLAGS.  Return newly
   created edge.  Use this only if you are sure that this edge can't
   possibly already exist.  */

edge
unchecked_make_edge (basic_block src, basic_block dst, int flags)
{
  edge e;
  e = pool_alloc (edge_pool);
  memset (e, 0, sizeof (*e));
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
cached_make_edge (sbitmap *edge_cache, basic_block src, basic_block dst, int flags)
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

      /* Fall through.  */
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
make_edge (basic_block src, basic_block dest, int flags)
{
  return cached_make_edge (NULL, src, dest, flags);
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
remove_edge (edge e)
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
redirect_edge_succ (edge e, basic_block new_succ)
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
redirect_edge_succ_nodup (edge e, basic_block new_succ)
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
redirect_edge_pred (edge e, basic_block new_pred)
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
clear_bb_flags (void)
{
  basic_block bb;

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, NULL, next_bb)
    bb->flags = 0;
}

void
dump_flow_info (FILE *file)
{
  int i;
  int max_regno = max_reg_num ();
  basic_block bb;
  static const char * const reg_class_names[] = REG_CLASS_NAMES;

  fprintf (file, "%d registers.\n", max_regno);
  if (reg_n_info)
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
	       bb->index, INSN_UID (BB_HEAD (bb)), INSN_UID (BB_END (bb)));
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
	 solved graphs, later eliminating of conditionals or roundoff errors.
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
debug_flow_info (void)
{
  dump_flow_info (stderr);
}

void
dump_edge_info (FILE *file, edge e, int do_succ)
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
      static const char * const bitnames[] = {
	"fallthru", "ab", "abcall", "eh", "fake", "dfs_back",
	"can_fallthru", "irreducible", "sibcall", "loop_exit"
      };
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
alloc_aux_for_block (basic_block bb, int size)
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
alloc_aux_for_blocks (int size)
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
  first_block_aux_obj = obstack_alloc (&block_aux_obstack, 0);
  if (size)
    {
      basic_block bb;

      FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, NULL, next_bb)
	alloc_aux_for_block (bb, size);
    }
}

/* Clear AUX pointers of all blocks.  */

void
clear_aux_for_blocks (void)
{
  basic_block bb;

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, NULL, next_bb)
    bb->aux = NULL;
}

/* Free data allocated in block_aux_obstack and clear AUX pointers
   of all blocks.  */

void
free_aux_for_blocks (void)
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
alloc_aux_for_edge (edge e, int size)
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
alloc_aux_for_edges (int size)
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

  first_edge_aux_obj = obstack_alloc (&edge_aux_obstack, 0);
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
clear_aux_for_edges (void)
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
free_aux_for_edges (void)
{
  if (!first_edge_aux_obj)
    abort ();
  obstack_free (&edge_aux_obstack, first_edge_aux_obj);
  first_edge_aux_obj = NULL;

  clear_aux_for_edges ();
}

/* Verify the CFG consistency.

   Currently it does following checks edge and basic block list correctness
   and calls into IL dependent checking then.  */
void
verify_flow_info (void)
{
  size_t *edge_checksum;
  int num_bb_notes, err = 0;
  basic_block bb, last_bb_seen;
  basic_block *last_visited;

  last_visited = xcalloc (last_basic_block + 2, sizeof (basic_block));
  edge_checksum = xcalloc (last_basic_block + 2, sizeof (size_t));

  /* Check bb chain & numbers.  */
  last_bb_seen = ENTRY_BLOCK_PTR;
  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR->next_bb, NULL, next_bb)
    {
      if (bb != EXIT_BLOCK_PTR
	  && bb != BASIC_BLOCK (bb->index))
	{
	  error ("bb %d on wrong place", bb->index);
	  err = 1;
	}

      if (bb->prev_bb != last_bb_seen)
	{
	  error ("prev_bb of %d should be %d, not %d",
		 bb->index, last_bb_seen->index, bb->prev_bb->index);
	  err = 1;
	}

      last_bb_seen = bb;
    }

  /* Now check the basic blocks (boundaries etc.) */
  FOR_EACH_BB_REVERSE (bb)
    {
      int n_fallthru = 0;
      edge e;

      if (bb->count < 0)
	{
	  error ("verify_flow_info: Wrong count of block %i %i",
	         bb->index, (int)bb->count);
	  err = 1;
	}
      if (bb->frequency < 0)
	{
	  error ("verify_flow_info: Wrong frequency of block %i %i",
	         bb->index, bb->frequency);
	  err = 1;
	}
      for (e = bb->succ; e; e = e->succ_next)
	{
	  if (last_visited [e->dest->index + 2] == bb)
	    {
	      error ("verify_flow_info: Duplicate edge %i->%i",
		     e->src->index, e->dest->index);
	      err = 1;
	    }
	  if (e->probability < 0 || e->probability > REG_BR_PROB_BASE)
	    {
	      error ("verify_flow_info: Wrong probability of edge %i->%i %i",
		     e->src->index, e->dest->index, e->probability);
	      err = 1;
	    }
	  if (e->count < 0)
	    {
	      error ("verify_flow_info: Wrong count of edge %i->%i %i",
		     e->src->index, e->dest->index, (int)e->count);
	      err = 1;
	    }

	  last_visited [e->dest->index + 2] = bb;

	  if (e->flags & EDGE_FALLTHRU)
	    n_fallthru++;

	  if (e->src != bb)
	    {
	      error ("verify_flow_info: Basic block %d succ edge is corrupted",
		     bb->index);
	      fprintf (stderr, "Predecessor: ");
	      dump_edge_info (stderr, e, 0);
	      fprintf (stderr, "\nSuccessor: ");
	      dump_edge_info (stderr, e, 1);
	      fprintf (stderr, "\n");
	      err = 1;
	    }

	  edge_checksum[e->dest->index + 2] += (size_t) e;
	}
      if (n_fallthru > 1)
	{
	  error ("Wrong amount of branch edges after unconditional jump %i", bb->index);
	  err = 1;
	}

      for (e = bb->pred; e; e = e->pred_next)
	{
	  if (e->dest != bb)
	    {
	      error ("basic block %d pred edge is corrupted", bb->index);
	      fputs ("Predecessor: ", stderr);
	      dump_edge_info (stderr, e, 0);
	      fputs ("\nSuccessor: ", stderr);
	      dump_edge_info (stderr, e, 1);
	      fputc ('\n', stderr);
	      err = 1;
	    }
	  edge_checksum[e->dest->index + 2] -= (size_t) e;
	}
    }

  /* Complete edge checksumming for ENTRY and EXIT.  */
  {
    edge e;

    for (e = ENTRY_BLOCK_PTR->succ; e ; e = e->succ_next)
      edge_checksum[e->dest->index + 2] += (size_t) e;

    for (e = EXIT_BLOCK_PTR->pred; e ; e = e->pred_next)
      edge_checksum[e->dest->index + 2] -= (size_t) e;
  }

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, NULL, next_bb)
    if (edge_checksum[bb->index + 2])
      {
	error ("basic block %i edge lists are corrupted", bb->index);
	err = 1;
      }

  num_bb_notes = 0;
  last_bb_seen = ENTRY_BLOCK_PTR;

  /* Clean up.  */
  free (last_visited);
  free (edge_checksum);
  err |= cfg_hooks->cfgh_verify_flow_info ();
  if (err)
    internal_error ("verify_flow_info failed");
}

/* Print out one basic block with live information at start and end.  */

void
dump_bb (basic_block bb, FILE *outf)
{
  edge e;

  fprintf (outf, ";; Basic block %d, loop depth %d, count ",
	   bb->index, bb->loop_depth);
  fprintf (outf, HOST_WIDEST_INT_PRINT_DEC, (HOST_WIDEST_INT) bb->count);
  putc ('\n', outf);
  fputs (";; Predecessors: ", outf);
  for (e = bb->pred; e; e = e->pred_next)
    dump_edge_info (outf, e, 0);
  putc ('\n', outf);

  cfg_hooks->dump_bb (bb, outf);

  fputs (";; Successors: ", outf);
  for (e = bb->succ; e; e = e->succ_next)
    dump_edge_info (outf, e, 1);
  putc ('\n', outf);
}

void
debug_bb (basic_block bb)
{
  dump_bb (bb, stderr);
}

basic_block
debug_bb_n (int n)
{
  basic_block bb = BASIC_BLOCK (n);
  dump_bb (bb, stderr);
  return bb;
}
