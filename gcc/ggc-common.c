/* Simple garbage collection for the GNU compiler.
   Copyright (C) 1999, 2000, 2001, 2002 Free Software Foundation, Inc.

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

/* Generic garbage collection (GC) functions and data, not specific to
   any particular GC implementation.  */

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "tree.h"
#include "tm_p.h"
#include "hashtab.h"
#include "varray.h"
#include "ggc.h"
#include "langhooks.h"

/* Statistics about the allocation.  */
static ggc_statistics *ggc_stats;

static void ggc_mark_rtx_children_1 PARAMS ((rtx));
static int ggc_htab_delete PARAMS ((void **, void *));

/* Maintain global roots that are preserved during GC.  */

/* Global roots that are preserved during calls to gc.  */

struct ggc_root
{
  struct ggc_root *next;
  void *base;
  int nelt;
  int size;
  void (*cb) PARAMS ((void *));
};

static struct ggc_root *roots;

/* Add BASE as a new garbage collection root.  It is an array of
   length NELT with each element SIZE bytes long.  CB is a
   function that will be called with a pointer to each element
   of the array; it is the intention that CB call the appropriate
   routine to mark gc-able memory for that element.  */

void
ggc_add_root (base, nelt, size, cb)
     void *base;
     int nelt, size;
     void (*cb) PARAMS ((void *));
{
  struct ggc_root *x = (struct ggc_root *) xmalloc (sizeof (*x));

  x->next = roots;
  x->base = base;
  x->nelt = nelt;
  x->size = size;
  x->cb = cb;

  roots = x;
}

/* Process a slot of an htab by deleting it if it has not been marked.  */

static int
ggc_htab_delete (slot, info)
     void **slot;
     void *info;
{
  const struct ggc_cache_tab *r = (const struct ggc_cache_tab *) info;

  if (! (*r->marked_p) (*slot))
    htab_clear_slot (*r->base, slot);
  else
    (*r->cb) (*slot);

  return 1;
}

/* Iterate through all registered roots and mark each element.  */

void
ggc_mark_roots ()
{
  struct ggc_root *x;
  const struct ggc_root_tab *const *rt;
  const struct ggc_root_tab *rti;
  const struct ggc_cache_tab *const *ct;
  const struct ggc_cache_tab *cti;
  size_t i;

  for (rt = gt_ggc_deletable_rtab; *rt; rt++)
    for (rti = *rt; rti->base != NULL; rti++)
      memset (rti->base, 0, rti->stride);

  for (rt = gt_ggc_rtab; *rt; rt++)
    for (rti = *rt; rti->base != NULL; rti++)
      for (i = 0; i < rti->nelt; i++)
	(*rti->cb)(*(void **)((char *)rti->base + rti->stride * i));

  for (x = roots; x != NULL; x = x->next)
    {
      char *elt = x->base;
      int s = x->size, n = x->nelt;
      void (*cb) PARAMS ((void *)) = x->cb;
      int i;

      for (i = 0; i < n; ++i, elt += s)
	(*cb)(elt);
    }

  /* Now scan all hash tables that have objects which are to be deleted if
     they are not already marked.  */
  for (ct = gt_ggc_cache_rtab; *ct; ct++)
    for (cti = *ct; cti->base != NULL; cti++)
      htab_traverse (*cti->base, ggc_htab_delete, (PTR) cti);
}

/* R had not been previously marked, but has now been marked via
   ggc_set_mark.  Now recurse and process the children.  */

void
ggc_mark_rtx_children (r)
     rtx r;
{
  rtx i, last;

  /* Special case the instruction chain.  This is a data structure whose
     chain length is potentially unbounded, and which contain references
     within the chain (e.g. label_ref and insn_list).  If do nothing here,
     we risk blowing the stack recursing through a long chain of insns.

     Combat this by marking all of the instructions in the chain before
     marking the contents of those instructions.  */

  switch (GET_CODE (r))
    {
    case INSN:
    case JUMP_INSN:
    case CALL_INSN:
    case NOTE:
    case CODE_LABEL:
    case BARRIER:
      for (i = NEXT_INSN (r); ; i = NEXT_INSN (i))
	if (! ggc_test_and_set_mark (i))
	  break;
      last = i;

      for (i = NEXT_INSN (r); i != last; i = NEXT_INSN (i))
	ggc_mark_rtx_children_1 (i);

    default:
      break;
    }

  ggc_mark_rtx_children_1 (r);
}

static void
ggc_mark_rtx_children_1 (r)
     rtx r;
{
  const char *fmt;
  int i;
  rtx next_rtx;

  do
    {
      enum rtx_code code = GET_CODE (r);
      /* This gets set to a child rtx to eliminate tail recursion.  */
      next_rtx = NULL;

      /* Collect statistics, if appropriate.  */
      if (ggc_stats)
	{
	  ++ggc_stats->num_rtxs[(int) code];
	  ggc_stats->size_rtxs[(int) code] += ggc_get_size (r);
	}

      /* ??? If (some of) these are really pass-dependent info, do we
	 have any right poking our noses in?  */
      switch (code)
	{
	case MEM:
	  gt_ggc_m_mem_attrs (MEM_ATTRS (r));
	  break;
	case JUMP_INSN:
	  ggc_mark_rtx (JUMP_LABEL (r));
	  break;
	case CODE_LABEL:
	  ggc_mark_rtx (LABEL_REFS (r));
	  break;
	case LABEL_REF:
	  ggc_mark_rtx (LABEL_NEXTREF (r));
	  ggc_mark_rtx (CONTAINING_INSN (r));
	  break;
	case ADDRESSOF:
	  ggc_mark_tree (ADDRESSOF_DECL (r));
	  break;
	case NOTE:
	  switch (NOTE_LINE_NUMBER (r))
	    {
	    case NOTE_INSN_EXPECTED_VALUE:
	      ggc_mark_rtx (NOTE_EXPECTED_VALUE (r));
	      break;

	    case NOTE_INSN_BLOCK_BEG:
	    case NOTE_INSN_BLOCK_END:
	      ggc_mark_tree (NOTE_BLOCK (r));
	      break;

	    default:
	      break;
	    }
	  break;

	default:
	  break;
	}

      for (fmt = GET_RTX_FORMAT (GET_CODE (r)), i = 0; *fmt ; ++fmt, ++i)
	{
	  rtx exp;
	  switch (*fmt)
	    {
	    case 'e': case 'u':
	      exp = XEXP (r, i);
	      if (ggc_test_and_set_mark (exp))
		{
		  if (next_rtx == NULL)
		    next_rtx = exp;
		  else
		    ggc_mark_rtx_children (exp);
		}
	      break;
	    case 'V': case 'E':
	      gt_ggc_m_rtvec_def (XVEC (r, i));
	      break;
	    }
	}
    }
  while ((r = next_rtx) != NULL);
}

/* Various adaptor functions.  */
void
gt_ggc_mx_rtx_def (x)
     void *x;
{
  ggc_mark_rtx((rtx)x);
}

/* Allocate a block of memory, then clear it.  */
void *
ggc_alloc_cleared (size)
     size_t size;
{
  void *buf = ggc_alloc (size);
  memset (buf, 0, size);
  return buf;
}

/* Resize a block of memory, possibly re-allocating it.  */
void *
ggc_realloc (x, size)
     void *x;
     size_t size;
{
  void *r;
  size_t old_size;

  if (x == NULL)
    return ggc_alloc (size);

  old_size = ggc_get_size (x);
  if (size <= old_size)
    return x;

  r = ggc_alloc (size);
  memcpy (r, x, old_size);
  return r;
}

/* Like ggc_alloc_cleared, but performs a multiplication.  */
void *
ggc_calloc (s1, s2)
     size_t s1, s2;
{
  return ggc_alloc_cleared (s1 * s2);
}

/* Print statistics that are independent of the collector in use.  */
#define SCALE(x) ((unsigned long) ((x) < 1024*10 \
		  ? (x) \
		  : ((x) < 1024*1024*10 \
		     ? (x) / 1024 \
		     : (x) / (1024*1024))))
#define LABEL(x) ((x) < 1024*10 ? ' ' : ((x) < 1024*1024*10 ? 'k' : 'M'))

void
ggc_print_common_statistics (stream, stats)
     FILE *stream;
     ggc_statistics *stats;
{
  int code;

  /* Set the pointer so that during collection we will actually gather
     the statistics.  */
  ggc_stats = stats;

  /* Then do one collection to fill in the statistics.  */
  ggc_collect ();

  /* Total the statistics.  */
  for (code = 0; code < MAX_TREE_CODES; ++code)
    {
      stats->total_num_trees += stats->num_trees[code];
      stats->total_size_trees += stats->size_trees[code];
    }
  for (code = 0; code < NUM_RTX_CODE; ++code)
    {
      stats->total_num_rtxs += stats->num_rtxs[code];
      stats->total_size_rtxs += stats->size_rtxs[code];
    }

  /* Print the statistics for trees.  */
  fprintf (stream, "\n%-17s%10s %16s %10s\n", "Tree",
	   "Number", "Bytes", "% Total");
  for (code = 0; code < MAX_TREE_CODES; ++code)
    if (ggc_stats->num_trees[code])
      {
	fprintf (stream, "%-17s%10u%16ld%c %10.3f\n",
		 tree_code_name[code],
		 ggc_stats->num_trees[code],
		 SCALE (ggc_stats->size_trees[code]),
		 LABEL (ggc_stats->size_trees[code]),
		 (100 * ((double) ggc_stats->size_trees[code])
		  / ggc_stats->total_size_trees));
      }
  fprintf (stream,
	   "%-17s%10u%16ld%c\n", "Total",
	   ggc_stats->total_num_trees,
	   SCALE (ggc_stats->total_size_trees),
	   LABEL (ggc_stats->total_size_trees));

  /* Print the statistics for RTL.  */
  fprintf (stream, "\n%-17s%10s %16s %10s\n", "RTX",
	   "Number", "Bytes", "% Total");
  for (code = 0; code < NUM_RTX_CODE; ++code)
    if (ggc_stats->num_rtxs[code])
      {
	fprintf (stream, "%-17s%10u%16ld%c %10.3f\n",
		 rtx_name[code],
		 ggc_stats->num_rtxs[code],
		 SCALE (ggc_stats->size_rtxs[code]),
		 LABEL (ggc_stats->size_rtxs[code]),
		 (100 * ((double) ggc_stats->size_rtxs[code])
		  / ggc_stats->total_size_rtxs));
      }
  fprintf (stream,
	   "%-17s%10u%16ld%c\n", "Total",
	   ggc_stats->total_num_rtxs,
	   SCALE (ggc_stats->total_size_rtxs),
	   LABEL (ggc_stats->total_size_rtxs));

  /* Don't gather statistics any more.  */
  ggc_stats = NULL;
}
