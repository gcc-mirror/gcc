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
#include "params.h"
#ifdef HAVE_SYS_RESOURCE_H
# include <sys/resource.h>
#endif
#ifdef ENABLE_VALGRIND_CHECKING
#include <valgrind.h>
#else
/* Avoid #ifdef:s when we can help it.  */
#define VALGRIND_DISCARD(x)
#endif

/* Statistics about the allocation.  */
static ggc_statistics *ggc_stats;

static int ggc_htab_delete PARAMS ((void **, void *));
static double ggc_rlimit_bound PARAMS ((double));

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
      if (*cti->base)
	htab_traverse (*cti->base, ggc_htab_delete, (PTR) cti);
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
    {
      /* Mark the unwanted memory as unaccessible.  We also need to make
	 the "new" size accessible, since ggc_get_size returns the size of
	 the pool, not the size of the individually allocated object, the
	 size which was previously made accessible.  Unfortunately, we
	 don't know that previously allocated size.  Without that
	 knowledge we have to lose some initialization-tracking for the
	 old parts of the object.  An alternative is to mark the whole
	 old_size as reachable, but that would lose tracking of writes 
	 after the end of the object (by small offsets).  Discard the
	 handle to avoid handle leak.  */
      VALGRIND_DISCARD (VALGRIND_MAKE_NOACCESS ((char *) x + size,
						old_size - size));
      VALGRIND_DISCARD (VALGRIND_MAKE_READABLE (x, size));
      return x;
    }

  r = ggc_alloc (size);

  /* Since ggc_get_size returns the size of the pool, not the size of the
     individually allocated object, we'd access parts of the old object
     that were marked invalid with the memcpy below.  We lose a bit of the
     initialization-tracking since some of it may be uninitialized.  */
  VALGRIND_DISCARD (VALGRIND_MAKE_READABLE (x, old_size));

  memcpy (r, x, old_size);

  /* The old object is not supposed to be used anymore.  */
  VALGRIND_DISCARD (VALGRIND_MAKE_NOACCESS (x, old_size));

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

/* Modify the bound based on rlimits.  Keep the smallest number found.  */
static double
ggc_rlimit_bound (limit)
     double limit;
{
#if defined(HAVE_GETRLIMIT)
  struct rlimit rlim;
# ifdef RLIMIT_RSS
  if (getrlimit (RLIMIT_RSS, &rlim) == 0
      && rlim.rlim_cur != (rlim_t) RLIM_INFINITY
      && rlim.rlim_cur < limit)
    limit = rlim.rlim_cur;
# endif
# ifdef RLIMIT_DATA
  if (getrlimit (RLIMIT_DATA, &rlim) == 0
      && rlim.rlim_cur != (rlim_t) RLIM_INFINITY
      && rlim.rlim_cur < limit)
    limit = rlim.rlim_cur;
# endif
# ifdef RLIMIT_AS
  if (getrlimit (RLIMIT_AS, &rlim) == 0
      && rlim.rlim_cur != (rlim_t) RLIM_INFINITY
      && rlim.rlim_cur < limit)
    limit = rlim.rlim_cur;
# endif
#endif /* HAVE_GETRLIMIT */

  return limit;
}

/* Heuristic to set a default for GGC_MIN_EXPAND.  */
int
ggc_min_expand_heuristic()
{
  double min_expand = physmem_total();

  /* Adjust for rlimits.  */
  min_expand = ggc_rlimit_bound (min_expand);
  
  /* The heuristic is a percentage equal to 30% + 70%*(RAM/1GB), yielding
     a lower bound of 30% and an upper bound of 100% (when RAM >= 1GB).  */
  min_expand /= 1024*1024*1024;
  min_expand *= 70;
  min_expand = MIN (min_expand, 70);
  min_expand += 30;

  return min_expand;
}

/* Heuristic to set a default for GGC_MIN_HEAPSIZE.  */
int
ggc_min_heapsize_heuristic()
{
  double min_heap_kbytes = physmem_total();

  /* Adjust for rlimits.  */
  min_heap_kbytes = ggc_rlimit_bound (min_heap_kbytes);

  min_heap_kbytes /= 1024; /* convert to Kbytes. */
  
  /* The heuristic is RAM/8, with a lower bound of 4M and an upper
     bound of 128M (when RAM >= 1GB).  */
  min_heap_kbytes /= 8;
  min_heap_kbytes = MAX (min_heap_kbytes, 4 * 1024);
  min_heap_kbytes = MIN (min_heap_kbytes, 128 * 1024);

  return min_heap_kbytes;
}

void
init_ggc_heuristics ()
{
#ifndef ENABLE_GC_ALWAYS_COLLECT
  set_param_value ("ggc-min-expand", ggc_min_expand_heuristic());
  set_param_value ("ggc-min-heapsize", ggc_min_heapsize_heuristic());
#endif
}
