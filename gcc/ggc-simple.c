/* Simple garbage collection for the GNU compiler.
   Copyright (C) 1998, 1999, 2000, 2001 Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "tree.h"
#include "tm_p.h"
#include "flags.h"
#include "varray.h"
#include "ggc.h"
#include "timevar.h"
#include "params.h"

/* Debugging flags.  */

/* Zap memory before freeing to catch dangling pointers.  */
#undef GGC_POISON

/* Collect statistics on how bushy the search tree is.  */
#undef GGC_BALANCE

/* Always verify that the to-be-marked memory is collectable.  */
#undef GGC_ALWAYS_VERIFY

#ifdef ENABLE_GC_CHECKING
#define GGC_POISON
#define GGC_ALWAYS_VERIFY
#endif

#ifndef HOST_BITS_PER_PTR
#define HOST_BITS_PER_PTR  HOST_BITS_PER_LONG
#endif

/* We'd like a balanced tree, but we don't really want to pay for the
   cost of keeping the tree balanced.  We'll settle for the next best
   thing -- nearly balanced.

   In this context, the most natural key is the node pointer itself,
   but due to the way memory managers work, we'd be virtually certain
   to wind up with a completely degenerate straight line.  What's needed
   is to make something more variable, and yet predictable, be more
   significant in the comparison.

   The handiest source of variability is the low bits of the pointer
   value itself.  Any sort of bit/byte swap would do, but such machine
   specific operations are not handy, and we don't want to put that much
   effort into it.  */

#define PTR_KEY(p)	((size_t)p << (HOST_BITS_PER_PTR - 8)		    \
			 | ((size_t)p & 0xff00) << (HOST_BITS_PER_PTR - 24) \
			 | (size_t)p >> 16)

/* GC'able memory; a node in a binary search tree.  */

struct ggc_mem
{
  /* A combination of the standard left/right nodes, indexable by `<'.  */
  struct ggc_mem *sub[2];

  unsigned int mark : 1;
  unsigned int context : 7;
  unsigned int size : 24;

  /* Make sure the data is reasonably aligned.  */
  union {
    HOST_WIDEST_INT i;
#ifdef HAVE_LONG_DOUBLE
    long double d;
#else
    double d;
#endif
  } u;
};

static struct globals
{
  /* Root of the object tree.  */
  struct ggc_mem *root;

  /* Data bytes currently allocated.  */
  size_t allocated;

  /* Data objects currently allocated.  */
  size_t objects;

  /* Data bytes allocated at time of last GC.  */
  size_t allocated_last_gc;

  /* Current context level.  */
  int context;
} G;

/* Local function prototypes.  */

static void tree_insert PARAMS ((struct ggc_mem *));
static int tree_lookup PARAMS ((struct ggc_mem *));
static void clear_marks PARAMS ((struct ggc_mem *));
static void sweep_objs PARAMS ((struct ggc_mem **));
static void ggc_pop_context_1 PARAMS ((struct ggc_mem *, int));

/* For use from debugger.  */
extern void debug_ggc_tree PARAMS ((struct ggc_mem *, int));

#ifdef GGC_BALANCE
extern void debug_ggc_balance PARAMS ((void));
#endif
static void tally_leaves PARAMS ((struct ggc_mem *, int, size_t *, size_t *));

/* Insert V into the search tree.  */

static inline void
tree_insert (v)
     struct ggc_mem *v;
{
  size_t v_key = PTR_KEY (v);
  struct ggc_mem *p, **pp;

  for (pp = &G.root, p = *pp; p ; p = *pp)
    {
      size_t p_key = PTR_KEY (p);
      pp = &p->sub[v_key < p_key];
    }
  *pp = v;
}

/* Return true if V is in the tree.  */

static inline int
tree_lookup (v)
     struct ggc_mem *v;
{
  size_t v_key = PTR_KEY (v);
  struct ggc_mem *p = G.root;

  while (p)
    {
      size_t p_key = PTR_KEY (p);
      if (p == v)
	return 1;
      p = p->sub[v_key < p_key];
    }

  return 0;
}

/* Alloc SIZE bytes of GC'able memory.  If ZERO, clear the memory.  */

void *
ggc_alloc (size)
     size_t size;
{
  struct ggc_mem *x;

  x = (struct ggc_mem *) xmalloc (offsetof (struct ggc_mem, u) + size);
  x->sub[0] = NULL;
  x->sub[1] = NULL;
  x->mark = 0;
  x->context = G.context;
  x->size = size;

#ifdef GGC_POISON
  memset (&x->u, 0xaf, size);
#endif

  tree_insert (x);
  G.allocated += size;
  G.objects += 1;

  return &x->u;
}

/* Mark a node.  */

int
ggc_set_mark (p)
     const void *p;
{
  struct ggc_mem *x;

  x = (struct ggc_mem *) ((const char *)p - offsetof (struct ggc_mem, u));
#ifdef GGC_ALWAYS_VERIFY
  if (! tree_lookup (x))
    abort ();
#endif

  if (x->mark)
    return 1;

  x->mark = 1;
  G.allocated += x->size;
  G.objects += 1;

  return 0;
}

/* Return 1 if P has been marked, zero otherwise.  */

int
ggc_marked_p (p)
     const void *p;
{
  struct ggc_mem *x;

  x = (struct ggc_mem *) ((const char *)p - offsetof (struct ggc_mem, u));
#ifdef GGC_ALWAYS_VERIFY
  if (! tree_lookup (x))
    abort ();
#endif

   return x->mark;
}

/* Return the size of the gc-able object P.  */

size_t
ggc_get_size (p)
     const void *p;
{
  struct ggc_mem *x
    = (struct ggc_mem *) ((const char *)p - offsetof (struct ggc_mem, u));
  return x->size;
}

/* Unmark all objects.  */

static void
clear_marks (x)
     struct ggc_mem *x;
{
  x->mark = 0;
  if (x->sub[0])
    clear_marks (x->sub[0]);
  if (x->sub[1])
    clear_marks (x->sub[1]);
}

/* Free all objects in the current context that are not marked.  */

static void
sweep_objs (root)
     struct ggc_mem **root;
{
  struct ggc_mem *x = *root;
  if (!x)
    return;

  sweep_objs (&x->sub[0]);
  sweep_objs (&x->sub[1]);

  if (! x->mark && x->context >= G.context)
    {
      struct ggc_mem *l, *r;

      l = x->sub[0];
      r = x->sub[1];
      if (!l)
	*root = r;
      else if (!r)
	*root = l;
      else if (!l->sub[1])
	{
	  *root = l;
	  l->sub[1] = r;
	}
      else if (!r->sub[0])
	{
	  *root = r;
	  r->sub[0] = l;
	}
      else
	{
	  *root = l;
	  do {
	    root = &l->sub[1];
	  } while ((l = *root) != NULL);
	  *root = r;
	}

#ifdef GGC_POISON
      memset (&x->u, 0xA5, x->size);
#endif

      free (x);
    }
}

/* The top level mark-and-sweep routine.  */

void
ggc_collect ()
{
  /* Avoid frequent unnecessary work by skipping collection if the
     total allocations haven't expanded much since the last
     collection.  */
  size_t allocated_last_gc =
    MAX (G.allocated_last_gc, (size_t)PARAM_VALUE (GGC_MIN_HEAPSIZE) * 1024);

  size_t min_expand = allocated_last_gc * PARAM_VALUE (GGC_MIN_EXPAND) / 100;

  if (G.allocated < allocated_last_gc + min_expand)
    return;

#ifdef GGC_BALANCE
  debug_ggc_balance ();
#endif

  timevar_push (TV_GC);
  if (!quiet_flag)
    fprintf (stderr, " {GC %luk -> ", (unsigned long)G.allocated / 1024);

  G.allocated = 0;
  G.objects = 0;

  clear_marks (G.root);
  ggc_mark_roots ();
  sweep_objs (&G.root);

  G.allocated_last_gc = G.allocated;

  timevar_pop (TV_GC);

  if (!quiet_flag)
    fprintf (stderr, "%luk}", (unsigned long) G.allocated / 1024);

#ifdef GGC_BALANCE
  debug_ggc_balance ();
#endif
}

/* Called once to initialize the garbage collector.  */

void
init_ggc ()
{
}

/* Start a new GGC context.  Memory allocated in previous contexts
   will not be collected while the new context is active.  */

void
ggc_push_context ()
{
  G.context++;

  /* We only allocated 7 bits in the node for the context.  This
     should be more than enough.  */
  if (G.context >= 128)
    abort ();
}

/* Finish a GC context.  Any uncollected memory in the new context
   will be merged with the old context.  */

void
ggc_pop_context ()
{
  G.context--;
  if (G.root)
    ggc_pop_context_1 (G.root, G.context);
}

static void
ggc_pop_context_1 (x, c)
     struct ggc_mem *x;
     int c;
{
  if (x->context > c)
    x->context = c;
  if (x->sub[0])
    ggc_pop_context_1 (x->sub[0], c);
  if (x->sub[1])
    ggc_pop_context_1 (x->sub[1], c);
}

/* Dump a tree.  */

void
debug_ggc_tree (p, indent)
     struct ggc_mem *p;
     int indent;
{
  int i;

  if (!p)
    {
      fputs ("(nil)\n", stderr);
      return;
    }

  if (p->sub[0])
    debug_ggc_tree (p->sub[0], indent + 1);

  for (i = 0; i < indent; ++i)
    putc (' ', stderr);
  fprintf (stderr, "%lx %p\n", (unsigned long)PTR_KEY (p), p);

  if (p->sub[1])
    debug_ggc_tree (p->sub[1], indent + 1);
}

#ifdef GGC_BALANCE
/* Collect tree balance metrics  */

#include <math.h>

void
debug_ggc_balance ()
{
  size_t nleaf, sumdepth;

  nleaf = sumdepth = 0;
  tally_leaves (G.root, 0, &nleaf, &sumdepth);

  fprintf (stderr, " {B %.2f,%.1f,%.1f}",
	   /* In a balanced tree, leaf/node should approach 1/2.  */
	   (float)nleaf / (float)G.objects,
	   /* In a balanced tree, average leaf depth should approach lg(n).  */
	   (float)sumdepth / (float)nleaf,
	   log ((double) G.objects) / M_LN2);
}
#endif

/* Used by debug_ggc_balance, and also by ggc_print_statistics.  */
static void
tally_leaves (x, depth, nleaf, sumdepth)
     struct ggc_mem *x;
     int depth;
     size_t *nleaf;
     size_t *sumdepth;
{
  if (! x->sub[0] && !x->sub[1])
    {
      *nleaf += 1;
      *sumdepth += depth;
    }
  else
    {
      if (x->sub[0])
	tally_leaves (x->sub[0], depth + 1, nleaf, sumdepth);
      if (x->sub[1])
	tally_leaves (x->sub[1], depth + 1, nleaf, sumdepth);
    }
}

#define SCALE(x) ((unsigned long) ((x) < 1024*10 \
		  ? (x) \
		  : ((x) < 1024*1024*10 \
		     ? (x) / 1024 \
		     : (x) / (1024*1024))))
#define LABEL(x) ((x) < 1024*10 ? ' ' : ((x) < 1024*1024*10 ? 'k' : 'M'))

/* Report on GC memory usage.  */
void
ggc_print_statistics ()
{
  struct ggc_statistics stats;
  size_t nleaf = 0, sumdepth = 0;

  /* Clear the statistics.  */
  memset (&stats, 0, sizeof (stats));

  /* Make sure collection will really occur.  */
  G.allocated_last_gc = 0;

  /* Collect and print the statistics common across collectors.  */
  ggc_print_common_statistics (stderr, &stats);

  /* Report on tree balancing.  */
  tally_leaves (G.root, 0, &nleaf, &sumdepth);

  fprintf (stderr, "\n\
Total internal data (bytes)\t%ld%c\n\
Number of leaves in tree\t%d\n\
Average leaf depth\t\t%.1f\n",
	   SCALE(G.objects * offsetof (struct ggc_mem, u)),
	   LABEL(G.objects * offsetof (struct ggc_mem, u)),
	   nleaf, (double)sumdepth / (double)nleaf);

  /* Report overall memory usage.  */
  fprintf (stderr, "\n\
Total objects allocated\t\t%d\n\
Total memory in GC arena\t%ld%c\n",
	   G.objects,
	   SCALE(G.allocated), LABEL(G.allocated));
}
