/* Simple garbage collection for the GNU compiler.
   Copyright (C) 1998, 1999 Free Software Foundation, Inc.

   This file is part of GNU CC.

   GNU CC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GNU CC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU CC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "tree.h"
#include "tm_p.h"
#include "flags.h"
#include "varray.h"
#include "ggc.h"

#ifndef offsetof
#define offsetof(TYPE, MEMBER)	((size_t) &((TYPE *)0)->MEMBER)
#endif

/* Debugging flags.  */

/* Zap memory before freeing to catch dangling pointers.  */
#define GGC_POISON

/* Collect statistics on how bushy the search tree is.  */
#undef GGC_BALANCE

/* Perform collection every time ggc_collect is invoked.  Otherwise,
   collection is performed only when a significant amount of memory
   has been allocated since the last collection.  */
#undef GGC_ALWAYS_COLLECT

/* Always verify that the to-be-marked memory is collectable.  */
#undef GGC_ALWAYS_VERIFY

#ifdef ENABLE_GC_CHECKING
#define GGC_POISON
#define GGC_ALWAYS_VERIFY
#endif
#ifdef ENABLE_GC_ALWAYS_COLLECT
#define GGC_ALWAYS_COLLECT
#endif

/* Constants for general use.  */

char *empty_string;
extern int gc_time;

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

/* Skip garbage collection if the current allocation is not at least
   this factor times the allocation at the end of the last collection.
   In other words, total allocation must expand by (this factor minus
   one) before collection is performed.  */
#define GGC_MIN_EXPAND_FOR_GC (1.3)

/* Bound `allocated_last_gc' to 4MB, to prevent the memory expansion
   test from triggering too often when the heap is small.  */
#define GGC_MIN_LAST_ALLOCATED (4 * 1024 * 1024)

/* Local function prototypes.  */

static void tree_insert PARAMS ((struct ggc_mem *));
static int tree_lookup PARAMS ((struct ggc_mem *));
static void clear_marks PARAMS ((struct ggc_mem *));
static void sweep_objs PARAMS ((struct ggc_mem **));
static void ggc_pop_context_1 PARAMS ((struct ggc_mem *, int));

#ifdef GGC_BALANCE
extern void debug_ggc_balance PARAMS ((void));
static void tally_leaves PARAMS ((struct ggc_mem *, int, size_t *, size_t *));
#endif

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
ggc_alloc_obj (size, zero)
     size_t size;
     int zero;
{
  struct ggc_mem *x;

  x = (struct ggc_mem *) xmalloc (offsetof (struct ggc_mem, u) + size);
  x->sub[0] = NULL;
  x->sub[1] = NULL;
  x->mark = 0;
  x->context = G.context;
  x->size = size;

  if (zero)
    memset (&x->u, 0, size);
#ifdef GGC_POISON
  else
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
     void *p;
{
  struct ggc_mem *x;

  x = (struct ggc_mem *) ((char *)p - offsetof (struct ggc_mem, u));
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

/* Mark a node, but check first to see that it's really gc-able memory.  */

void
ggc_mark_if_gcable (p)
     void *p;
{
  struct ggc_mem *x;

  if (p == NULL)
    return;

  x = (struct ggc_mem *) ((char *)p - offsetof (struct ggc_mem, u));
  if (! tree_lookup (x))
    return;

  if (x->mark)
    return;

  x->mark = 1;
  G.allocated += x->size;
  G.objects += 1;
}

/* Return the size of the gc-able object P.  */

size_t
ggc_get_size (p)
     void *p;
{
  struct ggc_mem *x 
    = (struct ggc_mem *) ((char *)p - offsetof (struct ggc_mem, u));
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
  int time;

#ifndef GGC_ALWAYS_COLLECT
  if (G.allocated < GGC_MIN_EXPAND_FOR_GC * G.allocated_last_gc)
    return;
#endif

#ifdef GGC_BALANCE
  debug_ggc_balance ();
#endif

  time = get_run_time ();
  if (!quiet_flag)
    fprintf (stderr, " {GC %luk -> ", (unsigned long)G.allocated / 1024);

  G.allocated = 0;
  G.objects = 0;

  clear_marks (G.root);
  ggc_mark_roots ();
  sweep_objs (&G.root);

  G.allocated_last_gc = G.allocated;
  if (G.allocated_last_gc < GGC_MIN_LAST_ALLOCATED)
    G.allocated_last_gc = GGC_MIN_LAST_ALLOCATED;

  time = get_run_time () - time;
  gc_time += time;

  if (!quiet_flag)
    {
      fprintf (stderr, "%luk in %.3f}", 
	       (unsigned long) G.allocated / 1024, time * 1e-6);
    }

#ifdef GGC_BALANCE
  debug_ggc_balance ();
#endif
}

/* Called once to initialize the garbage collector.  */

void 
init_ggc ()
{
  G.allocated_last_gc = GGC_MIN_LAST_ALLOCATED;

  empty_string = ggc_alloc_string ("", 0);
  ggc_add_string_root (&empty_string, 1);
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
  fprintf (stderr, "%lx %p\n", PTR_KEY (p), p);
 
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
#endif
