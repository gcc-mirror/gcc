/* Garbage collection for the GNU compiler.
   Copyright (C) 1998, 1999, 2000, 2001, 2002 Free Software Foundation, Inc.

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

#include "varray.h"
#include "gtype-desc.h"

/* Symbols are marked with `ggc' for `gcc gc' so as not to interfere with
   an external gc library that might be linked in.  */

/* Constants for general use.  */
extern const char empty_string[];	/* empty string */
extern const char digit_vector[];	/* "0" .. "9" */
#define digit_string(d) (digit_vector + ((d) * 2))

/* Manipulate global roots that are needed between calls to gc.  
   THIS ROUTINE IS OBSOLETE, do not use it for new code.  */
extern void ggc_add_root		PARAMS ((void *base, int nelt,
						 int size, void (*)(void *)));

/* Structures for the easy way to mark roots.
   In an array, terminated by having base == NULL.*/
struct ggc_root_tab {
  void *base;
  size_t nelt;
  size_t stride;
  void (*cb) PARAMS ((void *));
};
#define LAST_GGC_ROOT_TAB { NULL, 0, 0, NULL }
/* Pointers to arrays of ggc_root_tab, terminated by NULL.  */
extern const struct ggc_root_tab * const gt_ggc_rtab[];
extern const struct ggc_root_tab * const gt_ggc_deletable_rtab[];

/* Structure for hash table cache marking.  */
struct htab;
struct ggc_cache_tab {
  struct htab * *base;
  size_t nelt;
  size_t stride;
  void (*cb) PARAMS ((void *));
  int (*marked_p) PARAMS ((const void *));
};
#define LAST_GGC_CACHE_TAB { NULL, 0, 0, NULL, NULL }
/* Pointers to arrays of ggc_cache_tab, terminated by NULL.  */
extern const struct ggc_cache_tab * const gt_ggc_cache_rtab[];

extern void ggc_mark_roots		PARAMS ((void));

/* If EXPR is not NULL and previously unmarked, mark it and evaluate
   to true.  Otherwise evaluate to false.  */
#define ggc_test_and_set_mark(EXPR) \
  ((EXPR) != NULL && ((void *) (EXPR)) != (void *) 1 && ! ggc_set_mark (EXPR))

#define ggc_mark_rtx gt_ggc_m_7rtx_def
#define ggc_mark_tree gt_ggc_m_9tree_node

#define ggc_mark(EXPR)				\
  do {						\
    const void *const a__ = (EXPR);		\
    if (a__ != NULL && a__ != (void *) 1)	\
      ggc_set_mark (a__);			\
  } while (0)

/* A GC implementation must provide these functions.  */

/* Initialize the garbage collector.  */
extern void init_ggc		PARAMS ((void));
extern void init_stringpool	PARAMS ((void));

/* Start a new GGC context.  Memory allocated in previous contexts
   will not be collected while the new context is active.  */
extern void ggc_push_context	PARAMS ((void));

/* Finish a GC context.  Any uncollected memory in the new context
   will be merged with the old context.  */
extern void ggc_pop_context 	PARAMS ((void));

/* Allocation.  */

/* The internal primitive.  */
extern void *ggc_alloc		PARAMS ((size_t));
/* Like ggc_alloc, but allocates cleared memory.  */
extern void *ggc_alloc_cleared	PARAMS ((size_t));
/* Resize a block.  */
extern void *ggc_realloc	PARAMS ((void *, size_t));
/* Like ggc_alloc_cleared, but performs a multiplication.  */
extern void *ggc_calloc		PARAMS ((size_t, size_t));

#define ggc_alloc_rtx(NSLOTS)						  \
  ((struct rtx_def *) ggc_alloc (sizeof (struct rtx_def)		  \
				 + ((NSLOTS) - 1) * sizeof (rtunion)))

#define ggc_alloc_rtvec(NELT)						  \
  ((struct rtvec_def *) ggc_alloc (sizeof (struct rtvec_def)		  \
				   + ((NELT) - 1) * sizeof (rtx)))

#define ggc_alloc_tree(LENGTH) ((union tree_node *) ggc_alloc (LENGTH))

#define htab_create_ggc(SIZE, HASH, EQ, DEL) \
  htab_create_alloc (SIZE, HASH, EQ, DEL, ggc_calloc, NULL)

/* Allocate a gc-able string, and fill it with LENGTH bytes from CONTENTS.
   If LENGTH is -1, then CONTENTS is assumed to be a
   null-terminated string and the memory sized accordingly.  */
extern const char *ggc_alloc_string	PARAMS ((const char *contents,
						 int length));

/* Make a copy of S, in GC-able memory.  */
#define ggc_strdup(S) ggc_alloc_string((S), -1)

/* Invoke the collector.  Garbage collection occurs only when this
   function is called, not during allocations.  */
extern void ggc_collect			PARAMS ((void));

/* Actually set the mark on a particular region of memory, but don't
   follow pointers.  This function is called by ggc_mark_*.  It
   returns zero if the object was not previously marked; nonzero if
   the object was already marked, or if, for any other reason,
   pointers in this data structure should not be traversed.  */
extern int ggc_set_mark			PARAMS ((const void *));

/* Return 1 if P has been marked, zero otherwise.
   P must have been allocated by the GC allocator; it mustn't point to
   static objects, stack variables, or memory allocated with malloc.  */
extern int ggc_marked_p			PARAMS ((const void *));

/* Statistics.  */

/* This structure contains the statistics common to all collectors.
   Particular collectors can extend this structure.  */
typedef struct ggc_statistics
{
  /* The Ith element is the number of nodes allocated with code I.  */
  unsigned num_trees[256];
  /* The Ith element is the number of bytes allocated by nodes with
     code I.  */
  size_t size_trees[256];
  /* The Ith element is the number of nodes allocated with code I.  */
  unsigned num_rtxs[256];
  /* The Ith element is the number of bytes allocated by nodes with
     code I.  */
  size_t size_rtxs[256];
  /* The total size of the tree nodes allocated.  */
  size_t total_size_trees;
  /* The total size of the RTL nodes allocated.  */
  size_t total_size_rtxs;
  /* The total number of tree nodes allocated.  */
  unsigned total_num_trees;
  /* The total number of RTL nodes allocated.  */
  unsigned total_num_rtxs;
} ggc_statistics;

/* Return the number of bytes allocated at the indicated address.  */
extern size_t ggc_get_size		PARAMS ((const void *));

/* Used by the various collectors to gather and print statistics that
   do not depend on the collector in use.  */
extern void ggc_print_common_statistics PARAMS ((FILE *, ggc_statistics *));

/* Print allocation statistics.  */
extern void ggc_print_statistics	PARAMS ((void));
extern void stringpool_statistics	PARAMS ((void));

/* Heuristics.  */
extern int ggc_min_expand_heuristic PARAMS ((void));
extern int ggc_min_heapsize_heuristic PARAMS ((void));
extern void init_ggc_heuristics PARAMS ((void));
