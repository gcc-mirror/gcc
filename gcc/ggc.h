/* Garbage collection for the GNU compiler.
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003
   Free Software Foundation, Inc.

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

#ifndef GCC_GGC_H
#define GCC_GGC_H

/* Symbols are marked with `ggc' for `gcc gc' so as not to interfere with
   an external gc library that might be linked in.  */

/* Constants for general use.  */
extern const char empty_string[];	/* empty string */
extern const char digit_vector[];	/* "0" .. "9" */
#define digit_string(d) (digit_vector + ((d) * 2))

/* Internal functions and data structures used by the GTY
   machinery.  */

/* The first parameter is a pointer to a pointer, the second a cookie.  */
typedef void (*gt_pointer_operator) (void *, void *);

#include "gtype-desc.h"

/* One of these applies its third parameter (with cookie in the fourth
   parameter) to each pointer in the object pointed to by the first
   parameter, using the second parameter.  */
typedef void (*gt_note_pointers) (void *, void *, gt_pointer_operator,
				  void *);

/* One of these is called before objects are re-ordered in memory.
   The first parameter is the original object, the second is the
   subobject that has had its pointers reordered, the third parameter
   can compute the new values of a pointer when given the cookie in
   the fourth parameter.  */
typedef void (*gt_handle_reorder) (void *, void *, gt_pointer_operator,
				   void *);

/* Used by the gt_pch_n_* routines.  Register an object in the hash table.  */
extern int gt_pch_note_object (void *, void *, gt_note_pointers);

/* Used by the gt_pch_n_* routines.  Register that an object has a reorder
   function.  */
extern void gt_pch_note_reorder (void *, void *, gt_handle_reorder);

/* Mark the object in the first parameter and anything it points to.  */
typedef void (*gt_pointer_walker) (void *);

/* Structures for the easy way to mark roots.
   In an array, terminated by having base == NULL.  */
struct ggc_root_tab {
  void *base;
  size_t nelt;
  size_t stride;
  gt_pointer_walker cb;
  gt_pointer_walker pchw;
};
#define LAST_GGC_ROOT_TAB { NULL, 0, 0, NULL, NULL }
/* Pointers to arrays of ggc_root_tab, terminated by NULL.  */
extern const struct ggc_root_tab * const gt_ggc_rtab[];
extern const struct ggc_root_tab * const gt_ggc_deletable_rtab[];
extern const struct ggc_root_tab * const gt_pch_cache_rtab[];
extern const struct ggc_root_tab * const gt_pch_scalar_rtab[];

/* Structure for hash table cache marking.  */
struct htab;
struct ggc_cache_tab {
  struct htab * *base;
  size_t nelt;
  size_t stride;
  gt_pointer_walker cb;
  gt_pointer_walker pchw;
  int (*marked_p) (const void *);
};
#define LAST_GGC_CACHE_TAB { NULL, 0, 0, NULL, NULL, NULL }
/* Pointers to arrays of ggc_cache_tab, terminated by NULL.  */
extern const struct ggc_cache_tab * const gt_ggc_cache_rtab[];

/* If EXPR is not NULL and previously unmarked, mark it and evaluate
   to true.  Otherwise evaluate to false.  */
#define ggc_test_and_set_mark(EXPR) \
  ((EXPR) != NULL && ((void *) (EXPR)) != (void *) 1 && ! ggc_set_mark (EXPR))

#define ggc_mark(EXPR)				\
  do {						\
    const void *const a__ = (EXPR);		\
    if (a__ != NULL && a__ != (void *) 1)	\
      ggc_set_mark (a__);			\
  } while (0)

/* Actually set the mark on a particular region of memory, but don't
   follow pointers.  This function is called by ggc_mark_*.  It
   returns zero if the object was not previously marked; nonzero if
   the object was already marked, or if, for any other reason,
   pointers in this data structure should not be traversed.  */
extern int ggc_set_mark	(const void *);

/* Return 1 if P has been marked, zero otherwise.
   P must have been allocated by the GC allocator; it mustn't point to
   static objects, stack variables, or memory allocated with malloc.  */
extern int ggc_marked_p	(const void *);

/* Mark the entries in the string pool.  */
extern void ggc_mark_stringpool	(void);

/* Call ggc_set_mark on all the roots.  */

extern void ggc_mark_roots (void);

/* Save and restore the string pool entries for PCH.  */

extern void gt_pch_save_stringpool (void);
extern void gt_pch_fixup_stringpool (void);
extern void gt_pch_restore_stringpool (void);

/* PCH and GGC handling for strings, mostly trivial.  */

extern void gt_pch_p_S (void *, void *, gt_pointer_operator, void *);
extern void gt_pch_n_S (const void *);
extern void gt_ggc_m_S (void *);

/* Initialize the string pool.  */
extern void init_stringpool (void);

/* A GC implementation must provide these functions.  They are internal
   to the GC system.  */

/* Forward declare the zone structure.  Only ggc_zone implements this.  */
struct alloc_zone;

/* Initialize the garbage collector.  */
extern void init_ggc (void);

/* Start a new GGC zone.  */
extern struct alloc_zone *new_ggc_zone (const char *);

/* Free a complete GGC zone, destroying everything in it.  */
extern void destroy_ggc_zone (struct alloc_zone *);

/* Start a new GGC context.  Memory allocated in previous contexts
   will not be collected while the new context is active.  */
extern void ggc_push_context (void);

/* Finish a GC context.  Any uncollected memory in the new context
   will be merged with the old context.  */
extern void ggc_pop_context (void);

struct ggc_pch_data;

/* Return a new ggc_pch_data structure.  */
extern struct ggc_pch_data *init_ggc_pch (void);

/* The second parameter and third parameters give the address and size
   of an object.  Update the ggc_pch_data structure with as much of
   that information as is necessary. The last argument should be true
   if the object is a string.  */
extern void ggc_pch_count_object (struct ggc_pch_data *, void *, size_t, bool);

/* Return the total size of the data to be written to hold all
   the objects previously passed to ggc_pch_count_object.  */
extern size_t ggc_pch_total_size (struct ggc_pch_data *);

/* The objects, when read, will most likely be at the address
   in the second parameter.  */
extern void ggc_pch_this_base (struct ggc_pch_data *, void *);

/* Assuming that the objects really do end up at the address
   passed to ggc_pch_this_base, return the address of this object.
   The last argument should be true if the object is a string.  */
extern char *ggc_pch_alloc_object (struct ggc_pch_data *, void *, size_t, bool);

/* Write out any initial information required.  */
extern void ggc_pch_prepare_write (struct ggc_pch_data *, FILE *);
/* Write out this object, including any padding.  The last argument should be
   true if the object is a string.  */
extern void ggc_pch_write_object (struct ggc_pch_data *, FILE *, void *,
				  void *, size_t, bool);
/* All objects have been written, write out any final information
   required.  */
extern void ggc_pch_finish (struct ggc_pch_data *, FILE *);

/* A PCH file has just been read in at the address specified second
   parameter.  Set up the GC implementation for the new objects.  */
extern void ggc_pch_read (FILE *, void *);


/* Allocation.  */

/* For single pass garbage.  */
extern struct alloc_zone *garbage_zone;
/* For regular rtl allocations.  */
extern struct alloc_zone *rtl_zone;
/* For regular tree allocations.  */
extern struct alloc_zone *tree_zone;

/* The internal primitive.  */
extern void *ggc_alloc (size_t);
/* Allocate an object into the specified allocation zone.  */
extern void *ggc_alloc_zone (size_t, struct alloc_zone *);
/* Allocate an object of the specified type and size.  */
extern void *ggc_alloc_typed (enum gt_types_enum, size_t);
/* Like ggc_alloc, but allocates cleared memory.  */
extern void *ggc_alloc_cleared (size_t);
/* Like ggc_alloc_zone, but allocates cleared memory.  */
extern void *ggc_alloc_cleared_zone (size_t, struct alloc_zone *);
/* Resize a block.  */
extern void *ggc_realloc (void *, size_t);
/* Like ggc_alloc_cleared, but performs a multiplication.  */
extern void *ggc_calloc (size_t, size_t);

#define ggc_alloc_rtx(CODE)                    \
  ((rtx) ggc_alloc_typed (gt_ggc_e_7rtx_def, RTX_SIZE (CODE)))

#define ggc_alloc_rtvec(NELT)						  \
  ((rtvec) ggc_alloc_typed (gt_ggc_e_9rtvec_def, sizeof (struct rtvec_def) \
		      + ((NELT) - 1) * sizeof (rtx)))

#define ggc_alloc_tree(LENGTH) ((tree) ggc_alloc_zone (LENGTH, tree_zone))

#define htab_create_ggc(SIZE, HASH, EQ, DEL) \
  htab_create_alloc (SIZE, HASH, EQ, DEL, ggc_calloc, NULL)

#define splay_tree_new_ggc(COMPARE)					 \
  splay_tree_new_with_allocator (COMPARE, NULL, NULL,			 \
                                 &ggc_splay_alloc, &ggc_splay_dont_free, \
				 NULL)
extern void *ggc_splay_alloc (int, void *);
extern void ggc_splay_dont_free (void *, void *);

/* Allocate a gc-able string, and fill it with LENGTH bytes from CONTENTS.
   If LENGTH is -1, then CONTENTS is assumed to be a
   null-terminated string and the memory sized accordingly.  */
extern const char *ggc_alloc_string (const char *contents, int length);

/* Make a copy of S, in GC-able memory.  */
#define ggc_strdup(S) ggc_alloc_string((S), -1)

/* Invoke the collector.  Garbage collection occurs only when this
   function is called, not during allocations.  */
extern void ggc_collect	(void);

/* Return the number of bytes allocated at the indicated address.  */
extern size_t ggc_get_size (const void *);

/* Write out all GCed objects to F.  */
extern void gt_pch_save (FILE *f);

/* Read objects previously saved with gt_pch_save from F.  */
extern void gt_pch_restore (FILE *f);

/* Statistics.  */

/* This structure contains the statistics common to all collectors.
   Particular collectors can extend this structure.  */
typedef struct ggc_statistics
{
  /* At present, we don't really gather any interesting statistics.  */
  int unused;
} ggc_statistics;

/* Used by the various collectors to gather and print statistics that
   do not depend on the collector in use.  */
extern void ggc_print_common_statistics (FILE *, ggc_statistics *);

/* Print allocation statistics.  */
extern void ggc_print_statistics (void);
extern void stringpool_statistics (void);

/* Heuristics.  */
extern int ggc_min_expand_heuristic (void);
extern int ggc_min_heapsize_heuristic (void);
extern void init_ggc_heuristics (void);

#endif
