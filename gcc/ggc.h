/* Garbage collection for the GNU compiler.
   Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.

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

#include "gansidecl.h"

/* Symbols are marked with `ggc' for `gcc gc' so as not to interfere with
   an external gc library that might be linked in.  */

/* Language-specific code defines this variable to be either one (if
   it wants garbage collection), or zero (if it does not).  */
extern int ggc_p;

/* These structures are defined in various headers throughout the
   compiler.  However, rather than force everyone who includes this
   header to include all the headers in which they are declared, we
   just forward-declare them here.  */
struct eh_status;
struct emit_status;
struct expr_status;
struct hash_table;
struct label_node;
struct rtvec_def;
struct stmt_status;
union  tree_node;
struct varasm_status;
struct varray_head_tag;

/* Constants for general use.  */
extern char *empty_string;

/* Manipulate global roots that are needed between calls to gc.  */
void ggc_add_root PARAMS ((void *base, int nelt, int size, void (*)(void *)));
void ggc_add_rtx_root PARAMS ((struct rtx_def **, int nelt));
void ggc_add_tree_root PARAMS ((union tree_node **, int nelt));
void ggc_add_string_root PARAMS ((char **, int nelt));
void ggc_add_tree_varray_root PARAMS ((struct varray_head_tag **, int nelt));
void ggc_add_tree_hash_table_root PARAMS ((struct hash_table **, int nelt));
void ggc_del_root PARAMS ((void *base));

/* Mark nodes from the gc_add_root callback.  These functions follow
   pointers to mark other objects too.  */
extern void ggc_mark_tree_varray PARAMS ((struct varray_head_tag *));
extern void ggc_mark_tree_hash_table PARAMS ((struct hash_table *));
extern void ggc_mark_roots PARAMS ((void));

extern void ggc_mark_rtx_children PARAMS ((struct rtx_def *));
extern void ggc_mark_rtvec_children PARAMS ((struct rtvec_def *));
extern void ggc_mark_tree_children PARAMS ((union tree_node *));

/* If EXPR is not NULL and previously unmarked, mark it and evaluate
   to true.  Otherwise evaluate to false.  */
#define ggc_test_and_set_mark(EXPR) \
  ((EXPR) != NULL && ! ggc_set_mark (EXPR))

#define ggc_mark_rtx(EXPR)                      \
  do {                                          \
    rtx r__ = (EXPR);                           \
    if (ggc_test_and_set_mark (r__))            \
      ggc_mark_rtx_children (r__);              \
  } while (0)

#define ggc_mark_tree(EXPR)                     \
  do {                                          \
    tree t__ = (EXPR);                          \
    if (ggc_test_and_set_mark (t__))            \
      ggc_mark_tree_children (t__);             \
  } while (0)

#define ggc_mark_rtvec(EXPR)                    \
  do {                                          \
    rtvec v__ = (EXPR);                         \
    if (ggc_test_and_set_mark (v__))            \
      ggc_mark_rtvec_children (v__);            \
  } while (0)

#define ggc_mark_string(EXPR)			\
  do {						\
    char *s__ = (EXPR);				\
    if (s__ != NULL)				\
      ggc_set_mark (s__);			\
  } while (0)

#define ggc_mark(EXPR)				\
  do {						\
    void *a__ = (EXPR);				\
    if (a__ != NULL)				\
      ggc_set_mark (a__);			\
  } while (0)

/* Mark, but only if it was allocated in collectable memory.  */
extern void ggc_mark_if_gcable PARAMS ((void *));

/* A GC implementation must provide these functions.  */

/* Initialize the garbage collector.   */
extern void init_ggc PARAMS ((void));

/* Start a new GGC context.  Memory allocated in previous contexts
   will not be collected while the new context is active.  */
extern void ggc_push_context PARAMS ((void));

/* Finish a GC context.  Any uncollected memory in the new context
   will be merged with the old context.  */
extern void ggc_pop_context PARAMS ((void));

/* Allocation.  */

/* The internal primitive.  */
void *ggc_alloc_obj PARAMS ((size_t, int));

#define ggc_alloc_rtx(NSLOTS)						     \
  ((struct rtx_def *) ggc_alloc_obj (sizeof (struct rtx_def)		     \
				     + ((NSLOTS) - 1) * sizeof (rtunion), 1))

#define ggc_alloc_rtvec(NELT)						  \
  ((struct rtvec_def *) ggc_alloc_obj (sizeof (struct rtvec_def)	  \
				       + ((NELT) - 1) * sizeof (rtx), 1))

#define ggc_alloc_tree(LENGTH)				\
  ((union tree_node *) ggc_alloc_obj ((LENGTH), 1))

#define ggc_alloc(SIZE)  ggc_alloc_obj((SIZE), 0)

char *ggc_alloc_string PARAMS ((const char *contents, int length));

/* Invoke the collector.  This is really just a hint, but in the case of
   the simple collector, the only time it will happen.  */
void ggc_collect PARAMS ((void));

/* Actually set the mark on a particular region of memory, but don't
   follow pointers.  This function is called by ggc_mark_*.  It
   returns zero if the object was not previously marked; non-zero if
   the object was already marked, or if, for any other reason,
   pointers in this data structure should not be traversed.  */
int ggc_set_mark PARAMS ((void *));

/* Callbacks to the languages.  */

/* This is the language's opportunity to mark nodes held through
   the lang_specific hooks in the tree.  */
void lang_mark_tree PARAMS ((union tree_node *));

/* The FALSE_LABEL_STACK, declared in except.h, has
   language-dependent semantics.  Each front-end should define this
   function appropriately.  */
void lang_mark_false_label_stack PARAMS ((struct label_node *));

/* Mark functions for various structs scattered about.  */

void mark_eh_status PARAMS ((struct eh_status *));
void mark_emit_status PARAMS ((struct emit_status *));
void mark_expr_status PARAMS ((struct expr_status *));
void mark_stmt_status PARAMS ((struct stmt_status *));
void mark_varasm_status PARAMS ((struct varasm_status *));
void mark_optab PARAMS ((void *));

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
size_t ggc_get_size PARAMS ((void *));

/* Used by the various collectors to gather and print statistics that
   do not depend on the collector in use.  */
void ggc_print_statistics PARAMS ((FILE *, ggc_statistics *));
