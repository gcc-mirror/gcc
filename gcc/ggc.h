/* Garbage collection for the GNU compiler.
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
void ggc_add_root PROTO ((void *base, int nelt, int size, void (*)(void *)));
void ggc_add_rtx_root PROTO ((struct rtx_def **, int nelt));
void ggc_add_tree_root PROTO ((union tree_node **, int nelt));
void ggc_add_string_root PROTO ((char **, int nelt));
void ggc_add_tree_varray_root PROTO ((struct varray_head_tag **, int nelt));
void ggc_add_tree_hash_table_root PROTO ((struct hash_table **, int nelt));
void ggc_del_root PROTO ((void *base));

/* Mark nodes from the gc_add_root callback.  These functions follow
   pointers to mark other objects too.  */
extern void ggc_mark_rtvec PROTO ((struct rtvec_def *));
extern void ggc_mark_tree_varray PROTO ((struct varray_head_tag *));
extern void ggc_mark_tree_hash_table PROTO ((struct hash_table *));
extern void ggc_mark_string PROTO ((char *));
extern void ggc_mark PROTO ((void *));
extern void ggc_mark_roots PROTO((void));

extern void ggc_mark_rtx_children PROTO ((struct rtx_def *));
extern void ggc_mark_tree_children PROTO ((union tree_node *));

/* Mark the string, but only if it was allocated in collectable
   memory.  */
extern void ggc_mark_string_if_gcable PROTO ((char *));

#define ggc_mark_rtx(RTX_EXPR)				\
  do {							\
    rtx r__ = (RTX_EXPR);				\
    if (r__ != NULL && ! ggc_set_mark_rtx (r__))	\
      ggc_mark_rtx_children (r__);			\
  } while (0)

#define ggc_mark_tree(TREE_EXPR)			\
  do {							\
    tree t__ = (TREE_EXPR);				\
    if (t__ != NULL && ! ggc_set_mark_tree (t__))	\
      ggc_mark_tree_children (t__);			\
  } while (0)

/* A GC implementation must provide these functions.  */

/* Initialize the garbage collector.   */
extern void init_ggc PROTO ((void));

/* Start a new GGC context.  Memory allocated in previous contexts
   will not be collected while the new context is active.  */
extern void ggc_push_context PROTO ((void));

/* Finish a GC context.  Any uncollected memory in the new context
   will be merged with the old context.  */
extern void ggc_pop_context PROTO ((void));

/* Allocation.  */
struct rtx_def *ggc_alloc_rtx PROTO ((int nslots));
struct rtvec_def *ggc_alloc_rtvec PROTO ((int nelt));
union tree_node *ggc_alloc_tree PROTO ((int length));
char *ggc_alloc_string PROTO ((const char *contents, int length));
void *ggc_alloc PROTO ((size_t));

/* Invoke the collector.  This is really just a hint, but in the case of
   the simple collector, the only time it will happen.  */
void ggc_collect PROTO ((void));

/* Actually set the mark on a particular region of memory, but don't
   follow pointers.  These functions are called by ggc_mark_*.  They
   return zero if the object was not previously marked; they return
   non-zero if the object was already marked, or if, for any other
   reason, pointers in this data structure should not be traversed.  */
int ggc_set_mark_rtx PROTO ((struct rtx_def *));
int ggc_set_mark_rtvec PROTO ((struct rtvec_def *));
int ggc_set_mark_tree PROTO ((union tree_node *));

/* Callbacks to the languages.  */

/* This is the language's opportunity to mark nodes held through
   the lang_specific hooks in the tree.  */
void lang_mark_tree PROTO ((union tree_node *));

/* The FALSE_LABEL_STACK, declared in except.h, has
   language-dependent semantics.  Each front-end should define this
   function appropriately.  */
void lang_mark_false_label_stack PROTO ((struct label_node *));

/* Mark functions for various structs scattered about.  */

void mark_eh_status PROTO ((struct eh_status *));
void mark_emit_status PROTO ((struct emit_status *));
void mark_expr_status PROTO ((struct expr_status *));
void mark_stmt_status PROTO ((struct stmt_status *));
void mark_varasm_status PROTO ((struct varasm_status *));
void mark_optab PROTO ((void *));
