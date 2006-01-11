/* Tree inlining hooks and declarations.
   Copyright 2001, 2003, 2004, 2005 Free Software Foundation, Inc.
   Contributed by Alexandre Oliva  <aoliva@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#ifndef GCC_TREE_INLINE_H
#define GCC_TREE_INLINE_H

#include "varray.h"
/* Function prototypes.  */

void optimize_inline_calls (tree);
bool tree_inlinable_function_p (tree);
tree copy_tree_r (tree *, int *, void *);
void clone_body (tree, tree, void *);
void save_body (tree, tree *, tree *);
int estimate_move_cost (tree type);
void push_cfun (struct function *new_cfun);
void pop_cfun (void);
int estimate_num_insns (tree expr);
bool tree_versionable_function_p (tree);
void tree_function_versioning (tree, tree, varray_type, bool);

/* Copy a declaration when one function is substituted inline into
   another.  It is used also for versioning.  */
extern tree copy_decl_for_dup (tree, tree, tree, bool);

/* 0 if we should not perform inlining.
   1 if we should expand functions calls inline at the tree level.
   2 if we should consider *all* functions to be inline
   candidates.  */

extern int flag_inline_trees;

#endif /* GCC_TREE_INLINE_H */
