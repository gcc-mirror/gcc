/* Tree inlining hooks and declarations.
   Copyright 2001 Free Software Foundation, Inc.
   Contributed by Alexandre Oliva  <aoliva@redhat.com>

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

#ifndef GCC_TREE_INLINE_H
#define GCC_TREE_INLINE_H

/* Function prototypes.  */

void optimize_inline_calls PARAMS ((tree));
int tree_inlinable_function_p PARAMS ((tree));
tree walk_tree PARAMS ((tree*, walk_tree_fn, void*, void*));
tree walk_tree_without_duplicates PARAMS ((tree*, walk_tree_fn, void*));
tree copy_tree_r PARAMS ((tree*, int*, void*));
void clone_body PARAMS ((tree, tree, void*));
void remap_save_expr PARAMS ((tree*, void*, tree, int*));

/* 0 if we should not perform inlining.
   1 if we should expand functions calls inline at the tree level.
   2 if we should consider *all* functions to be inline
   candidates.  */

extern int flag_inline_trees;

#endif /* GCC_TREE_INLINE_H */
