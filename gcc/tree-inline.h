/* Tree inlining hooks and declarations.
   Copyright 2001, 2003, 2004 Free Software Foundation, Inc.
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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef GCC_TREE_INLINE_H
#define GCC_TREE_INLINE_H

/* Function prototypes.  */

void optimize_inline_calls (tree);
bool tree_inlinable_function_p (tree);
tree copy_tree_r (tree *, int *, void *);
void clone_body (tree, tree, void *);
tree save_body (tree, tree *, tree *);
int estimate_move_cost (tree type);
int estimate_num_insns (tree expr);

/* 0 if we should not perform inlining.
   1 if we should expand functions calls inline at the tree level.
   2 if we should consider *all* functions to be inline
   candidates.  */

extern int flag_inline_trees;

#endif /* GCC_TREE_INLINE_H */
