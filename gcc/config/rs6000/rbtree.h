/* Partial red-black tree implementation for rs6000-gen-builtins.c.
   Copyright (C) 2020-21 Free Software Foundation, Inc.
   Contributed by Bill Schmidt, IBM <wschmidt@linux.ibm.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* Red-black binary search tree on strings.  Presently we don't support
   deletes; only insert/find operations are implemented.  */
enum rbt_color
  {
    RBT_BLACK,
    RBT_RED
  };

struct rbt_string_node {
  char *str;
  struct rbt_string_node *left;
  struct rbt_string_node *right;
  struct rbt_string_node *par;
  enum rbt_color color;
};

/* Root and sentinel nodes of a red-black tree.
   rbt_nil points to a sentinel node, which is the parent of root
   and the child of every node without a "real" left or right child.
   rbt_root points to the root of the tree, if it exists yet.  The
   root and sentinel nodes are always black.  */
struct rbt_strings {
  struct rbt_string_node *rbt_nil;
  struct rbt_string_node *rbt_root;
};

void rbt_new (struct rbt_strings *);
int rbt_insert (struct rbt_strings *, char *);
int rbt_find (struct rbt_strings *, char *);
void rbt_dump (struct rbt_strings *, struct rbt_string_node *);
void rbt_inorder_callback (struct rbt_strings *, struct rbt_string_node *,
			   void (*) (char *));
