/* Partial red-black tree implementation for rs6000-gen-builtins.cc.
   Copyright (C) 2020-2023 Free Software Foundation, Inc.
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "rbtree.h"

/* Initialize a red-black tree.  */
void
rbt_new (struct rbt_strings *t)
{
  t->rbt_nil = (rbt_string_node *) malloc (sizeof (rbt_string_node));
  t->rbt_nil->color = RBT_BLACK;
  t->rbt_root = t->rbt_nil;
}

/* Create a new node to be inserted into the red-black tree.  An inserted
   node starts out red.  */
static struct rbt_string_node *
rbt_create_node (struct rbt_strings *t, char *str)
{
  struct rbt_string_node *nodeptr
    = (struct rbt_string_node *) malloc (sizeof (rbt_string_node));
  nodeptr->str = str;
  nodeptr->left = t->rbt_nil;
  nodeptr->right = t->rbt_nil;
  nodeptr->par = NULL;
  nodeptr->color = RBT_RED;
  return nodeptr;
}

/* Perform a left-rotate operation on NODE in the red-black tree.  */
static void
rbt_left_rotate (struct rbt_strings *t, struct rbt_string_node *node)
{
  struct rbt_string_node *right = node->right;
  assert (right);

  /* Turn RIGHT's left subtree into NODE's right subtree.  */
  node->right = right->left;
  if (right->left != t->rbt_nil)
    right->left->par = node;

  /* Link NODE's parent to RIGHT.  */
  right->par = node->par;

  if (node->par == t->rbt_nil)
    t->rbt_root = right;
  else if (node == node->par->left)
    node->par->left = right;
  else
    node->par->right = right;

  /* Put NODE on RIGHT's left.  */
  right->left = node;
  node->par = right;
}

/* Perform a right-rotate operation on NODE in the red-black tree.  */
static void
rbt_right_rotate (struct rbt_strings *t, struct rbt_string_node *node)
{
  struct rbt_string_node *left = node->left;
  assert (left);

  /* Turn LEFT's right subtree into NODE's left subtree.  */
  node->left = left->right;
  if (left->right != t->rbt_nil)
    left->right->par = node;

  /* Link NODE's parent to LEFT.  */
  left->par = node->par;

  if (node->par == t->rbt_nil)
    t->rbt_root = left;
  else if (node == node->par->right)
    node->par->right = left;
  else
    node->par->left = left;

  /* Put NODE on LEFT's right.  */
  left->right = node;
  node->par = left;
}

/* Insert STR into the tree, returning 1 for success and 0 if STR already
   appears in the tree.  */
int
rbt_insert (struct rbt_strings *t, char *str)
{
  struct rbt_string_node *curr = t->rbt_root;
  struct rbt_string_node *trail = t->rbt_nil;

  while (curr != t->rbt_nil)
    {
      trail = curr;
      int cmp = strcmp (str, curr->str);
      if (cmp < 0)
	curr = curr->left;
      else if (cmp > 0)
	curr = curr->right;
      else
	return 0;
    }

  struct rbt_string_node *fresh = rbt_create_node (t, str);
  fresh->par = trail;

  if (trail == t->rbt_nil)
    t->rbt_root = fresh;
  else if (strcmp (fresh->str, trail->str) < 0)
    trail->left = fresh;
  else
    trail->right = fresh;

  fresh->left = t->rbt_nil;
  fresh->right = t->rbt_nil;

  /* FRESH has now been inserted as a red leaf.  If we have invalidated
     one of the following preconditions, we must fix things up:
      (a) If a node is red, both of its children are black.
      (b) The root must be black.
     Note that only (a) or (b) applies at any given time during the
     process.  This algorithm works up the tree from NEW looking
     for a red child with a red parent, and cleaning that up.  If the
     root ends up red, it gets turned black at the end.  */
  curr = fresh;
  while (curr->par->color == RBT_RED)
    if (curr->par == curr->par->par->left)
      {
	struct rbt_string_node *uncle = curr->par->par->right;
	if (uncle->color == RBT_RED)
	  {
	    curr->par->color = RBT_BLACK;
	    uncle->color = RBT_BLACK;
	    curr->par->par->color = RBT_RED;
	    curr = curr->par->par;
	  }
	else if (curr == curr->par->right)
	  {
	    curr = curr->par;
	    rbt_left_rotate (t, curr);
	  }
	else
	  {
	    curr->par->color = RBT_BLACK;
	    curr->par->par->color = RBT_RED;
	    rbt_right_rotate (t, curr->par->par);
	  }
      }
    else /* curr->par == curr->par->par->right  */
      {
	/* Gender-neutral formations are awkward, so let's be fair. ;-)
	   ("Parent-sibling" is just awful.)  */
	struct rbt_string_node *aunt = curr->par->par->left;
	if (aunt->color == RBT_RED)
	  {
	    curr->par->color = RBT_BLACK;
	    aunt->color = RBT_BLACK;
	    curr->par->par->color = RBT_RED;
	    curr = curr->par->par;
	  }
	else if (curr == curr->par->left)
	  {
	    curr = curr->par;
	    rbt_right_rotate (t, curr);
	  }
	else
	  {
	    curr->par->color = RBT_BLACK;
	    curr->par->par->color = RBT_RED;
	    rbt_left_rotate (t, curr->par->par);
	  }
      }

  t->rbt_root->color = RBT_BLACK;
  return 1;
}

/* Return 1 if STR is in the red-black tree, else 0.  */
int
rbt_find (struct rbt_strings *t, char *str)
{
  struct rbt_string_node *curr = t->rbt_root;

  while (curr != t->rbt_nil)
    {
      int cmp = strcmp (str, curr->str);
      if (cmp < 0)
	curr = curr->left;
      else if (cmp > 0)
	curr = curr->right;
      else
	return 1;
    }

  return 0;
}

/* Inorder dump of the binary search tree.  */
void
rbt_dump (struct rbt_strings *t, struct rbt_string_node *subtree)
{
  if (subtree != t->rbt_nil)
    {
      rbt_dump (t, subtree->left);
      fprintf (stderr, "%s\n", subtree->str);
      rbt_dump (t, subtree->right);
    }
}

/* Inorder call-back for iteration over the tree.  */
void
rbt_inorder_callback (struct rbt_strings *t, struct rbt_string_node *subtree,
		      void (*fn) (char *))
{
  if (subtree != t->rbt_nil)
    {
      rbt_inorder_callback (t, subtree->left, fn);
      (*fn) (subtree->str);
      rbt_inorder_callback (t, subtree->right, fn);
    }
}
