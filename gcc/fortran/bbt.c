/* Balanced binary trees using treaps.
   Copyright (C) 2000, 2002, 2003 Free Software Foundation, Inc.
   Contributed by Andy Vaught

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

/* The idea is to balance the tree using pseudorandom numbers.  The
   main constraint on this implementation is that we have several
   distinct structures that have to be arranged in a binary tree.
   These structures all contain a BBT_HEADER() in front that gives the
   treap-related information.  The key and value are assumed to reside
   in the rest of the structure.

   When calling, we are also passed a comparison function that
   compares two nodes.  We don't implement a separate 'find' function
   here, but rather use separate functions for each variety of tree.
   We are also restricted to not copy treap structures, which most
   implementations find convenient, because we otherwise would need to
   know how long the structure is.

   This implementation is based on Stefan Nilsson's article in the
   July 1997 Doctor Dobb's Journal, "Treaps in Java".  */

#include "config.h"
#include "gfortran.h"

typedef struct gfc_treap
{
  BBT_HEADER (gfc_treap);
}
gfc_bbt;

/* Simple linear congruential pseudorandom number generator.  The
   period of this generator is 44071, which is plenty for our
   purposes.  */

static int
pseudo_random (void)
{
  static int x0 = 5341;

  x0 = (22611 * x0 + 10) % 44071;
  return x0;
}


/* Rotate the treap left.  */

static gfc_bbt *
rotate_left (gfc_bbt * t)
{
  gfc_bbt *temp;

  temp = t->right;
  t->right = t->right->left;
  temp->left = t;

  return temp;
}


/* Rotate the treap right.  */

static gfc_bbt *
rotate_right (gfc_bbt * t)
{
  gfc_bbt *temp;

  temp = t->left;
  t->left = t->left->right;
  temp->right = t;

  return temp;
}


/* Recursive insertion function.  Returns the updated treap, or
   aborts if we find a duplicate key.  */

static gfc_bbt *
insert (gfc_bbt * new, gfc_bbt * t, compare_fn compare)
{
  int c;

  if (t == NULL)
    return new;

  c = (*compare) (new, t);

  if (c < 0)
    {
      t->left = insert (new, t->left, compare);
      if (t->priority < t->left->priority)
	t = rotate_right (t);
    }

  else if (c > 0)
    {
      t->right = insert (new, t->right, compare);
      if (t->priority < t->right->priority)
	t = rotate_left (t);
    }

  else /* if (c == 0)  */
    gfc_internal_error("insert_bbt(): Duplicate key found!");

  return t;
}


/* Given root pointer, a new node and a comparison function, insert
   the new node into the treap.  It is an error to insert a key that
   already exists.  */

void
gfc_insert_bbt (void *root, void *new, compare_fn compare)
{
  gfc_bbt **r, *n;

  r = (gfc_bbt **) root;
  n = (gfc_bbt *) new;

  n->priority = pseudo_random ();
  *r = insert (n, *r, compare);
}

static gfc_bbt *
delete_root (gfc_bbt * t)
{
  gfc_bbt *temp;

  if (t->left == NULL)
    return t->right;
  if (t->right == NULL)
    return t->left;

  if (t->left->priority > t->right->priority)
    {
      temp = rotate_right (t);
      temp->right = delete_root (t);
    }
  else
    {
      temp = rotate_left (t);
      temp->left = delete_root (t);
    }

  return temp;
}


/* Delete an element from a tree.  The 'old' value does not
   necessarily have to point to the element to be deleted, it must
   just point to a treap structure with the key to be deleted.
   Returns the new root node of the tree.  */

static gfc_bbt *
delete_treap (gfc_bbt * old, gfc_bbt * t, compare_fn compare)
{
  int c;

  if (t == NULL)
    return NULL;

  c = (*compare) (old, t);

  if (c < 0)
    t->left = delete_treap (old, t->left, compare);
  if (c > 0)
    t->right = delete_treap (old, t->right, compare);
  if (c == 0)
    t = delete_root (t);

  return t;
}


void
gfc_delete_bbt (void *root, void *old, compare_fn compare)
{
  gfc_bbt **t;

  t = (gfc_bbt **) root;

  *t = delete_treap ((gfc_bbt *) old, *t, compare);
}
