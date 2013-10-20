/* Iterator routines for manipulating GENERIC and GIMPLE tree statements.
   Copyright (C) 2003-2013 Free Software Foundation, Inc.
   Contributed by Andrew MacLeod  <amacleod@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "gimple.h"
#include "tree-iterator.h"
#include "ggc.h"


/* This is a cache of STATEMENT_LIST nodes.  We create and destroy them
   fairly often during gimplification.  */

static GTY ((deletable (""))) vec<tree, va_gc> *stmt_list_cache;

tree
alloc_stmt_list (void)
{
  tree list;
  if (!vec_safe_is_empty (stmt_list_cache))
    {
      list = stmt_list_cache->pop ();
      memset (list, 0, sizeof (struct tree_base));
      TREE_SET_CODE (list, STATEMENT_LIST);
    }
  else
    list = make_node (STATEMENT_LIST);
  TREE_TYPE (list) = void_type_node;
  return list;
}

void
free_stmt_list (tree t)
{
  gcc_assert (!STATEMENT_LIST_HEAD (t));
  gcc_assert (!STATEMENT_LIST_TAIL (t));
  vec_safe_push (stmt_list_cache, t);
}

/* A subroutine of append_to_statement_list{,_force}.  T is not NULL.  */

static void
append_to_statement_list_1 (tree t, tree *list_p)
{
  tree list = *list_p;
  tree_stmt_iterator i;

  if (!list)
    {
      if (t && TREE_CODE (t) == STATEMENT_LIST)
	{
	  *list_p = t;
	  return;
	}
      *list_p = list = alloc_stmt_list ();
    }
  else if (TREE_CODE (list) != STATEMENT_LIST)
    {
      tree first = list;
      *list_p = list = alloc_stmt_list ();
      i = tsi_last (list);
      tsi_link_after (&i, first, TSI_CONTINUE_LINKING);
    }

  i = tsi_last (list);
  tsi_link_after (&i, t, TSI_CONTINUE_LINKING);
}

/* Add T to the end of the list container pointed to by LIST_P.
   If T is an expression with no effects, it is ignored.  */

void
append_to_statement_list (tree t, tree *list_p)
{
  if (t && TREE_SIDE_EFFECTS (t))
    append_to_statement_list_1 (t, list_p);
}

/* Similar, but the statement is always added, regardless of side effects.  */

void
append_to_statement_list_force (tree t, tree *list_p)
{
  if (t != NULL_TREE)
    append_to_statement_list_1 (t, list_p);
}

/* Links a statement, or a chain of statements, before the current stmt.  */

void
tsi_link_before (tree_stmt_iterator *i, tree t, enum tsi_iterator_update mode)
{
  struct tree_statement_list_node *head, *tail, *cur;

  /* Die on looping.  */
  gcc_assert (t != i->container);

  if (TREE_CODE (t) == STATEMENT_LIST)
    {
      head = STATEMENT_LIST_HEAD (t);
      tail = STATEMENT_LIST_TAIL (t);
      STATEMENT_LIST_HEAD (t) = NULL;
      STATEMENT_LIST_TAIL (t) = NULL;

      free_stmt_list (t);

      /* Empty statement lists need no work.  */
      if (!head || !tail)
	{
	  gcc_assert (head == tail);
	  return;
	}
    }
  else
    {
      head = ggc_alloc_tree_statement_list_node ();
      head->prev = NULL;
      head->next = NULL;
      head->stmt = t;
      tail = head;
    }

  TREE_SIDE_EFFECTS (i->container) = 1;

  cur = i->ptr;

  /* Link it into the list.  */
  if (cur)
    {
      head->prev = cur->prev;
      if (head->prev)
	head->prev->next = head;
      else
	STATEMENT_LIST_HEAD (i->container) = head;
      tail->next = cur;
      cur->prev = tail;
    }
  else
    {
      head->prev = STATEMENT_LIST_TAIL (i->container);
      if (head->prev)
       head->prev->next = head;
      else
       STATEMENT_LIST_HEAD (i->container) = head;
      STATEMENT_LIST_TAIL (i->container) = tail;
    }

  /* Update the iterator, if requested.  */
  switch (mode)
    {
    case TSI_NEW_STMT:
    case TSI_CONTINUE_LINKING:
    case TSI_CHAIN_START:
      i->ptr = head;
      break;
    case TSI_CHAIN_END:
      i->ptr = tail;
      break;
    case TSI_SAME_STMT:
      break;
    }
}

/* Links a statement, or a chain of statements, after the current stmt.  */

void
tsi_link_after (tree_stmt_iterator *i, tree t, enum tsi_iterator_update mode)
{
  struct tree_statement_list_node *head, *tail, *cur;

  /* Die on looping.  */
  gcc_assert (t != i->container);

  if (TREE_CODE (t) == STATEMENT_LIST)
    {
      head = STATEMENT_LIST_HEAD (t);
      tail = STATEMENT_LIST_TAIL (t);
      STATEMENT_LIST_HEAD (t) = NULL;
      STATEMENT_LIST_TAIL (t) = NULL;

      free_stmt_list (t);

      /* Empty statement lists need no work.  */
      if (!head || !tail)
	{
	  gcc_assert (head == tail);
	  return;
	}
    }
  else
    {
      head = ggc_alloc_tree_statement_list_node ();
      head->prev = NULL;
      head->next = NULL;
      head->stmt = t;
      tail = head;
    }

  TREE_SIDE_EFFECTS (i->container) = 1;

  cur = i->ptr;

  /* Link it into the list.  */
  if (cur)
    {
      tail->next = cur->next;
      if (tail->next)
	tail->next->prev = tail;
      else
	STATEMENT_LIST_TAIL (i->container) = tail;
      head->prev = cur;
      cur->next = head;
    }
  else
    {
      gcc_assert (!STATEMENT_LIST_TAIL (i->container));
      STATEMENT_LIST_HEAD (i->container) = head;
      STATEMENT_LIST_TAIL (i->container) = tail;
    }

  /* Update the iterator, if requested.  */
  switch (mode)
    {
    case TSI_NEW_STMT:
    case TSI_CHAIN_START:
      i->ptr = head;
      break;
    case TSI_CONTINUE_LINKING:
    case TSI_CHAIN_END:
      i->ptr = tail;
      break;
    case TSI_SAME_STMT:
      gcc_assert (cur);
      break;
    }
}

/* Remove a stmt from the tree list.  The iterator is updated to point to
   the next stmt.  */

void
tsi_delink (tree_stmt_iterator *i)
{
  struct tree_statement_list_node *cur, *next, *prev;

  cur = i->ptr;
  next = cur->next;
  prev = cur->prev;

  if (prev)
    prev->next = next;
  else
    STATEMENT_LIST_HEAD (i->container) = next;
  if (next)
    next->prev = prev;
  else
    STATEMENT_LIST_TAIL (i->container) = prev;

  if (!next && !prev)
    TREE_SIDE_EFFECTS (i->container) = 0;

  i->ptr = next;
}

/* Return the first expression in a sequence of COMPOUND_EXPRs,
   or in a STATEMENT_LIST.  */

tree
expr_first (tree expr)
{
  if (expr == NULL_TREE)
    return expr;

  if (TREE_CODE (expr) == STATEMENT_LIST)
    {
      struct tree_statement_list_node *n = STATEMENT_LIST_HEAD (expr);
      return n ? n->stmt : NULL_TREE;
    }

  while (TREE_CODE (expr) == COMPOUND_EXPR)
    expr = TREE_OPERAND (expr, 0);

  return expr;
}

/* Return the last expression in a sequence of COMPOUND_EXPRs,
   or in a STATEMENT_LIST.  */

tree
expr_last (tree expr)
{
  if (expr == NULL_TREE)
    return expr;

  if (TREE_CODE (expr) == STATEMENT_LIST)
    {
      struct tree_statement_list_node *n = STATEMENT_LIST_TAIL (expr);
      return n ? n->stmt : NULL_TREE;
    }

  while (TREE_CODE (expr) == COMPOUND_EXPR)
    expr = TREE_OPERAND (expr, 1);

  return expr;
}

#include "gt-tree-iterator.h"
