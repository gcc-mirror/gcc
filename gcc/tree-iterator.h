/* Iterator routines for manipulating GENERIC tree statement list. -*- C++ -*-
   Copyright (C) 2003-2025 Free Software Foundation, Inc.
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


/* This file is dependent upon the implementation of tree's. It provides an
   abstract interface to the tree objects such that if all tree creation and
   manipulations are done through this interface, we can easily change the
   implementation of tree's, and not impact other code.  */

#ifndef GCC_TREE_ITERATOR_H
#define GCC_TREE_ITERATOR_H 1

/* Iterator object for GENERIC or GIMPLE TREE statements.  */

struct tree_stmt_iterator {
  struct tree_statement_list_node *ptr;
  tree container;

  /* No need for user-defined constructors, the implicit definitions (or
     aggregate initialization) are fine.  */

  bool operator== (tree_stmt_iterator b) const
    { return b.ptr == ptr && b.container == container; }
  bool operator!= (tree_stmt_iterator b) const { return !(*this == b); }
  tree_stmt_iterator &operator++ () { ptr = ptr->next; return *this; }
  tree_stmt_iterator &operator-- () { ptr = ptr->prev; return *this; }
  tree_stmt_iterator operator++ (int)
    { tree_stmt_iterator x = *this; ++*this; return x; }
  tree_stmt_iterator operator-- (int)
    { tree_stmt_iterator x = *this; --*this; return x; }
  tree &operator* () { return ptr->stmt; }
  tree operator* () const { return ptr->stmt; }
};

inline tree_stmt_iterator
tsi_start (tree t)
{
  tree_stmt_iterator i;

  i.ptr = STATEMENT_LIST_HEAD (t);
  i.container = t;

  return i;
}

inline tree_stmt_iterator
tsi_last (tree t)
{
  tree_stmt_iterator i;

  i.ptr = STATEMENT_LIST_TAIL (t);
  i.container = t;

  return i;
}

inline bool
tsi_end_p (tree_stmt_iterator i)
{
  return i.ptr == NULL;
}

inline bool
tsi_one_before_end_p (tree_stmt_iterator i)
{
  return i.ptr != NULL && i.ptr->next == NULL;
}

inline void
tsi_next (tree_stmt_iterator *i)
{
  ++(*i);
}

inline void
tsi_prev (tree_stmt_iterator *i)
{
  --(*i);
}

inline tree *
tsi_stmt_ptr (tree_stmt_iterator i)
{
  return &(*i);
}

inline tree
tsi_stmt (tree_stmt_iterator i)
{
  return *i;
}

/* Make tree_stmt_iterator work as a C++ range, e.g.
   for (tree stmt : tsi_range (stmt_list)) { ... }  */
class tsi_range
{
  tree t;
 public:
  tsi_range (tree t): t(t) { }
  tree_stmt_iterator begin() const { return tsi_start (t); }
  tree_stmt_iterator end() const { return { nullptr, t }; }
};

enum tsi_iterator_update
{
  TSI_NEW_STMT,		/* Only valid when single statement is added, move
			   iterator to it.  */
  TSI_SAME_STMT,	/* Leave the iterator at the same statement.  */
  TSI_CHAIN_START,	/* Only valid when chain of statements is added, move
			   iterator to the first statement in the chain.  */
  TSI_CHAIN_END,	/* Only valid when chain of statements is added, move
			   iterator to the last statement in the chain.  */
  TSI_CONTINUE_LINKING	/* Move iterator to whatever position is suitable for
			   linking other statements/chains of statements in
			   the same direction.  */
};

extern void tsi_link_before (tree_stmt_iterator *, tree,
			     enum tsi_iterator_update);
extern void tsi_link_after (tree_stmt_iterator *, tree,
			    enum tsi_iterator_update);

extern void tsi_delink (tree_stmt_iterator *);
extern tree tsi_split_stmt_list (location_t, tree_stmt_iterator);

extern tree alloc_stmt_list (void);
extern void free_stmt_list (tree);
extern void append_to_statement_list (tree, tree *);
extern void append_to_statement_list_force (tree, tree *);
extern tree expr_first (tree);
extern tree expr_last (tree);
extern tree expr_single (tree);

#endif /* GCC_TREE_ITERATOR_H  */
