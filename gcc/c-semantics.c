/* This file contains the definitions and documentation for the common
   tree codes used in the GNU C and C++ compilers (see c-common.def
   for the standard codes).
   Copyright (C) 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
   Written by Benjamin Chelf (chelf@codesourcery.com).

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "function.h"
#include "splay-tree.h"
#include "varray.h"
#include "c-common.h"
#include "except.h"
/* In order for the format checking to accept the C frontend
   diagnostic framework extensions, you must define this token before
   including toplev.h.  */
#define GCC_DIAG_STYLE __gcc_cdiag__
#include "toplev.h"
#include "flags.h"
#include "ggc.h"
#include "rtl.h"
#include "output.h"
#include "timevar.h"
#include "predict.h"
#include "tree-inline.h"
#include "langhooks.h"

/* Create an empty statement tree rooted at T.  */

void
begin_stmt_tree (tree *t)
{
  /* We create a trivial EXPR_STMT so that last_tree is never NULL in
     what follows.  We remove the extraneous statement in
     finish_stmt_tree.  */
  *t = build_nt (EXPR_STMT, void_zero_node);
  last_tree = *t;
  last_expr_type = NULL_TREE;
  last_expr_filename = input_filename;
}

/* T is a statement.  Add it to the statement-tree.  */

tree
add_stmt (tree t)
{
  if (!EXPR_LOCUS (t))
    annotate_with_locus (t, input_location);

  /* Add T to the statement-tree.  */
  TREE_CHAIN (last_tree) = t;
  last_tree = t;

  /* When we expand a statement-tree, we must know whether or not the
     statements are full-expressions.  We record that fact here.  */
  STMT_IS_FULL_EXPR_P (last_tree) = stmts_are_full_exprs_p ();

  return t;
}

/* Create a declaration statement for the declaration given by the
   DECL.  */

void
add_decl_stmt (tree decl)
{
  tree decl_stmt;

  /* We need the type to last until instantiation time.  */
  decl_stmt = build_stmt (DECL_STMT, decl);
  add_stmt (decl_stmt);
}

/* Add a scope-statement to the statement-tree.  BEGIN_P indicates
   whether this statements opens or closes a scope.  PARTIAL_P is true
   for a partial scope, i.e, the scope that begins after a label when
   an object that needs a cleanup is created.  If BEGIN_P is nonzero,
   returns a new TREE_LIST representing the top of the SCOPE_STMT
   stack.  The TREE_PURPOSE is the new SCOPE_STMT.  If BEGIN_P is
   zero, returns a TREE_LIST whose TREE_VALUE is the new SCOPE_STMT,
   and whose TREE_PURPOSE is the matching SCOPE_STMT with
   SCOPE_BEGIN_P set.  */

tree
add_scope_stmt (int begin_p, int partial_p)
{
  tree *stack_ptr = current_scope_stmt_stack ();
  tree ss;
  tree top = *stack_ptr;

  /* Build the statement.  */
  ss = build_stmt (SCOPE_STMT, NULL_TREE);
  SCOPE_BEGIN_P (ss) = begin_p;
  SCOPE_PARTIAL_P (ss) = partial_p;

  /* Keep the scope stack up to date.  */
  if (begin_p)
    {
      top = tree_cons (ss, NULL_TREE, top);
      *stack_ptr = top;
    }
  else
    {
      if (partial_p != SCOPE_PARTIAL_P (TREE_PURPOSE (top)))
	abort ();
      TREE_VALUE (top) = ss;
      *stack_ptr = TREE_CHAIN (top);
    }

  /* Add the new statement to the statement-tree.  */
  add_stmt (ss);

  return top;
}

/* Finish the statement tree rooted at T.  */

void
finish_stmt_tree (tree *t)
{
  tree stmt;

  /* Remove the fake extra statement added in begin_stmt_tree.  */
  stmt = TREE_CHAIN (*t);
  *t = stmt;
  last_tree = NULL_TREE;
}

/* Build a generic statement based on the given type of node and
   arguments. Similar to `build_nt', except that we set
   EXPR_LOCUS to be the current source location.  */
/* ??? This should be obsolete with the lineno_stmt productions
   in the grammar.  */

tree
build_stmt (enum tree_code code, ...)
{
  tree ret;
  int length, i;
  va_list p;
  bool side_effects;

  va_start (p, code);

  ret = make_node (code);
  length = TREE_CODE_LENGTH (code);
  annotate_with_locus (ret, input_location);

  /* Most statements have implicit side effects all on their own, 
     such as control transfer.  For those that do, we'll compute
     the real value of TREE_SIDE_EFFECTS from its arguments.  */
  switch (code)
    {
    case EXPR_STMT:
      side_effects = false;
      break;
    default:
      side_effects = true;
      break;
    }

  for (i = 0; i < length; i++)
    {
      tree t = va_arg (p, tree);
      if (t && IS_NON_TYPE_CODE_CLASS (TREE_CODE_CLASS (TREE_CODE (t))))
        side_effects |= TREE_SIDE_EFFECTS (t);
      TREE_OPERAND (ret, i) = t;
    }

  TREE_SIDE_EFFECTS (ret) = side_effects;

  va_end (p);
  return ret;
}

/* Create RTL for the local static variable DECL.  */

void
make_rtl_for_local_static (tree decl)
{
  const char *asmspec = NULL;

  /* If we inlined this variable, we could see it's declaration
     again.  */
  if (TREE_ASM_WRITTEN (decl))
    return;

  /* If the DECL_ASSEMBLER_NAME is not the same as the DECL_NAME, then
     either we already created RTL for this DECL (and since it was a
     local variable, its DECL_ASSEMBLER_NAME got hacked up to prevent
     clashes with other local statics with the same name by a previous
     call to make_decl_rtl), or the user explicitly requested a
     particular assembly name for this variable, using the GNU
     extension for this purpose:

       int i asm ("j");

     There's no way to know which case we're in, here.  But, it turns
     out we're safe.  If there's already RTL, then
     rest_of_decl_compilation ignores the ASMSPEC parameter, so we
     may as well not pass it in.  If there isn't RTL, then we didn't
     already create RTL, which means that the modification to
     DECL_ASSEMBLER_NAME came only via the explicit extension.  */
  if (DECL_ASSEMBLER_NAME (decl) != DECL_NAME (decl)
      && !DECL_RTL_SET_P (decl))
    asmspec = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));

  rest_of_decl_compilation (decl, asmspec, /*top_level=*/0, /*at_end=*/0);
}

/* Let the back-end know about DECL.  */

void
emit_local_var (tree decl)
{
  /* Create RTL for this variable.  */
  if (!DECL_RTL_SET_P (decl))
    {
      if (DECL_HARD_REGISTER (decl))
	/* The user specified an assembler name for this variable.
	   Set that up now.  */
	rest_of_decl_compilation
	  (decl, IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl)),
	   /*top_level=*/0, /*at_end=*/0);
      else
	expand_decl (decl);
    }
}

/* Build the node for a return statement and return it.  */

tree
build_return_stmt (tree expr)
{
  return (build_stmt (RETURN_STMT, expr));
}

/* Build a break statement node and return it.  */

tree
build_break_stmt (void)
{
  return (build_stmt (BREAK_STMT));
}

/* Build a continue statement node and return it.  */

tree
build_continue_stmt (void)
{
  return (build_stmt (CONTINUE_STMT));
}

/* Create a CASE_LABEL tree node and return it.  */

tree
build_case_label (tree low_value, tree high_value, tree label_decl)
{
  return build_stmt (CASE_LABEL, low_value, high_value, label_decl);
}

/* We're about to expand T, a statement.  Set up appropriate context
   for the substitution.  */

void
prep_stmt (tree t)
{
  if (EXPR_LOCUS (t))
    input_location = *EXPR_LOCUS (t);
  current_stmt_tree ()->stmts_are_full_exprs_p = STMT_IS_FULL_EXPR_P (t);
}
