/* This file contains subroutine used by the C front-end to construct GENERIC.
   Copyright (C) 2000-2025 Free Software Foundation, Inc.
   Written by Benjamin Chelf (chelf@codesourcery.com).

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "c-common.h"
#include "tree-iterator.h"

/* Create an empty statement tree rooted at T.  */

tree
push_stmt_list (void)
{
  tree t;
  t = alloc_stmt_list ();
  vec_safe_push (stmt_list_stack, t);
  return t;
}

/* Return TRUE if, after I, there are any nondebug stmts.  */

static inline bool
only_debug_stmts_after_p (tree_stmt_iterator i)
{
  for (tsi_next (&i); !tsi_end_p (i); tsi_next (&i))
    if (TREE_CODE (tsi_stmt (i)) != DEBUG_BEGIN_STMT)
      return false;
  return true;
}

/* Finish the statement tree rooted at T.  */

tree
pop_stmt_list (tree t)
{
  tree u = NULL_TREE;

  /* Pop statement lists until we reach the target level.  The extra
     nestings will be due to outstanding cleanups.  */
  while (1)
    {
      u = stmt_list_stack->pop ();
      if (!stmt_list_stack->is_empty ())
	{
	  tree x = stmt_list_stack->last ();
	  STATEMENT_LIST_HAS_LABEL (x) |= STATEMENT_LIST_HAS_LABEL (u);
	}
      if (t == u)
	break;
    }

  gcc_assert (u != NULL_TREE);

  /* If the statement list is completely empty, just return it.  This is
     just as good small as build_empty_stmt, with the advantage that
     statement lists are merged when they appended to one another.  So
     using the STATEMENT_LIST avoids pathological buildup of EMPTY_STMT_P
     statements.  */
  if (TREE_SIDE_EFFECTS (t))
    {
      tree_stmt_iterator i = tsi_start (t);

      /* If the statement list contained exactly one statement, then
	 extract it immediately.  */
      if (tsi_one_before_end_p (i))
	{
	  u = tsi_stmt (i);
	  tsi_delink (&i);
	  free_stmt_list (t);
	  t = u;
	}
      /* If the statement list contained a debug begin stmt and a
	 statement list, move the debug begin stmt into the statement
	 list and return it.  */
      else if (!tsi_end_p (i)
	       && TREE_CODE (tsi_stmt (i)) == DEBUG_BEGIN_STMT)
	{
	  u = tsi_stmt (i);
	  tsi_next (&i);
	  if (tsi_one_before_end_p (i)
	      && TREE_CODE (tsi_stmt (i)) == STATEMENT_LIST)
	    {
	      tree l = tsi_stmt (i);
	      tsi_prev (&i);
	      tsi_delink (&i);
	      tsi_delink (&i);
	      i = tsi_start (l);
	      free_stmt_list (t);
	      t = l;
	      tsi_link_before (&i, u, TSI_SAME_STMT);
	    }
	  while (!tsi_end_p (i)
		 && TREE_CODE (tsi_stmt (i)) == DEBUG_BEGIN_STMT)
	    tsi_next (&i);
	  /* If there are only debug stmts in the list, without them
	     we'd have an empty stmt without side effects.  If there's
	     only one nondebug stmt, we'd have extracted the stmt and
	     dropped the list, and we'd take TREE_SIDE_EFFECTS from
	     that statement.  In either case, keep the list's
	     TREE_SIDE_EFFECTS in sync.  */
	  if (tsi_end_p (i))
	    TREE_SIDE_EFFECTS (t) = 0;
	  else if (only_debug_stmts_after_p (i))
	    TREE_SIDE_EFFECTS (t) = TREE_SIDE_EFFECTS (tsi_stmt (i));
	}
    }

  return t;
}

/* Build a generic statement based on the given type of node and
   arguments. Similar to `build_nt', except that we set
   EXPR_LOCATION to LOC. */
/* ??? This should be obsolete with the lineno_stmt productions
   in the grammar.  */

tree
build_stmt (location_t loc, enum tree_code code, ...)
{
  tree ret;
  int length, i;
  va_list p;
  bool side_effects;

  /* This function cannot be used to construct variably-sized nodes.  */
  gcc_assert (TREE_CODE_CLASS (code) != tcc_vl_exp);

  va_start (p, code);

  ret = make_node (code);
  TREE_TYPE (ret) = void_type_node;
  length = TREE_CODE_LENGTH (code);
  SET_EXPR_LOCATION (ret, loc);

  /* TREE_SIDE_EFFECTS will already be set for statements with
     implicit side effects.  Here we make sure it is set for other
     expressions by checking whether the parameters have side
     effects.  */

  side_effects = false;
  for (i = 0; i < length; i++)
    {
      tree t = va_arg (p, tree);
      if (t && !TYPE_P (t))
	side_effects |= TREE_SIDE_EFFECTS (t);
      TREE_OPERAND (ret, i) = t;
    }

  TREE_SIDE_EFFECTS (ret) |= side_effects;

  va_end (p);
  return ret;
}

/* Build a REALPART_EXPR or IMAGPART_EXPR, according to CODE, from ARG.  */

tree
build_real_imag_expr (location_t location, enum tree_code code, tree arg)
{
  tree ret;
  tree arg_type = TREE_TYPE (arg);

  gcc_assert (code == REALPART_EXPR || code == IMAGPART_EXPR);

  if (TREE_CODE (arg_type) == COMPLEX_TYPE)
    {
      ret = build1 (code, TREE_TYPE (TREE_TYPE (arg)), arg);
      SET_EXPR_LOCATION (ret, location);
    }
  else if (INTEGRAL_TYPE_P (arg_type) || SCALAR_FLOAT_TYPE_P (arg_type))
    {
      ret = (code == REALPART_EXPR
	     ? arg
	     : omit_one_operand_loc (location, arg_type,
				     integer_zero_node, arg));
    }
  else
    {
      error_at (location, "wrong type argument to %s",
		code == REALPART_EXPR ? "__real" : "__imag");
      ret = error_mark_node;
    }

  return ret;
}
