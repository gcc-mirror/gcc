/* This file contains subroutine used by the C front-end to construct GENERIC.
   Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.
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
#include "tm.h"
#include "tree.h"
#include "function.h"
#include "splay-tree.h"
#include "c-common.h"
#include "flags.h"
#include "output.h"
#include "tree-iterator.h"

/* Create an empty statement tree rooted at T.  */

tree
push_stmt_list (void)
{
  tree t;
  t = alloc_stmt_list ();
  TREE_CHAIN (t) = cur_stmt_list;
  cur_stmt_list = t;
  return t;
}

/* Finish the statement tree rooted at T.  */

tree
pop_stmt_list (tree t)
{
  tree u = cur_stmt_list, chain;

  /* Pop statement lists until we reach the target level.  The extra
     nestings will be due to outstanding cleanups.  */
  while (1)
    {
      chain = TREE_CHAIN (u);
      TREE_CHAIN (u) = NULL_TREE;
      if (chain)
	STATEMENT_LIST_HAS_LABEL (chain) |= STATEMENT_LIST_HAS_LABEL (u);
      if (t == u)
	break;
      u = chain;
    }
  cur_stmt_list = chain;

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

/* Create a CASE_LABEL_EXPR tree node and return it.  */

tree
build_case_label (location_t loc,
		  tree low_value, tree high_value, tree label_decl)
{
  return build_stmt (loc, CASE_LABEL_EXPR, low_value, high_value, label_decl);
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
