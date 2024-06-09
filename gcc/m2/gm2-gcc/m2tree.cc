/* m2tree.cc provides a simple interface to GCC tree queries and skips.

Copyright (C) 2012-2024 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "gcc-consolidation.h"

#include "../m2-tree.h"

#define m2tree_c
#include "m2tree.h"

bool
m2tree_is_var (tree var)
{
  return VAR_P (var);
}

bool
m2tree_is_array (tree array)
{
  return TREE_CODE (array) == ARRAY_TYPE;
}

bool
m2tree_is_type (tree type)
{
  switch (TREE_CODE (type))
    {

    case TYPE_DECL:
    case ARRAY_TYPE:
    case RECORD_TYPE:
    case SET_TYPE:
    case ENUMERAL_TYPE:
    case POINTER_TYPE:
    case INTEGER_TYPE:
    case REAL_TYPE:
    case UNION_TYPE:
    case BOOLEAN_TYPE:
    case COMPLEX_TYPE:
      return TRUE;
    default:
      return FALSE;
    }
}

tree
m2tree_skip_type_decl (tree type)
{
  if (type == error_mark_node)
    return error_mark_node;

  if (type == NULL_TREE)
    return NULL_TREE;

  if (TREE_CODE (type) == TYPE_DECL)
    return m2tree_skip_type_decl (TREE_TYPE (type));
  return type;
}

tree
m2tree_skip_const_decl (tree exp)
{
  if (exp == error_mark_node)
    return error_mark_node;

  if (exp == NULL_TREE)
    return NULL_TREE;

  if (TREE_CODE (exp) == CONST_DECL)
    return DECL_INITIAL (exp);
  return exp;
}

/* m2tree_skip_reference_type - skips all POINTER_TYPE and
   REFERENCE_TYPEs.  Otherwise return exp.  */

tree
m2tree_skip_reference_type (tree exp)
{
  if (TREE_CODE (exp) == REFERENCE_TYPE)
    return m2tree_skip_reference_type (TREE_TYPE (exp));
  if (TREE_CODE (exp) == POINTER_TYPE)
    return m2tree_skip_reference_type (TREE_TYPE (exp));
  return exp;
}

/* m2tree_IsOrdinal - return TRUE if code is an INTEGER, BOOLEAN or
   ENUMERAL type.  */

bool
m2tree_IsOrdinal (tree type)
{
  enum tree_code code = TREE_CODE (type);

  return (code == INTEGER_TYPE || (code) == BOOLEAN_TYPE
          || (code) == ENUMERAL_TYPE);
}

/* is_a_constant - returns TRUE if tree, t, is a constant.  */

bool
m2tree_IsAConstant (tree t)
{
  return (TREE_CODE (t) == INTEGER_CST) || (TREE_CODE (t) == REAL_CST)
    || (TREE_CODE (t) == REAL_CST) || (TREE_CODE (t) == COMPLEX_CST)
    || (TREE_CODE (t) == STRING_CST) || (TREE_CODE (t) == CONSTRUCTOR);
}


void
m2tree_debug_tree (tree t)
{
  debug_tree (t);
}
