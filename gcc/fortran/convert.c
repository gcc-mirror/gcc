/* Data type conversion
   Copyright (C) 1987-2013 Free Software Foundation, Inc.

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


/* This file contains the functions for converting expressions to
   different data types for the translation of the gfortran internal
   representation to GIMPLE.  The only entry point is `convert'.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "convert.h"

/* Prepare expr to be an argument of a TRUTH_NOT_EXPR,
   or validate its data type for a GIMPLE `if' or `while' statement.

   The resulting type should always be `boolean_type_node'.  */

static tree
truthvalue_conversion (tree expr)
{
  switch (TREE_CODE (TREE_TYPE (expr)))
    {
    case BOOLEAN_TYPE:
      if (TREE_TYPE (expr) == boolean_type_node)
	return expr;
      else if (COMPARISON_CLASS_P (expr))
	{
	  TREE_TYPE (expr) = boolean_type_node;
	  return expr;
	}
      else if (TREE_CODE (expr) == NOP_EXPR)
        return fold_build1_loc (input_location, NOP_EXPR,
			    boolean_type_node, TREE_OPERAND (expr, 0));
      else
        return fold_build1_loc (input_location, NOP_EXPR, boolean_type_node,
				expr);

    case INTEGER_TYPE:
      if (TREE_CODE (expr) == INTEGER_CST)
	return integer_zerop (expr) ? boolean_false_node : boolean_true_node;
      else
        return fold_build2_loc (input_location, NE_EXPR, boolean_type_node,
				expr, build_int_cst (TREE_TYPE (expr), 0));

    default:
      gcc_unreachable ();
    }
}

/* Create an expression whose value is that of EXPR,
   converted to type TYPE.  The TREE_TYPE of the value
   is always TYPE.  This function implements all reasonable
   conversions; callers should filter out those that are
   not permitted by the language being compiled.  */

tree
convert (tree type, tree expr)
{
  tree e = expr;
  enum tree_code code;

  if (type == TREE_TYPE (expr))
    return expr;

  if (TREE_CODE (type) == ERROR_MARK
      || TREE_CODE (expr) == ERROR_MARK
      || TREE_CODE (TREE_TYPE (expr)) == ERROR_MARK)
    return expr;

  gcc_checking_assert (TREE_CODE (TREE_TYPE (expr)) != VOID_TYPE);

  if (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (TREE_TYPE (expr)))
    return fold_build1_loc (input_location, NOP_EXPR, type, expr);

  code = TREE_CODE (type);
  if (code == VOID_TYPE)
    return fold_build1_loc (input_location, CONVERT_EXPR, type, e);
  if (code == BOOLEAN_TYPE)
    return fold_build1_loc (input_location, NOP_EXPR, type,
			    truthvalue_conversion (e));
  if (code == INTEGER_TYPE)
    return fold (convert_to_integer (type, e));
  if (code == POINTER_TYPE || code == REFERENCE_TYPE)
    return fold (convert_to_pointer (type, e));
  if (code == REAL_TYPE)
    return fold (convert_to_real (type, e));
  if (code == COMPLEX_TYPE)
    return fold (convert_to_complex (type, e));
  if (code == VECTOR_TYPE)
    return fold (convert_to_vector (type, e));

  gcc_unreachable ();
}

