/* Language-level data type conversion for GNU C.
   Copyright (C) 1987, 1988, 1991, 1998, 2002, 2003, 2004, 2005, 2007, 2008,
   2009, 2010 Free Software Foundation, Inc.

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


/* This file contains the functions for converting C expressions
   to different data types.  The only entry point is `convert'.
   Every language front end must have a `convert' function
   but what kind of conversions it does will depend on the language.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "flags.h"
#include "convert.h"
#include "c-family/c-common.h"
#include "c-tree.h"
#include "langhooks.h"
#include "target.h"

/* Change of width--truncation and extension of integers or reals--
   is represented with NOP_EXPR.  Proper functioning of many things
   assumes that no other conversions can be NOP_EXPRs.

   Conversion between integer and pointer is represented with CONVERT_EXPR.
   Converting integer to real uses FLOAT_EXPR
   and real to integer uses FIX_TRUNC_EXPR.

   Here is a list of all the functions that assume that widening and
   narrowing is always done with a NOP_EXPR:
     In convert.c, convert_to_integer.
     In c-typeck.c, build_binary_op (boolean ops), and
	c_common_truthvalue_conversion.
     In expr.c: expand_expr, for operands of a MULT_EXPR.
     In fold-const.c: fold.
     In tree.c: get_narrower and get_unwidened.  */

/* Subroutines of `convert'.  */



/* Create an expression whose value is that of EXPR,
   converted to type TYPE.  The TREE_TYPE of the value
   is always TYPE.  This function implements all reasonable
   conversions; callers should filter out those that are
   not permitted by the language being compiled.  */

tree
convert (tree type, tree expr)
{
  tree e = expr;
  enum tree_code code = TREE_CODE (type);
  const char *invalid_conv_diag;
  tree ret;
  location_t loc = EXPR_LOCATION (expr);

  if (type == error_mark_node
      || expr == error_mark_node
      || TREE_TYPE (expr) == error_mark_node)
    return error_mark_node;

  if ((invalid_conv_diag
       = targetm.invalid_conversion (TREE_TYPE (expr), type)))
    {
      error (invalid_conv_diag);
      return error_mark_node;
    }

  if (type == TREE_TYPE (expr))
    return expr;
  ret = targetm.convert_to_type (type, expr);
  if (ret)
      return ret;

  STRIP_TYPE_NOPS (e);

  if (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (TREE_TYPE (expr)))
    return fold_convert_loc (loc, type, expr);
  if (TREE_CODE (TREE_TYPE (expr)) == ERROR_MARK)
    return error_mark_node;
  if (TREE_CODE (TREE_TYPE (expr)) == VOID_TYPE)
    {
      error ("void value not ignored as it ought to be");
      return error_mark_node;
    }

  switch (code)
    {
    case VOID_TYPE:
      return fold_convert_loc (loc, type, e);

    case INTEGER_TYPE:
    case ENUMERAL_TYPE:
      ret = convert_to_integer (type, e);
      goto maybe_fold;

    case BOOLEAN_TYPE:
      return fold_convert_loc
	(loc, type, c_objc_common_truthvalue_conversion (input_location, expr));

    case POINTER_TYPE:
    case REFERENCE_TYPE:
      ret = convert_to_pointer (type, e);
      goto maybe_fold;

    case REAL_TYPE:
      ret = convert_to_real (type, e);
      goto maybe_fold;

    case FIXED_POINT_TYPE:
      ret = convert_to_fixed (type, e);
      goto maybe_fold;

    case COMPLEX_TYPE:
      /* If converting from COMPLEX_TYPE to a different COMPLEX_TYPE
	 and e is not COMPLEX_EXPR, convert_to_complex uses save_expr,
	 but for the C FE c_save_expr needs to be called instead.  */
      if (TREE_CODE (TREE_TYPE (e)) == COMPLEX_TYPE)
	{
	  tree subtype = TREE_TYPE (type);
	  tree elt_type = TREE_TYPE (TREE_TYPE (e));

	  if (TYPE_MAIN_VARIANT (elt_type) != TYPE_MAIN_VARIANT (subtype)
	      && TREE_CODE (e) != COMPLEX_EXPR)
	    {
	      if (in_late_binary_op)
		e = save_expr (e);
	      else
		e = c_save_expr (e);
	      ret
		= fold_build2 (COMPLEX_EXPR, type,
			       convert (subtype,
					fold_build1 (REALPART_EXPR,
						     elt_type, e)),
			       convert (subtype,
					fold_build1 (IMAGPART_EXPR,
						     elt_type, e)));
	      goto maybe_fold;
	    }
	}
      ret = convert_to_complex (type, e);
      goto maybe_fold;

    case VECTOR_TYPE:
      ret = convert_to_vector (type, e);
      goto maybe_fold;

    case RECORD_TYPE:
    case UNION_TYPE:
      if (lang_hooks.types_compatible_p (type, TREE_TYPE (expr)))
	return e;
      break;

    default:
      break;

    maybe_fold:
      if (TREE_CODE (ret) != C_MAYBE_CONST_EXPR)
	ret = fold (ret);
      return ret;
    }

  error ("conversion to non-scalar type requested");
  return error_mark_node;
}
