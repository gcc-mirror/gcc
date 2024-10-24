/* Language-level data type conversion for GNU C.
   Copyright (C) 1987-2024 Free Software Foundation, Inc.

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

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "c-tree.h"
#include "convert.h"
#include "langhooks.h"
#include "ubsan.h"
#include "stringpool.h"
#include "attribs.h"
#include "asan.h"

/* Change of width--truncation and extension of integers or reals--
   is represented with NOP_EXPR.  Proper functioning of many things
   assumes that no other conversions can be NOP_EXPRs.

   Conversion between integer and pointer is represented with CONVERT_EXPR.
   Converting integer to real uses FLOAT_EXPR
   and real to integer uses FIX_TRUNC_EXPR.

   Here is a list of all the functions that assume that widening and
   narrowing is always done with a NOP_EXPR:
     In convert.cc, convert_to_integer.
     In c-typeck.cc, build_binary_op (boolean ops), and
	c_common_truthvalue_conversion.
     In expr.cc: expand_expr, for operands of a MULT_EXPR.
     In fold-const.cc: fold.
     In tree.cc: get_narrower and get_unwidened.  */

/* Subroutines of `convert'.  */



/* Create an expression whose value is that of EXPR,
   converted to type TYPE.  The TREE_TYPE of the value
   is always TYPE.  This function implements all reasonable
   conversions; callers should filter out those that are
   not permitted by the language being compiled.
   INIT_CONST is true if the conversion is for arithmetic types for a static
   initializer and folding must apply accordingly (discarding floating-point
   exceptions and assuming the default rounding mode is in effect).  */

static tree
c_convert (tree type, tree expr, bool init_const)
{
  tree e = expr;
  enum tree_code code = TREE_CODE (type);
  const char *invalid_conv_diag;
  tree ret;
  location_t loc = EXPR_LOCATION (expr);

  if (type == error_mark_node
      || error_operand_p (expr))
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

  if (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (TREE_TYPE (expr))
      && (TREE_CODE (TREE_TYPE (expr)) != COMPLEX_TYPE
	  || TREE_CODE (e) == COMPLEX_EXPR))
    return fold_convert_loc (loc, type, expr);
  if (TREE_CODE (TREE_TYPE (expr)) == ERROR_MARK)
    return error_mark_node;
  if (VOID_TYPE_P (TREE_TYPE (expr)))
    {
      error ("void value not ignored as it ought to be");
      return error_mark_node;
    }

  {
    tree false_value, true_value;
    if (c_hardbool_type_attr (type, &false_value, &true_value))
      {
	bool save = in_late_binary_op;
	in_late_binary_op = true;
	expr = c_objc_common_truthvalue_conversion (input_location, expr);
	in_late_binary_op = save;

	return fold_build3_loc (loc, COND_EXPR, type,
				expr, true_value, false_value);
      }
  }

  switch (code)
    {
    case VOID_TYPE:
      return fold_convert_loc (loc, type, e);

    case ENUMERAL_TYPE:
      if (ENUM_UNDERLYING_TYPE (type) != NULL_TREE
	  && TREE_CODE (ENUM_UNDERLYING_TYPE (type)) == BOOLEAN_TYPE)
	goto convert_to_boolean;
      gcc_fallthrough ();

    case INTEGER_TYPE:
    case BITINT_TYPE:
      if (sanitize_flags_p (SANITIZE_FLOAT_CAST)
	  && current_function_decl != NULL_TREE
	  && SCALAR_FLOAT_TYPE_P (TREE_TYPE (expr))
	  && COMPLETE_TYPE_P (type))
	{
	  expr = save_expr (expr);
	  expr = c_fully_fold (expr, init_const, NULL);
	  tree check = ubsan_instrument_float_cast (loc, type, expr);
	  expr = fold_build1 (FIX_TRUNC_EXPR, type, expr);
	  if (check == NULL_TREE)
	    return expr;
	  return fold_build2 (COMPOUND_EXPR, TREE_TYPE (expr), check, expr);
	}
      ret = convert_to_integer (type, e);
      goto maybe_fold;

    case BOOLEAN_TYPE:
    convert_to_boolean:
      return c_objc_common_truthvalue_conversion (input_location, expr, type);

    case POINTER_TYPE:
      /* The type nullptr_t may be converted to a pointer type.  The result is
	 a null pointer value.  */
      if (NULLPTR_TYPE_P (TREE_TYPE (e)))
	{
	  /* To make sure that (void *)nullptr is not a null pointer constant,
	     build_c_cast will create an additional NOP_EXPR around the result
	     of this conversion.  */
	  if (TREE_SIDE_EFFECTS (e))
	    ret = build2 (COMPOUND_EXPR, type, e, build_int_cst (type, 0));
	  else
	    ret = build_int_cst (type, 0);
	  goto maybe_fold;
	}
      gcc_fallthrough ();
    case REFERENCE_TYPE:
      ret = convert_to_pointer (type, e);
      goto maybe_fold;

    case NULLPTR_TYPE:
      /* A null pointer constant or value of type nullptr_t may be
	 converted to nullptr_t.  The latter case has already been
	 handled.  build_c_cast will create an additional NOP_EXPR to
	 ensure the result of the conversion is not itself a null
	 pointer constant.  */
      if (null_pointer_constant_p (expr))
	{
	  ret = build_int_cst (type, 0);
	  goto maybe_fold;
	}
      break;

    case REAL_TYPE:
      ret = convert_to_real (type, e);
      goto maybe_fold;

    case FIXED_POINT_TYPE:
      ret = convert_to_fixed (type, e);
      goto maybe_fold;

    case COMPLEX_TYPE:
      ret = convert_to_complex (type, e);
      goto maybe_fold;

    case VECTOR_TYPE:
      if (gnu_vector_type_p (type)
	  || gnu_vector_type_p (TREE_TYPE (e))
	  /* Allow conversions between compatible non-GNU vector types
	     when -flax-vector-conversions is passed.  The whole purpose
	     of the option is to bend the normal type rules and accept
	     nonconforming code.  */
	  || (flag_lax_vector_conversions
	      && VECTOR_TYPE_P (TREE_TYPE (e))
	      && vector_types_convertible_p (type, TREE_TYPE (e), false)))
	{
	  ret = convert_to_vector (type, e);
	  goto maybe_fold;
	}
      break;

    case RECORD_TYPE:
    case UNION_TYPE:
      if (lang_hooks.types_compatible_p (type, TREE_TYPE (expr)))
	return e;
      break;

    default:
      break;

    maybe_fold:
      if (TREE_CODE (ret) != C_MAYBE_CONST_EXPR)
	ret = init_const ? fold_init (ret) : fold (ret);
      return ret;
    }

  /* If we are converting to nullptr_t, don't say "non-scalar type" because
     the nullptr_t type is a scalar type.  Only nullptr_t or a null pointer
     constant shall be converted to nullptr_t.  */
  if (code == NULLPTR_TYPE)
    {
      error ("conversion from %qT to %qT", TREE_TYPE (e), type);
      inform (input_location,
	      "only %qT or a null pointer constant can be converted to %qT",
	      type, type);
    }
  else
    error ("conversion to non-scalar type requested");
  return error_mark_node;
}

/* Create an expression whose value is that of EXPR, converted to type TYPE.
   The TREE_TYPE of the value is always TYPE.  This function implements all
   reasonable conversions; callers should filter out those that are not
   permitted by the language being compiled.  */

tree
convert (tree type, tree expr)
{
  return c_convert (type, expr, false);
}

/* Create an expression whose value is that of EXPR, converted to type TYPE, in
   a static initializer.  The TREE_TYPE of the value is always TYPE.  This
   function implements all reasonable conversions; callers should filter out
   those that are not permitted by the language being compiled.  */

tree
convert_init (tree type, tree expr)
{
  return c_convert (type, expr, true);
}
