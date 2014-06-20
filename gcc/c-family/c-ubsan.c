/* UndefinedBehaviorSanitizer, undefined behavior detector.
   Copyright (C) 2013-2014 Free Software Foundation, Inc.
   Contributed by Marek Polacek <polacek@redhat.com>

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
#include "tree.h"
#include "alloc-pool.h"
#include "cgraph.h"
#include "output.h"
#include "toplev.h"
#include "ubsan.h"
#include "c-family/c-common.h"
#include "c-family/c-ubsan.h"
#include "asan.h"
#include "internal-fn.h"

/* Instrument division by zero and INT_MIN / -1.  If not instrumenting,
   return NULL_TREE.  */

tree
ubsan_instrument_division (location_t loc, tree op0, tree op1)
{
  tree t, tt;
  tree type = TREE_TYPE (op0);

  /* At this point both operands should have the same type,
     because they are already converted to RESULT_TYPE.
     Use TYPE_MAIN_VARIANT since typedefs can confuse us.  */
  gcc_assert (TYPE_MAIN_VARIANT (TREE_TYPE (op0))
	      == TYPE_MAIN_VARIANT (TREE_TYPE (op1)));

  if (TREE_CODE (type) == INTEGER_TYPE
      && (flag_sanitize & SANITIZE_DIVIDE))
    t = fold_build2 (EQ_EXPR, boolean_type_node,
		     op1, build_int_cst (type, 0));
  else if (TREE_CODE (type) == REAL_TYPE
	   && (flag_sanitize & SANITIZE_FLOAT_DIVIDE))
    t = fold_build2 (EQ_EXPR, boolean_type_node,
		     op1, build_real (type, dconst0));
  else
    return NULL_TREE;

  /* We check INT_MIN / -1 only for signed types.  */
  if (TREE_CODE (type) == INTEGER_TYPE
      && (flag_sanitize & SANITIZE_DIVIDE)
      && !TYPE_UNSIGNED (type))
    {
      tree x;
      tt = fold_build2 (EQ_EXPR, boolean_type_node, op1,
			build_int_cst (type, -1));
      x = fold_build2 (EQ_EXPR, boolean_type_node, op0,
		       TYPE_MIN_VALUE (type));
      x = fold_build2 (TRUTH_AND_EXPR, boolean_type_node, x, tt);
      t = fold_build2 (TRUTH_OR_EXPR, boolean_type_node, t, x);
    }

  /* If the condition was folded to 0, no need to instrument
     this expression.  */
  if (integer_zerop (t))
    return NULL_TREE;

  /* In case we have a SAVE_EXPR in a conditional context, we need to
     make sure it gets evaluated before the condition.  If the OP0 is
     an instrumented array reference, mark it as having side effects so
     it's not folded away.  */
  if (flag_sanitize & SANITIZE_BOUNDS)
    {
      tree xop0 = op0;
      while (CONVERT_EXPR_P (xop0))
	xop0 = TREE_OPERAND (xop0, 0);
      if (TREE_CODE (xop0) == ARRAY_REF)
	{
	  TREE_SIDE_EFFECTS (xop0) = 1;
	  TREE_SIDE_EFFECTS (op0) = 1;
	}
    }
  t = fold_build2 (COMPOUND_EXPR, TREE_TYPE (t), op0, t);
  if (flag_sanitize_undefined_trap_on_error)
    tt = build_call_expr_loc (loc, builtin_decl_explicit (BUILT_IN_TRAP), 0);
  else
    {
      tree data = ubsan_create_data ("__ubsan_overflow_data", &loc, NULL,
				     ubsan_type_descriptor (type), NULL_TREE);
      data = build_fold_addr_expr_loc (loc, data);
      enum built_in_function bcode
	= flag_sanitize_recover
	  ? BUILT_IN_UBSAN_HANDLE_DIVREM_OVERFLOW
	  : BUILT_IN_UBSAN_HANDLE_DIVREM_OVERFLOW_ABORT;
      tt = builtin_decl_explicit (bcode);
      tt = build_call_expr_loc (loc, tt, 3, data, ubsan_encode_value (op0),
				ubsan_encode_value (op1));
    }
  t = fold_build3 (COND_EXPR, void_type_node, t, tt, void_node);

  return t;
}

/* Instrument left and right shifts.  */

tree
ubsan_instrument_shift (location_t loc, enum tree_code code,
			tree op0, tree op1)
{
  tree t, tt = NULL_TREE;
  tree type0 = TREE_TYPE (op0);
  tree type1 = TREE_TYPE (op1);
  tree op1_utype = unsigned_type_for (type1);
  HOST_WIDE_INT op0_prec = TYPE_PRECISION (type0);
  tree uprecm1 = build_int_cst (op1_utype, op0_prec - 1);
  tree precm1 = build_int_cst (type1, op0_prec - 1);

  t = fold_convert_loc (loc, op1_utype, op1);
  t = fold_build2 (GT_EXPR, boolean_type_node, t, uprecm1);

  /* For signed x << y, in C99/C11, the following:
     (unsigned) x >> (precm1 - y)
     if non-zero, is undefined.  */
  if (code == LSHIFT_EXPR
      && !TYPE_UNSIGNED (type0)
      && flag_isoc99)
    {
      tree x = fold_build2 (MINUS_EXPR, integer_type_node, precm1, op1);
      tt = fold_convert_loc (loc, unsigned_type_for (type0), op0);
      tt = fold_build2 (RSHIFT_EXPR, TREE_TYPE (tt), tt, x);
      tt = fold_build2 (NE_EXPR, boolean_type_node, tt,
			build_int_cst (TREE_TYPE (tt), 0));
    }

  /* For signed x << y, in C++11/C++14, the following:
     x < 0 || ((unsigned) x >> (precm1 - y))
     if > 1, is undefined.  */
  if (code == LSHIFT_EXPR
      && !TYPE_UNSIGNED (TREE_TYPE (op0))
      && (cxx_dialect == cxx11 || cxx_dialect == cxx1y))
    {
      tree x = fold_build2 (MINUS_EXPR, integer_type_node, precm1, op1);
      tt = fold_convert_loc (loc, unsigned_type_for (type0), op0);
      tt = fold_build2 (RSHIFT_EXPR, TREE_TYPE (tt), tt, x);
      tt = fold_build2 (GT_EXPR, boolean_type_node, tt,
			build_int_cst (TREE_TYPE (tt), 1));
      x = fold_build2 (LT_EXPR, boolean_type_node, op0,
		       build_int_cst (type0, 0));
      tt = fold_build2 (TRUTH_OR_EXPR, boolean_type_node, x, tt);
    }

  /* If the condition was folded to 0, no need to instrument
     this expression.  */
  if (integer_zerop (t) && (tt == NULL_TREE || integer_zerop (tt)))
    return NULL_TREE;

  /* In case we have a SAVE_EXPR in a conditional context, we need to
     make sure it gets evaluated before the condition.  If the OP0 is
     an instrumented array reference, mark it as having side effects so
     it's not folded away.  */
  if (flag_sanitize & SANITIZE_BOUNDS)
    {
      tree xop0 = op0;
      while (CONVERT_EXPR_P (xop0))
	xop0 = TREE_OPERAND (xop0, 0);
      if (TREE_CODE (xop0) == ARRAY_REF)
	{
	  TREE_SIDE_EFFECTS (xop0) = 1;
	  TREE_SIDE_EFFECTS (op0) = 1;
	}
    }
  t = fold_build2 (COMPOUND_EXPR, TREE_TYPE (t), op0, t);
  t = fold_build2 (TRUTH_OR_EXPR, boolean_type_node, t,
		   tt ? tt : integer_zero_node);

  if (flag_sanitize_undefined_trap_on_error)
    tt = build_call_expr_loc (loc, builtin_decl_explicit (BUILT_IN_TRAP), 0);
  else
    {
      tree data = ubsan_create_data ("__ubsan_shift_data", &loc, NULL,
				     ubsan_type_descriptor (type0),
				     ubsan_type_descriptor (type1), NULL_TREE);
      data = build_fold_addr_expr_loc (loc, data);

      enum built_in_function bcode
	= flag_sanitize_recover
	  ? BUILT_IN_UBSAN_HANDLE_SHIFT_OUT_OF_BOUNDS
	  : BUILT_IN_UBSAN_HANDLE_SHIFT_OUT_OF_BOUNDS_ABORT;
      tt = builtin_decl_explicit (bcode);
      tt = build_call_expr_loc (loc, tt, 3, data, ubsan_encode_value (op0),
				ubsan_encode_value (op1));
    }
  t = fold_build3 (COND_EXPR, void_type_node, t, tt, void_node);

  return t;
}

/* Instrument variable length array bound.  */

tree
ubsan_instrument_vla (location_t loc, tree size)
{
  tree type = TREE_TYPE (size);
  tree t, tt;

  t = fold_build2 (LE_EXPR, boolean_type_node, size, build_int_cst (type, 0));
  if (flag_sanitize_undefined_trap_on_error)
    tt = build_call_expr_loc (loc, builtin_decl_explicit (BUILT_IN_TRAP), 0);
  else
    {
      tree data = ubsan_create_data ("__ubsan_vla_data", &loc, NULL,
				     ubsan_type_descriptor (type), NULL_TREE);
      data = build_fold_addr_expr_loc (loc, data);
      enum built_in_function bcode
	= flag_sanitize_recover
	  ? BUILT_IN_UBSAN_HANDLE_VLA_BOUND_NOT_POSITIVE
	  : BUILT_IN_UBSAN_HANDLE_VLA_BOUND_NOT_POSITIVE_ABORT;
      tt = builtin_decl_explicit (bcode);
      tt = build_call_expr_loc (loc, tt, 2, data, ubsan_encode_value (size));
    }
  t = fold_build3 (COND_EXPR, void_type_node, t, tt, void_node);

  return t;
}

/* Instrument missing return in C++ functions returning non-void.  */

tree
ubsan_instrument_return (location_t loc)
{
  if (flag_sanitize_undefined_trap_on_error)
    return build_call_expr_loc (loc, builtin_decl_explicit (BUILT_IN_TRAP), 0);
  /* It is possible that PCH zapped table with definitions of sanitizer
     builtins.  Reinitialize them if needed.  */
  initialize_sanitizer_builtins ();

  tree data = ubsan_create_data ("__ubsan_missing_return_data", &loc,
				 NULL, NULL_TREE);
  tree t = builtin_decl_explicit (BUILT_IN_UBSAN_HANDLE_MISSING_RETURN);
  return build_call_expr_loc (loc, t, 1, build_fold_addr_expr_loc (loc, data));
}

/* Instrument array bounds for ARRAY_REFs.  We create special builtin,
   that gets expanded in the sanopt pass, and make an array dimension
   of it.  ARRAY is the array, *INDEX is an index to the array.
   Return NULL_TREE if no instrumentation is emitted.
   IGNORE_OFF_BY_ONE is true if the ARRAY_REF is inside a ADDR_EXPR.  */

tree
ubsan_instrument_bounds (location_t loc, tree array, tree *index,
			 bool ignore_off_by_one)
{
  tree type = TREE_TYPE (array);
  tree domain = TYPE_DOMAIN (type);

  if (domain == NULL_TREE)
    return NULL_TREE;

  tree bound = TYPE_MAX_VALUE (domain);
  if (ignore_off_by_one)
    bound = fold_build2 (PLUS_EXPR, TREE_TYPE (bound), bound,
			 build_int_cst (TREE_TYPE (bound), 1));

  /* Detect flexible array members and suchlike.  */
  tree base = get_base_address (array);
  if (base && (TREE_CODE (base) == INDIRECT_REF
	       || TREE_CODE (base) == MEM_REF))
    {
      tree next = NULL_TREE;
      tree cref = array;

      /* Walk all structs/unions.  */
      while (TREE_CODE (cref) == COMPONENT_REF)
	{
	  if (TREE_CODE (TREE_TYPE (TREE_OPERAND (cref, 0))) == RECORD_TYPE)
	    for (next = DECL_CHAIN (TREE_OPERAND (cref, 1));
		 next && TREE_CODE (next) != FIELD_DECL;
		 next = DECL_CHAIN (next))
	      ;
	  if (next)
	    /* Not a last element.  Instrument it.  */
	    break;
	  /* Ok, this is the last field of the structure/union.  But the
	     aggregate containing the field must be the last field too,
	     recursively.  */
	  cref = TREE_OPERAND (cref, 0);
	}
      if (!next)
	/* Don't instrument this flexible array member-like array in non-strict
	   -fsanitize=bounds mode.  */
        return NULL_TREE;
    }

  *index = save_expr (*index);
  /* Create a "(T *) 0" tree node to describe the array type.  */
  tree zero_with_type = build_int_cst (build_pointer_type (type), 0);
  return build_call_expr_internal_loc (loc, IFN_UBSAN_BOUNDS,
				       void_type_node, 3, zero_with_type,
				       *index, bound);
}

/* Return true iff T is an array that was instrumented by SANITIZE_BOUNDS.  */

bool
ubsan_array_ref_instrumented_p (const_tree t)
{
  if (TREE_CODE (t) != ARRAY_REF)
    return false;

  tree op1 = TREE_OPERAND (t, 1);
  return TREE_CODE (op1) == COMPOUND_EXPR
	 && TREE_CODE (TREE_OPERAND (op1, 0)) == CALL_EXPR
	 && CALL_EXPR_FN (TREE_OPERAND (op1, 0)) == NULL_TREE
	 && CALL_EXPR_IFN (TREE_OPERAND (op1, 0)) == IFN_UBSAN_BOUNDS;
}

/* Instrument an ARRAY_REF, if it hasn't already been instrumented.
   IGNORE_OFF_BY_ONE is true if the ARRAY_REF is inside a ADDR_EXPR.  */

void
ubsan_maybe_instrument_array_ref (tree *expr_p, bool ignore_off_by_one)
{
  if (!ubsan_array_ref_instrumented_p (*expr_p)
      && current_function_decl != NULL_TREE
      && !lookup_attribute ("no_sanitize_undefined",
			    DECL_ATTRIBUTES (current_function_decl)))
    {
      tree op0 = TREE_OPERAND (*expr_p, 0);
      tree op1 = TREE_OPERAND (*expr_p, 1);
      tree e = ubsan_instrument_bounds (EXPR_LOCATION (*expr_p), op0, &op1,
					ignore_off_by_one);
      if (e != NULL_TREE)
	{
	  tree t = copy_node (*expr_p);
	  TREE_OPERAND (t, 1) = build2 (COMPOUND_EXPR, TREE_TYPE (op1),
					e, op1);
	  *expr_p = t;
	}
    }
}
