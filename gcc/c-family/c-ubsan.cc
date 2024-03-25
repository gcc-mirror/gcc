/* UndefinedBehaviorSanitizer, undefined behavior detector.
   Copyright (C) 2013-2024 Free Software Foundation, Inc.
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
#include "tm.h"
#include "c-family/c-common.h"
#include "ubsan.h"
#include "c-family/c-ubsan.h"
#include "stor-layout.h"
#include "builtins.h"
#include "gimplify.h"
#include "stringpool.h"
#include "attribs.h"
#include "asan.h"
#include "langhooks.h"

/* Instrument division by zero and INT_MIN / -1.  If not instrumenting,
   return NULL_TREE.  */

tree
ubsan_instrument_division (location_t loc, tree op0, tree op1)
{
  tree t, tt, x = NULL_TREE;
  tree type = TREE_TYPE (op0);
  enum sanitize_code flag = SANITIZE_DIVIDE;

  /* At this point both operands should have the same type,
     because they are already converted to RESULT_TYPE.
     Use TYPE_MAIN_VARIANT since typedefs can confuse us.  */
  tree top0 = TYPE_MAIN_VARIANT (type);
  tree top1 = TYPE_MAIN_VARIANT (TREE_TYPE (op1));
  gcc_checking_assert (lang_hooks.types_compatible_p (top0, top1));

  op0 = unshare_expr (op0);
  op1 = unshare_expr (op1);

  if (INTEGRAL_TYPE_P (type)
      && sanitize_flags_p (SANITIZE_DIVIDE))
    t = fold_build2 (EQ_EXPR, boolean_type_node,
		     op1, build_int_cst (type, 0));
  else if (SCALAR_FLOAT_TYPE_P (type)
	   && sanitize_flags_p (SANITIZE_FLOAT_DIVIDE))
    {
      t = fold_build2 (EQ_EXPR, boolean_type_node,
		       op1, build_real (type, dconst0));
      flag = SANITIZE_FLOAT_DIVIDE;
    }
  else
    t = NULL_TREE;

  /* We check INT_MIN / -1 only for signed types.  */
  if (INTEGRAL_TYPE_P (type)
      && sanitize_flags_p (SANITIZE_SI_OVERFLOW)
      && !TYPE_UNSIGNED (type))
    {
      tt = fold_build2 (EQ_EXPR, boolean_type_node, unshare_expr (op1),
			build_int_cst (type, -1));
      x = fold_build2 (EQ_EXPR, boolean_type_node, op0,
		       TYPE_MIN_VALUE (type));
      x = fold_build2 (TRUTH_AND_EXPR, boolean_type_node, x, tt);
      if (t == NULL_TREE || integer_zerop (t))
	{
	  t = x;
	  x = NULL_TREE;
	  flag = SANITIZE_SI_OVERFLOW;
	}
      else if ((((flag_sanitize_trap & SANITIZE_DIVIDE) == 0)
		== ((flag_sanitize_trap & SANITIZE_SI_OVERFLOW) == 0))
	       && (((flag_sanitize_recover & SANITIZE_DIVIDE) == 0)
		   == ((flag_sanitize_recover & SANITIZE_SI_OVERFLOW) == 0)))
	{
	  t = fold_build2 (TRUTH_OR_EXPR, boolean_type_node, t, x);
	  x = NULL_TREE;
	}
      else if (integer_zerop (x))
	x = NULL_TREE;
    }
  else if (t == NULL_TREE)
    return NULL_TREE;

  /* If the condition was folded to 0, no need to instrument
     this expression.  */
  if (integer_zerop (t))
    return NULL_TREE;

  /* In case we have a SAVE_EXPR in a conditional context, we need to
     make sure it gets evaluated before the condition.  */
  t = fold_build2 (COMPOUND_EXPR, TREE_TYPE (t), unshare_expr (op0), t);
  t = fold_build2 (COMPOUND_EXPR, TREE_TYPE (t), unshare_expr (op1), t);
  if ((flag_sanitize_trap & flag) && x == NULL_TREE)
    tt = build_call_expr_loc (loc, builtin_decl_explicit (BUILT_IN_TRAP), 0);
  else
    {
      tree data = ubsan_create_data ("__ubsan_overflow_data", 1, &loc,
				     ubsan_type_descriptor (type), NULL_TREE,
				     NULL_TREE);
      data = build_fold_addr_expr_loc (loc, data);
      if (flag_sanitize_trap & flag)
	tt = build_call_expr_loc (loc, builtin_decl_explicit (BUILT_IN_TRAP),
				  0);
      else
	{
	  enum built_in_function bcode
	    = (flag_sanitize_recover & flag)
	      ? BUILT_IN_UBSAN_HANDLE_DIVREM_OVERFLOW
	      : BUILT_IN_UBSAN_HANDLE_DIVREM_OVERFLOW_ABORT;
	  tt = builtin_decl_explicit (bcode);
	  op0 = unshare_expr (op0);
	  op1 = unshare_expr (op1);
	  tt = build_call_expr_loc (loc, tt, 3, data, ubsan_encode_value (op0),
				    ubsan_encode_value (op1));
	}
      if (x)
	{
	  tree xt;
	  if (flag_sanitize_trap & SANITIZE_SI_OVERFLOW)
	    xt = build_call_expr_loc (loc,
				      builtin_decl_explicit (BUILT_IN_TRAP),
				      0);
	  else
	    {
	      enum built_in_function bcode
		= (flag_sanitize_recover & SANITIZE_SI_OVERFLOW)
		   ? BUILT_IN_UBSAN_HANDLE_DIVREM_OVERFLOW
		   : BUILT_IN_UBSAN_HANDLE_DIVREM_OVERFLOW_ABORT;
	      xt = builtin_decl_explicit (bcode);
	      op0 = unshare_expr (op0);
	      op1 = unshare_expr (op1);
	      xt = build_call_expr_loc (loc, xt, 3, data,
					ubsan_encode_value (op0),
					ubsan_encode_value (op1));
	    }
	  x = fold_build3 (COND_EXPR, void_type_node, x, xt, void_node);
	}
    }
  t = fold_build3 (COND_EXPR, void_type_node, t, tt, x ? x : void_node);

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
  if (!INTEGRAL_TYPE_P (type0))
    return NULL_TREE;

  tree op1_utype = unsigned_type_for (type1);
  HOST_WIDE_INT op0_prec = TYPE_PRECISION (type0);
  tree uprecm1 = build_int_cst (op1_utype, op0_prec - 1);

  op0 = unshare_expr (op0);
  op1 = unshare_expr (op1);

  t = fold_convert_loc (loc, op1_utype, op1);
  t = fold_build2 (GT_EXPR, boolean_type_node, t, uprecm1);

  /* If this is not a signed operation, don't perform overflow checks.
     Also punt on bit-fields.  */
  if (TYPE_OVERFLOW_WRAPS (type0)
      || maybe_ne (GET_MODE_BITSIZE (TYPE_MODE (type0)),
		   TYPE_PRECISION (type0))
      || !sanitize_flags_p (SANITIZE_SHIFT_BASE)
      /* In C++20 and later, shifts are well defined except when
	 the second operand is not within bounds.  */
      || cxx_dialect >= cxx20)
    ;

  /* For signed x << y, in C99 and later, the following:
     (unsigned) x >> (uprecm1 - y)
     if non-zero, is undefined.  */
  else if (code == LSHIFT_EXPR && flag_isoc99 && cxx_dialect < cxx11)
    {
      tree x = fold_build2 (MINUS_EXPR, op1_utype, uprecm1,
			    fold_convert (op1_utype, unshare_expr (op1)));
      tt = fold_convert_loc (loc, unsigned_type_for (type0), op0);
      tt = fold_build2 (RSHIFT_EXPR, TREE_TYPE (tt), tt, x);
      tt = fold_build2 (NE_EXPR, boolean_type_node, tt,
			build_int_cst (TREE_TYPE (tt), 0));
    }

  /* For signed x << y, in C++11 to C++17, the following:
     x < 0 || ((unsigned) x >> (uprecm1 - y))
     if > 1, is undefined.  */
  else if (code == LSHIFT_EXPR && cxx_dialect >= cxx11)
    {
      tree x = fold_build2 (MINUS_EXPR, op1_utype, uprecm1,
			    fold_convert (op1_utype, unshare_expr (op1)));
      tt = fold_convert_loc (loc, unsigned_type_for (type0),
			     unshare_expr (op0));
      tt = fold_build2 (RSHIFT_EXPR, TREE_TYPE (tt), tt, x);
      tt = fold_build2 (GT_EXPR, boolean_type_node, tt,
			build_int_cst (TREE_TYPE (tt), 1));
      x = fold_build2 (LT_EXPR, boolean_type_node, unshare_expr (op0),
		       build_int_cst (type0, 0));
      tt = fold_build2 (TRUTH_OR_EXPR, boolean_type_node, x, tt);
    }

  /* If the condition was folded to 0, no need to instrument
     this expression.  */
  if (integer_zerop (t) && (tt == NULL_TREE || integer_zerop (tt)))
    return NULL_TREE;

  /* In case we have a SAVE_EXPR in a conditional context, we need to
     make sure it gets evaluated before the condition.  */
  t = fold_build2 (COMPOUND_EXPR, TREE_TYPE (t), unshare_expr (op0), t);
  t = fold_build2 (COMPOUND_EXPR, TREE_TYPE (t), unshare_expr (op1), t);

  enum sanitize_code recover_kind = SANITIZE_SHIFT_EXPONENT;
  tree else_t = void_node;
  if (tt)
    {
      if (!sanitize_flags_p (SANITIZE_SHIFT_EXPONENT))
	{
	  t = fold_build1 (TRUTH_NOT_EXPR, boolean_type_node, t);
	  t = fold_build2 (TRUTH_AND_EXPR, boolean_type_node, t, tt);
	  recover_kind = SANITIZE_SHIFT_BASE;
	}
      else
	{
	  if (((!(flag_sanitize_trap & SANITIZE_SHIFT_EXPONENT))
	       == (!(flag_sanitize_trap & SANITIZE_SHIFT_BASE)))
	      && ((!(flag_sanitize_recover & SANITIZE_SHIFT_EXPONENT))
		  == (!(flag_sanitize_recover & SANITIZE_SHIFT_BASE))))
	    t = fold_build2 (TRUTH_OR_EXPR, boolean_type_node, t, tt);
	  else
	    else_t = tt;
	}
    }

  if ((flag_sanitize_trap & recover_kind) && else_t == void_node)
    tt = build_call_expr_loc (loc, builtin_decl_explicit (BUILT_IN_TRAP), 0);
  else
    {
      if (TREE_CODE (type1) == BITINT_TYPE
	  && TYPE_PRECISION (type1) > MAX_FIXED_MODE_SIZE)
	{
	  /* Workaround for missing _BitInt support in libsanitizer.
	     Instead of crashing in the library, pretend values above
	     maximum value of normal integral type or below minimum value
	     of that type are those extremes.  */
	  tree type2 = build_nonstandard_integer_type (MAX_FIXED_MODE_SIZE,
						       TYPE_UNSIGNED (type1));
	  tree op2 = op1;
	  if (!TYPE_UNSIGNED (type1))
	    {
	      op2 = fold_build2 (LT_EXPR, boolean_type_node, unshare_expr (op1),
				 fold_convert (type1, TYPE_MIN_VALUE (type2)));
	      op2 = fold_build3 (COND_EXPR, type2, op2, TYPE_MIN_VALUE (type2),
				 fold_convert (type2, unshare_expr (op1)));
	    }
	  else
	    op2 = fold_convert (type2, op1);
	  tree op3
	    = fold_build2 (GT_EXPR, boolean_type_node, unshare_expr (op1),
			   fold_convert (type1, TYPE_MAX_VALUE (type2)));
	  op1 = fold_build3 (COND_EXPR, type2, op3, TYPE_MAX_VALUE (type2),
			     op2);
	  type1 = type2;
	}
      tree utd0 = ubsan_type_descriptor (type0, UBSAN_PRINT_FORCE_INT);
      tree data = ubsan_create_data ("__ubsan_shift_data", 1, &loc, utd0,
				     ubsan_type_descriptor (type1), NULL_TREE,
				     NULL_TREE);
      data = build_fold_addr_expr_loc (loc, data);

      if (flag_sanitize_trap & recover_kind)
	tt = build_call_expr_loc (loc, builtin_decl_explicit (BUILT_IN_TRAP), 0);
      else
	{
	  enum built_in_function bcode
	    = (flag_sanitize_recover & recover_kind)
	      ? BUILT_IN_UBSAN_HANDLE_SHIFT_OUT_OF_BOUNDS
	      : BUILT_IN_UBSAN_HANDLE_SHIFT_OUT_OF_BOUNDS_ABORT;
	  tt = builtin_decl_explicit (bcode);
	  op0 = unshare_expr (op0);
	  op1 = unshare_expr (op1);
	  tt = build_call_expr_loc (loc, tt, 3, data, ubsan_encode_value (op0),
				    ubsan_encode_value (op1));
	}
      if (else_t != void_node)
	{
	  tree else_tt;
	  if (flag_sanitize_trap & SANITIZE_SHIFT_BASE)
	    else_tt
	      = build_call_expr_loc (loc,
				     builtin_decl_explicit (BUILT_IN_TRAP), 0);
	  else
	    {
	      enum built_in_function bcode
		= (flag_sanitize_recover & SANITIZE_SHIFT_BASE)
		  ? BUILT_IN_UBSAN_HANDLE_SHIFT_OUT_OF_BOUNDS
		  : BUILT_IN_UBSAN_HANDLE_SHIFT_OUT_OF_BOUNDS_ABORT;
	      else_tt = builtin_decl_explicit (bcode);
	      op0 = unshare_expr (op0);
	      op1 = unshare_expr (op1);
	      else_tt = build_call_expr_loc (loc, else_tt, 3, data,
					     ubsan_encode_value (op0),
					     ubsan_encode_value (op1));
	    }
	  else_t = fold_build3 (COND_EXPR, void_type_node, else_t,
				else_tt, void_node);
	}
    }
  t = fold_build3 (COND_EXPR, void_type_node, t, tt, else_t);

  return t;
}

/* Instrument variable length array bound.  */

tree
ubsan_instrument_vla (location_t loc, tree size)
{
  tree type = TREE_TYPE (size);
  tree t, tt;

  t = fold_build2 (LE_EXPR, boolean_type_node, size, build_int_cst (type, 0));
  if (flag_sanitize_trap & SANITIZE_VLA)
    tt = build_call_expr_loc (loc, builtin_decl_explicit (BUILT_IN_TRAP), 0);
  else
    {
      tree data = ubsan_create_data ("__ubsan_vla_data", 1, &loc,
				     ubsan_type_descriptor (type), NULL_TREE,
				     NULL_TREE);
      data = build_fold_addr_expr_loc (loc, data);
      enum built_in_function bcode
	= (flag_sanitize_recover & SANITIZE_VLA)
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
  if (flag_sanitize_trap & SANITIZE_RETURN)
    /* pass_warn_function_return checks for BUILTINS_LOCATION.  */
    return build_call_expr_loc (BUILTINS_LOCATION,
				builtin_decl_explicit (BUILT_IN_TRAP), 0);

  tree data = ubsan_create_data ("__ubsan_missing_return_data", 1, &loc,
				 NULL_TREE, NULL_TREE);
  tree t = builtin_decl_explicit (BUILT_IN_UBSAN_HANDLE_MISSING_RETURN);
  return build_call_expr_loc (loc, t, 1, build_fold_addr_expr_loc (loc, data));
}

/* Instrument array bounds for ARRAY_REFs.  We create special builtin,
   that gets expanded in the sanopt pass, and make an array dimension
   of it.  ARRAY is the array, *INDEX is an index to the array.
   Return NULL_TREE if no instrumentation is emitted.
   IGNORE_OFF_BY_ONE is true if the ARRAY_REF is inside an ADDR_EXPR.  */

tree
ubsan_instrument_bounds (location_t loc, tree array, tree *index,
			 bool ignore_off_by_one)
{
  tree type = TREE_TYPE (array);
  tree domain = TYPE_DOMAIN (type);

  if (domain == NULL_TREE)
    return NULL_TREE;

  tree bound = TYPE_MAX_VALUE (domain);
  if (!bound)
    {
      /* Handle C [0] arrays, which have TYPE_MAX_VALUE NULL, like
	 C++ [0] arrays which have TYPE_MIN_VALUE 0 TYPE_MAX_VALUE -1.  */
      if (!c_dialect_cxx ()
	  && COMPLETE_TYPE_P (type)
	  && integer_zerop (TYPE_SIZE (type)))
	bound = build_int_cst (TREE_TYPE (TYPE_MIN_VALUE (domain)), -1);
      else
	return NULL_TREE;
    }

  bound = fold_build2 (PLUS_EXPR, TREE_TYPE (bound), bound,
		       build_int_cst (TREE_TYPE (bound),
		       1 + ignore_off_by_one));

  /* Detect flexible array members and suchlike, unless
     -fsanitize=bounds-strict.  */
  tree base = get_base_address (array);
  if (!sanitize_flags_p (SANITIZE_BOUNDS_STRICT)
      && TREE_CODE (array) == COMPONENT_REF
      && base && (INDIRECT_REF_P (base) || TREE_CODE (base) == MEM_REF))
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
	  if (TREE_CODE (TREE_TYPE (TREE_OPERAND (cref, 1))) == ARRAY_TYPE
	      && !c_dialect_cxx ())
	    {
	      unsigned l
		= c_strict_flex_array_level_of (TREE_OPERAND (cref, 1));
	      tree type2 = TREE_TYPE (TREE_OPERAND (cref, 1));
	      if (TYPE_DOMAIN (type2) != NULL_TREE)
		{
		  tree max = TYPE_MAX_VALUE (TYPE_DOMAIN (type2));
		  if (max == NULL_TREE)
		    {
		      /* C [0] */
		      if (COMPLETE_TYPE_P (type2)
			  && integer_zerop (TYPE_SIZE (type2))
			  && l == 3)
			next = TREE_OPERAND (cref, 1);
		    }
		  else if (TREE_CODE (max) == INTEGER_CST)
		    {
		      if (c_dialect_cxx ()
			  && integer_all_onesp (max))
			{
			  /* C++ [0] */
			  if (l == 3)
			    next = TREE_OPERAND (cref, 1);
			}
		      else if (integer_zerop (max))
			{
			  /* C/C++ [1] */
			  if (l >= 2)
			    next = TREE_OPERAND (cref, 1);
			}
		      else if (l >= 1)
			next = TREE_OPERAND (cref, 1);
		    }
		}
	      if (next)
		break;
	    }
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

  /* Don't emit instrumentation in the most common cases.  */
  tree idx = NULL_TREE;
  if (TREE_CODE (*index) == INTEGER_CST)
    idx = *index;
  else if (TREE_CODE (*index) == BIT_AND_EXPR
	   && TREE_CODE (TREE_OPERAND (*index, 1)) == INTEGER_CST)
    idx = TREE_OPERAND (*index, 1);
  if (idx
      && TREE_CODE (bound) == INTEGER_CST
      && tree_int_cst_sgn (idx) >= 0
      && tree_int_cst_lt (idx, bound))
    return NULL_TREE;

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
      && sanitize_flags_p (SANITIZE_BOUNDS | SANITIZE_BOUNDS_STRICT)
      && current_function_decl != NULL_TREE)
    {
      tree op0 = TREE_OPERAND (*expr_p, 0);
      tree op1 = TREE_OPERAND (*expr_p, 1);
      tree e = ubsan_instrument_bounds (EXPR_LOCATION (*expr_p), op0, &op1,
					ignore_off_by_one);
      if (e != NULL_TREE)
	TREE_OPERAND (*expr_p, 1) = build2 (COMPOUND_EXPR, TREE_TYPE (op1),
					    e, op1);
    }
}

static tree
ubsan_maybe_instrument_reference_or_call (location_t loc, tree op, tree ptype,
					  enum ubsan_null_ckind ckind)
{
  if (!sanitize_flags_p (SANITIZE_ALIGNMENT | SANITIZE_NULL)
      || current_function_decl == NULL_TREE)
    return NULL_TREE;

  tree type = TREE_TYPE (ptype);
  tree orig_op = op;
  bool instrument = false;
  unsigned int mina = 0;

  if (sanitize_flags_p (SANITIZE_ALIGNMENT))
    {
      mina = min_align_of_type (type);
      if (mina <= 1)
	mina = 0;
    }
  while ((TREE_CODE (op) == NOP_EXPR
	  || TREE_CODE (op) == NON_LVALUE_EXPR)
	 && TREE_CODE (TREE_TYPE (op)) == POINTER_TYPE)
    op = TREE_OPERAND (op, 0);
  if (TREE_CODE (op) == NOP_EXPR
      && TREE_CODE (TREE_TYPE (op)) == REFERENCE_TYPE)
    {
      if (mina && mina > min_align_of_type (TREE_TYPE (TREE_TYPE (op))))
	instrument = true;
    }
  else
    {
      if (sanitize_flags_p (SANITIZE_NULL) && TREE_CODE (op) == ADDR_EXPR)
	{
	  bool strict_overflow_p = false;
	  /* tree_single_nonzero_warnv_p will not return true for non-weak
	     non-automatic decls with -fno-delete-null-pointer-checks,
	     which is disabled during -fsanitize=null.  We don't want to
	     instrument those, just weak vars though.  */
	  int save_flag_delete_null_pointer_checks
	    = flag_delete_null_pointer_checks;
	  flag_delete_null_pointer_checks = 1;
	  if (!tree_single_nonzero_warnv_p (op, &strict_overflow_p)
	      || strict_overflow_p)
	    instrument = true;
	  flag_delete_null_pointer_checks
	    = save_flag_delete_null_pointer_checks;
	}
      else if (sanitize_flags_p (SANITIZE_NULL))
	instrument = true;
      if (mina && mina > 1)
	{
	  if (!POINTER_TYPE_P (TREE_TYPE (op))
	      || mina > get_pointer_alignment (op) / BITS_PER_UNIT)
	    instrument = true;
	}
    }
  if (!instrument)
    return NULL_TREE;
  op = save_expr (orig_op);
  gcc_assert (POINTER_TYPE_P (ptype));
  if (TREE_CODE (ptype) == REFERENCE_TYPE)
    ptype = build_pointer_type (TREE_TYPE (ptype));
  tree kind = build_int_cst (ptype, ckind);
  tree align = build_int_cst (pointer_sized_int_node, mina);
  tree call
    = build_call_expr_internal_loc (loc, IFN_UBSAN_NULL, void_type_node,
				    3, op, kind, align);
  TREE_SIDE_EFFECTS (call) = 1;
  return fold_build2 (COMPOUND_EXPR, TREE_TYPE (op), call, op);
}

/* Instrument a NOP_EXPR to REFERENCE_TYPE or INTEGER_CST with REFERENCE_TYPE
   type if needed.  */

void
ubsan_maybe_instrument_reference (tree *stmt_p)
{
  tree stmt = *stmt_p;
  tree op = stmt;
  if (TREE_CODE (stmt) == NOP_EXPR)
    op = TREE_OPERAND (stmt, 0);
  op = ubsan_maybe_instrument_reference_or_call (EXPR_LOCATION (stmt), op,
						 TREE_TYPE (stmt),
						 UBSAN_REF_BINDING);
  if (op)
    {
      if (TREE_CODE (stmt) == NOP_EXPR) 
	TREE_OPERAND (stmt, 0) = op;
      else
	*stmt_p = op;
    }
}

/* Instrument a CALL_EXPR to a method if needed.  */

void
ubsan_maybe_instrument_member_call (tree stmt, bool is_ctor)
{
  if (call_expr_nargs (stmt) == 0)
    return;
  tree op = CALL_EXPR_ARG (stmt, 0);
  if (op == error_mark_node
      || !POINTER_TYPE_P (TREE_TYPE (op)))
    return;
  op = ubsan_maybe_instrument_reference_or_call (EXPR_LOCATION (stmt), op,
						 TREE_TYPE (op),
						 is_ctor ? UBSAN_CTOR_CALL
						 : UBSAN_MEMBER_CALL);
  if (op)
    CALL_EXPR_ARG (stmt, 0) = op;
}
