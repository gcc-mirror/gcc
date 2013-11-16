/* UndefinedBehaviorSanitizer, undefined behavior detector.
   Copyright (C) 2013 Free Software Foundation, Inc.
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

  /* TODO: REAL_TYPE is not supported yet.  */
  if (TREE_CODE (type) != INTEGER_TYPE)
    return NULL_TREE;

  t = fold_build2 (EQ_EXPR, boolean_type_node,
		    op1, build_int_cst (type, 0));

  /* We check INT_MIN / -1 only for signed types.  */
  if (!TYPE_UNSIGNED (type))
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
     make sure it gets evaluated before the condition.  */
  t = fold_build2 (COMPOUND_EXPR, TREE_TYPE (t), op0, t);
  tree data = ubsan_create_data ("__ubsan_overflow_data",
				 loc, ubsan_type_descriptor (type),
				 NULL_TREE);
  data = build_fold_addr_expr_loc (loc, data);
  tt = builtin_decl_explicit (BUILT_IN_UBSAN_HANDLE_DIVREM_OVERFLOW);
  tt = build_call_expr_loc (loc, tt, 3, data, ubsan_encode_value (op0),
			    ubsan_encode_value (op1));
  t = fold_build3 (COND_EXPR, void_type_node, t, tt, void_zero_node);

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
     make sure it gets evaluated before the condition.  */
  t = fold_build2 (COMPOUND_EXPR, TREE_TYPE (t), op0, t);
  tree data = ubsan_create_data ("__ubsan_shift_data",
				 loc, ubsan_type_descriptor (type0),
				 ubsan_type_descriptor (type1), NULL_TREE);

  data = build_fold_addr_expr_loc (loc, data);

  t = fold_build2 (TRUTH_OR_EXPR, boolean_type_node, t,
		   tt ? tt : integer_zero_node);
  tt = builtin_decl_explicit (BUILT_IN_UBSAN_HANDLE_SHIFT_OUT_OF_BOUNDS);
  tt = build_call_expr_loc (loc, tt, 3, data, ubsan_encode_value (op0),
			    ubsan_encode_value (op1));
  t = fold_build3 (COND_EXPR, void_type_node, t, tt, void_zero_node);

  return t;
}

/* Instrument variable length array bound.  */

tree
ubsan_instrument_vla (location_t loc, tree size)
{
  tree type = TREE_TYPE (size);
  tree t, tt;

  t = fold_build2 (LE_EXPR, boolean_type_node, size, build_int_cst (type, 0));
  tree data = ubsan_create_data ("__ubsan_vla_data",
				 loc, ubsan_type_descriptor (type), NULL_TREE);
  data = build_fold_addr_expr_loc (loc, data);
  tt = builtin_decl_explicit (BUILT_IN_UBSAN_HANDLE_VLA_BOUND_NOT_POSITIVE);
  tt = build_call_expr_loc (loc, tt, 2, data, ubsan_encode_value (size));
  t = fold_build3 (COND_EXPR, void_type_node, t, tt, void_zero_node);

  return t;
}
