/* Lower Algol 68 pre-defined operators and procedures.
   Copyright (C) 2025 Jose E. Marchesi.

   Written by Jose E. Marchesi.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"

#include "tree.h"
#include "fold-const.h"
#include "diagnostic.h"
#include "langhooks.h"
#include "tm.h"
#include "function.h"
#include "cgraph.h"
#include "toplev.h"
#include "varasm.h"
#include "predict.h"
#include "stor-layout.h"
#include "tree-iterator.h"
#include "stringpool.h"
#include "print-tree.h"
#include "gimplify.h"
#include "convert.h"

#include "a68.h"

/* The following handlers are for lowing the entities defined in
   a68-parser-prelude.c.  */

tree
a68_lower_unimplemented (NODE_T *p,
			 LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  fatal_error (a68_get_node_location (p),
	       "no lowering routine installed for construct.  jemarch has been lazy");
}

tree
a68_lower_charabs2 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op = a68_lower_tree (NEXT (SUB (p)), ctx);
  return a68_char_abs (op);
}

tree
a68_lower_boolabs2 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op = a68_lower_tree (NEXT (SUB (p)), ctx);
  return a68_bool_abs (op);
}

tree
a68_lower_intabs2 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op = a68_lower_tree (NEXT (SUB (p)), ctx);
  return a68_int_abs (op);
}

tree
a68_lower_realabs2 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op = a68_lower_tree (NEXT (SUB (p)), ctx);
  return a68_real_abs (op);
}

tree
a68_lower_confirm2 (NODE_T *p, LOW_CTX_T ctx)
{
  /* Used to implement monadic +.  */
  return a68_lower_tree (NEXT (SUB (p)), ctx);
}

tree
a68_lower_negate2 (NODE_T *p, LOW_CTX_T ctx)
{
  return fold_build1_loc (a68_get_node_location (p),
			  NEGATE_EXPR,
			  CTYPE (MOID (p)),
			  a68_lower_tree (NEXT (SUB (p)), ctx));
}

/* Lower an ENTIER standard monadic operator.  */

tree
a68_lower_entier2 (NODE_T *p, LOW_CTX_T ctx)
{
  NODE_T *op = NEXT (SUB (p));
  return a68_real_entier (a68_lower_tree (op, ctx), MOID (p), MOID (op));
}

/* Lower a ROUND standard monadic operator.

   This operator gets a LONGSETY REAL and produces a LONGSETY INT which is the
   nearest integer to the given real.  */

tree
a68_lower_round2 (NODE_T *p, LOW_CTX_T ctx)
{
  NODE_T *op = NEXT (SUB (p));
  return a68_real_round (a68_lower_tree (op, ctx), MOID (p), MOID (op));
}

tree
a68_lower_not2 (NODE_T *p, LOW_CTX_T ctx)
{
  return fold_build1_loc (a68_get_node_location (p),
			  TRUTH_NOT_EXPR,
			  CTYPE (MOID (p)),
			  a68_lower_tree (NEXT (SUB (p)), ctx));
}

tree
a68_lower_and3 (NODE_T *p, LOW_CTX_T ctx)
{
  return fold_build2_loc (a68_get_node_location (p),
			  TRUTH_AND_EXPR,
			  CTYPE (MOID (p)),
			  a68_lower_tree (SUB (p), ctx),
			  a68_lower_tree (NEXT (NEXT (SUB (p))), ctx));
}

tree
a68_lower_or3 (NODE_T *p, LOW_CTX_T ctx)
{
  return fold_build2_loc (a68_get_node_location (p),
			  TRUTH_OR_EXPR,
			  CTYPE (MOID (p)),
			  a68_lower_tree (SUB (p), ctx),
			  a68_lower_tree (NEXT (NEXT (SUB (p))), ctx));
}

tree
a68_lower_xor3 (NODE_T *p, LOW_CTX_T ctx)
{
  return fold_build2_loc (a68_get_node_location (p),
			  TRUTH_XOR_EXPR,
			  CTYPE (MOID (p)),
			  a68_lower_tree (SUB (p), ctx),
			  a68_lower_tree (NEXT (NEXT (SUB (p))), ctx));
}

tree
a68_lower_plus_int (NODE_T *p, LOW_CTX_T ctx)
{
  MOID_T *m = MOID (p);
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);
  return a68_int_plus (m, op1, op2, a68_get_node_location (p));
}

tree
a68_lower_plus_real (NODE_T *p, LOW_CTX_T ctx)
{
  MOID_T *m = MOID (p);
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);
  return a68_real_plus (m, op1, op2, a68_get_node_location (p));
}

tree
a68_lower_minus_int (NODE_T *p, LOW_CTX_T ctx)
{
  MOID_T *m = MOID (p);
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);
  return a68_int_minus (m, op1, op2, a68_get_node_location (p));
}

tree
a68_lower_minus_real (NODE_T *p, LOW_CTX_T ctx)
{
  MOID_T *m = MOID (p);
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);
  return a68_real_minus (m, op1, op2, a68_get_node_location (p));
}

tree
a68_lower_mult_int (NODE_T *p, LOW_CTX_T ctx)
{
  MOID_T *m = MOID (p);
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);
  return a68_int_mult (m, op1, op2, a68_get_node_location (p));
}

tree
a68_lower_mult_real (NODE_T *p, LOW_CTX_T ctx)
{
  MOID_T *m = MOID (p);
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);
  return a68_real_mult (m, op1, op2, a68_get_node_location (p));
}

tree
a68_lower_multab3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree lhs = a68_lower_tree (SUB (p), ctx);
  lhs = a68_consolidate_ref (MOID (SUB (p)), lhs);
  lhs = save_expr (lhs);
  NODE_T *rhs_node = NEXT (NEXT (SUB (p)));
  tree rhs = a68_lower_tree (rhs_node, ctx);
  tree operation = fold_build2_loc (a68_get_node_location (p),
				    MULT_EXPR,
				    TREE_TYPE (rhs),
				    a68_low_deref (lhs, SUB (p)),
				    rhs);

  return fold_build2_loc (a68_get_node_location (p),
			  COMPOUND_EXPR,
			  TREE_TYPE (lhs),
			  a68_low_assignation (p, lhs, MOID (SUB (p)), operation, MOID (rhs_node)),
			  lhs);
}

tree
a68_lower_over3 (NODE_T *p, LOW_CTX_T ctx)
{
  MOID_T *m = MOID (p);
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);
  return a68_int_div (m, op1, op2, a68_get_node_location (p));
}

tree
a68_lower_mod3 (NODE_T *p, LOW_CTX_T ctx)
{
  MOID_T *m = MOID (p);
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);
  return a68_int_mod (m, op1, op2, a68_get_node_location (p));
}

tree
a68_lower_div3 (NODE_T *p, LOW_CTX_T ctx)
{
  return a68_real_div (MOID (p),
		       a68_lower_tree (SUB (p), ctx),
		       a68_lower_tree (NEXT (NEXT (SUB (p))), ctx),
		       a68_get_node_location (p));
}

tree
a68_lower_rdiv3 (NODE_T *p, LOW_CTX_T ctx)
{
  return a68_real_div (MOID (p),
		       fold_build1 (FLOAT_EXPR, CTYPE (MOID (p)),
				    a68_lower_tree (SUB (p), ctx)),
		       fold_build1 (FLOAT_EXPR, CTYPE (MOID (p)),
				    a68_lower_tree (NEXT (NEXT (SUB (p))), ctx)),
		       a68_get_node_location (p));
}

tree
a68_lower_int_eq3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);

  return a68_int_eq (op1, op2, a68_get_node_location (p));
}

tree
a68_lower_int_ne3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);

  return a68_int_ne (op1, op2, a68_get_node_location (p));
}

tree
a68_lower_int_lt3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);

  return a68_int_lt (op1, op2, a68_get_node_location (p));
}

tree
a68_lower_int_le3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);

  return a68_int_le (op1, op2, a68_get_node_location (p));
}

tree
a68_lower_int_gt3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);

  return a68_int_gt (op1, op2, a68_get_node_location (p));
}

tree
a68_lower_int_ge3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);

  return a68_int_ge (op1, op2, a68_get_node_location (p));
}

tree
a68_lower_real_eq3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);

  return a68_real_eq (op1, op2, a68_get_node_location (p));
}

tree
a68_lower_real_ne3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);

  return a68_real_ne (op1, op2, a68_get_node_location (p));
}

tree
a68_lower_real_lt3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);

  return a68_real_lt (op1, op2, a68_get_node_location (p));
}

tree
a68_lower_real_le3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);

  return a68_real_le (op1, op2, a68_get_node_location (p));
}

tree
a68_lower_real_gt3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);

  return a68_real_gt (op1, op2, a68_get_node_location (p));
}

tree
a68_lower_real_ge3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);

  return a68_real_ge (op1, op2, a68_get_node_location (p));
}

tree
a68_lower_char_eq3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);

  return a68_char_eq (op1, op2, a68_get_node_location (p));
}

tree
a68_lower_char_ne3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);

  return a68_char_ne (op1, op2, a68_get_node_location (p));
}

tree
a68_lower_char_lt3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);

  return a68_char_lt (op1, op2, a68_get_node_location (p));
}

tree
a68_lower_char_le3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);

  return a68_char_le (op1, op2, a68_get_node_location (p));
}

tree
a68_lower_char_gt3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);

  return a68_char_gt (op1, op2, a68_get_node_location (p));
}

tree
a68_lower_char_ge3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);

  return a68_char_ge (op1, op2, a68_get_node_location (p));
}

tree
a68_lower_bool_eq3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);

  return a68_bool_eq (op1, op2, a68_get_node_location (p));
}

tree
a68_lower_bool_ne3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);

  return a68_bool_ne (op1, op2, a68_get_node_location (p));
}

tree
a68_lower_sign2 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op = a68_lower_tree (NEXT (SUB (p)), ctx);
  return a68_int_sign (op);
}

tree
a68_lower_realsign2 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op = a68_lower_tree (NEXT (SUB (p)), ctx);
  return a68_real_sign (op);
}

tree
a68_lower_plusab3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree lhs = a68_lower_tree (SUB (p), ctx);
  lhs = a68_consolidate_ref (MOID (SUB (p)), lhs);
  lhs = save_expr (lhs);
  NODE_T *rhs_node = NEXT (NEXT (SUB (p)));
  tree rhs = a68_lower_tree (rhs_node, ctx);
  tree operation = fold_build2_loc (a68_get_node_location (p),
				    PLUS_EXPR,
				    TREE_TYPE (rhs),
				    a68_low_deref (lhs, SUB (p)),
				    rhs);

  return fold_build2_loc (a68_get_node_location (p),
			  COMPOUND_EXPR,
			  TREE_TYPE (lhs),
			  a68_low_assignation (p, lhs, MOID (SUB (p)), operation, MOID (rhs_node)),
			  lhs);
}

tree
a68_lower_minusab3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree lhs = a68_lower_tree (SUB (p), ctx);
  lhs = a68_consolidate_ref (MOID (SUB (p)), lhs);
  lhs = save_expr (lhs);
  NODE_T *rhs_node = NEXT (NEXT (SUB (p)));
  tree rhs = a68_lower_tree (rhs_node, ctx);
  tree operation = fold_build2_loc (a68_get_node_location (p),
				    MINUS_EXPR,
				    TREE_TYPE (rhs),
				    a68_low_deref (lhs, SUB (p)),
				    rhs);

  return fold_build2_loc (a68_get_node_location (p),
			  COMPOUND_EXPR,
			  TREE_TYPE (lhs),
			  a68_low_assignation (p, lhs, MOID (SUB (p)), operation, MOID (rhs_node)),
			  lhs);
}

tree
a68_lower_overab3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree lhs = a68_lower_tree (SUB (p), ctx);
  lhs = a68_consolidate_ref (MOID (SUB (p)), lhs);
  lhs = save_expr (lhs);
  NODE_T *rhs_node = NEXT (NEXT (SUB (p)));
  tree rhs = a68_lower_tree (rhs_node, ctx);
  tree operation = fold_build2_loc (a68_get_node_location (p),
				    TRUNC_DIV_EXPR,
				    TREE_TYPE (rhs),
				    a68_low_deref (lhs, SUB (p)),
				    rhs);

  return fold_build2_loc (a68_get_node_location (p),
			  COMPOUND_EXPR,
			  TREE_TYPE (lhs),
			  a68_low_assignation (p, lhs, MOID (SUB (p)), operation, MOID (rhs_node)),
			  lhs);
}

tree
a68_lower_modab3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree lhs = a68_lower_tree (SUB (p), ctx);
  lhs = a68_consolidate_ref (MOID (SUB (p)), lhs);
  lhs = save_expr (lhs);
  NODE_T *rhs_node = NEXT (NEXT (SUB (p)));
  tree rhs = a68_lower_tree (rhs_node, ctx);
  tree operation = fold_build2_loc (a68_get_node_location (p),
				    TRUNC_MOD_EXPR,
				    TREE_TYPE (rhs),
				    a68_low_deref (lhs, SUB (p)),
				    rhs);

  return fold_build2_loc (a68_get_node_location (p),
			  COMPOUND_EXPR,
			  TREE_TYPE (lhs),
			  a68_low_assignation (p, lhs, MOID (SUB (p)), operation, MOID (rhs_node)),
			  lhs);
}

tree
a68_lower_divab3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree lhs = a68_lower_tree (SUB (p), ctx);
  lhs = a68_consolidate_ref (MOID (SUB (p)), lhs);
  lhs = save_expr (lhs);
  NODE_T *rhs_node = NEXT (NEXT (SUB (p)));
  tree rhs = a68_lower_tree (rhs_node, ctx);
  tree operation = fold_build2_loc (a68_get_node_location (p),
				    RDIV_EXPR,
				    TREE_TYPE (rhs),
				    a68_low_deref (lhs, SUB (p)),
				    rhs);

  return fold_build2_loc (a68_get_node_location (p),
			  COMPOUND_EXPR,
			  TREE_TYPE (lhs),
			  a68_low_assignation (p, lhs, MOID (SUB (p)), operation, MOID (rhs_node)),
			  lhs);
}

/* UPB comes in two flavors.

   The unary operator returns the upper bound of the first dimension of the
   operand multple.

   The binary operator returns the upper bound of the given dimension of the
   operand multiple.  The dimension is one-based.  If the specified dimension
   is out of bounds then an a run-time error is raised.  */

static tree
upb (NODE_T *p, tree boundable, tree dim)
{
  boundable = save_expr (boundable);
  dim = save_expr (dim);

  /* BOUNDABLE can be a multiple or a ROWS.  */
  tree zero_based_dim
    = save_expr (fold_build2 (MINUS_EXPR, TREE_TYPE (dim), dim, size_one_node));
  tree type = TREE_TYPE (boundable);
  if (A68_ROW_TYPE_P (type))
    {
      return fold_build2 (COMPOUND_EXPR, ssizetype,
			  a68_multiple_dim_check (p, boundable, dim),
			  a68_multiple_upper_bound (boundable, zero_based_dim));
    }
  else if (A68_ROWS_TYPE_P (type))
    {
      return fold_build2 (COMPOUND_EXPR, ssizetype,
			  a68_rows_dim_check (p, boundable, dim),
			  a68_rows_upper_bound (boundable, zero_based_dim));
    }
  else
    gcc_unreachable ();
}

tree
a68_lower_upb2 (NODE_T *p, LOW_CTX_T ctx)
{
  tree multiple = a68_lower_tree (NEXT (SUB (p)), ctx);
  return fold_convert (CTYPE (MOID (p)), upb (p, multiple, size_one_node));
}

tree
a68_lower_upb3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree dim = fold_convert (sizetype, a68_lower_tree (SUB (p), ctx));
  tree multiple = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);
  return fold_convert (CTYPE (MOID (p)), upb (p, multiple, dim));
}

/* LWB comes in two flavors.

   The unary operator returns the lower bound of the first dimension of the
   operand multple.

   The binary operator returns the lower bound of the given dimension of the
   operand multiple.  The dimension is one-based.  If the specified dimension
   is out of bounds then an a run-time error is raised.  */

static tree
lwb (NODE_T *p, tree boundable, tree dim)
{
  boundable = save_expr (boundable);
  dim = save_expr (dim);

  /* BOUNDABLE can be a multiple or an union whose all alternatives yield a
     multiple.  */
  tree zero_based_dim
    = save_expr (fold_build2 (MINUS_EXPR, TREE_TYPE (dim), dim, size_one_node));
  tree type = TREE_TYPE (boundable);
  if (A68_ROW_TYPE_P (type))
    {
      return fold_build2 (COMPOUND_EXPR, ssizetype,
			  a68_multiple_dim_check (p, boundable, dim),
			  a68_multiple_lower_bound (boundable, zero_based_dim));
    }
  else if (A68_ROWS_TYPE_P (type))
    {
      return fold_build2 (COMPOUND_EXPR, ssizetype,
			  a68_rows_dim_check (p, boundable, dim),
			  a68_rows_lower_bound (boundable, zero_based_dim));
    }
  else
    gcc_unreachable ();
}

tree
a68_lower_lwb2 (NODE_T *p, LOW_CTX_T ctx)
{
  tree multiple = a68_lower_tree (NEXT (SUB (p)), ctx);
  return fold_convert (CTYPE (MOID (p)), lwb (p, multiple, size_one_node));
}

tree
a68_lower_lwb3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree dim = fold_convert (sizetype, a68_lower_tree (SUB (p), ctx));
  tree multiple = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);
  return fold_convert (CTYPE (MOID (p)), lwb (p, multiple, dim));
}

/* ELEMS comes in two flavors.

   The unary operator returns the number of elements in the first dimension of
   the operand multple.

   DIM must be a size.

   The binary operator returns the number of elements in the given dimension of the
   operand multiple.  The dimension is one-based.  If the specified dimension
   is out of bounds then an a run-time error is raised.  */

static tree
elems (NODE_T *p, tree boundable, tree dim)
{
  dim = save_expr (dim);

  /* BOUNDABLE can be a multiple or a ROWS.  */
  tree type = TREE_TYPE (boundable);

  /* Make DIM zero-based.  */
  tree dim_minus_one
    = fold_build2 (MINUS_EXPR, TREE_TYPE (dim), dim, size_one_node);

  boundable = save_expr (boundable);
  tree upper_bound = NULL_TREE;
  tree lower_bound = NULL_TREE;
  tree check_dimension = NULL_TREE;
  if (A68_ROW_TYPE_P (type))
    {
      upper_bound = a68_multiple_upper_bound (boundable, dim_minus_one);
      lower_bound = a68_multiple_lower_bound (boundable, dim_minus_one);
      check_dimension = a68_multiple_dim_check (p, boundable, dim);
    }
  else if (A68_ROWS_TYPE_P (type))
    {
      upper_bound = a68_rows_upper_bound (boundable, dim_minus_one);
      lower_bound = a68_rows_lower_bound (boundable, dim_minus_one);
      check_dimension = a68_rows_dim_check (p, boundable, dim);
    }
  else
    gcc_unreachable ();

  upper_bound = save_expr (upper_bound);
  lower_bound = save_expr (lower_bound);

  tree non_flat = fold_build2 (PLUS_EXPR,
			       sizetype,
			       fold_convert (sizetype,
					     fold_build2 (MINUS_EXPR, ssizetype,
							  upper_bound, lower_bound)),
			       size_one_node);

  tree elems = fold_build3 (COND_EXPR, sizetype,
			    fold_build2 (LT_EXPR, boolean_type_node,
					 upper_bound, lower_bound),
			    size_zero_node,
			    non_flat);

  if (OPTION_BOUNDS_CHECKING (&A68_JOB))
    elems = fold_build2 (COMPOUND_EXPR, sizetype,
			 check_dimension,
			 elems);

  return elems;
}

tree
a68_lower_elems2 (NODE_T *p, LOW_CTX_T ctx)
{
  tree multiple = a68_lower_tree (NEXT (SUB (p)), ctx);
  return fold_convert (CTYPE (MOID (p)), elems (p, multiple, size_one_node));
}

tree
a68_lower_elems3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree dim = fold_convert (sizetype, a68_lower_tree (SUB (p), ctx));
  tree multiple = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);
  return fold_convert (CTYPE (MOID (p)), elems (p, multiple, dim));
}

tree
a68_lower_pow_int (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);
  return a68_int_pow (MOID (p), op1, op2, a68_get_node_location (p));
}

tree
a68_lower_pow_real (NODE_T *p, LOW_CTX_T ctx)
{
  MOID_T *mode = MOID (p);
  MOID_T *op1_mode = MOID (SUB (p));
  MOID_T *op2_mode = MOID (NEXT (NEXT (SUB (p))));
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);
  return a68_real_pow (mode, op1_mode, op2_mode,
		       op1, op2, a68_get_node_location (p));
}

tree
a68_lower_odd2 (NODE_T *p, LOW_CTX_T ctx)
{
  NODE_T *op = NEXT (SUB (p));

  return fold_build2_loc (a68_get_node_location (p),
			  EQ_EXPR,
			  a68_bool_type,
			  fold_build2 (BIT_AND_EXPR,
				       CTYPE (MOID (op)),
				       a68_lower_tree (op, ctx),
				       build_int_cst (CTYPE (MOID (op)), 1)),
			  build_int_cst (CTYPE (MOID (op)), 1));
}

tree
a68_lower_string_eq3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);

  tree cmp = a68_string_cmp (op1, op2);

  return fold_build2_loc (a68_get_node_location (p),
			  EQ_EXPR,
			  a68_bool_type,
			  cmp,
			  build_int_cst (a68_int_type, 0));
}

tree
a68_lower_string_ne3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);

  tree cmp = a68_string_cmp (op1, op2);

  return fold_build2_loc (a68_get_node_location (p),
			  NE_EXPR,
			  a68_bool_type,
			  cmp,
			  build_int_cst (a68_int_type, 0));
}

tree
a68_lower_string_lt3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);

  tree cmp = a68_string_cmp (op1, op2);

  return fold_build2_loc (a68_get_node_location (p),
			  LT_EXPR,
			  a68_bool_type,
			  cmp,
			  build_int_cst (a68_int_type, 0));
}

tree
a68_lower_string_le3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);

  tree cmp = a68_string_cmp (op1, op2);

  return fold_build2_loc (a68_get_node_location (p),
			  LE_EXPR,
			  a68_bool_type,
			  cmp,
			  build_int_cst (a68_int_type, 0));
}

tree
a68_lower_string_gt3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);

  tree cmp = a68_string_cmp (op1, op2);

  return fold_build2_loc (a68_get_node_location (p),
			  GT_EXPR,
			  a68_bool_type,
			  cmp,
			  build_int_cst (a68_int_type, 0));
}

tree
a68_lower_string_ge3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);

  tree cmp = a68_string_cmp (op1, op2);

  return fold_build2_loc (a68_get_node_location (p),
			  GE_EXPR,
			  a68_bool_type,
			  cmp,
			  build_int_cst (a68_int_type, 0));
}

tree
a68_lower_string_plus3 (NODE_T *p, LOW_CTX_T ctx)
{
  return a68_string_concat (a68_lower_tree (SUB (p), ctx),
			    a68_lower_tree (NEXT (NEXT (SUB (p))), ctx));
}

tree
a68_lower_string_plusab3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree lhs = a68_lower_tree (SUB (p), ctx);
  lhs = a68_consolidate_ref (MOID (SUB (p)), lhs);
  lhs = save_expr (lhs);
  NODE_T *rhs_node = NEXT (NEXT (SUB (p)));
  tree rhs = a68_lower_tree (rhs_node, ctx);
  tree operation = a68_string_concat (a68_low_deref (lhs, SUB (p)), rhs);

  return fold_build2_loc (a68_get_node_location (p),
			  COMPOUND_EXPR,
			  TREE_TYPE (lhs),
			  a68_low_assignation (p, lhs, MOID (SUB (p)), operation, MOID (rhs_node)),
			  lhs);
}

tree
a68_lower_string_plusto3 (NODE_T *p, LOW_CTX_T ctx)
{
  NODE_T *lhs_node = NEXT (NEXT (SUB (p)));
  tree lhs = a68_lower_tree (lhs_node, ctx);
  lhs = a68_consolidate_ref (MOID (lhs_node), lhs);
  lhs = save_expr (lhs);
  MOID_T *lhs_mode = MOID (lhs_node);
  NODE_T *rhs_node = SUB (p);
  tree rhs = a68_lower_tree (rhs_node, ctx);
  tree operation = a68_string_concat (rhs, a68_low_deref (lhs, NEXT (NEXT (SUB (p)))));

  return fold_build2_loc (a68_get_node_location (p),
			  COMPOUND_EXPR,
			  TREE_TYPE (lhs),
			  a68_low_assignation (p, lhs, lhs_mode,
					       operation, MOID (rhs_node)),
			  lhs);
}

tree
a68_lower_repr2 (NODE_T *p, LOW_CTX_T ctx)
{
  NODE_T *op = NEXT (SUB (p));
  return a68_char_repr (op, a68_lower_tree (op, ctx));
}

tree
a68_lower_char_plus3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);

  return a68_string_concat (a68_string_from_char (op1),
			    a68_string_from_char (op2));
}

tree
a68_lower_char_mult3 (NODE_T *p, LOW_CTX_T ctx)
{
  NODE_T *n1 = SUB (p);
  NODE_T *n2 = NEXT (NEXT (SUB (p)));

  if (MOID (n1) == M_INT)
    {
      gcc_assert (MOID (n2) == M_CHAR);
      return a68_string_mult (a68_string_from_char (a68_lower_tree (n2, ctx)),
			      a68_lower_tree (n1, ctx));
    }
  else
    {
      gcc_assert (MOID (n1) == M_CHAR);
      gcc_assert (MOID (n2) == M_INT);
      return a68_string_mult (a68_string_from_char (a68_lower_tree (n1, ctx)),
			      a68_lower_tree (n2, ctx));
    }
}

tree
a68_lower_string_mult3 (NODE_T *p, LOW_CTX_T ctx)
{
  NODE_T *n1 = SUB (p);
  NODE_T *n2 = NEXT (NEXT (SUB (p)));

  if (MOID (n1) == M_INT)
    {
      gcc_assert (MOID (n2) == M_STRING || MOID (n2) == M_ROW_CHAR);
      return a68_string_mult (a68_lower_tree (n2, ctx),
			      a68_lower_tree (n1, ctx));
    }
  else
    {
      gcc_assert (MOID (n1) == M_STRING || MOID (n1) == M_ROW_CHAR);
      gcc_assert (MOID (n2) == M_INT);
      return a68_string_mult (a68_lower_tree (n1, ctx),
			      a68_lower_tree (n2, ctx));
    }
}

tree
a68_lower_string_multab3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree lhs = a68_lower_tree (SUB (p), ctx);
  lhs = a68_consolidate_ref (MOID (SUB (p)), lhs);
  lhs = save_expr (lhs);
  NODE_T *rhs_node = NEXT (NEXT (SUB (p)));
  tree rhs = a68_lower_tree (rhs_node, ctx);
  tree operation = a68_string_mult (a68_low_deref (lhs, SUB (p)), rhs);

  return fold_build2_loc (a68_get_node_location (p),
			  COMPOUND_EXPR,
			  TREE_TYPE (lhs),
			  a68_low_assignation (p, lhs, MOID (SUB (p)), operation, MOID (rhs_node)),
			  lhs);
}

/* SIZETY BITS operators.  */

tree
a68_lower_bin2 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op = a68_lower_tree (NEXT (SUB (p)), ctx);
  return a68_bits_bin (MOID (p), op);
}

tree
a68_lower_bitabs2 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op = a68_lower_tree (NEXT (SUB (p)), ctx);
  return a68_bits_abs (MOID (p), op);
}

tree
a68_lower_bitleng2 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op = a68_lower_tree (NEXT (SUB (p)), ctx);
  return a68_bits_leng (CTYPE (MOID (p)), op);
}

tree
a68_lower_bitshorten2 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op = a68_lower_tree (NEXT (SUB (p)), ctx);
  return a68_bits_shorten (CTYPE (MOID (p)), op);
}

tree
a68_lower_bitnot2 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op = a68_lower_tree (NEXT (SUB (p)), ctx);
  return a68_bits_not (op);
}

tree
a68_lower_bitand3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);
  return a68_bits_and (op1, op2);
}

tree
a68_lower_bitior3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);
  return a68_bits_ior (op1, op2);
}

tree
a68_lower_bitxor3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);
  return a68_bits_xor (op1, op2);
}

tree
a68_lower_shl3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree bits = a68_lower_tree (SUB (p), ctx);
  tree shift = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);
  return a68_bits_shift (shift, bits);
}

tree
a68_lower_shr3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree bits = a68_lower_tree (SUB (p), ctx);
  tree shift = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);
  return a68_bits_shift (fold_build1 (NEGATE_EXPR,
				      TREE_TYPE (shift), shift),
			 bits);
}

tree
a68_lower_bitelem3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree pos = a68_lower_tree (SUB (p), ctx);
  tree bits = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);
  return a68_bits_elem (p, pos, bits);
}

tree
a68_lower_bit_eq3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);
  return a68_bits_eq (op1, op2);
}

tree
a68_lower_bit_ne3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);
  return a68_bits_ne (op1, op2);
}

tree
a68_lower_bit_le3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);
  return a68_bits_subset (op1, op2);
}

tree
a68_lower_bit_ge3 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);
  return a68_bits_subset (op2, op1);
}

/* Environment enquiries.  */

tree
a68_lower_maxint (NODE_T *p, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  return a68_int_maxval (CTYPE (MOID (p)));
}

tree
a68_lower_minint (NODE_T *p, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  return a68_int_minval (CTYPE (MOID (p)));
}

tree
a68_lower_maxbits (NODE_T *p, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  return a68_bits_maxbits (CTYPE (MOID (p)));
}

tree
a68_lower_maxreal (NODE_T *p ATTRIBUTE_UNUSED, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  return a68_real_maxval (CTYPE (MOID (p)));
}

tree
a68_lower_minreal (NODE_T *p ATTRIBUTE_UNUSED, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  return a68_real_minval (CTYPE (MOID (p)));
}

tree
a68_lower_smallreal (NODE_T *p, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  return a68_real_smallval (CTYPE (MOID (p)));
}

tree
a68_lower_bitswidth (NODE_T *p ATTRIBUTE_UNUSED, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  return a68_bits_width (a68_bits_type);
}

tree
a68_lower_longbitswidth (NODE_T *p ATTRIBUTE_UNUSED, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  return a68_bits_width (a68_long_bits_type);
}

tree
a68_lower_longlongbitswidth (NODE_T *p ATTRIBUTE_UNUSED, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  return a68_bits_width (a68_long_long_bits_type);
}

tree
a68_lower_shortbitswidth (NODE_T *p ATTRIBUTE_UNUSED, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  return a68_bits_width (a68_short_bits_type);
}

tree
a68_lower_shortshortbitswidth (NODE_T *p ATTRIBUTE_UNUSED, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  return a68_bits_width (a68_short_short_bits_type);
}

tree
a68_lower_intwidth (NODE_T *p ATTRIBUTE_UNUSED, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  return a68_int_width (a68_int_type);
}

tree
a68_lower_longintwidth (NODE_T *p ATTRIBUTE_UNUSED, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  return a68_int_width (a68_long_int_type);
}

tree
a68_lower_longlongintwidth (NODE_T *p ATTRIBUTE_UNUSED, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  return a68_int_width (a68_long_long_int_type);
}

tree
a68_lower_shortintwidth (NODE_T *p ATTRIBUTE_UNUSED, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  return a68_int_width (a68_short_int_type);
}

tree
a68_lower_shortshortintwidth (NODE_T *p ATTRIBUTE_UNUSED, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  return a68_int_width (a68_short_short_int_type);
}

tree
a68_lower_realwidth (NODE_T *p ATTRIBUTE_UNUSED, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  return a68_real_width (a68_real_type);
}

tree
a68_lower_longrealwidth (NODE_T *p ATTRIBUTE_UNUSED, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  return a68_real_width (a68_long_real_type);
}

tree
a68_lower_longlongrealwidth (NODE_T *p ATTRIBUTE_UNUSED, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  return a68_real_width (a68_long_long_real_type);
}

tree
a68_lower_expwidth (NODE_T *p ATTRIBUTE_UNUSED, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  return a68_real_exp_width (a68_real_type);
}

tree
a68_lower_longexpwidth (NODE_T *p ATTRIBUTE_UNUSED, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  return a68_real_exp_width (a68_long_real_type);
}

tree
a68_lower_longlongexpwidth (NODE_T *p ATTRIBUTE_UNUSED, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  return a68_real_exp_width (a68_long_long_real_type);
}

tree
a68_lower_pi (NODE_T *p, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  return a68_real_pi (CTYPE (MOID (p)));
}

tree
a68_lower_nullcharacter (NODE_T *p ATTRIBUTE_UNUSED, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  static tree null_character = NULL_TREE;

  if (null_character == NULL_TREE)
    null_character = build_int_cst (a68_char_type, 0);
  return null_character;
}

tree
a68_lower_flip (NODE_T *p ATTRIBUTE_UNUSED,
		LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  static tree flip = NULL_TREE;

  if (flip == NULL_TREE)
    flip = build_int_cst (a68_char_type, 84); /* T */
  return flip;
}

tree
a68_lower_eofchar (NODE_T *p ATTRIBUTE_UNUSED,
		   LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  static tree eofchar = NULL_TREE;

  if (eofchar == NULL_TREE)
    eofchar = build_int_cst (a68_char_type, -1);
  return eofchar;
}

tree
a68_lower_replacementchar (NODE_T *p ATTRIBUTE_UNUSED,
		       LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  static tree replacementchar = NULL_TREE;

  if (replacementchar == NULL_TREE)
    replacementchar = build_int_cst (a68_char_type, 0xfffd);
  return replacementchar;
}

tree
a68_lower_flop (NODE_T *p ATTRIBUTE_UNUSED,
		LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  static tree flop = NULL_TREE;

  if (flop == NULL_TREE)
    flop = build_int_cst (a68_char_type, 70); /* F */
  return flop;
}

tree
a68_lower_errorchar (NODE_T *p ATTRIBUTE_UNUSED, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  static tree errorchar = NULL_TREE;

  if (errorchar == NULL_TREE)
    errorchar = build_int_cst (a68_char_type, 42); /* * */
  return errorchar;
}

tree
a68_lower_blank (NODE_T *p ATTRIBUTE_UNUSED, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  static tree blank = NULL_TREE;

  if (blank == NULL_TREE)
    blank = build_int_cst (a68_char_type, 32);
  return blank;
}

tree
a68_lower_intlengths (NODE_T *p, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  HOST_WIDE_INT int_size = int_size_in_bytes (a68_int_type);
  HOST_WIDE_INT long_int_size = int_size_in_bytes (a68_long_int_type);
  HOST_WIDE_INT long_long_int_size = int_size_in_bytes (a68_long_long_int_type);

  gcc_assert (int_size != -1);
  gcc_assert (long_int_size != -1);
  gcc_assert (long_long_int_size != -1);

  int lengths = 1;
  if (long_long_int_size != long_int_size)
    lengths++;
  if (long_int_size != int_size)
    lengths++;

  return build_int_cst (CTYPE (MOID (p)), lengths);
}

tree
a68_lower_intshorths (NODE_T *p, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  HOST_WIDE_INT int_size = int_size_in_bytes (a68_int_type);
  HOST_WIDE_INT short_int_size = int_size_in_bytes (a68_short_int_type);
  HOST_WIDE_INT short_short_int_size = int_size_in_bytes (a68_short_short_int_type);

  gcc_assert (int_size != -1);
  gcc_assert (short_int_size != -1);
  gcc_assert (short_short_int_size != -1);

  int shorths = 1;
  if (short_short_int_size != short_int_size)
    shorths++;
  if (short_int_size != int_size)
    shorths++;

  return build_int_cst (CTYPE (MOID (p)), shorths);
}

tree
a68_lower_bitslengths (NODE_T *p, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  HOST_WIDE_INT bits_size = int_size_in_bytes (a68_bits_type);
  HOST_WIDE_INT long_bits_size = int_size_in_bytes (a68_long_bits_type);
  HOST_WIDE_INT long_long_bits_size = int_size_in_bytes (a68_long_long_bits_type);

  gcc_assert (bits_size != -1);
  gcc_assert (long_bits_size != -1);
  gcc_assert (long_long_bits_size != -1);

  int lengths = 1;
  if (long_long_bits_size != long_bits_size)
    lengths++;
  if (long_bits_size != bits_size)
    lengths++;

  return build_int_cst (CTYPE (MOID (p)), lengths);
}

tree
a68_lower_bitsshorths (NODE_T *p, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  HOST_WIDE_INT bits_size = int_size_in_bytes (a68_bits_type);
  HOST_WIDE_INT short_bits_size = int_size_in_bytes (a68_short_bits_type);
  HOST_WIDE_INT short_short_bits_size = int_size_in_bytes (a68_short_short_bits_type);

  gcc_assert (bits_size != -1);
  gcc_assert (short_bits_size != -1);
  gcc_assert (short_short_bits_size != -1);

  int shorths = 1;
  if (short_short_bits_size != short_bits_size)
    shorths++;
  if (short_bits_size != bits_size)
    shorths++;

  return build_int_cst (CTYPE (MOID (p)), shorths);
}

tree
a68_lower_reallengths (NODE_T *p, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  HOST_WIDE_INT real_size = int_size_in_bytes (a68_real_type);
  HOST_WIDE_INT long_real_size = int_size_in_bytes (a68_long_real_type);
  HOST_WIDE_INT long_long_real_size = int_size_in_bytes (a68_long_long_real_type);

  gcc_assert (real_size != -1);
  gcc_assert (long_real_size != -1);
  gcc_assert (long_long_real_size != -1);

  int lengths = 1;
  if (long_long_real_size != long_real_size)
    lengths++;
  if (long_real_size != real_size)
    lengths++;

  return build_int_cst (CTYPE (MOID (p)), lengths);
}

tree
a68_lower_realshorths (NODE_T *p, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  return build_int_cst (CTYPE (MOID (p)), 1);
}

tree
a68_lower_infinity (NODE_T *p, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  return build_real (CTYPE (MOID (p)), dconstinf);
}

tree
a68_lower_minusinfinity (NODE_T *p, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  return build_real (CTYPE (MOID (p)), dconstninf);
}

tree
a68_lower_maxabschar (NODE_T *p ATTRIBUTE_UNUSED,
		      LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  return a68_char_max ();
}

tree
a68_lower_sqrt (NODE_T *p ATTRIBUTE_UNUSED,
		LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_real_sqrt (a68_real_type);
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_long_sqrt (NODE_T *p ATTRIBUTE_UNUSED,
		     LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_real_sqrt (a68_long_real_type);
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_long_long_sqrt (NODE_T *p ATTRIBUTE_UNUSED,
			  LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_real_sqrt (a68_long_long_real_type);
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_tan (NODE_T *p ATTRIBUTE_UNUSED,
	       LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_real_tan (a68_real_type);
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_long_tan (NODE_T *p ATTRIBUTE_UNUSED,
		    LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_real_tan (a68_long_real_type);
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_long_long_tan (NODE_T *p ATTRIBUTE_UNUSED,
			 LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_real_tan (a68_long_long_real_type);
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_sin (NODE_T *p ATTRIBUTE_UNUSED,
	       LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_real_sin (a68_real_type);
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_long_sin (NODE_T *p ATTRIBUTE_UNUSED,
		    LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_real_sin (a68_long_real_type);
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_long_long_sin (NODE_T *p ATTRIBUTE_UNUSED,
			 LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_real_sin (a68_long_long_real_type);
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_cos (NODE_T *p ATTRIBUTE_UNUSED,
	       LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_real_cos (a68_real_type);
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_long_cos (NODE_T *p ATTRIBUTE_UNUSED,
		    LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_real_cos (a68_long_real_type);
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_long_long_cos (NODE_T *p ATTRIBUTE_UNUSED,
			 LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_real_cos (a68_long_long_real_type);
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_acos (NODE_T *p ATTRIBUTE_UNUSED,
		LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_real_acos (a68_real_type);
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_long_acos (NODE_T *p ATTRIBUTE_UNUSED,
		     LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_real_acos (a68_long_real_type);
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_long_long_acos (NODE_T *p ATTRIBUTE_UNUSED,
			  LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_real_acos (a68_long_long_real_type);
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_asin (NODE_T *p ATTRIBUTE_UNUSED,
		LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_real_asin (a68_real_type);
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_long_asin (NODE_T *p ATTRIBUTE_UNUSED,
		     LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_real_asin (a68_long_real_type);
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_long_long_asin (NODE_T *p ATTRIBUTE_UNUSED,
			  LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_real_asin (a68_long_long_real_type);
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_atan (NODE_T *p ATTRIBUTE_UNUSED,
		LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_real_atan (a68_real_type);
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_long_atan (NODE_T *p ATTRIBUTE_UNUSED,
		     LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_real_atan (a68_long_real_type);
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_long_long_atan (NODE_T *p ATTRIBUTE_UNUSED,
			  LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_real_atan (a68_long_long_real_type);
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_ln (NODE_T *p ATTRIBUTE_UNUSED,
	      LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_real_ln (a68_real_type);
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_long_ln (NODE_T *p ATTRIBUTE_UNUSED,
		   LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_real_ln (a68_long_real_type);
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_long_long_ln (NODE_T *p ATTRIBUTE_UNUSED,
			LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_real_ln (a68_long_long_real_type);
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_log (NODE_T *p ATTRIBUTE_UNUSED,
	       LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_real_log (a68_real_type);
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_long_log (NODE_T *p ATTRIBUTE_UNUSED,
		    LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_real_log (a68_long_real_type);
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_long_long_log (NODE_T *p ATTRIBUTE_UNUSED,
			 LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_real_log (a68_long_long_real_type);
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_exp (NODE_T *p ATTRIBUTE_UNUSED,
	       LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_real_exp (a68_real_type);
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_long_exp (NODE_T *p ATTRIBUTE_UNUSED,
		    LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_real_exp (a68_long_real_type);
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_long_long_exp (NODE_T *p ATTRIBUTE_UNUSED,
			 LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_real_exp (a68_long_long_real_type);
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_reali (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);
  tree t = a68_complex_i (M_COMPLEX, op1, op2);
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_longreali (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);
  tree t = a68_complex_i (M_LONG_COMPLEX, op1, op2);
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_longlongreali (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);
  tree t = a68_complex_i (M_LONG_LONG_COMPLEX, op1, op2);
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_inti (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);
  tree t = a68_complex_i (M_COMPLEX,
			  convert_to_real (a68_real_type, op1),
			  convert_to_real (a68_real_type, op2));
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_longinti (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);
  tree t= a68_complex_i (M_LONG_COMPLEX,
			 convert_to_real (a68_long_real_type, op1),
			 convert_to_real (a68_long_real_type, op2));
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_longlonginti (NODE_T *p, LOW_CTX_T ctx)
{
  tree op1 = a68_lower_tree (SUB (p), ctx);
  tree op2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);
  tree t = a68_complex_i (M_LONG_LONG_COMPLEX,
			  convert_to_real (a68_long_long_real_type, op1),
			  convert_to_real (a68_long_long_real_type, op2));
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_re2 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op = a68_lower_tree (NEXT (SUB (p)), ctx);
  tree t = a68_complex_re (op);
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_im2 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op = a68_lower_tree (NEXT (SUB (p)), ctx);
  tree t = a68_complex_im (op);
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_conj2 (NODE_T *p, LOW_CTX_T ctx)
{
  tree op = a68_lower_tree (NEXT (SUB (p)), ctx);
  return a68_complex_conj (MOID (p), op);
}

tree
a68_lower_shortenint2 (NODE_T *p, LOW_CTX_T ctx)
{
  return a68_int_shorten (MOID (p), MOID (NEXT (SUB (p))),
			  a68_lower_tree (NEXT (SUB (p)), ctx));
}

tree
a68_lower_lengint2 (NODE_T *p, LOW_CTX_T ctx)
{
  return a68_int_leng (MOID (p), MOID (NEXT (SUB (p))),
		       a68_lower_tree (NEXT (SUB (p)), ctx));
}

tree
a68_lower_shortenreal2 (NODE_T *p, LOW_CTX_T ctx)
{
  return a68_real_shorten (MOID (p), MOID (NEXT (SUB (p))),
			   a68_lower_tree (NEXT (SUB (p)), ctx));
}

tree
a68_lower_lengreal2 (NODE_T *p, LOW_CTX_T ctx)
{
  return a68_real_leng (MOID (p), MOID (NEXT (SUB (p))),
			a68_lower_tree (NEXT (SUB (p)), ctx));
}

tree
a68_lower_random (NODE_T *p ATTRIBUTE_UNUSED, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  return a68_get_libcall (A68_LIBCALL_RANDOM);
}

tree
a68_lower_longrandom (NODE_T *p ATTRIBUTE_UNUSED, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  /* Note we dont build a call because this will get deprocedured in case it is
     actually called.  */
  return a68_get_libcall (A68_LIBCALL_LONGRANDOM);
}

tree
a68_lower_longlongrandom (NODE_T *p ATTRIBUTE_UNUSED, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  return a68_get_libcall (A68_LIBCALL_LONGLONGRANDOM);
}

/********* POSIX prelude.  ***************/

tree
a68_lower_posixargc (NODE_T *p ATTRIBUTE_UNUSED, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_posix_argc ();
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_posixargv (NODE_T *p ATTRIBUTE_UNUSED,
		     LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_posix_argv ();
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_posixgetenv (NODE_T *p ATTRIBUTE_UNUSED,
		       LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_posix_getenv ();
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_posixputchar (NODE_T *p ATTRIBUTE_UNUSED,
			LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_posix_putchar ();
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_posixputs (NODE_T *p ATTRIBUTE_UNUSED,
		     LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_posix_puts ();
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_posixfconnect (NODE_T *p ATTRIBUTE_UNUSED,
			 LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_posix_fconnect ();
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_posixfopen (NODE_T *p ATTRIBUTE_UNUSED,
		      LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_posix_fopen ();
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_posixfcreate (NODE_T *p ATTRIBUTE_UNUSED,
			LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_posix_fcreate ();
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_posixfclose (NODE_T *p ATTRIBUTE_UNUSED,
		       LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_posix_fclose ();
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_posixfsize (NODE_T *p ATTRIBUTE_UNUSED,
		      LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_posix_fsize ();
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_posixlseek (NODE_T *p ATTRIBUTE_UNUSED,
		      LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_posix_lseek ();
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_posixseekcur (NODE_T *p ATTRIBUTE_UNUSED,
			LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  return build_int_cst (a68_int_type, 0);
}

tree
a68_lower_posixseekend (NODE_T *p ATTRIBUTE_UNUSED,
			LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  return build_int_cst (a68_int_type, 1);
}

tree
a68_lower_posixseekset (NODE_T *p ATTRIBUTE_UNUSED,
			LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  return build_int_cst (a68_int_type, 2);
}

tree
a68_lower_posixstdinfiledes (NODE_T *p ATTRIBUTE_UNUSED,
			     LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  return build_int_cst (a68_int_type, 0);
}

tree
a68_lower_posixstdoutfiledes (NODE_T *p ATTRIBUTE_UNUSED,
			      LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  return build_int_cst (a68_int_type, 1);
}

tree
a68_lower_posixstderrfiledes (NODE_T *p ATTRIBUTE_UNUSED,
			      LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  return build_int_cst (a68_int_type, 2);
}

tree
a68_lower_posixfileodefault (NODE_T *p ATTRIBUTE_UNUSED,
			     LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  /* Please keep in sync with libga68/ga68-posix.c  */
  return build_int_cst (a68_bits_type, 0x99999999);
}

tree
a68_lower_posixfileordwr (NODE_T *p ATTRIBUTE_UNUSED,
			  LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  /* Please keep in sync with libga68/ga68-posix.c  */
  return build_int_cst (a68_bits_type, 0x2);
}

tree
a68_lower_posixfileordonly (NODE_T *p ATTRIBUTE_UNUSED,
			    LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  /* Please keep in sync with libga68/ga68-posix.c  */
  return build_int_cst (a68_bits_type, 0x0);
}

tree
a68_lower_posixfileowronly (NODE_T *p ATTRIBUTE_UNUSED,
			    LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  /* Please keep in sync with libga68/ga68-posix.c  */
  return build_int_cst (a68_bits_type, 0x1);
}

tree
a68_lower_posixfileotrunc (NODE_T *p ATTRIBUTE_UNUSED,
			   LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  /* Please keep in sync with libga68/ga68-posix.c  */
  return build_int_cst (a68_bits_type, 0x8);
}

tree
a68_lower_posixerrno (NODE_T *p ATTRIBUTE_UNUSED,
		      LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_posix_errno ();
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_posixexit (NODE_T *p ATTRIBUTE_UNUSED,
		     LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_posix_exit ();
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_posixperror (NODE_T *p ATTRIBUTE_UNUSED,
		       LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_posix_perror ();
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_posixstrerror (NODE_T *p ATTRIBUTE_UNUSED,
			 LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_posix_strerror ();
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_posixfputc (NODE_T *p ATTRIBUTE_UNUSED,
		      LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_posix_fputc ();
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_posixfputs (NODE_T *p ATTRIBUTE_UNUSED,
		      LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_posix_fputs ();
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_posixgetchar (NODE_T *p ATTRIBUTE_UNUSED,
			LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_posix_getchar ();
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}


tree
a68_lower_posixfgetc (NODE_T *p ATTRIBUTE_UNUSED,
		      LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_posix_fgetc ();
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_posixgets (NODE_T *p ATTRIBUTE_UNUSED,
		     LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_posix_gets ();
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}

tree
a68_lower_posixfgets (NODE_T *p, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree t = a68_posix_fgets ();
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, a68_get_node_location (p));
  return t;
}
