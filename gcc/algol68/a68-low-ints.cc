/* Lowering routines for all things related to INT values.
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
#include "dumpfile.h"
#include "convert.h"

#include "a68.h"

/* Return a tree with the yielind of SKIP for the given integral mode.  */

tree
a68_get_int_skip_tree (MOID_T *m)
{
  tree type;

  if (m == M_INT)
    type = a68_int_type;
  else if (m == M_LONG_INT)
    type = a68_long_int_type;
  else if (m == M_LONG_LONG_INT)
    type = a68_long_long_int_type;
  else if (m == M_SHORT_INT)
    type = a68_short_int_type;
  else if (m == M_SHORT_SHORT_INT)
    type = a68_short_short_int_type;
  else
    gcc_unreachable ();

  return build_int_cst (type, 0);
}

/* Given an integral type, build the maximum value expressable in that
   type.  */

tree
a68_int_maxval (tree type)
{
  return fold_convert (type, TYPE_MAX_VALUE (type));
}

/* Given an integral type, build the minimum value expressable in that
   type.  */

tree
a68_int_minval (tree type)
{
  return fold_convert (type, TYPE_MIN_VALUE (type));
}

/* Given an integral type, build an INT with the number of decimal digits
   required to represent a value of that typ, not including sign.  */

tree
a68_int_width (tree type)
{
  /* Note that log10 (2) is ~ 0.3.
     Thanks to Andrew Pinski for suggesting using this expression.  */
  return fold_build2 (PLUS_EXPR, a68_int_type,
		      build_int_cst (a68_int_type, 1),
		      fold_build2 (TRUNC_DIV_EXPR,
				   a68_int_type,
				   fold_build2 (MULT_EXPR, a68_int_type,
						build_int_cst (a68_int_type, TYPE_PRECISION (type)),
						build_int_cst (a68_int_type, 3)),
				   build_int_cst (a68_int_type, 10)));
}

/* Given an integer value VAL, return -1 if it is less than zero, 0 if it is
   zero and +1 if it is bigger than zero.  The built value is always of mode
   M_INT.  */

tree
a68_int_sign (tree val)
{
  tree zero = build_int_cst (TREE_TYPE (val), 0);
  val = save_expr (val);
  return fold_build3 (COND_EXPR,
		      a68_int_type,
		      fold_build2 (EQ_EXPR, integer_type_node, val, zero),
		      build_int_cst (a68_int_type, 0),
		      fold_build3 (COND_EXPR,
				   a68_int_type,
				   fold_build2 (GT_EXPR, integer_type_node, val, zero),
				   build_int_cst (a68_int_type, 1),
				   build_int_cst (a68_int_type, -1)));
}

/* Absolute value of an integer.  */

tree
a68_int_abs (tree val)
{
  return fold_build1 (ABS_EXPR, TREE_TYPE (val), val);
}

/* Build the integral value lengthened from the value of VAL, from mode
   FROM_MODE to mode TO_MODE.  */

tree
a68_int_leng (MOID_T *to_mode, MOID_T *from_mode ATTRIBUTE_UNUSED, tree val)
{
  /* Lengthening can be done by just a cast.  */
  return fold_convert (CTYPE (to_mode), val);
}

/* Build the integral value that can be lengthened to the value of VAL, from
   mode FROM_MODE to mode TO_MODE.

   If VAL cannot be represented in TO_MODE because it is bigger than the most
   positive value representable in TO_MODE, then it is truncated to that value.

   Likewise, if VAL cannot be represented in TO_MODE because it is less than
   the most negative value representable in TO_MODE, then it is truncated to
   that value.  */

tree
a68_int_shorten (MOID_T *to_mode, MOID_T *from_mode ATTRIBUTE_UNUSED, tree val)
{
  tree most_positive_value = fold_convert (CTYPE (from_mode),
					   a68_int_maxval (CTYPE (to_mode)));
  tree most_negative_value = fold_convert (CTYPE (from_mode),
					   a68_int_minval (CTYPE (to_mode)));

  val = save_expr (val);
  most_positive_value = save_expr (most_positive_value);
  most_negative_value = save_expr (most_negative_value);
  return fold_build3 (COND_EXPR, CTYPE (to_mode),
		      fold_build2 (GT_EXPR, a68_bool_type, val, most_positive_value),
		      fold_convert (CTYPE (to_mode), most_positive_value),
		      fold_build3 (COND_EXPR, CTYPE (to_mode),
				   fold_build2 (LT_EXPR, a68_bool_type, val, most_negative_value),
				   fold_convert (CTYPE (to_mode), most_negative_value),
				   fold_convert (CTYPE (to_mode), val)));
}

/* Given two integral values of mode M, build an expression that calculates the
   addition of A and B.  */

tree
a68_int_plus (MOID_T *m, tree a, tree b, location_t loc)
{
  return fold_build2_loc (loc, PLUS_EXPR, CTYPE (m), a, b);
}

/* Given two integral values of mode M, build an expression that calculates the
   subtraction of A by B.  */

tree
a68_int_minus (MOID_T *m, tree a, tree b, location_t loc)
{
  return fold_build2_loc (loc, MINUS_EXPR, CTYPE (m), a, b);
}

/* Given two integral values of mode M, build an expression that calculates the
   multiplication of A by B.  */

tree
a68_int_mult (MOID_T *m, tree a, tree b, location_t loc)
{
  return fold_build2_loc (loc, MULT_EXPR, CTYPE (m), a, b);
}

/* Given two integral values of mode M, build an expression that calculates the
   division of A by B.  */

tree
a68_int_div (MOID_T *m, tree a, tree b, location_t loc)
{
  return fold_build2_loc (loc, TRUNC_DIV_EXPR, CTYPE (m), a, b);
}

/* Given two integral values of mode M, build an expression that calculates
   whether A = B.  */

tree
a68_int_eq (tree a, tree b, location_t loc)
{
  return fold_build2_loc (loc, EQ_EXPR, boolean_type_node, a, b);
}

/* Given two integral values of mode M, build an expression that calculates
   whether A /= B.  */

tree
a68_int_ne (tree a, tree b, location_t loc)
{
  return fold_build2_loc (loc, NE_EXPR, boolean_type_node, a, b);
}

/* Given two integral values of mode M, build an expression that calculates
   whether A < B.  */

tree
a68_int_lt (tree a, tree b, location_t loc)
{
  return fold_build2_loc (loc, LT_EXPR, boolean_type_node, a, b);
}

/* Given two integral values of mode M, build an expression that calculates
   whether A <= B.  */

tree
a68_int_le (tree a, tree b, location_t loc)
{
  return fold_build2_loc (loc, LE_EXPR, boolean_type_node, a, b);
}

/* Given two integral values of mode M, build an expression that calculates
   whether A > B.  */

tree
a68_int_gt (tree a, tree b, location_t loc)
{
  return fold_build2_loc (loc, GT_EXPR, boolean_type_node, a, b);
}

/* Given two integral values of mode M, build an expression that calculates
   whether A >= B.  */

tree
a68_int_ge (tree a, tree b, location_t loc)
{
  return fold_build2_loc (loc, GE_EXPR, boolean_type_node, a, b);
}

/* Given two integral values of mode M, build and expression that calculates the
   modulus as specified by the Revised Report:

   OP MOD = (L INT a, b) L INT:
     (INT r = a - a % b * b; r < 0 | r + ABS b | r)
*/

tree
a68_int_mod (MOID_T *m, tree a, tree b, location_t loc)
{
  a = save_expr (a);
  b = save_expr (b);
  tree r = a68_int_minus (m, a, a68_int_mult (m, a68_int_div (m, a, b), b));

  r = save_expr (r);
  return fold_build3_loc (loc, COND_EXPR, CTYPE (m),
			  a68_int_lt (r, build_int_cst (CTYPE (m), 0)),
			  a68_int_plus (m, r, a68_int_abs (b)),
			  r);
}

/* Given two integral values values, the first of mode M an the second of mode
   INT, build an expression that calculates the exponentiation of A by B, as
   specified by the Revised Report:

   OP ** = (L INT a, INT b) L INT:
     (b >= 0 | L INT p := L 1; TO b DO p := p * a OD; p)
*/

tree
a68_int_pow (MOID_T *m, tree a, tree b, location_t loc)
{
  tree zero = build_int_cst (CTYPE (m), 0);
  tree one = build_int_cst (CTYPE (m), 1);

  a = save_expr (a);
  b = save_expr (fold_convert (CTYPE (m), b));

  a68_push_range (m);
  tree index = a68_lower_tmpvar ("index%", CTYPE (m), zero);
  tree p = a68_lower_tmpvar ("p%", CTYPE (m), one);

  /* Begin of loop body.  */
  a68_push_range (NULL);
  {
    /* if (index == b) break;  */
    a68_add_stmt (fold_build1 (EXIT_EXPR,
			       void_type_node,
			       fold_build2 (EQ_EXPR, CTYPE (m),
					    index, b)));
    a68_add_stmt (fold_build2 (MODIFY_EXPR, CTYPE (m),
			       p, a68_int_mult (m, p, a)));

    /* index++ */
    a68_add_stmt (fold_build2 (POSTINCREMENT_EXPR, CTYPE (m),
			       index, one));
  }
  tree loop_body = a68_pop_range ();
  a68_add_stmt (fold_build1 (LOOP_EXPR,
			     void_type_node,
			     loop_body));
  a68_add_stmt (p);
  tree calculate_p = a68_pop_range ();
  return fold_build3_loc (loc, COND_EXPR, CTYPE (m),
			  a68_int_ge (b, zero),
			  calculate_p, zero);
}
