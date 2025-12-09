/* Lowering routines for all things related to REAL values.
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

#include "math.h" /* For log10 */

#include "a68.h"

tree
a68_get_real_skip_tree (MOID_T *m)
{
  tree int_type = NULL_TREE;
  tree real_type = NULL_TREE;

  if (m == M_REAL)
    {
      int_type = a68_int_type;
      real_type = a68_real_type;
    }
  else if (m == M_LONG_REAL)
    {
      int_type = a68_long_int_type;
      real_type = a68_long_real_type;
    }
  else if (m == M_LONG_LONG_REAL)
    {
      int_type = a68_long_long_int_type;
      real_type = a68_long_long_real_type;
    }
  else
    gcc_unreachable ();

  return build_real_from_int_cst (real_type,
				  build_int_cst (int_type, 0));
}

static tree
addr_of_builtin_decl (enum built_in_function fncode)
{
  tree builtin = builtin_decl_explicit (fncode);
  return fold_build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (builtin)), builtin);
}

/* Build PI for the given real type.  */

tree
a68_real_pi (tree type)
{
  return build_real (type, dconst_pi ());
}

/* Given a real type, build the maximum value expresssable with that type.  */

tree
a68_real_maxval (tree type)
{
  REAL_VALUE_TYPE max;
  real_maxval (&max, 0, TYPE_MODE (type));
  return build_real (type, max);
}

/* Given a real type, build the minimum value expressable with that type.  */

tree
a68_real_minval (tree type)
{
  REAL_VALUE_TYPE min;
  real_maxval (&min, 1, TYPE_MODE (type));
  return build_real (type, min);
}

/* Given a real type, build the smallest value which can be meaningfully added
   to or substracted from 1.  */

tree
a68_real_smallval (tree type)
{
  /* The smallest real value which can be meaningfully added to or subtracted
     from 1.  */
  const machine_mode mode = TYPE_MODE (type);
  const struct real_format *fmt = REAL_MODE_FORMAT (mode);

  char buf[128];
  if (fmt->pnan < fmt->p)
    snprintf (buf, sizeof (buf), "0x1p%d", fmt->emin - fmt->p);
  else
    snprintf (buf, sizeof (buf), "0x1p%d", 1 - fmt->p);

  REAL_VALUE_TYPE res;
  real_from_string (&res, buf);
  return build_real (type, res);
}

/* Given a real type, build an INT with the number of decimal digits required
   to represent a mantissa, such that a real is not reglected in comparison
   with 1, not including sign.  */

tree
a68_real_width (tree type)
{
  const machine_mode mode = TYPE_MODE (type);
  const struct real_format *fmt = REAL_MODE_FORMAT (mode);
  return build_int_cst (a68_int_type, fmt->p);
}

/* Given a real type, build an INT with the number of decimal digits required
   to represent a decimal exponent, such that a real can be correctly
   represented, not including sign.  */

tree
a68_real_exp_width (tree type ATTRIBUTE_UNUSED)
{
  const machine_mode mode = TYPE_MODE (type);
  const struct real_format *fmt = REAL_MODE_FORMAT (mode);
  const double log10_2 = .30102999566398119521;
  double log10_b = log10_2;
  int max_10_exp = fmt->emax * log10_b;

  return build_int_cst (a68_int_type, 1 + log10 (max_10_exp));
}

/* Given a real value VAL, return -1 if it is less than zero, 0 if it is zero
   and +1 if it is bigger than zero.  The built value is always of mode
   M_INT.  */

tree
a68_real_sign (tree val)
{
  tree zero = build_real (TREE_TYPE (val), dconst0);
  val = save_expr (val);
  return fold_build3 (COND_EXPR,
		      a68_int_type,
		      build2 (EQ_EXPR, integer_type_node, val, zero),
		      build_int_cst (a68_int_type, 0),
		      fold_build3 (COND_EXPR,
				   a68_int_type,
				   fold_build2 (GT_EXPR, integer_type_node, val, zero),
				   build_int_cst (a68_int_type, 1),
				   build_int_cst (a68_int_type, -1)));
}

/* Absolute value of a real value.  */

tree
a68_real_abs (tree val)
{
  return fold_build1 (ABS_EXPR, TREE_TYPE (val), val);
}

tree
a68_real_sqrt (tree type)
{
  enum built_in_function builtin;

  if (type == float_type_node)
    builtin = BUILT_IN_SQRTF;
  else if (type == double_type_node)
    builtin = BUILT_IN_SQRT;
  else if (type == long_double_type_node)
    builtin = BUILT_IN_SQRTL;
  else
    gcc_unreachable ();

  return addr_of_builtin_decl (builtin);
}

tree
a68_real_tan (tree type)
{
  enum built_in_function builtin;

  if (type == float_type_node)
    builtin = BUILT_IN_TANF;
  else if (type == double_type_node)
    builtin = BUILT_IN_TAN;
  else if (type == long_double_type_node)
    builtin = BUILT_IN_TANL;
  else
    gcc_unreachable ();

  return addr_of_builtin_decl (builtin);
}

tree
a68_real_sin (tree type)
{
  enum built_in_function builtin;

  if (type == float_type_node)
    builtin = BUILT_IN_SINF;
  else if (type == double_type_node)
    builtin = BUILT_IN_SIN;
  else if (type == long_double_type_node)
    builtin = BUILT_IN_SINL;
  else
    gcc_unreachable ();

  return addr_of_builtin_decl (builtin);
}

tree
a68_real_cos (tree type)
{
  enum built_in_function builtin;

  if (type == float_type_node)
    builtin = BUILT_IN_COSF;
  else if (type == double_type_node)
    builtin = BUILT_IN_COS;
  else if (type == long_double_type_node)
    builtin = BUILT_IN_COSL;
  else
    gcc_unreachable ();

  return addr_of_builtin_decl (builtin);
}

tree
a68_real_acos (tree type)
{
  enum built_in_function builtin;

  if (type == float_type_node)
    builtin = BUILT_IN_ACOSF;
  else if (type == double_type_node)
    builtin = BUILT_IN_ACOS;
  else if (type == long_double_type_node)
    builtin = BUILT_IN_ACOSL;
  else
    gcc_unreachable ();

  return addr_of_builtin_decl (builtin);
}

tree
a68_real_asin (tree type)
{
  enum built_in_function builtin;

  if (type == float_type_node)
    builtin = BUILT_IN_ASINF;
  else if (type == double_type_node)
    builtin = BUILT_IN_ASIN;
  else if (type == long_double_type_node)
    builtin = BUILT_IN_ASINL;
  else
    gcc_unreachable ();

  return addr_of_builtin_decl (builtin);
}

tree
a68_real_atan (tree type)
{
  enum built_in_function builtin;

  if (type == float_type_node)
    builtin = BUILT_IN_ATANF;
  else if (type == double_type_node)
    builtin = BUILT_IN_ATAN;
  else if (type == long_double_type_node)
    builtin = BUILT_IN_ATANL;
  else
    gcc_unreachable ();

  return addr_of_builtin_decl (builtin);
}

tree
a68_real_ln (tree type)
{
  enum built_in_function builtin;

  if (type == float_type_node)
    builtin = BUILT_IN_LOGF;
  else if (type == double_type_node)
    builtin = BUILT_IN_LOG;
  else if (type == long_double_type_node)
    builtin = BUILT_IN_LOGL;
  else
    gcc_unreachable ();

  return addr_of_builtin_decl (builtin);
}

tree
a68_real_log (tree type)
{
  enum built_in_function builtin;

  if (type == float_type_node)
    builtin = BUILT_IN_LOG10F;
  else if (type == double_type_node)
    builtin = BUILT_IN_LOG10;
  else if (type == long_double_type_node)
    builtin = BUILT_IN_LOG10L;
  else
    gcc_unreachable ();

  return addr_of_builtin_decl (builtin);
}

tree
a68_real_exp (tree type)
{
  enum built_in_function builtin;

  if (type == float_type_node)
    builtin = BUILT_IN_EXPF;
  else if (type == double_type_node)
    builtin = BUILT_IN_EXP;
  else if (type == long_double_type_node)
    builtin = BUILT_IN_EXPL;
  else
    gcc_unreachable ();

  return addr_of_builtin_decl (builtin);
}

/* Build the real value lengthened from the value of VAL, from mode
   FROM_MODE to mode TO_MODE.  */

tree
a68_real_leng (MOID_T *to_mode, MOID_T *from_mode ATTRIBUTE_UNUSED, tree val)
{
  /* Lengthening can be done by just a conversion.  */
  return fold_convert (CTYPE (to_mode), val);
}

/* Build the real value that can be lengthened to the value of VAL, from mode
   FROM_MODE to mode TO_MODE.

   If VAL cannot be represented in TO_MODE because it is bigger than the most
   positive value representable in TO_MODE, then it is truncated to that value.

   Likewise, if VAL cannot be represented in TO_MODE because it is less than
   the most negative value representable in TO_MODE, then it is truncated to
   that value.  */

tree
a68_real_shorten (MOID_T *to_mode, MOID_T *from_mode ATTRIBUTE_UNUSED, tree val)
{
  tree most_positive_value = fold_convert (CTYPE (from_mode),
					   a68_real_maxval (CTYPE (to_mode)));
  tree most_negative_value = fold_convert (CTYPE (from_mode),
					   a68_real_minval (CTYPE (to_mode)));

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

/* Given a real expression VAL of mode MODE, produce an integral value which is
   equal to the given real, or the next integer below (more negative than) the
   given real.  */

tree
a68_real_entier (tree val, MOID_T *to_mode, MOID_T *from_mode)
{
  tree fn = NULL_TREE;
  tree to_type = CTYPE (to_mode);

  if (from_mode == M_REAL)
    {
      if (to_type == integer_type_node)
	fn = builtin_decl_explicit (BUILT_IN_IFLOORF);
      else if (to_type == long_integer_type_node)
	fn = builtin_decl_explicit (BUILT_IN_LFLOORF);
      else if (to_type == long_long_integer_type_node)
	fn = builtin_decl_explicit (BUILT_IN_LLFLOORF);
      else
	gcc_unreachable ();
    }
  else if (from_mode == M_LONG_REAL)
    {
      if (to_type == integer_type_node)
	fn = builtin_decl_explicit (BUILT_IN_IFLOOR);
      else if (to_type == long_integer_type_node)
	fn = builtin_decl_explicit (BUILT_IN_LFLOOR);
      else if (to_type == long_long_integer_type_node)
	fn = builtin_decl_explicit (BUILT_IN_LLFLOOR);
      else
	gcc_unreachable ();
    }
  else if (from_mode == M_LONG_LONG_REAL)
    {
      if (to_type == integer_type_node)
	fn = builtin_decl_explicit (BUILT_IN_IFLOORL);
      else if (to_type == long_integer_type_node)
	fn = builtin_decl_explicit (BUILT_IN_LFLOORL);
      else if (to_type == long_long_integer_type_node)
	fn = builtin_decl_explicit (BUILT_IN_LLFLOORL);
      else
	gcc_unreachable ();
    }
  else
    gcc_unreachable ();

  return build_call_expr_loc (UNKNOWN_LOCATION, fn, 1, val);
}

/* Given a real expression VAL of mode MODE, produce an integral value which is
   the nearest integer to the given real.  */

tree
a68_real_round (tree val, MOID_T *to_mode, MOID_T *from_mode)
{
  tree fn = NULL_TREE;
  tree to_type = CTYPE (to_mode);

  if (from_mode == M_REAL)
    {
      if (to_type == integer_type_node)
	fn = builtin_decl_explicit (BUILT_IN_IROUNDF);
      else if (to_type == long_integer_type_node)
	fn = builtin_decl_explicit (BUILT_IN_LROUNDF);
      else if (to_type == long_long_integer_type_node)
	fn = builtin_decl_explicit (BUILT_IN_LLROUNDF);
      else
	gcc_unreachable ();
    }
  else if (from_mode == M_LONG_REAL)
    {
      if (to_type == integer_type_node)
	fn = builtin_decl_explicit (BUILT_IN_IROUND);
      else if (to_type == long_integer_type_node)
	fn = builtin_decl_explicit (BUILT_IN_LROUND);
      else if (to_type == long_long_integer_type_node)
	fn = builtin_decl_explicit (BUILT_IN_LLROUND);
      else
	gcc_unreachable ();
    }
  else if (from_mode == M_LONG_LONG_REAL)
    {
      if (to_type == integer_type_node)
	fn = builtin_decl_explicit (BUILT_IN_IROUNDL);
      else if (to_type == long_integer_type_node)
	fn = builtin_decl_explicit (BUILT_IN_LROUNDL);
      else if (to_type == long_long_integer_type_node)
	fn = builtin_decl_explicit (BUILT_IN_LLROUNDL);
      else
	gcc_unreachable ();
    }
  else
    gcc_unreachable ();

  return build_call_expr_loc (UNKNOWN_LOCATION, fn, 1, val);
}


/* Given two real values of mode M, build an expression that calculates the
   addition of A and B.  */

tree
a68_real_plus (MOID_T *m, tree a, tree b, location_t loc)
{
  return fold_build2_loc (loc, PLUS_EXPR, CTYPE (m), a, b);
}

/* Given two real values of mode M, build an expression that calculates the
   subtraction of A by B.  */

tree
a68_real_minus (MOID_T *m, tree a, tree b, location_t loc)
{
  return fold_build2_loc (loc, MINUS_EXPR, CTYPE (m), a, b);
}

/* Given two real values of mode M, build an expression that calculates the
   multiplication of A by B.  */

tree
a68_real_mult (MOID_T *m, tree a, tree b, location_t loc)
{
  return fold_build2_loc (loc, MULT_EXPR, CTYPE (m), a, b);
}

/* Given two real values of mode M, build an expression that calculates the
   division of A by B.  */

tree
a68_real_div (MOID_T *m, tree a, tree b, location_t loc)
{
  return fold_build2_loc (loc, RDIV_EXPR, CTYPE (m), a, b);
}

/* Given two real values of mode M, build an expression that calculates whether
   A = B.  */

tree
a68_real_eq (tree a, tree b, location_t loc)
{
  return fold_build2_loc (loc, EQ_EXPR, boolean_type_node, a, b);
}

/* Given two real values of mode M, build an expression that calculates whether
   A /= B.  */

tree
a68_real_ne (tree a, tree b, location_t loc)
{
  return fold_build2_loc (loc, NE_EXPR, boolean_type_node, a, b);
}

/* Given two real values of mode M, build an expression that calculates whether
   A < B.  */

tree
a68_real_lt (tree a, tree b, location_t loc)
{
  return fold_build2_loc (loc, LT_EXPR, boolean_type_node, a, b);
}

/* Given two real values of mode M, build an expression that calculates
   whether A <= B.  */

tree
a68_real_le (tree a, tree b, location_t loc)
{
  return fold_build2_loc (loc, LE_EXPR, boolean_type_node, a, b);
}

/* Given two real values of mode M, build an expression that calculates whether
   A > B.  */

tree
a68_real_gt (tree a, tree b, location_t loc)
{
  return fold_build2_loc (loc, GT_EXPR, boolean_type_node, a, b);
}

/* Given two real values of mode M, build an expression that calculates whether
   A >= B.  */

tree
a68_real_ge (tree a, tree b, location_t loc)
{
  return fold_build2_loc (loc, GE_EXPR, boolean_type_node, a, b);
}

/* Exponentiation involving real values.

   REAL <- REAL, REAL
   REAL <- REAL, INT
   LONG REAL <- LONG REAL, LONG REAL
   LONG REAL <- LONG REAL, INT
   LONG LONG REAL <- LONG LONG REAL, LONG LONG REAL
   LONG LONG REAL <- LONG LONG REAL, INT  */

tree
a68_real_pow (MOID_T *m, MOID_T *a_mode, MOID_T *b_mode,
	      tree a, tree b, location_t loc)
{
  enum built_in_function built_in;
  if (m == M_REAL)
    {
      gcc_assert (a_mode == M_REAL);
      built_in = b_mode == M_REAL ? BUILT_IN_POWF : BUILT_IN_POWIF;
    }
  else if (m == M_LONG_REAL)
    {
      gcc_assert (a_mode == M_LONG_REAL);
      built_in = b_mode == M_LONG_REAL ? BUILT_IN_POW : BUILT_IN_POWI;
    }
  else if (m == M_LONG_LONG_REAL)
    {
      gcc_assert (a_mode == M_LONG_LONG_REAL);
      built_in = b_mode == M_LONG_LONG_REAL ? BUILT_IN_POWL : BUILT_IN_POWIL;
    }
  else
    gcc_unreachable ();

  tree call = builtin_decl_explicit (built_in);
  gcc_assert (call != NULL_TREE);
  return build_call_expr_loc (loc, call, 2, a, b);
}
