/* Fold a constant sub-tree into a single node for C-compiler
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

/*@@ This file should be rewritten to use an arbitrary precision
  @@ representation for "struct tree_int_cst" and "struct tree_real_cst".
  @@ Perhaps the routines could also be used for bc/dc, and made a lib.
  @@ The routines that translate from the ap rep should
  @@ warn if precision et. al. is lost.
  @@ This would also make life easier when this technology is used
  @@ for cross-compilers.  */

/* The entry points in this file are fold, size_int_wide and size_binop.

   fold takes a tree as argument and returns a simplified tree.

   size_binop takes a tree code for an arithmetic operation
   and two operands that are trees, and produces a tree for the
   result, assuming the type comes from `sizetype'.

   size_int takes an integer value, and creates a tree constant
   with type from `sizetype'.

   Note: Since the folders get called on non-gimple code as well as
   gimple code, we need to handle GIMPLE tuples as well as their
   corresponding tree equivalents.  */

#define INCLUDE_ALGORITHM
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "predict.h"
#include "memmodel.h"
#include "tm_p.h"
#include "tree-ssa-operands.h"
#include "optabs-query.h"
#include "cgraph.h"
#include "diagnostic-core.h"
#include "flags.h"
#include "alias.h"
#include "fold-const.h"
#include "fold-const-call.h"
#include "stor-layout.h"
#include "calls.h"
#include "tree-iterator.h"
#include "expr.h"
#include "intl.h"
#include "langhooks.h"
#include "tree-eh.h"
#include "gimplify.h"
#include "tree-dfa.h"
#include "builtins.h"
#include "generic-match.h"
#include "gimple-iterator.h"
#include "gimple-fold.h"
#include "tree-into-ssa.h"
#include "md5.h"
#include "case-cfn-macros.h"
#include "stringpool.h"
#include "tree-vrp.h"
#include "tree-ssanames.h"
#include "selftest.h"
#include "stringpool.h"
#include "attribs.h"
#include "tree-vector-builder.h"
#include "vec-perm-indices.h"
#include "asan.h"
#include "gimple-range.h"

/* Nonzero if we are folding constants inside an initializer or a C++
   manifestly-constant-evaluated context; zero otherwise.
   Should be used when folding in initializer enables additional
   optimizations.  */
int folding_initializer = 0;

/* Nonzero if we are folding C++ manifestly-constant-evaluated context; zero
   otherwise.
   Should be used when certain constructs shouldn't be optimized
   during folding in that context.  */
bool folding_cxx_constexpr = false;

/* The following constants represent a bit based encoding of GCC's
   comparison operators.  This encoding simplifies transformations
   on relational comparison operators, such as AND and OR.  */
enum comparison_code {
  COMPCODE_FALSE = 0,
  COMPCODE_LT = 1,
  COMPCODE_EQ = 2,
  COMPCODE_LE = 3,
  COMPCODE_GT = 4,
  COMPCODE_LTGT = 5,
  COMPCODE_GE = 6,
  COMPCODE_ORD = 7,
  COMPCODE_UNORD = 8,
  COMPCODE_UNLT = 9,
  COMPCODE_UNEQ = 10,
  COMPCODE_UNLE = 11,
  COMPCODE_UNGT = 12,
  COMPCODE_NE = 13,
  COMPCODE_UNGE = 14,
  COMPCODE_TRUE = 15
};

static bool negate_expr_p (tree);
static tree negate_expr (tree);
static tree associate_trees (location_t, tree, tree, enum tree_code, tree);
static enum comparison_code comparison_to_compcode (enum tree_code);
static enum tree_code compcode_to_comparison (enum comparison_code);
static bool twoval_comparison_p (tree, tree *, tree *);
static tree eval_subst (location_t, tree, tree, tree, tree, tree);
static tree optimize_bit_field_compare (location_t, enum tree_code,
					tree, tree, tree);
static bool simple_operand_p (const_tree);
static tree range_binop (enum tree_code, tree, tree, int, tree, int);
static tree range_predecessor (tree);
static tree range_successor (tree);
static tree fold_range_test (location_t, enum tree_code, tree, tree, tree);
static tree fold_cond_expr_with_comparison (location_t, tree, enum tree_code,
					    tree, tree, tree, tree);
static tree unextend (tree, int, int, tree);
static tree extract_muldiv (tree, tree, enum tree_code, tree, bool *);
static tree extract_muldiv_1 (tree, tree, enum tree_code, tree, bool *);
static tree fold_binary_op_with_conditional_arg (location_t,
						 enum tree_code, tree,
						 tree, tree,
						 tree, tree, int);
static tree fold_negate_const (tree, tree);
static tree fold_not_const (const_tree, tree);
static tree fold_relational_const (enum tree_code, tree, tree, tree);
static tree fold_convert_const (enum tree_code, tree, tree);
static tree fold_view_convert_expr (tree, tree);
static tree fold_negate_expr (location_t, tree);

/* This is a helper function to detect min/max for some operands of COND_EXPR.
   The form is "(EXP0 CMP EXP1) ? EXP2 : EXP3". */
tree_code
minmax_from_comparison (tree_code cmp, tree exp0, tree exp1, tree exp2, tree exp3)
{
  enum tree_code code = ERROR_MARK;

  if (HONOR_NANS (exp0) || HONOR_SIGNED_ZEROS (exp0))
    return ERROR_MARK;

  if (!operand_equal_p (exp0, exp2))
    return ERROR_MARK;

  if (TREE_CODE (exp3) == INTEGER_CST && TREE_CODE (exp1) == INTEGER_CST)
    {
      if (wi::to_widest (exp1) == (wi::to_widest (exp3) - 1))
	{
	  /* X <= Y - 1 equals to X < Y.  */
	  if (cmp == LE_EXPR)
	    code = LT_EXPR;
	  /* X > Y - 1 equals to X >= Y.  */
	  if (cmp == GT_EXPR)
	    code = GE_EXPR;
	  /* a != MIN_RANGE<a> ? a : MIN_RANGE<a>+1 -> MAX_EXPR<MIN_RANGE<a>+1, a> */
	  if (cmp == NE_EXPR && TREE_CODE (exp0) == SSA_NAME)
	    {
	      int_range_max r;
	      get_range_query (cfun)->range_of_expr (r, exp0);
	      if (r.undefined_p ())
		r.set_varying (TREE_TYPE (exp0));

	      widest_int min = widest_int::from (r.lower_bound (),
						 TYPE_SIGN (TREE_TYPE (exp0)));
	      if (min == wi::to_widest (exp1))
		code = MAX_EXPR;
	    }
	}
      if (wi::to_widest (exp1) == (wi::to_widest (exp3) + 1))
	{
	  /* X < Y + 1 equals to X <= Y.  */
	  if (cmp == LT_EXPR)
	    code = LE_EXPR;
	  /* X >= Y + 1 equals to X > Y.  */
	  if (cmp == GE_EXPR)
	  code = GT_EXPR;
	  /* a != MAX_RANGE<a> ? a : MAX_RANGE<a>-1 -> MIN_EXPR<MIN_RANGE<a>-1, a> */
	  if (cmp == NE_EXPR && TREE_CODE (exp0) == SSA_NAME)
	    {
	      int_range_max r;
	      get_range_query (cfun)->range_of_expr (r, exp0);
	      if (r.undefined_p ())
		r.set_varying (TREE_TYPE (exp0));

	      widest_int max = widest_int::from (r.upper_bound (),
						 TYPE_SIGN (TREE_TYPE (exp0)));
	      if (max == wi::to_widest (exp1))
		code = MIN_EXPR;
	    }
	}
    }
  if (code != ERROR_MARK
      || operand_equal_p (exp1, exp3))
    {
      if (cmp == LT_EXPR || cmp == LE_EXPR)
	code = MIN_EXPR;
      if (cmp == GT_EXPR || cmp == GE_EXPR)
	code = MAX_EXPR;
    }
  return code;
}

/* Return EXPR_LOCATION of T if it is not UNKNOWN_LOCATION.
   Otherwise, return LOC.  */

static location_t
expr_location_or (tree t, location_t loc)
{
  location_t tloc = EXPR_LOCATION (t);
  return tloc == UNKNOWN_LOCATION ? loc : tloc;
}

/* Similar to protected_set_expr_location, but never modify x in place,
   if location can and needs to be set, unshare it.  */

tree
protected_set_expr_location_unshare (tree x, location_t loc)
{
  if (CAN_HAVE_LOCATION_P (x)
      && EXPR_LOCATION (x) != loc
      && !(TREE_CODE (x) == SAVE_EXPR
	   || TREE_CODE (x) == TARGET_EXPR
	   || TREE_CODE (x) == BIND_EXPR))
    {
      x = copy_node (x);
      SET_EXPR_LOCATION (x, loc);
    }
  return x;
}

/* If ARG2 divides ARG1 with zero remainder, carries out the exact
   division and returns the quotient.  Otherwise returns
   NULL_TREE.  */

tree
div_if_zero_remainder (const_tree arg1, const_tree arg2)
{
  widest_int quo;

  if (wi::multiple_of_p (wi::to_widest (arg1), wi::to_widest (arg2),
			 SIGNED, &quo))
    return wide_int_to_tree (TREE_TYPE (arg1), quo);

  return NULL_TREE; 
}

/* This is nonzero if we should defer warnings about undefined
   overflow.  This facility exists because these warnings are a
   special case.  The code to estimate loop iterations does not want
   to issue any warnings, since it works with expressions which do not
   occur in user code.  Various bits of cleanup code call fold(), but
   only use the result if it has certain characteristics (e.g., is a
   constant); that code only wants to issue a warning if the result is
   used.  */

static int fold_deferring_overflow_warnings;

/* If a warning about undefined overflow is deferred, this is the
   warning.  Note that this may cause us to turn two warnings into
   one, but that is fine since it is sufficient to only give one
   warning per expression.  */

static const char* fold_deferred_overflow_warning;

/* If a warning about undefined overflow is deferred, this is the
   level at which the warning should be emitted.  */

static enum warn_strict_overflow_code fold_deferred_overflow_code;

/* Start deferring overflow warnings.  We could use a stack here to
   permit nested calls, but at present it is not necessary.  */

void
fold_defer_overflow_warnings (void)
{
  ++fold_deferring_overflow_warnings;
}

/* Stop deferring overflow warnings.  If there is a pending warning,
   and ISSUE is true, then issue the warning if appropriate.  STMT is
   the statement with which the warning should be associated (used for
   location information); STMT may be NULL.  CODE is the level of the
   warning--a warn_strict_overflow_code value.  This function will use
   the smaller of CODE and the deferred code when deciding whether to
   issue the warning.  CODE may be zero to mean to always use the
   deferred code.  */

void
fold_undefer_overflow_warnings (bool issue, const gimple *stmt, int code)
{
  const char *warnmsg;
  location_t locus;

  gcc_assert (fold_deferring_overflow_warnings > 0);
  --fold_deferring_overflow_warnings;
  if (fold_deferring_overflow_warnings > 0)
    {
      if (fold_deferred_overflow_warning != NULL
	  && code != 0
	  && code < (int) fold_deferred_overflow_code)
	fold_deferred_overflow_code = (enum warn_strict_overflow_code) code;
      return;
    }

  warnmsg = fold_deferred_overflow_warning;
  fold_deferred_overflow_warning = NULL;

  if (!issue || warnmsg == NULL)
    return;

  if (warning_suppressed_p (stmt, OPT_Wstrict_overflow))
    return;

  /* Use the smallest code level when deciding to issue the
     warning.  */
  if (code == 0 || code > (int) fold_deferred_overflow_code)
    code = fold_deferred_overflow_code;

  if (!issue_strict_overflow_warning (code))
    return;

  if (stmt == NULL)
    locus = input_location;
  else
    locus = gimple_location (stmt);
  warning_at (locus, OPT_Wstrict_overflow, "%s", warnmsg);
}

/* Stop deferring overflow warnings, ignoring any deferred
   warnings.  */

void
fold_undefer_and_ignore_overflow_warnings (void)
{
  fold_undefer_overflow_warnings (false, NULL, 0);
}

/* Whether we are deferring overflow warnings.  */

bool
fold_deferring_overflow_warnings_p (void)
{
  return fold_deferring_overflow_warnings > 0;
}

/* This is called when we fold something based on the fact that signed
   overflow is undefined.  */

void
fold_overflow_warning (const char* gmsgid, enum warn_strict_overflow_code wc)
{
  if (fold_deferring_overflow_warnings > 0)
    {
      if (fold_deferred_overflow_warning == NULL
	  || wc < fold_deferred_overflow_code)
	{
	  fold_deferred_overflow_warning = gmsgid;
	  fold_deferred_overflow_code = wc;
	}
    }
  else if (issue_strict_overflow_warning (wc))
    warning (OPT_Wstrict_overflow, gmsgid);
}

/* Return true if the built-in mathematical function specified by CODE
   is odd, i.e. -f(x) == f(-x).  */

bool
negate_mathfn_p (combined_fn fn)
{
  switch (fn)
    {
    CASE_CFN_ASIN:
    CASE_CFN_ASIN_FN:
    CASE_CFN_ASINH:
    CASE_CFN_ASINH_FN:
    CASE_CFN_ATAN:
    CASE_CFN_ATAN_FN:
    CASE_CFN_ATANH:
    CASE_CFN_ATANH_FN:
    CASE_CFN_CASIN:
    CASE_CFN_CASIN_FN:
    CASE_CFN_CASINH:
    CASE_CFN_CASINH_FN:
    CASE_CFN_CATAN:
    CASE_CFN_CATAN_FN:
    CASE_CFN_CATANH:
    CASE_CFN_CATANH_FN:
    CASE_CFN_CBRT:
    CASE_CFN_CBRT_FN:
    CASE_CFN_CPROJ:
    CASE_CFN_CPROJ_FN:
    CASE_CFN_CSIN:
    CASE_CFN_CSIN_FN:
    CASE_CFN_CSINH:
    CASE_CFN_CSINH_FN:
    CASE_CFN_CTAN:
    CASE_CFN_CTAN_FN:
    CASE_CFN_CTANH:
    CASE_CFN_CTANH_FN:
    CASE_CFN_ERF:
    CASE_CFN_ERF_FN:
    CASE_CFN_LLROUND:
    CASE_CFN_LLROUND_FN:
    CASE_CFN_LROUND:
    CASE_CFN_LROUND_FN:
    CASE_CFN_ROUND:
    CASE_CFN_ROUNDEVEN:
    CASE_CFN_ROUNDEVEN_FN:
    CASE_CFN_SIN:
    CASE_CFN_SIN_FN:
    CASE_CFN_SINH:
    CASE_CFN_SINH_FN:
    CASE_CFN_TAN:
    CASE_CFN_TAN_FN:
    CASE_CFN_TANH:
    CASE_CFN_TANH_FN:
    CASE_CFN_TRUNC:
    CASE_CFN_TRUNC_FN:
      return true;

    CASE_CFN_LLRINT:
    CASE_CFN_LLRINT_FN:
    CASE_CFN_LRINT:
    CASE_CFN_LRINT_FN:
    CASE_CFN_NEARBYINT:
    CASE_CFN_NEARBYINT_FN:
    CASE_CFN_RINT:
    CASE_CFN_RINT_FN:
      return !flag_rounding_math;

    default:
      break;
    }
  return false;
}

/* Check whether we may negate an integer constant T without causing
   overflow.  */

bool
may_negate_without_overflow_p (const_tree t)
{
  tree type;

  gcc_assert (TREE_CODE (t) == INTEGER_CST);

  type = TREE_TYPE (t);
  if (TYPE_UNSIGNED (type))
    return false;

  return !wi::only_sign_bit_p (wi::to_wide (t));
}

/* Determine whether an expression T can be cheaply negated using
   the function negate_expr without introducing undefined overflow.  */

static bool
negate_expr_p (tree t)
{
  tree type;

  if (t == 0)
    return false;

  type = TREE_TYPE (t);

  STRIP_SIGN_NOPS (t);
  switch (TREE_CODE (t))
    {
    case INTEGER_CST:
      if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type))
	return true;

      /* Check that -CST will not overflow type.  */
      return may_negate_without_overflow_p (t);
    case BIT_NOT_EXPR:
      return (INTEGRAL_TYPE_P (type)
	      && TYPE_OVERFLOW_WRAPS (type));

    case FIXED_CST:
      return true;

    case NEGATE_EXPR:
      return !TYPE_OVERFLOW_SANITIZED (type);

    case REAL_CST:
      /* We want to canonicalize to positive real constants.  Pretend
         that only negative ones can be easily negated.  */
      return REAL_VALUE_NEGATIVE (TREE_REAL_CST (t));

    case COMPLEX_CST:
      return negate_expr_p (TREE_REALPART (t))
	     && negate_expr_p (TREE_IMAGPART (t));

    case VECTOR_CST:
      {
	if (FLOAT_TYPE_P (TREE_TYPE (type)) || TYPE_OVERFLOW_WRAPS (type))
	  return true;

	/* Steps don't prevent negation.  */
	unsigned int count = vector_cst_encoded_nelts (t);
	for (unsigned int i = 0; i < count; ++i)
	  if (!negate_expr_p (VECTOR_CST_ENCODED_ELT (t, i)))
	    return false;

	return true;
      }

    case COMPLEX_EXPR:
      return negate_expr_p (TREE_OPERAND (t, 0))
	     && negate_expr_p (TREE_OPERAND (t, 1));

    case CONJ_EXPR:
      return negate_expr_p (TREE_OPERAND (t, 0));

    case PLUS_EXPR:
      if (HONOR_SIGN_DEPENDENT_ROUNDING (type)
	  || HONOR_SIGNED_ZEROS (type)
	  || (ANY_INTEGRAL_TYPE_P (type)
	      && ! TYPE_OVERFLOW_WRAPS (type)))
	return false;
      /* -(A + B) -> (-B) - A.  */
      if (negate_expr_p (TREE_OPERAND (t, 1)))
	return true;
      /* -(A + B) -> (-A) - B.  */
      return negate_expr_p (TREE_OPERAND (t, 0));

    case MINUS_EXPR:
      /* We can't turn -(A-B) into B-A when we honor signed zeros.  */
      return !HONOR_SIGN_DEPENDENT_ROUNDING (type)
	     && !HONOR_SIGNED_ZEROS (type)
	     && (! ANY_INTEGRAL_TYPE_P (type)
		 || TYPE_OVERFLOW_WRAPS (type));

    case MULT_EXPR:
      if (TYPE_UNSIGNED (type))
	break;
      /* INT_MIN/n * n doesn't overflow while negating one operand it does
         if n is a (negative) power of two.  */
      if (INTEGRAL_TYPE_P (TREE_TYPE (t))
	  && ! TYPE_OVERFLOW_WRAPS (TREE_TYPE (t))
	  && ! ((TREE_CODE (TREE_OPERAND (t, 0)) == INTEGER_CST
		 && (wi::popcount
		     (wi::abs (wi::to_wide (TREE_OPERAND (t, 0))))) != 1)
		|| (TREE_CODE (TREE_OPERAND (t, 1)) == INTEGER_CST
		    && (wi::popcount
			(wi::abs (wi::to_wide (TREE_OPERAND (t, 1))))) != 1)))
	break;

      /* Fall through.  */

    case RDIV_EXPR:
      if (! HONOR_SIGN_DEPENDENT_ROUNDING (t))
	return negate_expr_p (TREE_OPERAND (t, 1))
	       || negate_expr_p (TREE_OPERAND (t, 0));
      break;

    case TRUNC_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case EXACT_DIV_EXPR:
      if (TYPE_UNSIGNED (type))
	break;
      /* In general we can't negate A in A / B, because if A is INT_MIN and
         B is not 1 we change the sign of the result.  */
      if (TREE_CODE (TREE_OPERAND (t, 0)) == INTEGER_CST
	  && negate_expr_p (TREE_OPERAND (t, 0)))
	return true;
      /* In general we can't negate B in A / B, because if A is INT_MIN and
	 B is 1, we may turn this into INT_MIN / -1 which is undefined
	 and actually traps on some architectures.  */
      if (! ANY_INTEGRAL_TYPE_P (TREE_TYPE (t))
	  || TYPE_OVERFLOW_WRAPS (TREE_TYPE (t))
	  || (TREE_CODE (TREE_OPERAND (t, 1)) == INTEGER_CST
	      && ! integer_onep (TREE_OPERAND (t, 1))))
	return negate_expr_p (TREE_OPERAND (t, 1));
      break;

    case NOP_EXPR:
      /* Negate -((double)float) as (double)(-float).  */
      if (SCALAR_FLOAT_TYPE_P (type))
	{
	  tree tem = strip_float_extensions (t);
	  if (tem != t)
	    return negate_expr_p (tem);
	}
      break;

    case CALL_EXPR:
      /* Negate -f(x) as f(-x).  */
      if (negate_mathfn_p (get_call_combined_fn (t)))
	return negate_expr_p (CALL_EXPR_ARG (t, 0));
      break;

    case RSHIFT_EXPR:
      /* Optimize -((int)x >> 31) into (unsigned)x >> 31 for int.  */
      if (TREE_CODE (TREE_OPERAND (t, 1)) == INTEGER_CST)
	{
	  tree op1 = TREE_OPERAND (t, 1);
	  if (wi::to_wide (op1) == element_precision (type) - 1)
	    return true;
	}
      break;

    default:
      break;
    }
  return false;
}

/* Given T, an expression, return a folded tree for -T or NULL_TREE, if no
   simplification is possible.
   If negate_expr_p would return true for T, NULL_TREE will never be
   returned.  */

static tree
fold_negate_expr_1 (location_t loc, tree t)
{
  tree type = TREE_TYPE (t);
  tree tem;

  switch (TREE_CODE (t))
    {
    /* Convert - (~A) to A + 1.  */
    case BIT_NOT_EXPR:
      if (INTEGRAL_TYPE_P (type))
        return fold_build2_loc (loc, PLUS_EXPR, type, TREE_OPERAND (t, 0),
				build_one_cst (type));
      break;

    case INTEGER_CST:
      tem = fold_negate_const (t, type);
      if (TREE_OVERFLOW (tem) == TREE_OVERFLOW (t)
	  || (ANY_INTEGRAL_TYPE_P (type)
	      && !TYPE_OVERFLOW_TRAPS (type)
	      && TYPE_OVERFLOW_WRAPS (type))
	  || (flag_sanitize & SANITIZE_SI_OVERFLOW) == 0)
	return tem;
      break;

    case POLY_INT_CST:
    case REAL_CST:
    case FIXED_CST:
      tem = fold_negate_const (t, type);
      return tem;

    case COMPLEX_CST:
      {
	tree rpart = fold_negate_expr (loc, TREE_REALPART (t));
	tree ipart = fold_negate_expr (loc, TREE_IMAGPART (t));
	if (rpart && ipart)
	  return build_complex (type, rpart, ipart);
      }
      break;

    case VECTOR_CST:
      {
	tree_vector_builder elts;
	elts.new_unary_operation (type, t, true);
	unsigned int count = elts.encoded_nelts ();
	for (unsigned int i = 0; i < count; ++i)
	  {
	    tree elt = fold_negate_expr (loc, VECTOR_CST_ELT (t, i));
	    if (elt == NULL_TREE)
	      return NULL_TREE;
	    elts.quick_push (elt);
	  }

	return elts.build ();
      }

    case COMPLEX_EXPR:
      if (negate_expr_p (t))
	return fold_build2_loc (loc, COMPLEX_EXPR, type,
				fold_negate_expr (loc, TREE_OPERAND (t, 0)),
				fold_negate_expr (loc, TREE_OPERAND (t, 1)));
      break;

    case CONJ_EXPR:
      if (negate_expr_p (t))
	return fold_build1_loc (loc, CONJ_EXPR, type,
				fold_negate_expr (loc, TREE_OPERAND (t, 0)));
      break;

    case NEGATE_EXPR:
      if (!TYPE_OVERFLOW_SANITIZED (type))
	return TREE_OPERAND (t, 0);
      break;

    case PLUS_EXPR:
      if (!HONOR_SIGN_DEPENDENT_ROUNDING (type)
	  && !HONOR_SIGNED_ZEROS (type))
	{
	  /* -(A + B) -> (-B) - A.  */
	  if (negate_expr_p (TREE_OPERAND (t, 1)))
	    {
	      tem = negate_expr (TREE_OPERAND (t, 1));
	      return fold_build2_loc (loc, MINUS_EXPR, type,
				      tem, TREE_OPERAND (t, 0));
	    }

	  /* -(A + B) -> (-A) - B.  */
	  if (negate_expr_p (TREE_OPERAND (t, 0)))
	    {
	      tem = negate_expr (TREE_OPERAND (t, 0));
	      return fold_build2_loc (loc, MINUS_EXPR, type,
				      tem, TREE_OPERAND (t, 1));
	    }
	}
      break;

    case MINUS_EXPR:
      /* - (A - B) -> B - A  */
      if (!HONOR_SIGN_DEPENDENT_ROUNDING (type)
	  && !HONOR_SIGNED_ZEROS (type))
	return fold_build2_loc (loc, MINUS_EXPR, type,
				TREE_OPERAND (t, 1), TREE_OPERAND (t, 0));
      break;

    case MULT_EXPR:
      if (TYPE_UNSIGNED (type))
        break;

      /* Fall through.  */

    case RDIV_EXPR:
      if (! HONOR_SIGN_DEPENDENT_ROUNDING (type))
	{
	  tem = TREE_OPERAND (t, 1);
	  if (negate_expr_p (tem))
	    return fold_build2_loc (loc, TREE_CODE (t), type,
				    TREE_OPERAND (t, 0), negate_expr (tem));
	  tem = TREE_OPERAND (t, 0);
	  if (negate_expr_p (tem))
	    return fold_build2_loc (loc, TREE_CODE (t), type,
				    negate_expr (tem), TREE_OPERAND (t, 1));
	}
      break;

    case TRUNC_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case EXACT_DIV_EXPR:
      if (TYPE_UNSIGNED (type))
	break;
      /* In general we can't negate A in A / B, because if A is INT_MIN and
	 B is not 1 we change the sign of the result.  */
      if (TREE_CODE (TREE_OPERAND (t, 0)) == INTEGER_CST
	  && negate_expr_p (TREE_OPERAND (t, 0)))
	return fold_build2_loc (loc, TREE_CODE (t), type,
				negate_expr (TREE_OPERAND (t, 0)),
				TREE_OPERAND (t, 1));
      /* In general we can't negate B in A / B, because if A is INT_MIN and
	 B is 1, we may turn this into INT_MIN / -1 which is undefined
	 and actually traps on some architectures.  */
      if ((! ANY_INTEGRAL_TYPE_P (TREE_TYPE (t))
	   || TYPE_OVERFLOW_WRAPS (TREE_TYPE (t))
	   || (TREE_CODE (TREE_OPERAND (t, 1)) == INTEGER_CST
	       && ! integer_onep (TREE_OPERAND (t, 1))))
	  && negate_expr_p (TREE_OPERAND (t, 1)))
	return fold_build2_loc (loc, TREE_CODE (t), type,
				TREE_OPERAND (t, 0),
				negate_expr (TREE_OPERAND (t, 1)));
      break;

    case NOP_EXPR:
      /* Convert -((double)float) into (double)(-float).  */
      if (SCALAR_FLOAT_TYPE_P (type))
	{
	  tem = strip_float_extensions (t);
	  if (tem != t && negate_expr_p (tem))
	    return fold_convert_loc (loc, type, negate_expr (tem));
	}
      break;

    case CALL_EXPR:
      /* Negate -f(x) as f(-x).  */
      if (negate_mathfn_p (get_call_combined_fn (t))
	  && negate_expr_p (CALL_EXPR_ARG (t, 0)))
	{
	  tree fndecl, arg;

	  fndecl = get_callee_fndecl (t);
	  arg = negate_expr (CALL_EXPR_ARG (t, 0));
	  return build_call_expr_loc (loc, fndecl, 1, arg);
	}
      break;

    case RSHIFT_EXPR:
      /* Optimize -((int)x >> 31) into (unsigned)x >> 31 for int.  */
      if (TREE_CODE (TREE_OPERAND (t, 1)) == INTEGER_CST)
	{
	  tree op1 = TREE_OPERAND (t, 1);
	  if (wi::to_wide (op1) == element_precision (type) - 1)
	    {
	      tree ntype = TYPE_UNSIGNED (type)
			   ? signed_type_for (type)
			   : unsigned_type_for (type);
	      tree temp = fold_convert_loc (loc, ntype, TREE_OPERAND (t, 0));
	      temp = fold_build2_loc (loc, RSHIFT_EXPR, ntype, temp, op1);
	      return fold_convert_loc (loc, type, temp);
	    }
	}
      break;

    default:
      break;
    }

  return NULL_TREE;
}

/* A wrapper for fold_negate_expr_1.  */

static tree
fold_negate_expr (location_t loc, tree t)
{
  tree type = TREE_TYPE (t);
  STRIP_SIGN_NOPS (t);
  tree tem = fold_negate_expr_1 (loc, t);
  if (tem == NULL_TREE)
    return NULL_TREE;
  return fold_convert_loc (loc, type, tem);
}

/* Like fold_negate_expr, but return a NEGATE_EXPR tree, if T cannot be
   negated in a simpler way.  Also allow for T to be NULL_TREE, in which case
   return NULL_TREE. */

static tree
negate_expr (tree t)
{
  tree type, tem;
  location_t loc;

  if (t == NULL_TREE)
    return NULL_TREE;

  loc = EXPR_LOCATION (t);
  type = TREE_TYPE (t);
  STRIP_SIGN_NOPS (t);

  tem = fold_negate_expr (loc, t);
  if (!tem)
    tem = build1_loc (loc, NEGATE_EXPR, TREE_TYPE (t), t);
  return fold_convert_loc (loc, type, tem);
}

/* Split a tree IN into a constant, literal and variable parts that could be
   combined with CODE to make IN.  "constant" means an expression with
   TREE_CONSTANT but that isn't an actual constant.  CODE must be a
   commutative arithmetic operation.  Store the constant part into *CONP,
   the literal in *LITP and return the variable part.  If a part isn't
   present, set it to null.  If the tree does not decompose in this way,
   return the entire tree as the variable part and the other parts as null.

   If CODE is PLUS_EXPR we also split trees that use MINUS_EXPR.  In that
   case, we negate an operand that was subtracted.  Except if it is a
   literal for which we use *MINUS_LITP instead.

   If NEGATE_P is true, we are negating all of IN, again except a literal
   for which we use *MINUS_LITP instead.  If a variable part is of pointer
   type, it is negated after converting to TYPE.  This prevents us from
   generating illegal MINUS pointer expression.  LOC is the location of
   the converted variable part.

   If IN is itself a literal or constant, return it as appropriate.

   Note that we do not guarantee that any of the three values will be the
   same type as IN, but they will have the same signedness and mode.  */

static tree
split_tree (tree in, tree type, enum tree_code code,
	    tree *minus_varp, tree *conp, tree *minus_conp,
	    tree *litp, tree *minus_litp, int negate_p)
{
  tree var = 0;
  *minus_varp = 0;
  *conp = 0;
  *minus_conp = 0;
  *litp = 0;
  *minus_litp = 0;

  /* Strip any conversions that don't change the machine mode or signedness.  */
  STRIP_SIGN_NOPS (in);

  if (TREE_CODE (in) == INTEGER_CST || TREE_CODE (in) == REAL_CST
      || TREE_CODE (in) == FIXED_CST)
    *litp = in;
  else if (TREE_CODE (in) == code
	   || ((! FLOAT_TYPE_P (TREE_TYPE (in)) || flag_associative_math)
	       && ! SAT_FIXED_POINT_TYPE_P (TREE_TYPE (in))
	       /* We can associate addition and subtraction together (even
		  though the C standard doesn't say so) for integers because
		  the value is not affected.  For reals, the value might be
		  affected, so we can't.  */
	       && ((code == PLUS_EXPR && TREE_CODE (in) == POINTER_PLUS_EXPR)
		   || (code == PLUS_EXPR && TREE_CODE (in) == MINUS_EXPR)
		   || (code == MINUS_EXPR
		       && (TREE_CODE (in) == PLUS_EXPR
			   || TREE_CODE (in) == POINTER_PLUS_EXPR)))))
    {
      tree op0 = TREE_OPERAND (in, 0);
      tree op1 = TREE_OPERAND (in, 1);
      bool neg1_p = TREE_CODE (in) == MINUS_EXPR;
      bool neg_litp_p = false, neg_conp_p = false, neg_var_p = false;

      /* First see if either of the operands is a literal, then a constant.  */
      if (TREE_CODE (op0) == INTEGER_CST || TREE_CODE (op0) == REAL_CST
	  || TREE_CODE (op0) == FIXED_CST)
	*litp = op0, op0 = 0;
      else if (TREE_CODE (op1) == INTEGER_CST || TREE_CODE (op1) == REAL_CST
	       || TREE_CODE (op1) == FIXED_CST)
	*litp = op1, neg_litp_p = neg1_p, op1 = 0;

      if (op0 != 0 && TREE_CONSTANT (op0))
	*conp = op0, op0 = 0;
      else if (op1 != 0 && TREE_CONSTANT (op1))
	*conp = op1, neg_conp_p = neg1_p, op1 = 0;

      /* If we haven't dealt with either operand, this is not a case we can
	 decompose.  Otherwise, VAR is either of the ones remaining, if any.  */
      if (op0 != 0 && op1 != 0)
	var = in;
      else if (op0 != 0)
	var = op0;
      else
	var = op1, neg_var_p = neg1_p;

      /* Now do any needed negations.  */
      if (neg_litp_p)
	*minus_litp = *litp, *litp = 0;
      if (neg_conp_p && *conp)
	*minus_conp = *conp, *conp = 0;
      if (neg_var_p && var)
	*minus_varp = var, var = 0;
    }
  else if (TREE_CONSTANT (in))
    *conp = in;
  else if (TREE_CODE (in) == BIT_NOT_EXPR
	   && code == PLUS_EXPR)
    {
      /* -1 - X is folded to ~X, undo that here.  Do _not_ do this
         when IN is constant.  */
      *litp = build_minus_one_cst (type);
      *minus_varp = TREE_OPERAND (in, 0);
    }
  else
    var = in;

  if (negate_p)
    {
      if (*litp)
	*minus_litp = *litp, *litp = 0;
      else if (*minus_litp)
	*litp = *minus_litp, *minus_litp = 0;
      if (*conp)
	*minus_conp = *conp, *conp = 0;
      else if (*minus_conp)
	*conp = *minus_conp, *minus_conp = 0;
      if (var)
	*minus_varp = var, var = 0;
      else if (*minus_varp)
	var = *minus_varp, *minus_varp = 0;
    }

  if (*litp
      && TREE_OVERFLOW_P (*litp))
    *litp = drop_tree_overflow (*litp);
  if (*minus_litp
      && TREE_OVERFLOW_P (*minus_litp))
    *minus_litp = drop_tree_overflow (*minus_litp);

  return var;
}

/* Re-associate trees split by the above function.  T1 and T2 are
   either expressions to associate or null.  Return the new
   expression, if any.  LOC is the location of the new expression.  If
   we build an operation, do it in TYPE and with CODE.  */

static tree
associate_trees (location_t loc, tree t1, tree t2, enum tree_code code, tree type)
{
  if (t1 == 0)
    {
      gcc_assert (t2 == 0 || code != MINUS_EXPR);
      return t2;
    }
  else if (t2 == 0)
    return t1;

  /* If either input is CODE, a PLUS_EXPR, or a MINUS_EXPR, don't
     try to fold this since we will have infinite recursion.  But do
     deal with any NEGATE_EXPRs.  */
  if (TREE_CODE (t1) == code || TREE_CODE (t2) == code
      || TREE_CODE (t1) == PLUS_EXPR || TREE_CODE (t2) == PLUS_EXPR
      || TREE_CODE (t1) == MINUS_EXPR || TREE_CODE (t2) == MINUS_EXPR)
    {
      if (code == PLUS_EXPR)
	{
	  if (TREE_CODE (t1) == NEGATE_EXPR)
	    return build2_loc (loc, MINUS_EXPR, type,
			       fold_convert_loc (loc, type, t2),
			       fold_convert_loc (loc, type,
						 TREE_OPERAND (t1, 0)));
	  else if (TREE_CODE (t2) == NEGATE_EXPR)
	    return build2_loc (loc, MINUS_EXPR, type,
			       fold_convert_loc (loc, type, t1),
			       fold_convert_loc (loc, type,
						 TREE_OPERAND (t2, 0)));
	  else if (integer_zerop (t2))
	    return fold_convert_loc (loc, type, t1);
	}
      else if (code == MINUS_EXPR)
	{
	  if (integer_zerop (t2))
	    return fold_convert_loc (loc, type, t1);
	}

      return build2_loc (loc, code, type, fold_convert_loc (loc, type, t1),
			 fold_convert_loc (loc, type, t2));
    }

  return fold_build2_loc (loc, code, type, fold_convert_loc (loc, type, t1),
			  fold_convert_loc (loc, type, t2));
}

/* Check whether TYPE1 and TYPE2 are equivalent integer types, suitable
   for use in int_const_binop, size_binop and size_diffop.  */

static bool
int_binop_types_match_p (enum tree_code code, const_tree type1, const_tree type2)
{
  if (!INTEGRAL_TYPE_P (type1) && !POINTER_TYPE_P (type1))
    return false;
  if (!INTEGRAL_TYPE_P (type2) && !POINTER_TYPE_P (type2))
    return false;

  switch (code)
    {
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
      return true;

    default:
      break;
    }

  return TYPE_UNSIGNED (type1) == TYPE_UNSIGNED (type2)
	 && TYPE_PRECISION (type1) == TYPE_PRECISION (type2)
	 && TYPE_MODE (type1) == TYPE_MODE (type2);
}

/* Combine two wide ints ARG1 and ARG2 under operation CODE to produce
   a new constant in RES.  Return FALSE if we don't know how to
   evaluate CODE at compile-time.  */

bool
wide_int_binop (wide_int &res,
		enum tree_code code, const wide_int &arg1, const wide_int &arg2,
		signop sign, wi::overflow_type *overflow)
{
  wide_int tmp;
  *overflow = wi::OVF_NONE;
  switch (code)
    {
    case BIT_IOR_EXPR:
      res = wi::bit_or (arg1, arg2);
      break;

    case BIT_XOR_EXPR:
      res = wi::bit_xor (arg1, arg2);
      break;

    case BIT_AND_EXPR:
      res = wi::bit_and (arg1, arg2);
      break;

    case LSHIFT_EXPR:
      if (wi::neg_p (arg2))
	return false;
      res = wi::lshift (arg1, arg2);
      break;

    case RSHIFT_EXPR:
      if (wi::neg_p (arg2))
	return false;
      /* It's unclear from the C standard whether shifts can overflow.
	 The following code ignores overflow; perhaps a C standard
	 interpretation ruling is needed.  */
      res = wi::rshift (arg1, arg2, sign);
      break;

    case RROTATE_EXPR:
    case LROTATE_EXPR:
      if (wi::neg_p (arg2))
	{
	  tmp = -arg2;
	  if (code == RROTATE_EXPR)
	    code = LROTATE_EXPR;
	  else
	    code = RROTATE_EXPR;
	}
      else
        tmp = arg2;

      if (code == RROTATE_EXPR)
	res = wi::rrotate (arg1, tmp);
      else
	res = wi::lrotate (arg1, tmp);
      break;

    case PLUS_EXPR:
      res = wi::add (arg1, arg2, sign, overflow);
      break;

    case MINUS_EXPR:
      res = wi::sub (arg1, arg2, sign, overflow);
      break;

    case MULT_EXPR:
      res = wi::mul (arg1, arg2, sign, overflow);
      break;

    case MULT_HIGHPART_EXPR:
      res = wi::mul_high (arg1, arg2, sign);
      break;

    case TRUNC_DIV_EXPR:
    case EXACT_DIV_EXPR:
      if (arg2 == 0)
	return false;
      res = wi::div_trunc (arg1, arg2, sign, overflow);
      break;

    case FLOOR_DIV_EXPR:
      if (arg2 == 0)
	return false;
      res = wi::div_floor (arg1, arg2, sign, overflow);
      break;

    case CEIL_DIV_EXPR:
      if (arg2 == 0)
	return false;
      res = wi::div_ceil (arg1, arg2, sign, overflow);
      break;

    case ROUND_DIV_EXPR:
      if (arg2 == 0)
	return false;
      res = wi::div_round (arg1, arg2, sign, overflow);
      break;

    case TRUNC_MOD_EXPR:
      if (arg2 == 0)
	return false;
      res = wi::mod_trunc (arg1, arg2, sign, overflow);
      break;

    case FLOOR_MOD_EXPR:
      if (arg2 == 0)
	return false;
      res = wi::mod_floor (arg1, arg2, sign, overflow);
      break;

    case CEIL_MOD_EXPR:
      if (arg2 == 0)
	return false;
      res = wi::mod_ceil (arg1, arg2, sign, overflow);
      break;

    case ROUND_MOD_EXPR:
      if (arg2 == 0)
	return false;
      res = wi::mod_round (arg1, arg2, sign, overflow);
      break;

    case MIN_EXPR:
      res = wi::min (arg1, arg2, sign);
      break;

    case MAX_EXPR:
      res = wi::max (arg1, arg2, sign);
      break;

    default:
      return false;
    }
  return true;
}

/* Returns true if we know who is smaller or equal, ARG1 or ARG2, and set the
   min value to RES.  */
bool
can_min_p (const_tree arg1, const_tree arg2, poly_wide_int &res)
{
  if (known_le (wi::to_poly_widest (arg1), wi::to_poly_widest (arg2)))
    {
      res = wi::to_poly_wide (arg1);
      return true;
    }
  else if (known_le (wi::to_poly_widest (arg2), wi::to_poly_widest (arg1)))
    {
      res = wi::to_poly_wide (arg2);
      return true;
    }

  return false;
}

/* Combine two poly int's ARG1 and ARG2 under operation CODE to
   produce a new constant in RES.  Return FALSE if we don't know how
   to evaluate CODE at compile-time.  */

bool
poly_int_binop (poly_wide_int &res, enum tree_code code,
		const_tree arg1, const_tree arg2,
		signop sign, wi::overflow_type *overflow)
{
  gcc_assert (poly_int_tree_p (arg1) && poly_int_tree_p (arg2));

  if (TREE_CODE (arg1) == INTEGER_CST && TREE_CODE (arg2) == INTEGER_CST)
    {
      wide_int warg1 = wi::to_wide (arg1), wi_res;
      wide_int warg2 = wi::to_wide (arg2, TYPE_PRECISION (TREE_TYPE (arg1)));
      if (!wide_int_binop (wi_res, code, warg1, warg2, sign, overflow))
	return NULL_TREE;
      res = wi_res;
      return true;
    }

  gcc_assert (NUM_POLY_INT_COEFFS != 1);

  switch (code)
    {
    case PLUS_EXPR:
      res = wi::add (wi::to_poly_wide (arg1),
		     wi::to_poly_wide (arg2), sign, overflow);
      break;

    case MINUS_EXPR:
      res = wi::sub (wi::to_poly_wide (arg1),
		     wi::to_poly_wide (arg2), sign, overflow);
      break;

    case MULT_EXPR:
      if (TREE_CODE (arg2) == INTEGER_CST)
	res = wi::mul (wi::to_poly_wide (arg1),
		       wi::to_wide (arg2), sign, overflow);
      else if (TREE_CODE (arg1) == INTEGER_CST)
	res = wi::mul (wi::to_poly_wide (arg2),
		       wi::to_wide (arg1), sign, overflow);
      else
	return NULL_TREE;
      break;

    case LSHIFT_EXPR:
      if (TREE_CODE (arg2) == INTEGER_CST)
	res = wi::to_poly_wide (arg1) << wi::to_wide (arg2);
      else
	return false;
      break;

    case BIT_IOR_EXPR:
      if (TREE_CODE (arg2) != INTEGER_CST
	  || !can_ior_p (wi::to_poly_wide (arg1), wi::to_wide (arg2),
			 &res))
	return false;
      break;

    case MIN_EXPR:
      if (!can_min_p (arg1, arg2, res))
	return false;
      break;

    default:
      return false;
    }
  return true;
}

/* Combine two integer constants ARG1 and ARG2 under operation CODE to
   produce a new constant.  Return NULL_TREE if we don't know how to
   evaluate CODE at compile-time.  */

tree
int_const_binop (enum tree_code code, const_tree arg1, const_tree arg2,
		 int overflowable)
{
  poly_wide_int poly_res;
  tree type = TREE_TYPE (arg1);
  signop sign = TYPE_SIGN (type);
  wi::overflow_type overflow = wi::OVF_NONE;

  if (!poly_int_tree_p (arg1)
      || !poly_int_tree_p (arg2)
      || !poly_int_binop (poly_res, code, arg1, arg2, sign, &overflow))
    return NULL_TREE;
  return force_fit_type (type, poly_res, overflowable,
			 (((sign == SIGNED || overflowable == -1)
			   && overflow)
			  | TREE_OVERFLOW (arg1) | TREE_OVERFLOW (arg2)));
}

/* Return true if binary operation OP distributes over addition in operand
   OPNO, with the other operand being held constant.  OPNO counts from 1.  */

static bool
distributes_over_addition_p (tree_code op, int opno)
{
  switch (op)
    {
    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
      return true;

    case LSHIFT_EXPR:
      return opno == 1;

    default:
      return false;
    }
}

/* OP is the INDEXth operand to CODE (counting from zero) and OTHER_OP
   is the other operand.  Try to use the value of OP to simplify the
   operation in one step, without having to process individual elements.  */
static tree
simplify_const_binop (tree_code code, tree op, tree other_op,
		      int index ATTRIBUTE_UNUSED)
{
  /* AND, IOR as well as XOR with a zerop can be simplified directly.  */
  if (TREE_CODE (op) == VECTOR_CST && TREE_CODE (other_op) == VECTOR_CST)
    {
      if (integer_zerop (other_op))
	{
	  if (code == BIT_IOR_EXPR || code == BIT_XOR_EXPR)
	    return op;
	  else if (code == BIT_AND_EXPR)
	    return other_op;
	}
    }

  return NULL_TREE;
}

/* If ARG1 and ARG2 are constants, and if performing CODE on them would
   be an elementwise vector operation, try to fold the operation to a
   constant vector, using ELT_CONST_BINOP to fold each element.  Return
   the folded value on success, otherwise return null.  */
tree
vector_const_binop (tree_code code, tree arg1, tree arg2,
		    tree (*elt_const_binop) (enum tree_code, tree, tree))
{
  if (TREE_CODE (arg1) == VECTOR_CST && TREE_CODE (arg2) == VECTOR_CST
      && known_eq (TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg1)),
		   TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg2))))
    {
      tree type = TREE_TYPE (arg1);
      bool step_ok_p;
      if (VECTOR_CST_STEPPED_P (arg1)
	  && VECTOR_CST_STEPPED_P (arg2))
      /* We can operate directly on the encoding if:

      a3 - a2 == a2 - a1 && b3 - b2 == b2 - b1
      implies
      (a3 op b3) - (a2 op b2) == (a2 op b2) - (a1 op b1)

      Addition and subtraction are the supported operators
      for which this is true.  */
	step_ok_p = (code == PLUS_EXPR || code == MINUS_EXPR);
      else if (VECTOR_CST_STEPPED_P (arg1))
      /* We can operate directly on stepped encodings if:

      a3 - a2 == a2 - a1
      implies:
      (a3 op c) - (a2 op c) == (a2 op c) - (a1 op c)

      which is true if (x -> x op c) distributes over addition.  */
	step_ok_p = distributes_over_addition_p (code, 1);
      else
      /* Similarly in reverse.  */
	step_ok_p = distributes_over_addition_p (code, 2);
      tree_vector_builder elts;
      if (!elts.new_binary_operation (type, arg1, arg2, step_ok_p))
	return NULL_TREE;
      unsigned int count = elts.encoded_nelts ();
      for (unsigned int i = 0; i < count; ++i)
	{
	  tree elem1 = VECTOR_CST_ELT (arg1, i);
	  tree elem2 = VECTOR_CST_ELT (arg2, i);

	  tree elt = elt_const_binop (code, elem1, elem2);

      /* It is possible that const_binop cannot handle the given
      code and return NULL_TREE */
	  if (elt == NULL_TREE)
	    return NULL_TREE;
	  elts.quick_push (elt);
	}

      return elts.build ();
    }

  if (TREE_CODE (arg1) == VECTOR_CST
      && TREE_CODE (arg2) == INTEGER_CST)
    {
      tree type = TREE_TYPE (arg1);
      bool step_ok_p = distributes_over_addition_p (code, 1);
      tree_vector_builder elts;
      if (!elts.new_unary_operation (type, arg1, step_ok_p))
	return NULL_TREE;
      unsigned int count = elts.encoded_nelts ();
      for (unsigned int i = 0; i < count; ++i)
	{
	  tree elem1 = VECTOR_CST_ELT (arg1, i);

	  tree elt = elt_const_binop (code, elem1, arg2);

	  /* It is possible that const_binop cannot handle the given
	     code and return NULL_TREE.  */
	  if (elt == NULL_TREE)
	    return NULL_TREE;
	  elts.quick_push (elt);
	}

      return elts.build ();
    }
  return NULL_TREE;
}

/* Combine two constants ARG1 and ARG2 under operation CODE to produce a new
   constant.  We assume ARG1 and ARG2 have the same data type, or at least
   are the same kind of constant and the same machine mode.  Return zero if
   combining the constants is not allowed in the current operating mode.  */

static tree
const_binop (enum tree_code code, tree arg1, tree arg2)
{
  /* Sanity check for the recursive cases.  */
  if (!arg1 || !arg2)
    return NULL_TREE;

  STRIP_NOPS (arg1);
  STRIP_NOPS (arg2);

  if (poly_int_tree_p (arg1) && poly_int_tree_p (arg2))
    {
      if (code == POINTER_PLUS_EXPR)
	return int_const_binop (PLUS_EXPR,
				arg1, fold_convert (TREE_TYPE (arg1), arg2));

      return int_const_binop (code, arg1, arg2);
    }

  if (TREE_CODE (arg1) == REAL_CST && TREE_CODE (arg2) == REAL_CST)
    {
      machine_mode mode;
      REAL_VALUE_TYPE d1;
      REAL_VALUE_TYPE d2;
      REAL_VALUE_TYPE value;
      REAL_VALUE_TYPE result;
      bool inexact;
      tree t, type;

      /* The following codes are handled by real_arithmetic.  */
      switch (code)
	{
	case PLUS_EXPR:
	case MINUS_EXPR:
	case MULT_EXPR:
	case RDIV_EXPR:
	case MIN_EXPR:
	case MAX_EXPR:
	  break;

	default:
	  return NULL_TREE;
	}

      d1 = TREE_REAL_CST (arg1);
      d2 = TREE_REAL_CST (arg2);

      type = TREE_TYPE (arg1);
      mode = TYPE_MODE (type);

      /* Don't perform operation if we honor signaling NaNs and
	 either operand is a signaling NaN.  */
      if (HONOR_SNANS (mode)
	  && (REAL_VALUE_ISSIGNALING_NAN (d1)
	      || REAL_VALUE_ISSIGNALING_NAN (d2)))
	return NULL_TREE;

      /* Don't perform operation if it would raise a division
	 by zero exception.  */
      if (code == RDIV_EXPR
	  && real_equal (&d2, &dconst0)
	  && (flag_trapping_math || ! MODE_HAS_INFINITIES (mode)))
	return NULL_TREE;

      /* If either operand is a NaN, just return it.  Otherwise, set up
	 for floating-point trap; we return an overflow.  */
      if (REAL_VALUE_ISNAN (d1))
      {
	/* Make resulting NaN value to be qNaN when flag_signaling_nans
	   is off.  */
	d1.signalling = 0;
	t = build_real (type, d1);
	return t;
      }
      else if (REAL_VALUE_ISNAN (d2))
      {
	/* Make resulting NaN value to be qNaN when flag_signaling_nans
	   is off.  */
	d2.signalling = 0;
	t = build_real (type, d2);
	return t;
      }

      inexact = real_arithmetic (&value, code, &d1, &d2);
      real_convert (&result, mode, &value);

      /* Don't constant fold this floating point operation if
	 both operands are not NaN but the result is NaN, and
	 flag_trapping_math.  Such operations should raise an
	 invalid operation exception.  */
      if (flag_trapping_math
	  && MODE_HAS_NANS (mode)
	  && REAL_VALUE_ISNAN (result)
	  && !REAL_VALUE_ISNAN (d1)
	  && !REAL_VALUE_ISNAN (d2))
	return NULL_TREE;

      /* Don't constant fold this floating point operation if
	 the result has overflowed and flag_trapping_math.  */
      if (flag_trapping_math
	  && MODE_HAS_INFINITIES (mode)
	  && REAL_VALUE_ISINF (result)
	  && !REAL_VALUE_ISINF (d1)
	  && !REAL_VALUE_ISINF (d2))
	return NULL_TREE;

      /* Don't constant fold this floating point operation if the
	 result may dependent upon the run-time rounding mode and
	 flag_rounding_math is set, or if GCC's software emulation
	 is unable to accurately represent the result.  */
      if ((flag_rounding_math
	   || (MODE_COMPOSITE_P (mode) && !flag_unsafe_math_optimizations))
	  && (inexact || !real_identical (&result, &value)))
	return NULL_TREE;

      t = build_real (type, result);

      TREE_OVERFLOW (t) = TREE_OVERFLOW (arg1) | TREE_OVERFLOW (arg2);
      return t;
    }

  if (TREE_CODE (arg1) == FIXED_CST)
    {
      FIXED_VALUE_TYPE f1;
      FIXED_VALUE_TYPE f2;
      FIXED_VALUE_TYPE result;
      tree t, type;
      bool sat_p;
      bool overflow_p;

      /* The following codes are handled by fixed_arithmetic.  */
      switch (code)
        {
	case PLUS_EXPR:
	case MINUS_EXPR:
	case MULT_EXPR:
	case TRUNC_DIV_EXPR:
	  if (TREE_CODE (arg2) != FIXED_CST)
	    return NULL_TREE;
	  f2 = TREE_FIXED_CST (arg2);
	  break;

	case LSHIFT_EXPR:
	case RSHIFT_EXPR:
	  {
	    if (TREE_CODE (arg2) != INTEGER_CST)
	      return NULL_TREE;
	    wi::tree_to_wide_ref w2 = wi::to_wide (arg2);
	    f2.data.high = w2.elt (1);
	    f2.data.low = w2.ulow ();
	    f2.mode = SImode;
	  }
	  break;

        default:
	  return NULL_TREE;
        }

      f1 = TREE_FIXED_CST (arg1);
      type = TREE_TYPE (arg1);
      sat_p = TYPE_SATURATING (type);
      overflow_p = fixed_arithmetic (&result, code, &f1, &f2, sat_p);
      t = build_fixed (type, result);
      /* Propagate overflow flags.  */
      if (overflow_p | TREE_OVERFLOW (arg1) | TREE_OVERFLOW (arg2))
	TREE_OVERFLOW (t) = 1;
      return t;
    }

  if (TREE_CODE (arg1) == COMPLEX_CST && TREE_CODE (arg2) == COMPLEX_CST)
    {
      tree type = TREE_TYPE (arg1);
      tree r1 = TREE_REALPART (arg1);
      tree i1 = TREE_IMAGPART (arg1);
      tree r2 = TREE_REALPART (arg2);
      tree i2 = TREE_IMAGPART (arg2);
      tree real, imag;

      switch (code)
	{
	case PLUS_EXPR:
	case MINUS_EXPR:
	  real = const_binop (code, r1, r2);
	  imag = const_binop (code, i1, i2);
	  break;

	case MULT_EXPR:
	  if (COMPLEX_FLOAT_TYPE_P (type))
	    return do_mpc_arg2 (arg1, arg2, type,
				/* do_nonfinite= */ folding_initializer,
				mpc_mul);

	  real = const_binop (MINUS_EXPR,
			      const_binop (MULT_EXPR, r1, r2),
			      const_binop (MULT_EXPR, i1, i2));
	  imag = const_binop (PLUS_EXPR,
			      const_binop (MULT_EXPR, r1, i2),
			      const_binop (MULT_EXPR, i1, r2));
	  break;

	case RDIV_EXPR:
	  if (COMPLEX_FLOAT_TYPE_P (type))
	    return do_mpc_arg2 (arg1, arg2, type,
                                /* do_nonfinite= */ folding_initializer,
				mpc_div);
	  /* Fallthru. */
	case TRUNC_DIV_EXPR:
	case CEIL_DIV_EXPR:
	case FLOOR_DIV_EXPR:
	case ROUND_DIV_EXPR:
	  if (flag_complex_method == 0)
	  {
	    /* Keep this algorithm in sync with
	       tree-complex.cc:expand_complex_div_straight().

	       Expand complex division to scalars, straightforward algorithm.
	       a / b = ((ar*br + ai*bi)/t) + i((ai*br - ar*bi)/t)
	       t = br*br + bi*bi
	    */
	    tree magsquared
	      = const_binop (PLUS_EXPR,
			     const_binop (MULT_EXPR, r2, r2),
			     const_binop (MULT_EXPR, i2, i2));
	    tree t1
	      = const_binop (PLUS_EXPR,
			     const_binop (MULT_EXPR, r1, r2),
			     const_binop (MULT_EXPR, i1, i2));
	    tree t2
	      = const_binop (MINUS_EXPR,
			     const_binop (MULT_EXPR, i1, r2),
			     const_binop (MULT_EXPR, r1, i2));

	    real = const_binop (code, t1, magsquared);
	    imag = const_binop (code, t2, magsquared);
	  }
	  else
	  {
	    /* Keep this algorithm in sync with
               tree-complex.cc:expand_complex_div_wide().

	       Expand complex division to scalars, modified algorithm to minimize
	       overflow with wide input ranges.  */
	    tree compare = fold_build2 (LT_EXPR, boolean_type_node,
					fold_abs_const (r2, TREE_TYPE (type)),
					fold_abs_const (i2, TREE_TYPE (type)));

	    if (integer_nonzerop (compare))
	      {
		/* In the TRUE branch, we compute
		   ratio = br/bi;
		   div = (br * ratio) + bi;
		   tr = (ar * ratio) + ai;
		   ti = (ai * ratio) - ar;
		   tr = tr / div;
		   ti = ti / div;  */
		tree ratio = const_binop (code, r2, i2);
		tree div = const_binop (PLUS_EXPR, i2,
					const_binop (MULT_EXPR, r2, ratio));
		real = const_binop (MULT_EXPR, r1, ratio);
		real = const_binop (PLUS_EXPR, real, i1);
		real = const_binop (code, real, div);

		imag = const_binop (MULT_EXPR, i1, ratio);
		imag = const_binop (MINUS_EXPR, imag, r1);
		imag = const_binop (code, imag, div);
	      }
	    else
	      {
		/* In the FALSE branch, we compute
		   ratio = d/c;
		   divisor = (d * ratio) + c;
		   tr = (b * ratio) + a;
		   ti = b - (a * ratio);
		   tr = tr / div;
		   ti = ti / div;  */
		tree ratio = const_binop (code, i2, r2);
		tree div = const_binop (PLUS_EXPR, r2,
                                        const_binop (MULT_EXPR, i2, ratio));

		real = const_binop (MULT_EXPR, i1, ratio);
		real = const_binop (PLUS_EXPR, real, r1);
		real = const_binop (code, real, div);

		imag = const_binop (MULT_EXPR, r1, ratio);
		imag = const_binop (MINUS_EXPR, i1, imag);
		imag = const_binop (code, imag, div);
	      }
	  }
	  break;

	default:
	  return NULL_TREE;
	}

      if (real && imag)
	return build_complex (type, real, imag);
    }

  tree simplified;
  if ((simplified = simplify_const_binop (code, arg1, arg2, 0)))
    return simplified;

  if (commutative_tree_code (code)
      && (simplified = simplify_const_binop (code, arg2, arg1, 1)))
    return simplified;

  return vector_const_binop (code, arg1, arg2, const_binop);
}

/* Overload that adds a TYPE parameter to be able to dispatch
   to fold_relational_const.  */

tree
const_binop (enum tree_code code, tree type, tree arg1, tree arg2)
{
  if (TREE_CODE_CLASS (code) == tcc_comparison)
    return fold_relational_const (code, type, arg1, arg2);

  /* ???  Until we make the const_binop worker take the type of the
     result as argument put those cases that need it here.  */
  switch (code)
    {
    case VEC_SERIES_EXPR:
      if (CONSTANT_CLASS_P (arg1)
	  && CONSTANT_CLASS_P (arg2))
	return build_vec_series (type, arg1, arg2);
      return NULL_TREE;

    case COMPLEX_EXPR:
      if ((TREE_CODE (arg1) == REAL_CST
	   && TREE_CODE (arg2) == REAL_CST)
	  || (TREE_CODE (arg1) == INTEGER_CST
	      && TREE_CODE (arg2) == INTEGER_CST))
	return build_complex (type, arg1, arg2);
      return NULL_TREE;

    case POINTER_DIFF_EXPR:
      if (poly_int_tree_p (arg1) && poly_int_tree_p (arg2))
	{
	  poly_offset_int res = (wi::to_poly_offset (arg1)
				 - wi::to_poly_offset (arg2));
	  return force_fit_type (type, res, 1,
				 TREE_OVERFLOW (arg1) | TREE_OVERFLOW (arg2));
	}
      return NULL_TREE;

    case VEC_PACK_TRUNC_EXPR:
    case VEC_PACK_FIX_TRUNC_EXPR:
    case VEC_PACK_FLOAT_EXPR:
      {
	unsigned int HOST_WIDE_INT out_nelts, in_nelts, i;

	if (TREE_CODE (arg1) != VECTOR_CST
	    || TREE_CODE (arg2) != VECTOR_CST)
	  return NULL_TREE;

	if (!VECTOR_CST_NELTS (arg1).is_constant (&in_nelts))
	  return NULL_TREE;

	out_nelts = in_nelts * 2;
	gcc_assert (known_eq (in_nelts, VECTOR_CST_NELTS (arg2))
		    && known_eq (out_nelts, TYPE_VECTOR_SUBPARTS (type)));

	tree_vector_builder elts (type, out_nelts, 1);
	for (i = 0; i < out_nelts; i++)
	  {
	    tree elt = (i < in_nelts
			? VECTOR_CST_ELT (arg1, i)
			: VECTOR_CST_ELT (arg2, i - in_nelts));
	    elt = fold_convert_const (code == VEC_PACK_TRUNC_EXPR
				      ? NOP_EXPR
				      : code == VEC_PACK_FLOAT_EXPR
				      ? FLOAT_EXPR : FIX_TRUNC_EXPR,
				      TREE_TYPE (type), elt);
	    if (elt == NULL_TREE || !CONSTANT_CLASS_P (elt))
	      return NULL_TREE;
	    elts.quick_push (elt);
	  }

	return elts.build ();
      }

    case VEC_WIDEN_MULT_LO_EXPR:
    case VEC_WIDEN_MULT_HI_EXPR:
    case VEC_WIDEN_MULT_EVEN_EXPR:
    case VEC_WIDEN_MULT_ODD_EXPR:
      {
	unsigned HOST_WIDE_INT out_nelts, in_nelts, out, ofs, scale;

	if (TREE_CODE (arg1) != VECTOR_CST || TREE_CODE (arg2) != VECTOR_CST)
	  return NULL_TREE;

	if (!VECTOR_CST_NELTS (arg1).is_constant (&in_nelts))
	  return NULL_TREE;
	out_nelts = in_nelts / 2;
	gcc_assert (known_eq (in_nelts, VECTOR_CST_NELTS (arg2))
		    && known_eq (out_nelts, TYPE_VECTOR_SUBPARTS (type)));

	if (code == VEC_WIDEN_MULT_LO_EXPR)
	  scale = 0, ofs = BYTES_BIG_ENDIAN ? out_nelts : 0;
	else if (code == VEC_WIDEN_MULT_HI_EXPR)
	  scale = 0, ofs = BYTES_BIG_ENDIAN ? 0 : out_nelts;
	else if (code == VEC_WIDEN_MULT_EVEN_EXPR)
	  scale = 1, ofs = 0;
	else /* if (code == VEC_WIDEN_MULT_ODD_EXPR) */
	  scale = 1, ofs = 1;

	tree_vector_builder elts (type, out_nelts, 1);
	for (out = 0; out < out_nelts; out++)
	  {
	    unsigned int in = (out << scale) + ofs;
	    tree t1 = fold_convert_const (NOP_EXPR, TREE_TYPE (type),
					  VECTOR_CST_ELT (arg1, in));
	    tree t2 = fold_convert_const (NOP_EXPR, TREE_TYPE (type),
					  VECTOR_CST_ELT (arg2, in));

	    if (t1 == NULL_TREE || t2 == NULL_TREE)
	      return NULL_TREE;
	    tree elt = const_binop (MULT_EXPR, t1, t2);
	    if (elt == NULL_TREE || !CONSTANT_CLASS_P (elt))
	      return NULL_TREE;
	    elts.quick_push (elt);
	  }

	return elts.build ();
      }

    default:;
    }

  if (TREE_CODE_CLASS (code) != tcc_binary)
    return NULL_TREE;

  /* Make sure type and arg0 have the same saturating flag.  */
  gcc_checking_assert (TYPE_SATURATING (type)
		       == TYPE_SATURATING (TREE_TYPE (arg1)));

  return const_binop (code, arg1, arg2);
}

/* Compute CODE ARG1 with resulting type TYPE with ARG1 being constant.
   Return zero if computing the constants is not possible.  */

tree
const_unop (enum tree_code code, tree type, tree arg0)
{
  /* Don't perform the operation, other than NEGATE and ABS, if
     flag_signaling_nans is on and the operand is a signaling NaN.  */
  if (TREE_CODE (arg0) == REAL_CST
      && HONOR_SNANS (arg0)
      && REAL_VALUE_ISSIGNALING_NAN (TREE_REAL_CST (arg0))
      && code != NEGATE_EXPR
      && code != ABS_EXPR
      && code != ABSU_EXPR)
    return NULL_TREE;

  switch (code)
    {
    CASE_CONVERT:
    case FLOAT_EXPR:
    case FIX_TRUNC_EXPR:
    case FIXED_CONVERT_EXPR:
      return fold_convert_const (code, type, arg0);

    case ADDR_SPACE_CONVERT_EXPR:
      /* If the source address is 0, and the source address space
	 cannot have a valid object at 0, fold to dest type null.  */
      if (integer_zerop (arg0)
	  && !(targetm.addr_space.zero_address_valid
	       (TYPE_ADDR_SPACE (TREE_TYPE (TREE_TYPE (arg0))))))
	return fold_convert_const (code, type, arg0);
      break;

    case VIEW_CONVERT_EXPR:
      return fold_view_convert_expr (type, arg0);

    case NEGATE_EXPR:
      {
	/* Can't call fold_negate_const directly here as that doesn't
	   handle all cases and we might not be able to negate some
	   constants.  */
	tree tem = fold_negate_expr (UNKNOWN_LOCATION, arg0);
	if (tem && CONSTANT_CLASS_P (tem))
	  return tem;
	break;
      }

    case ABS_EXPR:
    case ABSU_EXPR:
      if (TREE_CODE (arg0) == INTEGER_CST || TREE_CODE (arg0) == REAL_CST)
	return fold_abs_const (arg0, type);
      break;

    case CONJ_EXPR:
      if (TREE_CODE (arg0) == COMPLEX_CST)
	{
	  tree ipart = fold_negate_const (TREE_IMAGPART (arg0),
					  TREE_TYPE (type));
	  return build_complex (type, TREE_REALPART (arg0), ipart);
	}
      break;

    case BIT_NOT_EXPR:
      if (TREE_CODE (arg0) == INTEGER_CST)
	return fold_not_const (arg0, type);
      else if (POLY_INT_CST_P (arg0))
	return wide_int_to_tree (type, -poly_int_cst_value (arg0));
      /* Perform BIT_NOT_EXPR on each element individually.  */
      else if (TREE_CODE (arg0) == VECTOR_CST)
	{
	  tree elem;

	  /* This can cope with stepped encodings because ~x == -1 - x.  */
	  tree_vector_builder elements;
	  elements.new_unary_operation (type, arg0, true);
	  unsigned int i, count = elements.encoded_nelts ();
	  for (i = 0; i < count; ++i)
	    {
	      elem = VECTOR_CST_ELT (arg0, i);
	      elem = const_unop (BIT_NOT_EXPR, TREE_TYPE (type), elem);
	      if (elem == NULL_TREE)
		break;
	      elements.quick_push (elem);
	    }
	  if (i == count)
	    return elements.build ();
	}
      break;

    case TRUTH_NOT_EXPR:
      if (TREE_CODE (arg0) == INTEGER_CST)
	return constant_boolean_node (integer_zerop (arg0), type);
      break;

    case REALPART_EXPR:
      if (TREE_CODE (arg0) == COMPLEX_CST)
	return fold_convert (type, TREE_REALPART (arg0));
      break;

    case IMAGPART_EXPR:
      if (TREE_CODE (arg0) == COMPLEX_CST)
	return fold_convert (type, TREE_IMAGPART (arg0));
      break;

    case VEC_UNPACK_LO_EXPR:
    case VEC_UNPACK_HI_EXPR:
    case VEC_UNPACK_FLOAT_LO_EXPR:
    case VEC_UNPACK_FLOAT_HI_EXPR:
    case VEC_UNPACK_FIX_TRUNC_LO_EXPR:
    case VEC_UNPACK_FIX_TRUNC_HI_EXPR:
      {
	unsigned HOST_WIDE_INT out_nelts, in_nelts, i;
	enum tree_code subcode;

	if (TREE_CODE (arg0) != VECTOR_CST)
	  return NULL_TREE;

	if (!VECTOR_CST_NELTS (arg0).is_constant (&in_nelts))
	  return NULL_TREE;
	out_nelts = in_nelts / 2;
	gcc_assert (known_eq (out_nelts, TYPE_VECTOR_SUBPARTS (type)));

	unsigned int offset = 0;
	if ((!BYTES_BIG_ENDIAN) ^ (code == VEC_UNPACK_LO_EXPR
				   || code == VEC_UNPACK_FLOAT_LO_EXPR
				   || code == VEC_UNPACK_FIX_TRUNC_LO_EXPR))
	  offset = out_nelts;

	if (code == VEC_UNPACK_LO_EXPR || code == VEC_UNPACK_HI_EXPR)
	  subcode = NOP_EXPR;
	else if (code == VEC_UNPACK_FLOAT_LO_EXPR
		 || code == VEC_UNPACK_FLOAT_HI_EXPR)
	  subcode = FLOAT_EXPR;
	else
	  subcode = FIX_TRUNC_EXPR;

	tree_vector_builder elts (type, out_nelts, 1);
	for (i = 0; i < out_nelts; i++)
	  {
	    tree elt = fold_convert_const (subcode, TREE_TYPE (type),
					   VECTOR_CST_ELT (arg0, i + offset));
	    if (elt == NULL_TREE || !CONSTANT_CLASS_P (elt))
	      return NULL_TREE;
	    elts.quick_push (elt);
	  }

	return elts.build ();
      }

    case VEC_DUPLICATE_EXPR:
      if (CONSTANT_CLASS_P (arg0))
	return build_vector_from_val (type, arg0);
      return NULL_TREE;

    default:
      break;
    }

  return NULL_TREE;
}

/* Create a sizetype INT_CST node with NUMBER sign extended.  KIND
   indicates which particular sizetype to create.  */

tree
size_int_kind (poly_int64 number, enum size_type_kind kind)
{
  return build_int_cst (sizetype_tab[(int) kind], number);
}

/* Combine operands OP1 and OP2 with arithmetic operation CODE.  CODE
   is a tree code.  The type of the result is taken from the operands.
   Both must be equivalent integer types, ala int_binop_types_match_p.
   If the operands are constant, so is the result.  */

tree
size_binop_loc (location_t loc, enum tree_code code, tree arg0, tree arg1)
{
  tree type = TREE_TYPE (arg0);

  if (arg0 == error_mark_node || arg1 == error_mark_node)
    return error_mark_node;

  gcc_assert (int_binop_types_match_p (code, TREE_TYPE (arg0),
                                       TREE_TYPE (arg1)));

  /* Handle the special case of two poly_int constants faster.  */
  if (poly_int_tree_p (arg0) && poly_int_tree_p (arg1))
    {
      /* And some specific cases even faster than that.  */
      if (code == PLUS_EXPR)
	{
	  if (integer_zerop (arg0)
	      && !TREE_OVERFLOW (tree_strip_any_location_wrapper (arg0)))
	    return arg1;
	  if (integer_zerop (arg1)
	      && !TREE_OVERFLOW (tree_strip_any_location_wrapper (arg1)))
	    return arg0;
	}
      else if (code == MINUS_EXPR)
	{
	  if (integer_zerop (arg1)
	      && !TREE_OVERFLOW (tree_strip_any_location_wrapper (arg1)))
	    return arg0;
	}
      else if (code == MULT_EXPR)
	{
	  if (integer_onep (arg0)
	      && !TREE_OVERFLOW (tree_strip_any_location_wrapper (arg0)))
	    return arg1;
	}

      /* Handle general case of two integer constants.  For sizetype
         constant calculations we always want to know about overflow,
	 even in the unsigned case.  */
      tree res = int_const_binop (code, arg0, arg1, -1);
      if (res != NULL_TREE)
	return res;
    }

  return fold_build2_loc (loc, code, type, arg0, arg1);
}

/* Given two values, either both of sizetype or both of bitsizetype,
   compute the difference between the two values.  Return the value
   in signed type corresponding to the type of the operands.  */

tree
size_diffop_loc (location_t loc, tree arg0, tree arg1)
{
  tree type = TREE_TYPE (arg0);
  tree ctype;

  gcc_assert (int_binop_types_match_p (MINUS_EXPR, TREE_TYPE (arg0),
				       TREE_TYPE (arg1)));

  /* If the type is already signed, just do the simple thing.  */
  if (!TYPE_UNSIGNED (type))
    return size_binop_loc (loc, MINUS_EXPR, arg0, arg1);

  if (type == sizetype)
    ctype = ssizetype;
  else if (type == bitsizetype)
    ctype = sbitsizetype;
  else
    ctype = signed_type_for (type);

  /* If either operand is not a constant, do the conversions to the signed
     type and subtract.  The hardware will do the right thing with any
     overflow in the subtraction.  */
  if (TREE_CODE (arg0) != INTEGER_CST || TREE_CODE (arg1) != INTEGER_CST)
    return size_binop_loc (loc, MINUS_EXPR,
			   fold_convert_loc (loc, ctype, arg0),
			   fold_convert_loc (loc, ctype, arg1));

  /* If ARG0 is larger than ARG1, subtract and return the result in CTYPE.
     Otherwise, subtract the other way, convert to CTYPE (we know that can't
     overflow) and negate (which can't either).  Special-case a result
     of zero while we're here.  */
  if (tree_int_cst_equal (arg0, arg1))
    return build_int_cst (ctype, 0);
  else if (tree_int_cst_lt (arg1, arg0))
    return fold_convert_loc (loc, ctype,
			     size_binop_loc (loc, MINUS_EXPR, arg0, arg1));
  else
    return size_binop_loc (loc, MINUS_EXPR, build_int_cst (ctype, 0),
			   fold_convert_loc (loc, ctype,
					     size_binop_loc (loc,
							     MINUS_EXPR,
							     arg1, arg0)));
}

/* A subroutine of fold_convert_const handling conversions of an
   INTEGER_CST to another integer type.  */

static tree
fold_convert_const_int_from_int (tree type, const_tree arg1)
{
  /* Given an integer constant, make new constant with new type,
     appropriately sign-extended or truncated.  Use widest_int
     so that any extension is done according ARG1's type.  */
  tree arg1_type = TREE_TYPE (arg1);
  unsigned prec = MAX (TYPE_PRECISION (arg1_type), TYPE_PRECISION (type));
  return force_fit_type (type, wide_int::from (wi::to_wide (arg1), prec,
					       TYPE_SIGN (arg1_type)),
			 !POINTER_TYPE_P (TREE_TYPE (arg1)),
			 TREE_OVERFLOW (arg1));
}

/* A subroutine of fold_convert_const handling conversions a REAL_CST
   to an integer type.  */

static tree
fold_convert_const_int_from_real (enum tree_code code, tree type, const_tree arg1)
{
  bool overflow = false;
  tree t;

  /* The following code implements the floating point to integer
     conversion rules required by the Java Language Specification,
     that IEEE NaNs are mapped to zero and values that overflow
     the target precision saturate, i.e. values greater than
     INT_MAX are mapped to INT_MAX, and values less than INT_MIN
     are mapped to INT_MIN.  These semantics are allowed by the
     C and C++ standards that simply state that the behavior of
     FP-to-integer conversion is unspecified upon overflow.  */

  wide_int val;
  REAL_VALUE_TYPE r;
  REAL_VALUE_TYPE x = TREE_REAL_CST (arg1);

  switch (code)
    {
    case FIX_TRUNC_EXPR:
      real_trunc (&r, VOIDmode, &x);
      break;

    default:
      gcc_unreachable ();
    }

  /* If R is NaN, return zero and show we have an overflow.  */
  if (REAL_VALUE_ISNAN (r))
    {
      overflow = true;
      val = wi::zero (TYPE_PRECISION (type));
    }

  /* See if R is less than the lower bound or greater than the
     upper bound.  */

  if (! overflow)
    {
      tree lt = TYPE_MIN_VALUE (type);
      REAL_VALUE_TYPE l = real_value_from_int_cst (NULL_TREE, lt);
      if (real_less (&r, &l))
	{
	  overflow = true;
	  val = wi::to_wide (lt);
	}
    }

  if (! overflow)
    {
      tree ut = TYPE_MAX_VALUE (type);
      if (ut)
	{
	  REAL_VALUE_TYPE u = real_value_from_int_cst (NULL_TREE, ut);
	  if (real_less (&u, &r))
	    {
	      overflow = true;
	      val = wi::to_wide (ut);
	    }
	}
    }

  if (! overflow)
    val = real_to_integer (&r, &overflow, TYPE_PRECISION (type));

  /* According to IEEE standard, for conversions from floating point to
     integer. When a NaN or infinite operand cannot be represented in the
     destination format and this cannot otherwise be indicated, the invalid
     operation exception shall be signaled. When a numeric operand would
     convert to an integer outside the range of the destination format, the
     invalid operation exception shall be signaled if this situation cannot
     otherwise be indicated.  */
  if (!flag_trapping_math || !overflow)
    t = force_fit_type (type, val, -1, overflow | TREE_OVERFLOW (arg1));
  else
    t = NULL_TREE;

  return t;
}

/* A subroutine of fold_convert_const handling conversions of a
   FIXED_CST to an integer type.  */

static tree
fold_convert_const_int_from_fixed (tree type, const_tree arg1)
{
  tree t;
  double_int temp, temp_trunc;
  scalar_mode mode;

  /* Right shift FIXED_CST to temp by fbit.  */
  temp = TREE_FIXED_CST (arg1).data;
  mode = TREE_FIXED_CST (arg1).mode;
  if (GET_MODE_FBIT (mode) < HOST_BITS_PER_DOUBLE_INT)
    {
      temp = temp.rshift (GET_MODE_FBIT (mode),
			  HOST_BITS_PER_DOUBLE_INT,
			  SIGNED_FIXED_POINT_MODE_P (mode));

      /* Left shift temp to temp_trunc by fbit.  */
      temp_trunc = temp.lshift (GET_MODE_FBIT (mode),
				HOST_BITS_PER_DOUBLE_INT,
				SIGNED_FIXED_POINT_MODE_P (mode));
    }
  else
    {
      temp = double_int_zero;
      temp_trunc = double_int_zero;
    }

  /* If FIXED_CST is negative, we need to round the value toward 0.
     By checking if the fractional bits are not zero to add 1 to temp.  */
  if (SIGNED_FIXED_POINT_MODE_P (mode)
      && temp_trunc.is_negative ()
      && TREE_FIXED_CST (arg1).data != temp_trunc)
    temp += double_int_one;

  /* Given a fixed-point constant, make new constant with new type,
     appropriately sign-extended or truncated.  */
  t = force_fit_type (type, temp, -1,
		      (temp.is_negative ()
		       && (TYPE_UNSIGNED (type)
			   < TYPE_UNSIGNED (TREE_TYPE (arg1))))
		      | TREE_OVERFLOW (arg1));

  return t;
}

/* A subroutine of fold_convert_const handling conversions a REAL_CST
   to another floating point type.  */

static tree
fold_convert_const_real_from_real (tree type, const_tree arg1)
{
  REAL_VALUE_TYPE value;
  tree t;

  /* If the underlying modes are the same, simply treat it as
     copy and rebuild with TREE_REAL_CST information and the
     given type.  */
  if (TYPE_MODE (type) == TYPE_MODE (TREE_TYPE (arg1)))
    {
      t = build_real (type, TREE_REAL_CST (arg1));
      return t;
    }

  /* Don't perform the operation if flag_signaling_nans is on
     and the operand is a signaling NaN.  */
  if (HONOR_SNANS (arg1)
      && REAL_VALUE_ISSIGNALING_NAN (TREE_REAL_CST (arg1)))
    return NULL_TREE; 

  /* With flag_rounding_math we should respect the current rounding mode
     unless the conversion is exact.  */
  if (HONOR_SIGN_DEPENDENT_ROUNDING (arg1)
      && !exact_real_truncate (TYPE_MODE (type), &TREE_REAL_CST (arg1)))
    return NULL_TREE;

  real_convert (&value, TYPE_MODE (type), &TREE_REAL_CST (arg1));
  t = build_real (type, value);

  /* If converting an infinity or NAN to a representation that doesn't
     have one, set the overflow bit so that we can produce some kind of
     error message at the appropriate point if necessary.  It's not the
     most user-friendly message, but it's better than nothing.  */
  if (REAL_VALUE_ISINF (TREE_REAL_CST (arg1))
      && !MODE_HAS_INFINITIES (TYPE_MODE (type)))
    TREE_OVERFLOW (t) = 1;
  else if (REAL_VALUE_ISNAN (TREE_REAL_CST (arg1))
	   && !MODE_HAS_NANS (TYPE_MODE (type)))
    TREE_OVERFLOW (t) = 1;
  /* Regular overflow, conversion produced an infinity in a mode that
     can't represent them.  */
  else if (!MODE_HAS_INFINITIES (TYPE_MODE (type))
	   && REAL_VALUE_ISINF (value)
	   && !REAL_VALUE_ISINF (TREE_REAL_CST (arg1)))
    TREE_OVERFLOW (t) = 1;
  else
    TREE_OVERFLOW (t) = TREE_OVERFLOW (arg1);
  return t;
}

/* A subroutine of fold_convert_const handling conversions a FIXED_CST
   to a floating point type.  */

static tree
fold_convert_const_real_from_fixed (tree type, const_tree arg1)
{
  REAL_VALUE_TYPE value;
  tree t;

  real_convert_from_fixed (&value, SCALAR_FLOAT_TYPE_MODE (type),
			   &TREE_FIXED_CST (arg1));
  t = build_real (type, value);

  TREE_OVERFLOW (t) = TREE_OVERFLOW (arg1);
  return t;
}

/* A subroutine of fold_convert_const handling conversions a FIXED_CST
   to another fixed-point type.  */

static tree
fold_convert_const_fixed_from_fixed (tree type, const_tree arg1)
{
  FIXED_VALUE_TYPE value;
  tree t;
  bool overflow_p;

  overflow_p = fixed_convert (&value, SCALAR_TYPE_MODE (type),
			      &TREE_FIXED_CST (arg1), TYPE_SATURATING (type));
  t = build_fixed (type, value);

  /* Propagate overflow flags.  */
  if (overflow_p | TREE_OVERFLOW (arg1))
    TREE_OVERFLOW (t) = 1;
  return t;
}

/* A subroutine of fold_convert_const handling conversions an INTEGER_CST
   to a fixed-point type.  */

static tree
fold_convert_const_fixed_from_int (tree type, const_tree arg1)
{
  FIXED_VALUE_TYPE value;
  tree t;
  bool overflow_p;
  double_int di;

  gcc_assert (TREE_INT_CST_NUNITS (arg1) <= 2);

  di.low = TREE_INT_CST_ELT (arg1, 0);
  if (TREE_INT_CST_NUNITS (arg1) == 1)
    di.high = (HOST_WIDE_INT) di.low < 0 ? HOST_WIDE_INT_M1 : 0;
  else
    di.high = TREE_INT_CST_ELT (arg1, 1);

  overflow_p = fixed_convert_from_int (&value, SCALAR_TYPE_MODE (type), di,
				       TYPE_UNSIGNED (TREE_TYPE (arg1)),
				       TYPE_SATURATING (type));
  t = build_fixed (type, value);

  /* Propagate overflow flags.  */
  if (overflow_p | TREE_OVERFLOW (arg1))
    TREE_OVERFLOW (t) = 1;
  return t;
}

/* A subroutine of fold_convert_const handling conversions a REAL_CST
   to a fixed-point type.  */

static tree
fold_convert_const_fixed_from_real (tree type, const_tree arg1)
{
  FIXED_VALUE_TYPE value;
  tree t;
  bool overflow_p;

  overflow_p = fixed_convert_from_real (&value, SCALAR_TYPE_MODE (type),
					&TREE_REAL_CST (arg1),
					TYPE_SATURATING (type));
  t = build_fixed (type, value);

  /* Propagate overflow flags.  */
  if (overflow_p | TREE_OVERFLOW (arg1))
    TREE_OVERFLOW (t) = 1;
  return t;
}

/* Attempt to fold type conversion operation CODE of expression ARG1 to
   type TYPE.  If no simplification can be done return NULL_TREE.  */

static tree
fold_convert_const (enum tree_code code, tree type, tree arg1)
{
  tree arg_type = TREE_TYPE (arg1);
  if (arg_type == type)
    return arg1;

  /* We can't widen types, since the runtime value could overflow the
     original type before being extended to the new type.  */
  if (POLY_INT_CST_P (arg1)
      && (POINTER_TYPE_P (type) || INTEGRAL_TYPE_P (type))
      && TYPE_PRECISION (type) <= TYPE_PRECISION (arg_type))
    return build_poly_int_cst (type,
			       poly_wide_int::from (poly_int_cst_value (arg1),
						    TYPE_PRECISION (type),
						    TYPE_SIGN (arg_type)));

  if (POINTER_TYPE_P (type) || INTEGRAL_TYPE_P (type)
      || TREE_CODE (type) == OFFSET_TYPE)
    {
      if (TREE_CODE (arg1) == INTEGER_CST)
	return fold_convert_const_int_from_int (type, arg1);
      else if (TREE_CODE (arg1) == REAL_CST)
	return fold_convert_const_int_from_real (code, type, arg1);
      else if (TREE_CODE (arg1) == FIXED_CST)
	return fold_convert_const_int_from_fixed (type, arg1);
    }
  else if (SCALAR_FLOAT_TYPE_P (type))
    {
      if (TREE_CODE (arg1) == INTEGER_CST)
	{
	  tree res = build_real_from_int_cst (type, arg1);
	  /* Avoid the folding if flag_rounding_math is on and the
	     conversion is not exact.  */
	  if (HONOR_SIGN_DEPENDENT_ROUNDING (type))
	    {
	      bool fail = false;
	      wide_int w = real_to_integer (&TREE_REAL_CST (res), &fail,
					    TYPE_PRECISION (TREE_TYPE (arg1)));
	      if (fail || wi::ne_p (w, wi::to_wide (arg1)))
		return NULL_TREE;
	    }
	  return res;
	}
      else if (TREE_CODE (arg1) == REAL_CST)
	return fold_convert_const_real_from_real (type, arg1);
      else if (TREE_CODE (arg1) == FIXED_CST)
	return fold_convert_const_real_from_fixed (type, arg1);
    }
  else if (FIXED_POINT_TYPE_P (type))
    {
      if (TREE_CODE (arg1) == FIXED_CST)
	return fold_convert_const_fixed_from_fixed (type, arg1);
      else if (TREE_CODE (arg1) == INTEGER_CST)
	return fold_convert_const_fixed_from_int (type, arg1);
      else if (TREE_CODE (arg1) == REAL_CST)
	return fold_convert_const_fixed_from_real (type, arg1);
    }
  else if (VECTOR_TYPE_P (type))
    {
      if (TREE_CODE (arg1) == VECTOR_CST
	  && known_eq (TYPE_VECTOR_SUBPARTS (type), VECTOR_CST_NELTS (arg1)))
	{
	  tree elttype = TREE_TYPE (type);
	  tree arg1_elttype = TREE_TYPE (TREE_TYPE (arg1));
	  /* We can't handle steps directly when extending, since the
	     values need to wrap at the original precision first.  */
	  bool step_ok_p
	    = (INTEGRAL_TYPE_P (elttype)
	       && INTEGRAL_TYPE_P (arg1_elttype)
	       && TYPE_PRECISION (elttype) <= TYPE_PRECISION (arg1_elttype));
	  tree_vector_builder v;
	  if (!v.new_unary_operation (type, arg1, step_ok_p))
	    return NULL_TREE;
	  unsigned int len = v.encoded_nelts ();
	  for (unsigned int i = 0; i < len; ++i)
	    {
	      tree elt = VECTOR_CST_ELT (arg1, i);
	      tree cvt = fold_convert_const (code, elttype, elt);
	      if (cvt == NULL_TREE)
		return NULL_TREE;
	      v.quick_push (cvt);
	    }
	  return v.build ();
	}
    }
  return NULL_TREE;
}

/* Construct a vector of zero elements of vector type TYPE.  */

static tree
build_zero_vector (tree type)
{
  tree t;

  t = fold_convert_const (NOP_EXPR, TREE_TYPE (type), integer_zero_node);
  return build_vector_from_val (type, t);
}

/* Returns true, if ARG is convertible to TYPE using a NOP_EXPR.  */

bool
fold_convertible_p (const_tree type, const_tree arg)
{
  const_tree orig = TREE_TYPE (arg);

  if (type == orig)
    return true;

  if (TREE_CODE (arg) == ERROR_MARK
      || TREE_CODE (type) == ERROR_MARK
      || TREE_CODE (orig) == ERROR_MARK)
    return false;

  if (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (orig))
    return true;

  switch (TREE_CODE (type))
    {
    case INTEGER_TYPE: case ENUMERAL_TYPE: case BOOLEAN_TYPE:
    case POINTER_TYPE: case REFERENCE_TYPE:
    case OFFSET_TYPE:
      return (INTEGRAL_TYPE_P (orig)
	      || (POINTER_TYPE_P (orig)
		  && TYPE_PRECISION (type) <= TYPE_PRECISION (orig))
	      || TREE_CODE (orig) == OFFSET_TYPE);

    case REAL_TYPE:
    case FIXED_POINT_TYPE:
    case VOID_TYPE:
      return TREE_CODE (type) == TREE_CODE (orig);

    case VECTOR_TYPE:
      return (VECTOR_TYPE_P (orig)
	      && known_eq (TYPE_VECTOR_SUBPARTS (type),
			   TYPE_VECTOR_SUBPARTS (orig))
	      && tree_int_cst_equal (TYPE_SIZE (type), TYPE_SIZE (orig)));

    default:
      return false;
    }
}

/* Convert expression ARG to type TYPE.  Used by the middle-end for
   simple conversions in preference to calling the front-end's convert.  */

tree
fold_convert_loc (location_t loc, tree type, tree arg)
{
  tree orig = TREE_TYPE (arg);
  tree tem;

  if (type == orig)
    return arg;

  if (TREE_CODE (arg) == ERROR_MARK
      || TREE_CODE (type) == ERROR_MARK
      || TREE_CODE (orig) == ERROR_MARK)
    return error_mark_node;

  switch (TREE_CODE (type))
    {
    case POINTER_TYPE:
    case REFERENCE_TYPE:
      /* Handle conversions between pointers to different address spaces.  */
      if (POINTER_TYPE_P (orig)
	  && (TYPE_ADDR_SPACE (TREE_TYPE (type))
	      != TYPE_ADDR_SPACE (TREE_TYPE (orig))))
	return fold_build1_loc (loc, ADDR_SPACE_CONVERT_EXPR, type, arg);
      /* fall through */

    case INTEGER_TYPE: case ENUMERAL_TYPE: case BOOLEAN_TYPE:
    case OFFSET_TYPE: case BITINT_TYPE:
      if (TREE_CODE (arg) == INTEGER_CST)
	{
	  tem = fold_convert_const (NOP_EXPR, type, arg);
	  if (tem != NULL_TREE)
	    return tem;
	}
      if (INTEGRAL_TYPE_P (orig) || POINTER_TYPE_P (orig)
	  || TREE_CODE (orig) == OFFSET_TYPE)
	return fold_build1_loc (loc, NOP_EXPR, type, arg);
      if (TREE_CODE (orig) == COMPLEX_TYPE)
	return fold_convert_loc (loc, type,
				 fold_build1_loc (loc, REALPART_EXPR,
						  TREE_TYPE (orig), arg));
      gcc_assert (VECTOR_TYPE_P (orig)
		  && tree_int_cst_equal (TYPE_SIZE (type), TYPE_SIZE (orig)));
      return fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, arg);

    case REAL_TYPE:
      if (TREE_CODE (arg) == INTEGER_CST)
	{
	  tem = fold_convert_const (FLOAT_EXPR, type, arg);
	  if (tem != NULL_TREE)
	    return tem;
	}
      else if (TREE_CODE (arg) == REAL_CST)
	{
	  tem = fold_convert_const (NOP_EXPR, type, arg);
	  if (tem != NULL_TREE)
	    return tem;
	}
      else if (TREE_CODE (arg) == FIXED_CST)
	{
	  tem = fold_convert_const (FIXED_CONVERT_EXPR, type, arg);
	  if (tem != NULL_TREE)
	    return tem;
	}

      switch (TREE_CODE (orig))
	{
	case INTEGER_TYPE: case BITINT_TYPE:
	case BOOLEAN_TYPE: case ENUMERAL_TYPE:
	case POINTER_TYPE: case REFERENCE_TYPE:
	  return fold_build1_loc (loc, FLOAT_EXPR, type, arg);

	case REAL_TYPE:
	  return fold_build1_loc (loc, NOP_EXPR, type, arg);

	case FIXED_POINT_TYPE:
	  return fold_build1_loc (loc, FIXED_CONVERT_EXPR, type, arg);

	case COMPLEX_TYPE:
	  tem = fold_build1_loc (loc, REALPART_EXPR, TREE_TYPE (orig), arg);
	  return fold_convert_loc (loc, type, tem);

	default:
	  gcc_unreachable ();
	}

    case FIXED_POINT_TYPE:
      if (TREE_CODE (arg) == FIXED_CST || TREE_CODE (arg) == INTEGER_CST
	  || TREE_CODE (arg) == REAL_CST)
	{
	  tem = fold_convert_const (FIXED_CONVERT_EXPR, type, arg);
	  if (tem != NULL_TREE)
	    goto fold_convert_exit;
	}

      switch (TREE_CODE (orig))
	{
	case FIXED_POINT_TYPE:
	case INTEGER_TYPE:
	case ENUMERAL_TYPE:
	case BOOLEAN_TYPE:
	case REAL_TYPE:
	case BITINT_TYPE:
	  return fold_build1_loc (loc, FIXED_CONVERT_EXPR, type, arg);

	case COMPLEX_TYPE:
	  tem = fold_build1_loc (loc, REALPART_EXPR, TREE_TYPE (orig), arg);
	  return fold_convert_loc (loc, type, tem);

	default:
	  gcc_unreachable ();
	}

    case COMPLEX_TYPE:
      switch (TREE_CODE (orig))
	{
	case INTEGER_TYPE: case BITINT_TYPE:
	case BOOLEAN_TYPE: case ENUMERAL_TYPE:
	case POINTER_TYPE: case REFERENCE_TYPE:
	case REAL_TYPE:
	case FIXED_POINT_TYPE:
	  return fold_build2_loc (loc, COMPLEX_EXPR, type,
			      fold_convert_loc (loc, TREE_TYPE (type), arg),
			      fold_convert_loc (loc, TREE_TYPE (type),
					    integer_zero_node));
	case COMPLEX_TYPE:
	  {
	    tree rpart, ipart;

	    if (TREE_CODE (arg) == COMPLEX_EXPR)
	      {
		rpart = fold_convert_loc (loc, TREE_TYPE (type),
				      TREE_OPERAND (arg, 0));
		ipart = fold_convert_loc (loc, TREE_TYPE (type),
				      TREE_OPERAND (arg, 1));
		return fold_build2_loc (loc, COMPLEX_EXPR, type, rpart, ipart);
	      }

	    arg = save_expr (arg);
	    rpart = fold_build1_loc (loc, REALPART_EXPR, TREE_TYPE (orig), arg);
	    ipart = fold_build1_loc (loc, IMAGPART_EXPR, TREE_TYPE (orig), arg);
	    rpart = fold_convert_loc (loc, TREE_TYPE (type), rpart);
	    ipart = fold_convert_loc (loc, TREE_TYPE (type), ipart);
	    return fold_build2_loc (loc, COMPLEX_EXPR, type, rpart, ipart);
	  }

	default:
	  gcc_unreachable ();
	}

    case VECTOR_TYPE:
      if (integer_zerop (arg))
	return build_zero_vector (type);
      gcc_assert (tree_int_cst_equal (TYPE_SIZE (type), TYPE_SIZE (orig)));
      gcc_assert (INTEGRAL_TYPE_P (orig) || POINTER_TYPE_P (orig)
		  || VECTOR_TYPE_P (orig));
      return fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, arg);

    case VOID_TYPE:
      tem = fold_ignored_result (arg);
      return fold_build1_loc (loc, NOP_EXPR, type, tem);

    default:
      if (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (orig))
	return fold_build1_loc (loc, NOP_EXPR, type, arg);
      gcc_unreachable ();
    }
 fold_convert_exit:
  tem = protected_set_expr_location_unshare (tem, loc);
  return tem;
}

/* Return false if expr can be assumed not to be an lvalue, true
   otherwise.  */

static bool
maybe_lvalue_p (const_tree x)
{
  /* We only need to wrap lvalue tree codes.  */
  switch (TREE_CODE (x))
  {
  case VAR_DECL:
  case PARM_DECL:
  case RESULT_DECL:
  case LABEL_DECL:
  case FUNCTION_DECL:
  case SSA_NAME:
  case COMPOUND_LITERAL_EXPR:

  case COMPONENT_REF:
  case MEM_REF:
  case INDIRECT_REF:
  case ARRAY_REF:
  case ARRAY_RANGE_REF:
  case BIT_FIELD_REF:
  case OBJ_TYPE_REF:

  case REALPART_EXPR:
  case IMAGPART_EXPR:
  case PREINCREMENT_EXPR:
  case PREDECREMENT_EXPR:
  case SAVE_EXPR:
  case TRY_CATCH_EXPR:
  case WITH_CLEANUP_EXPR:
  case COMPOUND_EXPR:
  case MODIFY_EXPR:
  case TARGET_EXPR:
  case COND_EXPR:
  case BIND_EXPR:
  case VIEW_CONVERT_EXPR:
    break;

  default:
    /* Assume the worst for front-end tree codes.  */
    if ((int)TREE_CODE (x) >= NUM_TREE_CODES)
      break;
    return false;
  }

  return true;
}

/* Return an expr equal to X but certainly not valid as an lvalue.  */

tree
non_lvalue_loc (location_t loc, tree x)
{
  /* While we are in GIMPLE, NON_LVALUE_EXPR doesn't mean anything to
     us.  */
  if (in_gimple_form)
    return x;

  if (! maybe_lvalue_p (x))
    return x;
  return build1_loc (loc, NON_LVALUE_EXPR, TREE_TYPE (x), x);
}

/* Given a tree comparison code, return the code that is the logical inverse.
   It is generally not safe to do this for floating-point comparisons, except
   for EQ_EXPR, NE_EXPR, ORDERED_EXPR and UNORDERED_EXPR, so we return
   ERROR_MARK in this case.  */

enum tree_code
invert_tree_comparison (enum tree_code code, bool honor_nans)
{
  if (honor_nans && flag_trapping_math && code != EQ_EXPR && code != NE_EXPR
      && code != ORDERED_EXPR && code != UNORDERED_EXPR)
    return ERROR_MARK;

  switch (code)
    {
    case EQ_EXPR:
      return NE_EXPR;
    case NE_EXPR:
      return EQ_EXPR;
    case GT_EXPR:
      return honor_nans ? UNLE_EXPR : LE_EXPR;
    case GE_EXPR:
      return honor_nans ? UNLT_EXPR : LT_EXPR;
    case LT_EXPR:
      return honor_nans ? UNGE_EXPR : GE_EXPR;
    case LE_EXPR:
      return honor_nans ? UNGT_EXPR : GT_EXPR;
    case LTGT_EXPR:
      return UNEQ_EXPR;
    case UNEQ_EXPR:
      return LTGT_EXPR;
    case UNGT_EXPR:
      return LE_EXPR;
    case UNGE_EXPR:
      return LT_EXPR;
    case UNLT_EXPR:
      return GE_EXPR;
    case UNLE_EXPR:
      return GT_EXPR;
    case ORDERED_EXPR:
      return UNORDERED_EXPR;
    case UNORDERED_EXPR:
      return ORDERED_EXPR;
    default:
      gcc_unreachable ();
    }
}

/* Similar, but return the comparison that results if the operands are
   swapped.  This is safe for floating-point.  */

enum tree_code
swap_tree_comparison (enum tree_code code)
{
  switch (code)
    {
    case EQ_EXPR:
    case NE_EXPR:
    case ORDERED_EXPR:
    case UNORDERED_EXPR:
    case LTGT_EXPR:
    case UNEQ_EXPR:
      return code;
    case GT_EXPR:
      return LT_EXPR;
    case GE_EXPR:
      return LE_EXPR;
    case LT_EXPR:
      return GT_EXPR;
    case LE_EXPR:
      return GE_EXPR;
    case UNGT_EXPR:
      return UNLT_EXPR;
    case UNGE_EXPR:
      return UNLE_EXPR;
    case UNLT_EXPR:
      return UNGT_EXPR;
    case UNLE_EXPR:
      return UNGE_EXPR;
    default:
      gcc_unreachable ();
    }
}


/* Convert a comparison tree code from an enum tree_code representation
   into a compcode bit-based encoding.  This function is the inverse of
   compcode_to_comparison.  */

static enum comparison_code
comparison_to_compcode (enum tree_code code)
{
  switch (code)
    {
    case LT_EXPR:
      return COMPCODE_LT;
    case EQ_EXPR:
      return COMPCODE_EQ;
    case LE_EXPR:
      return COMPCODE_LE;
    case GT_EXPR:
      return COMPCODE_GT;
    case NE_EXPR:
      return COMPCODE_NE;
    case GE_EXPR:
      return COMPCODE_GE;
    case ORDERED_EXPR:
      return COMPCODE_ORD;
    case UNORDERED_EXPR:
      return COMPCODE_UNORD;
    case UNLT_EXPR:
      return COMPCODE_UNLT;
    case UNEQ_EXPR:
      return COMPCODE_UNEQ;
    case UNLE_EXPR:
      return COMPCODE_UNLE;
    case UNGT_EXPR:
      return COMPCODE_UNGT;
    case LTGT_EXPR:
      return COMPCODE_LTGT;
    case UNGE_EXPR:
      return COMPCODE_UNGE;
    default:
      gcc_unreachable ();
    }
}

/* Convert a compcode bit-based encoding of a comparison operator back
   to GCC's enum tree_code representation.  This function is the
   inverse of comparison_to_compcode.  */

static enum tree_code
compcode_to_comparison (enum comparison_code code)
{
  switch (code)
    {
    case COMPCODE_LT:
      return LT_EXPR;
    case COMPCODE_EQ:
      return EQ_EXPR;
    case COMPCODE_LE:
      return LE_EXPR;
    case COMPCODE_GT:
      return GT_EXPR;
    case COMPCODE_NE:
      return NE_EXPR;
    case COMPCODE_GE:
      return GE_EXPR;
    case COMPCODE_ORD:
      return ORDERED_EXPR;
    case COMPCODE_UNORD:
      return UNORDERED_EXPR;
    case COMPCODE_UNLT:
      return UNLT_EXPR;
    case COMPCODE_UNEQ:
      return UNEQ_EXPR;
    case COMPCODE_UNLE:
      return UNLE_EXPR;
    case COMPCODE_UNGT:
      return UNGT_EXPR;
    case COMPCODE_LTGT:
      return LTGT_EXPR;
    case COMPCODE_UNGE:
      return UNGE_EXPR;
    default:
      gcc_unreachable ();
    }
}

/* Return true if COND1 tests the opposite condition of COND2.  */

bool
inverse_conditions_p (const_tree cond1, const_tree cond2)
{
  return (COMPARISON_CLASS_P (cond1)
	  && COMPARISON_CLASS_P (cond2)
	  && (invert_tree_comparison
	      (TREE_CODE (cond1),
	       HONOR_NANS (TREE_OPERAND (cond1, 0))) == TREE_CODE (cond2))
	  && operand_equal_p (TREE_OPERAND (cond1, 0),
			      TREE_OPERAND (cond2, 0), 0)
	  && operand_equal_p (TREE_OPERAND (cond1, 1),
			      TREE_OPERAND (cond2, 1), 0));
}

/* Return a tree for the comparison which is the combination of
   doing the AND or OR (depending on CODE) of the two operations LCODE
   and RCODE on the identical operands LL_ARG and LR_ARG.  Take into account
   the possibility of trapping if the mode has NaNs, and return NULL_TREE
   if this makes the transformation invalid.  */

tree
combine_comparisons (location_t loc,
		     enum tree_code code, enum tree_code lcode,
		     enum tree_code rcode, tree truth_type,
		     tree ll_arg, tree lr_arg)
{
  bool honor_nans = HONOR_NANS (ll_arg);
  enum comparison_code lcompcode = comparison_to_compcode (lcode);
  enum comparison_code rcompcode = comparison_to_compcode (rcode);
  int compcode;

  switch (code)
    {
    case TRUTH_AND_EXPR: case TRUTH_ANDIF_EXPR:
      compcode = lcompcode & rcompcode;
      break;

    case TRUTH_OR_EXPR: case TRUTH_ORIF_EXPR:
      compcode = lcompcode | rcompcode;
      break;

    default:
      return NULL_TREE;
    }

  if (!honor_nans)
    {
      /* Eliminate unordered comparisons, as well as LTGT and ORD
	 which are not used unless the mode has NaNs.  */
      compcode &= ~COMPCODE_UNORD;
      if (compcode == COMPCODE_LTGT)
	compcode = COMPCODE_NE;
      else if (compcode == COMPCODE_ORD)
	compcode = COMPCODE_TRUE;
    }
   else if (flag_trapping_math)
     {
	/* Check that the original operation and the optimized ones will trap
	   under the same condition.  */
	bool ltrap = (lcompcode & COMPCODE_UNORD) == 0
		     && (lcompcode != COMPCODE_EQ)
		     && (lcompcode != COMPCODE_ORD);
	bool rtrap = (rcompcode & COMPCODE_UNORD) == 0
		     && (rcompcode != COMPCODE_EQ)
		     && (rcompcode != COMPCODE_ORD);
	bool trap = (compcode & COMPCODE_UNORD) == 0
		    && (compcode != COMPCODE_EQ)
		    && (compcode != COMPCODE_ORD);

        /* In a short-circuited boolean expression the LHS might be
	   such that the RHS, if evaluated, will never trap.  For
	   example, in ORD (x, y) && (x < y), we evaluate the RHS only
	   if neither x nor y is NaN.  (This is a mixed blessing: for
	   example, the expression above will never trap, hence
	   optimizing it to x < y would be invalid).  */
        if ((code == TRUTH_ORIF_EXPR && (lcompcode & COMPCODE_UNORD))
            || (code == TRUTH_ANDIF_EXPR && !(lcompcode & COMPCODE_UNORD)))
          rtrap = false;

        /* If the comparison was short-circuited, and only the RHS
	   trapped, we may now generate a spurious trap.  */
	if (rtrap && !ltrap
	    && (code == TRUTH_ANDIF_EXPR || code == TRUTH_ORIF_EXPR))
	  return NULL_TREE;

	/* If we changed the conditions that cause a trap, we lose.  */
	if ((ltrap || rtrap) != trap)
	  return NULL_TREE;
      }

  if (compcode == COMPCODE_TRUE)
    return constant_boolean_node (true, truth_type);
  else if (compcode == COMPCODE_FALSE)
    return constant_boolean_node (false, truth_type);
  else
    {
      enum tree_code tcode;

      tcode = compcode_to_comparison ((enum comparison_code) compcode);
      return fold_build2_loc (loc, tcode, truth_type, ll_arg, lr_arg);
    }
}

/* Return nonzero if two operands (typically of the same tree node)
   are necessarily equal. FLAGS modifies behavior as follows:

   If OEP_ONLY_CONST is set, only return nonzero for constants.
   This function tests whether the operands are indistinguishable;
   it does not test whether they are equal using C's == operation.
   The distinction is important for IEEE floating point, because
   (1) -0.0 and 0.0 are distinguishable, but -0.0==0.0, and
   (2) two NaNs may be indistinguishable, but NaN!=NaN.

   If OEP_ONLY_CONST is unset, a VAR_DECL is considered equal to itself
   even though it may hold multiple values during a function.
   This is because a GCC tree node guarantees that nothing else is
   executed between the evaluation of its "operands" (which may often
   be evaluated in arbitrary order).  Hence if the operands themselves
   don't side-effect, the VAR_DECLs, PARM_DECLs etc... must hold the
   same value in each operand/subexpression.  Hence leaving OEP_ONLY_CONST
   unset means assuming isochronic (or instantaneous) tree equivalence.
   Unless comparing arbitrary expression trees, such as from different
   statements, this flag can usually be left unset.

   If OEP_PURE_SAME is set, then pure functions with identical arguments
   are considered the same.  It is used when the caller has other ways
   to ensure that global memory is unchanged in between.

   If OEP_ADDRESS_OF is set, we are actually comparing addresses of objects,
   not values of expressions.

   If OEP_LEXICOGRAPHIC is set, then also handle expressions with side-effects
   such as MODIFY_EXPR, RETURN_EXPR, as well as STATEMENT_LISTs.

   If OEP_BITWISE is set, then require the values to be bitwise identical
   rather than simply numerically equal.  Do not take advantage of things
   like math-related flags or undefined behavior; only return true for
   values that are provably bitwise identical in all circumstances.

   Unless OEP_MATCH_SIDE_EFFECTS is set, the function returns false on
   any operand with side effect.  This is unnecesarily conservative in the
   case we know that arg0 and arg1 are in disjoint code paths (such as in
   ?: operator).  In addition OEP_MATCH_SIDE_EFFECTS is used when comparing
   addresses with TREE_CONSTANT flag set so we know that &var == &var
   even if var is volatile.  */

bool
operand_compare::operand_equal_p (const_tree arg0, const_tree arg1,
				  unsigned int flags)
{
  bool r;
  if (verify_hash_value (arg0, arg1, flags, &r))
    return r;

  STRIP_ANY_LOCATION_WRAPPER (arg0);
  STRIP_ANY_LOCATION_WRAPPER (arg1);

  /* If either is ERROR_MARK, they aren't equal.  */
  if (TREE_CODE (arg0) == ERROR_MARK || TREE_CODE (arg1) == ERROR_MARK
      || TREE_TYPE (arg0) == error_mark_node
      || TREE_TYPE (arg1) == error_mark_node)
    return false;

  /* Similar, if either does not have a type (like a template id),
     they aren't equal.  */
  if (!TREE_TYPE (arg0) || !TREE_TYPE (arg1))
    return false;

  /* Bitwise identity makes no sense if the values have different layouts.  */
  if ((flags & OEP_BITWISE)
      && !tree_nop_conversion_p (TREE_TYPE (arg0), TREE_TYPE (arg1)))
    return false;

  /* We cannot consider pointers to different address space equal.  */
  if (POINTER_TYPE_P (TREE_TYPE (arg0))
      && POINTER_TYPE_P (TREE_TYPE (arg1))
      && (TYPE_ADDR_SPACE (TREE_TYPE (TREE_TYPE (arg0)))
	  != TYPE_ADDR_SPACE (TREE_TYPE (TREE_TYPE (arg1)))))
    return false;

  /* Check equality of integer constants before bailing out due to
     precision differences.  */
  if (TREE_CODE (arg0) == INTEGER_CST && TREE_CODE (arg1) == INTEGER_CST)
    {
      /* Address of INTEGER_CST is not defined; check that we did not forget
	 to drop the OEP_ADDRESS_OF flags.  */
      gcc_checking_assert (!(flags & OEP_ADDRESS_OF));
      return tree_int_cst_equal (arg0, arg1);
    }

  if (!(flags & OEP_ADDRESS_OF))
    {
      /* If both types don't have the same signedness, then we can't consider
	 them equal.  We must check this before the STRIP_NOPS calls
	 because they may change the signedness of the arguments.  As pointers
	 strictly don't have a signedness, require either two pointers or
	 two non-pointers as well.  */
      if (TYPE_UNSIGNED (TREE_TYPE (arg0)) != TYPE_UNSIGNED (TREE_TYPE (arg1))
	  || POINTER_TYPE_P (TREE_TYPE (arg0))
			     != POINTER_TYPE_P (TREE_TYPE (arg1)))
	return false;

      /* If both types don't have the same precision, then it is not safe
	 to strip NOPs.  */
      if (element_precision (TREE_TYPE (arg0))
	  != element_precision (TREE_TYPE (arg1)))
	return false;

      STRIP_NOPS (arg0);
      STRIP_NOPS (arg1);
    }
#if 0
  /* FIXME: Fortran FE currently produce ADDR_EXPR of NOP_EXPR. Enable the
     sanity check once the issue is solved.  */
  else
    /* Addresses of conversions and SSA_NAMEs (and many other things)
       are not defined.  Check that we did not forget to drop the
       OEP_ADDRESS_OF/OEP_CONSTANT_ADDRESS_OF flags.  */
    gcc_checking_assert (!CONVERT_EXPR_P (arg0) && !CONVERT_EXPR_P (arg1)
			 && TREE_CODE (arg0) != SSA_NAME);
#endif

  /* In case both args are comparisons but with different comparison
     code, try to swap the comparison operands of one arg to produce
     a match and compare that variant.  */
  if (TREE_CODE (arg0) != TREE_CODE (arg1)
      && COMPARISON_CLASS_P (arg0)
      && COMPARISON_CLASS_P (arg1))
    {
      enum tree_code swap_code = swap_tree_comparison (TREE_CODE (arg1));

      if (TREE_CODE (arg0) == swap_code)
	return operand_equal_p (TREE_OPERAND (arg0, 0),
			        TREE_OPERAND (arg1, 1), flags)
	       && operand_equal_p (TREE_OPERAND (arg0, 1),
				   TREE_OPERAND (arg1, 0), flags);
    }

  if (TREE_CODE (arg0) != TREE_CODE (arg1))
    {
      /* NOP_EXPR and CONVERT_EXPR are considered equal.  */
      if (CONVERT_EXPR_P (arg0) && CONVERT_EXPR_P (arg1))
	;
      else if (flags & OEP_ADDRESS_OF)
	{
	  /* If we are interested in comparing addresses ignore
	     MEM_REF wrappings of the base that can appear just for
	     TBAA reasons.  */
	  if (TREE_CODE (arg0) == MEM_REF
	      && DECL_P (arg1)
	      && TREE_CODE (TREE_OPERAND (arg0, 0)) == ADDR_EXPR
	      && TREE_OPERAND (TREE_OPERAND (arg0, 0), 0) == arg1
	      && integer_zerop (TREE_OPERAND (arg0, 1)))
	    return true;
	  else if (TREE_CODE (arg1) == MEM_REF
		   && DECL_P (arg0)
		   && TREE_CODE (TREE_OPERAND (arg1, 0)) == ADDR_EXPR
		   && TREE_OPERAND (TREE_OPERAND (arg1, 0), 0) == arg0
		   && integer_zerop (TREE_OPERAND (arg1, 1)))
	    return true;
	  return false;
	}
      else
	return false;
    }

  /* When not checking adddresses, this is needed for conversions and for
     COMPONENT_REF.  Might as well play it safe and always test this.  */
  if (TREE_CODE (TREE_TYPE (arg0)) == ERROR_MARK
      || TREE_CODE (TREE_TYPE (arg1)) == ERROR_MARK
      || (TYPE_MODE (TREE_TYPE (arg0)) != TYPE_MODE (TREE_TYPE (arg1))
	  && !(flags & OEP_ADDRESS_OF)))
    return false;

  /* If ARG0 and ARG1 are the same SAVE_EXPR, they are necessarily equal.
     We don't care about side effects in that case because the SAVE_EXPR
     takes care of that for us. In all other cases, two expressions are
     equal if they have no side effects.  If we have two identical
     expressions with side effects that should be treated the same due
     to the only side effects being identical SAVE_EXPR's, that will
     be detected in the recursive calls below.
     If we are taking an invariant address of two identical objects
     they are necessarily equal as well.  */
  if (arg0 == arg1 && ! (flags & OEP_ONLY_CONST)
      && (TREE_CODE (arg0) == SAVE_EXPR
	  || (flags & OEP_MATCH_SIDE_EFFECTS)
	  || (! TREE_SIDE_EFFECTS (arg0) && ! TREE_SIDE_EFFECTS (arg1))))
    return true;

  /* Next handle constant cases, those for which we can return 1 even
     if ONLY_CONST is set.  */
  if (TREE_CONSTANT (arg0) && TREE_CONSTANT (arg1))
    switch (TREE_CODE (arg0))
      {
      case INTEGER_CST:
	return tree_int_cst_equal (arg0, arg1);

      case FIXED_CST:
	return FIXED_VALUES_IDENTICAL (TREE_FIXED_CST (arg0),
				       TREE_FIXED_CST (arg1));

      case REAL_CST:
	if (real_identical (&TREE_REAL_CST (arg0), &TREE_REAL_CST (arg1)))
	  return true;

	if (!(flags & OEP_BITWISE) && !HONOR_SIGNED_ZEROS (arg0))
	  {
	    /* If we do not distinguish between signed and unsigned zero,
	       consider them equal.  */
	    if (real_zerop (arg0) && real_zerop (arg1))
	      return true;
	  }
	return false;

      case VECTOR_CST:
	{
	  if (VECTOR_CST_LOG2_NPATTERNS (arg0)
	      != VECTOR_CST_LOG2_NPATTERNS (arg1))
	    return false;

	  if (VECTOR_CST_NELTS_PER_PATTERN (arg0)
	      != VECTOR_CST_NELTS_PER_PATTERN (arg1))
	    return false;

	  unsigned int count = vector_cst_encoded_nelts (arg0);
	  for (unsigned int i = 0; i < count; ++i)
	    if (!operand_equal_p (VECTOR_CST_ENCODED_ELT (arg0, i),
				  VECTOR_CST_ENCODED_ELT (arg1, i), flags))
	      return false;
	  return true;
	}

      case COMPLEX_CST:
	return (operand_equal_p (TREE_REALPART (arg0), TREE_REALPART (arg1),
				 flags)
		&& operand_equal_p (TREE_IMAGPART (arg0), TREE_IMAGPART (arg1),
				    flags));

      case STRING_CST:
	return (TREE_STRING_LENGTH (arg0) == TREE_STRING_LENGTH (arg1)
		&& ! memcmp (TREE_STRING_POINTER (arg0),
			      TREE_STRING_POINTER (arg1),
			      TREE_STRING_LENGTH (arg0)));

      case ADDR_EXPR:
	gcc_checking_assert (!(flags & OEP_ADDRESS_OF));
	return operand_equal_p (TREE_OPERAND (arg0, 0), TREE_OPERAND (arg1, 0),
				flags | OEP_ADDRESS_OF
				| OEP_MATCH_SIDE_EFFECTS);
      case CONSTRUCTOR:
	{
	  /* In GIMPLE empty constructors are allowed in initializers of
	     aggregates.  */
	  if (!CONSTRUCTOR_NELTS (arg0) && !CONSTRUCTOR_NELTS (arg1))
	    return true;

	  /* See sem_variable::equals in ipa-icf for a similar approach.  */
	  tree typ0 = TREE_TYPE (arg0);
	  tree typ1 = TREE_TYPE (arg1);

	  if (TREE_CODE (typ0) != TREE_CODE (typ1))
	    return false;
	  else if (TREE_CODE (typ0) == ARRAY_TYPE)
	    {
	      /* For arrays, check that the sizes all match.  */
	      const HOST_WIDE_INT siz0 = int_size_in_bytes (typ0);
	      if (TYPE_MODE (typ0) != TYPE_MODE (typ1)
		  || siz0 < 0
		  || siz0 != int_size_in_bytes (typ1))
		return false;
	    }
	  else if (!types_compatible_p (typ0, typ1))
	    return false;

	  vec<constructor_elt, va_gc> *v0 = CONSTRUCTOR_ELTS (arg0);
	  vec<constructor_elt, va_gc> *v1 = CONSTRUCTOR_ELTS (arg1);
	  if (vec_safe_length (v0) != vec_safe_length (v1))
	    return false;

	  /* Address of CONSTRUCTOR is defined in GENERIC to mean the value
	     of the CONSTRUCTOR referenced indirectly.  */
	  flags &= ~OEP_ADDRESS_OF;

	  for (unsigned idx = 0; idx < vec_safe_length (v0); ++idx)
	    {
	      constructor_elt *c0 = &(*v0)[idx];
	      constructor_elt *c1 = &(*v1)[idx];

	      /* Check that the values are the same...  */
	      if (c0->value != c1->value
		  && !operand_equal_p (c0->value, c1->value, flags))
		return false;

	      /* ... and that they apply to the same field!  */
	      if (c0->index != c1->index
		  && (TREE_CODE (typ0) == ARRAY_TYPE
		      ? !operand_equal_p (c0->index, c1->index, flags)
		      : !operand_equal_p (DECL_FIELD_OFFSET (c0->index),
					  DECL_FIELD_OFFSET (c1->index),
					  flags)
			|| !operand_equal_p (DECL_FIELD_BIT_OFFSET (c0->index),
					     DECL_FIELD_BIT_OFFSET (c1->index),
					     flags)))
		return false;
	    }

	  return true;
	}

      default:
	break;
      }

  /* Don't handle more cases for OEP_BITWISE, since we can't guarantee that
     two instances of undefined behavior will give identical results.  */
  if (flags & (OEP_ONLY_CONST | OEP_BITWISE))
    return false;

/* Define macros to test an operand from arg0 and arg1 for equality and a
   variant that allows null and views null as being different from any
   non-null value.  In the latter case, if either is null, the both
   must be; otherwise, do the normal comparison.  */
#define OP_SAME(N) operand_equal_p (TREE_OPERAND (arg0, N),	\
				    TREE_OPERAND (arg1, N), flags)

#define OP_SAME_WITH_NULL(N)				\
  ((!TREE_OPERAND (arg0, N) || !TREE_OPERAND (arg1, N))	\
   ? TREE_OPERAND (arg0, N) == TREE_OPERAND (arg1, N) : OP_SAME (N))

  switch (TREE_CODE_CLASS (TREE_CODE (arg0)))
    {
    case tcc_unary:
      /* Two conversions are equal only if signedness and modes match.  */
      switch (TREE_CODE (arg0))
        {
	CASE_CONVERT:
        case FIX_TRUNC_EXPR:
	  if (TYPE_UNSIGNED (TREE_TYPE (arg0))
	      != TYPE_UNSIGNED (TREE_TYPE (arg1)))
	    return false;
	  break;
	default:
	  break;
	}

      return OP_SAME (0);


    case tcc_comparison:
    case tcc_binary:
      if (OP_SAME (0) && OP_SAME (1))
	return true;

      /* For commutative ops, allow the other order.  */
      return (commutative_tree_code (TREE_CODE (arg0))
	      && operand_equal_p (TREE_OPERAND (arg0, 0),
				  TREE_OPERAND (arg1, 1), flags)
	      && operand_equal_p (TREE_OPERAND (arg0, 1),
				  TREE_OPERAND (arg1, 0), flags));

    case tcc_reference:
      /* If either of the pointer (or reference) expressions we are
	 dereferencing contain a side effect, these cannot be equal,
	 but their addresses can be.  */
      if ((flags & OEP_MATCH_SIDE_EFFECTS) == 0
	  && (TREE_SIDE_EFFECTS (arg0)
	      || TREE_SIDE_EFFECTS (arg1)))
	return false;

      switch (TREE_CODE (arg0))
	{
	case INDIRECT_REF:
	  if (!(flags & OEP_ADDRESS_OF))
	    {
	      if (TYPE_ALIGN (TREE_TYPE (arg0))
		  != TYPE_ALIGN (TREE_TYPE (arg1)))
		return false;
	      /* Verify that the access types are compatible.  */
	      if (TYPE_MAIN_VARIANT (TREE_TYPE (arg0))
		  != TYPE_MAIN_VARIANT (TREE_TYPE (arg1)))
		return false;
	    }
	  flags &= ~OEP_ADDRESS_OF;
	  return OP_SAME (0);

	case IMAGPART_EXPR:
	  /* Require the same offset.  */
	  if (!operand_equal_p (TYPE_SIZE (TREE_TYPE (arg0)),
				TYPE_SIZE (TREE_TYPE (arg1)),
				flags & ~OEP_ADDRESS_OF))
	    return false;

	/* Fallthru.  */
	case REALPART_EXPR:
	case VIEW_CONVERT_EXPR:
	  return OP_SAME (0);

	case TARGET_MEM_REF:
	case MEM_REF:
	  if (!(flags & OEP_ADDRESS_OF))
	    {
	      /* Require equal access sizes */
	      if (TYPE_SIZE (TREE_TYPE (arg0)) != TYPE_SIZE (TREE_TYPE (arg1))
		  && (!TYPE_SIZE (TREE_TYPE (arg0))
		      || !TYPE_SIZE (TREE_TYPE (arg1))
		      || !operand_equal_p (TYPE_SIZE (TREE_TYPE (arg0)),
					   TYPE_SIZE (TREE_TYPE (arg1)),
					   flags)))
		return false;
	      /* Verify that access happens in similar types.  */
	      if (!types_compatible_p (TREE_TYPE (arg0), TREE_TYPE (arg1)))
		return false;
	      /* Verify that accesses are TBAA compatible.  */
	      if (!alias_ptr_types_compatible_p
		    (TREE_TYPE (TREE_OPERAND (arg0, 1)),
		     TREE_TYPE (TREE_OPERAND (arg1, 1)))
		  || (MR_DEPENDENCE_CLIQUE (arg0)
		      != MR_DEPENDENCE_CLIQUE (arg1))
		  || (MR_DEPENDENCE_BASE (arg0)
		      != MR_DEPENDENCE_BASE (arg1)))
		return false;
	     /* Verify that alignment is compatible.  */
	     if (TYPE_ALIGN (TREE_TYPE (arg0))
		 != TYPE_ALIGN (TREE_TYPE (arg1)))
		return false;
	    }
	  flags &= ~OEP_ADDRESS_OF;
	  return (OP_SAME (0) && OP_SAME (1)
		  /* TARGET_MEM_REF require equal extra operands.  */
		  && (TREE_CODE (arg0) != TARGET_MEM_REF
		      || (OP_SAME_WITH_NULL (2)
			  && OP_SAME_WITH_NULL (3)
			  && OP_SAME_WITH_NULL (4))));

	case ARRAY_REF:
	case ARRAY_RANGE_REF:
	  if (!OP_SAME (0))
	    return false;
	  flags &= ~OEP_ADDRESS_OF;
	  /* Compare the array index by value if it is constant first as we
	     may have different types but same value here.  */
	  return ((tree_int_cst_equal (TREE_OPERAND (arg0, 1),
				       TREE_OPERAND (arg1, 1))
		   || OP_SAME (1))
		  && OP_SAME_WITH_NULL (2)
		  && OP_SAME_WITH_NULL (3)
		  /* Compare low bound and element size as with OEP_ADDRESS_OF
		     we have to account for the offset of the ref.  */
		  && (TREE_TYPE (TREE_OPERAND (arg0, 0))
		      == TREE_TYPE (TREE_OPERAND (arg1, 0))
		      || (operand_equal_p (array_ref_low_bound
					     (CONST_CAST_TREE (arg0)),
					   array_ref_low_bound
					     (CONST_CAST_TREE (arg1)), flags)
			  && operand_equal_p (array_ref_element_size
					        (CONST_CAST_TREE (arg0)),
					      array_ref_element_size
					        (CONST_CAST_TREE (arg1)),
					      flags))));

	case COMPONENT_REF:
	  /* Handle operand 2 the same as for ARRAY_REF.  Operand 0
	     may be NULL when we're called to compare MEM_EXPRs.  */
	  if (!OP_SAME_WITH_NULL (0))
	    return false;
	  {
	    bool compare_address = flags & OEP_ADDRESS_OF;

	    /* Most of time we only need to compare FIELD_DECLs for equality.
	       However when determining address look into actual offsets.
	       These may match for unions and unshared record types.  */
	    flags &= ~OEP_ADDRESS_OF;
	    if (!OP_SAME (1))
	      {
		if (compare_address
		    && (flags & OEP_ADDRESS_OF_SAME_FIELD) == 0)
		  {
		    tree field0 = TREE_OPERAND (arg0, 1);
		    tree field1 = TREE_OPERAND (arg1, 1);

		    /* Non-FIELD_DECL operands can appear in C++ templates.  */
		    if (TREE_CODE (field0) != FIELD_DECL
			|| TREE_CODE (field1) != FIELD_DECL)
		      return false;

		    if (!DECL_FIELD_OFFSET (field0)
			|| !DECL_FIELD_OFFSET (field1))
		      return field0 == field1;

		    if (!operand_equal_p (DECL_FIELD_OFFSET (field0),
					  DECL_FIELD_OFFSET (field1), flags)
			|| !operand_equal_p (DECL_FIELD_BIT_OFFSET (field0),
					     DECL_FIELD_BIT_OFFSET (field1),
					     flags))
		      return false;
		  }
		else
		  return false;
	      }
	  }
	  return OP_SAME_WITH_NULL (2);

	case BIT_FIELD_REF:
	  if (!OP_SAME (0))
	    return false;
	  flags &= ~OEP_ADDRESS_OF;
	  return OP_SAME (1) && OP_SAME (2);

	default:
	  return false;
	}

    case tcc_expression:
      switch (TREE_CODE (arg0))
	{
	case ADDR_EXPR:
	  /* Be sure we pass right ADDRESS_OF flag.  */
	  gcc_checking_assert (!(flags & OEP_ADDRESS_OF));
	  return operand_equal_p (TREE_OPERAND (arg0, 0),
				  TREE_OPERAND (arg1, 0),
				  flags | OEP_ADDRESS_OF);

	case TRUTH_NOT_EXPR:
	  return OP_SAME (0);

	case TRUTH_ANDIF_EXPR:
	case TRUTH_ORIF_EXPR:
	  return OP_SAME (0) && OP_SAME (1);

	case WIDEN_MULT_PLUS_EXPR:
	case WIDEN_MULT_MINUS_EXPR:
	  if (!OP_SAME (2))
	    return false;
	  /* The multiplcation operands are commutative.  */
	  /* FALLTHRU */

	case TRUTH_AND_EXPR:
	case TRUTH_OR_EXPR:
	case TRUTH_XOR_EXPR:
	  if (OP_SAME (0) && OP_SAME (1))
	    return true;

	  /* Otherwise take into account this is a commutative operation.  */
	  return (operand_equal_p (TREE_OPERAND (arg0, 0),
				   TREE_OPERAND (arg1, 1), flags)
		  && operand_equal_p (TREE_OPERAND (arg0, 1),
				      TREE_OPERAND (arg1, 0), flags));

	case COND_EXPR:
	  if (! OP_SAME (1) || ! OP_SAME_WITH_NULL (2))
	    return false;
	  flags &= ~OEP_ADDRESS_OF;
	  return OP_SAME (0);

	case BIT_INSERT_EXPR:
	  /* BIT_INSERT_EXPR has an implict operand as the type precision
	     of op1.  Need to check to make sure they are the same.  */
	  if (TREE_CODE (TREE_OPERAND (arg0, 1)) == INTEGER_CST
	      && TREE_CODE (TREE_OPERAND (arg1, 1)) == INTEGER_CST
	      && TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (arg0, 1)))
		 != TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (arg1, 1))))
	    return false;
	  /* FALLTHRU */

	case VEC_COND_EXPR:
	case DOT_PROD_EXPR:
	  return OP_SAME (0) && OP_SAME (1) && OP_SAME (2);

	case MODIFY_EXPR:
	case INIT_EXPR:
	case COMPOUND_EXPR:
	case PREDECREMENT_EXPR:
	case PREINCREMENT_EXPR:
	case POSTDECREMENT_EXPR:
	case POSTINCREMENT_EXPR:
	  if (flags & OEP_LEXICOGRAPHIC)
	    return OP_SAME (0) && OP_SAME (1);
	  return false;

	case CLEANUP_POINT_EXPR:
	case EXPR_STMT:
	case SAVE_EXPR:
	  if (flags & OEP_LEXICOGRAPHIC)
	    return OP_SAME (0);
	  return false;

	case OBJ_TYPE_REF:
	/* Virtual table reference.  */
	if (!operand_equal_p (OBJ_TYPE_REF_EXPR (arg0),
			      OBJ_TYPE_REF_EXPR (arg1), flags))
	  return false;
	flags &= ~OEP_ADDRESS_OF;
	if (tree_to_uhwi (OBJ_TYPE_REF_TOKEN (arg0))
	    != tree_to_uhwi (OBJ_TYPE_REF_TOKEN (arg1)))
	  return false;
	if (!operand_equal_p (OBJ_TYPE_REF_OBJECT (arg0),
			      OBJ_TYPE_REF_OBJECT (arg1), flags))
	  return false;
	if (virtual_method_call_p (arg0))
	  {
	    if (!virtual_method_call_p (arg1))
	      return false;
	    return types_same_for_odr (obj_type_ref_class (arg0),
				       obj_type_ref_class (arg1));
	  }
	return false;

	default:
	  return false;
	}

    case tcc_vl_exp:
      switch (TREE_CODE (arg0))
	{
	case CALL_EXPR:
	  if ((CALL_EXPR_FN (arg0) == NULL_TREE)
	      != (CALL_EXPR_FN (arg1) == NULL_TREE))
	    /* If not both CALL_EXPRs are either internal or normal function
	       functions, then they are not equal.  */
	    return false;
	  else if (CALL_EXPR_FN (arg0) == NULL_TREE)
	    {
	      /* If the CALL_EXPRs call different internal functions, then they
		 are not equal.  */
	      if (CALL_EXPR_IFN (arg0) != CALL_EXPR_IFN (arg1))
		return false;
	    }
	  else
	    {
	      /* If the CALL_EXPRs call different functions, then they are not
		 equal.  */
	      if (! operand_equal_p (CALL_EXPR_FN (arg0), CALL_EXPR_FN (arg1),
				     flags))
		return false;
	    }

	  /* FIXME: We could skip this test for OEP_MATCH_SIDE_EFFECTS.  */
	  {
	    unsigned int cef = call_expr_flags (arg0);
	    if (flags & OEP_PURE_SAME)
	      cef &= ECF_CONST | ECF_PURE;
	    else
	      cef &= ECF_CONST;
	    if (!cef && !(flags & OEP_LEXICOGRAPHIC))
	      return false;
	  }

	  /* Now see if all the arguments are the same.  */
	  {
	    const_call_expr_arg_iterator iter0, iter1;
	    const_tree a0, a1;
	    for (a0 = first_const_call_expr_arg (arg0, &iter0),
		   a1 = first_const_call_expr_arg (arg1, &iter1);
		 a0 && a1;
		 a0 = next_const_call_expr_arg (&iter0),
		   a1 = next_const_call_expr_arg (&iter1))
	      if (! operand_equal_p (a0, a1, flags))
		return false;

	    /* If we get here and both argument lists are exhausted
	       then the CALL_EXPRs are equal.  */
	    return ! (a0 || a1);
	  }
	default:
	  return false;
	}

    case tcc_declaration:
      /* Consider __builtin_sqrt equal to sqrt.  */
      if (TREE_CODE (arg0) == FUNCTION_DECL)
	return (fndecl_built_in_p (arg0) && fndecl_built_in_p (arg1)
		&& DECL_BUILT_IN_CLASS (arg0) == DECL_BUILT_IN_CLASS (arg1)
		&& (DECL_UNCHECKED_FUNCTION_CODE (arg0)
		    == DECL_UNCHECKED_FUNCTION_CODE (arg1)));

      if (DECL_P (arg0)
	  && (flags & OEP_DECL_NAME)
	  && (flags & OEP_LEXICOGRAPHIC))
	{
	  /* Consider decls with the same name equal.  The caller needs
	     to make sure they refer to the same entity (such as a function
	     formal parameter).  */
	  tree a0name = DECL_NAME (arg0);
	  tree a1name = DECL_NAME (arg1);
	  const char *a0ns = a0name ? IDENTIFIER_POINTER (a0name) : NULL;
	  const char *a1ns = a1name ? IDENTIFIER_POINTER (a1name) : NULL;
	  return a0ns && a1ns && strcmp (a0ns, a1ns) == 0;
	}
      return false;

    case tcc_exceptional:
      if (TREE_CODE (arg0) == CONSTRUCTOR)
	{
	  if (CONSTRUCTOR_NO_CLEARING (arg0) != CONSTRUCTOR_NO_CLEARING (arg1))
	    return false;

	  /* In GIMPLE constructors are used only to build vectors from
	     elements.  Individual elements in the constructor must be
	     indexed in increasing order and form an initial sequence.

	     We make no effort to compare nonconstant ones in GENERIC.  */
	  if (!VECTOR_TYPE_P (TREE_TYPE (arg0))
	      || !VECTOR_TYPE_P (TREE_TYPE (arg1)))
	    return false;

	  /* Be sure that vectors constructed have the same representation.
	     We only tested element precision and modes to match.
	     Vectors may be BLKmode and thus also check that the number of
	     parts match.  */
	  if (maybe_ne (TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg0)),
			TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg1))))
	    return false;

	  vec<constructor_elt, va_gc> *v0 = CONSTRUCTOR_ELTS (arg0);
	  vec<constructor_elt, va_gc> *v1 = CONSTRUCTOR_ELTS (arg1);
	  unsigned int len = vec_safe_length (v0);

	  if (len != vec_safe_length (v1))
	    return false;

	  for (unsigned int i = 0; i < len; i++)
	    {
	      constructor_elt *c0 = &(*v0)[i];
	      constructor_elt *c1 = &(*v1)[i];

	      if (!operand_equal_p (c0->value, c1->value, flags)
		  /* In GIMPLE the indexes can be either NULL or matching i.
		     Double check this so we won't get false
		     positives for GENERIC.  */
		  || (c0->index
		      && (TREE_CODE (c0->index) != INTEGER_CST 
			  || compare_tree_int (c0->index, i)))
		  || (c1->index
		      && (TREE_CODE (c1->index) != INTEGER_CST 
			  || compare_tree_int (c1->index, i))))
		return false;
	    }
	  return true;
	}
      else if (TREE_CODE (arg0) == STATEMENT_LIST
	       && (flags & OEP_LEXICOGRAPHIC))
	{
	  /* Compare the STATEMENT_LISTs.  */
	  tree_stmt_iterator tsi1, tsi2;
	  tree body1 = CONST_CAST_TREE (arg0);
	  tree body2 = CONST_CAST_TREE (arg1);
	  for (tsi1 = tsi_start (body1), tsi2 = tsi_start (body2); ;
	       tsi_next (&tsi1), tsi_next (&tsi2))
	    {
	      /* The lists don't have the same number of statements.  */
	      if (tsi_end_p (tsi1) ^ tsi_end_p (tsi2))
		return false;
	      if (tsi_end_p (tsi1) && tsi_end_p (tsi2))
		return true;
	      if (!operand_equal_p (tsi_stmt (tsi1), tsi_stmt (tsi2),
				    flags & (OEP_LEXICOGRAPHIC
					     | OEP_NO_HASH_CHECK)))
		return false;
	    }
	}
      return false;

    case tcc_statement:
      switch (TREE_CODE (arg0))
	{
	case RETURN_EXPR:
	  if (flags & OEP_LEXICOGRAPHIC)
	    return OP_SAME_WITH_NULL (0);
	  return false;
	case DEBUG_BEGIN_STMT:
	  if (flags & OEP_LEXICOGRAPHIC)
	    return true;
	  return false;
	default:
	  return false;
	 }

    default:
      return false;
    }

#undef OP_SAME
#undef OP_SAME_WITH_NULL
}

/* Generate a hash value for an expression.  This can be used iteratively
   by passing a previous result as the HSTATE argument.  */

void
operand_compare::hash_operand (const_tree t, inchash::hash &hstate,
			       unsigned int flags)
{
  int i;
  enum tree_code code;
  enum tree_code_class tclass;

  if (t == NULL_TREE || t == error_mark_node)
    {
      hstate.merge_hash (0);
      return;
    }

  STRIP_ANY_LOCATION_WRAPPER (t);

  if (!(flags & OEP_ADDRESS_OF))
    STRIP_NOPS (t);

  code = TREE_CODE (t);

  switch (code)
    {
    /* Alas, constants aren't shared, so we can't rely on pointer
       identity.  */
    case VOID_CST:
      hstate.merge_hash (0);
      return;
    case INTEGER_CST:
      gcc_checking_assert (!(flags & OEP_ADDRESS_OF));
      for (i = 0; i < TREE_INT_CST_EXT_NUNITS (t); i++)
	hstate.add_hwi (TREE_INT_CST_ELT (t, i));
      return;
    case REAL_CST:
      {
	unsigned int val2;
	if (!HONOR_SIGNED_ZEROS (t) && real_zerop (t))
	  val2 = rvc_zero;
	else
	  val2 = real_hash (TREE_REAL_CST_PTR (t));
	hstate.merge_hash (val2);
	return;
      }
    case FIXED_CST:
      {
	unsigned int val2 = fixed_hash (TREE_FIXED_CST_PTR (t));
	hstate.merge_hash (val2);
	return;
      }
    case STRING_CST:
      hstate.add ((const void *) TREE_STRING_POINTER (t),
		  TREE_STRING_LENGTH (t));
      return;
    case COMPLEX_CST:
      hash_operand (TREE_REALPART (t), hstate, flags);
      hash_operand (TREE_IMAGPART (t), hstate, flags);
      return;
    case VECTOR_CST:
      {
	hstate.add_int (VECTOR_CST_NPATTERNS (t));
	hstate.add_int (VECTOR_CST_NELTS_PER_PATTERN (t));
	unsigned int count = vector_cst_encoded_nelts (t);
	for (unsigned int i = 0; i < count; ++i)
	  hash_operand (VECTOR_CST_ENCODED_ELT (t, i), hstate, flags);
	return;
      }
    case SSA_NAME:
      /* We can just compare by pointer.  */
      hstate.add_hwi (SSA_NAME_VERSION (t));
      return;
    case PLACEHOLDER_EXPR:
      /* The node itself doesn't matter.  */
      return;
    case BLOCK:
    case OMP_CLAUSE:
      /* Ignore.  */
      return;
    case TREE_LIST:
      /* A list of expressions, for a CALL_EXPR or as the elements of a
	 VECTOR_CST.  */
      for (; t; t = TREE_CHAIN (t))
	hash_operand (TREE_VALUE (t), hstate, flags);
      return;
    case CONSTRUCTOR:
      {
	unsigned HOST_WIDE_INT idx;
	tree field, value;
	flags &= ~OEP_ADDRESS_OF;
	hstate.add_int (CONSTRUCTOR_NO_CLEARING (t));
	FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (t), idx, field, value)
	  {
	    /* In GIMPLE the indexes can be either NULL or matching i.  */
	    if (field == NULL_TREE)
	      field = bitsize_int (idx);
	    if (TREE_CODE (field) == FIELD_DECL)
	      {
		hash_operand (DECL_FIELD_OFFSET (field), hstate, flags);
		hash_operand (DECL_FIELD_BIT_OFFSET (field), hstate, flags);
	      }
	    else
	      hash_operand (field, hstate, flags);
	    hash_operand (value, hstate, flags);
	  }
	return;
      }
    case STATEMENT_LIST:
      {
	tree_stmt_iterator i;
	for (i = tsi_start (CONST_CAST_TREE (t));
	     !tsi_end_p (i); tsi_next (&i))
	  hash_operand (tsi_stmt (i), hstate, flags);
	return;
      }
    case TREE_VEC:
      for (i = 0; i < TREE_VEC_LENGTH (t); ++i)
	hash_operand (TREE_VEC_ELT (t, i), hstate, flags);
      return;
    case IDENTIFIER_NODE:
      hstate.add_object (IDENTIFIER_HASH_VALUE (t));
      return;
    case FUNCTION_DECL:
      /* When referring to a built-in FUNCTION_DECL, use the __builtin__ form.
	 Otherwise nodes that compare equal according to operand_equal_p might
	 get different hash codes.  However, don't do this for machine specific
	 or front end builtins, since the function code is overloaded in those
	 cases.  */
      if (DECL_BUILT_IN_CLASS (t) == BUILT_IN_NORMAL
	  && builtin_decl_explicit_p (DECL_FUNCTION_CODE (t)))
	{
	  t = builtin_decl_explicit (DECL_FUNCTION_CODE (t));
	  code = TREE_CODE (t);
	}
      /* FALL THROUGH */
    default:
      if (POLY_INT_CST_P (t))
	{
	  for (unsigned int i = 0; i < NUM_POLY_INT_COEFFS; ++i)
	    hstate.add_wide_int (wi::to_wide (POLY_INT_CST_COEFF (t, i)));
	  return;
	}
      tclass = TREE_CODE_CLASS (code);

      if (tclass == tcc_declaration)
	{
	  /* DECL's have a unique ID */
	  hstate.add_hwi (DECL_UID (t));
	}
      else if (tclass == tcc_comparison && !commutative_tree_code (code))
	{
	  /* For comparisons that can be swapped, use the lower
	     tree code.  */
	  enum tree_code ccode = swap_tree_comparison (code);
	  if (code < ccode)
	    ccode = code;
	  hstate.add_object (ccode);
	  hash_operand (TREE_OPERAND (t, ccode != code), hstate, flags);
	  hash_operand (TREE_OPERAND (t, ccode == code), hstate, flags);
	}
      else if (CONVERT_EXPR_CODE_P (code))
	{
	  /* NOP_EXPR and CONVERT_EXPR are considered equal by
	     operand_equal_p.  */
	  enum tree_code ccode = NOP_EXPR;
	  hstate.add_object (ccode);

	  /* Don't hash the type, that can lead to having nodes which
	     compare equal according to operand_equal_p, but which
	     have different hash codes.  Make sure to include signedness
	     in the hash computation.  */
	  hstate.add_int (TYPE_UNSIGNED (TREE_TYPE (t)));
	  hash_operand (TREE_OPERAND (t, 0), hstate, flags);
	}
      /* For OEP_ADDRESS_OF, hash MEM_EXPR[&decl, 0] the same as decl.  */
      else if (code == MEM_REF
	       && (flags & OEP_ADDRESS_OF) != 0
	       && TREE_CODE (TREE_OPERAND (t, 0)) == ADDR_EXPR
	       && DECL_P (TREE_OPERAND (TREE_OPERAND (t, 0), 0))
	       && integer_zerop (TREE_OPERAND (t, 1)))
	hash_operand (TREE_OPERAND (TREE_OPERAND (t, 0), 0),
		      hstate, flags);
      /* Don't ICE on FE specific trees, or their arguments etc.
	 during operand_equal_p hash verification.  */
      else if (!IS_EXPR_CODE_CLASS (tclass))
	gcc_assert (flags & OEP_HASH_CHECK);
      else
	{
	  unsigned int sflags = flags;

	  hstate.add_object (code);

	  switch (code)
	    {
	    case ADDR_EXPR:
	      gcc_checking_assert (!(flags & OEP_ADDRESS_OF));
	      flags |= OEP_ADDRESS_OF;
	      sflags = flags;
	      break;

	    case INDIRECT_REF:
	    case MEM_REF:
	    case TARGET_MEM_REF:
	      flags &= ~OEP_ADDRESS_OF;
	      sflags = flags;
	      break;

	    case COMPONENT_REF:
	      if (sflags & OEP_ADDRESS_OF)
		{
		  hash_operand (TREE_OPERAND (t, 0), hstate, flags);
		  hash_operand (DECL_FIELD_OFFSET (TREE_OPERAND (t, 1)),
				hstate, flags & ~OEP_ADDRESS_OF);
		  hash_operand (DECL_FIELD_BIT_OFFSET (TREE_OPERAND (t, 1)),
				hstate, flags & ~OEP_ADDRESS_OF);
		  return;
		}
	      break;
	    case ARRAY_REF:
	    case ARRAY_RANGE_REF:
	    case BIT_FIELD_REF:
	      sflags &= ~OEP_ADDRESS_OF;
	      break;

	    case COND_EXPR:
	      flags &= ~OEP_ADDRESS_OF;
	      break;

	    case WIDEN_MULT_PLUS_EXPR:
	    case WIDEN_MULT_MINUS_EXPR:
	      {
		/* The multiplication operands are commutative.  */
		inchash::hash one, two;
		hash_operand (TREE_OPERAND (t, 0), one, flags);
		hash_operand (TREE_OPERAND (t, 1), two, flags);
		hstate.add_commutative (one, two);
		hash_operand (TREE_OPERAND (t, 2), hstate, flags);
		return;
	      }

	    case CALL_EXPR:
	      if (CALL_EXPR_FN (t) == NULL_TREE)
		hstate.add_int (CALL_EXPR_IFN (t));
	      break;

	    case TARGET_EXPR:
	      /* For TARGET_EXPR, just hash on the TARGET_EXPR_SLOT.
		 Usually different TARGET_EXPRs just should use
		 different temporaries in their slots.  */
	      hash_operand (TARGET_EXPR_SLOT (t), hstate, flags);
	      return;

	    case OBJ_TYPE_REF:
	    /* Virtual table reference.  */
	      inchash::add_expr (OBJ_TYPE_REF_EXPR (t), hstate, flags);
	      flags &= ~OEP_ADDRESS_OF;
	      inchash::add_expr (OBJ_TYPE_REF_TOKEN (t), hstate, flags);
	      inchash::add_expr (OBJ_TYPE_REF_OBJECT (t), hstate, flags);
	      if (!virtual_method_call_p (t))
		return;
	      if (tree c = obj_type_ref_class (t))
		{
		  c = TYPE_NAME (TYPE_MAIN_VARIANT (c));
		  /* We compute mangled names only when free_lang_data is run.
		     In that case we can hash precisely.  */
		  if (TREE_CODE (c) == TYPE_DECL
		      && DECL_ASSEMBLER_NAME_SET_P (c))
		    hstate.add_object
			   (IDENTIFIER_HASH_VALUE
				   (DECL_ASSEMBLER_NAME (c)));
		}
	      return;
	    default:
	      break;
	    }

	  /* Don't hash the type, that can lead to having nodes which
	     compare equal according to operand_equal_p, but which
	     have different hash codes.  */
	  if (code == NON_LVALUE_EXPR)
	    {
	      /* Make sure to include signness in the hash computation.  */
	      hstate.add_int (TYPE_UNSIGNED (TREE_TYPE (t)));
	      hash_operand (TREE_OPERAND (t, 0), hstate, flags);
	    }

	  else if (commutative_tree_code (code))
	    {
	      /* It's a commutative expression.  We want to hash it the same
		 however it appears.  We do this by first hashing both operands
		 and then rehashing based on the order of their independent
		 hashes.  */
	      inchash::hash one, two;
	      hash_operand (TREE_OPERAND (t, 0), one, flags);
	      hash_operand (TREE_OPERAND (t, 1), two, flags);
	      hstate.add_commutative (one, two);
	    }
	  else
	    for (i = TREE_OPERAND_LENGTH (t) - 1; i >= 0; --i)
	      hash_operand (TREE_OPERAND (t, i), hstate,
			    i == 0 ? flags : sflags);
	}
      return;
    }
}

bool
operand_compare::verify_hash_value (const_tree arg0, const_tree arg1,
				    unsigned int flags, bool *ret)
{
  /* When checking and unless comparing DECL names, verify that if
     the outermost operand_equal_p call returns non-zero then ARG0
     and ARG1 have the same hash value.  */
  if (flag_checking && !(flags & OEP_NO_HASH_CHECK))
    {
      if (operand_equal_p (arg0, arg1, flags | OEP_NO_HASH_CHECK))
	{
	  if (arg0 != arg1 && !(flags & OEP_DECL_NAME))
	    {
	      inchash::hash hstate0 (0), hstate1 (0);
	      hash_operand (arg0, hstate0, flags | OEP_HASH_CHECK);
	      hash_operand (arg1, hstate1, flags | OEP_HASH_CHECK);
	      hashval_t h0 = hstate0.end ();
	      hashval_t h1 = hstate1.end ();
	      gcc_assert (h0 == h1);
	    }
	  *ret = true;
	}
      else
	*ret = false;

      return true;
    }

  return false;
}


static operand_compare default_compare_instance;

/* Conveinece wrapper around operand_compare class because usually we do
   not need to play with the valueizer.  */

bool
operand_equal_p (const_tree arg0, const_tree arg1, unsigned int flags)
{
  return default_compare_instance.operand_equal_p (arg0, arg1, flags);
}

namespace inchash
{

/* Generate a hash value for an expression.  This can be used iteratively
   by passing a previous result as the HSTATE argument.

   This function is intended to produce the same hash for expressions which
   would compare equal using operand_equal_p.  */
void
add_expr (const_tree t, inchash::hash &hstate, unsigned int flags)
{
  default_compare_instance.hash_operand (t, hstate, flags);
}

}

/* Similar to operand_equal_p, but see if ARG0 might be a variant of ARG1
   with a different signedness or a narrower precision.  */

static bool
operand_equal_for_comparison_p (tree arg0, tree arg1)
{
  if (operand_equal_p (arg0, arg1, 0))
    return true;

  if (! INTEGRAL_TYPE_P (TREE_TYPE (arg0))
      || ! INTEGRAL_TYPE_P (TREE_TYPE (arg1)))
    return false;

  /* Discard any conversions that don't change the modes of ARG0 and ARG1
     and see if the inner values are the same.  This removes any
     signedness comparison, which doesn't matter here.  */
  tree op0 = arg0;
  tree op1 = arg1;
  STRIP_NOPS (op0);
  STRIP_NOPS (op1);
  if (operand_equal_p (op0, op1, 0))
    return true;

  /* Discard a single widening conversion from ARG1 and see if the inner
     value is the same as ARG0.  */
  if (CONVERT_EXPR_P (arg1)
      && INTEGRAL_TYPE_P (TREE_TYPE (TREE_OPERAND (arg1, 0)))
      && TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (arg1, 0)))
         < TYPE_PRECISION (TREE_TYPE (arg1))
      && operand_equal_p (arg0, TREE_OPERAND (arg1, 0), 0))
    return true;

  return false;
}

/* See if ARG is an expression that is either a comparison or is performing
   arithmetic on comparisons.  The comparisons must only be comparing
   two different values, which will be stored in *CVAL1 and *CVAL2; if
   they are nonzero it means that some operands have already been found.
   No variables may be used anywhere else in the expression except in the
   comparisons.  

   If this is true, return 1.  Otherwise, return zero.  */

static bool
twoval_comparison_p (tree arg, tree *cval1, tree *cval2)
{
  enum tree_code code = TREE_CODE (arg);
  enum tree_code_class tclass = TREE_CODE_CLASS (code);

  /* We can handle some of the tcc_expression cases here.  */
  if (tclass == tcc_expression && code == TRUTH_NOT_EXPR)
    tclass = tcc_unary;
  else if (tclass == tcc_expression
	   && (code == TRUTH_ANDIF_EXPR || code == TRUTH_ORIF_EXPR
	       || code == COMPOUND_EXPR))
    tclass = tcc_binary;

  switch (tclass)
    {
    case tcc_unary:
      return twoval_comparison_p (TREE_OPERAND (arg, 0), cval1, cval2);

    case tcc_binary:
      return (twoval_comparison_p (TREE_OPERAND (arg, 0), cval1, cval2)
	      && twoval_comparison_p (TREE_OPERAND (arg, 1), cval1, cval2));

    case tcc_constant:
      return true;

    case tcc_expression:
      if (code == COND_EXPR)
	return (twoval_comparison_p (TREE_OPERAND (arg, 0), cval1, cval2)
		&& twoval_comparison_p (TREE_OPERAND (arg, 1), cval1, cval2)
		&& twoval_comparison_p (TREE_OPERAND (arg, 2), cval1, cval2));
      return false;

    case tcc_comparison:
      /* First see if we can handle the first operand, then the second.  For
	 the second operand, we know *CVAL1 can't be zero.  It must be that
	 one side of the comparison is each of the values; test for the
	 case where this isn't true by failing if the two operands
	 are the same.  */

      if (operand_equal_p (TREE_OPERAND (arg, 0),
			   TREE_OPERAND (arg, 1), 0))
	return false;

      if (*cval1 == 0)
	*cval1 = TREE_OPERAND (arg, 0);
      else if (operand_equal_p (*cval1, TREE_OPERAND (arg, 0), 0))
	;
      else if (*cval2 == 0)
	*cval2 = TREE_OPERAND (arg, 0);
      else if (operand_equal_p (*cval2, TREE_OPERAND (arg, 0), 0))
	;
      else
	return false;

      if (operand_equal_p (*cval1, TREE_OPERAND (arg, 1), 0))
	;
      else if (*cval2 == 0)
	*cval2 = TREE_OPERAND (arg, 1);
      else if (operand_equal_p (*cval2, TREE_OPERAND (arg, 1), 0))
	;
      else
	return false;

      return true;

    default:
      return false;
    }
}

/* ARG is a tree that is known to contain just arithmetic operations and
   comparisons.  Evaluate the operations in the tree substituting NEW0 for
   any occurrence of OLD0 as an operand of a comparison and likewise for
   NEW1 and OLD1.  */

static tree
eval_subst (location_t loc, tree arg, tree old0, tree new0,
	    tree old1, tree new1)
{
  tree type = TREE_TYPE (arg);
  enum tree_code code = TREE_CODE (arg);
  enum tree_code_class tclass = TREE_CODE_CLASS (code);

  /* We can handle some of the tcc_expression cases here.  */
  if (tclass == tcc_expression && code == TRUTH_NOT_EXPR)
    tclass = tcc_unary;
  else if (tclass == tcc_expression
	   && (code == TRUTH_ANDIF_EXPR || code == TRUTH_ORIF_EXPR))
    tclass = tcc_binary;

  switch (tclass)
    {
    case tcc_unary:
      return fold_build1_loc (loc, code, type,
			  eval_subst (loc, TREE_OPERAND (arg, 0),
				      old0, new0, old1, new1));

    case tcc_binary:
      return fold_build2_loc (loc, code, type,
			  eval_subst (loc, TREE_OPERAND (arg, 0),
				      old0, new0, old1, new1),
			  eval_subst (loc, TREE_OPERAND (arg, 1),
				      old0, new0, old1, new1));

    case tcc_expression:
      switch (code)
	{
	case SAVE_EXPR:
	  return eval_subst (loc, TREE_OPERAND (arg, 0), old0, new0,
			     old1, new1);

	case COMPOUND_EXPR:
	  return eval_subst (loc, TREE_OPERAND (arg, 1), old0, new0,
			     old1, new1);

	case COND_EXPR:
	  return fold_build3_loc (loc, code, type,
			      eval_subst (loc, TREE_OPERAND (arg, 0),
					  old0, new0, old1, new1),
			      eval_subst (loc, TREE_OPERAND (arg, 1),
					  old0, new0, old1, new1),
			      eval_subst (loc, TREE_OPERAND (arg, 2),
					  old0, new0, old1, new1));
	default:
	  break;
	}
      /* Fall through - ???  */

    case tcc_comparison:
      {
	tree arg0 = TREE_OPERAND (arg, 0);
	tree arg1 = TREE_OPERAND (arg, 1);

	/* We need to check both for exact equality and tree equality.  The
	   former will be true if the operand has a side-effect.  In that
	   case, we know the operand occurred exactly once.  */

	if (arg0 == old0 || operand_equal_p (arg0, old0, 0))
	  arg0 = new0;
	else if (arg0 == old1 || operand_equal_p (arg0, old1, 0))
	  arg0 = new1;

	if (arg1 == old0 || operand_equal_p (arg1, old0, 0))
	  arg1 = new0;
	else if (arg1 == old1 || operand_equal_p (arg1, old1, 0))
	  arg1 = new1;

	return fold_build2_loc (loc, code, type, arg0, arg1);
      }

    default:
      return arg;
    }
}

/* Return a tree for the case when the result of an expression is RESULT
   converted to TYPE and OMITTED was previously an operand of the expression
   but is now not needed (e.g., we folded OMITTED * 0).

   If OMITTED has side effects, we must evaluate it.  Otherwise, just do
   the conversion of RESULT to TYPE.  */

tree
omit_one_operand_loc (location_t loc, tree type, tree result, tree omitted)
{
  tree t = fold_convert_loc (loc, type, result);

  /* If the resulting operand is an empty statement, just return the omitted
     statement casted to void. */
  if (IS_EMPTY_STMT (t) && TREE_SIDE_EFFECTS (omitted))
    return build1_loc (loc, NOP_EXPR, void_type_node,
		       fold_ignored_result (omitted));

  if (TREE_SIDE_EFFECTS (omitted))
    return build2_loc (loc, COMPOUND_EXPR, type,
		       fold_ignored_result (omitted), t);

  return non_lvalue_loc (loc, t);
}

/* Return a tree for the case when the result of an expression is RESULT
   converted to TYPE and OMITTED1 and OMITTED2 were previously operands
   of the expression but are now not needed.

   If OMITTED1 or OMITTED2 has side effects, they must be evaluated.
   If both OMITTED1 and OMITTED2 have side effects, OMITTED1 is
   evaluated before OMITTED2.  Otherwise, if neither has side effects,
   just do the conversion of RESULT to TYPE.  */

tree
omit_two_operands_loc (location_t loc, tree type, tree result,
		       tree omitted1, tree omitted2)
{
  tree t = fold_convert_loc (loc, type, result);

  if (TREE_SIDE_EFFECTS (omitted2))
    t = build2_loc (loc, COMPOUND_EXPR, type, omitted2, t);
  if (TREE_SIDE_EFFECTS (omitted1))
    t = build2_loc (loc, COMPOUND_EXPR, type, omitted1, t);

  return TREE_CODE (t) != COMPOUND_EXPR ? non_lvalue_loc (loc, t) : t;
}


/* Return a simplified tree node for the truth-negation of ARG.  This
   never alters ARG itself.  We assume that ARG is an operation that
   returns a truth value (0 or 1).

   FIXME: one would think we would fold the result, but it causes
   problems with the dominator optimizer.  */

static tree
fold_truth_not_expr (location_t loc, tree arg)
{
  tree type = TREE_TYPE (arg);
  enum tree_code code = TREE_CODE (arg);
  location_t loc1, loc2;

  /* If this is a comparison, we can simply invert it, except for
     floating-point non-equality comparisons, in which case we just
     enclose a TRUTH_NOT_EXPR around what we have.  */

  if (TREE_CODE_CLASS (code) == tcc_comparison)
    {
      tree op_type = TREE_TYPE (TREE_OPERAND (arg, 0));
      if (FLOAT_TYPE_P (op_type)
	  && flag_trapping_math
	  && code != ORDERED_EXPR && code != UNORDERED_EXPR
	  && code != NE_EXPR && code != EQ_EXPR)
	return NULL_TREE;

      code = invert_tree_comparison (code, HONOR_NANS (op_type));
      if (code == ERROR_MARK)
	return NULL_TREE;

      tree ret = build2_loc (loc, code, type, TREE_OPERAND (arg, 0),
			     TREE_OPERAND (arg, 1));
      copy_warning (ret, arg);
      return ret;
    }

  switch (code)
    {
    case INTEGER_CST:
      return constant_boolean_node (integer_zerop (arg), type);

    case TRUTH_AND_EXPR:
      loc1 = expr_location_or (TREE_OPERAND (arg, 0), loc);
      loc2 = expr_location_or (TREE_OPERAND (arg, 1), loc);
      return build2_loc (loc, TRUTH_OR_EXPR, type,
			 invert_truthvalue_loc (loc1, TREE_OPERAND (arg, 0)),
			 invert_truthvalue_loc (loc2, TREE_OPERAND (arg, 1)));

    case TRUTH_OR_EXPR:
      loc1 = expr_location_or (TREE_OPERAND (arg, 0), loc);
      loc2 = expr_location_or (TREE_OPERAND (arg, 1), loc);
      return build2_loc (loc, TRUTH_AND_EXPR, type,
			 invert_truthvalue_loc (loc1, TREE_OPERAND (arg, 0)),
			 invert_truthvalue_loc (loc2, TREE_OPERAND (arg, 1)));

    case TRUTH_XOR_EXPR:
      /* Here we can invert either operand.  We invert the first operand
	 unless the second operand is a TRUTH_NOT_EXPR in which case our
	 result is the XOR of the first operand with the inside of the
	 negation of the second operand.  */

      if (TREE_CODE (TREE_OPERAND (arg, 1)) == TRUTH_NOT_EXPR)
	return build2_loc (loc, TRUTH_XOR_EXPR, type, TREE_OPERAND (arg, 0),
			   TREE_OPERAND (TREE_OPERAND (arg, 1), 0));
      else
	return build2_loc (loc, TRUTH_XOR_EXPR, type,
			   invert_truthvalue_loc (loc, TREE_OPERAND (arg, 0)),
			   TREE_OPERAND (arg, 1));

    case TRUTH_ANDIF_EXPR:
      loc1 = expr_location_or (TREE_OPERAND (arg, 0), loc);
      loc2 = expr_location_or (TREE_OPERAND (arg, 1), loc);
      return build2_loc (loc, TRUTH_ORIF_EXPR, type,
			 invert_truthvalue_loc (loc1, TREE_OPERAND (arg, 0)),
			 invert_truthvalue_loc (loc2, TREE_OPERAND (arg, 1)));

    case TRUTH_ORIF_EXPR:
      loc1 = expr_location_or (TREE_OPERAND (arg, 0), loc);
      loc2 = expr_location_or (TREE_OPERAND (arg, 1), loc);
      return build2_loc (loc, TRUTH_ANDIF_EXPR, type,
			 invert_truthvalue_loc (loc1, TREE_OPERAND (arg, 0)),
			 invert_truthvalue_loc (loc2, TREE_OPERAND (arg, 1)));

    case TRUTH_NOT_EXPR:
      return TREE_OPERAND (arg, 0);

    case COND_EXPR:
      {
	tree arg1 = TREE_OPERAND (arg, 1);
	tree arg2 = TREE_OPERAND (arg, 2);

	loc1 = expr_location_or (TREE_OPERAND (arg, 1), loc);
	loc2 = expr_location_or (TREE_OPERAND (arg, 2), loc);

	/* A COND_EXPR may have a throw as one operand, which
	   then has void type.  Just leave void operands
	   as they are.  */
	return build3_loc (loc, COND_EXPR, type, TREE_OPERAND (arg, 0),
			   VOID_TYPE_P (TREE_TYPE (arg1))
			   ? arg1 : invert_truthvalue_loc (loc1, arg1),
			   VOID_TYPE_P (TREE_TYPE (arg2))
			   ? arg2 : invert_truthvalue_loc (loc2, arg2));
      }

    case COMPOUND_EXPR:
      loc1 = expr_location_or (TREE_OPERAND (arg, 1), loc);
      return build2_loc (loc, COMPOUND_EXPR, type,
			 TREE_OPERAND (arg, 0),
			 invert_truthvalue_loc (loc1, TREE_OPERAND (arg, 1)));

    case NON_LVALUE_EXPR:
      loc1 = expr_location_or (TREE_OPERAND (arg, 0), loc);
      return invert_truthvalue_loc (loc1, TREE_OPERAND (arg, 0));

    CASE_CONVERT:
      if (TREE_CODE (TREE_TYPE (arg)) == BOOLEAN_TYPE)
	return build1_loc (loc, TRUTH_NOT_EXPR, type, arg);

      /* fall through */

    case FLOAT_EXPR:
      loc1 = expr_location_or (TREE_OPERAND (arg, 0), loc);
      return build1_loc (loc, TREE_CODE (arg), type,
			 invert_truthvalue_loc (loc1, TREE_OPERAND (arg, 0)));

    case BIT_AND_EXPR:
      if (!integer_onep (TREE_OPERAND (arg, 1)))
	return NULL_TREE;
      return build2_loc (loc, EQ_EXPR, type, arg, build_int_cst (type, 0));

    case SAVE_EXPR:
      return build1_loc (loc, TRUTH_NOT_EXPR, type, arg);

    case CLEANUP_POINT_EXPR:
      loc1 = expr_location_or (TREE_OPERAND (arg, 0), loc);
      return build1_loc (loc, CLEANUP_POINT_EXPR, type,
			 invert_truthvalue_loc (loc1, TREE_OPERAND (arg, 0)));

    default:
      return NULL_TREE;
    }
}

/* Fold the truth-negation of ARG.  This never alters ARG itself.  We
   assume that ARG is an operation that returns a truth value (0 or 1
   for scalars, 0 or -1 for vectors).  Return the folded expression if
   folding is successful.  Otherwise, return NULL_TREE.  */

static tree
fold_invert_truthvalue (location_t loc, tree arg)
{
  tree type = TREE_TYPE (arg);
  return fold_unary_loc (loc, VECTOR_TYPE_P (type)
			      ? BIT_NOT_EXPR
			      : TRUTH_NOT_EXPR,
			 type, arg);
}

/* Return a simplified tree node for the truth-negation of ARG.  This
   never alters ARG itself.  We assume that ARG is an operation that
   returns a truth value (0 or 1 for scalars, 0 or -1 for vectors).  */

tree
invert_truthvalue_loc (location_t loc, tree arg)
{
  if (TREE_CODE (arg) == ERROR_MARK)
    return arg;

  tree type = TREE_TYPE (arg);
  return fold_build1_loc (loc, VECTOR_TYPE_P (type)
			       ? BIT_NOT_EXPR
			       : TRUTH_NOT_EXPR,
			  type, arg);
}

/* Return a BIT_FIELD_REF of type TYPE to refer to BITSIZE bits of INNER
   starting at BITPOS.  The field is unsigned if UNSIGNEDP is nonzero
   and uses reverse storage order if REVERSEP is nonzero.  ORIG_INNER
   is the original memory reference used to preserve the alias set of
   the access.  */

static tree
make_bit_field_ref (location_t loc, tree inner, tree orig_inner, tree type,
		    HOST_WIDE_INT bitsize, poly_int64 bitpos,
		    int unsignedp, int reversep)
{
  tree result, bftype;

  /* Attempt not to lose the access path if possible.  */
  if (TREE_CODE (orig_inner) == COMPONENT_REF)
    {
      tree ninner = TREE_OPERAND (orig_inner, 0);
      machine_mode nmode;
      poly_int64 nbitsize, nbitpos;
      tree noffset;
      int nunsignedp, nreversep, nvolatilep = 0;
      tree base = get_inner_reference (ninner, &nbitsize, &nbitpos,
				       &noffset, &nmode, &nunsignedp,
				       &nreversep, &nvolatilep);
      if (base == inner
	  && noffset == NULL_TREE
	  && known_subrange_p (bitpos, bitsize, nbitpos, nbitsize)
	  && !reversep
	  && !nreversep
	  && !nvolatilep)
	{
	  inner = ninner;
	  bitpos -= nbitpos;
	}
    }

  alias_set_type iset = get_alias_set (orig_inner);
  if (iset == 0 && get_alias_set (inner) != iset)
    inner = fold_build2 (MEM_REF, TREE_TYPE (inner),
			 build_fold_addr_expr (inner),
			 build_int_cst (ptr_type_node, 0));

  if (known_eq (bitpos, 0) && !reversep)
    {
      tree size = TYPE_SIZE (TREE_TYPE (inner));
      if ((INTEGRAL_TYPE_P (TREE_TYPE (inner))
	   || POINTER_TYPE_P (TREE_TYPE (inner)))
	  && tree_fits_shwi_p (size)
	  && tree_to_shwi (size) == bitsize)
	return fold_convert_loc (loc, type, inner);
    }

  bftype = type;
  if (TYPE_PRECISION (bftype) != bitsize
      || TYPE_UNSIGNED (bftype) == !unsignedp)
    bftype = build_nonstandard_integer_type (bitsize, 0);

  result = build3_loc (loc, BIT_FIELD_REF, bftype, inner,
		       bitsize_int (bitsize), bitsize_int (bitpos));
  REF_REVERSE_STORAGE_ORDER (result) = reversep;

  if (bftype != type)
    result = fold_convert_loc (loc, type, result);

  return result;
}

/* Optimize a bit-field compare.

   There are two cases:  First is a compare against a constant and the
   second is a comparison of two items where the fields are at the same
   bit position relative to the start of a chunk (byte, halfword, word)
   large enough to contain it.  In these cases we can avoid the shift
   implicit in bitfield extractions.

   For constants, we emit a compare of the shifted constant with the
   BIT_AND_EXPR of a mask and a byte, halfword, or word of the operand being
   compared.  For two fields at the same position, we do the ANDs with the
   similar mask and compare the result of the ANDs.

   CODE is the comparison code, known to be either NE_EXPR or EQ_EXPR.
   COMPARE_TYPE is the type of the comparison, and LHS and RHS
   are the left and right operands of the comparison, respectively.

   If the optimization described above can be done, we return the resulting
   tree.  Otherwise we return zero.  */

static tree
optimize_bit_field_compare (location_t loc, enum tree_code code,
			    tree compare_type, tree lhs, tree rhs)
{
  poly_int64 plbitpos, plbitsize, rbitpos, rbitsize;
  HOST_WIDE_INT lbitpos, lbitsize, nbitpos, nbitsize;
  tree type = TREE_TYPE (lhs);
  tree unsigned_type;
  int const_p = TREE_CODE (rhs) == INTEGER_CST;
  machine_mode lmode, rmode;
  scalar_int_mode nmode;
  int lunsignedp, runsignedp;
  int lreversep, rreversep;
  int lvolatilep = 0, rvolatilep = 0;
  tree linner, rinner = NULL_TREE;
  tree mask;
  tree offset;

  /* Get all the information about the extractions being done.  If the bit size
     is the same as the size of the underlying object, we aren't doing an
     extraction at all and so can do nothing.  We also don't want to
     do anything if the inner expression is a PLACEHOLDER_EXPR since we
     then will no longer be able to replace it.  */
  linner = get_inner_reference (lhs, &plbitsize, &plbitpos, &offset, &lmode,
				&lunsignedp, &lreversep, &lvolatilep);
  if (linner == lhs
      || !known_size_p (plbitsize)
      || !plbitsize.is_constant (&lbitsize)
      || !plbitpos.is_constant (&lbitpos)
      || known_eq (lbitsize, GET_MODE_BITSIZE (lmode))
      || offset != 0
      || TREE_CODE (linner) == PLACEHOLDER_EXPR
      || lvolatilep)
    return 0;

  if (const_p)
    rreversep = lreversep;
  else
   {
     /* If this is not a constant, we can only do something if bit positions,
	sizes, signedness and storage order are the same.  */
     rinner
       = get_inner_reference (rhs, &rbitsize, &rbitpos, &offset, &rmode,
			      &runsignedp, &rreversep, &rvolatilep);

     if (rinner == rhs
	 || maybe_ne (lbitpos, rbitpos)
	 || maybe_ne (lbitsize, rbitsize)
	 || lunsignedp != runsignedp
	 || lreversep != rreversep
	 || offset != 0
	 || TREE_CODE (rinner) == PLACEHOLDER_EXPR
	 || rvolatilep)
       return 0;
   }

  /* Honor the C++ memory model and mimic what RTL expansion does.  */
  poly_uint64 bitstart = 0;
  poly_uint64 bitend = 0;
  if (TREE_CODE (lhs) == COMPONENT_REF)
    {
      get_bit_range (&bitstart, &bitend, lhs, &plbitpos, &offset);
      if (!plbitpos.is_constant (&lbitpos) || offset != NULL_TREE)
	return 0;
    }

  /* See if we can find a mode to refer to this field.  We should be able to,
     but fail if we can't.  */
  if (!get_best_mode (lbitsize, lbitpos, bitstart, bitend,
		      const_p ? TYPE_ALIGN (TREE_TYPE (linner))
		      : MIN (TYPE_ALIGN (TREE_TYPE (linner)),
			     TYPE_ALIGN (TREE_TYPE (rinner))),
		      BITS_PER_WORD, false, &nmode))
    return 0;

  /* Set signed and unsigned types of the precision of this mode for the
     shifts below.  */
  unsigned_type = lang_hooks.types.type_for_mode (nmode, 1);

  /* Compute the bit position and size for the new reference and our offset
     within it. If the new reference is the same size as the original, we
     won't optimize anything, so return zero.  */
  nbitsize = GET_MODE_BITSIZE (nmode);
  nbitpos = lbitpos & ~ (nbitsize - 1);
  lbitpos -= nbitpos;
  if (nbitsize == lbitsize)
    return 0;

  if (lreversep ? !BYTES_BIG_ENDIAN : BYTES_BIG_ENDIAN)
    lbitpos = nbitsize - lbitsize - lbitpos;

  /* Make the mask to be used against the extracted field.  */
  mask = build_int_cst_type (unsigned_type, -1);
  mask = const_binop (LSHIFT_EXPR, mask, size_int (nbitsize - lbitsize));
  mask = const_binop (RSHIFT_EXPR, mask,
		      size_int (nbitsize - lbitsize - lbitpos));

  if (! const_p)
    {
      if (nbitpos < 0)
	return 0;

      /* If not comparing with constant, just rework the comparison
	 and return.  */
      tree t1 = make_bit_field_ref (loc, linner, lhs, unsigned_type,
				    nbitsize, nbitpos, 1, lreversep);
      t1 = fold_build2_loc (loc, BIT_AND_EXPR, unsigned_type, t1, mask);
      tree t2 = make_bit_field_ref (loc, rinner, rhs, unsigned_type,
				    nbitsize, nbitpos, 1, rreversep);
      t2 = fold_build2_loc (loc, BIT_AND_EXPR, unsigned_type, t2, mask);
      return fold_build2_loc (loc, code, compare_type, t1, t2);
    }

  /* Otherwise, we are handling the constant case.  See if the constant is too
     big for the field.  Warn and return a tree for 0 (false) if so.  We do
     this not only for its own sake, but to avoid having to test for this
     error case below.  If we didn't, we might generate wrong code.

     For unsigned fields, the constant shifted right by the field length should
     be all zero.  For signed fields, the high-order bits should agree with
     the sign bit.  */

  if (lunsignedp)
    {
      if (wi::lrshift (wi::to_wide (rhs), lbitsize) != 0)
	{
	  warning (0, "comparison is always %d due to width of bit-field",
		   code == NE_EXPR);
	  return constant_boolean_node (code == NE_EXPR, compare_type);
	}
    }
  else
    {
      wide_int tem = wi::arshift (wi::to_wide (rhs), lbitsize - 1);
      if (tem != 0 && tem != -1)
	{
	  warning (0, "comparison is always %d due to width of bit-field",
		   code == NE_EXPR);
	  return constant_boolean_node (code == NE_EXPR, compare_type);
	}
    }

  if (nbitpos < 0)
    return 0;

  /* Single-bit compares should always be against zero.  */
  if (lbitsize == 1 && ! integer_zerop (rhs))
    {
      code = code == EQ_EXPR ? NE_EXPR : EQ_EXPR;
      rhs = build_int_cst (type, 0);
    }

  /* Make a new bitfield reference, shift the constant over the
     appropriate number of bits and mask it with the computed mask
     (in case this was a signed field).  If we changed it, make a new one.  */
  lhs = make_bit_field_ref (loc, linner, lhs, unsigned_type,
			    nbitsize, nbitpos, 1, lreversep);

  rhs = const_binop (BIT_AND_EXPR,
		     const_binop (LSHIFT_EXPR,
				  fold_convert_loc (loc, unsigned_type, rhs),
				  size_int (lbitpos)),
		     mask);

  lhs = build2_loc (loc, code, compare_type,
		    build2 (BIT_AND_EXPR, unsigned_type, lhs, mask), rhs);
  return lhs;
}

/* Subroutine for fold_truth_andor_1: decode a field reference.

   If EXP is a comparison reference, we return the innermost reference.

   *PBITSIZE is set to the number of bits in the reference, *PBITPOS is
   set to the starting bit number.

   If the innermost field can be completely contained in a mode-sized
   unit, *PMODE is set to that mode.  Otherwise, it is set to VOIDmode.

   *PVOLATILEP is set to 1 if the any expression encountered is volatile;
   otherwise it is not changed.

   *PUNSIGNEDP is set to the signedness of the field.

   *PREVERSEP is set to the storage order of the field.

   *PMASK is set to the mask used.  This is either contained in a
   BIT_AND_EXPR or derived from the width of the field.

   *PAND_MASK is set to the mask found in a BIT_AND_EXPR, if any.

   Return 0 if this is not a component reference or is one that we can't
   do anything with.  */

static tree
decode_field_reference (location_t loc, tree *exp_, HOST_WIDE_INT *pbitsize,
			HOST_WIDE_INT *pbitpos, machine_mode *pmode,
			int *punsignedp, int *preversep, int *pvolatilep,
			tree *pmask, tree *pand_mask)
{
  tree exp = *exp_;
  tree outer_type = 0;
  tree and_mask = 0;
  tree mask, inner, offset;
  tree unsigned_type;
  unsigned int precision;

  /* All the optimizations using this function assume integer fields.
     There are problems with FP fields since the type_for_size call
     below can fail for, e.g., XFmode.  */
  if (! INTEGRAL_TYPE_P (TREE_TYPE (exp)))
    return NULL_TREE;

  /* We are interested in the bare arrangement of bits, so strip everything
     that doesn't affect the machine mode.  However, record the type of the
     outermost expression if it may matter below.  */
  if (CONVERT_EXPR_P (exp)
      || TREE_CODE (exp) == NON_LVALUE_EXPR)
    outer_type = TREE_TYPE (exp);
  STRIP_NOPS (exp);

  if (TREE_CODE (exp) == BIT_AND_EXPR)
    {
      and_mask = TREE_OPERAND (exp, 1);
      exp = TREE_OPERAND (exp, 0);
      STRIP_NOPS (exp); STRIP_NOPS (and_mask);
      if (TREE_CODE (and_mask) != INTEGER_CST)
	return NULL_TREE;
    }

  poly_int64 poly_bitsize, poly_bitpos;
  inner = get_inner_reference (exp, &poly_bitsize, &poly_bitpos, &offset,
			       pmode, punsignedp, preversep, pvolatilep);
  if ((inner == exp && and_mask == 0)
      || !poly_bitsize.is_constant (pbitsize)
      || !poly_bitpos.is_constant (pbitpos)
      || *pbitsize < 0
      || offset != 0
      || TREE_CODE (inner) == PLACEHOLDER_EXPR
      /* We eventually want to build a larger reference and need to take
	 the address of this.  */
      || (!REFERENCE_CLASS_P (inner) && !DECL_P (inner))
      /* Reject out-of-bound accesses (PR79731).  */
      || (! AGGREGATE_TYPE_P (TREE_TYPE (inner))
	  && compare_tree_int (TYPE_SIZE (TREE_TYPE (inner)),
			       *pbitpos + *pbitsize) < 0))
    return NULL_TREE;

  unsigned_type = lang_hooks.types.type_for_size (*pbitsize, 1);
  if (unsigned_type == NULL_TREE)
    return NULL_TREE;

  *exp_ = exp;

  /* If the number of bits in the reference is the same as the bitsize of
     the outer type, then the outer type gives the signedness. Otherwise
     (in case of a small bitfield) the signedness is unchanged.  */
  if (outer_type && *pbitsize == TYPE_PRECISION (outer_type))
    *punsignedp = TYPE_UNSIGNED (outer_type);

  /* Compute the mask to access the bitfield.  */
  precision = TYPE_PRECISION (unsigned_type);

  mask = build_int_cst_type (unsigned_type, -1);

  mask = const_binop (LSHIFT_EXPR, mask, size_int (precision - *pbitsize));
  mask = const_binop (RSHIFT_EXPR, mask, size_int (precision - *pbitsize));

  /* Merge it with the mask we found in the BIT_AND_EXPR, if any.  */
  if (and_mask != 0)
    mask = fold_build2_loc (loc, BIT_AND_EXPR, unsigned_type,
			fold_convert_loc (loc, unsigned_type, and_mask), mask);

  *pmask = mask;
  *pand_mask = and_mask;
  return inner;
}

/* Return nonzero if MASK represents a mask of SIZE ones in the low-order
   bit positions and MASK is SIGNED.  */

static bool
all_ones_mask_p (const_tree mask, unsigned int size)
{
  tree type = TREE_TYPE (mask);
  unsigned int precision = TYPE_PRECISION (type);

  /* If this function returns true when the type of the mask is
     UNSIGNED, then there will be errors.  In particular see
     gcc.c-torture/execute/990326-1.c.  There does not appear to be
     any documentation paper trail as to why this is so.  But the pre
     wide-int worked with that restriction and it has been preserved
     here.  */
  if (size > precision || TYPE_SIGN (type) == UNSIGNED)
    return false;

  return wi::mask (size, false, precision) == wi::to_wide (mask);
}

/* Subroutine for fold: determine if VAL is the INTEGER_CONST that
   represents the sign bit of EXP's type.  If EXP represents a sign
   or zero extension, also test VAL against the unextended type.
   The return value is the (sub)expression whose sign bit is VAL,
   or NULL_TREE otherwise.  */

tree
sign_bit_p (tree exp, const_tree val)
{
  int width;
  tree t;

  /* Tree EXP must have an integral type.  */
  t = TREE_TYPE (exp);
  if (! INTEGRAL_TYPE_P (t))
    return NULL_TREE;

  /* Tree VAL must be an integer constant.  */
  if (TREE_CODE (val) != INTEGER_CST
      || TREE_OVERFLOW (val))
    return NULL_TREE;

  width = TYPE_PRECISION (t);
  if (wi::only_sign_bit_p (wi::to_wide (val), width))
    return exp;

  /* Handle extension from a narrower type.  */
  if (TREE_CODE (exp) == NOP_EXPR
      && TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (exp, 0))) < width)
    return sign_bit_p (TREE_OPERAND (exp, 0), val);

  return NULL_TREE;
}

/* Subroutine for fold_truth_andor_1 and simple_condition_p: determine if an
   operand is simple enough to be evaluated unconditionally.  */

static bool
simple_operand_p (const_tree exp)
{
  /* Strip any conversions that don't change the machine mode.  */
  STRIP_NOPS (exp);

  return (CONSTANT_CLASS_P (exp)
  	  || TREE_CODE (exp) == SSA_NAME
	  || (DECL_P (exp)
	      && ! TREE_ADDRESSABLE (exp)
	      && ! TREE_THIS_VOLATILE (exp)
	      && ! DECL_NONLOCAL (exp)
	      /* Don't regard global variables as simple.  They may be
		 allocated in ways unknown to the compiler (shared memory,
		 #pragma weak, etc).  */
	      && ! TREE_PUBLIC (exp)
	      && ! DECL_EXTERNAL (exp)
	      /* Weakrefs are not safe to be read, since they can be NULL.
 		 They are !TREE_PUBLIC && !DECL_EXTERNAL but still
		 have DECL_WEAK flag set.  */
	      && (! VAR_OR_FUNCTION_DECL_P (exp) || ! DECL_WEAK (exp))
	      /* Loading a static variable is unduly expensive, but global
		 registers aren't expensive.  */
	      && (! TREE_STATIC (exp) || DECL_REGISTER (exp))));
}

/* Determine if an operand is simple enough to be evaluated unconditionally.
   In addition to simple_operand_p, we assume that comparisons, conversions,
   and logic-not operations are simple, if their operands are simple, too.  */

bool
simple_condition_p (tree exp)
{
  enum tree_code code;

  if (TREE_SIDE_EFFECTS (exp) || generic_expr_could_trap_p (exp))
    return false;

  while (CONVERT_EXPR_P (exp))
    exp = TREE_OPERAND (exp, 0);

  code = TREE_CODE (exp);

  if (TREE_CODE_CLASS (code) == tcc_comparison)
    return (simple_operand_p (TREE_OPERAND (exp, 0))
	    && simple_operand_p (TREE_OPERAND (exp, 1)));

  if (code == TRUTH_NOT_EXPR)
    return simple_condition_p (TREE_OPERAND (exp, 0));

  return simple_operand_p (exp);
}


/* The following functions are subroutines to fold_range_test and allow it to
   try to change a logical combination of comparisons into a range test.

   For example, both
	X == 2 || X == 3 || X == 4 || X == 5
   and
	X >= 2 && X <= 5
   are converted to
	(unsigned) (X - 2) <= 3

   We describe each set of comparisons as being either inside or outside
   a range, using a variable named like IN_P, and then describe the
   range with a lower and upper bound.  If one of the bounds is omitted,
   it represents either the highest or lowest value of the type.

   In the comments below, we represent a range by two numbers in brackets
   preceded by a "+" to designate being inside that range, or a "-" to
   designate being outside that range, so the condition can be inverted by
   flipping the prefix.  An omitted bound is represented by a "-".  For
   example, "- [-, 10]" means being outside the range starting at the lowest
   possible value and ending at 10, in other words, being greater than 10.
   The range "+ [-, -]" is always true and hence the range "- [-, -]" is
   always false.

   We set up things so that the missing bounds are handled in a consistent
   manner so neither a missing bound nor "true" and "false" need to be
   handled using a special case.  */

/* Return the result of applying CODE to ARG0 and ARG1, but handle the case
   of ARG0 and/or ARG1 being omitted, meaning an unlimited range. UPPER0_P
   and UPPER1_P are nonzero if the respective argument is an upper bound
   and zero for a lower.  TYPE, if nonzero, is the type of the result; it
   must be specified for a comparison.  ARG1 will be converted to ARG0's
   type if both are specified.  */

static tree
range_binop (enum tree_code code, tree type, tree arg0, int upper0_p,
	     tree arg1, int upper1_p)
{
  tree tem;
  int result;
  int sgn0, sgn1;

  /* If neither arg represents infinity, do the normal operation.
     Else, if not a comparison, return infinity.  Else handle the special
     comparison rules. Note that most of the cases below won't occur, but
     are handled for consistency.  */

  if (arg0 != 0 && arg1 != 0)
    {
      tem = fold_build2 (code, type != 0 ? type : TREE_TYPE (arg0),
			 arg0, fold_convert (TREE_TYPE (arg0), arg1));
      STRIP_NOPS (tem);
      return TREE_CODE (tem) == INTEGER_CST ? tem : 0;
    }

  if (TREE_CODE_CLASS (code) != tcc_comparison)
    return 0;

  /* Set SGN[01] to -1 if ARG[01] is a lower bound, 1 for upper, and 0
     for neither.  In real maths, we cannot assume open ended ranges are
     the same. But, this is computer arithmetic, where numbers are finite.
     We can therefore make the transformation of any unbounded range with
     the value Z, Z being greater than any representable number. This permits
     us to treat unbounded ranges as equal.  */
  sgn0 = arg0 != 0 ? 0 : (upper0_p ? 1 : -1);
  sgn1 = arg1 != 0 ? 0 : (upper1_p ? 1 : -1);
  switch (code)
    {
    case EQ_EXPR:
      result = sgn0 == sgn1;
      break;
    case NE_EXPR:
      result = sgn0 != sgn1;
      break;
    case LT_EXPR:
      result = sgn0 < sgn1;
      break;
    case LE_EXPR:
      result = sgn0 <= sgn1;
      break;
    case GT_EXPR:
      result = sgn0 > sgn1;
      break;
    case GE_EXPR:
      result = sgn0 >= sgn1;
      break;
    default:
      gcc_unreachable ();
    }

  return constant_boolean_node (result, type);
}

/* Helper routine for make_range.  Perform one step for it, return
   new expression if the loop should continue or NULL_TREE if it should
   stop.  */

tree
make_range_step (location_t loc, enum tree_code code, tree arg0, tree arg1,
		 tree exp_type, tree *p_low, tree *p_high, int *p_in_p,
		 bool *strict_overflow_p)
{
  tree arg0_type = TREE_TYPE (arg0);
  tree n_low, n_high, low = *p_low, high = *p_high;
  int in_p = *p_in_p, n_in_p;

  switch (code)
    {
    case TRUTH_NOT_EXPR:
      /* We can only do something if the range is testing for zero.  */
      if (low == NULL_TREE || high == NULL_TREE
	  || ! integer_zerop (low) || ! integer_zerop (high))
	return NULL_TREE;
      *p_in_p = ! in_p;
      return arg0;

    case EQ_EXPR: case NE_EXPR:
    case LT_EXPR: case LE_EXPR: case GE_EXPR: case GT_EXPR:
      /* We can only do something if the range is testing for zero
	 and if the second operand is an integer constant.  Note that
	 saying something is "in" the range we make is done by
	 complementing IN_P since it will set in the initial case of
	 being not equal to zero; "out" is leaving it alone.  */
      if (low == NULL_TREE || high == NULL_TREE
	  || ! integer_zerop (low) || ! integer_zerop (high)
	  || TREE_CODE (arg1) != INTEGER_CST)
	return NULL_TREE;

      switch (code)
	{
	case NE_EXPR:  /* - [c, c]  */
	  low = high = arg1;
	  break;
	case EQ_EXPR:  /* + [c, c]  */
	  in_p = ! in_p, low = high = arg1;
	  break;
	case GT_EXPR:  /* - [-, c] */
	  low = 0, high = arg1;
	  break;
	case GE_EXPR:  /* + [c, -] */
	  in_p = ! in_p, low = arg1, high = 0;
	  break;
	case LT_EXPR:  /* - [c, -] */
	  low = arg1, high = 0;
	  break;
	case LE_EXPR:  /* + [-, c] */
	  in_p = ! in_p, low = 0, high = arg1;
	  break;
	default:
	  gcc_unreachable ();
	}

      /* If this is an unsigned comparison, we also know that EXP is
	 greater than or equal to zero.  We base the range tests we make
	 on that fact, so we record it here so we can parse existing
	 range tests.  We test arg0_type since often the return type
	 of, e.g. EQ_EXPR, is boolean.  */
      if (TYPE_UNSIGNED (arg0_type) && (low == 0 || high == 0))
	{
	  if (! merge_ranges (&n_in_p, &n_low, &n_high,
			      in_p, low, high, 1,
			      build_int_cst (arg0_type, 0),
			      NULL_TREE))
	    return NULL_TREE;

	  in_p = n_in_p, low = n_low, high = n_high;

	  /* If the high bound is missing, but we have a nonzero low
	     bound, reverse the range so it goes from zero to the low bound
	     minus 1.  */
	  if (high == 0 && low && ! integer_zerop (low))
	    {
	      in_p = ! in_p;
	      high = range_binop (MINUS_EXPR, NULL_TREE, low, 0,
				  build_int_cst (TREE_TYPE (low), 1), 0);
	      low = build_int_cst (arg0_type, 0);
	    }
	}

      *p_low = low;
      *p_high = high;
      *p_in_p = in_p;
      return arg0;

    case NEGATE_EXPR:
      /* If flag_wrapv and ARG0_TYPE is signed, make sure
	 low and high are non-NULL, then normalize will DTRT.  */
      if (!TYPE_UNSIGNED (arg0_type)
	  && !TYPE_OVERFLOW_UNDEFINED (arg0_type))
	{
	  if (low == NULL_TREE)
	    low = TYPE_MIN_VALUE (arg0_type);
	  if (high == NULL_TREE)
	    high = TYPE_MAX_VALUE (arg0_type);
	}

      /* (-x) IN [a,b] -> x in [-b, -a]  */
      n_low = range_binop (MINUS_EXPR, exp_type,
			   build_int_cst (exp_type, 0),
			   0, high, 1);
      n_high = range_binop (MINUS_EXPR, exp_type,
			    build_int_cst (exp_type, 0),
			    0, low, 0);
      if (n_high != 0 && TREE_OVERFLOW (n_high))
	return NULL_TREE;
      goto normalize;

    case BIT_NOT_EXPR:
      /* ~ X -> -X - 1  */
      return build2_loc (loc, MINUS_EXPR, exp_type, negate_expr (arg0),
			 build_int_cst (exp_type, 1));

    case PLUS_EXPR:
    case MINUS_EXPR:
      if (TREE_CODE (arg1) != INTEGER_CST)
	return NULL_TREE;

      /* If flag_wrapv and ARG0_TYPE is signed, then we cannot
	 move a constant to the other side.  */
      if (!TYPE_UNSIGNED (arg0_type)
	  && !TYPE_OVERFLOW_UNDEFINED (arg0_type))
	return NULL_TREE;

      /* If EXP is signed, any overflow in the computation is undefined,
	 so we don't worry about it so long as our computations on
	 the bounds don't overflow.  For unsigned, overflow is defined
	 and this is exactly the right thing.  */
      n_low = range_binop (code == MINUS_EXPR ? PLUS_EXPR : MINUS_EXPR,
			   arg0_type, low, 0, arg1, 0);
      n_high = range_binop (code == MINUS_EXPR ? PLUS_EXPR : MINUS_EXPR,
			    arg0_type, high, 1, arg1, 0);
      if ((n_low != 0 && TREE_OVERFLOW (n_low))
	  || (n_high != 0 && TREE_OVERFLOW (n_high)))
	return NULL_TREE;

      if (TYPE_OVERFLOW_UNDEFINED (arg0_type))
	*strict_overflow_p = true;

      normalize:
	/* Check for an unsigned range which has wrapped around the maximum
	   value thus making n_high < n_low, and normalize it.  */
	if (n_low && n_high && tree_int_cst_lt (n_high, n_low))
	  {
	    low = range_binop (PLUS_EXPR, arg0_type, n_high, 0,
			       build_int_cst (TREE_TYPE (n_high), 1), 0);
	    high = range_binop (MINUS_EXPR, arg0_type, n_low, 0,
				build_int_cst (TREE_TYPE (n_low), 1), 0);

	    /* If the range is of the form +/- [ x+1, x ], we won't
	       be able to normalize it.  But then, it represents the
	       whole range or the empty set, so make it
	       +/- [ -, - ].  */
	    if (tree_int_cst_equal (n_low, low)
		&& tree_int_cst_equal (n_high, high))
	      low = high = 0;
	    else
	      in_p = ! in_p;
	  }
	else
	  low = n_low, high = n_high;

	*p_low = low;
	*p_high = high;
	*p_in_p = in_p;
	return arg0;

    CASE_CONVERT:
    case NON_LVALUE_EXPR:
      if (TYPE_PRECISION (arg0_type) > TYPE_PRECISION (exp_type))
	return NULL_TREE;

      if (! INTEGRAL_TYPE_P (arg0_type)
	  || (low != 0 && ! int_fits_type_p (low, arg0_type))
	  || (high != 0 && ! int_fits_type_p (high, arg0_type)))
	return NULL_TREE;

      n_low = low, n_high = high;

      if (n_low != 0)
	n_low = fold_convert_loc (loc, arg0_type, n_low);

      if (n_high != 0)
	n_high = fold_convert_loc (loc, arg0_type, n_high);

      /* If we're converting arg0 from an unsigned type, to exp,
	 a signed type, we will be doing the comparison as unsigned.
	 The tests above have already verified that LOW and HIGH
	 are both positive.

	 So we have to ensure that we will handle large unsigned
	 values the same way that the current signed bounds treat
	 negative values.  */

      if (!TYPE_UNSIGNED (exp_type) && TYPE_UNSIGNED (arg0_type))
	{
	  tree high_positive;
	  tree equiv_type;
	  /* For fixed-point modes, we need to pass the saturating flag
	     as the 2nd parameter.  */
	  if (ALL_FIXED_POINT_MODE_P (TYPE_MODE (arg0_type)))
	    equiv_type
	      = lang_hooks.types.type_for_mode (TYPE_MODE (arg0_type),
						TYPE_SATURATING (arg0_type));
	  else if (TREE_CODE (arg0_type) == BITINT_TYPE)
	    equiv_type = arg0_type;
	  else
	    equiv_type
	      = lang_hooks.types.type_for_mode (TYPE_MODE (arg0_type), 1);

	  /* A range without an upper bound is, naturally, unbounded.
	     Since convert would have cropped a very large value, use
	     the max value for the destination type.  */
	  high_positive
	    = TYPE_MAX_VALUE (equiv_type) ? TYPE_MAX_VALUE (equiv_type)
	      : TYPE_MAX_VALUE (arg0_type);

	  if (TYPE_PRECISION (exp_type) == TYPE_PRECISION (arg0_type))
	    high_positive = fold_build2_loc (loc, RSHIFT_EXPR, arg0_type,
					     fold_convert_loc (loc, arg0_type,
							       high_positive),
					     build_int_cst (arg0_type, 1));

	  /* If the low bound is specified, "and" the range with the
	     range for which the original unsigned value will be
	     positive.  */
	  if (low != 0)
	    {
	      if (! merge_ranges (&n_in_p, &n_low, &n_high, 1, n_low, n_high,
				  1, fold_convert_loc (loc, arg0_type,
						       integer_zero_node),
				  high_positive))
		return NULL_TREE;

	      in_p = (n_in_p == in_p);
	    }
	  else
	    {
	      /* Otherwise, "or" the range with the range of the input
		 that will be interpreted as negative.  */
	      if (! merge_ranges (&n_in_p, &n_low, &n_high, 0, n_low, n_high,
				  1, fold_convert_loc (loc, arg0_type,
						       integer_zero_node),
				  high_positive))
		return NULL_TREE;

	      in_p = (in_p != n_in_p);
	    }
	}

      /* Otherwise, if we are converting arg0 from signed type, to exp,
	 an unsigned type, we will do the comparison as signed.  If
	 high is non-NULL, we punt above if it doesn't fit in the signed
	 type, so if we get through here, +[-, high] or +[low, high] are
	 equivalent to +[-, n_high] or +[n_low, n_high].  Similarly,
	 +[-, -] or -[-, -] are equivalent too.  But if low is specified and
	 high is not, the +[low, -] range is equivalent to union of
	 +[n_low, -] and +[-, -1] ranges, so +[low, -] is equivalent to
	 -[0, n_low-1] and similarly -[low, -] to +[0, n_low-1], except for
	 low being 0, which should be treated as [-, -].  */
      else if (TYPE_UNSIGNED (exp_type)
	       && !TYPE_UNSIGNED (arg0_type)
	       && low
	       && !high)
	{
	  if (integer_zerop (low))
	    n_low = NULL_TREE;
	  else
	    {
	      n_high = fold_build2_loc (loc, PLUS_EXPR, arg0_type,
					n_low, build_int_cst (arg0_type, -1));
	      n_low = build_zero_cst (arg0_type);
	      in_p = !in_p;
	    }
	}

      *p_low = n_low;
      *p_high = n_high;
      *p_in_p = in_p;
      return arg0;

    default:
      return NULL_TREE;
    }
}

/* Given EXP, a logical expression, set the range it is testing into
   variables denoted by PIN_P, PLOW, and PHIGH.  Return the expression
   actually being tested.  *PLOW and *PHIGH will be made of the same
   type as the returned expression.  If EXP is not a comparison, we
   will most likely not be returning a useful value and range.  Set
   *STRICT_OVERFLOW_P to true if the return value is only valid
   because signed overflow is undefined; otherwise, do not change
   *STRICT_OVERFLOW_P.  */

tree
make_range (tree exp, int *pin_p, tree *plow, tree *phigh,
	    bool *strict_overflow_p)
{
  enum tree_code code;
  tree arg0, arg1 = NULL_TREE;
  tree exp_type, nexp;
  int in_p;
  tree low, high;
  location_t loc = EXPR_LOCATION (exp);

  /* Start with simply saying "EXP != 0" and then look at the code of EXP
     and see if we can refine the range.  Some of the cases below may not
     happen, but it doesn't seem worth worrying about this.  We "continue"
     the outer loop when we've changed something; otherwise we "break"
     the switch, which will "break" the while.  */

  in_p = 0;
  low = high = build_int_cst (TREE_TYPE (exp), 0);

  while (1)
    {
      code = TREE_CODE (exp);
      exp_type = TREE_TYPE (exp);
      arg0 = NULL_TREE;

      if (IS_EXPR_CODE_CLASS (TREE_CODE_CLASS (code)))
	{
	  if (TREE_OPERAND_LENGTH (exp) > 0)
	    arg0 = TREE_OPERAND (exp, 0);
	  if (TREE_CODE_CLASS (code) == tcc_binary
	      || TREE_CODE_CLASS (code) == tcc_comparison
	      || (TREE_CODE_CLASS (code) == tcc_expression
		  && TREE_OPERAND_LENGTH (exp) > 1))
	    arg1 = TREE_OPERAND (exp, 1);
	}
      if (arg0 == NULL_TREE)
	break;

      nexp = make_range_step (loc, code, arg0, arg1, exp_type, &low,
			      &high, &in_p, strict_overflow_p);
      if (nexp == NULL_TREE)
	break;
      exp = nexp;
    }

  /* If EXP is a constant, we can evaluate whether this is true or false.  */
  if (TREE_CODE (exp) == INTEGER_CST)
    {
      in_p = in_p == (integer_onep (range_binop (GE_EXPR, integer_type_node,
						 exp, 0, low, 0))
		      && integer_onep (range_binop (LE_EXPR, integer_type_node,
						    exp, 1, high, 1)));
      low = high = 0;
      exp = 0;
    }

  *pin_p = in_p, *plow = low, *phigh = high;
  return exp;
}

/* Returns TRUE if [LOW, HIGH] range check can be optimized to
   a bitwise check i.e. when
     LOW  == 0xXX...X00...0
     HIGH == 0xXX...X11...1
   Return corresponding mask in MASK and stem in VALUE.  */

static bool
maskable_range_p (const_tree low, const_tree high, tree type, tree *mask,
		  tree *value)
{
  if (TREE_CODE (low) != INTEGER_CST
      || TREE_CODE (high) != INTEGER_CST)
    return false;

  unsigned prec = TYPE_PRECISION (type);
  wide_int lo = wi::to_wide (low, prec);
  wide_int hi = wi::to_wide (high, prec);

  wide_int end_mask = lo ^ hi;
  if ((end_mask & (end_mask + 1)) != 0
      || (lo & end_mask) != 0)
    return false;

  wide_int stem_mask = ~end_mask;
  wide_int stem = lo & stem_mask;
  if (stem != (hi & stem_mask))
    return false;

  *mask = wide_int_to_tree (type, stem_mask);
  *value = wide_int_to_tree (type, stem);

  return true;
}

/* Helper routine for build_range_check and match.pd.  Return the type to
   perform the check or NULL if it shouldn't be optimized.  */

tree
range_check_type (tree etype)
{
  /* First make sure that arithmetics in this type is valid, then make sure
     that it wraps around.  */
  if (TREE_CODE (etype) == ENUMERAL_TYPE || TREE_CODE (etype) == BOOLEAN_TYPE)
    etype = lang_hooks.types.type_for_size (TYPE_PRECISION (etype), 1);

  if (TREE_CODE (etype) == INTEGER_TYPE && !TYPE_UNSIGNED (etype))
    {
      tree utype, minv, maxv;

      /* Check if (unsigned) INT_MAX + 1 == (unsigned) INT_MIN
	 for the type in question, as we rely on this here.  */
      utype = unsigned_type_for (etype);
      maxv = fold_convert (utype, TYPE_MAX_VALUE (etype));
      maxv = range_binop (PLUS_EXPR, NULL_TREE, maxv, 1,
			  build_int_cst (TREE_TYPE (maxv), 1), 1);
      minv = fold_convert (utype, TYPE_MIN_VALUE (etype));

      if (integer_zerop (range_binop (NE_EXPR, integer_type_node,
				      minv, 1, maxv, 1)))
	etype = utype;
      else
	return NULL_TREE;
    }
  else if (POINTER_TYPE_P (etype)
	   || TREE_CODE (etype) == OFFSET_TYPE
	   /* Right now all BITINT_TYPEs satisfy
	      (unsigned) max + 1 == (unsigned) min, so no need to verify
	      that like for INTEGER_TYPEs.  */
	   || TREE_CODE (etype) == BITINT_TYPE)
    etype = unsigned_type_for (etype);
  return etype;
}

/* Given a range, LOW, HIGH, and IN_P, an expression, EXP, and a result
   type, TYPE, return an expression to test if EXP is in (or out of, depending
   on IN_P) the range.  Return 0 if the test couldn't be created.  */

tree
build_range_check (location_t loc, tree type, tree exp, int in_p,
		   tree low, tree high)
{
  tree etype = TREE_TYPE (exp), mask, value;

  /* Disable this optimization for function pointer expressions
     on targets that require function pointer canonicalization.  */
  if (targetm.have_canonicalize_funcptr_for_compare ()
      && POINTER_TYPE_P (etype)
      && FUNC_OR_METHOD_TYPE_P (TREE_TYPE (etype)))
    return NULL_TREE;

  if (! in_p)
    {
      value = build_range_check (loc, type, exp, 1, low, high);
      if (value != 0)
        return invert_truthvalue_loc (loc, value);

      return 0;
    }

  if (low == 0 && high == 0)
    return omit_one_operand_loc (loc, type, build_int_cst (type, 1), exp);

  if (low == 0)
    return fold_build2_loc (loc, LE_EXPR, type, exp,
			    fold_convert_loc (loc, etype, high));

  if (high == 0)
    return fold_build2_loc (loc, GE_EXPR, type, exp,
			    fold_convert_loc (loc, etype, low));

  if (operand_equal_p (low, high, 0))
    return fold_build2_loc (loc, EQ_EXPR, type, exp,
			    fold_convert_loc (loc, etype, low));

  if (TREE_CODE (exp) == BIT_AND_EXPR
      && maskable_range_p (low, high, etype, &mask, &value))
    return fold_build2_loc (loc, EQ_EXPR, type,
			    fold_build2_loc (loc, BIT_AND_EXPR, etype,
					     exp, mask),
			    value);

  if (integer_zerop (low))
    {
      if (! TYPE_UNSIGNED (etype))
	{
	  etype = unsigned_type_for (etype);
	  high = fold_convert_loc (loc, etype, high);
	  exp = fold_convert_loc (loc, etype, exp);
	}
      return build_range_check (loc, type, exp, 1, 0, high);
    }

  /* Optimize (c>=1) && (c<=127) into (signed char)c > 0.  */
  if (integer_onep (low) && TREE_CODE (high) == INTEGER_CST)
    {
      int prec = TYPE_PRECISION (etype);

      if (wi::mask <widest_int> (prec - 1, false) == wi::to_widest (high))
	{
	  if (TYPE_UNSIGNED (etype))
	    {
	      tree signed_etype = signed_type_for (etype);
	      if (TYPE_PRECISION (signed_etype) != TYPE_PRECISION (etype))
		etype
		  = build_nonstandard_integer_type (TYPE_PRECISION (etype), 0);
	      else
		etype = signed_etype;
	      exp = fold_convert_loc (loc, etype, exp);
	    }
	  return fold_build2_loc (loc, GT_EXPR, type, exp,
				  build_int_cst (etype, 0));
	}
    }

  /* Optimize (c>=low) && (c<=high) into (c-low>=0) && (c-low<=high-low).
     This requires wrap-around arithmetics for the type of the expression.  */
  etype = range_check_type (etype);
  if (etype == NULL_TREE)
    return NULL_TREE;

  high = fold_convert_loc (loc, etype, high);
  low = fold_convert_loc (loc, etype, low);
  exp = fold_convert_loc (loc, etype, exp);

  value = const_binop (MINUS_EXPR, high, low);

  if (value != 0 && !TREE_OVERFLOW (value))
    return build_range_check (loc, type,
			      fold_build2_loc (loc, MINUS_EXPR, etype, exp, low),
			      1, build_int_cst (etype, 0), value);

  return 0;
}

/* Return the predecessor of VAL in its type, handling the infinite case.  */

static tree
range_predecessor (tree val)
{
  tree type = TREE_TYPE (val);

  if (INTEGRAL_TYPE_P (type)
      && operand_equal_p (val, TYPE_MIN_VALUE (type), 0))
    return 0;
  else
    return range_binop (MINUS_EXPR, NULL_TREE, val, 0,
			build_int_cst (TREE_TYPE (val), 1), 0);
}

/* Return the successor of VAL in its type, handling the infinite case.  */

static tree
range_successor (tree val)
{
  tree type = TREE_TYPE (val);

  if (INTEGRAL_TYPE_P (type)
      && operand_equal_p (val, TYPE_MAX_VALUE (type), 0))
    return 0;
  else
    return range_binop (PLUS_EXPR, NULL_TREE, val, 0,
			build_int_cst (TREE_TYPE (val), 1), 0);
}

/* Given two ranges, see if we can merge them into one.  Return 1 if we
   can, 0 if we can't.  Set the output range into the specified parameters.  */

bool
merge_ranges (int *pin_p, tree *plow, tree *phigh, int in0_p, tree low0,
	      tree high0, int in1_p, tree low1, tree high1)
{
  bool no_overlap;
  int subset;
  int temp;
  tree tem;
  int in_p;
  tree low, high;
  int lowequal = ((low0 == 0 && low1 == 0)
		  || integer_onep (range_binop (EQ_EXPR, integer_type_node,
						low0, 0, low1, 0)));
  int highequal = ((high0 == 0 && high1 == 0)
		   || integer_onep (range_binop (EQ_EXPR, integer_type_node,
						 high0, 1, high1, 1)));

  /* Make range 0 be the range that starts first, or ends last if they
     start at the same value.  Swap them if it isn't.  */
  if (integer_onep (range_binop (GT_EXPR, integer_type_node,
				 low0, 0, low1, 0))
      || (lowequal
	  && integer_onep (range_binop (GT_EXPR, integer_type_node,
					high1, 1, high0, 1))))
    {
      temp = in0_p, in0_p = in1_p, in1_p = temp;
      tem = low0, low0 = low1, low1 = tem;
      tem = high0, high0 = high1, high1 = tem;
    }

  /* If the second range is != high1 where high1 is the type maximum of
     the type, try first merging with < high1 range.  */
  if (low1
      && high1
      && TREE_CODE (low1) == INTEGER_CST
      && (TREE_CODE (TREE_TYPE (low1)) == INTEGER_TYPE
	  || (TREE_CODE (TREE_TYPE (low1)) == ENUMERAL_TYPE
	      && known_eq (TYPE_PRECISION (TREE_TYPE (low1)),
			   GET_MODE_BITSIZE (TYPE_MODE (TREE_TYPE (low1))))))
      && operand_equal_p (low1, high1, 0))
    {
      if (tree_int_cst_equal (low1, TYPE_MAX_VALUE (TREE_TYPE (low1)))
	  && merge_ranges (pin_p, plow, phigh, in0_p, low0, high0,
			   !in1_p, NULL_TREE, range_predecessor (low1)))
	return true;
      /* Similarly for the second range != low1 where low1 is the type minimum
	 of the type, try first merging with > low1 range.  */
      if (tree_int_cst_equal (low1, TYPE_MIN_VALUE (TREE_TYPE (low1)))
	  && merge_ranges (pin_p, plow, phigh, in0_p, low0, high0,
			   !in1_p, range_successor (low1), NULL_TREE))
	return true;
    }

  /* Now flag two cases, whether the ranges are disjoint or whether the
     second range is totally subsumed in the first.  Note that the tests
     below are simplified by the ones above.  */
  no_overlap = integer_onep (range_binop (LT_EXPR, integer_type_node,
					  high0, 1, low1, 0));
  subset = integer_onep (range_binop (LE_EXPR, integer_type_node,
				      high1, 1, high0, 1));

  /* We now have four cases, depending on whether we are including or
     excluding the two ranges.  */
  if (in0_p && in1_p)
    {
      /* If they don't overlap, the result is false.  If the second range
	 is a subset it is the result.  Otherwise, the range is from the start
	 of the second to the end of the first.  */
      if (no_overlap)
	in_p = 0, low = high = 0;
      else if (subset)
	in_p = 1, low = low1, high = high1;
      else
	in_p = 1, low = low1, high = high0;
    }

  else if (in0_p && ! in1_p)
    {
      /* If they don't overlap, the result is the first range.  If they are
	 equal, the result is false.  If the second range is a subset of the
	 first, and the ranges begin at the same place, we go from just after
	 the end of the second range to the end of the first.  If the second
	 range is not a subset of the first, or if it is a subset and both
	 ranges end at the same place, the range starts at the start of the
	 first range and ends just before the second range.
	 Otherwise, we can't describe this as a single range.  */
      if (no_overlap)
	in_p = 1, low = low0, high = high0;
      else if (lowequal && highequal)
	in_p = 0, low = high = 0;
      else if (subset && lowequal)
	{
	  low = range_successor (high1);
	  high = high0;
	  in_p = 1;
	  if (low == 0)
	    {
	      /* We are in the weird situation where high0 > high1 but
		 high1 has no successor.  Punt.  */
	      return 0;
	    }
	}
      else if (! subset || highequal)
	{
	  low = low0;
	  high = range_predecessor (low1);
	  in_p = 1;
	  if (high == 0)
	    {
	      /* low0 < low1 but low1 has no predecessor.  Punt.  */
	      return 0;
	    }
	}
      else
	return 0;
    }

  else if (! in0_p && in1_p)
    {
      /* If they don't overlap, the result is the second range.  If the second
	 is a subset of the first, the result is false.  Otherwise,
	 the range starts just after the first range and ends at the
	 end of the second.  */
      if (no_overlap)
	in_p = 1, low = low1, high = high1;
      else if (subset || highequal)
	in_p = 0, low = high = 0;
      else
	{
	  low = range_successor (high0);
	  high = high1;
	  in_p = 1;
	  if (low == 0)
	    {
	      /* high1 > high0 but high0 has no successor.  Punt.  */
	      return 0;
	    }
	}
    }

  else
    {
      /* The case where we are excluding both ranges.  Here the complex case
	 is if they don't overlap.  In that case, the only time we have a
	 range is if they are adjacent.  If the second is a subset of the
	 first, the result is the first.  Otherwise, the range to exclude
	 starts at the beginning of the first range and ends at the end of the
	 second.  */
      if (no_overlap)
	{
	  if (integer_onep (range_binop (EQ_EXPR, integer_type_node,
					 range_successor (high0),
					 1, low1, 0)))
	    in_p = 0, low = low0, high = high1;
	  else
	    {
	      /* Canonicalize - [min, x] into - [-, x].  */
	      if (low0 && TREE_CODE (low0) == INTEGER_CST)
		switch (TREE_CODE (TREE_TYPE (low0)))
		  {
		  case ENUMERAL_TYPE:
		    if (maybe_ne (TYPE_PRECISION (TREE_TYPE (low0)),
				  GET_MODE_BITSIZE
				    (TYPE_MODE (TREE_TYPE (low0)))))
		      break;
		    /* FALLTHROUGH */
		  case INTEGER_TYPE:
		    if (tree_int_cst_equal (low0,
					    TYPE_MIN_VALUE (TREE_TYPE (low0))))
		      low0 = 0;
		    break;
		  case POINTER_TYPE:
		    if (TYPE_UNSIGNED (TREE_TYPE (low0))
			&& integer_zerop (low0))
		      low0 = 0;
		    break;
		  default:
		    break;
		  }

	      /* Canonicalize - [x, max] into - [x, -].  */
	      if (high1 && TREE_CODE (high1) == INTEGER_CST)
		switch (TREE_CODE (TREE_TYPE (high1)))
		  {
		  case ENUMERAL_TYPE:
		    if (maybe_ne (TYPE_PRECISION (TREE_TYPE (high1)),
				  GET_MODE_BITSIZE
				    (TYPE_MODE (TREE_TYPE (high1)))))
		      break;
		    /* FALLTHROUGH */
		  case INTEGER_TYPE:
		    if (tree_int_cst_equal (high1,
					    TYPE_MAX_VALUE (TREE_TYPE (high1))))
		      high1 = 0;
		    break;
		  case POINTER_TYPE:
		    if (TYPE_UNSIGNED (TREE_TYPE (high1))
			&& integer_zerop (range_binop (PLUS_EXPR, NULL_TREE,
						       high1, 1,
						       build_int_cst (TREE_TYPE (high1), 1),
						       1)))
		      high1 = 0;
		    break;
		  default:
		    break;
		  }

	      /* The ranges might be also adjacent between the maximum and
	         minimum values of the given type.  For
	         - [{min,-}, x] and - [y, {max,-}] ranges where x + 1 < y
	         return + [x + 1, y - 1].  */
	      if (low0 == 0 && high1 == 0)
	        {
		  low = range_successor (high0);
		  high = range_predecessor (low1);
		  if (low == 0 || high == 0)
		    return 0;

		  in_p = 1;
		}
	      else
		return 0;
	    }
	}
      else if (subset)
	in_p = 0, low = low0, high = high0;
      else
	in_p = 0, low = low0, high = high1;
    }

  *pin_p = in_p, *plow = low, *phigh = high;
  return 1;
}


/* Subroutine of fold, looking inside expressions of the form
   A op B ? A : C, where (ARG00, COMP_CODE, ARG01), ARG1 and ARG2
   are the three operands of the COND_EXPR.  This function is
   being used also to optimize A op B ? C : A, by reversing the
   comparison first.

   Return a folded expression whose code is not a COND_EXPR
   anymore, or NULL_TREE if no folding opportunity is found.  */

static tree
fold_cond_expr_with_comparison (location_t loc, tree type,
				enum tree_code comp_code,
				tree arg00, tree arg01, tree arg1, tree arg2)
{
  tree arg1_type = TREE_TYPE (arg1);
  tree tem;

  STRIP_NOPS (arg1);
  STRIP_NOPS (arg2);

  /* If we have A op 0 ? A : -A, consider applying the following
     transformations:

     A == 0? A : -A    same as -A
     A != 0? A : -A    same as A
     A >= 0? A : -A    same as abs (A)
     A > 0?  A : -A    same as abs (A)
     A <= 0? A : -A    same as -abs (A)
     A < 0?  A : -A    same as -abs (A)

     None of these transformations work for modes with signed
     zeros.  If A is +/-0, the first two transformations will
     change the sign of the result (from +0 to -0, or vice
     versa).  The last four will fix the sign of the result,
     even though the original expressions could be positive or
     negative, depending on the sign of A.

     Note that all these transformations are correct if A is
     NaN, since the two alternatives (A and -A) are also NaNs.  */
  if (!HONOR_SIGNED_ZEROS (type)
      && (FLOAT_TYPE_P (TREE_TYPE (arg01))
	  ? real_zerop (arg01)
	  : integer_zerop (arg01))
      && ((TREE_CODE (arg2) == NEGATE_EXPR
	   && operand_equal_p (TREE_OPERAND (arg2, 0), arg1, 0))
	     /* In the case that A is of the form X-Y, '-A' (arg2) may
	        have already been folded to Y-X, check for that. */
	  || (TREE_CODE (arg1) == MINUS_EXPR
	      && TREE_CODE (arg2) == MINUS_EXPR
	      && operand_equal_p (TREE_OPERAND (arg1, 0),
				  TREE_OPERAND (arg2, 1), 0)
	      && operand_equal_p (TREE_OPERAND (arg1, 1),
				  TREE_OPERAND (arg2, 0), 0))))
    switch (comp_code)
      {
      case EQ_EXPR:
      case UNEQ_EXPR:
	tem = fold_convert_loc (loc, arg1_type, arg1);
	return fold_convert_loc (loc, type, negate_expr (tem));
      case NE_EXPR:
      case LTGT_EXPR:
	return fold_convert_loc (loc, type, arg1);
      case UNGE_EXPR:
      case UNGT_EXPR:
	if (flag_trapping_math)
	  break;
	/* Fall through.  */
      case GE_EXPR:
      case GT_EXPR:
	if (TYPE_UNSIGNED (TREE_TYPE (arg1)))
	  break;
	tem = fold_build1_loc (loc, ABS_EXPR, TREE_TYPE (arg1), arg1);
	return fold_convert_loc (loc, type, tem);
      case UNLE_EXPR:
      case UNLT_EXPR:
	if (flag_trapping_math)
	  break;
	/* FALLTHRU */
      case LE_EXPR:
      case LT_EXPR:
	if (TYPE_UNSIGNED (TREE_TYPE (arg1)))
	  break;
	if (ANY_INTEGRAL_TYPE_P (TREE_TYPE (arg1))
	    && !TYPE_OVERFLOW_WRAPS (TREE_TYPE (arg1)))
	  {
	    /* A <= 0 ? A : -A for A INT_MIN is valid, but -abs(INT_MIN)
	       is not, invokes UB both in abs and in the negation of it.
	       So, use ABSU_EXPR instead.  */
	    tree utype = unsigned_type_for (TREE_TYPE (arg1));
	    tem = fold_build1_loc (loc, ABSU_EXPR, utype, arg1);
	    tem = negate_expr (tem);
	    return fold_convert_loc (loc, type, tem);
	  }
	else
	  {
	    tem = fold_build1_loc (loc, ABS_EXPR, TREE_TYPE (arg1), arg1);
	    return negate_expr (fold_convert_loc (loc, type, tem));
	  }
      default:
	gcc_assert (TREE_CODE_CLASS (comp_code) == tcc_comparison);
	break;
      }

  /* A != 0 ? A : 0 is simply A, unless A is -0.  Likewise
     A == 0 ? A : 0 is always 0 unless A is -0.  Note that
     both transformations are correct when A is NaN: A != 0
     is then true, and A == 0 is false.  */

  if (!HONOR_SIGNED_ZEROS (type)
      && integer_zerop (arg01) && integer_zerop (arg2))
    {
      if (comp_code == NE_EXPR)
	return fold_convert_loc (loc, type, arg1);
      else if (comp_code == EQ_EXPR)
	return build_zero_cst (type);
    }

  /* Try some transformations of A op B ? A : B.

     A == B? A : B    same as B
     A != B? A : B    same as A
     A >= B? A : B    same as max (A, B)
     A > B?  A : B    same as max (B, A)
     A <= B? A : B    same as min (A, B)
     A < B?  A : B    same as min (B, A)

     As above, these transformations don't work in the presence
     of signed zeros.  For example, if A and B are zeros of
     opposite sign, the first two transformations will change
     the sign of the result.  In the last four, the original
     expressions give different results for (A=+0, B=-0) and
     (A=-0, B=+0), but the transformed expressions do not.

     The first two transformations are correct if either A or B
     is a NaN.  In the first transformation, the condition will
     be false, and B will indeed be chosen.  In the case of the
     second transformation, the condition A != B will be true,
     and A will be chosen.

     The conversions to max() and min() are not correct if B is
     a number and A is not.  The conditions in the original
     expressions will be false, so all four give B.  The min()
     and max() versions would give a NaN instead.  */
  if (!HONOR_SIGNED_ZEROS (type)
      && operand_equal_for_comparison_p (arg01, arg2)
      /* Avoid these transformations if the COND_EXPR may be used
	 as an lvalue in the C++ front-end.  PR c++/19199.  */
      && (in_gimple_form
	  || VECTOR_TYPE_P (type)
	  || (! lang_GNU_CXX ()
	      && strcmp (lang_hooks.name, "GNU Objective-C++") != 0)
	  || ! maybe_lvalue_p (arg1)
	  || ! maybe_lvalue_p (arg2)))
    {
      tree comp_op0 = arg00;
      tree comp_op1 = arg01;
      tree comp_type = TREE_TYPE (comp_op0);

      switch (comp_code)
	{
	case EQ_EXPR:
	  return fold_convert_loc (loc, type, arg2);
	case NE_EXPR:
	  return fold_convert_loc (loc, type, arg1);
	case LE_EXPR:
	case LT_EXPR:
	case UNLE_EXPR:
	case UNLT_EXPR:
	  /* In C++ a ?: expression can be an lvalue, so put the
	     operand which will be used if they are equal first
	     so that we can convert this back to the
	     corresponding COND_EXPR.  */
	  if (!HONOR_NANS (arg1))
	    {
	      comp_op0 = fold_convert_loc (loc, comp_type, comp_op0);
	      comp_op1 = fold_convert_loc (loc, comp_type, comp_op1);
	      tem = (comp_code == LE_EXPR || comp_code == UNLE_EXPR)
		    ? fold_build2_loc (loc, MIN_EXPR, comp_type, comp_op0, comp_op1)
		    : fold_build2_loc (loc, MIN_EXPR, comp_type,
				   comp_op1, comp_op0);
	      return fold_convert_loc (loc, type, tem);
	    }
	  break;
	case GE_EXPR:
	case GT_EXPR:
	case UNGE_EXPR:
	case UNGT_EXPR:
	  if (!HONOR_NANS (arg1))
	    {
	      comp_op0 = fold_convert_loc (loc, comp_type, comp_op0);
	      comp_op1 = fold_convert_loc (loc, comp_type, comp_op1);
	      tem = (comp_code == GE_EXPR || comp_code == UNGE_EXPR)
		    ? fold_build2_loc (loc, MAX_EXPR, comp_type, comp_op0, comp_op1)
		    : fold_build2_loc (loc, MAX_EXPR, comp_type,
				   comp_op1, comp_op0);
	      return fold_convert_loc (loc, type, tem);
	    }
	  break;
	case UNEQ_EXPR:
	  if (!HONOR_NANS (arg1))
	    return fold_convert_loc (loc, type, arg2);
	  break;
	case LTGT_EXPR:
	  if (!HONOR_NANS (arg1))
	    return fold_convert_loc (loc, type, arg1);
	  break;
	default:
	  gcc_assert (TREE_CODE_CLASS (comp_code) == tcc_comparison);
	  break;
	}
    }

  return NULL_TREE;
}



#ifndef LOGICAL_OP_NON_SHORT_CIRCUIT
#define LOGICAL_OP_NON_SHORT_CIRCUIT \
  (BRANCH_COST (optimize_function_for_speed_p (cfun), \
		false) >= 2)
#endif

/* EXP is some logical combination of boolean tests.  See if we can
   merge it into some range test.  Return the new tree if so.  */

static tree
fold_range_test (location_t loc, enum tree_code code, tree type,
		 tree op0, tree op1)
{
  int or_op = (code == TRUTH_ORIF_EXPR
	       || code == TRUTH_OR_EXPR);
  int in0_p, in1_p, in_p;
  tree low0, low1, low, high0, high1, high;
  bool strict_overflow_p = false;
  tree tem, lhs, rhs;
  const char * const warnmsg = G_("assuming signed overflow does not occur "
				  "when simplifying range test");

  if (!INTEGRAL_TYPE_P (type))
    return 0;

  lhs = make_range (op0, &in0_p, &low0, &high0, &strict_overflow_p);
  /* If op0 is known true or false and this is a short-circuiting
     operation we must not merge with op1 since that makes side-effects
     unconditional.  So special-case this.  */
  if (!lhs
      && ((code == TRUTH_ORIF_EXPR && in0_p)
	  || (code == TRUTH_ANDIF_EXPR && !in0_p)))
    return op0;
  rhs = make_range (op1, &in1_p, &low1, &high1, &strict_overflow_p);

  /* If this is an OR operation, invert both sides; we will invert
     again at the end.  */
  if (or_op)
    in0_p = ! in0_p, in1_p = ! in1_p;

  /* If both expressions are the same, if we can merge the ranges, and we
     can build the range test, return it or it inverted.  If one of the
     ranges is always true or always false, consider it to be the same
     expression as the other.  */
  if ((lhs == 0 || rhs == 0 || operand_equal_p (lhs, rhs, 0))
      && merge_ranges (&in_p, &low, &high, in0_p, low0, high0,
		       in1_p, low1, high1)
      && (tem = (build_range_check (loc, type,
				    lhs != 0 ? lhs
				    : rhs != 0 ? rhs : integer_zero_node,
				    in_p, low, high))) != 0)
    {
      if (strict_overflow_p)
	fold_overflow_warning (warnmsg, WARN_STRICT_OVERFLOW_COMPARISON);
      return or_op ? invert_truthvalue_loc (loc, tem) : tem;
    }

  /* On machines where the branch cost is expensive, if this is a
     short-circuited branch and the underlying object on both sides
     is the same, make a non-short-circuit operation.  */
  bool logical_op_non_short_circuit = LOGICAL_OP_NON_SHORT_CIRCUIT;
  if (param_logical_op_non_short_circuit != -1)
    logical_op_non_short_circuit
      = param_logical_op_non_short_circuit;
  if (logical_op_non_short_circuit
      && !sanitize_coverage_p ()
      && lhs != 0 && rhs != 0
      && (code == TRUTH_ANDIF_EXPR || code == TRUTH_ORIF_EXPR)
      && operand_equal_p (lhs, rhs, 0))
    {
      /* If simple enough, just rewrite.  Otherwise, make a SAVE_EXPR
	 unless we are at top level or LHS contains a PLACEHOLDER_EXPR, in
	 which cases we can't do this.  */
      if (simple_operand_p (lhs))
	return build2_loc (loc, code == TRUTH_ANDIF_EXPR
			   ? TRUTH_AND_EXPR : TRUTH_OR_EXPR,
			   type, op0, op1);

      else if (!lang_hooks.decls.global_bindings_p ()
	       && !CONTAINS_PLACEHOLDER_P (lhs))
	{
	  tree common = save_expr (lhs);

	  if ((lhs = build_range_check (loc, type, common,
					or_op ? ! in0_p : in0_p,
					low0, high0)) != 0
	      && (rhs = build_range_check (loc, type, common,
					   or_op ? ! in1_p : in1_p,
					   low1, high1)) != 0)
	    {
	      if (strict_overflow_p)
		fold_overflow_warning (warnmsg,
				       WARN_STRICT_OVERFLOW_COMPARISON);
	      return build2_loc (loc, code == TRUTH_ANDIF_EXPR
				 ? TRUTH_AND_EXPR : TRUTH_OR_EXPR,
				 type, lhs, rhs);
	    }
	}
    }

  return 0;
}

/* Subroutine for fold_truth_andor_1: C is an INTEGER_CST interpreted as a P
   bit value.  Arrange things so the extra bits will be set to zero if and
   only if C is signed-extended to its full width.  If MASK is nonzero,
   it is an INTEGER_CST that should be AND'ed with the extra bits.  */

static tree
unextend (tree c, int p, int unsignedp, tree mask)
{
  tree type = TREE_TYPE (c);
  int modesize = GET_MODE_BITSIZE (SCALAR_INT_TYPE_MODE (type));
  tree temp;

  if (p == modesize || unsignedp)
    return c;

  /* We work by getting just the sign bit into the low-order bit, then
     into the high-order bit, then sign-extend.  We then XOR that value
     with C.  */
  temp = build_int_cst (TREE_TYPE (c),
			wi::extract_uhwi (wi::to_wide (c), p - 1, 1));

  /* We must use a signed type in order to get an arithmetic right shift.
     However, we must also avoid introducing accidental overflows, so that
     a subsequent call to integer_zerop will work.  Hence we must
     do the type conversion here.  At this point, the constant is either
     zero or one, and the conversion to a signed type can never overflow.
     We could get an overflow if this conversion is done anywhere else.  */
  if (TYPE_UNSIGNED (type))
    temp = fold_convert (signed_type_for (type), temp);

  temp = const_binop (LSHIFT_EXPR, temp, size_int (modesize - 1));
  temp = const_binop (RSHIFT_EXPR, temp, size_int (modesize - p - 1));
  if (mask != 0)
    temp = const_binop (BIT_AND_EXPR, temp,
			fold_convert (TREE_TYPE (c), mask));
  /* If necessary, convert the type back to match the type of C.  */
  if (TYPE_UNSIGNED (type))
    temp = fold_convert (type, temp);

  return fold_convert (type, const_binop (BIT_XOR_EXPR, c, temp));
}

/* For an expression that has the form
     (A && B) || ~B
   or
     (A || B) && ~B,
   we can drop one of the inner expressions and simplify to
     A || ~B
   or
     A && ~B
   LOC is the location of the resulting expression.  OP is the inner 
   logical operation; the left-hand side in the examples above, while CMPOP
   is the right-hand side.  RHS_ONLY is used to prevent us from accidentally
   removing a condition that guards another, as in
     (A != NULL && A->...) || A == NULL
   which we must not transform.  If RHS_ONLY is true, only eliminate the
   right-most operand of the inner logical operation.  */

static tree
merge_truthop_with_opposite_arm (location_t loc, tree op, tree cmpop,
				 bool rhs_only)
{
  enum tree_code code = TREE_CODE (cmpop);
  enum tree_code truthop_code = TREE_CODE (op);
  tree lhs = TREE_OPERAND (op, 0);
  tree rhs = TREE_OPERAND (op, 1);
  tree orig_lhs = lhs, orig_rhs = rhs;
  enum tree_code rhs_code = TREE_CODE (rhs);
  enum tree_code lhs_code = TREE_CODE (lhs);
  enum tree_code inv_code;

  if (TREE_SIDE_EFFECTS (op) || TREE_SIDE_EFFECTS (cmpop))
    return NULL_TREE;

  if (TREE_CODE_CLASS (code) != tcc_comparison)
    return NULL_TREE;

  tree type = TREE_TYPE (TREE_OPERAND (cmpop, 0));

  if (rhs_code == truthop_code)
    {
      tree newrhs = merge_truthop_with_opposite_arm (loc, rhs, cmpop, rhs_only);
      if (newrhs != NULL_TREE)
	{
	  rhs = newrhs;
	  rhs_code = TREE_CODE (rhs);
	}
    }
  if (lhs_code == truthop_code && !rhs_only)
    {
      tree newlhs = merge_truthop_with_opposite_arm (loc, lhs, cmpop, false);
      if (newlhs != NULL_TREE)
	{
	  lhs = newlhs;
	  lhs_code = TREE_CODE (lhs);
	}
    }

  inv_code = invert_tree_comparison (code, HONOR_NANS (type));
  if (inv_code == rhs_code
      && operand_equal_p (TREE_OPERAND (rhs, 0), TREE_OPERAND (cmpop, 0), 0)
      && operand_equal_p (TREE_OPERAND (rhs, 1), TREE_OPERAND (cmpop, 1), 0))
    return lhs;
  if (!rhs_only && inv_code == lhs_code
      && operand_equal_p (TREE_OPERAND (lhs, 0), TREE_OPERAND (cmpop, 0), 0)
      && operand_equal_p (TREE_OPERAND (lhs, 1), TREE_OPERAND (cmpop, 1), 0))
    return rhs;
  if (rhs != orig_rhs || lhs != orig_lhs)
    return fold_build2_loc (loc, truthop_code, TREE_TYPE (cmpop),
			    lhs, rhs);
  return NULL_TREE;
}

/* Find ways of folding logical expressions of LHS and RHS:
   Try to merge two comparisons to the same innermost item.
   Look for range tests like "ch >= '0' && ch <= '9'".
   Look for combinations of simple terms on machines with expensive branches
   and evaluate the RHS unconditionally.

   For example, if we have p->a == 2 && p->b == 4 and we can make an
   object large enough to span both A and B, we can do this with a comparison
   against the object ANDed with the a mask.

   If we have p->a == q->a && p->b == q->b, we may be able to use bit masking
   operations to do this with one comparison.

   We check for both normal comparisons and the BIT_AND_EXPRs made this by
   function and the one above.

   CODE is the logical operation being done.  It can be TRUTH_ANDIF_EXPR,
   TRUTH_AND_EXPR, TRUTH_ORIF_EXPR, or TRUTH_OR_EXPR.

   TRUTH_TYPE is the type of the logical operand and LHS and RHS are its
   two operands.

   We return the simplified tree or 0 if no optimization is possible.  */

static tree
fold_truth_andor_1 (location_t loc, enum tree_code code, tree truth_type,
		    tree lhs, tree rhs)
{
  /* If this is the "or" of two comparisons, we can do something if
     the comparisons are NE_EXPR.  If this is the "and", we can do something
     if the comparisons are EQ_EXPR.  I.e.,
	(a->b == 2 && a->c == 4) can become (a->new == NEW).

     WANTED_CODE is this operation code.  For single bit fields, we can
     convert EQ_EXPR to NE_EXPR so we need not reject the "wrong"
     comparison for one-bit fields.  */

  enum tree_code wanted_code;
  enum tree_code lcode, rcode;
  tree ll_arg, lr_arg, rl_arg, rr_arg;
  tree ll_inner, lr_inner, rl_inner, rr_inner;
  HOST_WIDE_INT ll_bitsize, ll_bitpos, lr_bitsize, lr_bitpos;
  HOST_WIDE_INT rl_bitsize, rl_bitpos, rr_bitsize, rr_bitpos;
  HOST_WIDE_INT xll_bitpos, xlr_bitpos, xrl_bitpos, xrr_bitpos;
  HOST_WIDE_INT lnbitsize, lnbitpos, rnbitsize, rnbitpos;
  int ll_unsignedp, lr_unsignedp, rl_unsignedp, rr_unsignedp;
  int ll_reversep, lr_reversep, rl_reversep, rr_reversep;
  machine_mode ll_mode, lr_mode, rl_mode, rr_mode;
  scalar_int_mode lnmode, rnmode;
  tree ll_mask, lr_mask, rl_mask, rr_mask;
  tree ll_and_mask, lr_and_mask, rl_and_mask, rr_and_mask;
  tree l_const, r_const;
  tree lntype, rntype, result;
  HOST_WIDE_INT first_bit, end_bit;
  int volatilep;

  /* Start by getting the comparison codes.  Fail if anything is volatile.
     If one operand is a BIT_AND_EXPR with the constant one, treat it as if
     it were surrounded with a NE_EXPR.  */

  if (TREE_SIDE_EFFECTS (lhs) || TREE_SIDE_EFFECTS (rhs))
    return 0;

  lcode = TREE_CODE (lhs);
  rcode = TREE_CODE (rhs);

  if (lcode == BIT_AND_EXPR && integer_onep (TREE_OPERAND (lhs, 1)))
    {
      lhs = build2 (NE_EXPR, truth_type, lhs,
		    build_int_cst (TREE_TYPE (lhs), 0));
      lcode = NE_EXPR;
    }

  if (rcode == BIT_AND_EXPR && integer_onep (TREE_OPERAND (rhs, 1)))
    {
      rhs = build2 (NE_EXPR, truth_type, rhs,
		    build_int_cst (TREE_TYPE (rhs), 0));
      rcode = NE_EXPR;
    }

  if (TREE_CODE_CLASS (lcode) != tcc_comparison
      || TREE_CODE_CLASS (rcode) != tcc_comparison)
    return 0;

  ll_arg = TREE_OPERAND (lhs, 0);
  lr_arg = TREE_OPERAND (lhs, 1);
  rl_arg = TREE_OPERAND (rhs, 0);
  rr_arg = TREE_OPERAND (rhs, 1);

  /* Simplify (x<y) && (x==y) into (x<=y) and related optimizations.  */
  if (simple_operand_p (ll_arg)
      && simple_operand_p (lr_arg))
    {
      if (operand_equal_p (ll_arg, rl_arg, 0)
          && operand_equal_p (lr_arg, rr_arg, 0))
	{
          result = combine_comparisons (loc, code, lcode, rcode,
					truth_type, ll_arg, lr_arg);
	  if (result)
	    return result;
	}
      else if (operand_equal_p (ll_arg, rr_arg, 0)
               && operand_equal_p (lr_arg, rl_arg, 0))
	{
          result = combine_comparisons (loc, code, lcode,
					swap_tree_comparison (rcode),
					truth_type, ll_arg, lr_arg);
	  if (result)
	    return result;
	}
    }

  code = ((code == TRUTH_AND_EXPR || code == TRUTH_ANDIF_EXPR)
	  ? TRUTH_AND_EXPR : TRUTH_OR_EXPR);

  /* If the RHS can be evaluated unconditionally and its operands are
     simple, it wins to evaluate the RHS unconditionally on machines
     with expensive branches.  In this case, this isn't a comparison
     that can be merged.  */

  if (BRANCH_COST (optimize_function_for_speed_p (cfun),
		   false) >= 2
      && ! FLOAT_TYPE_P (TREE_TYPE (rl_arg))
      && simple_operand_p (rl_arg)
      && simple_operand_p (rr_arg))
    {
      /* Convert (a != 0) || (b != 0) into (a | b) != 0.  */
      if (code == TRUTH_OR_EXPR
	  && lcode == NE_EXPR && integer_zerop (lr_arg)
	  && rcode == NE_EXPR && integer_zerop (rr_arg)
	  && TREE_TYPE (ll_arg) == TREE_TYPE (rl_arg)
	  && INTEGRAL_TYPE_P (TREE_TYPE (ll_arg)))
	return build2_loc (loc, NE_EXPR, truth_type,
			   build2 (BIT_IOR_EXPR, TREE_TYPE (ll_arg),
				   ll_arg, rl_arg),
			   build_int_cst (TREE_TYPE (ll_arg), 0));

      /* Convert (a == 0) && (b == 0) into (a | b) == 0.  */
      if (code == TRUTH_AND_EXPR
	  && lcode == EQ_EXPR && integer_zerop (lr_arg)
	  && rcode == EQ_EXPR && integer_zerop (rr_arg)
	  && TREE_TYPE (ll_arg) == TREE_TYPE (rl_arg)
	  && INTEGRAL_TYPE_P (TREE_TYPE (ll_arg)))
	return build2_loc (loc, EQ_EXPR, truth_type,
			   build2 (BIT_IOR_EXPR, TREE_TYPE (ll_arg),
				   ll_arg, rl_arg),
			   build_int_cst (TREE_TYPE (ll_arg), 0));
    }

  /* See if the comparisons can be merged.  Then get all the parameters for
     each side.  */

  if ((lcode != EQ_EXPR && lcode != NE_EXPR)
      || (rcode != EQ_EXPR && rcode != NE_EXPR))
    return 0;

  ll_reversep = lr_reversep = rl_reversep = rr_reversep = 0;
  volatilep = 0;
  ll_inner = decode_field_reference (loc, &ll_arg,
				     &ll_bitsize, &ll_bitpos, &ll_mode,
				     &ll_unsignedp, &ll_reversep, &volatilep,
				     &ll_mask, &ll_and_mask);
  lr_inner = decode_field_reference (loc, &lr_arg,
				     &lr_bitsize, &lr_bitpos, &lr_mode,
				     &lr_unsignedp, &lr_reversep, &volatilep,
				     &lr_mask, &lr_and_mask);
  rl_inner = decode_field_reference (loc, &rl_arg,
				     &rl_bitsize, &rl_bitpos, &rl_mode,
				     &rl_unsignedp, &rl_reversep, &volatilep,
				     &rl_mask, &rl_and_mask);
  rr_inner = decode_field_reference (loc, &rr_arg,
				     &rr_bitsize, &rr_bitpos, &rr_mode,
				     &rr_unsignedp, &rr_reversep, &volatilep,
				     &rr_mask, &rr_and_mask);

  /* It must be true that the inner operation on the lhs of each
     comparison must be the same if we are to be able to do anything.
     Then see if we have constants.  If not, the same must be true for
     the rhs's.  */
  if (volatilep
      || ll_reversep != rl_reversep
      || ll_inner == 0 || rl_inner == 0
      || ! operand_equal_p (ll_inner, rl_inner, 0))
    return 0;

  if (TREE_CODE (lr_arg) == INTEGER_CST
      && TREE_CODE (rr_arg) == INTEGER_CST)
    {
      l_const = lr_arg, r_const = rr_arg;
      lr_reversep = ll_reversep;
    }
  else if (lr_reversep != rr_reversep
	   || lr_inner == 0 || rr_inner == 0
	   || ! operand_equal_p (lr_inner, rr_inner, 0))
    return 0;
  else
    l_const = r_const = 0;

  /* If either comparison code is not correct for our logical operation,
     fail.  However, we can convert a one-bit comparison against zero into
     the opposite comparison against that bit being set in the field.  */

  wanted_code = (code == TRUTH_AND_EXPR ? EQ_EXPR : NE_EXPR);
  if (lcode != wanted_code)
    {
      if (l_const && integer_zerop (l_const) && integer_pow2p (ll_mask))
	{
	  /* Make the left operand unsigned, since we are only interested
	     in the value of one bit.  Otherwise we are doing the wrong
	     thing below.  */
	  ll_unsignedp = 1;
	  l_const = ll_mask;
	}
      else
	return 0;
    }

  /* This is analogous to the code for l_const above.  */
  if (rcode != wanted_code)
    {
      if (r_const && integer_zerop (r_const) && integer_pow2p (rl_mask))
	{
	  rl_unsignedp = 1;
	  r_const = rl_mask;
	}
      else
	return 0;
    }

  /* See if we can find a mode that contains both fields being compared on
     the left.  If we can't, fail.  Otherwise, update all constants and masks
     to be relative to a field of that size.  */
  first_bit = MIN (ll_bitpos, rl_bitpos);
  end_bit = MAX (ll_bitpos + ll_bitsize, rl_bitpos + rl_bitsize);
  if (!get_best_mode (end_bit - first_bit, first_bit, 0, 0,
		      TYPE_ALIGN (TREE_TYPE (ll_inner)), BITS_PER_WORD,
		      volatilep, &lnmode))
    return 0;

  lnbitsize = GET_MODE_BITSIZE (lnmode);
  lnbitpos = first_bit & ~ (lnbitsize - 1);
  lntype = lang_hooks.types.type_for_size (lnbitsize, 1);
  xll_bitpos = ll_bitpos - lnbitpos, xrl_bitpos = rl_bitpos - lnbitpos;

  if (ll_reversep ? !BYTES_BIG_ENDIAN : BYTES_BIG_ENDIAN)
    {
      xll_bitpos = lnbitsize - xll_bitpos - ll_bitsize;
      xrl_bitpos = lnbitsize - xrl_bitpos - rl_bitsize;
    }

  ll_mask = const_binop (LSHIFT_EXPR, fold_convert_loc (loc, lntype, ll_mask),
			 size_int (xll_bitpos));
  rl_mask = const_binop (LSHIFT_EXPR, fold_convert_loc (loc, lntype, rl_mask),
			 size_int (xrl_bitpos));
  if (ll_mask == NULL_TREE || rl_mask == NULL_TREE)
    return 0;

  if (l_const)
    {
      l_const = fold_convert_loc (loc, lntype, l_const);
      l_const = unextend (l_const, ll_bitsize, ll_unsignedp, ll_and_mask);
      l_const = const_binop (LSHIFT_EXPR, l_const, size_int (xll_bitpos));
      if (l_const == NULL_TREE)
	return 0;
      if (! integer_zerop (const_binop (BIT_AND_EXPR, l_const,
					fold_build1_loc (loc, BIT_NOT_EXPR,
							 lntype, ll_mask))))
	{
	  warning (0, "comparison is always %d", wanted_code == NE_EXPR);

	  return constant_boolean_node (wanted_code == NE_EXPR, truth_type);
	}
    }
  if (r_const)
    {
      r_const = fold_convert_loc (loc, lntype, r_const);
      r_const = unextend (r_const, rl_bitsize, rl_unsignedp, rl_and_mask);
      r_const = const_binop (LSHIFT_EXPR, r_const, size_int (xrl_bitpos));
      if (r_const == NULL_TREE)
	return 0;
      if (! integer_zerop (const_binop (BIT_AND_EXPR, r_const,
					fold_build1_loc (loc, BIT_NOT_EXPR,
							 lntype, rl_mask))))
	{
	  warning (0, "comparison is always %d", wanted_code == NE_EXPR);

	  return constant_boolean_node (wanted_code == NE_EXPR, truth_type);
	}
    }

  /* If the right sides are not constant, do the same for it.  Also,
     disallow this optimization if a size, signedness or storage order
     mismatch occurs between the left and right sides.  */
  if (l_const == 0)
    {
      if (ll_bitsize != lr_bitsize || rl_bitsize != rr_bitsize
	  || ll_unsignedp != lr_unsignedp || rl_unsignedp != rr_unsignedp
	  || ll_reversep != lr_reversep
	  /* Make sure the two fields on the right
	     correspond to the left without being swapped.  */
	  || ll_bitpos - rl_bitpos != lr_bitpos - rr_bitpos)
	return 0;

      first_bit = MIN (lr_bitpos, rr_bitpos);
      end_bit = MAX (lr_bitpos + lr_bitsize, rr_bitpos + rr_bitsize);
      if (!get_best_mode (end_bit - first_bit, first_bit, 0, 0,
			  TYPE_ALIGN (TREE_TYPE (lr_inner)), BITS_PER_WORD,
			  volatilep, &rnmode))
	return 0;

      rnbitsize = GET_MODE_BITSIZE (rnmode);
      rnbitpos = first_bit & ~ (rnbitsize - 1);
      rntype = lang_hooks.types.type_for_size (rnbitsize, 1);
      xlr_bitpos = lr_bitpos - rnbitpos, xrr_bitpos = rr_bitpos - rnbitpos;

      if (lr_reversep ? !BYTES_BIG_ENDIAN : BYTES_BIG_ENDIAN)
	{
	  xlr_bitpos = rnbitsize - xlr_bitpos - lr_bitsize;
	  xrr_bitpos = rnbitsize - xrr_bitpos - rr_bitsize;
	}

      lr_mask = const_binop (LSHIFT_EXPR, fold_convert_loc (loc,
							    rntype, lr_mask),
			     size_int (xlr_bitpos));
      rr_mask = const_binop (LSHIFT_EXPR, fold_convert_loc (loc,
							    rntype, rr_mask),
			     size_int (xrr_bitpos));
      if (lr_mask == NULL_TREE || rr_mask == NULL_TREE)
	return 0;

      /* Make a mask that corresponds to both fields being compared.
	 Do this for both items being compared.  If the operands are the
	 same size and the bits being compared are in the same position
	 then we can do this by masking both and comparing the masked
	 results.  */
      ll_mask = const_binop (BIT_IOR_EXPR, ll_mask, rl_mask);
      lr_mask = const_binop (BIT_IOR_EXPR, lr_mask, rr_mask);
      if (lnbitsize == rnbitsize
	  && xll_bitpos == xlr_bitpos
	  && lnbitpos >= 0
	  && rnbitpos >= 0)
	{
	  lhs = make_bit_field_ref (loc, ll_inner, ll_arg,
				    lntype, lnbitsize, lnbitpos,
				    ll_unsignedp || rl_unsignedp, ll_reversep);
	  if (! all_ones_mask_p (ll_mask, lnbitsize))
	    lhs = build2 (BIT_AND_EXPR, lntype, lhs, ll_mask);

	  rhs = make_bit_field_ref (loc, lr_inner, lr_arg,
				    rntype, rnbitsize, rnbitpos,
				    lr_unsignedp || rr_unsignedp, lr_reversep);
	  if (! all_ones_mask_p (lr_mask, rnbitsize))
	    rhs = build2 (BIT_AND_EXPR, rntype, rhs, lr_mask);

	  return build2_loc (loc, wanted_code, truth_type, lhs, rhs);
	}

      /* There is still another way we can do something:  If both pairs of
	 fields being compared are adjacent, we may be able to make a wider
	 field containing them both.

	 Note that we still must mask the lhs/rhs expressions.  Furthermore,
	 the mask must be shifted to account for the shift done by
	 make_bit_field_ref.  */
      if (((ll_bitsize + ll_bitpos == rl_bitpos
	    && lr_bitsize + lr_bitpos == rr_bitpos)
	   || (ll_bitpos == rl_bitpos + rl_bitsize
	       && lr_bitpos == rr_bitpos + rr_bitsize))
	  && ll_bitpos >= 0
	  && rl_bitpos >= 0
	  && lr_bitpos >= 0
	  && rr_bitpos >= 0)
	{
	  tree type;

	  lhs = make_bit_field_ref (loc, ll_inner, ll_arg, lntype,
				    ll_bitsize + rl_bitsize,
				    MIN (ll_bitpos, rl_bitpos),
				    ll_unsignedp, ll_reversep);
	  rhs = make_bit_field_ref (loc, lr_inner, lr_arg, rntype,
				    lr_bitsize + rr_bitsize,
				    MIN (lr_bitpos, rr_bitpos),
				    lr_unsignedp, lr_reversep);

	  ll_mask = const_binop (RSHIFT_EXPR, ll_mask,
				 size_int (MIN (xll_bitpos, xrl_bitpos)));
	  lr_mask = const_binop (RSHIFT_EXPR, lr_mask,
				 size_int (MIN (xlr_bitpos, xrr_bitpos)));
	  if (ll_mask == NULL_TREE || lr_mask == NULL_TREE)
	    return 0;

	  /* Convert to the smaller type before masking out unwanted bits.  */
	  type = lntype;
	  if (lntype != rntype)
	    {
	      if (lnbitsize > rnbitsize)
		{
		  lhs = fold_convert_loc (loc, rntype, lhs);
		  ll_mask = fold_convert_loc (loc, rntype, ll_mask);
		  type = rntype;
		}
	      else if (lnbitsize < rnbitsize)
		{
		  rhs = fold_convert_loc (loc, lntype, rhs);
		  lr_mask = fold_convert_loc (loc, lntype, lr_mask);
		  type = lntype;
		}
	    }

	  if (! all_ones_mask_p (ll_mask, ll_bitsize + rl_bitsize))
	    lhs = build2 (BIT_AND_EXPR, type, lhs, ll_mask);

	  if (! all_ones_mask_p (lr_mask, lr_bitsize + rr_bitsize))
	    rhs = build2 (BIT_AND_EXPR, type, rhs, lr_mask);

	  return build2_loc (loc, wanted_code, truth_type, lhs, rhs);
	}

      return 0;
    }

  /* Handle the case of comparisons with constants.  If there is something in
     common between the masks, those bits of the constants must be the same.
     If not, the condition is always false.  Test for this to avoid generating
     incorrect code below.  */
  result = const_binop (BIT_AND_EXPR, ll_mask, rl_mask);
  if (! integer_zerop (result)
      && simple_cst_equal (const_binop (BIT_AND_EXPR, result, l_const),
			   const_binop (BIT_AND_EXPR, result, r_const)) != 1)
    {
      if (wanted_code == NE_EXPR)
	{
	  warning (0, "%<or%> of unmatched not-equal tests is always 1");
	  return constant_boolean_node (true, truth_type);
	}
      else
	{
	  warning (0, "%<and%> of mutually exclusive equal-tests is always 0");
	  return constant_boolean_node (false, truth_type);
	}
    }

  if (lnbitpos < 0)
    return 0;

  /* Construct the expression we will return.  First get the component
     reference we will make.  Unless the mask is all ones the width of
     that field, perform the mask operation.  Then compare with the
     merged constant.  */
  result = make_bit_field_ref (loc, ll_inner, ll_arg,
			       lntype, lnbitsize, lnbitpos,
			       ll_unsignedp || rl_unsignedp, ll_reversep);

  ll_mask = const_binop (BIT_IOR_EXPR, ll_mask, rl_mask);
  if (! all_ones_mask_p (ll_mask, lnbitsize))
    result = build2_loc (loc, BIT_AND_EXPR, lntype, result, ll_mask);

  return build2_loc (loc, wanted_code, truth_type, result,
		     const_binop (BIT_IOR_EXPR, l_const, r_const));
}

/* T is an integer expression that is being multiplied, divided, or taken a
   modulus (CODE says which and what kind of divide or modulus) by a
   constant C.  See if we can eliminate that operation by folding it with
   other operations already in T.  WIDE_TYPE, if non-null, is a type that
   should be used for the computation if wider than our type.

   For example, if we are dividing (X * 8) + (Y * 16) by 4, we can return
   (X * 2) + (Y * 4).  We must, however, be assured that either the original
   expression would not overflow or that overflow is undefined for the type
   in the language in question.

   If we return a non-null expression, it is an equivalent form of the
   original computation, but need not be in the original type.

   We set *STRICT_OVERFLOW_P to true if the return values depends on
   signed overflow being undefined.  Otherwise we do not change
   *STRICT_OVERFLOW_P.  */

static tree
extract_muldiv (tree t, tree c, enum tree_code code, tree wide_type,
		bool *strict_overflow_p)
{
  /* To avoid exponential search depth, refuse to allow recursion past
     three levels.  Beyond that (1) it's highly unlikely that we'll find
     something interesting and (2) we've probably processed it before
     when we built the inner expression.  */

  static int depth;
  tree ret;

  if (depth > 3)
    return NULL;

  depth++;
  ret = extract_muldiv_1 (t, c, code, wide_type, strict_overflow_p);
  depth--;

  return ret;
}

static tree
extract_muldiv_1 (tree t, tree c, enum tree_code code, tree wide_type,
		  bool *strict_overflow_p)
{
  tree type = TREE_TYPE (t);
  enum tree_code tcode = TREE_CODE (t);
  tree ctype = type;
  if (wide_type)
    {
      if (TREE_CODE (type) == BITINT_TYPE
	  || TREE_CODE (wide_type) == BITINT_TYPE)
	{
	  if (TYPE_PRECISION (wide_type) > TYPE_PRECISION (type))
	    ctype = wide_type;
	}
      else if (GET_MODE_SIZE (SCALAR_INT_TYPE_MODE (wide_type))
	       > GET_MODE_SIZE (SCALAR_INT_TYPE_MODE (type)))
	ctype = wide_type;
    }
  tree t1, t2;
  bool same_p = tcode == code;
  tree op0 = NULL_TREE, op1 = NULL_TREE;
  bool sub_strict_overflow_p;

  /* Don't deal with constants of zero here; they confuse the code below.  */
  if (integer_zerop (c))
    return NULL_TREE;

  if (TREE_CODE_CLASS (tcode) == tcc_unary)
    op0 = TREE_OPERAND (t, 0);

  if (TREE_CODE_CLASS (tcode) == tcc_binary)
    op0 = TREE_OPERAND (t, 0), op1 = TREE_OPERAND (t, 1);

  /* Note that we need not handle conditional operations here since fold
     already handles those cases.  So just do arithmetic here.  */
  switch (tcode)
    {
    case INTEGER_CST:
      /* For a constant, we can always simplify if we are a multiply
	 or (for divide and modulus) if it is a multiple of our constant.  */
      if (code == MULT_EXPR
	  || wi::multiple_of_p (wi::to_wide (t), wi::to_wide (c),
				TYPE_SIGN (type)))
	{
	  tree tem = const_binop (code, fold_convert (ctype, t),
				  fold_convert (ctype, c));
	  /* If the multiplication overflowed, we lost information on it.
	     See PR68142 and PR69845.  */
	  if (TREE_OVERFLOW (tem))
	    return NULL_TREE;
	  return tem;
	}
      break;

    CASE_CONVERT: case NON_LVALUE_EXPR:
      if (!INTEGRAL_TYPE_P (TREE_TYPE (op0)))
	break;
      /* If op0 is an expression ...  */
      if ((COMPARISON_CLASS_P (op0)
	   || UNARY_CLASS_P (op0)
	   || BINARY_CLASS_P (op0)
	   || VL_EXP_CLASS_P (op0)
	   || EXPRESSION_CLASS_P (op0))
	  /* ... and has wrapping overflow, and its type is smaller
	     than ctype, then we cannot pass through as widening.  */
	  && ((TYPE_OVERFLOW_WRAPS (TREE_TYPE (op0))
	       && (TYPE_PRECISION (ctype)
	           > TYPE_PRECISION (TREE_TYPE (op0))))
	      /* ... or this is a truncation (t is narrower than op0),
		 then we cannot pass through this narrowing.  */
	      || (TYPE_PRECISION (type)
		  < TYPE_PRECISION (TREE_TYPE (op0)))
	      /* ... or signedness changes for division or modulus,
		 then we cannot pass through this conversion.  */
	      || (code != MULT_EXPR
		  && (TYPE_UNSIGNED (ctype)
		      != TYPE_UNSIGNED (TREE_TYPE (op0))))
	      /* ... or has undefined overflow while the converted to
		 type has not, we cannot do the operation in the inner type
		 as that would introduce undefined overflow.  */
	      || (TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (op0))
		  && !TYPE_OVERFLOW_UNDEFINED (type))))
	break;

      /* Pass the constant down and see if we can make a simplification.  If
	 we can, replace this expression with the inner simplification for
	 possible later conversion to our or some other type.  */
      if ((t2 = fold_convert (TREE_TYPE (op0), c)) != 0
	  && TREE_CODE (t2) == INTEGER_CST
	  && !TREE_OVERFLOW (t2)
	  && (t1 = extract_muldiv (op0, t2, code,
				   code == MULT_EXPR ? ctype : NULL_TREE,
				   strict_overflow_p)) != 0)
	return t1;
      break;

    case ABS_EXPR:
      /* If widening the type changes it from signed to unsigned, then we
         must avoid building ABS_EXPR itself as unsigned.  */
      if (TYPE_UNSIGNED (ctype) && !TYPE_UNSIGNED (type))
        {
          tree cstype = (*signed_type_for) (ctype);
          if ((t1 = extract_muldiv (op0, c, code, cstype, strict_overflow_p))
	      != 0)
            {
              t1 = fold_build1 (tcode, cstype, fold_convert (cstype, t1));
              return fold_convert (ctype, t1);
            }
          break;
        }
      /* If the constant is negative, we cannot simplify this.  */
      if (tree_int_cst_sgn (c) == -1)
        break;
      /* FALLTHROUGH */
    case NEGATE_EXPR:
      /* For division and modulus, type can't be unsigned, as e.g.
	 (-(x / 2U)) / 2U isn't equal to -((x / 2U) / 2U) for x >= 2.
	 For signed types, even with wrapping overflow, this is fine.  */
      if (code != MULT_EXPR && TYPE_UNSIGNED (type))
	break;
      if ((t1 = extract_muldiv (op0, c, code, wide_type, strict_overflow_p))
	  != 0)
	return fold_build1 (tcode, ctype, fold_convert (ctype, t1));
      break;

    case MIN_EXPR:  case MAX_EXPR:
      /* If widening the type changes the signedness, then we can't perform
	 this optimization as that changes the result.  */
      if (TYPE_UNSIGNED (ctype) != TYPE_UNSIGNED (type))
	break;

      /* Punt for multiplication altogether.
	 MAX (1U + INT_MAX, 1U) * 2U is not equivalent to
	 MAX ((1U + INT_MAX) * 2U, 1U * 2U), the former is
	 0U, the latter is 2U.
	 MAX (INT_MIN / 2, 0) * -2 is not equivalent to
	 MIN (INT_MIN / 2 * -2, 0 * -2), the former is
	 well defined 0, the latter invokes UB.
	 MAX (INT_MIN / 2, 5) * 5 is not equivalent to
	 MAX (INT_MIN / 2 * 5, 5 * 5), the former is
	 well defined 25, the latter invokes UB.  */
      if (code == MULT_EXPR)
	break;
      /* For division/modulo, punt on c being -1 for MAX, as
	 MAX (INT_MIN, 0) / -1 is not equivalent to
	 MIN (INT_MIN / -1, 0 / -1), the former is well defined
	 0, the latter invokes UB (or for -fwrapv is INT_MIN).
	 MIN (INT_MIN, 0) / -1 already invokes UB, so the
	 transformation won't make it worse.  */
      else if (tcode == MAX_EXPR && integer_minus_onep (c))
	break;

      /* MIN (a, b) / 5 -> MIN (a / 5, b / 5)  */
      sub_strict_overflow_p = false;
      if ((t1 = extract_muldiv (op0, c, code, wide_type,
				&sub_strict_overflow_p)) != 0
	  && (t2 = extract_muldiv (op1, c, code, wide_type,
				   &sub_strict_overflow_p)) != 0)
	{
	  if (tree_int_cst_sgn (c) < 0)
	    tcode = (tcode == MIN_EXPR ? MAX_EXPR : MIN_EXPR);
	  if (sub_strict_overflow_p)
	    *strict_overflow_p = true;
	  return fold_build2 (tcode, ctype, fold_convert (ctype, t1),
			      fold_convert (ctype, t2));
	}
      break;

    case LSHIFT_EXPR:  case RSHIFT_EXPR:
      /* If the second operand is constant, this is a multiplication
	 or floor division, by a power of two, so we can treat it that
	 way unless the multiplier or divisor overflows.  Signed
	 left-shift overflow is implementation-defined rather than
	 undefined in C90, so do not convert signed left shift into
	 multiplication.  */
      if (TREE_CODE (op1) == INTEGER_CST
	  && (tcode == RSHIFT_EXPR || TYPE_UNSIGNED (TREE_TYPE (op0)))
	  /* const_binop may not detect overflow correctly,
	     so check for it explicitly here.  */
	  && wi::gtu_p (TYPE_PRECISION (TREE_TYPE (size_one_node)),
			wi::to_wide (op1))
	  && (t1 = fold_convert (ctype,
				 const_binop (LSHIFT_EXPR, size_one_node,
					      op1))) != 0
	  && !TREE_OVERFLOW (t1))
	return extract_muldiv (build2 (tcode == LSHIFT_EXPR
				       ? MULT_EXPR : FLOOR_DIV_EXPR,
				       ctype,
				       fold_convert (ctype, op0),
				       t1),
			       c, code, wide_type, strict_overflow_p);
      break;

    case PLUS_EXPR:  case MINUS_EXPR:
      /* See if we can eliminate the operation on both sides.  If we can, we
	 can return a new PLUS or MINUS.  If we can't, the only remaining
	 cases where we can do anything are if the second operand is a
	 constant.  */
      sub_strict_overflow_p = false;
      t1 = extract_muldiv (op0, c, code, wide_type, &sub_strict_overflow_p);
      t2 = extract_muldiv (op1, c, code, wide_type, &sub_strict_overflow_p);
      if (t1 != 0 && t2 != 0
	  && TYPE_OVERFLOW_WRAPS (ctype)
	  && (code == MULT_EXPR
	      /* If not multiplication, we can only do this if both operands
		 are divisible by c.  */
	      || (multiple_of_p (ctype, op0, c)
	          && multiple_of_p (ctype, op1, c))))
	{
	  if (sub_strict_overflow_p)
	    *strict_overflow_p = true;
	  return fold_build2 (tcode, ctype, fold_convert (ctype, t1),
			      fold_convert (ctype, t2));
	}

      /* If this was a subtraction, negate OP1 and set it to be an addition.
	 This simplifies the logic below.  */
      if (tcode == MINUS_EXPR)
	{
	  tcode = PLUS_EXPR, op1 = negate_expr (op1);
	  /* If OP1 was not easily negatable, the constant may be OP0.  */
	  if (TREE_CODE (op0) == INTEGER_CST)
	    {
	      std::swap (op0, op1);
	      std::swap (t1, t2);
	    }
	}

      if (TREE_CODE (op1) != INTEGER_CST)
	break;

      /* If either OP1 or C are negative, this optimization is not safe for
	 some of the division and remainder types while for others we need
	 to change the code.  */
      if (tree_int_cst_sgn (op1) < 0 || tree_int_cst_sgn (c) < 0)
	{
	  if (code == CEIL_DIV_EXPR)
	    code = FLOOR_DIV_EXPR;
	  else if (code == FLOOR_DIV_EXPR)
	    code = CEIL_DIV_EXPR;
	  else if (code != MULT_EXPR
		   && code != CEIL_MOD_EXPR && code != FLOOR_MOD_EXPR)
	    break;
	}

      /* If it's a multiply or a division/modulus operation of a multiple
         of our constant, do the operation and verify it doesn't overflow.  */
      if (code == MULT_EXPR
	  || wi::multiple_of_p (wi::to_wide (op1), wi::to_wide (c),
				TYPE_SIGN (type)))
	{
	  op1 = const_binop (code, fold_convert (ctype, op1),
			     fold_convert (ctype, c));
	  /* We allow the constant to overflow with wrapping semantics.  */
	  if (op1 == 0
	      || (TREE_OVERFLOW (op1) && !TYPE_OVERFLOW_WRAPS (ctype)))
	    break;
	}
      else
	break;

      /* If we have an unsigned type, we cannot widen the operation since it
	 will change the result if the original computation overflowed.  */
      if (TYPE_UNSIGNED (ctype) && ctype != type)
	break;

      /* The last case is if we are a multiply.  In that case, we can
	 apply the distributive law to commute the multiply and addition
	 if the multiplication of the constants doesn't overflow
	 and overflow is defined.  With undefined overflow
	 op0 * c might overflow, while (op0 + orig_op1) * c doesn't.
	 But fold_plusminus_mult_expr would factor back any power-of-two
	 value so do not distribute in the first place in this case.  */
      if (code == MULT_EXPR
	  && TYPE_OVERFLOW_WRAPS (ctype)
	  && !(tree_fits_shwi_p (c) && pow2p_hwi (absu_hwi (tree_to_shwi (c)))))
	return fold_build2 (tcode, ctype,
			    fold_build2 (code, ctype,
					 fold_convert (ctype, op0),
					 fold_convert (ctype, c)),
			    op1);

      break;

    case MULT_EXPR:
      /* We have a special case here if we are doing something like
	 (C * 8) % 4 since we know that's zero.  */
      if ((code == TRUNC_MOD_EXPR || code == CEIL_MOD_EXPR
	   || code == FLOOR_MOD_EXPR || code == ROUND_MOD_EXPR)
	  /* If the multiplication can overflow we cannot optimize this.  */
	  && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (t))
	  && TREE_CODE (TREE_OPERAND (t, 1)) == INTEGER_CST
	  && wi::multiple_of_p (wi::to_wide (op1), wi::to_wide (c),
				TYPE_SIGN (type)))
	{
	  *strict_overflow_p = true;
	  return omit_one_operand (type, integer_zero_node, op0);
	}

      /* ... fall through ...  */

    case TRUNC_DIV_EXPR:  case CEIL_DIV_EXPR:  case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:  case EXACT_DIV_EXPR:
      /* If we can extract our operation from the LHS, do so and return a
	 new operation.  Likewise for the RHS from a MULT_EXPR.  Otherwise,
	 do something only if the second operand is a constant.  */
      if (same_p
	  && TYPE_OVERFLOW_WRAPS (ctype)
	  && (t1 = extract_muldiv (op0, c, code, wide_type,
				   strict_overflow_p)) != 0)
	return fold_build2 (tcode, ctype, fold_convert (ctype, t1),
			    fold_convert (ctype, op1));
      else if (tcode == MULT_EXPR && code == MULT_EXPR
	       && TYPE_OVERFLOW_WRAPS (ctype)
	       && (t1 = extract_muldiv (op1, c, code, wide_type,
					strict_overflow_p)) != 0)
	return fold_build2 (tcode, ctype, fold_convert (ctype, op0),
			    fold_convert (ctype, t1));
      else if (TREE_CODE (op1) != INTEGER_CST)
	return 0;

      /* If these are the same operation types, we can associate them
	 assuming no overflow.  */
      if (tcode == code)
	{
	  bool overflow_p = false;
	  wi::overflow_type overflow_mul;
	  signop sign = TYPE_SIGN (ctype);
	  unsigned prec = TYPE_PRECISION (ctype);
	  wide_int mul = wi::mul (wi::to_wide (op1, prec),
				  wi::to_wide (c, prec),
				  sign, &overflow_mul);
	  overflow_p = TREE_OVERFLOW (c) | TREE_OVERFLOW (op1);
	  if (overflow_mul
	      && ((sign == UNSIGNED && tcode != MULT_EXPR) || sign == SIGNED))
	    overflow_p = true;
	  if (!overflow_p)
	    return fold_build2 (tcode, ctype, fold_convert (ctype, op0),
				wide_int_to_tree (ctype, mul));
	}

      /* If these operations "cancel" each other, we have the main
	 optimizations of this pass, which occur when either constant is a
	 multiple of the other, in which case we replace this with either an
	 operation or CODE or TCODE.

	 If we have an unsigned type, we cannot do this since it will change
	 the result if the original computation overflowed.  */
      if (TYPE_OVERFLOW_UNDEFINED (ctype)
	  && !TYPE_OVERFLOW_SANITIZED (ctype)
	  && ((code == MULT_EXPR && tcode == EXACT_DIV_EXPR)
	      || (tcode == MULT_EXPR
		  && code != TRUNC_MOD_EXPR && code != CEIL_MOD_EXPR
		  && code != FLOOR_MOD_EXPR && code != ROUND_MOD_EXPR
		  && code != MULT_EXPR)))
	{
	  if (wi::multiple_of_p (wi::to_wide (op1), wi::to_wide (c),
				 TYPE_SIGN (type)))
	    {
	      *strict_overflow_p = true;
	      return fold_build2 (tcode, ctype, fold_convert (ctype, op0),
				  fold_convert (ctype,
						const_binop (TRUNC_DIV_EXPR,
							     op1, c)));
	    }
	  else if (wi::multiple_of_p (wi::to_wide (c), wi::to_wide (op1),
				      TYPE_SIGN (type)))
	    {
	      *strict_overflow_p = true;
	      return fold_build2 (code, ctype, fold_convert (ctype, op0),
				  fold_convert (ctype,
						const_binop (TRUNC_DIV_EXPR,
							     c, op1)));
	    }
	}
      break;

    default:
      break;
    }

  return 0;
}

/* Return a node which has the indicated constant VALUE (either 0 or
   1 for scalars or {-1,-1,..} or {0,0,...} for vectors),
   and is of the indicated TYPE.  */

tree
constant_boolean_node (bool value, tree type)
{
  if (type == integer_type_node)
    return value ? integer_one_node : integer_zero_node;
  else if (type == boolean_type_node)
    return value ? boolean_true_node : boolean_false_node;
  else if (VECTOR_TYPE_P (type))
    return build_vector_from_val (type,
				  build_int_cst (TREE_TYPE (type),
						 value ? -1 : 0));
  else
    return fold_convert (type, value ? integer_one_node : integer_zero_node);
}


/* Transform `a + (b ? x : y)' into `b ? (a + x) : (a + y)'.
   Transform, `a + (x < y)' into `(x < y) ? (a + 1) : (a + 0)'.  Here
   CODE corresponds to the `+', COND to the `(b ? x : y)' or `(x < y)'
   expression, and ARG to `a'.  If COND_FIRST_P is nonzero, then the
   COND is the first argument to CODE; otherwise (as in the example
   given here), it is the second argument.  TYPE is the type of the
   original expression.  Return NULL_TREE if no simplification is
   possible.  */

static tree
fold_binary_op_with_conditional_arg (location_t loc,
				     enum tree_code code,
				     tree type, tree op0, tree op1,
				     tree cond, tree arg, int cond_first_p)
{
  tree cond_type = cond_first_p ? TREE_TYPE (op0) : TREE_TYPE (op1);
  tree arg_type = cond_first_p ? TREE_TYPE (op1) : TREE_TYPE (op0);
  tree test, true_value, false_value;
  tree lhs = NULL_TREE;
  tree rhs = NULL_TREE;
  enum tree_code cond_code = COND_EXPR;

  /* Do not move possibly trapping operations into the conditional as this
     pessimizes code and causes gimplification issues when applied late.  */
  if (operation_could_trap_p (code, FLOAT_TYPE_P (type),
			      ANY_INTEGRAL_TYPE_P (type)
			      && TYPE_OVERFLOW_TRAPS (type), op1))
    return NULL_TREE;

  if (TREE_CODE (cond) == COND_EXPR
      || TREE_CODE (cond) == VEC_COND_EXPR)
    {
      test = TREE_OPERAND (cond, 0);
      true_value = TREE_OPERAND (cond, 1);
      false_value = TREE_OPERAND (cond, 2);
      /* If this operand throws an expression, then it does not make
	 sense to try to perform a logical or arithmetic operation
	 involving it.  */
      if (VOID_TYPE_P (TREE_TYPE (true_value)))
	lhs = true_value;
      if (VOID_TYPE_P (TREE_TYPE (false_value)))
	rhs = false_value;
    }
  else if (!(TREE_CODE (type) != VECTOR_TYPE
	     && VECTOR_TYPE_P (TREE_TYPE (cond))))
    {
      tree testtype = TREE_TYPE (cond);
      test = cond;
      true_value = constant_boolean_node (true, testtype);
      false_value = constant_boolean_node (false, testtype);
    }
  else
    /* Detect the case of mixing vector and scalar types - bail out.  */
    return NULL_TREE;

  if (VECTOR_TYPE_P (TREE_TYPE (test)))
    cond_code = VEC_COND_EXPR;

  /* This transformation is only worthwhile if we don't have to wrap ARG
     in a SAVE_EXPR and the operation can be simplified without recursing
     on at least one of the branches once its pushed inside the COND_EXPR.  */
  if (!TREE_CONSTANT (arg)
      && (TREE_SIDE_EFFECTS (arg)
	  || TREE_CODE (arg) == COND_EXPR || TREE_CODE (arg) == VEC_COND_EXPR
	  || TREE_CONSTANT (true_value) || TREE_CONSTANT (false_value)))
    return NULL_TREE;

  arg = fold_convert_loc (loc, arg_type, arg);
  if (lhs == 0)
    {
      true_value = fold_convert_loc (loc, cond_type, true_value);
      if (cond_first_p)
	lhs = fold_build2_loc (loc, code, type, true_value, arg);
      else
	lhs = fold_build2_loc (loc, code, type, arg, true_value);
    }
  if (rhs == 0)
    {
      false_value = fold_convert_loc (loc, cond_type, false_value);
      if (cond_first_p)
	rhs = fold_build2_loc (loc, code, type, false_value, arg);
      else
	rhs = fold_build2_loc (loc, code, type, arg, false_value);
    }

  /* Check that we have simplified at least one of the branches.  */
  if (!TREE_CONSTANT (arg) && !TREE_CONSTANT (lhs) && !TREE_CONSTANT (rhs))
    return NULL_TREE;

  return fold_build3_loc (loc, cond_code, type, test, lhs, rhs);
}


/* Subroutine of fold() that checks for the addition of ARG +/- 0.0.

   If !NEGATE, return true if ZERO_ARG is +/-0.0 and, for all ARG of
   type TYPE, ARG + ZERO_ARG is the same as ARG.  If NEGATE, return true
   if ARG - ZERO_ARG is the same as X.

   If ARG is NULL, check for any value of type TYPE.

   X + 0 and X - 0 both give X when X is NaN, infinite, or nonzero
   and finite.  The problematic cases are when X is zero, and its mode
   has signed zeros.  In the case of rounding towards -infinity,
   X - 0 is not the same as X because 0 - 0 is -0.  In other rounding
   modes, X + 0 is not the same as X because -0 + 0 is 0.  */

bool
fold_real_zero_addition_p (const_tree type, const_tree arg,
                           const_tree zero_arg, int negate)
{
  if (!real_zerop (zero_arg))
    return false;

  /* Don't allow the fold with -fsignaling-nans.  */
  if (arg ? tree_expr_maybe_signaling_nan_p (arg) : HONOR_SNANS (type))
    return false;

  /* Allow the fold if zeros aren't signed, or their sign isn't important.  */
  if (!HONOR_SIGNED_ZEROS (type))
    return true;

  /* There is no case that is safe for all rounding modes.  */
  if (HONOR_SIGN_DEPENDENT_ROUNDING (type))
    return false;

  /* In a vector or complex, we would need to check the sign of all zeros.  */
  if (TREE_CODE (zero_arg) == VECTOR_CST)
    zero_arg = uniform_vector_p (zero_arg);
  if (!zero_arg || TREE_CODE (zero_arg) != REAL_CST)
    return false;

  /* Treat x + -0 as x - 0 and x - -0 as x + 0.  */
  if (REAL_VALUE_MINUS_ZERO (TREE_REAL_CST (zero_arg)))
    negate = !negate;

  /* The mode has signed zeros, and we have to honor their sign.
     In this situation, there are only two cases we can return true for.
     (i) X - 0 is the same as X with default rounding.
     (ii) X + 0 is X when X can't possibly be -0.0.  */
  return negate || (arg && !tree_expr_maybe_real_minus_zero_p (arg));
}

/* Subroutine of match.pd that optimizes comparisons of a division by
   a nonzero integer constant against an integer constant, i.e.
   X/C1 op C2.

   CODE is the comparison operator: EQ_EXPR, NE_EXPR, GT_EXPR, LT_EXPR,
   GE_EXPR or LE_EXPR.  ARG01 and ARG1 must be a INTEGER_CST.  */

enum tree_code
fold_div_compare (enum tree_code code, tree c1, tree c2, tree *lo,
		  tree *hi, bool *neg_overflow)
{
  tree prod, tmp, type = TREE_TYPE (c1);
  signop sign = TYPE_SIGN (type);
  wi::overflow_type overflow;

  /* We have to do this the hard way to detect unsigned overflow.
     prod = int_const_binop (MULT_EXPR, c1, c2);  */
  wide_int val = wi::mul (wi::to_wide (c1), wi::to_wide (c2), sign, &overflow);
  prod = force_fit_type (type, val, -1, overflow);
  *neg_overflow = false;

  if (sign == UNSIGNED)
    {
      tmp = int_const_binop (MINUS_EXPR, c1, build_int_cst (type, 1));
      *lo = prod;

      /* Likewise *hi = int_const_binop (PLUS_EXPR, prod, tmp).  */
      val = wi::add (wi::to_wide (prod), wi::to_wide (tmp), sign, &overflow);
      *hi = force_fit_type (type, val, -1, overflow | TREE_OVERFLOW (prod));
    }
  else if (tree_int_cst_sgn (c1) >= 0)
    {
      tmp = int_const_binop (MINUS_EXPR, c1, build_int_cst (type, 1));
      switch (tree_int_cst_sgn (c2))
	{
	case -1:
	  *neg_overflow = true;
	  *lo = int_const_binop (MINUS_EXPR, prod, tmp);
	  *hi = prod;
	  break;

	case 0:
	  *lo = fold_negate_const (tmp, type);
	  *hi = tmp;
	  break;

	case 1:
	  *hi = int_const_binop (PLUS_EXPR, prod, tmp);
	  *lo = prod;
	  break;

	default:
	  gcc_unreachable ();
	}
    }
  else
    {
      /* A negative divisor reverses the relational operators.  */
      code = swap_tree_comparison (code);

      tmp = int_const_binop (PLUS_EXPR, c1, build_int_cst (type, 1));
      switch (tree_int_cst_sgn (c2))
	{
	case -1:
	  *hi = int_const_binop (MINUS_EXPR, prod, tmp);
	  *lo = prod;
	  break;

	case 0:
	  *hi = fold_negate_const (tmp, type);
	  *lo = tmp;
	  break;

	case 1:
	  *neg_overflow = true;
	  *lo = int_const_binop (PLUS_EXPR, prod, tmp);
	  *hi = prod;
	  break;

	default:
	  gcc_unreachable ();
	}
    }

  if (code != EQ_EXPR && code != NE_EXPR)
    return code;

  if (TREE_OVERFLOW (*lo)
      || operand_equal_p (*lo, TYPE_MIN_VALUE (type), 0))
    *lo = NULL_TREE;
  if (TREE_OVERFLOW (*hi)
      || operand_equal_p (*hi, TYPE_MAX_VALUE (type), 0))
    *hi = NULL_TREE;

  return code;
}

/* Test whether it is preferable to swap two operands, ARG0 and
   ARG1, for example because ARG0 is an integer constant and ARG1
   isn't.  */

bool
tree_swap_operands_p (const_tree arg0, const_tree arg1)
{
  if (CONSTANT_CLASS_P (arg1))
    return false;
  if (CONSTANT_CLASS_P (arg0))
    return true;

  STRIP_NOPS (arg0);
  STRIP_NOPS (arg1);

  if (TREE_CONSTANT (arg1))
    return false;
  if (TREE_CONSTANT (arg0))
    return true;

  /* It is preferable to swap two SSA_NAME to ensure a canonical form
     for commutative and comparison operators.  Ensuring a canonical
     form allows the optimizers to find additional redundancies without
     having to explicitly check for both orderings.  */
  if (TREE_CODE (arg0) == SSA_NAME
      && TREE_CODE (arg1) == SSA_NAME
      && SSA_NAME_VERSION (arg0) > SSA_NAME_VERSION (arg1))
    return true;

  /* Put SSA_NAMEs last.  */
  if (TREE_CODE (arg1) == SSA_NAME)
    return false;
  if (TREE_CODE (arg0) == SSA_NAME)
    return true;

  /* Put variables last.  */
  if (DECL_P (arg1))
    return false;
  if (DECL_P (arg0))
    return true;

  return false;
}


/* Fold A < X && A + 1 > Y to A < X && A >= Y.  Normally A + 1 > Y
   means A >= Y && A != MAX, but in this case we know that
   A < X <= MAX.  INEQ is A + 1 > Y, BOUND is A < X.  */

static tree
fold_to_nonsharp_ineq_using_bound (location_t loc, tree ineq, tree bound)
{
  tree a, typea, type = TREE_TYPE (bound), a1, diff, y;

  if (TREE_CODE (bound) == LT_EXPR)
    a = TREE_OPERAND (bound, 0);
  else if (TREE_CODE (bound) == GT_EXPR)
    a = TREE_OPERAND (bound, 1);
  else
    return NULL_TREE;

  typea = TREE_TYPE (a);
  if (!INTEGRAL_TYPE_P (typea)
      && !POINTER_TYPE_P (typea))
    return NULL_TREE;

  if (TREE_CODE (ineq) == LT_EXPR)
    {
      a1 = TREE_OPERAND (ineq, 1);
      y = TREE_OPERAND (ineq, 0);
    }
  else if (TREE_CODE (ineq) == GT_EXPR)
    {
      a1 = TREE_OPERAND (ineq, 0);
      y = TREE_OPERAND (ineq, 1);
    }
  else
    return NULL_TREE;

  if (TREE_TYPE (a1) != typea)
    return NULL_TREE;

  if (POINTER_TYPE_P (typea))
    {
      /* Convert the pointer types into integer before taking the difference.  */
      tree ta = fold_convert_loc (loc, ssizetype, a);
      tree ta1 = fold_convert_loc (loc, ssizetype, a1);
      diff = fold_binary_loc (loc, MINUS_EXPR, ssizetype, ta1, ta);
    }
  else
    diff = fold_binary_loc (loc, MINUS_EXPR, typea, a1, a);

  if (!diff || !integer_onep (diff))
   return NULL_TREE;

  return fold_build2_loc (loc, GE_EXPR, type, a, y);
}

/* Fold a sum or difference of at least one multiplication.
   Returns the folded tree or NULL if no simplification could be made.  */

static tree
fold_plusminus_mult_expr (location_t loc, enum tree_code code, tree type,
			  tree arg0, tree arg1)
{
  tree arg00, arg01, arg10, arg11;
  tree alt0 = NULL_TREE, alt1 = NULL_TREE, same;

  /* (A * C) +- (B * C) -> (A+-B) * C.
     (A * C) +- A -> A * (C+-1).
     We are most concerned about the case where C is a constant,
     but other combinations show up during loop reduction.  Since
     it is not difficult, try all four possibilities.  */

  if (TREE_CODE (arg0) == MULT_EXPR)
    {
      arg00 = TREE_OPERAND (arg0, 0);
      arg01 = TREE_OPERAND (arg0, 1);
    }
  else if (TREE_CODE (arg0) == INTEGER_CST)
    {
      arg00 = build_one_cst (type);
      arg01 = arg0;
    }
  else
    {
      /* We cannot generate constant 1 for fract.  */
      if (ALL_FRACT_MODE_P (TYPE_MODE (type)))
	return NULL_TREE;
      arg00 = arg0;
      arg01 = build_one_cst (type);
    }
  if (TREE_CODE (arg1) == MULT_EXPR)
    {
      arg10 = TREE_OPERAND (arg1, 0);
      arg11 = TREE_OPERAND (arg1, 1);
    }
  else if (TREE_CODE (arg1) == INTEGER_CST)
    {
      arg10 = build_one_cst (type);
      /* As we canonicalize A - 2 to A + -2 get rid of that sign for
	 the purpose of this canonicalization.  */
      if (wi::neg_p (wi::to_wide (arg1), TYPE_SIGN (TREE_TYPE (arg1)))
	  && negate_expr_p (arg1)
	  && code == PLUS_EXPR)
	{
	  arg11 = negate_expr (arg1);
	  code = MINUS_EXPR;
	}
      else
	arg11 = arg1;
    }
  else
    {
      /* We cannot generate constant 1 for fract.  */
      if (ALL_FRACT_MODE_P (TYPE_MODE (type)))
	return NULL_TREE;
      arg10 = arg1;
      arg11 = build_one_cst (type);
    }
  same = NULL_TREE;

  /* Prefer factoring a common non-constant.  */
  if (operand_equal_p (arg00, arg10, 0))
    same = arg00, alt0 = arg01, alt1 = arg11;
  else if (operand_equal_p (arg01, arg11, 0))
    same = arg01, alt0 = arg00, alt1 = arg10;
  else if (operand_equal_p (arg00, arg11, 0))
    same = arg00, alt0 = arg01, alt1 = arg10;
  else if (operand_equal_p (arg01, arg10, 0))
    same = arg01, alt0 = arg00, alt1 = arg11;

  /* No identical multiplicands; see if we can find a common
     power-of-two factor in non-power-of-two multiplies.  This
     can help in multi-dimensional array access.  */
  else if (tree_fits_shwi_p (arg01) && tree_fits_shwi_p (arg11))
    {
      HOST_WIDE_INT int01 = tree_to_shwi (arg01);
      HOST_WIDE_INT int11 = tree_to_shwi (arg11);
      HOST_WIDE_INT tmp;
      bool swap = false;
      tree maybe_same;

      /* Move min of absolute values to int11.  */
      if (absu_hwi (int01) < absu_hwi (int11))
        {
	  tmp = int01, int01 = int11, int11 = tmp;
	  alt0 = arg00, arg00 = arg10, arg10 = alt0;
	  maybe_same = arg01;
	  swap = true;
	}
      else
	maybe_same = arg11;

      const unsigned HOST_WIDE_INT factor = absu_hwi (int11);
      if (factor > 1
	  && pow2p_hwi (factor)
	  && (int01 & (factor - 1)) == 0
	  /* The remainder should not be a constant, otherwise we
	     end up folding i * 4 + 2 to (i * 2 + 1) * 2 which has
	     increased the number of multiplications necessary.  */
	  && TREE_CODE (arg10) != INTEGER_CST)
        {
	  alt0 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (arg00), arg00,
			      build_int_cst (TREE_TYPE (arg00),
					     int01 / int11));
	  alt1 = arg10;
	  same = maybe_same;
	  if (swap)
	    maybe_same = alt0, alt0 = alt1, alt1 = maybe_same;
	}
    }

  if (!same)
    return NULL_TREE;

  if (! ANY_INTEGRAL_TYPE_P (type)
      || TYPE_OVERFLOW_WRAPS (type)
      /* We are neither factoring zero nor minus one.  */
      || TREE_CODE (same) == INTEGER_CST)
    return fold_build2_loc (loc, MULT_EXPR, type,
			fold_build2_loc (loc, code, type,
				     fold_convert_loc (loc, type, alt0),
				     fold_convert_loc (loc, type, alt1)),
			fold_convert_loc (loc, type, same));

  /* Same may be zero and thus the operation 'code' may overflow.  Likewise
     same may be minus one and thus the multiplication may overflow.  Perform
     the sum operation in an unsigned type.  */
  tree utype = unsigned_type_for (type);
  tree tem = fold_build2_loc (loc, code, utype,
			      fold_convert_loc (loc, utype, alt0),
			      fold_convert_loc (loc, utype, alt1));
  /* If the sum evaluated to a constant that is not -INF the multiplication
     cannot overflow.  */
  if (TREE_CODE (tem) == INTEGER_CST
      && (wi::to_wide (tem)
	  != wi::min_value (TYPE_PRECISION (utype), SIGNED)))
    return fold_build2_loc (loc, MULT_EXPR, type,
			    fold_convert (type, tem), same);

  /* Do not resort to unsigned multiplication because
     we lose the no-overflow property of the expression.  */
  return NULL_TREE;
}

/* Subroutine of native_encode_expr.  Encode the INTEGER_CST
   specified by EXPR into the buffer PTR of length LEN bytes.
   Return the number of bytes placed in the buffer, or zero
   upon failure.  */

static int
native_encode_int (const_tree expr, unsigned char *ptr, int len, int off)
{
  tree type = TREE_TYPE (expr);
  int total_bytes;
  if (TREE_CODE (type) == BITINT_TYPE)
    {
      struct bitint_info info;
      bool ok = targetm.c.bitint_type_info (TYPE_PRECISION (type), &info);
      gcc_assert (ok);
      scalar_int_mode limb_mode = as_a <scalar_int_mode> (info.limb_mode);
      if (TYPE_PRECISION (type) > GET_MODE_PRECISION (limb_mode))
	{
	  total_bytes = tree_to_uhwi (TYPE_SIZE_UNIT (type));
	  /* More work is needed when adding _BitInt support to PDP endian
	     if limb is smaller than word, or if _BitInt limb ordering doesn't
	     match target endianity here.  */
	  gcc_checking_assert (info.big_endian == WORDS_BIG_ENDIAN
			       && (BYTES_BIG_ENDIAN == WORDS_BIG_ENDIAN
				   || (GET_MODE_SIZE (limb_mode)
				       >= UNITS_PER_WORD)));
	}
      else
	total_bytes = GET_MODE_SIZE (SCALAR_INT_TYPE_MODE (type));
    }
  else
    total_bytes = GET_MODE_SIZE (SCALAR_INT_TYPE_MODE (type));
  int byte, offset, word, words;
  unsigned char value;

  if ((off == -1 && total_bytes > len) || off >= total_bytes)
    return 0;
  if (off == -1)
    off = 0;

  if (ptr == NULL)
    /* Dry run.  */
    return MIN (len, total_bytes - off);

  words = total_bytes / UNITS_PER_WORD;

  for (byte = 0; byte < total_bytes; byte++)
    {
      int bitpos = byte * BITS_PER_UNIT;
      /* Extend EXPR according to TYPE_SIGN if the precision isn't a whole
	 number of bytes.  */
      value = wi::extract_uhwi (wi::to_widest (expr), bitpos, BITS_PER_UNIT);

      if (total_bytes > UNITS_PER_WORD)
	{
	  word = byte / UNITS_PER_WORD;
	  if (WORDS_BIG_ENDIAN)
	    word = (words - 1) - word;
	  offset = word * UNITS_PER_WORD;
	  if (BYTES_BIG_ENDIAN)
	    offset += (UNITS_PER_WORD - 1) - (byte % UNITS_PER_WORD);
	  else
	    offset += byte % UNITS_PER_WORD;
	}
      else
	offset = BYTES_BIG_ENDIAN ? (total_bytes - 1) - byte : byte;
      if (offset >= off && offset - off < len)
	ptr[offset - off] = value;
    }
  return MIN (len, total_bytes - off);
}


/* Subroutine of native_encode_expr.  Encode the FIXED_CST
   specified by EXPR into the buffer PTR of length LEN bytes.
   Return the number of bytes placed in the buffer, or zero
   upon failure.  */

static int
native_encode_fixed (const_tree expr, unsigned char *ptr, int len, int off)
{
  tree type = TREE_TYPE (expr);
  scalar_mode mode = SCALAR_TYPE_MODE (type);
  int total_bytes = GET_MODE_SIZE (mode);
  FIXED_VALUE_TYPE value;
  tree i_value, i_type;

  if (total_bytes * BITS_PER_UNIT > HOST_BITS_PER_DOUBLE_INT)
    return 0;

  i_type = lang_hooks.types.type_for_size (GET_MODE_BITSIZE (mode), 1);

  if (NULL_TREE == i_type || TYPE_PRECISION (i_type) != total_bytes)
    return 0;
  
  value = TREE_FIXED_CST (expr);
  i_value = double_int_to_tree (i_type, value.data);

  return native_encode_int (i_value, ptr, len, off);
}


/* Subroutine of native_encode_expr.  Encode the REAL_CST
   specified by EXPR into the buffer PTR of length LEN bytes.
   Return the number of bytes placed in the buffer, or zero
   upon failure.  */

static int
native_encode_real (const_tree expr, unsigned char *ptr, int len, int off)
{
  tree type = TREE_TYPE (expr);
  int total_bytes = GET_MODE_SIZE (SCALAR_FLOAT_TYPE_MODE (type));
  int byte, offset, word, words, bitpos;
  unsigned char value;

  /* There are always 32 bits in each long, no matter the size of
     the hosts long.  We handle floating point representations with
     up to 192 bits.  */
  long tmp[6];

  if ((off == -1 && total_bytes > len) || off >= total_bytes)
    return 0;
  if (off == -1)
    off = 0;

  if (ptr == NULL)
    /* Dry run.  */
    return MIN (len, total_bytes - off);

  words = (32 / BITS_PER_UNIT) / UNITS_PER_WORD;

  real_to_target (tmp, TREE_REAL_CST_PTR (expr), TYPE_MODE (type));

  for (bitpos = 0; bitpos < total_bytes * BITS_PER_UNIT;
       bitpos += BITS_PER_UNIT)
    {
      byte = (bitpos / BITS_PER_UNIT) & 3;
      value = (unsigned char) (tmp[bitpos / 32] >> (bitpos & 31));

      if (UNITS_PER_WORD < 4)
	{
	  word = byte / UNITS_PER_WORD;
	  if (WORDS_BIG_ENDIAN)
	    word = (words - 1) - word;
	  offset = word * UNITS_PER_WORD;
	  if (BYTES_BIG_ENDIAN)
	    offset += (UNITS_PER_WORD - 1) - (byte % UNITS_PER_WORD);
	  else
	    offset += byte % UNITS_PER_WORD;
	}
      else
	{
	  offset = byte;
	  if (BYTES_BIG_ENDIAN)
	    {
	      /* Reverse bytes within each long, or within the entire float
		 if it's smaller than a long (for HFmode).  */
	      offset = MIN (3, total_bytes - 1) - offset;
	      gcc_assert (offset >= 0);
	    }
	}
      offset = offset + ((bitpos / BITS_PER_UNIT) & ~3);
      if (offset >= off
	  && offset - off < len)
	ptr[offset - off] = value;
    }
  return MIN (len, total_bytes - off);
}

/* Subroutine of native_encode_expr.  Encode the COMPLEX_CST
   specified by EXPR into the buffer PTR of length LEN bytes.
   Return the number of bytes placed in the buffer, or zero
   upon failure.  */

static int
native_encode_complex (const_tree expr, unsigned char *ptr, int len, int off)
{
  int rsize, isize;
  tree part;

  part = TREE_REALPART (expr);
  rsize = native_encode_expr (part, ptr, len, off);
  if (off == -1 && rsize == 0)
    return 0;
  part = TREE_IMAGPART (expr);
  if (off != -1)
    off = MAX (0, off - GET_MODE_SIZE (SCALAR_TYPE_MODE (TREE_TYPE (part))));
  isize = native_encode_expr (part, ptr ? ptr + rsize : NULL,
			      len - rsize, off);
  if (off == -1 && isize != rsize)
    return 0;
  return rsize + isize;
}

/* Like native_encode_vector, but only encode the first COUNT elements.
   The other arguments are as for native_encode_vector.  */

static int
native_encode_vector_part (const_tree expr, unsigned char *ptr, int len,
			   int off, unsigned HOST_WIDE_INT count)
{
  tree itype = TREE_TYPE (TREE_TYPE (expr));
  if (VECTOR_BOOLEAN_TYPE_P (TREE_TYPE (expr))
      && TYPE_PRECISION (itype) <= BITS_PER_UNIT)
    {
      /* This is the only case in which elements can be smaller than a byte.
	 Element 0 is always in the lsb of the containing byte.  */
      unsigned int elt_bits = TYPE_PRECISION (itype);
      int total_bytes = CEIL (elt_bits * count, BITS_PER_UNIT);
      if ((off == -1 && total_bytes > len) || off >= total_bytes)
	return 0;

      if (off == -1)
	off = 0;

      /* Zero the buffer and then set bits later where necessary.  */
      int extract_bytes = MIN (len, total_bytes - off);
      if (ptr)
	memset (ptr, 0, extract_bytes);

      unsigned int elts_per_byte = BITS_PER_UNIT / elt_bits;
      unsigned int first_elt = off * elts_per_byte;
      unsigned int extract_elts = extract_bytes * elts_per_byte;
      unsigned int elt_mask = (1 << elt_bits) - 1;
      for (unsigned int i = 0; i < extract_elts; ++i)
	{
	  tree elt = VECTOR_CST_ELT (expr, first_elt + i);
	  if (TREE_CODE (elt) != INTEGER_CST)
	    return 0;

	  if (ptr && integer_nonzerop (elt))
	    {
	      unsigned int bit = i * elt_bits;
	      ptr[bit / BITS_PER_UNIT] |= elt_mask << (bit % BITS_PER_UNIT);
	    }
	}
      return extract_bytes;
    }

  int offset = 0;
  int size = GET_MODE_SIZE (SCALAR_TYPE_MODE (itype));
  for (unsigned HOST_WIDE_INT i = 0; i < count; i++)
    {
      if (off >= size)
	{
	  off -= size;
	  continue;
	}
      tree elem = VECTOR_CST_ELT (expr, i);
      int res = native_encode_expr (elem, ptr ? ptr + offset : NULL,
				    len - offset, off);
      if ((off == -1 && res != size) || res == 0)
	return 0;
      offset += res;
      if (offset >= len)
	return (off == -1 && i < count - 1) ? 0 : offset;
      if (off != -1)
	off = 0;
    }
  return offset;
}

/* Subroutine of native_encode_expr.  Encode the VECTOR_CST
   specified by EXPR into the buffer PTR of length LEN bytes.
   Return the number of bytes placed in the buffer, or zero
   upon failure.  */

static int
native_encode_vector (const_tree expr, unsigned char *ptr, int len, int off)
{
  unsigned HOST_WIDE_INT count;
  if (!VECTOR_CST_NELTS (expr).is_constant (&count))
    return 0;
  return native_encode_vector_part (expr, ptr, len, off, count);
}


/* Subroutine of native_encode_expr.  Encode the STRING_CST
   specified by EXPR into the buffer PTR of length LEN bytes.
   Return the number of bytes placed in the buffer, or zero
   upon failure.  */

static int
native_encode_string (const_tree expr, unsigned char *ptr, int len, int off)
{
  tree type = TREE_TYPE (expr);

  /* Wide-char strings are encoded in target byte-order so native
     encoding them is trivial.  */
  if (BITS_PER_UNIT != CHAR_BIT
      || TREE_CODE (type) != ARRAY_TYPE
      || TREE_CODE (TREE_TYPE (type)) != INTEGER_TYPE
      || !tree_fits_shwi_p (TYPE_SIZE_UNIT (type)))
    return 0;

  HOST_WIDE_INT total_bytes = tree_to_shwi (TYPE_SIZE_UNIT (TREE_TYPE (expr)));
  if ((off == -1 && total_bytes > len) || off >= total_bytes)
    return 0;
  if (off == -1)
    off = 0;
  len = MIN (total_bytes - off, len);
  if (ptr == NULL)
    /* Dry run.  */;
  else
    {
      int written = 0;
      if (off < TREE_STRING_LENGTH (expr))
	{
	  written = MIN (len, TREE_STRING_LENGTH (expr) - off);
	  memcpy (ptr, TREE_STRING_POINTER (expr) + off, written);
	}
      memset (ptr + written, 0, len - written);
    }
  return len;
}

/* Subroutine of native_encode_expr.  Encode the CONSTRUCTOR
   specified by EXPR into the buffer PTR of length LEN bytes.
   Return the number of bytes placed in the buffer, or zero
   upon failure.  */

static int
native_encode_constructor (const_tree expr, unsigned char *ptr, int len, int off)
{
  /* We are only concerned with zero-initialization constructors here.  That's
     all we expect to see in GIMPLE, so that's all native_encode_expr should
     deal with.  For more general handling of constructors, there is
     native_encode_initializer.  */
  if (CONSTRUCTOR_NELTS (expr))
    return 0;

  /* Wide-char strings are encoded in target byte-order so native
     encoding them is trivial.  */
  if (BITS_PER_UNIT != CHAR_BIT
      || !tree_fits_shwi_p (TYPE_SIZE_UNIT (TREE_TYPE (expr))))
    return 0;

  HOST_WIDE_INT total_bytes = tree_to_shwi (TYPE_SIZE_UNIT (TREE_TYPE (expr)));
  if ((off == -1 && total_bytes > len) || off >= total_bytes)
    return 0;
  if (off == -1)
    off = 0;
  len = MIN (total_bytes - off, len);
  if (ptr == NULL)
    /* Dry run.  */;
  else
    memset (ptr, 0, len);
  return len;
}

/* Subroutine of fold_view_convert_expr.  Encode the INTEGER_CST, REAL_CST,
   FIXED_CST, COMPLEX_CST, STRING_CST, or VECTOR_CST specified by EXPR into
   the buffer PTR of size LEN bytes.  If PTR is NULL, don't actually store
   anything, just do a dry run.  Fail either if OFF is -1 and LEN isn't
   sufficient to encode the entire EXPR, or if OFF is out of bounds.
   Otherwise, start at byte offset OFF and encode at most LEN bytes.
   Return the number of bytes placed in the buffer, or zero upon failure.  */

int
native_encode_expr (const_tree expr, unsigned char *ptr, int len, int off)
{
  /* We don't support starting at negative offset and -1 is special.  */
  if (off < -1)
    return 0;

  switch (TREE_CODE (expr))
    {
    case INTEGER_CST:
      return native_encode_int (expr, ptr, len, off);

    case REAL_CST:
      return native_encode_real (expr, ptr, len, off);

    case FIXED_CST:
      return native_encode_fixed (expr, ptr, len, off);

    case COMPLEX_CST:
      return native_encode_complex (expr, ptr, len, off);

    case VECTOR_CST:
      return native_encode_vector (expr, ptr, len, off);

    case STRING_CST:
      return native_encode_string (expr, ptr, len, off);

    case CONSTRUCTOR:
      return native_encode_constructor (expr, ptr, len, off);

    default:
      return 0;
    }
}

/* Try to find a type whose byte size is smaller or equal to LEN bytes larger
   or equal to FIELDSIZE bytes, with underlying mode precision/size multiple
   of BITS_PER_UNIT.  As native_{interpret,encode}_int works in term of
   machine modes, we can't just use build_nonstandard_integer_type.  */

tree
find_bitfield_repr_type (int fieldsize, int len)
{
  machine_mode mode;
  for (int pass = 0; pass < 2; pass++)
    {
      enum mode_class mclass = pass ? MODE_PARTIAL_INT : MODE_INT;
      FOR_EACH_MODE_IN_CLASS (mode, mclass)
	if (known_ge (GET_MODE_SIZE (mode), fieldsize)
	    && known_eq (GET_MODE_PRECISION (mode),
			 GET_MODE_BITSIZE (mode))
	    && known_le (GET_MODE_SIZE (mode), len))
	  {
	    tree ret = lang_hooks.types.type_for_mode (mode, 1);
	    if (ret && TYPE_MODE (ret) == mode)
	      return ret;
	  }
    }

  for (int i = 0; i < NUM_INT_N_ENTS; i ++)
    if (int_n_enabled_p[i]
	&& int_n_data[i].bitsize >= (unsigned) (BITS_PER_UNIT * fieldsize)
	&& int_n_trees[i].unsigned_type)
      {
	tree ret = int_n_trees[i].unsigned_type;
	mode = TYPE_MODE (ret);
	if (known_ge (GET_MODE_SIZE (mode), fieldsize)
	    && known_eq (GET_MODE_PRECISION (mode),
			 GET_MODE_BITSIZE (mode))
	    && known_le (GET_MODE_SIZE (mode), len))
	  return ret;
      }

  return NULL_TREE;
}

/* Similar to native_encode_expr, but also handle CONSTRUCTORs, VCEs,
   NON_LVALUE_EXPRs and nops.  If MASK is non-NULL (then PTR has
   to be non-NULL and OFF zero), then in addition to filling the
   bytes pointed by PTR with the value also clear any bits pointed
   by MASK that are known to be initialized, keep them as is for
   e.g. uninitialized padding bits or uninitialized fields.  */

int
native_encode_initializer (tree init, unsigned char *ptr, int len,
			   int off, unsigned char *mask)
{
  int r;

  /* We don't support starting at negative offset and -1 is special.  */
  if (off < -1 || init == NULL_TREE)
    return 0;

  gcc_assert (mask == NULL || (off == 0 && ptr));

  STRIP_NOPS (init);
  switch (TREE_CODE (init))
    {
    case VIEW_CONVERT_EXPR:
    case NON_LVALUE_EXPR:
      return native_encode_initializer (TREE_OPERAND (init, 0), ptr, len, off,
					mask);
    default:
      r = native_encode_expr (init, ptr, len, off);
      if (mask)
	memset (mask, 0, r);
      return r;
    case CONSTRUCTOR:
      tree type = TREE_TYPE (init);
      HOST_WIDE_INT total_bytes = int_size_in_bytes (type);
      if (total_bytes < 0)
	return 0;
      if ((off == -1 && total_bytes > len) || off >= total_bytes)
	return 0;
      int o = off == -1 ? 0 : off;
      if (TREE_CODE (type) == ARRAY_TYPE)
	{
	  tree min_index;
	  unsigned HOST_WIDE_INT cnt;
	  HOST_WIDE_INT curpos = 0, fieldsize, valueinit = -1;
	  constructor_elt *ce;

	  if (!TYPE_DOMAIN (type)
	      || TREE_CODE (TYPE_MIN_VALUE (TYPE_DOMAIN (type))) != INTEGER_CST)
	    return 0;

	  fieldsize = int_size_in_bytes (TREE_TYPE (type));
	  if (fieldsize <= 0)
	    return 0;

	  min_index = TYPE_MIN_VALUE (TYPE_DOMAIN (type));
	  if (ptr)
	    memset (ptr, '\0', MIN (total_bytes - off, len));

	  for (cnt = 0; ; cnt++)
	    {
	      tree val = NULL_TREE, index = NULL_TREE;
	      HOST_WIDE_INT pos = curpos, count = 0;
	      bool full = false;
	      if (vec_safe_iterate (CONSTRUCTOR_ELTS (init), cnt, &ce))
		{
		  val = ce->value;
		  index = ce->index;
		}
	      else if (mask == NULL
		       || CONSTRUCTOR_NO_CLEARING (init)
		       || curpos >= total_bytes)
		break;
	      else
		pos = total_bytes;

	      if (index && TREE_CODE (index) == RANGE_EXPR)
		{
		  if (TREE_CODE (TREE_OPERAND (index, 0)) != INTEGER_CST
		      || TREE_CODE (TREE_OPERAND (index, 1)) != INTEGER_CST)
		    return 0;
		  offset_int wpos
		    = wi::sext (wi::to_offset (TREE_OPERAND (index, 0))
				- wi::to_offset (min_index),
				TYPE_PRECISION (sizetype));
		  wpos *= fieldsize;
		  if (!wi::fits_shwi_p (pos))
		    return 0;
		  pos = wpos.to_shwi ();
		  offset_int wcount
		    = wi::sext (wi::to_offset (TREE_OPERAND (index, 1))
				- wi::to_offset (TREE_OPERAND (index, 0)),
				TYPE_PRECISION (sizetype));
		  if (!wi::fits_shwi_p (wcount))
		    return 0;
		  count = wcount.to_shwi ();
		}
	      else if (index)
		{
		  if (TREE_CODE (index) != INTEGER_CST)
		    return 0;
		  offset_int wpos
		    = wi::sext (wi::to_offset (index)
				- wi::to_offset (min_index),
				TYPE_PRECISION (sizetype));
		  wpos *= fieldsize;
		  if (!wi::fits_shwi_p (wpos))
		    return 0;
		  pos = wpos.to_shwi ();
		}

	      if (mask && !CONSTRUCTOR_NO_CLEARING (init) && curpos != pos)
		{
		  if (valueinit == -1)
		    {
		      tree zero = build_zero_cst (TREE_TYPE (type));
		      r = native_encode_initializer (zero, ptr + curpos,
						     fieldsize, 0,
						     mask + curpos);
		      if (TREE_CODE (zero) == CONSTRUCTOR)
			ggc_free (zero);
		      if (!r)
			return 0;
		      valueinit = curpos;
		      curpos += fieldsize;
		    }
		  while (curpos != pos)
		    {
		      memcpy (ptr + curpos, ptr + valueinit, fieldsize);
		      memcpy (mask + curpos, mask + valueinit, fieldsize);
		      curpos += fieldsize;
		    }
		}

	      curpos = pos;
	      if (val)
		do
		  {
		    if (off == -1
			|| (curpos >= off
			    && (curpos + fieldsize
				<= (HOST_WIDE_INT) off + len)))
		      {
			if (full)
			  {
			    if (ptr)
			      memcpy (ptr + (curpos - o), ptr + (pos - o),
				      fieldsize);
			    if (mask)
			      memcpy (mask + curpos, mask + pos, fieldsize);
			  }
			else if (!native_encode_initializer (val,
							     ptr
							     ? ptr + curpos - o
							     : NULL,
							     fieldsize,
							     off == -1 ? -1
								       : 0,
							     mask
							     ? mask + curpos
							     : NULL))
			  return 0;
			else
			  {
			    full = true;
			    pos = curpos;
			  }
		      }
		    else if (curpos + fieldsize > off
			     && curpos < (HOST_WIDE_INT) off + len)
		      {
			/* Partial overlap.  */
			unsigned char *p = NULL;
			int no = 0;
			int l;
			gcc_assert (mask == NULL);
			if (curpos >= off)
			  {
			    if (ptr)
			      p = ptr + curpos - off;
			    l = MIN ((HOST_WIDE_INT) off + len - curpos,
				     fieldsize);
			  }
			else
			  {
			    p = ptr;
			    no = off - curpos;
			    l = len;
			  }
			if (!native_encode_initializer (val, p, l, no, NULL))
			  return 0;
		      }
		    curpos += fieldsize;
		  }
		while (count-- != 0);
	    }
	  return MIN (total_bytes - off, len);
	}
      else if (TREE_CODE (type) == RECORD_TYPE
	       || TREE_CODE (type) == UNION_TYPE)
	{
	  unsigned HOST_WIDE_INT cnt;
	  constructor_elt *ce;
	  tree fld_base = TYPE_FIELDS (type);
	  tree to_free = NULL_TREE;

	  gcc_assert (TREE_CODE (type) == RECORD_TYPE || mask == NULL);
	  if (ptr != NULL)
	    memset (ptr, '\0', MIN (total_bytes - o, len));
	  for (cnt = 0; ; cnt++)
	    {
	      tree val = NULL_TREE, field = NULL_TREE;
	      HOST_WIDE_INT pos = 0, fieldsize;
	      unsigned HOST_WIDE_INT bpos = 0, epos = 0;

	      if (to_free)
		{
		  ggc_free (to_free);
		  to_free = NULL_TREE;
		}

	      if (vec_safe_iterate (CONSTRUCTOR_ELTS (init), cnt, &ce))
		{
		  val = ce->value;
		  field = ce->index;
		  if (field == NULL_TREE)
		    return 0;

		  pos = int_byte_position (field);
		  if (off != -1 && (HOST_WIDE_INT) off + len <= pos)
		    continue;
		}
	      else if (mask == NULL
		       || CONSTRUCTOR_NO_CLEARING (init))
		break;
	      else
		pos = total_bytes;

	      if (mask && !CONSTRUCTOR_NO_CLEARING (init))
		{
		  tree fld;
		  for (fld = fld_base; fld; fld = DECL_CHAIN (fld))
		    {
		      if (TREE_CODE (fld) != FIELD_DECL)
			continue;
		      if (fld == field)
			break;
		      if (DECL_PADDING_P (fld))
			continue;
		      if (DECL_SIZE_UNIT (fld) == NULL_TREE
			  || !tree_fits_shwi_p (DECL_SIZE_UNIT (fld)))
			return 0;
		      if (integer_zerop (DECL_SIZE_UNIT (fld)))
			continue;
		      break;
		    }
		  if (fld == NULL_TREE)
		    {
		      if (ce == NULL)
			break;
		      return 0;
		    }
		  fld_base = DECL_CHAIN (fld);
		  if (fld != field)
		    {
		      cnt--;
		      field = fld;
		      pos = int_byte_position (field);
		      val = build_zero_cst (TREE_TYPE (fld));
		      if (TREE_CODE (val) == CONSTRUCTOR)
			to_free = val;
		    }
		}

	      if (TREE_CODE (TREE_TYPE (field)) == ARRAY_TYPE
		  && TYPE_DOMAIN (TREE_TYPE (field))
		  && ! TYPE_MAX_VALUE (TYPE_DOMAIN (TREE_TYPE (field))))
		{
		  if (mask || off != -1)
		    return 0;
		  if (val == NULL_TREE)
		    continue;
		  if (TREE_CODE (TREE_TYPE (val)) != ARRAY_TYPE)
		    return 0;
		  fieldsize = int_size_in_bytes (TREE_TYPE (val));
		  if (fieldsize < 0
		      || (int) fieldsize != fieldsize
		      || (pos + fieldsize) > INT_MAX)
		    return 0;
		  if (pos + fieldsize > total_bytes)
		    {
		      if (ptr != NULL && total_bytes < len)
			memset (ptr + total_bytes, '\0',
				MIN (pos + fieldsize, len) - total_bytes);
		      total_bytes = pos + fieldsize;
		    }
		}
	      else
		{
		  if (DECL_SIZE_UNIT (field) == NULL_TREE
		      || !tree_fits_shwi_p (DECL_SIZE_UNIT (field)))
		    return 0;
		  fieldsize = tree_to_shwi (DECL_SIZE_UNIT (field));
		}
	      if (fieldsize == 0)
		continue;

	      /* Prepare to deal with integral bit-fields and filter out other
		 bit-fields that do not start and end on a byte boundary.  */
	      if (DECL_BIT_FIELD (field))
		{
		  if (!tree_fits_uhwi_p (DECL_FIELD_BIT_OFFSET (field)))
		    return 0;
		  bpos = tree_to_uhwi (DECL_FIELD_BIT_OFFSET (field));
		  if (INTEGRAL_TYPE_P (TREE_TYPE (field)))
		    {
		      bpos %= BITS_PER_UNIT;
		      fieldsize = TYPE_PRECISION (TREE_TYPE (field)) + bpos;
		      epos = fieldsize % BITS_PER_UNIT;
		      fieldsize += BITS_PER_UNIT - 1;
		      fieldsize /= BITS_PER_UNIT;
		    }
		  else if (bpos % BITS_PER_UNIT
			   || DECL_SIZE (field) == NULL_TREE
			   || !tree_fits_shwi_p (DECL_SIZE (field))
			   || tree_to_shwi (DECL_SIZE (field)) % BITS_PER_UNIT)
		    return 0;
		}

	      if (off != -1 && pos + fieldsize <= off)
		continue;

	      if (val == NULL_TREE)
		continue;

	      if (DECL_BIT_FIELD (field)
		  && INTEGRAL_TYPE_P (TREE_TYPE (field)))
		{
		  /* FIXME: Handle PDP endian.  */
		  if (BYTES_BIG_ENDIAN != WORDS_BIG_ENDIAN)
		    return 0;

		  if (TREE_CODE (val) == NON_LVALUE_EXPR)
		    val = TREE_OPERAND (val, 0);
		  if (TREE_CODE (val) != INTEGER_CST)
		    return 0;

		  tree repr = DECL_BIT_FIELD_REPRESENTATIVE (field);
		  tree repr_type = NULL_TREE;
		  HOST_WIDE_INT rpos = 0;
		  if (repr && INTEGRAL_TYPE_P (TREE_TYPE (repr)))
		    {
		      rpos = int_byte_position (repr);
		      repr_type = TREE_TYPE (repr);
		    }
		  else
		    {
		      repr_type = find_bitfield_repr_type (fieldsize, len);
		      if (repr_type == NULL_TREE)
			return 0;
		      HOST_WIDE_INT repr_size = int_size_in_bytes (repr_type);
		      gcc_assert (repr_size > 0 && repr_size <= len);
		      if (pos + repr_size <= o + len)
			rpos = pos;
		      else
			{
			  rpos = o + len - repr_size;
			  gcc_assert (rpos <= pos);
			}
		    }

		  if (rpos > pos)
		    return 0;
		  wide_int w = wi::to_wide (val, TYPE_PRECISION (repr_type));
		  int diff = (TYPE_PRECISION (repr_type)
			      - TYPE_PRECISION (TREE_TYPE (field)));
		  HOST_WIDE_INT bitoff = (pos - rpos) * BITS_PER_UNIT + bpos;
		  if (!BYTES_BIG_ENDIAN)
		    w = wi::lshift (w, bitoff);
		  else
		    w = wi::lshift (w, diff - bitoff);
		  val = wide_int_to_tree (repr_type, w);

		  unsigned char buf[MAX_BITSIZE_MODE_ANY_INT
				    / BITS_PER_UNIT + 1];
		  int l = native_encode_int (val, buf, sizeof buf, 0);
		  if (l * BITS_PER_UNIT != TYPE_PRECISION (repr_type))
		    return 0;

		  if (ptr == NULL)
		    continue;

		  /* If the bitfield does not start at byte boundary, handle
		     the partial byte at the start.  */
		  if (bpos
		      && (off == -1 || (pos >= off && len >= 1)))
		    {
		      if (!BYTES_BIG_ENDIAN)
			{
			  int msk = (1 << bpos) - 1;
			  buf[pos - rpos] &= ~msk;
			  buf[pos - rpos] |= ptr[pos - o] & msk;
			  if (mask)
			    {
			      if (fieldsize > 1 || epos == 0)
				mask[pos] &= msk;
			      else
				mask[pos] &= (msk | ~((1 << epos) - 1));
			    }
			}
		      else
			{
			  int msk = (1 << (BITS_PER_UNIT - bpos)) - 1;
			  buf[pos - rpos] &= msk;
			  buf[pos - rpos] |= ptr[pos - o] & ~msk;
			  if (mask)
			    {
			      if (fieldsize > 1 || epos == 0)
				mask[pos] &= ~msk;
			      else
				mask[pos] &= (~msk
					      | ((1 << (BITS_PER_UNIT - epos))
						 - 1));
			    }
			}
		    }
		  /* If the bitfield does not end at byte boundary, handle
		     the partial byte at the end.  */
		  if (epos
		      && (off == -1
			  || pos + fieldsize <= (HOST_WIDE_INT) off + len))
		    {
		      if (!BYTES_BIG_ENDIAN)
			{
			  int msk = (1 << epos) - 1;
			  buf[pos - rpos + fieldsize - 1] &= msk;
			  buf[pos - rpos + fieldsize - 1]
			    |= ptr[pos + fieldsize - 1 - o] & ~msk;
			  if (mask && (fieldsize > 1 || bpos == 0))
			    mask[pos + fieldsize - 1] &= ~msk;
			}
		       else
			{
			  int msk = (1 << (BITS_PER_UNIT - epos)) - 1;
			  buf[pos - rpos + fieldsize - 1] &= ~msk;
			  buf[pos - rpos + fieldsize - 1]
			    |= ptr[pos + fieldsize - 1 - o] & msk;
			  if (mask && (fieldsize > 1 || bpos == 0))
			    mask[pos + fieldsize - 1] &= msk;
			}
		    }
		  if (off == -1
		      || (pos >= off
			  && (pos + fieldsize <= (HOST_WIDE_INT) off + len)))
		    {
		      memcpy (ptr + pos - o, buf + (pos - rpos), fieldsize);
		      if (mask && (fieldsize > (bpos != 0) + (epos != 0)))
			memset (mask + pos + (bpos != 0), 0,
				fieldsize - (bpos != 0) - (epos != 0));
		    }
		  else
		    {
		      /* Partial overlap.  */
		      HOST_WIDE_INT fsz = fieldsize;
		      gcc_assert (mask == NULL);
		      if (pos < off)
			{
			  fsz -= (off - pos);
			  pos = off;
			}
		      if (pos + fsz > (HOST_WIDE_INT) off + len)
			fsz = (HOST_WIDE_INT) off + len - pos;
		      memcpy (ptr + pos - off, buf + (pos - rpos), fsz);
		    }
		  continue;
		}

	      if (off == -1
		  || (pos >= off
		      && (pos + fieldsize <= (HOST_WIDE_INT) off + len)))
		{
		  int fldsize = fieldsize;
		  if (off == -1)
		    {
		      tree fld = DECL_CHAIN (field);
		      while (fld)
			{
			  if (TREE_CODE (fld) == FIELD_DECL)
			    break;
			  fld = DECL_CHAIN (fld);
			}
		      if (fld == NULL_TREE)
			fldsize = len - pos;
		    }
		  r = native_encode_initializer (val, ptr ? ptr + pos - o
							  : NULL,
						 fldsize,
						 off == -1 ? -1 : 0,
						 mask ? mask + pos : NULL);
		  if (!r)
		    return 0;
		  if (off == -1
		      && fldsize != fieldsize
		      && r > fieldsize
		      && pos + r > total_bytes)
		    total_bytes = pos + r;
		}
	      else
		{
		  /* Partial overlap.  */
		  unsigned char *p = NULL;
		  int no = 0;
		  int l;
		  gcc_assert (mask == NULL);
		  if (pos >= off)
		    {
		      if (ptr)
			p = ptr + pos - off;
		      l = MIN ((HOST_WIDE_INT) off + len - pos,
				fieldsize);
		    }
		  else
		    {
		      p = ptr;
		      no = off - pos;
		      l = len;
		    }
		  if (!native_encode_initializer (val, p, l, no, NULL))
		    return 0;
		}
	    }
	  return MIN (total_bytes - off, len);
	}
      return 0;
    }
}


/* Subroutine of native_interpret_expr.  Interpret the contents of
   the buffer PTR of length LEN as an INTEGER_CST of type TYPE.
   If the buffer cannot be interpreted, return NULL_TREE.  */

static tree
native_interpret_int (tree type, const unsigned char *ptr, int len)
{
  int total_bytes;
  if (TREE_CODE (type) == BITINT_TYPE)
    {
      struct bitint_info info;
      bool ok = targetm.c.bitint_type_info (TYPE_PRECISION (type), &info);
      gcc_assert (ok);
      scalar_int_mode limb_mode = as_a <scalar_int_mode> (info.limb_mode);
      if (TYPE_PRECISION (type) > GET_MODE_PRECISION (limb_mode))
	{
	  total_bytes = tree_to_uhwi (TYPE_SIZE_UNIT (type));
	  /* More work is needed when adding _BitInt support to PDP endian
	     if limb is smaller than word, or if _BitInt limb ordering doesn't
	     match target endianity here.  */
	  gcc_checking_assert (info.big_endian == WORDS_BIG_ENDIAN
			       && (BYTES_BIG_ENDIAN == WORDS_BIG_ENDIAN
				   || (GET_MODE_SIZE (limb_mode)
				       >= UNITS_PER_WORD)));
	}
      else
	total_bytes = GET_MODE_SIZE (SCALAR_INT_TYPE_MODE (type));
    }
  else
    total_bytes = GET_MODE_SIZE (SCALAR_INT_TYPE_MODE (type));

  if (total_bytes > len)
    return NULL_TREE;

  wide_int result = wi::from_buffer (ptr, total_bytes);

  return wide_int_to_tree (type, result);
}


/* Subroutine of native_interpret_expr.  Interpret the contents of
   the buffer PTR of length LEN as a FIXED_CST of type TYPE.
   If the buffer cannot be interpreted, return NULL_TREE.  */

static tree
native_interpret_fixed (tree type, const unsigned char *ptr, int len)
{
  scalar_mode mode = SCALAR_TYPE_MODE (type);
  int total_bytes = GET_MODE_SIZE (mode);
  double_int result;
  FIXED_VALUE_TYPE fixed_value;

  if (total_bytes > len
      || total_bytes * BITS_PER_UNIT > HOST_BITS_PER_DOUBLE_INT)
    return NULL_TREE;

  result = double_int::from_buffer (ptr, total_bytes);
  fixed_value = fixed_from_double_int (result, mode);

  return build_fixed (type, fixed_value);
}


/* Subroutine of native_interpret_expr.  Interpret the contents of
   the buffer PTR of length LEN as a REAL_CST of type TYPE.
   If the buffer cannot be interpreted, return NULL_TREE.  */

tree
native_interpret_real (tree type, const unsigned char *ptr, int len)
{
  scalar_float_mode mode = SCALAR_FLOAT_TYPE_MODE (type);
  int total_bytes = GET_MODE_SIZE (mode);
  unsigned char value;
  /* There are always 32 bits in each long, no matter the size of
     the hosts long.  We handle floating point representations with
     up to 192 bits.  */
  REAL_VALUE_TYPE r;
  long tmp[6];

  if (total_bytes > len || total_bytes > 24)
    return NULL_TREE;
  int words = (32 / BITS_PER_UNIT) / UNITS_PER_WORD;

  memset (tmp, 0, sizeof (tmp));
  for (int bitpos = 0; bitpos < total_bytes * BITS_PER_UNIT;
       bitpos += BITS_PER_UNIT)
    {
      /* Both OFFSET and BYTE index within a long;
	 bitpos indexes the whole float.  */
      int offset, byte = (bitpos / BITS_PER_UNIT) & 3;
      if (UNITS_PER_WORD < 4)
	{
	  int word = byte / UNITS_PER_WORD;
	  if (WORDS_BIG_ENDIAN)
	    word = (words - 1) - word;
	  offset = word * UNITS_PER_WORD;
	  if (BYTES_BIG_ENDIAN)
	    offset += (UNITS_PER_WORD - 1) - (byte % UNITS_PER_WORD);
	  else
	    offset += byte % UNITS_PER_WORD;
	}
      else
	{
	  offset = byte;
	  if (BYTES_BIG_ENDIAN)
	    {
	      /* Reverse bytes within each long, or within the entire float
		 if it's smaller than a long (for HFmode).  */
	      offset = MIN (3, total_bytes - 1) - offset;
	      gcc_assert (offset >= 0);
	    }
	}
      value = ptr[offset + ((bitpos / BITS_PER_UNIT) & ~3)];

      tmp[bitpos / 32] |= (unsigned long)value << (bitpos & 31);
    }

  real_from_target (&r, tmp, mode);
  return build_real (type, r);
}


/* Subroutine of native_interpret_expr.  Interpret the contents of
   the buffer PTR of length LEN as a COMPLEX_CST of type TYPE.
   If the buffer cannot be interpreted, return NULL_TREE.  */

static tree
native_interpret_complex (tree type, const unsigned char *ptr, int len)
{
  tree etype, rpart, ipart;
  int size;

  etype = TREE_TYPE (type);
  size = GET_MODE_SIZE (SCALAR_TYPE_MODE (etype));
  if (size * 2 > len)
    return NULL_TREE;
  rpart = native_interpret_expr (etype, ptr, size);
  if (!rpart)
    return NULL_TREE;
  ipart = native_interpret_expr (etype, ptr+size, size);
  if (!ipart)
    return NULL_TREE;
  return build_complex (type, rpart, ipart);
}

/* Read a vector of type TYPE from the target memory image given by BYTES,
   which contains LEN bytes.  The vector is known to be encodable using
   NPATTERNS interleaved patterns with NELTS_PER_PATTERN elements each.

   Return the vector on success, otherwise return null.  */

static tree
native_interpret_vector_part (tree type, const unsigned char *bytes,
			      unsigned int len, unsigned int npatterns,
			      unsigned int nelts_per_pattern)
{
  tree elt_type = TREE_TYPE (type);
  if (VECTOR_BOOLEAN_TYPE_P (type)
      && TYPE_PRECISION (elt_type) <= BITS_PER_UNIT)
    {
      /* This is the only case in which elements can be smaller than a byte.
	 Element 0 is always in the lsb of the containing byte.  */
      unsigned int elt_bits = TYPE_PRECISION (elt_type);
      if (elt_bits * npatterns * nelts_per_pattern > len * BITS_PER_UNIT)
	return NULL_TREE;

      tree_vector_builder builder (type, npatterns, nelts_per_pattern);
      for (unsigned int i = 0; i < builder.encoded_nelts (); ++i)
	{
	  unsigned int bit_index = i * elt_bits;
	  unsigned int byte_index = bit_index / BITS_PER_UNIT;
	  unsigned int lsb = bit_index % BITS_PER_UNIT;
	  builder.quick_push (bytes[byte_index] & (1 << lsb)
			      ? build_all_ones_cst (elt_type)
			      : build_zero_cst (elt_type));
	}
      return builder.build ();
    }

  unsigned int elt_bytes = tree_to_uhwi (TYPE_SIZE_UNIT (elt_type));
  if (elt_bytes * npatterns * nelts_per_pattern > len)
    return NULL_TREE;

  tree_vector_builder builder (type, npatterns, nelts_per_pattern);
  for (unsigned int i = 0; i < builder.encoded_nelts (); ++i)
    {
      tree elt = native_interpret_expr (elt_type, bytes, elt_bytes);
      if (!elt)
	return NULL_TREE;
      builder.quick_push (elt);
      bytes += elt_bytes;
    }
  return builder.build ();
}

/* Subroutine of native_interpret_expr.  Interpret the contents of
   the buffer PTR of length LEN as a VECTOR_CST of type TYPE.
   If the buffer cannot be interpreted, return NULL_TREE.  */

static tree
native_interpret_vector (tree type, const unsigned char *ptr, unsigned int len)
{
  unsigned HOST_WIDE_INT size;

  if (!tree_to_poly_uint64 (TYPE_SIZE_UNIT (type)).is_constant (&size)
      || size > len)
    return NULL_TREE;

  unsigned HOST_WIDE_INT count = TYPE_VECTOR_SUBPARTS (type).to_constant ();
  return native_interpret_vector_part (type, ptr, len, count, 1);
}


/* Subroutine of fold_view_convert_expr.  Interpret the contents of
   the buffer PTR of length LEN as a constant of type TYPE.  For
   INTEGRAL_TYPE_P we return an INTEGER_CST, for SCALAR_FLOAT_TYPE_P
   we return a REAL_CST, etc...  If the buffer cannot be interpreted,
   return NULL_TREE.  */

tree
native_interpret_expr (tree type, const unsigned char *ptr, int len)
{
  switch (TREE_CODE (type))
    {
    case INTEGER_TYPE:
    case ENUMERAL_TYPE:
    case BOOLEAN_TYPE:
    case POINTER_TYPE:
    case REFERENCE_TYPE:
    case OFFSET_TYPE:
    case BITINT_TYPE:
      return native_interpret_int (type, ptr, len);

    case REAL_TYPE:
      if (tree ret = native_interpret_real (type, ptr, len))
	{
	  /* For floating point values in composite modes, punt if this
	     folding doesn't preserve bit representation.  As the mode doesn't
	     have fixed precision while GCC pretends it does, there could be
	     valid values that GCC can't really represent accurately.
	     See PR95450.  Even for other modes, e.g. x86 XFmode can have some
	     bit combinationations which GCC doesn't preserve.  */
	  unsigned char buf[24 * 2];
	  scalar_float_mode mode = SCALAR_FLOAT_TYPE_MODE (type);
	  int total_bytes = GET_MODE_SIZE (mode);
	  memcpy (buf + 24, ptr, total_bytes);
	  clear_type_padding_in_mask (type, buf + 24);
	  if (native_encode_expr (ret, buf, total_bytes, 0) != total_bytes
	      || memcmp (buf + 24, buf, total_bytes) != 0)
	    return NULL_TREE;
	  return ret;
	}
      return NULL_TREE;

    case FIXED_POINT_TYPE:
      return native_interpret_fixed (type, ptr, len);

    case COMPLEX_TYPE:
      return native_interpret_complex (type, ptr, len);

    case VECTOR_TYPE:
      return native_interpret_vector (type, ptr, len);

    default:
      return NULL_TREE;
    }
}

/* Returns true if we can interpret the contents of a native encoding
   as TYPE.  */

bool
can_native_interpret_type_p (tree type)
{
  switch (TREE_CODE (type))
    {
    case INTEGER_TYPE:
    case ENUMERAL_TYPE:
    case BOOLEAN_TYPE:
    case POINTER_TYPE:
    case REFERENCE_TYPE:
    case FIXED_POINT_TYPE:
    case REAL_TYPE:
    case COMPLEX_TYPE:
    case VECTOR_TYPE:
    case OFFSET_TYPE:
      return true;
    default:
      return false;
    }
}

/* Attempt to interpret aggregate of TYPE from bytes encoded in target
   byte order at PTR + OFF with LEN bytes.  Does not handle unions.  */

tree
native_interpret_aggregate (tree type, const unsigned char *ptr, int off,
			    int len)
{
  vec<constructor_elt, va_gc> *elts = NULL;
  if (TREE_CODE (type) == ARRAY_TYPE)
    {
      HOST_WIDE_INT eltsz = int_size_in_bytes (TREE_TYPE (type));
      if (eltsz < 0 || eltsz > len || TYPE_DOMAIN (type) == NULL_TREE)
	return NULL_TREE;

      HOST_WIDE_INT cnt = 0;
      if (TYPE_MAX_VALUE (TYPE_DOMAIN (type)))
	{
	  if (!tree_fits_shwi_p (TYPE_MAX_VALUE (TYPE_DOMAIN (type))))
	    return NULL_TREE;
	  cnt = tree_to_shwi (TYPE_MAX_VALUE (TYPE_DOMAIN (type))) + 1;
	}
      if (eltsz == 0)
	cnt = 0;
      HOST_WIDE_INT pos = 0;
      for (HOST_WIDE_INT i = 0; i < cnt; i++, pos += eltsz)
	{
	  tree v = NULL_TREE;
	  if (pos >= len || pos + eltsz > len)
	    return NULL_TREE;
	  if (can_native_interpret_type_p (TREE_TYPE (type)))
	    {
	      v = native_interpret_expr (TREE_TYPE (type),
					 ptr + off + pos, eltsz);
	      if (v == NULL_TREE)
		return NULL_TREE;
	    }
	  else if (TREE_CODE (TREE_TYPE (type)) == RECORD_TYPE
		   || TREE_CODE (TREE_TYPE (type)) == ARRAY_TYPE)
	    v = native_interpret_aggregate (TREE_TYPE (type), ptr, off + pos,
					    eltsz);
	  if (v == NULL_TREE)
	    return NULL_TREE;
	  CONSTRUCTOR_APPEND_ELT (elts, size_int (i), v);
	}
      return build_constructor (type, elts);
    }
  if (TREE_CODE (type) != RECORD_TYPE)
    return NULL_TREE;
  for (tree field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
    {
      if (TREE_CODE (field) != FIELD_DECL || DECL_PADDING_P (field)
	  || is_empty_type (TREE_TYPE (field)))
	continue;
      tree fld = field;
      HOST_WIDE_INT bitoff = 0, pos = 0, sz = 0;
      int diff = 0;
      tree v = NULL_TREE;
      if (DECL_BIT_FIELD (field))
	{
	  fld = DECL_BIT_FIELD_REPRESENTATIVE (field);
	  if (fld && INTEGRAL_TYPE_P (TREE_TYPE (fld)))
	    {
	      poly_int64 bitoffset;
	      poly_uint64 field_offset, fld_offset;
	      if (poly_int_tree_p (DECL_FIELD_OFFSET (field), &field_offset)
		  && poly_int_tree_p (DECL_FIELD_OFFSET (fld), &fld_offset))
		bitoffset = (field_offset - fld_offset) * BITS_PER_UNIT;
	      else
		bitoffset = 0;
	      bitoffset += (tree_to_uhwi (DECL_FIELD_BIT_OFFSET (field))
			    - tree_to_uhwi (DECL_FIELD_BIT_OFFSET (fld)));
	      diff = (TYPE_PRECISION (TREE_TYPE (fld))
		      - TYPE_PRECISION (TREE_TYPE (field)));
	      if (!bitoffset.is_constant (&bitoff)
		  || bitoff < 0
		  || bitoff > diff)
		return NULL_TREE;
	    }
	  else
	    {
	      if (!tree_fits_uhwi_p (DECL_FIELD_BIT_OFFSET (field)))
		return NULL_TREE;
	      int fieldsize = TYPE_PRECISION (TREE_TYPE (field));
	      int bpos = tree_to_uhwi (DECL_FIELD_BIT_OFFSET (field));
	      bpos %= BITS_PER_UNIT;
	      fieldsize += bpos;
	      fieldsize += BITS_PER_UNIT - 1;
	      fieldsize /= BITS_PER_UNIT;
	      tree repr_type = find_bitfield_repr_type (fieldsize, len);
	      if (repr_type == NULL_TREE)
		return NULL_TREE;
	      sz = int_size_in_bytes (repr_type);
	      if (sz < 0 || sz > len)
		return NULL_TREE;
	      pos = int_byte_position (field);
	      if (pos < 0 || pos > len || pos + fieldsize > len)
		return NULL_TREE;
	      HOST_WIDE_INT rpos;
	      if (pos + sz <= len)
		rpos = pos;
	      else
		{
		  rpos = len - sz;
		  gcc_assert (rpos <= pos);
		}
	      bitoff = (HOST_WIDE_INT) (pos - rpos) * BITS_PER_UNIT + bpos;
	      pos = rpos;
	      diff = (TYPE_PRECISION (repr_type)
		      - TYPE_PRECISION (TREE_TYPE (field)));
	      v = native_interpret_expr (repr_type, ptr + off + pos, sz);
	      if (v == NULL_TREE)
		return NULL_TREE;
	      fld = NULL_TREE;
	    }
	}

      if (fld)
	{
	  sz = int_size_in_bytes (TREE_TYPE (fld));
	  if (sz < 0 || sz > len)
	    return NULL_TREE;
	  tree byte_pos = byte_position (fld);
	  if (!tree_fits_shwi_p (byte_pos))
	    return NULL_TREE;
	  pos = tree_to_shwi (byte_pos);
	  if (pos < 0 || pos > len || pos + sz > len)
	    return NULL_TREE;
	}
      if (fld == NULL_TREE)
	/* Already handled above.  */;
      else if (can_native_interpret_type_p (TREE_TYPE (fld)))
	{
	  v = native_interpret_expr (TREE_TYPE (fld),
				     ptr + off + pos, sz);
	  if (v == NULL_TREE)
	    return NULL_TREE;
	}
      else if (TREE_CODE (TREE_TYPE (fld)) == RECORD_TYPE
	       || TREE_CODE (TREE_TYPE (fld)) == ARRAY_TYPE)
	v = native_interpret_aggregate (TREE_TYPE (fld), ptr, off + pos, sz);
      if (v == NULL_TREE)
	return NULL_TREE;
      if (fld != field)
	{
	  if (TREE_CODE (v) != INTEGER_CST)
	    return NULL_TREE;

	  /* FIXME: Figure out how to handle PDP endian bitfields.  */
	  if (BYTES_BIG_ENDIAN != WORDS_BIG_ENDIAN)
	    return NULL_TREE;
	  if (!BYTES_BIG_ENDIAN)
	    v = wide_int_to_tree (TREE_TYPE (field),
				  wi::lrshift (wi::to_wide (v), bitoff));
	  else
	    v = wide_int_to_tree (TREE_TYPE (field),
				  wi::lrshift (wi::to_wide (v),
					       diff - bitoff));
	}
      CONSTRUCTOR_APPEND_ELT (elts, field, v);
    }
  return build_constructor (type, elts);
}

/* Routines for manipulation of native_encode_expr encoded data if the encoded
   or extracted constant positions and/or sizes aren't byte aligned.  */

/* Shift left the bytes in PTR of SZ elements by AMNT bits, carrying over the
   bits between adjacent elements.  AMNT should be within
   [0, BITS_PER_UNIT).
   Example, AMNT = 2:
   00011111|11100000 << 2 = 01111111|10000000
   PTR[1]  | PTR[0]         PTR[1]  | PTR[0].  */

void
shift_bytes_in_array_left (unsigned char *ptr, unsigned int sz,
			   unsigned int amnt)
{
  if (amnt == 0)
    return;

  unsigned char carry_over = 0U;
  unsigned char carry_mask = (~0U) << (unsigned char) (BITS_PER_UNIT - amnt);
  unsigned char clear_mask = (~0U) << amnt;

  for (unsigned int i = 0; i < sz; i++)
    {
      unsigned prev_carry_over = carry_over;
      carry_over = (ptr[i] & carry_mask) >> (BITS_PER_UNIT - amnt);

      ptr[i] <<= amnt;
      if (i != 0)
	{
	  ptr[i] &= clear_mask;
	  ptr[i] |= prev_carry_over;
	}
    }
}

/* Like shift_bytes_in_array_left but for big-endian.
   Shift right the bytes in PTR of SZ elements by AMNT bits, carrying over the
   bits between adjacent elements.  AMNT should be within
   [0, BITS_PER_UNIT).
   Example, AMNT = 2:
   00011111|11100000 >> 2 = 00000111|11111000
   PTR[0]  | PTR[1]         PTR[0]  | PTR[1].  */

void
shift_bytes_in_array_right (unsigned char *ptr, unsigned int sz,
			    unsigned int amnt)
{
  if (amnt == 0)
    return;

  unsigned char carry_over = 0U;
  unsigned char carry_mask = ~(~0U << amnt);

  for (unsigned int i = 0; i < sz; i++)
    {
      unsigned prev_carry_over = carry_over;
      carry_over = ptr[i] & carry_mask;

      carry_over <<= (unsigned char) BITS_PER_UNIT - amnt;
      ptr[i] >>= amnt;
      ptr[i] |= prev_carry_over;
    }
}

/* Try to view-convert VECTOR_CST EXPR to VECTOR_TYPE TYPE by operating
   directly on the VECTOR_CST encoding, in a way that works for variable-
   length vectors.  Return the resulting VECTOR_CST on success or null
   on failure.  */

static tree
fold_view_convert_vector_encoding (tree type, tree expr)
{
  tree expr_type = TREE_TYPE (expr);
  poly_uint64 type_bits, expr_bits;
  if (!poly_int_tree_p (TYPE_SIZE (type), &type_bits)
      || !poly_int_tree_p (TYPE_SIZE (expr_type), &expr_bits))
    return NULL_TREE;

  poly_uint64 type_units = TYPE_VECTOR_SUBPARTS (type);
  poly_uint64 expr_units = TYPE_VECTOR_SUBPARTS (expr_type);
  unsigned int type_elt_bits = vector_element_size (type_bits, type_units);
  unsigned int expr_elt_bits = vector_element_size (expr_bits, expr_units);

  /* We can only preserve the semantics of a stepped pattern if the new
     vector element is an integer of the same size.  */
  if (VECTOR_CST_STEPPED_P (expr)
      && (!INTEGRAL_TYPE_P (type) || type_elt_bits != expr_elt_bits))
    return NULL_TREE;

  /* The number of bits needed to encode one element from every pattern
     of the original vector.  */
  unsigned int expr_sequence_bits
    = VECTOR_CST_NPATTERNS (expr) * expr_elt_bits;

  /* The number of bits needed to encode one element from every pattern
     of the result.  */
  unsigned int type_sequence_bits
    = least_common_multiple (expr_sequence_bits, type_elt_bits);

  /* Don't try to read more bytes than are available, which can happen
     for constant-sized vectors if TYPE has larger elements than EXPR_TYPE.
     The general VIEW_CONVERT handling can cope with that case, so there's
     no point complicating things here.  */
  unsigned int nelts_per_pattern = VECTOR_CST_NELTS_PER_PATTERN (expr);
  unsigned int buffer_bytes = CEIL (nelts_per_pattern * type_sequence_bits,
				    BITS_PER_UNIT);
  unsigned int buffer_bits = buffer_bytes * BITS_PER_UNIT;
  if (known_gt (buffer_bits, expr_bits))
    return NULL_TREE;

  /* Get enough bytes of EXPR to form the new encoding.  */
  auto_vec<unsigned char, 128> buffer (buffer_bytes);
  buffer.quick_grow (buffer_bytes);
  if (native_encode_vector_part (expr, buffer.address (), buffer_bytes, 0,
				 buffer_bits / expr_elt_bits)
      != (int) buffer_bytes)
    return NULL_TREE;

  /* Reencode the bytes as TYPE.  */
  unsigned int type_npatterns = type_sequence_bits / type_elt_bits;
  return native_interpret_vector_part (type, &buffer[0], buffer.length (),
				       type_npatterns, nelts_per_pattern);
}

/* Fold a VIEW_CONVERT_EXPR of a constant expression EXPR to type
   TYPE at compile-time.  If we're unable to perform the conversion
   return NULL_TREE.  */

static tree
fold_view_convert_expr (tree type, tree expr)
{
  unsigned char buffer[128];
  unsigned char *buf;
  int len;
  HOST_WIDE_INT l;

  /* Check that the host and target are sane.  */
  if (CHAR_BIT != 8 || BITS_PER_UNIT != 8)
    return NULL_TREE;

  if (VECTOR_TYPE_P (type) && TREE_CODE (expr) == VECTOR_CST)
    if (tree res = fold_view_convert_vector_encoding (type, expr))
      return res;

  l = int_size_in_bytes (type);
  if (l > (int) sizeof (buffer)
      && l <= WIDE_INT_MAX_PRECISION / BITS_PER_UNIT)
    {
      buf = XALLOCAVEC (unsigned char, l);
      len = l;
    }
  else
    {
      buf = buffer;
      len = sizeof (buffer);
    }
  len = native_encode_expr (expr, buf, len);
  if (len == 0)
    return NULL_TREE;

  return native_interpret_expr (type, buf, len);
}

/* Build an expression for the address of T.  Folds away INDIRECT_REF
   to avoid confusing the gimplify process.  */

tree
build_fold_addr_expr_with_type_loc (location_t loc, tree t, tree ptrtype)
{
  /* The size of the object is not relevant when talking about its address.  */
  if (TREE_CODE (t) == WITH_SIZE_EXPR)
    t = TREE_OPERAND (t, 0);

  if (INDIRECT_REF_P (t))
    {
      t = TREE_OPERAND (t, 0);

      if (TREE_TYPE (t) != ptrtype)
	t = build1_loc (loc, NOP_EXPR, ptrtype, t);
    }
  else if (TREE_CODE (t) == MEM_REF
	   && integer_zerop (TREE_OPERAND (t, 1)))
    {
      t = TREE_OPERAND (t, 0);

      if (TREE_TYPE (t) != ptrtype)
	t = fold_convert_loc (loc, ptrtype, t);
    }
  else if (TREE_CODE (t) == MEM_REF
	   && TREE_CODE (TREE_OPERAND (t, 0)) == INTEGER_CST)
    return fold_binary (POINTER_PLUS_EXPR, ptrtype,
			TREE_OPERAND (t, 0),
			convert_to_ptrofftype (TREE_OPERAND (t, 1)));
  else if (TREE_CODE (t) == VIEW_CONVERT_EXPR)
    {
      t = build_fold_addr_expr_loc (loc, TREE_OPERAND (t, 0));

      if (TREE_TYPE (t) != ptrtype)
	t = fold_convert_loc (loc, ptrtype, t);
    }
  else
    t = build1_loc (loc, ADDR_EXPR, ptrtype, t);

  return t;
}

/* Build an expression for the address of T.  */

tree
build_fold_addr_expr_loc (location_t loc, tree t)
{
  tree ptrtype = build_pointer_type (TREE_TYPE (t));

  return build_fold_addr_expr_with_type_loc (loc, t, ptrtype);
}

/* Fold a unary expression of code CODE and type TYPE with operand
   OP0.  Return the folded expression if folding is successful.
   Otherwise, return NULL_TREE.  */

tree
fold_unary_loc (location_t loc, enum tree_code code, tree type, tree op0)
{
  tree tem;
  tree arg0;
  enum tree_code_class kind = TREE_CODE_CLASS (code);

  gcc_assert (IS_EXPR_CODE_CLASS (kind)
	      && TREE_CODE_LENGTH (code) == 1);

  arg0 = op0;
  if (arg0)
    {
      if (CONVERT_EXPR_CODE_P (code)
	  || code == FLOAT_EXPR || code == ABS_EXPR || code == NEGATE_EXPR)
	{
	  /* Don't use STRIP_NOPS, because signedness of argument type
	     matters.  */
	  STRIP_SIGN_NOPS (arg0);
	}
      else
	{
	  /* Strip any conversions that don't change the mode.  This
	     is safe for every expression, except for a comparison
	     expression because its signedness is derived from its
	     operands.

	     Note that this is done as an internal manipulation within
	     the constant folder, in order to find the simplest
	     representation of the arguments so that their form can be
	     studied.  In any cases, the appropriate type conversions
	     should be put back in the tree that will get out of the
	     constant folder.  */
	  STRIP_NOPS (arg0);
	}

      if (CONSTANT_CLASS_P (arg0))
	{
	  tree tem = const_unop (code, type, arg0);
	  if (tem)
	    {
	      if (TREE_TYPE (tem) != type)
		tem = fold_convert_loc (loc, type, tem);
	      return tem;
	    }
	}
    }

  tem = generic_simplify (loc, code, type, op0);
  if (tem)
    return tem;

  if (TREE_CODE_CLASS (code) == tcc_unary)
    {
      if (TREE_CODE (arg0) == COMPOUND_EXPR)
	return build2 (COMPOUND_EXPR, type, TREE_OPERAND (arg0, 0),
		       fold_build1_loc (loc, code, type,
				    fold_convert_loc (loc, TREE_TYPE (op0),
						      TREE_OPERAND (arg0, 1))));
      else if (TREE_CODE (arg0) == COND_EXPR)
	{
	  tree arg01 = TREE_OPERAND (arg0, 1);
	  tree arg02 = TREE_OPERAND (arg0, 2);
	  if (! VOID_TYPE_P (TREE_TYPE (arg01)))
	    arg01 = fold_build1_loc (loc, code, type,
				 fold_convert_loc (loc,
						   TREE_TYPE (op0), arg01));
	  if (! VOID_TYPE_P (TREE_TYPE (arg02)))
	    arg02 = fold_build1_loc (loc, code, type,
				 fold_convert_loc (loc,
						   TREE_TYPE (op0), arg02));
	  tem = fold_build3_loc (loc, COND_EXPR, type, TREE_OPERAND (arg0, 0),
			     arg01, arg02);

	  /* If this was a conversion, and all we did was to move into
	     inside the COND_EXPR, bring it back out.  But leave it if
	     it is a conversion from integer to integer and the
	     result precision is no wider than a word since such a
	     conversion is cheap and may be optimized away by combine,
	     while it couldn't if it were outside the COND_EXPR.  Then return
	     so we don't get into an infinite recursion loop taking the
	     conversion out and then back in.  */

	  if ((CONVERT_EXPR_CODE_P (code)
	       || code == NON_LVALUE_EXPR)
	      && TREE_CODE (tem) == COND_EXPR
	      && TREE_CODE (TREE_OPERAND (tem, 1)) == code
	      && TREE_CODE (TREE_OPERAND (tem, 2)) == code
	      && ! VOID_TYPE_P (TREE_TYPE (TREE_OPERAND (tem, 1)))
	      && ! VOID_TYPE_P (TREE_TYPE (TREE_OPERAND (tem, 2)))
	      && (TREE_TYPE (TREE_OPERAND (TREE_OPERAND (tem, 1), 0))
		  == TREE_TYPE (TREE_OPERAND (TREE_OPERAND (tem, 2), 0)))
	      && (! (INTEGRAL_TYPE_P (TREE_TYPE (tem))
		     && (INTEGRAL_TYPE_P
			 (TREE_TYPE (TREE_OPERAND (TREE_OPERAND (tem, 1), 0))))
		     && TYPE_PRECISION (TREE_TYPE (tem)) <= BITS_PER_WORD)
		  || flag_syntax_only))
	    tem = build1_loc (loc, code, type,
			      build3 (COND_EXPR,
				      TREE_TYPE (TREE_OPERAND
						 (TREE_OPERAND (tem, 1), 0)),
				      TREE_OPERAND (tem, 0),
				      TREE_OPERAND (TREE_OPERAND (tem, 1), 0),
				      TREE_OPERAND (TREE_OPERAND (tem, 2),
						    0)));
	  return tem;
	}
   }

  switch (code)
    {
    case NON_LVALUE_EXPR:
      if (!maybe_lvalue_p (op0))
	return fold_convert_loc (loc, type, op0);
      return NULL_TREE;

    CASE_CONVERT:
    case FLOAT_EXPR:
    case FIX_TRUNC_EXPR:
      if (COMPARISON_CLASS_P (op0))
	{
	  /* If we have (type) (a CMP b) and type is an integral type, return
	     new expression involving the new type.  Canonicalize
	     (type) (a CMP b) to (a CMP b) ? (type) true : (type) false for
	     non-integral type.
	     Do not fold the result as that would not simplify further, also
	     folding again results in recursions.  */
	  if (TREE_CODE (type) == BOOLEAN_TYPE)
	    return build2_loc (loc, TREE_CODE (op0), type,
			       TREE_OPERAND (op0, 0),
			       TREE_OPERAND (op0, 1));
	  else if (!INTEGRAL_TYPE_P (type) && !VOID_TYPE_P (type)
		   && TREE_CODE (type) != VECTOR_TYPE)
	    return build3_loc (loc, COND_EXPR, type, op0,
			       constant_boolean_node (true, type),
			       constant_boolean_node (false, type));
	}

      /* Handle (T *)&A.B.C for A being of type T and B and C
	 living at offset zero.  This occurs frequently in
	 C++ upcasting and then accessing the base.  */
      if (TREE_CODE (op0) == ADDR_EXPR
	  && POINTER_TYPE_P (type)
	  && handled_component_p (TREE_OPERAND (op0, 0)))
        {
	  poly_int64 bitsize, bitpos;
	  tree offset;
	  machine_mode mode;
	  int unsignedp, reversep, volatilep;
	  tree base
	    = get_inner_reference (TREE_OPERAND (op0, 0), &bitsize, &bitpos,
				   &offset, &mode, &unsignedp, &reversep,
				   &volatilep);
	  /* If the reference was to a (constant) zero offset, we can use
	     the address of the base if it has the same base type
	     as the result type and the pointer type is unqualified.  */
	  if (!offset
	      && known_eq (bitpos, 0)
	      && (TYPE_MAIN_VARIANT (TREE_TYPE (type))
		  == TYPE_MAIN_VARIANT (TREE_TYPE (base)))
	      && TYPE_QUALS (type) == TYPE_UNQUALIFIED)
	    return fold_convert_loc (loc, type,
				     build_fold_addr_expr_loc (loc, base));
        }

      if (TREE_CODE (op0) == MODIFY_EXPR
	  && TREE_CONSTANT (TREE_OPERAND (op0, 1))
	  /* Detect assigning a bitfield.  */
	  && !(TREE_CODE (TREE_OPERAND (op0, 0)) == COMPONENT_REF
	       && DECL_BIT_FIELD
	       (TREE_OPERAND (TREE_OPERAND (op0, 0), 1))))
	{
	  /* Don't leave an assignment inside a conversion
	     unless assigning a bitfield.  */
	  tem = fold_build1_loc (loc, code, type, TREE_OPERAND (op0, 1));
	  /* First do the assignment, then return converted constant.  */
	  tem = build2_loc (loc, COMPOUND_EXPR, TREE_TYPE (tem), op0, tem);
	  suppress_warning (tem /* What warning? */);
	  TREE_USED (tem) = 1;
	  return tem;
	}

      /* Convert (T)(x & c) into (T)x & (T)c, if c is an integer
	 constants (if x has signed type, the sign bit cannot be set
	 in c).  This folds extension into the BIT_AND_EXPR.
	 ??? We don't do it for BOOLEAN_TYPE or ENUMERAL_TYPE because they
	 very likely don't have maximal range for their precision and this
	 transformation effectively doesn't preserve non-maximal ranges.  */
      if (TREE_CODE (type) == INTEGER_TYPE
	  && TREE_CODE (op0) == BIT_AND_EXPR
	  && TREE_CODE (TREE_OPERAND (op0, 1)) == INTEGER_CST)
	{
	  tree and_expr = op0;
	  tree and0 = TREE_OPERAND (and_expr, 0);
	  tree and1 = TREE_OPERAND (and_expr, 1);
	  int change = 0;

	  if (TYPE_UNSIGNED (TREE_TYPE (and_expr))
	      || (TYPE_PRECISION (type)
		  <= TYPE_PRECISION (TREE_TYPE (and_expr))))
	    change = 1;
	  else if (TYPE_PRECISION (TREE_TYPE (and1))
		   <= HOST_BITS_PER_WIDE_INT
		   && tree_fits_uhwi_p (and1))
	    {
	      unsigned HOST_WIDE_INT cst;

	      cst = tree_to_uhwi (and1);
	      cst &= HOST_WIDE_INT_M1U
		     << (TYPE_PRECISION (TREE_TYPE (and1)) - 1);
	      change = (cst == 0);
	      if (change
		  && !flag_syntax_only
		  && (load_extend_op (TYPE_MODE (TREE_TYPE (and0)))
		      == ZERO_EXTEND))
		{
		  tree uns = unsigned_type_for (TREE_TYPE (and0));
		  and0 = fold_convert_loc (loc, uns, and0);
		  and1 = fold_convert_loc (loc, uns, and1);
		}
	    }
	  if (change)
	    {
	      tree and1_type = TREE_TYPE (and1);
	      unsigned prec = MAX (TYPE_PRECISION (and1_type),
				   TYPE_PRECISION (type));
	      tem = force_fit_type (type,
				    wide_int::from (wi::to_wide (and1), prec,
						    TYPE_SIGN (and1_type)),
				    0, TREE_OVERFLOW (and1));
	      return fold_build2_loc (loc, BIT_AND_EXPR, type,
				      fold_convert_loc (loc, type, and0), tem);
	    }
	}

      /* Convert (T1)(X p+ Y) into ((T1)X p+ Y), for pointer type, when the new
	 cast (T1)X will fold away.  We assume that this happens when X itself
	 is a cast.  */
      if (POINTER_TYPE_P (type)
	  && TREE_CODE (arg0) == POINTER_PLUS_EXPR
	  && CONVERT_EXPR_P (TREE_OPERAND (arg0, 0)))
	{
	  tree arg00 = TREE_OPERAND (arg0, 0);
	  tree arg01 = TREE_OPERAND (arg0, 1);

	  /* If -fsanitize=alignment, avoid this optimization in GENERIC
	     when the pointed type needs higher alignment than
	     the p+ first operand's pointed type.  */
	  if (!in_gimple_form
	      && sanitize_flags_p (SANITIZE_ALIGNMENT)
	      && (min_align_of_type (TREE_TYPE (type))
		  > min_align_of_type (TREE_TYPE (TREE_TYPE (arg00)))))
	    return NULL_TREE;

	  /* Similarly, avoid this optimization in GENERIC for -fsanitize=null
	     when type is a reference type and arg00's type is not,
	     because arg00 could be validly nullptr and if arg01 doesn't return,
	     we don't want false positive binding of reference to nullptr.  */
	  if (TREE_CODE (type) == REFERENCE_TYPE
	      && !in_gimple_form
	      && sanitize_flags_p (SANITIZE_NULL)
	      && TREE_CODE (TREE_TYPE (arg00)) != REFERENCE_TYPE)
	    return NULL_TREE;

	  arg00 = fold_convert_loc (loc, type, arg00);
	  return fold_build_pointer_plus_loc (loc, arg00, arg01);
	}

      /* Convert (T1)(~(T2)X) into ~(T1)X if T1 and T2 are integral types
	 of the same precision, and X is an integer type not narrower than
	 types T1 or T2, i.e. the cast (T2)X isn't an extension.  */
      if (INTEGRAL_TYPE_P (type)
	  && TREE_CODE (op0) == BIT_NOT_EXPR
	  && INTEGRAL_TYPE_P (TREE_TYPE (op0))
	  && CONVERT_EXPR_P (TREE_OPERAND (op0, 0))
	  && TYPE_PRECISION (type) == TYPE_PRECISION (TREE_TYPE (op0)))
	{
	  tem = TREE_OPERAND (TREE_OPERAND (op0, 0), 0);
	  if (INTEGRAL_TYPE_P (TREE_TYPE (tem))
	      && TYPE_PRECISION (type) <= TYPE_PRECISION (TREE_TYPE (tem)))
	    return fold_build1_loc (loc, BIT_NOT_EXPR, type,
				fold_convert_loc (loc, type, tem));
	}

      /* Convert (T1)(X * Y) into (T1)X * (T1)Y if T1 is narrower than the
	 type of X and Y (integer types only).  */
      if (INTEGRAL_TYPE_P (type)
	  && TREE_CODE (op0) == MULT_EXPR
	  && INTEGRAL_TYPE_P (TREE_TYPE (op0))
	  && TYPE_PRECISION (type) < TYPE_PRECISION (TREE_TYPE (op0))
	  && (TYPE_OVERFLOW_WRAPS (TREE_TYPE (op0))
	      || !sanitize_flags_p (SANITIZE_SI_OVERFLOW)))
	{
	  /* Be careful not to introduce new overflows.  */
	  tree mult_type;
          if (TYPE_OVERFLOW_WRAPS (type))
	    mult_type = type;
	  else
	    mult_type = unsigned_type_for (type);

	  if (TYPE_PRECISION (mult_type) < TYPE_PRECISION (TREE_TYPE (op0)))
	    {
	      tem = fold_build2_loc (loc, MULT_EXPR, mult_type,
				 fold_convert_loc (loc, mult_type,
						   TREE_OPERAND (op0, 0)),
				 fold_convert_loc (loc, mult_type,
						   TREE_OPERAND (op0, 1)));
	      return fold_convert_loc (loc, type, tem);
	    }
	}

      return NULL_TREE;

    case VIEW_CONVERT_EXPR:
      if (TREE_CODE (op0) == MEM_REF)
        {
	  if (TYPE_ALIGN (TREE_TYPE (op0)) != TYPE_ALIGN (type))
	    type = build_aligned_type (type, TYPE_ALIGN (TREE_TYPE (op0)));
	  tem = fold_build2_loc (loc, MEM_REF, type,
				 TREE_OPERAND (op0, 0), TREE_OPERAND (op0, 1));
	  REF_REVERSE_STORAGE_ORDER (tem) = REF_REVERSE_STORAGE_ORDER (op0);
	  return tem;
	}

      return NULL_TREE;

    case NEGATE_EXPR:
      tem = fold_negate_expr (loc, arg0);
      if (tem)
	return fold_convert_loc (loc, type, tem);
      return NULL_TREE;

    case ABS_EXPR:
      /* Convert fabs((double)float) into (double)fabsf(float).  */
      if (TREE_CODE (arg0) == NOP_EXPR
	  && TREE_CODE (type) == REAL_TYPE)
	{
	  tree targ0 = strip_float_extensions (arg0);
	  if (targ0 != arg0)
	    return fold_convert_loc (loc, type,
				     fold_build1_loc (loc, ABS_EXPR,
						  TREE_TYPE (targ0),
						  targ0));
	}
      return NULL_TREE;

    case BIT_NOT_EXPR:
      /* Convert ~(X ^ Y) to ~X ^ Y or X ^ ~Y if ~X or ~Y simplify.  */
      if (TREE_CODE (arg0) == BIT_XOR_EXPR
	  && (tem = fold_unary_loc (loc, BIT_NOT_EXPR, type,
				    fold_convert_loc (loc, type,
						      TREE_OPERAND (arg0, 0)))))
	return fold_build2_loc (loc, BIT_XOR_EXPR, type, tem,
				fold_convert_loc (loc, type,
						  TREE_OPERAND (arg0, 1)));
      else if (TREE_CODE (arg0) == BIT_XOR_EXPR
	       && (tem = fold_unary_loc (loc, BIT_NOT_EXPR, type,
			       	     fold_convert_loc (loc, type,
						       TREE_OPERAND (arg0, 1)))))
	return fold_build2_loc (loc, BIT_XOR_EXPR, type,
			    fold_convert_loc (loc, type,
					      TREE_OPERAND (arg0, 0)), tem);

      return NULL_TREE;

    case TRUTH_NOT_EXPR:
      /* Note that the operand of this must be an int
	 and its values must be 0 or 1.
	 ("true" is a fixed value perhaps depending on the language,
	 but we don't handle values other than 1 correctly yet.)  */
      tem = fold_truth_not_expr (loc, arg0);
      if (!tem)
	return NULL_TREE;
      return fold_convert_loc (loc, type, tem);

    case INDIRECT_REF:
      /* Fold *&X to X if X is an lvalue.  */
      if (TREE_CODE (op0) == ADDR_EXPR)
	{
	  tree op00 = TREE_OPERAND (op0, 0);
	  if ((VAR_P (op00)
	       || TREE_CODE (op00) == PARM_DECL
	       || TREE_CODE (op00) == RESULT_DECL)
	      && !TREE_READONLY (op00))
	    return op00;
	}
      return NULL_TREE;

    default:
      return NULL_TREE;
    } /* switch (code) */
}


/* If the operation was a conversion do _not_ mark a resulting constant
   with TREE_OVERFLOW if the original constant was not.  These conversions
   have implementation defined behavior and retaining the TREE_OVERFLOW
   flag here would confuse later passes such as VRP.  */
tree
fold_unary_ignore_overflow_loc (location_t loc, enum tree_code code,
				tree type, tree op0)
{
  tree res = fold_unary_loc (loc, code, type, op0);
  if (res
      && TREE_CODE (res) == INTEGER_CST
      && TREE_CODE (op0) == INTEGER_CST
      && CONVERT_EXPR_CODE_P (code))
    TREE_OVERFLOW (res) = TREE_OVERFLOW (op0);

  return res;
}

/* Fold a binary bitwise/truth expression of code CODE and type TYPE with
   operands OP0 and OP1.  LOC is the location of the resulting expression.
   ARG0 and ARG1 are the NOP_STRIPed results of OP0 and OP1.
   Return the folded expression if folding is successful.  Otherwise,
   return NULL_TREE.  */
static tree
fold_truth_andor (location_t loc, enum tree_code code, tree type,
		  tree arg0, tree arg1, tree op0, tree op1)
{
  tree tem;

  /* We only do these simplifications if we are optimizing.  */
  if (!optimize)
    return NULL_TREE;

  /* Check for things like (A || B) && (A || C).  We can convert this
     to A || (B && C).  Note that either operator can be any of the four
     truth and/or operations and the transformation will still be
     valid.   Also note that we only care about order for the
     ANDIF and ORIF operators.  If B contains side effects, this
     might change the truth-value of A.  */
  if (TREE_CODE (arg0) == TREE_CODE (arg1)
      && (TREE_CODE (arg0) == TRUTH_ANDIF_EXPR
	  || TREE_CODE (arg0) == TRUTH_ORIF_EXPR
	  || TREE_CODE (arg0) == TRUTH_AND_EXPR
	  || TREE_CODE (arg0) == TRUTH_OR_EXPR)
      && ! TREE_SIDE_EFFECTS (TREE_OPERAND (arg0, 1)))
    {
      tree a00 = TREE_OPERAND (arg0, 0);
      tree a01 = TREE_OPERAND (arg0, 1);
      tree a10 = TREE_OPERAND (arg1, 0);
      tree a11 = TREE_OPERAND (arg1, 1);
      bool commutative = ((TREE_CODE (arg0) == TRUTH_OR_EXPR
			   || TREE_CODE (arg0) == TRUTH_AND_EXPR)
			  && (code == TRUTH_AND_EXPR
			      || code == TRUTH_OR_EXPR));

      if (operand_equal_p (a00, a10, 0))
	return fold_build2_loc (loc, TREE_CODE (arg0), type, a00,
			    fold_build2_loc (loc, code, type, a01, a11));
      else if (commutative && operand_equal_p (a00, a11, 0))
	return fold_build2_loc (loc, TREE_CODE (arg0), type, a00,
			    fold_build2_loc (loc, code, type, a01, a10));
      else if (commutative && operand_equal_p (a01, a10, 0))
	return fold_build2_loc (loc, TREE_CODE (arg0), type, a01,
			    fold_build2_loc (loc, code, type, a00, a11));

      /* This case if tricky because we must either have commutative
	 operators or else A10 must not have side-effects.  */

      else if ((commutative || ! TREE_SIDE_EFFECTS (a10))
	       && operand_equal_p (a01, a11, 0))
	return fold_build2_loc (loc, TREE_CODE (arg0), type,
			    fold_build2_loc (loc, code, type, a00, a10),
			    a01);
    }

  /* See if we can build a range comparison.  */
  if ((tem = fold_range_test (loc, code, type, op0, op1)) != 0)
    return tem;

  if ((code == TRUTH_ANDIF_EXPR && TREE_CODE (arg0) == TRUTH_ORIF_EXPR)
      || (code == TRUTH_ORIF_EXPR && TREE_CODE (arg0) == TRUTH_ANDIF_EXPR))
    {
      tem = merge_truthop_with_opposite_arm (loc, arg0, arg1, true);
      if (tem)
	return fold_build2_loc (loc, code, type, tem, arg1);
    }

  if ((code == TRUTH_ANDIF_EXPR && TREE_CODE (arg1) == TRUTH_ORIF_EXPR)
      || (code == TRUTH_ORIF_EXPR && TREE_CODE (arg1) == TRUTH_ANDIF_EXPR))
    {
      tem = merge_truthop_with_opposite_arm (loc, arg1, arg0, false);
      if (tem)
	return fold_build2_loc (loc, code, type, arg0, tem);
    }

  /* Check for the possibility of merging component references.  If our
     lhs is another similar operation, try to merge its rhs with our
     rhs.  Then try to merge our lhs and rhs.  */
  if (TREE_CODE (arg0) == code
      && (tem = fold_truth_andor_1 (loc, code, type,
				    TREE_OPERAND (arg0, 1), arg1)) != 0)
    return fold_build2_loc (loc, code, type, TREE_OPERAND (arg0, 0), tem);

  if ((tem = fold_truth_andor_1 (loc, code, type, arg0, arg1)) != 0)
    return tem;

  bool logical_op_non_short_circuit = LOGICAL_OP_NON_SHORT_CIRCUIT;
  if (param_logical_op_non_short_circuit != -1)
    logical_op_non_short_circuit
      = param_logical_op_non_short_circuit;
  if (logical_op_non_short_circuit
      && !sanitize_coverage_p ()
      && (code == TRUTH_AND_EXPR
          || code == TRUTH_ANDIF_EXPR
          || code == TRUTH_OR_EXPR
          || code == TRUTH_ORIF_EXPR))
    {
      enum tree_code ncode, icode;

      ncode = (code == TRUTH_ANDIF_EXPR || code == TRUTH_AND_EXPR)
	      ? TRUTH_AND_EXPR : TRUTH_OR_EXPR;
      icode = ncode == TRUTH_AND_EXPR ? TRUTH_ANDIF_EXPR : TRUTH_ORIF_EXPR;

      /* Transform ((A AND-IF B) AND[-IF] C) into (A AND-IF (B AND C)),
	 or ((A OR-IF B) OR[-IF] C) into (A OR-IF (B OR C))
	 We don't want to pack more than two leafs to a non-IF AND/OR
	 expression.
	 If tree-code of left-hand operand isn't an AND/OR-IF code and not
	 equal to IF-CODE, then we don't want to add right-hand operand.
	 If the inner right-hand side of left-hand operand has
	 side-effects, or isn't simple, then we can't add to it,
	 as otherwise we might destroy if-sequence.  */
      if (TREE_CODE (arg0) == icode
	  && simple_condition_p (arg1)
	  /* Needed for sequence points to handle trappings, and
	     side-effects.  */
	  && simple_condition_p (TREE_OPERAND (arg0, 1)))
	{
	  tem = fold_build2_loc (loc, ncode, type, TREE_OPERAND (arg0, 1),
				 arg1);
	  return fold_build2_loc (loc, icode, type, TREE_OPERAND (arg0, 0),
				  tem);
	}
	/* Same as above but for (A AND[-IF] (B AND-IF C)) -> ((A AND B) AND-IF C),
	   or (A OR[-IF] (B OR-IF C) -> ((A OR B) OR-IF C).  */
      else if (TREE_CODE (arg1) == icode
	  && simple_condition_p (arg0)
	  /* Needed for sequence points to handle trappings, and
	     side-effects.  */
	  && simple_condition_p (TREE_OPERAND (arg1, 0)))
	{
	  tem = fold_build2_loc (loc, ncode, type, 
				 arg0, TREE_OPERAND (arg1, 0));
	  return fold_build2_loc (loc, icode, type, tem,
				  TREE_OPERAND (arg1, 1));
	}
      /* Transform (A AND-IF B) into (A AND B), or (A OR-IF B)
	 into (A OR B).
	 For sequence point consistancy, we need to check for trapping,
	 and side-effects.  */
      else if (code == icode && simple_condition_p (arg0)
	       && simple_condition_p (arg1))
	return fold_build2_loc (loc, ncode, type, arg0, arg1);
    }

  return NULL_TREE;
}

/* Helper that tries to canonicalize the comparison ARG0 CODE ARG1
   by changing CODE to reduce the magnitude of constants involved in
   ARG0 of the comparison.
   Returns a canonicalized comparison tree if a simplification was
   possible, otherwise returns NULL_TREE.
   Set *STRICT_OVERFLOW_P to true if the canonicalization is only
   valid if signed overflow is undefined.  */

static tree
maybe_canonicalize_comparison_1 (location_t loc, enum tree_code code, tree type,
				 tree arg0, tree arg1,
				 bool *strict_overflow_p)
{
  enum tree_code code0 = TREE_CODE (arg0);
  tree t, cst0 = NULL_TREE;
  int sgn0;

  /* Match A +- CST code arg1.  We can change this only if overflow
     is undefined.  */
  if (!((ANY_INTEGRAL_TYPE_P (TREE_TYPE (arg0))
	 && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (arg0)))
	/* In principle pointers also have undefined overflow behavior,
	   but that causes problems elsewhere.  */
	&& !POINTER_TYPE_P (TREE_TYPE (arg0))
	&& (code0 == MINUS_EXPR
	    || code0 == PLUS_EXPR)
	&& TREE_CODE (TREE_OPERAND (arg0, 1)) == INTEGER_CST))
    return NULL_TREE;

  /* Identify the constant in arg0 and its sign.  */
  cst0 = TREE_OPERAND (arg0, 1);
  sgn0 = tree_int_cst_sgn (cst0);

  /* Overflowed constants and zero will cause problems.  */
  if (integer_zerop (cst0)
      || TREE_OVERFLOW (cst0))
    return NULL_TREE;

  /* See if we can reduce the magnitude of the constant in
     arg0 by changing the comparison code.  */
  /* A - CST < arg1  ->  A - CST-1 <= arg1.  */
  if (code == LT_EXPR
      && code0 == ((sgn0 == -1) ? PLUS_EXPR : MINUS_EXPR))
    code = LE_EXPR;
  /* A + CST > arg1  ->  A + CST-1 >= arg1.  */
  else if (code == GT_EXPR
	   && code0 == ((sgn0 == -1) ? MINUS_EXPR : PLUS_EXPR))
    code = GE_EXPR;
  /* A + CST <= arg1  ->  A + CST-1 < arg1.  */
  else if (code == LE_EXPR
	   && code0 == ((sgn0 == -1) ? MINUS_EXPR : PLUS_EXPR))
    code = LT_EXPR;
  /* A - CST >= arg1  ->  A - CST-1 > arg1.  */
  else if (code == GE_EXPR
	   && code0 == ((sgn0 == -1) ? PLUS_EXPR : MINUS_EXPR))
    code = GT_EXPR;
  else
    return NULL_TREE;
  *strict_overflow_p = true;

  /* Now build the constant reduced in magnitude.  But not if that
     would produce one outside of its types range.  */
  if (INTEGRAL_TYPE_P (TREE_TYPE (cst0))
      && ((sgn0 == 1
	   && TYPE_MIN_VALUE (TREE_TYPE (cst0))
	   && tree_int_cst_equal (cst0, TYPE_MIN_VALUE (TREE_TYPE (cst0))))
	  || (sgn0 == -1
	      && TYPE_MAX_VALUE (TREE_TYPE (cst0))
	      && tree_int_cst_equal (cst0, TYPE_MAX_VALUE (TREE_TYPE (cst0))))))
    return NULL_TREE;

  t = int_const_binop (sgn0 == -1 ? PLUS_EXPR : MINUS_EXPR,
		       cst0, build_int_cst (TREE_TYPE (cst0), 1));
  t = fold_build2_loc (loc, code0, TREE_TYPE (arg0), TREE_OPERAND (arg0, 0), t);
  t = fold_convert (TREE_TYPE (arg1), t);

  return fold_build2_loc (loc, code, type, t, arg1);
}

/* Canonicalize the comparison ARG0 CODE ARG1 with type TYPE with undefined
   overflow further.  Try to decrease the magnitude of constants involved
   by changing LE_EXPR and GE_EXPR to LT_EXPR and GT_EXPR or vice versa
   and put sole constants at the second argument position.
   Returns the canonicalized tree if changed, otherwise NULL_TREE.  */

static tree
maybe_canonicalize_comparison (location_t loc, enum tree_code code, tree type,
			       tree arg0, tree arg1)
{
  tree t;
  bool strict_overflow_p;
  const char * const warnmsg = G_("assuming signed overflow does not occur "
				  "when reducing constant in comparison");

  /* Try canonicalization by simplifying arg0.  */
  strict_overflow_p = false;
  t = maybe_canonicalize_comparison_1 (loc, code, type, arg0, arg1,
				       &strict_overflow_p);
  if (t)
    {
      if (strict_overflow_p)
	fold_overflow_warning (warnmsg, WARN_STRICT_OVERFLOW_MAGNITUDE);
      return t;
    }

  /* Try canonicalization by simplifying arg1 using the swapped
     comparison.  */
  code = swap_tree_comparison (code);
  strict_overflow_p = false;
  t = maybe_canonicalize_comparison_1 (loc, code, type, arg1, arg0,
				       &strict_overflow_p);
  if (t && strict_overflow_p)
    fold_overflow_warning (warnmsg, WARN_STRICT_OVERFLOW_MAGNITUDE);
  return t;
}

/* Return whether BASE + OFFSET + BITPOS may wrap around the address
   space.  This is used to avoid issuing overflow warnings for
   expressions like &p->x which cannot wrap.  */

static bool
pointer_may_wrap_p (tree base, tree offset, poly_int64 bitpos)
{
  if (!POINTER_TYPE_P (TREE_TYPE (base)))
    return true;

  if (maybe_lt (bitpos, 0))
    return true;

  poly_wide_int wi_offset;
  int precision = TYPE_PRECISION (TREE_TYPE (base));
  if (offset == NULL_TREE)
    wi_offset = wi::zero (precision);
  else if (!poly_int_tree_p (offset) || TREE_OVERFLOW (offset))
    return true;
  else
    wi_offset = wi::to_poly_wide (offset);

  wi::overflow_type overflow;
  poly_wide_int units = wi::shwi (bits_to_bytes_round_down (bitpos),
				  precision);
  poly_wide_int total = wi::add (wi_offset, units, UNSIGNED, &overflow);
  if (overflow)
    return true;

  poly_uint64 total_hwi, size;
  if (!total.to_uhwi (&total_hwi)
      || !poly_int_tree_p (TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (base))),
			   &size)
      || known_eq (size, 0U))
    return true;

  if (known_le (total_hwi, size))
    return false;

  /* We can do slightly better for SIZE if we have an ADDR_EXPR of an
     array.  */
  if (TREE_CODE (base) == ADDR_EXPR
      && poly_int_tree_p (TYPE_SIZE_UNIT (TREE_TYPE (TREE_OPERAND (base, 0))),
			  &size)
      && maybe_ne (size, 0U)
      && known_le (total_hwi, size))
    return false;

  return true;
}

/* Return a positive integer when the symbol DECL is known to have
   a nonzero address, zero when it's known not to (e.g., it's a weak
   symbol), and a negative integer when the symbol is not yet in the
   symbol table and so whether or not its address is zero is unknown.
   For function local objects always return positive integer.  */
static int
maybe_nonzero_address (tree decl)
{
  /* Normally, don't do anything for variables and functions before symtab is
     built; it is quite possible that DECL will be declared weak later.
     But if folding_initializer, we need a constant answer now, so create
     the symtab entry and prevent later weak declaration.  */
  if (DECL_P (decl) && decl_in_symtab_p (decl))
    if (struct symtab_node *symbol
	= (folding_initializer
	   ? symtab_node::get_create (decl)
	   : symtab_node::get (decl)))
      return symbol->nonzero_address ();

  /* Function local objects are never NULL.  */
  if (DECL_P (decl)
      && (DECL_CONTEXT (decl)
      && TREE_CODE (DECL_CONTEXT (decl)) == FUNCTION_DECL
      && auto_var_in_fn_p (decl, DECL_CONTEXT (decl))))
    return 1;

  return -1;
}

/* Subroutine of fold_binary.  This routine performs all of the
   transformations that are common to the equality/inequality
   operators (EQ_EXPR and NE_EXPR) and the ordering operators
   (LT_EXPR, LE_EXPR, GE_EXPR and GT_EXPR).  Callers other than
   fold_binary should call fold_binary.  Fold a comparison with
   tree code CODE and type TYPE with operands OP0 and OP1.  Return
   the folded comparison or NULL_TREE.  */

static tree
fold_comparison (location_t loc, enum tree_code code, tree type,
		 tree op0, tree op1)
{
  const bool equality_code = (code == EQ_EXPR || code == NE_EXPR);
  tree arg0, arg1, tem;

  arg0 = op0;
  arg1 = op1;

  STRIP_SIGN_NOPS (arg0);
  STRIP_SIGN_NOPS (arg1);

  /* For comparisons of pointers we can decompose it to a compile time
     comparison of the base objects and the offsets into the object.
     This requires at least one operand being an ADDR_EXPR or a
     POINTER_PLUS_EXPR to do more than the operand_equal_p test below.  */
  if (POINTER_TYPE_P (TREE_TYPE (arg0))
      && (TREE_CODE (arg0) == ADDR_EXPR
	  || TREE_CODE (arg1) == ADDR_EXPR
	  || TREE_CODE (arg0) == POINTER_PLUS_EXPR
	  || TREE_CODE (arg1) == POINTER_PLUS_EXPR))
    {
      tree base0, base1, offset0 = NULL_TREE, offset1 = NULL_TREE;
      poly_int64 bitsize, bitpos0 = 0, bitpos1 = 0;
      machine_mode mode;
      int volatilep, reversep, unsignedp;
      bool indirect_base0 = false, indirect_base1 = false;

      /* Get base and offset for the access.  Strip ADDR_EXPR for
	 get_inner_reference, but put it back by stripping INDIRECT_REF
	 off the base object if possible.  indirect_baseN will be true
	 if baseN is not an address but refers to the object itself.  */
      base0 = arg0;
      if (TREE_CODE (arg0) == ADDR_EXPR)
	{
	  base0
	    = get_inner_reference (TREE_OPERAND (arg0, 0),
				   &bitsize, &bitpos0, &offset0, &mode,
				   &unsignedp, &reversep, &volatilep);
	  if (INDIRECT_REF_P (base0))
	    base0 = TREE_OPERAND (base0, 0);
	  else
	    indirect_base0 = true;
	}
      else if (TREE_CODE (arg0) == POINTER_PLUS_EXPR)
	{
	  base0 = TREE_OPERAND (arg0, 0);
	  STRIP_SIGN_NOPS (base0);
	  if (TREE_CODE (base0) == ADDR_EXPR)
	    {
	      base0
		= get_inner_reference (TREE_OPERAND (base0, 0),
				       &bitsize, &bitpos0, &offset0, &mode,
				       &unsignedp, &reversep, &volatilep);
	      if (INDIRECT_REF_P (base0))
		base0 = TREE_OPERAND (base0, 0);
	      else
		indirect_base0 = true;
	    }
	  if (offset0 == NULL_TREE || integer_zerop (offset0))
	    offset0 = TREE_OPERAND (arg0, 1);
	  else
	    offset0 = size_binop (PLUS_EXPR, offset0,
				  TREE_OPERAND (arg0, 1));
	  if (poly_int_tree_p (offset0))
	    {
	      poly_offset_int tem = wi::sext (wi::to_poly_offset (offset0),
					      TYPE_PRECISION (sizetype));
	      tem <<= LOG2_BITS_PER_UNIT;
	      tem += bitpos0;
	      if (tem.to_shwi (&bitpos0))
		offset0 = NULL_TREE;
	    }
	}

      base1 = arg1;
      if (TREE_CODE (arg1) == ADDR_EXPR)
	{
	  base1
	    = get_inner_reference (TREE_OPERAND (arg1, 0),
				   &bitsize, &bitpos1, &offset1, &mode,
				   &unsignedp, &reversep, &volatilep);
	  if (INDIRECT_REF_P (base1))
	    base1 = TREE_OPERAND (base1, 0);
	  else
	    indirect_base1 = true;
	}
      else if (TREE_CODE (arg1) == POINTER_PLUS_EXPR)
	{
	  base1 = TREE_OPERAND (arg1, 0);
	  STRIP_SIGN_NOPS (base1);
	  if (TREE_CODE (base1) == ADDR_EXPR)
	    {
	      base1
		= get_inner_reference (TREE_OPERAND (base1, 0),
				       &bitsize, &bitpos1, &offset1, &mode,
				       &unsignedp, &reversep, &volatilep);
	      if (INDIRECT_REF_P (base1))
		base1 = TREE_OPERAND (base1, 0);
	      else
		indirect_base1 = true;
	    }
	  if (offset1 == NULL_TREE || integer_zerop (offset1))
	    offset1 = TREE_OPERAND (arg1, 1);
	  else
	    offset1 = size_binop (PLUS_EXPR, offset1,
				  TREE_OPERAND (arg1, 1));
	  if (poly_int_tree_p (offset1))
	    {
	      poly_offset_int tem = wi::sext (wi::to_poly_offset (offset1),
					      TYPE_PRECISION (sizetype));
	      tem <<= LOG2_BITS_PER_UNIT;
	      tem += bitpos1;
	      if (tem.to_shwi (&bitpos1))
		offset1 = NULL_TREE;
	    }
	}

      /* If we have equivalent bases we might be able to simplify.  */
      if (indirect_base0 == indirect_base1
	  && operand_equal_p (base0, base1,
			      indirect_base0 ? OEP_ADDRESS_OF : 0))
	{
	  /* We can fold this expression to a constant if the non-constant
	     offset parts are equal.  */
	  if ((offset0 == offset1
	       || (offset0 && offset1
		   && operand_equal_p (offset0, offset1, 0)))
	      && (equality_code
		  || (indirect_base0
		      && (DECL_P (base0) || CONSTANT_CLASS_P (base0)))
		  || TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (arg0))))
	    {
	      if (!equality_code
		  && maybe_ne (bitpos0, bitpos1)
		  && (pointer_may_wrap_p (base0, offset0, bitpos0)
		      || pointer_may_wrap_p (base1, offset1, bitpos1)))
		fold_overflow_warning (("assuming pointer wraparound does not "
					"occur when comparing P +- C1 with "
					"P +- C2"),
				       WARN_STRICT_OVERFLOW_CONDITIONAL);

	      switch (code)
		{
		case EQ_EXPR:
		  if (known_eq (bitpos0, bitpos1))
		    return constant_boolean_node (true, type);
		  if (known_ne (bitpos0, bitpos1))
		    return constant_boolean_node (false, type);
		  break;
		case NE_EXPR:
		  if (known_ne (bitpos0, bitpos1))
		    return constant_boolean_node (true, type);
		  if (known_eq (bitpos0, bitpos1))
		    return constant_boolean_node (false, type);
		  break;
		case LT_EXPR:
		  if (known_lt (bitpos0, bitpos1))
		    return constant_boolean_node (true, type);
		  if (known_ge (bitpos0, bitpos1))
		    return constant_boolean_node (false, type);
		  break;
		case LE_EXPR:
		  if (known_le (bitpos0, bitpos1))
		    return constant_boolean_node (true, type);
		  if (known_gt (bitpos0, bitpos1))
		    return constant_boolean_node (false, type);
		  break;
		case GE_EXPR:
		  if (known_ge (bitpos0, bitpos1))
		    return constant_boolean_node (true, type);
		  if (known_lt (bitpos0, bitpos1))
		    return constant_boolean_node (false, type);
		  break;
		case GT_EXPR:
		  if (known_gt (bitpos0, bitpos1))
		    return constant_boolean_node (true, type);
		  if (known_le (bitpos0, bitpos1))
		    return constant_boolean_node (false, type);
		  break;
		default:;
		}
	    }
	  /* We can simplify the comparison to a comparison of the variable
	     offset parts if the constant offset parts are equal.
	     Be careful to use signed sizetype here because otherwise we
	     mess with array offsets in the wrong way.  This is possible
	     because pointer arithmetic is restricted to retain within an
	     object and overflow on pointer differences is undefined as of
	     6.5.6/8 and /9 with respect to the signed ptrdiff_t.  */
	  else if (known_eq (bitpos0, bitpos1)
		   && (equality_code
		       || (indirect_base0
			   && (DECL_P (base0) || CONSTANT_CLASS_P (base0)))
		       || TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (arg0))))
	    {
	      /* By converting to signed sizetype we cover middle-end pointer
	         arithmetic which operates on unsigned pointer types of size
	         type size and ARRAY_REF offsets which are properly sign or
	         zero extended from their type in case it is narrower than
	         sizetype.  */
	      if (offset0 == NULL_TREE)
		offset0 = build_int_cst (ssizetype, 0);
	      else
		offset0 = fold_convert_loc (loc, ssizetype, offset0);
	      if (offset1 == NULL_TREE)
		offset1 = build_int_cst (ssizetype, 0);
	      else
		offset1 = fold_convert_loc (loc, ssizetype, offset1);

	      if (!equality_code
		  && (pointer_may_wrap_p (base0, offset0, bitpos0)
		      || pointer_may_wrap_p (base1, offset1, bitpos1)))
		fold_overflow_warning (("assuming pointer wraparound does not "
					"occur when comparing P +- C1 with "
					"P +- C2"),
				       WARN_STRICT_OVERFLOW_COMPARISON);

	      return fold_build2_loc (loc, code, type, offset0, offset1);
	    }
	}
      /* For equal offsets we can simplify to a comparison of the
	 base addresses.  */
      else if (known_eq (bitpos0, bitpos1)
	       && (indirect_base0
		   ? base0 != TREE_OPERAND (arg0, 0) : base0 != arg0)
	       && (indirect_base1
		   ? base1 != TREE_OPERAND (arg1, 0) : base1 != arg1)
	       && ((offset0 == offset1)
		   || (offset0 && offset1
		       && operand_equal_p (offset0, offset1, 0))))
	{
	  if (indirect_base0)
	    base0 = build_fold_addr_expr_loc (loc, base0);
	  if (indirect_base1)
	    base1 = build_fold_addr_expr_loc (loc, base1);
	  return fold_build2_loc (loc, code, type, base0, base1);
	}
      /* Comparison between an ordinary (non-weak) symbol and a null
	 pointer can be eliminated since such symbols must have a non
	 null address.  In C, relational expressions between pointers
	 to objects and null pointers are undefined.  The results
	 below follow the C++ rules with the additional property that
	 every object pointer compares greater than a null pointer.
      */
      else if (((DECL_P (base0)
		 && maybe_nonzero_address (base0) > 0
		 /* Avoid folding references to struct members at offset 0 to
		    prevent tests like '&ptr->firstmember == 0' from getting
		    eliminated.  When ptr is null, although the -> expression
		    is strictly speaking invalid, GCC retains it as a matter
		    of QoI.  See PR c/44555. */
		 && (offset0 == NULL_TREE && known_ne (bitpos0, 0)))
		|| CONSTANT_CLASS_P (base0))
	       && indirect_base0
	       /* The caller guarantees that when one of the arguments is
		  constant (i.e., null in this case) it is second.  */
	       && integer_zerop (arg1))
	{
	  switch (code)
	    {
	    case EQ_EXPR:
	    case LE_EXPR:
	    case LT_EXPR:
	      return constant_boolean_node (false, type);
	    case GE_EXPR:
	    case GT_EXPR:
	    case NE_EXPR:
	      return constant_boolean_node (true, type);
	    default:
	      gcc_unreachable ();
	    }
	}
    }

  /* Transform comparisons of the form X +- C1 CMP Y +- C2 to
     X CMP Y +- C2 +- C1 for signed X, Y.  This is valid if
     the resulting offset is smaller in absolute value than the
     original one and has the same sign.  */
  if (ANY_INTEGRAL_TYPE_P (TREE_TYPE (arg0))
      && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (arg0))
      && (TREE_CODE (arg0) == PLUS_EXPR || TREE_CODE (arg0) == MINUS_EXPR)
      && (TREE_CODE (TREE_OPERAND (arg0, 1)) == INTEGER_CST
	  && !TREE_OVERFLOW (TREE_OPERAND (arg0, 1)))
      && (TREE_CODE (arg1) == PLUS_EXPR || TREE_CODE (arg1) == MINUS_EXPR)
      && (TREE_CODE (TREE_OPERAND (arg1, 1)) == INTEGER_CST
	  && !TREE_OVERFLOW (TREE_OPERAND (arg1, 1))))
    {
      tree const1 = TREE_OPERAND (arg0, 1);
      tree const2 = TREE_OPERAND (arg1, 1);
      tree variable1 = TREE_OPERAND (arg0, 0);
      tree variable2 = TREE_OPERAND (arg1, 0);
      tree cst;
      const char * const warnmsg = G_("assuming signed overflow does not "
				      "occur when combining constants around "
				      "a comparison");

      /* Put the constant on the side where it doesn't overflow and is
	 of lower absolute value and of same sign than before.  */
      cst = int_const_binop (TREE_CODE (arg0) == TREE_CODE (arg1)
			     ? MINUS_EXPR : PLUS_EXPR,
			     const2, const1);
      if (!TREE_OVERFLOW (cst)
	  && tree_int_cst_compare (const2, cst) == tree_int_cst_sgn (const2)
	  && tree_int_cst_sgn (cst) == tree_int_cst_sgn (const2))
	{
	  fold_overflow_warning (warnmsg, WARN_STRICT_OVERFLOW_COMPARISON);
	  return fold_build2_loc (loc, code, type,
				  variable1,
				  fold_build2_loc (loc, TREE_CODE (arg1),
						   TREE_TYPE (arg1),
						   variable2, cst));
	}

      cst = int_const_binop (TREE_CODE (arg0) == TREE_CODE (arg1)
			     ? MINUS_EXPR : PLUS_EXPR,
			     const1, const2);
      if (!TREE_OVERFLOW (cst)
	  && tree_int_cst_compare (const1, cst) == tree_int_cst_sgn (const1)
	  && tree_int_cst_sgn (cst) == tree_int_cst_sgn (const1))
	{
	  fold_overflow_warning (warnmsg, WARN_STRICT_OVERFLOW_COMPARISON);
	  return fold_build2_loc (loc, code, type,
				  fold_build2_loc (loc, TREE_CODE (arg0),
						   TREE_TYPE (arg0),
						   variable1, cst),
				  variable2);
	}
    }

  tem = maybe_canonicalize_comparison (loc, code, type, arg0, arg1);
  if (tem)
    return tem;

  /* If we are comparing an expression that just has comparisons
     of two integer values, arithmetic expressions of those comparisons,
     and constants, we can simplify it.  There are only three cases
     to check: the two values can either be equal, the first can be
     greater, or the second can be greater.  Fold the expression for
     those three values.  Since each value must be 0 or 1, we have
     eight possibilities, each of which corresponds to the constant 0
     or 1 or one of the six possible comparisons.

     This handles common cases like (a > b) == 0 but also handles
     expressions like  ((x > y) - (y > x)) > 0, which supposedly
     occur in macroized code.  */

  if (TREE_CODE (arg1) == INTEGER_CST && TREE_CODE (arg0) != INTEGER_CST)
    {
      tree cval1 = 0, cval2 = 0;

      if (twoval_comparison_p (arg0, &cval1, &cval2)
	  /* Don't handle degenerate cases here; they should already
	     have been handled anyway.  */
	  && cval1 != 0 && cval2 != 0
	  && ! (TREE_CONSTANT (cval1) && TREE_CONSTANT (cval2))
	  && TREE_TYPE (cval1) == TREE_TYPE (cval2)
	  && INTEGRAL_TYPE_P (TREE_TYPE (cval1))
	  && TYPE_MAX_VALUE (TREE_TYPE (cval1))
	  && TYPE_MAX_VALUE (TREE_TYPE (cval2))
	  && ! operand_equal_p (TYPE_MIN_VALUE (TREE_TYPE (cval1)),
				TYPE_MAX_VALUE (TREE_TYPE (cval2)), 0))
	{
	  tree maxval = TYPE_MAX_VALUE (TREE_TYPE (cval1));
	  tree minval = TYPE_MIN_VALUE (TREE_TYPE (cval1));

	  /* We can't just pass T to eval_subst in case cval1 or cval2
	     was the same as ARG1.  */

	  tree high_result
		= fold_build2_loc (loc, code, type,
			       eval_subst (loc, arg0, cval1, maxval,
					   cval2, minval),
			       arg1);
	  tree equal_result
		= fold_build2_loc (loc, code, type,
			       eval_subst (loc, arg0, cval1, maxval,
					   cval2, maxval),
			       arg1);
	  tree low_result
		= fold_build2_loc (loc, code, type,
			       eval_subst (loc, arg0, cval1, minval,
					   cval2, maxval),
			       arg1);

	  /* All three of these results should be 0 or 1.  Confirm they are.
	     Then use those values to select the proper code to use.  */

	  if (TREE_CODE (high_result) == INTEGER_CST
	      && TREE_CODE (equal_result) == INTEGER_CST
	      && TREE_CODE (low_result) == INTEGER_CST)
	    {
	      /* Make a 3-bit mask with the high-order bit being the
		 value for `>', the next for '=', and the low for '<'.  */
	      switch ((integer_onep (high_result) * 4)
		      + (integer_onep (equal_result) * 2)
		      + integer_onep (low_result))
		{
		case 0:
		  /* Always false.  */
		  return omit_one_operand_loc (loc, type, integer_zero_node, arg0);
		case 1:
		  code = LT_EXPR;
		  break;
		case 2:
		  code = EQ_EXPR;
		  break;
		case 3:
		  code = LE_EXPR;
		  break;
		case 4:
		  code = GT_EXPR;
		  break;
		case 5:
		  code = NE_EXPR;
		  break;
		case 6:
		  code = GE_EXPR;
		  break;
		case 7:
		  /* Always true.  */
		  return omit_one_operand_loc (loc, type, integer_one_node, arg0);
		}

	      return fold_build2_loc (loc, code, type, cval1, cval2);
	    }
	}
    }

  return NULL_TREE;
}


/* Subroutine of fold_binary.  Optimize complex multiplications of the
   form z * conj(z), as pow(realpart(z),2) + pow(imagpart(z),2).  The
   argument EXPR represents the expression "z" of type TYPE.  */

static tree
fold_mult_zconjz (location_t loc, tree type, tree expr)
{
  tree itype = TREE_TYPE (type);
  tree rpart, ipart, tem;

  if (TREE_CODE (expr) == COMPLEX_EXPR)
    {
      rpart = TREE_OPERAND (expr, 0);
      ipart = TREE_OPERAND (expr, 1);
    }
  else if (TREE_CODE (expr) == COMPLEX_CST)
    {
      rpart = TREE_REALPART (expr);
      ipart = TREE_IMAGPART (expr);
    }
  else
    {
      expr = save_expr (expr);
      rpart = fold_build1_loc (loc, REALPART_EXPR, itype, expr);
      ipart = fold_build1_loc (loc, IMAGPART_EXPR, itype, expr);
    }

  rpart = save_expr (rpart);
  ipart = save_expr (ipart);
  tem = fold_build2_loc (loc, PLUS_EXPR, itype,
		     fold_build2_loc (loc, MULT_EXPR, itype, rpart, rpart),
		     fold_build2_loc (loc, MULT_EXPR, itype, ipart, ipart));
  return fold_build2_loc (loc, COMPLEX_EXPR, type, tem,
			  build_zero_cst (itype));
}


/* Helper function for fold_vec_perm.  Store elements of VECTOR_CST or
   CONSTRUCTOR ARG into array ELTS, which has NELTS elements, and return
   true if successful.  */

static bool
vec_cst_ctor_to_array (tree arg, unsigned int nelts, tree *elts)
{
  unsigned HOST_WIDE_INT i, nunits;

  if (TREE_CODE (arg) == VECTOR_CST
      && VECTOR_CST_NELTS (arg).is_constant (&nunits))
    {
      for (i = 0; i < nunits; ++i)
	elts[i] = VECTOR_CST_ELT (arg, i);
    }
  else if (TREE_CODE (arg) == CONSTRUCTOR)
    {
      constructor_elt *elt;

      FOR_EACH_VEC_SAFE_ELT (CONSTRUCTOR_ELTS (arg), i, elt)
	if (i >= nelts || TREE_CODE (TREE_TYPE (elt->value)) == VECTOR_TYPE)
	  return false;
	else
	  elts[i] = elt->value;
    }
  else
    return false;
  for (; i < nelts; i++)
    elts[i]
      = fold_convert (TREE_TYPE (TREE_TYPE (arg)), integer_zero_node);
  return true;
}

/* Helper routine for fold_vec_perm_cst to check if SEL is a suitable
   mask for VLA vec_perm folding.
   REASON if specified, will contain the reason why SEL is not suitable.
   Used only for debugging and unit-testing.  */

static bool
valid_mask_for_fold_vec_perm_cst_p (tree arg0, tree arg1,
				    const vec_perm_indices &sel,
				    const char **reason = NULL)
{
  unsigned sel_npatterns = sel.encoding ().npatterns ();
  unsigned sel_nelts_per_pattern = sel.encoding ().nelts_per_pattern ();

  if (!(pow2p_hwi (sel_npatterns)
	&& pow2p_hwi (VECTOR_CST_NPATTERNS (arg0))
	&& pow2p_hwi (VECTOR_CST_NPATTERNS (arg1))))
    {
      if (reason)
	*reason = "npatterns is not power of 2";
      return false;
    }

  /* We want to avoid cases where sel.length is not a multiple of npatterns.
     For eg: sel.length = 2 + 2x, and sel npatterns = 4.  */
  poly_uint64 esel;
  if (!multiple_p (sel.length (), sel_npatterns, &esel))
    {
      if (reason)
	*reason = "sel.length is not multiple of sel_npatterns";
      return false;
    }

  if (sel_nelts_per_pattern < 3)
    return true;

  for (unsigned pattern = 0; pattern < sel_npatterns; pattern++)
    {
      poly_uint64 a1 = sel[pattern + sel_npatterns];
      poly_uint64 a2 = sel[pattern + 2 * sel_npatterns];
      HOST_WIDE_INT step;
      if (!poly_int64 (a2 - a1).is_constant (&step))
	{
	  if (reason)
	    *reason = "step is not constant";
	  return false;
	}
      // FIXME: Punt on step < 0 for now, revisit later.
      if (step < 0)
	return false;
      if (step == 0)
	continue;

      if (!pow2p_hwi (step))
	{
	  if (reason)
	    *reason = "step is not power of 2";
	  return false;
	}

      /* Ensure that stepped sequence of the pattern selects elements
	 only from the same input vector.  */
      uint64_t q1, qe;
      poly_uint64 r1, re;
      poly_uint64 ae = a1 + (esel - 2) * step;
      poly_uint64 arg_len = TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg0));

      if (!(can_div_trunc_p (a1, arg_len, &q1, &r1)
	    && can_div_trunc_p (ae, arg_len, &qe, &re)
	    && q1 == qe))
	{
	  if (reason)
	    *reason = "crossed input vectors";
	  return false;
	}

      /* Ensure that the stepped sequence always selects from the same
	 input pattern.  */
      tree arg = ((q1 & 1) == 0) ? arg0 : arg1;
      unsigned arg_npatterns = VECTOR_CST_NPATTERNS (arg);

      if (!multiple_p (step, arg_npatterns))
	{
	  if (reason)
	    *reason = "step is not multiple of npatterns";
	  return false;
	}

      /* If a1 chooses base element from arg, ensure that it's a natural
	 stepped sequence, ie, (arg[2] - arg[1]) == (arg[1] - arg[0])
	 to preserve arg's encoding.  */

      if (maybe_lt (r1, arg_npatterns))
	{
	  unsigned HOST_WIDE_INT index;
	  if (!r1.is_constant (&index))
	    return false;

	  tree arg_elem0 = vector_cst_elt (arg, index);
	  tree arg_elem1 = vector_cst_elt (arg, index + arg_npatterns);
	  tree arg_elem2 = vector_cst_elt (arg, index + arg_npatterns * 2);

	  tree step1, step2;
	  if (!(step1 = const_binop (MINUS_EXPR, arg_elem1, arg_elem0))
	      || !(step2 = const_binop (MINUS_EXPR, arg_elem2, arg_elem1))
	      || !operand_equal_p (step1, step2, 0))
	    {
	      if (reason)
		*reason = "not a natural stepped sequence";
	      return false;
	    }
	}
    }

  return true;
}

/* Try to fold permutation of ARG0 and ARG1 with SEL selector when
   the input vectors are VECTOR_CST. Return NULL_TREE otherwise.
   REASON has same purpose as described in
   valid_mask_for_fold_vec_perm_cst_p.  */

static tree
fold_vec_perm_cst (tree type, tree arg0, tree arg1, const vec_perm_indices &sel,
		   const char **reason = NULL)
{
  unsigned res_npatterns, res_nelts_per_pattern;
  unsigned HOST_WIDE_INT res_nelts;

  /* First try to implement the fold in a VLA-friendly way.

     (1) If the selector is simply a duplication of N elements, the
	 result is likewise a duplication of N elements.

     (2) If the selector is N elements followed by a duplication
	 of N elements, the result is too.

     (3) If the selector is N elements followed by an interleaving
	 of N linear series, the situation is more complex.

	 valid_mask_for_fold_vec_perm_cst_p detects whether we
	 can handle this case.  If we can, then each of the N linear
	 series either (a) selects the same element each time or
	 (b) selects a linear series from one of the input patterns.

	 If (b) holds for one of the linear series, the result
	 will contain a linear series, and so the result will have
	 the same shape as the selector.  If (a) holds for all of
	 the linear series, the result will be the same as (2) above.

	 (b) can only hold if one of the input patterns has a
	 stepped encoding.  */

  if (valid_mask_for_fold_vec_perm_cst_p (arg0, arg1, sel, reason))
    {
      res_npatterns = sel.encoding ().npatterns ();
      res_nelts_per_pattern = sel.encoding ().nelts_per_pattern ();
      if (res_nelts_per_pattern == 3
	  && VECTOR_CST_NELTS_PER_PATTERN (arg0) < 3
	  && VECTOR_CST_NELTS_PER_PATTERN (arg1) < 3)
	res_nelts_per_pattern = 2;
      res_nelts = res_npatterns * res_nelts_per_pattern;
    }
  else if (TYPE_VECTOR_SUBPARTS (type).is_constant (&res_nelts))
    {
      res_npatterns = res_nelts;
      res_nelts_per_pattern = 1;
    }
  else
    return NULL_TREE;

  tree_vector_builder out_elts (type, res_npatterns, res_nelts_per_pattern);
  for (unsigned i = 0; i < res_nelts; i++)
    {
      poly_uint64 len = TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg0));
      uint64_t q;
      poly_uint64 r;
      unsigned HOST_WIDE_INT index;

      /* Punt if sel[i] /trunc_div len cannot be determined,
	 because the input vector to be chosen will depend on
	 runtime vector length.
	 For example if len == 4 + 4x, and sel[i] == 4,
	 If len at runtime equals 4, we choose arg1[0].
	 For any other value of len > 4 at runtime, we choose arg0[4].
	 which makes the element choice dependent on runtime vector length.  */
      if (!can_div_trunc_p (sel[i], len, &q, &r))
	{
	  if (reason)
	    *reason = "cannot divide selector element by arg len";
	  return NULL_TREE;
	}

      /* sel[i] % len will give the index of element in the chosen input
	 vector. For example if sel[i] == 5 + 4x and len == 4 + 4x,
	 we will choose arg1[1] since (5 + 4x) % (4 + 4x) == 1.  */
      if (!r.is_constant (&index))
	{
	  if (reason)
	    *reason = "remainder is not constant";
	  return NULL_TREE;
	}

      tree arg = ((q & 1) == 0) ? arg0 : arg1;
      tree elem = vector_cst_elt (arg, index);
      out_elts.quick_push (elem);
    }

  return out_elts.build ();
}

/* Attempt to fold vector permutation of ARG0 and ARG1 vectors using SEL
   selector.  Return the folded VECTOR_CST or CONSTRUCTOR if successful,
   NULL_TREE otherwise.  */

tree
fold_vec_perm (tree type, tree arg0, tree arg1, const vec_perm_indices &sel)
{
  unsigned int i;
  unsigned HOST_WIDE_INT nelts;

  gcc_assert (known_eq (TYPE_VECTOR_SUBPARTS (type), sel.length ())
	      && known_eq (TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg0)),
			   TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg1))));

  if (TREE_TYPE (TREE_TYPE (arg0)) != TREE_TYPE (type)
      || TREE_TYPE (TREE_TYPE (arg1)) != TREE_TYPE (type))
    return NULL_TREE;

  if (TREE_CODE (arg0) == VECTOR_CST
      && TREE_CODE (arg1) == VECTOR_CST)
    return fold_vec_perm_cst (type, arg0, arg1, sel);

  /* For fall back case, we want to ensure we have VLS vectors
     with equal length.  */
  if (!sel.length ().is_constant (&nelts))
    return NULL_TREE;

  gcc_assert (known_eq (sel.length (),
			TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg0))));
  tree *in_elts = XALLOCAVEC (tree, nelts * 2);
  if (!vec_cst_ctor_to_array (arg0, nelts, in_elts)
      || !vec_cst_ctor_to_array (arg1, nelts, in_elts + nelts))
    return NULL_TREE;

  vec<constructor_elt, va_gc> *v;
  vec_alloc (v, nelts);
  for (i = 0; i < nelts; i++)
    {
      HOST_WIDE_INT index;
      if (!sel[i].is_constant (&index))
	return NULL_TREE;
      CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, in_elts[index]);
    }
  return build_constructor (type, v);
}

/* Try to fold a pointer difference of type TYPE two address expressions of
   array references AREF0 and AREF1 using location LOC.  Return a
   simplified expression for the difference or NULL_TREE.  */

static tree
fold_addr_of_array_ref_difference (location_t loc, tree type,
				   tree aref0, tree aref1,
				   bool use_pointer_diff)
{
  tree base0 = TREE_OPERAND (aref0, 0);
  tree base1 = TREE_OPERAND (aref1, 0);
  tree base_offset = build_int_cst (type, 0);

  /* If the bases are array references as well, recurse.  If the bases
     are pointer indirections compute the difference of the pointers.
     If the bases are equal, we are set.  */
  if ((TREE_CODE (base0) == ARRAY_REF
       && TREE_CODE (base1) == ARRAY_REF
       && (base_offset
	   = fold_addr_of_array_ref_difference (loc, type, base0, base1,
						use_pointer_diff)))
      || (INDIRECT_REF_P (base0)
	  && INDIRECT_REF_P (base1)
	  && (base_offset
	        = use_pointer_diff
		  ? fold_binary_loc (loc, POINTER_DIFF_EXPR, type,
				     TREE_OPERAND (base0, 0),
				     TREE_OPERAND (base1, 0))
		  : fold_binary_loc (loc, MINUS_EXPR, type,
				     fold_convert (type,
						   TREE_OPERAND (base0, 0)),
				     fold_convert (type,
						   TREE_OPERAND (base1, 0)))))
      || operand_equal_p (base0, base1, OEP_ADDRESS_OF))
    {
      tree op0 = fold_convert_loc (loc, type, TREE_OPERAND (aref0, 1));
      tree op1 = fold_convert_loc (loc, type, TREE_OPERAND (aref1, 1));
      tree esz = fold_convert_loc (loc, type, array_ref_element_size (aref0));
      tree diff = fold_build2_loc (loc, MINUS_EXPR, type, op0, op1);
      return fold_build2_loc (loc, PLUS_EXPR, type,
			      base_offset,
			      fold_build2_loc (loc, MULT_EXPR, type,
					       diff, esz));
    }
  return NULL_TREE;
}

/* If the real or vector real constant CST of type TYPE has an exact
   inverse, return it, else return NULL.  */

tree
exact_inverse (tree type, tree cst)
{
  REAL_VALUE_TYPE r;
  tree unit_type;
  machine_mode mode;

  switch (TREE_CODE (cst))
    {
    case REAL_CST:
      r = TREE_REAL_CST (cst);

      if (exact_real_inverse (TYPE_MODE (type), &r))
	return build_real (type, r);

      return NULL_TREE;

    case VECTOR_CST:
      {
	unit_type = TREE_TYPE (type);
	mode = TYPE_MODE (unit_type);

	tree_vector_builder elts;
	if (!elts.new_unary_operation (type, cst, false))
	  return NULL_TREE;
	unsigned int count = elts.encoded_nelts ();
	for (unsigned int i = 0; i < count; ++i)
	  {
	    r = TREE_REAL_CST (VECTOR_CST_ELT (cst, i));
	    if (!exact_real_inverse (mode, &r))
	      return NULL_TREE;
	    elts.quick_push (build_real (unit_type, r));
	  }

	return elts.build ();
      }

    default:
      return NULL_TREE;
    }
}

/*  Mask out the tz least significant bits of X of type TYPE where
    tz is the number of trailing zeroes in Y.  */
static wide_int
mask_with_tz (tree type, const wide_int &x, const wide_int &y)
{
  int tz = wi::ctz (y);
  if (tz > 0)
    return wi::mask (tz, true, TYPE_PRECISION (type)) & x;
  return x;
}

/* Return true when T is an address and is known to be nonzero.
   For floating point we further ensure that T is not denormal.
   Similar logic is present in nonzero_address in rtlanal.h.

   If the return value is based on the assumption that signed overflow
   is undefined, set *STRICT_OVERFLOW_P to true; otherwise, don't
   change *STRICT_OVERFLOW_P.  */

static bool
tree_expr_nonzero_warnv_p (tree t, bool *strict_overflow_p)
{
  tree type = TREE_TYPE (t);
  enum tree_code code;

  /* Doing something useful for floating point would need more work.  */
  if (!INTEGRAL_TYPE_P (type) && !POINTER_TYPE_P (type))
    return false;

  code = TREE_CODE (t);
  switch (TREE_CODE_CLASS (code))
    {
    case tcc_unary:
      return tree_unary_nonzero_warnv_p (code, type, TREE_OPERAND (t, 0),
					      strict_overflow_p);
    case tcc_binary:
    case tcc_comparison:
      return tree_binary_nonzero_warnv_p (code, type,
					       TREE_OPERAND (t, 0),
					       TREE_OPERAND (t, 1),
					       strict_overflow_p);
    case tcc_constant:
    case tcc_declaration:
    case tcc_reference:
      return tree_single_nonzero_warnv_p (t, strict_overflow_p);

    default:
      break;
    }

  switch (code)
    {
    case TRUTH_NOT_EXPR:
      return tree_unary_nonzero_warnv_p (code, type, TREE_OPERAND (t, 0),
					      strict_overflow_p);

    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
      return tree_binary_nonzero_warnv_p (code, type,
					       TREE_OPERAND (t, 0),
					       TREE_OPERAND (t, 1),
					       strict_overflow_p);

    case COND_EXPR:
    case CONSTRUCTOR:
    case OBJ_TYPE_REF:
    case ADDR_EXPR:
    case WITH_SIZE_EXPR:
    case SSA_NAME:
      return tree_single_nonzero_warnv_p (t, strict_overflow_p);

    case COMPOUND_EXPR:
    case MODIFY_EXPR:
    case BIND_EXPR:
      return tree_expr_nonzero_warnv_p (TREE_OPERAND (t, 1),
					strict_overflow_p);

    case SAVE_EXPR:
      return tree_expr_nonzero_warnv_p (TREE_OPERAND (t, 0),
					strict_overflow_p);

    case CALL_EXPR:
      {
	tree fndecl = get_callee_fndecl (t);
	if (!fndecl) return false;
	if (flag_delete_null_pointer_checks && !flag_check_new
	    && DECL_IS_OPERATOR_NEW_P (fndecl)
	    && !TREE_NOTHROW (fndecl))
	  return true;
	if (flag_delete_null_pointer_checks
	    && lookup_attribute ("returns_nonnull",
		 TYPE_ATTRIBUTES (TREE_TYPE (fndecl))))
	  return true;
	return alloca_call_p (t);
      }

    default:
      break;
    }
  return false;
}

/* Return true when T is an address and is known to be nonzero.
   Handle warnings about undefined signed overflow.  */

bool
tree_expr_nonzero_p (tree t)
{
  bool ret, strict_overflow_p;

  strict_overflow_p = false;
  ret = tree_expr_nonzero_warnv_p (t, &strict_overflow_p);
  if (strict_overflow_p)
    fold_overflow_warning (("assuming signed overflow does not occur when "
			    "determining that expression is always "
			    "non-zero"),
			   WARN_STRICT_OVERFLOW_MISC);
  return ret;
}

/* Return true if T is known not to be equal to an integer W.  */

bool
expr_not_equal_to (tree t, const wide_int &w)
{
  int_range_max vr;
  switch (TREE_CODE (t))
    {
    case INTEGER_CST:
      return wi::to_wide (t) != w;

    case SSA_NAME:
      if (!INTEGRAL_TYPE_P (TREE_TYPE (t)))
	return false;

      get_range_query (cfun)->range_of_expr (vr, t);
      if (!vr.undefined_p () && !vr.contains_p (w))
	return true;
      /* If T has some known zero bits and W has any of those bits set,
	 then T is known not to be equal to W.  */
      if (wi::ne_p (wi::zext (wi::bit_and_not (w, get_nonzero_bits (t)),
			      TYPE_PRECISION (TREE_TYPE (t))), 0))
	return true;
      return false;

    default:
      return false;
    }
}

/* Fold a binary expression of code CODE and type TYPE with operands
   OP0 and OP1.  LOC is the location of the resulting expression.
   Return the folded expression if folding is successful.  Otherwise,
   return NULL_TREE.  */

tree
fold_binary_loc (location_t loc, enum tree_code code, tree type,
		 tree op0, tree op1)
{
  enum tree_code_class kind = TREE_CODE_CLASS (code);
  tree arg0, arg1, tem;
  tree t1 = NULL_TREE;
  bool strict_overflow_p;
  unsigned int prec;

  gcc_assert (IS_EXPR_CODE_CLASS (kind)
	      && TREE_CODE_LENGTH (code) == 2
	      && op0 != NULL_TREE
	      && op1 != NULL_TREE);

  arg0 = op0;
  arg1 = op1;

  /* Strip any conversions that don't change the mode.  This is
     safe for every expression, except for a comparison expression
     because its signedness is derived from its operands.  So, in
     the latter case, only strip conversions that don't change the
     signedness.  MIN_EXPR/MAX_EXPR also need signedness of arguments
     preserved.

     Note that this is done as an internal manipulation within the
     constant folder, in order to find the simplest representation
     of the arguments so that their form can be studied.  In any
     cases, the appropriate type conversions should be put back in
     the tree that will get out of the constant folder.  */

  if (kind == tcc_comparison || code == MIN_EXPR || code == MAX_EXPR)
    {
      STRIP_SIGN_NOPS (arg0);
      STRIP_SIGN_NOPS (arg1);
    }
  else
    {
      STRIP_NOPS (arg0);
      STRIP_NOPS (arg1);
    }

  /* Note that TREE_CONSTANT isn't enough: static var addresses are
     constant but we can't do arithmetic on them.  */
  if (CONSTANT_CLASS_P (arg0) && CONSTANT_CLASS_P (arg1))
    {
      tem = const_binop (code, type, arg0, arg1);
      if (tem != NULL_TREE)
	{
	  if (TREE_TYPE (tem) != type)
	    tem = fold_convert_loc (loc, type, tem);
	  return tem;
	}
    }

  /* If this is a commutative operation, and ARG0 is a constant, move it
     to ARG1 to reduce the number of tests below.  */
  if (commutative_tree_code (code)
      && tree_swap_operands_p (arg0, arg1))
    return fold_build2_loc (loc, code, type, op1, op0);

  /* Likewise if this is a comparison, and ARG0 is a constant, move it
     to ARG1 to reduce the number of tests below.  */
  if (kind == tcc_comparison
      && tree_swap_operands_p (arg0, arg1))
    return fold_build2_loc (loc, swap_tree_comparison (code), type, op1, op0);

  tem = generic_simplify (loc, code, type, op0, op1);
  if (tem)
    return tem;

  /* ARG0 is the first operand of EXPR, and ARG1 is the second operand.

     First check for cases where an arithmetic operation is applied to a
     compound, conditional, or comparison operation.  Push the arithmetic
     operation inside the compound or conditional to see if any folding
     can then be done.  Convert comparison to conditional for this purpose.
     The also optimizes non-constant cases that used to be done in
     expand_expr.

     Before we do that, see if this is a BIT_AND_EXPR or a BIT_IOR_EXPR,
     one of the operands is a comparison and the other is a comparison, a
     BIT_AND_EXPR with the constant 1, or a truth value.  In that case, the
     code below would make the expression more complex.  Change it to a
     TRUTH_{AND,OR}_EXPR.  Likewise, convert a similar NE_EXPR to
     TRUTH_XOR_EXPR and an EQ_EXPR to the inversion of a TRUTH_XOR_EXPR.  */

  if ((code == BIT_AND_EXPR || code == BIT_IOR_EXPR
       || code == EQ_EXPR || code == NE_EXPR)
      && !VECTOR_TYPE_P (TREE_TYPE (arg0))
      && ((truth_value_p (TREE_CODE (arg0))
	   && (truth_value_p (TREE_CODE (arg1))
	       || (TREE_CODE (arg1) == BIT_AND_EXPR
		   && integer_onep (TREE_OPERAND (arg1, 1)))))
	  || (truth_value_p (TREE_CODE (arg1))
	      && (truth_value_p (TREE_CODE (arg0))
		  || (TREE_CODE (arg0) == BIT_AND_EXPR
		      && integer_onep (TREE_OPERAND (arg0, 1)))))))
    {
      tem = fold_build2_loc (loc, code == BIT_AND_EXPR ? TRUTH_AND_EXPR
			 : code == BIT_IOR_EXPR ? TRUTH_OR_EXPR
			 : TRUTH_XOR_EXPR,
			 boolean_type_node,
			 fold_convert_loc (loc, boolean_type_node, arg0),
			 fold_convert_loc (loc, boolean_type_node, arg1));

      if (code == EQ_EXPR)
	tem = invert_truthvalue_loc (loc, tem);

      return fold_convert_loc (loc, type, tem);
    }

  if (TREE_CODE_CLASS (code) == tcc_binary
      || TREE_CODE_CLASS (code) == tcc_comparison)
    {
      if (TREE_CODE (arg0) == COMPOUND_EXPR)
	{
	  tem = fold_build2_loc (loc, code, type,
			     fold_convert_loc (loc, TREE_TYPE (op0),
					       TREE_OPERAND (arg0, 1)), op1);
	  return build2_loc (loc, COMPOUND_EXPR, type, TREE_OPERAND (arg0, 0),
			     tem);
	}
      if (TREE_CODE (arg1) == COMPOUND_EXPR)
	{
	  tem = fold_build2_loc (loc, code, type, op0,
			     fold_convert_loc (loc, TREE_TYPE (op1),
					       TREE_OPERAND (arg1, 1)));
	  return build2_loc (loc, COMPOUND_EXPR, type, TREE_OPERAND (arg1, 0),
			     tem);
	}

      if (TREE_CODE (arg0) == COND_EXPR
	  || TREE_CODE (arg0) == VEC_COND_EXPR
	  || COMPARISON_CLASS_P (arg0))
	{
	  tem = fold_binary_op_with_conditional_arg (loc, code, type, op0, op1,
						     arg0, arg1,
						     /*cond_first_p=*/1);
	  if (tem != NULL_TREE)
	    return tem;
	}

      if (TREE_CODE (arg1) == COND_EXPR
	  || TREE_CODE (arg1) == VEC_COND_EXPR
	  || COMPARISON_CLASS_P (arg1))
	{
	  tem = fold_binary_op_with_conditional_arg (loc, code, type, op0, op1,
						     arg1, arg0,
					             /*cond_first_p=*/0);
	  if (tem != NULL_TREE)
	    return tem;
	}
    }

  switch (code)
    {
    case MEM_REF:
      /* MEM[&MEM[p, CST1], CST2] -> MEM[p, CST1 + CST2].  */
      if (TREE_CODE (arg0) == ADDR_EXPR
	  && TREE_CODE (TREE_OPERAND (arg0, 0)) == MEM_REF)
	{
	  tree iref = TREE_OPERAND (arg0, 0);
	  return fold_build2 (MEM_REF, type,
			      TREE_OPERAND (iref, 0),
			      int_const_binop (PLUS_EXPR, arg1,
					       TREE_OPERAND (iref, 1)));
	}

      /* MEM[&a.b, CST2] -> MEM[&a, offsetof (a, b) + CST2].  */
      if (TREE_CODE (arg0) == ADDR_EXPR
	  && handled_component_p (TREE_OPERAND (arg0, 0)))
	{
	  tree base;
	  poly_int64 coffset;
	  base = get_addr_base_and_unit_offset (TREE_OPERAND (arg0, 0),
						&coffset);
	  if (!base)
	    return NULL_TREE;
	  return fold_build2 (MEM_REF, type,
			      build1 (ADDR_EXPR, TREE_TYPE (arg0), base),
			      int_const_binop (PLUS_EXPR, arg1,
					       size_int (coffset)));
	}

      return NULL_TREE;

    case POINTER_PLUS_EXPR:
      /* INT +p INT -> (PTR)(INT + INT).  Stripping types allows for this. */
      if (INTEGRAL_TYPE_P (TREE_TYPE (arg1))
	   && INTEGRAL_TYPE_P (TREE_TYPE (arg0)))
        return fold_convert_loc (loc, type,
				 fold_build2_loc (loc, PLUS_EXPR, sizetype,
					      fold_convert_loc (loc, sizetype,
								arg1),
					      fold_convert_loc (loc, sizetype,
								arg0)));

      return NULL_TREE;

    case PLUS_EXPR:
      if (INTEGRAL_TYPE_P (type) || VECTOR_INTEGER_TYPE_P (type))
	{
	  /* X + (X / CST) * -CST is X % CST.  */
	  if (TREE_CODE (arg1) == MULT_EXPR
	      && TREE_CODE (TREE_OPERAND (arg1, 0)) == TRUNC_DIV_EXPR
	      && operand_equal_p (arg0,
				  TREE_OPERAND (TREE_OPERAND (arg1, 0), 0), 0))
	    {
	      tree cst0 = TREE_OPERAND (TREE_OPERAND (arg1, 0), 1);
	      tree cst1 = TREE_OPERAND (arg1, 1);
	      tree sum = fold_binary_loc (loc, PLUS_EXPR, TREE_TYPE (cst1),
				      cst1, cst0);
	      if (sum && integer_zerop (sum))
		return fold_convert_loc (loc, type,
					 fold_build2_loc (loc, TRUNC_MOD_EXPR,
						      TREE_TYPE (arg0), arg0,
						      cst0));
	    }
	}

      /* Handle (A1 * C1) + (A2 * C2) with A1, A2 or C1, C2 being the same or
	 one.  Make sure the type is not saturating and has the signedness of
	 the stripped operands, as fold_plusminus_mult_expr will re-associate.
	 ??? The latter condition should use TYPE_OVERFLOW_* flags instead.  */
      if ((TREE_CODE (arg0) == MULT_EXPR
	   || TREE_CODE (arg1) == MULT_EXPR)
	  && !TYPE_SATURATING (type)
	  && TYPE_UNSIGNED (type) == TYPE_UNSIGNED (TREE_TYPE (arg0))
	  && TYPE_UNSIGNED (type) == TYPE_UNSIGNED (TREE_TYPE (arg1))
	  && (!FLOAT_TYPE_P (type) || flag_associative_math))
        {
	  tree tem = fold_plusminus_mult_expr (loc, code, type, arg0, arg1);
	  if (tem)
	    return tem;
	}

      if (! FLOAT_TYPE_P (type))
	{
	  /* Reassociate (plus (plus (mult) (foo)) (mult)) as
	     (plus (plus (mult) (mult)) (foo)) so that we can
	     take advantage of the factoring cases below.  */
	  if (ANY_INTEGRAL_TYPE_P (type)
	      && TYPE_OVERFLOW_WRAPS (type)
	      && (((TREE_CODE (arg0) == PLUS_EXPR
		    || TREE_CODE (arg0) == MINUS_EXPR)
		   && TREE_CODE (arg1) == MULT_EXPR)
		  || ((TREE_CODE (arg1) == PLUS_EXPR
		       || TREE_CODE (arg1) == MINUS_EXPR)
		      && TREE_CODE (arg0) == MULT_EXPR)))
	    {
	      tree parg0, parg1, parg, marg;
	      enum tree_code pcode;

	      if (TREE_CODE (arg1) == MULT_EXPR)
		parg = arg0, marg = arg1;
	      else
		parg = arg1, marg = arg0;
	      pcode = TREE_CODE (parg);
	      parg0 = TREE_OPERAND (parg, 0);
	      parg1 = TREE_OPERAND (parg, 1);
	      STRIP_NOPS (parg0);
	      STRIP_NOPS (parg1);

	      if (TREE_CODE (parg0) == MULT_EXPR
		  && TREE_CODE (parg1) != MULT_EXPR)
		return fold_build2_loc (loc, pcode, type,
				    fold_build2_loc (loc, PLUS_EXPR, type,
						 fold_convert_loc (loc, type,
								   parg0),
						 fold_convert_loc (loc, type,
								   marg)),
				    fold_convert_loc (loc, type, parg1));
	      if (TREE_CODE (parg0) != MULT_EXPR
		  && TREE_CODE (parg1) == MULT_EXPR)
		return
		  fold_build2_loc (loc, PLUS_EXPR, type,
			       fold_convert_loc (loc, type, parg0),
			       fold_build2_loc (loc, pcode, type,
					    fold_convert_loc (loc, type, marg),
					    fold_convert_loc (loc, type,
							      parg1)));
	    }
	}
      else
	{
	  /* Fold __complex__ ( x, 0 ) + __complex__ ( 0, y )
	     to __complex__ ( x, y ).  This is not the same for SNaNs or
	     if signed zeros are involved.  */
	  if (!HONOR_SNANS (arg0)
	      && !HONOR_SIGNED_ZEROS (arg0)
	      && COMPLEX_FLOAT_TYPE_P (TREE_TYPE (arg0)))
	    {
	      tree rtype = TREE_TYPE (TREE_TYPE (arg0));
	      tree arg0r = fold_unary_loc (loc, REALPART_EXPR, rtype, arg0);
	      tree arg0i = fold_unary_loc (loc, IMAGPART_EXPR, rtype, arg0);
	      bool arg0rz = false, arg0iz = false;
	      if ((arg0r && (arg0rz = real_zerop (arg0r)))
		  || (arg0i && (arg0iz = real_zerop (arg0i))))
		{
		  tree arg1r = fold_unary_loc (loc, REALPART_EXPR, rtype, arg1);
		  tree arg1i = fold_unary_loc (loc, IMAGPART_EXPR, rtype, arg1);
		  if (arg0rz && arg1i && real_zerop (arg1i))
		    {
		      tree rp = arg1r ? arg1r
				  : build1 (REALPART_EXPR, rtype, arg1);
		      tree ip = arg0i ? arg0i
				  : build1 (IMAGPART_EXPR, rtype, arg0);
		      return fold_build2_loc (loc, COMPLEX_EXPR, type, rp, ip);
		    }
		  else if (arg0iz && arg1r && real_zerop (arg1r))
		    {
		      tree rp = arg0r ? arg0r
				  : build1 (REALPART_EXPR, rtype, arg0);
		      tree ip = arg1i ? arg1i
				  : build1 (IMAGPART_EXPR, rtype, arg1);
		      return fold_build2_loc (loc, COMPLEX_EXPR, type, rp, ip);
		    }
		}
	    }

          /* Convert a + (b*c + d*e) into (a + b*c) + d*e.
             We associate floats only if the user has specified
             -fassociative-math.  */
          if (flag_associative_math
              && TREE_CODE (arg1) == PLUS_EXPR
              && TREE_CODE (arg0) != MULT_EXPR)
            {
              tree tree10 = TREE_OPERAND (arg1, 0);
              tree tree11 = TREE_OPERAND (arg1, 1);
              if (TREE_CODE (tree11) == MULT_EXPR
		  && TREE_CODE (tree10) == MULT_EXPR)
                {
                  tree tree0;
                  tree0 = fold_build2_loc (loc, PLUS_EXPR, type, arg0, tree10);
                  return fold_build2_loc (loc, PLUS_EXPR, type, tree0, tree11);
                }
            }
          /* Convert (b*c + d*e) + a into b*c + (d*e +a).
             We associate floats only if the user has specified
             -fassociative-math.  */
          if (flag_associative_math
              && TREE_CODE (arg0) == PLUS_EXPR
              && TREE_CODE (arg1) != MULT_EXPR)
            {
              tree tree00 = TREE_OPERAND (arg0, 0);
              tree tree01 = TREE_OPERAND (arg0, 1);
              if (TREE_CODE (tree01) == MULT_EXPR
		  && TREE_CODE (tree00) == MULT_EXPR)
                {
                  tree tree0;
                  tree0 = fold_build2_loc (loc, PLUS_EXPR, type, tree01, arg1);
                  return fold_build2_loc (loc, PLUS_EXPR, type, tree00, tree0);
                }
            }
	}

     bit_rotate:
      /* (A << C1) + (A >> C2) if A is unsigned and C1+C2 is the size of A
	 is a rotate of A by C1 bits.  */
      /* (A << B) + (A >> (Z - B)) if A is unsigned and Z is the size of A
	 is a rotate of A by B bits.
	 Similarly for (A << B) | (A >> (-B & C3)) where C3 is Z-1,
	 though in this case CODE must be | and not + or ^, otherwise
	 it doesn't return A when B is 0.  */
      {
	enum tree_code code0, code1;
	tree rtype;
	code0 = TREE_CODE (arg0);
	code1 = TREE_CODE (arg1);
	if (((code0 == RSHIFT_EXPR && code1 == LSHIFT_EXPR)
	     || (code1 == RSHIFT_EXPR && code0 == LSHIFT_EXPR))
	    && operand_equal_p (TREE_OPERAND (arg0, 0),
			        TREE_OPERAND (arg1, 0), 0)
	    && (rtype = TREE_TYPE (TREE_OPERAND (arg0, 0)),
	        TYPE_UNSIGNED (rtype))
	    /* Only create rotates in complete modes.  Other cases are not
	       expanded properly.  */
	    && (element_precision (rtype)
		== GET_MODE_UNIT_PRECISION (TYPE_MODE (rtype))))
	  {
	    tree tree01, tree11;
	    tree orig_tree01, orig_tree11;
	    enum tree_code code01, code11;

	    tree01 = orig_tree01 = TREE_OPERAND (arg0, 1);
	    tree11 = orig_tree11 = TREE_OPERAND (arg1, 1);
	    STRIP_NOPS (tree01);
	    STRIP_NOPS (tree11);
	    code01 = TREE_CODE (tree01);
	    code11 = TREE_CODE (tree11);
	    if (code11 != MINUS_EXPR
		&& (code01 == MINUS_EXPR || code01 == BIT_AND_EXPR))
	      {
		std::swap (code0, code1);
		std::swap (code01, code11);
		std::swap (tree01, tree11);
		std::swap (orig_tree01, orig_tree11);
	      }
	    if (code01 == INTEGER_CST
		&& code11 == INTEGER_CST
		&& (wi::to_widest (tree01) + wi::to_widest (tree11)
		    == element_precision (rtype)))
	      {
		tem = build2_loc (loc, LROTATE_EXPR,
				  rtype, TREE_OPERAND (arg0, 0),
				  code0 == LSHIFT_EXPR
				  ? orig_tree01 : orig_tree11);
		return fold_convert_loc (loc, type, tem);
	      }
	    else if (code11 == MINUS_EXPR)
	      {
		tree tree110, tree111;
		tree110 = TREE_OPERAND (tree11, 0);
		tree111 = TREE_OPERAND (tree11, 1);
		STRIP_NOPS (tree110);
		STRIP_NOPS (tree111);
		if (TREE_CODE (tree110) == INTEGER_CST
		    && compare_tree_int (tree110,
					 element_precision (rtype)) == 0
		    && operand_equal_p (tree01, tree111, 0))
		  {
		    tem = build2_loc (loc, (code0 == LSHIFT_EXPR
					    ? LROTATE_EXPR : RROTATE_EXPR),
				      rtype, TREE_OPERAND (arg0, 0),
				      orig_tree01);
		    return fold_convert_loc (loc, type, tem);
		  }
	      }
	    else if (code == BIT_IOR_EXPR
		     && code11 == BIT_AND_EXPR
		     && pow2p_hwi (element_precision (rtype)))
	      {
		tree tree110, tree111;
		tree110 = TREE_OPERAND (tree11, 0);
		tree111 = TREE_OPERAND (tree11, 1);
		STRIP_NOPS (tree110);
		STRIP_NOPS (tree111);
		if (TREE_CODE (tree110) == NEGATE_EXPR
		    && TREE_CODE (tree111) == INTEGER_CST
		    && compare_tree_int (tree111,
					 element_precision (rtype) - 1) == 0
		    && operand_equal_p (tree01, TREE_OPERAND (tree110, 0), 0))
		  {
		    tem = build2_loc (loc, (code0 == LSHIFT_EXPR
					    ? LROTATE_EXPR : RROTATE_EXPR),
				      rtype, TREE_OPERAND (arg0, 0),
				      orig_tree01);
		    return fold_convert_loc (loc, type, tem);
		  }
	      }
	  }
      }

    associate:
      /* In most languages, can't associate operations on floats through
	 parentheses.  Rather than remember where the parentheses were, we
	 don't associate floats at all, unless the user has specified
	 -fassociative-math.
	 And, we need to make sure type is not saturating.  */

      if ((! FLOAT_TYPE_P (type) || flag_associative_math)
	  && !TYPE_SATURATING (type)
	  && !TYPE_OVERFLOW_SANITIZED (type))
	{
	  tree var0, minus_var0, con0, minus_con0, lit0, minus_lit0;
	  tree var1, minus_var1, con1, minus_con1, lit1, minus_lit1;
	  tree atype = type;
	  bool ok = true;

	  /* Split both trees into variables, constants, and literals.  Then
	     associate each group together, the constants with literals,
	     then the result with variables.  This increases the chances of
	     literals being recombined later and of generating relocatable
	     expressions for the sum of a constant and literal.  */
	  var0 = split_tree (arg0, type, code,
			     &minus_var0, &con0, &minus_con0,
			     &lit0, &minus_lit0, 0);
	  var1 = split_tree (arg1, type, code,
			     &minus_var1, &con1, &minus_con1,
			     &lit1, &minus_lit1, code == MINUS_EXPR);

	  /* Recombine MINUS_EXPR operands by using PLUS_EXPR.  */
	  if (code == MINUS_EXPR)
	    code = PLUS_EXPR;

	  /* With undefined overflow prefer doing association in a type
	     which wraps on overflow, if that is one of the operand types.  */
	  if ((POINTER_TYPE_P (type) || INTEGRAL_TYPE_P (type))
	      && !TYPE_OVERFLOW_WRAPS (type))
	    {
	      if (INTEGRAL_TYPE_P (TREE_TYPE (arg0))
		  && TYPE_OVERFLOW_WRAPS (TREE_TYPE (arg0)))
		atype = TREE_TYPE (arg0);
	      else if (INTEGRAL_TYPE_P (TREE_TYPE (arg1))
		       && TYPE_OVERFLOW_WRAPS (TREE_TYPE (arg1)))
		atype = TREE_TYPE (arg1);
	      gcc_assert (TYPE_PRECISION (atype) == TYPE_PRECISION (type));
	    }

	  /* With undefined overflow we can only associate constants with one
	     variable, and constants whose association doesn't overflow.  */
	  if ((POINTER_TYPE_P (atype) || INTEGRAL_TYPE_P (atype))
	      && !TYPE_OVERFLOW_WRAPS (atype))
	    {
	      if ((var0 && var1) || (minus_var0 && minus_var1))
		{
		  /* ???  If split_tree would handle NEGATE_EXPR we could
		     simply reject these cases and the allowed cases would
		     be the var0/minus_var1 ones.  */
		  tree tmp0 = var0 ? var0 : minus_var0;
		  tree tmp1 = var1 ? var1 : minus_var1;
		  bool one_neg = false;

		  if (TREE_CODE (tmp0) == NEGATE_EXPR)
		    {
		      tmp0 = TREE_OPERAND (tmp0, 0);
		      one_neg = !one_neg;
		    }
		  if (CONVERT_EXPR_P (tmp0)
		      && INTEGRAL_TYPE_P (TREE_TYPE (TREE_OPERAND (tmp0, 0)))
		      && (TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (tmp0, 0)))
			  <= TYPE_PRECISION (atype)))
		    tmp0 = TREE_OPERAND (tmp0, 0);
		  if (TREE_CODE (tmp1) == NEGATE_EXPR)
		    {
		      tmp1 = TREE_OPERAND (tmp1, 0);
		      one_neg = !one_neg;
		    }
		  if (CONVERT_EXPR_P (tmp1)
		      && INTEGRAL_TYPE_P (TREE_TYPE (TREE_OPERAND (tmp1, 0)))
		      && (TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (tmp1, 0)))
			  <= TYPE_PRECISION (atype)))
		    tmp1 = TREE_OPERAND (tmp1, 0);
		  /* The only case we can still associate with two variables
		     is if they cancel out.  */
		  if (!one_neg
		      || !operand_equal_p (tmp0, tmp1, 0))
		    ok = false;
		}
	      else if ((var0 && minus_var1
			&& ! operand_equal_p (var0, minus_var1, 0))
		       || (minus_var0 && var1
			   && ! operand_equal_p (minus_var0, var1, 0)))
		ok = false;
	    }

	  /* Only do something if we found more than two objects.  Otherwise,
	     nothing has changed and we risk infinite recursion.  */
	  if (ok
	      && ((var0 != 0) + (var1 != 0)
		  + (minus_var0 != 0) + (minus_var1 != 0)
		  + (con0 != 0) + (con1 != 0)
		  + (minus_con0 != 0) + (minus_con1 != 0)
		  + (lit0 != 0) + (lit1 != 0)
		  + (minus_lit0 != 0) + (minus_lit1 != 0)) > 2)
	    {
	      int var0_origin = (var0 != 0) + 2 * (var1 != 0);
	      int minus_var0_origin
		= (minus_var0 != 0) + 2 * (minus_var1 != 0);
	      int con0_origin = (con0 != 0) + 2 * (con1 != 0);
	      int minus_con0_origin
		= (minus_con0 != 0) + 2 * (minus_con1 != 0);
	      int lit0_origin = (lit0 != 0) + 2 * (lit1 != 0);
	      int minus_lit0_origin
		= (minus_lit0 != 0) + 2 * (minus_lit1 != 0);
	      var0 = associate_trees (loc, var0, var1, code, atype);
	      minus_var0 = associate_trees (loc, minus_var0, minus_var1,
					    code, atype);
	      con0 = associate_trees (loc, con0, con1, code, atype);
	      minus_con0 = associate_trees (loc, minus_con0, minus_con1,
					    code, atype);
	      lit0 = associate_trees (loc, lit0, lit1, code, atype);
	      minus_lit0 = associate_trees (loc, minus_lit0, minus_lit1,
					    code, atype);

	      if (minus_var0 && var0)
		{
		  var0_origin |= minus_var0_origin;
		  var0 = associate_trees (loc, var0, minus_var0,
					  MINUS_EXPR, atype);
		  minus_var0 = 0;
		  minus_var0_origin = 0;
		}
	      if (minus_con0 && con0)
		{
		  con0_origin |= minus_con0_origin;
		  con0 = associate_trees (loc, con0, minus_con0,
					  MINUS_EXPR, atype);
		  minus_con0 = 0;
		  minus_con0_origin = 0;
		}

	      /* Preserve the MINUS_EXPR if the negative part of the literal is
		 greater than the positive part.  Otherwise, the multiplicative
		 folding code (i.e extract_muldiv) may be fooled in case
		 unsigned constants are subtracted, like in the following
		 example: ((X*2 + 4) - 8U)/2.  */
	      if (minus_lit0 && lit0)
		{
		  if (TREE_CODE (lit0) == INTEGER_CST
		      && TREE_CODE (minus_lit0) == INTEGER_CST
		      && tree_int_cst_lt (lit0, minus_lit0)
		      /* But avoid ending up with only negated parts.  */
		      && (var0 || con0))
		    {
		      minus_lit0_origin |= lit0_origin;
		      minus_lit0 = associate_trees (loc, minus_lit0, lit0,
						    MINUS_EXPR, atype);
		      lit0 = 0;
		      lit0_origin = 0;
		    }
		  else
		    {
		      lit0_origin |= minus_lit0_origin;
		      lit0 = associate_trees (loc, lit0, minus_lit0,
					      MINUS_EXPR, atype);
		      minus_lit0 = 0;
		      minus_lit0_origin = 0;
		    }
		}

	      /* Don't introduce overflows through reassociation.  */
	      if ((lit0 && TREE_OVERFLOW_P (lit0))
		  || (minus_lit0 && TREE_OVERFLOW_P (minus_lit0)))
		return NULL_TREE;

	      /* Eliminate lit0 and minus_lit0 to con0 and minus_con0. */
	      con0_origin |= lit0_origin;
	      con0 = associate_trees (loc, con0, lit0, code, atype);
	      minus_con0_origin |= minus_lit0_origin;
	      minus_con0 = associate_trees (loc, minus_con0, minus_lit0,
					    code, atype);

	      /* Eliminate minus_con0.  */
	      if (minus_con0)
		{
		  if (con0)
		    {
		      con0_origin |= minus_con0_origin;
		      con0 = associate_trees (loc, con0, minus_con0,
					      MINUS_EXPR, atype);
		    }
		  else if (var0)
		    {
		      var0_origin |= minus_con0_origin;
		      var0 = associate_trees (loc, var0, minus_con0,
					      MINUS_EXPR, atype);
		    }
		  else
		    gcc_unreachable ();
		}

	      /* Eliminate minus_var0.  */
	      if (minus_var0)
		{
		  if (con0)
		    {
		      con0_origin |= minus_var0_origin;
		      con0 = associate_trees (loc, con0, minus_var0,
					      MINUS_EXPR, atype);
		    }
		  else
		    gcc_unreachable ();
		}

	      /* Reassociate only if there has been any actual association
		 between subtrees from op0 and subtrees from op1 in at
		 least one of the operands, otherwise we risk infinite
		 recursion.  See PR114084.  */
	      if (var0_origin != 3 && con0_origin != 3)
		return NULL_TREE;

	      return
		fold_convert_loc (loc, type, associate_trees (loc, var0, con0,
							      code, atype));
	    }
	}

      return NULL_TREE;

    case POINTER_DIFF_EXPR:
    case MINUS_EXPR:
      /* Fold &a[i] - &a[j] to i-j.  */
      if (TREE_CODE (arg0) == ADDR_EXPR
	  && TREE_CODE (TREE_OPERAND (arg0, 0)) == ARRAY_REF
	  && TREE_CODE (arg1) == ADDR_EXPR
	  && TREE_CODE (TREE_OPERAND (arg1, 0)) == ARRAY_REF)
        {
	  tree tem = fold_addr_of_array_ref_difference (loc, type,
							TREE_OPERAND (arg0, 0),
							TREE_OPERAND (arg1, 0),
							code
							== POINTER_DIFF_EXPR);
	  if (tem)
	    return tem;
	}

      /* Further transformations are not for pointers.  */
      if (code == POINTER_DIFF_EXPR)
	return NULL_TREE;

      /* (-A) - B -> (-B) - A  where B is easily negated and we can swap.  */
      if (TREE_CODE (arg0) == NEGATE_EXPR
	  && negate_expr_p (op1)
	  /* If arg0 is e.g. unsigned int and type is int, then this could
	     introduce UB, because if A is INT_MIN at runtime, the original
	     expression can be well defined while the latter is not.
	     See PR83269.  */
	  && !(ANY_INTEGRAL_TYPE_P (type)
	       && TYPE_OVERFLOW_UNDEFINED (type)
	       && ANY_INTEGRAL_TYPE_P (TREE_TYPE (arg0))
	       && !TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (arg0))))
	return fold_build2_loc (loc, MINUS_EXPR, type, negate_expr (op1),
			        fold_convert_loc (loc, type,
						  TREE_OPERAND (arg0, 0)));

      /* Fold __complex__ ( x, 0 ) - __complex__ ( 0, y ) to
	 __complex__ ( x, -y ).  This is not the same for SNaNs or if
	 signed zeros are involved.  */
      if (!HONOR_SNANS (arg0)
	  && !HONOR_SIGNED_ZEROS (arg0)
	  && COMPLEX_FLOAT_TYPE_P (TREE_TYPE (arg0)))
        {
	  tree rtype = TREE_TYPE (TREE_TYPE (arg0));
	  tree arg0r = fold_unary_loc (loc, REALPART_EXPR, rtype, arg0);
	  tree arg0i = fold_unary_loc (loc, IMAGPART_EXPR, rtype, arg0);
	  bool arg0rz = false, arg0iz = false;
	  if ((arg0r && (arg0rz = real_zerop (arg0r)))
	      || (arg0i && (arg0iz = real_zerop (arg0i))))
	    {
	      tree arg1r = fold_unary_loc (loc, REALPART_EXPR, rtype, arg1);
	      tree arg1i = fold_unary_loc (loc, IMAGPART_EXPR, rtype, arg1);
	      if (arg0rz && arg1i && real_zerop (arg1i))
	        {
		  tree rp = fold_build1_loc (loc, NEGATE_EXPR, rtype,
					 arg1r ? arg1r
					 : build1 (REALPART_EXPR, rtype, arg1));
		  tree ip = arg0i ? arg0i
		    : build1 (IMAGPART_EXPR, rtype, arg0);
		  return fold_build2_loc (loc, COMPLEX_EXPR, type, rp, ip);
		}
	      else if (arg0iz && arg1r && real_zerop (arg1r))
	        {
		  tree rp = arg0r ? arg0r
		    : build1 (REALPART_EXPR, rtype, arg0);
		  tree ip = fold_build1_loc (loc, NEGATE_EXPR, rtype,
					 arg1i ? arg1i
					 : build1 (IMAGPART_EXPR, rtype, arg1));
		  return fold_build2_loc (loc, COMPLEX_EXPR, type, rp, ip);
		}
	    }
	}

      /* A - B -> A + (-B) if B is easily negatable.  */
      if (negate_expr_p (op1)
	  && ! TYPE_OVERFLOW_SANITIZED (type)
	  && ((FLOAT_TYPE_P (type)
               /* Avoid this transformation if B is a positive REAL_CST.  */
	       && (TREE_CODE (op1) != REAL_CST
		   || REAL_VALUE_NEGATIVE (TREE_REAL_CST (op1))))
	      || INTEGRAL_TYPE_P (type)))
	return fold_build2_loc (loc, PLUS_EXPR, type,
				fold_convert_loc (loc, type, arg0),
				negate_expr (op1));

      /* Handle (A1 * C1) - (A2 * C2) with A1, A2 or C1, C2 being the same or
	 one.  Make sure the type is not saturating and has the signedness of
	 the stripped operands, as fold_plusminus_mult_expr will re-associate.
	 ??? The latter condition should use TYPE_OVERFLOW_* flags instead.  */
      if ((TREE_CODE (arg0) == MULT_EXPR
	   || TREE_CODE (arg1) == MULT_EXPR)
	  && !TYPE_SATURATING (type)
	  && TYPE_UNSIGNED (type) == TYPE_UNSIGNED (TREE_TYPE (arg0))
	  && TYPE_UNSIGNED (type) == TYPE_UNSIGNED (TREE_TYPE (arg1))
	  && (!FLOAT_TYPE_P (type) || flag_associative_math))
        {
	  tree tem = fold_plusminus_mult_expr (loc, code, type, arg0, arg1);
	  if (tem)
	    return tem;
	}

      goto associate;

    case MULT_EXPR:
      if (! FLOAT_TYPE_P (type))
	{
	  /* Transform x * -C into -x * C if x is easily negatable.  */
	  if (TREE_CODE (op1) == INTEGER_CST
	      && tree_int_cst_sgn (op1) == -1
	      && negate_expr_p (op0)
	      && negate_expr_p (op1)
	      && (tem = negate_expr (op1)) != op1
	      && ! TREE_OVERFLOW (tem))
	    return fold_build2_loc (loc, MULT_EXPR, type,
				    fold_convert_loc (loc, type,
						      negate_expr (op0)), tem);

	  strict_overflow_p = false;
	  if (TREE_CODE (arg1) == INTEGER_CST
	      && (tem = extract_muldiv (op0, arg1, code, NULL_TREE,
					&strict_overflow_p)) != 0)
	    {
	      if (strict_overflow_p)
		fold_overflow_warning (("assuming signed overflow does not "
					"occur when simplifying "
					"multiplication"),
				       WARN_STRICT_OVERFLOW_MISC);
	      return fold_convert_loc (loc, type, tem);
	    }

	  /* Optimize z * conj(z) for integer complex numbers.  */
	  if (TREE_CODE (arg0) == CONJ_EXPR
	      && operand_equal_p (TREE_OPERAND (arg0, 0), arg1, 0))
	    return fold_mult_zconjz (loc, type, arg1);
	  if (TREE_CODE (arg1) == CONJ_EXPR
	      && operand_equal_p (arg0, TREE_OPERAND (arg1, 0), 0))
	    return fold_mult_zconjz (loc, type, arg0);
	}
      else
	{
	  /* Fold z * +-I to __complex__ (-+__imag z, +-__real z).
	     This is not the same for NaNs or if signed zeros are
	     involved.  */
	  if (!HONOR_NANS (arg0)
	      && !HONOR_SIGNED_ZEROS (arg0)
	      && COMPLEX_FLOAT_TYPE_P (TREE_TYPE (arg0))
	      && TREE_CODE (arg1) == COMPLEX_CST
	      && real_zerop (TREE_REALPART (arg1)))
	    {
	      tree rtype = TREE_TYPE (TREE_TYPE (arg0));
	      if (real_onep (TREE_IMAGPART (arg1)))
		{
		  if (TREE_CODE (arg0) != COMPLEX_EXPR)
		    arg0 = save_expr (arg0);
		  tree iarg0 = fold_build1_loc (loc, IMAGPART_EXPR,
						rtype, arg0);
		  tree rarg0 = fold_build1_loc (loc, REALPART_EXPR,
						rtype, arg0);
		  return fold_build2_loc (loc, COMPLEX_EXPR, type,
					  negate_expr (iarg0),
					  rarg0);
		}
	      else if (real_minus_onep (TREE_IMAGPART (arg1)))
		{
		  if (TREE_CODE (arg0) != COMPLEX_EXPR)
		    arg0 = save_expr (arg0);
		  tree iarg0 = fold_build1_loc (loc, IMAGPART_EXPR,
						rtype, arg0);
		  tree rarg0 = fold_build1_loc (loc, REALPART_EXPR,
						rtype, arg0);
		  return fold_build2_loc (loc, COMPLEX_EXPR, type,
					  iarg0,
					  negate_expr (rarg0));
		}
	    }

	  /* Optimize z * conj(z) for floating point complex numbers.
	     Guarded by flag_unsafe_math_optimizations as non-finite
	     imaginary components don't produce scalar results.  */
	  if (flag_unsafe_math_optimizations
	      && TREE_CODE (arg0) == CONJ_EXPR
	      && operand_equal_p (TREE_OPERAND (arg0, 0), arg1, 0))
	    return fold_mult_zconjz (loc, type, arg1);
	  if (flag_unsafe_math_optimizations
	      && TREE_CODE (arg1) == CONJ_EXPR
	      && operand_equal_p (arg0, TREE_OPERAND (arg1, 0), 0))
	    return fold_mult_zconjz (loc, type, arg0);
	}
      goto associate;

    case BIT_IOR_EXPR:
      /* Canonicalize (X & C1) | C2.  */
      if (TREE_CODE (arg0) == BIT_AND_EXPR
	  && TREE_CODE (arg1) == INTEGER_CST
	  && TREE_CODE (TREE_OPERAND (arg0, 1)) == INTEGER_CST)
	{
	  int width = TYPE_PRECISION (type), w;
	  wide_int c1 = wi::to_wide (TREE_OPERAND (arg0, 1));
	  wide_int c2 = wi::to_wide (arg1);

	  /* If (C1&C2) == C1, then (X&C1)|C2 becomes (X,C2).  */
	  if ((c1 & c2) == c1)
	    return omit_one_operand_loc (loc, type, arg1,
					 TREE_OPERAND (arg0, 0));

	  wide_int msk = wi::mask (width, false,
				   TYPE_PRECISION (TREE_TYPE (arg1)));

	  /* If (C1|C2) == ~0 then (X&C1)|C2 becomes X|C2.  */
	  if (wi::bit_and_not (msk, c1 | c2) == 0)
	    {
	      tem = fold_convert_loc (loc, type, TREE_OPERAND (arg0, 0));
	      return fold_build2_loc (loc, BIT_IOR_EXPR, type, tem, arg1);
	    }

	  /* Minimize the number of bits set in C1, i.e. C1 := C1 & ~C2,
	     unless (C1 & ~C2) | (C2 & C3) for some C3 is a mask of some
	     mode which allows further optimizations.  */
	  c1 &= msk;
	  c2 &= msk;
	  wide_int c3 = wi::bit_and_not (c1, c2);
	  for (w = BITS_PER_UNIT; w <= width; w <<= 1)
	    {
	      wide_int mask = wi::mask (w, false,
					TYPE_PRECISION (type));
	      if (((c1 | c2) & mask) == mask
		  && wi::bit_and_not (c1, mask) == 0)
		{
		  c3 = mask;
		  break;
		}
	    }

	  if (c3 != c1)
	    {
	      tem = fold_convert_loc (loc, type, TREE_OPERAND (arg0, 0));
	      tem = fold_build2_loc (loc, BIT_AND_EXPR, type, tem,
				     wide_int_to_tree (type, c3));
	      return fold_build2_loc (loc, BIT_IOR_EXPR, type, tem, arg1);
	    }
	}

      /* See if this can be simplified into a rotate first.  If that
	 is unsuccessful continue in the association code.  */
      goto bit_rotate;

    case BIT_XOR_EXPR:
      /* Fold (X & 1) ^ 1 as (X & 1) == 0.  */
      if (TREE_CODE (arg0) == BIT_AND_EXPR
	  && INTEGRAL_TYPE_P (type)
	  && integer_onep (TREE_OPERAND (arg0, 1))
	  && integer_onep (arg1))
	return fold_build2_loc (loc, EQ_EXPR, type, arg0,
				build_zero_cst (TREE_TYPE (arg0)));

      /* See if this can be simplified into a rotate first.  If that
	 is unsuccessful continue in the association code.  */
      goto bit_rotate;

    case BIT_AND_EXPR:
      /* Fold !X & 1 as X == 0.  */
      if (TREE_CODE (arg0) == TRUTH_NOT_EXPR
	  && integer_onep (arg1))
	{
	  tem = TREE_OPERAND (arg0, 0);
	  return fold_build2_loc (loc, EQ_EXPR, type, tem,
				  build_zero_cst (TREE_TYPE (tem)));
	}

      /* Fold (X * Y) & -(1 << CST) to X * Y if Y is a constant
         multiple of 1 << CST.  */
      if (TREE_CODE (arg1) == INTEGER_CST)
	{
	  wi::tree_to_wide_ref cst1 = wi::to_wide (arg1);
	  wide_int ncst1 = -cst1;
	  if ((cst1 & ncst1) == ncst1
	      && multiple_of_p (type, arg0,
				wide_int_to_tree (TREE_TYPE (arg1), ncst1)))
	    return fold_convert_loc (loc, type, arg0);
	}

      /* Fold (X * CST1) & CST2 to zero if we can, or drop known zero
         bits from CST2.  */
      if (TREE_CODE (arg1) == INTEGER_CST
	  && TREE_CODE (arg0) == MULT_EXPR
	  && TREE_CODE (TREE_OPERAND (arg0, 1)) == INTEGER_CST)
	{
	  wi::tree_to_wide_ref warg1 = wi::to_wide (arg1);
	  wide_int masked
	    = mask_with_tz (type, warg1, wi::to_wide (TREE_OPERAND (arg0, 1)));

	  if (masked == 0)
	    return omit_two_operands_loc (loc, type, build_zero_cst (type),
	                                  arg0, arg1);
	  else if (masked != warg1)
	    {
	      /* Avoid the transform if arg1 is a mask of some
	         mode which allows further optimizations.  */
	      int pop = wi::popcount (warg1);
	      if (!(pop >= BITS_PER_UNIT
		    && pow2p_hwi (pop)
		    && wi::mask (pop, false, warg1.get_precision ()) == warg1))
		return fold_build2_loc (loc, code, type, op0,
					wide_int_to_tree (type, masked));
	    }
	}

      /* Simplify ((int)c & 0377) into (int)c, if c is unsigned char.  */
      if (TREE_CODE (arg1) == INTEGER_CST && TREE_CODE (arg0) == NOP_EXPR
	  && TYPE_UNSIGNED (TREE_TYPE (TREE_OPERAND (arg0, 0))))
	{
	  prec = element_precision (TREE_TYPE (TREE_OPERAND (arg0, 0)));

	  wide_int mask = wide_int::from (wi::to_wide (arg1), prec, UNSIGNED);
	  if (mask == -1)
	    return
	      fold_convert_loc (loc, type, TREE_OPERAND (arg0, 0));
	}

      goto associate;

    case RDIV_EXPR:
      /* Don't touch a floating-point divide by zero unless the mode
	 of the constant can represent infinity.  */
      if (TREE_CODE (arg1) == REAL_CST
	  && !MODE_HAS_INFINITIES (TYPE_MODE (TREE_TYPE (arg1)))
	  && real_zerop (arg1))
	return NULL_TREE;

      /* (-A) / (-B) -> A / B  */
      if (TREE_CODE (arg0) == NEGATE_EXPR && negate_expr_p (arg1))
	return fold_build2_loc (loc, RDIV_EXPR, type,
			    TREE_OPERAND (arg0, 0),
			    negate_expr (arg1));
      if (TREE_CODE (arg1) == NEGATE_EXPR && negate_expr_p (arg0))
	return fold_build2_loc (loc, RDIV_EXPR, type,
			    negate_expr (arg0),
			    TREE_OPERAND (arg1, 0));
      return NULL_TREE;

    case TRUNC_DIV_EXPR:
      /* Fall through */
      
    case FLOOR_DIV_EXPR:
      /* Simplify A / (B << N) where A and B are positive and B is
	 a power of 2, to A >> (N + log2(B)).  */
      strict_overflow_p = false;
      if (TREE_CODE (arg1) == LSHIFT_EXPR
	  && (TYPE_UNSIGNED (type)
	      || tree_expr_nonnegative_warnv_p (op0, &strict_overflow_p)))
	{
	  tree sval = TREE_OPERAND (arg1, 0);
	  if (integer_pow2p (sval) && tree_int_cst_sgn (sval) > 0)
	    {
	      tree sh_cnt = TREE_OPERAND (arg1, 1);
	      tree pow2 = build_int_cst (TREE_TYPE (sh_cnt),
					 wi::exact_log2 (wi::to_wide (sval)));

	      if (strict_overflow_p)
		fold_overflow_warning (("assuming signed overflow does not "
					"occur when simplifying A / (B << N)"),
				       WARN_STRICT_OVERFLOW_MISC);

	      sh_cnt = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (sh_cnt),
					sh_cnt, pow2);
	      return fold_build2_loc (loc, RSHIFT_EXPR, type,
				      fold_convert_loc (loc, type, arg0), sh_cnt);
	    }
	}

      /* Fall through */

    case ROUND_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case EXACT_DIV_EXPR:
      if (integer_zerop (arg1))
	return NULL_TREE;

      /* Convert -A / -B to A / B when the type is signed and overflow is
	 undefined.  */
      if ((!ANY_INTEGRAL_TYPE_P (type) || TYPE_OVERFLOW_UNDEFINED (type))
	  && TREE_CODE (op0) == NEGATE_EXPR
	  && negate_expr_p (op1))
	{
	  if (ANY_INTEGRAL_TYPE_P (type))
	    fold_overflow_warning (("assuming signed overflow does not occur "
				    "when distributing negation across "
				    "division"),
				   WARN_STRICT_OVERFLOW_MISC);
	  return fold_build2_loc (loc, code, type,
				  fold_convert_loc (loc, type,
						    TREE_OPERAND (arg0, 0)),
				  negate_expr (op1));
	}
      if ((!ANY_INTEGRAL_TYPE_P (type) || TYPE_OVERFLOW_UNDEFINED (type))
	  && TREE_CODE (arg1) == NEGATE_EXPR
	  && negate_expr_p (op0))
	{
	  if (ANY_INTEGRAL_TYPE_P (type))
	    fold_overflow_warning (("assuming signed overflow does not occur "
				    "when distributing negation across "
				    "division"),
				   WARN_STRICT_OVERFLOW_MISC);
	  return fold_build2_loc (loc, code, type,
				  negate_expr (op0),
				  fold_convert_loc (loc, type,
						    TREE_OPERAND (arg1, 0)));
	}

      /* If arg0 is a multiple of arg1, then rewrite to the fastest div
	 operation, EXACT_DIV_EXPR.

	 Note that only CEIL_DIV_EXPR and FLOOR_DIV_EXPR are rewritten now.
	 At one time others generated faster code, it's not clear if they do
	 after the last round to changes to the DIV code in expmed.cc.  */
      if ((code == CEIL_DIV_EXPR || code == FLOOR_DIV_EXPR)
	  && multiple_of_p (type, arg0, arg1))
	return fold_build2_loc (loc, EXACT_DIV_EXPR, type,
				fold_convert (type, arg0),
				fold_convert (type, arg1));

      strict_overflow_p = false;
      if (TREE_CODE (arg1) == INTEGER_CST
	  && (tem = extract_muldiv (op0, arg1, code, NULL_TREE,
				    &strict_overflow_p)) != 0)
	{
	  if (strict_overflow_p)
	    fold_overflow_warning (("assuming signed overflow does not occur "
				    "when simplifying division"),
				   WARN_STRICT_OVERFLOW_MISC);
	  return fold_convert_loc (loc, type, tem);
	}

      return NULL_TREE;

    case CEIL_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case ROUND_MOD_EXPR:
    case TRUNC_MOD_EXPR:
      strict_overflow_p = false;
      if (TREE_CODE (arg1) == INTEGER_CST
	  && (tem = extract_muldiv (op0, arg1, code, NULL_TREE,
				    &strict_overflow_p)) != 0)
	{
	  if (strict_overflow_p)
	    fold_overflow_warning (("assuming signed overflow does not occur "
				    "when simplifying modulus"),
				   WARN_STRICT_OVERFLOW_MISC);
	  return fold_convert_loc (loc, type, tem);
	}

      return NULL_TREE;

    case LROTATE_EXPR:
    case RROTATE_EXPR:
    case RSHIFT_EXPR:
    case LSHIFT_EXPR:
      /* Since negative shift count is not well-defined,
	 don't try to compute it in the compiler.  */
      if (TREE_CODE (arg1) == INTEGER_CST && tree_int_cst_sgn (arg1) < 0)
	return NULL_TREE;

      prec = element_precision (type);

      /* If we have a rotate of a bit operation with the rotate count and
	 the second operand of the bit operation both constant,
	 permute the two operations.  */
      if (code == RROTATE_EXPR && TREE_CODE (arg1) == INTEGER_CST
	  && (TREE_CODE (arg0) == BIT_AND_EXPR
	      || TREE_CODE (arg0) == BIT_IOR_EXPR
	      || TREE_CODE (arg0) == BIT_XOR_EXPR)
	  && TREE_CODE (TREE_OPERAND (arg0, 1)) == INTEGER_CST)
	{
	  tree arg00 = fold_convert_loc (loc, type, TREE_OPERAND (arg0, 0));
	  tree arg01 = fold_convert_loc (loc, type, TREE_OPERAND (arg0, 1));
	  return fold_build2_loc (loc, TREE_CODE (arg0), type,
				  fold_build2_loc (loc, code, type,
						   arg00, arg1),
				  fold_build2_loc (loc, code, type,
						   arg01, arg1));
	}

      /* Two consecutive rotates adding up to the some integer
	 multiple of the precision of the type can be ignored.  */
      if (code == RROTATE_EXPR && TREE_CODE (arg1) == INTEGER_CST
	  && TREE_CODE (arg0) == RROTATE_EXPR
	  && TREE_CODE (TREE_OPERAND (arg0, 1)) == INTEGER_CST
	  && wi::umod_trunc (wi::to_wide (arg1)
			     + wi::to_wide (TREE_OPERAND (arg0, 1)),
			     prec) == 0)
	return fold_convert_loc (loc, type, TREE_OPERAND (arg0, 0));

      return NULL_TREE;

    case MIN_EXPR:
    case MAX_EXPR:
      goto associate;

    case TRUTH_ANDIF_EXPR:
      /* Note that the operands of this must be ints
	 and their values must be 0 or 1.
	 ("true" is a fixed value perhaps depending on the language.)  */
      /* If first arg is constant zero, return it.  */
      if (integer_zerop (arg0))
	return fold_convert_loc (loc, type, arg0);
      /* FALLTHRU */
    case TRUTH_AND_EXPR:
      /* If either arg is constant true, drop it.  */
      if (TREE_CODE (arg0) == INTEGER_CST && ! integer_zerop (arg0))
	return non_lvalue_loc (loc, fold_convert_loc (loc, type, arg1));
      if (TREE_CODE (arg1) == INTEGER_CST && ! integer_zerop (arg1)
	  /* Preserve sequence points.  */
	  && (code != TRUTH_ANDIF_EXPR || ! TREE_SIDE_EFFECTS (arg0)))
	return non_lvalue_loc (loc, fold_convert_loc (loc, type, arg0));
      /* If second arg is constant zero, result is zero, but first arg
	 must be evaluated.  */
      if (integer_zerop (arg1))
	return omit_one_operand_loc (loc, type, arg1, arg0);
      /* Likewise for first arg, but note that only the TRUTH_AND_EXPR
	 case will be handled here.  */
      if (integer_zerop (arg0))
	return omit_one_operand_loc (loc, type, arg0, arg1);

      /* !X && X is always false.  */
      if (TREE_CODE (arg0) == TRUTH_NOT_EXPR
	  && operand_equal_p (TREE_OPERAND (arg0, 0), arg1, 0))
	return omit_one_operand_loc (loc, type, integer_zero_node, arg1);
      /* X && !X is always false.  */
      if (TREE_CODE (arg1) == TRUTH_NOT_EXPR
	  && operand_equal_p (arg0, TREE_OPERAND (arg1, 0), 0))
	return omit_one_operand_loc (loc, type, integer_zero_node, arg0);

      /* A < X && A + 1 > Y ==> A < X && A >= Y.  Normally A + 1 > Y
	 means A >= Y && A != MAX, but in this case we know that
	 A < X <= MAX.  */

      if (!TREE_SIDE_EFFECTS (arg0)
	  && !TREE_SIDE_EFFECTS (arg1))
	{
	  tem = fold_to_nonsharp_ineq_using_bound (loc, arg0, arg1);
	  if (tem && !operand_equal_p (tem, arg0, 0))
	    return fold_convert (type,
				 fold_build2_loc (loc, code, TREE_TYPE (arg1),
						  tem, arg1));

	  tem = fold_to_nonsharp_ineq_using_bound (loc, arg1, arg0);
	  if (tem && !operand_equal_p (tem, arg1, 0))
	    return fold_convert (type,
				 fold_build2_loc (loc, code, TREE_TYPE (arg0),
						  arg0, tem));
	}

      if ((tem = fold_truth_andor (loc, code, type, arg0, arg1, op0, op1))
          != NULL_TREE)
        return tem;

      return NULL_TREE;

    case TRUTH_ORIF_EXPR:
      /* Note that the operands of this must be ints
	 and their values must be 0 or true.
	 ("true" is a fixed value perhaps depending on the language.)  */
      /* If first arg is constant true, return it.  */
      if (TREE_CODE (arg0) == INTEGER_CST && ! integer_zerop (arg0))
	return fold_convert_loc (loc, type, arg0);
      /* FALLTHRU */
    case TRUTH_OR_EXPR:
      /* If either arg is constant zero, drop it.  */
      if (TREE_CODE (arg0) == INTEGER_CST && integer_zerop (arg0))
	return non_lvalue_loc (loc, fold_convert_loc (loc, type, arg1));
      if (TREE_CODE (arg1) == INTEGER_CST && integer_zerop (arg1)
	  /* Preserve sequence points.  */
	  && (code != TRUTH_ORIF_EXPR || ! TREE_SIDE_EFFECTS (arg0)))
	return non_lvalue_loc (loc, fold_convert_loc (loc, type, arg0));
      /* If second arg is constant true, result is true, but we must
	 evaluate first arg.  */
      if (TREE_CODE (arg1) == INTEGER_CST && ! integer_zerop (arg1))
	return omit_one_operand_loc (loc, type, arg1, arg0);
      /* Likewise for first arg, but note this only occurs here for
	 TRUTH_OR_EXPR.  */
      if (TREE_CODE (arg0) == INTEGER_CST && ! integer_zerop (arg0))
	return omit_one_operand_loc (loc, type, arg0, arg1);

      /* !X || X is always true.  */
      if (TREE_CODE (arg0) == TRUTH_NOT_EXPR
	  && operand_equal_p (TREE_OPERAND (arg0, 0), arg1, 0))
	return omit_one_operand_loc (loc, type, integer_one_node, arg1);
      /* X || !X is always true.  */
      if (TREE_CODE (arg1) == TRUTH_NOT_EXPR
	  && operand_equal_p (arg0, TREE_OPERAND (arg1, 0), 0))
	return omit_one_operand_loc (loc, type, integer_one_node, arg0);

      /* (X && !Y) || (!X && Y) is X ^ Y */
      if (TREE_CODE (arg0) == TRUTH_AND_EXPR
	  && TREE_CODE (arg1) == TRUTH_AND_EXPR)
        {
	  tree a0, a1, l0, l1, n0, n1;

	  a0 = fold_convert_loc (loc, type, TREE_OPERAND (arg1, 0));
	  a1 = fold_convert_loc (loc, type, TREE_OPERAND (arg1, 1));

	  l0 = fold_convert_loc (loc, type, TREE_OPERAND (arg0, 0));
	  l1 = fold_convert_loc (loc, type, TREE_OPERAND (arg0, 1));
	  
	  n0 = fold_build1_loc (loc, TRUTH_NOT_EXPR, type, l0);
	  n1 = fold_build1_loc (loc, TRUTH_NOT_EXPR, type, l1);
	  
	  if ((operand_equal_p (n0, a0, 0)
	       && operand_equal_p (n1, a1, 0))
	      || (operand_equal_p (n0, a1, 0)
		  && operand_equal_p (n1, a0, 0)))
	    return fold_build2_loc (loc, TRUTH_XOR_EXPR, type, l0, n1);
	}

      if ((tem = fold_truth_andor (loc, code, type, arg0, arg1, op0, op1))
          != NULL_TREE)
        return tem;

      return NULL_TREE;

    case TRUTH_XOR_EXPR:
      /* If the second arg is constant zero, drop it.  */
      if (integer_zerop (arg1))
	return non_lvalue_loc (loc, fold_convert_loc (loc, type, arg0));
      /* If the second arg is constant true, this is a logical inversion.  */
      if (integer_onep (arg1))
	{
	  tem = invert_truthvalue_loc (loc, arg0);
	  return non_lvalue_loc (loc, fold_convert_loc (loc, type, tem));
	}
      /* Identical arguments cancel to zero.  */
      if (operand_equal_p (arg0, arg1, 0))
	return omit_one_operand_loc (loc, type, integer_zero_node, arg0);

      /* !X ^ X is always true.  */
      if (TREE_CODE (arg0) == TRUTH_NOT_EXPR
	  && operand_equal_p (TREE_OPERAND (arg0, 0), arg1, 0))
	return omit_one_operand_loc (loc, type, integer_one_node, arg1);

      /* X ^ !X is always true.  */
      if (TREE_CODE (arg1) == TRUTH_NOT_EXPR
	  && operand_equal_p (arg0, TREE_OPERAND (arg1, 0), 0))
	return omit_one_operand_loc (loc, type, integer_one_node, arg0);

      return NULL_TREE;

    case EQ_EXPR:
    case NE_EXPR:
      STRIP_NOPS (arg0);
      STRIP_NOPS (arg1);

      tem = fold_comparison (loc, code, type, op0, op1);
      if (tem != NULL_TREE)
	return tem;

      /* bool_var != 1 becomes !bool_var. */
      if (TREE_CODE (TREE_TYPE (arg0)) == BOOLEAN_TYPE && integer_onep (arg1)
          && code == NE_EXPR)
        return fold_convert_loc (loc, type,
				 fold_build1_loc (loc, TRUTH_NOT_EXPR,
						  TREE_TYPE (arg0), arg0));

      /* bool_var == 0 becomes !bool_var. */
      if (TREE_CODE (TREE_TYPE (arg0)) == BOOLEAN_TYPE && integer_zerop (arg1)
          && code == EQ_EXPR)
        return fold_convert_loc (loc, type,
				 fold_build1_loc (loc, TRUTH_NOT_EXPR,
						  TREE_TYPE (arg0), arg0));

      /* !exp != 0 becomes !exp */
      if (TREE_CODE (arg0) == TRUTH_NOT_EXPR && integer_zerop (arg1)
	  && code == NE_EXPR)
        return non_lvalue_loc (loc, fold_convert_loc (loc, type, arg0));

      /* If this is an EQ or NE comparison with zero and ARG0 is
	 (1 << foo) & bar, convert it to (bar >> foo) & 1.  Both require
	 two operations, but the latter can be done in one less insn
	 on machines that have only two-operand insns or on which a
	 constant cannot be the first operand.  */
      if (TREE_CODE (arg0) == BIT_AND_EXPR
	  && integer_zerop (arg1))
	{
	  tree arg00 = TREE_OPERAND (arg0, 0);
	  tree arg01 = TREE_OPERAND (arg0, 1);
	  if (TREE_CODE (arg00) == LSHIFT_EXPR
	      && integer_onep (TREE_OPERAND (arg00, 0)))
	    {
	      tree tem = fold_build2_loc (loc, RSHIFT_EXPR, TREE_TYPE (arg00),
					  arg01, TREE_OPERAND (arg00, 1));
	      tem = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (arg0), tem,
				     build_one_cst (TREE_TYPE (arg0)));
	      return fold_build2_loc (loc, code, type,
				      fold_convert_loc (loc, TREE_TYPE (arg1),
							tem), arg1);
	    }
	  else if (TREE_CODE (arg01) == LSHIFT_EXPR
		   && integer_onep (TREE_OPERAND (arg01, 0)))
	    {
	      tree tem = fold_build2_loc (loc, RSHIFT_EXPR, TREE_TYPE (arg01),
					  arg00, TREE_OPERAND (arg01, 1));
	      tem = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (arg0), tem,
				     build_one_cst (TREE_TYPE (arg0)));
	      return fold_build2_loc (loc, code, type,
				      fold_convert_loc (loc, TREE_TYPE (arg1),
							tem), arg1);
	    }
	}

      /* If this is a comparison of a field, we may be able to simplify it.  */
      if ((TREE_CODE (arg0) == COMPONENT_REF
	   || TREE_CODE (arg0) == BIT_FIELD_REF)
	  /* Handle the constant case even without -O
	     to make sure the warnings are given.  */
	  && (optimize || TREE_CODE (arg1) == INTEGER_CST))
	{
	  t1 = optimize_bit_field_compare (loc, code, type, arg0, arg1);
	  if (t1)
	    return t1;
	}

      /* Optimize comparisons of strlen vs zero to a compare of the
	 first character of the string vs zero.  To wit,
		strlen(ptr) == 0   =>  *ptr == 0
		strlen(ptr) != 0   =>  *ptr != 0
	 Other cases should reduce to one of these two (or a constant)
	 due to the return value of strlen being unsigned.  */
      if (TREE_CODE (arg0) == CALL_EXPR && integer_zerop (arg1))
	{
	  tree fndecl = get_callee_fndecl (arg0);

	  if (fndecl
	      && fndecl_built_in_p (fndecl, BUILT_IN_STRLEN)
	      && call_expr_nargs (arg0) == 1
	      && (TREE_CODE (TREE_TYPE (CALL_EXPR_ARG (arg0, 0)))
		  == POINTER_TYPE))
	    {
	      tree ptrtype
		= build_pointer_type (build_qualified_type (char_type_node,
							    TYPE_QUAL_CONST));
	      tree ptr = fold_convert_loc (loc, ptrtype,
					   CALL_EXPR_ARG (arg0, 0));
	      tree iref = build_fold_indirect_ref_loc (loc, ptr);
	      return fold_build2_loc (loc, code, type, iref,
				      build_int_cst (TREE_TYPE (iref), 0));
	    }
	}

      /* Fold (X >> C) != 0 into X < 0 if C is one less than the width
	 of X.  Similarly fold (X >> C) == 0 into X >= 0.  */
      if (TREE_CODE (arg0) == RSHIFT_EXPR
	  && integer_zerop (arg1)
	  && TREE_CODE (TREE_OPERAND (arg0, 1)) == INTEGER_CST)
	{
	  tree arg00 = TREE_OPERAND (arg0, 0);
	  tree arg01 = TREE_OPERAND (arg0, 1);
	  tree itype = TREE_TYPE (arg00);
	  if (wi::to_wide (arg01) == element_precision (itype) - 1)
	    {
	      if (TYPE_UNSIGNED (itype))
		{
		  itype = signed_type_for (itype);
		  arg00 = fold_convert_loc (loc, itype, arg00);
		}
	      return fold_build2_loc (loc, code == EQ_EXPR ? GE_EXPR : LT_EXPR,
				  type, arg00, build_zero_cst (itype));
	    }
	}

      /* Fold (~X & C) == 0 into (X & C) != 0 and (~X & C) != 0 into
	 (X & C) == 0 when C is a single bit.  */
      if (TREE_CODE (arg0) == BIT_AND_EXPR
	  && TREE_CODE (TREE_OPERAND (arg0, 0)) == BIT_NOT_EXPR
	  && integer_zerop (arg1)
	  && integer_pow2p (TREE_OPERAND (arg0, 1)))
	{
	  tem = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (arg0),
				 TREE_OPERAND (TREE_OPERAND (arg0, 0), 0),
				 TREE_OPERAND (arg0, 1));
	  return fold_build2_loc (loc, code == EQ_EXPR ? NE_EXPR : EQ_EXPR,
				  type, tem,
				  fold_convert_loc (loc, TREE_TYPE (arg0),
						    arg1));
	}

      /* Fold ((X & C) ^ C) eq/ne 0 into (X & C) ne/eq 0, when the
	 constant C is a power of two, i.e. a single bit.  */
      if (TREE_CODE (arg0) == BIT_XOR_EXPR
	  && TREE_CODE (TREE_OPERAND (arg0, 0)) == BIT_AND_EXPR
	  && integer_zerop (arg1)
	  && integer_pow2p (TREE_OPERAND (arg0, 1))
	  && operand_equal_p (TREE_OPERAND (TREE_OPERAND (arg0, 0), 1),
			      TREE_OPERAND (arg0, 1), OEP_ONLY_CONST))
	{
	  tree arg00 = TREE_OPERAND (arg0, 0);
	  return fold_build2_loc (loc, code == EQ_EXPR ? NE_EXPR : EQ_EXPR, type,
			      arg00, build_int_cst (TREE_TYPE (arg00), 0));
	}

      /* Likewise, fold ((X ^ C) & C) eq/ne 0 into (X & C) ne/eq 0,
	 when is C is a power of two, i.e. a single bit.  */
      if (TREE_CODE (arg0) == BIT_AND_EXPR
	  && TREE_CODE (TREE_OPERAND (arg0, 0)) == BIT_XOR_EXPR
	  && integer_zerop (arg1)
	  && integer_pow2p (TREE_OPERAND (arg0, 1))
	  && operand_equal_p (TREE_OPERAND (TREE_OPERAND (arg0, 0), 1),
			      TREE_OPERAND (arg0, 1), OEP_ONLY_CONST))
	{
	  tree arg000 = TREE_OPERAND (TREE_OPERAND (arg0, 0), 0);
	  tem = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (arg000),
			     arg000, TREE_OPERAND (arg0, 1));
	  return fold_build2_loc (loc, code == EQ_EXPR ? NE_EXPR : EQ_EXPR, type,
			      tem, build_int_cst (TREE_TYPE (tem), 0));
	}

      if (TREE_CODE (arg0) == BIT_XOR_EXPR
	  && TREE_CODE (arg1) == BIT_XOR_EXPR)
	{
	  tree arg00 = TREE_OPERAND (arg0, 0);
	  tree arg01 = TREE_OPERAND (arg0, 1);
	  tree arg10 = TREE_OPERAND (arg1, 0);
	  tree arg11 = TREE_OPERAND (arg1, 1);
	  tree itype = TREE_TYPE (arg0);

	  /* Optimize (X ^ Z) op (Y ^ Z) as X op Y, and symmetries.
	     operand_equal_p guarantees no side-effects so we don't need
	     to use omit_one_operand on Z.  */
	  if (operand_equal_p (arg01, arg11, 0))
	    return fold_build2_loc (loc, code, type, arg00,
				    fold_convert_loc (loc, TREE_TYPE (arg00),
						      arg10));
	  if (operand_equal_p (arg01, arg10, 0))
	    return fold_build2_loc (loc, code, type, arg00,
				    fold_convert_loc (loc, TREE_TYPE (arg00),
						      arg11));
	  if (operand_equal_p (arg00, arg11, 0))
	    return fold_build2_loc (loc, code, type, arg01,
				    fold_convert_loc (loc, TREE_TYPE (arg01),
						      arg10));
	  if (operand_equal_p (arg00, arg10, 0))
	    return fold_build2_loc (loc, code, type, arg01,
				    fold_convert_loc (loc, TREE_TYPE (arg01),
						      arg11));

	  /* Optimize (X ^ C1) op (Y ^ C2) as (X ^ (C1 ^ C2)) op Y.  */
	  if (TREE_CODE (arg01) == INTEGER_CST
	      && TREE_CODE (arg11) == INTEGER_CST)
	    {
	      tem = fold_build2_loc (loc, BIT_XOR_EXPR, itype, arg01,
				     fold_convert_loc (loc, itype, arg11));
	      tem = fold_build2_loc (loc, BIT_XOR_EXPR, itype, arg00, tem);
	      return fold_build2_loc (loc, code, type, tem,
				      fold_convert_loc (loc, itype, arg10));
	    }
	}

      /* Attempt to simplify equality/inequality comparisons of complex
	 values.  Only lower the comparison if the result is known or
	 can be simplified to a single scalar comparison.  */
      if ((TREE_CODE (arg0) == COMPLEX_EXPR
	   || TREE_CODE (arg0) == COMPLEX_CST)
	  && (TREE_CODE (arg1) == COMPLEX_EXPR
	      || TREE_CODE (arg1) == COMPLEX_CST))
	{
	  tree real0, imag0, real1, imag1;
	  tree rcond, icond;

	  if (TREE_CODE (arg0) == COMPLEX_EXPR)
	    {
	      real0 = TREE_OPERAND (arg0, 0);
	      imag0 = TREE_OPERAND (arg0, 1);
	    }
	  else
	    {
	      real0 = TREE_REALPART (arg0);
	      imag0 = TREE_IMAGPART (arg0);
	    }

	  if (TREE_CODE (arg1) == COMPLEX_EXPR)
	    {
	      real1 = TREE_OPERAND (arg1, 0);
	      imag1 = TREE_OPERAND (arg1, 1);
	    }
	  else
	    {
	      real1 = TREE_REALPART (arg1);
	      imag1 = TREE_IMAGPART (arg1);
	    }

	  rcond = fold_binary_loc (loc, code, type, real0, real1);
	  if (rcond && TREE_CODE (rcond) == INTEGER_CST)
	    {
	      if (integer_zerop (rcond))
		{
		  if (code == EQ_EXPR)
		    return omit_two_operands_loc (loc, type, boolean_false_node,
					      imag0, imag1);
		  return fold_build2_loc (loc, NE_EXPR, type, imag0, imag1);
		}
	      else
		{
		  if (code == NE_EXPR)
		    return omit_two_operands_loc (loc, type, boolean_true_node,
					      imag0, imag1);
		  return fold_build2_loc (loc, EQ_EXPR, type, imag0, imag1);
		}
	    }

	  icond = fold_binary_loc (loc, code, type, imag0, imag1);
	  if (icond && TREE_CODE (icond) == INTEGER_CST)
	    {
	      if (integer_zerop (icond))
		{
		  if (code == EQ_EXPR)
		    return omit_two_operands_loc (loc, type, boolean_false_node,
					      real0, real1);
		  return fold_build2_loc (loc, NE_EXPR, type, real0, real1);
		}
	      else
		{
		  if (code == NE_EXPR)
		    return omit_two_operands_loc (loc, type, boolean_true_node,
					      real0, real1);
		  return fold_build2_loc (loc, EQ_EXPR, type, real0, real1);
		}
	    }
	}

      return NULL_TREE;

    case LT_EXPR:
    case GT_EXPR:
    case LE_EXPR:
    case GE_EXPR:
      tem = fold_comparison (loc, code, type, op0, op1);
      if (tem != NULL_TREE)
	return tem;

      /* Transform comparisons of the form X +- C CMP X.  */
      if ((TREE_CODE (arg0) == PLUS_EXPR || TREE_CODE (arg0) == MINUS_EXPR)
	  && operand_equal_p (TREE_OPERAND (arg0, 0), arg1, 0)
	  && TREE_CODE (TREE_OPERAND (arg0, 1)) == REAL_CST
	  && !HONOR_SNANS (arg0))
	{
	  tree arg01 = TREE_OPERAND (arg0, 1);
	  enum tree_code code0 = TREE_CODE (arg0);
	  int is_positive = REAL_VALUE_NEGATIVE (TREE_REAL_CST (arg01)) ? -1 : 1;

	  /* (X - c) > X becomes false.  */
	  if (code == GT_EXPR
	      && ((code0 == MINUS_EXPR && is_positive >= 0)
		  || (code0 == PLUS_EXPR && is_positive <= 0)))
	    return constant_boolean_node (0, type);

	  /* Likewise (X + c) < X becomes false.  */
	  if (code == LT_EXPR
	      && ((code0 == PLUS_EXPR && is_positive >= 0)
		  || (code0 == MINUS_EXPR && is_positive <= 0)))
	    return constant_boolean_node (0, type);

	  /* Convert (X - c) <= X to true.  */
	  if (!HONOR_NANS (arg1)
	      && code == LE_EXPR
	      && ((code0 == MINUS_EXPR && is_positive >= 0)
		  || (code0 == PLUS_EXPR && is_positive <= 0)))
	    return constant_boolean_node (1, type);

	  /* Convert (X + c) >= X to true.  */
	  if (!HONOR_NANS (arg1)
	      && code == GE_EXPR
	      && ((code0 == PLUS_EXPR && is_positive >= 0)
		  || (code0 == MINUS_EXPR && is_positive <= 0)))
	    return constant_boolean_node (1, type);
	}

      /* If we are comparing an ABS_EXPR with a constant, we can
	 convert all the cases into explicit comparisons, but they may
	 well not be faster than doing the ABS and one comparison.
	 But ABS (X) <= C is a range comparison, which becomes a subtraction
	 and a comparison, and is probably faster.  */
      if (code == LE_EXPR
	  && TREE_CODE (arg1) == INTEGER_CST
	  && TREE_CODE (arg0) == ABS_EXPR
	  && ! TREE_SIDE_EFFECTS (arg0)
	  && (tem = negate_expr (arg1)) != 0
	  && TREE_CODE (tem) == INTEGER_CST
	  && !TREE_OVERFLOW (tem))
	return fold_build2_loc (loc, TRUTH_ANDIF_EXPR, type,
			    build2 (GE_EXPR, type,
				    TREE_OPERAND (arg0, 0), tem),
			    build2 (LE_EXPR, type,
				    TREE_OPERAND (arg0, 0), arg1));

      /* Convert ABS_EXPR<x> >= 0 to true.  */
      strict_overflow_p = false;
      if (code == GE_EXPR
	  && (integer_zerop (arg1)
	      || (! HONOR_NANS (arg0)
		  && real_zerop (arg1)))
	  && tree_expr_nonnegative_warnv_p (arg0, &strict_overflow_p))
	{
	  if (strict_overflow_p)
	    fold_overflow_warning (("assuming signed overflow does not occur "
				    "when simplifying comparison of "
				    "absolute value and zero"),
				   WARN_STRICT_OVERFLOW_CONDITIONAL);
	  return omit_one_operand_loc (loc, type,
				       constant_boolean_node (true, type),
				       arg0);
	}

      /* Convert ABS_EXPR<x> < 0 to false.  */
      strict_overflow_p = false;
      if (code == LT_EXPR
	  && (integer_zerop (arg1) || real_zerop (arg1))
	  && tree_expr_nonnegative_warnv_p (arg0, &strict_overflow_p))
	{
	  if (strict_overflow_p)
	    fold_overflow_warning (("assuming signed overflow does not occur "
				    "when simplifying comparison of "
				    "absolute value and zero"),
				   WARN_STRICT_OVERFLOW_CONDITIONAL);
	  return omit_one_operand_loc (loc, type,
				       constant_boolean_node (false, type),
				       arg0);
	}

      /* If X is unsigned, convert X < (1 << Y) into X >> Y == 0
	 and similarly for >= into !=.  */
      if ((code == LT_EXPR || code == GE_EXPR)
	  && TYPE_UNSIGNED (TREE_TYPE (arg0))
	  && TREE_CODE (arg1) == LSHIFT_EXPR
	  && integer_onep (TREE_OPERAND (arg1, 0)))
	return build2_loc (loc, code == LT_EXPR ? EQ_EXPR : NE_EXPR, type,
			   build2 (RSHIFT_EXPR, TREE_TYPE (arg0), arg0,
				   TREE_OPERAND (arg1, 1)),
			   build_zero_cst (TREE_TYPE (arg0)));

      /* Similarly for X < (cast) (1 << Y).  But cast can't be narrowing,
	 otherwise Y might be >= # of bits in X's type and thus e.g.
	 (unsigned char) (1 << Y) for Y 15 might be 0.
	 If the cast is widening, then 1 << Y should have unsigned type,
	 otherwise if Y is number of bits in the signed shift type minus 1,
	 we can't optimize this.  E.g. (unsigned long long) (1 << Y) for Y
	 31 might be 0xffffffff80000000.  */
      if ((code == LT_EXPR || code == GE_EXPR)
	  && (INTEGRAL_TYPE_P (TREE_TYPE (arg0))
	      || VECTOR_INTEGER_TYPE_P (TREE_TYPE (arg0)))
	  && TYPE_UNSIGNED (TREE_TYPE (arg0))
	  && CONVERT_EXPR_P (arg1)
	  && TREE_CODE (TREE_OPERAND (arg1, 0)) == LSHIFT_EXPR
	  && (element_precision (TREE_TYPE (arg1))
	      >= element_precision (TREE_TYPE (TREE_OPERAND (arg1, 0))))
	  && (TYPE_UNSIGNED (TREE_TYPE (TREE_OPERAND (arg1, 0)))
	      || (element_precision (TREE_TYPE (arg1))
		  == element_precision (TREE_TYPE (TREE_OPERAND (arg1, 0)))))
	  && integer_onep (TREE_OPERAND (TREE_OPERAND (arg1, 0), 0)))
	{
	  tem = build2 (RSHIFT_EXPR, TREE_TYPE (arg0), arg0,
			TREE_OPERAND (TREE_OPERAND (arg1, 0), 1));
	  return build2_loc (loc, code == LT_EXPR ? EQ_EXPR : NE_EXPR, type,
			     fold_convert_loc (loc, TREE_TYPE (arg0), tem),
			     build_zero_cst (TREE_TYPE (arg0)));
	}

      return NULL_TREE;

    case UNORDERED_EXPR:
    case ORDERED_EXPR:
    case UNLT_EXPR:
    case UNLE_EXPR:
    case UNGT_EXPR:
    case UNGE_EXPR:
    case UNEQ_EXPR:
    case LTGT_EXPR:
      /* Fold (double)float1 CMP (double)float2 into float1 CMP float2.  */
      {
	tree targ0 = strip_float_extensions (arg0);
	tree targ1 = strip_float_extensions (arg1);
	tree newtype = TREE_TYPE (targ0);

	if (element_precision (TREE_TYPE (targ1)) > element_precision (newtype))
	  newtype = TREE_TYPE (targ1);

	if (element_precision (newtype) < element_precision (TREE_TYPE (arg0))
	    && (!VECTOR_TYPE_P (type) || is_truth_type_for (newtype, type)))
	  return fold_build2_loc (loc, code, type,
			      fold_convert_loc (loc, newtype, targ0),
			      fold_convert_loc (loc, newtype, targ1));
      }

      return NULL_TREE;

    case COMPOUND_EXPR:
      /* When pedantic, a compound expression can be neither an lvalue
	 nor an integer constant expression.  */
      if (TREE_SIDE_EFFECTS (arg0) || TREE_CONSTANT (arg1))
	return NULL_TREE;
      /* Don't let (0, 0) be null pointer constant.  */
      tem = integer_zerop (arg1) ? build1_loc (loc, NOP_EXPR, type, arg1)
				 : fold_convert_loc (loc, type, arg1);
      return tem;

    default:
      return NULL_TREE;
    } /* switch (code) */
}

/* For constants M and N, if M == (1LL << cst) - 1 && (N & M) == M,
   ((A & N) + B) & M -> (A + B) & M
   Similarly if (N & M) == 0,
   ((A | N) + B) & M -> (A + B) & M
   and for - instead of + (or unary - instead of +)
   and/or ^ instead of |.
   If B is constant and (B & M) == 0, fold into A & M.

   This function is a helper for match.pd patterns.  Return non-NULL
   type in which the simplified operation should be performed only
   if any optimization is possible.

   ARG1 is M above, ARG00 is left operand of +/-, if CODE00 is BIT_*_EXPR,
   then ARG00{0,1} are operands of that bitop, otherwise CODE00 is ERROR_MARK.
   Similarly for ARG01, CODE01 and ARG01{0,1}, just for the right operand of
   +/-.  */
tree
fold_bit_and_mask (tree type, tree arg1, enum tree_code code,
		   tree arg00, enum tree_code code00, tree arg000, tree arg001,
		   tree arg01, enum tree_code code01, tree arg010, tree arg011,
		   tree *pmop)
{
  gcc_assert (TREE_CODE (arg1) == INTEGER_CST);
  gcc_assert (code == PLUS_EXPR || code == MINUS_EXPR || code == NEGATE_EXPR);
  wi::tree_to_wide_ref cst1 = wi::to_wide (arg1);
  if (~cst1 == 0
      || (cst1 & (cst1 + 1)) != 0
      || !INTEGRAL_TYPE_P (type)
      || (!TYPE_OVERFLOW_WRAPS (type)
	  && TREE_CODE (type) != INTEGER_TYPE)
      || (wi::max_value (type) & cst1) != cst1)
    return NULL_TREE;

  enum tree_code codes[2] = { code00, code01 };
  tree arg0xx[4] = { arg000, arg001, arg010, arg011 };
  int which = 0;
  wide_int cst0;

  /* Now we know that arg0 is (C + D) or (C - D) or -C and
     arg1 (M) is == (1LL << cst) - 1.
     Store C into PMOP[0] and D into PMOP[1].  */
  pmop[0] = arg00;
  pmop[1] = arg01;
  which = code != NEGATE_EXPR;

  for (; which >= 0; which--)
    switch (codes[which])
      {
      case BIT_AND_EXPR:
      case BIT_IOR_EXPR:
      case BIT_XOR_EXPR:
	gcc_assert (TREE_CODE (arg0xx[2 * which + 1]) == INTEGER_CST);
	cst0 = wi::to_wide (arg0xx[2 * which + 1]) & cst1;
	if (codes[which] == BIT_AND_EXPR)
	  {
	    if (cst0 != cst1)
	      break;
	  }
	else if (cst0 != 0)
	  break;
	/* If C or D is of the form (A & N) where
	   (N & M) == M, or of the form (A | N) or
	   (A ^ N) where (N & M) == 0, replace it with A.  */
	pmop[which] = arg0xx[2 * which];
	break;
      case ERROR_MARK:
	if (TREE_CODE (pmop[which]) != INTEGER_CST)
	  break;
	/* If C or D is a N where (N & M) == 0, it can be
	   omitted (replaced with 0).  */
	if ((code == PLUS_EXPR
	     || (code == MINUS_EXPR && which == 0))
	    && (cst1 & wi::to_wide (pmop[which])) == 0)
	  pmop[which] = build_int_cst (type, 0);
	/* Similarly, with C - N where (-N & M) == 0.  */
	if (code == MINUS_EXPR
	    && which == 1
	    && (cst1 & -wi::to_wide (pmop[which])) == 0)
	  pmop[which] = build_int_cst (type, 0);
	break;
      default:
	gcc_unreachable ();
      }

  /* Only build anything new if we optimized one or both arguments above.  */
  if (pmop[0] == arg00 && pmop[1] == arg01)
    return NULL_TREE;

  if (TYPE_OVERFLOW_WRAPS (type))
    return type;
  else
    return unsigned_type_for (type);
}

/* Used by contains_label_[p1].  */

struct contains_label_data
{
  hash_set<tree> *pset;
  bool inside_switch_p;
};

/* Callback for walk_tree, looking for LABEL_EXPR.  Return *TP if it is
   a LABEL_EXPR or CASE_LABEL_EXPR not inside of another SWITCH_EXPR; otherwise
   return NULL_TREE.  Do not check the subtrees of GOTO_EXPR.  */

static tree
contains_label_1 (tree *tp, int *walk_subtrees, void *data)
{
  contains_label_data *d = (contains_label_data *) data;
  switch (TREE_CODE (*tp))
    {
    case LABEL_EXPR:
      return *tp;

    case CASE_LABEL_EXPR:
      if (!d->inside_switch_p)
	return *tp;
      return NULL_TREE;

    case SWITCH_EXPR:
      if (!d->inside_switch_p)
	{
	  if (walk_tree (&SWITCH_COND (*tp), contains_label_1, data, d->pset))
	    return *tp;
	  d->inside_switch_p = true;
	  if (walk_tree (&SWITCH_BODY (*tp), contains_label_1, data, d->pset))
	    return *tp;
	  d->inside_switch_p = false;
	  *walk_subtrees = 0;
	}
      return NULL_TREE;

    case GOTO_EXPR:
      *walk_subtrees = 0;
      return NULL_TREE;

    default:
      return NULL_TREE;
    }
}

/* Return whether the sub-tree ST contains a label which is accessible from
   outside the sub-tree.  */

static bool
contains_label_p (tree st)
{
  hash_set<tree> pset;
  contains_label_data data = { &pset, false };
  return walk_tree (&st, contains_label_1, &data, &pset) != NULL_TREE;
}

/* Fold a ternary expression of code CODE and type TYPE with operands
   OP0, OP1, and OP2.  Return the folded expression if folding is
   successful.  Otherwise, return NULL_TREE.  */

tree
fold_ternary_loc (location_t loc, enum tree_code code, tree type,
		  tree op0, tree op1, tree op2)
{
  tree tem;
  tree arg0 = NULL_TREE, arg1 = NULL_TREE, arg2 = NULL_TREE;
  enum tree_code_class kind = TREE_CODE_CLASS (code);

  gcc_assert (IS_EXPR_CODE_CLASS (kind)
	      && TREE_CODE_LENGTH (code) == 3);

  /* If this is a commutative operation, and OP0 is a constant, move it
     to OP1 to reduce the number of tests below.  */
  if (commutative_ternary_tree_code (code)
      && tree_swap_operands_p (op0, op1))
    return fold_build3_loc (loc, code, type, op1, op0, op2);

  tem = generic_simplify (loc, code, type, op0, op1, op2);
  if (tem)
    return tem;

  /* Strip any conversions that don't change the mode.  This is safe
     for every expression, except for a comparison expression because
     its signedness is derived from its operands.  So, in the latter
     case, only strip conversions that don't change the signedness.

     Note that this is done as an internal manipulation within the
     constant folder, in order to find the simplest representation of
     the arguments so that their form can be studied.  In any cases,
     the appropriate type conversions should be put back in the tree
     that will get out of the constant folder.  */
  if (op0)
    {
      arg0 = op0;
      STRIP_NOPS (arg0);
    }

  if (op1)
    {
      arg1 = op1;
      STRIP_NOPS (arg1);
    }

  if (op2)
    {
      arg2 = op2;
      STRIP_NOPS (arg2);
    }

  switch (code)
    {
    case COMPONENT_REF:
      if (TREE_CODE (arg0) == CONSTRUCTOR
	  && ! type_contains_placeholder_p (TREE_TYPE (arg0)))
	{
	  unsigned HOST_WIDE_INT idx;
	  tree field, value;
	  FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (arg0), idx, field, value)
	    if (field == arg1)
	      return value;
	}
      return NULL_TREE;

    case COND_EXPR:
    case VEC_COND_EXPR:
      /* Pedantic ANSI C says that a conditional expression is never an lvalue,
	 so all simple results must be passed through pedantic_non_lvalue.  */
      if (TREE_CODE (arg0) == INTEGER_CST)
	{
	  tree unused_op = integer_zerop (arg0) ? op1 : op2;
	  tem = integer_zerop (arg0) ? op2 : op1;
	  /* Only optimize constant conditions when the selected branch
	     has the same type as the COND_EXPR.  This avoids optimizing
             away "c ? x : throw", where the throw has a void type.
             Avoid throwing away that operand which contains label.  */
          if ((!TREE_SIDE_EFFECTS (unused_op)
               || !contains_label_p (unused_op))
              && (! VOID_TYPE_P (TREE_TYPE (tem))
                  || VOID_TYPE_P (type)))
	    return protected_set_expr_location_unshare (tem, loc);
	  return NULL_TREE;
	}
      else if (TREE_CODE (arg0) == VECTOR_CST)
	{
	  unsigned HOST_WIDE_INT nelts;
	  if ((TREE_CODE (arg1) == VECTOR_CST
	       || TREE_CODE (arg1) == CONSTRUCTOR)
	      && (TREE_CODE (arg2) == VECTOR_CST
		  || TREE_CODE (arg2) == CONSTRUCTOR)
	      && TYPE_VECTOR_SUBPARTS (type).is_constant (&nelts))
	    {
	      vec_perm_builder sel (nelts, nelts, 1);
	      for (unsigned int i = 0; i < nelts; i++)
		{
		  tree val = VECTOR_CST_ELT (arg0, i);
		  if (integer_all_onesp (val))
		    sel.quick_push (i);
		  else if (integer_zerop (val))
		    sel.quick_push (nelts + i);
		  else /* Currently unreachable.  */
		    return NULL_TREE;
		}
	      vec_perm_indices indices (sel, 2, nelts);
	      tree t = fold_vec_perm (type, arg1, arg2, indices);
	      if (t != NULL_TREE)
		return t;
	    }
	}

      /* If we have A op B ? A : C, we may be able to convert this to a
	 simpler expression, depending on the operation and the values
	 of B and C.  Signed zeros prevent all of these transformations,
	 for reasons given above each one.

         Also try swapping the arguments and inverting the conditional.  */
      if (COMPARISON_CLASS_P (arg0)
	  && operand_equal_for_comparison_p (TREE_OPERAND (arg0, 0), op1)
	  && !HONOR_SIGNED_ZEROS (op1))
	{
	  tem = fold_cond_expr_with_comparison (loc, type, TREE_CODE (arg0),
						TREE_OPERAND (arg0, 0),
						TREE_OPERAND (arg0, 1),
						op1, op2);
	  if (tem)
	    return tem;
	}

      if (COMPARISON_CLASS_P (arg0)
	  && operand_equal_for_comparison_p (TREE_OPERAND (arg0, 0), op2)
	  && !HONOR_SIGNED_ZEROS (op2))
	{
	  enum tree_code comp_code = TREE_CODE (arg0);
	  tree arg00 = TREE_OPERAND (arg0, 0);
	  tree arg01 = TREE_OPERAND (arg0, 1);
	  comp_code = invert_tree_comparison (comp_code, HONOR_NANS (arg00));
	  if (comp_code != ERROR_MARK)
	    tem = fold_cond_expr_with_comparison (loc, type, comp_code,
						  arg00,
						  arg01,
						  op2, op1);
	  if (tem)
	    return tem;
	}

      /* If the second operand is simpler than the third, swap them
	 since that produces better jump optimization results.  */
      if (truth_value_p (TREE_CODE (arg0))
	  && tree_swap_operands_p (op1, op2))
	{
	  location_t loc0 = expr_location_or (arg0, loc);
	  /* See if this can be inverted.  If it can't, possibly because
	     it was a floating-point inequality comparison, don't do
	     anything.  */
	  tem = fold_invert_truthvalue (loc0, arg0);
	  if (tem)
	    return fold_build3_loc (loc, code, type, tem, op2, op1);
	}

      /* Convert A ? 1 : 0 to simply A.  */
      if ((code == VEC_COND_EXPR ? integer_all_onesp (op1)
				 : (integer_onep (op1)
				    && !VECTOR_TYPE_P (type)))
	  && integer_zerop (op2)
	  /* If we try to convert OP0 to our type, the
	     call to fold will try to move the conversion inside
	     a COND, which will recurse.  In that case, the COND_EXPR
	     is probably the best choice, so leave it alone.  */
	  && type == TREE_TYPE (arg0))
	return protected_set_expr_location_unshare (arg0, loc);

      /* Convert A ? 0 : 1 to !A.  This prefers the use of NOT_EXPR
	 over COND_EXPR in cases such as floating point comparisons.  */
      if (integer_zerop (op1)
	  && code == COND_EXPR
	  && integer_onep (op2)
	  && !VECTOR_TYPE_P (type)
	  && truth_value_p (TREE_CODE (arg0)))
	return fold_convert_loc (loc, type,
				 invert_truthvalue_loc (loc, arg0));

      /* A < 0 ? <sign bit of A> : 0 is simply (A & <sign bit of A>).  */
      if (TREE_CODE (arg0) == LT_EXPR
	  && integer_zerop (TREE_OPERAND (arg0, 1))
	  && integer_zerop (op2)
	  && (tem = sign_bit_p (TREE_OPERAND (arg0, 0), arg1)))
	{
	  /* sign_bit_p looks through both zero and sign extensions,
	     but for this optimization only sign extensions are
	     usable.  */
	  tree tem2 = TREE_OPERAND (arg0, 0);
	  while (tem != tem2)
	    {
	      if (TREE_CODE (tem2) != NOP_EXPR
		  || TYPE_UNSIGNED (TREE_TYPE (TREE_OPERAND (tem2, 0))))
		{
		  tem = NULL_TREE;
		  break;
		}
	      tem2 = TREE_OPERAND (tem2, 0);
	    }
	  /* sign_bit_p only checks ARG1 bits within A's precision.
	     If <sign bit of A> has wider type than A, bits outside
	     of A's precision in <sign bit of A> need to be checked.
	     If they are all 0, this optimization needs to be done
	     in unsigned A's type, if they are all 1 in signed A's type,
	     otherwise this can't be done.  */
	  if (tem
	      && TYPE_PRECISION (TREE_TYPE (tem))
		 < TYPE_PRECISION (TREE_TYPE (arg1))
	      && TYPE_PRECISION (TREE_TYPE (tem))
		 < TYPE_PRECISION (type))
	    {
	      int inner_width, outer_width;
	      tree tem_type;

	      inner_width = TYPE_PRECISION (TREE_TYPE (tem));
	      outer_width = TYPE_PRECISION (TREE_TYPE (arg1));
	      if (outer_width > TYPE_PRECISION (type))
		outer_width = TYPE_PRECISION (type);

	      wide_int mask = wi::shifted_mask
		(inner_width, outer_width - inner_width, false,
		 TYPE_PRECISION (TREE_TYPE (arg1)));

	      wide_int common = mask & wi::to_wide (arg1);
	      if (common == mask)
		{
		  tem_type = signed_type_for (TREE_TYPE (tem));
		  tem = fold_convert_loc (loc, tem_type, tem);
		}
	      else if (common == 0)
		{
		  tem_type = unsigned_type_for (TREE_TYPE (tem));
		  tem = fold_convert_loc (loc, tem_type, tem);
		}
	      else
		tem = NULL;
	    }

	  if (tem)
	    return
	      fold_convert_loc (loc, type,
				fold_build2_loc (loc, BIT_AND_EXPR,
					     TREE_TYPE (tem), tem,
					     fold_convert_loc (loc,
							       TREE_TYPE (tem),
							       arg1)));
	}

      /* (A >> N) & 1 ? (1 << N) : 0 is simply A & (1 << N).  A & 1 was
	 already handled above.  */
      if (TREE_CODE (arg0) == BIT_AND_EXPR
	  && integer_onep (TREE_OPERAND (arg0, 1))
	  && integer_zerop (op2)
	  && integer_pow2p (arg1))
	{
	  tree tem = TREE_OPERAND (arg0, 0);
	  STRIP_NOPS (tem);
	  if (TREE_CODE (tem) == RSHIFT_EXPR
	      && tree_fits_uhwi_p (TREE_OPERAND (tem, 1))
              && (unsigned HOST_WIDE_INT) tree_log2 (arg1)
		 == tree_to_uhwi (TREE_OPERAND (tem, 1)))
	    return fold_build2_loc (loc, BIT_AND_EXPR, type,
				    fold_convert_loc (loc, type,
						      TREE_OPERAND (tem, 0)),
				    op1);
	}

      /* A & N ? N : 0 is simply A & N if N is a power of two.  This
	 is probably obsolete because the first operand should be a
	 truth value (that's why we have the two cases above), but let's
	 leave it in until we can confirm this for all front-ends.  */
      if (integer_zerop (op2)
	  && TREE_CODE (arg0) == NE_EXPR
	  && integer_zerop (TREE_OPERAND (arg0, 1))
	  && integer_pow2p (arg1)
	  && TREE_CODE (TREE_OPERAND (arg0, 0)) == BIT_AND_EXPR
	  && operand_equal_p (TREE_OPERAND (TREE_OPERAND (arg0, 0), 1),
			      arg1, OEP_ONLY_CONST)
	  /* operand_equal_p compares just value, not precision, so e.g.
	     arg1 could be 8-bit -128 and be power of two, but BIT_AND_EXPR
	     second operand 32-bit -128, which is not a power of two (or vice
	     versa.  */
	  && integer_pow2p (TREE_OPERAND (TREE_OPERAND (arg0, 0), 1)))
	return fold_convert_loc (loc, type, TREE_OPERAND (arg0, 0));

      /* Disable the transformations below for vectors, since
	 fold_binary_op_with_conditional_arg may undo them immediately,
	 yielding an infinite loop.  */
      if (code == VEC_COND_EXPR)
	return NULL_TREE;

      /* Convert A ? B : 0 into A && B if A and B are truth values.  */
      if (integer_zerop (op2)
	  && truth_value_p (TREE_CODE (arg0))
	  && truth_value_p (TREE_CODE (arg1))
	  && (code == VEC_COND_EXPR || !VECTOR_TYPE_P (type)))
	return fold_build2_loc (loc, code == VEC_COND_EXPR ? BIT_AND_EXPR
							   : TRUTH_ANDIF_EXPR,
				type, fold_convert_loc (loc, type, arg0), op1);

      /* Convert A ? B : 1 into !A || B if A and B are truth values.  */
      if (code == VEC_COND_EXPR ? integer_all_onesp (op2) : integer_onep (op2)
	  && truth_value_p (TREE_CODE (arg0))
	  && truth_value_p (TREE_CODE (arg1))
	  && (code == VEC_COND_EXPR || !VECTOR_TYPE_P (type)))
	{
	  location_t loc0 = expr_location_or (arg0, loc);
	  /* Only perform transformation if ARG0 is easily inverted.  */
	  tem = fold_invert_truthvalue (loc0, arg0);
	  if (tem)
	    return fold_build2_loc (loc, code == VEC_COND_EXPR
					 ? BIT_IOR_EXPR
					 : TRUTH_ORIF_EXPR,
				    type, fold_convert_loc (loc, type, tem),
				    op1);
	}

      /* Convert A ? 0 : B into !A && B if A and B are truth values.  */
      if (integer_zerop (arg1)
	  && truth_value_p (TREE_CODE (arg0))
	  && truth_value_p (TREE_CODE (op2))
	  && (code == VEC_COND_EXPR || !VECTOR_TYPE_P (type)))
	{
	  location_t loc0 = expr_location_or (arg0, loc);
	  /* Only perform transformation if ARG0 is easily inverted.  */
	  tem = fold_invert_truthvalue (loc0, arg0);
	  if (tem)
	    return fold_build2_loc (loc, code == VEC_COND_EXPR
					 ? BIT_AND_EXPR : TRUTH_ANDIF_EXPR,
				    type, fold_convert_loc (loc, type, tem),
				    op2);
	}

      /* Convert A ? 1 : B into A || B if A and B are truth values.  */
      if (code == VEC_COND_EXPR ? integer_all_onesp (arg1) : integer_onep (arg1)
	  && truth_value_p (TREE_CODE (arg0))
	  && truth_value_p (TREE_CODE (op2))
	  && (code == VEC_COND_EXPR || !VECTOR_TYPE_P (type)))
	return fold_build2_loc (loc, code == VEC_COND_EXPR
				     ? BIT_IOR_EXPR : TRUTH_ORIF_EXPR,
				type, fold_convert_loc (loc, type, arg0), op2);

      return NULL_TREE;

    case CALL_EXPR:
      /* CALL_EXPRs used to be ternary exprs.  Catch any mistaken uses
	 of fold_ternary on them.  */
      gcc_unreachable ();

    case BIT_FIELD_REF:
      if (TREE_CODE (arg0) == VECTOR_CST
	  && (type == TREE_TYPE (TREE_TYPE (arg0))
	      || (VECTOR_TYPE_P (type)
		  && TREE_TYPE (type) == TREE_TYPE (TREE_TYPE (arg0))))
	  && tree_fits_uhwi_p (op1)
	  && tree_fits_uhwi_p (op2))
	{
	  tree eltype = TREE_TYPE (TREE_TYPE (arg0));
	  unsigned HOST_WIDE_INT width
	    = (TREE_CODE (eltype) == BOOLEAN_TYPE
	       ? TYPE_PRECISION (eltype) : tree_to_uhwi (TYPE_SIZE (eltype)));
	  unsigned HOST_WIDE_INT n = tree_to_uhwi (arg1);
	  unsigned HOST_WIDE_INT idx = tree_to_uhwi (op2);

	  if (n != 0
	      && (idx % width) == 0
	      && (n % width) == 0
	      && known_le ((idx + n) / width,
			   TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg0))))
	    {
	      idx = idx / width;
	      n = n / width;

	      if (TREE_CODE (arg0) == VECTOR_CST)
		{
		  if (n == 1)
		    {
		      tem = VECTOR_CST_ELT (arg0, idx);
		      if (VECTOR_TYPE_P (type))
			tem = fold_build1 (VIEW_CONVERT_EXPR, type, tem);
		      return tem;
		    }

		  tree_vector_builder vals (type, n, 1);
		  for (unsigned i = 0; i < n; ++i)
		    vals.quick_push (VECTOR_CST_ELT (arg0, idx + i));
		  return vals.build ();
		}
	    }
	}

      /* On constants we can use native encode/interpret to constant
         fold (nearly) all BIT_FIELD_REFs.  */
      if (CONSTANT_CLASS_P (arg0)
	  && can_native_interpret_type_p (type)
	  && BITS_PER_UNIT == 8
	  && tree_fits_uhwi_p (op1)
	  && tree_fits_uhwi_p (op2))
	{
	  unsigned HOST_WIDE_INT bitpos = tree_to_uhwi (op2);
	  unsigned HOST_WIDE_INT bitsize = tree_to_uhwi (op1);
	  /* Limit us to a reasonable amount of work.  To relax the
	     other limitations we need bit-shifting of the buffer
	     and rounding up the size.  */
	  if (bitpos % BITS_PER_UNIT == 0
	      && bitsize % BITS_PER_UNIT == 0
	      && bitsize <= MAX_BITSIZE_MODE_ANY_MODE)
	    {
	      unsigned char b[MAX_BITSIZE_MODE_ANY_MODE / BITS_PER_UNIT];
	      unsigned HOST_WIDE_INT len
		= native_encode_expr (arg0, b, bitsize / BITS_PER_UNIT,
				      bitpos / BITS_PER_UNIT);
	      if (len > 0
		  && len * BITS_PER_UNIT >= bitsize)
		{
		  tree v = native_interpret_expr (type, b,
						  bitsize / BITS_PER_UNIT);
		  if (v)
		    return v;
		}
	    }
	}

      return NULL_TREE;

    case VEC_PERM_EXPR:
      /* Perform constant folding of BIT_INSERT_EXPR.  */
      if (TREE_CODE (arg2) == VECTOR_CST
	  && TREE_CODE (op0) == VECTOR_CST
	  && TREE_CODE (op1) == VECTOR_CST)
	{
	  /* Build a vector of integers from the tree mask.  */
	  vec_perm_builder builder;
	  if (!tree_to_vec_perm_builder (&builder, arg2))
	    return NULL_TREE;

	  /* Create a vec_perm_indices for the integer vector.  */
	  poly_uint64 nelts = TYPE_VECTOR_SUBPARTS (type);
	  bool single_arg = (op0 == op1);
	  vec_perm_indices sel (builder, single_arg ? 1 : 2, nelts);
	  return fold_vec_perm (type, op0, op1, sel);
	}
      return NULL_TREE;

    case BIT_INSERT_EXPR:
      /* Perform (partial) constant folding of BIT_INSERT_EXPR.  */
      if (TREE_CODE (arg0) == INTEGER_CST
	  && TREE_CODE (arg1) == INTEGER_CST)
	{
	  unsigned HOST_WIDE_INT bitpos = tree_to_uhwi (op2);
	  unsigned bitsize = TYPE_PRECISION (TREE_TYPE (arg1));
	  if (BYTES_BIG_ENDIAN)
	    bitpos = TYPE_PRECISION (type) - bitpos - bitsize;
	  wide_int tem = (wi::to_wide (arg0)
			  & wi::shifted_mask (bitpos, bitsize, true,
					      TYPE_PRECISION (type)));
	  wide_int tem2
	    = wi::lshift (wi::zext (wi::to_wide (arg1, TYPE_PRECISION (type)),
				    bitsize), bitpos);
	  return wide_int_to_tree (type, wi::bit_or (tem, tem2));
	}
      else if (TREE_CODE (arg0) == VECTOR_CST
	       && CONSTANT_CLASS_P (arg1)
	       && types_compatible_p (TREE_TYPE (TREE_TYPE (arg0)),
				      TREE_TYPE (arg1)))
	{
	  unsigned HOST_WIDE_INT bitpos = tree_to_uhwi (op2);
	  unsigned HOST_WIDE_INT elsize
	    = tree_to_uhwi (TYPE_SIZE (TREE_TYPE (arg1)));
	  if (bitpos % elsize == 0)
	    {
	      unsigned k = bitpos / elsize;
	      unsigned HOST_WIDE_INT nelts;
	      if (operand_equal_p (VECTOR_CST_ELT (arg0, k), arg1, 0))
		return arg0;
	      else if (VECTOR_CST_NELTS (arg0).is_constant (&nelts))
		{
		  tree_vector_builder elts (type, nelts, 1);
		  elts.quick_grow (nelts);
		  for (unsigned HOST_WIDE_INT i = 0; i < nelts; ++i)
		    elts[i] = (i == k ? arg1 : VECTOR_CST_ELT (arg0, i));
		  return elts.build ();
		}
	    }
	}
      return NULL_TREE;

    default:
      return NULL_TREE;
    } /* switch (code) */
}

/* Gets the element ACCESS_INDEX from CTOR, which must be a CONSTRUCTOR
   of an array (or vector).  *CTOR_IDX if non-NULL is updated with the
   constructor element index of the value returned.  If the element is
   not found NULL_TREE is returned and *CTOR_IDX is updated to
   the index of the element after the ACCESS_INDEX position (which
   may be outside of the CTOR array).  */

tree
get_array_ctor_element_at_index (tree ctor, offset_int access_index,
				 unsigned *ctor_idx)
{
  tree index_type = NULL_TREE;
  signop index_sgn = UNSIGNED;
  offset_int low_bound = 0;

  if (TREE_CODE (TREE_TYPE (ctor)) == ARRAY_TYPE)
    {
      tree domain_type = TYPE_DOMAIN (TREE_TYPE (ctor));
      if (domain_type && TYPE_MIN_VALUE (domain_type))
	{
	  /* Static constructors for variably sized objects makes no sense.  */
	  gcc_assert (TREE_CODE (TYPE_MIN_VALUE (domain_type)) == INTEGER_CST);
	  index_type = TREE_TYPE (TYPE_MIN_VALUE (domain_type));
	  /* ???  When it is obvious that the range is signed, treat it so.  */
	  if (TYPE_UNSIGNED (index_type)
	      && TYPE_MAX_VALUE (domain_type)
	      && tree_int_cst_lt (TYPE_MAX_VALUE (domain_type),
				  TYPE_MIN_VALUE (domain_type)))
	    {
	      index_sgn = SIGNED;
	      low_bound
		= offset_int::from (wi::to_wide (TYPE_MIN_VALUE (domain_type)),
				    SIGNED);
	    }
	  else
	    {
	      index_sgn = TYPE_SIGN (index_type);
	      low_bound = wi::to_offset (TYPE_MIN_VALUE (domain_type));
	    }
	}
    }

  if (index_type)
    access_index = wi::ext (access_index, TYPE_PRECISION (index_type),
			    index_sgn);

  offset_int index = low_bound;
  if (index_type)
    index = wi::ext (index, TYPE_PRECISION (index_type), index_sgn);

  offset_int max_index = index;
  unsigned cnt;
  tree cfield, cval;
  bool first_p = true;

  FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (ctor), cnt, cfield, cval)
    {
      /* Array constructor might explicitly set index, or specify a range,
	 or leave index NULL meaning that it is next index after previous
	 one.  */
      if (cfield)
	{
	  if (TREE_CODE (cfield) == INTEGER_CST)
	    max_index = index
	      = offset_int::from (wi::to_wide (cfield), index_sgn);
	  else
	    {
	      gcc_assert (TREE_CODE (cfield) == RANGE_EXPR);
	      index = offset_int::from (wi::to_wide (TREE_OPERAND (cfield, 0)),
					index_sgn);
	      max_index
	        = offset_int::from (wi::to_wide (TREE_OPERAND (cfield, 1)),
				    index_sgn);
	      gcc_checking_assert (wi::le_p (index, max_index, index_sgn));
	    }
	}
      else if (!first_p)
	{
	  index = max_index + 1;
	  if (index_type)
	    index = wi::ext (index, TYPE_PRECISION (index_type), index_sgn);
	  gcc_checking_assert (wi::gt_p (index, max_index, index_sgn));
	  max_index = index;
	}
      else
	first_p = false;

      /* Do we have match?  */
      if (wi::cmp (access_index, index, index_sgn) >= 0)
	{
	  if (wi::cmp (access_index, max_index, index_sgn) <= 0)
	    {
	      if (ctor_idx)
		*ctor_idx = cnt;
	      return cval;
	    }
	}
      else if (in_gimple_form)
	/* We're past the element we search for.  Note during parsing
	   the elements might not be sorted.
	   ???  We should use a binary search and a flag on the
	   CONSTRUCTOR as to whether elements are sorted in declaration
	   order.  */
	break;
    }
  if (ctor_idx)
    *ctor_idx = cnt;
  return NULL_TREE;
}

/* Perform constant folding and related simplification of EXPR.
   The related simplifications include x*1 => x, x*0 => 0, etc.,
   and application of the associative law.
   NOP_EXPR conversions may be removed freely (as long as we
   are careful not to change the type of the overall expression).
   We cannot simplify through a CONVERT_EXPR, FIX_EXPR or FLOAT_EXPR,
   but we can constant-fold them if they have constant operands.  */

#ifdef ENABLE_FOLD_CHECKING
# define fold(x) fold_1 (x)
static tree fold_1 (tree);
static
#endif
tree
fold (tree expr)
{
  const tree t = expr;
  enum tree_code code = TREE_CODE (t);
  enum tree_code_class kind = TREE_CODE_CLASS (code);
  tree tem;
  location_t loc = EXPR_LOCATION (expr);

  /* Return right away if a constant.  */
  if (kind == tcc_constant)
    return t;

  /* CALL_EXPR-like objects with variable numbers of operands are
     treated specially.  */
  if (kind == tcc_vl_exp)
    {
      if (code == CALL_EXPR)
	{
	  tem = fold_call_expr (loc, expr, false);
	  return tem ? tem : expr;
	}
      return expr;
    }

  if (IS_EXPR_CODE_CLASS (kind))
    {
      tree type = TREE_TYPE (t);
      tree op0, op1, op2;

      switch (TREE_CODE_LENGTH (code))
	{
	case 1:
	  op0 = TREE_OPERAND (t, 0);
	  tem = fold_unary_loc (loc, code, type, op0);
	  return tem ? tem : expr;
	case 2:
	  op0 = TREE_OPERAND (t, 0);
	  op1 = TREE_OPERAND (t, 1);
	  tem = fold_binary_loc (loc, code, type, op0, op1);
	  return tem ? tem : expr;
	case 3:
	  op0 = TREE_OPERAND (t, 0);
	  op1 = TREE_OPERAND (t, 1);
	  op2 = TREE_OPERAND (t, 2);
	  tem = fold_ternary_loc (loc, code, type, op0, op1, op2);
	  return tem ? tem : expr;
	default:
	  break;
	}
    }

  switch (code)
    {
    case ARRAY_REF:
      {
	tree op0 = TREE_OPERAND (t, 0);
	tree op1 = TREE_OPERAND (t, 1);

	if (TREE_CODE (op1) == INTEGER_CST
	    && TREE_CODE (op0) == CONSTRUCTOR
	    && ! type_contains_placeholder_p (TREE_TYPE (op0)))
	  {
	    tree val = get_array_ctor_element_at_index (op0,
							wi::to_offset (op1));
	    if (val)
	      return val;
	  }

	return t;
      }

      /* Return a VECTOR_CST if possible.  */
    case CONSTRUCTOR:
      {
	tree type = TREE_TYPE (t);
	if (TREE_CODE (type) != VECTOR_TYPE)
	  return t;

	unsigned i;
	tree val;
	FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (t), i, val)
	  if (! CONSTANT_CLASS_P (val))
	    return t;

	return build_vector_from_ctor (type, CONSTRUCTOR_ELTS (t));
      }

    case CONST_DECL:
      return fold (DECL_INITIAL (t));

    default:
      return t;
    } /* switch (code) */
}

#ifdef ENABLE_FOLD_CHECKING
#undef fold

static void fold_checksum_tree (const_tree, struct md5_ctx *,
				hash_table<nofree_ptr_hash<const tree_node> > *);
static void fold_check_failed (const_tree, const_tree);
void print_fold_checksum (const_tree);

/* When --enable-checking=fold, compute a digest of expr before
   and after actual fold call to see if fold did not accidentally
   change original expr.  */

tree
fold (tree expr)
{
  tree ret;
  struct md5_ctx ctx;
  unsigned char checksum_before[16], checksum_after[16];
  hash_table<nofree_ptr_hash<const tree_node> > ht (32);

  md5_init_ctx (&ctx);
  fold_checksum_tree (expr, &ctx, &ht);
  md5_finish_ctx (&ctx, checksum_before);
  ht.empty ();

  ret = fold_1 (expr);

  md5_init_ctx (&ctx);
  fold_checksum_tree (expr, &ctx, &ht);
  md5_finish_ctx (&ctx, checksum_after);

  if (memcmp (checksum_before, checksum_after, 16))
    fold_check_failed (expr, ret);

  return ret;
}

void
print_fold_checksum (const_tree expr)
{
  struct md5_ctx ctx;
  unsigned char checksum[16], cnt;
  hash_table<nofree_ptr_hash<const tree_node> > ht (32);

  md5_init_ctx (&ctx);
  fold_checksum_tree (expr, &ctx, &ht);
  md5_finish_ctx (&ctx, checksum);
  for (cnt = 0; cnt < 16; ++cnt)
    fprintf (stderr, "%02x", checksum[cnt]);
  putc ('\n', stderr);
}

static void
fold_check_failed (const_tree expr ATTRIBUTE_UNUSED, const_tree ret ATTRIBUTE_UNUSED)
{
  internal_error ("fold check: original tree changed by fold");
}

static void
fold_checksum_tree (const_tree expr, struct md5_ctx *ctx,
		    hash_table<nofree_ptr_hash <const tree_node> > *ht)
{
  const tree_node **slot;
  enum tree_code code;
  union tree_node *buf;
  int i, len;

 recursive_label:
  if (expr == NULL)
    return;
  slot = ht->find_slot (expr, INSERT);
  if (*slot != NULL)
    return;
  *slot = expr;
  code = TREE_CODE (expr);
  if (TREE_CODE_CLASS (code) == tcc_declaration
      && HAS_DECL_ASSEMBLER_NAME_P (expr))
    {
      /* Allow DECL_ASSEMBLER_NAME and symtab_node to be modified.  */
      size_t sz = tree_size (expr);
      buf = XALLOCAVAR (union tree_node, sz);
      memcpy ((char *) buf, expr, sz);
      SET_DECL_ASSEMBLER_NAME ((tree) buf, NULL);
      buf->decl_with_vis.symtab_node = NULL;
      buf->base.nowarning_flag = 0;
      expr = (tree) buf;
    }
  else if (TREE_CODE_CLASS (code) == tcc_type
	   && (TYPE_POINTER_TO (expr)
	       || TYPE_REFERENCE_TO (expr)
	       || TYPE_CACHED_VALUES_P (expr)
	       || TYPE_CONTAINS_PLACEHOLDER_INTERNAL (expr)
	       || TYPE_NEXT_VARIANT (expr)
	       || TYPE_ALIAS_SET_KNOWN_P (expr)))
    {
      /* Allow these fields to be modified.  */
      tree tmp;
      size_t sz = tree_size (expr);
      buf = XALLOCAVAR (union tree_node, sz);
      memcpy ((char *) buf, expr, sz);
      expr = tmp = (tree) buf;
      TYPE_CONTAINS_PLACEHOLDER_INTERNAL (tmp) = 0;
      TYPE_POINTER_TO (tmp) = NULL;
      TYPE_REFERENCE_TO (tmp) = NULL;
      TYPE_NEXT_VARIANT (tmp) = NULL;
      TYPE_ALIAS_SET (tmp) = -1;
      if (TYPE_CACHED_VALUES_P (tmp))
	{
	  TYPE_CACHED_VALUES_P (tmp) = 0;
	  TYPE_CACHED_VALUES (tmp) = NULL;
	}
    }
  else if (warning_suppressed_p (expr) && (DECL_P (expr) || EXPR_P (expr)))
    {
      /* Allow the no-warning bit to be set.  Perhaps we shouldn't allow
	 that and change builtins.cc etc. instead - see PR89543.  */
      size_t sz = tree_size (expr);
      buf = XALLOCAVAR (union tree_node, sz);
      memcpy ((char *) buf, expr, sz);
      buf->base.nowarning_flag = 0;
      expr = (tree) buf;
    }
  md5_process_bytes (expr, tree_size (expr), ctx);
  if (CODE_CONTAINS_STRUCT (code, TS_TYPED))
    fold_checksum_tree (TREE_TYPE (expr), ctx, ht);
  if (TREE_CODE_CLASS (code) != tcc_type
      && TREE_CODE_CLASS (code) != tcc_declaration
      && code != TREE_LIST
      && code != SSA_NAME
      && CODE_CONTAINS_STRUCT (code, TS_COMMON))
    fold_checksum_tree (TREE_CHAIN (expr), ctx, ht);
  switch (TREE_CODE_CLASS (code))
    {
    case tcc_constant:
      switch (code)
	{
	case STRING_CST:
	  md5_process_bytes (TREE_STRING_POINTER (expr),
			     TREE_STRING_LENGTH (expr), ctx);
	  break;
	case COMPLEX_CST:
	  fold_checksum_tree (TREE_REALPART (expr), ctx, ht);
	  fold_checksum_tree (TREE_IMAGPART (expr), ctx, ht);
	  break;
	case VECTOR_CST:
	  len = vector_cst_encoded_nelts (expr);
	  for (i = 0; i < len; ++i)
	    fold_checksum_tree (VECTOR_CST_ENCODED_ELT (expr, i), ctx, ht);
	  break;
	default:
	  break;
	}
      break;
    case tcc_exceptional:
      switch (code)
	{
	case TREE_LIST:
	  fold_checksum_tree (TREE_PURPOSE (expr), ctx, ht);
	  fold_checksum_tree (TREE_VALUE (expr), ctx, ht);
	  expr = TREE_CHAIN (expr);
	  goto recursive_label;
	  break;
	case TREE_VEC:
	  for (i = 0; i < TREE_VEC_LENGTH (expr); ++i)
	    fold_checksum_tree (TREE_VEC_ELT (expr, i), ctx, ht);
	  break;
	default:
	  break;
	}
      break;
    case tcc_expression:
    case tcc_reference:
    case tcc_comparison:
    case tcc_unary:
    case tcc_binary:
    case tcc_statement:
    case tcc_vl_exp:
      len = TREE_OPERAND_LENGTH (expr);
      for (i = 0; i < len; ++i)
	fold_checksum_tree (TREE_OPERAND (expr, i), ctx, ht);
      break;
    case tcc_declaration:
      fold_checksum_tree (DECL_NAME (expr), ctx, ht);
      fold_checksum_tree (DECL_CONTEXT (expr), ctx, ht);
      if (CODE_CONTAINS_STRUCT (TREE_CODE (expr), TS_DECL_COMMON))
	{
	  fold_checksum_tree (DECL_SIZE (expr), ctx, ht);
	  fold_checksum_tree (DECL_SIZE_UNIT (expr), ctx, ht);
	  fold_checksum_tree (DECL_INITIAL (expr), ctx, ht);
	  fold_checksum_tree (DECL_ABSTRACT_ORIGIN (expr), ctx, ht);
	  fold_checksum_tree (DECL_ATTRIBUTES (expr), ctx, ht);
	}

      if (CODE_CONTAINS_STRUCT (TREE_CODE (expr), TS_DECL_NON_COMMON))
	{
	  if (TREE_CODE (expr) == FUNCTION_DECL)
	    {
	      fold_checksum_tree (DECL_VINDEX (expr), ctx, ht);
	      fold_checksum_tree (DECL_ARGUMENTS (expr), ctx, ht);
	    }
	  fold_checksum_tree (DECL_RESULT_FLD (expr), ctx, ht);
	}
      break;
    case tcc_type:
      if (TREE_CODE (expr) == ENUMERAL_TYPE)
        fold_checksum_tree (TYPE_VALUES (expr), ctx, ht);
      fold_checksum_tree (TYPE_SIZE (expr), ctx, ht);
      fold_checksum_tree (TYPE_SIZE_UNIT (expr), ctx, ht);
      fold_checksum_tree (TYPE_ATTRIBUTES (expr), ctx, ht);
      fold_checksum_tree (TYPE_NAME (expr), ctx, ht);
      if (INTEGRAL_TYPE_P (expr)
          || SCALAR_FLOAT_TYPE_P (expr))
	{
	  fold_checksum_tree (TYPE_MIN_VALUE (expr), ctx, ht);
	  fold_checksum_tree (TYPE_MAX_VALUE (expr), ctx, ht);
	}
      fold_checksum_tree (TYPE_MAIN_VARIANT (expr), ctx, ht);
      if (RECORD_OR_UNION_TYPE_P (expr))
	fold_checksum_tree (TYPE_BINFO (expr), ctx, ht);
      fold_checksum_tree (TYPE_CONTEXT (expr), ctx, ht);
      break;
    default:
      break;
    }
}

/* Helper function for outputting the checksum of a tree T.  When
   debugging with gdb, you can "define mynext" to be "next" followed
   by "call debug_fold_checksum (op0)", then just trace down till the
   outputs differ.  */

DEBUG_FUNCTION void
debug_fold_checksum (const_tree t)
{
  int i;
  unsigned char checksum[16];
  struct md5_ctx ctx;
  hash_table<nofree_ptr_hash<const tree_node> > ht (32);

  md5_init_ctx (&ctx);
  fold_checksum_tree (t, &ctx, &ht);
  md5_finish_ctx (&ctx, checksum);
  ht.empty ();

  for (i = 0; i < 16; i++)
    fprintf (stderr, "%d ", checksum[i]);

  fprintf (stderr, "\n");
}

#endif

/* Fold a unary tree expression with code CODE of type TYPE with an
   operand OP0.  LOC is the location of the resulting expression.
   Return a folded expression if successful.  Otherwise, return a tree
   expression with code CODE of type TYPE with an operand OP0.  */

tree
fold_build1_loc (location_t loc,
		 enum tree_code code, tree type, tree op0 MEM_STAT_DECL)
{
  tree tem;
#ifdef ENABLE_FOLD_CHECKING
  unsigned char checksum_before[16], checksum_after[16];
  struct md5_ctx ctx;
  hash_table<nofree_ptr_hash<const tree_node> > ht (32);

  md5_init_ctx (&ctx);
  fold_checksum_tree (op0, &ctx, &ht);
  md5_finish_ctx (&ctx, checksum_before);
  ht.empty ();
#endif

  tem = fold_unary_loc (loc, code, type, op0);
  if (!tem)
    tem = build1_loc (loc, code, type, op0 PASS_MEM_STAT);

#ifdef ENABLE_FOLD_CHECKING
  md5_init_ctx (&ctx);
  fold_checksum_tree (op0, &ctx, &ht);
  md5_finish_ctx (&ctx, checksum_after);

  if (memcmp (checksum_before, checksum_after, 16))
    fold_check_failed (op0, tem);
#endif
  return tem;
}

/* Fold a binary tree expression with code CODE of type TYPE with
   operands OP0 and OP1.  LOC is the location of the resulting
   expression.  Return a folded expression if successful.  Otherwise,
   return a tree expression with code CODE of type TYPE with operands
   OP0 and OP1.  */

tree
fold_build2_loc (location_t loc,
		      enum tree_code code, tree type, tree op0, tree op1
		      MEM_STAT_DECL)
{
  tree tem;
#ifdef ENABLE_FOLD_CHECKING
  unsigned char checksum_before_op0[16],
                checksum_before_op1[16],
		checksum_after_op0[16],
		checksum_after_op1[16];
  struct md5_ctx ctx;
  hash_table<nofree_ptr_hash<const tree_node> > ht (32);

  md5_init_ctx (&ctx);
  fold_checksum_tree (op0, &ctx, &ht);
  md5_finish_ctx (&ctx, checksum_before_op0);
  ht.empty ();

  md5_init_ctx (&ctx);
  fold_checksum_tree (op1, &ctx, &ht);
  md5_finish_ctx (&ctx, checksum_before_op1);
  ht.empty ();
#endif

  tem = fold_binary_loc (loc, code, type, op0, op1);
  if (!tem)
    tem = build2_loc (loc, code, type, op0, op1 PASS_MEM_STAT);

#ifdef ENABLE_FOLD_CHECKING
  md5_init_ctx (&ctx);
  fold_checksum_tree (op0, &ctx, &ht);
  md5_finish_ctx (&ctx, checksum_after_op0);
  ht.empty ();

  if (memcmp (checksum_before_op0, checksum_after_op0, 16))
    fold_check_failed (op0, tem);

  md5_init_ctx (&ctx);
  fold_checksum_tree (op1, &ctx, &ht);
  md5_finish_ctx (&ctx, checksum_after_op1);

  if (memcmp (checksum_before_op1, checksum_after_op1, 16))
    fold_check_failed (op1, tem);
#endif
  return tem;
}

/* Fold a ternary tree expression with code CODE of type TYPE with
   operands OP0, OP1, and OP2.  Return a folded expression if
   successful.  Otherwise, return a tree expression with code CODE of
   type TYPE with operands OP0, OP1, and OP2.  */

tree
fold_build3_loc (location_t loc, enum tree_code code, tree type,
		      tree op0, tree op1, tree op2 MEM_STAT_DECL)
{
  tree tem;
#ifdef ENABLE_FOLD_CHECKING
  unsigned char checksum_before_op0[16],
                checksum_before_op1[16],
                checksum_before_op2[16],
		checksum_after_op0[16],
		checksum_after_op1[16],
		checksum_after_op2[16];
  struct md5_ctx ctx;
  hash_table<nofree_ptr_hash<const tree_node> > ht (32);

  md5_init_ctx (&ctx);
  fold_checksum_tree (op0, &ctx, &ht);
  md5_finish_ctx (&ctx, checksum_before_op0);
  ht.empty ();

  md5_init_ctx (&ctx);
  fold_checksum_tree (op1, &ctx, &ht);
  md5_finish_ctx (&ctx, checksum_before_op1);
  ht.empty ();

  md5_init_ctx (&ctx);
  fold_checksum_tree (op2, &ctx, &ht);
  md5_finish_ctx (&ctx, checksum_before_op2);
  ht.empty ();
#endif

  gcc_assert (TREE_CODE_CLASS (code) != tcc_vl_exp);
  tem = fold_ternary_loc (loc, code, type, op0, op1, op2);
  if (!tem)
    tem = build3_loc (loc, code, type, op0, op1, op2 PASS_MEM_STAT);

#ifdef ENABLE_FOLD_CHECKING
  md5_init_ctx (&ctx);
  fold_checksum_tree (op0, &ctx, &ht);
  md5_finish_ctx (&ctx, checksum_after_op0);
  ht.empty ();

  if (memcmp (checksum_before_op0, checksum_after_op0, 16))
    fold_check_failed (op0, tem);

  md5_init_ctx (&ctx);
  fold_checksum_tree (op1, &ctx, &ht);
  md5_finish_ctx (&ctx, checksum_after_op1);
  ht.empty ();

  if (memcmp (checksum_before_op1, checksum_after_op1, 16))
    fold_check_failed (op1, tem);

  md5_init_ctx (&ctx);
  fold_checksum_tree (op2, &ctx, &ht);
  md5_finish_ctx (&ctx, checksum_after_op2);

  if (memcmp (checksum_before_op2, checksum_after_op2, 16))
    fold_check_failed (op2, tem);
#endif
  return tem;
}

/* Fold a CALL_EXPR expression of type TYPE with operands FN and NARGS
   arguments in ARGARRAY, and a null static chain.
   Return a folded expression if successful.  Otherwise, return a CALL_EXPR
   of type TYPE from the given operands as constructed by build_call_array.  */

tree
fold_build_call_array_loc (location_t loc, tree type, tree fn,
			   int nargs, tree *argarray)
{
  tree tem;
#ifdef ENABLE_FOLD_CHECKING
  unsigned char checksum_before_fn[16],
                checksum_before_arglist[16],
		checksum_after_fn[16],
		checksum_after_arglist[16];
  struct md5_ctx ctx;
  hash_table<nofree_ptr_hash<const tree_node> > ht (32);
  int i;

  md5_init_ctx (&ctx);
  fold_checksum_tree (fn, &ctx, &ht);
  md5_finish_ctx (&ctx, checksum_before_fn);
  ht.empty ();

  md5_init_ctx (&ctx);
  for (i = 0; i < nargs; i++)
    fold_checksum_tree (argarray[i], &ctx, &ht);
  md5_finish_ctx (&ctx, checksum_before_arglist);
  ht.empty ();
#endif

  tem = fold_builtin_call_array (loc, type, fn, nargs, argarray);
  if (!tem)
    tem = build_call_array_loc (loc, type, fn, nargs, argarray);

#ifdef ENABLE_FOLD_CHECKING
  md5_init_ctx (&ctx);
  fold_checksum_tree (fn, &ctx, &ht);
  md5_finish_ctx (&ctx, checksum_after_fn);
  ht.empty ();

  if (memcmp (checksum_before_fn, checksum_after_fn, 16))
    fold_check_failed (fn, tem);

  md5_init_ctx (&ctx);
  for (i = 0; i < nargs; i++)
    fold_checksum_tree (argarray[i], &ctx, &ht);
  md5_finish_ctx (&ctx, checksum_after_arglist);

  if (memcmp (checksum_before_arglist, checksum_after_arglist, 16))
    fold_check_failed (NULL_TREE, tem);
#endif
  return tem;
}

/* Perform constant folding and related simplification of initializer
   expression EXPR.  These behave identically to "fold_buildN" but ignore
   potential run-time traps and exceptions that fold must preserve.  */

#define START_FOLD_INIT \
  int saved_signaling_nans = flag_signaling_nans;\
  int saved_trapping_math = flag_trapping_math;\
  int saved_rounding_math = flag_rounding_math;\
  int saved_trapv = flag_trapv;\
  int saved_folding_initializer = folding_initializer;\
  flag_signaling_nans = 0;\
  flag_trapping_math = 0;\
  flag_rounding_math = 0;\
  flag_trapv = 0;\
  folding_initializer = 1;

#define END_FOLD_INIT \
  flag_signaling_nans = saved_signaling_nans;\
  flag_trapping_math = saved_trapping_math;\
  flag_rounding_math = saved_rounding_math;\
  flag_trapv = saved_trapv;\
  folding_initializer = saved_folding_initializer;

tree
fold_init (tree expr)
{
  tree result;
  START_FOLD_INIT;

  result = fold (expr);

  END_FOLD_INIT;
  return result;
}

tree
fold_build1_initializer_loc (location_t loc, enum tree_code code,
			     tree type, tree op)
{
  tree result;
  START_FOLD_INIT;

  result = fold_build1_loc (loc, code, type, op);

  END_FOLD_INIT;
  return result;
}

tree
fold_build2_initializer_loc (location_t loc, enum tree_code code,
			     tree type, tree op0, tree op1)
{
  tree result;
  START_FOLD_INIT;

  result = fold_build2_loc (loc, code, type, op0, op1);

  END_FOLD_INIT;
  return result;
}

tree
fold_build_call_array_initializer_loc (location_t loc, tree type, tree fn,
				       int nargs, tree *argarray)
{
  tree result;
  START_FOLD_INIT;

  result = fold_build_call_array_loc (loc, type, fn, nargs, argarray);

  END_FOLD_INIT;
  return result;
}

tree
fold_binary_initializer_loc (location_t loc, tree_code code, tree type,
			     tree lhs, tree rhs)
{
  tree result;
  START_FOLD_INIT;

  result = fold_binary_loc (loc, code, type, lhs, rhs);

  END_FOLD_INIT;
  return result;
}

#undef START_FOLD_INIT
#undef END_FOLD_INIT

/* Determine if first argument is a multiple of second argument.  Return
   false if it is not, or we cannot easily determined it to be.

   An example of the sort of thing we care about (at this point; this routine
   could surely be made more general, and expanded to do what the *_DIV_EXPR's
   fold cases do now) is discovering that

     SAVE_EXPR (I) * SAVE_EXPR (J * 8)

   is a multiple of

     SAVE_EXPR (J * 8)

   when we know that the two SAVE_EXPR (J * 8) nodes are the same node.

   This code also handles discovering that

     SAVE_EXPR (I) * SAVE_EXPR (J * 8)

   is a multiple of 8 so we don't have to worry about dealing with a
   possible remainder.

   Note that we *look* inside a SAVE_EXPR only to determine how it was
   calculated; it is not safe for fold to do much of anything else with the
   internals of a SAVE_EXPR, since it cannot know when it will be evaluated
   at run time.  For example, the latter example above *cannot* be implemented
   as SAVE_EXPR (I) * J or any variant thereof, since the value of J at
   evaluation time of the original SAVE_EXPR is not necessarily the same at
   the time the new expression is evaluated.  The only optimization of this
   sort that would be valid is changing

     SAVE_EXPR (I) * SAVE_EXPR (SAVE_EXPR (J) * 8)

   divided by 8 to

     SAVE_EXPR (I) * SAVE_EXPR (J)

   (where the same SAVE_EXPR (J) is used in the original and the
   transformed version).

   NOWRAP specifies whether all outer operations in TYPE should
   be considered not wrapping.  Any type conversion within TOP acts
   as a barrier and we will fall back to NOWRAP being false.
   NOWRAP is mostly used to treat expressions in TYPE_SIZE and friends
   as not wrapping even though they are generally using unsigned arithmetic.  */

bool
multiple_of_p (tree type, const_tree top, const_tree bottom, bool nowrap)
{
  gimple *stmt;
  tree op1, op2;

  if (operand_equal_p (top, bottom, 0))
    return true;

  if (TREE_CODE (type) != INTEGER_TYPE)
    return false;

  switch (TREE_CODE (top))
    {
    case BIT_AND_EXPR:
      /* Bitwise and provides a power of two multiple.  If the mask is
	 a multiple of BOTTOM then TOP is a multiple of BOTTOM.  */
      if (!integer_pow2p (bottom))
	return false;
      return (multiple_of_p (type, TREE_OPERAND (top, 1), bottom, nowrap)
	      || multiple_of_p (type, TREE_OPERAND (top, 0), bottom, nowrap));

    case MULT_EXPR:
      /* If the multiplication can wrap we cannot recurse further unless
	 the bottom is a power of two which is where wrapping does not
	 matter.  */
      if (!nowrap
	  && !TYPE_OVERFLOW_UNDEFINED (type)
	  && !integer_pow2p (bottom))
	return false;
      if (TREE_CODE (bottom) == INTEGER_CST)
	{
	  op1 = TREE_OPERAND (top, 0);
	  op2 = TREE_OPERAND (top, 1);
	  if (TREE_CODE (op1) == INTEGER_CST)
	    std::swap (op1, op2);
	  if (TREE_CODE (op2) == INTEGER_CST)
	    {
	      if (multiple_of_p (type, op2, bottom, nowrap))
		return true;
	      /* Handle multiple_of_p ((x * 2 + 2) * 4, 8).  */
	      if (multiple_of_p (type, bottom, op2, nowrap))
		{
		  widest_int w = wi::sdiv_trunc (wi::to_widest (bottom),
						 wi::to_widest (op2));
		  if (wi::fits_to_tree_p (w, TREE_TYPE (bottom)))
		    {
		      op2 = wide_int_to_tree (TREE_TYPE (bottom), w);
		      return multiple_of_p (type, op1, op2, nowrap);
		    }
		}
	      return multiple_of_p (type, op1, bottom, nowrap);
	    }
	}
      return (multiple_of_p (type, TREE_OPERAND (top, 1), bottom, nowrap)
	      || multiple_of_p (type, TREE_OPERAND (top, 0), bottom, nowrap));

    case LSHIFT_EXPR:
      /* Handle X << CST as X * (1 << CST) and only process the constant.  */
      if (TREE_CODE (TREE_OPERAND (top, 1)) == INTEGER_CST)
	{
	  op1 = TREE_OPERAND (top, 1);
	  if (wi::to_widest (op1) < TYPE_PRECISION (type))
	    {
	      wide_int mul_op
		= wi::one (TYPE_PRECISION (type)) << wi::to_wide (op1);
	      return multiple_of_p (type,
				    wide_int_to_tree (type, mul_op), bottom,
				    nowrap);
	    }
	}
      return false;

    case MINUS_EXPR:
    case PLUS_EXPR:
      /* If the addition or subtraction can wrap we cannot recurse further
	 unless bottom is a power of two which is where wrapping does not
	 matter.  */
      if (!nowrap
	  && !TYPE_OVERFLOW_UNDEFINED (type)
	  && !integer_pow2p (bottom))
	return false;

      /* Handle cases like op0 + 0xfffffffd as op0 - 3 if the expression has
	 unsigned type.  For example, (X / 3) + 0xfffffffd is multiple of 3,
	 but 0xfffffffd is not.  */
      op1 = TREE_OPERAND (top, 1);
      if (TREE_CODE (top) == PLUS_EXPR
	  && nowrap
	  && TYPE_UNSIGNED (type)
	  && TREE_CODE (op1) == INTEGER_CST && tree_int_cst_sign_bit (op1))
	op1 = fold_build1 (NEGATE_EXPR, type, op1);

      /* It is impossible to prove if op0 +- op1 is multiple of bottom
	 precisely, so be conservative here checking if both op0 and op1
	 are multiple of bottom.  Note we check the second operand first
	 since it's usually simpler.  */
      return (multiple_of_p (type, op1, bottom, nowrap)
	      && multiple_of_p (type, TREE_OPERAND (top, 0), bottom, nowrap));

    CASE_CONVERT:
      /* Can't handle conversions from non-integral or wider integral type.  */
      if ((TREE_CODE (TREE_TYPE (TREE_OPERAND (top, 0))) != INTEGER_TYPE)
	  || (TYPE_PRECISION (type)
	      < TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (top, 0)))))
	return false;
      /* NOWRAP only extends to operations in the outermost type so
	 make sure to strip it off here.  */
      return multiple_of_p (TREE_TYPE (TREE_OPERAND (top, 0)),
			    TREE_OPERAND (top, 0), bottom, false);

    case SAVE_EXPR:
      return multiple_of_p (type, TREE_OPERAND (top, 0), bottom, nowrap);

    case COND_EXPR:
      return (multiple_of_p (type, TREE_OPERAND (top, 1), bottom, nowrap)
	      && multiple_of_p (type, TREE_OPERAND (top, 2), bottom, nowrap));

    case INTEGER_CST:
      if (TREE_CODE (bottom) != INTEGER_CST || integer_zerop (bottom))
	return false;
      return wi::multiple_of_p (wi::to_widest (top), wi::to_widest (bottom),
				SIGNED);

    case SSA_NAME:
      if (TREE_CODE (bottom) == INTEGER_CST
	  && (stmt = SSA_NAME_DEF_STMT (top)) != NULL
	  && gimple_code (stmt) == GIMPLE_ASSIGN)
	{
	  enum tree_code code = gimple_assign_rhs_code (stmt);

	  /* Check for special cases to see if top is defined as multiple
	     of bottom:

	       top = (X & ~(bottom - 1) ; bottom is power of 2

	     or

	       Y = X % bottom
	       top = X - Y.  */
	  if (code == BIT_AND_EXPR
	      && (op2 = gimple_assign_rhs2 (stmt)) != NULL_TREE
	      && TREE_CODE (op2) == INTEGER_CST
	      && integer_pow2p (bottom)
	      && wi::multiple_of_p (wi::to_widest (op2),
				    wi::to_widest (bottom), SIGNED))
	    return true;

	  op1 = gimple_assign_rhs1 (stmt);
	  if (code == MINUS_EXPR
	      && (op2 = gimple_assign_rhs2 (stmt)) != NULL_TREE
	      && TREE_CODE (op2) == SSA_NAME
	      && (stmt = SSA_NAME_DEF_STMT (op2)) != NULL
	      && gimple_code (stmt) == GIMPLE_ASSIGN
	      && (code = gimple_assign_rhs_code (stmt)) == TRUNC_MOD_EXPR
	      && operand_equal_p (op1, gimple_assign_rhs1 (stmt), 0)
	      && operand_equal_p (bottom, gimple_assign_rhs2 (stmt), 0))
	    return true;
	}

      /* fall through */

    default:
      if (POLY_INT_CST_P (top) && poly_int_tree_p (bottom))
	return multiple_p (wi::to_poly_widest (top),
			   wi::to_poly_widest (bottom));

      return false;
    }
}

/* Return true if expression X cannot be (or contain) a NaN or infinity.
   This function returns true for integer expressions, and returns
   false if uncertain.  */

bool
tree_expr_finite_p (const_tree x)
{
  machine_mode mode = element_mode (x);
  if (!HONOR_NANS (mode) && !HONOR_INFINITIES (mode))
    return true;
  switch (TREE_CODE (x))
    {
    case REAL_CST:
      return real_isfinite (TREE_REAL_CST_PTR (x));
    case COMPLEX_CST:
      return tree_expr_finite_p (TREE_REALPART (x))
	     && tree_expr_finite_p (TREE_IMAGPART (x));
    case FLOAT_EXPR:
      return true;
    case ABS_EXPR:
    case CONVERT_EXPR:
    case NON_LVALUE_EXPR:
    case NEGATE_EXPR:
    case SAVE_EXPR:
      return tree_expr_finite_p (TREE_OPERAND (x, 0));
    case MIN_EXPR:
    case MAX_EXPR:
      return tree_expr_finite_p (TREE_OPERAND (x, 0))
	     && tree_expr_finite_p (TREE_OPERAND (x, 1));
    case COND_EXPR:
      return tree_expr_finite_p (TREE_OPERAND (x, 1))
	     && tree_expr_finite_p (TREE_OPERAND (x, 2));
    case CALL_EXPR:
      switch (get_call_combined_fn (x))
	{
	CASE_CFN_FABS:
	CASE_CFN_FABS_FN:
	  return tree_expr_finite_p (CALL_EXPR_ARG (x, 0));
	CASE_CFN_FMAX:
	CASE_CFN_FMAX_FN:
	CASE_CFN_FMIN:
	CASE_CFN_FMIN_FN:
	  return tree_expr_finite_p (CALL_EXPR_ARG (x, 0))
		 && tree_expr_finite_p (CALL_EXPR_ARG (x, 1));
	default:
	  return false;
	}

    default:
      return false;
    }
}

/* Return true if expression X evaluates to an infinity.
   This function returns false for integer expressions.  */

bool
tree_expr_infinite_p (const_tree x)
{
  if (!HONOR_INFINITIES (x))
    return false;
  switch (TREE_CODE (x))
    {
    case REAL_CST:
      return real_isinf (TREE_REAL_CST_PTR (x));
    case ABS_EXPR:
    case NEGATE_EXPR:
    case NON_LVALUE_EXPR:
    case SAVE_EXPR:
      return tree_expr_infinite_p (TREE_OPERAND (x, 0));
    case COND_EXPR:
      return tree_expr_infinite_p (TREE_OPERAND (x, 1))
	     && tree_expr_infinite_p (TREE_OPERAND (x, 2));
    default:
      return false;
    }
}

/* Return true if expression X could evaluate to an infinity.
   This function returns false for integer expressions, and returns
   true if uncertain.  */

bool
tree_expr_maybe_infinite_p (const_tree x)
{
  if (!HONOR_INFINITIES (x))
    return false;
  switch (TREE_CODE (x))
    {
    case REAL_CST:
      return real_isinf (TREE_REAL_CST_PTR (x));
    case FLOAT_EXPR:
      return false;
    case ABS_EXPR:
    case NEGATE_EXPR:
      return tree_expr_maybe_infinite_p (TREE_OPERAND (x, 0));
    case COND_EXPR:
      return tree_expr_maybe_infinite_p (TREE_OPERAND (x, 1))
	     || tree_expr_maybe_infinite_p (TREE_OPERAND (x, 2));
    default:
      return true;
    }
}

/* Return true if expression X evaluates to a signaling NaN.
   This function returns false for integer expressions.  */

bool
tree_expr_signaling_nan_p (const_tree x)
{
  if (!HONOR_SNANS (x))
    return false;
  switch (TREE_CODE (x))
    {
    case REAL_CST:
      return real_issignaling_nan (TREE_REAL_CST_PTR (x));
    case NON_LVALUE_EXPR:
    case SAVE_EXPR:
      return tree_expr_signaling_nan_p (TREE_OPERAND (x, 0));
    case COND_EXPR:
      return tree_expr_signaling_nan_p (TREE_OPERAND (x, 1))
	     && tree_expr_signaling_nan_p (TREE_OPERAND (x, 2));
    default:
      return false;
    }
}

/* Return true if expression X could evaluate to a signaling NaN.
   This function returns false for integer expressions, and returns
   true if uncertain.  */

bool
tree_expr_maybe_signaling_nan_p (const_tree x)
{
  if (!HONOR_SNANS (x))
    return false;
  switch (TREE_CODE (x))
    {
    case REAL_CST:
      return real_issignaling_nan (TREE_REAL_CST_PTR (x));
    case FLOAT_EXPR:
      return false;
    case ABS_EXPR:
    case CONVERT_EXPR:
    case NEGATE_EXPR:
    case NON_LVALUE_EXPR:
    case SAVE_EXPR:
      return tree_expr_maybe_signaling_nan_p (TREE_OPERAND (x, 0));
    case MIN_EXPR:
    case MAX_EXPR:
      return tree_expr_maybe_signaling_nan_p (TREE_OPERAND (x, 0))
	     || tree_expr_maybe_signaling_nan_p (TREE_OPERAND (x, 1));
    case COND_EXPR:
      return tree_expr_maybe_signaling_nan_p (TREE_OPERAND (x, 1))
	     || tree_expr_maybe_signaling_nan_p (TREE_OPERAND (x, 2));
    case CALL_EXPR:
      switch (get_call_combined_fn (x))
	{
	CASE_CFN_FABS:
	CASE_CFN_FABS_FN:
	  return tree_expr_maybe_signaling_nan_p (CALL_EXPR_ARG (x, 0));
	CASE_CFN_FMAX:
	CASE_CFN_FMAX_FN:
	CASE_CFN_FMIN:
	CASE_CFN_FMIN_FN:
	  return tree_expr_maybe_signaling_nan_p (CALL_EXPR_ARG (x, 0))
		 || tree_expr_maybe_signaling_nan_p (CALL_EXPR_ARG (x, 1));
	default:
	  return true;
	}
    default:
      return true;
    }
}

/* Return true if expression X evaluates to a NaN.
   This function returns false for integer expressions.  */

bool
tree_expr_nan_p (const_tree x)
{
  if (!HONOR_NANS (x))
    return false;
  switch (TREE_CODE (x))
    {
    case REAL_CST:
      return real_isnan (TREE_REAL_CST_PTR (x));
    case NON_LVALUE_EXPR:
    case SAVE_EXPR:
      return tree_expr_nan_p (TREE_OPERAND (x, 0));
    case COND_EXPR:
      return tree_expr_nan_p (TREE_OPERAND (x, 1))
	     && tree_expr_nan_p (TREE_OPERAND (x, 2));
    default:
      return false;
    }
}

/* Return true if expression X could evaluate to a NaN.
   This function returns false for integer expressions, and returns
   true if uncertain.  */

bool
tree_expr_maybe_nan_p (const_tree x)
{
  if (!HONOR_NANS (x))
    return false;
  switch (TREE_CODE (x))
    {
    case REAL_CST:
      return real_isnan (TREE_REAL_CST_PTR (x));
    case FLOAT_EXPR:
      return false;
    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
      return !tree_expr_finite_p (TREE_OPERAND (x, 0))
	     || !tree_expr_finite_p (TREE_OPERAND (x, 1));
    case ABS_EXPR:
    case CONVERT_EXPR:
    case NEGATE_EXPR:
    case NON_LVALUE_EXPR:
    case SAVE_EXPR:
      return tree_expr_maybe_nan_p (TREE_OPERAND (x, 0));
    case MIN_EXPR:
    case MAX_EXPR:
      return tree_expr_maybe_nan_p (TREE_OPERAND (x, 0))
	     || tree_expr_maybe_nan_p (TREE_OPERAND (x, 1));
    case COND_EXPR:
      return tree_expr_maybe_nan_p (TREE_OPERAND (x, 1))
	     || tree_expr_maybe_nan_p (TREE_OPERAND (x, 2));
    case CALL_EXPR:
      switch (get_call_combined_fn (x))
	{
	CASE_CFN_FABS:
	CASE_CFN_FABS_FN:
	  return tree_expr_maybe_nan_p (CALL_EXPR_ARG (x, 0));
	CASE_CFN_FMAX:
	CASE_CFN_FMAX_FN:
	CASE_CFN_FMIN:
	CASE_CFN_FMIN_FN:
	  return tree_expr_maybe_nan_p (CALL_EXPR_ARG (x, 0))
		 || tree_expr_maybe_nan_p (CALL_EXPR_ARG (x, 1));
	default:
	  return true;
	}
    default:
      return true;
    }
}

/* Return true if expression X could evaluate to -0.0.
   This function returns true if uncertain.  */

bool
tree_expr_maybe_real_minus_zero_p (const_tree x)
{
  if (!HONOR_SIGNED_ZEROS (x))
    return false;
  switch (TREE_CODE (x))
    {
    case REAL_CST:
      return REAL_VALUE_MINUS_ZERO (TREE_REAL_CST (x));
    case INTEGER_CST:
    case FLOAT_EXPR:
    case ABS_EXPR:
      return false;
    case NON_LVALUE_EXPR:
    case SAVE_EXPR:
      return tree_expr_maybe_real_minus_zero_p (TREE_OPERAND (x, 0));
    case COND_EXPR:
      return tree_expr_maybe_real_minus_zero_p (TREE_OPERAND (x, 1))
	     || tree_expr_maybe_real_minus_zero_p (TREE_OPERAND (x, 2));
    case CALL_EXPR:
      switch (get_call_combined_fn (x))
	{
	CASE_CFN_FABS:
	CASE_CFN_FABS_FN:
	  return false;
	default:
	  break;
	}
    default:
      break;
    }
  /* Ideally !(tree_expr_nonzero_p (X) || tree_expr_nonnegative_p (X))
   * but currently those predicates require tree and not const_tree.  */
  return true;
}

#define tree_expr_nonnegative_warnv_p(X, Y) \
  _Pragma ("GCC error \"Use RECURSE for recursive calls\"") 0

#define RECURSE(X) \
  ((tree_expr_nonnegative_warnv_p) (X, strict_overflow_p, depth + 1))

/* Return true if CODE or TYPE is known to be non-negative. */

static bool
tree_simple_nonnegative_warnv_p (enum tree_code code, tree type)
{
  if (!VECTOR_TYPE_P (type)
      && (TYPE_PRECISION (type) != 1 || TYPE_UNSIGNED (type))
      && truth_value_p (code))
    /* Truth values evaluate to 0 or 1, which is nonnegative unless we
       have a signed:1 type (where the value is -1 and 0).  */
    return true;
  return false;
}

/* Return true if (CODE OP0) is known to be non-negative.  If the return
   value is based on the assumption that signed overflow is undefined,
   set *STRICT_OVERFLOW_P to true; otherwise, don't change
   *STRICT_OVERFLOW_P.  DEPTH is the current nesting depth of the query.  */

bool
tree_unary_nonnegative_warnv_p (enum tree_code code, tree type, tree op0,
				bool *strict_overflow_p, int depth)
{
  if (TYPE_UNSIGNED (type))
    return true;

  switch (code)
    {
    case ABS_EXPR:
      /* We can't return 1 if flag_wrapv is set because
	 ABS_EXPR<INT_MIN> = INT_MIN.  */
      if (!ANY_INTEGRAL_TYPE_P (type))
	return true;
      if (TYPE_OVERFLOW_UNDEFINED (type))
	{
	  *strict_overflow_p = true;
	  return true;
	}
      break;

    case NON_LVALUE_EXPR:
    case FLOAT_EXPR:
    case FIX_TRUNC_EXPR:
      return RECURSE (op0);

    CASE_CONVERT:
      {
	tree inner_type = TREE_TYPE (op0);
	tree outer_type = type;

	if (SCALAR_FLOAT_TYPE_P (outer_type))
	  {
	    if (SCALAR_FLOAT_TYPE_P (inner_type))
	      return RECURSE (op0);
	    if (INTEGRAL_TYPE_P (inner_type))
	      {
		if (TYPE_UNSIGNED (inner_type))
		  return true;
		return RECURSE (op0);
	      }
	  }
	else if (INTEGRAL_TYPE_P (outer_type))
	  {
	    if (SCALAR_FLOAT_TYPE_P (inner_type))
	      return RECURSE (op0);
	    if (INTEGRAL_TYPE_P (inner_type))
	      return TYPE_PRECISION (inner_type) < TYPE_PRECISION (outer_type)
		      && TYPE_UNSIGNED (inner_type);
	  }
      }
      break;

    default:
      return tree_simple_nonnegative_warnv_p (code, type);
    }

  /* We don't know sign of `t', so be conservative and return false.  */
  return false;
}

/* Return true if (CODE OP0 OP1) is known to be non-negative.  If the return
   value is based on the assumption that signed overflow is undefined,
   set *STRICT_OVERFLOW_P to true; otherwise, don't change
   *STRICT_OVERFLOW_P.  DEPTH is the current nesting depth of the query.  */

bool
tree_binary_nonnegative_warnv_p (enum tree_code code, tree type, tree op0,
				 tree op1, bool *strict_overflow_p,
				 int depth)
{
  if (TYPE_UNSIGNED (type))
    return true;

  switch (code)
    {
    case POINTER_PLUS_EXPR:
    case PLUS_EXPR:
      if (FLOAT_TYPE_P (type))
	return RECURSE (op0) && RECURSE (op1);

      /* zero_extend(x) + zero_extend(y) is non-negative if x and y are
	 both unsigned and at least 2 bits shorter than the result.  */
      if (TREE_CODE (type) == INTEGER_TYPE
	  && TREE_CODE (op0) == NOP_EXPR
	  && TREE_CODE (op1) == NOP_EXPR)
	{
	  tree inner1 = TREE_TYPE (TREE_OPERAND (op0, 0));
	  tree inner2 = TREE_TYPE (TREE_OPERAND (op1, 0));
	  if (TREE_CODE (inner1) == INTEGER_TYPE && TYPE_UNSIGNED (inner1)
	      && TREE_CODE (inner2) == INTEGER_TYPE && TYPE_UNSIGNED (inner2))
	    {
	      unsigned int prec = MAX (TYPE_PRECISION (inner1),
				       TYPE_PRECISION (inner2)) + 1;
	      return prec < TYPE_PRECISION (type);
	    }
	}
      break;

    case MULT_EXPR:
      if (FLOAT_TYPE_P (type) || TYPE_OVERFLOW_UNDEFINED (type))
	{
	  /* x * x is always non-negative for floating point x
	     or without overflow.  */
	  if (operand_equal_p (op0, op1, 0)
	      || (RECURSE (op0) && RECURSE (op1)))
	    {
	      if (ANY_INTEGRAL_TYPE_P (type)
		  && TYPE_OVERFLOW_UNDEFINED (type))
		*strict_overflow_p = true;
	      return true;
	    }
	}

      /* zero_extend(x) * zero_extend(y) is non-negative if x and y are
	 both unsigned and their total bits is shorter than the result.  */
      if (TREE_CODE (type) == INTEGER_TYPE
	  && (TREE_CODE (op0) == NOP_EXPR || TREE_CODE (op0) == INTEGER_CST)
	  && (TREE_CODE (op1) == NOP_EXPR || TREE_CODE (op1) == INTEGER_CST))
	{
	  tree inner0 = (TREE_CODE (op0) == NOP_EXPR)
	    ? TREE_TYPE (TREE_OPERAND (op0, 0))
	    : TREE_TYPE (op0);
	  tree inner1 = (TREE_CODE (op1) == NOP_EXPR)
	    ? TREE_TYPE (TREE_OPERAND (op1, 0))
	    : TREE_TYPE (op1);

	  bool unsigned0 = TYPE_UNSIGNED (inner0);
	  bool unsigned1 = TYPE_UNSIGNED (inner1);

	  if (TREE_CODE (op0) == INTEGER_CST)
	    unsigned0 = unsigned0 || tree_int_cst_sgn (op0) >= 0;

	  if (TREE_CODE (op1) == INTEGER_CST)
	    unsigned1 = unsigned1 || tree_int_cst_sgn (op1) >= 0;

	  if (TREE_CODE (inner0) == INTEGER_TYPE && unsigned0
	      && TREE_CODE (inner1) == INTEGER_TYPE && unsigned1)
	    {
	      unsigned int precision0 = (TREE_CODE (op0) == INTEGER_CST)
		? tree_int_cst_min_precision (op0, UNSIGNED)
		: TYPE_PRECISION (inner0);

	      unsigned int precision1 = (TREE_CODE (op1) == INTEGER_CST)
		? tree_int_cst_min_precision (op1, UNSIGNED)
		: TYPE_PRECISION (inner1);

	      return precision0 + precision1 < TYPE_PRECISION (type);
	    }
	}
      return false;

    case BIT_AND_EXPR:
      return RECURSE (op0) || RECURSE (op1);

    case MAX_EXPR:
      /* Usually RECURSE (op0) || RECURSE (op1) but NaNs complicate
	 things.  */
      if (tree_expr_maybe_nan_p (op0) || tree_expr_maybe_nan_p (op1))
	return RECURSE (op0) && RECURSE (op1);
      return RECURSE (op0) || RECURSE (op1);

    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case MIN_EXPR:
    case RDIV_EXPR:
    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
      return RECURSE (op0) && RECURSE (op1);

    case TRUNC_MOD_EXPR:
      return RECURSE (op0);

    case FLOOR_MOD_EXPR:
      return RECURSE (op1);

    case CEIL_MOD_EXPR:
    case ROUND_MOD_EXPR:
    default:
      return tree_simple_nonnegative_warnv_p (code, type);
    }

  /* We don't know sign of `t', so be conservative and return false.  */
  return false;
}

/* Return true if T is known to be non-negative.  If the return
   value is based on the assumption that signed overflow is undefined,
   set *STRICT_OVERFLOW_P to true; otherwise, don't change
   *STRICT_OVERFLOW_P.  DEPTH is the current nesting depth of the query.  */

bool
tree_single_nonnegative_warnv_p (tree t, bool *strict_overflow_p, int depth)
{
  if (TYPE_UNSIGNED (TREE_TYPE (t)))
    return true;

  switch (TREE_CODE (t))
    {
    case INTEGER_CST:
      return tree_int_cst_sgn (t) >= 0;

    case REAL_CST:
      return ! REAL_VALUE_NEGATIVE (TREE_REAL_CST (t));

    case FIXED_CST:
      return ! FIXED_VALUE_NEGATIVE (TREE_FIXED_CST (t));

    case COND_EXPR:
      return RECURSE (TREE_OPERAND (t, 1)) && RECURSE (TREE_OPERAND (t, 2));

    case SSA_NAME:
      /* Limit the depth of recursion to avoid quadratic behavior.
	 This is expected to catch almost all occurrences in practice.
	 If this code misses important cases that unbounded recursion
	 would not, passes that need this information could be revised
	 to provide it through dataflow propagation.  */
      return (!name_registered_for_update_p (t)
	      && depth < param_max_ssa_name_query_depth
	      && gimple_stmt_nonnegative_warnv_p (SSA_NAME_DEF_STMT (t),
						  strict_overflow_p, depth));

    default:
      return tree_simple_nonnegative_warnv_p (TREE_CODE (t), TREE_TYPE (t));
    }
}

/* Return true if T is known to be non-negative.  If the return
   value is based on the assumption that signed overflow is undefined,
   set *STRICT_OVERFLOW_P to true; otherwise, don't change
   *STRICT_OVERFLOW_P.  DEPTH is the current nesting depth of the query.  */

bool
tree_call_nonnegative_warnv_p (tree type, combined_fn fn, tree arg0, tree arg1,
			       bool *strict_overflow_p, int depth)
{
  switch (fn)
    {
    CASE_CFN_ACOS:
    CASE_CFN_ACOS_FN:
    CASE_CFN_ACOSH:
    CASE_CFN_ACOSH_FN:
    CASE_CFN_CABS:
    CASE_CFN_CABS_FN:
    CASE_CFN_COSH:
    CASE_CFN_COSH_FN:
    CASE_CFN_ERFC:
    CASE_CFN_ERFC_FN:
    CASE_CFN_EXP:
    CASE_CFN_EXP_FN:
    CASE_CFN_EXP10:
    CASE_CFN_EXP2:
    CASE_CFN_EXP2_FN:
    CASE_CFN_FABS:
    CASE_CFN_FABS_FN:
    CASE_CFN_FDIM:
    CASE_CFN_FDIM_FN:
    CASE_CFN_HYPOT:
    CASE_CFN_HYPOT_FN:
    CASE_CFN_POW10:
    CASE_CFN_FFS:
    CASE_CFN_PARITY:
    CASE_CFN_POPCOUNT:
    CASE_CFN_CLRSB:
    case CFN_BUILT_IN_BSWAP16:
    case CFN_BUILT_IN_BSWAP32:
    case CFN_BUILT_IN_BSWAP64:
    case CFN_BUILT_IN_BSWAP128:
      /* Always true.  */
      return true;

    CASE_CFN_CLZ:
    CASE_CFN_CTZ:
      if (arg1)
	return RECURSE (arg1);
      return true;

    CASE_CFN_SQRT:
    CASE_CFN_SQRT_FN:
      /* sqrt(-0.0) is -0.0.  */
      if (!HONOR_SIGNED_ZEROS (type))
	return true;
      return RECURSE (arg0);

    CASE_CFN_ASINH:
    CASE_CFN_ASINH_FN:
    CASE_CFN_ATAN:
    CASE_CFN_ATAN_FN:
    CASE_CFN_ATANH:
    CASE_CFN_ATANH_FN:
    CASE_CFN_CBRT:
    CASE_CFN_CBRT_FN:
    CASE_CFN_CEIL:
    CASE_CFN_CEIL_FN:
    CASE_CFN_ERF:
    CASE_CFN_ERF_FN:
    CASE_CFN_EXPM1:
    CASE_CFN_EXPM1_FN:
    CASE_CFN_FLOOR:
    CASE_CFN_FLOOR_FN:
    CASE_CFN_FMOD:
    CASE_CFN_FMOD_FN:
    CASE_CFN_FREXP:
    CASE_CFN_FREXP_FN:
    CASE_CFN_ICEIL:
    CASE_CFN_IFLOOR:
    CASE_CFN_IRINT:
    CASE_CFN_IROUND:
    CASE_CFN_LCEIL:
    CASE_CFN_LDEXP:
    CASE_CFN_LFLOOR:
    CASE_CFN_LLCEIL:
    CASE_CFN_LLFLOOR:
    CASE_CFN_LLRINT:
    CASE_CFN_LLRINT_FN:
    CASE_CFN_LLROUND:
    CASE_CFN_LLROUND_FN:
    CASE_CFN_LRINT:
    CASE_CFN_LRINT_FN:
    CASE_CFN_LROUND:
    CASE_CFN_LROUND_FN:
    CASE_CFN_MODF:
    CASE_CFN_MODF_FN:
    CASE_CFN_NEARBYINT:
    CASE_CFN_NEARBYINT_FN:
    CASE_CFN_RINT:
    CASE_CFN_RINT_FN:
    CASE_CFN_ROUND:
    CASE_CFN_ROUND_FN:
    CASE_CFN_ROUNDEVEN:
    CASE_CFN_ROUNDEVEN_FN:
    CASE_CFN_SCALB:
    CASE_CFN_SCALBLN:
    CASE_CFN_SCALBLN_FN:
    CASE_CFN_SCALBN:
    CASE_CFN_SCALBN_FN:
    CASE_CFN_SIGNBIT:
    CASE_CFN_SIGNIFICAND:
    CASE_CFN_SINH:
    CASE_CFN_SINH_FN:
    CASE_CFN_TANH:
    CASE_CFN_TANH_FN:
    CASE_CFN_TRUNC:
    CASE_CFN_TRUNC_FN:
      /* True if the 1st argument is nonnegative.  */
      return RECURSE (arg0);

    CASE_CFN_FMAX:
    CASE_CFN_FMAX_FN:
      /* Usually RECURSE (arg0) || RECURSE (arg1) but NaNs complicate
	 things.  In the presence of sNaNs, we're only guaranteed to be
	 non-negative if both operands are non-negative.  In the presence
	 of qNaNs, we're non-negative if either operand is non-negative
	 and can't be a qNaN, or if both operands are non-negative.  */
      if (tree_expr_maybe_signaling_nan_p (arg0)
	  || tree_expr_maybe_signaling_nan_p (arg1))
        return RECURSE (arg0) && RECURSE (arg1);
      return RECURSE (arg0) ? (!tree_expr_maybe_nan_p (arg0)
			       || RECURSE (arg1))
			    : (RECURSE (arg1)
			       && !tree_expr_maybe_nan_p (arg1));

    CASE_CFN_FMIN:
    CASE_CFN_FMIN_FN:
      /* True if the 1st AND 2nd arguments are nonnegative.  */
      return RECURSE (arg0) && RECURSE (arg1);

    CASE_CFN_COPYSIGN:
    CASE_CFN_COPYSIGN_FN:
      /* True if the 2nd argument is nonnegative.  */
      return RECURSE (arg1);

    CASE_CFN_POWI:
      /* True if the 1st argument is nonnegative or the second
	 argument is an even integer.  */
      if (TREE_CODE (arg1) == INTEGER_CST
	  && (TREE_INT_CST_LOW (arg1) & 1) == 0)
	return true;
      return RECURSE (arg0);

    CASE_CFN_POW:
    CASE_CFN_POW_FN:
      /* True if the 1st argument is nonnegative or the second
	 argument is an even integer valued real.  */
      if (TREE_CODE (arg1) == REAL_CST)
	{
	  REAL_VALUE_TYPE c;
	  HOST_WIDE_INT n;

	  c = TREE_REAL_CST (arg1);
	  n = real_to_integer (&c);
	  if ((n & 1) == 0)
	    {
	      REAL_VALUE_TYPE cint;
	      real_from_integer (&cint, VOIDmode, n, SIGNED);
	      if (real_identical (&c, &cint))
		return true;
	    }
	}
      return RECURSE (arg0);

    default:
      break;
    }
  return tree_simple_nonnegative_warnv_p (CALL_EXPR, type);
}

/* Return true if T is known to be non-negative.  If the return
   value is based on the assumption that signed overflow is undefined,
   set *STRICT_OVERFLOW_P to true; otherwise, don't change
   *STRICT_OVERFLOW_P.  DEPTH is the current nesting depth of the query.  */

static bool
tree_invalid_nonnegative_warnv_p (tree t, bool *strict_overflow_p, int depth)
{
  enum tree_code code = TREE_CODE (t);
  if (TYPE_UNSIGNED (TREE_TYPE (t)))
    return true;

  switch (code)
    {
    case TARGET_EXPR:
      {
	tree temp = TARGET_EXPR_SLOT (t);
	t = TARGET_EXPR_INITIAL (t);

	/* If the initializer is non-void, then it's a normal expression
	   that will be assigned to the slot.  */
	if (!VOID_TYPE_P (TREE_TYPE (t)))
	  return RECURSE (t);

	/* Otherwise, the initializer sets the slot in some way.  One common
	   way is an assignment statement at the end of the initializer.  */
	while (1)
	  {
	    if (TREE_CODE (t) == BIND_EXPR)
	      t = expr_last (BIND_EXPR_BODY (t));
	    else if (TREE_CODE (t) == TRY_FINALLY_EXPR
		     || TREE_CODE (t) == TRY_CATCH_EXPR)
	      t = expr_last (TREE_OPERAND (t, 0));
	    else if (TREE_CODE (t) == STATEMENT_LIST)
	      t = expr_last (t);
	    else
	      break;
	  }
	if (TREE_CODE (t) == MODIFY_EXPR
	    && TREE_OPERAND (t, 0) == temp)
	  return RECURSE (TREE_OPERAND (t, 1));

	return false;
      }

    case CALL_EXPR:
      {
	tree arg0 = call_expr_nargs (t) > 0 ? CALL_EXPR_ARG (t, 0) : NULL_TREE;
	tree arg1 = call_expr_nargs (t) > 1 ? CALL_EXPR_ARG (t, 1) : NULL_TREE;

	return tree_call_nonnegative_warnv_p (TREE_TYPE (t),
					      get_call_combined_fn (t),
					      arg0,
					      arg1,
					      strict_overflow_p, depth);
      }
    case COMPOUND_EXPR:
    case MODIFY_EXPR:
      return RECURSE (TREE_OPERAND (t, 1));

    case BIND_EXPR:
      return RECURSE (expr_last (TREE_OPERAND (t, 1)));

    case SAVE_EXPR:
      return RECURSE (TREE_OPERAND (t, 0));

    default:
      return tree_simple_nonnegative_warnv_p (TREE_CODE (t), TREE_TYPE (t));
    }
}

#undef RECURSE
#undef tree_expr_nonnegative_warnv_p

/* Return true if T is known to be non-negative.  If the return
   value is based on the assumption that signed overflow is undefined,
   set *STRICT_OVERFLOW_P to true; otherwise, don't change
   *STRICT_OVERFLOW_P.  DEPTH is the current nesting depth of the query.  */

bool
tree_expr_nonnegative_warnv_p (tree t, bool *strict_overflow_p, int depth)
{
  enum tree_code code;
  if (t == error_mark_node)
    return false;

  code = TREE_CODE (t);
  switch (TREE_CODE_CLASS (code))
    {
    case tcc_binary:
    case tcc_comparison:
      return tree_binary_nonnegative_warnv_p (TREE_CODE (t),
					      TREE_TYPE (t),
					      TREE_OPERAND (t, 0),
					      TREE_OPERAND (t, 1),
					      strict_overflow_p, depth);

    case tcc_unary:
      return tree_unary_nonnegative_warnv_p (TREE_CODE (t),
					     TREE_TYPE (t),
					     TREE_OPERAND (t, 0),
					     strict_overflow_p, depth);

    case tcc_constant:
    case tcc_declaration:
    case tcc_reference:
      return tree_single_nonnegative_warnv_p (t, strict_overflow_p, depth);

    default:
      break;
    }

  switch (code)
    {
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
      return tree_binary_nonnegative_warnv_p (TREE_CODE (t),
					      TREE_TYPE (t),
					      TREE_OPERAND (t, 0),
					      TREE_OPERAND (t, 1),
					      strict_overflow_p, depth);
    case TRUTH_NOT_EXPR:
      return tree_unary_nonnegative_warnv_p (TREE_CODE (t),
					     TREE_TYPE (t),
					     TREE_OPERAND (t, 0),
					     strict_overflow_p, depth);

    case COND_EXPR:
    case CONSTRUCTOR:
    case OBJ_TYPE_REF:
    case ADDR_EXPR:
    case WITH_SIZE_EXPR:
    case SSA_NAME:
      return tree_single_nonnegative_warnv_p (t, strict_overflow_p, depth);

    default:
      return tree_invalid_nonnegative_warnv_p (t, strict_overflow_p, depth);
    }
}

/* Return true if `t' is known to be non-negative.  Handle warnings
   about undefined signed overflow.  */

bool
tree_expr_nonnegative_p (tree t)
{
  bool ret, strict_overflow_p;

  strict_overflow_p = false;
  ret = tree_expr_nonnegative_warnv_p (t, &strict_overflow_p);
  if (strict_overflow_p)
    fold_overflow_warning (("assuming signed overflow does not occur when "
			    "determining that expression is always "
			    "non-negative"),
			   WARN_STRICT_OVERFLOW_MISC);
  return ret;
}


/* Return true when (CODE OP0) is an address and is known to be nonzero.
   For floating point we further ensure that T is not denormal.
   Similar logic is present in nonzero_address in rtlanal.h.

   If the return value is based on the assumption that signed overflow
   is undefined, set *STRICT_OVERFLOW_P to true; otherwise, don't
   change *STRICT_OVERFLOW_P.  */

bool
tree_unary_nonzero_warnv_p (enum tree_code code, tree type, tree op0,
				 bool *strict_overflow_p)
{
  switch (code)
    {
    case ABS_EXPR:
      return tree_expr_nonzero_warnv_p (op0,
					strict_overflow_p);

    case NOP_EXPR:
      {
	tree inner_type = TREE_TYPE (op0);
	tree outer_type = type;

	return (TYPE_PRECISION (outer_type) >= TYPE_PRECISION (inner_type)
		&& tree_expr_nonzero_warnv_p (op0,
					      strict_overflow_p));
      }
      break;

    case NON_LVALUE_EXPR:
      return tree_expr_nonzero_warnv_p (op0,
					strict_overflow_p);

    default:
      break;
  }

  return false;
}

/* Return true when (CODE OP0 OP1) is an address and is known to be nonzero.
   For floating point we further ensure that T is not denormal.
   Similar logic is present in nonzero_address in rtlanal.h.

   If the return value is based on the assumption that signed overflow
   is undefined, set *STRICT_OVERFLOW_P to true; otherwise, don't
   change *STRICT_OVERFLOW_P.  */

bool
tree_binary_nonzero_warnv_p (enum tree_code code,
			     tree type,
			     tree op0,
			     tree op1, bool *strict_overflow_p)
{
  bool sub_strict_overflow_p;
  switch (code)
    {
    case POINTER_PLUS_EXPR:
    case PLUS_EXPR:
      if (ANY_INTEGRAL_TYPE_P (type) && TYPE_OVERFLOW_UNDEFINED (type))
	{
	  /* With the presence of negative values it is hard
	     to say something.  */
	  sub_strict_overflow_p = false;
	  if (!tree_expr_nonnegative_warnv_p (op0,
					      &sub_strict_overflow_p)
	      || !tree_expr_nonnegative_warnv_p (op1,
						 &sub_strict_overflow_p))
	    return false;
	  /* One of operands must be positive and the other non-negative.  */
	  /* We don't set *STRICT_OVERFLOW_P here: even if this value
	     overflows, on a twos-complement machine the sum of two
	     nonnegative numbers can never be zero.  */
	  return (tree_expr_nonzero_warnv_p (op0,
					     strict_overflow_p)
		  || tree_expr_nonzero_warnv_p (op1,
						strict_overflow_p));
	}
      break;

    case MULT_EXPR:
      if (TYPE_OVERFLOW_UNDEFINED (type))
	{
	  if (tree_expr_nonzero_warnv_p (op0,
					 strict_overflow_p)
	      && tree_expr_nonzero_warnv_p (op1,
					    strict_overflow_p))
	    {
	      *strict_overflow_p = true;
	      return true;
	    }
	}
      break;

    case MIN_EXPR:
      sub_strict_overflow_p = false;
      if (tree_expr_nonzero_warnv_p (op0,
				     &sub_strict_overflow_p)
	  && tree_expr_nonzero_warnv_p (op1,
					&sub_strict_overflow_p))
	{
	  if (sub_strict_overflow_p)
	    *strict_overflow_p = true;
	}
      break;

    case MAX_EXPR:
      sub_strict_overflow_p = false;
      if (tree_expr_nonzero_warnv_p (op0,
				     &sub_strict_overflow_p))
	{
	  if (sub_strict_overflow_p)
	    *strict_overflow_p = true;

	  /* When both operands are nonzero, then MAX must be too.  */
	  if (tree_expr_nonzero_warnv_p (op1,
					 strict_overflow_p))
	    return true;

	  /* MAX where operand 0 is positive is positive.  */
	  return tree_expr_nonnegative_warnv_p (op0,
					       strict_overflow_p);
	}
      /* MAX where operand 1 is positive is positive.  */
      else if (tree_expr_nonzero_warnv_p (op1,
					  &sub_strict_overflow_p)
	       && tree_expr_nonnegative_warnv_p (op1,
						 &sub_strict_overflow_p))
	{
	  if (sub_strict_overflow_p)
	    *strict_overflow_p = true;
	  return true;
	}
      break;

    case BIT_IOR_EXPR:
      return (tree_expr_nonzero_warnv_p (op1,
					 strict_overflow_p)
	      || tree_expr_nonzero_warnv_p (op0,
					    strict_overflow_p));

    default:
      break;
  }

  return false;
}

/* Return true when T is an address and is known to be nonzero.
   For floating point we further ensure that T is not denormal.
   Similar logic is present in nonzero_address in rtlanal.h.

   If the return value is based on the assumption that signed overflow
   is undefined, set *STRICT_OVERFLOW_P to true; otherwise, don't
   change *STRICT_OVERFLOW_P.  */

bool
tree_single_nonzero_warnv_p (tree t, bool *strict_overflow_p)
{
  bool sub_strict_overflow_p;
  switch (TREE_CODE (t))
    {
    case INTEGER_CST:
      return !integer_zerop (t);

    case ADDR_EXPR:
      {
	tree base = TREE_OPERAND (t, 0);

	if (!DECL_P (base))
	  base = get_base_address (base);

	if (base && TREE_CODE (base) == TARGET_EXPR)
	  base = TARGET_EXPR_SLOT (base);

	if (!base)
	  return false;

	/* For objects in symbol table check if we know they are non-zero.
	   Don't do anything for variables and functions before symtab is built;
	   it is quite possible that they will be declared weak later.  */
	int nonzero_addr = maybe_nonzero_address (base);
	if (nonzero_addr >= 0)
	  return nonzero_addr;

	/* Constants are never weak.  */
	if (CONSTANT_CLASS_P (base))
	  return true;

	return false;
      }

    case COND_EXPR:
      sub_strict_overflow_p = false;
      if (tree_expr_nonzero_warnv_p (TREE_OPERAND (t, 1),
				     &sub_strict_overflow_p)
	  && tree_expr_nonzero_warnv_p (TREE_OPERAND (t, 2),
					&sub_strict_overflow_p))
	{
	  if (sub_strict_overflow_p)
	    *strict_overflow_p = true;
	  return true;
	}
      break;

    case SSA_NAME:
      if (!INTEGRAL_TYPE_P (TREE_TYPE (t)))
	break;
      return expr_not_equal_to (t, wi::zero (TYPE_PRECISION (TREE_TYPE (t))));

    default:
      break;
    }
  return false;
}

#define integer_valued_real_p(X) \
  _Pragma ("GCC error \"Use RECURSE for recursive calls\"") 0

#define RECURSE(X) \
  ((integer_valued_real_p) (X, depth + 1))

/* Return true if the floating point result of (CODE OP0) has an
   integer value.  We also allow +Inf, -Inf and NaN to be considered
   integer values. Return false for signaling NaN.

   DEPTH is the current nesting depth of the query.  */

bool
integer_valued_real_unary_p (tree_code code, tree op0, int depth)
{
  switch (code)
    {
    case FLOAT_EXPR:
      return true;

    case ABS_EXPR:
      return RECURSE (op0);

    CASE_CONVERT:
      {
	tree type = TREE_TYPE (op0);
	if (TREE_CODE (type) == INTEGER_TYPE)
	  return true;
	if (SCALAR_FLOAT_TYPE_P (type))
	  return RECURSE (op0);
	break;
      }

    default:
      break;
    }
  return false;
}

/* Return true if the floating point result of (CODE OP0 OP1) has an
   integer value.  We also allow +Inf, -Inf and NaN to be considered
   integer values. Return false for signaling NaN.

   DEPTH is the current nesting depth of the query.  */

bool
integer_valued_real_binary_p (tree_code code, tree op0, tree op1, int depth)
{
  switch (code)
    {
    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case MIN_EXPR:
    case MAX_EXPR:
      return RECURSE (op0) && RECURSE (op1);

    default:
      break;
    }
  return false;
}

/* Return true if the floating point result of calling FNDECL with arguments
   ARG0 and ARG1 has an integer value.  We also allow +Inf, -Inf and NaN to be
   considered integer values. Return false for signaling NaN.  If FNDECL
   takes fewer than 2 arguments, the remaining ARGn are null.

   DEPTH is the current nesting depth of the query.  */

bool
integer_valued_real_call_p (combined_fn fn, tree arg0, tree arg1, int depth)
{
  switch (fn)
    {
    CASE_CFN_CEIL:
    CASE_CFN_CEIL_FN:
    CASE_CFN_FLOOR:
    CASE_CFN_FLOOR_FN:
    CASE_CFN_NEARBYINT:
    CASE_CFN_NEARBYINT_FN:
    CASE_CFN_RINT:
    CASE_CFN_RINT_FN:
    CASE_CFN_ROUND:
    CASE_CFN_ROUND_FN:
    CASE_CFN_ROUNDEVEN:
    CASE_CFN_ROUNDEVEN_FN:
    CASE_CFN_TRUNC:
    CASE_CFN_TRUNC_FN:
      return true;

    CASE_CFN_FMIN:
    CASE_CFN_FMIN_FN:
    CASE_CFN_FMAX:
    CASE_CFN_FMAX_FN:
      return RECURSE (arg0) && RECURSE (arg1);

    default:
      break;
    }
  return false;
}

/* Return true if the floating point expression T (a GIMPLE_SINGLE_RHS)
   has an integer value.  We also allow +Inf, -Inf and NaN to be
   considered integer values. Return false for signaling NaN.

   DEPTH is the current nesting depth of the query.  */

bool
integer_valued_real_single_p (tree t, int depth)
{
  switch (TREE_CODE (t))
    {
    case REAL_CST:
      return real_isinteger (TREE_REAL_CST_PTR (t), TYPE_MODE (TREE_TYPE (t)));

    case COND_EXPR:
      return RECURSE (TREE_OPERAND (t, 1)) && RECURSE (TREE_OPERAND (t, 2));

    case SSA_NAME:
      /* Limit the depth of recursion to avoid quadratic behavior.
	 This is expected to catch almost all occurrences in practice.
	 If this code misses important cases that unbounded recursion
	 would not, passes that need this information could be revised
	 to provide it through dataflow propagation.  */
      return (!name_registered_for_update_p (t)
	      && depth < param_max_ssa_name_query_depth
	      && gimple_stmt_integer_valued_real_p (SSA_NAME_DEF_STMT (t),
						    depth));

    default:
      break;
    }
  return false;
}

/* Return true if the floating point expression T (a GIMPLE_INVALID_RHS)
   has an integer value.  We also allow +Inf, -Inf and NaN to be
   considered integer values. Return false for signaling NaN.

   DEPTH is the current nesting depth of the query.  */

static bool
integer_valued_real_invalid_p (tree t, int depth)
{
  switch (TREE_CODE (t))
    {
    case COMPOUND_EXPR:
    case MODIFY_EXPR:
    case BIND_EXPR:
      return RECURSE (TREE_OPERAND (t, 1));

    case SAVE_EXPR:
      return RECURSE (TREE_OPERAND (t, 0));

    default:
      break;
    }
  return false;
}

#undef RECURSE
#undef integer_valued_real_p

/* Return true if the floating point expression T has an integer value.
   We also allow +Inf, -Inf and NaN to be considered integer values.
   Return false for signaling NaN.

   DEPTH is the current nesting depth of the query.  */

bool
integer_valued_real_p (tree t, int depth)
{
  if (t == error_mark_node)
    return false;

  STRIP_ANY_LOCATION_WRAPPER (t);

  tree_code code = TREE_CODE (t);
  switch (TREE_CODE_CLASS (code))
    {
    case tcc_binary:
    case tcc_comparison:
      return integer_valued_real_binary_p (code, TREE_OPERAND (t, 0),
					   TREE_OPERAND (t, 1), depth);

    case tcc_unary:
      return integer_valued_real_unary_p (code, TREE_OPERAND (t, 0), depth);

    case tcc_constant:
    case tcc_declaration:
    case tcc_reference:
      return integer_valued_real_single_p (t, depth);

    default:
      break;
    }

  switch (code)
    {
    case COND_EXPR:
    case SSA_NAME:
      return integer_valued_real_single_p (t, depth);

    case CALL_EXPR:
      {
	tree arg0 = (call_expr_nargs (t) > 0
		     ? CALL_EXPR_ARG (t, 0)
		     : NULL_TREE);
	tree arg1 = (call_expr_nargs (t) > 1
		     ? CALL_EXPR_ARG (t, 1)
		     : NULL_TREE);
	return integer_valued_real_call_p (get_call_combined_fn (t),
					   arg0, arg1, depth);
      }

    default:
      return integer_valued_real_invalid_p (t, depth);
    }
}

/* Given the components of a binary expression CODE, TYPE, OP0 and OP1,
   attempt to fold the expression to a constant without modifying TYPE,
   OP0 or OP1.

   If the expression could be simplified to a constant, then return
   the constant.  If the expression would not be simplified to a
   constant, then return NULL_TREE.  */

tree
fold_binary_to_constant (enum tree_code code, tree type, tree op0, tree op1)
{
  tree tem = fold_binary (code, type, op0, op1);
  return (tem && TREE_CONSTANT (tem)) ? tem : NULL_TREE;
}

/* Given the components of a unary expression CODE, TYPE and OP0,
   attempt to fold the expression to a constant without modifying
   TYPE or OP0.

   If the expression could be simplified to a constant, then return
   the constant.  If the expression would not be simplified to a
   constant, then return NULL_TREE.  */

tree
fold_unary_to_constant (enum tree_code code, tree type, tree op0)
{
  tree tem = fold_unary (code, type, op0);
  return (tem && TREE_CONSTANT (tem)) ? tem : NULL_TREE;
}

/* If EXP represents referencing an element in a constant string
   (either via pointer arithmetic or array indexing), return the
   tree representing the value accessed, otherwise return NULL.  */

tree
fold_read_from_constant_string (tree exp)
{
  if ((INDIRECT_REF_P (exp)
       || TREE_CODE (exp) == ARRAY_REF)
      && TREE_CODE (TREE_TYPE (exp)) == INTEGER_TYPE)
    {
      tree exp1 = TREE_OPERAND (exp, 0);
      tree index;
      tree string;
      location_t loc = EXPR_LOCATION (exp);

      if (INDIRECT_REF_P (exp))
	string = string_constant (exp1, &index, NULL, NULL);
      else
	{
	  tree low_bound = array_ref_low_bound (exp);
	  index = fold_convert_loc (loc, sizetype, TREE_OPERAND (exp, 1));

	  /* Optimize the special-case of a zero lower bound.

	     We convert the low_bound to sizetype to avoid some problems
	     with constant folding.  (E.g. suppose the lower bound is 1,
	     and its mode is QI.  Without the conversion,l (ARRAY
	     +(INDEX-(unsigned char)1)) becomes ((ARRAY+(-(unsigned char)1))
	     +INDEX), which becomes (ARRAY+255+INDEX).  Oops!)  */
	  if (! integer_zerop (low_bound))
	    index = size_diffop_loc (loc, index,
				 fold_convert_loc (loc, sizetype, low_bound));

	  string = exp1;
	}

      scalar_int_mode char_mode;
      if (string
	  && TYPE_MODE (TREE_TYPE (exp)) == TYPE_MODE (TREE_TYPE (TREE_TYPE (string)))
	  && TREE_CODE (string) == STRING_CST
	  && tree_fits_uhwi_p (index)
	  && compare_tree_int (index, TREE_STRING_LENGTH (string)) < 0
	  && is_int_mode (TYPE_MODE (TREE_TYPE (TREE_TYPE (string))),
			  &char_mode)
	  && GET_MODE_SIZE (char_mode) == 1)
	return build_int_cst_type (TREE_TYPE (exp),
				   (TREE_STRING_POINTER (string)
				    [TREE_INT_CST_LOW (index)]));
    }
  return NULL;
}

/* Folds a read from vector element at IDX of vector ARG.  */

tree
fold_read_from_vector (tree arg, poly_uint64 idx)
{
  unsigned HOST_WIDE_INT i;
  if (known_lt (idx, TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg)))
      && known_ge (idx, 0u)
      && idx.is_constant (&i))
    {
      if (TREE_CODE (arg) == VECTOR_CST)
	return VECTOR_CST_ELT (arg, i);
      else if (TREE_CODE (arg) == CONSTRUCTOR)
	{
	  if (CONSTRUCTOR_NELTS (arg)
	      && VECTOR_TYPE_P (TREE_TYPE (CONSTRUCTOR_ELT (arg, 0)->value)))
	    return NULL_TREE;
	  if (i >= CONSTRUCTOR_NELTS (arg))
	    return build_zero_cst (TREE_TYPE (TREE_TYPE (arg)));
	  return CONSTRUCTOR_ELT (arg, i)->value;
	}
    }
  return NULL_TREE;
}

/* Return the tree for neg (ARG0) when ARG0 is known to be either
   an integer constant, real, or fixed-point constant.

   TYPE is the type of the result.  */

static tree
fold_negate_const (tree arg0, tree type)
{
  tree t = NULL_TREE;

  switch (TREE_CODE (arg0))
    {
    case REAL_CST:
      t = build_real (type, real_value_negate (&TREE_REAL_CST (arg0)));
      break;

    case FIXED_CST:
      {
        FIXED_VALUE_TYPE f;
        bool overflow_p = fixed_arithmetic (&f, NEGATE_EXPR,
					    &(TREE_FIXED_CST (arg0)), NULL,
					    TYPE_SATURATING (type));
	t = build_fixed (type, f);
	/* Propagate overflow flags.  */
	if (overflow_p | TREE_OVERFLOW (arg0))
	  TREE_OVERFLOW (t) = 1;
	break;
      }

    default:
      if (poly_int_tree_p (arg0))
	{
	  wi::overflow_type overflow;
	  poly_wide_int res = wi::neg (wi::to_poly_wide (arg0), &overflow);
	  t = force_fit_type (type, res, 1,
			      (overflow && ! TYPE_UNSIGNED (type))
			      || TREE_OVERFLOW (arg0));
	  break;
	}

      gcc_unreachable ();
    }

  return t;
}

/* Return the tree for abs (ARG0) when ARG0 is known to be either
   an integer constant or real constant.

   TYPE is the type of the result.  */

tree
fold_abs_const (tree arg0, tree type)
{
  tree t = NULL_TREE;

  switch (TREE_CODE (arg0))
    {
    case INTEGER_CST:
      {
        /* If the value is unsigned or non-negative, then the absolute value
	   is the same as the ordinary value.  */
	wide_int val = wi::to_wide (arg0);
	wi::overflow_type overflow = wi::OVF_NONE;
	if (!wi::neg_p (val, TYPE_SIGN (TREE_TYPE (arg0))))
	  ;

	/* If the value is negative, then the absolute value is
	   its negation.  */
	else
	  val = wi::neg (val, &overflow);

	/* Force to the destination type, set TREE_OVERFLOW for signed
	   TYPE only.  */
	t = force_fit_type (type, val, 1, overflow | TREE_OVERFLOW (arg0));
      }
    break;

    case REAL_CST:
      if (REAL_VALUE_NEGATIVE (TREE_REAL_CST (arg0)))
	t = build_real (type, real_value_negate (&TREE_REAL_CST (arg0)));
      else
	t =  arg0;
      break;

    default:
      gcc_unreachable ();
    }

  return t;
}

/* Return the tree for not (ARG0) when ARG0 is known to be an integer
   constant.  TYPE is the type of the result.  */

static tree
fold_not_const (const_tree arg0, tree type)
{
  gcc_assert (TREE_CODE (arg0) == INTEGER_CST);

  return force_fit_type (type, ~wi::to_wide (arg0), 0, TREE_OVERFLOW (arg0));
}

/* Given CODE, a relational operator, the target type, TYPE and two
   constant operands OP0 and OP1, return the result of the
   relational operation.  If the result is not a compile time
   constant, then return NULL_TREE.  */

static tree
fold_relational_const (enum tree_code code, tree type, tree op0, tree op1)
{
  int result, invert;

  /* From here on, the only cases we handle are when the result is
     known to be a constant.  */

  if (TREE_CODE (op0) == REAL_CST && TREE_CODE (op1) == REAL_CST)
    {
      const REAL_VALUE_TYPE *c0 = TREE_REAL_CST_PTR (op0);
      const REAL_VALUE_TYPE *c1 = TREE_REAL_CST_PTR (op1);

      /* Handle the cases where either operand is a NaN.  */
      if (real_isnan (c0) || real_isnan (c1))
	{
	  switch (code)
	    {
	    case EQ_EXPR:
	    case ORDERED_EXPR:
	      result = 0;
	      break;

	    case NE_EXPR:
	    case UNORDERED_EXPR:
	    case UNLT_EXPR:
	    case UNLE_EXPR:
	    case UNGT_EXPR:
	    case UNGE_EXPR:
	    case UNEQ_EXPR:
              result = 1;
	      break;

	    case LT_EXPR:
	    case LE_EXPR:
	    case GT_EXPR:
	    case GE_EXPR:
	    case LTGT_EXPR:
	      if (flag_trapping_math)
		return NULL_TREE;
	      result = 0;
	      break;

	    default:
	      gcc_unreachable ();
	    }

	  return constant_boolean_node (result, type);
	}

      return constant_boolean_node (real_compare (code, c0, c1), type);
    }

  if (TREE_CODE (op0) == FIXED_CST && TREE_CODE (op1) == FIXED_CST)
    {
      const FIXED_VALUE_TYPE *c0 = TREE_FIXED_CST_PTR (op0);
      const FIXED_VALUE_TYPE *c1 = TREE_FIXED_CST_PTR (op1);
      return constant_boolean_node (fixed_compare (code, c0, c1), type);
    }

  /* Handle equality/inequality of complex constants.  */
  if (TREE_CODE (op0) == COMPLEX_CST && TREE_CODE (op1) == COMPLEX_CST)
    {
      tree rcond = fold_relational_const (code, type,
					  TREE_REALPART (op0),
					  TREE_REALPART (op1));
      tree icond = fold_relational_const (code, type,
					  TREE_IMAGPART (op0),
					  TREE_IMAGPART (op1));
      if (code == EQ_EXPR)
	return fold_build2 (TRUTH_ANDIF_EXPR, type, rcond, icond);
      else if (code == NE_EXPR)
	return fold_build2 (TRUTH_ORIF_EXPR, type, rcond, icond);
      else
	return NULL_TREE;
    }

  if (TREE_CODE (op0) == VECTOR_CST && TREE_CODE (op1) == VECTOR_CST)
    {
      if (!VECTOR_TYPE_P (type))
	{
	  /* Have vector comparison with scalar boolean result.  */
	  gcc_assert ((code == EQ_EXPR || code == NE_EXPR)
		      && known_eq (VECTOR_CST_NELTS (op0),
				   VECTOR_CST_NELTS (op1)));
	  unsigned HOST_WIDE_INT nunits;
	  if (!VECTOR_CST_NELTS (op0).is_constant (&nunits))
	    return NULL_TREE;
	  for (unsigned i = 0; i < nunits; i++)
	    {
	      tree elem0 = VECTOR_CST_ELT (op0, i);
	      tree elem1 = VECTOR_CST_ELT (op1, i);
	      tree tmp = fold_relational_const (EQ_EXPR, type, elem0, elem1);
	      if (tmp == NULL_TREE)
		return NULL_TREE;
	      if (integer_zerop (tmp))
		return constant_boolean_node (code == NE_EXPR, type);
	    }
	  return constant_boolean_node (code == EQ_EXPR, type);
	}
      tree_vector_builder elts;
      if (!elts.new_binary_operation (type, op0, op1, false))
	return NULL_TREE;
      unsigned int count = elts.encoded_nelts ();
      for (unsigned i = 0; i < count; i++)
	{
	  tree elem_type = TREE_TYPE (type);
	  tree elem0 = VECTOR_CST_ELT (op0, i);
	  tree elem1 = VECTOR_CST_ELT (op1, i);

	  tree tem = fold_relational_const (code, elem_type,
					    elem0, elem1);

	  if (tem == NULL_TREE)
	    return NULL_TREE;

	  elts.quick_push (build_int_cst (elem_type,
					  integer_zerop (tem) ? 0 : -1));
	}

      return elts.build ();
    }

  /* From here on we only handle LT, LE, GT, GE, EQ and NE.

     To compute GT, swap the arguments and do LT.
     To compute GE, do LT and invert the result.
     To compute LE, swap the arguments, do LT and invert the result.
     To compute NE, do EQ and invert the result.

     Therefore, the code below must handle only EQ and LT.  */

  if (code == LE_EXPR || code == GT_EXPR)
    {
      std::swap (op0, op1);
      code = swap_tree_comparison (code);
    }

  /* Note that it is safe to invert for real values here because we
     have already handled the one case that it matters.  */

  invert = 0;
  if (code == NE_EXPR || code == GE_EXPR)
    {
      invert = 1;
      code = invert_tree_comparison (code, false);
    }

  /* Compute a result for LT or EQ if args permit;
     Otherwise return T.  */
  if (TREE_CODE (op0) == INTEGER_CST && TREE_CODE (op1) == INTEGER_CST)
    {
      if (code == EQ_EXPR)
	result = tree_int_cst_equal (op0, op1);
      else
	result = tree_int_cst_lt (op0, op1);
    }
  else
    return NULL_TREE;

  if (invert)
    result ^= 1;
  return constant_boolean_node (result, type);
}

/* If necessary, return a CLEANUP_POINT_EXPR for EXPR with the
   indicated TYPE.  If no CLEANUP_POINT_EXPR is necessary, return EXPR
   itself.  */

tree
fold_build_cleanup_point_expr (tree type, tree expr)
{
  /* If the expression does not have side effects then we don't have to wrap
     it with a cleanup point expression.  */
  if (!TREE_SIDE_EFFECTS (expr))
    return expr;

  /* If the expression is a return, check to see if the expression inside the
     return has no side effects or the right hand side of the modify expression
     inside the return. If either don't have side effects set we don't need to
     wrap the expression in a cleanup point expression.  Note we don't check the
     left hand side of the modify because it should always be a return decl.  */
  if (TREE_CODE (expr) == RETURN_EXPR)
    {
      tree op = TREE_OPERAND (expr, 0);
      if (!op || !TREE_SIDE_EFFECTS (op))
        return expr;
      op = TREE_OPERAND (op, 1);
      if (!TREE_SIDE_EFFECTS (op))
        return expr;
    }

  return build1_loc (EXPR_LOCATION (expr), CLEANUP_POINT_EXPR, type, expr);
}

/* Given a pointer value OP0 and a type TYPE, return a simplified version
   of an indirection through OP0, or NULL_TREE if no simplification is
   possible.  */

tree
fold_indirect_ref_1 (location_t loc, tree type, tree op0)
{
  tree sub = op0;
  tree subtype;
  poly_uint64 const_op01;

  STRIP_NOPS (sub);
  subtype = TREE_TYPE (sub);
  if (!POINTER_TYPE_P (subtype)
      || TYPE_REF_CAN_ALIAS_ALL (TREE_TYPE (op0)))
    return NULL_TREE;

  if (TREE_CODE (sub) == ADDR_EXPR)
    {
      tree op = TREE_OPERAND (sub, 0);
      tree optype = TREE_TYPE (op);

      /* *&CONST_DECL -> to the value of the const decl.  */
      if (TREE_CODE (op) == CONST_DECL)
	return DECL_INITIAL (op);
      /* *&p => p;  make sure to handle *&"str"[cst] here.  */
      if (type == optype)
	{
	  tree fop = fold_read_from_constant_string (op);
	  if (fop)
	    return fop;
	  else
	    return op;
	}
      /* *(foo *)&fooarray => fooarray[0] */
      else if (TREE_CODE (optype) == ARRAY_TYPE
	       && type == TREE_TYPE (optype)
	       && (!in_gimple_form
		   || TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST))
	{
	  tree type_domain = TYPE_DOMAIN (optype);
	  tree min_val = size_zero_node;
	  if (type_domain && TYPE_MIN_VALUE (type_domain))
	    min_val = TYPE_MIN_VALUE (type_domain);
	  if (in_gimple_form
	      && TREE_CODE (min_val) != INTEGER_CST)
	    return NULL_TREE;
	  return build4_loc (loc, ARRAY_REF, type, op, min_val,
			     NULL_TREE, NULL_TREE);
	}
      /* *(foo *)&complexfoo => __real__ complexfoo */
      else if (TREE_CODE (optype) == COMPLEX_TYPE
	       && type == TREE_TYPE (optype))
	return fold_build1_loc (loc, REALPART_EXPR, type, op);
      /* *(foo *)&vectorfoo => BIT_FIELD_REF<vectorfoo,...> */
      else if (VECTOR_TYPE_P (optype)
	       && type == TREE_TYPE (optype))
	{
	  tree part_width = TYPE_SIZE (type);
	  tree index = bitsize_int (0);
	  return fold_build3_loc (loc, BIT_FIELD_REF, type, op, part_width,
				  index);
	}
    }

  if (TREE_CODE (sub) == POINTER_PLUS_EXPR
      && poly_int_tree_p (TREE_OPERAND (sub, 1), &const_op01))
    {
      tree op00 = TREE_OPERAND (sub, 0);
      tree op01 = TREE_OPERAND (sub, 1);

      STRIP_NOPS (op00);
      if (TREE_CODE (op00) == ADDR_EXPR)
	{
	  tree op00type;
	  op00 = TREE_OPERAND (op00, 0);
	  op00type = TREE_TYPE (op00);

	  /* ((foo*)&vectorfoo)[1] => BIT_FIELD_REF<vectorfoo,...> */
	  if (VECTOR_TYPE_P (op00type)
	      && type == TREE_TYPE (op00type)
	      /* POINTER_PLUS_EXPR second operand is sizetype, unsigned,
		 but we want to treat offsets with MSB set as negative.
		 For the code below negative offsets are invalid and
		 TYPE_SIZE of the element is something unsigned, so
		 check whether op01 fits into poly_int64, which implies
		 it is from 0 to INTTYPE_MAXIMUM (HOST_WIDE_INT), and
		 then just use poly_uint64 because we want to treat the
		 value as unsigned.  */
	      && tree_fits_poly_int64_p (op01))
	    {
	      tree part_width = TYPE_SIZE (type);
	      poly_uint64 max_offset
		= (tree_to_uhwi (part_width) / BITS_PER_UNIT
		   * TYPE_VECTOR_SUBPARTS (op00type));
	      if (known_lt (const_op01, max_offset))
		{
		  tree index = bitsize_int (const_op01 * BITS_PER_UNIT);
		  return fold_build3_loc (loc,
					  BIT_FIELD_REF, type, op00,
					  part_width, index);
		}
	    }
	  /* ((foo*)&complexfoo)[1] => __imag__ complexfoo */
	  else if (TREE_CODE (op00type) == COMPLEX_TYPE
		   && type == TREE_TYPE (op00type))
	    {
	      if (known_eq (wi::to_poly_offset (TYPE_SIZE_UNIT (type)),
			    const_op01))
		return fold_build1_loc (loc, IMAGPART_EXPR, type, op00);
	    }
	  /* ((foo *)&fooarray)[1] => fooarray[1] */
	  else if (TREE_CODE (op00type) == ARRAY_TYPE
		   && type == TREE_TYPE (op00type))
	    {
	      tree type_domain = TYPE_DOMAIN (op00type);
	      tree min_val = size_zero_node;
	      if (type_domain && TYPE_MIN_VALUE (type_domain))
		min_val = TYPE_MIN_VALUE (type_domain);
	      poly_uint64 type_size, index;
	      if (poly_int_tree_p (min_val)
		  && poly_int_tree_p (TYPE_SIZE_UNIT (type), &type_size)
		  && multiple_p (const_op01, type_size, &index))
		{
		  poly_offset_int off = index + wi::to_poly_offset (min_val);
		  op01 = wide_int_to_tree (sizetype, off);
		  return build4_loc (loc, ARRAY_REF, type, op00, op01,
				     NULL_TREE, NULL_TREE);
		}
	    }
	}
    }

  /* *(foo *)fooarrptr => (*fooarrptr)[0] */
  if (TREE_CODE (TREE_TYPE (subtype)) == ARRAY_TYPE
      && type == TREE_TYPE (TREE_TYPE (subtype))
      && (!in_gimple_form
	  || TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST))
    {
      tree type_domain;
      tree min_val = size_zero_node;
      sub = build_fold_indirect_ref_loc (loc, sub);
      type_domain = TYPE_DOMAIN (TREE_TYPE (sub));
      if (type_domain && TYPE_MIN_VALUE (type_domain))
	min_val = TYPE_MIN_VALUE (type_domain);
      if (in_gimple_form
	  && TREE_CODE (min_val) != INTEGER_CST)
	return NULL_TREE;
      return build4_loc (loc, ARRAY_REF, type, sub, min_val, NULL_TREE,
			 NULL_TREE);
    }

  return NULL_TREE;
}

/* Builds an expression for an indirection through T, simplifying some
   cases.  */

tree
build_fold_indirect_ref_loc (location_t loc, tree t)
{
  tree type = TREE_TYPE (TREE_TYPE (t));
  tree sub = fold_indirect_ref_1 (loc, type, t);

  if (sub)
    return sub;

  return build1_loc (loc, INDIRECT_REF, type, t);
}

/* Given an INDIRECT_REF T, return either T or a simplified version.  */

tree
fold_indirect_ref_loc (location_t loc, tree t)
{
  tree sub = fold_indirect_ref_1 (loc, TREE_TYPE (t), TREE_OPERAND (t, 0));

  if (sub)
    return sub;
  else
    return t;
}

/* Strip non-trapping, non-side-effecting tree nodes from an expression
   whose result is ignored.  The type of the returned tree need not be
   the same as the original expression.  */

tree
fold_ignored_result (tree t)
{
  if (!TREE_SIDE_EFFECTS (t))
    return integer_zero_node;

  for (;;)
    switch (TREE_CODE_CLASS (TREE_CODE (t)))
      {
      case tcc_unary:
	t = TREE_OPERAND (t, 0);
	break;

      case tcc_binary:
      case tcc_comparison:
	if (!TREE_SIDE_EFFECTS (TREE_OPERAND (t, 1)))
	  t = TREE_OPERAND (t, 0);
	else if (!TREE_SIDE_EFFECTS (TREE_OPERAND (t, 0)))
	  t = TREE_OPERAND (t, 1);
	else
	  return t;
	break;

      case tcc_expression:
	switch (TREE_CODE (t))
	  {
	  case COMPOUND_EXPR:
	    if (TREE_SIDE_EFFECTS (TREE_OPERAND (t, 1)))
	      return t;
	    t = TREE_OPERAND (t, 0);
	    break;

	  case COND_EXPR:
	    if (TREE_SIDE_EFFECTS (TREE_OPERAND (t, 1))
		|| TREE_SIDE_EFFECTS (TREE_OPERAND (t, 2)))
	      return t;
	    t = TREE_OPERAND (t, 0);
	    break;

	  default:
	    return t;
	  }
	break;

      default:
	return t;
      }
}

/* Return the value of VALUE, rounded up to a multiple of DIVISOR. */

tree
round_up_loc (location_t loc, tree value, unsigned int divisor)
{
  tree div = NULL_TREE;

  if (divisor == 1)
    return value;

  /* See if VALUE is already a multiple of DIVISOR.  If so, we don't
     have to do anything.  Only do this when we are not given a const,
     because in that case, this check is more expensive than just
     doing it.  */
  if (TREE_CODE (value) != INTEGER_CST)
    {
      div = build_int_cst (TREE_TYPE (value), divisor);

      if (multiple_of_p (TREE_TYPE (value), value, div))
	return value;
    }

  /* If divisor is a power of two, simplify this to bit manipulation.  */
  if (pow2_or_zerop (divisor))
    {
      if (TREE_CODE (value) == INTEGER_CST)
	{
	  wide_int val = wi::to_wide (value);
	  bool overflow_p;

	  if ((val & (divisor - 1)) == 0)
	    return value;

	  overflow_p = TREE_OVERFLOW (value);
	  val += divisor - 1;
	  val &= (int) -divisor;
	  if (val == 0)
	    overflow_p = true;

	  return force_fit_type (TREE_TYPE (value), val, -1, overflow_p);
	}
      else
	{
	  tree t;

	  t = build_int_cst (TREE_TYPE (value), divisor - 1);
	  value = size_binop_loc (loc, PLUS_EXPR, value, t);
	  t = build_int_cst (TREE_TYPE (value), - (int) divisor);
	  value = size_binop_loc (loc, BIT_AND_EXPR, value, t);
	}
    }
  else
    {
      if (!div)
	div = build_int_cst (TREE_TYPE (value), divisor);
      value = size_binop_loc (loc, CEIL_DIV_EXPR, value, div);
      value = size_binop_loc (loc, MULT_EXPR, value, div);
    }

  return value;
}

/* Likewise, but round down.  */

tree
round_down_loc (location_t loc, tree value, int divisor)
{
  tree div = NULL_TREE;

  gcc_assert (divisor > 0);
  if (divisor == 1)
    return value;

  /* See if VALUE is already a multiple of DIVISOR.  If so, we don't
     have to do anything.  Only do this when we are not given a const,
     because in that case, this check is more expensive than just
     doing it.  */
  if (TREE_CODE (value) != INTEGER_CST)
    {
      div = build_int_cst (TREE_TYPE (value), divisor);

      if (multiple_of_p (TREE_TYPE (value), value, div))
	return value;
    }

  /* If divisor is a power of two, simplify this to bit manipulation.  */
  if (pow2_or_zerop (divisor))
    {
      tree t;

      t = build_int_cst (TREE_TYPE (value), -divisor);
      value = size_binop_loc (loc, BIT_AND_EXPR, value, t);
    }
  else
    {
      if (!div)
	div = build_int_cst (TREE_TYPE (value), divisor);
      value = size_binop_loc (loc, FLOOR_DIV_EXPR, value, div);
      value = size_binop_loc (loc, MULT_EXPR, value, div);
    }

  return value;
}

/* Returns the pointer to the base of the object addressed by EXP and
   extracts the information about the offset of the access, storing it
   to PBITPOS and POFFSET.  */

static tree
split_address_to_core_and_offset (tree exp,
				  poly_int64 *pbitpos, tree *poffset)
{
  tree core;
  machine_mode mode;
  int unsignedp, reversep, volatilep;
  poly_int64 bitsize;
  location_t loc = EXPR_LOCATION (exp);

  if (TREE_CODE (exp) == SSA_NAME)
    if (gassign *def = dyn_cast <gassign *> (SSA_NAME_DEF_STMT (exp)))
      if (gimple_assign_rhs_code (def) == ADDR_EXPR)
	exp = gimple_assign_rhs1 (def);

  if (TREE_CODE (exp) == ADDR_EXPR)
    {
      core = get_inner_reference (TREE_OPERAND (exp, 0), &bitsize, pbitpos,
				  poffset, &mode, &unsignedp, &reversep,
				  &volatilep);
      core = build_fold_addr_expr_loc (loc, core);
    }
  else if (TREE_CODE (exp) == POINTER_PLUS_EXPR)
    {
      core = TREE_OPERAND (exp, 0);
      STRIP_NOPS (core);
      *pbitpos = 0;
      *poffset = TREE_OPERAND (exp, 1);
      if (poly_int_tree_p (*poffset))
	{
	  poly_offset_int tem
	    = wi::sext (wi::to_poly_offset (*poffset),
			TYPE_PRECISION (TREE_TYPE (*poffset)));
	  tem <<= LOG2_BITS_PER_UNIT;
	  if (tem.to_shwi (pbitpos))
	    *poffset = NULL_TREE;
	}
    }
  else
    {
      core = exp;
      *pbitpos = 0;
      *poffset = NULL_TREE;
    }

  return core;
}

/* Returns true if addresses of E1 and E2 differ by a constant, false
   otherwise.  If they do, E1 - E2 is stored in *DIFF.  */

bool
ptr_difference_const (tree e1, tree e2, poly_int64 *diff)
{
  tree core1, core2;
  poly_int64 bitpos1, bitpos2;
  tree toffset1, toffset2, tdiff, type;

  core1 = split_address_to_core_and_offset (e1, &bitpos1, &toffset1);
  core2 = split_address_to_core_and_offset (e2, &bitpos2, &toffset2);

  poly_int64 bytepos1, bytepos2;
  if (!multiple_p (bitpos1, BITS_PER_UNIT, &bytepos1)
      || !multiple_p (bitpos2, BITS_PER_UNIT, &bytepos2)
      || !operand_equal_p (core1, core2, 0))
    return false;

  if (toffset1 && toffset2)
    {
      type = TREE_TYPE (toffset1);
      if (type != TREE_TYPE (toffset2))
	toffset2 = fold_convert (type, toffset2);

      tdiff = fold_build2 (MINUS_EXPR, type, toffset1, toffset2);
      if (!cst_and_fits_in_hwi (tdiff))
	return false;

      *diff = int_cst_value (tdiff);
    }
  else if (toffset1 || toffset2)
    {
      /* If only one of the offsets is non-constant, the difference cannot
	 be a constant.  */
      return false;
    }
  else
    *diff = 0;

  *diff += bytepos1 - bytepos2;
  return true;
}

/* Return OFF converted to a pointer offset type suitable as offset for
   POINTER_PLUS_EXPR.  Use location LOC for this conversion.  */
tree
convert_to_ptrofftype_loc (location_t loc, tree off)
{
  if (ptrofftype_p (TREE_TYPE (off)))
    return off;
  return fold_convert_loc (loc, sizetype, off);
}

/* Build and fold a POINTER_PLUS_EXPR at LOC offsetting PTR by OFF.  */
tree
fold_build_pointer_plus_loc (location_t loc, tree ptr, tree off)
{
  return fold_build2_loc (loc, POINTER_PLUS_EXPR, TREE_TYPE (ptr),
			  ptr, convert_to_ptrofftype_loc (loc, off));
}

/* Build and fold a POINTER_PLUS_EXPR at LOC offsetting PTR by OFF.  */
tree
fold_build_pointer_plus_hwi_loc (location_t loc, tree ptr, HOST_WIDE_INT off)
{
  return fold_build2_loc (loc, POINTER_PLUS_EXPR, TREE_TYPE (ptr),
			  ptr, size_int (off));
}

/* Return a pointer to a NUL-terminated string containing the sequence
   of bytes corresponding to the representation of the object referred to
   by SRC (or a subsequence of such bytes within it if SRC is a reference
   to an initialized constant array plus some constant offset).
   Set *STRSIZE the number of bytes in the constant sequence including
   the terminating NUL byte.  *STRSIZE is equal to sizeof(A) - OFFSET
   where A is the array that stores the constant sequence that SRC points
   to and OFFSET is the byte offset of SRC from the beginning of A.  SRC
   need not point to a string or even an array of characters but may point
   to an object of any type.  */

const char *
getbyterep (tree src, unsigned HOST_WIDE_INT *strsize)
{
  /* The offset into the array A storing the string, and A's byte size.  */
  tree offset_node;
  tree mem_size;

  if (strsize)
    *strsize = 0;

  if (strsize)
    src = byte_representation (src, &offset_node, &mem_size, NULL);
  else
    src = string_constant (src, &offset_node, &mem_size, NULL);
  if (!src)
    return NULL;

  unsigned HOST_WIDE_INT offset = 0;
  if (offset_node != NULL_TREE)
    {
      if (!tree_fits_uhwi_p (offset_node))
	return NULL;
      else
	offset = tree_to_uhwi (offset_node);
    }

  if (!tree_fits_uhwi_p (mem_size))
    return NULL;

  /* ARRAY_SIZE is the byte size of the array the constant sequence
     is stored in and equal to sizeof A.  INIT_BYTES is the number
     of bytes in the constant sequence used to initialize the array,
     including any embedded NULs as well as the terminating NUL (for
     strings), but not including any trailing zeros/NULs past
     the terminating one appended implicitly to a string literal to
     zero out the remainder of the array it's stored in.  For example,
     given:
       const char a[7] = "abc\0d";
       n = strlen (a + 1);
     ARRAY_SIZE is 7, INIT_BYTES is 6, and OFFSET is 1.  For a valid
     (i.e., nul-terminated) string with no embedded nuls, INIT_BYTES
     is equal to strlen (A) + 1.  */
  const unsigned HOST_WIDE_INT array_size = tree_to_uhwi (mem_size);
  unsigned HOST_WIDE_INT init_bytes = TREE_STRING_LENGTH (src);
  const char *string = TREE_STRING_POINTER (src);

  /* Ideally this would turn into a gcc_checking_assert over time.  */
  if (init_bytes > array_size)
    init_bytes = array_size;

  if (init_bytes == 0 || offset >= array_size)
    return NULL;

  if (strsize)
    {
      /* Compute and store the number of characters from the beginning
	 of the substring at OFFSET to the end, including the terminating
	 nul.  Offsets past the initial length refer to null strings.  */
      if (offset < init_bytes)
	*strsize = init_bytes - offset;
      else
	*strsize = 1;
    }
  else
    {
      tree eltype = TREE_TYPE (TREE_TYPE (src));
      /* Support only properly NUL-terminated single byte strings.  */
      if (tree_to_uhwi (TYPE_SIZE_UNIT (eltype)) != 1)
	return NULL;
      if (string[init_bytes - 1] != '\0')
	return NULL;
    }

  return offset < init_bytes ? string + offset : "";
}

/* Return a pointer to a NUL-terminated string corresponding to
   the expression STR referencing a constant string, possibly
   involving a constant offset.  Return null if STR either doesn't
   reference a constant string or if it involves a nonconstant
   offset.  */

const char *
c_getstr (tree str)
{
  return getbyterep (str, NULL);
}

/* Given a tree T, compute which bits in T may be nonzero.  */

wide_int
tree_nonzero_bits (const_tree t)
{
  switch (TREE_CODE (t))
    {
    case INTEGER_CST:
      return wi::to_wide (t);
    case SSA_NAME:
      return get_nonzero_bits (t);
    case NON_LVALUE_EXPR:
    case SAVE_EXPR:
      return tree_nonzero_bits (TREE_OPERAND (t, 0));
    case BIT_AND_EXPR:
      return wi::bit_and (tree_nonzero_bits (TREE_OPERAND (t, 0)),
			  tree_nonzero_bits (TREE_OPERAND (t, 1)));
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
      return wi::bit_or (tree_nonzero_bits (TREE_OPERAND (t, 0)),
			 tree_nonzero_bits (TREE_OPERAND (t, 1)));
    case COND_EXPR:
      return wi::bit_or (tree_nonzero_bits (TREE_OPERAND (t, 1)),
			 tree_nonzero_bits (TREE_OPERAND (t, 2)));
    CASE_CONVERT:
      return wide_int::from (tree_nonzero_bits (TREE_OPERAND (t, 0)),
			     TYPE_PRECISION (TREE_TYPE (t)),
			     TYPE_SIGN (TREE_TYPE (TREE_OPERAND (t, 0))));
    case PLUS_EXPR:
      if (INTEGRAL_TYPE_P (TREE_TYPE (t)))
	{
	  wide_int nzbits1 = tree_nonzero_bits (TREE_OPERAND (t, 0));
	  wide_int nzbits2 = tree_nonzero_bits (TREE_OPERAND (t, 1));
	  if (wi::bit_and (nzbits1, nzbits2) == 0)
	    return wi::bit_or (nzbits1, nzbits2);
	}
      break;
    case LSHIFT_EXPR:
      if (TREE_CODE (TREE_OPERAND (t, 1)) == INTEGER_CST)
	{
	  tree type = TREE_TYPE (t);
	  wide_int nzbits = tree_nonzero_bits (TREE_OPERAND (t, 0));
	  wide_int arg1 = wi::to_wide (TREE_OPERAND (t, 1),
				       TYPE_PRECISION (type));
	  return wi::neg_p (arg1)
		 ? wi::rshift (nzbits, -arg1, TYPE_SIGN (type))
		 : wi::lshift (nzbits, arg1);
	}
      break;
    case RSHIFT_EXPR:
      if (TREE_CODE (TREE_OPERAND (t, 1)) == INTEGER_CST)
        {
	  tree type = TREE_TYPE (t);
	  wide_int nzbits = tree_nonzero_bits (TREE_OPERAND (t, 0));
	  wide_int arg1 = wi::to_wide (TREE_OPERAND (t, 1),
				       TYPE_PRECISION (type));
	  return wi::neg_p (arg1)
		 ? wi::lshift (nzbits, -arg1)
		 : wi::rshift (nzbits, arg1, TYPE_SIGN (type));
        }
      break;
    default:
      break;
    }

  return wi::shwi (-1, TYPE_PRECISION (TREE_TYPE (t)));
}

/* Helper function for address compare simplifications in match.pd.
   OP0 and OP1 are ADDR_EXPR operands being compared by CODE.
   TYPE is the type of comparison operands.
   BASE0, BASE1, OFF0 and OFF1 are set by the function.
   GENERIC is true if GENERIC folding and false for GIMPLE folding.
   Returns 0 if OP0 is known to be unequal to OP1 regardless of OFF{0,1},
   1 if bases are known to be equal and OP0 cmp OP1 depends on OFF0 cmp OFF1,
   and 2 if unknown.  */

int
address_compare (tree_code code, tree type, tree op0, tree op1,
		 tree &base0, tree &base1, poly_int64 &off0, poly_int64 &off1,
		 bool generic)
{
  if (TREE_CODE (op0) == SSA_NAME)
    op0 = gimple_assign_rhs1 (SSA_NAME_DEF_STMT (op0));
  if (TREE_CODE (op1) == SSA_NAME)
    op1 = gimple_assign_rhs1 (SSA_NAME_DEF_STMT (op1));
  gcc_checking_assert (TREE_CODE (op0) == ADDR_EXPR);
  gcc_checking_assert (TREE_CODE (op1) == ADDR_EXPR);
  base0 = get_addr_base_and_unit_offset (TREE_OPERAND (op0, 0), &off0);
  base1 = get_addr_base_and_unit_offset (TREE_OPERAND (op1, 0), &off1);
  if (base0 && TREE_CODE (base0) == MEM_REF)
    {
      off0 += mem_ref_offset (base0).force_shwi ();
      base0 = TREE_OPERAND (base0, 0);
    }
  if (base1 && TREE_CODE (base1) == MEM_REF)
    {
      off1 += mem_ref_offset (base1).force_shwi ();
      base1 = TREE_OPERAND (base1, 0);
    }
  if (base0 == NULL_TREE || base1 == NULL_TREE)
    return 2;

  int equal = 2;
  /* Punt in GENERIC on variables with value expressions;
     the value expressions might point to fields/elements
     of other vars etc.  */
  if (generic
      && ((VAR_P (base0) && DECL_HAS_VALUE_EXPR_P (base0))
	  || (VAR_P (base1) && DECL_HAS_VALUE_EXPR_P (base1))))
    return 2;
  else if (decl_in_symtab_p (base0) && decl_in_symtab_p (base1))
    {
      symtab_node *node0 = symtab_node::get_create (base0);
      symtab_node *node1 = symtab_node::get_create (base1);
      equal = node0->equal_address_to (node1);
    }
  else if ((DECL_P (base0)
	    || TREE_CODE (base0) == SSA_NAME
	    || TREE_CODE (base0) == STRING_CST)
	   && (DECL_P (base1)
	       || TREE_CODE (base1) == SSA_NAME
	       || TREE_CODE (base1) == STRING_CST))
    equal = (base0 == base1);
  /* Assume different STRING_CSTs with the same content will be
     merged.  */
  if (equal == 0
      && TREE_CODE (base0) == STRING_CST
      && TREE_CODE (base1) == STRING_CST
      && TREE_STRING_LENGTH (base0) == TREE_STRING_LENGTH (base1)
      && memcmp (TREE_STRING_POINTER (base0), TREE_STRING_POINTER (base1),
		 TREE_STRING_LENGTH (base0)) == 0)
    equal = 1;
  if (equal == 1)
    {
      if (code == EQ_EXPR
	  || code == NE_EXPR
	  /* If the offsets are equal we can ignore overflow.  */
	  || known_eq (off0, off1)
	  || TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (op0))
	  /* Or if we compare using pointers to decls or strings.  */
	  || (POINTER_TYPE_P (type)
	      && (DECL_P (base0) || TREE_CODE (base0) == STRING_CST)))
	return 1;
      return 2;
    }
  if (equal != 0)
    return equal;
  if (code != EQ_EXPR && code != NE_EXPR)
    return 2;

  /* At this point we know (or assume) the two pointers point at
     different objects.  */
  HOST_WIDE_INT ioff0 = -1, ioff1 = -1;
  off0.is_constant (&ioff0);
  off1.is_constant (&ioff1);
  /* Punt on non-zero offsets from functions.  */
  if ((TREE_CODE (base0) == FUNCTION_DECL && ioff0)
      || (TREE_CODE (base1) == FUNCTION_DECL && ioff1))
    return 2;
  /* Or if the bases are neither decls nor string literals.  */
  if (!DECL_P (base0) && TREE_CODE (base0) != STRING_CST)
    return 2;
  if (!DECL_P (base1) && TREE_CODE (base1) != STRING_CST)
    return 2;
  /* For initializers, assume addresses of different functions are
     different.  */
  if (folding_initializer
      && TREE_CODE (base0) == FUNCTION_DECL
      && TREE_CODE (base1) == FUNCTION_DECL)
    return 0;

  /* Compute whether one address points to the start of one
     object and another one to the end of another one.  */
  poly_int64 size0 = 0, size1 = 0;
  if (TREE_CODE (base0) == STRING_CST)
    {
      if (ioff0 < 0 || ioff0 > TREE_STRING_LENGTH (base0))
	equal = 2;
      else
	size0 = TREE_STRING_LENGTH (base0);
    }
  else if (TREE_CODE (base0) == FUNCTION_DECL)
    size0 = 1;
  else
    {
      tree sz0 = DECL_SIZE_UNIT (base0);
      if (!tree_fits_poly_int64_p (sz0))
	equal = 2;
      else
	size0 = tree_to_poly_int64 (sz0);
    }
  if (TREE_CODE (base1) == STRING_CST)
    {
      if (ioff1 < 0 || ioff1 > TREE_STRING_LENGTH (base1))
	equal = 2;
      else
	size1 = TREE_STRING_LENGTH (base1);
    }
  else if (TREE_CODE (base1) == FUNCTION_DECL)
    size1 = 1;
  else
    {
      tree sz1 = DECL_SIZE_UNIT (base1);
      if (!tree_fits_poly_int64_p (sz1))
	equal = 2;
      else
	size1 = tree_to_poly_int64 (sz1);
    }
  if (equal == 0)
    {
      /* If one offset is pointing (or could be) to the beginning of one
	 object and the other is pointing to one past the last byte of the
	 other object, punt.  */
      if (maybe_eq (off0, 0) && maybe_eq (off1, size1))
	equal = 2;
      else if (maybe_eq (off1, 0) && maybe_eq (off0, size0))
	equal = 2;
      /* If both offsets are the same, there are some cases we know that are
	 ok.  Either if we know they aren't zero, or if we know both sizes
	 are no zero.  */
      if (equal == 2
	  && known_eq (off0, off1)
	  && (known_ne (off0, 0)
	      || (known_ne (size0, 0) && known_ne (size1, 0))))
	equal = 0;
    }

  /* At this point, equal is 2 if either one or both pointers are out of
     bounds of their object, or one points to start of its object and the
     other points to end of its object.  This is unspecified behavior
     e.g. in C++.  Otherwise equal is 0.  */
  if (folding_cxx_constexpr && equal)
    return equal;

  /* When both pointers point to string literals, even when equal is 0,
     due to tail merging of string literals the pointers might be the same.  */
  if (TREE_CODE (base0) == STRING_CST && TREE_CODE (base1) == STRING_CST)
    {
      if (ioff0 < 0
	  || ioff1 < 0
	  || ioff0 > TREE_STRING_LENGTH (base0)
	  || ioff1 > TREE_STRING_LENGTH (base1))
	return 2;

      /* If the bytes in the string literals starting at the pointers
	 differ, the pointers need to be different.  */
      if (memcmp (TREE_STRING_POINTER (base0) + ioff0,
		  TREE_STRING_POINTER (base1) + ioff1,
		  MIN (TREE_STRING_LENGTH (base0) - ioff0,
		       TREE_STRING_LENGTH (base1) - ioff1)) == 0)
	{
	  HOST_WIDE_INT ioffmin = MIN (ioff0, ioff1);
	  if (memcmp (TREE_STRING_POINTER (base0) + ioff0 - ioffmin,
		      TREE_STRING_POINTER (base1) + ioff1 - ioffmin,
		      ioffmin) == 0)
	    /* If even the bytes in the string literal before the
	       pointers are the same, the string literals could be
	       tail merged.  */
	    return 2;
	}
      return 0;
    }

  if (folding_cxx_constexpr)
    return 0;

  /* If this is a pointer comparison, ignore for now even
     valid equalities where one pointer is the offset zero
     of one object and the other to one past end of another one.  */
  if (!INTEGRAL_TYPE_P (type))
    return 0;

  /* Assume that string literals can't be adjacent to variables
     (automatic or global).  */
  if (TREE_CODE (base0) == STRING_CST || TREE_CODE (base1) == STRING_CST)
    return 0;

  /* Assume that automatic variables can't be adjacent to global
     variables.  */
  if (is_global_var (base0) != is_global_var (base1))
    return 0;

  return equal;
}

/* Return the single non-zero element of a CONSTRUCTOR or NULL_TREE.  */
tree
ctor_single_nonzero_element (const_tree t)
{
  unsigned HOST_WIDE_INT idx;
  constructor_elt *ce;
  tree elt = NULL_TREE;

  if (TREE_CODE (t) != CONSTRUCTOR)
    return NULL_TREE;
  for (idx = 0; vec_safe_iterate (CONSTRUCTOR_ELTS (t), idx, &ce); idx++)
    if (!integer_zerop (ce->value) && !real_zerop (ce->value))
      {
	if (elt)
	  return NULL_TREE;
	elt = ce->value;
      }
  return elt;
}

#if CHECKING_P

namespace selftest {

/* Helper functions for writing tests of folding trees.  */

/* Verify that the binary op (LHS CODE RHS) folds to CONSTANT.  */

static void
assert_binop_folds_to_const (tree lhs, enum tree_code code, tree rhs,
			     tree constant)
{
  ASSERT_EQ (constant, fold_build2 (code, TREE_TYPE (lhs), lhs, rhs));
}

/* Verify that the binary op (LHS CODE RHS) folds to an NON_LVALUE_EXPR
   wrapping WRAPPED_EXPR.  */

static void
assert_binop_folds_to_nonlvalue (tree lhs, enum tree_code code, tree rhs,
				 tree wrapped_expr)
{
  tree result = fold_build2 (code, TREE_TYPE (lhs), lhs, rhs);
  ASSERT_NE (wrapped_expr, result);
  ASSERT_EQ (NON_LVALUE_EXPR, TREE_CODE (result));
  ASSERT_EQ (wrapped_expr, TREE_OPERAND (result, 0));
}

/* Verify that various arithmetic binary operations are folded
   correctly.  */

static void
test_arithmetic_folding ()
{
  tree type = integer_type_node;
  tree x = create_tmp_var_raw (type, "x");
  tree zero = build_zero_cst (type);
  tree one = build_int_cst (type, 1);

  /* Addition.  */
  /* 1 <-- (0 + 1) */
  assert_binop_folds_to_const (zero, PLUS_EXPR, one,
			       one);
  assert_binop_folds_to_const (one, PLUS_EXPR, zero,
			       one);

  /* (nonlvalue)x <-- (x + 0) */
  assert_binop_folds_to_nonlvalue (x, PLUS_EXPR, zero,
				   x);

  /* Subtraction.  */
  /* 0 <-- (x - x) */
  assert_binop_folds_to_const (x, MINUS_EXPR, x,
			       zero);
  assert_binop_folds_to_nonlvalue (x, MINUS_EXPR, zero,
				   x);

  /* Multiplication.  */
  /* 0 <-- (x * 0) */
  assert_binop_folds_to_const (x, MULT_EXPR, zero,
			       zero);

  /* (nonlvalue)x <-- (x * 1) */
  assert_binop_folds_to_nonlvalue (x, MULT_EXPR, one,
				   x);
}

namespace test_fold_vec_perm_cst {

/* Build a VECTOR_CST corresponding to VMODE, and has
   encoding given by NPATTERNS, NELTS_PER_PATTERN and STEP.
   Fill it with randomized elements, using rand() % THRESHOLD.  */

static tree
build_vec_cst_rand (machine_mode vmode, unsigned npatterns,
		    unsigned nelts_per_pattern,
		    int step = 0, bool natural_stepped = false,
		    int threshold = 100)
{
  tree inner_type = lang_hooks.types.type_for_mode (GET_MODE_INNER (vmode), 1);
  tree vectype = build_vector_type_for_mode (inner_type, vmode);
  tree_vector_builder builder (vectype, npatterns, nelts_per_pattern);

  // Fill a0 for each pattern
  for (unsigned i = 0; i < npatterns; i++)
    builder.quick_push (build_int_cst (inner_type, rand () % threshold));

  if (nelts_per_pattern == 1)
    return builder.build ();

  // Fill a1 for each pattern
  for (unsigned i = 0; i < npatterns; i++)
    {
      tree a1;
      if (natural_stepped)
	{
	  tree a0 = builder[i];
	  wide_int a0_val = wi::to_wide (a0);
	  wide_int a1_val = a0_val + step;
	  a1 = wide_int_to_tree (inner_type, a1_val);
	}
      else
	a1 = build_int_cst (inner_type, rand () % threshold);
      builder.quick_push (a1);
    }
  if (nelts_per_pattern == 2)
    return builder.build ();

  for (unsigned i = npatterns * 2; i < npatterns * nelts_per_pattern; i++)
    {
      tree prev_elem = builder[i - npatterns];
      wide_int prev_elem_val = wi::to_wide (prev_elem);
      wide_int val = prev_elem_val + step;
      builder.quick_push (wide_int_to_tree (inner_type, val));
    }

  return builder.build ();
}

/* Validate result of VEC_PERM_EXPR folding for the unit-tests below,
   when result is VLA.  */

static void
validate_res (unsigned npatterns, unsigned nelts_per_pattern,
	      tree res, tree *expected_res)
{
  /* Actual npatterns and encoded_elts in res may be less than expected due
     to canonicalization.  */
  ASSERT_TRUE (res != NULL_TREE);
  ASSERT_TRUE (VECTOR_CST_NPATTERNS (res) <= npatterns);
  ASSERT_TRUE (vector_cst_encoded_nelts (res) <= npatterns * nelts_per_pattern);

  for (unsigned i = 0; i < npatterns * nelts_per_pattern; i++)
    ASSERT_TRUE (operand_equal_p (VECTOR_CST_ELT (res, i), expected_res[i], 0));
}

/* Validate result of VEC_PERM_EXPR folding for the unit-tests below,
   when the result is VLS.  */

static void
validate_res_vls (tree res, tree *expected_res, unsigned expected_nelts)
{
  ASSERT_TRUE (known_eq (VECTOR_CST_NELTS (res), expected_nelts));
  for (unsigned i = 0; i < expected_nelts; i++)
    ASSERT_TRUE (operand_equal_p (VECTOR_CST_ELT (res, i), expected_res[i], 0));
}

/* Helper routine to push multiple elements into BUILDER.  */
template<unsigned N>
static void builder_push_elems (vec_perm_builder& builder,
				poly_uint64 (&elems)[N])
{
  for (unsigned i = 0; i < N; i++)
    builder.quick_push (elems[i]);
}

#define ARG0(index) vector_cst_elt (arg0, index)
#define ARG1(index) vector_cst_elt (arg1, index)

/* Test cases where result is VNx4SI and input vectors are V4SI.  */

static void
test_vnx4si_v4si (machine_mode vnx4si_mode, machine_mode v4si_mode)
{
  for (int i = 0; i < 10; i++)
    {
      /* Case 1:
	 sel = { 0, 4, 1, 5, ... }
	 res = { arg[0], arg1[0], arg0[1], arg1[1], ...} // (4, 1)  */
      {
	tree arg0 = build_vec_cst_rand (v4si_mode, 4, 1, 0);
	tree arg1 = build_vec_cst_rand (v4si_mode, 4, 1, 0);

	tree inner_type
	  = lang_hooks.types.type_for_mode (GET_MODE_INNER (vnx4si_mode), 1);
	tree res_type = build_vector_type_for_mode (inner_type, vnx4si_mode);

	poly_uint64 res_len = TYPE_VECTOR_SUBPARTS (res_type);
	vec_perm_builder builder (res_len, 4, 1);
	poly_uint64 mask_elems[] = { 0, 4, 1, 5 };
	builder_push_elems (builder, mask_elems);

	vec_perm_indices sel (builder, 2, res_len);
	tree res = fold_vec_perm_cst (res_type, arg0, arg1, sel);

	tree expected_res[] = { ARG0(0), ARG1(0), ARG0(1), ARG1(1) };
	validate_res (4, 1, res, expected_res);
      }

      /* Case 2: Same as case 1, but contains an out of bounds access which
	 should wrap around.
	 sel = {0, 8, 4, 12, ...} (4, 1)
	 res = { arg0[0], arg0[0], arg1[0], arg1[0], ... } (4, 1).  */
      {
	tree arg0 = build_vec_cst_rand (v4si_mode, 4, 1, 0);
	tree arg1 = build_vec_cst_rand (v4si_mode, 4, 1, 0);

	tree inner_type
	  = lang_hooks.types.type_for_mode (GET_MODE_INNER (vnx4si_mode), 1);
	tree res_type = build_vector_type_for_mode (inner_type, vnx4si_mode);

	poly_uint64 res_len = TYPE_VECTOR_SUBPARTS (res_type);
	vec_perm_builder builder (res_len, 4, 1);
	poly_uint64 mask_elems[] = { 0, 8, 4, 12 };
	builder_push_elems (builder, mask_elems);

	vec_perm_indices sel (builder, 2, res_len);
	tree res = fold_vec_perm_cst (res_type, arg0, arg1, sel);

	tree expected_res[] = { ARG0(0), ARG0(0), ARG1(0), ARG1(0) };
	validate_res (4, 1, res, expected_res);
      }
    }
}

/* Test cases where result is V4SI and input vectors are VNx4SI.  */

static void
test_v4si_vnx4si (machine_mode v4si_mode, machine_mode vnx4si_mode)
{
  for (int i = 0; i < 10; i++)
    {
      /* Case 1:
	 sel = { 0, 1, 2, 3}
	 res = { arg0[0], arg0[1], arg0[2], arg0[3] }.  */
      {
	tree arg0 = build_vec_cst_rand (vnx4si_mode, 4, 1);
	tree arg1 = build_vec_cst_rand (vnx4si_mode, 4, 1);

	tree inner_type
	  = lang_hooks.types.type_for_mode (GET_MODE_INNER (v4si_mode), 1);
	tree res_type = build_vector_type_for_mode (inner_type, v4si_mode);

	poly_uint64 res_len = TYPE_VECTOR_SUBPARTS (res_type);
	vec_perm_builder builder (res_len, 4, 1);
	poly_uint64 mask_elems[] = {0, 1, 2, 3};
	builder_push_elems (builder, mask_elems);

	vec_perm_indices sel (builder, 2, res_len);
	tree res = fold_vec_perm_cst (res_type, arg0, arg1, sel);

	tree expected_res[] = { ARG0(0), ARG0(1), ARG0(2), ARG0(3) };
	validate_res_vls (res, expected_res, 4);
      }

      /* Case 2: Same as Case 1, but crossing input vector.
	 sel = {0, 2, 4, 6}
	 In this case,the index 4 is ambiguous since len = 4 + 4x.
	 Since we cannot determine, which vector to choose from during
	 compile time, should return NULL_TREE.  */
      {
	tree arg0 = build_vec_cst_rand (vnx4si_mode, 4, 1);
	tree arg1 = build_vec_cst_rand (vnx4si_mode, 4, 1);

	tree inner_type
	  = lang_hooks.types.type_for_mode (GET_MODE_INNER (v4si_mode), 1);
	tree res_type = build_vector_type_for_mode (inner_type, v4si_mode);

	poly_uint64 res_len = TYPE_VECTOR_SUBPARTS (res_type);
	vec_perm_builder builder (res_len, 4, 1);
	poly_uint64 mask_elems[] = {0, 2, 4, 6};
	builder_push_elems (builder, mask_elems);

	vec_perm_indices sel (builder, 2, res_len);
	const char *reason;
	tree res = fold_vec_perm_cst (res_type, arg0, arg1, sel, &reason);

	ASSERT_TRUE (res == NULL_TREE);
	ASSERT_TRUE (!strcmp (reason, "cannot divide selector element by arg len"));
      }
    }
}

/* Test all input vectors.  */

static void
test_all_nunits (machine_mode vmode)
{
  /* Test with 10 different inputs.  */
  for (int i = 0; i < 10; i++)
    {
      tree arg0 = build_vec_cst_rand (vmode, 1, 3, 1);
      tree arg1 = build_vec_cst_rand (vmode, 1, 3, 1);
      poly_uint64 len = TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg0));

      /* Case 1: mask = {0, ...} // (1, 1)
	 res = { arg0[0], ... } // (1, 1)  */
      {
	vec_perm_builder builder (len, 1, 1);
	builder.quick_push (0);
	vec_perm_indices sel (builder, 2, len);
	tree res = fold_vec_perm_cst (TREE_TYPE (arg0), arg0, arg1, sel);
	tree expected_res[] = { ARG0(0) };
	validate_res (1, 1, res, expected_res);
      }

      /* Case 2: mask = {len, ...} // (1, 1)
	 res = { arg1[0], ... } // (1, 1)  */
      {
	vec_perm_builder builder (len, 1, 1);
	builder.quick_push (len);
	vec_perm_indices sel (builder, 2, len);
	tree res = fold_vec_perm_cst (TREE_TYPE (arg0), arg0, arg1, sel);

	tree expected_res[] = { ARG1(0) };
	validate_res (1, 1, res, expected_res);
      }
    }
}

/* Test all vectors which contain at-least 2 elements.  */

static void
test_nunits_min_2 (machine_mode vmode)
{
  for (int i = 0; i < 10; i++)
    {
      /* Case 1: mask = { 0, len, ... }  // (2, 1)
	 res = { arg0[0], arg1[0], ... } // (2, 1)  */
      {
	tree arg0 = build_vec_cst_rand (vmode, 1, 3, 1);
	tree arg1 = build_vec_cst_rand (vmode, 1, 3, 1);
	poly_uint64 len = TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg0));

	vec_perm_builder builder (len, 2, 1);
	poly_uint64 mask_elems[] = { 0, len };
	builder_push_elems (builder, mask_elems);

	vec_perm_indices sel (builder, 2, len);
	tree res = fold_vec_perm_cst (TREE_TYPE (arg0), arg0, arg1, sel);

	tree expected_res[] = { ARG0(0), ARG1(0) };
	validate_res (2, 1, res, expected_res);
      }

      /* Case 2: mask = { 0, len, 1, len+1, ... } // (2, 2)
	 res = { arg0[0], arg1[0], arg0[1], arg1[1], ... } // (2, 2)  */
      {
	tree arg0 = build_vec_cst_rand (vmode, 1, 3, 1);
	tree arg1 = build_vec_cst_rand (vmode, 1, 3, 1);
	poly_uint64 len = TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg0));

	vec_perm_builder builder (len, 2, 2);
	poly_uint64 mask_elems[] = { 0, len, 1, len + 1 };
	builder_push_elems (builder, mask_elems);

	vec_perm_indices sel (builder, 2, len);
	tree res = fold_vec_perm_cst (TREE_TYPE (arg0), arg0, arg1, sel);

	tree expected_res[] = { ARG0(0), ARG1(0), ARG0(1), ARG1(1) };
	validate_res (2, 2, res, expected_res);
      }

      /* Case 4: mask = {0, 0, 1, ...} // (1, 3)
	 Test that the stepped sequence of the pattern selects from
	 same input pattern. Since input vectors have npatterns = 2,
	 and step (a2 - a1) = 1, step is not a multiple of npatterns
	 in input vector. So return NULL_TREE.  */
      {
	tree arg0 = build_vec_cst_rand (vmode, 2, 3, 1, true);
	tree arg1 = build_vec_cst_rand (vmode, 2, 3, 1);
	poly_uint64 len = TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg0));

	vec_perm_builder builder (len, 1, 3);
	poly_uint64 mask_elems[] = { 0, 0, 1 };
	builder_push_elems (builder, mask_elems);

	vec_perm_indices sel (builder, 2, len);
	const char *reason;
	tree res = fold_vec_perm_cst (TREE_TYPE (arg0), arg0, arg1, sel,
				      &reason);
	ASSERT_TRUE (res == NULL_TREE);
	ASSERT_TRUE (!strcmp (reason, "step is not multiple of npatterns"));
      }

      /* Case 5: mask = {len, 0, 1, ...} // (1, 3)
	 Test that stepped sequence of the pattern selects from arg0.
	 res = { arg1[0], arg0[0], arg0[1], ... } // (1, 3)  */
      {
	tree arg0 = build_vec_cst_rand (vmode, 1, 3, 1, true);
	tree arg1 = build_vec_cst_rand (vmode, 1, 3, 1);
	poly_uint64 len = TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg0));

	vec_perm_builder builder (len, 1, 3);
	poly_uint64 mask_elems[] = { len, 0, 1 };
	builder_push_elems (builder, mask_elems);

	vec_perm_indices sel (builder, 2, len);
	tree res = fold_vec_perm_cst (TREE_TYPE (arg0), arg0, arg1, sel);

	tree expected_res[] = { ARG1(0), ARG0(0), ARG0(1) };
	validate_res (1, 3, res, expected_res);
      }

      /* Case 6: PR111648 - a1 chooses base element from input vector arg.
	 In this case ensure that arg has a natural stepped sequence
	 to preserve arg's encoding.

	 As a concrete example, consider:
	 arg0: { -16, -9, -10, ... } // (1, 3)
	 arg1: { -12, -5, -6, ... }  // (1, 3)
	 sel = { 0, len, len + 1, ... } // (1, 3)

	 This will create res with following encoding:
	 res = { arg0[0], arg1[0], arg1[1], ... } // (1, 3)
	     = { -16, -12, -5, ... }

	 The step in above encoding would be: (-5) - (-12) = 7
	 And hence res[3] would be computed as -5 + 7 = 2.
	 instead of arg1[2], ie, -6.
	 Ensure that valid_mask_for_fold_vec_perm_cst returns false
	 for this case.  */
      {
	tree arg0 = build_vec_cst_rand (vmode, 1, 3, 1);
	tree arg1 = build_vec_cst_rand (vmode, 1, 3, 1);
	poly_uint64 len = TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg0));

	vec_perm_builder builder (len, 1, 3);
	poly_uint64 mask_elems[] = { 0, len, len+1 };
	builder_push_elems (builder, mask_elems);

	vec_perm_indices sel (builder, 2, len);
	const char *reason;
	/* FIXME: It may happen that build_vec_cst_rand may build a natural
	   stepped pattern, even if we didn't explicitly tell it to. So folding
	   may not always fail, but if it does, ensure that's because arg1 does
	   not have a natural stepped sequence (and not due to other reason)  */
	tree res = fold_vec_perm_cst (TREE_TYPE (arg0), arg0, arg1, sel, &reason);
	if (res == NULL_TREE)
	  ASSERT_TRUE (!strcmp (reason, "not a natural stepped sequence"));
      }

      /* Case 7: Same as Case 6, except that arg1 contains natural stepped
	 sequence and thus folding should be valid for this case.  */
      {
	tree arg0 = build_vec_cst_rand (vmode, 1, 3, 1);
	tree arg1 = build_vec_cst_rand (vmode, 1, 3, 1, true);
	poly_uint64 len = TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg0));

	vec_perm_builder builder (len, 1, 3);
	poly_uint64 mask_elems[] = { 0, len, len+1 };
	builder_push_elems (builder, mask_elems);

	vec_perm_indices sel (builder, 2, len);
	tree res = fold_vec_perm_cst (TREE_TYPE (arg0), arg0, arg1, sel);

	tree expected_res[] = { ARG0(0), ARG1(0), ARG1(1) };
	validate_res (1, 3, res, expected_res);
      }

      /* Case 8: Same as aarch64/sve/slp_3.c:
	 arg0, arg1 are dup vectors.
	 sel = { 0, len, 1, len+1, 2, len+2, ... } // (2, 3)
	 So res = { arg0[0], arg1[0], ... } // (2, 1)

	 In this case, since the input vectors are dup, only the first two
	 elements per pattern in sel are considered significant.  */
      {
	tree arg0 = build_vec_cst_rand (vmode, 1, 1);
	tree arg1 = build_vec_cst_rand (vmode, 1, 1);
	poly_uint64 len = TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg0));

	vec_perm_builder builder (len, 2, 3);
	poly_uint64 mask_elems[] = { 0, len, 1, len + 1, 2, len + 2 };
	builder_push_elems (builder, mask_elems);

	vec_perm_indices sel (builder, 2, len);
	tree res = fold_vec_perm_cst (TREE_TYPE (arg0), arg0, arg1, sel);

	tree expected_res[] = { ARG0(0), ARG1(0) };
	validate_res (2, 1, res, expected_res);
      }
    }
}

/* Test all vectors which contain at-least 4 elements.  */

static void
test_nunits_min_4 (machine_mode vmode)
{
  for (int i = 0; i < 10; i++)
    {
      /* Case 1: mask = { 0, len, 1, len+1, ... } // (4, 1)
	 res: { arg0[0], arg1[0], arg0[1], arg1[1], ... } // (4, 1)  */
      {
	tree arg0 = build_vec_cst_rand (vmode, 1, 3, 1);
	tree arg1 = build_vec_cst_rand (vmode, 1, 3, 1);
	poly_uint64 len = TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg0));

	vec_perm_builder builder (len, 4, 1);
	poly_uint64 mask_elems[] = { 0, len, 1, len + 1 };
	builder_push_elems (builder, mask_elems);

	vec_perm_indices sel (builder, 2, len);
	tree res = fold_vec_perm_cst (TREE_TYPE (arg0), arg0, arg1, sel);

	tree expected_res[] = { ARG0(0), ARG1(0), ARG0(1), ARG1(1) };
	validate_res (4, 1, res, expected_res);
      }

      /* Case 2: sel = {0, 1, 2, ...}  // (1, 3)
	 res: { arg0[0], arg0[1], arg0[2], ... } // (1, 3) */
      {
	tree arg0 = build_vec_cst_rand (vmode, 1, 3, 2);
	tree arg1 = build_vec_cst_rand (vmode, 1, 3, 2);
	poly_uint64 arg0_len = TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg0));

	vec_perm_builder builder (arg0_len, 1, 3);
	poly_uint64 mask_elems[] = {0, 1, 2};
	builder_push_elems (builder, mask_elems);

	vec_perm_indices sel (builder, 2, arg0_len);
	tree res = fold_vec_perm_cst (TREE_TYPE (arg0), arg0, arg1, sel);
	tree expected_res[] = { ARG0(0), ARG0(1), ARG0(2) };
	validate_res (1, 3, res, expected_res);
      }

      /* Case 3: sel = {len, len+1, len+2, ...} // (1, 3)
	 res: { arg1[0], arg1[1], arg1[2], ... } // (1, 3) */
      {
	tree arg0 = build_vec_cst_rand (vmode, 1, 3, 2);
	tree arg1 = build_vec_cst_rand (vmode, 1, 3, 2);
	poly_uint64 len = TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg0));

	vec_perm_builder builder (len, 1, 3);
	poly_uint64 mask_elems[] = {len, len + 1, len + 2};
	builder_push_elems (builder, mask_elems);

	vec_perm_indices sel (builder, 2, len);
	tree res = fold_vec_perm_cst (TREE_TYPE (arg0), arg0, arg1, sel);
	tree expected_res[] = { ARG1(0), ARG1(1), ARG1(2) };
	validate_res (1, 3, res, expected_res);
      }

      /* Case 4:
	sel = { len, 0, 2, ... } // (1, 3)
	This should return NULL because we cross the input vectors.
	Because,
	Let's assume len = C + Cx
	a1 = 0
	S = 2
	esel = arg0_len / sel_npatterns = C + Cx
	ae = 0 + (esel - 2) * S
	   = 0 + (C + Cx - 2) * 2
	   = 2(C-2) + 2Cx

	For C >= 4:
	Let q1 = a1 / arg0_len = 0 / (C + Cx) = 0
	Let qe = ae / arg0_len = (2(C-2) + 2Cx) / (C + Cx) = 1
	Since q1 != qe, we cross input vectors.
	So return NULL_TREE.  */
      {
	tree arg0 = build_vec_cst_rand (vmode, 1, 3, 2);
	tree arg1 = build_vec_cst_rand (vmode, 1, 3, 2);
	poly_uint64 arg0_len = TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg0));

	vec_perm_builder builder (arg0_len, 1, 3);
	poly_uint64 mask_elems[] = { arg0_len, 0, 2 };
	builder_push_elems (builder, mask_elems);

	vec_perm_indices sel (builder, 2, arg0_len);
	const char *reason;
	tree res = fold_vec_perm_cst (TREE_TYPE (arg0), arg0, arg1, sel, &reason);
	ASSERT_TRUE (res == NULL_TREE);
	ASSERT_TRUE (!strcmp (reason, "crossed input vectors"));
      }

      /* Case 5: npatterns(arg0) = 4 > npatterns(sel) = 2
	 mask = { 0, len, 1, len + 1, ...} // (2, 2)
	 res = { arg0[0], arg1[0], arg0[1], arg1[1], ... } // (2, 2)

	 Note that fold_vec_perm_cst will set
	 res_npatterns = max(4, max(4, 2)) = 4
	 However after canonicalizing, we will end up with shape (2, 2).  */
      {
	tree arg0 = build_vec_cst_rand (vmode, 4, 1);
	tree arg1 = build_vec_cst_rand (vmode, 4, 1);
	poly_uint64 len = TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg0));

	vec_perm_builder builder (len, 2, 2);
	poly_uint64 mask_elems[] = { 0, len, 1, len + 1 };
	builder_push_elems (builder, mask_elems);

	vec_perm_indices sel (builder, 2, len);
	tree res = fold_vec_perm_cst (TREE_TYPE (arg0), arg0, arg1, sel);
	tree expected_res[] = { ARG0(0), ARG1(0), ARG0(1), ARG1(1) };
	validate_res (2, 2, res, expected_res);
      }

      /* Case 6: Test combination in sel, where one pattern is dup and other
	 is stepped sequence.
	 sel = { 0, 0, 0, 1, 0, 2, ... } // (2, 3)
	 res = { arg0[0], arg0[0], arg0[0],
		 arg0[1], arg0[0], arg0[2], ... } // (2, 3)  */
      {
	tree arg0 = build_vec_cst_rand (vmode, 1, 3, 1);
	tree arg1 = build_vec_cst_rand (vmode, 1, 3, 1);
	poly_uint64 len = TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg0));

	vec_perm_builder builder (len, 2, 3);
	poly_uint64 mask_elems[] = { 0, 0, 0, 1, 0, 2 };
	builder_push_elems (builder, mask_elems);

	vec_perm_indices sel (builder, 2, len);
	tree res = fold_vec_perm_cst (TREE_TYPE (arg0), arg0, arg1, sel);

	tree expected_res[] = { ARG0(0), ARG0(0), ARG0(0),
				ARG0(1), ARG0(0), ARG0(2) };
	validate_res (2, 3, res, expected_res);
      }

      /* Case 7: PR111048: Check that we set arg_npatterns correctly,
	 when arg0, arg1 and sel have different number of patterns.
	 arg0 is of shape (1, 1)
	 arg1 is of shape (4, 1)
	 sel is of shape (2, 3) = {1, len, 2, len+1, 3, len+2, ...}

	 In this case the pattern: {len, len+1, len+2, ...} chooses arg1.
	 However,
	 step = (len+2) - (len+1) = 1
	 arg_npatterns = VECTOR_CST_NPATTERNS (arg1) = 4
	 Since step is not a multiple of arg_npatterns,
	 valid_mask_for_fold_vec_perm_cst should return false,
	 and thus fold_vec_perm_cst should return NULL_TREE.  */
      {
	tree arg0 = build_vec_cst_rand (vmode, 1, 1);
	tree arg1 = build_vec_cst_rand (vmode, 4, 1);
	poly_uint64 len = TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg0));

	vec_perm_builder builder (len, 2, 3);
	poly_uint64 mask_elems[] = { 0, len, 1, len + 1, 2, len + 2 };
	builder_push_elems (builder, mask_elems);

	vec_perm_indices sel (builder, 2, len);
	const char *reason;
	tree res = fold_vec_perm_cst (TREE_TYPE (arg0), arg0, arg1, sel, &reason);

	ASSERT_TRUE (res == NULL_TREE);
	ASSERT_TRUE (!strcmp (reason, "step is not multiple of npatterns"));
      }

      /* Case 8: PR111754: When input vector is not a stepped sequence,
	 check that the result is not a stepped sequence either, even
	 if sel has a stepped sequence.  */
      {
	tree arg0 = build_vec_cst_rand (vmode, 1, 2);
	poly_uint64 len = TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg0));

	vec_perm_builder builder (len, 1, 3);
	poly_uint64 mask_elems[] = { 0, 1, 2 };
	builder_push_elems (builder, mask_elems);

	vec_perm_indices sel (builder, 1, len);
	tree res = fold_vec_perm_cst (TREE_TYPE (arg0), arg0, arg0, sel);

	tree expected_res[] = { ARG0(0), ARG0(1) };
	validate_res (sel.encoding ().npatterns (), 2, res, expected_res);
      }

      /* Case 9: If sel doesn't contain a stepped sequence,
	 check that the result has same encoding as sel, irrespective
	 of shape of input vectors.  */
      {
	tree arg0 = build_vec_cst_rand (vmode, 1, 3, 1);
	tree arg1 = build_vec_cst_rand (vmode, 1, 3, 1);
	poly_uint64 len = TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg0));

	vec_perm_builder builder (len, 1, 2);
	poly_uint64 mask_elems[] = { 0, len };
	builder_push_elems (builder, mask_elems);

	vec_perm_indices sel (builder, 2, len);
	tree res = fold_vec_perm_cst (TREE_TYPE (arg0), arg0, arg1, sel);

	tree expected_res[] = { ARG0(0), ARG1(0) };
	validate_res (sel.encoding ().npatterns (),
		      sel.encoding ().nelts_per_pattern (), res, expected_res);
      }
    }
}

/* Test all vectors which contain at-least 8 elements.  */

static void
test_nunits_min_8 (machine_mode vmode)
{
  for (int i = 0; i < 10; i++)
    {
      /* Case 1: sel_npatterns (4) > input npatterns (2)
	 sel: { 0, 0, 1, len, 2, 0, 3, len, 4, 0, 5, len, ...} // (4, 3)
	 res: { arg0[0], arg0[0], arg0[0], arg1[0],
		arg0[2], arg0[0], arg0[3], arg1[0],
		arg0[4], arg0[0], arg0[5], arg1[0], ... } // (4, 3)  */
      {
	tree arg0 = build_vec_cst_rand (vmode, 2, 3, 2);
	tree arg1 = build_vec_cst_rand (vmode, 2, 3, 2);
	poly_uint64 len = TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg0));

	vec_perm_builder builder(len, 4, 3);
	poly_uint64 mask_elems[] = { 0, 0, 1, len, 2, 0, 3, len,
				     4, 0, 5, len };
	builder_push_elems (builder, mask_elems);

	vec_perm_indices sel (builder, 2, len);
	tree res = fold_vec_perm_cst (TREE_TYPE (arg0), arg0, arg1, sel);

	tree expected_res[] = { ARG0(0), ARG0(0), ARG0(1), ARG1(0),
				ARG0(2), ARG0(0), ARG0(3), ARG1(0),
				ARG0(4), ARG0(0), ARG0(5), ARG1(0) };
	validate_res (4, 3, res, expected_res);
      }
    }
}

/* Test vectors for which nunits[0] <= 4.  */

static void
test_nunits_max_4 (machine_mode vmode)
{
  /* Case 1: mask = {0, 4, ...} // (1, 2)
     This should return NULL_TREE because the index 4 may choose
     from either arg0 or arg1 depending on vector length.  */
  {
    tree arg0 = build_vec_cst_rand (vmode, 1, 3, 1);
    tree arg1 = build_vec_cst_rand (vmode, 1, 3, 1);
    poly_uint64 len = TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg0));

    vec_perm_builder builder (len, 1, 2);
    poly_uint64 mask_elems[] = {0, 4};
    builder_push_elems (builder, mask_elems);

    vec_perm_indices sel (builder, 2, len);
    const char *reason;
    tree res = fold_vec_perm_cst (TREE_TYPE (arg0), arg0, arg1, sel, &reason);
    ASSERT_TRUE (res == NULL_TREE);
    ASSERT_TRUE (reason != NULL);
    ASSERT_TRUE (!strcmp (reason, "cannot divide selector element by arg len"));
  }
}

#undef ARG0
#undef ARG1

/* Return true if SIZE is of the form C + Cx and C is power of 2.  */

static bool
is_simple_vla_size (poly_uint64 size)
{
  if (size.is_constant ()
      || !pow2p_hwi (size.coeffs[0]))
    return false;
  for (unsigned i = 1; i < ARRAY_SIZE (size.coeffs); ++i)
    if (size.coeffs[i] != (i <= 1 ? size.coeffs[0] : 0))
      return false;
  return true;
}

/* Execute fold_vec_perm_cst unit tests.  */

static void
test ()
{
  machine_mode vnx4si_mode = E_VOIDmode;
  machine_mode v4si_mode = E_VOIDmode;

  machine_mode vmode;
  FOR_EACH_MODE_IN_CLASS (vmode, MODE_VECTOR_INT)
    {
      /* Obtain modes corresponding to VNx4SI and V4SI,
	 to call mixed mode tests below.
	 FIXME: Is there a better way to do this ?  */
      if (GET_MODE_INNER (vmode) == SImode)
	{
	  poly_uint64 nunits = GET_MODE_NUNITS (vmode);
	  if (is_simple_vla_size (nunits)
	      && nunits.coeffs[0] == 4)
	    vnx4si_mode = vmode;
	  else if (known_eq (nunits, poly_uint64 (4)))
	    v4si_mode = vmode;
	}

      if (!is_simple_vla_size (GET_MODE_NUNITS (vmode))
	  || !targetm.vector_mode_supported_p (vmode))
	continue;

      poly_uint64 nunits = GET_MODE_NUNITS (vmode);
      test_all_nunits (vmode);
      if (nunits.coeffs[0] >= 2)
	test_nunits_min_2 (vmode);
      if (nunits.coeffs[0] >= 4)
	test_nunits_min_4 (vmode);
      if (nunits.coeffs[0] >= 8)
	test_nunits_min_8 (vmode);

      if (nunits.coeffs[0] <= 4)
	test_nunits_max_4 (vmode);
    }

  if (vnx4si_mode != E_VOIDmode && v4si_mode != E_VOIDmode
      && targetm.vector_mode_supported_p (vnx4si_mode)
      && targetm.vector_mode_supported_p (v4si_mode))
    {
      test_vnx4si_v4si (vnx4si_mode, v4si_mode);
      test_v4si_vnx4si (v4si_mode, vnx4si_mode);
    }
}
} // end of test_fold_vec_perm_cst namespace

/* Verify that various binary operations on vectors are folded
   correctly.  */

static void
test_vector_folding ()
{
  tree inner_type = integer_type_node;
  tree type = build_vector_type (inner_type, 4);
  tree zero = build_zero_cst (type);
  tree one = build_one_cst (type);
  tree index = build_index_vector (type, 0, 1);

  /* Verify equality tests that return a scalar boolean result.  */
  tree res_type = boolean_type_node;
  ASSERT_FALSE (integer_nonzerop (fold_build2 (EQ_EXPR, res_type, zero, one)));
  ASSERT_TRUE (integer_nonzerop (fold_build2 (EQ_EXPR, res_type, zero, zero)));
  ASSERT_TRUE (integer_nonzerop (fold_build2 (NE_EXPR, res_type, zero, one)));
  ASSERT_FALSE (integer_nonzerop (fold_build2 (NE_EXPR, res_type, one, one)));
  ASSERT_TRUE (integer_nonzerop (fold_build2 (NE_EXPR, res_type, index, one)));
  ASSERT_FALSE (integer_nonzerop (fold_build2 (EQ_EXPR, res_type,
					       index, one)));
  ASSERT_FALSE (integer_nonzerop (fold_build2 (NE_EXPR, res_type,
					      index, index)));
  ASSERT_TRUE (integer_nonzerop (fold_build2 (EQ_EXPR, res_type,
					      index, index)));
}

/* Verify folding of VEC_DUPLICATE_EXPRs.  */

static void
test_vec_duplicate_folding ()
{
  scalar_int_mode int_mode = SCALAR_INT_TYPE_MODE (ssizetype);
  machine_mode vec_mode = targetm.vectorize.preferred_simd_mode (int_mode);
  /* This will be 1 if VEC_MODE isn't a vector mode.  */
  poly_uint64 nunits = GET_MODE_NUNITS (vec_mode);

  tree type = build_vector_type (ssizetype, nunits);
  tree dup5_expr = fold_unary (VEC_DUPLICATE_EXPR, type, ssize_int (5));
  tree dup5_cst = build_vector_from_val (type, ssize_int (5));
  ASSERT_TRUE (operand_equal_p (dup5_expr, dup5_cst, 0));
}

/* Run all of the selftests within this file.  */

void
fold_const_cc_tests ()
{
  test_arithmetic_folding ();
  test_vector_folding ();
  test_vec_duplicate_folding ();
  test_fold_vec_perm_cst::test ();
}

} // namespace selftest

#endif /* CHECKING_P */
