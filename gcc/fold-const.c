/* Fold a constant sub-tree into a single node for C-compiler
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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "flags.h"
#include "tree.h"
#include "realmpfr.h"
#include "rtl.h"
#include "expr.h"
#include "tm_p.h"
#include "target.h"
#include "diagnostic-core.h"
#include "intl.h"
#include "ggc.h"
#include "hash-table.h"
#include "langhooks.h"
#include "md5.h"
#include "gimple.h"
#include "tree-ssa.h"

/* Nonzero if we are folding constants inside an initializer; zero
   otherwise.  */
int folding_initializer = 0;

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

static bool negate_mathfn_p (enum built_in_function);
static bool negate_expr_p (tree);
static tree negate_expr (tree);
static tree split_tree (tree, enum tree_code, tree *, tree *, tree *, int);
static tree associate_trees (location_t, tree, tree, enum tree_code, tree);
static tree const_binop (enum tree_code, tree, tree);
static enum comparison_code comparison_to_compcode (enum tree_code);
static enum tree_code compcode_to_comparison (enum comparison_code);
static int operand_equal_for_comparison_p (tree, tree, tree);
static int twoval_comparison_p (tree, tree *, tree *, int *);
static tree eval_subst (location_t, tree, tree, tree, tree, tree);
static tree pedantic_omit_one_operand_loc (location_t, tree, tree, tree);
static tree distribute_bit_expr (location_t, enum tree_code, tree, tree, tree);
static tree make_bit_field_ref (location_t, tree, tree,
				HOST_WIDE_INT, HOST_WIDE_INT, int);
static tree optimize_bit_field_compare (location_t, enum tree_code,
					tree, tree, tree);
static tree decode_field_reference (location_t, tree, HOST_WIDE_INT *,
				    HOST_WIDE_INT *,
				    enum machine_mode *, int *, int *,
				    tree *, tree *);
static int all_ones_mask_p (const_tree, int);
static tree sign_bit_p (tree, const_tree);
static int simple_operand_p (const_tree);
static bool simple_operand_p_2 (tree);
static tree range_binop (enum tree_code, tree, tree, int, tree, int);
static tree range_predecessor (tree);
static tree range_successor (tree);
static tree fold_range_test (location_t, enum tree_code, tree, tree, tree);
static tree fold_cond_expr_with_comparison (location_t, tree, tree, tree, tree);
static tree unextend (tree, int, int, tree);
static tree optimize_minmax_comparison (location_t, enum tree_code,
					tree, tree, tree);
static tree extract_muldiv (tree, tree, enum tree_code, tree, bool *);
static tree extract_muldiv_1 (tree, tree, enum tree_code, tree, bool *);
static tree fold_binary_op_with_conditional_arg (location_t,
						 enum tree_code, tree,
						 tree, tree,
						 tree, tree, int);
static tree fold_mathfn_compare (location_t,
				 enum built_in_function, enum tree_code,
				 tree, tree, tree);
static tree fold_inf_compare (location_t, enum tree_code, tree, tree, tree);
static tree fold_div_compare (location_t, enum tree_code, tree, tree, tree);
static bool reorder_operands_p (const_tree, const_tree);
static tree fold_negate_const (tree, tree);
static tree fold_not_const (const_tree, tree);
static tree fold_relational_const (enum tree_code, tree, tree, tree);
static tree fold_convert_const (enum tree_code, tree, tree);

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

static inline tree
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

/* If ARG2 divides ARG1 with zero remainder, carries out the division
   of type CODE and returns the quotient.
   Otherwise returns NULL_TREE.  */

tree
div_if_zero_remainder (enum tree_code code, const_tree arg1, const_tree arg2)
{
  double_int quo, rem;
  int uns;

  /* The sign of the division is according to operand two, that
     does the correct thing for POINTER_PLUS_EXPR where we want
     a signed division.  */
  uns = TYPE_UNSIGNED (TREE_TYPE (arg2));

  quo = tree_to_double_int (arg1).divmod (tree_to_double_int (arg2),
					  uns, code, &rem);

  if (rem.is_zero ())
    return build_int_cst_wide (TREE_TYPE (arg1), quo.low, quo.high);

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
fold_undefer_overflow_warnings (bool issue, const_gimple stmt, int code)
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

  if (gimple_no_warning_p (stmt))
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

static void
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

static bool
negate_mathfn_p (enum built_in_function code)
{
  switch (code)
    {
    CASE_FLT_FN (BUILT_IN_ASIN):
    CASE_FLT_FN (BUILT_IN_ASINH):
    CASE_FLT_FN (BUILT_IN_ATAN):
    CASE_FLT_FN (BUILT_IN_ATANH):
    CASE_FLT_FN (BUILT_IN_CASIN):
    CASE_FLT_FN (BUILT_IN_CASINH):
    CASE_FLT_FN (BUILT_IN_CATAN):
    CASE_FLT_FN (BUILT_IN_CATANH):
    CASE_FLT_FN (BUILT_IN_CBRT):
    CASE_FLT_FN (BUILT_IN_CPROJ):
    CASE_FLT_FN (BUILT_IN_CSIN):
    CASE_FLT_FN (BUILT_IN_CSINH):
    CASE_FLT_FN (BUILT_IN_CTAN):
    CASE_FLT_FN (BUILT_IN_CTANH):
    CASE_FLT_FN (BUILT_IN_ERF):
    CASE_FLT_FN (BUILT_IN_LLROUND):
    CASE_FLT_FN (BUILT_IN_LROUND):
    CASE_FLT_FN (BUILT_IN_ROUND):
    CASE_FLT_FN (BUILT_IN_SIN):
    CASE_FLT_FN (BUILT_IN_SINH):
    CASE_FLT_FN (BUILT_IN_TAN):
    CASE_FLT_FN (BUILT_IN_TANH):
    CASE_FLT_FN (BUILT_IN_TRUNC):
      return true;

    CASE_FLT_FN (BUILT_IN_LLRINT):
    CASE_FLT_FN (BUILT_IN_LRINT):
    CASE_FLT_FN (BUILT_IN_NEARBYINT):
    CASE_FLT_FN (BUILT_IN_RINT):
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
  unsigned HOST_WIDE_INT val;
  unsigned int prec;
  tree type;

  gcc_assert (TREE_CODE (t) == INTEGER_CST);

  type = TREE_TYPE (t);
  if (TYPE_UNSIGNED (type))
    return false;

  prec = TYPE_PRECISION (type);
  if (prec > HOST_BITS_PER_WIDE_INT)
    {
      if (TREE_INT_CST_LOW (t) != 0)
	return true;
      prec -= HOST_BITS_PER_WIDE_INT;
      val = TREE_INT_CST_HIGH (t);
    }
  else
    val = TREE_INT_CST_LOW (t);
  if (prec < HOST_BITS_PER_WIDE_INT)
    val &= ((unsigned HOST_WIDE_INT) 1 << prec) - 1;
  return val != ((unsigned HOST_WIDE_INT) 1 << (prec - 1));
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
      if (TYPE_OVERFLOW_WRAPS (type))
	return true;

      /* Check that -CST will not overflow type.  */
      return may_negate_without_overflow_p (t);
    case BIT_NOT_EXPR:
      return (INTEGRAL_TYPE_P (type)
	      && TYPE_OVERFLOW_WRAPS (type));

    case FIXED_CST:
    case NEGATE_EXPR:
      return true;

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

	int count = TYPE_VECTOR_SUBPARTS (type), i;

	for (i = 0; i < count; i++)
	  if (!negate_expr_p (VECTOR_CST_ELT (t, i)))
	    return false;

	return true;
      }

    case COMPLEX_EXPR:
      return negate_expr_p (TREE_OPERAND (t, 0))
	     && negate_expr_p (TREE_OPERAND (t, 1));

    case CONJ_EXPR:
      return negate_expr_p (TREE_OPERAND (t, 0));

    case PLUS_EXPR:
      if (HONOR_SIGN_DEPENDENT_ROUNDING (TYPE_MODE (type))
	  || HONOR_SIGNED_ZEROS (TYPE_MODE (type)))
	return false;
      /* -(A + B) -> (-B) - A.  */
      if (negate_expr_p (TREE_OPERAND (t, 1))
	  && reorder_operands_p (TREE_OPERAND (t, 0),
				 TREE_OPERAND (t, 1)))
	return true;
      /* -(A + B) -> (-A) - B.  */
      return negate_expr_p (TREE_OPERAND (t, 0));

    case MINUS_EXPR:
      /* We can't turn -(A-B) into B-A when we honor signed zeros.  */
      return !HONOR_SIGN_DEPENDENT_ROUNDING (TYPE_MODE (type))
	     && !HONOR_SIGNED_ZEROS (TYPE_MODE (type))
	     && reorder_operands_p (TREE_OPERAND (t, 0),
				    TREE_OPERAND (t, 1));

    case MULT_EXPR:
      if (TYPE_UNSIGNED (TREE_TYPE (t)))
        break;

      /* Fall through.  */

    case RDIV_EXPR:
      if (! HONOR_SIGN_DEPENDENT_ROUNDING (TYPE_MODE (TREE_TYPE (t))))
	return negate_expr_p (TREE_OPERAND (t, 1))
	       || negate_expr_p (TREE_OPERAND (t, 0));
      break;

    case TRUNC_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case EXACT_DIV_EXPR:
      /* In general we can't negate A / B, because if A is INT_MIN and
	 B is 1, we may turn this into INT_MIN / -1 which is undefined
	 and actually traps on some architectures.  But if overflow is
	 undefined, we can negate, because - (INT_MIN / 1) is an
	 overflow.  */
      if (INTEGRAL_TYPE_P (TREE_TYPE (t)))
	{
	  if (!TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (t)))
	    break;
	  /* If overflow is undefined then we have to be careful because
	     we ask whether it's ok to associate the negate with the
	     division which is not ok for example for
	     -((a - b) / c) where (-(a - b)) / c may invoke undefined
	     overflow because of negating INT_MIN.  So do not use
	     negate_expr_p here but open-code the two important cases.  */
	  if (TREE_CODE (TREE_OPERAND (t, 0)) == NEGATE_EXPR
	      || (TREE_CODE (TREE_OPERAND (t, 0)) == INTEGER_CST
		  && may_negate_without_overflow_p (TREE_OPERAND (t, 0))))
	    return true;
	}
      else if (negate_expr_p (TREE_OPERAND (t, 0)))
	return true;
      return negate_expr_p (TREE_OPERAND (t, 1));

    case NOP_EXPR:
      /* Negate -((double)float) as (double)(-float).  */
      if (TREE_CODE (type) == REAL_TYPE)
	{
	  tree tem = strip_float_extensions (t);
	  if (tem != t)
	    return negate_expr_p (tem);
	}
      break;

    case CALL_EXPR:
      /* Negate -f(x) as f(-x).  */
      if (negate_mathfn_p (builtin_mathfn_code (t)))
	return negate_expr_p (CALL_EXPR_ARG (t, 0));
      break;

    case RSHIFT_EXPR:
      /* Optimize -((int)x >> 31) into (unsigned)x >> 31.  */
      if (TREE_CODE (TREE_OPERAND (t, 1)) == INTEGER_CST)
	{
	  tree op1 = TREE_OPERAND (t, 1);
	  if (TREE_INT_CST_HIGH (op1) == 0
	      && (unsigned HOST_WIDE_INT) (TYPE_PRECISION (type) - 1)
		 == TREE_INT_CST_LOW (op1))
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
fold_negate_expr (location_t loc, tree t)
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
	  || !TYPE_OVERFLOW_TRAPS (type))
	return tem;
      break;

    case REAL_CST:
      tem = fold_negate_const (t, type);
      /* Two's complement FP formats, such as c4x, may overflow.  */
      if (!TREE_OVERFLOW (tem) || !flag_trapping_math)
	return tem;
      break;

    case FIXED_CST:
      tem = fold_negate_const (t, type);
      return tem;

    case COMPLEX_CST:
      {
	tree rpart = negate_expr (TREE_REALPART (t));
	tree ipart = negate_expr (TREE_IMAGPART (t));

	if ((TREE_CODE (rpart) == REAL_CST
	     && TREE_CODE (ipart) == REAL_CST)
	    || (TREE_CODE (rpart) == INTEGER_CST
		&& TREE_CODE (ipart) == INTEGER_CST))
	  return build_complex (type, rpart, ipart);
      }
      break;

    case VECTOR_CST:
      {
	int count = TYPE_VECTOR_SUBPARTS (type), i;
	tree *elts = XALLOCAVEC (tree, count);

	for (i = 0; i < count; i++)
	  {
	    elts[i] = fold_negate_expr (loc, VECTOR_CST_ELT (t, i));
	    if (elts[i] == NULL_TREE)
	      return NULL_TREE;
	  }

	return build_vector (type, elts);
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
      return TREE_OPERAND (t, 0);

    case PLUS_EXPR:
      if (!HONOR_SIGN_DEPENDENT_ROUNDING (TYPE_MODE (type))
	  && !HONOR_SIGNED_ZEROS (TYPE_MODE (type)))
	{
	  /* -(A + B) -> (-B) - A.  */
	  if (negate_expr_p (TREE_OPERAND (t, 1))
	      && reorder_operands_p (TREE_OPERAND (t, 0),
				     TREE_OPERAND (t, 1)))
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
      if (!HONOR_SIGN_DEPENDENT_ROUNDING (TYPE_MODE (type))
	  && !HONOR_SIGNED_ZEROS (TYPE_MODE (type))
	  && reorder_operands_p (TREE_OPERAND (t, 0), TREE_OPERAND (t, 1)))
	return fold_build2_loc (loc, MINUS_EXPR, type,
			    TREE_OPERAND (t, 1), TREE_OPERAND (t, 0));
      break;

    case MULT_EXPR:
      if (TYPE_UNSIGNED (type))
        break;

      /* Fall through.  */

    case RDIV_EXPR:
      if (! HONOR_SIGN_DEPENDENT_ROUNDING (TYPE_MODE (type)))
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
    case FLOOR_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case EXACT_DIV_EXPR:
      /* In general we can't negate A / B, because if A is INT_MIN and
	 B is 1, we may turn this into INT_MIN / -1 which is undefined
	 and actually traps on some architectures.  But if overflow is
	 undefined, we can negate, because - (INT_MIN / 1) is an
	 overflow.  */
      if (!INTEGRAL_TYPE_P (type) || TYPE_OVERFLOW_UNDEFINED (type))
        {
	  const char * const warnmsg = G_("assuming signed overflow does not "
					  "occur when negating a division");
          tem = TREE_OPERAND (t, 1);
          if (negate_expr_p (tem))
	    {
	      if (INTEGRAL_TYPE_P (type)
		  && (TREE_CODE (tem) != INTEGER_CST
		      || integer_onep (tem)))
		fold_overflow_warning (warnmsg, WARN_STRICT_OVERFLOW_MISC);
	      return fold_build2_loc (loc, TREE_CODE (t), type,
				  TREE_OPERAND (t, 0), negate_expr (tem));
	    }
	  /* If overflow is undefined then we have to be careful because
	     we ask whether it's ok to associate the negate with the
	     division which is not ok for example for
	     -((a - b) / c) where (-(a - b)) / c may invoke undefined
	     overflow because of negating INT_MIN.  So do not use
	     negate_expr_p here but open-code the two important cases.  */
          tem = TREE_OPERAND (t, 0);
	  if ((INTEGRAL_TYPE_P (type)
	       && (TREE_CODE (tem) == NEGATE_EXPR
		   || (TREE_CODE (tem) == INTEGER_CST
		       && may_negate_without_overflow_p (tem))))
	      || !INTEGRAL_TYPE_P (type))
	    return fold_build2_loc (loc, TREE_CODE (t), type,
				    negate_expr (tem), TREE_OPERAND (t, 1));
        }
      break;

    case NOP_EXPR:
      /* Convert -((double)float) into (double)(-float).  */
      if (TREE_CODE (type) == REAL_TYPE)
	{
	  tem = strip_float_extensions (t);
	  if (tem != t && negate_expr_p (tem))
	    return fold_convert_loc (loc, type, negate_expr (tem));
	}
      break;

    case CALL_EXPR:
      /* Negate -f(x) as f(-x).  */
      if (negate_mathfn_p (builtin_mathfn_code (t))
	  && negate_expr_p (CALL_EXPR_ARG (t, 0)))
	{
	  tree fndecl, arg;

	  fndecl = get_callee_fndecl (t);
	  arg = negate_expr (CALL_EXPR_ARG (t, 0));
	  return build_call_expr_loc (loc, fndecl, 1, arg);
	}
      break;

    case RSHIFT_EXPR:
      /* Optimize -((int)x >> 31) into (unsigned)x >> 31.  */
      if (TREE_CODE (TREE_OPERAND (t, 1)) == INTEGER_CST)
	{
	  tree op1 = TREE_OPERAND (t, 1);
	  if (TREE_INT_CST_HIGH (op1) == 0
	      && (unsigned HOST_WIDE_INT) (TYPE_PRECISION (type) - 1)
		 == TREE_INT_CST_LOW (op1))
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

/* Like fold_negate_expr, but return a NEGATE_EXPR tree, if T can not be
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
   for which we use *MINUS_LITP instead.

   If IN is itself a literal or constant, return it as appropriate.

   Note that we do not guarantee that any of the three values will be the
   same type as IN, but they will have the same signedness and mode.  */

static tree
split_tree (tree in, enum tree_code code, tree *conp, tree *litp,
	    tree *minus_litp, int negate_p)
{
  tree var = 0;

  *conp = 0;
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
	       && ((code == PLUS_EXPR && TREE_CODE (in) == MINUS_EXPR)
		   || (code == MINUS_EXPR && TREE_CODE (in) == PLUS_EXPR))))
    {
      tree op0 = TREE_OPERAND (in, 0);
      tree op1 = TREE_OPERAND (in, 1);
      int neg1_p = TREE_CODE (in) == MINUS_EXPR;
      int neg_litp_p = 0, neg_conp_p = 0, neg_var_p = 0;

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
      if (neg_conp_p)
	*conp = negate_expr (*conp);
      if (neg_var_p)
	var = negate_expr (var);
    }
  else if (TREE_CODE (in) == BIT_NOT_EXPR
	   && code == PLUS_EXPR)
    {
      /* -X - 1 is folded to ~X, undo that here.  */
      *minus_litp = build_one_cst (TREE_TYPE (in));
      var = negate_expr (TREE_OPERAND (in, 0));
    }
  else if (TREE_CONSTANT (in))
    *conp = in;
  else
    var = in;

  if (negate_p)
    {
      if (*litp)
	*minus_litp = *litp, *litp = 0;
      else if (*minus_litp)
	*litp = *minus_litp, *minus_litp = 0;
      *conp = negate_expr (*conp);
      var = negate_expr (var);
    }

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
    return t2;
  else if (t2 == 0)
    return t1;

  /* If either input is CODE, a PLUS_EXPR, or a MINUS_EXPR, don't
     try to fold this since we will have infinite recursion.  But do
     deal with any NEGATE_EXPRs.  */
  if (TREE_CODE (t1) == code || TREE_CODE (t2) == code
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


/* Combine two integer constants ARG1 and ARG2 under operation CODE
   to produce a new constant.  Return NULL_TREE if we don't know how
   to evaluate CODE at compile-time.  */

static tree
int_const_binop_1 (enum tree_code code, const_tree arg1, const_tree arg2,
		   int overflowable)
{
  double_int op1, op2, res, tmp;
  tree t;
  tree type = TREE_TYPE (arg1);
  bool uns = TYPE_UNSIGNED (type);
  bool overflow = false;

  op1 = tree_to_double_int (arg1);
  op2 = tree_to_double_int (arg2);

  switch (code)
    {
    case BIT_IOR_EXPR:
      res = op1 | op2;
      break;

    case BIT_XOR_EXPR:
      res = op1 ^ op2;
      break;

    case BIT_AND_EXPR:
      res = op1 & op2;
      break;

    case RSHIFT_EXPR:
      res = op1.rshift (op2.to_shwi (), TYPE_PRECISION (type), !uns);
      break;

    case LSHIFT_EXPR:
      /* It's unclear from the C standard whether shifts can overflow.
	 The following code ignores overflow; perhaps a C standard
	 interpretation ruling is needed.  */
      res = op1.lshift (op2.to_shwi (), TYPE_PRECISION (type), !uns);
      break;

    case RROTATE_EXPR:
      res = op1.rrotate (op2.to_shwi (), TYPE_PRECISION (type));
      break;

    case LROTATE_EXPR:
      res = op1.lrotate (op2.to_shwi (), TYPE_PRECISION (type));
      break;

    case PLUS_EXPR:
      res = op1.add_with_sign (op2, false, &overflow);
      break;

    case MINUS_EXPR:
      res = op1.sub_with_overflow (op2, &overflow);
      break;

    case MULT_EXPR:
      res = op1.mul_with_sign (op2, false, &overflow);
      break;

    case MULT_HIGHPART_EXPR:
      if (TYPE_PRECISION (type) > HOST_BITS_PER_WIDE_INT)
	{
	  bool dummy_overflow;
	  if (TYPE_PRECISION (type) != 2 * HOST_BITS_PER_WIDE_INT)
	    return NULL_TREE;
	  op1.wide_mul_with_sign (op2, uns, &res, &dummy_overflow);
	}
      else
	{
	  bool dummy_overflow;
	  /* MULT_HIGHPART_EXPR can't ever oveflow, as the multiplication
	     is performed in twice the precision of arguments.  */
	  tmp = op1.mul_with_sign (op2, false, &dummy_overflow);
	  res = tmp.rshift (TYPE_PRECISION (type),
			    2 * TYPE_PRECISION (type), !uns);
	}
      break;

    case TRUNC_DIV_EXPR:
    case FLOOR_DIV_EXPR: case CEIL_DIV_EXPR:
    case EXACT_DIV_EXPR:
      /* This is a shortcut for a common special case.  */
      if (op2.high == 0 && (HOST_WIDE_INT) op2.low > 0
	  && !TREE_OVERFLOW (arg1)
	  && !TREE_OVERFLOW (arg2)
	  && op1.high == 0 && (HOST_WIDE_INT) op1.low >= 0)
	{
	  if (code == CEIL_DIV_EXPR)
	    op1.low += op2.low - 1;

	  res.low = op1.low / op2.low, res.high = 0;
	  break;
	}

      /* ... fall through ...  */

    case ROUND_DIV_EXPR:
      if (op2.is_zero ())
	return NULL_TREE;
      if (op2.is_one ())
	{
	  res = op1;
	  break;
	}
      if (op1 == op2 && !op1.is_zero ())
	{
	  res = double_int_one;
	  break;
	}
      res = op1.divmod_with_overflow (op2, uns, code, &tmp, &overflow);
      break;

    case TRUNC_MOD_EXPR:
    case FLOOR_MOD_EXPR: case CEIL_MOD_EXPR:
      /* This is a shortcut for a common special case.  */
      if (op2.high == 0 && (HOST_WIDE_INT) op2.low > 0
	  && !TREE_OVERFLOW (arg1)
	  && !TREE_OVERFLOW (arg2)
	  && op1.high == 0 && (HOST_WIDE_INT) op1.low >= 0)
	{
	  if (code == CEIL_MOD_EXPR)
	    op1.low += op2.low - 1;
	  res.low = op1.low % op2.low, res.high = 0;
	  break;
	}

      /* ... fall through ...  */

    case ROUND_MOD_EXPR:
      if (op2.is_zero ())
	return NULL_TREE;
      tmp = op1.divmod_with_overflow (op2, uns, code, &res, &overflow);
      break;

    case MIN_EXPR:
      res = op1.min (op2, uns);
      break;

    case MAX_EXPR:
      res = op1.max (op2, uns);
      break;

    default:
      return NULL_TREE;
    }

  t = force_fit_type_double (TREE_TYPE (arg1), res, overflowable,
			     (!uns && overflow)
			     | TREE_OVERFLOW (arg1) | TREE_OVERFLOW (arg2));

  return t;
}

tree
int_const_binop (enum tree_code code, const_tree arg1, const_tree arg2)
{
  return int_const_binop_1 (code, arg1, arg2, 1);
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

  if (TREE_CODE (arg1) == INTEGER_CST)
    return int_const_binop (code, arg1, arg2);

  if (TREE_CODE (arg1) == REAL_CST)
    {
      enum machine_mode mode;
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
	 either operand is a NaN.  */
      if (HONOR_SNANS (mode)
	  && (REAL_VALUE_ISNAN (d1) || REAL_VALUE_ISNAN (d2)))
	return NULL_TREE;

      /* Don't perform operation if it would raise a division
	 by zero exception.  */
      if (code == RDIV_EXPR
	  && REAL_VALUES_EQUAL (d2, dconst0)
	  && (flag_trapping_math || ! MODE_HAS_INFINITIES (mode)))
	return NULL_TREE;

      /* If either operand is a NaN, just return it.  Otherwise, set up
	 for floating-point trap; we return an overflow.  */
      if (REAL_VALUE_ISNAN (d1))
	return arg1;
      else if (REAL_VALUE_ISNAN (d2))
	return arg2;

      inexact = real_arithmetic (&value, code, &d1, &d2);
      real_convert (&result, mode, &value);

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
      int sat_p;
      bool overflow_p;

      /* The following codes are handled by fixed_arithmetic.  */
      switch (code)
        {
	case PLUS_EXPR:
	case MINUS_EXPR:
	case MULT_EXPR:
	case TRUNC_DIV_EXPR:
	  f2 = TREE_FIXED_CST (arg2);
	  break;

	case LSHIFT_EXPR:
	case RSHIFT_EXPR:
	  f2.data.high = TREE_INT_CST_HIGH (arg2);
	  f2.data.low = TREE_INT_CST_LOW (arg2);
	  f2.mode = SImode;
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

  if (TREE_CODE (arg1) == COMPLEX_CST)
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
	  /* Fallthru ... */
	case TRUNC_DIV_EXPR:
	case CEIL_DIV_EXPR:
	case FLOOR_DIV_EXPR:
	case ROUND_DIV_EXPR:
	  if (flag_complex_method == 0)
	  {
	    /* Keep this algorithm in sync with
	       tree-complex.c:expand_complex_div_straight().

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
               tree-complex.c:expand_complex_div_wide().

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

  if (TREE_CODE (arg1) == VECTOR_CST
      && TREE_CODE (arg2) == VECTOR_CST)
    {
      tree type = TREE_TYPE (arg1);
      int count = TYPE_VECTOR_SUBPARTS (type), i;
      tree *elts = XALLOCAVEC (tree, count);

      for (i = 0; i < count; i++)
	{
	  tree elem1 = VECTOR_CST_ELT (arg1, i);
	  tree elem2 = VECTOR_CST_ELT (arg2, i);

	  elts[i] = const_binop (code, elem1, elem2);

	  /* It is possible that const_binop cannot handle the given
	     code and return NULL_TREE */
	  if (elts[i] == NULL_TREE)
	    return NULL_TREE;
	}

      return build_vector (type, elts);
    }

  /* Shifts allow a scalar offset for a vector.  */
  if (TREE_CODE (arg1) == VECTOR_CST
      && TREE_CODE (arg2) == INTEGER_CST)
    {
      tree type = TREE_TYPE (arg1);
      int count = TYPE_VECTOR_SUBPARTS (type), i;
      tree *elts = XALLOCAVEC (tree, count);

      if (code == VEC_LSHIFT_EXPR
	  || code == VEC_RSHIFT_EXPR)
	{
	  if (!host_integerp (arg2, 1))
	    return NULL_TREE;

	  unsigned HOST_WIDE_INT shiftc = tree_low_cst (arg2, 1);
	  unsigned HOST_WIDE_INT outerc = tree_low_cst (TYPE_SIZE (type), 1);
	  unsigned HOST_WIDE_INT innerc
	    = tree_low_cst (TYPE_SIZE (TREE_TYPE (type)), 1);
	  if (shiftc >= outerc || (shiftc % innerc) != 0)
	    return NULL_TREE;
	  int offset = shiftc / innerc;
	  /* The direction of VEC_[LR]SHIFT_EXPR is endian dependent.
	     For reductions, compiler emits VEC_RSHIFT_EXPR always,
	     for !BYTES_BIG_ENDIAN picks first vector element, but
	     for BYTES_BIG_ENDIAN last element from the vector.  */
	  if ((code == VEC_RSHIFT_EXPR) ^ (!BYTES_BIG_ENDIAN))
	    offset = -offset;
	  tree zero = build_zero_cst (TREE_TYPE (type));
	  for (i = 0; i < count; i++)
	    {
	      if (i + offset < 0 || i + offset >= count)
		elts[i] = zero;
	      else
		elts[i] = VECTOR_CST_ELT (arg1, i + offset);
	    }
	}
      else
	for (i = 0; i < count; i++)
	  {
	    tree elem1 = VECTOR_CST_ELT (arg1, i);

	    elts[i] = const_binop (code, elem1, arg2);

	    /* It is possible that const_binop cannot handle the given
	       code and return NULL_TREE */
	    if (elts[i] == NULL_TREE)
	      return NULL_TREE;
	  }

      return build_vector (type, elts);
    }
  return NULL_TREE;
}

/* Create a sizetype INT_CST node with NUMBER sign extended.  KIND
   indicates which particular sizetype to create.  */

tree
size_int_kind (HOST_WIDE_INT number, enum size_type_kind kind)
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

  /* Handle the special case of two integer constants faster.  */
  if (TREE_CODE (arg0) == INTEGER_CST && TREE_CODE (arg1) == INTEGER_CST)
    {
      /* And some specific cases even faster than that.  */
      if (code == PLUS_EXPR)
	{
	  if (integer_zerop (arg0) && !TREE_OVERFLOW (arg0))
	    return arg1;
	  if (integer_zerop (arg1) && !TREE_OVERFLOW (arg1))
	    return arg0;
	}
      else if (code == MINUS_EXPR)
	{
	  if (integer_zerop (arg1) && !TREE_OVERFLOW (arg1))
	    return arg0;
	}
      else if (code == MULT_EXPR)
	{
	  if (integer_onep (arg0) && !TREE_OVERFLOW (arg0))
	    return arg1;
	}

      /* Handle general case of two integer constants.  For sizetype
         constant calculations we always want to know about overflow,
	 even in the unsigned case.  */
      return int_const_binop_1 (code, arg0, arg1, -1);
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
  tree t;

  /* Given an integer constant, make new constant with new type,
     appropriately sign-extended or truncated.  */
  t = force_fit_type_double (type, tree_to_double_int (arg1),
			     !POINTER_TYPE_P (TREE_TYPE (arg1)),
			     (TREE_INT_CST_HIGH (arg1) < 0
		 	      && (TYPE_UNSIGNED (type)
				  < TYPE_UNSIGNED (TREE_TYPE (arg1))))
			     | TREE_OVERFLOW (arg1));

  return t;
}

/* A subroutine of fold_convert_const handling conversions a REAL_CST
   to an integer type.  */

static tree
fold_convert_const_int_from_real (enum tree_code code, tree type, const_tree arg1)
{
  int overflow = 0;
  tree t;

  /* The following code implements the floating point to integer
     conversion rules required by the Java Language Specification,
     that IEEE NaNs are mapped to zero and values that overflow
     the target precision saturate, i.e. values greater than
     INT_MAX are mapped to INT_MAX, and values less than INT_MIN
     are mapped to INT_MIN.  These semantics are allowed by the
     C and C++ standards that simply state that the behavior of
     FP-to-integer conversion is unspecified upon overflow.  */

  double_int val;
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
      overflow = 1;
      val = double_int_zero;
    }

  /* See if R is less than the lower bound or greater than the
     upper bound.  */

  if (! overflow)
    {
      tree lt = TYPE_MIN_VALUE (type);
      REAL_VALUE_TYPE l = real_value_from_int_cst (NULL_TREE, lt);
      if (REAL_VALUES_LESS (r, l))
	{
	  overflow = 1;
	  val = tree_to_double_int (lt);
	}
    }

  if (! overflow)
    {
      tree ut = TYPE_MAX_VALUE (type);
      if (ut)
	{
	  REAL_VALUE_TYPE u = real_value_from_int_cst (NULL_TREE, ut);
	  if (REAL_VALUES_LESS (u, r))
	    {
	      overflow = 1;
	      val = tree_to_double_int (ut);
	    }
	}
    }

  if (! overflow)
    real_to_integer2 ((HOST_WIDE_INT *) &val.low, &val.high, &r);

  t = force_fit_type_double (type, val, -1, overflow | TREE_OVERFLOW (arg1));
  return t;
}

/* A subroutine of fold_convert_const handling conversions of a
   FIXED_CST to an integer type.  */

static tree
fold_convert_const_int_from_fixed (tree type, const_tree arg1)
{
  tree t;
  double_int temp, temp_trunc;
  unsigned int mode;

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
  t = force_fit_type_double (type, temp, -1,
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

  real_convert_from_fixed (&value, TYPE_MODE (type), &TREE_FIXED_CST (arg1));
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

  overflow_p = fixed_convert (&value, TYPE_MODE (type), &TREE_FIXED_CST (arg1),
			      TYPE_SATURATING (type));
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

  overflow_p = fixed_convert_from_int (&value, TYPE_MODE (type),
				       TREE_INT_CST (arg1),
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

  overflow_p = fixed_convert_from_real (&value, TYPE_MODE (type),
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
  if (TREE_TYPE (arg1) == type)
    return arg1;

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
  else if (TREE_CODE (type) == REAL_TYPE)
    {
      if (TREE_CODE (arg1) == INTEGER_CST)
	return build_real_from_int_cst (type, arg1);
      else if (TREE_CODE (arg1) == REAL_CST)
	return fold_convert_const_real_from_real (type, arg1);
      else if (TREE_CODE (arg1) == FIXED_CST)
	return fold_convert_const_real_from_fixed (type, arg1);
    }
  else if (TREE_CODE (type) == FIXED_POINT_TYPE)
    {
      if (TREE_CODE (arg1) == FIXED_CST)
	return fold_convert_const_fixed_from_fixed (type, arg1);
      else if (TREE_CODE (arg1) == INTEGER_CST)
	return fold_convert_const_fixed_from_int (type, arg1);
      else if (TREE_CODE (arg1) == REAL_CST)
	return fold_convert_const_fixed_from_real (type, arg1);
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
  tree orig = TREE_TYPE (arg);

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
      if (INTEGRAL_TYPE_P (orig) || POINTER_TYPE_P (orig)
	  || TREE_CODE (orig) == OFFSET_TYPE)
        return true;
      return (TREE_CODE (orig) == VECTOR_TYPE
	      && tree_int_cst_equal (TYPE_SIZE (type), TYPE_SIZE (orig)));

    case REAL_TYPE:
    case FIXED_POINT_TYPE:
    case COMPLEX_TYPE:
    case VECTOR_TYPE:
    case VOID_TYPE:
      return TREE_CODE (type) == TREE_CODE (orig);

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
    case OFFSET_TYPE:
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
      gcc_assert (TREE_CODE (orig) == VECTOR_TYPE
		  && tree_int_cst_equal (TYPE_SIZE (type), TYPE_SIZE (orig)));
      return fold_build1_loc (loc, NOP_EXPR, type, arg);

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
	case INTEGER_TYPE:
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
	case INTEGER_TYPE:
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
		  || TREE_CODE (orig) == VECTOR_TYPE);
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
  protected_set_expr_location_unshare (tem, loc);
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

/* Nonzero means lvalues are limited to those valid in pedantic ANSI C.
   Zero means allow extended lvalues.  */

int pedantic_lvalues;

/* When pedantic, return an expr equal to X but certainly not valid as a
   pedantic lvalue.  Otherwise, return X.  */

static tree
pedantic_non_lvalue_loc (location_t loc, tree x)
{
  if (pedantic_lvalues)
    return non_lvalue_loc (loc, x);

  return protected_set_expr_location_unshare (x, loc);
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
  bool honor_nans = HONOR_NANS (TYPE_MODE (TREE_TYPE (ll_arg)));
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
   are necessarily equal.  If either argument has side-effects this
   function returns zero.  FLAGS modifies behavior as follows:

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
   to ensure that global memory is unchanged in between.  */

int
operand_equal_p (const_tree arg0, const_tree arg1, unsigned int flags)
{
  /* If either is ERROR_MARK, they aren't equal.  */
  if (TREE_CODE (arg0) == ERROR_MARK || TREE_CODE (arg1) == ERROR_MARK
      || TREE_TYPE (arg0) == error_mark_node
      || TREE_TYPE (arg1) == error_mark_node)
    return 0;

  /* Similar, if either does not have a type (like a released SSA name), 
     they aren't equal.  */
  if (!TREE_TYPE (arg0) || !TREE_TYPE (arg1))
    return 0;

  /* Check equality of integer constants before bailing out due to
     precision differences.  */
  if (TREE_CODE (arg0) == INTEGER_CST && TREE_CODE (arg1) == INTEGER_CST)
    return tree_int_cst_equal (arg0, arg1);

  /* If both types don't have the same signedness, then we can't consider
     them equal.  We must check this before the STRIP_NOPS calls
     because they may change the signedness of the arguments.  As pointers
     strictly don't have a signedness, require either two pointers or
     two non-pointers as well.  */
  if (TYPE_UNSIGNED (TREE_TYPE (arg0)) != TYPE_UNSIGNED (TREE_TYPE (arg1))
      || POINTER_TYPE_P (TREE_TYPE (arg0)) != POINTER_TYPE_P (TREE_TYPE (arg1)))
    return 0;

  /* We cannot consider pointers to different address space equal.  */
  if (POINTER_TYPE_P (TREE_TYPE (arg0)) && POINTER_TYPE_P (TREE_TYPE (arg1))
      && (TYPE_ADDR_SPACE (TREE_TYPE (TREE_TYPE (arg0)))
	  != TYPE_ADDR_SPACE (TREE_TYPE (TREE_TYPE (arg1)))))
    return 0;

  /* If both types don't have the same precision, then it is not safe
     to strip NOPs.  */
  if (element_precision (TREE_TYPE (arg0))
      != element_precision (TREE_TYPE (arg1)))
    return 0;

  STRIP_NOPS (arg0);
  STRIP_NOPS (arg1);

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

  if (TREE_CODE (arg0) != TREE_CODE (arg1)
      /* NOP_EXPR and CONVERT_EXPR are considered equal.  */
      && !(CONVERT_EXPR_P (arg0) && CONVERT_EXPR_P (arg1)))
    return 0;

  /* This is needed for conversions and for COMPONENT_REF.
     Might as well play it safe and always test this.  */
  if (TREE_CODE (TREE_TYPE (arg0)) == ERROR_MARK
      || TREE_CODE (TREE_TYPE (arg1)) == ERROR_MARK
      || TYPE_MODE (TREE_TYPE (arg0)) != TYPE_MODE (TREE_TYPE (arg1)))
    return 0;

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
	  || (flags & OEP_CONSTANT_ADDRESS_OF)
	  || (! TREE_SIDE_EFFECTS (arg0) && ! TREE_SIDE_EFFECTS (arg1))))
    return 1;

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
	if (REAL_VALUES_IDENTICAL (TREE_REAL_CST (arg0),
				   TREE_REAL_CST (arg1)))
	  return 1;


	if (!HONOR_SIGNED_ZEROS (TYPE_MODE (TREE_TYPE (arg0))))
	  {
	    /* If we do not distinguish between signed and unsigned zero,
	       consider them equal.  */
	    if (real_zerop (arg0) && real_zerop (arg1))
	      return 1;
	  }
	return 0;

      case VECTOR_CST:
	{
	  unsigned i;

	  if (VECTOR_CST_NELTS (arg0) != VECTOR_CST_NELTS (arg1))
	    return 0;

	  for (i = 0; i < VECTOR_CST_NELTS (arg0); ++i)
	    {
	      if (!operand_equal_p (VECTOR_CST_ELT (arg0, i),
				    VECTOR_CST_ELT (arg1, i), flags))
		return 0;
	    }
	  return 1;
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
	return operand_equal_p (TREE_OPERAND (arg0, 0), TREE_OPERAND (arg1, 0),
				TREE_CONSTANT (arg0) && TREE_CONSTANT (arg1)
				? OEP_CONSTANT_ADDRESS_OF : 0);
      default:
	break;
      }

  if (flags & OEP_ONLY_CONST)
    return 0;

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
	    return 0;
	  break;
	default:
	  break;
	}

      return OP_SAME (0);


    case tcc_comparison:
    case tcc_binary:
      if (OP_SAME (0) && OP_SAME (1))
	return 1;

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
      if ((flags & OEP_CONSTANT_ADDRESS_OF) == 0
	  && (TREE_SIDE_EFFECTS (arg0)
	      || TREE_SIDE_EFFECTS (arg1)))
	return 0;

      switch (TREE_CODE (arg0))
	{
	case INDIRECT_REF:
	  flags &= ~OEP_CONSTANT_ADDRESS_OF;
	  return OP_SAME (0);

	case REALPART_EXPR:
	case IMAGPART_EXPR:
	  return OP_SAME (0);

	case TARGET_MEM_REF:
	  flags &= ~OEP_CONSTANT_ADDRESS_OF;
	  /* Require equal extra operands and then fall through to MEM_REF
	     handling of the two common operands.  */
	  if (!OP_SAME_WITH_NULL (2)
	      || !OP_SAME_WITH_NULL (3)
	      || !OP_SAME_WITH_NULL (4))
	    return 0;
	  /* Fallthru.  */
	case MEM_REF:
	  flags &= ~OEP_CONSTANT_ADDRESS_OF;
	  /* Require equal access sizes, and similar pointer types.
	     We can have incomplete types for array references of
	     variable-sized arrays from the Fortran frontend
	     though.  Also verify the types are compatible.  */
	  return ((TYPE_SIZE (TREE_TYPE (arg0)) == TYPE_SIZE (TREE_TYPE (arg1))
		   || (TYPE_SIZE (TREE_TYPE (arg0))
		       && TYPE_SIZE (TREE_TYPE (arg1))
		       && operand_equal_p (TYPE_SIZE (TREE_TYPE (arg0)),
					   TYPE_SIZE (TREE_TYPE (arg1)), flags)))
		  && types_compatible_p (TREE_TYPE (arg0), TREE_TYPE (arg1))
		  && alias_ptr_types_compatible_p
		       (TREE_TYPE (TREE_OPERAND (arg0, 1)),
			TREE_TYPE (TREE_OPERAND (arg1, 1)))
		  && OP_SAME (0) && OP_SAME (1));

	case ARRAY_REF:
	case ARRAY_RANGE_REF:
	  /* Operands 2 and 3 may be null.
	     Compare the array index by value if it is constant first as we
	     may have different types but same value here.  */
	  if (!OP_SAME (0))
	    return 0;
	  flags &= ~OEP_CONSTANT_ADDRESS_OF;
	  return ((tree_int_cst_equal (TREE_OPERAND (arg0, 1),
				       TREE_OPERAND (arg1, 1))
		   || OP_SAME (1))
		  && OP_SAME_WITH_NULL (2)
		  && OP_SAME_WITH_NULL (3));

	case COMPONENT_REF:
	  /* Handle operand 2 the same as for ARRAY_REF.  Operand 0
	     may be NULL when we're called to compare MEM_EXPRs.  */
	  if (!OP_SAME_WITH_NULL (0)
	      || !OP_SAME (1))
	    return 0;
	  flags &= ~OEP_CONSTANT_ADDRESS_OF;
	  return OP_SAME_WITH_NULL (2);

	case BIT_FIELD_REF:
	  if (!OP_SAME (0))
	    return 0;
	  flags &= ~OEP_CONSTANT_ADDRESS_OF;
	  return OP_SAME (1) && OP_SAME (2);

	default:
	  return 0;
	}

    case tcc_expression:
      switch (TREE_CODE (arg0))
	{
	case ADDR_EXPR:
	case TRUTH_NOT_EXPR:
	  return OP_SAME (0);

	case TRUTH_ANDIF_EXPR:
	case TRUTH_ORIF_EXPR:
	  return OP_SAME (0) && OP_SAME (1);

	case FMA_EXPR:
	case WIDEN_MULT_PLUS_EXPR:
	case WIDEN_MULT_MINUS_EXPR:
	  if (!OP_SAME (2))
	    return 0;
	  /* The multiplcation operands are commutative.  */
	  /* FALLTHRU */

	case TRUTH_AND_EXPR:
	case TRUTH_OR_EXPR:
	case TRUTH_XOR_EXPR:
	  if (OP_SAME (0) && OP_SAME (1))
	    return 1;

	  /* Otherwise take into account this is a commutative operation.  */
	  return (operand_equal_p (TREE_OPERAND (arg0, 0),
				   TREE_OPERAND (arg1, 1), flags)
		  && operand_equal_p (TREE_OPERAND (arg0, 1),
				      TREE_OPERAND (arg1, 0), flags));

	case COND_EXPR:
	case VEC_COND_EXPR:
	case DOT_PROD_EXPR:
	  return OP_SAME (0) && OP_SAME (1) && OP_SAME (2);

	default:
	  return 0;
	}

    case tcc_vl_exp:
      switch (TREE_CODE (arg0))
	{
	case CALL_EXPR:
	  /* If the CALL_EXPRs call different functions, then they
	     clearly can not be equal.  */
	  if (! operand_equal_p (CALL_EXPR_FN (arg0), CALL_EXPR_FN (arg1),
				 flags))
	    return 0;

	  {
	    unsigned int cef = call_expr_flags (arg0);
	    if (flags & OEP_PURE_SAME)
	      cef &= ECF_CONST | ECF_PURE;
	    else
	      cef &= ECF_CONST;
	    if (!cef)
	      return 0;
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
		return 0;

	    /* If we get here and both argument lists are exhausted
	       then the CALL_EXPRs are equal.  */
	    return ! (a0 || a1);
	  }
	default:
	  return 0;
	}

    case tcc_declaration:
      /* Consider __builtin_sqrt equal to sqrt.  */
      return (TREE_CODE (arg0) == FUNCTION_DECL
	      && DECL_BUILT_IN (arg0) && DECL_BUILT_IN (arg1)
	      && DECL_BUILT_IN_CLASS (arg0) == DECL_BUILT_IN_CLASS (arg1)
	      && DECL_FUNCTION_CODE (arg0) == DECL_FUNCTION_CODE (arg1));

    default:
      return 0;
    }

#undef OP_SAME
#undef OP_SAME_WITH_NULL
}

/* Similar to operand_equal_p, but see if ARG0 might have been made by
   shorten_compare from ARG1 when ARG1 was being compared with OTHER.

   When in doubt, return 0.  */

static int
operand_equal_for_comparison_p (tree arg0, tree arg1, tree other)
{
  int unsignedp1, unsignedpo;
  tree primarg0, primarg1, primother;
  unsigned int correct_width;

  if (operand_equal_p (arg0, arg1, 0))
    return 1;

  if (! INTEGRAL_TYPE_P (TREE_TYPE (arg0))
      || ! INTEGRAL_TYPE_P (TREE_TYPE (arg1)))
    return 0;

  /* Discard any conversions that don't change the modes of ARG0 and ARG1
     and see if the inner values are the same.  This removes any
     signedness comparison, which doesn't matter here.  */
  primarg0 = arg0, primarg1 = arg1;
  STRIP_NOPS (primarg0);
  STRIP_NOPS (primarg1);
  if (operand_equal_p (primarg0, primarg1, 0))
    return 1;

  /* Duplicate what shorten_compare does to ARG1 and see if that gives the
     actual comparison operand, ARG0.

     First throw away any conversions to wider types
     already present in the operands.  */

  primarg1 = get_narrower (arg1, &unsignedp1);
  primother = get_narrower (other, &unsignedpo);

  correct_width = TYPE_PRECISION (TREE_TYPE (arg1));
  if (unsignedp1 == unsignedpo
      && TYPE_PRECISION (TREE_TYPE (primarg1)) < correct_width
      && TYPE_PRECISION (TREE_TYPE (primother)) < correct_width)
    {
      tree type = TREE_TYPE (arg0);

      /* Make sure shorter operand is extended the right way
	 to match the longer operand.  */
      primarg1 = fold_convert (signed_or_unsigned_type_for
			       (unsignedp1, TREE_TYPE (primarg1)), primarg1);

      if (operand_equal_p (arg0, fold_convert (type, primarg1), 0))
	return 1;
    }

  return 0;
}

/* See if ARG is an expression that is either a comparison or is performing
   arithmetic on comparisons.  The comparisons must only be comparing
   two different values, which will be stored in *CVAL1 and *CVAL2; if
   they are nonzero it means that some operands have already been found.
   No variables may be used anywhere else in the expression except in the
   comparisons.  If SAVE_P is true it means we removed a SAVE_EXPR around
   the expression and save_expr needs to be called with CVAL1 and CVAL2.

   If this is true, return 1.  Otherwise, return zero.  */

static int
twoval_comparison_p (tree arg, tree *cval1, tree *cval2, int *save_p)
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

  else if (tclass == tcc_expression && code == SAVE_EXPR
	   && ! TREE_SIDE_EFFECTS (TREE_OPERAND (arg, 0)))
    {
      /* If we've already found a CVAL1 or CVAL2, this expression is
	 two complex to handle.  */
      if (*cval1 || *cval2)
	return 0;

      tclass = tcc_unary;
      *save_p = 1;
    }

  switch (tclass)
    {
    case tcc_unary:
      return twoval_comparison_p (TREE_OPERAND (arg, 0), cval1, cval2, save_p);

    case tcc_binary:
      return (twoval_comparison_p (TREE_OPERAND (arg, 0), cval1, cval2, save_p)
	      && twoval_comparison_p (TREE_OPERAND (arg, 1),
				      cval1, cval2, save_p));

    case tcc_constant:
      return 1;

    case tcc_expression:
      if (code == COND_EXPR)
	return (twoval_comparison_p (TREE_OPERAND (arg, 0),
				     cval1, cval2, save_p)
		&& twoval_comparison_p (TREE_OPERAND (arg, 1),
					cval1, cval2, save_p)
		&& twoval_comparison_p (TREE_OPERAND (arg, 2),
					cval1, cval2, save_p));
      return 0;

    case tcc_comparison:
      /* First see if we can handle the first operand, then the second.  For
	 the second operand, we know *CVAL1 can't be zero.  It must be that
	 one side of the comparison is each of the values; test for the
	 case where this isn't true by failing if the two operands
	 are the same.  */

      if (operand_equal_p (TREE_OPERAND (arg, 0),
			   TREE_OPERAND (arg, 1), 0))
	return 0;

      if (*cval1 == 0)
	*cval1 = TREE_OPERAND (arg, 0);
      else if (operand_equal_p (*cval1, TREE_OPERAND (arg, 0), 0))
	;
      else if (*cval2 == 0)
	*cval2 = TREE_OPERAND (arg, 0);
      else if (operand_equal_p (*cval2, TREE_OPERAND (arg, 0), 0))
	;
      else
	return 0;

      if (operand_equal_p (*cval1, TREE_OPERAND (arg, 1), 0))
	;
      else if (*cval2 == 0)
	*cval2 = TREE_OPERAND (arg, 1);
      else if (operand_equal_p (*cval2, TREE_OPERAND (arg, 1), 0))
	;
      else
	return 0;

      return 1;

    default:
      return 0;
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

/* Similar, but call pedantic_non_lvalue instead of non_lvalue.  */

static tree
pedantic_omit_one_operand_loc (location_t loc, tree type, tree result,
			       tree omitted)
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

  return pedantic_non_lvalue_loc (loc, t);
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

      code = invert_tree_comparison (code, HONOR_NANS (TYPE_MODE (op_type)));
      if (code == ERROR_MARK)
	return NULL_TREE;

      return build2_loc (loc, code, type, TREE_OPERAND (arg, 0),
			 TREE_OPERAND (arg, 1));
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

      /* ... fall through ...  */

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

/* Given a bit-wise operation CODE applied to ARG0 and ARG1, see if both
   operands are another bit-wise operation with a common input.  If so,
   distribute the bit operations to save an operation and possibly two if
   constants are involved.  For example, convert
	(A | B) & (A | C) into A | (B & C)
   Further simplification will occur if B and C are constants.

   If this optimization cannot be done, 0 will be returned.  */

static tree
distribute_bit_expr (location_t loc, enum tree_code code, tree type,
		     tree arg0, tree arg1)
{
  tree common;
  tree left, right;

  if (TREE_CODE (arg0) != TREE_CODE (arg1)
      || TREE_CODE (arg0) == code
      || (TREE_CODE (arg0) != BIT_AND_EXPR
	  && TREE_CODE (arg0) != BIT_IOR_EXPR))
    return 0;

  if (operand_equal_p (TREE_OPERAND (arg0, 0), TREE_OPERAND (arg1, 0), 0))
    {
      common = TREE_OPERAND (arg0, 0);
      left = TREE_OPERAND (arg0, 1);
      right = TREE_OPERAND (arg1, 1);
    }
  else if (operand_equal_p (TREE_OPERAND (arg0, 0), TREE_OPERAND (arg1, 1), 0))
    {
      common = TREE_OPERAND (arg0, 0);
      left = TREE_OPERAND (arg0, 1);
      right = TREE_OPERAND (arg1, 0);
    }
  else if (operand_equal_p (TREE_OPERAND (arg0, 1), TREE_OPERAND (arg1, 0), 0))
    {
      common = TREE_OPERAND (arg0, 1);
      left = TREE_OPERAND (arg0, 0);
      right = TREE_OPERAND (arg1, 1);
    }
  else if (operand_equal_p (TREE_OPERAND (arg0, 1), TREE_OPERAND (arg1, 1), 0))
    {
      common = TREE_OPERAND (arg0, 1);
      left = TREE_OPERAND (arg0, 0);
      right = TREE_OPERAND (arg1, 0);
    }
  else
    return 0;

  common = fold_convert_loc (loc, type, common);
  left = fold_convert_loc (loc, type, left);
  right = fold_convert_loc (loc, type, right);
  return fold_build2_loc (loc, TREE_CODE (arg0), type, common,
		      fold_build2_loc (loc, code, type, left, right));
}

/* Knowing that ARG0 and ARG1 are both RDIV_EXPRs, simplify a binary operation
   with code CODE.  This optimization is unsafe.  */
static tree
distribute_real_division (location_t loc, enum tree_code code, tree type,
			  tree arg0, tree arg1)
{
  bool mul0 = TREE_CODE (arg0) == MULT_EXPR;
  bool mul1 = TREE_CODE (arg1) == MULT_EXPR;

  /* (A / C) +- (B / C) -> (A +- B) / C.  */
  if (mul0 == mul1
      && operand_equal_p (TREE_OPERAND (arg0, 1),
		       TREE_OPERAND (arg1, 1), 0))
    return fold_build2_loc (loc, mul0 ? MULT_EXPR : RDIV_EXPR, type,
			fold_build2_loc (loc, code, type,
				     TREE_OPERAND (arg0, 0),
				     TREE_OPERAND (arg1, 0)),
			TREE_OPERAND (arg0, 1));

  /* (A / C1) +- (A / C2) -> A * (1 / C1 +- 1 / C2).  */
  if (operand_equal_p (TREE_OPERAND (arg0, 0),
		       TREE_OPERAND (arg1, 0), 0)
      && TREE_CODE (TREE_OPERAND (arg0, 1)) == REAL_CST
      && TREE_CODE (TREE_OPERAND (arg1, 1)) == REAL_CST)
    {
      REAL_VALUE_TYPE r0, r1;
      r0 = TREE_REAL_CST (TREE_OPERAND (arg0, 1));
      r1 = TREE_REAL_CST (TREE_OPERAND (arg1, 1));
      if (!mul0)
	real_arithmetic (&r0, RDIV_EXPR, &dconst1, &r0);
      if (!mul1)
        real_arithmetic (&r1, RDIV_EXPR, &dconst1, &r1);
      real_arithmetic (&r0, code, &r0, &r1);
      return fold_build2_loc (loc, MULT_EXPR, type,
			  TREE_OPERAND (arg0, 0),
			  build_real (type, r0));
    }

  return NULL_TREE;
}

/* Return a BIT_FIELD_REF of type TYPE to refer to BITSIZE bits of INNER
   starting at BITPOS.  The field is unsigned if UNSIGNEDP is nonzero.  */

static tree
make_bit_field_ref (location_t loc, tree inner, tree type,
		    HOST_WIDE_INT bitsize, HOST_WIDE_INT bitpos, int unsignedp)
{
  tree result, bftype;

  if (bitpos == 0)
    {
      tree size = TYPE_SIZE (TREE_TYPE (inner));
      if ((INTEGRAL_TYPE_P (TREE_TYPE (inner))
	   || POINTER_TYPE_P (TREE_TYPE (inner)))
	  && host_integerp (size, 0)
	  && tree_low_cst (size, 0) == bitsize)
	return fold_convert_loc (loc, type, inner);
    }

  bftype = type;
  if (TYPE_PRECISION (bftype) != bitsize
      || TYPE_UNSIGNED (bftype) == !unsignedp)
    bftype = build_nonstandard_integer_type (bitsize, 0);

  result = build3_loc (loc, BIT_FIELD_REF, bftype, inner,
		       size_int (bitsize), bitsize_int (bitpos));

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
  HOST_WIDE_INT lbitpos, lbitsize, rbitpos, rbitsize, nbitpos, nbitsize;
  tree type = TREE_TYPE (lhs);
  tree signed_type, unsigned_type;
  int const_p = TREE_CODE (rhs) == INTEGER_CST;
  enum machine_mode lmode, rmode, nmode;
  int lunsignedp, runsignedp;
  int lvolatilep = 0, rvolatilep = 0;
  tree linner, rinner = NULL_TREE;
  tree mask;
  tree offset;

  /* Get all the information about the extractions being done.  If the bit size
     if the same as the size of the underlying object, we aren't doing an
     extraction at all and so can do nothing.  We also don't want to
     do anything if the inner expression is a PLACEHOLDER_EXPR since we
     then will no longer be able to replace it.  */
  linner = get_inner_reference (lhs, &lbitsize, &lbitpos, &offset, &lmode,
				&lunsignedp, &lvolatilep, false);
  if (linner == lhs || lbitsize == GET_MODE_BITSIZE (lmode) || lbitsize < 0
      || offset != 0 || TREE_CODE (linner) == PLACEHOLDER_EXPR || lvolatilep)
    return 0;

 if (!const_p)
   {
     /* If this is not a constant, we can only do something if bit positions,
	sizes, and signedness are the same.  */
     rinner = get_inner_reference (rhs, &rbitsize, &rbitpos, &offset, &rmode,
				   &runsignedp, &rvolatilep, false);

     if (rinner == rhs || lbitpos != rbitpos || lbitsize != rbitsize
	 || lunsignedp != runsignedp || offset != 0
	 || TREE_CODE (rinner) == PLACEHOLDER_EXPR || rvolatilep)
       return 0;
   }

  /* See if we can find a mode to refer to this field.  We should be able to,
     but fail if we can't.  */
  nmode = get_best_mode (lbitsize, lbitpos, 0, 0,
			 const_p ? TYPE_ALIGN (TREE_TYPE (linner))
			 : MIN (TYPE_ALIGN (TREE_TYPE (linner)),
				TYPE_ALIGN (TREE_TYPE (rinner))),
			 word_mode, false);
  if (nmode == VOIDmode)
    return 0;

  /* Set signed and unsigned types of the precision of this mode for the
     shifts below.  */
  signed_type = lang_hooks.types.type_for_mode (nmode, 0);
  unsigned_type = lang_hooks.types.type_for_mode (nmode, 1);

  /* Compute the bit position and size for the new reference and our offset
     within it. If the new reference is the same size as the original, we
     won't optimize anything, so return zero.  */
  nbitsize = GET_MODE_BITSIZE (nmode);
  nbitpos = lbitpos & ~ (nbitsize - 1);
  lbitpos -= nbitpos;
  if (nbitsize == lbitsize)
    return 0;

  if (BYTES_BIG_ENDIAN)
    lbitpos = nbitsize - lbitsize - lbitpos;

  /* Make the mask to be used against the extracted field.  */
  mask = build_int_cst_type (unsigned_type, -1);
  mask = const_binop (LSHIFT_EXPR, mask, size_int (nbitsize - lbitsize));
  mask = const_binop (RSHIFT_EXPR, mask,
		      size_int (nbitsize - lbitsize - lbitpos));

  if (! const_p)
    /* If not comparing with constant, just rework the comparison
       and return.  */
    return fold_build2_loc (loc, code, compare_type,
			fold_build2_loc (loc, BIT_AND_EXPR, unsigned_type,
				     make_bit_field_ref (loc, linner,
							 unsigned_type,
							 nbitsize, nbitpos,
							 1),
				     mask),
			fold_build2_loc (loc, BIT_AND_EXPR, unsigned_type,
				     make_bit_field_ref (loc, rinner,
							 unsigned_type,
							 nbitsize, nbitpos,
							 1),
				     mask));

  /* Otherwise, we are handling the constant case. See if the constant is too
     big for the field.  Warn and return a tree of for 0 (false) if so.  We do
     this not only for its own sake, but to avoid having to test for this
     error case below.  If we didn't, we might generate wrong code.

     For unsigned fields, the constant shifted right by the field length should
     be all zero.  For signed fields, the high-order bits should agree with
     the sign bit.  */

  if (lunsignedp)
    {
      if (! integer_zerop (const_binop (RSHIFT_EXPR,
					fold_convert_loc (loc,
							  unsigned_type, rhs),
					size_int (lbitsize))))
	{
	  warning (0, "comparison is always %d due to width of bit-field",
		   code == NE_EXPR);
	  return constant_boolean_node (code == NE_EXPR, compare_type);
	}
    }
  else
    {
      tree tem = const_binop (RSHIFT_EXPR,
			      fold_convert_loc (loc, signed_type, rhs),
			      size_int (lbitsize - 1));
      if (! integer_zerop (tem) && ! integer_all_onesp (tem))
	{
	  warning (0, "comparison is always %d due to width of bit-field",
		   code == NE_EXPR);
	  return constant_boolean_node (code == NE_EXPR, compare_type);
	}
    }

  /* Single-bit compares should always be against zero.  */
  if (lbitsize == 1 && ! integer_zerop (rhs))
    {
      code = code == EQ_EXPR ? NE_EXPR : EQ_EXPR;
      rhs = build_int_cst (type, 0);
    }

  /* Make a new bitfield reference, shift the constant over the
     appropriate number of bits and mask it with the computed mask
     (in case this was a signed field).  If we changed it, make a new one.  */
  lhs = make_bit_field_ref (loc, linner, unsigned_type, nbitsize, nbitpos, 1);

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

   *PMASK is set to the mask used.  This is either contained in a
   BIT_AND_EXPR or derived from the width of the field.

   *PAND_MASK is set to the mask found in a BIT_AND_EXPR, if any.

   Return 0 if this is not a component reference or is one that we can't
   do anything with.  */

static tree
decode_field_reference (location_t loc, tree exp, HOST_WIDE_INT *pbitsize,
			HOST_WIDE_INT *pbitpos, enum machine_mode *pmode,
			int *punsignedp, int *pvolatilep,
			tree *pmask, tree *pand_mask)
{
  tree outer_type = 0;
  tree and_mask = 0;
  tree mask, inner, offset;
  tree unsigned_type;
  unsigned int precision;

  /* All the optimizations using this function assume integer fields.
     There are problems with FP fields since the type_for_size call
     below can fail for, e.g., XFmode.  */
  if (! INTEGRAL_TYPE_P (TREE_TYPE (exp)))
    return 0;

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
	return 0;
    }

  inner = get_inner_reference (exp, pbitsize, pbitpos, &offset, pmode,
			       punsignedp, pvolatilep, false);
  if ((inner == exp && and_mask == 0)
      || *pbitsize < 0 || offset != 0
      || TREE_CODE (inner) == PLACEHOLDER_EXPR)
    return 0;

  /* If the number of bits in the reference is the same as the bitsize of
     the outer type, then the outer type gives the signedness. Otherwise
     (in case of a small bitfield) the signedness is unchanged.  */
  if (outer_type && *pbitsize == TYPE_PRECISION (outer_type))
    *punsignedp = TYPE_UNSIGNED (outer_type);

  /* Compute the mask to access the bitfield.  */
  unsigned_type = lang_hooks.types.type_for_size (*pbitsize, 1);
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
   bit positions.  */

static int
all_ones_mask_p (const_tree mask, int size)
{
  tree type = TREE_TYPE (mask);
  unsigned int precision = TYPE_PRECISION (type);
  tree tmask;

  tmask = build_int_cst_type (signed_type_for (type), -1);

  return
    tree_int_cst_equal (mask,
			const_binop (RSHIFT_EXPR,
				     const_binop (LSHIFT_EXPR, tmask,
						  size_int (precision - size)),
				     size_int (precision - size)));
}

/* Subroutine for fold: determine if VAL is the INTEGER_CONST that
   represents the sign bit of EXP's type.  If EXP represents a sign
   or zero extension, also test VAL against the unextended type.
   The return value is the (sub)expression whose sign bit is VAL,
   or NULL_TREE otherwise.  */

static tree
sign_bit_p (tree exp, const_tree val)
{
  unsigned HOST_WIDE_INT mask_lo, lo;
  HOST_WIDE_INT mask_hi, hi;
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
  if (width > HOST_BITS_PER_WIDE_INT)
    {
      hi = (unsigned HOST_WIDE_INT) 1 << (width - HOST_BITS_PER_WIDE_INT - 1);
      lo = 0;

      mask_hi = (HOST_WIDE_INT_M1U >> (HOST_BITS_PER_DOUBLE_INT - width));
      mask_lo = -1;
    }
  else
    {
      hi = 0;
      lo = (unsigned HOST_WIDE_INT) 1 << (width - 1);

      mask_hi = 0;
      mask_lo = (HOST_WIDE_INT_M1U >> (HOST_BITS_PER_WIDE_INT - width));
    }

  /* We mask off those bits beyond TREE_TYPE (exp) so that we can
     treat VAL as if it were unsigned.  */
  if ((TREE_INT_CST_HIGH (val) & mask_hi) == hi
      && (TREE_INT_CST_LOW (val) & mask_lo) == lo)
    return exp;

  /* Handle extension from a narrower type.  */
  if (TREE_CODE (exp) == NOP_EXPR
      && TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (exp, 0))) < width)
    return sign_bit_p (TREE_OPERAND (exp, 0), val);

  return NULL_TREE;
}

/* Subroutine for fold_truth_andor_1: determine if an operand is simple enough
   to be evaluated unconditionally.  */

static int
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

/* Subroutine for fold_truth_andor: determine if an operand is simple enough
   to be evaluated unconditionally.
   I addition to simple_operand_p, we assume that comparisons, conversions,
   and logic-not operations are simple, if their operands are simple, too.  */

static bool
simple_operand_p_2 (tree exp)
{
  enum tree_code code;

  if (TREE_SIDE_EFFECTS (exp)
      || tree_could_trap_p (exp))
    return false;

  while (CONVERT_EXPR_P (exp))
    exp = TREE_OPERAND (exp, 0);

  code = TREE_CODE (exp);

  if (TREE_CODE_CLASS (code) == tcc_comparison)
    return (simple_operand_p (TREE_OPERAND (exp, 0))
	    && simple_operand_p (TREE_OPERAND (exp, 1)));

  if (code == TRUTH_NOT_EXPR)
      return simple_operand_p_2 (TREE_OPERAND (exp, 0));

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
				  integer_one_node, 0);
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
			       integer_one_node, 0);
	    high = range_binop (MINUS_EXPR, arg0_type, n_low, 0,
				integer_one_node, 0);

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
	 a signed type,  we will be doing the comparison as unsigned.
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

/* Given a range, LOW, HIGH, and IN_P, an expression, EXP, and a result
   type, TYPE, return an expression to test if EXP is in (or out of, depending
   on IN_P) the range.  Return 0 if the test couldn't be created.  */

tree
build_range_check (location_t loc, tree type, tree exp, int in_p,
		   tree low, tree high)
{
  tree etype = TREE_TYPE (exp), value;

#ifdef HAVE_canonicalize_funcptr_for_compare
  /* Disable this optimization for function pointer expressions
     on targets that require function pointer canonicalization.  */
  if (HAVE_canonicalize_funcptr_for_compare
      && TREE_CODE (etype) == POINTER_TYPE
      && TREE_CODE (TREE_TYPE (etype)) == FUNCTION_TYPE)
    return NULL_TREE;
#endif

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
      unsigned HOST_WIDE_INT lo;
      HOST_WIDE_INT hi;
      int prec;

      prec = TYPE_PRECISION (etype);
      if (prec <= HOST_BITS_PER_WIDE_INT)
	{
	  hi = 0;
	  lo = ((unsigned HOST_WIDE_INT) 1 << (prec - 1)) - 1;
	}
      else
	{
	  hi = ((HOST_WIDE_INT) 1 << (prec - HOST_BITS_PER_WIDE_INT - 1)) - 1;
	  lo = HOST_WIDE_INT_M1U;
	}

      if (TREE_INT_CST_HIGH (high) == hi && TREE_INT_CST_LOW (high) == lo)
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
     This requires wrap-around arithmetics for the type of the expression.
     First make sure that arithmetics in this type is valid, then make sure
     that it wraps around.  */
  if (TREE_CODE (etype) == ENUMERAL_TYPE || TREE_CODE (etype) == BOOLEAN_TYPE)
    etype = lang_hooks.types.type_for_size (TYPE_PRECISION (etype),
					    TYPE_UNSIGNED (etype));

  if (TREE_CODE (etype) == INTEGER_TYPE && !TYPE_OVERFLOW_WRAPS (etype))
    {
      tree utype, minv, maxv;

      /* Check if (unsigned) INT_MAX + 1 == (unsigned) INT_MIN
	 for the type in question, as we rely on this here.  */
      utype = unsigned_type_for (etype);
      maxv = fold_convert_loc (loc, utype, TYPE_MAX_VALUE (etype));
      maxv = range_binop (PLUS_EXPR, NULL_TREE, maxv, 1,
			  integer_one_node, 1);
      minv = fold_convert_loc (loc, utype, TYPE_MIN_VALUE (etype));

      if (integer_zerop (range_binop (NE_EXPR, integer_type_node,
				      minv, 1, maxv, 1)))
	etype = utype;
      else
	return 0;
    }

  high = fold_convert_loc (loc, etype, high);
  low = fold_convert_loc (loc, etype, low);
  exp = fold_convert_loc (loc, etype, exp);

  value = const_binop (MINUS_EXPR, high, low);


  if (POINTER_TYPE_P (etype))
    {
      if (value != 0 && !TREE_OVERFLOW (value))
	{
	  low = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (low), low);
          return build_range_check (loc, type,
			     	    fold_build_pointer_plus_loc (loc, exp, low),
			            1, build_int_cst (etype, 0), value);
	}
      return 0;
    }

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
    return range_binop (MINUS_EXPR, NULL_TREE, val, 0, integer_one_node, 0);
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
    return range_binop (PLUS_EXPR, NULL_TREE, val, 0, integer_one_node, 0);
}

/* Given two ranges, see if we can merge them into one.  Return 1 if we
   can, 0 if we can't.  Set the output range into the specified parameters.  */

bool
merge_ranges (int *pin_p, tree *plow, tree *phigh, int in0_p, tree low0,
	      tree high0, int in1_p, tree low1, tree high1)
{
  int no_overlap;
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
		    if (TYPE_PRECISION (TREE_TYPE (low0))
			!= GET_MODE_BITSIZE (TYPE_MODE (TREE_TYPE (low0))))
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
		    if (TYPE_PRECISION (TREE_TYPE (high1))
			!= GET_MODE_BITSIZE (TYPE_MODE (TREE_TYPE (high1))))
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
						       integer_one_node, 1)))
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
   A op B ? A : C, where ARG0, ARG1 and ARG2 are the three operands
   of the COND_EXPR.  This function is being used also to optimize
   A op B ? C : A, by reversing the comparison first.

   Return a folded expression whose code is not a COND_EXPR
   anymore, or NULL_TREE if no folding opportunity is found.  */

static tree
fold_cond_expr_with_comparison (location_t loc, tree type,
				tree arg0, tree arg1, tree arg2)
{
  enum tree_code comp_code = TREE_CODE (arg0);
  tree arg00 = TREE_OPERAND (arg0, 0);
  tree arg01 = TREE_OPERAND (arg0, 1);
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
  if (!HONOR_SIGNED_ZEROS (TYPE_MODE (type))
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
	return pedantic_non_lvalue_loc (loc,
				    fold_convert_loc (loc, type,
						  negate_expr (tem)));
      case NE_EXPR:
      case LTGT_EXPR:
	return pedantic_non_lvalue_loc (loc, fold_convert_loc (loc, type, arg1));
      case UNGE_EXPR:
      case UNGT_EXPR:
	if (flag_trapping_math)
	  break;
	/* Fall through.  */
      case GE_EXPR:
      case GT_EXPR:
	if (TYPE_UNSIGNED (TREE_TYPE (arg1)))
	  arg1 = fold_convert_loc (loc, signed_type_for
			       (TREE_TYPE (arg1)), arg1);
	tem = fold_build1_loc (loc, ABS_EXPR, TREE_TYPE (arg1), arg1);
	return pedantic_non_lvalue_loc (loc, fold_convert_loc (loc, type, tem));
      case UNLE_EXPR:
      case UNLT_EXPR:
	if (flag_trapping_math)
	  break;
      case LE_EXPR:
      case LT_EXPR:
	if (TYPE_UNSIGNED (TREE_TYPE (arg1)))
	  arg1 = fold_convert_loc (loc, signed_type_for
			       (TREE_TYPE (arg1)), arg1);
	tem = fold_build1_loc (loc, ABS_EXPR, TREE_TYPE (arg1), arg1);
	return negate_expr (fold_convert_loc (loc, type, tem));
      default:
	gcc_assert (TREE_CODE_CLASS (comp_code) == tcc_comparison);
	break;
      }

  /* A != 0 ? A : 0 is simply A, unless A is -0.  Likewise
     A == 0 ? A : 0 is always 0 unless A is -0.  Note that
     both transformations are correct when A is NaN: A != 0
     is then true, and A == 0 is false.  */

  if (!HONOR_SIGNED_ZEROS (TYPE_MODE (type))
      && integer_zerop (arg01) && integer_zerop (arg2))
    {
      if (comp_code == NE_EXPR)
	return pedantic_non_lvalue_loc (loc, fold_convert_loc (loc, type, arg1));
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
  if (!HONOR_SIGNED_ZEROS (TYPE_MODE (type))
      && operand_equal_for_comparison_p (arg01, arg2, arg00)
      /* Avoid these transformations if the COND_EXPR may be used
	 as an lvalue in the C++ front-end.  PR c++/19199.  */
      && (in_gimple_form
	  || VECTOR_TYPE_P (type)
	  || (strcmp (lang_hooks.name, "GNU C++") != 0
	      && strcmp (lang_hooks.name, "GNU Objective-C++") != 0)
	  || ! maybe_lvalue_p (arg1)
	  || ! maybe_lvalue_p (arg2)))
    {
      tree comp_op0 = arg00;
      tree comp_op1 = arg01;
      tree comp_type = TREE_TYPE (comp_op0);

      /* Avoid adding NOP_EXPRs in case this is an lvalue.  */
      if (TYPE_MAIN_VARIANT (comp_type) == TYPE_MAIN_VARIANT (type))
	{
	  comp_type = type;
	  comp_op0 = arg1;
	  comp_op1 = arg2;
	}

      switch (comp_code)
	{
	case EQ_EXPR:
	  return pedantic_non_lvalue_loc (loc, fold_convert_loc (loc, type, arg2));
	case NE_EXPR:
	  return pedantic_non_lvalue_loc (loc, fold_convert_loc (loc, type, arg1));
	case LE_EXPR:
	case LT_EXPR:
	case UNLE_EXPR:
	case UNLT_EXPR:
	  /* In C++ a ?: expression can be an lvalue, so put the
	     operand which will be used if they are equal first
	     so that we can convert this back to the
	     corresponding COND_EXPR.  */
	  if (!HONOR_NANS (TYPE_MODE (TREE_TYPE (arg1))))
	    {
	      comp_op0 = fold_convert_loc (loc, comp_type, comp_op0);
	      comp_op1 = fold_convert_loc (loc, comp_type, comp_op1);
	      tem = (comp_code == LE_EXPR || comp_code == UNLE_EXPR)
		    ? fold_build2_loc (loc, MIN_EXPR, comp_type, comp_op0, comp_op1)
		    : fold_build2_loc (loc, MIN_EXPR, comp_type,
				   comp_op1, comp_op0);
	      return pedantic_non_lvalue_loc (loc,
					  fold_convert_loc (loc, type, tem));
	    }
	  break;
	case GE_EXPR:
	case GT_EXPR:
	case UNGE_EXPR:
	case UNGT_EXPR:
	  if (!HONOR_NANS (TYPE_MODE (TREE_TYPE (arg1))))
	    {
	      comp_op0 = fold_convert_loc (loc, comp_type, comp_op0);
	      comp_op1 = fold_convert_loc (loc, comp_type, comp_op1);
	      tem = (comp_code == GE_EXPR || comp_code == UNGE_EXPR)
		    ? fold_build2_loc (loc, MAX_EXPR, comp_type, comp_op0, comp_op1)
		    : fold_build2_loc (loc, MAX_EXPR, comp_type,
				   comp_op1, comp_op0);
	      return pedantic_non_lvalue_loc (loc,
					  fold_convert_loc (loc, type, tem));
	    }
	  break;
	case UNEQ_EXPR:
	  if (!HONOR_NANS (TYPE_MODE (TREE_TYPE (arg1))))
	    return pedantic_non_lvalue_loc (loc,
					fold_convert_loc (loc, type, arg2));
	  break;
	case LTGT_EXPR:
	  if (!HONOR_NANS (TYPE_MODE (TREE_TYPE (arg1))))
	    return pedantic_non_lvalue_loc (loc,
					fold_convert_loc (loc, type, arg1));
	  break;
	default:
	  gcc_assert (TREE_CODE_CLASS (comp_code) == tcc_comparison);
	  break;
	}
    }

  /* If this is A op C1 ? A : C2 with C1 and C2 constant integers,
     we might still be able to simplify this.  For example,
     if C1 is one less or one more than C2, this might have started
     out as a MIN or MAX and been transformed by this function.
     Only good for INTEGER_TYPEs, because we need TYPE_MAX_VALUE.  */

  if (INTEGRAL_TYPE_P (type)
      && TREE_CODE (arg01) == INTEGER_CST
      && TREE_CODE (arg2) == INTEGER_CST)
    switch (comp_code)
      {
      case EQ_EXPR:
	if (TREE_CODE (arg1) == INTEGER_CST)
	  break;
	/* We can replace A with C1 in this case.  */
	arg1 = fold_convert_loc (loc, type, arg01);
	return fold_build3_loc (loc, COND_EXPR, type, arg0, arg1, arg2);

      case LT_EXPR:
	/* If C1 is C2 + 1, this is min(A, C2), but use ARG00's type for
	   MIN_EXPR, to preserve the signedness of the comparison.  */
	if (! operand_equal_p (arg2, TYPE_MAX_VALUE (type),
			       OEP_ONLY_CONST)
	    && operand_equal_p (arg01,
				const_binop (PLUS_EXPR, arg2,
					     build_int_cst (type, 1)),
				OEP_ONLY_CONST))
	  {
	    tem = fold_build2_loc (loc, MIN_EXPR, TREE_TYPE (arg00), arg00,
				   fold_convert_loc (loc, TREE_TYPE (arg00),
						     arg2));
	    return pedantic_non_lvalue_loc (loc,
					    fold_convert_loc (loc, type, tem));
	  }
	break;

      case LE_EXPR:
	/* If C1 is C2 - 1, this is min(A, C2), with the same care
	   as above.  */
	if (! operand_equal_p (arg2, TYPE_MIN_VALUE (type),
			       OEP_ONLY_CONST)
	    && operand_equal_p (arg01,
				const_binop (MINUS_EXPR, arg2,
					     build_int_cst (type, 1)),
				OEP_ONLY_CONST))
	  {
	    tem = fold_build2_loc (loc, MIN_EXPR, TREE_TYPE (arg00), arg00,
				   fold_convert_loc (loc, TREE_TYPE (arg00),
						     arg2));
	    return pedantic_non_lvalue_loc (loc,
					    fold_convert_loc (loc, type, tem));
	  }
	break;

      case GT_EXPR:
	/* If C1 is C2 - 1, this is max(A, C2), but use ARG00's type for
	   MAX_EXPR, to preserve the signedness of the comparison.  */
	if (! operand_equal_p (arg2, TYPE_MIN_VALUE (type),
			       OEP_ONLY_CONST)
	    && operand_equal_p (arg01,
				const_binop (MINUS_EXPR, arg2,
					     build_int_cst (type, 1)),
				OEP_ONLY_CONST))
	  {
	    tem = fold_build2_loc (loc, MAX_EXPR, TREE_TYPE (arg00), arg00,
				   fold_convert_loc (loc, TREE_TYPE (arg00),
						     arg2));
	    return pedantic_non_lvalue_loc (loc, fold_convert_loc (loc, type, tem));
	  }
	break;

      case GE_EXPR:
	/* If C1 is C2 + 1, this is max(A, C2), with the same care as above.  */
	if (! operand_equal_p (arg2, TYPE_MAX_VALUE (type),
			       OEP_ONLY_CONST)
	    && operand_equal_p (arg01,
				const_binop (PLUS_EXPR, arg2,
					     build_int_cst (type, 1)),
				OEP_ONLY_CONST))
	  {
	    tem = fold_build2_loc (loc, MAX_EXPR, TREE_TYPE (arg00), arg00,
				   fold_convert_loc (loc, TREE_TYPE (arg00),
						     arg2));
	    return pedantic_non_lvalue_loc (loc, fold_convert_loc (loc, type, tem));
	  }
	break;
      case NE_EXPR:
	break;
      default:
	gcc_unreachable ();
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
      && 0 != (tem = (build_range_check (loc, type,
					 lhs != 0 ? lhs
					 : rhs != 0 ? rhs : integer_zero_node,
					 in_p, low, high))))
    {
      if (strict_overflow_p)
	fold_overflow_warning (warnmsg, WARN_STRICT_OVERFLOW_COMPARISON);
      return or_op ? invert_truthvalue_loc (loc, tem) : tem;
    }

  /* On machines where the branch cost is expensive, if this is a
     short-circuited branch and the underlying object on both sides
     is the same, make a non-short-circuit operation.  */
  else if (LOGICAL_OP_NON_SHORT_CIRCUIT
	   && lhs != 0 && rhs != 0
	   && (code == TRUTH_ANDIF_EXPR
	       || code == TRUTH_ORIF_EXPR)
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

	  if (0 != (lhs = build_range_check (loc, type, common,
					     or_op ? ! in0_p : in0_p,
					     low0, high0))
	      && (0 != (rhs = build_range_check (loc, type, common,
						 or_op ? ! in1_p : in1_p,
						 low1, high1))))
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
  int modesize = GET_MODE_BITSIZE (TYPE_MODE (type));
  tree temp;

  if (p == modesize || unsignedp)
    return c;

  /* We work by getting just the sign bit into the low-order bit, then
     into the high-order bit, then sign-extend.  We then XOR that value
     with C.  */
  temp = const_binop (RSHIFT_EXPR, c, size_int (p - 1));
  temp = const_binop (BIT_AND_EXPR, temp, size_int (1));

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
  tree type = TREE_TYPE (cmpop);
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

  inv_code = invert_tree_comparison (code, HONOR_NANS (TYPE_MODE (type)));
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
  enum machine_mode ll_mode, lr_mode, rl_mode, rr_mode;
  enum machine_mode lnmode, rnmode;
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

  volatilep = 0;
  ll_inner = decode_field_reference (loc, ll_arg,
				     &ll_bitsize, &ll_bitpos, &ll_mode,
				     &ll_unsignedp, &volatilep, &ll_mask,
				     &ll_and_mask);
  lr_inner = decode_field_reference (loc, lr_arg,
				     &lr_bitsize, &lr_bitpos, &lr_mode,
				     &lr_unsignedp, &volatilep, &lr_mask,
				     &lr_and_mask);
  rl_inner = decode_field_reference (loc, rl_arg,
				     &rl_bitsize, &rl_bitpos, &rl_mode,
				     &rl_unsignedp, &volatilep, &rl_mask,
				     &rl_and_mask);
  rr_inner = decode_field_reference (loc, rr_arg,
				     &rr_bitsize, &rr_bitpos, &rr_mode,
				     &rr_unsignedp, &volatilep, &rr_mask,
				     &rr_and_mask);

  /* It must be true that the inner operation on the lhs of each
     comparison must be the same if we are to be able to do anything.
     Then see if we have constants.  If not, the same must be true for
     the rhs's.  */
  if (volatilep || ll_inner == 0 || rl_inner == 0
      || ! operand_equal_p (ll_inner, rl_inner, 0))
    return 0;

  if (TREE_CODE (lr_arg) == INTEGER_CST
      && TREE_CODE (rr_arg) == INTEGER_CST)
    l_const = lr_arg, r_const = rr_arg;
  else if (lr_inner == 0 || rr_inner == 0
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
  lnmode = get_best_mode (end_bit - first_bit, first_bit, 0, 0,
			  TYPE_ALIGN (TREE_TYPE (ll_inner)), word_mode,
			  volatilep);
  if (lnmode == VOIDmode)
    return 0;

  lnbitsize = GET_MODE_BITSIZE (lnmode);
  lnbitpos = first_bit & ~ (lnbitsize - 1);
  lntype = lang_hooks.types.type_for_size (lnbitsize, 1);
  xll_bitpos = ll_bitpos - lnbitpos, xrl_bitpos = rl_bitpos - lnbitpos;

  if (BYTES_BIG_ENDIAN)
    {
      xll_bitpos = lnbitsize - xll_bitpos - ll_bitsize;
      xrl_bitpos = lnbitsize - xrl_bitpos - rl_bitsize;
    }

  ll_mask = const_binop (LSHIFT_EXPR, fold_convert_loc (loc, lntype, ll_mask),
			 size_int (xll_bitpos));
  rl_mask = const_binop (LSHIFT_EXPR, fold_convert_loc (loc, lntype, rl_mask),
			 size_int (xrl_bitpos));

  if (l_const)
    {
      l_const = fold_convert_loc (loc, lntype, l_const);
      l_const = unextend (l_const, ll_bitsize, ll_unsignedp, ll_and_mask);
      l_const = const_binop (LSHIFT_EXPR, l_const, size_int (xll_bitpos));
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
      if (! integer_zerop (const_binop (BIT_AND_EXPR, r_const,
					fold_build1_loc (loc, BIT_NOT_EXPR,
						     lntype, rl_mask))))
	{
	  warning (0, "comparison is always %d", wanted_code == NE_EXPR);

	  return constant_boolean_node (wanted_code == NE_EXPR, truth_type);
	}
    }

  /* If the right sides are not constant, do the same for it.  Also,
     disallow this optimization if a size or signedness mismatch occurs
     between the left and right sides.  */
  if (l_const == 0)
    {
      if (ll_bitsize != lr_bitsize || rl_bitsize != rr_bitsize
	  || ll_unsignedp != lr_unsignedp || rl_unsignedp != rr_unsignedp
	  /* Make sure the two fields on the right
	     correspond to the left without being swapped.  */
	  || ll_bitpos - rl_bitpos != lr_bitpos - rr_bitpos)
	return 0;

      first_bit = MIN (lr_bitpos, rr_bitpos);
      end_bit = MAX (lr_bitpos + lr_bitsize, rr_bitpos + rr_bitsize);
      rnmode = get_best_mode (end_bit - first_bit, first_bit, 0, 0,
			      TYPE_ALIGN (TREE_TYPE (lr_inner)), word_mode,
			      volatilep);
      if (rnmode == VOIDmode)
	return 0;

      rnbitsize = GET_MODE_BITSIZE (rnmode);
      rnbitpos = first_bit & ~ (rnbitsize - 1);
      rntype = lang_hooks.types.type_for_size (rnbitsize, 1);
      xlr_bitpos = lr_bitpos - rnbitpos, xrr_bitpos = rr_bitpos - rnbitpos;

      if (BYTES_BIG_ENDIAN)
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

      /* Make a mask that corresponds to both fields being compared.
	 Do this for both items being compared.  If the operands are the
	 same size and the bits being compared are in the same position
	 then we can do this by masking both and comparing the masked
	 results.  */
      ll_mask = const_binop (BIT_IOR_EXPR, ll_mask, rl_mask);
      lr_mask = const_binop (BIT_IOR_EXPR, lr_mask, rr_mask);
      if (lnbitsize == rnbitsize && xll_bitpos == xlr_bitpos)
	{
	  lhs = make_bit_field_ref (loc, ll_inner, lntype, lnbitsize, lnbitpos,
				    ll_unsignedp || rl_unsignedp);
	  if (! all_ones_mask_p (ll_mask, lnbitsize))
	    lhs = build2 (BIT_AND_EXPR, lntype, lhs, ll_mask);

	  rhs = make_bit_field_ref (loc, lr_inner, rntype, rnbitsize, rnbitpos,
				    lr_unsignedp || rr_unsignedp);
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
      if ((ll_bitsize + ll_bitpos == rl_bitpos
	   && lr_bitsize + lr_bitpos == rr_bitpos)
	  || (ll_bitpos == rl_bitpos + rl_bitsize
	      && lr_bitpos == rr_bitpos + rr_bitsize))
	{
	  tree type;

	  lhs = make_bit_field_ref (loc, ll_inner, lntype,
				    ll_bitsize + rl_bitsize,
				    MIN (ll_bitpos, rl_bitpos), ll_unsignedp);
	  rhs = make_bit_field_ref (loc, lr_inner, rntype,
				    lr_bitsize + rr_bitsize,
				    MIN (lr_bitpos, rr_bitpos), lr_unsignedp);

	  ll_mask = const_binop (RSHIFT_EXPR, ll_mask,
				 size_int (MIN (xll_bitpos, xrl_bitpos)));
	  lr_mask = const_binop (RSHIFT_EXPR, lr_mask,
				 size_int (MIN (xlr_bitpos, xrr_bitpos)));

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

  /* Construct the expression we will return.  First get the component
     reference we will make.  Unless the mask is all ones the width of
     that field, perform the mask operation.  Then compare with the
     merged constant.  */
  result = make_bit_field_ref (loc, ll_inner, lntype, lnbitsize, lnbitpos,
			       ll_unsignedp || rl_unsignedp);

  ll_mask = const_binop (BIT_IOR_EXPR, ll_mask, rl_mask);
  if (! all_ones_mask_p (ll_mask, lnbitsize))
    result = build2_loc (loc, BIT_AND_EXPR, lntype, result, ll_mask);

  return build2_loc (loc, wanted_code, truth_type, result,
		     const_binop (BIT_IOR_EXPR, l_const, r_const));
}

/* Optimize T, which is a comparison of a MIN_EXPR or MAX_EXPR with a
   constant.  */

static tree
optimize_minmax_comparison (location_t loc, enum tree_code code, tree type,
			    tree op0, tree op1)
{
  tree arg0 = op0;
  enum tree_code op_code;
  tree comp_const;
  tree minmax_const;
  int consts_equal, consts_lt;
  tree inner;

  STRIP_SIGN_NOPS (arg0);

  op_code = TREE_CODE (arg0);
  minmax_const = TREE_OPERAND (arg0, 1);
  comp_const = fold_convert_loc (loc, TREE_TYPE (arg0), op1);
  consts_equal = tree_int_cst_equal (minmax_const, comp_const);
  consts_lt = tree_int_cst_lt (minmax_const, comp_const);
  inner = TREE_OPERAND (arg0, 0);

  /* If something does not permit us to optimize, return the original tree.  */
  if ((op_code != MIN_EXPR && op_code != MAX_EXPR)
      || TREE_CODE (comp_const) != INTEGER_CST
      || TREE_OVERFLOW (comp_const)
      || TREE_CODE (minmax_const) != INTEGER_CST
      || TREE_OVERFLOW (minmax_const))
    return NULL_TREE;

  /* Now handle all the various comparison codes.  We only handle EQ_EXPR
     and GT_EXPR, doing the rest with recursive calls using logical
     simplifications.  */
  switch (code)
    {
    case NE_EXPR:  case LT_EXPR:  case LE_EXPR:
      {
	tree tem
	  = optimize_minmax_comparison (loc,
					invert_tree_comparison (code, false),
					type, op0, op1);
	if (tem)
	  return invert_truthvalue_loc (loc, tem);
	return NULL_TREE;
      }

    case GE_EXPR:
      return
	fold_build2_loc (loc, TRUTH_ORIF_EXPR, type,
		     optimize_minmax_comparison
		     (loc, EQ_EXPR, type, arg0, comp_const),
		     optimize_minmax_comparison
		     (loc, GT_EXPR, type, arg0, comp_const));

    case EQ_EXPR:
      if (op_code == MAX_EXPR && consts_equal)
	/* MAX (X, 0) == 0  ->  X <= 0  */
	return fold_build2_loc (loc, LE_EXPR, type, inner, comp_const);

      else if (op_code == MAX_EXPR && consts_lt)
	/* MAX (X, 0) == 5  ->  X == 5   */
	return fold_build2_loc (loc, EQ_EXPR, type, inner, comp_const);

      else if (op_code == MAX_EXPR)
	/* MAX (X, 0) == -1  ->  false  */
	return omit_one_operand_loc (loc, type, integer_zero_node, inner);

      else if (consts_equal)
	/* MIN (X, 0) == 0  ->  X >= 0  */
	return fold_build2_loc (loc, GE_EXPR, type, inner, comp_const);

      else if (consts_lt)
	/* MIN (X, 0) == 5  ->  false  */
	return omit_one_operand_loc (loc, type, integer_zero_node, inner);

      else
	/* MIN (X, 0) == -1  ->  X == -1  */
	return fold_build2_loc (loc, EQ_EXPR, type, inner, comp_const);

    case GT_EXPR:
      if (op_code == MAX_EXPR && (consts_equal || consts_lt))
	/* MAX (X, 0) > 0  ->  X > 0
	   MAX (X, 0) > 5  ->  X > 5  */
	return fold_build2_loc (loc, GT_EXPR, type, inner, comp_const);

      else if (op_code == MAX_EXPR)
	/* MAX (X, 0) > -1  ->  true  */
	return omit_one_operand_loc (loc, type, integer_one_node, inner);

      else if (op_code == MIN_EXPR && (consts_equal || consts_lt))
	/* MIN (X, 0) > 0  ->  false
	   MIN (X, 0) > 5  ->  false  */
	return omit_one_operand_loc (loc, type, integer_zero_node, inner);

      else
	/* MIN (X, 0) > -1  ->  X > -1  */
	return fold_build2_loc (loc, GT_EXPR, type, inner, comp_const);

    default:
      return NULL_TREE;
    }
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
  tree ctype = (wide_type != 0 && (GET_MODE_SIZE (TYPE_MODE (wide_type))
				   > GET_MODE_SIZE (TYPE_MODE (type)))
		? wide_type : type);
  tree t1, t2;
  int same_p = tcode == code;
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
	  || integer_zerop (const_binop (TRUNC_MOD_EXPR, t, c)))
	return const_binop (code, fold_convert (ctype, t),
			    fold_convert (ctype, c));
      break;

    CASE_CONVERT: case NON_LVALUE_EXPR:
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
	  && (0 != (t1 = extract_muldiv (op0, t2, code,
					 code == MULT_EXPR
					 ? ctype : NULL_TREE,
					 strict_overflow_p))))
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
	  && TYPE_PRECISION (TREE_TYPE (size_one_node)) > TREE_INT_CST_LOW (op1)
	  && TREE_INT_CST_HIGH (op1) == 0
	  && 0 != (t1 = fold_convert (ctype,
				      const_binop (LSHIFT_EXPR,
						   size_one_node,
						   op1)))
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
	      tree tem = op0;
	      op0 = op1;
	      op1 = tem;
	      tem = t1;
	      t1 = t2;
	      t2 = tem;
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
	  || integer_zerop (const_binop (TRUNC_MOD_EXPR, op1, c)))
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

      /* If we were able to eliminate our operation from the first side,
	 apply our operation to the second side and reform the PLUS.  */
      if (t1 != 0 && (TREE_CODE (t1) != code || code == MULT_EXPR))
	return fold_build2 (tcode, ctype, fold_convert (ctype, t1), op1);

      /* The last case is if we are a multiply.  In that case, we can
	 apply the distributive law to commute the multiply and addition
	 if the multiplication of the constants doesn't overflow
	 and overflow is defined.  With undefined overflow
	 op0 * c might overflow, while (op0 + orig_op1) * c doesn't.  */
      if (code == MULT_EXPR && TYPE_OVERFLOW_WRAPS (ctype))
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
	  && integer_zerop (const_binop (TRUNC_MOD_EXPR, op1, c)))
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
	  && (t1 = extract_muldiv (op0, c, code, wide_type,
				   strict_overflow_p)) != 0)
	return fold_build2 (tcode, ctype, fold_convert (ctype, t1),
			    fold_convert (ctype, op1));
      else if (tcode == MULT_EXPR && code == MULT_EXPR
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
	  double_int mul;
	  bool overflow_p;
	  unsigned prec = TYPE_PRECISION (ctype);
	  bool uns = TYPE_UNSIGNED (ctype);
	  double_int diop1 = tree_to_double_int (op1).ext (prec, uns);
	  double_int dic = tree_to_double_int (c).ext (prec, uns);
	  mul = diop1.mul_with_sign (dic, false, &overflow_p);
	  overflow_p = ((!uns && overflow_p)
			| TREE_OVERFLOW (c) | TREE_OVERFLOW (op1));
	  if (!double_int_fits_to_tree_p (ctype, mul)
	      && ((uns && tcode != MULT_EXPR) || !uns))
	    overflow_p = 1;
	  if (!overflow_p)
	    return fold_build2 (tcode, ctype, fold_convert (ctype, op0),
				double_int_to_tree (ctype, mul));
	}

      /* If these operations "cancel" each other, we have the main
	 optimizations of this pass, which occur when either constant is a
	 multiple of the other, in which case we replace this with either an
	 operation or CODE or TCODE.

	 If we have an unsigned type, we cannot do this since it will change
	 the result if the original computation overflowed.  */
      if (TYPE_OVERFLOW_UNDEFINED (ctype)
	  && ((code == MULT_EXPR && tcode == EXACT_DIV_EXPR)
	      || (tcode == MULT_EXPR
		  && code != TRUNC_MOD_EXPR && code != CEIL_MOD_EXPR
		  && code != FLOOR_MOD_EXPR && code != ROUND_MOD_EXPR
		  && code != MULT_EXPR)))
	{
	  if (integer_zerop (const_binop (TRUNC_MOD_EXPR, op1, c)))
	    {
	      if (TYPE_OVERFLOW_UNDEFINED (ctype))
		*strict_overflow_p = true;
	      return fold_build2 (tcode, ctype, fold_convert (ctype, op0),
				  fold_convert (ctype,
						const_binop (TRUNC_DIV_EXPR,
							     op1, c)));
	    }
	  else if (integer_zerop (const_binop (TRUNC_MOD_EXPR, c, op1)))
	    {
	      if (TYPE_OVERFLOW_UNDEFINED (ctype))
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
  else if (TREE_CODE (type) == VECTOR_TYPE)
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
  else
    {
      tree testtype = TREE_TYPE (cond);
      test = cond;
      true_value = constant_boolean_node (true, testtype);
      false_value = constant_boolean_node (false, testtype);
    }

  if (TREE_CODE (TREE_TYPE (test)) == VECTOR_TYPE)
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


/* Subroutine of fold() that checks for the addition of +/- 0.0.

   If !NEGATE, return true if ADDEND is +/-0.0 and, for all X of type
   TYPE, X + ADDEND is the same as X.  If NEGATE, return true if X -
   ADDEND is the same as X.

   X + 0 and X - 0 both give X when X is NaN, infinite, or nonzero
   and finite.  The problematic cases are when X is zero, and its mode
   has signed zeros.  In the case of rounding towards -infinity,
   X - 0 is not the same as X because 0 - 0 is -0.  In other rounding
   modes, X + 0 is not the same as X because -0 + 0 is 0.  */

bool
fold_real_zero_addition_p (const_tree type, const_tree addend, int negate)
{
  if (!real_zerop (addend))
    return false;

  /* Don't allow the fold with -fsignaling-nans.  */
  if (HONOR_SNANS (TYPE_MODE (type)))
    return false;

  /* Allow the fold if zeros aren't signed, or their sign isn't important.  */
  if (!HONOR_SIGNED_ZEROS (TYPE_MODE (type)))
    return true;

  /* In a vector or complex, we would need to check the sign of all zeros.  */
  if (TREE_CODE (addend) != REAL_CST)
    return false;

  /* Treat x + -0 as x - 0 and x - -0 as x + 0.  */
  if (REAL_VALUE_MINUS_ZERO (TREE_REAL_CST (addend)))
    negate = !negate;

  /* The mode has signed zeros, and we have to honor their sign.
     In this situation, there is only one case we can return true for.
     X - 0 is the same as X unless rounding towards -infinity is
     supported.  */
  return negate && !HONOR_SIGN_DEPENDENT_ROUNDING (TYPE_MODE (type));
}

/* Subroutine of fold() that checks comparisons of built-in math
   functions against real constants.

   FCODE is the DECL_FUNCTION_CODE of the built-in, CODE is the comparison
   operator: EQ_EXPR, NE_EXPR, GT_EXPR, LT_EXPR, GE_EXPR or LE_EXPR.  TYPE
   is the type of the result and ARG0 and ARG1 are the operands of the
   comparison.  ARG1 must be a TREE_REAL_CST.

   The function returns the constant folded tree if a simplification
   can be made, and NULL_TREE otherwise.  */

static tree
fold_mathfn_compare (location_t loc,
		     enum built_in_function fcode, enum tree_code code,
		     tree type, tree arg0, tree arg1)
{
  REAL_VALUE_TYPE c;

  if (BUILTIN_SQRT_P (fcode))
    {
      tree arg = CALL_EXPR_ARG (arg0, 0);
      enum machine_mode mode = TYPE_MODE (TREE_TYPE (arg0));

      c = TREE_REAL_CST (arg1);
      if (REAL_VALUE_NEGATIVE (c))
	{
	  /* sqrt(x) < y is always false, if y is negative.  */
	  if (code == EQ_EXPR || code == LT_EXPR || code == LE_EXPR)
	    return omit_one_operand_loc (loc, type, integer_zero_node, arg);

	  /* sqrt(x) > y is always true, if y is negative and we
	     don't care about NaNs, i.e. negative values of x.  */
	  if (code == NE_EXPR || !HONOR_NANS (mode))
	    return omit_one_operand_loc (loc, type, integer_one_node, arg);

	  /* sqrt(x) > y is the same as x >= 0, if y is negative.  */
	  return fold_build2_loc (loc, GE_EXPR, type, arg,
			      build_real (TREE_TYPE (arg), dconst0));
	}
      else if (code == GT_EXPR || code == GE_EXPR)
	{
	  REAL_VALUE_TYPE c2;

	  REAL_ARITHMETIC (c2, MULT_EXPR, c, c);
	  real_convert (&c2, mode, &c2);

	  if (REAL_VALUE_ISINF (c2))
	    {
	      /* sqrt(x) > y is x == +Inf, when y is very large.  */
	      if (HONOR_INFINITIES (mode))
		return fold_build2_loc (loc, EQ_EXPR, type, arg,
				    build_real (TREE_TYPE (arg), c2));

	      /* sqrt(x) > y is always false, when y is very large
		 and we don't care about infinities.  */
	      return omit_one_operand_loc (loc, type, integer_zero_node, arg);
	    }

	  /* sqrt(x) > c is the same as x > c*c.  */
	  return fold_build2_loc (loc, code, type, arg,
			      build_real (TREE_TYPE (arg), c2));
	}
      else if (code == LT_EXPR || code == LE_EXPR)
	{
	  REAL_VALUE_TYPE c2;

	  REAL_ARITHMETIC (c2, MULT_EXPR, c, c);
	  real_convert (&c2, mode, &c2);

	  if (REAL_VALUE_ISINF (c2))
	    {
	      /* sqrt(x) < y is always true, when y is a very large
		 value and we don't care about NaNs or Infinities.  */
	      if (! HONOR_NANS (mode) && ! HONOR_INFINITIES (mode))
		return omit_one_operand_loc (loc, type, integer_one_node, arg);

	      /* sqrt(x) < y is x != +Inf when y is very large and we
		 don't care about NaNs.  */
	      if (! HONOR_NANS (mode))
		return fold_build2_loc (loc, NE_EXPR, type, arg,
				    build_real (TREE_TYPE (arg), c2));

	      /* sqrt(x) < y is x >= 0 when y is very large and we
		 don't care about Infinities.  */
	      if (! HONOR_INFINITIES (mode))
		return fold_build2_loc (loc, GE_EXPR, type, arg,
				    build_real (TREE_TYPE (arg), dconst0));

	      /* sqrt(x) < y is x >= 0 && x != +Inf, when y is large.  */
	      arg = save_expr (arg);
	      return fold_build2_loc (loc, TRUTH_ANDIF_EXPR, type,
				  fold_build2_loc (loc, GE_EXPR, type, arg,
					       build_real (TREE_TYPE (arg),
							   dconst0)),
				  fold_build2_loc (loc, NE_EXPR, type, arg,
					       build_real (TREE_TYPE (arg),
							   c2)));
	    }

	  /* sqrt(x) < c is the same as x < c*c, if we ignore NaNs.  */
	  if (! HONOR_NANS (mode))
	    return fold_build2_loc (loc, code, type, arg,
				build_real (TREE_TYPE (arg), c2));

	  /* sqrt(x) < c is the same as x >= 0 && x < c*c.  */
	  arg = save_expr (arg);
	  return fold_build2_loc (loc, TRUTH_ANDIF_EXPR, type,
				  fold_build2_loc (loc, GE_EXPR, type, arg,
					       build_real (TREE_TYPE (arg),
							   dconst0)),
				  fold_build2_loc (loc, code, type, arg,
					       build_real (TREE_TYPE (arg),
							   c2)));
	}
    }

  return NULL_TREE;
}

/* Subroutine of fold() that optimizes comparisons against Infinities,
   either +Inf or -Inf.

   CODE is the comparison operator: EQ_EXPR, NE_EXPR, GT_EXPR, LT_EXPR,
   GE_EXPR or LE_EXPR.  TYPE is the type of the result and ARG0 and ARG1
   are the operands of the comparison.  ARG1 must be a TREE_REAL_CST.

   The function returns the constant folded tree if a simplification
   can be made, and NULL_TREE otherwise.  */

static tree
fold_inf_compare (location_t loc, enum tree_code code, tree type,
		  tree arg0, tree arg1)
{
  enum machine_mode mode;
  REAL_VALUE_TYPE max;
  tree temp;
  bool neg;

  mode = TYPE_MODE (TREE_TYPE (arg0));

  /* For negative infinity swap the sense of the comparison.  */
  neg = REAL_VALUE_NEGATIVE (TREE_REAL_CST (arg1));
  if (neg)
    code = swap_tree_comparison (code);

  switch (code)
    {
    case GT_EXPR:
      /* x > +Inf is always false, if with ignore sNANs.  */
      if (HONOR_SNANS (mode))
        return NULL_TREE;
      return omit_one_operand_loc (loc, type, integer_zero_node, arg0);

    case LE_EXPR:
      /* x <= +Inf is always true, if we don't case about NaNs.  */
      if (! HONOR_NANS (mode))
	return omit_one_operand_loc (loc, type, integer_one_node, arg0);

      /* x <= +Inf is the same as x == x, i.e. isfinite(x).  */
      arg0 = save_expr (arg0);
      return fold_build2_loc (loc, EQ_EXPR, type, arg0, arg0);

    case EQ_EXPR:
    case GE_EXPR:
      /* x == +Inf and x >= +Inf are always equal to x > DBL_MAX.  */
      real_maxval (&max, neg, mode);
      return fold_build2_loc (loc, neg ? LT_EXPR : GT_EXPR, type,
			  arg0, build_real (TREE_TYPE (arg0), max));

    case LT_EXPR:
      /* x < +Inf is always equal to x <= DBL_MAX.  */
      real_maxval (&max, neg, mode);
      return fold_build2_loc (loc, neg ? GE_EXPR : LE_EXPR, type,
			  arg0, build_real (TREE_TYPE (arg0), max));

    case NE_EXPR:
      /* x != +Inf is always equal to !(x > DBL_MAX).  */
      real_maxval (&max, neg, mode);
      if (! HONOR_NANS (mode))
	return fold_build2_loc (loc, neg ? GE_EXPR : LE_EXPR, type,
			    arg0, build_real (TREE_TYPE (arg0), max));

      temp = fold_build2_loc (loc, neg ? LT_EXPR : GT_EXPR, type,
			  arg0, build_real (TREE_TYPE (arg0), max));
      return fold_build1_loc (loc, TRUTH_NOT_EXPR, type, temp);

    default:
      break;
    }

  return NULL_TREE;
}

/* Subroutine of fold() that optimizes comparisons of a division by
   a nonzero integer constant against an integer constant, i.e.
   X/C1 op C2.

   CODE is the comparison operator: EQ_EXPR, NE_EXPR, GT_EXPR, LT_EXPR,
   GE_EXPR or LE_EXPR.  TYPE is the type of the result and ARG0 and ARG1
   are the operands of the comparison.  ARG1 must be a TREE_REAL_CST.

   The function returns the constant folded tree if a simplification
   can be made, and NULL_TREE otherwise.  */

static tree
fold_div_compare (location_t loc,
		  enum tree_code code, tree type, tree arg0, tree arg1)
{
  tree prod, tmp, hi, lo;
  tree arg00 = TREE_OPERAND (arg0, 0);
  tree arg01 = TREE_OPERAND (arg0, 1);
  double_int val;
  bool unsigned_p = TYPE_UNSIGNED (TREE_TYPE (arg0));
  bool neg_overflow;
  bool overflow;

  /* We have to do this the hard way to detect unsigned overflow.
     prod = int_const_binop (MULT_EXPR, arg01, arg1);  */
  val = TREE_INT_CST (arg01)
	.mul_with_sign (TREE_INT_CST (arg1), unsigned_p, &overflow);
  prod = force_fit_type_double (TREE_TYPE (arg00), val, -1, overflow);
  neg_overflow = false;

  if (unsigned_p)
    {
      tmp = int_const_binop (MINUS_EXPR, arg01,
                             build_int_cst (TREE_TYPE (arg01), 1));
      lo = prod;

      /* Likewise hi = int_const_binop (PLUS_EXPR, prod, tmp).  */
      val = TREE_INT_CST (prod)
	    .add_with_sign (TREE_INT_CST (tmp), unsigned_p, &overflow);
      hi = force_fit_type_double (TREE_TYPE (arg00), val,
				  -1, overflow | TREE_OVERFLOW (prod));
    }
  else if (tree_int_cst_sgn (arg01) >= 0)
    {
      tmp = int_const_binop (MINUS_EXPR, arg01,
			     build_int_cst (TREE_TYPE (arg01), 1));
      switch (tree_int_cst_sgn (arg1))
	{
	case -1:
	  neg_overflow = true;
	  lo = int_const_binop (MINUS_EXPR, prod, tmp);
	  hi = prod;
	  break;

	case  0:
	  lo = fold_negate_const (tmp, TREE_TYPE (arg0));
	  hi = tmp;
	  break;

	case  1:
          hi = int_const_binop (PLUS_EXPR, prod, tmp);
	  lo = prod;
	  break;

	default:
	  gcc_unreachable ();
	}
    }
  else
    {
      /* A negative divisor reverses the relational operators.  */
      code = swap_tree_comparison (code);

      tmp = int_const_binop (PLUS_EXPR, arg01,
			     build_int_cst (TREE_TYPE (arg01), 1));
      switch (tree_int_cst_sgn (arg1))
	{
	case -1:
	  hi = int_const_binop (MINUS_EXPR, prod, tmp);
	  lo = prod;
	  break;

	case  0:
	  hi = fold_negate_const (tmp, TREE_TYPE (arg0));
	  lo = tmp;
	  break;

	case  1:
	  neg_overflow = true;
	  lo = int_const_binop (PLUS_EXPR, prod, tmp);
	  hi = prod;
	  break;

	default:
	  gcc_unreachable ();
	}
    }

  switch (code)
    {
    case EQ_EXPR:
      if (TREE_OVERFLOW (lo) && TREE_OVERFLOW (hi))
	return omit_one_operand_loc (loc, type, integer_zero_node, arg00);
      if (TREE_OVERFLOW (hi))
	return fold_build2_loc (loc, GE_EXPR, type, arg00, lo);
      if (TREE_OVERFLOW (lo))
	return fold_build2_loc (loc, LE_EXPR, type, arg00, hi);
      return build_range_check (loc, type, arg00, 1, lo, hi);

    case NE_EXPR:
      if (TREE_OVERFLOW (lo) && TREE_OVERFLOW (hi))
	return omit_one_operand_loc (loc, type, integer_one_node, arg00);
      if (TREE_OVERFLOW (hi))
	return fold_build2_loc (loc, LT_EXPR, type, arg00, lo);
      if (TREE_OVERFLOW (lo))
	return fold_build2_loc (loc, GT_EXPR, type, arg00, hi);
      return build_range_check (loc, type, arg00, 0, lo, hi);

    case LT_EXPR:
      if (TREE_OVERFLOW (lo))
	{
	  tmp = neg_overflow ? integer_zero_node : integer_one_node;
	  return omit_one_operand_loc (loc, type, tmp, arg00);
	}
      return fold_build2_loc (loc, LT_EXPR, type, arg00, lo);

    case LE_EXPR:
      if (TREE_OVERFLOW (hi))
	{
	  tmp = neg_overflow ? integer_zero_node : integer_one_node;
	  return omit_one_operand_loc (loc, type, tmp, arg00);
	}
      return fold_build2_loc (loc, LE_EXPR, type, arg00, hi);

    case GT_EXPR:
      if (TREE_OVERFLOW (hi))
	{
	  tmp = neg_overflow ? integer_one_node : integer_zero_node;
	  return omit_one_operand_loc (loc, type, tmp, arg00);
	}
      return fold_build2_loc (loc, GT_EXPR, type, arg00, hi);

    case GE_EXPR:
      if (TREE_OVERFLOW (lo))
	{
	  tmp = neg_overflow ? integer_one_node : integer_zero_node;
	  return omit_one_operand_loc (loc, type, tmp, arg00);
	}
      return fold_build2_loc (loc, GE_EXPR, type, arg00, lo);

    default:
      break;
    }

  return NULL_TREE;
}


/* If CODE with arguments ARG0 and ARG1 represents a single bit
   equality/inequality test, then return a simplified form of the test
   using a sign testing.  Otherwise return NULL.  TYPE is the desired
   result type.  */

static tree
fold_single_bit_test_into_sign_test (location_t loc,
				     enum tree_code code, tree arg0, tree arg1,
				     tree result_type)
{
  /* If this is testing a single bit, we can optimize the test.  */
  if ((code == NE_EXPR || code == EQ_EXPR)
      && TREE_CODE (arg0) == BIT_AND_EXPR && integer_zerop (arg1)
      && integer_pow2p (TREE_OPERAND (arg0, 1)))
    {
      /* If we have (A & C) != 0 where C is the sign bit of A, convert
	 this into A < 0.  Similarly for (A & C) == 0 into A >= 0.  */
      tree arg00 = sign_bit_p (TREE_OPERAND (arg0, 0), TREE_OPERAND (arg0, 1));

      if (arg00 != NULL_TREE
	  /* This is only a win if casting to a signed type is cheap,
	     i.e. when arg00's type is not a partial mode.  */
	  && TYPE_PRECISION (TREE_TYPE (arg00))
	     == GET_MODE_BITSIZE (TYPE_MODE (TREE_TYPE (arg00))))
	{
	  tree stype = signed_type_for (TREE_TYPE (arg00));
	  return fold_build2_loc (loc, code == EQ_EXPR ? GE_EXPR : LT_EXPR,
			      result_type,
			      fold_convert_loc (loc, stype, arg00),
			      build_int_cst (stype, 0));
	}
    }

  return NULL_TREE;
}

/* If CODE with arguments ARG0 and ARG1 represents a single bit
   equality/inequality test, then return a simplified form of
   the test using shifts and logical operations.  Otherwise return
   NULL.  TYPE is the desired result type.  */

tree
fold_single_bit_test (location_t loc, enum tree_code code,
		      tree arg0, tree arg1, tree result_type)
{
  /* If this is testing a single bit, we can optimize the test.  */
  if ((code == NE_EXPR || code == EQ_EXPR)
      && TREE_CODE (arg0) == BIT_AND_EXPR && integer_zerop (arg1)
      && integer_pow2p (TREE_OPERAND (arg0, 1)))
    {
      tree inner = TREE_OPERAND (arg0, 0);
      tree type = TREE_TYPE (arg0);
      int bitnum = tree_log2 (TREE_OPERAND (arg0, 1));
      enum machine_mode operand_mode = TYPE_MODE (type);
      int ops_unsigned;
      tree signed_type, unsigned_type, intermediate_type;
      tree tem, one;

      /* First, see if we can fold the single bit test into a sign-bit
	 test.  */
      tem = fold_single_bit_test_into_sign_test (loc, code, arg0, arg1,
						 result_type);
      if (tem)
	return tem;

      /* Otherwise we have (A & C) != 0 where C is a single bit,
	 convert that into ((A >> C2) & 1).  Where C2 = log2(C).
	 Similarly for (A & C) == 0.  */

      /* If INNER is a right shift of a constant and it plus BITNUM does
	 not overflow, adjust BITNUM and INNER.  */
      if (TREE_CODE (inner) == RSHIFT_EXPR
	  && TREE_CODE (TREE_OPERAND (inner, 1)) == INTEGER_CST
	  && host_integerp (TREE_OPERAND (inner, 1), 1)
	  && bitnum < TYPE_PRECISION (type)
	  && (TREE_INT_CST_LOW (TREE_OPERAND (inner, 1))
	      < (unsigned) (TYPE_PRECISION (type) - bitnum)))
	{
	  bitnum += TREE_INT_CST_LOW (TREE_OPERAND (inner, 1));
	  inner = TREE_OPERAND (inner, 0);
	}

      /* If we are going to be able to omit the AND below, we must do our
	 operations as unsigned.  If we must use the AND, we have a choice.
	 Normally unsigned is faster, but for some machines signed is.  */
#ifdef LOAD_EXTEND_OP
      ops_unsigned = (LOAD_EXTEND_OP (operand_mode) == SIGN_EXTEND
		      && !flag_syntax_only) ? 0 : 1;
#else
      ops_unsigned = 1;
#endif

      signed_type = lang_hooks.types.type_for_mode (operand_mode, 0);
      unsigned_type = lang_hooks.types.type_for_mode (operand_mode, 1);
      intermediate_type = ops_unsigned ? unsigned_type : signed_type;
      inner = fold_convert_loc (loc, intermediate_type, inner);

      if (bitnum != 0)
	inner = build2 (RSHIFT_EXPR, intermediate_type,
			inner, size_int (bitnum));

      one = build_int_cst (intermediate_type, 1);

      if (code == EQ_EXPR)
	inner = fold_build2_loc (loc, BIT_XOR_EXPR, intermediate_type, inner, one);

      /* Put the AND last so it can combine with more things.  */
      inner = build2 (BIT_AND_EXPR, intermediate_type, inner, one);

      /* Make sure to return the proper type.  */
      inner = fold_convert_loc (loc, result_type, inner);

      return inner;
    }
  return NULL_TREE;
}

/* Check whether we are allowed to reorder operands arg0 and arg1,
   such that the evaluation of arg1 occurs before arg0.  */

static bool
reorder_operands_p (const_tree arg0, const_tree arg1)
{
  if (! flag_evaluation_order)
      return true;
  if (TREE_CONSTANT (arg0) || TREE_CONSTANT (arg1))
    return true;
  return ! TREE_SIDE_EFFECTS (arg0)
	 && ! TREE_SIDE_EFFECTS (arg1);
}

/* Test whether it is preferable two swap two operands, ARG0 and
   ARG1, for example because ARG0 is an integer constant and ARG1
   isn't.  If REORDER is true, only recommend swapping if we can
   evaluate the operands in reverse order.  */

bool
tree_swap_operands_p (const_tree arg0, const_tree arg1, bool reorder)
{
  STRIP_SIGN_NOPS (arg0);
  STRIP_SIGN_NOPS (arg1);

  if (TREE_CODE (arg1) == INTEGER_CST)
    return 0;
  if (TREE_CODE (arg0) == INTEGER_CST)
    return 1;

  if (TREE_CODE (arg1) == REAL_CST)
    return 0;
  if (TREE_CODE (arg0) == REAL_CST)
    return 1;

  if (TREE_CODE (arg1) == FIXED_CST)
    return 0;
  if (TREE_CODE (arg0) == FIXED_CST)
    return 1;

  if (TREE_CODE (arg1) == COMPLEX_CST)
    return 0;
  if (TREE_CODE (arg0) == COMPLEX_CST)
    return 1;

  if (TREE_CONSTANT (arg1))
    return 0;
  if (TREE_CONSTANT (arg0))
    return 1;

  if (optimize_function_for_size_p (cfun))
    return 0;

  if (reorder && flag_evaluation_order
      && (TREE_SIDE_EFFECTS (arg0) || TREE_SIDE_EFFECTS (arg1)))
    return 0;

  /* It is preferable to swap two SSA_NAME to ensure a canonical form
     for commutative and comparison operators.  Ensuring a canonical
     form allows the optimizers to find additional redundancies without
     having to explicitly check for both orderings.  */
  if (TREE_CODE (arg0) == SSA_NAME
      && TREE_CODE (arg1) == SSA_NAME
      && SSA_NAME_VERSION (arg0) > SSA_NAME_VERSION (arg1))
    return 1;

  /* Put SSA_NAMEs last.  */
  if (TREE_CODE (arg1) == SSA_NAME)
    return 0;
  if (TREE_CODE (arg0) == SSA_NAME)
    return 1;

  /* Put variables last.  */
  if (DECL_P (arg1))
    return 0;
  if (DECL_P (arg0))
    return 1;

  return 0;
}

/* Fold comparison ARG0 CODE ARG1 (with result in TYPE), where
   ARG0 is extended to a wider type.  */

static tree
fold_widened_comparison (location_t loc, enum tree_code code,
			 tree type, tree arg0, tree arg1)
{
  tree arg0_unw = get_unwidened (arg0, NULL_TREE);
  tree arg1_unw;
  tree shorter_type, outer_type;
  tree min, max;
  bool above, below;

  if (arg0_unw == arg0)
    return NULL_TREE;
  shorter_type = TREE_TYPE (arg0_unw);

#ifdef HAVE_canonicalize_funcptr_for_compare
  /* Disable this optimization if we're casting a function pointer
     type on targets that require function pointer canonicalization.  */
  if (HAVE_canonicalize_funcptr_for_compare
      && TREE_CODE (shorter_type) == POINTER_TYPE
      && TREE_CODE (TREE_TYPE (shorter_type)) == FUNCTION_TYPE)
    return NULL_TREE;
#endif

  if (TYPE_PRECISION (TREE_TYPE (arg0)) <= TYPE_PRECISION (shorter_type))
    return NULL_TREE;

  arg1_unw = get_unwidened (arg1, NULL_TREE);

  /* If possible, express the comparison in the shorter mode.  */
  if ((code == EQ_EXPR || code == NE_EXPR
       || TYPE_UNSIGNED (TREE_TYPE (arg0)) == TYPE_UNSIGNED (shorter_type))
      && (TREE_TYPE (arg1_unw) == shorter_type
	  || ((TYPE_PRECISION (shorter_type)
	       >= TYPE_PRECISION (TREE_TYPE (arg1_unw)))
	      && (TYPE_UNSIGNED (shorter_type)
		  == TYPE_UNSIGNED (TREE_TYPE (arg1_unw))))
	  || (TREE_CODE (arg1_unw) == INTEGER_CST
	      && (TREE_CODE (shorter_type) == INTEGER_TYPE
		  || TREE_CODE (shorter_type) == BOOLEAN_TYPE)
	      && int_fits_type_p (arg1_unw, shorter_type))))
    return fold_build2_loc (loc, code, type, arg0_unw,
			fold_convert_loc (loc, shorter_type, arg1_unw));

  if (TREE_CODE (arg1_unw) != INTEGER_CST
      || TREE_CODE (shorter_type) != INTEGER_TYPE
      || !int_fits_type_p (arg1_unw, shorter_type))
    return NULL_TREE;

  /* If we are comparing with the integer that does not fit into the range
     of the shorter type, the result is known.  */
  outer_type = TREE_TYPE (arg1_unw);
  min = lower_bound_in_type (outer_type, shorter_type);
  max = upper_bound_in_type (outer_type, shorter_type);

  above = integer_nonzerop (fold_relational_const (LT_EXPR, type,
						   max, arg1_unw));
  below = integer_nonzerop (fold_relational_const (LT_EXPR, type,
						   arg1_unw, min));

  switch (code)
    {
    case EQ_EXPR:
      if (above || below)
	return omit_one_operand_loc (loc, type, integer_zero_node, arg0);
      break;

    case NE_EXPR:
      if (above || below)
	return omit_one_operand_loc (loc, type, integer_one_node, arg0);
      break;

    case LT_EXPR:
    case LE_EXPR:
      if (above)
	return omit_one_operand_loc (loc, type, integer_one_node, arg0);
      else if (below)
	return omit_one_operand_loc (loc, type, integer_zero_node, arg0);

    case GT_EXPR:
    case GE_EXPR:
      if (above)
	return omit_one_operand_loc (loc, type, integer_zero_node, arg0);
      else if (below)
	return omit_one_operand_loc (loc, type, integer_one_node, arg0);

    default:
      break;
    }

  return NULL_TREE;
}

/* Fold comparison ARG0 CODE ARG1 (with result in TYPE), where for
   ARG0 just the signedness is changed.  */

static tree
fold_sign_changed_comparison (location_t loc, enum tree_code code, tree type,
			      tree arg0, tree arg1)
{
  tree arg0_inner;
  tree inner_type, outer_type;

  if (!CONVERT_EXPR_P (arg0))
    return NULL_TREE;

  outer_type = TREE_TYPE (arg0);
  arg0_inner = TREE_OPERAND (arg0, 0);
  inner_type = TREE_TYPE (arg0_inner);

#ifdef HAVE_canonicalize_funcptr_for_compare
  /* Disable this optimization if we're casting a function pointer
     type on targets that require function pointer canonicalization.  */
  if (HAVE_canonicalize_funcptr_for_compare
      && TREE_CODE (inner_type) == POINTER_TYPE
      && TREE_CODE (TREE_TYPE (inner_type)) == FUNCTION_TYPE)
    return NULL_TREE;
#endif

  if (TYPE_PRECISION (inner_type) != TYPE_PRECISION (outer_type))
    return NULL_TREE;

  if (TREE_CODE (arg1) != INTEGER_CST
      && !(CONVERT_EXPR_P (arg1)
	   && TREE_TYPE (TREE_OPERAND (arg1, 0)) == inner_type))
    return NULL_TREE;

  if (TYPE_UNSIGNED (inner_type) != TYPE_UNSIGNED (outer_type)
      && code != NE_EXPR
      && code != EQ_EXPR)
    return NULL_TREE;

  if (POINTER_TYPE_P (inner_type) != POINTER_TYPE_P (outer_type))
    return NULL_TREE;

  if (TREE_CODE (arg1) == INTEGER_CST)
    arg1 = force_fit_type_double (inner_type, tree_to_double_int (arg1),
				  0, TREE_OVERFLOW (arg1));
  else
    arg1 = fold_convert_loc (loc, inner_type, arg1);

  return fold_build2_loc (loc, code, type, arg0_inner, arg1);
}

/* Tries to replace &a[idx] p+ s * delta with &a[idx + delta], if s is
   step of the array.  Reconstructs s and delta in the case of s *
   delta being an integer constant (and thus already folded).  ADDR is
   the address. MULT is the multiplicative expression.  If the
   function succeeds, the new address expression is returned.
   Otherwise NULL_TREE is returned.  LOC is the location of the
   resulting expression.  */

static tree
try_move_mult_to_index (location_t loc, tree addr, tree op1)
{
  tree s, delta, step;
  tree ref = TREE_OPERAND (addr, 0), pref;
  tree ret, pos;
  tree itype;
  bool mdim = false;

  /*  Strip the nops that might be added when converting op1 to sizetype. */
  STRIP_NOPS (op1);

  /* Canonicalize op1 into a possibly non-constant delta
     and an INTEGER_CST s.  */
  if (TREE_CODE (op1) == MULT_EXPR)
    {
      tree arg0 = TREE_OPERAND (op1, 0), arg1 = TREE_OPERAND (op1, 1);

      STRIP_NOPS (arg0);
      STRIP_NOPS (arg1);

      if (TREE_CODE (arg0) == INTEGER_CST)
        {
          s = arg0;
          delta = arg1;
        }
      else if (TREE_CODE (arg1) == INTEGER_CST)
        {
          s = arg1;
          delta = arg0;
        }
      else
        return NULL_TREE;
    }
  else if (TREE_CODE (op1) == INTEGER_CST)
    {
      delta = op1;
      s = NULL_TREE;
    }
  else
    {
      /* Simulate we are delta * 1.  */
      delta = op1;
      s = integer_one_node;
    }

  /* Handle &x.array the same as we would handle &x.array[0].  */
  if (TREE_CODE (ref) == COMPONENT_REF
      && TREE_CODE (TREE_TYPE (ref)) == ARRAY_TYPE)
    {
      tree domain;

      /* Remember if this was a multi-dimensional array.  */
      if (TREE_CODE (TREE_OPERAND (ref, 0)) == ARRAY_REF)
	mdim = true;

      domain = TYPE_DOMAIN (TREE_TYPE (ref));
      if (! domain)
	goto cont;
      itype = TREE_TYPE (domain);

      step = TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (ref)));
      if (TREE_CODE (step) != INTEGER_CST)
	goto cont;

      if (s)
	{
	  if (! tree_int_cst_equal (step, s))
	    goto cont;
	}
      else
	{
	  /* Try if delta is a multiple of step.  */
	  tree tmp = div_if_zero_remainder (EXACT_DIV_EXPR, op1, step);
	  if (! tmp)
	    goto cont;
	  delta = tmp;
	}

      /* Only fold here if we can verify we do not overflow one
	 dimension of a multi-dimensional array.  */
      if (mdim)
	{
	  tree tmp;

	  if (!TYPE_MIN_VALUE (domain)
	      || !TYPE_MAX_VALUE (domain)
	      || TREE_CODE (TYPE_MAX_VALUE (domain)) != INTEGER_CST)
	    goto cont;

	  tmp = fold_binary_loc (loc, PLUS_EXPR, itype,
				 fold_convert_loc (loc, itype,
						   TYPE_MIN_VALUE (domain)),
				 fold_convert_loc (loc, itype, delta));
	  if (TREE_CODE (tmp) != INTEGER_CST
	      || tree_int_cst_lt (TYPE_MAX_VALUE (domain), tmp))
	    goto cont;
	}

      /* We found a suitable component reference.  */

      pref = TREE_OPERAND (addr, 0);
      ret = copy_node (pref);
      SET_EXPR_LOCATION (ret, loc);

      ret = build4_loc (loc, ARRAY_REF, TREE_TYPE (TREE_TYPE (ref)), ret,
			fold_build2_loc
			  (loc, PLUS_EXPR, itype,
			   fold_convert_loc (loc, itype,
					     TYPE_MIN_VALUE
					       (TYPE_DOMAIN (TREE_TYPE (ref)))),
			   fold_convert_loc (loc, itype, delta)),
			NULL_TREE, NULL_TREE);
      return build_fold_addr_expr_loc (loc, ret);
    }

cont:

  for (;; ref = TREE_OPERAND (ref, 0))
    {
      if (TREE_CODE (ref) == ARRAY_REF)
	{
	  tree domain;

	  /* Remember if this was a multi-dimensional array.  */
	  if (TREE_CODE (TREE_OPERAND (ref, 0)) == ARRAY_REF)
	    mdim = true;

	  domain = TYPE_DOMAIN (TREE_TYPE (TREE_OPERAND (ref, 0)));
	  if (! domain)
	    continue;
	  itype = TREE_TYPE (domain);

	  step = array_ref_element_size (ref);
	  if (TREE_CODE (step) != INTEGER_CST)
	    continue;

	  if (s)
	    {
	      if (! tree_int_cst_equal (step, s))
                continue;
	    }
	  else
	    {
	      /* Try if delta is a multiple of step.  */
	      tree tmp = div_if_zero_remainder (EXACT_DIV_EXPR, op1, step);
	      if (! tmp)
		continue;
	      delta = tmp;
	    }

	  /* Only fold here if we can verify we do not overflow one
	     dimension of a multi-dimensional array.  */
	  if (mdim)
	    {
	      tree tmp;

	      if (TREE_CODE (TREE_OPERAND (ref, 1)) != INTEGER_CST
		  || !TYPE_MAX_VALUE (domain)
		  || TREE_CODE (TYPE_MAX_VALUE (domain)) != INTEGER_CST)
		continue;

	      tmp = fold_binary_loc (loc, PLUS_EXPR, itype,
				     fold_convert_loc (loc, itype,
						       TREE_OPERAND (ref, 1)),
				     fold_convert_loc (loc, itype, delta));
	      if (!tmp
		  || TREE_CODE (tmp) != INTEGER_CST
		  || tree_int_cst_lt (TYPE_MAX_VALUE (domain), tmp))
		continue;
	    }

	  break;
	}
      else
	mdim = false;

      if (!handled_component_p (ref))
	return NULL_TREE;
    }

  /* We found the suitable array reference.  So copy everything up to it,
     and replace the index.  */

  pref = TREE_OPERAND (addr, 0);
  ret = copy_node (pref);
  SET_EXPR_LOCATION (ret, loc);
  pos = ret;

  while (pref != ref)
    {
      pref = TREE_OPERAND (pref, 0);
      TREE_OPERAND (pos, 0) = copy_node (pref);
      pos = TREE_OPERAND (pos, 0);
    }

  TREE_OPERAND (pos, 1)
    = fold_build2_loc (loc, PLUS_EXPR, itype,
		       fold_convert_loc (loc, itype, TREE_OPERAND (pos, 1)),
		       fold_convert_loc (loc, itype, delta));
  return fold_build1_loc (loc, ADDR_EXPR, TREE_TYPE (addr), ret);
}


/* Fold A < X && A + 1 > Y to A < X && A >= Y.  Normally A + 1 > Y
   means A >= Y && A != MAX, but in this case we know that
   A < X <= MAX.  INEQ is A + 1 > Y, BOUND is A < X.  */

static tree
fold_to_nonsharp_ineq_using_bound (location_t loc, tree ineq, tree bound)
{
  tree a, typea, type = TREE_TYPE (ineq), a1, diff, y;

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
      if (TREE_INT_CST_HIGH (arg1) == -1
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

  if (operand_equal_p (arg01, arg11, 0))
    same = arg01, alt0 = arg00, alt1 = arg10;
  else if (operand_equal_p (arg00, arg10, 0))
    same = arg00, alt0 = arg01, alt1 = arg11;
  else if (operand_equal_p (arg00, arg11, 0))
    same = arg00, alt0 = arg01, alt1 = arg10;
  else if (operand_equal_p (arg01, arg10, 0))
    same = arg01, alt0 = arg00, alt1 = arg11;

  /* No identical multiplicands; see if we can find a common
     power-of-two factor in non-power-of-two multiplies.  This
     can help in multi-dimensional array access.  */
  else if (host_integerp (arg01, 0)
	   && host_integerp (arg11, 0))
    {
      HOST_WIDE_INT int01, int11, tmp;
      bool swap = false;
      tree maybe_same;
      int01 = TREE_INT_CST_LOW (arg01);
      int11 = TREE_INT_CST_LOW (arg11);

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

      if (exact_log2 (absu_hwi (int11)) > 0 && int01 % int11 == 0
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

  if (same)
    return fold_build2_loc (loc, MULT_EXPR, type,
			fold_build2_loc (loc, code, type,
				     fold_convert_loc (loc, type, alt0),
				     fold_convert_loc (loc, type, alt1)),
			fold_convert_loc (loc, type, same));

  return NULL_TREE;
}

/* Subroutine of native_encode_expr.  Encode the INTEGER_CST
   specified by EXPR into the buffer PTR of length LEN bytes.
   Return the number of bytes placed in the buffer, or zero
   upon failure.  */

static int
native_encode_int (const_tree expr, unsigned char *ptr, int len)
{
  tree type = TREE_TYPE (expr);
  int total_bytes = GET_MODE_SIZE (TYPE_MODE (type));
  int byte, offset, word, words;
  unsigned char value;

  if (total_bytes > len)
    return 0;
  words = total_bytes / UNITS_PER_WORD;

  for (byte = 0; byte < total_bytes; byte++)
    {
      int bitpos = byte * BITS_PER_UNIT;
      if (bitpos < HOST_BITS_PER_WIDE_INT)
	value = (unsigned char) (TREE_INT_CST_LOW (expr) >> bitpos);
      else
	value = (unsigned char) (TREE_INT_CST_HIGH (expr)
				 >> (bitpos - HOST_BITS_PER_WIDE_INT));

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
      ptr[offset] = value;
    }
  return total_bytes;
}


/* Subroutine of native_encode_expr.  Encode the FIXED_CST
   specified by EXPR into the buffer PTR of length LEN bytes.
   Return the number of bytes placed in the buffer, or zero
   upon failure.  */

static int
native_encode_fixed (const_tree expr, unsigned char *ptr, int len)
{
  tree type = TREE_TYPE (expr);
  enum machine_mode mode = TYPE_MODE (type);
  int total_bytes = GET_MODE_SIZE (mode);
  FIXED_VALUE_TYPE value;
  tree i_value, i_type;

  if (total_bytes * BITS_PER_UNIT > HOST_BITS_PER_DOUBLE_INT)
    return 0;

  i_type = lang_hooks.types.type_for_size (GET_MODE_BITSIZE (mode), 1);

  if (NULL_TREE == i_type
      || TYPE_PRECISION (i_type) != total_bytes)
    return 0;
  
  value = TREE_FIXED_CST (expr);
  i_value = double_int_to_tree (i_type, value.data);

  return native_encode_int (i_value, ptr, len);
}


/* Subroutine of native_encode_expr.  Encode the REAL_CST
   specified by EXPR into the buffer PTR of length LEN bytes.
   Return the number of bytes placed in the buffer, or zero
   upon failure.  */

static int
native_encode_real (const_tree expr, unsigned char *ptr, int len)
{
  tree type = TREE_TYPE (expr);
  int total_bytes = GET_MODE_SIZE (TYPE_MODE (type));
  int byte, offset, word, words, bitpos;
  unsigned char value;

  /* There are always 32 bits in each long, no matter the size of
     the hosts long.  We handle floating point representations with
     up to 192 bits.  */
  long tmp[6];

  if (total_bytes > len)
    return 0;
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
	offset = BYTES_BIG_ENDIAN ? 3 - byte : byte;
      ptr[offset + ((bitpos / BITS_PER_UNIT) & ~3)] = value;
    }
  return total_bytes;
}

/* Subroutine of native_encode_expr.  Encode the COMPLEX_CST
   specified by EXPR into the buffer PTR of length LEN bytes.
   Return the number of bytes placed in the buffer, or zero
   upon failure.  */

static int
native_encode_complex (const_tree expr, unsigned char *ptr, int len)
{
  int rsize, isize;
  tree part;

  part = TREE_REALPART (expr);
  rsize = native_encode_expr (part, ptr, len);
  if (rsize == 0)
    return 0;
  part = TREE_IMAGPART (expr);
  isize = native_encode_expr (part, ptr+rsize, len-rsize);
  if (isize != rsize)
    return 0;
  return rsize + isize;
}


/* Subroutine of native_encode_expr.  Encode the VECTOR_CST
   specified by EXPR into the buffer PTR of length LEN bytes.
   Return the number of bytes placed in the buffer, or zero
   upon failure.  */

static int
native_encode_vector (const_tree expr, unsigned char *ptr, int len)
{
  unsigned i, count;
  int size, offset;
  tree itype, elem;

  offset = 0;
  count = VECTOR_CST_NELTS (expr);
  itype = TREE_TYPE (TREE_TYPE (expr));
  size = GET_MODE_SIZE (TYPE_MODE (itype));
  for (i = 0; i < count; i++)
    {
      elem = VECTOR_CST_ELT (expr, i);
      if (native_encode_expr (elem, ptr+offset, len-offset) != size)
	return 0;
      offset += size;
    }
  return offset;
}


/* Subroutine of native_encode_expr.  Encode the STRING_CST
   specified by EXPR into the buffer PTR of length LEN bytes.
   Return the number of bytes placed in the buffer, or zero
   upon failure.  */

static int
native_encode_string (const_tree expr, unsigned char *ptr, int len)
{
  tree type = TREE_TYPE (expr);
  HOST_WIDE_INT total_bytes;

  if (TREE_CODE (type) != ARRAY_TYPE
      || TREE_CODE (TREE_TYPE (type)) != INTEGER_TYPE
      || GET_MODE_BITSIZE (TYPE_MODE (TREE_TYPE (type))) != BITS_PER_UNIT
      || !host_integerp (TYPE_SIZE_UNIT (type), 0))
    return 0;
  total_bytes = tree_low_cst (TYPE_SIZE_UNIT (type), 0);
  if (total_bytes > len)
    return 0;
  if (TREE_STRING_LENGTH (expr) < total_bytes)
    {
      memcpy (ptr, TREE_STRING_POINTER (expr), TREE_STRING_LENGTH (expr));
      memset (ptr + TREE_STRING_LENGTH (expr), 0,
	      total_bytes - TREE_STRING_LENGTH (expr));
    }
  else
    memcpy (ptr, TREE_STRING_POINTER (expr), total_bytes);
  return total_bytes;
}


/* Subroutine of fold_view_convert_expr.  Encode the INTEGER_CST,
   REAL_CST, COMPLEX_CST or VECTOR_CST specified by EXPR into the
   buffer PTR of length LEN bytes.  Return the number of bytes
   placed in the buffer, or zero upon failure.  */

int
native_encode_expr (const_tree expr, unsigned char *ptr, int len)
{
  switch (TREE_CODE (expr))
    {
    case INTEGER_CST:
      return native_encode_int (expr, ptr, len);

    case REAL_CST:
      return native_encode_real (expr, ptr, len);

    case FIXED_CST:
      return native_encode_fixed (expr, ptr, len);

    case COMPLEX_CST:
      return native_encode_complex (expr, ptr, len);

    case VECTOR_CST:
      return native_encode_vector (expr, ptr, len);

    case STRING_CST:
      return native_encode_string (expr, ptr, len);

    default:
      return 0;
    }
}


/* Subroutine of native_interpret_expr.  Interpret the contents of
   the buffer PTR of length LEN as an INTEGER_CST of type TYPE.
   If the buffer cannot be interpreted, return NULL_TREE.  */

static tree
native_interpret_int (tree type, const unsigned char *ptr, int len)
{
  int total_bytes = GET_MODE_SIZE (TYPE_MODE (type));
  double_int result;

  if (total_bytes > len
      || total_bytes * BITS_PER_UNIT > HOST_BITS_PER_DOUBLE_INT)
    return NULL_TREE;

  result = double_int::from_buffer (ptr, total_bytes);

  return double_int_to_tree (type, result);
}


/* Subroutine of native_interpret_expr.  Interpret the contents of
   the buffer PTR of length LEN as a FIXED_CST of type TYPE.
   If the buffer cannot be interpreted, return NULL_TREE.  */

static tree
native_interpret_fixed (tree type, const unsigned char *ptr, int len)
{
  int total_bytes = GET_MODE_SIZE (TYPE_MODE (type));
  double_int result;
  FIXED_VALUE_TYPE fixed_value;

  if (total_bytes > len
      || total_bytes * BITS_PER_UNIT > HOST_BITS_PER_DOUBLE_INT)
    return NULL_TREE;

  result = double_int::from_buffer (ptr, total_bytes);
  fixed_value = fixed_from_double_int (result, TYPE_MODE (type));

  return build_fixed (type, fixed_value);
}


/* Subroutine of native_interpret_expr.  Interpret the contents of
   the buffer PTR of length LEN as a REAL_CST of type TYPE.
   If the buffer cannot be interpreted, return NULL_TREE.  */

static tree
native_interpret_real (tree type, const unsigned char *ptr, int len)
{
  enum machine_mode mode = TYPE_MODE (type);
  int total_bytes = GET_MODE_SIZE (mode);
  int byte, offset, word, words, bitpos;
  unsigned char value;
  /* There are always 32 bits in each long, no matter the size of
     the hosts long.  We handle floating point representations with
     up to 192 bits.  */
  REAL_VALUE_TYPE r;
  long tmp[6];

  total_bytes = GET_MODE_SIZE (TYPE_MODE (type));
  if (total_bytes > len || total_bytes > 24)
    return NULL_TREE;
  words = (32 / BITS_PER_UNIT) / UNITS_PER_WORD;

  memset (tmp, 0, sizeof (tmp));
  for (bitpos = 0; bitpos < total_bytes * BITS_PER_UNIT;
       bitpos += BITS_PER_UNIT)
    {
      byte = (bitpos / BITS_PER_UNIT) & 3;
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
	offset = BYTES_BIG_ENDIAN ? 3 - byte : byte;
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
  size = GET_MODE_SIZE (TYPE_MODE (etype));
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


/* Subroutine of native_interpret_expr.  Interpret the contents of
   the buffer PTR of length LEN as a VECTOR_CST of type TYPE.
   If the buffer cannot be interpreted, return NULL_TREE.  */

static tree
native_interpret_vector (tree type, const unsigned char *ptr, int len)
{
  tree etype, elem;
  int i, size, count;
  tree *elements;

  etype = TREE_TYPE (type);
  size = GET_MODE_SIZE (TYPE_MODE (etype));
  count = TYPE_VECTOR_SUBPARTS (type);
  if (size * count > len)
    return NULL_TREE;

  elements = XALLOCAVEC (tree, count);
  for (i = count - 1; i >= 0; i--)
    {
      elem = native_interpret_expr (etype, ptr+(i*size), size);
      if (!elem)
	return NULL_TREE;
      elements[i] = elem;
    }
  return build_vector (type, elements);
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
      return native_interpret_int (type, ptr, len);

    case REAL_TYPE:
      return native_interpret_real (type, ptr, len);

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

static bool
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
      return true;
    default:
      return false;
    }
}

/* Fold a VIEW_CONVERT_EXPR of a constant expression EXPR to type
   TYPE at compile-time.  If we're unable to perform the conversion
   return NULL_TREE.  */

static tree
fold_view_convert_expr (tree type, tree expr)
{
  /* We support up to 512-bit values (for V8DFmode).  */
  unsigned char buffer[64];
  int len;

  /* Check that the host and target are sane.  */
  if (CHAR_BIT != 8 || BITS_PER_UNIT != 8)
    return NULL_TREE;

  len = native_encode_expr (expr, buffer, sizeof (buffer));
  if (len == 0)
    return NULL_TREE;

  return native_interpret_expr (type, buffer, len);
}

/* Build an expression for the address of T.  Folds away INDIRECT_REF
   to avoid confusing the gimplify process.  */

tree
build_fold_addr_expr_with_type_loc (location_t loc, tree t, tree ptrtype)
{
  /* The size of the object is not relevant when talking about its address.  */
  if (TREE_CODE (t) == WITH_SIZE_EXPR)
    t = TREE_OPERAND (t, 0);

  if (TREE_CODE (t) == INDIRECT_REF)
    {
      t = TREE_OPERAND (t, 0);

      if (TREE_TYPE (t) != ptrtype)
	t = build1_loc (loc, NOP_EXPR, ptrtype, t);
    }
  else if (TREE_CODE (t) == MEM_REF
	   && integer_zerop (TREE_OPERAND (t, 1)))
    return TREE_OPERAND (t, 0);
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

static bool vec_cst_ctor_to_array (tree, tree *);

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
    }

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
	      && ! VOID_TYPE_P (TREE_OPERAND (tem, 1))
	      && ! VOID_TYPE_P (TREE_OPERAND (tem, 2))
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
    case PAREN_EXPR:
      /* Re-association barriers around constants and other re-association
	 barriers can be removed.  */
      if (CONSTANT_CLASS_P (op0)
	  || TREE_CODE (op0) == PAREN_EXPR)
	return fold_convert_loc (loc, type, op0);
      return NULL_TREE;

    CASE_CONVERT:
    case FLOAT_EXPR:
    case FIX_TRUNC_EXPR:
      if (TREE_TYPE (op0) == type)
	return op0;

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

      /* Handle cases of two conversions in a row.  */
      if (CONVERT_EXPR_P (op0))
	{
	  tree inside_type = TREE_TYPE (TREE_OPERAND (op0, 0));
	  tree inter_type = TREE_TYPE (op0);
	  int inside_int = INTEGRAL_TYPE_P (inside_type);
	  int inside_ptr = POINTER_TYPE_P (inside_type);
	  int inside_float = FLOAT_TYPE_P (inside_type);
	  int inside_vec = TREE_CODE (inside_type) == VECTOR_TYPE;
	  unsigned int inside_prec = TYPE_PRECISION (inside_type);
	  int inside_unsignedp = TYPE_UNSIGNED (inside_type);
	  int inter_int = INTEGRAL_TYPE_P (inter_type);
	  int inter_ptr = POINTER_TYPE_P (inter_type);
	  int inter_float = FLOAT_TYPE_P (inter_type);
	  int inter_vec = TREE_CODE (inter_type) == VECTOR_TYPE;
	  unsigned int inter_prec = TYPE_PRECISION (inter_type);
	  int inter_unsignedp = TYPE_UNSIGNED (inter_type);
	  int final_int = INTEGRAL_TYPE_P (type);
	  int final_ptr = POINTER_TYPE_P (type);
	  int final_float = FLOAT_TYPE_P (type);
	  int final_vec = TREE_CODE (type) == VECTOR_TYPE;
	  unsigned int final_prec = TYPE_PRECISION (type);
	  int final_unsignedp = TYPE_UNSIGNED (type);

	  /* In addition to the cases of two conversions in a row
	     handled below, if we are converting something to its own
	     type via an object of identical or wider precision, neither
	     conversion is needed.  */
	  if (TYPE_MAIN_VARIANT (inside_type) == TYPE_MAIN_VARIANT (type)
	      && (((inter_int || inter_ptr) && final_int)
		  || (inter_float && final_float))
	      && inter_prec >= final_prec)
	    return fold_build1_loc (loc, code, type, TREE_OPERAND (op0, 0));

	  /* Likewise, if the intermediate and initial types are either both
	     float or both integer, we don't need the middle conversion if the
	     former is wider than the latter and doesn't change the signedness
	     (for integers).  Avoid this if the final type is a pointer since
	     then we sometimes need the middle conversion.  Likewise if the
	     final type has a precision not equal to the size of its mode.  */
	  if (((inter_int && inside_int)
	       || (inter_float && inside_float)
	       || (inter_vec && inside_vec))
	      && inter_prec >= inside_prec
	      && (inter_float || inter_vec
		  || inter_unsignedp == inside_unsignedp)
	      && ! (final_prec != GET_MODE_PRECISION (TYPE_MODE (type))
		    && TYPE_MODE (type) == TYPE_MODE (inter_type))
	      && ! final_ptr
	      && (! final_vec || inter_prec == inside_prec))
	    return fold_build1_loc (loc, code, type, TREE_OPERAND (op0, 0));

	  /* If we have a sign-extension of a zero-extended value, we can
	     replace that by a single zero-extension.  Likewise if the
	     final conversion does not change precision we can drop the
	     intermediate conversion.  */
	  if (inside_int && inter_int && final_int
	      && ((inside_prec < inter_prec && inter_prec < final_prec
		   && inside_unsignedp && !inter_unsignedp)
		  || final_prec == inter_prec))
	    return fold_build1_loc (loc, code, type, TREE_OPERAND (op0, 0));

	  /* Two conversions in a row are not needed unless:
	     - some conversion is floating-point (overstrict for now), or
	     - some conversion is a vector (overstrict for now), or
	     - the intermediate type is narrower than both initial and
	       final, or
	     - the intermediate type and innermost type differ in signedness,
	       and the outermost type is wider than the intermediate, or
	     - the initial type is a pointer type and the precisions of the
	       intermediate and final types differ, or
	     - the final type is a pointer type and the precisions of the
	       initial and intermediate types differ.  */
	  if (! inside_float && ! inter_float && ! final_float
	      && ! inside_vec && ! inter_vec && ! final_vec
	      && (inter_prec >= inside_prec || inter_prec >= final_prec)
	      && ! (inside_int && inter_int
		    && inter_unsignedp != inside_unsignedp
		    && inter_prec < final_prec)
	      && ((inter_unsignedp && inter_prec > inside_prec)
		  == (final_unsignedp && final_prec > inter_prec))
	      && ! (inside_ptr && inter_prec != final_prec)
	      && ! (final_ptr && inside_prec != inter_prec)
	      && ! (final_prec != GET_MODE_PRECISION (TYPE_MODE (type))
		    && TYPE_MODE (type) == TYPE_MODE (inter_type)))
	    return fold_build1_loc (loc, code, type, TREE_OPERAND (op0, 0));
	}

      /* Handle (T *)&A.B.C for A being of type T and B and C
	 living at offset zero.  This occurs frequently in
	 C++ upcasting and then accessing the base.  */
      if (TREE_CODE (op0) == ADDR_EXPR
	  && POINTER_TYPE_P (type)
	  && handled_component_p (TREE_OPERAND (op0, 0)))
        {
	  HOST_WIDE_INT bitsize, bitpos;
	  tree offset;
	  enum machine_mode mode;
	  int unsignedp, volatilep;
          tree base = TREE_OPERAND (op0, 0);
	  base = get_inner_reference (base, &bitsize, &bitpos, &offset,
				      &mode, &unsignedp, &volatilep, false);
	  /* If the reference was to a (constant) zero offset, we can use
	     the address of the base if it has the same base type
	     as the result type and the pointer type is unqualified.  */
	  if (! offset && bitpos == 0
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
	  TREE_NO_WARNING (tem) = 1;
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
		   && host_integerp (and1, 1))
	    {
	      unsigned HOST_WIDE_INT cst;

	      cst = tree_low_cst (and1, 1);
	      cst &= HOST_WIDE_INT_M1U
		     << (TYPE_PRECISION (TREE_TYPE (and1)) - 1);
	      change = (cst == 0);
#ifdef LOAD_EXTEND_OP
	      if (change
		  && !flag_syntax_only
		  && (LOAD_EXTEND_OP (TYPE_MODE (TREE_TYPE (and0)))
		      == ZERO_EXTEND))
		{
		  tree uns = unsigned_type_for (TREE_TYPE (and0));
		  and0 = fold_convert_loc (loc, uns, and0);
		  and1 = fold_convert_loc (loc, uns, and1);
		}
#endif
	    }
	  if (change)
	    {
	      tem = force_fit_type_double (type, tree_to_double_int (and1),
					   0, TREE_OVERFLOW (and1));
	      return fold_build2_loc (loc, BIT_AND_EXPR, type,
				  fold_convert_loc (loc, type, and0), tem);
	    }
	}

      /* Convert (T1)(X p+ Y) into ((T1)X p+ Y), for pointer type,
         when one of the new casts will fold away. Conservatively we assume
	 that this happens when X or Y is NOP_EXPR or Y is INTEGER_CST. */
      if (POINTER_TYPE_P (type)
	  && TREE_CODE (arg0) == POINTER_PLUS_EXPR
	  && (!TYPE_RESTRICT (type) || TYPE_RESTRICT (TREE_TYPE (arg0)))
	  && (TREE_CODE (TREE_OPERAND (arg0, 1)) == INTEGER_CST
	      || TREE_CODE (TREE_OPERAND (arg0, 0)) == NOP_EXPR
	      || TREE_CODE (TREE_OPERAND (arg0, 1)) == NOP_EXPR))
	{
	  tree arg00 = TREE_OPERAND (arg0, 0);
	  tree arg01 = TREE_OPERAND (arg0, 1);

	  return fold_build_pointer_plus_loc
		   (loc, fold_convert_loc (loc, type, arg00), arg01);
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
	  && TYPE_PRECISION (type) < TYPE_PRECISION (TREE_TYPE (op0)))
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

      tem = fold_convert_const (code, type, op0);
      return tem ? tem : NULL_TREE;

    case ADDR_SPACE_CONVERT_EXPR:
      if (integer_zerop (arg0))
	return fold_convert_const (code, type, arg0);
      return NULL_TREE;

    case FIXED_CONVERT_EXPR:
      tem = fold_convert_const (code, type, arg0);
      return tem ? tem : NULL_TREE;

    case VIEW_CONVERT_EXPR:
      if (TREE_TYPE (op0) == type)
	return op0;
      if (TREE_CODE (op0) == VIEW_CONVERT_EXPR)
	return fold_build1_loc (loc, VIEW_CONVERT_EXPR,
			    type, TREE_OPERAND (op0, 0));
      if (TREE_CODE (op0) == MEM_REF)
	return fold_build2_loc (loc, MEM_REF, type,
				TREE_OPERAND (op0, 0), TREE_OPERAND (op0, 1));

      /* For integral conversions with the same precision or pointer
	 conversions use a NOP_EXPR instead.  */
      if ((INTEGRAL_TYPE_P (type)
	   || POINTER_TYPE_P (type))
	  && (INTEGRAL_TYPE_P (TREE_TYPE (op0))
	      || POINTER_TYPE_P (TREE_TYPE (op0)))
	  && TYPE_PRECISION (type) == TYPE_PRECISION (TREE_TYPE (op0)))
	return fold_convert_loc (loc, type, op0);

      /* Strip inner integral conversions that do not change the precision.  */
      if (CONVERT_EXPR_P (op0)
	  && (INTEGRAL_TYPE_P (TREE_TYPE (op0))
	      || POINTER_TYPE_P (TREE_TYPE (op0)))
	  && (INTEGRAL_TYPE_P (TREE_TYPE (TREE_OPERAND (op0, 0)))
	      || POINTER_TYPE_P (TREE_TYPE (TREE_OPERAND (op0, 0))))
	  && (TYPE_PRECISION (TREE_TYPE (op0))
	      == TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (op0, 0)))))
	return fold_build1_loc (loc, VIEW_CONVERT_EXPR,
			    type, TREE_OPERAND (op0, 0));

      return fold_view_convert_expr (type, op0);

    case NEGATE_EXPR:
      tem = fold_negate_expr (loc, arg0);
      if (tem)
	return fold_convert_loc (loc, type, tem);
      return NULL_TREE;

    case ABS_EXPR:
      if (TREE_CODE (arg0) == INTEGER_CST || TREE_CODE (arg0) == REAL_CST)
	return fold_abs_const (arg0, type);
      else if (TREE_CODE (arg0) == NEGATE_EXPR)
	return fold_build1_loc (loc, ABS_EXPR, type, TREE_OPERAND (arg0, 0));
      /* Convert fabs((double)float) into (double)fabsf(float).  */
      else if (TREE_CODE (arg0) == NOP_EXPR
	       && TREE_CODE (type) == REAL_TYPE)
	{
	  tree targ0 = strip_float_extensions (arg0);
	  if (targ0 != arg0)
	    return fold_convert_loc (loc, type,
				     fold_build1_loc (loc, ABS_EXPR,
						  TREE_TYPE (targ0),
						  targ0));
	}
      /* ABS_EXPR<ABS_EXPR<x>> = ABS_EXPR<x> even if flag_wrapv is on.  */
      else if (TREE_CODE (arg0) == ABS_EXPR)
	return arg0;
      else if (tree_expr_nonnegative_p (arg0))
	return arg0;

      /* Strip sign ops from argument.  */
      if (TREE_CODE (type) == REAL_TYPE)
	{
	  tem = fold_strip_sign_ops (arg0);
	  if (tem)
	    return fold_build1_loc (loc, ABS_EXPR, type,
				fold_convert_loc (loc, type, tem));
	}
      return NULL_TREE;

    case CONJ_EXPR:
      if (TREE_CODE (TREE_TYPE (arg0)) != COMPLEX_TYPE)
	return fold_convert_loc (loc, type, arg0);
      if (TREE_CODE (arg0) == COMPLEX_EXPR)
	{
	  tree itype = TREE_TYPE (type);
	  tree rpart = fold_convert_loc (loc, itype, TREE_OPERAND (arg0, 0));
	  tree ipart = fold_convert_loc (loc, itype, TREE_OPERAND (arg0, 1));
	  return fold_build2_loc (loc, COMPLEX_EXPR, type, rpart,
			      negate_expr (ipart));
	}
      if (TREE_CODE (arg0) == COMPLEX_CST)
	{
	  tree itype = TREE_TYPE (type);
	  tree rpart = fold_convert_loc (loc, itype, TREE_REALPART (arg0));
	  tree ipart = fold_convert_loc (loc, itype, TREE_IMAGPART (arg0));
	  return build_complex (type, rpart, negate_expr (ipart));
	}
      if (TREE_CODE (arg0) == CONJ_EXPR)
	return fold_convert_loc (loc, type, TREE_OPERAND (arg0, 0));
      return NULL_TREE;

    case BIT_NOT_EXPR:
      if (TREE_CODE (arg0) == INTEGER_CST)
        return fold_not_const (arg0, type);
      else if (TREE_CODE (arg0) == BIT_NOT_EXPR)
	return fold_convert_loc (loc, type, TREE_OPERAND (arg0, 0));
      /* Convert ~ (-A) to A - 1.  */
      else if (INTEGRAL_TYPE_P (type) && TREE_CODE (arg0) == NEGATE_EXPR)
	return fold_build2_loc (loc, MINUS_EXPR, type,
			    fold_convert_loc (loc, type, TREE_OPERAND (arg0, 0)),
			    build_int_cst (type, 1));
      /* Convert ~ (A - 1) or ~ (A + -1) to -A.  */
      else if (INTEGRAL_TYPE_P (type)
	       && ((TREE_CODE (arg0) == MINUS_EXPR
		    && integer_onep (TREE_OPERAND (arg0, 1)))
		   || (TREE_CODE (arg0) == PLUS_EXPR
		       && integer_all_onesp (TREE_OPERAND (arg0, 1)))))
	return fold_build1_loc (loc, NEGATE_EXPR, type,
			    fold_convert_loc (loc, type,
					      TREE_OPERAND (arg0, 0)));
      /* Convert ~(X ^ Y) to ~X ^ Y or X ^ ~Y if ~X or ~Y simplify.  */
      else if (TREE_CODE (arg0) == BIT_XOR_EXPR
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
      /* Perform BIT_NOT_EXPR on each element individually.  */
      else if (TREE_CODE (arg0) == VECTOR_CST)
	{
	  tree *elements;
	  tree elem;
	  unsigned count = VECTOR_CST_NELTS (arg0), i;

	  elements = XALLOCAVEC (tree, count);
	  for (i = 0; i < count; i++)
	    {
	      elem = VECTOR_CST_ELT (arg0, i);
	      elem = fold_unary_loc (loc, BIT_NOT_EXPR, TREE_TYPE (type), elem);
	      if (elem == NULL_TREE)
		break;
	      elements[i] = elem;
	    }
	  if (i == count)
	    return build_vector (type, elements);
	}
      else if (COMPARISON_CLASS_P (arg0)
	       && (VECTOR_TYPE_P (type)
		   || (INTEGRAL_TYPE_P (type) && TYPE_PRECISION (type) == 1)))
	{
	  tree op_type = TREE_TYPE (TREE_OPERAND (arg0, 0));
	  enum tree_code subcode = invert_tree_comparison (TREE_CODE (arg0),
				     HONOR_NANS (TYPE_MODE (op_type)));
	  if (subcode != ERROR_MARK)
	    return build2_loc (loc, subcode, type, TREE_OPERAND (arg0, 0),
			       TREE_OPERAND (arg0, 1));
	}


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

    case REALPART_EXPR:
      if (TREE_CODE (TREE_TYPE (arg0)) != COMPLEX_TYPE)
	return fold_convert_loc (loc, type, arg0);
      if (TREE_CODE (arg0) == COMPLEX_EXPR)
	return omit_one_operand_loc (loc, type, TREE_OPERAND (arg0, 0),
				 TREE_OPERAND (arg0, 1));
      if (TREE_CODE (arg0) == COMPLEX_CST)
	return fold_convert_loc (loc, type, TREE_REALPART (arg0));
      if (TREE_CODE (arg0) == PLUS_EXPR || TREE_CODE (arg0) == MINUS_EXPR)
	{
	  tree itype = TREE_TYPE (TREE_TYPE (arg0));
	  tem = fold_build2_loc (loc, TREE_CODE (arg0), itype,
			     fold_build1_loc (loc, REALPART_EXPR, itype,
					  TREE_OPERAND (arg0, 0)),
			     fold_build1_loc (loc, REALPART_EXPR, itype,
					  TREE_OPERAND (arg0, 1)));
	  return fold_convert_loc (loc, type, tem);
	}
      if (TREE_CODE (arg0) == CONJ_EXPR)
	{
	  tree itype = TREE_TYPE (TREE_TYPE (arg0));
	  tem = fold_build1_loc (loc, REALPART_EXPR, itype,
			     TREE_OPERAND (arg0, 0));
	  return fold_convert_loc (loc, type, tem);
	}
      if (TREE_CODE (arg0) == CALL_EXPR)
	{
	  tree fn = get_callee_fndecl (arg0);
	  if (fn && DECL_BUILT_IN_CLASS (fn) == BUILT_IN_NORMAL)
	    switch (DECL_FUNCTION_CODE (fn))
	      {
	      CASE_FLT_FN (BUILT_IN_CEXPI):
	        fn = mathfn_built_in (type, BUILT_IN_COS);
		if (fn)
	          return build_call_expr_loc (loc, fn, 1, CALL_EXPR_ARG (arg0, 0));
		break;

	      default:
		break;
	      }
	}
      return NULL_TREE;

    case IMAGPART_EXPR:
      if (TREE_CODE (TREE_TYPE (arg0)) != COMPLEX_TYPE)
	return build_zero_cst (type);
      if (TREE_CODE (arg0) == COMPLEX_EXPR)
	return omit_one_operand_loc (loc, type, TREE_OPERAND (arg0, 1),
				 TREE_OPERAND (arg0, 0));
      if (TREE_CODE (arg0) == COMPLEX_CST)
	return fold_convert_loc (loc, type, TREE_IMAGPART (arg0));
      if (TREE_CODE (arg0) == PLUS_EXPR || TREE_CODE (arg0) == MINUS_EXPR)
	{
	  tree itype = TREE_TYPE (TREE_TYPE (arg0));
	  tem = fold_build2_loc (loc, TREE_CODE (arg0), itype,
			     fold_build1_loc (loc, IMAGPART_EXPR, itype,
					  TREE_OPERAND (arg0, 0)),
			     fold_build1_loc (loc, IMAGPART_EXPR, itype,
					  TREE_OPERAND (arg0, 1)));
	  return fold_convert_loc (loc, type, tem);
	}
      if (TREE_CODE (arg0) == CONJ_EXPR)
	{
	  tree itype = TREE_TYPE (TREE_TYPE (arg0));
	  tem = fold_build1_loc (loc, IMAGPART_EXPR, itype, TREE_OPERAND (arg0, 0));
	  return fold_convert_loc (loc, type, negate_expr (tem));
	}
      if (TREE_CODE (arg0) == CALL_EXPR)
	{
	  tree fn = get_callee_fndecl (arg0);
	  if (fn && DECL_BUILT_IN_CLASS (fn) == BUILT_IN_NORMAL)
	    switch (DECL_FUNCTION_CODE (fn))
	      {
	      CASE_FLT_FN (BUILT_IN_CEXPI):
	        fn = mathfn_built_in (type, BUILT_IN_SIN);
		if (fn)
	          return build_call_expr_loc (loc, fn, 1, CALL_EXPR_ARG (arg0, 0));
		break;

	      default:
		break;
	      }
	}
      return NULL_TREE;

    case INDIRECT_REF:
      /* Fold *&X to X if X is an lvalue.  */
      if (TREE_CODE (op0) == ADDR_EXPR)
	{
	  tree op00 = TREE_OPERAND (op0, 0);
	  if ((TREE_CODE (op00) == VAR_DECL
	       || TREE_CODE (op00) == PARM_DECL
	       || TREE_CODE (op00) == RESULT_DECL)
	      && !TREE_READONLY (op00))
	    return op00;
	}
      return NULL_TREE;

    case VEC_UNPACK_LO_EXPR:
    case VEC_UNPACK_HI_EXPR:
    case VEC_UNPACK_FLOAT_LO_EXPR:
    case VEC_UNPACK_FLOAT_HI_EXPR:
      {
	unsigned int nelts = TYPE_VECTOR_SUBPARTS (type), i;
	tree *elts;
	enum tree_code subcode;

	gcc_assert (TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg0)) == nelts * 2);
	if (TREE_CODE (arg0) != VECTOR_CST)
	  return NULL_TREE;

	elts = XALLOCAVEC (tree, nelts * 2);
	if (!vec_cst_ctor_to_array (arg0, elts))
	  return NULL_TREE;

	if ((!BYTES_BIG_ENDIAN) ^ (code == VEC_UNPACK_LO_EXPR
				   || code == VEC_UNPACK_FLOAT_LO_EXPR))
	  elts += nelts;

	if (code == VEC_UNPACK_LO_EXPR || code == VEC_UNPACK_HI_EXPR)
	  subcode = NOP_EXPR;
	else
	  subcode = FLOAT_EXPR;

	for (i = 0; i < nelts; i++)
	  {
	    elts[i] = fold_convert_const (subcode, TREE_TYPE (type), elts[i]);
	    if (elts[i] == NULL_TREE || !CONSTANT_CLASS_P (elts[i]))
	      return NULL_TREE;
	  }

	return build_vector (type, elts);
      }

    case REDUC_MIN_EXPR:
    case REDUC_MAX_EXPR:
    case REDUC_PLUS_EXPR:
      {
	unsigned int nelts = TYPE_VECTOR_SUBPARTS (type), i;
	tree *elts;
	enum tree_code subcode;

	if (TREE_CODE (op0) != VECTOR_CST)
	  return NULL_TREE;

	elts = XALLOCAVEC (tree, nelts);
	if (!vec_cst_ctor_to_array (op0, elts))
	  return NULL_TREE;

	switch (code)
	  {
	  case REDUC_MIN_EXPR: subcode = MIN_EXPR; break;
	  case REDUC_MAX_EXPR: subcode = MAX_EXPR; break;
	  case REDUC_PLUS_EXPR: subcode = PLUS_EXPR; break;
	  default: gcc_unreachable ();
	  }

	for (i = 1; i < nelts; i++)
	  {
	    elts[0] = const_binop (subcode, elts[0], elts[i]);
	    if (elts[0] == NULL_TREE || !CONSTANT_CLASS_P (elts[0]))
	      return NULL_TREE;
	    elts[i] = build_zero_cst (TREE_TYPE (type));
	  }

	return build_vector (type, elts);
      }

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
      int commutative = ((TREE_CODE (arg0) == TRUTH_OR_EXPR
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
  if (0 != (tem = fold_range_test (loc, code, type, op0, op1)))
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
      && 0 != (tem = fold_truth_andor_1 (loc, code, type,
					 TREE_OPERAND (arg0, 1), arg1)))
    return fold_build2_loc (loc, code, type, TREE_OPERAND (arg0, 0), tem);

  if ((tem = fold_truth_andor_1 (loc, code, type, arg0, arg1)) != 0)
    return tem;

  if (LOGICAL_OP_NON_SHORT_CIRCUIT
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
	  && simple_operand_p_2 (arg1)
	  /* Needed for sequence points to handle trappings, and
	     side-effects.  */
	  && simple_operand_p_2 (TREE_OPERAND (arg0, 1)))
	{
	  tem = fold_build2_loc (loc, ncode, type, TREE_OPERAND (arg0, 1),
				 arg1);
	  return fold_build2_loc (loc, icode, type, TREE_OPERAND (arg0, 0),
				  tem);
	}
	/* Same as abouve but for (A AND[-IF] (B AND-IF C)) -> ((A AND B) AND-IF C),
	   or (A OR[-IF] (B OR-IF C) -> ((A OR B) OR-IF C).  */
      else if (TREE_CODE (arg1) == icode
	  && simple_operand_p_2 (arg0)
	  /* Needed for sequence points to handle trappings, and
	     side-effects.  */
	  && simple_operand_p_2 (TREE_OPERAND (arg1, 0)))
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
      else if (code == icode && simple_operand_p_2 (arg0)
               && simple_operand_p_2 (arg1))
	return fold_build2_loc (loc, ncode, type, arg0, arg1);
    }

  return NULL_TREE;
}

/* Fold a binary expression of code CODE and type TYPE with operands
   OP0 and OP1, containing either a MIN-MAX or a MAX-MIN combination.
   Return the folded expression if folding is successful.  Otherwise,
   return NULL_TREE.  */

static tree
fold_minmax (location_t loc, enum tree_code code, tree type, tree op0, tree op1)
{
  enum tree_code compl_code;

  if (code == MIN_EXPR)
    compl_code = MAX_EXPR;
  else if (code == MAX_EXPR)
    compl_code = MIN_EXPR;
  else
    gcc_unreachable ();

  /* MIN (MAX (a, b), b) == b.  */
  if (TREE_CODE (op0) == compl_code
      && operand_equal_p (TREE_OPERAND (op0, 1), op1, 0))
    return omit_one_operand_loc (loc, type, op1, TREE_OPERAND (op0, 0));

  /* MIN (MAX (b, a), b) == b.  */
  if (TREE_CODE (op0) == compl_code
      && operand_equal_p (TREE_OPERAND (op0, 0), op1, 0)
      && reorder_operands_p (TREE_OPERAND (op0, 1), op1))
    return omit_one_operand_loc (loc, type, op1, TREE_OPERAND (op0, 1));

  /* MIN (a, MAX (a, b)) == a.  */
  if (TREE_CODE (op1) == compl_code
      && operand_equal_p (op0, TREE_OPERAND (op1, 0), 0)
      && reorder_operands_p (op0, TREE_OPERAND (op1, 1)))
    return omit_one_operand_loc (loc, type, op0, TREE_OPERAND (op1, 1));

  /* MIN (a, MAX (b, a)) == a.  */
  if (TREE_CODE (op1) == compl_code
      && operand_equal_p (op0, TREE_OPERAND (op1, 1), 0)
      && reorder_operands_p (op0, TREE_OPERAND (op1, 0)))
    return omit_one_operand_loc (loc, type, op0, TREE_OPERAND (op1, 0));

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
  bool swap = false;

  /* Match A +- CST code arg1 and CST code arg1.  We can change the
     first form only if overflow is undefined.  */
  if (!((TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (arg0))
	 /* In principle pointers also have undefined overflow behavior,
	    but that causes problems elsewhere.  */
	 && !POINTER_TYPE_P (TREE_TYPE (arg0))
	 && (code0 == MINUS_EXPR
	     || code0 == PLUS_EXPR)
         && TREE_CODE (TREE_OPERAND (arg0, 1)) == INTEGER_CST)
	|| code0 == INTEGER_CST))
    return NULL_TREE;

  /* Identify the constant in arg0 and its sign.  */
  if (code0 == INTEGER_CST)
    cst0 = arg0;
  else
    cst0 = TREE_OPERAND (arg0, 1);
  sgn0 = tree_int_cst_sgn (cst0);

  /* Overflowed constants and zero will cause problems.  */
  if (integer_zerop (cst0)
      || TREE_OVERFLOW (cst0))
    return NULL_TREE;

  /* See if we can reduce the magnitude of the constant in
     arg0 by changing the comparison code.  */
  if (code0 == INTEGER_CST)
    {
      /* CST <= arg1  ->  CST-1 < arg1.  */
      if (code == LE_EXPR && sgn0 == 1)
	code = LT_EXPR;
      /* -CST < arg1  ->  -CST-1 <= arg1.  */
      else if (code == LT_EXPR && sgn0 == -1)
	code = LE_EXPR;
      /* CST > arg1  ->  CST-1 >= arg1.  */
      else if (code == GT_EXPR && sgn0 == 1)
	code = GE_EXPR;
      /* -CST >= arg1  ->  -CST-1 > arg1.  */
      else if (code == GE_EXPR && sgn0 == -1)
	code = GT_EXPR;
      else
        return NULL_TREE;
      /* arg1 code' CST' might be more canonical.  */
      swap = true;
    }
  else
    {
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
    }

  /* Now build the constant reduced in magnitude.  But not if that
     would produce one outside of its types range.  */
  if (INTEGRAL_TYPE_P (TREE_TYPE (cst0))
      && ((sgn0 == 1
	   && TYPE_MIN_VALUE (TREE_TYPE (cst0))
	   && tree_int_cst_equal (cst0, TYPE_MIN_VALUE (TREE_TYPE (cst0))))
	  || (sgn0 == -1
	      && TYPE_MAX_VALUE (TREE_TYPE (cst0))
	      && tree_int_cst_equal (cst0, TYPE_MAX_VALUE (TREE_TYPE (cst0))))))
    /* We cannot swap the comparison here as that would cause us to
       endlessly recurse.  */
    return NULL_TREE;

  t = int_const_binop (sgn0 == -1 ? PLUS_EXPR : MINUS_EXPR,
		       cst0, build_int_cst (TREE_TYPE (cst0), 1));
  if (code0 != INTEGER_CST)
    t = fold_build2_loc (loc, code0, TREE_TYPE (arg0), TREE_OPERAND (arg0, 0), t);
  t = fold_convert (TREE_TYPE (arg1), t);

  /* If swapping might yield to a more canonical form, do so.  */
  if (swap)
    return fold_build2_loc (loc, swap_tree_comparison (code), type, arg1, t);
  else
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
   expressions like &p->x which can not wrap.  */

static bool
pointer_may_wrap_p (tree base, tree offset, HOST_WIDE_INT bitpos)
{
  double_int di_offset, total;

  if (!POINTER_TYPE_P (TREE_TYPE (base)))
    return true;

  if (bitpos < 0)
    return true;

  if (offset == NULL_TREE)
    di_offset = double_int_zero;
  else if (TREE_CODE (offset) != INTEGER_CST || TREE_OVERFLOW (offset))
    return true;
  else
    di_offset = TREE_INT_CST (offset);

  bool overflow;
  double_int units = double_int::from_uhwi (bitpos / BITS_PER_UNIT);
  total = di_offset.add_with_sign (units, true, &overflow);
  if (overflow)
    return true;

  if (total.high != 0)
    return true;

  HOST_WIDE_INT size = int_size_in_bytes (TREE_TYPE (TREE_TYPE (base)));
  if (size <= 0)
    return true;

  /* We can do slightly better for SIZE if we have an ADDR_EXPR of an
     array.  */
  if (TREE_CODE (base) == ADDR_EXPR)
    {
      HOST_WIDE_INT base_size;

      base_size = int_size_in_bytes (TREE_TYPE (TREE_OPERAND (base, 0)));
      if (base_size > 0 && size < base_size)
	size = base_size;
    }

  return total.low > (unsigned HOST_WIDE_INT) size;
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
  tree arg0, arg1, tem;

  arg0 = op0;
  arg1 = op1;

  STRIP_SIGN_NOPS (arg0);
  STRIP_SIGN_NOPS (arg1);

  tem = fold_relational_const (code, type, arg0, arg1);
  if (tem != NULL_TREE)
    return tem;

  /* If one arg is a real or integer constant, put it last.  */
  if (tree_swap_operands_p (arg0, arg1, true))
    return fold_build2_loc (loc, swap_tree_comparison (code), type, op1, op0);

  /* Transform comparisons of the form X +- C1 CMP C2 to X CMP C2 +- C1.  */
  if ((TREE_CODE (arg0) == PLUS_EXPR || TREE_CODE (arg0) == MINUS_EXPR)
      && (TREE_CODE (TREE_OPERAND (arg0, 1)) == INTEGER_CST
	  && !TREE_OVERFLOW (TREE_OPERAND (arg0, 1))
	  && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (arg1)))
      && (TREE_CODE (arg1) == INTEGER_CST
	  && !TREE_OVERFLOW (arg1)))
    {
      tree const1 = TREE_OPERAND (arg0, 1);
      tree const2 = arg1;
      tree variable = TREE_OPERAND (arg0, 0);
      tree lhs;
      int lhs_add;
      lhs_add = TREE_CODE (arg0) != PLUS_EXPR;

      lhs = fold_build2_loc (loc, lhs_add ? PLUS_EXPR : MINUS_EXPR,
			 TREE_TYPE (arg1), const2, const1);

      /* If the constant operation overflowed this can be
	 simplified as a comparison against INT_MAX/INT_MIN.  */
      if (TREE_CODE (lhs) == INTEGER_CST
	  && TREE_OVERFLOW (lhs))
	{
	  int const1_sgn = tree_int_cst_sgn (const1);
	  enum tree_code code2 = code;

	  /* Get the sign of the constant on the lhs if the
	     operation were VARIABLE + CONST1.  */
	  if (TREE_CODE (arg0) == MINUS_EXPR)
	    const1_sgn = -const1_sgn;

	  /* The sign of the constant determines if we overflowed
	     INT_MAX (const1_sgn == -1) or INT_MIN (const1_sgn == 1).
	     Canonicalize to the INT_MIN overflow by swapping the comparison
	     if necessary.  */
	  if (const1_sgn == -1)
	    code2 = swap_tree_comparison (code);

	  /* We now can look at the canonicalized case
	       VARIABLE + 1  CODE2  INT_MIN
	     and decide on the result.  */
	  if (code2 == LT_EXPR
	      || code2 == LE_EXPR
	      || code2 == EQ_EXPR)
	    return omit_one_operand_loc (loc, type, boolean_false_node, variable);
	  else if (code2 == NE_EXPR
		   || code2 == GE_EXPR
		   || code2 == GT_EXPR)
	    return omit_one_operand_loc (loc, type, boolean_true_node, variable);
	}

      if (TREE_CODE (lhs) == TREE_CODE (arg1)
	  && (TREE_CODE (lhs) != INTEGER_CST
	      || !TREE_OVERFLOW (lhs)))
	{
	  if (code != EQ_EXPR && code != NE_EXPR)
	    fold_overflow_warning ("assuming signed overflow does not occur "
				   "when changing X +- C1 cmp C2 to "
				   "X cmp C1 +- C2",
				   WARN_STRICT_OVERFLOW_COMPARISON);
	  return fold_build2_loc (loc, code, type, variable, lhs);
	}
    }

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
      HOST_WIDE_INT bitsize, bitpos0 = 0, bitpos1 = 0;
      enum machine_mode mode;
      int volatilep, unsignedp;
      bool indirect_base0 = false, indirect_base1 = false;

      /* Get base and offset for the access.  Strip ADDR_EXPR for
	 get_inner_reference, but put it back by stripping INDIRECT_REF
	 off the base object if possible.  indirect_baseN will be true
	 if baseN is not an address but refers to the object itself.  */
      base0 = arg0;
      if (TREE_CODE (arg0) == ADDR_EXPR)
	{
	  base0 = get_inner_reference (TREE_OPERAND (arg0, 0),
				       &bitsize, &bitpos0, &offset0, &mode,
				       &unsignedp, &volatilep, false);
	  if (TREE_CODE (base0) == INDIRECT_REF)
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
	      base0 = TREE_OPERAND (base0, 0);
	      indirect_base0 = true;
	    }
	  offset0 = TREE_OPERAND (arg0, 1);
	  if (host_integerp (offset0, 0))
	    {
	      HOST_WIDE_INT off = size_low_cst (offset0);
	      if ((HOST_WIDE_INT) (((unsigned HOST_WIDE_INT) off)
				   * BITS_PER_UNIT)
		  / BITS_PER_UNIT == (HOST_WIDE_INT) off)
		{
		  bitpos0 = off * BITS_PER_UNIT;
		  offset0 = NULL_TREE;
		}
	    }
	}

      base1 = arg1;
      if (TREE_CODE (arg1) == ADDR_EXPR)
	{
	  base1 = get_inner_reference (TREE_OPERAND (arg1, 0),
				       &bitsize, &bitpos1, &offset1, &mode,
				       &unsignedp, &volatilep, false);
	  if (TREE_CODE (base1) == INDIRECT_REF)
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
	      base1 = TREE_OPERAND (base1, 0);
	      indirect_base1 = true;
	    }
	  offset1 = TREE_OPERAND (arg1, 1);
	  if (host_integerp (offset1, 0))
	    {
	      HOST_WIDE_INT off = size_low_cst (offset1);
	      if ((HOST_WIDE_INT) (((unsigned HOST_WIDE_INT) off)
				   * BITS_PER_UNIT)
		  / BITS_PER_UNIT == (HOST_WIDE_INT) off)
		{
		  bitpos1 = off * BITS_PER_UNIT;
		  offset1 = NULL_TREE;
		}
	    }
	}

      /* A local variable can never be pointed to by
         the default SSA name of an incoming parameter.  */
      if ((TREE_CODE (arg0) == ADDR_EXPR
           && indirect_base0
           && TREE_CODE (base0) == VAR_DECL
           && auto_var_in_fn_p (base0, current_function_decl)
           && !indirect_base1
           && TREE_CODE (base1) == SSA_NAME
           && SSA_NAME_IS_DEFAULT_DEF (base1)
	   && TREE_CODE (SSA_NAME_VAR (base1)) == PARM_DECL)
          || (TREE_CODE (arg1) == ADDR_EXPR
              && indirect_base1
              && TREE_CODE (base1) == VAR_DECL
              && auto_var_in_fn_p (base1, current_function_decl)
              && !indirect_base0
              && TREE_CODE (base0) == SSA_NAME
              && SSA_NAME_IS_DEFAULT_DEF (base0)
	      && TREE_CODE (SSA_NAME_VAR (base0)) == PARM_DECL))
        {
          if (code == NE_EXPR)
            return constant_boolean_node (1, type);
          else if (code == EQ_EXPR)
            return constant_boolean_node (0, type);
        }
      /* If we have equivalent bases we might be able to simplify.  */
      else if (indirect_base0 == indirect_base1
               && operand_equal_p (base0, base1, 0))
	{
	  /* We can fold this expression to a constant if the non-constant
	     offset parts are equal.  */
	  if ((offset0 == offset1
	       || (offset0 && offset1
		   && operand_equal_p (offset0, offset1, 0)))
	      && (code == EQ_EXPR
		  || code == NE_EXPR
		  || (indirect_base0 && DECL_P (base0))
		  || POINTER_TYPE_OVERFLOW_UNDEFINED))

	    {
	      if (code != EQ_EXPR
		  && code != NE_EXPR
		  && bitpos0 != bitpos1
		  && (pointer_may_wrap_p (base0, offset0, bitpos0)
		      || pointer_may_wrap_p (base1, offset1, bitpos1)))
		fold_overflow_warning (("assuming pointer wraparound does not "
					"occur when comparing P +- C1 with "
					"P +- C2"),
				       WARN_STRICT_OVERFLOW_CONDITIONAL);

	      switch (code)
		{
		case EQ_EXPR:
		  return constant_boolean_node (bitpos0 == bitpos1, type);
		case NE_EXPR:
		  return constant_boolean_node (bitpos0 != bitpos1, type);
		case LT_EXPR:
		  return constant_boolean_node (bitpos0 < bitpos1, type);
		case LE_EXPR:
		  return constant_boolean_node (bitpos0 <= bitpos1, type);
		case GE_EXPR:
		  return constant_boolean_node (bitpos0 >= bitpos1, type);
		case GT_EXPR:
		  return constant_boolean_node (bitpos0 > bitpos1, type);
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
	  else if (bitpos0 == bitpos1
		   && ((code == EQ_EXPR || code == NE_EXPR)
		       || (indirect_base0 && DECL_P (base0))
		       || POINTER_TYPE_OVERFLOW_UNDEFINED))
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

	      if (code != EQ_EXPR
		  && code != NE_EXPR
		  && (pointer_may_wrap_p (base0, offset0, bitpos0)
		      || pointer_may_wrap_p (base1, offset1, bitpos1)))
		fold_overflow_warning (("assuming pointer wraparound does not "
					"occur when comparing P +- C1 with "
					"P +- C2"),
				       WARN_STRICT_OVERFLOW_COMPARISON);

	      return fold_build2_loc (loc, code, type, offset0, offset1);
	    }
	}
      /* For non-equal bases we can simplify if they are addresses
	 of local binding decls or constants.  */
      else if (indirect_base0 && indirect_base1
	       /* We know that !operand_equal_p (base0, base1, 0)
		  because the if condition was false.  But make
		  sure two decls are not the same.  */
	       && base0 != base1
	       && TREE_CODE (arg0) == ADDR_EXPR
	       && TREE_CODE (arg1) == ADDR_EXPR
	       && (((TREE_CODE (base0) == VAR_DECL
		     || TREE_CODE (base0) == PARM_DECL)
		    && (targetm.binds_local_p (base0)
			|| CONSTANT_CLASS_P (base1)))
		   || CONSTANT_CLASS_P (base0))
	       && (((TREE_CODE (base1) == VAR_DECL
		     || TREE_CODE (base1) == PARM_DECL)
		    && (targetm.binds_local_p (base1)
			|| CONSTANT_CLASS_P (base0)))
		   || CONSTANT_CLASS_P (base1)))
	{
	  if (code == EQ_EXPR)
	    return omit_two_operands_loc (loc, type, boolean_false_node,
				      arg0, arg1);
	  else if (code == NE_EXPR)
	    return omit_two_operands_loc (loc, type, boolean_true_node,
				      arg0, arg1);
	}
      /* For equal offsets we can simplify to a comparison of the
	 base addresses.  */
      else if (bitpos0 == bitpos1
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
    }

  /* Transform comparisons of the form X +- C1 CMP Y +- C2 to
     X CMP Y +- C2 +- C1 for signed X, Y.  This is valid if
     the resulting offset is smaller in absolute value than the
     original one.  */
  if (TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (arg0))
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
	 of lower absolute value than before.  */
      cst = int_const_binop (TREE_CODE (arg0) == TREE_CODE (arg1)
			     ? MINUS_EXPR : PLUS_EXPR,
			     const2, const1);
      if (!TREE_OVERFLOW (cst)
	  && tree_int_cst_compare (const2, cst) == tree_int_cst_sgn (const2))
	{
	  fold_overflow_warning (warnmsg, WARN_STRICT_OVERFLOW_COMPARISON);
	  return fold_build2_loc (loc, code, type,
			      variable1,
			      fold_build2_loc (loc,
					   TREE_CODE (arg1), TREE_TYPE (arg1),
					   variable2, cst));
	}

      cst = int_const_binop (TREE_CODE (arg0) == TREE_CODE (arg1)
			     ? MINUS_EXPR : PLUS_EXPR,
			     const1, const2);
      if (!TREE_OVERFLOW (cst)
	  && tree_int_cst_compare (const1, cst) == tree_int_cst_sgn (const1))
	{
	  fold_overflow_warning (warnmsg, WARN_STRICT_OVERFLOW_COMPARISON);
	  return fold_build2_loc (loc, code, type,
			      fold_build2_loc (loc, TREE_CODE (arg0), TREE_TYPE (arg0),
					   variable1, cst),
			      variable2);
	}
    }

  /* Transform comparisons of the form X * C1 CMP 0 to X CMP 0 in the
     signed arithmetic case.  That form is created by the compiler
     often enough for folding it to be of value.  One example is in
     computing loop trip counts after Operator Strength Reduction.  */
  if (TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (arg0))
      && TREE_CODE (arg0) == MULT_EXPR
      && (TREE_CODE (TREE_OPERAND (arg0, 1)) == INTEGER_CST
          && !TREE_OVERFLOW (TREE_OPERAND (arg0, 1)))
      && integer_zerop (arg1))
    {
      tree const1 = TREE_OPERAND (arg0, 1);
      tree const2 = arg1;                       /* zero */
      tree variable1 = TREE_OPERAND (arg0, 0);
      enum tree_code cmp_code = code;

      /* Handle unfolded multiplication by zero.  */
      if (integer_zerop (const1))
	return fold_build2_loc (loc, cmp_code, type, const1, const2);

      fold_overflow_warning (("assuming signed overflow does not occur when "
			      "eliminating multiplication in comparison "
			      "with zero"),
			     WARN_STRICT_OVERFLOW_COMPARISON);

      /* If const1 is negative we swap the sense of the comparison.  */
      if (tree_int_cst_sgn (const1) < 0)
        cmp_code = swap_tree_comparison (cmp_code);

      return fold_build2_loc (loc, cmp_code, type, variable1, const2);
    }

  tem = maybe_canonicalize_comparison (loc, code, type, arg0, arg1);
  if (tem)
    return tem;

  if (FLOAT_TYPE_P (TREE_TYPE (arg0)))
    {
      tree targ0 = strip_float_extensions (arg0);
      tree targ1 = strip_float_extensions (arg1);
      tree newtype = TREE_TYPE (targ0);

      if (TYPE_PRECISION (TREE_TYPE (targ1)) > TYPE_PRECISION (newtype))
	newtype = TREE_TYPE (targ1);

      /* Fold (double)float1 CMP (double)float2 into float1 CMP float2.  */
      if (TYPE_PRECISION (newtype) < TYPE_PRECISION (TREE_TYPE (arg0)))
	return fold_build2_loc (loc, code, type,
			    fold_convert_loc (loc, newtype, targ0),
			    fold_convert_loc (loc, newtype, targ1));

      /* (-a) CMP (-b) -> b CMP a  */
      if (TREE_CODE (arg0) == NEGATE_EXPR
	  && TREE_CODE (arg1) == NEGATE_EXPR)
	return fold_build2_loc (loc, code, type, TREE_OPERAND (arg1, 0),
			    TREE_OPERAND (arg0, 0));

      if (TREE_CODE (arg1) == REAL_CST)
	{
	  REAL_VALUE_TYPE cst;
	  cst = TREE_REAL_CST (arg1);

	  /* (-a) CMP CST -> a swap(CMP) (-CST)  */
	  if (TREE_CODE (arg0) == NEGATE_EXPR)
	    return fold_build2_loc (loc, swap_tree_comparison (code), type,
				TREE_OPERAND (arg0, 0),
				build_real (TREE_TYPE (arg1),
					    real_value_negate (&cst)));

	  /* IEEE doesn't distinguish +0 and -0 in comparisons.  */
	  /* a CMP (-0) -> a CMP 0  */
	  if (REAL_VALUE_MINUS_ZERO (cst))
	    return fold_build2_loc (loc, code, type, arg0,
				build_real (TREE_TYPE (arg1), dconst0));

	  /* x != NaN is always true, other ops are always false.  */
	  if (REAL_VALUE_ISNAN (cst)
	      && ! HONOR_SNANS (TYPE_MODE (TREE_TYPE (arg1))))
	    {
	      tem = (code == NE_EXPR) ? integer_one_node : integer_zero_node;
	      return omit_one_operand_loc (loc, type, tem, arg0);
	    }

	  /* Fold comparisons against infinity.  */
	  if (REAL_VALUE_ISINF (cst)
	      && MODE_HAS_INFINITIES (TYPE_MODE (TREE_TYPE (arg1))))
	    {
	      tem = fold_inf_compare (loc, code, type, arg0, arg1);
	      if (tem != NULL_TREE)
		return tem;
	    }
	}

      /* If this is a comparison of a real constant with a PLUS_EXPR
	 or a MINUS_EXPR of a real constant, we can convert it into a
	 comparison with a revised real constant as long as no overflow
	 occurs when unsafe_math_optimizations are enabled.  */
      if (flag_unsafe_math_optimizations
	  && TREE_CODE (arg1) == REAL_CST
	  && (TREE_CODE (arg0) == PLUS_EXPR
	      || TREE_CODE (arg0) == MINUS_EXPR)
	  && TREE_CODE (TREE_OPERAND (arg0, 1)) == REAL_CST
	  && 0 != (tem = const_binop (TREE_CODE (arg0) == PLUS_EXPR
				      ? MINUS_EXPR : PLUS_EXPR,
				      arg1, TREE_OPERAND (arg0, 1)))
	  && !TREE_OVERFLOW (tem))
	return fold_build2_loc (loc, code, type, TREE_OPERAND (arg0, 0), tem);

      /* Likewise, we can simplify a comparison of a real constant with
         a MINUS_EXPR whose first operand is also a real constant, i.e.
         (c1 - x) < c2 becomes x > c1-c2.  Reordering is allowed on
         floating-point types only if -fassociative-math is set.  */
      if (flag_associative_math
	  && TREE_CODE (arg1) == REAL_CST
	  && TREE_CODE (arg0) == MINUS_EXPR
	  && TREE_CODE (TREE_OPERAND (arg0, 0)) == REAL_CST
	  && 0 != (tem = const_binop (MINUS_EXPR, TREE_OPERAND (arg0, 0),
				      arg1))
	  && !TREE_OVERFLOW (tem))
	return fold_build2_loc (loc, swap_tree_comparison (code), type,
			    TREE_OPERAND (arg0, 1), tem);

      /* Fold comparisons against built-in math functions.  */
      if (TREE_CODE (arg1) == REAL_CST
	  && flag_unsafe_math_optimizations
	  && ! flag_errno_math)
	{
	  enum built_in_function fcode = builtin_mathfn_code (arg0);

	  if (fcode != END_BUILTINS)
	    {
	      tem = fold_mathfn_compare (loc, fcode, code, type, arg0, arg1);
	      if (tem != NULL_TREE)
		return tem;
	    }
	}
    }

  if (TREE_CODE (TREE_TYPE (arg0)) == INTEGER_TYPE
      && CONVERT_EXPR_P (arg0))
    {
      /* If we are widening one operand of an integer comparison,
	 see if the other operand is similarly being widened.  Perhaps we
	 can do the comparison in the narrower type.  */
      tem = fold_widened_comparison (loc, code, type, arg0, arg1);
      if (tem)
	return tem;

      /* Or if we are changing signedness.  */
      tem = fold_sign_changed_comparison (loc, code, type, arg0, arg1);
      if (tem)
	return tem;
    }

  /* If this is comparing a constant with a MIN_EXPR or a MAX_EXPR of a
     constant, we can simplify it.  */
  if (TREE_CODE (arg1) == INTEGER_CST
      && (TREE_CODE (arg0) == MIN_EXPR
	  || TREE_CODE (arg0) == MAX_EXPR)
      && TREE_CODE (TREE_OPERAND (arg0, 1)) == INTEGER_CST)
    {
      tem = optimize_minmax_comparison (loc, code, type, op0, op1);
      if (tem)
	return tem;
    }

  /* Simplify comparison of something with itself.  (For IEEE
     floating-point, we can only do some of these simplifications.)  */
  if (operand_equal_p (arg0, arg1, 0))
    {
      switch (code)
	{
	case EQ_EXPR:
	  if (! FLOAT_TYPE_P (TREE_TYPE (arg0))
	      || ! HONOR_NANS (TYPE_MODE (TREE_TYPE (arg0))))
	    return constant_boolean_node (1, type);
	  break;

	case GE_EXPR:
	case LE_EXPR:
	  if (! FLOAT_TYPE_P (TREE_TYPE (arg0))
	      || ! HONOR_NANS (TYPE_MODE (TREE_TYPE (arg0))))
	    return constant_boolean_node (1, type);
	  return fold_build2_loc (loc, EQ_EXPR, type, arg0, arg1);

	case NE_EXPR:
	  /* For NE, we can only do this simplification if integer
	     or we don't honor IEEE floating point NaNs.  */
	  if (FLOAT_TYPE_P (TREE_TYPE (arg0))
	      && HONOR_NANS (TYPE_MODE (TREE_TYPE (arg0))))
	    break;
	  /* ... fall through ...  */
	case GT_EXPR:
	case LT_EXPR:
	  return constant_boolean_node (0, type);
	default:
	  gcc_unreachable ();
	}
    }

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
      int save_p = 0;

      if (twoval_comparison_p (arg0, &cval1, &cval2, &save_p)
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

	      if (save_p)
		{
		  tem = save_expr (build2 (code, type, cval1, cval2));
		  SET_EXPR_LOCATION (tem, loc);
		  return tem;
		}
	      return fold_build2_loc (loc, code, type, cval1, cval2);
	    }
	}
    }

  /* We can fold X/C1 op C2 where C1 and C2 are integer constants
     into a single range test.  */
  if ((TREE_CODE (arg0) == TRUNC_DIV_EXPR
       || TREE_CODE (arg0) == EXACT_DIV_EXPR)
      && TREE_CODE (arg1) == INTEGER_CST
      && TREE_CODE (TREE_OPERAND (arg0, 1)) == INTEGER_CST
      && !integer_zerop (TREE_OPERAND (arg0, 1))
      && !TREE_OVERFLOW (TREE_OPERAND (arg0, 1))
      && !TREE_OVERFLOW (arg1))
    {
      tem = fold_div_compare (loc, code, type, arg0, arg1);
      if (tem != NULL_TREE)
	return tem;
    }

  /* Fold ~X op ~Y as Y op X.  */
  if (TREE_CODE (arg0) == BIT_NOT_EXPR
      && TREE_CODE (arg1) == BIT_NOT_EXPR)
    {
      tree cmp_type = TREE_TYPE (TREE_OPERAND (arg0, 0));
      return fold_build2_loc (loc, code, type,
			  fold_convert_loc (loc, cmp_type,
					    TREE_OPERAND (arg1, 0)),
			  TREE_OPERAND (arg0, 0));
    }

  /* Fold ~X op C as X op' ~C, where op' is the swapped comparison.  */
  if (TREE_CODE (arg0) == BIT_NOT_EXPR
      && (TREE_CODE (arg1) == INTEGER_CST || TREE_CODE (arg1) == VECTOR_CST))
    {
      tree cmp_type = TREE_TYPE (TREE_OPERAND (arg0, 0));
      return fold_build2_loc (loc, swap_tree_comparison (code), type,
			  TREE_OPERAND (arg0, 0),
			  fold_build1_loc (loc, BIT_NOT_EXPR, cmp_type,
				       fold_convert_loc (loc, cmp_type, arg1)));
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


/* Subroutine of fold_binary.  If P is the value of EXPR, computes
   power-of-two M and (arbitrary) N such that M divides (P-N).  This condition
   guarantees that P and N have the same least significant log2(M) bits.
   N is not otherwise constrained.  In particular, N is not normalized to
   0 <= N < M as is common.  In general, the precise value of P is unknown.
   M is chosen as large as possible such that constant N can be determined.

   Returns M and sets *RESIDUE to N.

   If ALLOW_FUNC_ALIGN is true, do take functions' DECL_ALIGN_UNIT into
   account.  This is not always possible due to PR 35705.
 */

static unsigned HOST_WIDE_INT
get_pointer_modulus_and_residue (tree expr, unsigned HOST_WIDE_INT *residue,
				 bool allow_func_align)
{
  enum tree_code code;

  *residue = 0;

  code = TREE_CODE (expr);
  if (code == ADDR_EXPR)
    {
      unsigned int bitalign;
      get_object_alignment_1 (TREE_OPERAND (expr, 0), &bitalign, residue);
      *residue /= BITS_PER_UNIT;
      return bitalign / BITS_PER_UNIT;
    }
  else if (code == POINTER_PLUS_EXPR)
    {
      tree op0, op1;
      unsigned HOST_WIDE_INT modulus;
      enum tree_code inner_code;

      op0 = TREE_OPERAND (expr, 0);
      STRIP_NOPS (op0);
      modulus = get_pointer_modulus_and_residue (op0, residue,
						 allow_func_align);

      op1 = TREE_OPERAND (expr, 1);
      STRIP_NOPS (op1);
      inner_code = TREE_CODE (op1);
      if (inner_code == INTEGER_CST)
	{
	  *residue += TREE_INT_CST_LOW (op1);
	  return modulus;
	}
      else if (inner_code == MULT_EXPR)
	{
	  op1 = TREE_OPERAND (op1, 1);
	  if (TREE_CODE (op1) == INTEGER_CST)
	    {
	      unsigned HOST_WIDE_INT align;

	      /* Compute the greatest power-of-2 divisor of op1.  */
	      align = TREE_INT_CST_LOW (op1);
	      align &= -align;

	      /* If align is non-zero and less than *modulus, replace
		 *modulus with align., If align is 0, then either op1 is 0
		 or the greatest power-of-2 divisor of op1 doesn't fit in an
		 unsigned HOST_WIDE_INT.  In either case, no additional
		 constraint is imposed.  */
	      if (align)
		modulus = MIN (modulus, align);

	      return modulus;
	    }
	}
    }

  /* If we get here, we were unable to determine anything useful about the
     expression.  */
  return 1;
}

/* Helper function for fold_vec_perm.  Store elements of VECTOR_CST or
   CONSTRUCTOR ARG into array ELTS and return true if successful.  */

static bool
vec_cst_ctor_to_array (tree arg, tree *elts)
{
  unsigned int nelts = TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg)), i;

  if (TREE_CODE (arg) == VECTOR_CST)
    {
      for (i = 0; i < VECTOR_CST_NELTS (arg); ++i)
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

/* Attempt to fold vector permutation of ARG0 and ARG1 vectors using SEL
   selector.  Return the folded VECTOR_CST or CONSTRUCTOR if successful,
   NULL_TREE otherwise.  */

static tree
fold_vec_perm (tree type, tree arg0, tree arg1, const unsigned char *sel)
{
  unsigned int nelts = TYPE_VECTOR_SUBPARTS (type), i;
  tree *elts;
  bool need_ctor = false;

  gcc_assert (TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg0)) == nelts
	      && TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg1)) == nelts);
  if (TREE_TYPE (TREE_TYPE (arg0)) != TREE_TYPE (type)
      || TREE_TYPE (TREE_TYPE (arg1)) != TREE_TYPE (type))
    return NULL_TREE;

  elts = XALLOCAVEC (tree, nelts * 3);
  if (!vec_cst_ctor_to_array (arg0, elts)
      || !vec_cst_ctor_to_array (arg1, elts + nelts))
    return NULL_TREE;

  for (i = 0; i < nelts; i++)
    {
      if (!CONSTANT_CLASS_P (elts[sel[i]]))
	need_ctor = true;
      elts[i + 2 * nelts] = unshare_expr (elts[sel[i]]);
    }

  if (need_ctor)
    {
      vec<constructor_elt, va_gc> *v;
      vec_alloc (v, nelts);
      for (i = 0; i < nelts; i++)
	CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, elts[2 * nelts + i]);
      return build_constructor (type, v);
    }
  else
    return build_vector (type, &elts[2 * nelts]);
}

/* Try to fold a pointer difference of type TYPE two address expressions of
   array references AREF0 and AREF1 using location LOC.  Return a
   simplified expression for the difference or NULL_TREE.  */

static tree
fold_addr_of_array_ref_difference (location_t loc, tree type,
				   tree aref0, tree aref1)
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
	   = fold_addr_of_array_ref_difference (loc, type, base0, base1)))
      || (INDIRECT_REF_P (base0)
	  && INDIRECT_REF_P (base1)
	  && (base_offset = fold_binary_loc (loc, MINUS_EXPR, type,
					     TREE_OPERAND (base0, 0),
					     TREE_OPERAND (base1, 0))))
      || operand_equal_p (base0, base1, 0))
    {
      tree op0 = fold_convert_loc (loc, type, TREE_OPERAND (aref0, 1));
      tree op1 = fold_convert_loc (loc, type, TREE_OPERAND (aref1, 1));
      tree esz = fold_convert_loc (loc, type, array_ref_element_size (aref0));
      tree diff = build2 (MINUS_EXPR, type, op0, op1);
      return fold_build2_loc (loc, PLUS_EXPR, type,
			      base_offset,
			      fold_build2_loc (loc, MULT_EXPR, type,
					       diff, esz));
    }
  return NULL_TREE;
}

/* If the real or vector real constant CST of type TYPE has an exact
   inverse, return it, else return NULL.  */

static tree
exact_inverse (tree type, tree cst)
{
  REAL_VALUE_TYPE r;
  tree unit_type, *elts;
  enum machine_mode mode;
  unsigned vec_nelts, i;

  switch (TREE_CODE (cst))
    {
    case REAL_CST:
      r = TREE_REAL_CST (cst);

      if (exact_real_inverse (TYPE_MODE (type), &r))
	return build_real (type, r);

      return NULL_TREE;

    case VECTOR_CST:
      vec_nelts = VECTOR_CST_NELTS (cst);
      elts = XALLOCAVEC (tree, vec_nelts);
      unit_type = TREE_TYPE (type);
      mode = TYPE_MODE (unit_type);

      for (i = 0; i < vec_nelts; i++)
	{
	  r = TREE_REAL_CST (VECTOR_CST_ELT (cst, i));
	  if (!exact_real_inverse (mode, &r))
	    return NULL_TREE;
	  elts[i] = build_real (unit_type, r);
	}

      return build_vector (type, elts);

    default:
      return NULL_TREE;
    }
}

/*  Mask out the tz least significant bits of X of type TYPE where
    tz is the number of trailing zeroes in Y.  */
static double_int
mask_with_tz (tree type, double_int x, double_int y)
{
  int tz = y.trailing_zeros ();

  if (tz > 0)
    {
      double_int mask;

      mask = ~double_int::mask (tz);
      mask = mask.ext (TYPE_PRECISION (type), TYPE_UNSIGNED (type));
      return mask & x;
    }
  return x;
}

/* Fold a binary expression of code CODE and type TYPE with operands
   OP0 and OP1.  LOC is the location of the resulting expression.
   Return the folded expression if folding is successful.  Otherwise,
   return NULL_TREE.  */

tree
fold_binary_loc (location_t loc,
	     enum tree_code code, tree type, tree op0, tree op1)
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
  if ((TREE_CODE (arg0) == INTEGER_CST && TREE_CODE (arg1) == INTEGER_CST)
      || (TREE_CODE (arg0) == REAL_CST && TREE_CODE (arg1) == REAL_CST)
      || (TREE_CODE (arg0) == FIXED_CST && TREE_CODE (arg1) == FIXED_CST)
      || (TREE_CODE (arg0) == FIXED_CST && TREE_CODE (arg1) == INTEGER_CST)
      || (TREE_CODE (arg0) == COMPLEX_CST && TREE_CODE (arg1) == COMPLEX_CST)
      || (TREE_CODE (arg0) == VECTOR_CST && TREE_CODE (arg1) == VECTOR_CST)
      || (TREE_CODE (arg0) == VECTOR_CST && TREE_CODE (arg1) == INTEGER_CST))
    {
      if (kind == tcc_binary)
	{
	  /* Make sure type and arg0 have the same saturating flag.  */
	  gcc_assert (TYPE_SATURATING (type)
		      == TYPE_SATURATING (TREE_TYPE (arg0)));
	  tem = const_binop (code, arg0, arg1);
	}
      else if (kind == tcc_comparison)
	tem = fold_relational_const (code, type, arg0, arg1);
      else
	tem = NULL_TREE;

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
      && tree_swap_operands_p (arg0, arg1, true))
    return fold_build2_loc (loc, code, type, op1, op0);

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
      && TREE_CODE (type) != VECTOR_TYPE
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
      if (TREE_CODE (arg1) == COMPOUND_EXPR
	  && reorder_operands_p (arg0, TREE_OPERAND (arg1, 0)))
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
	  HOST_WIDE_INT coffset;
	  base = get_addr_base_and_unit_offset (TREE_OPERAND (arg0, 0),
						&coffset);
	  if (!base)
	    return NULL_TREE;
	  return fold_build2 (MEM_REF, type,
			      build_fold_addr_expr (base),
			      int_const_binop (PLUS_EXPR, arg1,
					       size_int (coffset)));
	}

      return NULL_TREE;

    case POINTER_PLUS_EXPR:
      /* 0 +p index -> (type)index */
      if (integer_zerop (arg0))
	return non_lvalue_loc (loc, fold_convert_loc (loc, type, arg1));

      /* PTR +p 0 -> PTR */
      if (integer_zerop (arg1))
	return non_lvalue_loc (loc, fold_convert_loc (loc, type, arg0));

      /* INT +p INT -> (PTR)(INT + INT).  Stripping types allows for this. */
      if (INTEGRAL_TYPE_P (TREE_TYPE (arg1))
	   && INTEGRAL_TYPE_P (TREE_TYPE (arg0)))
        return fold_convert_loc (loc, type,
				 fold_build2_loc (loc, PLUS_EXPR, sizetype,
					      fold_convert_loc (loc, sizetype,
								arg1),
					      fold_convert_loc (loc, sizetype,
								arg0)));

      /* (PTR +p B) +p A -> PTR +p (B + A) */
      if (TREE_CODE (arg0) == POINTER_PLUS_EXPR)
	{
	  tree inner;
	  tree arg01 = fold_convert_loc (loc, sizetype, TREE_OPERAND (arg0, 1));
	  tree arg00 = TREE_OPERAND (arg0, 0);
	  inner = fold_build2_loc (loc, PLUS_EXPR, sizetype,
			       arg01, fold_convert_loc (loc, sizetype, arg1));
	  return fold_convert_loc (loc, type,
				   fold_build_pointer_plus_loc (loc,
								arg00, inner));
	}

      /* PTR_CST +p CST -> CST1 */
      if (TREE_CODE (arg0) == INTEGER_CST && TREE_CODE (arg1) == INTEGER_CST)
	return fold_build2_loc (loc, PLUS_EXPR, type, arg0,
			    fold_convert_loc (loc, type, arg1));

     /* Try replacing &a[i1] +p c * i2 with &a[i1 + i2], if c is step
	of the array.  Loop optimizer sometimes produce this type of
	expressions.  */
      if (TREE_CODE (arg0) == ADDR_EXPR)
	{
	  tem = try_move_mult_to_index (loc, arg0,
					fold_convert_loc (loc,
							  ssizetype, arg1));
	  if (tem)
	    return fold_convert_loc (loc, type, tem);
	}

      return NULL_TREE;

    case PLUS_EXPR:
      /* A + (-B) -> A - B */
      if (TREE_CODE (arg1) == NEGATE_EXPR)
	return fold_build2_loc (loc, MINUS_EXPR, type,
			    fold_convert_loc (loc, type, arg0),
			    fold_convert_loc (loc, type,
					      TREE_OPERAND (arg1, 0)));
      /* (-A) + B -> B - A */
      if (TREE_CODE (arg0) == NEGATE_EXPR
	  && reorder_operands_p (TREE_OPERAND (arg0, 0), arg1))
	return fold_build2_loc (loc, MINUS_EXPR, type,
			    fold_convert_loc (loc, type, arg1),
			    fold_convert_loc (loc, type,
					      TREE_OPERAND (arg0, 0)));

      if (INTEGRAL_TYPE_P (type) || VECTOR_INTEGER_TYPE_P (type))
	{
	  /* Convert ~A + 1 to -A.  */
	  if (TREE_CODE (arg0) == BIT_NOT_EXPR
	      && integer_onep (arg1))
	    return fold_build1_loc (loc, NEGATE_EXPR, type,
				fold_convert_loc (loc, type,
						  TREE_OPERAND (arg0, 0)));

	  /* ~X + X is -1.  */
	  if (TREE_CODE (arg0) == BIT_NOT_EXPR
	      && !TYPE_OVERFLOW_TRAPS (type))
	    {
	      tree tem = TREE_OPERAND (arg0, 0);

	      STRIP_NOPS (tem);
	      if (operand_equal_p (tem, arg1, 0))
		{
		  t1 = build_all_ones_cst (type);
		  return omit_one_operand_loc (loc, type, t1, arg1);
		}
	    }

	  /* X + ~X is -1.  */
	  if (TREE_CODE (arg1) == BIT_NOT_EXPR
	      && !TYPE_OVERFLOW_TRAPS (type))
	    {
	      tree tem = TREE_OPERAND (arg1, 0);

	      STRIP_NOPS (tem);
	      if (operand_equal_p (arg0, tem, 0))
		{
		  t1 = build_all_ones_cst (type);
		  return omit_one_operand_loc (loc, type, t1, arg0);
		}
	    }

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
	  if (integer_zerop (arg1))
	    return non_lvalue_loc (loc, fold_convert_loc (loc, type, arg0));

	  /* If we are adding two BIT_AND_EXPR's, both of which are and'ing
	     with a constant, and the two constants have no bits in common,
	     we should treat this as a BIT_IOR_EXPR since this may produce more
	     simplifications.  */
	  if (TREE_CODE (arg0) == BIT_AND_EXPR
	      && TREE_CODE (arg1) == BIT_AND_EXPR
	      && TREE_CODE (TREE_OPERAND (arg0, 1)) == INTEGER_CST
	      && TREE_CODE (TREE_OPERAND (arg1, 1)) == INTEGER_CST
	      && integer_zerop (const_binop (BIT_AND_EXPR,
					     TREE_OPERAND (arg0, 1),
					     TREE_OPERAND (arg1, 1))))
	    {
	      code = BIT_IOR_EXPR;
	      goto bit_ior;
	    }

	  /* Reassociate (plus (plus (mult) (foo)) (mult)) as
	     (plus (plus (mult) (mult)) (foo)) so that we can
	     take advantage of the factoring cases below.  */
	  if (TYPE_OVERFLOW_WRAPS (type)
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
	  /* See if ARG1 is zero and X + ARG1 reduces to X.  */
	  if (fold_real_zero_addition_p (TREE_TYPE (arg0), arg1, 0))
	    return non_lvalue_loc (loc, fold_convert_loc (loc, type, arg0));

	  /* Likewise if the operands are reversed.  */
	  if (fold_real_zero_addition_p (TREE_TYPE (arg1), arg0, 0))
	    return non_lvalue_loc (loc, fold_convert_loc (loc, type, arg1));

	  /* Convert X + -C into X - C.  */
	  if (TREE_CODE (arg1) == REAL_CST
	      && REAL_VALUE_NEGATIVE (TREE_REAL_CST (arg1)))
	    {
	      tem = fold_negate_const (arg1, type);
	      if (!TREE_OVERFLOW (arg1) || !flag_trapping_math)
		return fold_build2_loc (loc, MINUS_EXPR, type,
				    fold_convert_loc (loc, type, arg0),
				    fold_convert_loc (loc, type, tem));
	    }

	  /* Fold __complex__ ( x, 0 ) + __complex__ ( 0, y )
	     to __complex__ ( x, y ).  This is not the same for SNaNs or
	     if signed zeros are involved.  */
	  if (!HONOR_SNANS (TYPE_MODE (TREE_TYPE (arg0)))
              && !HONOR_SIGNED_ZEROS (TYPE_MODE (TREE_TYPE (arg0)))
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

	  if (flag_unsafe_math_optimizations
	      && (TREE_CODE (arg0) == RDIV_EXPR || TREE_CODE (arg0) == MULT_EXPR)
	      && (TREE_CODE (arg1) == RDIV_EXPR || TREE_CODE (arg1) == MULT_EXPR)
	      && (tem = distribute_real_division (loc, code, type, arg0, arg1)))
	    return tem;

	  /* Convert x+x into x*2.0.  */
	  if (operand_equal_p (arg0, arg1, 0)
	      && SCALAR_FLOAT_TYPE_P (type))
	    return fold_build2_loc (loc, MULT_EXPR, type, arg0,
				build_real (type, dconst2));

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
	 is a rotate of A by B bits.  */
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
		== element_precision (TYPE_MODE (rtype))))
	  {
	    tree tree01, tree11;
	    enum tree_code code01, code11;

	    tree01 = TREE_OPERAND (arg0, 1);
	    tree11 = TREE_OPERAND (arg1, 1);
	    STRIP_NOPS (tree01);
	    STRIP_NOPS (tree11);
	    code01 = TREE_CODE (tree01);
	    code11 = TREE_CODE (tree11);
	    if (code01 == INTEGER_CST
		&& code11 == INTEGER_CST
		&& TREE_INT_CST_HIGH (tree01) == 0
		&& TREE_INT_CST_HIGH (tree11) == 0
		&& ((TREE_INT_CST_LOW (tree01) + TREE_INT_CST_LOW (tree11))
		    == element_precision (TREE_TYPE (TREE_OPERAND (arg0, 0)))))
	      {
		tem = build2_loc (loc, LROTATE_EXPR,
				  TREE_TYPE (TREE_OPERAND (arg0, 0)),
				  TREE_OPERAND (arg0, 0),
				  code0 == LSHIFT_EXPR ? tree01 : tree11);
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
		    && 0 == compare_tree_int (tree110,
					      element_precision
					      (TREE_TYPE (TREE_OPERAND
							  (arg0, 0))))
		    && operand_equal_p (tree01, tree111, 0))
		  return
		    fold_convert_loc (loc, type,
				      build2 ((code0 == LSHIFT_EXPR
					       ? LROTATE_EXPR
					       : RROTATE_EXPR),
					      TREE_TYPE (TREE_OPERAND (arg0, 0)),
					      TREE_OPERAND (arg0, 0), tree01));
	      }
	    else if (code01 == MINUS_EXPR)
	      {
		tree tree010, tree011;
		tree010 = TREE_OPERAND (tree01, 0);
		tree011 = TREE_OPERAND (tree01, 1);
		STRIP_NOPS (tree010);
		STRIP_NOPS (tree011);
		if (TREE_CODE (tree010) == INTEGER_CST
		    && 0 == compare_tree_int (tree010,
					      element_precision
					      (TREE_TYPE (TREE_OPERAND
							  (arg0, 0))))
		    && operand_equal_p (tree11, tree011, 0))
		    return fold_convert_loc
		      (loc, type,
		       build2 ((code0 != LSHIFT_EXPR
				? LROTATE_EXPR
				: RROTATE_EXPR),
			       TREE_TYPE (TREE_OPERAND (arg0, 0)),
			       TREE_OPERAND (arg0, 0), tree11));
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
	  && !TYPE_SATURATING (type))
	{
	  tree var0, con0, lit0, minus_lit0;
	  tree var1, con1, lit1, minus_lit1;
	  tree atype = type;
	  bool ok = true;

	  /* Split both trees into variables, constants, and literals.  Then
	     associate each group together, the constants with literals,
	     then the result with variables.  This increases the chances of
	     literals being recombined later and of generating relocatable
	     expressions for the sum of a constant and literal.  */
	  var0 = split_tree (arg0, code, &con0, &lit0, &minus_lit0, 0);
	  var1 = split_tree (arg1, code, &con1, &lit1, &minus_lit1,
			     code == MINUS_EXPR);

	  /* Recombine MINUS_EXPR operands by using PLUS_EXPR.  */
	  if (code == MINUS_EXPR)
	    code = PLUS_EXPR;

	  /* With undefined overflow prefer doing association in a type
	     which wraps on overflow, if that is one of the operand types.  */
	  if ((POINTER_TYPE_P (type) && POINTER_TYPE_OVERFLOW_UNDEFINED)
	      || (INTEGRAL_TYPE_P (type) && !TYPE_OVERFLOW_WRAPS (type)))
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
	  if ((POINTER_TYPE_P (atype) && POINTER_TYPE_OVERFLOW_UNDEFINED)
	      || (INTEGRAL_TYPE_P (atype) && !TYPE_OVERFLOW_WRAPS (atype)))
	    {
	      if (var0 && var1)
		{
		  tree tmp0 = var0;
		  tree tmp1 = var1;

		  if (TREE_CODE (tmp0) == NEGATE_EXPR)
		    tmp0 = TREE_OPERAND (tmp0, 0);
		  if (CONVERT_EXPR_P (tmp0)
		      && INTEGRAL_TYPE_P (TREE_TYPE (TREE_OPERAND (tmp0, 0)))
		      && (TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (tmp0, 0)))
			  <= TYPE_PRECISION (atype)))
		    tmp0 = TREE_OPERAND (tmp0, 0);
		  if (TREE_CODE (tmp1) == NEGATE_EXPR)
		    tmp1 = TREE_OPERAND (tmp1, 0);
		  if (CONVERT_EXPR_P (tmp1)
		      && INTEGRAL_TYPE_P (TREE_TYPE (TREE_OPERAND (tmp1, 0)))
		      && (TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (tmp1, 0)))
			  <= TYPE_PRECISION (atype)))
		    tmp1 = TREE_OPERAND (tmp1, 0);
		  /* The only case we can still associate with two variables
		     is if they are the same, modulo negation and bit-pattern
		     preserving conversions.  */
		  if (!operand_equal_p (tmp0, tmp1, 0))
		    ok = false;
		}
	    }

	  /* Only do something if we found more than two objects.  Otherwise,
	     nothing has changed and we risk infinite recursion.  */
	  if (ok
	      && (2 < ((var0 != 0) + (var1 != 0)
		       + (con0 != 0) + (con1 != 0)
		       + (lit0 != 0) + (lit1 != 0)
		       + (minus_lit0 != 0) + (minus_lit1 != 0))))
	    {
	      bool any_overflows = false;
	      if (lit0) any_overflows |= TREE_OVERFLOW (lit0);
	      if (lit1) any_overflows |= TREE_OVERFLOW (lit1);
	      if (minus_lit0) any_overflows |= TREE_OVERFLOW (minus_lit0);
	      if (minus_lit1) any_overflows |= TREE_OVERFLOW (minus_lit1);
	      var0 = associate_trees (loc, var0, var1, code, atype);
	      con0 = associate_trees (loc, con0, con1, code, atype);
	      lit0 = associate_trees (loc, lit0, lit1, code, atype);
	      minus_lit0 = associate_trees (loc, minus_lit0, minus_lit1,
					    code, atype);

	      /* Preserve the MINUS_EXPR if the negative part of the literal is
		 greater than the positive part.  Otherwise, the multiplicative
		 folding code (i.e extract_muldiv) may be fooled in case
		 unsigned constants are subtracted, like in the following
		 example: ((X*2 + 4) - 8U)/2.  */
	      if (minus_lit0 && lit0)
		{
		  if (TREE_CODE (lit0) == INTEGER_CST
		      && TREE_CODE (minus_lit0) == INTEGER_CST
		      && tree_int_cst_lt (lit0, minus_lit0))
		    {
		      minus_lit0 = associate_trees (loc, minus_lit0, lit0,
						    MINUS_EXPR, atype);
		      lit0 = 0;
		    }
		  else
		    {
		      lit0 = associate_trees (loc, lit0, minus_lit0,
					      MINUS_EXPR, atype);
		      minus_lit0 = 0;
		    }
		}

	      /* Don't introduce overflows through reassociation.  */
	      if (!any_overflows
		  && ((lit0 && TREE_OVERFLOW (lit0))
		      || (minus_lit0 && TREE_OVERFLOW (minus_lit0))))
		return NULL_TREE;

	      if (minus_lit0)
		{
		  if (con0 == 0)
		    return
		      fold_convert_loc (loc, type,
					associate_trees (loc, var0, minus_lit0,
							 MINUS_EXPR, atype));
		  else
		    {
		      con0 = associate_trees (loc, con0, minus_lit0,
					      MINUS_EXPR, atype);
		      return
			fold_convert_loc (loc, type,
					  associate_trees (loc, var0, con0,
							   PLUS_EXPR, atype));
		    }
		}

	      con0 = associate_trees (loc, con0, lit0, code, atype);
	      return
		fold_convert_loc (loc, type, associate_trees (loc, var0, con0,
							      code, atype));
	    }
	}

      return NULL_TREE;

    case MINUS_EXPR:
      /* Pointer simplifications for subtraction, simple reassociations. */
      if (POINTER_TYPE_P (TREE_TYPE (arg1)) && POINTER_TYPE_P (TREE_TYPE (arg0)))
	{
	  /* (PTR0 p+ A) - (PTR1 p+ B) -> (PTR0 - PTR1) + (A - B) */
	  if (TREE_CODE (arg0) == POINTER_PLUS_EXPR
	      && TREE_CODE (arg1) == POINTER_PLUS_EXPR)
	    {
	      tree arg00 = fold_convert_loc (loc, type, TREE_OPERAND (arg0, 0));
	      tree arg01 = fold_convert_loc (loc, type, TREE_OPERAND (arg0, 1));
	      tree arg10 = fold_convert_loc (loc, type, TREE_OPERAND (arg1, 0));
	      tree arg11 = fold_convert_loc (loc, type, TREE_OPERAND (arg1, 1));
	      return fold_build2_loc (loc, PLUS_EXPR, type,
				  fold_build2_loc (loc, MINUS_EXPR, type,
					       arg00, arg10),
				  fold_build2_loc (loc, MINUS_EXPR, type,
					       arg01, arg11));
	    }
	  /* (PTR0 p+ A) - PTR1 -> (PTR0 - PTR1) + A, assuming PTR0 - PTR1 simplifies. */
	  else if (TREE_CODE (arg0) == POINTER_PLUS_EXPR)
	    {
	      tree arg00 = fold_convert_loc (loc, type, TREE_OPERAND (arg0, 0));
	      tree arg01 = fold_convert_loc (loc, type, TREE_OPERAND (arg0, 1));
	      tree tmp = fold_binary_loc (loc, MINUS_EXPR, type, arg00,
				      fold_convert_loc (loc, type, arg1));
	      if (tmp)
	        return fold_build2_loc (loc, PLUS_EXPR, type, tmp, arg01);
	    }
	}
      /* A - (-B) -> A + B */
      if (TREE_CODE (arg1) == NEGATE_EXPR)
	return fold_build2_loc (loc, PLUS_EXPR, type, op0,
			    fold_convert_loc (loc, type,
					      TREE_OPERAND (arg1, 0)));
      /* (-A) - B -> (-B) - A  where B is easily negated and we can swap.  */
      if (TREE_CODE (arg0) == NEGATE_EXPR
	  && negate_expr_p (arg1)
	  && reorder_operands_p (arg0, arg1))
	return fold_build2_loc (loc, MINUS_EXPR, type,
			    fold_convert_loc (loc, type,
					      negate_expr (arg1)),
			    fold_convert_loc (loc, type,
					      TREE_OPERAND (arg0, 0)));
      /* Convert -A - 1 to ~A.  */
      if (TREE_CODE (type) != COMPLEX_TYPE
	  && TREE_CODE (arg0) == NEGATE_EXPR
	  && integer_onep (arg1)
	  && !TYPE_OVERFLOW_TRAPS (type))
	return fold_build1_loc (loc, BIT_NOT_EXPR, type,
			    fold_convert_loc (loc, type,
					      TREE_OPERAND (arg0, 0)));

      /* Convert -1 - A to ~A.  */
      if (TREE_CODE (type) != COMPLEX_TYPE
	  && integer_all_onesp (arg0))
	return fold_build1_loc (loc, BIT_NOT_EXPR, type, op1);


      /* X - (X / Y) * Y is X % Y.  */
      if ((INTEGRAL_TYPE_P (type) || VECTOR_INTEGER_TYPE_P (type))
	  && TREE_CODE (arg1) == MULT_EXPR
	  && TREE_CODE (TREE_OPERAND (arg1, 0)) == TRUNC_DIV_EXPR
	  && operand_equal_p (arg0,
			      TREE_OPERAND (TREE_OPERAND (arg1, 0), 0), 0)
	  && operand_equal_p (TREE_OPERAND (TREE_OPERAND (arg1, 0), 1),
			      TREE_OPERAND (arg1, 1), 0))
	return
	  fold_convert_loc (loc, type,
			    fold_build2_loc (loc, TRUNC_MOD_EXPR, TREE_TYPE (arg0),
					 arg0, TREE_OPERAND (arg1, 1)));

      if (! FLOAT_TYPE_P (type))
	{
	  if (integer_zerop (arg0))
	    return negate_expr (fold_convert_loc (loc, type, arg1));
	  if (integer_zerop (arg1))
	    return non_lvalue_loc (loc, fold_convert_loc (loc, type, arg0));

	  /* Fold A - (A & B) into ~B & A.  */
	  if (!TREE_SIDE_EFFECTS (arg0)
	      && TREE_CODE (arg1) == BIT_AND_EXPR)
	    {
	      if (operand_equal_p (arg0, TREE_OPERAND (arg1, 1), 0))
		{
		  tree arg10 = fold_convert_loc (loc, type,
						 TREE_OPERAND (arg1, 0));
		  return fold_build2_loc (loc, BIT_AND_EXPR, type,
				      fold_build1_loc (loc, BIT_NOT_EXPR,
						   type, arg10),
				      fold_convert_loc (loc, type, arg0));
		}
	      if (operand_equal_p (arg0, TREE_OPERAND (arg1, 0), 0))
		{
		  tree arg11 = fold_convert_loc (loc,
						 type, TREE_OPERAND (arg1, 1));
		  return fold_build2_loc (loc, BIT_AND_EXPR, type,
				      fold_build1_loc (loc, BIT_NOT_EXPR,
						   type, arg11),
				      fold_convert_loc (loc, type, arg0));
		}
	    }

	  /* Fold (A & ~B) - (A & B) into (A ^ B) - B, where B is
	     any power of 2 minus 1.  */
	  if (TREE_CODE (arg0) == BIT_AND_EXPR
	      && TREE_CODE (arg1) == BIT_AND_EXPR
	      && operand_equal_p (TREE_OPERAND (arg0, 0),
				  TREE_OPERAND (arg1, 0), 0))
	    {
	      tree mask0 = TREE_OPERAND (arg0, 1);
	      tree mask1 = TREE_OPERAND (arg1, 1);
	      tree tem = fold_build1_loc (loc, BIT_NOT_EXPR, type, mask0);

	      if (operand_equal_p (tem, mask1, 0))
		{
		  tem = fold_build2_loc (loc, BIT_XOR_EXPR, type,
				     TREE_OPERAND (arg0, 0), mask1);
		  return fold_build2_loc (loc, MINUS_EXPR, type, tem, mask1);
		}
	    }
	}

      /* See if ARG1 is zero and X - ARG1 reduces to X.  */
      else if (fold_real_zero_addition_p (TREE_TYPE (arg0), arg1, 1))
	return non_lvalue_loc (loc, fold_convert_loc (loc, type, arg0));

      /* (ARG0 - ARG1) is the same as (-ARG1 + ARG0).  So check whether
	 ARG0 is zero and X + ARG0 reduces to X, since that would mean
	 (-ARG1 + ARG0) reduces to -ARG1.  */
      else if (fold_real_zero_addition_p (TREE_TYPE (arg1), arg0, 0))
	return negate_expr (fold_convert_loc (loc, type, arg1));

      /* Fold __complex__ ( x, 0 ) - __complex__ ( 0, y ) to
	 __complex__ ( x, -y ).  This is not the same for SNaNs or if
	 signed zeros are involved.  */
      if (!HONOR_SNANS (TYPE_MODE (TREE_TYPE (arg0)))
	  && !HONOR_SIGNED_ZEROS (TYPE_MODE (TREE_TYPE (arg0)))
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

      /* Fold &x - &x.  This can happen from &x.foo - &x.
	 This is unsafe for certain floats even in non-IEEE formats.
	 In IEEE, it is unsafe because it does wrong for NaNs.
	 Also note that operand_equal_p is always false if an operand
	 is volatile.  */

      if ((!FLOAT_TYPE_P (type) || !HONOR_NANS (TYPE_MODE (type)))
	  && operand_equal_p (arg0, arg1, 0))
	return build_zero_cst (type);

      /* A - B -> A + (-B) if B is easily negatable.  */
      if (negate_expr_p (arg1)
	  && ((FLOAT_TYPE_P (type)
               /* Avoid this transformation if B is a positive REAL_CST.  */
	       && (TREE_CODE (arg1) != REAL_CST
		   ||  REAL_VALUE_NEGATIVE (TREE_REAL_CST (arg1))))
	      || INTEGRAL_TYPE_P (type)))
	return fold_build2_loc (loc, PLUS_EXPR, type,
			    fold_convert_loc (loc, type, arg0),
			    fold_convert_loc (loc, type,
					      negate_expr (arg1)));

      /* Try folding difference of addresses.  */
      {
	HOST_WIDE_INT diff;

	if ((TREE_CODE (arg0) == ADDR_EXPR
	     || TREE_CODE (arg1) == ADDR_EXPR)
	    && ptr_difference_const (arg0, arg1, &diff))
	  return build_int_cst_type (type, diff);
      }

      /* Fold &a[i] - &a[j] to i-j.  */
      if (TREE_CODE (arg0) == ADDR_EXPR
	  && TREE_CODE (TREE_OPERAND (arg0, 0)) == ARRAY_REF
	  && TREE_CODE (arg1) == ADDR_EXPR
	  && TREE_CODE (TREE_OPERAND (arg1, 0)) == ARRAY_REF)
        {
	  tree tem = fold_addr_of_array_ref_difference (loc, type,
							TREE_OPERAND (arg0, 0),
							TREE_OPERAND (arg1, 0));
	  if (tem)
	    return tem;
	}

      if (FLOAT_TYPE_P (type)
	  && flag_unsafe_math_optimizations
	  && (TREE_CODE (arg0) == RDIV_EXPR || TREE_CODE (arg0) == MULT_EXPR)
	  && (TREE_CODE (arg1) == RDIV_EXPR || TREE_CODE (arg1) == MULT_EXPR)
	  && (tem = distribute_real_division (loc, code, type, arg0, arg1)))
	return tem;

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
      /* (-A) * (-B) -> A * B  */
      if (TREE_CODE (arg0) == NEGATE_EXPR && negate_expr_p (arg1))
	return fold_build2_loc (loc, MULT_EXPR, type,
			    fold_convert_loc (loc, type,
					      TREE_OPERAND (arg0, 0)),
			    fold_convert_loc (loc, type,
					      negate_expr (arg1)));
      if (TREE_CODE (arg1) == NEGATE_EXPR && negate_expr_p (arg0))
	return fold_build2_loc (loc, MULT_EXPR, type,
			    fold_convert_loc (loc, type,
					      negate_expr (arg0)),
			    fold_convert_loc (loc, type,
					      TREE_OPERAND (arg1, 0)));

      if (! FLOAT_TYPE_P (type))
	{
	  if (integer_zerop (arg1))
	    return omit_one_operand_loc (loc, type, arg1, arg0);
	  if (integer_onep (arg1))
	    return non_lvalue_loc (loc, fold_convert_loc (loc, type, arg0));
	  /* Transform x * -1 into -x.  Make sure to do the negation
	     on the original operand with conversions not stripped
	     because we can only strip non-sign-changing conversions.  */
	  if (integer_minus_onep (arg1))
	    return fold_convert_loc (loc, type, negate_expr (op0));
	  /* Transform x * -C into -x * C if x is easily negatable.  */
	  if (TREE_CODE (arg1) == INTEGER_CST
	      && tree_int_cst_sgn (arg1) == -1
	      && negate_expr_p (arg0)
	      && (tem = negate_expr (arg1)) != arg1
	      && !TREE_OVERFLOW (tem))
	    return fold_build2_loc (loc, MULT_EXPR, type,
	    			fold_convert_loc (loc, type,
						  negate_expr (arg0)),
				tem);

	  /* (a * (1 << b)) is (a << b)  */
	  if (TREE_CODE (arg1) == LSHIFT_EXPR
	      && integer_onep (TREE_OPERAND (arg1, 0)))
	    return fold_build2_loc (loc, LSHIFT_EXPR, type, op0,
				TREE_OPERAND (arg1, 1));
	  if (TREE_CODE (arg0) == LSHIFT_EXPR
	      && integer_onep (TREE_OPERAND (arg0, 0)))
	    return fold_build2_loc (loc, LSHIFT_EXPR, type, op1,
				TREE_OPERAND (arg0, 1));

	  /* (A + A) * C -> A * 2 * C  */
	  if (TREE_CODE (arg0) == PLUS_EXPR
	      && TREE_CODE (arg1) == INTEGER_CST
	      && operand_equal_p (TREE_OPERAND (arg0, 0),
			          TREE_OPERAND (arg0, 1), 0))
	    return fold_build2_loc (loc, MULT_EXPR, type,
				omit_one_operand_loc (loc, type,
						  TREE_OPERAND (arg0, 0),
						  TREE_OPERAND (arg0, 1)),
				fold_build2_loc (loc, MULT_EXPR, type,
					     build_int_cst (type, 2) , arg1));

	  /* ((T) (X /[ex] C)) * C cancels out if the conversion is
	     sign-changing only.  */
	  if (TREE_CODE (arg1) == INTEGER_CST
	      && TREE_CODE (arg0) == EXACT_DIV_EXPR
	      && operand_equal_p (arg1, TREE_OPERAND (arg0, 1), 0))
	    return fold_convert_loc (loc, type, TREE_OPERAND (arg0, 0));

	  strict_overflow_p = false;
	  if (TREE_CODE (arg1) == INTEGER_CST
	      && 0 != (tem = extract_muldiv (op0, arg1, code, NULL_TREE,
					     &strict_overflow_p)))
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
	  /* Maybe fold x * 0 to 0.  The expressions aren't the same
	     when x is NaN, since x * 0 is also NaN.  Nor are they the
	     same in modes with signed zeros, since multiplying a
	     negative value by 0 gives -0, not +0.  */
	  if (!HONOR_NANS (TYPE_MODE (TREE_TYPE (arg0)))
	      && !HONOR_SIGNED_ZEROS (TYPE_MODE (TREE_TYPE (arg0)))
	      && real_zerop (arg1))
	    return omit_one_operand_loc (loc, type, arg1, arg0);
	  /* In IEEE floating point, x*1 is not equivalent to x for snans.
	     Likewise for complex arithmetic with signed zeros.  */
	  if (!HONOR_SNANS (TYPE_MODE (TREE_TYPE (arg0)))
	      && (!HONOR_SIGNED_ZEROS (TYPE_MODE (TREE_TYPE (arg0)))
		  || !COMPLEX_FLOAT_TYPE_P (TREE_TYPE (arg0)))
	      && real_onep (arg1))
	    return non_lvalue_loc (loc, fold_convert_loc (loc, type, arg0));

	  /* Transform x * -1.0 into -x.  */
	  if (!HONOR_SNANS (TYPE_MODE (TREE_TYPE (arg0)))
	      && (!HONOR_SIGNED_ZEROS (TYPE_MODE (TREE_TYPE (arg0)))
		  || !COMPLEX_FLOAT_TYPE_P (TREE_TYPE (arg0)))
	      && real_minus_onep (arg1))
	    return fold_convert_loc (loc, type, negate_expr (arg0));

	  /* Convert (C1/X)*C2 into (C1*C2)/X.  This transformation may change
             the result for floating point types due to rounding so it is applied
             only if -fassociative-math was specify.  */
	  if (flag_associative_math
	      && TREE_CODE (arg0) == RDIV_EXPR
	      && TREE_CODE (arg1) == REAL_CST
	      && TREE_CODE (TREE_OPERAND (arg0, 0)) == REAL_CST)
	    {
	      tree tem = const_binop (MULT_EXPR, TREE_OPERAND (arg0, 0),
				      arg1);
	      if (tem)
		return fold_build2_loc (loc, RDIV_EXPR, type, tem,
				    TREE_OPERAND (arg0, 1));
	    }

          /* Strip sign operations from X in X*X, i.e. -Y*-Y -> Y*Y.  */
	  if (operand_equal_p (arg0, arg1, 0))
	    {
	      tree tem = fold_strip_sign_ops (arg0);
	      if (tem != NULL_TREE)
		{
		  tem = fold_convert_loc (loc, type, tem);
		  return fold_build2_loc (loc, MULT_EXPR, type, tem, tem);
		}
	    }

	  /* Fold z * +-I to __complex__ (-+__imag z, +-__real z).
	     This is not the same for NaNs or if signed zeros are
	     involved.  */
	  if (!HONOR_NANS (TYPE_MODE (TREE_TYPE (arg0)))
              && !HONOR_SIGNED_ZEROS (TYPE_MODE (TREE_TYPE (arg0)))
	      && COMPLEX_FLOAT_TYPE_P (TREE_TYPE (arg0))
	      && TREE_CODE (arg1) == COMPLEX_CST
	      && real_zerop (TREE_REALPART (arg1)))
	    {
	      tree rtype = TREE_TYPE (TREE_TYPE (arg0));
	      if (real_onep (TREE_IMAGPART (arg1)))
		return
		  fold_build2_loc (loc, COMPLEX_EXPR, type,
			       negate_expr (fold_build1_loc (loc, IMAGPART_EXPR,
							     rtype, arg0)),
			       fold_build1_loc (loc, REALPART_EXPR, rtype, arg0));
	      else if (real_minus_onep (TREE_IMAGPART (arg1)))
		return
		  fold_build2_loc (loc, COMPLEX_EXPR, type,
			       fold_build1_loc (loc, IMAGPART_EXPR, rtype, arg0),
			       negate_expr (fold_build1_loc (loc, REALPART_EXPR,
							     rtype, arg0)));
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

	  if (flag_unsafe_math_optimizations)
	    {
	      enum built_in_function fcode0 = builtin_mathfn_code (arg0);
	      enum built_in_function fcode1 = builtin_mathfn_code (arg1);

	      /* Optimizations of root(...)*root(...).  */
	      if (fcode0 == fcode1 && BUILTIN_ROOT_P (fcode0))
		{
		  tree rootfn, arg;
		  tree arg00 = CALL_EXPR_ARG (arg0, 0);
		  tree arg10 = CALL_EXPR_ARG (arg1, 0);

		  /* Optimize sqrt(x)*sqrt(x) as x.  */
		  if (BUILTIN_SQRT_P (fcode0)
		      && operand_equal_p (arg00, arg10, 0)
		      && ! HONOR_SNANS (TYPE_MODE (type)))
		    return arg00;

	          /* Optimize root(x)*root(y) as root(x*y).  */
		  rootfn = TREE_OPERAND (CALL_EXPR_FN (arg0), 0);
		  arg = fold_build2_loc (loc, MULT_EXPR, type, arg00, arg10);
		  return build_call_expr_loc (loc, rootfn, 1, arg);
		}

	      /* Optimize expN(x)*expN(y) as expN(x+y).  */
	      if (fcode0 == fcode1 && BUILTIN_EXPONENT_P (fcode0))
		{
		  tree expfn = TREE_OPERAND (CALL_EXPR_FN (arg0), 0);
		  tree arg = fold_build2_loc (loc, PLUS_EXPR, type,
					  CALL_EXPR_ARG (arg0, 0),
					  CALL_EXPR_ARG (arg1, 0));
		  return build_call_expr_loc (loc, expfn, 1, arg);
		}

	      /* Optimizations of pow(...)*pow(...).  */
	      if ((fcode0 == BUILT_IN_POW && fcode1 == BUILT_IN_POW)
		  || (fcode0 == BUILT_IN_POWF && fcode1 == BUILT_IN_POWF)
		  || (fcode0 == BUILT_IN_POWL && fcode1 == BUILT_IN_POWL))
		{
		  tree arg00 = CALL_EXPR_ARG (arg0, 0);
		  tree arg01 = CALL_EXPR_ARG (arg0, 1);
		  tree arg10 = CALL_EXPR_ARG (arg1, 0);
		  tree arg11 = CALL_EXPR_ARG (arg1, 1);

		  /* Optimize pow(x,y)*pow(z,y) as pow(x*z,y).  */
		  if (operand_equal_p (arg01, arg11, 0))
		    {
		      tree powfn = TREE_OPERAND (CALL_EXPR_FN (arg0), 0);
		      tree arg = fold_build2_loc (loc, MULT_EXPR, type,
					      arg00, arg10);
		      return build_call_expr_loc (loc, powfn, 2, arg, arg01);
		    }

		  /* Optimize pow(x,y)*pow(x,z) as pow(x,y+z).  */
		  if (operand_equal_p (arg00, arg10, 0))
		    {
		      tree powfn = TREE_OPERAND (CALL_EXPR_FN (arg0), 0);
		      tree arg = fold_build2_loc (loc, PLUS_EXPR, type,
					      arg01, arg11);
		      return build_call_expr_loc (loc, powfn, 2, arg00, arg);
		    }
		}

	      /* Optimize tan(x)*cos(x) as sin(x).  */
	      if (((fcode0 == BUILT_IN_TAN && fcode1 == BUILT_IN_COS)
		   || (fcode0 == BUILT_IN_TANF && fcode1 == BUILT_IN_COSF)
		   || (fcode0 == BUILT_IN_TANL && fcode1 == BUILT_IN_COSL)
		   || (fcode0 == BUILT_IN_COS && fcode1 == BUILT_IN_TAN)
		   || (fcode0 == BUILT_IN_COSF && fcode1 == BUILT_IN_TANF)
		   || (fcode0 == BUILT_IN_COSL && fcode1 == BUILT_IN_TANL))
		  && operand_equal_p (CALL_EXPR_ARG (arg0, 0),
				      CALL_EXPR_ARG (arg1, 0), 0))
		{
		  tree sinfn = mathfn_built_in (type, BUILT_IN_SIN);

		  if (sinfn != NULL_TREE)
		    return build_call_expr_loc (loc, sinfn, 1,
					    CALL_EXPR_ARG (arg0, 0));
		}

	      /* Optimize x*pow(x,c) as pow(x,c+1).  */
	      if (fcode1 == BUILT_IN_POW
		  || fcode1 == BUILT_IN_POWF
		  || fcode1 == BUILT_IN_POWL)
		{
		  tree arg10 = CALL_EXPR_ARG (arg1, 0);
		  tree arg11 = CALL_EXPR_ARG (arg1, 1);
		  if (TREE_CODE (arg11) == REAL_CST
		      && !TREE_OVERFLOW (arg11)
		      && operand_equal_p (arg0, arg10, 0))
		    {
		      tree powfn = TREE_OPERAND (CALL_EXPR_FN (arg1), 0);
		      REAL_VALUE_TYPE c;
		      tree arg;

		      c = TREE_REAL_CST (arg11);
		      real_arithmetic (&c, PLUS_EXPR, &c, &dconst1);
		      arg = build_real (type, c);
		      return build_call_expr_loc (loc, powfn, 2, arg0, arg);
		    }
		}

	      /* Optimize pow(x,c)*x as pow(x,c+1).  */
	      if (fcode0 == BUILT_IN_POW
		  || fcode0 == BUILT_IN_POWF
		  || fcode0 == BUILT_IN_POWL)
		{
		  tree arg00 = CALL_EXPR_ARG (arg0, 0);
		  tree arg01 = CALL_EXPR_ARG (arg0, 1);
		  if (TREE_CODE (arg01) == REAL_CST
		      && !TREE_OVERFLOW (arg01)
		      && operand_equal_p (arg1, arg00, 0))
		    {
		      tree powfn = TREE_OPERAND (CALL_EXPR_FN (arg0), 0);
		      REAL_VALUE_TYPE c;
		      tree arg;

		      c = TREE_REAL_CST (arg01);
		      real_arithmetic (&c, PLUS_EXPR, &c, &dconst1);
		      arg = build_real (type, c);
		      return build_call_expr_loc (loc, powfn, 2, arg1, arg);
		    }
		}

	      /* Canonicalize x*x as pow(x,2.0), which is expanded as x*x.  */
	      if (!in_gimple_form
		  && optimize
		  && operand_equal_p (arg0, arg1, 0))
		{
		  tree powfn = mathfn_built_in (type, BUILT_IN_POW);

		  if (powfn)
		    {
		      tree arg = build_real (type, dconst2);
		      return build_call_expr_loc (loc, powfn, 2, arg0, arg);
		    }
		}
	    }
	}
      goto associate;

    case BIT_IOR_EXPR:
    bit_ior:
      if (integer_all_onesp (arg1))
	return omit_one_operand_loc (loc, type, arg1, arg0);
      if (integer_zerop (arg1))
	return non_lvalue_loc (loc, fold_convert_loc (loc, type, arg0));
      if (operand_equal_p (arg0, arg1, 0))
	return non_lvalue_loc (loc, fold_convert_loc (loc, type, arg0));

      /* ~X | X is -1.  */
      if (TREE_CODE (arg0) == BIT_NOT_EXPR
	  && operand_equal_p (TREE_OPERAND (arg0, 0), arg1, 0))
	{
	  t1 = build_zero_cst (type);
	  t1 = fold_unary_loc (loc, BIT_NOT_EXPR, type, t1);
	  return omit_one_operand_loc (loc, type, t1, arg1);
	}

      /* X | ~X is -1.  */
      if (TREE_CODE (arg1) == BIT_NOT_EXPR
	  && operand_equal_p (arg0, TREE_OPERAND (arg1, 0), 0))
	{
	  t1 = build_zero_cst (type);
	  t1 = fold_unary_loc (loc, BIT_NOT_EXPR, type, t1);
	  return omit_one_operand_loc (loc, type, t1, arg0);
	}

      /* Canonicalize (X & C1) | C2.  */
      if (TREE_CODE (arg0) == BIT_AND_EXPR
	  && TREE_CODE (arg1) == INTEGER_CST
	  && TREE_CODE (TREE_OPERAND (arg0, 1)) == INTEGER_CST)
	{
	  double_int c1, c2, c3, msk;
	  int width = TYPE_PRECISION (type), w;
	  bool try_simplify = true;

	  c1 = tree_to_double_int (TREE_OPERAND (arg0, 1));
	  c2 = tree_to_double_int (arg1);

	  /* If (C1&C2) == C1, then (X&C1)|C2 becomes (X,C2).  */
	  if ((c1 & c2) == c1)
	    return omit_one_operand_loc (loc, type, arg1,
					 TREE_OPERAND (arg0, 0));

	  msk = double_int::mask (width);

	  /* If (C1|C2) == ~0 then (X&C1)|C2 becomes X|C2.  */
	  if (msk.and_not (c1 | c2).is_zero ())
	    return fold_build2_loc (loc, BIT_IOR_EXPR, type,
				    TREE_OPERAND (arg0, 0), arg1);

	  /* Minimize the number of bits set in C1, i.e. C1 := C1 & ~C2,
	     unless (C1 & ~C2) | (C2 & C3) for some C3 is a mask of some
	     mode which allows further optimizations.  */
	  c1 &= msk;
	  c2 &= msk;
	  c3 = c1.and_not (c2);
	  for (w = BITS_PER_UNIT;
	       w <= width && w <= HOST_BITS_PER_WIDE_INT;
	       w <<= 1)
	    {
	      unsigned HOST_WIDE_INT mask
		= HOST_WIDE_INT_M1U >> (HOST_BITS_PER_WIDE_INT - w);
	      if (((c1.low | c2.low) & mask) == mask
		  && (c1.low & ~mask) == 0 && c1.high == 0)
		{
		  c3 = double_int::from_uhwi (mask);
		  break;
		}
	    }

	  /* If X is a tree of the form (Y * K1) & K2, this might conflict
	     with that optimization from the BIT_AND_EXPR optimizations.
	     This could end up in an infinite recursion.  */
	  if (TREE_CODE (TREE_OPERAND (arg0, 0)) == MULT_EXPR
	      && TREE_CODE (TREE_OPERAND (TREE_OPERAND (arg0, 0), 1))
	                    == INTEGER_CST)
	  {
	    tree t = TREE_OPERAND (TREE_OPERAND (arg0, 0), 1);
	    double_int masked = mask_with_tz (type, c3, tree_to_double_int (t));

	    try_simplify = (masked != c1);
	  }

	  if (try_simplify && c3 != c1)
	    return fold_build2_loc (loc, BIT_IOR_EXPR, type,
				    fold_build2_loc (loc, BIT_AND_EXPR, type,
						     TREE_OPERAND (arg0, 0),
						     double_int_to_tree (type,
									 c3)),
				    arg1);
	}

      /* (X & Y) | Y is (X, Y).  */
      if (TREE_CODE (arg0) == BIT_AND_EXPR
	  && operand_equal_p (TREE_OPERAND (arg0, 1), arg1, 0))
	return omit_one_operand_loc (loc, type, arg1, TREE_OPERAND (arg0, 0));
      /* (X & Y) | X is (Y, X).  */
      if (TREE_CODE (arg0) == BIT_AND_EXPR
	  && operand_equal_p (TREE_OPERAND (arg0, 0), arg1, 0)
	  && reorder_operands_p (TREE_OPERAND (arg0, 1), arg1))
	return omit_one_operand_loc (loc, type, arg1, TREE_OPERAND (arg0, 1));
      /* X | (X & Y) is (Y, X).  */
      if (TREE_CODE (arg1) == BIT_AND_EXPR
	  && operand_equal_p (arg0, TREE_OPERAND (arg1, 0), 0)
	  && reorder_operands_p (arg0, TREE_OPERAND (arg1, 1)))
	return omit_one_operand_loc (loc, type, arg0, TREE_OPERAND (arg1, 1));
      /* X | (Y & X) is (Y, X).  */
      if (TREE_CODE (arg1) == BIT_AND_EXPR
	  && operand_equal_p (arg0, TREE_OPERAND (arg1, 1), 0)
	  && reorder_operands_p (arg0, TREE_OPERAND (arg1, 0)))
	return omit_one_operand_loc (loc, type, arg0, TREE_OPERAND (arg1, 0));

      /* (X & ~Y) | (~X & Y) is X ^ Y */
      if (TREE_CODE (arg0) == BIT_AND_EXPR
	  && TREE_CODE (arg1) == BIT_AND_EXPR)
        {
	  tree a0, a1, l0, l1, n0, n1;

	  a0 = fold_convert_loc (loc, type, TREE_OPERAND (arg1, 0));
	  a1 = fold_convert_loc (loc, type, TREE_OPERAND (arg1, 1));

	  l0 = fold_convert_loc (loc, type, TREE_OPERAND (arg0, 0));
	  l1 = fold_convert_loc (loc, type, TREE_OPERAND (arg0, 1));
	  
	  n0 = fold_build1_loc (loc, BIT_NOT_EXPR, type, l0);
	  n1 = fold_build1_loc (loc, BIT_NOT_EXPR, type, l1);
	  
	  if ((operand_equal_p (n0, a0, 0)
	       && operand_equal_p (n1, a1, 0))
	      || (operand_equal_p (n0, a1, 0)
		  && operand_equal_p (n1, a0, 0)))
	    return fold_build2_loc (loc, BIT_XOR_EXPR, type, l0, n1);
	}

      t1 = distribute_bit_expr (loc, code, type, arg0, arg1);
      if (t1 != NULL_TREE)
	return t1;

      /* Convert (or (not arg0) (not arg1)) to (not (and (arg0) (arg1))).

	 This results in more efficient code for machines without a NAND
	 instruction.  Combine will canonicalize to the first form
	 which will allow use of NAND instructions provided by the
	 backend if they exist.  */
      if (TREE_CODE (arg0) == BIT_NOT_EXPR
	  && TREE_CODE (arg1) == BIT_NOT_EXPR)
	{
	  return
	    fold_build1_loc (loc, BIT_NOT_EXPR, type,
			 build2 (BIT_AND_EXPR, type,
				 fold_convert_loc (loc, type,
						   TREE_OPERAND (arg0, 0)),
				 fold_convert_loc (loc, type,
						   TREE_OPERAND (arg1, 0))));
	}

      /* See if this can be simplified into a rotate first.  If that
	 is unsuccessful continue in the association code.  */
      goto bit_rotate;

    case BIT_XOR_EXPR:
      if (integer_zerop (arg1))
	return non_lvalue_loc (loc, fold_convert_loc (loc, type, arg0));
      if (integer_all_onesp (arg1))
	return fold_build1_loc (loc, BIT_NOT_EXPR, type, op0);
      if (operand_equal_p (arg0, arg1, 0))
	return omit_one_operand_loc (loc, type, integer_zero_node, arg0);

      /* ~X ^ X is -1.  */
      if (TREE_CODE (arg0) == BIT_NOT_EXPR
	  && operand_equal_p (TREE_OPERAND (arg0, 0), arg1, 0))
	{
	  t1 = build_zero_cst (type);
	  t1 = fold_unary_loc (loc, BIT_NOT_EXPR, type, t1);
	  return omit_one_operand_loc (loc, type, t1, arg1);
	}

      /* X ^ ~X is -1.  */
      if (TREE_CODE (arg1) == BIT_NOT_EXPR
	  && operand_equal_p (arg0, TREE_OPERAND (arg1, 0), 0))
	{
	  t1 = build_zero_cst (type);
	  t1 = fold_unary_loc (loc, BIT_NOT_EXPR, type, t1);
	  return omit_one_operand_loc (loc, type, t1, arg0);
	}

      /* If we are XORing two BIT_AND_EXPR's, both of which are and'ing
         with a constant, and the two constants have no bits in common,
	 we should treat this as a BIT_IOR_EXPR since this may produce more
	 simplifications.  */
      if (TREE_CODE (arg0) == BIT_AND_EXPR
	  && TREE_CODE (arg1) == BIT_AND_EXPR
	  && TREE_CODE (TREE_OPERAND (arg0, 1)) == INTEGER_CST
	  && TREE_CODE (TREE_OPERAND (arg1, 1)) == INTEGER_CST
	  && integer_zerop (const_binop (BIT_AND_EXPR,
					 TREE_OPERAND (arg0, 1),
					 TREE_OPERAND (arg1, 1))))
	{
	  code = BIT_IOR_EXPR;
	  goto bit_ior;
	}

      /* (X | Y) ^ X -> Y & ~ X*/
      if (TREE_CODE (arg0) == BIT_IOR_EXPR
          && operand_equal_p (TREE_OPERAND (arg0, 0), arg1, 0))
        {
	  tree t2 = TREE_OPERAND (arg0, 1);
	  t1 = fold_build1_loc (loc, BIT_NOT_EXPR, TREE_TYPE (arg1),
			    arg1);
	  t1 = fold_build2_loc (loc, BIT_AND_EXPR, type,
			    fold_convert_loc (loc, type, t2),
			    fold_convert_loc (loc, type, t1));
	  return t1;
	}

      /* (Y | X) ^ X -> Y & ~ X*/
      if (TREE_CODE (arg0) == BIT_IOR_EXPR
          && operand_equal_p (TREE_OPERAND (arg0, 1), arg1, 0))
        {
	  tree t2 = TREE_OPERAND (arg0, 0);
	  t1 = fold_build1_loc (loc, BIT_NOT_EXPR, TREE_TYPE (arg1),
			    arg1);
	  t1 = fold_build2_loc (loc, BIT_AND_EXPR, type,
			    fold_convert_loc (loc, type, t2),
			    fold_convert_loc (loc, type, t1));
	  return t1;
	}

      /* X ^ (X | Y) -> Y & ~ X*/
      if (TREE_CODE (arg1) == BIT_IOR_EXPR
          && operand_equal_p (TREE_OPERAND (arg1, 0), arg0, 0))
        {
	  tree t2 = TREE_OPERAND (arg1, 1);
	  t1 = fold_build1_loc (loc, BIT_NOT_EXPR, TREE_TYPE (arg0),
			    arg0);
	  t1 = fold_build2_loc (loc, BIT_AND_EXPR, type,
			    fold_convert_loc (loc, type, t2),
			    fold_convert_loc (loc, type, t1));
	  return t1;
	}

      /* X ^ (Y | X) -> Y & ~ X*/
      if (TREE_CODE (arg1) == BIT_IOR_EXPR
          && operand_equal_p (TREE_OPERAND (arg1, 1), arg0, 0))
        {
	  tree t2 = TREE_OPERAND (arg1, 0);
	  t1 = fold_build1_loc (loc, BIT_NOT_EXPR, TREE_TYPE (arg0),
			    arg0);
	  t1 = fold_build2_loc (loc, BIT_AND_EXPR, type,
			    fold_convert_loc (loc, type, t2),
			    fold_convert_loc (loc, type, t1));
	  return t1;
	}

      /* Convert ~X ^ ~Y to X ^ Y.  */
      if (TREE_CODE (arg0) == BIT_NOT_EXPR
	  && TREE_CODE (arg1) == BIT_NOT_EXPR)
	return fold_build2_loc (loc, code, type,
			    fold_convert_loc (loc, type,
					      TREE_OPERAND (arg0, 0)),
			    fold_convert_loc (loc, type,
					      TREE_OPERAND (arg1, 0)));

      /* Convert ~X ^ C to X ^ ~C.  */
      if (TREE_CODE (arg0) == BIT_NOT_EXPR
	  && TREE_CODE (arg1) == INTEGER_CST)
	return fold_build2_loc (loc, code, type,
			    fold_convert_loc (loc, type,
					      TREE_OPERAND (arg0, 0)),
			    fold_build1_loc (loc, BIT_NOT_EXPR, type, arg1));

      /* Fold (X & 1) ^ 1 as (X & 1) == 0.  */
      if (TREE_CODE (arg0) == BIT_AND_EXPR
	  && integer_onep (TREE_OPERAND (arg0, 1))
	  && integer_onep (arg1))
	return fold_build2_loc (loc, EQ_EXPR, type, arg0,
				build_zero_cst (TREE_TYPE (arg0)));

      /* Fold (X & Y) ^ Y as ~X & Y.  */
      if (TREE_CODE (arg0) == BIT_AND_EXPR
	  && operand_equal_p (TREE_OPERAND (arg0, 1), arg1, 0))
	{
	  tem = fold_convert_loc (loc, type, TREE_OPERAND (arg0, 0));
	  return fold_build2_loc (loc, BIT_AND_EXPR, type,
			      fold_build1_loc (loc, BIT_NOT_EXPR, type, tem),
			      fold_convert_loc (loc, type, arg1));
	}
      /* Fold (X & Y) ^ X as ~Y & X.  */
      if (TREE_CODE (arg0) == BIT_AND_EXPR
	  && operand_equal_p (TREE_OPERAND (arg0, 0), arg1, 0)
	  && reorder_operands_p (TREE_OPERAND (arg0, 1), arg1))
	{
	  tem = fold_convert_loc (loc, type, TREE_OPERAND (arg0, 1));
	  return fold_build2_loc (loc, BIT_AND_EXPR, type,
			      fold_build1_loc (loc, BIT_NOT_EXPR, type, tem),
			      fold_convert_loc (loc, type, arg1));
	}
      /* Fold X ^ (X & Y) as X & ~Y.  */
      if (TREE_CODE (arg1) == BIT_AND_EXPR
	  && operand_equal_p (arg0, TREE_OPERAND (arg1, 0), 0))
	{
	  tem = fold_convert_loc (loc, type, TREE_OPERAND (arg1, 1));
	  return fold_build2_loc (loc, BIT_AND_EXPR, type,
			      fold_convert_loc (loc, type, arg0),
			      fold_build1_loc (loc, BIT_NOT_EXPR, type, tem));
	}
      /* Fold X ^ (Y & X) as ~Y & X.  */
      if (TREE_CODE (arg1) == BIT_AND_EXPR
	  && operand_equal_p (arg0, TREE_OPERAND (arg1, 1), 0)
	  && reorder_operands_p (arg0, TREE_OPERAND (arg1, 0)))
	{
	  tem = fold_convert_loc (loc, type, TREE_OPERAND (arg1, 0));
	  return fold_build2_loc (loc, BIT_AND_EXPR, type,
			      fold_build1_loc (loc, BIT_NOT_EXPR, type, tem),
			      fold_convert_loc (loc, type, arg0));
	}

      /* See if this can be simplified into a rotate first.  If that
	 is unsuccessful continue in the association code.  */
      goto bit_rotate;

    case BIT_AND_EXPR:
      if (integer_all_onesp (arg1))
	return non_lvalue_loc (loc, fold_convert_loc (loc, type, arg0));
      if (integer_zerop (arg1))
	return omit_one_operand_loc (loc, type, arg1, arg0);
      if (operand_equal_p (arg0, arg1, 0))
	return non_lvalue_loc (loc, fold_convert_loc (loc, type, arg0));

      /* ~X & X, (X == 0) & X, and !X & X are always zero.  */
      if ((TREE_CODE (arg0) == BIT_NOT_EXPR
	   || TREE_CODE (arg0) == TRUTH_NOT_EXPR
	   || (TREE_CODE (arg0) == EQ_EXPR
	       && integer_zerop (TREE_OPERAND (arg0, 1))))
	  && operand_equal_p (TREE_OPERAND (arg0, 0), arg1, 0))
	return omit_one_operand_loc (loc, type, integer_zero_node, arg1);

      /* X & ~X , X & (X == 0), and X & !X are always zero.  */
      if ((TREE_CODE (arg1) == BIT_NOT_EXPR
	   || TREE_CODE (arg1) == TRUTH_NOT_EXPR
	   || (TREE_CODE (arg1) == EQ_EXPR
	       && integer_zerop (TREE_OPERAND (arg1, 1))))
	  && operand_equal_p (arg0, TREE_OPERAND (arg1, 0), 0))
	return omit_one_operand_loc (loc, type, integer_zero_node, arg0);

      /* Canonicalize (X | C1) & C2 as (X & C2) | (C1 & C2).  */
      if (TREE_CODE (arg0) == BIT_IOR_EXPR
	  && TREE_CODE (arg1) == INTEGER_CST
	  && TREE_CODE (TREE_OPERAND (arg0, 1)) == INTEGER_CST)
	{
	  tree tmp1 = fold_convert_loc (loc, type, arg1);
	  tree tmp2 = fold_convert_loc (loc, type, TREE_OPERAND (arg0, 0));
	  tree tmp3 = fold_convert_loc (loc, type, TREE_OPERAND (arg0, 1));
	  tmp2 = fold_build2_loc (loc, BIT_AND_EXPR, type, tmp2, tmp1);
	  tmp3 = fold_build2_loc (loc, BIT_AND_EXPR, type, tmp3, tmp1);
	  return
	    fold_convert_loc (loc, type,
			      fold_build2_loc (loc, BIT_IOR_EXPR,
					   type, tmp2, tmp3));
	}

      /* (X | Y) & Y is (X, Y).  */
      if (TREE_CODE (arg0) == BIT_IOR_EXPR
	  && operand_equal_p (TREE_OPERAND (arg0, 1), arg1, 0))
	return omit_one_operand_loc (loc, type, arg1, TREE_OPERAND (arg0, 0));
      /* (X | Y) & X is (Y, X).  */
      if (TREE_CODE (arg0) == BIT_IOR_EXPR
	  && operand_equal_p (TREE_OPERAND (arg0, 0), arg1, 0)
	  && reorder_operands_p (TREE_OPERAND (arg0, 1), arg1))
	return omit_one_operand_loc (loc, type, arg1, TREE_OPERAND (arg0, 1));
      /* X & (X | Y) is (Y, X).  */
      if (TREE_CODE (arg1) == BIT_IOR_EXPR
	  && operand_equal_p (arg0, TREE_OPERAND (arg1, 0), 0)
	  && reorder_operands_p (arg0, TREE_OPERAND (arg1, 1)))
	return omit_one_operand_loc (loc, type, arg0, TREE_OPERAND (arg1, 1));
      /* X & (Y | X) is (Y, X).  */
      if (TREE_CODE (arg1) == BIT_IOR_EXPR
	  && operand_equal_p (arg0, TREE_OPERAND (arg1, 1), 0)
	  && reorder_operands_p (arg0, TREE_OPERAND (arg1, 0)))
	return omit_one_operand_loc (loc, type, arg0, TREE_OPERAND (arg1, 0));

      /* Fold (X ^ 1) & 1 as (X & 1) == 0.  */
      if (TREE_CODE (arg0) == BIT_XOR_EXPR
	  && integer_onep (TREE_OPERAND (arg0, 1))
	  && integer_onep (arg1))
	{
	  tree tem2;
	  tem = TREE_OPERAND (arg0, 0);
	  tem2 = fold_convert_loc (loc, TREE_TYPE (tem), arg1);
	  tem2 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (tem),
				  tem, tem2);
	  return fold_build2_loc (loc, EQ_EXPR, type, tem2,
				  build_zero_cst (TREE_TYPE (tem)));
	}
      /* Fold ~X & 1 as (X & 1) == 0.  */
      if (TREE_CODE (arg0) == BIT_NOT_EXPR
	  && integer_onep (arg1))
	{
	  tree tem2;
	  tem = TREE_OPERAND (arg0, 0);
	  tem2 = fold_convert_loc (loc, TREE_TYPE (tem), arg1);
	  tem2 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (tem),
				  tem, tem2);
	  return fold_build2_loc (loc, EQ_EXPR, type, tem2,
				  build_zero_cst (TREE_TYPE (tem)));
	}
      /* Fold !X & 1 as X == 0.  */
      if (TREE_CODE (arg0) == TRUTH_NOT_EXPR
	  && integer_onep (arg1))
	{
	  tem = TREE_OPERAND (arg0, 0);
	  return fold_build2_loc (loc, EQ_EXPR, type, tem,
				  build_zero_cst (TREE_TYPE (tem)));
	}

      /* Fold (X ^ Y) & Y as ~X & Y.  */
      if (TREE_CODE (arg0) == BIT_XOR_EXPR
	  && operand_equal_p (TREE_OPERAND (arg0, 1), arg1, 0))
	{
	  tem = fold_convert_loc (loc, type, TREE_OPERAND (arg0, 0));
	  return fold_build2_loc (loc, BIT_AND_EXPR, type,
			      fold_build1_loc (loc, BIT_NOT_EXPR, type, tem),
			      fold_convert_loc (loc, type, arg1));
	}
      /* Fold (X ^ Y) & X as ~Y & X.  */
      if (TREE_CODE (arg0) == BIT_XOR_EXPR
	  && operand_equal_p (TREE_OPERAND (arg0, 0), arg1, 0)
	  && reorder_operands_p (TREE_OPERAND (arg0, 1), arg1))
	{
	  tem = fold_convert_loc (loc, type, TREE_OPERAND (arg0, 1));
	  return fold_build2_loc (loc, BIT_AND_EXPR, type,
			      fold_build1_loc (loc, BIT_NOT_EXPR, type, tem),
			      fold_convert_loc (loc, type, arg1));
	}
      /* Fold X & (X ^ Y) as X & ~Y.  */
      if (TREE_CODE (arg1) == BIT_XOR_EXPR
	  && operand_equal_p (arg0, TREE_OPERAND (arg1, 0), 0))
	{
	  tem = fold_convert_loc (loc, type, TREE_OPERAND (arg1, 1));
	  return fold_build2_loc (loc, BIT_AND_EXPR, type,
			      fold_convert_loc (loc, type, arg0),
			      fold_build1_loc (loc, BIT_NOT_EXPR, type, tem));
	}
      /* Fold X & (Y ^ X) as ~Y & X.  */
      if (TREE_CODE (arg1) == BIT_XOR_EXPR
	  && operand_equal_p (arg0, TREE_OPERAND (arg1, 1), 0)
	  && reorder_operands_p (arg0, TREE_OPERAND (arg1, 0)))
	{
	  tem = fold_convert_loc (loc, type, TREE_OPERAND (arg1, 0));
	  return fold_build2_loc (loc, BIT_AND_EXPR, type,
			      fold_build1_loc (loc, BIT_NOT_EXPR, type, tem),
			      fold_convert_loc (loc, type, arg0));
	}

      /* Fold (X * Y) & -(1 << CST) to X * Y if Y is a constant
         multiple of 1 << CST.  */
      if (TREE_CODE (arg1) == INTEGER_CST)
	{
	  double_int cst1 = tree_to_double_int (arg1);
	  double_int ncst1 = (-cst1).ext (TYPE_PRECISION (TREE_TYPE (arg1)),
					  TYPE_UNSIGNED (TREE_TYPE (arg1)));
	  if ((cst1 & ncst1) == ncst1
	      && multiple_of_p (type, arg0,
				double_int_to_tree (TREE_TYPE (arg1), ncst1)))
	    return fold_convert_loc (loc, type, arg0);
	}

      /* Fold (X * CST1) & CST2 to zero if we can, or drop known zero
         bits from CST2.  */
      if (TREE_CODE (arg1) == INTEGER_CST
	  && TREE_CODE (arg0) == MULT_EXPR
	  && TREE_CODE (TREE_OPERAND (arg0, 1)) == INTEGER_CST)
	{
	  double_int masked
	    = mask_with_tz (type, tree_to_double_int (arg1),
	                    tree_to_double_int (TREE_OPERAND (arg0, 1)));

	  if (masked.is_zero ())
	    return omit_two_operands_loc (loc, type, build_zero_cst (type),
	                                  arg0, arg1);
	  else if (masked != tree_to_double_int (arg1))
	    return fold_build2_loc (loc, code, type, op0,
	                            double_int_to_tree (type, masked));
	}

      /* For constants M and N, if M == (1LL << cst) - 1 && (N & M) == M,
	 ((A & N) + B) & M -> (A + B) & M
	 Similarly if (N & M) == 0,
	 ((A | N) + B) & M -> (A + B) & M
	 and for - instead of + (or unary - instead of +)
	 and/or ^ instead of |.
	 If B is constant and (B & M) == 0, fold into A & M.  */
      if (host_integerp (arg1, 1))
	{
	  unsigned HOST_WIDE_INT cst1 = tree_low_cst (arg1, 1);
	  if (~cst1 && (cst1 & (cst1 + 1)) == 0
	      && INTEGRAL_TYPE_P (TREE_TYPE (arg0))
	      && (TREE_CODE (arg0) == PLUS_EXPR
		  || TREE_CODE (arg0) == MINUS_EXPR
		  || TREE_CODE (arg0) == NEGATE_EXPR)
	      && (TYPE_OVERFLOW_WRAPS (TREE_TYPE (arg0))
		  || TREE_CODE (TREE_TYPE (arg0)) == INTEGER_TYPE))
	    {
	      tree pmop[2];
	      int which = 0;
	      unsigned HOST_WIDE_INT cst0;

	      /* Now we know that arg0 is (C + D) or (C - D) or
		 -C and arg1 (M) is == (1LL << cst) - 1.
		 Store C into PMOP[0] and D into PMOP[1].  */
	      pmop[0] = TREE_OPERAND (arg0, 0);
	      pmop[1] = NULL;
	      if (TREE_CODE (arg0) != NEGATE_EXPR)
		{
		  pmop[1] = TREE_OPERAND (arg0, 1);
		  which = 1;
		}

	      if (!host_integerp (TYPE_MAX_VALUE (TREE_TYPE (arg0)), 1)
		  || (tree_low_cst (TYPE_MAX_VALUE (TREE_TYPE (arg0)), 1)
		      & cst1) != cst1)
		which = -1;

	      for (; which >= 0; which--)
		switch (TREE_CODE (pmop[which]))
		  {
		  case BIT_AND_EXPR:
		  case BIT_IOR_EXPR:
		  case BIT_XOR_EXPR:
		    if (TREE_CODE (TREE_OPERAND (pmop[which], 1))
			!= INTEGER_CST)
		      break;
		    /* tree_low_cst not used, because we don't care about
		       the upper bits.  */
		    cst0 = TREE_INT_CST_LOW (TREE_OPERAND (pmop[which], 1));
		    cst0 &= cst1;
		    if (TREE_CODE (pmop[which]) == BIT_AND_EXPR)
		      {
			if (cst0 != cst1)
			  break;
		      }
		    else if (cst0 != 0)
		      break;
		    /* If C or D is of the form (A & N) where
		       (N & M) == M, or of the form (A | N) or
		       (A ^ N) where (N & M) == 0, replace it with A.  */
		    pmop[which] = TREE_OPERAND (pmop[which], 0);
		    break;
		  case INTEGER_CST:
		    /* If C or D is a N where (N & M) == 0, it can be
		       omitted (assumed 0).  */
		    if ((TREE_CODE (arg0) == PLUS_EXPR
			 || (TREE_CODE (arg0) == MINUS_EXPR && which == 0))
			&& (TREE_INT_CST_LOW (pmop[which]) & cst1) == 0)
		      pmop[which] = NULL;
		    break;
		  default:
		    break;
		  }

	      /* Only build anything new if we optimized one or both arguments
		 above.  */
	      if (pmop[0] != TREE_OPERAND (arg0, 0)
		  || (TREE_CODE (arg0) != NEGATE_EXPR
		      && pmop[1] != TREE_OPERAND (arg0, 1)))
		{
		  tree utype = TREE_TYPE (arg0);
		  if (! TYPE_OVERFLOW_WRAPS (TREE_TYPE (arg0)))
		    {
		      /* Perform the operations in a type that has defined
			 overflow behavior.  */
		      utype = unsigned_type_for (TREE_TYPE (arg0));
		      if (pmop[0] != NULL)
			pmop[0] = fold_convert_loc (loc, utype, pmop[0]);
		      if (pmop[1] != NULL)
			pmop[1] = fold_convert_loc (loc, utype, pmop[1]);
		    }

		  if (TREE_CODE (arg0) == NEGATE_EXPR)
		    tem = fold_build1_loc (loc, NEGATE_EXPR, utype, pmop[0]);
		  else if (TREE_CODE (arg0) == PLUS_EXPR)
		    {
		      if (pmop[0] != NULL && pmop[1] != NULL)
			tem = fold_build2_loc (loc, PLUS_EXPR, utype,
					       pmop[0], pmop[1]);
		      else if (pmop[0] != NULL)
			tem = pmop[0];
		      else if (pmop[1] != NULL)
			tem = pmop[1];
		      else
			return build_int_cst (type, 0);
		    }
		  else if (pmop[0] == NULL)
		    tem = fold_build1_loc (loc, NEGATE_EXPR, utype, pmop[1]);
		  else
		    tem = fold_build2_loc (loc, MINUS_EXPR, utype,
					   pmop[0], pmop[1]);
		  /* TEM is now the new binary +, - or unary - replacement.  */
		  tem = fold_build2_loc (loc, BIT_AND_EXPR, utype, tem,
					 fold_convert_loc (loc, utype, arg1));
		  return fold_convert_loc (loc, type, tem);
		}
	    }
	}

      t1 = distribute_bit_expr (loc, code, type, arg0, arg1);
      if (t1 != NULL_TREE)
	return t1;
      /* Simplify ((int)c & 0377) into (int)c, if c is unsigned char.  */
      if (TREE_CODE (arg1) == INTEGER_CST && TREE_CODE (arg0) == NOP_EXPR
	  && TYPE_UNSIGNED (TREE_TYPE (TREE_OPERAND (arg0, 0))))
	{
	  prec = TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (arg0, 0)));

	  if (prec < BITS_PER_WORD && prec < HOST_BITS_PER_WIDE_INT
	      && (~TREE_INT_CST_LOW (arg1)
		  & (((HOST_WIDE_INT) 1 << prec) - 1)) == 0)
	    return
	      fold_convert_loc (loc, type, TREE_OPERAND (arg0, 0));
	}

      /* Convert (and (not arg0) (not arg1)) to (not (or (arg0) (arg1))).

	 This results in more efficient code for machines without a NOR
	 instruction.  Combine will canonicalize to the first form
	 which will allow use of NOR instructions provided by the
	 backend if they exist.  */
      if (TREE_CODE (arg0) == BIT_NOT_EXPR
	  && TREE_CODE (arg1) == BIT_NOT_EXPR)
	{
	  return fold_build1_loc (loc, BIT_NOT_EXPR, type,
			      build2 (BIT_IOR_EXPR, type,
				      fold_convert_loc (loc, type,
							TREE_OPERAND (arg0, 0)),
				      fold_convert_loc (loc, type,
							TREE_OPERAND (arg1, 0))));
	}

      /* If arg0 is derived from the address of an object or function, we may
	 be able to fold this expression using the object or function's
	 alignment.  */
      if (POINTER_TYPE_P (TREE_TYPE (arg0)) && host_integerp (arg1, 1))
	{
	  unsigned HOST_WIDE_INT modulus, residue;
	  unsigned HOST_WIDE_INT low = TREE_INT_CST_LOW (arg1);

	  modulus = get_pointer_modulus_and_residue (arg0, &residue,
						     integer_onep (arg1));

	  /* This works because modulus is a power of 2.  If this weren't the
	     case, we'd have to replace it by its greatest power-of-2
	     divisor: modulus & -modulus.  */
	  if (low < modulus)
	    return build_int_cst (type, residue & low);
	}

      /* Fold (X << C1) & C2 into (X << C1) & (C2 | ((1 << C1) - 1))
	      (X >> C1) & C2 into (X >> C1) & (C2 | ~((type) -1 >> C1))
	 if the new mask might be further optimized.  */
      if ((TREE_CODE (arg0) == LSHIFT_EXPR
	   || TREE_CODE (arg0) == RSHIFT_EXPR)
	  && host_integerp (TREE_OPERAND (arg0, 1), 1)
	  && host_integerp (arg1, TYPE_UNSIGNED (TREE_TYPE (arg1)))
	  && tree_low_cst (TREE_OPERAND (arg0, 1), 1)
	     < TYPE_PRECISION (TREE_TYPE (arg0))
	  && TYPE_PRECISION (TREE_TYPE (arg0)) <= HOST_BITS_PER_WIDE_INT
	  && tree_low_cst (TREE_OPERAND (arg0, 1), 1) > 0)
	{
	  unsigned int shiftc = tree_low_cst (TREE_OPERAND (arg0, 1), 1);
	  unsigned HOST_WIDE_INT mask
	    = tree_low_cst (arg1, TYPE_UNSIGNED (TREE_TYPE (arg1)));
	  unsigned HOST_WIDE_INT newmask, zerobits = 0;
	  tree shift_type = TREE_TYPE (arg0);

	  if (TREE_CODE (arg0) == LSHIFT_EXPR)
	    zerobits = ((((unsigned HOST_WIDE_INT) 1) << shiftc) - 1);
	  else if (TREE_CODE (arg0) == RSHIFT_EXPR
		   && TYPE_PRECISION (TREE_TYPE (arg0))
		      == GET_MODE_BITSIZE (TYPE_MODE (TREE_TYPE (arg0))))
	    {
	      prec = TYPE_PRECISION (TREE_TYPE (arg0));
	      tree arg00 = TREE_OPERAND (arg0, 0);
	      /* See if more bits can be proven as zero because of
		 zero extension.  */
	      if (TREE_CODE (arg00) == NOP_EXPR
		  && TYPE_UNSIGNED (TREE_TYPE (TREE_OPERAND (arg00, 0))))
		{
		  tree inner_type = TREE_TYPE (TREE_OPERAND (arg00, 0));
		  if (TYPE_PRECISION (inner_type)
		      == GET_MODE_BITSIZE (TYPE_MODE (inner_type))
		      && TYPE_PRECISION (inner_type) < prec)
		    {
		      prec = TYPE_PRECISION (inner_type);
		      /* See if we can shorten the right shift.  */
		      if (shiftc < prec)
			shift_type = inner_type;
		    }
		}
	      zerobits = ~(unsigned HOST_WIDE_INT) 0;
	      zerobits >>= HOST_BITS_PER_WIDE_INT - shiftc;
	      zerobits <<= prec - shiftc;
	      /* For arithmetic shift if sign bit could be set, zerobits
		 can contain actually sign bits, so no transformation is
		 possible, unless MASK masks them all away.  In that
		 case the shift needs to be converted into logical shift.  */
	      if (!TYPE_UNSIGNED (TREE_TYPE (arg0))
		  && prec == TYPE_PRECISION (TREE_TYPE (arg0)))
		{
		  if ((mask & zerobits) == 0)
		    shift_type = unsigned_type_for (TREE_TYPE (arg0));
		  else
		    zerobits = 0;
		}
	    }

	  /* ((X << 16) & 0xff00) is (X, 0).  */
	  if ((mask & zerobits) == mask)
	    return omit_one_operand_loc (loc, type,
				     build_int_cst (type, 0), arg0);

	  newmask = mask | zerobits;
	  if (newmask != mask && (newmask & (newmask + 1)) == 0)
	    {
	      /* Only do the transformation if NEWMASK is some integer
		 mode's mask.  */
	      for (prec = BITS_PER_UNIT;
		   prec < HOST_BITS_PER_WIDE_INT; prec <<= 1)
		if (newmask == (((unsigned HOST_WIDE_INT) 1) << prec) - 1)
		  break;
	      if (prec < HOST_BITS_PER_WIDE_INT
		  || newmask == ~(unsigned HOST_WIDE_INT) 0)
		{
		  tree newmaskt;

		  if (shift_type != TREE_TYPE (arg0))
		    {
		      tem = fold_build2_loc (loc, TREE_CODE (arg0), shift_type,
					 fold_convert_loc (loc, shift_type,
							   TREE_OPERAND (arg0, 0)),
					 TREE_OPERAND (arg0, 1));
		      tem = fold_convert_loc (loc, type, tem);
		    }
		  else
		    tem = op0;
		  newmaskt = build_int_cst_type (TREE_TYPE (op1), newmask);
		  if (!tree_int_cst_equal (newmaskt, arg1))
		    return fold_build2_loc (loc, BIT_AND_EXPR, type, tem, newmaskt);
		}
	    }
	}

      goto associate;

    case RDIV_EXPR:
      /* Don't touch a floating-point divide by zero unless the mode
	 of the constant can represent infinity.  */
      if (TREE_CODE (arg1) == REAL_CST
	  && !MODE_HAS_INFINITIES (TYPE_MODE (TREE_TYPE (arg1)))
	  && real_zerop (arg1))
	return NULL_TREE;

      /* Optimize A / A to 1.0 if we don't care about
	 NaNs or Infinities.  Skip the transformation
	 for non-real operands.  */
      if (SCALAR_FLOAT_TYPE_P (TREE_TYPE (arg0))
	  && ! HONOR_NANS (TYPE_MODE (TREE_TYPE (arg0)))
	  && ! HONOR_INFINITIES (TYPE_MODE (TREE_TYPE (arg0)))
	  && operand_equal_p (arg0, arg1, 0))
	{
	  tree r = build_real (TREE_TYPE (arg0), dconst1);

	  return omit_two_operands_loc (loc, type, r, arg0, arg1);
	}

      /* The complex version of the above A / A optimization.  */
      if (COMPLEX_FLOAT_TYPE_P (TREE_TYPE (arg0))
	  && operand_equal_p (arg0, arg1, 0))
	{
	  tree elem_type = TREE_TYPE (TREE_TYPE (arg0));
	  if (! HONOR_NANS (TYPE_MODE (elem_type))
	      && ! HONOR_INFINITIES (TYPE_MODE (elem_type)))
	    {
	      tree r = build_real (elem_type, dconst1);
	      /* omit_two_operands will call fold_convert for us.  */
	      return omit_two_operands_loc (loc, type, r, arg0, arg1);
	    }
	}

      /* (-A) / (-B) -> A / B  */
      if (TREE_CODE (arg0) == NEGATE_EXPR && negate_expr_p (arg1))
	return fold_build2_loc (loc, RDIV_EXPR, type,
			    TREE_OPERAND (arg0, 0),
			    negate_expr (arg1));
      if (TREE_CODE (arg1) == NEGATE_EXPR && negate_expr_p (arg0))
	return fold_build2_loc (loc, RDIV_EXPR, type,
			    negate_expr (arg0),
			    TREE_OPERAND (arg1, 0));

      /* In IEEE floating point, x/1 is not equivalent to x for snans.  */
      if (!HONOR_SNANS (TYPE_MODE (TREE_TYPE (arg0)))
	  && real_onep (arg1))
	return non_lvalue_loc (loc, fold_convert_loc (loc, type, arg0));

      /* In IEEE floating point, x/-1 is not equivalent to -x for snans.  */
      if (!HONOR_SNANS (TYPE_MODE (TREE_TYPE (arg0)))
	  && real_minus_onep (arg1))
	return non_lvalue_loc (loc, fold_convert_loc (loc, type,
						  negate_expr (arg0)));

      /* If ARG1 is a constant, we can convert this to a multiply by the
	 reciprocal.  This does not have the same rounding properties,
	 so only do this if -freciprocal-math.  We can actually
	 always safely do it if ARG1 is a power of two, but it's hard to
	 tell if it is or not in a portable manner.  */
      if (optimize
	  && (TREE_CODE (arg1) == REAL_CST
	      || (TREE_CODE (arg1) == COMPLEX_CST
		  && COMPLEX_FLOAT_TYPE_P (TREE_TYPE (arg1)))
	      || (TREE_CODE (arg1) == VECTOR_CST
		  && VECTOR_FLOAT_TYPE_P (TREE_TYPE (arg1)))))
	{
	  if (flag_reciprocal_math
	      && 0 != (tem = const_binop (code, build_one_cst (type), arg1)))
	    return fold_build2_loc (loc, MULT_EXPR, type, arg0, tem);
	  /* Find the reciprocal if optimizing and the result is exact.
	     TODO: Complex reciprocal not implemented.  */
	  if (TREE_CODE (arg1) != COMPLEX_CST)
	    {
	      tree inverse = exact_inverse (TREE_TYPE (arg0), arg1);

	      if (inverse)
		return fold_build2_loc (loc, MULT_EXPR, type, arg0, inverse);
	    }
	}
      /* Convert A/B/C to A/(B*C).  */
      if (flag_reciprocal_math
	  && TREE_CODE (arg0) == RDIV_EXPR)
	return fold_build2_loc (loc, RDIV_EXPR, type, TREE_OPERAND (arg0, 0),
			    fold_build2_loc (loc, MULT_EXPR, type,
					 TREE_OPERAND (arg0, 1), arg1));

      /* Convert A/(B/C) to (A/B)*C.  */
      if (flag_reciprocal_math
	  && TREE_CODE (arg1) == RDIV_EXPR)
	return fold_build2_loc (loc, MULT_EXPR, type,
			    fold_build2_loc (loc, RDIV_EXPR, type, arg0,
					 TREE_OPERAND (arg1, 0)),
			    TREE_OPERAND (arg1, 1));

      /* Convert C1/(X*C2) into (C1/C2)/X.  */
      if (flag_reciprocal_math
	  && TREE_CODE (arg1) == MULT_EXPR
	  && TREE_CODE (arg0) == REAL_CST
	  && TREE_CODE (TREE_OPERAND (arg1, 1)) == REAL_CST)
	{
	  tree tem = const_binop (RDIV_EXPR, arg0,
				  TREE_OPERAND (arg1, 1));
	  if (tem)
	    return fold_build2_loc (loc, RDIV_EXPR, type, tem,
				TREE_OPERAND (arg1, 0));
	}

      if (flag_unsafe_math_optimizations)
	{
	  enum built_in_function fcode0 = builtin_mathfn_code (arg0);
	  enum built_in_function fcode1 = builtin_mathfn_code (arg1);

	  /* Optimize sin(x)/cos(x) as tan(x).  */
	  if (((fcode0 == BUILT_IN_SIN && fcode1 == BUILT_IN_COS)
	       || (fcode0 == BUILT_IN_SINF && fcode1 == BUILT_IN_COSF)
	       || (fcode0 == BUILT_IN_SINL && fcode1 == BUILT_IN_COSL))
	      && operand_equal_p (CALL_EXPR_ARG (arg0, 0),
				  CALL_EXPR_ARG (arg1, 0), 0))
	    {
	      tree tanfn = mathfn_built_in (type, BUILT_IN_TAN);

	      if (tanfn != NULL_TREE)
		return build_call_expr_loc (loc, tanfn, 1, CALL_EXPR_ARG (arg0, 0));
	    }

	  /* Optimize cos(x)/sin(x) as 1.0/tan(x).  */
	  if (((fcode0 == BUILT_IN_COS && fcode1 == BUILT_IN_SIN)
	       || (fcode0 == BUILT_IN_COSF && fcode1 == BUILT_IN_SINF)
	       || (fcode0 == BUILT_IN_COSL && fcode1 == BUILT_IN_SINL))
	      && operand_equal_p (CALL_EXPR_ARG (arg0, 0),
				  CALL_EXPR_ARG (arg1, 0), 0))
	    {
	      tree tanfn = mathfn_built_in (type, BUILT_IN_TAN);

	      if (tanfn != NULL_TREE)
		{
		  tree tmp = build_call_expr_loc (loc, tanfn, 1,
					      CALL_EXPR_ARG (arg0, 0));
		  return fold_build2_loc (loc, RDIV_EXPR, type,
				      build_real (type, dconst1), tmp);
		}
	    }

 	  /* Optimize sin(x)/tan(x) as cos(x) if we don't care about
	     NaNs or Infinities.  */
 	  if (((fcode0 == BUILT_IN_SIN && fcode1 == BUILT_IN_TAN)
 	       || (fcode0 == BUILT_IN_SINF && fcode1 == BUILT_IN_TANF)
 	       || (fcode0 == BUILT_IN_SINL && fcode1 == BUILT_IN_TANL)))
	    {
	      tree arg00 = CALL_EXPR_ARG (arg0, 0);
	      tree arg01 = CALL_EXPR_ARG (arg1, 0);

	      if (! HONOR_NANS (TYPE_MODE (TREE_TYPE (arg00)))
		  && ! HONOR_INFINITIES (TYPE_MODE (TREE_TYPE (arg00)))
		  && operand_equal_p (arg00, arg01, 0))
		{
		  tree cosfn = mathfn_built_in (type, BUILT_IN_COS);

		  if (cosfn != NULL_TREE)
		    return build_call_expr_loc (loc, cosfn, 1, arg00);
		}
	    }

 	  /* Optimize tan(x)/sin(x) as 1.0/cos(x) if we don't care about
	     NaNs or Infinities.  */
 	  if (((fcode0 == BUILT_IN_TAN && fcode1 == BUILT_IN_SIN)
 	       || (fcode0 == BUILT_IN_TANF && fcode1 == BUILT_IN_SINF)
 	       || (fcode0 == BUILT_IN_TANL && fcode1 == BUILT_IN_SINL)))
	    {
	      tree arg00 = CALL_EXPR_ARG (arg0, 0);
	      tree arg01 = CALL_EXPR_ARG (arg1, 0);

	      if (! HONOR_NANS (TYPE_MODE (TREE_TYPE (arg00)))
		  && ! HONOR_INFINITIES (TYPE_MODE (TREE_TYPE (arg00)))
		  && operand_equal_p (arg00, arg01, 0))
		{
		  tree cosfn = mathfn_built_in (type, BUILT_IN_COS);

		  if (cosfn != NULL_TREE)
		    {
		      tree tmp = build_call_expr_loc (loc, cosfn, 1, arg00);
		      return fold_build2_loc (loc, RDIV_EXPR, type,
					  build_real (type, dconst1),
					  tmp);
		    }
		}
	    }

	  /* Optimize pow(x,c)/x as pow(x,c-1).  */
	  if (fcode0 == BUILT_IN_POW
	      || fcode0 == BUILT_IN_POWF
	      || fcode0 == BUILT_IN_POWL)
	    {
	      tree arg00 = CALL_EXPR_ARG (arg0, 0);
	      tree arg01 = CALL_EXPR_ARG (arg0, 1);
	      if (TREE_CODE (arg01) == REAL_CST
		  && !TREE_OVERFLOW (arg01)
		  && operand_equal_p (arg1, arg00, 0))
		{
		  tree powfn = TREE_OPERAND (CALL_EXPR_FN (arg0), 0);
		  REAL_VALUE_TYPE c;
		  tree arg;

		  c = TREE_REAL_CST (arg01);
		  real_arithmetic (&c, MINUS_EXPR, &c, &dconst1);
		  arg = build_real (type, c);
		  return build_call_expr_loc (loc, powfn, 2, arg1, arg);
		}
	    }

	  /* Optimize a/root(b/c) into a*root(c/b).  */
	  if (BUILTIN_ROOT_P (fcode1))
	    {
	      tree rootarg = CALL_EXPR_ARG (arg1, 0);

	      if (TREE_CODE (rootarg) == RDIV_EXPR)
		{
		  tree rootfn = TREE_OPERAND (CALL_EXPR_FN (arg1), 0);
		  tree b = TREE_OPERAND (rootarg, 0);
		  tree c = TREE_OPERAND (rootarg, 1);

		  tree tmp = fold_build2_loc (loc, RDIV_EXPR, type, c, b);

		  tmp = build_call_expr_loc (loc, rootfn, 1, tmp);
		  return fold_build2_loc (loc, MULT_EXPR, type, arg0, tmp);
		}
	    }

	  /* Optimize x/expN(y) into x*expN(-y).  */
	  if (BUILTIN_EXPONENT_P (fcode1))
	    {
	      tree expfn = TREE_OPERAND (CALL_EXPR_FN (arg1), 0);
	      tree arg = negate_expr (CALL_EXPR_ARG (arg1, 0));
	      arg1 = build_call_expr_loc (loc,
				      expfn, 1,
				      fold_convert_loc (loc, type, arg));
	      return fold_build2_loc (loc, MULT_EXPR, type, arg0, arg1);
	    }

	  /* Optimize x/pow(y,z) into x*pow(y,-z).  */
	  if (fcode1 == BUILT_IN_POW
	      || fcode1 == BUILT_IN_POWF
	      || fcode1 == BUILT_IN_POWL)
	    {
	      tree powfn = TREE_OPERAND (CALL_EXPR_FN (arg1), 0);
	      tree arg10 = CALL_EXPR_ARG (arg1, 0);
	      tree arg11 = CALL_EXPR_ARG (arg1, 1);
	      tree neg11 = fold_convert_loc (loc, type,
					     negate_expr (arg11));
	      arg1 = build_call_expr_loc (loc, powfn, 2, arg10, neg11);
	      return fold_build2_loc (loc, MULT_EXPR, type, arg0, arg1);
	    }
	}
      return NULL_TREE;

    case TRUNC_DIV_EXPR:
      /* Optimize (X & (-A)) / A where A is a power of 2,
	 to X >> log2(A) */
      if (TREE_CODE (arg0) == BIT_AND_EXPR
	  && !TYPE_UNSIGNED (type) && TREE_CODE (arg1) == INTEGER_CST
	  && integer_pow2p (arg1) && tree_int_cst_sgn (arg1) > 0)
	{
	  tree sum = fold_binary_loc (loc, PLUS_EXPR, TREE_TYPE (arg1),
				      arg1, TREE_OPERAND (arg0, 1));
	  if (sum && integer_zerop (sum)) {
	    unsigned long pow2;

	    if (TREE_INT_CST_LOW (arg1))
	      pow2 = exact_log2 (TREE_INT_CST_LOW (arg1));
	    else
	      pow2 = exact_log2 (TREE_INT_CST_HIGH (arg1))
		      + HOST_BITS_PER_WIDE_INT;

	    return fold_build2_loc (loc, RSHIFT_EXPR, type,
			  TREE_OPERAND (arg0, 0),
			  build_int_cst (integer_type_node, pow2));
	  }
	}

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
	      unsigned long pow2;

	      if (TREE_INT_CST_LOW (sval))
		pow2 = exact_log2 (TREE_INT_CST_LOW (sval));
	      else
		pow2 = exact_log2 (TREE_INT_CST_HIGH (sval))
		       + HOST_BITS_PER_WIDE_INT;

	      if (strict_overflow_p)
		fold_overflow_warning (("assuming signed overflow does not "
					"occur when simplifying A / (B << N)"),
				       WARN_STRICT_OVERFLOW_MISC);

	      sh_cnt = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (sh_cnt),
					sh_cnt,
					build_int_cst (TREE_TYPE (sh_cnt),
						       pow2));
	      return fold_build2_loc (loc, RSHIFT_EXPR, type,
				  fold_convert_loc (loc, type, arg0), sh_cnt);
	    }
	}

      /* For unsigned integral types, FLOOR_DIV_EXPR is the same as
	 TRUNC_DIV_EXPR.  Rewrite into the latter in this case.  */
      if (INTEGRAL_TYPE_P (type)
	  && TYPE_UNSIGNED (type)
	  && code == FLOOR_DIV_EXPR)
	return fold_build2_loc (loc, TRUNC_DIV_EXPR, type, op0, op1);

      /* Fall through */

    case ROUND_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case EXACT_DIV_EXPR:
      if (integer_onep (arg1))
	return non_lvalue_loc (loc, fold_convert_loc (loc, type, arg0));
      if (integer_zerop (arg1))
	return NULL_TREE;
      /* X / -1 is -X.  */
      if (!TYPE_UNSIGNED (type)
	  && TREE_CODE (arg1) == INTEGER_CST
	  && TREE_INT_CST_LOW (arg1) == HOST_WIDE_INT_M1U
	  && TREE_INT_CST_HIGH (arg1) == -1)
	return fold_convert_loc (loc, type, negate_expr (arg0));

      /* Convert -A / -B to A / B when the type is signed and overflow is
	 undefined.  */
      if ((!INTEGRAL_TYPE_P (type) || TYPE_OVERFLOW_UNDEFINED (type))
	  && TREE_CODE (arg0) == NEGATE_EXPR
	  && negate_expr_p (arg1))
	{
	  if (INTEGRAL_TYPE_P (type))
	    fold_overflow_warning (("assuming signed overflow does not occur "
				    "when distributing negation across "
				    "division"),
				   WARN_STRICT_OVERFLOW_MISC);
	  return fold_build2_loc (loc, code, type,
			      fold_convert_loc (loc, type,
						TREE_OPERAND (arg0, 0)),
			      fold_convert_loc (loc, type,
						negate_expr (arg1)));
	}
      if ((!INTEGRAL_TYPE_P (type) || TYPE_OVERFLOW_UNDEFINED (type))
	  && TREE_CODE (arg1) == NEGATE_EXPR
	  && negate_expr_p (arg0))
	{
	  if (INTEGRAL_TYPE_P (type))
	    fold_overflow_warning (("assuming signed overflow does not occur "
				    "when distributing negation across "
				    "division"),
				   WARN_STRICT_OVERFLOW_MISC);
	  return fold_build2_loc (loc, code, type,
			      fold_convert_loc (loc, type,
						negate_expr (arg0)),
			      fold_convert_loc (loc, type,
						TREE_OPERAND (arg1, 0)));
	}

      /* If arg0 is a multiple of arg1, then rewrite to the fastest div
	 operation, EXACT_DIV_EXPR.

	 Note that only CEIL_DIV_EXPR and FLOOR_DIV_EXPR are rewritten now.
	 At one time others generated faster code, it's not clear if they do
	 after the last round to changes to the DIV code in expmed.c.  */
      if ((code == CEIL_DIV_EXPR || code == FLOOR_DIV_EXPR)
	  && multiple_of_p (type, arg0, arg1))
	return fold_build2_loc (loc, EXACT_DIV_EXPR, type, arg0, arg1);

      strict_overflow_p = false;
      if (TREE_CODE (arg1) == INTEGER_CST
	  && 0 != (tem = extract_muldiv (op0, arg1, code, NULL_TREE,
					 &strict_overflow_p)))
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
      /* X % 1 is always zero, but be sure to preserve any side
	 effects in X.  */
      if (integer_onep (arg1))
	return omit_one_operand_loc (loc, type, integer_zero_node, arg0);

      /* X % 0, return X % 0 unchanged so that we can get the
	 proper warnings and errors.  */
      if (integer_zerop (arg1))
	return NULL_TREE;

      /* 0 % X is always zero, but be sure to preserve any side
	 effects in X.  Place this after checking for X == 0.  */
      if (integer_zerop (arg0))
	return omit_one_operand_loc (loc, type, integer_zero_node, arg1);

      /* X % -1 is zero.  */
      if (!TYPE_UNSIGNED (type)
	  && TREE_CODE (arg1) == INTEGER_CST
	  && TREE_INT_CST_LOW (arg1) == HOST_WIDE_INT_M1U
	  && TREE_INT_CST_HIGH (arg1) == -1)
	return omit_one_operand_loc (loc, type, integer_zero_node, arg0);

      /* X % -C is the same as X % C.  */
      if (code == TRUNC_MOD_EXPR
	  && !TYPE_UNSIGNED (type)
	  && TREE_CODE (arg1) == INTEGER_CST
	  && !TREE_OVERFLOW (arg1)
	  && TREE_INT_CST_HIGH (arg1) < 0
	  && !TYPE_OVERFLOW_TRAPS (type)
	  /* Avoid this transformation if C is INT_MIN, i.e. C == -C.  */
	  && !sign_bit_p (arg1, arg1))
	return fold_build2_loc (loc, code, type,
			    fold_convert_loc (loc, type, arg0),
			    fold_convert_loc (loc, type,
					      negate_expr (arg1)));

      /* X % -Y is the same as X % Y.  */
      if (code == TRUNC_MOD_EXPR
	  && !TYPE_UNSIGNED (type)
	  && TREE_CODE (arg1) == NEGATE_EXPR
	  && !TYPE_OVERFLOW_TRAPS (type))
	return fold_build2_loc (loc, code, type, fold_convert_loc (loc, type, arg0),
			    fold_convert_loc (loc, type,
					      TREE_OPERAND (arg1, 0)));

      strict_overflow_p = false;
      if (TREE_CODE (arg1) == INTEGER_CST
	  && 0 != (tem = extract_muldiv (op0, arg1, code, NULL_TREE,
					 &strict_overflow_p)))
	{
	  if (strict_overflow_p)
	    fold_overflow_warning (("assuming signed overflow does not occur "
				    "when simplifying modulus"),
				   WARN_STRICT_OVERFLOW_MISC);
	  return fold_convert_loc (loc, type, tem);
	}

      /* Optimize TRUNC_MOD_EXPR by a power of two into a BIT_AND_EXPR,
         i.e. "X % C" into "X & (C - 1)", if X and C are positive.  */
      if ((code == TRUNC_MOD_EXPR || code == FLOOR_MOD_EXPR)
	  && (TYPE_UNSIGNED (type)
	      || tree_expr_nonnegative_warnv_p (op0, &strict_overflow_p)))
	{
	  tree c = arg1;
	  /* Also optimize A % (C << N)  where C is a power of 2,
	     to A & ((C << N) - 1).  */
	  if (TREE_CODE (arg1) == LSHIFT_EXPR)
	    c = TREE_OPERAND (arg1, 0);

	  if (integer_pow2p (c) && tree_int_cst_sgn (c) > 0)
	    {
	      tree mask
		= fold_build2_loc (loc, MINUS_EXPR, TREE_TYPE (arg1), arg1,
				   build_int_cst (TREE_TYPE (arg1), 1));
	      if (strict_overflow_p)
		fold_overflow_warning (("assuming signed overflow does not "
					"occur when simplifying "
					"X % (power of two)"),
				       WARN_STRICT_OVERFLOW_MISC);
	      return fold_build2_loc (loc, BIT_AND_EXPR, type,
				      fold_convert_loc (loc, type, arg0),
				      fold_convert_loc (loc, type, mask));
	    }
	}

      return NULL_TREE;

    case LROTATE_EXPR:
    case RROTATE_EXPR:
      if (integer_all_onesp (arg0))
	return omit_one_operand_loc (loc, type, arg0, arg1);
      goto shift;

    case RSHIFT_EXPR:
      /* Optimize -1 >> x for arithmetic right shifts.  */
      if (integer_all_onesp (arg0) && !TYPE_UNSIGNED (type)
	  && tree_expr_nonnegative_p (arg1))
	return omit_one_operand_loc (loc, type, arg0, arg1);
      /* ... fall through ...  */

    case LSHIFT_EXPR:
    shift:
      if (integer_zerop (arg1))
	return non_lvalue_loc (loc, fold_convert_loc (loc, type, arg0));
      if (integer_zerop (arg0))
	return omit_one_operand_loc (loc, type, arg0, arg1);

      /* Prefer vector1 << scalar to vector1 << vector2
	 if vector2 is uniform.  */
      if (VECTOR_TYPE_P (TREE_TYPE (arg1))
	  && (tem = uniform_vector_p (arg1)) != NULL_TREE)
	return fold_build2_loc (loc, code, type, op0, tem);

      /* Since negative shift count is not well-defined,
	 don't try to compute it in the compiler.  */
      if (TREE_CODE (arg1) == INTEGER_CST && tree_int_cst_sgn (arg1) < 0)
	return NULL_TREE;

      prec = element_precision (type);

      /* Turn (a OP c1) OP c2 into a OP (c1+c2).  */
      if (TREE_CODE (op0) == code && host_integerp (arg1, true)
	  && TREE_INT_CST_LOW (arg1) < prec
	  && host_integerp (TREE_OPERAND (arg0, 1), true)
	  && TREE_INT_CST_LOW (TREE_OPERAND (arg0, 1)) < prec)
	{
	  unsigned int low = (TREE_INT_CST_LOW (TREE_OPERAND (arg0, 1))
			      + TREE_INT_CST_LOW (arg1));

	  /* Deal with a OP (c1 + c2) being undefined but (a OP c1) OP c2
	     being well defined.  */
	  if (low >= prec)
	    {
	      if (code == LROTATE_EXPR || code == RROTATE_EXPR)
	        low = low % prec;
	      else if (TYPE_UNSIGNED (type) || code == LSHIFT_EXPR)
		return omit_one_operand_loc (loc, type, build_zero_cst (type),
					 TREE_OPERAND (arg0, 0));
	      else
		low = prec - 1;
	    }

	  return fold_build2_loc (loc, code, type, TREE_OPERAND (arg0, 0),
				  build_int_cst (TREE_TYPE (arg1), low));
	}

      /* Transform (x >> c) << c into x & (-1<<c), or transform (x << c) >> c
         into x & ((unsigned)-1 >> c) for unsigned types.  */
      if (((code == LSHIFT_EXPR && TREE_CODE (arg0) == RSHIFT_EXPR)
           || (TYPE_UNSIGNED (type)
	       && code == RSHIFT_EXPR && TREE_CODE (arg0) == LSHIFT_EXPR))
	  && host_integerp (arg1, false)
	  && TREE_INT_CST_LOW (arg1) < prec
	  && host_integerp (TREE_OPERAND (arg0, 1), false)
	  && TREE_INT_CST_LOW (TREE_OPERAND (arg0, 1)) < prec)
	{
	  HOST_WIDE_INT low0 = TREE_INT_CST_LOW (TREE_OPERAND (arg0, 1));
	  HOST_WIDE_INT low1 = TREE_INT_CST_LOW (arg1);
	  tree lshift;
	  tree arg00;

	  if (low0 == low1)
	    {
	      arg00 = fold_convert_loc (loc, type, TREE_OPERAND (arg0, 0));

	      lshift = build_minus_one_cst (type);
	      lshift = const_binop (code, lshift, arg1);

	      return fold_build2_loc (loc, BIT_AND_EXPR, type, arg00, lshift);
	    }
	}

      /* Rewrite an LROTATE_EXPR by a constant into an
	 RROTATE_EXPR by a new constant.  */
      if (code == LROTATE_EXPR && TREE_CODE (arg1) == INTEGER_CST)
	{
	  tree tem = build_int_cst (TREE_TYPE (arg1), prec);
	  tem = const_binop (MINUS_EXPR, tem, arg1);
	  return fold_build2_loc (loc, RROTATE_EXPR, type, op0, tem);
	}

      /* If we have a rotate of a bit operation with the rotate count and
	 the second operand of the bit operation both constant,
	 permute the two operations.  */
      if (code == RROTATE_EXPR && TREE_CODE (arg1) == INTEGER_CST
	  && (TREE_CODE (arg0) == BIT_AND_EXPR
	      || TREE_CODE (arg0) == BIT_IOR_EXPR
	      || TREE_CODE (arg0) == BIT_XOR_EXPR)
	  && TREE_CODE (TREE_OPERAND (arg0, 1)) == INTEGER_CST)
	return fold_build2_loc (loc, TREE_CODE (arg0), type,
			    fold_build2_loc (loc, code, type,
					 TREE_OPERAND (arg0, 0), arg1),
			    fold_build2_loc (loc, code, type,
					 TREE_OPERAND (arg0, 1), arg1));

      /* Two consecutive rotates adding up to the precision of the
	 type can be ignored.  */
      if (code == RROTATE_EXPR && TREE_CODE (arg1) == INTEGER_CST
	  && TREE_CODE (arg0) == RROTATE_EXPR
	  && TREE_CODE (TREE_OPERAND (arg0, 1)) == INTEGER_CST
	  && TREE_INT_CST_HIGH (arg1) == 0
	  && TREE_INT_CST_HIGH (TREE_OPERAND (arg0, 1)) == 0
	  && ((TREE_INT_CST_LOW (arg1)
	       + TREE_INT_CST_LOW (TREE_OPERAND (arg0, 1)))
	      == prec))
	return TREE_OPERAND (arg0, 0);

      /* Fold (X & C2) << C1 into (X << C1) & (C2 << C1)
	      (X & C2) >> C1 into (X >> C1) & (C2 >> C1)
	 if the latter can be further optimized.  */
      if ((code == LSHIFT_EXPR || code == RSHIFT_EXPR)
	  && TREE_CODE (arg0) == BIT_AND_EXPR
	  && TREE_CODE (arg1) == INTEGER_CST
	  && TREE_CODE (TREE_OPERAND (arg0, 1)) == INTEGER_CST)
	{
	  tree mask = fold_build2_loc (loc, code, type,
				   fold_convert_loc (loc, type,
						     TREE_OPERAND (arg0, 1)),
				   arg1);
	  tree shift = fold_build2_loc (loc, code, type,
				    fold_convert_loc (loc, type,
						      TREE_OPERAND (arg0, 0)),
				    arg1);
	  tem = fold_binary_loc (loc, BIT_AND_EXPR, type, shift, mask);
	  if (tem)
	    return tem;
	}

      return NULL_TREE;

    case MIN_EXPR:
      if (operand_equal_p (arg0, arg1, 0))
	return omit_one_operand_loc (loc, type, arg0, arg1);
      if (INTEGRAL_TYPE_P (type)
	  && operand_equal_p (arg1, TYPE_MIN_VALUE (type), OEP_ONLY_CONST))
	return omit_one_operand_loc (loc, type, arg1, arg0);
      tem = fold_minmax (loc, MIN_EXPR, type, arg0, arg1);
      if (tem)
	return tem;
      goto associate;

    case MAX_EXPR:
      if (operand_equal_p (arg0, arg1, 0))
	return omit_one_operand_loc (loc, type, arg0, arg1);
      if (INTEGRAL_TYPE_P (type)
	  && TYPE_MAX_VALUE (type)
	  && operand_equal_p (arg1, TYPE_MAX_VALUE (type), OEP_ONLY_CONST))
	return omit_one_operand_loc (loc, type, arg1, arg0);
      tem = fold_minmax (loc, MAX_EXPR, type, arg0, arg1);
      if (tem)
	return tem;
      goto associate;

    case TRUTH_ANDIF_EXPR:
      /* Note that the operands of this must be ints
	 and their values must be 0 or 1.
	 ("true" is a fixed value perhaps depending on the language.)  */
      /* If first arg is constant zero, return it.  */
      if (integer_zerop (arg0))
	return fold_convert_loc (loc, type, arg0);
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
	    return fold_build2_loc (loc, code, type, tem, arg1);

	  tem = fold_to_nonsharp_ineq_using_bound (loc, arg1, arg0);
	  if (tem && !operand_equal_p (tem, arg1, 0))
	    return fold_build2_loc (loc, code, type, arg0, tem);
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

      /* bool_var != 0 becomes bool_var. */
      if (TREE_CODE (TREE_TYPE (arg0)) == BOOLEAN_TYPE && integer_zerop (arg1)
          && code == NE_EXPR)
        return non_lvalue_loc (loc, fold_convert_loc (loc, type, arg0));

      /* bool_var == 1 becomes bool_var. */
      if (TREE_CODE (TREE_TYPE (arg0)) == BOOLEAN_TYPE && integer_onep (arg1)
          && code == EQ_EXPR)
        return non_lvalue_loc (loc, fold_convert_loc (loc, type, arg0));

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

      /* If this is an equality comparison of the address of two non-weak,
	 unaliased symbols neither of which are extern (since we do not
	 have access to attributes for externs), then we know the result.  */
      if (TREE_CODE (arg0) == ADDR_EXPR
	  && VAR_OR_FUNCTION_DECL_P (TREE_OPERAND (arg0, 0))
	  && ! DECL_WEAK (TREE_OPERAND (arg0, 0))
	  && ! lookup_attribute ("alias",
				 DECL_ATTRIBUTES (TREE_OPERAND (arg0, 0)))
	  && ! DECL_EXTERNAL (TREE_OPERAND (arg0, 0))
	  && TREE_CODE (arg1) == ADDR_EXPR
	  && VAR_OR_FUNCTION_DECL_P (TREE_OPERAND (arg1, 0))
	  && ! DECL_WEAK (TREE_OPERAND (arg1, 0))
	  && ! lookup_attribute ("alias",
				 DECL_ATTRIBUTES (TREE_OPERAND (arg1, 0)))
	  && ! DECL_EXTERNAL (TREE_OPERAND (arg1, 0)))
	{
	  /* We know that we're looking at the address of two
	     non-weak, unaliased, static _DECL nodes.

	     It is both wasteful and incorrect to call operand_equal_p
	     to compare the two ADDR_EXPR nodes.  It is wasteful in that
	     all we need to do is test pointer equality for the arguments
	     to the two ADDR_EXPR nodes.  It is incorrect to use
	     operand_equal_p as that function is NOT equivalent to a
	     C equality test.  It can in fact return false for two
	     objects which would test as equal using the C equality
	     operator.  */
	  bool equal = TREE_OPERAND (arg0, 0) == TREE_OPERAND (arg1, 0);
	  return constant_boolean_node (equal
				        ? code == EQ_EXPR : code != EQ_EXPR,
				        type);
	}

      /* If this is an EQ or NE comparison of a constant with a PLUS_EXPR or
	 a MINUS_EXPR of a constant, we can convert it into a comparison with
	 a revised constant as long as no overflow occurs.  */
      if (TREE_CODE (arg1) == INTEGER_CST
	  && (TREE_CODE (arg0) == PLUS_EXPR
	      || TREE_CODE (arg0) == MINUS_EXPR)
	  && TREE_CODE (TREE_OPERAND (arg0, 1)) == INTEGER_CST
	  && 0 != (tem = const_binop (TREE_CODE (arg0) == PLUS_EXPR
				      ? MINUS_EXPR : PLUS_EXPR,
				      fold_convert_loc (loc, TREE_TYPE (arg0),
							arg1),
				      TREE_OPERAND (arg0, 1)))
	  && !TREE_OVERFLOW (tem))
	return fold_build2_loc (loc, code, type, TREE_OPERAND (arg0, 0), tem);

      /* Similarly for a NEGATE_EXPR.  */
      if (TREE_CODE (arg0) == NEGATE_EXPR
	  && TREE_CODE (arg1) == INTEGER_CST
	  && 0 != (tem = negate_expr (fold_convert_loc (loc, TREE_TYPE (arg0),
							arg1)))
	  && TREE_CODE (tem) == INTEGER_CST
	  && !TREE_OVERFLOW (tem))
	return fold_build2_loc (loc, code, type, TREE_OPERAND (arg0, 0), tem);

      /* Similarly for a BIT_XOR_EXPR;  X ^ C1 == C2 is X == (C1 ^ C2).  */
      if (TREE_CODE (arg0) == BIT_XOR_EXPR
	  && TREE_CODE (arg1) == INTEGER_CST
	  && TREE_CODE (TREE_OPERAND (arg0, 1)) == INTEGER_CST)
	return fold_build2_loc (loc, code, type, TREE_OPERAND (arg0, 0),
			    fold_build2_loc (loc, BIT_XOR_EXPR, TREE_TYPE (arg0),
					 fold_convert_loc (loc,
							   TREE_TYPE (arg0),
							   arg1),
					 TREE_OPERAND (arg0, 1)));

      /* Transform comparisons of the form X +- Y CMP X to Y CMP 0.  */
      if ((TREE_CODE (arg0) == PLUS_EXPR
	   || TREE_CODE (arg0) == POINTER_PLUS_EXPR
	   || TREE_CODE (arg0) == MINUS_EXPR)
	  && operand_equal_p (tree_strip_nop_conversions (TREE_OPERAND (arg0,
									0)),
			      arg1, 0)
	  && (INTEGRAL_TYPE_P (TREE_TYPE (arg0))
	      || POINTER_TYPE_P (TREE_TYPE (arg0))))
	{
	  tree val = TREE_OPERAND (arg0, 1);
	  return omit_two_operands_loc (loc, type,
				    fold_build2_loc (loc, code, type,
						 val,
						 build_int_cst (TREE_TYPE (val),
								0)),
				    TREE_OPERAND (arg0, 0), arg1);
	}

      /* Transform comparisons of the form C - X CMP X if C % 2 == 1.  */
      if (TREE_CODE (arg0) == MINUS_EXPR
	  && TREE_CODE (TREE_OPERAND (arg0, 0)) == INTEGER_CST
	  && operand_equal_p (tree_strip_nop_conversions (TREE_OPERAND (arg0,
									1)),
			      arg1, 0)
	  && (TREE_INT_CST_LOW (TREE_OPERAND (arg0, 0)) & 1) == 1)
	{
	  return omit_two_operands_loc (loc, type,
				    code == NE_EXPR
				    ? boolean_true_node : boolean_false_node,
				    TREE_OPERAND (arg0, 1), arg1);
	}

      /* If we have X - Y == 0, we can convert that to X == Y and similarly
	 for !=.  Don't do this for ordered comparisons due to overflow.  */
      if (TREE_CODE (arg0) == MINUS_EXPR
	  && integer_zerop (arg1))
	return fold_build2_loc (loc, code, type,
			    TREE_OPERAND (arg0, 0), TREE_OPERAND (arg0, 1));

      /* Convert ABS_EXPR<x> == 0 or ABS_EXPR<x> != 0 to x == 0 or x != 0.  */
      if (TREE_CODE (arg0) == ABS_EXPR
	  && (integer_zerop (arg1) || real_zerop (arg1)))
	return fold_build2_loc (loc, code, type, TREE_OPERAND (arg0, 0), arg1);

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
				 build_int_cst (TREE_TYPE (arg0), 1));
	      return fold_build2_loc (loc, code, type,
				  fold_convert_loc (loc, TREE_TYPE (arg1), tem),
				  arg1);
	    }
	  else if (TREE_CODE (arg01) == LSHIFT_EXPR
		   && integer_onep (TREE_OPERAND (arg01, 0)))
	    {
	      tree tem = fold_build2_loc (loc, RSHIFT_EXPR, TREE_TYPE (arg01),
				      arg00, TREE_OPERAND (arg01, 1));
	      tem = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (arg0), tem,
				 build_int_cst (TREE_TYPE (arg0), 1));
	      return fold_build2_loc (loc, code, type,
				  fold_convert_loc (loc, TREE_TYPE (arg1), tem),
				  arg1);
	    }
	}

      /* If this is an NE or EQ comparison of zero against the result of a
	 signed MOD operation whose second operand is a power of 2, make
	 the MOD operation unsigned since it is simpler and equivalent.  */
      if (integer_zerop (arg1)
	  && !TYPE_UNSIGNED (TREE_TYPE (arg0))
	  && (TREE_CODE (arg0) == TRUNC_MOD_EXPR
	      || TREE_CODE (arg0) == CEIL_MOD_EXPR
	      || TREE_CODE (arg0) == FLOOR_MOD_EXPR
	      || TREE_CODE (arg0) == ROUND_MOD_EXPR)
	  && integer_pow2p (TREE_OPERAND (arg0, 1)))
	{
	  tree newtype = unsigned_type_for (TREE_TYPE (arg0));
	  tree newmod = fold_build2_loc (loc, TREE_CODE (arg0), newtype,
				     fold_convert_loc (loc, newtype,
						       TREE_OPERAND (arg0, 0)),
				     fold_convert_loc (loc, newtype,
						       TREE_OPERAND (arg0, 1)));

	  return fold_build2_loc (loc, code, type, newmod,
			      fold_convert_loc (loc, newtype, arg1));
	}

      /* Fold ((X >> C1) & C2) == 0 and ((X >> C1) & C2) != 0 where
	 C1 is a valid shift constant, and C2 is a power of two, i.e.
	 a single bit.  */
      if (TREE_CODE (arg0) == BIT_AND_EXPR
	  && TREE_CODE (TREE_OPERAND (arg0, 0)) == RSHIFT_EXPR
	  && TREE_CODE (TREE_OPERAND (TREE_OPERAND (arg0, 0), 1))
	     == INTEGER_CST
	  && integer_pow2p (TREE_OPERAND (arg0, 1))
	  && integer_zerop (arg1))
	{
	  tree itype = TREE_TYPE (arg0);
	  tree arg001 = TREE_OPERAND (TREE_OPERAND (arg0, 0), 1);
	  prec = TYPE_PRECISION (itype);

	  /* Check for a valid shift count.  */
	  if (TREE_INT_CST_HIGH (arg001) == 0
	      && TREE_INT_CST_LOW (arg001) < prec)
	    {
	      tree arg01 = TREE_OPERAND (arg0, 1);
	      tree arg000 = TREE_OPERAND (TREE_OPERAND (arg0, 0), 0);
	      unsigned HOST_WIDE_INT log2 = tree_log2 (arg01);
	      /* If (C2 << C1) doesn't overflow, then ((X >> C1) & C2) != 0
		 can be rewritten as (X & (C2 << C1)) != 0.  */
	      if ((log2 + TREE_INT_CST_LOW (arg001)) < prec)
		{
		  tem = fold_build2_loc (loc, LSHIFT_EXPR, itype, arg01, arg001);
		  tem = fold_build2_loc (loc, BIT_AND_EXPR, itype, arg000, tem);
		  return fold_build2_loc (loc, code, type, tem,
					  fold_convert_loc (loc, itype, arg1));
		}
	      /* Otherwise, for signed (arithmetic) shifts,
		 ((X >> C1) & C2) != 0 is rewritten as X < 0, and
		 ((X >> C1) & C2) == 0 is rewritten as X >= 0.  */
	      else if (!TYPE_UNSIGNED (itype))
		return fold_build2_loc (loc, code == EQ_EXPR ? GE_EXPR : LT_EXPR, type,
				    arg000, build_int_cst (itype, 0));
	      /* Otherwise, of unsigned (logical) shifts,
		 ((X >> C1) & C2) != 0 is rewritten as (X,false), and
		 ((X >> C1) & C2) == 0 is rewritten as (X,true).  */
	      else
		return omit_one_operand_loc (loc, type,
					 code == EQ_EXPR ? integer_one_node
							 : integer_zero_node,
					 arg000);
	    }
	}

      /* If we have (A & C) == C where C is a power of 2, convert this into
	 (A & C) != 0.  Similarly for NE_EXPR.  */
      if (TREE_CODE (arg0) == BIT_AND_EXPR
	  && integer_pow2p (TREE_OPERAND (arg0, 1))
	  && operand_equal_p (TREE_OPERAND (arg0, 1), arg1, 0))
	return fold_build2_loc (loc, code == EQ_EXPR ? NE_EXPR : EQ_EXPR, type,
			    arg0, fold_convert_loc (loc, TREE_TYPE (arg0),
						    integer_zero_node));

      /* If we have (A & C) != 0 or (A & C) == 0 and C is the sign
	 bit, then fold the expression into A < 0 or A >= 0.  */
      tem = fold_single_bit_test_into_sign_test (loc, code, arg0, arg1, type);
      if (tem)
	return tem;

      /* If we have (A & C) == D where D & ~C != 0, convert this into 0.
	 Similarly for NE_EXPR.  */
      if (TREE_CODE (arg0) == BIT_AND_EXPR
	  && TREE_CODE (arg1) == INTEGER_CST
	  && TREE_CODE (TREE_OPERAND (arg0, 1)) == INTEGER_CST)
	{
	  tree notc = fold_build1_loc (loc, BIT_NOT_EXPR,
				   TREE_TYPE (TREE_OPERAND (arg0, 1)),
				   TREE_OPERAND (arg0, 1));
	  tree dandnotc
	    = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (arg0),
			       fold_convert_loc (loc, TREE_TYPE (arg0), arg1),
			       notc);
	  tree rslt = code == EQ_EXPR ? integer_zero_node : integer_one_node;
	  if (integer_nonzerop (dandnotc))
	    return omit_one_operand_loc (loc, type, rslt, arg0);
	}

      /* If we have (A | C) == D where C & ~D != 0, convert this into 0.
	 Similarly for NE_EXPR.  */
      if (TREE_CODE (arg0) == BIT_IOR_EXPR
	  && TREE_CODE (arg1) == INTEGER_CST
	  && TREE_CODE (TREE_OPERAND (arg0, 1)) == INTEGER_CST)
	{
	  tree notd = fold_build1_loc (loc, BIT_NOT_EXPR, TREE_TYPE (arg1), arg1);
	  tree candnotd
	    = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (arg0),
			       TREE_OPERAND (arg0, 1),
			       fold_convert_loc (loc, TREE_TYPE (arg0), notd));
	  tree rslt = code == EQ_EXPR ? integer_zero_node : integer_one_node;
	  if (integer_nonzerop (candnotd))
	    return omit_one_operand_loc (loc, type, rslt, arg0);
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
      if (TREE_CODE (arg0) == CALL_EXPR
	  && integer_zerop (arg1))
	{
	  tree fndecl = get_callee_fndecl (arg0);

	  if (fndecl
	      && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL
	      && DECL_FUNCTION_CODE (fndecl) == BUILT_IN_STRLEN
	      && call_expr_nargs (arg0) == 1
	      && TREE_CODE (TREE_TYPE (CALL_EXPR_ARG (arg0, 0))) == POINTER_TYPE)
	    {
	      tree iref = build_fold_indirect_ref_loc (loc,
						   CALL_EXPR_ARG (arg0, 0));
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
	  if (TREE_INT_CST_HIGH (arg01) == 0
	      && TREE_INT_CST_LOW (arg01)
		 == (unsigned HOST_WIDE_INT) (TYPE_PRECISION (itype) - 1))
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

      /* (X ^ Y) == 0 becomes X == Y, and (X ^ Y) != 0 becomes X != Y.  */
      if (integer_zerop (arg1)
	  && TREE_CODE (arg0) == BIT_XOR_EXPR)
	return fold_build2_loc (loc, code, type, TREE_OPERAND (arg0, 0),
			    TREE_OPERAND (arg0, 1));

      /* (X ^ Y) == Y becomes X == 0.  We know that Y has no side-effects.  */
      if (TREE_CODE (arg0) == BIT_XOR_EXPR
	  && operand_equal_p (TREE_OPERAND (arg0, 1), arg1, 0))
	return fold_build2_loc (loc, code, type, TREE_OPERAND (arg0, 0),
				build_zero_cst (TREE_TYPE (arg0)));
      /* Likewise (X ^ Y) == X becomes Y == 0.  X has no side-effects.  */
      if (TREE_CODE (arg0) == BIT_XOR_EXPR
	  && operand_equal_p (TREE_OPERAND (arg0, 0), arg1, 0)
	  && reorder_operands_p (TREE_OPERAND (arg0, 1), arg1))
	return fold_build2_loc (loc, code, type, TREE_OPERAND (arg0, 1),
				build_zero_cst (TREE_TYPE (arg0)));

      /* (X ^ C1) op C2 can be rewritten as X op (C1 ^ C2).  */
      if (TREE_CODE (arg0) == BIT_XOR_EXPR
	  && TREE_CODE (arg1) == INTEGER_CST
	  && TREE_CODE (TREE_OPERAND (arg0, 1)) == INTEGER_CST)
	return fold_build2_loc (loc, code, type, TREE_OPERAND (arg0, 0),
			    fold_build2_loc (loc, BIT_XOR_EXPR, TREE_TYPE (arg1),
					 TREE_OPERAND (arg0, 1), arg1));

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

      if (integer_zerop (arg1)
	  && tree_expr_nonzero_p (arg0))
        {
	  tree res = constant_boolean_node (code==NE_EXPR, type);
	  return omit_one_operand_loc (loc, type, res, arg0);
	}

      /* Fold -X op -Y as X op Y, where op is eq/ne.  */
      if (TREE_CODE (arg0) == NEGATE_EXPR
          && TREE_CODE (arg1) == NEGATE_EXPR)
	return fold_build2_loc (loc, code, type,
				TREE_OPERAND (arg0, 0),
				fold_convert_loc (loc, TREE_TYPE (arg0),
						  TREE_OPERAND (arg1, 0)));

      /* Fold (X & C) op (Y & C) as (X ^ Y) & C op 0", and symmetries.  */
      if (TREE_CODE (arg0) == BIT_AND_EXPR
	  && TREE_CODE (arg1) == BIT_AND_EXPR)
	{
	  tree arg00 = TREE_OPERAND (arg0, 0);
	  tree arg01 = TREE_OPERAND (arg0, 1);
	  tree arg10 = TREE_OPERAND (arg1, 0);
	  tree arg11 = TREE_OPERAND (arg1, 1);
	  tree itype = TREE_TYPE (arg0);

	  if (operand_equal_p (arg01, arg11, 0))
	    return fold_build2_loc (loc, code, type,
				fold_build2_loc (loc, BIT_AND_EXPR, itype,
					     fold_build2_loc (loc,
							  BIT_XOR_EXPR, itype,
							  arg00, arg10),
					     arg01),
				build_zero_cst (itype));

	  if (operand_equal_p (arg01, arg10, 0))
	    return fold_build2_loc (loc, code, type,
				fold_build2_loc (loc, BIT_AND_EXPR, itype,
					     fold_build2_loc (loc,
							  BIT_XOR_EXPR, itype,
							  arg00, arg11),
					     arg01),
				build_zero_cst (itype));

	  if (operand_equal_p (arg00, arg11, 0))
	    return fold_build2_loc (loc, code, type,
				fold_build2_loc (loc, BIT_AND_EXPR, itype,
					     fold_build2_loc (loc,
							  BIT_XOR_EXPR, itype,
							  arg01, arg10),
					     arg00),
				build_zero_cst (itype));

	  if (operand_equal_p (arg00, arg10, 0))
	    return fold_build2_loc (loc, code, type,
				fold_build2_loc (loc, BIT_AND_EXPR, itype,
					     fold_build2_loc (loc,
							  BIT_XOR_EXPR, itype,
							  arg01, arg11),
					     arg00),
				build_zero_cst (itype));
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
	  && ((TREE_CODE (TREE_OPERAND (arg0, 1)) == REAL_CST
	       && !HONOR_SNANS (TYPE_MODE (TREE_TYPE (arg0))))
	      || (TREE_CODE (TREE_OPERAND (arg0, 1)) == INTEGER_CST
		  && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (arg1)))))
	{
	  tree arg01 = TREE_OPERAND (arg0, 1);
	  enum tree_code code0 = TREE_CODE (arg0);
	  int is_positive;

	  if (TREE_CODE (arg01) == REAL_CST)
	    is_positive = REAL_VALUE_NEGATIVE (TREE_REAL_CST (arg01)) ? -1 : 1;
	  else
	    is_positive = tree_int_cst_sgn (arg01);

	  /* (X - c) > X becomes false.  */
	  if (code == GT_EXPR
	      && ((code0 == MINUS_EXPR && is_positive >= 0)
		  || (code0 == PLUS_EXPR && is_positive <= 0)))
	    {
	      if (TREE_CODE (arg01) == INTEGER_CST
		  && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (arg1)))
		fold_overflow_warning (("assuming signed overflow does not "
					"occur when assuming that (X - c) > X "
					"is always false"),
				       WARN_STRICT_OVERFLOW_ALL);
	      return constant_boolean_node (0, type);
	    }

	  /* Likewise (X + c) < X becomes false.  */
	  if (code == LT_EXPR
	      && ((code0 == PLUS_EXPR && is_positive >= 0)
		  || (code0 == MINUS_EXPR && is_positive <= 0)))
	    {
	      if (TREE_CODE (arg01) == INTEGER_CST
		  && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (arg1)))
		fold_overflow_warning (("assuming signed overflow does not "
					"occur when assuming that "
					"(X + c) < X is always false"),
				       WARN_STRICT_OVERFLOW_ALL);
	      return constant_boolean_node (0, type);
	    }

	  /* Convert (X - c) <= X to true.  */
	  if (!HONOR_NANS (TYPE_MODE (TREE_TYPE (arg1)))
	      && code == LE_EXPR
	      && ((code0 == MINUS_EXPR && is_positive >= 0)
		  || (code0 == PLUS_EXPR && is_positive <= 0)))
	    {
	      if (TREE_CODE (arg01) == INTEGER_CST
		  && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (arg1)))
		fold_overflow_warning (("assuming signed overflow does not "
					"occur when assuming that "
					"(X - c) <= X is always true"),
				       WARN_STRICT_OVERFLOW_ALL);
	      return constant_boolean_node (1, type);
	    }

	  /* Convert (X + c) >= X to true.  */
	  if (!HONOR_NANS (TYPE_MODE (TREE_TYPE (arg1)))
	      && code == GE_EXPR
	      && ((code0 == PLUS_EXPR && is_positive >= 0)
		  || (code0 == MINUS_EXPR && is_positive <= 0)))
	    {
	      if (TREE_CODE (arg01) == INTEGER_CST
		  && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (arg1)))
		fold_overflow_warning (("assuming signed overflow does not "
					"occur when assuming that "
					"(X + c) >= X is always true"),
				       WARN_STRICT_OVERFLOW_ALL);
	      return constant_boolean_node (1, type);
	    }

	  if (TREE_CODE (arg01) == INTEGER_CST)
	    {
	      /* Convert X + c > X and X - c < X to true for integers.  */
	      if (code == GT_EXPR
	          && ((code0 == PLUS_EXPR && is_positive > 0)
		      || (code0 == MINUS_EXPR && is_positive < 0)))
		{
		  if (TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (arg1)))
		    fold_overflow_warning (("assuming signed overflow does "
					    "not occur when assuming that "
					    "(X + c) > X is always true"),
					   WARN_STRICT_OVERFLOW_ALL);
		  return constant_boolean_node (1, type);
		}

	      if (code == LT_EXPR
	          && ((code0 == MINUS_EXPR && is_positive > 0)
		      || (code0 == PLUS_EXPR && is_positive < 0)))
		{
		  if (TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (arg1)))
		    fold_overflow_warning (("assuming signed overflow does "
					    "not occur when assuming that "
					    "(X - c) < X is always true"),
					   WARN_STRICT_OVERFLOW_ALL);
		  return constant_boolean_node (1, type);
		}

	      /* Convert X + c <= X and X - c >= X to false for integers.  */
	      if (code == LE_EXPR
	          && ((code0 == PLUS_EXPR && is_positive > 0)
		      || (code0 == MINUS_EXPR && is_positive < 0)))
		{
		  if (TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (arg1)))
		    fold_overflow_warning (("assuming signed overflow does "
					    "not occur when assuming that "
					    "(X + c) <= X is always false"),
					   WARN_STRICT_OVERFLOW_ALL);
		  return constant_boolean_node (0, type);
		}

	      if (code == GE_EXPR
	          && ((code0 == MINUS_EXPR && is_positive > 0)
		      || (code0 == PLUS_EXPR && is_positive < 0)))
		{
		  if (TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (arg1)))
		    fold_overflow_warning (("assuming signed overflow does "
					    "not occur when assuming that "
					    "(X - c) >= X is always false"),
					   WARN_STRICT_OVERFLOW_ALL);
		  return constant_boolean_node (0, type);
		}
	    }
	}

      /* Comparisons with the highest or lowest possible integer of
	 the specified precision will have known values.  */
      {
	tree arg1_type = TREE_TYPE (arg1);
	unsigned int width = TYPE_PRECISION (arg1_type);

	if (TREE_CODE (arg1) == INTEGER_CST
	    && width <= HOST_BITS_PER_DOUBLE_INT
	    && (INTEGRAL_TYPE_P (arg1_type) || POINTER_TYPE_P (arg1_type)))
	  {
	    HOST_WIDE_INT signed_max_hi;
	    unsigned HOST_WIDE_INT signed_max_lo;
	    unsigned HOST_WIDE_INT max_hi, max_lo, min_hi, min_lo;

	    if (width <= HOST_BITS_PER_WIDE_INT)
	      {
		signed_max_lo = ((unsigned HOST_WIDE_INT) 1 << (width - 1))
				- 1;
		signed_max_hi = 0;
		max_hi = 0;

		if (TYPE_UNSIGNED (arg1_type))
		  {
		    max_lo = ((unsigned HOST_WIDE_INT) 2 << (width - 1)) - 1;
		    min_lo = 0;
		    min_hi = 0;
		  }
		else
		  {
		    max_lo = signed_max_lo;
		    min_lo = (HOST_WIDE_INT_M1U << (width - 1));
		    min_hi = -1;
		  }
	      }
	    else
	      {
		width -= HOST_BITS_PER_WIDE_INT;
		signed_max_lo = -1;
		signed_max_hi = ((unsigned HOST_WIDE_INT) 1 << (width - 1))
				- 1;
		max_lo = -1;
		min_lo = 0;

		if (TYPE_UNSIGNED (arg1_type))
		  {
		    max_hi = ((unsigned HOST_WIDE_INT) 2 << (width - 1)) - 1;
		    min_hi = 0;
		  }
		else
		  {
		    max_hi = signed_max_hi;
		    min_hi = (HOST_WIDE_INT_M1U << (width - 1));
		  }
	      }

	    if ((unsigned HOST_WIDE_INT) TREE_INT_CST_HIGH (arg1) == max_hi
		&& TREE_INT_CST_LOW (arg1) == max_lo)
	      switch (code)
		{
		case GT_EXPR:
		  return omit_one_operand_loc (loc, type, integer_zero_node, arg0);

		case GE_EXPR:
		  return fold_build2_loc (loc, EQ_EXPR, type, op0, op1);

		case LE_EXPR:
		  return omit_one_operand_loc (loc, type, integer_one_node, arg0);

		case LT_EXPR:
		  return fold_build2_loc (loc, NE_EXPR, type, op0, op1);

		/* The GE_EXPR and LT_EXPR cases above are not normally
		   reached because of previous transformations.  */

		default:
		  break;
		}
	    else if ((unsigned HOST_WIDE_INT) TREE_INT_CST_HIGH (arg1)
		     == max_hi
		     && TREE_INT_CST_LOW (arg1) == max_lo - 1)
	      switch (code)
		{
		case GT_EXPR:
		  arg1 = const_binop (PLUS_EXPR, arg1,
				      build_int_cst (TREE_TYPE (arg1), 1));
		  return fold_build2_loc (loc, EQ_EXPR, type,
				      fold_convert_loc (loc,
							TREE_TYPE (arg1), arg0),
				      arg1);
		case LE_EXPR:
		  arg1 = const_binop (PLUS_EXPR, arg1,
				      build_int_cst (TREE_TYPE (arg1), 1));
		  return fold_build2_loc (loc, NE_EXPR, type,
				      fold_convert_loc (loc, TREE_TYPE (arg1),
							arg0),
				      arg1);
		default:
		  break;
		}
	    else if ((unsigned HOST_WIDE_INT) TREE_INT_CST_HIGH (arg1)
		     == min_hi
		     && TREE_INT_CST_LOW (arg1) == min_lo)
	      switch (code)
		{
		case LT_EXPR:
		  return omit_one_operand_loc (loc, type, integer_zero_node, arg0);

		case LE_EXPR:
		  return fold_build2_loc (loc, EQ_EXPR, type, op0, op1);

		case GE_EXPR:
		  return omit_one_operand_loc (loc, type, integer_one_node, arg0);

		case GT_EXPR:
		  return fold_build2_loc (loc, NE_EXPR, type, op0, op1);

		default:
		  break;
		}
	    else if ((unsigned HOST_WIDE_INT) TREE_INT_CST_HIGH (arg1)
		     == min_hi
		     && TREE_INT_CST_LOW (arg1) == min_lo + 1)
	      switch (code)
		{
		case GE_EXPR:
		  arg1 = const_binop (MINUS_EXPR, arg1, integer_one_node);
		  return fold_build2_loc (loc, NE_EXPR, type,
				      fold_convert_loc (loc,
							TREE_TYPE (arg1), arg0),
				      arg1);
		case LT_EXPR:
		  arg1 = const_binop (MINUS_EXPR, arg1, integer_one_node);
		  return fold_build2_loc (loc, EQ_EXPR, type,
				      fold_convert_loc (loc, TREE_TYPE (arg1),
							arg0),
				      arg1);
		default:
		  break;
		}

	    else if (TREE_INT_CST_HIGH (arg1) == signed_max_hi
		     && TREE_INT_CST_LOW (arg1) == signed_max_lo
		     && TYPE_UNSIGNED (arg1_type)
		     /* We will flip the signedness of the comparison operator
			associated with the mode of arg1, so the sign bit is
			specified by this mode.  Check that arg1 is the signed
			max associated with this sign bit.  */
		     && width == GET_MODE_BITSIZE (TYPE_MODE (arg1_type))
		     /* signed_type does not work on pointer types.  */
		     && INTEGRAL_TYPE_P (arg1_type))
	      {
		/* The following case also applies to X < signed_max+1
		   and X >= signed_max+1 because previous transformations.  */
		if (code == LE_EXPR || code == GT_EXPR)
		  {
		    tree st;
		    st = signed_type_for (TREE_TYPE (arg1));
		    return fold_build2_loc (loc,
					code == LE_EXPR ? GE_EXPR : LT_EXPR,
					type, fold_convert_loc (loc, st, arg0),
					build_int_cst (st, 0));
		  }
	      }
	  }
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
	  && (0 != (tem = negate_expr (arg1)))
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
	      || (! HONOR_NANS (TYPE_MODE (TREE_TYPE (arg0)))
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
	  && TYPE_UNSIGNED (TREE_TYPE (arg0))
	  && CONVERT_EXPR_P (arg1)
	  && TREE_CODE (TREE_OPERAND (arg1, 0)) == LSHIFT_EXPR
	  && (TYPE_PRECISION (TREE_TYPE (arg1))
	      >= TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (arg1, 0))))
	  && (TYPE_UNSIGNED (TREE_TYPE (TREE_OPERAND (arg1, 0)))
	      || (TYPE_PRECISION (TREE_TYPE (arg1))
		  == TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (arg1, 0)))))
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
      if (TREE_CODE (arg0) == REAL_CST && TREE_CODE (arg1) == REAL_CST)
	{
	  t1 = fold_relational_const (code, type, arg0, arg1);
	  if (t1 != NULL_TREE)
	    return t1;
	}

      /* If the first operand is NaN, the result is constant.  */
      if (TREE_CODE (arg0) == REAL_CST
	  && REAL_VALUE_ISNAN (TREE_REAL_CST (arg0))
	  && (code != LTGT_EXPR || ! flag_trapping_math))
	{
	  t1 = (code == ORDERED_EXPR || code == LTGT_EXPR)
	       ? integer_zero_node
	       : integer_one_node;
	  return omit_one_operand_loc (loc, type, t1, arg1);
	}

      /* If the second operand is NaN, the result is constant.  */
      if (TREE_CODE (arg1) == REAL_CST
	  && REAL_VALUE_ISNAN (TREE_REAL_CST (arg1))
	  && (code != LTGT_EXPR || ! flag_trapping_math))
	{
	  t1 = (code == ORDERED_EXPR || code == LTGT_EXPR)
	       ? integer_zero_node
	       : integer_one_node;
	  return omit_one_operand_loc (loc, type, t1, arg0);
	}

      /* Simplify unordered comparison of something with itself.  */
      if ((code == UNLE_EXPR || code == UNGE_EXPR || code == UNEQ_EXPR)
	  && operand_equal_p (arg0, arg1, 0))
	return constant_boolean_node (1, type);

      if (code == LTGT_EXPR
	  && !flag_trapping_math
	  && operand_equal_p (arg0, arg1, 0))
	return constant_boolean_node (0, type);

      /* Fold (double)float1 CMP (double)float2 into float1 CMP float2.  */
      {
	tree targ0 = strip_float_extensions (arg0);
	tree targ1 = strip_float_extensions (arg1);
	tree newtype = TREE_TYPE (targ0);

	if (TYPE_PRECISION (TREE_TYPE (targ1)) > TYPE_PRECISION (newtype))
	  newtype = TREE_TYPE (targ1);

	if (TYPE_PRECISION (newtype) < TYPE_PRECISION (TREE_TYPE (arg0)))
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
      tem = integer_zerop (arg1) ? build1 (NOP_EXPR, type, arg1)
				 : fold_convert_loc (loc, type, arg1);
      return pedantic_non_lvalue_loc (loc, tem);

    case COMPLEX_EXPR:
      if ((TREE_CODE (arg0) == REAL_CST
	   && TREE_CODE (arg1) == REAL_CST)
	  || (TREE_CODE (arg0) == INTEGER_CST
	      && TREE_CODE (arg1) == INTEGER_CST))
	return build_complex (type, arg0, arg1);
      if (TREE_CODE (arg0) == REALPART_EXPR
	  && TREE_CODE (arg1) == IMAGPART_EXPR
	  && TREE_TYPE (TREE_OPERAND (arg0, 0)) == type
	  && operand_equal_p (TREE_OPERAND (arg0, 0),
			      TREE_OPERAND (arg1, 0), 0))
	return omit_one_operand_loc (loc, type, TREE_OPERAND (arg0, 0),
				     TREE_OPERAND (arg1, 0));
      return NULL_TREE;

    case ASSERT_EXPR:
      /* An ASSERT_EXPR should never be passed to fold_binary.  */
      gcc_unreachable ();

    case VEC_PACK_TRUNC_EXPR:
    case VEC_PACK_FIX_TRUNC_EXPR:
      {
	unsigned int nelts = TYPE_VECTOR_SUBPARTS (type), i;
	tree *elts;

	gcc_assert (TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg0)) == nelts / 2
		    && TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg1)) == nelts / 2);
	if (TREE_CODE (arg0) != VECTOR_CST || TREE_CODE (arg1) != VECTOR_CST)
	  return NULL_TREE;

	elts = XALLOCAVEC (tree, nelts);
	if (!vec_cst_ctor_to_array (arg0, elts)
	    || !vec_cst_ctor_to_array (arg1, elts + nelts / 2))
	  return NULL_TREE;

	for (i = 0; i < nelts; i++)
	  {
	    elts[i] = fold_convert_const (code == VEC_PACK_TRUNC_EXPR
					  ? NOP_EXPR : FIX_TRUNC_EXPR,
					  TREE_TYPE (type), elts[i]);
	    if (elts[i] == NULL_TREE || !CONSTANT_CLASS_P (elts[i]))
	      return NULL_TREE;
	  }

	return build_vector (type, elts);
      }

    case VEC_WIDEN_MULT_LO_EXPR:
    case VEC_WIDEN_MULT_HI_EXPR:
    case VEC_WIDEN_MULT_EVEN_EXPR:
    case VEC_WIDEN_MULT_ODD_EXPR:
      {
	unsigned int nelts = TYPE_VECTOR_SUBPARTS (type);
	unsigned int out, ofs, scale;
	tree *elts;

	gcc_assert (TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg0)) == nelts * 2
		    && TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg1)) == nelts * 2);
	if (TREE_CODE (arg0) != VECTOR_CST || TREE_CODE (arg1) != VECTOR_CST)
	  return NULL_TREE;

	elts = XALLOCAVEC (tree, nelts * 4);
	if (!vec_cst_ctor_to_array (arg0, elts)
	    || !vec_cst_ctor_to_array (arg1, elts + nelts * 2))
	  return NULL_TREE;

	if (code == VEC_WIDEN_MULT_LO_EXPR)
	  scale = 0, ofs = BYTES_BIG_ENDIAN ? nelts : 0;
	else if (code == VEC_WIDEN_MULT_HI_EXPR)
	  scale = 0, ofs = BYTES_BIG_ENDIAN ? 0 : nelts;
	else if (code == VEC_WIDEN_MULT_EVEN_EXPR)
	  scale = 1, ofs = 0;
	else /* if (code == VEC_WIDEN_MULT_ODD_EXPR) */
	  scale = 1, ofs = 1;
	
	for (out = 0; out < nelts; out++)
	  {
	    unsigned int in1 = (out << scale) + ofs;
	    unsigned int in2 = in1 + nelts * 2;
	    tree t1, t2;

	    t1 = fold_convert_const (NOP_EXPR, TREE_TYPE (type), elts[in1]);
	    t2 = fold_convert_const (NOP_EXPR, TREE_TYPE (type), elts[in2]);

	    if (t1 == NULL_TREE || t2 == NULL_TREE)
	      return NULL_TREE;
	    elts[out] = const_binop (MULT_EXPR, t1, t2);
	    if (elts[out] == NULL_TREE || !CONSTANT_CLASS_P (elts[out]))
	      return NULL_TREE;
	  }

	return build_vector (type, elts);
      }

    default:
      return NULL_TREE;
    } /* switch (code) */
}

/* Callback for walk_tree, looking for LABEL_EXPR.  Return *TP if it is
   a LABEL_EXPR; otherwise return NULL_TREE.  Do not check the subtrees
   of GOTO_EXPR.  */

static tree
contains_label_1 (tree *tp, int *walk_subtrees, void *data ATTRIBUTE_UNUSED)
{
  switch (TREE_CODE (*tp))
    {
    case LABEL_EXPR:
      return *tp;

    case GOTO_EXPR:
      *walk_subtrees = 0;

      /* ... fall through ...  */

    default:
      return NULL_TREE;
    }
}

/* Return whether the sub-tree ST contains a label which is accessible from
   outside the sub-tree.  */

static bool
contains_label_p (tree st)
{
  return
   (walk_tree_without_duplicates (&st, contains_label_1 , NULL) != NULL_TREE);
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
	    return pedantic_non_lvalue_loc (loc, tem);
	  return NULL_TREE;
	}
      else if (TREE_CODE (arg0) == VECTOR_CST)
	{
	  if (integer_all_onesp (arg0))
	    return pedantic_omit_one_operand_loc (loc, type, arg1, arg2);
	  if (integer_zerop (arg0))
	    return pedantic_omit_one_operand_loc (loc, type, arg2, arg1);

	  if ((TREE_CODE (arg1) == VECTOR_CST
	       || TREE_CODE (arg1) == CONSTRUCTOR)
	      && (TREE_CODE (arg2) == VECTOR_CST
		  || TREE_CODE (arg2) == CONSTRUCTOR))
	    {
	      unsigned int nelts = TYPE_VECTOR_SUBPARTS (type), i;
	      unsigned char *sel = XALLOCAVEC (unsigned char, nelts);
	      gcc_assert (nelts == VECTOR_CST_NELTS (arg0));
	      for (i = 0; i < nelts; i++)
		{
		  tree val = VECTOR_CST_ELT (arg0, i);
		  if (integer_all_onesp (val))
		    sel[i] = i;
		  else if (integer_zerop (val))
		    sel[i] = nelts + i;
		  else /* Currently unreachable.  */
		    return NULL_TREE;
		}
	      tree t = fold_vec_perm (type, arg1, arg2, sel);
	      if (t != NULL_TREE)
		return t;
	    }
	}

      if (operand_equal_p (arg1, op2, 0))
	return pedantic_omit_one_operand_loc (loc, type, arg1, arg0);

      /* If we have A op B ? A : C, we may be able to convert this to a
	 simpler expression, depending on the operation and the values
	 of B and C.  Signed zeros prevent all of these transformations,
	 for reasons given above each one.

         Also try swapping the arguments and inverting the conditional.  */
      if (COMPARISON_CLASS_P (arg0)
	  && operand_equal_for_comparison_p (TREE_OPERAND (arg0, 0),
					     arg1, TREE_OPERAND (arg0, 1))
	  && !HONOR_SIGNED_ZEROS (TYPE_MODE (TREE_TYPE (arg1))))
	{
	  tem = fold_cond_expr_with_comparison (loc, type, arg0, op1, op2);
	  if (tem)
	    return tem;
	}

      if (COMPARISON_CLASS_P (arg0)
	  && operand_equal_for_comparison_p (TREE_OPERAND (arg0, 0),
					     op2,
					     TREE_OPERAND (arg0, 1))
	  && !HONOR_SIGNED_ZEROS (TYPE_MODE (TREE_TYPE (op2))))
	{
	  location_t loc0 = expr_location_or (arg0, loc);
	  tem = fold_invert_truthvalue (loc0, arg0);
	  if (tem && COMPARISON_CLASS_P (tem))
	    {
	      tem = fold_cond_expr_with_comparison (loc, type, tem, op2, op1);
	      if (tem)
		return tem;
	    }
	}

      /* If the second operand is simpler than the third, swap them
	 since that produces better jump optimization results.  */
      if (truth_value_p (TREE_CODE (arg0))
	  && tree_swap_operands_p (op1, op2, false))
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
	return pedantic_non_lvalue_loc (loc, arg0);

      /* Convert A ? 0 : 1 to !A.  This prefers the use of NOT_EXPR
	 over COND_EXPR in cases such as floating point comparisons.  */
      if (integer_zerop (op1)
	  && (code == VEC_COND_EXPR ? integer_all_onesp (op2)
				    : (integer_onep (op2)
				       && !VECTOR_TYPE_P (type)))
	  && truth_value_p (TREE_CODE (arg0)))
	return pedantic_non_lvalue_loc (loc,
				    fold_convert_loc (loc, type,
					      invert_truthvalue_loc (loc,
								     arg0)));

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
	      unsigned HOST_WIDE_INT mask_lo;
	      HOST_WIDE_INT mask_hi;
	      int inner_width, outer_width;
	      tree tem_type;

	      inner_width = TYPE_PRECISION (TREE_TYPE (tem));
	      outer_width = TYPE_PRECISION (TREE_TYPE (arg1));
	      if (outer_width > TYPE_PRECISION (type))
		outer_width = TYPE_PRECISION (type);

	      if (outer_width > HOST_BITS_PER_WIDE_INT)
		{
		  mask_hi = (HOST_WIDE_INT_M1U
			     >> (HOST_BITS_PER_DOUBLE_INT - outer_width));
		  mask_lo = -1;
		}
	      else
		{
		  mask_hi = 0;
		  mask_lo = (HOST_WIDE_INT_M1U
			     >> (HOST_BITS_PER_WIDE_INT - outer_width));
		}
	      if (inner_width > HOST_BITS_PER_WIDE_INT)
		{
		  mask_hi &= ~(HOST_WIDE_INT_M1U
			       >> (HOST_BITS_PER_WIDE_INT - inner_width));
		  mask_lo = 0;
		}
	      else
		mask_lo &= ~(HOST_WIDE_INT_M1U
			     >> (HOST_BITS_PER_WIDE_INT - inner_width));

	      if ((TREE_INT_CST_HIGH (arg1) & mask_hi) == mask_hi
		  && (TREE_INT_CST_LOW (arg1) & mask_lo) == mask_lo)
		{
		  tem_type = signed_type_for (TREE_TYPE (tem));
		  tem = fold_convert_loc (loc, tem_type, tem);
		}
	      else if ((TREE_INT_CST_HIGH (arg1) & mask_hi) == 0
		       && (TREE_INT_CST_LOW (arg1) & mask_lo) == 0)
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
              && TREE_CODE (TREE_OPERAND (tem, 1)) == INTEGER_CST
              && (unsigned HOST_WIDE_INT) tree_log2 (arg1) ==
	         TREE_INT_CST_LOW (TREE_OPERAND (tem, 1)))
	    return fold_build2_loc (loc, BIT_AND_EXPR, type,
				TREE_OPERAND (tem, 0), arg1);
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
			      arg1, OEP_ONLY_CONST))
	return pedantic_non_lvalue_loc (loc,
				    fold_convert_loc (loc, type,
						      TREE_OPERAND (arg0, 0)));

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
				type, fold_convert_loc (loc, type, arg0), arg1);

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
				    arg1);
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
      if ((TREE_CODE (arg0) == VECTOR_CST
	   || (TREE_CODE (arg0) == CONSTRUCTOR
	       && TREE_CODE (TREE_TYPE (arg0)) == VECTOR_TYPE))
	  && (type == TREE_TYPE (TREE_TYPE (arg0))
	      || (TREE_CODE (type) == VECTOR_TYPE
		  && TREE_TYPE (type) == TREE_TYPE (TREE_TYPE (arg0)))))
	{
	  tree eltype = TREE_TYPE (TREE_TYPE (arg0));
	  unsigned HOST_WIDE_INT width = tree_low_cst (TYPE_SIZE (eltype), 1);
	  unsigned HOST_WIDE_INT n = tree_low_cst (arg1, 1);
	  unsigned HOST_WIDE_INT idx = tree_low_cst (op2, 1);

	  if (n != 0
	      && (idx % width) == 0
	      && (n % width) == 0
	      && ((idx + n) / width) <= TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg0)))
	    {
	      idx = idx / width;
	      n = n / width;

	      if (TREE_CODE (arg0) == VECTOR_CST)
		{
		  if (n == 1)
		    return VECTOR_CST_ELT (arg0, idx);

		  tree *vals = XALLOCAVEC (tree, n);
		  for (unsigned i = 0; i < n; ++i)
		    vals[i] = VECTOR_CST_ELT (arg0, idx + i);
		  return build_vector (type, vals);
		}

	      /* Constructor elements can be subvectors.  */
	      unsigned HOST_WIDE_INT k = 1;
	      if (CONSTRUCTOR_NELTS (arg0) != 0)
		{
		  tree cons_elem = TREE_TYPE (CONSTRUCTOR_ELT (arg0, 0)->value);
		  if (TREE_CODE (cons_elem) == VECTOR_TYPE)
		    k = TYPE_VECTOR_SUBPARTS (cons_elem);
		}

	      /* We keep an exact subset of the constructor elements.  */
	      if ((idx % k) == 0 && (n % k) == 0)
		{
		  if (CONSTRUCTOR_NELTS (arg0) == 0)
		    return build_constructor (type, NULL);
		  idx /= k;
		  n /= k;
		  if (n == 1)
		    {
		      if (idx < CONSTRUCTOR_NELTS (arg0))
			return CONSTRUCTOR_ELT (arg0, idx)->value;
		      return build_zero_cst (type);
		    }

		  vec<constructor_elt, va_gc> *vals;
		  vec_alloc (vals, n);
		  for (unsigned i = 0;
		       i < n && idx + i < CONSTRUCTOR_NELTS (arg0);
		       ++i)
		    CONSTRUCTOR_APPEND_ELT (vals, NULL_TREE,
					    CONSTRUCTOR_ELT
					      (arg0, idx + i)->value);
		  return build_constructor (type, vals);
		}
	      /* The bitfield references a single constructor element.  */
	      else if (idx + n <= (idx / k + 1) * k)
		{
		  if (CONSTRUCTOR_NELTS (arg0) <= idx / k)
		    return build_zero_cst (type);
		  else if (n == k)
		    return CONSTRUCTOR_ELT (arg0, idx / k)->value;
		  else
		    return fold_build3_loc (loc, code, type,
		      CONSTRUCTOR_ELT (arg0, idx / k)->value, op1,
		      build_int_cst (TREE_TYPE (op2), (idx % k) * width));
		}
	    }
	}

      /* A bit-field-ref that referenced the full argument can be stripped.  */
      if (INTEGRAL_TYPE_P (TREE_TYPE (arg0))
	  && TYPE_PRECISION (TREE_TYPE (arg0)) == tree_low_cst (arg1, 1)
	  && integer_zerop (op2))
	return fold_convert_loc (loc, type, arg0);

      /* On constants we can use native encode/interpret to constant
         fold (nearly) all BIT_FIELD_REFs.  */
      if (CONSTANT_CLASS_P (arg0)
	  && can_native_interpret_type_p (type)
	  && host_integerp (TYPE_SIZE_UNIT (TREE_TYPE (arg0)), 1)
	  /* This limitation should not be necessary, we just need to
	     round this up to mode size.  */
	  && tree_low_cst (op1, 1) % BITS_PER_UNIT == 0
	  /* Need bit-shifting of the buffer to relax the following.  */
	  && tree_low_cst (op2, 1) % BITS_PER_UNIT == 0)
	{
	  unsigned HOST_WIDE_INT bitpos = tree_low_cst (op2, 1);
	  unsigned HOST_WIDE_INT bitsize = tree_low_cst (op1, 1);
	  unsigned HOST_WIDE_INT clen;
	  clen = tree_low_cst (TYPE_SIZE_UNIT (TREE_TYPE (arg0)), 1);
	  /* ???  We cannot tell native_encode_expr to start at
	     some random byte only.  So limit us to a reasonable amount
	     of work.  */
	  if (clen <= 4096)
	    {
	      unsigned char *b = XALLOCAVEC (unsigned char, clen);
	      unsigned HOST_WIDE_INT len = native_encode_expr (arg0, b, clen);
	      if (len > 0
		  && len * BITS_PER_UNIT >= bitpos + bitsize)
		{
		  tree v = native_interpret_expr (type,
						  b + bitpos / BITS_PER_UNIT,
						  bitsize / BITS_PER_UNIT);
		  if (v)
		    return v;
		}
	    }
	}

      return NULL_TREE;

    case FMA_EXPR:
      /* For integers we can decompose the FMA if possible.  */
      if (TREE_CODE (arg0) == INTEGER_CST
	  && TREE_CODE (arg1) == INTEGER_CST)
	return fold_build2_loc (loc, PLUS_EXPR, type,
				const_binop (MULT_EXPR, arg0, arg1), arg2);
      if (integer_zerop (arg2))
	return fold_build2_loc (loc, MULT_EXPR, type, arg0, arg1);

      return fold_fma (loc, type, arg0, arg1, arg2);

    case VEC_PERM_EXPR:
      if (TREE_CODE (arg2) == VECTOR_CST)
	{
	  unsigned int nelts = TYPE_VECTOR_SUBPARTS (type), i, mask;
	  unsigned char *sel = XALLOCAVEC (unsigned char, nelts);
	  tree t;
	  bool need_mask_canon = false;
	  bool all_in_vec0 = true;
	  bool all_in_vec1 = true;
	  bool maybe_identity = true;
	  bool single_arg = (op0 == op1);
	  bool changed = false;

	  mask = single_arg ? (nelts - 1) : (2 * nelts - 1);
	  gcc_assert (nelts == VECTOR_CST_NELTS (arg2));
	  for (i = 0; i < nelts; i++)
	    {
	      tree val = VECTOR_CST_ELT (arg2, i);
	      if (TREE_CODE (val) != INTEGER_CST)
		return NULL_TREE;

	      sel[i] = TREE_INT_CST_LOW (val) & mask;
	      if (TREE_INT_CST_HIGH (val)
		  || ((unsigned HOST_WIDE_INT)
		      TREE_INT_CST_LOW (val) != sel[i]))
		need_mask_canon = true;

	      if (sel[i] < nelts)
		all_in_vec1 = false;
	      else
		all_in_vec0 = false;

	      if ((sel[i] & (nelts-1)) != i)
		maybe_identity = false;
	    }

	  if (maybe_identity)
	    {
	      if (all_in_vec0)
		return op0;
	      if (all_in_vec1)
		return op1;
	    }

	  if (all_in_vec0)
	    op1 = op0;
	  else if (all_in_vec1)
	    {
	      op0 = op1;
	      for (i = 0; i < nelts; i++)
		sel[i] -= nelts;
	      need_mask_canon = true;
	    }

	  if ((TREE_CODE (op0) == VECTOR_CST
	       || TREE_CODE (op0) == CONSTRUCTOR)
	      && (TREE_CODE (op1) == VECTOR_CST
		  || TREE_CODE (op1) == CONSTRUCTOR))
	    {
	      t = fold_vec_perm (type, op0, op1, sel);
	      if (t != NULL_TREE)
		return t;
	    }

	  if (op0 == op1 && !single_arg)
	    changed = true;

	  if (need_mask_canon && arg2 == op2)
	    {
	      tree *tsel = XALLOCAVEC (tree, nelts);
	      tree eltype = TREE_TYPE (TREE_TYPE (arg2));
	      for (i = 0; i < nelts; i++)
		tsel[i] = build_int_cst (eltype, sel[i]);
	      op2 = build_vector (TREE_TYPE (arg2), tsel);
	      changed = true;
	    }

	  if (changed)
	    return build3_loc (loc, VEC_PERM_EXPR, type, op0, op1, op2);
	}
      return NULL_TREE;

    default:
      return NULL_TREE;
    } /* switch (code) */
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
	    vec<constructor_elt, va_gc> *elts = CONSTRUCTOR_ELTS (op0);
	    unsigned HOST_WIDE_INT end = vec_safe_length (elts);
	    unsigned HOST_WIDE_INT begin = 0;

	    /* Find a matching index by means of a binary search.  */
	    while (begin != end)
	      {
		unsigned HOST_WIDE_INT middle = (begin + end) / 2;
		tree index = (*elts)[middle].index;

		if (TREE_CODE (index) == INTEGER_CST
		    && tree_int_cst_lt (index, op1))
		  begin = middle + 1;
		else if (TREE_CODE (index) == INTEGER_CST
			 && tree_int_cst_lt (op1, index))
		  end = middle;
		else if (TREE_CODE (index) == RANGE_EXPR
			 && tree_int_cst_lt (TREE_OPERAND (index, 1), op1))
		  begin = middle + 1;
		else if (TREE_CODE (index) == RANGE_EXPR
			 && tree_int_cst_lt (op1, TREE_OPERAND (index, 0)))
		  end = middle;
		else
		  return (*elts)[middle].value;
	      }
	  }

	return t;
      }

      /* Return a VECTOR_CST if possible.  */
    case CONSTRUCTOR:
      {
	tree type = TREE_TYPE (t);
	if (TREE_CODE (type) != VECTOR_TYPE)
	  return t;

	tree *vec = XALLOCAVEC (tree, TYPE_VECTOR_SUBPARTS (type));
	unsigned HOST_WIDE_INT idx, pos = 0;
	tree value;

	FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (t), idx, value)
	  {
	    if (!CONSTANT_CLASS_P (value))
	      return t;
	    if (TREE_CODE (value) == VECTOR_CST)
	      {
		for (unsigned i = 0; i < VECTOR_CST_NELTS (value); ++i)
		  vec[pos++] = VECTOR_CST_ELT (value, i);
	      }
	    else
	      vec[pos++] = value;
	  }
	for (; pos < TYPE_VECTOR_SUBPARTS (type); ++pos)
	  vec[pos] = build_zero_cst (TREE_TYPE (type));

	return build_vector (type, vec);
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
				hash_table <pointer_hash <tree_node> >);
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
  hash_table <pointer_hash <tree_node> > ht;

  ht.create (32);
  md5_init_ctx (&ctx);
  fold_checksum_tree (expr, &ctx, ht);
  md5_finish_ctx (&ctx, checksum_before);
  ht.empty ();

  ret = fold_1 (expr);

  md5_init_ctx (&ctx);
  fold_checksum_tree (expr, &ctx, ht);
  md5_finish_ctx (&ctx, checksum_after);
  ht.dispose ();

  if (memcmp (checksum_before, checksum_after, 16))
    fold_check_failed (expr, ret);

  return ret;
}

void
print_fold_checksum (const_tree expr)
{
  struct md5_ctx ctx;
  unsigned char checksum[16], cnt;
  hash_table <pointer_hash <tree_node> > ht;

  ht.create (32);
  md5_init_ctx (&ctx);
  fold_checksum_tree (expr, &ctx, ht);
  md5_finish_ctx (&ctx, checksum);
  ht.dispose ();
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
		    hash_table <pointer_hash <tree_node> > ht)
{
  tree_node **slot;
  enum tree_code code;
  union tree_node buf;
  int i, len;

 recursive_label:
  if (expr == NULL)
    return;
  slot = ht.find_slot (expr, INSERT);
  if (*slot != NULL)
    return;
  *slot = CONST_CAST_TREE (expr);
  code = TREE_CODE (expr);
  if (TREE_CODE_CLASS (code) == tcc_declaration
      && DECL_ASSEMBLER_NAME_SET_P (expr))
    {
      /* Allow DECL_ASSEMBLER_NAME to be modified.  */
      memcpy ((char *) &buf, expr, tree_size (expr));
      SET_DECL_ASSEMBLER_NAME ((tree)&buf, NULL);
      expr = (tree) &buf;
    }
  else if (TREE_CODE_CLASS (code) == tcc_type
	   && (TYPE_POINTER_TO (expr)
	       || TYPE_REFERENCE_TO (expr)
	       || TYPE_CACHED_VALUES_P (expr)
	       || TYPE_CONTAINS_PLACEHOLDER_INTERNAL (expr)
	       || TYPE_NEXT_VARIANT (expr)))
    {
      /* Allow these fields to be modified.  */
      tree tmp;
      memcpy ((char *) &buf, expr, tree_size (expr));
      expr = tmp = (tree) &buf;
      TYPE_CONTAINS_PLACEHOLDER_INTERNAL (tmp) = 0;
      TYPE_POINTER_TO (tmp) = NULL;
      TYPE_REFERENCE_TO (tmp) = NULL;
      TYPE_NEXT_VARIANT (tmp) = NULL;
      if (TYPE_CACHED_VALUES_P (tmp))
	{
	  TYPE_CACHED_VALUES_P (tmp) = 0;
	  TYPE_CACHED_VALUES (tmp) = NULL;
	}
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
	  for (i = 0; i < (int) VECTOR_CST_NELTS (expr); ++i)
	    fold_checksum_tree (VECTOR_CST_ELT (expr, i), ctx, ht);
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
      if (CODE_CONTAINS_STRUCT (TREE_CODE (expr), TS_DECL_WITH_VIS))
	fold_checksum_tree (DECL_SECTION_NAME (expr), ctx, ht);

      if (CODE_CONTAINS_STRUCT (TREE_CODE (expr), TS_DECL_NON_COMMON))
	{
	  fold_checksum_tree (DECL_VINDEX (expr), ctx, ht);
	  fold_checksum_tree (DECL_RESULT_FLD (expr), ctx, ht);
	  fold_checksum_tree (DECL_ARGUMENT_FLD (expr), ctx, ht);
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
      if (TREE_CODE (expr) == RECORD_TYPE
	  || TREE_CODE (expr) == UNION_TYPE
	  || TREE_CODE (expr) == QUAL_UNION_TYPE)
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
  hash_table <pointer_hash <tree_node> > ht;
  ht.create (32);

  md5_init_ctx (&ctx);
  fold_checksum_tree (t, &ctx, ht);
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
fold_build1_stat_loc (location_t loc,
		      enum tree_code code, tree type, tree op0 MEM_STAT_DECL)
{
  tree tem;
#ifdef ENABLE_FOLD_CHECKING
  unsigned char checksum_before[16], checksum_after[16];
  struct md5_ctx ctx;
  hash_table <pointer_hash <tree_node> > ht;

  ht.create (32);
  md5_init_ctx (&ctx);
  fold_checksum_tree (op0, &ctx, ht);
  md5_finish_ctx (&ctx, checksum_before);
  ht.empty ();
#endif

  tem = fold_unary_loc (loc, code, type, op0);
  if (!tem)
    tem = build1_stat_loc (loc, code, type, op0 PASS_MEM_STAT);

#ifdef ENABLE_FOLD_CHECKING
  md5_init_ctx (&ctx);
  fold_checksum_tree (op0, &ctx, ht);
  md5_finish_ctx (&ctx, checksum_after);
  ht.dispose ();

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
fold_build2_stat_loc (location_t loc,
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
  hash_table <pointer_hash <tree_node> > ht;

  ht.create (32);
  md5_init_ctx (&ctx);
  fold_checksum_tree (op0, &ctx, ht);
  md5_finish_ctx (&ctx, checksum_before_op0);
  ht.empty ();

  md5_init_ctx (&ctx);
  fold_checksum_tree (op1, &ctx, ht);
  md5_finish_ctx (&ctx, checksum_before_op1);
  ht.empty ();
#endif

  tem = fold_binary_loc (loc, code, type, op0, op1);
  if (!tem)
    tem = build2_stat_loc (loc, code, type, op0, op1 PASS_MEM_STAT);

#ifdef ENABLE_FOLD_CHECKING
  md5_init_ctx (&ctx);
  fold_checksum_tree (op0, &ctx, ht);
  md5_finish_ctx (&ctx, checksum_after_op0);
  ht.empty ();

  if (memcmp (checksum_before_op0, checksum_after_op0, 16))
    fold_check_failed (op0, tem);

  md5_init_ctx (&ctx);
  fold_checksum_tree (op1, &ctx, ht);
  md5_finish_ctx (&ctx, checksum_after_op1);
  ht.dispose ();

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
fold_build3_stat_loc (location_t loc, enum tree_code code, tree type,
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
  hash_table <pointer_hash <tree_node> > ht;

  ht.create (32);
  md5_init_ctx (&ctx);
  fold_checksum_tree (op0, &ctx, ht);
  md5_finish_ctx (&ctx, checksum_before_op0);
  ht.empty ();

  md5_init_ctx (&ctx);
  fold_checksum_tree (op1, &ctx, ht);
  md5_finish_ctx (&ctx, checksum_before_op1);
  ht.empty ();

  md5_init_ctx (&ctx);
  fold_checksum_tree (op2, &ctx, ht);
  md5_finish_ctx (&ctx, checksum_before_op2);
  ht.empty ();
#endif

  gcc_assert (TREE_CODE_CLASS (code) != tcc_vl_exp);
  tem = fold_ternary_loc (loc, code, type, op0, op1, op2);
  if (!tem)
    tem = build3_stat_loc (loc, code, type, op0, op1, op2 PASS_MEM_STAT);

#ifdef ENABLE_FOLD_CHECKING
  md5_init_ctx (&ctx);
  fold_checksum_tree (op0, &ctx, ht);
  md5_finish_ctx (&ctx, checksum_after_op0);
  ht.empty ();

  if (memcmp (checksum_before_op0, checksum_after_op0, 16))
    fold_check_failed (op0, tem);

  md5_init_ctx (&ctx);
  fold_checksum_tree (op1, &ctx, ht);
  md5_finish_ctx (&ctx, checksum_after_op1);
  ht.empty ();

  if (memcmp (checksum_before_op1, checksum_after_op1, 16))
    fold_check_failed (op1, tem);

  md5_init_ctx (&ctx);
  fold_checksum_tree (op2, &ctx, ht);
  md5_finish_ctx (&ctx, checksum_after_op2);
  ht.dispose ();

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
  hash_table <pointer_hash <tree_node> > ht;
  int i;

  ht.create (32);
  md5_init_ctx (&ctx);
  fold_checksum_tree (fn, &ctx, ht);
  md5_finish_ctx (&ctx, checksum_before_fn);
  ht.empty ();

  md5_init_ctx (&ctx);
  for (i = 0; i < nargs; i++)
    fold_checksum_tree (argarray[i], &ctx, ht);
  md5_finish_ctx (&ctx, checksum_before_arglist);
  ht.empty ();
#endif

  tem = fold_builtin_call_array (loc, type, fn, nargs, argarray);

#ifdef ENABLE_FOLD_CHECKING
  md5_init_ctx (&ctx);
  fold_checksum_tree (fn, &ctx, ht);
  md5_finish_ctx (&ctx, checksum_after_fn);
  ht.empty ();

  if (memcmp (checksum_before_fn, checksum_after_fn, 16))
    fold_check_failed (fn, tem);

  md5_init_ctx (&ctx);
  for (i = 0; i < nargs; i++)
    fold_checksum_tree (argarray[i], &ctx, ht);
  md5_finish_ctx (&ctx, checksum_after_arglist);
  ht.dispose ();

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
fold_build3_initializer_loc (location_t loc, enum tree_code code,
			     tree type, tree op0, tree op1, tree op2)
{
  tree result;
  START_FOLD_INIT;

  result = fold_build3_loc (loc, code, type, op0, op1, op2);

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

#undef START_FOLD_INIT
#undef END_FOLD_INIT

/* Determine if first argument is a multiple of second argument.  Return 0 if
   it is not, or we cannot easily determined it to be.

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
   transformed version).  */

int
multiple_of_p (tree type, const_tree top, const_tree bottom)
{
  if (operand_equal_p (top, bottom, 0))
    return 1;

  if (TREE_CODE (type) != INTEGER_TYPE)
    return 0;

  switch (TREE_CODE (top))
    {
    case BIT_AND_EXPR:
      /* Bitwise and provides a power of two multiple.  If the mask is
	 a multiple of BOTTOM then TOP is a multiple of BOTTOM.  */
      if (!integer_pow2p (bottom))
	return 0;
      /* FALLTHRU */

    case MULT_EXPR:
      return (multiple_of_p (type, TREE_OPERAND (top, 0), bottom)
	      || multiple_of_p (type, TREE_OPERAND (top, 1), bottom));

    case PLUS_EXPR:
    case MINUS_EXPR:
      return (multiple_of_p (type, TREE_OPERAND (top, 0), bottom)
	      && multiple_of_p (type, TREE_OPERAND (top, 1), bottom));

    case LSHIFT_EXPR:
      if (TREE_CODE (TREE_OPERAND (top, 1)) == INTEGER_CST)
	{
	  tree op1, t1;

	  op1 = TREE_OPERAND (top, 1);
	  /* const_binop may not detect overflow correctly,
	     so check for it explicitly here.  */
	  if (TYPE_PRECISION (TREE_TYPE (size_one_node))
	      > TREE_INT_CST_LOW (op1)
	      && TREE_INT_CST_HIGH (op1) == 0
	      && 0 != (t1 = fold_convert (type,
					  const_binop (LSHIFT_EXPR,
						       size_one_node,
						       op1)))
	      && !TREE_OVERFLOW (t1))
	    return multiple_of_p (type, t1, bottom);
	}
      return 0;

    case NOP_EXPR:
      /* Can't handle conversions from non-integral or wider integral type.  */
      if ((TREE_CODE (TREE_TYPE (TREE_OPERAND (top, 0))) != INTEGER_TYPE)
	  || (TYPE_PRECISION (type)
	      < TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (top, 0)))))
	return 0;

      /* .. fall through ...  */

    case SAVE_EXPR:
      return multiple_of_p (type, TREE_OPERAND (top, 0), bottom);

    case COND_EXPR:
      return (multiple_of_p (type, TREE_OPERAND (top, 1), bottom)
	      && multiple_of_p (type, TREE_OPERAND (top, 2), bottom));

    case INTEGER_CST:
      if (TREE_CODE (bottom) != INTEGER_CST
	  || integer_zerop (bottom)
	  || (TYPE_UNSIGNED (type)
	      && (tree_int_cst_sgn (top) < 0
		  || tree_int_cst_sgn (bottom) < 0)))
	return 0;
      return integer_zerop (int_const_binop (TRUNC_MOD_EXPR,
					     top, bottom));

    default:
      return 0;
    }
}

/* Return true if CODE or TYPE is known to be non-negative. */

static bool
tree_simple_nonnegative_warnv_p (enum tree_code code, tree type)
{
  if ((TYPE_PRECISION (type) != 1 || TYPE_UNSIGNED (type))
      && truth_value_p (code))
    /* Truth values evaluate to 0 or 1, which is nonnegative unless we
       have a signed:1 type (where the value is -1 and 0).  */
    return true;
  return false;
}

/* Return true if (CODE OP0) is known to be non-negative.  If the return
   value is based on the assumption that signed overflow is undefined,
   set *STRICT_OVERFLOW_P to true; otherwise, don't change
   *STRICT_OVERFLOW_P.  */

bool
tree_unary_nonnegative_warnv_p (enum tree_code code, tree type, tree op0,
				bool *strict_overflow_p)
{
  if (TYPE_UNSIGNED (type))
    return true;

  switch (code)
    {
    case ABS_EXPR:
      /* We can't return 1 if flag_wrapv is set because
	 ABS_EXPR<INT_MIN> = INT_MIN.  */
      if (!INTEGRAL_TYPE_P (type))
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
      return tree_expr_nonnegative_warnv_p (op0,
					    strict_overflow_p);

    case NOP_EXPR:
      {
	tree inner_type = TREE_TYPE (op0);
	tree outer_type = type;

	if (TREE_CODE (outer_type) == REAL_TYPE)
	  {
	    if (TREE_CODE (inner_type) == REAL_TYPE)
	      return tree_expr_nonnegative_warnv_p (op0,
						    strict_overflow_p);
	    if (INTEGRAL_TYPE_P (inner_type))
	      {
		if (TYPE_UNSIGNED (inner_type))
		  return true;
		return tree_expr_nonnegative_warnv_p (op0,
						      strict_overflow_p);
	      }
	  }
	else if (INTEGRAL_TYPE_P (outer_type))
	  {
	    if (TREE_CODE (inner_type) == REAL_TYPE)
	      return tree_expr_nonnegative_warnv_p (op0,
						    strict_overflow_p);
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
   *STRICT_OVERFLOW_P.  */

bool
tree_binary_nonnegative_warnv_p (enum tree_code code, tree type, tree op0,
				      tree op1, bool *strict_overflow_p)
{
  if (TYPE_UNSIGNED (type))
    return true;

  switch (code)
    {
    case POINTER_PLUS_EXPR:
    case PLUS_EXPR:
      if (FLOAT_TYPE_P (type))
	return (tree_expr_nonnegative_warnv_p (op0,
					       strict_overflow_p)
		&& tree_expr_nonnegative_warnv_p (op1,
						  strict_overflow_p));

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
	      || (tree_expr_nonnegative_warnv_p (op0, strict_overflow_p)
		  && tree_expr_nonnegative_warnv_p (op1, strict_overflow_p)))
	    {
	      if (TYPE_OVERFLOW_UNDEFINED (type))
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
		? tree_int_cst_min_precision (op0, /*unsignedp=*/true)
		: TYPE_PRECISION (inner0);

	      unsigned int precision1 = (TREE_CODE (op1) == INTEGER_CST)
		? tree_int_cst_min_precision (op1, /*unsignedp=*/true)
		: TYPE_PRECISION (inner1);

	      return precision0 + precision1 < TYPE_PRECISION (type);
	    }
	}
      return false;

    case BIT_AND_EXPR:
    case MAX_EXPR:
      return (tree_expr_nonnegative_warnv_p (op0,
					     strict_overflow_p)
	      || tree_expr_nonnegative_warnv_p (op1,
						strict_overflow_p));

    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case MIN_EXPR:
    case RDIV_EXPR:
    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
      return (tree_expr_nonnegative_warnv_p (op0,
					     strict_overflow_p)
	      && tree_expr_nonnegative_warnv_p (op1,
						strict_overflow_p));

    case TRUNC_MOD_EXPR:
    case CEIL_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case ROUND_MOD_EXPR:
      return tree_expr_nonnegative_warnv_p (op0,
					    strict_overflow_p);
    default:
      return tree_simple_nonnegative_warnv_p (code, type);
    }

  /* We don't know sign of `t', so be conservative and return false.  */
  return false;
}

/* Return true if T is known to be non-negative.  If the return
   value is based on the assumption that signed overflow is undefined,
   set *STRICT_OVERFLOW_P to true; otherwise, don't change
   *STRICT_OVERFLOW_P.  */

bool
tree_single_nonnegative_warnv_p (tree t, bool *strict_overflow_p)
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
      return (tree_expr_nonnegative_warnv_p (TREE_OPERAND (t, 1),
					     strict_overflow_p)
	      && tree_expr_nonnegative_warnv_p (TREE_OPERAND (t, 2),
						strict_overflow_p));
    default:
      return tree_simple_nonnegative_warnv_p (TREE_CODE (t),
						   TREE_TYPE (t));
    }
  /* We don't know sign of `t', so be conservative and return false.  */
  return false;
}

/* Return true if T is known to be non-negative.  If the return
   value is based on the assumption that signed overflow is undefined,
   set *STRICT_OVERFLOW_P to true; otherwise, don't change
   *STRICT_OVERFLOW_P.  */

bool
tree_call_nonnegative_warnv_p (tree type, tree fndecl,
			       tree arg0, tree arg1, bool *strict_overflow_p)
{
  if (fndecl && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL)
    switch (DECL_FUNCTION_CODE (fndecl))
      {
	CASE_FLT_FN (BUILT_IN_ACOS):
	CASE_FLT_FN (BUILT_IN_ACOSH):
	CASE_FLT_FN (BUILT_IN_CABS):
	CASE_FLT_FN (BUILT_IN_COSH):
	CASE_FLT_FN (BUILT_IN_ERFC):
	CASE_FLT_FN (BUILT_IN_EXP):
	CASE_FLT_FN (BUILT_IN_EXP10):
	CASE_FLT_FN (BUILT_IN_EXP2):
	CASE_FLT_FN (BUILT_IN_FABS):
	CASE_FLT_FN (BUILT_IN_FDIM):
	CASE_FLT_FN (BUILT_IN_HYPOT):
	CASE_FLT_FN (BUILT_IN_POW10):
	CASE_INT_FN (BUILT_IN_FFS):
	CASE_INT_FN (BUILT_IN_PARITY):
	CASE_INT_FN (BUILT_IN_POPCOUNT):
	CASE_INT_FN (BUILT_IN_CLZ):
	CASE_INT_FN (BUILT_IN_CLRSB):
      case BUILT_IN_BSWAP32:
      case BUILT_IN_BSWAP64:
	/* Always true.  */
	return true;

	CASE_FLT_FN (BUILT_IN_SQRT):
	/* sqrt(-0.0) is -0.0.  */
	if (!HONOR_SIGNED_ZEROS (TYPE_MODE (type)))
	  return true;
	return tree_expr_nonnegative_warnv_p (arg0,
					      strict_overflow_p);

	CASE_FLT_FN (BUILT_IN_ASINH):
	CASE_FLT_FN (BUILT_IN_ATAN):
	CASE_FLT_FN (BUILT_IN_ATANH):
	CASE_FLT_FN (BUILT_IN_CBRT):
	CASE_FLT_FN (BUILT_IN_CEIL):
	CASE_FLT_FN (BUILT_IN_ERF):
	CASE_FLT_FN (BUILT_IN_EXPM1):
	CASE_FLT_FN (BUILT_IN_FLOOR):
	CASE_FLT_FN (BUILT_IN_FMOD):
	CASE_FLT_FN (BUILT_IN_FREXP):
	CASE_FLT_FN (BUILT_IN_ICEIL):
	CASE_FLT_FN (BUILT_IN_IFLOOR):
	CASE_FLT_FN (BUILT_IN_IRINT):
	CASE_FLT_FN (BUILT_IN_IROUND):
	CASE_FLT_FN (BUILT_IN_LCEIL):
	CASE_FLT_FN (BUILT_IN_LDEXP):
	CASE_FLT_FN (BUILT_IN_LFLOOR):
	CASE_FLT_FN (BUILT_IN_LLCEIL):
	CASE_FLT_FN (BUILT_IN_LLFLOOR):
	CASE_FLT_FN (BUILT_IN_LLRINT):
	CASE_FLT_FN (BUILT_IN_LLROUND):
	CASE_FLT_FN (BUILT_IN_LRINT):
	CASE_FLT_FN (BUILT_IN_LROUND):
	CASE_FLT_FN (BUILT_IN_MODF):
	CASE_FLT_FN (BUILT_IN_NEARBYINT):
	CASE_FLT_FN (BUILT_IN_RINT):
	CASE_FLT_FN (BUILT_IN_ROUND):
	CASE_FLT_FN (BUILT_IN_SCALB):
	CASE_FLT_FN (BUILT_IN_SCALBLN):
	CASE_FLT_FN (BUILT_IN_SCALBN):
	CASE_FLT_FN (BUILT_IN_SIGNBIT):
	CASE_FLT_FN (BUILT_IN_SIGNIFICAND):
	CASE_FLT_FN (BUILT_IN_SINH):
	CASE_FLT_FN (BUILT_IN_TANH):
	CASE_FLT_FN (BUILT_IN_TRUNC):
	/* True if the 1st argument is nonnegative.  */
	return tree_expr_nonnegative_warnv_p (arg0,
					      strict_overflow_p);

	CASE_FLT_FN (BUILT_IN_FMAX):
	/* True if the 1st OR 2nd arguments are nonnegative.  */
	return (tree_expr_nonnegative_warnv_p (arg0,
					       strict_overflow_p)
		|| (tree_expr_nonnegative_warnv_p (arg1,
						   strict_overflow_p)));

	CASE_FLT_FN (BUILT_IN_FMIN):
	/* True if the 1st AND 2nd arguments are nonnegative.  */
	return (tree_expr_nonnegative_warnv_p (arg0,
					       strict_overflow_p)
		&& (tree_expr_nonnegative_warnv_p (arg1,
						   strict_overflow_p)));

	CASE_FLT_FN (BUILT_IN_COPYSIGN):
	/* True if the 2nd argument is nonnegative.  */
	return tree_expr_nonnegative_warnv_p (arg1,
					      strict_overflow_p);

	CASE_FLT_FN (BUILT_IN_POWI):
	/* True if the 1st argument is nonnegative or the second
	   argument is an even integer.  */
	if (TREE_CODE (arg1) == INTEGER_CST
	    && (TREE_INT_CST_LOW (arg1) & 1) == 0)
	  return true;
	return tree_expr_nonnegative_warnv_p (arg0,
					      strict_overflow_p);

	CASE_FLT_FN (BUILT_IN_POW):
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
		real_from_integer (&cint, VOIDmode, n,
				   n < 0 ? -1 : 0, 0);
		if (real_identical (&c, &cint))
		  return true;
	      }
	  }
	return tree_expr_nonnegative_warnv_p (arg0,
					      strict_overflow_p);

      default:
	break;
      }
  return tree_simple_nonnegative_warnv_p (CALL_EXPR,
					  type);
}

/* Return true if T is known to be non-negative.  If the return
   value is based on the assumption that signed overflow is undefined,
   set *STRICT_OVERFLOW_P to true; otherwise, don't change
   *STRICT_OVERFLOW_P.  */

bool
tree_invalid_nonnegative_warnv_p (tree t, bool *strict_overflow_p)
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
	if (!VOID_TYPE_P (t))
	  return tree_expr_nonnegative_warnv_p (t, strict_overflow_p);

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
	  return tree_expr_nonnegative_warnv_p (TREE_OPERAND (t, 1),
						strict_overflow_p);

	return false;
      }

    case CALL_EXPR:
      {
	tree arg0 = call_expr_nargs (t) > 0 ?  CALL_EXPR_ARG (t, 0) : NULL_TREE;
	tree arg1 = call_expr_nargs (t) > 1 ?  CALL_EXPR_ARG (t, 1) : NULL_TREE;

	return tree_call_nonnegative_warnv_p (TREE_TYPE (t),
					      get_callee_fndecl (t),
					      arg0,
					      arg1,
					      strict_overflow_p);
      }
    case COMPOUND_EXPR:
    case MODIFY_EXPR:
      return tree_expr_nonnegative_warnv_p (TREE_OPERAND (t, 1),
					    strict_overflow_p);
    case BIND_EXPR:
      return tree_expr_nonnegative_warnv_p (expr_last (TREE_OPERAND (t, 1)),
					    strict_overflow_p);
    case SAVE_EXPR:
      return tree_expr_nonnegative_warnv_p (TREE_OPERAND (t, 0),
					    strict_overflow_p);

    default:
      return tree_simple_nonnegative_warnv_p (TREE_CODE (t),
						   TREE_TYPE (t));
    }

  /* We don't know sign of `t', so be conservative and return false.  */
  return false;
}

/* Return true if T is known to be non-negative.  If the return
   value is based on the assumption that signed overflow is undefined,
   set *STRICT_OVERFLOW_P to true; otherwise, don't change
   *STRICT_OVERFLOW_P.  */

bool
tree_expr_nonnegative_warnv_p (tree t, bool *strict_overflow_p)
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
					      strict_overflow_p);

    case tcc_unary:
      return tree_unary_nonnegative_warnv_p (TREE_CODE (t),
					     TREE_TYPE (t),
					     TREE_OPERAND (t, 0),
					     strict_overflow_p);

    case tcc_constant:
    case tcc_declaration:
    case tcc_reference:
      return tree_single_nonnegative_warnv_p (t, strict_overflow_p);

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
					      strict_overflow_p);
    case TRUTH_NOT_EXPR:
      return tree_unary_nonnegative_warnv_p (TREE_CODE (t),
					     TREE_TYPE (t),
					     TREE_OPERAND (t, 0),
					     strict_overflow_p);

    case COND_EXPR:
    case CONSTRUCTOR:
    case OBJ_TYPE_REF:
    case ASSERT_EXPR:
    case ADDR_EXPR:
    case WITH_SIZE_EXPR:
    case SSA_NAME:
      return tree_single_nonnegative_warnv_p (t, strict_overflow_p);

    default:
      return tree_invalid_nonnegative_warnv_p (t, strict_overflow_p);
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
      if (TYPE_OVERFLOW_UNDEFINED (type))
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

	if (!base)
	  return false;

	/* Weak declarations may link to NULL.  Other things may also be NULL
	   so protect with -fdelete-null-pointer-checks; but not variables
	   allocated on the stack.  */
	if (DECL_P (base)
	    && (flag_delete_null_pointer_checks
		|| (DECL_CONTEXT (base)
		    && TREE_CODE (DECL_CONTEXT (base)) == FUNCTION_DECL
		    && auto_var_in_fn_p (base, DECL_CONTEXT (base)))))
	  return !VAR_OR_FUNCTION_DECL_P (base) || !DECL_WEAK (base);

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
    case ASSERT_EXPR:
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
	    && DECL_IS_OPERATOR_NEW (fndecl)
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
  if ((TREE_CODE (exp) == INDIRECT_REF
       || TREE_CODE (exp) == ARRAY_REF)
      && TREE_CODE (TREE_TYPE (exp)) == INTEGER_TYPE)
    {
      tree exp1 = TREE_OPERAND (exp, 0);
      tree index;
      tree string;
      location_t loc = EXPR_LOCATION (exp);

      if (TREE_CODE (exp) == INDIRECT_REF)
	string = string_constant (exp1, &index);
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

      if (string
	  && TYPE_MODE (TREE_TYPE (exp)) == TYPE_MODE (TREE_TYPE (TREE_TYPE (string)))
	  && TREE_CODE (string) == STRING_CST
	  && TREE_CODE (index) == INTEGER_CST
	  && compare_tree_int (index, TREE_STRING_LENGTH (string)) < 0
	  && (GET_MODE_CLASS (TYPE_MODE (TREE_TYPE (TREE_TYPE (string))))
	      == MODE_INT)
	  && (GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (TREE_TYPE (string)))) == 1))
	return build_int_cst_type (TREE_TYPE (exp),
				   (TREE_STRING_POINTER (string)
				    [TREE_INT_CST_LOW (index)]));
    }
  return NULL;
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
    case INTEGER_CST:
      {
	double_int val = tree_to_double_int (arg0);
	bool overflow;
	val = val.neg_with_overflow (&overflow);
	t = force_fit_type_double (type, val, 1,
				   (overflow | TREE_OVERFLOW (arg0))
				   && !TYPE_UNSIGNED (type));
	break;
      }

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
	double_int val = tree_to_double_int (arg0);

        /* If the value is unsigned or non-negative, then the absolute value
	   is the same as the ordinary value.  */
	if (TYPE_UNSIGNED (type)
	    || !val.is_negative ())
	  t = arg0;

	/* If the value is negative, then the absolute value is
	   its negation.  */
	else
	  {
	    bool overflow;
	    val = val.neg_with_overflow (&overflow);
	    t = force_fit_type_double (type, val, -1,
				       overflow | TREE_OVERFLOW (arg0));
	  }
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
  double_int val;  

  gcc_assert (TREE_CODE (arg0) == INTEGER_CST);

  val = ~tree_to_double_int (arg0);
  return force_fit_type_double (type, val, 0, TREE_OVERFLOW (arg0));
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
      unsigned count = VECTOR_CST_NELTS (op0);
      tree *elts =  XALLOCAVEC (tree, count);
      gcc_assert (VECTOR_CST_NELTS (op1) == count
		  && TYPE_VECTOR_SUBPARTS (type) == count);

      for (unsigned i = 0; i < count; i++)
	{
	  tree elem_type = TREE_TYPE (type);
	  tree elem0 = VECTOR_CST_ELT (op0, i);
	  tree elem1 = VECTOR_CST_ELT (op1, i);

	  tree tem = fold_relational_const (code, elem_type,
					    elem0, elem1);

	  if (tem == NULL_TREE)
	    return NULL_TREE;

	  elts[i] = build_int_cst (elem_type, integer_zerop (tem) ? 0 : -1);
	}

      return build_vector (type, elts);
    }

  /* From here on we only handle LT, LE, GT, GE, EQ and NE.

     To compute GT, swap the arguments and do LT.
     To compute GE, do LT and invert the result.
     To compute LE, swap the arguments, do LT and invert the result.
     To compute NE, do EQ and invert the result.

     Therefore, the code below must handle only EQ and LT.  */

  if (code == LE_EXPR || code == GT_EXPR)
    {
      tree tem = op0;
      op0 = op1;
      op1 = tem;
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
      else if (TYPE_UNSIGNED (TREE_TYPE (op0)))
	result = INT_CST_LT_UNSIGNED (op0, op1);
      else
	result = INT_CST_LT (op0, op1);
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

  return build1 (CLEANUP_POINT_EXPR, type, expr);
}

/* Given a pointer value OP0 and a type TYPE, return a simplified version
   of an indirection through OP0, or NULL_TREE if no simplification is
   possible.  */

tree
fold_indirect_ref_1 (location_t loc, tree type, tree op0)
{
  tree sub = op0;
  tree subtype;

  STRIP_NOPS (sub);
  subtype = TREE_TYPE (sub);
  if (!POINTER_TYPE_P (subtype))
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
      else if (TREE_CODE (optype) == VECTOR_TYPE
	       && type == TREE_TYPE (optype))
	{
	  tree part_width = TYPE_SIZE (type);
	  tree index = bitsize_int (0);
	  return fold_build3_loc (loc, BIT_FIELD_REF, type, op, part_width, index);
	}
    }

  if (TREE_CODE (sub) == POINTER_PLUS_EXPR
      && TREE_CODE (TREE_OPERAND (sub, 1)) == INTEGER_CST)
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
	  if (TREE_CODE (op00type) == VECTOR_TYPE
	      && type == TREE_TYPE (op00type))
	    {
	      HOST_WIDE_INT offset = tree_low_cst (op01, 0);
	      tree part_width = TYPE_SIZE (type);
	      unsigned HOST_WIDE_INT part_widthi = tree_low_cst (part_width, 0)/BITS_PER_UNIT;
	      unsigned HOST_WIDE_INT indexi = offset * BITS_PER_UNIT;
	      tree index = bitsize_int (indexi);

	      if (offset/part_widthi <= TYPE_VECTOR_SUBPARTS (op00type))
		return fold_build3_loc (loc,
					BIT_FIELD_REF, type, op00,
					part_width, index);

	    }
	  /* ((foo*)&complexfoo)[1] => __imag__ complexfoo */
	  else if (TREE_CODE (op00type) == COMPLEX_TYPE
		   && type == TREE_TYPE (op00type))
	    {
	      tree size = TYPE_SIZE_UNIT (type);
	      if (tree_int_cst_equal (size, op01))
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
	      op01 = size_binop_loc (loc, EXACT_DIV_EXPR, op01,
				     TYPE_SIZE_UNIT (type));
	      op01 = size_binop_loc (loc, PLUS_EXPR, op01, min_val);
	      return build4_loc (loc, ARRAY_REF, type, op00, op01,
				 NULL_TREE, NULL_TREE);
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

/* Return the value of VALUE, rounded up to a multiple of DIVISOR.
   This can only be applied to objects of a sizetype.  */

tree
round_up_loc (location_t loc, tree value, int divisor)
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
  if (divisor == (divisor & -divisor))
    {
      if (TREE_CODE (value) == INTEGER_CST)
	{
	  double_int val = tree_to_double_int (value);
	  bool overflow_p;

	  if ((val.low & (divisor - 1)) == 0)
	    return value;

	  overflow_p = TREE_OVERFLOW (value);
	  val.low &= ~(divisor - 1);
	  val.low += divisor;
	  if (val.low == 0)
	    {
	      val.high++;
	      if (val.high == 0)
		overflow_p = true;
	    }

	  return force_fit_type_double (TREE_TYPE (value), val,
					-1, overflow_p);
	}
      else
	{
	  tree t;

	  t = build_int_cst (TREE_TYPE (value), divisor - 1);
	  value = size_binop_loc (loc, PLUS_EXPR, value, t);
	  t = build_int_cst (TREE_TYPE (value), -divisor);
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
  if (divisor == (divisor & -divisor))
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
				  HOST_WIDE_INT *pbitpos, tree *poffset)
{
  tree core;
  enum machine_mode mode;
  int unsignedp, volatilep;
  HOST_WIDE_INT bitsize;
  location_t loc = EXPR_LOCATION (exp);

  if (TREE_CODE (exp) == ADDR_EXPR)
    {
      core = get_inner_reference (TREE_OPERAND (exp, 0), &bitsize, pbitpos,
				  poffset, &mode, &unsignedp, &volatilep,
				  false);
      core = build_fold_addr_expr_loc (loc, core);
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
ptr_difference_const (tree e1, tree e2, HOST_WIDE_INT *diff)
{
  tree core1, core2;
  HOST_WIDE_INT bitpos1, bitpos2;
  tree toffset1, toffset2, tdiff, type;

  core1 = split_address_to_core_and_offset (e1, &bitpos1, &toffset1);
  core2 = split_address_to_core_and_offset (e2, &bitpos2, &toffset2);

  if (bitpos1 % BITS_PER_UNIT != 0
      || bitpos2 % BITS_PER_UNIT != 0
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

  *diff += (bitpos1 - bitpos2) / BITS_PER_UNIT;
  return true;
}

/* Simplify the floating point expression EXP when the sign of the
   result is not significant.  Return NULL_TREE if no simplification
   is possible.  */

tree
fold_strip_sign_ops (tree exp)
{
  tree arg0, arg1;
  location_t loc = EXPR_LOCATION (exp);

  switch (TREE_CODE (exp))
    {
    case ABS_EXPR:
    case NEGATE_EXPR:
      arg0 = fold_strip_sign_ops (TREE_OPERAND (exp, 0));
      return arg0 ? arg0 : TREE_OPERAND (exp, 0);

    case MULT_EXPR:
    case RDIV_EXPR:
      if (HONOR_SIGN_DEPENDENT_ROUNDING (TYPE_MODE (TREE_TYPE (exp))))
	return NULL_TREE;
      arg0 = fold_strip_sign_ops (TREE_OPERAND (exp, 0));
      arg1 = fold_strip_sign_ops (TREE_OPERAND (exp, 1));
      if (arg0 != NULL_TREE || arg1 != NULL_TREE)
	return fold_build2_loc (loc, TREE_CODE (exp), TREE_TYPE (exp),
			    arg0 ? arg0 : TREE_OPERAND (exp, 0),
			    arg1 ? arg1 : TREE_OPERAND (exp, 1));
      break;

    case COMPOUND_EXPR:
      arg0 = TREE_OPERAND (exp, 0);
      arg1 = fold_strip_sign_ops (TREE_OPERAND (exp, 1));
      if (arg1)
	return fold_build2_loc (loc, COMPOUND_EXPR, TREE_TYPE (exp), arg0, arg1);
      break;

    case COND_EXPR:
      arg0 = fold_strip_sign_ops (TREE_OPERAND (exp, 1));
      arg1 = fold_strip_sign_ops (TREE_OPERAND (exp, 2));
      if (arg0 || arg1)
	return fold_build3_loc (loc,
			    COND_EXPR, TREE_TYPE (exp), TREE_OPERAND (exp, 0),
			    arg0 ? arg0 : TREE_OPERAND (exp, 1),
			    arg1 ? arg1 : TREE_OPERAND (exp, 2));
      break;

    case CALL_EXPR:
      {
	const enum built_in_function fcode = builtin_mathfn_code (exp);
	switch (fcode)
	{
	CASE_FLT_FN (BUILT_IN_COPYSIGN):
	  /* Strip copysign function call, return the 1st argument. */
	  arg0 = CALL_EXPR_ARG (exp, 0);
	  arg1 = CALL_EXPR_ARG (exp, 1);
	  return omit_one_operand_loc (loc, TREE_TYPE (exp), arg0, arg1);

	default:
	  /* Strip sign ops from the argument of "odd" math functions.  */
	  if (negate_mathfn_p (fcode))
            {
	      arg0 = fold_strip_sign_ops (CALL_EXPR_ARG (exp, 0));
	      if (arg0)
		return build_call_expr_loc (loc, get_callee_fndecl (exp), 1, arg0);
	    }
	  break;
	}
      }
      break;

    default:
      break;
    }
  return NULL_TREE;
}
