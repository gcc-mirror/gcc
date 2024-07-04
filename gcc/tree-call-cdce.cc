/* Conditional Dead Call Elimination pass for the GNU compiler.
   Copyright (C) 2008-2024 Free Software Foundation, Inc.
   Contributed by Xinliang David Li <davidxl@google.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-pretty-print.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "tree-into-ssa.h"
#include "builtins.h"
#include "internal-fn.h"
#include "tree-dfa.h"


/* This pass serves two closely-related purposes:

   1. It conditionally executes calls that set errno if (a) the result of
      the call is unused and (b) a simple range check on the arguments can
      detect most cases where errno does not need to be set.

      This is the "conditional dead-code elimination" that gave the pass
      its original name, since the call is dead for most argument values.
      The calls for which it helps are usually part of the C++ abstraction
      penalty exposed after inlining.

   2. It looks for calls to built-in functions that set errno and whose
      result is used.  It checks whether there is an associated internal
      function that doesn't set errno and whether the target supports
      that internal function.  If so, the pass uses the internal function
      to compute the result of the built-in function but still arranges
      for errno to be set when necessary.  There are two ways of setting
      errno:

      a. by protecting the original call with the same argument checks as (1)

      b. by protecting the original call with a check that the result
	 of the internal function is not equal to itself (i.e. is NaN).

      (b) requires that NaNs are the only erroneous results.  It is not
      appropriate for functions like log, which returns ERANGE for zero
      arguments.  (b) is also likely to perform worse than (a) because it
      requires the result to be calculated first.  The pass therefore uses
      (a) when it can and uses (b) as a fallback.

      For (b) the pass can replace the original call with a call to
      IFN_SET_EDOM, if the target supports direct assignments to errno.

   In both cases, arguments that require errno to be set should occur
   rarely in practice.  Checks of the errno result should also be rare,
   but the compiler would need powerful interprocedural analysis to
   prove that errno is not checked.  It's much easier to add argument
   checks or result checks instead.

     An example of (1) is:

	 log (x);   // Mostly dead call
     ==>
	 if (__builtin_islessequal (x, 0))
	     log (x);

     With this change, call to log (x) is effectively eliminated, as
     in the majority of the cases, log won't be called with x out of
     range.  The branch is totally predictable, so the branch cost
     is low.

     An example of (2) is:

	y = sqrt (x);
     ==>
	if (__builtin_isless (x, 0))
	  y =  sqrt (x);
	else
	  y = IFN_SQRT (x);
     In the vast majority of cases we should then never need to call sqrt.

   Note that library functions are not supposed to clear errno to zero without
   error.  See IEEE Std 1003.1, section 2.3 Error Numbers, and section 7.5:3 of
   ISO/IEC 9899 (C99).

   The condition wrapping the builtin call is conservatively set to avoid too
   aggressive (wrong) shrink wrapping.  */


/* A structure for representing input domain of
   a function argument in integer.  If the lower
   bound is -inf, has_lb is set to false.  If the
   upper bound is +inf, has_ub is false.
   is_lb_inclusive and is_ub_inclusive are flags
   to indicate if lb and ub value are inclusive
   respectively.  */

struct inp_domain
{
  int lb;
  int ub;
  bool has_lb;
  bool has_ub;
  bool is_lb_inclusive;
  bool is_ub_inclusive;
};

/* A helper function to construct and return an input
   domain object.  LB is the lower bound, HAS_LB is
   a boolean flag indicating if the lower bound exists,
   and LB_INCLUSIVE is a boolean flag indicating if the
   lower bound is inclusive or not.  UB, HAS_UB, and
   UB_INCLUSIVE have the same meaning, but for upper
   bound of the domain.  */

static inp_domain
get_domain (int lb, bool has_lb, bool lb_inclusive,
            int ub, bool has_ub, bool ub_inclusive)
{
  inp_domain domain;
  domain.lb = lb;
  domain.has_lb = has_lb;
  domain.is_lb_inclusive = lb_inclusive;
  domain.ub = ub;
  domain.has_ub = has_ub;
  domain.is_ub_inclusive = ub_inclusive;
  return domain;
}

/* A helper function to check the target format for the
   argument type. In this implementation, only IEEE formats
   are supported.  ARG is the call argument to be checked.
   Returns true if the format is supported.  To support other
   target formats,  function get_no_error_domain needs to be
   enhanced to have range bounds properly computed. Since
   the check is cheap (very small number of candidates
   to be checked), the result is not cached for each float type.  */

static bool
check_target_format (tree arg)
{
  tree type;
  machine_mode mode;
  const struct real_format *rfmt;

  type = TREE_TYPE (arg);
  mode = TYPE_MODE (type);
  rfmt = REAL_MODE_FORMAT (mode);
  if ((mode == SFmode
       && (rfmt == &ieee_single_format || rfmt == &mips_single_format
	   || rfmt == &motorola_single_format))
      || (mode == DFmode
	  && (rfmt == &ieee_double_format || rfmt == &mips_double_format
	      || rfmt == &motorola_double_format))
      /* For long double, we cannot really check XFmode
         which is only defined on intel platforms.
         Candidate pre-selection using builtin function
         code guarantees that we are checking formats
         for long double modes: double, quad, and extended.  */
      || (mode != SFmode && mode != DFmode
          && (rfmt == &ieee_quad_format
	      || rfmt == &mips_quad_format
	      || rfmt == &ieee_extended_motorola_format
              || rfmt == &ieee_extended_intel_96_format
              || rfmt == &ieee_extended_intel_128_format
              || rfmt == &ieee_extended_intel_96_round_53_format)))
    return true;

  return false;
}


/* A helper function to help select calls to pow that are suitable for
   conditional DCE transformation.  It looks for pow calls that can be
   guided with simple conditions.  Such calls either have constant base
   values or base values converted from integers.  Returns true if
   the pow call POW_CALL is a candidate.  */

/* The maximum integer bit size for base argument of a pow call
   that is suitable for shrink-wrapping transformation.  */
#define MAX_BASE_INT_BIT_SIZE 32

static bool
check_pow (gcall *pow_call)
{
  tree base, expn;
  enum tree_code bc, ec;

  if (gimple_call_num_args (pow_call) != 2)
    return false;

  base = gimple_call_arg (pow_call, 0);
  expn = gimple_call_arg (pow_call, 1);

  if (!check_target_format (expn))
    return false;

  bc = TREE_CODE (base);
  ec = TREE_CODE (expn);

  /* Folding candidates are not interesting.
     Can actually assert that it is already folded.  */
  if (ec == REAL_CST && bc == REAL_CST)
    return false;

  if (bc == REAL_CST)
    {
      /* Only handle a fixed range of constant.  */
      REAL_VALUE_TYPE mv;
      REAL_VALUE_TYPE bcv = TREE_REAL_CST (base);
      if (real_equal (&bcv, &dconst1))
        return false;
      if (real_less (&bcv, &dconst1))
        return false;
      real_from_integer (&mv, TYPE_MODE (TREE_TYPE (base)), 256, UNSIGNED);
      if (real_less (&mv, &bcv))
        return false;
      return true;
    }
  else if (bc == SSA_NAME)
    {
      tree base_val0, type;
      gimple *base_def;
      int bit_sz;

      /* Only handles cases where base value is converted
         from integer values.  */
      base_def = SSA_NAME_DEF_STMT (base);
      if (gimple_code (base_def) != GIMPLE_ASSIGN)
        return false;

      if (gimple_assign_rhs_code (base_def) != FLOAT_EXPR)
        return false;
      base_val0 = gimple_assign_rhs1 (base_def);

      type = TREE_TYPE (base_val0);
      if (TREE_CODE (type) != INTEGER_TYPE)
        return false;
      bit_sz = TYPE_PRECISION (type);
      /* If the type of the base is too wide,
         the resulting shrink wrapping condition
	 will be too conservative.  */
      if (bit_sz != 8 && bit_sz != 16 && bit_sz != MAX_BASE_INT_BIT_SIZE)
        return false;

      return true;
    }
  else
    return false;
}

/* A helper function to help select candidate function calls that are
   suitable for conditional DCE.  Candidate functions must have single
   valid input domain in this implementation except for pow (see check_pow).
   Returns true if the function call is a candidate.  */

static bool
check_builtin_call (gcall *bcall)
{
  tree arg;

  arg = gimple_call_arg (bcall, 0);
  return check_target_format (arg);
}

/* Return true if built-in function call CALL calls a math function
   and if we know how to test the range of its arguments to detect _most_
   situations in which errno is not set.  The test must err on the side
   of treating non-erroneous values as potentially erroneous.  */

static bool
can_test_argument_range (gcall *call)
{
  switch (DECL_FUNCTION_CODE (gimple_call_fndecl (call)))
    {
    /* Trig functions.  */
    CASE_FLT_FN (BUILT_IN_ACOS):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_ACOS):
    CASE_FLT_FN (BUILT_IN_ASIN):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_ASIN):
    /* Hyperbolic functions.  */
    CASE_FLT_FN (BUILT_IN_ACOSH):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_ACOSH):
    CASE_FLT_FN (BUILT_IN_ATANH):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_ATANH):
    CASE_FLT_FN (BUILT_IN_COSH):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_COSH):
    CASE_FLT_FN (BUILT_IN_SINH):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_SINH):
    /* Log functions.  */
    CASE_FLT_FN (BUILT_IN_LOG):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_LOG):
    CASE_FLT_FN (BUILT_IN_LOG2):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_LOG2):
    CASE_FLT_FN (BUILT_IN_LOG10):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_LOG10):
    CASE_FLT_FN (BUILT_IN_LOG1P):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_LOG1P):
    /* Exp functions.  */
    CASE_FLT_FN (BUILT_IN_EXP):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_EXP):
    CASE_FLT_FN (BUILT_IN_EXP2):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_EXP2):
    CASE_FLT_FN (BUILT_IN_EXP10):
    CASE_FLT_FN (BUILT_IN_EXPM1):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_EXPM1):
    CASE_FLT_FN (BUILT_IN_POW10):
    /* Sqrt.  */
    CASE_FLT_FN (BUILT_IN_SQRT):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_SQRT):
      return check_builtin_call (call);
    /* Special one: two argument pow.  */
    case BUILT_IN_POW:
      return check_pow (call);
    default:
      break;
    }

  return false;
}

/* Return true if CALL can produce a domain error (EDOM) but can never
   produce a pole, range overflow or range underflow error (all ERANGE).
   This means that we can tell whether a function would have set errno
   by testing whether the result is a NaN.  */

static bool
edom_only_function (gcall *call)
{
  switch (DECL_FUNCTION_CODE (gimple_call_fndecl (call)))
    {
    CASE_FLT_FN (BUILT_IN_ACOS):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_ACOS):
    CASE_FLT_FN (BUILT_IN_ASIN):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_ASIN):
    CASE_FLT_FN (BUILT_IN_ATAN):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_ATAN):
    CASE_FLT_FN (BUILT_IN_COS):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_COS):
    CASE_FLT_FN (BUILT_IN_SIGNIFICAND):
    CASE_FLT_FN (BUILT_IN_SIN):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_SIN):
    CASE_FLT_FN (BUILT_IN_SQRT):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_SQRT):
    CASE_FLT_FN (BUILT_IN_FMOD):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_FMOD):
    CASE_FLT_FN (BUILT_IN_REMAINDER):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_REMAINDER):
      return true;

    default:
      return false;
    }
}

/* Return true if it is structurally possible to guard CALL.  */

static bool
can_guard_call_p (gimple *call)
{
  return (!stmt_ends_bb_p (call)
	  || find_fallthru_edge (gimple_bb (call)->succs));
}

/* For a comparison code return the comparison code we should use if we don't
   HONOR_NANS.  */

static enum tree_code
comparison_code_if_no_nans (tree_code code)
{
  switch (code)
    {
    case UNLT_EXPR:
      return LT_EXPR;
    case UNGT_EXPR:
      return GT_EXPR;
    case UNLE_EXPR:
      return LE_EXPR;
    case UNGE_EXPR:
      return GE_EXPR;
    case UNEQ_EXPR:
      return EQ_EXPR;
    case LTGT_EXPR:
      return NE_EXPR;

    case LT_EXPR:
    case GT_EXPR:
    case LE_EXPR:
    case GE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
      return code;

    default:
      gcc_unreachable ();
    }
}

/* A helper function to generate gimple statements for one bound
   comparison, so that the built-in function is called whenever
   TCODE <ARG, LBUB> is *false*.  TEMP_NAME1/TEMP_NAME2 are names
   of the temporaries, CONDS is a vector holding the produced GIMPLE
   statements, and NCONDS points to the variable holding the number of
   logical comparisons.  CONDS is either empty or a list ended with a
   null tree.  */

static void
gen_one_condition (tree arg, int lbub,
                   enum tree_code tcode,
                   const char *temp_name1,
		   const char *temp_name2,
		   vec<gimple *> conds,
                   unsigned *nconds)
{
  if (!HONOR_NANS (arg))
    tcode = comparison_code_if_no_nans (tcode);

  tree lbub_real_cst, lbub_cst, float_type;
  tree temp, tempn, tempc, tempcn;
  gassign *stmt1;
  gassign *stmt2;
  gcond *stmt3;

  float_type = TREE_TYPE (arg);
  lbub_cst = build_int_cst (integer_type_node, lbub);
  lbub_real_cst = build_real_from_int_cst (float_type, lbub_cst);

  temp = create_tmp_var (float_type, temp_name1);
  stmt1 = gimple_build_assign (temp, arg);
  tempn = make_ssa_name (temp, stmt1);
  gimple_assign_set_lhs (stmt1, tempn);

  tempc = create_tmp_var (boolean_type_node, temp_name2);
  stmt2 = gimple_build_assign (tempc,
                               fold_build2 (tcode,
					    boolean_type_node,
					    tempn, lbub_real_cst));
  tempcn = make_ssa_name (tempc, stmt2);
  gimple_assign_set_lhs (stmt2, tempcn);

  stmt3 = gimple_build_cond_from_tree (tempcn, NULL_TREE, NULL_TREE);
  conds.quick_push (stmt1);
  conds.quick_push (stmt2);
  conds.quick_push (stmt3);
  (*nconds)++;
}

/* A helper function to generate GIMPLE statements for
   out of input domain check.  ARG is the call argument
   to be runtime checked, DOMAIN holds the valid domain
   for the given function, CONDS points to the vector
   holding the result GIMPLE statements.  *NCONDS is
   the number of logical comparisons.  This function
   produces no more than two logical comparisons, one
   for lower bound check, one for upper bound check.  */

static void
gen_conditions_for_domain (tree arg, inp_domain domain,
			   vec<gimple *> conds,
                           unsigned *nconds)
{
  if (domain.has_lb)
    gen_one_condition (arg, domain.lb,
                       (domain.is_lb_inclusive
                        ? UNGE_EXPR : UNGT_EXPR),
                       "DCE_COND_LB", "DCE_COND_LB_TEST",
                       conds, nconds);

  if (domain.has_ub)
    {
      /* Now push a separator.  */
      if (domain.has_lb)
        conds.quick_push (NULL);

      gen_one_condition (arg, domain.ub,
                         (domain.is_ub_inclusive
                          ? UNLE_EXPR : UNLT_EXPR),
                         "DCE_COND_UB", "DCE_COND_UB_TEST",
                         conds, nconds);
    }
}


/* A helper function to generate condition
   code for the y argument in call pow (some_const, y).
   See candidate selection in check_pow.  Since the
   candidates' base values have a limited range,
   the guarded code generated for y are simple:
   if (__builtin_isgreater (y, max_y))
     pow (const, y);
   Note max_y can be computed separately for each
   const base, but in this implementation, we
   choose to compute it using the max base
   in the allowed range for the purpose of
   simplicity.  BASE is the constant base value,
   EXPN is the expression for the exponent argument,
   *CONDS is the vector to hold resulting statements,
   and *NCONDS is the number of logical conditions.  */

static void
gen_conditions_for_pow_cst_base (tree base, tree expn,
				 vec<gimple *> conds,
                                 unsigned *nconds)
{
  inp_domain exp_domain;
  /* Validate the range of the base constant to make
     sure it is consistent with check_pow.  */
  REAL_VALUE_TYPE mv;
  REAL_VALUE_TYPE bcv = TREE_REAL_CST (base);
  gcc_assert (!real_equal (&bcv, &dconst1)
              && !real_less (&bcv, &dconst1));
  real_from_integer (&mv, TYPE_MODE (TREE_TYPE (base)), 256, UNSIGNED);
  gcc_assert (!real_less (&mv, &bcv));

  exp_domain = get_domain (0, false, false,
                           127, true, false);

  gen_conditions_for_domain (expn, exp_domain,
                             conds, nconds);
}

/* Generate error condition code for pow calls with
   non constant base values.  The candidates selected
   have their base argument value converted from
   integer (see check_pow) value (1, 2, 4 bytes), and
   the max exp value is computed based on the size
   of the integer type (i.e. max possible base value).
   The resulting input domain for exp argument is thus
   conservative (smaller than the max value allowed by
   the runtime value of the base).  BASE is the integer
   base value, EXPN is the expression for the exponent
   argument, *CONDS is the vector to hold resulting
   statements, and *NCONDS is the number of logical
   conditions.  */

static void
gen_conditions_for_pow_int_base (tree base, tree expn,
				 vec<gimple *> conds,
                                 unsigned *nconds)
{
  gimple *base_def;
  tree base_val0;
  tree int_type;
  tree temp, tempn;
  tree cst0;
  gimple *stmt1, *stmt2;
  int bit_sz, max_exp;
  inp_domain exp_domain;

  base_def = SSA_NAME_DEF_STMT (base);
  base_val0 = gimple_assign_rhs1 (base_def);
  int_type = TREE_TYPE (base_val0);
  bit_sz = TYPE_PRECISION (int_type);
  gcc_assert (bit_sz > 0
              && bit_sz <= MAX_BASE_INT_BIT_SIZE);

  /* Determine the max exp argument value according to
     the size of the base integer.  The max exp value
     is conservatively estimated assuming IEEE754 double
     precision format.  */
  if (bit_sz == 8)
    max_exp = 128;
  else if (bit_sz == 16)
    max_exp = 64;
  else
    {
      gcc_assert (bit_sz == MAX_BASE_INT_BIT_SIZE);
      max_exp = 32;
    }

  /* For pow ((double)x, y), generate the following conditions:
     cond 1:
     temp1 = x;
     if (__builtin_islessequal (temp1, 0))

     cond 2:
     temp2 = y;
     if (__builtin_isgreater (temp2, max_exp_real_cst))  */

  /* Generate condition in reverse order -- first
     the condition for the exp argument.  */

  exp_domain = get_domain (0, false, false,
                           max_exp, true, true);

  gen_conditions_for_domain (expn, exp_domain,
                             conds, nconds);

  /* Now generate condition for the base argument.
     Note it does not use the helper function
     gen_conditions_for_domain because the base
     type is integer.  */

  /* Push a separator.  */
  conds.quick_push (NULL);

  temp = create_tmp_var (int_type, "DCE_COND1");
  cst0 = build_int_cst (int_type, 0);
  stmt1 = gimple_build_assign (temp, base_val0);
  tempn = make_ssa_name (temp, stmt1);
  gimple_assign_set_lhs (stmt1, tempn);
  stmt2 = gimple_build_cond (GT_EXPR, tempn, cst0, NULL_TREE, NULL_TREE);

  conds.quick_push (stmt1);
  conds.quick_push (stmt2);
  (*nconds)++;
}

/* Method to generate conditional statements for guarding conditionally
   dead calls to pow.  One or more statements can be generated for
   each logical condition.  Statement groups of different conditions
   are separated by a NULL tree and they are stored in the vec
   conds.  The number of logical conditions are stored in *nconds.

   See C99 standard, 7.12.7.4:2, for description of pow (x, y).
   The precise condition for domain errors are complex.  In this
   implementation, a simplified (but conservative) valid domain
   for x and y are used: x is positive to avoid dom errors, while
   y is smaller than a upper bound (depending on x) to avoid range
   errors.  Runtime code is generated to check x (if not constant)
   and y against the valid domain.  If it is out, jump to the call,
   otherwise the call is bypassed.  POW_CALL is the call statement,
   *CONDS is a vector holding the resulting condition statements,
   and *NCONDS is the number of logical conditions.  */

static void
gen_conditions_for_pow (gcall *pow_call, vec<gimple *> conds,
                        unsigned *nconds)
{
  tree base, expn;
  enum tree_code bc;

  gcc_checking_assert (check_pow (pow_call));

  *nconds = 0;

  base = gimple_call_arg (pow_call, 0);
  expn = gimple_call_arg (pow_call, 1);

  bc = TREE_CODE (base);

  if (bc == REAL_CST)
    gen_conditions_for_pow_cst_base (base, expn, conds, nconds);
  else if (bc == SSA_NAME)
    gen_conditions_for_pow_int_base (base, expn, conds, nconds);
  else
    gcc_unreachable ();
}

/* A helper routine to help computing the valid input domain
   for a builtin function.  See C99 7.12.7 for details.  In this
   implementation, we only handle single region domain.  The
   resulting region can be conservative (smaller) than the actual
   one and rounded to integers.  Some of the bounds are documented
   in the standard, while other limit constants are computed
   assuming IEEE floating point format (for SF and DF modes).
   Since IEEE only sets minimum requirements for long double format,
   different long double formats exist under different implementations
   (e.g, 64 bit double precision (DF), 80 bit double-extended
   precision (XF), and 128 bit quad precision (TF) ).  For simplicity,
   in this implementation, the computed bounds for long double assume
   64 bit format (DF) except when it is IEEE quad or extended with the same
   emax, and are therefore sometimes conservative.  Another assumption is
   that single precision float type is always SF mode, and double type is DF
   mode.  This function is quite implementation specific, so it may not be
   suitable to be part of builtins.cc.  This needs to be revisited later
   to see if it can be leveraged in x87 assembly expansion.  */

static inp_domain
get_no_error_domain (enum built_in_function fnc)
{
  switch (fnc)
    {
    /* Trig functions: return [-1, +1]  */
    CASE_FLT_FN (BUILT_IN_ACOS):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_ACOS):
    CASE_FLT_FN (BUILT_IN_ASIN):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_ASIN):
      return get_domain (-1, true, true,
                         1, true, true);
    /* Hyperbolic functions.  */
    CASE_FLT_FN (BUILT_IN_ACOSH):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_ACOSH):
      /* acosh: [1, +inf)  */
      return get_domain (1, true, true,
                         1, false, false);
    CASE_FLT_FN (BUILT_IN_ATANH):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_ATANH):
      /* atanh: (-1, +1)  */
      return get_domain (-1, true, false,
                         1, true, false);
    case BUILT_IN_COSHF16:
    case BUILT_IN_SINHF16:
      /* coshf16: (-11, +11)  */
      return get_domain (-11, true, false,
			 11, true, false);
    case BUILT_IN_COSHF:
    case BUILT_IN_SINHF:
    case BUILT_IN_COSHF32:
    case BUILT_IN_SINHF32:
      /* coshf: (-89, +89)  */
      return get_domain (-89, true, false,
                         89, true, false);
    case BUILT_IN_COSH:
    case BUILT_IN_SINH:
    case BUILT_IN_COSHF64:
    case BUILT_IN_SINHF64:
    case BUILT_IN_COSHF32X:
    case BUILT_IN_SINHF32X:
      /* cosh: (-710, +710)  */
      return get_domain (-710, true, false,
                         710, true, false);
    case BUILT_IN_COSHF128:
    case BUILT_IN_SINHF128:
      /* coshf128: (-11357, +11357)  */
      return get_domain (-11357, true, false,
			 11357, true, false);
    case BUILT_IN_COSHL:
    case BUILT_IN_SINHL:
      if (REAL_MODE_FORMAT (TYPE_MODE (long_double_type_node))->emax == 16384)
	return get_no_error_domain (BUILT_IN_COSHF128);
      return get_no_error_domain (BUILT_IN_COSH);
    case BUILT_IN_COSHF64X:
    case BUILT_IN_SINHF64X:
      if (REAL_MODE_FORMAT (TYPE_MODE (float64x_type_node))->emax == 16384)
	return get_no_error_domain (BUILT_IN_COSHF128);
      return get_no_error_domain (BUILT_IN_COSH);
    /* Log functions: (0, +inf)  */
    CASE_FLT_FN (BUILT_IN_LOG):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_LOG):
    CASE_FLT_FN (BUILT_IN_LOG2):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_LOG2):
    CASE_FLT_FN (BUILT_IN_LOG10):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_LOG10):
      return get_domain (0, true, false,
                         0, false, false);
    CASE_FLT_FN (BUILT_IN_LOG1P):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_LOG1P):
      return get_domain (-1, true, false,
                         0, false, false);
    /* Exp functions.  */
    case BUILT_IN_EXPF16:
    case BUILT_IN_EXPM1F16:
      /* expf16: (-inf, 11)  */
      return get_domain (-1, false, false,
			 11, true, false);
    case BUILT_IN_EXPF:
    case BUILT_IN_EXPM1F:
    case BUILT_IN_EXPF32:
    case BUILT_IN_EXPM1F32:
      /* expf: (-inf, 88)  */
      return get_domain (-1, false, false,
                         88, true, false);
    case BUILT_IN_EXP:
    case BUILT_IN_EXPM1:
    case BUILT_IN_EXPF64:
    case BUILT_IN_EXPM1F64:
    case BUILT_IN_EXPF32X:
    case BUILT_IN_EXPM1F32X:
      /* exp: (-inf, 709)  */
      return get_domain (-1, false, false,
                         709, true, false);
    case BUILT_IN_EXPF128:
    case BUILT_IN_EXPM1F128:
      /* expf128: (-inf, 11356)  */
      return get_domain (-1, false, false,
			 11356, true, false);
    case BUILT_IN_EXPL:
    case BUILT_IN_EXPM1L:
      if (REAL_MODE_FORMAT (TYPE_MODE (long_double_type_node))->emax == 16384)
	return get_no_error_domain (BUILT_IN_EXPF128);
      return get_no_error_domain (BUILT_IN_EXP);
    case BUILT_IN_EXPF64X:
    case BUILT_IN_EXPM1F64X:
      if (REAL_MODE_FORMAT (TYPE_MODE (float64x_type_node))->emax == 16384)
	return get_no_error_domain (BUILT_IN_EXPF128);
      return get_no_error_domain (BUILT_IN_EXP);
    case BUILT_IN_EXP2F16:
      /* exp2f16: (-inf, 16)  */
      return get_domain (-1, false, false,
			 16, true, false);
    case BUILT_IN_EXP2F:
    case BUILT_IN_EXP2F32:
      /* exp2f: (-inf, 128)  */
      return get_domain (-1, false, false,
                         128, true, false);
    case BUILT_IN_EXP2:
    case BUILT_IN_EXP2F64:
    case BUILT_IN_EXP2F32X:
      /* exp2: (-inf, 1024)  */
      return get_domain (-1, false, false,
                         1024, true, false);
    case BUILT_IN_EXP2F128:
      /* exp2f128: (-inf, 16384)  */
      return get_domain (-1, false, false,
			 16384, true, false);
    case BUILT_IN_EXP2L:
      if (REAL_MODE_FORMAT (TYPE_MODE (long_double_type_node))->emax == 16384)
	return get_no_error_domain (BUILT_IN_EXP2F128);
      return get_no_error_domain (BUILT_IN_EXP2);
    case BUILT_IN_EXP2F64X:
      if (REAL_MODE_FORMAT (TYPE_MODE (float64x_type_node))->emax == 16384)
	return get_no_error_domain (BUILT_IN_EXP2F128);
      return get_no_error_domain (BUILT_IN_EXP2);
    case BUILT_IN_EXP10F:
    case BUILT_IN_POW10F:
      /* exp10f: (-inf, 38)  */
      return get_domain (-1, false, false,
                         38, true, false);
    case BUILT_IN_EXP10:
    case BUILT_IN_POW10:
      /* exp10: (-inf, 308)  */
      return get_domain (-1, false, false,
                         308, true, false);
    case BUILT_IN_EXP10L:
    case BUILT_IN_POW10L:
      if (REAL_MODE_FORMAT (TYPE_MODE (long_double_type_node))->emax == 16384)
	/* exp10l: (-inf, 4932)  */
	return get_domain (-1, false, false,
			   4932, true, false);
      return get_no_error_domain (BUILT_IN_EXP10);
    /* sqrt: [0, +inf)  */
    CASE_FLT_FN (BUILT_IN_SQRT):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_SQRT):
      return get_domain (0, true, true,
                         0, false, false);
    default:
      gcc_unreachable ();
    }

  gcc_unreachable ();
}

/* The function to generate shrink wrap conditions for a partially
   dead builtin call whose return value is not used anywhere,
   but has to be kept live due to potential error condition.
   BI_CALL is the builtin call, CONDS is the vector of statements
   for condition code, NCODES is the pointer to the number of
   logical conditions.  Statements belonging to different logical
   condition are separated by NULL tree in the vector.  */

static void
gen_shrink_wrap_conditions (gcall *bi_call, const vec<gimple *> &conds,
                            unsigned int *nconds)
{
  gcall *call;
  tree fn;
  enum built_in_function fnc;

  gcc_assert (nconds && conds.exists ());
  gcc_assert (conds.length () == 0);
  gcc_assert (is_gimple_call (bi_call));

  call = bi_call;
  fn = gimple_call_fndecl (call);
  gcc_assert (fn && fndecl_built_in_p (fn));
  fnc = DECL_FUNCTION_CODE (fn);
  *nconds = 0;

  if (fnc == BUILT_IN_POW)
    gen_conditions_for_pow (call, conds, nconds);
  else
    {
      tree arg;
      inp_domain domain = get_no_error_domain (fnc);
      *nconds = 0;
      arg = gimple_call_arg (bi_call, 0);
      gen_conditions_for_domain (arg, domain, conds, nconds);
    }

  return;
}

/* Shrink-wrap BI_CALL so that it is only called when one of the NCONDS
   conditions in CONDS is false.  Also move BI_NEWCALL to a new basic block
   when it is non-null, it is called while all of the CONDS are true.  */

static void
shrink_wrap_one_built_in_call_with_conds (gcall *bi_call,
					  const vec <gimple *> &conds,
					  unsigned int nconds,
					  gcall *bi_newcall = NULL)
{
  gimple_stmt_iterator bi_call_bsi;
  basic_block bi_call_bb, bi_newcall_bb, join_tgt_bb, guard_bb;
  edge join_tgt_in_edge_from_call, join_tgt_in_edge_fall_thru;
  edge bi_call_in_edge0, guard_bb_in_edge;
  unsigned tn_cond_stmts;
  unsigned ci;
  gimple *cond_expr = NULL;
  gimple *cond_expr_start;

  /* The cfg we want to create looks like this:
          [guard n-1]         <- guard_bb (old block)
            |    \
            | [guard n-2]                   }
            |    / \                        }
            |   /  ...                      } new blocks
            |  /  [guard 0]                 }
            | /  /    |                     }
           [call]     |      <- bi_call_bb  }
             \    [newcall]  <-bi_newcall_bb}
              \       |
                [join]       <- join_tgt_bb (old iff call must end bb)
	 possible EH edges (only if [join] is old)

     When [join] is new, the immediate dominators for these blocks are:

     1. [guard n-1]: unchanged
     2. [call]: [guard n-1]
     3. [newcall]: [guard 0]
     4. [guard m]: [guard m+1] for 0 <= m <= n-2
     5. [join]: [guard n-1]

     We punt for the more complex case of [join] being old and
     simply free the dominance info.  We also punt on postdominators,
     which aren't expected to be available at this point anyway.  */
  bi_call_bb = gimple_bb (bi_call);

  /* Now find the join target bb -- split bi_call_bb if needed.  */
  if (stmt_ends_bb_p (bi_call))
    {
      /* We checked that there was a fallthrough edge in
	 can_guard_call_p.  */
      join_tgt_in_edge_from_call = find_fallthru_edge (bi_call_bb->succs);
      gcc_assert (join_tgt_in_edge_from_call);
      /* We don't want to handle PHIs.  */
      if (EDGE_COUNT (join_tgt_in_edge_from_call->dest->preds) > 1)
	join_tgt_bb = split_edge (join_tgt_in_edge_from_call);
      else
	{
	  join_tgt_bb = join_tgt_in_edge_from_call->dest;
	  /* We may have degenerate PHIs in the destination.  Propagate
	     those out.  */
	  for (gphi_iterator i = gsi_start_phis (join_tgt_bb); !gsi_end_p (i);)
	    {
	      gphi *phi = i.phi ();
	      replace_uses_by (gimple_phi_result (phi),
			       gimple_phi_arg_def (phi, 0));
	      remove_phi_node (&i, true);
	    }
	}
    }
  else
    {
      join_tgt_in_edge_from_call = split_block (bi_call_bb, bi_call);
      join_tgt_bb = join_tgt_in_edge_from_call->dest;
    }

  bi_call_bsi = gsi_for_stmt (bi_call);

  /* Now it is time to insert the first conditional expression
     into bi_call_bb and split this bb so that bi_call is
     shrink-wrapped.  */
  tn_cond_stmts = conds.length ();
  cond_expr = NULL;
  cond_expr_start = conds[0];
  for (ci = 0; ci < tn_cond_stmts; ci++)
    {
      gimple *c = conds[ci];
      gcc_assert (c || ci != 0);
      if (!c)
        break;
      gsi_insert_before (&bi_call_bsi, c, GSI_SAME_STMT);
      cond_expr = c;
    }
  ci++;
  gcc_assert (cond_expr && gimple_code (cond_expr) == GIMPLE_COND);

  typedef std::pair<edge, edge> edge_pair;
  auto_vec<edge_pair, 8> edges;

  bi_call_in_edge0 = split_block (bi_call_bb, cond_expr);
  bi_call_in_edge0->flags &= ~EDGE_FALLTHRU;
  bi_call_in_edge0->flags |= EDGE_FALSE_VALUE;
  guard_bb = bi_call_bb;
  bi_call_bb = bi_call_in_edge0->dest;
  join_tgt_in_edge_fall_thru = make_edge (guard_bb, join_tgt_bb,
                                          EDGE_TRUE_VALUE);

  edges.reserve (nconds);
  edges.quick_push (edge_pair (bi_call_in_edge0, join_tgt_in_edge_fall_thru));

  /* Code generation for the rest of the conditions  */
  for (unsigned int i = 1; i < nconds; ++i)
    {
      unsigned ci0;
      edge bi_call_in_edge;
      gimple_stmt_iterator guard_bsi = gsi_for_stmt (cond_expr_start);
      ci0 = ci;
      cond_expr_start = conds[ci0];
      for (; ci < tn_cond_stmts; ci++)
        {
	  gimple *c = conds[ci];
          gcc_assert (c || ci != ci0);
          if (!c)
            break;
          gsi_insert_before (&guard_bsi, c, GSI_SAME_STMT);
          cond_expr = c;
        }
      ci++;
      gcc_assert (cond_expr && gimple_code (cond_expr) == GIMPLE_COND);
      guard_bb_in_edge = split_block (guard_bb, cond_expr);
      guard_bb_in_edge->flags &= ~EDGE_FALLTHRU;
      guard_bb_in_edge->flags |= EDGE_TRUE_VALUE;

      bi_call_in_edge = make_edge (guard_bb, bi_call_bb, EDGE_FALSE_VALUE);
      edges.quick_push (edge_pair (bi_call_in_edge, guard_bb_in_edge));
    }

  /* Move BI_NEWCALL to new basic block when it is non-null.  */
  if (bi_newcall)
    {
      /* Get bi_newcall_bb by split join_tgt_in_edge_fall_thru edge,
         and move BI_NEWCALL to bi_newcall_bb.  */
      bi_newcall_bb = split_edge (join_tgt_in_edge_fall_thru);
      gimple_stmt_iterator to_gsi = gsi_start_bb (bi_newcall_bb);
      gimple_stmt_iterator from_gsi = gsi_for_stmt (bi_newcall);
      gsi_move_before (&from_gsi, &to_gsi);
      join_tgt_in_edge_fall_thru = EDGE_SUCC (bi_newcall_bb, 0);
      join_tgt_bb = join_tgt_in_edge_fall_thru->dest;

      tree bi_newcall_lhs = gimple_call_lhs (bi_newcall);
      tree bi_call_lhs = gimple_call_lhs (bi_call);
      if (!bi_call_lhs)
        {
          bi_call_lhs = copy_ssa_name (bi_newcall_lhs);
          gimple_call_set_lhs (bi_call, bi_call_lhs);
          SSA_NAME_DEF_STMT (bi_call_lhs) = bi_call;
        }

      /* Create phi node for lhs of BI_CALL and BI_NEWCALL.  */
      gphi *new_phi = create_phi_node (copy_ssa_name (bi_newcall_lhs),
				       join_tgt_bb);
      SSA_NAME_OCCURS_IN_ABNORMAL_PHI (PHI_RESULT (new_phi))
        = SSA_NAME_OCCURS_IN_ABNORMAL_PHI (bi_newcall_lhs);
      add_phi_arg (new_phi, bi_call_lhs, join_tgt_in_edge_from_call,
                   gimple_location (bi_call));
      add_phi_arg (new_phi, bi_newcall_lhs, join_tgt_in_edge_fall_thru,
                   gimple_location (bi_newcall));

      /* Replace all use of original return value with result of phi node.  */
      use_operand_p use_p;
      gimple *use_stmt;
      imm_use_iterator iterator;
      FOR_EACH_IMM_USE_STMT (use_stmt, iterator, bi_newcall_lhs)
        if (use_stmt != new_phi)
	  FOR_EACH_IMM_USE_ON_STMT (use_p, iterator)
	    SET_USE (use_p, PHI_RESULT (new_phi));
    }

  /* Now update the probability and profile information, processing the
     guards in order of execution.

     There are two approaches we could take here.  On the one hand we
     could assign a probability of X to the call block and distribute
     that probability among its incoming edges.  On the other hand we
     could assign a probability of X to each individual call edge.

     The choice only affects calls that have more than one condition.
     In those cases, the second approach would give the call block
     a greater probability than the first.  However, the difference
     is only small, and our chosen X is a pure guess anyway.

     Here we take the second approach because it's slightly simpler
     and because it's easy to see that it doesn't lose profile counts.  */
  bi_call_bb->count = profile_count::zero ();
  while (!edges.is_empty ())
    {
      edge_pair e = edges.pop ();
      edge call_edge = e.first;
      edge nocall_edge = e.second;
      basic_block src_bb = call_edge->src;
      gcc_assert (src_bb == nocall_edge->src);

      call_edge->probability = profile_probability::very_unlikely ();
      nocall_edge->probability = profile_probability::always ()
				 - call_edge->probability;

      bi_call_bb->count += call_edge->count ();

      if (nocall_edge->dest != join_tgt_bb)
	nocall_edge->dest->count = src_bb->count - bi_call_bb->count;
    }

  if (dom_info_available_p (CDI_DOMINATORS))
    {
      /* The split_blocks leave [guard 0] as the immediate dominator
	 of [call] and [call] as the immediate dominator of [join].
	 Fix them up.  */
      set_immediate_dominator (CDI_DOMINATORS, bi_call_bb, guard_bb);
      set_immediate_dominator (CDI_DOMINATORS, join_tgt_bb, guard_bb);
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      location_t loc;
      loc = gimple_location (bi_call);
      fprintf (dump_file,
               "%s:%d: note: function call is shrink-wrapped"
               " into error conditions.\n",
               LOCATION_FILE (loc), LOCATION_LINE (loc));
    }
}

/* Shrink-wrap BI_CALL so that it is only called when it might set errno
   (but is always called if it would set errno).  */

static void
shrink_wrap_one_built_in_call (gcall *bi_call)
{
  unsigned nconds = 0;
  auto_vec<gimple *, 12> conds;
  gen_shrink_wrap_conditions (bi_call, conds, &nconds);
  gcc_assert (nconds != 0);
  shrink_wrap_one_built_in_call_with_conds (bi_call, conds, nconds);
}

/* Return true if built-in function call CALL could be implemented using
   a combination of an internal function to compute the result and a
   separate call to set errno.  */

static bool
can_use_internal_fn (gcall *call)
{
  /* Only replace calls that set errno.  */
  if (!gimple_vdef (call))
    return false;

  /* See whether there is an internal function for this built-in.  */
  if (replacement_internal_fn (call) == IFN_LAST)
    return false;

  /* See whether we can catch all cases where errno would be set,
     while still avoiding the call in most cases.  */
  if (!can_test_argument_range (call)
      && !edom_only_function (call))
    return false;

  return true;
}

/* Implement built-in function call CALL using an internal function.  */

static void
use_internal_fn (gcall *call)
{
  /* We'll be inserting another call with the same arguments after the
     lhs has been set, so prevent any possible coalescing failure from
     having both values live at once.  See PR 71020.  */
  replace_abnormal_ssa_names (call);

  unsigned nconds = 0;
  auto_vec<gimple *, 12> conds;
  bool is_arg_conds = false;
  if (can_test_argument_range (call))
    {
      gen_shrink_wrap_conditions (call, conds, &nconds);
      is_arg_conds = true;
      gcc_assert (nconds != 0);
    }
  else
    gcc_assert (edom_only_function (call));

  internal_fn ifn = replacement_internal_fn (call);
  gcc_assert (ifn != IFN_LAST);

  /* Construct the new call, with the same arguments as the original one.  */
  auto_vec <tree, 16> args;
  unsigned int nargs = gimple_call_num_args (call);
  for (unsigned int i = 0; i < nargs; ++i)
    args.safe_push (gimple_call_arg (call, i));
  gcall *new_call = gimple_build_call_internal_vec (ifn, args);
  gimple_set_location (new_call, gimple_location (call));
  gimple_call_set_nothrow (new_call, gimple_call_nothrow_p (call));

  /* Transfer the LHS to the new call.  */
  tree lhs = gimple_call_lhs (call);
  gimple_call_set_lhs (new_call, lhs);
  gimple_call_set_lhs (call, NULL_TREE);
  SSA_NAME_DEF_STMT (lhs) = new_call;

  /* Insert the new call.  */
  gimple_stmt_iterator gsi = gsi_for_stmt (call);
  gsi_insert_before (&gsi, new_call, GSI_SAME_STMT);

  if (nconds == 0)
    {
      /* Skip the call if LHS == LHS.  If we reach here, EDOM is the only
	 valid errno value and it is used iff the result is NaN.  */
      conds.quick_push (gimple_build_cond (EQ_EXPR, lhs, lhs,
					   NULL_TREE, NULL_TREE));
      nconds++;

      /* Try replacing the original call with a direct assignment to
	 errno, via an internal function.  */
      if (set_edom_supported_p () && !stmt_ends_bb_p (call))
	{
	  gimple_stmt_iterator gsi = gsi_for_stmt (call);
	  gcall *new_call = gimple_build_call_internal (IFN_SET_EDOM, 0);
	  gimple_move_vops (new_call, call);
	  gimple_set_location (new_call, gimple_location (call));
	  gsi_replace (&gsi, new_call, false);
	  call = new_call;
	}
    }
  shrink_wrap_one_built_in_call_with_conds (call, conds, nconds,
					    is_arg_conds ? new_call : NULL);
}

/* The top level function for conditional dead code shrink
   wrapping transformation.  */

static void
shrink_wrap_conditional_dead_built_in_calls (const vec<gcall *> &calls)
{
  unsigned i = 0;

  unsigned n = calls.length ();
  for (; i < n ; i++)
    {
      gcall *bi_call = calls[i];
      if (gimple_call_lhs (bi_call))
	use_internal_fn (bi_call);
      else
	shrink_wrap_one_built_in_call (bi_call);
    }
}

namespace {

const pass_data pass_data_call_cdce =
{
  GIMPLE_PASS, /* type */
  "cdce", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_CALL_CDCE, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_call_cdce : public gimple_opt_pass
{
public:
  pass_call_cdce (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_call_cdce, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) final override
    {
      /* The limit constants used in the implementation
	 assume IEEE floating point format.  Other formats
	 can be supported in the future if needed.  */
      return flag_tree_builtin_call_dce != 0;
    }

  unsigned int execute (function *) final override;

}; // class pass_call_cdce

unsigned int
pass_call_cdce::execute (function *fun)
{
  basic_block bb;
  gimple_stmt_iterator i;
  auto_vec<gcall *> cond_dead_built_in_calls;
  FOR_EACH_BB_FN (bb, fun)
    {
      /* Skip blocks that are being optimized for size, since our
	 transformation always increases code size.  */
      if (optimize_bb_for_size_p (bb))
	continue;

      /* Collect dead call candidates.  */
      for (i = gsi_start_bb (bb); !gsi_end_p (i); gsi_next (&i))
        {
	  gcall *stmt = dyn_cast <gcall *> (gsi_stmt (i));
          if (stmt
	      && gimple_call_builtin_p (stmt, BUILT_IN_NORMAL)
	      && (gimple_call_lhs (stmt)
		  ? can_use_internal_fn (stmt)
		  : can_test_argument_range (stmt))
	      && can_guard_call_p (stmt))
            {
              if (dump_file && (dump_flags & TDF_DETAILS))
                {
                  fprintf (dump_file, "Found conditional dead call: ");
                  print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
                  fprintf (dump_file, "\n");
                }
	      if (!cond_dead_built_in_calls.exists ())
		cond_dead_built_in_calls.create (64);
	      cond_dead_built_in_calls.safe_push (stmt);
            }
	}
    }

  if (!cond_dead_built_in_calls.exists ())
    return 0;

  shrink_wrap_conditional_dead_built_in_calls (cond_dead_built_in_calls);
  free_dominance_info (CDI_POST_DOMINATORS);
  /* As we introduced new control-flow we need to insert PHI-nodes
     for the call-clobbers of the remaining call.  */
  mark_virtual_operands_for_renaming (fun);
  return TODO_update_ssa;
}

} // anon namespace

gimple_opt_pass *
make_pass_call_cdce (gcc::context *ctxt)
{
  return new pass_call_cdce (ctxt);
}
