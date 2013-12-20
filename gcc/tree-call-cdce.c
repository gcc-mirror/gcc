/* Conditional Dead Call Elimination pass for the GNU compiler.
   Copyright (C) 2008-2013 Free Software Foundation, Inc.
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
#include "tm.h"
#include "basic-block.h"
#include "tree.h"
#include "stor-layout.h"
#include "gimple-pretty-print.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimple-ssa.h"
#include "tree-cfg.h"
#include "stringpool.h"
#include "tree-ssanames.h"
#include "tree-into-ssa.h"
#include "tree-pass.h"
#include "flags.h"


/* Conditional dead call elimination

   Some builtin functions can set errno on error conditions, but they
   are otherwise pure.  If the result of a call to such a function is
   not used, the compiler can still not eliminate the call without
   powerful interprocedural analysis to prove that the errno is not
   checked.  However, if the conditions under which the error occurs
   are known, the compiler can conditionally dead code eliminate the
   calls by shrink-wrapping the semi-dead calls into the error condition:

        built_in_call (args)
          ==>
        if (error_cond (args))
             built_in_call (args)

    An actual simple example is :
         log (x);   // Mostly dead call
     ==>
         if (x < 0)
             log (x);
     With this change, call to log (x) is effectively eliminated, as
     in majority of the cases, log won't be called with x out of
     range.  The branch is totally predictable, so the branch cost
     is low.

   Note that library functions are not supposed to clear errno to zero without
   error.  See IEEE Std 1003.1, section 2.3 Error Numbers, and section 7.5:3 of
   ISO/IEC 9899 (C99).

   The condition wrapping the builtin call is conservatively set to avoid too
   aggressive (wrong) shrink wrapping.  The optimization is called conditional
   dead call elimination because the call is eliminated under the condition
   that the input arguments would not lead to domain or range error (for
   instance when x <= 0 for a log (x) call), however the chances that the error
   condition is hit is very low (those builtin calls which are conditionally
   dead are usually part of the C++ abstraction penalty exposed after
   inlining).  */


/* A structure for representing input domain of
   a function argument in integer.  If the lower
   bound is -inf, has_lb is set to false.  If the
   upper bound is +inf, has_ub is false.
   is_lb_inclusive and is_ub_inclusive are flags
   to indicate if lb and ub value are inclusive
   respectively.  */

typedef struct input_domain
{
  int lb;
  int ub;
  bool has_lb;
  bool has_ub;
  bool is_lb_inclusive;
  bool is_ub_inclusive;
} inp_domain;

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
  enum machine_mode mode;
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
      /* For long double, we can not really check XFmode
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
check_pow (gimple pow_call)
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
      if (REAL_VALUES_EQUAL (bcv, dconst1))
        return false;
      if (REAL_VALUES_LESS (bcv, dconst1))
        return false;
      real_from_integer (&mv, TYPE_MODE (TREE_TYPE (base)), 256, 0, 1);
      if (REAL_VALUES_LESS (mv, bcv))
        return false;
      return true;
    }
  else if (bc == SSA_NAME)
    {
      tree base_val0, type;
      gimple base_def;
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
      if (bit_sz > MAX_BASE_INT_BIT_SIZE)
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
check_builtin_call (gimple bcall)
{
  tree arg;

  arg = gimple_call_arg (bcall, 0);
  return check_target_format (arg);
}

/* A helper function to determine if a builtin function call is a
   candidate for conditional DCE.  Returns true if the builtin call
   is a candidate.  */

static bool
is_call_dce_candidate (gimple call)
{
  tree fn;
  enum built_in_function fnc;

  /* Only potentially dead calls are considered.  */
  if (gimple_call_lhs (call))
    return false;

  fn = gimple_call_fndecl (call);
  if (!fn
      || !DECL_BUILT_IN (fn)
      || (DECL_BUILT_IN_CLASS (fn) != BUILT_IN_NORMAL))
    return false;

  fnc = DECL_FUNCTION_CODE (fn);
  switch (fnc)
    {
    /* Trig functions.  */
    CASE_FLT_FN (BUILT_IN_ACOS):
    CASE_FLT_FN (BUILT_IN_ASIN):
    /* Hyperbolic functions.  */
    CASE_FLT_FN (BUILT_IN_ACOSH):
    CASE_FLT_FN (BUILT_IN_ATANH):
    CASE_FLT_FN (BUILT_IN_COSH):
    CASE_FLT_FN (BUILT_IN_SINH):
    /* Log functions.  */
    CASE_FLT_FN (BUILT_IN_LOG):
    CASE_FLT_FN (BUILT_IN_LOG2):
    CASE_FLT_FN (BUILT_IN_LOG10):
    CASE_FLT_FN (BUILT_IN_LOG1P):
    /* Exp functions.  */
    CASE_FLT_FN (BUILT_IN_EXP):
    CASE_FLT_FN (BUILT_IN_EXP2):
    CASE_FLT_FN (BUILT_IN_EXP10):
    CASE_FLT_FN (BUILT_IN_EXPM1):
    CASE_FLT_FN (BUILT_IN_POW10):
    /* Sqrt.  */
    CASE_FLT_FN (BUILT_IN_SQRT):
      return check_builtin_call (call);
    /* Special one: two argument pow.  */
    case BUILT_IN_POW:
      return check_pow (call);
    default:
      break;
    }

  return false;
}


/* A helper function to generate gimple statements for
   one bound comparison.  ARG is the call argument to
   be compared with the bound, LBUB is the bound value
   in integer, TCODE is the tree_code of the comparison,
   TEMP_NAME1/TEMP_NAME2 are names of the temporaries,
   CONDS is a vector holding the produced GIMPLE statements,
   and NCONDS points to the variable holding the number
   of logical comparisons.  CONDS is either empty or
   a list ended with a null tree.  */

static void
gen_one_condition (tree arg, int lbub,
                   enum tree_code tcode,
                   const char *temp_name1,
		   const char *temp_name2,
                   vec<gimple> conds,
                   unsigned *nconds)
{
  tree lbub_real_cst, lbub_cst, float_type;
  tree temp, tempn, tempc, tempcn;
  gimple stmt1, stmt2, stmt3;

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
                           vec<gimple> conds,
                           unsigned *nconds)
{
  if (domain.has_lb)
    gen_one_condition (arg, domain.lb,
                       (domain.is_lb_inclusive
                        ? LT_EXPR : LE_EXPR),
                       "DCE_COND_LB", "DCE_COND_LB_TEST",
                       conds, nconds);

  if (domain.has_ub)
    {
      /* Now push a separator.  */
      if (domain.has_lb)
        conds.quick_push (NULL);

      gen_one_condition (arg, domain.ub,
                         (domain.is_ub_inclusive
                          ? GT_EXPR : GE_EXPR),
                         "DCE_COND_UB", "DCE_COND_UB_TEST",
                         conds, nconds);
    }
}


/* A helper function to generate condition
   code for the y argument in call pow (some_const, y).
   See candidate selection in check_pow.  Since the
   candidates' base values have a limited range,
   the guarded code generated for y are simple:
   if (y > max_y)
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
                                 vec<gimple> conds,
                                 unsigned *nconds)
{
  inp_domain exp_domain;
  /* Validate the range of the base constant to make
     sure it is consistent with check_pow.  */
  REAL_VALUE_TYPE mv;
  REAL_VALUE_TYPE bcv = TREE_REAL_CST (base);
  gcc_assert (!REAL_VALUES_EQUAL (bcv, dconst1)
              && !REAL_VALUES_LESS (bcv, dconst1));
  real_from_integer (&mv, TYPE_MODE (TREE_TYPE (base)), 256, 0, 1);
  gcc_assert (!REAL_VALUES_LESS (mv, bcv));

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
                                 vec<gimple> conds,
                                 unsigned *nconds)
{
  gimple base_def;
  tree base_val0;
  tree int_type;
  tree temp, tempn;
  tree cst0;
  gimple stmt1, stmt2;
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
     if (temp1 <= 0)

     cond 2:
     temp2 = y;
     if (temp2 > max_exp_real_cst)  */

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
  stmt2 = gimple_build_cond (LE_EXPR, tempn, cst0, NULL_TREE, NULL_TREE);

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
gen_conditions_for_pow (gimple pow_call, vec<gimple> conds,
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
   precision (XF), and 128 bit quad precision (QF) ).  For simplicity,
   in this implementation, the computed bounds for long double assume
   64 bit format (DF), and are therefore conservative.  Another
   assumption is that single precision float type is always SF mode,
   and double type is DF mode.  This function is quite
   implementation specific, so it may not be suitable to be part of
   builtins.c.  This needs to be revisited later to see if it can
   be leveraged in x87 assembly expansion.  */

static inp_domain
get_no_error_domain (enum built_in_function fnc)
{
  switch (fnc)
    {
    /* Trig functions: return [-1, +1]  */
    CASE_FLT_FN (BUILT_IN_ACOS):
    CASE_FLT_FN (BUILT_IN_ASIN):
      return get_domain (-1, true, true,
                         1, true, true);
    /* Hyperbolic functions.  */
    CASE_FLT_FN (BUILT_IN_ACOSH):
      /* acosh: [1, +inf)  */
      return get_domain (1, true, true,
                         1, false, false);
    CASE_FLT_FN (BUILT_IN_ATANH):
      /* atanh: (-1, +1)  */
      return get_domain (-1, true, false,
                         1, true, false);
    case BUILT_IN_COSHF:
    case BUILT_IN_SINHF:
      /* coshf: (-89, +89)  */
      return get_domain (-89, true, false,
                         89, true, false);
    case BUILT_IN_COSH:
    case BUILT_IN_SINH:
    case BUILT_IN_COSHL:
    case BUILT_IN_SINHL:
      /* cosh: (-710, +710)  */
      return get_domain (-710, true, false,
                         710, true, false);
    /* Log functions: (0, +inf)  */
    CASE_FLT_FN (BUILT_IN_LOG):
    CASE_FLT_FN (BUILT_IN_LOG2):
    CASE_FLT_FN (BUILT_IN_LOG10):
      return get_domain (0, true, false,
                         0, false, false);
    CASE_FLT_FN (BUILT_IN_LOG1P):
      return get_domain (-1, true, false,
                         0, false, false);
    /* Exp functions.  */
    case BUILT_IN_EXPF:
    case BUILT_IN_EXPM1F:
      /* expf: (-inf, 88)  */
      return get_domain (-1, false, false,
                         88, true, false);
    case BUILT_IN_EXP:
    case BUILT_IN_EXPM1:
    case BUILT_IN_EXPL:
    case BUILT_IN_EXPM1L:
      /* exp: (-inf, 709)  */
      return get_domain (-1, false, false,
                         709, true, false);
    case BUILT_IN_EXP2F:
      /* exp2f: (-inf, 128)  */
      return get_domain (-1, false, false,
                         128, true, false);
    case BUILT_IN_EXP2:
    case BUILT_IN_EXP2L:
      /* exp2: (-inf, 1024)  */
      return get_domain (-1, false, false,
                         1024, true, false);
    case BUILT_IN_EXP10F:
    case BUILT_IN_POW10F:
      /* exp10f: (-inf, 38)  */
      return get_domain (-1, false, false,
                         38, true, false);
    case BUILT_IN_EXP10:
    case BUILT_IN_POW10:
    case BUILT_IN_EXP10L:
    case BUILT_IN_POW10L:
      /* exp10: (-inf, 308)  */
      return get_domain (-1, false, false,
                         308, true, false);
    /* sqrt: [0, +inf)  */
    CASE_FLT_FN (BUILT_IN_SQRT):
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
gen_shrink_wrap_conditions (gimple bi_call, vec<gimple> conds,
                            unsigned int *nconds)
{
  gimple call;
  tree fn;
  enum built_in_function fnc;

  gcc_assert (nconds && conds.exists ());
  gcc_assert (conds.length () == 0);
  gcc_assert (is_gimple_call (bi_call));

  call = bi_call;
  fn = gimple_call_fndecl (call);
  gcc_assert (fn && DECL_BUILT_IN (fn));
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


/* Probability of the branch (to the call) is taken.  */
#define ERR_PROB 0.01

/* The function to shrink wrap a partially dead builtin call
   whose return value is not used anywhere, but has to be kept
   live due to potential error condition.  Returns true if the
   transformation actually happens.  */

static bool
shrink_wrap_one_built_in_call (gimple bi_call)
{
  gimple_stmt_iterator bi_call_bsi;
  basic_block bi_call_bb, join_tgt_bb, guard_bb, guard_bb0;
  edge join_tgt_in_edge_from_call, join_tgt_in_edge_fall_thru;
  edge bi_call_in_edge0, guard_bb_in_edge;
  unsigned tn_cond_stmts, nconds;
  unsigned ci;
  gimple cond_expr = NULL;
  gimple cond_expr_start;
  tree bi_call_label_decl;
  gimple bi_call_label;

  auto_vec<gimple, 12> conds;
  gen_shrink_wrap_conditions (bi_call, conds, &nconds);

  /* This can happen if the condition generator decides
     it is not beneficial to do the transformation.  Just
     return false and do not do any transformation for
     the call.  */
  if (nconds == 0)
    return false;

  bi_call_bb = gimple_bb (bi_call);

  /* Now find the join target bb -- split bi_call_bb if needed.  */
  if (stmt_ends_bb_p (bi_call))
    {
      /* If the call must be the last in the bb, don't split the block,
	 it could e.g. have EH edges.  */
      join_tgt_in_edge_from_call = find_fallthru_edge (bi_call_bb->succs);
      if (join_tgt_in_edge_from_call == NULL)
        return false;
    }
  else
    join_tgt_in_edge_from_call = split_block (bi_call_bb, bi_call);

  bi_call_bsi = gsi_for_stmt (bi_call);

  join_tgt_bb = join_tgt_in_edge_from_call->dest;

  /* Now it is time to insert the first conditional expression
     into bi_call_bb and split this bb so that bi_call is
     shrink-wrapped.  */
  tn_cond_stmts = conds.length ();
  cond_expr = NULL;
  cond_expr_start = conds[0];
  for (ci = 0; ci < tn_cond_stmts; ci++)
    {
      gimple c = conds[ci];
      gcc_assert (c || ci != 0);
      if (!c)
        break;
      gsi_insert_before (&bi_call_bsi, c, GSI_SAME_STMT);
      cond_expr = c;
    }
  nconds--;
  ci++;
  gcc_assert (cond_expr && gimple_code (cond_expr) == GIMPLE_COND);

  /* Now the label.  */
  bi_call_label_decl = create_artificial_label (gimple_location (bi_call));
  bi_call_label = gimple_build_label (bi_call_label_decl);
  gsi_insert_before (&bi_call_bsi, bi_call_label, GSI_SAME_STMT);

  bi_call_in_edge0 = split_block (bi_call_bb, cond_expr);
  bi_call_in_edge0->flags &= ~EDGE_FALLTHRU;
  bi_call_in_edge0->flags |= EDGE_TRUE_VALUE;
  guard_bb0 = bi_call_bb;
  bi_call_bb = bi_call_in_edge0->dest;
  join_tgt_in_edge_fall_thru = make_edge (guard_bb0, join_tgt_bb,
                                          EDGE_FALSE_VALUE);

  bi_call_in_edge0->probability = REG_BR_PROB_BASE * ERR_PROB;
  bi_call_in_edge0->count =
      apply_probability (guard_bb0->count,
			 bi_call_in_edge0->probability);
  join_tgt_in_edge_fall_thru->probability =
      inverse_probability (bi_call_in_edge0->probability);
  join_tgt_in_edge_fall_thru->count =
      guard_bb0->count - bi_call_in_edge0->count;

  /* Code generation for the rest of the conditions  */
  guard_bb = guard_bb0;
  while (nconds > 0)
    {
      unsigned ci0;
      edge bi_call_in_edge;
      gimple_stmt_iterator guard_bsi = gsi_for_stmt (cond_expr_start);
      ci0 = ci;
      cond_expr_start = conds[ci0];
      for (; ci < tn_cond_stmts; ci++)
        {
          gimple c = conds[ci];
          gcc_assert (c || ci != ci0);
          if (!c)
            break;
          gsi_insert_before (&guard_bsi, c, GSI_SAME_STMT);
          cond_expr = c;
        }
      nconds--;
      ci++;
      gcc_assert (cond_expr && gimple_code (cond_expr) == GIMPLE_COND);
      guard_bb_in_edge = split_block (guard_bb, cond_expr);
      guard_bb_in_edge->flags &= ~EDGE_FALLTHRU;
      guard_bb_in_edge->flags |= EDGE_FALSE_VALUE;

      bi_call_in_edge = make_edge (guard_bb, bi_call_bb, EDGE_TRUE_VALUE);

      bi_call_in_edge->probability = REG_BR_PROB_BASE * ERR_PROB;
      bi_call_in_edge->count =
	  apply_probability (guard_bb->count,
			     bi_call_in_edge->probability);
      guard_bb_in_edge->probability =
          inverse_probability (bi_call_in_edge->probability);
      guard_bb_in_edge->count = guard_bb->count - bi_call_in_edge->count;
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

  return true;
}

/* The top level function for conditional dead code shrink
   wrapping transformation.  */

static bool
shrink_wrap_conditional_dead_built_in_calls (vec<gimple> calls)
{
  bool changed = false;
  unsigned i = 0;

  unsigned n = calls.length ();
  if (n == 0)
    return false;

  for (; i < n ; i++)
    {
      gimple bi_call = calls[i];
      changed |= shrink_wrap_one_built_in_call (bi_call);
    }

  return changed;
}

/* Pass entry points.  */

static unsigned int
tree_call_cdce (void)
{
  basic_block bb;
  gimple_stmt_iterator i;
  bool something_changed = false;
  auto_vec<gimple> cond_dead_built_in_calls;
  FOR_EACH_BB_FN (bb, cfun)
    {
      /* Collect dead call candidates.  */
      for (i = gsi_start_bb (bb); !gsi_end_p (i); gsi_next (&i))
        {
	  gimple stmt = gsi_stmt (i);
          if (is_gimple_call (stmt)
              && is_call_dce_candidate (stmt))
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

  something_changed
    = shrink_wrap_conditional_dead_built_in_calls (cond_dead_built_in_calls);

  if (something_changed)
    {
      free_dominance_info (CDI_DOMINATORS);
      free_dominance_info (CDI_POST_DOMINATORS);
      /* As we introduced new control-flow we need to insert PHI-nodes
         for the call-clobbers of the remaining call.  */
      mark_virtual_operands_for_renaming (cfun);
      return TODO_update_ssa;
    }

  return 0;
}

static bool
gate_call_cdce (void)
{
  /* The limit constants used in the implementation
     assume IEEE floating point format.  Other formats
     can be supported in the future if needed.  */
  return flag_tree_builtin_call_dce != 0 && optimize_function_for_speed_p (cfun);
}

namespace {

const pass_data pass_data_call_cdce =
{
  GIMPLE_PASS, /* type */
  "cdce", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_TREE_CALL_CDCE, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_verify_ssa, /* todo_flags_finish */
};

class pass_call_cdce : public gimple_opt_pass
{
public:
  pass_call_cdce (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_call_cdce, ctxt)
  {}

  /* opt_pass methods: */
  bool gate () { return gate_call_cdce (); }
  unsigned int execute () { return tree_call_cdce (); }

}; // class pass_call_cdce

} // anon namespace

gimple_opt_pass *
make_pass_call_cdce (gcc::context *ctxt)
{
  return new pass_call_cdce (ctxt);
}
