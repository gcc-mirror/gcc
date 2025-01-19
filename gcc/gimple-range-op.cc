/* Code for GIMPLE range op related routines.
   Copyright (C) 2019-2025 Free Software Foundation, Inc.
   Contributed by Andrew MacLeod <amacleod@redhat.com>
   and Aldy Hernandez <aldyh@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "insn-codes.h"
#include "tree.h"
#include "gimple.h"
#include "ssa.h"
#include "gimple-pretty-print.h"
#include "optabs-tree.h"
#include "gimple-iterator.h"
#include "gimple-fold.h"
#include "wide-int.h"
#include "fold-const.h"
#include "case-cfn-macros.h"
#include "omp-general.h"
#include "cfgloop.h"
#include "tree-ssa-loop.h"
#include "tree-scalar-evolution.h"
#include "langhooks.h"
#include "vr-values.h"
#include "range.h"
#include "value-query.h"
#include "gimple-range.h"
#include "attr-fnspec.h"
#include "realmpfr.h"

// Given stmt S, fill VEC, up to VEC_SIZE elements, with relevant ssa-names
// on the statement.  For efficiency, it is an error to not pass in enough
// elements for the vector.  Return the number of ssa-names.

unsigned
gimple_range_ssa_names (tree *vec, unsigned vec_size, gimple *stmt)
{
  tree ssa;
  int count = 0;

  gimple_range_op_handler handler (stmt);
  if (handler)
    {
      gcc_checking_assert (vec_size >= 2);
      if ((ssa = gimple_range_ssa_p (handler.operand1 ())))
	vec[count++] = ssa;
      if ((ssa = gimple_range_ssa_p (handler.operand2 ())))
	vec[count++] = ssa;
    }
  else if (is_a<gassign *> (stmt)
	   && gimple_assign_rhs_code (stmt) == COND_EXPR)
    {
      gcc_checking_assert (vec_size >= 3);
      gassign *st = as_a<gassign *> (stmt);
      if ((ssa = gimple_range_ssa_p (gimple_assign_rhs1 (st))))
	vec[count++] = ssa;
      if ((ssa = gimple_range_ssa_p (gimple_assign_rhs2 (st))))
	vec[count++] = ssa;
      if ((ssa = gimple_range_ssa_p (gimple_assign_rhs3 (st))))
	vec[count++] = ssa;
    }
  return count;
}

// Return the base of the RHS of an assignment.

static tree
gimple_range_base_of_assignment (const gimple *stmt)
{
  gcc_checking_assert (gimple_code (stmt) == GIMPLE_ASSIGN);
  tree op1 = gimple_assign_rhs1 (stmt);
  if (gimple_assign_rhs_code (stmt) == ADDR_EXPR)
    return get_base_address (TREE_OPERAND (op1, 0));
  return op1;
}

// If statement is supported by range-ops, set the CODE and return the TYPE.

static inline enum tree_code
get_code (gimple *s)
{
  if (const gassign *ass = dyn_cast<const gassign *> (s))
    return gimple_assign_rhs_code (ass);
  if (const gcond *cond = dyn_cast<const gcond *> (s))
    return gimple_cond_code (cond);
  return ERROR_MARK;
}

// If statement S has a supported range_op handler return TRUE.

bool
gimple_range_op_handler::supported_p (gimple *s)
{
  enum tree_code code = get_code (s);
  if (range_op_handler (code))
    return true;
  if (is_a <gcall *> (s) && gimple_range_op_handler (s))
    return true;
  return false;
}

// Construct a handler object for statement S.

gimple_range_op_handler::gimple_range_op_handler (gimple *s)
{
  range_op_handler oper (get_code (s));
  m_stmt = s;
  m_op1 = NULL_TREE;
  m_op2 = NULL_TREE;

  if (oper)
    switch (gimple_code (m_stmt))
      {
	case GIMPLE_COND:
	  m_op1 = gimple_cond_lhs (m_stmt);
	  m_op2 = gimple_cond_rhs (m_stmt);
	  // Check that operands are supported types.  One check is enough.
	  if (value_range::supports_type_p (TREE_TYPE (m_op1)))
	    m_operator = oper.range_op ();
	  gcc_checking_assert (m_operator);
	  return;
	case GIMPLE_ASSIGN:
	  m_op1 = gimple_range_base_of_assignment (m_stmt);
	  if (m_op1 && TREE_CODE (m_op1) == MEM_REF)
	    {
	      // If the base address is an SSA_NAME, we return it
	      // here.  This allows processing of the range of that
	      // name, while the rest of the expression is simply
	      // ignored.  The code in range_ops will see the
	      // ADDR_EXPR and do the right thing.
	      tree ssa = TREE_OPERAND (m_op1, 0);
	      if (TREE_CODE (ssa) == SSA_NAME)
		m_op1 = ssa;
	    }
	  if (gimple_num_ops (m_stmt) >= 3)
	    m_op2 = gimple_assign_rhs2 (m_stmt);
	  // Check that operands are supported types.  One check is enough.
	  if ((m_op1 && !value_range::supports_type_p (TREE_TYPE (m_op1))))
	    return;
	  m_operator = oper.range_op ();
	  gcc_checking_assert (m_operator);
	  return;
	default:
	  gcc_unreachable ();
	  return;
      }
  // If no range-op table entry handled this stmt, check for other supported
  // statements.
  if (is_a <gcall *> (m_stmt))
    maybe_builtin_call ();
  else
    maybe_non_standard ();
  gcc_checking_assert (m_operator);
}

// Calculate what we can determine of the range of this unary
// statement's operand if the lhs of the expression has the range
// LHS_RANGE.  Return false if nothing can be determined.

bool
gimple_range_op_handler::calc_op1 (vrange &r, const vrange &lhs_range)
{
  // Give up on empty ranges.
  if (lhs_range.undefined_p ())
    return false;

  // Unary operations require the type of the first operand in the
  // second range position.
  tree type = TREE_TYPE (operand1 ());
  value_range type_range (type);
  type_range.set_varying (type);
  return op1_range (r, type, lhs_range, type_range);
}

// Calculate what we can determine of the range of this statement's
// first operand if the lhs of the expression has the range LHS_RANGE
// and the second operand has the range OP2_RANGE.  Return false if
// nothing can be determined.

bool
gimple_range_op_handler::calc_op1 (vrange &r, const vrange &lhs_range,
				   const vrange &op2_range, relation_trio k)
{
  // Give up on empty ranges.
  if (lhs_range.undefined_p ())
    return false;

  // Unary operation are allowed to pass a range in for second operand
  // as there are often additional restrictions beyond the type which
  // can be imposed.  See operator_cast::op1_range().
  tree type = TREE_TYPE (operand1 ());
  // If op2 is undefined, solve as if it is varying.
  if (op2_range.undefined_p ())
    {
      if (gimple_num_ops (m_stmt) < 3)
	return false;
      tree op2_type;
      // This is sometimes invoked on single operand stmts.
      if (operand2 ())
	op2_type = TREE_TYPE (operand2 ());
      else
	op2_type = TREE_TYPE (operand1 ());
      value_range trange (op2_type);
      trange.set_varying (op2_type);
      return op1_range (r, type, lhs_range, trange, k);
    }
  return op1_range (r, type, lhs_range, op2_range, k);
}

// Calculate what we can determine of the range of this statement's
// second operand if the lhs of the expression has the range LHS_RANGE
// and the first operand has the range OP1_RANGE.  Return false if
// nothing can be determined.

bool
gimple_range_op_handler::calc_op2 (vrange &r, const vrange &lhs_range,
				   const vrange &op1_range, relation_trio k)
{
  // Give up on empty ranges.
  if (lhs_range.undefined_p ())
    return false;

  tree type = TREE_TYPE (operand2 ());
  // If op1 is undefined, solve as if it is varying.
  if (op1_range.undefined_p ())
    {
      tree op1_type = TREE_TYPE (operand1 ());
      value_range trange (op1_type);
      trange.set_varying (op1_type);
      return op2_range (r, type, lhs_range, trange, k);
    }
  return op2_range (r, type, lhs_range, op1_range, k);
}

// --------------------------------------------------------------------

// Implement range operator for float CFN_BUILT_IN_CONSTANT_P.
class cfn_constant_float_p : public range_operator
{
public:
  using range_operator::fold_range;
  virtual bool fold_range (irange &r, tree type, const frange &lh,
			   const irange &, relation_trio) const
  {
    if (lh.singleton_p ())
      {
	wide_int one = wi::one (TYPE_PRECISION (type));
	r.set (type, one, one);
	return true;
      }
    if (cfun->after_inlining)
      {
	r.set_zero (type);
	return true;
      }
    return false;
  }
} op_cfn_constant_float_p;

// Implement range operator for integral CFN_BUILT_IN_CONSTANT_P.
class cfn_constant_p : public range_operator
{
public:
  using range_operator::fold_range;
  virtual bool fold_range (irange &r, tree type, const irange &lh,
			   const irange &, relation_trio) const
  {
    if (lh.singleton_p ())
      {
	wide_int one = wi::one (TYPE_PRECISION (type));
	r.set (type, one, one);
	return true;
      }
    if (cfun->after_inlining)
      {
	r.set_zero (type);
	return true;
      }
    return false;
  }
} op_cfn_constant_p;

// Implement range operator for integral/pointer functions returning
// the first argument.
class cfn_pass_through_arg1 : public range_operator
{
public:
  using range_operator::fold_range;
  using range_operator::op1_range;
  virtual bool fold_range (irange &r, tree, const irange &lh,
			   const irange &, relation_trio) const
  {
    r = lh;
    return true;
  }
  virtual bool fold_range (prange &r, tree, const prange &lh,
			   const prange &, relation_trio) const
  {
    r = lh;
    return true;
  }
  virtual bool op1_range (irange &r, tree, const irange &lhs,
			  const irange &, relation_trio) const
  {
    r = lhs;
    return true;
  }
  virtual bool op1_range (prange &r, tree, const prange &lhs,
			  const prange &, relation_trio) const
  {
    r = lhs;
    return true;
  }
} op_cfn_pass_through_arg1;

// Implement range operator for CFN_BUILT_IN_SIGNBIT.
class cfn_signbit : public range_operator
{
public:
  using range_operator::fold_range;
  using range_operator::op1_range;
  virtual bool fold_range (irange &r, tree type, const frange &lh,
			   const irange &, relation_trio) const override
  {
    bool signbit;
    if (lh.signbit_p (signbit))
      {
	if (signbit)
	  r.set_nonzero (type);
	else
	  r.set_zero (type);
	return true;
      }
   return false;
  }
  virtual bool op1_range (frange &r, tree type, const irange &lhs,
			  const frange &, relation_trio) const override
  {
    if (lhs.zero_p ())
      {
	r.set (type, dconst0, frange_val_max (type));
	r.update_nan (false);
	return true;
      }
    if (!lhs.contains_p (wi::zero (TYPE_PRECISION (lhs.type ()))))
      {
	r.set (type, frange_val_min (type), dconstm0);
	r.update_nan (true);
	return true;
      }
    return false;
  }
} op_cfn_signbit;

// Implement range operator for CFN_BUILT_IN_COPYSIGN
class cfn_copysign : public range_operator
{
public:
  using range_operator::fold_range;
  virtual bool fold_range (frange &r, tree type, const frange &lh,
			   const frange &rh, relation_trio) const override
  {
    frange neg;
    if (!range_op_handler (ABS_EXPR).fold_range (r, type, lh, frange (type)))
      return false;
    if (!range_op_handler (NEGATE_EXPR).fold_range (neg, type, r,
						    frange (type)))
      return false;

    bool signbit;
    if (rh.signbit_p (signbit))
      {
	// If the sign is negative, flip the result from ABS,
	// otherwise leave things positive.
	if (signbit)
	  r = neg;
      }
    else
      // If the sign is unknown, keep the positive and negative
      // alternatives.
      r.union_ (neg);
    return true;
  }
} op_cfn_copysign;

/* Compute FUNC (ARG) where FUNC is a mpfr function.  If RES_LOW is non-NULL,
   set it to low bound of possible range if the function is expected to have
   ULPS precision and similarly if RES_HIGH is non-NULL, set it to high bound.
   If the function returns false, the results weren't set.  */

static bool
frange_mpfr_arg1 (REAL_VALUE_TYPE *res_low, REAL_VALUE_TYPE *res_high,
		  int (*func) (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t),
		  const REAL_VALUE_TYPE &arg, tree type, unsigned ulps)
{
  if (ulps == ~0U || !real_isfinite (&arg))
    return false;
  machine_mode mode = TYPE_MODE (type);
  const real_format *format = REAL_MODE_FORMAT (mode);
  auto_mpfr m (format->p);
  mpfr_from_real (m, &arg, MPFR_RNDN);
  mpfr_clear_flags ();
  bool inexact = func (m, m, MPFR_RNDN);
  if (!mpfr_number_p (m) || mpfr_overflow_p () || mpfr_underflow_p ())
    return false;

  REAL_VALUE_TYPE value, result;
  real_from_mpfr (&value, m, format, MPFR_RNDN);
  if (!real_isfinite (&value))
    return false;
  if ((value.cl == rvc_zero) != (mpfr_zero_p (m) != 0))
    inexact = true;

  real_convert (&result, format, &value);
  if (!real_isfinite (&result))
    return false;
  bool round_low = false;
  bool round_high = false;
  if (!ulps && flag_rounding_math)
    ++ulps;
  if (inexact || !real_identical (&result, &value))
    {
      if (MODE_COMPOSITE_P (mode))
	round_low = round_high = true;
      else
	{
	  round_low = !real_less (&result, &value);
	  round_high = !real_less (&value, &result);
	}
    }
  if (res_low)
    {
      *res_low = result;
      for (unsigned int i = 0; i < ulps + round_low; ++i)
	frange_nextafter (mode, *res_low, dconstninf);
    }
  if (res_high)
    {
      *res_high = result;
      for (unsigned int i = 0; i < ulps + round_high; ++i)
	frange_nextafter (mode, *res_high, dconstinf);
    }
  return true;
}

class cfn_sqrt : public range_operator
{
public:
  using range_operator::fold_range;
  using range_operator::op1_range;
  virtual bool fold_range (frange &r, tree type,
			   const frange &lh, const frange &,
			   relation_trio) const final override
  {
    if (lh.undefined_p ())
      return false;
    if (lh.known_isnan () || real_less (&lh.upper_bound (), &dconstm0))
      {
	r.set_nan (type);
	return true;
      }
    unsigned bulps
      = targetm.libm_function_max_error (CFN_SQRT, TYPE_MODE (type), true);
    if (bulps == ~0U)
      r.set_varying (type);
    else if (bulps == 0)
      r.set (type, dconstm0, dconstinf);
    else
      {
	REAL_VALUE_TYPE boundmin = dconstm0;
	while (bulps--)
	  frange_nextafter (TYPE_MODE (type), boundmin, dconstninf);
	r.set (type, boundmin, dconstinf);
      }
    if (!lh.maybe_isnan () && !real_less (&lh.lower_bound (), &dconst0))
      r.clear_nan ();

    unsigned ulps
      = targetm.libm_function_max_error (CFN_SQRT, TYPE_MODE (type), false);
    if (ulps == ~0U)
      return true;
    REAL_VALUE_TYPE lb = lh.lower_bound ();
    REAL_VALUE_TYPE ub = lh.upper_bound ();
    if (!frange_mpfr_arg1 (&lb, NULL, mpfr_sqrt, lb, type, ulps))
      lb = dconstninf;
    if (!frange_mpfr_arg1 (NULL, &ub, mpfr_sqrt, ub, type, ulps))
      ub = dconstinf;
    frange r2;
    r2.set (type, lb, ub);
    r2.flush_denormals_to_zero ();
    r.intersect (r2);
    return true;
  }
  virtual bool op1_range (frange &r, tree type,
			  const frange &lhs, const frange &,
			  relation_trio) const final override
  {
    if (lhs.undefined_p ())
      return false;

    // A known NAN means the input is [-INF,-0.) U +-NAN.
    if (lhs.known_isnan ())
      {
      known_nan:
	REAL_VALUE_TYPE ub = dconstm0;
	frange_nextafter (TYPE_MODE (type), ub, dconstninf);
	r.set (type, dconstninf, ub);
	// No r.flush_denormals_to_zero (); here - it is a reverse op.
	return true;
      }

    // Results outside of [-0.0, +Inf] are impossible.
    unsigned bulps
      = targetm.libm_function_max_error (CFN_SQRT, TYPE_MODE (type), true);
    if (bulps != ~0U)
      {
	const REAL_VALUE_TYPE &ub = lhs.upper_bound ();
	REAL_VALUE_TYPE m0 = dconstm0;
	while (bulps--)
	  frange_nextafter (TYPE_MODE (type), m0, dconstninf);
	if (real_less (&ub, &m0))
	  {
	    if (!lhs.maybe_isnan ())
	      r.set_undefined ();
	    else
	      // If lhs could be NAN and finite result is impossible,
	      // the range is like lhs.known_isnan () above.
	      goto known_nan;
	    return true;
	  }
      }

    if (!lhs.maybe_isnan ())
      // If NAN is not valid result, the input cannot include either
      // a NAN nor values smaller than -0.
      r.set (type, dconstm0, dconstinf, nan_state (false, false));
    else
      r.set_varying (type);

    unsigned ulps
      = targetm.libm_function_max_error (CFN_SQRT, TYPE_MODE (type), false);
    if (ulps == ~0U)
      return true;
    REAL_VALUE_TYPE lb = lhs.lower_bound ();
    REAL_VALUE_TYPE ub = lhs.upper_bound ();
    if (!lhs.maybe_isnan () && real_less (&dconst0, &lb))
      {
	for (unsigned i = 0; i < ulps; ++i)
	  frange_nextafter (TYPE_MODE (type), lb, dconstninf);
	if (real_less (&dconst0, &lb))
	  {
	    REAL_VALUE_TYPE op = lb;
	    frange_arithmetic (MULT_EXPR, type, lb, op, op, dconstninf);
	  }
	else
	  lb = dconstninf;
      }
    else
      lb = dconstninf;
    if (real_isfinite (&ub) && real_less (&dconst0, &ub))
      {
	for (unsigned i = 0; i < ulps; ++i)
	  frange_nextafter (TYPE_MODE (type), ub, dconstinf);
	if (real_isfinite (&ub))
	  {
	    REAL_VALUE_TYPE op = ub;
	    frange_arithmetic (MULT_EXPR, type, ub, op, op, dconstinf);
	  }
	else
	  ub = dconstinf;
      }
    else
      ub = dconstinf;
    frange r2;
    r2.set (type, lb, ub);
    r.intersect (r2);
    return true;
  }
} op_cfn_sqrt;

class cfn_sincos : public range_operator
{
public:
  using range_operator::fold_range;
  using range_operator::op1_range;
  cfn_sincos (combined_fn cfn) { m_cfn = cfn; }
  virtual bool fold_range (frange &r, tree type,
			   const frange &lh, const frange &,
			   relation_trio) const final override
  {
    if (lh.undefined_p ())
      return false;
    if (lh.known_isnan () || lh.known_isinf ())
      {
	r.set_nan (type);
	return true;
      }
    unsigned bulps = targetm.libm_function_max_error (m_cfn, TYPE_MODE (type),
						      true);
    if (bulps == ~0U)
      r.set_varying (type);
    else if (bulps == 0)
      r.set (type, dconstm1, dconst1);
    else
      {
	REAL_VALUE_TYPE boundmin, boundmax;
	boundmax = dconst1;
	while (bulps--)
	  frange_nextafter (TYPE_MODE (type), boundmax, dconstinf);
	real_arithmetic (&boundmin, NEGATE_EXPR, &boundmax, NULL);
	r.set (type, boundmin, boundmax);
      }
    if (!lh.maybe_isnan () && !lh.maybe_isinf ())
      r.clear_nan ();

    unsigned ulps
      = targetm.libm_function_max_error (m_cfn, TYPE_MODE (type), false);
    if (ulps == ~0U)
      return true;
    REAL_VALUE_TYPE lb = lh.lower_bound ();
    REAL_VALUE_TYPE ub = lh.upper_bound ();
    REAL_VALUE_TYPE diff;
    real_arithmetic (&diff, MINUS_EXPR, &ub, &lb);
    if (!real_isfinite (&diff))
      return true;
    REAL_VALUE_TYPE pi = dconst_pi ();
    REAL_VALUE_TYPE pix2;
    real_arithmetic (&pix2, PLUS_EXPR, &pi, &pi);
    // We can only try to narrow the range further if ub-lb < 2*pi.
    if (!real_less (&diff, &pix2))
      return true;
    REAL_VALUE_TYPE lb_lo, lb_hi, ub_lo, ub_hi;
    REAL_VALUE_TYPE lb_deriv_lo, lb_deriv_hi, ub_deriv_lo, ub_deriv_hi;
    if (!frange_mpfr_arg1 (&lb_lo, &lb_hi,
			   m_cfn == CFN_SIN ? mpfr_sin : mpfr_cos, lb,
			   type, ulps)
	|| !frange_mpfr_arg1 (&ub_lo, &ub_hi,
			      m_cfn == CFN_SIN ? mpfr_sin : mpfr_cos, ub,
			      type, ulps)
	|| !frange_mpfr_arg1 (&lb_deriv_lo, &lb_deriv_hi,
			      m_cfn == CFN_SIN ? mpfr_cos : mpfr_sin, lb,
			      type, 0)
	|| !frange_mpfr_arg1 (&ub_deriv_lo, &ub_deriv_hi,
			      m_cfn == CFN_SIN ? mpfr_cos : mpfr_sin, ub,
			      type, 0))
      return true;
    if (m_cfn == CFN_COS)
      {
	// Derivative of cos is -sin, so negate.
	lb_deriv_lo.sign ^= 1;
	lb_deriv_hi.sign ^= 1;
	ub_deriv_lo.sign ^= 1;
	ub_deriv_hi.sign ^= 1;
      }

    if (real_less (&lb_lo, &ub_lo))
      lb = lb_lo;
    else
      lb = ub_lo;
    if (real_less (&lb_hi, &ub_hi))
      ub = ub_hi;
    else
      ub = lb_hi;

    // The range between the function result on the boundaries may need
    // to be extended to +1 (+Inf) or -1 (-Inf) or both depending on the
    // derivative or length of the argument range (diff).

    // First handle special case, where the derivative has different signs,
    // so the bound must be roughly -1 or +1.
    if (real_isneg (&lb_deriv_lo) != real_isneg (&lb_deriv_hi))
      {
	if (real_isneg (&lb_lo))
	  lb = dconstninf;
	else
	  ub = dconstinf;
      }
    if (real_isneg (&ub_deriv_lo) != real_isneg (&ub_deriv_hi))
      {
	if (real_isneg (&ub_lo))
	  lb = dconstninf;
	else
	  ub = dconstinf;
      }

    // If derivative at lower_bound and upper_bound have the same sign,
    // the function grows or declines on the whole range if diff < pi, so
    // [lb, ub] is correct, and if diff >= pi the result range must include
    // both the minimum and maximum.
    if (real_isneg (&lb_deriv_lo) == real_isneg (&ub_deriv_lo))
      {
	if (!real_less (&diff, &pi))
	  return true;
      }
    // If function declines at lower_bound and grows at upper_bound,
    // the result range must include the minimum, so set lb to -Inf.
    else if (real_isneg (&lb_deriv_lo))
      lb = dconstninf;
    // If function grows at lower_bound and declines at upper_bound,
    // the result range must include the maximum, so set ub to +Inf.
    else
      ub = dconstinf;
    frange r2;
    r2.set (type, lb, ub);
    r2.flush_denormals_to_zero ();
    r.intersect (r2);
    return true;
  }
  virtual bool op1_range (frange &r, tree type,
			  const frange &lhs, const frange &,
			  relation_trio) const final override
  {
    if (lhs.undefined_p ())
      return false;

    // A known NAN means the input is [-INF,-INF][+INF,+INF] U +-NAN,
    // which we can't currently represent.
    if (lhs.known_isnan ())
      {
	r.set_varying (type);
	return true;
      }

    // Results outside of [-1.0, +1.0] are impossible.
    unsigned bulps
      = targetm.libm_function_max_error (m_cfn, TYPE_MODE (type), true);
    if (bulps != ~0U)
      {
	const REAL_VALUE_TYPE &lb = lhs.lower_bound ();
	const REAL_VALUE_TYPE &ub = lhs.upper_bound ();
	REAL_VALUE_TYPE m1 = dconstm1;
	REAL_VALUE_TYPE p1 = dconst1;
	while (bulps--)
	  {
	    frange_nextafter (TYPE_MODE (type), m1, dconstninf);
	    frange_nextafter (TYPE_MODE (type), p1, dconstinf);
	  }
	if (real_less (&ub, &m1) || real_less (&p1, &lb))
	  {
	    if (!lhs.maybe_isnan ())
	      r.set_undefined ();
	    else
	      /* If lhs could be NAN and finite result is impossible,
		 the range is like lhs.known_isnan () above,
		 [-INF,-INF][+INF,+INF] U +-NAN.  */
	      r.set_varying (type);
	    return true;
	  }
      }

    if (!lhs.maybe_isnan ())
      {
	// If NAN is not valid result, the input cannot include either
	// a NAN nor a +-INF.
	REAL_VALUE_TYPE lb = real_min_representable (type);
	REAL_VALUE_TYPE ub = real_max_representable (type);
	r.set (type, lb, ub, nan_state (false, false));
      }
    else
      r.set_varying (type);
    return true;
  }
private:
  combined_fn m_cfn;
} op_cfn_sin (CFN_SIN), op_cfn_cos (CFN_COS);

// Implement range operator for CFN_BUILT_IN_TOUPPER and CFN_BUILT_IN_TOLOWER.
class cfn_toupper_tolower : public range_operator
{
public:
  using range_operator::fold_range;
  cfn_toupper_tolower (bool toupper)  { m_toupper = toupper; }
  virtual bool fold_range (irange &r, tree type, const irange &lh,
			   const irange &, relation_trio) const;
private:
  bool get_letter_range (tree type, irange &lowers, irange &uppers) const;
  bool m_toupper;
} op_cfn_toupper (true), op_cfn_tolower (false);

// Return TRUE if we recognize the target character set and return the
// range for lower case and upper case letters.

bool
cfn_toupper_tolower::get_letter_range (tree type, irange &lowers,
				       irange &uppers) const
{
  // ASCII
  int a = lang_hooks.to_target_charset ('a');
  int z = lang_hooks.to_target_charset ('z');
  int A = lang_hooks.to_target_charset ('A');
  int Z = lang_hooks.to_target_charset ('Z');

  if ((z - a == 25) && (Z - A == 25))
    {
      lowers = int_range<2> (type,
			     wi::shwi (a, TYPE_PRECISION (type)),
			     wi::shwi (z, TYPE_PRECISION (type)));
      uppers = int_range<2> (type,
			     wi::shwi (A, TYPE_PRECISION (type)),
			     wi::shwi (Z, TYPE_PRECISION (type)));
      return true;
    }
  // Unknown character set.
  return false;
}

bool
cfn_toupper_tolower::fold_range (irange &r, tree type, const irange &lh,
				 const irange &, relation_trio) const
{
  int_range<3> lowers;
  int_range<3> uppers;
  if (!get_letter_range (type, lowers, uppers))
    return false;

  r = lh;
  if (m_toupper)
    {
      // Return the range passed in without any lower case characters,
      // but including all the upper case ones.
      lowers.invert ();
      r.intersect (lowers);
      r.union_ (uppers);
    }
  else
    {
      // Return the range passed in without any lower case characters,
      // but including all the upper case ones.
      uppers.invert ();
      r.intersect (uppers);
      r.union_ (lowers);
    }
  return true;
}

// Implement range operator for CFN_BUILT_IN_FFS.
class cfn_ffs : public range_operator
{
public:
  using range_operator::fold_range;
  virtual bool fold_range (irange &r, tree type, const irange &lh,
			   const irange &, relation_trio) const
  {
    if (lh.undefined_p ())
      return false;
    // __builtin_ffs* and __builtin_popcount* return [0, prec].
    int prec = TYPE_PRECISION (lh.type ());
    // If arg is non-zero, then ffs or popcount are non-zero.
    int mini = range_includes_zero_p (lh) ? 0 : 1;
    int maxi = prec;

    // If some high bits are known to be zero, decrease the maximum.
    int_range_max tmp = lh;
    if (TYPE_SIGN (tmp.type ()) == SIGNED)
      range_cast (tmp, unsigned_type_for (tmp.type ()));
    wide_int max = tmp.upper_bound ();
    maxi = wi::floor_log2 (max) + 1;
    r.set (type,
	   wi::shwi (mini, TYPE_PRECISION (type)),
	   wi::shwi (maxi, TYPE_PRECISION (type)));
    return true;
  }
} op_cfn_ffs;

// Implement range operator for CFN_BUILT_IN_POPCOUNT.
class cfn_popcount : public cfn_ffs
{
public:
  using range_operator::fold_range;
  virtual bool fold_range (irange &r, tree type, const irange &lh,
			   const irange &rh, relation_trio rel) const
  {
    if (lh.undefined_p ())
      return false;
    unsigned prec = TYPE_PRECISION (type);
    irange_bitmask bm = lh.get_bitmask ();
    wide_int nz = bm.get_nonzero_bits ();
    wide_int high = wi::shwi (wi::popcount (nz), prec);
    // Calculating the popcount of a singleton is trivial.
    if (lh.singleton_p ())
      {
	r.set (type, high, high);
	return true;
      }
    if (cfn_ffs::fold_range (r, type, lh, rh, rel))
      {
	wide_int known_ones = ~bm.mask () & bm.value ();
	wide_int low = wi::shwi (wi::popcount (known_ones), prec);
	int_range<2> tmp (type, low, high);
	r.intersect (tmp);
	return true;
      }
    return false;
  }
} op_cfn_popcount;

// Implement range operator for CFN_BUILT_IN_CLZ
class cfn_clz : public range_operator
{
public:
  cfn_clz (bool internal) { m_gimple_call_internal_p = internal; }
  using range_operator::fold_range;
  virtual bool fold_range (irange &r, tree type, const irange &lh,
			   const irange &rh, relation_trio) const;
private:
  bool m_gimple_call_internal_p;
} op_cfn_clz (false), op_cfn_clz_internal (true);

bool
cfn_clz::fold_range (irange &r, tree type, const irange &lh,
		     const irange &rh, relation_trio) const
{
  // __builtin_c[lt]z* return [0, prec-1], except when the
  // argument is 0, but that is undefined behavior.
  //
  // For __builtin_c[lt]z* consider argument of 0 always undefined
  // behavior, for internal fns likewise, unless it has 2 arguments,
  // then the second argument is the value at zero.
  if (lh.undefined_p ())
    return false;
  int prec = TYPE_PRECISION (lh.type ());
  int mini = 0;
  int maxi = prec - 1;
  if (m_gimple_call_internal_p)
    {
      // Handle only the two common values.
      if (rh.lower_bound () == -1)
	mini = -1;
      else if (rh.lower_bound () == prec)
	maxi = prec;
      else
	// Magic value to give up, unless we can prove arg is non-zero.
	mini = -2;
    }

  // From clz of minimum we can compute result maximum.
  if (wi::gt_p (lh.lower_bound (), 0, TYPE_SIGN (lh.type ())))
    {
      maxi = prec - 1 - wi::floor_log2 (lh.lower_bound ());
      if (mini < 0)
	mini = 0;
    }
  else if (!range_includes_zero_p (lh))
    {
      mini = 0;
      maxi = prec - 1;
    }
  if (mini == -2)
    return false;
  // From clz of maximum we can compute result minimum.
  wide_int max = lh.upper_bound ();
  int newmini = prec - 1 - wi::floor_log2 (max);
  if (max == 0)
    {
      // If CLZ_DEFINED_VALUE_AT_ZERO is 2 with VALUE of prec,
      // return [prec, prec] or [-1, -1], otherwise ignore the range.
      if (maxi == prec)
	mini = prec;
      else if (mini == -1)
	maxi = -1;
    }
  else if (mini >= 0)
    mini = newmini;

  if (mini == -2)
    return false;
  r.set (type,
	 wi::shwi (mini, TYPE_PRECISION (type)),
	 wi::shwi (maxi, TYPE_PRECISION (type)));
  return true;
}

// Implement range operator for CFN_BUILT_IN_CTZ
class cfn_ctz : public range_operator
{
public:
  cfn_ctz (bool internal) { m_gimple_call_internal_p = internal; }
  using range_operator::fold_range;
  virtual bool fold_range (irange &r, tree type, const irange &lh,
			   const irange &rh, relation_trio) const;
private:
  bool m_gimple_call_internal_p;
} op_cfn_ctz (false), op_cfn_ctz_internal (true);

bool
cfn_ctz::fold_range (irange &r, tree type, const irange &lh,
		     const irange &rh, relation_trio) const
{
  if (lh.undefined_p ())
    return false;
  int prec = TYPE_PRECISION (lh.type ());
  int mini = 0;
  int maxi = prec - 1;

  if (m_gimple_call_internal_p)
    {
      // Handle only the two common values.
      if (rh.lower_bound () == -1)
	mini = -1;
      else if (rh.lower_bound () == prec)
	maxi = prec;
      else
	// Magic value to give up, unless we can prove arg is non-zero.
	mini = -2;
    }
  // If arg is non-zero, then use [0, prec - 1].
  if (!range_includes_zero_p (lh))
    {
      mini = 0;
      maxi = prec - 1;
    }
  // If some high bits are known to be zero, we can decrease
  // the maximum.
  wide_int max = lh.upper_bound ();
  if (max == 0)
    {
      // Argument is [0, 0].  If CTZ_DEFINED_VALUE_AT_ZERO
      // is 2 with value -1 or prec, return [-1, -1] or [prec, prec].
      // Otherwise ignore the range.
      if (mini == -1)
	maxi = -1;
      else if (maxi == prec)
	mini = prec;
    }
  // If value at zero is prec and 0 is in the range, we can't lower
  // the upper bound.  We could create two separate ranges though,
  // [0,floor_log2(max)][prec,prec] though.
  else if (maxi != prec)
    maxi = wi::floor_log2 (max);

  if (mini == -2)
    return false;
  r.set (type,
	 wi::shwi (mini, TYPE_PRECISION (type)),
	 wi::shwi (maxi, TYPE_PRECISION (type)));
  return true;
}


// Implement range operator for CFN_BUILT_IN_
class cfn_clrsb : public range_operator
{
public:
  using range_operator::fold_range;
  virtual bool fold_range (irange &r, tree type, const irange &lh,
			   const irange &, relation_trio) const
  {
    if (lh.undefined_p ())
      return false;
    int prec = TYPE_PRECISION (lh.type ());
    r.set (type,
	   wi::zero (TYPE_PRECISION (type)),
	   wi::shwi (prec - 1, TYPE_PRECISION (type)));
    return true;
  }
} op_cfn_clrsb;


// Implement range operator for CFN_BUILT_IN_
class cfn_ubsan : public range_operator
{
public:
  cfn_ubsan (enum tree_code code) { m_code = code; }
  using range_operator::fold_range;
  virtual bool fold_range (irange &r, tree type, const irange &lh,
			   const irange &rh, relation_trio rel) const
  {
    bool saved_flag_wrapv = flag_wrapv;
    // Pretend the arithmetic is wrapping.  If there is any overflow,
    // we'll complain, but will actually do wrapping operation.
    flag_wrapv = 1;
    bool result = range_op_handler (m_code).fold_range (r, type, lh, rh, rel);
    flag_wrapv = saved_flag_wrapv;

    // If for both arguments vrp_valueize returned non-NULL, this should
    // have been already folded and if not, it wasn't folded because of
    // overflow.  Avoid removing the UBSAN_CHECK_* calls in that case.
    if (result && r.singleton_p ())
      r.set_varying (type);
    return result;
  }
private:
  enum tree_code m_code;
};

cfn_ubsan op_cfn_ubsan_add (PLUS_EXPR);
cfn_ubsan op_cfn_ubsan_sub (MINUS_EXPR);
cfn_ubsan op_cfn_ubsan_mul (MULT_EXPR);


// Implement range operator for CFN_BUILT_IN_STRLEN
class cfn_strlen : public range_operator
{
public:
  using range_operator::fold_range;
  virtual bool fold_range (irange &r, tree type, const prange &,
			   const irange &, relation_trio) const
  {
    wide_int max = irange_val_max (ptrdiff_type_node);
    // To account for the terminating NULL, the maximum length
    // is one less than the maximum array size, which in turn
    // is one less than PTRDIFF_MAX (or SIZE_MAX where it's
    // smaller than the former type).
    // FIXME: Use max_object_size() - 1 here.
    r.set (type, wi::zero (TYPE_PRECISION (type)), max - 2);
    return true;
  }
} op_cfn_strlen;


// Implement range operator for CFN_BUILT_IN_GOACC_DIM
class cfn_goacc_dim : public range_operator
{
public:
  cfn_goacc_dim (bool is_pos) { m_is_pos = is_pos; }
  using range_operator::fold_range;
  virtual bool fold_range (irange &r, tree type, const irange &lh,
			   const irange &, relation_trio) const
  {
    tree axis_tree;
    if (!lh.singleton_p (&axis_tree))
      return false;
    HOST_WIDE_INT axis = TREE_INT_CST_LOW (axis_tree);
    int size = oacc_get_fn_dim_size (current_function_decl, axis);
    if (!size)
      // If it's dynamic, the backend might know a hardware limitation.
      size = targetm.goacc.dim_limit (axis);

    r.set (type,
	   wi::shwi (m_is_pos ? 0 : 1, TYPE_PRECISION (type)),
	   size
	   ? wi::shwi (size - m_is_pos, TYPE_PRECISION (type))
	   : irange_val_max (type));
    return true;
  }
private:
  bool m_is_pos;
} op_cfn_goacc_dim_size (false), op_cfn_goacc_dim_pos (true);

// Implement range operator for CFN_BUILT_IN_ISINF
class cfn_isinf : public range_operator
{
public:
  using range_operator::fold_range;
  using range_operator::op1_range;
  virtual bool fold_range (irange &r, tree type, const frange &op1,
			   const irange &, relation_trio) const override
  {
    if (op1.undefined_p ())
      return false;

    if (op1.known_isinf ())
      {
	wide_int one = wi::one (TYPE_PRECISION (type));
	r.set (type, one, one);
	return true;
      }

    if (op1.known_isnan ()
	|| (!real_isinf (&op1.lower_bound ())
	    && !real_isinf (&op1.upper_bound ())))
      {
	r.set_zero (type);
	return true;
      }

    r.set_varying (type);
    return true;
  }
  virtual bool op1_range (frange &r, tree type, const irange &lhs,
			  const frange &, relation_trio) const override
  {
    if (lhs.undefined_p ())
      return false;

    if (lhs.zero_p ())
      {
	nan_state nan (true);
	r.set (type, real_min_representable (type),
	       real_max_representable (type), nan);
	return true;
      }

    if (!range_includes_zero_p (lhs))
      {
	// The range is [-INF,-INF][+INF,+INF], but it can't be represented.
	// Set range to [-INF,+INF]
	r.set_varying (type);
	r.clear_nan ();
	return true;
      }

    r.set_varying (type);
    return true;
  }
} op_cfn_isinf;

//Implement range operator for CFN_BUILT_IN_ISFINITE
class cfn_isfinite : public range_operator
{
public:
  using range_operator::fold_range;
  using range_operator::op1_range;
  virtual bool fold_range (irange &r, tree type, const frange &op1,
			   const irange &, relation_trio) const override
  {
    if (op1.undefined_p ())
      return false;

    if (op1.known_isfinite ())
      {
	wide_int one = wi::one (TYPE_PRECISION (type));
	r.set (type, one, one);
	return true;
      }

    if (op1.known_isnan ()
	|| op1.known_isinf ())
      {
	r.set_zero (type);
	return true;
      }

    r.set_varying (type);
    return true;
  }
  virtual bool op1_range (frange &r, tree type, const irange &lhs,
			  const frange &, relation_trio) const override
  {
    if (lhs.undefined_p ())
      return false;

    if (lhs.zero_p ())
      {
	// The range is [-INF,-INF][+INF,+INF] NAN, but it can't be represented.
	// Set range to varying
	r.set_varying (type);
	return true;
      }

    if (!range_includes_zero_p (lhs))
      {
	nan_state nan (false);
	r.set (type, real_min_representable (type),
	       real_max_representable (type), nan);
	return true;
      }

    r.set_varying (type);
    return true;
  }
} op_cfn_isfinite;

//Implement range operator for CFN_BUILT_IN_ISNORMAL
class cfn_isnormal :  public range_operator
{
public:
  using range_operator::fold_range;
  using range_operator::op1_range;
  virtual bool fold_range (irange &r, tree type, const frange &op1,
			   const irange &, relation_trio) const override
  {
    if (op1.undefined_p ())
      return false;

    if (op1.known_isnormal ())
      {
	wide_int one = wi::one (TYPE_PRECISION (type));
	r.set (type, one, one);
	return true;
      }

    if (op1.known_isnan ()
	|| op1.known_isinf ()
	|| op1.known_isdenormal_or_zero ())
      {
	r.set_zero (type);
	return true;
      }

    r.set_varying (type);
    return true;
  }
  virtual bool op1_range (frange &r, tree type, const irange &lhs,
			  const frange &, relation_trio) const override
  {
    if (lhs.undefined_p ())
      return false;

    if (lhs.zero_p ())
      {
	r.set_varying (type);
	return true;
      }

    if (!range_includes_zero_p (lhs))
      {
	nan_state nan (false);
	r.set (type, real_min_representable (type),
	       real_max_representable (type), nan);
	return true;
      }

    r.set_varying (type);
    return true;
  }
} op_cfn_isnormal;

// Implement range operator for CFN_BUILT_IN_
class cfn_parity : public range_operator
{
public:
  using range_operator::fold_range;
  virtual bool fold_range (irange &r, tree type, const irange &,
			   const irange &, relation_trio) const
  {
    r = range_true_and_false (type);
    return true;
  }
} op_cfn_parity;

// Set up a gimple_range_op_handler for any nonstandard function which can be
// supported via range-ops.

void
gimple_range_op_handler::maybe_non_standard ()
{
  range_op_handler signed_op (OP_WIDEN_MULT_SIGNED);
  gcc_checking_assert (signed_op);
  range_op_handler unsigned_op (OP_WIDEN_MULT_UNSIGNED);
  gcc_checking_assert (unsigned_op);

  if (gimple_code (m_stmt) == GIMPLE_ASSIGN)
    switch (gimple_assign_rhs_code (m_stmt))
      {
	case WIDEN_MULT_EXPR:
	{
	  m_op1 = gimple_assign_rhs1 (m_stmt);
	  m_op2 = gimple_assign_rhs2 (m_stmt);
	  tree ret = gimple_assign_lhs (m_stmt);
	  bool signed1 = TYPE_SIGN (TREE_TYPE (m_op1)) == SIGNED;
	  bool signed2 = TYPE_SIGN (TREE_TYPE (m_op2)) == SIGNED;
	  bool signed_ret = TYPE_SIGN (TREE_TYPE (ret)) == SIGNED;

	  /* Normally these operands should all have the same sign, but
	     some passes and violate this by taking mismatched sign args.  At
	     the moment the only one that's possible is mismatch inputs and
	     unsigned output.  Once ranger supports signs for the operands we
	     can properly fix it,  for now only accept the case we can do
	     correctly.  */
	  if ((signed1 ^ signed2) && signed_ret)
	    return;

	  if (signed2 && !signed1)
	    std::swap (m_op1, m_op2);

	  if (signed1 || signed2)
	    m_operator = signed_op.range_op ();
	  else
	    m_operator = unsigned_op.range_op ();
	  break;
	}
	default:
	  break;
      }
}

// Set up a gimple_range_op_handler for any built in function which can be
// supported via range-ops.

void
gimple_range_op_handler::maybe_builtin_call ()
{
  gcc_checking_assert (is_a <gcall *> (m_stmt));

  gcall *call = as_a <gcall *> (m_stmt);
  combined_fn func = gimple_call_combined_fn (call);
  if (func == CFN_LAST)
    return;
  tree type = gimple_range_type (call);
  if (!type)
    return;
  if (!value_range::supports_type_p (type))
    return;

  switch (func)
    {
    case CFN_BUILT_IN_CONSTANT_P:
      m_op1 = gimple_call_arg (call, 0);
      if (irange::supports_p (TREE_TYPE (m_op1)))
	m_operator = &op_cfn_constant_p;
      else if (frange::supports_p (TREE_TYPE (m_op1)))
	m_operator = &op_cfn_constant_float_p;
      break;

    CASE_FLT_FN (CFN_BUILT_IN_SIGNBIT):
      m_op1 = gimple_call_arg (call, 0);
      m_operator = &op_cfn_signbit;
      break;

    CASE_FLT_FN (BUILT_IN_ISINF):
      m_op1 = gimple_call_arg (call, 0);
      m_operator = &op_cfn_isinf;
      break;

    case CFN_BUILT_IN_ISFINITE:
      m_op1 = gimple_call_arg (call, 0);
      m_operator = &op_cfn_isfinite;
      break;

    case CFN_BUILT_IN_ISNORMAL:
      m_op1 = gimple_call_arg (call, 0);
      m_operator = &op_cfn_isnormal;
      break;

    CASE_CFN_COPYSIGN_ALL:
      m_op1 = gimple_call_arg (call, 0);
      m_op2 = gimple_call_arg (call, 1);
      m_operator = &op_cfn_copysign;
      break;

    CASE_CFN_SQRT:
    CASE_CFN_SQRT_FN:
      m_op1 = gimple_call_arg (call, 0);
      m_operator = &op_cfn_sqrt;
      break;

    CASE_CFN_SIN:
    CASE_CFN_SIN_FN:
      m_op1 = gimple_call_arg (call, 0);
      m_operator = &op_cfn_sin;
      break;

    CASE_CFN_COS:
    CASE_CFN_COS_FN:
      m_op1 = gimple_call_arg (call, 0);
      m_operator = &op_cfn_cos;
      break;

    case CFN_BUILT_IN_TOUPPER:
    case CFN_BUILT_IN_TOLOWER:
      // Only proceed If the argument is compatible with the LHS.
      m_op1 = gimple_call_arg (call, 0);
      if (range_compatible_p (type, TREE_TYPE (m_op1)))
	m_operator = (func == CFN_BUILT_IN_TOLOWER) ? &op_cfn_tolower
						    : &op_cfn_toupper;
      break;

    CASE_CFN_FFS:
      m_op1 = gimple_call_arg (call, 0);
      m_operator = &op_cfn_ffs;
      break;

    CASE_CFN_POPCOUNT:
      m_op1 = gimple_call_arg (call, 0);
      m_operator = &op_cfn_popcount;
      break;

    CASE_CFN_CLZ:
      m_op1 = gimple_call_arg (call, 0);
      if (gimple_call_internal_p (call)
	  && gimple_call_num_args (call) == 2)
	{
	  m_op2 = gimple_call_arg (call, 1);
	  m_operator = &op_cfn_clz_internal;
	}
      else
	m_operator = &op_cfn_clz;
      break;

    CASE_CFN_CTZ:
      m_op1 = gimple_call_arg (call, 0);
      if (gimple_call_internal_p (call)
	  && gimple_call_num_args (call) == 2)
	{
	  m_op2 = gimple_call_arg (call, 1);
	  m_operator = &op_cfn_ctz_internal;
	}
      else
	m_operator = &op_cfn_ctz;
      break;

    CASE_CFN_CLRSB:
      m_op1 = gimple_call_arg (call, 0);
      m_operator = &op_cfn_clrsb;
      break;

    case CFN_UBSAN_CHECK_ADD:
      m_op1 = gimple_call_arg (call, 0);
      m_op2 = gimple_call_arg (call, 1);
      m_operator = &op_cfn_ubsan_add;
      break;

    case CFN_UBSAN_CHECK_SUB:
      m_op1 = gimple_call_arg (call, 0);
      m_op2 = gimple_call_arg (call, 1);
      m_operator = &op_cfn_ubsan_sub;
      break;

    case CFN_UBSAN_CHECK_MUL:
      m_op1 = gimple_call_arg (call, 0);
      m_op2 = gimple_call_arg (call, 1);
      m_operator = &op_cfn_ubsan_mul;
      break;

    case CFN_BUILT_IN_STRLEN:
      {
	tree lhs = gimple_call_lhs (call);
	if (lhs && ptrdiff_type_node && (TYPE_PRECISION (ptrdiff_type_node)
					 == TYPE_PRECISION (TREE_TYPE (lhs))))
	  {
	    m_op1 = gimple_call_arg (call, 0);
	    m_operator = &op_cfn_strlen;
	  }
	break;
      }

    // Optimizing these two internal functions helps the loop
    // optimizer eliminate outer comparisons.  Size is [1,N]
    // and pos is [0,N-1].
    case CFN_GOACC_DIM_SIZE:
      // This call will ensure all the asserts are triggered.
      oacc_get_ifn_dim_arg (call);
      m_op1 = gimple_call_arg (call, 0);
      m_operator = &op_cfn_goacc_dim_size;
      break;

    case CFN_GOACC_DIM_POS:
      // This call will ensure all the asserts are triggered.
      oacc_get_ifn_dim_arg (call);
      m_op1 = gimple_call_arg (call, 0);
      m_operator = &op_cfn_goacc_dim_pos;
      break;

    CASE_CFN_PARITY:
      m_operator = &op_cfn_parity;
      break;

    default:
      {
	unsigned arg;
	if (gimple_call_fnspec (call).returns_arg (&arg) && arg == 0)
	  {
	    m_op1 = gimple_call_arg (call, 0);
	    m_operator = &op_cfn_pass_through_arg1;
	  }
	break;
      }
    }
}
