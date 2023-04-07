/* Code for GIMPLE range op related routines.
   Copyright (C) 2019-2023 Free Software Foundation, Inc.
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

static tree
get_code_and_type (gimple *s, enum tree_code &code)
{
  tree type = NULL_TREE;
  code = NOP_EXPR;

  if (const gassign *ass = dyn_cast<const gassign *> (s))
    {
      code = gimple_assign_rhs_code (ass);
      // The LHS of a comparison is always an int, so we must look at
      // the operands.
      if (TREE_CODE_CLASS (code) == tcc_comparison)
	type = TREE_TYPE (gimple_assign_rhs1 (ass));
      else
	type = TREE_TYPE (gimple_assign_lhs (ass));
    }
  else if (const gcond *cond = dyn_cast<const gcond *> (s))
    {
      code = gimple_cond_code (cond);
      type = TREE_TYPE (gimple_cond_lhs (cond));
    }
  return type;
}

// If statement S has a supported range_op handler return TRUE.

bool
gimple_range_op_handler::supported_p (gimple *s)
{
  enum tree_code code;
  tree type = get_code_and_type (s, code);
  if (type && range_op_handler (code, type))
    return true;
  if (is_a <gcall *> (s) && gimple_range_op_handler (s))
    return true;
  return false;
}

// Construct a handler object for statement S.

gimple_range_op_handler::gimple_range_op_handler (gimple *s)
{
  enum tree_code code;
  tree type = get_code_and_type (s, code);
  m_stmt = s;
  m_op1 = NULL_TREE;
  m_op2 = NULL_TREE;
  if (type)
    set_op_handler (code, type);

  if (m_valid)
    switch (gimple_code (m_stmt))
      {
	case GIMPLE_COND:
	  m_op1 = gimple_cond_lhs (m_stmt);
	  m_op2 = gimple_cond_rhs (m_stmt);
	  // Check that operands are supported types.  One check is enough.
	  if (!Value_Range::supports_type_p (TREE_TYPE (m_op1)))
	    m_valid = false;
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
	  if ((m_op1 && !Value_Range::supports_type_p (TREE_TYPE (m_op1))))
	    m_valid = false;
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
}

// Calculate what we can determine of the range of this unary
// statement's operand if the lhs of the expression has the range
// LHS_RANGE.  Return false if nothing can be determined.

bool
gimple_range_op_handler::calc_op1 (vrange &r, const vrange &lhs_range)
{
  gcc_checking_assert (gimple_num_ops (m_stmt) < 3);
  // Give up on empty ranges.
  if (lhs_range.undefined_p ())
    return false;

  // Unary operations require the type of the first operand in the
  // second range position.
  tree type = TREE_TYPE (operand1 ());
  Value_Range type_range (type);
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
      Value_Range trange (op2_type);
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
      Value_Range trange (op1_type);
      trange.set_varying (op1_type);
      return op2_range (r, type, lhs_range, trange, k);
    }
  return op2_range (r, type, lhs_range, op1_range, k);
}

// --------------------------------------------------------------------

// Implement range operator for float CFN_BUILT_IN_CONSTANT_P.
class cfn_constant_float_p : public range_operator_float
{
public:
  using range_operator_float::fold_range;
  virtual bool fold_range (irange &r, tree type, const frange &lh,
			   const irange &, relation_trio) const
  {
    if (lh.singleton_p ())
      {
	r.set (build_one_cst (type), build_one_cst (type));
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
	r.set (build_one_cst (type), build_one_cst (type));
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
  virtual bool fold_range (irange &r, tree, const irange &lh,
			   const irange &, relation_trio) const
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
} op_cfn_pass_through_arg1;

// Implement range operator for CFN_BUILT_IN_SIGNBIT.
class cfn_signbit : public range_operator_float
{
public:
  using range_operator_float::fold_range;
  using range_operator_float::op1_range;
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
    if (!lhs.contains_p (build_zero_cst (lhs.type ())))
      {
	REAL_VALUE_TYPE dconstm0 = dconst0;
	dconstm0.sign = 1;
	r.set (type, frange_val_min (type), dconstm0);
	r.update_nan (true);
	return true;
      }
    return false;
  }
} op_cfn_signbit;

// Implement range operator for CFN_BUILT_IN_COPYSIGN
class cfn_copysign : public range_operator_float
{
public:
  using range_operator_float::fold_range;
  virtual bool fold_range (frange &r, tree type, const frange &lh,
			   const frange &rh, relation_trio) const override
  {
    frange neg;
    range_op_handler abs_op (ABS_EXPR, type);
    range_op_handler neg_op (NEGATE_EXPR, type);
    if (!abs_op || !abs_op.fold_range (r, type, lh, frange (type)))
      return false;
    if (!neg_op || !neg_op.fold_range (neg, type, r, frange (type)))
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
      lowers = int_range<2> (build_int_cst (type, a), build_int_cst (type, z));
      uppers = int_range<2> (build_int_cst (type, A), build_int_cst (type, Z));
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
    int mini = range_includes_zero_p (&lh) ? 0 : 1;
    int maxi = prec;

    // If some high bits are known to be zero, decrease the maximum.
    int_range_max tmp = lh;
    if (TYPE_SIGN (tmp.type ()) == SIGNED)
      range_cast (tmp, unsigned_type_for (tmp.type ()));
    wide_int max = tmp.upper_bound ();
    maxi = wi::floor_log2 (max) + 1;
    r.set (build_int_cst (type, mini), build_int_cst (type, maxi));
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
    wide_int nz = lh.get_nonzero_bits ();
    wide_int pop = wi::shwi (wi::popcount (nz), prec);
    // Calculating the popcount of a singleton is trivial.
    if (lh.singleton_p ())
      {
	r.set (type, pop, pop);
	return true;
      }
    if (cfn_ffs::fold_range (r, type, lh, rh, rel))
      {
	int_range<2> tmp (type, wi::zero (prec), pop);
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
			   const irange &, relation_trio) const;
private:
  bool m_gimple_call_internal_p;
} op_cfn_clz (false), op_cfn_clz_internal (true);

bool
cfn_clz::fold_range (irange &r, tree type, const irange &lh,
		     const irange &, relation_trio) const
{
  // __builtin_c[lt]z* return [0, prec-1], except when the
  // argument is 0, but that is undefined behavior.
  //
  // For __builtin_c[lt]z* consider argument of 0 always undefined
  // behavior, for internal fns depending on C?Z_DEFINED_VALUE_AT_ZERO.
  if (lh.undefined_p ())
    return false;
  int prec = TYPE_PRECISION (lh.type ());
  int mini = 0;
  int maxi = prec - 1;
  int zerov = 0;
  scalar_int_mode mode = SCALAR_INT_TYPE_MODE (lh.type ());
  if (m_gimple_call_internal_p)
    {
      if (optab_handler (clz_optab, mode) != CODE_FOR_nothing
	  && CLZ_DEFINED_VALUE_AT_ZERO (mode, zerov) == 2)
	{
	  // Only handle the single common value.
	  if (zerov == prec)
	    maxi = prec;
	  else
	    // Magic value to give up, unless we can prove arg is non-zero.
	    mini = -2;
	}
    }

  // From clz of minimum we can compute result maximum.
  if (wi::gt_p (lh.lower_bound (), 0, TYPE_SIGN (lh.type ())))
    {
      maxi = prec - 1 - wi::floor_log2 (lh.lower_bound ());
      if (mini == -2)
	mini = 0;
    }
  else if (!range_includes_zero_p (&lh))
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
      // return [prec, prec], otherwise ignore the range.
      if (maxi == prec)
	mini = prec;
    }
  else
    mini = newmini;

  if (mini == -2)
    return false;
  r.set (build_int_cst (type, mini), build_int_cst (type, maxi));
  return true;
}

// Implement range operator for CFN_BUILT_IN_CTZ
class cfn_ctz : public range_operator
{
public:
  cfn_ctz (bool internal) { m_gimple_call_internal_p = internal; }
  using range_operator::fold_range;
  virtual bool fold_range (irange &r, tree type, const irange &lh,
			   const irange &, relation_trio) const;
private:
  bool m_gimple_call_internal_p;
} op_cfn_ctz (false), op_cfn_ctz_internal (true);

bool
cfn_ctz::fold_range (irange &r, tree type, const irange &lh,
		     const irange &, relation_trio) const
{
  if (lh.undefined_p ())
    return false;
  int prec = TYPE_PRECISION (lh.type ());
  int mini = 0;
  int maxi = prec - 1;
  int zerov = 0;
  scalar_int_mode mode = SCALAR_INT_TYPE_MODE (lh.type ());

  if (m_gimple_call_internal_p)
    {
      if (optab_handler (ctz_optab, mode) != CODE_FOR_nothing
	  && CTZ_DEFINED_VALUE_AT_ZERO (mode, zerov) == 2)
	{
	  // Handle only the two common values.
	  if (zerov == -1)
	    mini = -1;
	  else if (zerov == prec)
	    maxi = prec;
	  else
	    // Magic value to give up, unless we can prove arg is non-zero.
	    mini = -2;
	}
    }
  // If arg is non-zero, then use [0, prec - 1].
  if (!range_includes_zero_p (&lh))
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
  r.set (build_int_cst (type, mini), build_int_cst (type, maxi));
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
    r.set (build_int_cst (type, 0), build_int_cst (type, prec - 1));
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
    range_op_handler handler (m_code, type);
    gcc_checking_assert (handler);

    bool saved_flag_wrapv = flag_wrapv;
    // Pretend the arithmetic is wrapping.  If there is any overflow,
    // we'll complain, but will actually do wrapping operation.
    flag_wrapv = 1;
    bool result = handler.fold_range (r, type, lh, rh, rel);
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
  virtual bool fold_range (irange &r, tree type, const irange &,
			   const irange &, relation_trio) const
  {
    tree max = vrp_val_max (ptrdiff_type_node);
    wide_int wmax
      = wi::to_wide (max, TYPE_PRECISION (TREE_TYPE (max)));
    tree range_min = build_zero_cst (type);
    // To account for the terminating NULL, the maximum length
    // is one less than the maximum array size, which in turn
    // is one less than PTRDIFF_MAX (or SIZE_MAX where it's
    // smaller than the former type).
    // FIXME: Use max_object_size() - 1 here.
    tree range_max = wide_int_to_tree (type, wmax - 2);
    r.set (range_min, range_max);
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

    r.set (build_int_cst (type, m_is_pos ? 0 : 1),
	   size
	   ? build_int_cst (type, size - m_is_pos) : vrp_val_max (type));
    return true;
  }
private:
  bool m_is_pos;
} op_cfn_goacc_dim_size (false), op_cfn_goacc_dim_pos (true);


// Implement range operator for CFN_BUILT_IN_
class cfn_parity : public range_operator
{
public:
  using range_operator::fold_range;
  virtual bool fold_range (irange &r, tree type, const irange &,
			   const irange &, relation_trio) const
  {
    r.set (build_zero_cst (type), build_one_cst (type));
    return true;
  }
} op_cfn_parity;

// Set up a gimple_range_op_handler for any nonstandard function which can be
// supported via range-ops.

void
gimple_range_op_handler::maybe_non_standard ()
{
  range_operator *signed_op = ptr_op_widen_mult_signed;
  range_operator *unsigned_op = ptr_op_widen_mult_unsigned;
  if (gimple_code (m_stmt) == GIMPLE_ASSIGN)
    switch (gimple_assign_rhs_code (m_stmt))
      {
	case WIDEN_PLUS_EXPR:
	{
	  signed_op = ptr_op_widen_plus_signed;
	  unsigned_op = ptr_op_widen_plus_unsigned;
	}
	gcc_fallthrough ();
	case WIDEN_MULT_EXPR:
	{
	  m_valid = false;
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

	  m_valid = true;
	  if (signed2 && !signed1)
	    std::swap (m_op1, m_op2);

	  if (signed1 || signed2)
	    m_int = signed_op;
	  else
	    m_int = unsigned_op;
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
  gcc_checking_assert (type);
  if (!Value_Range::supports_type_p (type))
    return;

  switch (func)
    {
    case CFN_BUILT_IN_CONSTANT_P:
      m_op1 = gimple_call_arg (call, 0);
      m_valid = true;
      if (irange::supports_p (TREE_TYPE (m_op1)))
	m_int = &op_cfn_constant_p;
      else if (frange::supports_p (TREE_TYPE (m_op1)))
	m_float = &op_cfn_constant_float_p;
      else
	m_valid = false;
      break;

    CASE_FLT_FN (CFN_BUILT_IN_SIGNBIT):
      m_op1 = gimple_call_arg (call, 0);
      m_float = &op_cfn_signbit;
      m_valid = true;
      break;

    CASE_CFN_COPYSIGN_ALL:
      m_op1 = gimple_call_arg (call, 0);
      m_op2 = gimple_call_arg (call, 1);
      m_float = &op_cfn_copysign;
      m_valid = true;
      break;

    case CFN_BUILT_IN_TOUPPER:
    case CFN_BUILT_IN_TOLOWER:
      // Only proceed If the argument is compatible with the LHS.
      m_op1 = gimple_call_arg (call, 0);
      if (range_compatible_p (type, TREE_TYPE (m_op1)))
	{
	  m_valid = true;
	  m_int = (func == CFN_BUILT_IN_TOLOWER) ? &op_cfn_tolower
						 : &op_cfn_toupper;
	}
      break;

    CASE_CFN_FFS:
      m_op1 = gimple_call_arg (call, 0);
      m_int = &op_cfn_ffs;
      m_valid = true;
      break;

    CASE_CFN_POPCOUNT:
      m_op1 = gimple_call_arg (call, 0);
      m_int = &op_cfn_popcount;
      m_valid = true;
      break;

    CASE_CFN_CLZ:
      m_op1 = gimple_call_arg (call, 0);
      m_valid = true;
      if (gimple_call_internal_p (call))
	m_int = &op_cfn_clz_internal;
      else
	m_int = &op_cfn_clz;
      break;

    CASE_CFN_CTZ:
      m_op1 = gimple_call_arg (call, 0);
      m_valid = true;
      if (gimple_call_internal_p (call))
	m_int = &op_cfn_ctz_internal;
      else
	m_int = &op_cfn_ctz;
      break;

    CASE_CFN_CLRSB:
      m_op1 = gimple_call_arg (call, 0);
      m_valid = true;
      m_int = &op_cfn_clrsb;
      break;

    case CFN_UBSAN_CHECK_ADD:
      m_op1 = gimple_call_arg (call, 0);
      m_op2 = gimple_call_arg (call, 1);
      m_valid = true;
      m_int = &op_cfn_ubsan_add;
      break;

    case CFN_UBSAN_CHECK_SUB:
      m_op1 = gimple_call_arg (call, 0);
      m_op2 = gimple_call_arg (call, 1);
      m_valid = true;
      m_int = &op_cfn_ubsan_sub;
      break;

    case CFN_UBSAN_CHECK_MUL:
      m_op1 = gimple_call_arg (call, 0);
      m_op2 = gimple_call_arg (call, 1);
      m_valid = true;
      m_int = &op_cfn_ubsan_mul;
      break;

    case CFN_BUILT_IN_STRLEN:
      {
	tree lhs = gimple_call_lhs (call);
	if (lhs && ptrdiff_type_node && (TYPE_PRECISION (ptrdiff_type_node)
					 == TYPE_PRECISION (TREE_TYPE (lhs))))
	  {
	    m_op1 = gimple_call_arg (call, 0);
	    m_valid = true;
	    m_int = &op_cfn_strlen;
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
      m_valid = true;
      m_int = &op_cfn_goacc_dim_size;
      break;

    case CFN_GOACC_DIM_POS:
      // This call will ensure all the asserts are triggered.
      oacc_get_ifn_dim_arg (call);
      m_op1 = gimple_call_arg (call, 0);
      m_valid = true;
      m_int = &op_cfn_goacc_dim_pos;
      break;

    CASE_CFN_PARITY:
      m_valid = true;
      m_int = &op_cfn_parity;
      break;

    case CFN_BUILT_IN_EXPECT:
    case CFN_BUILT_IN_EXPECT_WITH_PROBABILITY:
      m_valid = true;
      m_op1 = gimple_call_arg (call, 0);
      m_int = &op_cfn_pass_through_arg1;
      break;

    default:
      break;
    }
}
