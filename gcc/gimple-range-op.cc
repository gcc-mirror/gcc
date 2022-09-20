/* Code for GIMPLE range op related routines.
   Copyright (C) 2019-2022 Free Software Foundation, Inc.
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
	  return;
	default:
	  gcc_unreachable ();
	  return;
      }
  // If no range-op table entry handled this stmt, check for other supported
  // statements.
  if (is_a <gcall *> (m_stmt))
    maybe_builtin_call ();
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
				   const vrange &op2_range)
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
      return op1_range (r, type, lhs_range, trange);
    }
  return op1_range (r, type, lhs_range, op2_range);
}

// Calculate what we can determine of the range of this statement's
// second operand if the lhs of the expression has the range LHS_RANGE
// and the first operand has the range OP1_RANGE.  Return false if
// nothing can be determined.

bool
gimple_range_op_handler::calc_op2 (vrange &r, const vrange &lhs_range,
				   const vrange &op1_range)
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
      return op2_range (r, type, lhs_range, trange);
    }
  return op2_range (r, type, lhs_range, op1_range);
}

// --------------------------------------------------------------------

// Implement range operator for float CFN_BUILT_IN_CONSTANT_P.
class cfn_constant_float_p : public range_operator_float
{
public:
  using range_operator_float::fold_range;
  virtual bool fold_range (irange &r, tree type, const frange &lh,
			   const irange &, relation_kind) const
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
			   const irange &, relation_kind) const
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

    default:
      break;
    }
}
