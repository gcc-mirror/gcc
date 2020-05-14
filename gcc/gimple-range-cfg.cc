/* Implementation of the gimple_ranger class.
   Copyright (C) 2017-2020 Free Software Foundation, Inc.
   Contributed by Andrew MacLeod <amacleod@redhat.com>
   and Aldy Hernandez <aldyh@redhat.com>.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "insn-codes.h"
#include "tree.h"
#include "gimple.h"
#include "ssa.h"
#include "optabs-tree.h"
#include "gimple-fold.h"
#include "tree-cfg.h"
#include "wide-int.h"
#include "gimple-range-stmt.h"
#include "gimple-range-gori.h"
#include "gimple-range-cfg.h"
#include "fold-const.h"
#include "case-cfn-macros.h"
#include "omp-general.h"

// Calculate a range for statement S and return it in R. If NAME is provided it
// represents the SSA_NAME on the LHS of the statement. It is only required
// if there is more than one lhs/output.  If a range cannot
// be calculated, return false.

bool
gimple_ranger::range_of_stmt (irange &r, gimple *s, tree name)
{
  bool res = false;
  // If name is specified, make sure it is a LHS of S.
  gcc_checking_assert (name ? SSA_NAME_DEF_STMT (name) == s : true);

  if (gimple_range_handler (s))
    res = range_of_range_op (r, s);
  else if (is_a<gphi *>(s))
    res = range_of_phi (r, as_a<gphi *> (s));
  else if (is_a<gcall *>(s))
    res = range_of_call (r, as_a<gcall *> (s));
  else if (is_a<gassign *> (s) && gimple_assign_rhs_code (s) == COND_EXPR)
    res = range_of_cond_expr (r, as_a<gassign *> (s));
  else
    {
      // If no name is specified, try the expression kind.
      if (!name)
	{
	  tree t = gimple_expr_type (s);
	  if (!irange::supports_type_p (t))
	    return false;
	  r.set_varying (t);
	  return true;
	}
      // We don't understand the stmt, so return the global range.
      r = gimple_range_global (name);
      return true;
    }
  if (res)
    {
      if (r.undefined_p ())
	return true;
      if (name && TREE_TYPE (name) != r.type ())
	range_cast (r, TREE_TYPE (name));
      return true;
    }
  return false;
}


// Calculate a range for NAME on edge E and return it in R.

void
gimple_ranger::range_on_edge (irange &r, edge e, tree name)
{
  widest_irange edge_range;
  gcc_checking_assert (irange::supports_type_p (TREE_TYPE (name)));

  // PHI arguments can be constants, catch these here.
  if (!gimple_range_ssa_p (name))
    {
      gcc_assert (range_of_expr (r, name));
      return;
    }

  range_on_exit (r, e->src, name);
  gcc_checking_assert  (r.undefined_p ()
			|| types_compatible_p (r.type(), TREE_TYPE (name)));

  // Check to see if NAME is defined on edge e.
  if (outgoing_edge_range_p (edge_range, e, name, &r))
    r = edge_range;
}

// Return the range for NAME on entry to block BB in R.
// At the statement level, this amounts to whatever the global value is.

void
gimple_ranger::range_on_entry (irange &r, basic_block bb ATTRIBUTE_UNUSED,
			       tree name)
{
  range_of_ssa_name (r, name);
}


// Return the range for NAME on exit from block BB in R.
// At the statement level, this amounts to whatever the global value is.

void
gimple_ranger::range_on_exit (irange &r, basic_block bb ATTRIBUTE_UNUSED,
			      tree name)
{
  range_of_ssa_name (r, name);
}


// Calculate a range for range_op statement S and return it in R.  If any
// If a range cannot be calculated, return false.

bool
gimple_ranger::range_of_range_op (irange &r, gimple *s)
{
  widest_irange range1, range2;
  tree type = gimple_expr_type (s);
  gcc_checking_assert (irange::supports_type_p (type));

  tree op1 = gimple_range_operand1 (s);
  tree op2 = gimple_range_operand2 (s);

  if (range_of_expr (range1, op1, s))
    {
      if (!op2)
	return gimple_range_fold (s, r, range1);

      if (range_of_expr (range2, op2, s))
	return gimple_range_fold (s, r, range1, range2);
    }
  r.set_varying (type);
  return true;
}


// Calculate a range for phi statement S and return it in R.
// If a range cannot be calculated, return false.

bool
gimple_ranger::range_of_phi (irange &r, gphi *phi)
{
  tree phi_def = gimple_phi_result (phi);
  tree type = TREE_TYPE (phi_def);
  widest_irange phi_range;
  unsigned x;

  if (!irange::supports_type_p (type))
    return false;

  // And start with an empty range, unioning in each argument's range.
  r.set_undefined ();
  for (x = 0; x < gimple_phi_num_args (phi); x++)
    {
      widest_irange arg_range;
      tree arg = gimple_phi_arg_def (phi, x);
      edge e = gimple_phi_arg_edge (phi, x);

      range_on_edge (arg_range, e, arg);
      r.union_ (arg_range);
      // Once the value reaches varying, stop looking.
      if (r.varying_p ())
	break;
    }

  return true;
}


// Calculate a range for call statement S and return it in R.
// If a range cannot be calculated, return false.

bool
gimple_ranger::range_of_call (irange &r, gcall *call)
{
  tree type = gimple_call_return_type (call);
  tree lhs = gimple_call_lhs (call);
  bool strict_overflow_p;

  if (!irange::supports_type_p (type))
    return false;

  if (range_of_builtin_call (r, call))
    ;
  else if (gimple_stmt_nonnegative_warnv_p (call, &strict_overflow_p))
    r.set (build_int_cst (type, 0), TYPE_MAX_VALUE (type));
  else if (gimple_call_nonnull_result_p (call)
	   || gimple_call_nonnull_arg (call))
    r = range_nonzero (type);
  else
    r.set_varying (type);

  // If there is a lHS, intersect that with what is known.
  if (lhs)
    {
      value_range def;
      def = gimple_range_global (lhs);
      r.intersect (def);
    }
  return true;
}


void
gimple_ranger::range_of_builtin_ubsan_call (irange &r, gcall *call,
					    tree_code code)
{
  gcc_checking_assert (code == PLUS_EXPR || code == MINUS_EXPR
		       || code == MULT_EXPR);
  tree type = gimple_call_return_type (call);
  range_operator *op = range_op_handler (code, type);
  gcc_checking_assert (op);
  widest_irange ir0, ir1;
  tree arg0 = gimple_call_arg (call, 0);
  tree arg1 = gimple_call_arg (call, 1);
  gcc_assert (range_of_expr (ir0, arg0, call));
  gcc_assert (range_of_expr (ir1, arg1, call));

  bool saved_flag_wrapv = flag_wrapv;
  /* Pretend the arithmetics is wrapping.  If there is
     any overflow, we'll complain, but will actually do
     wrapping operation.  */
  flag_wrapv = 1;
  op->fold_range (r, type, ir0, ir1);
  flag_wrapv = saved_flag_wrapv;

  /* If for both arguments vrp_valueize returned non-NULL,
     this should have been already folded and if not, it
     wasn't folded because of overflow.  Avoid removing the
     UBSAN_CHECK_* calls in that case.  */
  if (r.singleton_p ())
    r.set_varying (type);
}


bool
gimple_ranger::range_of_builtin_call (irange &r, gcall *call)
{
  combined_fn func = gimple_call_combined_fn (call);
  if (func == CFN_LAST)
    return false;

  tree type = gimple_call_return_type (call);
  tree arg;
  int mini, maxi, zerov, prec;
  scalar_int_mode mode;

  switch (func)
    {
    case CFN_BUILT_IN_CONSTANT_P:
      if (cfun->after_inlining)
	{
	  r.set_zero (type);
	  // r.equiv_clear ();
	  return true;
	}
      arg = gimple_call_arg (call, 0);
      if (range_of_expr (r, arg, call) && r.singleton_p ())
	{
	  r.set (build_one_cst (type), build_one_cst (type));
	  return true;
	}
      break;

    CASE_CFN_FFS:
    CASE_CFN_POPCOUNT:
      // __builtin_ffs* and __builtin_popcount* return [0, prec].
      arg = gimple_call_arg (call, 0);
      prec = TYPE_PRECISION (TREE_TYPE (arg));
      mini = 0;
      maxi = prec;
      gcc_assert (range_of_expr (r, arg, call));
      // If arg is non-zero, then ffs or popcount are non-zero.
      if (!range_includes_zero_p (&r))
	mini = 1;
      // If some high bits are known to be zero, decrease the maximum.
      if (!r.undefined_p ())
	{
	  wide_int max = r.upper_bound ();
	  maxi = wi::floor_log2 (max);
	}
      r.set (build_int_cst (type, mini), build_int_cst (type, maxi));
      return true;

    CASE_CFN_PARITY:
      r.set (build_zero_cst (type), build_one_cst (type));
      return true;

    CASE_CFN_CLZ:
      // __builtin_c[lt]z* return [0, prec-1], except when the
      // argument is 0, but that is undefined behavior.
      //
      // On many targets where the CLZ RTL or optab value is defined
      // for 0, the value is prec, so include that in the range by
      // default.
      arg = gimple_call_arg (call, 0);
      prec = TYPE_PRECISION (TREE_TYPE (arg));
      mini = 0;
      maxi = prec;
      mode = SCALAR_INT_TYPE_MODE (TREE_TYPE (arg));
      if (optab_handler (clz_optab, mode) != CODE_FOR_nothing
	  && CLZ_DEFINED_VALUE_AT_ZERO (mode, zerov)
	  // Only handle the single common value.
	  && zerov != prec)
	// Magic value to give up, unless we can prove arg is non-zero.
	mini = -2;

      gcc_assert (range_of_expr (r, arg, call));
      // From clz of minimum we can compute result maximum.
      if (r.constant_p ())
	{
	  maxi = prec - 1 - wi::floor_log2 (r.lower_bound ());
	  if (maxi != prec)
	    mini = 0;
	}
      else if (!range_includes_zero_p (&r))
	{
	  maxi = prec - 1;
	  mini = 0;
	}
      if (mini == -2)
	break;
      // From clz of maximum we can compute result minimum.
      if (r.constant_p ())
	{
	  mini = prec - 1 - wi::floor_log2 (r.upper_bound ());
	  if (mini == prec)
	    break;
	}
      if (mini == -2)
	break;
      r.set (build_int_cst (type, mini), build_int_cst (type, maxi));
      return true;

    CASE_CFN_CTZ:
      // __builtin_ctz* return [0, prec-1], except for when the
      // argument is 0, but that is undefined behavior.
      //
      // If there is a ctz optab for this mode and
      // CTZ_DEFINED_VALUE_AT_ZERO, include that in the range,
      // otherwise just assume 0 won't be seen.
      arg = gimple_call_arg (call, 0);
      prec = TYPE_PRECISION (TREE_TYPE (arg));
      mini = 0;
      maxi = prec - 1;
      mode = SCALAR_INT_TYPE_MODE (TREE_TYPE (arg));
      if (optab_handler (ctz_optab, mode) != CODE_FOR_nothing
	  && CTZ_DEFINED_VALUE_AT_ZERO (mode, zerov))
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
      gcc_assert (range_of_expr (r, arg, call));
      if (!r.undefined_p ())
	{
	  if (r.lower_bound () != 0)
	    {
	      mini = 0;
	      maxi = prec - 1;
	    }
	  // If some high bits are known to be zero, we can decrease
	  // the maximum.
	  wide_int max = r.upper_bound ();
	  if (max == 0)
	    break;
	  maxi = wi::floor_log2 (max);
	}
      if (mini == -2)
	break;
      r.set (build_int_cst (type, mini), build_int_cst (type, maxi));
      return true;

    CASE_CFN_CLRSB:
      arg = gimple_call_arg (call, 0);
      prec = TYPE_PRECISION (TREE_TYPE (arg));
      r.set (build_int_cst (type, 0), build_int_cst (type, prec - 1));
      return true;
    case CFN_UBSAN_CHECK_ADD:
      range_of_builtin_ubsan_call (r, call, PLUS_EXPR);
      return true;
    case CFN_UBSAN_CHECK_SUB:
      range_of_builtin_ubsan_call (r, call, MINUS_EXPR);
      return true;
    case CFN_UBSAN_CHECK_MUL:
      range_of_builtin_ubsan_call (r, call, MULT_EXPR);
      return true;

    case CFN_GOACC_DIM_SIZE:
    case CFN_GOACC_DIM_POS:
      // Optimizing these two internal functions helps the loop
      // optimizer eliminate outer comparisons.  Size is [1,N]
      // and pos is [0,N-1].
      {
	bool is_pos = func == CFN_GOACC_DIM_POS;
	int axis = oacc_get_ifn_dim_arg (call);
	int size = oacc_get_fn_dim_size (current_function_decl, axis);
	if (!size)
	  // If it's dynamic, the backend might know a hardware limitation.
	  size = targetm.goacc.dim_limit (axis);

	r.set (build_int_cst (type, is_pos ? 0 : 1),
	       size
	       ? build_int_cst (type, size - is_pos) : vrp_val_max (type));
	return true;
      }

    case CFN_BUILT_IN_STRLEN:
      if (tree lhs = gimple_call_lhs (call))
	if (ptrdiff_type_node
	    && (TYPE_PRECISION (ptrdiff_type_node)
		== TYPE_PRECISION (TREE_TYPE (lhs))))
	  {
	    tree type = TREE_TYPE (lhs);
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
      break;
    default:
      break;
    }
  return false;
}
