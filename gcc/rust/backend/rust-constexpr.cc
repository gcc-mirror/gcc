// This file is part of GCC.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include "rust-constexpr.h"
#include "rust-location.h"
#include "rust-diagnostics.h"
#include "rust-tree.h"

#include "fold-const.h"
#include "realmpfr.h"
#include "convert.h"
#include "print-tree.h"
#include "gimplify.h"
#include "tree-iterator.h"

namespace Rust {
namespace Compile {

static tree
constant_value_1 (tree decl, bool strict_p, bool return_aggregate_cst_ok_p,
		  bool unshare_p);
tree
decl_constant_value (tree decl, bool unshare_p);

static void
non_const_var_error (location_t loc, tree r);

static tree
get_function_named_in_call (tree t);

ConstCtx::ConstCtx () : constexpr_ops_count (0) {}

tree
ConstCtx::fold (tree expr)
{
  tree folded = ConstCtx ().constexpr_expression (expr);
  rust_assert (folded != NULL_TREE);
  return folded;
}

tree
ConstCtx::constexpr_expression (tree t)
{
  location_t loc = EXPR_LOCATION (t);
  if (CONSTANT_CLASS_P (t))
    {
      if (TREE_OVERFLOW (t))
	{
	  rust_error_at (Location (loc), "overflow in constant expression");
	  return t;
	}

      if (TREE_CODE (t) == INTEGER_CST && TYPE_PTR_P (TREE_TYPE (t))
	  && !integer_zerop (t))
	{
	  // FIXME check does this actually work to print out tree types
	  rust_error_at (Location (loc),
			 "value %qE of type %qT is not a constant expression",
			 t, TREE_TYPE (t));
	  return t;
	}

      return t;
    }

  // Avoid excessively long constexpr evaluations
  if (++constexpr_ops_count >= constexpr_ops_limit)
    {
      rust_error_at (
	Location (loc),
	"%<constexpr%> evaluation operation count exceeds limit of "
	"%wd (use %<-fconstexpr-ops-limit=%> to increase the limit)",
	constexpr_ops_limit);

      return t;
    }

  tree r = t;
  tree_code tcode = TREE_CODE (t);
  switch (tcode)
    {
      case CONST_DECL: {
	r = decl_constant_value (t, /*unshare_p=*/false);
	if (TREE_CODE (r) == TARGET_EXPR
	    && TREE_CODE (TARGET_EXPR_INITIAL (r)) == CONSTRUCTOR)
	  r = TARGET_EXPR_INITIAL (r);
	if (DECL_P (r))
	  {
	    non_const_var_error (loc, r);
	    return r;
	  }
      }
      break;

    case POINTER_PLUS_EXPR:
    case POINTER_DIFF_EXPR:
    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case TRUNC_MOD_EXPR:
    case CEIL_MOD_EXPR:
    case ROUND_MOD_EXPR:
    case RDIV_EXPR:
    case EXACT_DIV_EXPR:
    case MIN_EXPR:
    case MAX_EXPR:
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case BIT_AND_EXPR:
    case TRUTH_XOR_EXPR:
    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
    case SPACESHIP_EXPR:
    case UNORDERED_EXPR:
    case ORDERED_EXPR:
    case UNLT_EXPR:
    case UNLE_EXPR:
    case UNGT_EXPR:
    case UNGE_EXPR:
    case UNEQ_EXPR:
    case LTGT_EXPR:
    case RANGE_EXPR:
    case COMPLEX_EXPR:
      r = eval_binary_expression (t);
      break;

    case CALL_EXPR:
      r = eval_call_expression (t);
      break;

    case RETURN_EXPR:
      rust_assert (TREE_OPERAND (t, 0) != NULL_TREE);
      r = constexpr_expression (TREE_OPERAND (t, 0));
      break;

    case MODIFY_EXPR:
      r = eval_store_expression (t);
      break;

    default:
      break;
    }

  return r;
}

tree
ConstCtx::eval_store_expression (tree t)
{
  tree init = TREE_OPERAND (t, 1);
  if (TREE_CLOBBER_P (init))
    /* Just ignore clobbers.  */
    return void_node;

  /* First we figure out where we're storing to.  */
  tree target = TREE_OPERAND (t, 0);

  tree type = TREE_TYPE (target);
  bool preeval = SCALAR_TYPE_P (type) || TREE_CODE (t) == MODIFY_EXPR;
  if (preeval)
    {
      /* Evaluate the value to be stored without knowing what object it will be
	 stored in, so that any side-effects happen first.  */
      init = ConstCtx::fold (init);
    }

  bool evaluated = false;
  tree object = NULL_TREE;
  for (tree probe = target; object == NULL_TREE;)
    {
      switch (TREE_CODE (probe))
	{
	default:
	  if (evaluated)
	    object = probe;
	  else
	    {
	      probe = constexpr_expression (probe);
	      evaluated = true;
	    }
	  break;
	}
    }

  return init;
}

/* Subroutine of cxx_eval_constant_expression.
 Like cxx_eval_unary_expression, except for binary expressions.  */

tree
ConstCtx::eval_binary_expression (tree t)
{
  tree orig_lhs = TREE_OPERAND (t, 0);
  tree orig_rhs = TREE_OPERAND (t, 1);
  tree lhs, rhs;

  lhs = constexpr_expression (orig_lhs);
  rhs = constexpr_expression (orig_rhs);

  location_t loc = EXPR_LOCATION (t);
  enum tree_code code = TREE_CODE (t);
  tree type = TREE_TYPE (t);

  return fold_binary_loc (loc, code, type, lhs, rhs);
}

// Subroutine of cxx_eval_constant_expression.
// Evaluate the call expression tree T in the context of OLD_CALL expression
// evaluation.
tree
ConstCtx::eval_call_expression (tree t)
{
  tree fun = get_function_named_in_call (t);
  return constexpr_fn_retval (DECL_SAVED_TREE (fun));
}

// Subroutine of check_constexpr_fundef.  BODY is the body of a function
// declared to be constexpr, or a sub-statement thereof.  Returns the
// return value if suitable, error_mark_node for a statement not allowed in
// a constexpr function, or NULL_TREE if no return value was found.
tree
ConstCtx::constexpr_fn_retval (tree body)
{
  switch (TREE_CODE (body))
    {
      case STATEMENT_LIST: {
	tree expr = NULL_TREE;
	for (tree stmt : tsi_range (body))
	  {
	    tree s = constexpr_fn_retval (stmt);
	    if (s == error_mark_node)
	      return error_mark_node;
	    else if (s == NULL_TREE)
	      /* Keep iterating.  */;
	    else if (expr)
	      /* Multiple return statements.  */
	      return error_mark_node;
	    else
	      expr = s;
	  }
	return expr;
      }

    case RETURN_EXPR:
      return constexpr_expression (body);

      case DECL_EXPR: {
	tree decl = DECL_EXPR_DECL (body);
	if (TREE_CODE (decl) == USING_DECL
	    /* Accept __func__, __FUNCTION__, and __PRETTY_FUNCTION__.  */
	    || DECL_ARTIFICIAL (decl))
	  return NULL_TREE;
	return error_mark_node;
      }

    case CLEANUP_POINT_EXPR:
      return constexpr_fn_retval (TREE_OPERAND (body, 0));

      case BIND_EXPR: {
	tree b = BIND_EXPR_BODY (body);
	return constexpr_fn_retval (b);
      }
      break;

    default:
      return error_mark_node;
    }
  return error_mark_node;
}

// Taken from cp/constexpr.cc
//
// If DECL is a scalar enumeration constant or variable with a
// constant initializer, return the initializer (or, its initializers,
// recursively); otherwise, return DECL.  If STRICT_P, the
// initializer is only returned if DECL is a
// constant-expression.  If RETURN_AGGREGATE_CST_OK_P, it is ok to
// return an aggregate constant.  If UNSHARE_P, return an unshared
// copy of the initializer.
static tree
constant_value_1 (tree decl, bool strict_p, bool return_aggregate_cst_ok_p,
		  bool unshare_p)
{
  while (TREE_CODE (decl) == CONST_DECL)
    {
      tree init;
      /* If DECL is a static data member in a template
	 specialization, we must instantiate it here.  The
	 initializer for the static data member is not processed
	 until needed; we need it now.  */

      init = DECL_INITIAL (decl);
      if (init == error_mark_node)
	{
	  if (TREE_CODE (decl) == CONST_DECL)
	    /* Treat the error as a constant to avoid cascading errors on
	       excessively recursive template instantiation (c++/9335).  */
	    return init;
	  else
	    return decl;
	}

      decl = init;
    }
  return unshare_p ? unshare_expr (decl) : decl;
}

// A more relaxed version of decl_really_constant_value, used by the
// common C/C++ code.
tree
decl_constant_value (tree decl, bool unshare_p)
{
  return constant_value_1 (decl, /*strict_p=*/false,
			   /*return_aggregate_cst_ok_p=*/true,
			   /*unshare_p=*/unshare_p);
}

static void
non_const_var_error (location_t loc, tree r)
{
  error_at (loc,
	    "the value of %qD is not usable in a constant "
	    "expression",
	    r);
  /* Avoid error cascade.  */
  if (DECL_INITIAL (r) == error_mark_node)
    return;

  // more in cp/constexpr.cc
}

static tree
get_callee (tree call)
{
  if (call == NULL_TREE)
    return call;
  else if (TREE_CODE (call) == CALL_EXPR)
    return CALL_EXPR_FN (call);

  return NULL_TREE;
}

// We have an expression tree T that represents a call, either CALL_EXPR
// or AGGR_INIT_EXPR. If the call is lexically to a named function,
// return the _DECL for that function.
static tree
get_function_named_in_call (tree t)
{
  tree fun = get_callee (t);
  if (fun && TREE_CODE (fun) == ADDR_EXPR
      && TREE_CODE (TREE_OPERAND (fun, 0)) == FUNCTION_DECL)
    fun = TREE_OPERAND (fun, 0);
  return fun;
}

} // namespace Compile
} // namespace Rust
