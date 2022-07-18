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
#include "timevar.h"
#include "varasm.h"
#include "cgraph.h"

#define VERIFY_CONSTANT(X)                                                     \
  do                                                                           \
    {                                                                          \
      if (verify_constant ((X), ctx->quiet, non_constant_p, overflow_p))       \
	return t;                                                              \
    }                                                                          \
  while (0)

namespace Rust {
namespace Compile {

static bool
verify_constant (tree, bool, bool *, bool *);

static HOST_WIDE_INT
find_array_ctor_elt (tree ary, tree dindex, bool insert = false);
static int
array_index_cmp (tree key, tree index);

struct constexpr_global_ctx
{
  HOST_WIDE_INT constexpr_ops_count;

  /* Cleanups that need to be evaluated at the end of CLEANUP_POINT_EXPR.  */
  vec<tree> *cleanups;
  /* Heap VAR_DECLs created during the evaluation of the outermost constant
     expression.  */
  auto_vec<tree, 16> heap_vars;
  constexpr_global_ctx () : constexpr_ops_count (0) {}
};

struct constexpr_ctx
{
  /* The part of the context that needs to be unique to the whole
   cxx_eval_outermost_constant_expr invocation.  */
  constexpr_global_ctx *global;

  /* Whether we should error on a non-constant expression or fail quietly.
    This flag needs to be here, but some of the others could move to global
    if they get larger than a word.  */
  bool quiet;
  /* The object we're building the CONSTRUCTOR for.  */
  tree object;
  /* The CONSTRUCTOR we're currently building up for an aggregate
     initializer.  */
  tree ctor;
};

static tree
constant_value_1 (tree decl, bool strict_p, bool return_aggregate_cst_ok_p,
		  bool unshare_p);
tree
decl_constant_value (tree decl, bool unshare_p);

static void
non_const_var_error (location_t loc, tree r);

static tree
constexpr_expression (const constexpr_ctx *ctx, tree, bool, bool *, bool *,
		      tree * = NULL);

static tree
constexpr_fn_retval (const constexpr_ctx *ctx, tree r);

static tree
eval_store_expression (const constexpr_ctx *ctx, tree r, bool, bool *, bool *);

static tree
eval_call_expression (const constexpr_ctx *ctx, tree r);

static tree
eval_binary_expression (const constexpr_ctx *ctx, tree r, bool, bool *, bool *);

static tree
get_function_named_in_call (tree t);

tree
fold_expr (tree expr)
{
  constexpr_global_ctx global_ctx;
  constexpr_ctx ctx = {&global_ctx, false};
  bool non_constant_p = false;
  bool overflow_p = false;

  tree folded
    = constexpr_expression (&ctx, expr, false, &non_constant_p, &overflow_p);
  rust_assert (folded != NULL_TREE);
  return folded;
}

static tree
constexpr_expression (const constexpr_ctx *ctx, tree t, bool lval,
		      bool *non_constant_p, bool *overflow_p,
		      tree *jump_target /* = NULL */)
{
  location_t loc = EXPR_LOCATION (t);

  if (CONSTANT_CLASS_P (t))
    {
      if (TREE_OVERFLOW (t))
	{
	  error_at (loc, "overflow in constant expression");
	  return t;
	}

      return t;
    }

  // Avoid excessively long constexpr evaluations
  if (++ctx->global->constexpr_ops_count >= constexpr_ops_limit)
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
      r = eval_binary_expression (ctx, t, false, non_constant_p, overflow_p);
      break;

    case CALL_EXPR:
      r = eval_call_expression (ctx, t);
      break;

    case RETURN_EXPR:
      rust_assert (TREE_OPERAND (t, 0) != NULL_TREE);
      r = constexpr_expression (ctx, TREE_OPERAND (t, 0), false, non_constant_p,
				overflow_p);
      break;

    case MODIFY_EXPR:
      r = eval_store_expression (ctx, t, false, non_constant_p, overflow_p);
      break;

    default:
      break;
    }

  return r;
}

static tree
eval_store_expression (const constexpr_ctx *ctx, tree t, bool lval,
		       bool *non_constant_p, bool *overflow_p)
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
      init = fold_expr (init);
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
	      probe = constexpr_expression (ctx, probe, lval, non_constant_p,
					    overflow_p);
	      evaluated = true;
	    }
	  break;
	}
    }

  return init;
}

/* Subroutine of cxx_eval_constant_expression.
 Like cxx_eval_unary_expression, except for binary expressions.  */
static tree
eval_binary_expression (const constexpr_ctx *ctx, tree t, bool lval,
			bool *non_constant_p, bool *overflow_p)
{
  tree orig_lhs = TREE_OPERAND (t, 0);
  tree orig_rhs = TREE_OPERAND (t, 1);
  tree lhs, rhs;

  lhs = constexpr_expression (ctx, orig_lhs, lval, non_constant_p, overflow_p);
  rhs = constexpr_expression (ctx, orig_rhs, lval, non_constant_p, overflow_p);

  location_t loc = EXPR_LOCATION (t);
  enum tree_code code = TREE_CODE (t);
  tree type = TREE_TYPE (t);

  return fold_binary_loc (loc, code, type, lhs, rhs);
}

// Subroutine of cxx_eval_constant_expression.
// Evaluate the call expression tree T in the context of OLD_CALL expression
// evaluation.
static tree
eval_call_expression (const constexpr_ctx *ctx, tree t)
{
  tree fun = get_function_named_in_call (t);
  return constexpr_fn_retval (ctx, DECL_SAVED_TREE (fun));
}

// Subroutine of check_constexpr_fundef.  BODY is the body of a function
// declared to be constexpr, or a sub-statement thereof.  Returns the
// return value if suitable, error_mark_node for a statement not allowed in
// a constexpr function, or NULL_TREE if no return value was found.
static tree
constexpr_fn_retval (const constexpr_ctx *ctx, tree body)
{
  switch (TREE_CODE (body))
    {
      case STATEMENT_LIST: {
	tree expr = NULL_TREE;
	for (tree stmt : tsi_range (body))
	  {
	    tree s = constexpr_fn_retval (ctx, stmt);
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

      case RETURN_EXPR: {
	bool non_constant_p = false;
	bool overflow_p = false;
	return constexpr_expression (ctx, body, false, &non_constant_p,
				     &overflow_p);
      }
      case DECL_EXPR: {
	tree decl = DECL_EXPR_DECL (body);
	if (TREE_CODE (decl) == USING_DECL
	    /* Accept __func__, __FUNCTION__, and __PRETTY_FUNCTION__.  */
	    || DECL_ARTIFICIAL (decl))
	  return NULL_TREE;
	return error_mark_node;
      }

    case CLEANUP_POINT_EXPR:
      return constexpr_fn_retval (ctx, TREE_OPERAND (body, 0));

      case BIND_EXPR: {
	tree b = BIND_EXPR_BODY (body);
	return constexpr_fn_retval (ctx, b);
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

// forked from gcc/cp/constexpr.cc maybe_constexpr_fn

/* True if a function might be declared constexpr  */

bool
maybe_constexpr_fn (tree t)
{
  return (DECL_DECLARED_CONSTEXPR_P (t));
}

// forked from gcc/cp/constexpr.cc get_nth_callarg

/* We have an expression tree T that represents a call, either CALL_EXPR.
  Return the Nth argument.  */

inline tree
get_nth_callarg (tree t, int n)
{
  switch (TREE_CODE (t))
    {
    case CALL_EXPR:
      return CALL_EXPR_ARG (t, n);

    default:
      gcc_unreachable ();
      return NULL;
    }
}

// forked from gcc/cp/constexpr.cc var_in_maybe_constexpr_fn

/* True if T was declared in a function that might be constexpr: either a
   function that was declared constexpr.  */

bool
var_in_maybe_constexpr_fn (tree t)
{
  return (DECL_FUNCTION_SCOPE_P (t) && maybe_constexpr_fn (DECL_CONTEXT (t)));
}

// forked from gcc/cp/constexpr.cc array_index_cmp

/* Some of the expressions fed to the constexpr mechanism are calls to
   constructors, which have type void.  In that case, return the type being
   initialized by the constructor.  */

static tree
initialized_type (tree t)
{
  if (TYPE_P (t))
    return t;
  tree type = TREE_TYPE (t);
  if (TREE_CODE (t) == CALL_EXPR)
    {
      /* A constructor call has void type, so we need to look deeper.  */
      tree fn = get_function_named_in_call (t);
      if (fn && TREE_CODE (fn) == FUNCTION_DECL && DECL_CXX_CONSTRUCTOR_P (fn))
	type = DECL_CONTEXT (fn);
    }
  else if (TREE_CODE (t) == COMPOUND_EXPR)
    return initialized_type (TREE_OPERAND (t, 1));

  return cv_unqualified (type);
}

/* P0859: A function is needed for constant evaluation if it is a constexpr
   function that is named by an expression ([basic.def.odr]) that is
   potentially constant evaluated.

   So we need to instantiate any constexpr functions mentioned by the
   expression even if the definition isn't needed for evaluating the
   expression.  */

static tree
instantiate_cx_fn_r (tree *tp, int *walk_subtrees, void * /*data*/)
{
  if (TREE_CODE (*tp) == CALL_EXPR)
    {
      if (EXPR_HAS_LOCATION (*tp))
	input_location = EXPR_LOCATION (*tp);
    }

  if (!EXPR_P (*tp))
    *walk_subtrees = 0;

  return NULL_TREE;
}

static void
instantiate_constexpr_fns (tree t)
{
  location_t loc = input_location;
  rs_walk_tree_without_duplicates (&t, instantiate_cx_fn_r, NULL);
  input_location = loc;
}

/* Returns less than, equal to, or greater than zero if KEY is found to be
   less than, to match, or to be greater than the constructor_elt's INDEX.  */

static int
array_index_cmp (tree key, tree index)
{
  gcc_assert (TREE_CODE (key) == INTEGER_CST);

  switch (TREE_CODE (index))
    {
    case INTEGER_CST:
      return tree_int_cst_compare (key, index);
      case RANGE_EXPR: {
	tree lo = TREE_OPERAND (index, 0);
	tree hi = TREE_OPERAND (index, 1);
	if (tree_int_cst_lt (key, lo))
	  return -1;
	else if (tree_int_cst_lt (hi, key))
	  return 1;
	else
	  return 0;
      }
    default:
      gcc_unreachable ();
    }
}

/* If T is a CONSTRUCTOR, return an unshared copy of T and any
   sub-CONSTRUCTORs.  Otherwise return T.

   We use this whenever we initialize an object as a whole, whether it's a
   parameter, a local variable, or a subobject, so that subsequent
   modifications don't affect other places where it was used.  */

tree
unshare_constructor (tree t MEM_STAT_DECL)
{
  if (!t || TREE_CODE (t) != CONSTRUCTOR)
    return t;
  auto_vec<tree *, 4> ptrs;
  ptrs.safe_push (&t);
  while (!ptrs.is_empty ())
    {
      tree *p = ptrs.pop ();
      tree n = copy_node (*p PASS_MEM_STAT);
      CONSTRUCTOR_ELTS (n)
	= vec_safe_copy (CONSTRUCTOR_ELTS (*p) PASS_MEM_STAT);
      *p = n;
      vec<constructor_elt, va_gc> *v = CONSTRUCTOR_ELTS (n);
      constructor_elt *ce;
      for (HOST_WIDE_INT i = 0; vec_safe_iterate (v, i, &ce); ++i)
	if (ce->value && TREE_CODE (ce->value) == CONSTRUCTOR)
	  ptrs.safe_push (&ce->value);
    }
  return t;
}

/* Returns the index of the constructor_elt of ARY which matches DINDEX, or -1
   if none.  If INSERT is true, insert a matching element rather than fail.  */

static HOST_WIDE_INT
find_array_ctor_elt (tree ary, tree dindex, bool insert)
{
  if (tree_int_cst_sgn (dindex) < 0)
    return -1;

  unsigned HOST_WIDE_INT i = tree_to_uhwi (dindex);
  vec<constructor_elt, va_gc> *elts = CONSTRUCTOR_ELTS (ary);
  unsigned HOST_WIDE_INT len = vec_safe_length (elts);

  unsigned HOST_WIDE_INT end = len;
  unsigned HOST_WIDE_INT begin = 0;

  /* If the last element of the CONSTRUCTOR has its own index, we can assume
     that the same is true of the other elements and index directly.  */
  if (end > 0)
    {
      tree cindex = (*elts)[end - 1].index;
      if (cindex == NULL_TREE)
	{
	  /* Verify that if the last index is missing, all indexes
	     are missing.  */
	  if (flag_checking)
	    for (unsigned int j = 0; j < len - 1; ++j)
	      gcc_assert ((*elts)[j].index == NULL_TREE);
	  if (i < end)
	    return i;
	  else
	    {
	      begin = end;
	      if (i == end)
		/* If the element is to be added right at the end,
		   make sure it is added with cleared index too.  */
		dindex = NULL_TREE;
	      else if (insert)
		/* Otherwise, in order not to break the assumption
		   that CONSTRUCTOR either has all indexes or none,
		   we need to add indexes to all elements.  */
		for (unsigned int j = 0; j < len; ++j)
		  (*elts)[j].index = build_int_cst (TREE_TYPE (dindex), j);
	    }
	}
      else if (TREE_CODE (cindex) == INTEGER_CST
	       && compare_tree_int (cindex, end - 1) == 0)
	{
	  if (i < end)
	    return i;
	  else
	    begin = end;
	}
    }

  /* Otherwise, find a matching index by means of a binary search.  */
  while (begin != end)
    {
      unsigned HOST_WIDE_INT middle = (begin + end) / 2;
      constructor_elt &elt = (*elts)[middle];
      tree idx = elt.index;

      int cmp = array_index_cmp (dindex, idx);
      if (cmp < 0)
	end = middle;
      else if (cmp > 0)
	begin = middle + 1;
      else
	{
	  if (insert && TREE_CODE (idx) == RANGE_EXPR)
	    {
	      /* We need to split the range.  */
	      constructor_elt e;
	      tree lo = TREE_OPERAND (idx, 0);
	      tree hi = TREE_OPERAND (idx, 1);
	      tree value = elt.value;
	      dindex = fold_convert (sizetype, dindex);
	      if (tree_int_cst_lt (lo, dindex))
		{
		  /* There are still some lower elts; shorten the range.  */
		  tree new_hi
		    = int_const_binop (MINUS_EXPR, dindex, size_one_node);
		  if (tree_int_cst_equal (lo, new_hi))
		    /* Only one element left, no longer a range.  */
		    elt.index = lo;
		  else
		    TREE_OPERAND (idx, 1) = new_hi;
		  /* Append the element we want to insert.  */
		  ++middle;
		  e.index = dindex;
		  e.value = unshare_constructor (value);
		  vec_safe_insert (CONSTRUCTOR_ELTS (ary), middle, e);
		}
	      else
		/* No lower elts, the range elt is now ours.  */
		elt.index = dindex;

	      if (tree_int_cst_lt (dindex, hi))
		{
		  /* There are still some higher elts; append a range.  */
		  tree new_lo
		    = int_const_binop (PLUS_EXPR, dindex, size_one_node);
		  if (tree_int_cst_equal (new_lo, hi))
		    e.index = hi;
		  else
		    e.index = build2 (RANGE_EXPR, sizetype, new_lo, hi);
		  e.value = unshare_constructor (value);
		  vec_safe_insert (CONSTRUCTOR_ELTS (ary), middle + 1, e);
		}
	    }
	  return middle;
	}
    }

  if (insert)
    {
      constructor_elt e = {dindex, NULL_TREE};
      vec_safe_insert (CONSTRUCTOR_ELTS (ary), end, e);
      return end;
    }

  return -1;
}

/* Return true if T is a valid constant initializer.  If a CONSTRUCTOR
   initializes all the members, the CONSTRUCTOR_NO_CLEARING flag will be
   cleared.
   FIXME speed this up, it's taking 16% of compile time on sieve testcase.  */

bool
reduced_constant_expression_p (tree t)
{
  if (t == NULL_TREE)
    return false;

  switch (TREE_CODE (t))
    {
    case PTRMEM_CST:
      /* Even if we can't lower this yet, it's constant.  */
      return true;

    case CONSTRUCTOR:
      /* And we need to handle PTRMEM_CST wrapped in a CONSTRUCTOR.  */
      tree field;
      if (CONSTRUCTOR_NO_CLEARING (t))
	{
	  if (TREE_CODE (TREE_TYPE (t)) == VECTOR_TYPE)
	    /* An initialized vector would have a VECTOR_CST.  */
	    return false;
	  else if (TREE_CODE (TREE_TYPE (t)) == ARRAY_TYPE)
	    {
	      /* There must be a valid constant initializer at every array
		 index.  */
	      tree min = TYPE_MIN_VALUE (TYPE_DOMAIN (TREE_TYPE (t)));
	      tree max = TYPE_MAX_VALUE (TYPE_DOMAIN (TREE_TYPE (t)));
	      tree cursor = min;
	      for (auto &e : CONSTRUCTOR_ELTS (t))
		{
		  if (!reduced_constant_expression_p (e.value))
		    return false;
		  if (array_index_cmp (cursor, e.index) != 0)
		    return false;
		  if (TREE_CODE (e.index) == RANGE_EXPR)
		    cursor = TREE_OPERAND (e.index, 1);
		  cursor = int_const_binop (PLUS_EXPR, cursor, size_one_node);
		}
	      if (find_array_ctor_elt (t, max) == -1)
		return false;
	      goto ok;
	    }
	  else if (TREE_CODE (TREE_TYPE (t)) == UNION_TYPE)
	    {
	      if (CONSTRUCTOR_NELTS (t) == 0)
		/* An initialized union has a constructor element.  */
		return false;
	      /* And it only initializes one member.  */
	      field = NULL_TREE;
	    }
	  else
	    field = next_initializable_field (TYPE_FIELDS (TREE_TYPE (t)));
	}
      else
	field = NULL_TREE;
      for (auto &e : CONSTRUCTOR_ELTS (t))
	{
	  /* If VAL is null, we're in the middle of initializing this
	     element.  */
	  if (!reduced_constant_expression_p (e.value))
	    return false;
	  /* Empty class field may or may not have an initializer.  */
	  for (; field && e.index != field;
	       field = next_initializable_field (DECL_CHAIN (field)))
	    if (!is_really_empty_class (TREE_TYPE (field),
					/*ignore_vptr*/ false))
	      return false;
	  if (field)
	    field = next_initializable_field (DECL_CHAIN (field));
	}
      /* There could be a non-empty field at the end.  */
      for (; field; field = next_initializable_field (DECL_CHAIN (field)))
	if (!is_really_empty_class (TREE_TYPE (field), /*ignore_vptr*/ false))
	  return false;
    ok:
      if (CONSTRUCTOR_NO_CLEARING (t))
	/* All the fields are initialized.  */
	CONSTRUCTOR_NO_CLEARING (t) = false;
      return true;

    default:
      /* FIXME are we calling this too much?  */
      return initializer_constant_valid_p (t, TREE_TYPE (t)) != NULL_TREE;
    }
}

/* Some expressions may have constant operands but are not constant
   themselves, such as 1/0.  Call this function to check for that
   condition.

   We only call this in places that require an arithmetic constant, not in
   places where we might have a non-constant expression that can be a
   component of a constant expression, such as the address of a constexpr
   variable that might be dereferenced later.  */

static bool
verify_constant (tree t, bool allow_non_constant, bool *non_constant_p,
		 bool *overflow_p)
{
  if (!*non_constant_p && !reduced_constant_expression_p (t) && t != void_node)
    {
      if (!allow_non_constant)
	error ("%q+E is not a constant expression", t);
      *non_constant_p = true;
    }
  if (TREE_OVERFLOW_P (t))
    {
      if (!allow_non_constant)
	{
	  permerror (input_location, "overflow in constant expression");
	  /* If we're being permissive (and are in an enforcing
	     context), ignore the overflow.  */
	  if (flag_permissive)
	    return *non_constant_p;
	}
      *overflow_p = true;
    }
  return *non_constant_p;
}

// forked from gcc/cp/constexpr.cc find_heap_var_refs

/* Look for heap variables in the expression *TP.  */

static tree
find_heap_var_refs (tree *tp, int *walk_subtrees, void * /*data*/)
{
  if (VAR_P (*tp)
      && (DECL_NAME (*tp) == heap_uninit_identifier
	  || DECL_NAME (*tp) == heap_identifier
	  || DECL_NAME (*tp) == heap_vec_uninit_identifier
	  || DECL_NAME (*tp) == heap_vec_identifier
	  || DECL_NAME (*tp) == heap_deleted_identifier))
    return *tp;

  if (TYPE_P (*tp))
    *walk_subtrees = 0;
  return NULL_TREE;
}

// forked from gcc/cp/constexpr.cc find_immediate_fndecl

/* Find immediate function decls in *TP if any.  */

static tree
find_immediate_fndecl (tree *tp, int * /*walk_subtrees*/, void * /*data*/)
{
  if (TREE_CODE (*tp) == FUNCTION_DECL && DECL_IMMEDIATE_FUNCTION_P (*tp))
    return *tp;
  if (TREE_CODE (*tp) == PTRMEM_CST
      && TREE_CODE (PTRMEM_CST_MEMBER (*tp)) == FUNCTION_DECL
      && DECL_IMMEDIATE_FUNCTION_P (PTRMEM_CST_MEMBER (*tp)))
    return PTRMEM_CST_MEMBER (*tp);
  return NULL_TREE;
}

// forked in gcc/cp/constexpr.cc diag_array_subscript

/* Under the control of CTX, issue a detailed diagnostic for
   an out-of-bounds subscript INDEX into the expression ARRAY.  */

static void
diag_array_subscript (location_t loc, const constexpr_ctx *ctx, tree array,
		      tree index)
{
  if (!ctx->quiet)
    {
      tree arraytype = TREE_TYPE (array);

      /* Convert the unsigned array subscript to a signed integer to avoid
	 printing huge numbers for small negative values.  */
      tree sidx = fold_convert (ssizetype, index);
      STRIP_ANY_LOCATION_WRAPPER (array);
      if (DECL_P (array))
	{
	  if (TYPE_DOMAIN (arraytype))
	    error_at (loc,
		      "array subscript value %qE is outside the bounds "
		      "of array %qD of type %qT",
		      sidx, array, arraytype);
	  else
	    error_at (loc,
		      "nonzero array subscript %qE is used with array %qD of "
		      "type %qT with unknown bounds",
		      sidx, array, arraytype);
	  inform (DECL_SOURCE_LOCATION (array), "declared here");
	}
      else if (TYPE_DOMAIN (arraytype))
	error_at (loc,
		  "array subscript value %qE is outside the bounds "
		  "of array type %qT",
		  sidx, arraytype);
      else
	error_at (loc,
		  "nonzero array subscript %qE is used with array of type %qT "
		  "with unknown bounds",
		  sidx, arraytype);
    }
}

// forked from gcc/cp/constexpr.cc get_array_or_vector_nelts

/* Return the number of elements for TYPE (which is an ARRAY_TYPE or
   a VECTOR_TYPE).  */

static tree
get_array_or_vector_nelts (const constexpr_ctx *ctx, tree type,
			   bool *non_constant_p, bool *overflow_p)
{
  tree nelts;
  if (TREE_CODE (type) == ARRAY_TYPE)
    {
      if (TYPE_DOMAIN (type))
	nelts = array_type_nelts_top (type);
      else
	nelts = size_zero_node;
    }
  else if (VECTOR_TYPE_P (type))
    nelts = size_int (TYPE_VECTOR_SUBPARTS (type));
  else
    gcc_unreachable ();

  /* For VLAs, the number of elements won't be an integer constant.  */
  nelts = constexpr_expression (ctx, nelts, false, non_constant_p, overflow_p);
  return nelts;
}

// forked from gcc/cp/constexpr.cc eval_and_check_array_index

/* Subroutine of cxx_eval_array_reference.  T is an ARRAY_REF; evaluate the
   subscript, diagnose any problems with it, and return the result.  */

static tree
eval_and_check_array_index (const constexpr_ctx *ctx, tree t,
			    bool allow_one_past, bool *non_constant_p,
			    bool *overflow_p)
{
  location_t loc = rs_expr_loc_or_input_loc (t);
  tree ary = TREE_OPERAND (t, 0);
  t = TREE_OPERAND (t, 1);
  tree index
    = constexpr_expression (ctx, t, allow_one_past, non_constant_p, overflow_p);
  VERIFY_CONSTANT (index);

  if (!tree_fits_shwi_p (index) || tree_int_cst_sgn (index) < 0)
    {
      diag_array_subscript (loc, ctx, ary, index);
      *non_constant_p = true;
      return t;
    }

  tree nelts = get_array_or_vector_nelts (ctx, TREE_TYPE (ary), non_constant_p,
					  overflow_p);
  VERIFY_CONSTANT (nelts);
  if (allow_one_past ? !tree_int_cst_le (index, nelts)
		     : !tree_int_cst_lt (index, nelts))
    {
      diag_array_subscript (loc, ctx, ary, index);
      *non_constant_p = true;
      return t;
    }

  return index;
}

// forked from gcc/cp/constexpr.cc extract_string_elt

/* Extract element INDEX consisting of CHARS_PER_ELT chars from
   STRING_CST STRING.  */

static tree
extract_string_elt (tree string, unsigned chars_per_elt, unsigned index)
{
  tree type = cv_unqualified (TREE_TYPE (TREE_TYPE (string)));
  tree r;

  if (chars_per_elt == 1)
    r = build_int_cst (type, TREE_STRING_POINTER (string)[index]);
  else
    {
      const unsigned char *ptr
	= ((const unsigned char *) TREE_STRING_POINTER (string)
	   + index * chars_per_elt);
      r = native_interpret_expr (type, ptr, chars_per_elt);
    }
  return r;
}

// forked from gcc/cp/constexpr.cc free_constructor

/* If T is a CONSTRUCTOR, ggc_free T and any sub-CONSTRUCTORs.  */

static void
free_constructor (tree t)
{
  if (!t || TREE_CODE (t) != CONSTRUCTOR)
    return;
  releasing_vec ctors;
  vec_safe_push (ctors, t);
  while (!ctors->is_empty ())
    {
      tree c = ctors->pop ();
      if (vec<constructor_elt, va_gc> *elts = CONSTRUCTOR_ELTS (c))
	{
	  constructor_elt *ce;
	  for (HOST_WIDE_INT i = 0; vec_safe_iterate (elts, i, &ce); ++i)
	    if (TREE_CODE (ce->value) == CONSTRUCTOR)
	      vec_safe_push (ctors, ce->value);
	  ggc_free (elts);
	}
      ggc_free (c);
    }
}

} // namespace Compile
} // namespace Rust
