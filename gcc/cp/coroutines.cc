/* coroutine-specific state, expansions and tests.

   Copyright (C) 2018-2019 Free Software Foundation, Inc.

 Contributed by Iain Sandoe <iain@sandoe.co.uk> under contract to Facebook.

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

/* FIXME: minimise headers.. */
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "bitmap.h"
#include "cp-tree.h"
#include "stringpool.h"
#include "cgraph.h"
#include "stmt.h"
#include "varasm.h"
#include "stor-layout.h"
#include "c-family/c-objc.h"
#include "tree-inline.h"
#include "intl.h"
#include "tree-iterator.h"
#include "omp-general.h"
#include "convert.h"
#include "stringpool.h"
#include "attribs.h"
#include "gomp-constants.h"
#include "predict.h"
#include "tree.h"
#include "cxx-pretty-print.h"
#include "gcc-rich-location.h"

/* Investigation of different strategies.  */
#define USE_SWITCH_CO_YIELD_GUARD 1

/* DEBUG remove me.  */
extern void debug_tree(tree);

static tree find_coro_traits_template_decl (location_t);
static tree find_coro_handle_type (location_t, tree);
static tree find_promise_type (tree);
static tree lookup_promise_member (tree, const char *, location_t, bool);
static bool coro_promise_type_found_p (tree, location_t);
static tree build_co_await (location_t, tree, tree);

/* ================= Parse, Semantics and Type checking ================= */

/* Lookup std::experimental.  */
static tree
find_std_experimental (location_t loc)
{
  /* we want std::experimental::coroutine_traits class template decl.  */
  tree exp_name = get_identifier ("experimental");
  tree exp_ns = lookup_qualified_name (std_node, exp_name, 0, false, false);

  if (exp_ns == error_mark_node)
    {
      error_at (loc, "std::experimental not found");
      return NULL_TREE;
    }
  return exp_ns;
}

/* Lookup the coroutine_traits template decl.
   Instantiate that for the function signature.  */

static tree
find_coro_traits_template_decl (location_t kw)
{
  tree exp_ns = find_std_experimental (kw);
  if (!exp_ns)
    return NULL_TREE;

  /* So now build up a type list for the template <R, ...>.
     The function arg list length includes a terminating 'void' which we
     don't want - but we use that slot for the fn return type (which we do
     list even if it's 'void').  */
  tree functyp = TREE_TYPE (current_function_decl);
  tree arg_node = TYPE_ARG_TYPES (functyp);
  tree targ = make_tree_vec (list_length (arg_node));
  TREE_VEC_ELT (targ, 0) = TYPE_MAIN_VARIANT (TREE_TYPE (functyp));
  unsigned p = 1;
  while (arg_node != NULL_TREE
         && !VOID_TYPE_P (TREE_VALUE (arg_node)))
    {
      TREE_VEC_ELT (targ, p++) = TREE_VALUE (arg_node);
      arg_node = TREE_CHAIN (arg_node);
    }

  tree traits_name = get_identifier ("coroutine_traits");
  tree traits_decl = lookup_template_class (traits_name, targ,
					    /* in_decl */ NULL_TREE,
					    /* context */ exp_ns,
					    /* entering scope */ false,
					    tf_none);

  if (traits_decl == error_mark_node)
    {
      error_at (kw, "couldn't instantiate coroutine_traits");
      return NULL_TREE;
    }

  return traits_decl;
}

static tree
find_coro_handle_type (location_t kw, tree promise_type)
{
  tree exp_ns = find_std_experimental (kw);
  if (!exp_ns)
    return NULL_TREE;

  /* So now build up a type list for the template, one entry, the promise.  */
  tree targ = make_tree_vec (1);
  TREE_VEC_ELT (targ, 0) = promise_type;
  tree handle_name = get_identifier ("coroutine_handle");
  tree handle_type = lookup_template_class (handle_name, targ,
					    /* in_decl */ NULL_TREE,
					    /* context */ exp_ns,
					    /* entering scope */ false,
					    tf_none);

  if (handle_type == error_mark_node)
    {
      error_at (kw, "couldn't instantiate coroutine_handle for promise");
      return NULL_TREE;
    }

  return handle_type;
}

/* Look for the promise_type in the instantiated.  */

static tree
find_promise_type (tree handle_type)
{
  tree promise_name = get_identifier ("promise_type");

  tree promise_type = lookup_member (handle_type, promise_name,
				     /* protect */1, /*want_type=*/ true,
				     tf_warning_or_error);
  if (promise_type)
    promise_type = complete_type_or_else (TREE_TYPE (promise_type),
					  promise_type);

  /* NULL_TREE on fail.  */
  return promise_type;
}

static bool
coro_promise_type_found_p (tree fndecl, location_t loc)
{
  gcc_assert (fndecl != NULL_TREE);

  /* FIXME: the state needs to be in a hashtab on the side and this name
     is too long!!  */

  /* If we don't already have a current promise type, try to look it up.  */
  if (DECL_COROUTINE_PROMISE_TYPE(fndecl) == NULL_TREE)
    {
      /* Get the coroutine traits temple decl for the specified return and
         argument type list.  coroutine_traits <R, ...> */
      tree templ_decl = find_coro_traits_template_decl (loc);
      /* Find the promise type for that.  */
      DECL_COROUTINE_PROMISE_TYPE (fndecl) = find_promise_type (templ_decl);
      /* Find the handle type for that.  */
      tree handle_type
        = find_coro_handle_type (loc, DECL_COROUTINE_PROMISE_TYPE(fndecl));
      /* Instantiate this, we're going to use it.  */
      handle_type = complete_type_or_else (handle_type, fndecl);
      DECL_COROUTINE_HANDLE_TYPE (fndecl) = handle_type;
      /* Build a proxy for a handle to "self" as the param to await_suspend()
	 calls.  */
      DECL_COROUTINE_SELF_H_PROXY (fndecl)
	= build_lang_decl (VAR_DECL, get_identifier ("self_h.proxy"),
			   handle_type);
      /* Build a proxy for the promise so that we can perform lookups.  */
      DECL_COROUTINE_PROMISE_PROXY (fndecl)
	= build_lang_decl (VAR_DECL, get_identifier ("promise.proxy"),
			   DECL_COROUTINE_PROMISE_TYPE (fndecl));
      /* Note where we first saw a coroutine keyword.  */
      DECL_COROUTINE_FIRST_KEYWD_LOC (fndecl) = loc;
    }

  if (DECL_COROUTINE_PROMISE_TYPE(fndecl) == NULL_TREE)
    {
      error_at (loc, "unable to find the promise type for this coroutine");
      return false;
    }
  return true;
}

/* Lookup a Promise member.  */

static tree
lookup_promise_member (tree fndecl, const char * member_name,
		       location_t loc, bool musthave)
{
  tree pm_name = get_identifier (member_name);
  tree promise = DECL_COROUTINE_PROMISE_TYPE(fndecl);
  tree pm_memb = lookup_member (promise, pm_name,
				/*protect*/1,  /*want_type*/ 0,
				tf_warning_or_error);
  if (musthave && (pm_memb == NULL_TREE || pm_memb == error_mark_node))
    {
      error_at (loc, "no member named %qs in %qT", member_name, promise);
      return error_mark_node;
    }
  return pm_memb;
}

/* Here we will check the constraints that are comon to all keywords.  */

static bool
coro_common_keyword_context_valid_p (tree fndecl, location_t kw_loc,
				     const char *kw_name)
{
  if (fndecl == NULL_TREE)
    {
      error_at (kw_loc, "%qs cannot be used outside a function", kw_name);
      return false;
    }

  /* This is arranged in order of prohibitions in the TS.  */
  if (DECL_MAIN_P (fndecl))
    {
      // [6.6.1, main shall not be a coroutine].
      error_at (kw_loc, "%qs cannot be used in"
		" the %<main%> function", kw_name);
      return false;
    }

  if (DECL_DECLARED_CONSTEXPR_P (fndecl))
    {
      // [10.1.5, not constexpr specifier].
      error_at (kw_loc, "%qs cannot be used in"
		" a %<constexpr%> function", kw_name);
      cp_function_chain->invalid_constexpr = true;
      return false;
    }

  if (FNDECL_USED_AUTO (fndecl))
    {
      // [10.1.6.4, not auto specifier].
      error_at (kw_loc, "%qs cannot be used in"
		" a function with a deduced return type", kw_name);
      return false;
    }

  if (varargs_function_p (fndecl))
    {
      // [11.4.4, shall not be varargs].
      error_at (kw_loc, "%qs cannot be used in"
		" a varargs function", kw_name);
      return false;
    }

  if (DECL_CONSTRUCTOR_P (fndecl))
    {
      // [15.1, A constructor shall not be a coroutine.
      error_at (kw_loc, "%qs cannot be used in a constructor", kw_name);
      return false;
    }

  if (DECL_DESTRUCTOR_P (fndecl))
    {
      // [15.2, A destructor shall not be a coroutine.
      error_at (kw_loc, "%qs cannot be used in a destructor", kw_name);
      return false;
    }

  return true;
}

/* Here we will check the constraints that are not per keyword.  */

static bool
coro_function_valid_p (tree fndecl)
{
  location_t f_loc = DECL_SOURCE_LOCATION (fndecl);

  /* Since we think the function is a coroutine, that implies we parsed
     a keyword that triggered this.  Keywords check promise validity for
     their context and thus the promise type should be known at this point.
  */
  gcc_assert (DECL_COROUTINE_HANDLE_TYPE(fndecl) != NULL_TREE
              && DECL_COROUTINE_PROMISE_TYPE(fndecl) != NULL_TREE);

  if (current_function_returns_value || current_function_returns_null)
    /* TODO: record or extract positions of returns (and the first coro
       keyword) so that we can add notes to the diagnostic about where
       the bad keyword is and what made the function into a coro.  */
    error_at (f_loc, "return statement not allowed in coroutine;"
                     " did you mean %<co_return%>?" );

  return true;
}

/*  This performs 8.3.8 bullet 3.3 and validates the interface obtained.
    It is also used to build the initial and final suspend points.   */
static tree
build_co_await (location_t loc, tree a, tree mode)
{
  /* Try and overload of operator co_await, .... */
  tree o;
  if (MAYBE_CLASS_TYPE_P (TREE_TYPE (a)))
    {
      tree overload = NULL_TREE;
      o = build_new_op (loc, CO_AWAIT_EXPR, LOOKUP_NORMAL, a,
			NULL_TREE, NULL_TREE, &overload,
			tf_warning_or_error);
      /* If no viable functions are found, o is a.  */
      if (!o || o == error_mark_node)
        o = a;
    }
  else
    o = a; /* This is most likely about to fail anyway.  */    

  tree o_type = complete_type_or_else (TREE_TYPE (o), o);
  if (TREE_CODE (o_type) != RECORD_TYPE)
    {
      error_at (loc, "member reference base type %qT is not a"
		" structure or union", o_type);
      return error_mark_node;
    }

  /* Check for required awaitable members and their types.  */
  tree awrd_meth = lookup_member (o_type, get_identifier ("await_ready"),
				  /* protect */1, /*want_type=*/ 0,
				  tf_warning_or_error);

  if (!awrd_meth || awrd_meth == error_mark_node)
    return error_mark_node;

  tree awsp_meth = lookup_member (o_type, get_identifier ("await_suspend"),
				  /* protect */1, /*want_type=*/ 0,
				  tf_warning_or_error);

  if (!awsp_meth || awsp_meth == error_mark_node)
    return error_mark_node;

  /* The type of the co_await is the return type of the awaitable's
     co_resume(), so we need to look that up.  */
  tree awrs_meth = lookup_member (o_type, get_identifier ("await_resume"),
				 /* protect */1, /*want_type=*/ 0,
				 tf_warning_or_error);

  if (!awrs_meth || awrs_meth == error_mark_node)
    return error_mark_node;

  /* To complete the lookups, we need an instance of 'e' which is built from
     'o' according to 8.3.8 3.4.  However, we don't want to materialise 'e'
     here (it might need to be placed in the coroutine frame) so we will make
     a temp placeholder instead. */
  tree e_proxy = build_lang_decl (VAR_DECL, NULL_TREE, o_type);

  /* I suppose we could check that this is contextually convertible to bool.  */
  tree awrd_func = NULL_TREE;
  tree awrd_call  = build_new_method_call (e_proxy, awrd_meth,  NULL, NULL_TREE,
					  LOOKUP_NORMAL, &awrd_func,
					  tf_warning_or_error);

  if (!awrd_func || !awrd_call || awrd_call == error_mark_node)
    return error_mark_node;

  /* The suspend method has constraints on its return type.  */
  tree awsp_func = NULL_TREE;
  tree h_proxy = DECL_COROUTINE_SELF_H_PROXY (current_function_decl);
  vec<tree, va_gc>* args = make_tree_vector_single (h_proxy);
  tree awsp_call  = build_new_method_call (e_proxy, awsp_meth, &args, NULL_TREE,
					  LOOKUP_NORMAL, &awsp_func,
					  tf_warning_or_error);

  release_tree_vector (args);
  if (!awsp_func || !awsp_call || awsp_call == error_mark_node)
    return error_mark_node;

  bool OK = false;
  tree susp_return_type = TYPE_CANONICAL (TREE_TYPE (TREE_TYPE (awsp_func)));
  if (same_type_p (susp_return_type, void_type_node))
    OK = true;
  else if (same_type_p (susp_return_type, boolean_type_node))
    OK = true;
  else if (POINTER_TYPE_P (susp_return_type))
    OK = true;

  if (!OK)
    {
      fprintf (stderr, "didn't grok the suspend return : " );
      debug_tree (susp_return_type);
      error_at (loc, "%<await_suspend%> must return %<void%>, %<bool%> or"
		" a coroutine handle.");
      return error_mark_node;
    }

  /* Finally, the type of e.await_resume() is the co_await's type.  */
  tree awrs_func = NULL_TREE;
  tree awrs_call  = build_new_method_call (e_proxy, awrs_meth,  NULL, NULL_TREE,
					  LOOKUP_NORMAL, &awrs_func,
					  tf_warning_or_error);

  if (!awrs_func || !awrs_call || awrs_call == error_mark_node)
    return error_mark_node;

  /* We now have three call expressions, in terms of the promise, handle and
     'e' proxies.  Save them in the await expression for later expansion.  */

  tree awaiter_calls = make_tree_vec (3);
  TREE_VEC_ELT (awaiter_calls, 0) = awrd_call; /* await_ready().  */
  TREE_VEC_ELT (awaiter_calls, 1) = awsp_call; /* await_suspend().  */
  TREE_VEC_ELT (awaiter_calls, 2) = awrs_call; /* await_resume().  */

  return build5_loc (loc, CO_AWAIT_EXPR, TREE_TYPE (TREE_TYPE (awrs_func)),
		     a, e_proxy, o, awaiter_calls, mode);
}


tree
finish_co_await_expr (location_t kw, tree expr)
{
  if (!coro_common_keyword_context_valid_p (current_function_decl, kw,
					   "co_await"))
    return error_mark_node;

  /* We must be able to look up the "await_transform" method in the scope of
     the promise type, and obtain it's return type.  */
  if (!coro_promise_type_found_p (current_function_decl, kw))
    return error_mark_node;

  /* The current function has now become a coroutine, if it wasn't already.  */
  DECL_COROUTINE_P (current_function_decl) = 1;

  if (expr == NULL_TREE)
    {
      error_at (kw, "%<co_await%> requires an expression." );
      return error_mark_node;
    }

  /* The incoming cast expression might be transformed by a promise
     'await_transform()'.  */
  tree at_meth = lookup_promise_member (current_function_decl, 
					"await_transform", kw, false /*musthave*/);
  if (at_meth == error_mark_node)
    return error_mark_node;

  tree a = expr;
  if (at_meth)
    {
      /* try to build a = p.await_transform (e). */
      tree at_fn = NULL_TREE;
      vec<tree, va_gc>* args = make_tree_vector_single (expr);
      a = build_new_method_call
	(DECL_COROUTINE_PROMISE_PROXY (current_function_decl), at_meth,  &args,
	 NULL_TREE, LOOKUP_NORMAL, &at_fn, tf_warning_or_error);

      /* Probably it's not an error to fail here, although possibly a bit odd
	 to find await_transform but not a valid one?  */
      if (!at_fn || a == error_mark_node)
	return error_mark_node;
     }

  /* Now we want to build co_await a.
     The trailing '0' is a flag that notes this is a regular co_await.  */
  tree op = build_co_await (kw, a, integer_zero_node);
  TREE_SIDE_EFFECTS (op) = 1;
  SET_EXPR_LOCATION (op, kw);

  return op;
}

/* Take the EXPR given and attempt to build:
     co_await p.yield_value (expr);
   per 8.21 §1.
*/
tree
finish_co_yield_expr (location_t kw, tree expr)
{
  if (expr == error_mark_node)
    return error_mark_node;

  /* Check the general requirements and simple syntax errors.  */
  if (!coro_common_keyword_context_valid_p (current_function_decl, kw,
					    "co_yield"))
    return error_mark_node;

  /* We must be able to look up the "yield_value" method in the scope of
     the promise type, and obtain it's return type.  */
  if (!coro_promise_type_found_p (current_function_decl, kw))
    return error_mark_node;

  /* The current function has now become a coroutine, if it wasn't already.  */
  DECL_COROUTINE_P (current_function_decl) = 1;

  /* Belt and braces, we should never get here, the expression should be
     required in the parser. */
  if (expr == NULL_TREE)
    {
      error_at (kw, "%<co_yield%> requires an expression." );
      return error_mark_node;
    }

  /* The incoming expr is "e" per 8.21 §1, lookup and build a call for
     p.yield_value(e).  */
  tree y_meth = lookup_promise_member (current_function_decl, "yield_value",
				       kw, true /*musthave*/);
  if (!y_meth || y_meth == error_mark_node)
    return error_mark_node;

  tree yield_fn = NULL_TREE;
  vec<tree, va_gc>* args = make_tree_vector_single (expr);
  tree yield_call = build_new_method_call
    (DECL_COROUTINE_PROMISE_PROXY (current_function_decl), y_meth,  &args,
     NULL_TREE, LOOKUP_NORMAL, &yield_fn, tf_warning_or_error);

  if (!yield_fn || yield_call == error_mark_node)
    return error_mark_node;

  /* So now we have the type of p.yield_value (e).
     Now we want to build co_await p.yield_value (e).
     Noting that for co_yield, there is no evaluation of any potential
     promise transform_await().  The trailing '1' is a flag that notes this
     co_await resulted from a co_yield.   */

  tree op = build_co_await (kw, yield_call, integer_one_node);
  op = build2 (CO_YIELD_EXPR, TREE_TYPE (op), expr, op);
  TREE_SIDE_EFFECTS (op) = 1;
  SET_EXPR_LOCATION (op, kw);

  return op;
}

/* placeholder; in case we really need something more than the contextual
   checks.  */
static tree
check_co_return_expr (tree retval, bool *no_warning)
{

  *no_warning = false;

  return retval;
}

/* Check that it's valid to have a co_return keyword here.
   If it is, then check and build the p.return_{void(),value(expr)}.
   These are built against the promise proxy, but saved for expand time.  */

tree
finish_co_return_stmt (location_t kw, tree expr)
{
  if (expr == error_mark_node)
    return error_mark_node;

  if (! coro_common_keyword_context_valid_p (current_function_decl, kw,
					   "co_return"))
    return error_mark_node;

  if (! coro_promise_type_found_p (current_function_decl, kw))
    return error_mark_node;

  /* The current function has now become a coroutine, if it wasn't
     already.  */
  DECL_COROUTINE_P (current_function_decl) = 1;

  bool no_warning;
  expr = check_co_return_expr (expr, &no_warning);

  if (error_operand_p (expr))
    return error_mark_node;

  /* Suppress -Wreturn-type for co_return, we need to check indirectly
     whether the promise type has a suitable return_void/return_value.  */
  if (warn_return_type)
    TREE_NO_WARNING (current_function_decl) = true;

  if (!processing_template_decl && warn_sequence_point)
    verify_sequence_points (expr);    

  /* If the promise object doesn't have the correct return call then
     there's a mis-match between the co_return <expr> and this.  */
  tree co_ret_call = NULL_TREE;
  if (expr == NULL_TREE || VOID_TYPE_P (TREE_TYPE (expr)))
    {
      tree crv_meth = lookup_promise_member (current_function_decl, "return_void",
					     kw, true /*musthave*/);
      if (!crv_meth || crv_meth == error_mark_node)
	return error_mark_node;

      co_ret_call = build_new_method_call
	(DECL_COROUTINE_PROMISE_PROXY (current_function_decl), crv_meth,  NULL,
	 NULL_TREE, LOOKUP_NORMAL, NULL, tf_warning_or_error);
    }
  else
    {
      tree crv_meth = lookup_promise_member (current_function_decl, "return_value",
					     kw, true /*musthave*/);
      if (!crv_meth || crv_meth == error_mark_node)
	return error_mark_node;

      vec<tree, va_gc>* args = make_tree_vector_single (expr);
      co_ret_call = build_new_method_call
	(DECL_COROUTINE_PROMISE_PROXY (current_function_decl), crv_meth,  &args,
	 NULL_TREE, LOOKUP_NORMAL, NULL, tf_warning_or_error);
    }

  /* Makes no sense for a co-routine really. */
  if (TREE_THIS_VOLATILE (current_function_decl))
    warning_at (kw, 0, "function declared %<noreturn%> has a"
		" %<co_return%> statement");

  if (!co_ret_call || co_ret_call == error_mark_node)
    return error_mark_node;

  expr = build2_loc (kw, CO_RETRN_EXPR, void_type_node, expr, co_ret_call);
  TREE_NO_WARNING (expr) |= no_warning;
  expr = maybe_cleanup_point_expr_void (expr);
  expr = add_stmt (expr);
  return expr;
}

/* ================= Morph and Expand. ================= */

struct __proxy_replace {
  tree from, to;
};

static tree
replace_proxy (tree *here, int *do_subtree, void *d)
{
  struct __proxy_replace *data = (struct __proxy_replace *)d;

  if (*here == data->from)
    {
      *here = data->to;
      *do_subtree = 0;
    }
  else
    *do_subtree = 1;
  return NULL_TREE;
}

/* Support for expansion of co_return statements.  */
struct __coro_ret_data {
  tree promise_proxy;
  tree real_promise;
  tree fs_label;
};

/* Callback that rewrites co_return as per 9.6.3.1
   - for co_return;
   { p.return_void (); goto final_suspend; }
   - for co_return [void expr];
   { expr; p.return_void(); goto final_suspend;}
   - for co_return [non void expr];
   { p.return_value(expr); goto final_suspend; }
*/
   
static tree
co_return_expander (tree *stmt, int *, void *d)
{
  struct __coro_ret_data *data = (struct __coro_ret_data *) d;

  if (TREE_CODE (*stmt) != CO_RETRN_EXPR)
    return NULL_TREE;

  location_t loc = EXPR_LOCATION (*stmt);
  tree expr = TREE_OPERAND (*stmt, 0);
  tree call = TREE_OPERAND (*stmt, 1);
  tree stmt_list = NULL;
  if (expr)
    append_to_statement_list (expr, &stmt_list);

  /* Now replace the promise proxy with its real value.  */
  struct __proxy_replace p_data;
  p_data.from = data->promise_proxy;
  p_data.to = data->real_promise;
  cp_walk_tree (&call, replace_proxy, &p_data, NULL);
  append_to_statement_list (call, &stmt_list);
  tree r = build1_loc (loc, GOTO_EXPR, void_type_node, data->fs_label);
  append_to_statement_list (r, &stmt_list);
  *stmt = stmt_list;

  return NULL_TREE;
}

/* Walk the original function body, rewriting co_returns.  */
static tree
expand_co_returns (tree *fnbody, tree promise_proxy, tree promise, tree fs_label)
{
  struct __coro_ret_data data = { promise_proxy, promise, fs_label};
  cp_walk_tree (fnbody, co_return_expander, &data, NULL);
  return *fnbody;
}

/* Support for expansion of co_await statements.  */
struct __coro_aw_data {
  tree actor_fn; /* Decl for context */
  tree coro_fp;
  tree resume_idx;
  tree cleanup;
  tree cororet;
  tree self_h;
  unsigned index; // resume point.
};


static tree
create_anon_label_with_ctx (location_t loc, tree ctx)
{
  tree lab = build_decl (loc, LABEL_DECL, NULL_TREE, void_type_node);

  DECL_ARTIFICIAL (lab) = 1;
  DECL_IGNORED_P (lab) = 1;
  DECL_CONTEXT (lab) = ctx;
  return lab;
}

static tree
co_await_find_in_subtree (tree *stmt, int *do_subtree ATTRIBUTE_UNUSED,
			  void *d)
{
  tree **p = (tree **)d;
  if (TREE_CODE (*stmt) == CO_AWAIT_EXPR)
    {
      *p = stmt;
      return *stmt;
    }
  return NULL_TREE;
}

/* When we come here:
    the first operand is 'o' as defined in 8.3.8 (3.3)
    the second operand is the var to be copy-initialised
    the third operand is the handle for suspend.
    the fourth operand is NULL unless this is final suspend, when it's 1.

   When we leave:
   the CO_AWAIT carries the labels of the resume and destroy
   branch targets for this await.

TODO :
  This doesn't check the return type await_suspend, it assumes it to be void.
  This doesn't deal with the return value from await_resume, it assumes it to
  be void.

*/
    
static tree
co_await_expander (tree *stmt, int *do_subtree, void *d)
{
  if (!EXPR_P (*stmt))
    return NULL_TREE;

  struct __coro_aw_data *data = (struct __coro_aw_data *) d;

  enum tree_code stmt_code = TREE_CODE (*stmt);
  tree stripped_stmt = *stmt;

  /* Look inside <(void) (expr)> cleanup */
  if (stmt_code == CLEANUP_POINT_EXPR)
    {
      stripped_stmt = TREE_OPERAND (*stmt, 0);
      stmt_code = TREE_CODE (stripped_stmt);
      if (stmt_code == EXPR_STMT
	  && (TREE_CODE (EXPR_STMT_EXPR (stripped_stmt)) == CONVERT_EXPR
	      || TREE_CODE (EXPR_STMT_EXPR (stripped_stmt)) == CAST_EXPR)
	  && VOID_TYPE_P (TREE_TYPE (EXPR_STMT_EXPR (stripped_stmt))))
	{
	  stripped_stmt = TREE_OPERAND (EXPR_STMT_EXPR (stripped_stmt), 0);
	  stmt_code = TREE_CODE (stripped_stmt);
	}
    }

  tree *buried_stmt = NULL;
  tree saved_co_await = NULL_TREE;
  enum tree_code sub_code;

  if (stmt_code == EXPR_STMT
      && TREE_CODE (EXPR_STMT_EXPR (stripped_stmt)) == CO_AWAIT_EXPR)
    saved_co_await = EXPR_STMT_EXPR (stripped_stmt); /* hopefully, a void exp.  */
  else if (stmt_code == MODIFY_EXPR || stmt_code == INIT_EXPR)
    {
      sub_code = TREE_CODE (TREE_OPERAND (stripped_stmt, 1));
      if (sub_code == CO_AWAIT_EXPR)
        saved_co_await = TREE_OPERAND (stripped_stmt, 1); /* Get the RHS.  */
      else if (tree r = cp_walk_tree 
			 (&TREE_OPERAND (stripped_stmt, 1),
			  co_await_find_in_subtree, &buried_stmt, NULL))
	{
          //debug_tree(*stmt);
          saved_co_await = r;
	}
    }

  if (!saved_co_await)
    return NULL_TREE;

  /* We want to splice in the await_resume() value in some cases.  */
  tree saved_statement = *stmt;

  tree actor = data->actor_fn;
  location_t loc = EXPR_LOCATION (*stmt);
  tree expr = TREE_OPERAND (saved_co_await, 0);
  tree var = TREE_OPERAND (saved_co_await, 1);
  //tree sv_handle = TREE_OPERAND (saved_co_await, 2); /* not yet.  */
  tree awaiter_calls = TREE_OPERAND (saved_co_await, 3);

  tree source = TREE_OPERAND (saved_co_await, 4);
  bool is_final = (source && TREE_INT_CST_LOW (source) == 3);
  bool needs_dtor = TYPE_HAS_NONTRIVIAL_DESTRUCTOR (TREE_TYPE (var));
  int resume_point = data->index;
  size_t bufsize = sizeof ("destroy.") + 10;
  char *buf = (char *) alloca (bufsize);
  snprintf (buf, bufsize, "destroy.%d", resume_point);
  tree destroy_label = get_identifier (buf);
  destroy_label = define_label (loc, destroy_label);
  DECL_CONTEXT (destroy_label) = actor;
  snprintf (buf, bufsize, "resume.%d", resume_point);
  tree resume_label = get_identifier (buf);
  resume_label = define_label (loc, resume_label);
  DECL_CONTEXT (resume_label) = actor;
  tree empty_list = build_empty_stmt (loc);

  tree dtor = NULL_TREE;
  tree await_type = TREE_TYPE (var);
  if (needs_dtor)
    dtor = build_special_member_call(var, complete_dtor_identifier, NULL,
				     await_type, LOOKUP_NORMAL,
				     tf_warning_or_error);

  tree stmt_list = NULL;
  /* Initialise the var from the provided 'o' expression.  */
  tree r = build2 (INIT_EXPR, await_type, var, expr);
  r = build1 (CONVERT_EXPR, void_type_node, r);
  r = build_stmt (loc, EXPR_STMT, r);
  r = maybe_cleanup_point_expr_void (r);
  append_to_statement_list (r, &stmt_list);

  /* Use the await_ready() call to test if we need to suspend.  */
  tree ready_cond = TREE_VEC_ELT (awaiter_calls, 0); /* await_ready().  */
  ready_cond = build1_loc (loc, TRUTH_NOT_EXPR, boolean_type_node, ready_cond);
  ready_cond = build1_loc (loc, CLEANUP_POINT_EXPR,
			   boolean_type_node, ready_cond);

  tree body_list = NULL;
  tree susp_idx = build_int_cst (short_unsigned_type_node, data->index);
  r = build2_loc (loc, MODIFY_EXPR, short_unsigned_type_node,
		  data->resume_idx, susp_idx);
  r = build1 (CONVERT_EXPR, void_type_node, r);
  r = build_stmt (loc, EXPR_STMT, r);
  r = maybe_cleanup_point_expr_void (r);
  append_to_statement_list (r, &body_list);

  /* Make a TARGET_EXPR for the handle argument to the suspend call
     i.e. coroutine_handle::from_address ((void *) frame_pointer).
     from_address is a static member function.  */
  tree handle_type = TREE_TYPE (data->self_h);
  tree hfa_m = lookup_member (handle_type, get_identifier ("from_address"),
			     1, 0, tf_warning_or_error);
  r = build1 (CONVERT_EXPR, build_pointer_type (void_type_node), data->coro_fp);
  vec<tree, va_gc>* args = make_tree_vector_single (r);
  /* Dummy instance for the method call.  */
  tree tmp_hdl = build_lang_decl (VAR_DECL, NULL_TREE, handle_type);
  tree hfa = build_new_method_call (tmp_hdl, hfa_m, &args, NULL_TREE,
				    LOOKUP_NORMAL, NULL, tf_warning_or_error);
  hfa = build_target_expr_with_type (hfa, handle_type, tf_warning_or_error);

  tree suspend = TREE_VEC_ELT (awaiter_calls, 1); /* await_suspend().  */

  /* FIXME: we shouldn't do this twice, but actually replace the handle proxy
     here and discard the frame.self_h.  */
  struct __proxy_replace xform;
  xform.from = data->self_h;
  xform.to = hfa;
  cp_walk_tree (&suspend, replace_proxy, &xform, NULL);

  /* We are not (yet) going to muck around with figuring out the actions
     for different return type on the suspend, our simple case assumes
     it's void.  */

  suspend = build1 (CONVERT_EXPR, void_type_node, suspend);
  suspend = build_stmt (loc, EXPR_STMT, suspend);
  suspend = maybe_cleanup_point_expr_void (suspend);
  append_to_statement_list (suspend, &body_list);

  tree d_l = build1 (ADDR_EXPR, build_reference_type (void_type_node),
		     destroy_label);
  tree r_l = build1 (ADDR_EXPR, build_reference_type (void_type_node),
		     resume_label);
  tree final_susp = build_int_cst (integer_type_node, is_final ? 1 : 0);

  susp_idx = build_int_cst (integer_type_node, data->index);

#if USE_SWITCH_CO_YIELD_GUARD
  tree sw = begin_switch_stmt ();
  tree cond = build_decl (loc, VAR_DECL, NULL_TREE, integer_type_node);
  DECL_ARTIFICIAL (cond) = 1;
  DECL_IGNORED_P (cond) = 1;
  layout_decl (cond, 0);

  r =  build_call_expr_internal_loc (loc, IFN_CO_YIELD, integer_type_node, 4,
				     susp_idx, final_susp, r_l, d_l);
  r = build2 (INIT_EXPR, integer_type_node, cond, r);
  finish_switch_cond (r, sw);
  r = build_case_label (build_int_cst (integer_type_node, 0), NULL_TREE,
			create_anon_label_with_ctx (loc, actor));
  add_stmt (r); // case 0:
  r = build1_loc (loc, GOTO_EXPR, void_type_node, data->cororet);
  add_stmt (r); //   goto ret;
  r = build_case_label (build_int_cst (integer_type_node, 1), NULL_TREE,
			create_anon_label_with_ctx (loc, actor));
  add_stmt (r); // case 1:
  r = build1_loc (loc, GOTO_EXPR, void_type_node, resume_label);
  add_stmt (r); //  goto resume;
  r = build_case_label (NULL_TREE, NULL_TREE,
			create_anon_label_with_ctx (loc, actor));
  add_stmt (r); // default:;
  r = build1_loc (loc, GOTO_EXPR, void_type_node, destroy_label);
  add_stmt (r); // goto destroy;

  /* part of finish switch.  */
  SWITCH_STMT_BODY (sw) = pop_stmt_list (SWITCH_STMT_BODY (sw));
  pop_switch ();
  tree scope = SWITCH_STMT_SCOPE (sw);
  SWITCH_STMT_SCOPE (sw) = NULL;
  r = do_poplevel (scope);
  append_to_statement_list (r, &body_list);
#else
  /* anon temp. */
  tree cond = build_decl (loc, VAR_DECL, NULL_TREE, integer_type_node);
  DECL_ARTIFICIAL (cond) = 1;
  DECL_IGNORED_P (cond) = 1;
  layout_decl (cond, 0);
  r =  build_call_expr_internal_loc (loc, IFN_CO_YIELD, integer_type_node, 4,
				     susp_idx, final_susp, r_l, d_l);
  r = build2 (INIT_EXPR, integer_type_node, cond, r);
  r = build1 (CLEANUP_POINT_EXPR, integer_type_node, r);
  append_to_statement_list (r, &body_list);
  tree ret_list = NULL_TREE;
  tree alt_list = NULL_TREE;

  tree outer_if = begin_if_stmt ();
  tree cmp1 = build2 (EQ_EXPR, integer_type_node, cond, integer_zero_node);
  finish_if_stmt_cond (cmp1, outer_if);
  r = build1_loc (loc, GOTO_EXPR, void_type_node, data->cororet);
  add_stmt (r); // if 0 goto return;
  finish_then_clause (outer_if);
  tree inner_if = begin_if_stmt ();
   tree cmp2 = build2 (EQ_EXPR, integer_type_node, cond, integer_one_node);
   finish_if_stmt_cond (cmp2, inner_if);
   r = build1_loc (loc, GOTO_EXPR, void_type_node, resume_label);
   add_stmt (r); // else if 1 goto resume;
   finish_then_clause (inner_if);
   r = build1_loc (loc, GOTO_EXPR, void_type_node, destroy_label);
   add_stmt (r); // else goto resume;
   finish_if_stmt (inner_if);
  /* Most of finish if for the outer.  */
  tree scope = IF_SCOPE (outer_if);
  IF_SCOPE (outer_if) = NULL;
  r = do_poplevel (scope);
  append_to_statement_list (r, &body_list);
#endif

  destroy_label = build_stmt (loc, LABEL_EXPR, destroy_label);
  append_to_statement_list (destroy_label, &body_list);
  if (needs_dtor)
    append_to_statement_list (dtor, &body_list);
  r = build1_loc (loc, GOTO_EXPR, void_type_node, data->cleanup);
  append_to_statement_list (r, &body_list);
  
  r = build3_loc (loc, COND_EXPR, void_type_node,
		  ready_cond, body_list, empty_list);
  
  append_to_statement_list (r, &stmt_list);

  /* Resume point.  */
  resume_label = build_stmt (loc, LABEL_EXPR, resume_label);
  append_to_statement_list (resume_label, &stmt_list);

  /* This will produce the value (if one is provided) from the co_await
     expression.  */
  tree resume_call = TREE_VEC_ELT (awaiter_calls, 2); /* await_resume().  */
  switch (stmt_code)
    {
    default: /* not likely to work .. but... */
      append_to_statement_list (resume_call, &stmt_list);
      break;
    case INIT_EXPR:
    case MODIFY_EXPR:
       /* Replace the use of co_await by the resume expr.  */
      if (sub_code == CO_AWAIT_EXPR)
	{
	  /* We're updating the interior of a possibly <(void) expr>cleanup.  */
	  TREE_OPERAND (stripped_stmt, 1) = resume_call;
	  append_to_statement_list (saved_statement, &stmt_list);
	}
      else if (buried_stmt != NULL)
        {
	  *buried_stmt = resume_call;
	  append_to_statement_list (saved_statement, &stmt_list);
        }
      else
        {
	  error_at (loc, "failed to substitute the resume method in %qE",
		    saved_statement);
	  append_to_statement_list (saved_statement, &stmt_list);
        }
      break;
      
    }
  if (needs_dtor)
    append_to_statement_list (dtor, &stmt_list);
  data->index += 2;
  *stmt = stmt_list;
  *do_subtree = 0;
  return NULL_TREE;
}

static tree
expand_co_awaits (tree fn, tree *fnbody, tree coro_fp, tree resume_idx,
		  tree cleanup, tree cororet, tree self_h)
{
  struct __coro_aw_data data = {fn, coro_fp, resume_idx,
				cleanup, cororet, self_h, 2};
  cp_walk_tree (fnbody, co_await_expander, &data, NULL);
  return *fnbody;
}

/* Suspend point hash_map.  */

struct suspend_point_info {
  /* coro frame field type.  */
  tree awaitable_type;
  /* coro frame field name.  */
  tree await_field_id;
  /* coro frame handle field name, NULL_TREE if not needed.  */
  tree save_handle_id;
};

static hash_map<tree, struct suspend_point_info> *suspend_points;

struct __await_xform_data {
  tree actor_frame; 
  tree promise_proxy;
  tree real_promise;
  tree self_h_proxy;
  tree real_self_h;
};

/* When we built the await expressions, we didn't know the coro frame
   layout, therefore no idea where to find the promise or where to put
   the awaitables.  Now we know these things, fill them in.  */
static tree
transform_await_expr (tree await_expr, struct __await_xform_data *xform)
{
  struct suspend_point_info *si = suspend_points->get (await_expr);
  location_t loc = EXPR_LOCATION (await_expr);
  if (!si)
    {
      error_at (loc, "no suspend point info for %qD", await_expr);
      return error_mark_node;
    }
  
  /* Get a reference to the initial suspend var in the frame.
     Synthesize the init expression.  */
  tree coro_frame_type = TREE_TYPE (xform->actor_frame);
  tree as_m = lookup_member (coro_frame_type, si->await_field_id,
			     /*protect*/1,  /*want_type*/ 0,
			     tf_warning_or_error);
  tree as = build_class_member_access_expr (xform->actor_frame, as_m, NULL_TREE,
					    true, tf_warning_or_error);
 
  tree ah = NULL_TREE;
  if (si->save_handle_id)
    {
      tree ah_m = lookup_member (coro_frame_type, si->save_handle_id,
				 /*protect*/1,  /*want_type*/ 0,
				 tf_warning_or_error);
      ah = build_class_member_access_expr (xform->actor_frame, ah_m, NULL_TREE,
					   true, tf_warning_or_error);
    }

  /* So, we have now :
     in : CO_AWAIT_EXPR (a, e_proxy, o, awr_call, mode)
          We no longer need a [it had diagnostic value, maybe?]
          We need to replace the promise proxy in all elements
          We need to replace the e_proxy in the awr_call.
  */

  /* FIXME: determine if it's better to walk the co_await several times with
     a quick test, or once with a more complex test.  */
  /* the initialise expression is 'o', currently in slot 2, move it to slot
     0 (thus discarding 'a').  */
  TREE_OPERAND (await_expr, 0) = TREE_OPERAND (await_expr, 2);
  /* Op 2 becomes the frame slot for the temporary handle.  */
  TREE_OPERAND (await_expr, 2) = ah;

  struct __proxy_replace data = { TREE_OPERAND (await_expr, 1), as};
  cp_walk_tree (&await_expr, replace_proxy, &data, NULL);
  TREE_OPERAND (await_expr, 1) = as;

 /* Now do the self_handle.  */
  data.from = xform->self_h_proxy;
  data.to = xform->real_self_h;
  cp_walk_tree (&await_expr, replace_proxy, &data, NULL);

  /* Now do the promise.  */
  data.from = xform->promise_proxy;
  data.to = xform->real_promise;
  cp_walk_tree (&await_expr, replace_proxy, &data, NULL);
  
  return await_expr;
}

/* A wrapper for the routine above so that it can be a callback from 
   cp_walk_tree.  */
static tree
transform_await_wrapper (tree *stmt, int *do_subtree, void *d)
{
  if (TREE_CODE (*stmt) != CO_AWAIT_EXPR
      && TREE_CODE (*stmt) != CO_YIELD_EXPR)
    return NULL_TREE;

  tree await_expr = *stmt;
  struct __await_xform_data *xform = (struct __await_xform_data *)d;

  *stmt = transform_await_expr (await_expr, xform);
  if (*stmt == error_mark_node)
    *do_subtree = 0;
  return NULL_TREE;
}

/* The actor transform.  */

static void
build_actor_fn (location_t loc, tree coro_frame_type, tree actor,
	        tree fnbody, tree orig, 
		tree initial_await, tree final_await, unsigned body_count)
{
  verify_stmt_tree (fnbody);
  /* Some things we inherit from the original function.  */
  tree coro_frame_ptr = build_pointer_type (coro_frame_type);
  tree handle_type = DECL_COROUTINE_HANDLE_TYPE (orig);
  tree self_h_proxy = DECL_COROUTINE_SELF_H_PROXY (orig);
  tree promise_type = DECL_COROUTINE_PROMISE_TYPE (orig);
  tree promise_proxy = DECL_COROUTINE_PROMISE_PROXY (orig);
  tree act_des_fn_type = build_function_type_list (void_type_node,
						   coro_frame_ptr, NULL_TREE);
  tree act_des_fn_ptr = build_pointer_type (act_des_fn_type);

  /* One param, the coro frame pointer.  */
  tree actor_fp = build_lang_decl (PARM_DECL, get_identifier ("frame_ptr"),
				   coro_frame_ptr);
  DECL_CONTEXT (actor_fp) = actor;
  DECL_ARG_TYPE (actor_fp) = type_passed_as (coro_frame_ptr);
  DECL_ARGUMENTS (actor) = actor_fp;

  /* A void return.  */
  tree resdecl = build_decl (loc, RESULT_DECL, 0, void_type_node);
  DECL_ARTIFICIAL (resdecl) = 1;
  DECL_IGNORED_P (resdecl) = 1;
  DECL_RESULT (actor) = resdecl;
  TREE_STATIC (actor) = 1;

  tree actor_outer = push_stmt_list ();
  current_stmt_tree ()->stmts_are_full_exprs_p = 1;
  tree stmt = begin_compound_stmt (BCS_FN_BODY);

  tree actor_bind = build3 (BIND_EXPR, void_type_node, NULL, NULL, NULL);
  add_stmt (actor_bind);
  tree actor_body = push_stmt_list ();

  tree actor_begin_label = get_identifier ("actor.begin");
  actor_begin_label = define_label (loc, actor_begin_label);
  DECL_CONTEXT (actor_begin_label) = actor;

  tree actor_frame = build1 (INDIRECT_REF, coro_frame_type, actor_fp);

  tree resume_idx_name = get_identifier ("__resume_at");
  tree rat_field = lookup_member (coro_frame_type, resume_idx_name, 1, 0,
			     tf_warning_or_error);
  tree rat = build3 (COMPONENT_REF, short_unsigned_type_node, actor_frame,
		    rat_field, NULL_TREE);

  tree ret_label = get_identifier ("actor.ret");
  ret_label = define_label (input_location, ret_label);
  DECL_CONTEXT (ret_label) = actor;

  tree lsb_if = begin_if_stmt ();
  tree chkb0 = build2 (BIT_AND_EXPR, short_unsigned_type_node, rat,
		       build_int_cst (short_unsigned_type_node, 1));
  chkb0 = build2 (NE_EXPR, short_unsigned_type_node, chkb0,
		  build_int_cst (short_unsigned_type_node, 0));
  finish_if_stmt_cond (chkb0, lsb_if);

  tree destroy_dispatcher = begin_switch_stmt ();
  finish_switch_cond (rat, destroy_dispatcher);
  tree ddeflab = build_case_label (NULL_TREE, NULL_TREE,
				     create_anon_label_with_ctx (loc, actor));
  add_stmt (ddeflab);
  tree b = build_call_expr_loc (loc, builtin_decl_explicit (BUILT_IN_TRAP), 0);
  b = build_stmt (loc, EXPR_STMT, b);
  b = maybe_cleanup_point_expr_void (b);
  add_stmt (b);

  short unsigned lab_num = 3;
  for (unsigned destr_pt = 0; destr_pt < body_count + 2; destr_pt++)
    {
      tree l_num = build_int_cst (short_unsigned_type_node, lab_num);
      b = build_case_label (l_num, NULL_TREE,
			    create_anon_label_with_ctx (loc, actor));
      add_stmt (b);
      b = build_call_expr_internal_loc (loc, IFN_CO_ACTOR, void_type_node, 1,
					l_num);
      b = build_stmt (loc, EXPR_STMT, b);
      b = maybe_cleanup_point_expr_void (b);
      add_stmt (b);
      b = build1 (GOTO_EXPR, void_type_node, CASE_LABEL (ddeflab));
      add_stmt (b);
      lab_num += 2;
    }

  /* Insert the prototype dspatcher.  */
  finish_switch_stmt (destroy_dispatcher);

  finish_then_clause (lsb_if);

  tree dispatcher = begin_switch_stmt ();
  finish_switch_cond (rat, dispatcher);
  b = build_case_label (build_int_cst (short_unsigned_type_node, 0), NULL_TREE,
			 create_anon_label_with_ctx (loc, actor));
  add_stmt (b);
  b = build1 (GOTO_EXPR, void_type_node, actor_begin_label);
  add_stmt (b);

  tree rdeflab = build_case_label (NULL_TREE, NULL_TREE,
				  create_anon_label_with_ctx (loc, actor));
  add_stmt (rdeflab);
  b = build_call_expr_loc (loc, builtin_decl_explicit (BUILT_IN_TRAP), 0);
  b = build_stmt (loc, EXPR_STMT, b);
  b = maybe_cleanup_point_expr_void (b);
  add_stmt (b);

  lab_num = 2;
  /* The final resume should be made to hit the default (trap, UB) entry.  */
  for (unsigned resu_pt = 0; resu_pt < body_count + 1; resu_pt++)
    {
      tree l_num = build_int_cst (short_unsigned_type_node, lab_num);
      b = build_case_label (l_num, NULL_TREE,
			    create_anon_label_with_ctx (loc, actor));
      add_stmt (b);
      b = build_call_expr_internal_loc (loc, IFN_CO_ACTOR, void_type_node, 1,
					l_num);
      b = build_stmt (loc, EXPR_STMT, b);
      b = maybe_cleanup_point_expr_void (b);
      add_stmt (b);
      b = build1 (GOTO_EXPR, void_type_node, CASE_LABEL (rdeflab));
      add_stmt (b);
      lab_num += 2;
    }

  /* Insert the prototype dspatcher.  */
  finish_switch_stmt (dispatcher);

  finish_if_stmt (lsb_if);

  tree r = build_stmt (loc, LABEL_EXPR, actor_begin_label);
  add_stmt (r);

  /* actor's version of the promise.  */
  tree ap_m = lookup_member (coro_frame_type, get_identifier ("__p"), 1, 0,
			     tf_warning_or_error);
  tree ap = build_class_member_access_expr (actor_frame, ap_m, NULL_TREE,
					    false, tf_warning_or_error);

  /* actor's coroutine 'self handle'.  */
  tree ash_m = lookup_member (coro_frame_type, get_identifier ("__self_h"),
			     1, 0, tf_warning_or_error);
  tree ash = build_class_member_access_expr (actor_frame, ash_m, NULL_TREE,
					     false, tf_warning_or_error);
  /* So construct the self-handle from the frame address.  */
  tree hfa_m = lookup_member (handle_type, get_identifier ("from_address"),
			     1, 0, tf_warning_or_error);

  r = build1 (CONVERT_EXPR, build_pointer_type (void_type_node), actor_fp);
  vec<tree, va_gc>* args = make_tree_vector_single (r);
  tree hfa = build_new_method_call (ash, hfa_m, &args, NULL_TREE, LOOKUP_NORMAL,
				    NULL, tf_warning_or_error);
  r = build2 (INIT_EXPR, handle_type, ash, hfa);
  r = build1 (CONVERT_EXPR, void_type_node, r);
  r = build_stmt (loc, EXPR_STMT, r);
  r = maybe_cleanup_point_expr_void (r);
  add_stmt (r);
  release_tree_vector (args);

  /* Now we know the real promise, and enough about the frame layout to
     decide where to put things.  */

  struct __await_xform_data xform = { actor_frame,
				      promise_proxy, ap,
				      self_h_proxy, ash };

  /* Get a reference to the initial suspend var in the frame.  */
  transform_await_expr (initial_await, &xform);
  r = build_stmt (loc, EXPR_STMT, initial_await);
  add_stmt (r);

  /* Now we've built the promise etc, process fnbody for co_returns.
     We want the call to return_void () below and it has no params so
     we can create it once here.
     Calls to return_value () will have to be checked and created as
     required.  */

  tree return_void = NULL_TREE;
  tree rvm = lookup_promise_member (orig, "return_void",
				   loc, false /*musthave*/);
  if (rvm && rvm != error_mark_node)
    return_void = build_new_method_call (ap, rvm, NULL, NULL_TREE,
					 LOOKUP_NORMAL, NULL,
					 tf_warning_or_error);

  /* co_return branches to the final_suspend label, so declare that now.  */
  tree fs_label = get_identifier ("final_suspend");
  fs_label = define_label (loc, fs_label);
  DECL_CONTEXT (fs_label) = actor;

  /* Expand co_returns in the saved function body  */
  fnbody = expand_co_returns (&fnbody, promise_proxy, ap, fs_label);

  /* Transform the await expressions in the function body.  Only do each
     await tree once!  */
  hash_set<tree> pset;
  cp_walk_tree (&fnbody, transform_await_wrapper, &xform, &pset);

  /* Add in our function body with the co_returns rewritten to final form.  */
  add_stmt (fnbody);

  /* 9.6.3.1 (2.2 : 3) if p.return_void() is a valid expression, flowing
     off the end of a coroutine is equivalent to co_return; otherwise UB.
     We just inject the call to p.return_void() here, and fall through to
     the final_suspend: label (eliding the goto).  */
  if (return_void != NULL_TREE)
    add_stmt (return_void);

  /* Final suspend starts here.  */
  r = build_stmt (loc, LABEL_EXPR, fs_label);
  add_stmt (r);

  /* Set the actor pointer to null, so that 'done' will work.
     Resume from here is UB anyway - although a 'ready' await will
     branch to the final resume, and fall through to the destroy.  */
  tree resume_m = lookup_member (coro_frame_type, get_identifier ("__resume"),
				     /*protect*/1,  /*want_type*/ 0,
				     tf_warning_or_error);
  tree res_x = build_class_member_access_expr (actor_frame, resume_m,
						NULL_TREE, false,
						tf_warning_or_error);
  r = build1 (CONVERT_EXPR, act_des_fn_ptr, integer_zero_node);
  r = build2 (INIT_EXPR, act_des_fn_ptr, res_x, r);
  r = build1 (CONVERT_EXPR, void_type_node, r);
  r = build_stmt (loc, EXPR_STMT, r);
  r = maybe_cleanup_point_expr_void (r);
  add_stmt (r);

  /* Get a reference to the final suspend var in the frame.  */
  transform_await_expr (final_await, &xform);
  r = build_stmt (loc, EXPR_STMT, final_await);
  add_stmt (r);

  /* now do the tail of the function.  */

  tree del_promise_label = get_identifier ("coro.delete.promise");
  del_promise_label = define_label (input_location, del_promise_label);
  DECL_CONTEXT (del_promise_label) = actor;
  r = build_stmt (loc, LABEL_EXPR, del_promise_label);
  add_stmt (r);

  /* Destructors for the things we built explicitly.  */
  r = build_special_member_call(ap, complete_dtor_identifier, NULL,
				promise_type, LOOKUP_NORMAL,
				tf_warning_or_error);
  add_stmt (r);

  tree del_frame_label = get_identifier ("coro.delete.frame");
  del_frame_label = define_label (input_location, del_frame_label);
  DECL_CONTEXT (del_frame_label) = actor;
  r = build_stmt (loc, LABEL_EXPR, del_frame_label);
  add_stmt (r);

  /* Here deallocate the frame (if we allocated it), which we will have at
     present.  */
  tree fnf_m = lookup_member (coro_frame_type,
			      get_identifier ("__frame_needs_free"), 1, 0,
			      tf_warning_or_error);
  tree fnf2_x = build_class_member_access_expr (actor_frame, fnf_m, NULL_TREE,
					       false, tf_warning_or_error);
  tree free_coro_fr
    = build_call_expr_loc (loc,
			   builtin_decl_explicit (BUILT_IN_FREE), 1, actor_fp);
  free_coro_fr = build_stmt (loc, EXPR_STMT, free_coro_fr);
  free_coro_fr = maybe_cleanup_point_expr_void (free_coro_fr);
  tree free_list = NULL;
  append_to_statement_list (free_coro_fr, &free_list);

  tree goto_ret_list = NULL;
  r = build1 (GOTO_EXPR, void_type_node, ret_label);
  append_to_statement_list (r, &goto_ret_list);

  r = build3 (COND_EXPR, void_type_node, fnf2_x, free_list, goto_ret_list);
  r = build_stmt (loc, EXPR_STMT, r);
  r = maybe_cleanup_point_expr_void (r);
  add_stmt (r);

  /* This is the eventual (or suspend) return point.  */
  r = build_stmt (loc, LABEL_EXPR, ret_label);
  add_stmt (r);

  /* done.  */
  r = build_stmt (loc, RETURN_EXPR, NULL);
  TREE_NO_WARNING (r) |= 1; /* We don't want a warning about this.  */
  r = maybe_cleanup_point_expr_void (r);
  add_stmt (r);

  /* We need the resume index to work with.  */
  tree res_idx_m = lookup_member (coro_frame_type, resume_idx_name,
				  /*protect*/1,  /*want_type*/ 0,
				  tf_warning_or_error);
  tree res_idx = build_class_member_access_expr (actor_frame, res_idx_m,
						 NULL_TREE, false,
						 tf_warning_or_error);

  /* We've now rewritten the tree and added the initial and final
     co_awaits.  Now pass over the tree and expand the co_awaits.  */
  actor_body = expand_co_awaits (actor, &actor_body, actor_fp, res_idx,
				 del_promise_label, ret_label, ash);

  actor_body = pop_stmt_list (actor_body);
  BIND_EXPR_BODY (actor_bind) = actor_body;

  finish_compound_stmt (stmt);
  DECL_SAVED_TREE (actor) = pop_stmt_list (actor_outer);
  verify_stmt_tree (DECL_SAVED_TREE (actor));
}

/* The prototype 'destroy' function :
   frame->__resume_at |= 1;
   actor (frame);
*/
static void
build_destroy_fn (location_t loc, tree coro_frame_type,
		  tree destroy, tree actor)
{
  /* One param, the coro frame pointer.  */
  tree coro_frame_ptr = build_pointer_type (coro_frame_type);
  tree destr_fp = build_lang_decl (PARM_DECL, get_identifier ("frame_ptr"),
  				   coro_frame_ptr);
  DECL_CONTEXT (destr_fp) = destroy;
  DECL_ARG_TYPE (destr_fp) = type_passed_as (coro_frame_ptr);
  DECL_ARGUMENTS (destroy) = destr_fp;

  /* A void return.  */
  tree resdecl = build_decl (loc, RESULT_DECL, 0, void_type_node);
  DECL_ARTIFICIAL (resdecl) = 1;
  DECL_IGNORED_P (resdecl) = 1;
  DECL_RESULT (destroy) = resdecl;
  TREE_STATIC (destroy) = 1;

  tree destr_outer = push_stmt_list ();
  current_stmt_tree ()->stmts_are_full_exprs_p = 1;
  tree dstr_stmt = begin_compound_stmt (BCS_FN_BODY);

  tree destr_bind = build3 (BIND_EXPR, void_type_node, NULL, NULL, NULL);
  add_stmt (destr_bind);
  tree destr_body = push_stmt_list ();

  tree destr_frame = build1 (INDIRECT_REF, coro_frame_type, destr_fp);

  tree resume_idx_name = get_identifier ("__resume_at");
  tree rat_field = lookup_member (coro_frame_type, resume_idx_name, 1, 0,
				  tf_warning_or_error);
  tree rat = build3 (COMPONENT_REF, short_unsigned_type_node, destr_frame,
		     rat_field, NULL_TREE);

  /* _resume_at |= 1 */
  tree dstr_idx = build2 (BIT_IOR_EXPR, short_unsigned_type_node, rat,
			  build_int_cst (short_unsigned_type_node, 1));
  tree r = build2 (MODIFY_EXPR, short_unsigned_type_node, rat, dstr_idx);
  r = build1 (CONVERT_EXPR, void_type_node, r);
  r = build_stmt (loc, EXPR_STMT, r);
  r = maybe_cleanup_point_expr_void (r);
  add_stmt (r);

  /* So .. call the actor ..  */
  r = build_call_expr_loc (loc, actor, 1, destr_fp);
  r = build_stmt (loc, EXPR_STMT, r);
  r = maybe_cleanup_point_expr_void (r);
  add_stmt (r);

  /* done. */
  r = build_stmt (loc, RETURN_EXPR, NULL);
  r = maybe_cleanup_point_expr_void (r);
  add_stmt (r);

  destr_body = pop_stmt_list (destr_body);
  BIND_EXPR_BODY (destr_bind) = destr_body;

  finish_compound_stmt (dstr_stmt);
  DECL_SAVED_TREE (destroy) = pop_stmt_list (destr_outer);
}

/* Helper that returns an identifier for an appended extension to the
   current un-mangled function name.  */
static tree
get_fn_local_identifier (tree orig, const char *append)
{
  /* Figure out the bits we need to generate names for the outlined things
     For consistency this needs to behave the same way as
     ASM_FORMAT_PRIVATE_NAME does. */
  tree nm = DECL_NAME (orig);
  const char *sep, *pfx = "";
#ifndef NO_DOT_IN_LABEL
  sep = ".";
#else
# ifndef NO_DOLLAR_IN_LABEL
  sep = "$"
# else
  sep = "_";
  pfx = "__";
# endif
#endif

  char *an = ACONCAT ((pfx, IDENTIFIER_POINTER (nm), sep, append, (char*)0));
  return get_identifier (an);
}

static tree
build_init_or_final_await (location_t loc, bool is_final)
{
  const char *suspend_alt = is_final ? "final_suspend" : "initial_suspend";
  tree setup_meth = lookup_promise_member (current_function_decl, suspend_alt,
					   loc, true /*musthave*/);
  if (!setup_meth || setup_meth == error_mark_node)
    return error_mark_node;

  tree s_fn = NULL_TREE;
  tree setup_call = build_new_method_call
    (DECL_COROUTINE_PROMISE_PROXY (current_function_decl), setup_meth,  NULL,
     NULL_TREE, LOOKUP_NORMAL, &s_fn, tf_warning_or_error);

  if (!s_fn || setup_call == error_mark_node)
    return error_mark_node;

  /* So build the co_await for this */
  /* For initial/final suspends the call is is "a" per 8.3.8 3.1.  */

  tree point = build_int_cst (integer_type_node,  (is_final ? 3 : 2));
  return build_co_await (loc, setup_call, point);
}


static bool
register_await_info (tree await_expr, tree aw_type, tree aw_nam, tree h_nam)
{
  bool seen;
  struct suspend_point_info &s
    = suspend_points->get_or_insert (await_expr, &seen);
  if (seen)
    {
      error_at (EXPR_LOCATION (await_expr), "duplicate info for %qE",
		await_expr);
      debug_tree (await_expr);
      return false;
    }
  s.awaitable_type = aw_type;
  s.await_field_id = aw_nam;
  s.save_handle_id = h_nam;
  return true;
}

struct __susp_frame_data {
  tree *field_list;
  tree handle_type;
  unsigned count;
};

/* If this is an await, then register it and decide on what coro
   frame storage is needed.
   If this is a co_yield (which embeds an await), drop the yield
   and record the await (the yield was kept for diagnostics only.  */
static tree
register_awaits (tree *stmt, int *do_subtree ATTRIBUTE_UNUSED, void *d)
{
  struct __susp_frame_data *data = (struct __susp_frame_data *) d;

  if (TREE_CODE (*stmt) != CO_AWAIT_EXPR
      && TREE_CODE (*stmt) != CO_YIELD_EXPR)
    return NULL_TREE;

  size_t bufsize = sizeof ("__aw_s.") + 10;
  char *buf = (char *) alloca (bufsize);
  snprintf (buf, bufsize, "__aw_s.%d", data->count);
  tree aw_field_nam = get_identifier (buf);
  /* TODO: be more intelligent about whether we need a coro handle
     saved across the suspend.  */
  snprintf (buf, bufsize, "__aw_h.%d", data->count);
  tree handle_field_nam = get_identifier (buf);

  tree aw_expr = *stmt;
  location_t aw_loc = EXPR_LOCATION (aw_expr); /* location of the co_xxxx.  */
  if (TREE_CODE (aw_expr) == CO_YIELD_EXPR)
    {
      aw_expr = TREE_OPERAND (aw_expr, 1);
      *stmt = aw_expr; //build_stmt (aw_loc, EXPR_STMT, aw_expr);
    }

  /* The required field has the same type as the proxy stored in the
      await expr.  */
  tree aw_field_type = TREE_TYPE (TREE_OPERAND (aw_expr, 1));

  tree decl = build_decl (aw_loc, FIELD_DECL, aw_field_nam, aw_field_type);
  DECL_CHAIN (decl) = *data->field_list; *data->field_list = decl;

  decl = build_decl (aw_loc, FIELD_DECL, handle_field_nam, data->handle_type);
  DECL_CHAIN (decl) = *data->field_list; *data->field_list = decl;

  register_await_info (aw_expr, aw_field_type, aw_field_nam, handle_field_nam);

  data->count++;
  return NULL_TREE;
}

/* Here we:
   a) Check that the function and promise type are valid for a
      coroutine.
   b) Carry out the initial morph to create the skeleton of the
      coroutine ramp function and the rewritten body.

  Assumptions.

  1. We only hit this code once all dependencies are resolved.
  2. The function body will be either a bind expr or a statement list
  3. That cfun and current_function_decl are valid for the case we're
     expanding.
  4. 'input_location' will be of the final brace for the function.

 We do something like this:
 declare a dummy coro frame.
 struct _R_frame {
  using handle_type = coro::coroutine_handle<coro1::promise_type>;
  void (*__resume)(struct _R_frame *);
  void (*__destroy)(struct _R_frame *);
  struct coro1::promise_type __p;
  bool frame_needs_free; // free the coro frame mem if set.
  bool __suspended; // set if we're suspended (use for lib. check mode).
  short __resume_at; // this is where clang puts it - but it's a smaller entity.
  coro1::suspend_never_prt __is;
  (maybe) handle_type i_hand;
  coro1::suspend_always_prt __fs;
  (maybe) handle_type f_hand;
  // here could be args.
  // and then trailing space.
 };

*/
bool
morph_fn_to_coro (tree orig, tree *resumer, tree *destroyer)
{
  if (! orig || TREE_CODE (orig) != FUNCTION_DECL)
    return false;

  gcc_assert (orig == current_function_decl);

  if (! coro_function_valid_p (orig))
    return false;

  /* We can't validly get here with an empty statement list, since there's no
     way for the FE to decide it's a coroutine in the absence of any code.  */
  tree fnbody = pop_stmt_list (DECL_SAVED_TREE (orig));
  if (fnbody == NULL_TREE)
    return false;

  /* We don't have the locus of the opening brace - it's filled in later (and
     there doesn't really seem to be any easy way to get at it).
     The closing brace is assumed to be input_location.  */
  location_t fn_start = DECL_SOURCE_LOCATION (orig);
  gcc_rich_location fn_start_loc (fn_start);

  /* So, we've tied off the original body.  Now start the replacement.
     If we encounter a fatal error we might return a now-empty body.
     TODO: determine if it would help to restore the original.
	   determine if looking for more errors in coro_function_valid_p()
	   and stashing types is a better solution.
  */
  tree newbody = push_stmt_list ();
  DECL_SAVED_TREE (orig) = newbody;

  /* Types we already know.  */

  tree fn_return_type = TREE_TYPE (TREE_TYPE (orig));
  gcc_assert (! VOID_TYPE_P (fn_return_type));
  tree handle_type = DECL_COROUTINE_HANDLE_TYPE(orig);
  tree promise_type = DECL_COROUTINE_PROMISE_TYPE(orig);

  /* Types we need to define or look up.  */

  suspend_points = hash_map<tree, struct suspend_point_info>::create_ggc (11);

  /* Initial and final suspend types are special in that the co_awaits for
     them are synthetic.  We need to find the type for each awaiter from
     the coroutine promise.  */
  tree initial_await = build_init_or_final_await (fn_start, false);
  if (initial_await == error_mark_node)
    return false;
  /* The type of the frame var for this is the type of its temp proxy.  */
  tree initial_suspend_type = TREE_TYPE (TREE_OPERAND (initial_await, 1));

  tree final_await = build_init_or_final_await (fn_start, true);
  if (final_await == error_mark_node)
    return false;
  /* The type of the frame var for this is the type of it's temp proxy.  */
  tree final_suspend_type = TREE_TYPE (TREE_OPERAND (final_await, 1));

  tree coro_frame_type = xref_tag (record_type, get_identifier ("_R_frame"),
				   ts_current, false);
  DECL_CONTEXT (TYPE_NAME (coro_frame_type)) = current_scope ();
  tree coro_frame_ptr = build_pointer_type (coro_frame_type);
  tree act_des_fn_type = build_function_type_list (void_type_node,
						   coro_frame_ptr, NULL_TREE);
  tree act_des_fn_ptr = build_pointer_type (act_des_fn_type);

  /* Declare the actor function.  */
  tree actor_name = get_fn_local_identifier (orig, "actor");
  tree actor = build_lang_decl (FUNCTION_DECL, actor_name, act_des_fn_type);
  DECL_CONTEXT (actor) = DECL_CONTEXT (orig);
  DECL_INITIAL (actor) = error_mark_node;

  /* Declare the destroyer function.  */
  tree destr_name = get_fn_local_identifier (orig, "destroy");
  tree destroy = build_lang_decl (FUNCTION_DECL, destr_name, act_des_fn_type);
  DECL_CONTEXT (destroy) = DECL_CONTEXT (orig);
  DECL_INITIAL (destroy) = error_mark_node;

   /* Build our dummy coro frame layout.  */
  coro_frame_type = begin_class_definition (coro_frame_type);

  tree resume_name = get_identifier ("__resume");
  tree decl = build_decl (fn_start, FIELD_DECL, resume_name, act_des_fn_ptr);
  tree field_list = decl;
  tree destroy_name = get_identifier ("__destroy");
  decl = build_decl (fn_start, FIELD_DECL, destroy_name, act_des_fn_ptr);
  DECL_CHAIN (decl) = field_list; field_list = decl;
  tree promise_name = get_identifier ("__p");
  decl = build_decl (fn_start, FIELD_DECL, promise_name, promise_type);
  DECL_CHAIN (decl) = field_list; field_list = decl;
  tree fnf_name = get_identifier ("__frame_needs_free");
  decl = build_decl (fn_start, FIELD_DECL, fnf_name, boolean_type_node);
  DECL_CHAIN (decl) = field_list; field_list = decl;
  tree susp_name = get_identifier ("__suspended");
  decl = build_decl (fn_start, FIELD_DECL, susp_name, boolean_type_node);
  DECL_CHAIN (decl) = field_list; field_list = decl;
  tree resume_idx_name = get_identifier ("__resume_at");
  decl = build_decl (fn_start, FIELD_DECL, resume_idx_name,
		     short_unsigned_type_node);
  DECL_CHAIN (decl) = field_list; field_list = decl;

  /* We need a handle to this coroutine, which is passed to every
     await_suspend().  There's no point in creating it over and over.  */
  tree self_h_name = get_identifier ("__self_h");
  decl = build_decl (fn_start, FIELD_DECL, self_h_name, handle_type);
  DECL_CHAIN (decl) = field_list; field_list = decl;

  /* Initial suspend is mandated.  */
  tree init_susp_name = get_identifier ("__is");
  decl = build_decl (fn_start, FIELD_DECL, init_susp_name,
		     initial_suspend_type);
  DECL_CHAIN (decl) = field_list; field_list = decl;
  /* We really need to figure this out from the awaiter type.  */
  tree init_hand_name = get_identifier ("__ih");
  decl = build_decl (fn_start, FIELD_DECL, init_hand_name, handle_type);
  DECL_CHAIN (decl) = field_list; field_list = decl;

  register_await_info (initial_await, initial_suspend_type,
		       init_susp_name, init_hand_name);

  /* Now insert the data for any body await points.  */
  struct __susp_frame_data body_aw_points = { &field_list, handle_type, 0 };
  /* we don't want to duplicate.  */
  hash_set<tree> *visited = new hash_set<tree>;
  cp_walk_tree (&fnbody, register_awaits, &body_aw_points, visited);
  delete visited;

  /* Final suspend is mandated.  */
  tree fin_susp_name = get_identifier ("__fs");
  decl = build_decl (fn_start, FIELD_DECL, fin_susp_name, final_suspend_type);
  DECL_CHAIN (decl) = field_list; field_list = decl;
  tree fin_hand_name = get_identifier ("__fh");
  decl = build_decl (fn_start, FIELD_DECL, fin_hand_name, handle_type);
  DECL_CHAIN (decl) = field_list; field_list = decl;

  register_await_info (final_await, final_suspend_type,
		       fin_susp_name, fin_hand_name);

  TYPE_FIELDS (coro_frame_type) = field_list;

  TYPE_BINFO (coro_frame_type) = make_tree_binfo (0);
  BINFO_OFFSET (TYPE_BINFO (coro_frame_type)) = size_zero_node;
  BINFO_TYPE (TYPE_BINFO (coro_frame_type)) = coro_frame_type;

  coro_frame_type = finish_struct (coro_frame_type, NULL_TREE);

  /* Ramp: */
  tree ramp_label = get_identifier ("ramp.start");
  ramp_label = define_label (fn_start, ramp_label);
  
  ramp_label = build_stmt (fn_start, LABEL_EXPR, ramp_label);
  add_stmt (ramp_label);

  /* Now build the ramp function pieces.  */
  tree ramp_bind = build3 (BIND_EXPR, void_type_node, NULL, NULL, NULL);
  add_stmt (ramp_bind);
  tree ramp_body = push_stmt_list ();
  tree empty_list = build_empty_stmt (fn_start);

  tree coro_fp = build_lang_decl (VAR_DECL, get_identifier ("coro.frameptr"),
				  coro_frame_ptr);
  DECL_CONTEXT (coro_fp) = current_scope ();
  tree r = build_stmt (fn_start, DECL_EXPR, coro_fp);
  add_stmt (r);
  tree varlist = coro_fp;

  /* Collected the scope vars we need ... only one for now. */
  BIND_EXPR_VARS (ramp_bind) = nreverse (varlist);

  /* Allocate the frame.  This is the "real version"...
  tree allocated
    = build_call_expr_internal_loc (fn_start, IFN_CO_FRAME,
				    build_pointer_type (void_type_node), 1,
				    coro_fp);
  .. and this is a temporary one until we do heap allocation elision. */
  tree allocated
    = build_call_expr_loc (fn_start,
			   builtin_decl_explicit (BUILT_IN_MALLOC), 1,
			   TYPE_SIZE_UNIT (coro_frame_type));
  allocated = build1 (CONVERT_EXPR, coro_frame_ptr, allocated);
  r = build2 (INIT_EXPR, TREE_TYPE (coro_fp), coro_fp, allocated);
  r = build_stmt (fn_start, EXPR_STMT, r);
  r = maybe_cleanup_point_expr_void (r);
  add_stmt (r);

  /* Test for NULL and quit.  */

  tree cfra_label = get_identifier ("coro.frame.active");
  cfra_label = define_label (fn_start, cfra_label);

  tree early_ret_list = NULL;
  r = build_stmt (input_location, RETURN_EXPR, NULL);
  TREE_NO_WARNING (r) |= 1; /* We don't want a warning about this.  */
  r = maybe_cleanup_point_expr_void (r);
  append_to_statement_list (r, &early_ret_list);

  tree goto_st = NULL;
  r = build1 (GOTO_EXPR, void_type_node, cfra_label);
  append_to_statement_list (r, &goto_st);

  tree ckk = build1 (CONVERT_EXPR, coro_frame_ptr, integer_zero_node);
  tree ckz = build2 (EQ_EXPR, boolean_type_node, coro_fp, ckk);
  r = build3 (COND_EXPR, void_type_node, ckz, early_ret_list, empty_list);
  add_stmt (r);

  cfra_label = build_stmt (fn_start, LABEL_EXPR, cfra_label);
  add_stmt (cfra_label);

  /* deref the frame pointer, to use in member access code.  */
  tree deref_fp = build_x_arrow (fn_start, coro_fp, tf_warning_or_error);

  /* For now, we always assume that this needs destruction, there's no impl.
     for frame allocation elision.  */
  tree fnf_m = lookup_member (coro_frame_type, fnf_name, 1, 0,
			      tf_warning_or_error);
  tree fnf_x = build_class_member_access_expr (deref_fp, fnf_m, NULL_TREE,
					       false, tf_warning_or_error);
  r = build2 (INIT_EXPR, boolean_type_node, fnf_x, boolean_true_node);
  r = build1 (CONVERT_EXPR, void_type_node, r);
  r = build_stmt (fn_start, EXPR_STMT, r);
  r = maybe_cleanup_point_expr_void (r);
  add_stmt (r);

  /* We're not suspended.  */
  tree susp_m = lookup_member (coro_frame_type, susp_name, 1, 0,
			      tf_warning_or_error);
  tree susp_x = build_class_member_access_expr (deref_fp, susp_m, NULL_TREE,
					       false, tf_warning_or_error);
  r = build2 (INIT_EXPR, boolean_type_node, susp_x, boolean_false_node);
  r = build1 (CONVERT_EXPR, void_type_node, r);
  r = build_stmt (fn_start, EXPR_STMT, r);
  r = maybe_cleanup_point_expr_void (r);
  add_stmt (r);

  /* Put the resumer and destroyer functions in.  */

  tree actor_addr = build1 (ADDR_EXPR, act_des_fn_ptr, actor);
  tree resume_m = lookup_member (coro_frame_type, resume_name,
				     /*protect*/1,  /*want_type*/ 0,
				     tf_warning_or_error);
  tree resume_x = build_class_member_access_expr (deref_fp, resume_m,
						  NULL_TREE, false,
						  tf_warning_or_error);
  r = build2 (INIT_EXPR, act_des_fn_ptr, resume_x, actor_addr);
  r = build1 (CONVERT_EXPR, void_type_node, r);
  r = build_stmt (fn_start, EXPR_STMT, r);
  r = maybe_cleanup_point_expr_void (r);
  add_stmt (r);

  tree destroy_addr = build1 (ADDR_EXPR, act_des_fn_ptr, destroy);
  tree destroy_m = lookup_member (coro_frame_type, destroy_name,
				     /*protect*/1,  /*want_type*/ 0,
				     tf_warning_or_error);
  tree destroy_x = build_class_member_access_expr (deref_fp, destroy_m,
						  NULL_TREE, false,
						  tf_warning_or_error);
  r = build2 (INIT_EXPR, act_des_fn_ptr, destroy_x, destroy_addr);
  r = build1 (CONVERT_EXPR, void_type_node, r);
  r = build_stmt (fn_start, EXPR_STMT, r);
  r = maybe_cleanup_point_expr_void (r);
  add_stmt (r);

  /* Set up the promise.  */
  tree promise_m = lookup_member (coro_frame_type, promise_name,
				  /*protect*/1,  /*want_type*/ 0,
				  tf_warning_or_error);

  tree p = build_class_member_access_expr (deref_fp, promise_m, NULL_TREE,
					   false, tf_warning_or_error);

  /* Do a placement new constructor for the promise type (we never call the
     new operator, just the constructor on the object in place in the frame.
     
     First try to find a constructor with the same parameter list as the
     original function (if it has params), failing that find a constructor
     with no parameter list.
  */

  if (DECL_ARGUMENTS (orig))
    {
      vec<tree, va_gc>* args = make_tree_vector ();
      tree arg;
      for (arg = DECL_ARGUMENTS (orig); arg != NULL; arg = DECL_CHAIN (arg))
        vec_safe_push (args, arg);
      r = build_special_member_call(p, complete_ctor_identifier, &args,
				    promise_type, LOOKUP_NORMAL,
				    tf_none);
      release_tree_vector (args);
    }
  else
    r = NULL_TREE;
    
  if (r == NULL_TREE || r == error_mark_node)
    r = build_special_member_call(p, complete_ctor_identifier, NULL,
				  promise_type, LOOKUP_NORMAL,
				  tf_warning_or_error);
  add_stmt (r);

  /* Set up a new bind context for the GRO.  */
  tree gro_context_bind = build3 (BIND_EXPR, void_type_node, NULL, NULL, NULL);
  add_stmt (gro_context_bind);

  tree gro_meth = lookup_promise_member (orig, "get_return_object",
					 fn_start, true /*musthave*/);
  tree get_ro = build_new_method_call (p, gro_meth, NULL, NULL_TREE,
				       LOOKUP_NORMAL, NULL,
				       tf_warning_or_error);
  /* Without a return object we haven't got much clue what's going on.  */
  if (get_ro == error_mark_node)
    {
      BIND_EXPR_BODY (ramp_bind) = pop_stmt_list (ramp_body);
      DECL_SAVED_TREE (orig) = newbody;
      return false;
    }

  tree gro_context_body = push_stmt_list ();
  tree gro = build_lang_decl (VAR_DECL, get_identifier ("coro.gro"),
			      TREE_TYPE (TREE_OPERAND (get_ro, 0)));
  DECL_CONTEXT (gro) = current_scope ();
  r = build_stmt (fn_start, DECL_EXPR, gro);
  add_stmt (r);
  tree gro_bind_vars = gro;

  // init our actual var.
  r = build2 (INIT_EXPR, TREE_TYPE (gro), gro, get_ro);
  r = build1 (CONVERT_EXPR, void_type_node, r);
  r = build_stmt (fn_start, EXPR_STMT, r);
  r = maybe_cleanup_point_expr_void (r);
  add_stmt (r);

  /* Initialise the resume_idx_name to 0, meaning "not started".  */
  tree resume_idx_m = lookup_member (coro_frame_type, resume_idx_name,
				     /*protect*/1,  /*want_type*/ 0,
				     tf_warning_or_error);
  tree resume_idx = build_class_member_access_expr (deref_fp, resume_idx_m,
						    NULL_TREE, false,
						    tf_warning_or_error);
  r = build_int_cst (short_unsigned_type_node, 0);
  r = build2 (INIT_EXPR, short_unsigned_type_node, resume_idx, r);
  r = build1 (CONVERT_EXPR, void_type_node, r);
  r = build_stmt (fn_start, EXPR_STMT, r);
  r = maybe_cleanup_point_expr_void (r);
  add_stmt (r);

  /* So .. call the actor ..  */
  r = build_call_expr_loc (fn_start, actor, 1, coro_fp);
  r = maybe_cleanup_point_expr_void (r);
  add_stmt (r);

  /* done, we just need the return value.  */
  bool no_warning;
  if (same_type_p (TREE_TYPE (gro), fn_return_type))
     /* Already got it.  */
    r = check_return_expr (rvalue (gro), &no_warning);
  else
    {
      // construct the return value with a single GRO param.
      vec<tree, va_gc> *args = make_tree_vector_single (gro);
      r = build_special_member_call (DECL_RESULT (orig),
				     complete_ctor_identifier, &args,
				     fn_return_type, LOOKUP_NORMAL,
				     tf_warning_or_error);
      r = build1 (CONVERT_EXPR, void_type_node, r);
      r = build_stmt (fn_start, EXPR_STMT, r);
      r = maybe_cleanup_point_expr_void (r);
      add_stmt (r);
      release_tree_vector (args);
      // We know it's the correct type.
      r = DECL_RESULT (orig);
    }

  r = build_stmt (input_location, RETURN_EXPR, r);
  TREE_NO_WARNING (r) |= no_warning;
  r = maybe_cleanup_point_expr_void (r);
  add_stmt (r);

#if CLANG_DOES_THIS_BUT_IT_SEEMS_UNREACHABLE
  tree del_gro_label = get_identifier ("coro.destroy.retval");
  del_gro_label = define_label (input_location, del_gro_label);
  r = build_stmt (input_location, LABEL_EXPR, del_gro_label);
  add_stmt (r);

  r = build_special_member_call(gro, complete_dtor_identifier, NULL,
				fn_return_type, LOOKUP_NORMAL,
				tf_warning_or_error);
  add_stmt (r);
#endif
  BIND_EXPR_VARS (gro_context_bind) = gro_bind_vars;
  BIND_EXPR_BODY (gro_context_bind) = pop_stmt_list (gro_context_body);
  BIND_EXPR_BODY (ramp_bind) = pop_stmt_list (ramp_body);

  /* We know the "real" promise and have a frame layout with a slot for each
     suspend point.  */

  /* We do this to avoid these routines being seen as nested by the middle
     end.  */

  push_deferring_access_checks (dk_no_check);

  /* Actor...  */
  build_actor_fn (fn_start, coro_frame_type, actor, fnbody, orig,
		  initial_await, final_await, body_aw_points.count);
  
  /* Destroyer ... */
  build_destroy_fn (fn_start, coro_frame_type, destroy, actor);

  pop_deferring_access_checks ();

  DECL_SAVED_TREE (orig) = newbody;
  *resumer = actor;
  *destroyer = destroy;
  return true;
}
