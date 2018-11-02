/* coroutine-specific state, expansions and tests.

   Copyright (C) 2018 Free Software Foundation, Inc.

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

/* Lookup the coroutine_traits template decl.
   Instantiate that for the function signature.
   Look for the promise_type in the instantiated.  */

static tree
find_promise_type (location_t kw)
{
  tree h_type;
  tree promise_type;
  unsigned p;
  /* we want std::experimental::coroutine_traits class template decl.  */
  tree exp_name = get_identifier ("experimental");
  tree traits_name = get_identifier ("coroutine_traits");
  tree promise_name = get_identifier ("promise_type");
  tree functyp = TREE_TYPE (current_function_decl);
  tree arg_node = TYPE_ARG_TYPES (functyp);
  tree targ = make_tree_vec (list_length (arg_node));
  tree exp_ns = lookup_qualified_name (std_node, exp_name, 0, false, false);

  if (exp_ns == error_mark_node)
    {
      error_at (kw, "std::experimental not found");
      return NULL_TREE;
    }

  /* So now build up a type list for the template.  The list length includes a
     terminating 'void' which we don't want - but we use that slot for the fn
     return type (which we do list even if it's 'void').  */
  TREE_VEC_ELT (targ, 0) = TYPE_MAIN_VARIANT (TREE_TYPE (functyp));
  p = 1;
  while (arg_node != NULL_TREE
         && !VOID_TYPE_P (TREE_VALUE (arg_node)))
    {
      TREE_VEC_ELT (targ, p++) = TREE_VALUE (arg_node);
      arg_node = TREE_CHAIN (arg_node);
    }

  h_type = lookup_template_class (traits_name, targ,
				  /* in_decl */ NULL_TREE,
				  /* context */ exp_ns,
				  /* entering scope */ false, tf_none);

  if (h_type == error_mark_node)
    {
      error_at (kw, "couldn't instantiate coroutine_traits");
      return NULL_TREE;
    }

  promise_type = lookup_member (h_type, promise_name, /* protect */1,
				/*want_type=*/ true, tf_warning_or_error);
  if (promise_type)
    promise_type = TREE_TYPE (promise_type);

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
    DECL_COROUTINE_PROMISE_TYPE(fndecl) = find_promise_type (loc);

  if (DECL_COROUTINE_PROMISE_TYPE(fndecl) == NULL_TREE)
    {
      error_at (loc, "unable to find the promise type for this coroutine");
      return false;
    }
  return true;
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

  if (current_function_auto_return_pattern)
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
  gcc_assert (DECL_COROUTINE_PROMISE_TYPE(fndecl) != NULL_TREE);

  if (current_function_returns_value || current_function_returns_null)
    /* TODO: file or extract positions of returns (and the first coro
       keyword so that we can add notes to the diagnostic about where
       the bad keyword is and what mad eit into a coro.  */
    error_at (f_loc, "return statement not allowed in coroutine;"
                     " did you mean %<co_return%>?" );

  return true;
}

/* Here we:
   a) Check that the function and promise type are valid for a
      coroutine.
   b) Carry out the initial morph to create the skeleton of the
      coroutine ramp function and the rewritten body.
*/
tree morph_fn_to_coro (tree orig)
{
  if (!coro_function_valid_p (orig))
    return NULL_TREE;

  gcc_assert (orig == current_function_decl);
  return orig;
}

bool
co_await_context_valid_p (location_t kw, tree expr)
{
  if (! coro_common_keyword_context_valid_p (current_function_decl, kw,
					   "co_await"))
    return false;

  if (! coro_promise_type_found_p (current_function_decl, kw))
    return false;

  if (expr == NULL_TREE)
    {
      error_at (kw, "co_await requires an expression." );
      return false;
    }

  /* FIXME: we can probably do more here.  */
  return true;
}

/* Check that it's valid to have a co_return keyword here.
   True if this is a valid context (we don't check the content
   of the expr - except to decide if the promise_type needs as
   return_void() or a return_value().  */

bool
co_return_context_valid_p (location_t kw, tree expr)
{
  if (! coro_common_keyword_context_valid_p (current_function_decl, kw,
					   "co_return"))
    return false;

  if (! coro_promise_type_found_p (current_function_decl, kw))
    return false;

  /* If the promise object doesn't have the correct return call then
     there's a mis-match between the co_return <expr> and this.  */
  if (expr == NULL_TREE)
    {
      /* Need to check for the result_void() promise member.  */
      tree r_void_name = get_identifier ("return_void");
      tree r_void_memb = lookup_member (DECL_COROUTINE_PROMISE_TYPE(current_function_decl),
					r_void_name, /*protect*/1,  /*want_type*/ 0,
					tf_warning_or_error);
      if (r_void_memb == NULL_TREE || r_void_memb == error_mark_node)
	{
	  error_at (kw, "no member named %<return_void%> in %qT",
		    DECL_COROUTINE_PROMISE_TYPE(current_function_decl));
	  return false;
	}
    }
  else
    {
      /* Need to check for the result_value() promise member.  */
      tree r_value_name = get_identifier ("return_value");
      tree r_valu_memb = lookup_member (DECL_COROUTINE_PROMISE_TYPE(current_function_decl),
					r_value_name, /*protect*/1,  /*want_type*/ 0,
					tf_warning_or_error);
      if (r_valu_memb == NULL_TREE || r_valu_memb == error_mark_node)
	{
	  error_at (kw, "no member named %<return_value%> in %qT",
		    DECL_COROUTINE_PROMISE_TYPE(current_function_decl));
	  return false;
	}
    }

  /* Makes no sense for a co-routine really. */
  if (TREE_THIS_VOLATILE (current_function_decl))
    warning_at (kw, 0, "function declared %<noreturn%> has a"
		" %<co_return%> statement");

  return true;
}
