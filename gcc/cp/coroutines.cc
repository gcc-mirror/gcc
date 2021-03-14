/* coroutine-specific state, expansions and tests.

   Copyright (C) 2018-2020 Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "cp-tree.h"
#include "stringpool.h"
#include "stmt.h"
#include "stor-layout.h"
#include "tree-iterator.h"
#include "tree.h"
#include "gcc-rich-location.h"
#include "hash-map.h"

static bool coro_promise_type_found_p (tree, location_t);

/* GCC C++ coroutines implementation.

  The user authors a function that becomes a coroutine (lazily) by
  making use of any of the co_await, co_yield or co_return keywords.

  Unlike a regular function, where the activation record is placed on the
  stack, and is destroyed on function exit, a coroutine has some state that
  persists between calls - the coroutine frame (analogous to a stack frame).

  We transform the user's function into three pieces:
  1. A so-called ramp function, that establishes the coroutine frame and
     begins execution of the coroutine.
  2. An actor function that contains the state machine corresponding to the
     user's suspend/resume structure.
  3. A stub function that calls the actor function in 'destroy' mode.

  The actor function is executed:
   * from "resume point 0" by the ramp.
   * from resume point N ( > 0 ) for handle.resume() calls.
   * from the destroy stub for destroy point N for handle.destroy() calls.

  The functions in this file carry out the necessary analysis of, and
  transforms to, the AST to perform this.

  The C++ coroutine design makes use of some helper functions that are
  authored in a so-called "promise" class provided by the user.

  At parse time (or post substitution) the type of the coroutine promise
  will be determined.  At that point, we can look up the required promise
  class methods and issue diagnostics if they are missing or incorrect.  To
  avoid repeating these actions at code-gen time, we make use of temporary
  'proxy' variables for the coroutine handle and the promise - which will
  eventually be instantiated in the coroutine frame.

  Each of the keywords will expand to a code sequence (although co_yield is
  just syntactic sugar for a co_await).

  We defer the analysis and transformation until template expansion is
  complete so that we have complete types at that time.  */


/* The state that we collect during parsing (and template expansion) for
   a coroutine.  */

struct GTY((for_user)) coroutine_info
{
  tree function_decl; /* The original function decl.  */
  tree promise_type; /* The cached promise type for this function.  */
  tree handle_type;  /* The cached coroutine handle for this function.  */
  tree self_h_proxy; /* A handle instance that is used as the proxy for the
			one that will eventually be allocated in the coroutine
			frame.  */
  tree promise_proxy; /* Likewise, a proxy promise instance.  */
  tree return_void;   /* The expression for p.return_void() if it exists.  */
  location_t first_coro_keyword; /* The location of the keyword that made this
				    function into a coroutine.  */
  /* Flags to avoid repeated errors for per-function issues.  */
  bool coro_ret_type_error_emitted;
  bool coro_promise_error_emitted;
  bool coro_co_return_error_emitted;
};

struct coroutine_info_hasher : ggc_ptr_hash<coroutine_info>
{
  typedef tree compare_type; /* We only compare the function decl.  */
  static inline hashval_t hash (coroutine_info *);
  static inline hashval_t hash (const compare_type &);
  static inline bool equal (coroutine_info *, coroutine_info *);
  static inline bool equal (coroutine_info *, const compare_type &);
};

/* This table holds all the collected coroutine state for coroutines in
   the current translation unit.  */

static GTY (()) hash_table<coroutine_info_hasher> *coroutine_info_table;

/* We will initialise state lazily.  */
static bool coro_initialized = false;

/* Return a hash value for the entry pointed to by INFO.
   The compare type is a tree, but the only trees we are going use are
   function decls.  We use the DECL_UID as the hash value since that is
   stable across PCH.  */

hashval_t
coroutine_info_hasher::hash (coroutine_info *info)
{
  return DECL_UID (info->function_decl);
}

/* Return a hash value for the compare value COMP.  */

hashval_t
coroutine_info_hasher::hash (const compare_type& comp)
{
  return DECL_UID (comp);
}

/* Return true if the entries pointed to by LHS and RHS are for the
   same coroutine.  */

bool
coroutine_info_hasher::equal (coroutine_info *lhs, coroutine_info *rhs)
{
  return lhs->function_decl == rhs->function_decl;
}

bool
coroutine_info_hasher::equal (coroutine_info *lhs, const compare_type& rhs)
{
  return lhs->function_decl == rhs;
}

/* Get the existing coroutine_info for FN_DECL, or insert a new one if the
   entry does not yet exist.  */

coroutine_info *
get_or_insert_coroutine_info (tree fn_decl)
{
  gcc_checking_assert (coroutine_info_table != NULL);

  coroutine_info **slot = coroutine_info_table->find_slot_with_hash
    (fn_decl, coroutine_info_hasher::hash (fn_decl), INSERT);

  if (*slot == NULL)
    {
      *slot = new (ggc_cleared_alloc<coroutine_info> ()) coroutine_info ();
      (*slot)->function_decl = fn_decl;
    }

  return *slot;
}

/* Get the existing coroutine_info for FN_DECL, fail if it doesn't exist.  */

coroutine_info *
get_coroutine_info (tree fn_decl)
{
  if (coroutine_info_table == NULL)
    return NULL;

  coroutine_info **slot = coroutine_info_table->find_slot_with_hash
    (fn_decl, coroutine_info_hasher::hash (fn_decl), NO_INSERT);
  if (slot)
    return *slot;
  return NULL;
}

/* We will lazily create all the identifiers that are used by coroutines
   on the first attempt to lookup the traits.  */

/* Identifiers that are used by all coroutines.  */

static GTY(()) tree coro_traits_identifier;
static GTY(()) tree coro_handle_identifier;
static GTY(()) tree coro_promise_type_identifier;

/* Required promise method name identifiers.  */

static GTY(()) tree coro_await_transform_identifier;
static GTY(()) tree coro_initial_suspend_identifier;
static GTY(()) tree coro_final_suspend_identifier;
static GTY(()) tree coro_return_void_identifier;
static GTY(()) tree coro_return_value_identifier;
static GTY(()) tree coro_yield_value_identifier;
static GTY(()) tree coro_resume_identifier;
static GTY(()) tree coro_address_identifier;
static GTY(()) tree coro_from_address_identifier;
static GTY(()) tree coro_get_return_object_identifier;
static GTY(()) tree coro_gro_on_allocation_fail_identifier;
static GTY(()) tree coro_unhandled_exception_identifier;

/* Awaitable methods.  */

static GTY(()) tree coro_await_ready_identifier;
static GTY(()) tree coro_await_suspend_identifier;
static GTY(()) tree coro_await_resume_identifier;

/* Create the identifiers used by the coroutines library interfaces.  */

static void
coro_init_identifiers ()
{
  coro_traits_identifier = get_identifier ("coroutine_traits");
  coro_handle_identifier = get_identifier ("coroutine_handle");
  coro_promise_type_identifier = get_identifier ("promise_type");

  coro_await_transform_identifier = get_identifier ("await_transform");
  coro_initial_suspend_identifier = get_identifier ("initial_suspend");
  coro_final_suspend_identifier = get_identifier ("final_suspend");
  coro_return_void_identifier = get_identifier ("return_void");
  coro_return_value_identifier = get_identifier ("return_value");
  coro_yield_value_identifier = get_identifier ("yield_value");
  coro_resume_identifier = get_identifier ("resume");
  coro_address_identifier = get_identifier ("address");
  coro_from_address_identifier = get_identifier ("from_address");
  coro_get_return_object_identifier = get_identifier ("get_return_object");
  coro_gro_on_allocation_fail_identifier =
    get_identifier ("get_return_object_on_allocation_failure");
  coro_unhandled_exception_identifier = get_identifier ("unhandled_exception");

  coro_await_ready_identifier = get_identifier ("await_ready");
  coro_await_suspend_identifier = get_identifier ("await_suspend");
  coro_await_resume_identifier = get_identifier ("await_resume");
}

/* Trees we only need to set up once.  */

static GTY(()) tree coro_traits_templ;
static GTY(()) tree coro_handle_templ;
static GTY(()) tree void_coro_handle_type;

/* ================= Parse, Semantics and Type checking ================= */

/* This initial set of routines are helper for the parsing and template
   expansion phases.

   At the completion of this, we will have completed trees for each of the
   keywords, but making use of proxy variables for the self-handle and the
   promise class instance.  */

/* [coroutine.traits]
   Lookup the coroutine_traits template decl.  */

static tree
find_coro_traits_template_decl (location_t kw)
{
  /* If we are missing fundmental information, such as the traits, (or the
     declaration found is not a type template), then don't emit an error for
     every keyword in a TU, just do it once.  */
  static bool traits_error_emitted = false;

  tree traits_decl = lookup_qualified_name (std_node, coro_traits_identifier,
					    0,
					    /*complain=*/!traits_error_emitted);
  if (traits_decl == error_mark_node
      || !DECL_TYPE_TEMPLATE_P (traits_decl))
    {
      if (!traits_error_emitted)
	{
	  gcc_rich_location richloc (kw);
	  error_at (&richloc, "coroutines require a traits template; cannot"
		    " find %<%E::%E%>", std_node, coro_traits_identifier);
	  inform (&richloc, "perhaps %<#include <coroutine>%> is missing");
	  traits_error_emitted = true;
	}
      return NULL_TREE;
    }
  else
    return traits_decl;
}

/*  Instantiate Coroutine traits for the function signature.  */

static tree
instantiate_coro_traits (tree fndecl, location_t kw)
{
  /* [coroutine.traits.primary]
     So now build up a type list for the template <typename _R, typename...>.
     The types are the function's arg types and _R is the function return
     type.  */

  tree functyp = TREE_TYPE (fndecl);
  tree arg = DECL_ARGUMENTS (fndecl);
  tree arg_node = TYPE_ARG_TYPES (functyp);
  tree argtypes = make_tree_vec (list_length (arg_node)-1);
  unsigned p = 0;

  while (arg_node != NULL_TREE && !VOID_TYPE_P (TREE_VALUE (arg_node)))
    {
      if (is_this_parameter (arg)
	  || DECL_NAME (arg) == closure_identifier)
	{
	  /* We pass a reference to *this to the param preview.  */
	  tree ct = TREE_TYPE (TREE_TYPE (arg));
	  TREE_VEC_ELT (argtypes, p++) = cp_build_reference_type (ct, false);
	}
      else
	TREE_VEC_ELT (argtypes, p++) = TREE_VALUE (arg_node);

      arg_node = TREE_CHAIN (arg_node);
      arg = DECL_CHAIN (arg);
    }

  tree argtypepack = cxx_make_type (TYPE_ARGUMENT_PACK);
  SET_ARGUMENT_PACK_ARGS (argtypepack, argtypes);

  tree targ = make_tree_vec (2);
  TREE_VEC_ELT (targ, 0) = TREE_TYPE (functyp);
  TREE_VEC_ELT (targ, 1) = argtypepack;

  tree traits_class
    = lookup_template_class (coro_traits_templ, targ,
			     /*in_decl=*/NULL_TREE, /*context=*/NULL_TREE,
			     /*entering scope=*/false, tf_warning_or_error);

  if (traits_class == error_mark_node)
    {
      error_at (kw, "cannot instantiate %<coroutine traits%>");
      return NULL_TREE;
    }

  return traits_class;
}

/* [coroutine.handle] */

static tree
find_coro_handle_template_decl (location_t kw)
{
  /* As for the coroutine traits, this error is per TU, so only emit
    it once.  */
  static bool coro_handle_error_emitted = false;
  tree handle_decl = lookup_qualified_name (std_node, coro_handle_identifier,
					    0, !coro_handle_error_emitted);
  if (handle_decl == error_mark_node
      || !DECL_CLASS_TEMPLATE_P (handle_decl))
    {
      if (!coro_handle_error_emitted)
	error_at (kw, "coroutines require a handle class template;"
		  " cannot find %<%E::%E%>", std_node, coro_handle_identifier);
      coro_handle_error_emitted = true;
      return NULL_TREE;
    }
  else
    return handle_decl;
}

/* Instantiate the handle template for a given promise type.  */

static tree
instantiate_coro_handle_for_promise_type (location_t kw, tree promise_type)
{
  /* So now build up a type list for the template, one entry, the promise.  */
  tree targ = make_tree_vec (1);
  TREE_VEC_ELT (targ, 0) = promise_type;
  tree handle_type
    = lookup_template_class (coro_handle_identifier, targ,
			     /* in_decl=*/NULL_TREE,
			     /* context=*/std_node,
			     /* entering scope=*/false, tf_warning_or_error);

  if (handle_type == error_mark_node)
    {
      error_at (kw, "cannot instantiate a %<coroutine handle%> for"
		" promise type %qT", promise_type);
      return NULL_TREE;
    }

  return handle_type;
}

/* Look for the promise_type in the instantiated traits.  */

static tree
find_promise_type (tree traits_class)
{
  tree promise_type
    = lookup_member (traits_class, coro_promise_type_identifier,
		     /* protect=*/1, /*want_type=*/true, tf_warning_or_error);

  if (promise_type)
    promise_type
      = complete_type_or_else (TREE_TYPE (promise_type), promise_type);

  /* NULL_TREE on fail.  */
  return promise_type;
}

static bool
coro_promise_type_found_p (tree fndecl, location_t loc)
{
  gcc_assert (fndecl != NULL_TREE);

  if (!coro_initialized)
    {
      /* Trees we only need to create once.
	 Set up the identifiers we will use.  */
      coro_init_identifiers ();

      /* Coroutine traits template.  */
      coro_traits_templ = find_coro_traits_template_decl (loc);
      if (coro_traits_templ == NULL_TREE)
	return false;

      /*  coroutine_handle<> template.  */
      coro_handle_templ = find_coro_handle_template_decl (loc);
      if (coro_handle_templ == NULL_TREE)
	return false;

      /*  We can also instantiate the void coroutine_handle<>  */
      void_coro_handle_type =
	instantiate_coro_handle_for_promise_type (loc, NULL_TREE);
      if (void_coro_handle_type == NULL_TREE)
	return false;

      /* A table to hold the state, per coroutine decl.  */
      gcc_checking_assert (coroutine_info_table == NULL);
      coroutine_info_table =
	hash_table<coroutine_info_hasher>::create_ggc (11);

      if (coroutine_info_table == NULL)
	return false;

      coro_initialized = true;
    }

  /* Save the coroutine data on the side to avoid the overhead on every
     function decl tree.  */

  coroutine_info *coro_info = get_or_insert_coroutine_info (fndecl);
  /* Without this, we cannot really proceed.  */
  gcc_checking_assert (coro_info);

  /* If we don't already have a current promise type, try to look it up.  */
  if (coro_info->promise_type == NULL_TREE)
    {
      /* Get the coroutine traits template class instance for the function
	 signature we have - coroutine_traits <R, ...>  */

      tree templ_class = instantiate_coro_traits (fndecl, loc);

      /* Find the promise type for that.  */
      coro_info->promise_type = find_promise_type (templ_class);

      /* If we don't find it, punt on the rest.  */
      if (coro_info->promise_type == NULL_TREE)
	{
	  if (!coro_info->coro_promise_error_emitted)
	    error_at (loc, "unable to find the promise type for"
		      " this coroutine");
	  coro_info->coro_promise_error_emitted = true;
	  return false;
	}

      /* Test for errors in the promise type that can be determined now.  */
      tree has_ret_void = lookup_member (coro_info->promise_type,
					 coro_return_void_identifier,
					 /*protect=*/1, /*want_type=*/0,
					 tf_none);
      tree has_ret_val = lookup_member (coro_info->promise_type,
					coro_return_value_identifier,
					/*protect=*/1, /*want_type=*/0,
					tf_none);
      if (has_ret_void && has_ret_val)
	{
	  location_t ploc = DECL_SOURCE_LOCATION (fndecl);
	  if (!coro_info->coro_co_return_error_emitted)
	    error_at (ploc, "the coroutine promise type %qT declares both"
		      " %<return_value%> and %<return_void%>",
		      coro_info->promise_type);
	  inform (DECL_SOURCE_LOCATION (BASELINK_FUNCTIONS (has_ret_void)),
		  "%<return_void%> declared here");
	  inform (DECL_SOURCE_LOCATION (BASELINK_FUNCTIONS (has_ret_val)),
		  "%<return_value%> declared here");
	  coro_info->coro_co_return_error_emitted = true;
	  return false;
	}

      /* Try to find the handle type for the promise.  */
      tree handle_type =
	instantiate_coro_handle_for_promise_type (loc, coro_info->promise_type);
      if (handle_type == NULL_TREE)
	return false;

      /* Complete this, we're going to use it.  */
      coro_info->handle_type = complete_type_or_else (handle_type, fndecl);

      /* Diagnostic would be emitted by complete_type_or_else.  */
      if (!coro_info->handle_type)
	return false;

      /* Build a proxy for a handle to "self" as the param to
	 await_suspend() calls.  */
      coro_info->self_h_proxy
	= build_lang_decl (VAR_DECL, get_identifier ("self_h.proxy"),
			   coro_info->handle_type);

      /* Build a proxy for the promise so that we can perform lookups.  */
      coro_info->promise_proxy
	= build_lang_decl (VAR_DECL, get_identifier ("promise.proxy"),
			   coro_info->promise_type);

      /* Note where we first saw a coroutine keyword.  */
      coro_info->first_coro_keyword = loc;
    }

  return true;
}

/* These functions assumes that the caller has verified that the state for
   the decl has been initialized, we try to minimize work here.  */

static tree
get_coroutine_promise_type (tree decl)
{
  if (coroutine_info *info = get_coroutine_info (decl))
    return info->promise_type;

  return NULL_TREE;
}

static tree
get_coroutine_handle_type (tree decl)
{
  if (coroutine_info *info = get_coroutine_info (decl))
    return info->handle_type;

  return NULL_TREE;
}

static tree
get_coroutine_self_handle_proxy (tree decl)
{
  if (coroutine_info *info = get_coroutine_info (decl))
    return info->self_h_proxy;

  return NULL_TREE;
}

static tree
get_coroutine_promise_proxy (tree decl)
{
  if (coroutine_info *info = get_coroutine_info (decl))
    return info->promise_proxy;

  return NULL_TREE;
}

static tree
lookup_promise_method (tree fndecl, tree member_id, location_t loc,
		       bool musthave)
{
  tree promise = get_coroutine_promise_type (fndecl);
  tree pm_memb
    = lookup_member (promise, member_id,
		     /*protect=*/1, /*want_type=*/0, tf_warning_or_error);
  if (musthave && pm_memb == NULL_TREE)
    {
      error_at (loc, "no member named %qE in %qT", member_id, promise);
      return error_mark_node;
    }
  return pm_memb;
}

/* Build an expression of the form p.method (args) where the p is a promise
   object for the current coroutine.
   OBJECT is the promise object instance to use, it may be NULL, in which case
   we will use the promise_proxy instance for this coroutine.
   ARGS may be NULL, for empty parm lists.  */

static tree
coro_build_promise_expression (tree fn, tree promise_obj, tree member_id,
			       location_t loc, vec<tree, va_gc> **args,
			       bool musthave)
{
  tree meth = lookup_promise_method (fn, member_id, loc, musthave);
  if (meth == error_mark_node)
    return error_mark_node;

  /* If we don't find it, and it isn't needed, an empty return is OK.  */
  if (!meth)
    return NULL_TREE;

  tree promise
    = promise_obj ? promise_obj
		  : get_coroutine_promise_proxy (current_function_decl);
  tree expr;
  if (BASELINK_P (meth))
    expr = build_new_method_call (promise, meth, args, NULL_TREE,
				  LOOKUP_NORMAL, NULL, tf_warning_or_error);
  else
    {
      expr = build_class_member_access_expr (promise, meth, NULL_TREE,
					     true, tf_warning_or_error);
      vec<tree, va_gc> *real_args;
      if (!args)
	real_args = make_tree_vector ();
      else
	real_args = *args;
      expr = build_op_call (expr, &real_args, tf_warning_or_error);
    }
  return expr;
}

/* Caching get for the expression p.return_void ().  */

static tree
get_coroutine_return_void_expr (tree decl, location_t loc, bool musthave)
{
  if (coroutine_info *info = get_coroutine_info (decl))
    {
      /* If we don't have it try to build it.  */
      if (!info->return_void)
	info->return_void
	  = coro_build_promise_expression (current_function_decl, NULL,
					   coro_return_void_identifier,
					   loc, NULL, musthave);
      /* Don't return an error if it's an optional call.  */
      if (!musthave && info->return_void == error_mark_node)
	return NULL_TREE;
      return info->return_void;
    }
  return musthave ? error_mark_node : NULL_TREE;
}

/* Lookup an Awaitable member, which should be await_ready, await_suspend
   or await_resume.  */

static tree
lookup_awaitable_member (tree await_type, tree member_id, location_t loc)
{
  tree aw_memb
    = lookup_member (await_type, member_id,
		     /*protect=*/1, /*want_type=*/0, tf_warning_or_error);
  if (aw_memb == NULL_TREE)
    {
      error_at (loc, "no member named %qE in %qT", member_id, await_type);
      return error_mark_node;
    }
  return aw_memb;
}

/* Here we check the constraints that are common to all keywords (since the
   presence of a coroutine keyword makes the function into a coroutine).  */

static bool
coro_common_keyword_context_valid_p (tree fndecl, location_t kw_loc,
				     const char *kw_name)
{
  if (fndecl == NULL_TREE)
    {
      error_at (kw_loc, "%qs cannot be used outside a function", kw_name);
      return false;
    }

  /* This is arranged in order of prohibitions in the std.  */
  if (DECL_MAIN_P (fndecl))
    {
      /* [basic.start.main] 3. The function main shall not be a coroutine.  */
      error_at (kw_loc, "%qs cannot be used in the %<main%> function",
		kw_name);
      return false;
    }

  if (DECL_DECLARED_CONSTEXPR_P (fndecl))
    {
      cp_function_chain->invalid_constexpr = true;
      if (!is_instantiation_of_constexpr (fndecl))
	{
	  /* [dcl.constexpr] 3.3 it shall not be a coroutine.  */
	  error_at (kw_loc, "%qs cannot be used in a %<constexpr%> function",
		    kw_name);
	  return false;
	}
    }

  if (FNDECL_USED_AUTO (fndecl))
    {
      /* [dcl.spec.auto] 15. A function declared with a return type that uses
	 a placeholder type shall not be a coroutine.  */
      error_at (kw_loc,
		"%qs cannot be used in a function with a deduced return type",
		kw_name);
      return false;
    }

  if (varargs_function_p (fndecl))
    {
      /* [dcl.fct.def.coroutine] The parameter-declaration-clause of the
	 coroutine shall not terminate with an ellipsis that is not part
	 of a parameter-declaration.  */
      error_at (kw_loc,
		"%qs cannot be used in a varargs function", kw_name);
      return false;
    }

  if (DECL_CONSTRUCTOR_P (fndecl))
    {
      /* [class.ctor] 7. a constructor shall not be a coroutine.  */
      error_at (kw_loc, "%qs cannot be used in a constructor", kw_name);
      return false;
    }

  if (DECL_DESTRUCTOR_P (fndecl))
    {
      /* [class.dtor] 21. a destructor shall not be a coroutine.  */
      error_at (kw_loc, "%qs cannot be used in a destructor", kw_name);
      return false;
    }

  return true;
}

/* Here we check the constraints that are not per keyword.  */

static bool
coro_function_valid_p (tree fndecl)
{
  location_t f_loc = DECL_SOURCE_LOCATION (fndecl);

  /* For cases where fundamental information cannot be found, e.g. the
     coroutine traits are missing, we need to punt early.  */
  if (!coro_promise_type_found_p (fndecl, f_loc))
    return false;

  /* Since we think the function is a coroutine, that implies we parsed
     a keyword that triggered this.  Keywords check promise validity for
     their context and thus the promise type should be known at this point.  */
  if (get_coroutine_handle_type (fndecl) == NULL_TREE
      || get_coroutine_promise_type (fndecl) == NULL_TREE)
    return false;

  if (current_function_returns_value || current_function_returns_null)
    {
       /* TODO: record or extract positions of returns (and the first coro
	  keyword) so that we can add notes to the diagnostic about where
	  the bad keyword is and what made the function into a coro.  */
      error_at (f_loc, "a %<return%> statement is not allowed in coroutine;"
			" did you mean %<co_return%>?");
      return false;
    }

  return true;
}

enum suspend_point_kind {
  CO_AWAIT_SUSPEND_POINT = 0,
  CO_YIELD_SUSPEND_POINT,
  INITIAL_SUSPEND_POINT,
  FINAL_SUSPEND_POINT
};

/* Helper function to build a named variable for the temps we use for each
   await point.  The root of the name is determined by SUSPEND_KIND, and
   the variable is of type V_TYPE.  The awaitable number is reset each time
   we encounter a final suspend.  */

static tree
get_awaitable_var (suspend_point_kind suspend_kind, tree v_type)
{
  static int awn = 0;
  char *buf;
  switch (suspend_kind)
    {
      default: buf = xasprintf ("Aw%d", awn++); break;
      case CO_YIELD_SUSPEND_POINT: buf =  xasprintf ("Yd%d", awn++); break;
      case INITIAL_SUSPEND_POINT: buf =  xasprintf ("Is"); break;
      case FINAL_SUSPEND_POINT: buf =  xasprintf ("Fs"); awn = 0; break;
  }
  tree ret = get_identifier (buf);
  free (buf);
  ret = build_lang_decl (VAR_DECL, ret, v_type);
  DECL_ARTIFICIAL (ret) = true;
  return ret;
}

/* Helpers to diagnose missing noexcept on final await expressions.  */

static bool
coro_diagnose_throwing_fn (tree fndecl)
{
  if (!TYPE_NOTHROW_P (TREE_TYPE (fndecl)))
    {
      location_t f_loc = cp_expr_loc_or_loc (fndecl,
					     DECL_SOURCE_LOCATION (fndecl));
      error_at (f_loc, "the expression %qE is required to be non-throwing",
		fndecl);
      inform (f_loc, "must be declared with %<noexcept(true)%>");
      return true;
    }
  return false;
}

static bool
coro_diagnose_throwing_final_aw_expr (tree expr)
{
  tree t = TARGET_EXPR_INITIAL (expr);
  tree fn = NULL_TREE;
  if (TREE_CODE (t) == CALL_EXPR)
    fn = CALL_EXPR_FN(t);
  else if (TREE_CODE (t) == AGGR_INIT_EXPR)
    fn = AGGR_INIT_EXPR_FN (t);
  else if (TREE_CODE (t) == CONSTRUCTOR)
    return false;
  else
    {
      gcc_checking_assert (0 && "unhandled expression type");
      return false;
    }
  fn = TREE_OPERAND (fn, 0);
  return coro_diagnose_throwing_fn (fn);
}

/*  This performs [expr.await] bullet 3.3 and validates the interface obtained.
    It is also used to build the initial and final suspend points.

    'a', 'o' and 'e' are used as per the description in the section noted.

    A, the original yield/await expr, is found at source location LOC.

    We will be constructing a CO_AWAIT_EXPR for a suspend point of one of
    the four suspend_point_kind kinds.  This is indicated by SUSPEND_KIND.  */

static tree
build_co_await (location_t loc, tree a, suspend_point_kind suspend_kind)
{
  /* Try and overload of operator co_await, .... */
  tree o;
  if (MAYBE_CLASS_TYPE_P (TREE_TYPE (a)))
    {
      tree overload = NULL_TREE;
      o = build_new_op (loc, CO_AWAIT_EXPR, LOOKUP_NORMAL, a, NULL_TREE,
			NULL_TREE, &overload, tf_warning_or_error);
      /* If no viable functions are found, o is a.  */
      if (!o || o == error_mark_node)
	o = a;
      else if (flag_exceptions && suspend_kind == FINAL_SUSPEND_POINT)
	{
	  /* We found an overload for co_await(), diagnose throwing cases.  */
	  if (TREE_CODE (o) == TARGET_EXPR
	      && coro_diagnose_throwing_final_aw_expr (o))
	    return error_mark_node;

	  /* We now know that the final suspend object is distinct from the
	     final awaiter, so check for a non-throwing DTOR where needed.  */
	  tree a_type = TREE_TYPE (a);
	  if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (a_type))
	    if (tree dummy
		= build_special_member_call (a, complete_dtor_identifier,
					     NULL, a_type, LOOKUP_NORMAL,
					     tf_none))
	      {
		if (CONVERT_EXPR_P (dummy))
		  dummy = TREE_OPERAND (dummy, 0);
		dummy = TREE_OPERAND (CALL_EXPR_FN (dummy), 0);
		if (coro_diagnose_throwing_fn (dummy))
		  return error_mark_node;
	      }
	}
    }
  else
    o = a; /* This is most likely about to fail anyway.  */

  tree o_type = TREE_TYPE (o);
  if (o_type && !VOID_TYPE_P (o_type))
    o_type = complete_type_or_else (o_type, o);

  if (!o_type)
    return error_mark_node;

  if (TREE_CODE (o_type) != RECORD_TYPE)
    {
      error_at (loc, "awaitable type %qT is not a structure",
		o_type);
      return error_mark_node;
    }

  /* Check for required awaitable members and their types.  */
  tree awrd_meth
    = lookup_awaitable_member (o_type, coro_await_ready_identifier, loc);
  if (!awrd_meth || awrd_meth == error_mark_node)
    return error_mark_node;
  tree awsp_meth
    = lookup_awaitable_member (o_type, coro_await_suspend_identifier, loc);
  if (!awsp_meth || awsp_meth == error_mark_node)
    return error_mark_node;

  /* The type of the co_await is the return type of the awaitable's
     await_resume, so we need to look that up.  */
  tree awrs_meth
    = lookup_awaitable_member (o_type, coro_await_resume_identifier, loc);
  if (!awrs_meth || awrs_meth == error_mark_node)
    return error_mark_node;

  /* To complete the lookups, we need an instance of 'e' which is built from
     'o' according to [expr.await] 3.4.

     If we need to materialize this as a temporary, then that will have to be
     'promoted' to a coroutine frame var.  However, if the awaitable is a
     user variable, parameter or comes from a scope outside this function,
     then we must use it directly - or we will see unnecessary copies.

     If o is a variable, find the underlying var.  */
  tree e_proxy = STRIP_NOPS (o);
  if (INDIRECT_REF_P (e_proxy))
    e_proxy = TREE_OPERAND (e_proxy, 0);
  while (TREE_CODE (e_proxy) == COMPONENT_REF)
    {
      e_proxy = TREE_OPERAND (e_proxy, 0);
      if (INDIRECT_REF_P (e_proxy))
	e_proxy = TREE_OPERAND (e_proxy, 0);
      if (TREE_CODE (e_proxy) == CALL_EXPR)
	{
	  /* We could have operator-> here too.  */
	  tree op = TREE_OPERAND (CALL_EXPR_FN (e_proxy), 0);
	  if (DECL_OVERLOADED_OPERATOR_P (op)
	      && DECL_OVERLOADED_OPERATOR_IS (op, COMPONENT_REF))
	    {
	      e_proxy = CALL_EXPR_ARG (e_proxy, 0);
	      STRIP_NOPS (e_proxy);
	      gcc_checking_assert (TREE_CODE (e_proxy) == ADDR_EXPR);
	      e_proxy = TREE_OPERAND (e_proxy, 0);
	    }
	}
      STRIP_NOPS (e_proxy);
    }

  /* Only build a temporary if we need it.  */
  if (TREE_CODE (e_proxy) == PARM_DECL
      || (VAR_P (e_proxy) && !is_local_temp (e_proxy)))
    {
      e_proxy = o;
      o = NULL_TREE; /* The var is already present.  */
    }
  else if (type_build_ctor_call (o_type))
    {
      e_proxy = get_awaitable_var (suspend_kind, o_type);
      releasing_vec arg (make_tree_vector_single (rvalue (o)));
      o = build_special_member_call (e_proxy, complete_ctor_identifier,
				     &arg, o_type, LOOKUP_NORMAL,
				     tf_warning_or_error);
    }
  else
    {
      e_proxy = get_awaitable_var (suspend_kind, o_type);
      o = build2 (INIT_EXPR, o_type, e_proxy, rvalue (o));
    }

  /* I suppose we could check that this is contextually convertible to bool.  */
  tree awrd_func = NULL_TREE;
  tree awrd_call
    = build_new_method_call (e_proxy, awrd_meth, NULL, NULL_TREE, LOOKUP_NORMAL,
			     &awrd_func, tf_warning_or_error);

  if (!awrd_func || !awrd_call || awrd_call == error_mark_node)
    return error_mark_node;

  /* The suspend method may return one of three types:
      1. void (no special action needed).
      2. bool (if true, we don't need to suspend).
      3. a coroutine handle, we execute the handle.resume() call.  */
  tree awsp_func = NULL_TREE;
  tree h_proxy = get_coroutine_self_handle_proxy (current_function_decl);
  vec<tree, va_gc> *args = make_tree_vector_single (h_proxy);
  tree awsp_call
    = build_new_method_call (e_proxy, awsp_meth, &args, NULL_TREE,
			     LOOKUP_NORMAL, &awsp_func, tf_warning_or_error);

  release_tree_vector (args);
  if (!awsp_func || !awsp_call || awsp_call == error_mark_node)
    return error_mark_node;

  bool ok = false;
  tree susp_return_type = TREE_TYPE (TREE_TYPE (awsp_func));
  if (same_type_p (susp_return_type, void_type_node))
    ok = true;
  else if (same_type_p (susp_return_type, boolean_type_node))
    ok = true;
  else if (TREE_CODE (susp_return_type) == RECORD_TYPE
	   && CLASS_TYPE_P (susp_return_type))
    {
      tree tt = CLASSTYPE_TI_TEMPLATE (susp_return_type);
      if (tt == coro_handle_templ)
	ok = true;
    }

  if (!ok)
    {
      error_at (loc, "%<await_suspend%> must return %<void%>, %<bool%> or"
		     " a coroutine handle");
      return error_mark_node;
    }

  /* Finally, the type of e.await_resume() is the co_await's type.  */
  tree awrs_func = NULL_TREE;
  tree awrs_call
    = build_new_method_call (e_proxy, awrs_meth, NULL, NULL_TREE, LOOKUP_NORMAL,
			     &awrs_func, tf_warning_or_error);

  if (!awrs_func || !awrs_call || awrs_call == error_mark_node)
    return error_mark_node;

  if (flag_exceptions && suspend_kind == FINAL_SUSPEND_POINT)
    {
      if (coro_diagnose_throwing_fn (awrd_func))
	return error_mark_node;
      if (coro_diagnose_throwing_fn (awsp_func))
	return error_mark_node;
      if (coro_diagnose_throwing_fn (awrs_func))
	return error_mark_node;
      if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (o_type))
	if (tree dummy
	    = build_special_member_call (e_proxy, complete_dtor_identifier,
					 NULL, o_type, LOOKUP_NORMAL,
					 tf_none))
	  {
	    if (CONVERT_EXPR_P (dummy))
	      dummy = TREE_OPERAND (dummy, 0);
	    dummy = TREE_OPERAND (CALL_EXPR_FN (dummy), 0);
	    if (coro_diagnose_throwing_fn (dummy))
	      return error_mark_node;
	  }
    }

  /* We now have three call expressions, in terms of the promise, handle and
     'e' proxies.  Save them in the await expression for later expansion.  */

  tree awaiter_calls = make_tree_vec (3);
  TREE_VEC_ELT (awaiter_calls, 0) = awrd_call; /* await_ready().  */
  TREE_VEC_ELT (awaiter_calls, 1) = awsp_call; /* await_suspend().  */
  tree te = NULL_TREE;
  if (TREE_CODE (awrs_call) == TARGET_EXPR)
    {
      te = awrs_call;
      awrs_call = TREE_OPERAND (awrs_call, 1);
    }
  TREE_VEC_ELT (awaiter_calls, 2) = awrs_call; /* await_resume().  */

  tree await_expr = build5_loc (loc, CO_AWAIT_EXPR,
				TREE_TYPE (TREE_TYPE (awrs_func)),
				a, e_proxy, o, awaiter_calls,
				build_int_cst (integer_type_node,
					       (int) suspend_kind));
  if (te)
    {
      TREE_OPERAND (te, 1) = await_expr;
      await_expr = te;
    }
  tree t = convert_from_reference (await_expr);
  return t;
}

tree
finish_co_await_expr (location_t kw, tree expr)
{
  if (!expr || error_operand_p (expr))
    return error_mark_node;

  if (!coro_common_keyword_context_valid_p (current_function_decl, kw,
					    "co_await"))
    return error_mark_node;

  /* The current function has now become a coroutine, if it wasn't already.  */
  DECL_COROUTINE_P (current_function_decl) = 1;

  /* This function will appear to have no return statement, even if it
     is declared to return non-void (most likely).  This is correct - we
     synthesize the return for the ramp in the compiler.  So suppress any
     extraneous warnings during substitution.  */
  TREE_NO_WARNING (current_function_decl) = true;

  /* If we don't know the promise type, we can't proceed, build the
     co_await with the expression unchanged.  */
  tree functype = TREE_TYPE (current_function_decl);
  if (dependent_type_p (functype) || type_dependent_expression_p (expr))
    return build5_loc (kw, CO_AWAIT_EXPR, unknown_type_node, expr,
		       NULL_TREE, NULL_TREE, NULL_TREE, integer_zero_node);

  /* We must be able to look up the "await_transform" method in the scope of
     the promise type, and obtain its return type.  */
  if (!coro_promise_type_found_p (current_function_decl, kw))
    return error_mark_node;

  /* [expr.await] 3.2
     The incoming cast expression might be transformed by a promise
     'await_transform()'.  */
  tree at_meth
    = lookup_promise_method (current_function_decl,
			     coro_await_transform_identifier, kw,
			     /*musthave=*/false);
  if (at_meth == error_mark_node)
    return error_mark_node;

  tree a = expr;
  if (at_meth)
    {
      /* try to build a = p.await_transform (e). */
      tree at_fn = NULL_TREE;
      vec<tree, va_gc> *args = make_tree_vector_single (expr);
      a = build_new_method_call (get_coroutine_promise_proxy (
				   current_function_decl),
				 at_meth, &args, NULL_TREE, LOOKUP_NORMAL,
				 &at_fn, tf_warning_or_error);

      /* As I read the section.
	 We saw an await_transform method, so it's mandatory that we replace
	 expr with p.await_transform (expr), therefore if the method call fails
	 (presumably, we don't have suitable arguments) then this part of the
	 process fails.  */
      if (!at_fn || a == error_mark_node)
	return error_mark_node;
    }

  /* Now we want to build co_await a.  */
  tree op = build_co_await (kw, a, CO_AWAIT_SUSPEND_POINT);
  if (op != error_mark_node)
    {
      TREE_SIDE_EFFECTS (op) = 1;
      SET_EXPR_LOCATION (op, kw);
    }

  return op;
}

/* Take the EXPR given and attempt to build:
     co_await p.yield_value (expr);
   per [expr.yield] para 1. */

tree
finish_co_yield_expr (location_t kw, tree expr)
{
  if (!expr || error_operand_p (expr))
    return error_mark_node;

  /* Check the general requirements and simple syntax errors.  */
  if (!coro_common_keyword_context_valid_p (current_function_decl, kw,
					    "co_yield"))
    return error_mark_node;

  /* The current function has now become a coroutine, if it wasn't already.  */
  DECL_COROUTINE_P (current_function_decl) = 1;

  /* This function will appear to have no return statement, even if it
     is declared to return non-void (most likely).  This is correct - we
     synthesize the return for the ramp in the compiler.  So suppress any
     extraneous warnings during substitution.  */
  TREE_NO_WARNING (current_function_decl) = true;

  /* If we don't know the promise type, we can't proceed, build the
     co_await with the expression unchanged.  */
  tree functype = TREE_TYPE (current_function_decl);
  if (dependent_type_p (functype) || type_dependent_expression_p (expr))
    return build2_loc (kw, CO_YIELD_EXPR, unknown_type_node, expr, NULL_TREE);

  if (!coro_promise_type_found_p (current_function_decl, kw))
    /* We must be able to look up the "yield_value" method in the scope of
       the promise type, and obtain its return type.  */
    return error_mark_node;

  /* [expr.yield] / 1
     Let e be the operand of the yield-expression and p be an lvalue naming
     the promise object of the enclosing coroutine, then the yield-expression
     is equivalent to the expression co_await p.yield_value(e).
     build p.yield_value(e):  */
  vec<tree, va_gc> *args = make_tree_vector_single (expr);
  tree yield_call
    = coro_build_promise_expression (current_function_decl, NULL,
				     coro_yield_value_identifier, kw,
				     &args, /*musthave=*/true);
  release_tree_vector (args);

  /* So now we have the type of p.yield_value (e).
     Now we want to build co_await p.yield_value (e).
     Noting that for co_yield, there is no evaluation of any potential
     promise transform_await().  */

  tree op = build_co_await (kw, yield_call, CO_YIELD_SUSPEND_POINT);
  if (op != error_mark_node)
    {
      if (REFERENCE_REF_P (op))
	op = TREE_OPERAND (op, 0);
      /* If the await expression is wrapped in a TARGET_EXPR, then transfer
	 that wrapper to the CO_YIELD_EXPR, since this is just a proxy for
	 its contained await.  Otherwise, just build the CO_YIELD_EXPR.  */
      if (TREE_CODE (op) == TARGET_EXPR)
	{
	  tree t = TREE_OPERAND (op, 1);
	  t = build2_loc (kw, CO_YIELD_EXPR, TREE_TYPE (t), expr, t);
	  TREE_OPERAND (op, 1) = t;
	}
      else
	op = build2_loc (kw, CO_YIELD_EXPR, TREE_TYPE (op), expr, op);
      TREE_SIDE_EFFECTS (op) = 1;
      op = convert_from_reference (op);
    }

  return op;
}

/* Check and build a co_return statememt.
   First that it's valid to have a co_return keyword here.
   If it is, then check and build the p.return_{void(),value(expr)}.
   These are built against a proxy for the promise, which will be filled
   in with the actual frame version when the function is transformed.  */

tree
finish_co_return_stmt (location_t kw, tree expr)
{
  if (expr)
    STRIP_ANY_LOCATION_WRAPPER (expr);

  if (error_operand_p (expr))
    return error_mark_node;

  /* If it fails the following test, the function is not permitted to be a
     coroutine, so the co_return statement is erroneous.  */
  if (!coro_common_keyword_context_valid_p (current_function_decl, kw,
					    "co_return"))
    return error_mark_node;

  /* The current function has now become a coroutine, if it wasn't
     already.  */
  DECL_COROUTINE_P (current_function_decl) = 1;

  /* This function will appear to have no return statement, even if it
     is declared to return non-void (most likely).  This is correct - we
     synthesize the return for the ramp in the compiler.  So suppress any
     extraneous warnings during substitution.  */
  TREE_NO_WARNING (current_function_decl) = true;

  if (processing_template_decl
      && check_for_bare_parameter_packs (expr))
    return error_mark_node;

  /* If we don't know the promise type, we can't proceed, build the
     co_return with the expression unchanged.  */
  tree functype = TREE_TYPE (current_function_decl);
  if (dependent_type_p (functype) || type_dependent_expression_p (expr))
    {
      /* co_return expressions are always void type, regardless of the
	 expression type.  */
      expr = build2_loc (kw, CO_RETURN_EXPR, void_type_node,
			 expr, NULL_TREE);
      expr = maybe_cleanup_point_expr_void (expr);
      return add_stmt (expr);
    }

  if (!coro_promise_type_found_p (current_function_decl, kw))
    return error_mark_node;

  /* Suppress -Wreturn-type for co_return, we need to check indirectly
     whether the promise type has a suitable return_void/return_value.  */
  TREE_NO_WARNING (current_function_decl) = true;

  if (!processing_template_decl && warn_sequence_point)
    verify_sequence_points (expr);

  if (expr)
    {
      /* If we had an id-expression obfuscated by force_paren_expr, we need
	 to undo it so we can try to treat it as an rvalue below.  */
      expr = maybe_undo_parenthesized_ref (expr);

      if (processing_template_decl)
	expr = build_non_dependent_expr (expr);

      if (error_operand_p (expr))
	return error_mark_node;
    }

  /* If the promise object doesn't have the correct return call then
     there's a mis-match between the co_return <expr> and this.  */
  tree co_ret_call = error_mark_node;
  if (expr == NULL_TREE || VOID_TYPE_P (TREE_TYPE (expr)))
    co_ret_call
      = get_coroutine_return_void_expr (current_function_decl, kw, true);
  else
    {
      /* [class.copy.elision] / 3.
	 An implicitly movable entity is a variable of automatic storage
	 duration that is either a non-volatile object or an rvalue reference
	 to a non-volatile object type.  For such objects in the context of
	 the co_return, the overload resolution should be carried out first
	 treating the object as an rvalue, if that fails, then we fall back
	 to regular overload resolution.  */

      if (treat_lvalue_as_rvalue_p (expr, /*parm_ok*/true)
	  && CLASS_TYPE_P (TREE_TYPE (expr))
	  && !TYPE_VOLATILE (TREE_TYPE (expr)))
	{
	  /* It's OK if this fails... */
	  vec<tree, va_gc> *args = make_tree_vector_single (move (expr));
	  co_ret_call
	    = coro_build_promise_expression (current_function_decl, NULL,
					     coro_return_value_identifier, kw,
					     &args, /*musthave=*/false);
	  release_tree_vector (args);
	}

      if (!co_ret_call || co_ret_call == error_mark_node)
	{
	  /* ... but this must succeed if we didn't get the move variant.  */
	  vec<tree, va_gc> *args = make_tree_vector_single (expr);
	  co_ret_call
	    = coro_build_promise_expression (current_function_decl, NULL,
					     coro_return_value_identifier, kw,
					     &args, /*musthave=*/true);
	  release_tree_vector (args);
	}
    }

  /* Makes no sense for a co-routine really. */
  if (TREE_THIS_VOLATILE (current_function_decl))
    warning_at (kw, 0,
		"function declared %<noreturn%> has a"
		" %<co_return%> statement");

  expr = build2_loc (kw, CO_RETURN_EXPR, void_type_node, expr, co_ret_call);
  expr = maybe_cleanup_point_expr_void (expr);
  return add_stmt (expr);
}

/* We need to validate the arguments to __builtin_coro_promise, since the
   second two must be constant, and the builtins machinery doesn't seem to
   deal with that properly.  */

tree
coro_validate_builtin_call (tree call, tsubst_flags_t)
{
  tree fn = TREE_OPERAND (CALL_EXPR_FN (call), 0);

  gcc_checking_assert (DECL_BUILT_IN_CLASS (fn) == BUILT_IN_NORMAL);
  switch (DECL_FUNCTION_CODE (fn))
    {
    default:
      return call;

    case BUILT_IN_CORO_PROMISE:
      {
	/* Argument 0 is already checked by the normal built-in machinery
	   Argument 1 must be a constant of size type.  It probably makes
	   little sense if it's not a power of 2, but that isn't specified
	   formally.  */
	tree arg = CALL_EXPR_ARG (call, 1);
	location_t loc = EXPR_LOCATION (arg);

	/* We expect alignof expressions in templates.  */
	if (TREE_CODE (arg) == NON_DEPENDENT_EXPR
	    && TREE_CODE (TREE_OPERAND (arg, 0)) == ALIGNOF_EXPR)
	  ;
	else if (!TREE_CONSTANT (arg))
	  {
	    error_at (loc, "the align argument to %<__builtin_coro_promise%>"
			   " must be a constant");
	    return error_mark_node;
	  }
	/* Argument 2 is the direction - to / from handle address to promise
	   address.  */
	arg = CALL_EXPR_ARG (call, 2);
	loc = EXPR_LOCATION (arg);
	if (!TREE_CONSTANT (arg))
	  {
	    error_at (loc, "the direction argument to"
			   " %<__builtin_coro_promise%> must be a constant");
	    return error_mark_node;
	  }
	return call;
	break;
      }
    }
}

/* ================= Morph and Expand. =================

   The entry point here is morph_fn_to_coro () which is called from
   finish_function () when we have completed any template expansion.

   This is preceded by helper functions that implement the phases below.

   The process proceeds in four phases.

   A Initial framing.
     The user's function body is wrapped in the initial and final suspend
     points and we begin building the coroutine frame.
     We build empty decls for the actor and destroyer functions at this
     time too.
     When exceptions are enabled, the user's function body will also be
     wrapped in a try-catch block with the catch invoking the promise
     class 'unhandled_exception' method.

   B Analysis.
     The user's function body is analyzed to determine the suspend points,
     if any, and to capture local variables that might persist across such
     suspensions.  In most cases, it is not necessary to capture compiler
     temporaries, since the tree-lowering nests the suspensions correctly.
     However, in the case of a captured reference, there is a lifetime
     extension to the end of the full expression - which can mean across a
     suspend point in which case it must be promoted to a frame variable.

     At the conclusion of analysis, we have a conservative frame layout and
     maps of the local variables to their frame entry points.

   C Build the ramp function.
     Carry out the allocation for the coroutine frame (NOTE; the actual size
     computation is deferred until late in the middle end to allow for future
     optimizations that will be allowed to elide unused frame entries).
     We build the return object.

   D Build and expand the actor and destroyer function bodies.
     The destroyer is a trivial shim that sets a bit to indicate that the
     destroy dispatcher should be used and then calls into the actor.

     The actor function is the implementation of the user's state machine.
     The current suspend point is noted in an index.
     Each suspend point is encoded as a pair of internal functions, one in
     the relevant dispatcher, and one representing the suspend point.

     During this process, the user's local variables and the proxies for the
     self-handle and the promise class instance are re-written to their
     coroutine frame equivalents.

     The complete bodies for the ramp, actor and destroy function are passed
     back to finish_function for folding and gimplification.  */

/* Helpers to build EXPR_STMT and void-cast EXPR_STMT, common ops.  */

static tree
coro_build_expr_stmt (tree expr, location_t loc)
{
  return maybe_cleanup_point_expr_void (build_stmt (loc, EXPR_STMT, expr));
}

static tree
coro_build_cvt_void_expr_stmt (tree expr, location_t loc)
{
  tree t = build1 (CONVERT_EXPR, void_type_node, expr);
  return coro_build_expr_stmt (t, loc);
}

/* Helpers for label creation:
   1. Create a named label in the specified context.  */

static tree
create_anon_label_with_ctx (location_t loc, tree ctx)
{
  tree lab = build_decl (loc, LABEL_DECL, NULL_TREE, void_type_node);

  DECL_CONTEXT (lab) = ctx;
  DECL_ARTIFICIAL (lab) = true;
  DECL_IGNORED_P (lab) = true;
  TREE_USED (lab) = true;
  return lab;
}

/*  2. Create a named label in the specified context.  */

static tree
create_named_label_with_ctx (location_t loc, const char *name, tree ctx)
{
  tree lab_id = get_identifier (name);
  tree lab = define_label (loc, lab_id);
  DECL_CONTEXT (lab) = ctx;
  DECL_ARTIFICIAL (lab) = true;
  TREE_USED (lab) = true;
  return lab;
}

struct proxy_replace
{
  tree from, to;
};

static tree
replace_proxy (tree *here, int *do_subtree, void *d)
{
  proxy_replace *data = (proxy_replace *) d;

  if (*here == data->from)
    {
      *here = data->to;
      *do_subtree = 0;
    }
  else
    *do_subtree = 1;
  return NULL_TREE;
}

/* Support for expansion of co_await statements.  */

struct coro_aw_data
{
  tree actor_fn;   /* Decl for context.  */
  tree coro_fp;    /* Frame pointer var.  */
  tree resume_idx; /* This is the index var in the frame.  */
  tree i_a_r_c;    /* initial suspend await_resume() was called if true.  */
  tree self_h;     /* This is a handle to the current coro (frame var).  */
  tree cleanup;    /* This is where to go once we complete local destroy.  */
  tree cororet;    /* This is where to go if we suspend.  */
  tree corocont;   /* This is where to go if we continue.  */
  tree conthand;   /* This is the handle for a continuation.  */
  unsigned index;  /* This is our current resume index.  */
};

/* Lighweight search for the first await expression in tree-walk order.
   returns:
     The first await expression found in STMT.
     NULL_TREE if there are none.
   So can be used to determine if the statement needs to be processed for
   awaits.  */

static tree
co_await_find_in_subtree (tree *stmt, int *, void *d)
{
  tree **p = (tree **) d;
  if (TREE_CODE (*stmt) == CO_AWAIT_EXPR)
    {
      *p = stmt;
      return *stmt;
    }
  return NULL_TREE;
}

/* Starting with a statment:

   stmt => some tree containing one or more await expressions.

   We replace the statement with:
   <STATEMENT_LIST> {
      initialise awaitable
      if (!ready)
	{
	  suspension context.
	}
      resume:
	revised statement with one await expression rewritten to its
	await_resume() return value.
   }

   We then recurse into the initializer and the revised statement
   repeating this replacement until there are no more await expressions
   in either.  */

static tree *
expand_one_await_expression (tree *stmt, tree *await_expr, void *d)
{
  coro_aw_data *data = (coro_aw_data *) d;

  tree saved_statement = *stmt;
  tree saved_co_await = *await_expr;

  tree actor = data->actor_fn;
  location_t loc = EXPR_LOCATION (*stmt);
  tree var = TREE_OPERAND (saved_co_await, 1);  /* frame slot. */
  tree expr = TREE_OPERAND (saved_co_await, 2); /* initializer.  */
  tree awaiter_calls = TREE_OPERAND (saved_co_await, 3);

  tree source = TREE_OPERAND (saved_co_await, 4);
  bool is_final = (source
		   && TREE_INT_CST_LOW (source) == (int) FINAL_SUSPEND_POINT);
  bool needs_dtor = TYPE_HAS_NONTRIVIAL_DESTRUCTOR (TREE_TYPE (var));
  int resume_point = data->index;
  size_t bufsize = sizeof ("destroy.") + 10;
  char *buf = (char *) alloca (bufsize);
  snprintf (buf, bufsize, "destroy.%d", resume_point);
  tree destroy_label = create_named_label_with_ctx (loc, buf, actor);
  snprintf (buf, bufsize, "resume.%d", resume_point);
  tree resume_label = create_named_label_with_ctx (loc, buf, actor);
  tree empty_list = build_empty_stmt (loc);

  tree await_type = TREE_TYPE (var);
  tree stmt_list = NULL;
  tree r;
  tree *await_init = NULL;

  if (!expr)
    needs_dtor = false; /* No need, the var's lifetime is managed elsewhere.  */
  else
    {
      r = coro_build_cvt_void_expr_stmt (expr, loc);
      append_to_statement_list_force (r, &stmt_list);
      /* We have an initializer, which might itself contain await exprs.  */
      await_init = tsi_stmt_ptr (tsi_last (stmt_list));
    }

  /* Use the await_ready() call to test if we need to suspend.  */
  tree ready_cond = TREE_VEC_ELT (awaiter_calls, 0); /* await_ready().  */
  /* Convert to bool, if necessary.  */
  if (TREE_CODE (TREE_TYPE (ready_cond)) != BOOLEAN_TYPE)
    ready_cond = cp_convert (boolean_type_node, ready_cond,
			     tf_warning_or_error);
  /* Be aggressive in folding here, since there are a significant number of
     cases where the ready condition is constant.  */
  ready_cond = invert_truthvalue_loc (loc, ready_cond);
  ready_cond
    = build1_loc (loc, CLEANUP_POINT_EXPR, boolean_type_node, ready_cond);

  tree body_list = NULL;
  tree susp_idx = build_int_cst (short_unsigned_type_node, data->index);
  r = build2_loc (loc, MODIFY_EXPR, short_unsigned_type_node, data->resume_idx,
		  susp_idx);
  r = coro_build_cvt_void_expr_stmt (r, loc);
  append_to_statement_list (r, &body_list);

  /* Find out what we have to do with the awaiter's suspend method.
     [expr.await]
     (5.1) If the result of await-ready is false, the coroutine is considered
	   suspended. Then:
     (5.1.1) If the type of await-suspend is std::coroutine_handle<Z>,
	     await-suspend.resume() is evaluated.
     (5.1.2) if the type of await-suspend is bool, await-suspend is evaluated,
	     and the coroutine is resumed if the result is false.
     (5.1.3) Otherwise, await-suspend is evaluated.  */

  tree suspend = TREE_VEC_ELT (awaiter_calls, 1); /* await_suspend().  */
  tree susp_type = TREE_TYPE (suspend);

  bool is_cont = false;
  /* NOTE: final suspend can't resume; the "resume" label in that case
     corresponds to implicit destruction.  */
  if (VOID_TYPE_P (susp_type))
    {
      /* We just call await_suspend() and hit the yield.  */
      suspend = coro_build_cvt_void_expr_stmt (suspend, loc);
      append_to_statement_list (suspend, &body_list);
    }
  else if (TREE_CODE (susp_type) == BOOLEAN_TYPE)
    {
      /* Boolean return, continue if the call returns false.  */
      suspend = build1_loc (loc, TRUTH_NOT_EXPR, boolean_type_node, suspend);
      suspend
	= build1_loc (loc, CLEANUP_POINT_EXPR, boolean_type_node, suspend);
      tree go_on = build1_loc (loc, GOTO_EXPR, void_type_node, resume_label);
      r = build3_loc (loc, COND_EXPR, void_type_node, suspend, go_on,
		      empty_list);
      append_to_statement_list (r, &body_list);
    }
  else
    {
      r = build1_loc (loc, CONVERT_EXPR, void_coro_handle_type, suspend);
      r = build2_loc (loc, INIT_EXPR, void_coro_handle_type, data->conthand, r);
      r = build1 (CONVERT_EXPR, void_type_node, r);
      append_to_statement_list (r, &body_list);
      is_cont = true;
    }

  tree d_l = build_address (destroy_label);
  tree r_l = build_address (resume_label);
  tree susp = build_address (data->cororet);
  tree cont = build_address (data->corocont);
  tree final_susp = build_int_cst (integer_type_node, is_final ? 1 : 0);

  susp_idx = build_int_cst (integer_type_node, data->index);

  tree sw = begin_switch_stmt ();
  tree cond = build_decl (loc, VAR_DECL, NULL_TREE, integer_type_node);
  DECL_ARTIFICIAL (cond) = 1;
  DECL_IGNORED_P (cond) = 1;
  layout_decl (cond, 0);

  r = build_call_expr_internal_loc (loc, IFN_CO_YIELD, integer_type_node, 5,
				    susp_idx, final_susp, r_l, d_l,
				    data->coro_fp);
  r = build2 (INIT_EXPR, integer_type_node, cond, r);
  finish_switch_cond (r, sw);
  r = build_case_label (build_int_cst (integer_type_node, 0), NULL_TREE,
			create_anon_label_with_ctx (loc, actor));
  add_stmt (r); /* case 0: */
  /* Implement the suspend, a scope exit without clean ups.  */
  r = build_call_expr_internal_loc (loc, IFN_CO_SUSPN, void_type_node, 1,
				    is_cont ? cont : susp);
  r = coro_build_cvt_void_expr_stmt (r, loc);
  add_stmt (r); /*   goto ret;  */
  r = build_case_label (build_int_cst (integer_type_node, 1), NULL_TREE,
			create_anon_label_with_ctx (loc, actor));
  add_stmt (r); /* case 1:  */
  r = build1_loc (loc, GOTO_EXPR, void_type_node, resume_label);
  add_stmt (r); /*  goto resume;  */
  r = build_case_label (NULL_TREE, NULL_TREE,
			create_anon_label_with_ctx (loc, actor));
  add_stmt (r); /* default:;  */
  r = build1_loc (loc, GOTO_EXPR, void_type_node, destroy_label);
  add_stmt (r); /* goto destroy;  */

  /* part of finish switch.  */
  SWITCH_STMT_BODY (sw) = pop_stmt_list (SWITCH_STMT_BODY (sw));
  pop_switch ();
  tree scope = SWITCH_STMT_SCOPE (sw);
  SWITCH_STMT_SCOPE (sw) = NULL;
  r = do_poplevel (scope);
  append_to_statement_list (r, &body_list);

  destroy_label = build_stmt (loc, LABEL_EXPR, destroy_label);
  append_to_statement_list (destroy_label, &body_list);
  if (needs_dtor)
    {
      tree dtor = build_special_member_call (var, complete_dtor_identifier,
					     NULL, await_type, LOOKUP_NORMAL,
					     tf_warning_or_error);
      append_to_statement_list (dtor, &body_list);
    }
  r = build1_loc (loc, GOTO_EXPR, void_type_node, data->cleanup);
  append_to_statement_list (r, &body_list);

  r = build3_loc (loc, COND_EXPR, void_type_node, ready_cond, body_list,
		  empty_list);

  append_to_statement_list (r, &stmt_list);

  /* Resume point.  */
  resume_label = build_stmt (loc, LABEL_EXPR, resume_label);
  append_to_statement_list (resume_label, &stmt_list);

  /* This will produce the value (if one is provided) from the co_await
     expression.  */
  tree resume_call = TREE_VEC_ELT (awaiter_calls, 2); /* await_resume().  */
  if (REFERENCE_REF_P (resume_call))
    /* Sink to await_resume call_expr.  */
    resume_call = TREE_OPERAND (resume_call, 0);

  *await_expr = resume_call; /* Replace the co_await expr with its result.  */
  append_to_statement_list_force (saved_statement, &stmt_list);
  /* Get a pointer to the revised statment.  */
  tree *revised = tsi_stmt_ptr (tsi_last (stmt_list));
  if (needs_dtor)
    {
      tree dtor = build_special_member_call (var, complete_dtor_identifier,
					     NULL, await_type, LOOKUP_NORMAL,
					     tf_warning_or_error);
      append_to_statement_list (dtor, &stmt_list);
    }
  data->index += 2;

  /* Replace the original statement with the expansion.  */
  *stmt = stmt_list;

  /* Now, if the awaitable had an initializer, expand any awaits that might
     be embedded in it.  */
  tree *aw_expr_ptr;
  if (await_init &&
      cp_walk_tree (await_init, co_await_find_in_subtree, &aw_expr_ptr, NULL))
    expand_one_await_expression (await_init, aw_expr_ptr, d);

  /* Expand any more await expressions in the the original statement.  */
  if (cp_walk_tree (revised, co_await_find_in_subtree, &aw_expr_ptr, NULL))
    expand_one_await_expression (revised, aw_expr_ptr, d);

  return NULL;
}

/* Check to see if a statement contains at least one await expression, if
   so, then process that.  */

static tree
process_one_statement (tree *stmt, void *d)
{
  tree *aw_expr_ptr;
  if (cp_walk_tree (stmt, co_await_find_in_subtree, &aw_expr_ptr, NULL))
    expand_one_await_expression (stmt, aw_expr_ptr, d);
  return NULL_TREE;
}

static tree
await_statement_expander (tree *stmt, int *do_subtree, void *d)
{
  tree res = NULL_TREE;

  /* Process a statement at a time.  */
  if (STATEMENT_CLASS_P (*stmt) || TREE_CODE (*stmt) == BIND_EXPR)
    return NULL_TREE; /* Just process the sub-trees.  */
  else if (TREE_CODE (*stmt) == STATEMENT_LIST)
    {
      tree_stmt_iterator i;
      for (i = tsi_start (*stmt); !tsi_end_p (i); tsi_next (&i))
	{
	  res = cp_walk_tree (tsi_stmt_ptr (i), await_statement_expander,
			      d, NULL);
	  if (res)
	    return res;
	}
      *do_subtree = 0; /* Done subtrees.  */
    }
  else if (EXPR_P (*stmt))
    {
      process_one_statement (stmt, d);
      *do_subtree = 0; /* Done subtrees.  */
    }

  /* Continue statement walk, where required.  */
  return res;
}

/* Suspend point hash_map.  */

struct suspend_point_info
{
  /* coro frame field type.  */
  tree awaitable_type;
  /* coro frame field name.  */
  tree await_field_id;
};

static hash_map<tree, suspend_point_info> *suspend_points;

struct await_xform_data
{
  tree actor_fn;   /* Decl for context.  */
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
transform_await_expr (tree await_expr, await_xform_data *xform)
{
  suspend_point_info *si = suspend_points->get (await_expr);
  location_t loc = EXPR_LOCATION (await_expr);
  if (!si)
    {
      error_at (loc, "no suspend point info for %qD", await_expr);
      return error_mark_node;
    }

  /* So, on entry, we have:
     in : CO_AWAIT_EXPR (a, e_proxy, o, awr_call_vector, mode)
	  We no longer need a [it had diagnostic value, maybe?]
	  We need to replace the promise proxy in all elements
	  We need to replace the e_proxy in the awr_call.  */

  tree coro_frame_type = TREE_TYPE (xform->actor_frame);

  /* If we have a frame var for the awaitable, get a reference to it.  */
  proxy_replace data;
  if (si->await_field_id)
    {
      tree as_m
	 = lookup_member (coro_frame_type, si->await_field_id,
			  /*protect=*/1, /*want_type=*/0, tf_warning_or_error);
      tree as = build_class_member_access_expr (xform->actor_frame, as_m,
						NULL_TREE, true,
						tf_warning_or_error);

      /* Replace references to the instance proxy with the frame entry now
	 computed.  */
      data.from = TREE_OPERAND (await_expr, 1);
      data.to = as;
      cp_walk_tree (&await_expr, replace_proxy, &data, NULL);

      /* .. and replace.  */
      TREE_OPERAND (await_expr, 1) = as;
    }

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

/* A wrapper for the transform_await_expr function so that it can be a
   callback from cp_walk_tree.  */

static tree
transform_await_wrapper (tree *stmt, int *do_subtree, void *d)
{
  /* Set actor function as new DECL_CONTEXT of label_decl.  */
  struct await_xform_data *xform = (struct await_xform_data *) d;
  if (TREE_CODE (*stmt) == LABEL_DECL
      && DECL_CONTEXT (*stmt) != xform->actor_fn)
    DECL_CONTEXT (*stmt) = xform->actor_fn;

  /* We should have already lowered co_yields to their co_await.  */
  gcc_checking_assert (TREE_CODE (*stmt) != CO_YIELD_EXPR);
  if (TREE_CODE (*stmt) != CO_AWAIT_EXPR)
    return NULL_TREE;

  tree await_expr = *stmt;
  *stmt = transform_await_expr (await_expr, xform);
  if (*stmt == error_mark_node)
    *do_subtree = 0;
  return NULL_TREE;
}

/* This caches information that we determine about function params,
   their uses and copies in the coroutine frame.  */

struct param_info
{
  tree field_id;     /* The name of the copy in the coroutine frame.  */
  vec<tree *> *body_uses; /* Worklist of uses, void if there are none.  */
  tree frame_type;   /* The type used to represent this parm in the frame.  */
  tree orig_type;    /* The original type of the parm (not as passed).  */
  tree guard_var;    /* If we need a DTOR on exception, this bool guards it.  */
  tree fr_copy_dtor; /* If we need a DTOR on exception, this is it.  */
  bool by_ref;       /* Was passed by reference.  */
  bool pt_ref;       /* Was a pointer to object.  */
  bool rv_ref;       /* Was an rvalue ref.  */
  bool trivial_dtor; /* The frame type has a trivial DTOR.  */
  bool this_ptr;     /* Is 'this' */
  bool lambda_cobj;  /* Lambda capture object */
};

struct local_var_info
{
  tree field_id;
  tree field_idx;
  tree frame_type;
  bool is_lambda_capture;
  bool is_static;
  bool has_value_expr_p;
  location_t def_loc;
};

/* For figuring out what local variable usage we have.  */
struct local_vars_transform
{
  tree context;
  tree actor_frame;
  tree coro_frame_type;
  location_t loc;
  hash_map<tree, local_var_info> *local_var_uses;
};

static tree
transform_local_var_uses (tree *stmt, int *do_subtree, void *d)
{
  local_vars_transform *lvd = (local_vars_transform *) d;

  /* For each var in this bind expr (that has a frame id, which means it was
     accessed), build a frame reference for each and then walk the bind expr
     statements, substituting the frame ref for the original var.  */

  if (TREE_CODE (*stmt) == BIND_EXPR)
    {
      tree lvar;
      for (lvar = BIND_EXPR_VARS (*stmt); lvar != NULL;
	   lvar = DECL_CHAIN (lvar))
	{
	  bool existed;
	  local_var_info &local_var
	    = lvd->local_var_uses->get_or_insert (lvar, &existed);
	  gcc_checking_assert (existed);

	  /* Re-write the variable's context to be in the actor func.  */
	  DECL_CONTEXT (lvar) = lvd->context;

	/* For capture proxies, this could include the decl value expr.  */
	if (local_var.is_lambda_capture || local_var.has_value_expr_p)
	  {
	    tree ve = DECL_VALUE_EXPR (lvar);
	    cp_walk_tree (&ve, transform_local_var_uses, d, NULL);
	    continue; /* No frame entry for this.  */
	  }

	  /* TODO: implement selective generation of fields when vars are
	     known not-used.  */
	  if (local_var.field_id == NULL_TREE)
	    continue; /* Wasn't used.  */

	  tree fld_ref
	    = lookup_member (lvd->coro_frame_type, local_var.field_id,
			     /*protect=*/1, /*want_type=*/0,
			     tf_warning_or_error);
	  tree fld_idx = build3_loc (lvd->loc, COMPONENT_REF, TREE_TYPE (lvar),
				     lvd->actor_frame, fld_ref, NULL_TREE);
	  local_var.field_idx = fld_idx;
	}
      /* FIXME: we should be able to do this in the loop above, but (at least
	 for range for) there are cases where the DECL_INITIAL contains
	 forward references.
	 So, now we've built the revised var in the frame, substitute uses of
	 it in initializers and the bind expr body.  */
      for (lvar = BIND_EXPR_VARS (*stmt); lvar != NULL;
	   lvar = DECL_CHAIN (lvar))
	{
	  /* we need to walk some of the decl trees, which might contain
	     references to vars replaced at a higher level.  */
	  cp_walk_tree (&DECL_INITIAL (lvar), transform_local_var_uses, d,
			NULL);
	  cp_walk_tree (&DECL_SIZE (lvar), transform_local_var_uses, d, NULL);
	  cp_walk_tree (&DECL_SIZE_UNIT (lvar), transform_local_var_uses, d,
			NULL);
	}
      cp_walk_tree (&BIND_EXPR_BODY (*stmt), transform_local_var_uses, d, NULL);

      /* Now we have processed and removed references to the original vars,
	 we can drop those from the bind - leaving capture proxies alone.  */
      for (tree *pvar = &BIND_EXPR_VARS (*stmt); *pvar != NULL;)
	{
	  bool existed;
	  local_var_info &local_var
	    = lvd->local_var_uses->get_or_insert (*pvar, &existed);
	  gcc_checking_assert (existed);

	  /* Leave lambda closure captures alone, we replace the *this
	     pointer with the frame version and let the normal process
	     deal with the rest.
	     Likewise, variables with their value found elsewhere.
	     Skip past unused ones too.  */
	  if (local_var.is_lambda_capture
	     || local_var.has_value_expr_p
	     || local_var.field_id == NULL_TREE)
	    {
	      pvar = &DECL_CHAIN (*pvar);
	      continue;
	    }

	  /* Discard this one, we replaced it.  */
	  *pvar = DECL_CHAIN (*pvar);
	}

      *do_subtree = 0; /* We've done the body already.  */
      return NULL_TREE;
    }

  tree var_decl = *stmt;
  /* Look inside cleanups, we don't want to wrap a statement list in a
     cleanup.  */
  bool needs_cleanup = true;
  if (TREE_CODE (var_decl) == CLEANUP_POINT_EXPR)
    var_decl = TREE_OPERAND (var_decl, 0);
  else
    needs_cleanup = false;

  /* Look inside the decl_expr for the actual var.  */
  bool decl_expr_p = TREE_CODE (var_decl) == DECL_EXPR;
  if (decl_expr_p && TREE_CODE (DECL_EXPR_DECL (var_decl)) == VAR_DECL)
    var_decl = DECL_EXPR_DECL (var_decl);
  else if (TREE_CODE (var_decl) != VAR_DECL)
    return NULL_TREE;

  /* VAR_DECLs that are not recorded can belong to the proxies we've placed
     for the promise and coroutine handle(s), to global vars or to compiler
     temporaries.  Skip past these, we will handle them later.  */
  local_var_info *local_var_i = lvd->local_var_uses->get (var_decl);

  if (local_var_i == NULL)
    return NULL_TREE;

  if (local_var_i->is_lambda_capture
      || local_var_i->is_static
      || local_var_i->has_value_expr_p)
    return NULL_TREE;

  /* This is our revised 'local' i.e. a frame slot.  */
  tree revised = local_var_i->field_idx;
  gcc_checking_assert (DECL_CONTEXT (var_decl) == lvd->context);

  if (decl_expr_p && DECL_INITIAL (var_decl))
    {
      location_t loc = DECL_SOURCE_LOCATION (var_decl);
      tree r
	= cp_build_modify_expr (loc, revised, INIT_EXPR,
				DECL_INITIAL (var_decl), tf_warning_or_error);
      if (needs_cleanup)
	r = coro_build_cvt_void_expr_stmt (r, EXPR_LOCATION (*stmt));
      *stmt = r;
    }
  else
    *stmt = revised;

  if (decl_expr_p)
    *do_subtree = 0; /* We've accounted for the nested use.  */
  return NULL_TREE;
}

/* A helper to build the frame DTOR.
   [dcl.fct.def.coroutine] / 12
   The deallocation function’s name is looked up in the scope of the promise
   type.  If this lookup fails, the deallocation function’s name is looked up
   in the global scope.  If deallocation function lookup finds both a usual
   deallocation function with only a pointer parameter and a usual
   deallocation function with both a pointer parameter and a size parameter,
   then the selected deallocation function shall be the one with two
   parameters.  Otherwise, the selected deallocation function shall be the
   function with one parameter.  If no usual deallocation function is found
   the program is ill-formed.  The selected deallocation function shall be
   called with the address of the block of storage to be reclaimed as its
   first argument.  If a deallocation function with a parameter of type
   std::size_t is used, the size of the block is passed as the corresponding
   argument.  */

static tree
coro_get_frame_dtor (tree coro_fp, tree orig, tree frame_size,
		     tree promise_type, location_t loc)
{
  tree del_coro_fr = NULL_TREE;
  tree frame_arg = build1 (CONVERT_EXPR, ptr_type_node, coro_fp);
  tree delname = ovl_op_identifier (false, DELETE_EXPR);
  tree fns = lookup_promise_method (orig, delname, loc,
					/*musthave=*/false);
  if (fns && BASELINK_P (fns))
    {
      /* Look for sized version first, since this takes precedence.  */
      vec<tree, va_gc> *args = make_tree_vector ();
      vec_safe_push (args, frame_arg);
      vec_safe_push (args, frame_size);
      tree dummy_promise = build_dummy_object (promise_type);

      /* It's OK to fail for this one... */
      del_coro_fr = build_new_method_call (dummy_promise, fns, &args,
					   NULL_TREE, LOOKUP_NORMAL, NULL,
					   tf_none);

      if (!del_coro_fr || del_coro_fr == error_mark_node)
	{
	  release_tree_vector (args);
	  args = make_tree_vector_single (frame_arg);
	  del_coro_fr = build_new_method_call (dummy_promise, fns, &args,
					       NULL_TREE, LOOKUP_NORMAL, NULL,
					       tf_none);
	}

      /* But one of them must succeed, or the program is ill-formed.  */
      if (!del_coro_fr || del_coro_fr == error_mark_node)
	{
	  error_at (loc, "%qE is provided by %qT but is not usable with"
		  " the function signature %qD", delname, promise_type, orig);
	  del_coro_fr = error_mark_node;
	}
    }
  else
    {
      del_coro_fr = build_op_delete_call (DELETE_EXPR, frame_arg, frame_size,
					  /*global_p=*/true, /*placement=*/NULL,
					  /*alloc_fn=*/NULL,
					  tf_warning_or_error);
      if (!del_coro_fr || del_coro_fr == error_mark_node)
	del_coro_fr = error_mark_node;
    }
  return del_coro_fr;
}

/* The actor transform.  */

static void
build_actor_fn (location_t loc, tree coro_frame_type, tree actor, tree fnbody,
		tree orig, hash_map<tree, param_info> *param_uses,
		hash_map<tree, local_var_info> *local_var_uses,
		vec<tree, va_gc> *param_dtor_list, tree resume_fn_field,
		tree resume_idx_field, unsigned body_count, tree frame_size)
{
  verify_stmt_tree (fnbody);
  /* Some things we inherit from the original function.  */
  tree handle_type = get_coroutine_handle_type (orig);
  tree self_h_proxy = get_coroutine_self_handle_proxy (orig);
  tree promise_type = get_coroutine_promise_type (orig);
  tree promise_proxy = get_coroutine_promise_proxy (orig);

  /* One param, the coro frame pointer.  */
  tree actor_fp = DECL_ARGUMENTS (actor);

  /* A void return.  */
  tree resdecl = build_decl (loc, RESULT_DECL, 0, void_type_node);
  DECL_ARTIFICIAL (resdecl) = 1;
  DECL_IGNORED_P (resdecl) = 1;
  DECL_RESULT (actor) = resdecl;
  DECL_COROUTINE_P (actor) = 1;

  /* We have a definition here.  */
  TREE_STATIC (actor) = 1;

  tree actor_outer = push_stmt_list ();
  current_stmt_tree ()->stmts_are_full_exprs_p = 1;
  tree stmt = begin_compound_stmt (BCS_FN_BODY);

  tree actor_bind = build3 (BIND_EXPR, void_type_node, NULL, NULL, NULL);
  tree top_block = make_node (BLOCK);
  BIND_EXPR_BLOCK (actor_bind) = top_block;

  tree continuation = build_lang_decl (VAR_DECL,
				       get_identifier ("actor.continue"),
				       void_coro_handle_type);
  DECL_ARTIFICIAL (continuation) = 1;
  DECL_IGNORED_P (continuation) = 1;
  DECL_CONTEXT (continuation) = actor;
  BIND_EXPR_VARS (actor_bind) = continuation;

  /* Link in the block associated with the outer scope of the re-written
     function body.  */
  tree first = expr_first (fnbody);
  gcc_checking_assert (first && TREE_CODE (first) == BIND_EXPR);
  tree block = BIND_EXPR_BLOCK (first);
  gcc_checking_assert (BLOCK_SUPERCONTEXT (block) == NULL_TREE);
  gcc_checking_assert (BLOCK_CHAIN (block) == NULL_TREE);
  BLOCK_SUPERCONTEXT (block) = top_block;
  BLOCK_SUBBLOCKS (top_block) = block;

  add_stmt (actor_bind);
  tree actor_body = push_stmt_list ();

  /* The entry point for the actor code from the ramp.  */
  tree actor_begin_label
    = create_named_label_with_ctx (loc, "actor.begin", actor);
  tree actor_frame = build1_loc (loc, INDIRECT_REF, coro_frame_type, actor_fp);

  /* Declare the continuation handle.  */
  add_decl_expr (continuation);

  /* Re-write param references in the body, no code should be generated
     here.  */
  if (DECL_ARGUMENTS (orig))
    {
      tree arg;
      for (arg = DECL_ARGUMENTS (orig); arg != NULL; arg = DECL_CHAIN (arg))
	{
	  bool existed;
	  param_info &parm = param_uses->get_or_insert (arg, &existed);
	  if (!parm.body_uses)
	    continue; /* Wasn't used in the orignal function body.  */

	  tree fld_ref = lookup_member (coro_frame_type, parm.field_id,
					/*protect=*/1, /*want_type=*/0,
					tf_warning_or_error);
	  tree fld_idx = build3_loc (loc, COMPONENT_REF, parm.frame_type,
				     actor_frame, fld_ref, NULL_TREE);

	  /* We keep these in the frame as a regular pointer, so convert that
	   back to the type expected.  */
	  if (parm.pt_ref)
	    fld_idx = build1_loc (loc, CONVERT_EXPR, TREE_TYPE (arg), fld_idx);

	  int i;
	  tree *puse;
	  FOR_EACH_VEC_ELT (*parm.body_uses, i, puse)
	    *puse = fld_idx;
	}
    }

  /* Re-write local vars, similarly.  */
  local_vars_transform xform_vars_data
    = {actor, actor_frame, coro_frame_type, loc, local_var_uses};
  cp_walk_tree (&fnbody, transform_local_var_uses, &xform_vars_data, NULL);

  tree resume_idx_name = get_identifier ("__resume_at");
  tree rat_field = lookup_member (coro_frame_type, resume_idx_name, 1, 0,
				  tf_warning_or_error);
  tree rat = build3 (COMPONENT_REF, short_unsigned_type_node, actor_frame,
		     rat_field, NULL_TREE);

  tree ret_label
    = create_named_label_with_ctx (loc, "actor.suspend.ret", actor);

  tree continue_label
    = create_named_label_with_ctx (loc, "actor.continue.ret", actor);

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
  b = coro_build_cvt_void_expr_stmt (b, loc);
  add_stmt (b);

  /* The destroy point numbered #1 is special, in that it is reached from a
     coroutine that is suspended after re-throwing from unhandled_exception().
     This label just invokes the cleanup of promise, param copies and the
     frame itself.  */
  tree del_promise_label
    = create_named_label_with_ctx (loc, "coro.delete.promise", actor);
  b = build_case_label (build_int_cst (short_unsigned_type_node, 1), NULL_TREE,
			create_anon_label_with_ctx (loc, actor));
  add_stmt (b);
  add_stmt (build_stmt (loc, GOTO_EXPR, del_promise_label));

  short unsigned lab_num = 3;
  for (unsigned destr_pt = 0; destr_pt < body_count; destr_pt++)
    {
      tree l_num = build_int_cst (short_unsigned_type_node, lab_num);
      b = build_case_label (l_num, NULL_TREE,
			    create_anon_label_with_ctx (loc, actor));
      add_stmt (b);
      b = build_call_expr_internal_loc (loc, IFN_CO_ACTOR, void_type_node, 1,
					l_num);
      b = coro_build_cvt_void_expr_stmt (b, loc);
      add_stmt (b);
      b = build1 (GOTO_EXPR, void_type_node, CASE_LABEL (ddeflab));
      add_stmt (b);
      lab_num += 2;
    }

  /* Insert the prototype dispatcher.  */
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
  b = coro_build_cvt_void_expr_stmt (b, loc);
  add_stmt (b);

  lab_num = 2;
  /* The final resume should be made to hit the default (trap, UB) entry
     although it will be unreachable via the normal entry point, since that
     is set to NULL on reaching final suspend.  */
  for (unsigned resu_pt = 0; resu_pt < body_count; resu_pt++)
    {
      tree l_num = build_int_cst (short_unsigned_type_node, lab_num);
      b = build_case_label (l_num, NULL_TREE,
			    create_anon_label_with_ctx (loc, actor));
      add_stmt (b);
      b = build_call_expr_internal_loc (loc, IFN_CO_ACTOR, void_type_node, 1,
					l_num);
      b = coro_build_cvt_void_expr_stmt (b, loc);
      add_stmt (b);
      b = build1 (GOTO_EXPR, void_type_node, CASE_LABEL (rdeflab));
      add_stmt (b);
      lab_num += 2;
    }

  /* Insert the prototype dispatcher.  */
  finish_switch_stmt (dispatcher);

  finish_if_stmt (lsb_if);

  tree r = build_stmt (loc, LABEL_EXPR, actor_begin_label);
  add_stmt (r);

  /* actor's version of the promise.  */
  tree ap_m = lookup_member (coro_frame_type, get_identifier ("__p"), 1, 0,
			     tf_warning_or_error);
  tree ap = build_class_member_access_expr (actor_frame, ap_m, NULL_TREE, false,
					    tf_warning_or_error);

  /* actor's coroutine 'self handle'.  */
  tree ash_m = lookup_member (coro_frame_type, get_identifier ("__self_h"), 1,
			      0, tf_warning_or_error);
  tree ash = build_class_member_access_expr (actor_frame, ash_m, NULL_TREE,
					     false, tf_warning_or_error);
  /* So construct the self-handle from the frame address.  */
  tree hfa_m = lookup_member (handle_type, coro_from_address_identifier, 1,
			      0, tf_warning_or_error);

  r = build1 (CONVERT_EXPR, build_pointer_type (void_type_node), actor_fp);
  vec<tree, va_gc> *args = make_tree_vector_single (r);
  tree hfa = build_new_method_call (ash, hfa_m, &args, NULL_TREE, LOOKUP_NORMAL,
				    NULL, tf_warning_or_error);
  r = build2 (INIT_EXPR, handle_type, ash, hfa);
  r = coro_build_cvt_void_expr_stmt (r, loc);
  add_stmt (r);
  release_tree_vector (args);

  /* Now we know the real promise, and enough about the frame layout to
     decide where to put things.  */

  await_xform_data xform
    = {actor, actor_frame, promise_proxy, ap, self_h_proxy, ash};

  /* Transform the await expressions in the function body.  Only do each
     await tree once!  */
  hash_set<tree> pset;
  cp_walk_tree (&fnbody, transform_await_wrapper, &xform, &pset);

  /* Now replace the promise proxy with its real value.  */
  proxy_replace p_data;
  p_data.from = promise_proxy;
  p_data.to = ap;
  cp_walk_tree (&fnbody, replace_proxy, &p_data, NULL);

  /* The rewrite of the function adds code to set the __resume field to
     nullptr when the coroutine is done and also the index to zero when
     calling an unhandled exception.  These are represented by two proxies
     in the function, so rewrite them to the proper frame access.  */
  tree resume_m
    = lookup_member (coro_frame_type, get_identifier ("__resume"),
		     /*protect=*/1, /*want_type=*/0, tf_warning_or_error);
  tree res_x = build_class_member_access_expr (actor_frame, resume_m, NULL_TREE,
					       false, tf_warning_or_error);
  p_data.from = resume_fn_field;
  p_data.to = res_x;
  cp_walk_tree (&fnbody, replace_proxy, &p_data, NULL);

  p_data.from = resume_idx_field;
  p_data.to = rat;
  cp_walk_tree (&fnbody, replace_proxy, &p_data, NULL);

  /* Add in our function body with the co_returns rewritten to final form.  */
  add_stmt (fnbody);

  /* now do the tail of the function.  */
  r = build_stmt (loc, LABEL_EXPR, del_promise_label);
  add_stmt (r);

  /* Destructors for the things we built explicitly.  */
  r = build_special_member_call (ap, complete_dtor_identifier, NULL,
				 promise_type, LOOKUP_NORMAL,
				 tf_warning_or_error);
  add_stmt (r);

  tree del_frame_label
    = create_named_label_with_ctx (loc, "coro.delete.frame", actor);
  r = build_stmt (loc, LABEL_EXPR, del_frame_label);
  add_stmt (r);

  /* Here deallocate the frame (if we allocated it), which we will have at
     present.  */
  tree fnf_m
    = lookup_member (coro_frame_type, get_identifier ("__frame_needs_free"), 1,
		     0, tf_warning_or_error);
  tree fnf2_x = build_class_member_access_expr (actor_frame, fnf_m, NULL_TREE,
						false, tf_warning_or_error);

  tree need_free_if = begin_if_stmt ();
  fnf2_x = build1 (CONVERT_EXPR, integer_type_node, fnf2_x);
  tree cmp = build2 (NE_EXPR, integer_type_node, fnf2_x, integer_zero_node);
  finish_if_stmt_cond (cmp, need_free_if);
  if (param_dtor_list != NULL)
    {
      int i;
      tree pid;
      FOR_EACH_VEC_ELT (*param_dtor_list, i, pid)
	{
	  tree m
	    = lookup_member (coro_frame_type, pid, 1, 0, tf_warning_or_error);
	  tree a = build_class_member_access_expr (actor_frame, m, NULL_TREE,
						   false, tf_warning_or_error);
	  tree t = TREE_TYPE (a);
	  tree dtor;
	  dtor
	    = build_special_member_call (a, complete_dtor_identifier, NULL, t,
					 LOOKUP_NORMAL, tf_warning_or_error);
	  add_stmt (dtor);
	}
    }

  /* Build the frame DTOR.  */
  tree del_coro_fr = coro_get_frame_dtor (actor_fp, orig, frame_size,
					  promise_type, loc);
  finish_expr_stmt (del_coro_fr);
  finish_then_clause (need_free_if);
  tree scope = IF_SCOPE (need_free_if);
  IF_SCOPE (need_free_if) = NULL;
  r = do_poplevel (scope);
  add_stmt (r);

  /* done.  */
  r = build_stmt (loc, RETURN_EXPR, NULL);
  TREE_NO_WARNING (r) |= 1; /* We don't want a warning about this.  */
  r = maybe_cleanup_point_expr_void (r);
  add_stmt (r);

  /* This is the suspend return point.  */
  r = build_stmt (loc, LABEL_EXPR, ret_label);
  add_stmt (r);

  r = build_stmt (loc, RETURN_EXPR, NULL);
  TREE_NO_WARNING (r) |= 1; /* We don't want a warning about this.  */
  r = maybe_cleanup_point_expr_void (r);
  add_stmt (r);

  /* This is the 'continuation' return point.  For such a case we have a coro
     handle (from the await_suspend() call) and we want handle.resume() to
     execute as a tailcall allowing arbitrary chaining of coroutines.  */
  r = build_stmt (loc, LABEL_EXPR, continue_label);
  add_stmt (r);

  /* We want to force a tail-call even for O0/1, so this expands the resume
     call into its underlying implementation.  */
  tree addr = lookup_member (void_coro_handle_type, coro_address_identifier,
			       1, 0, tf_warning_or_error);
  addr = build_new_method_call (continuation, addr, NULL, NULL_TREE,
				  LOOKUP_NORMAL, NULL, tf_warning_or_error);
  tree resume = build_call_expr_loc
    (loc, builtin_decl_explicit (BUILT_IN_CORO_RESUME), 1, addr);

  /* In order to support an arbitrary number of coroutine continuations,
     we must tail call them.  However, some targets do not support indirect
     tail calls to arbitrary callees.  See PR94359.  */
  CALL_EXPR_TAILCALL (resume) = true;
  resume = coro_build_cvt_void_expr_stmt (resume, loc);
  add_stmt (resume);

  r = build_stmt (loc, RETURN_EXPR, NULL);
  gcc_checking_assert (maybe_cleanup_point_expr_void (r) == r);
  add_stmt (r);

  /* We will need to know which resume point number should be encoded.  */
  tree res_idx_m
    = lookup_member (coro_frame_type, resume_idx_name,
		     /*protect=*/1, /*want_type=*/0, tf_warning_or_error);
  tree resume_pt_number
    = build_class_member_access_expr (actor_frame, res_idx_m, NULL_TREE, false,
				      tf_warning_or_error);

  /* We've now rewritten the tree and added the initial and final
     co_awaits.  Now pass over the tree and expand the co_awaits.  */

  coro_aw_data data = {actor, actor_fp, resume_pt_number, NULL_TREE,
		       ash, del_promise_label, ret_label,
		       continue_label, continuation, 2};
  cp_walk_tree (&actor_body, await_statement_expander, &data, NULL);

  BIND_EXPR_BODY (actor_bind) = pop_stmt_list (actor_body);
  TREE_SIDE_EFFECTS (actor_bind) = true;

  finish_compound_stmt (stmt);
  DECL_SAVED_TREE (actor) = pop_stmt_list (actor_outer);
  verify_stmt_tree (DECL_SAVED_TREE (actor));
}

/* The prototype 'destroy' function :
   frame->__resume_at |= 1;
   actor (frame);  */

static void
build_destroy_fn (location_t loc, tree coro_frame_type, tree destroy,
		  tree actor)
{
  /* One param, the coro frame pointer.  */
  tree destr_fp = DECL_ARGUMENTS (destroy);

  /* A void return.  */
  tree resdecl = build_decl (loc, RESULT_DECL, 0, void_type_node);
  DECL_ARTIFICIAL (resdecl) = 1;
  DECL_IGNORED_P (resdecl) = 1;
  DECL_RESULT (destroy) = resdecl;

  /* We have a definition here.  */
  TREE_STATIC (destroy) = 1;
  DECL_COROUTINE_P (destroy) = 1;

  tree destr_outer = push_stmt_list ();
  current_stmt_tree ()->stmts_are_full_exprs_p = 1;
  tree dstr_stmt = begin_compound_stmt (BCS_FN_BODY);

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
  r = coro_build_cvt_void_expr_stmt (r, loc);
  add_stmt (r);

  /* So .. call the actor ..  */
  r = build_call_expr_loc (loc, actor, 1, destr_fp);
  r = coro_build_cvt_void_expr_stmt (r, loc);
  add_stmt (r);

  /* done. */
  r = build_stmt (loc, RETURN_EXPR, NULL);
  r = maybe_cleanup_point_expr_void (r);
  add_stmt (r);

  finish_compound_stmt (dstr_stmt);
  DECL_SAVED_TREE (destroy) = pop_stmt_list (destr_outer);
}

/* Helper that returns an identifier for an appended extension to the
   current un-mangled function name.  */

static tree
get_fn_local_identifier (tree orig, const char *append)
{
  /* Figure out the bits we need to generate names for the outlined things
     For consistency, this needs to behave the same way as
     ASM_FORMAT_PRIVATE_NAME does. */
  tree nm = DECL_NAME (orig);
  const char *sep, *pfx = "";
#ifndef NO_DOT_IN_LABEL
  sep = ".";
#else
#ifndef NO_DOLLAR_IN_LABEL
  sep = "$";
#else
  sep = "_";
  pfx = "__";
#endif
#endif

  char *an;
  if (DECL_ASSEMBLER_NAME (orig))
    an = ACONCAT ((IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (orig)), sep, append,
		   (char *) 0));
  else if (DECL_USE_TEMPLATE (orig) && DECL_TEMPLATE_INFO (orig)
	   && DECL_TI_ARGS (orig))
    {
      tree tpl_args = DECL_TI_ARGS (orig);
      an = ACONCAT ((pfx, IDENTIFIER_POINTER (nm), (char *) 0));
      for (int i = 0; i < TREE_VEC_LENGTH (tpl_args); ++i)
	{
	  tree typ = DECL_NAME (TYPE_NAME (TREE_VEC_ELT (tpl_args, i)));
	  an = ACONCAT ((an, sep, IDENTIFIER_POINTER (typ), (char *) 0));
	}
      an = ACONCAT ((an, sep, append, (char *) 0));
    }
  else
    an = ACONCAT ((pfx, IDENTIFIER_POINTER (nm), sep, append, (char *) 0));

  return get_identifier (an);
}

/* Build an initial or final await initialized from the promise
   initial_suspend or final_suspend expression.  */

static tree
build_init_or_final_await (location_t loc, bool is_final)
{
  tree suspend_alt = is_final ? coro_final_suspend_identifier
			      : coro_initial_suspend_identifier;

  tree setup_call
    = coro_build_promise_expression (current_function_decl, NULL, suspend_alt,
				     loc, NULL, /*musthave=*/true);

  /* Check for noexcept on the final_suspend call.  */
  if (flag_exceptions && is_final && setup_call != error_mark_node
      && coro_diagnose_throwing_final_aw_expr (setup_call))
    return error_mark_node;

  /* So build the co_await for this */
  /* For initial/final suspends the call is "a" per [expr.await] 3.2.  */
  return build_co_await (loc, setup_call, (is_final ? FINAL_SUSPEND_POINT
						    : INITIAL_SUSPEND_POINT));
}

/* Callback to record the essential data for each await point found in the
   function.  */

static bool
register_await_info (tree await_expr, tree aw_type, tree aw_nam)
{
  bool seen;
  suspend_point_info &s
    = suspend_points->get_or_insert (await_expr, &seen);
  if (seen)
    {
      warning_at (EXPR_LOCATION (await_expr), 0, "duplicate info for %qE",
		await_expr);
      return false;
    }
  s.awaitable_type = aw_type;
  s.await_field_id = aw_nam;
  return true;
}

/* This data set is used when analyzing statements for await expressions.  */

struct susp_frame_data
{
  /* Function-wide.  */
  tree *field_list; /* The current coroutine frame field list.  */
  tree handle_type; /* The self-handle type for this coroutine.  */
  tree fs_label;    /* The destination for co_returns.  */
  vec<tree, va_gc> *block_stack; /* Track block scopes.  */
  vec<tree, va_gc> *bind_stack;  /* Track current bind expr.  */
  unsigned await_number;	 /* Which await in the function.  */
  unsigned cond_number;		 /* Which replaced condition in the fn.  */
  /* Temporary values for one statement or expression being analyzed.  */
  hash_set<tree> captured_temps; /* The suspend captured these temps.  */
  vec<tree, va_gc> *to_replace;  /* The VAR decls to replace.  */
  hash_set<tree> *truth_aoif_to_expand; /* The set of TRUTH exprs to expand.  */
  unsigned saw_awaits;		 /* Count of awaits in this statement  */
  bool captures_temporary;	 /* This expr captures temps by ref.  */
  bool needs_truth_if_exp;	 /* We must expand a truth_if expression.  */
  bool has_awaiter_init;	 /* We must handle initializing an awaiter.  */
};

/* If this is an await expression, then count it (both uniquely within the
   function and locally within a single statement).  */

static tree
register_awaits (tree *stmt, int *, void *d)
{
  tree aw_expr = *stmt;

  /* We should have already lowered co_yields to their co_await.  */
  gcc_checking_assert (TREE_CODE (aw_expr) != CO_YIELD_EXPR);

  if (TREE_CODE (aw_expr) != CO_AWAIT_EXPR)
    return NULL_TREE;

  /* Count how many awaits the current expression contains.  */
  susp_frame_data *data = (susp_frame_data *) d;
  data->saw_awaits++;
  /* Each await suspend context is unique, this is a function-wide value.  */
  data->await_number++;

  /* Awaitables should either be user-locals or promoted to coroutine frame
     entries at this point, and their initializers should have been broken
     out.  */
  tree aw = TREE_OPERAND (aw_expr, 1);
  gcc_checking_assert (!TREE_OPERAND (aw_expr, 2));

  tree aw_field_type = TREE_TYPE (aw);
  tree aw_field_nam = NULL_TREE;
  register_await_info (aw_expr, aw_field_type, aw_field_nam);

  /* Rewrite target expressions on the await_suspend () to remove extraneous
     cleanups for the awaitables, which are now promoted to frame vars and
     managed via that.  */
  tree v = TREE_OPERAND (aw_expr, 3);
  tree o = TREE_VEC_ELT (v, 1);
  if (TREE_CODE (o) == TARGET_EXPR)
    TREE_VEC_ELT (v, 1) = get_target_expr (TREE_OPERAND (o, 1));
  return NULL_TREE;
}

/* There are cases where any await expression is relevant.  */
static tree
find_any_await (tree *stmt, int *dosub, void *d)
{
  if (TREE_CODE (*stmt) == CO_AWAIT_EXPR)
    {
      *dosub = 0; /* We don't need to consider this any further.  */
      tree **p = (tree **) d;
      *p = stmt;
      return *stmt;
    }
  return NULL_TREE;
}

static bool
tmp_target_expr_p (tree t)
{
  if (TREE_CODE (t) != TARGET_EXPR)
    return false;
  tree v = TREE_OPERAND (t, 0);
  if (!DECL_ARTIFICIAL (v))
    return false;
  if (DECL_NAME (v))
    return false;
  return true;
}

/* Structure to record sub-expressions that need to be handled by the
   statement flattener.  */

struct coro_interesting_subtree
{
  tree* entry;
  hash_set<tree> *temps_used;
};

/* tree-walk callback that returns the first encountered sub-expression of
   a kind that needs to be handled specifically by the statement flattener.  */

static tree
find_interesting_subtree (tree *expr_p, int *dosub, void *d)
{
  tree expr = *expr_p;
  coro_interesting_subtree *p = (coro_interesting_subtree *)d;
  if (TREE_CODE (expr) == CO_AWAIT_EXPR)
    {
      *dosub = 0; /* We don't need to consider this any further.  */
      if (TREE_OPERAND (expr, 2))
	{
	  p->entry = expr_p;
	  return expr;
	}
    }
  else if (tmp_target_expr_p (expr)
	   && !p->temps_used->contains (expr))
    {
      p->entry = expr_p;
      return expr;
    }

  return NULL_TREE;
}

/* Node for a doubly-linked list of promoted variables and their
   initializers.  When the initializer is a conditional expression
   the 'then' and 'else' clauses are represented by a linked list
   attached to then_cl and else_cl respectively.  */

struct var_nest_node
{
  var_nest_node ()
    : var(NULL_TREE), init(NULL_TREE),
      prev(NULL), next(NULL), then_cl(NULL), else_cl(NULL) {}
  var_nest_node (tree v, tree i, var_nest_node *p, var_nest_node *n)
    : var(v), init(i), prev(p), next(n)
    {
      if (p)
	p->next = this;
      if (n)
	n->prev = this;
    }
  tree var;
  tree init;
  var_nest_node *prev;
  var_nest_node *next;
  var_nest_node *then_cl;
  var_nest_node *else_cl;
};

/* This is called for single statements from the co-await statement walker.
   It checks to see if the statement contains any initializers for awaitables
   and if any of these capture items by reference.  */

static void
flatten_await_stmt (var_nest_node *n, hash_set<tree> *promoted,
		    hash_set<tree> *temps_used, tree *replace_in)
{
  bool init_expr = false;
  switch (TREE_CODE (n->init))
    {
      default: break;
      /* Compound expressions must be flattened specifically.  */
      case COMPOUND_EXPR:
	{
	  tree first = TREE_OPERAND (n->init, 0);
	  n->init = TREE_OPERAND (n->init, 1);
	  var_nest_node *ins
	    = new var_nest_node(NULL_TREE, first, n->prev, n);
	  /* The compiler (but not the user) can generate temporaries with
	     uses in the second arm of a compound expr.  */
	  flatten_await_stmt (ins, promoted, temps_used, &n->init);
	  flatten_await_stmt (n, promoted, temps_used, NULL);
	  /* The two arms have been processed separately.  */
	  return;
	}
	break;
      /* Handle conditional expressions.  */
      case INIT_EXPR:
	init_expr = true;
	/* FALLTHROUGH */
      case MODIFY_EXPR:
	{
	  tree old_expr = TREE_OPERAND (n->init, 1);
	  if (TREE_CODE (old_expr) == COMPOUND_EXPR)
	    {
	      tree first = TREE_OPERAND (old_expr, 0);
	      TREE_OPERAND (n->init, 1) = TREE_OPERAND (old_expr, 1);
	      var_nest_node *ins
		= new var_nest_node(NULL_TREE, first, n->prev, n);
	      flatten_await_stmt (ins, promoted, temps_used,
				  &TREE_OPERAND (n->init, 1));
	      flatten_await_stmt (n, promoted, temps_used, NULL);
	      return;
	    }
	  if (TREE_CODE (old_expr) != COND_EXPR)
	    break;
	  /* Reconstruct x = t ? y : z;
	     as (void) t ? x = y : x = z;  */
	  tree var = TREE_OPERAND (n->init, 0);
	  tree var_type = TREE_TYPE (var);
	  tree cond = COND_EXPR_COND (old_expr);
	  /* We are allowed a void type throw in one or both of the cond
	     expr arms.  */
	  tree then_cl = COND_EXPR_THEN (old_expr);
	  if (!VOID_TYPE_P (TREE_TYPE (then_cl)))
	    {
	      gcc_checking_assert (TREE_CODE (then_cl) != STATEMENT_LIST);
	      then_cl
		= build2 (init_expr ? INIT_EXPR : MODIFY_EXPR, var_type,
			  var, then_cl);
	    }
	  tree else_cl = COND_EXPR_ELSE (old_expr);
	  if (!VOID_TYPE_P (TREE_TYPE (else_cl)))
	    {
	      gcc_checking_assert (TREE_CODE (then_cl) != STATEMENT_LIST);
	      else_cl
		= build2 (init_expr ? INIT_EXPR : MODIFY_EXPR, var_type,
			  var, else_cl);
	    }
	  n->init = build3 (COND_EXPR, var_type, cond, then_cl, else_cl);
	}
	/* FALLTHROUGH */
      case COND_EXPR:
	{
	  tree *found;
	  tree cond = COND_EXPR_COND (n->init);
	  /* If the condition contains an await expression, then we need to
	     set that first and use a separate var.  */
	  if (cp_walk_tree (&cond, find_any_await, &found, NULL))
	    {
	      tree cond_type = TREE_TYPE (cond);
	      tree cond_var  = build_lang_decl (VAR_DECL, NULL_TREE, cond_type);
	      DECL_ARTIFICIAL (cond_var) = true;
	      layout_decl (cond_var, 0);
	      gcc_checking_assert (!TYPE_NEEDS_CONSTRUCTING (cond_type));
	      cond = build2 (INIT_EXPR, cond_type, cond_var, cond);
	      var_nest_node *ins
		= new var_nest_node (cond_var, cond, n->prev, n);
	      COND_EXPR_COND (n->init) = cond_var;
	      flatten_await_stmt (ins, promoted, temps_used, NULL);
	    }

	  n->then_cl
	    = new var_nest_node (n->var, COND_EXPR_THEN (n->init), NULL, NULL);
	  n->else_cl
	    = new var_nest_node (n->var, COND_EXPR_ELSE (n->init), NULL, NULL);
	  flatten_await_stmt (n->then_cl, promoted, temps_used, NULL);
	  /* Point to the start of the flattened code.  */
	  while (n->then_cl->prev)
	    n->then_cl = n->then_cl->prev;
	  flatten_await_stmt (n->else_cl, promoted, temps_used, NULL);
	  while (n->else_cl->prev)
	    n->else_cl = n->else_cl->prev;
	  return;
	}
	break;
    }
  coro_interesting_subtree v = { NULL, temps_used };
  tree t = cp_walk_tree (&n->init, find_interesting_subtree, (void *)&v, NULL);
  if (!t)
    return;
  switch (TREE_CODE (t))
    {
      default: break;
      case CO_AWAIT_EXPR:
	{
	  /* Await expressions with initializers have a compiler-temporary
	     as the awaitable.  'promote' this.  */
	  tree var = TREE_OPERAND (t, 1);
	  bool already_present = promoted->add (var);
	  gcc_checking_assert (!already_present);
	  tree init = TREE_OPERAND (t, 2);
	  switch (TREE_CODE (init))
	    {
	      default: break;
	      case INIT_EXPR:
	      case MODIFY_EXPR:
		{
		  tree inner = TREE_OPERAND (init, 1);
		  /* We can have non-lvalue-expressions here, but when we see
		     a target expression, mark it as already used.  */
		  if (TREE_CODE (inner) == TARGET_EXPR)
		    {
		      temps_used->add (inner);
		      gcc_checking_assert
			(TREE_CODE (TREE_OPERAND (inner, 1)) != COND_EXPR);
		    }
		}
		break;
	      case CALL_EXPR:
		/* If this is a call and not a CTOR, then we didn't expect it.  */
		gcc_checking_assert
		  (DECL_CONSTRUCTOR_P (TREE_OPERAND (CALL_EXPR_FN (init), 0)));
		break;
	    }
	  var_nest_node *ins = new var_nest_node (var, init, n->prev, n);
	  TREE_OPERAND (t, 2) = NULL_TREE;
	  flatten_await_stmt (ins, promoted, temps_used, NULL);
	  flatten_await_stmt (n, promoted, temps_used, NULL);
	  return;
	}
	break;
      case TARGET_EXPR:
	{
	  /* We have a temporary; promote it, but allow for the idiom in code
	     generated by the compiler like
	     a = (target_expr produces temp, op uses temp).  */
	  tree init = t;
	  temps_used->add (init);
	  tree var_type = TREE_TYPE (init);
	  char *buf = xasprintf ("D.%d", DECL_UID (TREE_OPERAND (init, 0)));
	  tree var = build_lang_decl (VAR_DECL, get_identifier (buf), var_type);
	  DECL_ARTIFICIAL (var) = true;
	  free (buf);
	  bool already_present = promoted->add (var);
	  gcc_checking_assert (!already_present);
	  tree inner = TREE_OPERAND (init, 1);
	  gcc_checking_assert (TREE_CODE (inner) != COND_EXPR);
	  if (type_build_ctor_call (var_type))
	    {
	      releasing_vec p_in (make_tree_vector_single (init));
	      init = build_special_member_call (var, complete_ctor_identifier,
						&p_in, var_type, LOOKUP_NORMAL,
						tf_warning_or_error);
	    }
	  else
	    init = build2 (INIT_EXPR, var_type, var, init);
	  /* Simplify for the case that we have an init containing the temp
	     alone.  */
	  if (t == n->init && n->var == NULL_TREE)
	    {
	      n->var = var;
	      proxy_replace pr = {TREE_OPERAND (t, 0), var};
	      cp_walk_tree (&init, replace_proxy, &pr, NULL);
	      n->init = init;
	      if (replace_in)
		cp_walk_tree (replace_in, replace_proxy, &pr, NULL);
	      flatten_await_stmt (n, promoted, temps_used, NULL);
	    }
	  else
	    {
	      var_nest_node *ins
		= new var_nest_node (var, init, n->prev, n);
	      /* We have to replace the target expr... */
	      *v.entry = var;
	      /* ... and any uses of its var.  */
	      proxy_replace pr = {TREE_OPERAND (t, 0), var};
	      cp_walk_tree (&n->init, replace_proxy, &pr, NULL);
	      /* Compiler-generated temporaries can also have uses in
		 following arms of compound expressions, which will be listed
		 in 'replace_in' if present.  */
	      if (replace_in)
		cp_walk_tree (replace_in, replace_proxy, &pr, NULL);
	      flatten_await_stmt (ins, promoted, temps_used, NULL);
	      flatten_await_stmt (n, promoted, temps_used, NULL);
	    }
	  return;
	}
	break;
    }
}

/* Helper for 'process_conditional' that handles recursion into nested
   conditionals.  */

static void
handle_nested_conditionals (var_nest_node *n, vec<tree>& list,
			    hash_map<tree, tree>& map)
{
  do
    {
      if (n->var && DECL_NAME (n->var))
	{
	  list.safe_push (n->var);
	  if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (TREE_TYPE (n->var)))
	    {
	      bool existed;
	      tree& flag = map.get_or_insert (n->var, &existed);
	      if (!existed)
		{
		  /* We didn't see this var before and it needs a DTOR, so
		     build a guard variable for it.  */
		  char *nam
		    = xasprintf ("%s_guard",
				 IDENTIFIER_POINTER (DECL_NAME (n->var)));
		  flag = build_lang_decl (VAR_DECL, get_identifier (nam),
					  boolean_type_node);
		  free (nam);
		  DECL_ARTIFICIAL (flag) = true;
		}

	      /* The initializer for this variable is replaced by a compound
		 expression that performs the init and then records that the
		 variable is live (and the DTOR should be run at the scope
		 exit.  */
	      tree set_flag = build2 (INIT_EXPR, boolean_type_node,
				      flag, boolean_true_node);
	      n->init
		= build2 (COMPOUND_EXPR, boolean_type_node, n->init, set_flag);
	}
	}
      if (TREE_CODE (n->init) == COND_EXPR)
	{
	  tree new_then = push_stmt_list ();
	  handle_nested_conditionals (n->then_cl, list, map);
	  new_then = pop_stmt_list (new_then);
	  tree new_else = push_stmt_list ();
	  handle_nested_conditionals (n->else_cl, list, map);
	  new_else = pop_stmt_list (new_else);
	  tree new_if
	    = build4 (IF_STMT, void_type_node, COND_EXPR_COND (n->init),
		      new_then, new_else, NULL_TREE);
	  add_stmt (new_if);
	}
      else
	finish_expr_stmt (n->init);
      n = n->next;
    } while (n);
}

/* helper for 'maybe_promote_temps'.

   When we have a conditional expression which might embed await expressions
   and/or promoted variables, we need to handle it appropriately.

   The linked lists for the 'then' and 'else' clauses in a conditional node
   identify the promoted variables (but these cannot be wrapped in a regular
   cleanup).

   So recurse through the lists and build up a composite list of captured vars.
   Declare these and any guard variables needed to decide if a DTOR should be
   run.  Then embed the conditional into a try-finally expression that handles
   running each DTOR conditionally on its guard variable.  */

static void
process_conditional (var_nest_node *n, tree& vlist)
{
  tree init = n->init;
  hash_map<tree, tree> var_flags;
  vec<tree> var_list = vNULL;
  tree new_then = push_stmt_list ();
  handle_nested_conditionals (n->then_cl, var_list, var_flags);
  new_then = pop_stmt_list (new_then);
  tree new_else = push_stmt_list ();
  handle_nested_conditionals (n->else_cl, var_list, var_flags);
  new_else = pop_stmt_list (new_else);
  /* Declare the vars.  There are two loops so that the boolean flags are
     grouped in the frame.  */
  for (unsigned i = 0; i < var_list.length(); i++)
    {
      tree var = var_list[i];
      DECL_CHAIN (var) = vlist;
      vlist = var;
      add_decl_expr (var);
    }
  /* Define the guard flags for variables that need a DTOR.  */
  for (unsigned i = 0; i < var_list.length(); i++)
    {
      tree *flag = var_flags.get (var_list[i]);
      if (flag)
	{
	  DECL_INITIAL (*flag) = boolean_false_node;
	  DECL_CHAIN (*flag) = vlist;
	  vlist = *flag;
	  add_decl_expr (*flag);
	}
    }
  tree new_if
    = build4 (IF_STMT, void_type_node, COND_EXPR_COND (init),
	      new_then, new_else, NULL_TREE);
  /* Build a set of conditional DTORs.  */
  tree final_actions = push_stmt_list ();
  while (!var_list.is_empty())
    {
      tree var = var_list.pop ();
      tree *flag = var_flags.get (var);
      if (!flag)
	continue;
      tree var_type = TREE_TYPE (var);
      tree cleanup
	= build_special_member_call (var, complete_dtor_identifier,
				     NULL, var_type, LOOKUP_NORMAL,
				     tf_warning_or_error);
      tree cond_cleanup = begin_if_stmt ();
      finish_if_stmt_cond (*flag, cond_cleanup);
      finish_expr_stmt (cleanup);
      finish_then_clause (cond_cleanup);
      finish_if_stmt (cond_cleanup);
    }
  final_actions = pop_stmt_list (final_actions);
  tree try_finally
    = build2 (TRY_FINALLY_EXPR, void_type_node, new_if, final_actions);
  add_stmt (try_finally);
}

/* Given *STMT, that contains at least one await expression.

   The full expression represented in the original source code will contain
   suspension points, but it is still required that the lifetime of temporary
   values extends to the end of the expression.

   We already have a mechanism to 'promote' user-authored local variables
   to a coroutine frame counterpart (which allows explicit management of the
   lifetime across suspensions).  The transform here re-writes STMT into
   a bind expression, promotes temporary values into local variables in that
   and flattens the statement into a series of cleanups.

   Conditional expressions are re-written to regular 'if' statements.
   The cleanups for variables initialized inside a conditional (including
   nested cases) are wrapped in a try-finally clause, with guard variables
   to determine which DTORs need to be run.  */

static tree
maybe_promote_temps (tree *stmt, void *d)
{
  susp_frame_data *awpts = (susp_frame_data *) d;

  location_t sloc = EXPR_LOCATION (*stmt);
  tree expr = *stmt;
  /* Strip off uninteresting wrappers.  */
  if (TREE_CODE (expr) == CLEANUP_POINT_EXPR)
    expr = TREE_OPERAND (expr, 0);
  if (TREE_CODE (expr) == EXPR_STMT)
    expr = EXPR_STMT_EXPR (expr);
  if (TREE_CODE (expr) == CONVERT_EXPR
      && VOID_TYPE_P (TREE_TYPE (expr)))
    expr = TREE_OPERAND (expr, 0);
  STRIP_NOPS (expr);

  /* We walk the statement trees, flattening it into an ordered list of
     variables with initializers and fragments corresponding to compound
     expressions, truth or/and if and ternary conditionals.  Conditional
     expressions carry a nested list of fragments for the then and else
     clauses.  We anchor to the 'bottom' of the fragment list; we will write
     a cleanup nest with one shell for each variable initialized.  */
  var_nest_node *root = new var_nest_node (NULL_TREE, expr, NULL, NULL);
  /* Check to see we didn't promote one twice.  */
  hash_set<tree> promoted_vars;
  hash_set<tree> used_temps;
  flatten_await_stmt (root, &promoted_vars, &used_temps, NULL);

  gcc_checking_assert (root->next == NULL);
  tree vlist = NULL_TREE;
  var_nest_node *t = root;
  /* We build the bind scope expression from the bottom-up.
     EXPR_LIST holds the inner expression nest at the current cleanup
     level (becoming the final expression list when we've exhausted the
     number of sub-expression fragments).  */
  tree expr_list = NULL_TREE;
  do
    {
      tree new_list = push_stmt_list ();
      /* When we have a promoted variable, then add that to the bind scope
	 and initialize it.  When there's no promoted variable, we just need
	 to run the initializer.
	 If the initializer is a conditional expression, we need to collect
	 and declare any promoted variables nested within it.  DTORs for such
	 variables must be run conditionally too.  */
      if (t->var && DECL_NAME (t->var))
	{
	  tree var = t->var;
	  DECL_CHAIN (var) = vlist;
	  vlist = var;
	  add_decl_expr (var);
	  if (TREE_CODE (t->init) == COND_EXPR)
	    process_conditional (t, vlist);
	  else
	    finish_expr_stmt (t->init);
	  tree var_type = TREE_TYPE (var);
	  if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (var_type))
	    {
	      tree cleanup
		= build_special_member_call (var, complete_dtor_identifier,
					     NULL, var_type, LOOKUP_NORMAL,
					     tf_warning_or_error);
	      tree cl = build_stmt (sloc, CLEANUP_STMT, expr_list, cleanup, var);
	      add_stmt (cl); /* push this onto the level above.  */
	    }
	  else if (expr_list)
	    {
	      if (TREE_CODE (expr_list) != STATEMENT_LIST)
		add_stmt (expr_list);
	      else if (!tsi_end_p (tsi_start (expr_list)))
		add_stmt (expr_list);
	    }
	}
      else
	{
	  if (TREE_CODE (t->init) == COND_EXPR)
	    process_conditional (t, vlist);
	  else
	    finish_expr_stmt (t->init);
	  if (expr_list)
	    {
	      if (TREE_CODE (expr_list) != STATEMENT_LIST)
		add_stmt (expr_list);
	      else if (!tsi_end_p (tsi_start (expr_list)))
		add_stmt (expr_list);
	    }
	}
      expr_list = pop_stmt_list (new_list);
      var_nest_node *old = t;
      t = t->prev;
      delete old;
    } while (t);

  /* Now produce the bind expression containing the 'promoted' temporaries
     as its variable list, and the cleanup nest as the statement.  */
  tree await_bind = build3_loc (sloc, BIND_EXPR, void_type_node,
				NULL, NULL, NULL);
  BIND_EXPR_BODY (await_bind) = expr_list;
  BIND_EXPR_VARS (await_bind) = nreverse (vlist);
  tree b_block = make_node (BLOCK);
  if (!awpts->block_stack->is_empty ())
    {
      tree s_block = awpts->block_stack->last ();
      if (s_block)
	{
	BLOCK_SUPERCONTEXT (b_block) = s_block;
	BLOCK_CHAIN (b_block) = BLOCK_SUBBLOCKS (s_block);
	BLOCK_SUBBLOCKS (s_block) = b_block;
	}
    }
  BLOCK_VARS (b_block) = BIND_EXPR_VARS (await_bind) ;
  BIND_EXPR_BLOCK (await_bind) = b_block;
  TREE_SIDE_EFFECTS (await_bind) = TREE_SIDE_EFFECTS (BIND_EXPR_BODY (await_bind));
  *stmt = await_bind;
  hash_set<tree> visited;
  return cp_walk_tree (stmt, register_awaits, d, &visited);
}

/* Lightweight callback to determine two key factors:
   1) If the statement/expression contains any await expressions.
   2) If the statement/expression potentially requires a re-write to handle
      TRUTH_{AND,OR}IF_EXPRs since, in most cases, they will need expansion
      so that the await expressions are not processed in the case of the
      short-circuit arm.

   CO_YIELD expressions are re-written to their underlying co_await.  */

static tree
analyze_expression_awaits (tree *stmt, int *do_subtree, void *d)
{
  susp_frame_data *awpts = (susp_frame_data *) d;

  switch (TREE_CODE (*stmt))
    {
      default: return NULL_TREE;
      case CO_YIELD_EXPR:
	/* co_yield is syntactic sugar, re-write it to co_await.  */
	*stmt = TREE_OPERAND (*stmt, 1);
	/* FALLTHROUGH */
      case CO_AWAIT_EXPR:
	awpts->saw_awaits++;
	/* A non-null initializer for the awaiter means we need to expand.  */
	if (TREE_OPERAND (*stmt, 2))
	  awpts->has_awaiter_init = true;
	break;
      case TRUTH_ANDIF_EXPR:
      case TRUTH_ORIF_EXPR:
	{
	  /* We don't need special action for awaits in the always-executed
	     arm of a TRUTH_IF.  */
	  if (tree res = cp_walk_tree (&TREE_OPERAND (*stmt, 0),
				       analyze_expression_awaits, d, NULL))
	    return res;
	  /* However, if there are await expressions on the conditionally
	     executed branch, we must expand the TRUTH_IF to ensure that the
	     expanded await expression control-flow is fully contained in the
	     conditionally executed code.  */
	  unsigned aw_count = awpts->saw_awaits;
	  if (tree res = cp_walk_tree (&TREE_OPERAND (*stmt, 1),
				       analyze_expression_awaits, d, NULL))
	    return res;
	  if (awpts->saw_awaits > aw_count)
	    {
	      awpts->truth_aoif_to_expand->add (*stmt);
	      awpts->needs_truth_if_exp = true;
	    }
	  /* We've done the sub-trees here.  */
	  *do_subtree = 0;
	}
	break;
    }

  return NULL_TREE; /* Recurse until done.  */
}

/* Given *EXPR
   If EXPR contains a TRUTH_{AND,OR}IF_EXPR, TAOIE with an await expr on
   the conditionally executed branch, change this in a ternary operator.

   bool not_expr = TAOIE == TRUTH_ORIF_EXPR ? NOT : NOP;
   not_expr (always-exec expr) ? conditionally-exec expr : not_expr;

   Apply this recursively to the condition and the conditionally-exec
   branch.  */

struct truth_if_transform {
  tree *orig_stmt;
  tree scratch_var;
  hash_set<tree> *truth_aoif_to_expand;
};

static tree
expand_one_truth_if (tree *expr, int *do_subtree, void *d)
{
  truth_if_transform *xform = (truth_if_transform *) d;

  bool needs_not = false;
  switch (TREE_CODE (*expr))
    {
      default: break;
      case TRUTH_ORIF_EXPR:
	needs_not = true;
	/* FALLTHROUGH */
      case TRUTH_ANDIF_EXPR:
	{
	  if (!xform->truth_aoif_to_expand->contains (*expr))
	    break;

	  location_t sloc = EXPR_LOCATION (*expr);
	  /* Transform truth expression into a cond expression with
	     * the always-executed arm as the condition.
	     * the conditionally-executed arm as the then clause.
	     * the 'else' clause is fixed: 'true' for ||,'false' for &&.  */
	  tree cond = TREE_OPERAND (*expr, 0);
	  tree test1 = TREE_OPERAND (*expr, 1);
	  tree fixed = needs_not ? boolean_true_node : boolean_false_node;
	  if (needs_not)
	    cond = build1 (TRUTH_NOT_EXPR, boolean_type_node, cond);
	  tree cond_expr
	    = build3_loc (sloc, COND_EXPR, boolean_type_node,
			  cond, test1, fixed);
	  *expr = cond_expr;
	  if (tree res = cp_walk_tree (&COND_EXPR_COND (*expr),
				       expand_one_truth_if, d, NULL))
	    return res;
	  if (tree res = cp_walk_tree (&COND_EXPR_THEN (*expr),
				       expand_one_truth_if, d, NULL))
	    return res;
	  /* We've manually processed necessary sub-trees here.  */
	  *do_subtree = 0;
	}
	break;
    }
  return NULL_TREE;
}

/* Helper that adds a new variable of VAR_TYPE to a bind scope BIND, the
   name is made up from NAM_ROOT, NAM_VERS.  */

static tree
add_var_to_bind (tree& bind, tree var_type,
		 const char *nam_root, unsigned nam_vers)
{
  tree b_vars = BIND_EXPR_VARS (bind);
  /* Build a variable to hold the condition, this will be included in the
     frame as a local var.  */
  char *nam = xasprintf ("%s.%d", nam_root, nam_vers);
  tree newvar = build_lang_decl (VAR_DECL, get_identifier (nam), var_type);
  free (nam);
  DECL_CHAIN (newvar) = b_vars;
  BIND_EXPR_VARS (bind) = newvar;
  return newvar;
}

/* Helper to build and add if (!cond) break;  */

static void
coro_build_add_if_not_cond_break (tree cond)
{
  tree if_stmt = begin_if_stmt ();
  tree invert = build1 (TRUTH_NOT_EXPR, boolean_type_node, cond);
  finish_if_stmt_cond (invert, if_stmt);
  finish_break_stmt ();
  finish_then_clause (if_stmt);
  finish_if_stmt (if_stmt);
}

/* Tree walk callback to replace continue statements with goto label.  */
static tree
replace_continue (tree *stmt, int *do_subtree, void *d)
{
  tree expr = *stmt;
  if (TREE_CODE (expr) == CLEANUP_POINT_EXPR)
    expr = TREE_OPERAND (expr, 0);
  if (CONVERT_EXPR_P (expr) && VOID_TYPE_P (expr))
    expr = TREE_OPERAND (expr, 0);
  STRIP_NOPS (expr);
  if (!STATEMENT_CLASS_P (expr))
    return NULL_TREE;

  switch (TREE_CODE (expr))
    {
      /* Unless it's a special case, just walk the subtrees as usual.  */
      default: return NULL_TREE;

      case CONTINUE_STMT:
	{
	  tree *label = (tree *)d;
	  location_t loc = EXPR_LOCATION (expr);
	  /* re-write a continue to goto label.  */
	  *stmt = build_stmt (loc, GOTO_EXPR, *label);
	  *do_subtree = 0;
	  return NULL_TREE;
	}

      /* Statements that do not require recursion.  */
      case DECL_EXPR:
      case BREAK_STMT:
      case GOTO_EXPR:
      case LABEL_EXPR:
      case CASE_LABEL_EXPR:
      case ASM_EXPR:
      /* These must break recursion.  */
      case FOR_STMT:
      case WHILE_STMT:
      case DO_STMT:
	*do_subtree = 0;
	return NULL_TREE;
    }
}

/* Tree walk callback to analyze, register and pre-process statements that
   contain await expressions.  */

static tree
await_statement_walker (tree *stmt, int *do_subtree, void *d)
{
  tree res = NULL_TREE;
  susp_frame_data *awpts = (susp_frame_data *) d;

  /* Process a statement at a time.  */
  if (TREE_CODE (*stmt) == BIND_EXPR)
    {
      /* For conditional expressions, we might wish to add an artificial var
	 to their containing bind expr.  */
      vec_safe_push (awpts->bind_stack, *stmt);
      /* We might need to insert a new bind expression, and want to link it
	 into the correct scope, so keep a note of the current block scope.  */
      tree blk = BIND_EXPR_BLOCK (*stmt);
      vec_safe_push (awpts->block_stack, blk);
      res = cp_walk_tree (&BIND_EXPR_BODY (*stmt), await_statement_walker,
			  d, NULL);
      awpts->block_stack->pop ();
      awpts->bind_stack->pop ();
      *do_subtree = 0; /* Done subtrees.  */
      return res;
    }
  else if (TREE_CODE (*stmt) == STATEMENT_LIST)
    {
      tree_stmt_iterator i;
      for (i = tsi_start (*stmt); !tsi_end_p (i); tsi_next (&i))
	{
	  res = cp_walk_tree (tsi_stmt_ptr (i), await_statement_walker,
			      d, NULL);
	  if (res)
	    return res;
	}
      *do_subtree = 0; /* Done subtrees.  */
      return NULL_TREE;
    }

  /* We have something to be handled as a single statement.  */
  bool has_cleanup_wrapper = TREE_CODE (*stmt) == CLEANUP_POINT_EXPR;
  hash_set<tree> visited;
  awpts->saw_awaits = 0;
  hash_set<tree> truth_aoif_to_expand;
  awpts->truth_aoif_to_expand = &truth_aoif_to_expand;
  awpts->needs_truth_if_exp = false;
  awpts->has_awaiter_init = false;
  tree expr = *stmt;
  if (has_cleanup_wrapper)
    expr = TREE_OPERAND (expr, 0);
  STRIP_NOPS (expr);

  if (STATEMENT_CLASS_P (expr))
    switch (TREE_CODE (expr))
      {
	/* Unless it's a special case, just walk the subtrees as usual.  */
	default: return NULL_TREE;

	/* When we have a conditional expression, which contains one or more
	   await expressions, we have to break the condition out into a
	   regular statement so that the control flow introduced by the await
	   transforms can be implemented.  */
	case IF_STMT:
	  {
	    /* Transform 'if (cond with awaits) then stmt1 else stmt2' into
	       bool cond = cond with awaits.
	       if (cond) then stmt1 else stmt2.  */
	    tree if_stmt = *stmt;
	    /* We treat the condition as if it was a stand-alone statement,
	       to see if there are any await expressions which will be analysed
	       and registered.  */
	    if ((res = cp_walk_tree (&IF_COND (if_stmt),
		analyze_expression_awaits, d, &visited)))
	      return res;
	    if (!awpts->saw_awaits)
	      return NULL_TREE; /* Nothing special to do here.  */

	    gcc_checking_assert (!awpts->bind_stack->is_empty());
	    tree& bind_expr = awpts->bind_stack->last ();
	    tree newvar = add_var_to_bind (bind_expr, boolean_type_node,
					   "ifcd", awpts->cond_number++);
	    tree insert_list = push_stmt_list ();
	    tree cond_inner = IF_COND (if_stmt);
	    if (TREE_CODE (cond_inner) == CLEANUP_POINT_EXPR)
	      cond_inner = TREE_OPERAND (cond_inner, 0);
	    add_decl_expr (newvar);
	    location_t sloc = EXPR_LOCATION (IF_COND (if_stmt));
	    /* We want to initialize the new variable with the expression
	       that contains the await(s) and potentially also needs to
	       have truth_if expressions expanded.  */
	    tree new_s = build2_loc (sloc, MODIFY_EXPR, boolean_type_node,
				     newvar, cond_inner);
	    finish_expr_stmt (new_s);
	    IF_COND (if_stmt) = newvar;
	    add_stmt (if_stmt);
	    *stmt = pop_stmt_list (insert_list);
	    /* So now walk the new statement list.  */
	    res = cp_walk_tree (stmt, await_statement_walker, d, NULL);
	    *do_subtree = 0; /* Done subtrees.  */
	    return res;
	  }
	  break;
	case FOR_STMT:
	  {
	    /* for loops only need special treatment if the condition or the
	       iteration expression contain a co_await.  */
	    tree for_stmt = *stmt;
	    /* Sanity check.  */
	    if ((res = cp_walk_tree (&FOR_INIT_STMT (for_stmt),
		analyze_expression_awaits, d, &visited)))
	      return res;
	    gcc_checking_assert (!awpts->saw_awaits);

	    if ((res = cp_walk_tree (&FOR_COND (for_stmt),
		analyze_expression_awaits, d, &visited)))
	      return res;
	    bool for_cond_await = awpts->saw_awaits != 0;
	    unsigned save_awaits = awpts->saw_awaits;

	    if ((res = cp_walk_tree (&FOR_EXPR (for_stmt),
		analyze_expression_awaits, d, &visited)))
	      return res;
	    bool for_expr_await = awpts->saw_awaits > save_awaits;

	    /* If the condition has an await, then we will need to rewrite the
	       loop as
	       for (init expression;true;iteration expression) {
		  condition = await expression;
		  if (condition)
		    break;
		  ...
		}
	    */
	    if (for_cond_await)
	      {
		tree insert_list = push_stmt_list ();
		/* This will be expanded when the revised body is handled.  */
		coro_build_add_if_not_cond_break (FOR_COND (for_stmt));
		/* .. add the original for body.  */
		add_stmt (FOR_BODY (for_stmt));
		/* To make the new for body.  */
		FOR_BODY (for_stmt) = pop_stmt_list (insert_list);
		FOR_COND (for_stmt) = boolean_true_node;
	      }
	    /* If the iteration expression has an await, it's a bit more
	       tricky.
	       for (init expression;condition;) {
		 ...
		 iteration_expr_label:
		   iteration expression with await;
	       }
	       but, then we will need to re-write any continue statements into
	       'goto iteration_expr_label:'.
	    */
	    if (for_expr_await)
	      {
		location_t sloc = EXPR_LOCATION (FOR_EXPR (for_stmt));
		tree insert_list = push_stmt_list ();
		/* The original for body.  */
		add_stmt (FOR_BODY (for_stmt));
		char *buf = xasprintf ("for.iter.expr.%u", awpts->cond_number++);
		tree it_expr_label
		  = create_named_label_with_ctx (sloc, buf, NULL_TREE);
		free (buf);
		add_stmt (build_stmt (sloc, LABEL_EXPR, it_expr_label));
		add_stmt (FOR_EXPR (for_stmt));
		FOR_EXPR (for_stmt) = NULL_TREE;
		FOR_BODY (for_stmt) = pop_stmt_list (insert_list);
		/* rewrite continue statements to goto label.  */
		hash_set<tree> visited_continue;
		if ((res = cp_walk_tree (&FOR_BODY (for_stmt),
		     replace_continue, &it_expr_label, &visited_continue)))
		  return res;
	      }

	    /* So now walk the body statement (list), if there were no await
	       expressions, then this handles the original body - and either
	       way we will have finished with this statement.  */
	    res = cp_walk_tree (&FOR_BODY (for_stmt),
				await_statement_walker, d, NULL);
	    *do_subtree = 0; /* Done subtrees.  */
	    return res;
	  }
	  break;
	case WHILE_STMT:
	  {
	    /* We turn 'while (cond with awaits) stmt' into
	       while (true) {
		  if (!(cond with awaits))
		    break;
		  stmt..
		} */
	    tree while_stmt = *stmt;
	    if ((res = cp_walk_tree (&WHILE_COND (while_stmt),
		analyze_expression_awaits, d, &visited)))
	      return res;
	    if (!awpts->saw_awaits)
	      return NULL_TREE; /* Nothing special to do here.  */

	    tree insert_list = push_stmt_list ();
	    coro_build_add_if_not_cond_break (WHILE_COND (while_stmt));
	    /* The original while body.  */
	    add_stmt (WHILE_BODY (while_stmt));
	    /* The new while body.  */
	    WHILE_BODY (while_stmt) = pop_stmt_list (insert_list);
	    WHILE_COND (while_stmt) = boolean_true_node;
	    /* So now walk the new statement list.  */
	    res = cp_walk_tree (&WHILE_BODY (while_stmt),
				await_statement_walker, d, NULL);
	    *do_subtree = 0; /* Done subtrees.  */
	    return res;
	  }
	  break;
	case DO_STMT:
	  {
	    /* We turn do stmt while (cond with awaits) into:
	       do {
		  stmt..
		  if (!(cond with awaits))
		    break;
	       } while (true); */
	    tree do_stmt = *stmt;
	    if ((res = cp_walk_tree (&DO_COND (do_stmt),
		analyze_expression_awaits, d, &visited)))
	      return res;
	    if (!awpts->saw_awaits)
	      return NULL_TREE; /* Nothing special to do here.  */

	    tree insert_list = push_stmt_list ();
	    /* The original do stmt body.  */
	    add_stmt (DO_BODY (do_stmt));
	    coro_build_add_if_not_cond_break (DO_COND (do_stmt));
	    /* The new while body.  */
	    DO_BODY (do_stmt) = pop_stmt_list (insert_list);
	    DO_COND (do_stmt) = boolean_true_node;
	    /* So now walk the new statement list.  */
	    res = cp_walk_tree (&DO_BODY (do_stmt), await_statement_walker,
				d, NULL);
	    *do_subtree = 0; /* Done subtrees.  */
	    return res;
	  }
	  break;
	case SWITCH_STMT:
	  {
	    /* We turn 'switch (cond with awaits) stmt' into
	       switch_type cond = cond with awaits
	       switch (cond) stmt.  */
	    tree sw_stmt = *stmt;
	    if ((res = cp_walk_tree (&SWITCH_STMT_COND (sw_stmt),
		analyze_expression_awaits, d, &visited)))
	      return res;
	    if (!awpts->saw_awaits)
	      return NULL_TREE; /* Nothing special to do here.  */

	    gcc_checking_assert (!awpts->bind_stack->is_empty());
	    /* Build a variable to hold the condition, this will be
		   included in the frame as a local var.  */
	    tree& bind_expr = awpts->bind_stack->last ();
	    tree sw_type = SWITCH_STMT_TYPE (sw_stmt);
	    tree newvar = add_var_to_bind (bind_expr, sw_type, "swch",
					   awpts->cond_number++);
	    tree insert_list = push_stmt_list ();
	    add_decl_expr (newvar);

	    tree cond_inner = SWITCH_STMT_COND (sw_stmt);
	    if (TREE_CODE (cond_inner) == CLEANUP_POINT_EXPR)
	      cond_inner = TREE_OPERAND (cond_inner, 0);
	    location_t sloc = EXPR_LOCATION (SWITCH_STMT_COND (sw_stmt));
	    tree new_s = build2_loc (sloc, INIT_EXPR, sw_type, newvar,
				     cond_inner);
	    finish_expr_stmt (new_s);
	    SWITCH_STMT_COND (sw_stmt) = newvar;
	    /* Now add the switch statement with the condition re-
		   written to use the local var.  */
	    add_stmt (sw_stmt);
	    *stmt = pop_stmt_list (insert_list);
	    /* Process the expanded list.  */
	    res = cp_walk_tree (stmt, await_statement_walker,
				d, NULL);
	    *do_subtree = 0; /* Done subtrees.  */
	    return res;
	  }
	  break;
	case CO_RETURN_EXPR:
	  {
	    /* Expand the co_return as per [stmt.return.coroutine]
	       - for co_return;
		{ p.return_void (); goto final_suspend; }
	       - for co_return [void expr];
		{ expr; p.return_void(); goto final_suspend;}
	       - for co_return [non void expr];
		{ p.return_value(expr); goto final_suspend; }  */
	    if ((res = cp_walk_tree (stmt, analyze_expression_awaits,
		 d, &visited)))
	      return res;
	    location_t loc = EXPR_LOCATION (expr);
	    tree call = TREE_OPERAND (expr, 1);
	    expr = TREE_OPERAND (expr, 0);
	    tree ret_list = push_stmt_list ();
	    /* [stmt.return.coroutine], 2.2
	       If expr is present and void, it is placed immediately before
	       the call for return_void;  */
	    tree *maybe_await_stmt = NULL;
	    if (expr && VOID_TYPE_P (TREE_TYPE (expr)))
	      {
		finish_expr_stmt (expr);
		/* If the return argument was a void expression, then any
		   awaits must be contained in that.  */
		maybe_await_stmt = tsi_stmt_ptr (tsi_last (ret_list));
	      }
	    /* Insert p.return_{void,value(expr)}.  */
	    finish_expr_stmt (call);
	    /* Absent a return of a void expression, any awaits must be in
	       the parameter to return_value().  */
	    if (!maybe_await_stmt)
	      maybe_await_stmt = tsi_stmt_ptr (tsi_last (ret_list));
	    expr = build1_loc (loc, GOTO_EXPR, void_type_node, awpts->fs_label);
	    finish_expr_stmt (expr);
	    *stmt = pop_stmt_list (ret_list);
	    /* Once this is complete, we will have processed subtrees.  */
	    *do_subtree = 0;
	    if (awpts->saw_awaits)
	      {
		gcc_checking_assert (maybe_await_stmt);
		res = cp_walk_tree (maybe_await_stmt, await_statement_walker,
				    d, NULL);
		if (res)
		  return res;
	      }
	    return NULL_TREE; /* Done.  */
	  }
	break;
      }
  else if (EXPR_P (expr))
    {
      if ((res = cp_walk_tree (stmt, analyze_expression_awaits, d, &visited)))
	return res;
      *do_subtree = 0; /* Done subtrees.  */
      if (!awpts->saw_awaits)
	return NULL_TREE; /* Nothing special to do here.  */

      if (awpts->needs_truth_if_exp)
	{
	  /* If a truth-and/or-if expression has an await expression in the
	     conditionally-taken branch, then it must be rewritten into a
	     regular conditional.  */
	  truth_if_transform xf = {stmt, NULL_TREE, &truth_aoif_to_expand};
	  if ((res = cp_walk_tree (stmt, expand_one_truth_if, &xf, NULL)))
	    return res;
	}
      /* Process this statement, which contains at least one await expression
	 to 'promote' temporary values to a coroutine frame slot.  */
      return maybe_promote_temps (stmt, d);
    }
  /* Continue recursion, if needed.  */
  return res;
}

/* For figuring out what param usage we have.  */

struct param_frame_data
{
  tree *field_list;
  hash_map<tree, param_info> *param_uses;
  hash_set<tree *> *visited;
  location_t loc;
  bool param_seen;
};

/* A tree-walk callback that records the use of parameters (to allow for
   optimizations where handling unused parameters may be omitted).  */

static tree
register_param_uses (tree *stmt, int *do_subtree ATTRIBUTE_UNUSED, void *d)
{
  param_frame_data *data = (param_frame_data *) d;

  /* For lambda closure content, we have to look specifically.  */
  if (TREE_CODE (*stmt) == VAR_DECL && DECL_HAS_VALUE_EXPR_P (*stmt))
    {
      tree t = DECL_VALUE_EXPR (*stmt);
      return cp_walk_tree (&t, register_param_uses, d, NULL);
    }

  if (TREE_CODE (*stmt) != PARM_DECL)
    return NULL_TREE;

  /* If we already saw the containing expression, then we're done.  */
  if (data->visited->add (stmt))
    return NULL_TREE;

  bool existed;
  param_info &parm = data->param_uses->get_or_insert (*stmt, &existed);
  gcc_checking_assert (existed);

  if (!parm.body_uses)
    {
      vec_alloc (parm.body_uses, 4);
      parm.body_uses->quick_push (stmt);
      data->param_seen = true;
    }
  else
    parm.body_uses->safe_push (stmt);

  return NULL_TREE;
}

/* Small helper for the repetitive task of adding a new field to the coro
   frame type.  */

static tree
coro_make_frame_entry (tree *field_list, const char *name, tree fld_type,
		       location_t loc)
{
  tree id = get_identifier (name);
  tree decl = build_decl (loc, FIELD_DECL, id, fld_type);
  DECL_CHAIN (decl) = *field_list;
  *field_list = decl;
  return id;
}

/* For recording local variable usage.  */

struct local_vars_frame_data
{
  tree *field_list;
  hash_map<tree, local_var_info> *local_var_uses;
  unsigned int nest_depth, bind_indx;
  location_t loc;
  bool saw_capture;
  bool local_var_seen;
};

/* A tree-walk callback that processes one bind expression noting local
   variables, and making a coroutine frame slot available for those that
   need it, so that they can be 'promoted' across suspension points.  */

static tree
register_local_var_uses (tree *stmt, int *do_subtree, void *d)
{
  local_vars_frame_data *lvd = (local_vars_frame_data *) d;

  /* As we enter a bind expression - record the vars there and then recurse.
     As we exit drop the nest depth.
     The bind index is a growing count of how many bind indices we've seen.
     We build a space in the frame for each local var.  */

  if (TREE_CODE (*stmt) == BIND_EXPR)
    {
      lvd->bind_indx++;
      lvd->nest_depth++;
      tree lvar;
      for (lvar = BIND_EXPR_VARS (*stmt); lvar != NULL;
	   lvar = DECL_CHAIN (lvar))
	{
	  bool existed;
	  local_var_info &local_var
	    = lvd->local_var_uses->get_or_insert (lvar, &existed);
	  gcc_checking_assert (!existed);
	  local_var.def_loc = DECL_SOURCE_LOCATION (lvar);
	  tree lvtype = TREE_TYPE (lvar);
	  local_var.frame_type = lvtype;
	  local_var.field_idx = local_var.field_id = NULL_TREE;

	  /* Make sure that we only present vars to the tests below.  */
	  if (TREE_CODE (lvar) == TYPE_DECL
	      || TREE_CODE (lvar) == NAMESPACE_DECL)
	    continue;

	  /* We don't move static vars into the frame. */
	  local_var.is_static = TREE_STATIC (lvar);
	  if (local_var.is_static)
	    continue;

	  lvd->local_var_seen = true;
	  /* If this var is a lambda capture proxy, we want to leave it alone,
	     and later rewrite the DECL_VALUE_EXPR to indirect through the
	     frame copy of the pointer to the lambda closure object.  */
	  local_var.is_lambda_capture = is_capture_proxy (lvar);
	  if (local_var.is_lambda_capture)
	    continue;

	  /* If a variable has a value expression, then that's what needs
	     to be processed.  */
	  local_var.has_value_expr_p = DECL_HAS_VALUE_EXPR_P (lvar);
	  if (local_var.has_value_expr_p)
	    continue;

	  /* Make names depth+index unique, so that we can support nested
	     scopes with identically named locals.  */
	  tree lvname = DECL_NAME (lvar);
	  char *buf;
	  if (lvname != NULL_TREE)
	    buf = xasprintf ("__%s.%u.%u", IDENTIFIER_POINTER (lvname),
			     lvd->nest_depth, lvd->bind_indx);
	  else
	    buf = xasprintf ("_D%u.%u.%u", DECL_UID (lvar), lvd->nest_depth,
			     lvd->bind_indx);
	  /* TODO: Figure out if we should build a local type that has any
	     excess alignment or size from the original decl.  */
	  local_var.field_id
	    = coro_make_frame_entry (lvd->field_list, buf, lvtype, lvd->loc);
	  free (buf);
	  /* We don't walk any of the local var sub-trees, they won't contain
	     any bind exprs.  */
	}
      cp_walk_tree (&BIND_EXPR_BODY (*stmt), register_local_var_uses, d, NULL);
      *do_subtree = 0; /* We've done this.  */
      lvd->nest_depth--;
    }
  return NULL_TREE;
}

/* Build, return FUNCTION_DECL node with its coroutine frame pointer argument
   for either actor or destroy functions.  */

static tree
act_des_fn (tree orig, tree fn_type, tree coro_frame_ptr, const char* name)
{
  tree fn_name = get_fn_local_identifier (orig, name);
  tree fn = build_lang_decl (FUNCTION_DECL, fn_name, fn_type);
  DECL_CONTEXT (fn) = DECL_CONTEXT (orig);
  DECL_ARTIFICIAL (fn) = true;
  DECL_INITIAL (fn) = error_mark_node;
  tree id = get_identifier ("frame_ptr");
  tree fp = build_lang_decl (PARM_DECL, id, coro_frame_ptr);
  DECL_CONTEXT (fp) = fn;
  DECL_ARG_TYPE (fp) = type_passed_as (coro_frame_ptr);
  DECL_ARGUMENTS (fn) = fp;
  /* Copy selected attributes from the original function.  */
  TREE_USED (fn) = TREE_USED (orig);
  if (DECL_SECTION_NAME (orig))
    set_decl_section_name (fn, DECL_SECTION_NAME (orig));
  /* Copy any alignment that the FE added.  */
  if (DECL_ALIGN (orig))
    SET_DECL_ALIGN (fn, DECL_ALIGN (orig));
  /* Copy any alignment the user added.  */
  DECL_USER_ALIGN (fn) = DECL_USER_ALIGN (orig);
  /* Apply attributes from the original fn.  */
  DECL_ATTRIBUTES (fn) = copy_list (DECL_ATTRIBUTES (orig));
  return fn;
}

/* Re-write the body as per [dcl.fct.def.coroutine] / 5.  */

static tree
coro_rewrite_function_body (location_t fn_start, tree fnbody, tree orig,
			    tree resume_fn_ptr_type, tree& resume_fn_field,
			    tree& resume_idx_field, tree& fs_label)
{
  /* This will be our new outer scope.  */
  tree update_body = build3 (BIND_EXPR, void_type_node, NULL, NULL, NULL);
  tree top_block = make_node (BLOCK);
  BIND_EXPR_BLOCK (update_body) = top_block;
  BIND_EXPR_BODY (update_body) = push_stmt_list ();

  /* If the function has a top level bind expression, then connect that
     after first making sure we give it a new block.  */
  tree first = expr_first (fnbody);
  if (first && TREE_CODE (first) == BIND_EXPR)
    {
      tree block = BIND_EXPR_BLOCK (first);
      gcc_checking_assert (block);
      gcc_checking_assert (BLOCK_SUPERCONTEXT (block) == NULL_TREE);
      gcc_checking_assert (BLOCK_CHAIN (block) == NULL_TREE);
      /* Replace the top block to avoid issues with locations for args
	 appearing to be in a non-existent place.  */
      tree replace_blk = make_node (BLOCK);
      BLOCK_VARS (replace_blk) = BLOCK_VARS (block);
      BLOCK_SUBBLOCKS (replace_blk) = BLOCK_SUBBLOCKS (block);
      for (tree b = BLOCK_SUBBLOCKS (replace_blk); b; b = BLOCK_CHAIN (b))
	BLOCK_SUPERCONTEXT (b) = replace_blk;
      BIND_EXPR_BLOCK (first) = replace_blk;
      /* The top block has one child, so far, and we have now got a 
	 superblock.  */
      BLOCK_SUPERCONTEXT (block) = top_block;
      BLOCK_SUBBLOCKS (top_block) = block;
    }

  /* Wrap the function body in a try {} catch (...) {} block, if exceptions
     are enabled.  */
  tree promise = get_coroutine_promise_proxy (orig);
  tree var_list = NULL_TREE;
  tree initial_await = build_init_or_final_await (fn_start, false);

  /* [stmt.return.coroutine] / 3
     If p.return_void() is a valid expression, flowing off the end of a
     coroutine is equivalent to a co_return with no operand; otherwise
     flowing off the end of a coroutine results in undefined behavior.  */
  tree return_void
    = get_coroutine_return_void_expr (current_function_decl, fn_start, false);

  /* We will need to be able to set the resume function pointer to nullptr
     to signal that the coroutine is 'done'.  */
  resume_fn_field
    = build_lang_decl (VAR_DECL, get_identifier ("resume.fn.ptr.proxy"),
		       resume_fn_ptr_type);
  DECL_ARTIFICIAL (resume_fn_field) = true;
  tree zero_resume
    = build1 (CONVERT_EXPR, resume_fn_ptr_type, integer_zero_node);
  zero_resume
    = build2 (INIT_EXPR, resume_fn_ptr_type, resume_fn_field, zero_resume);
  /* Likewise, the resume index needs to be reset.  */
  resume_idx_field
    = build_lang_decl (VAR_DECL, get_identifier ("resume.index.proxy"),
		       short_unsigned_type_node);
  DECL_ARTIFICIAL (resume_idx_field) = true;
  tree zero_resume_idx = build_int_cst (short_unsigned_type_node, 0);
  zero_resume_idx = build2 (INIT_EXPR, short_unsigned_type_node,
			    resume_idx_field, zero_resume_idx);

  if (flag_exceptions)
    {
      /* Build promise.unhandled_exception();  */
      tree ueh
	= coro_build_promise_expression (current_function_decl, promise,
					 coro_unhandled_exception_identifier,
					 fn_start, NULL, /*musthave=*/true);
      /* Create and initialize the initial-await-resume-called variable per
	 [dcl.fct.def.coroutine] / 5.3.  */
      tree i_a_r_c = build_lang_decl (VAR_DECL, get_identifier ("i_a_r_c"),
				      boolean_type_node);
      DECL_ARTIFICIAL (i_a_r_c) = true;
      DECL_CHAIN (i_a_r_c) = var_list;
      var_list = i_a_r_c;
      DECL_INITIAL (i_a_r_c) = boolean_false_node;
      add_decl_expr (i_a_r_c);
      /* Start the try-catch.  */
      tree tcb = build_stmt (fn_start, TRY_BLOCK, NULL_TREE, NULL_TREE);
      add_stmt (tcb);
      TRY_STMTS (tcb) = push_stmt_list ();
      if (initial_await != error_mark_node)
	{
	  /* Build a compound expression that sets the
	     initial-await-resume-called variable true and then calls the
	     initial suspend expression await resume.  */
	  tree vec = TREE_OPERAND (initial_await, 3);
	  tree aw_r = TREE_VEC_ELT (vec, 2);
	  tree update = build2 (MODIFY_EXPR, boolean_type_node, i_a_r_c,
				boolean_true_node);
	  aw_r = cp_build_compound_expr (update, aw_r, tf_warning_or_error);
	  TREE_VEC_ELT (vec, 2) = aw_r;
	}
      /* Add the initial await to the start of the user-authored function.  */
      finish_expr_stmt (initial_await);
      /* Append the original function body.  */
      add_stmt (fnbody);
      if (return_void)
	add_stmt (return_void);
      TRY_STMTS (tcb) = pop_stmt_list (TRY_STMTS (tcb));
      TRY_HANDLERS (tcb) = push_stmt_list ();
      /* Mimic what the parser does for the catch.  */
      tree handler = begin_handler ();
      finish_handler_parms (NULL_TREE, handler); /* catch (...) */

      /* Get the initial await resume called value.  */
      tree not_iarc_if = begin_if_stmt ();
      tree not_iarc = build1_loc (fn_start, TRUTH_NOT_EXPR,
				  boolean_type_node, i_a_r_c);
      finish_if_stmt_cond (not_iarc, not_iarc_if);
      /* If the initial await resume called value is false, rethrow...  */
      tree rethrow = build_throw (fn_start, NULL_TREE);
      TREE_NO_WARNING (rethrow) = true;
      finish_expr_stmt (rethrow);
      finish_then_clause (not_iarc_if);
      tree iarc_scope = IF_SCOPE (not_iarc_if);
      IF_SCOPE (not_iarc_if) = NULL;
      not_iarc_if = do_poplevel (iarc_scope);
      add_stmt (not_iarc_if);
      /* ... else call the promise unhandled exception method
	 but first we set done = true and the resume index to 0.
	 If the unhandled exception method returns, then we continue
	 to the final await expression (which duplicates the clearing of
	 the field). */
      finish_expr_stmt (zero_resume);
      finish_expr_stmt (zero_resume_idx);
      ueh = maybe_cleanup_point_expr_void (ueh);
      add_stmt (ueh);
      finish_handler (handler);
      TRY_HANDLERS (tcb) = pop_stmt_list (TRY_HANDLERS (tcb));
    }
  else
    {
      if (pedantic)
	{
	  /* We still try to look for the promise method and warn if it's not
	     present.  */
	  tree ueh_meth
	    = lookup_promise_method (orig, coro_unhandled_exception_identifier,
				     fn_start, /*musthave=*/false);
	  if (!ueh_meth || ueh_meth == error_mark_node)
	    warning_at (fn_start, 0, "no member named %qE in %qT",
			coro_unhandled_exception_identifier,
			get_coroutine_promise_type (orig));
	}
      /* Else we don't check and don't care if the method is missing..
	 just add the initial suspend, function and return.  */
      finish_expr_stmt (initial_await);
      /* Append the original function body.  */
      add_stmt (fnbody);
      if (return_void)
	add_stmt (return_void);
    }

  /* co_return branches to the final_suspend label, so declare that now.  */
  fs_label
    = create_named_label_with_ctx (fn_start, "final.suspend", NULL_TREE);
  add_stmt (build_stmt (fn_start, LABEL_EXPR, fs_label));

  /* Before entering the final suspend point, we signal that this point has
     been reached by setting the resume function pointer to zero (this is
     what the 'done()' builtin tests) as per the current ABI.  */
  finish_expr_stmt (zero_resume);
  finish_expr_stmt (build_init_or_final_await (fn_start, true));
  BIND_EXPR_BODY (update_body) = pop_stmt_list (BIND_EXPR_BODY (update_body));
  BIND_EXPR_VARS (update_body) = nreverse (var_list);
  BLOCK_VARS (top_block) = BIND_EXPR_VARS (update_body);

  return update_body;
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
  void (*__resume)(_R_frame *);
  void (*__destroy)(_R_frame *);
  coro1::promise_type __p;
  bool frame_needs_free; free the coro frame mem if set.
  bool i_a_r_c; [dcl.fct.def.coroutine] / 5.3
  short __resume_at;
  handle_type self_handle;
  (maybe) parameter copies.
  (maybe) local variables saved (including awaitables)
  (maybe) trailing space.
 };  */

bool
morph_fn_to_coro (tree orig, tree *resumer, tree *destroyer)
{
  gcc_checking_assert (orig && TREE_CODE (orig) == FUNCTION_DECL);

  *resumer = error_mark_node;
  *destroyer = error_mark_node;
  if (!coro_function_valid_p (orig))
    {
      /* For early errors, we do not want a diagnostic about the missing
	 ramp return value, since the user cannot fix this - a 'return' is
	 not allowed in a coroutine.  */
      TREE_NO_WARNING (orig) = true;
      /* Discard the body, we can't process it further.  */
      pop_stmt_list (DECL_SAVED_TREE (orig));
      DECL_SAVED_TREE (orig) = push_stmt_list ();
      return false;
    }

  /* We can't validly get here with an empty statement list, since there's no
     way for the FE to decide it's a coroutine in the absence of any code.  */
  tree fnbody = pop_stmt_list (DECL_SAVED_TREE (orig));
  gcc_checking_assert (fnbody != NULL_TREE);

  /* We don't have the locus of the opening brace - it's filled in later (and
     there doesn't really seem to be any easy way to get at it).
     The closing brace is assumed to be input_location.  */
  location_t fn_start = DECL_SOURCE_LOCATION (orig);
  gcc_rich_location fn_start_loc (fn_start);

  /* Initial processing of the function-body.
     If we have no expressions or just an error then punt.  */
  tree body_start = expr_first (fnbody);
  if (body_start == NULL_TREE || body_start == error_mark_node)
    {
      DECL_SAVED_TREE (orig) = push_stmt_list ();
      append_to_statement_list (fnbody, &DECL_SAVED_TREE (orig));
      /* Suppress warnings about the missing return value.  */
      TREE_NO_WARNING (orig) = true;
      return false;
    }

  /* So, we've tied off the original user-authored body in fn_body.

     Start the replacement synthesized ramp body as newbody.
     If we encounter a fatal error we might return a now-empty body.

     Note, the returned ramp body is not 'popped', to be compatible with
     the way that decl.c handles regular functions, the scope pop is done
     in the caller.  */

  tree newbody = push_stmt_list ();
  DECL_SAVED_TREE (orig) = newbody;

  /* If our original body is noexcept, then that's what we apply to our
     generated ramp, transfer any MUST_NOT_THOW_EXPR to that.  */
  bool is_noexcept = TREE_CODE (body_start) == MUST_NOT_THROW_EXPR;
  if (is_noexcept)
    {
      /* The function body we will continue with is the single operand to
	 the must-not-throw.  */
      fnbody = TREE_OPERAND (body_start, 0);
      /* Transfer the must-not-throw to the ramp body.  */
      add_stmt (body_start);
      /* Re-start the ramp as must-not-throw.  */
      TREE_OPERAND (body_start, 0) = push_stmt_list ();
    }

  /* If the original function has a return value with a non-trivial DTOR
     and the body contains a var with a DTOR that might throw, the decl is
     marked "throwing_cleanup".
     We do not [in the ramp, which is synthesised here], use any body var
     types with DTORs that might throw.
     The original body is transformed into the actor function which only
     contains void returns, and is also wrapped in a try-catch block.
     So (a) the 'throwing_cleanup' is not correct for the ramp and (b) we do
     not need to transfer it to the actor which only contains void returns.  */
  cp_function_chain->throwing_cleanup = false;

  /* Create the coro frame type, as far as it can be known at this stage.
     1. Types we already know.  */

  tree fn_return_type = TREE_TYPE (TREE_TYPE (orig));
  tree handle_type = get_coroutine_handle_type (orig);
  tree promise_type = get_coroutine_promise_type (orig);

  /* 2. Types we need to define or look up.  */

  tree fr_name = get_fn_local_identifier (orig, "frame");
  tree coro_frame_type = xref_tag (record_type, fr_name, ts_current, false);
  DECL_CONTEXT (TYPE_NAME (coro_frame_type)) = current_scope ();
  tree coro_frame_ptr = build_pointer_type (coro_frame_type);
  tree act_des_fn_type
    = build_function_type_list (void_type_node, coro_frame_ptr, NULL_TREE);
  tree act_des_fn_ptr = build_pointer_type (act_des_fn_type);

  /* Declare the actor and destroyer function.  */
  tree actor = act_des_fn (orig, act_des_fn_type, coro_frame_ptr, "actor");
  tree destroy = act_des_fn (orig, act_des_fn_type, coro_frame_ptr, "destroy");

  /* Construct the wrapped function body; we will analyze this to determine
     the requirements for the coroutine frame.  */

  tree resume_fn_field = NULL_TREE;
  tree resume_idx_field = NULL_TREE;
  tree fs_label = NULL_TREE;
  fnbody = coro_rewrite_function_body (fn_start, fnbody, orig,
				       act_des_fn_ptr, resume_fn_field,
				       resume_idx_field, fs_label);
  /* Build our dummy coro frame layout.  */
  coro_frame_type = begin_class_definition (coro_frame_type);

  tree field_list = NULL_TREE;
  tree resume_name
    = coro_make_frame_entry (&field_list, "__resume",
			     act_des_fn_ptr, fn_start);
  tree destroy_name
    = coro_make_frame_entry (&field_list, "__destroy",
			     act_des_fn_ptr, fn_start);
  tree promise_name
    = coro_make_frame_entry (&field_list, "__p", promise_type, fn_start);
  tree fnf_name = coro_make_frame_entry (&field_list, "__frame_needs_free",
					 boolean_type_node, fn_start);
  tree resume_idx_name
    = coro_make_frame_entry (&field_list, "__resume_at",
			     short_unsigned_type_node, fn_start);

  /* We need a handle to this coroutine, which is passed to every
     await_suspend().  There's no point in creating it over and over.  */
  (void) coro_make_frame_entry (&field_list, "__self_h", handle_type, fn_start);

  /* Now add in fields for function params (if there are any).
     We do not attempt elision of copies at this stage, we do analyse the
     uses and build worklists to replace those when the state machine is
     lowered.  */

  hash_map<tree, param_info> *param_uses = NULL;
  if (DECL_ARGUMENTS (orig))
    {
      /* Build a hash map with an entry for each param.
	  The key is the param tree.
	  Then we have an entry for the frame field name.
	  Then a cache for the field ref when we come to use it.
	  Then a tree list of the uses.
	  The second two entries start out empty - and only get populated
	  when we see uses.  */
      param_uses = new hash_map<tree, param_info>;
      bool lambda_p = LAMBDA_FUNCTION_P (orig);

      unsigned no_name_parm = 0;
      for (tree arg = DECL_ARGUMENTS (orig); arg != NULL;
	   arg = DECL_CHAIN (arg))
	{
	  bool existed;
	  param_info &parm = param_uses->get_or_insert (arg, &existed);
	  gcc_checking_assert (!existed);
	  parm.body_uses = NULL;
	  tree actual_type = TREE_TYPE (arg);
	  actual_type = complete_type_or_else (actual_type, orig);
	  if (actual_type == NULL_TREE)
	    actual_type = error_mark_node;
	  parm.orig_type = actual_type;
	  parm.by_ref = parm.pt_ref = parm.rv_ref =  false;
	  if (TREE_CODE (actual_type) == REFERENCE_TYPE)
	    {
	      /* If the user passes by reference, then we will save the
		 pointer to the original.  As noted in
		 [dcl.fct.def.coroutine] / 13, if the lifetime of the
		 referenced item ends and then the coroutine is resumed,
		 we have UB; well, the user asked for it.  */
	      if (TYPE_REF_IS_RVALUE (actual_type))
		parm.rv_ref = true;
	      else
		parm.pt_ref = true;
	    }
	  else if (TYPE_REF_P (DECL_ARG_TYPE (arg)))
	    parm.by_ref = true;

	  parm.frame_type = actual_type;

	  parm.this_ptr = is_this_parameter (arg);
	  /* See PR94807.  When a lambda is in a template instantiation, the
	     closure object is named 'this' instead of '__closure'.  */
	  if (lambda_p)
	    {
	      parm.lambda_cobj = parm.this_ptr
				 || (DECL_NAME (arg) == closure_identifier);
	      parm.this_ptr = false;
	    }
	  else
	    parm.lambda_cobj = false;

	  char *buf;
	  if (DECL_NAME (arg))
	    {
	      tree pname = DECL_NAME (arg);
	      buf = xasprintf ("__parm.%s", IDENTIFIER_POINTER (pname));
	    }
	  else
	    buf = xasprintf ("__unnamed_parm.%d", no_name_parm++);

	  if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (parm.frame_type))
	    {
	      char *gbuf = xasprintf ("%s.live", buf);
	      parm.guard_var
		= build_lang_decl (VAR_DECL, get_identifier (gbuf),
				   boolean_type_node);
	      free (gbuf);
	      DECL_ARTIFICIAL (parm.guard_var) = true;
	      DECL_INITIAL (parm.guard_var) = boolean_false_node;
	      parm.trivial_dtor = false;
	    }
	  else
	    parm.trivial_dtor = true;
	  parm.field_id = coro_make_frame_entry
	    (&field_list, buf, actual_type, DECL_SOURCE_LOCATION (arg));
	  free (buf);
	}

      /* We want to record every instance of param's use, so don't include
	 a 'visited' hash_set on the tree walk, but only record a containing
	 expression once.  */
      hash_set<tree *> visited;
      param_frame_data param_data
	= {&field_list, param_uses, &visited, fn_start, false};
      cp_walk_tree (&fnbody, register_param_uses, &param_data, NULL);
    }

  /* We need to know, and inspect, each suspend point in the function
     in several places.  It's convenient to place this map out of line
     since it's used from tree walk callbacks.  */
  suspend_points = new hash_map<tree, suspend_point_info>;

  /* Now insert the data for any body await points, at this time we also need
     to promote any temporaries that are captured by reference (to regular
     vars) they will get added to the coro frame along with other locals.  */
  susp_frame_data body_aw_points
    = {&field_list, handle_type, fs_label, NULL, NULL, 0, 0,
       hash_set<tree> (), NULL, NULL, 0, false, false, false};
  body_aw_points.block_stack = make_tree_vector ();
  body_aw_points.bind_stack = make_tree_vector ();
  body_aw_points.to_replace = make_tree_vector ();
  cp_walk_tree (&fnbody, await_statement_walker, &body_aw_points, NULL);

  /* 4. Now make space for local vars, this is conservative again, and we
     would expect to delete unused entries later.  */
  hash_map<tree, local_var_info> local_var_uses;
  local_vars_frame_data local_vars_data
    = {&field_list, &local_var_uses, 0, 0, fn_start, false, false};
  cp_walk_tree (&fnbody, register_local_var_uses, &local_vars_data, NULL);

  /* Tie off the struct for now, so that we can build offsets to the
     known entries.  */
  TYPE_FIELDS (coro_frame_type) = field_list;
  TYPE_BINFO (coro_frame_type) = make_tree_binfo (0);
  BINFO_OFFSET (TYPE_BINFO (coro_frame_type)) = size_zero_node;
  BINFO_TYPE (TYPE_BINFO (coro_frame_type)) = coro_frame_type;

  coro_frame_type = finish_struct (coro_frame_type, NULL_TREE);

  /* Ramp: */
  /* Now build the ramp function pieces.  */
  tree ramp_bind = build3 (BIND_EXPR, void_type_node, NULL, NULL, NULL);
  add_stmt (ramp_bind);
  tree ramp_body = push_stmt_list ();

  tree coro_fp = build_lang_decl (VAR_DECL, get_identifier ("coro.frameptr"),
				  coro_frame_ptr);
  tree varlist = coro_fp;

  /* To signal that we need to cleanup copied function args.  */
  if (flag_exceptions && DECL_ARGUMENTS (orig))
    for (tree arg = DECL_ARGUMENTS (orig); arg != NULL;
	arg = DECL_CHAIN (arg))
      {
	param_info *parm_i = param_uses->get (arg);
	gcc_checking_assert (parm_i);
	if (parm_i->trivial_dtor)
	  continue;
	DECL_CHAIN (parm_i->guard_var) = varlist;
	varlist = parm_i->guard_var;
      }

  /* Signal that we need to clean up the promise object on exception.  */
  tree coro_promise_live
   = build_lang_decl (VAR_DECL, get_identifier ("coro.promise.live"),
		      boolean_type_node);
  DECL_ARTIFICIAL (coro_promise_live) = true;
  DECL_CHAIN (coro_promise_live) = varlist;
  varlist = coro_promise_live;
  DECL_INITIAL (coro_promise_live) = boolean_false_node;
  /* When the get-return-object is in the RETURN slot, we need to arrange for
     cleanup on exception.  */
  tree coro_gro_live
   = build_lang_decl (VAR_DECL, get_identifier ("coro.gro.live"),
		      boolean_type_node);
  DECL_ARTIFICIAL (coro_gro_live) = true;
  DECL_CHAIN (coro_gro_live) = varlist;
  varlist = coro_gro_live;
  DECL_INITIAL (coro_gro_live) = boolean_false_node;

  /* Collected the scope vars we need ... only one for now. */
  BIND_EXPR_VARS (ramp_bind) = nreverse (varlist);

  /* We're now going to create a new top level scope block for the ramp
     function.  */
  tree top_block = make_node (BLOCK);

  BIND_EXPR_BLOCK (ramp_bind) = top_block;
  BLOCK_VARS (top_block) = BIND_EXPR_VARS (ramp_bind);
  BLOCK_SUBBLOCKS (top_block) = NULL_TREE;

  /* The decl_expr for the coro frame pointer, initialize to zero so that we
     can pass it to the IFN_CO_FRAME (since there's no way to pass a type,
     directly apparently).  This avoids a "used uninitialized" warning.  */
  tree zeroinit = build1 (CONVERT_EXPR, coro_frame_ptr, integer_zero_node);
  DECL_INITIAL (coro_fp) = zeroinit;
  add_decl_expr (coro_fp);
  if (flag_exceptions && DECL_ARGUMENTS (orig))
    for (tree arg = DECL_ARGUMENTS (orig); arg != NULL;
	arg = DECL_CHAIN (arg))
      {
	param_info *parm_i = param_uses->get (arg);
	if (parm_i->trivial_dtor)
	  continue;
	add_decl_expr (parm_i->guard_var);;
      }
  add_decl_expr (coro_promise_live);
  add_decl_expr (coro_gro_live);

  /* The CO_FRAME internal function is a mechanism to allow the middle end
     to adjust the allocation in response to optimisations.  We provide the
     current conservative estimate of the frame size (as per the current)
     computed layout.  */
  tree frame_size = TYPE_SIZE_UNIT (coro_frame_type);
  tree resizeable
    = build_call_expr_internal_loc (fn_start, IFN_CO_FRAME, size_type_node, 2,
				    frame_size, coro_fp);

  /* n4849 [dcl.fct.def.coroutine] / 10 (part1)
    The unqualified-id get_return_object_on_allocation_failure is looked up
    in the scope of the promise type by class member access lookup.  */

  /* We don't require this, so coro_build_promise_expression can return NULL,
     but, if the lookup succeeds, then the function must be usable.  */
  tree dummy_promise = build_dummy_object (get_coroutine_promise_type (orig));
  tree grooaf
    = coro_build_promise_expression (orig, dummy_promise,
				     coro_gro_on_allocation_fail_identifier,
				     fn_start, NULL, /*musthave=*/false);

  /* however, should that fail, returning an error, the later stages can't
     handle the erroneous expression, so we reset the call as if it was
     absent.  */
  if (grooaf == error_mark_node)
    grooaf = NULL_TREE;

  /* Allocate the frame, this has several possibilities:
     n4849 [dcl.fct.def.coroutine] / 9 (part 1)
     The allocation function’s name is looked up in the scope of the promise
     type.  It's not a failure for it to be absent see part 4, below.  */

  tree nwname = ovl_op_identifier (false, NEW_EXPR);
  tree new_fn = NULL_TREE;

  if (TYPE_HAS_NEW_OPERATOR (promise_type))
    {
      tree fns = lookup_promise_method (orig, nwname, fn_start,
					/*musthave=*/true);
      /* [dcl.fct.def.coroutine] / 9 (part 2)
	If the lookup finds an allocation function in the scope of the promise
	type, overload resolution is performed on a function call created by
	assembling an argument list.  The first argument is the amount of space
	requested, and has type std::size_t.  The succeeding arguments are
	those of the original function.  */
      vec<tree, va_gc> *args = make_tree_vector ();
      vec_safe_push (args, resizeable); /* Space needed.  */

      for (tree arg = DECL_ARGUMENTS (orig); arg != NULL;
	   arg = DECL_CHAIN (arg))
	{
	  param_info *parm_i = param_uses->get (arg);
	  gcc_checking_assert (parm_i);
	  if (parm_i->this_ptr || parm_i->lambda_cobj)
	    {
	      /* We pass a reference to *this to the allocator lookup.  */
	      tree tt = TREE_TYPE (TREE_TYPE (arg));
	      tree this_ref = build1 (INDIRECT_REF, tt, arg);
	      tt = cp_build_reference_type (tt, false);
	      this_ref = convert_to_reference (tt, this_ref, CONV_STATIC,
					       LOOKUP_NORMAL , NULL_TREE,
					       tf_warning_or_error);
	      vec_safe_push (args, this_ref);
	    }
	  else
	    vec_safe_push (args, arg);
	}

      /* Note the function selected; we test to see if it's NOTHROW.  */
      tree func;
      /* Failure is not an error for this attempt.  */
      new_fn = build_new_method_call (dummy_promise, fns, &args, NULL,
				      LOOKUP_NORMAL, &func, tf_none);
      release_tree_vector (args);

      if (new_fn == error_mark_node)
	{
	  /* n4849 [dcl.fct.def.coroutine] / 9 (part 3)
	    If no viable function is found, overload resolution is performed
	    again on a function call created by passing just the amount of
	    space required as an argument of type std::size_t.  */
	  args = make_tree_vector_single (resizeable); /* Space needed.  */
	  new_fn = build_new_method_call (dummy_promise, fns, &args,
					  NULL_TREE, LOOKUP_NORMAL, &func,
					  tf_none);
	  release_tree_vector (args);
	}

     /* However, if the promise provides an operator new, then one of these
	two options must be available.  */
    if (new_fn == error_mark_node)
      {
	error_at (fn_start, "%qE is provided by %qT but is not usable with"
		  " the function signature %qD", nwname, promise_type, orig);
	new_fn = error_mark_node;
      }
    else if (grooaf && !TYPE_NOTHROW_P (TREE_TYPE (func)))
      error_at (fn_start, "%qE is provided by %qT but %qE is not marked"
		" %<throw()%> or %<noexcept%>", grooaf, promise_type, nwname);
    else if (!grooaf && TYPE_NOTHROW_P (TREE_TYPE (func)))
      warning_at (fn_start, 0, "%qE is marked %<throw()%> or %<noexcept%> but"
		  " no usable %<get_return_object_on_allocation_failure%>"
		  " is provided by %qT", nwname, promise_type);
    }
  else /* No operator new in the promise.  */
    {
      /* n4849 [dcl.fct.def.coroutine] / 9 (part 4)
	 If this lookup fails, the allocation function’s name is looked up in
	 the global scope.  */

      vec<tree, va_gc> *args;
      /* build_operator_new_call () will insert size needed as element 0 of
	 this, and we might need to append the std::nothrow constant.  */
      vec_alloc (args, 2);
      if (grooaf)
	{
	  /* n4849 [dcl.fct.def.coroutine] / 10 (part 2)
	   If any declarations (of the get return on allocation fail) are
	   found, then the result of a call to an allocation function used
	   to obtain storage for the coroutine state is assumed to return
	   nullptr if it fails to obtain storage and, if a global allocation
	   function is selected, the ::operator new(size_t, nothrow_t) form
	   is used.  The allocation function used in this case shall have a
	   non-throwing noexcept-specification.  So we need std::nothrow.  */
	  tree std_nt = lookup_qualified_name (std_node,
					       get_identifier ("nothrow"),
					       0, /*complain=*/true, false);
	  if (!std_nt || std_nt == error_mark_node)
	    error_at (fn_start, "%qE is provided by %qT but %<std::nothrow%> "
		      "cannot be found", grooaf, promise_type);
	  vec_safe_push (args, std_nt);
	}

      /* If we get to this point, we must succeed in looking up the global
	 operator new for the params provided.  Extract a simplified version
	 of the machinery from build_operator_new_call.  This can update the
	 frame size.  */
      tree cookie = NULL;
      new_fn = build_operator_new_call (nwname, &args, &frame_size, &cookie,
					/*align_arg=*/NULL,
					/*size_check=*/NULL, /*fn=*/NULL,
					tf_warning_or_error);
      resizeable = build_call_expr_internal_loc
	(fn_start, IFN_CO_FRAME, size_type_node, 2, frame_size, coro_fp);
      /* If the operator call fails for some reason, then don't try to
	 amend it.  */
      if (new_fn != error_mark_node)
	CALL_EXPR_ARG (new_fn, 0) = resizeable;

      release_tree_vector (args);
    }

  tree allocated = build1 (CONVERT_EXPR, coro_frame_ptr, new_fn);
  tree r = build2 (INIT_EXPR, TREE_TYPE (coro_fp), coro_fp, allocated);
  r = coro_build_cvt_void_expr_stmt (r, fn_start);
  add_stmt (r);

  /* If the user provided a method to return an object on alloc fail, then
     check the returned pointer and call the func if it's null.
     Otherwise, no check, and we fail for noexcept/fno-exceptions cases.  */

  if (grooaf)
    {
      /* n4849 [dcl.fct.def.coroutine] / 10 (part 3)
	 If the allocation function returns nullptr,the coroutine returns
	 control to the caller of the coroutine and the return value is
	 obtained by a call to T::get_return_object_on_allocation_failure(),
	 where T is the promise type.  */

      gcc_checking_assert (same_type_p (fn_return_type, TREE_TYPE (grooaf)));
      tree if_stmt = begin_if_stmt ();
      tree cond = build1 (CONVERT_EXPR, coro_frame_ptr, integer_zero_node);
      cond = build2 (EQ_EXPR, boolean_type_node, coro_fp, cond);
      finish_if_stmt_cond (cond, if_stmt);
      if (VOID_TYPE_P (fn_return_type))
	{
	  /* Execute the get-return-object-on-alloc-fail call...  */
	  finish_expr_stmt (grooaf);
	  /* ... but discard the result, since we return void.  */
	  finish_return_stmt (NULL_TREE);
	}
      else
	{
	  /* Get the fallback return object.  */
	  r = build_cplus_new (fn_return_type, grooaf, tf_warning_or_error);
	  finish_return_stmt (r);
	}
      finish_then_clause (if_stmt);
      finish_if_stmt (if_stmt);
    }

  /* Up to now any exception thrown will propagate directly to the caller.
     This is OK since the only source of such exceptions would be in allocation
     of the coroutine frame, and therefore the ramp will not have initialized
     any further state.  From here, we will track state that needs explicit
     destruction in the case that promise or g.r.o setup fails or an exception
     is thrown from the initial suspend expression.  */
  tree ramp_cleanup = NULL_TREE;
  if (flag_exceptions)
    {
      ramp_cleanup = build_stmt (fn_start, TRY_BLOCK, NULL, NULL);
      add_stmt (ramp_cleanup);
      TRY_STMTS (ramp_cleanup) = push_stmt_list ();
    }

  /* deref the frame pointer, to use in member access code.  */
  tree deref_fp = build_x_arrow (fn_start, coro_fp, tf_warning_or_error);

  /* For now, once allocation has succeeded we always assume that this needs
     destruction, there's no impl. for frame allocation elision.  */
  tree fnf_m
    = lookup_member (coro_frame_type, fnf_name, 1, 0, tf_warning_or_error);
  tree fnf_x = build_class_member_access_expr (deref_fp, fnf_m, NULL_TREE,
					       false, tf_warning_or_error);
  r = build2 (INIT_EXPR, boolean_type_node, fnf_x, boolean_true_node);
  r = coro_build_cvt_void_expr_stmt (r, fn_start);
  add_stmt (r);

  /* Put the resumer and destroyer functions in.  */

  tree actor_addr = build1 (ADDR_EXPR, act_des_fn_ptr, actor);
  tree resume_m
    = lookup_member (coro_frame_type, resume_name,
		     /*protect=*/1, /*want_type=*/0, tf_warning_or_error);
  tree resume_x = build_class_member_access_expr (deref_fp, resume_m, NULL_TREE,
						  false, tf_warning_or_error);
  r = build2_loc (fn_start, INIT_EXPR, act_des_fn_ptr, resume_x, actor_addr);
  r = coro_build_cvt_void_expr_stmt (r, fn_start);
  add_stmt (r);

  tree destroy_addr = build1 (ADDR_EXPR, act_des_fn_ptr, destroy);
  tree destroy_m
    = lookup_member (coro_frame_type, destroy_name,
		     /*protect=*/1, /*want_type=*/0, tf_warning_or_error);
  tree destroy_x
    = build_class_member_access_expr (deref_fp, destroy_m, NULL_TREE, false,
				      tf_warning_or_error);
  r = build2_loc (fn_start, INIT_EXPR, act_des_fn_ptr, destroy_x, destroy_addr);
  r = coro_build_cvt_void_expr_stmt (r, fn_start);
  add_stmt (r);

  /* n4849 [dcl.fct.def.coroutine] /13
     When a coroutine is invoked, a copy is created for each coroutine
     parameter.  Each such copy is an object with automatic storage duration
     that is direct-initialized from an lvalue referring to the corresponding
     parameter if the parameter is an lvalue reference, and from an xvalue
     referring to it otherwise.  A reference to a parameter in the function-
     body of the coroutine and in the call to the coroutine promise
     constructor is replaced by a reference to its copy.  */

  vec<tree, va_gc> *promise_args = NULL; /* So that we can adjust refs.  */

  /* The initialization and destruction of each parameter copy occurs in the
     context of the called coroutine.  Initializations of parameter copies are
     sequenced before the call to the coroutine promise constructor and
     indeterminately sequenced with respect to each other.  The lifetime of
     parameter copies ends immediately after the lifetime of the coroutine
     promise object ends.  */

  vec<tree, va_gc> *param_dtor_list = NULL;

  if (DECL_ARGUMENTS (orig))
    {
      promise_args = make_tree_vector ();
      for (tree arg = DECL_ARGUMENTS (orig); arg != NULL;
	   arg = DECL_CHAIN (arg))
	{
	  bool existed;
	  param_info &parm = param_uses->get_or_insert (arg, &existed);

	  tree fld_ref = lookup_member (coro_frame_type, parm.field_id,
					/*protect=*/1, /*want_type=*/0,
					tf_warning_or_error);
	  tree fld_idx
	    = build_class_member_access_expr (deref_fp, fld_ref, NULL_TREE,
					      false, tf_warning_or_error);

	  /* Add this to the promise CTOR arguments list, accounting for
	     refs and special handling for method this ptr.  */
	  if (parm.this_ptr || parm.lambda_cobj)
	    {
	      /* We pass a reference to *this to the param preview.  */
	      tree tt = TREE_TYPE (arg);
	      gcc_checking_assert (POINTER_TYPE_P (tt));
	      tree ct = TREE_TYPE (tt);
	      tree this_ref = build1 (INDIRECT_REF, ct, arg);
	      tree rt = cp_build_reference_type (ct, false);
	      this_ref = convert_to_reference (rt, this_ref, CONV_STATIC,
					       LOOKUP_NORMAL, NULL_TREE,
					       tf_warning_or_error);
	      vec_safe_push (promise_args, this_ref);
	    }
	  else if (parm.rv_ref)
	    vec_safe_push (promise_args, rvalue(fld_idx));
	  else
	    vec_safe_push (promise_args, fld_idx);

	  if (parm.rv_ref || parm.pt_ref)
	    /* Initialise the frame reference field directly.  */
	    r = build_modify_expr (fn_start, TREE_OPERAND (fld_idx, 0),
				   parm.frame_type, INIT_EXPR,
				   DECL_SOURCE_LOCATION (arg), arg,
				   DECL_ARG_TYPE (arg));
	  else if (type_build_ctor_call (parm.frame_type))
	    {
	      vec<tree, va_gc> *p_in;
	      if (CLASS_TYPE_P (parm.frame_type)
		  && classtype_has_non_deleted_move_ctor (parm.frame_type))
		p_in = make_tree_vector_single (move (arg));
	      else if (lvalue_p (arg))
		p_in = make_tree_vector_single (rvalue (arg));
	      else
		p_in = make_tree_vector_single (arg);
	      /* Construct in place or move as relevant.  */
	      r = build_special_member_call (fld_idx, complete_ctor_identifier,
					     &p_in, parm.frame_type,
					     LOOKUP_NORMAL,
					     tf_warning_or_error);
	      release_tree_vector (p_in);
	    }
	  else
	    {
	      if (!same_type_p (parm.frame_type, DECL_ARG_TYPE (arg)))
		r = build1_loc (DECL_SOURCE_LOCATION (arg), CONVERT_EXPR,
				parm.frame_type, arg);
	      else
		r = arg;
	      r = build_modify_expr (fn_start, fld_idx, parm.frame_type,
				     INIT_EXPR, DECL_SOURCE_LOCATION (arg), r,
				     TREE_TYPE (r));
	    }
	  finish_expr_stmt (r);
	  if (!parm.trivial_dtor)
	    {
	      if (param_dtor_list == NULL)
		param_dtor_list = make_tree_vector ();
	      vec_safe_push (param_dtor_list, parm.field_id);
	      /* Cleanup this frame copy on exception.  */
	      parm.fr_copy_dtor
		= build_special_member_call (fld_idx, complete_dtor_identifier,
					     NULL, parm.frame_type,
					     LOOKUP_NORMAL,
					     tf_warning_or_error);
	      /* This var is now live.  */
	      r = build_modify_expr (fn_start, parm.guard_var,
				     boolean_type_node, INIT_EXPR, fn_start,
				     boolean_true_node, boolean_type_node);
	      finish_expr_stmt (r);
	    }
	}
    }

  /* Set up the promise.  */
  tree promise_m
    = lookup_member (coro_frame_type, promise_name,
		     /*protect=*/1, /*want_type=*/0, tf_warning_or_error);

  tree p = build_class_member_access_expr (deref_fp, promise_m, NULL_TREE,
					   false, tf_warning_or_error);

  tree promise_dtor = NULL_TREE;
  if (type_build_ctor_call (promise_type))
    {
      /* Do a placement new constructor for the promise type (we never call
	 the new operator, just the constructor on the object in place in the
	 frame).

	 First try to find a constructor with the same parameter list as the
	 original function (if it has params), failing that find a constructor
	 with no parameter list.  */

      if (DECL_ARGUMENTS (orig))
	{
	  r = build_special_member_call (p, complete_ctor_identifier,
					 &promise_args, promise_type,
					 LOOKUP_NORMAL, tf_none);
	  release_tree_vector (promise_args);
	}
      else
	r = NULL_TREE;

      if (r == NULL_TREE || r == error_mark_node)
	r = build_special_member_call (p, complete_ctor_identifier, NULL,
				       promise_type, LOOKUP_NORMAL,
				       tf_warning_or_error);

      r = coro_build_cvt_void_expr_stmt (r, fn_start);
      finish_expr_stmt (r);

      r = build_modify_expr (fn_start, coro_promise_live, boolean_type_node,
			     INIT_EXPR, fn_start, boolean_true_node,
			     boolean_type_node);
      finish_expr_stmt (r);

      promise_dtor
	= build_special_member_call (p, complete_dtor_identifier,
				     NULL, promise_type, LOOKUP_NORMAL,
				     tf_warning_or_error);
    }

  /* Set up a new bind context for the GRO.  */
  tree gro_context_bind = build3 (BIND_EXPR, void_type_node, NULL, NULL, NULL);
  /* Make and connect the scope blocks.  */
  tree gro_block = make_node (BLOCK);
  BLOCK_SUPERCONTEXT (gro_block) = top_block;
  BLOCK_SUBBLOCKS (top_block) = gro_block;
  BIND_EXPR_BLOCK (gro_context_bind) = gro_block;
  add_stmt (gro_context_bind);

  tree get_ro
    = coro_build_promise_expression (orig, p,
				     coro_get_return_object_identifier,
				     fn_start, NULL, /*musthave=*/true);
  /* Without a return object we haven't got much clue what's going on.  */
  if (get_ro == error_mark_node)
    {
      BIND_EXPR_BODY (ramp_bind) = pop_stmt_list (ramp_body);
      DECL_SAVED_TREE (orig) = newbody;
      /* Suppress warnings about the missing return value.  */
      TREE_NO_WARNING (orig) = true;
      return false;
    }

  tree gro_context_body = push_stmt_list ();
  tree gro_type = TREE_TYPE (get_ro);
  bool gro_is_void_p = VOID_TYPE_P (gro_type);

  tree gro = NULL_TREE;
  tree gro_bind_vars = NULL_TREE;
  /* Used for return objects in the RESULT slot.  */
  tree gro_ret_dtor = NULL_TREE;
  tree gro_cleanup_stmt = NULL_TREE;
  /* We have to sequence the call to get_return_object before initial
     suspend.  */
  if (gro_is_void_p)
    r = get_ro;
  else if (same_type_p (gro_type, fn_return_type))
    {
     /* [dcl.fct.def.coroutine] / 7
	The expression promise.get_return_object() is used to initialize the
	glvalue result or... (see below)
	Construct the return result directly.  */
      if (type_build_ctor_call (gro_type))
	{
	  vec<tree, va_gc> *arg = make_tree_vector_single (get_ro);
	  r = build_special_member_call (DECL_RESULT (orig),
					 complete_ctor_identifier,
					 &arg, gro_type, LOOKUP_NORMAL,
					 tf_warning_or_error);
	  release_tree_vector (arg);
	}
      else
	r = build2_loc (fn_start, INIT_EXPR, gro_type,
			DECL_RESULT (orig), get_ro);

      if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (gro_type))
	/* If some part of the initalization code (prior to the await_resume
	     of the initial suspend expression), then we need to clean up the
	     return value.  */
	gro_ret_dtor
	  = build_special_member_call (DECL_RESULT (orig),
				       complete_dtor_identifier, NULL,
				       gro_type, LOOKUP_NORMAL,
				       tf_warning_or_error);
    }
  else
    {
      /* ... or ... Construct an object that will be used as the single
	param to the CTOR for the return object.  */
      gro = build_lang_decl (VAR_DECL, get_identifier ("coro.gro"), gro_type);
      DECL_CONTEXT (gro) = current_scope ();
      add_decl_expr (gro);
      gro_bind_vars = gro;
      if (type_build_ctor_call (gro_type))
	{
	  vec<tree, va_gc> *arg = make_tree_vector_single (get_ro);
	  r = build_special_member_call (gro, complete_ctor_identifier,
					 &arg, gro_type, LOOKUP_NORMAL,
					 tf_warning_or_error);
	  release_tree_vector (arg);
	}
      else
	r = build2_loc (fn_start, INIT_EXPR, gro_type, gro, get_ro);
      /* The constructed object might require a cleanup.  */
      if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (gro_type))
	{
	  gro_cleanup_stmt
	    = build_special_member_call (gro, complete_dtor_identifier,
					 NULL, gro_type, LOOKUP_NORMAL,
					 tf_warning_or_error);
	  gro_cleanup_stmt = build_stmt (input_location, CLEANUP_STMT, NULL,
					 gro_cleanup_stmt, gro);
	}
    }
  finish_expr_stmt (r);

  if (gro_cleanup_stmt && gro_cleanup_stmt != error_mark_node)
    CLEANUP_BODY (gro_cleanup_stmt) = push_stmt_list ();

  /* If we have a live g.r.o in the return slot, then signal this for exception
     cleanup.  */
  if (gro_ret_dtor)
    {
       r = build_modify_expr (fn_start, coro_gro_live, boolean_type_node,
			      INIT_EXPR, fn_start, boolean_true_node,
			      boolean_type_node);
      finish_expr_stmt (r);
    }
  /* Initialize the resume_idx_name to 0, meaning "not started".  */
  tree resume_idx_m
    = lookup_member (coro_frame_type, resume_idx_name,
		     /*protect=*/1, /*want_type=*/0, tf_warning_or_error);
  tree resume_idx
    = build_class_member_access_expr (deref_fp, resume_idx_m, NULL_TREE, false,
				      tf_warning_or_error);
  r = build_int_cst (short_unsigned_type_node, 0);
  r = build2_loc (fn_start, INIT_EXPR, short_unsigned_type_node, resume_idx, r);
  r = coro_build_cvt_void_expr_stmt (r, fn_start);
  add_stmt (r);

  /* So .. call the actor ..  */
  r = build_call_expr_loc (fn_start, actor, 1, coro_fp);
  r = maybe_cleanup_point_expr_void (r);
  add_stmt (r);

  /* Switch to using 'input_location' as the loc, since we're now more
     logically doing things related to the end of the function.  */

  /* The ramp is done, we just need the return value.
     [dcl.fct.def.coroutine] / 7
     The expression promise.get_return_object() is used to initialize the
     glvalue result or prvalue result object of a call to a coroutine.

     If the 'get return object' is non-void, then we built it before the
     promise was constructed.  We now supply a reference to that var,
     either as the return value (if it's the same type) or to the CTOR
     for an object of the return type.  */

  if (same_type_p (gro_type, fn_return_type))
    r = gro_is_void_p ? NULL_TREE : DECL_RESULT (orig);
  else
    {
      if (CLASS_TYPE_P (fn_return_type))
	{
	  /* For class type return objects, we can attempt to construct,
	     even if the gro is void.  */
	  vec<tree, va_gc> *args = NULL;
	  vec<tree, va_gc> **arglist = NULL;
	  if (!gro_is_void_p)
	    {
	      args = make_tree_vector_single (rvalue (gro));
	      arglist = &args;
	    }
	  r = build_special_member_call (NULL_TREE,
					 complete_ctor_identifier, arglist,
					 fn_return_type, LOOKUP_NORMAL,
					 tf_warning_or_error);
	  r = build_cplus_new (fn_return_type, r, tf_warning_or_error);
	  if (args)
	    release_tree_vector (args);
	}
      else if (gro_is_void_p)
	{
	  /* We can't initialize a non-class return value from void.  */
	  error_at (input_location, "cannot initialize a return object of type"
		    " %qT with an rvalue of type %<void%>", fn_return_type);
	  r = error_mark_node;
	}
      else
	r = build1_loc (input_location, CONVERT_EXPR,
			fn_return_type, rvalue (gro));
    }

  finish_return_stmt (r);

  if (gro_cleanup_stmt)
    {
      CLEANUP_BODY (gro_cleanup_stmt)
	= pop_stmt_list (CLEANUP_BODY (gro_cleanup_stmt));
      add_stmt (gro_cleanup_stmt);
    }

  /* Finish up the ramp function.  */
  BIND_EXPR_VARS (gro_context_bind) = gro_bind_vars;
  BIND_EXPR_BODY (gro_context_bind) = pop_stmt_list (gro_context_body);
  TREE_SIDE_EFFECTS (gro_context_bind) = true;

  if (flag_exceptions)
    {
      TRY_HANDLERS (ramp_cleanup) = push_stmt_list ();
      tree handler = begin_handler ();
      finish_handler_parms (NULL_TREE, handler); /* catch (...) */

      /* If we have a live G.R.O in the return slot, then run its DTOR.
     When the return object is constructed from a separate g.r.o, this is
     already handled by its regular cleanup.  */
      if (gro_ret_dtor && gro_ret_dtor != error_mark_node)
	{
	  tree gro_d_if = begin_if_stmt ();
	  finish_if_stmt_cond (coro_gro_live, gro_d_if);
	  finish_expr_stmt (gro_ret_dtor);
	  finish_then_clause (gro_d_if);
	  tree gro_d_if_scope = IF_SCOPE (gro_d_if);
	  IF_SCOPE (gro_d_if) = NULL;
	  gro_d_if = do_poplevel (gro_d_if_scope);
	  add_stmt (gro_d_if);
	}

      /* If the promise is live, then run its dtor if that's available.  */
      if (promise_dtor && promise_dtor != error_mark_node)
	{
	  tree promise_d_if = begin_if_stmt ();
	  finish_if_stmt_cond (coro_promise_live, promise_d_if);
	  finish_expr_stmt (promise_dtor);
	  finish_then_clause (promise_d_if);
	  tree promise_d_if_scope = IF_SCOPE (promise_d_if);
	  IF_SCOPE (promise_d_if) = NULL;
	  promise_d_if = do_poplevel (promise_d_if_scope);
	  add_stmt (promise_d_if);
	}

      /* Clean up any frame copies of parms with non-trivial dtors.  */
      if (DECL_ARGUMENTS (orig))
	for (tree arg = DECL_ARGUMENTS (orig); arg != NULL;
	     arg = DECL_CHAIN (arg))
	  {
	    param_info *parm_i = param_uses->get (arg);
	    if (parm_i->trivial_dtor)
	      continue;
	    if (parm_i->fr_copy_dtor && parm_i->fr_copy_dtor != error_mark_node)
	      {
		tree dtor_if = begin_if_stmt ();
		finish_if_stmt_cond (parm_i->guard_var, dtor_if);
		finish_expr_stmt (parm_i->fr_copy_dtor);
		finish_then_clause (dtor_if);
		tree parm_d_if_scope = IF_SCOPE (dtor_if);
		IF_SCOPE (dtor_if) = NULL;
		dtor_if = do_poplevel (parm_d_if_scope);
		add_stmt (dtor_if);
	      }
	  }

      /* We always expect to delete the frame.  */
      tree del_coro_fr = coro_get_frame_dtor (coro_fp, orig, frame_size,
					      promise_type, fn_start);
      finish_expr_stmt (del_coro_fr);
      tree rethrow = build_throw (fn_start, NULL_TREE);
      TREE_NO_WARNING (rethrow) = true;
      finish_expr_stmt (rethrow);
      finish_handler (handler);
      TRY_HANDLERS (ramp_cleanup) = pop_stmt_list (TRY_HANDLERS (ramp_cleanup));
    }

  BIND_EXPR_BODY (ramp_bind) = pop_stmt_list (ramp_body);
  TREE_SIDE_EFFECTS (ramp_bind) = true;

  /* Start to build the final functions.

     We push_deferring_access_checks to avoid these routines being seen as
     nested by the middle end; we are doing the outlining here.  */

  push_deferring_access_checks (dk_no_check);

  /* Build the actor...  */
  build_actor_fn (fn_start, coro_frame_type, actor, fnbody, orig, param_uses,
		  &local_var_uses, param_dtor_list, resume_fn_field,
		  resume_idx_field, body_aw_points.await_number, frame_size);

  /* Destroyer ... */
  build_destroy_fn (fn_start, coro_frame_type, destroy, actor);

  pop_deferring_access_checks ();

  DECL_SAVED_TREE (orig) = newbody;
  /* Link our new functions into the list.  */
  TREE_CHAIN (destroy) = TREE_CHAIN (orig);
  TREE_CHAIN (actor) = destroy;
  TREE_CHAIN (orig) = actor;

  *resumer = actor;
  *destroyer = destroy;

  delete suspend_points;
  suspend_points = NULL;
  return true;
}

#include "gt-cp-coroutines.h"

