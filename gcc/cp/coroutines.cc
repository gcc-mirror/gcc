/* coroutine-specific state, expansions and tests.

   Copyright (C) 2018-2025 Free Software Foundation, Inc.

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
#include "coroutines.h"

/* ================= Debug. ================= */

#include "langhooks.h"
#include "cxx-pretty-print.h"

/* Walk through the fields of the type TYP and print them to the pretty printer
   PP.  */

static void
dump_record_fields (cxx_pretty_printer *pp, tree typ)
{
  pp->type_id (typ);
  pp_newline_and_indent (pp, 2);
  pp_left_brace (pp);
  pp_indentation (pp) += 2;

  /* We'll be on a new line, we don't need padding.  */
  pp->set_padding (pp_none);

  for (tree tmp = TYPE_FIELDS (typ); tmp; tmp = DECL_CHAIN (tmp))
    {
      pp_newline_and_indent (pp, 0);
      pp->declaration (tmp);
    }

  pp_newline_and_indent (pp, -2);
  pp_right_brace (pp);
  pp_newline_and_indent (pp, -2);
}

/* The lang-coro stream.  */
static FILE *dmp_str = NULL;

/* ID of the lang-coro dump. */
int coro_dump_id;

/* Flags passed to the lang-coro dump.  */
static dump_flags_t coro_dump_flags;

/* Pretty print the function FNDECL, which ought to be a coroutine before
   co_await expansion, into the lang-coro dump, if it is enabled.  */

static void
coro_maybe_dump_initial_function (tree fndecl)
{
  if (!dmp_str)
    return;

  bool lambda_p = LAMBDA_TYPE_P (DECL_CONTEXT (fndecl));
  fprintf (dmp_str, "%s %s original:\n",
	   (lambda_p ? "Lambda" : "Function"),
	    lang_hooks.decl_printable_name (fndecl, 2));

  cxx_pretty_printer pp;
  pp.set_output_stream (dmp_str);
  pp.flags = coro_dump_flags;
  pp.declaration (fndecl);
  pp_newline_and_flush (&pp);
}

/* Pretty print the RAMP function to the lang-coro dump, if it is enabled.  */

static void
coro_maybe_dump_ramp (tree ramp)
{
  if (!dmp_str)
    return;

  cxx_pretty_printer pp;
  pp.set_output_stream (dmp_str);
  pp.flags = coro_dump_flags;

  pp_string (&pp, "Ramp function:");
  pp_newline_and_indent (&pp, 0);
  pp.declaration (ramp);
  pp_newline_and_flush (&pp);
}

/* For a given ACTOR and DESTROY, if lang-coro dumping is enabled, pretty-print
   their contents to the lang-coro dump.  */

static void
coro_maybe_dump_transformed_functions (tree actor, tree destroy)
{
  if (!dmp_str)
    return;

  cxx_pretty_printer pp;
  pp.set_output_stream (dmp_str);
  pp.flags = coro_dump_flags;

  if (!actor || actor == error_mark_node)
    {
      pp_string (&pp, "Transform failed");
      pp_newline_and_flush (&pp);
      return;
    }

  tree frame = TREE_TYPE (TREE_TYPE (DECL_ARGUMENTS (actor)));
  pp_string (&pp, "Frame type:");
  pp_newline (&pp);
  dump_record_fields (&pp, frame);
  pp_newline_and_flush (&pp);

  pp_string (&pp, "Actor/resumer:");
  pp_newline (&pp);
  pp.declaration (actor);
  pp_newline_and_flush (&pp);

  pp_string (&pp, "Destroyer:");
  pp_newline (&pp);
  pp.declaration (destroy);
  pp_newline_and_flush (&pp);
}

/* ================= END Debug. ================= */

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
  tree actor_decl;    /* The synthesized actor function.  */
  tree destroy_decl;  /* The synthesized destroy function.  */
  tree promise_type;  /* The cached promise type for this function.  */
  tree traits_type;   /* The cached traits type for this function.  */
  tree handle_type;   /* The cached coroutine handle for this function.  */
  tree self_h_proxy;  /* A handle instance that is used as the proxy for the
			 one that will eventually be allocated in the coroutine
			 frame.  */
  tree promise_proxy; /* Likewise, a proxy promise instance.  */
  tree from_address;  /* handle_type from_address function.  */
  tree return_void;   /* The expression for p.return_void() if it exists.  */
  location_t first_coro_keyword; /* The location of the keyword that made this
				    function into a coroutine.  */

  /* Temporary variable number assigned by get_awaitable_var.  */
  int awaitable_number = 0;

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

/* We will initialize state lazily.  */
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
static GTY(()) tree coro_address_identifier;
static GTY(()) tree coro_from_address_identifier;
static GTY(()) tree coro_get_return_object_identifier;
static GTY(()) tree coro_gro_on_allocation_fail_identifier;
static GTY(()) tree coro_unhandled_exception_identifier;

/* Awaitable methods.  */

static GTY(()) tree coro_await_ready_identifier;
static GTY(()) tree coro_await_suspend_identifier;
static GTY(()) tree coro_await_resume_identifier;

/* Accessors for the coroutine frame state used by the implementation.  */

static GTY(()) tree coro_resume_fn_id;
static GTY(()) tree coro_destroy_fn_id;
static GTY(()) tree coro_promise_id;
static GTY(()) tree coro_frame_needs_free_id;
static GTY(()) tree coro_resume_index_id;
static GTY(()) tree coro_self_handle_id;
static GTY(()) tree coro_actor_continue_id;
static GTY(()) tree coro_frame_i_a_r_c_id;

/* Create the identifiers used by the coroutines library interfaces and
   the implementation frame state.  */

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
  coro_address_identifier = get_identifier ("address");
  coro_from_address_identifier = get_identifier ("from_address");
  coro_get_return_object_identifier = get_identifier ("get_return_object");
  coro_gro_on_allocation_fail_identifier =
    get_identifier ("get_return_object_on_allocation_failure");
  coro_unhandled_exception_identifier = get_identifier ("unhandled_exception");

  coro_await_ready_identifier = get_identifier ("await_ready");
  coro_await_suspend_identifier = get_identifier ("await_suspend");
  coro_await_resume_identifier = get_identifier ("await_resume");

  /* Coroutine state frame field accessors.  */
  coro_resume_fn_id = get_identifier ("_Coro_resume_fn");
  coro_destroy_fn_id = get_identifier ("_Coro_destroy_fn");
  coro_promise_id = get_identifier ("_Coro_promise");
  coro_frame_needs_free_id = get_identifier ("_Coro_frame_needs_free");
  coro_frame_i_a_r_c_id = get_identifier ("_Coro_initial_await_resume_called");
  coro_resume_index_id = get_identifier ("_Coro_resume_index");
  coro_self_handle_id = get_identifier ("_Coro_self_handle");
  coro_actor_continue_id = get_identifier ("_Coro_actor_continue");
}

/* Trees we only need to set up once.  */

static GTY(()) tree coro_traits_templ;
static GTY(()) tree coro_handle_templ;
static GTY(()) tree void_coro_handle_type;
static GTY(()) tree void_coro_handle_address;

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
  /* If we are missing fundamental information, such as the traits, (or the
     declaration found is not a type template), then don't emit an error for
     every keyword in a TU, just do it once.  */
  static bool traits_error_emitted = false;

  tree traits_decl = lookup_qualified_name (std_node, coro_traits_identifier,
					    LOOK_want::NORMAL,
					    /*complain=*/!traits_error_emitted);
  if (traits_decl == error_mark_node
      || !DECL_TYPE_TEMPLATE_P (traits_decl))
    {
      if (!traits_error_emitted)
	{
	  auto_diagnostic_group d;
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
  ARGUMENT_PACK_ARGS (argtypepack) = argtypes;

  tree targ = make_tree_vec (2);
  TREE_VEC_ELT (targ, 0) = TREE_TYPE (functyp);
  TREE_VEC_ELT (targ, 1) = argtypepack;

  tree traits_class
    = lookup_template_class (coro_traits_templ, targ,
			     /*in_decl=*/NULL_TREE, /*context=*/NULL_TREE,
			     tf_warning_or_error);

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
					    LOOK_want::NORMAL,
					    !coro_handle_error_emitted);
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

/* Get and validate HANDLE_TYPE::address.  The resulting function, if any, will
   be a non-overloaded member function that takes no arguments and returns
   void*.  If that is not the case, signals an error and returns NULL_TREE.  */

static tree
get_handle_type_address (location_t kw, tree handle_type)
{
  tree addr_getter = lookup_member (handle_type, coro_address_identifier, 1,
				    0, tf_warning_or_error);
  if (!addr_getter || addr_getter == error_mark_node)
    {
      qualified_name_lookup_error (handle_type, coro_address_identifier,
				   error_mark_node, kw);
      return NULL_TREE;
    }

  if (!BASELINK_P (addr_getter)
      || TREE_CODE (TREE_TYPE (addr_getter)) != METHOD_TYPE)
    {
      error_at (kw, "%qE must be a non-overloaded method", addr_getter);
      return NULL_TREE;
    }

  tree fn_t = TREE_TYPE (addr_getter);
  tree arg = TYPE_ARG_TYPES (fn_t);

  /* Skip the 'this' pointer.  */
  arg = TREE_CHAIN (arg);

  /* Check that from_addr has the argument list ().  */
  if (arg != void_list_node)
    {
      error_at (kw, "%qE must take no arguments", addr_getter);
      return NULL_TREE;
    }

  tree ret_t = TREE_TYPE (fn_t);
  if (!same_type_p (ret_t, ptr_type_node))
    {
      error_at (kw, "%qE must return %qT, not %qT",
		addr_getter, ptr_type_node, ret_t);
      return NULL_TREE;
    }

  return addr_getter;
}

/* Get and validate HANDLE_TYPE::from_address.  The resulting function, if
   any, will be a non-overloaded static function that takes a single void* and
   returns HANDLE_TYPE.  If that is not the case, signals an error and returns
   NULL_TREE.  */

static tree
get_handle_type_from_address (location_t kw, tree handle_type)
{
  tree from_addr = lookup_member (handle_type, coro_from_address_identifier, 1,
				  0, tf_warning_or_error);
  if (!from_addr || from_addr == error_mark_node)
    {
      qualified_name_lookup_error (handle_type, coro_from_address_identifier,
				   error_mark_node, kw);
      return NULL_TREE;
    }
  if (!BASELINK_P (from_addr)
      || TREE_CODE (TREE_TYPE (from_addr)) != FUNCTION_TYPE)
    {
      error_at (kw, "%qE must be a non-overloaded static function", from_addr);
      return NULL_TREE;
    }

  tree fn_t = TREE_TYPE (from_addr);
  tree arg = TYPE_ARG_TYPES (fn_t);
  /* Check that from_addr has the argument list (void*).  */
  if (!arg
      || !same_type_p (TREE_VALUE (arg), ptr_type_node)
      || TREE_CHAIN (arg) != void_list_node)
    {
      error_at (kw, "%qE must take a single %qT", from_addr, ptr_type_node);
      return NULL_TREE;
    }

  tree ret_t = TREE_TYPE (fn_t);
  if (!same_type_p (ret_t, handle_type))
    {
      error_at (kw, "%qE must return %qT, not %qT",
		from_addr, handle_type, ret_t);
      return NULL_TREE;
    }

  return from_addr;
}

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
			     tf_warning_or_error);

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

/* Perform initialization of the coroutine processor state, if not done
   before.  */

static bool
ensure_coro_initialized (location_t loc)
{
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
      void_coro_handle_type
	= instantiate_coro_handle_for_promise_type (loc, void_type_node);
      if (void_coro_handle_type == NULL_TREE)
	return false;

      void_coro_handle_address
	= get_handle_type_address (loc, void_coro_handle_type);
      if (!void_coro_handle_address)
	return false;

      /* A table to hold the state, per coroutine decl.  */
      gcc_checking_assert (coroutine_info_table == NULL);
      coroutine_info_table =
	hash_table<coroutine_info_hasher>::create_ggc (11);

      if (coroutine_info_table == NULL)
	return false;

      coro_initialized = true;
    }
  return true;
}

/* Try to get the coroutine traits class.  */
static tree
coro_get_traits_class (tree fndecl, location_t loc)
{
  gcc_assert (fndecl != NULL_TREE);
  gcc_assert (coro_initialized);

  coroutine_info *coro_info = get_or_insert_coroutine_info (fndecl);
  auto& traits_type = coro_info->traits_type;
  if (!traits_type)
    traits_type = instantiate_coro_traits (fndecl, loc);
  return traits_type;
}

static bool
coro_promise_type_found_p (tree fndecl, location_t loc)
{
  gcc_assert (fndecl != NULL_TREE);

  if (!ensure_coro_initialized (loc))
    return false;

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

      tree templ_class = coro_get_traits_class (fndecl, loc);

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
	  auto_diagnostic_group d;
	  location_t ploc = DECL_SOURCE_LOCATION (fndecl);
	  if (!coro_info->coro_co_return_error_emitted)
	    error_at (ploc, "the coroutine promise type %qT declares both"
		      " %<return_value%> and %<return_void%>",
		      coro_info->promise_type);
	  inform (DECL_SOURCE_LOCATION (BASELINK_FUNCTIONS (has_ret_void)),
		  "%<return_void%> declared here");
	  has_ret_val = BASELINK_FUNCTIONS (has_ret_val);
	  const char *message = "%<return_value%> declared here";
	  if (TREE_CODE (has_ret_val) == OVERLOAD)
	    {
	      has_ret_val = OVL_FIRST (has_ret_val);
	      message = "%<return_value%> first declared here";
	    }
	  inform (DECL_SOURCE_LOCATION (has_ret_val), message);
	  coro_info->coro_co_return_error_emitted = true;
	  return false;
	}

      /* Try to find the handle type for the promise.  */
      tree handle_type
	= instantiate_coro_handle_for_promise_type (loc, coro_info->promise_type);
      if (handle_type == NULL_TREE)
	return false;
      tree from_address = get_handle_type_from_address (loc, handle_type);
      if (from_address == NULL_TREE)
	return false;

      /* Complete this, we're going to use it.  */
      coro_info->handle_type = complete_type_or_else (handle_type, fndecl);
      coro_info->from_address = from_address;

      /* Diagnostic would be emitted by complete_type_or_else.  */
      if (!coro_info->handle_type)
	return false;

      /* Build a proxy for a handle to "self" as the param to
	 await_suspend() calls.  */
      coro_info->self_h_proxy
	= build_lang_decl (VAR_DECL, coro_self_handle_id,
			   coro_info->handle_type);

      /* Build a proxy for the promise so that we can perform lookups.  */
      coro_info->promise_proxy
	= build_lang_decl (VAR_DECL, coro_promise_id,
			   coro_info->promise_type);

      /* Note where we first saw a coroutine keyword.  */
      coro_info->first_coro_keyword = loc;
    }

  return true;
}

/* Map from actor or destroyer to ramp.  */
static GTY(()) hash_map<tree, tree> *to_ramp;

/* Given a tree that is an actor or destroy, find the ramp function.  */

tree
coro_get_ramp_function (tree decl)
{
  if (!to_ramp)
    return NULL_TREE;
  tree *p = to_ramp->get (decl);
  if (p)
    return *p;
  return NULL_TREE;
}

/* Given the DECL for a ramp function (the user's original declaration) return
   the actor function if it has been defined.  */

tree
coro_get_actor_function (tree decl)
{
  if (coroutine_info *info = get_coroutine_info (decl))
    return info->actor_decl;

  return NULL_TREE;
}

/* Given the DECL for a ramp function (the user's original declaration) return
   the destroy function if it has been defined.  */

tree
coro_get_destroy_function (tree decl)
{
  if (coroutine_info *info = get_coroutine_info (decl))
    return info->destroy_decl;

  return NULL_TREE;
}

/* Given a CO_AWAIT_EXPR AWAIT_EXPR, return its resume call.  */

tree
co_await_get_resume_call (tree await_expr)
{
  gcc_checking_assert (TREE_CODE (await_expr) == CO_AWAIT_EXPR);
  tree vec = TREE_OPERAND (await_expr, 3);
  if (!vec)
    return nullptr;
  return TREE_VEC_ELT (vec, 2);
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
get_coroutine_from_address (tree decl)
{
  if (coroutine_info *info = get_coroutine_info (decl))
    return info->from_address;

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
  auto cinfo = get_coroutine_info (current_function_decl);
  gcc_checking_assert (cinfo);
  char *buf;
  switch (suspend_kind)
    {
    default: buf = xasprintf ("Aw%d", cinfo->awaitable_number++); break;
    case CO_YIELD_SUSPEND_POINT:
      buf = xasprintf ("Yd%d", cinfo->awaitable_number++);
      break;
    case INITIAL_SUSPEND_POINT: buf = xasprintf ("Is"); break;
    case FINAL_SUSPEND_POINT: buf = xasprintf ("Fs"); break;
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
      auto_diagnostic_group d;
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
  if (TREE_CODE (expr) == TARGET_EXPR)
    expr = TARGET_EXPR_INITIAL (expr);
  tree fn = NULL_TREE;
  if (TREE_CODE (expr) == CALL_EXPR)
    fn = CALL_EXPR_FN (expr);
  else if (TREE_CODE (expr) == AGGR_INIT_EXPR)
    fn = AGGR_INIT_EXPR_FN (expr);
  else if (TREE_CODE (expr) == CONSTRUCTOR)
    return false;
  else
    {
      gcc_checking_assert (0 && "unhandled expression type");
      return false;
    }
  fn = TREE_OPERAND (fn, 0);
  return coro_diagnose_throwing_fn (fn);
}

/* Build a co_await suitable for later expansion.  */

tree
build_template_co_await_expr (location_t kw, tree type, tree expr, tree kind)
{
  tree aw_expr = build5_loc (kw, CO_AWAIT_EXPR, type, expr,
			     NULL_TREE, NULL_TREE, NULL_TREE,
			     kind);
  TREE_SIDE_EFFECTS (aw_expr) = true;
  return aw_expr;
}

/* Is EXPR an lvalue that will refer to the same object after a resume?

   This is close to asking tree_invariant_p of its address, but that doesn't
   distinguish temporaries from other variables.  */

static bool
is_stable_lvalue (tree expr)
{
  if (TREE_SIDE_EFFECTS (expr))
    return false;

  for (; handled_component_p (expr);
       expr = TREE_OPERAND (expr, 0))
    {
      if (TREE_CODE (expr) == ARRAY_REF
	  && !TREE_CONSTANT (TREE_OPERAND (expr, 1)))
	return false;
    }

  return (TREE_CODE (expr) == PARM_DECL
	  || (VAR_P (expr) && !is_local_temp (expr)));
}

/*  This performs [expr.await] bullet 3.3 and validates the interface obtained.
    It is also used to build the initial and final suspend points.

    'a', 'o' and 'e' are used as per the description in the section noted.

    A, the original yield/await expr, is found at source location LOC.

    We will be constructing a CO_AWAIT_EXPR for a suspend point of one of
    the four suspend_point_kind kinds.  This is indicated by SUSPEND_KIND.

    In the case that we're processing a template declaration, we can't save
    actual awaiter expressions as the frame type will differ between
    instantiations, but we can generate them to type-check them and compute the
    resulting expression type.  In those cases, we will return a
    template-appropriate CO_AWAIT_EXPR and throw away the rest of the results.
    Such an expression requires the original co_await operand unaltered.  Pass
    it as ORIG_OPERAND.  If it is the same as 'a', you can pass NULL_TREE (the
    default) to use 'a' as the value.  */

static tree
build_co_await (location_t loc, tree a, suspend_point_kind suspend_kind,
		tree orig_operand = NULL_TREE)
{
  if (orig_operand == NULL_TREE)
    orig_operand = a;

  /* Try and overload of operator co_await, .... */
  tree o;
  if (MAYBE_CLASS_TYPE_P (TREE_TYPE (a)))
    {
      o = build_new_op (loc, CO_AWAIT_EXPR, LOOKUP_NORMAL, a, NULL_TREE,
			NULL_TREE, NULL_TREE, NULL, tf_warning_or_error);
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
	  if (tree dummy = cxx_maybe_build_cleanup (a, tf_none))
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

  if (!o_type || o_type == error_mark_node)
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

  /* [expr.await]/3.3 If o would be a prvalue, the temporary
     materialization conversion ([conv.rval]) is applied.  */
  if (!glvalue_p (o))
    o = get_target_expr (o, tf_warning_or_error);

  /* [expr.await]/3.4 e is an lvalue referring to the result of evaluating the
     (possibly-converted) o.

     So, either reuse an existing stable lvalue such as a variable or
     COMPONENT_REF thereof, or create a new a coroutine state frame variable
     for the awaiter, since it must persist across suspension.  */
  tree e_var = NULL_TREE;
  tree e_proxy = o;
  if (is_stable_lvalue (o))
    o = NULL_TREE; /* Use the existing entity.  */
  else /* We need a non-temp var.  */
    {
      tree p_type = TREE_TYPE (o);
      tree o_a = o;
      if (glvalue_p (o))
	{
	  /* Build a reference variable for a non-stable lvalue o.  */
	  p_type = cp_build_reference_type (p_type, xvalue_p (o));
	  o_a = build_address (o);
	  o_a = cp_fold_convert (p_type, o_a);
	}
      e_var = get_awaitable_var (suspend_kind, p_type);
      o = cp_build_init_expr (loc, e_var, o_a);
      e_proxy = convert_from_reference (e_var);
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
	   && CLASS_TYPE_P (susp_return_type)
	   && CLASSTYPE_TEMPLATE_INFO (susp_return_type))
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
      if (tree dummy = cxx_maybe_build_cleanup (e_var, tf_none))
	{
	  if (CONVERT_EXPR_P (dummy))
	    dummy = TREE_OPERAND (dummy, 0);
	  dummy = TREE_OPERAND (CALL_EXPR_FN (dummy), 0);
	  if (coro_diagnose_throwing_fn (dummy))
	    return error_mark_node;
	}
    }

  /* We now have three call expressions, in terms of the promise, handle and
     'e' proxy expression.  Save them in the await expression for later
     expansion.  */

  tree awaiter_calls = make_tree_vec (3);
  TREE_VEC_ELT (awaiter_calls, 0) = awrd_call; /* await_ready().  */
  TREE_VEC_ELT (awaiter_calls, 1) = awsp_call; /* await_suspend().  */
  tree te = NULL_TREE;
  if (TREE_CODE (awrs_call) == TARGET_EXPR)
    {
      te = awrs_call;
      awrs_call = TARGET_EXPR_INITIAL (awrs_call);
    }
  TREE_VEC_ELT (awaiter_calls, 2) = awrs_call; /* await_resume().  */

  if (e_var)
    e_proxy = e_var;

  tree awrs_type = TREE_TYPE (TREE_TYPE (awrs_func));
  tree suspend_kind_cst = build_int_cst (integer_type_node,
					 (int) suspend_kind);
  tree await_expr = build5_loc (loc, CO_AWAIT_EXPR,
				awrs_type,
				a, e_proxy, o, awaiter_calls,
				suspend_kind_cst);
  TREE_SIDE_EFFECTS (await_expr) = true;
  if (te)
    {
      TREE_OPERAND (te, 1) = await_expr;
      TREE_SIDE_EFFECTS (te) = true;
      await_expr = te;
    }
  SET_EXPR_LOCATION (await_expr, loc);

  if (processing_template_decl)
    return build_template_co_await_expr (loc, awrs_type, orig_operand,
					 suspend_kind_cst);
 return convert_from_reference (await_expr);
}

/* Returns true iff EXPR or the TRAITS_CLASS, which should be a
   coroutine_traits instance, are dependent.  In those cases, we can't decide
   what the types of our co_{await,yield,return} expressions are, so we defer
   expansion entirely.  */

static bool
coro_dependent_p (tree expr, tree traits_class)
{
  return type_dependent_expression_p (expr)
    || dependent_type_p (traits_class);
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
  suppress_warning (current_function_decl, OPT_Wreturn_type);

  /* Prepare for coroutine transformations.  */
  if (!ensure_coro_initialized (kw))
    return error_mark_node;

  /* Get our traits class.  */
  tree traits_class = coro_get_traits_class (current_function_decl, kw);

  /* Defer expansion when we are processing a template, unless the traits type
     and expression would not be dependent.  In the case that the types are
     not dependent but we are processing a template declaration, we will do
     most of the computation but throw away the results (except for the
     await_resume type).  Otherwise, if our co_await is type-dependent
     (i.e. the promise type or operand type is dependent), we can't do much,
     and just return early with a NULL_TREE type (indicating that we cannot
     compute the type yet).  */
  if (coro_dependent_p (expr, traits_class))
    return build_template_co_await_expr (kw, NULL_TREE, expr,
					 integer_zero_node);

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
      vec<tree, va_gc> *args = make_tree_vector_single (expr);
      a = build_new_method_call (get_coroutine_promise_proxy (
				   current_function_decl),
				 at_meth, &args, NULL_TREE, LOOKUP_NORMAL,
				 NULL, tf_warning_or_error);

      /* As I read the section.
	 We saw an await_transform method, so it's mandatory that we replace
	 expr with p.await_transform (expr), therefore if the method call fails
	 (presumably, we don't have suitable arguments) then this part of the
	 process fails.  */
      if (a == error_mark_node)
	return error_mark_node;
    }

  /* Now we want to build co_await a.  */
  return build_co_await (kw, a, CO_AWAIT_SUSPEND_POINT, expr);
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
  suppress_warning (current_function_decl, OPT_Wreturn_type);

  /* Prepare for coroutine transformations.  */
  if (!ensure_coro_initialized (kw))
    return error_mark_node;

  /* Get our traits class.  */
  tree traits_class = coro_get_traits_class (current_function_decl, kw);

  /* Defer expansion when we are processing a template; see note in
     finish_co_await_expr.  Also note that a yield is equivalent to

       co_await p.yield_value(EXPR)

      If either p or EXPR are type-dependent, then the whole expression is
      certainly type-dependent, and we can't proceed.  */
  if (coro_dependent_p (expr, traits_class))
    return build2_loc (kw, CO_YIELD_EXPR, NULL_TREE, expr, NULL_TREE);

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

  /* Now build co_await p.yield_value (e).
     Noting that for co_yield, there is no evaluation of any potential
     promise transform_await(), so we call build_co_await directly.  */

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
	  tree t = TARGET_EXPR_INITIAL (op);
	  t = build2_loc (kw, CO_YIELD_EXPR, TREE_TYPE (t), expr, t);
	  TARGET_EXPR_INITIAL (op) = t;
	}
      else
	op = build2_loc (kw, CO_YIELD_EXPR, TREE_TYPE (op), expr, op);
      TREE_SIDE_EFFECTS (op) = 1;
      op = convert_from_reference (op);
    }

  return op;
}

/* Check and build a co_return statement.
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
  suppress_warning (current_function_decl, OPT_Wreturn_type);

  /* Prepare for coroutine transformations.  */
  if (!ensure_coro_initialized (kw))
    return error_mark_node;

  /* Get our traits class.  */
  tree traits_class = coro_get_traits_class (current_function_decl, kw);

  if (processing_template_decl
      && check_for_bare_parameter_packs (expr))
    return error_mark_node;

  /* Defer expansion when we must and are processing a template; see note in
     finish_co_await_expr.  */
  if (coro_dependent_p (expr, traits_class))
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
  suppress_warning (current_function_decl, OPT_Wreturn_type);

  if (!processing_template_decl && warn_sequence_point)
    verify_sequence_points (expr);

  if (expr)
    {
      /* If we had an id-expression obfuscated by force_paren_expr, we need
	 to undo it so we can try to treat it as an rvalue below.  */
      expr = maybe_undo_parenthesized_ref (expr);

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

      tree arg = expr;
      if (tree moved = treat_lvalue_as_rvalue_p (expr, /*return*/true))
	arg = moved;

      releasing_vec args = make_tree_vector_single (arg);
      co_ret_call
	= coro_build_promise_expression (current_function_decl, NULL,
					 coro_return_value_identifier, kw,
					 &args, /*musthave=*/true);
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
	if (TREE_CODE (arg) == ALIGNOF_EXPR)
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

/* Helper to build a coroutine state frame access expression
   CORO_FP is a frame pointer
   MEMBER_ID is an identifier for a frame field
   PRESERVE_REF is true, the expression returned will have REFERENCE_TYPE if
   the MEMBER does.
   COMPLAIN is passed to the underlying functions. */

static tree
coro_build_frame_access_expr (tree coro_ref, tree member_id, bool preserve_ref,
			      tsubst_flags_t complain)
{
  gcc_checking_assert (INDIRECT_REF_P (coro_ref));
  tree fr_type = TREE_TYPE (coro_ref);
  tree mb = lookup_member (fr_type, member_id, /*protect=*/1, /*want_type=*/0,
			   complain);
  if (!mb || mb == error_mark_node)
    return error_mark_node;
  tree expr
    = build_class_member_access_expr (coro_ref, mb, NULL_TREE,
				      preserve_ref, complain);
  return expr;
}

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

/* Helpers to build an artificial var, with location LOC, NAME and TYPE, in
   CTX, and with initializer INIT.  */

static tree
coro_build_artificial_var (location_t loc, tree name, tree type, tree ctx,
			   tree init)
{
  tree res = build_lang_decl (VAR_DECL, name, type);
  DECL_SOURCE_LOCATION (res) = loc;
  DECL_CONTEXT (res) = ctx;
  DECL_ARTIFICIAL (res) = true;
  DECL_INITIAL (res) = init;
  return res;
}

/* As above, except allowing the name to be a string.  */

static tree
coro_build_artificial_var (location_t loc, const char *name, tree type,
			   tree ctx, tree init)
{
  return coro_build_artificial_var (loc, get_identifier (name),
				    type, ctx, init);
}

/* As for coro_build_artificial_var, except that we push this decl into
   the current binding scope and add the decl_expr.  */

static tree
coro_build_and_push_artificial_var (location_t loc, const char *name,
				    tree type, tree ctx, tree init)
{
  tree v = coro_build_artificial_var (loc, name, type, ctx, init);
  v = pushdecl (v);
  add_decl_expr (v);
  return v;
}

/* Build a var NAME of TYPE in CTX and with INIT; add a DECL_VALUE_EXPR that
   refers to BASE.FIELD.  */

static tree
coro_build_artificial_var_with_dve (location_t loc, tree name, tree type,
				    tree ctx, tree init, tree base,
				    tree field = NULL_TREE)
{
  tree v = coro_build_artificial_var (loc, name, type, ctx, init);
  if (field == NULL_TREE)
    field = name;
  tree dve  = coro_build_frame_access_expr (base, field, true,
					    tf_warning_or_error);
  SET_DECL_VALUE_EXPR (v, dve);
  DECL_HAS_VALUE_EXPR_P (v) = true;
  return v;
}

/* As for coro_build_artificial_var_with_dve, but push into the current binding
   scope and add the decl_expr.  */

static tree
coro_build_and_push_artificial_var_with_dve (location_t loc, tree name,
					     tree type, tree ctx, tree init,
					     tree base, tree field = NULL_TREE)
{
  tree v = coro_build_artificial_var_with_dve (loc, name, type, ctx, init,
					       base, field);
  v = pushdecl (v);
  add_decl_expr (v);
  return v;
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
  tree dispatch;   /* This is where we go if we restart the dispatch.  */
  tree conthand;   /* This is the handle for a continuation.  */
  unsigned index;  /* This is our current resume index.  */
};

/* Lightweight search for the first await expression in tree-walk order.
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

/* Starting with a statement:

   stmt => some tree containing one or more await expressions.

   We replace the statement with:
   <STATEMENT_LIST> {
      initialize awaitable
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
expand_one_await_expression (tree *expr, tree *await_expr, void *d)
{
  coro_aw_data *data = (coro_aw_data *) d;

  tree saved_statement = *expr;
  tree saved_co_await = *await_expr;

  tree actor = data->actor_fn;
  location_t loc = EXPR_LOCATION (*expr);
  tree var = TREE_OPERAND (saved_co_await, 1);  /* frame slot. */
  tree init_expr = TREE_OPERAND (saved_co_await, 2); /* initializer.  */
  tree awaiter_calls = TREE_OPERAND (saved_co_await, 3);

  tree source = TREE_OPERAND (saved_co_await, 4);
  bool is_final = (source
		   && TREE_INT_CST_LOW (source) == (int) FINAL_SUSPEND_POINT);

  /* Build labels for the destinations of the control flow when we are resuming
     or destroying.  */
  int resume_point = data->index;
  char *buf = xasprintf ("destroy.%d", resume_point);
  tree destroy_label = create_named_label_with_ctx (loc, buf, actor);
  free (buf);
  buf = xasprintf ("resume.%d", resume_point);
  tree resume_label = create_named_label_with_ctx (loc, buf, actor);
  free (buf);

  /* This will contain our expanded expression and replace the original
     expression.  */
  tree stmt_list = push_stmt_list ();
  tree *await_init = NULL;

  bool needs_dtor = TYPE_HAS_NONTRIVIAL_DESTRUCTOR (TREE_TYPE (var));
  if (!init_expr)
    needs_dtor = false; /* No need, the var's lifetime is managed elsewhere.  */
  else
    {
      finish_expr_stmt (init_expr);
      /* We have an initializer, which might itself contain await exprs.  */
      await_init = tsi_stmt_ptr (tsi_last (stmt_list));
    }

  /* Use the await_ready() call to test if we need to suspend.  */
  tree ready_cond = TREE_VEC_ELT (awaiter_calls, 0); /* await_ready().  */

  /* We will resume (or continue) at the following index.  */
  tree resume_idx = build_int_cst (short_unsigned_type_node, data->index);
  tree r = cp_build_init_expr (data->resume_idx, resume_idx);
  finish_expr_stmt (r);

  /* Convert to bool, if necessary.  */
  if (TREE_CODE (TREE_TYPE (ready_cond)) != BOOLEAN_TYPE)
    ready_cond = cp_convert (boolean_type_node, ready_cond,
			     tf_warning_or_error);
  tree susp_if = begin_if_stmt ();
  /* Be aggressive in folding here, since there are a significant number of
     cases where the ready condition is constant.  */
  ready_cond = invert_truthvalue_loc (loc, ready_cond);
  finish_if_stmt_cond (ready_cond, susp_if);

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
    /* Void return - just proceed to suspend.  */
    finish_expr_stmt (suspend);
  else if (TREE_CODE (susp_type) == BOOLEAN_TYPE)
    {
      /* Boolean return, "continue" if the call returns false.  */
      tree restart_if = begin_if_stmt ();
      suspend = invert_truthvalue_loc (loc, suspend);
      finish_if_stmt_cond (suspend, restart_if);
      /* Resume - so restart the dispatcher, since we do not know if this
	 coroutine was already resumed from within await_suspend.  We must
	 exit this scope without cleanups.  */
      r = build_call_expr_internal_loc (loc, IFN_CO_SUSPN, void_type_node, 1,
					build_address (data->dispatch));
      /* This will eventually expand to 'goto coro.restart.dispatch'.  */
      finish_expr_stmt (r);
      finish_then_clause (restart_if);
      finish_if_stmt (restart_if);
    }
  else
    {
      /* Handle return, save it to the continuation.  */
      r = suspend;
      if (!same_type_ignoring_top_level_qualifiers_p (susp_type,
						      void_coro_handle_type))
	r = build1_loc (loc, VIEW_CONVERT_EXPR, void_coro_handle_type, r);
      r = cp_build_init_expr (loc, data->conthand, r);
      finish_expr_stmt (r);
      is_cont = true;
    }

  tree d_l = build_address (destroy_label);
  tree r_l = build_address (resume_label);
  tree susp = build_address (data->cororet);
  tree cont = build_address (data->corocont);
  tree final_susp = build_int_cst (integer_type_node, is_final ? 1 : 0);

  resume_idx = build_int_cst (integer_type_node, data->index);

  tree sw = begin_switch_stmt ();

  r = build_call_expr_internal_loc (loc, IFN_CO_YIELD, integer_type_node, 5,
				    resume_idx, final_susp, r_l, d_l,
				    data->coro_fp);
  finish_switch_cond (r, sw);
  finish_case_label (loc, integer_zero_node, NULL_TREE); /*  case 0: */
  /* Implement the suspend, a scope exit without clean ups.  */
  r = build_call_expr_internal_loc (loc, IFN_CO_SUSPN, void_type_node, 1,
				    is_cont ? cont : susp);
  finish_expr_stmt (r); /* This will eventually expand to 'goto return'.  */
  finish_case_label (loc, integer_one_node, NULL_TREE); /*  case 1:  */
  add_stmt (build_stmt (loc, GOTO_EXPR, resume_label)); /*  goto resume;  */
  finish_case_label (loc, NULL_TREE, NULL_TREE); /* default:;  */
  add_stmt (build_stmt (loc, GOTO_EXPR, destroy_label)); /* goto destroy;  */

  finish_switch_stmt (sw);
  add_stmt (build_stmt (loc, LABEL_EXPR, destroy_label));
  if (needs_dtor)
    finish_expr_stmt (build_cleanup (var));
  add_stmt (build_stmt (loc, GOTO_EXPR, data->cleanup));

  finish_then_clause (susp_if);
  finish_if_stmt (susp_if);

  /* Resume point.  */
  add_stmt (build_stmt (loc, LABEL_EXPR, resume_label));

  /* This will produce the value (if one is provided) from the co_await
     expression.  */
  tree resume_call = TREE_VEC_ELT (awaiter_calls, 2); /* await_resume().  */
  if (REFERENCE_REF_P (resume_call))
    /* Sink to await_resume call_expr.  */
    resume_call = TREE_OPERAND (resume_call, 0);

  *await_expr = resume_call; /* Replace the co_await expr with its result.  */
  append_to_statement_list_force (saved_statement, &stmt_list);
  /* Get a pointer to the revised statement.  */
  tree *revised = tsi_stmt_ptr (tsi_last (stmt_list));
  if (needs_dtor)
    finish_expr_stmt (build_cleanup (var));
  data->index += 2;

  /* Replace the original expression with the expansion.  */
  *expr = pop_stmt_list (stmt_list);

  /* Now, if the awaitable had an initializer, expand any awaits that might
     be embedded in it.  */
  tree *aw_expr_ptr;
  if (await_init &&
      cp_walk_tree (await_init, co_await_find_in_subtree, &aw_expr_ptr, NULL))
    expand_one_await_expression (await_init, aw_expr_ptr, d);

  /* Expand any more await expressions in the original statement.  */
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
  if (STATEMENT_CLASS_P (*stmt)
      || TREE_CODE (*stmt) == BIND_EXPR
      || TREE_CODE (*stmt) == CLEANUP_POINT_EXPR)
    return NULL_TREE; /* Just process the sub-trees.  */
  else if (TREE_CODE (*stmt) == STATEMENT_LIST)
    {
      for (tree &s : tsi_range (*stmt))
	{
	  res = cp_walk_tree (&s, await_statement_expander,
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

struct await_xform_data
{
  tree actor_fn;   /* Decl for context.  */
  tree actor_frame;
  hash_map<tree, suspend_point_info> *suspend_points;
};

/* When we built the await expressions, we didn't know the coro frame
   layout, therefore no idea where to find the promise or where to put
   the awaitables.  Now we know these things, fill them in.  */

static tree
transform_await_expr (tree await_expr, await_xform_data *xform)
{
  suspend_point_info *si = xform->suspend_points->get (await_expr);
  location_t loc = EXPR_LOCATION (await_expr);
  if (!si)
    {
      error_at (loc, "no suspend point info for %qD", await_expr);
      return error_mark_node;
    }

  /* So, on entry, we have:
     in : CO_AWAIT_EXPR (a, e_proxy, o, awr_call_vector, mode)
	  We no longer need a [it had diagnostic value, maybe?]
	  We need to replace the e_proxy in the awr_call.  */

  /* If we have a frame var for the awaitable, get a reference to it.  */
  proxy_replace data;
  if (si->await_field_id)
    {
      tree as
	= coro_build_frame_access_expr (xform->actor_frame, si->await_field_id,
					true, tf_warning_or_error);
      /* Replace references to the instance proxy with the frame entry now
	 computed.  */
      data.from = TREE_OPERAND (await_expr, 1);
      data.to = as;
      cp_walk_tree (&await_expr, replace_proxy, &data, NULL);

      /* .. and replace.  */
      TREE_OPERAND (await_expr, 1) = as;
    }

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
     accessed), build a frame reference and add it as the DECL_VALUE_EXPR.  */

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
	    continue; /* No frame entry for this.  */

	  /* TODO: implement selective generation of fields when vars are
	     known not-used.  */
	  if (local_var.field_id == NULL_TREE)
	    continue; /* Wasn't used.  */
	  tree fld_idx
	    = coro_build_frame_access_expr (lvd->actor_frame,
					    local_var.field_id, true,
					    tf_warning_or_error);
	  local_var.field_idx = fld_idx;
	  SET_DECL_VALUE_EXPR (lvar, fld_idx);
	  DECL_HAS_VALUE_EXPR_P (lvar) = true;
	}
      cp_walk_tree (&BIND_EXPR_BODY (*stmt), transform_local_var_uses, d, NULL);
      *do_subtree = 0; /* We've done the body already.  */
      return NULL_TREE;
    }
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
build_coroutine_frame_delete_expr (tree, tree, tree, location_t);

/* The actor transform.  */

static void
build_actor_fn (location_t loc, tree coro_frame_type, tree actor, tree fnbody,
		tree orig, hash_map<tree, local_var_info> *local_var_uses,
		hash_map<tree, suspend_point_info> *suspend_points,
		vec<tree> *param_dtor_list,
		tree resume_idx_var, unsigned body_count, tree frame_size,
		bool inline_p)
{
  verify_stmt_tree (fnbody);
  /* Some things we inherit from the original function.  */
  tree promise_type = get_coroutine_promise_type (orig);
  tree promise_proxy = get_coroutine_promise_proxy (orig);

  /* One param, the coro frame pointer.  */
  tree actor_fp = DECL_ARGUMENTS (actor);

  bool spf = start_preparsed_function (actor, NULL_TREE, SF_PRE_PARSED);
  gcc_checking_assert (spf);
  gcc_checking_assert (cfun && current_function_decl && TREE_STATIC (actor));
  tree stmt = begin_function_body ();

  tree actor_bind = build3 (BIND_EXPR, void_type_node, NULL, NULL, NULL);
  tree top_block = make_node (BLOCK);
  BIND_EXPR_BLOCK (actor_bind) = top_block;

  tree continuation = coro_build_artificial_var (loc, coro_actor_continue_id,
						 void_coro_handle_type, actor,
						 NULL_TREE);

  BIND_EXPR_VARS (actor_bind) = continuation;
  BLOCK_VARS (top_block) = BIND_EXPR_VARS (actor_bind) ;

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

  /* Re-write local vars, similarly.  */
  local_vars_transform xform_vars_data
    = {actor, actor_frame, coro_frame_type, loc, local_var_uses};
  cp_walk_tree (&fnbody, transform_local_var_uses, &xform_vars_data, NULL);
  tree rat = coro_build_frame_access_expr (actor_frame, coro_resume_index_id,
					   false, tf_warning_or_error);
  tree ret_label
    = create_named_label_with_ctx (loc, "actor.suspend.ret", actor);

  tree continue_label
    = create_named_label_with_ctx (loc, "actor.continue.ret", actor);

  /* Build the dispatcher; for each await expression there is an odd entry
     corresponding to the destruction action and an even entry for the resume
     one:
     if (resume index is odd)
	{
	  switch (resume index)
	   case 1:
	     goto cleanup.
	   case ... odd suspension point number
	     .CO_ACTOR (... odd suspension point number)
	     break;
	   default:
	     break;
	}
      else
	{
	  coro.restart.dispatch:
	   case 0:
	     goto start.
	   case ... even suspension point number
	     .CO_ACTOR (... even suspension point number)
	     break;
	   default:
	     break;
	}
    we should not get here unless something is broken badly.
     __builtin_trap ();
*/
  tree lsb_if = begin_if_stmt ();
  tree chkb0 = build2 (BIT_AND_EXPR, short_unsigned_type_node, rat,
		       build_int_cst (short_unsigned_type_node, 1));
  chkb0 = build2 (NE_EXPR, short_unsigned_type_node, chkb0,
		  build_int_cst (short_unsigned_type_node, 0));
  finish_if_stmt_cond (chkb0, lsb_if);

  tree destroy_dispatcher = begin_switch_stmt ();
  finish_switch_cond (rat, destroy_dispatcher);

  /* The destroy point numbered #1 is special, in that it is reached from a
     coroutine that is suspended after re-throwing from unhandled_exception().
     This label just invokes the cleanup of promise, param copies and the
     frame itself.  */
  tree del_promise_label
    = create_named_label_with_ctx (loc, "coro.delete.promise", actor);
  finish_case_label (loc, build_int_cst (short_unsigned_type_node, 1),
		     NULL_TREE);
  add_stmt (build_stmt (loc, GOTO_EXPR, del_promise_label));

  short unsigned lab_num = 3;
  for (unsigned destr_pt = 0; destr_pt < body_count; destr_pt++)
    {
      tree l_num = build_int_cst (short_unsigned_type_node, lab_num);
      finish_case_label (loc, l_num, NULL_TREE);
      tree c = build_call_expr_internal_loc (loc, IFN_CO_ACTOR, void_type_node,
					     1, l_num);
      finish_expr_stmt (c);
      finish_break_stmt ();
      lab_num += 2;
    }
  finish_case_label (loc, NULL_TREE, NULL_TREE);
  finish_break_stmt ();

  /* Finish the destroy dispatcher.  */
  finish_switch_stmt (destroy_dispatcher);

  finish_then_clause (lsb_if);
  begin_else_clause (lsb_if);

  /* For the case of a boolean await_resume () that returns 'true' we should
     restart the dispatch, since we cannot know if additional resumes were
     executed from within the await_suspend function.  */
  tree restart_dispatch_label
    = create_named_label_with_ctx (loc, "coro.restart.dispatch", actor);
  add_stmt (build_stmt (loc, LABEL_EXPR, restart_dispatch_label));

  tree dispatcher = begin_switch_stmt ();
  finish_switch_cond (rat, dispatcher);
  finish_case_label (loc, build_int_cst (short_unsigned_type_node, 0),
		     NULL_TREE);
  add_stmt (build_stmt (loc, GOTO_EXPR, actor_begin_label));

  lab_num = 2;
  /* The final resume should be made to hit the default (trap, UB) entry
     although it will be unreachable via the normal entry point, since that
     is set to NULL on reaching final suspend.  */
  for (unsigned resu_pt = 0; resu_pt < body_count; resu_pt++)
    {
      tree l_num = build_int_cst (short_unsigned_type_node, lab_num);
      finish_case_label (loc, l_num, NULL_TREE);
      tree c = build_call_expr_internal_loc (loc, IFN_CO_ACTOR, void_type_node,
					     1, l_num);
      finish_expr_stmt (c);
      finish_break_stmt ();
      lab_num += 2;
    }
  finish_case_label (loc, NULL_TREE, NULL_TREE);
  finish_break_stmt ();

  /* Finish the resume dispatcher.  */
  finish_switch_stmt (dispatcher);
  finish_else_clause (lsb_if);

  finish_if_stmt (lsb_if);

  /* If we reach here then we've hit UB.  */
  tree t = build_call_expr_loc (loc, builtin_decl_explicit (BUILT_IN_TRAP), 0);
  finish_expr_stmt (t);

  /* Now we start building the rewritten function body.  */
  add_stmt (build_stmt (loc, LABEL_EXPR, actor_begin_label));

  /* actor's coroutine 'self handle'.  */
  tree ash = coro_build_frame_access_expr (actor_frame, coro_self_handle_id,
					   false, tf_warning_or_error);
  /* So construct the self-handle from the frame address.  */
  tree hfa_m = get_coroutine_from_address (orig);
  /* Should have been set earlier by coro_promise_type_found_p.  */
  gcc_assert (hfa_m);

  tree r = build1 (CONVERT_EXPR, build_pointer_type (void_type_node), actor_fp);
  vec<tree, va_gc> *args = make_tree_vector_single (r);
  tree hfa = build_new_method_call (ash, hfa_m, &args, NULL_TREE, LOOKUP_NORMAL,
				    NULL, tf_warning_or_error);
  r = cp_build_init_expr (ash, hfa);
  r = coro_build_cvt_void_expr_stmt (r, loc);
  add_stmt (r);
  release_tree_vector (args);

  /* Now we know the real promise, and enough about the frame layout to
     decide where to put things.  */

  await_xform_data xform = {actor, actor_frame, suspend_points};

  /* Transform the await expressions in the function body.  Only do each
     await tree once!  */
  hash_set<tree> pset;
  cp_walk_tree (&fnbody, transform_await_wrapper, &xform, &pset);

  /* Add in our function body with the co_returns rewritten to final form.  */
  add_stmt (fnbody);

  /* now do the tail of the function.  */
  r = build_stmt (loc, LABEL_EXPR, del_promise_label);
  add_stmt (r);

  /* Destructors for the things we built explicitly.  */
  if (tree c = cxx_maybe_build_cleanup (promise_proxy, tf_warning_or_error))
    add_stmt (c);

  tree del_frame_label
    = create_named_label_with_ctx (loc, "coro.delete.frame", actor);
  r = build_stmt (loc, LABEL_EXPR, del_frame_label);
  add_stmt (r);

  /* Here deallocate the frame (if we allocated it), which we will have at
     present.  */
  tree fnf2_x
   = coro_build_frame_access_expr (actor_frame, coro_frame_needs_free_id,
				   false, tf_warning_or_error);

  tree need_free_if = begin_if_stmt ();
  fnf2_x = build1 (CONVERT_EXPR, integer_type_node, fnf2_x);
  tree cmp = build2 (NE_EXPR, integer_type_node, fnf2_x, integer_zero_node);
  finish_if_stmt_cond (cmp, need_free_if);
  while (!param_dtor_list->is_empty ())
    {
      tree parm_id = param_dtor_list->pop ();
      tree a = coro_build_frame_access_expr (actor_frame, parm_id, false,
					     tf_warning_or_error);
      if (tree dtor = cxx_maybe_build_cleanup (a, tf_warning_or_error))
	add_stmt (dtor);
    }

  /* Build the frame DTOR.  */
  tree del_coro_fr
    = build_coroutine_frame_delete_expr (actor_fp, frame_size,
					 promise_type, loc);
  finish_expr_stmt (del_coro_fr);
  finish_then_clause (need_free_if);
  tree scope = IF_SCOPE (need_free_if);
  IF_SCOPE (need_free_if) = NULL;
  r = do_poplevel (scope);
  add_stmt (r);

  /* done.  */
  r = build_stmt (loc, RETURN_EXPR, NULL);
  suppress_warning (r); /* We don't want a warning about this.  */
  r = maybe_cleanup_point_expr_void (r);
  add_stmt (r);

  /* This is the suspend return point.  */
  r = build_stmt (loc, LABEL_EXPR, ret_label);
  add_stmt (r);

  r = build_stmt (loc, RETURN_EXPR, NULL);
  suppress_warning (r); /* We don't want a warning about this.  */
  r = maybe_cleanup_point_expr_void (r);
  add_stmt (r);

  /* This is the 'continuation' return point.  For such a case we have a coro
     handle (from the await_suspend() call) and we want handle.resume() to
     execute as a tailcall allowing arbitrary chaining of coroutines.  */
  r = build_stmt (loc, LABEL_EXPR, continue_label);
  add_stmt (r);

  /* Should have been set earlier by the coro_initialized code.  */
  gcc_assert (void_coro_handle_address);

  /* We want to force a tail-call even for O0/1, so this expands the resume
     call into its underlying implementation.  */
  tree addr = build_new_method_call (continuation, void_coro_handle_address,
				     NULL, NULL_TREE, LOOKUP_NORMAL, NULL,
				     tf_warning_or_error);
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

  /* We've now rewritten the tree and added the initial and final
     co_awaits.  Now pass over the tree and expand the co_awaits.  */

  coro_aw_data data = {actor, actor_fp, resume_idx_var, NULL_TREE,
		       ash, del_promise_label, ret_label,
		       continue_label, restart_dispatch_label, continuation, 2};
  cp_walk_tree (&actor_body, await_statement_expander, &data, NULL);

  BIND_EXPR_BODY (actor_bind) = pop_stmt_list (actor_body);
  TREE_SIDE_EFFECTS (actor_bind) = true;

  cfun->coroutine_component = 1;
  finish_function_body (stmt);
  finish_function (inline_p);
}

/* The prototype 'destroy' function :
   frame->__Coro_resume_index |= 1;
   actor (frame);  */

static void
build_destroy_fn (location_t loc, tree coro_frame_type, tree destroy,
		  tree actor, bool inline_p)
{
  /* One param, the coro frame pointer.  */
  tree destr_fp = DECL_ARGUMENTS (destroy);
  gcc_checking_assert (POINTER_TYPE_P (TREE_TYPE (destr_fp))
		       && same_type_p (coro_frame_type,
				       TREE_TYPE (TREE_TYPE (destr_fp))));

  bool spf = start_preparsed_function (destroy, NULL_TREE, SF_PRE_PARSED);
  gcc_checking_assert (spf);
  tree dstr_stmt = begin_function_body ();

  tree destr_frame
    = cp_build_indirect_ref (loc, destr_fp, RO_UNARY_STAR,
			     tf_warning_or_error);

  tree rat = coro_build_frame_access_expr (destr_frame, coro_resume_index_id,
					   false, tf_warning_or_error);

  /* _resume_at |= 1 */
  tree dstr_idx
    = build2_loc (loc, BIT_IOR_EXPR, short_unsigned_type_node, rat,
		  build_int_cst (short_unsigned_type_node, 1));
  tree r = cp_build_modify_expr (loc, rat, NOP_EXPR, dstr_idx,
				 tf_warning_or_error);
  finish_expr_stmt (r);

  /* So .. call the actor ..  */
  finish_expr_stmt (build_call_expr_loc (loc, actor, 1, destr_fp));

  /* done. */
  finish_return_stmt (NULL_TREE);

  gcc_checking_assert (cfun && current_function_decl);
  cfun->coroutine_component = 1;
  finish_function_body (dstr_stmt);
  finish_function (inline_p);
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
register_await_info (tree await_expr, tree aw_type, tree aw_nam,
		     hash_map<tree, suspend_point_info> *suspend_points)
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
  register_await_info (aw_expr, aw_field_type, aw_field_nam, data->suspend_points);

  /* Rewrite target expressions on the await_suspend () to remove extraneous
     cleanups for the awaitables, which are now promoted to frame vars and
     managed via that.  */
  tree v = TREE_OPERAND (aw_expr, 3);
  tree o = TREE_VEC_ELT (v, 1);
  if (TREE_CODE (o) == TARGET_EXPR)
    TREE_VEC_ELT (v, 1) = get_target_expr (TARGET_EXPR_INITIAL (o));
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
  tree v = TARGET_EXPR_SLOT (t);
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
	   && !TARGET_EXPR_ELIDING_P (expr)
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
  var_nest_node () = default;
  var_nest_node (tree v, tree i, var_nest_node *p, var_nest_node *n)
    : var(v), init(i), prev(p), next(n), then_cl (NULL), else_cl (NULL)
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
	      if (init_expr)
		then_cl = cp_build_init_expr (var, then_cl);
	      else
		then_cl = build2 (MODIFY_EXPR, var_type, var, then_cl);
	    }
	  tree else_cl = COND_EXPR_ELSE (old_expr);
	  if (!VOID_TYPE_P (TREE_TYPE (else_cl)))
	    {
	      gcc_checking_assert (TREE_CODE (else_cl) != STATEMENT_LIST);
	      if (init_expr)
		else_cl = cp_build_init_expr (var, else_cl);
	      else
		else_cl = build2 (MODIFY_EXPR, var_type, var, else_cl);
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
	      cond = cp_build_init_expr (cond_var, cond);
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
			(TREE_CODE (TARGET_EXPR_INITIAL (inner)) != COND_EXPR);
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
	  char *buf = xasprintf ("T%03u", (unsigned) temps_used->elements ());
	  tree var = build_lang_decl (VAR_DECL, get_identifier (buf), var_type);
	  DECL_ARTIFICIAL (var) = true;
	  free (buf);
	  bool already_present = promoted->add (var);
	  gcc_checking_assert (!already_present);
	  tree inner = TARGET_EXPR_INITIAL (init);
	  gcc_checking_assert (TREE_CODE (inner) != COND_EXPR);
	  init = cp_build_modify_expr (input_location, var, INIT_EXPR, init,
				       tf_warning_or_error);
	  /* Simplify for the case that we have an init containing the temp
	     alone.  */
	  if (t == n->init && n->var == NULL_TREE)
	    {
	      n->var = var;
	      proxy_replace pr = {TARGET_EXPR_SLOT (t), var};
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
	      proxy_replace pr = {TARGET_EXPR_SLOT (t), var};
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
	      tree set_flag = cp_build_init_expr (flag, boolean_true_node);
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
  auto_vec<tree> var_list;
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
      if (tree cleanup = cxx_maybe_build_cleanup (var, tf_warning_or_error))
	{
	  tree cond_cleanup = begin_if_stmt ();
	  finish_if_stmt_cond (*flag, cond_cleanup);
	  finish_expr_stmt (cleanup);
	  finish_then_clause (cond_cleanup);
	  finish_if_stmt (cond_cleanup);
	}
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
	 variables must be run conditionally too.

	 Since here we're synthetically processing code here, we've already
	 emitted any Wunused-result warnings.  Below, however, we call
	 finish_expr_stmt, which will convert its operand to void, and could
	 result in such a diagnostic being emitted.  To avoid that, convert to
	 void ahead of time.  */
      if (t->var)
	{
	  tree var = t->var;
	  DECL_CHAIN (var) = vlist;
	  vlist = var;
	  add_decl_expr (var);
	  if (TREE_CODE (t->init) == COND_EXPR)
	    process_conditional (t, vlist);
	  else
	    finish_expr_stmt (convert_to_void (t->init, ICV_STATEMENT, tf_none));
	  if (tree cleanup = cxx_maybe_build_cleanup (var, tf_warning_or_error))
	    {
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
	    finish_expr_stmt (convert_to_void (t->init, ICV_STATEMENT, tf_none));
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
  char *nam = xasprintf ("__%s_%d", nam_root, nam_vers);
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
  if (CONVERT_EXPR_P (expr) && VOID_TYPE_P (TREE_TYPE (expr)))
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
      for (tree &s : tsi_range (*stmt))
	{
	  res = cp_walk_tree (&s, await_statement_walker,
			      d, NULL);
	  if (res)
	    return res;
	}
      *do_subtree = 0; /* Done subtrees.  */
      return NULL_TREE;
    }

  /* We have something to be handled as a single statement.  We have to handle
     a few statements specially where await statements have to be moved out of
     constructs.  */
  tree expr = *stmt;
  if (TREE_CODE (*stmt) == CLEANUP_POINT_EXPR)
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
	    tree *await_ptr;
	    hash_set<tree> visited;
	    /* Transform 'if (cond with awaits) then stmt1 else stmt2' into
	       bool cond = cond with awaits.
	       if (cond) then stmt1 else stmt2.  */
	    tree if_stmt = *stmt;
	    /* We treat the condition as if it was a stand-alone statement,
	       to see if there are any await expressions which will be analyzed
	       and registered.  */
	    if (!(cp_walk_tree (&IF_COND (if_stmt),
		  find_any_await, &await_ptr, &visited)))
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
	    tree new_s = cp_build_init_expr (sloc, newvar, cond_inner);
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
	    tree *await_ptr;
	    hash_set<tree> visited;
	    /* for loops only need special treatment if the condition or the
	       iteration expression contain a co_await.  */
	    tree for_stmt = *stmt;
	    /* At present, the FE always generates a separate initializer for
	       the FOR_INIT_STMT, when the expression has an await.  Check that
	       this assumption holds in the future. */
	    gcc_checking_assert
	      (!(cp_walk_tree (&FOR_INIT_STMT (for_stmt), find_any_await,
			       &await_ptr, &visited)));

	    visited.empty ();
	    bool for_cond_await
	      = cp_walk_tree (&FOR_COND (for_stmt), find_any_await,
			      &await_ptr, &visited);

	    visited.empty ();
	    bool for_expr_await
	      = cp_walk_tree (&FOR_EXPR (for_stmt), find_any_await,
			      &await_ptr, &visited);

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
		tree for_expr = FOR_EXPR (for_stmt);
		/* Present the iteration expression as a statement.  */
		if (TREE_CODE (for_expr) == CLEANUP_POINT_EXPR)
		  for_expr = TREE_OPERAND (for_expr, 0);
		STRIP_NOPS (for_expr);
		finish_expr_stmt (for_expr);
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
	    tree *await_ptr;
	    hash_set<tree> visited;
	    tree while_stmt = *stmt;
	    if (!(cp_walk_tree (&WHILE_COND (while_stmt),
		  find_any_await, &await_ptr, &visited)))
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
	    tree *await_ptr;
	    hash_set<tree> visited;
	    if (!(cp_walk_tree (&DO_COND (do_stmt),
		  find_any_await, &await_ptr, &visited)))
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
	    tree *await_ptr;
	    hash_set<tree> visited;
	    if (!(cp_walk_tree (&SWITCH_STMT_COND (sw_stmt),
		  find_any_await, &await_ptr, &visited)))
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
	    tree new_s = cp_build_init_expr (sloc, newvar,
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
	    location_t loc = EXPR_LOCATION (expr);
	    tree call = TREE_OPERAND (expr, 1);
	    expr = TREE_OPERAND (expr, 0);
	    tree ret_list = push_stmt_list ();
	    /* [stmt.return.coroutine], 2.2
	       If expr is present and void, it is placed immediately before
	       the call for return_void;  */
	    if (expr && VOID_TYPE_P (TREE_TYPE (expr)))
	      finish_expr_stmt (expr);
	    /* Insert p.return_{void,value(expr)}.  */
	    finish_expr_stmt (call);
	    TREE_USED (awpts->fs_label) = 1;
	    add_stmt (build_stmt (loc, GOTO_EXPR, awpts->fs_label));
	    *stmt = pop_stmt_list (ret_list);
	    res = cp_walk_tree (stmt, await_statement_walker, d, NULL);
	    /* Once this is complete, we will have processed subtrees.  */
	    *do_subtree = 0;
	    return res;
	  }
	  break;
	case HANDLER:
	  {
	    /* [expr.await] An await-expression shall appear only in a
	       potentially-evaluated expression within the compound-statement
	       of a function-body outside of a handler.  */
	    tree *await_ptr;
	    hash_set<tree> visited;
	    if (!(cp_walk_tree (&HANDLER_BODY (expr), find_any_await,
		  &await_ptr, &visited)))
	      return NULL_TREE; /* All OK.  */
	    location_t loc = EXPR_LOCATION (*await_ptr);
	    error_at (loc, "await expressions are not permitted in handlers");
	    return NULL_TREE; /* This is going to fail later anyway.  */
	  }
	  break;
      }
  else if (EXPR_P (expr))
    {
      hash_set<tree> visited;
      tree *await_ptr;
      if (!(cp_walk_tree (stmt, find_any_await, &await_ptr, &visited)))
	return NULL_TREE; /* Nothing special to do here.  */

      visited.empty ();
      awpts->saw_awaits = 0;
      hash_set<tree> truth_aoif_to_expand;
      awpts->truth_aoif_to_expand = &truth_aoif_to_expand;
      awpts->needs_truth_if_exp = false;
      awpts->has_awaiter_init = false;
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

/* A tree walk callback that rewrites each parm use to the local variable
   that represents its copy in the frame.  */

static tree
rewrite_param_uses (tree *stmt, int *do_subtree ATTRIBUTE_UNUSED, void *d)
{
  param_frame_data *data = (param_frame_data *) d;

  /* For lambda closure content, we have to look specifically.  */
  if (VAR_P (*stmt) && DECL_HAS_VALUE_EXPR_P (*stmt))
    {
      tree t = DECL_VALUE_EXPR (*stmt);
      return cp_walk_tree (&t, rewrite_param_uses, d, NULL);
    }

  if (unevaluated_p (TREE_CODE (*stmt)))
    {
      /* No odr-uses in unevaluated operands.  */
      *do_subtree = 0;
      return NULL_TREE;
    }

  if (TREE_CODE (*stmt) != PARM_DECL)
    return NULL_TREE;

  /* If we already saw the containing expression, then we're done.  */
  if (data->visited->add (stmt))
    return NULL_TREE;

  bool existed;
  param_info &parm = data->param_uses->get_or_insert (*stmt, &existed);
  gcc_checking_assert (existed);

  *stmt = parm.copy_var;
  return NULL_TREE;
}

/* Build up a set of info that determines how each param copy will be
   handled.  */

static void
analyze_fn_parms (tree orig, hash_map<tree, param_info> *param_uses)
{
  if (!DECL_ARGUMENTS (orig))
    return;

  /* Build a hash map with an entry for each param.
     The key is the param tree.
     Then we have an entry for the frame field name.
     Then a cache for the field ref when we come to use it.
     Then a tree list of the uses.
     The second two entries start out empty - and only get populated
     when we see uses.  */
  bool lambda_p = LAMBDA_FUNCTION_P (orig);

  /* Count the param copies from 1 as per the std.  */
  unsigned parm_num = 1;
  for (tree arg = DECL_ARGUMENTS (orig); arg != NULL;
       ++parm_num, arg = DECL_CHAIN (arg))
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
      parm.lambda_cobj = lambda_p && DECL_NAME (arg) == closure_identifier;

      tree name = DECL_NAME (arg);
      if (!name)
	{
	  char *buf = xasprintf ("_Coro_q%u___unnamed", parm_num);
	  name = get_identifier (buf);
	  free (buf);
	}
      parm.field_id = name;
      if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (parm.frame_type))
	{
	  char *buf = xasprintf ("_Coro_q%u_%s_live", parm_num,
				 DECL_NAME (arg) ? IDENTIFIER_POINTER (name)
						 : "__unnamed");
	  parm.guard_var
	    = coro_build_artificial_var (UNKNOWN_LOCATION, get_identifier (buf),
					 boolean_type_node, orig,
					 boolean_false_node);
	  free (buf);
	  parm.trivial_dtor = false;
	}
      else
	parm.trivial_dtor = true;
    }
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

/* A tree-walk callback that processes one bind expression noting local
   variables, and making a coroutine frame slot available for those that
   need it, so that they can be 'promoted' across suspension points.  */

static tree
register_local_var_uses (tree *stmt, int *do_subtree, void *d)
{
  if (TREE_CODE (*stmt) != BIND_EXPR)
    return NULL_TREE;

  local_vars_frame_data *lvd = (local_vars_frame_data *) d;

  /* As we enter a bind expression - record the vars there and then recurse.
     As we exit drop the nest depth.
     The bind index is a growing count of how many bind indices we've seen.
     We build a space in the frame for each local var.  */

  tree lvar;
  unsigned serial = 0;
  for (lvar = BIND_EXPR_VARS (*stmt); lvar != NULL; lvar = DECL_CHAIN (lvar))
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
      if (TREE_CODE (lvar) != PARM_DECL
	  && TREE_CODE (lvar) != VAR_DECL)
	continue;

      /* We don't move static vars into the frame. */
      local_var.is_static = TREE_STATIC (lvar);
      if (local_var.is_static)
	continue;

      poly_uint64 size;
      if (TREE_CODE (lvtype) == ARRAY_TYPE
	  && !poly_int_tree_p (DECL_SIZE_UNIT (lvar), &size))
	{
	  sorry_at (local_var.def_loc, "variable length arrays are not"
		    " yet supported in coroutines");
	  /* Ignore it, this is broken anyway.  */
	  continue;
	}

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
	 scopes with identically named locals and still be able to
	 identify them in the coroutine frame.  */
      tree lvname = DECL_NAME (lvar);
      char *buf = NULL;

      /* The outermost bind scope contains the artificial variables that
	 we inject to implement the coro state machine.  We want to be able
	 to inspect these in debugging.  */
      if (lvname != NULL_TREE && lvd->nest_depth == 0)
	buf = xasprintf ("%s", IDENTIFIER_POINTER (lvname));
      else if (lvname != NULL_TREE)
	buf = xasprintf ("%s_%u_%u", IDENTIFIER_POINTER (lvname),
			 lvd->nest_depth, lvd->bind_indx);
      else
	buf = xasprintf ("_D%u_%u_%u", lvd->nest_depth, lvd->bind_indx,
			 serial++);

      /* TODO: Figure out if we should build a local type that has any
	 excess alignment or size from the original decl.  */
      local_var.field_id = coro_make_frame_entry (lvd->field_list, buf,
						  lvtype, lvd->loc);
      free (buf);
      /* We don't walk any of the local var sub-trees, they won't contain
	 any bind exprs.  */
    }
  lvd->bind_indx++;
  lvd->nest_depth++;
  /* Ensure we only visit each expression once.  */
  cp_walk_tree_without_duplicates (&BIND_EXPR_BODY (*stmt),
				   register_local_var_uses, d);
  *do_subtree = 0; /* We've done this.  */
  lvd->nest_depth--;
  return NULL_TREE;
}

/* Build, return FUNCTION_DECL node based on ORIG with a type FN_TYPE which has
   a single argument of type CORO_FRAME_PTR.  Build the actor function if
   ACTOR_P is true, otherwise the destroy. */

static tree
coro_build_actor_or_destroy_function (tree orig, tree fn_type,
				      tree coro_frame_ptr, bool actor_p)
{
  location_t loc = DECL_SOURCE_LOCATION (orig);
  tree fn
    = build_lang_decl (FUNCTION_DECL, copy_node (DECL_NAME (orig)), fn_type);

  /* Allow for locating the ramp (original) function from this one.  */
  if (!to_ramp)
    to_ramp = hash_map<tree, tree>::create_ggc (10);
  to_ramp->put (fn, orig);

  DECL_CONTEXT (fn) = DECL_CONTEXT (orig);
  DECL_SOURCE_LOCATION (fn) = loc;
  DECL_ARTIFICIAL (fn) = true;
  DECL_INITIAL (fn) = error_mark_node;

  tree id = get_identifier ("frame_ptr");
  tree fp = build_lang_decl (PARM_DECL, id, coro_frame_ptr);
  DECL_ARTIFICIAL (fp) = true;
  DECL_CONTEXT (fp) = fn;
  DECL_ARG_TYPE (fp) = type_passed_as (coro_frame_ptr);
  DECL_ARGUMENTS (fn) = fp;

  /* Copy selected attributes from the original function.  */
  TREE_USED (fn) = TREE_USED (orig);
  if (DECL_SECTION_NAME (orig))
    set_decl_section_name (fn, orig);
  /* Copy any alignment that the FE added.  */
  if (DECL_ALIGN (orig))
    SET_DECL_ALIGN (fn, DECL_ALIGN (orig));
  /* Copy any alignment the user added.  */
  DECL_USER_ALIGN (fn) = DECL_USER_ALIGN (orig);
  /* Apply attributes from the original fn.  */
  DECL_ATTRIBUTES (fn) = copy_list (DECL_ATTRIBUTES (orig));
  /* but we do not want ones for contracts.  */
  remove_contract_attributes (fn);

  /* A void return.  */
  tree resdecl = build_decl (loc, RESULT_DECL, 0, void_type_node);
  DECL_CONTEXT (resdecl) = fn;
  DECL_ARTIFICIAL (resdecl) = 1;
  DECL_IGNORED_P (resdecl) = 1;
  DECL_RESULT (fn) = resdecl;

  /* Set up a means to find out if a decl is one of the helpers and, if so,
     which one.  */
  if (coroutine_info *info = get_coroutine_info (orig))
    {
      gcc_checking_assert ((actor_p && info->actor_decl == NULL_TREE)
			   || info->destroy_decl == NULL_TREE);
      if (actor_p)
	info->actor_decl = fn;
      else
	info->destroy_decl = fn;
    }
  return fn;
}

/* Re-write the body as per [dcl.fct.def.coroutine] / 5.  */

void
cp_coroutine_transform::wrap_original_function_body ()
{
  /* Avoid the code here attaching a location that makes the debugger jump.  */
  iloc_sentinel stable_input_loc (fn_start);
  location_t loc = UNKNOWN_LOCATION;
  input_location = loc;

  /* This will be our new outer scope.  */
  tree update_body
    = build3_loc (loc, BIND_EXPR, void_type_node, NULL, NULL, NULL);
  tree top_block = make_node (BLOCK);
  BIND_EXPR_BLOCK (update_body) = top_block;
  BIND_EXPR_BODY (update_body) = push_stmt_list ();

  /* If the function has a top level bind expression, then connect that
     after first making sure we give it a new block.  */
  tree first = expr_first (coroutine_body);
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
      BLOCK_SUPERCONTEXT (replace_blk) = top_block;
      BLOCK_SUBBLOCKS (top_block) = replace_blk;
    }
  else
    {
      /* We are missing a top level BIND_EXPR. We need one to ensure that we
	 don't shuffle around the coroutine frame and corrupt it.  */
      tree bind_wrap = build3_loc (loc, BIND_EXPR, void_type_node,
				   NULL, NULL, NULL);
      BIND_EXPR_BODY (bind_wrap) = coroutine_body;
      /* Ensure we have a block to connect up the scopes.  */
      tree new_blk = make_node (BLOCK);
      BIND_EXPR_BLOCK (bind_wrap) = new_blk;
      BLOCK_SUBBLOCKS (top_block) = new_blk;
      coroutine_body = bind_wrap;
    }

  /* Wrap the function body in a try {} catch (...) {} block, if exceptions
     are enabled.  */
  tree var_list = NULL_TREE;
  tree initial_await = build_init_or_final_await (fn_start, false);

  /* [stmt.return.coroutine] / 3
     If p.return_void() is a valid expression, flowing off the end of a
     coroutine is equivalent to a co_return with no operand; otherwise
     flowing off the end of a coroutine results in undefined behavior.  */
  tree return_void
    = get_coroutine_return_void_expr (orig_fn_decl, loc, false);

  /* The pointer to the resume function.  */
  tree resume_fn_ptr
    = coro_build_artificial_var (loc, coro_resume_fn_id,
				 act_des_fn_ptr_type, orig_fn_decl, NULL_TREE);
  DECL_CHAIN (resume_fn_ptr) = var_list;
  var_list = resume_fn_ptr;
  add_decl_expr (resume_fn_ptr);

  /* We will need to be able to set the resume function pointer to nullptr
     to signal that the coroutine is 'done'.  */
  tree zero_resume
    = build1 (CONVERT_EXPR, act_des_fn_ptr_type, nullptr_node);

  /* The pointer to the destroy function.  */
  tree var
    = coro_build_artificial_var (loc, coro_destroy_fn_id,
				 act_des_fn_ptr_type, orig_fn_decl, NULL_TREE);
  DECL_CHAIN (var) = var_list;
  var_list = var;
  add_decl_expr (var);

  /* The promise was created on demand when parsing we now link it into
      our scope.  */
  tree promise = get_coroutine_promise_proxy (orig_fn_decl);
  DECL_CONTEXT (promise) = orig_fn_decl;
  DECL_SOURCE_LOCATION (promise) = loc;
  DECL_CHAIN (promise) = var_list;
  var_list = promise;
  add_decl_expr (promise);

  /* We need a handle to this coroutine, which is passed to every
     await_suspend().  This was created on demand when parsing we now link it
     into our scope.  */
  var = get_coroutine_self_handle_proxy (orig_fn_decl);
  DECL_CONTEXT (var) = orig_fn_decl;
  DECL_SOURCE_LOCATION (var) = loc;
  DECL_CHAIN (var) = var_list;
  var_list = var;
  add_decl_expr (var);

  /* If we have function parms, then these will be copied to the coroutine
     frame as per [dcl.fct.def.coroutine] / 13.
     Here, we create a local (proxy) variable for each parm, since the original
     parms will be out of scope once the ramp has finished.  The proxy vars will
     get DECL_VALUE_EXPRs pointing to the frame copies, so that we can interact
     with them in the debugger.  */
  if (DECL_ARGUMENTS (orig_fn_decl))
    {
      /* Add a local var for each parm.  */
      for (tree arg = DECL_ARGUMENTS (orig_fn_decl); arg != NULL;
	   arg = DECL_CHAIN (arg))
	{
	  param_info *parm_i = param_uses.get (arg);
	  gcc_checking_assert (parm_i);
	  parm_i->copy_var
	    = build_lang_decl (VAR_DECL, parm_i->field_id, TREE_TYPE (arg));
	  DECL_SOURCE_LOCATION (parm_i->copy_var) = DECL_SOURCE_LOCATION (arg);
	  DECL_CONTEXT (parm_i->copy_var) = orig_fn_decl;
	  DECL_ARTIFICIAL (parm_i->copy_var) = true;
	  DECL_CHAIN (parm_i->copy_var) = var_list;
	  var_list = parm_i->copy_var;
	  add_decl_expr (parm_i->copy_var);
	}

      /* Now replace all uses of the parms in the function body with the proxy
	 vars.  We want to this to apply to every instance of param's use, so
	 don't include a 'visited' hash_set on the tree walk, however we will
	 arrange to visit each containing expression only once.  */
      hash_set<tree *> visited;
      param_frame_data param_data = {NULL, &param_uses,
				     &visited, loc, false};
      cp_walk_tree (&coroutine_body, rewrite_param_uses, &param_data, NULL);
    }

  /* We create a resume index, this is initialized in the ramp.  */
  resume_idx_var
    = coro_build_artificial_var (loc, coro_resume_index_id,
				 short_unsigned_type_node, orig_fn_decl,
				 NULL_TREE);
  DECL_CHAIN (resume_idx_var) = var_list;
  var_list = resume_idx_var;
  add_decl_expr (resume_idx_var);

  /* If the coroutine has a frame that needs to be freed, this will be set by
     the ramp.  */
  var = coro_build_artificial_var (fn_start, coro_frame_needs_free_id,
				   boolean_type_node, orig_fn_decl, NULL_TREE);
  DECL_CHAIN (var) = var_list;
  var_list = var;
  add_decl_expr (var);

  if (flag_exceptions)
    {
      /* Build promise.unhandled_exception();  */
      tree ueh
	= coro_build_promise_expression (orig_fn_decl, promise,
					 coro_unhandled_exception_identifier,
					 fn_start, NULL, /*musthave=*/true);
      /* Create and initialize the initial-await-resume-called variable per
	 [dcl.fct.def.coroutine] / 5.3.  */
      tree i_a_r_c
	= coro_build_artificial_var (loc, coro_frame_i_a_r_c_id,
				     boolean_type_node, orig_fn_decl,
				     boolean_false_node);
      DECL_CHAIN (i_a_r_c) = var_list;
      var_list = i_a_r_c;
      add_decl_expr (i_a_r_c);
      /* Start the try-catch.  */
      tree tcb = build_stmt (loc, TRY_BLOCK, NULL_TREE, NULL_TREE);
      add_stmt (tcb);
      TRY_STMTS (tcb) = push_stmt_list ();
      if (initial_await != error_mark_node)
	{
	  /* Build a compound expression that sets the
	     initial-await-resume-called variable true and then calls the
	     initial suspend expression await resume.
	     In the case that the user decides to make the initial await
	     await_resume() return a value, we need to discard it and, it is
	     a reference type, look past the indirection.  */
	  if (INDIRECT_REF_P (initial_await))
	    initial_await = TREE_OPERAND (initial_await, 0);
	  /* In the case that the initial_await returns a target expression
	     we might need to look through that to update the await expr.  */
	  tree iaw = initial_await;
	  if (TREE_CODE (iaw) == TARGET_EXPR)
	    iaw = TARGET_EXPR_INITIAL (iaw);
	  gcc_checking_assert (TREE_CODE (iaw) == CO_AWAIT_EXPR);
	  tree vec = TREE_OPERAND (iaw, 3);
	  tree aw_r = TREE_VEC_ELT (vec, 2);
	  aw_r = convert_to_void (aw_r, ICV_STATEMENT, tf_warning_or_error);
	  tree update = build2 (MODIFY_EXPR, boolean_type_node, i_a_r_c,
				boolean_true_node);
	  aw_r = cp_build_compound_expr (update, aw_r, tf_warning_or_error);
	  TREE_VEC_ELT (vec, 2) = aw_r;
	}
      /* Add the initial await to the start of the user-authored function.  */
      finish_expr_stmt (initial_await);
      /* Append the original function body.  */
      add_stmt (coroutine_body);
      if (return_void)
	add_stmt (return_void);
      TRY_STMTS (tcb) = pop_stmt_list (TRY_STMTS (tcb));
      TRY_HANDLERS (tcb) = push_stmt_list ();
      /* Mimic what the parser does for the catch.  */
      tree handler = begin_handler ();
      finish_handler_parms (NULL_TREE, handler); /* catch (...) */

      /* Get the initial await resume called value.  */
      tree not_iarc_if = begin_if_stmt ();
      tree not_iarc = build1_loc (loc, TRUTH_NOT_EXPR,
				  boolean_type_node, i_a_r_c);
      finish_if_stmt_cond (not_iarc, not_iarc_if);
      /* If the initial await resume called value is false, rethrow...  */
      tree rethrow = build_throw (loc, NULL_TREE, tf_warning_or_error);
      suppress_warning (rethrow);
      finish_expr_stmt (rethrow);
      finish_then_clause (not_iarc_if);
      finish_if_stmt (not_iarc_if);
      /* ... else call the promise unhandled exception method
	 but first we set done = true and the resume index to 0.
	 If the unhandled exception method returns, then we continue
	 to the final await expression (which duplicates the clearing of
	 the field). */
      tree r = build2_loc (loc, MODIFY_EXPR, act_des_fn_ptr_type, resume_fn_ptr,
			   zero_resume);
      finish_expr_stmt (r);
      tree short_zero = build_int_cst (short_unsigned_type_node, 0);
      r = build2 (MODIFY_EXPR, short_unsigned_type_node, resume_idx_var,
		  short_zero);
      finish_expr_stmt (r);
      finish_expr_stmt (ueh);
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
	    = lookup_promise_method (orig_fn_decl,
				     coro_unhandled_exception_identifier,
				     fn_start, /*musthave=*/false);
	  if (!ueh_meth || ueh_meth == error_mark_node)
	    warning_at (fn_start, 0, "no member named %qE in %qT",
			coro_unhandled_exception_identifier,
			get_coroutine_promise_type (orig_fn_decl));
	}
      /* Else we don't check and don't care if the method is missing..
	 just add the initial suspend, function and return.  */
      finish_expr_stmt (initial_await);
      /* Append the original function body.  */
      add_stmt (coroutine_body);
      if (return_void)
	add_stmt (return_void);
    }

  /* co_return branches to the final_suspend label, so declare that now.  */
  fs_label
    = create_named_label_with_ctx (loc, "final.suspend", NULL_TREE);
  add_stmt (build_stmt (loc, LABEL_EXPR, fs_label));

  /* Before entering the final suspend point, we signal that this point has
     been reached by setting the resume function pointer to zero (this is
     what the 'done()' builtin tests) as per the current ABI.  */
  zero_resume = build2_loc (loc, MODIFY_EXPR, act_des_fn_ptr_type,
			    resume_fn_ptr, zero_resume);
  finish_expr_stmt (zero_resume);
  finish_expr_stmt (build_init_or_final_await (fn_start, true));
  BIND_EXPR_BODY (update_body) = pop_stmt_list (BIND_EXPR_BODY (update_body));
  BIND_EXPR_VARS (update_body) = nreverse (var_list);
  BLOCK_VARS (top_block) = BIND_EXPR_VARS (update_body);

  coroutine_body = update_body;
}

/* Extract the body of the function we are going to outline, leaving
   to original function decl ready to build the ramp.  */

static tree
split_coroutine_body_from_ramp (tree fndecl)
{
  /* Sanity-check and punt if we have a nonsense tree because of earlier
     parse errors, perhaps.  */
  if (!current_binding_level
      || current_binding_level->kind != sk_function_parms)
    return NULL_TREE;

  /* Once we've tied off the original user-authored body in fn_body.
     Start the replacement synthesized ramp body.  */

  tree body;
  if (use_eh_spec_block (fndecl))
    {
      body = pop_stmt_list (TREE_OPERAND (current_eh_spec_block, 0));
      TREE_OPERAND (current_eh_spec_block, 0) = push_stmt_list ();
    }
  else
    {
      body = pop_stmt_list (DECL_SAVED_TREE (fndecl));
      DECL_SAVED_TREE (fndecl) = push_stmt_list ();
    }

  /* We can't validly get here with an empty statement list, since there's no
     way for the FE to decide it's a coroutine in the absence of any code.  */
  gcc_checking_assert (body != NULL_TREE);

  /* If we have an empty or erroneous function body, do not try to transform it
     since that would potentially wrap errors.  */
  tree body_start = expr_first (body);
  if (body_start == NULL_TREE || body_start == error_mark_node)
    {
      /* Restore the original state.  */
      add_stmt (body);
      return NULL_TREE;
    }
  return body;
}

/* Build the expression to allocate the coroutine frame according to the
   rules of [dcl.fct.def.coroutine] / 9.  */

static tree
build_coroutine_frame_alloc_expr (tree promise_type, tree orig_fn_decl,
				  location_t fn_start, tree grooaf,
				  hash_map<tree, param_info> *param_uses,
				  tree frame_size)
{
  /* Allocate the frame, this has several possibilities:
     [dcl.fct.def.coroutine] / 9 (part 1)
     The allocation function’s name is looked up in the scope of the promise
     type.  It is not a failure for it to be absent see part 4, below.  */

  tree nwname = ovl_op_identifier (false, NEW_EXPR);
  tree new_fn_call = NULL_TREE;
  tree dummy_promise
    = build_dummy_object (get_coroutine_promise_type (orig_fn_decl));

  if (TYPE_HAS_NEW_OPERATOR (promise_type))
    {
      tree fns = lookup_promise_method (orig_fn_decl, nwname, fn_start,
					/*musthave=*/true);
      /* [dcl.fct.def.coroutine] / 9 (part 2)
	If the lookup finds an allocation function in the scope of the promise
	type, overload resolution is performed on a function call created by
	assembling an argument list.  The first argument is the amount of space
	requested, and has type std::size_t.  The lvalues p1...pn are the
	succeeding arguments..  */
      vec<tree, va_gc> *args = make_tree_vector ();
      vec_safe_push (args, frame_size); /* Space needed.  */

      for (tree arg = DECL_ARGUMENTS (orig_fn_decl); arg != NULL;
	   arg = DECL_CHAIN (arg))
	{
	  param_info *parm_i = param_uses->get (arg);
	  gcc_checking_assert (parm_i);
	  if (parm_i->this_ptr || parm_i->lambda_cobj)
	    {
	      /* We pass a reference to *this to the allocator lookup.  */
	      /* It's unsafe to use the cp_ version here since current_class_ref
		 might've gotten clobbered earlier during rewrite_param_uses.  */
	      tree this_ref = build_fold_indirect_ref (arg);
	      vec_safe_push (args, this_ref);
	    }
	  else
	    vec_safe_push (args, convert_from_reference (arg));
	}

      /* Note the function selected; we test to see if it's NOTHROW.  */
      tree func;
      /* Failure is not an error for this attempt.  */
      new_fn_call = build_new_method_call (dummy_promise, fns, &args, NULL,
				      LOOKUP_NORMAL, &func, tf_none);
      release_tree_vector (args);

      if (new_fn_call == error_mark_node)
	{
	  /* [dcl.fct.def.coroutine] / 9 (part 3)
	    If no viable function is found, overload resolution is performed
	    again on a function call created by passing just the amount of
	    space required as an argument of type std::size_t.  */
	  args = make_tree_vector_single (frame_size); /* Space needed.  */
	  new_fn_call = build_new_method_call (dummy_promise, fns, &args,
					  NULL_TREE, LOOKUP_NORMAL, &func,
					  tf_none);
	  release_tree_vector (args);
	}

     /* However, if the promise provides an operator new, then one of these
	two options must be available.  */
    if (new_fn_call == error_mark_node)
      {
	error_at (fn_start, "%qE is provided by %qT but is not usable with"
		  " the function signature %qD", nwname, promise_type,
		  orig_fn_decl);
	return error_mark_node;
      }
    else if (grooaf && !TYPE_NOTHROW_P (TREE_TYPE (func)))
      {
	error_at (fn_start, "%qE is provided by %qT but %qE is not marked"
		" %<throw()%> or %<noexcept%>", grooaf, promise_type, nwname);
	return error_mark_node;
      }
    else if (!grooaf && TYPE_NOTHROW_P (TREE_TYPE (func)))
      warning_at (fn_start, 0, "%qE is marked %<throw()%> or %<noexcept%> but"
		  " no usable %<get_return_object_on_allocation_failure%>"
		  " is provided by %qT", nwname, promise_type);
    }
  else /* No operator new in the promise.  */
    {
      /* [dcl.fct.def.coroutine] / 9 (part 4)
	 If this lookup fails, the allocation function’s name is looked up in
	 the global scope.  */

      vec<tree, va_gc> *args;
      /* build_operator_new_call () will insert size needed as element 0 of
	 this, and we might need to append the std::nothrow constant.  */
      vec_alloc (args, 2);
      if (grooaf)
	{
	  /* [dcl.fct.def.coroutine] / 10 (part 2)
	   If any declarations (of the get return on allocation fail) are
	   found, then the result of a call to an allocation function used
	   to obtain storage for the coroutine state is assumed to return
	   nullptr if it fails to obtain storage and, if a global allocation
	   function is selected, the ::operator new(size_t, nothrow_t) form
	   is used.  The allocation function used in this case shall have a
	   non-throwing noexcept-specification.  So we need std::nothrow.  */
	  tree std_nt = lookup_qualified_name (std_node,
					       get_identifier ("nothrow"),
					       LOOK_want::NORMAL,
					       /*complain=*/true);
	  if (!std_nt || std_nt == error_mark_node)
	    {
	      /* Something is seriously wrong, punt.  */
	      error_at (fn_start, "%qE is provided by %qT but %<std::nothrow%>"
			" cannot be found", grooaf, promise_type);
	      return error_mark_node;
	    }
	  vec_safe_push (args, std_nt);
	}

      /* If we get to this point, we must succeed in looking up the global
	 operator new for the params provided.  Since we are not setting
	 size_check or cookie, we expect frame_size to be unaltered.  */
      tree cookie = NULL;
      new_fn_call = build_operator_new_call (nwname, &args, &frame_size,
					     &cookie, /*align_arg=*/NULL,
					     /*size_check=*/NULL, /*fn=*/NULL,
					     tf_warning_or_error);
      release_tree_vector (args);
    }
  return new_fn_call;
}

/* Build an expression to delete the coroutine state frame.  */

static tree
build_coroutine_frame_delete_expr (tree coro_fp, tree frame_size,
				   tree promise_type, location_t loc)
{
  /* Cast the frame pointer to a pointer to promise so that the build op
     delete call will search the promise.  */
  tree pptr_type = build_pointer_type (promise_type);
  tree frame_arg = build1_loc (loc, CONVERT_EXPR, pptr_type, coro_fp);
  /* [dcl.fct.def.coroutine] / 12 sentence 3:
     If both a usual deallocation function with only a pointer parameter and
     a usual deallocation function with both a pointer parameter and a size
     parameter are found, then the selected deallocation function shall be the
     one with two parameters.  */
  tree del_coro_fr
    = build_coroutine_op_delete_call (DELETE_EXPR, frame_arg, frame_size,
				      /*global_p=*/false,  /*placement=*/NULL,
				      /*alloc_fn=*/NULL, tf_warning_or_error);
  if (!del_coro_fr || del_coro_fr == error_mark_node)
    return error_mark_node;
  return del_coro_fr;
}

/* Build the ramp function.
   Here we take the original function definition which has now had its body
   removed, and use it as the declaration of the ramp which both replaces the
   user's written function at call sites, and is responsible for starting
   the coroutine it defined.
   returns false on error.

   We should arrive here with the state of the compiler as if we had just
   executed start_preparsed_function().  */

bool
cp_coroutine_transform::build_ramp_function ()
{
  gcc_checking_assert (current_binding_level
		       && current_binding_level->kind == sk_function_parms);

  /* This is completely synthetic code, if we find an issue then we have not
     much chance to point at the most useful place in the user's code.  In
     lieu of this use the function start - so at least the diagnostic relates
     to something that the user can inspect.  */
  iloc_sentinel saved_position (fn_start);
  location_t loc = fn_start;

  tree promise_type = get_coroutine_promise_type (orig_fn_decl);
  tree fn_return_type = TREE_TYPE (TREE_TYPE (orig_fn_decl));
  bool void_ramp_p = VOID_TYPE_P (fn_return_type);
  /* We know there was no return statement, that is intentional.  */
  suppress_warning (orig_fn_decl, OPT_Wreturn_type);

  /* [dcl.fct.def.coroutine] / 10 (part1)
    The unqualified-id get_return_object_on_allocation_failure is looked up
    in the scope of the promise type by class member access lookup.  */

  /* We don't require this,  but, if the lookup succeeds, then the function
     must be usable, punt if it is not.  */
  tree grooaf_meth
    = lookup_promise_method (orig_fn_decl,
			     coro_gro_on_allocation_fail_identifier, loc,
			     /*musthave*/ false);
  tree grooaf = NULL_TREE;
  tree dummy_promise
    = build_dummy_object (get_coroutine_promise_type (orig_fn_decl));
  if (grooaf_meth && grooaf_meth != error_mark_node)
    {
      grooaf
	= coro_build_promise_expression (orig_fn_decl, dummy_promise,
					 coro_gro_on_allocation_fail_identifier,
					 fn_start, NULL, /*musthave=*/false);

      /* That should succeed.  */
      if (!grooaf || grooaf == error_mark_node)
	{
	  error_at (fn_start, "%qE is provided by %qT but is not usable with"
		    " the function %qD", coro_gro_on_allocation_fail_identifier,
		    promise_type, orig_fn_decl);
	  return false;
	}
    }

  /* Check early for usable allocator/deallocator, without which we cannot
     build a useful ramp; early exit if they are not available or usable.  */

  frame_size = TYPE_SIZE_UNIT (frame_type);

  /* Make a var to represent the frame pointer early.  */
  tree coro_fp = coro_build_artificial_var (loc, "_Coro_frameptr",
					    frame_ptr_type, orig_fn_decl,
					    NULL_TREE);

  tree new_fn_call
    = build_coroutine_frame_alloc_expr (promise_type, orig_fn_decl, fn_start,
					grooaf, &param_uses, frame_size);

  /* We must have a useable allocator to proceed.  */
  if (!new_fn_call || new_fn_call == error_mark_node)
    return false;

  /* Likewise, we need the DTOR to delete the frame.  */
  tree delete_frame_call
    = build_coroutine_frame_delete_expr (coro_fp, frame_size, promise_type,
					 fn_start);
  if (!delete_frame_call || delete_frame_call == error_mark_node)
    return false;

  /* At least verify we can lookup the get return object method.  */
  tree get_ro_meth
    = lookup_promise_method (orig_fn_decl,
			     coro_get_return_object_identifier, loc,
			     /*musthave*/ true);
  if (!get_ro_meth || get_ro_meth == error_mark_node)
    return false;

  /* So now construct the Ramp: */

  tree ramp_fnbody = begin_compound_stmt (BCS_FN_BODY);
  coro_fp = pushdecl (coro_fp);
  add_decl_expr (coro_fp);

  tree coro_promise_live = NULL_TREE;
  tree coro_gro_live = NULL_TREE;
  if (flag_exceptions)
    {
      /* Signal that we need to clean up the promise object on exception.  */
      coro_promise_live
	= coro_build_and_push_artificial_var (loc, "_Coro_promise_live",
					      boolean_type_node, orig_fn_decl,
					      boolean_false_node);

      /* When the get-return-object is in the RETURN slot, we need to arrange
	 for cleanup on exception.  */
      coro_gro_live
	= coro_build_and_push_artificial_var (loc, "_Coro_gro_live",
					      boolean_type_node, orig_fn_decl,
					      boolean_false_node);

      /* To signal that we need to cleanup copied function args.  */
      if (DECL_ARGUMENTS (orig_fn_decl))
	for (tree arg = DECL_ARGUMENTS (orig_fn_decl); arg != NULL;
	     arg = DECL_CHAIN (arg))
	  {
	    param_info *parm_i = param_uses.get (arg);
	    if (parm_i->trivial_dtor)
	      continue;
	    parm_i->guard_var = pushdecl (parm_i->guard_var);
	    add_decl_expr (parm_i->guard_var);
	  }
    }

  /* deref the frame pointer, to use in member access code.  */
  tree deref_fp
    = cp_build_indirect_ref (loc, coro_fp, RO_UNARY_STAR,
			     tf_warning_or_error);
  tree frame_needs_free
    = coro_build_and_push_artificial_var_with_dve (loc,
						   coro_frame_needs_free_id,
						   boolean_type_node,
						   orig_fn_decl, NULL_TREE,
						   deref_fp);

  /* Build the frame.  */

  /* The CO_FRAME internal function is a mechanism to allow the middle end
     to adjust the allocation in response to optimizations.  We provide the
     current conservative estimate of the frame size (as per the current)
     computed layout.  */

  tree resizeable
    = build_call_expr_internal_loc (loc, IFN_CO_FRAME, size_type_node, 2,
				    frame_size,
				    build_zero_cst (frame_ptr_type));
  CALL_EXPR_ARG (new_fn_call, 0) = resizeable;
  tree allocated = build1 (CONVERT_EXPR, frame_ptr_type, new_fn_call);
  tree r = cp_build_init_expr (coro_fp, allocated);
  finish_expr_stmt (r);

  /* If the user provided a method to return an object on alloc fail, then
     check the returned pointer and call the func if it's null.
     Otherwise, no check, and we fail for noexcept/fno-exceptions cases.  */

  if (grooaf)
    {
      /* [dcl.fct.def.coroutine] / 10 (part 3)
	 If the allocation function returns nullptr,the coroutine returns
	 control to the caller of the coroutine and the return value is
	 obtained by a call to T::get_return_object_on_allocation_failure(),
	 where T is the promise type.  */
      tree if_stmt = begin_if_stmt ();
      tree cond = build1 (CONVERT_EXPR, frame_ptr_type, nullptr_node);
      cond = build2 (EQ_EXPR, boolean_type_node, coro_fp, cond);
      finish_if_stmt_cond (cond, if_stmt);
      r = NULL_TREE;
      if (void_ramp_p)
	/* Execute the get-return-object-on-alloc-fail call...  */
	finish_expr_stmt (grooaf);
      else
	/* Get the fallback return object.  */
	r = grooaf;
      finish_return_stmt (r);
      finish_then_clause (if_stmt);
      finish_if_stmt (if_stmt);
    }

  /* For now, once allocation has succeeded we always assume that this needs
     destruction, there's no impl. for frame allocation elision.  */
  r = cp_build_init_expr (frame_needs_free, boolean_true_node);
  finish_expr_stmt (r);

  /* Set up the promise.  */
  tree p
    = coro_build_and_push_artificial_var_with_dve (loc, coro_promise_id,
						   promise_type, orig_fn_decl,
						   NULL_TREE, deref_fp);

  /* Up to now any exception thrown will propagate directly to the caller.
     This is OK since the only source of such exceptions would be in allocation
     of the coroutine frame, and therefore the ramp will not have initialized
     any further state.  From here, we will track state that needs explicit
     destruction in the case that promise or g.r.o setup fails or an exception
     is thrown from the initial suspend expression.  */
  tree ramp_try_block = NULL_TREE;
  tree ramp_try_stmts = NULL_TREE;
  tree iarc_x = NULL_TREE;
  if (flag_exceptions)
    {
      iarc_x
	= coro_build_and_push_artificial_var_with_dve (loc,
						       coro_frame_i_a_r_c_id,
						       boolean_type_node,
						       orig_fn_decl, NULL_TREE,
						       deref_fp);
      ramp_try_block = begin_try_block ();
      ramp_try_stmts = begin_compound_stmt (BCS_TRY_BLOCK);
    }

  /* Put the resumer and destroyer functions in.  */

  tree actor_addr = build1 (ADDR_EXPR, act_des_fn_ptr_type, resumer);
  coro_build_and_push_artificial_var_with_dve (loc, coro_resume_fn_id,
					       act_des_fn_ptr_type,
					       orig_fn_decl,
					       actor_addr, deref_fp);

  tree destroy_addr = build1 (ADDR_EXPR, act_des_fn_ptr_type, destroyer);
  coro_build_and_push_artificial_var_with_dve (loc, coro_destroy_fn_id,
					       act_des_fn_ptr_type,
					       orig_fn_decl,
					       destroy_addr, deref_fp);

  /* [dcl.fct.def.coroutine] /13
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

  if (DECL_ARGUMENTS (orig_fn_decl))
    {
      promise_args = make_tree_vector ();
      for (tree arg = DECL_ARGUMENTS (orig_fn_decl); arg != NULL;
	   arg = DECL_CHAIN (arg))
	{
	  bool existed;
	  param_info &parm = param_uses.get_or_insert (arg, &existed);
	  tree fld_idx
	    = coro_build_frame_access_expr (deref_fp, parm.field_id,
					    false, tf_warning_or_error);

	  /* Add this to the promise CTOR arguments list, accounting for
	     refs and special handling for method this ptr.  */
	  if (parm.this_ptr || parm.lambda_cobj)
	    {
	      /* We pass a reference to *this to the param preview.  */
	      /* It's unsafe to use the cp_ version here since current_class_ref
		 might've gotten clobbered earlier during rewrite_param_uses.  */
	      tree this_ref = build_fold_indirect_ref (arg);
	      vec_safe_push (promise_args, this_ref);
	    }
	  else if (parm.rv_ref)
	    vec_safe_push (promise_args, move (fld_idx));
	  else
	    vec_safe_push (promise_args, fld_idx);

	  if (parm.rv_ref || parm.pt_ref)
	    /* Initialise the frame reference field directly.  */
	    r = build2 (INIT_EXPR, TREE_TYPE (arg),
			TREE_OPERAND (fld_idx, 0), arg);
	  else
	    {
	      r = forward_parm (arg);
	      r = cp_build_modify_expr (loc, fld_idx, INIT_EXPR, r,
					tf_warning_or_error);
	    }
	  finish_expr_stmt (r);
	  if (!parm.trivial_dtor)
	    {
	      param_dtor_list.safe_push (parm.field_id);
	      /* Cleanup this frame copy on exception.  */
	      parm.fr_copy_dtor
		= cxx_maybe_build_cleanup (fld_idx, tf_warning_or_error);
	      if (flag_exceptions)
		{
		  /* This var is now live.  */
		  r = build_modify_expr (loc, parm.guard_var,
					 boolean_type_node, INIT_EXPR, loc,
					 boolean_true_node, boolean_type_node);
		  finish_expr_stmt (r);
		}
	    }
	}
    }

  if (type_build_ctor_call (promise_type))
    {
      /* Construct the promise object [dcl.fct.def.coroutine] / 5.7.

	 First try to find a constructor with an argument list comprised of
	 the parameter copies.  */

      if (DECL_ARGUMENTS (orig_fn_decl))
	{
	  r = build_special_member_call (p, complete_ctor_identifier,
					 &promise_args, promise_type,
					 LOOKUP_NORMAL, tf_none);
	  release_tree_vector (promise_args);
	}
      else
	r = NULL_TREE;

      /* If that fails then the promise constructor argument list is empty.  */
      if (r == NULL_TREE || r == error_mark_node)
	r = build_special_member_call (p, complete_ctor_identifier, NULL,
				       promise_type, LOOKUP_NORMAL,
				       tf_warning_or_error);

      /* If type_build_ctor_call() encounters deprecated implicit CTORs it will
	 return true, and therefore we will execute this code path.  However,
	 we might well not actually require a CTOR and under those conditions
	 the build call above will not return a call expression, but the
	 original instance object.  Do not attempt to add the statement unless
	 it has side-effects.  */
      if (r && r != error_mark_node && TREE_SIDE_EFFECTS (r))
	finish_expr_stmt (r);
    }

  tree promise_dtor = cxx_maybe_build_cleanup (p, tf_warning_or_error);;
  if (flag_exceptions && promise_dtor)
    {
      r = cp_build_init_expr (coro_promise_live, boolean_true_node);
      finish_expr_stmt (r);
    }

  tree get_ro
    = coro_build_promise_expression (orig_fn_decl, p,
				     coro_get_return_object_identifier,
				     fn_start, NULL, /*musthave=*/true);

  /* Without a return object we haven't got much clue what's going on.  */
  if (!get_ro || get_ro == error_mark_node)
    return false;

  /* Check for a bad get return object type.
     [dcl.fct.def.coroutine] / 7 requires:
     The expression promise.get_return_object() is used to initialize the
     returned reference or prvalue result object ... */
  tree gro_type = TREE_TYPE (get_ro);
  if (VOID_TYPE_P (gro_type) && !void_ramp_p)
    {
      error_at (fn_start, "no viable conversion from %<void%> provided by"
		" %<get_return_object%> to return type %qT", fn_return_type);
      return false;
    }

  /* Initialize the resume_idx_var to 0, meaning "not started".  */
  coro_build_and_push_artificial_var_with_dve
    (loc, coro_resume_index_id, short_unsigned_type_node,  orig_fn_decl,
     build_zero_cst (short_unsigned_type_node), deref_fp);

  if (flag_exceptions && iarc_x)
    {
      r = cp_build_init_expr (iarc_x, boolean_false_node);
      finish_expr_stmt (r);
    }

  /* Used for return objects in the RESULT slot.  */
  tree ret_val_dtor = NULL_TREE;

  /* [dcl.fct.def.coroutine] / 7
     The expression promise.get_return_object() is used to initialize the
     glvalue result or prvalue result object of a call to a coroutine.  */

  /* We must manage the cleanups ourselves, because the responsibility for
     them changes after the initial suspend.  However, any use of
     cxx_maybe_build_cleanup () can set the throwing_cleanup flag.  */
  cp_function_chain->throwing_cleanup = false;
  if (void_ramp_p)
    /* We still want to call the method, even if the result is unused.  */
    r = get_ro;
  else
    {
      /* The initial section of finish_return_expr ().  */
      bool no_warning;
      bool dangling;
      /* Without a relevant location, bad conversions in check_return_expr
	 result in unusable diagnostics, since there is not even a mention
	 of the relevant function.  Here we carry out the first part of
	 finish_return_expr().  */
      input_location = fn_start;
      r = check_return_expr (get_ro, &no_warning, &dangling);
      input_location = UNKNOWN_LOCATION;
      gcc_checking_assert (!dangling);
      /* Check for bad things.  */
      if (!r || r == error_mark_node)
	return false;
    }

  finish_expr_stmt (r);

  if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (fn_return_type))
    /* If some part of the initalization code (prior to the await_resume
       of the initial suspend expression), then we need to clean up the
       return value.  */
    ret_val_dtor = cxx_maybe_build_cleanup (DECL_RESULT (orig_fn_decl),
					    tf_warning_or_error);

  /* If we have a live g.r.o in the return slot, then signal this for exception
     cleanup.  */
  if (flag_exceptions && ret_val_dtor)
    {
      r = cp_build_init_expr (coro_gro_live, boolean_true_node);
      finish_expr_stmt (r);
    }

  /* Start the coroutine body.  */
  r = build_call_expr_loc (fn_start, resumer, 1, coro_fp);
  finish_expr_stmt (r);

  /* The ramp is done, we just need the return statement, which we build from
     the return object we constructed before we called the actor.  */

  r = void_ramp_p ? NULL_TREE : DECL_RESULT (orig_fn_decl);

  /* The reminder of finish_return_expr ().  */
  r = build_stmt (loc, RETURN_EXPR, r);
  r = maybe_cleanup_point_expr_void (r);
  r = add_stmt (r);

  if (flag_exceptions)
    {
      finish_compound_stmt (ramp_try_stmts);
      finish_try_block (ramp_try_block);
      tree handler = begin_handler ();
      finish_handler_parms (NULL_TREE, handler); /* catch (...) */

      /* If we have a live G.R.O in the return slot, then run its DTOR.  */
      if (ret_val_dtor && ret_val_dtor != error_mark_node)
	{
	  tree gro_d_if = begin_if_stmt ();
	  finish_if_stmt_cond (coro_gro_live, gro_d_if);
	  finish_expr_stmt (ret_val_dtor);
	  finish_then_clause (gro_d_if);
	  finish_if_stmt (gro_d_if);
	}

      /* Before initial resume is called, the responsibility for cleanup on
	 exception falls to the ramp.  After that, the coroutine body code
	 should do the cleanup.  This is signalled by the flag
	 'initial_await_resume_called'.  */

      tree not_iarc
	= build1_loc (loc, TRUTH_NOT_EXPR, boolean_type_node, iarc_x);
      tree cleanup_if = begin_if_stmt ();
      finish_if_stmt_cond (not_iarc, cleanup_if);
      /* If the promise is live, then run its dtor if that's available.  */
      if (promise_dtor && promise_dtor != error_mark_node)
	{
	  tree promise_d_if = begin_if_stmt ();
	  finish_if_stmt_cond (coro_promise_live, promise_d_if);
	  finish_expr_stmt (promise_dtor);
	  finish_then_clause (promise_d_if);
	  finish_if_stmt (promise_d_if);
	}

      /* Clean up any frame copies of parms with non-trivial dtors.
	 Do this in reverse order from their creation.  */
      auto_vec<param_info *> worklist;
      if (DECL_ARGUMENTS (orig_fn_decl))
	for (tree arg = DECL_ARGUMENTS (orig_fn_decl); arg != NULL;
	     arg = DECL_CHAIN (arg))
	  {
	    param_info *parm_i = param_uses.get (arg);
	    if (parm_i->trivial_dtor)
	      continue;
	    worklist.safe_push (parm_i);
	  }
      while (!worklist.is_empty ())
	{
	  param_info *parm_i = worklist.pop ();
	  if (parm_i->fr_copy_dtor && parm_i->fr_copy_dtor != error_mark_node)
	    {
	      tree dtor_if = begin_if_stmt ();
	      finish_if_stmt_cond (parm_i->guard_var, dtor_if);
	      finish_expr_stmt (parm_i->fr_copy_dtor);
	      finish_then_clause (dtor_if);
	      finish_if_stmt (dtor_if);
	    }
	}

      /* No delete the frame if required.  */
      tree fnf_if = begin_if_stmt ();
      finish_if_stmt_cond (frame_needs_free, fnf_if);
      finish_expr_stmt (delete_frame_call);
      finish_then_clause (fnf_if);
      finish_if_stmt (fnf_if);

      /* Finished cleanups conditional on "initial resume is not called".  */
      finish_then_clause (cleanup_if);
      finish_if_stmt (cleanup_if);

      tree rethrow = build_throw (loc, NULL_TREE, tf_warning_or_error);
      suppress_warning (rethrow);
      finish_expr_stmt (rethrow);
      finish_handler (handler);
      finish_handler_sequence (ramp_try_block);
    }
  finish_compound_stmt (ramp_fnbody);
  return true;
}

/* ------- Encapsulate analysis of the couroutine -------- */


cp_coroutine_transform::cp_coroutine_transform (tree _orig_fn, bool _inl)
    : orig_fn_decl (_orig_fn), inline_p (_inl)
  {
    /* We don't expect to be called with missing decl or e_m_n.  */
    gcc_checking_assert (orig_fn_decl
			 && TREE_CODE (orig_fn_decl) == FUNCTION_DECL);
    if (!coro_function_valid_p (orig_fn_decl))
      {
	/* For early errors, we do not want a diagnostic about the missing
	   ramp return value, since the user cannot fix this - a 'return' is
	   not allowed in a coroutine.  */
	suppress_warning (orig_fn_decl, OPT_Wreturn_type);
	/* Discard the body, we can't process it further...  */
	pop_stmt_list (DECL_SAVED_TREE (orig_fn_decl));
	/* ... and make an empty fn.  */
	DECL_SAVED_TREE (orig_fn_decl) = push_stmt_list ();
	/* Match the expected nesting when an eh block is in use.  */
	if (use_eh_spec_block (orig_fn_decl))
	  current_eh_spec_block = begin_eh_spec_block ();
	valid_coroutine = false;
      }

    /* We don't have the locus of the opening brace - it's filled in later (and
       there doesn't really seem to be any easy way to get at it).
       The closing brace is assumed to be input_location.  */
    fn_start = DECL_SOURCE_LOCATION (orig_fn_decl);

    /* Build types we need.  */
    tree fr_name = get_fn_local_identifier (orig_fn_decl, "Frame");
    frame_type = xref_tag (record_type, fr_name);
    DECL_CONTEXT (TYPE_NAME (frame_type)) = DECL_CONTEXT (orig_fn_decl);
    frame_ptr_type = build_pointer_type (frame_type);
    act_des_fn_type
      = build_function_type_list (void_type_node, frame_ptr_type, NULL_TREE);
    act_des_fn_ptr_type = build_pointer_type (act_des_fn_type);
    valid_coroutine = true;
  }

cp_coroutine_transform::~cp_coroutine_transform ()
{
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
  void (*_Coro_resume_fn)(_R_frame *);
  void (*_Coro_destroy_fn)(_R_frame *);
  coro1::promise_type _Coro_promise;
  bool _Coro_frame_needs_free; free the coro frame mem if set.
  bool _Coro_i_a_r_c; [dcl.fct.def.coroutine] / 5.3
  short _Coro_resume_index;
  handle_type _Coro_self_handle;
  parameter copies (were required).
  local variables saved (including awaitables)
  (maybe) trailing space.
 };  */

void
cp_coroutine_transform::apply_transforms ()
{
  if (dmp_str == NULL)
    dmp_str = dump_begin (coro_dump_id, &coro_dump_flags);

  coro_maybe_dump_initial_function (orig_fn_decl);

  coroutine_body
    = split_coroutine_body_from_ramp (orig_fn_decl);
  if (!coroutine_body)
    {
      valid_coroutine = false;
      return;
    }
  /* Keep the original function block tree to one side and reset.  */
  body_blocks = current_binding_level->blocks;
  current_binding_level->blocks = NULL_TREE;

  /* Collect information on the original function params and their use in the
     function body.  */
  analyze_fn_parms (orig_fn_decl, &param_uses);

  /* Declare the actor and destroyer functions, the following code needs to
     see these.  */
  resumer
    = coro_build_actor_or_destroy_function (orig_fn_decl, act_des_fn_type,
					    frame_ptr_type, true);
  destroyer
    = coro_build_actor_or_destroy_function (orig_fn_decl, act_des_fn_type,
					    frame_ptr_type, false);

  /* Transform the function body as per [dcl.fct.def.coroutine] / 5.  */
  wrap_original_function_body ();

  /* Analyze the body await expressions.  */
  susp_frame_data body_aw_points (fs_label, &suspend_points);
  cp_walk_tree (&coroutine_body, await_statement_walker, &body_aw_points, NULL);
  await_count = body_aw_points.await_number;

  /* Determine the fields for the coroutine state.  */
  tree field_list = NULL_TREE;
  local_vars_frame_data local_vars_data (&field_list, &local_var_uses);
  cp_walk_tree_without_duplicates (&coroutine_body, register_local_var_uses,
				   &local_vars_data);

  /* Conservative computation of the coroutine frame content.  */
  frame_type = begin_class_definition (frame_type);
  TYPE_FIELDS (frame_type) = field_list;
  TYPE_BINFO (frame_type) = make_tree_binfo (0);
  BINFO_OFFSET (TYPE_BINFO (frame_type)) = size_zero_node;
  BINFO_TYPE (TYPE_BINFO (frame_type)) = frame_type;
  frame_type = finish_struct (frame_type, NULL_TREE);

  valid_coroutine = build_ramp_function ();
  coro_maybe_dump_ramp (orig_fn_decl);
}

/* Having analysed and collected the necessary data we are now in a position
   to build the outlined coroutine body and the destroyer shim.  */

void
cp_coroutine_transform::finish_transforms ()
{
  if (!valid_coroutine)
    return;

  current_function_decl = resumer;
  build_actor_fn (fn_start, frame_type, resumer, coroutine_body, orig_fn_decl,
		  &local_var_uses, &suspend_points, &param_dtor_list,
		  resume_idx_var, await_count, frame_size, inline_p);

  current_function_decl = destroyer;
  build_destroy_fn (fn_start, frame_type, destroyer, resumer, inline_p);

  coro_maybe_dump_transformed_functions (resumer, destroyer);
}

#include "gt-cp-coroutines.h"

