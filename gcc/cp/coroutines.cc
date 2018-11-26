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
#include "tree.h"
#include "cxx-pretty-print.h"
#include "gcc-rich-location.h"

/* DEBUG remove me.  */
extern void debug_tree(tree);

static tree find_handle_type (location_t);
static tree find_coro_handle_type (location_t, tree);
static tree find_promise_type (tree);
static tree lookup_promise_member (tree, const char *, location_t, bool);
static bool coro_promise_type_found_p (tree, location_t);

/* Lookup the coroutine_traits template decl.
   Instantiate that for the function signature.  */

static tree
find_handle_type (location_t kw)
{
  unsigned p;
  /* we want std::experimental::coroutine_traits class template decl.  */
  tree exp_name = get_identifier ("experimental");
  tree traits_name = get_identifier ("coroutine_traits");
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

  tree handle_type = lookup_template_class (traits_name, targ,
					    /* in_decl */ NULL_TREE,
					    /* context */ exp_ns,
					    /* entering scope */ false,
					    tf_none);

  if (handle_type == error_mark_node)
    {
      error_at (kw, "couldn't instantiate coroutine_traits");
      return NULL_TREE;
    }

  return handle_type;
}

static tree
find_coro_handle_type (location_t kw, tree promise_type)
{
  /* we want std::experimental::coroutine_handle class template decl.  */
  tree exp_name = get_identifier ("experimental");
  tree handle_name = get_identifier ("coroutine_handle");
  tree exp_ns = lookup_qualified_name (std_node, exp_name, 0, false, false);
  tree targ = make_tree_vec (1);

  if (exp_ns == error_mark_node)
    {
      error_at (kw, "std::experimental not found");
      return NULL_TREE;
    }

  /* So now build up a type list for the template, one entr, the promise.  */
  TREE_VEC_ELT (targ, 0) = promise_type;

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
    {
      tree handle_type = find_handle_type (loc);
      DECL_COROUTINE_PROMISE_TYPE(fndecl) = find_promise_type (handle_type);
      DECL_COROUTINE_HANDLE_TYPE(fndecl)
       = find_coro_handle_type (loc, DECL_COROUTINE_PROMISE_TYPE(fndecl));
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
  int __resume_at; // this is where clang puts it - but it's a smaller entity.
  coro1::suspend_never_prt __is;
  (maybe) handle_type i_hand;
  coro1::suspend_always_prt __fs;
  (maybe) handle_type f_hand;
  // here could be args.
  // and then trailing space.
 };

*/
tree
morph_fn_to_coro (tree orig)
{
  if (! orig || TREE_CODE (orig) != FUNCTION_DECL)
    return orig;

  gcc_assert (orig == current_function_decl);

  if (! coro_function_valid_p (orig))
    return NULL_TREE;

  /* We can't validly get here with an empty statement list, since there's no
     way for the FE to decide it's a coroutine in the absence of any code.  */
  tree fnbody = pop_stmt_list (DECL_SAVED_TREE (orig));
  if (fnbody == NULL_TREE)
    return orig;

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
  tree initial_suspend_meth = lookup_promise_member (orig, "initial_suspend",
						     fn_start,
						     true /*musthave*/);
  if (! initial_suspend_meth || initial_suspend_meth == error_mark_node)
    return orig;

  tree initial_suspend_type = TREE_TYPE (TREE_TYPE (initial_suspend_meth));
  if (TREE_CODE (initial_suspend_type) != RECORD_TYPE)
    {
      error_at (&fn_start_loc,
		"the initial suspend type needs to be an aggregate");
      inform (DECL_SOURCE_LOCATION
               (MAYBE_BASELINK_FUNCTIONS (initial_suspend_meth)),
	      "declared here");
      /* FIXME: Match clang wording.  */
      return orig;
    }
  tree final_suspend_meth = lookup_promise_member (orig, "final_suspend",
						   fn_start,
						   true /*musthave*/);
  if (! final_suspend_meth || final_suspend_meth == error_mark_node)
    return orig;

  tree final_suspend_type = TREE_TYPE (TREE_TYPE (final_suspend_meth));
  if (TREE_CODE (final_suspend_type) != RECORD_TYPE)
    {
      error_at (fn_start, "the final suspend type needs to be an aggregate");
      /* FIXME: Match clang wording, add a note to the definition.  */
      return orig;
    }

  /* Types we need to define.  */

  tree coro_frame_type = xref_tag (record_type, get_identifier ("_R_frame"),
				   ts_current, false);
  DECL_CONTEXT (TYPE_NAME (coro_frame_type)) = current_scope ();
  tree coro_frame_ptr = build_pointer_type (coro_frame_type);
  tree act_des_fn_type = build_function_type_list (void_type_node,
						   coro_frame_ptr, NULL_TREE);
  tree act_des_fn_ptr = build_pointer_type (act_des_fn_type);

   /* Build our dummy coro frame layout.  */
  coro_frame_type = begin_class_definition (coro_frame_type);
  tree resume_name = get_identifier ("__resume");
  tree decls = build_decl (fn_start, FIELD_DECL, resume_name, act_des_fn_ptr);
  tree destroy_name = get_identifier ("__destroy");
  tree decl = build_decl (fn_start, FIELD_DECL, destroy_name, act_des_fn_ptr);
  DECL_CHAIN (decl) = decls; decls = decl;
  tree promise_name = get_identifier ("__p");
  decl = build_decl (fn_start, FIELD_DECL, promise_name, promise_type);
  DECL_CHAIN (decl) = decls; decls = decl;
  tree phase_name = get_identifier ("__resume_at");
  decl = build_decl (fn_start, FIELD_DECL, phase_name, integer_type_node);
  DECL_CHAIN (decl) = decls; decls = decl;
  // TODO: decide if we need to preserve things across initial susp.
  tree init_susp_name = get_identifier ("__is");
  decl = build_decl (fn_start, FIELD_DECL, init_susp_name,
		     initial_suspend_type);
  DECL_CHAIN (decl) = decls; decls = decl;
  // TODO: decide if we need to preserve things across final susp.
  tree fin_susp_name = get_identifier ("__fs");
  decl = build_decl (fn_start, FIELD_DECL, fin_susp_name, final_suspend_type);
  DECL_CHAIN (decl) = decls; decls = decl;
  TYPE_FIELDS (coro_frame_type) = decls;

  TYPE_BINFO (coro_frame_type) = make_tree_binfo (0);
  BINFO_OFFSET (TYPE_BINFO (coro_frame_type)) = size_zero_node;
  BINFO_TYPE (TYPE_BINFO (coro_frame_type)) = coro_frame_type;

  coro_frame_type = finish_struct (coro_frame_type, NULL_TREE);

  /* Now build the ramp function pieces.  */
  tree bind = build3 (BIND_EXPR, void_type_node, NULL, NULL, NULL);
  add_stmt (bind);
  BIND_EXPR_BODY (bind) = push_stmt_list ();

  tree coro_fp = build_lang_decl (VAR_DECL, get_identifier ("coro.frameptr"),
				  coro_frame_ptr);
  DECL_CONTEXT (coro_fp) = current_scope ();
  tree r = build_stmt (fn_start, DECL_EXPR, coro_fp);
  add_stmt (r);
#if CAN_GIMPLIFY_CORO
  tree allocated = build1 (CORO_ALLOCATOR,
			   build_pointer_type (void_type_node),
			   coro_frame_type);
#else
  /* Placeholder.  */
  tree allocated = build1 (CONVERT_EXPR, build_pointer_type (void_type_node),
			   integer_zero_node);
#endif
  allocated = build1 (CONVERT_EXPR, coro_frame_ptr, allocated);
  r = build2 (INIT_EXPR, TREE_TYPE (coro_fp), coro_fp, allocated);
  add_stmt (r);
  tree varlist = coro_fp;

  tree gro = build_lang_decl (VAR_DECL, get_identifier ("coro.gro"),
			      fn_return_type);
  DECL_CONTEXT (gro) = current_scope ();
  r = build_stmt (fn_start, DECL_EXPR, gro);
  add_stmt (r);
  TREE_CHAIN (gro) = varlist; varlist = gro;

  /* Collected the scope vars we need ... */
  BIND_EXPR_VARS (bind) = nreverse (varlist);

  tree deref_fp = build_x_arrow (fn_start, coro_fp, tf_warning_or_error);
  tree promise_m = lookup_member (coro_frame_type, promise_name,
				  /*protect*/1,  /*want_type*/ 0,
				  tf_warning_or_error);
  tree p = build_class_member_access_expr (deref_fp, promise_m, NULL_TREE,
					   false, tf_warning_or_error);
  /* Do a placement new constructor for the promise type (we never call the
     new operator, just the constructor on the object in place in the frame.
  */
  r = build_special_member_call(p, complete_ctor_identifier, NULL,
				promise_type, LOOKUP_NORMAL,
				tf_warning_or_error);
  add_stmt (r);

  /* Set the initial_suspend var.  */
  tree is_m = lookup_member (coro_frame_type, init_susp_name,
			     /*protect*/1,  /*want_type*/ 0,
			     tf_warning_or_error);
  tree is = build_class_member_access_expr (deref_fp, is_m, NULL_TREE,
					   false, tf_warning_or_error);

  tree get_is_meth = lookup_promise_member (orig, "initial_suspend",
					   fn_start, true /*musthave*/);
  r = build_new_method_call (rvalue (p), get_is_meth,
			     NULL, NULL_TREE, LOOKUP_NORMAL,
			     NULL, tf_warning_or_error);
  // init our actual var.
  r = build2 (INIT_EXPR, initial_suspend_type, is, TREE_OPERAND (r, 1));
  r = build_stmt (fn_start, EXPR_STMT, r);
  add_stmt (r);

  /* Set the final suspend var.  */
  tree fs_m = lookup_member (coro_frame_type, fin_susp_name,
			     /*protect*/1,  /*want_type*/ 0,
			     tf_warning_or_error);
  tree fs = build_class_member_access_expr (deref_fp, fs_m, NULL_TREE,
					   false, tf_warning_or_error);
  tree get_fs_meth = lookup_promise_member (orig, "final_suspend",
					   fn_start, true /*musthave*/);
  r = build_new_method_call (rvalue (p), get_fs_meth,
			     NULL, NULL_TREE, LOOKUP_NORMAL,
			     NULL, tf_warning_or_error);
  // init our actual var.
  r = build2 (INIT_EXPR, final_suspend_type, fs, TREE_OPERAND (r, 1));
  r = build_stmt (fn_start, EXPR_STMT, r);
  add_stmt (r);

  tree gro_meth = lookup_promise_member (orig, "get_return_object",
				    fn_start, true /*musthave*/);
  r = build_new_method_call (rvalue (p), gro_meth,
			     NULL, NULL_TREE, LOOKUP_NORMAL,
			     NULL, tf_warning_or_error);
  // init our actual var.
  r = build2 (INIT_EXPR, TREE_TYPE (gro), gro, TREE_OPERAND (r, 1));
  r = build_stmt (fn_start, EXPR_STMT, r);
  r = maybe_cleanup_point_expr_void (r);
  add_stmt (r);

  tree start_label = get_identifier ("coro.start");
  start_label = define_label (fn_start, start_label);

  start_label = build_stmt (fn_start, LABEL_EXPR, start_label);
  add_stmt (start_label);

  /* Initialise the phase to -1, not started.  */
  tree phase_m = lookup_member (coro_frame_type, phase_name,
				/*protect*/1,  /*want_type*/ 0,
				tf_warning_or_error);
  tree phase = build_class_member_access_expr (deref_fp, phase_m, NULL_TREE,
						false, tf_warning_or_error);
  r = build2 (INIT_EXPR, integer_type_node, phase, integer_minus_one_node);
  r = build_stmt (fn_start, EXPR_STMT, r);
  add_stmt (r);

#if CAN_GIMPLIFY_CORO
  tree initial_aw = build4_loc (fn_start, COAWAIT_EXPR, void_type_node,
				is, initial_suspend_type,
				NULL_TREE, NULL_TREE);
  r = build_stmt (fn_start, EXPR_STMT, initial_aw);
  add_stmt (r);
#endif

  // TODO: process fnbody for co_returns / possible expansions.
  add_stmt (fnbody);

  tree fs_label = get_identifier ("final_suspend");
  fs_label = define_label (input_location, fs_label);
  r = build_stmt (input_location, LABEL_EXPR, fs_label);
  add_stmt (r);

#if CAN_GIMPLIFY_CORO
  tree final_aw = build4 (COAWAIT_EXPR, void_type_node, fs,
			  final_suspend_type, NULL_TREE, NULL_TREE);
  r = build_stmt (fn_start, EXPR_STMT, final_aw);
  add_stmt (r);
#endif

  /* now do the tail of the function.  */
  /* We can have the same label name per function (???).
     FIXME: do we have some FE way to test for support for '.' c.f. '$' as
     a non-user-label character.  */
  tree ret_label = get_identifier ("coro.ret");
  ret_label = define_label (input_location, ret_label);
  r = build_stmt (input_location, LABEL_EXPR, ret_label);
  add_stmt (r);

  bool no_warning;
  r = check_return_expr (rvalue (gro), &no_warning);
  r = build_stmt (input_location, RETURN_EXPR, r);
  TREE_NO_WARNING (r) |= no_warning;
  r = maybe_cleanup_point_expr_void (r);
  add_stmt (r);

  /* Exit points.  */
  tree del_promise_label = get_identifier ("coro.delete.retval");
  del_promise_label = define_label (input_location, del_promise_label);
  r = build_stmt (input_location, LABEL_EXPR, del_promise_label);
  add_stmt (r);

  r = build_special_member_call(gro, complete_dtor_identifier, NULL,
				fn_return_type, LOOKUP_NORMAL,
				tf_warning_or_error);
  add_stmt (r);

  tree del_frame_label = get_identifier ("coro.delete.frame");
  del_frame_label = define_label (input_location, del_frame_label);
  r = build_stmt (input_location, LABEL_EXPR, del_frame_label);
  add_stmt (r);

  /* Destructors for the things we built explicitly.  */
  r = build_special_member_call(fs, complete_dtor_identifier, NULL,
				final_suspend_type, LOOKUP_NORMAL,
				tf_warning_or_error);
  add_stmt (r);
  r = build_special_member_call(is, complete_dtor_identifier, NULL,
				initial_suspend_type, LOOKUP_NORMAL,
				tf_warning_or_error);
  add_stmt (r);
  r = build_special_member_call(p, complete_dtor_identifier, NULL,
				promise_type, LOOKUP_NORMAL,
				tf_warning_or_error);
  add_stmt (r);

  /* Here deallocate the frame (if we allocated it).
     Not yet.  */

  /* Weird case of having different return types, on this path we would
     return void (it should only be taken by the destroy actor function
     when split).  */
  r = build_stmt (input_location, RETURN_EXPR, NULL);
  TREE_NO_WARNING (r) |= 1; /* We don't want a warning about this.  */
  r = maybe_cleanup_point_expr_void (r);
  add_stmt (r);

  DECL_SAVED_TREE (orig) = newbody;
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
  if (expr == NULL_TREE || VOID_TYPE_P (expr))
    {
      if (lookup_promise_member (current_function_decl, "return_void",
				 kw, true /*musthave*/) == error_mark_node)
	return false;
    }
  else
    {
      if (lookup_promise_member (current_function_decl, "return_value",
				 kw, true /*musthave*/) == error_mark_node)
	return false;
    }

  /* Makes no sense for a co-routine really. */
  if (TREE_THIS_VOLATILE (current_function_decl))
    warning_at (kw, 0, "function declared %<noreturn%> has a"
		" %<co_return%> statement");

  return true;
}
