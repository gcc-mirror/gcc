/* Perform the semantic phase of lambda parsing, i.e., the process of
   building tree structure, checking semantic consistency, and
   building RTL.  These routines are used both during actual parsing
   and during the instantiation of template functions.

   Copyright (C) 1998-2019 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "cp-tree.h"
#include "stringpool.h"
#include "cgraph.h"
#include "tree-iterator.h"
#include "toplev.h"
#include "gimplify.h"

/* Constructor for a lambda expression.  */

tree
build_lambda_expr (void)
{
  tree lambda = make_node (LAMBDA_EXPR);
  LAMBDA_EXPR_DEFAULT_CAPTURE_MODE (lambda) = CPLD_NONE;
  LAMBDA_EXPR_CAPTURE_LIST         (lambda) = NULL_TREE;
  LAMBDA_EXPR_THIS_CAPTURE         (lambda) = NULL_TREE;
  LAMBDA_EXPR_PENDING_PROXIES      (lambda) = NULL;
  LAMBDA_EXPR_MUTABLE_P            (lambda) = false;
  return lambda;
}

/* Create the closure object for a LAMBDA_EXPR.  */

tree
build_lambda_object (tree lambda_expr)
{
  /* Build aggregate constructor call.
     - cp_parser_braced_list
     - cp_parser_functional_cast  */
  vec<constructor_elt, va_gc> *elts = NULL;
  tree node, expr, type;
  location_t saved_loc;

  if (processing_template_decl || lambda_expr == error_mark_node)
    return lambda_expr;

  /* Make sure any error messages refer to the lambda-introducer.  */
  saved_loc = input_location;
  input_location = LAMBDA_EXPR_LOCATION (lambda_expr);

  for (node = LAMBDA_EXPR_CAPTURE_LIST (lambda_expr);
       node;
       node = TREE_CHAIN (node))
    {
      tree field = TREE_PURPOSE (node);
      tree val = TREE_VALUE (node);

      if (field == error_mark_node)
	{
	  expr = error_mark_node;
	  goto out;
	}

      if (TREE_CODE (val) == TREE_LIST)
	val = build_x_compound_expr_from_list (val, ELK_INIT,
					       tf_warning_or_error);

      if (DECL_P (val))
	mark_used (val);

      /* Mere mortals can't copy arrays with aggregate initialization, so
	 do some magic to make it work here.  */
      if (TREE_CODE (TREE_TYPE (field)) == ARRAY_TYPE)
	val = build_array_copy (val);
      else if (DECL_NORMAL_CAPTURE_P (field)
	       && !DECL_VLA_CAPTURE_P (field)
	       && !TYPE_REF_P (TREE_TYPE (field)))
	{
	  /* "the entities that are captured by copy are used to
	     direct-initialize each corresponding non-static data
	     member of the resulting closure object."

	     There's normally no way to express direct-initialization
	     from an element of a CONSTRUCTOR, so we build up a special
	     TARGET_EXPR to bypass the usual copy-initialization.  */
	  val = force_rvalue (val, tf_warning_or_error);
	  if (TREE_CODE (val) == TARGET_EXPR)
	    TARGET_EXPR_DIRECT_INIT_P (val) = true;
	}

      CONSTRUCTOR_APPEND_ELT (elts, DECL_NAME (field), val);
    }

  expr = build_constructor (init_list_type_node, elts);
  CONSTRUCTOR_IS_DIRECT_INIT (expr) = 1;

  /* N2927: "[The closure] class type is not an aggregate."
     But we briefly treat it as an aggregate to make this simpler.  */
  type = LAMBDA_EXPR_CLOSURE (lambda_expr);
  CLASSTYPE_NON_AGGREGATE (type) = 0;
  expr = finish_compound_literal (type, expr, tf_warning_or_error);
  CLASSTYPE_NON_AGGREGATE (type) = 1;

 out:
  input_location = saved_loc;
  return expr;
}

/* Return an initialized RECORD_TYPE for LAMBDA.
   LAMBDA must have its explicit captures already.  */

tree
begin_lambda_type (tree lambda)
{
  /* Lambda names are nearly but not quite anonymous.  */
  tree name = make_anon_name ();
  IDENTIFIER_LAMBDA_P (name) = true;

  /* Create the new RECORD_TYPE for this lambda.  */
  tree type = xref_tag (/*tag_code=*/record_type, name,
			/*scope=*/ts_lambda, /*template_header_p=*/false);
  if (type == error_mark_node)
    return error_mark_node;

  /* Designate it as a struct so that we can use aggregate initialization.  */
  CLASSTYPE_DECLARED_CLASS (type) = false;

  /* Cross-reference the expression and the type.  */
  LAMBDA_EXPR_CLOSURE (lambda) = type;
  CLASSTYPE_LAMBDA_EXPR (type) = lambda;

  /* In C++17, assume the closure is literal; we'll clear the flag later if
     necessary.  */
  if (cxx_dialect >= cxx17)
    CLASSTYPE_LITERAL_P (type) = true;

  /* Clear base types.  */
  xref_basetypes (type, /*bases=*/NULL_TREE);

  /* Start the class.  */
  type = begin_class_definition (type);

  return type;
}

/* Returns the type to use for the return type of the operator() of a
   closure class.  */

tree
lambda_return_type (tree expr)
{
  if (expr == NULL_TREE)
    return void_type_node;
  if (type_unknown_p (expr)
      || BRACE_ENCLOSED_INITIALIZER_P (expr))
    {
      cxx_incomplete_type_error (expr, TREE_TYPE (expr));
      return error_mark_node;
    }
  gcc_checking_assert (!type_dependent_expression_p (expr));
  return cv_unqualified (type_decays_to (unlowered_expr_type (expr)));
}

/* Given a LAMBDA_EXPR or closure type LAMBDA, return the op() of the
   closure type.  */

tree
lambda_function (tree lambda)
{
  tree type;
  if (TREE_CODE (lambda) == LAMBDA_EXPR)
    type = LAMBDA_EXPR_CLOSURE (lambda);
  else
    type = lambda;
  gcc_assert (LAMBDA_TYPE_P (type));
  /* Don't let debug_tree cause instantiation.  */
  if (CLASSTYPE_TEMPLATE_INSTANTIATION (type)
      && !COMPLETE_OR_OPEN_TYPE_P (type))
    return NULL_TREE;
  lambda = lookup_member (type, call_op_identifier,
			  /*protect=*/0, /*want_type=*/false,
			  tf_warning_or_error);
  if (lambda)
    lambda = STRIP_TEMPLATE (get_first_fn (lambda));
  return lambda;
}

/* Returns the type to use for the FIELD_DECL corresponding to the
   capture of EXPR.  EXPLICIT_INIT_P indicates whether this is a
   C++14 init capture, and BY_REFERENCE_P indicates whether we're
   capturing by reference.  */

tree
lambda_capture_field_type (tree expr, bool explicit_init_p,
			   bool by_reference_p)
{
  tree type;
  bool is_this = is_this_parameter (tree_strip_nop_conversions (expr));

  if (!is_this && explicit_init_p)
    {
      tree auto_node = make_auto ();
      
      type = auto_node;
      if (by_reference_p)
	/* Add the reference now, so deduction doesn't lose
	   outermost CV qualifiers of EXPR.  */
	type = build_reference_type (type);
      type = do_auto_deduction (type, expr, auto_node);
    }
  else if (!is_this && type_dependent_expression_p (expr))
    {
      type = cxx_make_type (DECLTYPE_TYPE);
      DECLTYPE_TYPE_EXPR (type) = expr;
      DECLTYPE_FOR_LAMBDA_CAPTURE (type) = true;
      DECLTYPE_FOR_REF_CAPTURE (type) = by_reference_p;
      SET_TYPE_STRUCTURAL_EQUALITY (type);
    }
  else
    {
      type = non_reference (unlowered_expr_type (expr));

      if (!is_this
	  && (by_reference_p || TREE_CODE (type) == FUNCTION_TYPE))
	type = build_reference_type (type);
    }

  return type;
}

/* Returns true iff DECL is a lambda capture proxy variable created by
   build_capture_proxy.  */

bool
is_capture_proxy (tree decl)
{
  return (VAR_P (decl)
	  && DECL_HAS_VALUE_EXPR_P (decl)
	  && !DECL_ANON_UNION_VAR_P (decl)
	  && !DECL_DECOMPOSITION_P (decl)
	  && !DECL_FNAME_P (decl)
	  && !(DECL_ARTIFICIAL (decl)
	       && DECL_LANG_SPECIFIC (decl)
	       && DECL_OMP_PRIVATIZED_MEMBER (decl))
	  && LAMBDA_FUNCTION_P (DECL_CONTEXT (decl)));
}

/* Returns true iff DECL is a capture proxy for a normal capture
   (i.e. without explicit initializer).  */

bool
is_normal_capture_proxy (tree decl)
{
  if (!is_capture_proxy (decl))
    /* It's not a capture proxy.  */
    return false;

  return (DECL_LANG_SPECIFIC (decl)
	  && DECL_CAPTURED_VARIABLE (decl));
}

/* Returns true iff DECL is a capture proxy for a normal capture
   of a constant variable.  */

bool
is_constant_capture_proxy (tree decl)
{
  if (is_normal_capture_proxy (decl))
    return decl_constant_var_p (DECL_CAPTURED_VARIABLE (decl));
  return false;
}

/* VAR is a capture proxy created by build_capture_proxy; add it to the
   current function, which is the operator() for the appropriate lambda.  */

void
insert_capture_proxy (tree var)
{
  if (is_normal_capture_proxy (var))
    {
      tree cap = DECL_CAPTURED_VARIABLE (var);
      if (CHECKING_P)
	{
	  gcc_assert (!is_normal_capture_proxy (cap));
	  tree old = retrieve_local_specialization (cap);
	  if (old)
	    gcc_assert (DECL_CONTEXT (old) != DECL_CONTEXT (var));
	}
      register_local_specialization (var, cap);
    }

  /* Put the capture proxy in the extra body block so that it won't clash
     with a later local variable.  */
  pushdecl_outermost_localscope (var);

  /* And put a DECL_EXPR in the STATEMENT_LIST for the same block.  */
  var = build_stmt (DECL_SOURCE_LOCATION (var), DECL_EXPR, var);
  tree stmt_list = (*stmt_list_stack)[1];
  gcc_assert (stmt_list);
  append_to_statement_list_force (var, &stmt_list);
}

/* We've just finished processing a lambda; if the containing scope is also
   a lambda, insert any capture proxies that were created while processing
   the nested lambda.  */

void
insert_pending_capture_proxies (void)
{
  tree lam;
  vec<tree, va_gc> *proxies;
  unsigned i;

  if (!current_function_decl || !LAMBDA_FUNCTION_P (current_function_decl))
    return;

  lam = CLASSTYPE_LAMBDA_EXPR (DECL_CONTEXT (current_function_decl));
  proxies = LAMBDA_EXPR_PENDING_PROXIES (lam);
  for (i = 0; i < vec_safe_length (proxies); ++i)
    {
      tree var = (*proxies)[i];
      insert_capture_proxy (var);
    }
  release_tree_vector (LAMBDA_EXPR_PENDING_PROXIES (lam));
  LAMBDA_EXPR_PENDING_PROXIES (lam) = NULL;
}

/* Given REF, a COMPONENT_REF designating a field in the lambda closure,
   return the type we want the proxy to have: the type of the field itself,
   with added const-qualification if the lambda isn't mutable and the
   capture is by value.  */

tree
lambda_proxy_type (tree ref)
{
  tree type;
  if (ref == error_mark_node)
    return error_mark_node;
  if (REFERENCE_REF_P (ref))
    ref = TREE_OPERAND (ref, 0);
  gcc_assert (TREE_CODE (ref) == COMPONENT_REF);
  type = TREE_TYPE (ref);
  if (!type || WILDCARD_TYPE_P (non_reference (type)))
    {
      type = cxx_make_type (DECLTYPE_TYPE);
      DECLTYPE_TYPE_EXPR (type) = ref;
      DECLTYPE_FOR_LAMBDA_PROXY (type) = true;
      SET_TYPE_STRUCTURAL_EQUALITY (type);
    }
  if (DECL_PACK_P (TREE_OPERAND (ref, 1)))
    type = make_pack_expansion (type);
  return type;
}

/* MEMBER is a capture field in a lambda closure class.  Now that we're
   inside the operator(), build a placeholder var for future lookups and
   debugging.  */

static tree
build_capture_proxy (tree member, tree init)
{
  tree var, object, fn, closure, name, lam, type;

  if (PACK_EXPANSION_P (member))
    member = PACK_EXPANSION_PATTERN (member);

  closure = DECL_CONTEXT (member);
  fn = lambda_function (closure);
  lam = CLASSTYPE_LAMBDA_EXPR (closure);

  /* The proxy variable forwards to the capture field.  */
  object = build_fold_indirect_ref (DECL_ARGUMENTS (fn));
  object = finish_non_static_data_member (member, object, NULL_TREE);
  if (REFERENCE_REF_P (object))
    object = TREE_OPERAND (object, 0);

  /* Remove the __ inserted by add_capture.  */
  name = get_identifier (IDENTIFIER_POINTER (DECL_NAME (member)) + 2);

  type = lambda_proxy_type (object);

  if (name == this_identifier && !INDIRECT_TYPE_P (type))
    {
      type = build_pointer_type (type);
      type = cp_build_qualified_type (type, TYPE_QUAL_CONST);
      object = build_fold_addr_expr_with_type (object, type);
    }

  if (DECL_VLA_CAPTURE_P (member))
    {
      /* Rebuild the VLA type from the pointer and maxindex.  */
      tree field = next_initializable_field (TYPE_FIELDS (type));
      tree ptr = build_simple_component_ref (object, field);
      field = next_initializable_field (DECL_CHAIN (field));
      tree max = build_simple_component_ref (object, field);
      type = build_cplus_array_type (TREE_TYPE (TREE_TYPE (ptr)),
				     build_index_type (max));
      type = build_reference_type (type);
      object = convert (type, ptr);
    }

  complete_type (type);

  var = build_decl (input_location, VAR_DECL, name, type);
  SET_DECL_VALUE_EXPR (var, object);
  DECL_HAS_VALUE_EXPR_P (var) = 1;
  DECL_ARTIFICIAL (var) = 1;
  TREE_USED (var) = 1;
  DECL_CONTEXT (var) = fn;

  if (DECL_NORMAL_CAPTURE_P (member))
    {
      if (DECL_VLA_CAPTURE_P (member))
	{
	  init = CONSTRUCTOR_ELT (init, 0)->value;
	  init = TREE_OPERAND (init, 0); // Strip ADDR_EXPR.
	  init = TREE_OPERAND (init, 0); // Strip ARRAY_REF.
	}
      else
	{
	  if (PACK_EXPANSION_P (init))
	    init = PACK_EXPANSION_PATTERN (init);
	}

      if (INDIRECT_REF_P (init))
	init = TREE_OPERAND (init, 0);
      STRIP_NOPS (init);

      gcc_assert (VAR_P (init) || TREE_CODE (init) == PARM_DECL);
      while (is_normal_capture_proxy (init))
	init = DECL_CAPTURED_VARIABLE (init);
      retrofit_lang_decl (var);
      DECL_CAPTURED_VARIABLE (var) = init;
    }

  if (name == this_identifier)
    {
      gcc_assert (LAMBDA_EXPR_THIS_CAPTURE (lam) == member);
      LAMBDA_EXPR_THIS_CAPTURE (lam) = var;
    }

  if (fn == current_function_decl)
    insert_capture_proxy (var);
  else
    vec_safe_push (LAMBDA_EXPR_PENDING_PROXIES (lam), var);

  return var;
}

static GTY(()) tree ptr_id;
static GTY(()) tree max_id;

/* Return a struct containing a pointer and a length for lambda capture of
   an array of runtime length.  */

static tree
vla_capture_type (tree array_type)
{
  tree type = xref_tag (record_type, make_anon_name (), ts_current, false);
  xref_basetypes (type, NULL_TREE);
  type = begin_class_definition (type);
  if (!ptr_id)
    {
      ptr_id = get_identifier ("ptr");
      max_id = get_identifier ("max");
    }
  tree ptrtype = build_pointer_type (TREE_TYPE (array_type));
  tree field = build_decl (input_location, FIELD_DECL, ptr_id, ptrtype);
  finish_member_declaration (field);
  field = build_decl (input_location, FIELD_DECL, max_id, sizetype);
  finish_member_declaration (field);
  return finish_struct (type, NULL_TREE);
}

/* From an ID and INITIALIZER, create a capture (by reference if
   BY_REFERENCE_P is true), add it to the capture-list for LAMBDA,
   and return it.  If ID is `this', BY_REFERENCE_P says whether
   `*this' is captured by reference.  */

tree
add_capture (tree lambda, tree id, tree orig_init, bool by_reference_p,
	     bool explicit_init_p)
{
  char *buf;
  tree type, member, name;
  bool vla = false;
  bool variadic = false;
  tree initializer = orig_init;

  if (PACK_EXPANSION_P (initializer))
    {
      initializer = PACK_EXPANSION_PATTERN (initializer);
      variadic = true;
    }

  if (TREE_CODE (initializer) == TREE_LIST
      /* A pack expansion might end up with multiple elements.  */
      && !PACK_EXPANSION_P (TREE_VALUE (initializer)))
    initializer = build_x_compound_expr_from_list (initializer, ELK_INIT,
						   tf_warning_or_error);
  type = TREE_TYPE (initializer);
  if (type == error_mark_node)
    return error_mark_node;

  if (!dependent_type_p (type) && array_of_runtime_bound_p (type))
    {
      vla = true;
      if (!by_reference_p)
	error ("array of runtime bound cannot be captured by copy, "
	       "only by reference");

      /* For a VLA, we capture the address of the first element and the
	 maximum index, and then reconstruct the VLA for the proxy.  */
      tree elt = cp_build_array_ref (input_location, initializer,
				     integer_zero_node, tf_warning_or_error);
      initializer = build_constructor_va (init_list_type_node, 2,
					  NULL_TREE, build_address (elt),
					  NULL_TREE, array_type_nelts (type));
      type = vla_capture_type (type);
    }
  else if (!dependent_type_p (type)
	   && variably_modified_type_p (type, NULL_TREE))
    {
      sorry ("capture of variably-modified type %qT that is not an N3639 array "
	     "of runtime bound", type);
      if (TREE_CODE (type) == ARRAY_TYPE
	  && variably_modified_type_p (TREE_TYPE (type), NULL_TREE))
	inform (input_location, "because the array element type %qT has "
		"variable size", TREE_TYPE (type));
      return error_mark_node;
    }
  else
    {
      type = lambda_capture_field_type (initializer, explicit_init_p,
					by_reference_p);
      if (type == error_mark_node)
	return error_mark_node;

      if (id == this_identifier && !by_reference_p)
	{
	  gcc_assert (INDIRECT_TYPE_P (type));
	  type = TREE_TYPE (type);
	  initializer = cp_build_fold_indirect_ref (initializer);
	}

      if (dependent_type_p (type))
	;
      else if (id != this_identifier && by_reference_p)
	{
	  if (!lvalue_p (initializer))
	    {
	      error ("cannot capture %qE by reference", initializer);
	      return error_mark_node;
	    }
	}
      else
	{
	  /* Capture by copy requires a complete type.  */
	  type = complete_type (type);
	  if (!COMPLETE_TYPE_P (type))
	    {
	      error ("capture by copy of incomplete type %qT", type);
	      cxx_incomplete_type_inform (type);
	      return error_mark_node;
	    }
	}
    }

  /* Add __ to the beginning of the field name so that user code
     won't find the field with name lookup.  We can't just leave the name
     unset because template instantiation uses the name to find
     instantiated fields.  */
  buf = (char *) alloca (IDENTIFIER_LENGTH (id) + 3);
  buf[1] = buf[0] = '_';
  memcpy (buf + 2, IDENTIFIER_POINTER (id),
	  IDENTIFIER_LENGTH (id) + 1);
  name = get_identifier (buf);

  if (variadic)
    {
      type = make_pack_expansion (type);
      if (explicit_init_p)
	/* With an explicit initializer 'type' is auto, which isn't really a
	   parameter pack in this context.  We will want as many fields as we
	   have elements in the expansion of the initializer, so use its packs
	   instead.  */
	PACK_EXPANSION_PARAMETER_PACKS (type)
	  = uses_parameter_packs (initializer);
    }

  /* Make member variable.  */
  member = build_decl (input_location, FIELD_DECL, name, type);
  DECL_VLA_CAPTURE_P (member) = vla;

  if (!explicit_init_p)
    /* Normal captures are invisible to name lookup but uses are replaced
       with references to the capture field; we implement this by only
       really making them invisible in unevaluated context; see
       qualify_lookup.  For now, let's make explicitly initialized captures
       always visible.  */
    DECL_NORMAL_CAPTURE_P (member) = true;

  if (id == this_identifier)
    LAMBDA_EXPR_THIS_CAPTURE (lambda) = member;

  /* Add it to the appropriate closure class if we've started it.  */
  if (current_class_type
      && current_class_type == LAMBDA_EXPR_CLOSURE (lambda))
    {
      if (COMPLETE_TYPE_P (current_class_type))
	internal_error ("trying to capture %qD in instantiation of "
			"generic lambda", id);
      finish_member_declaration (member);
    }

  tree listmem = member;
  if (variadic)
    {
      listmem = make_pack_expansion (member);
      initializer = orig_init;
    }
  LAMBDA_EXPR_CAPTURE_LIST (lambda)
    = tree_cons (listmem, initializer, LAMBDA_EXPR_CAPTURE_LIST (lambda));

  if (LAMBDA_EXPR_CLOSURE (lambda))
    return build_capture_proxy (member, initializer);
  /* For explicit captures we haven't started the function yet, so we wait
     and build the proxy from cp_parser_lambda_body.  */
  LAMBDA_CAPTURE_EXPLICIT_P (LAMBDA_EXPR_CAPTURE_LIST (lambda)) = true;
  return NULL_TREE;
}

/* Register all the capture members on the list CAPTURES, which is the
   LAMBDA_EXPR_CAPTURE_LIST for the lambda after the introducer.  */

void
register_capture_members (tree captures)
{
  if (captures == NULL_TREE)
    return;

  register_capture_members (TREE_CHAIN (captures));

  tree field = TREE_PURPOSE (captures);
  if (PACK_EXPANSION_P (field))
    field = PACK_EXPANSION_PATTERN (field);

  finish_member_declaration (field);
}

/* Similar to add_capture, except this works on a stack of nested lambdas.
   BY_REFERENCE_P in this case is derived from the default capture mode.
   Returns the capture for the lambda at the bottom of the stack.  */

tree
add_default_capture (tree lambda_stack, tree id, tree initializer)
{
  bool this_capture_p = (id == this_identifier);
  tree var = NULL_TREE;
  tree saved_class_type = current_class_type;

  for (tree node = lambda_stack;
       node;
       node = TREE_CHAIN (node))
    {
      tree lambda = TREE_VALUE (node);

      current_class_type = LAMBDA_EXPR_CLOSURE (lambda);
      if (DECL_PACK_P (initializer))
	initializer = make_pack_expansion (initializer);
      var = add_capture (lambda,
                            id,
                            initializer,
                            /*by_reference_p=*/
			    (this_capture_p
			     || (LAMBDA_EXPR_DEFAULT_CAPTURE_MODE (lambda)
				 == CPLD_REFERENCE)),
			    /*explicit_init_p=*/false);
      initializer = convert_from_reference (var);

      /* Warn about deprecated implicit capture of this via [=].  */
      if (cxx_dialect >= cxx2a
	  && this_capture_p
	  && LAMBDA_EXPR_DEFAULT_CAPTURE_MODE (lambda) == CPLD_COPY)
	{
	  if (warning_at (LAMBDA_EXPR_LOCATION (lambda), OPT_Wdeprecated,
			  "implicit capture of %qE via %<[=]%> is deprecated "
			  "in C++20", this_identifier))
	    inform (LAMBDA_EXPR_LOCATION (lambda), "add explicit %<this%> or "
		    "%<*this%> capture");
	}
    }

  current_class_type = saved_class_type;

  return var;
}

/* Return the capture pertaining to a use of 'this' in LAMBDA, in the
   form of an INDIRECT_REF, possibly adding it through default
   capturing, if ADD_CAPTURE_P is nonzero.  If ADD_CAPTURE_P is negative,
   try to capture but don't complain if we can't.  */

tree
lambda_expr_this_capture (tree lambda, int add_capture_p)
{
  tree result;

  tree this_capture = LAMBDA_EXPR_THIS_CAPTURE (lambda);

  /* In unevaluated context this isn't an odr-use, so don't capture.  */
  if (cp_unevaluated_operand)
    add_capture_p = false;

  /* Try to default capture 'this' if we can.  */
  if (!this_capture)
    {
      tree lambda_stack = NULL_TREE;
      tree init = NULL_TREE;

      /* If we are in a lambda function, we can move out until we hit:
           1. a non-lambda function or NSDMI,
           2. a lambda function capturing 'this', or
           3. a non-default capturing lambda function.  */
      for (tree tlambda = lambda; ;)
	{
	  if (add_capture_p
	      && LAMBDA_EXPR_DEFAULT_CAPTURE_MODE (tlambda) == CPLD_NONE)
	    /* tlambda won't let us capture 'this'.  */
	    break;

	  if (add_capture_p)
	    lambda_stack = tree_cons (NULL_TREE,
				      tlambda,
				      lambda_stack);

	  tree closure = LAMBDA_EXPR_CLOSURE (tlambda);
	  tree containing_function
	    = decl_function_context (TYPE_NAME (closure));

	  tree ex = LAMBDA_EXPR_EXTRA_SCOPE (tlambda);
	  if (ex && TREE_CODE (ex) == FIELD_DECL)
	    {
	      /* Lambda in an NSDMI.  We don't have a function to look up
		 'this' in, but we can find (or rebuild) the fake one from
		 inject_this_parameter.  */
	      if (!containing_function && !COMPLETE_TYPE_P (closure))
		/* If we're parsing a lambda in a non-local class,
		   we can find the fake 'this' in scope_chain.  */
		init = scope_chain->x_current_class_ptr;
	      else
		/* Otherwise it's either gone or buried in
		   function_context_stack, so make another.  */
		init = build_this_parm (NULL_TREE, DECL_CONTEXT (ex),
					TYPE_UNQUALIFIED);
	      gcc_checking_assert
		(init && (TREE_TYPE (TREE_TYPE (init))
			  == current_nonlambda_class_type ()));
	      break;
	    }

	  if (containing_function == NULL_TREE)
	    /* We ran out of scopes; there's no 'this' to capture.  */
	    break;

	  if (!LAMBDA_FUNCTION_P (containing_function))
	    {
	      /* We found a non-lambda function.  */
	      if (DECL_NONSTATIC_MEMBER_FUNCTION_P (containing_function))
		/* First parameter is 'this'.  */
		init = DECL_ARGUMENTS (containing_function);
	      break;
	    }

	  tlambda
            = CLASSTYPE_LAMBDA_EXPR (DECL_CONTEXT (containing_function));

          if (LAMBDA_EXPR_THIS_CAPTURE (tlambda))
	    {
	      /* An outer lambda has already captured 'this'.  */
	      init = LAMBDA_EXPR_THIS_CAPTURE (tlambda);
	      break;
	    }
	}

      if (init)
        {
          if (add_capture_p)
	    this_capture = add_default_capture (lambda_stack,
					        /*id=*/this_identifier,
					        init);
          else
	    this_capture = init;
        }
    }

  if (cp_unevaluated_operand)
    result = this_capture;
  else if (!this_capture)
    {
      if (add_capture_p == 1)
	{
	  error ("%<this%> was not captured for this lambda function");
	  result = error_mark_node;
	}
      else
	result = NULL_TREE;
    }
  else
    {
      /* To make sure that current_class_ref is for the lambda.  */
      gcc_assert (TYPE_MAIN_VARIANT (TREE_TYPE (current_class_ref))
		  == LAMBDA_EXPR_CLOSURE (lambda));

      result = this_capture;

      /* If 'this' is captured, each use of 'this' is transformed into an
	 access to the corresponding unnamed data member of the closure
	 type cast (_expr.cast_ 5.4) to the type of 'this'. [ The cast
	 ensures that the transformed expression is an rvalue. ] */
      result = rvalue (result);
    }

  return result;
}

/* Return the innermost LAMBDA_EXPR we're currently in, if any.  */

tree
current_lambda_expr (void)
{
  tree type = current_class_type;
  while (type && !LAMBDA_TYPE_P (type))
    type = decl_type_context (TYPE_NAME (type));
  if (type)
    return CLASSTYPE_LAMBDA_EXPR (type);
  else
    return NULL_TREE;
}

/* Return the current LAMBDA_EXPR, if this is a resolvable dummy
   object.  NULL otherwise..  */

static tree
resolvable_dummy_lambda (tree object)
{
  if (!is_dummy_object (object))
    return NULL_TREE;

  tree type = TYPE_MAIN_VARIANT (TREE_TYPE (object));
  gcc_assert (!TYPE_PTR_P (type));

  if (type != current_class_type
      && current_class_type
      && LAMBDA_TYPE_P (current_class_type)
      && lambda_function (current_class_type)
      && DERIVED_FROM_P (type, nonlambda_method_basetype()))
    return CLASSTYPE_LAMBDA_EXPR (current_class_type);

  return NULL_TREE;
}

/* We don't want to capture 'this' until we know we need it, i.e. after
   overload resolution has chosen a non-static member function.  At that
   point we call this function to turn a dummy object into a use of the
   'this' capture.  */

tree
maybe_resolve_dummy (tree object, bool add_capture_p)
{
  if (tree lam = resolvable_dummy_lambda (object))
    if (tree cap = lambda_expr_this_capture (lam, add_capture_p))
      if (cap != error_mark_node)
	object = build_fold_indirect_ref (cap);

  return object;
}

/* When parsing a generic lambda containing an argument-dependent
   member function call we defer overload resolution to instantiation
   time.  But we have to know now whether to capture this or not.
   Do that if FNS contains any non-static fns.
   The std doesn't anticipate this case, but I expect this to be the
   outcome of discussion.  */

void
maybe_generic_this_capture (tree object, tree fns)
{
  if (tree lam = resolvable_dummy_lambda (object))
    if (!LAMBDA_EXPR_THIS_CAPTURE (lam))
      {
	/* We've not yet captured, so look at the function set of
	   interest.  */
	if (BASELINK_P (fns))
	  fns = BASELINK_FUNCTIONS (fns);
	bool id_expr = TREE_CODE (fns) == TEMPLATE_ID_EXPR;
	if (id_expr)
	  fns = TREE_OPERAND (fns, 0);

	for (lkp_iterator iter (fns); iter; ++iter)
	  if (((!id_expr && TREE_CODE (*iter) != USING_DECL)
	       || TREE_CODE (*iter) == TEMPLATE_DECL)
	      && DECL_NONSTATIC_MEMBER_FUNCTION_P (*iter))
	    {
	      /* Found a non-static member.  Capture this.  */
	      lambda_expr_this_capture (lam, /*maybe*/-1);
	      break;
	    }
      }
}

/* Returns the innermost non-lambda function.  */

tree
current_nonlambda_function (void)
{
  tree fn = current_function_decl;
  while (fn && LAMBDA_FUNCTION_P (fn))
    fn = decl_function_context (fn);
  return fn;
}

/* Returns the method basetype of the innermost non-lambda function, including
   a hypothetical constructor if inside an NSDMI, or NULL_TREE if none.  */

tree
nonlambda_method_basetype (void)
{
  if (!current_class_ref)
    return NULL_TREE;

  tree type = current_class_type;
  if (!type || !LAMBDA_TYPE_P (type))
    return type;

  while (true)
    {
      tree lam = CLASSTYPE_LAMBDA_EXPR (type);
      tree ex = LAMBDA_EXPR_EXTRA_SCOPE (lam);
      if (ex && TREE_CODE (ex) == FIELD_DECL)
	/* Lambda in an NSDMI.  */
	return DECL_CONTEXT (ex);

      tree fn = TYPE_CONTEXT (type);
      if (!fn || TREE_CODE (fn) != FUNCTION_DECL
	  || !DECL_NONSTATIC_MEMBER_FUNCTION_P (fn))
	/* No enclosing non-lambda method.  */
	return NULL_TREE;
      if (!LAMBDA_FUNCTION_P (fn))
	/* Found an enclosing non-lambda method.  */
	return TYPE_METHOD_BASETYPE (TREE_TYPE (fn));
      type = DECL_CONTEXT (fn);
    }
}

/* Like current_scope, but looking through lambdas.  */

tree
current_nonlambda_scope (void)
{
  tree scope = current_scope ();
  for (;;)
    {
      if (TREE_CODE (scope) == FUNCTION_DECL
	  && LAMBDA_FUNCTION_P (scope))
	{
	  scope = CP_TYPE_CONTEXT (DECL_CONTEXT (scope));
	  continue;
	}
      else if (LAMBDA_TYPE_P (scope))
	{
	  scope = CP_TYPE_CONTEXT (scope);
	  continue;
	}
      break;
    }
  return scope;
}

/* Helper function for maybe_add_lambda_conv_op; build a CALL_EXPR with
   indicated FN and NARGS, but do not initialize the return type or any of the
   argument slots.  */

static tree
prepare_op_call (tree fn, int nargs)
{
  tree t;

  t = build_vl_exp (CALL_EXPR, nargs + 3);
  CALL_EXPR_FN (t) = fn;
  CALL_EXPR_STATIC_CHAIN (t) = NULL;

  return t;
}

/* Return true iff CALLOP is the op() for a generic lambda.  */

bool
generic_lambda_fn_p (tree callop)
{
  return (LAMBDA_FUNCTION_P (callop)
	  && DECL_TEMPLATE_INFO (callop)
	  && PRIMARY_TEMPLATE_P (DECL_TI_TEMPLATE (callop)));
}

/* If the closure TYPE has a static op(), also add a conversion to function
   pointer.  */

void
maybe_add_lambda_conv_op (tree type)
{
  bool nested = (cfun != NULL);
  bool nested_def = decl_function_context (TYPE_MAIN_DECL (type));
  tree callop = lambda_function (type);
  tree lam = CLASSTYPE_LAMBDA_EXPR (type);

  if (LAMBDA_EXPR_CAPTURE_LIST (lam) != NULL_TREE
      || LAMBDA_EXPR_DEFAULT_CAPTURE_MODE (lam) != CPLD_NONE)
    return;

  if (processing_template_decl)
    return;

  bool const generic_lambda_p = generic_lambda_fn_p (callop);

  if (!generic_lambda_p && undeduced_auto_decl (callop))
    {
      /* If the op() wasn't deduced due to errors, give up.  */
      gcc_assert (errorcount || sorrycount);
      return;
    }

  /* Non-generic non-capturing lambdas only have a conversion function to
     pointer to function when the trailing requires-clause's constraints are
     satisfied.  */
  if (!generic_lambda_p && !constraints_satisfied_p (callop))
    return;

  /* Non-template conversion operators are defined directly with build_call_a
     and using DIRECT_ARGVEC for arguments (including 'this').  Templates are
     deferred and the CALL is built in-place.  In the case of a deduced return
     call op, the decltype expression, DECLTYPE_CALL, used as a substitute for
     the return type is also built in-place.  The arguments of DECLTYPE_CALL in
     the return expression may differ in flags from those in the body CALL.  In
     particular, parameter pack expansions are marked PACK_EXPANSION_LOCAL_P in
     the body CALL, but not in DECLTYPE_CALL.  */

  vec<tree, va_gc> *direct_argvec = 0;
  tree decltype_call = 0, call = 0;
  tree optype = TREE_TYPE (callop);
  tree fn_result = TREE_TYPE (optype);

  tree thisarg = build_int_cst (TREE_TYPE (DECL_ARGUMENTS (callop)), 0);
  if (generic_lambda_p)
    {
      ++processing_template_decl;

      /* Prepare the dependent member call for the static member function
	 '_FUN' and, potentially, prepare another call to be used in a decltype
	 return expression for a deduced return call op to allow for simple
	 implementation of the conversion operator.  */

      tree instance = cp_build_fold_indirect_ref (thisarg);
      tree objfn = lookup_template_function (DECL_NAME (callop),
					     DECL_TI_ARGS (callop));
      objfn = build_min (COMPONENT_REF, NULL_TREE,
			 instance, objfn, NULL_TREE);
      int nargs = list_length (DECL_ARGUMENTS (callop)) - 1;

      call = prepare_op_call (objfn, nargs);
      if (type_uses_auto (fn_result))
	decltype_call = prepare_op_call (objfn, nargs);
    }
  else
    {
      direct_argvec = make_tree_vector ();
      direct_argvec->quick_push (thisarg);
    }

  /* Copy CALLOP's argument list (as per 'copy_list') as FN_ARGS in order to
     declare the static member function "_FUN" below.  For each arg append to
     DIRECT_ARGVEC (for the non-template case) or populate the pre-allocated
     call args (for the template case).  If a parameter pack is found, expand
     it, flagging it as PACK_EXPANSION_LOCAL_P for the body call.  */

  tree fn_args = NULL_TREE;
  {
    int ix = 0;
    tree src = DECL_CHAIN (DECL_ARGUMENTS (callop));
    tree tgt = NULL;

    while (src)
      {
	tree new_node = copy_node (src);

	/* Clear TREE_ADDRESSABLE on thunk arguments.  */
	TREE_ADDRESSABLE (new_node) = 0;

	if (!fn_args)
	  fn_args = tgt = new_node;
	else
	  {
	    TREE_CHAIN (tgt) = new_node;
	    tgt = new_node;
	  }

	mark_exp_read (tgt);

	if (generic_lambda_p)
	  {
	    tree a = tgt;
	    if (DECL_PACK_P (tgt))
	      {
		a = make_pack_expansion (a);
		PACK_EXPANSION_LOCAL_P (a) = true;
	      }
	    CALL_EXPR_ARG (call, ix) = a;

	    if (decltype_call)
	      {
		/* Avoid capturing variables in this context.  */
		++cp_unevaluated_operand;
		CALL_EXPR_ARG (decltype_call, ix) = forward_parm (tgt);
		--cp_unevaluated_operand;
	      }

	    ++ix;
	  }
	else
	  vec_safe_push (direct_argvec, tgt);

	src = TREE_CHAIN (src);
      }
  }

  if (generic_lambda_p)
    {
      if (decltype_call)
	{
	  fn_result = finish_decltype_type
	    (decltype_call, /*id_expression_or_member_access_p=*/false,
	     tf_warning_or_error);
	}
    }
  else
    call = build_call_a (callop,
			 direct_argvec->length (),
			 direct_argvec->address ());

  CALL_FROM_THUNK_P (call) = 1;
  SET_EXPR_LOCATION (call, UNKNOWN_LOCATION);

  tree stattype = build_function_type (fn_result, FUNCTION_ARG_CHAIN (callop));
  stattype = (cp_build_type_attribute_variant
	      (stattype, TYPE_ATTRIBUTES (optype)));
  if (flag_noexcept_type
      && TYPE_NOTHROW_P (TREE_TYPE (callop)))
    stattype = build_exception_variant (stattype, noexcept_true_spec);

  if (generic_lambda_p)
    --processing_template_decl;

  /* First build up the conversion op.  */

  tree rettype = build_pointer_type (stattype);
  tree name = make_conv_op_name (rettype);
  tree thistype = cp_build_qualified_type (type, TYPE_QUAL_CONST);
  tree fntype = build_method_type_directly (thistype, rettype, void_list_node);
  tree convfn = build_lang_decl (FUNCTION_DECL, name, fntype);
  SET_DECL_LANGUAGE (convfn, lang_cplusplus);
  tree fn = convfn;
  DECL_SOURCE_LOCATION (fn) = DECL_SOURCE_LOCATION (callop);
  SET_DECL_ALIGN (fn, MINIMUM_METHOD_BOUNDARY);
  grokclassfn (type, fn, NO_SPECIAL);
  set_linkage_according_to_type (type, fn);
  rest_of_decl_compilation (fn, namespace_bindings_p (), at_eof);
  DECL_IN_AGGR_P (fn) = 1;
  DECL_ARTIFICIAL (fn) = 1;
  DECL_NOT_REALLY_EXTERN (fn) = 1;
  DECL_DECLARED_INLINE_P (fn) = 1;
  DECL_DECLARED_CONSTEXPR_P (fn) = DECL_DECLARED_CONSTEXPR_P (callop);
  if (DECL_IMMEDIATE_FUNCTION_P (callop))
    SET_DECL_IMMEDIATE_FUNCTION_P (fn);
  DECL_ARGUMENTS (fn) = build_this_parm (fn, fntype, TYPE_QUAL_CONST);

  if (nested_def)
    DECL_INTERFACE_KNOWN (fn) = 1;

  if (generic_lambda_p)
    fn = add_inherited_template_parms (fn, DECL_TI_TEMPLATE (callop));

  add_method (type, fn, false);

  /* Generic thunk code fails for varargs; we'll complain in mark_used if
     the conversion op is used.  */
  if (varargs_function_p (callop))
    {
      DECL_DELETED_FN (fn) = 1;
      return;
    }

  /* Now build up the thunk to be returned.  */

  tree statfn = build_lang_decl (FUNCTION_DECL, fun_identifier, stattype);
  SET_DECL_LANGUAGE (statfn, lang_cplusplus);
  fn = statfn;
  DECL_SOURCE_LOCATION (fn) = DECL_SOURCE_LOCATION (callop);
  grokclassfn (type, fn, NO_SPECIAL);
  set_linkage_according_to_type (type, fn);
  rest_of_decl_compilation (fn, namespace_bindings_p (), at_eof);
  DECL_IN_AGGR_P (fn) = 1;
  DECL_ARTIFICIAL (fn) = 1;
  DECL_NOT_REALLY_EXTERN (fn) = 1;
  DECL_DECLARED_INLINE_P (fn) = 1;
  DECL_STATIC_FUNCTION_P (fn) = 1;
  DECL_DECLARED_CONSTEXPR_P (fn) = DECL_DECLARED_CONSTEXPR_P (callop);
  if (DECL_IMMEDIATE_FUNCTION_P (callop))
    SET_DECL_IMMEDIATE_FUNCTION_P (fn);
  DECL_ARGUMENTS (fn) = fn_args;
  for (tree arg = fn_args; arg; arg = DECL_CHAIN (arg))
    {
      /* Avoid duplicate -Wshadow warnings.  */
      DECL_NAME (arg) = NULL_TREE;
      DECL_CONTEXT (arg) = fn;
    }
  if (nested_def)
    DECL_INTERFACE_KNOWN (fn) = 1;

  if (generic_lambda_p)
    fn = add_inherited_template_parms (fn, DECL_TI_TEMPLATE (callop));

  if (flag_sanitize & SANITIZE_NULL)
    /* Don't UBsan this function; we're deliberately calling op() with a null
       object argument.  */
    add_no_sanitize_value (fn, SANITIZE_UNDEFINED);

  add_method (type, fn, false);

  if (nested)
    push_function_context ();
  else
    /* Still increment function_depth so that we don't GC in the
       middle of an expression.  */
    ++function_depth;

  /* Generate the body of the thunk.  */

  start_preparsed_function (statfn, NULL_TREE,
			    SF_PRE_PARSED | SF_INCLASS_INLINE);
  tree body = begin_function_body ();
  tree compound_stmt = begin_compound_stmt (0);
  if (!generic_lambda_p)
    {
      set_flags_from_callee (call);
      if (MAYBE_CLASS_TYPE_P (TREE_TYPE (call)))
	call = build_cplus_new (TREE_TYPE (call), call, tf_warning_or_error);
    }
  call = convert_from_reference (call);
  finish_return_stmt (call);

  finish_compound_stmt (compound_stmt);
  finish_function_body (body);

  fn = finish_function (/*inline_p=*/true);
  if (!generic_lambda_p)
    expand_or_defer_fn (fn);

  /* Generate the body of the conversion op.  */

  start_preparsed_function (convfn, NULL_TREE,
			    SF_PRE_PARSED | SF_INCLASS_INLINE);
  body = begin_function_body ();
  compound_stmt = begin_compound_stmt (0);

  /* decl_needed_p needs to see that it's used.  */
  TREE_USED (statfn) = 1;
  finish_return_stmt (decay_conversion (statfn, tf_warning_or_error));

  finish_compound_stmt (compound_stmt);
  finish_function_body (body);

  fn = finish_function (/*inline_p=*/true);
  if (!generic_lambda_p)
    expand_or_defer_fn (fn);

  if (nested)
    pop_function_context ();
  else
    --function_depth;
}

/* True if FN is the static function "_FUN" that gets returned from the lambda
   conversion operator.  */

bool
lambda_static_thunk_p (tree fn)
{
  return (fn && TREE_CODE (fn) == FUNCTION_DECL
	  && DECL_ARTIFICIAL (fn)
	  && DECL_STATIC_FUNCTION_P (fn)
	  && LAMBDA_TYPE_P (CP_DECL_CONTEXT (fn)));
}

/* Returns true iff VAL is a lambda-related declaration which should
   be ignored by unqualified lookup.  */

bool
is_lambda_ignored_entity (tree val)
{
  /* Look past normal capture proxies.  */
  if (is_normal_capture_proxy (val))
    return true;

  /* Always ignore lambda fields, their names are only for debugging.  */
  if (TREE_CODE (val) == FIELD_DECL
      && CLASSTYPE_LAMBDA_EXPR (DECL_CONTEXT (val)))
    return true;

  /* None of the lookups that use qualify_lookup want the op() from the
     lambda; they want the one from the enclosing class.  */
  if (TREE_CODE (val) == FUNCTION_DECL && LAMBDA_FUNCTION_P (val))
    return true;

  return false;
}

/* Lambdas that appear in variable initializer or default argument scope
   get that in their mangling, so we need to record it.  We might as well
   use the count for function and namespace scopes as well.  */
static GTY(()) tree lambda_scope;
static GTY(()) int lambda_count;
struct GTY(()) tree_int
{
  tree t;
  int i;
};
static GTY(()) vec<tree_int, va_gc> *lambda_scope_stack;

void
start_lambda_scope (tree decl)
{
  tree_int ti;
  gcc_assert (decl);
  /* Once we're inside a function, we ignore variable scope and just push
     the function again so that popping works properly.  */
  if (current_function_decl && TREE_CODE (decl) == VAR_DECL)
    decl = current_function_decl;
  ti.t = lambda_scope;
  ti.i = lambda_count;
  vec_safe_push (lambda_scope_stack, ti);
  if (lambda_scope != decl)
    {
      /* Don't reset the count if we're still in the same function.  */
      lambda_scope = decl;
      lambda_count = 0;
    }
}

void
record_lambda_scope (tree lambda)
{
  LAMBDA_EXPR_EXTRA_SCOPE (lambda) = lambda_scope;
  LAMBDA_EXPR_DISCRIMINATOR (lambda) = lambda_count++;
}

/* This lambda is an instantiation of a lambda in a template default argument
   that got no LAMBDA_EXPR_EXTRA_SCOPE, so this shouldn't either.  But we do
   need to use and increment the global count to avoid collisions.  */

void
record_null_lambda_scope (tree lambda)
{
  if (vec_safe_is_empty (lambda_scope_stack))
    record_lambda_scope (lambda);
  else
    {
      tree_int *p = lambda_scope_stack->begin();
      LAMBDA_EXPR_EXTRA_SCOPE (lambda) = p->t;
      LAMBDA_EXPR_DISCRIMINATOR (lambda) = p->i++;
    }
  gcc_assert (LAMBDA_EXPR_EXTRA_SCOPE (lambda) == NULL_TREE);
}

void
finish_lambda_scope (void)
{
  tree_int *p = &lambda_scope_stack->last ();
  if (lambda_scope != p->t)
    {
      lambda_scope = p->t;
      lambda_count = p->i;
    }
  lambda_scope_stack->pop ();
}

tree
start_lambda_function (tree fco, tree lambda_expr)
{
  /* Let the front end know that we are going to be defining this
     function.  */
  start_preparsed_function (fco,
			    NULL_TREE,
			    SF_PRE_PARSED | SF_INCLASS_INLINE);

  tree body = begin_function_body ();

  /* Push the proxies for any explicit captures.  */
  for (tree cap = LAMBDA_EXPR_CAPTURE_LIST (lambda_expr); cap;
       cap = TREE_CHAIN (cap))
    build_capture_proxy (TREE_PURPOSE (cap), TREE_VALUE (cap));

  return body;
}

/* Subroutine of prune_lambda_captures: CAP is a node in
   LAMBDA_EXPR_CAPTURE_LIST.  Return the variable it captures for which we
   might optimize away the capture, or NULL_TREE if there is no such
   variable.  */

static tree
var_to_maybe_prune (tree cap)
{
  if (LAMBDA_CAPTURE_EXPLICIT_P (cap))
    /* Don't prune explicit captures.  */
    return NULL_TREE;

  tree mem = TREE_PURPOSE (cap);
  if (!DECL_P (mem) || !DECL_NORMAL_CAPTURE_P (mem))
    /* Packs and init-captures aren't captures of constant vars.  */
    return NULL_TREE;

  tree init = TREE_VALUE (cap);
  if (is_normal_capture_proxy (init))
    init = DECL_CAPTURED_VARIABLE (init);
  if (decl_constant_var_p (init))
    return init;

  return NULL_TREE;
}

/* walk_tree helper for prune_lambda_captures: Remember which capture proxies
   for constant variables are actually used in the lambda body.

   There will always be a DECL_EXPR for the capture proxy; remember it when we
   see it, but replace it with any other use.  */

static tree
mark_const_cap_r (tree *t, int *walk_subtrees, void *data)
{
  hash_map<tree,tree*> &const_vars = *(hash_map<tree,tree*>*)data;

  tree var = NULL_TREE;
  if (TREE_CODE (*t) == DECL_EXPR)
    {
      tree decl = DECL_EXPR_DECL (*t);
      if (is_constant_capture_proxy (decl))
	{
	  var = DECL_CAPTURED_VARIABLE (decl);
	  *walk_subtrees = 0;
	}
    }
  else if (is_constant_capture_proxy (*t))
    var = DECL_CAPTURED_VARIABLE (*t);

  if (var)
    {
      tree *&slot = const_vars.get_or_insert (var);
      if (!slot || VAR_P (*t))
	slot = t;
    }

  return NULL_TREE;
}

/* We're at the end of processing a lambda; go back and remove any captures of
   constant variables for which we've folded away all uses.  */

static void
prune_lambda_captures (tree body)
{
  tree lam = current_lambda_expr ();
  if (!LAMBDA_EXPR_CAPTURE_OPTIMIZED (lam))
    /* No uses were optimized away.  */
    return;
  if (LAMBDA_EXPR_DEFAULT_CAPTURE_MODE (lam) == CPLD_NONE)
    /* No default captures, and we don't prune explicit captures.  */
    return;

  hash_map<tree,tree*> const_vars;

  cp_walk_tree_without_duplicates (&body, mark_const_cap_r, &const_vars);

  tree *fieldp = &TYPE_FIELDS (LAMBDA_EXPR_CLOSURE (lam));
  for (tree *capp = &LAMBDA_EXPR_CAPTURE_LIST (lam); *capp; )
    {
      tree cap = *capp;
      if (tree var = var_to_maybe_prune (cap))
	{
	  tree **use = const_vars.get (var);
	  if (use && TREE_CODE (**use) == DECL_EXPR)
	    {
	      /* All uses of this capture were folded away, leaving only the
		 proxy declaration.  */

	      /* Splice the capture out of LAMBDA_EXPR_CAPTURE_LIST.  */
	      *capp = TREE_CHAIN (cap);

	      /* And out of TYPE_FIELDS.  */
	      tree field = TREE_PURPOSE (cap);
	      while (*fieldp != field)
		fieldp = &DECL_CHAIN (*fieldp);
	      *fieldp = DECL_CHAIN (*fieldp);

	      /* And remove the capture proxy declaration.  */
	      **use = void_node;
	      continue;
	    }
	}

      capp = &TREE_CHAIN (cap);
    }
}

void
finish_lambda_function (tree body)
{
  finish_function_body (body);

  prune_lambda_captures (body);

  /* Finish the function and generate code for it if necessary.  */
  tree fn = finish_function (/*inline_p=*/true);

  /* Only expand if the call op is not a template.  */
  if (!DECL_TEMPLATE_INFO (fn))
    expand_or_defer_fn (fn);
}

#include "gt-cp-lambda.h"
