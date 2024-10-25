/* Handle initialization things in -*- C++ -*-
   Copyright (C) 1987-2024 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com)

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

/* High-level class interface.  */

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "cp-tree.h"
#include "stringpool.h"
#include "varasm.h"
#include "gimplify.h"
#include "c-family/c-ubsan.h"
#include "intl.h"
#include "stringpool.h"
#include "attribs.h"
#include "asan.h"
#include "stor-layout.h"
#include "pointer-query.h"

static bool begin_init_stmts (tree *, tree *);
static tree finish_init_stmts (bool, tree, tree);
static void construct_virtual_base (tree, tree);
static bool expand_aggr_init_1 (tree, tree, tree, tree, int, tsubst_flags_t);
static bool expand_default_init (tree, tree, tree, tree, int, tsubst_flags_t);
static int member_init_ok_or_else (tree, tree, tree);
static void expand_virtual_init (tree, tree);
static tree sort_mem_initializers (tree, tree);
static tree initializing_context (tree);
static void expand_cleanup_for_base (tree, tree);
static tree dfs_initialize_vtbl_ptrs (tree, void *);
static tree build_field_list (tree, tree, int *);
static int diagnose_uninitialized_cst_or_ref_member_1 (tree, tree, bool, bool);

static GTY(()) tree fn;

/* We are about to generate some complex initialization code.
   Conceptually, it is all a single expression.  However, we may want
   to include conditionals, loops, and other such statement-level
   constructs.  Therefore, we build the initialization code inside a
   statement-expression.  This function starts such an expression.
   STMT_EXPR_P and COMPOUND_STMT_P are filled in by this function;
   pass them back to finish_init_stmts when the expression is
   complete.  */

static bool
begin_init_stmts (tree *stmt_expr_p, tree *compound_stmt_p)
{
  bool is_global = !building_stmt_list_p ();

  *stmt_expr_p = begin_stmt_expr ();
  *compound_stmt_p = begin_compound_stmt (BCS_NO_SCOPE);

  return is_global;
}

/* Finish out the statement-expression begun by the previous call to
   begin_init_stmts.  Returns the statement-expression itself.  */

static tree
finish_init_stmts (bool is_global, tree stmt_expr, tree compound_stmt)
{
  finish_compound_stmt (compound_stmt);

  stmt_expr = finish_stmt_expr (stmt_expr, true);

  gcc_assert (!building_stmt_list_p () == is_global);

  return stmt_expr;
}

/* Constructors */

/* Called from initialize_vtbl_ptrs via dfs_walk.  BINFO is the base
   which we want to initialize the vtable pointer for, DATA is
   TREE_LIST whose TREE_VALUE is the this ptr expression.  */

static tree
dfs_initialize_vtbl_ptrs (tree binfo, void *data)
{
  if (!TYPE_CONTAINS_VPTR_P (BINFO_TYPE (binfo)))
    return dfs_skip_bases;

  if (!BINFO_PRIMARY_P (binfo) || BINFO_VIRTUAL_P (binfo))
    {
      tree base_ptr = TREE_VALUE ((tree) data);

      base_ptr = build_base_path (PLUS_EXPR, base_ptr, binfo, /*nonnull=*/1,
				  tf_warning_or_error);

      expand_virtual_init (binfo, base_ptr);
    }

  return NULL_TREE;
}

/* Initialize all the vtable pointers in the object pointed to by
   ADDR.  */

void
initialize_vtbl_ptrs (tree addr)
{
  tree list;
  tree type;

  type = TREE_TYPE (TREE_TYPE (addr));
  list = build_tree_list (type, addr);

  /* Walk through the hierarchy, initializing the vptr in each base
     class.  We do these in pre-order because we can't find the virtual
     bases for a class until we've initialized the vtbl for that
     class.  */
  dfs_walk_once (TYPE_BINFO (type), dfs_initialize_vtbl_ptrs, NULL, list);
}

/* Return an expression for the zero-initialization of an object with
   type T.  This expression will either be a constant (in the case
   that T is a scalar), or a CONSTRUCTOR (in the case that T is an
   aggregate), or NULL (in the case that T does not require
   initialization).  In either case, the value can be used as
   DECL_INITIAL for a decl of the indicated TYPE; it is a valid static
   initializer. If NELTS is non-NULL, and TYPE is an ARRAY_TYPE, NELTS
   is the number of elements in the array.  If STATIC_STORAGE_P is
   TRUE, initializers are only generated for entities for which
   zero-initialization does not simply mean filling the storage with
   zero bytes.  FIELD_SIZE, if non-NULL, is the bit size of the field,
   subfields with bit positions at or above that bit size shouldn't
   be added.  Note that this only works when the result is assigned
   to a base COMPONENT_REF; if we only have a pointer to the base subobject,
   expand_assignment will end up clearing the full size of TYPE.  */

static tree
build_zero_init_1 (tree type, tree nelts, bool static_storage_p,
		   tree field_size)
{
  tree init = NULL_TREE;

  /* [dcl.init]

     To zero-initialize an object of type T means:

     -- if T is a scalar type, the storage is set to the value of zero
	converted to T.

     -- if T is a non-union class type, the storage for each non-static
	data member and each base-class subobject is zero-initialized.

     -- if T is a union type, the storage for its first data member is
	zero-initialized.

     -- if T is an array type, the storage for each element is
	zero-initialized.

     -- if T is a reference type, no initialization is performed.  */

  gcc_assert (nelts == NULL_TREE || TREE_CODE (nelts) == INTEGER_CST);

  /* An initializer is unqualified.  */
  type = cv_unqualified (type);

  if (type == error_mark_node)
    ;
  else if (static_storage_p && zero_init_p (type))
    /* In order to save space, we do not explicitly build initializers
       for items that do not need them.  GCC's semantics are that
       items with static storage duration that are not otherwise
       initialized are initialized to zero.  */
    ;
  else if (TYPE_PTR_OR_PTRMEM_P (type))
    init = fold (convert (type, nullptr_node));
  else if (NULLPTR_TYPE_P (type))
    init = build_int_cst (type, 0);
  else if (SCALAR_TYPE_P (type))
    init = build_zero_cst (type);
  else if (RECORD_OR_UNION_CODE_P (TREE_CODE (type)))
    {
      tree field, next;
      vec<constructor_elt, va_gc> *v = NULL;

      /* Iterate over the fields, building initializations.  */
      for (field = TYPE_FIELDS (type); field; field = next)
	{
	  next = DECL_CHAIN (field);

	  if (TREE_CODE (field) != FIELD_DECL)
	    continue;

	  /* For unions, only the first field is initialized.  */
	  if (TREE_CODE (type) == UNION_TYPE)
	    next = NULL_TREE;

	  if (TREE_TYPE (field) == error_mark_node)
	    continue;

	  /* Don't add virtual bases for base classes if they are beyond
	     the size of the current field, that means it is present
	     somewhere else in the object.  */
	  if (field_size)
	    {
	      tree bitpos = bit_position (field);
	      if (TREE_CODE (bitpos) == INTEGER_CST
		  && !tree_int_cst_lt (bitpos, field_size))
		continue;
	    }

	  /* Don't add zero width bitfields.  */
	  if (DECL_C_BIT_FIELD (field)
	      && integer_zerop (DECL_SIZE (field)))
	    continue;

	  /* Note that for class types there will be FIELD_DECLs
	     corresponding to base classes as well.  Thus, iterating
	     over TYPE_FIELDs will result in correct initialization of
	     all of the subobjects.  */
	  if (!static_storage_p || !zero_init_p (TREE_TYPE (field)))
	    {
	      tree new_field_size
		= (DECL_FIELD_IS_BASE (field)
		   && DECL_SIZE (field)
		   && TREE_CODE (DECL_SIZE (field)) == INTEGER_CST)
		  ? DECL_SIZE (field) : NULL_TREE;
	      tree value = build_zero_init_1 (TREE_TYPE (field),
					      /*nelts=*/NULL_TREE,
					      static_storage_p,
					      new_field_size);
	      if (value)
		CONSTRUCTOR_APPEND_ELT(v, field, value);
	    }
	}

      /* Build a constructor to contain the initializations.  */
      init = build_constructor (type, v);
    }
  else if (TREE_CODE (type) == ARRAY_TYPE)
    {
      tree max_index;
      vec<constructor_elt, va_gc> *v = NULL;

      /* Iterate over the array elements, building initializations.  */
      if (nelts)
	max_index = fold_build2_loc (input_location, MINUS_EXPR,
				     TREE_TYPE (nelts), nelts,
				     build_one_cst (TREE_TYPE (nelts)));
      /* Treat flexible array members like [0] arrays.  */
      else if (TYPE_DOMAIN (type) == NULL_TREE)
	return NULL_TREE;
      else
	max_index = array_type_nelts_minus_one (type);

      /* If we have an error_mark here, we should just return error mark
	 as we don't know the size of the array yet.  */
      if (max_index == error_mark_node)
	return error_mark_node;
      gcc_assert (TREE_CODE (max_index) == INTEGER_CST);

      /* A zero-sized array, which is accepted as an extension, will
	 have an upper bound of -1.  */
      if (!integer_minus_onep (max_index))
	{
	  constructor_elt ce;

	  /* If this is a one element array, we just use a regular init.  */
	  if (integer_zerop (max_index))
	    ce.index = size_zero_node;
	  else
	    ce.index = build2 (RANGE_EXPR, sizetype, size_zero_node,
			       max_index);

	  ce.value = build_zero_init_1 (TREE_TYPE (type), /*nelts=*/NULL_TREE,
					static_storage_p, NULL_TREE);
	  if (ce.value)
	    {
	      vec_alloc (v, 1);
	      v->quick_push (ce);
	    }
	}

      /* Build a constructor to contain the initializations.  */
      init = build_constructor (type, v);
    }
  else if (VECTOR_TYPE_P (type))
    init = build_zero_cst (type);
  else
    gcc_assert (TYPE_REF_P (type));

  /* In all cases, the initializer is a constant.  */
  if (init)
    TREE_CONSTANT (init) = 1;

  return init;
}

/* Return an expression for the zero-initialization of an object with
   type T.  This expression will either be a constant (in the case
   that T is a scalar), or a CONSTRUCTOR (in the case that T is an
   aggregate), or NULL (in the case that T does not require
   initialization).  In either case, the value can be used as
   DECL_INITIAL for a decl of the indicated TYPE; it is a valid static
   initializer. If NELTS is non-NULL, and TYPE is an ARRAY_TYPE, NELTS
   is the number of elements in the array.  If STATIC_STORAGE_P is
   TRUE, initializers are only generated for entities for which
   zero-initialization does not simply mean filling the storage with
   zero bytes.  */

tree
build_zero_init (tree type, tree nelts, bool static_storage_p)
{
  return build_zero_init_1 (type, nelts, static_storage_p, NULL_TREE);
}

/* Return a suitable initializer for value-initializing an object of type
   TYPE, as described in [dcl.init].  */

tree
build_value_init (tree type, tsubst_flags_t complain)
{
  /* [dcl.init]

     To value-initialize an object of type T means:

     - if T is a class type (clause 9) with either no default constructor
       (12.1) or a default constructor that is user-provided or deleted,
       then the object is default-initialized;

     - if T is a (possibly cv-qualified) class type without a user-provided
       or deleted default constructor, then the object is zero-initialized
       and the semantic constraints for default-initialization are checked,
       and if T has a non-trivial default constructor, the object is
       default-initialized;

     - if T is an array type, then each element is value-initialized;

     - otherwise, the object is zero-initialized.

     A program that calls for default-initialization or
     value-initialization of an entity of reference type is ill-formed.  */

  if (CLASS_TYPE_P (type) && type_build_ctor_call (type))
    {
      tree ctor
	= build_special_member_call (NULL_TREE, complete_ctor_identifier,
				     NULL, type, LOOKUP_NORMAL, complain);
      if (ctor == error_mark_node || TREE_CONSTANT (ctor))
	return ctor;
      if (processing_template_decl)
	/* The AGGR_INIT_EXPR tweaking below breaks in templates.  */
	return build_min (CAST_EXPR, type, NULL_TREE);
      tree fn = NULL_TREE;
      if (TREE_CODE (ctor) == CALL_EXPR)
	fn = get_callee_fndecl (ctor);
      ctor = build_aggr_init_expr (type, ctor);
      if (fn && user_provided_p (fn))
	return ctor;
      else if (TYPE_HAS_COMPLEX_DFLT (type))
	{
	  /* This is a class that needs constructing, but doesn't have
	     a user-provided constructor.  So we need to zero-initialize
	     the object and then call the implicitly defined ctor.
	     This will be handled in simplify_aggr_init_expr.  */
	  AGGR_INIT_ZERO_FIRST (ctor) = 1;
	  return ctor;
	}
    }

  /* Discard any access checking during subobject initialization;
     the checks are implied by the call to the ctor which we have
     verified is OK (cpp0x/defaulted46.C).  */
  push_deferring_access_checks (dk_deferred);
  tree r = build_value_init_noctor (type, complain);
  pop_deferring_access_checks ();
  return r;
}

/* Like build_value_init, but don't call the constructor for TYPE.  Used
   for base initializers.  */

tree
build_value_init_noctor (tree type, tsubst_flags_t complain)
{
  if (!COMPLETE_TYPE_P (type))
    {
      if (complain & tf_error)
	error ("value-initialization of incomplete type %qT", type);
      return error_mark_node;
    }
  /* FIXME the class and array cases should just use digest_init once it is
     SFINAE-enabled.  */
  if (CLASS_TYPE_P (type))
    {
      gcc_assert (!TYPE_HAS_COMPLEX_DFLT (type)
		  || errorcount != 0);

      if (TREE_CODE (type) != UNION_TYPE)
	{
	  tree field;
	  vec<constructor_elt, va_gc> *v = NULL;

	  /* Iterate over the fields, building initializations.  */
	  for (field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
	    {
	      tree ftype, value;

	      if (TREE_CODE (field) != FIELD_DECL)
		continue;

	      ftype = TREE_TYPE (field);

	      if (ftype == error_mark_node)
		continue;

	      /* Ignore flexible array members for value initialization.  */
	      if (TREE_CODE (ftype) == ARRAY_TYPE
		  && !COMPLETE_TYPE_P (ftype)
		  && !TYPE_DOMAIN (ftype)
		  && COMPLETE_TYPE_P (TREE_TYPE (ftype))
		  && (next_aggregate_field (DECL_CHAIN (field))
		      == NULL_TREE))
		continue;

	      /* Ignore unnamed zero-width bitfields.  */
	      if (DECL_UNNAMED_BIT_FIELD (field)
		  && integer_zerop (DECL_SIZE (field)))
		continue;

	      /* We could skip vfields and fields of types with
		 user-defined constructors, but I think that won't improve
		 performance at all; it should be simpler in general just
		 to zero out the entire object than try to only zero the
		 bits that actually need it.  */

	      /* Note that for class types there will be FIELD_DECLs
		 corresponding to base classes as well.  Thus, iterating
		 over TYPE_FIELDs will result in correct initialization of
		 all of the subobjects.  */
	      value = build_value_init (ftype, complain);
	      value = maybe_constant_init (value);

	      if (value == error_mark_node)
		return error_mark_node;

	      CONSTRUCTOR_APPEND_ELT(v, field, value);

	      /* We shouldn't have gotten here for anything that would need
		 non-trivial initialization, and gimplify_init_ctor_preeval
		 would need to be fixed to allow it.  */
	      gcc_assert (TREE_CODE (value) != TARGET_EXPR
			  && TREE_CODE (value) != AGGR_INIT_EXPR);
	    }

	  /* Build a constructor to contain the zero- initializations.  */
	  return build_constructor (type, v);
	}
    }
  else if (TREE_CODE (type) == ARRAY_TYPE)
    {
      vec<constructor_elt, va_gc> *v = NULL;

      /* Iterate over the array elements, building initializations.  */
      tree max_index = array_type_nelts_minus_one (type);

      /* If we have an error_mark here, we should just return error mark
	 as we don't know the size of the array yet.  */
      if (max_index == error_mark_node)
	{
	  if (complain & tf_error)
	    error ("cannot value-initialize array of unknown bound %qT",
		   type);
	  return error_mark_node;
	}
      gcc_assert (TREE_CODE (max_index) == INTEGER_CST);

      /* A zero-sized array, which is accepted as an extension, will
	 have an upper bound of -1.  */
      if (!tree_int_cst_equal (max_index, integer_minus_one_node))
	{
	  constructor_elt ce;

	  /* If this is a one element array, we just use a regular init.  */
	  if (tree_int_cst_equal (size_zero_node, max_index))
	    ce.index = size_zero_node;
	  else
	    ce.index = build2 (RANGE_EXPR, sizetype, size_zero_node, max_index);

	  ce.value = build_value_init (TREE_TYPE (type), complain);
	  ce.value = maybe_constant_init (ce.value);
	  if (ce.value == error_mark_node)
	    return error_mark_node;

	  vec_alloc (v, 1);
	  v->quick_push (ce);

	  /* We shouldn't have gotten here for anything that would need
	     non-trivial initialization, and gimplify_init_ctor_preeval
	     would need to be fixed to allow it.  */
	  gcc_assert (TREE_CODE (ce.value) != TARGET_EXPR
		      && TREE_CODE (ce.value) != AGGR_INIT_EXPR);
	}

      /* Build a constructor to contain the initializations.  */
      return build_constructor (type, v);
    }
  else if (TREE_CODE (type) == FUNCTION_TYPE)
    {
      if (complain & tf_error)
	error ("value-initialization of function type %qT", type);
      return error_mark_node;
    }
  else if (TYPE_REF_P (type))
    {
      if (complain & tf_error)
	error ("value-initialization of reference type %qT", type);
      return error_mark_node;
    }

  return build_zero_init (type, NULL_TREE, /*static_storage_p=*/false);
}

/* Initialize current class with INIT, a TREE_LIST of arguments for
   a target constructor.  If TREE_LIST is void_type_node, an empty
   initializer list was given.  Return the target constructor.  */

static tree
perform_target_ctor (tree init)
{
  tree decl = current_class_ref;
  tree type = current_class_type;

  init = build_aggr_init (decl, init, LOOKUP_NORMAL|LOOKUP_DELEGATING_CONS,
			  tf_warning_or_error);
  finish_expr_stmt (init);
  if (type_build_dtor_call (type))
    {
      tree expr = build_delete (input_location,
				type, decl, sfk_complete_destructor,
				LOOKUP_NORMAL
				|LOOKUP_NONVIRTUAL
				|LOOKUP_DESTRUCTOR,
				0, tf_warning_or_error);
      if (DECL_HAS_IN_CHARGE_PARM_P (current_function_decl))
	{
	  tree base = build_delete (input_location,
				    type, decl, sfk_base_destructor,
				    LOOKUP_NORMAL
				    |LOOKUP_NONVIRTUAL
				    |LOOKUP_DESTRUCTOR,
				    0, tf_warning_or_error);
	  expr = build_if_in_charge (expr, base);
	}
      if (expr != error_mark_node
	  && TYPE_HAS_NONTRIVIAL_DESTRUCTOR (type))
	finish_eh_cleanup (expr);
    }
  return init;
}

/* Instantiate the default member initializer of MEMBER, if needed.
   Only get_nsdmi should use the return value of this function.  */

tree
maybe_instantiate_nsdmi_init (tree member, tsubst_flags_t complain)
{
  tree init = DECL_INITIAL (member);

  /* tsubst_decl uses void_node to indicate an uninstantiated DMI.  */
  if (init == void_node)
    {
      /* Clear any special tsubst flags; the result of NSDMI instantiation
	 should be independent of the substitution context.  */
      complain &= tf_warning_or_error;

      init = DECL_INITIAL (DECL_TI_TEMPLATE (member));
      location_t expr_loc
	= cp_expr_loc_or_loc (init, DECL_SOURCE_LOCATION (member));
      if (TREE_CODE (init) == DEFERRED_PARSE)
	/* Unparsed.  */;
      /* Check recursive instantiation.  */
      else if (DECL_INSTANTIATING_NSDMI_P (member))
	{
	  if (complain & tf_error)
	    error_at (expr_loc, "recursive instantiation of default member "
		      "initializer for %qD", member);
	  init = error_mark_node;
	}
      else
	{
	  cp_evaluated ev;

	  location_t sloc = input_location;
	  input_location = expr_loc;

	  DECL_INSTANTIATING_NSDMI_P (member) = 1;

	  bool pushed = false;
	  tree ctx = type_context_for_name_lookup (member);

	  bool push_to_top = maybe_push_to_top_level (member);
	  if (!currently_open_class (ctx))
	    {
	      push_nested_class (ctx);
	      push_deferring_access_checks (dk_no_deferred);
	      pushed = true;
	    }

	  inject_this_parameter (ctx, TYPE_UNQUALIFIED);

	  start_lambda_scope (member);

	  /* Do deferred instantiation of the NSDMI.  */
	  init = tsubst_expr (init, DECL_TI_ARGS (member), complain, member);
	  init = digest_nsdmi_init (member, init, complain);

	  finish_lambda_scope ();

	  DECL_INSTANTIATING_NSDMI_P (member) = 0;

	  if (init != error_mark_node)
	    DECL_INITIAL (member) = init;

	  if (pushed)
	    {
	      pop_deferring_access_checks ();
	      pop_nested_class ();
	    }
	  maybe_pop_from_top_level (push_to_top);

	  input_location = sloc;
	}
    }

  return init;
}

/* Return the non-static data initializer for FIELD_DECL MEMBER.  */

tree
get_nsdmi (tree member, bool in_ctor, tsubst_flags_t complain)
{
  tree save_ccp = current_class_ptr;
  tree save_ccr = current_class_ref;

  tree init = maybe_instantiate_nsdmi_init (member, complain);

  if (init && TREE_CODE (init) == DEFERRED_PARSE)
    {
      if (complain & tf_error)
	{
	  auto_diagnostic_group d;
	  error ("default member initializer for %qD required before the end "
		 "of its enclosing class", member);
	  inform (location_of (init), "defined here");
	  DECL_INITIAL (member) = error_mark_node;
	}
      init = error_mark_node;
    }

  if (in_ctor)
    {
      current_class_ptr = save_ccp;
      current_class_ref = save_ccr;
    }
  else
    {
      /* Use a PLACEHOLDER_EXPR when we don't have a 'this' parameter to
	 refer to; constexpr evaluation knows what to do with it.  */
      current_class_ref = build0 (PLACEHOLDER_EXPR, DECL_CONTEXT (member));
      current_class_ptr = build_address (current_class_ref);
    }

  /* Clear processing_template_decl for sake of break_out_target_exprs;
     INIT is always non-templated.  */
  processing_template_decl_sentinel ptds;

  /* Strip redundant TARGET_EXPR so we don't need to remap it, and
     so the aggregate init code below will see a CONSTRUCTOR.  */
  bool simple_target = (init && SIMPLE_TARGET_EXPR_P (init));
  if (simple_target)
    init = TARGET_EXPR_INITIAL (init);
  init = break_out_target_exprs (init, /*loc*/true);
  if (init && TREE_CODE (init) == TARGET_EXPR)
    /* In a constructor, this expresses the full initialization, prevent
       perform_member_init from calling another constructor (58162).  */
    TARGET_EXPR_DIRECT_INIT_P (init) = in_ctor;
  if (simple_target && TREE_CODE (init) != CONSTRUCTOR)
    /* Now put it back so C++17 copy elision works.  */
    init = get_target_expr (init);

  set_target_expr_eliding (init);

  current_class_ptr = save_ccp;
  current_class_ref = save_ccr;
  return init;
}

/* Diagnose the flexible array MEMBER if its INITializer is non-null
   and return true if so.  Otherwise return false.  */

bool
maybe_reject_flexarray_init (tree member, tree init)
{
  tree type = TREE_TYPE (member);

  if (!init
      || TREE_CODE (type) != ARRAY_TYPE
      || TYPE_DOMAIN (type))
    return false;

  /* Point at the flexible array member declaration if it's initialized
     in-class, and at the ctor if it's initialized in a ctor member
     initializer list.  */
  location_t loc;
  if (DECL_INITIAL (member) == init
      || !current_function_decl
      || DECL_DEFAULTED_FN (current_function_decl))
    loc = DECL_SOURCE_LOCATION (member);
  else
    loc = DECL_SOURCE_LOCATION (current_function_decl);

  error_at (loc, "initializer for flexible array member %q#D", member);
  return true;
}

/* If INIT's value can come from a call to std::initializer_list<T>::begin,
   return that function.  Otherwise, NULL_TREE.  */

static tree
find_list_begin (tree init)
{
  STRIP_NOPS (init);
  while (TREE_CODE (init) == COMPOUND_EXPR)
    init = TREE_OPERAND (init, 1);
  STRIP_NOPS (init);
  if (TREE_CODE (init) == COND_EXPR)
    {
      tree left = TREE_OPERAND (init, 1);
      if (!left)
	left = TREE_OPERAND (init, 0);
      left = find_list_begin (left);
      if (left)
	return left;
      return find_list_begin (TREE_OPERAND (init, 2));
    }
  if (TREE_CODE (init) == CALL_EXPR)
    if (tree fn = get_callee_fndecl (init))
      if (id_equal (DECL_NAME (fn), "begin")
	  && is_std_init_list (DECL_CONTEXT (fn)))
	return fn;
  return NULL_TREE;
}

/* If INIT initializing MEMBER is copying the address of the underlying array
   of an initializer_list, warn.  */

static void
maybe_warn_list_ctor (tree member, tree init)
{
  tree memtype = TREE_TYPE (member);
  if (!init || !TYPE_PTR_P (memtype)
      || !is_list_ctor (current_function_decl))
    return;

  tree parm = FUNCTION_FIRST_USER_PARMTYPE (current_function_decl);
  parm = TREE_VALUE (parm);
  tree initlist = non_reference (parm);

  /* Do not warn if the parameter is an lvalue reference to non-const.  */
  if (TYPE_REF_P (parm) && !TYPE_REF_IS_RVALUE (parm)
      && !CP_TYPE_CONST_P (initlist))
    return;

  tree targs = CLASSTYPE_TI_ARGS (initlist);
  tree elttype = TREE_VEC_ELT (targs, 0);

  if (!same_type_ignoring_top_level_qualifiers_p
      (TREE_TYPE (memtype), elttype))
    return;

  tree begin = find_list_begin (init);
  if (!begin)
    return;

  location_t loc = cp_expr_loc_or_input_loc (init);
  warning_at (loc, OPT_Winit_list_lifetime,
	     "initializing %qD from %qE does not extend the lifetime "
	     "of the underlying array", member, begin);
}

/* Data structure for find_uninit_fields_r, below.  */

struct find_uninit_data {
  /* The set tracking the yet-uninitialized members.  */
  hash_set<tree> *uninitialized;
  /* The data member we are currently initializing.  It can be either
     a type (initializing a base class/delegating constructors), or
     a COMPONENT_REF.  */
  tree member;
};

/* walk_tree callback that warns about using uninitialized data in
   a member-initializer-list.  */

static tree
find_uninit_fields_r (tree *tp, int *walk_subtrees, void *data)
{
  find_uninit_data *d = static_cast<find_uninit_data *>(data);
  hash_set<tree> *uninitialized = d->uninitialized;
  tree init = *tp;
  const tree_code code = TREE_CODE (init);

  /* No need to look into types or unevaluated operands.  */
  if (TYPE_P (init) || unevaluated_p (code))
    {
      *walk_subtrees = false;
      return NULL_TREE;
    }

  switch (code)
    {
    /* We'd need data flow info to avoid false positives.  */
    case COND_EXPR:
    case VEC_COND_EXPR:
    case BIND_EXPR:
    /* We might see a MODIFY_EXPR in cases like S() : a((b = 42)), c(b) { }
       where the initializer for 'a' surreptitiously initializes 'b'.  Let's
       not bother with these complicated scenarios in the front end.  */
    case MODIFY_EXPR:
    /* Don't attempt to handle statement-expressions, either.  */
    case STATEMENT_LIST:
      uninitialized->empty ();
      gcc_fallthrough ();
    /* If we're just taking the address of an object, it doesn't matter
       whether it's been initialized.  */
    case ADDR_EXPR:
      *walk_subtrees = false;
      return NULL_TREE;
    default:
      break;
    }

  /* We'd need data flow info to avoid false positives.  */
  if (truth_value_p (code))
    goto give_up;
  /* Attempt to handle a simple a{b}, but no more.  */
  else if (BRACE_ENCLOSED_INITIALIZER_P (init))
    {
      if (CONSTRUCTOR_NELTS (init) == 1
	  && !BRACE_ENCLOSED_INITIALIZER_P (CONSTRUCTOR_ELT (init, 0)->value))
	init = CONSTRUCTOR_ELT (init, 0)->value;
      else
	goto give_up;
    }
  /* Warn about uninitialized 'this'.  */
  else if (code == CALL_EXPR)
    {
      tree fn = get_callee_fndecl (init);
      if (fn && DECL_IOBJ_MEMBER_FUNCTION_P (fn))
	{
	  tree op = CALL_EXPR_ARG (init, 0);
	  if (TREE_CODE (op) == ADDR_EXPR)
	    op = TREE_OPERAND (op, 0);
	  temp_override<tree> ovr (d->member, DECL_ARGUMENTS (fn));
	  cp_walk_tree_without_duplicates (&op, find_uninit_fields_r, data);
	}
      /* Functions (whether static or nonstatic member) may have side effects
	 and initialize other members; it's not the front end's job to try to
	 figure it out.  But don't give up for constructors: we still want to
	 warn when initializing base classes:

	   struct D : public B {
	     int x;
	     D() : B(x) {}
	   };

	 so carry on to detect that 'x' is used uninitialized.  */
      if (!fn || !DECL_CONSTRUCTOR_P (fn))
	goto give_up;
    }

  /* If we find FIELD in the uninitialized set, we warn.  */
  if (code == COMPONENT_REF)
    {
      tree field = TREE_OPERAND (init, 1);
      tree type = TYPE_P (d->member) ? d->member : TREE_TYPE (d->member);

      /* We're initializing a reference member with itself.  */
      if (TYPE_REF_P (type) && cp_tree_equal (d->member, init))
	warning_at (EXPR_LOCATION (init), OPT_Winit_self,
		    "%qD is initialized with itself", field);
      else if (cp_tree_equal (TREE_OPERAND (init, 0), current_class_ref)
	       && uninitialized->contains (field))
	{
	  if (TYPE_REF_P (TREE_TYPE (field)))
	    warning_at (EXPR_LOCATION (init), OPT_Wuninitialized,
			"reference %qD is not yet bound to a value when used "
			"here", field);
	  else if ((!INDIRECT_TYPE_P (type) || is_this_parameter (d->member))
		   && !conv_binds_to_reference_parm_p (type, init))
	    warning_at (EXPR_LOCATION (init), OPT_Wuninitialized,
			"member %qD is used uninitialized", field);
	  *walk_subtrees = false;
	}
    }

  return NULL_TREE;

give_up:
  *walk_subtrees = false;
  uninitialized->empty ();
  return integer_zero_node;
}

/* Wrapper around find_uninit_fields_r above.  */

static void
find_uninit_fields (tree *t, hash_set<tree> *uninitialized, tree member)
{
  if (!uninitialized->is_empty ())
    {
      find_uninit_data data = { uninitialized, member };
      cp_walk_tree_without_duplicates (t, find_uninit_fields_r, &data);
    }
}

/* Return true if it's OK to initialize an array TYPE from INIT.  Mere mortals
   can't copy arrays, but the compiler can do so with a VEC_INIT_EXPR in
   certain cases.  */

static bool
can_init_array_with_p (tree type, tree init)
{
  if (!init)
    /* Value-init, OK.  */
    return true;
  if (!same_type_ignoring_top_level_qualifiers_p (type, TREE_TYPE (init)))
    return false;
  /* We're called from synthesize_method, and we're processing the
     mem-initializers of a constructor.  */
  if (DECL_DEFAULTED_FN (current_function_decl))
    return true;
  if (TREE_CODE (init) == TARGET_EXPR)
    {
      init = TARGET_EXPR_INITIAL (init);
      /* As an extension, we allow copying from a compound literal.  */
      if (TREE_CODE (init) == CONSTRUCTOR)
	return CONSTRUCTOR_C99_COMPOUND_LITERAL (init);
      /* VEC_INIT_EXPR is used for non-constant initialization of trailing
	 elements with no explicit initializers.  */
      else if (TREE_CODE (init) == VEC_INIT_EXPR)
	return true;
    }

  return false;
}

/* Initialize MEMBER, a FIELD_DECL, with INIT, a TREE_LIST of
   arguments.  If TREE_LIST is void_type_node, an empty initializer
   list was given; if NULL_TREE no initializer was given.  UNINITIALIZED
   is the hash set that tracks uninitialized fields.  */

static void
perform_member_init (tree member, tree init, hash_set<tree> &uninitialized)
{
  tree decl;
  tree type = TREE_TYPE (member);

  /* Use the non-static data member initializer if there was no
     mem-initializer for this field.  */
  if (init == NULL_TREE)
    init = get_nsdmi (member, /*ctor*/true, tf_warning_or_error);

  if (init == error_mark_node)
    return;

  /* Effective C++ rule 12 requires that all data members be
     initialized.  */
  if (warn_ecpp && init == NULL_TREE && TREE_CODE (type) != ARRAY_TYPE)
    warning_at (DECL_SOURCE_LOCATION (current_function_decl), OPT_Weffc__,
		"%qD should be initialized in the member initialization list",
		member);

  /* Get an lvalue for the data member.  */
  decl = build_class_member_access_expr (current_class_ref, member,
					 /*access_path=*/NULL_TREE,
					 /*preserve_reference=*/true,
					 tf_warning_or_error);
  if (decl == error_mark_node)
    return;

  if ((warn_init_self || warn_uninitialized || warn_self_move)
      && init
      && TREE_CODE (init) == TREE_LIST
      && TREE_CHAIN (init) == NULL_TREE)
    {
      tree val = TREE_VALUE (init);
      /* Handle references.  */
      if (REFERENCE_REF_P (val))
	val = TREE_OPERAND (val, 0);
      if (TREE_CODE (val) == COMPONENT_REF && TREE_OPERAND (val, 1) == member
	  && TREE_OPERAND (val, 0) == current_class_ref)
	warning_at (DECL_SOURCE_LOCATION (current_function_decl),
		    OPT_Winit_self, "%qD is initialized with itself",
		    member);
      else if (!maybe_warn_self_move (input_location, member,
				      TREE_VALUE (init)))
	find_uninit_fields (&val, &uninitialized, decl);
    }

  if (array_of_unknown_bound_p (type))
    {
      maybe_reject_flexarray_init (member, init);
      return;
    }

  if (init && TREE_CODE (init) == TREE_LIST)
    {
      /* A(): a{e} */
      if (DIRECT_LIST_INIT_P (TREE_VALUE (init)))
	init = build_x_compound_expr_from_list (init, ELK_MEM_INIT,
						tf_warning_or_error);
      /* We are trying to initialize an array from a ()-list.  If we
	 should attempt to do so, conjure up a CONSTRUCTOR.  */
      else if (TREE_CODE (type) == ARRAY_TYPE
	       /* P0960 is a C++20 feature.  */
	       && cxx_dialect >= cxx20)
	init = do_aggregate_paren_init (init, type);
      else if (!CLASS_TYPE_P (type))
	init = build_x_compound_expr_from_list (init, ELK_MEM_INIT,
						tf_warning_or_error);
      /* If we're initializing a class from a ()-list, leave the TREE_LIST
	 alone: we might call an appropriate constructor, or (in C++20)
	 do aggregate-initialization.  */
    }

  /* Assume we are initializing the member.  */
  bool member_initialized_p = true;

  if (init == void_type_node)
    {
      /* mem() means value-initialization.  */
      if (TREE_CODE (type) == ARRAY_TYPE)
	{
	  init = build_vec_init_expr (type, init, tf_warning_or_error);
	  init = cp_build_init_expr (decl, init);
	  finish_expr_stmt (init);
	}
      else
	{
	  tree value = build_value_init (type, tf_warning_or_error);
	  if (value == error_mark_node)
	    return;
	  init = cp_build_init_expr (decl, value);
	  finish_expr_stmt (init);
	}
    }
  /* Deal with this here, as we will get confused if we try to call the
     assignment op for an anonymous union.  This can happen in a
     synthesized copy constructor.  */
  else if (ANON_AGGR_TYPE_P (type))
    {
      if (init)
	{
	  init = cp_build_init_expr (decl, TREE_VALUE (init));
	  finish_expr_stmt (init);
	}
    }
  else if (init
	   && (TYPE_REF_P (type)
	       || (TREE_CODE (init) == CONSTRUCTOR
		   && (CP_AGGREGATE_TYPE_P (type)
		       || is_std_init_list (type)))))
    {
      /* With references and list-initialization, we need to deal with
	 extending temporary lifetimes.  12.2p5: "A temporary bound to a
	 reference member in a constructor’s ctor-initializer (12.6.2)
	 persists until the constructor exits."  */
      unsigned i; tree t;
      releasing_vec cleanups;
      if (!same_type_ignoring_top_level_qualifiers_p (TREE_TYPE (init), type))
	{
	  if (BRACE_ENCLOSED_INITIALIZER_P (init)
	      && CP_AGGREGATE_TYPE_P (type))
	    init = reshape_init (type, init, tf_warning_or_error);
	  init = digest_init (type, init, tf_warning_or_error);
	}
      if (init == error_mark_node)
	return;
      if (is_empty_field (member)
	  && !TREE_SIDE_EFFECTS (init))
	/* Don't add trivial initialization of an empty base/field, as they
	   might not be ordered the way the back-end expects.  */
	return;
      /* A FIELD_DECL doesn't really have a suitable lifetime, but
	 make_temporary_var_for_ref_to_temp will treat it as automatic and
	 set_up_extended_ref_temp wants to use the decl in a warning.  */
      init = extend_ref_init_temps (member, init, &cleanups);
      if (TREE_CODE (type) == ARRAY_TYPE
	  && TYPE_HAS_NONTRIVIAL_DESTRUCTOR (TREE_TYPE (type)))
	init = build_vec_init_expr (type, init, tf_warning_or_error);
      init = cp_build_init_expr (decl, init);
      finish_expr_stmt (init);
      FOR_EACH_VEC_ELT (*cleanups, i, t)
	push_cleanup (NULL_TREE, t, false);
    }
  else if (type_build_ctor_call (type)
	   || (init && CLASS_TYPE_P (strip_array_types (type))))
    {
      if (TREE_CODE (type) == ARRAY_TYPE)
	{
	  if (can_init_array_with_p (type, init))
	    {
	      if (TYPE_DOMAIN (type) && TYPE_MAX_VALUE (TYPE_DOMAIN (type)))
		{
		  /* Initialize the array only if it's not a flexible
		     array member (i.e., if it has an upper bound).  */
		  init = build_vec_init_expr (type, init, tf_warning_or_error);
		  init = cp_build_init_expr (decl, init);
		  finish_expr_stmt (init);
		}
	    }
	  else
	    error ("invalid initializer for array member %q#D", member);
	}
      else
	{
	  int flags = LOOKUP_NORMAL;
	  if (DECL_DEFAULTED_FN (current_function_decl))
	    flags |= LOOKUP_DEFAULTED;
	  if (CP_TYPE_CONST_P (type)
	      && init == NULL_TREE
	      && default_init_uninitialized_part (type))
	    {
	      /* TYPE_NEEDS_CONSTRUCTING can be set just because we have a
		 vtable; still give this diagnostic.  */
	      auto_diagnostic_group d;
	      if (permerror (DECL_SOURCE_LOCATION (current_function_decl),
			     "uninitialized const member in %q#T", type))
		inform (DECL_SOURCE_LOCATION (member),
			"%q#D should be initialized", member );
	    }
	  finish_expr_stmt (build_aggr_init (decl, init, flags,
					     tf_warning_or_error));
	}
    }
  else
    {
      if (init == NULL_TREE)
	{
	  tree core_type;
	  /* member traversal: note it leaves init NULL */
	  if (TYPE_REF_P (type))
	    {
	      auto_diagnostic_group d;
	      if (permerror (DECL_SOURCE_LOCATION (current_function_decl),
			     "uninitialized reference member in %q#T", type))
		inform (DECL_SOURCE_LOCATION (member),
			"%q#D should be initialized", member);
	    }
	  else if (CP_TYPE_CONST_P (type))
	    {
	      auto_diagnostic_group d;
	      if (permerror (DECL_SOURCE_LOCATION (current_function_decl),
			     "uninitialized const member in %q#T", type))
		  inform (DECL_SOURCE_LOCATION (member),
			  "%q#D should be initialized", member );
	    }

	  core_type = strip_array_types (type);

	  if (CLASS_TYPE_P (core_type)
	      && (CLASSTYPE_READONLY_FIELDS_NEED_INIT (core_type)
		  || CLASSTYPE_REF_FIELDS_NEED_INIT (core_type)))
	    diagnose_uninitialized_cst_or_ref_member (core_type,
						      /*using_new=*/false,
						      /*complain=*/true);

	  /* We left the member uninitialized.  */
	  member_initialized_p = false;
	}

      maybe_warn_list_ctor (member, init);

      if (init)
	finish_expr_stmt (cp_build_modify_expr (input_location, decl,
						INIT_EXPR, init,
						tf_warning_or_error));
    }

  if (member_initialized_p && warn_uninitialized)
    /* This member is now initialized, remove it from the uninitialized
       set.  */
    uninitialized.remove (member);

  if (type_build_dtor_call (type))
    {
      tree expr;

      expr = build_class_member_access_expr (current_class_ref, member,
					     /*access_path=*/NULL_TREE,
					     /*preserve_reference=*/false,
					     tf_warning_or_error);
      expr = build_delete (input_location,
			   type, expr, sfk_complete_destructor,
			   LOOKUP_NONVIRTUAL|LOOKUP_DESTRUCTOR, 0,
			   tf_warning_or_error);

      if (expr != error_mark_node
	  && TYPE_HAS_NONTRIVIAL_DESTRUCTOR (type))
	finish_eh_cleanup (expr);
    }
}

/* Returns a TREE_LIST containing (as the TREE_PURPOSE of each node) all
   the FIELD_DECLs on the TYPE_FIELDS list for T, in reverse order.  */

static tree
build_field_list (tree t, tree list, int *uses_unions_or_anon_p)
{
  tree fields;

  /* Note whether or not T is a union.  */
  if (TREE_CODE (t) == UNION_TYPE)
    *uses_unions_or_anon_p = 1;

  for (fields = TYPE_FIELDS (t); fields; fields = DECL_CHAIN (fields))
    {
      tree fieldtype;

      /* Skip CONST_DECLs for enumeration constants and so forth.  */
      if (TREE_CODE (fields) != FIELD_DECL || DECL_ARTIFICIAL (fields))
	continue;

      fieldtype = TREE_TYPE (fields);

      /* For an anonymous struct or union, we must recursively
	 consider the fields of the anonymous type.  They can be
	 directly initialized from the constructor.  */
      if (ANON_AGGR_TYPE_P (fieldtype))
	{
	  /* Add this field itself.  Synthesized copy constructors
	     initialize the entire aggregate.  */
	  list = tree_cons (fields, NULL_TREE, list);
	  /* And now add the fields in the anonymous aggregate.  */
	  list = build_field_list (fieldtype, list, uses_unions_or_anon_p);
	  *uses_unions_or_anon_p = 1;
	}
      /* Add this field.  */
      else if (DECL_NAME (fields))
	list = tree_cons (fields, NULL_TREE, list);
    }

  return list;
}

/* Return the innermost aggregate scope for FIELD, whether that is
   the enclosing class or an anonymous aggregate within it.  */

static tree
innermost_aggr_scope (tree field)
{
  if (ANON_AGGR_TYPE_P (TREE_TYPE (field)))
    return TREE_TYPE (field);
  else
    return DECL_CONTEXT (field);
}

/* The MEM_INITS are a TREE_LIST.  The TREE_PURPOSE of each list gives
   a FIELD_DECL or BINFO in T that needs initialization.  The
   TREE_VALUE gives the initializer, or list of initializer arguments.

   Return a TREE_LIST containing all of the initializations required
   for T, in the order in which they should be performed.  The output
   list has the same format as the input.  */

static tree
sort_mem_initializers (tree t, tree mem_inits)
{
  tree init;
  tree base, binfo, base_binfo;
  tree sorted_inits;
  tree next_subobject;
  vec<tree, va_gc> *vbases;
  int i;
  int uses_unions_or_anon_p = 0;

  /* Build up a list of initializations.  The TREE_PURPOSE of entry
     will be the subobject (a FIELD_DECL or BINFO) to initialize.  The
     TREE_VALUE will be the constructor arguments, or NULL if no
     explicit initialization was provided.  */
  sorted_inits = NULL_TREE;

  /* Process the virtual bases.  */
  for (vbases = CLASSTYPE_VBASECLASSES (t), i = 0;
       vec_safe_iterate (vbases, i, &base); i++)
    sorted_inits = tree_cons (base, NULL_TREE, sorted_inits);

  /* Process the direct bases.  */
  for (binfo = TYPE_BINFO (t), i = 0;
       BINFO_BASE_ITERATE (binfo, i, base_binfo); ++i)
    if (!BINFO_VIRTUAL_P (base_binfo))
      sorted_inits = tree_cons (base_binfo, NULL_TREE, sorted_inits);

  /* Process the non-static data members.  */
  sorted_inits = build_field_list (t, sorted_inits, &uses_unions_or_anon_p);
  /* Reverse the entire list of initializations, so that they are in
     the order that they will actually be performed.  */
  sorted_inits = nreverse (sorted_inits);

  /* If the user presented the initializers in an order different from
     that in which they will actually occur, we issue a warning.  Keep
     track of the next subobject which can be explicitly initialized
     without issuing a warning.  */
  next_subobject = sorted_inits;

  /* Go through the explicit initializers, filling in TREE_PURPOSE in
     the SORTED_INITS.  */
  for (init = mem_inits; init; init = TREE_CHAIN (init))
    {
      tree subobject;
      tree subobject_init;

      subobject = TREE_PURPOSE (init);

      /* If the explicit initializers are in sorted order, then
	 SUBOBJECT will be NEXT_SUBOBJECT, or something following
	 it.  */
      for (subobject_init = next_subobject;
	   subobject_init;
	   subobject_init = TREE_CHAIN (subobject_init))
	if (TREE_PURPOSE (subobject_init) == subobject)
	  break;

      /* Issue a warning if the explicit initializer order does not
	 match that which will actually occur.
	 ??? Are all these on the correct lines?  */
      if (warn_reorder && !subobject_init)
	{
	  if (TREE_CODE (TREE_PURPOSE (next_subobject)) == FIELD_DECL)
	    warning_at (DECL_SOURCE_LOCATION (TREE_PURPOSE (next_subobject)),
			OPT_Wreorder, "%qD will be initialized after",
			TREE_PURPOSE (next_subobject));
	  else
	    warning (OPT_Wreorder, "base %qT will be initialized after",
		     TREE_PURPOSE (next_subobject));
	  if (TREE_CODE (subobject) == FIELD_DECL)
	    warning_at (DECL_SOURCE_LOCATION (subobject),
			OPT_Wreorder, "  %q#D", subobject);
	  else
	    warning (OPT_Wreorder, "  base %qT", subobject);
	  warning_at (DECL_SOURCE_LOCATION (current_function_decl),
		      OPT_Wreorder, "  when initialized here");
	}

      /* Look again, from the beginning of the list.  */
      if (!subobject_init)
	{
	  subobject_init = sorted_inits;
	  while (TREE_PURPOSE (subobject_init) != subobject)
	    subobject_init = TREE_CHAIN (subobject_init);
	}

      /* It is invalid to initialize the same subobject more than
	 once.  */
      if (TREE_VALUE (subobject_init))
	{
	  if (TREE_CODE (subobject) == FIELD_DECL)
	    error_at (DECL_SOURCE_LOCATION (current_function_decl),
		      "multiple initializations given for %qD",
		      subobject);
	  else
	    error_at (DECL_SOURCE_LOCATION (current_function_decl),
		      "multiple initializations given for base %qT",
		      subobject);
	}

      /* Record the initialization.  */
      TREE_VALUE (subobject_init) = TREE_VALUE (init);
      /* Carry over the dummy TREE_TYPE node containing the source location.  */
      TREE_TYPE (subobject_init) = TREE_TYPE (init);
      next_subobject = subobject_init;
    }

  /* [class.base.init]

     If a ctor-initializer specifies more than one mem-initializer for
     multiple members of the same union (including members of
     anonymous unions), the ctor-initializer is ill-formed.

     Here we also splice out uninitialized union members.  */
  if (uses_unions_or_anon_p)
    {
      tree *last_p = NULL;
      tree *p;
      for (p = &sorted_inits; *p; )
	{
	  tree field;
	  tree ctx;

	  init = *p;

	  field = TREE_PURPOSE (init);

	  /* Skip base classes.  */
	  if (TREE_CODE (field) != FIELD_DECL)
	    goto next;

	  /* If this is an anonymous aggregate with no explicit initializer,
	     splice it out.  */
	  if (!TREE_VALUE (init) && ANON_AGGR_TYPE_P (TREE_TYPE (field)))
	    goto splice;

	  /* See if this field is a member of a union, or a member of a
	     structure contained in a union, etc.  */
	  ctx = innermost_aggr_scope (field);

	  /* If this field is not a member of a union, skip it.  */
	  if (TREE_CODE (ctx) != UNION_TYPE
	      && !ANON_AGGR_TYPE_P (ctx))
	    goto next;

	  /* If this union member has no explicit initializer and no NSDMI,
	     splice it out.  */
	  if (TREE_VALUE (init) || DECL_INITIAL (field))
	    /* OK.  */;
	  else
	    goto splice;

	  /* It's only an error if we have two initializers for the same
	     union type.  */
	  if (!last_p)
	    {
	      last_p = p;
	      goto next;
	    }

	  /* See if LAST_FIELD and the field initialized by INIT are
	     members of the same union (or the union itself). If so, there's
	     a problem, unless they're actually members of the same structure
	     which is itself a member of a union.  For example, given:

	       union { struct { int i; int j; }; };

	     initializing both `i' and `j' makes sense.  */
	  ctx = common_enclosing_class
	    (innermost_aggr_scope (field),
	     innermost_aggr_scope (TREE_PURPOSE (*last_p)));

	  if (ctx && (TREE_CODE (ctx) == UNION_TYPE
		      || ctx == TREE_TYPE (TREE_PURPOSE (*last_p))))
	    {
	      /* A mem-initializer hides an NSDMI.  */
	      if (TREE_VALUE (init) && !TREE_VALUE (*last_p))
		*last_p = TREE_CHAIN (*last_p);
	      else if (TREE_VALUE (*last_p) && !TREE_VALUE (init))
		goto splice;
	      else
		{
		  error_at (DECL_SOURCE_LOCATION (current_function_decl),
			    "initializations for multiple members of %qT",
			    ctx);
		  goto splice;
		}
	    }

	  last_p = p;

	next:
	  p = &TREE_CHAIN (*p);
	  continue;
	splice:
	  *p = TREE_CHAIN (*p);
	}
    }

  return sorted_inits;
}

/* Callback for cp_walk_tree to mark all PARM_DECLs in a tree as read.  */

static tree
mark_exp_read_r (tree *tp, int *, void *)
{
  tree t = *tp;
  if (TREE_CODE (t) == PARM_DECL)
    mark_exp_read (t);
  return NULL_TREE;
}

/* Initialize all bases and members of CURRENT_CLASS_TYPE.  MEM_INITS
   is a TREE_LIST giving the explicit mem-initializer-list for the
   constructor.  The TREE_PURPOSE of each entry is a subobject (a
   FIELD_DECL or a BINFO) of the CURRENT_CLASS_TYPE.  The TREE_VALUE
   is a TREE_LIST giving the arguments to the constructor or
   void_type_node for an empty list of arguments.  */

void
emit_mem_initializers (tree mem_inits)
{
  int flags = LOOKUP_NORMAL;

  /* We will already have issued an error message about the fact that
     the type is incomplete.  */
  if (!COMPLETE_TYPE_P (current_class_type))
    return;

  /* Keep a set holding fields that are not initialized.  */
  hash_set<tree> uninitialized;

  /* Initially that is all of them.  */
  if (warn_uninitialized)
    for (tree f = next_aggregate_field (TYPE_FIELDS (current_class_type));
	 f != NULL_TREE;
	 f = next_aggregate_field (DECL_CHAIN (f)))
      if (!DECL_ARTIFICIAL (f)
	  && !is_really_empty_class (TREE_TYPE (f), /*ignore_vptr*/false))
	uninitialized.add (f);

  if (mem_inits
      && TYPE_P (TREE_PURPOSE (mem_inits))
      && same_type_p (TREE_PURPOSE (mem_inits), current_class_type))
    {
      /* Delegating constructor. */
      gcc_assert (TREE_CHAIN (mem_inits) == NULL_TREE);
      tree ctor = perform_target_ctor (TREE_VALUE (mem_inits));
      find_uninit_fields (&ctor, &uninitialized, current_class_type);
      return;
    }

  if (DECL_DEFAULTED_FN (current_function_decl)
      && ! DECL_INHERITED_CTOR (current_function_decl))
    flags |= LOOKUP_DEFAULTED;

  /* Sort the mem-initializers into the order in which the
     initializations should be performed.  */
  mem_inits = sort_mem_initializers (current_class_type, mem_inits);

  in_base_initializer = 1;

  /* Initialize base classes.  */
  for (; (mem_inits
	  && TREE_CODE (TREE_PURPOSE (mem_inits)) != FIELD_DECL);
       mem_inits = TREE_CHAIN (mem_inits))
    {
      tree subobject = TREE_PURPOSE (mem_inits);
      tree arguments = TREE_VALUE (mem_inits);

      /* We already have issued an error message.  */
      if (arguments == error_mark_node)
	continue;

      /* Suppress access control when calling the inherited ctor.  */
      bool inherited_base = (DECL_INHERITED_CTOR (current_function_decl)
			     && flag_new_inheriting_ctors
			     && arguments);
      if (inherited_base)
	push_deferring_access_checks (dk_deferred);

      if (arguments == NULL_TREE)
	{
	  /* If these initializations are taking place in a copy constructor,
	     the base class should probably be explicitly initialized if there
	     is a user-defined constructor in the base class (other than the
	     default constructor, which will be called anyway).  */
	  if (extra_warnings
	      && DECL_COPY_CONSTRUCTOR_P (current_function_decl)
	      && type_has_user_nondefault_constructor (BINFO_TYPE (subobject)))
	    warning_at (DECL_SOURCE_LOCATION (current_function_decl),
			OPT_Wextra, "base class %q#T should be explicitly "
			"initialized in the copy constructor",
			BINFO_TYPE (subobject));
	}

      /* Initialize the base.  */
      if (!BINFO_VIRTUAL_P (subobject))
	{
	  tree base_addr;

	  base_addr = build_base_path (PLUS_EXPR, current_class_ptr,
				       subobject, 1, tf_warning_or_error);
	  expand_aggr_init_1 (subobject, NULL_TREE,
			      cp_build_fold_indirect_ref (base_addr),
			      arguments,
			      flags,
                              tf_warning_or_error);
	  expand_cleanup_for_base (subobject, NULL_TREE);
	  if (STATEMENT_LIST_TAIL (cur_stmt_list))
	    find_uninit_fields (&STATEMENT_LIST_TAIL (cur_stmt_list)->stmt,
				&uninitialized, BINFO_TYPE (subobject));
	}
      else if (!ABSTRACT_CLASS_TYPE_P (current_class_type))
	/* C++14 DR1658 Means we do not have to construct vbases of
	   abstract classes.  */
	construct_virtual_base (subobject, arguments);
      else
	/* When not constructing vbases of abstract classes, at least mark
	   the arguments expressions as read to avoid
	   -Wunused-but-set-parameter false positives.  */
	cp_walk_tree (&arguments, mark_exp_read_r, NULL, NULL);

      if (inherited_base)
	pop_deferring_access_checks ();
    }
  in_base_initializer = 0;

  /* Initialize the vptrs.  */
  initialize_vtbl_ptrs (current_class_ptr);

  /* Initialize the data members.  */
  while (mem_inits)
    {
      /* If this initializer was explicitly provided, then the dummy TREE_TYPE
	 node contains the source location.  */
      iloc_sentinel ils (EXPR_LOCATION (TREE_TYPE (mem_inits)));

      perform_member_init (TREE_PURPOSE (mem_inits),
			   TREE_VALUE (mem_inits),
			   uninitialized);

      mem_inits = TREE_CHAIN (mem_inits);
    }
}

/* Returns the address of the vtable (i.e., the value that should be
   assigned to the vptr) for BINFO.  */

tree
build_vtbl_address (tree binfo)
{
  tree binfo_for = binfo;
  tree vtbl;

  if (BINFO_VPTR_INDEX (binfo) && BINFO_VIRTUAL_P (binfo))
    /* If this is a virtual primary base, then the vtable we want to store
       is that for the base this is being used as the primary base of.  We
       can't simply skip the initialization, because we may be expanding the
       inits of a subobject constructor where the virtual base layout
       can be different.  */
    while (BINFO_PRIMARY_P (binfo_for))
      binfo_for = BINFO_INHERITANCE_CHAIN (binfo_for);

  /* Figure out what vtable BINFO's vtable is based on, and mark it as
     used.  */
  vtbl = get_vtbl_decl_for_binfo (binfo_for);
  TREE_USED (vtbl) = true;

  /* Now compute the address to use when initializing the vptr.  */
  vtbl = unshare_expr (BINFO_VTABLE (binfo_for));
  if (VAR_P (vtbl))
    vtbl = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (vtbl)), vtbl);

  return vtbl;
}

/* This code sets up the virtual function tables appropriate for
   the pointer DECL.  It is a one-ply initialization.

   BINFO is the exact type that DECL is supposed to be.  In
   multiple inheritance, this might mean "C's A" if C : A, B.  */

static void
expand_virtual_init (tree binfo, tree decl)
{
  tree vtbl, vtbl_ptr;
  tree vtt_index;

  /* Compute the initializer for vptr.  */
  vtbl = build_vtbl_address (binfo);

  /* We may get this vptr from a VTT, if this is a subobject
     constructor or subobject destructor.  */
  vtt_index = BINFO_VPTR_INDEX (binfo);
  if (vtt_index)
    {
      tree vtbl2;
      tree vtt_parm;

      /* Compute the value to use, when there's a VTT.  */
      vtt_parm = current_vtt_parm;
      vtbl2 = fold_build_pointer_plus (vtt_parm, vtt_index);
      vtbl2 = cp_build_fold_indirect_ref (vtbl2);
      vtbl2 = convert (TREE_TYPE (vtbl), vtbl2);

      /* The actual initializer is the VTT value only in the subobject
	 constructor.  In maybe_clone_body we'll substitute NULL for
	 the vtt_parm in the case of the non-subobject constructor.  */
      vtbl = build_if_in_charge (vtbl, vtbl2);
    }

  /* Compute the location of the vtpr.  */
  vtbl_ptr = build_vfield_ref (cp_build_fold_indirect_ref (decl),
			       TREE_TYPE (binfo));
  gcc_assert (vtbl_ptr != error_mark_node);

  /* Assign the vtable to the vptr.  */
  vtbl = convert_force (TREE_TYPE (vtbl_ptr), vtbl, 0, tf_warning_or_error);
  finish_expr_stmt (cp_build_modify_expr (input_location, vtbl_ptr, NOP_EXPR,
					  vtbl, tf_warning_or_error));
}

/* If an exception is thrown in a constructor, those base classes already
   constructed must be destroyed.  This function creates the cleanup
   for BINFO, which has just been constructed.  If FLAG is non-NULL,
   it is a DECL which is nonzero when this base needs to be
   destroyed.  */

static void
expand_cleanup_for_base (tree binfo, tree flag)
{
  tree expr;

  if (!type_build_dtor_call (BINFO_TYPE (binfo)))
    return;

  /* Call the destructor.  */
  expr = build_special_member_call (current_class_ref,
				    base_dtor_identifier,
				    NULL,
				    binfo,
				    LOOKUP_NORMAL | LOOKUP_NONVIRTUAL,
                                    tf_warning_or_error);

  if (TYPE_HAS_TRIVIAL_DESTRUCTOR (BINFO_TYPE (binfo)))
    return;

  if (flag)
    expr = fold_build3_loc (input_location,
			COND_EXPR, void_type_node,
			c_common_truthvalue_conversion (input_location, flag),
			expr, integer_zero_node);

  finish_eh_cleanup (expr);
}

/* Construct the virtual base-class VBASE passing the ARGUMENTS to its
   constructor.  */

static void
construct_virtual_base (tree vbase, tree arguments)
{
  tree inner_if_stmt;
  tree exp;
  tree flag;

  /* If there are virtual base classes with destructors, we need to
     emit cleanups to destroy them if an exception is thrown during
     the construction process.  These exception regions (i.e., the
     period during which the cleanups must occur) begin from the time
     the construction is complete to the end of the function.  If we
     create a conditional block in which to initialize the
     base-classes, then the cleanup region for the virtual base begins
     inside a block, and ends outside of that block.  This situation
     confuses the sjlj exception-handling code.  Therefore, we do not
     create a single conditional block, but one for each
     initialization.  (That way the cleanup regions always begin
     in the outer block.)  We trust the back end to figure out
     that the FLAG will not change across initializations, and
     avoid doing multiple tests.  */
  flag = DECL_CHAIN (DECL_ARGUMENTS (current_function_decl));
  inner_if_stmt = begin_if_stmt ();
  finish_if_stmt_cond (flag, inner_if_stmt);

  /* Compute the location of the virtual base.  If we're
     constructing virtual bases, then we must be the most derived
     class.  Therefore, we don't have to look up the virtual base;
     we already know where it is.  */
  exp = convert_to_base_statically (current_class_ref, vbase);

  expand_aggr_init_1 (vbase, current_class_ref, exp, arguments,
		      0, tf_warning_or_error);
  finish_then_clause (inner_if_stmt);
  finish_if_stmt (inner_if_stmt);

  expand_cleanup_for_base (vbase, flag);
}

/* Find the context in which this FIELD can be initialized.  */

static tree
initializing_context (tree field)
{
  tree t = DECL_CONTEXT (field);

  /* Anonymous union members can be initialized in the first enclosing
     non-anonymous union context.  */
  while (t && ANON_AGGR_TYPE_P (t))
    t = TYPE_CONTEXT (t);
  return t;
}

/* Function to give error message if member initialization specification
   is erroneous.  FIELD is the member we decided to initialize.
   TYPE is the type for which the initialization is being performed.
   FIELD must be a member of TYPE.

   MEMBER_NAME is the name of the member.  */

static int
member_init_ok_or_else (tree field, tree type, tree member_name)
{
  if (field == error_mark_node)
    return 0;
  if (!field)
    {
      error ("class %qT does not have any field named %qD", type,
	     member_name);
      return 0;
    }
  if (VAR_P (field))
    {
      error ("%q#D is a static data member; it can only be "
	     "initialized at its definition",
	     field);
      return 0;
    }
  if (TREE_CODE (field) != FIELD_DECL)
    {
      error ("%q#D is not a non-static data member of %qT",
	     field, type);
      return 0;
    }
  if (initializing_context (field) != type)
    {
      error ("class %qT does not have any field named %qD", type,
		member_name);
      return 0;
    }

  return 1;
}

/* NAME is a FIELD_DECL, an IDENTIFIER_NODE which names a field, or it
   is a _TYPE node or TYPE_DECL which names a base for that type.
   Check the validity of NAME, and return either the base _TYPE, base
   binfo, or the FIELD_DECL of the member.  If NAME is invalid, return
   NULL_TREE and issue a diagnostic.

   An old style unnamed direct single base construction is permitted,
   where NAME is NULL.  */

tree
expand_member_init (tree name)
{
  tree basetype;
  tree field;

  if (!current_class_ref)
    return NULL_TREE;

  if (!name)
    {
      /* This is an obsolete unnamed base class initializer.  The
	 parser will already have warned about its use.  */
      switch (BINFO_N_BASE_BINFOS (TYPE_BINFO (current_class_type)))
	{
	case 0:
	  error ("unnamed initializer for %qT, which has no base classes",
		 current_class_type);
	  return NULL_TREE;
	case 1:
	  basetype = BINFO_TYPE
	    (BINFO_BASE_BINFO (TYPE_BINFO (current_class_type), 0));
	  break;
	default:
	  error ("unnamed initializer for %qT, which uses multiple inheritance",
		 current_class_type);
	  return NULL_TREE;
      }
    }
  else if (TYPE_P (name))
    {
      basetype = TYPE_MAIN_VARIANT (name);
      name = TYPE_NAME (name);
    }
  else if (TREE_CODE (name) == TYPE_DECL)
    basetype = TYPE_MAIN_VARIANT (TREE_TYPE (name));
  else
    basetype = NULL_TREE;

  if (basetype)
    {
      tree class_binfo;
      tree direct_binfo;
      tree virtual_binfo;
      int i;

      if (current_template_parms
	  || same_type_p (basetype, current_class_type))
	  return basetype;

      class_binfo = TYPE_BINFO (current_class_type);
      direct_binfo = NULL_TREE;
      virtual_binfo = NULL_TREE;

      /* Look for a direct base.  */
      for (i = 0; BINFO_BASE_ITERATE (class_binfo, i, direct_binfo); ++i)
	if (SAME_BINFO_TYPE_P (BINFO_TYPE (direct_binfo), basetype))
	  break;

      /* Look for a virtual base -- unless the direct base is itself
	 virtual.  */
      if (!direct_binfo || !BINFO_VIRTUAL_P (direct_binfo))
	virtual_binfo = binfo_for_vbase (basetype, current_class_type);

      /* [class.base.init]

	 If a mem-initializer-id is ambiguous because it designates
	 both a direct non-virtual base class and an inherited virtual
	 base class, the mem-initializer is ill-formed.  */
      if (direct_binfo && virtual_binfo)
	{
	  error ("%qD is both a direct base and an indirect virtual base",
		 basetype);
	  return NULL_TREE;
	}

      if (!direct_binfo && !virtual_binfo)
	{
	  if (CLASSTYPE_VBASECLASSES (current_class_type))
	    error ("type %qT is not a direct or virtual base of %qT",
		   basetype, current_class_type);
	  else
	    error ("type %qT is not a direct base of %qT",
		   basetype, current_class_type);
	  return NULL_TREE;
	}

      return direct_binfo ? direct_binfo : virtual_binfo;
    }
  else
    {
      if (identifier_p (name))
	field = lookup_field (current_class_type, name, 1, false);
      else
	field = name;

      if (member_init_ok_or_else (field, current_class_type, name))
	return field;
    }

  return NULL_TREE;
}

/* This is like `expand_member_init', only it stores one aggregate
   value into another.

   INIT comes in two flavors: it is either a value which
   is to be stored in EXP, or it is a parameter list
   to go to a constructor, which will operate on EXP.
   If INIT is not a parameter list for a constructor, then set
   LOOKUP_ONLYCONVERTING.
   If FLAGS is LOOKUP_ONLYCONVERTING then it is the = init form of
   the initializer, if FLAGS is 0, then it is the (init) form.
   If `init' is a CONSTRUCTOR, then we emit a warning message,
   explaining that such initializations are invalid.

   If INIT resolves to a CALL_EXPR which happens to return
   something of the type we are looking for, then we know
   that we can safely use that call to perform the
   initialization.

   The virtual function table pointer cannot be set up here, because
   we do not really know its type.

   This never calls operator=().

   When initializing, nothing is CONST.

   A default copy constructor may have to be used to perform the
   initialization.

   A constructor or a conversion operator may have to be used to
   perform the initialization, but not both, as it would be ambiguous.  */

tree
build_aggr_init (tree exp, tree init, int flags, tsubst_flags_t complain)
{
  tree stmt_expr;
  tree compound_stmt;
  int destroy_temps;
  tree type = TREE_TYPE (exp);
  int was_const = TREE_READONLY (exp);
  int was_volatile = TREE_THIS_VOLATILE (exp);
  int is_global;

  if (init == error_mark_node)
    return error_mark_node;

  location_t init_loc = (init
			 ? cp_expr_loc_or_input_loc (init)
			 : location_of (exp));

  TREE_READONLY (exp) = 0;
  TREE_THIS_VOLATILE (exp) = 0;

  if (TREE_CODE (type) == ARRAY_TYPE)
    {
      tree itype = init ? TREE_TYPE (init) : NULL_TREE;
      int from_array = 0;

      if (DECL_DECOMPOSITION_P (exp))
	{
	  from_array = 1;
	  init = mark_rvalue_use (init);
	  if (init
	      && DECL_P (tree_strip_any_location_wrapper (init))
	      && !(flags & LOOKUP_ONLYCONVERTING))
	    {
	      /* Wrap the initializer in a CONSTRUCTOR so that build_vec_init
		 recognizes it as direct-initialization.  */
	      init = build_constructor_single (init_list_type_node,
					       NULL_TREE, init);
	      CONSTRUCTOR_IS_DIRECT_INIT (init) = true;
	    }
	}
      else
	{
	  /* Must arrange to initialize each element of EXP
	     from elements of INIT.  */
	  if (cv_qualified_p (type))
	    TREE_TYPE (exp) = cv_unqualified (type);
	  if (itype && cv_qualified_p (itype))
	    TREE_TYPE (init) = cv_unqualified (itype);
	  from_array = (itype && same_type_p (TREE_TYPE (init),
					      TREE_TYPE (exp)));

	  if (init && !BRACE_ENCLOSED_INITIALIZER_P (init)
	      && (!from_array
		  || (TREE_CODE (init) != CONSTRUCTOR
		      /* Can happen, eg, handling the compound-literals
			 extension (ext/complit12.C).  */
		      && TREE_CODE (init) != TARGET_EXPR)))
	    {
	      if (complain & tf_error)
		error_at (init_loc, "array must be initialized "
			  "with a brace-enclosed initializer");
	      return error_mark_node;
	    }
	}

      stmt_expr = build_vec_init (exp, NULL_TREE, init,
				  /*explicit_value_init_p=*/false,
				  from_array,
                                  complain);
      TREE_READONLY (exp) = was_const;
      TREE_THIS_VOLATILE (exp) = was_volatile;
      TREE_TYPE (exp) = type;
      /* Restore the type of init unless it was used directly.  */
      if (init && TREE_CODE (stmt_expr) != INIT_EXPR)
	TREE_TYPE (init) = itype;
      return stmt_expr;
    }

  if (is_copy_initialization (init))
    flags |= LOOKUP_ONLYCONVERTING;

  is_global = begin_init_stmts (&stmt_expr, &compound_stmt);
  destroy_temps = stmts_are_full_exprs_p ();
  current_stmt_tree ()->stmts_are_full_exprs_p = 0;
  bool ok = expand_aggr_init_1 (TYPE_BINFO (type), exp, exp,
				init, LOOKUP_NORMAL|flags, complain);
  stmt_expr = finish_init_stmts (is_global, stmt_expr, compound_stmt);
  current_stmt_tree ()->stmts_are_full_exprs_p = destroy_temps;
  TREE_READONLY (exp) = was_const;
  TREE_THIS_VOLATILE (exp) = was_volatile;
  if (!ok)
    return error_mark_node;

  if ((VAR_P (exp) || TREE_CODE (exp) == PARM_DECL)
      && TREE_SIDE_EFFECTS (stmt_expr)
      && !lookup_attribute ("warn_unused", TYPE_ATTRIBUTES (type)))
    /* Just know that we've seen something for this node.  */
    TREE_USED (exp) = 1;

  return stmt_expr;
}

static bool
expand_default_init (tree binfo, tree true_exp, tree exp, tree init, int flags,
                     tsubst_flags_t complain)
{
  tree type = TREE_TYPE (exp);

  /* It fails because there may not be a constructor which takes
     its own type as the first (or only parameter), but which does
     take other types via a conversion.  So, if the thing initializing
     the expression is a unit element of type X, first try X(X&),
     followed by initialization by X.  If neither of these work
     out, then look hard.  */
  tree rval;
  vec<tree, va_gc> *parms;

  /* If we have direct-initialization from an initializer list, pull
     it out of the TREE_LIST so the code below can see it.  */
  if (init && TREE_CODE (init) == TREE_LIST
      && DIRECT_LIST_INIT_P (TREE_VALUE (init)))
    {
      gcc_checking_assert ((flags & LOOKUP_ONLYCONVERTING) == 0
			   && TREE_CHAIN (init) == NULL_TREE);
      init = TREE_VALUE (init);
      /* Only call reshape_init if it has not been called earlier
	 by the callers.  */
      if (BRACE_ENCLOSED_INITIALIZER_P (init) && CP_AGGREGATE_TYPE_P (type))
	init = reshape_init (type, init, complain);
    }

  if (init && BRACE_ENCLOSED_INITIALIZER_P (init)
      && CP_AGGREGATE_TYPE_P (type))
    /* A brace-enclosed initializer for an aggregate.  In C++0x this can
       happen for direct-initialization, too.  */
    init = digest_init (type, init, complain);

  if (init == error_mark_node)
    return false;

  /* A CONSTRUCTOR of the target's type is a previously digested
     initializer, whether that happened just above or in
     cp_parser_late_parsing_nsdmi.

     A TARGET_EXPR with TARGET_EXPR_DIRECT_INIT_P or TARGET_EXPR_LIST_INIT_P
     set represents the whole initialization, so we shouldn't build up
     another ctor call.  */
  if (init
      && (TREE_CODE (init) == CONSTRUCTOR
	  || (TREE_CODE (init) == TARGET_EXPR
	      && (TARGET_EXPR_DIRECT_INIT_P (init)
		  || TARGET_EXPR_LIST_INIT_P (init))))
      && same_type_ignoring_top_level_qualifiers_p (TREE_TYPE (init), type))
    {
      /* Early initialization via a TARGET_EXPR only works for
	 complete objects.  */
      gcc_assert (TREE_CODE (init) == CONSTRUCTOR || true_exp == exp);

      init = cp_build_init_expr (exp, init);
      TREE_SIDE_EFFECTS (init) = 1;
      finish_expr_stmt (init);
      return true;
    }

  if (init && TREE_CODE (init) != TREE_LIST
      && (flags & LOOKUP_ONLYCONVERTING)
      && !unsafe_return_slot_p (exp))
    {
      /* Base subobjects should only get direct-initialization.  */
      gcc_assert (true_exp == exp);

      if (flags & DIRECT_BIND)
	/* Do nothing.  We hit this in two cases:  Reference initialization,
	   where we aren't initializing a real variable, so we don't want
	   to run a new constructor; and catching an exception, where we
	   have already built up the constructor call so we could wrap it
	   in an exception region.  */;
      else
	{
	  init = ocp_convert (type, init, CONV_IMPLICIT|CONV_FORCE_TEMP,
			      flags, complain | tf_no_cleanup);
	  if (init == error_mark_node)
	    return false;
	}

      /* We need to protect the initialization of a catch parm with a
	 call to terminate(), which shows up as a MUST_NOT_THROW_EXPR
	 around the TARGET_EXPR for the copy constructor.  See
	 initialize_handler_parm.  */
      tree *p = &init;
      while (TREE_CODE (*p) == MUST_NOT_THROW_EXPR
	     || TREE_CODE (*p) == CLEANUP_POINT_EXPR)
	{
	  /* Avoid voidify_wrapper_expr making a temporary.  */
	  TREE_TYPE (*p) = void_type_node;
	  p = &TREE_OPERAND (*p, 0);
	}
      *p = cp_build_init_expr (exp, *p);
      finish_expr_stmt (init);
      return true;
    }

  if (init == NULL_TREE)
    parms = NULL;
  else if (TREE_CODE (init) == TREE_LIST && !TREE_TYPE (init))
    {
      parms = make_tree_vector ();
      for (; init != NULL_TREE; init = TREE_CHAIN (init))
	vec_safe_push (parms, TREE_VALUE (init));
    }
  else
    parms = make_tree_vector_single (init);

  if (exp == current_class_ref && current_function_decl
      && DECL_HAS_IN_CHARGE_PARM_P (current_function_decl))
    {
      /* Delegating constructor. */
      tree complete;
      tree base;
      tree elt; unsigned i;

      /* Unshare the arguments for the second call.  */
      releasing_vec parms2;
      FOR_EACH_VEC_SAFE_ELT (parms, i, elt)
	{
	  elt = break_out_target_exprs (elt);
	  vec_safe_push (parms2, elt);
	}
      complete = build_special_member_call (exp, complete_ctor_identifier,
					    &parms2, binfo, flags,
					    complain);
      complete = fold_build_cleanup_point_expr (void_type_node, complete);

      base = build_special_member_call (exp, base_ctor_identifier,
					&parms, binfo, flags,
					complain);
      base = fold_build_cleanup_point_expr (void_type_node, base);
      if (complete == error_mark_node || base == error_mark_node)
	return false;
      rval = build_if_in_charge (complete, base);
    }
   else
    {
      tree ctor_name = (true_exp == exp
			? complete_ctor_identifier : base_ctor_identifier);

      rval = build_special_member_call (exp, ctor_name, &parms, binfo, flags,
					complain);
      if (rval == error_mark_node)
	return false;
    }

  if (parms != NULL)
    release_tree_vector (parms);

  if (exp == true_exp && TREE_CODE (rval) == CALL_EXPR)
    {
      tree fn = get_callee_fndecl (rval);
      if (fn && DECL_DECLARED_CONSTEXPR_P (fn))
	{
	  tree e = maybe_constant_init (rval, exp);
	  if (TREE_CONSTANT (e))
	    rval = cp_build_init_expr (exp, e);
	}
    }

  /* FIXME put back convert_to_void?  */
  if (TREE_SIDE_EFFECTS (rval))
    finish_expr_stmt (rval);

  return true;
}

/* This function is responsible for initializing EXP with INIT
   (if any).  Returns true on success, false on failure.

   BINFO is the binfo of the type for who we are performing the
   initialization.  For example, if W is a virtual base class of A and B,
   and C : A, B.
   If we are initializing B, then W must contain B's W vtable, whereas
   were we initializing C, W must contain C's W vtable.

   TRUE_EXP is nonzero if it is the true expression being initialized.
   In this case, it may be EXP, or may just contain EXP.  The reason we
   need this is because if EXP is a base element of TRUE_EXP, we
   don't necessarily know by looking at EXP where its virtual
   baseclass fields should really be pointing.  But we do know
   from TRUE_EXP.  In constructors, we don't know anything about
   the value being initialized.

   FLAGS is just passed to `build_new_method_call'.  See that function
   for its description.  */

static bool
expand_aggr_init_1 (tree binfo, tree true_exp, tree exp, tree init, int flags,
                    tsubst_flags_t complain)
{
  tree type = TREE_TYPE (exp);

  gcc_assert (init != error_mark_node && type != error_mark_node);
  gcc_assert (building_stmt_list_p ());

  /* Use a function returning the desired type to initialize EXP for us.
     If the function is a constructor, and its first argument is
     NULL_TREE, know that it was meant for us--just slide exp on
     in and expand the constructor.  Constructors now come
     as TARGET_EXPRs.  */

  if (init && VAR_P (exp)
      && COMPOUND_LITERAL_P (init))
    {
      vec<tree, va_gc> *cleanups = NULL;
      /* If store_init_value returns NULL_TREE, the INIT has been
	 recorded as the DECL_INITIAL for EXP.  That means there's
	 nothing more we have to do.  */
      init = store_init_value (exp, init, &cleanups, flags);
      if (init)
	finish_expr_stmt (init);
      gcc_assert (!cleanups);
      return true;
    }

  /* List-initialization from {} becomes value-initialization for non-aggregate
     classes with default constructors.  Handle this here when we're
     initializing a base, so protected access works.  */
  if (exp != true_exp && init && TREE_CODE (init) == TREE_LIST)
    {
      tree elt = TREE_VALUE (init);
      if (DIRECT_LIST_INIT_P (elt)
	  && CONSTRUCTOR_ELTS (elt) == 0
	  && CLASSTYPE_NON_AGGREGATE (type)
	  && TYPE_HAS_DEFAULT_CONSTRUCTOR (type))
	init = void_type_node;
    }

  /* If an explicit -- but empty -- initializer list was present,
     that's value-initialization.  */
  if (init == void_type_node)
    {
      /* If the type has data but no user-provided default ctor, we need to zero
	 out the object.  */
      if (type_has_non_user_provided_default_constructor (type)
	  && !is_really_empty_class (type, /*ignore_vptr*/true))
	{
	  tree field_size = NULL_TREE;
	  if (exp != true_exp && CLASSTYPE_AS_BASE (type) != type)
	    /* Don't clobber already initialized virtual bases.  */
	    field_size = TYPE_SIZE (CLASSTYPE_AS_BASE (type));
	  init = build_zero_init_1 (type, NULL_TREE, /*static_storage_p=*/false,
				    field_size);
	  init = cp_build_init_expr (exp, init);
	  finish_expr_stmt (init);
	}

      /* If we don't need to mess with the constructor at all,
	 then we're done.  */
      if (! type_build_ctor_call (type))
	return true;

      /* Otherwise fall through and call the constructor.  */
      init = NULL_TREE;
    }

  /* We know that expand_default_init can handle everything we want
     at this point.  */
  return expand_default_init (binfo, true_exp, exp, init, flags, complain);
}

/* Report an error if TYPE is not a user-defined, class type.  If
   OR_ELSE is nonzero, give an error message.  */

int
is_class_type (tree type, int or_else)
{
  if (type == error_mark_node)
    return 0;

  if (! CLASS_TYPE_P (type))
    {
      if (or_else)
	error ("%qT is not a class type", type);
      return 0;
    }
  return 1;
}

/* Returns true iff the initializer INIT represents copy-initialization
   (and therefore we must set LOOKUP_ONLYCONVERTING when processing it).  */

bool
is_copy_initialization (tree init)
{
  return (init && init != void_type_node
	  && TREE_CODE (init) != TREE_LIST
	  && !(TREE_CODE (init) == TARGET_EXPR
	       && TARGET_EXPR_DIRECT_INIT_P (init))
	  && !DIRECT_LIST_INIT_P (init));
}

/* Build a reference to a member of an aggregate.  This is not a C++
   `&', but really something which can have its address taken, and
   then act as a pointer to member, for example TYPE :: FIELD can have
   its address taken by saying & TYPE :: FIELD.  ADDRESS_P is true if
   this expression is the operand of "&".

   @@ Prints out lousy diagnostics for operator <typename>
   @@ fields.

   @@ This function should be rewritten and placed in search.cc.  */

tree
build_offset_ref (tree type, tree member, bool address_p,
		  tsubst_flags_t complain)
{
  tree decl;
  tree basebinfo = NULL_TREE;

  /* class templates can come in as TEMPLATE_DECLs here.  */
  if (TREE_CODE (member) == TEMPLATE_DECL)
    return member;

  if (dependent_scope_p (type) || type_dependent_expression_p (member))
    return build_qualified_name (NULL_TREE, type, member,
				  /*template_p=*/false);

  gcc_assert (TYPE_P (type));
  if (! is_class_type (type, 1))
    return error_mark_node;

  gcc_assert (DECL_P (member) || BASELINK_P (member));
  /* Callers should call mark_used before this point, except for functions.  */
  gcc_assert (!DECL_P (member) || TREE_USED (member)
	      || TREE_CODE (member) == FUNCTION_DECL);

  type = TYPE_MAIN_VARIANT (type);
  if (!COMPLETE_OR_OPEN_TYPE_P (complete_type (type)))
    {
      if (complain & tf_error)
	error ("incomplete type %qT does not have member %qD", type, member);
      return error_mark_node;
    }

  /* Entities other than non-static members need no further
     processing.  */
  if (TREE_CODE (member) == TYPE_DECL)
    return member;
  if (VAR_P (member) || TREE_CODE (member) == CONST_DECL)
    return convert_from_reference (member);

  if (TREE_CODE (member) == FIELD_DECL && DECL_C_BIT_FIELD (member))
    {
      if (complain & tf_error)
	error ("invalid pointer to bit-field %qD", member);
      return error_mark_node;
    }

  /* Set up BASEBINFO for member lookup.  */
  decl = maybe_dummy_object (type, &basebinfo);

  /* A lot of this logic is now handled in lookup_member.  */
  if (BASELINK_P (member))
    {
      /* Go from the TREE_BASELINK to the member function info.  */
      tree t = BASELINK_FUNCTIONS (member);

      if (TREE_CODE (t) != TEMPLATE_ID_EXPR && !really_overloaded_fn (t))
	{
	  /* Get rid of a potential OVERLOAD around it.  */
	  t = OVL_FIRST (t);

	  /* Unique functions are handled easily.  */

	  /* For non-static member of base class, we need a special rule
	     for access checking [class.protected]:

	       If the access is to form a pointer to member, the
	       nested-name-specifier shall name the derived class
	       (or any class derived from that class).  */
	  bool ok;
	  if (address_p && DECL_P (t)
	      && DECL_NONSTATIC_MEMBER_P (t))
	    ok = perform_or_defer_access_check (TYPE_BINFO (type), t, t,
						complain);
	  else
	    ok = perform_or_defer_access_check (basebinfo, t, t,
						complain);
	  if (!ok)
	    return error_mark_node;
	  if (DECL_STATIC_FUNCTION_P (t))
	    return member;
	  member = t;
	}
      else
	TREE_TYPE (member) = unknown_type_node;
    }
  else if (address_p && TREE_CODE (member) == FIELD_DECL)
    {
      /* We need additional test besides the one in
	 check_accessibility_of_qualified_id in case it is
	 a pointer to non-static member.  */
      if (!perform_or_defer_access_check (TYPE_BINFO (type), member, member,
					  complain))
	return error_mark_node;
    }

  if (!address_p)
    {
      /* If MEMBER is non-static, then the program has fallen afoul of
	 [expr.prim]:

	   An id-expression that denotes a non-static data member or
	   non-static member function of a class can only be used:

	   -- as part of a class member access (_expr.ref_) in which the
	   object-expression refers to the member's class or a class
	   derived from that class, or

	   -- to form a pointer to member (_expr.unary.op_), or

	   -- in the body of a non-static member function of that class or
	   of a class derived from that class (_class.mfct.non-static_), or

	   -- in a mem-initializer for a constructor for that class or for
	   a class derived from that class (_class.base.init_).  */
      if (DECL_OBJECT_MEMBER_FUNCTION_P (member))
	{
	  /* Build a representation of the qualified name suitable
	     for use as the operand to "&" -- even though the "&" is
	     not actually present.  */
	  member = build2 (OFFSET_REF, TREE_TYPE (member), decl, member);
	  /* In Microsoft mode, treat a non-static member function as if
	     it were a pointer-to-member.  */
	  if (flag_ms_extensions)
	    {
	      PTRMEM_OK_P (member) = 1;
	      return cp_build_addr_expr (member, complain);
	    }
	  if (complain & tf_error)
	    error ("invalid use of non-static member function %qD",
		   TREE_OPERAND (member, 1));
	  return error_mark_node;
	}
      else if (TREE_CODE (member) == FIELD_DECL)
	{
	  if (complain & tf_error)
	    error ("invalid use of non-static data member %qD", member);
	  return error_mark_node;
	}
      return member;
    }

  member = build2 (OFFSET_REF, TREE_TYPE (member), decl, member);
  PTRMEM_OK_P (member) = 1;
  return member;
}

/* If DECL is a scalar enumeration constant or variable with a
   constant initializer, return the initializer (or, its initializers,
   recursively); otherwise, return DECL.  If STRICT_P, the
   initializer is only returned if DECL is a
   constant-expression.  If RETURN_AGGREGATE_CST_OK_P, it is ok to
   return an aggregate constant.  If UNSHARE_P, return an unshared
   copy of the initializer.  */

static tree
constant_value_1 (tree decl, bool strict_p, bool return_aggregate_cst_ok_p,
		  bool unshare_p)
{
  while (TREE_CODE (decl) == CONST_DECL
	 || decl_constant_var_p (decl)
	 || (!strict_p && VAR_P (decl)
	     && CP_TYPE_CONST_NON_VOLATILE_P (TREE_TYPE (decl))))
    {
      tree init;
      /* If DECL is a static data member in a template
	 specialization, we must instantiate it here.  The
	 initializer for the static data member is not processed
	 until needed; we need it now.  */
      mark_used (decl, tf_none);
      init = DECL_INITIAL (decl);
      if (init == error_mark_node)
	{
	  if (TREE_CODE (decl) == CONST_DECL
	      || DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P (decl))
	    /* Treat the error as a constant to avoid cascading errors on
	       excessively recursive template instantiation (c++/9335).  */
	    return init;
	  else
	    return decl;
	}
      /* Initializers in templates are generally expanded during
	 instantiation, so before that for const int i(2)
	 INIT is a TREE_LIST with the actual initializer as
	 TREE_VALUE.  */
      if (processing_template_decl
	  && init
	  && TREE_CODE (init) == TREE_LIST
	  && TREE_CHAIN (init) == NULL_TREE)
	init = TREE_VALUE (init);
      /* Instantiate a non-dependent initializer for user variables.  We
	 mustn't do this for the temporary for an array compound literal;
	 trying to instatiate the initializer will keep creating new
	 temporaries until we crash.  Probably it's not useful to do it for
	 other artificial variables, either.  */
      if (!DECL_ARTIFICIAL (decl))
	init = instantiate_non_dependent_or_null (init);
      if (!init
	  || !TREE_TYPE (init)
	  || !TREE_CONSTANT (init)
	  || (!return_aggregate_cst_ok_p
	      /* Unless RETURN_AGGREGATE_CST_OK_P is true, do not
		 return an aggregate constant (of which string
		 literals are a special case), as we do not want
		 to make inadvertent copies of such entities, and
		 we must be sure that their addresses are the
 		 same everywhere.  */
	      && (TREE_CODE (init) == CONSTRUCTOR
		  || TREE_CODE (init) == STRING_CST)))
	break;
      /* Don't return a CONSTRUCTOR for a variable with partial run-time
	 initialization, since it doesn't represent the entire value.
	 Similarly for VECTOR_CSTs created by cp_folding those
	 CONSTRUCTORs.  */
      if ((TREE_CODE (init) == CONSTRUCTOR
	   || TREE_CODE (init) == VECTOR_CST)
	  && !DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P (decl))
	break;
      /* If the variable has a dynamic initializer, don't use its
	 DECL_INITIAL which doesn't reflect the real value.  */
      if (VAR_P (decl)
	  && TREE_STATIC (decl)
	  && !DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P (decl)
	  && DECL_NONTRIVIALLY_INITIALIZED_P (decl))
	break;
      decl = init;
    }
  return unshare_p ? unshare_expr (decl) : decl;
}

/* If DECL is a CONST_DECL, or a constant VAR_DECL initialized by constant
   of integral or enumeration type, or a constexpr variable of scalar type,
   then return that value.  These are those variables permitted in constant
   expressions by [5.19/1].  */

tree
scalar_constant_value (tree decl)
{
  return constant_value_1 (decl, /*strict_p=*/true,
			   /*return_aggregate_cst_ok_p=*/false,
			   /*unshare_p=*/true);
}

/* Like scalar_constant_value, but can also return aggregate initializers.
   If UNSHARE_P, return an unshared copy of the initializer.  */

tree
decl_really_constant_value (tree decl, bool unshare_p /*= true*/)
{
  return constant_value_1 (decl, /*strict_p=*/true,
			   /*return_aggregate_cst_ok_p=*/true,
			   /*unshare_p=*/unshare_p);
}

/* A more relaxed version of decl_really_constant_value, used by the
   common C/C++ code.  */

tree
decl_constant_value (tree decl, bool unshare_p)
{
  return constant_value_1 (decl, /*strict_p=*/processing_template_decl,
			   /*return_aggregate_cst_ok_p=*/true,
			   /*unshare_p=*/unshare_p);
}

tree
decl_constant_value (tree decl)
{
  return decl_constant_value (decl, /*unshare_p=*/true);
}

/* Common subroutines of build_new and build_vec_delete.  */

/* Build and return a NEW_EXPR.  If NELTS is non-NULL, TYPE[NELTS] is
   the type of the object being allocated; otherwise, it's just TYPE.
   INIT is the initializer, if any.  USE_GLOBAL_NEW is true if the
   user explicitly wrote "::operator new".  PLACEMENT, if non-NULL, is
   a vector of arguments to be provided as arguments to a placement
   new operator.  This routine performs no semantic checks; it just
   creates and returns a NEW_EXPR.  */

static tree
build_raw_new_expr (location_t loc, vec<tree, va_gc> *placement, tree type,
		    tree nelts, vec<tree, va_gc> *init, int use_global_new)
{
  tree init_list;
  tree new_expr;

  /* If INIT is NULL, the we want to store NULL_TREE in the NEW_EXPR.
     If INIT is not NULL, then we want to store VOID_ZERO_NODE.  This
     permits us to distinguish the case of a missing initializer "new
     int" from an empty initializer "new int()".  */
  if (init == NULL)
    init_list = NULL_TREE;
  else if (init->is_empty ())
    init_list = void_node;
  else
    init_list = build_tree_list_vec (init);

  new_expr = build4_loc (loc, NEW_EXPR, build_pointer_type (type),
			 build_tree_list_vec (placement), type, nelts,
			 init_list);
  NEW_EXPR_USE_GLOBAL (new_expr) = use_global_new;
  TREE_SIDE_EFFECTS (new_expr) = 1;

  return new_expr;
}

/* Diagnose uninitialized const members or reference members of type
   TYPE. USING_NEW is used to disambiguate the diagnostic between a
   new expression without a new-initializer and a declaration. Returns
   the error count. */

static int
diagnose_uninitialized_cst_or_ref_member_1 (tree type, tree origin,
					    bool using_new, bool complain)
{
  tree field;
  int error_count = 0;

  if (type_has_user_provided_constructor (type))
    return 0;

  for (field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
    {
      tree field_type;

      if (TREE_CODE (field) != FIELD_DECL)
	continue;

      field_type = strip_array_types (TREE_TYPE (field));

      if (type_has_user_provided_constructor (field_type))
	continue;

      if (TYPE_REF_P (field_type))
	{
	  ++ error_count;
	  if (complain)
	    {
	      auto_diagnostic_group d;
	      if (DECL_CONTEXT (field) == origin)
		{
		  if (using_new)
		    error ("uninitialized reference member in %q#T "
			   "using %<new%> without new-initializer", origin);
		  else
		    error ("uninitialized reference member in %q#T", origin);
		}
	      else
		{
		  if (using_new)
		    error ("uninitialized reference member in base %q#T "
			   "of %q#T using %<new%> without new-initializer",
			   DECL_CONTEXT (field), origin);
		  else
		    error ("uninitialized reference member in base %q#T "
			   "of %q#T", DECL_CONTEXT (field), origin);
		}
	      inform (DECL_SOURCE_LOCATION (field),
		      "%q#D should be initialized", field);
	    }
	}

      if (CP_TYPE_CONST_P (field_type))
	{
	  ++ error_count;
	  if (complain)
	    {
	      auto_diagnostic_group d;
	      if (DECL_CONTEXT (field) == origin)
		{
		  if (using_new)
		    error ("uninitialized const member in %q#T "
			   "using %<new%> without new-initializer", origin);
		  else
		    error ("uninitialized const member in %q#T", origin);
		}
	      else
		{
		  if (using_new)
		    error ("uninitialized const member in base %q#T "
			   "of %q#T using %<new%> without new-initializer",
			   DECL_CONTEXT (field), origin);
		  else
		    error ("uninitialized const member in base %q#T "
			   "of %q#T", DECL_CONTEXT (field), origin);
		}
	      inform (DECL_SOURCE_LOCATION (field),
		      "%q#D should be initialized", field);
	    }
	}

      if (CLASS_TYPE_P (field_type))
	error_count
	  += diagnose_uninitialized_cst_or_ref_member_1 (field_type, origin,
							 using_new, complain);
    }
  return error_count;
}

int
diagnose_uninitialized_cst_or_ref_member (tree type, bool using_new, bool complain)
{
  return diagnose_uninitialized_cst_or_ref_member_1 (type, type, using_new, complain);
}

/* Call __cxa_bad_array_new_length to indicate that the size calculation
   overflowed.  Pretend it returns sizetype so that it plays nicely in the
   COND_EXPR.  */

tree
throw_bad_array_new_length (void)
{
  if (!fn)
    {
      tree name = get_identifier ("__cxa_throw_bad_array_new_length");

      fn = get_global_binding (name);
      if (!fn)
	fn = push_throw_library_fn
	  (name, build_function_type_list (sizetype, NULL_TREE));
    }

  return build_cxx_call (fn, 0, NULL, tf_warning_or_error);
}

/* Attempt to verify that the argument, OPER, of a placement new expression
   refers to an object sufficiently large for an object of TYPE or an array
   of NELTS of such objects when NELTS is non-null, and issue a warning when
   it does not.  SIZE specifies the size needed to construct the object or
   array and captures the result of NELTS * sizeof (TYPE). (SIZE could be
   greater when the array under construction requires a cookie to store
   NELTS.  GCC's placement new expression stores the cookie when invoking
   a user-defined placement new operator function but not the default one.
   Placement new expressions with user-defined placement new operator are
   not diagnosed since we don't know how they use the buffer (this could
   be a future extension).  */
static void
warn_placement_new_too_small (tree type, tree nelts, tree size, tree oper)
{
  location_t loc = cp_expr_loc_or_input_loc (oper);

  STRIP_NOPS (oper);

  /* Using a function argument or a (non-array) variable as an argument
     to placement new is not checked since it's unknown what it might
     point to.  */
  if (TREE_CODE (oper) == PARM_DECL
      || VAR_P (oper)
      || TREE_CODE (oper) == COMPONENT_REF)
    return;

  /* Evaluate any constant expressions.  */
  size = fold_non_dependent_expr (size);

  access_ref ref;
  ref.eval = [](tree x){ return fold_non_dependent_expr (x); };
  ref.trail1special = warn_placement_new < 2;
  tree objsize =  compute_objsize (oper, 1, &ref);
  if (!objsize)
    return;

  /* We can only draw conclusions if ref.deref == -1,
     i.e. oper is the address of the object.  */
  if (ref.deref != -1)
    return;

  offset_int bytes_avail = wi::to_offset (objsize);
  offset_int bytes_need;

  if (CONSTANT_CLASS_P (size))
    bytes_need = wi::to_offset (size);
  else if (nelts && CONSTANT_CLASS_P (nelts))
    bytes_need = (wi::to_offset (nelts)
		  * wi::to_offset (TYPE_SIZE_UNIT (type)));
  else if (tree_fits_uhwi_p (TYPE_SIZE_UNIT (type)))
    bytes_need = wi::to_offset (TYPE_SIZE_UNIT (type));
  else
    {
      /* The type is a VLA.  */
      return;
    }

  if (bytes_avail >= bytes_need)
    return;

  /* True when the size to mention in the warning is exact as opposed
     to "at least N".  */
  const bool exact_size = (ref.offrng[0] == ref.offrng[1]
			   || ref.sizrng[1] - ref.offrng[0] == 0);

  tree opertype = ref.ref ? TREE_TYPE (ref.ref) : TREE_TYPE (oper);
  bool warned = false;
  if (nelts)
    nelts = fold_for_warn (nelts);

  auto_diagnostic_group d;
  if (nelts)
    if (CONSTANT_CLASS_P (nelts))
      warned = warning_at (loc, OPT_Wplacement_new_,
			   (exact_size
			    ? G_("placement new constructing an object "
				 "of type %<%T [%wu]%> and size %qwu "
				 "in a region of type %qT and size %qwi")
			    : G_("placement new constructing an object "
				 "of type %<%T [%wu]%> and size %qwu "
				 "in a region of type %qT and size "
				 "at most %qwu")),
			   type, tree_to_uhwi (nelts),
			   bytes_need.to_uhwi (),
			   opertype, bytes_avail.to_uhwi ());
    else
      warned = warning_at (loc, OPT_Wplacement_new_,
			   (exact_size
			    ? G_("placement new constructing an array "
				 "of objects of type %qT and size %qwu "
				 "in a region of type %qT and size %qwi")
			    : G_("placement new constructing an array "
				 "of objects of type %qT and size %qwu "
				 "in a region of type %qT and size "
				 "at most %qwu")),
			   type, bytes_need.to_uhwi (), opertype,
			   bytes_avail.to_uhwi ());
  else
    warned = warning_at (loc, OPT_Wplacement_new_,
			 (exact_size
			  ? G_("placement new constructing an object "
			       "of type %qT and size %qwu in a region "
			       "of type %qT and size %qwi")
			  : G_("placement new constructing an object "
			       "of type %qT "
			       "and size %qwu in a region of type %qT "
			       "and size at most %qwu")),
			       type, bytes_need.to_uhwi (), opertype,
			 bytes_avail.to_uhwi ());

  if (!warned || !ref.ref)
    return;

  if (ref.offrng[0] == 0 || !ref.offset_bounded ())
    /* Avoid mentioning the offset when its lower bound is zero
       or when it's impossibly large.  */
    inform (DECL_SOURCE_LOCATION (ref.ref),
	    "%qD declared here", ref.ref);
  else if (ref.offrng[0] == ref.offrng[1])
    inform (DECL_SOURCE_LOCATION (ref.ref),
	    "at offset %wi from %qD declared here",
	    ref.offrng[0].to_shwi (), ref.ref);
  else
    inform (DECL_SOURCE_LOCATION (ref.ref),
	    "at offset [%wi, %wi] from %qD declared here",
	    ref.offrng[0].to_shwi (), ref.offrng[1].to_shwi (), ref.ref);
}

/* True if alignof(T) > __STDCPP_DEFAULT_NEW_ALIGNMENT__.  */

bool
type_has_new_extended_alignment (tree t)
{
  return (aligned_new_threshold
	  && TYPE_ALIGN_UNIT (t) > (unsigned)aligned_new_threshold);
}

/* Return the alignment we expect malloc to guarantee.  This should just be
   MALLOC_ABI_ALIGNMENT, but that macro defaults to only BITS_PER_WORD for some
   reason, so don't let the threshold be smaller than max_align_t_align.  */

unsigned
malloc_alignment ()
{
  return MAX (max_align_t_align(), MALLOC_ABI_ALIGNMENT);
}

/* Determine whether an allocation function is a namespace-scope
   non-replaceable placement new function. See DR 1748.  */
static bool
std_placement_new_fn_p (tree alloc_fn)
{
  if (DECL_NAMESPACE_SCOPE_P (alloc_fn))
    {
      tree first_arg = TREE_CHAIN (TYPE_ARG_TYPES (TREE_TYPE (alloc_fn)));
      if ((TREE_VALUE (first_arg) == ptr_type_node)
	  && TREE_CHAIN (first_arg) == void_list_node)
	return true;
    }
  return false;
}

/* For element type ELT_TYPE, return the appropriate type of the heap object
   containing such element(s).  COOKIE_SIZE is the size of cookie in bytes.
   Return
   struct { size_t[COOKIE_SIZE/sizeof(size_t)]; ELT_TYPE[N]; }
   where N is nothing (flexible array member) if ITYPE2 is NULL, otherwise
   the array has ITYPE2 as its TYPE_DOMAIN.  */

tree
build_new_constexpr_heap_type (tree elt_type, tree cookie_size, tree itype2)
{
  gcc_assert (tree_fits_uhwi_p (cookie_size));
  unsigned HOST_WIDE_INT csz = tree_to_uhwi (cookie_size);
  csz /= int_size_in_bytes (sizetype);
  tree itype1 = build_index_type (size_int (csz - 1));
  tree atype1 = build_cplus_array_type (sizetype, itype1);
  tree atype2 = build_cplus_array_type (elt_type, itype2);
  tree rtype = cxx_make_type (RECORD_TYPE);
  TYPE_NAME (rtype) = heap_identifier;
  tree fld1 = build_decl (UNKNOWN_LOCATION, FIELD_DECL, NULL_TREE, atype1);
  tree fld2 = build_decl (UNKNOWN_LOCATION, FIELD_DECL, NULL_TREE, atype2);
  DECL_FIELD_CONTEXT (fld1) = rtype;
  DECL_FIELD_CONTEXT (fld2) = rtype;
  DECL_ARTIFICIAL (fld1) = true;
  DECL_ARTIFICIAL (fld2) = true;
  TYPE_FIELDS (rtype) = fld1;
  DECL_CHAIN (fld1) = fld2;
  layout_type (rtype);
  return rtype;
}

/* Help the constexpr code to find the right type for the heap variable
   by adding a NOP_EXPR around ALLOC_CALL if needed for cookie_size.
   Return ALLOC_CALL or ALLOC_CALL cast to a pointer to
   struct { size_t[cookie_size/sizeof(size_t)]; elt_type[]; }.  */

static tree
maybe_wrap_new_for_constexpr (tree alloc_call, tree elt_type, tree cookie_size)
{
  if (cxx_dialect < cxx20)
    return alloc_call;

  if (current_function_decl != NULL_TREE
      && !DECL_DECLARED_CONSTEXPR_P (current_function_decl))
    return alloc_call;

  tree call_expr = extract_call_expr (alloc_call);
  if (call_expr == error_mark_node)
    return alloc_call;

  tree alloc_call_fndecl = cp_get_callee_fndecl_nofold (call_expr);
  if (alloc_call_fndecl == NULL_TREE
      || !IDENTIFIER_NEW_OP_P (DECL_NAME (alloc_call_fndecl))
      || CP_DECL_CONTEXT (alloc_call_fndecl) != global_namespace)
    return alloc_call;

  tree rtype = build_new_constexpr_heap_type (elt_type, cookie_size,
					      NULL_TREE);
  return build_nop (build_pointer_type (rtype), alloc_call);
}

/* Generate code for a new-expression, including calling the "operator
   new" function, initializing the object, and, if an exception occurs
   during construction, cleaning up.  The arguments are as for
   build_raw_new_expr.  This may change PLACEMENT and INIT.
   TYPE is the type of the object being constructed, possibly an array
   of NELTS elements when NELTS is non-null (in "new T[NELTS]", T may
   be an array of the form U[inner], with the whole expression being
   "new U[NELTS][inner]").  */

static tree
build_new_1 (vec<tree, va_gc> **placement, tree type, tree nelts,
	     vec<tree, va_gc> **init, bool globally_qualified_p,
	     tsubst_flags_t complain)
{
  tree size, rval;
  /* True iff this is a call to "operator new[]" instead of just
     "operator new".  */
  bool array_p = false;
  /* If ARRAY_P is true, the element type of the array.  This is never
     an ARRAY_TYPE; for something like "new int[3][4]", the
     ELT_TYPE is "int".  If ARRAY_P is false, this is the same type as
     TYPE.  */
  tree elt_type;
  /* The type of the new-expression.  (This type is always a pointer
     type.)  */
  tree pointer_type;
  tree non_const_pointer_type;
  /* The most significant array bound in int[OUTER_NELTS][inner].  */
  tree outer_nelts = NULL_TREE;
  /* For arrays with a non-constant number of elements, a bounds checks
     on the NELTS parameter to avoid integer overflow at runtime. */
  tree outer_nelts_check = NULL_TREE;
  bool outer_nelts_from_type = false;
  /* Number of the "inner" elements in "new T[OUTER_NELTS][inner]".  */
  offset_int inner_nelts_count = 1;
  tree alloc_call, alloc_expr;
  /* Size of the inner array elements (those with constant dimensions). */
  offset_int inner_size;
  /* The address returned by the call to "operator new".  This node is
     a VAR_DECL and is therefore reusable.  */
  tree alloc_node;
  tree alloc_fn;
  tree cookie_expr, init_expr;
  int nothrow, check_new;
  /* If non-NULL, the number of extra bytes to allocate at the
     beginning of the storage allocated for an array-new expression in
     order to store the number of elements.  */
  tree cookie_size = NULL_TREE;
  tree placement_first;
  tree placement_expr = NULL_TREE;
  /* True if the function we are calling is a placement allocation
     function.  */
  bool placement_allocation_fn_p;
  /* True if the storage must be initialized, either by a constructor
     or due to an explicit new-initializer.  */
  bool is_initialized;
  /* The address of the thing allocated, not including any cookie.  In
     particular, if an array cookie is in use, DATA_ADDR is the
     address of the first array element.  This node is a VAR_DECL, and
     is therefore reusable.  */
  tree data_addr;
  tree orig_type = type;

  if (nelts)
    {
      outer_nelts = nelts;
      array_p = true;
    }
  else if (TREE_CODE (type) == ARRAY_TYPE)
    {
      /* Transforms new (T[N]) to new T[N].  The former is a GNU
	 extension for variable N.  (This also covers new T where T is
	 a VLA typedef.)  */
      array_p = true;
      nelts = array_type_nelts_top (type);
      outer_nelts = nelts;
      type = TREE_TYPE (type);
      outer_nelts_from_type = true;
    }

  /* Lots of logic below depends on whether we have a constant number of
     elements, so go ahead and fold it now.  */
  const_tree cst_outer_nelts = fold_non_dependent_expr (outer_nelts, complain);

  /* If our base type is an array, then make sure we know how many elements
     it has.  */
  for (elt_type = type;
       TREE_CODE (elt_type) == ARRAY_TYPE;
       elt_type = TREE_TYPE (elt_type))
    {
      tree inner_nelts = array_type_nelts_top (elt_type);
      tree inner_nelts_cst = maybe_constant_value (inner_nelts);
      if (TREE_CODE (inner_nelts_cst) == INTEGER_CST)
	{
	  wi::overflow_type overflow;
	  offset_int result = wi::mul (wi::to_offset (inner_nelts_cst),
				       inner_nelts_count, SIGNED, &overflow);
	  if (overflow)
	    {
	      if (complain & tf_error)
		error ("integer overflow in array size");
	      nelts = error_mark_node;
	    }
	  inner_nelts_count = result;
	}
      else
	{
	  if (complain & tf_error)
	    {
	      error_at (cp_expr_loc_or_input_loc (inner_nelts),
			"array size in new-expression must be constant");
	      cxx_constant_value(inner_nelts);
	    }
	  nelts = error_mark_node;
	}
      if (nelts != error_mark_node)
	nelts = cp_build_binary_op (input_location,
				    MULT_EXPR, nelts,
				    inner_nelts_cst,
				    complain);
    }

  if (!verify_type_context (input_location, TCTX_ALLOCATION, elt_type,
			    !(complain & tf_error)))
    return error_mark_node;

  if (variably_modified_type_p (elt_type, NULL_TREE) && (complain & tf_error))
    {
      error ("variably modified type not allowed in new-expression");
      return error_mark_node;
    }

  if (nelts == error_mark_node)
    return error_mark_node;

  /* Warn if we performed the (T[N]) to T[N] transformation and N is
     variable.  */
  if (outer_nelts_from_type
      && !TREE_CONSTANT (cst_outer_nelts))
    {
      if (complain & tf_warning_or_error)
	{
	  pedwarn (cp_expr_loc_or_input_loc (outer_nelts), OPT_Wvla,
		   typedef_variant_p (orig_type)
		   ? G_("non-constant array new length must be specified "
			"directly, not by %<typedef%>")
		   : G_("non-constant array new length must be specified "
			"without parentheses around the type-id"));
	}
      else
	return error_mark_node;
    }

  if (VOID_TYPE_P (elt_type))
    {
      if (complain & tf_error)
	error ("invalid type %<void%> for %<new%>");
      return error_mark_node;
    }

  if (is_std_init_list (elt_type) && !cp_unevaluated_operand)
    warning (OPT_Winit_list_lifetime,
	     "%<new%> of %<initializer_list%> does not "
	     "extend the lifetime of the underlying array");

  if (abstract_virtuals_error (ACU_NEW, elt_type, complain))
    return error_mark_node;

  is_initialized = (type_build_ctor_call (elt_type) || *init != NULL);

  if (*init == NULL && cxx_dialect < cxx11)
    {
      bool maybe_uninitialized_error = false;
      /* A program that calls for default-initialization [...] of an
	 entity of reference type is ill-formed. */
      if (CLASSTYPE_REF_FIELDS_NEED_INIT (elt_type))
	maybe_uninitialized_error = true;

      /* A new-expression that creates an object of type T initializes
	 that object as follows:
      - If the new-initializer is omitted:
        -- If T is a (possibly cv-qualified) non-POD class type
	   (or array thereof), the object is default-initialized (8.5).
	   [...]
        -- Otherwise, the object created has indeterminate
	   value. If T is a const-qualified type, or a (possibly
	   cv-qualified) POD class type (or array thereof)
	   containing (directly or indirectly) a member of
	   const-qualified type, the program is ill-formed; */

      if (CLASSTYPE_READONLY_FIELDS_NEED_INIT (elt_type))
	maybe_uninitialized_error = true;

      if (maybe_uninitialized_error
	  && diagnose_uninitialized_cst_or_ref_member (elt_type,
						       /*using_new=*/true,
						       complain & tf_error))
	return error_mark_node;
    }

  if (CP_TYPE_CONST_P (elt_type) && *init == NULL
      && default_init_uninitialized_part (elt_type))
    {
      if (complain & tf_error)
        error ("uninitialized const in %<new%> of %q#T", elt_type);
      return error_mark_node;
    }

  size = size_in_bytes (elt_type);
  if (array_p)
    {
      /* Maximum available size in bytes.  Half of the address space
	 minus the cookie size.  */
      offset_int max_size
	= wi::set_bit_in_zero <offset_int> (TYPE_PRECISION (sizetype) - 1);
      /* Maximum number of outer elements which can be allocated. */
      offset_int max_outer_nelts;
      tree max_outer_nelts_tree;

      gcc_assert (TREE_CODE (size) == INTEGER_CST);
      cookie_size = targetm.cxx.get_cookie_size (elt_type);
      gcc_assert (TREE_CODE (cookie_size) == INTEGER_CST);
      gcc_checking_assert (wi::ltu_p (wi::to_offset (cookie_size), max_size));
      /* Unconditionally subtract the cookie size.  This decreases the
	 maximum object size and is safe even if we choose not to use
	 a cookie after all.  */
      max_size -= wi::to_offset (cookie_size);
      wi::overflow_type overflow;
      inner_size = wi::mul (wi::to_offset (size), inner_nelts_count, SIGNED,
			    &overflow);
      if (overflow || wi::gtu_p (inner_size, max_size))
	{
	  if (complain & tf_error)
	    {
	      cst_size_error error;
	      if (overflow)
		error = cst_size_overflow;
	      else
		{
		  error = cst_size_too_big;
		  size = size_binop (MULT_EXPR, size,
				     wide_int_to_tree (sizetype,
						       inner_nelts_count));
		  size = cp_fully_fold (size);
		}
	      invalid_array_size_error (input_location, error, size,
					/*name=*/NULL_TREE);
	    }
	  return error_mark_node;
	}

      max_outer_nelts = wi::udiv_trunc (max_size, inner_size);
      max_outer_nelts_tree = wide_int_to_tree (sizetype, max_outer_nelts);

      size = build2 (MULT_EXPR, sizetype, size, nelts);

      if (TREE_CODE (cst_outer_nelts) == INTEGER_CST)
	{
	  if (tree_int_cst_lt (max_outer_nelts_tree, cst_outer_nelts))
	    {
	      /* When the array size is constant, check it at compile time
		 to make sure it doesn't exceed the implementation-defined
		 maximum, as required by C++ 14 (in C++ 11 this requirement
		 isn't explicitly stated but it's enforced anyway -- see
		 grokdeclarator in cp/decl.cc).  */
	      if (complain & tf_error)
		{
		  size = cp_fully_fold (size);
		  invalid_array_size_error (input_location, cst_size_too_big,
					    size, NULL_TREE);
		}
	      return error_mark_node;
	    }
	}
      else
 	{
	  /* When a runtime check is necessary because the array size
	     isn't constant, keep only the top-most seven bits (starting
	     with the most significant non-zero bit) of the maximum size
	     to compare the array size against, to simplify encoding the
	     constant maximum size in the instruction stream.  */

	  unsigned shift = (max_outer_nelts.get_precision ()) - 7
	    - wi::clz (max_outer_nelts);
	  max_outer_nelts = (max_outer_nelts >> shift) << shift;

	  outer_nelts_check = build2 (LE_EXPR, boolean_type_node,
				      outer_nelts,
				      max_outer_nelts_tree);
	}
    }

  tree align_arg = NULL_TREE;
  if (type_has_new_extended_alignment (elt_type))
    {
      unsigned align = TYPE_ALIGN_UNIT (elt_type);
      /* Also consider the alignment of the cookie, if any.  */
      if (array_p && TYPE_VEC_NEW_USES_COOKIE (elt_type))
	align = MAX (align, TYPE_ALIGN_UNIT (size_type_node));
      align_arg = build_int_cst (align_type_node, align);
    }

  alloc_fn = NULL_TREE;

  /* If PLACEMENT is a single simple pointer type not passed by
     reference, prepare to capture it in a temporary variable.  Do
     this now, since PLACEMENT will change in the calls below.  */
  placement_first = NULL_TREE;
  if (vec_safe_length (*placement) == 1
      && (TYPE_PTR_P (TREE_TYPE ((**placement)[0]))))
    placement_first = (**placement)[0];

  bool member_new_p = false;

  /* Allocate the object.  */
  tree fnname;
  tree fns;

  fnname = ovl_op_identifier (false, array_p ? VEC_NEW_EXPR : NEW_EXPR);

  member_new_p = !globally_qualified_p
		 && CLASS_TYPE_P (elt_type)
		 && (array_p
		     ? TYPE_HAS_ARRAY_NEW_OPERATOR (elt_type)
		     : TYPE_HAS_NEW_OPERATOR (elt_type));

  bool member_delete_p = (!globally_qualified_p
			  && CLASS_TYPE_P (elt_type)
			  && (array_p
			      ? TYPE_GETS_VEC_DELETE (elt_type)
			      : TYPE_GETS_REG_DELETE (elt_type)));

  if (member_new_p)
    {
      /* Use a class-specific operator new.  */
      /* If a cookie is required, add some extra space.  */
      if (array_p && TYPE_VEC_NEW_USES_COOKIE (elt_type))
	size = build2 (PLUS_EXPR, sizetype, size, cookie_size);
      else
	{
	  cookie_size = NULL_TREE;
	  /* No size arithmetic necessary, so the size check is
	     not needed. */
	  if (outer_nelts_check != NULL && inner_size == 1)
	    outer_nelts_check = NULL_TREE;
	}
      /* Perform the overflow check.  */
      tree errval = TYPE_MAX_VALUE (sizetype);
      if (cxx_dialect >= cxx11 && flag_exceptions)
	errval = throw_bad_array_new_length ();
      if (outer_nelts_check != NULL_TREE)
	size = build3 (COND_EXPR, sizetype, outer_nelts_check, size, errval);
      size = cp_fully_fold (size);
      /* Create the argument list.  */
      vec_safe_insert (*placement, 0, size);
      /* Do name-lookup to find the appropriate operator.  */
      fns = lookup_fnfields (elt_type, fnname, /*protect=*/2, complain);
      if (fns == NULL_TREE)
	{
	  if (complain & tf_error)
	    error ("no suitable %qD found in class %qT", fnname, elt_type);
	  return error_mark_node;
	}
      if (TREE_CODE (fns) == TREE_LIST)
	{
	  if (complain & tf_error)
	    {
	      auto_diagnostic_group d;
	      error ("request for member %qD is ambiguous", fnname);
	      print_candidates (fns);
	    }
	  return error_mark_node;
	}
      tree dummy = build_dummy_object (elt_type);
      alloc_call = NULL_TREE;
      if (align_arg)
	{
	  vec<tree, va_gc> *align_args
	    = vec_copy_and_insert (*placement, align_arg, 1);
	  alloc_call
	    = build_new_method_call (dummy, fns, &align_args,
				     /*conversion_path=*/NULL_TREE,
				     LOOKUP_NORMAL, &alloc_fn, tf_none);
	  /* If no matching function is found and the allocated object type
	     has new-extended alignment, the alignment argument is removed
	     from the argument list, and overload resolution is performed
	     again.  */
	  if (alloc_call == error_mark_node)
	    alloc_call = NULL_TREE;
	}
      if (!alloc_call)
	alloc_call = build_new_method_call (dummy, fns, placement,
					    /*conversion_path=*/NULL_TREE,
					    LOOKUP_NORMAL,
					    &alloc_fn, complain);
    }
  else
    {
      /* Use a global operator new.  */
      /* See if a cookie might be required.  */
      if (!(array_p && TYPE_VEC_NEW_USES_COOKIE (elt_type)))
	{
	  cookie_size = NULL_TREE;
	  /* No size arithmetic necessary, so the size check is
	     not needed. */
	  if (outer_nelts_check != NULL && inner_size == 1)
	    outer_nelts_check = NULL_TREE;
	}

      size = cp_fully_fold (size);
      /* If size is zero e.g. due to type having zero size, try to
	 preserve outer_nelts for constant expression evaluation
	 purposes.  */
      if (integer_zerop (size) && outer_nelts)
	size = build2 (MULT_EXPR, TREE_TYPE (size), size, outer_nelts);

      alloc_call = build_operator_new_call (fnname, placement,
					    &size, &cookie_size,
					    align_arg, outer_nelts_check,
					    &alloc_fn, complain);
    }

  if (alloc_call == error_mark_node)
    return error_mark_node;

  gcc_assert (alloc_fn != NULL_TREE);

  /* Now, check to see if this function is actually a placement
     allocation function.  This can happen even when PLACEMENT is NULL
     because we might have something like:

       struct S { void* operator new (size_t, int i = 0); };

     A call to `new S' will get this allocation function, even though
     there is no explicit placement argument.  If there is more than
     one argument, or there are variable arguments, then this is a
     placement allocation function.  */
  placement_allocation_fn_p
    = (type_num_arguments (TREE_TYPE (alloc_fn)) > 1
       || varargs_function_p (alloc_fn));

  if (complain & tf_warning_or_error
      && warn_aligned_new
      && !placement_allocation_fn_p
      && TYPE_ALIGN (elt_type) > malloc_alignment ()
      && (warn_aligned_new > 1
	  || CP_DECL_CONTEXT (alloc_fn) == global_namespace)
      && !aligned_allocation_fn_p (alloc_fn))
    {
      auto_diagnostic_group d;
      if (warning (OPT_Waligned_new_, "%<new%> of type %qT with extended "
		   "alignment %d", elt_type, TYPE_ALIGN_UNIT (elt_type)))
	{
	  inform (input_location, "uses %qD, which does not have an alignment "
		  "parameter", alloc_fn);
	  if (!aligned_new_threshold)
	    inform (input_location, "use %<-faligned-new%> to enable C++17 "
				    "over-aligned new support");
	}
    }

  /* If we found a simple case of PLACEMENT_EXPR above, then copy it
     into a temporary variable.  */
  if (!processing_template_decl
      && TREE_CODE (alloc_call) == CALL_EXPR
      && call_expr_nargs (alloc_call) == 2
      && TREE_CODE (TREE_TYPE (CALL_EXPR_ARG (alloc_call, 0))) == INTEGER_TYPE
      && TYPE_PTR_P (TREE_TYPE (CALL_EXPR_ARG (alloc_call, 1))))
    {
      tree placement = CALL_EXPR_ARG (alloc_call, 1);

      if (placement_first != NULL_TREE
	  && (INTEGRAL_OR_ENUMERATION_TYPE_P (TREE_TYPE (TREE_TYPE (placement)))
	      || VOID_TYPE_P (TREE_TYPE (TREE_TYPE (placement)))))
	{
	  placement_expr = get_target_expr (placement_first);
	  CALL_EXPR_ARG (alloc_call, 1)
	    = fold_convert (TREE_TYPE (placement), placement_expr);
	}

      if (!member_new_p
	  && VOID_TYPE_P (TREE_TYPE (TREE_TYPE (CALL_EXPR_ARG (alloc_call, 1)))))
	{
	  /* Attempt to make the warning point at the operator new argument.  */
	  if (placement_first)
	    placement = placement_first;

	  warn_placement_new_too_small (orig_type, nelts, size, placement);
	}
    }

  alloc_expr = alloc_call;
  if (cookie_size)
    alloc_expr = maybe_wrap_new_for_constexpr (alloc_expr, type,
					       cookie_size);

  /* In the simple case, we can stop now.  */
  pointer_type = build_pointer_type (type);
  if (!cookie_size && !is_initialized && !member_delete_p)
    return build_nop (pointer_type, alloc_expr);

  /* Store the result of the allocation call in a variable so that we can
     use it more than once.  */
  alloc_expr = get_target_expr (alloc_expr);
  alloc_node = TARGET_EXPR_SLOT (alloc_expr);

  /* Strip any COMPOUND_EXPRs from ALLOC_CALL.  */
  while (TREE_CODE (alloc_call) == COMPOUND_EXPR)
    alloc_call = TREE_OPERAND (alloc_call, 1);

  /* Preevaluate the placement args so that we don't reevaluate them for a
     placement delete.  */
  if (placement_allocation_fn_p)
    {
      tree inits;
      stabilize_call (alloc_call, &inits);
      if (inits)
	alloc_expr = build2 (COMPOUND_EXPR, TREE_TYPE (alloc_expr), inits,
			     alloc_expr);
    }

  /*        unless an allocation function is declared with an empty  excep-
     tion-specification  (_except.spec_),  throw(), it indicates failure to
     allocate storage by throwing a bad_alloc exception  (clause  _except_,
     _lib.bad.alloc_); it returns a non-null pointer otherwise If the allo-
     cation function is declared  with  an  empty  exception-specification,
     throw(), it returns null to indicate failure to allocate storage and a
     non-null pointer otherwise.

     So check for a null exception spec on the op new we just called.  */

  nothrow = TYPE_NOTHROW_P (TREE_TYPE (alloc_fn));
  check_new
    = flag_check_new || (nothrow && !std_placement_new_fn_p (alloc_fn));

  if (cookie_size)
    {
      tree cookie;
      tree cookie_ptr;
      tree size_ptr_type;

      /* Adjust so we're pointing to the start of the object.  */
      data_addr = fold_build_pointer_plus (alloc_node, cookie_size);

      /* Store the number of bytes allocated so that we can know how
	 many elements to destroy later.  We use the last sizeof
	 (size_t) bytes to store the number of elements.  */
      cookie_ptr = size_binop (MINUS_EXPR, cookie_size, size_in_bytes (sizetype));
      cookie_ptr = fold_build_pointer_plus_loc (input_location,
						alloc_node, cookie_ptr);
      size_ptr_type = build_pointer_type (sizetype);
      cookie_ptr = fold_convert (size_ptr_type, cookie_ptr);
      cookie = cp_build_fold_indirect_ref (cookie_ptr);

      cookie_expr = build2 (MODIFY_EXPR, sizetype, cookie, nelts);

      if (targetm.cxx.cookie_has_size ())
	{
	  /* Also store the element size.  */
	  cookie_ptr = fold_build_pointer_plus (cookie_ptr,
			       fold_build1_loc (input_location,
						NEGATE_EXPR, sizetype,
						size_in_bytes (sizetype)));

	  cookie = cp_build_fold_indirect_ref (cookie_ptr);
	  cookie = build2 (MODIFY_EXPR, sizetype, cookie,
			   size_in_bytes (elt_type));
	  cookie_expr = build2 (COMPOUND_EXPR, TREE_TYPE (cookie_expr),
				cookie, cookie_expr);
	}
    }
  else
    {
      cookie_expr = NULL_TREE;
      data_addr = alloc_node;
    }

  /* Now use a pointer to the type we've actually allocated.  */

  /* But we want to operate on a non-const version to start with,
     since we'll be modifying the elements.  */
  non_const_pointer_type = build_pointer_type
    (cp_build_qualified_type (type, cp_type_quals (type) & ~TYPE_QUAL_CONST));

  data_addr = fold_convert (non_const_pointer_type, data_addr);
  /* Any further uses of alloc_node will want this type, too.  */
  alloc_node = fold_convert (non_const_pointer_type, alloc_node);

  /* Now initialize the allocated object.  Note that we preevaluate the
     initialization expression, apart from the actual constructor call or
     assignment--we do this because we want to delay the allocation as long
     as possible in order to minimize the size of the exception region for
     placement delete.  */
  if (is_initialized)
    {
      bool explicit_value_init_p = false;

      if (*init != NULL && (*init)->is_empty ())
	{
	  *init = NULL;
	  explicit_value_init_p = true;
	}

      if (processing_template_decl)
	{
	  /* Avoid an ICE when converting to a base in build_simple_base_path.
	     We'll throw this all away anyway, and build_new will create
	     a NEW_EXPR.  */
	  tree t = fold_convert (build_pointer_type (elt_type), data_addr);
	  /* build_value_init doesn't work in templates, and we don't need
	     the initializer anyway since we're going to throw it away and
	     rebuild it at instantiation time, so just build up a single
	     constructor call to get any appropriate diagnostics.  */
	  init_expr = cp_build_fold_indirect_ref (t);
	  if (type_build_ctor_call (elt_type))
	    init_expr = build_special_member_call (init_expr,
						   complete_ctor_identifier,
						   init, elt_type,
						   LOOKUP_NORMAL,
						   complain);
	}
      else if (array_p)
	{
	  tree vecinit = NULL_TREE;
	  const size_t len = vec_safe_length (*init);
	  if (len == 1 && DIRECT_LIST_INIT_P ((**init)[0]))
	    {
	      vecinit = (**init)[0];
	      if (CONSTRUCTOR_NELTS (vecinit) == 0)
		/* List-value-initialization, leave it alone.  */;
	      else
		{
		  tree arraytype, domain;
		  if (TREE_CONSTANT (nelts))
		    domain = compute_array_index_type (NULL_TREE, nelts,
						       complain);
		  else
		    /* We'll check the length at runtime.  */
		    domain = NULL_TREE;
		  arraytype = build_cplus_array_type (type, domain);
		  /* If we have new char[4]{"foo"}, we have to reshape
		     so that the STRING_CST isn't wrapped in { }.  */
		  vecinit = reshape_init (arraytype, vecinit, complain);
		  /* The middle end doesn't cope with the location wrapper
		     around a STRING_CST.  */
		  STRIP_ANY_LOCATION_WRAPPER (vecinit);
		  vecinit = digest_init (arraytype, vecinit, complain);
		}
	    }
	  else if (*init)
            {
              if (complain & tf_error)
                error ("parenthesized initializer in array new");
	      return error_mark_node;
            }
	  init_expr
	    = build_vec_init (data_addr,
			      cp_build_binary_op (input_location,
						  MINUS_EXPR, outer_nelts,
						  integer_one_node,
						  complain),
			      vecinit,
			      explicit_value_init_p,
			      /*from_array=*/0,
                              complain);
	}
      else
	{
	  init_expr = cp_build_fold_indirect_ref (data_addr);

	  if (type_build_ctor_call (type) && !explicit_value_init_p)
	    {
	      init_expr = build_special_member_call (init_expr,
						     complete_ctor_identifier,
						     init, elt_type,
						     LOOKUP_NORMAL,
						     complain|tf_no_cleanup);
	    }
	  else if (explicit_value_init_p)
	    {
	      /* Something like `new int()'.  NO_CLEANUP is needed so
		 we don't try and build a (possibly ill-formed)
		 destructor.  */
	      tree val = build_value_init (type, complain | tf_no_cleanup);
	      if (val == error_mark_node)
		return error_mark_node;
	      init_expr = cp_build_init_expr (init_expr, val);
	    }
	  else
	    {
	      tree ie;

	      /* We are processing something like `new int (10)', which
		 means allocate an int, and initialize it with 10.

		 In C++20, also handle `new A(1, 2)'.  */
	      if (cxx_dialect >= cxx20
		  && AGGREGATE_TYPE_P (type)
		  && (*init)->length () > 1)
		{
		  ie = build_constructor_from_vec (init_list_type_node, *init);
		  CONSTRUCTOR_IS_DIRECT_INIT (ie) = true;
		  CONSTRUCTOR_IS_PAREN_INIT (ie) = true;
		  ie = digest_init (type, ie, complain);
		}
	      else
		ie = build_x_compound_expr_from_vec (*init, "new initializer",
						     complain);
	      init_expr = cp_build_modify_expr (input_location, init_expr,
						INIT_EXPR, ie, complain);
	    }
	  /* If the initializer uses C++14 aggregate NSDMI that refer to the
	     object being initialized, replace them now and don't try to
	     preevaluate.  */
	  bool had_placeholder = false;
	  if (!processing_template_decl
	      && TREE_CODE (init_expr) == INIT_EXPR)
	    TREE_OPERAND (init_expr, 1)
	      = replace_placeholders (TREE_OPERAND (init_expr, 1),
				      TREE_OPERAND (init_expr, 0),
				      &had_placeholder);
	}

      if (init_expr == error_mark_node)
	return error_mark_node;
    }
  else
    init_expr = NULL_TREE;

  /* If any part of the object initialization terminates by throwing an
     exception and a suitable deallocation function can be found, the
     deallocation function is called to free the memory in which the
     object was being constructed, after which the exception continues
     to propagate in the context of the new-expression. If no
     unambiguous matching deallocation function can be found,
     propagating the exception does not cause the object's memory to be
     freed.  */
  if (flag_exceptions && (init_expr || member_delete_p))
    {
      enum tree_code dcode = array_p ? VEC_DELETE_EXPR : DELETE_EXPR;
      tree cleanup;

      /* The Standard is unclear here, but the right thing to do
	 is to use the same method for finding deallocation
	 functions that we use for finding allocation functions.  */
      cleanup = (build_op_delete_call
		 (dcode,
		  alloc_node,
		  size,
		  globally_qualified_p,
		  placement_allocation_fn_p ? alloc_call : NULL_TREE,
		  alloc_fn,
		  complain));

      if (cleanup && init_expr && !processing_template_decl)
	/* Ack!  First we allocate the memory.  Then we set our sentry
	   variable to true, and expand a cleanup that deletes the
	   memory if sentry is true.  Then we run the constructor, and
	   finally clear the sentry.

	   We need to do this because we allocate the space first, so
	   if there are any temporaries with cleanups in the
	   constructor args, we need this EH region to extend until
	   end of full-expression to preserve nesting.

	   We used to try to evaluate the args first to avoid this, but
	   since C++17 [expr.new] says that "The invocation of the
	   allocation function is sequenced before the evaluations of
	   expressions in the new-initializer."  */
	{
	  tree end, sentry, begin;

	  begin = get_target_expr (boolean_true_node);
	  CLEANUP_EH_ONLY (begin) = 1;

	  sentry = TARGET_EXPR_SLOT (begin);

	  /* CLEANUP is compiler-generated, so no diagnostics.  */
	  suppress_warning (cleanup);

	  TARGET_EXPR_CLEANUP (begin)
	    = build3 (COND_EXPR, void_type_node, sentry,
		      cleanup, void_node);

	  end = build2 (MODIFY_EXPR, TREE_TYPE (sentry),
			sentry, boolean_false_node);

	  init_expr
	    = build2 (COMPOUND_EXPR, void_type_node, begin,
		      build2 (COMPOUND_EXPR, void_type_node, init_expr,
			      end));
	  /* Likewise, this is compiler-generated.  */
	  suppress_warning (init_expr);
	}
    }

  /* Now build up the return value in reverse order.  */

  rval = data_addr;

  if (init_expr)
    rval = build2 (COMPOUND_EXPR, TREE_TYPE (rval), init_expr, rval);
  if (cookie_expr)
    rval = build2 (COMPOUND_EXPR, TREE_TYPE (rval), cookie_expr, rval);

  suppress_warning (rval, OPT_Wunused_value);

  if (rval == data_addr && TREE_CODE (alloc_expr) == TARGET_EXPR)
    /* If we don't have an initializer or a cookie, strip the TARGET_EXPR
       and return the call (which doesn't need to be adjusted).  */
    rval = TARGET_EXPR_INITIAL (alloc_expr);
  else
    {
      if (check_new)
	{
	  tree ifexp = cp_build_binary_op (input_location,
					   NE_EXPR, alloc_node,
					   nullptr_node,
					   complain);
	  rval = build_conditional_expr (input_location, ifexp, rval,
					 alloc_node, complain);
	}

      /* Perform the allocation before anything else, so that ALLOC_NODE
	 has been initialized before we start using it.  */
      rval = build2 (COMPOUND_EXPR, TREE_TYPE (rval), alloc_expr, rval);
    }

  /* A new-expression is never an lvalue.  */
  gcc_assert (!obvalue_p (rval));

  return convert (pointer_type, rval);
}

/* Generate a representation for a C++ "new" expression.  *PLACEMENT
   is a vector of placement-new arguments (or NULL if none).  If NELTS
   is NULL, TYPE is the type of the storage to be allocated.  If NELTS
   is not NULL, then this is an array-new allocation; TYPE is the type
   of the elements in the array and NELTS is the number of elements in
   the array.  *INIT, if non-NULL, is the initializer for the new
   object, or an empty vector to indicate an initializer of "()".  If
   USE_GLOBAL_NEW is true, then the user explicitly wrote "::new"
   rather than just "new".  This may change PLACEMENT and INIT.  */

tree
build_new (location_t loc, vec<tree, va_gc> **placement, tree type,
	   tree nelts, vec<tree, va_gc> **init, int use_global_new,
	   tsubst_flags_t complain)
{
  tree rval;
  vec<tree, va_gc> *orig_placement = NULL;
  tree orig_nelts = NULL_TREE;
  vec<tree, va_gc> *orig_init = NULL;

  if (type == error_mark_node)
    return error_mark_node;

  if (nelts == NULL_TREE
      /* Don't do auto deduction where it might affect mangling.  */
      && (!processing_template_decl || at_function_scope_p ()))
    {
      tree auto_node = type_uses_auto (type);
      if (auto_node)
	{
	  tree d_init = NULL_TREE;
	  const size_t len = vec_safe_length (*init);
	  /* E.g. new auto(x) must have exactly one element, or
	     a {} initializer will have one element.  */
	  if (len == 1)
	    {
	      d_init = (**init)[0];
	      d_init = resolve_nondeduced_context (d_init, complain);
	    }
	  /* For the rest, e.g. new A(1, 2, 3), create a list.  */
	  else if (len > 1)
	    {
	      unsigned int n;
	      tree t;
	      tree *pp = &d_init;
	      FOR_EACH_VEC_ELT (**init, n, t)
		{
		  t = resolve_nondeduced_context (t, complain);
		  *pp = build_tree_list (NULL_TREE, t);
		  pp = &TREE_CHAIN (*pp);
		}
	    }
	  type = do_auto_deduction (type, d_init, auto_node, complain);
	}
    }

  if (processing_template_decl)
    {
      if (dependent_type_p (type)
	  || any_type_dependent_arguments_p (*placement)
	  || (nelts && type_dependent_expression_p (nelts))
	  || (nelts && *init)
	  || any_type_dependent_arguments_p (*init))
	return build_raw_new_expr (loc, *placement, type, nelts, *init,
				   use_global_new);

      orig_placement = make_tree_vector_copy (*placement);
      orig_nelts = nelts;
      if (*init)
	{
	  orig_init = make_tree_vector_copy (*init);
	  /* Also copy any CONSTRUCTORs in *init, since reshape_init and
	     digest_init clobber them in place.  */
	  for (unsigned i = 0; i < orig_init->length(); ++i)
	    {
	      tree e = (**init)[i];
	      if (TREE_CODE (e) == CONSTRUCTOR)
		(**init)[i] = copy_node (e);
	    }
	}
    }

  if (nelts)
    {
      location_t nelts_loc = cp_expr_loc_or_loc (nelts, loc);
      if (!build_expr_type_conversion (WANT_INT | WANT_ENUM, nelts, false))
        {
          if (complain & tf_error)
	    permerror (nelts_loc,
		       "size in array new must have integral type");
          else
            return error_mark_node;
        }

      /* Try to determine the constant value only for the purposes
	 of the diagnostic below but continue to use the original
	 value and handle const folding later.  */
      const_tree cst_nelts = fold_non_dependent_expr (nelts, complain);

      /* The expression in a noptr-new-declarator is erroneous if it's of
	 non-class type and its value before converting to std::size_t is
	 less than zero. ... If the expression is a constant expression,
	 the program is ill-fomed.  */
      if (TREE_CODE (cst_nelts) == INTEGER_CST
	  && !valid_array_size_p (nelts_loc, cst_nelts, NULL_TREE,
				  complain & tf_error))
	return error_mark_node;

      nelts = mark_rvalue_use (nelts);
      nelts = cp_save_expr (cp_convert (sizetype, nelts, complain));
    }

  /* ``A reference cannot be created by the new operator.  A reference
     is not an object (8.2.2, 8.4.3), so a pointer to it could not be
     returned by new.'' ARM 5.3.3 */
  if (TYPE_REF_P (type))
    {
      if (complain & tf_error)
        error_at (loc, "new cannot be applied to a reference type");
      else
        return error_mark_node;
      type = TREE_TYPE (type);
    }

  if (TREE_CODE (type) == FUNCTION_TYPE)
    {
      if (complain & tf_error)
        error_at (loc, "new cannot be applied to a function type");
      return error_mark_node;
    }

  /* P1009: Array size deduction in new-expressions.  */
  const bool array_p = TREE_CODE (type) == ARRAY_TYPE;
  if (*init
      /* If the array didn't specify its bound, we have to deduce it.  */
      && ((array_p && !TYPE_DOMAIN (type))
	  /* For C++20 array with parenthesized-init, we have to process
	     the parenthesized-list.  But don't do it for (), which is
	     value-initialization, and INIT should stay empty.  */
	  || (cxx_dialect >= cxx20
	      && (array_p || nelts)
	      && !(*init)->is_empty ())))
    {
      /* This means we have 'new T[]()'.  */
      if ((*init)->is_empty ())
	{
	  tree ctor = build_constructor (init_list_type_node, NULL);
	  CONSTRUCTOR_IS_DIRECT_INIT (ctor) = true;
	  vec_safe_push (*init, ctor);
	}
      tree &elt = (**init)[0];
      /* The C++20 'new T[](e_0, ..., e_k)' case allowed by P0960.  */
      if (!DIRECT_LIST_INIT_P (elt) && cxx_dialect >= cxx20)
	{
	  tree ctor = build_constructor_from_vec (init_list_type_node, *init);
	  CONSTRUCTOR_IS_DIRECT_INIT (ctor) = true;
	  CONSTRUCTOR_IS_PAREN_INIT (ctor) = true;
	  elt = ctor;
	  /* We've squashed all the vector elements into the first one;
	     truncate the rest.  */
	  (*init)->truncate (1);
	}
      /* Otherwise we should have 'new T[]{e_0, ..., e_k}'.  */
      if (array_p && !TYPE_DOMAIN (type))
	{
	  /* We need to reshape before deducing the bounds to handle code like

	       struct S { int x, y; };
	       new S[]{1, 2, 3, 4};

	     which should deduce S[2].	But don't change ELT itself: we want to
	     pass a list-initializer to build_new_1, even for STRING_CSTs.  */
	  tree e = elt;
	  if (BRACE_ENCLOSED_INITIALIZER_P (e))
	    e = reshape_init (type, e, complain);
	  cp_complete_array_type (&type, e, /*do_default*/false);
	}
    }

  /* The type allocated must be complete.  If the new-type-id was
     "T[N]" then we are just checking that "T" is complete here, but
     that is equivalent, since the value of "N" doesn't matter.  */
  if (!complete_type_or_maybe_complain (type, NULL_TREE, complain))
    return error_mark_node;

  rval = build_new_1 (placement, type, nelts, init, use_global_new, complain);
  if (rval == error_mark_node)
    return error_mark_node;

  if (processing_template_decl)
    {
      tree ret = build_raw_new_expr (loc, orig_placement, type, orig_nelts,
				     orig_init, use_global_new);
      release_tree_vector (orig_placement);
      release_tree_vector (orig_init);
      return ret;
    }

  /* Wrap it in a NOP_EXPR so warn_if_unused_value doesn't complain.  */
  rval = build1_loc (loc, NOP_EXPR, TREE_TYPE (rval), rval);
  suppress_warning (rval, OPT_Wunused_value);

  return rval;
}

static tree
build_vec_delete_1 (location_t loc, tree base, tree maxindex, tree type,
		    special_function_kind auto_delete_vec,
		    int use_global_delete, tsubst_flags_t complain,
		    bool in_cleanup = false)
{
  tree virtual_size;
  tree ptype = build_pointer_type (type = complete_type (type));
  tree size_exp;

  /* Temporary variables used by the loop.  */
  tree tbase, tbase_init;

  /* This is the body of the loop that implements the deletion of a
     single element, and moves temp variables to next elements.  */
  tree body;

  /* This is the LOOP_EXPR that governs the deletion of the elements.  */
  tree loop = 0;

  /* This is the thing that governs what to do after the loop has run.  */
  tree deallocate_expr = 0;

  /* This is the BIND_EXPR which holds the outermost iterator of the
     loop.  It is convenient to set this variable up and test it before
     executing any other code in the loop.
     This is also the containing expression returned by this function.  */
  tree controller = NULL_TREE;
  tree tmp;

  /* We should only have 1-D arrays here.  */
  gcc_assert (TREE_CODE (type) != ARRAY_TYPE);

  if (base == error_mark_node || maxindex == error_mark_node)
    return error_mark_node;

  if (!verify_type_context (loc, TCTX_DEALLOCATION, type,
			    !(complain & tf_error)))
    return error_mark_node;

  if (!COMPLETE_TYPE_P (type))
    {
      if (cxx_dialect > cxx23)
	{
	  if (complain & tf_error)
	    {
	      auto_diagnostic_group d;
	      int saved_errorcount = errorcount;
	      if (permerror_opt (loc, OPT_Wdelete_incomplete,
				 "operator %<delete []%> used on "
				 "incomplete type"))
		{
		  cxx_incomplete_type_inform (type);
		  if (errorcount != saved_errorcount)
		    return error_mark_node;
		}
	    }
	  else
	    return error_mark_node;
	}
      else if (complain & tf_warning)
	{
	  auto_diagnostic_group d;
	  if (warning_at (loc, OPT_Wdelete_incomplete,
			  "possible problem detected in invocation of "
			  "operator %<delete []%>"))
	    {
	      cxx_incomplete_type_diagnostic (base, type, DK_WARNING);
	      inform (loc, "neither the destructor nor the "
		      "class-specific operator %<delete []%> will be called, "
		      "even if they are declared when the class is defined");
	    }
	}
      /* This size won't actually be used.  */
      size_exp = size_one_node;
      goto no_destructor;
    }

  size_exp = size_in_bytes (type);

  if (! MAYBE_CLASS_TYPE_P (type))
    goto no_destructor;
  else if (TYPE_HAS_TRIVIAL_DESTRUCTOR (type))
    {
      /* Make sure the destructor is callable.  */
      if (type_build_dtor_call (type))
	{
	  tmp = build_delete (loc, ptype, base, sfk_complete_destructor,
			      LOOKUP_NORMAL|LOOKUP_DESTRUCTOR|LOOKUP_NONVIRTUAL,
			      1, complain);
	  if (tmp == error_mark_node)
	    return error_mark_node;
	}
      goto no_destructor;
    }

  /* The below is short by the cookie size.  */
  virtual_size = size_binop (MULT_EXPR, size_exp,
			     fold_convert (sizetype, maxindex));

  tbase = create_temporary_var (ptype);
  DECL_INITIAL (tbase)
    = fold_build_pointer_plus_loc (loc, fold_convert (ptype, base),
				   virtual_size);
  tbase_init = build_stmt (loc, DECL_EXPR, tbase);
  controller = build3 (BIND_EXPR, void_type_node, tbase, NULL_TREE, NULL_TREE);
  TREE_SIDE_EFFECTS (controller) = 1;
  BIND_EXPR_VEC_DTOR (controller) = true;

  body = build1 (EXIT_EXPR, void_type_node,
		 build2 (EQ_EXPR, boolean_type_node, tbase,
			 fold_convert (ptype, base)));
  tmp = fold_build1_loc (loc, NEGATE_EXPR, sizetype, size_exp);
  tmp = fold_build_pointer_plus (tbase, tmp);
  tmp = cp_build_modify_expr (loc, tbase, NOP_EXPR, tmp, complain);
  if (tmp == error_mark_node)
    return error_mark_node;
  body = build_compound_expr (loc, body, tmp);
  /* [expr.delete]/3: "In an array delete expression, if the dynamic type of
     the object to be deleted is not similar to its static type, the behavior
     is undefined."  So we can set LOOKUP_NONVIRTUAL.  */
  tmp = build_delete (loc, ptype, tbase, sfk_complete_destructor,
		      LOOKUP_NORMAL|LOOKUP_DESTRUCTOR|LOOKUP_NONVIRTUAL,
		      1, complain);
  if (tmp == error_mark_node)
    return error_mark_node;
  body = build_compound_expr (loc, body, tmp);

  loop = build1 (LOOP_EXPR, void_type_node, body);

  /* If one destructor throws, keep trying to clean up the rest, unless we're
     already in a build_vec_init cleanup.  */
  if (flag_exceptions && !in_cleanup && !processing_template_decl
      && !expr_noexcept_p (tmp, tf_none))
    {
      loop = build2 (TRY_CATCH_EXPR, void_type_node, loop,
		     unshare_expr (loop));
      /* Tell honor_protect_cleanup_actions to discard this on the
	 exceptional path.  */
      TRY_CATCH_IS_CLEANUP (loop) = true;
    }

  loop = build_compound_expr (loc, tbase_init, loop);

 no_destructor:
  /* Delete the storage if appropriate.  */
  if (auto_delete_vec == sfk_deleting_destructor)
    {
      tree base_tbd;

      /* The below is short by the cookie size.  */
      virtual_size = size_binop (MULT_EXPR, size_exp,
				 fold_convert (sizetype, maxindex));

      if (! TYPE_VEC_NEW_USES_COOKIE (type))
	/* no header */
	base_tbd = base;
      else
	{
	  tree cookie_size;

	  cookie_size = targetm.cxx.get_cookie_size (type);
	  base_tbd = cp_build_binary_op (loc,
					 MINUS_EXPR,
					 cp_convert (string_type_node,
						     base, complain),
					 cookie_size,
					 complain);
	  if (base_tbd == error_mark_node)
	    return error_mark_node;
	  base_tbd = cp_convert (ptype, base_tbd, complain);
	  /* True size with header.  */
	  virtual_size = size_binop (PLUS_EXPR, virtual_size, cookie_size);
	}

      deallocate_expr = build_op_delete_call (VEC_DELETE_EXPR,
					      base_tbd, virtual_size,
					      use_global_delete & 1,
					      /*placement=*/NULL_TREE,
					      /*alloc_fn=*/NULL_TREE,
					      complain);
    }

  body = loop;
  if (deallocate_expr == error_mark_node)
    return error_mark_node;
  else if (!deallocate_expr)
    ;
  else if (!body)
    body = deallocate_expr;
  else
    /* The delete operator must be called, even if a destructor
       throws.  */
    body = build2 (TRY_FINALLY_EXPR, void_type_node, body, deallocate_expr);

  if (!body)
    body = integer_zero_node;

  /* Outermost wrapper: If pointer is null, punt.  */
  tree cond = build2_loc (loc, NE_EXPR, boolean_type_node, base,
			  fold_convert (TREE_TYPE (base), nullptr_node));
  /* This is a compiler generated comparison, don't emit
     e.g. -Wnonnull-compare warning for it.  */
  suppress_warning (cond, OPT_Wnonnull_compare);
  body = build3_loc (loc, COND_EXPR, void_type_node,
		     cond, body, integer_zero_node);
  COND_EXPR_IS_VEC_DELETE (body) = true;
  body = build1 (NOP_EXPR, void_type_node, body);

  if (controller)
    {
      TREE_OPERAND (controller, 1) = body;
      body = controller;
    }

  if (TREE_CODE (base) == SAVE_EXPR)
    /* Pre-evaluate the SAVE_EXPR outside of the BIND_EXPR.  */
    body = build2 (COMPOUND_EXPR, void_type_node, base, body);

  return convert_to_void (body, ICV_CAST, complain);
}

/* Create an unnamed variable of the indicated TYPE.  */

tree
create_temporary_var (tree type)
{
  tree decl;

  decl = build_decl (input_location,
		     VAR_DECL, NULL_TREE, type);
  TREE_USED (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  DECL_IGNORED_P (decl) = 1;
  DECL_CONTEXT (decl) = current_function_decl;

  return decl;
}

/* Create a new temporary variable of the indicated TYPE, initialized
   to INIT.

   It is not entered into current_binding_level, because that breaks
   things when it comes time to do final cleanups (which take place
   "outside" the binding contour of the function).  */

tree
get_temp_regvar (tree type, tree init)
{
  tree decl;

  decl = create_temporary_var (type);
  add_decl_expr (decl);

  finish_expr_stmt (cp_build_modify_expr (input_location, decl, INIT_EXPR,
					  init, tf_warning_or_error));

  return decl;
}

/* Subroutine of build_vec_init.  Returns true if assigning to an array of
   INNER_ELT_TYPE from INIT is trivial.  */

static bool
vec_copy_assign_is_trivial (tree inner_elt_type, tree init)
{
  tree fromtype = inner_elt_type;
  if (lvalue_p (init))
    fromtype = cp_build_reference_type (fromtype, /*rval*/false);
  return is_trivially_xible (MODIFY_EXPR, inner_elt_type, fromtype);
}

/* Subroutine of build_vec_init: Check that the array has at least N
   elements.  Other parameters are local variables in build_vec_init.  */

void
finish_length_check (tree atype, tree iterator, tree obase, unsigned n)
{
  tree nelts = build_int_cst (ptrdiff_type_node, n - 1);
  if (TREE_CODE (atype) != ARRAY_TYPE)
    {
      if (flag_exceptions)
	{
	  tree c = fold_build2 (LT_EXPR, boolean_type_node, iterator,
				nelts);
	  c = build3 (COND_EXPR, void_type_node, c,
		      throw_bad_array_new_length (), void_node);
	  finish_expr_stmt (c);
	}
      /* Don't check an array new when -fno-exceptions.  */
    }
  else if (sanitize_flags_p (SANITIZE_BOUNDS)
	   && current_function_decl != NULL_TREE)
    {
      /* Make sure the last element of the initializer is in bounds. */
      finish_expr_stmt
	(ubsan_instrument_bounds
	 (input_location, obase, &nelts, /*ignore_off_by_one*/false));
    }
}

/* walk_tree callback to collect temporaries in an expression.  */

tree
find_temps_r (tree *tp, int *walk_subtrees, void *data)
{
  vec<tree*> &temps = *static_cast<auto_vec<tree*> *>(data);
  tree t = *tp;
  if (TREE_CODE (t) == TARGET_EXPR
      && !TARGET_EXPR_ELIDING_P (t))
    temps.safe_push (tp);
  else if (TYPE_P (t))
    *walk_subtrees = 0;

  return NULL_TREE;
}

/* walk_tree callback to collect temporaries in an expression that
   are allocator arguments to standard library classes.  */

static tree
find_allocator_temps_r (tree *tp, int *walk_subtrees, void *data)
{
  vec<tree*> &temps = *static_cast<auto_vec<tree*> *>(data);
  tree t = *tp;
  if (TYPE_P (t))
    {
      *walk_subtrees = 0;
      return NULL_TREE;
    }

  /* If this is a call to a constructor for a std:: class, look for
     a reference-to-allocator argument.  */
  tree fn = cp_get_callee_fndecl_nofold (t);
  if (fn && DECL_CONSTRUCTOR_P (fn)
      && decl_in_std_namespace_p (TYPE_NAME (DECL_CONTEXT (fn))))
    {
      int nargs = call_expr_nargs (t);
      for (int i = 1; i < nargs; ++i)
	{
	  tree arg = get_nth_callarg (t, i);
	  tree atype = TREE_TYPE (arg);
	  if (TREE_CODE (atype) == REFERENCE_TYPE
	      && is_std_allocator (TREE_TYPE (atype)))
	    {
	      STRIP_NOPS (arg);
	      if (TREE_CODE (arg) == ADDR_EXPR)
		{
		  tree *ap = &TREE_OPERAND (arg, 0);
		  if (TREE_CODE (*ap) == TARGET_EXPR)
		    temps.safe_push (ap);
		}
	    }
	}
    }

  return NULL_TREE;
}

/* If INIT initializes a standard library class, and involves a temporary
   std::allocator<T>, use ALLOC_OBJ for all such temporaries.

   Note that this can clobber the input to build_vec_init; no unsharing is
   done.  To make this safe we use the TARGET_EXPR in all places rather than
   pulling out the TARGET_EXPR_SLOT.

   Used by build_vec_init when initializing an array of e.g. strings to reuse
   the same temporary allocator for all of the strings.  We can do this because
   std::allocator has no data and the standard library doesn't care about the
   address of allocator objects.

   ??? Add an attribute to allow users to assert the same property for other
   classes, i.e. one object of the type is interchangeable with any other?  */

static void
combine_allocator_temps (tree &init, tree &alloc_obj)
{
  auto_vec<tree*> temps;
  cp_walk_tree_without_duplicates (&init, find_allocator_temps_r, &temps);
  for (tree *p : temps)
    {
      if (!alloc_obj)
	alloc_obj = *p;
      else
	*p = alloc_obj;
    }
}

/* `build_vec_init' returns tree structure that performs
   initialization of a vector of aggregate types.

   BASE is a reference to the vector, of ARRAY_TYPE, or a pointer
     to the first element, of POINTER_TYPE.
   MAXINDEX is the maximum index of the array (one less than the
     number of elements).  It is only used if BASE is a pointer or
     TYPE_DOMAIN (TREE_TYPE (BASE)) == NULL_TREE.

   INIT is the (possibly NULL) initializer.

   If EXPLICIT_VALUE_INIT_P is true, then INIT must be NULL.  All
   elements in the array are value-initialized.

   FROM_ARRAY is 0 if we should init everything with INIT
   (i.e., every element initialized from INIT).
   FROM_ARRAY is 1 if we should index into INIT in parallel
   with initialization of DECL.
   FROM_ARRAY is 2 if we should index into INIT in parallel,
   but use assignment instead of initialization.  */

tree
build_vec_init (tree base, tree maxindex, tree init,
		bool explicit_value_init_p,
		int from_array,
		tsubst_flags_t complain,
		vec<tree, va_gc>** cleanup_flags /* = nullptr */)
{
  tree rval;
  tree base2 = NULL_TREE;
  tree itype = NULL_TREE;
  tree iterator;
  /* The type of BASE.  */
  tree atype = TREE_TYPE (base);
  /* The type of an element in the array.  */
  tree type = TREE_TYPE (atype);
  /* The element type reached after removing all outer array
     types.  */
  tree inner_elt_type;
  /* The type of a pointer to an element in the array.  */
  tree ptype;
  tree stmt_expr;
  tree compound_stmt;
  int destroy_temps;
  HOST_WIDE_INT num_initialized_elts = 0;
  bool is_global;
  tree obase = base;
  bool xvalue = false;
  bool errors = false;
  location_t loc = (init ? cp_expr_loc_or_input_loc (init)
		    : location_of (base));

  if (TREE_CODE (atype) == ARRAY_TYPE && TYPE_DOMAIN (atype))
    maxindex = array_type_nelts_minus_one (atype);

  if (maxindex == NULL_TREE || maxindex == error_mark_node)
    return error_mark_node;

  maxindex = maybe_constant_value (maxindex);
  if (explicit_value_init_p)
    gcc_assert (!init);

  inner_elt_type = strip_array_types (type);

  /* Look through the TARGET_EXPR around a compound literal.  */
  if (init && TREE_CODE (init) == TARGET_EXPR
      && TREE_CODE (TARGET_EXPR_INITIAL (init)) == CONSTRUCTOR
      && from_array != 2
      && (same_type_ignoring_top_level_qualifiers_p
	  (TREE_TYPE (init), atype)))
    init = TARGET_EXPR_INITIAL (init);

  if (tree vi = get_vec_init_expr (init))
    init = VEC_INIT_EXPR_INIT (vi);

  bool direct_init = false;
  if (from_array && init && BRACE_ENCLOSED_INITIALIZER_P (init)
      && CONSTRUCTOR_NELTS (init) == 1)
    {
      tree elt = CONSTRUCTOR_ELT (init, 0)->value;
      if (TREE_CODE (TREE_TYPE (elt)) == ARRAY_TYPE
	  && TREE_CODE (elt) != VEC_INIT_EXPR)
	{
	  direct_init = DIRECT_LIST_INIT_P (init);
	  init = elt;
	}
    }

  /* from_array doesn't apply to initialization from CONSTRUCTOR.  */
  if (init && TREE_CODE (init) == CONSTRUCTOR)
    from_array = 0;

  /* If we have a braced-init-list or string constant, make sure that the array
     is big enough for all the initializers.  */
  bool length_check = (init
		       && (TREE_CODE (init) == STRING_CST
			   || (TREE_CODE (init) == CONSTRUCTOR
			       && CONSTRUCTOR_NELTS (init) > 0))
		       && !TREE_CONSTANT (maxindex));

  if (init
      && TREE_CODE (atype) == ARRAY_TYPE
      && TREE_CONSTANT (maxindex)
      && !vla_type_p (type)
      && (from_array == 2
	  ? vec_copy_assign_is_trivial (inner_elt_type, init)
	  : !TYPE_NEEDS_CONSTRUCTING (type))
      && ((TREE_CODE (init) == CONSTRUCTOR
	   && (BRACE_ENCLOSED_INITIALIZER_P (init)
	       || (same_type_ignoring_top_level_qualifiers_p
		   (atype, TREE_TYPE (init))))
	   /* Don't do this if the CONSTRUCTOR might contain something
	      that might throw and require us to clean up.  */
	   && (vec_safe_is_empty (CONSTRUCTOR_ELTS (init))
	       || ! TYPE_HAS_NONTRIVIAL_DESTRUCTOR (inner_elt_type)))
	  || from_array))
    {
      /* Do non-default initialization of trivial arrays resulting from
	 brace-enclosed initializers.  In this case, digest_init and
	 store_constructor will handle the semantics for us.  */

      if (BRACE_ENCLOSED_INITIALIZER_P (init))
	init = digest_init (atype, init, complain);
      stmt_expr = cp_build_init_expr (base, init);
      return stmt_expr;
    }

  maxindex = cp_convert (ptrdiff_type_node, maxindex, complain);
  maxindex = fold_simple (maxindex);

  if (TREE_CODE (atype) == ARRAY_TYPE)
    {
      ptype = build_pointer_type (type);
      base = decay_conversion (base, complain);
      if (base == error_mark_node)
	return error_mark_node;
      base = cp_convert (ptype, base, complain);
    }
  else
    ptype = atype;

  if (integer_all_onesp (maxindex))
    {
      /* Shortcut zero element case to avoid unneeded constructor synthesis.  */
      if (init && TREE_SIDE_EFFECTS (init))
	base = build2 (COMPOUND_EXPR, ptype, init, base);
      return base;
    }

  /* The code we are generating looks like:
     ({
       T* t1 = (T*) base;
       T* rval = t1;
       ptrdiff_t iterator = maxindex;
       try {
	 for (; iterator != -1; --iterator) {
	   ... initialize *t1 ...
	   ++t1;
	 }
       } catch (...) {
	 ... destroy elements that were constructed ...
       }
       rval;
     })

     We can omit the try and catch blocks if we know that the
     initialization will never throw an exception, or if the array
     elements do not have destructors.  We can omit the loop completely if
     the elements of the array do not have constructors.

     We actually wrap the entire body of the above in a STMT_EXPR, for
     tidiness.

     When copying from array to another, when the array elements have
     only trivial copy constructors, we should use __builtin_memcpy
     rather than generating a loop.  That way, we could take advantage
     of whatever cleverness the back end has for dealing with copies
     of blocks of memory.  */

  is_global = begin_init_stmts (&stmt_expr, &compound_stmt);
  destroy_temps = stmts_are_full_exprs_p ();
  current_stmt_tree ()->stmts_are_full_exprs_p = 0;
  rval = get_temp_regvar (ptype, base);
  base = get_temp_regvar (ptype, rval);
  tree iterator_targ = get_target_expr (maxindex);
  add_stmt (iterator_targ);
  iterator = TARGET_EXPR_SLOT (iterator_targ);

  /* If initializing one array from another, initialize element by
     element.  We rely upon the below calls to do the argument
     checking.  Evaluate the initializer before entering the try block.  */
  if (from_array)
    {
      if (lvalue_kind (init) & clk_rvalueref)
	xvalue = true;
      if (TREE_CODE (init) == TARGET_EXPR)
	{
	  /* Avoid error in decay_conversion.  */
	  base2 = decay_conversion (TARGET_EXPR_SLOT (init), complain);
	  base2 = cp_build_compound_expr (init, base2, tf_none);
	}
      else
	base2 = decay_conversion (init, complain);
      if (base2 == error_mark_node)
	return error_mark_node;
      itype = TREE_TYPE (base2);
      base2 = get_temp_regvar (itype, base2);
      itype = TREE_TYPE (itype);
    }

  /* Protect the entire array initialization so that we can destroy
     the partially constructed array if an exception is thrown.
     But don't do this if we're assigning.  */
  if (flag_exceptions && TYPE_HAS_NONTRIVIAL_DESTRUCTOR (type)
      && from_array != 2)
    {
      tree e;
      tree m = cp_build_binary_op (input_location,
				   MINUS_EXPR, maxindex, iterator,
				   complain);

      /* Flatten multi-dimensional array since build_vec_delete only
	 expects one-dimensional array.  */
      if (TREE_CODE (type) == ARRAY_TYPE)
	m = cp_build_binary_op (input_location,
				MULT_EXPR, m,
				/* Avoid mixing signed and unsigned.  */
				convert (TREE_TYPE (m),
					 array_type_nelts_total (type)),
				complain);

      e = build_vec_delete_1 (input_location, rval, m,
			      inner_elt_type, sfk_complete_destructor,
			      /*use_global_delete=*/0, complain,
			      /*in_cleanup*/true);
      if (e == error_mark_node)
	errors = true;
      TARGET_EXPR_CLEANUP (iterator_targ) = e;
      CLEANUP_EH_ONLY (iterator_targ) = true;

      /* Since we push this cleanup before doing any initialization, cleanups
	 for any temporaries in the initialization are naturally within our
	 cleanup region, so we don't want wrap_temporary_cleanups to do
	 anything for arrays.  But if the array is a subobject, we need to
	 tell split_nonconstant_init or cp_genericize_target_expr how to turn
	 off this cleanup in favor of the cleanup for the complete object.

	 ??? For an array temporary such as an initializer_list backing array,
	 it would avoid redundancy to leave this cleanup active, clear
	 CLEANUP_EH_ONLY, and not build another cleanup for the temporary
	 itself.  But that breaks when gimplify_target_expr adds a clobber
	 cleanup that runs before the build_vec_init cleanup.  */
      if (cleanup_flags)
	vec_safe_push (*cleanup_flags, build_tree_list (iterator, maxindex));
    }

  /* Should we try to create a constant initializer?  */
  bool try_const = (TREE_CODE (atype) == ARRAY_TYPE
		    && TREE_CONSTANT (maxindex)
		    && (init ? TREE_CODE (init) == CONSTRUCTOR
			: (type_has_constexpr_default_constructor
			   (inner_elt_type)
			   /* Value-initialization of scalars is constexpr.  */
			   || (explicit_value_init_p
			       && SCALAR_TYPE_P (inner_elt_type))))
		    && (literal_type_p (inner_elt_type)
			|| TYPE_HAS_CONSTEXPR_CTOR (inner_elt_type)));
  vec<constructor_elt, va_gc> *const_vec = NULL;
  bool saw_non_const = false;
  /* If we're initializing a static array, we want to do static
     initialization of any elements with constant initializers even if
     some are non-constant.  */
  bool do_static_init = (DECL_P (obase) && TREE_STATIC (obase));

  bool empty_list = false;
  if (init && BRACE_ENCLOSED_INITIALIZER_P (init)
      && CONSTRUCTOR_NELTS (init) == 0)
    /* Skip over the handling of non-empty init lists.  */
    empty_list = true;

  /* Maybe pull out constant value when from_array? */

  else if (init != NULL_TREE && TREE_CODE (init) == CONSTRUCTOR)
    {
      /* Do non-default initialization of non-trivial arrays resulting from
	 brace-enclosed initializers.  */
      unsigned HOST_WIDE_INT idx;
      tree field, elt;
      /* If the constructor already has the array type, it's been through
	 digest_init, so we shouldn't try to do anything more.  */
      bool digested = same_type_p (atype, TREE_TYPE (init));
      from_array = 0;

      if (length_check)
	finish_length_check (atype, iterator, obase, CONSTRUCTOR_NELTS (init));

      if (try_const)
	vec_alloc (const_vec, CONSTRUCTOR_NELTS (init));

      tree alloc_obj = NULL_TREE;

      FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (init), idx, field, elt)
	{
	  tree baseref = build1 (INDIRECT_REF, type, base);
	  tree one_init;

	  num_initialized_elts++;

	  /* We need to see sub-array TARGET_EXPR before cp_fold_r so we can
	     handle cleanup flags properly.  */
	  gcc_checking_assert (!target_expr_needs_replace (elt));

	  if (digested)
	    one_init = cp_build_init_expr (baseref, elt);
	  else if (tree vi = get_vec_init_expr (elt))
	    one_init = expand_vec_init_expr (baseref, vi, complain,
					     cleanup_flags);
	  else if (MAYBE_CLASS_TYPE_P (type) || TREE_CODE (type) == ARRAY_TYPE)
	    one_init = build_aggr_init (baseref, elt, 0, complain);
	  else
	    one_init = cp_build_modify_expr (input_location, baseref,
					     NOP_EXPR, elt, complain);
	  if (one_init == error_mark_node)
	    errors = true;
	  if (try_const)
	    {
	      if (!field)
		field = size_int (idx);
	      tree e = maybe_constant_init (one_init);
	      if (reduced_constant_expression_p (e))
		{
		  CONSTRUCTOR_APPEND_ELT (const_vec, field, e);
		  if (do_static_init)
		    one_init = NULL_TREE;
		  else
		    one_init = cp_build_init_expr (baseref, e);
		}
	      else
		{
		  if (do_static_init)
		    {
		      tree value = build_zero_init (TREE_TYPE (e), NULL_TREE,
						    true);
		      if (value)
			CONSTRUCTOR_APPEND_ELT (const_vec, field, value);
		    }
		  saw_non_const = true;
		}
	    }

	  if (one_init)
	    {
	      /* Only create one std::allocator temporary.  */
	      combine_allocator_temps (one_init, alloc_obj);
	      finish_expr_stmt (one_init);
	    }

	  one_init = cp_build_unary_op (PREINCREMENT_EXPR, base, false,
					complain);
	  if (one_init == error_mark_node)
	    errors = true;
	  else
	    finish_expr_stmt (one_init);

	  one_init = cp_build_unary_op (PREDECREMENT_EXPR, iterator, false,
					complain);
	  if (one_init == error_mark_node)
	    errors = true;
	  else
	    finish_expr_stmt (one_init);
	}

      /* Any elements without explicit initializers get T{}.  */
      empty_list = true;
    }
  else if (init && TREE_CODE (init) == STRING_CST)
    {
      /* Check that the array is at least as long as the string.  */
      if (length_check)
	finish_length_check (atype, iterator, obase,
			     TREE_STRING_LENGTH (init));
      tree length = build_int_cst (ptrdiff_type_node,
				   TREE_STRING_LENGTH (init));

      /* Copy the string to the first part of the array.  */
      tree alias_set = build_int_cst (build_pointer_type (type), 0);
      tree lhs = build2 (MEM_REF, TREE_TYPE (init), base, alias_set);
      tree stmt = build2 (MODIFY_EXPR, void_type_node, lhs, init);
      finish_expr_stmt (stmt);

      /* Adjust the counter and pointer.  */
      stmt = cp_build_binary_op (loc, MINUS_EXPR, iterator, length, complain);
      stmt = build2 (MODIFY_EXPR, void_type_node, iterator, stmt);
      finish_expr_stmt (stmt);

      stmt = cp_build_binary_op (loc, PLUS_EXPR, base, length, complain);
      stmt = build2 (MODIFY_EXPR, void_type_node, base, stmt);
      finish_expr_stmt (stmt);

      /* And set the rest of the array to NUL.  */
      from_array = 0;
      explicit_value_init_p = true;
    }
  else if (from_array)
    {
      if (init)
	/* OK, we set base2 above.  */;
      else if (CLASS_TYPE_P (type)
	       && ! TYPE_HAS_DEFAULT_CONSTRUCTOR (type))
	{
          if (complain & tf_error)
            error ("initializer ends prematurely");
	  errors = true;
	}
    }

  /* Now, default-initialize any remaining elements.  We don't need to
     do that if a) the type does not need constructing, or b) we've
     already initialized all the elements.

     We do need to keep going if we're copying an array.  */

  if (try_const && !init
      && (cxx_dialect < cxx20
	  || !default_init_uninitialized_part (inner_elt_type)))
    /* With a constexpr default constructor, which we checked for when
       setting try_const above, default-initialization is equivalent to
       value-initialization, and build_value_init gives us something more
       friendly to maybe_constant_init.  Except in C++20 and up a constexpr
       constructor need not initialize all the members.  */
    explicit_value_init_p = true;
  if (from_array
      || ((type_build_ctor_call (type) || init || explicit_value_init_p)
	  && ! (tree_fits_shwi_p (maxindex)
		&& (num_initialized_elts
		    == tree_to_shwi (maxindex) + 1))))
    {
      /* If the ITERATOR is lesser or equal to -1, then we don't have to loop;
	 we've already initialized all the elements.  */
      tree for_stmt;
      tree elt_init;
      tree to;

      for_stmt = begin_for_stmt (NULL_TREE, NULL_TREE);
      finish_init_stmt (for_stmt);
      finish_for_cond (build2 (GT_EXPR, boolean_type_node, iterator,
			       build_int_cst (TREE_TYPE (iterator), -1)),
		       for_stmt, false, 0, false);
      /* We used to pass this decrement to finish_for_expr; now we add it to
	 elt_init below so it's part of the same full-expression as the
	 initialization, and thus happens before any potentially throwing
	 temporary cleanups.  */
      tree decr = cp_build_unary_op (PREDECREMENT_EXPR, iterator, false,
				     complain);


      to = build1 (INDIRECT_REF, type, base);

      /* If the initializer is {}, then all elements are initialized from T{}.
	 But for non-classes, that's the same as value-initialization.  */
      if (empty_list)
	{
	  if (cxx_dialect >= cxx11
	      && (CLASS_TYPE_P (type)
		  || TREE_CODE (type) == ARRAY_TYPE))
	    {
	      init = build_constructor (init_list_type_node, NULL);
	    }
	  else
	    {
	      init = NULL_TREE;
	      explicit_value_init_p = true;
	    }
	}

      if (from_array)
	{
	  tree from;

	  if (base2)
	    {
	      from = build1 (INDIRECT_REF, itype, base2);
	      if (xvalue)
		from = move (from);
	      if (direct_init)
		{
		  /* Wrap the initializer in a CONSTRUCTOR so that
		     build_vec_init recognizes it as direct-initialization.  */
		  from = build_constructor_single (init_list_type_node,
						   NULL_TREE, from);
		  CONSTRUCTOR_IS_DIRECT_INIT (from) = true;
		}
	    }
	  else
	    from = NULL_TREE;

	  if (TREE_CODE (type) == ARRAY_TYPE)
	    elt_init = build_vec_init (to, NULL_TREE, from, /*val_init*/false,
				       from_array, complain);
	  else if (from_array == 2)
	    elt_init = cp_build_modify_expr (input_location, to, NOP_EXPR,
					     from, complain);
	  else if (type_build_ctor_call (type))
	    elt_init = build_aggr_init (to, from, 0, complain);
	  else if (from)
	    elt_init = cp_build_modify_expr (input_location, to, NOP_EXPR, from,
					     complain);
	  else
	    gcc_unreachable ();
	}
      else if (TREE_CODE (type) == ARRAY_TYPE)
	{
	  if (init && !BRACE_ENCLOSED_INITIALIZER_P (init))
	    {
	      if ((complain & tf_error))
		error_at (loc, "array must be initialized "
			  "with a brace-enclosed initializer");
	      elt_init = error_mark_node;
	    }
	  else
	    elt_init = build_vec_init (build1 (INDIRECT_REF, type, base),
				       0, init,
				       explicit_value_init_p,
				       0, complain);
	}
      else if (explicit_value_init_p)
	{
	  elt_init = build_value_init (type, complain);
	  if (elt_init != error_mark_node)
	    elt_init = cp_build_init_expr (to, elt_init);
	}
      else
	{
	  gcc_assert (type_build_ctor_call (type) || init);
	  if (CLASS_TYPE_P (type))
	    elt_init = build_aggr_init (to, init, 0, complain);
	  else
	    {
	      if (TREE_CODE (init) == TREE_LIST)
		init = build_x_compound_expr_from_list (init, ELK_INIT,
							complain);
	      elt_init = (init == error_mark_node
			  ? error_mark_node
			  : build2 (INIT_EXPR, type, to, init));
	    }
	}

      if (elt_init == error_mark_node)
	errors = true;

      if (try_const)
	{
	  /* FIXME refs to earlier elts */
	  tree e = maybe_constant_init (elt_init);
	  if (reduced_constant_expression_p (e))
	    {
	      if (initializer_zerop (e))
		/* Don't fill the CONSTRUCTOR with zeros.  */
		e = NULL_TREE;
	      if (do_static_init)
		elt_init = NULL_TREE;
	    }
	  else
	    {
	      saw_non_const = true;
	      if (do_static_init)
		e = build_zero_init (TREE_TYPE (e), NULL_TREE, true);
	      else
		e = NULL_TREE;
	    }

	  if (e)
	    {
	      HOST_WIDE_INT last = tree_to_shwi (maxindex);
	      if (num_initialized_elts <= last)
		{
		  tree field = size_int (num_initialized_elts);
		  if (num_initialized_elts != last)
		    field = build2 (RANGE_EXPR, sizetype, field,
				    size_int (last));
		  CONSTRUCTOR_APPEND_ELT (const_vec, field, e);
		}
	    }
	}

      /* [class.temporary]: "There are three contexts in which temporaries are
	 destroyed at a different point than the end of the full-
	 expression. The first context is when a default constructor is called
	 to initialize an element of an array with no corresponding
	 initializer. The second context is when a copy constructor is called
	 to copy an element of an array while the entire array is copied. In
	 either case, if the constructor has one or more default arguments, the
	 destruction of every temporary created in a default argument is
	 sequenced before the construction of the next array element, if any."

	 So, for this loop, statements are full-expressions.  */
      current_stmt_tree ()->stmts_are_full_exprs_p = 1;
      if (elt_init && !errors)
	elt_init = build2 (COMPOUND_EXPR, void_type_node, elt_init, decr);
      else
	elt_init = decr;
      finish_expr_stmt (elt_init);
      current_stmt_tree ()->stmts_are_full_exprs_p = 0;

      finish_expr_stmt (cp_build_unary_op (PREINCREMENT_EXPR, base, false,
                                           complain));
      if (base2)
	finish_expr_stmt (cp_build_unary_op (PREINCREMENT_EXPR, base2, false,
                                             complain));

      finish_for_stmt (for_stmt);
    }

  /* The value of the array initialization is the array itself, RVAL
     is a pointer to the first element.  */
  finish_stmt_expr_expr (rval, stmt_expr);

  stmt_expr = finish_init_stmts (is_global, stmt_expr, compound_stmt);

  current_stmt_tree ()->stmts_are_full_exprs_p = destroy_temps;

  if (errors)
    return error_mark_node;

  if (try_const)
    {
      if (!saw_non_const)
	{
	  tree const_init = build_constructor (atype, const_vec);
	  return build2 (INIT_EXPR, atype, obase, const_init);
	}
      else if (do_static_init && !vec_safe_is_empty (const_vec))
	DECL_INITIAL (obase) = build_constructor (atype, const_vec);
      else
	vec_free (const_vec);
    }

  /* Now make the result have the correct type.  */
  if (TREE_CODE (atype) == ARRAY_TYPE)
    {
      atype = build_reference_type (atype);
      stmt_expr = build1 (NOP_EXPR, atype, stmt_expr);
      stmt_expr = convert_from_reference (stmt_expr);
    }

  return stmt_expr;
}

/* Call the DTOR_KIND destructor for EXP.  FLAGS are as for
   build_delete.  */

static tree
build_dtor_call (tree exp, special_function_kind dtor_kind, int flags,
		 tsubst_flags_t complain)
{
  tree name;
  switch (dtor_kind)
    {
    case sfk_complete_destructor:
      name = complete_dtor_identifier;
      break;

    case sfk_base_destructor:
      name = base_dtor_identifier;
      break;

    case sfk_deleting_destructor:
      name = deleting_dtor_identifier;
      break;

    default:
      gcc_unreachable ();
    }

  return build_special_member_call (exp, name,
				    /*args=*/NULL,
				    /*binfo=*/TREE_TYPE (exp),
				    flags,
				    complain);
}

/* Generate a call to a destructor. TYPE is the type to cast ADDR to.
   ADDR is an expression which yields the store to be destroyed.
   AUTO_DELETE is the name of the destructor to call, i.e., either
   sfk_complete_destructor, sfk_base_destructor, or
   sfk_deleting_destructor.

   FLAGS is the logical disjunction of zero or more LOOKUP_
   flags.  See cp-tree.h for more info.  */

tree
build_delete (location_t loc, tree otype, tree addr,
	      special_function_kind auto_delete,
	      int flags, int use_global_delete, tsubst_flags_t complain)
{
  tree expr;

  if (addr == error_mark_node)
    return error_mark_node;

  tree type = TYPE_MAIN_VARIANT (otype);

  /* Can happen when CURRENT_EXCEPTION_OBJECT gets its type
     set to `error_mark_node' before it gets properly cleaned up.  */
  if (type == error_mark_node)
    return error_mark_node;

  if (TYPE_PTR_P (type))
    type = TYPE_MAIN_VARIANT (TREE_TYPE (type));

  if (TREE_CODE (type) == ARRAY_TYPE)
    {
      if (TYPE_DOMAIN (type) == NULL_TREE)
	{
	  if (complain & tf_error)
	    error_at (loc, "unknown array size in delete");
	  return error_mark_node;
	}
      return build_vec_delete (loc, addr, array_type_nelts_minus_one (type),
			       auto_delete, use_global_delete, complain);
    }

  bool deleting = (auto_delete == sfk_deleting_destructor);
  gcc_assert (deleting == !(flags & LOOKUP_DESTRUCTOR));

  if (TYPE_PTR_P (otype))
    {
      addr = mark_rvalue_use (addr);

      /* We don't want to warn about delete of void*, only other
	  incomplete types.  Deleting other incomplete types
	  invokes undefined behavior, but it is not ill-formed, so
	  compile to something that would even do The Right Thing
	  (TM) should the type have a trivial dtor and no delete
	  operator.  */
      if (!VOID_TYPE_P (type))
	{
	  complete_type (type);
	  if (deleting
	      && !verify_type_context (loc, TCTX_DEALLOCATION, type,
				       !(complain & tf_error)))
	    return error_mark_node;

	  if (!COMPLETE_TYPE_P (type))
	    {
	      if (cxx_dialect > cxx23)
		{
		  if (complain & tf_error)
		    {
		      auto_diagnostic_group d;
		      int saved_errorcount = errorcount;
		      if (permerror_opt (loc, OPT_Wdelete_incomplete,
					 "operator %<delete%> used on "
					 "incomplete type"))
			{
			  cxx_incomplete_type_inform (type);
			  if (errorcount != saved_errorcount)
			    return error_mark_node;
			}
		    }
		  else
		    return error_mark_node;
		}
	      else if (complain & tf_warning)
		{
		  auto_diagnostic_group d;
		  if (warning_at (loc, OPT_Wdelete_incomplete,
				  "possible problem detected in invocation of "
				  "%<operator delete%>"))
		    {
		      cxx_incomplete_type_diagnostic (addr, type, DK_WARNING);
		      inform (loc,
			      "neither the destructor nor the class-specific "
			      "%<operator delete%> will be called, even if "
			      "they are declared when the class is defined");
		    }
		}
	    }
	  else if (deleting && warn_delnonvdtor
	           && MAYBE_CLASS_TYPE_P (type) && !CLASSTYPE_FINAL (type)
		   && TYPE_POLYMORPHIC_P (type))
	    {
	      tree dtor = CLASSTYPE_DESTRUCTOR (type);
	      if (!dtor || !DECL_VINDEX (dtor))
		{
		  if (CLASSTYPE_PURE_VIRTUALS (type))
		    warning_at (loc, OPT_Wdelete_non_virtual_dtor,
				"deleting object of abstract class type %qT"
				" which has non-virtual destructor"
				" will cause undefined behavior", type);
		  else
		    warning_at (loc, OPT_Wdelete_non_virtual_dtor,
				"deleting object of polymorphic class type %qT"
				" which has non-virtual destructor"
				" might cause undefined behavior", type);
		}
	    }
	}

      /* Throw away const and volatile on target type of addr.  */
      addr = convert_force (build_pointer_type (type), addr, 0, complain);
    }
  else
    {
      /* Don't check PROTECT here; leave that decision to the
	 destructor.  If the destructor is accessible, call it,
	 else report error.  */
      addr = cp_build_addr_expr (addr, complain);
      if (addr == error_mark_node)
	return error_mark_node;

      addr = convert_force (build_pointer_type (type), addr, 0, complain);
    }

  tree addr_expr = NULL_TREE;
  if (deleting)
    /* We will use ADDR multiple times so we must save it.  */
    {
      addr_expr = get_target_expr (addr);
      addr = TARGET_EXPR_SLOT (addr_expr);
    }

  bool virtual_p = false;
  if (type_build_dtor_call (type))
    {
      if (CLASSTYPE_LAZY_DESTRUCTOR (type))
	lazily_declare_fn (sfk_destructor, type);
      virtual_p = DECL_VIRTUAL_P (CLASSTYPE_DESTRUCTOR (type));
    }

  tree head = NULL_TREE;
  tree do_delete = NULL_TREE;
  bool destroying_delete = false;

  if (!deleting)
    {
      /* Leave do_delete null.  */
    }
  /* For `::delete x', we must not use the deleting destructor
     since then we would not be sure to get the global `operator
     delete'.  */
  else if (use_global_delete)
    {
      head = get_target_expr (build_headof (addr));
      /* Delete the object.  */
      do_delete = build_op_delete_call (DELETE_EXPR,
					head,
					cxx_sizeof_nowarn (type),
					/*global_p=*/true,
					/*placement=*/NULL_TREE,
					/*alloc_fn=*/NULL_TREE,
					complain);
      /* Otherwise, treat this like a complete object destructor
	 call.  */
      auto_delete = sfk_complete_destructor;
    }
  /* If the destructor is non-virtual, there is no deleting
     variant.  Instead, we must explicitly call the appropriate
     `operator delete' here.  */
  else if (!virtual_p)
    {
      /* Build the call.  */
      do_delete = build_op_delete_call (DELETE_EXPR,
					addr,
					cxx_sizeof_nowarn (type),
					/*global_p=*/false,
					/*placement=*/NULL_TREE,
					/*alloc_fn=*/NULL_TREE,
					complain);
      /* Call the complete object destructor.  */
      auto_delete = sfk_complete_destructor;
      if (do_delete != error_mark_node)
	{
	  tree fn = get_callee_fndecl (do_delete);
	  destroying_delete = destroying_delete_p (fn);
	}
    }
  else if (TYPE_GETS_REG_DELETE (type))
    {
      /* Make sure we have access to the member op delete, even though
	 we'll actually be calling it from the destructor.  */
      build_op_delete_call (DELETE_EXPR, addr, cxx_sizeof_nowarn (type),
			    /*global_p=*/false,
			    /*placement=*/NULL_TREE,
			    /*alloc_fn=*/NULL_TREE,
			    complain);
    }

  if (destroying_delete)
    /* The operator delete will call the destructor.  */
    expr = addr;
  else if (type_build_dtor_call (type))
    expr = build_dtor_call (cp_build_fold_indirect_ref (addr),
			    auto_delete, flags, complain);
  else
    expr = build_trivial_dtor_call (addr);
  if (expr == error_mark_node)
    return error_mark_node;

  if (!deleting)
    {
      protected_set_expr_location (expr, loc);
      return expr;
    }

  if (do_delete == error_mark_node)
    return error_mark_node;

  if (do_delete && !TREE_SIDE_EFFECTS (expr))
    expr = do_delete;
  else if (do_delete)
    /* The delete operator must be called, regardless of whether
       the destructor throws.

       [expr.delete]/7 The deallocation function is called
       regardless of whether the destructor for the object or some
       element of the array throws an exception.  */
    expr = build2 (TRY_FINALLY_EXPR, void_type_node, expr, do_delete);

  /* We need to calculate this before the dtor changes the vptr.  */
  if (head)
    expr = build2 (COMPOUND_EXPR, void_type_node, head, expr);

  /* Handle deleting a null pointer.  */
  warning_sentinel s (warn_address);
  tree ifexp = cp_build_binary_op (loc, NE_EXPR, addr,
				   nullptr_node, complain);
  ifexp = cp_fully_fold (ifexp);

  if (ifexp == error_mark_node)
    return error_mark_node;
  /* This is a compiler generated comparison, don't emit
     e.g. -Wnonnull-compare warning for it.  */
  else if (TREE_CODE (ifexp) == NE_EXPR)
    suppress_warning (ifexp, OPT_Wnonnull_compare);

  if (!integer_nonzerop (ifexp))
    expr = build3 (COND_EXPR, void_type_node, ifexp, expr, void_node);

  if (addr_expr)
    expr = cp_build_compound_expr (addr_expr, expr, tf_none);

  protected_set_expr_location (expr, loc);
  return expr;
}

/* At the beginning of a destructor, push cleanups that will call the
   destructors for our base classes and members.

   Called from begin_destructor_body.  */

void
push_base_cleanups (void)
{
  tree binfo, base_binfo;
  int i;
  tree member;
  tree expr;
  vec<tree, va_gc> *vbases;

  /* Run destructors for all virtual baseclasses.  */
  if (!ABSTRACT_CLASS_TYPE_P (current_class_type)
      && CLASSTYPE_VBASECLASSES (current_class_type))
    {
      tree cond = (condition_conversion
		   (build2 (BIT_AND_EXPR, integer_type_node,
			    current_in_charge_parm,
			    integer_two_node)));

      /* The CLASSTYPE_VBASECLASSES vector is in initialization
	 order, which is also the right order for pushing cleanups.  */
      for (vbases = CLASSTYPE_VBASECLASSES (current_class_type), i = 0;
	   vec_safe_iterate (vbases, i, &base_binfo); i++)
	{
	  if (type_build_dtor_call (BINFO_TYPE (base_binfo)))
	    {
	      expr = build_special_member_call (current_class_ref,
						base_dtor_identifier,
						NULL,
						base_binfo,
						(LOOKUP_NORMAL
						 | LOOKUP_NONVIRTUAL),
						tf_warning_or_error);
	      if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (BINFO_TYPE (base_binfo)))
		{
		  expr = build3 (COND_EXPR, void_type_node, cond,
				 expr, void_node);
		  finish_decl_cleanup (NULL_TREE, expr);
		}
	    }
	}
    }

  /* Take care of the remaining baseclasses.  */
  for (binfo = TYPE_BINFO (current_class_type), i = 0;
       BINFO_BASE_ITERATE (binfo, i, base_binfo); i++)
    {
      if (BINFO_VIRTUAL_P (base_binfo)
	  || !type_build_dtor_call (BINFO_TYPE (base_binfo)))
	continue;

      expr = build_special_member_call (current_class_ref,
					base_dtor_identifier,
					NULL, base_binfo,
					LOOKUP_NORMAL | LOOKUP_NONVIRTUAL,
                                        tf_warning_or_error);
      if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (BINFO_TYPE (base_binfo)))
	finish_decl_cleanup (NULL_TREE, expr);
    }

  /* Don't automatically destroy union members.  */
  if (TREE_CODE (current_class_type) == UNION_TYPE)
    return;

  for (member = TYPE_FIELDS (current_class_type); member;
       member = DECL_CHAIN (member))
    {
      tree this_type = TREE_TYPE (member);
      if (this_type == error_mark_node
	  || TREE_CODE (member) != FIELD_DECL
	  || DECL_ARTIFICIAL (member))
	continue;
      if (ANON_AGGR_TYPE_P (this_type))
	continue;
      if (type_build_dtor_call (this_type))
	{
	  tree this_member = (build_class_member_access_expr
			      (current_class_ref, member,
			       /*access_path=*/NULL_TREE,
			       /*preserve_reference=*/false,
			       tf_warning_or_error));
	  expr = build_delete (input_location, this_type, this_member,
			       sfk_complete_destructor,
			       LOOKUP_NONVIRTUAL|LOOKUP_DESTRUCTOR|LOOKUP_NORMAL,
			       0, tf_warning_or_error);
	  if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (this_type))
	    finish_decl_cleanup (NULL_TREE, expr);
	}
    }
}

/* Build a C++ vector delete expression.
   MAXINDEX is the number of elements to be deleted.
   ELT_SIZE is the nominal size of each element in the vector.
   BASE is the expression that should yield the store to be deleted.
   This function expands (or synthesizes) these calls itself.
   AUTO_DELETE_VEC says whether the container (vector) should be deallocated.

   This also calls delete for virtual baseclasses of elements of the vector.

   Update: MAXINDEX is no longer needed.  The size can be extracted from the
   start of the vector for pointers, and from the type for arrays.  We still
   use MAXINDEX for arrays because it happens to already have one of the
   values we'd have to extract.  (We could use MAXINDEX with pointers to
   confirm the size, and trap if the numbers differ; not clear that it'd
   be worth bothering.)  */

tree
build_vec_delete (location_t loc, tree base, tree maxindex,
		  special_function_kind auto_delete_vec,
		  int use_global_delete, tsubst_flags_t complain)
{
  tree type;
  tree rval;
  tree base_init = NULL_TREE;

  type = TREE_TYPE (base);

  if (TYPE_PTR_P (type))
    {
      /* Step back one from start of vector, and read dimension.  */
      tree cookie_addr;
      tree size_ptr_type = build_pointer_type (sizetype);

      base = mark_rvalue_use (base);
      if (TREE_SIDE_EFFECTS (base))
	{
	  base_init = get_target_expr (base);
	  base = TARGET_EXPR_SLOT (base_init);
	}
      type = strip_array_types (TREE_TYPE (type));
      cookie_addr = fold_build1_loc (loc, NEGATE_EXPR,
				 sizetype, TYPE_SIZE_UNIT (sizetype));
      cookie_addr = fold_build_pointer_plus (fold_convert (size_ptr_type, base),
					     cookie_addr);
      maxindex = cp_build_fold_indirect_ref (cookie_addr);
    }
  else if (TREE_CODE (type) == ARRAY_TYPE)
    {
      /* Get the total number of things in the array, maxindex is a
	 bad name.  */
      maxindex = array_type_nelts_total (type);
      type = strip_array_types (type);
      base = decay_conversion (base, complain);
      if (base == error_mark_node)
	return error_mark_node;
      if (TREE_SIDE_EFFECTS (base))
	{
	  base_init = get_target_expr (base);
	  base = TARGET_EXPR_SLOT (base_init);
	}
    }
  else
    {
      if (base != error_mark_node && !(complain & tf_error))
	error_at (loc,
		  "type to vector delete is neither pointer or array type");
      return error_mark_node;
    }

  rval = build_vec_delete_1 (loc, base, maxindex, type, auto_delete_vec,
			     use_global_delete, complain);
  if (base_init && rval != error_mark_node)
    rval = build2 (COMPOUND_EXPR, TREE_TYPE (rval), base_init, rval);

  protected_set_expr_location (rval, loc);
  return rval;
}

#include "gt-cp-init.h"
