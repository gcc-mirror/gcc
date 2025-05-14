/* Report error messages, build initializers, and perform
   some front-end optimizations for C++ compiler.
   Copyright (C) 1987-2025 Free Software Foundation, Inc.
   Hacked by Michael Tiemann (tiemann@cygnus.com)

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


/* This file is part of the C++ front end.
   It contains routines to build C++ expressions given their operands,
   including computing the types of the result, C and C++ specific error
   checks, and some optimization.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "cp-tree.h"
#include "stor-layout.h"
#include "varasm.h"
#include "intl.h"
#include "gcc-rich-location.h"
#include "target.h"

static tree
process_init_constructor (tree type, tree init, int nested, int flags,
			  tsubst_flags_t complain);


/* Print an error message stemming from an attempt to use
   BASETYPE as a base class for TYPE.  */

tree
error_not_base_type (tree basetype, tree type)
{
  if (TREE_CODE (basetype) == FUNCTION_DECL)
    basetype = DECL_CONTEXT (basetype);
  error ("type %qT is not a base type for type %qT", basetype, type);
  return error_mark_node;
}

tree
binfo_or_else (tree base, tree type)
{
  tree binfo = lookup_base (type, base, ba_unique,
			    NULL, tf_warning_or_error);

  if (binfo == error_mark_node)
    return NULL_TREE;
  else if (!binfo)
    error_not_base_type (base, type);
  return binfo;
}

/* According to ARM $7.1.6, "A `const' object may be initialized, but its
   value may not be changed thereafter.  */

void
cxx_readonly_error (location_t loc, tree arg, enum lvalue_use errstring)
{

/* This macro is used to emit diagnostics to ensure that all format
   strings are complete sentences, visible to gettext and checked at
   compile time.  */

#define ERROR_FOR_ASSIGNMENT(LOC, AS, ASM, IN, DE, ARG)			\
  do {                                                                  \
    switch (errstring)                                                  \
      {                                                                 \
      case lv_assign:							\
	error_at (LOC, AS, ARG);					\
        break;                                                          \
      case lv_asm:							\
	error_at (LOC, ASM, ARG);					\
        break;                                                          \
      case lv_increment:						\
	error_at (LOC, IN, ARG);					\
        break;                                                          \
      case lv_decrement:                                                \
	error_at (LOC, DE, ARG);					\
        break;                                                          \
      default:                                                          \
        gcc_unreachable ();                                             \
      }                                                                 \
  } while (0)

  /* Handle C++-specific things first.  */

  if (VAR_P (arg)
      && DECL_LANG_SPECIFIC (arg)
      && DECL_IN_AGGR_P (arg)
      && !TREE_STATIC (arg))
    ERROR_FOR_ASSIGNMENT (loc,
			  G_("assignment of constant field %qD"),
			  G_("constant field %qD used as %<asm%> output"),
			  G_("increment of constant field %qD"),
			  G_("decrement of constant field %qD"),
			  arg);
  else if (INDIRECT_REF_P (arg)
	   && TYPE_REF_P (TREE_TYPE (TREE_OPERAND (arg, 0)))
	   && (VAR_P (TREE_OPERAND (arg, 0))
	       || TREE_CODE (TREE_OPERAND (arg, 0)) == PARM_DECL))
    ERROR_FOR_ASSIGNMENT (loc,
			  G_("assignment of read-only reference %qD"),
			  G_("read-only reference %qD used as %<asm%> output"),
			  G_("increment of read-only reference %qD"),
			  G_("decrement of read-only reference %qD"),
			  TREE_OPERAND (arg, 0));
  else
    readonly_error (loc, arg, errstring);
}

/* If TYPE has abstract virtual functions, issue an error about trying
   to create an object of that type.  DECL is the object declared, or
   NULL_TREE if the declaration is unavailable, in which case USE specifies
   the kind of invalid use.  Returns 1 if an error occurred; zero if
   all was well.  */

static int
abstract_virtuals_error (tree decl, tree type, abstract_class_use use,
			 tsubst_flags_t complain)
{
  vec<tree, va_gc> *pure;

  if (TREE_CODE (type) == ARRAY_TYPE)
    {
      decl = NULL_TREE;
      use = ACU_ARRAY;
      type = strip_array_types (type);
    }

  /* This function applies only to classes. Any other entity can never
     be abstract.  */
  if (!CLASS_TYPE_P (type))
    return 0;
  type = TYPE_MAIN_VARIANT (type);

#if 0
  /* Instantiation here seems to be required by the standard,
     but breaks e.g. boost::bind.  FIXME!  */
  /* In SFINAE, non-N3276 context, force instantiation.  */
  if (!(complain & (tf_error|tf_decltype)))
    complete_type (type);
#endif

  if (!TYPE_SIZE (type))
    /* TYPE is being defined, and during that time
       CLASSTYPE_PURE_VIRTUALS holds the inline friends.  */
    return 0;

  pure = CLASSTYPE_PURE_VIRTUALS (type);
  if (!pure)
    return 0;

  if (!(complain & tf_error))
    return 1;

  auto_diagnostic_group d;
  if (decl)
    {
      if (VAR_P (decl))
	error ("cannot declare variable %q+D to be of abstract "
	       "type %qT", decl, type);
      else if (TREE_CODE (decl) == PARM_DECL)
	{
	  if (DECL_NAME (decl))
	    error ("cannot declare parameter %q+D to be of abstract type %qT",
		   decl, type);
	  else
	    error ("cannot declare parameter to be of abstract type %qT",
		   type);
	}
      else if (TREE_CODE (decl) == FIELD_DECL)
	error ("cannot declare field %q+D to be of abstract type %qT",
	       decl, type);
      else if (TREE_CODE (decl) == FUNCTION_DECL
	       && TREE_CODE (TREE_TYPE (decl)) == METHOD_TYPE)
	error ("invalid abstract return type for member function %q+#D", decl);
      else if (TREE_CODE (decl) == FUNCTION_DECL)
	error ("invalid abstract return type for function %q+#D", decl);
      else if (identifier_p (decl))
	/* Here we do not have location information.  */
	error ("invalid abstract type %qT for %qE", type, decl);
      else
	error ("invalid abstract type for %q+D", decl);
    }
  else switch (use)
    {
    case ACU_ARRAY:
      error ("creating array of %qT, which is an abstract class type", type);
      break;
    case ACU_CAST:
      error ("invalid cast to abstract class type %qT", type);
      break;
    case ACU_NEW:
      error ("invalid new-expression of abstract class type %qT", type);
      break;
    case ACU_RETURN:
      error ("invalid abstract return type %qT", type);
      break;
    case ACU_PARM:
      error ("invalid abstract parameter type %qT", type);
      break;
    case ACU_THROW:
      error ("expression of abstract class type %qT cannot "
	     "be used in throw-expression", type);
      break;
    case ACU_CATCH:
      error ("cannot declare %<catch%> parameter to be of abstract "
	     "class type %qT", type);
      break;
    default:
      error ("cannot allocate an object of abstract type %qT", type);
    }

  /* Only go through this once.  */
  if (pure->length ())
    {
      unsigned ix;
      tree fn;

      inform (DECL_SOURCE_LOCATION (TYPE_MAIN_DECL (type)),
	      "  because the following virtual functions are pure within %qT:",
	      type);

      FOR_EACH_VEC_ELT (*pure, ix, fn)
	if (! DECL_CLONED_FUNCTION_P (fn)
	    || DECL_COMPLETE_DESTRUCTOR_P (fn))
	  inform (DECL_SOURCE_LOCATION (fn), "    %#qD", fn);

      /* Now truncate the vector.  This leaves it non-null, so we know
	 there are pure virtuals, but empty so we don't list them out
	 again.  */
      pure->truncate (0);
    }

  return 1;
}

int
abstract_virtuals_error (tree decl, tree type,
			 tsubst_flags_t complain /* = tf_warning_or_error */)
{
  return abstract_virtuals_error (decl, type, ACU_UNKNOWN, complain);
}

int
abstract_virtuals_error (abstract_class_use use, tree type,
			 tsubst_flags_t complain /* = tf_warning_or_error */)
{
  return abstract_virtuals_error (NULL_TREE, type, use, complain);
}


/* Print an inform about the declaration of the incomplete type TYPE.  */

void
cxx_incomplete_type_inform (const_tree type)
{
  if (!TYPE_MAIN_DECL (type))
    return;

  location_t loc = DECL_SOURCE_LOCATION (TYPE_MAIN_DECL (type));
  tree ptype = strip_top_quals (CONST_CAST_TREE (type));

  if (current_class_type
      && TYPE_BEING_DEFINED (current_class_type)
      && same_type_p (ptype, current_class_type))
    inform (loc, "definition of %q#T is not complete until "
	    "the closing brace", ptype);
  else if (!TYPE_TEMPLATE_INFO (ptype))
    inform (loc, "forward declaration of %q#T", ptype);
  else
    inform (loc, "declaration of %q#T", ptype);
}

/* Print an error message for invalid use of an incomplete type.
   VALUE is the expression that was used (or 0 if that isn't known)
   and TYPE is the type that was invalid.  DIAG_KIND indicates the
   type of diagnostic (see diagnostic.def).  */

bool
cxx_incomplete_type_diagnostic (location_t loc, const_tree value,
				const_tree type, diagnostic_t diag_kind)
{
  bool is_decl = false, complained = false;

  /* Avoid duplicate error message.  */
  if (TREE_CODE (type) == ERROR_MARK)
    return false;

  auto_diagnostic_group d;
  if (value)
    {
      STRIP_ANY_LOCATION_WRAPPER (value);

      if (VAR_P (value)
	  || TREE_CODE (value) == PARM_DECL
	  || TREE_CODE (value) == FIELD_DECL)
	{
	  complained = emit_diagnostic (diag_kind, DECL_SOURCE_LOCATION (value), 0,
					"%qD has incomplete type", value);
	  is_decl = true;
	}
    }
 retry:
  /* We must print an error message.  Be clever about what it says.  */

  switch (TREE_CODE (type))
    {
    case RECORD_TYPE:
    case UNION_TYPE:
    case ENUMERAL_TYPE:
      if (!is_decl)
	complained = emit_diagnostic (diag_kind, loc, 0,
				      "invalid use of incomplete type %q#T",
				      type);
      if (complained)
	cxx_incomplete_type_inform (type);
      break;

    case VOID_TYPE:
      complained = emit_diagnostic (diag_kind, loc, 0,
		       "invalid use of %qT", type);
      break;

    case ARRAY_TYPE:
      if (TYPE_DOMAIN (type))
	{
	  type = TREE_TYPE (type);
	  goto retry;
	}
      complained = emit_diagnostic (diag_kind, loc, 0,
		       "invalid use of array with unspecified bounds");
      break;

    case OFFSET_TYPE:
    bad_member:
      {
	tree member = TREE_OPERAND (value, 1);
	if (is_overloaded_fn (member) && !flag_ms_extensions)
	  {
	    gcc_rich_location richloc (loc);
	    /* If "member" has no arguments (other than "this"), then
	       add a fix-it hint.  */
	    member = MAYBE_BASELINK_FUNCTIONS (member);
	    if (TREE_CODE (member) == FUNCTION_DECL
		&& DECL_OBJECT_MEMBER_FUNCTION_P (member)
		&& type_num_arguments (TREE_TYPE (member)) == 1)
	      richloc.add_fixit_insert_after ("()");
	    complained = emit_diagnostic (diag_kind, &richloc, 0,
			     "invalid use of member function %qD "
			     "(did you forget the %<()%> ?)", member);
	  }
	else
	  complained = emit_diagnostic (diag_kind, loc, 0,
			   "invalid use of member %qD "
			   "(did you forget the %<&%> ?)", member);
      }
      break;

    case TEMPLATE_TYPE_PARM:
      if (is_auto (type))
	{
	  if (CLASS_PLACEHOLDER_TEMPLATE (type))
	    complained = emit_diagnostic (diag_kind, loc, 0,
			     "invalid use of placeholder %qT", type);
	  else
	    complained = emit_diagnostic (diag_kind, loc, 0,
			     "invalid use of %qT", type);
	}
      else
	complained = emit_diagnostic (diag_kind, loc, 0,
			 "invalid use of template type parameter %qT", type);
      break;

    case BOUND_TEMPLATE_TEMPLATE_PARM:
      complained = emit_diagnostic (diag_kind, loc, 0,
		       "invalid use of template template parameter %qT",
		       TYPE_NAME (type));
      break;

    case TYPE_PACK_EXPANSION:
      complained = emit_diagnostic (diag_kind, loc, 0,
		       "invalid use of pack expansion %qT", type);
      break;

    case TYPENAME_TYPE:
    case DECLTYPE_TYPE:
      complained = emit_diagnostic (diag_kind, loc, 0,
		       "invalid use of dependent type %qT", type);
      break;

    case LANG_TYPE:
      if (type == init_list_type_node)
	{
	  complained = emit_diagnostic (diag_kind, loc, 0,
			   "invalid use of brace-enclosed initializer list");
	  break;
	}
      gcc_assert (type == unknown_type_node);
      if (value && TREE_CODE (value) == COMPONENT_REF)
	goto bad_member;
      else if (value && TREE_CODE (value) == ADDR_EXPR)
	complained = emit_diagnostic (diag_kind, loc, 0,
			 "address of overloaded function with no contextual "
			 "type information");
      else if (value && TREE_CODE (value) == OVERLOAD)
	complained = emit_diagnostic (diag_kind, loc, 0,
			 "overloaded function with no contextual type information");
      else
	complained = emit_diagnostic (diag_kind, loc, 0,
			 "insufficient contextual information to determine type");
      break;

    default:
      gcc_unreachable ();
    }

  return complained;
}

/* Print an error message for invalid use of an incomplete type.
   VALUE is the expression that was used (or 0 if that isn't known)
   and TYPE is the type that was invalid.  */

void
cxx_incomplete_type_error (location_t loc, const_tree value, const_tree type)
{
  cxx_incomplete_type_diagnostic (loc, value, type, DK_ERROR);
}


/* We've just initialized subobject SUB; also insert a TARGET_EXPR with an
   EH-only cleanup for SUB.  Because of EH region nesting issues, we need to
   make the cleanup conditional on a flag that we will clear once the object is
   fully initialized, so push a new flag onto FLAGS.  */

static void
maybe_push_temp_cleanup (tree sub, vec<tree,va_gc> **flags)
{
  if (!flag_exceptions)
    return;
  if (tree cleanup
      = cxx_maybe_build_cleanup (sub, tf_warning_or_error))
    {
      tree tx = get_internal_target_expr (boolean_true_node);
      tree flag = TARGET_EXPR_SLOT (tx);
      TARGET_EXPR_CLEANUP (tx) = build3 (COND_EXPR, void_type_node,
					 flag, cleanup, void_node);
      add_stmt (tx);
      vec_safe_push (*flags, flag);
    }
}

/* F is something added to a cleanup flags vec by maybe_push_temp_cleanup or
   build_vec_init.  Return the code to disable the cleanup it controls.  */

tree
build_disable_temp_cleanup (tree f)
{
  tree d = f;
  tree i = boolean_false_node;
  if (TREE_CODE (f) == TREE_LIST)
    {
      /* To disable a build_vec_init cleanup, set
	 iterator = maxindex.  */
      d = TREE_PURPOSE (f);
      i = TREE_VALUE (f);
      ggc_free (f);
    }
  return build2 (MODIFY_EXPR, TREE_TYPE (d), d, i);
}

/* The recursive part of split_nonconstant_init.  DEST is an lvalue
   expression to which INIT should be assigned.  INIT is a CONSTRUCTOR.
   Return true if the whole of the value was initialized by the
   generated statements.  */

static bool
split_nonconstant_init_1 (tree dest, tree init, bool last,
			  vec<tree,va_gc> **flags)
{
  unsigned HOST_WIDE_INT idx, tidx = HOST_WIDE_INT_M1U;
  tree field_index, value;
  tree type = TREE_TYPE (dest);
  tree inner_type = NULL;
  bool array_type_p = false;
  bool complete_p = true;
  HOST_WIDE_INT num_split_elts = 0;
  tree last_split_elt = NULL_TREE;

  switch (TREE_CODE (type))
    {
    case ARRAY_TYPE:
      inner_type = TREE_TYPE (type);
      array_type_p = true;
      if ((TREE_SIDE_EFFECTS (init)
	   && TYPE_HAS_NONTRIVIAL_DESTRUCTOR (type))
	  || vla_type_p (type))
	{
	  if (!TYPE_DOMAIN (type)
	      && TREE_CODE (init) == CONSTRUCTOR
	      && CONSTRUCTOR_NELTS (init))
	    {
	      /* Flexible array.  */
	      cp_complete_array_type (&type, init, /*default*/true);
	      dest = build1 (VIEW_CONVERT_EXPR, type, dest);
	    }

	  /* For an array, we only need/want a single cleanup region rather
	     than one per element.  build_vec_init will handle it.  */
	  tree code = build_vec_init (dest, NULL_TREE, init, false, 1,
				      tf_warning_or_error, flags);
	  add_stmt (code);
	  return true;
	}
      /* FALLTHRU */

    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (init), idx,
				field_index, value)
	{
	  /* The current implementation of this algorithm assumes that
	     the field was set for all the elements. This is usually done
	     by process_init_constructor.  */
	  gcc_assert (field_index);

	  if (!array_type_p)
	    inner_type = TREE_TYPE (field_index);

	  tree sub;
	  if (array_type_p)
	    sub = build4 (ARRAY_REF, inner_type, dest, field_index,
			  NULL_TREE, NULL_TREE);
	  else
	    sub = build3 (COMPONENT_REF, inner_type, dest, field_index,
			  NULL_TREE);

	  bool elt_last = last && idx == CONSTRUCTOR_NELTS (init) - 1;

	  /* We need to see sub-array TARGET_EXPR before cp_fold_r so we can
	     handle cleanup flags properly.  */
	  gcc_checking_assert (!target_expr_needs_replace (value));

	  if (TREE_CODE (value) == CONSTRUCTOR)
	    {
	      if (!split_nonconstant_init_1 (sub, value, elt_last, flags)
		      /* For flexible array member with initializer we
			 can't remove the initializer, because only the
			 initializer determines how many elements the
			 flexible array member has.  */
		  || (!array_type_p
		      && TREE_CODE (inner_type) == ARRAY_TYPE
		      && TYPE_DOMAIN (inner_type) == NULL
		      && TREE_CODE (TREE_TYPE (value)) == ARRAY_TYPE
		      && COMPLETE_TYPE_P (TREE_TYPE (value))
		      && !integer_zerop (TYPE_SIZE (TREE_TYPE (value)))
		      && elt_last
		      && TYPE_HAS_TRIVIAL_DESTRUCTOR
				(strip_array_types (inner_type))))
		complete_p = false;
	      else
		{
		  /* Mark element for removal.  */
		  last_split_elt = field_index;
		  CONSTRUCTOR_ELT (init, idx)->index = NULL_TREE;
		  if (idx < tidx)
		    tidx = idx;
		  num_split_elts++;
		}
	    }
	  else if (tree vi = get_vec_init_expr (value))
	    {
	      add_stmt (expand_vec_init_expr (sub, vi, tf_warning_or_error,
					      flags));

	      /* Mark element for removal.  */
	      last_split_elt = field_index;
	      CONSTRUCTOR_ELT (init, idx)->index = NULL_TREE;
	      if (idx < tidx)
		tidx = idx;
	      num_split_elts++;
	    }
	  else if (!initializer_constant_valid_p (value, inner_type))
	    {
	      tree code;

	      /* Push cleanups for any preceding members with constant
		 initialization.  */
	      if (CLASS_TYPE_P (type))
		for (tree prev = (last_split_elt ?
				  DECL_CHAIN (last_split_elt)
				  : TYPE_FIELDS (type));
		     ; prev = DECL_CHAIN (prev))
		  {
		    prev = next_aggregate_field (prev);
		    if (prev == field_index)
		      break;
		    tree ptype = TREE_TYPE (prev);
		    if (TYPE_P (ptype) && type_build_dtor_call (ptype))
		      {
			tree pcref = build3 (COMPONENT_REF, ptype, dest, prev,
					     NULL_TREE);
			maybe_push_temp_cleanup (pcref, flags);
		      }
		  }

	      /* Mark element for removal.  */
	      CONSTRUCTOR_ELT (init, idx)->index = NULL_TREE;
	      if (idx < tidx)
		tidx = idx;

	      if (TREE_CODE (field_index) == RANGE_EXPR)
		{
		  /* Use build_vec_init to initialize a range.  */
		  tree low = TREE_OPERAND (field_index, 0);
		  tree hi = TREE_OPERAND (field_index, 1);
		  sub = build4 (ARRAY_REF, inner_type, dest, low,
				NULL_TREE, NULL_TREE);
		  sub = cp_build_addr_expr (sub, tf_warning_or_error);
		  tree max = size_binop (MINUS_EXPR, hi, low);
		  code = build_vec_init (sub, max, value, false, 0,
					 tf_warning_or_error);
		  add_stmt (code);
		  if (tree_fits_shwi_p (max))
		    num_split_elts += tree_to_shwi (max);
		}
	      else
		{
		  /* We may need to add a copy constructor call if
		     the field has [[no_unique_address]].  */
		  if (unsafe_return_slot_p (sub))
		    {
		      /* But not if the initializer is an implicit ctor call
			 we just built in digest_init.  */
		      if (TREE_CODE (value) == TARGET_EXPR
			  && TARGET_EXPR_LIST_INIT_P (value)
			  && make_safe_copy_elision (sub, value))
			goto build_init;

		      if (TREE_CODE (value) == TARGET_EXPR)
			/* We have to add this constructor, so we will not
			   elide.  */
			TARGET_EXPR_ELIDING_P (value) = false;

		      tree name = (DECL_FIELD_IS_BASE (field_index)
				   ? base_ctor_identifier
				   : complete_ctor_identifier);
		      releasing_vec args = make_tree_vector_single (value);
		      code = build_special_member_call
			(sub, name, &args, inner_type,
			 LOOKUP_NORMAL, tf_warning_or_error);
		    }
		  else
		    {
		    build_init:
		      code = cp_build_init_expr (sub, value);
		    }
		  code = build_stmt (input_location, EXPR_STMT, code);
		  add_stmt (code);
		  if (!elt_last)
		    maybe_push_temp_cleanup (sub, flags);
		}

	      last_split_elt = field_index;
	      num_split_elts++;
	    }
	}
      if (num_split_elts == 1)
	CONSTRUCTOR_ELTS (init)->ordered_remove (tidx);
      else if (num_split_elts > 1)
	{
	  /* Perform the delayed ordered removal of non-constant elements
	     we split out.  */
	  for (idx = tidx; idx < CONSTRUCTOR_NELTS (init); ++idx)
	    if (CONSTRUCTOR_ELT (init, idx)->index == NULL_TREE)
	      ;
	    else
	      {
		*CONSTRUCTOR_ELT (init, tidx) = *CONSTRUCTOR_ELT (init, idx);
		++tidx;
	      }
	  vec_safe_truncate (CONSTRUCTOR_ELTS (init), tidx);
	}
      break;

    case VECTOR_TYPE:
      if (!initializer_constant_valid_p (init, type))
	{
	  tree code;
	  tree cons = copy_node (init);
	  CONSTRUCTOR_ELTS (init) = NULL;
	  code = build2 (MODIFY_EXPR, type, dest, cons);
	  code = build_stmt (input_location, EXPR_STMT, code);
	  add_stmt (code);
	  num_split_elts += CONSTRUCTOR_NELTS (init);
	}
      break;

    default:
      gcc_unreachable ();
    }

  /* The rest of the initializer is now a constant. */
  TREE_CONSTANT (init) = 1;
  TREE_SIDE_EFFECTS (init) = 0;

  /* We didn't split out anything.  */
  if (num_split_elts == 0)
    return false;

  return complete_p && complete_ctor_at_level_p (TREE_TYPE (init),
						 num_split_elts, inner_type);
}

/* A subroutine of store_init_value.  Splits non-constant static
   initializer INIT into a constant part and generates code to
   perform the non-constant part of the initialization to DEST.
   Returns the code for the runtime init.  */

tree
split_nonconstant_init (tree dest, tree init)
{
  tree code;

  if (TREE_CODE (init) == TARGET_EXPR)
    init = TARGET_EXPR_INITIAL (init);
  if (TREE_CODE (init) == CONSTRUCTOR)
    {
      /* Subobject initializers are not full-expressions.  */
      auto fe = (make_temp_override
		 (current_stmt_tree ()->stmts_are_full_exprs_p, 0));

      init = cp_fully_fold_init (init);
      code = push_stmt_list ();

      /* If the complete object is an array, build_vec_init's cleanup is
	 enough.  Otherwise, collect flags for disabling subobject
	 cleanups once the complete object is fully constructed.  */
      vec<tree, va_gc> *flags = nullptr;
      if (TREE_CODE (TREE_TYPE (dest)) != ARRAY_TYPE)
	flags = make_tree_vector ();

      if (split_nonconstant_init_1 (dest, init, true, &flags))
	init = NULL_TREE;

      for (tree f : flags)
	finish_expr_stmt (build_disable_temp_cleanup (f));
      release_tree_vector (flags);

      code = pop_stmt_list (code);
      if (VAR_P (dest) && !is_local_temp (dest))
	{
	  DECL_INITIAL (dest) = init;
	  TREE_READONLY (dest) = 0;
	}
      else if (init)
	{
	  tree ie = cp_build_init_expr (dest, init);
	  code = add_stmt_to_compound (ie, code);
	}
    }
  else if (TREE_CODE (init) == STRING_CST
	   && array_of_runtime_bound_p (TREE_TYPE (dest)))
    code = build_vec_init (dest, NULL_TREE, init, /*value-init*/false,
			   /*from array*/1, tf_warning_or_error);
  else
    code = cp_build_init_expr (dest, init);

  return code;
}

/* T is the initializer of a constexpr variable.  Set CONSTRUCTOR_MUTABLE_POISON
   for any CONSTRUCTOR within T that contains (directly or indirectly) a mutable
   member, thereby poisoning it so it can't be copied to another a constexpr
   variable or read during constexpr evaluation.  */

static void
poison_mutable_constructors (tree t)
{
  if (TREE_CODE (t) != CONSTRUCTOR)
    return;

  if (cp_has_mutable_p (TREE_TYPE (t)))
    {
      CONSTRUCTOR_MUTABLE_POISON (t) = true;

      if (vec<constructor_elt, va_gc> *elts = CONSTRUCTOR_ELTS (t))
	for (const constructor_elt &ce : *elts)
	  poison_mutable_constructors (ce.value);
    }
}

/* Perform appropriate conversions on the initial value of a variable,
   store it in the declaration DECL,
   and print any error messages that are appropriate.
   If the init is invalid, store an ERROR_MARK.

   C++: Note that INIT might be a TREE_LIST, which would mean that it is
   a base class initializer for some aggregate type, hopefully compatible
   with DECL.  If INIT is a single element, and DECL is an aggregate
   type, we silently convert INIT into a TREE_LIST, allowing a constructor
   to be called.

   If INIT is a TREE_LIST and there is no constructor, turn INIT
   into a CONSTRUCTOR and use standard initialization techniques.
   Perhaps a warning should be generated?

   Returns code to be executed if initialization could not be performed
   for static variable.  In that case, caller must emit the code.  */

tree
store_init_value (tree decl, tree init, vec<tree, va_gc>** cleanups, int flags)
{
  tree value, type;

  /* If variable's type was invalidly declared, just ignore it.  */

  type = TREE_TYPE (decl);
  if (TREE_CODE (type) == ERROR_MARK)
    return NULL_TREE;

  if (MAYBE_CLASS_TYPE_P (type))
    {
      if (TREE_CODE (init) == TREE_LIST)
	{
	  error ("constructor syntax used, but no constructor declared "
		 "for type %qT", type);
	  init = build_constructor_from_list (init_list_type_node, nreverse (init));
	}
    }

  /* End of special C++ code.  */

  if (flags & LOOKUP_ALREADY_DIGESTED)
    value = init;
  else
    {
      if (TREE_STATIC (decl))
	flags |= LOOKUP_ALLOW_FLEXARRAY_INIT;
      /* Digest the specified initializer into an expression.  */
      value = digest_init_flags (type, init, flags, tf_warning_or_error);
    }

  /* Look for braced array initializers for character arrays and
     recursively convert them into STRING_CSTs.  */
  value = braced_lists_to_strings (type, value);

  current_ref_temp_count = 0;
  value = extend_ref_init_temps (decl, value, cleanups);

  /* In C++11 constant expression is a semantic, not syntactic, property.
     In C++98, make sure that what we thought was a constant expression at
     template definition time is still constant and otherwise perform this
     as optimization, e.g. to fold SIZEOF_EXPRs in the initializer.  */
  if (decl_maybe_constant_var_p (decl) || TREE_STATIC (decl))
    {
      bool const_init;
      tree oldval = value;
      if (DECL_DECLARED_CONSTEXPR_P (decl)
	  || DECL_DECLARED_CONSTINIT_P (decl)
	  || (DECL_IN_AGGR_P (decl)
	      && DECL_INITIALIZED_IN_CLASS_P (decl)))
	{
	  value = fold_non_dependent_expr (value, tf_warning_or_error,
					   /*manifestly_const_eval=*/true,
					   decl);
	  if (value == error_mark_node)
	    ;
	  /* Diagnose a non-constant initializer for constexpr variable or
	     non-inline in-class-initialized static data member.  */
	  else if (!is_constant_expression (value))
	    {
	      /* Maybe we want to give this message for constexpr variables as
		 well, but that will mean a lot of testsuite adjustment.  */
	      if (DECL_DECLARED_CONSTINIT_P (decl))
	      error_at (location_of (decl),
			"%<constinit%> variable %qD does not have a "
			"constant initializer", decl);
	      require_constant_expression (value);
	      value = error_mark_node;
	    }
	  else
	    {
	      value = maybe_constant_init (value, decl, true);

	      /* In a template we might not have done the necessary
		 transformations to make value actually constant,
		 e.g. extend_ref_init_temps.  */
	      if (!processing_template_decl
		  && !TREE_CONSTANT (value))
		{
		  if (DECL_DECLARED_CONSTINIT_P (decl))
		  error_at (location_of (decl),
			    "%<constinit%> variable %qD does not have a "
			    "constant initializer", decl);
		  value = cxx_constant_init (value, decl);
		}
	    }
	}
      else
	value = fold_non_dependent_init (value, tf_warning_or_error,
					 /*manifestly_const_eval=*/true, decl);
      poison_mutable_constructors (value);
      const_init = (reduced_constant_expression_p (value)
		    || error_operand_p (value));
      DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P (decl) = const_init;
      /* FIXME setting TREE_CONSTANT on refs breaks the back end.  */
      if (!TYPE_REF_P (type))
	TREE_CONSTANT (decl) = const_init && decl_maybe_constant_var_p (decl);
      if (!const_init)
	value = oldval;
    }
  /* Don't fold initializers of automatic variables in constexpr functions,
     that might fold away something that needs to be diagnosed at constexpr
     evaluation time.  */
  if (!current_function_decl
      || !DECL_DECLARED_CONSTEXPR_P (current_function_decl)
      || TREE_STATIC (decl))
    value = cp_fully_fold_init (value);

  /* Handle aggregate NSDMI in non-constant initializers, too.  */
  value = replace_placeholders (value, decl);

  /* A COMPOUND_LITERAL_P CONSTRUCTOR is the syntactic form; by the time we get
     here it should have been digested into an actual value for the type.  */
  gcc_checking_assert (TREE_CODE (value) != CONSTRUCTOR
		       || processing_template_decl
		       || VECTOR_TYPE_P (type)
		       || !TREE_HAS_CONSTRUCTOR (value));

  /* If the initializer is not a constant, fill in DECL_INITIAL with
     the bits that are constant, and then return an expression that
     will perform the dynamic initialization.  */
  if (value != error_mark_node
      && !processing_template_decl
      && (TREE_SIDE_EFFECTS (value)
	  || vla_type_p (type)
	  || ! reduced_constant_expression_p (value)))
    return split_nonconstant_init (decl, value);

  /* DECL may change value; purge caches.  */
  clear_cv_and_fold_caches ();

  /* If the value is a constant, just put it in DECL_INITIAL.  If DECL
     is an automatic variable, the middle end will turn this into a
     dynamic initialization later.  */
  DECL_INITIAL (decl) = value;
  return NULL_TREE;
}


/* Give diagnostic about narrowing conversions within { }, or as part of
   a converted constant expression.  If CONST_ONLY, only check
   constants.  */

bool
check_narrowing (tree type, tree init, tsubst_flags_t complain,
		 bool const_only/*= false*/)
{
  tree ftype = unlowered_expr_type (init);
  bool ok = true;
  REAL_VALUE_TYPE d;

  if (((!warn_narrowing || !(complain & tf_warning))
       && cxx_dialect == cxx98)
      || !ARITHMETIC_TYPE_P (type)
      /* Don't emit bogus warnings with e.g. value-dependent trees.  */
      || instantiation_dependent_expression_p (init))
    return ok;

  if (BRACE_ENCLOSED_INITIALIZER_P (init)
      && TREE_CODE (type) == COMPLEX_TYPE)
    {
      tree elttype = TREE_TYPE (type);
      if (CONSTRUCTOR_NELTS (init) > 0)
        ok &= check_narrowing (elttype, CONSTRUCTOR_ELT (init, 0)->value,
			       complain);
      if (CONSTRUCTOR_NELTS (init) > 1)
	ok &= check_narrowing (elttype, CONSTRUCTOR_ELT (init, 1)->value,
			       complain);
      return ok;
    }

  /* Even non-dependent expressions can still have template
     codes like CAST_EXPR, so use *_non_dependent_expr to cope.  */
  init = fold_non_dependent_expr (init, complain, /*manifest*/true);
  if (init == error_mark_node)
    return ok;

  /* If we were asked to only check constants, return early.  */
  if (const_only && !TREE_CONSTANT (init))
    return ok;

  if (CP_INTEGRAL_TYPE_P (type)
      && SCALAR_FLOAT_TYPE_P (ftype))
    ok = false;
  else if (INTEGRAL_OR_ENUMERATION_TYPE_P (ftype)
	   && CP_INTEGRAL_TYPE_P (type))
    {
      if (TREE_CODE (ftype) == ENUMERAL_TYPE)
	/* Check for narrowing based on the values of the enumeration. */
	ftype = ENUM_UNDERLYING_TYPE (ftype);
      /* Undo convert_bitfield_to_declared_type (STRIP_NOPS isn't enough).  */
      tree op = init;
      while (CONVERT_EXPR_P (op))
	op = TREE_OPERAND (op, 0);
      /* Core 2627 says that we shouldn't warn when "the source is a bit-field
	 whose width w is less than that of its type (or, for an enumeration
	 type, its underlying type) and the target type can represent all the
	 values of a hypothetical extended integer type with width w and with
	 the same signedness as the original type".  */
      if (is_bitfield_expr_with_lowered_type (op)
	  && TYPE_PRECISION (TREE_TYPE (op)) < TYPE_PRECISION (ftype))
	ftype = TREE_TYPE (op);
      if ((tree_int_cst_lt (TYPE_MAX_VALUE (type),
			    TYPE_MAX_VALUE (ftype))
	   || tree_int_cst_lt (TYPE_MIN_VALUE (ftype),
			       TYPE_MIN_VALUE (type)))
	  && (TREE_CODE (init) != INTEGER_CST
	      || !int_fits_type_p (init, type)))
	ok = false;
    }
  /* [dcl.init.list]#7.2: "from long double to double or float, or from
      double to float".  */
  else if (SCALAR_FLOAT_TYPE_P (ftype)
	   && SCALAR_FLOAT_TYPE_P (type))
    {
      if ((extended_float_type_p (ftype) || extended_float_type_p (type))
	  ? /* "from a floating-point type T to another floating-point type
	       whose floating-point conversion rank is neither greater than
	       nor equal to that of T".
	       So, it is ok if
	       cp_compare_floating_point_conversion_ranks (ftype, type)
	       returns -2 (type has greater conversion rank than ftype)
	       or [-1..1] (type has equal conversion rank as ftype, possibly
	       different subrank.  Only do this if at least one of the
	       types is extended floating-point type, otherwise keep doing
	       what we did before (for the sake of non-standard
	       backend types).  */
	    cp_compare_floating_point_conversion_ranks (ftype, type) >= 2
	  : ((same_type_p (ftype, long_double_type_node)
	      && (same_type_p (type, double_type_node)
		  || same_type_p (type, float_type_node)))
	     || (same_type_p (ftype, double_type_node)
		 && same_type_p (type, float_type_node))
	     || (TYPE_PRECISION (type) < TYPE_PRECISION (ftype))))
	{
	  if (TREE_CODE (init) == REAL_CST)
	    {
	      /* Issue 703: Loss of precision is OK as long as the value is
		 within the representable range of the new type.  */
	      REAL_VALUE_TYPE r;
	      d = TREE_REAL_CST (init);
	      real_convert (&r, TYPE_MODE (type), &d);
	      if (real_isinf (&r))
		ok = false;
	    }
	  else
	    ok = false;
	}
    }
  else if (INTEGRAL_OR_ENUMERATION_TYPE_P (ftype)
	   && SCALAR_FLOAT_TYPE_P (type))
    {
      ok = false;
      if (TREE_CODE (init) == INTEGER_CST)
	{
	  d = real_value_from_int_cst (0, init);
	  if (exact_real_truncate (TYPE_MODE (type), &d))
	    ok = true;
	}
    }
  else if (TREE_CODE (type) == BOOLEAN_TYPE
	   && (TYPE_PTR_P (ftype) || TYPE_PTRMEM_P (ftype)))
    /* C++20 P1957R2: converting from a pointer type or a pointer-to-member
       type to bool should be considered narrowing.  This is a DR so is not
       limited to C++20 only.  */
    ok = false;

  bool almost_ok = ok;
  if (!ok && !CONSTANT_CLASS_P (init) && (complain & tf_warning_or_error))
    {
      tree folded = cp_fully_fold (init);
      if (TREE_CONSTANT (folded) && check_narrowing (type, folded, tf_none))
	almost_ok = true;
    }

  if (!ok)
    {
      location_t loc = cp_expr_loc_or_input_loc (init);
      if (cxx_dialect == cxx98)
	{
	  if (complain & tf_warning)
	    warning_at (loc, OPT_Wnarrowing, "narrowing conversion of %qE "
			"from %qH to %qI is ill-formed in C++11",
			init, ftype, type);
	  ok = true;
	}
      else if (!CONSTANT_CLASS_P (init))
	{
	  if (complain & tf_warning_or_error)
	    {
	      auto_diagnostic_group d;
	      if ((!almost_ok || pedantic)
		  && pedwarn (loc, OPT_Wnarrowing,
			      "narrowing conversion of %qE from %qH to %qI",
			      init, ftype, type)
		  && almost_ok)
		inform (loc, " the expression has a constant value but is not "
			"a C++ constant-expression");
	      ok = true;
	    }
	}
      else if (complain & tf_error)
	{
	  int savederrorcount = errorcount;
	  permerror_opt (loc, OPT_Wnarrowing,
			 "narrowing conversion of %qE from %qH to %qI",
			 init, ftype, type);
	  if (errorcount == savederrorcount)
	    ok = true;
	}
    }

  return ok;
}

/* True iff TYPE is a C++20 "ordinary" character type.  */

bool
ordinary_char_type_p (tree type)
{
  type = TYPE_MAIN_VARIANT (type);
  return (type == char_type_node
	  || type == signed_char_type_node
	  || type == unsigned_char_type_node);
}

/* True iff the string literal INIT has a type suitable for initializing array
   TYPE.  */

bool
array_string_literal_compatible_p (tree type, tree init)
{
  tree to_char_type = TYPE_MAIN_VARIANT (TREE_TYPE (type));
  tree from_char_type = TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (init)));

  if (to_char_type == from_char_type)
    return true;
  /* The array element type does not match the initializing string
     literal element type; this is only allowed when both types are
     ordinary character type.  There are no string literals of
     signed or unsigned char type in the language, but we can get
     them internally from converting braced-init-lists to
     STRING_CST.  */
  if (ordinary_char_type_p (to_char_type)
      && ordinary_char_type_p (from_char_type))
    return true;

  /* P2513 (C++20/C++23): "an array of char or unsigned char may
     be initialized by a UTF-8 string literal, or by such a string
     literal enclosed in braces."  */
  if (from_char_type == char8_type_node
      && (to_char_type == char_type_node
	  || to_char_type == unsigned_char_type_node))
    return true;

  return false;
}

/* Process the initializer INIT for a variable of type TYPE, emitting
   diagnostics for invalid initializers and converting the initializer as
   appropriate.

   For aggregate types, it assumes that reshape_init has already run, thus the
   initializer will have the right shape (brace elision has been undone).

   NESTED is non-zero iff we are being called for an element of a CONSTRUCTOR,
   2 iff the element of a CONSTRUCTOR is inside another CONSTRUCTOR.  */

static tree
digest_init_r (tree type, tree init, int nested, int flags,
	       tsubst_flags_t complain)
{
  enum tree_code code = TREE_CODE (type);

  if (error_operand_p (init))
    return error_mark_node;

  gcc_assert (init);

  /* We must strip the outermost array type when completing the type,
     because the its bounds might be incomplete at the moment.  */
  if (!complete_type_or_maybe_complain (code == ARRAY_TYPE
					? TREE_TYPE (type) : type, NULL_TREE,
					complain))
    return error_mark_node;

  location_t loc = cp_expr_loc_or_input_loc (init);

  tree stripped_init = init;

  if (BRACE_ENCLOSED_INITIALIZER_P (init)
      && CONSTRUCTOR_IS_PAREN_INIT (init))
    flags |= LOOKUP_AGGREGATE_PAREN_INIT;

  /* Strip NON_LVALUE_EXPRs since we aren't using as an lvalue
     (g++.old-deja/g++.law/casts2.C).  */
  if (TREE_CODE (init) == NON_LVALUE_EXPR)
    stripped_init = TREE_OPERAND (init, 0);

  stripped_init = tree_strip_any_location_wrapper (stripped_init);

  /* Initialization of an array of chars from a string constant. The initializer
     can be optionally enclosed in braces, but reshape_init has already removed
     them if they were present.  */
  if (code == ARRAY_TYPE)
    {
      if (nested && !TYPE_DOMAIN (type))
	/* C++ flexible array members have a null domain.  */
	{
	  if (flags & LOOKUP_ALLOW_FLEXARRAY_INIT)
	    pedwarn (loc, OPT_Wpedantic,
		     "initialization of a flexible array member");
	  else
	    {
	      if (complain & tf_error)
		error_at (loc, "non-static initialization of"
			       " a flexible array member");
	      return error_mark_node;
	    }
	}

      tree typ1 = TYPE_MAIN_VARIANT (TREE_TYPE (type));
      if (char_type_p (typ1)
	  && TREE_CODE (stripped_init) == STRING_CST)
	{
	  if (!array_string_literal_compatible_p (type, init))
	    {
	      if (complain & tf_error)
		error_at (loc, "cannot initialize array of %qT from "
			  "a string literal with type array of %qT",
			  typ1,
			  TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (init))));
	      return error_mark_node;
	    }

	  if (nested == 2 && !TYPE_DOMAIN (type))
	    {
	      if (complain & tf_error)
		error_at (loc, "initialization of flexible array member "
			       "in a nested context");
	      return error_mark_node;
	    }

	  if (type != TREE_TYPE (init)
	      && !variably_modified_type_p (type, NULL_TREE))
	    {
	      init = copy_node (init);
	      TREE_TYPE (init) = type;
	      /* If we have a location wrapper, then also copy the wrapped
		 node, and update the copy's type.  */
	      if (location_wrapper_p (init))
		{
		  stripped_init = copy_node (stripped_init);
		  TREE_OPERAND (init, 0) = stripped_init;
		  TREE_TYPE (stripped_init) = type;
		}
	    }
	  if (TYPE_DOMAIN (type) && TREE_CONSTANT (TYPE_SIZE (type)))
	    {
	      /* Not a flexible array member.  */
	      int size = TREE_INT_CST_LOW (TYPE_SIZE (type));
	      size = (size + BITS_PER_UNIT - 1) / BITS_PER_UNIT;
	      /* In C it is ok to subtract 1 from the length of the string
		 because it's ok to ignore the terminating null char that is
		 counted in the length of the constant, but in C++ this would
		 be invalid.  */
	      if (size < TREE_STRING_LENGTH (stripped_init))
		{
		  permerror (loc, "initializer-string for %qT is too long",
			     type);

		  init = build_string (size,
				       TREE_STRING_POINTER (stripped_init));
		  TREE_TYPE (init) = type;
		}
	    }
	  return init;
	}
    }

  /* Handle scalar types (including conversions) and references.  */
  if ((code != COMPLEX_TYPE || BRACE_ENCLOSED_INITIALIZER_P (stripped_init))
      && (SCALAR_TYPE_P (type) || code == REFERENCE_TYPE))
    {
      /* Narrowing is OK when initializing an aggregate from
	 a parenthesized list.  */
      if (nested && !(flags & LOOKUP_AGGREGATE_PAREN_INIT))
	flags |= LOOKUP_NO_NARROWING;
      if (TREE_CODE (init) == RAW_DATA_CST && !TYPE_UNSIGNED (type))
	{
	  tree ret = init;
	  if ((flags & LOOKUP_NO_NARROWING) || warn_conversion)
	    for (unsigned int i = 0;
		 i < (unsigned) RAW_DATA_LENGTH (init); ++i)
	      if (RAW_DATA_SCHAR_ELT (init, i) < 0)
		{
		  if ((flags & LOOKUP_NO_NARROWING))
		    {
		      tree elt
			= build_int_cst (integer_type_node,
					 RAW_DATA_UCHAR_ELT (init, i));
		      if (!check_narrowing (type, elt, complain, false))
			{
			  if (!(complain & tf_warning_or_error))
			    ret = error_mark_node;
			  continue;
			}
		    }
		  if (warn_conversion)
		    warning (OPT_Wconversion,
			     "conversion from %qT to %qT changes value from "
			     "%qd to %qd",
			     integer_type_node, type,
			     RAW_DATA_UCHAR_ELT (init, i),
			     RAW_DATA_SCHAR_ELT (init, i));
		}
	  return ret;
	}
      init = convert_for_initialization (0, type, init, flags,
					 ICR_INIT, NULL_TREE, 0,
					 complain);

      return init;
    }

  /* Come here only for aggregates: records, arrays, unions, complex numbers
     and vectors.  */
  gcc_assert (code == ARRAY_TYPE
	      || VECTOR_TYPE_P (type)
	      || code == RECORD_TYPE
	      || code == UNION_TYPE
	      || code == OPAQUE_TYPE
	      || code == COMPLEX_TYPE);

  /* "If T is a class type and the initializer list has a single
     element of type cv U, where U is T or a class derived from T,
     the object is initialized from that element."  */
  if (cxx_dialect >= cxx11
      && BRACE_ENCLOSED_INITIALIZER_P (stripped_init)
      && !CONSTRUCTOR_IS_DESIGNATED_INIT (stripped_init)
      && CONSTRUCTOR_NELTS (stripped_init) == 1
      && ((CLASS_TYPE_P (type) && !CLASSTYPE_NON_AGGREGATE (type))
	  || VECTOR_TYPE_P (type)))
    {
      tree elt = CONSTRUCTOR_ELT (stripped_init, 0)->value;
      if (reference_related_p (type, TREE_TYPE (elt)))
	{
	  /* In C++17, aggregates can have bases, thus participate in
	     aggregate initialization.  In the following case:

	       struct B { int c; };
	       struct D : B { };
	       D d{{D{{42}}}};

	    there's an extra set of braces, so the D temporary initializes
	    the first element of d, which is the B base subobject.  The base
	    of type B is copy-initialized from the D temporary, causing
	    object slicing.  */
	  tree field = next_aggregate_field (TYPE_FIELDS (type));
	  if (field && DECL_FIELD_IS_BASE (field))
	    {
	      if (warning_at (loc, 0, "initializing a base class of type %qT "
			      "results in object slicing", TREE_TYPE (field)))
		inform (loc, "remove %<{ }%> around initializer");
	    }
	  else if (flag_checking)
	    /* We should have fixed this in reshape_init.  */
	    gcc_unreachable ();
	}
    }

  if (SIMPLE_TARGET_EXPR_P (stripped_init))
    stripped_init = TARGET_EXPR_INITIAL (stripped_init);

  if (BRACE_ENCLOSED_INITIALIZER_P (stripped_init)
      && !TYPE_NON_AGGREGATE_CLASS (type))
    return process_init_constructor (type, stripped_init, nested, flags,
				     complain);
  else
    {
      if (COMPOUND_LITERAL_P (stripped_init) && code == ARRAY_TYPE)
	{
	  if (complain & tf_error)
	    error_at (loc, "cannot initialize aggregate of type %qT with "
		      "a compound literal", type);

	  return error_mark_node;
	}

      if (code == ARRAY_TYPE
	  && !BRACE_ENCLOSED_INITIALIZER_P (stripped_init))
	{
	  /* Allow the result of build_array_copy and of
	     build_value_init_noctor.  */
	  if ((TREE_CODE (stripped_init) == VEC_INIT_EXPR
	       || TREE_CODE (stripped_init) == CONSTRUCTOR)
	      && (same_type_ignoring_top_level_qualifiers_p
		  (type, TREE_TYPE (init))))
	    return init;

	  if (complain & tf_error)
	    error_at (loc, "array must be initialized with a brace-enclosed"
		      " initializer");
	  return error_mark_node;
	}

      return convert_for_initialization (NULL_TREE, type, init,
					 flags,
					 ICR_INIT, NULL_TREE, 0,
                                         complain);
    }
}

tree
digest_init (tree type, tree init, tsubst_flags_t complain)
{
  return digest_init_r (type, init, 0, LOOKUP_IMPLICIT, complain);
}

tree
digest_init_flags (tree type, tree init, int flags, tsubst_flags_t complain)
{
  return digest_init_r (type, init, 0, flags, complain);
}

/* Callback to replace PLACEHOLDER_EXPRs in a TARGET_EXPR (which isn't used
   in the context of guaranteed copy elision).  */

static tree
replace_placeholders_for_class_temp_r (tree *tp, int *, void *)
{
  tree t = *tp;

  /* We're looking for a TARGET_EXPR nested in the whole expression.  */
  if (TREE_CODE (t) == TARGET_EXPR
      /* That serves as temporary materialization, not an initializer.  */
      && !TARGET_EXPR_ELIDING_P (t))
    {
      tree init = TARGET_EXPR_INITIAL (t);
      while (TREE_CODE (init) == COMPOUND_EXPR)
	init = TREE_OPERAND (init, 1);
      if (TREE_CODE (init) == CONSTRUCTOR
	  && CONSTRUCTOR_PLACEHOLDER_BOUNDARY (init))
	{
	  tree obj = TARGET_EXPR_SLOT (t);
	  replace_placeholders (init, obj);
	  /* We should have dealt with all PLACEHOLDER_EXPRs.  */
	  CONSTRUCTOR_PLACEHOLDER_BOUNDARY (init) = false;
	  gcc_checking_assert (!find_placeholders (init));
	}
    }

  return NULL_TREE;
}

/* Process the initializer INIT for an NSDMI DECL (a FIELD_DECL).  */
tree
digest_nsdmi_init (tree decl, tree init, tsubst_flags_t complain)
{
  gcc_assert (TREE_CODE (decl) == FIELD_DECL);

  tree type = TREE_TYPE (decl);
  if (DECL_BIT_FIELD_TYPE (decl))
    type = DECL_BIT_FIELD_TYPE (decl);
  int flags = LOOKUP_IMPLICIT;
  if (DIRECT_LIST_INIT_P (init))
    {
      flags = LOOKUP_NORMAL;
      complain |= tf_no_cleanup;
    }
  if (BRACE_ENCLOSED_INITIALIZER_P (init)
      && CP_AGGREGATE_TYPE_P (type))
    init = reshape_init (type, init, complain);
  init = digest_init_flags (type, init, flags, complain);
  set_target_expr_eliding (init);

  /* We may have temporary materialization in a NSDMI, if the initializer
     has something like A{} in it.  Digesting the {} could have introduced
     a PLACEHOLDER_EXPR referring to A.  Now that we've got a TARGET_EXPR,
     we have an object we can refer to.  The reason we bother doing this
     here is for code like

       struct A {
	 int x;
	 int y = x;
       };

       struct B {
	 int x = 0;
	 int y = A{x}.y; // #1
       };

     where in #1 we don't want to end up with two PLACEHOLDER_EXPRs for
     different types on the same level in a {} when lookup_placeholder
     wouldn't find a named object for the PLACEHOLDER_EXPR for A.  Note,
     temporary materialization does not occur when initializing an object
     from a prvalue of the same type, therefore we must not replace the
     placeholder with a temporary object so that it can be elided.  */
  cp_walk_tree_without_duplicates (&init, replace_placeholders_for_class_temp_r,
				   nullptr);

  return init;
}

/* Set of flags used within process_init_constructor to describe the
   initializers.  */
#define PICFLAG_ERRONEOUS 1
#define PICFLAG_NOT_ALL_CONSTANT 2
#define PICFLAG_NOT_ALL_SIMPLE 4
#define PICFLAG_SIDE_EFFECTS 8
#define PICFLAG_VEC_INIT 16

/* Given an initializer INIT, return the flag (PICFLAG_*) which better
   describe it.  */

static int
picflag_from_initializer (tree init)
{
  if (init == error_mark_node)
    return PICFLAG_ERRONEOUS;
  else if (!TREE_CONSTANT (init))
    {
      if (TREE_SIDE_EFFECTS (init))
	return PICFLAG_SIDE_EFFECTS;
      else
	return PICFLAG_NOT_ALL_CONSTANT;
    }
  else if (!initializer_constant_valid_p (init, TREE_TYPE (init)))
    return PICFLAG_NOT_ALL_SIMPLE;
  return 0;
}

/* Adjust INIT for going into a CONSTRUCTOR.  */

static tree
massage_init_elt (tree type, tree init, int nested, int flags,
		  tsubst_flags_t complain)
{
  int new_flags = LOOKUP_IMPLICIT;
  if (flags & LOOKUP_ALLOW_FLEXARRAY_INIT)
    new_flags |= LOOKUP_ALLOW_FLEXARRAY_INIT;
  if (flags & LOOKUP_AGGREGATE_PAREN_INIT)
    new_flags |= LOOKUP_AGGREGATE_PAREN_INIT;
  init = digest_init_r (type, init, nested ? 2 : 1, new_flags, complain);
  /* When we defer constant folding within a statement, we may want to
     defer this folding as well.  Don't call this on CONSTRUCTORs in
     a template because their elements have already been folded, and
     we must avoid folding the result of get_nsdmi.  */
  if (!(processing_template_decl && TREE_CODE (init) == CONSTRUCTOR))
    {
      tree t = fold_non_dependent_init (init, complain);
      if (TREE_CONSTANT (t))
	init = t;
      set_target_expr_eliding (init);
    }
  return init;
}

/* Subroutine of process_init_constructor, which will process an initializer
   INIT for an array or vector of type TYPE. Returns the flags (PICFLAG_*)
   which describe the initializers.  */

static int
process_init_constructor_array (tree type, tree init, int nested, int flags,
				tsubst_flags_t complain)
{
  unsigned HOST_WIDE_INT i, j, len = 0;
  int picflags = 0;
  bool unbounded = false;
  constructor_elt *ce;
  vec<constructor_elt, va_gc> *v = CONSTRUCTOR_ELTS (init);

  gcc_assert (TREE_CODE (type) == ARRAY_TYPE
	      || VECTOR_TYPE_P (type));

  if (TREE_CODE (type) == ARRAY_TYPE)
    {
      /* C++ flexible array members have a null domain.  */
      tree domain = TYPE_DOMAIN (type);
      if (domain && TREE_CONSTANT (TYPE_MAX_VALUE (domain)))
	len = wi::ext (wi::to_offset (TYPE_MAX_VALUE (domain))
                       - wi::to_offset (TYPE_MIN_VALUE (domain)) + 1,
		       TYPE_PRECISION (TREE_TYPE (domain)),
		       TYPE_SIGN (TREE_TYPE (domain))).to_uhwi ();
      else
	unbounded = true;  /* Take as many as there are.  */

      if (nested == 2 && !domain && !vec_safe_is_empty (v))
	{
	  if (complain & tf_error)
	    error_at (cp_expr_loc_or_input_loc (init),
		      "initialization of flexible array member "
		      "in a nested context");
	  return PICFLAG_ERRONEOUS;
	}
    }
  else
    /* Vectors are like simple fixed-size arrays.  */
    unbounded = !TYPE_VECTOR_SUBPARTS (type).is_constant (&len);

  /* There must not be more initializers than needed.  */
  if (!unbounded && vec_safe_length (v) > len)
    {
      if (complain & tf_error)
	error ("too many initializers for %qT", type);
      else
	return PICFLAG_ERRONEOUS;
    }

  j = 0;
  FOR_EACH_VEC_SAFE_ELT (v, i, ce)
    {
      if (!ce->index)
	ce->index = size_int (j);
      else if (!check_array_designated_initializer (ce, j))
	ce->index = error_mark_node;
      gcc_assert (ce->value);
      ce->value
	= massage_init_elt (TREE_TYPE (type), ce->value, nested, flags,
			    complain);

      gcc_checking_assert
	(ce->value == error_mark_node
	 || (same_type_ignoring_top_level_qualifiers_p
	     (strip_array_types (TREE_TYPE (type)),
	      strip_array_types (TREE_TYPE (ce->value)))));

      picflags |= picflag_from_initializer (ce->value);
      /* Propagate CONSTRUCTOR_PLACEHOLDER_BOUNDARY to outer
	 CONSTRUCTOR.  */
      if (TREE_CODE (ce->value) == CONSTRUCTOR
	  && CONSTRUCTOR_PLACEHOLDER_BOUNDARY (ce->value))
	{
	  CONSTRUCTOR_PLACEHOLDER_BOUNDARY (init) = 1;
	  CONSTRUCTOR_PLACEHOLDER_BOUNDARY (ce->value) = 0;
	}
      if (TREE_CODE (ce->value) == RAW_DATA_CST)
	j += RAW_DATA_LENGTH (ce->value);
      else
	++j;
    }

  /* No more initializers. If the array is unbounded, we are done. Otherwise,
     we must add initializers ourselves.  */
  if (!unbounded)
    for (; i < len; ++i)
      {
	tree next;

	if (type_build_ctor_call (TREE_TYPE (type)))
	  {
	    /* If this type needs constructors run for default-initialization,
	       we can't rely on the back end to do it for us, so make the
	       initialization explicit by list-initializing from T{}.  */
	    next = build_constructor (init_list_type_node, NULL);
	    next = massage_init_elt (TREE_TYPE (type), next, nested, flags,
				     complain);
	    if (initializer_zerop (next))
	      /* The default zero-initialization is fine for us; don't
		 add anything to the CONSTRUCTOR.  */
	      next = NULL_TREE;
	  }
	else if (!zero_init_p (TREE_TYPE (type)))
	  next = build_zero_init (TREE_TYPE (type),
				  /*nelts=*/NULL_TREE,
				  /*static_storage_p=*/false);
	else
	  /* The default zero-initialization is fine for us; don't
	     add anything to the CONSTRUCTOR.  */
	  next = NULL_TREE;

	if (next)
	  {
	    if (next != error_mark_node
		&& (initializer_constant_valid_p (next, TREE_TYPE (next))
		    != null_pointer_node))
	      {
		/* Use VEC_INIT_EXPR for non-constant initialization of
		   trailing elements with no explicit initializers.  */
		picflags |= PICFLAG_VEC_INIT;
		break;
	      }

	    picflags |= picflag_from_initializer (next);
	    /* Propagate CONSTRUCTOR_PLACEHOLDER_BOUNDARY to outer
	       CONSTRUCTOR.  */
	    if (TREE_CODE (next) == CONSTRUCTOR
		&& CONSTRUCTOR_PLACEHOLDER_BOUNDARY (next))
	      {
		CONSTRUCTOR_PLACEHOLDER_BOUNDARY (init) = 1;
		CONSTRUCTOR_PLACEHOLDER_BOUNDARY (next) = 0;
	      }
	    if (len > i+1)
	      {
		tree range = build2 (RANGE_EXPR, size_type_node,
				     build_int_cst (size_type_node, i),
				     build_int_cst (size_type_node, len - 1));
		CONSTRUCTOR_APPEND_ELT (v, range, next);
		break;
	      }
	    else
	      CONSTRUCTOR_APPEND_ELT (v, size_int (i), next);
	  }
	else
	  /* Don't bother checking all the other elements.  */
	  break;
      }

  CONSTRUCTOR_ELTS (init) = v;
  return picflags;
}

/* Subroutine of process_init_constructor, which will process an initializer
   INIT for a class of type TYPE. Returns the flags (PICFLAG_*) which describe
   the initializers.  */

static int
process_init_constructor_record (tree type, tree init, int nested, int flags,
				 tsubst_flags_t complain)
{
  vec<constructor_elt, va_gc> *v = NULL;
  tree field;
  int skipped = 0;

  gcc_assert (TREE_CODE (type) == RECORD_TYPE);
  gcc_assert (!CLASSTYPE_VBASECLASSES (type));
  gcc_assert (!TYPE_BINFO (type)
	      || cxx_dialect >= cxx17
	      || !BINFO_N_BASE_BINFOS (TYPE_BINFO (type)));
  gcc_assert (!TYPE_POLYMORPHIC_P (type));

 restart:
  int picflags = 0;
  unsigned HOST_WIDE_INT idx = 0;
  int designator_skip = -1;
  /* Generally, we will always have an index for each initializer (which is
     a FIELD_DECL, put by reshape_init), but compound literals don't go trough
     reshape_init. So we need to handle both cases.  */
  for (field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
    {
      tree next;

      if (TREE_CODE (field) != FIELD_DECL
	  || (DECL_ARTIFICIAL (field)
	      && !(cxx_dialect >= cxx17 && DECL_FIELD_IS_BASE (field))))
	continue;

      if (DECL_UNNAMED_BIT_FIELD (field))
	continue;

      /* If this is a bitfield, first convert to the declared type.  */
      tree fldtype = TREE_TYPE (field);
      if (DECL_BIT_FIELD_TYPE (field))
	fldtype = DECL_BIT_FIELD_TYPE (field);
      if (fldtype == error_mark_node)
	return PICFLAG_ERRONEOUS;

      next = NULL_TREE;
      if (idx < CONSTRUCTOR_NELTS (init))
	{
	  constructor_elt *ce = &(*CONSTRUCTOR_ELTS (init))[idx];
	  if (ce->index)
	    {
	      /* We can have either a FIELD_DECL or an IDENTIFIER_NODE. The
		 latter case can happen in templates where lookup has to be
		 deferred.  */
	      gcc_assert (TREE_CODE (ce->index) == FIELD_DECL
			  || identifier_p (ce->index));
	      if (ce->index == field || ce->index == DECL_NAME (field))
		next = ce->value;
	      else
		{
		  ce = NULL;
		  if (designator_skip == -1)
		    designator_skip = 1;
		}
	    }
	  else
	    {
	      designator_skip = 0;
	      next = ce->value;
	    }

	  if (ce)
	    {
	      gcc_assert (ce->value);
	      next = massage_init_elt (fldtype, next, nested, flags, complain);
	      ++idx;
	    }
	}
      if (next == error_mark_node)
	/* We skip initializers for empty bases/fields, so skipping an invalid
	   one could make us accept invalid code.  */
	return PICFLAG_ERRONEOUS;
      else if (next)
	/* Already handled above.  */;
      else if (DECL_INITIAL (field))
	{
	  if (skipped > 0)
	    {
	      /* We're using an NSDMI past a field with implicit
	         zero-init.  Go back and make it explicit.  */
	      skipped = -1;
	      vec_safe_truncate (v, 0);
	      goto restart;
	    }
	  /* C++14 aggregate NSDMI.  */
	  next = get_nsdmi (field, /*ctor*/false, complain);
	  if (!CONSTRUCTOR_PLACEHOLDER_BOUNDARY (init)
	      && find_placeholders (next))
	    CONSTRUCTOR_PLACEHOLDER_BOUNDARY (init) = 1;
	}
      else if (type_build_ctor_call (fldtype))
	{
	  /* If this type needs constructors run for
	     default-initialization, we can't rely on the back end to do it
	     for us, so build up TARGET_EXPRs.  If the type in question is
	     a class, just build one up; if it's an array, recurse.  */
	  next = build_constructor (init_list_type_node, NULL);
	  next = massage_init_elt (fldtype, next, nested, flags, complain);
	  if (TREE_CODE (next) == TARGET_EXPR
	      && unsafe_copy_elision_p (field, next))
	    TARGET_EXPR_ELIDING_P (next) = false;

	  /* Warn when some struct elements are implicitly initialized.  */
	  if ((complain & tf_warning)
	      && !cp_unevaluated_operand
	      && !EMPTY_CONSTRUCTOR_P (init))
	    warning (OPT_Wmissing_field_initializers,
		     "missing initializer for member %qD", field);
	}
      else
	{
	  if (TYPE_REF_P (fldtype))
	    {
	      if (complain & tf_error)
		error ("member %qD is uninitialized reference", field);
	      else
		return PICFLAG_ERRONEOUS;
	    }
	  else if (CLASSTYPE_REF_FIELDS_NEED_INIT (fldtype))
	    {
	      if (complain & tf_error)
		error ("member %qD with uninitialized reference fields", field);
	      else
		return PICFLAG_ERRONEOUS;
	    }
	  /* Do nothing for flexible array members since they need not have any
	     elements.  Don't worry about 'skipped' because a flexarray has to
	     be the last field.  */
	  else if (TREE_CODE (fldtype) == ARRAY_TYPE && !TYPE_DOMAIN (fldtype))
	    continue;

	  /* Warn when some struct elements are implicitly initialized
	     to zero.  */
	  if ((complain & tf_warning)
	      && !cp_unevaluated_operand
	      && !EMPTY_CONSTRUCTOR_P (init)
	      && !is_really_empty_class (fldtype, /*ignore_vptr*/false))
	    warning (OPT_Wmissing_field_initializers,
		     "missing initializer for member %qD", field);

	  if (!zero_init_p (fldtype) || skipped < 0)
	    {
	      if (TYPE_REF_P (fldtype))
		next = build_zero_cst (fldtype);
	      else
		next = build_zero_init (fldtype, /*nelts=*/NULL_TREE,
					/*static_storage_p=*/false);
	    }
	  else
	    {
	      /* The default zero-initialization is fine for us; don't
		 add anything to the CONSTRUCTOR.  */
	      skipped = 1;
	      continue;
	    }
	}

      /* We can't actually elide the temporary when initializing a
	 potentially-overlapping field from a function that returns by
	 value.  */
      if (TREE_CODE (next) == TARGET_EXPR
	  && unsafe_copy_elision_p (field, next))
	TARGET_EXPR_ELIDING_P (next) = false;

      if (is_empty_field (field)
	  && !TREE_SIDE_EFFECTS (next))
	/* Don't add trivial initialization of an empty base/field to the
	   constructor, as they might not be ordered the way the back-end
	   expects.  */
	continue;

      /* If this is a bitfield, now convert to the lowered type.  */
      if (fldtype != TREE_TYPE (field))
	next = cp_convert_and_check (TREE_TYPE (field), next, complain);
      picflags |= picflag_from_initializer (next);
      /* Propagate CONSTRUCTOR_PLACEHOLDER_BOUNDARY to outer CONSTRUCTOR.  */
      if (TREE_CODE (next) == CONSTRUCTOR
	  && CONSTRUCTOR_PLACEHOLDER_BOUNDARY (next))
	{
	  CONSTRUCTOR_PLACEHOLDER_BOUNDARY (init) = 1;
	  CONSTRUCTOR_PLACEHOLDER_BOUNDARY (next) = 0;
	}
      CONSTRUCTOR_APPEND_ELT (v, field, next);
    }

  if (idx < CONSTRUCTOR_NELTS (init))
    {
      if (complain & tf_error)
	{
	  constructor_elt *ce = &(*CONSTRUCTOR_ELTS (init))[idx];
	  /* For better diagnostics, try to find out if it is really
	     the case of too many initializers or if designators are
	     in incorrect order.  */
	  if (designator_skip == 1 && ce->index)
	    {
	      gcc_assert (TREE_CODE (ce->index) == FIELD_DECL
			  || identifier_p (ce->index));
	      for (field = TYPE_FIELDS (type);
		   field; field = DECL_CHAIN (field))
		{
		  if (TREE_CODE (field) != FIELD_DECL
		      || (DECL_ARTIFICIAL (field)
			  && !(cxx_dialect >= cxx17
			       && DECL_FIELD_IS_BASE (field))))
		    continue;

		  if (DECL_UNNAMED_BIT_FIELD (field))
		    continue;

		  if (ce->index == field || ce->index == DECL_NAME (field))
		    break;
		}
	    }
	  if (field)
	    error ("designator order for field %qD does not match declaration "
		   "order in %qT", field, type);
	  else
	    error ("too many initializers for %qT", type);
	}
      else
	return PICFLAG_ERRONEOUS;
    }

  CONSTRUCTOR_ELTS (init) = v;
  return picflags;
}

/* Subroutine of process_init_constructor, which will process a single
   initializer INIT for a union of type TYPE. Returns the flags (PICFLAG_*)
   which describe the initializer.  */

static int
process_init_constructor_union (tree type, tree init, int nested, int flags,
				tsubst_flags_t complain)
{
  constructor_elt *ce;
  int len;

  /* If the initializer was empty, use the union's NSDMI if it has one.
     Otherwise use default zero initialization.  */
  if (vec_safe_is_empty (CONSTRUCTOR_ELTS (init)))
    {
      for (tree field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	{
	  if (TREE_CODE (field) == FIELD_DECL
	      && DECL_INITIAL (field) != NULL_TREE)
	    {
	      tree val = get_nsdmi (field, /*in_ctor=*/false, complain);
	      if (!CONSTRUCTOR_PLACEHOLDER_BOUNDARY (init)
		  && find_placeholders (val))
		CONSTRUCTOR_PLACEHOLDER_BOUNDARY (init) = 1;
	      CONSTRUCTOR_APPEND_ELT (CONSTRUCTOR_ELTS (init), field, val);
	      break;
	    }
	}

      if (vec_safe_is_empty (CONSTRUCTOR_ELTS (init)))
	return 0;
    }

  len = CONSTRUCTOR_ELTS (init)->length ();
  if (len > 1)
    {
      if (!(complain & tf_error))
	return PICFLAG_ERRONEOUS;
      error ("too many initializers for %qT", type);
      CONSTRUCTOR_ELTS (init)->block_remove (1, len-1);
    }

  ce = &(*CONSTRUCTOR_ELTS (init))[0];

  /* If this element specifies a field, initialize via that field.  */
  if (ce->index)
    {
      if (TREE_CODE (ce->index) == FIELD_DECL)
	;
      else if (identifier_p (ce->index))
	{
	  /* This can happen within a cast, see g++.dg/opt/cse2.C.  */
	  tree name = ce->index;
	  tree field;
	  for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	    if (DECL_NAME (field) == name)
	      break;
	  if (!field)
	    {
	      if (complain & tf_error)
		error ("no field %qD found in union being initialized",
		       field);
	      ce->value = error_mark_node;
	    }
	  ce->index = field;
	}
      else
	{
	  gcc_assert (TREE_CODE (ce->index) == INTEGER_CST
		      || TREE_CODE (ce->index) == RANGE_EXPR);
	  if (complain & tf_error)
	    error ("index value instead of field name in union initializer");
	  ce->value = error_mark_node;
	}
    }
  else
    {
      /* Find the first named field.  ANSI decided in September 1990
	 that only named fields count here.  */
      tree field = TYPE_FIELDS (type);
      while (field && (!DECL_NAME (field) || TREE_CODE (field) != FIELD_DECL))
	field = TREE_CHAIN (field);
      if (field == NULL_TREE)
	{
	  if (complain & tf_error)
	    error ("too many initializers for %qT", type);
	  ce->value = error_mark_node;
	}
      ce->index = field;
    }

  if (ce->value && ce->value != error_mark_node)
    ce->value = massage_init_elt (TREE_TYPE (ce->index), ce->value, nested,
				  flags, complain);

  /* Propagate CONSTRUCTOR_PLACEHOLDER_BOUNDARY to outer CONSTRUCTOR.  */
  if (ce->value
      && TREE_CODE (ce->value) == CONSTRUCTOR
      && CONSTRUCTOR_PLACEHOLDER_BOUNDARY (ce->value))
    {
      CONSTRUCTOR_PLACEHOLDER_BOUNDARY (init) = 1;
      CONSTRUCTOR_PLACEHOLDER_BOUNDARY (ce->value) = 0;
    }
  return picflag_from_initializer (ce->value);
}

/* Process INIT, a constructor for a variable of aggregate type TYPE. The
   constructor is a brace-enclosed initializer, and will be modified in-place.

   Each element is converted to the right type through digest_init, and
   missing initializers are added following the language rules (zero-padding,
   etc.).

   After the execution, the initializer will have TREE_CONSTANT if all elts are
   constant, and TREE_STATIC set if, in addition, all elts are simple enough
   constants that the assembler and linker can compute them.

   The function returns the initializer itself, or error_mark_node in case
   of error.  */

static tree
process_init_constructor (tree type, tree init, int nested, int flags,
			  tsubst_flags_t complain)
{
  int picflags;

  gcc_assert (BRACE_ENCLOSED_INITIALIZER_P (init));

  if (TREE_CODE (type) == ARRAY_TYPE || VECTOR_TYPE_P (type))
    picflags = process_init_constructor_array (type, init, nested, flags,
					       complain);
  else if (TREE_CODE (type) == RECORD_TYPE)
    picflags = process_init_constructor_record (type, init, nested, flags,
						complain);
  else if (TREE_CODE (type) == UNION_TYPE)
    picflags = process_init_constructor_union (type, init, nested, flags,
					       complain);
  else
    gcc_unreachable ();

  if (picflags & PICFLAG_ERRONEOUS)
    return error_mark_node;

  TREE_TYPE (init) = type;
  if (TREE_CODE (type) == ARRAY_TYPE && TYPE_DOMAIN (type) == NULL_TREE)
    cp_complete_array_type (&TREE_TYPE (init), init, /*do_default=*/0);
  if (picflags & PICFLAG_SIDE_EFFECTS)
    {
      TREE_CONSTANT (init) = false;
      TREE_SIDE_EFFECTS (init) = true;
    }
  else if (picflags & PICFLAG_NOT_ALL_CONSTANT)
    {
      /* Make sure TREE_CONSTANT isn't set from build_constructor.  */
      TREE_CONSTANT (init) = false;
      TREE_SIDE_EFFECTS (init) = false;
    }
  else
    {
      TREE_CONSTANT (init) = 1;
      TREE_SIDE_EFFECTS (init) = false;
      if (!(picflags & PICFLAG_NOT_ALL_SIMPLE))
	TREE_STATIC (init) = 1;
    }
  if (picflags & PICFLAG_VEC_INIT)
    {
      /* Defer default-initialization of array elements with no corresponding
	 initializer-clause until later so we can use a loop.  */
      TREE_TYPE (init) = init_list_type_node;
      init = build_vec_init_expr (type, init, complain);
      init = get_target_expr (init);
    }
  return init;
}

/* Given a structure or union value DATUM, construct and return
   the structure or union component which results from narrowing
   that value to the base specified in BASETYPE.  For example, given the
   hierarchy

   class L { int ii; };
   class A : L { ... };
   class B : L { ... };
   class C : A, B { ... };

   and the declaration

   C x;

   then the expression

   x.A::ii refers to the ii member of the L part of
   the A part of the C object named by X.  In this case,
   DATUM would be x, and BASETYPE would be A.

   I used to think that this was nonconformant, that the standard specified
   that first we look up ii in A, then convert x to an L& and pull out the
   ii part.  But in fact, it does say that we convert x to an A&; A here
   is known as the "naming class".  (jason 2000-12-19)

   BINFO_P points to a variable initialized either to NULL_TREE or to the
   binfo for the specific base subobject we want to convert to.  */

tree
build_scoped_ref (tree datum, tree basetype, tree* binfo_p)
{
  tree binfo;

  if (datum == error_mark_node)
    return error_mark_node;
  if (*binfo_p)
    binfo = *binfo_p;
  else
    binfo = lookup_base (TREE_TYPE (datum), basetype, ba_check,
			 NULL, tf_warning_or_error);

  if (!binfo || binfo == error_mark_node)
    {
      *binfo_p = NULL_TREE;
      if (!binfo)
	error_not_base_type (basetype, TREE_TYPE (datum));
      return error_mark_node;
    }

  *binfo_p = binfo;
  return build_base_path (PLUS_EXPR, datum, binfo, 1,
			  tf_warning_or_error);
}

/* Build a reference to an object specified by the C++ `->' operator.
   Usually this just involves dereferencing the object, but if the
   `->' operator is overloaded, then such overloads must be
   performed until an object which does not have the `->' operator
   overloaded is found.  An error is reported when circular pointer
   delegation is detected.  */

tree
build_x_arrow (location_t loc, tree expr, tsubst_flags_t complain)
{
  tree orig_expr = expr;
  tree type = TREE_TYPE (expr);
  tree last_rval = NULL_TREE;
  vec<tree, va_gc> *types_memoized = NULL;

  if (type == error_mark_node)
    return error_mark_node;

  if (processing_template_decl)
    {
      tree ttype = NULL_TREE;
      if (type && TYPE_PTR_P (type))
	ttype = TREE_TYPE (type);
      if (ttype && !dependent_scope_p (ttype))
	/* Pointer to current instantiation, don't treat as dependent.  */;
      else if (type_dependent_expression_p (expr))
	{
	  expr = build_min_nt_loc (loc, ARROW_EXPR, expr);
	  TREE_TYPE (expr) = ttype;
	  return expr;
	}
    }

  if (MAYBE_CLASS_TYPE_P (type))
    {
      struct tinst_level *actual_inst = current_instantiation ();
      tree fn = NULL;

      while ((expr = build_new_op (loc, COMPONENT_REF,
				   LOOKUP_NORMAL, expr, NULL_TREE, NULL_TREE,
				   NULL_TREE, &fn, complain)))
	{
	  if (expr == error_mark_node)
	    return error_mark_node;

	  /* This provides a better instantiation backtrace in case of
	     error.  */
	  if (fn && DECL_USE_TEMPLATE (fn))
	    push_tinst_level_loc (fn,
				  (current_instantiation () != actual_inst)
				  ? DECL_SOURCE_LOCATION (fn)
				  : input_location);
	  fn = NULL;

	  if (vec_member (TREE_TYPE (expr), types_memoized))
	    {
	      if (complain & tf_error)
		error ("circular pointer delegation detected");
	      return error_mark_node;
	    }

	  vec_safe_push (types_memoized, TREE_TYPE (expr));
	  last_rval = expr;
	}

      while (current_instantiation () != actual_inst)
	pop_tinst_level ();

      if (last_rval == NULL_TREE)
	{
	  if (complain & tf_error)
	    error ("base operand of %<->%> has non-pointer type %qT", type);
	  return error_mark_node;
	}

      if (TYPE_REF_P (TREE_TYPE (last_rval)))
	last_rval = convert_from_reference (last_rval);
    }
  else
    {
      last_rval = decay_conversion (expr, complain);
      if (last_rval == error_mark_node)
	return error_mark_node;
    }

  if (TYPE_PTR_P (TREE_TYPE (last_rval)))
    {
      if (processing_template_decl)
	{
	  expr = build_min (ARROW_EXPR, TREE_TYPE (TREE_TYPE (last_rval)),
			    orig_expr);
	  TREE_SIDE_EFFECTS (expr) = TREE_SIDE_EFFECTS (last_rval);
	  return expr;
	}

      return cp_build_indirect_ref (loc, last_rval, RO_ARROW, complain);
    }

  if (complain & tf_error)
    {
      if (types_memoized)
	error ("result of %<operator->()%> yields non-pointer result");
      else
	error ("base operand of %<->%> is not a pointer");
    }
  return error_mark_node;
}

/* Return an expression for "DATUM .* COMPONENT".  DATUM has not
   already been checked out to be of aggregate type.  */

tree
build_m_component_ref (tree datum, tree component, tsubst_flags_t complain)
{
  tree ptrmem_type;
  tree objtype;
  tree type;
  tree binfo;
  tree ctype;

  datum = mark_lvalue_use (datum);
  component = mark_rvalue_use (component);

  if (error_operand_p (datum) || error_operand_p (component))
    return error_mark_node;

  ptrmem_type = TREE_TYPE (component);
  if (!TYPE_PTRMEM_P (ptrmem_type))
    {
      if (complain & tf_error)
	error ("%qE cannot be used as a member pointer, since it is of "
	       "type %qT", component, ptrmem_type);
      return error_mark_node;
    }

  objtype = TYPE_MAIN_VARIANT (TREE_TYPE (datum));
  if (! MAYBE_CLASS_TYPE_P (objtype))
    {
      if (complain & tf_error)
	error ("cannot apply member pointer %qE to %qE, which is of "
	       "non-class type %qT", component, datum, objtype);
      return error_mark_node;
    }

  type = TYPE_PTRMEM_POINTED_TO_TYPE (ptrmem_type);
  ctype = complete_type (TYPE_PTRMEM_CLASS_TYPE (ptrmem_type));

  if (!COMPLETE_TYPE_P (ctype))
    {
      if (!same_type_p (ctype, objtype))
	goto mismatch;
      binfo = NULL;
    }
  else
    {
      binfo = lookup_base (objtype, ctype, ba_check, NULL, complain);

      if (!binfo)
	{
	mismatch:
	  if (complain & tf_error)
	    error ("pointer to member type %qT incompatible with object "
		   "type %qT", type, objtype);
	  return error_mark_node;
	}
      else if (binfo == error_mark_node)
	return error_mark_node;
    }

  if (TYPE_PTRDATAMEM_P (ptrmem_type))
    {
      bool is_lval = real_lvalue_p (datum);
      tree ptype;

      /* Compute the type of the field, as described in [expr.ref].
	 There's no such thing as a mutable pointer-to-member, so
	 things are not as complex as they are for references to
	 non-static data members.  */
      type = cp_build_qualified_type (type,
				      (cp_type_quals (type)
				       | cp_type_quals (TREE_TYPE (datum))));

      datum = cp_build_addr_expr (datum, complain);

      /* Convert object to the correct base.  */
      if (binfo)
	{
	  datum = build_base_path (PLUS_EXPR, datum, binfo, 1, complain);
	  if (datum == error_mark_node)
	    return error_mark_node;
	}

      /* Build an expression for "object + offset" where offset is the
	 value stored in the pointer-to-data-member.  */
      ptype = build_pointer_type (type);
      datum = convert (ptype, datum);
      if (!processing_template_decl)
	datum = build2 (POINTER_PLUS_EXPR, ptype,
			datum, convert_to_ptrofftype (component));
      datum = cp_fully_fold (datum);
      datum = cp_build_fold_indirect_ref (datum);
      if (datum == error_mark_node)
	return error_mark_node;

      /* If the object expression was an rvalue, return an rvalue.  */
      if (!is_lval)
	datum = move (datum);
      return datum;
    }
  else
    {
      /* 5.5/6: In a .* expression whose object expression is an rvalue, the
	 program is ill-formed if the second operand is a pointer to member
	 function with ref-qualifier & (for C++20: unless its cv-qualifier-seq
	 is const). In a .* expression whose object expression is an lvalue,
	 the program is ill-formed if the second operand is a pointer to member
	 function with ref-qualifier &&.  */
      if (FUNCTION_REF_QUALIFIED (type))
	{
	  bool lval = lvalue_p (datum);
	  if (lval && FUNCTION_RVALUE_QUALIFIED (type))
	    {
	      if (complain & tf_error)
		error ("pointer-to-member-function type %qT requires an rvalue",
		       ptrmem_type);
	      return error_mark_node;
	    }
	  else if (!lval && !FUNCTION_RVALUE_QUALIFIED (type))
	    {
	      if ((type_memfn_quals (type)
		   & (TYPE_QUAL_CONST | TYPE_QUAL_VOLATILE))
		  != TYPE_QUAL_CONST)
		{
		  if (complain & tf_error)
		    error ("pointer-to-member-function type %qT requires "
			   "an lvalue", ptrmem_type);
		  return error_mark_node;
		}
	      else if (cxx_dialect < cxx20)
		{
		  if (complain & tf_warning_or_error)
		    pedwarn (input_location, OPT_Wpedantic,
			     "pointer-to-member-function type %qT requires "
			     "an lvalue before C++20", ptrmem_type);
		  else
		    return error_mark_node;
		}
	    }
	}
      return build2 (OFFSET_REF, type, datum, component);
    }
}

/* Return a tree node for the expression TYPENAME '(' PARMS ')'.  */

static tree
build_functional_cast_1 (location_t loc, tree exp, tree parms,
			 tsubst_flags_t complain)
{
  /* This is either a call to a constructor,
     or a C cast in C++'s `functional' notation.  */

  /* The type to which we are casting.  */
  tree type;

  if (error_operand_p (exp) || parms == error_mark_node)
    return error_mark_node;

  if (TREE_CODE (exp) == TYPE_DECL)
    {
      type = TREE_TYPE (exp);

      if (DECL_ARTIFICIAL (exp))
	cp_handle_deprecated_or_unavailable (type);
    }
  else
    type = exp;

  /* We need to check this explicitly, since value-initialization of
     arrays is allowed in other situations.  */
  if (TREE_CODE (type) == ARRAY_TYPE)
    {
      if (complain & tf_error)
	error_at (loc, "functional cast to array type %qT", type);
      return error_mark_node;
    }

  if (tree anode = type_uses_auto (type))
    {
      tree init;
      if (CLASS_PLACEHOLDER_TEMPLATE (anode))
	init = parms;
      /* C++23 auto(x).  */
      else if (!AUTO_IS_DECLTYPE (anode)
	       && list_length (parms) == 1)
	{
	  init = TREE_VALUE (parms);
	  if (is_constrained_auto (anode))
	    {
	      if (complain & tf_error)
		error_at (loc, "%<auto(x)%> cannot be constrained");
	      return error_mark_node;
	    }
	  else if (cxx_dialect < cxx23)
	    pedwarn (loc, OPT_Wc__23_extensions,
		     "%<auto(x)%> only available with "
		     "%<-std=c++23%> or %<-std=gnu++23%>");
	}
      else
	{
	  if (complain & tf_error)
	    error_at (loc, "invalid use of %qT", anode);
	  return error_mark_node;
	}
      type = do_auto_deduction (type, init, anode, complain,
				adc_variable_type);
      if (type == error_mark_node)
	return error_mark_node;
    }

  if (processing_template_decl)
    {
      tree t;

      /* Diagnose this even in a template.  We could also try harder
	 to give all the usual errors when the type and args are
	 non-dependent...  */
      if (TYPE_REF_P (type) && !parms)
	{
	  if (complain & tf_error)
	    error_at (loc, "invalid value-initialization of reference type");
	  return error_mark_node;
	}

      t = build_min (CAST_EXPR, type, parms);
      /* We don't know if it will or will not have side effects.  */
      TREE_SIDE_EFFECTS (t) = 1;
      return t;
    }

  if (! MAYBE_CLASS_TYPE_P (type))
    {
      if (parms == NULL_TREE)
	{
	  if (VOID_TYPE_P (type))
	    return void_node;
	  return build_value_init (cv_unqualified (type), complain);
	}

      /* This must build a C cast.  */
      parms = build_x_compound_expr_from_list (parms, ELK_FUNC_CAST, complain);
      return cp_build_c_cast (loc, type, parms, complain);
    }

  /* Prepare to evaluate as a call to a constructor.  If this expression
     is actually used, for example,

     return X (arg1, arg2, ...);

     then the slot being initialized will be filled in.  */

  if (!complete_type_or_maybe_complain (type, NULL_TREE, complain))
    return error_mark_node;
  if (abstract_virtuals_error (ACU_CAST, type, complain))
    return error_mark_node;

  /* [expr.type.conv]

     If the expression list is a single-expression, the type
     conversion is equivalent (in definedness, and if defined in
     meaning) to the corresponding cast expression.  */
  if (parms && TREE_CHAIN (parms) == NULL_TREE)
    return cp_build_c_cast (loc, type, TREE_VALUE (parms), complain);

  /* [expr.type.conv]

     The expression T(), where T is a simple-type-specifier for a
     non-array complete object type or the (possibly cv-qualified)
     void type, creates an rvalue of the specified type, which is
     value-initialized.  */

  if (parms == NULL_TREE)
    {
      exp = build_value_init (type, complain);
      exp = get_target_expr (exp, complain);
      return exp;
    }

  /* Call the constructor.  */
  releasing_vec parmvec;
  for (; parms != NULL_TREE; parms = TREE_CHAIN (parms))
    vec_safe_push (parmvec, TREE_VALUE (parms));
  exp = build_special_member_call (NULL_TREE, complete_ctor_identifier,
				   &parmvec, type, LOOKUP_NORMAL, complain);

  if (exp == error_mark_node)
    return error_mark_node;

  return build_cplus_new (type, exp, complain);
}

tree
build_functional_cast (location_t loc, tree exp, tree parms,
		       tsubst_flags_t complain)
{
  tree result = build_functional_cast_1 (loc, exp, parms, complain);
  protected_set_expr_location (result, loc);
  return result;
}


/* Add new exception specifier SPEC, to the LIST we currently have.
   If it's already in LIST then do nothing.
   Moan if it's bad and we're allowed to. COMPLAIN < 0 means we
   know what we're doing.  */

tree
add_exception_specifier (tree list, tree spec, tsubst_flags_t complain)
{
  bool ok;
  tree core = spec;
  bool is_ptr;
  diagnostic_t diag_type = DK_UNSPECIFIED; /* none */

  if (spec == error_mark_node)
    return list;

  gcc_assert (spec && (!list || TREE_VALUE (list)));

  /* [except.spec] 1, type in an exception specifier shall not be
     incomplete, or pointer or ref to incomplete other than pointer
     to cv void.  */
  is_ptr = TYPE_PTR_P (core);
  if (is_ptr || TYPE_REF_P (core))
    core = TREE_TYPE (core);
  if (complain < 0)
    ok = true;
  else if (VOID_TYPE_P (core))
    ok = is_ptr;
  else if (TREE_CODE (core) == TEMPLATE_TYPE_PARM)
    ok = true;
  else if (processing_template_decl)
    ok = true;
  else if (!verify_type_context (input_location, TCTX_EXCEPTIONS, core,
				 !(complain & tf_error)))
    return error_mark_node;
  else
    {
      ok = true;
      /* 15.4/1 says that types in an exception specifier must be complete,
	 but it seems more reasonable to only require this on definitions
	 and calls.  So just give a pedwarn at this point; we will give an
	 error later if we hit one of those two cases.  */
      if (!COMPLETE_TYPE_P (complete_type (core)))
	diag_type = DK_PEDWARN; /* pedwarn */
    }

  if (ok)
    {
      tree probe;

      for (probe = list; probe; probe = TREE_CHAIN (probe))
	if (same_type_p (TREE_VALUE (probe), spec))
	  break;
      if (!probe)
	list = tree_cons (NULL_TREE, spec, list);
    }
  else
    diag_type = DK_ERROR; /* error */

  if (diag_type != DK_UNSPECIFIED
      && (complain & tf_warning_or_error))
    cxx_incomplete_type_diagnostic (NULL_TREE, core, diag_type);

  return list;
}

/* Like nothrow_spec_p, but don't abort on deferred noexcept.  */

static bool
nothrow_spec_p_uninst (const_tree spec)
{
  if (DEFERRED_NOEXCEPT_SPEC_P (spec))
    return false;
  return nothrow_spec_p (spec);
}

/* Combine the two exceptions specifier lists LIST and ADD, and return
   their union.  */

tree
merge_exception_specifiers (tree list, tree add)
{
  tree noex, orig_list;

  if (list == error_mark_node || add == error_mark_node)
    return error_mark_node;

  /* No exception-specifier or noexcept(false) are less strict than
     anything else.  Prefer the newer variant (LIST).  */
  if (!list || list == noexcept_false_spec)
    return list;
  else if (!add || add == noexcept_false_spec)
    return add;

  /* noexcept(true) and throw() are stricter than anything else.
     As above, prefer the more recent one (LIST).  */
  if (nothrow_spec_p_uninst (add))
    return list;

  /* Two implicit noexcept specs (e.g. on a destructor) are equivalent.  */
  if (UNEVALUATED_NOEXCEPT_SPEC_P (add)
      && UNEVALUATED_NOEXCEPT_SPEC_P (list))
    return list;
  /* We should have instantiated other deferred noexcept specs by now.  */
  gcc_assert (!DEFERRED_NOEXCEPT_SPEC_P (add));

  if (nothrow_spec_p_uninst (list))
    return add;
  noex = TREE_PURPOSE (list);
  gcc_checking_assert (!TREE_PURPOSE (add)
		       || errorcount || !flag_exceptions
		       || cp_tree_equal (noex, TREE_PURPOSE (add)));

  /* Combine the dynamic-exception-specifiers, if any.  */
  orig_list = list;
  for (; add && TREE_VALUE (add); add = TREE_CHAIN (add))
    {
      tree spec = TREE_VALUE (add);
      tree probe;

      for (probe = orig_list; probe && TREE_VALUE (probe);
	   probe = TREE_CHAIN (probe))
	if (same_type_p (TREE_VALUE (probe), spec))
	  break;
      if (!probe)
	{
	  spec = build_tree_list (NULL_TREE, spec);
	  TREE_CHAIN (spec) = list;
	  list = spec;
	}
    }

  /* Keep the noexcept-specifier at the beginning of the list.  */
  if (noex != TREE_PURPOSE (list))
    list = tree_cons (noex, TREE_VALUE (list), TREE_CHAIN (list));

  return list;
}

/* Subroutine of build_call.  Ensure that each of the types in the
   exception specification is complete.  Technically, 15.4/1 says that
   they need to be complete when we see a declaration of the function,
   but we should be able to get away with only requiring this when the
   function is defined or called.  See also add_exception_specifier.  */

void
require_complete_eh_spec_types (tree fntype, tree decl)
{
  tree raises;
  /* Don't complain about calls to op new.  */
  if (decl && DECL_ARTIFICIAL (decl))
    return;
  for (raises = TYPE_RAISES_EXCEPTIONS (fntype); raises;
       raises = TREE_CHAIN (raises))
    {
      tree type = TREE_VALUE (raises);
      if (type && !COMPLETE_TYPE_P (type))
	{
	  if (decl)
	    error
	      ("call to function %qD which throws incomplete type %q#T",
	       decl, type);
	  else
	    error ("call to function which throws incomplete type %q#T",
		   decl);
	}
    }
}

/* Record that any TARGET_EXPR in T are going to be elided in
   cp_gimplify_init_expr (or sooner).  */

void
set_target_expr_eliding (tree t)
{
  if (!t)
    return;
  switch (TREE_CODE (t))
    {
    case TARGET_EXPR:
      TARGET_EXPR_ELIDING_P (t) = true;
      break;
    case COMPOUND_EXPR:
      set_target_expr_eliding (TREE_OPERAND (t, 1));
      break;
    case COND_EXPR:
      set_target_expr_eliding (TREE_OPERAND (t, 1));
      set_target_expr_eliding (TREE_OPERAND (t, 2));
      break;

    default:
      break;
    }
}

/* Call the above in the process of building an INIT_EXPR.  */

tree
cp_build_init_expr (location_t loc, tree target, tree init)
{
  set_target_expr_eliding (init);
  tree ie = build2_loc (loc, INIT_EXPR, TREE_TYPE (target),
			target, init);
  TREE_SIDE_EFFECTS (ie) = true;
  return ie;
}
