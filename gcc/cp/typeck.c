/* Build expressions with type checking for C++ compiler.
   Copyright (C) 1987-2014 Free Software Foundation, Inc.
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
#include "tm.h"
#include "tree.h"
#include "stor-layout.h"
#include "varasm.h"
#include "cp-tree.h"
#include "flags.h"
#include "diagnostic.h"
#include "intl.h"
#include "target.h"
#include "convert.h"
#include "c-family/c-common.h"
#include "c-family/c-objc.h"
#include "c-family/c-ubsan.h"
#include "params.h"

static tree pfn_from_ptrmemfunc (tree);
static tree delta_from_ptrmemfunc (tree);
static tree convert_for_assignment (tree, tree, impl_conv_rhs, tree, int,
				    tsubst_flags_t, int);
static tree cp_pointer_int_sum (enum tree_code, tree, tree, tsubst_flags_t);
static tree rationalize_conditional_expr (enum tree_code, tree, 
					  tsubst_flags_t);
static int comp_ptr_ttypes_real (tree, tree, int);
static bool comp_except_types (tree, tree, bool);
static bool comp_array_types (const_tree, const_tree, bool);
static tree pointer_diff (tree, tree, tree, tsubst_flags_t);
static tree get_delta_difference (tree, tree, bool, bool, tsubst_flags_t);
static void casts_away_constness_r (tree *, tree *, tsubst_flags_t);
static bool casts_away_constness (tree, tree, tsubst_flags_t);
static void maybe_warn_about_returning_address_of_local (tree);
static tree lookup_destructor (tree, tree, tree, tsubst_flags_t);
static void warn_args_num (location_t, tree, bool);
static int convert_arguments (tree, vec<tree, va_gc> **, tree, int,
                              tsubst_flags_t);

/* Do `exp = require_complete_type (exp);' to make sure exp
   does not have an incomplete type.  (That includes void types.)
   Returns error_mark_node if the VALUE does not have
   complete type when this function returns.  */

tree
require_complete_type_sfinae (tree value, tsubst_flags_t complain)
{
  tree type;

  if (processing_template_decl || value == error_mark_node)
    return value;

  if (TREE_CODE (value) == OVERLOAD)
    type = unknown_type_node;
  else
    type = TREE_TYPE (value);

  if (type == error_mark_node)
    return error_mark_node;

  /* First, detect a valid value with a complete type.  */
  if (COMPLETE_TYPE_P (type))
    return value;

  if (complete_type_or_maybe_complain (type, value, complain))
    return value;
  else
    return error_mark_node;
}

tree
require_complete_type (tree value)
{
  return require_complete_type_sfinae (value, tf_warning_or_error);
}

/* Try to complete TYPE, if it is incomplete.  For example, if TYPE is
   a template instantiation, do the instantiation.  Returns TYPE,
   whether or not it could be completed, unless something goes
   horribly wrong, in which case the error_mark_node is returned.  */

tree
complete_type (tree type)
{
  if (type == NULL_TREE)
    /* Rather than crash, we return something sure to cause an error
       at some point.  */
    return error_mark_node;

  if (type == error_mark_node || COMPLETE_TYPE_P (type))
    ;
  else if (TREE_CODE (type) == ARRAY_TYPE && TYPE_DOMAIN (type))
    {
      tree t = complete_type (TREE_TYPE (type));
      unsigned int needs_constructing, has_nontrivial_dtor;
      if (COMPLETE_TYPE_P (t) && !dependent_type_p (type))
	layout_type (type);
      needs_constructing
	= TYPE_NEEDS_CONSTRUCTING (TYPE_MAIN_VARIANT (t));
      has_nontrivial_dtor
	= TYPE_HAS_NONTRIVIAL_DESTRUCTOR (TYPE_MAIN_VARIANT (t));
      for (t = TYPE_MAIN_VARIANT (type); t; t = TYPE_NEXT_VARIANT (t))
	{
	  TYPE_NEEDS_CONSTRUCTING (t) = needs_constructing;
	  TYPE_HAS_NONTRIVIAL_DESTRUCTOR (t) = has_nontrivial_dtor;
	}
    }
  else if (CLASS_TYPE_P (type) && CLASSTYPE_TEMPLATE_INSTANTIATION (type))
    instantiate_class_template (TYPE_MAIN_VARIANT (type));

  return type;
}

/* Like complete_type, but issue an error if the TYPE cannot be completed.
   VALUE is used for informative diagnostics.
   Returns NULL_TREE if the type cannot be made complete.  */

tree
complete_type_or_maybe_complain (tree type, tree value, tsubst_flags_t complain)
{
  type = complete_type (type);
  if (type == error_mark_node)
    /* We already issued an error.  */
    return NULL_TREE;
  else if (!COMPLETE_TYPE_P (type))
    {
      if (complain & tf_error)
	cxx_incomplete_type_diagnostic (value, type, DK_ERROR);
      return NULL_TREE;
    }
  else
    return type;
}

tree
complete_type_or_else (tree type, tree value)
{
  return complete_type_or_maybe_complain (type, value, tf_warning_or_error);
}

/* Return truthvalue of whether type of EXP is instantiated.  */

int
type_unknown_p (const_tree exp)
{
  return (TREE_CODE (exp) == TREE_LIST
	  || TREE_TYPE (exp) == unknown_type_node);
}


/* Return the common type of two parameter lists.
   We assume that comptypes has already been done and returned 1;
   if that isn't so, this may crash.

   As an optimization, free the space we allocate if the parameter
   lists are already common.  */

static tree
commonparms (tree p1, tree p2)
{
  tree oldargs = p1, newargs, n;
  int i, len;
  int any_change = 0;

  len = list_length (p1);
  newargs = tree_last (p1);

  if (newargs == void_list_node)
    i = 1;
  else
    {
      i = 0;
      newargs = 0;
    }

  for (; i < len; i++)
    newargs = tree_cons (NULL_TREE, NULL_TREE, newargs);

  n = newargs;

  for (i = 0; p1;
       p1 = TREE_CHAIN (p1), p2 = TREE_CHAIN (p2), n = TREE_CHAIN (n), i++)
    {
      if (TREE_PURPOSE (p1) && !TREE_PURPOSE (p2))
	{
	  TREE_PURPOSE (n) = TREE_PURPOSE (p1);
	  any_change = 1;
	}
      else if (! TREE_PURPOSE (p1))
	{
	  if (TREE_PURPOSE (p2))
	    {
	      TREE_PURPOSE (n) = TREE_PURPOSE (p2);
	      any_change = 1;
	    }
	}
      else
	{
	  if (1 != simple_cst_equal (TREE_PURPOSE (p1), TREE_PURPOSE (p2)))
	    any_change = 1;
	  TREE_PURPOSE (n) = TREE_PURPOSE (p2);
	}
      if (TREE_VALUE (p1) != TREE_VALUE (p2))
	{
	  any_change = 1;
	  TREE_VALUE (n) = merge_types (TREE_VALUE (p1), TREE_VALUE (p2));
	}
      else
	TREE_VALUE (n) = TREE_VALUE (p1);
    }
  if (! any_change)
    return oldargs;

  return newargs;
}

/* Given a type, perhaps copied for a typedef,
   find the "original" version of it.  */
static tree
original_type (tree t)
{
  int quals = cp_type_quals (t);
  while (t != error_mark_node
	 && TYPE_NAME (t) != NULL_TREE)
    {
      tree x = TYPE_NAME (t);
      if (TREE_CODE (x) != TYPE_DECL)
	break;
      x = DECL_ORIGINAL_TYPE (x);
      if (x == NULL_TREE)
	break;
      t = x;
    }
  return cp_build_qualified_type (t, quals);
}

/* Return the common type for two arithmetic types T1 and T2 under the
   usual arithmetic conversions.  The default conversions have already
   been applied, and enumerated types converted to their compatible
   integer types.  */

static tree
cp_common_type (tree t1, tree t2)
{
  enum tree_code code1 = TREE_CODE (t1);
  enum tree_code code2 = TREE_CODE (t2);
  tree attributes;


  /* In what follows, we slightly generalize the rules given in [expr] so
     as to deal with `long long' and `complex'.  First, merge the
     attributes.  */
  attributes = (*targetm.merge_type_attributes) (t1, t2);

  if (SCOPED_ENUM_P (t1) || SCOPED_ENUM_P (t2))
    {
      if (TYPE_MAIN_VARIANT (t1) == TYPE_MAIN_VARIANT (t2))
	return build_type_attribute_variant (t1, attributes);
      else
	return NULL_TREE;
    }

  /* FIXME: Attributes.  */
  gcc_assert (ARITHMETIC_TYPE_P (t1)
	      || TREE_CODE (t1) == VECTOR_TYPE
	      || UNSCOPED_ENUM_P (t1));
  gcc_assert (ARITHMETIC_TYPE_P (t2)
	      || TREE_CODE (t2) == VECTOR_TYPE
	      || UNSCOPED_ENUM_P (t2));

  /* If one type is complex, form the common type of the non-complex
     components, then make that complex.  Use T1 or T2 if it is the
     required type.  */
  if (code1 == COMPLEX_TYPE || code2 == COMPLEX_TYPE)
    {
      tree subtype1 = code1 == COMPLEX_TYPE ? TREE_TYPE (t1) : t1;
      tree subtype2 = code2 == COMPLEX_TYPE ? TREE_TYPE (t2) : t2;
      tree subtype
	= type_after_usual_arithmetic_conversions (subtype1, subtype2);

      if (code1 == COMPLEX_TYPE && TREE_TYPE (t1) == subtype)
	return build_type_attribute_variant (t1, attributes);
      else if (code2 == COMPLEX_TYPE && TREE_TYPE (t2) == subtype)
	return build_type_attribute_variant (t2, attributes);
      else
	return build_type_attribute_variant (build_complex_type (subtype),
					     attributes);
    }

  if (code1 == VECTOR_TYPE)
    {
      /* When we get here we should have two vectors of the same size.
	 Just prefer the unsigned one if present.  */
      if (TYPE_UNSIGNED (t1))
	return build_type_attribute_variant (t1, attributes);
      else
	return build_type_attribute_variant (t2, attributes);
    }

  /* If only one is real, use it as the result.  */
  if (code1 == REAL_TYPE && code2 != REAL_TYPE)
    return build_type_attribute_variant (t1, attributes);
  if (code2 == REAL_TYPE && code1 != REAL_TYPE)
    return build_type_attribute_variant (t2, attributes);

  /* Both real or both integers; use the one with greater precision.  */
  if (TYPE_PRECISION (t1) > TYPE_PRECISION (t2))
    return build_type_attribute_variant (t1, attributes);
  else if (TYPE_PRECISION (t2) > TYPE_PRECISION (t1))
    return build_type_attribute_variant (t2, attributes);

  /* The types are the same; no need to do anything fancy.  */
  if (TYPE_MAIN_VARIANT (t1) == TYPE_MAIN_VARIANT (t2))
    return build_type_attribute_variant (t1, attributes);

  if (code1 != REAL_TYPE)
    {
      /* If one is unsigned long long, then convert the other to unsigned
	 long long.  */
      if (same_type_p (TYPE_MAIN_VARIANT (t1), long_long_unsigned_type_node)
	  || same_type_p (TYPE_MAIN_VARIANT (t2), long_long_unsigned_type_node))
	return build_type_attribute_variant (long_long_unsigned_type_node,
					     attributes);
      /* If one is a long long, and the other is an unsigned long, and
	 long long can represent all the values of an unsigned long, then
	 convert to a long long.  Otherwise, convert to an unsigned long
	 long.  Otherwise, if either operand is long long, convert the
	 other to long long.

	 Since we're here, we know the TYPE_PRECISION is the same;
	 therefore converting to long long cannot represent all the values
	 of an unsigned long, so we choose unsigned long long in that
	 case.  */
      if (same_type_p (TYPE_MAIN_VARIANT (t1), long_long_integer_type_node)
	  || same_type_p (TYPE_MAIN_VARIANT (t2), long_long_integer_type_node))
	{
	  tree t = ((TYPE_UNSIGNED (t1) || TYPE_UNSIGNED (t2))
		    ? long_long_unsigned_type_node
		    : long_long_integer_type_node);
	  return build_type_attribute_variant (t, attributes);
	}
      if (int128_integer_type_node != NULL_TREE
	  && (same_type_p (TYPE_MAIN_VARIANT (t1),
			   int128_integer_type_node)
	      || same_type_p (TYPE_MAIN_VARIANT (t2),
			      int128_integer_type_node)))
	{
	  tree t = ((TYPE_UNSIGNED (t1) || TYPE_UNSIGNED (t2))
		    ? int128_unsigned_type_node
		    : int128_integer_type_node);
	  return build_type_attribute_variant (t, attributes);
	}

      /* Go through the same procedure, but for longs.  */
      if (same_type_p (TYPE_MAIN_VARIANT (t1), long_unsigned_type_node)
	  || same_type_p (TYPE_MAIN_VARIANT (t2), long_unsigned_type_node))
	return build_type_attribute_variant (long_unsigned_type_node,
					     attributes);
      if (same_type_p (TYPE_MAIN_VARIANT (t1), long_integer_type_node)
	  || same_type_p (TYPE_MAIN_VARIANT (t2), long_integer_type_node))
	{
	  tree t = ((TYPE_UNSIGNED (t1) || TYPE_UNSIGNED (t2))
		    ? long_unsigned_type_node : long_integer_type_node);
	  return build_type_attribute_variant (t, attributes);
	}
      /* Otherwise prefer the unsigned one.  */
      if (TYPE_UNSIGNED (t1))
	return build_type_attribute_variant (t1, attributes);
      else
	return build_type_attribute_variant (t2, attributes);
    }
  else
    {
      if (same_type_p (TYPE_MAIN_VARIANT (t1), long_double_type_node)
	  || same_type_p (TYPE_MAIN_VARIANT (t2), long_double_type_node))
	return build_type_attribute_variant (long_double_type_node,
					     attributes);
      if (same_type_p (TYPE_MAIN_VARIANT (t1), double_type_node)
	  || same_type_p (TYPE_MAIN_VARIANT (t2), double_type_node))
	return build_type_attribute_variant (double_type_node,
					     attributes);
      if (same_type_p (TYPE_MAIN_VARIANT (t1), float_type_node)
	  || same_type_p (TYPE_MAIN_VARIANT (t2), float_type_node))
	return build_type_attribute_variant (float_type_node,
					     attributes);

      /* Two floating-point types whose TYPE_MAIN_VARIANTs are none of
	 the standard C++ floating-point types.  Logic earlier in this
	 function has already eliminated the possibility that
	 TYPE_PRECISION (t2) != TYPE_PRECISION (t1), so there's no
	 compelling reason to choose one or the other.  */
      return build_type_attribute_variant (t1, attributes);
    }
}

/* T1 and T2 are arithmetic or enumeration types.  Return the type
   that will result from the "usual arithmetic conversions" on T1 and
   T2 as described in [expr].  */

tree
type_after_usual_arithmetic_conversions (tree t1, tree t2)
{
  gcc_assert (ARITHMETIC_TYPE_P (t1)
	      || TREE_CODE (t1) == VECTOR_TYPE
	      || UNSCOPED_ENUM_P (t1));
  gcc_assert (ARITHMETIC_TYPE_P (t2)
	      || TREE_CODE (t2) == VECTOR_TYPE
	      || UNSCOPED_ENUM_P (t2));

  /* Perform the integral promotions.  We do not promote real types here.  */
  if (INTEGRAL_OR_ENUMERATION_TYPE_P (t1)
      && INTEGRAL_OR_ENUMERATION_TYPE_P (t2))
    {
      t1 = type_promotes_to (t1);
      t2 = type_promotes_to (t2);
    }

  return cp_common_type (t1, t2);
}

static void
composite_pointer_error (diagnostic_t kind, tree t1, tree t2,
			 composite_pointer_operation operation)
{
  switch (operation)
    {
    case CPO_COMPARISON:
      emit_diagnostic (kind, input_location, 0,
		       "comparison between "
		       "distinct pointer types %qT and %qT lacks a cast",
		       t1, t2);
      break;
    case CPO_CONVERSION:
      emit_diagnostic (kind, input_location, 0,
		       "conversion between "
		       "distinct pointer types %qT and %qT lacks a cast",
		       t1, t2);
      break;
    case CPO_CONDITIONAL_EXPR:
      emit_diagnostic (kind, input_location, 0,
		       "conditional expression between "
		       "distinct pointer types %qT and %qT lacks a cast",
		       t1, t2);
      break;
    default:
      gcc_unreachable ();
    }
}

/* Subroutine of composite_pointer_type to implement the recursive
   case.  See that function for documentation of the parameters.  */

static tree
composite_pointer_type_r (tree t1, tree t2, 
			  composite_pointer_operation operation,
			  tsubst_flags_t complain)
{
  tree pointee1;
  tree pointee2;
  tree result_type;
  tree attributes;

  /* Determine the types pointed to by T1 and T2.  */
  if (TYPE_PTR_P (t1))
    {
      pointee1 = TREE_TYPE (t1);
      pointee2 = TREE_TYPE (t2);
    }
  else
    {
      pointee1 = TYPE_PTRMEM_POINTED_TO_TYPE (t1);
      pointee2 = TYPE_PTRMEM_POINTED_TO_TYPE (t2);
    }

  /* [expr.rel]

     Otherwise, the composite pointer type is a pointer type
     similar (_conv.qual_) to the type of one of the operands,
     with a cv-qualification signature (_conv.qual_) that is the
     union of the cv-qualification signatures of the operand
     types.  */
  if (same_type_ignoring_top_level_qualifiers_p (pointee1, pointee2))
    result_type = pointee1;
  else if ((TYPE_PTR_P (pointee1) && TYPE_PTR_P (pointee2))
	   || (TYPE_PTRMEM_P (pointee1) && TYPE_PTRMEM_P (pointee2)))
    {
      result_type = composite_pointer_type_r (pointee1, pointee2, operation,
					      complain);
      if (result_type == error_mark_node)
	return error_mark_node;
    }
  else
    {
      if (complain & tf_error)
	composite_pointer_error (DK_PERMERROR, t1, t2, operation);
      else
	return error_mark_node;
      result_type = void_type_node;
    }
  result_type = cp_build_qualified_type (result_type,
					 (cp_type_quals (pointee1)
					  | cp_type_quals (pointee2)));
  /* If the original types were pointers to members, so is the
     result.  */
  if (TYPE_PTRMEM_P (t1))
    {
      if (!same_type_p (TYPE_PTRMEM_CLASS_TYPE (t1),
			TYPE_PTRMEM_CLASS_TYPE (t2)))
	{
	  if (complain & tf_error)
	    composite_pointer_error (DK_PERMERROR, t1, t2, operation);
	  else
	    return error_mark_node;
	}
      result_type = build_ptrmem_type (TYPE_PTRMEM_CLASS_TYPE (t1),
				       result_type);
    }
  else
    result_type = build_pointer_type (result_type);

  /* Merge the attributes.  */
  attributes = (*targetm.merge_type_attributes) (t1, t2);
  return build_type_attribute_variant (result_type, attributes);
}

/* Return the composite pointer type (see [expr.rel]) for T1 and T2.
   ARG1 and ARG2 are the values with those types.  The OPERATION is to
   describe the operation between the pointer types,
   in case an error occurs.

   This routine also implements the computation of a common type for
   pointers-to-members as per [expr.eq].  */

tree
composite_pointer_type (tree t1, tree t2, tree arg1, tree arg2,
			composite_pointer_operation operation, 
			tsubst_flags_t complain)
{
  tree class1;
  tree class2;

  /* [expr.rel]

     If one operand is a null pointer constant, the composite pointer
     type is the type of the other operand.  */
  if (null_ptr_cst_p (arg1))
    return t2;
  if (null_ptr_cst_p (arg2))
    return t1;

  /* We have:

       [expr.rel]

       If one of the operands has type "pointer to cv1 void*", then
       the other has type "pointer to cv2T", and the composite pointer
       type is "pointer to cv12 void", where cv12 is the union of cv1
       and cv2.

    If either type is a pointer to void, make sure it is T1.  */
  if (TYPE_PTR_P (t2) && VOID_TYPE_P (TREE_TYPE (t2)))
    {
      tree t;
      t = t1;
      t1 = t2;
      t2 = t;
    }

  /* Now, if T1 is a pointer to void, merge the qualifiers.  */
  if (TYPE_PTR_P (t1) && VOID_TYPE_P (TREE_TYPE (t1)))
    {
      tree attributes;
      tree result_type;

      if (TYPE_PTRFN_P (t2) && (complain & tf_error))
        {
          switch (operation)
              {
              case CPO_COMPARISON:
                pedwarn (input_location, OPT_Wpedantic, 
                         "ISO C++ forbids comparison between "
                         "pointer of type %<void *%> and pointer-to-function");
                break;
              case CPO_CONVERSION:
                pedwarn (input_location, OPT_Wpedantic,
                         "ISO C++ forbids conversion between "
                         "pointer of type %<void *%> and pointer-to-function");
                break;
              case CPO_CONDITIONAL_EXPR:
                pedwarn (input_location, OPT_Wpedantic,
                         "ISO C++ forbids conditional expression between "
                         "pointer of type %<void *%> and pointer-to-function");
                break;
              default:
                gcc_unreachable ();
              }
        }
      result_type
	= cp_build_qualified_type (void_type_node,
				   (cp_type_quals (TREE_TYPE (t1))
				    | cp_type_quals (TREE_TYPE (t2))));
      result_type = build_pointer_type (result_type);
      /* Merge the attributes.  */
      attributes = (*targetm.merge_type_attributes) (t1, t2);
      return build_type_attribute_variant (result_type, attributes);
    }

  if (c_dialect_objc () && TYPE_PTR_P (t1)
      && TYPE_PTR_P (t2))
    {
      if (objc_have_common_type (t1, t2, -3, NULL_TREE))
	return objc_common_type (t1, t2);
    }

  /* [expr.eq] permits the application of a pointer conversion to
     bring the pointers to a common type.  */
  if (TYPE_PTR_P (t1) && TYPE_PTR_P (t2)
      && CLASS_TYPE_P (TREE_TYPE (t1))
      && CLASS_TYPE_P (TREE_TYPE (t2))
      && !same_type_ignoring_top_level_qualifiers_p (TREE_TYPE (t1),
						     TREE_TYPE (t2)))
    {
      class1 = TREE_TYPE (t1);
      class2 = TREE_TYPE (t2);

      if (DERIVED_FROM_P (class1, class2))
	t2 = (build_pointer_type
	      (cp_build_qualified_type (class1, cp_type_quals (class2))));
      else if (DERIVED_FROM_P (class2, class1))
	t1 = (build_pointer_type
	      (cp_build_qualified_type (class2, cp_type_quals (class1))));
      else
        {
          if (complain & tf_error)
	    composite_pointer_error (DK_ERROR, t1, t2, operation);
          return error_mark_node;
        }
    }
  /* [expr.eq] permits the application of a pointer-to-member
     conversion to change the class type of one of the types.  */
  else if (TYPE_PTRMEM_P (t1)
           && !same_type_p (TYPE_PTRMEM_CLASS_TYPE (t1),
			    TYPE_PTRMEM_CLASS_TYPE (t2)))
    {
      class1 = TYPE_PTRMEM_CLASS_TYPE (t1);
      class2 = TYPE_PTRMEM_CLASS_TYPE (t2);

      if (DERIVED_FROM_P (class1, class2))
	t1 = build_ptrmem_type (class2, TYPE_PTRMEM_POINTED_TO_TYPE (t1));
      else if (DERIVED_FROM_P (class2, class1))
	t2 = build_ptrmem_type (class1, TYPE_PTRMEM_POINTED_TO_TYPE (t2));
      else
        {
          if (complain & tf_error)
            switch (operation)
              {
              case CPO_COMPARISON:
                error ("comparison between distinct "
                       "pointer-to-member types %qT and %qT lacks a cast",
                       t1, t2);
                break;
              case CPO_CONVERSION:
                error ("conversion between distinct "
                       "pointer-to-member types %qT and %qT lacks a cast",
                       t1, t2);
                break;
              case CPO_CONDITIONAL_EXPR:
                error ("conditional expression between distinct "
                       "pointer-to-member types %qT and %qT lacks a cast",
                       t1, t2);
                break;
              default:
                gcc_unreachable ();
              }
          return error_mark_node;
        }
    }

  return composite_pointer_type_r (t1, t2, operation, complain);
}

/* Return the merged type of two types.
   We assume that comptypes has already been done and returned 1;
   if that isn't so, this may crash.

   This just combines attributes and default arguments; any other
   differences would cause the two types to compare unalike.  */

tree
merge_types (tree t1, tree t2)
{
  enum tree_code code1;
  enum tree_code code2;
  tree attributes;

  /* Save time if the two types are the same.  */
  if (t1 == t2)
    return t1;
  if (original_type (t1) == original_type (t2))
    return t1;

  /* If one type is nonsense, use the other.  */
  if (t1 == error_mark_node)
    return t2;
  if (t2 == error_mark_node)
    return t1;

  /* Handle merging an auto redeclaration with a previous deduced
     return type.  */
  if (is_auto (t1))
    return t2;

  /* Merge the attributes.  */
  attributes = (*targetm.merge_type_attributes) (t1, t2);

  if (TYPE_PTRMEMFUNC_P (t1))
    t1 = TYPE_PTRMEMFUNC_FN_TYPE (t1);
  if (TYPE_PTRMEMFUNC_P (t2))
    t2 = TYPE_PTRMEMFUNC_FN_TYPE (t2);

  code1 = TREE_CODE (t1);
  code2 = TREE_CODE (t2);
  if (code1 != code2)
    {
      gcc_assert (code1 == TYPENAME_TYPE || code2 == TYPENAME_TYPE);
      if (code1 == TYPENAME_TYPE)
	{
          t1 = resolve_typename_type (t1, /*only_current_p=*/true);
	  code1 = TREE_CODE (t1);
	}
      else
	{
          t2 = resolve_typename_type (t2, /*only_current_p=*/true);
	  code2 = TREE_CODE (t2);
	}
    }

  switch (code1)
    {
    case POINTER_TYPE:
    case REFERENCE_TYPE:
      /* For two pointers, do this recursively on the target type.  */
      {
	tree target = merge_types (TREE_TYPE (t1), TREE_TYPE (t2));
	int quals = cp_type_quals (t1);

	if (code1 == POINTER_TYPE)
	  t1 = build_pointer_type (target);
	else
	  t1 = cp_build_reference_type (target, TYPE_REF_IS_RVALUE (t1));
	t1 = build_type_attribute_variant (t1, attributes);
	t1 = cp_build_qualified_type (t1, quals);

	if (TREE_CODE (target) == METHOD_TYPE)
	  t1 = build_ptrmemfunc_type (t1);

	return t1;
      }

    case OFFSET_TYPE:
      {
	int quals;
	tree pointee;
	quals = cp_type_quals (t1);
	pointee = merge_types (TYPE_PTRMEM_POINTED_TO_TYPE (t1),
			       TYPE_PTRMEM_POINTED_TO_TYPE (t2));
	t1 = build_ptrmem_type (TYPE_PTRMEM_CLASS_TYPE (t1),
				pointee);
	t1 = cp_build_qualified_type (t1, quals);
	break;
      }

    case ARRAY_TYPE:
      {
	tree elt = merge_types (TREE_TYPE (t1), TREE_TYPE (t2));
	/* Save space: see if the result is identical to one of the args.  */
	if (elt == TREE_TYPE (t1) && TYPE_DOMAIN (t1))
	  return build_type_attribute_variant (t1, attributes);
	if (elt == TREE_TYPE (t2) && TYPE_DOMAIN (t2))
	  return build_type_attribute_variant (t2, attributes);
	/* Merge the element types, and have a size if either arg has one.  */
	t1 = build_cplus_array_type
	  (elt, TYPE_DOMAIN (TYPE_DOMAIN (t1) ? t1 : t2));
	break;
      }

    case FUNCTION_TYPE:
      /* Function types: prefer the one that specified arg types.
	 If both do, merge the arg types.  Also merge the return types.  */
      {
	tree valtype = merge_types (TREE_TYPE (t1), TREE_TYPE (t2));
	tree p1 = TYPE_ARG_TYPES (t1);
	tree p2 = TYPE_ARG_TYPES (t2);
	tree parms;
	tree rval, raises;

	/* Save space: see if the result is identical to one of the args.  */
	if (valtype == TREE_TYPE (t1) && ! p2)
	  return cp_build_type_attribute_variant (t1, attributes);
	if (valtype == TREE_TYPE (t2) && ! p1)
	  return cp_build_type_attribute_variant (t2, attributes);

	/* Simple way if one arg fails to specify argument types.  */
	if (p1 == NULL_TREE || TREE_VALUE (p1) == void_type_node)
	  parms = p2;
	else if (p2 == NULL_TREE || TREE_VALUE (p2) == void_type_node)
	  parms = p1;
	else
	  parms = commonparms (p1, p2);

	rval = build_function_type (valtype, parms);
	gcc_assert (type_memfn_quals (t1) == type_memfn_quals (t2));
	gcc_assert (type_memfn_rqual (t1) == type_memfn_rqual (t2));
	rval = apply_memfn_quals (rval,
				  type_memfn_quals (t1),
				  type_memfn_rqual (t1));
	raises = merge_exception_specifiers (TYPE_RAISES_EXCEPTIONS (t1),
					     TYPE_RAISES_EXCEPTIONS (t2),
					     NULL_TREE);
	t1 = build_exception_variant (rval, raises);
	break;
      }

    case METHOD_TYPE:
      {
	/* Get this value the long way, since TYPE_METHOD_BASETYPE
	   is just the main variant of this.  */
	tree basetype = class_of_this_parm (t2);
	tree raises = merge_exception_specifiers (TYPE_RAISES_EXCEPTIONS (t1),
						  TYPE_RAISES_EXCEPTIONS (t2),
						  NULL_TREE);
	cp_ref_qualifier rqual = type_memfn_rqual (t1);
	tree t3;

	/* If this was a member function type, get back to the
	   original type of type member function (i.e., without
	   the class instance variable up front.  */
	t1 = build_function_type (TREE_TYPE (t1),
				  TREE_CHAIN (TYPE_ARG_TYPES (t1)));
	t2 = build_function_type (TREE_TYPE (t2),
				  TREE_CHAIN (TYPE_ARG_TYPES (t2)));
	t3 = merge_types (t1, t2);
	t3 = build_method_type_directly (basetype, TREE_TYPE (t3),
					 TYPE_ARG_TYPES (t3));
	t1 = build_exception_variant (t3, raises);
	t1 = build_ref_qualified_type (t1, rqual);
	break;
      }

    case TYPENAME_TYPE:
      /* There is no need to merge attributes into a TYPENAME_TYPE.
	 When the type is instantiated it will have whatever
	 attributes result from the instantiation.  */
      return t1;

    default:;
    }

  if (attribute_list_equal (TYPE_ATTRIBUTES (t1), attributes))
    return t1;
  else if (attribute_list_equal (TYPE_ATTRIBUTES (t2), attributes))
    return t2;
  else
    return cp_build_type_attribute_variant (t1, attributes);
}

/* Return the ARRAY_TYPE type without its domain.  */

tree
strip_array_domain (tree type)
{
  tree t2;
  gcc_assert (TREE_CODE (type) == ARRAY_TYPE);
  if (TYPE_DOMAIN (type) == NULL_TREE)
    return type;
  t2 = build_cplus_array_type (TREE_TYPE (type), NULL_TREE);
  return cp_build_type_attribute_variant (t2, TYPE_ATTRIBUTES (type));
}

/* Wrapper around cp_common_type that is used by c-common.c and other
   front end optimizations that remove promotions.  

   Return the common type for two arithmetic types T1 and T2 under the
   usual arithmetic conversions.  The default conversions have already
   been applied, and enumerated types converted to their compatible
   integer types.  */

tree
common_type (tree t1, tree t2)
{
  /* If one type is nonsense, use the other  */
  if (t1 == error_mark_node)
    return t2;
  if (t2 == error_mark_node)
    return t1;

  return cp_common_type (t1, t2);
}

/* Return the common type of two pointer types T1 and T2.  This is the
   type for the result of most arithmetic operations if the operands
   have the given two types.
 
   We assume that comp_target_types has already been done and returned
   nonzero; if that isn't so, this may crash.  */

tree
common_pointer_type (tree t1, tree t2)
{
  gcc_assert ((TYPE_PTR_P (t1) && TYPE_PTR_P (t2))
              || (TYPE_PTRDATAMEM_P (t1) && TYPE_PTRDATAMEM_P (t2))
              || (TYPE_PTRMEMFUNC_P (t1) && TYPE_PTRMEMFUNC_P (t2)));

  return composite_pointer_type (t1, t2, error_mark_node, error_mark_node,
                                 CPO_CONVERSION, tf_warning_or_error);
}

/* Compare two exception specifier types for exactness or subsetness, if
   allowed. Returns false for mismatch, true for match (same, or
   derived and !exact).

   [except.spec] "If a class X ... objects of class X or any class publicly
   and unambiguously derived from X. Similarly, if a pointer type Y * ...
   exceptions of type Y * or that are pointers to any type publicly and
   unambiguously derived from Y. Otherwise a function only allows exceptions
   that have the same type ..."
   This does not mention cv qualifiers and is different to what throw
   [except.throw] and catch [except.catch] will do. They will ignore the
   top level cv qualifiers, and allow qualifiers in the pointer to class
   example.

   We implement the letter of the standard.  */

static bool
comp_except_types (tree a, tree b, bool exact)
{
  if (same_type_p (a, b))
    return true;
  else if (!exact)
    {
      if (cp_type_quals (a) || cp_type_quals (b))
	return false;

      if (TYPE_PTR_P (a) && TYPE_PTR_P (b))
	{
	  a = TREE_TYPE (a);
	  b = TREE_TYPE (b);
	  if (cp_type_quals (a) || cp_type_quals (b))
	    return false;
	}

      if (TREE_CODE (a) != RECORD_TYPE
	  || TREE_CODE (b) != RECORD_TYPE)
	return false;

      if (publicly_uniquely_derived_p (a, b))
	return true;
    }
  return false;
}

/* Return true if TYPE1 and TYPE2 are equivalent exception specifiers.
   If EXACT is ce_derived, T2 can be stricter than T1 (according to 15.4/5).
   If EXACT is ce_normal, the compatibility rules in 15.4/3 apply.
   If EXACT is ce_exact, the specs must be exactly the same. Exception lists
   are unordered, but we've already filtered out duplicates. Most lists will
   be in order, we should try to make use of that.  */

bool
comp_except_specs (const_tree t1, const_tree t2, int exact)
{
  const_tree probe;
  const_tree base;
  int  length = 0;

  if (t1 == t2)
    return true;

  /* First handle noexcept.  */
  if (exact < ce_exact)
    {
      /* noexcept(false) is compatible with no exception-specification,
	 and stricter than any spec.  */
      if (t1 == noexcept_false_spec)
	return t2 == NULL_TREE || exact == ce_derived;
      /* Even a derived noexcept(false) is compatible with no
	 exception-specification.  */
      if (t2 == noexcept_false_spec)
	return t1 == NULL_TREE;

      /* Otherwise, if we aren't looking for an exact match, noexcept is
	 equivalent to throw().  */
      if (t1 == noexcept_true_spec)
	t1 = empty_except_spec;
      if (t2 == noexcept_true_spec)
	t2 = empty_except_spec;
    }

  /* If any noexcept is left, it is only comparable to itself;
     either we're looking for an exact match or we're redeclaring a
     template with dependent noexcept.  */
  if ((t1 && TREE_PURPOSE (t1))
      || (t2 && TREE_PURPOSE (t2)))
    return (t1 && t2
	    && cp_tree_equal (TREE_PURPOSE (t1), TREE_PURPOSE (t2)));

  if (t1 == NULL_TREE)			   /* T1 is ...  */
    return t2 == NULL_TREE || exact == ce_derived;
  if (!TREE_VALUE (t1))			   /* t1 is EMPTY */
    return t2 != NULL_TREE && !TREE_VALUE (t2);
  if (t2 == NULL_TREE)			   /* T2 is ...  */
    return false;
  if (TREE_VALUE (t1) && !TREE_VALUE (t2)) /* T2 is EMPTY, T1 is not */
    return exact == ce_derived;

  /* Neither set is ... or EMPTY, make sure each part of T2 is in T1.
     Count how many we find, to determine exactness. For exact matching and
     ordered T1, T2, this is an O(n) operation, otherwise its worst case is
     O(nm).  */
  for (base = t1; t2 != NULL_TREE; t2 = TREE_CHAIN (t2))
    {
      for (probe = base; probe != NULL_TREE; probe = TREE_CHAIN (probe))
	{
	  tree a = TREE_VALUE (probe);
	  tree b = TREE_VALUE (t2);

	  if (comp_except_types (a, b, exact))
	    {
	      if (probe == base && exact > ce_derived)
		base = TREE_CHAIN (probe);
	      length++;
	      break;
	    }
	}
      if (probe == NULL_TREE)
	return false;
    }
  return exact == ce_derived || base == NULL_TREE || length == list_length (t1);
}

/* Compare the array types T1 and T2.  ALLOW_REDECLARATION is true if
   [] can match [size].  */

static bool
comp_array_types (const_tree t1, const_tree t2, bool allow_redeclaration)
{
  tree d1;
  tree d2;
  tree max1, max2;

  if (t1 == t2)
    return true;

  /* The type of the array elements must be the same.  */
  if (!same_type_p (TREE_TYPE (t1), TREE_TYPE (t2)))
    return false;

  d1 = TYPE_DOMAIN (t1);
  d2 = TYPE_DOMAIN (t2);

  if (d1 == d2)
    return true;

  /* If one of the arrays is dimensionless, and the other has a
     dimension, they are of different types.  However, it is valid to
     write:

       extern int a[];
       int a[3];

     by [basic.link]:

       declarations for an array object can specify
       array types that differ by the presence or absence of a major
       array bound (_dcl.array_).  */
  if (!d1 || !d2)
    return allow_redeclaration;

  /* Check that the dimensions are the same.  */

  if (!cp_tree_equal (TYPE_MIN_VALUE (d1), TYPE_MIN_VALUE (d2)))
    return false;
  max1 = TYPE_MAX_VALUE (d1);
  max2 = TYPE_MAX_VALUE (d2);
  if (processing_template_decl && !abi_version_at_least (2)
      && !value_dependent_expression_p (max1)
      && !value_dependent_expression_p (max2))
    {
      /* With abi-1 we do not fold non-dependent array bounds, (and
	 consequently mangle them incorrectly).  We must therefore
	 fold them here, to verify the domains have the same
	 value.  */
      max1 = fold (max1);
      max2 = fold (max2);
    }

  if (!cp_tree_equal (max1, max2))
    return false;

  return true;
}

/* Compare the relative position of T1 and T2 into their respective
   template parameter list.
   T1 and T2 must be template parameter types.
   Return TRUE if T1 and T2 have the same position, FALSE otherwise.  */

static bool
comp_template_parms_position (tree t1, tree t2)
{
  tree index1, index2;
  gcc_assert (t1 && t2
	      && TREE_CODE (t1) == TREE_CODE (t2)
	      && (TREE_CODE (t1) == BOUND_TEMPLATE_TEMPLATE_PARM
		  || TREE_CODE (t1) == TEMPLATE_TEMPLATE_PARM
		  || TREE_CODE (t1) == TEMPLATE_TYPE_PARM));

  index1 = TEMPLATE_TYPE_PARM_INDEX (TYPE_MAIN_VARIANT (t1));
  index2 = TEMPLATE_TYPE_PARM_INDEX (TYPE_MAIN_VARIANT (t2));

  /* Then compare their relative position.  */
  if (TEMPLATE_PARM_IDX (index1) != TEMPLATE_PARM_IDX (index2)
      || TEMPLATE_PARM_LEVEL (index1) != TEMPLATE_PARM_LEVEL (index2)
      || (TEMPLATE_PARM_PARAMETER_PACK (index1)
	  != TEMPLATE_PARM_PARAMETER_PACK (index2)))
    return false;

  return true;
}

/* Subroutine in comptypes.  */

static bool
structural_comptypes (tree t1, tree t2, int strict)
{
  if (t1 == t2)
    return true;

  /* Suppress errors caused by previously reported errors.  */
  if (t1 == error_mark_node || t2 == error_mark_node)
    return false;

  gcc_assert (TYPE_P (t1) && TYPE_P (t2));

  /* TYPENAME_TYPEs should be resolved if the qualifying scope is the
     current instantiation.  */
  if (TREE_CODE (t1) == TYPENAME_TYPE)
    t1 = resolve_typename_type (t1, /*only_current_p=*/true);

  if (TREE_CODE (t2) == TYPENAME_TYPE)
    t2 = resolve_typename_type (t2, /*only_current_p=*/true);

  if (TYPE_PTRMEMFUNC_P (t1))
    t1 = TYPE_PTRMEMFUNC_FN_TYPE (t1);
  if (TYPE_PTRMEMFUNC_P (t2))
    t2 = TYPE_PTRMEMFUNC_FN_TYPE (t2);

  /* Different classes of types can't be compatible.  */
  if (TREE_CODE (t1) != TREE_CODE (t2))
    return false;

  /* Qualifiers must match.  For array types, we will check when we
     recur on the array element types.  */
  if (TREE_CODE (t1) != ARRAY_TYPE
      && cp_type_quals (t1) != cp_type_quals (t2))
    return false;
  if (TREE_CODE (t1) == FUNCTION_TYPE
      && type_memfn_quals (t1) != type_memfn_quals (t2))
    return false;
  /* Need to check this before TYPE_MAIN_VARIANT.
     FIXME function qualifiers should really change the main variant.  */
  if ((TREE_CODE (t1) == FUNCTION_TYPE
       || TREE_CODE (t1) == METHOD_TYPE)
      && type_memfn_rqual (t1) != type_memfn_rqual (t2))
    return false;
  if (TYPE_FOR_JAVA (t1) != TYPE_FOR_JAVA (t2))
    return false;

  /* Allow for two different type nodes which have essentially the same
     definition.  Note that we already checked for equality of the type
     qualifiers (just above).  */

  if (TREE_CODE (t1) != ARRAY_TYPE
      && TYPE_MAIN_VARIANT (t1) == TYPE_MAIN_VARIANT (t2))
    return true;


  /* Compare the types.  Break out if they could be the same.  */
  switch (TREE_CODE (t1))
    {
    case VOID_TYPE:
    case BOOLEAN_TYPE:
      /* All void and bool types are the same.  */
      break;

    case INTEGER_TYPE:
    case FIXED_POINT_TYPE:
    case REAL_TYPE:
      /* With these nodes, we can't determine type equivalence by
	 looking at what is stored in the nodes themselves, because
	 two nodes might have different TYPE_MAIN_VARIANTs but still
	 represent the same type.  For example, wchar_t and int could
	 have the same properties (TYPE_PRECISION, TYPE_MIN_VALUE,
	 TYPE_MAX_VALUE, etc.), but have different TYPE_MAIN_VARIANTs
	 and are distinct types. On the other hand, int and the
	 following typedef

           typedef int INT __attribute((may_alias));

	 have identical properties, different TYPE_MAIN_VARIANTs, but
	 represent the same type.  The canonical type system keeps
	 track of equivalence in this case, so we fall back on it.  */
      return TYPE_CANONICAL (t1) == TYPE_CANONICAL (t2);

    case TEMPLATE_TEMPLATE_PARM:
    case BOUND_TEMPLATE_TEMPLATE_PARM:
      if (!comp_template_parms_position (t1, t2))
	return false;
      if (!comp_template_parms
	  (DECL_TEMPLATE_PARMS (TEMPLATE_TEMPLATE_PARM_TEMPLATE_DECL (t1)),
	   DECL_TEMPLATE_PARMS (TEMPLATE_TEMPLATE_PARM_TEMPLATE_DECL (t2))))
	return false;
      if (TREE_CODE (t1) == TEMPLATE_TEMPLATE_PARM)
	break;
      /* Don't check inheritance.  */
      strict = COMPARE_STRICT;
      /* Fall through.  */

    case RECORD_TYPE:
    case UNION_TYPE:
      if (TYPE_TEMPLATE_INFO (t1) && TYPE_TEMPLATE_INFO (t2)
	  && (TYPE_TI_TEMPLATE (t1) == TYPE_TI_TEMPLATE (t2)
	      || TREE_CODE (t1) == BOUND_TEMPLATE_TEMPLATE_PARM)
	  && comp_template_args (TYPE_TI_ARGS (t1), TYPE_TI_ARGS (t2)))
	break;

      if ((strict & COMPARE_BASE) && DERIVED_FROM_P (t1, t2))
	break;
      else if ((strict & COMPARE_DERIVED) && DERIVED_FROM_P (t2, t1))
	break;

      return false;

    case OFFSET_TYPE:
      if (!comptypes (TYPE_OFFSET_BASETYPE (t1), TYPE_OFFSET_BASETYPE (t2),
		      strict & ~COMPARE_REDECLARATION))
	return false;
      if (!same_type_p (TREE_TYPE (t1), TREE_TYPE (t2)))
	return false;
      break;

    case REFERENCE_TYPE:
      if (TYPE_REF_IS_RVALUE (t1) != TYPE_REF_IS_RVALUE (t2))
	return false;
      /* fall through to checks for pointer types */

    case POINTER_TYPE:
      if (TYPE_MODE (t1) != TYPE_MODE (t2)
	  || TYPE_REF_CAN_ALIAS_ALL (t1) != TYPE_REF_CAN_ALIAS_ALL (t2)
	  || !same_type_p (TREE_TYPE (t1), TREE_TYPE (t2)))
	return false;
      break;

    case METHOD_TYPE:
    case FUNCTION_TYPE:
      if (!same_type_p (TREE_TYPE (t1), TREE_TYPE (t2)))
	return false;
      if (!compparms (TYPE_ARG_TYPES (t1), TYPE_ARG_TYPES (t2)))
	return false;
      break;

    case ARRAY_TYPE:
      /* Target types must match incl. qualifiers.  */
      if (!comp_array_types (t1, t2, !!(strict & COMPARE_REDECLARATION)))
	return false;
      break;

    case TEMPLATE_TYPE_PARM:
      /* If T1 and T2 don't have the same relative position in their
	 template parameters set, they can't be equal.  */
      if (!comp_template_parms_position (t1, t2))
	return false;
      break;

    case TYPENAME_TYPE:
      if (!cp_tree_equal (TYPENAME_TYPE_FULLNAME (t1),
			  TYPENAME_TYPE_FULLNAME (t2)))
	return false;
      /* Qualifiers don't matter on scopes.  */
      if (!same_type_ignoring_top_level_qualifiers_p (TYPE_CONTEXT (t1),
						      TYPE_CONTEXT (t2)))
	return false;
      break;

    case UNBOUND_CLASS_TEMPLATE:
      if (!cp_tree_equal (TYPE_IDENTIFIER (t1), TYPE_IDENTIFIER (t2)))
	return false;
      if (!same_type_p (TYPE_CONTEXT (t1), TYPE_CONTEXT (t2)))
	return false;
      break;

    case COMPLEX_TYPE:
      if (!same_type_p (TREE_TYPE (t1), TREE_TYPE (t2)))
	return false;
      break;

    case VECTOR_TYPE:
      if (TYPE_VECTOR_SUBPARTS (t1) != TYPE_VECTOR_SUBPARTS (t2)
	  || !same_type_p (TREE_TYPE (t1), TREE_TYPE (t2)))
	return false;
      break;

    case TYPE_PACK_EXPANSION:
      return (same_type_p (PACK_EXPANSION_PATTERN (t1),
			   PACK_EXPANSION_PATTERN (t2))
	      && comp_template_args (PACK_EXPANSION_EXTRA_ARGS (t1),
				     PACK_EXPANSION_EXTRA_ARGS (t2)));

    case DECLTYPE_TYPE:
      if (DECLTYPE_TYPE_ID_EXPR_OR_MEMBER_ACCESS_P (t1)
          != DECLTYPE_TYPE_ID_EXPR_OR_MEMBER_ACCESS_P (t2)
	  || (DECLTYPE_FOR_LAMBDA_CAPTURE (t1)
	      != DECLTYPE_FOR_LAMBDA_CAPTURE (t2))
	  || (DECLTYPE_FOR_LAMBDA_PROXY (t1)
	      != DECLTYPE_FOR_LAMBDA_PROXY (t2))
          || !cp_tree_equal (DECLTYPE_TYPE_EXPR (t1), 
                             DECLTYPE_TYPE_EXPR (t2)))
        return false;
      break;

    case UNDERLYING_TYPE:
      return same_type_p (UNDERLYING_TYPE_TYPE (t1), 
			  UNDERLYING_TYPE_TYPE (t2));

    default:
      return false;
    }

  /* If we get here, we know that from a target independent POV the
     types are the same.  Make sure the target attributes are also
     the same.  */
  return comp_type_attributes (t1, t2);
}

/* Return true if T1 and T2 are related as allowed by STRICT.  STRICT
   is a bitwise-or of the COMPARE_* flags.  */

bool
comptypes (tree t1, tree t2, int strict)
{
  if (strict == COMPARE_STRICT)
    {
      if (t1 == t2)
	return true;

      if (t1 == error_mark_node || t2 == error_mark_node)
	return false;

      if (TYPE_STRUCTURAL_EQUALITY_P (t1) || TYPE_STRUCTURAL_EQUALITY_P (t2))
	/* At least one of the types requires structural equality, so
	   perform a deep check. */
	return structural_comptypes (t1, t2, strict);

#ifdef ENABLE_CHECKING
      if (USE_CANONICAL_TYPES)
	{
	  bool result = structural_comptypes (t1, t2, strict);
	  
	  if (result && TYPE_CANONICAL (t1) != TYPE_CANONICAL (t2))
	    /* The two types are structurally equivalent, but their
	       canonical types were different. This is a failure of the
	       canonical type propagation code.*/
	    internal_error 
	      ("canonical types differ for identical types %T and %T", 
	       t1, t2);
	  else if (!result && TYPE_CANONICAL (t1) == TYPE_CANONICAL (t2))
	    /* Two types are structurally different, but the canonical
	       types are the same. This means we were over-eager in
	       assigning canonical types. */
	    internal_error 
	      ("same canonical type node for different types %T and %T",
	       t1, t2);
	  
	  return result;
	}
#else
      if (USE_CANONICAL_TYPES)
	return TYPE_CANONICAL (t1) == TYPE_CANONICAL (t2);
#endif
      else
	return structural_comptypes (t1, t2, strict);
    }
  else if (strict == COMPARE_STRUCTURAL)
    return structural_comptypes (t1, t2, COMPARE_STRICT);
  else
    return structural_comptypes (t1, t2, strict);
}

/* Returns nonzero iff TYPE1 and TYPE2 are the same type, ignoring
   top-level qualifiers.  */

bool
same_type_ignoring_top_level_qualifiers_p (tree type1, tree type2)
{
  if (type1 == error_mark_node || type2 == error_mark_node)
    return false;

  return same_type_p (TYPE_MAIN_VARIANT (type1), TYPE_MAIN_VARIANT (type2));
}

/* Returns 1 if TYPE1 is at least as qualified as TYPE2.  */

bool
at_least_as_qualified_p (const_tree type1, const_tree type2)
{
  int q1 = cp_type_quals (type1);
  int q2 = cp_type_quals (type2);

  /* All qualifiers for TYPE2 must also appear in TYPE1.  */
  return (q1 & q2) == q2;
}

/* Returns 1 if TYPE1 is more cv-qualified than TYPE2, -1 if TYPE2 is
   more cv-qualified that TYPE1, and 0 otherwise.  */

int
comp_cv_qualification (const_tree type1, const_tree type2)
{
  int q1 = cp_type_quals (type1);
  int q2 = cp_type_quals (type2);

  if (q1 == q2)
    return 0;

  if ((q1 & q2) == q2)
    return 1;
  else if ((q1 & q2) == q1)
    return -1;

  return 0;
}

/* Returns 1 if the cv-qualification signature of TYPE1 is a proper
   subset of the cv-qualification signature of TYPE2, and the types
   are similar.  Returns -1 if the other way 'round, and 0 otherwise.  */

int
comp_cv_qual_signature (tree type1, tree type2)
{
  if (comp_ptr_ttypes_real (type2, type1, -1))
    return 1;
  else if (comp_ptr_ttypes_real (type1, type2, -1))
    return -1;
  else
    return 0;
}

/* Subroutines of `comptypes'.  */

/* Return true if two parameter type lists PARMS1 and PARMS2 are
   equivalent in the sense that functions with those parameter types
   can have equivalent types.  The two lists must be equivalent,
   element by element.  */

bool
compparms (const_tree parms1, const_tree parms2)
{
  const_tree t1, t2;

  /* An unspecified parmlist matches any specified parmlist
     whose argument types don't need default promotions.  */

  for (t1 = parms1, t2 = parms2;
       t1 || t2;
       t1 = TREE_CHAIN (t1), t2 = TREE_CHAIN (t2))
    {
      /* If one parmlist is shorter than the other,
	 they fail to match.  */
      if (!t1 || !t2)
	return false;
      if (!same_type_p (TREE_VALUE (t1), TREE_VALUE (t2)))
	return false;
    }
  return true;
}


/* Process a sizeof or alignof expression where the operand is a
   type.  */

tree
cxx_sizeof_or_alignof_type (tree type, enum tree_code op, bool complain)
{
  tree value;
  bool dependent_p;

  gcc_assert (op == SIZEOF_EXPR || op == ALIGNOF_EXPR);
  if (type == error_mark_node)
    return error_mark_node;

  type = non_reference (type);
  if (TREE_CODE (type) == METHOD_TYPE)
    {
      if (complain)
	pedwarn (input_location, OPT_Wpointer_arith, 
		 "invalid application of %qs to a member function", 
		 operator_name_info[(int) op].name);
      value = size_one_node;
    }

  dependent_p = dependent_type_p (type);
  if (!dependent_p)
    complete_type (type);
  if (dependent_p
      /* VLA types will have a non-constant size.  In the body of an
	 uninstantiated template, we don't need to try to compute the
	 value, because the sizeof expression is not an integral
	 constant expression in that case.  And, if we do try to
	 compute the value, we'll likely end up with SAVE_EXPRs, which
	 the template substitution machinery does not expect to see.  */
      || (processing_template_decl 
	  && COMPLETE_TYPE_P (type)
	  && TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST))
    {
      value = build_min (op, size_type_node, type);
      TREE_READONLY (value) = 1;
      return value;
    }

  if (cxx_dialect >= cxx1y && array_of_runtime_bound_p (type))
    {
      if (complain & tf_warning_or_error)
	pedwarn (input_location, OPT_Wvla,
		 "taking sizeof array of runtime bound");
      else
	return error_mark_node;
    }

  return c_sizeof_or_alignof_type (input_location, complete_type (type),
				   op == SIZEOF_EXPR, false,
				   complain);
}

/* Return the size of the type, without producing any warnings for
   types whose size cannot be taken.  This routine should be used only
   in some other routine that has already produced a diagnostic about
   using the size of such a type.  */
tree 
cxx_sizeof_nowarn (tree type)
{
  if (TREE_CODE (type) == FUNCTION_TYPE
      || VOID_TYPE_P (type)
      || TREE_CODE (type) == ERROR_MARK)
    return size_one_node;
  else if (!COMPLETE_TYPE_P (type))
    return size_zero_node;
  else
    return cxx_sizeof_or_alignof_type (type, SIZEOF_EXPR, false);
}

/* Process a sizeof expression where the operand is an expression.  */

static tree
cxx_sizeof_expr (tree e, tsubst_flags_t complain)
{
  if (e == error_mark_node)
    return error_mark_node;

  if (processing_template_decl)
    {
      e = build_min (SIZEOF_EXPR, size_type_node, e);
      TREE_SIDE_EFFECTS (e) = 0;
      TREE_READONLY (e) = 1;

      return e;
    }

  /* To get the size of a static data member declared as an array of
     unknown bound, we need to instantiate it.  */
  if (VAR_P (e)
      && VAR_HAD_UNKNOWN_BOUND (e)
      && DECL_TEMPLATE_INSTANTIATION (e))
    instantiate_decl (e, /*defer_ok*/true, /*expl_inst_mem*/false);

  e = mark_type_use (e);

  if (TREE_CODE (e) == COMPONENT_REF
      && TREE_CODE (TREE_OPERAND (e, 1)) == FIELD_DECL
      && DECL_C_BIT_FIELD (TREE_OPERAND (e, 1)))
    {
      if (complain & tf_error)
        error ("invalid application of %<sizeof%> to a bit-field");
      else
        return error_mark_node;
      e = char_type_node;
    }
  else if (is_overloaded_fn (e))
    {
      if (complain & tf_error)
        permerror (input_location, "ISO C++ forbids applying %<sizeof%> to an expression of "
                   "function type");
      else
        return error_mark_node;
      e = char_type_node;
    }
  else if (type_unknown_p (e))
    {
      if (complain & tf_error)
        cxx_incomplete_type_error (e, TREE_TYPE (e));
      else
        return error_mark_node;
      e = char_type_node;
    }
  else
    e = TREE_TYPE (e);

  return cxx_sizeof_or_alignof_type (e, SIZEOF_EXPR, complain & tf_error);
}

/* Implement the __alignof keyword: Return the minimum required
   alignment of E, measured in bytes.  For VAR_DECL's and
   FIELD_DECL's return DECL_ALIGN (which can be set from an
   "aligned" __attribute__ specification).  */

static tree
cxx_alignof_expr (tree e, tsubst_flags_t complain)
{
  tree t;

  if (e == error_mark_node)
    return error_mark_node;

  if (processing_template_decl)
    {
      e = build_min (ALIGNOF_EXPR, size_type_node, e);
      TREE_SIDE_EFFECTS (e) = 0;
      TREE_READONLY (e) = 1;

      return e;
    }

  e = mark_type_use (e);

  if (VAR_P (e))
    t = size_int (DECL_ALIGN_UNIT (e));
  else if (TREE_CODE (e) == COMPONENT_REF
	   && TREE_CODE (TREE_OPERAND (e, 1)) == FIELD_DECL
	   && DECL_C_BIT_FIELD (TREE_OPERAND (e, 1)))
    {
      if (complain & tf_error)
        error ("invalid application of %<__alignof%> to a bit-field");
      else
        return error_mark_node;
      t = size_one_node;
    }
  else if (TREE_CODE (e) == COMPONENT_REF
	   && TREE_CODE (TREE_OPERAND (e, 1)) == FIELD_DECL)
    t = size_int (DECL_ALIGN_UNIT (TREE_OPERAND (e, 1)));
  else if (is_overloaded_fn (e))
    {
      if (complain & tf_error)
        permerror (input_location, "ISO C++ forbids applying %<__alignof%> to an expression of "
                   "function type");
      else
        return error_mark_node;
      if (TREE_CODE (e) == FUNCTION_DECL)
	t = size_int (DECL_ALIGN_UNIT (e));
      else
	t = size_one_node;
    }
  else if (type_unknown_p (e))
    {
      if (complain & tf_error)
        cxx_incomplete_type_error (e, TREE_TYPE (e));
      else
        return error_mark_node;
      t = size_one_node;
    }
  else
    return cxx_sizeof_or_alignof_type (TREE_TYPE (e), ALIGNOF_EXPR, 
                                       complain & tf_error);

  return fold_convert (size_type_node, t);
}

/* Process a sizeof or alignof expression E with code OP where the operand
   is an expression.  */

tree
cxx_sizeof_or_alignof_expr (tree e, enum tree_code op, bool complain)
{
  if (op == SIZEOF_EXPR)
    return cxx_sizeof_expr (e, complain? tf_warning_or_error : tf_none);
  else
    return cxx_alignof_expr (e, complain? tf_warning_or_error : tf_none);
}

/*  Build a representation of an expression 'alignas(E).'  Return the
    folded integer value of E if it is an integral constant expression
    that resolves to a valid alignment.  If E depends on a template
    parameter, return a syntactic representation tree of kind
    ALIGNOF_EXPR.  Otherwise, return an error_mark_node if the
    expression is ill formed, or NULL_TREE if E is NULL_TREE.  */

tree
cxx_alignas_expr (tree e)
{
  if (e == NULL_TREE || e == error_mark_node
      || (!TYPE_P (e) && !require_potential_rvalue_constant_expression (e)))
    return e;
  
  if (TYPE_P (e))
    /* [dcl.align]/3:
       
	   When the alignment-specifier is of the form
	   alignas(type-id ), it shall have the same effect as
	   alignas(alignof(type-id )).  */

    return cxx_sizeof_or_alignof_type (e, ALIGNOF_EXPR, false);
  
  /* If we reach this point, it means the alignas expression if of
     the form "alignas(assignment-expression)", so we should follow
     what is stated by [dcl.align]/2.  */

  if (value_dependent_expression_p (e))
    /* Leave value-dependent expression alone for now. */
    return e;

  e = fold_non_dependent_expr (e);
  e = mark_rvalue_use (e);

  /* [dcl.align]/2 says:

         the assignment-expression shall be an integral constant
	 expression.  */
  
  return cxx_constant_value (e);
}


/* EXPR is being used in a context that is not a function call.
   Enforce:

     [expr.ref]

     The expression can be used only as the left-hand operand of a
     member function call.

     [expr.mptr.operator]

     If the result of .* or ->* is a function, then that result can be
     used only as the operand for the function call operator ().

   by issuing an error message if appropriate.  Returns true iff EXPR
   violates these rules.  */

bool
invalid_nonstatic_memfn_p (tree expr, tsubst_flags_t complain)
{
  if (expr == NULL_TREE)
    return false;
  /* Don't enforce this in MS mode.  */
  if (flag_ms_extensions)
    return false;
  if (is_overloaded_fn (expr) && !really_overloaded_fn (expr))
    expr = get_first_fn (expr);
  if (DECL_NONSTATIC_MEMBER_FUNCTION_P (expr))
    {
      if (complain & tf_error)
        error ("invalid use of non-static member function");
      return true;
    }
  return false;
}

/* If EXP is a reference to a bitfield, and the type of EXP does not
   match the declared type of the bitfield, return the declared type
   of the bitfield.  Otherwise, return NULL_TREE.  */

tree
is_bitfield_expr_with_lowered_type (const_tree exp)
{
  switch (TREE_CODE (exp))
    {
    case COND_EXPR:
      if (!is_bitfield_expr_with_lowered_type (TREE_OPERAND (exp, 1)
					       ? TREE_OPERAND (exp, 1)
					       : TREE_OPERAND (exp, 0)))
	return NULL_TREE;
      return is_bitfield_expr_with_lowered_type (TREE_OPERAND (exp, 2));

    case COMPOUND_EXPR:
      return is_bitfield_expr_with_lowered_type (TREE_OPERAND (exp, 1));

    case MODIFY_EXPR:
    case SAVE_EXPR:
      return is_bitfield_expr_with_lowered_type (TREE_OPERAND (exp, 0));

    case COMPONENT_REF:
      {
	tree field;
	
	field = TREE_OPERAND (exp, 1);
	if (TREE_CODE (field) != FIELD_DECL || !DECL_BIT_FIELD_TYPE (field))
	  return NULL_TREE;
	if (same_type_ignoring_top_level_qualifiers_p
	    (TREE_TYPE (exp), DECL_BIT_FIELD_TYPE (field)))
	  return NULL_TREE;
	return DECL_BIT_FIELD_TYPE (field);
      }

    CASE_CONVERT:
      if (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_OPERAND (exp, 0)))
	  == TYPE_MAIN_VARIANT (TREE_TYPE (exp)))
	return is_bitfield_expr_with_lowered_type (TREE_OPERAND (exp, 0));
      /* Fallthrough.  */

    default:
      return NULL_TREE;
    }
}

/* Like is_bitfield_with_lowered_type, except that if EXP is not a
   bitfield with a lowered type, the type of EXP is returned, rather
   than NULL_TREE.  */

tree
unlowered_expr_type (const_tree exp)
{
  tree type;
  tree etype = TREE_TYPE (exp);

  type = is_bitfield_expr_with_lowered_type (exp);
  if (type)
    type = cp_build_qualified_type (type, cp_type_quals (etype));
  else
    type = etype;

  return type;
}

/* Perform the conversions in [expr] that apply when an lvalue appears
   in an rvalue context: the lvalue-to-rvalue, array-to-pointer, and
   function-to-pointer conversions.  In addition, manifest constants
   are replaced by their values, and bitfield references are converted
   to their declared types. Note that this function does not perform the
   lvalue-to-rvalue conversion for class types. If you need that conversion
   to for class types, then you probably need to use force_rvalue.

   Although the returned value is being used as an rvalue, this
   function does not wrap the returned expression in a
   NON_LVALUE_EXPR; the caller is expected to be mindful of the fact
   that the return value is no longer an lvalue.  */

tree
decay_conversion (tree exp, tsubst_flags_t complain)
{
  tree type;
  enum tree_code code;
  location_t loc = EXPR_LOC_OR_LOC (exp, input_location);

  type = TREE_TYPE (exp);
  if (type == error_mark_node)
    return error_mark_node;

  exp = mark_rvalue_use (exp);

  exp = resolve_nondeduced_context (exp);
  if (type_unknown_p (exp))
    {
      if (complain & tf_error)
	cxx_incomplete_type_error (exp, TREE_TYPE (exp));
      return error_mark_node;
    }

  code = TREE_CODE (type);

  /* For an array decl decay_conversion should not try to return its
     initializer.  */
  if (code != ARRAY_TYPE)
    {
      /* FIXME remove? at least need to remember that this isn't really a
	 constant expression if EXP isn't decl_constant_var_p, like with
	 C_MAYBE_CONST_EXPR.  */
      exp = decl_constant_value_safe (exp);
      if (error_operand_p (exp))
	return error_mark_node;
    }

  if (NULLPTR_TYPE_P (type) && !TREE_SIDE_EFFECTS (exp))
    return nullptr_node;

  /* build_c_cast puts on a NOP_EXPR to make the result not an lvalue.
     Leave such NOP_EXPRs, since RHS is being used in non-lvalue context.  */
  if (code == VOID_TYPE)
    {
      if (complain & tf_error)
	error_at (loc, "void value not ignored as it ought to be");
      return error_mark_node;
    }
  if (invalid_nonstatic_memfn_p (exp, complain))
    return error_mark_node;
  if (code == FUNCTION_TYPE || is_overloaded_fn (exp))
    return cp_build_addr_expr (exp, complain);
  if (code == ARRAY_TYPE)
    {
      tree adr;
      tree ptrtype;

      if (INDIRECT_REF_P (exp))
	return build_nop (build_pointer_type (TREE_TYPE (type)),
			  TREE_OPERAND (exp, 0));

      if (TREE_CODE (exp) == COMPOUND_EXPR)
	{
	  tree op1 = decay_conversion (TREE_OPERAND (exp, 1), complain);
	  if (op1 == error_mark_node)
            return error_mark_node;
	  return build2 (COMPOUND_EXPR, TREE_TYPE (op1),
			 TREE_OPERAND (exp, 0), op1);
	}

      if (!lvalue_p (exp)
	  && ! (TREE_CODE (exp) == CONSTRUCTOR && TREE_STATIC (exp)))
	{
	  if (complain & tf_error)
	    error_at (loc, "invalid use of non-lvalue array");
	  return error_mark_node;
	}

      /* Don't let an array compound literal decay to a pointer.  It can
	 still be used to initialize an array or bind to a reference.  */
      if (TREE_CODE (exp) == TARGET_EXPR)
	{
	  if (complain & tf_error)
	    error_at (loc, "taking address of temporary array");
	  return error_mark_node;
	}

      ptrtype = build_pointer_type (TREE_TYPE (type));

      if (VAR_P (exp))
	{
	  if (!cxx_mark_addressable (exp))
	    return error_mark_node;
	  adr = build_nop (ptrtype, build_address (exp));
	  return adr;
	}
      /* This way is better for a COMPONENT_REF since it can
	 simplify the offset for a component.  */
      adr = cp_build_addr_expr (exp, complain);
      return cp_convert (ptrtype, adr, complain);
    }

  /* If a bitfield is used in a context where integral promotion
     applies, then the caller is expected to have used
     default_conversion.  That function promotes bitfields correctly
     before calling this function.  At this point, if we have a
     bitfield referenced, we may assume that is not subject to
     promotion, and that, therefore, the type of the resulting rvalue
     is the declared type of the bitfield.  */
  exp = convert_bitfield_to_declared_type (exp);

  /* We do not call rvalue() here because we do not want to wrap EXP
     in a NON_LVALUE_EXPR.  */

  /* [basic.lval]

     Non-class rvalues always have cv-unqualified types.  */
  type = TREE_TYPE (exp);
  if (!CLASS_TYPE_P (type) && cv_qualified_p (type))
    exp = build_nop (cv_unqualified (type), exp);

  return exp;
}

/* Perform preparatory conversions, as part of the "usual arithmetic
   conversions".  In particular, as per [expr]:

     Whenever an lvalue expression appears as an operand of an
     operator that expects the rvalue for that operand, the
     lvalue-to-rvalue, array-to-pointer, or function-to-pointer
     standard conversions are applied to convert the expression to an
     rvalue.

   In addition, we perform integral promotions here, as those are
   applied to both operands to a binary operator before determining
   what additional conversions should apply.  */

static tree
cp_default_conversion (tree exp, tsubst_flags_t complain)
{
  /* Check for target-specific promotions.  */
  tree promoted_type = targetm.promoted_type (TREE_TYPE (exp));
  if (promoted_type)
    exp = cp_convert (promoted_type, exp, complain);
  /* Perform the integral promotions first so that bitfield
     expressions (which may promote to "int", even if the bitfield is
     declared "unsigned") are promoted correctly.  */
  else if (INTEGRAL_OR_UNSCOPED_ENUMERATION_TYPE_P (TREE_TYPE (exp)))
    exp = cp_perform_integral_promotions (exp, complain);
  /* Perform the other conversions.  */
  exp = decay_conversion (exp, complain);

  return exp;
}

/* C version.  */

tree
default_conversion (tree exp)
{
  return cp_default_conversion (exp, tf_warning_or_error);
}

/* EXPR is an expression with an integral or enumeration type.
   Perform the integral promotions in [conv.prom], and return the
   converted value.  */

tree
cp_perform_integral_promotions (tree expr, tsubst_flags_t complain)
{
  tree type;
  tree promoted_type;

  expr = mark_rvalue_use (expr);

  /* [conv.prom]

     If the bitfield has an enumerated type, it is treated as any
     other value of that type for promotion purposes.  */
  type = is_bitfield_expr_with_lowered_type (expr);
  if (!type || TREE_CODE (type) != ENUMERAL_TYPE)
    type = TREE_TYPE (expr);
  gcc_assert (INTEGRAL_OR_ENUMERATION_TYPE_P (type));
  /* Scoped enums don't promote.  */
  if (SCOPED_ENUM_P (type))
    return expr;
  promoted_type = type_promotes_to (type);
  if (type != promoted_type)
    expr = cp_convert (promoted_type, expr, complain);
  return expr;
}

/* C version.  */

tree
perform_integral_promotions (tree expr)
{
  return cp_perform_integral_promotions (expr, tf_warning_or_error);
}

/* Returns nonzero iff exp is a STRING_CST or the result of applying
   decay_conversion to one.  */

int
string_conv_p (const_tree totype, const_tree exp, int warn)
{
  tree t;

  if (!TYPE_PTR_P (totype))
    return 0;

  t = TREE_TYPE (totype);
  if (!same_type_p (t, char_type_node)
      && !same_type_p (t, char16_type_node)
      && !same_type_p (t, char32_type_node)
      && !same_type_p (t, wchar_type_node))
    return 0;

  if (TREE_CODE (exp) == STRING_CST)
    {
      /* Make sure that we don't try to convert between char and wide chars.  */
      if (!same_type_p (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (exp))), t))
	return 0;
    }
  else
    {
      /* Is this a string constant which has decayed to 'const char *'?  */
      t = build_pointer_type (cp_build_qualified_type (t, TYPE_QUAL_CONST));
      if (!same_type_p (TREE_TYPE (exp), t))
	return 0;
      STRIP_NOPS (exp);
      if (TREE_CODE (exp) != ADDR_EXPR
	  || TREE_CODE (TREE_OPERAND (exp, 0)) != STRING_CST)
	return 0;
    }

  /* This warning is not very useful, as it complains about printf.  */
  if (warn)
    warning (OPT_Wwrite_strings,
	     "deprecated conversion from string constant to %qT",
	     totype);

  return 1;
}

/* Given a COND_EXPR, MIN_EXPR, or MAX_EXPR in T, return it in a form that we
   can, for example, use as an lvalue.  This code used to be in
   unary_complex_lvalue, but we needed it to deal with `a = (d == c) ? b : c'
   expressions, where we're dealing with aggregates.  But now it's again only
   called from unary_complex_lvalue.  The case (in particular) that led to
   this was with CODE == ADDR_EXPR, since it's not an lvalue when we'd
   get it there.  */

static tree
rationalize_conditional_expr (enum tree_code code, tree t,
                              tsubst_flags_t complain)
{
  location_t loc = EXPR_LOC_OR_LOC (t, input_location);

  /* For MIN_EXPR or MAX_EXPR, fold-const.c has arranged things so that
     the first operand is always the one to be used if both operands
     are equal, so we know what conditional expression this used to be.  */
  if (TREE_CODE (t) == MIN_EXPR || TREE_CODE (t) == MAX_EXPR)
    {
      tree op0 = TREE_OPERAND (t, 0);
      tree op1 = TREE_OPERAND (t, 1);

      /* The following code is incorrect if either operand side-effects.  */
      gcc_assert (!TREE_SIDE_EFFECTS (op0)
		  && !TREE_SIDE_EFFECTS (op1));
      return
	build_conditional_expr (loc,
				build_x_binary_op (loc,
						   (TREE_CODE (t) == MIN_EXPR
						    ? LE_EXPR : GE_EXPR),
						   op0, TREE_CODE (op0),
						   op1, TREE_CODE (op1),
						   /*overload=*/NULL,
						   complain),
                                cp_build_unary_op (code, op0, 0, complain),
                                cp_build_unary_op (code, op1, 0, complain),
                                complain);
    }

  return
    build_conditional_expr (loc, TREE_OPERAND (t, 0),
			    cp_build_unary_op (code, TREE_OPERAND (t, 1), 0,
                                               complain),
			    cp_build_unary_op (code, TREE_OPERAND (t, 2), 0,
                                               complain),
                            complain);
}

/* Given the TYPE of an anonymous union field inside T, return the
   FIELD_DECL for the field.  If not found return NULL_TREE.  Because
   anonymous unions can nest, we must also search all anonymous unions
   that are directly reachable.  */

tree
lookup_anon_field (tree t, tree type)
{
  tree field;

  for (field = TYPE_FIELDS (t); field; field = DECL_CHAIN (field))
    {
      if (TREE_STATIC (field))
	continue;
      if (TREE_CODE (field) != FIELD_DECL || DECL_ARTIFICIAL (field))
	continue;

      /* If we find it directly, return the field.  */
      if (DECL_NAME (field) == NULL_TREE
	  && type == TYPE_MAIN_VARIANT (TREE_TYPE (field)))
	{
	  return field;
	}

      /* Otherwise, it could be nested, search harder.  */
      if (DECL_NAME (field) == NULL_TREE
	  && ANON_AGGR_TYPE_P (TREE_TYPE (field)))
	{
	  tree subfield = lookup_anon_field (TREE_TYPE (field), type);
	  if (subfield)
	    return subfield;
	}
    }
  return NULL_TREE;
}

/* Build an expression representing OBJECT.MEMBER.  OBJECT is an
   expression; MEMBER is a DECL or baselink.  If ACCESS_PATH is
   non-NULL, it indicates the path to the base used to name MEMBER.
   If PRESERVE_REFERENCE is true, the expression returned will have
   REFERENCE_TYPE if the MEMBER does.  Otherwise, the expression
   returned will have the type referred to by the reference.

   This function does not perform access control; that is either done
   earlier by the parser when the name of MEMBER is resolved to MEMBER
   itself, or later when overload resolution selects one of the
   functions indicated by MEMBER.  */

tree
build_class_member_access_expr (tree object, tree member,
				tree access_path, bool preserve_reference,
				tsubst_flags_t complain)
{
  tree object_type;
  tree member_scope;
  tree result = NULL_TREE;
  tree using_decl = NULL_TREE;

  if (error_operand_p (object) || error_operand_p (member))
    return error_mark_node;

  gcc_assert (DECL_P (member) || BASELINK_P (member));

  /* [expr.ref]

     The type of the first expression shall be "class object" (of a
     complete type).  */
  object_type = TREE_TYPE (object);
  if (!currently_open_class (object_type)
      && !complete_type_or_maybe_complain (object_type, object, complain))
    return error_mark_node;
  if (!CLASS_TYPE_P (object_type))
    {
      if (complain & tf_error)
	{
	  if (POINTER_TYPE_P (object_type)
	      && CLASS_TYPE_P (TREE_TYPE (object_type)))
	    error ("request for member %qD in %qE, which is of pointer "
		   "type %qT (maybe you meant to use %<->%> ?)",
		   member, object, object_type);
	  else
	    error ("request for member %qD in %qE, which is of non-class "
		   "type %qT", member, object, object_type);
	}
      return error_mark_node;
    }

  /* The standard does not seem to actually say that MEMBER must be a
     member of OBJECT_TYPE.  However, that is clearly what is
     intended.  */
  if (DECL_P (member))
    {
      member_scope = DECL_CLASS_CONTEXT (member);
      mark_used (member);
      if (TREE_DEPRECATED (member))
	warn_deprecated_use (member, NULL_TREE);
    }
  else
    member_scope = BINFO_TYPE (BASELINK_ACCESS_BINFO (member));
  /* If MEMBER is from an anonymous aggregate, MEMBER_SCOPE will
     presently be the anonymous union.  Go outwards until we find a
     type related to OBJECT_TYPE.  */
  while ((ANON_AGGR_TYPE_P (member_scope) || UNSCOPED_ENUM_P (member_scope))
	 && !same_type_ignoring_top_level_qualifiers_p (member_scope,
							object_type))
    member_scope = TYPE_CONTEXT (member_scope);
  if (!member_scope || !DERIVED_FROM_P (member_scope, object_type))
    {
      if (complain & tf_error)
	{
	  if (TREE_CODE (member) == FIELD_DECL)
	    error ("invalid use of nonstatic data member %qE", member);
	  else
	    error ("%qD is not a member of %qT", member, object_type);
	}
      return error_mark_node;
    }

  /* Transform `(a, b).x' into `(*(a, &b)).x', `(a ? b : c).x' into
     `(*(a ?  &b : &c)).x', and so on.  A COND_EXPR is only an lvalue
     in the front end; only _DECLs and _REFs are lvalues in the back end.  */
  {
    tree temp = unary_complex_lvalue (ADDR_EXPR, object);
    if (temp)
      object = cp_build_indirect_ref (temp, RO_NULL, complain);
  }

  /* In [expr.ref], there is an explicit list of the valid choices for
     MEMBER.  We check for each of those cases here.  */
  if (VAR_P (member))
    {
      /* A static data member.  */
      result = member;
      mark_exp_read (object);
      /* If OBJECT has side-effects, they are supposed to occur.  */
      if (TREE_SIDE_EFFECTS (object))
	result = build2 (COMPOUND_EXPR, TREE_TYPE (result), object, result);
    }
  else if (TREE_CODE (member) == FIELD_DECL)
    {
      /* A non-static data member.  */
      bool null_object_p;
      int type_quals;
      tree member_type;

      null_object_p = (INDIRECT_REF_P (object)
		       && integer_zerop (TREE_OPERAND (object, 0)));

      /* Convert OBJECT to the type of MEMBER.  */
      if (!same_type_p (TYPE_MAIN_VARIANT (object_type),
			TYPE_MAIN_VARIANT (member_scope)))
	{
	  tree binfo;
	  base_kind kind;

	  binfo = lookup_base (access_path ? access_path : object_type,
			       member_scope, ba_unique, &kind, complain);
	  if (binfo == error_mark_node)
	    return error_mark_node;

	  /* It is invalid to try to get to a virtual base of a
	     NULL object.  The most common cause is invalid use of
	     offsetof macro.  */
	  if (null_object_p && kind == bk_via_virtual)
	    {
	      if (complain & tf_error)
		{
		  error ("invalid access to non-static data member %qD of "
			 "NULL object",
			 member);
		  error ("(perhaps the %<offsetof%> macro was used incorrectly)");
		}
	      return error_mark_node;
	    }

	  /* Convert to the base.  */
	  object = build_base_path (PLUS_EXPR, object, binfo,
				    /*nonnull=*/1, complain);
	  /* If we found the base successfully then we should be able
	     to convert to it successfully.  */
	  gcc_assert (object != error_mark_node);
	}

      /* Complain about other invalid uses of offsetof, even though they will
	 give the right answer.  Note that we complain whether or not they
	 actually used the offsetof macro, since there's no way to know at this
	 point.  So we just give a warning, instead of a pedwarn.  */
      /* Do not produce this warning for base class field references, because
	 we know for a fact that didn't come from offsetof.  This does occur
	 in various testsuite cases where a null object is passed where a
	 vtable access is required.  */
      if (null_object_p && warn_invalid_offsetof
	  && CLASSTYPE_NON_STD_LAYOUT (object_type)
	  && !DECL_FIELD_IS_BASE (member)
	  && cp_unevaluated_operand == 0
	  && (complain & tf_warning))
	{
	  warning (OPT_Winvalid_offsetof, 
                   "invalid access to non-static data member %qD "
                   " of NULL object", member);
	  warning (OPT_Winvalid_offsetof, 
                   "(perhaps the %<offsetof%> macro was used incorrectly)");
	}

      /* If MEMBER is from an anonymous aggregate, we have converted
	 OBJECT so that it refers to the class containing the
	 anonymous union.  Generate a reference to the anonymous union
	 itself, and recur to find MEMBER.  */
      if (ANON_AGGR_TYPE_P (DECL_CONTEXT (member))
	  /* When this code is called from build_field_call, the
	     object already has the type of the anonymous union.
	     That is because the COMPONENT_REF was already
	     constructed, and was then disassembled before calling
	     build_field_call.  After the function-call code is
	     cleaned up, this waste can be eliminated.  */
	  && (!same_type_ignoring_top_level_qualifiers_p
	      (TREE_TYPE (object), DECL_CONTEXT (member))))
	{
	  tree anonymous_union;

	  anonymous_union = lookup_anon_field (TREE_TYPE (object),
					       DECL_CONTEXT (member));
	  object = build_class_member_access_expr (object,
						   anonymous_union,
						   /*access_path=*/NULL_TREE,
						   preserve_reference,
						   complain);
	}

      /* Compute the type of the field, as described in [expr.ref].  */
      type_quals = TYPE_UNQUALIFIED;
      member_type = TREE_TYPE (member);
      if (TREE_CODE (member_type) != REFERENCE_TYPE)
	{
	  type_quals = (cp_type_quals (member_type)
			| cp_type_quals (object_type));

	  /* A field is const (volatile) if the enclosing object, or the
	     field itself, is const (volatile).  But, a mutable field is
	     not const, even within a const object.  */
	  if (DECL_MUTABLE_P (member))
	    type_quals &= ~TYPE_QUAL_CONST;
	  member_type = cp_build_qualified_type (member_type, type_quals);
	}

      result = build3 (COMPONENT_REF, member_type, object, member,
		       NULL_TREE);
      result = fold_if_not_in_template (result);

      /* Mark the expression const or volatile, as appropriate.  Even
	 though we've dealt with the type above, we still have to mark the
	 expression itself.  */
      if (type_quals & TYPE_QUAL_CONST)
	TREE_READONLY (result) = 1;
      if (type_quals & TYPE_QUAL_VOLATILE)
	TREE_THIS_VOLATILE (result) = 1;
    }
  else if (BASELINK_P (member))
    {
      /* The member is a (possibly overloaded) member function.  */
      tree functions;
      tree type;

      /* If the MEMBER is exactly one static member function, then we
	 know the type of the expression.  Otherwise, we must wait
	 until overload resolution has been performed.  */
      functions = BASELINK_FUNCTIONS (member);
      if (TREE_CODE (functions) == FUNCTION_DECL
	  && DECL_STATIC_FUNCTION_P (functions))
	type = TREE_TYPE (functions);
      else
	type = unknown_type_node;
      /* Note that we do not convert OBJECT to the BASELINK_BINFO
	 base.  That will happen when the function is called.  */
      result = build3 (COMPONENT_REF, type, object, member, NULL_TREE);
    }
  else if (TREE_CODE (member) == CONST_DECL)
    {
      /* The member is an enumerator.  */
      result = member;
      /* If OBJECT has side-effects, they are supposed to occur.  */
      if (TREE_SIDE_EFFECTS (object))
	result = build2 (COMPOUND_EXPR, TREE_TYPE (result),
			 object, result);
    }
  else if ((using_decl = strip_using_decl (member)) != member)
    result = build_class_member_access_expr (object,
					     using_decl,
					     access_path, preserve_reference,
					     complain);
  else
    {
      if (complain & tf_error)
	error ("invalid use of %qD", member);
      return error_mark_node;
    }

  if (!preserve_reference)
    /* [expr.ref]

       If E2 is declared to have type "reference to T", then ... the
       type of E1.E2 is T.  */
    result = convert_from_reference (result);

  return result;
}

/* Return the destructor denoted by OBJECT.SCOPE::DTOR_NAME, or, if
   SCOPE is NULL, by OBJECT.DTOR_NAME, where DTOR_NAME is ~type.  */

static tree
lookup_destructor (tree object, tree scope, tree dtor_name,
		   tsubst_flags_t complain)
{
  tree object_type = TREE_TYPE (object);
  tree dtor_type = TREE_OPERAND (dtor_name, 0);
  tree expr;

  /* We've already complained about this destructor.  */
  if (dtor_type == error_mark_node)
    return error_mark_node;

  if (scope && !check_dtor_name (scope, dtor_type))
    {
      if (complain & tf_error)
	error ("qualified type %qT does not match destructor name ~%qT",
	       scope, dtor_type);
      return error_mark_node;
    }
  if (is_auto (dtor_type))
    dtor_type = object_type;
  else if (identifier_p (dtor_type))
    {
      /* In a template, names we can't find a match for are still accepted
	 destructor names, and we check them here.  */
      if (check_dtor_name (object_type, dtor_type))
	dtor_type = object_type;
      else
	{
	  if (complain & tf_error)
	    error ("object type %qT does not match destructor name ~%qT",
		   object_type, dtor_type);
	  return error_mark_node;
	}
      
    }
  else if (!DERIVED_FROM_P (dtor_type, TYPE_MAIN_VARIANT (object_type)))
    {
      if (complain & tf_error)
	error ("the type being destroyed is %qT, but the destructor "
	       "refers to %qT", TYPE_MAIN_VARIANT (object_type), dtor_type);
      return error_mark_node;
    }
  expr = lookup_member (dtor_type, complete_dtor_identifier,
			/*protect=*/1, /*want_type=*/false,
			tf_warning_or_error);
  expr = (adjust_result_of_qualified_name_lookup
	  (expr, dtor_type, object_type));
  if (scope == NULL_TREE)
    /* We need to call adjust_result_of_qualified_name_lookup in case the
       destructor names a base class, but we unset BASELINK_QUALIFIED_P so
       that we still get virtual function binding.  */
    BASELINK_QUALIFIED_P (expr) = false;
  return expr;
}

/* An expression of the form "A::template B" has been resolved to
   DECL.  Issue a diagnostic if B is not a template or template
   specialization.  */

void
check_template_keyword (tree decl)
{
  /* The standard says:

      [temp.names]

      If a name prefixed by the keyword template is not a member
      template, the program is ill-formed.

     DR 228 removed the restriction that the template be a member
     template.

     DR 96, if accepted would add the further restriction that explicit
     template arguments must be provided if the template keyword is
     used, but, as of 2005-10-16, that DR is still in "drafting".  If
     this DR is accepted, then the semantic checks here can be
     simplified, as the entity named must in fact be a template
     specialization, rather than, as at present, a set of overloaded
     functions containing at least one template function.  */
  if (TREE_CODE (decl) != TEMPLATE_DECL
      && TREE_CODE (decl) != TEMPLATE_ID_EXPR)
    {
      if (!is_overloaded_fn (decl))
	permerror (input_location, "%qD is not a template", decl);
      else
	{
	  tree fns;
	  fns = decl;
	  if (BASELINK_P (fns))
	    fns = BASELINK_FUNCTIONS (fns);
	  while (fns)
	    {
	      tree fn = OVL_CURRENT (fns);
	      if (TREE_CODE (fn) == TEMPLATE_DECL
		  || TREE_CODE (fn) == TEMPLATE_ID_EXPR)
		break;
	      if (TREE_CODE (fn) == FUNCTION_DECL
		  && DECL_USE_TEMPLATE (fn)
		  && PRIMARY_TEMPLATE_P (DECL_TI_TEMPLATE (fn)))
		break;
	      fns = OVL_NEXT (fns);
	    }
	  if (!fns)
	    permerror (input_location, "%qD is not a template", decl);
	}
    }
}

/* This function is called by the parser to process a class member
   access expression of the form OBJECT.NAME.  NAME is a node used by
   the parser to represent a name; it is not yet a DECL.  It may,
   however, be a BASELINK where the BASELINK_FUNCTIONS is a
   TEMPLATE_ID_EXPR.  Templates must be looked up by the parser, and
   there is no reason to do the lookup twice, so the parser keeps the
   BASELINK.  TEMPLATE_P is true iff NAME was explicitly declared to
   be a template via the use of the "A::template B" syntax.  */

tree
finish_class_member_access_expr (tree object, tree name, bool template_p,
				 tsubst_flags_t complain)
{
  tree expr;
  tree object_type;
  tree member;
  tree access_path = NULL_TREE;
  tree orig_object = object;
  tree orig_name = name;

  if (object == error_mark_node || name == error_mark_node)
    return error_mark_node;

  /* If OBJECT is an ObjC class instance, we must obey ObjC access rules.  */
  if (!objc_is_public (object, name))
    return error_mark_node;

  object_type = TREE_TYPE (object);

  if (processing_template_decl)
    {
      if (/* If OBJECT_TYPE is dependent, so is OBJECT.NAME.  */
	  dependent_type_p (object_type)
	  /* If NAME is just an IDENTIFIER_NODE, then the expression
	     is dependent.  */
	  || identifier_p (object)
	  /* If NAME is "f<args>", where either 'f' or 'args' is
	     dependent, then the expression is dependent.  */
	  || (TREE_CODE (name) == TEMPLATE_ID_EXPR
	      && dependent_template_id_p (TREE_OPERAND (name, 0),
					  TREE_OPERAND (name, 1)))
	  /* If NAME is "T::X" where "T" is dependent, then the
	     expression is dependent.  */
	  || (TREE_CODE (name) == SCOPE_REF
	      && TYPE_P (TREE_OPERAND (name, 0))
	      && dependent_type_p (TREE_OPERAND (name, 0))))
	return build_min_nt_loc (UNKNOWN_LOCATION, COMPONENT_REF,
				 object, name, NULL_TREE);
      object = build_non_dependent_expr (object);
    }
  else if (c_dialect_objc ()
	   && identifier_p (name)
	   && (expr = objc_maybe_build_component_ref (object, name)))
    return expr;
    
  /* [expr.ref]

     The type of the first expression shall be "class object" (of a
     complete type).  */
  if (!currently_open_class (object_type)
      && !complete_type_or_maybe_complain (object_type, object, complain))
    return error_mark_node;
  if (!CLASS_TYPE_P (object_type))
    {
      if (complain & tf_error)
	{
	  if (POINTER_TYPE_P (object_type)
	      && CLASS_TYPE_P (TREE_TYPE (object_type)))
	    error ("request for member %qD in %qE, which is of pointer "
		   "type %qT (maybe you meant to use %<->%> ?)",
		   name, object, object_type);
	  else
	    error ("request for member %qD in %qE, which is of non-class "
		   "type %qT", name, object, object_type);
	}
      return error_mark_node;
    }

  if (BASELINK_P (name))
    /* A member function that has already been looked up.  */
    member = name;
  else
    {
      bool is_template_id = false;
      tree template_args = NULL_TREE;
      tree scope;

      if (TREE_CODE (name) == TEMPLATE_ID_EXPR)
	{
	  is_template_id = true;
	  template_args = TREE_OPERAND (name, 1);
	  name = TREE_OPERAND (name, 0);

	  if (TREE_CODE (name) == OVERLOAD)
	    name = DECL_NAME (get_first_fn (name));
	  else if (DECL_P (name))
	    name = DECL_NAME (name);
	}

      if (TREE_CODE (name) == SCOPE_REF)
	{
	  /* A qualified name.  The qualifying class or namespace `S'
	     has already been looked up; it is either a TYPE or a
	     NAMESPACE_DECL.  */
	  scope = TREE_OPERAND (name, 0);
	  name = TREE_OPERAND (name, 1);

	  /* If SCOPE is a namespace, then the qualified name does not
	     name a member of OBJECT_TYPE.  */
	  if (TREE_CODE (scope) == NAMESPACE_DECL)
	    {
	      if (complain & tf_error)
		error ("%<%D::%D%> is not a member of %qT",
		       scope, name, object_type);
	      return error_mark_node;
	    }

	  if (TREE_CODE (scope) == ENUMERAL_TYPE)
	    {
	      /* Looking up a member enumerator (c++/56793).  */
	      if (!TYPE_CLASS_SCOPE_P (scope)
		  || !DERIVED_FROM_P (TYPE_CONTEXT (scope), object_type))
		{
		  if (complain & tf_error)
		    error ("%<%D::%D%> is not a member of %qT",
			   scope, name, object_type);
		  return error_mark_node;
		}
	      tree val = lookup_enumerator (scope, name);
	      if (TREE_SIDE_EFFECTS (object))
		val = build2 (COMPOUND_EXPR, TREE_TYPE (val), object, val);
	      return val;
	    }

	  gcc_assert (CLASS_TYPE_P (scope));
	  gcc_assert (identifier_p (name) || TREE_CODE (name) == BIT_NOT_EXPR);

	  if (constructor_name_p (name, scope))
	    {
	      if (complain & tf_error)
		error ("cannot call constructor %<%T::%D%> directly",
		       scope, name);
	      return error_mark_node;
	    }

	  /* Find the base of OBJECT_TYPE corresponding to SCOPE.  */
	  access_path = lookup_base (object_type, scope, ba_check,
				     NULL, complain);
	  if (access_path == error_mark_node)
	    return error_mark_node;
	  if (!access_path)
	    {
	      if (complain & tf_error)
		error ("%qT is not a base of %qT", scope, object_type);
	      return error_mark_node;
	    }
	}
      else
	{
	  scope = NULL_TREE;
	  access_path = object_type;
	}

      if (TREE_CODE (name) == BIT_NOT_EXPR)
	member = lookup_destructor (object, scope, name, complain);
      else
	{
	  /* Look up the member.  */
	  member = lookup_member (access_path, name, /*protect=*/1,
				  /*want_type=*/false, complain);
	  if (member == NULL_TREE)
	    {
	      if (complain & tf_error)
		error ("%qD has no member named %qE",
		       TREE_CODE (access_path) == TREE_BINFO
		       ? TREE_TYPE (access_path) : object_type, name);
	      return error_mark_node;
	    }
	  if (member == error_mark_node)
	    return error_mark_node;
	}

      if (is_template_id)
	{
	  tree templ = member;

	  if (BASELINK_P (templ))
	    templ = lookup_template_function (templ, template_args);
	  else
	    {
	      if (complain & tf_error)
		error ("%qD is not a member template function", name);
	      return error_mark_node;
	    }
	}
    }

  if (TREE_DEPRECATED (member))
    warn_deprecated_use (member, NULL_TREE);

  if (template_p)
    check_template_keyword (member);

  expr = build_class_member_access_expr (object, member, access_path,
					 /*preserve_reference=*/false,
					 complain);
  if (processing_template_decl && expr != error_mark_node)
    {
      if (BASELINK_P (member))
	{
	  if (TREE_CODE (orig_name) == SCOPE_REF)
	    BASELINK_QUALIFIED_P (member) = 1;
	  orig_name = member;
	}
      return build_min_non_dep (COMPONENT_REF, expr,
				orig_object, orig_name,
				NULL_TREE);
    }

  return expr;
}

/* Build a COMPONENT_REF of OBJECT and MEMBER with the appropriate
   type.  */

tree
build_simple_component_ref (tree object, tree member)
{
  tree type = cp_build_qualified_type (TREE_TYPE (member),
				       cp_type_quals (TREE_TYPE (object)));
  return fold_build3_loc (input_location,
			  COMPONENT_REF, type,
			  object, member, NULL_TREE);
}

/* Return an expression for the MEMBER_NAME field in the internal
   representation of PTRMEM, a pointer-to-member function.  (Each
   pointer-to-member function type gets its own RECORD_TYPE so it is
   more convenient to access the fields by name than by FIELD_DECL.)
   This routine converts the NAME to a FIELD_DECL and then creates the
   node for the complete expression.  */

tree
build_ptrmemfunc_access_expr (tree ptrmem, tree member_name)
{
  tree ptrmem_type;
  tree member;

  /* This code is a stripped down version of
     build_class_member_access_expr.  It does not work to use that
     routine directly because it expects the object to be of class
     type.  */
  ptrmem_type = TREE_TYPE (ptrmem);
  gcc_assert (TYPE_PTRMEMFUNC_P (ptrmem_type));
  member = lookup_member (ptrmem_type, member_name, /*protect=*/0,
			  /*want_type=*/false, tf_warning_or_error);
  return build_simple_component_ref (ptrmem, member);
}

/* Given an expression PTR for a pointer, return an expression
   for the value pointed to.
   ERRORSTRING is the name of the operator to appear in error messages.

   This function may need to overload OPERATOR_FNNAME.
   Must also handle REFERENCE_TYPEs for C++.  */

tree
build_x_indirect_ref (location_t loc, tree expr, ref_operator errorstring, 
                      tsubst_flags_t complain)
{
  tree orig_expr = expr;
  tree rval;

  if (processing_template_decl)
    {
      /* Retain the type if we know the operand is a pointer.  */
      if (TREE_TYPE (expr) && POINTER_TYPE_P (TREE_TYPE (expr)))
	return build_min (INDIRECT_REF, TREE_TYPE (TREE_TYPE (expr)), expr);
      if (type_dependent_expression_p (expr))
	return build_min_nt_loc (loc, INDIRECT_REF, expr);
      expr = build_non_dependent_expr (expr);
    }

  rval = build_new_op (loc, INDIRECT_REF, LOOKUP_NORMAL, expr,
		       NULL_TREE, NULL_TREE, /*overload=*/NULL, complain);
  if (!rval)
    rval = cp_build_indirect_ref (expr, errorstring, complain);

  if (processing_template_decl && rval != error_mark_node)
    return build_min_non_dep (INDIRECT_REF, rval, orig_expr);
  else
    return rval;
}

/* Helper function called from c-common.  */
tree
build_indirect_ref (location_t /*loc*/,
		    tree ptr, ref_operator errorstring)
{
  return cp_build_indirect_ref (ptr, errorstring, tf_warning_or_error);
}

tree
cp_build_indirect_ref (tree ptr, ref_operator errorstring, 
                       tsubst_flags_t complain)
{
  tree pointer, type;

  if (ptr == current_class_ptr
      || (TREE_CODE (ptr) == NOP_EXPR
	  && TREE_OPERAND (ptr, 0) == current_class_ptr
	  && (same_type_ignoring_top_level_qualifiers_p
	      (TREE_TYPE (ptr), TREE_TYPE (current_class_ptr)))))
    return current_class_ref;

  pointer = (TREE_CODE (TREE_TYPE (ptr)) == REFERENCE_TYPE
	     ? ptr : decay_conversion (ptr, complain));
  if (pointer == error_mark_node)
    return error_mark_node;

  type = TREE_TYPE (pointer);

  if (POINTER_TYPE_P (type))
    {
      /* [expr.unary.op]

	 If the type of the expression is "pointer to T," the type
	 of  the  result  is  "T."  */
      tree t = TREE_TYPE (type);

      if (CONVERT_EXPR_P (ptr)
          || TREE_CODE (ptr) == VIEW_CONVERT_EXPR)
	{
	  /* If a warning is issued, mark it to avoid duplicates from
	     the backend.  This only needs to be done at
	     warn_strict_aliasing > 2.  */
	  if (warn_strict_aliasing > 2)
	    if (strict_aliasing_warning (TREE_TYPE (TREE_OPERAND (ptr, 0)),
					 type, TREE_OPERAND (ptr, 0)))
	      TREE_NO_WARNING (ptr) = 1;
	}

      if (VOID_TYPE_P (t))
	{
	  /* A pointer to incomplete type (other than cv void) can be
	     dereferenced [expr.unary.op]/1  */
          if (complain & tf_error)
            error ("%qT is not a pointer-to-object type", type);
	  return error_mark_node;
	}
      else if (TREE_CODE (pointer) == ADDR_EXPR
	       && same_type_p (t, TREE_TYPE (TREE_OPERAND (pointer, 0))))
	/* The POINTER was something like `&x'.  We simplify `*&x' to
	   `x'.  */
	return TREE_OPERAND (pointer, 0);
      else
	{
	  tree ref = build1 (INDIRECT_REF, t, pointer);

	  /* We *must* set TREE_READONLY when dereferencing a pointer to const,
	     so that we get the proper error message if the result is used
	     to assign to.  Also, &* is supposed to be a no-op.  */
	  TREE_READONLY (ref) = CP_TYPE_CONST_P (t);
	  TREE_THIS_VOLATILE (ref) = CP_TYPE_VOLATILE_P (t);
	  TREE_SIDE_EFFECTS (ref)
	    = (TREE_THIS_VOLATILE (ref) || TREE_SIDE_EFFECTS (pointer));
	  return ref;
	}
    }
  else if (!(complain & tf_error))
    /* Don't emit any errors; we'll just return ERROR_MARK_NODE later.  */
    ;
  /* `pointer' won't be an error_mark_node if we were given a
     pointer to member, so it's cool to check for this here.  */
  else if (TYPE_PTRMEM_P (type))
    switch (errorstring)
      {
         case RO_ARRAY_INDEXING:
           error ("invalid use of array indexing on pointer to member");
           break;
         case RO_UNARY_STAR:
           error ("invalid use of unary %<*%> on pointer to member");
           break;
         case RO_IMPLICIT_CONVERSION:
           error ("invalid use of implicit conversion on pointer to member");
           break;
         case RO_ARROW_STAR:
           error ("left hand operand of %<->*%> must be a pointer to class, "
		  "but is a pointer to member of type %qT", type);
           break;
         default:
           gcc_unreachable ();
      }
  else if (pointer != error_mark_node)
    invalid_indirection_error (input_location, type, errorstring);

  return error_mark_node;
}

/* This handles expressions of the form "a[i]", which denotes
   an array reference.

   This is logically equivalent in C to *(a+i), but we may do it differently.
   If A is a variable or a member, we generate a primitive ARRAY_REF.
   This avoids forcing the array out of registers, and can work on
   arrays that are not lvalues (for example, members of structures returned
   by functions).

   If INDEX is of some user-defined type, it must be converted to
   integer type.  Otherwise, to make a compatible PLUS_EXPR, it
   will inherit the type of the array, which will be some pointer type.
   
   LOC is the location to use in building the array reference.  */

tree
cp_build_array_ref (location_t loc, tree array, tree idx,
		    tsubst_flags_t complain)
{
  tree ret;

  if (idx == 0)
    {
      if (complain & tf_error)
	error_at (loc, "subscript missing in array reference");
      return error_mark_node;
    }

  /* If an array's index is an array notation, then its rank cannot be
     greater than one.  */ 
  if (flag_cilkplus && contains_array_notation_expr (idx))
    {
      size_t rank = 0;

      /* If find_rank returns false, then it should have reported an error,
	 thus it is unnecessary for repetition.  */
      if (!find_rank (loc, idx, idx, true, &rank))
	return error_mark_node;
      if (rank > 1)
	{
	  error_at (loc, "rank of the array%'s index is greater than 1");
	  return error_mark_node;
	}
    }
  if (TREE_TYPE (array) == error_mark_node
      || TREE_TYPE (idx) == error_mark_node)
    return error_mark_node;

  /* If ARRAY is a COMPOUND_EXPR or COND_EXPR, move our reference
     inside it.  */
  switch (TREE_CODE (array))
    {
    case COMPOUND_EXPR:
      {
	tree value = cp_build_array_ref (loc, TREE_OPERAND (array, 1), idx,
					 complain);
	ret = build2 (COMPOUND_EXPR, TREE_TYPE (value),
		      TREE_OPERAND (array, 0), value);
	SET_EXPR_LOCATION (ret, loc);
	return ret;
      }

    case COND_EXPR:
      ret = build_conditional_expr
	       (loc, TREE_OPERAND (array, 0),
	       cp_build_array_ref (loc, TREE_OPERAND (array, 1), idx,
				   complain),
	       cp_build_array_ref (loc, TREE_OPERAND (array, 2), idx,
				   complain),
	       complain);
      protected_set_expr_location (ret, loc);
      return ret;

    default:
      break;
    }

  convert_vector_to_pointer_for_subscript (loc, &array, idx);

  if (TREE_CODE (TREE_TYPE (array)) == ARRAY_TYPE)
    {
      tree rval, type;

      warn_array_subscript_with_type_char (idx);

      if (!INTEGRAL_OR_UNSCOPED_ENUMERATION_TYPE_P (TREE_TYPE (idx)))
	{
	  if (complain & tf_error)
	    error_at (loc, "array subscript is not an integer");
	  return error_mark_node;
	}

      /* Apply integral promotions *after* noticing character types.
	 (It is unclear why we do these promotions -- the standard
	 does not say that we should.  In fact, the natural thing would
	 seem to be to convert IDX to ptrdiff_t; we're performing
	 pointer arithmetic.)  */
      idx = cp_perform_integral_promotions (idx, complain);

      /* An array that is indexed by a non-constant
	 cannot be stored in a register; we must be able to do
	 address arithmetic on its address.
	 Likewise an array of elements of variable size.  */
      if (TREE_CODE (idx) != INTEGER_CST
	  || (COMPLETE_TYPE_P (TREE_TYPE (TREE_TYPE (array)))
	      && (TREE_CODE (TYPE_SIZE (TREE_TYPE (TREE_TYPE (array))))
		  != INTEGER_CST)))
	{
	  if (!cxx_mark_addressable (array))
	    return error_mark_node;
	}

      /* An array that is indexed by a constant value which is not within
	 the array bounds cannot be stored in a register either; because we
	 would get a crash in store_bit_field/extract_bit_field when trying
	 to access a non-existent part of the register.  */
      if (TREE_CODE (idx) == INTEGER_CST
	  && TYPE_DOMAIN (TREE_TYPE (array))
	  && ! int_fits_type_p (idx, TYPE_DOMAIN (TREE_TYPE (array))))
	{
	  if (!cxx_mark_addressable (array))
	    return error_mark_node;
	}

      if (!lvalue_p (array) && (complain & tf_error))
	pedwarn (loc, OPT_Wpedantic, 
	         "ISO C++ forbids subscripting non-lvalue array");

      /* Note in C++ it is valid to subscript a `register' array, since
	 it is valid to take the address of something with that
	 storage specification.  */
      if (extra_warnings)
	{
	  tree foo = array;
	  while (TREE_CODE (foo) == COMPONENT_REF)
	    foo = TREE_OPERAND (foo, 0);
	  if (VAR_P (foo) && DECL_REGISTER (foo)
	      && (complain & tf_warning))
	    warning_at (loc, OPT_Wextra,
			"subscripting array declared %<register%>");
	}

      type = TREE_TYPE (TREE_TYPE (array));
      rval = build4 (ARRAY_REF, type, array, idx, NULL_TREE, NULL_TREE);
      /* Array ref is const/volatile if the array elements are
	 or if the array is..  */
      TREE_READONLY (rval)
	|= (CP_TYPE_CONST_P (type) | TREE_READONLY (array));
      TREE_SIDE_EFFECTS (rval)
	|= (CP_TYPE_VOLATILE_P (type) | TREE_SIDE_EFFECTS (array));
      TREE_THIS_VOLATILE (rval)
	|= (CP_TYPE_VOLATILE_P (type) | TREE_THIS_VOLATILE (array));
      ret = require_complete_type_sfinae (fold_if_not_in_template (rval),
					  complain);
      protected_set_expr_location (ret, loc);
      return ret;
    }

  {
    tree ar = cp_default_conversion (array, complain);
    tree ind = cp_default_conversion (idx, complain);

    /* Put the integer in IND to simplify error checking.  */
    if (TREE_CODE (TREE_TYPE (ar)) == INTEGER_TYPE)
      {
	tree temp = ar;
	ar = ind;
	ind = temp;
      }

    if (ar == error_mark_node || ind == error_mark_node)
      return error_mark_node;

    if (!TYPE_PTR_P (TREE_TYPE (ar)))
      {
	if (complain & tf_error)
	  error_at (loc, "subscripted value is neither array nor pointer");
	return error_mark_node;
      }
    if (TREE_CODE (TREE_TYPE (ind)) != INTEGER_TYPE)
      {
	if (complain & tf_error)
	  error_at (loc, "array subscript is not an integer");
	return error_mark_node;
      }

    warn_array_subscript_with_type_char (idx);

    ret = cp_build_indirect_ref (cp_build_binary_op (input_location,
						     PLUS_EXPR, ar, ind,
						     complain),
                                 RO_ARRAY_INDEXING,
                                 complain);
    protected_set_expr_location (ret, loc);
    return ret;
  }
}

/* Entry point for Obj-C++.  */

tree
build_array_ref (location_t loc, tree array, tree idx)
{
  return cp_build_array_ref (loc, array, idx, tf_warning_or_error);
}

/* Resolve a pointer to member function.  INSTANCE is the object
   instance to use, if the member points to a virtual member.

   This used to avoid checking for virtual functions if basetype
   has no virtual functions, according to an earlier ANSI draft.
   With the final ISO C++ rules, such an optimization is
   incorrect: A pointer to a derived member can be static_cast
   to pointer-to-base-member, as long as the dynamic object
   later has the right member.  So now we only do this optimization
   when we know the dynamic type of the object.  */

tree
get_member_function_from_ptrfunc (tree *instance_ptrptr, tree function,
				  tsubst_flags_t complain)
{
  if (TREE_CODE (function) == OFFSET_REF)
    function = TREE_OPERAND (function, 1);

  if (TYPE_PTRMEMFUNC_P (TREE_TYPE (function)))
    {
      tree idx, delta, e1, e2, e3, vtbl;
      bool nonvirtual;
      tree fntype = TYPE_PTRMEMFUNC_FN_TYPE (TREE_TYPE (function));
      tree basetype = TYPE_METHOD_BASETYPE (TREE_TYPE (fntype));

      tree instance_ptr = *instance_ptrptr;
      tree instance_save_expr = 0;
      if (instance_ptr == error_mark_node)
	{
	  if (TREE_CODE (function) == PTRMEM_CST)
	    {
	      /* Extracting the function address from a pmf is only
		 allowed with -Wno-pmf-conversions. It only works for
		 pmf constants.  */
	      e1 = build_addr_func (PTRMEM_CST_MEMBER (function), complain);
	      e1 = convert (fntype, e1);
	      return e1;
	    }
	  else
	    {
	      if (complain & tf_error)
		error ("object missing in use of %qE", function);
	      return error_mark_node;
	    }
	}

      /* True if we know that the dynamic type of the object doesn't have
	 virtual functions, so we can assume the PFN field is a pointer.  */
      nonvirtual = (COMPLETE_TYPE_P (basetype)
		    && !TYPE_POLYMORPHIC_P (basetype)
		    && resolves_to_fixed_type_p (instance_ptr, 0));

      if (TREE_SIDE_EFFECTS (instance_ptr))
	instance_ptr = instance_save_expr = save_expr (instance_ptr);

      if (TREE_SIDE_EFFECTS (function))
	function = save_expr (function);

      /* Start by extracting all the information from the PMF itself.  */
      e3 = pfn_from_ptrmemfunc (function);
      delta = delta_from_ptrmemfunc (function);
      idx = build1 (NOP_EXPR, vtable_index_type, e3);
      switch (TARGET_PTRMEMFUNC_VBIT_LOCATION)
	{
	case ptrmemfunc_vbit_in_pfn:
	  e1 = cp_build_binary_op (input_location,
				   BIT_AND_EXPR, idx, integer_one_node,
				   complain);
	  idx = cp_build_binary_op (input_location,
				    MINUS_EXPR, idx, integer_one_node,
				    complain);
	  if (idx == error_mark_node)
	    return error_mark_node;
	  break;

	case ptrmemfunc_vbit_in_delta:
	  e1 = cp_build_binary_op (input_location,
				   BIT_AND_EXPR, delta, integer_one_node,
				   complain);
	  delta = cp_build_binary_op (input_location,
				      RSHIFT_EXPR, delta, integer_one_node,
				      complain);
	  if (delta == error_mark_node)
	    return error_mark_node;
	  break;

	default:
	  gcc_unreachable ();
	}

      if (e1 == error_mark_node)
	return error_mark_node;

      /* Convert down to the right base before using the instance.  A
	 special case is that in a pointer to member of class C, C may
	 be incomplete.  In that case, the function will of course be
	 a member of C, and no conversion is required.  In fact,
	 lookup_base will fail in that case, because incomplete
	 classes do not have BINFOs.  */
      if (!same_type_ignoring_top_level_qualifiers_p
	  (basetype, TREE_TYPE (TREE_TYPE (instance_ptr))))
	{
	  basetype = lookup_base (TREE_TYPE (TREE_TYPE (instance_ptr)),
				  basetype, ba_check, NULL, complain);
	  instance_ptr = build_base_path (PLUS_EXPR, instance_ptr, basetype,
					  1, complain);
	  if (instance_ptr == error_mark_node)
	    return error_mark_node;
	}
      /* ...and then the delta in the PMF.  */
      instance_ptr = fold_build_pointer_plus (instance_ptr, delta);

      /* Hand back the adjusted 'this' argument to our caller.  */
      *instance_ptrptr = instance_ptr;

      if (nonvirtual)
	/* Now just return the pointer.  */
	return e3;

      /* Next extract the vtable pointer from the object.  */
      vtbl = build1 (NOP_EXPR, build_pointer_type (vtbl_ptr_type_node),
		     instance_ptr);
      vtbl = cp_build_indirect_ref (vtbl, RO_NULL, complain);
      if (vtbl == error_mark_node)
	return error_mark_node;

      /* Finally, extract the function pointer from the vtable.  */
      e2 = fold_build_pointer_plus_loc (input_location, vtbl, idx);
      e2 = cp_build_indirect_ref (e2, RO_NULL, complain);
      if (e2 == error_mark_node)
	return error_mark_node;
      TREE_CONSTANT (e2) = 1;

      /* When using function descriptors, the address of the
	 vtable entry is treated as a function pointer.  */
      if (TARGET_VTABLE_USES_DESCRIPTORS)
	e2 = build1 (NOP_EXPR, TREE_TYPE (e2),
		     cp_build_addr_expr (e2, complain));

      e2 = fold_convert (TREE_TYPE (e3), e2);
      e1 = build_conditional_expr (input_location, e1, e2, e3, complain);
      if (e1 == error_mark_node)
	return error_mark_node;

      /* Make sure this doesn't get evaluated first inside one of the
	 branches of the COND_EXPR.  */
      if (instance_save_expr)
	e1 = build2 (COMPOUND_EXPR, TREE_TYPE (e1),
		     instance_save_expr, e1);

      function = e1;
    }
  return function;
}

/* Used by the C-common bits.  */
tree
build_function_call (location_t /*loc*/, 
		     tree function, tree params)
{
  return cp_build_function_call (function, params, tf_warning_or_error);
}

/* Used by the C-common bits.  */
tree
build_function_call_vec (location_t /*loc*/, vec<location_t> /*arg_loc*/,
			 tree function, vec<tree, va_gc> *params,
			 vec<tree, va_gc> * /*origtypes*/)
{
  vec<tree, va_gc> *orig_params = params;
  tree ret = cp_build_function_call_vec (function, &params,
					 tf_warning_or_error);

  /* cp_build_function_call_vec can reallocate PARAMS by adding
     default arguments.  That should never happen here.  Verify
     that.  */
  gcc_assert (params == orig_params);

  return ret;
}

/* Build a function call using a tree list of arguments.  */

tree
cp_build_function_call (tree function, tree params, tsubst_flags_t complain)
{
  vec<tree, va_gc> *vec;
  tree ret;

  vec = make_tree_vector ();
  for (; params != NULL_TREE; params = TREE_CHAIN (params))
    vec_safe_push (vec, TREE_VALUE (params));
  ret = cp_build_function_call_vec (function, &vec, complain);
  release_tree_vector (vec);
  return ret;
}

/* Build a function call using varargs.  */

tree
cp_build_function_call_nary (tree function, tsubst_flags_t complain, ...)
{
  vec<tree, va_gc> *vec;
  va_list args;
  tree ret, t;

  vec = make_tree_vector ();
  va_start (args, complain);
  for (t = va_arg (args, tree); t != NULL_TREE; t = va_arg (args, tree))
    vec_safe_push (vec, t);
  va_end (args);
  ret = cp_build_function_call_vec (function, &vec, complain);
  release_tree_vector (vec);
  return ret;
}

/* Build a function call using a vector of arguments.  PARAMS may be
   NULL if there are no parameters.  This changes the contents of
   PARAMS.  */

tree
cp_build_function_call_vec (tree function, vec<tree, va_gc> **params,
			    tsubst_flags_t complain)
{
  tree fntype, fndecl;
  int is_method;
  tree original = function;
  int nargs;
  tree *argarray;
  tree parm_types;
  vec<tree, va_gc> *allocated = NULL;
  tree ret;

  /* For Objective-C, convert any calls via a cast to OBJC_TYPE_REF
     expressions, like those used for ObjC messenger dispatches.  */
  if (params != NULL && !vec_safe_is_empty (*params))
    function = objc_rewrite_function_call (function, (**params)[0]);

  /* build_c_cast puts on a NOP_EXPR to make the result not an lvalue.
     Strip such NOP_EXPRs, since FUNCTION is used in non-lvalue context.  */
  if (TREE_CODE (function) == NOP_EXPR
      && TREE_TYPE (function) == TREE_TYPE (TREE_OPERAND (function, 0)))
    function = TREE_OPERAND (function, 0);

  if (TREE_CODE (function) == FUNCTION_DECL)
    {
      mark_used (function);
      fndecl = function;

      /* Convert anything with function type to a pointer-to-function.  */
      if (DECL_MAIN_P (function) && (complain & tf_error))
	pedwarn (input_location, OPT_Wpedantic, 
		 "ISO C++ forbids calling %<::main%> from within program");

      function = build_addr_func (function, complain);
    }
  else
    {
      fndecl = NULL_TREE;

      function = build_addr_func (function, complain);
    }

  if (function == error_mark_node)
    return error_mark_node;

  fntype = TREE_TYPE (function);

  if (TYPE_PTRMEMFUNC_P (fntype))
    {
      if (complain & tf_error)
	error ("must use %<.*%> or %<->*%> to call pointer-to-member "
	       "function in %<%E (...)%>, e.g. %<(... ->* %E) (...)%>",
	       original, original);
      return error_mark_node;
    }

  is_method = (TYPE_PTR_P (fntype)
	       && TREE_CODE (TREE_TYPE (fntype)) == METHOD_TYPE);

  if (!(TYPE_PTRFN_P (fntype)
	|| is_method
	|| TREE_CODE (function) == TEMPLATE_ID_EXPR))
    {
      if (complain & tf_error)
	{
	  if (!flag_diagnostics_show_caret)
	    error_at (input_location,
		      "%qE cannot be used as a function", original);
	  else if (DECL_P (original))
	    error_at (input_location,
		      "%qD cannot be used as a function", original);
	  else 
	    error_at (input_location,
		      "expression cannot be used as a function");
	}

      return error_mark_node;
    }

  /* fntype now gets the type of function pointed to.  */
  fntype = TREE_TYPE (fntype);
  parm_types = TYPE_ARG_TYPES (fntype);

  if (params == NULL)
    {
      allocated = make_tree_vector ();
      params = &allocated;
    }

    nargs = convert_arguments (parm_types, params, fndecl, LOOKUP_NORMAL,
			       complain);
  if (nargs < 0)
    return error_mark_node;

  argarray = (*params)->address ();

  /* Check for errors in format strings and inappropriately
     null parameters.  */
  check_function_arguments (fntype, nargs, argarray);

  ret = build_cxx_call (function, nargs, argarray, complain);

  if (allocated != NULL)
    release_tree_vector (allocated);

  return ret;
}

/* Subroutine of convert_arguments.
   Warn about wrong number of args are genereted. */

static void
warn_args_num (location_t loc, tree fndecl, bool too_many_p)
{
  if (fndecl)
    {
      if (TREE_CODE (TREE_TYPE (fndecl)) == METHOD_TYPE)
	{
	  if (DECL_NAME (fndecl) == NULL_TREE
	      || IDENTIFIER_HAS_TYPE_VALUE (DECL_NAME (fndecl)))
	    error_at (loc,
		      too_many_p
		      ? G_("too many arguments to constructor %q#D")
		      : G_("too few arguments to constructor %q#D"),
		      fndecl);
	  else
	    error_at (loc,
		      too_many_p
		      ? G_("too many arguments to member function %q#D")
		      : G_("too few arguments to member function %q#D"),
		      fndecl);
	}
      else
	error_at (loc,
		  too_many_p
		  ? G_("too many arguments to function %q#D")
		  : G_("too few arguments to function %q#D"),
		  fndecl);
      inform (DECL_SOURCE_LOCATION (fndecl),
	      "declared here");
    }
  else
    {
      if (c_dialect_objc ()  &&  objc_message_selector ())
	error_at (loc,
		  too_many_p 
		  ? G_("too many arguments to method %q#D")
		  : G_("too few arguments to method %q#D"),
		  objc_message_selector ());
      else
	error_at (loc, too_many_p ? G_("too many arguments to function")
		                  : G_("too few arguments to function"));
    }
}

/* Convert the actual parameter expressions in the list VALUES to the
   types in the list TYPELIST.  The converted expressions are stored
   back in the VALUES vector.
   If parmdecls is exhausted, or when an element has NULL as its type,
   perform the default conversions.

   NAME is an IDENTIFIER_NODE or 0.  It is used only for error messages.

   This is also where warnings about wrong number of args are generated.

   Returns the actual number of arguments processed (which might be less
   than the length of the vector), or -1 on error.

   In C++, unspecified trailing parameters can be filled in with their
   default arguments, if such were specified.  Do so here.  */

static int
convert_arguments (tree typelist, vec<tree, va_gc> **values, tree fndecl,
		   int flags, tsubst_flags_t complain)
{
  tree typetail;
  unsigned int i;

  /* Argument passing is always copy-initialization.  */
  flags |= LOOKUP_ONLYCONVERTING;

  for (i = 0, typetail = typelist;
       i < vec_safe_length (*values);
       i++)
    {
      tree type = typetail ? TREE_VALUE (typetail) : 0;
      tree val = (**values)[i];

      if (val == error_mark_node || type == error_mark_node)
	return -1;

      if (type == void_type_node)
	{
          if (complain & tf_error)
            {
	      warn_args_num (input_location, fndecl, /*too_many_p=*/true);
              return i;
            }
          else
            return -1;
	}

      /* build_c_cast puts on a NOP_EXPR to make the result not an lvalue.
	 Strip such NOP_EXPRs, since VAL is used in non-lvalue context.  */
      if (TREE_CODE (val) == NOP_EXPR
	  && TREE_TYPE (val) == TREE_TYPE (TREE_OPERAND (val, 0))
	  && (type == 0 || TREE_CODE (type) != REFERENCE_TYPE))
	val = TREE_OPERAND (val, 0);

      if (type == 0 || TREE_CODE (type) != REFERENCE_TYPE)
	{
	  if (TREE_CODE (TREE_TYPE (val)) == ARRAY_TYPE
	      || TREE_CODE (TREE_TYPE (val)) == FUNCTION_TYPE
	      || TREE_CODE (TREE_TYPE (val)) == METHOD_TYPE)
	    val = decay_conversion (val, complain);
	}

      if (val == error_mark_node)
	return -1;

      if (type != 0)
	{
	  /* Formal parm type is specified by a function prototype.  */
	  tree parmval;

	  if (!COMPLETE_TYPE_P (complete_type (type)))
	    {
              if (complain & tf_error)
                {
                  if (fndecl)
                    error ("parameter %P of %qD has incomplete type %qT",
                           i, fndecl, type);
                  else
                    error ("parameter %P has incomplete type %qT", i, type);
                }
	      parmval = error_mark_node;
	    }
	  else
	    {
	      parmval = convert_for_initialization
		(NULL_TREE, type, val, flags,
		 ICR_ARGPASS, fndecl, i, complain);
	      parmval = convert_for_arg_passing (type, parmval, complain);
	    }

	  if (parmval == error_mark_node)
	    return -1;

	  (**values)[i] = parmval;
	}
      else
	{
	  if (fndecl && magic_varargs_p (fndecl))
	    /* Don't do ellipsis conversion for __built_in_constant_p
	       as this will result in spurious errors for non-trivial
	       types.  */
	    val = require_complete_type_sfinae (val, complain);
	  else
	    val = convert_arg_to_ellipsis (val, complain);

	  (**values)[i] = val;
	}

      if (typetail)
	typetail = TREE_CHAIN (typetail);
    }

  if (typetail != 0 && typetail != void_list_node)
    {
      /* See if there are default arguments that can be used.  Because
	 we hold default arguments in the FUNCTION_TYPE (which is so
	 wrong), we can see default parameters here from deduced
	 contexts (and via typeof) for indirect function calls.
	 Fortunately we know whether we have a function decl to
	 provide default arguments in a language conformant
	 manner.  */
      if (fndecl && TREE_PURPOSE (typetail)
	  && TREE_CODE (TREE_PURPOSE (typetail)) != DEFAULT_ARG)
	{
	  for (; typetail != void_list_node; ++i)
	    {
	      tree parmval
		= convert_default_arg (TREE_VALUE (typetail),
				       TREE_PURPOSE (typetail),
				       fndecl, i, complain);

	      if (parmval == error_mark_node)
		return -1;

	      vec_safe_push (*values, parmval);
	      typetail = TREE_CHAIN (typetail);
	      /* ends with `...'.  */
	      if (typetail == NULL_TREE)
		break;
	    }
	}
      else
	{
          if (complain & tf_error)
	    warn_args_num (input_location, fndecl, /*too_many_p=*/false);
	  return -1;
	}
    }

  return (int) i;
}

/* Build a binary-operation expression, after performing default
   conversions on the operands.  CODE is the kind of expression to
   build.  ARG1 and ARG2 are the arguments.  ARG1_CODE and ARG2_CODE
   are the tree codes which correspond to ARG1 and ARG2 when issuing
   warnings about possibly misplaced parentheses.  They may differ
   from the TREE_CODE of ARG1 and ARG2 if the parser has done constant
   folding (e.g., if the parser sees "a | 1 + 1", it may call this
   routine with ARG2 being an INTEGER_CST and ARG2_CODE == PLUS_EXPR).
   To avoid issuing any parentheses warnings, pass ARG1_CODE and/or
   ARG2_CODE as ERROR_MARK.  */

tree
build_x_binary_op (location_t loc, enum tree_code code, tree arg1,
		   enum tree_code arg1_code, tree arg2,
		   enum tree_code arg2_code, tree *overload,
		   tsubst_flags_t complain)
{
  tree orig_arg1;
  tree orig_arg2;
  tree expr;

  orig_arg1 = arg1;
  orig_arg2 = arg2;

  if (processing_template_decl)
    {
      if (type_dependent_expression_p (arg1)
	  || type_dependent_expression_p (arg2))
	return build_min_nt_loc (loc, code, arg1, arg2);
      arg1 = build_non_dependent_expr (arg1);
      arg2 = build_non_dependent_expr (arg2);
    }

  if (code == DOTSTAR_EXPR)
    expr = build_m_component_ref (arg1, arg2, complain);
  else
    expr = build_new_op (loc, code, LOOKUP_NORMAL, arg1, arg2, NULL_TREE,
			 overload, complain);

  /* Check for cases such as x+y<<z which users are likely to
     misinterpret.  But don't warn about obj << x + y, since that is a
     common idiom for I/O.  */
  if (warn_parentheses
      && (complain & tf_warning)
      && !processing_template_decl
      && !error_operand_p (arg1)
      && !error_operand_p (arg2)
      && (code != LSHIFT_EXPR
	  || !CLASS_TYPE_P (TREE_TYPE (arg1))))
    warn_about_parentheses (loc, code, arg1_code, orig_arg1,
			    arg2_code, orig_arg2);

  if (processing_template_decl && expr != error_mark_node)
    return build_min_non_dep (code, expr, orig_arg1, orig_arg2);

  return expr;
}

/* Build and return an ARRAY_REF expression.  */

tree
build_x_array_ref (location_t loc, tree arg1, tree arg2,
		   tsubst_flags_t complain)
{
  tree orig_arg1 = arg1;
  tree orig_arg2 = arg2;
  tree expr;

  if (processing_template_decl)
    {
      if (type_dependent_expression_p (arg1)
	  || type_dependent_expression_p (arg2))
	return build_min_nt_loc (loc, ARRAY_REF, arg1, arg2,
				 NULL_TREE, NULL_TREE);
      arg1 = build_non_dependent_expr (arg1);
      arg2 = build_non_dependent_expr (arg2);
    }

  expr = build_new_op (loc, ARRAY_REF, LOOKUP_NORMAL, arg1, arg2,
		       NULL_TREE, /*overload=*/NULL, complain);

  if (processing_template_decl && expr != error_mark_node)
    return build_min_non_dep (ARRAY_REF, expr, orig_arg1, orig_arg2,
			      NULL_TREE, NULL_TREE);
  return expr;
}

/* Return whether OP is an expression of enum type cast to integer
   type.  In C++ even unsigned enum types are cast to signed integer
   types.  We do not want to issue warnings about comparisons between
   signed and unsigned types when one of the types is an enum type.
   Those warnings are always false positives in practice.  */

static bool
enum_cast_to_int (tree op)
{
  if (TREE_CODE (op) == NOP_EXPR
      && TREE_TYPE (op) == integer_type_node
      && TREE_CODE (TREE_TYPE (TREE_OPERAND (op, 0))) == ENUMERAL_TYPE
      && TYPE_UNSIGNED (TREE_TYPE (TREE_OPERAND (op, 0))))
    return true;

  /* The cast may have been pushed into a COND_EXPR.  */
  if (TREE_CODE (op) == COND_EXPR)
    return (enum_cast_to_int (TREE_OPERAND (op, 1))
	    || enum_cast_to_int (TREE_OPERAND (op, 2)));

  return false;
}

/* For the c-common bits.  */
tree
build_binary_op (location_t location, enum tree_code code, tree op0, tree op1,
		 int /*convert_p*/)
{
  return cp_build_binary_op (location, code, op0, op1, tf_warning_or_error);
}


/* Build a binary-operation expression without default conversions.
   CODE is the kind of expression to build.
   LOCATION is the location_t of the operator in the source code.
   This function differs from `build' in several ways:
   the data type of the result is computed and recorded in it,
   warnings are generated if arg data types are invalid,
   special handling for addition and subtraction of pointers is known,
   and some optimization is done (operations on narrow ints
   are done in the narrower type when that gives the same result).
   Constant folding is also done before the result is returned.

   Note that the operands will never have enumeral types
   because either they have just had the default conversions performed
   or they have both just been converted to some other type in which
   the arithmetic is to be done.

   C++: must do special pointer arithmetic when implementing
   multiple inheritance, and deal with pointer to member functions.  */

tree
cp_build_binary_op (location_t location,
		    enum tree_code code, tree orig_op0, tree orig_op1,
		    tsubst_flags_t complain)
{
  tree op0, op1;
  enum tree_code code0, code1;
  tree type0, type1;
  const char *invalid_op_diag;

  /* Expression code to give to the expression when it is built.
     Normally this is CODE, which is what the caller asked for,
     but in some special cases we change it.  */
  enum tree_code resultcode = code;

  /* Data type in which the computation is to be performed.
     In the simplest cases this is the common type of the arguments.  */
  tree result_type = NULL;

  /* Nonzero means operands have already been type-converted
     in whatever way is necessary.
     Zero means they need to be converted to RESULT_TYPE.  */
  int converted = 0;

  /* Nonzero means create the expression with this type, rather than
     RESULT_TYPE.  */
  tree build_type = 0;

  /* Nonzero means after finally constructing the expression
     convert it to this type.  */
  tree final_type = 0;

  tree result;
  tree orig_type = NULL;

  /* Nonzero if this is an operation like MIN or MAX which can
     safely be computed in short if both args are promoted shorts.
     Also implies COMMON.
     -1 indicates a bitwise operation; this makes a difference
     in the exact conditions for when it is safe to do the operation
     in a narrower mode.  */
  int shorten = 0;

  /* Nonzero if this is a comparison operation;
     if both args are promoted shorts, compare the original shorts.
     Also implies COMMON.  */
  int short_compare = 0;

  /* Nonzero means set RESULT_TYPE to the common type of the args.  */
  int common = 0;

  /* True if both operands have arithmetic type.  */
  bool arithmetic_types_p;

  /* Apply default conversions.  */
  op0 = orig_op0;
  op1 = orig_op1;

  /* Remember whether we're doing / or %.  */
  bool doing_div_or_mod = false;

  /* Remember whether we're doing << or >>.  */
  bool doing_shift = false;

  /* Tree holding instrumentation expression.  */
  tree instrument_expr = NULL;

  if (code == TRUTH_AND_EXPR || code == TRUTH_ANDIF_EXPR
      || code == TRUTH_OR_EXPR || code == TRUTH_ORIF_EXPR
      || code == TRUTH_XOR_EXPR)
    {
      if (!really_overloaded_fn (op0) && !VOID_TYPE_P (TREE_TYPE (op0)))
	op0 = decay_conversion (op0, complain);
      if (!really_overloaded_fn (op1) && !VOID_TYPE_P (TREE_TYPE (op1)))
	op1 = decay_conversion (op1, complain);
    }
  else
    {
      if (!really_overloaded_fn (op0) && !VOID_TYPE_P (TREE_TYPE (op0)))
	op0 = cp_default_conversion (op0, complain);
      if (!really_overloaded_fn (op1) && !VOID_TYPE_P (TREE_TYPE (op1)))
	op1 = cp_default_conversion (op1, complain);
    }

  /* Strip NON_LVALUE_EXPRs, etc., since we aren't using as an lvalue.  */
  STRIP_TYPE_NOPS (op0);
  STRIP_TYPE_NOPS (op1);

  /* DTRT if one side is an overloaded function, but complain about it.  */
  if (type_unknown_p (op0))
    {
      tree t = instantiate_type (TREE_TYPE (op1), op0, tf_none);
      if (t != error_mark_node)
	{
	  if (complain & tf_error)
	    permerror (input_location, "assuming cast to type %qT from overloaded function",
		       TREE_TYPE (t));
	  op0 = t;
	}
    }
  if (type_unknown_p (op1))
    {
      tree t = instantiate_type (TREE_TYPE (op0), op1, tf_none);
      if (t != error_mark_node)
	{
	  if (complain & tf_error)
	    permerror (input_location, "assuming cast to type %qT from overloaded function",
		       TREE_TYPE (t));
	  op1 = t;
	}
    }

  type0 = TREE_TYPE (op0); 
  type1 = TREE_TYPE (op1);

  /* The expression codes of the data types of the arguments tell us
     whether the arguments are integers, floating, pointers, etc.  */
  code0 = TREE_CODE (type0);
  code1 = TREE_CODE (type1);

  /* If an error was already reported for one of the arguments,
     avoid reporting another error.  */
  if (code0 == ERROR_MARK || code1 == ERROR_MARK)
    return error_mark_node;

  if ((invalid_op_diag
       = targetm.invalid_binary_op (code, type0, type1)))
    {
      if (complain & tf_error)
	error (invalid_op_diag);
      return error_mark_node;
    }

  /* Issue warnings about peculiar, but valid, uses of NULL.  */
  if ((orig_op0 == null_node || orig_op1 == null_node)
      /* It's reasonable to use pointer values as operands of &&
	 and ||, so NULL is no exception.  */
      && code != TRUTH_ANDIF_EXPR && code != TRUTH_ORIF_EXPR 
      && ( /* Both are NULL (or 0) and the operation was not a
	      comparison or a pointer subtraction.  */
	  (null_ptr_cst_p (orig_op0) && null_ptr_cst_p (orig_op1) 
	   && code != EQ_EXPR && code != NE_EXPR && code != MINUS_EXPR) 
	  /* Or if one of OP0 or OP1 is neither a pointer nor NULL.  */
	  || (!null_ptr_cst_p (orig_op0)
	      && !TYPE_PTR_OR_PTRMEM_P (type0))
	  || (!null_ptr_cst_p (orig_op1) 
	      && !TYPE_PTR_OR_PTRMEM_P (type1)))
      && (complain & tf_warning))
    {
      source_location loc =
	expansion_point_location_if_in_system_header (input_location);

      warning_at (loc, OPT_Wpointer_arith, "NULL used in arithmetic");
    }

  /* In case when one of the operands of the binary operation is
     a vector and another is a scalar -- convert scalar to vector.  */
  if ((code0 == VECTOR_TYPE) != (code1 == VECTOR_TYPE))
    {
      enum stv_conv convert_flag = scalar_to_vector (location, code, op0, op1,
						     complain & tf_error);

      switch (convert_flag)
        {
          case stv_error:
            return error_mark_node;
          case stv_firstarg:
            {
	      op0 = save_expr (op0);
              op0 = convert (TREE_TYPE (type1), op0);
              op0 = build_vector_from_val (type1, op0);
              type0 = TREE_TYPE (op0);
              code0 = TREE_CODE (type0);
              converted = 1;
              break;
            }
          case stv_secondarg:
            {
	      op1 = save_expr (op1);
              op1 = convert (TREE_TYPE (type0), op1);
              op1 = build_vector_from_val (type0, op1);
              type1 = TREE_TYPE (op1);
              code1 = TREE_CODE (type1);
              converted = 1;
              break;
            }
          default:
            break;
        }
    }

  switch (code)
    {
    case MINUS_EXPR:
      /* Subtraction of two similar pointers.
	 We must subtract them as integers, then divide by object size.  */
      if (code0 == POINTER_TYPE && code1 == POINTER_TYPE
	  && same_type_ignoring_top_level_qualifiers_p (TREE_TYPE (type0),
							TREE_TYPE (type1)))
	return pointer_diff (op0, op1, common_pointer_type (type0, type1),
			     complain);
      /* In all other cases except pointer - int, the usual arithmetic
	 rules apply.  */
      else if (!(code0 == POINTER_TYPE && code1 == INTEGER_TYPE))
	{
	  common = 1;
	  break;
	}
      /* The pointer - int case is just like pointer + int; fall
	 through.  */
    case PLUS_EXPR:
      if ((code0 == POINTER_TYPE || code1 == POINTER_TYPE)
	  && (code0 == INTEGER_TYPE || code1 == INTEGER_TYPE))
	{
	  tree ptr_operand;
	  tree int_operand;
	  ptr_operand = ((code0 == POINTER_TYPE) ? op0 : op1);
	  int_operand = ((code0 == INTEGER_TYPE) ? op0 : op1);
	  if (processing_template_decl)
	    {
	      result_type = TREE_TYPE (ptr_operand);
	      break;
	    }
	  return cp_pointer_int_sum (code,
				     ptr_operand, 
				     int_operand,
				     complain);
	}
      common = 1;
      break;

    case MULT_EXPR:
      common = 1;
      break;

    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case EXACT_DIV_EXPR:
      if ((code0 == INTEGER_TYPE || code0 == REAL_TYPE
	   || code0 == COMPLEX_TYPE || code0 == VECTOR_TYPE)
	  && (code1 == INTEGER_TYPE || code1 == REAL_TYPE
	      || code1 == COMPLEX_TYPE || code1 == VECTOR_TYPE))
	{
	  enum tree_code tcode0 = code0, tcode1 = code1;
	  tree cop1 = fold_non_dependent_expr_sfinae (op1, tf_none);
	  cop1 = maybe_constant_value (cop1);

	  if (tcode0 == INTEGER_TYPE)
	    doing_div_or_mod = true;

	  warn_for_div_by_zero (location, cop1);

	  if (tcode0 == COMPLEX_TYPE || tcode0 == VECTOR_TYPE)
	    tcode0 = TREE_CODE (TREE_TYPE (TREE_TYPE (op0)));
	  if (tcode1 == COMPLEX_TYPE || tcode1 == VECTOR_TYPE)
	    tcode1 = TREE_CODE (TREE_TYPE (TREE_TYPE (op1)));

	  if (!(tcode0 == INTEGER_TYPE && tcode1 == INTEGER_TYPE))
	    resultcode = RDIV_EXPR;
	  else
	    /* When dividing two signed integers, we have to promote to int.
	       unless we divide by a constant != -1.  Note that default
	       conversion will have been performed on the operands at this
	       point, so we have to dig out the original type to find out if
	       it was unsigned.  */
	    shorten = ((TREE_CODE (op0) == NOP_EXPR
			&& TYPE_UNSIGNED (TREE_TYPE (TREE_OPERAND (op0, 0))))
		       || (TREE_CODE (op1) == INTEGER_CST
			   && ! integer_all_onesp (op1)));

	  common = 1;
	}
      break;

    case BIT_AND_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
      if ((code0 == INTEGER_TYPE && code1 == INTEGER_TYPE)
	  || (code0 == VECTOR_TYPE && code1 == VECTOR_TYPE
	      && !VECTOR_FLOAT_TYPE_P (type0)
	      && !VECTOR_FLOAT_TYPE_P (type1)))
	shorten = -1;
      break;

    case TRUNC_MOD_EXPR:
    case FLOOR_MOD_EXPR:
      {
	tree cop1 = fold_non_dependent_expr_sfinae (op1, tf_none);
	cop1 = maybe_constant_value (cop1);

	if (code0 == INTEGER_TYPE)
	  doing_div_or_mod = true;
	warn_for_div_by_zero (location, cop1);
      }

      if (code0 == VECTOR_TYPE && code1 == VECTOR_TYPE
	  && TREE_CODE (TREE_TYPE (type0)) == INTEGER_TYPE
	  && TREE_CODE (TREE_TYPE (type1)) == INTEGER_TYPE)
	common = 1;
      else if (code0 == INTEGER_TYPE && code1 == INTEGER_TYPE)
	{
	  /* Although it would be tempting to shorten always here, that loses
	     on some targets, since the modulo instruction is undefined if the
	     quotient can't be represented in the computation mode.  We shorten
	     only if unsigned or if dividing by something we know != -1.  */
	  shorten = ((TREE_CODE (op0) == NOP_EXPR
		      && TYPE_UNSIGNED (TREE_TYPE (TREE_OPERAND (op0, 0))))
		     || (TREE_CODE (op1) == INTEGER_CST
			 && ! integer_all_onesp (op1)));
	  common = 1;
	}
      break;

    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
      if (VECTOR_TYPE_P (type0) || VECTOR_TYPE_P (type1))
	{
	  sorry ("logical operation on vector type");
	  return error_mark_node;
	}
      result_type = boolean_type_node;
      break;

      /* Shift operations: result has same type as first operand;
	 always convert second operand to int.
	 Also set SHORT_SHIFT if shifting rightward.  */

    case RSHIFT_EXPR:
      if (code0 == VECTOR_TYPE && code1 == INTEGER_TYPE
          && TREE_CODE (TREE_TYPE (type0)) == INTEGER_TYPE)
        {
          result_type = type0;
          converted = 1;
        }
      else if (code0 == VECTOR_TYPE && code1 == VECTOR_TYPE
	  && TREE_CODE (TREE_TYPE (type0)) == INTEGER_TYPE
	  && TREE_CODE (TREE_TYPE (type1)) == INTEGER_TYPE
	  && TYPE_VECTOR_SUBPARTS (type0) == TYPE_VECTOR_SUBPARTS (type1))
	{
	  result_type = type0;
	  converted = 1;
	}
      else if (code0 == INTEGER_TYPE && code1 == INTEGER_TYPE)
	{
	  tree const_op1 = fold_non_dependent_expr_sfinae (op1, tf_none);
	  const_op1 = maybe_constant_value (const_op1);
	  if (TREE_CODE (const_op1) != INTEGER_CST)
	    const_op1 = op1;
	  result_type = type0;
	  doing_shift = true;
	  if (TREE_CODE (const_op1) == INTEGER_CST)
	    {
	      if (tree_int_cst_lt (const_op1, integer_zero_node))
		{
		  if ((complain & tf_warning)
		      && c_inhibit_evaluation_warnings == 0)
		    warning (0, "right shift count is negative");
		}
	      else
		{
		  if (compare_tree_int (const_op1, TYPE_PRECISION (type0)) >= 0
		      && (complain & tf_warning)
		      && c_inhibit_evaluation_warnings == 0)
		    warning (0, "right shift count >= width of type");
		}
	    }
	  /* Convert the shift-count to an integer, regardless of
	     size of value being shifted.  */
	  if (TYPE_MAIN_VARIANT (TREE_TYPE (op1)) != integer_type_node)
	    op1 = cp_convert (integer_type_node, op1, complain);
	  /* Avoid converting op1 to result_type later.  */
	  converted = 1;
	}
      break;

    case LSHIFT_EXPR:
      if (code0 == VECTOR_TYPE && code1 == INTEGER_TYPE
          && TREE_CODE (TREE_TYPE (type0)) == INTEGER_TYPE)
        {
          result_type = type0;
          converted = 1;
        }
      else if (code0 == VECTOR_TYPE && code1 == VECTOR_TYPE
	  && TREE_CODE (TREE_TYPE (type0)) == INTEGER_TYPE
	  && TREE_CODE (TREE_TYPE (type1)) == INTEGER_TYPE
	  && TYPE_VECTOR_SUBPARTS (type0) == TYPE_VECTOR_SUBPARTS (type1))
	{
	  result_type = type0;
	  converted = 1;
	}
      else if (code0 == INTEGER_TYPE && code1 == INTEGER_TYPE)
	{
	  tree const_op1 = fold_non_dependent_expr_sfinae (op1, tf_none);
	  const_op1 = maybe_constant_value (const_op1);
	  if (TREE_CODE (const_op1) != INTEGER_CST)
	    const_op1 = op1;
	  result_type = type0;
	  doing_shift = true;
	  if (TREE_CODE (const_op1) == INTEGER_CST)
	    {
	      if (tree_int_cst_lt (const_op1, integer_zero_node))
		{
		  if ((complain & tf_warning)
		      && c_inhibit_evaluation_warnings == 0)
		    warning (0, "left shift count is negative");
		}
	      else if (compare_tree_int (const_op1,
					 TYPE_PRECISION (type0)) >= 0)
		{
		  if ((complain & tf_warning)
		      && c_inhibit_evaluation_warnings == 0)
		    warning (0, "left shift count >= width of type");
		}
	    }
	  /* Convert the shift-count to an integer, regardless of
	     size of value being shifted.  */
	  if (TYPE_MAIN_VARIANT (TREE_TYPE (op1)) != integer_type_node)
	    op1 = cp_convert (integer_type_node, op1, complain);
	  /* Avoid converting op1 to result_type later.  */
	  converted = 1;
	}
      break;

    case RROTATE_EXPR:
    case LROTATE_EXPR:
      if (code0 == INTEGER_TYPE && code1 == INTEGER_TYPE)
	{
	  result_type = type0;
	  if (TREE_CODE (op1) == INTEGER_CST)
	    {
	      if (tree_int_cst_lt (op1, integer_zero_node))
		{
		  if (complain & tf_warning)
		    warning (0, (code == LROTATE_EXPR)
			          ? G_("left rotate count is negative")
   			          : G_("right rotate count is negative"));
		}
	      else if (compare_tree_int (op1, TYPE_PRECISION (type0)) >= 0)
		{
		  if (complain & tf_warning)
		    warning (0, (code == LROTATE_EXPR) 
                                  ? G_("left rotate count >= width of type")
                                  : G_("right rotate count >= width of type"));
		}
	    }
	  /* Convert the shift-count to an integer, regardless of
	     size of value being shifted.  */
	  if (TYPE_MAIN_VARIANT (TREE_TYPE (op1)) != integer_type_node)
	    op1 = cp_convert (integer_type_node, op1, complain);
	}
      break;

    case EQ_EXPR:
    case NE_EXPR:
      if (code0 == VECTOR_TYPE && code1 == VECTOR_TYPE)
	goto vector_compare;
      if ((complain & tf_warning)
	  && (FLOAT_TYPE_P (type0) || FLOAT_TYPE_P (type1)))
	warning (OPT_Wfloat_equal,
		 "comparing floating point with == or != is unsafe");
      if ((complain & tf_warning)
	  && ((TREE_CODE (orig_op0) == STRING_CST && !integer_zerop (op1))
	      || (TREE_CODE (orig_op1) == STRING_CST && !integer_zerop (op0))))
	warning (OPT_Waddress, "comparison with string literal results in unspecified behaviour");

      build_type = boolean_type_node;
      if ((code0 == INTEGER_TYPE || code0 == REAL_TYPE
	   || code0 == COMPLEX_TYPE || code0 == ENUMERAL_TYPE)
	  && (code1 == INTEGER_TYPE || code1 == REAL_TYPE
	      || code1 == COMPLEX_TYPE || code1 == ENUMERAL_TYPE))
	short_compare = 1;
      else if ((code0 == POINTER_TYPE && code1 == POINTER_TYPE)
	       || (TYPE_PTRDATAMEM_P (type0) && TYPE_PTRDATAMEM_P (type1)))
	result_type = composite_pointer_type (type0, type1, op0, op1,
					      CPO_COMPARISON, complain);
      else if ((code0 == POINTER_TYPE || TYPE_PTRDATAMEM_P (type0))
	       && null_ptr_cst_p (op1))
	{
	  if (TREE_CODE (op0) == ADDR_EXPR
	      && decl_with_nonnull_addr_p (TREE_OPERAND (op0, 0)))
	    {
	      if ((complain & tf_warning)
		  && c_inhibit_evaluation_warnings == 0)
		warning (OPT_Waddress, "the address of %qD will never be NULL",
			 TREE_OPERAND (op0, 0));
	    }
	  result_type = type0;
	}
      else if ((code1 == POINTER_TYPE || TYPE_PTRDATAMEM_P (type1))
	       && null_ptr_cst_p (op0))
	{
	  if (TREE_CODE (op1) == ADDR_EXPR 
	      && decl_with_nonnull_addr_p (TREE_OPERAND (op1, 0)))
	    {
	      if ((complain & tf_warning)
		  && c_inhibit_evaluation_warnings == 0)
		warning (OPT_Waddress, "the address of %qD will never be NULL",
			 TREE_OPERAND (op1, 0));
	    }
	  result_type = type1;
	}
      else if (null_ptr_cst_p (op0) && null_ptr_cst_p (op1))
	/* One of the operands must be of nullptr_t type.  */
        result_type = TREE_TYPE (nullptr_node);
      else if (code0 == POINTER_TYPE && code1 == INTEGER_TYPE)
	{
	  result_type = type0;
	  if (complain & tf_error) 
            permerror (input_location, "ISO C++ forbids comparison between pointer and integer");
          else
            return error_mark_node;
	}
      else if (code0 == INTEGER_TYPE && code1 == POINTER_TYPE)
	{
	  result_type = type1;
	  if (complain & tf_error)
	    permerror (input_location, "ISO C++ forbids comparison between pointer and integer");
          else
            return error_mark_node;
	}
      else if (TYPE_PTRMEMFUNC_P (type0) && null_ptr_cst_p (op1))
	{
	  if (TARGET_PTRMEMFUNC_VBIT_LOCATION
	      == ptrmemfunc_vbit_in_delta)
	    {
	      tree pfn0, delta0, e1, e2;

	      if (TREE_SIDE_EFFECTS (op0))
		op0 = save_expr (op0);

	      pfn0 = pfn_from_ptrmemfunc (op0);
	      delta0 = delta_from_ptrmemfunc (op0);
	      e1 = cp_build_binary_op (location,
				       EQ_EXPR,
	  			       pfn0,
				       build_zero_cst (TREE_TYPE (pfn0)),
				       complain);
	      e2 = cp_build_binary_op (location,
				       BIT_AND_EXPR,
				       delta0,
				       integer_one_node,
				       complain);

	      if (complain & tf_warning)
		maybe_warn_zero_as_null_pointer_constant (op1, input_location);

	      e2 = cp_build_binary_op (location,
				       EQ_EXPR, e2, integer_zero_node,
				       complain);
	      op0 = cp_build_binary_op (location,
					TRUTH_ANDIF_EXPR, e1, e2,
					complain);
	      op1 = cp_convert (TREE_TYPE (op0), integer_one_node, complain); 
	    }
     	  else 
	    {
	      op0 = build_ptrmemfunc_access_expr (op0, pfn_identifier);
	      op1 = cp_convert (TREE_TYPE (op0), op1, complain);
	    }
	  result_type = TREE_TYPE (op0);
	}
      else if (TYPE_PTRMEMFUNC_P (type1) && null_ptr_cst_p (op0))
	return cp_build_binary_op (location, code, op1, op0, complain);
      else if (TYPE_PTRMEMFUNC_P (type0) && TYPE_PTRMEMFUNC_P (type1))
	{
	  tree type;
	  /* E will be the final comparison.  */
	  tree e;
	  /* E1 and E2 are for scratch.  */
	  tree e1;
	  tree e2;
	  tree pfn0;
	  tree pfn1;
	  tree delta0;
	  tree delta1;

	  type = composite_pointer_type (type0, type1, op0, op1, 
					 CPO_COMPARISON, complain);

	  if (!same_type_p (TREE_TYPE (op0), type))
	    op0 = cp_convert_and_check (type, op0, complain);
	  if (!same_type_p (TREE_TYPE (op1), type))
	    op1 = cp_convert_and_check (type, op1, complain);

	  if (op0 == error_mark_node || op1 == error_mark_node)
	    return error_mark_node;

	  if (TREE_SIDE_EFFECTS (op0))
	    op0 = save_expr (op0);
	  if (TREE_SIDE_EFFECTS (op1))
	    op1 = save_expr (op1);

	  pfn0 = pfn_from_ptrmemfunc (op0);
	  pfn1 = pfn_from_ptrmemfunc (op1);
	  delta0 = delta_from_ptrmemfunc (op0);
	  delta1 = delta_from_ptrmemfunc (op1);
	  if (TARGET_PTRMEMFUNC_VBIT_LOCATION
	      == ptrmemfunc_vbit_in_delta)
	    {
	      /* We generate:

		 (op0.pfn == op1.pfn
		  && ((op0.delta == op1.delta)
     		       || (!op0.pfn && op0.delta & 1 == 0 
			   && op1.delta & 1 == 0))

	         The reason for the `!op0.pfn' bit is that a NULL
	         pointer-to-member is any member with a zero PFN and
	         LSB of the DELTA field is 0.  */

	      e1 = cp_build_binary_op (location, BIT_AND_EXPR,
				       delta0, 
				       integer_one_node,
				       complain);
	      e1 = cp_build_binary_op (location,
				       EQ_EXPR, e1, integer_zero_node,
				       complain);
	      e2 = cp_build_binary_op (location, BIT_AND_EXPR,
				       delta1,
				       integer_one_node,
				       complain);
	      e2 = cp_build_binary_op (location,
				       EQ_EXPR, e2, integer_zero_node,
				       complain);
	      e1 = cp_build_binary_op (location,
				       TRUTH_ANDIF_EXPR, e2, e1,
				       complain);
	      e2 = cp_build_binary_op (location, EQ_EXPR,
				       pfn0,
				       build_zero_cst (TREE_TYPE (pfn0)),
				       complain);
	      e2 = cp_build_binary_op (location,
				       TRUTH_ANDIF_EXPR, e2, e1, complain);
	      e1 = cp_build_binary_op (location,
				       EQ_EXPR, delta0, delta1, complain);
	      e1 = cp_build_binary_op (location,
				       TRUTH_ORIF_EXPR, e1, e2, complain);
	    }
	  else
	    {
	      /* We generate:

	         (op0.pfn == op1.pfn
	         && (!op0.pfn || op0.delta == op1.delta))

	         The reason for the `!op0.pfn' bit is that a NULL
	         pointer-to-member is any member with a zero PFN; the
	         DELTA field is unspecified.  */
 
    	      e1 = cp_build_binary_op (location,
				       EQ_EXPR, delta0, delta1, complain);
	      e2 = cp_build_binary_op (location,
				       EQ_EXPR,
		      		       pfn0,
			   	       build_zero_cst (TREE_TYPE (pfn0)),
				       complain);
	      e1 = cp_build_binary_op (location,
				       TRUTH_ORIF_EXPR, e1, e2, complain);
	    }
	  e2 = build2 (EQ_EXPR, boolean_type_node, pfn0, pfn1);
	  e = cp_build_binary_op (location,
				  TRUTH_ANDIF_EXPR, e2, e1, complain);
	  if (code == EQ_EXPR)
	    return e;
	  return cp_build_binary_op (location,
				     EQ_EXPR, e, integer_zero_node, complain);
	}
      else
	{
	  gcc_assert (!TYPE_PTRMEMFUNC_P (type0)
		      || !same_type_p (TYPE_PTRMEMFUNC_FN_TYPE (type0),
				       type1));
	  gcc_assert (!TYPE_PTRMEMFUNC_P (type1)
		      || !same_type_p (TYPE_PTRMEMFUNC_FN_TYPE (type1),
				       type0));
	}

      break;

    case MAX_EXPR:
    case MIN_EXPR:
      if ((code0 == INTEGER_TYPE || code0 == REAL_TYPE)
	   && (code1 == INTEGER_TYPE || code1 == REAL_TYPE))
	shorten = 1;
      else if (code0 == POINTER_TYPE && code1 == POINTER_TYPE)
	result_type = composite_pointer_type (type0, type1, op0, op1,
					      CPO_COMPARISON, complain);
      break;

    case LE_EXPR:
    case GE_EXPR:
    case LT_EXPR:
    case GT_EXPR:
      if (TREE_CODE (orig_op0) == STRING_CST
	  || TREE_CODE (orig_op1) == STRING_CST)
	{
	  if (complain & tf_warning)
	    warning (OPT_Waddress, "comparison with string literal results in unspecified behaviour");
	}

      if (code0 == VECTOR_TYPE && code1 == VECTOR_TYPE)
	{
	vector_compare:
	  tree intt;
	  if (!same_type_ignoring_top_level_qualifiers_p (TREE_TYPE (type0),
							  TREE_TYPE (type1))
	      && !vector_types_compatible_elements_p (type0, type1))
	    {
	      if (complain & tf_error)
		{
		  error_at (location, "comparing vectors with different "
				      "element types");
		  inform (location, "operand types are %qT and %qT",
			  type0, type1);
		}
	      return error_mark_node;
	    }

	  if (TYPE_VECTOR_SUBPARTS (type0) != TYPE_VECTOR_SUBPARTS (type1))
	    {
	      if (complain & tf_error)
		{
		  error_at (location, "comparing vectors with different "
				      "number of elements");
		  inform (location, "operand types are %qT and %qT",
			  type0, type1);
		}
	      return error_mark_node;
	    }

	  /* Always construct signed integer vector type.  */
	  intt = c_common_type_for_size (GET_MODE_BITSIZE
					   (TYPE_MODE (TREE_TYPE (type0))), 0);
	  if (!intt)
	    {
	      if (complain & tf_error)
		error_at (location, "could not find an integer type "
			  "of the same size as %qT", TREE_TYPE (type0));
	      return error_mark_node;
	    }
	  result_type = build_opaque_vector_type (intt,
						  TYPE_VECTOR_SUBPARTS (type0));
	  converted = 1;
	  break;
	}
      build_type = boolean_type_node;
      if ((code0 == INTEGER_TYPE || code0 == REAL_TYPE
	   || code0 == ENUMERAL_TYPE)
	   && (code1 == INTEGER_TYPE || code1 == REAL_TYPE
	       || code1 == ENUMERAL_TYPE))
	short_compare = 1;
      else if (code0 == POINTER_TYPE && code1 == POINTER_TYPE)
	result_type = composite_pointer_type (type0, type1, op0, op1,
					      CPO_COMPARISON, complain);
      else if (code0 == POINTER_TYPE && null_ptr_cst_p (op1))
	{
	  result_type = type0;
	  if (extra_warnings && (complain & tf_warning))
	    warning (OPT_Wextra,
		     "ordered comparison of pointer with integer zero");
	}
      else if (code1 == POINTER_TYPE && null_ptr_cst_p (op0))
	{
	  result_type = type1;
	  if (extra_warnings && (complain & tf_warning))
	    warning (OPT_Wextra,
		     "ordered comparison of pointer with integer zero");
	}
      else if (null_ptr_cst_p (op0) && null_ptr_cst_p (op1))
	/* One of the operands must be of nullptr_t type.  */
        result_type = TREE_TYPE (nullptr_node);
      else if (code0 == POINTER_TYPE && code1 == INTEGER_TYPE)
	{
	  result_type = type0;
	  if (complain & tf_error)
	    permerror (input_location, "ISO C++ forbids comparison between pointer and integer");
          else
            return error_mark_node;
	}
      else if (code0 == INTEGER_TYPE && code1 == POINTER_TYPE)
	{
	  result_type = type1;
	  if (complain & tf_error)
	    permerror (input_location, "ISO C++ forbids comparison between pointer and integer");
          else
            return error_mark_node;
	}
      break;

    case UNORDERED_EXPR:
    case ORDERED_EXPR:
    case UNLT_EXPR:
    case UNLE_EXPR:
    case UNGT_EXPR:
    case UNGE_EXPR:
    case UNEQ_EXPR:
      build_type = integer_type_node;
      if (code0 != REAL_TYPE || code1 != REAL_TYPE)
	{
	  if (complain & tf_error)
	    error ("unordered comparison on non-floating point argument");
	  return error_mark_node;
	}
      common = 1;
      break;

    default:
      break;
    }

  if (((code0 == INTEGER_TYPE || code0 == REAL_TYPE || code0 == COMPLEX_TYPE
	|| code0 == ENUMERAL_TYPE)
       && (code1 == INTEGER_TYPE || code1 == REAL_TYPE
	   || code1 == COMPLEX_TYPE || code1 == ENUMERAL_TYPE)))
    arithmetic_types_p = 1;
  else
    {
      arithmetic_types_p = 0;
      /* Vector arithmetic is only allowed when both sides are vectors.  */
      if (code0 == VECTOR_TYPE && code1 == VECTOR_TYPE)
	{
	  if (!tree_int_cst_equal (TYPE_SIZE (type0), TYPE_SIZE (type1))
	      || !vector_types_compatible_elements_p (type0, type1))
	    {
	      if (complain & tf_error)
		binary_op_error (location, code, type0, type1);
	      return error_mark_node;
	    }
	  arithmetic_types_p = 1;
	}
    }
  /* Determine the RESULT_TYPE, if it is not already known.  */
  if (!result_type
      && arithmetic_types_p
      && (shorten || common || short_compare))
    {
      result_type = cp_common_type (type0, type1);
      if (complain & tf_warning)
	do_warn_double_promotion (result_type, type0, type1,
				  "implicit conversion from %qT to %qT "
				  "to match other operand of binary "
				  "expression",
				  location);
    }

  if (!result_type)
    {
      if (complain & tf_error)
	error ("invalid operands of types %qT and %qT to binary %qO",
	       TREE_TYPE (orig_op0), TREE_TYPE (orig_op1), code);
      return error_mark_node;
    }

  /* If we're in a template, the only thing we need to know is the
     RESULT_TYPE.  */
  if (processing_template_decl)
    {
      /* Since the middle-end checks the type when doing a build2, we
	 need to build the tree in pieces.  This built tree will never
	 get out of the front-end as we replace it when instantiating
	 the template.  */
      tree tmp = build2 (resultcode,
			 build_type ? build_type : result_type,
			 NULL_TREE, op1);
      TREE_OPERAND (tmp, 0) = op0;
      return tmp;
    }

  if (arithmetic_types_p)
    {
      bool first_complex = (code0 == COMPLEX_TYPE);
      bool second_complex = (code1 == COMPLEX_TYPE);
      int none_complex = (!first_complex && !second_complex);

      /* Adapted from patch for c/24581.  */
      if (first_complex != second_complex
	  && (code == PLUS_EXPR
	      || code == MINUS_EXPR
	      || code == MULT_EXPR
	      || (code == TRUNC_DIV_EXPR && first_complex))
	  && TREE_CODE (TREE_TYPE (result_type)) == REAL_TYPE
	  && flag_signed_zeros)
	{
	  /* An operation on mixed real/complex operands must be
	     handled specially, but the language-independent code can
	     more easily optimize the plain complex arithmetic if
	     -fno-signed-zeros.  */
	  tree real_type = TREE_TYPE (result_type);
	  tree real, imag;
	  if (first_complex)
	    {
	      if (TREE_TYPE (op0) != result_type)
		op0 = cp_convert_and_check (result_type, op0, complain);
	      if (TREE_TYPE (op1) != real_type)
		op1 = cp_convert_and_check (real_type, op1, complain);
	    }
	  else
	    {
	      if (TREE_TYPE (op0) != real_type)
		op0 = cp_convert_and_check (real_type, op0, complain);
	      if (TREE_TYPE (op1) != result_type)
		op1 = cp_convert_and_check (result_type, op1, complain);
	    }
	  if (TREE_CODE (op0) == ERROR_MARK || TREE_CODE (op1) == ERROR_MARK)
	    return error_mark_node;
	  if (first_complex)
	    {
	      op0 = save_expr (op0);
	      real = cp_build_unary_op (REALPART_EXPR, op0, 1, complain);
	      imag = cp_build_unary_op (IMAGPART_EXPR, op0, 1, complain);
	      switch (code)
		{
		case MULT_EXPR:
		case TRUNC_DIV_EXPR:
		  op1 = save_expr (op1);
		  imag = build2 (resultcode, real_type, imag, op1);
		  /* Fall through.  */
		case PLUS_EXPR:
		case MINUS_EXPR:
		  real = build2 (resultcode, real_type, real, op1);
		  break;
		default:
		  gcc_unreachable();
		}
	    }
	  else
	    {
	      op1 = save_expr (op1);
	      real = cp_build_unary_op (REALPART_EXPR, op1, 1, complain);
	      imag = cp_build_unary_op (IMAGPART_EXPR, op1, 1, complain);
	      switch (code)
		{
		case MULT_EXPR:
		  op0 = save_expr (op0);
		  imag = build2 (resultcode, real_type, op0, imag);
		  /* Fall through.  */
		case PLUS_EXPR:
		  real = build2 (resultcode, real_type, op0, real);
		  break;
		case MINUS_EXPR:
		  real = build2 (resultcode, real_type, op0, real);
		  imag = build1 (NEGATE_EXPR, real_type, imag);
		  break;
		default:
		  gcc_unreachable();
		}
	    }
	  real = fold_if_not_in_template (real);
	  imag = fold_if_not_in_template (imag);
	  result = build2 (COMPLEX_EXPR, result_type, real, imag);
	  result = fold_if_not_in_template (result);
	  return result;
	}

      /* For certain operations (which identify themselves by shorten != 0)
	 if both args were extended from the same smaller type,
	 do the arithmetic in that type and then extend.

	 shorten !=0 and !=1 indicates a bitwise operation.
	 For them, this optimization is safe only if
	 both args are zero-extended or both are sign-extended.
	 Otherwise, we might change the result.
	 E.g., (short)-1 | (unsigned short)-1 is (int)-1
	 but calculated in (unsigned short) it would be (unsigned short)-1.  */

      if (shorten && none_complex)
	{
	  orig_type = result_type;
	  final_type = result_type;
	  result_type = shorten_binary_op (result_type, op0, op1,
					   shorten == -1);
	}

      /* Comparison operations are shortened too but differently.
	 They identify themselves by setting short_compare = 1.  */

      if (short_compare)
	{
	  /* Don't write &op0, etc., because that would prevent op0
	     from being kept in a register.
	     Instead, make copies of the our local variables and
	     pass the copies by reference, then copy them back afterward.  */
	  tree xop0 = op0, xop1 = op1, xresult_type = result_type;
	  enum tree_code xresultcode = resultcode;
	  tree val
	    = shorten_compare (location, &xop0, &xop1, &xresult_type,
			       &xresultcode);
	  if (val != 0)
	    return cp_convert (boolean_type_node, val, complain);
	  op0 = xop0, op1 = xop1;
	  converted = 1;
	  resultcode = xresultcode;
	}

      if ((short_compare || code == MIN_EXPR || code == MAX_EXPR)
	  && warn_sign_compare
	  /* Do not warn until the template is instantiated; we cannot
	     bound the ranges of the arguments until that point.  */
	  && !processing_template_decl
          && (complain & tf_warning)
	  && c_inhibit_evaluation_warnings == 0
	  /* Even unsigned enum types promote to signed int.  We don't
	     want to issue -Wsign-compare warnings for this case.  */
	  && !enum_cast_to_int (orig_op0)
	  && !enum_cast_to_int (orig_op1))
	{
	  tree oop0 = maybe_constant_value (orig_op0);
	  tree oop1 = maybe_constant_value (orig_op1);

	  if (TREE_CODE (oop0) != INTEGER_CST)
	    oop0 = orig_op0;
	  if (TREE_CODE (oop1) != INTEGER_CST)
	    oop1 = orig_op1;
	  warn_for_sign_compare (location, oop0, oop1, op0, op1, 
				 result_type, resultcode);
	}
    }

  /* If CONVERTED is zero, both args will be converted to type RESULT_TYPE.
     Then the expression will be built.
     It will be given type FINAL_TYPE if that is nonzero;
     otherwise, it will be given type RESULT_TYPE.  */
  if (! converted)
    {
      if (TREE_TYPE (op0) != result_type)
	op0 = cp_convert_and_check (result_type, op0, complain);
      if (TREE_TYPE (op1) != result_type)
	op1 = cp_convert_and_check (result_type, op1, complain);

      if (op0 == error_mark_node || op1 == error_mark_node)
	return error_mark_node;
    }

  if (build_type == NULL_TREE)
    build_type = result_type;

  if ((flag_sanitize & (SANITIZE_SHIFT | SANITIZE_DIVIDE))
      && !processing_template_decl
      && current_function_decl != 0
      && !lookup_attribute ("no_sanitize_undefined",
			    DECL_ATTRIBUTES (current_function_decl))
      && (doing_div_or_mod || doing_shift))
    {
      /* OP0 and/or OP1 might have side-effects.  */
      op0 = cp_save_expr (op0);
      op1 = cp_save_expr (op1);
      op0 = maybe_constant_value (fold_non_dependent_expr_sfinae (op0,
								  tf_none));
      op1 = maybe_constant_value (fold_non_dependent_expr_sfinae (op1,
								  tf_none));
      if (doing_div_or_mod && (flag_sanitize & SANITIZE_DIVIDE))
	{
	  /* For diagnostics we want to use the promoted types without
	     shorten_binary_op.  So convert the arguments to the
	     original result_type.  */
	  tree cop0 = op0;
	  tree cop1 = op1;
	  if (orig_type != NULL && result_type != orig_type)
	    {
	      cop0 = cp_convert (orig_type, op0, complain);
	      cop1 = cp_convert (orig_type, op1, complain);
	    }
	  instrument_expr = ubsan_instrument_division (location, cop0, cop1);
	}
      else if (doing_shift && (flag_sanitize & SANITIZE_SHIFT))
	instrument_expr = ubsan_instrument_shift (location, code, op0, op1);
    }

  result = build2 (resultcode, build_type, op0, op1);
  result = fold_if_not_in_template (result);
  if (final_type != 0)
    result = cp_convert (final_type, result, complain);

  if (TREE_OVERFLOW_P (result) 
      && !TREE_OVERFLOW_P (op0) 
      && !TREE_OVERFLOW_P (op1))
    overflow_warning (location, result);

  if (instrument_expr != NULL)
    result = fold_build2 (COMPOUND_EXPR, TREE_TYPE (result),
			  instrument_expr, result);

  return result;
}

/* Build a VEC_PERM_EXPR.
   This is a simple wrapper for c_build_vec_perm_expr.  */
tree
build_x_vec_perm_expr (location_t loc,
			tree arg0, tree arg1, tree arg2,
			tsubst_flags_t complain)
{
  tree orig_arg0 = arg0;
  tree orig_arg1 = arg1;
  tree orig_arg2 = arg2;
  if (processing_template_decl)
    {
      if (type_dependent_expression_p (arg0)
	  || type_dependent_expression_p (arg1)
	  || type_dependent_expression_p (arg2))
	return build_min_nt_loc (loc, VEC_PERM_EXPR, arg0, arg1, arg2);
      arg0 = build_non_dependent_expr (arg0);
      if (arg1)
	arg1 = build_non_dependent_expr (arg1);
      arg2 = build_non_dependent_expr (arg2);
    }
  tree exp = c_build_vec_perm_expr (loc, arg0, arg1, arg2, complain & tf_error);
  if (processing_template_decl && exp != error_mark_node)
    return build_min_non_dep (VEC_PERM_EXPR, exp, orig_arg0,
			      orig_arg1, orig_arg2);
  return exp;
}

/* Return a tree for the sum or difference (RESULTCODE says which)
   of pointer PTROP and integer INTOP.  */

static tree
cp_pointer_int_sum (enum tree_code resultcode, tree ptrop, tree intop,
		    tsubst_flags_t complain)
{
  tree res_type = TREE_TYPE (ptrop);

  /* pointer_int_sum() uses size_in_bytes() on the TREE_TYPE(res_type)
     in certain circumstance (when it's valid to do so).  So we need
     to make sure it's complete.  We don't need to check here, if we
     can actually complete it at all, as those checks will be done in
     pointer_int_sum() anyway.  */
  complete_type (TREE_TYPE (res_type));

  return pointer_int_sum (input_location, resultcode, ptrop,
			  fold_if_not_in_template (intop),
			  complain & tf_warning_or_error);
}

/* Return a tree for the difference of pointers OP0 and OP1.
   The resulting tree has type int.  */

static tree
pointer_diff (tree op0, tree op1, tree ptrtype, tsubst_flags_t complain)
{
  tree result;
  tree restype = ptrdiff_type_node;
  tree target_type = TREE_TYPE (ptrtype);

  if (!complete_type_or_else (target_type, NULL_TREE))
    return error_mark_node;

  if (VOID_TYPE_P (target_type))
    {
      if (complain & tf_error)
	permerror (input_location, "ISO C++ forbids using pointer of "
		   "type %<void *%> in subtraction");
      else
	return error_mark_node;
    }
  if (TREE_CODE (target_type) == FUNCTION_TYPE)
    {
      if (complain & tf_error)
	permerror (input_location, "ISO C++ forbids using pointer to "
		   "a function in subtraction");
      else
	return error_mark_node;
    }
  if (TREE_CODE (target_type) == METHOD_TYPE)
    {
      if (complain & tf_error)
	permerror (input_location, "ISO C++ forbids using pointer to "
		   "a method in subtraction");
      else
	return error_mark_node;
    }

  /* First do the subtraction as integers;
     then drop through to build the divide operator.  */

  op0 = cp_build_binary_op (input_location,
			    MINUS_EXPR,
			    cp_convert (restype, op0, complain),
			    cp_convert (restype, op1, complain),
			    complain);

  /* This generates an error if op1 is a pointer to an incomplete type.  */
  if (!COMPLETE_TYPE_P (TREE_TYPE (TREE_TYPE (op1))))
    {
      if (complain & tf_error)
	error ("invalid use of a pointer to an incomplete type in "
	       "pointer arithmetic");
      else
	return error_mark_node;
    }

  if (pointer_to_zero_sized_aggr_p (TREE_TYPE (op1)))
    {
      if (complain & tf_error)
	error ("arithmetic on pointer to an empty aggregate");
      else
	return error_mark_node;
    }

  op1 = (TYPE_PTROB_P (ptrtype)
	 ? size_in_bytes (target_type)
	 : integer_one_node);

  /* Do the division.  */

  result = build2 (EXACT_DIV_EXPR, restype, op0,
		   cp_convert (restype, op1, complain));
  return fold_if_not_in_template (result);
}

/* Construct and perhaps optimize a tree representation
   for a unary operation.  CODE, a tree_code, specifies the operation
   and XARG is the operand.  */

tree
build_x_unary_op (location_t loc, enum tree_code code, tree xarg,
		  tsubst_flags_t complain)
{
  tree orig_expr = xarg;
  tree exp;
  int ptrmem = 0;

  if (processing_template_decl)
    {
      if (type_dependent_expression_p (xarg))
	return build_min_nt_loc (loc, code, xarg, NULL_TREE);

      xarg = build_non_dependent_expr (xarg);
    }

  exp = NULL_TREE;

  /* [expr.unary.op] says:

       The address of an object of incomplete type can be taken.

     (And is just the ordinary address operator, not an overloaded
     "operator &".)  However, if the type is a template
     specialization, we must complete the type at this point so that
     an overloaded "operator &" will be available if required.  */
  if (code == ADDR_EXPR
      && TREE_CODE (xarg) != TEMPLATE_ID_EXPR
      && ((CLASS_TYPE_P (TREE_TYPE (xarg))
	   && !COMPLETE_TYPE_P (complete_type (TREE_TYPE (xarg))))
	  || (TREE_CODE (xarg) == OFFSET_REF)))
    /* Don't look for a function.  */;
  else
    exp = build_new_op (loc, code, LOOKUP_NORMAL, xarg, NULL_TREE,
			NULL_TREE, /*overload=*/NULL, complain);
  if (!exp && code == ADDR_EXPR)
    {
      if (is_overloaded_fn (xarg))
	{
	  tree fn = get_first_fn (xarg);
	  if (DECL_CONSTRUCTOR_P (fn) || DECL_DESTRUCTOR_P (fn))
	    {
	      if (complain & tf_error)
		error (DECL_CONSTRUCTOR_P (fn)
		       ? G_("taking address of constructor %qE")
		       : G_("taking address of destructor %qE"),
		       xarg);
	      return error_mark_node;
	    }
	}

      /* A pointer to member-function can be formed only by saying
	 &X::mf.  */
      if (!flag_ms_extensions && TREE_CODE (TREE_TYPE (xarg)) == METHOD_TYPE
	  && (TREE_CODE (xarg) != OFFSET_REF || !PTRMEM_OK_P (xarg)))
	{
	  if (TREE_CODE (xarg) != OFFSET_REF
	      || !TYPE_P (TREE_OPERAND (xarg, 0)))
	    {
	      if (complain & tf_error)
		{
		  error ("invalid use of %qE to form a "
			 "pointer-to-member-function", xarg);
		  if (TREE_CODE (xarg) != OFFSET_REF)
		    inform (input_location, "  a qualified-id is required");
		}
	      return error_mark_node;
	    }
	  else
	    {
	      if (complain & tf_error)
		error ("parentheses around %qE cannot be used to form a"
		       " pointer-to-member-function",
		       xarg);
	      else
		return error_mark_node;
	      PTRMEM_OK_P (xarg) = 1;
	    }
	}

      if (TREE_CODE (xarg) == OFFSET_REF)
	{
	  ptrmem = PTRMEM_OK_P (xarg);

	  if (!ptrmem && !flag_ms_extensions
	      && TREE_CODE (TREE_TYPE (TREE_OPERAND (xarg, 1))) == METHOD_TYPE)
	    {
	      /* A single non-static member, make sure we don't allow a
		 pointer-to-member.  */
	      xarg = build2 (OFFSET_REF, TREE_TYPE (xarg),
			     TREE_OPERAND (xarg, 0),
			     ovl_cons (TREE_OPERAND (xarg, 1), NULL_TREE));
	      PTRMEM_OK_P (xarg) = ptrmem;
	    }
	}

      exp = cp_build_addr_expr_strict (xarg, complain);
    }

  if (processing_template_decl && exp != error_mark_node)
    exp = build_min_non_dep (code, exp, orig_expr,
			     /*For {PRE,POST}{INC,DEC}REMENT_EXPR*/NULL_TREE);
  if (TREE_CODE (exp) == ADDR_EXPR)
    PTRMEM_OK_P (exp) = ptrmem;
  return exp;
}

/* Like c_common_truthvalue_conversion, but handle pointer-to-member
   constants, where a null value is represented by an INTEGER_CST of
   -1.  */

tree
cp_truthvalue_conversion (tree expr)
{
  tree type = TREE_TYPE (expr);
  if (TYPE_PTRDATAMEM_P (type)
      /* Avoid ICE on invalid use of non-static member function.  */
      || TREE_CODE (expr) == FUNCTION_DECL)
    return build_binary_op (EXPR_LOCATION (expr),
			    NE_EXPR, expr, nullptr_node, 1);
  else if (TYPE_PTR_P (type) || TYPE_PTRMEMFUNC_P (type))
    {
      /* With -Wzero-as-null-pointer-constant do not warn for an
	 'if (p)' or a 'while (!p)', where p is a pointer.  */
      tree ret;
      ++c_inhibit_evaluation_warnings;
      ret = c_common_truthvalue_conversion (input_location, expr);
      --c_inhibit_evaluation_warnings;
      return ret;
    }
  else
    return c_common_truthvalue_conversion (input_location, expr);
}

/* Just like cp_truthvalue_conversion, but we want a CLEANUP_POINT_EXPR.  */

tree
condition_conversion (tree expr)
{
  tree t;
  if (processing_template_decl)
    return expr;
  t = perform_implicit_conversion_flags (boolean_type_node, expr,
					 tf_warning_or_error, LOOKUP_NORMAL);
  t = fold_build_cleanup_point_expr (boolean_type_node, t);
  return t;
}

/* Returns the address of T.  This function will fold away
   ADDR_EXPR of INDIRECT_REF.  */

tree
build_address (tree t)
{
  if (error_operand_p (t) || !cxx_mark_addressable (t))
    return error_mark_node;
  t = build_fold_addr_expr (t);
  if (TREE_CODE (t) != ADDR_EXPR)
    t = rvalue (t);
  return t;
}

/* Returns the address of T with type TYPE.  */

tree
build_typed_address (tree t, tree type)
{
  if (error_operand_p (t) || !cxx_mark_addressable (t))
    return error_mark_node;
  t = build_fold_addr_expr_with_type (t, type);
  if (TREE_CODE (t) != ADDR_EXPR)
    t = rvalue (t);
  return t;
}

/* Return a NOP_EXPR converting EXPR to TYPE.  */

tree
build_nop (tree type, tree expr)
{
  if (type == error_mark_node || error_operand_p (expr))
    return expr;
  return build1 (NOP_EXPR, type, expr);
}

/* Take the address of ARG, whatever that means under C++ semantics.
   If STRICT_LVALUE is true, require an lvalue; otherwise, allow xvalues
   and class rvalues as well.

   Nothing should call this function directly; instead, callers should use
   cp_build_addr_expr or cp_build_addr_expr_strict.  */

static tree
cp_build_addr_expr_1 (tree arg, bool strict_lvalue, tsubst_flags_t complain)
{
  tree argtype;
  tree val;

  if (!arg || error_operand_p (arg))
    return error_mark_node;

  arg = mark_lvalue_use (arg);
  argtype = lvalue_type (arg);

  gcc_assert (!identifier_p (arg) || !IDENTIFIER_OPNAME_P (arg));

  if (TREE_CODE (arg) == COMPONENT_REF && type_unknown_p (arg)
      && !really_overloaded_fn (TREE_OPERAND (arg, 1)))
    {
      /* They're trying to take the address of a unique non-static
	 member function.  This is ill-formed (except in MS-land),
	 but let's try to DTRT.
	 Note: We only handle unique functions here because we don't
	 want to complain if there's a static overload; non-unique
	 cases will be handled by instantiate_type.  But we need to
	 handle this case here to allow casts on the resulting PMF.
	 We could defer this in non-MS mode, but it's easier to give
	 a useful error here.  */

      /* Inside constant member functions, the `this' pointer
	 contains an extra const qualifier.  TYPE_MAIN_VARIANT
	 is used here to remove this const from the diagnostics
	 and the created OFFSET_REF.  */
      tree base = TYPE_MAIN_VARIANT (TREE_TYPE (TREE_OPERAND (arg, 0)));
      tree fn = get_first_fn (TREE_OPERAND (arg, 1));
      mark_used (fn);

      if (! flag_ms_extensions)
	{
	  tree name = DECL_NAME (fn);
	  if (!(complain & tf_error))
	    return error_mark_node;
	  else if (current_class_type
		   && TREE_OPERAND (arg, 0) == current_class_ref)
	    /* An expression like &memfn.  */
	    permerror (input_location, "ISO C++ forbids taking the address of an unqualified"
		       " or parenthesized non-static member function to form"
		       " a pointer to member function.  Say %<&%T::%D%>",
		       base, name);
	  else
	    permerror (input_location, "ISO C++ forbids taking the address of a bound member"
		       " function to form a pointer to member function."
		       "  Say %<&%T::%D%>",
		       base, name);
	}
      arg = build_offset_ref (base, fn, /*address_p=*/true, complain);
    }

  /* Uninstantiated types are all functions.  Taking the
     address of a function is a no-op, so just return the
     argument.  */
  if (type_unknown_p (arg))
    return build1 (ADDR_EXPR, unknown_type_node, arg);

  if (TREE_CODE (arg) == OFFSET_REF)
    /* We want a pointer to member; bypass all the code for actually taking
       the address of something.  */
    goto offset_ref;

  /* Anything not already handled and not a true memory reference
     is an error.  */
  if (TREE_CODE (argtype) != FUNCTION_TYPE
      && TREE_CODE (argtype) != METHOD_TYPE)
    {
      cp_lvalue_kind kind = lvalue_kind (arg);
      if (kind == clk_none)
	{
	  if (complain & tf_error)
	    lvalue_error (input_location, lv_addressof);
	  return error_mark_node;
	}
      if (strict_lvalue && (kind & (clk_rvalueref|clk_class)))
	{
	  if (!(complain & tf_error))
	    return error_mark_node;
	  if (kind & clk_class)
	    /* Make this a permerror because we used to accept it.  */
	    permerror (input_location, "taking address of temporary");
	  else
	    error ("taking address of xvalue (rvalue reference)");
	}
    }

  if (TREE_CODE (argtype) == REFERENCE_TYPE)
    {
      tree type = build_pointer_type (TREE_TYPE (argtype));
      arg = build1 (CONVERT_EXPR, type, arg);
      return arg;
    }
  else if (pedantic && DECL_MAIN_P (arg))
    {
      /* ARM $3.4 */
      /* Apparently a lot of autoconf scripts for C++ packages do this,
	 so only complain if -Wpedantic.  */
      if (complain & (flag_pedantic_errors ? tf_error : tf_warning))
	pedwarn (input_location, OPT_Wpedantic,
		 "ISO C++ forbids taking address of function %<::main%>");
      else if (flag_pedantic_errors)
	return error_mark_node;
    }

  /* Let &* cancel out to simplify resulting code.  */
  if (INDIRECT_REF_P (arg))
    {
      /* We don't need to have `current_class_ptr' wrapped in a
	 NON_LVALUE_EXPR node.  */
      if (arg == current_class_ref)
	return current_class_ptr;

      arg = TREE_OPERAND (arg, 0);
      if (TREE_CODE (TREE_TYPE (arg)) == REFERENCE_TYPE)
	{
	  tree type = build_pointer_type (TREE_TYPE (TREE_TYPE (arg)));
	  arg = build1 (CONVERT_EXPR, type, arg);
	}
      else
	/* Don't let this be an lvalue.  */
	arg = rvalue (arg);
      return arg;
    }

  /* ??? Cope with user tricks that amount to offsetof.  */
  if (TREE_CODE (argtype) != FUNCTION_TYPE
      && TREE_CODE (argtype) != METHOD_TYPE
      && argtype != unknown_type_node
      && (val = get_base_address (arg))
      && COMPLETE_TYPE_P (TREE_TYPE (val))
      && INDIRECT_REF_P (val)
      && TREE_CONSTANT (TREE_OPERAND (val, 0)))
    {
      tree type = build_pointer_type (argtype);
      return fold_convert (type, fold_offsetof_1 (arg));
    }

  /* Handle complex lvalues (when permitted)
     by reduction to simpler cases.  */
  val = unary_complex_lvalue (ADDR_EXPR, arg);
  if (val != 0)
    return val;

  switch (TREE_CODE (arg))
    {
    CASE_CONVERT:
    case FLOAT_EXPR:
    case FIX_TRUNC_EXPR:
      /* Even if we're not being pedantic, we cannot allow this
	 extension when we're instantiating in a SFINAE
	 context.  */
      if (! lvalue_p (arg) && complain == tf_none)
	{
	  if (complain & tf_error)
	    permerror (input_location, "ISO C++ forbids taking the address of a cast to a non-lvalue expression");
	  else
	    return error_mark_node;
	}
      break;

    case BASELINK:
      arg = BASELINK_FUNCTIONS (arg);
      /* Fall through.  */

    case OVERLOAD:
      arg = OVL_CURRENT (arg);
      break;

    case OFFSET_REF:
    offset_ref:
      /* Turn a reference to a non-static data member into a
	 pointer-to-member.  */
      {
	tree type;
	tree t;

	gcc_assert (PTRMEM_OK_P (arg));

	t = TREE_OPERAND (arg, 1);
	if (TREE_CODE (TREE_TYPE (t)) == REFERENCE_TYPE)
	  {
	    if (complain & tf_error)
	      error ("cannot create pointer to reference member %qD", t);
	    return error_mark_node;
	  }

	type = build_ptrmem_type (context_for_name_lookup (t),
				  TREE_TYPE (t));
	t = make_ptrmem_cst (type, TREE_OPERAND (arg, 1));
	return t;
      }

    default:
      break;
    }

  if (argtype != error_mark_node)
    {
      if (cxx_dialect >= cxx1y && array_of_runtime_bound_p (argtype))
	{
	  if (complain & tf_warning_or_error)
	    pedwarn (input_location, OPT_Wvla,
		     "taking address of array of runtime bound");
	  else
	    return error_mark_node;
	}
      argtype = build_pointer_type (argtype);
    }

  /* In a template, we are processing a non-dependent expression
     so we can just form an ADDR_EXPR with the correct type.  */
  if (processing_template_decl || TREE_CODE (arg) != COMPONENT_REF)
    {
      val = build_address (arg);
      if (TREE_CODE (arg) == OFFSET_REF)
	PTRMEM_OK_P (val) = PTRMEM_OK_P (arg);
    }
  else if (BASELINK_P (TREE_OPERAND (arg, 1)))
    {
      tree fn = BASELINK_FUNCTIONS (TREE_OPERAND (arg, 1));

      /* We can only get here with a single static member
	 function.  */
      gcc_assert (TREE_CODE (fn) == FUNCTION_DECL
		  && DECL_STATIC_FUNCTION_P (fn));
      mark_used (fn);
      val = build_address (fn);
      if (TREE_SIDE_EFFECTS (TREE_OPERAND (arg, 0)))
	/* Do not lose object's side effects.  */
	val = build2 (COMPOUND_EXPR, TREE_TYPE (val),
		      TREE_OPERAND (arg, 0), val);
    }
  else if (DECL_C_BIT_FIELD (TREE_OPERAND (arg, 1)))
    {
      if (complain & tf_error)
	error ("attempt to take address of bit-field structure member %qD",
	       TREE_OPERAND (arg, 1));
      return error_mark_node;
    }
  else
    {
      tree object = TREE_OPERAND (arg, 0);
      tree field = TREE_OPERAND (arg, 1);
      gcc_assert (same_type_ignoring_top_level_qualifiers_p
		  (TREE_TYPE (object), decl_type_context (field)));
      val = build_address (arg);
    }

  if (TYPE_PTR_P (argtype)
      && TREE_CODE (TREE_TYPE (argtype)) == METHOD_TYPE)
    {
      build_ptrmemfunc_type (argtype);
      val = build_ptrmemfunc (argtype, val, 0,
			      /*c_cast_p=*/false,
			      complain);
    }

  return val;
}

/* Take the address of ARG if it has one, even if it's an rvalue.  */

tree
cp_build_addr_expr (tree arg, tsubst_flags_t complain)
{
  return cp_build_addr_expr_1 (arg, 0, complain);
}

/* Take the address of ARG, but only if it's an lvalue.  */

tree
cp_build_addr_expr_strict (tree arg, tsubst_flags_t complain)
{
  return cp_build_addr_expr_1 (arg, 1, complain);
}

/* C++: Must handle pointers to members.

   Perhaps type instantiation should be extended to handle conversion
   from aggregates to types we don't yet know we want?  (Or are those
   cases typically errors which should be reported?)

   NOCONVERT nonzero suppresses the default promotions
   (such as from short to int).  */

tree
cp_build_unary_op (enum tree_code code, tree xarg, int noconvert, 
                   tsubst_flags_t complain)
{
  /* No default_conversion here.  It causes trouble for ADDR_EXPR.  */
  tree arg = xarg;
  tree argtype = 0;
  const char *errstring = NULL;
  tree val;
  const char *invalid_op_diag;

  if (!arg || error_operand_p (arg))
    return error_mark_node;

  if ((invalid_op_diag
       = targetm.invalid_unary_op ((code == UNARY_PLUS_EXPR
				    ? CONVERT_EXPR
				    : code),
				   TREE_TYPE (xarg))))
    {
      if (complain & tf_error)
	error (invalid_op_diag);
      return error_mark_node;
    }

  switch (code)
    {
    case UNARY_PLUS_EXPR:
    case NEGATE_EXPR:
      {
	int flags = WANT_ARITH | WANT_ENUM;
	/* Unary plus (but not unary minus) is allowed on pointers.  */
	if (code == UNARY_PLUS_EXPR)
	  flags |= WANT_POINTER;
	arg = build_expr_type_conversion (flags, arg, true);
	if (!arg)
	  errstring = (code == NEGATE_EXPR
		       ? _("wrong type argument to unary minus")
		       : _("wrong type argument to unary plus"));
	else
	  {
	    if (!noconvert && CP_INTEGRAL_TYPE_P (TREE_TYPE (arg)))
	      arg = cp_perform_integral_promotions (arg, complain);

	    /* Make sure the result is not an lvalue: a unary plus or minus
	       expression is always a rvalue.  */
	    arg = rvalue (arg);
	  }
      }
      break;

    case BIT_NOT_EXPR:
      if (TREE_CODE (TREE_TYPE (arg)) == COMPLEX_TYPE)
	{
	  code = CONJ_EXPR;
	  if (!noconvert)
	    {
	      arg = cp_default_conversion (arg, complain);
	      if (arg == error_mark_node)
		return error_mark_node;
	    }
	}
      else if (!(arg = build_expr_type_conversion (WANT_INT | WANT_ENUM
						   | WANT_VECTOR_OR_COMPLEX,
						   arg, true)))
	errstring = _("wrong type argument to bit-complement");
      else if (!noconvert && CP_INTEGRAL_TYPE_P (TREE_TYPE (arg)))
	arg = cp_perform_integral_promotions (arg, complain);
      break;

    case ABS_EXPR:
      if (!(arg = build_expr_type_conversion (WANT_ARITH | WANT_ENUM, arg, true)))
	errstring = _("wrong type argument to abs");
      else if (!noconvert)
	{
	  arg = cp_default_conversion (arg, complain);
	  if (arg == error_mark_node)
	    return error_mark_node;
	}
      break;

    case CONJ_EXPR:
      /* Conjugating a real value is a no-op, but allow it anyway.  */
      if (!(arg = build_expr_type_conversion (WANT_ARITH | WANT_ENUM, arg, true)))
	errstring = _("wrong type argument to conjugation");
      else if (!noconvert)
	{
	  arg = cp_default_conversion (arg, complain);
	  if (arg == error_mark_node)
	    return error_mark_node;
	}
      break;

    case TRUTH_NOT_EXPR:
      arg = perform_implicit_conversion (boolean_type_node, arg,
					 complain);
      val = invert_truthvalue_loc (input_location, arg);
      if (arg != error_mark_node)
	return val;
      errstring = _("in argument to unary !");
      break;

    case NOP_EXPR:
      break;

    case REALPART_EXPR:
    case IMAGPART_EXPR:
      arg = build_real_imag_expr (input_location, code, arg);
      if (arg == error_mark_node)
	return arg;
      else
	return fold_if_not_in_template (arg);

    case PREINCREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case POSTDECREMENT_EXPR:
      /* Handle complex lvalues (when permitted)
	 by reduction to simpler cases.  */

      val = unary_complex_lvalue (code, arg);
      if (val != 0)
	return val;

      arg = mark_lvalue_use (arg);

      /* Increment or decrement the real part of the value,
	 and don't change the imaginary part.  */
      if (TREE_CODE (TREE_TYPE (arg)) == COMPLEX_TYPE)
	{
	  tree real, imag;

	  arg = stabilize_reference (arg);
	  real = cp_build_unary_op (REALPART_EXPR, arg, 1, complain);
	  imag = cp_build_unary_op (IMAGPART_EXPR, arg, 1, complain);
	  real = cp_build_unary_op (code, real, 1, complain);
	  if (real == error_mark_node || imag == error_mark_node)
	    return error_mark_node;
	  return build2 (COMPLEX_EXPR, TREE_TYPE (arg),
			 real, imag);
	}

      /* Report invalid types.  */

      if (!(arg = build_expr_type_conversion (WANT_ARITH | WANT_POINTER,
					      arg, true)))
	{
	  if (code == PREINCREMENT_EXPR)
	    errstring = _("no pre-increment operator for type");
	  else if (code == POSTINCREMENT_EXPR)
	    errstring = _("no post-increment operator for type");
	  else if (code == PREDECREMENT_EXPR)
	    errstring = _("no pre-decrement operator for type");
	  else
	    errstring = _("no post-decrement operator for type");
	  break;
	}
      else if (arg == error_mark_node)
	return error_mark_node;

      /* Report something read-only.  */

      if (CP_TYPE_CONST_P (TREE_TYPE (arg))
	  || TREE_READONLY (arg)) 
        {
          if (complain & tf_error)
            cxx_readonly_error (arg, ((code == PREINCREMENT_EXPR
				      || code == POSTINCREMENT_EXPR)
				     ? lv_increment : lv_decrement));
          else
            return error_mark_node;
        }

      {
	tree inc;
	tree declared_type = unlowered_expr_type (arg);

	argtype = TREE_TYPE (arg);

	/* ARM $5.2.5 last annotation says this should be forbidden.  */
	if (TREE_CODE (argtype) == ENUMERAL_TYPE)
          {
            if (complain & tf_error)
              permerror (input_location, (code == PREINCREMENT_EXPR || code == POSTINCREMENT_EXPR)
                         ? G_("ISO C++ forbids incrementing an enum")
                         : G_("ISO C++ forbids decrementing an enum"));
            else
              return error_mark_node;
          }

	/* Compute the increment.  */

	if (TYPE_PTR_P (argtype))
	  {
	    tree type = complete_type (TREE_TYPE (argtype));

	    if (!COMPLETE_OR_VOID_TYPE_P (type))
              {
                if (complain & tf_error)
                  error (((code == PREINCREMENT_EXPR
                           || code == POSTINCREMENT_EXPR))
                         ? G_("cannot increment a pointer to incomplete type %qT")
                         : G_("cannot decrement a pointer to incomplete type %qT"),
                         TREE_TYPE (argtype));
                else
                  return error_mark_node;
              }
	    else if (!TYPE_PTROB_P (argtype)) 
              {
                if (complain & tf_error)
                  pedwarn (input_location, OPT_Wpointer_arith,
			   (code == PREINCREMENT_EXPR
                              || code == POSTINCREMENT_EXPR)
			   ? G_("ISO C++ forbids incrementing a pointer of type %qT")
			   : G_("ISO C++ forbids decrementing a pointer of type %qT"),
			   argtype);
                else
                  return error_mark_node;
              }

	    inc = cxx_sizeof_nowarn (TREE_TYPE (argtype));
	  }
	else
	  inc = VECTOR_TYPE_P (argtype)
	    ? build_one_cst (argtype)
	    : integer_one_node;

	inc = cp_convert (argtype, inc, complain);

	/* If 'arg' is an Objective-C PROPERTY_REF expression, then we
	   need to ask Objective-C to build the increment or decrement
	   expression for it.  */
	if (objc_is_property_ref (arg))
	  return objc_build_incr_expr_for_property_ref (input_location, code, 
							arg, inc);	

	/* Complain about anything else that is not a true lvalue.  */
	if (!lvalue_or_else (arg, ((code == PREINCREMENT_EXPR
				    || code == POSTINCREMENT_EXPR)
				   ? lv_increment : lv_decrement),
                             complain))
	  return error_mark_node;

	/* Forbid using -- on `bool'.  */
	if (TREE_CODE (declared_type) == BOOLEAN_TYPE)
	  {
	    if (code == POSTDECREMENT_EXPR || code == PREDECREMENT_EXPR)
	      {
                if (complain & tf_error)
                  error ("invalid use of Boolean expression as operand "
                         "to %<operator--%>");
		return error_mark_node;
	      }
	    val = boolean_increment (code, arg);
	  }
	else if (code == POSTINCREMENT_EXPR || code == POSTDECREMENT_EXPR)
	  /* An rvalue has no cv-qualifiers.  */
	  val = build2 (code, cv_unqualified (TREE_TYPE (arg)), arg, inc);
	else
	  val = build2 (code, TREE_TYPE (arg), arg, inc);

	TREE_SIDE_EFFECTS (val) = 1;
	return val;
      }

    case ADDR_EXPR:
      /* Note that this operation never does default_conversion
	 regardless of NOCONVERT.  */
      return cp_build_addr_expr (arg, complain);

    default:
      break;
    }

  if (!errstring)
    {
      if (argtype == 0)
	argtype = TREE_TYPE (arg);
      return fold_if_not_in_template (build1 (code, argtype, arg));
    }

  if (complain & tf_error)
    error ("%s", errstring);
  return error_mark_node;
}

/* Hook for the c-common bits that build a unary op.  */
tree
build_unary_op (location_t /*location*/,
		enum tree_code code, tree xarg, int noconvert)
{
  return cp_build_unary_op (code, xarg, noconvert, tf_warning_or_error);
}

/* Apply unary lvalue-demanding operator CODE to the expression ARG
   for certain kinds of expressions which are not really lvalues
   but which we can accept as lvalues.

   If ARG is not a kind of expression we can handle, return
   NULL_TREE.  */

tree
unary_complex_lvalue (enum tree_code code, tree arg)
{
  /* Inside a template, making these kinds of adjustments is
     pointless; we are only concerned with the type of the
     expression.  */
  if (processing_template_decl)
    return NULL_TREE;

  /* Handle (a, b) used as an "lvalue".  */
  if (TREE_CODE (arg) == COMPOUND_EXPR)
    {
      tree real_result = cp_build_unary_op (code, TREE_OPERAND (arg, 1), 0,
                                            tf_warning_or_error);
      return build2 (COMPOUND_EXPR, TREE_TYPE (real_result),
		     TREE_OPERAND (arg, 0), real_result);
    }

  /* Handle (a ? b : c) used as an "lvalue".  */
  if (TREE_CODE (arg) == COND_EXPR
      || TREE_CODE (arg) == MIN_EXPR || TREE_CODE (arg) == MAX_EXPR)
    return rationalize_conditional_expr (code, arg, tf_warning_or_error);

  /* Handle (a = b), (++a), and (--a) used as an "lvalue".  */
  if (TREE_CODE (arg) == MODIFY_EXPR
      || TREE_CODE (arg) == PREINCREMENT_EXPR
      || TREE_CODE (arg) == PREDECREMENT_EXPR)
    {
      tree lvalue = TREE_OPERAND (arg, 0);
      if (TREE_SIDE_EFFECTS (lvalue))
	{
	  lvalue = stabilize_reference (lvalue);
	  arg = build2 (TREE_CODE (arg), TREE_TYPE (arg),
			lvalue, TREE_OPERAND (arg, 1));
	}
      return unary_complex_lvalue
	(code, build2 (COMPOUND_EXPR, TREE_TYPE (lvalue), arg, lvalue));
    }

  if (code != ADDR_EXPR)
    return NULL_TREE;

  /* Handle (a = b) used as an "lvalue" for `&'.  */
  if (TREE_CODE (arg) == MODIFY_EXPR
      || TREE_CODE (arg) == INIT_EXPR)
    {
      tree real_result = cp_build_unary_op (code, TREE_OPERAND (arg, 0), 0,
                                            tf_warning_or_error);
      arg = build2 (COMPOUND_EXPR, TREE_TYPE (real_result),
		    arg, real_result);
      TREE_NO_WARNING (arg) = 1;
      return arg;
    }

  if (TREE_CODE (TREE_TYPE (arg)) == FUNCTION_TYPE
      || TREE_CODE (TREE_TYPE (arg)) == METHOD_TYPE
      || TREE_CODE (arg) == OFFSET_REF)
    return NULL_TREE;

  /* We permit compiler to make function calls returning
     objects of aggregate type look like lvalues.  */
  {
    tree targ = arg;

    if (TREE_CODE (targ) == SAVE_EXPR)
      targ = TREE_OPERAND (targ, 0);

    if (TREE_CODE (targ) == CALL_EXPR && MAYBE_CLASS_TYPE_P (TREE_TYPE (targ)))
      {
	if (TREE_CODE (arg) == SAVE_EXPR)
	  targ = arg;
	else
	  targ = build_cplus_new (TREE_TYPE (arg), arg, tf_warning_or_error);
	return build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (arg)), targ);
      }

    if (TREE_CODE (arg) == SAVE_EXPR && INDIRECT_REF_P (targ))
      return build3 (SAVE_EXPR, build_pointer_type (TREE_TYPE (arg)),
		     TREE_OPERAND (targ, 0), current_function_decl, NULL);
  }

  /* Don't let anything else be handled specially.  */
  return NULL_TREE;
}

/* Mark EXP saying that we need to be able to take the
   address of it; it should not be allocated in a register.
   Value is true if successful.

   C++: we do not allow `current_class_ptr' to be addressable.  */

bool
cxx_mark_addressable (tree exp)
{
  tree x = exp;

  while (1)
    switch (TREE_CODE (x))
      {
      case ADDR_EXPR:
      case COMPONENT_REF:
      case ARRAY_REF:
      case REALPART_EXPR:
      case IMAGPART_EXPR:
	x = TREE_OPERAND (x, 0);
	break;

      case PARM_DECL:
	if (x == current_class_ptr)
	  {
	    error ("cannot take the address of %<this%>, which is an rvalue expression");
	    TREE_ADDRESSABLE (x) = 1; /* so compiler doesn't die later.  */
	    return true;
	  }
	/* Fall through.  */

      case VAR_DECL:
	/* Caller should not be trying to mark initialized
	   constant fields addressable.  */
	gcc_assert (DECL_LANG_SPECIFIC (x) == 0
		    || DECL_IN_AGGR_P (x) == 0
		    || TREE_STATIC (x)
		    || DECL_EXTERNAL (x));
	/* Fall through.  */

      case RESULT_DECL:
	if (DECL_REGISTER (x) && !TREE_ADDRESSABLE (x)
	    && !DECL_ARTIFICIAL (x))
	  {
	    if (VAR_P (x) && DECL_HARD_REGISTER (x))
	      {
		error
		  ("address of explicit register variable %qD requested", x);
		return false;
	      }
	    else if (extra_warnings)
	      warning
		(OPT_Wextra, "address requested for %qD, which is declared %<register%>", x);
	  }
	TREE_ADDRESSABLE (x) = 1;
	return true;

      case CONST_DECL:
      case FUNCTION_DECL:
	TREE_ADDRESSABLE (x) = 1;
	return true;

      case CONSTRUCTOR:
	TREE_ADDRESSABLE (x) = 1;
	return true;

      case TARGET_EXPR:
	TREE_ADDRESSABLE (x) = 1;
	cxx_mark_addressable (TREE_OPERAND (x, 0));
	return true;

      default:
	return true;
    }
}

/* Build and return a conditional expression IFEXP ? OP1 : OP2.  */

tree
build_x_conditional_expr (location_t loc, tree ifexp, tree op1, tree op2, 
                          tsubst_flags_t complain)
{
  tree orig_ifexp = ifexp;
  tree orig_op1 = op1;
  tree orig_op2 = op2;
  tree expr;

  if (processing_template_decl)
    {
      /* The standard says that the expression is type-dependent if
	 IFEXP is type-dependent, even though the eventual type of the
	 expression doesn't dependent on IFEXP.  */
      if (type_dependent_expression_p (ifexp)
	  /* As a GNU extension, the middle operand may be omitted.  */
	  || (op1 && type_dependent_expression_p (op1))
	  || type_dependent_expression_p (op2))
	return build_min_nt_loc (loc, COND_EXPR, ifexp, op1, op2);
      ifexp = build_non_dependent_expr (ifexp);
      if (op1)
	op1 = build_non_dependent_expr (op1);
      op2 = build_non_dependent_expr (op2);
    }

  expr = build_conditional_expr (loc, ifexp, op1, op2, complain);
  if (processing_template_decl && expr != error_mark_node
      && TREE_CODE (expr) != VEC_COND_EXPR)
    {
      tree min = build_min_non_dep (COND_EXPR, expr,
				    orig_ifexp, orig_op1, orig_op2);
      /* In C++11, remember that the result is an lvalue or xvalue.
         In C++98, lvalue_kind can just assume lvalue in a template.  */
      if (cxx_dialect >= cxx11
	  && lvalue_or_rvalue_with_address_p (expr)
	  && !lvalue_or_rvalue_with_address_p (min))
	TREE_TYPE (min) = cp_build_reference_type (TREE_TYPE (min),
						   !real_lvalue_p (expr));
      expr = convert_from_reference (min);
    }
  return expr;
}

/* Given a list of expressions, return a compound expression
   that performs them all and returns the value of the last of them.  */

tree
build_x_compound_expr_from_list (tree list, expr_list_kind exp,
				 tsubst_flags_t complain)
{
  tree expr = TREE_VALUE (list);

  if (BRACE_ENCLOSED_INITIALIZER_P (expr)
      && !CONSTRUCTOR_IS_DIRECT_INIT (expr))
    {
      if (complain & tf_error)
	pedwarn (EXPR_LOC_OR_LOC (expr, input_location), 0,
		 "list-initializer for non-class type must not "
		 "be parenthesized");
      else
	return error_mark_node;
    }

  if (TREE_CHAIN (list))
    {
      if (complain & tf_error)
	switch (exp)
	  {
	  case ELK_INIT:
	    permerror (input_location, "expression list treated as compound "
				       "expression in initializer");
	    break;
	  case ELK_MEM_INIT:
	    permerror (input_location, "expression list treated as compound "
				       "expression in mem-initializer");
	    break;
	  case ELK_FUNC_CAST:
	    permerror (input_location, "expression list treated as compound "
				       "expression in functional cast");
	    break;
	  default:
	    gcc_unreachable ();
	  }
      else
	return error_mark_node;

      for (list = TREE_CHAIN (list); list; list = TREE_CHAIN (list))
	expr = build_x_compound_expr (EXPR_LOCATION (TREE_VALUE (list)),
				      expr, TREE_VALUE (list), complain);
    }

  return expr;
}

/* Like build_x_compound_expr_from_list, but using a VEC.  */

tree
build_x_compound_expr_from_vec (vec<tree, va_gc> *vec, const char *msg,
				tsubst_flags_t complain)
{
  if (vec_safe_is_empty (vec))
    return NULL_TREE;
  else if (vec->length () == 1)
    return (*vec)[0];
  else
    {
      tree expr;
      unsigned int ix;
      tree t;

      if (msg != NULL)
	{
	  if (complain & tf_error)
	    permerror (input_location,
		       "%s expression list treated as compound expression",
		       msg);
	  else
	    return error_mark_node;
	}

      expr = (*vec)[0];
      for (ix = 1; vec->iterate (ix, &t); ++ix)
	expr = build_x_compound_expr (EXPR_LOCATION (t), expr,
				      t, complain);

      return expr;
    }
}

/* Handle overloading of the ',' operator when needed.  */

tree
build_x_compound_expr (location_t loc, tree op1, tree op2,
		       tsubst_flags_t complain)
{
  tree result;
  tree orig_op1 = op1;
  tree orig_op2 = op2;

  if (processing_template_decl)
    {
      if (type_dependent_expression_p (op1)
	  || type_dependent_expression_p (op2))
	return build_min_nt_loc (loc, COMPOUND_EXPR, op1, op2);
      op1 = build_non_dependent_expr (op1);
      op2 = build_non_dependent_expr (op2);
    }

  result = build_new_op (loc, COMPOUND_EXPR, LOOKUP_NORMAL, op1, op2,
			 NULL_TREE, /*overload=*/NULL, complain);
  if (!result)
    result = cp_build_compound_expr (op1, op2, complain);

  if (processing_template_decl && result != error_mark_node)
    return build_min_non_dep (COMPOUND_EXPR, result, orig_op1, orig_op2);

  return result;
}

/* Like cp_build_compound_expr, but for the c-common bits.  */

tree
build_compound_expr (location_t /*loc*/, tree lhs, tree rhs)
{
  return cp_build_compound_expr (lhs, rhs, tf_warning_or_error);
}

/* Build a compound expression.  */

tree
cp_build_compound_expr (tree lhs, tree rhs, tsubst_flags_t complain)
{
  lhs = convert_to_void (lhs, ICV_LEFT_OF_COMMA, complain);

  if (lhs == error_mark_node || rhs == error_mark_node)
    return error_mark_node;

  if (flag_cilkplus
      && (TREE_CODE (lhs) == CILK_SPAWN_STMT
	  || TREE_CODE (rhs) == CILK_SPAWN_STMT))
    {
      location_t loc = (EXPR_HAS_LOCATION (lhs) ? EXPR_LOCATION (lhs)
			: EXPR_LOCATION (rhs));
      error_at (loc,
		"spawned function call cannot be part of a comma expression");
      return error_mark_node;
    }

  if (TREE_CODE (rhs) == TARGET_EXPR)
    {
      /* If the rhs is a TARGET_EXPR, then build the compound
	 expression inside the target_expr's initializer. This
	 helps the compiler to eliminate unnecessary temporaries.  */
      tree init = TREE_OPERAND (rhs, 1);

      init = build2 (COMPOUND_EXPR, TREE_TYPE (init), lhs, init);
      TREE_OPERAND (rhs, 1) = init;

      return rhs;
    }

  if (type_unknown_p (rhs))
    {
      if (complain & tf_error)
	error ("no context to resolve type of %qE", rhs);
      return error_mark_node;
    }
  
  return build2 (COMPOUND_EXPR, TREE_TYPE (rhs), lhs, rhs);
}

/* Issue a diagnostic message if casting from SRC_TYPE to DEST_TYPE
   casts away constness.  CAST gives the type of cast.  Returns true
   if the cast is ill-formed, false if it is well-formed.

   ??? This function warns for casting away any qualifier not just
   const.  We would like to specify exactly what qualifiers are casted
   away.
*/

static bool
check_for_casting_away_constness (tree src_type, tree dest_type,
				  enum tree_code cast, tsubst_flags_t complain)
{
  /* C-style casts are allowed to cast away constness.  With
     WARN_CAST_QUAL, we still want to issue a warning.  */
  if (cast == CAST_EXPR && !warn_cast_qual)
    return false;
  
  if (!casts_away_constness (src_type, dest_type, complain))
    return false;

  switch (cast)
    {
    case CAST_EXPR:
      if (complain & tf_warning)
	warning (OPT_Wcast_qual,
		 "cast from type %qT to type %qT casts away qualifiers",
		 src_type, dest_type);
      return false;
      
    case STATIC_CAST_EXPR:
      if (complain & tf_error)
	error ("static_cast from type %qT to type %qT casts away qualifiers",
	       src_type, dest_type);
      return true;
      
    case REINTERPRET_CAST_EXPR:
      if (complain & tf_error)
	error ("reinterpret_cast from type %qT to type %qT casts away qualifiers",
	       src_type, dest_type);
      return true;

    default:
      gcc_unreachable();
    }
}

/*
  Warns if the cast from expression EXPR to type TYPE is useless.
 */
void
maybe_warn_about_useless_cast (tree type, tree expr, tsubst_flags_t complain)
{
  if (warn_useless_cast
      && complain & tf_warning)
    {
      if (REFERENCE_REF_P (expr))
	expr = TREE_OPERAND (expr, 0);

      if ((TREE_CODE (type) == REFERENCE_TYPE
	   && (TYPE_REF_IS_RVALUE (type)
	       ? xvalue_p (expr) : real_lvalue_p (expr))
	   && same_type_p (TREE_TYPE (expr), TREE_TYPE (type)))
	  || same_type_p (TREE_TYPE (expr), type))
	warning (OPT_Wuseless_cast, "useless cast to type %qT", type);
    }
}

/* Convert EXPR (an expression with pointer-to-member type) to TYPE
   (another pointer-to-member type in the same hierarchy) and return
   the converted expression.  If ALLOW_INVERSE_P is permitted, a
   pointer-to-derived may be converted to pointer-to-base; otherwise,
   only the other direction is permitted.  If C_CAST_P is true, this
   conversion is taking place as part of a C-style cast.  */

tree
convert_ptrmem (tree type, tree expr, bool allow_inverse_p,
		bool c_cast_p, tsubst_flags_t complain)
{
  if (TYPE_PTRDATAMEM_P (type))
    {
      tree delta;

      if (TREE_CODE (expr) == PTRMEM_CST)
	expr = cplus_expand_constant (expr);
      delta = get_delta_difference (TYPE_PTRMEM_CLASS_TYPE (TREE_TYPE (expr)),
				    TYPE_PTRMEM_CLASS_TYPE (type),
				    allow_inverse_p,
				    c_cast_p, complain);
      if (delta == error_mark_node)
	return error_mark_node;

      if (!integer_zerop (delta))
	{
	  tree cond, op1, op2;

	  cond = cp_build_binary_op (input_location,
				     EQ_EXPR,
				     expr,
				     build_int_cst (TREE_TYPE (expr), -1),
				     complain);
	  op1 = build_nop (ptrdiff_type_node, expr);
	  op2 = cp_build_binary_op (input_location,
				    PLUS_EXPR, op1, delta,
				    complain);

	  expr = fold_build3_loc (input_location,
			      COND_EXPR, ptrdiff_type_node, cond, op1, op2);
			 
	}

      return build_nop (type, expr);
    }
  else
    return build_ptrmemfunc (TYPE_PTRMEMFUNC_FN_TYPE (type), expr,
			     allow_inverse_p, c_cast_p, complain);
}

/* Perform a static_cast from EXPR to TYPE.  When C_CAST_P is true,
   this static_cast is being attempted as one of the possible casts
   allowed by a C-style cast.  (In that case, accessibility of base
   classes is not considered, and it is OK to cast away
   constness.)  Return the result of the cast.  *VALID_P is set to
   indicate whether or not the cast was valid.  */

static tree
build_static_cast_1 (tree type, tree expr, bool c_cast_p,
		     bool *valid_p, tsubst_flags_t complain)
{
  tree intype;
  tree result;
  cp_lvalue_kind clk;

  /* Assume the cast is valid.  */
  *valid_p = true;

  intype = unlowered_expr_type (expr);

  /* Save casted types in the function's used types hash table.  */
  used_types_insert (type);

  /* [expr.static.cast]

     An lvalue of type "cv1 B", where B is a class type, can be cast
     to type "reference to cv2 D", where D is a class derived (clause
     _class.derived_) from B, if a valid standard conversion from
     "pointer to D" to "pointer to B" exists (_conv.ptr_), cv2 is the
     same cv-qualification as, or greater cv-qualification than, cv1,
     and B is not a virtual base class of D.  */
  /* We check this case before checking the validity of "TYPE t =
     EXPR;" below because for this case:

       struct B {};
       struct D : public B { D(const B&); };
       extern B& b;
       void f() { static_cast<const D&>(b); }

     we want to avoid constructing a new D.  The standard is not
     completely clear about this issue, but our interpretation is
     consistent with other compilers.  */
  if (TREE_CODE (type) == REFERENCE_TYPE
      && CLASS_TYPE_P (TREE_TYPE (type))
      && CLASS_TYPE_P (intype)
      && (TYPE_REF_IS_RVALUE (type) || real_lvalue_p (expr))
      && DERIVED_FROM_P (intype, TREE_TYPE (type))
      && can_convert (build_pointer_type (TYPE_MAIN_VARIANT (intype)),
		      build_pointer_type (TYPE_MAIN_VARIANT
					  (TREE_TYPE (type))),
		      complain)
      && (c_cast_p
	  || at_least_as_qualified_p (TREE_TYPE (type), intype)))
    {
      tree base;

      /* There is a standard conversion from "D*" to "B*" even if "B"
	 is ambiguous or inaccessible.  If this is really a
	 static_cast, then we check both for inaccessibility and
	 ambiguity.  However, if this is a static_cast being performed
	 because the user wrote a C-style cast, then accessibility is
	 not considered.  */
      base = lookup_base (TREE_TYPE (type), intype,
			  c_cast_p ? ba_unique : ba_check,
			  NULL, complain);

      /* Convert from "B*" to "D*".  This function will check that "B"
	 is not a virtual base of "D".  */
      expr = build_base_path (MINUS_EXPR, build_address (expr),
			      base, /*nonnull=*/false, complain);
      /* Convert the pointer to a reference -- but then remember that
	 there are no expressions with reference type in C++.

         We call rvalue so that there's an actual tree code
         (NON_LVALUE_EXPR) for the static_cast; otherwise, if the operand
         is a variable with the same type, the conversion would get folded
         away, leaving just the variable and causing lvalue_kind to give
         the wrong answer.  */
      return convert_from_reference (rvalue (cp_fold_convert (type, expr)));
    }

  /* "A glvalue of type cv1 T1 can be cast to type rvalue reference to
     cv2 T2 if cv2 T2 is reference-compatible with cv1 T1 (8.5.3)."  */
  if (TREE_CODE (type) == REFERENCE_TYPE
      && TYPE_REF_IS_RVALUE (type)
      && (clk = real_lvalue_p (expr))
      && reference_related_p (TREE_TYPE (type), intype)
      && (c_cast_p || at_least_as_qualified_p (TREE_TYPE (type), intype)))
    {
      if (clk == clk_ordinary)
	{
	  /* Handle the (non-bit-field) lvalue case here by casting to
	     lvalue reference and then changing it to an rvalue reference.
	     Casting an xvalue to rvalue reference will be handled by the
	     main code path.  */
	  tree lref = cp_build_reference_type (TREE_TYPE (type), false);
	  result = (perform_direct_initialization_if_possible
		    (lref, expr, c_cast_p, complain));
	  result = cp_fold_convert (type, result);
	  /* Make sure we don't fold back down to a named rvalue reference,
	     because that would be an lvalue.  */
	  if (DECL_P (result))
	    result = build1 (NON_LVALUE_EXPR, type, result);
	  return convert_from_reference (result);
	}
      else
	/* For a bit-field or packed field, bind to a temporary.  */
	expr = rvalue (expr);
    }

  /* Resolve overloaded address here rather than once in
     implicit_conversion and again in the inverse code below.  */
  if (TYPE_PTRMEMFUNC_P (type) && type_unknown_p (expr))
    {
      expr = instantiate_type (type, expr, complain);
      intype = TREE_TYPE (expr);
    }

  /* [expr.static.cast]

     Any expression can be explicitly converted to type cv void.  */
  if (VOID_TYPE_P (type))
    return convert_to_void (expr, ICV_CAST, complain);

  /* [class.abstract]
     An abstract class shall not be used ... as the type of an explicit
     conversion.  */
  if (abstract_virtuals_error_sfinae (ACU_CAST, type, complain))
    return error_mark_node;

  /* [expr.static.cast]

     An expression e can be explicitly converted to a type T using a
     static_cast of the form static_cast<T>(e) if the declaration T
     t(e);" is well-formed, for some invented temporary variable
     t.  */
  result = perform_direct_initialization_if_possible (type, expr,
						      c_cast_p, complain);
  if (result)
    {
      result = convert_from_reference (result);

      /* [expr.static.cast]

	 If T is a reference type, the result is an lvalue; otherwise,
	 the result is an rvalue.  */
      if (TREE_CODE (type) != REFERENCE_TYPE)
	result = rvalue (result);
      return result;
    }

  /* [expr.static.cast]

     The inverse of any standard conversion sequence (clause _conv_),
     other than the lvalue-to-rvalue (_conv.lval_), array-to-pointer
     (_conv.array_), function-to-pointer (_conv.func_), and boolean
     (_conv.bool_) conversions, can be performed explicitly using
     static_cast subject to the restriction that the explicit
     conversion does not cast away constness (_expr.const.cast_), and
     the following additional rules for specific cases:  */
  /* For reference, the conversions not excluded are: integral
     promotions, floating point promotion, integral conversions,
     floating point conversions, floating-integral conversions,
     pointer conversions, and pointer to member conversions.  */
  /* DR 128

     A value of integral _or enumeration_ type can be explicitly
     converted to an enumeration type.  */
  /* The effect of all that is that any conversion between any two
     types which are integral, floating, or enumeration types can be
     performed.  */
  if ((INTEGRAL_OR_ENUMERATION_TYPE_P (type)
       || SCALAR_FLOAT_TYPE_P (type))
      && (INTEGRAL_OR_ENUMERATION_TYPE_P (intype)
	  || SCALAR_FLOAT_TYPE_P (intype)))
    return ocp_convert (type, expr, CONV_C_CAST, LOOKUP_NORMAL, complain);

  if (TYPE_PTR_P (type) && TYPE_PTR_P (intype)
      && CLASS_TYPE_P (TREE_TYPE (type))
      && CLASS_TYPE_P (TREE_TYPE (intype))
      && can_convert (build_pointer_type (TYPE_MAIN_VARIANT
					  (TREE_TYPE (intype))),
		      build_pointer_type (TYPE_MAIN_VARIANT
					  (TREE_TYPE (type))),
		      complain))
    {
      tree base;

      if (!c_cast_p
	  && check_for_casting_away_constness (intype, type, STATIC_CAST_EXPR,
					       complain))
	return error_mark_node;
      base = lookup_base (TREE_TYPE (type), TREE_TYPE (intype),
			  c_cast_p ? ba_unique : ba_check,
			  NULL, complain);
      expr = build_base_path (MINUS_EXPR, expr, base, /*nonnull=*/false,
			      complain);
      return cp_fold_convert(type, expr);
    }

  if ((TYPE_PTRDATAMEM_P (type) && TYPE_PTRDATAMEM_P (intype))
      || (TYPE_PTRMEMFUNC_P (type) && TYPE_PTRMEMFUNC_P (intype)))
    {
      tree c1;
      tree c2;
      tree t1;
      tree t2;

      c1 = TYPE_PTRMEM_CLASS_TYPE (intype);
      c2 = TYPE_PTRMEM_CLASS_TYPE (type);

      if (TYPE_PTRDATAMEM_P (type))
	{
	  t1 = (build_ptrmem_type
		(c1,
		 TYPE_MAIN_VARIANT (TYPE_PTRMEM_POINTED_TO_TYPE (intype))));
	  t2 = (build_ptrmem_type
		(c2,
		 TYPE_MAIN_VARIANT (TYPE_PTRMEM_POINTED_TO_TYPE (type))));
	}
      else
	{
	  t1 = intype;
	  t2 = type;
	}
      if (can_convert (t1, t2, complain) || can_convert (t2, t1, complain))
	{
	  if (!c_cast_p
	      && check_for_casting_away_constness (intype, type,
						   STATIC_CAST_EXPR,
						   complain))
	    return error_mark_node;
	  return convert_ptrmem (type, expr, /*allow_inverse_p=*/1,
				 c_cast_p, complain);
	}
    }

  /* [expr.static.cast]

     An rvalue of type "pointer to cv void" can be explicitly
     converted to a pointer to object type.  A value of type pointer
     to object converted to "pointer to cv void" and back to the
     original pointer type will have its original value.  */
  if (TYPE_PTR_P (intype)
      && VOID_TYPE_P (TREE_TYPE (intype))
      && TYPE_PTROB_P (type))
    {
      if (!c_cast_p
	  && check_for_casting_away_constness (intype, type, STATIC_CAST_EXPR,
					       complain))
	return error_mark_node;
      return build_nop (type, expr);
    }

  *valid_p = false;
  return error_mark_node;
}

/* Return an expression representing static_cast<TYPE>(EXPR).  */

tree
build_static_cast (tree type, tree expr, tsubst_flags_t complain)
{
  tree result;
  bool valid_p;

  if (type == error_mark_node || expr == error_mark_node)
    return error_mark_node;

  if (processing_template_decl)
    {
      expr = build_min (STATIC_CAST_EXPR, type, expr);
      /* We don't know if it will or will not have side effects.  */
      TREE_SIDE_EFFECTS (expr) = 1;
      return convert_from_reference (expr);
    }

  /* build_c_cast puts on a NOP_EXPR to make the result not an lvalue.
     Strip such NOP_EXPRs if VALUE is being used in non-lvalue context.  */
  if (TREE_CODE (type) != REFERENCE_TYPE
      && TREE_CODE (expr) == NOP_EXPR
      && TREE_TYPE (expr) == TREE_TYPE (TREE_OPERAND (expr, 0)))
    expr = TREE_OPERAND (expr, 0);

  result = build_static_cast_1 (type, expr, /*c_cast_p=*/false, &valid_p,
                                complain);
  if (valid_p)
    {
      if (result != error_mark_node)
	maybe_warn_about_useless_cast (type, expr, complain);
      return result;
    }

  if (complain & tf_error)
    error ("invalid static_cast from type %qT to type %qT",
           TREE_TYPE (expr), type);
  return error_mark_node;
}

/* EXPR is an expression with member function or pointer-to-member
   function type.  TYPE is a pointer type.  Converting EXPR to TYPE is
   not permitted by ISO C++, but we accept it in some modes.  If we
   are not in one of those modes, issue a diagnostic.  Return the
   converted expression.  */

tree
convert_member_func_to_ptr (tree type, tree expr, tsubst_flags_t complain)
{
  tree intype;
  tree decl;

  intype = TREE_TYPE (expr);
  gcc_assert (TYPE_PTRMEMFUNC_P (intype)
	      || TREE_CODE (intype) == METHOD_TYPE);

  if (!(complain & tf_warning_or_error))
    return error_mark_node;

  if (pedantic || warn_pmf2ptr)
    pedwarn (input_location, pedantic ? OPT_Wpedantic : OPT_Wpmf_conversions,
	     "converting from %qT to %qT", intype, type);

  if (TREE_CODE (intype) == METHOD_TYPE)
    expr = build_addr_func (expr, complain);
  else if (TREE_CODE (expr) == PTRMEM_CST)
    expr = build_address (PTRMEM_CST_MEMBER (expr));
  else
    {
      decl = maybe_dummy_object (TYPE_PTRMEM_CLASS_TYPE (intype), 0);
      decl = build_address (decl);
      expr = get_member_function_from_ptrfunc (&decl, expr, complain);
    }

  if (expr == error_mark_node)
    return error_mark_node;

  return build_nop (type, expr);
}

/* Return a representation for a reinterpret_cast from EXPR to TYPE.
   If C_CAST_P is true, this reinterpret cast is being done as part of
   a C-style cast.  If VALID_P is non-NULL, *VALID_P is set to
   indicate whether or not reinterpret_cast was valid.  */

static tree
build_reinterpret_cast_1 (tree type, tree expr, bool c_cast_p,
			  bool *valid_p, tsubst_flags_t complain)
{
  tree intype;

  /* Assume the cast is invalid.  */
  if (valid_p)
    *valid_p = true;

  if (type == error_mark_node || error_operand_p (expr))
    return error_mark_node;

  intype = TREE_TYPE (expr);

  /* Save casted types in the function's used types hash table.  */
  used_types_insert (type);

  /* [expr.reinterpret.cast]
     An lvalue expression of type T1 can be cast to the type
     "reference to T2" if an expression of type "pointer to T1" can be
     explicitly converted to the type "pointer to T2" using a
     reinterpret_cast.  */
  if (TREE_CODE (type) == REFERENCE_TYPE)
    {
      if (! real_lvalue_p (expr))
	{
          if (complain & tf_error)
            error ("invalid cast of an rvalue expression of type "
                   "%qT to type %qT",
                   intype, type);
	  return error_mark_node;
	}

      /* Warn about a reinterpret_cast from "A*" to "B&" if "A" and
	 "B" are related class types; the reinterpret_cast does not
	 adjust the pointer.  */
      if (TYPE_PTR_P (intype)
          && (complain & tf_warning)
	  && (comptypes (TREE_TYPE (intype), TREE_TYPE (type),
			 COMPARE_BASE | COMPARE_DERIVED)))
	warning (0, "casting %qT to %qT does not dereference pointer",
		 intype, type);

      expr = cp_build_addr_expr (expr, complain);

      if (warn_strict_aliasing > 2)
	strict_aliasing_warning (TREE_TYPE (expr), type, expr);

      if (expr != error_mark_node)
	expr = build_reinterpret_cast_1
	  (build_pointer_type (TREE_TYPE (type)), expr, c_cast_p,
	   valid_p, complain);
      if (expr != error_mark_node)
	/* cp_build_indirect_ref isn't right for rvalue refs.  */
	expr = convert_from_reference (fold_convert (type, expr));
      return expr;
    }

  /* As a G++ extension, we consider conversions from member
     functions, and pointers to member functions to
     pointer-to-function and pointer-to-void types.  If
     -Wno-pmf-conversions has not been specified,
     convert_member_func_to_ptr will issue an error message.  */
  if ((TYPE_PTRMEMFUNC_P (intype)
       || TREE_CODE (intype) == METHOD_TYPE)
      && TYPE_PTR_P (type)
      && (TREE_CODE (TREE_TYPE (type)) == FUNCTION_TYPE
	  || VOID_TYPE_P (TREE_TYPE (type))))
    return convert_member_func_to_ptr (type, expr, complain);

  /* If the cast is not to a reference type, the lvalue-to-rvalue,
     array-to-pointer, and function-to-pointer conversions are
     performed.  */
  expr = decay_conversion (expr, complain);

  /* build_c_cast puts on a NOP_EXPR to make the result not an lvalue.
     Strip such NOP_EXPRs if VALUE is being used in non-lvalue context.  */
  if (TREE_CODE (expr) == NOP_EXPR
      && TREE_TYPE (expr) == TREE_TYPE (TREE_OPERAND (expr, 0)))
    expr = TREE_OPERAND (expr, 0);

  if (error_operand_p (expr))
    return error_mark_node;

  intype = TREE_TYPE (expr);

  /* [expr.reinterpret.cast]
     A pointer can be converted to any integral type large enough to
     hold it. ... A value of type std::nullptr_t can be converted to
     an integral type; the conversion has the same meaning and
     validity as a conversion of (void*)0 to the integral type.  */
  if (CP_INTEGRAL_TYPE_P (type)
      && (TYPE_PTR_P (intype) || NULLPTR_TYPE_P (intype)))
    {
      if (TYPE_PRECISION (type) < TYPE_PRECISION (intype))
        {
          if (complain & tf_error)
            permerror (input_location, "cast from %qT to %qT loses precision",
                       intype, type);
          else
            return error_mark_node;
        }
      if (NULLPTR_TYPE_P (intype))
        return build_int_cst (type, 0);
    }
  /* [expr.reinterpret.cast]
     A value of integral or enumeration type can be explicitly
     converted to a pointer.  */
  else if (TYPE_PTR_P (type) && INTEGRAL_OR_ENUMERATION_TYPE_P (intype))
    /* OK */
    ;
  else if ((INTEGRAL_OR_ENUMERATION_TYPE_P (type)
	    || TYPE_PTR_OR_PTRMEM_P (type))
	   && same_type_p (type, intype))
    /* DR 799 */
    return fold_if_not_in_template (build_nop (type, expr));
  else if ((TYPE_PTRFN_P (type) && TYPE_PTRFN_P (intype))
	   || (TYPE_PTRMEMFUNC_P (type) && TYPE_PTRMEMFUNC_P (intype)))
    return fold_if_not_in_template (build_nop (type, expr));
  else if ((TYPE_PTRDATAMEM_P (type) && TYPE_PTRDATAMEM_P (intype))
	   || (TYPE_PTROBV_P (type) && TYPE_PTROBV_P (intype)))
    {
      tree sexpr = expr;

      if (!c_cast_p
	  && check_for_casting_away_constness (intype, type,
					       REINTERPRET_CAST_EXPR,
					       complain))
	return error_mark_node;
      /* Warn about possible alignment problems.  */
      if (STRICT_ALIGNMENT && warn_cast_align
          && (complain & tf_warning)
	  && !VOID_TYPE_P (type)
	  && TREE_CODE (TREE_TYPE (intype)) != FUNCTION_TYPE
	  && COMPLETE_TYPE_P (TREE_TYPE (type))
	  && COMPLETE_TYPE_P (TREE_TYPE (intype))
	  && TYPE_ALIGN (TREE_TYPE (type)) > TYPE_ALIGN (TREE_TYPE (intype)))
	warning (OPT_Wcast_align, "cast from %qT to %qT "
                 "increases required alignment of target type", intype, type);

      /* We need to strip nops here, because the front end likes to
	 create (int *)&a for array-to-pointer decay, instead of &a[0].  */
      STRIP_NOPS (sexpr);
      if (warn_strict_aliasing <= 2)
	strict_aliasing_warning (intype, type, sexpr);

      return fold_if_not_in_template (build_nop (type, expr));
    }
  else if ((TYPE_PTRFN_P (type) && TYPE_PTROBV_P (intype))
	   || (TYPE_PTRFN_P (intype) && TYPE_PTROBV_P (type)))
    {
      if (complain & tf_warning)
	/* C++11 5.2.10 p8 says that "Converting a function pointer to an
	   object pointer type or vice versa is conditionally-supported."  */
	warning (OPT_Wconditionally_supported,
		 "casting between pointer-to-function and pointer-to-object "
		 "is conditionally-supported");
      return fold_if_not_in_template (build_nop (type, expr));
    }
  else if (TREE_CODE (type) == VECTOR_TYPE)
    return fold_if_not_in_template (convert_to_vector (type, expr));
  else if (TREE_CODE (intype) == VECTOR_TYPE
	   && INTEGRAL_OR_ENUMERATION_TYPE_P (type))
    return fold_if_not_in_template (convert_to_integer (type, expr));
  else
    {
      if (valid_p)
	*valid_p = false;
      if (complain & tf_error)
        error ("invalid cast from type %qT to type %qT", intype, type);
      return error_mark_node;
    }

  return cp_convert (type, expr, complain);
}

tree
build_reinterpret_cast (tree type, tree expr, tsubst_flags_t complain)
{
  tree r;

  if (type == error_mark_node || expr == error_mark_node)
    return error_mark_node;

  if (processing_template_decl)
    {
      tree t = build_min (REINTERPRET_CAST_EXPR, type, expr);

      if (!TREE_SIDE_EFFECTS (t)
	  && type_dependent_expression_p (expr))
	/* There might turn out to be side effects inside expr.  */
	TREE_SIDE_EFFECTS (t) = 1;
      return convert_from_reference (t);
    }

  r = build_reinterpret_cast_1 (type, expr, /*c_cast_p=*/false,
				/*valid_p=*/NULL, complain);
  if (r != error_mark_node)
    maybe_warn_about_useless_cast (type, expr, complain);
  return r;
}

/* Perform a const_cast from EXPR to TYPE.  If the cast is valid,
   return an appropriate expression.  Otherwise, return
   error_mark_node.  If the cast is not valid, and COMPLAIN is true,
   then a diagnostic will be issued.  If VALID_P is non-NULL, we are
   performing a C-style cast, its value upon return will indicate
   whether or not the conversion succeeded.  */

static tree
build_const_cast_1 (tree dst_type, tree expr, tsubst_flags_t complain,
		    bool *valid_p)
{
  tree src_type;
  tree reference_type;

  /* Callers are responsible for handling error_mark_node as a
     destination type.  */
  gcc_assert (dst_type != error_mark_node);
  /* In a template, callers should be building syntactic
     representations of casts, not using this machinery.  */
  gcc_assert (!processing_template_decl);

  /* Assume the conversion is invalid.  */
  if (valid_p)
    *valid_p = false;

  if (!POINTER_TYPE_P (dst_type) && !TYPE_PTRDATAMEM_P (dst_type))
    {
      if (complain & tf_error)
	error ("invalid use of const_cast with type %qT, "
	       "which is not a pointer, "
	       "reference, nor a pointer-to-data-member type", dst_type);
      return error_mark_node;
    }

  if (TREE_CODE (TREE_TYPE (dst_type)) == FUNCTION_TYPE)
    {
      if (complain & tf_error)
	error ("invalid use of const_cast with type %qT, which is a pointer "
	       "or reference to a function type", dst_type);
      return error_mark_node;
    }

  /* Save casted types in the function's used types hash table.  */
  used_types_insert (dst_type);

  src_type = TREE_TYPE (expr);
  /* Expressions do not really have reference types.  */
  if (TREE_CODE (src_type) == REFERENCE_TYPE)
    src_type = TREE_TYPE (src_type);

  /* [expr.const.cast]

     For two object types T1 and T2, if a pointer to T1 can be explicitly
     converted to the type "pointer to T2" using a const_cast, then the
     following conversions can also be made:

     -- an lvalue of type T1 can be explicitly converted to an lvalue of
     type T2 using the cast const_cast<T2&>;

     -- a glvalue of type T1 can be explicitly converted to an xvalue of
     type T2 using the cast const_cast<T2&&>; and

     -- if T1 is a class type, a prvalue of type T1 can be explicitly
     converted to an xvalue of type T2 using the cast const_cast<T2&&>.  */

  if (TREE_CODE (dst_type) == REFERENCE_TYPE)
    {
      reference_type = dst_type;
      if (!TYPE_REF_IS_RVALUE (dst_type)
	  ? real_lvalue_p (expr)
	  : (CLASS_TYPE_P (TREE_TYPE (dst_type))
	     ? lvalue_p (expr)
	     : lvalue_or_rvalue_with_address_p (expr)))
	/* OK.  */;
      else
	{
	  if (complain & tf_error)
	    error ("invalid const_cast of an rvalue of type %qT to type %qT",
		   src_type, dst_type);
	  return error_mark_node;
	}
      dst_type = build_pointer_type (TREE_TYPE (dst_type));
      src_type = build_pointer_type (src_type);
    }
  else
    {
      reference_type = NULL_TREE;
      /* If the destination type is not a reference type, the
	 lvalue-to-rvalue, array-to-pointer, and function-to-pointer
	 conversions are performed.  */
      src_type = type_decays_to (src_type);
      if (src_type == error_mark_node)
	return error_mark_node;
    }

  if (TYPE_PTR_P (src_type) || TYPE_PTRDATAMEM_P (src_type))
    {
      if (comp_ptr_ttypes_const (dst_type, src_type))
	{
	  if (valid_p)
	    {
	      *valid_p = true;
	      /* This cast is actually a C-style cast.  Issue a warning if
		 the user is making a potentially unsafe cast.  */
	      check_for_casting_away_constness (src_type, dst_type,
						CAST_EXPR, complain);
	    }
	  if (reference_type)
	    {
	      expr = cp_build_addr_expr (expr, complain);
	      if (expr == error_mark_node)
		return error_mark_node;
	      expr = build_nop (reference_type, expr);
	      return convert_from_reference (expr);
	    }
	  else
	    {
	      expr = decay_conversion (expr, complain);
	      if (expr == error_mark_node)
		return error_mark_node;

	      /* build_c_cast puts on a NOP_EXPR to make the result not an
		 lvalue.  Strip such NOP_EXPRs if VALUE is being used in
		 non-lvalue context.  */
	      if (TREE_CODE (expr) == NOP_EXPR
		  && TREE_TYPE (expr) == TREE_TYPE (TREE_OPERAND (expr, 0)))
		expr = TREE_OPERAND (expr, 0);
	      return build_nop (dst_type, expr);
	    }
	}
      else if (valid_p
	       && !at_least_as_qualified_p (TREE_TYPE (dst_type),
					    TREE_TYPE (src_type)))
	check_for_casting_away_constness (src_type, dst_type, CAST_EXPR,
					  complain);
    }

  if (complain & tf_error)
    error ("invalid const_cast from type %qT to type %qT",
	   src_type, dst_type);
  return error_mark_node;
}

tree
build_const_cast (tree type, tree expr, tsubst_flags_t complain)
{
  tree r;

  if (type == error_mark_node || error_operand_p (expr))
    return error_mark_node;

  if (processing_template_decl)
    {
      tree t = build_min (CONST_CAST_EXPR, type, expr);

      if (!TREE_SIDE_EFFECTS (t)
	  && type_dependent_expression_p (expr))
	/* There might turn out to be side effects inside expr.  */
	TREE_SIDE_EFFECTS (t) = 1;
      return convert_from_reference (t);
    }

  r = build_const_cast_1 (type, expr, complain, /*valid_p=*/NULL);
  if (r != error_mark_node)
    maybe_warn_about_useless_cast (type, expr, complain);
  return r;
}

/* Like cp_build_c_cast, but for the c-common bits.  */

tree
build_c_cast (location_t /*loc*/, tree type, tree expr)
{
  return cp_build_c_cast (type, expr, tf_warning_or_error);
}

/* Build an expression representing an explicit C-style cast to type
   TYPE of expression EXPR.  */

tree
cp_build_c_cast (tree type, tree expr, tsubst_flags_t complain)
{
  tree value = expr;
  tree result;
  bool valid_p;

  if (type == error_mark_node || error_operand_p (expr))
    return error_mark_node;

  if (processing_template_decl)
    {
      tree t = build_min (CAST_EXPR, type,
			  tree_cons (NULL_TREE, value, NULL_TREE));
      /* We don't know if it will or will not have side effects.  */
      TREE_SIDE_EFFECTS (t) = 1;
      return convert_from_reference (t);
    }

  /* Casts to a (pointer to a) specific ObjC class (or 'id' or
     'Class') should always be retained, because this information aids
     in method lookup.  */
  if (objc_is_object_ptr (type)
      && objc_is_object_ptr (TREE_TYPE (expr)))
    return build_nop (type, expr);

  /* build_c_cast puts on a NOP_EXPR to make the result not an lvalue.
     Strip such NOP_EXPRs if VALUE is being used in non-lvalue context.  */
  if (TREE_CODE (type) != REFERENCE_TYPE
      && TREE_CODE (value) == NOP_EXPR
      && TREE_TYPE (value) == TREE_TYPE (TREE_OPERAND (value, 0)))
    value = TREE_OPERAND (value, 0);

  if (TREE_CODE (type) == ARRAY_TYPE)
    {
      /* Allow casting from T1* to T2[] because Cfront allows it.
	 NIHCL uses it. It is not valid ISO C++ however.  */
      if (TYPE_PTR_P (TREE_TYPE (expr)))
	{
          if (complain & tf_error)
            permerror (input_location, "ISO C++ forbids casting to an array type %qT", type);
          else
            return error_mark_node;
	  type = build_pointer_type (TREE_TYPE (type));
	}
      else
	{
          if (complain & tf_error)
            error ("ISO C++ forbids casting to an array type %qT", type);
	  return error_mark_node;
	}
    }

  if (TREE_CODE (type) == FUNCTION_TYPE
      || TREE_CODE (type) == METHOD_TYPE)
    {
      if (complain & tf_error)
        error ("invalid cast to function type %qT", type);
      return error_mark_node;
    }

  if (TYPE_PTR_P (type)
      && TREE_CODE (TREE_TYPE (value)) == INTEGER_TYPE
      /* Casting to an integer of smaller size is an error detected elsewhere.  */
      && TYPE_PRECISION (type) > TYPE_PRECISION (TREE_TYPE (value))
      /* Don't warn about converting any constant.  */
      && !TREE_CONSTANT (value))
    warning_at (input_location, OPT_Wint_to_pointer_cast, 
		"cast to pointer from integer of different size");

  /* A C-style cast can be a const_cast.  */
  result = build_const_cast_1 (type, value, complain & tf_warning,
			       &valid_p);
  if (valid_p)
    {
      if (result != error_mark_node)
	maybe_warn_about_useless_cast (type, value, complain);
      return result;
    }

  /* Or a static cast.  */
  result = build_static_cast_1 (type, value, /*c_cast_p=*/true,
				&valid_p, complain);
  /* Or a reinterpret_cast.  */
  if (!valid_p)
    result = build_reinterpret_cast_1 (type, value, /*c_cast_p=*/true,
				       &valid_p, complain);
  /* The static_cast or reinterpret_cast may be followed by a
     const_cast.  */
  if (valid_p
      /* A valid cast may result in errors if, for example, a
	 conversion to an ambiguous base class is required.  */
      && !error_operand_p (result))
    {
      tree result_type;

      maybe_warn_about_useless_cast (type, value, complain);

      /* Non-class rvalues always have cv-unqualified type.  */
      if (!CLASS_TYPE_P (type))
	type = TYPE_MAIN_VARIANT (type);
      result_type = TREE_TYPE (result);
      if (!CLASS_TYPE_P (result_type) && TREE_CODE (type) != REFERENCE_TYPE)
	result_type = TYPE_MAIN_VARIANT (result_type);
      /* If the type of RESULT does not match TYPE, perform a
	 const_cast to make it match.  If the static_cast or
	 reinterpret_cast succeeded, we will differ by at most
	 cv-qualification, so the follow-on const_cast is guaranteed
	 to succeed.  */
      if (!same_type_p (non_reference (type), non_reference (result_type)))
	{
	  result = build_const_cast_1 (type, result, false, &valid_p);
	  gcc_assert (valid_p);
	}
      return result;
    }

  return error_mark_node;
}

/* For use from the C common bits.  */
tree
build_modify_expr (location_t /*location*/,
		   tree lhs, tree /*lhs_origtype*/,
		   enum tree_code modifycode, 
		   location_t /*rhs_location*/, tree rhs,
		   tree /*rhs_origtype*/)
{
  return cp_build_modify_expr (lhs, modifycode, rhs, tf_warning_or_error);
}

/* Build an assignment expression of lvalue LHS from value RHS.
   MODIFYCODE is the code for a binary operator that we use
   to combine the old value of LHS with RHS to get the new value.
   Or else MODIFYCODE is NOP_EXPR meaning do a simple assignment.

   C++: If MODIFYCODE is INIT_EXPR, then leave references unbashed.  */

tree
cp_build_modify_expr (tree lhs, enum tree_code modifycode, tree rhs,
		      tsubst_flags_t complain)
{
  tree result;
  tree newrhs = rhs;
  tree lhstype = TREE_TYPE (lhs);
  tree olhstype = lhstype;
  bool plain_assign = (modifycode == NOP_EXPR);

  /* Avoid duplicate error messages from operands that had errors.  */
  if (error_operand_p (lhs) || error_operand_p (rhs))
    return error_mark_node;

  /* Handle control structure constructs used as "lvalues".  */
  switch (TREE_CODE (lhs))
    {
      /* Handle --foo = 5; as these are valid constructs in C++.  */
    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
      if (TREE_SIDE_EFFECTS (TREE_OPERAND (lhs, 0)))
	lhs = build2 (TREE_CODE (lhs), TREE_TYPE (lhs),
		      stabilize_reference (TREE_OPERAND (lhs, 0)),
		      TREE_OPERAND (lhs, 1));
      newrhs = cp_build_modify_expr (TREE_OPERAND (lhs, 0),
				     modifycode, rhs, complain);
      if (newrhs == error_mark_node)
	return error_mark_node;
      return build2 (COMPOUND_EXPR, lhstype, lhs, newrhs);

      /* Handle (a, b) used as an "lvalue".  */
    case COMPOUND_EXPR:
      newrhs = cp_build_modify_expr (TREE_OPERAND (lhs, 1),
				     modifycode, rhs, complain);
      if (newrhs == error_mark_node)
	return error_mark_node;
      return build2 (COMPOUND_EXPR, lhstype,
		     TREE_OPERAND (lhs, 0), newrhs);

    case MODIFY_EXPR:
      if (TREE_SIDE_EFFECTS (TREE_OPERAND (lhs, 0)))
	lhs = build2 (TREE_CODE (lhs), TREE_TYPE (lhs),
		      stabilize_reference (TREE_OPERAND (lhs, 0)),
		      TREE_OPERAND (lhs, 1));
      newrhs = cp_build_modify_expr (TREE_OPERAND (lhs, 0), modifycode, rhs,
				     complain);
      if (newrhs == error_mark_node)
	return error_mark_node;
      return build2 (COMPOUND_EXPR, lhstype, lhs, newrhs);

    case MIN_EXPR:
    case MAX_EXPR:
      /* MIN_EXPR and MAX_EXPR are currently only permitted as lvalues,
	 when neither operand has side-effects.  */
      if (!lvalue_or_else (lhs, lv_assign, complain))
	return error_mark_node;

      gcc_assert (!TREE_SIDE_EFFECTS (TREE_OPERAND (lhs, 0))
		  && !TREE_SIDE_EFFECTS (TREE_OPERAND (lhs, 1)));

      lhs = build3 (COND_EXPR, TREE_TYPE (lhs),
		    build2 (TREE_CODE (lhs) == MIN_EXPR ? LE_EXPR : GE_EXPR,
			    boolean_type_node,
			    TREE_OPERAND (lhs, 0),
			    TREE_OPERAND (lhs, 1)),
		    TREE_OPERAND (lhs, 0),
		    TREE_OPERAND (lhs, 1));
      /* Fall through.  */

      /* Handle (a ? b : c) used as an "lvalue".  */
    case COND_EXPR:
      {
	/* Produce (a ? (b = rhs) : (c = rhs))
	   except that the RHS goes through a save-expr
	   so the code to compute it is only emitted once.  */
	tree cond;
	tree preeval = NULL_TREE;

	if (VOID_TYPE_P (TREE_TYPE (rhs)))
	  {
	    if (complain & tf_error)
	      error ("void value not ignored as it ought to be");
	    return error_mark_node;
	  }

	rhs = stabilize_expr (rhs, &preeval);

	/* Check this here to avoid odd errors when trying to convert
	   a throw to the type of the COND_EXPR.  */
	if (!lvalue_or_else (lhs, lv_assign, complain))
	  return error_mark_node;

	cond = build_conditional_expr
	  (input_location, TREE_OPERAND (lhs, 0),
	   cp_build_modify_expr (TREE_OPERAND (lhs, 1),
				 modifycode, rhs, complain),
	   cp_build_modify_expr (TREE_OPERAND (lhs, 2),
				 modifycode, rhs, complain),
           complain);

	if (cond == error_mark_node)
	  return cond;
	/* Make sure the code to compute the rhs comes out
	   before the split.  */
	if (preeval)
	  cond = build2 (COMPOUND_EXPR, TREE_TYPE (lhs), preeval, cond);
	return cond;
      }

    default:
      break;
    }

  if (modifycode == INIT_EXPR)
    {
      if (BRACE_ENCLOSED_INITIALIZER_P (rhs))
	/* Do the default thing.  */;
      else if (TREE_CODE (rhs) == CONSTRUCTOR)
	{
	  /* Compound literal.  */
	  if (! same_type_p (TREE_TYPE (rhs), lhstype))
	    /* Call convert to generate an error; see PR 11063.  */
	    rhs = convert (lhstype, rhs);
	  result = build2 (INIT_EXPR, lhstype, lhs, rhs);
	  TREE_SIDE_EFFECTS (result) = 1;
	  return result;
	}
      else if (! MAYBE_CLASS_TYPE_P (lhstype))
	/* Do the default thing.  */;
      else
	{
	  vec<tree, va_gc> *rhs_vec = make_tree_vector_single (rhs);
	  result = build_special_member_call (lhs, complete_ctor_identifier,
					      &rhs_vec, lhstype, LOOKUP_NORMAL,
                                              complain);
	  release_tree_vector (rhs_vec);
	  if (result == NULL_TREE)
	    return error_mark_node;
	  return result;
	}
    }
  else
    {
      lhs = require_complete_type_sfinae (lhs, complain);
      if (lhs == error_mark_node)
	return error_mark_node;

      if (modifycode == NOP_EXPR)
	{
	  if (c_dialect_objc ())
	    {
	      result = objc_maybe_build_modify_expr (lhs, rhs);
	      if (result)
		return result;
	    }

	  /* `operator=' is not an inheritable operator.  */
	  if (! MAYBE_CLASS_TYPE_P (lhstype))
	    /* Do the default thing.  */;
	  else
	    {
	      result = build_new_op (input_location, MODIFY_EXPR,
				     LOOKUP_NORMAL, lhs, rhs,
				     make_node (NOP_EXPR), /*overload=*/NULL,
				     complain);
	      if (result == NULL_TREE)
		return error_mark_node;
	      return result;
	    }
	  lhstype = olhstype;
	}
      else
	{
	  tree init = NULL_TREE;

	  /* A binary op has been requested.  Combine the old LHS
	     value with the RHS producing the value we should actually
	     store into the LHS.  */
	  gcc_assert (!((TREE_CODE (lhstype) == REFERENCE_TYPE
			 && MAYBE_CLASS_TYPE_P (TREE_TYPE (lhstype)))
			|| MAYBE_CLASS_TYPE_P (lhstype)));

	  /* Preevaluate the RHS to make sure its evaluation is complete
	     before the lvalue-to-rvalue conversion of the LHS:

	     [expr.ass] With respect to an indeterminately-sequenced
	     function call, the operation of a compound assignment is a
	     single evaluation. [ Note: Therefore, a function call shall
	     not intervene between the lvalue-to-rvalue conversion and the
	     side effect associated with any single compound assignment
	     operator. -- end note ]  */
	  lhs = stabilize_reference (lhs);
	  rhs = rvalue (rhs);
	  rhs = stabilize_expr (rhs, &init);
	  newrhs = cp_build_binary_op (input_location,
				       modifycode, lhs, rhs,
				       complain);
	  if (newrhs == error_mark_node)
	    {
	      if (complain & tf_error)
		error ("  in evaluation of %<%Q(%#T, %#T)%>", modifycode,
		       TREE_TYPE (lhs), TREE_TYPE (rhs));
	      return error_mark_node;
	    }

	  if (init)
	    newrhs = build2 (COMPOUND_EXPR, TREE_TYPE (newrhs), init, newrhs);

	  /* Now it looks like a plain assignment.  */
	  modifycode = NOP_EXPR;
	  if (c_dialect_objc ())
	    {
	      result = objc_maybe_build_modify_expr (lhs, newrhs);
	      if (result)
		return result;
	    }
	}
      gcc_assert (TREE_CODE (lhstype) != REFERENCE_TYPE);
      gcc_assert (TREE_CODE (TREE_TYPE (newrhs)) != REFERENCE_TYPE);
    }

  /* The left-hand side must be an lvalue.  */
  if (!lvalue_or_else (lhs, lv_assign, complain))
    return error_mark_node;

  /* Warn about modifying something that is `const'.  Don't warn if
     this is initialization.  */
  if (modifycode != INIT_EXPR
      && (TREE_READONLY (lhs) || CP_TYPE_CONST_P (lhstype)
	  /* Functions are not modifiable, even though they are
	     lvalues.  */
	  || TREE_CODE (TREE_TYPE (lhs)) == FUNCTION_TYPE
	  || TREE_CODE (TREE_TYPE (lhs)) == METHOD_TYPE
	  /* If it's an aggregate and any field is const, then it is
	     effectively const.  */
	  || (CLASS_TYPE_P (lhstype)
	      && C_TYPE_FIELDS_READONLY (lhstype))))
    {
      if (complain & tf_error)
	cxx_readonly_error (lhs, lv_assign);
      else
	return error_mark_node;
    }

  /* If storing into a structure or union member, it may have been given a
     lowered bitfield type.  We need to convert to the declared type first,
     so retrieve it now.  */

  olhstype = unlowered_expr_type (lhs);

  /* Convert new value to destination type.  */

  if (TREE_CODE (lhstype) == ARRAY_TYPE)
    {
      int from_array;

      if (BRACE_ENCLOSED_INITIALIZER_P (newrhs))
	{
	  if (modifycode != INIT_EXPR)
	    {
	      if (complain & tf_error)
		error ("assigning to an array from an initializer list");
	      return error_mark_node;
	    }
	  if (check_array_initializer (lhs, lhstype, newrhs))
	    return error_mark_node;
	  newrhs = digest_init (lhstype, newrhs, complain);
	  if (newrhs == error_mark_node)
	    return error_mark_node;
	}

      else if (!same_or_base_type_p (TYPE_MAIN_VARIANT (lhstype),
				     TYPE_MAIN_VARIANT (TREE_TYPE (newrhs))))
	{
	  if (complain & tf_error)
	    error ("incompatible types in assignment of %qT to %qT",
		   TREE_TYPE (rhs), lhstype);
	  return error_mark_node;
	}

      /* Allow array assignment in compiler-generated code.  */
      else if (!current_function_decl
	       || !DECL_DEFAULTED_FN (current_function_decl))
	{
          /* This routine is used for both initialization and assignment.
             Make sure the diagnostic message differentiates the context.  */
	  if (complain & tf_error)
	    {
	      if (modifycode == INIT_EXPR)
		error ("array used as initializer");
	      else
		error ("invalid array assignment");
	    }
	  return error_mark_node;
	}

      from_array = TREE_CODE (TREE_TYPE (newrhs)) == ARRAY_TYPE
		   ? 1 + (modifycode != INIT_EXPR): 0;
      return build_vec_init (lhs, NULL_TREE, newrhs,
			     /*explicit_value_init_p=*/false,
			     from_array, complain);
    }

  if (modifycode == INIT_EXPR)
    /* Calls with INIT_EXPR are all direct-initialization, so don't set
       LOOKUP_ONLYCONVERTING.  */
    newrhs = convert_for_initialization (lhs, olhstype, newrhs, LOOKUP_NORMAL,
					 ICR_INIT, NULL_TREE, 0,
                                         complain);
  else
    newrhs = convert_for_assignment (olhstype, newrhs, ICR_ASSIGN,
				     NULL_TREE, 0, complain, LOOKUP_IMPLICIT);

  if (!same_type_p (lhstype, olhstype))
    newrhs = cp_convert_and_check (lhstype, newrhs, complain);

  if (modifycode != INIT_EXPR)
    {
      if (TREE_CODE (newrhs) == CALL_EXPR
	  && TYPE_NEEDS_CONSTRUCTING (lhstype))
	newrhs = build_cplus_new (lhstype, newrhs, complain);

      /* Can't initialize directly from a TARGET_EXPR, since that would
	 cause the lhs to be constructed twice, and possibly result in
	 accidental self-initialization.  So we force the TARGET_EXPR to be
	 expanded without a target.  */
      if (TREE_CODE (newrhs) == TARGET_EXPR)
	newrhs = build2 (COMPOUND_EXPR, TREE_TYPE (newrhs), newrhs,
			 TREE_OPERAND (newrhs, 0));
    }

  if (newrhs == error_mark_node)
    return error_mark_node;

  if (c_dialect_objc () && flag_objc_gc)
    {
      result = objc_generate_write_barrier (lhs, modifycode, newrhs);

      if (result)
	return result;
    }

  result = build2 (modifycode == NOP_EXPR ? MODIFY_EXPR : INIT_EXPR,
		   lhstype, lhs, newrhs);

  TREE_SIDE_EFFECTS (result) = 1;
  if (!plain_assign)
    TREE_NO_WARNING (result) = 1;

  return result;
}

tree
build_x_modify_expr (location_t loc, tree lhs, enum tree_code modifycode,
		     tree rhs, tsubst_flags_t complain)
{
  if (processing_template_decl)
    return build_min_nt_loc (loc, MODOP_EXPR, lhs,
			     build_min_nt_loc (loc, modifycode, NULL_TREE,
					       NULL_TREE), rhs);

  if (modifycode != NOP_EXPR)
    {
      tree rval = build_new_op (loc, MODIFY_EXPR, LOOKUP_NORMAL, lhs, rhs,
				make_node (modifycode), /*overload=*/NULL,
				complain);
      if (rval)
	{
	  TREE_NO_WARNING (rval) = 1;
	  return rval;
	}
    }
  return cp_build_modify_expr (lhs, modifycode, rhs, complain);
}

/* Helper function for get_delta_difference which assumes FROM is a base
   class of TO.  Returns a delta for the conversion of pointer-to-member
   of FROM to pointer-to-member of TO.  If the conversion is invalid and 
   tf_error is not set in COMPLAIN returns error_mark_node, otherwise
   returns zero.  If FROM is not a base class of TO, returns NULL_TREE.
   If C_CAST_P is true, this conversion is taking place as part of a 
   C-style cast.  */

static tree
get_delta_difference_1 (tree from, tree to, bool c_cast_p,
			tsubst_flags_t complain)
{
  tree binfo;
  base_kind kind;

  binfo = lookup_base (to, from, c_cast_p ? ba_unique : ba_check,
		       &kind, complain);

  if (binfo == error_mark_node)
    {
      if (!(complain & tf_error))
	return error_mark_node;

      error ("   in pointer to member function conversion");
      return size_zero_node;
    }
  else if (binfo)
    {
      if (kind != bk_via_virtual)
	return BINFO_OFFSET (binfo);
      else
	/* FROM is a virtual base class of TO.  Issue an error or warning
	   depending on whether or not this is a reinterpret cast.  */
	{
	  if (!(complain & tf_error))
	    return error_mark_node;

	  error ("pointer to member conversion via virtual base %qT",
		 BINFO_TYPE (binfo_from_vbase (binfo)));

	  return size_zero_node;
	}
      }
  else
    return NULL_TREE;
}

/* Get difference in deltas for different pointer to member function
   types.  If the conversion is invalid and tf_error is not set in
   COMPLAIN, returns error_mark_node, otherwise returns an integer
   constant of type PTRDIFF_TYPE_NODE and its value is zero if the
   conversion is invalid.  If ALLOW_INVERSE_P is true, then allow reverse
   conversions as well.  If C_CAST_P is true this conversion is taking
   place as part of a C-style cast.

   Note that the naming of FROM and TO is kind of backwards; the return
   value is what we add to a TO in order to get a FROM.  They are named
   this way because we call this function to find out how to convert from
   a pointer to member of FROM to a pointer to member of TO.  */

static tree
get_delta_difference (tree from, tree to,
		      bool allow_inverse_p,
		      bool c_cast_p, tsubst_flags_t complain)
{
  tree result;

  if (same_type_ignoring_top_level_qualifiers_p (from, to))
    /* Pointer to member of incomplete class is permitted*/
    result = size_zero_node;
  else
    result = get_delta_difference_1 (from, to, c_cast_p, complain);

  if (result == error_mark_node)
    return error_mark_node;

  if (!result)
  {
    if (!allow_inverse_p)
      {
	if (!(complain & tf_error))
	  return error_mark_node;

	error_not_base_type (from, to);
	error ("   in pointer to member conversion");
      	result = size_zero_node;
      }
    else
      {
	result = get_delta_difference_1 (to, from, c_cast_p, complain);

	if (result == error_mark_node)
	  return error_mark_node;

	if (result)
	  result = size_diffop_loc (input_location,
				    size_zero_node, result);
	else
	  {
	    if (!(complain & tf_error))
	      return error_mark_node;

	    error_not_base_type (from, to);
	    error ("   in pointer to member conversion");
	    result = size_zero_node;
	  }
      }
  }

  return fold_if_not_in_template (convert_to_integer (ptrdiff_type_node,
						      result));
}

/* Return a constructor for the pointer-to-member-function TYPE using
   the other components as specified.  */

tree
build_ptrmemfunc1 (tree type, tree delta, tree pfn)
{
  tree u = NULL_TREE;
  tree delta_field;
  tree pfn_field;
  vec<constructor_elt, va_gc> *v;

  /* Pull the FIELD_DECLs out of the type.  */
  pfn_field = TYPE_FIELDS (type);
  delta_field = DECL_CHAIN (pfn_field);

  /* Make sure DELTA has the type we want.  */
  delta = convert_and_check (input_location, delta_type_node, delta);

  /* Convert to the correct target type if necessary.  */
  pfn = fold_convert (TREE_TYPE (pfn_field), pfn);

  /* Finish creating the initializer.  */
  vec_alloc (v, 2);
  CONSTRUCTOR_APPEND_ELT(v, pfn_field, pfn);
  CONSTRUCTOR_APPEND_ELT(v, delta_field, delta);
  u = build_constructor (type, v);
  TREE_CONSTANT (u) = TREE_CONSTANT (pfn) & TREE_CONSTANT (delta);
  TREE_STATIC (u) = (TREE_CONSTANT (u)
		     && (initializer_constant_valid_p (pfn, TREE_TYPE (pfn))
			 != NULL_TREE)
		     && (initializer_constant_valid_p (delta, TREE_TYPE (delta))
			 != NULL_TREE));
  return u;
}

/* Build a constructor for a pointer to member function.  It can be
   used to initialize global variables, local variable, or used
   as a value in expressions.  TYPE is the POINTER to METHOD_TYPE we
   want to be.

   If FORCE is nonzero, then force this conversion, even if
   we would rather not do it.  Usually set when using an explicit
   cast.  A C-style cast is being processed iff C_CAST_P is true.

   Return error_mark_node, if something goes wrong.  */

tree
build_ptrmemfunc (tree type, tree pfn, int force, bool c_cast_p,
		  tsubst_flags_t complain)
{
  tree fn;
  tree pfn_type;
  tree to_type;

  if (error_operand_p (pfn))
    return error_mark_node;

  pfn_type = TREE_TYPE (pfn);
  to_type = build_ptrmemfunc_type (type);

  /* Handle multiple conversions of pointer to member functions.  */
  if (TYPE_PTRMEMFUNC_P (pfn_type))
    {
      tree delta = NULL_TREE;
      tree npfn = NULL_TREE;
      tree n;

      if (!force
	  && !can_convert_arg (to_type, TREE_TYPE (pfn), pfn,
			       LOOKUP_NORMAL, complain))
	{
	  if (complain & tf_error)
	    error ("invalid conversion to type %qT from type %qT",
		   to_type, pfn_type);
	  else
	    return error_mark_node;
	}

      n = get_delta_difference (TYPE_PTRMEMFUNC_OBJECT_TYPE (pfn_type),
				TYPE_PTRMEMFUNC_OBJECT_TYPE (to_type),
				force,
				c_cast_p, complain);
      if (n == error_mark_node)
	return error_mark_node;

      /* We don't have to do any conversion to convert a
	 pointer-to-member to its own type.  But, we don't want to
	 just return a PTRMEM_CST if there's an explicit cast; that
	 cast should make the expression an invalid template argument.  */
      if (TREE_CODE (pfn) != PTRMEM_CST)
	{
	  if (same_type_p (to_type, pfn_type))
	    return pfn;
	  else if (integer_zerop (n))
	    return build_reinterpret_cast (to_type, pfn, 
                                           complain);
	}

      if (TREE_SIDE_EFFECTS (pfn))
	pfn = save_expr (pfn);

      /* Obtain the function pointer and the current DELTA.  */
      if (TREE_CODE (pfn) == PTRMEM_CST)
	expand_ptrmemfunc_cst (pfn, &delta, &npfn);
      else
	{
	  npfn = build_ptrmemfunc_access_expr (pfn, pfn_identifier);
	  delta = build_ptrmemfunc_access_expr (pfn, delta_identifier);
	}

      /* Just adjust the DELTA field.  */
      gcc_assert  (same_type_ignoring_top_level_qualifiers_p
		   (TREE_TYPE (delta), ptrdiff_type_node));
      if (TARGET_PTRMEMFUNC_VBIT_LOCATION == ptrmemfunc_vbit_in_delta)
	n = cp_build_binary_op (input_location,
				LSHIFT_EXPR, n, integer_one_node,
				complain);
      delta = cp_build_binary_op (input_location,
				  PLUS_EXPR, delta, n, complain);
      return build_ptrmemfunc1 (to_type, delta, npfn);
    }

  /* Handle null pointer to member function conversions.  */
  if (null_ptr_cst_p (pfn))
    {
      pfn = cp_build_c_cast (type, pfn, complain);
      return build_ptrmemfunc1 (to_type,
				integer_zero_node,
				pfn);
    }

  if (type_unknown_p (pfn))
    return instantiate_type (type, pfn, complain);

  fn = TREE_OPERAND (pfn, 0);
  gcc_assert (TREE_CODE (fn) == FUNCTION_DECL
	      /* In a template, we will have preserved the
		 OFFSET_REF.  */
	      || (processing_template_decl && TREE_CODE (fn) == OFFSET_REF));
  return make_ptrmem_cst (to_type, fn);
}

/* Return the DELTA, IDX, PFN, and DELTA2 values for the PTRMEM_CST
   given by CST.

   ??? There is no consistency as to the types returned for the above
   values.  Some code acts as if it were a sizetype and some as if it were
   integer_type_node.  */

void
expand_ptrmemfunc_cst (tree cst, tree *delta, tree *pfn)
{
  tree type = TREE_TYPE (cst);
  tree fn = PTRMEM_CST_MEMBER (cst);
  tree ptr_class, fn_class;

  gcc_assert (TREE_CODE (fn) == FUNCTION_DECL);

  /* The class that the function belongs to.  */
  fn_class = DECL_CONTEXT (fn);

  /* The class that we're creating a pointer to member of.  */
  ptr_class = TYPE_PTRMEMFUNC_OBJECT_TYPE (type);

  /* First, calculate the adjustment to the function's class.  */
  *delta = get_delta_difference (fn_class, ptr_class, /*force=*/0,
				 /*c_cast_p=*/0, tf_warning_or_error);

  if (!DECL_VIRTUAL_P (fn))
    *pfn = convert (TYPE_PTRMEMFUNC_FN_TYPE (type),
		    build_addr_func (fn, tf_warning_or_error));
  else
    {
      /* If we're dealing with a virtual function, we have to adjust 'this'
	 again, to point to the base which provides the vtable entry for
	 fn; the call will do the opposite adjustment.  */
      tree orig_class = DECL_CONTEXT (fn);
      tree binfo = binfo_or_else (orig_class, fn_class);
      *delta = build2 (PLUS_EXPR, TREE_TYPE (*delta),
		       *delta, BINFO_OFFSET (binfo));
      *delta = fold_if_not_in_template (*delta);

      /* We set PFN to the vtable offset at which the function can be
	 found, plus one (unless ptrmemfunc_vbit_in_delta, in which
	 case delta is shifted left, and then incremented).  */
      *pfn = DECL_VINDEX (fn);
      *pfn = build2 (MULT_EXPR, integer_type_node, *pfn,
		     TYPE_SIZE_UNIT (vtable_entry_type));
      *pfn = fold_if_not_in_template (*pfn);

      switch (TARGET_PTRMEMFUNC_VBIT_LOCATION)
	{
	case ptrmemfunc_vbit_in_pfn:
	  *pfn = build2 (PLUS_EXPR, integer_type_node, *pfn,
			 integer_one_node);
	  *pfn = fold_if_not_in_template (*pfn);
	  break;

	case ptrmemfunc_vbit_in_delta:
	  *delta = build2 (LSHIFT_EXPR, TREE_TYPE (*delta),
			   *delta, integer_one_node);
	  *delta = fold_if_not_in_template (*delta);
	  *delta = build2 (PLUS_EXPR, TREE_TYPE (*delta),
			   *delta, integer_one_node);
	  *delta = fold_if_not_in_template (*delta);
	  break;

	default:
	  gcc_unreachable ();
	}

      *pfn = build_nop (TYPE_PTRMEMFUNC_FN_TYPE (type), *pfn);
      *pfn = fold_if_not_in_template (*pfn);
    }
}

/* Return an expression for PFN from the pointer-to-member function
   given by T.  */

static tree
pfn_from_ptrmemfunc (tree t)
{
  if (TREE_CODE (t) == PTRMEM_CST)
    {
      tree delta;
      tree pfn;

      expand_ptrmemfunc_cst (t, &delta, &pfn);
      if (pfn)
	return pfn;
    }

  return build_ptrmemfunc_access_expr (t, pfn_identifier);
}

/* Return an expression for DELTA from the pointer-to-member function
   given by T.  */

static tree
delta_from_ptrmemfunc (tree t)
{
  if (TREE_CODE (t) == PTRMEM_CST)
    {
      tree delta;
      tree pfn;

      expand_ptrmemfunc_cst (t, &delta, &pfn);
      if (delta)
	return delta;
    }

  return build_ptrmemfunc_access_expr (t, delta_identifier);
}

/* Convert value RHS to type TYPE as preparation for an assignment to
   an lvalue of type TYPE.  ERRTYPE indicates what kind of error the
   implicit conversion is.  If FNDECL is non-NULL, we are doing the
   conversion in order to pass the PARMNUMth argument of FNDECL.
   If FNDECL is NULL, we are doing the conversion in function pointer
   argument passing, conversion in initialization, etc. */

static tree
convert_for_assignment (tree type, tree rhs,
			impl_conv_rhs errtype, tree fndecl, int parmnum,
			tsubst_flags_t complain, int flags)
{
  tree rhstype;
  enum tree_code coder;

  /* Strip NON_LVALUE_EXPRs since we aren't using as an lvalue.  */
  if (TREE_CODE (rhs) == NON_LVALUE_EXPR)
    rhs = TREE_OPERAND (rhs, 0);

  rhstype = TREE_TYPE (rhs);
  coder = TREE_CODE (rhstype);

  if (TREE_CODE (type) == VECTOR_TYPE && coder == VECTOR_TYPE
      && vector_types_convertible_p (type, rhstype, true))
    {
      rhs = mark_rvalue_use (rhs);
      return convert (type, rhs);
    }

  if (rhs == error_mark_node || rhstype == error_mark_node)
    return error_mark_node;
  if (TREE_CODE (rhs) == TREE_LIST && TREE_VALUE (rhs) == error_mark_node)
    return error_mark_node;

  /* The RHS of an assignment cannot have void type.  */
  if (coder == VOID_TYPE)
    {
      if (complain & tf_error)
	error ("void value not ignored as it ought to be");
      return error_mark_node;
    }

  if (c_dialect_objc ())
    {
      int parmno;
      tree selector;
      tree rname = fndecl;

      switch (errtype)
        {
	  case ICR_ASSIGN:
	    parmno = -1;
	    break;
	  case ICR_INIT:
	    parmno = -2;
	    break;
	  default:
	    selector = objc_message_selector ();
	    parmno = parmnum;
	    if (selector && parmno > 1)
	      {
		rname = selector;
		parmno -= 1;
	      }
	}

      if (objc_compare_types (type, rhstype, parmno, rname))
	{
	  rhs = mark_rvalue_use (rhs);
	  return convert (type, rhs);
	}
    }

  /* [expr.ass]

     The expression is implicitly converted (clause _conv_) to the
     cv-unqualified type of the left operand.

     We allow bad conversions here because by the time we get to this point
     we are committed to doing the conversion.  If we end up doing a bad
     conversion, convert_like will complain.  */
  if (!can_convert_arg_bad (type, rhstype, rhs, flags, complain))
    {
      /* When -Wno-pmf-conversions is use, we just silently allow
	 conversions from pointers-to-members to plain pointers.  If
	 the conversion doesn't work, cp_convert will complain.  */
      if (!warn_pmf2ptr
	  && TYPE_PTR_P (type)
	  && TYPE_PTRMEMFUNC_P (rhstype))
	rhs = cp_convert (strip_top_quals (type), rhs, complain);
      else
	{
	  if (complain & tf_error)
	    {
	      /* If the right-hand side has unknown type, then it is an
		 overloaded function.  Call instantiate_type to get error
		 messages.  */
	      if (rhstype == unknown_type_node)
		instantiate_type (type, rhs, tf_warning_or_error);
	      else if (fndecl)
		error ("cannot convert %qT to %qT for argument %qP to %qD",
		       rhstype, type, parmnum, fndecl);
	      else
		switch (errtype)
		  {
		    case ICR_DEFAULT_ARGUMENT:
		      error ("cannot convert %qT to %qT in default argument",
			     rhstype, type);
		      break;
		    case ICR_ARGPASS:
		      error ("cannot convert %qT to %qT in argument passing",
			     rhstype, type);
		      break;
		    case ICR_CONVERTING:
		      error ("cannot convert %qT to %qT",
			     rhstype, type);
		      break;
		    case ICR_INIT:
		      error ("cannot convert %qT to %qT in initialization",
			     rhstype, type);
		      break;
		    case ICR_RETURN:
		      error ("cannot convert %qT to %qT in return",
			     rhstype, type);
		      break;
		    case ICR_ASSIGN:
		      error ("cannot convert %qT to %qT in assignment",
			     rhstype, type);
		      break;
		    default:
		      gcc_unreachable();
		  }
	    }
	  return error_mark_node;
	}
    }
  if (warn_suggest_attribute_format)
    {
      const enum tree_code codel = TREE_CODE (type);
      if ((codel == POINTER_TYPE || codel == REFERENCE_TYPE)
	  && coder == codel
	  && check_missing_format_attribute (type, rhstype)
	  && (complain & tf_warning))
	switch (errtype)
	  {
	    case ICR_ARGPASS:
	    case ICR_DEFAULT_ARGUMENT:
	      if (fndecl)
		warning (OPT_Wsuggest_attribute_format,
			 "parameter %qP of %qD might be a candidate "
			 "for a format attribute", parmnum, fndecl);
	      else
		warning (OPT_Wsuggest_attribute_format,
			 "parameter might be a candidate "
			 "for a format attribute");
	      break;
	    case ICR_CONVERTING:
	      warning (OPT_Wsuggest_attribute_format,
		       "target of conversion might be a candidate "
		       "for a format attribute");
	      break;
	    case ICR_INIT:
	      warning (OPT_Wsuggest_attribute_format,
		       "target of initialization might be a candidate "
		       "for a format attribute");
	      break;
	    case ICR_RETURN:
	      warning (OPT_Wsuggest_attribute_format,
		       "return type might be a candidate "
		       "for a format attribute");
	      break;
	    case ICR_ASSIGN:
	      warning (OPT_Wsuggest_attribute_format,
		       "left-hand side of assignment might be a candidate "
		       "for a format attribute");
	      break;
	    default:
	      gcc_unreachable();
	  }
    }

  /* If -Wparentheses, warn about a = b = c when a has type bool and b
     does not.  */
  if (warn_parentheses
      && TREE_CODE (type) == BOOLEAN_TYPE
      && TREE_CODE (rhs) == MODIFY_EXPR
      && !TREE_NO_WARNING (rhs)
      && TREE_CODE (TREE_TYPE (rhs)) != BOOLEAN_TYPE
      && (complain & tf_warning))
    {
      location_t loc = EXPR_LOC_OR_LOC (rhs, input_location);

      warning_at (loc, OPT_Wparentheses,
		  "suggest parentheses around assignment used as truth value");
      TREE_NO_WARNING (rhs) = 1;
    }

  return perform_implicit_conversion_flags (strip_top_quals (type), rhs,
					    complain, flags);
}

/* Convert RHS to be of type TYPE.
   If EXP is nonzero, it is the target of the initialization.
   ERRTYPE indicates what kind of error the implicit conversion is.

   Two major differences between the behavior of
   `convert_for_assignment' and `convert_for_initialization'
   are that references are bashed in the former, while
   copied in the latter, and aggregates are assigned in
   the former (operator=) while initialized in the
   latter (X(X&)).

   If using constructor make sure no conversion operator exists, if one does
   exist, an ambiguity exists.  */

tree
convert_for_initialization (tree exp, tree type, tree rhs, int flags,
			    impl_conv_rhs errtype, tree fndecl, int parmnum,
                            tsubst_flags_t complain)
{
  enum tree_code codel = TREE_CODE (type);
  tree rhstype;
  enum tree_code coder;

  /* build_c_cast puts on a NOP_EXPR to make the result not an lvalue.
     Strip such NOP_EXPRs, since RHS is used in non-lvalue context.  */
  if (TREE_CODE (rhs) == NOP_EXPR
      && TREE_TYPE (rhs) == TREE_TYPE (TREE_OPERAND (rhs, 0))
      && codel != REFERENCE_TYPE)
    rhs = TREE_OPERAND (rhs, 0);

  if (type == error_mark_node
      || rhs == error_mark_node
      || (TREE_CODE (rhs) == TREE_LIST && TREE_VALUE (rhs) == error_mark_node))
    return error_mark_node;

  if ((TREE_CODE (TREE_TYPE (rhs)) == ARRAY_TYPE
       && TREE_CODE (type) != ARRAY_TYPE
       && (TREE_CODE (type) != REFERENCE_TYPE
	   || TREE_CODE (TREE_TYPE (type)) != ARRAY_TYPE))
      || (TREE_CODE (TREE_TYPE (rhs)) == FUNCTION_TYPE
	  && !TYPE_REFFN_P (type))
      || TREE_CODE (TREE_TYPE (rhs)) == METHOD_TYPE)
    rhs = decay_conversion (rhs, complain);

  rhstype = TREE_TYPE (rhs);
  coder = TREE_CODE (rhstype);

  if (coder == ERROR_MARK)
    return error_mark_node;

  /* We accept references to incomplete types, so we can
     return here before checking if RHS is of complete type.  */

  if (codel == REFERENCE_TYPE)
    {
      /* This should eventually happen in convert_arguments.  */
      int savew = 0, savee = 0;

      if (fndecl)
	savew = warningcount + werrorcount, savee = errorcount;
      rhs = initialize_reference (type, rhs, flags, complain);

      if (fndecl
	  && (warningcount + werrorcount > savew || errorcount > savee))
	inform (input_location,
		"in passing argument %P of %q+D", parmnum, fndecl);

      return rhs;
    }

  if (exp != 0)
    exp = require_complete_type_sfinae (exp, complain);
  if (exp == error_mark_node)
    return error_mark_node;

  rhstype = non_reference (rhstype);

  type = complete_type (type);

  if (DIRECT_INIT_EXPR_P (type, rhs))
    /* Don't try to do copy-initialization if we already have
       direct-initialization.  */
    return rhs;

  if (MAYBE_CLASS_TYPE_P (type))
    return perform_implicit_conversion_flags (type, rhs, complain, flags);

  return convert_for_assignment (type, rhs, errtype, fndecl, parmnum,
				 complain, flags);
}

/* If RETVAL is the address of, or a reference to, a local variable or
   temporary give an appropriate warning.  */

static void
maybe_warn_about_returning_address_of_local (tree retval)
{
  tree valtype = TREE_TYPE (DECL_RESULT (current_function_decl));
  tree whats_returned = retval;

  for (;;)
    {
      if (TREE_CODE (whats_returned) == COMPOUND_EXPR)
	whats_returned = TREE_OPERAND (whats_returned, 1);
      else if (CONVERT_EXPR_P (whats_returned)
	       || TREE_CODE (whats_returned) == NON_LVALUE_EXPR)
	whats_returned = TREE_OPERAND (whats_returned, 0);
      else
	break;
    }

  if (TREE_CODE (whats_returned) != ADDR_EXPR)
    return;
  whats_returned = TREE_OPERAND (whats_returned, 0);

  if (TREE_CODE (valtype) == REFERENCE_TYPE)
    {
      if (TREE_CODE (whats_returned) == AGGR_INIT_EXPR
	  || TREE_CODE (whats_returned) == TARGET_EXPR)
	{
	  warning (OPT_Wreturn_local_addr, "returning reference to temporary");
	  return;
	}
      if (VAR_P (whats_returned)
	  && DECL_NAME (whats_returned)
	  && TEMP_NAME_P (DECL_NAME (whats_returned)))
	{
	  warning (OPT_Wreturn_local_addr, "reference to non-lvalue returned");
	  return;
	}
    }

  while (TREE_CODE (whats_returned) == COMPONENT_REF
	 || TREE_CODE (whats_returned) == ARRAY_REF)
    whats_returned = TREE_OPERAND (whats_returned, 0);

  if (DECL_P (whats_returned)
      && DECL_NAME (whats_returned)
      && DECL_FUNCTION_SCOPE_P (whats_returned)
      && !is_capture_proxy (whats_returned)
      && !(TREE_STATIC (whats_returned)
	   || TREE_PUBLIC (whats_returned)))
    {
      if (TREE_CODE (valtype) == REFERENCE_TYPE)
	warning (OPT_Wreturn_local_addr, "reference to local variable %q+D returned",
		 whats_returned);
      else
	warning (OPT_Wreturn_local_addr, "address of local variable %q+D returned",
		 whats_returned);
      return;
    }
}

/* Check that returning RETVAL from the current function is valid.
   Return an expression explicitly showing all conversions required to
   change RETVAL into the function return type, and to assign it to
   the DECL_RESULT for the function.  Set *NO_WARNING to true if
   code reaches end of non-void function warning shouldn't be issued
   on this RETURN_EXPR.  */

tree
check_return_expr (tree retval, bool *no_warning)
{
  tree result;
  /* The type actually returned by the function.  */
  tree valtype;
  /* The type the function is declared to return, or void if
     the declared type is incomplete.  */
  tree functype;
  int fn_returns_value_p;
  bool named_return_value_okay_p;

  *no_warning = false;

  if (flag_cilkplus && retval && contains_cilk_spawn_stmt (retval))
    {
      error_at (EXPR_LOCATION (retval), "use of %<_Cilk_spawn%> in a return "
		"statement is not allowed");
      return NULL_TREE;
    }

  /* A `volatile' function is one that isn't supposed to return, ever.
     (This is a G++ extension, used to get better code for functions
     that call the `volatile' function.)  */
  if (TREE_THIS_VOLATILE (current_function_decl))
    warning (0, "function declared %<noreturn%> has a %<return%> statement");

  /* Check for various simple errors.  */
  if (DECL_DESTRUCTOR_P (current_function_decl))
    {
      if (retval)
	error ("returning a value from a destructor");
      return NULL_TREE;
    }
  else if (DECL_CONSTRUCTOR_P (current_function_decl))
    {
      if (in_function_try_handler)
	/* If a return statement appears in a handler of the
	   function-try-block of a constructor, the program is ill-formed.  */
	error ("cannot return from a handler of a function-try-block of a constructor");
      else if (retval)
	/* You can't return a value from a constructor.  */
	error ("returning a value from a constructor");
      return NULL_TREE;
    }

  if (processing_template_decl)
    {
      current_function_returns_value = 1;
      if (check_for_bare_parameter_packs (retval))
        retval = error_mark_node;
      return retval;
    }

  functype = TREE_TYPE (TREE_TYPE (current_function_decl));

  /* Deduce auto return type from a return statement.  */
  if (current_function_auto_return_pattern)
    {
      tree auto_node;
      tree type;

      if (!retval && !is_auto (current_function_auto_return_pattern))
	{
	  /* Give a helpful error message.  */
	  error ("return-statement with no value, in function returning %qT",
		 current_function_auto_return_pattern);
	  inform (input_location, "only plain %<auto%> return type can be "
		  "deduced to %<void%>");
	  type = error_mark_node;
	}
      else if (retval && BRACE_ENCLOSED_INITIALIZER_P (retval))
	{
	  error ("returning initializer list");
	  type = error_mark_node;
	}
      else
	{
	  if (!retval)
	    retval = void_zero_node;
	  auto_node = type_uses_auto (current_function_auto_return_pattern);
	  type = do_auto_deduction (current_function_auto_return_pattern,
				    retval, auto_node);
	}

      if (type == error_mark_node)
	/* Leave it.  */;
      else if (functype == current_function_auto_return_pattern)
	apply_deduced_return_type (current_function_decl, type);
      else
	/* A mismatch should have been diagnosed in do_auto_deduction.  */
	gcc_assert (same_type_p (type, functype));
      functype = type;
    }

  /* When no explicit return-value is given in a function with a named
     return value, the named return value is used.  */
  result = DECL_RESULT (current_function_decl);
  valtype = TREE_TYPE (result);
  gcc_assert (valtype != NULL_TREE);
  fn_returns_value_p = !VOID_TYPE_P (valtype);
  if (!retval && DECL_NAME (result) && fn_returns_value_p)
    retval = result;

  /* Check for a return statement with no return value in a function
     that's supposed to return a value.  */
  if (!retval && fn_returns_value_p)
    {
      if (functype != error_mark_node)
	permerror (input_location, "return-statement with no value, in "
		   "function returning %qT", valtype);
      /* Remember that this function did return.  */
      current_function_returns_value = 1;
      /* And signal caller that TREE_NO_WARNING should be set on the
	 RETURN_EXPR to avoid control reaches end of non-void function
	 warnings in tree-cfg.c.  */
      *no_warning = true;
    }
  /* Check for a return statement with a value in a function that
     isn't supposed to return a value.  */
  else if (retval && !fn_returns_value_p)
    {
      if (VOID_TYPE_P (TREE_TYPE (retval)))
	/* You can return a `void' value from a function of `void'
	   type.  In that case, we have to evaluate the expression for
	   its side-effects.  */
	  finish_expr_stmt (retval);
      else
	permerror (input_location, "return-statement with a value, in function "
		   "returning 'void'");
      current_function_returns_null = 1;

      /* There's really no value to return, after all.  */
      return NULL_TREE;
    }
  else if (!retval)
    /* Remember that this function can sometimes return without a
       value.  */
    current_function_returns_null = 1;
  else
    /* Remember that this function did return a value.  */
    current_function_returns_value = 1;

  /* Check for erroneous operands -- but after giving ourselves a
     chance to provide an error about returning a value from a void
     function.  */
  if (error_operand_p (retval))
    {
      current_function_return_value = error_mark_node;
      return error_mark_node;
    }

  /* Only operator new(...) throw(), can return NULL [expr.new/13].  */
  if ((DECL_OVERLOADED_OPERATOR_P (current_function_decl) == NEW_EXPR
       || DECL_OVERLOADED_OPERATOR_P (current_function_decl) == VEC_NEW_EXPR)
      && !TYPE_NOTHROW_P (TREE_TYPE (current_function_decl))
      && ! flag_check_new
      && retval && null_ptr_cst_p (retval))
    warning (0, "%<operator new%> must not return NULL unless it is "
	     "declared %<throw()%> (or -fcheck-new is in effect)");

  /* Effective C++ rule 15.  See also start_function.  */
  if (warn_ecpp
      && DECL_NAME (current_function_decl) == ansi_assopname(NOP_EXPR))
    {
      bool warn = true;

      /* The function return type must be a reference to the current
	class.  */
      if (TREE_CODE (valtype) == REFERENCE_TYPE
	  && same_type_ignoring_top_level_qualifiers_p
	      (TREE_TYPE (valtype), TREE_TYPE (current_class_ref)))
	{
	  /* Returning '*this' is obviously OK.  */
	  if (retval == current_class_ref)
	    warn = false;
	  /* If we are calling a function whose return type is the same of
	     the current class reference, it is ok.  */
	  else if (INDIRECT_REF_P (retval)
		   && TREE_CODE (TREE_OPERAND (retval, 0)) == CALL_EXPR)
	    warn = false;
	}

      if (warn)
	warning (OPT_Weffc__, "%<operator=%> should return a reference to %<*this%>");
    }

  /* The fabled Named Return Value optimization, as per [class.copy]/15:

     [...]      For  a function with a class return type, if the expression
     in the return statement is the name of a local  object,  and  the  cv-
     unqualified  type  of  the  local  object  is the same as the function
     return type, an implementation is permitted to omit creating the  tem-
     porary  object  to  hold  the function return value [...]

     So, if this is a value-returning function that always returns the same
     local variable, remember it.

     It might be nice to be more flexible, and choose the first suitable
     variable even if the function sometimes returns something else, but
     then we run the risk of clobbering the variable we chose if the other
     returned expression uses the chosen variable somehow.  And people expect
     this restriction, anyway.  (jason 2000-11-19)

     See finish_function and finalize_nrv for the rest of this optimization.  */

  named_return_value_okay_p = 
    (retval != NULL_TREE
     /* Must be a local, automatic variable.  */
     && VAR_P (retval)
     && DECL_CONTEXT (retval) == current_function_decl
     && ! TREE_STATIC (retval)
     /* And not a lambda or anonymous union proxy.  */
     && !DECL_HAS_VALUE_EXPR_P (retval)
     && (DECL_ALIGN (retval) <= DECL_ALIGN (result))
     /* The cv-unqualified type of the returned value must be the
        same as the cv-unqualified return type of the
        function.  */
     && same_type_p ((TYPE_MAIN_VARIANT (TREE_TYPE (retval))),
                     (TYPE_MAIN_VARIANT (functype)))
     /* And the returned value must be non-volatile.  */
     && ! TYPE_VOLATILE (TREE_TYPE (retval)));
     
  if (fn_returns_value_p && flag_elide_constructors)
    {
      if (named_return_value_okay_p
          && (current_function_return_value == NULL_TREE
              || current_function_return_value == retval))
	current_function_return_value = retval;
      else
	current_function_return_value = error_mark_node;
    }

  /* We don't need to do any conversions when there's nothing being
     returned.  */
  if (!retval)
    return NULL_TREE;

  /* Do any required conversions.  */
  if (retval == result || DECL_CONSTRUCTOR_P (current_function_decl))
    /* No conversions are required.  */
    ;
  else
    {
      int flags = LOOKUP_NORMAL | LOOKUP_ONLYCONVERTING;

      /* The functype's return type will have been set to void, if it
	 was an incomplete type.  Just treat this as 'return;' */
      if (VOID_TYPE_P (functype))
	return error_mark_node;

      /* Under C++0x [12.8/16 class.copy], a returned lvalue is sometimes
	 treated as an rvalue for the purposes of overload resolution to
	 favor move constructors over copy constructors.

         Note that these conditions are similar to, but not as strict as,
	 the conditions for the named return value optimization.  */
      if ((cxx_dialect != cxx98)
          && ((VAR_P (retval) && !DECL_HAS_VALUE_EXPR_P (retval))
	      || TREE_CODE (retval) == PARM_DECL)
	  && DECL_CONTEXT (retval) == current_function_decl
	  && !TREE_STATIC (retval)
	  && same_type_p ((TYPE_MAIN_VARIANT (TREE_TYPE (retval))),
			  (TYPE_MAIN_VARIANT (functype)))
	  /* This is only interesting for class type.  */
	  && CLASS_TYPE_P (functype))
	flags = flags | LOOKUP_PREFER_RVALUE;

      /* First convert the value to the function's return type, then
	 to the type of return value's location to handle the
	 case that functype is smaller than the valtype.  */
      retval = convert_for_initialization
	(NULL_TREE, functype, retval, flags, ICR_RETURN, NULL_TREE, 0,
         tf_warning_or_error);
      retval = convert (valtype, retval);

      /* If the conversion failed, treat this just like `return;'.  */
      if (retval == error_mark_node)
	return retval;
      /* We can't initialize a register from a AGGR_INIT_EXPR.  */
      else if (! cfun->returns_struct
	       && TREE_CODE (retval) == TARGET_EXPR
	       && TREE_CODE (TREE_OPERAND (retval, 1)) == AGGR_INIT_EXPR)
	retval = build2 (COMPOUND_EXPR, TREE_TYPE (retval), retval,
			 TREE_OPERAND (retval, 0));
      else
	maybe_warn_about_returning_address_of_local (retval);
    }

  /* Actually copy the value returned into the appropriate location.  */
  if (retval && retval != result)
    retval = build2 (INIT_EXPR, TREE_TYPE (result), result, retval);

  return retval;
}


/* Returns nonzero if the pointer-type FROM can be converted to the
   pointer-type TO via a qualification conversion.  If CONSTP is -1,
   then we return nonzero if the pointers are similar, and the
   cv-qualification signature of FROM is a proper subset of that of TO.

   If CONSTP is positive, then all outer pointers have been
   const-qualified.  */

static int
comp_ptr_ttypes_real (tree to, tree from, int constp)
{
  bool to_more_cv_qualified = false;
  bool is_opaque_pointer = false;

  for (; ; to = TREE_TYPE (to), from = TREE_TYPE (from))
    {
      if (TREE_CODE (to) != TREE_CODE (from))
	return 0;

      if (TREE_CODE (from) == OFFSET_TYPE
	  && !same_type_p (TYPE_OFFSET_BASETYPE (from),
			   TYPE_OFFSET_BASETYPE (to)))
	return 0;

      /* Const and volatile mean something different for function types,
	 so the usual checks are not appropriate.  */
      if (TREE_CODE (to) != FUNCTION_TYPE && TREE_CODE (to) != METHOD_TYPE)
	{
	  if (!at_least_as_qualified_p (to, from))
	    return 0;

	  if (!at_least_as_qualified_p (from, to))
	    {
	      if (constp == 0)
		return 0;
	      to_more_cv_qualified = true;
	    }

	  if (constp > 0)
	    constp &= TYPE_READONLY (to);
	}

      if (TREE_CODE (to) == VECTOR_TYPE)
	is_opaque_pointer = vector_targets_convertible_p (to, from);

      if (!TYPE_PTR_P (to) && !TYPE_PTRDATAMEM_P (to))
	return ((constp >= 0 || to_more_cv_qualified)
		&& (is_opaque_pointer
		    || same_type_ignoring_top_level_qualifiers_p (to, from)));
    }
}

/* When comparing, say, char ** to char const **, this function takes
   the 'char *' and 'char const *'.  Do not pass non-pointer/reference
   types to this function.  */

int
comp_ptr_ttypes (tree to, tree from)
{
  return comp_ptr_ttypes_real (to, from, 1);
}

/* Returns true iff FNTYPE is a non-class type that involves
   error_mark_node.  We can get FUNCTION_TYPE with buried error_mark_node
   if a parameter type is ill-formed.  */

bool
error_type_p (const_tree type)
{
  tree t;

  switch (TREE_CODE (type))
    {
    case ERROR_MARK:
      return true;

    case POINTER_TYPE:
    case REFERENCE_TYPE:
    case OFFSET_TYPE:
      return error_type_p (TREE_TYPE (type));

    case FUNCTION_TYPE:
    case METHOD_TYPE:
      if (error_type_p (TREE_TYPE (type)))
	return true;
      for (t = TYPE_ARG_TYPES (type); t; t = TREE_CHAIN (t))
	if (error_type_p (TREE_VALUE (t)))
	  return true;
      return false;

    case RECORD_TYPE:
      if (TYPE_PTRMEMFUNC_P (type))
	return error_type_p (TYPE_PTRMEMFUNC_FN_TYPE (type));
      return false;

    default:
      return false;
    }
}

/* Returns true if to and from are (possibly multi-level) pointers to the same
   type or inheritance-related types, regardless of cv-quals.  */

bool
ptr_reasonably_similar (const_tree to, const_tree from)
{
  for (; ; to = TREE_TYPE (to), from = TREE_TYPE (from))
    {
      /* Any target type is similar enough to void.  */
      if (VOID_TYPE_P (to))
	return !error_type_p (from);
      if (VOID_TYPE_P (from))
	return !error_type_p (to);

      if (TREE_CODE (to) != TREE_CODE (from))
	return false;

      if (TREE_CODE (from) == OFFSET_TYPE
	  && comptypes (TYPE_OFFSET_BASETYPE (to),
			TYPE_OFFSET_BASETYPE (from),
			COMPARE_BASE | COMPARE_DERIVED))
	continue;

      if (TREE_CODE (to) == VECTOR_TYPE
	  && vector_types_convertible_p (to, from, false))
	return true;

      if (TREE_CODE (to) == INTEGER_TYPE
	  && TYPE_PRECISION (to) == TYPE_PRECISION (from))
	return true;

      if (TREE_CODE (to) == FUNCTION_TYPE)
	return !error_type_p (to) && !error_type_p (from);

      if (!TYPE_PTR_P (to))
	{
	  /* When either type is incomplete avoid DERIVED_FROM_P,
	     which may call complete_type (c++/57942).  */
	  bool b = !COMPLETE_TYPE_P (to) || !COMPLETE_TYPE_P (from);
	  return comptypes
	    (TYPE_MAIN_VARIANT (to), TYPE_MAIN_VARIANT (from),
	     b ? COMPARE_STRICT : COMPARE_BASE | COMPARE_DERIVED);
	}
    }
}

/* Return true if TO and FROM (both of which are POINTER_TYPEs or
   pointer-to-member types) are the same, ignoring cv-qualification at
   all levels.  */

bool
comp_ptr_ttypes_const (tree to, tree from)
{
  bool is_opaque_pointer = false;

  for (; ; to = TREE_TYPE (to), from = TREE_TYPE (from))
    {
      if (TREE_CODE (to) != TREE_CODE (from))
	return false;

      if (TREE_CODE (from) == OFFSET_TYPE
	  && same_type_p (TYPE_OFFSET_BASETYPE (from),
			  TYPE_OFFSET_BASETYPE (to)))
	  continue;

      if (TREE_CODE (to) == VECTOR_TYPE)
	is_opaque_pointer = vector_targets_convertible_p (to, from);

      if (!TYPE_PTR_P (to))
	return (is_opaque_pointer
		|| same_type_ignoring_top_level_qualifiers_p (to, from));
    }
}

/* Returns the type qualifiers for this type, including the qualifiers on the
   elements for an array type.  */

int
cp_type_quals (const_tree type)
{
  int quals;
  /* This CONST_CAST is okay because strip_array_types returns its
     argument unmodified and we assign it to a const_tree.  */
  type = strip_array_types (CONST_CAST_TREE (type));
  if (type == error_mark_node
      /* Quals on a FUNCTION_TYPE are memfn quals.  */
      || TREE_CODE (type) == FUNCTION_TYPE)
    return TYPE_UNQUALIFIED;
  quals = TYPE_QUALS (type);
  /* METHOD and REFERENCE_TYPEs should never have quals.  */
  gcc_assert ((TREE_CODE (type) != METHOD_TYPE
	       && TREE_CODE (type) != REFERENCE_TYPE)
	      || ((quals & (TYPE_QUAL_CONST|TYPE_QUAL_VOLATILE))
		  == TYPE_UNQUALIFIED));
  return quals;
}

/* Returns the function-ref-qualifier for TYPE */

cp_ref_qualifier
type_memfn_rqual (const_tree type)
{
  gcc_assert (TREE_CODE (type) == FUNCTION_TYPE
              || TREE_CODE (type) == METHOD_TYPE);

  if (!FUNCTION_REF_QUALIFIED (type))
    return REF_QUAL_NONE;
  else if (FUNCTION_RVALUE_QUALIFIED (type))
    return REF_QUAL_RVALUE;
  else
    return REF_QUAL_LVALUE;
}

/* Returns the function-cv-quals for TYPE, which must be a FUNCTION_TYPE or
   METHOD_TYPE.  */

int
type_memfn_quals (const_tree type)
{
  if (TREE_CODE (type) == FUNCTION_TYPE)
    return TYPE_QUALS (type);
  else if (TREE_CODE (type) == METHOD_TYPE)
    return cp_type_quals (class_of_this_parm (type));
  else
    gcc_unreachable ();
}

/* Returns the FUNCTION_TYPE TYPE with its function-cv-quals changed to
   MEMFN_QUALS and its ref-qualifier to RQUAL. */

tree
apply_memfn_quals (tree type, cp_cv_quals memfn_quals, cp_ref_qualifier rqual)
{
  /* Could handle METHOD_TYPE here if necessary.  */
  gcc_assert (TREE_CODE (type) == FUNCTION_TYPE);
  if (TYPE_QUALS (type) == memfn_quals
      && type_memfn_rqual (type) == rqual)
    return type;

  /* This should really have a different TYPE_MAIN_VARIANT, but that gets
     complex.  */
  tree result = build_qualified_type (type, memfn_quals);
  result = build_exception_variant (result, TYPE_RAISES_EXCEPTIONS (type));
  return build_ref_qualified_type (result, rqual);
}

/* Returns nonzero if TYPE is const or volatile.  */

bool
cv_qualified_p (const_tree type)
{
  int quals = cp_type_quals (type);
  return (quals & (TYPE_QUAL_CONST|TYPE_QUAL_VOLATILE)) != 0;
}

/* Returns nonzero if the TYPE contains a mutable member.  */

bool
cp_has_mutable_p (const_tree type)
{
  /* This CONST_CAST is okay because strip_array_types returns its
     argument unmodified and we assign it to a const_tree.  */
  type = strip_array_types (CONST_CAST_TREE(type));

  return CLASS_TYPE_P (type) && CLASSTYPE_HAS_MUTABLE (type);
}

/* Set TREE_READONLY and TREE_VOLATILE on DECL as indicated by the
   TYPE_QUALS.  For a VAR_DECL, this may be an optimistic
   approximation.  In particular, consider:

     int f();
     struct S { int i; };
     const S s = { f(); }

   Here, we will make "s" as TREE_READONLY (because it is declared
   "const") -- only to reverse ourselves upon seeing that the
   initializer is non-constant.  */

void
cp_apply_type_quals_to_decl (int type_quals, tree decl)
{
  tree type = TREE_TYPE (decl);

  if (type == error_mark_node)
    return;

  if (TREE_CODE (decl) == TYPE_DECL)
    return;

  gcc_assert (!(TREE_CODE (type) == FUNCTION_TYPE
		&& type_quals != TYPE_UNQUALIFIED));

  /* Avoid setting TREE_READONLY incorrectly.  */
  /* We used to check TYPE_NEEDS_CONSTRUCTING here, but now a constexpr
     constructor can produce constant init, so rely on cp_finish_decl to
     clear TREE_READONLY if the variable has non-constant init.  */

  /* If the type has (or might have) a mutable component, that component
     might be modified.  */
  if (TYPE_HAS_MUTABLE_P (type) || !COMPLETE_TYPE_P (type))
    type_quals &= ~TYPE_QUAL_CONST;

  c_apply_type_quals_to_decl (type_quals, decl);
}

/* Subroutine of casts_away_constness.  Make T1 and T2 point at
   exemplar types such that casting T1 to T2 is casting away constness
   if and only if there is no implicit conversion from T1 to T2.  */

static void
casts_away_constness_r (tree *t1, tree *t2, tsubst_flags_t complain)
{
  int quals1;
  int quals2;

  /* [expr.const.cast]

     For multi-level pointer to members and multi-level mixed pointers
     and pointers to members (conv.qual), the "member" aspect of a
     pointer to member level is ignored when determining if a const
     cv-qualifier has been cast away.  */
  /* [expr.const.cast]

     For  two  pointer types:

	    X1 is T1cv1,1 * ... cv1,N *   where T1 is not a pointer type
	    X2 is T2cv2,1 * ... cv2,M *   where T2 is not a pointer type
	    K is min(N,M)

     casting from X1 to X2 casts away constness if, for a non-pointer
     type T there does not exist an implicit conversion (clause
     _conv_) from:

	    Tcv1,(N-K+1) * cv1,(N-K+2) * ... cv1,N *

     to

	    Tcv2,(M-K+1) * cv2,(M-K+2) * ... cv2,M *.  */
  if ((!TYPE_PTR_P (*t1) && !TYPE_PTRDATAMEM_P (*t1))
      || (!TYPE_PTR_P (*t2) && !TYPE_PTRDATAMEM_P (*t2)))
    {
      *t1 = cp_build_qualified_type (void_type_node,
				     cp_type_quals (*t1));
      *t2 = cp_build_qualified_type (void_type_node,
				     cp_type_quals (*t2));
      return;
    }

  quals1 = cp_type_quals (*t1);
  quals2 = cp_type_quals (*t2);

  if (TYPE_PTRDATAMEM_P (*t1))
    *t1 = TYPE_PTRMEM_POINTED_TO_TYPE (*t1);
  else
    *t1 = TREE_TYPE (*t1);
  if (TYPE_PTRDATAMEM_P (*t2))
    *t2 = TYPE_PTRMEM_POINTED_TO_TYPE (*t2);
  else
    *t2 = TREE_TYPE (*t2);

  casts_away_constness_r (t1, t2, complain);
  *t1 = build_pointer_type (*t1);
  *t2 = build_pointer_type (*t2);
  *t1 = cp_build_qualified_type (*t1, quals1);
  *t2 = cp_build_qualified_type (*t2, quals2);
}

/* Returns nonzero if casting from TYPE1 to TYPE2 casts away
   constness.  

   ??? This function returns non-zero if casting away qualifiers not
   just const.  We would like to return to the caller exactly which
   qualifiers are casted away to give more accurate diagnostics.
*/

static bool
casts_away_constness (tree t1, tree t2, tsubst_flags_t complain)
{
  if (TREE_CODE (t2) == REFERENCE_TYPE)
    {
      /* [expr.const.cast]

	 Casting from an lvalue of type T1 to an lvalue of type T2
	 using a reference cast casts away constness if a cast from an
	 rvalue of type "pointer to T1" to the type "pointer to T2"
	 casts away constness.  */
      t1 = (TREE_CODE (t1) == REFERENCE_TYPE ? TREE_TYPE (t1) : t1);
      return casts_away_constness (build_pointer_type (t1),
				   build_pointer_type (TREE_TYPE (t2)),
				   complain);
    }

  if (TYPE_PTRDATAMEM_P (t1) && TYPE_PTRDATAMEM_P (t2))
    /* [expr.const.cast]

       Casting from an rvalue of type "pointer to data member of X
       of type T1" to the type "pointer to data member of Y of type
       T2" casts away constness if a cast from an rvalue of type
       "pointer to T1" to the type "pointer to T2" casts away
       constness.  */
    return casts_away_constness
      (build_pointer_type (TYPE_PTRMEM_POINTED_TO_TYPE (t1)),
       build_pointer_type (TYPE_PTRMEM_POINTED_TO_TYPE (t2)),
       complain);

  /* Casting away constness is only something that makes sense for
     pointer or reference types.  */
  if (!TYPE_PTR_P (t1) || !TYPE_PTR_P (t2))
    return false;

  /* Top-level qualifiers don't matter.  */
  t1 = TYPE_MAIN_VARIANT (t1);
  t2 = TYPE_MAIN_VARIANT (t2);
  casts_away_constness_r (&t1, &t2, complain);
  if (!can_convert (t2, t1, complain))
    return true;

  return false;
}

/* If T is a REFERENCE_TYPE return the type to which T refers.
   Otherwise, return T itself.  */

tree
non_reference (tree t)
{
  if (t && TREE_CODE (t) == REFERENCE_TYPE)
    t = TREE_TYPE (t);
  return t;
}


/* Return nonzero if REF is an lvalue valid for this language;
   otherwise, print an error message and return zero.  USE says
   how the lvalue is being used and so selects the error message.  */

int
lvalue_or_else (tree ref, enum lvalue_use use, tsubst_flags_t complain)
{
  cp_lvalue_kind kind = lvalue_kind (ref);

  if (kind == clk_none)
    {
      if (complain & tf_error)
	lvalue_error (input_location, use);
      return 0;
    }
  else if (kind & (clk_rvalueref|clk_class))
    {
      if (!(complain & tf_error))
	return 0;
      if (kind & clk_class)
	/* Make this a permerror because we used to accept it.  */
	permerror (input_location, "using temporary as lvalue");
      else
	error ("using xvalue (rvalue reference) as lvalue");
    }
  return 1;
}

/* Return true if a user-defined literal operator is a raw operator.  */

bool
check_raw_literal_operator (const_tree decl)
{
  tree argtypes = TYPE_ARG_TYPES (TREE_TYPE (decl));
  tree argtype;
  int arity;
  bool maybe_raw_p = false;

  /* Count the number and type of arguments and check for ellipsis.  */
  for (argtype = argtypes, arity = 0;
       argtype && argtype != void_list_node;
       ++arity, argtype = TREE_CHAIN (argtype))
    {
      tree t = TREE_VALUE (argtype);

      if (same_type_p (t, const_string_type_node))
	maybe_raw_p = true;
    }
  if (!argtype)
    return false; /* Found ellipsis.  */

  if (!maybe_raw_p || arity != 1)
    return false;

  return true;
}


/* Return true if a user-defined literal operator has one of the allowed
   argument types.  */

bool
check_literal_operator_args (const_tree decl,
			     bool *long_long_unsigned_p, bool *long_double_p)
{
  tree argtypes = TYPE_ARG_TYPES (TREE_TYPE (decl));

  *long_long_unsigned_p = false;
  *long_double_p = false;
  if (processing_template_decl || processing_specialization)
    return argtypes == void_list_node;
  else
    {
      tree argtype;
      int arity;
      int max_arity = 2;

      /* Count the number and type of arguments and check for ellipsis.  */
      for (argtype = argtypes, arity = 0;
	   argtype && argtype != void_list_node;
	   argtype = TREE_CHAIN (argtype))
	{
	  tree t = TREE_VALUE (argtype);
	  ++arity;

	  if (TYPE_PTR_P (t))
	    {
	      bool maybe_raw_p = false;
	      t = TREE_TYPE (t);
	      if (cp_type_quals (t) != TYPE_QUAL_CONST)
		return false;
	      t = TYPE_MAIN_VARIANT (t);
	      if ((maybe_raw_p = same_type_p (t, char_type_node))
		  || same_type_p (t, wchar_type_node)
		  || same_type_p (t, char16_type_node)
		  || same_type_p (t, char32_type_node))
		{
		  argtype = TREE_CHAIN (argtype);
		  if (!argtype)
		    return false;
		  t = TREE_VALUE (argtype);
		  if (maybe_raw_p && argtype == void_list_node)
		    return true;
		  else if (same_type_p (t, size_type_node))
		    {
		      ++arity;
		      continue;
		    }
		  else
		    return false;
		}
	    }
	  else if (same_type_p (t, long_long_unsigned_type_node))
	    {
	      max_arity = 1;
	      *long_long_unsigned_p = true;
	    }
	  else if (same_type_p (t, long_double_type_node))
	    {
	      max_arity = 1;
	      *long_double_p = true;
	    }
	  else if (same_type_p (t, char_type_node))
	    max_arity = 1;
	  else if (same_type_p (t, wchar_type_node))
	    max_arity = 1;
	  else if (same_type_p (t, char16_type_node))
	    max_arity = 1;
	  else if (same_type_p (t, char32_type_node))
	    max_arity = 1;
	  else
	    return false;
	}
      if (!argtype)
	return false; /* Found ellipsis.  */

      if (arity != max_arity)
	return false;

      return true;
    }
}
