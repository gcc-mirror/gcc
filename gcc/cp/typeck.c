/* Build expressions with type checking for C++ compiler.
   Copyright (C) 1987, 88, 89, 92-96, 1997 Free Software Foundation, Inc.
   Hacked by Michael Tiemann (tiemann@cygnus.com)

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


/* This file is part of the C++ front end.
   It contains routines to build C++ expressions given their operands,
   including computing the types of the result, C and C++ specific error
   checks, and some optimization.

   There are also routines to build RETURN_STMT nodes and CASE_STMT nodes,
   and to process initializations in declarations (since they work
   like a strange sort of assignment).  */

#include "config.h"
#include <stdio.h>
#include "tree.h"
#include "rtl.h"
#include "cp-tree.h"
#include "flags.h"
#include "output.h"
#include "expr.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

extern void compiler_error ();

static tree convert_for_assignment PROTO((tree, tree, char*, tree,
					  int));
static tree pointer_int_sum PROTO((enum tree_code, tree, tree));
static tree rationalize_conditional_expr PROTO((enum tree_code, tree));
static int comp_target_parms PROTO((tree, tree, int));
static int comp_ptr_ttypes_real PROTO((tree, tree, int));
static int comp_ptr_ttypes_const PROTO((tree, tree));
static int comp_ptr_ttypes_reinterpret PROTO((tree, tree));
static int comp_array_types PROTO((int (*) (tree, tree, int), tree,
				   tree, int));
static tree build_ptrmemfunc1 PROTO((tree, tree, tree, tree, tree));
static tree common_base_type PROTO((tree, tree));
static tree convert_sequence PROTO((tree, tree));
static tree lookup_anon_field PROTO((tree, tree));
static tree pointer_diff PROTO((tree, tree));
static tree qualify_type PROTO((tree, tree));
static tree expand_target_expr PROTO((tree));
static tree get_delta_difference PROTO((tree, tree, int));

/* Return the target type of TYPE, which meas return T for:
   T*, T&, T[], T (...), and otherwise, just T.  */

tree
target_type (type)
     tree type;
{
  if (TREE_CODE (type) == REFERENCE_TYPE)
    type = TREE_TYPE (type);
  while (TREE_CODE (type) == POINTER_TYPE
	 || TREE_CODE (type) == ARRAY_TYPE
	 || TREE_CODE (type) == FUNCTION_TYPE
	 || TREE_CODE (type) == METHOD_TYPE
	 || TREE_CODE (type) == OFFSET_TYPE)
    type = TREE_TYPE (type);
  return type;
}

/* Do `exp = require_complete_type (exp);' to make sure exp
   does not have an incomplete type.  (That includes void types.)  */

tree
require_complete_type (value)
     tree value;
{
  tree type;

  if (processing_template_decl)
    return value;

  type = TREE_TYPE (value);

  /* First, detect a valid value with a complete type.  */
  if (TYPE_SIZE (type) != 0
      && type != void_type_node
      && ! (TYPE_LANG_SPECIFIC (type)
	    && (IS_SIGNATURE_POINTER (type) || IS_SIGNATURE_REFERENCE (type))
	    && TYPE_SIZE (SIGNATURE_TYPE (type)) == 0))
    return value;

  /* If we see X::Y, we build an OFFSET_TYPE which has
     not been laid out.  Try to avoid an error by interpreting
     it as this->X::Y, if reasonable.  */
  if (TREE_CODE (value) == OFFSET_REF
      && current_class_ref != 0
      && TREE_OPERAND (value, 0) == current_class_ref)
    {
      tree base, member = TREE_OPERAND (value, 1);
      tree basetype = TYPE_OFFSET_BASETYPE (type);
      my_friendly_assert (TREE_CODE (member) == FIELD_DECL, 305);
      base = convert_pointer_to (basetype, current_class_ptr);
      value = build (COMPONENT_REF, TREE_TYPE (member),
		     build_indirect_ref (base, NULL_PTR), member);
      return require_complete_type (value);
    }

  if (IS_AGGR_TYPE (type) && CLASSTYPE_TEMPLATE_INSTANTIATION (type))
    {
      instantiate_class_template (TYPE_MAIN_VARIANT (type));
      if (TYPE_SIZE (type) != 0)
	return value;
    }

  incomplete_type_error (value, type);
  return error_mark_node;
}

tree
complete_type (type)
     tree type;
{
  if (type == error_mark_node || TYPE_SIZE (type) != NULL_TREE)
    ;
  else if (TREE_CODE (type) == ARRAY_TYPE && TYPE_DOMAIN (type))
    {
      tree t = complete_type (TREE_TYPE (type));
      if (TYPE_SIZE (t) != NULL_TREE && ! processing_template_decl)
	layout_type (type);
      TYPE_NEEDS_CONSTRUCTING (type)
	= TYPE_NEEDS_CONSTRUCTING (TYPE_MAIN_VARIANT (t));
      TYPE_NEEDS_DESTRUCTOR (type)
	= TYPE_NEEDS_DESTRUCTOR (TYPE_MAIN_VARIANT (t));
    }
  else if (IS_AGGR_TYPE (type) && CLASSTYPE_TEMPLATE_INSTANTIATION (type))
    instantiate_class_template (TYPE_MAIN_VARIANT (type));

  return type;
}

/* Return truthvalue of whether type of EXP is instantiated.  */

int
type_unknown_p (exp)
     tree exp;
{
  return (TREE_CODE (exp) == TREE_LIST
	  || TREE_TYPE (exp) == unknown_type_node
	  || (TREE_CODE (TREE_TYPE (exp)) == OFFSET_TYPE
	      && TREE_TYPE (TREE_TYPE (exp)) == unknown_type_node));
}

/* Return truthvalue of whether T is function (or pfn) type.  */

int
fntype_p (t)
     tree t;
{
  return (TREE_CODE (t) == FUNCTION_TYPE || TREE_CODE (t) == METHOD_TYPE
	  || (TREE_CODE (t) == POINTER_TYPE
	      && (TREE_CODE (TREE_TYPE (t)) == FUNCTION_TYPE
		  || TREE_CODE (TREE_TYPE (t)) == METHOD_TYPE)));
}

/* Do `exp = require_instantiated_type (type, exp);' to make sure EXP
   does not have an uninstantiated type.
   TYPE is type to instantiate with, if uninstantiated.  */

tree
require_instantiated_type (type, exp, errval)
     tree type, exp, errval;
{
  if (TREE_TYPE (exp) == NULL_TREE)
    {
      error ("argument list may not have an initializer list");
      return errval;
    }

  if (TREE_TYPE (exp) == unknown_type_node
      || (TREE_CODE (TREE_TYPE (exp)) == OFFSET_TYPE
	  && TREE_TYPE (TREE_TYPE (exp)) == unknown_type_node))
    {
      exp = instantiate_type (type, exp, 1);
      if (TREE_TYPE (exp) == error_mark_node)
	return errval;
    }
  return exp;
}

/* Return a variant of TYPE which has all the type qualifiers of LIKE
   as well as those of TYPE.  */

static tree
qualify_type (type, like)
     tree type, like;
{
  int constflag = TYPE_READONLY (type) || TYPE_READONLY (like);
  int volflag = TYPE_VOLATILE (type) || TYPE_VOLATILE (like);
  /* @@ Must do member pointers here.  */
  return cp_build_type_variant (type, constflag, volflag);
}

/* Return the common type of two parameter lists.
   We assume that comptypes has already been done and returned 1;
   if that isn't so, this may crash.

   As an optimization, free the space we allocate if the parameter
   lists are already common.  */

tree
commonparms (p1, p2)
     tree p1, p2;
{
  tree oldargs = p1, newargs, n;
  int i, len;
  int any_change = 0;
  char *first_obj = (char *) oballoc (0);

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
	  TREE_VALUE (n) = common_type (TREE_VALUE (p1), TREE_VALUE (p2));
	}
      else
	TREE_VALUE (n) = TREE_VALUE (p1);
    }
  if (! any_change)
    {
      obfree (first_obj);
      return oldargs;
    }

  return newargs;
}

/* Return the common type of two types.
   We assume that comptypes has already been done and returned 1;
   if that isn't so, this may crash.

   This is the type for the result of most arithmetic operations
   if the operands have the given two types.

   We do not deal with enumeral types here because they have already been
   converted to integer types.  */

tree
common_type (t1, t2)
     tree t1, t2;
{
  register enum tree_code code1;
  register enum tree_code code2;
  tree attributes;

  /* Save time if the two types are the same.  */

  if (t1 == t2) return t1;

  /* If one type is nonsense, use the other.  */
  if (t1 == error_mark_node)
    return t2;
  if (t2 == error_mark_node)
    return t1;

  /* Merge the attributes */

  { register tree a1, a2;
    a1 = TYPE_ATTRIBUTES (t1);
    a2 = TYPE_ATTRIBUTES (t2);

    /* Either one unset?  Take the set one.  */

    if (!(attributes = a1))
       attributes = a2;

    /* One that completely contains the other?  Take it.  */

    else if (a2 && !attribute_list_contained (a1, a2))
       if (attribute_list_contained (a2, a1))
	  attributes = a2;
       else
	{
	  /* Pick the longest list, and hang on the other list.  */
	  /* ??? For the moment we punt on the issue of attrs with args.  */
	
	  if (list_length (a1) < list_length (a2))
	     attributes = a2, a2 = a1;

	  for (; a2; a2 = TREE_CHAIN (a2))
	    if (lookup_attribute (IDENTIFIER_POINTER (TREE_PURPOSE (a2)),
				  attributes) == NULL_TREE)
	      {
		a1 = copy_node (a2);
		TREE_CHAIN (a1) = attributes;
		attributes = a1;
	      }
	}
  }

  /* Treat an enum type as the unsigned integer type of the same width.  */

  if (TREE_CODE (t1) == ENUMERAL_TYPE)
    t1 = type_for_size (TYPE_PRECISION (t1), 1);
  if (TREE_CODE (t2) == ENUMERAL_TYPE)
    t2 = type_for_size (TYPE_PRECISION (t2), 1);

  if (TYPE_PTRMEMFUNC_P (t1))
    t1 = TYPE_PTRMEMFUNC_FN_TYPE (t1);
  if (TYPE_PTRMEMFUNC_P (t2))
    t2 = TYPE_PTRMEMFUNC_FN_TYPE (t2);

  code1 = TREE_CODE (t1);
  code2 = TREE_CODE (t2);

  /* If one type is complex, form the common type of the non-complex
     components, then make that complex.  Use T1 or T2 if it is the
     required type.  */
  if (code1 == COMPLEX_TYPE || code2 == COMPLEX_TYPE)
    {
      tree subtype1 = code1 == COMPLEX_TYPE ? TREE_TYPE (t1) : t1;
      tree subtype2 = code2 == COMPLEX_TYPE ? TREE_TYPE (t2) : t2;
      tree subtype = common_type (subtype1, subtype2);

      if (code1 == COMPLEX_TYPE && TREE_TYPE (t1) == subtype)
	return build_type_attribute_variant (t1, attributes);
      else if (code2 == COMPLEX_TYPE && TREE_TYPE (t2) == subtype)
	return build_type_attribute_variant (t2, attributes);
      else
	return build_type_attribute_variant (build_complex_type (subtype),
					     attributes);
    }

  switch (code1)
    {
    case INTEGER_TYPE:
    case REAL_TYPE:
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

      /* Same precision.  Prefer longs to ints even when same size.  */
  
      if (TYPE_MAIN_VARIANT (t1) == long_unsigned_type_node
	  || TYPE_MAIN_VARIANT (t2) == long_unsigned_type_node)
        return build_type_attribute_variant (long_unsigned_type_node,
					     attributes);

      if (TYPE_MAIN_VARIANT (t1) == long_integer_type_node
	  || TYPE_MAIN_VARIANT (t2) == long_integer_type_node)
	{
	  /* But preserve unsignedness from the other type,
	     since long cannot hold all the values of an unsigned int.  */
	  if (TREE_UNSIGNED (t1) || TREE_UNSIGNED (t2))
	     t1 = long_unsigned_type_node;
	  else
	     t1 = long_integer_type_node;
	  return build_type_attribute_variant (t1, attributes);
	}

      if (TYPE_MAIN_VARIANT (t1) == long_double_type_node
	  || TYPE_MAIN_VARIANT (t2) == long_double_type_node)
	return build_type_attribute_variant (long_double_type_node,
					     attributes);	  

      /* Otherwise prefer the unsigned one.  */

      if (TREE_UNSIGNED (t1))
	return build_type_attribute_variant (t1, attributes);
      else
	return build_type_attribute_variant (t2, attributes);

    case POINTER_TYPE:
    case REFERENCE_TYPE:
      /* For two pointers, do this recursively on the target type,
	 and combine the qualifiers of the two types' targets.  */
      /* This code was turned off; I don't know why.
 	 But ANSI C++ specifies doing this with the qualifiers.
 	 So I turned it on again.  */
      {
	tree tt1 = TYPE_MAIN_VARIANT (TREE_TYPE (t1));
	tree tt2 = TYPE_MAIN_VARIANT (TREE_TYPE (t2));
	int constp
	  = TYPE_READONLY (TREE_TYPE (t1)) || TYPE_READONLY (TREE_TYPE (t2));
	int volatilep
	  = TYPE_VOLATILE (TREE_TYPE (t1)) || TYPE_VOLATILE (TREE_TYPE (t2));
	tree target;

	if (tt1 == tt2)
	  target = tt1;
	else if (tt1 == void_type_node || tt2 == void_type_node)
	  target = void_type_node;
	else if (tt1 == unknown_type_node)
	  target = tt2;
	else if (tt2 == unknown_type_node)
	  target = tt1;
	else
	  target = common_type (tt1, tt2);

	target = cp_build_type_variant (target, constp, volatilep);
	if (code1 == POINTER_TYPE)
	  t1 = build_pointer_type (target);
	else
	  t1 = build_reference_type (target);
	t1 = build_type_attribute_variant (t1, attributes);

	if (TREE_CODE (target) == METHOD_TYPE)
	  t1 = build_ptrmemfunc_type (t1);

	return t1;
      }

    case ARRAY_TYPE:
      {
	tree elt = common_type (TREE_TYPE (t1), TREE_TYPE (t2));
	/* Save space: see if the result is identical to one of the args.  */
	if (elt == TREE_TYPE (t1) && TYPE_DOMAIN (t1))
	  return build_type_attribute_variant (t1, attributes);
	if (elt == TREE_TYPE (t2) && TYPE_DOMAIN (t2))
	  return build_type_attribute_variant (t2, attributes);
	/* Merge the element types, and have a size if either arg has one.  */
	t1 = build_cplus_array_type
	  (elt, TYPE_DOMAIN (TYPE_DOMAIN (t1) ? t1 : t2));
	return build_type_attribute_variant (t1, attributes);
      }

    case FUNCTION_TYPE:
      /* Function types: prefer the one that specified arg types.
	 If both do, merge the arg types.  Also merge the return types.  */
      {
	tree valtype = common_type (TREE_TYPE (t1), TREE_TYPE (t2));
	tree p1 = TYPE_ARG_TYPES (t1);
	tree p2 = TYPE_ARG_TYPES (t2);
	tree rval, raises;

	/* Save space: see if the result is identical to one of the args.  */
	if (valtype == TREE_TYPE (t1) && ! p2)
	  return build_type_attribute_variant (t1, attributes);
	if (valtype == TREE_TYPE (t2) && ! p1)
	  return build_type_attribute_variant (t2, attributes);

	/* Simple way if one arg fails to specify argument types.  */
	if (p1 == NULL_TREE || TREE_VALUE (p1) == void_type_node)
	  {
	    rval = build_function_type (valtype, p2);
	    if ((raises = TYPE_RAISES_EXCEPTIONS (t2)))
	      rval = build_exception_variant (rval, raises);
	    return build_type_attribute_variant (rval, attributes);
	  }
	raises = TYPE_RAISES_EXCEPTIONS (t1);
	if (p2 == NULL_TREE || TREE_VALUE (p2) == void_type_node)
	  {
	    rval = build_function_type (valtype, p1);
	    if (raises)
	      rval = build_exception_variant (rval, raises);
	    return build_type_attribute_variant (rval, attributes);
	  }

	rval = build_function_type (valtype, commonparms (p1, p2));
	rval = build_exception_variant (rval, raises);
	return build_type_attribute_variant (rval, attributes);
      }

    case RECORD_TYPE:
    case UNION_TYPE:
      my_friendly_assert (TYPE_MAIN_VARIANT (t1) == t1
			  && TYPE_MAIN_VARIANT (t2) == t2, 306);

      if (DERIVED_FROM_P (t1, t2) && binfo_or_else (t1, t2))
	return build_type_attribute_variant (t1, attributes);
      else if (binfo_or_else (t2, t1))
	return build_type_attribute_variant (t2, attributes);
      else
	compiler_error ("common_type called with uncommon aggregate types");

    case METHOD_TYPE:
      if (TREE_CODE (TREE_TYPE (t1)) == TREE_CODE (TREE_TYPE (t2)))
	{
	  /* Get this value the long way, since TYPE_METHOD_BASETYPE
	     is just the main variant of this.  */
	  tree basetype;
	  tree raises, t3;

	  tree b1 = TYPE_OFFSET_BASETYPE (t1);
	  tree b2 = TYPE_OFFSET_BASETYPE (t2);

	  if (comptypes (b1, b2, 1)
	      || (DERIVED_FROM_P (b1, b2) && binfo_or_else (b1, b2)))
	    basetype = TREE_TYPE (TREE_VALUE (TYPE_ARG_TYPES (t2)));
	  else
	    {
	      if (binfo_or_else (b2, b1) == NULL_TREE)
		compiler_error ("common_type called with uncommon method types");
	      basetype = TREE_TYPE (TREE_VALUE (TYPE_ARG_TYPES (t1)));
	    }

	  raises = TYPE_RAISES_EXCEPTIONS (t1);

	  /* If this was a member function type, get back to the
	     original type of type member function (i.e., without
	     the class instance variable up front.  */
	  t1 = build_function_type (TREE_TYPE (t1), TREE_CHAIN (TYPE_ARG_TYPES (t1)));
	  t2 = build_function_type (TREE_TYPE (t2), TREE_CHAIN (TYPE_ARG_TYPES (t2)));
	  t3 = common_type (t1, t2);
	  t3 = build_cplus_method_type (basetype, TREE_TYPE (t3), TYPE_ARG_TYPES (t3));
	  t1 = build_exception_variant (t3, raises);
	}
      else
        compiler_error ("common_type called with uncommon method types");

      return build_type_attribute_variant (t1, attributes);

    case OFFSET_TYPE:
      if (TREE_TYPE (t1) == TREE_TYPE (t2))
	{
	  tree b1 = TYPE_OFFSET_BASETYPE (t1);
	  tree b2 = TYPE_OFFSET_BASETYPE (t2);

	  if (comptypes (b1, b2, 1)
	      || (DERIVED_FROM_P (b1, b2) && binfo_or_else (b1, b2)))
	    return build_type_attribute_variant (t2, attributes);
	  else if (binfo_or_else (b2, b1))
	    return build_type_attribute_variant (t1, attributes);
	}
      compiler_error ("common_type called with uncommon member types");

    default:
      return build_type_attribute_variant (t1, attributes);
    }
}

/* Return 1 if TYPE1 and TYPE2 raise the same exceptions.  */

int
compexcepttypes (t1, t2)
     tree t1, t2;
{
  return TYPE_RAISES_EXCEPTIONS (t1) == TYPE_RAISES_EXCEPTIONS (t2);
}

static int
comp_array_types (cmp, t1, t2, strict)
     register int (*cmp) PROTO((tree, tree, int));
     tree t1, t2;
     int strict;
{
  tree d1 = TYPE_DOMAIN (t1);
  tree d2 = TYPE_DOMAIN (t2);

  /* Target types must match incl. qualifiers.  */
  if (!(TREE_TYPE (t1) == TREE_TYPE (t2)
	|| (*cmp) (TREE_TYPE (t1), TREE_TYPE (t2), strict)))
    return 0;

  /* Sizes must match unless one is missing or variable.  */
  if (d1 == 0 || d2 == 0 || d1 == d2
      || TREE_CODE (TYPE_MIN_VALUE (d1)) != INTEGER_CST
      || TREE_CODE (TYPE_MIN_VALUE (d2)) != INTEGER_CST
      || TREE_CODE (TYPE_MAX_VALUE (d1)) != INTEGER_CST
      || TREE_CODE (TYPE_MAX_VALUE (d2)) != INTEGER_CST)
    return 1;

  return ((TREE_INT_CST_LOW (TYPE_MIN_VALUE (d1))
	   == TREE_INT_CST_LOW (TYPE_MIN_VALUE (d2)))
	  && (TREE_INT_CST_HIGH (TYPE_MIN_VALUE (d1))
	      == TREE_INT_CST_HIGH (TYPE_MIN_VALUE (d2)))
	  && (TREE_INT_CST_LOW (TYPE_MAX_VALUE (d1))
	      == TREE_INT_CST_LOW (TYPE_MAX_VALUE (d2)))
	  && (TREE_INT_CST_HIGH (TYPE_MAX_VALUE (d1))
	      == TREE_INT_CST_HIGH (TYPE_MAX_VALUE (d2))));
}

/* Return 1 if TYPE1 and TYPE2 are compatible types for assignment
   or various other operations.  This is what ANSI C++ speaks of as
   "being the same".

   For C++: argument STRICT says we should be strict about this
   comparison:

	2 : strict, except that if one type is a reference and
	    the other is not, compare the target type of the
	    reference to the type that's not a reference (ARM, p308).
	    This is used for checking for invalid overloading.
	1 : strict (compared according to ANSI C)
	    This is used for checking whether two function decls match.
	0 : <= (compared according to C++)
	-1: <= or >= (relaxed)

   Otherwise, pointers involving base classes and derived classes
   can be mixed as valid: i.e. a pointer to a base class may be assigned
   to a pointer to one of its derived classes, as per C++. A pointer to
   a derived class may be passed as a parameter to a function expecting a
   pointer to a base classes. These allowances do not commute. In this
   case, TYPE1 is assumed to be the base class, and TYPE2 is assumed to
   be the derived class.  */

int
comptypes (type1, type2, strict)
     tree type1, type2;
     int strict;
{
  register tree t1 = type1;
  register tree t2 = type2;
  int attrval, val;

  /* Suppress errors caused by previously reported errors */

  if (t1 == t2)
    return 1;

  /* This should never happen.  */
  my_friendly_assert (t1 != error_mark_node, 307);

  if (t2 == error_mark_node)
    return 0;

  if (strict < 0)
    {
      /* Treat an enum type as the unsigned integer type of the same width.  */

      if (TREE_CODE (t1) == ENUMERAL_TYPE)
	t1 = type_for_size (TYPE_PRECISION (t1), 1);
      if (TREE_CODE (t2) == ENUMERAL_TYPE)
	t2 = type_for_size (TYPE_PRECISION (t2), 1);

      if (t1 == t2)
	return 1;
    }

  if (TYPE_PTRMEMFUNC_P (t1))
    t1 = TYPE_PTRMEMFUNC_FN_TYPE (t1);
  if (TYPE_PTRMEMFUNC_P (t2))
    t2 = TYPE_PTRMEMFUNC_FN_TYPE (t2);

  /* Different classes of types can't be compatible.  */

  if (TREE_CODE (t1) != TREE_CODE (t2))
    {
      if (strict == 2
	  && ((TREE_CODE (t1) == REFERENCE_TYPE)
	      ^ (TREE_CODE (t2) == REFERENCE_TYPE)))
	{
	  if (TREE_CODE (t1) == REFERENCE_TYPE)
	    return comptypes (TREE_TYPE (t1), t2, 1);
	  return comptypes (t1, TREE_TYPE (t2), 1);
	}

      return 0;
    }
  if (strict > 1)
    strict = 1;

  /* Qualifiers must match.  */

  if (TYPE_READONLY (t1) != TYPE_READONLY (t2))
    return 0;
  if (TYPE_VOLATILE (t1) != TYPE_VOLATILE (t2))
    return 0;

  /* Allow for two different type nodes which have essentially the same
     definition.  Note that we already checked for equality of the type
     type qualifiers (just above).  */

  if (TYPE_MAIN_VARIANT (t1) == TYPE_MAIN_VARIANT (t2))
    return 1;

  /* ??? COMP_TYPE_ATTRIBUTES is currently useless for variables as each
     attribute is its own main variant (`val' will remain 0).  */
#ifndef COMP_TYPE_ATTRIBUTES
#define COMP_TYPE_ATTRIBUTES(t1,t2)	1
#endif

  /* 1 if no need for warning yet, 2 if warning cause has been seen.  */
  if (! (attrval = COMP_TYPE_ATTRIBUTES (t1, t2)))
     return 0;

  /* 1 if no need for warning yet, 2 if warning cause has been seen.  */
  val = 0;

  switch (TREE_CODE (t1))
    {
    case RECORD_TYPE:
    case UNION_TYPE:
      if (CLASSTYPE_TEMPLATE_INFO (t1) && CLASSTYPE_TEMPLATE_INFO (t2)
	  && CLASSTYPE_TI_TEMPLATE (t1) == CLASSTYPE_TI_TEMPLATE (t2))
	{
	  int i = TREE_VEC_LENGTH (CLASSTYPE_TI_ARGS (t1));
	  tree *p1 = &TREE_VEC_ELT (CLASSTYPE_TI_ARGS (t1), 0);
	  tree *p2 = &TREE_VEC_ELT (CLASSTYPE_TI_ARGS (t2), 0);
	
	  while (i--)
	    {
	      if (TREE_CODE_CLASS (TREE_CODE (p1[i])) == 't')
		{
		  if (! comptypes (p1[i], p2[i], 1))
		    return 0;
		}
	      else
		{
		  if (simple_cst_equal (p1[i], p2[i]) <= 0)
		    return 0;
		}
	    }
	  return 1;
	}
      if (strict <= 0)
	goto look_hard;
      return 0;

    case OFFSET_TYPE:
      val = (comptypes (build_pointer_type (TYPE_OFFSET_BASETYPE (t1)),
			build_pointer_type (TYPE_OFFSET_BASETYPE (t2)), strict)
	     && comptypes (TREE_TYPE (t1), TREE_TYPE (t2), strict));
      break;

    case METHOD_TYPE:
      if (! compexcepttypes (t1, t2))
	return 0;

      /* This case is anti-symmetrical!
	 One can pass a base member (or member function)
	 to something expecting a derived member (or member function),
	 but not vice-versa!  */

      val = (comptypes (TREE_TYPE (t1), TREE_TYPE (t2), strict)
	     && compparms (TYPE_ARG_TYPES (t1),
			   TYPE_ARG_TYPES (t2), strict));
      break;

    case POINTER_TYPE:
    case REFERENCE_TYPE:
      t1 = TREE_TYPE (t1);
      t2 = TREE_TYPE (t2);
      if (t1 == t2)
	{
	  val = 1;
	  break;
	}
      if (strict <= 0)
	{
	  if (TREE_CODE (t1) == RECORD_TYPE && TREE_CODE (t2) == RECORD_TYPE)
	    {
	      int rval;
	    look_hard:
	      rval = t1 == t2 || DERIVED_FROM_P (t1, t2);

	      if (rval)
		{
		  val = 1;
		  break;
		}
	      if (strict < 0)
		{
		  val = DERIVED_FROM_P (t2, t1);
		  break;
		}
	    }
	  return 0;
	}
      else
	val = comptypes (t1, t2, strict);
      break;

    case FUNCTION_TYPE:
      if (! compexcepttypes (t1, t2))
	return 0;

      val = ((TREE_TYPE (t1) == TREE_TYPE (t2)
	      || comptypes (TREE_TYPE (t1), TREE_TYPE (t2), strict))
	     && compparms (TYPE_ARG_TYPES (t1), TYPE_ARG_TYPES (t2), strict));
      break;

    case ARRAY_TYPE:
      /* Target types must match incl. qualifiers.  */
      val = comp_array_types (comptypes, t1, t2, strict);
      break;

    case TEMPLATE_TYPE_PARM:
      return TEMPLATE_TYPE_IDX (t1) == TEMPLATE_TYPE_IDX (t2)
	&& TEMPLATE_TYPE_LEVEL (t1) == TEMPLATE_TYPE_LEVEL (t2);

    case TYPENAME_TYPE:
      if (TYPE_IDENTIFIER (t1) != TYPE_IDENTIFIER (t2))
	return 0;
      return comptypes (TYPE_CONTEXT (t1), TYPE_CONTEXT (t2), 1);
    }
  return attrval == 2 && val == 1 ? 2 : val;
}

/* Return 1 if TTL and TTR are pointers to types that are equivalent,
   ignoring their qualifiers.

   NPTRS is the number of pointers we can strip off and keep cool.
   This is used to permit (for aggr A, aggr B) A, B* to convert to A*,
   but to not permit B** to convert to A**.  */

int
comp_target_types (ttl, ttr, nptrs)
     tree ttl, ttr;
     int nptrs;
{
  ttl = TYPE_MAIN_VARIANT (ttl);
  ttr = TYPE_MAIN_VARIANT (ttr);
  if (ttl == ttr)
    return 1;

  if (TREE_CODE (ttr) != TREE_CODE (ttl))
    return 0;

  if (TREE_CODE (ttr) == POINTER_TYPE)
    {
      ttl = TREE_TYPE (ttl);
      ttr = TREE_TYPE (ttr);

      if (nptrs > 0)
	{
	  if (TREE_CODE (ttl) == UNKNOWN_TYPE
	      || TREE_CODE (ttr) == UNKNOWN_TYPE)
	    return 1;
	  else if (TREE_CODE (ttl) == VOID_TYPE
		   && TREE_CODE (ttr) != FUNCTION_TYPE
		   && TREE_CODE (ttr) != METHOD_TYPE
		   && TREE_CODE (ttr) != OFFSET_TYPE)
	    return 1;
	  else if (TREE_CODE (ttr) == VOID_TYPE
		   && TREE_CODE (ttl) != FUNCTION_TYPE
		   && TREE_CODE (ttl) != METHOD_TYPE
		   && TREE_CODE (ttl) != OFFSET_TYPE)
	    return -1;
	  else if (TREE_CODE (ttl) == POINTER_TYPE
		   || TREE_CODE (ttl) == ARRAY_TYPE)
	    {
	      if (comp_ptr_ttypes (ttl, ttr))
		return 1;
	      else if (comp_ptr_ttypes (ttr, ttl))
		return -1;
	      return 0;
	    }
	}

      /* Const and volatile mean something different for function types,
	 so the usual checks are not appropriate.  */
      if (TREE_CODE (ttl) == FUNCTION_TYPE || TREE_CODE (ttl) == METHOD_TYPE)
	return comp_target_types (ttl, ttr, nptrs - 1);

      /* Make sure that the cv-quals change only in the same direction as
	 the target type.  */
      {
	int t;
	int c = TYPE_READONLY (ttl) - TYPE_READONLY (ttr);
	int v = TYPE_VOLATILE (ttl) - TYPE_VOLATILE (ttr);

	if ((c > 0 && v < 0) || (c < 0 && v > 0))
	  return 0;

	if (TYPE_MAIN_VARIANT (ttl) == TYPE_MAIN_VARIANT (ttr))
	  return (c + v < 0) ? -1 : 1;

	t = comp_target_types (ttl, ttr, nptrs - 1);
	if ((t == 1 && c + v >= 0) || (t == -1 && c + v <= 0))
	  return t;

	return 0;
      }
    }

  if (TREE_CODE (ttr) == REFERENCE_TYPE)
    return comp_target_types (TREE_TYPE (ttl), TREE_TYPE (ttr), nptrs);
  if (TREE_CODE (ttr) == ARRAY_TYPE)
    return comp_array_types (comp_target_types, ttl, ttr, 0);
  else if (TREE_CODE (ttr) == FUNCTION_TYPE || TREE_CODE (ttr) == METHOD_TYPE)
    if (comp_target_types (TREE_TYPE (ttl), TREE_TYPE (ttr), nptrs))
      switch (comp_target_parms (TYPE_ARG_TYPES (ttl), TYPE_ARG_TYPES (ttr), 1))
	{
	case 0:
	  return 0;
	case 1:
	  return 1;
	case 2:
	  return -1;
	default:
	  my_friendly_abort (112);
	}
    else
      return 0;

  /* for C++ */
  else if (TREE_CODE (ttr) == OFFSET_TYPE)
    {
      /* Contravariance: we can assign a pointer to base member to a pointer
	 to derived member.  Note difference from simple pointer case, where
	 we can pass a pointer to derived to a pointer to base.  */
      if (comptypes (TYPE_OFFSET_BASETYPE (ttr), TYPE_OFFSET_BASETYPE (ttl), 0))
	return comp_target_types (TREE_TYPE (ttl), TREE_TYPE (ttr), nptrs);
      else if (comptypes (TYPE_OFFSET_BASETYPE (ttl), TYPE_OFFSET_BASETYPE (ttr), 0)
	       && comp_target_types (TREE_TYPE (ttl), TREE_TYPE (ttr), nptrs))
	return -1;
    }
  else if (IS_AGGR_TYPE (ttl))
    {
      if (nptrs < 0)
	return 0;
      if (comptypes (build_pointer_type (ttl), build_pointer_type (ttr), 0))
	return 1;
      if (comptypes (build_pointer_type (ttr), build_pointer_type (ttl), 0))
	return -1;
      return 0;
    }

  return 0;
}

/* If two types share a common base type, return that basetype.
   If there is not a unique most-derived base type, this function
   returns ERROR_MARK_NODE.  */

static tree
common_base_type (tt1, tt2)
     tree tt1, tt2;
{
  tree best = NULL_TREE;
  int i;

  /* If one is a baseclass of another, that's good enough.  */
  if (UNIQUELY_DERIVED_FROM_P (tt1, tt2))
    return tt1;
  if (UNIQUELY_DERIVED_FROM_P (tt2, tt1))
    return tt2;

  /* Otherwise, try to find a unique baseclass of TT1
     that is shared by TT2, and follow that down.  */
  for (i = CLASSTYPE_N_BASECLASSES (tt1)-1; i >= 0; i--)
    {
      tree basetype = TYPE_BINFO_BASETYPE (tt1, i);
      tree trial = common_base_type (basetype, tt2);
      if (trial)
	{
	  if (trial == error_mark_node)
	    return trial;
	  if (best == NULL_TREE)
	    best = trial;
	  else if (best != trial)
	    return error_mark_node;
	}
    }

  /* Same for TT2.  */
  for (i = CLASSTYPE_N_BASECLASSES (tt2)-1; i >= 0; i--)
    {
      tree basetype = TYPE_BINFO_BASETYPE (tt2, i);
      tree trial = common_base_type (tt1, basetype);
      if (trial)
	{
	  if (trial == error_mark_node)
	    return trial;
	  if (best == NULL_TREE)
	    best = trial;
	  else if (best != trial)
	    return error_mark_node;
	}
    }
  return best;
}

/* Subroutines of `comptypes'.  */

/* Return 1 if two parameter type lists PARMS1 and PARMS2
   are equivalent in the sense that functions with those parameter types
   can have equivalent types.
   If either list is empty, we win.
   Otherwise, the two lists must be equivalent, element by element.

   C++: See comment above about TYPE1, TYPE2, STRICT.
   If STRICT == 3, it means checking is strict, but do not compare
   default parameter values.  */

int
compparms (parms1, parms2, strict)
     tree parms1, parms2;
     int strict;
{
  register tree t1 = parms1, t2 = parms2;

  /* An unspecified parmlist matches any specified parmlist
     whose argument types don't need default promotions.  */

  if (strict <= 0 && t1 == 0)
	return self_promoting_args_p (t2);
  if (strict < 0 && t2 == 0)
	return self_promoting_args_p (t1);

  while (1)
    {
      if (t1 == 0 && t2 == 0)
	return 1;
      /* If one parmlist is shorter than the other,
	 they fail to match, unless STRICT is <= 0.  */
      if (t1 == 0 || t2 == 0)
	{
	  if (strict > 0)
	    return 0;
	  if (strict < 0)
	    return 1;
	  if (strict == 0)
	    return t1 && TREE_PURPOSE (t1);
	}
      if (! comptypes (TREE_VALUE (t2), TREE_VALUE (t1), strict))
	{
	  if (strict > 0)
	    return 0;
	  if (strict == 0)
	    return t2 == void_list_node && TREE_PURPOSE (t1);
	  return TREE_PURPOSE (t1) || TREE_PURPOSE (t2);
	}

      t1 = TREE_CHAIN (t1);
      t2 = TREE_CHAIN (t2);
    }
}

/* This really wants return whether or not parameter type lists
   would make their owning functions assignment compatible or not.  */

static int
comp_target_parms (parms1, parms2, strict)
     tree parms1, parms2;
     int strict;
{
  register tree t1 = parms1, t2 = parms2;
  int warn_contravariance = 0;

  /* An unspecified parmlist matches any specified parmlist
     whose argument types don't need default promotions.
     @@@ see 13.3.3 for a counterexample...  */

  if (t1 == 0 && t2 != 0)
    {
      cp_pedwarn ("ANSI C++ prohibits conversion from `(%#T)' to `(...)'",
		  parms2);
      return self_promoting_args_p (t2);
    }
  if (t2 == 0)
    return self_promoting_args_p (t1);

  for (; t1 || t2; t1 = TREE_CHAIN (t1), t2 = TREE_CHAIN (t2))
    {
      tree p1, p2;

      /* If one parmlist is shorter than the other,
	 they fail to match, unless STRICT is <= 0.  */
      if (t1 == 0 || t2 == 0)
	{
	  if (strict > 0)
	    return 0;
	  if (strict < 0)
	    return 1 + warn_contravariance;
	  return ((t1 && TREE_PURPOSE (t1)) + warn_contravariance);
	}
      p1 = TREE_VALUE (t1);
      p2 = TREE_VALUE (t2);
      if (p1 == p2)
	continue;

      if ((TREE_CODE (p1) == POINTER_TYPE && TREE_CODE (p2) == POINTER_TYPE)
	  || (TREE_CODE (p1) == REFERENCE_TYPE && TREE_CODE (p2) == REFERENCE_TYPE))
	{
	  if (strict <= 0
	      && (TYPE_MAIN_VARIANT (TREE_TYPE (p1))
		  == TYPE_MAIN_VARIANT (TREE_TYPE (p2))))
	    continue;

	  /* The following is wrong for contravariance,
	     but many programs depend on it.  */
	  if (TREE_TYPE (p1) == void_type_node)
	    continue;
	  if (TREE_TYPE (p2) == void_type_node)
	    {
	      warn_contravariance = 1;
	      continue;
	    }
	  if (IS_AGGR_TYPE (TREE_TYPE (p1)))
	    {
	      if (comptypes (p2, p1, 0) == 0)
		{
		  if (comptypes (p1, p2, 0) != 0)
		    warn_contravariance = 1;
		  else
		    return 0;
		}
	      continue;
	    }
	}
      /* Note backwards order due to contravariance.  */
      if (comp_target_types (p2, p1, 1) == 0)
	{
	  if (comp_target_types (p1, p2, 1))
	    {
	      warn_contravariance = 1;
	      continue;
	    }
	  if (strict != 0)
	    return 0;
	}
      /* Target types are compatible--just make sure that if
	 we use parameter lists, that they are ok as well.  */
      if (TREE_CODE (p1) == FUNCTION_TYPE || TREE_CODE (p1) == METHOD_TYPE)
	switch (comp_target_parms (TYPE_ARG_TYPES (p1),
				   TYPE_ARG_TYPES (p2),
				   strict))
	  {
	  case 0:
	    return 0;
	  case 1:
	    break;
	  case 2:
	    warn_contravariance = 1;
	  }

      if (TREE_PURPOSE (t1) && TREE_PURPOSE (t2))
	{
	  int cmp = simple_cst_equal (TREE_PURPOSE (t1), TREE_PURPOSE (t2));
	  if (cmp < 0)
	    my_friendly_abort (114);
	  if (cmp == 0)
	    return 0;
	}
    }
  return 1 + warn_contravariance;
}

/* Return 1 if PARMS specifies a fixed number of parameters
   and none of their types is affected by default promotions.  */

int
self_promoting_args_p (parms)
     tree parms;
{
  register tree t;
  for (t = parms; t; t = TREE_CHAIN (t))
    {
      register tree type = TREE_VALUE (t);

      if (TREE_CHAIN (t) == 0 && type != void_type_node)
	return 0;

      if (type == 0)
	return 0;

      if (TYPE_MAIN_VARIANT (type) == float_type_node)
	return 0;

      if (C_PROMOTING_INTEGER_TYPE_P (type))
	return 0;
    }
  return 1;
}

/* Return an unsigned type the same as TYPE in other respects.

   C++: must make these work for type variants as well.  */

tree
unsigned_type (type)
     tree type;
{
  tree type1 = TYPE_MAIN_VARIANT (type);
  if (type1 == signed_char_type_node || type1 == char_type_node)
    return unsigned_char_type_node;
  if (type1 == integer_type_node)
    return unsigned_type_node;
  if (type1 == short_integer_type_node)
    return short_unsigned_type_node;
  if (type1 == long_integer_type_node)
    return long_unsigned_type_node;
  if (type1 == long_long_integer_type_node)
    return long_long_unsigned_type_node;
  if (type1 == intDI_type_node)
    return unsigned_intDI_type_node;
  if (type1 == intSI_type_node)
    return unsigned_intSI_type_node;
  if (type1 == intHI_type_node)
    return unsigned_intHI_type_node;
  if (type1 == intQI_type_node)
    return unsigned_intQI_type_node;

  return signed_or_unsigned_type (1, type);
}

/* Return a signed type the same as TYPE in other respects.  */

tree
signed_type (type)
     tree type;
{
  tree type1 = TYPE_MAIN_VARIANT (type);
  if (type1 == unsigned_char_type_node || type1 == char_type_node)
    return signed_char_type_node;
  if (type1 == unsigned_type_node)
    return integer_type_node;
  if (type1 == short_unsigned_type_node)
    return short_integer_type_node;
  if (type1 == long_unsigned_type_node)
    return long_integer_type_node;
  if (type1 == long_long_unsigned_type_node)
    return long_long_integer_type_node;
  if (type1 == unsigned_intDI_type_node)
    return intDI_type_node;
  if (type1 == unsigned_intSI_type_node)
    return intSI_type_node;
  if (type1 == unsigned_intHI_type_node)
    return intHI_type_node;
  if (type1 == unsigned_intQI_type_node)
    return intQI_type_node;

  return signed_or_unsigned_type (0, type);
}

/* Return a type the same as TYPE except unsigned or
   signed according to UNSIGNEDP.  */

tree
signed_or_unsigned_type (unsignedp, type)
     int unsignedp;
     tree type;
{
  if (! INTEGRAL_TYPE_P (type)
      || TREE_UNSIGNED (type) == unsignedp)
    return type;

  if (TYPE_PRECISION (type) == TYPE_PRECISION (signed_char_type_node))
    return unsignedp ? unsigned_char_type_node : signed_char_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (integer_type_node)) 
    return unsignedp ? unsigned_type_node : integer_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (short_integer_type_node)) 
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (long_integer_type_node)) 
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (long_long_integer_type_node)) 
    return (unsignedp ? long_long_unsigned_type_node
	    : long_long_integer_type_node);
  return type;
}

/* Compute the value of the `sizeof' operator.  */

tree
c_sizeof (type)
     tree type;
{
  enum tree_code code = TREE_CODE (type);
  tree t;

  if (processing_template_decl)
    return build_min (SIZEOF_EXPR, sizetype, type);

  if (code == FUNCTION_TYPE)
    {
      if (pedantic || warn_pointer_arith)
	pedwarn ("ANSI C++ forbids taking the sizeof a function type");
      return size_int (1);
    }
  if (code == METHOD_TYPE)
    {
      if (pedantic || warn_pointer_arith)
	pedwarn ("ANSI C++ forbids taking the sizeof a method type");
      return size_int (1);
    }
  if (code == VOID_TYPE)
    {
      if (pedantic || warn_pointer_arith)
	pedwarn ("ANSI C++ forbids taking the sizeof a void type");
      return size_int (1);
    }
  if (code == ERROR_MARK)
    return size_int (1);

  /* ARM $5.3.2: ``When applied to a reference, the result is the size of the
     referenced object.'' */
  if (code == REFERENCE_TYPE)
    type = TREE_TYPE (type);

  /* We couldn't find anything in the ARM or the draft standard that says,
     one way or the other, if doing sizeof on something that doesn't have
     an object associated with it is correct or incorrect.  For example, if
     you declare `struct S { char str[16]; };', and in your program do
     a `sizeof (S::str)', should we flag that as an error or should we give
     the size of it?  Since it seems like a reasonable thing to do, we'll go
     with giving the value.  */
  if (code == OFFSET_TYPE)
    type = TREE_TYPE (type);

  /* @@ This also produces an error for a signature ref.
        In that case we should be able to do better.  */
  if (IS_SIGNATURE (type))
    {
      error ("`sizeof' applied to a signature type");
      return size_int (0);
    }

  if (TYPE_SIZE (complete_type (type)) == 0)
    {
      cp_error ("`sizeof' applied to incomplete type `%T'", type);
      return size_int (0);
    }

  /* Convert in case a char is more than one unit.  */
  t = size_binop (CEIL_DIV_EXPR, TYPE_SIZE (type), 
		  size_int (TYPE_PRECISION (char_type_node)));
  /* size_binop does not put the constant in range, so do it now.  */
  if (TREE_CODE (t) == INTEGER_CST && force_fit_type (t, 0))
    TREE_CONSTANT_OVERFLOW (t) = TREE_OVERFLOW (t) = 1;
  return t;
}

tree
expr_sizeof (e)
     tree e;
{
  if (processing_template_decl)
    return build_min (SIZEOF_EXPR, sizetype, e);

  if (TREE_CODE (e) == COMPONENT_REF
      && DECL_BIT_FIELD (TREE_OPERAND (e, 1)))
    error ("sizeof applied to a bit-field");
  /* ANSI says arrays and functions are converted inside comma.
     But we can't really convert them in build_compound_expr
     because that would break commas in lvalues.
     So do the conversion here if operand was a comma.  */
  if (TREE_CODE (e) == COMPOUND_EXPR
      && (TREE_CODE (TREE_TYPE (e)) == ARRAY_TYPE
	  || TREE_CODE (TREE_TYPE (e)) == FUNCTION_TYPE))
    e = default_conversion (e);
  else if (TREE_CODE (e) == TREE_LIST)
    {
      tree t = TREE_VALUE (e);
      if (t != NULL_TREE
	  && ((TREE_TYPE (t)
	       && TREE_CODE (TREE_TYPE (t)) == FUNCTION_TYPE)
	      || is_overloaded_fn (t)))
	pedwarn ("ANSI C++ forbids taking the sizeof a function type");
    }
  return c_sizeof (TREE_TYPE (e));
}
  
tree
c_sizeof_nowarn (type)
     tree type;
{
  enum tree_code code = TREE_CODE (type);
  tree t;

  if (code == FUNCTION_TYPE
      || code == METHOD_TYPE
      || code == VOID_TYPE
      || code == ERROR_MARK)
    return size_int (1);
  if (code == REFERENCE_TYPE)
    type = TREE_TYPE (type);

  if (TYPE_SIZE (type) == 0)
    return size_int (0);

  /* Convert in case a char is more than one unit.  */
  t = size_binop (CEIL_DIV_EXPR, TYPE_SIZE (type), 
		  size_int (TYPE_PRECISION (char_type_node)));
  force_fit_type (t, 0);
  return t;
}

/* Implement the __alignof keyword: Return the minimum required
   alignment of TYPE, measured in bytes.  */

tree
c_alignof (type)
     tree type;
{
  enum tree_code code = TREE_CODE (type);
  tree t;

  if (code == FUNCTION_TYPE || code == METHOD_TYPE)
    return size_int (FUNCTION_BOUNDARY / BITS_PER_UNIT);

  if (code == VOID_TYPE || code == ERROR_MARK)
    return size_int (1);

  /* C++: this is really correct!  */
  if (code == REFERENCE_TYPE)
    type = TREE_TYPE (type);

  /* @@ This also produces an error for a signature ref.
        In that case we should be able to do better.  */
  if (IS_SIGNATURE (type))
    {
      error ("`__alignof' applied to a signature type");
      return size_int (1);
    }

  t = size_int (TYPE_ALIGN (type) / BITS_PER_UNIT);
  force_fit_type (t, 0);
  return t;
}

/* Perform default promotions for C data used in expressions.
   Arrays and functions are converted to pointers;
   enumeral types or short or char, to int.
   In addition, manifest constants symbols are replaced by their values.

   C++: this will automatically bash references to their target type.  */

tree
decay_conversion (exp)
     tree exp;
{
  register tree type = TREE_TYPE (exp);
  register enum tree_code code = TREE_CODE (type);

  if (code == OFFSET_TYPE)
    {
      if (TREE_CODE (exp) == OFFSET_REF)
	return decay_conversion (resolve_offset_ref (exp));

      type = TREE_TYPE (type);
      code = TREE_CODE (type);

      if (type == unknown_type_node)
	{
	  cp_pedwarn ("assuming & on overloaded member function");
	  return build_unary_op (ADDR_EXPR, exp, 0);
	}
    }

  if (code == REFERENCE_TYPE)
    {
      exp = convert_from_reference (exp);
      type = TREE_TYPE (exp);
      code = TREE_CODE (type);
    }

  /* Constants can be used directly unless they're not loadable.  */
  if (TREE_CODE (exp) == CONST_DECL)
    exp = DECL_INITIAL (exp);
  /* Replace a nonvolatile const static variable with its value.  */
  else if (TREE_READONLY_DECL_P (exp))
    {
      exp = decl_constant_value (exp);
      type = TREE_TYPE (exp);
    }

  /* build_c_cast puts on a NOP_EXPR to make the result not an lvalue.
     Leave such NOP_EXPRs, since RHS is being used in non-lvalue context.  */

  if (code == VOID_TYPE)
    {
      error ("void value not ignored as it ought to be");
      return error_mark_node;
    }
  if (code == FUNCTION_TYPE)
    {
      return build_unary_op (ADDR_EXPR, exp, 0);
    }
  if (code == METHOD_TYPE)
    {
      cp_pedwarn ("assuming & on `%E'", exp);
      return build_unary_op (ADDR_EXPR, exp, 0);
    }
  if (code == ARRAY_TYPE)
    {
      register tree adr;
      tree restype;
      tree ptrtype;
      int constp, volatilep;

      if (TREE_CODE (exp) == INDIRECT_REF)
	{
	  /* Stripping away the INDIRECT_REF is not the right
	     thing to do for references...  */
	  tree inner = TREE_OPERAND (exp, 0);
	  if (TREE_CODE (TREE_TYPE (inner)) == REFERENCE_TYPE)
	    {
	      inner = build1 (CONVERT_EXPR,
			      build_pointer_type (TREE_TYPE (TREE_TYPE (inner))),
			      inner);
	      TREE_CONSTANT (inner) = TREE_CONSTANT (TREE_OPERAND (inner, 0));
	    }
	  return cp_convert (build_pointer_type (TREE_TYPE (type)), inner);
	}

      if (TREE_CODE (exp) == COMPOUND_EXPR)
	{
	  tree op1 = decay_conversion (TREE_OPERAND (exp, 1));
	  return build (COMPOUND_EXPR, TREE_TYPE (op1),
			TREE_OPERAND (exp, 0), op1);
	}

      if (!lvalue_p (exp)
	  && ! (TREE_CODE (exp) == CONSTRUCTOR && TREE_STATIC (exp)))
	{
	  error ("invalid use of non-lvalue array");
	  return error_mark_node;
	}

      constp = volatilep = 0;
      if (TREE_CODE_CLASS (TREE_CODE (exp)) == 'r'
	  || TREE_CODE_CLASS (TREE_CODE (exp)) == 'd')
	{
	  constp = TREE_READONLY (exp);
	  volatilep = TREE_THIS_VOLATILE (exp);
	}

      restype = TREE_TYPE (type);
      if (TYPE_READONLY (type) || TYPE_VOLATILE (type)
	  || constp || volatilep)
	restype = cp_build_type_variant (restype,
					TYPE_READONLY (type) || constp,
					TYPE_VOLATILE (type) || volatilep);
      ptrtype = build_pointer_type (restype);

      if (TREE_CODE (exp) == VAR_DECL)
	{
	  /* ??? This is not really quite correct
	     in that the type of the operand of ADDR_EXPR
	     is not the target type of the type of the ADDR_EXPR itself.
	     Question is, can this lossage be avoided?  */
	  adr = build1 (ADDR_EXPR, ptrtype, exp);
	  if (mark_addressable (exp) == 0)
	    return error_mark_node;
	  TREE_CONSTANT (adr) = staticp (exp);
	  TREE_SIDE_EFFECTS (adr) = 0;   /* Default would be, same as EXP.  */
	  return adr;
	}
      /* This way is better for a COMPONENT_REF since it can
	 simplify the offset for a component.  */
      adr = build_unary_op (ADDR_EXPR, exp, 1);
      return cp_convert (ptrtype, adr);
    }

  return exp;
}

tree
default_conversion (exp)
     tree exp;
{
  tree type;
  enum tree_code code;

  exp = decay_conversion (exp);

  type = TREE_TYPE (exp);
  code = TREE_CODE (type);

  if (INTEGRAL_CODE_P (code))
    {
      tree t = type_promotes_to (type);
      if (t != type)
	return cp_convert (t, exp);
    }

  return exp;
}

/* Take the address of an inline function without setting TREE_ADDRESSABLE
   or TREE_USED.  */

tree
inline_conversion (exp)
     tree exp;
{
  if (TREE_CODE (exp) == FUNCTION_DECL)
    {
      tree type = build_type_variant
	(TREE_TYPE (exp), TREE_READONLY (exp), TREE_THIS_VOLATILE (exp));
      exp = build1 (ADDR_EXPR, build_pointer_type (type), exp);
    }
  return exp;
}

tree
build_object_ref (datum, basetype, field)
     tree datum, basetype, field;
{
  tree dtype;
  if (datum == error_mark_node)
    return error_mark_node;

  dtype = TREE_TYPE (datum);
  if (TREE_CODE (dtype) == REFERENCE_TYPE)
    dtype = TREE_TYPE (dtype);
  if (! IS_AGGR_TYPE_CODE (TREE_CODE (dtype)))
    {
      cp_error ("request for member `%T::%D' in expression of non-aggregate type `%T'",
		basetype, field, dtype);
      return error_mark_node;
    }
  else if (IS_SIGNATURE (basetype))
    {
      warning ("signature name in scope resolution ignored");
      return build_component_ref (datum, field, NULL_TREE, 1);
    }
  else if (is_aggr_type (basetype, 1))
    {
      tree binfo = binfo_or_else (basetype, dtype);
      if (binfo)
	return build_x_component_ref (build_scoped_ref (datum, basetype),
				      field, binfo, 1);
    }
  return error_mark_node;
}

/* Like `build_component_ref, but uses an already found field, and converts
   from a reference.  Must compute access for current_class_ref.
   Otherwise, ok.  */

tree
build_component_ref_1 (datum, field, protect)
     tree datum, field;
     int protect;
{
  return convert_from_reference
    (build_component_ref (datum, field, NULL_TREE, protect));
}

/* Given a COND_EXPR, MIN_EXPR, or MAX_EXPR in T, return it in a form that we
   can, for example, use as an lvalue.  This code used to be in
   unary_complex_lvalue, but we needed it to deal with `a = (d == c) ? b : c'
   expressions, where we're dealing with aggregates.  But now it's again only
   called from unary_complex_lvalue.  The case (in particular) that led to
   this was with CODE == ADDR_EXPR, since it's not an lvalue when we'd
   get it there.  */

static tree
rationalize_conditional_expr (code, t)
     enum tree_code code;
     tree t;
{
  /* For MIN_EXPR or MAX_EXPR, fold-const.c has arranged things so that
     the first operand is always the one to be used if both operands
     are equal, so we know what conditional expression this used to be.  */
  if (TREE_CODE (t) == MIN_EXPR || TREE_CODE (t) == MAX_EXPR)
    {
      return
	build_conditional_expr (build_x_binary_op ((TREE_CODE (t) == MIN_EXPR
						    ? LE_EXPR : GE_EXPR),
						   TREE_OPERAND (t, 0),
						   TREE_OPERAND (t, 1)),
			    build_unary_op (code, TREE_OPERAND (t, 0), 0),
			    build_unary_op (code, TREE_OPERAND (t, 1), 0));
    }

  return
    build_conditional_expr (TREE_OPERAND (t, 0),
			    build_unary_op (code, TREE_OPERAND (t, 1), 0),
			    build_unary_op (code, TREE_OPERAND (t, 2), 0));
}

/* Given the TYPE of an anonymous union field inside T, return the
   FIELD_DECL for the field.  If not found return NULL_TREE.  Because
   anonymous unions can nest, we must also search all anonymous unions
   that are directly reachable.  */

static tree
lookup_anon_field (t, type)
     tree t, type;
{
  tree field;

  for (field = TYPE_FIELDS (t); field; field = TREE_CHAIN (field))
    {
      if (TREE_STATIC (field))
	continue;
      if (TREE_CODE (field) != FIELD_DECL)
	continue;

      /* If we find it directly, return the field.  */
      if (DECL_NAME (field) == NULL_TREE
	  && type == TREE_TYPE (field))
	{
	  return field;
	}

      /* Otherwise, it could be nested, search harder.  */
      if (DECL_NAME (field) == NULL_TREE
	  && TREE_CODE (TREE_TYPE (field)) == UNION_TYPE)
	{
	  tree subfield = lookup_anon_field (TREE_TYPE (field), type);
	  if (subfield)
	    return subfield;
	}
    }
  return NULL_TREE;
}

/* Build a COMPONENT_REF for a given DATUM, and it's member COMPONENT.
   COMPONENT can be an IDENTIFIER_NODE that is the name of the member
   that we are interested in, or it can be a FIELD_DECL.  */

tree
build_component_ref (datum, component, basetype_path, protect)
     tree datum, component, basetype_path;
     int protect;
{
  register tree basetype = TREE_TYPE (datum);
  register enum tree_code code;
  register tree field = NULL;
  register tree ref;

  if (processing_template_decl)
    return build_min_nt (COMPONENT_REF, datum, component);

  /* If DATUM is a COMPOUND_EXPR or COND_EXPR, move our reference
     inside it.  */
  switch (TREE_CODE (datum))
    {
    case COMPOUND_EXPR:
      {
	tree value = build_component_ref (TREE_OPERAND (datum, 1), component,
					  basetype_path, protect);
	return build (COMPOUND_EXPR, TREE_TYPE (value),
		      TREE_OPERAND (datum, 0), value);
      }
    case COND_EXPR:
      return build_conditional_expr
	(TREE_OPERAND (datum, 0),
	 build_component_ref (TREE_OPERAND (datum, 1), component,
			      basetype_path, protect),
	 build_component_ref (TREE_OPERAND (datum, 2), component,
			      basetype_path, protect));
    }

  code = TREE_CODE (basetype);

  if (code == REFERENCE_TYPE)
    {
      datum = convert_from_reference (datum);
      basetype = TREE_TYPE (datum);
      code = TREE_CODE (basetype);
    }
  if (TREE_CODE (datum) == OFFSET_REF)
    {
      datum = resolve_offset_ref (datum);
      basetype = TREE_TYPE (datum);
      code = TREE_CODE (basetype);
    }

  /* First, see if there is a field or component with name COMPONENT.  */
  if (TREE_CODE (component) == TREE_LIST)
    {
      my_friendly_assert (!(TREE_CHAIN (component) == NULL_TREE
		&& DECL_CHAIN (TREE_VALUE (component)) == NULL_TREE), 309);
      return build (COMPONENT_REF, TREE_TYPE (component), datum, component);
    }

  if (! IS_AGGR_TYPE_CODE (code))
    {
      if (code != ERROR_MARK)
	cp_error ("request for member `%D' in `%E', which is of non-aggregate type `%T'",
		  component, datum, basetype);
      return error_mark_node;
    }

  if (TYPE_SIZE (complete_type (basetype)) == 0)
    {
      incomplete_type_error (0, basetype);
      return error_mark_node;
    }

  if (TREE_CODE (component) == BIT_NOT_EXPR)
    {
      if (TYPE_IDENTIFIER (basetype) != TREE_OPERAND (component, 0))
	{
	  cp_error ("destructor specifier `%T::~%T' must have matching names",
		    basetype, TREE_OPERAND (component, 0));
	  return error_mark_node;
	}
      if (! TYPE_HAS_DESTRUCTOR (basetype))
	{
	  cp_error ("type `%T' has no destructor", basetype);
	  return error_mark_node;
	}
      return TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (basetype), 1);
    }

  /* Look up component name in the structure type definition.  */
  if (CLASSTYPE_VFIELD (basetype)
      && DECL_NAME (CLASSTYPE_VFIELD (basetype)) == component)
    /* Special-case this because if we use normal lookups in an ambiguous
       hierarchy, the compiler will abort (because vptr lookups are
       not supposed to be ambiguous.  */
    field = CLASSTYPE_VFIELD (basetype);
  else if (TREE_CODE (component) == FIELD_DECL
	   || TREE_CODE (component) == TYPE_DECL)
    {
      field = component;
    }
  else
    {
      tree name = component;
      if (TREE_CODE (component) == VAR_DECL)
	name = DECL_NAME (component);
      if (basetype_path == NULL_TREE)
	basetype_path = TYPE_BINFO (basetype);
      field = lookup_field (basetype_path, name,
			    protect && !VFIELD_NAME_P (name), 0);
      if (field == error_mark_node)
	return error_mark_node;

      if (field == NULL_TREE)
	{
	  /* Not found as a data field, look for it as a method.  If found,
	     then if this is the only possible one, return it, else
	     report ambiguity error.  */
	  tree fndecls = lookup_fnfields (basetype_path, name, 1);
	  if (fndecls == error_mark_node)
	    return error_mark_node;
	  if (fndecls)
	    {
	      if (TREE_CHAIN (fndecls) == NULL_TREE
		  && DECL_CHAIN (TREE_VALUE (fndecls)) == NULL_TREE)
		{
		  tree access, fndecl;

		  /* Unique, so use this one now.  */
		  basetype = TREE_PURPOSE (fndecls);
		  fndecl = TREE_VALUE (fndecls);
		  access = compute_access (TREE_PURPOSE (fndecls), fndecl);
		  if (access == access_public_node)
		    {
		      if (DECL_VINDEX (fndecl)
			  && ! resolves_to_fixed_type_p (datum, 0))
			{
			  tree addr = build_unary_op (ADDR_EXPR, datum, 0);
			  tree fntype = TREE_TYPE (fndecl);

			  addr = convert_pointer_to (DECL_CONTEXT (fndecl), addr);
			  datum = build_indirect_ref (addr, NULL_PTR);
			  my_friendly_assert (datum != error_mark_node, 310);
			  fndecl = build_vfn_ref (&addr, datum, DECL_VINDEX (fndecl));
			  TREE_TYPE (fndecl) = build_pointer_type (fntype);
			}
		      else
			mark_used (fndecl);
		      return build (OFFSET_REF, TREE_TYPE (fndecl), datum, fndecl);
		    }
		  if (access == access_protected_node)
		    cp_error ("member function `%D' is protected", fndecl);
		  else
		    cp_error ("member function `%D' is private", fndecl);
		  return error_mark_node;
		}
	      else
		{
		  /* Just act like build_offset_ref, since the object does
                     not matter unless we're actually calling the function.  */
		  tree t;

		  t = build_tree_list (error_mark_node, fndecls);
		  TREE_TYPE (t) = build_offset_type (basetype,
						     unknown_type_node);
		  return t;
		}
	    }

	  cp_error ("`%#T' has no member named `%D'", basetype, name);
	  return error_mark_node;
	}
      else if (TREE_TYPE (field) == error_mark_node)
	return error_mark_node;

      if (TREE_CODE (field) != FIELD_DECL)
	{
	  if (TREE_CODE (field) == TYPE_DECL)
	    {
	      cp_error ("invalid use of type decl `%#D' as expression", field);
	      return error_mark_node;
	    }
	  else if (DECL_RTL (field) != 0)
	    mark_used (field);
	  else
	    TREE_USED (field) = 1;
	  return field;
	}
    }

  /* See if we have to do any conversions so that we pick up the field from the
     right context.  */
  if (DECL_FIELD_CONTEXT (field) != basetype)
    {
      tree context = DECL_FIELD_CONTEXT (field);
      tree base = context;
      while (base != basetype && TYPE_NAME (base)
	     && ANON_AGGRNAME_P (TYPE_IDENTIFIER (base)))
	{
	  base = TYPE_CONTEXT (base);
	}

      /* Handle base classes here...  */
      if (base != basetype && TYPE_USES_COMPLEX_INHERITANCE (basetype))
	{
	  tree addr = build_unary_op (ADDR_EXPR, datum, 0);
	  if (integer_zerop (addr))
	    {
	      error ("invalid reference to NULL ptr, use ptr-to-member instead");
	      return error_mark_node;
	    }
	  if (VBASE_NAME_P (DECL_NAME (field)))
	    {
	      /* It doesn't matter which vbase pointer we grab, just
		 find one of them.  */
	      tree binfo = get_binfo (base,
				      TREE_TYPE (TREE_TYPE (addr)), 0);
	      addr = convert_pointer_to_real (binfo, addr);
	    }
	  else
	    addr = convert_pointer_to (base, addr);
	  datum = build_indirect_ref (addr, NULL_PTR);
	  my_friendly_assert (datum != error_mark_node, 311);
	}
      basetype = base;
 
      /* Handle things from anon unions here...  */
      if (TYPE_NAME (context) && ANON_AGGRNAME_P (TYPE_IDENTIFIER (context)))
	{
	  tree subfield = lookup_anon_field (basetype, context);
	  tree subdatum = build_component_ref (datum, subfield,
					       basetype_path, protect);
	  return build_component_ref (subdatum, field, basetype_path, protect);
	}
    }

  ref = fold (build (COMPONENT_REF, TREE_TYPE (field),
		     break_out_cleanups (datum), field));

  if (TREE_READONLY (datum) || TREE_READONLY (field))
    TREE_READONLY (ref) = 1;
  if (TREE_THIS_VOLATILE (datum) || TREE_THIS_VOLATILE (field))
    TREE_THIS_VOLATILE (ref) = 1;
  if (DECL_MUTABLE_P (field))
    TREE_READONLY (ref) = 0;

  return ref;
}

/* Variant of build_component_ref for use in expressions, which should
   never have REFERENCE_TYPE.  */

tree
build_x_component_ref (datum, component, basetype_path, protect)
     tree datum, component, basetype_path;
     int protect;
{
  tree t = build_component_ref (datum, component, basetype_path, protect);

  if (! processing_template_decl)
    t = convert_from_reference (t);

  return t;
}

/* Given an expression PTR for a pointer, return an expression
   for the value pointed to.
   ERRORSTRING is the name of the operator to appear in error messages.

   This function may need to overload OPERATOR_FNNAME.
   Must also handle REFERENCE_TYPEs for C++.  */

tree
build_x_indirect_ref (ptr, errorstring)
     tree ptr;
     char *errorstring;
{
  tree rval;

  if (processing_template_decl)
    return build_min_nt (INDIRECT_REF, ptr);

  rval = build_opfncall (INDIRECT_REF, LOOKUP_NORMAL, ptr, NULL_TREE, NULL_TREE);
  if (rval)
    return rval;
  return build_indirect_ref (ptr, errorstring);
}

tree
build_indirect_ref (ptr, errorstring)
     tree ptr;
     char *errorstring;
{
  register tree pointer, type;

  if (ptr == error_mark_node)
    return error_mark_node;

  pointer = (TREE_CODE (TREE_TYPE (ptr)) == REFERENCE_TYPE
	     ? ptr : default_conversion (ptr));
  type = TREE_TYPE (pointer);

  if (ptr == current_class_ptr)
    return current_class_ref;

  if (IS_AGGR_TYPE (type))
    {
      ptr = build_expr_type_conversion (WANT_POINTER, pointer, 1);

      if (ptr)
	{
	  pointer = ptr;
	  type = TREE_TYPE (pointer);
	}
    }

  if (TREE_CODE (type) == POINTER_TYPE || TREE_CODE (type) == REFERENCE_TYPE)
    {
      if (TREE_CODE (pointer) == ADDR_EXPR
	  && !flag_volatile
	  && (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_OPERAND (pointer, 0)))
	      == TYPE_MAIN_VARIANT (TREE_TYPE (type)))
	  && (TREE_READONLY (TREE_OPERAND (pointer, 0))
	      == TYPE_READONLY (TREE_TYPE (type)))
	  && (TREE_THIS_VOLATILE (TREE_OPERAND (pointer, 0))
	      == TYPE_VOLATILE (TREE_TYPE (type))))
	return TREE_OPERAND (pointer, 0);
      else
	{
	  tree t = TREE_TYPE (type);
	  register tree ref = build1 (INDIRECT_REF,
				      TYPE_MAIN_VARIANT (t), pointer);

	  /* We *must* set TREE_READONLY when dereferencing a pointer to const,
	     so that we get the proper error message if the result is used
	     to assign to.  Also, &* is supposed to be a no-op.  */
	  TREE_READONLY (ref) = TYPE_READONLY (t);
	  TREE_SIDE_EFFECTS (ref)
	    = TYPE_VOLATILE (t) || TREE_SIDE_EFFECTS (pointer) || flag_volatile;
	  TREE_THIS_VOLATILE (ref) = TYPE_VOLATILE (t);
	  return ref;
	}
    }
  /* `pointer' won't be an error_mark_node if we were given a
     pointer to member, so it's cool to check for this here.  */
  else if (TYPE_PTRMEMFUNC_P (type))
    error ("invalid use of `%s' on pointer to member function", errorstring);
  else if (TREE_CODE (type) == RECORD_TYPE
	   && (IS_SIGNATURE_POINTER (type) || IS_SIGNATURE_REFERENCE (type)))
    error ("cannot dereference signature pointer/reference");
  else if (pointer != error_mark_node)
    {
      if (errorstring)
	error ("invalid type argument of `%s'", errorstring);
      else
	error ("invalid type argument");
    }
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
   will inherit the type of the array, which will be some pointer type.  */

tree
build_array_ref (array, idx)
     tree array, idx;
{
  if (idx == 0)
    {
      error ("subscript missing in array reference");
      return error_mark_node;
    }

  if (TREE_TYPE (array) == error_mark_node
      || TREE_TYPE (idx) == error_mark_node)
    return error_mark_node;

  if (TREE_CODE (TREE_TYPE (array)) == ARRAY_TYPE
      && TREE_CODE (array) != INDIRECT_REF)
    {
      tree rval, type;

      /* Subscripting with type char is likely to lose
	 on a machine where chars are signed.
	 So warn on any machine, but optionally.
	 Don't warn for unsigned char since that type is safe.
	 Don't warn for signed char because anyone who uses that
	 must have done so deliberately.  */
      if (warn_char_subscripts
	  && TYPE_MAIN_VARIANT (TREE_TYPE (idx)) == char_type_node)
	warning ("array subscript has type `char'");

      /* Apply default promotions *after* noticing character types.  */
      idx = default_conversion (idx);

      if (TREE_CODE (TREE_TYPE (idx)) != INTEGER_TYPE)
	{
	  error ("array subscript is not an integer");
	  return error_mark_node;
	}

      /* An array that is indexed by a non-constant
	 cannot be stored in a register; we must be able to do
	 address arithmetic on its address.
	 Likewise an array of elements of variable size.  */
      if (TREE_CODE (idx) != INTEGER_CST
	  || (TYPE_SIZE (TREE_TYPE (TREE_TYPE (array))) != 0
	      && TREE_CODE (TYPE_SIZE (TREE_TYPE (TREE_TYPE (array)))) != INTEGER_CST))
	{
	  if (mark_addressable (array) == 0)
	    return error_mark_node;
	}
      /* An array that is indexed by a constant value which is not within
	 the array bounds cannot be stored in a register either; because we
	 would get a crash in store_bit_field/extract_bit_field when trying
	 to access a non-existent part of the register.  */
      if (TREE_CODE (idx) == INTEGER_CST
	  && TYPE_VALUES (TREE_TYPE (array))
	  && ! int_fits_type_p (idx, TYPE_VALUES (TREE_TYPE (array))))
	{
	  if (mark_addressable (array) == 0)
	    return error_mark_node;
	}

      if (pedantic && !lvalue_p (array))
	pedwarn ("ANSI C++ forbids subscripting non-lvalue array");

      /* Note in C++ it is valid to subscript a `register' array, since
	 it is valid to take the address of something with that
	 storage specification.  */
      if (extra_warnings)
	{
	  tree foo = array;
	  while (TREE_CODE (foo) == COMPONENT_REF)
	    foo = TREE_OPERAND (foo, 0);
	  if (TREE_CODE (foo) == VAR_DECL && DECL_REGISTER (foo))
	    warning ("subscripting array declared `register'");
	}

      type = TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (array)));
      rval = build (ARRAY_REF, type, array, idx);
      /* Array ref is const/volatile if the array elements are
	 or if the array is..  */
      TREE_READONLY (rval)
	|= (TYPE_READONLY (TREE_TYPE (TREE_TYPE (array)))
	    | TREE_READONLY (array));
      TREE_SIDE_EFFECTS (rval)
	|= (TYPE_VOLATILE (TREE_TYPE (TREE_TYPE (array)))
	    | TREE_SIDE_EFFECTS (array));
      TREE_THIS_VOLATILE (rval)
	|= (TYPE_VOLATILE (TREE_TYPE (TREE_TYPE (array)))
	    /* This was added by rms on 16 Nov 91.
	       It fixes  vol struct foo *a;  a->elts[1] 
	       in an inline function.
	       Hope it doesn't break something else.  */
	    | TREE_THIS_VOLATILE (array));
      return require_complete_type (fold (rval));
    }

  {
    tree ar = default_conversion (array);
    tree ind = default_conversion (idx);

    /* Put the integer in IND to simplify error checking.  */
    if (TREE_CODE (TREE_TYPE (ar)) == INTEGER_TYPE)
      {
	tree temp = ar;
	ar = ind;
	ind = temp;
      }

    if (ar == error_mark_node)
      return ar;

    if (TREE_CODE (TREE_TYPE (ar)) != POINTER_TYPE)
      {
	error ("subscripted value is neither array nor pointer");
	return error_mark_node;
      }
    if (TREE_CODE (TREE_TYPE (ind)) != INTEGER_TYPE)
      {
	error ("array subscript is not an integer");
	return error_mark_node;
      }

    return build_indirect_ref (build_binary_op_nodefault (PLUS_EXPR, ar, ind, PLUS_EXPR),
			       "array indexing");
  }
}

/* Build a function call to function FUNCTION with parameters PARAMS.
   PARAMS is a list--a chain of TREE_LIST nodes--in which the
   TREE_VALUE of each node is a parameter-expression.  The PARAMS do
   not include any object pointer that may be required.  FUNCTION's
   data type may be a function type or a pointer-to-function.

   For C++: If FUNCTION's data type is a TREE_LIST, then the tree list
   is the list of possible methods that FUNCTION could conceivably
   be.  If the list of methods comes from a class, then it will be
   a list of lists (where each element is associated with the class
   that produced it), otherwise it will be a simple list (for
   functions overloaded in global scope).

   In the first case, TREE_VALUE (function) is the head of one of those
   lists, and TREE_PURPOSE is the name of the function.

   In the second case, TREE_PURPOSE (function) is the function's
   name directly.

   DECL is the class instance variable, usually CURRENT_CLASS_REF.

   When calling a TEMPLATE_DECL, we don't require a complete return
   type.  */

tree
build_x_function_call (function, params, decl)
     tree function, params, decl;
{
  tree type;
  int is_method;

  if (function == error_mark_node)
    return error_mark_node;

  if (processing_template_decl)
    return build_min_nt (CALL_EXPR, function, params, NULL_TREE);

  type = TREE_TYPE (function);

  if (TREE_CODE (type) == OFFSET_TYPE
      && TREE_TYPE (type) == unknown_type_node
      && TREE_CODE (function) == TREE_LIST
      && TREE_CHAIN (function) == NULL_TREE)
    {
      /* Undo (Foo:bar)()...  */
      type = TYPE_OFFSET_BASETYPE (type);
      function = TREE_VALUE (function);
      my_friendly_assert (TREE_CODE (function) == TREE_LIST, 999);
      my_friendly_assert (TREE_CHAIN (function) == NULL_TREE, 999);
      function = TREE_VALUE (function);
      my_friendly_assert (TREE_CODE (function) == FUNCTION_DECL, 999);
      function = DECL_NAME (function);
      return build_method_call (decl, function, params, TYPE_BINFO (type), LOOKUP_NORMAL);
    }
    
  is_method = ((TREE_CODE (function) == TREE_LIST
		&& current_class_type != NULL_TREE
		&& IDENTIFIER_CLASS_VALUE (TREE_PURPOSE (function)) == function)
	       || TREE_CODE (function) == IDENTIFIER_NODE
	       || TREE_CODE (type) == METHOD_TYPE
	       || TYPE_PTRMEMFUNC_P (type));

  if (TREE_CODE (function) == FUNCTION_DECL
      && DECL_STATIC_FUNCTION_P (function))
    return build_member_call
      (DECL_CONTEXT (function), DECL_NAME (function), params);

  /* Handle methods, friends, and overloaded functions, respectively.  */
  if (is_method)
    {
      tree basetype = NULL_TREE;

      if (TREE_CODE (function) == FUNCTION_DECL
	  || DECL_FUNCTION_TEMPLATE_P (function))
	{
	  basetype = DECL_CLASS_CONTEXT (function);

	  if (DECL_NAME (function))
	    function = DECL_NAME (function);
	  else
	    function = TYPE_IDENTIFIER (DECL_CLASS_CONTEXT (function));
	}
      else if (TREE_CODE (function) == TREE_LIST)
	{
	  my_friendly_assert (TREE_CODE (TREE_VALUE (function)) == FUNCTION_DECL, 312);
	  basetype = DECL_CLASS_CONTEXT (TREE_VALUE (function));
	  function = TREE_PURPOSE (function);
	}
      else if (TREE_CODE (function) != IDENTIFIER_NODE)
	{
	  if (TREE_CODE (function) == OFFSET_REF)
	    {
	      if (TREE_OPERAND (function, 0))
		decl = TREE_OPERAND (function, 0);
	    }
	  /* Call via a pointer to member function.  */
	  if (decl == NULL_TREE)
	    {
	      error ("pointer to member function called, but not in class scope");
	      return error_mark_node;
	    }
	  /* What other type of POINTER_TYPE could this be? */
	  if (TREE_CODE (TREE_TYPE (function)) != POINTER_TYPE
	      && ! TYPE_PTRMEMFUNC_P (TREE_TYPE (function))
	      && TREE_CODE (function) != OFFSET_REF)
	    function = build (OFFSET_REF, TREE_TYPE (type), NULL_TREE, function);
	  goto do_x_function;
	}

      /* this is an abbreviated method call.
         must go through here in case it is a virtual function.
	 @@ Perhaps this could be optimized.  */

      if (basetype && (! current_class_type
		       || ! DERIVED_FROM_P (basetype, current_class_type)))
	return build_member_call (basetype, function, params);

      if (decl == NULL_TREE)
	{
	  if (current_class_type == NULL_TREE)
	    {
	      error ("object missing in call to method `%s'",
		     IDENTIFIER_POINTER (function));
	      return error_mark_node;
	    }
	  /* Yow: call from a static member function.  */
	  decl = build1 (NOP_EXPR, build_pointer_type (current_class_type),
			 error_mark_node);
	  decl = build_indirect_ref (decl, NULL_PTR);
	}

      return build_method_call (decl, function, params,
				NULL_TREE, LOOKUP_NORMAL);
    }
  else if (TREE_CODE (function) == COMPONENT_REF
	   && type == unknown_type_node)
    {
      /* Should we undo what was done in build_component_ref? */
      if (TREE_CODE (TREE_PURPOSE (TREE_OPERAND (function, 1))) == TREE_VEC)
	/* Get the name that build_component_ref hid.  */
	function = DECL_NAME (TREE_VALUE (TREE_OPERAND (function, 1)));
      else
	function = TREE_PURPOSE (TREE_OPERAND (function, 1));
      return build_method_call (decl, function, params,
				NULL_TREE, LOOKUP_NORMAL);
    }
  else if (really_overloaded_fn (function))
    {
      if (TREE_VALUE (function) == NULL_TREE)
	{
	  cp_error ("function `%D' declared overloaded, but no definitions appear with which to resolve it?!?",
		    TREE_PURPOSE (function));
	  return error_mark_node;
	}
      else
	{
	  tree val = TREE_VALUE (function);

	  if (flag_ansi_overloading)
	    return build_new_function_call (function, params, NULL_TREE);

	  if (TREE_CODE (val) == TEMPLATE_DECL)
	    return build_overload_call_real
	      (function, params, LOOKUP_COMPLAIN, (struct candidate *)0, 0);
	  else if (DECL_CHAIN (val) != NULL_TREE)
	    return build_overload_call
	      (function, params, LOOKUP_COMPLAIN);
	  else
	    my_friendly_abort (360);
	}
    }

 do_x_function:
  if (TREE_CODE (function) == OFFSET_REF)
    {
      /* If the component is a data element (or a virtual function), we play
	 games here to make things work.  */
      tree decl_addr;

      if (TREE_OPERAND (function, 0))
	decl = TREE_OPERAND (function, 0);
      else
	decl = current_class_ref;

      decl_addr = build_unary_op (ADDR_EXPR, decl, 0);
      function = get_member_function_from_ptrfunc (&decl_addr,
						   TREE_OPERAND (function, 1));
      params = expr_tree_cons (NULL_TREE, decl_addr, params);
      return build_function_call (function, params);
    }

  type = TREE_TYPE (function);
  if (type != error_mark_node)
    {
      if (TREE_CODE (type) == REFERENCE_TYPE)
	type = TREE_TYPE (type);

      if (IS_AGGR_TYPE (type))
	return build_opfncall (CALL_EXPR, LOOKUP_NORMAL, function, params, NULL_TREE);
    }

  if (is_method)
    {
      tree fntype = TREE_TYPE (function);
      tree ctypeptr;

      /* Explicitly named method?  */
      if (TREE_CODE (function) == FUNCTION_DECL)
	ctypeptr = build_pointer_type (DECL_CLASS_CONTEXT (function));
      /* Expression with ptr-to-method type?  It could either be a plain
	 usage, or it might be a case where the ptr-to-method is being
	 passed in as an argument.  */
      else if (TYPE_PTRMEMFUNC_P (fntype))
	{
	  tree rec = TYPE_METHOD_BASETYPE (TREE_TYPE (TYPE_PTRMEMFUNC_FN_TYPE (fntype)));
	  ctypeptr = build_pointer_type (rec);
	}
      /* Unexpected node type?  */
      else
	my_friendly_abort (116);
      if (decl == NULL_TREE)
	{
	  if (current_function_decl
	      && DECL_STATIC_FUNCTION_P (current_function_decl))
	    error ("invalid call to member function needing `this' in static member function scope");
	  else
	    error ("pointer to member function called, but not in class scope");
	  return error_mark_node;
	}
      if (TREE_CODE (TREE_TYPE (decl)) != POINTER_TYPE
	  && ! TYPE_PTRMEMFUNC_P (TREE_TYPE (decl)))
	{
	  decl = build_unary_op (ADDR_EXPR, decl, 0);
	  decl = convert_pointer_to (TREE_TYPE (ctypeptr), decl);
	}
      else
	decl = build_c_cast (ctypeptr, decl);
      params = expr_tree_cons (NULL_TREE, decl, params);
    }

  return build_function_call (function, params);
}

/* Resolve a pointer to member function.  INSTANCE is the object
   instance to use, if the member points to a virtual member.  */

tree
get_member_function_from_ptrfunc (instance_ptrptr, function)
     tree *instance_ptrptr;
     tree function;
{
  if (TREE_CODE (function) == OFFSET_REF)
    {
      function = TREE_OPERAND (function, 1);
    }

  if (TYPE_PTRMEMFUNC_P (TREE_TYPE (function)))
    {
      tree fntype, idx, e1, delta, delta2, e2, e3, aref, vtbl;
      tree instance;

      tree instance_ptr = *instance_ptrptr;

      if (TREE_SIDE_EFFECTS (instance_ptr))
	instance_ptr = save_expr (instance_ptr);

      if (TREE_SIDE_EFFECTS (function))
	function = save_expr (function);

      fntype = TYPE_PTRMEMFUNC_FN_TYPE (TREE_TYPE (function));

      /* Promoting idx before saving it improves performance on RISC
	 targets.  Without promoting, the first compare used
	 load-with-sign-extend, while the second used normal load then
	 shift to sign-extend.  An optimizer flaw, perhaps, but it's easier
	 to make this change.  */
      idx = save_expr (default_conversion
		       (build_component_ref (function,
					     index_identifier,
					     NULL_TREE, 0)));
      e1 = build_binary_op (GT_EXPR, idx, integer_zero_node, 1);
      delta = cp_convert (ptrdiff_type_node,
			  build_component_ref (function, delta_identifier, NULL_TREE, 0));
      delta2 = DELTA2_FROM_PTRMEMFUNC (function);

      /* Convert down to the right base, before using the instance.  */
      instance
	= convert_pointer_to_real (TYPE_METHOD_BASETYPE (TREE_TYPE (fntype)),
				   instance_ptr);
      if (instance == error_mark_node && instance_ptr != error_mark_node)
	return instance;

      vtbl = convert_pointer_to (ptr_type_node, instance);
      vtbl
	= build (PLUS_EXPR,
		 build_pointer_type (build_pointer_type (vtable_entry_type)),
		 vtbl, cp_convert (ptrdiff_type_node, delta2));
      vtbl = build_indirect_ref (vtbl, NULL_PTR);
      aref = build_array_ref (vtbl, build_binary_op (MINUS_EXPR,
						     idx,
						     integer_one_node, 1));
      if (! flag_vtable_thunks)
	{
	  aref = save_expr (aref);

	  delta = build_binary_op (PLUS_EXPR,
				   build_conditional_expr (e1, build_component_ref (aref, delta_identifier, NULL_TREE, 0), integer_zero_node),
				   delta, 1);
	}

      *instance_ptrptr = build (PLUS_EXPR, TREE_TYPE (instance_ptr),
				instance_ptr, delta);
      if (flag_vtable_thunks)
	e2 = aref;
      else
	e2 = build_component_ref (aref, pfn_identifier, NULL_TREE, 0);

      e3 = PFN_FROM_PTRMEMFUNC (function);
      TREE_TYPE (e2) = TREE_TYPE (e3);
      e1 = build_conditional_expr (e1, e2, e3);

      if (instance_ptr == error_mark_node
	  && TREE_CODE (e1) != ADDR_EXPR
	  && TREE_CODE (TREE_OPERAND (e1, 0)) != FUNCTION_DECL)
	cp_error ("object missing in `%E'", function);

      function = e1;

      /* Make sure this doesn't get evaluated first inside one of the
         branches of the COND_EXPR.  */
      if (TREE_CODE (instance_ptr) == SAVE_EXPR)
	function = build (COMPOUND_EXPR, TREE_TYPE (function),
			  instance_ptr, function);
    }
  return function;
}

tree
build_function_call_real (function, params, require_complete, flags)
     tree function, params;
     int require_complete, flags;
{
  register tree fntype, fndecl;
  register tree value_type;
  register tree coerced_params;
  tree name = NULL_TREE, assembler_name = NULL_TREE;
  int is_method;

  /* build_c_cast puts on a NOP_EXPR to make the result not an lvalue.
     Strip such NOP_EXPRs, since FUNCTION is used in non-lvalue context.  */
  if (TREE_CODE (function) == NOP_EXPR
      && TREE_TYPE (function) == TREE_TYPE (TREE_OPERAND (function, 0)))
    function = TREE_OPERAND (function, 0);

  if (TREE_CODE (function) == FUNCTION_DECL)
    {
      name = DECL_NAME (function);
      assembler_name = DECL_ASSEMBLER_NAME (function);

      GNU_xref_call (current_function_decl,
		     IDENTIFIER_POINTER (name ? name
					 : TYPE_IDENTIFIER (DECL_CLASS_CONTEXT (function))));
      mark_used (function);
      fndecl = function;

      /* Convert anything with function type to a pointer-to-function.  */
      if (pedantic
	  && name
	  && IDENTIFIER_LENGTH (name) == 4
	  && ! strcmp (IDENTIFIER_POINTER (name), "main")
	  && DECL_CONTEXT (function) == NULL_TREE)
	{
	  pedwarn ("ANSI C++ forbids calling `main' from within program");
	}

      if (pedantic && DECL_THIS_INLINE (function) && ! DECL_INITIAL (function)
	  && ! DECL_ARTIFICIAL (function)
	  && ! DECL_PENDING_INLINE_INFO (function))
	cp_pedwarn ("inline function `%#D' called before definition",
		    function);

      /* Differs from default_conversion by not setting TREE_ADDRESSABLE
	 (because calling an inline function does not mean the function
	 needs to be separately compiled).  */

      if (DECL_INLINE (function))
	function = inline_conversion (function);
      else
	function = build_addr_func (function);
    }
  else
    {
      fndecl = NULL_TREE;

      function = build_addr_func (function);
    }

  if (function == error_mark_node)
    return error_mark_node;

  fntype = TREE_TYPE (function);

  if (TYPE_PTRMEMFUNC_P (fntype))
    {
      cp_error ("must use .* or ->* to call pointer-to-member function in `%E (...)'",
		function);
      return error_mark_node;
    }

  is_method = (TREE_CODE (fntype) == POINTER_TYPE
	       && TREE_CODE (TREE_TYPE (fntype)) == METHOD_TYPE);

  if (!((TREE_CODE (fntype) == POINTER_TYPE
	 && TREE_CODE (TREE_TYPE (fntype)) == FUNCTION_TYPE)
	|| is_method
	|| TREE_CODE (function) == TEMPLATE_ID_EXPR))
    {
      cp_error ("`%E' cannot be used as a function", function);
      return error_mark_node;
    }

  /* fntype now gets the type of function pointed to.  */
  fntype = TREE_TYPE (fntype);

  /* Convert the parameters to the types declared in the
     function prototype, or apply default promotions.  */

  if (flags & LOOKUP_COMPLAIN)
    coerced_params = convert_arguments (NULL_TREE, TYPE_ARG_TYPES (fntype),
					params, fndecl, LOOKUP_NORMAL);
  else
    coerced_params = convert_arguments (NULL_TREE, TYPE_ARG_TYPES (fntype),
					params, fndecl, 0);

  if (coerced_params == error_mark_node)
    if (flags & LOOKUP_SPECULATIVELY)
      return NULL_TREE;
    else
      return error_mark_node;

  /* Check for errors in format strings.  */

  if (warn_format && (name || assembler_name))
    check_function_format (name, assembler_name, coerced_params);

  /* Recognize certain built-in functions so we can make tree-codes
     other than CALL_EXPR.  We do this when it enables fold-const.c
     to do something useful.  */

  if (TREE_CODE (function) == ADDR_EXPR
      && TREE_CODE (TREE_OPERAND (function, 0)) == FUNCTION_DECL
      && DECL_BUILT_IN (TREE_OPERAND (function, 0)))
    switch (DECL_FUNCTION_CODE (TREE_OPERAND (function, 0)))
      {
      case BUILT_IN_ABS:
      case BUILT_IN_LABS:
      case BUILT_IN_FABS:
	if (coerced_params == 0)
	  return integer_zero_node;
	return build_unary_op (ABS_EXPR, TREE_VALUE (coerced_params), 0);
      }

  /* C++ */
  value_type = TREE_TYPE (fntype) ? TREE_TYPE (fntype) : void_type_node;
  {
    register tree result
      = build_call (function, value_type, coerced_params);

    if (require_complete)
      {
	if (value_type == void_type_node)
	  return result;
	result = require_complete_type (result);
      }
    if (IS_AGGR_TYPE (value_type))
      result = build_cplus_new (value_type, result);
    return convert_from_reference (result);
  }
}

tree
build_function_call (function, params)
     tree function, params;
{
  return build_function_call_real (function, params, 1, LOOKUP_NORMAL);
}

/* Convert the actual parameter expressions in the list VALUES
   to the types in the list TYPELIST.
   If parmdecls is exhausted, or when an element has NULL as its type,
   perform the default conversions.

   RETURN_LOC is the location of the return value, if known, NULL_TREE
   otherwise.  This is useful in the case where we can avoid creating
   a temporary variable in the case where we can initialize the return
   value directly.  If we are not eliding constructors, then we set this
   to NULL_TREE to avoid this avoidance.

   NAME is an IDENTIFIER_NODE or 0.  It is used only for error messages.

   This is also where warnings about wrong number of args are generated.
   
   Return a list of expressions for the parameters as converted.

   Both VALUES and the returned value are chains of TREE_LIST nodes
   with the elements of the list in the TREE_VALUE slots of those nodes.

   In C++, unspecified trailing parameters can be filled in with their
   default arguments, if such were specified.  Do so here.  */

tree
convert_arguments (return_loc, typelist, values, fndecl, flags)
     tree return_loc, typelist, values, fndecl;
     int flags;
{
  register tree typetail, valtail;
  register tree result = NULL_TREE;
  char *called_thing;
  int i = 0;

  if (! flag_elide_constructors)
    return_loc = 0;

  /* Argument passing is always copy-initialization.  */
  flags |= LOOKUP_ONLYCONVERTING;

  if (fndecl)
    {
      if (TREE_CODE (TREE_TYPE (fndecl)) == METHOD_TYPE)
	{
	  if (DECL_NAME (fndecl) == NULL_TREE
	      || IDENTIFIER_HAS_TYPE_VALUE (DECL_NAME (fndecl)))
	    called_thing = "constructor";
	  else
	    called_thing = "member function";
	}
      else
	called_thing = "function";
    }

  for (valtail = values, typetail = typelist;
       valtail;
       valtail = TREE_CHAIN (valtail), i++)
    {
      register tree type = typetail ? TREE_VALUE (typetail) : 0;
      register tree val = TREE_VALUE (valtail);

      if (val == error_mark_node)
	return error_mark_node;

      if (type == void_type_node)
	{
	  if (fndecl)
	    {
	      cp_error_at ("too many arguments to %s `%+D'", called_thing,
			   fndecl);
	      error ("at this point in file");
	    }
	  else
	    error ("too many arguments to function");
	  /* In case anybody wants to know if this argument
	     list is valid.  */
	  if (result)
	    TREE_TYPE (tree_last (result)) = error_mark_node;
	  break;
	}

      /* The tree type of the parameter being passed may not yet be
	 known.  In this case, its type is TYPE_UNKNOWN, and will
	 be instantiated by the type given by TYPE.  If TYPE
	 is also NULL, the tree type of VAL is ERROR_MARK_NODE.  */
      if (type && type_unknown_p (val))
	val = require_instantiated_type (type, val, integer_zero_node);
      else if (type_unknown_p (val))
	{
	  /* Strip the `&' from an overloaded FUNCTION_DECL.  */
	  if (TREE_CODE (val) == ADDR_EXPR)
	    val = TREE_OPERAND (val, 0);
	  if (really_overloaded_fn (val))
	    cp_error ("insufficient type information to resolve address of overloaded function `%D'",
		      DECL_NAME (get_first_fn (val)));
	  else
	    error ("insufficient type information in parameter list");
	  val = integer_zero_node;
	}
      else if (TREE_CODE (val) == OFFSET_REF
	    && TREE_CODE (TREE_TYPE (val)) == METHOD_TYPE)
	{
	  /* This is unclean.  Should be handled elsewhere.  */
	  val = build_unary_op (ADDR_EXPR, val, 0);
	}
      else if (TREE_CODE (val) == OFFSET_REF)
	val = resolve_offset_ref (val);

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
	    val = default_conversion (val);

	  val = require_complete_type (val);
	}

      if (val == error_mark_node)
	return error_mark_node;

      if (type != 0)
	{
	  /* Formal parm type is specified by a function prototype.  */
	  tree parmval;

	  if (TYPE_SIZE (complete_type (type)) == 0)
	    {
	      error ("parameter type of called function is incomplete");
	      parmval = val;
	    }
	  else
	    {
	      parmval = convert_for_initialization
		(return_loc, type, val, flags,
		 "argument passing", fndecl, i);
#ifdef PROMOTE_PROTOTYPES
	      if ((TREE_CODE (type) == INTEGER_TYPE
		   || TREE_CODE (type) == ENUMERAL_TYPE)
		  && (TYPE_PRECISION (type) < TYPE_PRECISION (integer_type_node)))
		parmval = default_conversion (parmval);
#endif
	    }

	  if (parmval == error_mark_node)
	    return error_mark_node;

	  result = expr_tree_cons (NULL_TREE, parmval, result);
	}
      else
	{
	  if (TREE_CODE (TREE_TYPE (val)) == REFERENCE_TYPE)
	    val = convert_from_reference (val);

	  if (TREE_CODE (TREE_TYPE (val)) == REAL_TYPE
	      && (TYPE_PRECISION (TREE_TYPE (val))
		  < TYPE_PRECISION (double_type_node)))
	    /* Convert `float' to `double'.  */
	    result = expr_tree_cons (NULL_TREE, cp_convert (double_type_node, val), result);
	  else if (TYPE_LANG_SPECIFIC (TREE_TYPE (val))
		   && ! TYPE_HAS_TRIVIAL_INIT_REF (TREE_TYPE (val)))
	    {
	      cp_warning ("cannot pass objects of type `%T' through `...'",
			  TREE_TYPE (val));
	      result = expr_tree_cons (NULL_TREE, val, result);
	    }
	  else
	    /* Convert `short' and `char' to full-size `int'.  */
	    result = expr_tree_cons (NULL_TREE, default_conversion (val), result);
	}

      if (typetail)
	typetail = TREE_CHAIN (typetail);
    }

  if (typetail != 0 && typetail != void_list_node)
    {
      /* See if there are default arguments that can be used */
      if (TREE_PURPOSE (typetail))
	{
	  for (; typetail != void_list_node; ++i)
	    {
	      tree type = TREE_VALUE (typetail);
	      tree val = break_out_target_exprs (TREE_PURPOSE (typetail));
	      tree parmval;

	      if (val == NULL_TREE)
		parmval = error_mark_node;
	      else if (TREE_CODE (val) == CONSTRUCTOR)
		{
		  parmval = digest_init (type, val, (tree *)0);
		  parmval = convert_for_initialization (return_loc, type, parmval, flags,
							"default constructor", fndecl, i);
		}
	      else
		{
		  /* This could get clobbered by the following call.  */
		  if (TREE_HAS_CONSTRUCTOR (val))
		    val = copy_node (val);

		  parmval = convert_for_initialization (return_loc, type, val, flags,
							"default argument", fndecl, i);
#ifdef PROMOTE_PROTOTYPES
		  if ((TREE_CODE (type) == INTEGER_TYPE
		       || TREE_CODE (type) == ENUMERAL_TYPE)
		      && (TYPE_PRECISION (type) < TYPE_PRECISION (integer_type_node)))
		    parmval = default_conversion (parmval);
#endif
		}

	      if (parmval == error_mark_node)
		return error_mark_node;

	      result = expr_tree_cons (0, parmval, result);
	      typetail = TREE_CHAIN (typetail);
	      /* ends with `...'.  */
	      if (typetail == NULL_TREE)
		break;
	    }
	}
      else
	{
	  if (fndecl)
	    {
	      char *buf = (char *)alloca (32 + strlen (called_thing));
	      sprintf (buf, "too few arguments to %s `%%#D'", called_thing);
	      cp_error_at (buf, fndecl);
	      error ("at this point in file");
	    }
	  else
	    error ("too few arguments to function");
	  return error_mark_list;
	}
    }

  return nreverse (result);
}

/* Build a binary-operation expression, after performing default
   conversions on the operands.  CODE is the kind of expression to build.  */

tree
build_x_binary_op (code, arg1, arg2)
     enum tree_code code;
     tree arg1, arg2;
{
  tree rval;

  if (processing_template_decl)
    return build_min_nt (code, arg1, arg2);

  if (flag_ansi_overloading)
    return build_new_op (code, LOOKUP_NORMAL, arg1, arg2, NULL_TREE);

  rval = build_opfncall (code, LOOKUP_SPECULATIVELY,
			 arg1, arg2, NULL_TREE);
  if (rval)
    return build_opfncall (code, LOOKUP_NORMAL, arg1, arg2, NULL_TREE);
  if (code == MEMBER_REF)
    return build_m_component_ref (build_indirect_ref (arg1, NULL_PTR),
				  arg2);
  return build_binary_op (code, arg1, arg2, 1);
}

tree
build_binary_op (code, arg1, arg2, convert_p)
     enum tree_code code;
     tree arg1, arg2;
     int convert_p;
{
  tree args[2];

  args[0] = arg1;
  args[1] = arg2;

  if (convert_p)
    {
      tree type0, type1;
      args[0] = decay_conversion (args[0]);
      args[1] = decay_conversion (args[1]);

      if (args[0] == error_mark_node || args[1] == error_mark_node)
	return error_mark_node;

      type0 = TREE_TYPE (args[0]);
      type1 = TREE_TYPE (args[1]);

      if (type_unknown_p (args[0]))
	{
	  args[0] = instantiate_type (type1, args[0], 1);
	  args[0] = decay_conversion (args[0]);
	}
      else if (type_unknown_p (args[1]))
	{
	  args[1] = require_instantiated_type (type0, args[1],
					       error_mark_node);
	  args[1] = decay_conversion (args[1]);
	}

      if (IS_AGGR_TYPE (type0) || IS_AGGR_TYPE (type1))
	{
	  /* Try to convert this to something reasonable.  */
	  if (! build_default_binary_type_conversion(code, &args[0], &args[1]))
	    {
	      cp_error ("no match for `%O(%#T, %#T)'", code,
			TREE_TYPE (arg1), TREE_TYPE (arg2));
	      return error_mark_node;
	    }
	}
    }
  return build_binary_op_nodefault (code, args[0], args[1], code);
}

/* Build a binary-operation expression without default conversions.
   CODE is the kind of expression to build.
   This function differs from `build' in several ways:
   the data type of the result is computed and recorded in it,
   warnings are generated if arg data types are invalid,
   special handling for addition and subtraction of pointers is known,
   and some optimization is done (operations on narrow ints
   are done in the narrower type when that gives the same result).
   Constant folding is also done before the result is returned.

   ERROR_CODE is the code that determines what to say in error messages.
   It is usually, but not always, the same as CODE.

   Note that the operands will never have enumeral types
   because either they have just had the default conversions performed
   or they have both just been converted to some other type in which
   the arithmetic is to be done.

   C++: must do special pointer arithmetic when implementing
   multiple inheritance, and deal with pointer to member functions.  */

tree
build_binary_op_nodefault (code, orig_op0, orig_op1, error_code)
     enum tree_code code;
     tree orig_op0, orig_op1;
     enum tree_code error_code;
{
  tree op0, op1;
  register enum tree_code code0, code1;
  tree type0, type1;

  /* Expression code to give to the expression when it is built.
     Normally this is CODE, which is what the caller asked for,
     but in some special cases we change it.  */
  register enum tree_code resultcode = code;

  /* Data type in which the computation is to be performed.
     In the simplest cases this is the common type of the arguments.  */
  register tree result_type = NULL;

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

  /* Nonzero if this is a right-shift operation, which can be computed on the
     original short and then promoted if the operand is a promoted short.  */
  int short_shift = 0;

  /* Nonzero means set RESULT_TYPE to the common type of the args.  */
  int common = 0;

  /* Apply default conversions.  */
  if (code == TRUTH_AND_EXPR || code == TRUTH_ANDIF_EXPR
      || code == TRUTH_OR_EXPR || code == TRUTH_ORIF_EXPR
      || code == TRUTH_XOR_EXPR)
    {
      op0 = decay_conversion (orig_op0);
      op1 = decay_conversion (orig_op1);
    }
  else
    {
      op0 = default_conversion (orig_op0);
      op1 = default_conversion (orig_op1);
    }

  type0 = TREE_TYPE (op0);
  type1 = TREE_TYPE (op1);

  /* The expression codes of the data types of the arguments tell us
     whether the arguments are integers, floating, pointers, etc.  */
  code0 = TREE_CODE (type0);
  code1 = TREE_CODE (type1);

  /* Strip NON_LVALUE_EXPRs, etc., since we aren't using as an lvalue.  */
  STRIP_TYPE_NOPS (op0);
  STRIP_TYPE_NOPS (op1);

  /* If an error was already reported for one of the arguments,
     avoid reporting another error.  */

  if (code0 == ERROR_MARK || code1 == ERROR_MARK)
    return error_mark_node;

  switch (code)
    {
    case PLUS_EXPR:
      /* Handle the pointer + int case.  */
      if (code0 == POINTER_TYPE && code1 == INTEGER_TYPE)
	return pointer_int_sum (PLUS_EXPR, op0, op1);
      else if (code1 == POINTER_TYPE && code0 == INTEGER_TYPE)
	return pointer_int_sum (PLUS_EXPR, op1, op0);
      else
	common = 1;
      break;

    case MINUS_EXPR:
      /* Subtraction of two similar pointers.
	 We must subtract them as integers, then divide by object size.  */
      if (code0 == POINTER_TYPE && code1 == POINTER_TYPE
	  && comp_target_types (type0, type1, 1))
	return pointer_diff (op0, op1);
      /* Handle pointer minus int.  Just like pointer plus int.  */
      else if (code0 == POINTER_TYPE && code1 == INTEGER_TYPE)
	return pointer_int_sum (MINUS_EXPR, op0, op1);
      else
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
	   || code0 == COMPLEX_TYPE)
	  && (code1 == INTEGER_TYPE || code1 == REAL_TYPE
	      || code1 == COMPLEX_TYPE))
	{
	  if (TREE_CODE (op1) == INTEGER_CST && integer_zerop (op1))
	    cp_warning ("division by zero in `%E / 0'", op0);
	  else if (TREE_CODE (op1) == REAL_CST && real_zerop (op1))
	    cp_warning ("division by zero in `%E / 0.'", op0);
	      
	  if (!(code0 == INTEGER_TYPE && code1 == INTEGER_TYPE))
	    resultcode = RDIV_EXPR;
	  else
	    /* When dividing two signed integers, we have to promote to int.
	       unless we divide by a constant != -1.  Note that default
	       conversion will have been performed on the operands at this
	       point, so we have to dig out the original type to find out if
	       it was unsigned.  */
	    shorten = ((TREE_CODE (op0) == NOP_EXPR
			&& TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (op0, 0))))
		       || (TREE_CODE (op1) == INTEGER_CST
			   && (TREE_INT_CST_LOW (op1) != -1
			       || TREE_INT_CST_HIGH (op1) != -1)));
	  common = 1;
	}
      break;

    case BIT_AND_EXPR:
    case BIT_ANDTC_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
      if (code0 == INTEGER_TYPE && code1 == INTEGER_TYPE)
	shorten = -1;
      /* If one operand is a constant, and the other is a short type
	 that has been converted to an int,
	 really do the work in the short type and then convert the
	 result to int.  If we are lucky, the constant will be 0 or 1
	 in the short type, making the entire operation go away.  */
      if (TREE_CODE (op0) == INTEGER_CST
	  && TREE_CODE (op1) == NOP_EXPR
	  && TYPE_PRECISION (type1) > TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (op1, 0)))
	  && TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (op1, 0))))
	{
	  final_type = result_type;
	  op1 = TREE_OPERAND (op1, 0);
	  result_type = TREE_TYPE (op1);
	}
      if (TREE_CODE (op1) == INTEGER_CST
	  && TREE_CODE (op0) == NOP_EXPR
	  && TYPE_PRECISION (type0) > TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (op0, 0)))
	  && TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (op0, 0))))
	{
	  final_type = result_type;
	  op0 = TREE_OPERAND (op0, 0);
	  result_type = TREE_TYPE (op0);
	}
      break;

    case TRUNC_MOD_EXPR:
    case FLOOR_MOD_EXPR:
      if (code1 == INTEGER_TYPE && integer_zerop (op1))
	cp_warning ("division by zero in `%E % 0'", op0);
      else if (code1 == REAL_TYPE && real_zerop (op1))
	cp_warning ("division by zero in `%E % 0.'", op0);
      
      if (code0 == INTEGER_TYPE && code1 == INTEGER_TYPE)
	{
	  /* Although it would be tempting to shorten always here, that loses
	     on some targets, since the modulo instruction is undefined if the
	     quotient can't be represented in the computation mode.  We shorten
	     only if unsigned or if dividing by something we know != -1.  */
	  shorten = ((TREE_CODE (op0) == NOP_EXPR
		      && TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (op0, 0))))
		     || (TREE_CODE (op1) == INTEGER_CST
			 && (TREE_INT_CST_LOW (op1) != -1
			     || TREE_INT_CST_HIGH (op1) != -1)));
	  common = 1;
	}
      break;

    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
      result_type = boolean_type_node;
      break;

      /* Shift operations: result has same type as first operand;
	 always convert second operand to int.
	 Also set SHORT_SHIFT if shifting rightward.  */

    case RSHIFT_EXPR:
      if (code0 == INTEGER_TYPE && code1 == INTEGER_TYPE)
	{
	  result_type = type0;
	  if (TREE_CODE (op1) == INTEGER_CST)
	    {
	      if (tree_int_cst_lt (op1, integer_zero_node))
		warning ("right shift count is negative");
	      else
		{
		  if (TREE_INT_CST_LOW (op1) | TREE_INT_CST_HIGH (op1))
		    short_shift = 1;
		  if (TREE_INT_CST_HIGH (op1) != 0
		      || ((unsigned HOST_WIDE_INT) TREE_INT_CST_LOW (op1)
			  >= TYPE_PRECISION (type0)))
		    warning ("right shift count >= width of type");
		}
	    }
	  /* Convert the shift-count to an integer, regardless of
	     size of value being shifted.  */
	  if (TYPE_MAIN_VARIANT (TREE_TYPE (op1)) != integer_type_node)
	    op1 = cp_convert (integer_type_node, op1);
	  /* Avoid converting op1 to result_type later.  */
	  converted = 1;
	}
      break;

    case LSHIFT_EXPR:
      if (code0 == INTEGER_TYPE && code1 == INTEGER_TYPE)
	{
	  result_type = type0;
	  if (TREE_CODE (op1) == INTEGER_CST)
	    {
	      if (tree_int_cst_lt (op1, integer_zero_node))
		warning ("left shift count is negative");
	      else if (TREE_INT_CST_HIGH (op1) != 0
		       || ((unsigned HOST_WIDE_INT) TREE_INT_CST_LOW (op1)
			   >= TYPE_PRECISION (type0)))
		warning ("left shift count >= width of type");
	    }
	  /* Convert the shift-count to an integer, regardless of
	     size of value being shifted.  */
	  if (TYPE_MAIN_VARIANT (TREE_TYPE (op1)) != integer_type_node)
	    op1 = cp_convert (integer_type_node, op1);
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
		warning ("%s rotate count is negative",
			 (code == LROTATE_EXPR) ? "left" : "right");
	      else if (TREE_INT_CST_HIGH (op1) != 0
		       || ((unsigned HOST_WIDE_INT) TREE_INT_CST_LOW (op1)
			   >= TYPE_PRECISION (type0)))
		warning ("%s rotate count >= width of type",
			 (code == LROTATE_EXPR) ? "left" : "right");
	    }
	  /* Convert the shift-count to an integer, regardless of
	     size of value being shifted.  */
	  if (TYPE_MAIN_VARIANT (TREE_TYPE (op1)) != integer_type_node)
	    op1 = cp_convert (integer_type_node, op1);
	}
      break;

    case EQ_EXPR:
    case NE_EXPR:
      build_type = boolean_type_node; 
      if ((code0 == INTEGER_TYPE || code0 == REAL_TYPE
	   || code0 == COMPLEX_TYPE)
	  && (code1 == INTEGER_TYPE || code1 == REAL_TYPE
	      || code1 == COMPLEX_TYPE))
	short_compare = 1;
      else if (code0 == POINTER_TYPE && code1 == POINTER_TYPE)
	{
	  register tree tt0 = TYPE_MAIN_VARIANT (TREE_TYPE (type0));
	  register tree tt1 = TYPE_MAIN_VARIANT (TREE_TYPE (type1));

	  if (comp_target_types (type0, type1, 1))
	    result_type = common_type (type0, type1);
	  else if (tt0 == void_type_node)
	    {
	      if (pedantic && TREE_CODE (tt1) == FUNCTION_TYPE
		  && tree_int_cst_lt (TYPE_SIZE (type0), TYPE_SIZE (type1)))
		pedwarn ("ANSI C++ forbids comparison of `void *' with function pointer");
	      else if (TREE_CODE (tt1) == OFFSET_TYPE)
		pedwarn ("ANSI C++ forbids conversion of a pointer to member to `void *'");
	    }
	  else if (tt1 == void_type_node)
	    {
	      if (pedantic && TREE_CODE (tt0) == FUNCTION_TYPE
		  && tree_int_cst_lt (TYPE_SIZE (type1), TYPE_SIZE (type0)))
		pedwarn ("ANSI C++ forbids comparison of `void *' with function pointer");
	    }
	  else
	    cp_pedwarn ("comparison of distinct pointer types `%T' and `%T' lacks a cast",
			type0, type1);

	  if (result_type == NULL_TREE)
	    result_type = ptr_type_node;
	}
      else if (code0 == POINTER_TYPE && TREE_CODE (op1) == INTEGER_CST
	       && integer_zerop (op1))
	result_type = type0;
      else if (code1 == POINTER_TYPE && TREE_CODE (op0) == INTEGER_CST
	       && integer_zerop (op0))
	result_type = type1;
      else if (code0 == POINTER_TYPE && code1 == INTEGER_TYPE)
	{
	  result_type = type0;
	  error ("ANSI C++ forbids comparison between pointer and integer");
	}
      else if (code0 == INTEGER_TYPE && code1 == POINTER_TYPE)
	{
	  result_type = type1;
	  error ("ANSI C++ forbids comparison between pointer and integer");
	}
      else if (TYPE_PTRMEMFUNC_P (type0) && TREE_CODE (op1) == INTEGER_CST
	       && integer_zerop (op1))
	{
	  op0 = build_component_ref (op0, index_identifier, NULL_TREE, 0);
	  op1 = integer_zero_node;
	  result_type = TREE_TYPE (op0);
	}
      else if (TYPE_PTRMEMFUNC_P (type1) && TREE_CODE (op0) == INTEGER_CST
	       && integer_zerop (op0))
	{
	  op0 = build_component_ref (op1, index_identifier, NULL_TREE, 0);
	  op1 = integer_zero_node;
	  result_type = TREE_TYPE (op0);
	}
      else if (TYPE_PTRMEMFUNC_P (type0) && TYPE_PTRMEMFUNC_P (type1)
	       && (TYPE_PTRMEMFUNC_FN_TYPE (type0)
		   == TYPE_PTRMEMFUNC_FN_TYPE (type1)))
	{
	  /* The code we generate for the test is:

	  (op0.index == op1.index
	   && ((op1.index != -1 && op0.delta2 == op1.delta2)
	       || op0.pfn == op1.pfn)) */

	  tree index0 = build_component_ref (op0, index_identifier, NULL_TREE, 0);
	  tree index1 = save_expr (build_component_ref (op1, index_identifier, NULL_TREE, 0));
	  tree pfn0 = PFN_FROM_PTRMEMFUNC (op0);
	  tree pfn1 = PFN_FROM_PTRMEMFUNC (op1);
	  tree delta20 = DELTA2_FROM_PTRMEMFUNC (op0);
	  tree delta21 = DELTA2_FROM_PTRMEMFUNC (op1);
	  tree e1, e2, e3;
	  tree integer_neg_one_node
	    = build_binary_op (MINUS_EXPR, integer_zero_node, integer_one_node, 1);
	  e1 = build_binary_op (EQ_EXPR, index0, index1, 1);
	  e2 = build_binary_op (NE_EXPR, index1, integer_neg_one_node, 1);
	  e2 = build_binary_op (TRUTH_ANDIF_EXPR, e2, build_binary_op (EQ_EXPR, delta20, delta21, 1), 1);
	  e3 = build_binary_op (EQ_EXPR, pfn0, pfn1, 1);
	  e2 = build_binary_op (TRUTH_ORIF_EXPR, e2, e3, 1);
	  e2 = build_binary_op (TRUTH_ANDIF_EXPR, e1, e2, 1);
	  if (code == EQ_EXPR)
	    return e2;
	  return build_binary_op (EQ_EXPR, e2, integer_zero_node, 1);
	}
      else if (TYPE_PTRMEMFUNC_P (type0)
	       && TYPE_PTRMEMFUNC_FN_TYPE (type0) == type1)
	{
	  tree index0 = build_component_ref (op0, index_identifier, NULL_TREE, 0);
	  tree index1;
	  tree pfn0 = PFN_FROM_PTRMEMFUNC (op0);
	  tree delta20 = DELTA2_FROM_PTRMEMFUNC (op0);
	  tree delta21 = integer_zero_node;
	  tree e1, e2, e3;
	  tree integer_neg_one_node
	    = build_binary_op (MINUS_EXPR, integer_zero_node, integer_one_node, 1);
	  if (TREE_CODE (TREE_OPERAND (op1, 0)) == FUNCTION_DECL
	      && DECL_VINDEX (TREE_OPERAND (op1, 0)))
	    {
	      /* Map everything down one to make room for the null pointer to member.  */
	      index1 = size_binop (PLUS_EXPR,
				   DECL_VINDEX (TREE_OPERAND (op1, 0)),
				   integer_one_node);
	      op1 = integer_zero_node;
	      delta21 = CLASSTYPE_VFIELD (TYPE_METHOD_BASETYPE (TREE_TYPE (type1)));
	      delta21 = DECL_FIELD_BITPOS (delta21);
	      delta21 = size_binop (FLOOR_DIV_EXPR, delta21, size_int (BITS_PER_UNIT));
	    }
	  else
	    index1 = integer_neg_one_node;
	  {
	    tree nop1 = build1 (NOP_EXPR, TYPE_PTRMEMFUNC_FN_TYPE (type0), op1);
	    TREE_CONSTANT (nop1) = TREE_CONSTANT (op1);
	    op1 = nop1;
	  }
	  e1 = build_binary_op (EQ_EXPR, index0, index1, 1);
	  e2 = build_binary_op (NE_EXPR, index1, integer_neg_one_node, 1);
	  e2 = build_binary_op (TRUTH_ANDIF_EXPR, e2, build_binary_op (EQ_EXPR, delta20, delta21, 1), 1);
	  e3 = build_binary_op (EQ_EXPR, pfn0, op1, 1);
	  e2 = build_binary_op (TRUTH_ORIF_EXPR, e2, e3, 1);
	  e2 = build_binary_op (TRUTH_ANDIF_EXPR, e1, e2, 1);
	  if (code == EQ_EXPR)
	    return e2;
	  return build_binary_op (EQ_EXPR, e2, integer_zero_node, 1);
	}
      else if (TYPE_PTRMEMFUNC_P (type1)
	       && TYPE_PTRMEMFUNC_FN_TYPE (type1) == type0)
	{
	  return build_binary_op (code, op1, op0, 1);
	}
      break;

    case MAX_EXPR:
    case MIN_EXPR:
      if ((code0 == INTEGER_TYPE || code0 == REAL_TYPE)
	   && (code1 == INTEGER_TYPE || code1 == REAL_TYPE))
	shorten = 1;
      else if (code0 == POINTER_TYPE && code1 == POINTER_TYPE)
	{
	  if (comp_target_types (type0, type1, 1))
	    result_type = common_type (type0, type1);
	  else
	    {
	      cp_pedwarn ("comparison of distinct pointer types `%T' and `%T' lacks a cast",
			  type0, type1);
	      result_type = ptr_type_node;
	    }
	}
      break;

    case LE_EXPR:
    case GE_EXPR:
    case LT_EXPR:
    case GT_EXPR:
      build_type = boolean_type_node;
      if ((code0 == INTEGER_TYPE || code0 == REAL_TYPE)
	   && (code1 == INTEGER_TYPE || code1 == REAL_TYPE))
	short_compare = 1;
      else if (code0 == POINTER_TYPE && code1 == POINTER_TYPE)
	{
	  if (comp_target_types (type0, type1, 1))
	    result_type = common_type (type0, type1);
	  else
	    {
	      cp_pedwarn ("comparison of distinct pointer types `%T' and `%T' lacks a cast",
			  type0, type1);
	      result_type = ptr_type_node;
	    }
	}
      else if (code0 == POINTER_TYPE && TREE_CODE (op1) == INTEGER_CST
	       && integer_zerop (op1))
	result_type = type0;
      else if (code1 == POINTER_TYPE && TREE_CODE (op0) == INTEGER_CST
	       && integer_zerop (op0))
	result_type = type1;
      else if (code0 == POINTER_TYPE && code1 == INTEGER_TYPE)
	{
	  result_type = type0;
	  pedwarn ("ANSI C++ forbids comparison between pointer and integer");
	}
      else if (code0 == INTEGER_TYPE && code1 == POINTER_TYPE)
	{
	  result_type = type1;
	  pedwarn ("ANSI C++ forbids comparison between pointer and integer");
	}
      break;
    }

  if ((code0 == INTEGER_TYPE || code0 == REAL_TYPE || code0 == COMPLEX_TYPE)
      &&
      (code1 == INTEGER_TYPE || code1 == REAL_TYPE || code1 == COMPLEX_TYPE))
    {
      int none_complex = (code0 != COMPLEX_TYPE && code1 != COMPLEX_TYPE);

      if (shorten || common || short_compare)
	result_type = common_type (type0, type1);

      /* For certain operations (which identify themselves by shorten != 0)
	 if both args were extended from the same smaller type,
	 do the arithmetic in that type and then extend.

	 shorten !=0 and !=1 indicates a bitwise operation.
	 For them, this optimization is safe only if
	 both args are zero-extended or both are sign-extended.
	 Otherwise, we might change the result.
	 Eg, (short)-1 | (unsigned short)-1 is (int)-1
	 but calculated in (unsigned short) it would be (unsigned short)-1.  */

      if (shorten && none_complex)
	{
	  int unsigned0, unsigned1;
	  tree arg0 = get_narrower (op0, &unsigned0);
	  tree arg1 = get_narrower (op1, &unsigned1);
	  /* UNS is 1 if the operation to be done is an unsigned one.  */
	  int uns = TREE_UNSIGNED (result_type);
	  tree type;

	  final_type = result_type;

	  /* Handle the case that OP0 does not *contain* a conversion
	     but it *requires* conversion to FINAL_TYPE.  */

	  if (op0 == arg0 && TREE_TYPE (op0) != final_type)
	    unsigned0 = TREE_UNSIGNED (TREE_TYPE (op0));
	  if (op1 == arg1 && TREE_TYPE (op1) != final_type)
	    unsigned1 = TREE_UNSIGNED (TREE_TYPE (op1));

	  /* Now UNSIGNED0 is 1 if ARG0 zero-extends to FINAL_TYPE.  */

	  /* For bitwise operations, signedness of nominal type
	     does not matter.  Consider only how operands were extended.  */
	  if (shorten == -1)
	    uns = unsigned0;

	  /* Note that in all three cases below we refrain from optimizing
	     an unsigned operation on sign-extended args.
	     That would not be valid.  */

	  /* Both args variable: if both extended in same way
	     from same width, do it in that width.
	     Do it unsigned if args were zero-extended.  */
	  if ((TYPE_PRECISION (TREE_TYPE (arg0))
	       < TYPE_PRECISION (result_type))
	      && (TYPE_PRECISION (TREE_TYPE (arg1))
		  == TYPE_PRECISION (TREE_TYPE (arg0)))
	      && unsigned0 == unsigned1
	      && (unsigned0 || !uns))
	    result_type
	      = signed_or_unsigned_type (unsigned0,
					 common_type (TREE_TYPE (arg0), TREE_TYPE (arg1)));
	  else if (TREE_CODE (arg0) == INTEGER_CST
		   && (unsigned1 || !uns)
		   && (TYPE_PRECISION (TREE_TYPE (arg1))
		       < TYPE_PRECISION (result_type))
		   && (type = signed_or_unsigned_type (unsigned1,
						       TREE_TYPE (arg1)),
		       int_fits_type_p (arg0, type)))
	    result_type = type;
	  else if (TREE_CODE (arg1) == INTEGER_CST
		   && (unsigned0 || !uns)
		   && (TYPE_PRECISION (TREE_TYPE (arg0))
		       < TYPE_PRECISION (result_type))
		   && (type = signed_or_unsigned_type (unsigned0,
						       TREE_TYPE (arg0)),
		       int_fits_type_p (arg1, type)))
	    result_type = type;
	}

      /* Shifts can be shortened if shifting right.  */

      if (short_shift)
	{
	  int unsigned_arg;
	  tree arg0 = get_narrower (op0, &unsigned_arg);

	  final_type = result_type;

	  if (arg0 == op0 && final_type == TREE_TYPE (op0))
	    unsigned_arg = TREE_UNSIGNED (TREE_TYPE (op0));

	  if (TYPE_PRECISION (TREE_TYPE (arg0)) < TYPE_PRECISION (result_type)
	      /* We can shorten only if the shift count is less than the
		 number of bits in the smaller type size.  */
	      && TREE_INT_CST_HIGH (op1) == 0
	      && TYPE_PRECISION (TREE_TYPE (arg0)) > TREE_INT_CST_LOW (op1)
	      /* If arg is sign-extended and then unsigned-shifted,
		 we can simulate this with a signed shift in arg's type
		 only if the extended result is at least twice as wide
		 as the arg.  Otherwise, the shift could use up all the
		 ones made by sign-extension and bring in zeros.
		 We can't optimize that case at all, but in most machines
		 it never happens because available widths are 2**N.  */
	      && (!TREE_UNSIGNED (final_type)
		  || unsigned_arg
		  || (((unsigned) 2 * TYPE_PRECISION (TREE_TYPE (arg0)))
		      <= TYPE_PRECISION (result_type))))
	    {
	      /* Do an unsigned shift if the operand was zero-extended.  */
	      result_type
		= signed_or_unsigned_type (unsigned_arg,
					   TREE_TYPE (arg0));
	      /* Convert value-to-be-shifted to that type.  */
	      if (TREE_TYPE (op0) != result_type)
		op0 = cp_convert (result_type, op0);
	      converted = 1;
	    }
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
	    = shorten_compare (&xop0, &xop1, &xresult_type, &xresultcode);
	  if (val != 0)
	    return cp_convert (boolean_type_node, val);
	  op0 = xop0, op1 = xop1;
	  converted = 1;
	  resultcode = xresultcode;
	}

      if (short_compare && warn_sign_compare)
	{
	  int op0_signed = ! TREE_UNSIGNED (TREE_TYPE (orig_op0));
	  int op1_signed = ! TREE_UNSIGNED (TREE_TYPE (orig_op1));

	  int unsignedp0, unsignedp1;
	  tree primop0 = get_narrower (op0, &unsignedp0);
	  tree primop1 = get_narrower (op1, &unsignedp1);

	  /* Check for comparison of different enum types.  */
	  if (flag_int_enum_equivalence == 0 
	      && TREE_CODE (TREE_TYPE (orig_op0)) == ENUMERAL_TYPE 
	      && TREE_CODE (TREE_TYPE (orig_op1)) == ENUMERAL_TYPE 
	      && TYPE_MAIN_VARIANT (TREE_TYPE (orig_op0))
	         != TYPE_MAIN_VARIANT (TREE_TYPE (orig_op1)))
	    {
	      cp_warning ("comparison between `%#T' and `%#T'", 
			  TREE_TYPE (orig_op0), TREE_TYPE (orig_op1));
	    }

	  /* Give warnings for comparisons between signed and unsigned
	     quantities that may fail.  */
	  /* Do the checking based on the original operand trees, so that
	     casts will be considered, but default promotions won't be.  */

	  /* Do not warn if the comparison is being done in a signed type,
	     since the signed type will only be chosen if it can represent
	     all the values of the unsigned type.  */
	  if (! TREE_UNSIGNED (result_type))
	    /* OK */;
	  /* Do not warn if both operands are unsigned.  */
	  else if (op0_signed == op1_signed)
	    /* OK */;
	  /* Do not warn if the signed quantity is an unsuffixed
	     integer literal (or some static constant expression
	     involving such literals) and it is non-negative.  */
	  else if ((op0_signed && TREE_CODE (orig_op0) == INTEGER_CST
		    && tree_int_cst_sgn (orig_op0) >= 0)
		   || (op1_signed && TREE_CODE (orig_op1) == INTEGER_CST
		       && tree_int_cst_sgn (orig_op1) >= 0))
	    /* OK */;
	  /* Do not warn if the comparison is an equality operation,
	     the unsigned quantity is an integral constant and it does
	     not use the most significant bit of result_type.  */
	  else if ((resultcode == EQ_EXPR || resultcode == NE_EXPR)
		   && ((op0_signed && TREE_CODE (orig_op1) == INTEGER_CST
			&& int_fits_type_p (orig_op1, signed_type (result_type))
			|| (op1_signed && TREE_CODE (orig_op0) == INTEGER_CST
			    && int_fits_type_p (orig_op0, signed_type (result_type))))))
	    /* OK */;
	  else
	    warning ("comparison between signed and unsigned");

	  /* Warn if two unsigned values are being compared in a size
	     larger than their original size, and one (and only one) is the
	     result of a `~' operator.  This comparison will always fail.

	     Also warn if one operand is a constant, and the constant does not
	     have all bits set that are set in the ~ operand when it is
	     extended.  */

	  if ((TREE_CODE (primop0) == BIT_NOT_EXPR)
	      ^ (TREE_CODE (primop1) == BIT_NOT_EXPR))
	    {
	      if (TREE_CODE (primop0) == BIT_NOT_EXPR)
		primop0 = get_narrower (TREE_OPERAND (op0, 0), &unsignedp0);
	      if (TREE_CODE (primop1) == BIT_NOT_EXPR)
		primop1 = get_narrower (TREE_OPERAND (op1, 0), &unsignedp1);
	      
	      if (TREE_CODE (primop0) == INTEGER_CST
		  || TREE_CODE (primop1) == INTEGER_CST)
		{
		  tree primop;
		  HOST_WIDE_INT constant, mask;
		  int unsignedp;
		  unsigned bits;

		  if (TREE_CODE (primop0) == INTEGER_CST)
		    {
		      primop = primop1;
		      unsignedp = unsignedp1;
		      constant = TREE_INT_CST_LOW (primop0);
		    }
		  else
		    {
		      primop = primop0;
		      unsignedp = unsignedp0;
		      constant = TREE_INT_CST_LOW (primop1);
		    }

		  bits = TYPE_PRECISION (TREE_TYPE (primop));
		  if (bits < TYPE_PRECISION (result_type)
		      && bits < HOST_BITS_PER_LONG && unsignedp)
		    {
		      mask = (~ (HOST_WIDE_INT) 0) << bits;
		      if ((mask & constant) != mask)
			warning ("comparison of promoted ~unsigned with constant");
		    }
		}
	      else if (unsignedp0 && unsignedp1
		       && (TYPE_PRECISION (TREE_TYPE (primop0))
			   < TYPE_PRECISION (result_type))
		       && (TYPE_PRECISION (TREE_TYPE (primop1))
			   < TYPE_PRECISION (result_type)))
		warning ("comparison of promoted ~unsigned with unsigned");
	    }
	}
    }

  /* At this point, RESULT_TYPE must be nonzero to avoid an error message.
     If CONVERTED is zero, both args will be converted to type RESULT_TYPE.
     Then the expression will be built.
     It will be given type FINAL_TYPE if that is nonzero;
     otherwise, it will be given type RESULT_TYPE.  */

  if (!result_type)
    {
      cp_error ("invalid operands `%T' and `%T' to binary `%O'",
		TREE_TYPE (orig_op0), TREE_TYPE (orig_op1), error_code);
      return error_mark_node;
    }

  if (! converted)
    {
      if (TREE_TYPE (op0) != result_type)
	op0 = cp_convert (result_type, op0); 
      if (TREE_TYPE (op1) != result_type)
	op1 = cp_convert (result_type, op1); 
    }

  if (build_type == NULL_TREE)
    build_type = result_type;

  {
    register tree result = build (resultcode, build_type, op0, op1);
    register tree folded;

    folded = fold (result);
    if (folded == result)
      TREE_CONSTANT (folded) = TREE_CONSTANT (op0) & TREE_CONSTANT (op1);
    if (final_type != 0)
      return cp_convert (final_type, folded);
    return folded;
  }
}

/* Return a tree for the sum or difference (RESULTCODE says which)
   of pointer PTROP and integer INTOP.  */

static tree
pointer_int_sum (resultcode, ptrop, intop)
     enum tree_code resultcode;
     register tree ptrop, intop;
{
  tree size_exp;

  register tree result;
  register tree folded = fold (intop);

  /* The result is a pointer of the same type that is being added.  */

  register tree result_type = TREE_TYPE (ptrop);

  if (TREE_CODE (TREE_TYPE (result_type)) == VOID_TYPE)
    {
      if (pedantic || warn_pointer_arith)
	pedwarn ("ANSI C++ forbids using pointer of type `void *' in arithmetic");
      size_exp = integer_one_node;
    }
  else if (TREE_CODE (TREE_TYPE (result_type)) == FUNCTION_TYPE)
    {
      if (pedantic || warn_pointer_arith)
	pedwarn ("ANSI C++ forbids using pointer to a function in arithmetic");
      size_exp = integer_one_node;
    }
  else if (TREE_CODE (TREE_TYPE (result_type)) == METHOD_TYPE)
    {
      if (pedantic || warn_pointer_arith)
	pedwarn ("ANSI C++ forbids using pointer to a method in arithmetic");
      size_exp = integer_one_node;
    }
  else if (TREE_CODE (TREE_TYPE (result_type)) == OFFSET_TYPE)
    {
      if (pedantic || warn_pointer_arith)
	pedwarn ("ANSI C++ forbids using pointer to a member in arithmetic");
      size_exp = integer_one_node;
    }
  else
    size_exp = size_in_bytes (complete_type (TREE_TYPE (result_type)));

  /* Needed to make OOPS V2R3 work.  */
  intop = folded;
  if (TREE_CODE (intop) == INTEGER_CST
      && TREE_INT_CST_LOW (intop) == 0
      && TREE_INT_CST_HIGH (intop) == 0)
    return ptrop;

  /* If what we are about to multiply by the size of the elements
     contains a constant term, apply distributive law
     and multiply that constant term separately.
     This helps produce common subexpressions.  */

  if ((TREE_CODE (intop) == PLUS_EXPR || TREE_CODE (intop) == MINUS_EXPR)
      && ! TREE_CONSTANT (intop)
      && TREE_CONSTANT (TREE_OPERAND (intop, 1))
      && TREE_CONSTANT (size_exp))
    {
      enum tree_code subcode = resultcode;
      if (TREE_CODE (intop) == MINUS_EXPR)
	subcode = (subcode == PLUS_EXPR ? MINUS_EXPR : PLUS_EXPR);
      ptrop = build_binary_op (subcode, ptrop, TREE_OPERAND (intop, 1), 1);
      intop = TREE_OPERAND (intop, 0);
    }

  /* Convert the integer argument to a type the same size as sizetype
     so the multiply won't overflow spuriously.  */

  if (TYPE_PRECISION (TREE_TYPE (intop)) != TYPE_PRECISION (sizetype))
    intop = cp_convert (type_for_size (TYPE_PRECISION (sizetype), 0), intop);

  /* Replace the integer argument with a suitable product by the object size.
     Do this multiplication as signed, then convert to the appropriate
     pointer type (actually unsigned integral).  */

  intop = cp_convert (result_type,
		      build_binary_op (MULT_EXPR, intop,
				       cp_convert (TREE_TYPE (intop), size_exp), 1));

  /* Create the sum or difference.  */

  result = build (resultcode, result_type, ptrop, intop);

  folded = fold (result);
  if (folded == result)
    TREE_CONSTANT (folded) = TREE_CONSTANT (ptrop) & TREE_CONSTANT (intop);
  return folded;
}

/* Return a tree for the difference of pointers OP0 and OP1.
   The resulting tree has type int.  */

static tree
pointer_diff (op0, op1)
     register tree op0, op1;
{
  register tree result, folded;
  tree restype = ptrdiff_type_node;
  tree target_type = TREE_TYPE (TREE_TYPE (op0));

  if (pedantic || warn_pointer_arith)
    {
      if (TREE_CODE (target_type) == VOID_TYPE)
	pedwarn ("ANSI C++ forbids using pointer of type `void *' in subtraction");
      if (TREE_CODE (target_type) == FUNCTION_TYPE)
	pedwarn ("ANSI C++ forbids using pointer to a function in subtraction");
      if (TREE_CODE (target_type) == METHOD_TYPE)
	pedwarn ("ANSI C++ forbids using pointer to a method in subtraction");
      if (TREE_CODE (target_type) == OFFSET_TYPE)
	pedwarn ("ANSI C++ forbids using pointer to a member in subtraction");
    }

  /* First do the subtraction as integers;
     then drop through to build the divide operator.  */

  op0 = build_binary_op (MINUS_EXPR,
			 cp_convert (restype, op0), cp_convert (restype, op1), 1);

  /* This generates an error if op1 is a pointer to an incomplete type.  */
  if (TYPE_SIZE (TREE_TYPE (TREE_TYPE (op1))) == 0)
    error ("arithmetic on pointer to an incomplete type");

  op1 = ((TREE_CODE (target_type) == VOID_TYPE
	  || TREE_CODE (target_type) == FUNCTION_TYPE
	  || TREE_CODE (target_type) == METHOD_TYPE
	  || TREE_CODE (target_type) == OFFSET_TYPE)
	 ? integer_one_node
	 : size_in_bytes (target_type));

  /* Do the division.  */

  result = build (EXACT_DIV_EXPR, restype, op0, cp_convert (restype, op1));

  folded = fold (result);
  if (folded == result)
    TREE_CONSTANT (folded) = TREE_CONSTANT (op0) & TREE_CONSTANT (op1);
  return folded;
}

/* Handle the case of taking the address of a COMPONENT_REF.
   Called by `build_unary_op' and `build_up_reference'.

   ARG is the COMPONENT_REF whose address we want.
   ARGTYPE is the pointer type that this address should have.
   MSG is an error message to print if this COMPONENT_REF is not
   addressable (such as a bitfield).  */

tree
build_component_addr (arg, argtype, msg)
     tree arg, argtype;
     char *msg;
{
  tree field = TREE_OPERAND (arg, 1);
  tree basetype = decl_type_context (field);
  tree rval = build_unary_op (ADDR_EXPR, TREE_OPERAND (arg, 0), 0);

  if (DECL_BIT_FIELD (field))
    {
      error (msg, IDENTIFIER_POINTER (DECL_NAME (field)));
      return error_mark_node;
    }

  if (TREE_CODE (field) == FIELD_DECL
      && TYPE_USES_COMPLEX_INHERITANCE (basetype))
    {
      /* Can't convert directly to ARGTYPE, since that
	 may have the same pointer type as one of our
	 baseclasses.  */
      rval = build1 (NOP_EXPR, argtype,
		     convert_pointer_to (basetype, rval));
      TREE_CONSTANT (rval) = TREE_CONSTANT (TREE_OPERAND (rval, 0));
    }
  else
    /* This conversion is harmless.  */
    rval = convert_force (argtype, rval, 0);

  if (! integer_zerop (DECL_FIELD_BITPOS (field)))
    {
      tree offset = size_binop (EASY_DIV_EXPR, DECL_FIELD_BITPOS (field),
				size_int (BITS_PER_UNIT));
      int flag = TREE_CONSTANT (rval);
      rval = fold (build (PLUS_EXPR, argtype,
			  rval, cp_convert (argtype, offset)));
      TREE_CONSTANT (rval) = flag;
    }
  return rval;
}
   
/* Construct and perhaps optimize a tree representation
   for a unary operation.  CODE, a tree_code, specifies the operation
   and XARG is the operand.  */

tree
build_x_unary_op (code, xarg)
     enum tree_code code;
     tree xarg;
{
  if (processing_template_decl)
    return build_min_nt (code, xarg, NULL_TREE);

  /* & rec, on incomplete RECORD_TYPEs is the simple opr &, not an
     error message.  */
  if (code == ADDR_EXPR
      && TREE_CODE (xarg) != TEMPLATE_ID_EXPR
      && ((IS_AGGR_TYPE_CODE (TREE_CODE (TREE_TYPE (xarg)))
	   && TYPE_SIZE (TREE_TYPE (xarg)) == NULL_TREE)
	  || (TREE_CODE (xarg) == OFFSET_REF)))
    /* don't look for a function */;
  else
    {
      tree rval;

      if (flag_ansi_overloading)
	{
	  rval = build_new_op (code, LOOKUP_NORMAL, xarg,
			       NULL_TREE, NULL_TREE);
	  if (rval || code != ADDR_EXPR)
	    return rval;
	}
      else
	{
	  rval = build_opfncall (code, LOOKUP_SPECULATIVELY, xarg,
				 NULL_TREE, NULL_TREE);
	  if (rval)
	    return build_opfncall (code, LOOKUP_NORMAL, xarg,
				   NULL_TREE, NULL_TREE);
	}
    }

  if (code == ADDR_EXPR)
    {
      if (TREE_CODE (xarg) == TARGET_EXPR)
	warning ("taking address of temporary");
    }

  return build_unary_op (code, xarg, 0);
}

/* Just like truthvalue_conversion, but we want a CLEANUP_POINT_EXPR.  */
   
tree
condition_conversion (expr)
     tree expr;
{
  tree t;
  if (processing_template_decl)
    return expr;
  t = cp_convert (boolean_type_node, expr);
  t = fold (build1 (CLEANUP_POINT_EXPR, boolean_type_node, t));
  return t;
}
			       
/* C++: Must handle pointers to members.

   Perhaps type instantiation should be extended to handle conversion
   from aggregates to types we don't yet know we want?  (Or are those
   cases typically errors which should be reported?)

   NOCONVERT nonzero suppresses the default promotions
   (such as from short to int).  */

tree
build_unary_op (code, xarg, noconvert)
     enum tree_code code;
     tree xarg;
     int noconvert;
{
  /* No default_conversion here.  It causes trouble for ADDR_EXPR.  */
  register tree arg = xarg;
  register tree argtype = 0;
  char *errstring = NULL;
  tree val;

  if (arg == error_mark_node)
    return error_mark_node;

  switch (code)
    {
    case CONVERT_EXPR:
      /* This is used for unary plus, because a CONVERT_EXPR
	 is enough to prevent anybody from looking inside for
	 associativity, but won't generate any code.  */
      if (!(arg = build_expr_type_conversion
	    (WANT_ARITH | WANT_ENUM | WANT_POINTER, arg, 1)))
	errstring = "wrong type argument to unary plus";
      else
	{
	  if (!noconvert)
	   arg = default_conversion (arg);
	  arg = build1 (NON_LVALUE_EXPR, TREE_TYPE (arg), arg);
	}
      break;

    case NEGATE_EXPR:
      if (!(arg = build_expr_type_conversion (WANT_ARITH | WANT_ENUM, arg, 1)))
	errstring = "wrong type argument to unary minus";
      else if (!noconvert)
	arg = default_conversion (arg);
      break;

    case BIT_NOT_EXPR:
      if (TREE_CODE (TREE_TYPE (arg)) == COMPLEX_TYPE)
	{
	  code = CONJ_EXPR;
	  if (!noconvert)
	    arg = default_conversion (arg);
	}
      else if (!(arg = build_expr_type_conversion (WANT_INT | WANT_ENUM,
						   arg, 1)))
	errstring = "wrong type argument to bit-complement";
      else if (!noconvert)
	arg = default_conversion (arg);
      break;

    case ABS_EXPR:
      if (!(arg = build_expr_type_conversion (WANT_ARITH | WANT_ENUM, arg, 1)))
	errstring = "wrong type argument to abs";
      else if (!noconvert)
	arg = default_conversion (arg);
      break;

    case CONJ_EXPR:
      /* Conjugating a real value is a no-op, but allow it anyway.  */
      if (!(arg = build_expr_type_conversion (WANT_ARITH | WANT_ENUM, arg, 1)))
	errstring = "wrong type argument to conjugation";
      else if (!noconvert)
	arg = default_conversion (arg);
      break;

    case TRUTH_NOT_EXPR:
      arg = cp_convert (boolean_type_node, arg);
      val = invert_truthvalue (arg);
      if (arg != error_mark_node)
	return val;
      errstring = "in argument to unary !";
      break;

    case NOP_EXPR:
      break;
      
    case REALPART_EXPR:
      if (TREE_CODE (arg) == COMPLEX_CST)
	return TREE_REALPART (arg);
      else if (TREE_CODE (TREE_TYPE (arg)) == COMPLEX_TYPE)
	return fold (build1 (REALPART_EXPR, TREE_TYPE (TREE_TYPE (arg)), arg));
      else
	return arg;

    case IMAGPART_EXPR:
      if (TREE_CODE (arg) == COMPLEX_CST)
	return TREE_IMAGPART (arg);
      else if (TREE_CODE (TREE_TYPE (arg)) == COMPLEX_TYPE)
	return fold (build1 (IMAGPART_EXPR, TREE_TYPE (TREE_TYPE (arg)), arg));
      else
	return cp_convert (TREE_TYPE (arg), integer_zero_node);
      
    case PREINCREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case POSTDECREMENT_EXPR:
      /* Handle complex lvalues (when permitted)
	 by reduction to simpler cases.  */

      val = unary_complex_lvalue (code, arg);
      if (val != 0)
	return val;

      /* Increment or decrement the real part of the value,
	 and don't change the imaginary part.  */
      if (TREE_CODE (TREE_TYPE (arg)) == COMPLEX_TYPE)
	{
	  tree real, imag;

	  arg = stabilize_reference (arg);
	  real = build_unary_op (REALPART_EXPR, arg, 1);
	  imag = build_unary_op (IMAGPART_EXPR, arg, 1);
	  return build (COMPLEX_EXPR, TREE_TYPE (arg),
			build_unary_op (code, real, 1), imag);
	}

      /* Report invalid types.  */

      if (!(arg = build_expr_type_conversion (WANT_ARITH | WANT_POINTER,
					      arg, 1)))
	{
	  if (code == PREINCREMENT_EXPR)
	    errstring ="no pre-increment operator for type";
	  else if (code == POSTINCREMENT_EXPR)
	    errstring ="no post-increment operator for type";
	  else if (code == PREDECREMENT_EXPR)
	    errstring ="no pre-decrement operator for type";
	  else
	    errstring ="no post-decrement operator for type";
	  break;
	}

      /* Report something read-only.  */

      if (TYPE_READONLY (TREE_TYPE (arg))
	  || TREE_READONLY (arg))
	readonly_error (arg, ((code == PREINCREMENT_EXPR
			       || code == POSTINCREMENT_EXPR)
			      ? "increment" : "decrement"),
			0);

      {
	register tree inc;
	tree result_type = TREE_TYPE (arg);

	arg = get_unwidened (arg, 0);
	argtype = TREE_TYPE (arg);

	/* ARM $5.2.5 last annotation says this should be forbidden.  */
	if (TREE_CODE (argtype) == ENUMERAL_TYPE)
	  pedwarn ("ANSI C++ forbids %sing an enum",
		   (code == PREINCREMENT_EXPR || code == POSTINCREMENT_EXPR)
		   ? "increment" : "decrement");
	    
	/* Compute the increment.  */

	if (TREE_CODE (argtype) == POINTER_TYPE)
	  {
	    enum tree_code tmp = TREE_CODE (TREE_TYPE (argtype));
	    if (TYPE_SIZE (complete_type (TREE_TYPE (argtype))) == 0)
	      cp_error ("cannot %s a pointer to incomplete type `%T'",
			((code == PREINCREMENT_EXPR
			  || code == POSTINCREMENT_EXPR)
			 ? "increment" : "decrement"), TREE_TYPE (argtype));
	    else if (tmp == FUNCTION_TYPE || tmp == METHOD_TYPE
		     || tmp == VOID_TYPE || tmp == OFFSET_TYPE)
	      cp_pedwarn ("ANSI C++ forbids %sing a pointer of type `%T'",
			  ((code == PREINCREMENT_EXPR
			    || code == POSTINCREMENT_EXPR)
			   ? "increment" : "decrement"), argtype);
	    inc = c_sizeof_nowarn (TREE_TYPE (argtype));
	  }
	else
	  inc = integer_one_node;

	inc = cp_convert (argtype, inc);

	/* Handle incrementing a cast-expression.  */

	switch (TREE_CODE (arg))
	  {
	  case NOP_EXPR:
	  case CONVERT_EXPR:
	  case FLOAT_EXPR:
	  case FIX_TRUNC_EXPR:
	  case FIX_FLOOR_EXPR:
	  case FIX_ROUND_EXPR:
	  case FIX_CEIL_EXPR:
	    {
	      tree incremented, modify, value, compound;
	      if (! lvalue_p (arg) && pedantic)
		pedwarn ("cast to non-reference type used as lvalue");
	      arg = stabilize_reference (arg);
	      if (code == PREINCREMENT_EXPR || code == PREDECREMENT_EXPR)
		value = arg;
	      else
		value = save_expr (arg);
	      incremented = build (((code == PREINCREMENT_EXPR
				     || code == POSTINCREMENT_EXPR)
				    ? PLUS_EXPR : MINUS_EXPR),
				   argtype, value, inc);
	      TREE_SIDE_EFFECTS (incremented) = 1;

	      modify = build_modify_expr (arg, NOP_EXPR, incremented);
	      compound = build (COMPOUND_EXPR, TREE_TYPE (arg), modify, value);

	      /* Eliminate warning about unused result of + or -.  */
	      TREE_NO_UNUSED_WARNING (compound) = 1;
	      return compound;
	    }
	  }

	/* Complain about anything else that is not a true lvalue.  */
	if (!lvalue_or_else (arg, ((code == PREINCREMENT_EXPR
				    || code == POSTINCREMENT_EXPR)
				   ? "increment" : "decrement")))
	  return error_mark_node;

	/* Forbid using -- on `bool'.  */
	if (TREE_TYPE (arg) == boolean_type_node)
	  {
	    if (code == POSTDECREMENT_EXPR || code == PREDECREMENT_EXPR)
	      {
		cp_error ("invalid use of `--' on bool variable `%D'", arg);
		return error_mark_node;
	      }
#if 0
	    /* This will only work if someone can convince Kenner to accept
	       my patch to expand_increment. (jason)  */
	    val = build (code, TREE_TYPE (arg), arg, inc);
#else
	    if (code == POSTINCREMENT_EXPR)
	      {
		arg = stabilize_reference (arg);
		val = build (MODIFY_EXPR, TREE_TYPE (arg), arg,
			     boolean_true_node);
		TREE_SIDE_EFFECTS (val) = 1;
		arg = save_expr (arg);
		val = build (COMPOUND_EXPR, TREE_TYPE (arg), val, arg);
		val = build (COMPOUND_EXPR, TREE_TYPE (arg), arg, val);
	      }
	    else
	      val = build (MODIFY_EXPR, TREE_TYPE (arg), arg,
			   boolean_true_node);
#endif
	  }
	else
	  val = build (code, TREE_TYPE (arg), arg, inc);

	TREE_SIDE_EFFECTS (val) = 1;
	return cp_convert (result_type, val);
      }

    case ADDR_EXPR:
      /* Note that this operation never does default_conversion
	 regardless of NOCONVERT.  */

      argtype = TREE_TYPE (arg);
      if (TREE_CODE (argtype) == REFERENCE_TYPE)
	{
	  arg = build1
	    (CONVERT_EXPR,
	     build_pointer_type (TREE_TYPE (TREE_TYPE (arg))), arg);
	  TREE_CONSTANT (arg) = TREE_CONSTANT (TREE_OPERAND (arg, 0));
	  return arg;
	}
      else if (pedantic
	       && TREE_CODE (arg) == FUNCTION_DECL
	       && DECL_NAME (arg)
	       && DECL_CONTEXT (arg) == NULL_TREE
	       && IDENTIFIER_LENGTH (DECL_NAME (arg)) == 4
	       && IDENTIFIER_POINTER (DECL_NAME (arg))[0] == 'm'
	       && ! strcmp (IDENTIFIER_POINTER (DECL_NAME (arg)), "main"))
	/* ARM $3.4 */
	pedwarn ("taking address of function `main'");

      /* Let &* cancel out to simplify resulting code.  */
      if (TREE_CODE (arg) == INDIRECT_REF)
	{
	  /* We don't need to have `current_class_ptr' wrapped in a
	     NON_LVALUE_EXPR node.  */
	  if (arg == current_class_ref)
	    return current_class_ptr;

	  arg = TREE_OPERAND (arg, 0);
	  if (TREE_CODE (TREE_TYPE (arg)) == REFERENCE_TYPE)
	    {
	      arg = build1
		(CONVERT_EXPR,
		 build_pointer_type (TREE_TYPE (TREE_TYPE (arg))), arg);
	      TREE_CONSTANT (arg) = TREE_CONSTANT (TREE_OPERAND (arg, 0));
	    }
	  else if (lvalue_p (arg))
	    /* Don't let this be an lvalue.  */
	    return non_lvalue (arg);
	  return arg;
	}

      /* For &x[y], return x+y */
      if (TREE_CODE (arg) == ARRAY_REF)
	{
	  if (mark_addressable (TREE_OPERAND (arg, 0)) == 0)
	    return error_mark_node;
	  return build_binary_op (PLUS_EXPR, TREE_OPERAND (arg, 0),
				  TREE_OPERAND (arg, 1), 1);
	}

      /* Uninstantiated types are all functions.  Taking the
	 address of a function is a no-op, so just return the
	 argument.  */

      if (TREE_CODE (arg) == IDENTIFIER_NODE
	  && IDENTIFIER_OPNAME_P (arg))
	{
	  my_friendly_abort (117);
	  /* We don't know the type yet, so just work around the problem.
	     We know that this will resolve to an lvalue.  */
	  return build1 (ADDR_EXPR, unknown_type_node, arg);
	}

      if (TREE_CODE (arg) == TREE_LIST)
	{
	  if (TREE_CODE (TREE_VALUE (arg)) == FUNCTION_DECL
	      && DECL_CHAIN (TREE_VALUE (arg)) == NULL_TREE)
	    /* Unique overloaded non-member function.  */
	    return build_unary_op (ADDR_EXPR, TREE_VALUE (arg), 0);
	  if (TREE_CHAIN (arg) == NULL_TREE
	      && TREE_CODE (TREE_VALUE (arg)) == TREE_LIST
	      && DECL_CHAIN (TREE_VALUE (TREE_VALUE (arg))) == NULL_TREE)
	    /* Unique overloaded member function.  */
	    return build_unary_op (ADDR_EXPR, TREE_VALUE (TREE_VALUE (arg)),
				   0);
	  return build1 (ADDR_EXPR, unknown_type_node, arg);
	}
      else if (TREE_CODE (arg) == TEMPLATE_ID_EXPR)
	{
	  tree targs;
	  tree fn;
	  
	  /* We don't require a match here; it's possible that the
	     context (like a cast to a particular type) will resolve
	     the particular choice of template.  */
	  fn = determine_explicit_specialization (arg, NULL_TREE,
						  &targs,
						  0, 0);

	  if (fn)
	    {
	      fn = instantiate_template (fn, targs);
	      mark_addressable (fn);
	      return build_unary_op (ADDR_EXPR, fn, 0);
	    }

	  return build1 (ADDR_EXPR, unknown_type_node, arg);
	}

      /* Handle complex lvalues (when permitted)
	 by reduction to simpler cases.  */
      val = unary_complex_lvalue (code, arg);
      if (val != 0)
	return val;

      switch (TREE_CODE (arg))
	{
	case NOP_EXPR:
	case CONVERT_EXPR:
	case FLOAT_EXPR:
	case FIX_TRUNC_EXPR:
	case FIX_FLOOR_EXPR:
	case FIX_ROUND_EXPR:
	case FIX_CEIL_EXPR:
	  if (! lvalue_p (arg) && pedantic)
	    pedwarn ("taking the address of a cast to non-reference type");
	}

      /* Allow the address of a constructor if all the elements
	 are constant.  */
      if (TREE_CODE (arg) == CONSTRUCTOR && TREE_CONSTANT (arg))
	;
      /* Anything not already handled and not a true memory reference
	 is an error.  */
      else if (TREE_CODE (argtype) != FUNCTION_TYPE
	       && TREE_CODE (argtype) != METHOD_TYPE
	       && !lvalue_or_else (arg, "unary `&'"))
	return error_mark_node;

      /* Ordinary case; arg is a COMPONENT_REF or a decl.  */
      /* If the lvalue is const or volatile,
	 merge that into the type that the address will point to.  */
      if (TREE_CODE_CLASS (TREE_CODE (arg)) == 'd'
	  || TREE_CODE_CLASS (TREE_CODE (arg)) == 'r')
	{
	  if (TREE_READONLY (arg) || TREE_THIS_VOLATILE (arg))
	    argtype = cp_build_type_variant (argtype,
					    TREE_READONLY (arg),
					    TREE_THIS_VOLATILE (arg));
	}

      argtype = build_pointer_type (argtype);

      if (mark_addressable (arg) == 0)
	return error_mark_node;

      {
	tree addr;

	if (TREE_CODE (arg) == COMPONENT_REF)
	  addr = build_component_addr (arg, argtype,
				       "attempt to take address of bit-field structure member `%s'");
	else
	  addr = build1 (code, argtype, arg);

	/* Address of a static or external variable or
	   function counts as a constant */
	if (staticp (arg))
	  TREE_CONSTANT (addr) = 1;

	if (TREE_CODE (argtype) == POINTER_TYPE
	    && TREE_CODE (TREE_TYPE (argtype)) == METHOD_TYPE)
	  {
	    build_ptrmemfunc_type (argtype);
	    addr = build_ptrmemfunc (argtype, addr, 0);
	  }

	return addr;
      }
    }

  if (!errstring)
    {
      if (argtype == 0)
	argtype = TREE_TYPE (arg);
      return fold (build1 (code, argtype, arg));
    }

  error (errstring);
  return error_mark_node;
}

#if 0
/* If CONVERSIONS is a conversion expression or a nested sequence of such,
   convert ARG with the same conversions in the same order
   and return the result.  */

static tree
convert_sequence (conversions, arg)
     tree conversions;
     tree arg;
{
  switch (TREE_CODE (conversions))
    {
    case NOP_EXPR:
    case CONVERT_EXPR:
    case FLOAT_EXPR:
    case FIX_TRUNC_EXPR:
    case FIX_FLOOR_EXPR:
    case FIX_ROUND_EXPR:
    case FIX_CEIL_EXPR:
      return cp_convert (TREE_TYPE (conversions),
			 convert_sequence (TREE_OPERAND (conversions, 0),
					   arg));

    default:
      return arg;
    }
}
#endif

/* Apply unary lvalue-demanding operator CODE to the expression ARG
   for certain kinds of expressions which are not really lvalues
   but which we can accept as lvalues.

   If ARG is not a kind of expression we can handle, return zero.  */
   
tree
unary_complex_lvalue (code, arg)
     enum tree_code code;
     tree arg;
{
  /* Handle (a, b) used as an "lvalue".  */
  if (TREE_CODE (arg) == COMPOUND_EXPR)
    {
      tree real_result = build_unary_op (code, TREE_OPERAND (arg, 1), 0);
      return build (COMPOUND_EXPR, TREE_TYPE (real_result),
		    TREE_OPERAND (arg, 0), real_result);
    }

  /* Handle (a ? b : c) used as an "lvalue".  */
  if (TREE_CODE (arg) == COND_EXPR
      || TREE_CODE (arg) == MIN_EXPR || TREE_CODE (arg) == MAX_EXPR)
    return rationalize_conditional_expr (code, arg);

  if (TREE_CODE (arg) == MODIFY_EXPR
      || TREE_CODE (arg) == PREINCREMENT_EXPR
      || TREE_CODE (arg) == PREDECREMENT_EXPR)
    return unary_complex_lvalue
      (code, build (COMPOUND_EXPR, TREE_TYPE (TREE_OPERAND (arg, 0)),
		    arg, TREE_OPERAND (arg, 0)));

  if (code != ADDR_EXPR)
    return 0;

  /* Handle (a = b) used as an "lvalue" for `&'.  */
  if (TREE_CODE (arg) == MODIFY_EXPR
      || TREE_CODE (arg) == INIT_EXPR)
    {
      tree real_result = build_unary_op (code, TREE_OPERAND (arg, 0), 0);
      arg = build (COMPOUND_EXPR, TREE_TYPE (real_result), arg, real_result);
      TREE_NO_UNUSED_WARNING (arg) = 1;
      return arg;
    }

  if (TREE_CODE (TREE_TYPE (arg)) == FUNCTION_TYPE
      || TREE_CODE (TREE_TYPE (arg)) == METHOD_TYPE
      || TREE_CODE (TREE_TYPE (arg)) == OFFSET_TYPE)
    {
      /* The representation of something of type OFFSET_TYPE
	 is really the representation of a pointer to it.
	 Here give the representation its true type.  */
      tree t;

      my_friendly_assert (TREE_CODE (arg) != SCOPE_REF, 313);

      if (TREE_CODE (arg) != OFFSET_REF)
	return 0;

      t = TREE_OPERAND (arg, 1);

      if (TREE_CODE (t) == FUNCTION_DECL) /* Check all this code for right semantics.  */
	return build_unary_op (ADDR_EXPR, t, 0);
      if (TREE_CODE (t) == VAR_DECL)
	return build_unary_op (ADDR_EXPR, t, 0);
      else
	{
	  tree type;
	  tree offset = integer_zero_node;

	  if (TREE_OPERAND (arg, 0)
	      && (TREE_CODE (TREE_OPERAND (arg, 0)) != NOP_EXPR
		  || TREE_OPERAND (TREE_OPERAND (arg, 0), 0) != error_mark_node))
	    if (TREE_CODE (t) != FIELD_DECL)
	      {
		/* Don't know if this should return address to just
		   _DECL, or actual address resolved in this expression.  */
		sorry ("address of bound pointer-to-member expression");
		return error_mark_node;
	      }

	  type = TREE_TYPE (TREE_OPERAND (arg, 0));

	  if (TREE_CODE (TREE_TYPE (arg)) == OFFSET_TYPE)
	    {
	      /* Add in the offset to the intermediate subobject, if any.  */
	      offset = get_delta_difference (TYPE_OFFSET_BASETYPE (TREE_TYPE (arg)),
					     type,
					     0);
	      type = TYPE_OFFSET_BASETYPE (TREE_TYPE (arg));
	    }

	  /* Now in the offset to the final subobject.  */
	  offset = size_binop (PLUS_EXPR,
			       offset,
			       get_delta_difference (DECL_FIELD_CONTEXT (t), 
						     type,
						     0));

	  /* Add in the offset to the field.  */
	  offset = size_binop (PLUS_EXPR, offset,
			       size_binop (EASY_DIV_EXPR,
					   DECL_FIELD_BITPOS (t),
					   size_int (BITS_PER_UNIT)));

	  /* We offset all pointer to data members by 1 so that we can
	     distinguish between a null pointer to data member and the first
	     data member of a structure.  */
	  offset = size_binop (PLUS_EXPR, offset, size_int (1));

	  return cp_convert (build_pointer_type (TREE_TYPE (arg)), offset);
	}
    }

  
  /* We permit compiler to make function calls returning
     objects of aggregate type look like lvalues.  */
  {
    tree targ = arg;

    if (TREE_CODE (targ) == SAVE_EXPR)
      targ = TREE_OPERAND (targ, 0);

    if (TREE_CODE (targ) == CALL_EXPR && IS_AGGR_TYPE (TREE_TYPE (targ)))
      {
	if (TREE_CODE (arg) == SAVE_EXPR)
	  targ = arg;
	else
	  targ = build_cplus_new (TREE_TYPE (arg), arg);
	return build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (arg)), targ);
      }

    if (TREE_CODE (arg) == SAVE_EXPR && TREE_CODE (targ) == INDIRECT_REF)
      return build (SAVE_EXPR, build_pointer_type (TREE_TYPE (arg)),
		     TREE_OPERAND (targ, 0), current_function_decl, NULL);
  }

  /* Don't let anything else be handled specially.  */
  return 0;
}

/* Mark EXP saying that we need to be able to take the
   address of it; it should not be allocated in a register.
   Value is 1 if successful.

   C++: we do not allow `current_class_ptr' to be addressable.  */

int
mark_addressable (exp)
     tree exp;
{
  register tree x = exp;

  if (TREE_ADDRESSABLE (x) == 1)
    return 1;

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
	    if (! flag_this_is_variable)
	      error ("address of `this' not available");
	    TREE_ADDRESSABLE (x) = 1; /* so compiler doesn't die later */
	    put_var_into_stack (x);
	    return 1;
	  }
      case VAR_DECL:
	if (TREE_STATIC (x) && TREE_READONLY (x)
	    && DECL_RTL (x) != 0
	    && ! DECL_IN_MEMORY_P (x))
	  {
	    /* We thought this would make a good constant variable,
	       but we were wrong.  */
	    push_obstacks_nochange ();
	    end_temporary_allocation ();

	    TREE_ASM_WRITTEN (x) = 0;
	    DECL_RTL (x) = 0;
	    rest_of_decl_compilation (x, 0, IDENTIFIER_LOCAL_VALUE (x) == 0, 0);
	    TREE_ADDRESSABLE (x) = 1;

	    pop_obstacks ();

	    return 1;
	  }
	/* Caller should not be trying to mark initialized
	   constant fields addressable.  */
	my_friendly_assert (DECL_LANG_SPECIFIC (x) == 0
			    || DECL_IN_AGGR_P (x) == 0
			    || TREE_STATIC (x)
			    || DECL_EXTERNAL (x), 314);

      case CONST_DECL:
      case RESULT_DECL:
	if (DECL_REGISTER (x) && !TREE_ADDRESSABLE (x)
	    && !DECL_ARTIFICIAL (x) && extra_warnings)
	  cp_warning ("address requested for `%D', which is declared `register'",
		      x);
	put_var_into_stack (x);
	TREE_ADDRESSABLE (x) = 1;
	return 1;

      case FUNCTION_DECL:
	/* We have to test both conditions here.  The first may
	   be non-zero in the case of processing a default function.
	   The second may be non-zero in the case of a template function.  */
	x = DECL_MAIN_VARIANT (x);
	if (DECL_TEMPLATE_INFO (x) && !DECL_TEMPLATE_SPECIALIZATION (x))
	  mark_used (x);
	TREE_ADDRESSABLE (x) = 1;
	TREE_USED (x) = 1;
	TREE_ADDRESSABLE (DECL_ASSEMBLER_NAME (x)) = 1;
	return 1;

      case CONSTRUCTOR:
	TREE_ADDRESSABLE (x) = 1;
	return 1;

      case TARGET_EXPR:
	TREE_ADDRESSABLE (x) = 1;
	mark_addressable (TREE_OPERAND (x, 0));
	return 1;

      default:
	return 1;
    }
}

/* Build and return a conditional expression IFEXP ? OP1 : OP2.  */

tree
build_x_conditional_expr (ifexp, op1, op2)
     tree ifexp, op1, op2;
{
  tree rval = NULL_TREE;

  if (processing_template_decl)
    return build_min_nt (COND_EXPR, ifexp, op1, op2);

  if (flag_ansi_overloading)
    return build_new_op (COND_EXPR, LOOKUP_NORMAL, ifexp, op1, op2);

  /* See comments in `build_x_binary_op'.  */
  if (op1 != 0)
    rval = build_opfncall (COND_EXPR, LOOKUP_SPECULATIVELY, ifexp, op1, op2);
  if (rval)
    return build_opfncall (COND_EXPR, LOOKUP_NORMAL, ifexp, op1, op2);
  
  return build_conditional_expr (ifexp, op1, op2);
}

tree
build_conditional_expr (ifexp, op1, op2)
     tree ifexp, op1, op2;
{
  register tree type1;
  register tree type2;
  register enum tree_code code1;
  register enum tree_code code2;
  register tree result_type = NULL_TREE;

  /* If second operand is omitted, it is the same as the first one;
     make sure it is calculated only once.  */
  if (op1 == 0)
    {
      if (pedantic)
	pedwarn ("ANSI C++ forbids omitting the middle term of a ?: expression");
      ifexp = op1 = save_expr (ifexp);
    }

  ifexp = cp_convert (boolean_type_node, ifexp);

  if (TREE_CODE (ifexp) == ERROR_MARK)
    return error_mark_node;

  op1 = require_instantiated_type (TREE_TYPE (op2), op1, error_mark_node);
  if (op1 == error_mark_node)
    return error_mark_node;
  op2 = require_instantiated_type (TREE_TYPE (op1), op2, error_mark_node);
  if (op2 == error_mark_node)
    return error_mark_node;

  /* C++: REFERENCE_TYPES must be dereferenced.  */
  type1 = TREE_TYPE (op1);
  code1 = TREE_CODE (type1);
  type2 = TREE_TYPE (op2);
  code2 = TREE_CODE (type2);

  if (code1 == REFERENCE_TYPE)
    {
      op1 = convert_from_reference (op1);
      type1 = TREE_TYPE (op1);
      code1 = TREE_CODE (type1);
    }
  if (code2 == REFERENCE_TYPE)
    {
      op2 = convert_from_reference (op2);
      type2 = TREE_TYPE (op2);
      code2 = TREE_CODE (type2);
    }

  /* Don't promote the operands separately if they promote
     the same way.  Return the unpromoted type and let the combined
     value get promoted if necessary.  */

  if (TYPE_MAIN_VARIANT (type1) == TYPE_MAIN_VARIANT (type2)
      && code2 != ARRAY_TYPE
      && code2 != FUNCTION_TYPE
      && code2 != METHOD_TYPE)
    {
      tree result;

      if (TREE_CONSTANT (ifexp)
	  && (TREE_CODE (ifexp) == INTEGER_CST
	      || TREE_CODE (ifexp) == ADDR_EXPR))
	return (integer_zerop (ifexp) ? op2 : op1);

      if (TREE_CODE (op1) == CONST_DECL)
	op1 = DECL_INITIAL (op1);
      else if (TREE_READONLY_DECL_P (op1))
	op1 = decl_constant_value (op1);
      if (TREE_CODE (op2) == CONST_DECL)
	op2 = DECL_INITIAL (op2);
      else if (TREE_READONLY_DECL_P (op2))
	op2 = decl_constant_value (op2);
      if (type1 != type2)
	type1 = cp_build_type_variant
			(type1,
			 TREE_READONLY (op1) || TREE_READONLY (op2),
			 TREE_THIS_VOLATILE (op1) || TREE_THIS_VOLATILE (op2));
      /* ??? This is a kludge to deal with the fact that
	 we don't sort out integers and enums properly, yet.  */
      result = fold (build (COND_EXPR, type1, ifexp, op1, op2));
      if (TREE_TYPE (result) != type1)
	result = build1 (NOP_EXPR, type1, result);
      /* Expand both sides into the same slot,
	 hopefully the target of the ?: expression.  */
      if (TREE_CODE (op1) == TARGET_EXPR && TREE_CODE (op2) == TARGET_EXPR)
	{
	  tree slot = build (VAR_DECL, TREE_TYPE (result));
	  layout_decl (slot, 0);
	  result = build (TARGET_EXPR, TREE_TYPE (result),
			  slot, result, NULL_TREE, NULL_TREE);
	}
      return result;
    }

  /* They don't match; promote them both and then try to reconcile them.
     But don't permit mismatching enum types.  */
  if (code1 == ENUMERAL_TYPE)
    {
      if (code2 == ENUMERAL_TYPE)
	{
	  cp_error ("enumeral mismatch in conditional expression: `%T' vs `%T'", type1, type2);
	  return error_mark_node;
	}
      else if (extra_warnings && ! IS_AGGR_TYPE_CODE (code2)
	       && type2 != type_promotes_to (type1))
	warning ("enumeral and non-enumeral type in conditional expression");
    }
  else if (extra_warnings
	   && code2 == ENUMERAL_TYPE && ! IS_AGGR_TYPE_CODE (code1)
	   && type1 != type_promotes_to (type2))
    warning ("enumeral and non-enumeral type in conditional expression");

  if (code1 != VOID_TYPE)
    {
      op1 = default_conversion (op1);
      type1 = TREE_TYPE (op1);
      if (TYPE_PTRMEMFUNC_P (type1))
	type1 = TYPE_PTRMEMFUNC_FN_TYPE (type1);
      code1 = TREE_CODE (type1);
    }
  if (code2 != VOID_TYPE)
    {
      op2 = default_conversion (op2);
      type2 = TREE_TYPE (op2);
      if (TYPE_PTRMEMFUNC_P (type2))
	type2 = TYPE_PTRMEMFUNC_FN_TYPE (type2);
      code2 = TREE_CODE (type2);
    }

  if (code1 == RECORD_TYPE && code2 == RECORD_TYPE
      && real_lvalue_p (op1) && real_lvalue_p (op2)
      && comptypes (type1, type2, -1))
    {
      type1 = build_reference_type (type1);
      type2 = build_reference_type (type2);
      result_type = common_type (type1, type2);
      op1 = convert_to_reference (result_type, op1, CONV_IMPLICIT,
				  LOOKUP_NORMAL, NULL_TREE);
      op2 = convert_to_reference (result_type, op2, CONV_IMPLICIT,
				  LOOKUP_NORMAL, NULL_TREE);
    }
  /* Quickly detect the usual case where op1 and op2 have the same type
     after promotion.  */
  else if (TYPE_MAIN_VARIANT (type1) == TYPE_MAIN_VARIANT (type2))
    {
      if (type1 == type2)
	result_type = type1;
      else
	result_type = cp_build_type_variant
			(type1,
			 TREE_READONLY (op1) || TREE_READONLY (op2),
			 TREE_THIS_VOLATILE (op1) || TREE_THIS_VOLATILE (op2));
    }
  else if ((code1 == INTEGER_TYPE || code1 == REAL_TYPE)
           && (code2 == INTEGER_TYPE || code2 == REAL_TYPE))
    {
      result_type = common_type (type1, type2);
    }
  else if (code1 == VOID_TYPE || code2 == VOID_TYPE)
    {
      if (pedantic && (code1 != VOID_TYPE || code2 != VOID_TYPE))
	pedwarn ("ANSI C++ forbids conditional expr with only one void side");
      result_type = void_type_node;
    }
  else if (code1 == POINTER_TYPE && null_ptr_cst_p (op2))
    result_type = qualify_type (type1, type2);
  else if (code2 == POINTER_TYPE && null_ptr_cst_p (op1))
    result_type = qualify_type (type2, type1);
  else if (code1 == POINTER_TYPE && code2 == POINTER_TYPE)
    {
      if (comp_target_types (type1, type2, 1))
	result_type = common_type (type1, type2);
      else if (TYPE_MAIN_VARIANT (TREE_TYPE (type1)) == void_type_node)
	{
	  if (pedantic && TREE_CODE (type2) == FUNCTION_TYPE)
	    pedwarn ("ANSI C++ forbids conditional expr between `void *' and function pointer");
	  result_type = qualify_type (type1, type2);
	}
      else if (TYPE_MAIN_VARIANT (TREE_TYPE (type2)) == void_type_node)
	{
	  if (pedantic && TREE_CODE (type1) == FUNCTION_TYPE)
	    pedwarn ("ANSI C++ forbids conditional expr between `void *' and function pointer");
	  result_type = qualify_type (type2, type1);
	}
      /* C++ */
      else if (comptypes (type2, type1, 0))
	result_type = type2;
      else if (IS_AGGR_TYPE (TREE_TYPE (type1))
	       && IS_AGGR_TYPE (TREE_TYPE (type2))
	       && (result_type = common_base_type (TREE_TYPE (type1), TREE_TYPE (type2))))
	{
	  if (result_type == error_mark_node)
	    {
	      cp_error ("common base type of types `%T' and `%T' is ambiguous",
			TREE_TYPE (type1), TREE_TYPE (type2));
	      result_type = ptr_type_node;
	    }
	  else
	    {
	      if (pedantic
		  && result_type != TREE_TYPE (type1)
		  && result_type != TREE_TYPE (type2))
		cp_pedwarn ("`%T' and `%T' converted to `%T *' in conditional expression",
			    type1, type2, result_type);

	      result_type = build_pointer_type (result_type);
	    }
	}
      else
	{
	  pedwarn ("pointer type mismatch in conditional expression");
	  result_type = ptr_type_node;
	}
    }
  else if (code1 == POINTER_TYPE && code2 == INTEGER_TYPE)
    {
      pedwarn ("pointer/integer type mismatch in conditional expression");
      result_type = type1;
    }
  else if (code2 == POINTER_TYPE && code1 == INTEGER_TYPE)
    {
      pedwarn ("pointer/integer type mismatch in conditional expression");
      result_type = type2;
    }

  if (!result_type)
    {
      /* The match does not look good.  If either is
	 an aggregate value, try converting to a scalar type.  */
      if (code1 == RECORD_TYPE && code2 == RECORD_TYPE)
	{
	  cp_error ("aggregate mismatch in conditional expression: `%T' vs `%T'", type1, type2);
	  return error_mark_node;
	}
      /* Warning: this code assumes that conversion between cv-variants of
         a type is done using NOP_EXPRs.  */
      if (code1 == RECORD_TYPE && TYPE_HAS_CONVERSION (type1))
	{
	  /* There are other types besides pointers and records.  */
	  tree tmp;
	  if (code2 == POINTER_TYPE)
	      tmp = build_pointer_type
		(build_type_variant (TREE_TYPE (type2), 1, 1));
	  else
	    tmp = type2;
	  tmp = build_type_conversion (CONVERT_EXPR, tmp, op1, 0);
	  if (tmp == NULL_TREE)
	    {
	      cp_error ("incompatible types `%T' and `%T' in `?:'",
			type1, type2);
	      return error_mark_node;
	    }
	  if (tmp == error_mark_node)
	    error ("ambiguous pointer conversion");
	  else
	    STRIP_NOPS (tmp);
	  result_type = common_type (type2, TREE_TYPE (tmp));
	  op1 = tmp;
	}
      else if (code2 == RECORD_TYPE && TYPE_HAS_CONVERSION (type2))
	{
	  tree tmp;
	  if (code1 == POINTER_TYPE)
	    tmp = build_pointer_type
	      (build_type_variant (TREE_TYPE (type1), 1, 1));
	  else
	    tmp = type1;

	  tmp = build_type_conversion (CONVERT_EXPR, tmp, op2, 0);
	  if (tmp == NULL_TREE)
	    {
	      cp_error ("incompatible types `%T' and `%T' in `?:'",
			type1, type2);
	      return error_mark_node;
	    }
	  if (tmp == error_mark_node)
	    error ("ambiguous pointer conversion");
	  else
	    STRIP_NOPS (tmp);
	  result_type = common_type (type1, TREE_TYPE (tmp));
	  op2 = tmp;
	}
      else if (flag_cond_mismatch)
	result_type = void_type_node;
      else
	{
	  error ("type mismatch in conditional expression");
	  return error_mark_node;
	}
    }

  if (TREE_CODE (result_type) == POINTER_TYPE
      && TREE_CODE (TREE_TYPE (result_type)) == METHOD_TYPE)
    result_type = build_ptrmemfunc_type (result_type);

  if (result_type != TREE_TYPE (op1))
    op1 = convert_for_initialization
      (NULL_TREE, result_type, op1, LOOKUP_NORMAL, "converting", NULL_TREE, 0);
  if (result_type != TREE_TYPE (op2))
    op2 = convert_for_initialization
      (NULL_TREE, result_type, op2, LOOKUP_NORMAL, "converting", NULL_TREE, 0);

  if (TREE_CONSTANT (ifexp))
    return integer_zerop (ifexp) ? op2 : op1;

  return convert_from_reference
    (fold (build (COND_EXPR, result_type, ifexp, op1, op2)));
}

/* Handle overloading of the ',' operator when needed.  Otherwise,
   this function just builds an expression list.  */

tree
build_x_compound_expr (list)
     tree list;
{
  tree rest = TREE_CHAIN (list);
  tree result;

  if (processing_template_decl)
    return build_min_nt (COMPOUND_EXPR, list, NULL_TREE);

  if (rest == NULL_TREE)
    return build_compound_expr (list);

  result = build_opfncall (COMPOUND_EXPR, LOOKUP_NORMAL,
			   TREE_VALUE (list), TREE_VALUE (rest), NULL_TREE);
  if (result)
    return build_x_compound_expr (expr_tree_cons (NULL_TREE, result, TREE_CHAIN (rest)));

  if (! TREE_SIDE_EFFECTS (TREE_VALUE (list)))
    {
      /* the left-hand operand of a comma expression is like an expression
         statement: we should warn if it doesn't have any side-effects,
         unless it was explicitly cast to (void).  */
      if ((extra_warnings || warn_unused)
           && !(TREE_CODE (TREE_VALUE(list)) == CONVERT_EXPR
                && TREE_TYPE (TREE_VALUE(list)) == void_type_node))
        warning("left-hand operand of comma expression has no effect");
    }
#if 0 /* this requires a gcc backend patch to export warn_if_unused_value */
  else if (warn_unused)
    warn_if_unused_value (TREE_VALUE(list));
#endif

  return build_compound_expr (expr_tree_cons (NULL_TREE, TREE_VALUE (list),
					 build_expr_list (NULL_TREE, build_x_compound_expr (rest))));
}

/* Given a list of expressions, return a compound expression
   that performs them all and returns the value of the last of them.  */

tree
build_compound_expr (list)
     tree list;
{
  register tree rest;

  if (TREE_READONLY_DECL_P (TREE_VALUE (list)))
    TREE_VALUE (list) = decl_constant_value (TREE_VALUE (list));

  if (TREE_CHAIN (list) == 0)
    {
      /* build_c_cast puts on a NOP_EXPR to make the result not an lvalue.
	 Strip such NOP_EXPRs, since LIST is used in non-lvalue context.  */
      if (TREE_CODE (list) == NOP_EXPR
	  && TREE_TYPE (list) == TREE_TYPE (TREE_OPERAND (list, 0)))
	list = TREE_OPERAND (list, 0);

      /* Convert arrays to pointers.  */
      if (TREE_CODE (TREE_TYPE (TREE_VALUE (list))) == ARRAY_TYPE)
	return default_conversion (TREE_VALUE (list));
      else
	return TREE_VALUE (list);
    }

  rest = build_compound_expr (TREE_CHAIN (list));

  /* When pedantic, a compound expression cannot be a constant expression.  */
  if (! TREE_SIDE_EFFECTS (TREE_VALUE (list)) && ! pedantic)
    return rest;

  return build (COMPOUND_EXPR, TREE_TYPE (rest),
		break_out_cleanups (TREE_VALUE (list)), rest);
}

tree
build_static_cast (type, expr)
   tree type, expr;
{
  tree intype, binfo;
  int ok;

  if (type == error_mark_node || expr == error_mark_node)
    return error_mark_node;

  if (TREE_CODE (expr) == OFFSET_REF)
    expr = resolve_offset_ref (expr);

  if (processing_template_decl)
    {
      tree t = build_min (STATIC_CAST_EXPR, type, expr);
      return t;
    }

  /* build_c_cast puts on a NOP_EXPR to make the result not an lvalue.
     Strip such NOP_EXPRs if VALUE is being used in non-lvalue context.  */
  if (TREE_CODE (type) != REFERENCE_TYPE
      && TREE_CODE (expr) == NOP_EXPR
      && TREE_TYPE (expr) == TREE_TYPE (TREE_OPERAND (expr, 0)))
    expr = TREE_OPERAND (expr, 0);

  if (TREE_CODE (type) == VOID_TYPE)
    return build1 (CONVERT_EXPR, type, expr);

  if (type_unknown_p (expr))
    {
      expr = instantiate_type (type, expr, 1);
      if (expr == error_mark_node)
	return error_mark_node;
    }

  if (TREE_CODE (type) == REFERENCE_TYPE)
    return (convert_from_reference
	    (convert_to_reference (type, expr, CONV_STATIC|CONV_IMPLICIT,
				   LOOKUP_COMPLAIN, NULL_TREE)));

  if (IS_AGGR_TYPE (type))
    return build_cplus_new
      (type, (build_method_call
	      (NULL_TREE, ctor_identifier, build_expr_list (NULL_TREE, expr),
	       TYPE_BINFO (type), LOOKUP_NORMAL)));

  expr = decay_conversion (expr);
  intype = TREE_TYPE (expr);

  /* FIXME handle casting to array type.  */

  ok = 0;
  if (can_convert_arg (type, intype, expr))
    ok = 1;
  else if (TYPE_PTROB_P (type) && TYPE_PTROB_P (intype))
    {
      tree binfo;
      if (IS_AGGR_TYPE (TREE_TYPE (type)) && IS_AGGR_TYPE (TREE_TYPE (intype))
	  && (TYPE_READONLY (TREE_TYPE (type))
	      >= TYPE_READONLY (TREE_TYPE (intype)))
	  && (TYPE_VOLATILE (TREE_TYPE (type))
	      >= TYPE_VOLATILE (TREE_TYPE (intype)))
	  && (binfo = get_binfo (TREE_TYPE (intype), TREE_TYPE (type), 0))
	  && ! TREE_VIA_VIRTUAL (binfo))
	ok = 1;
    }
  else if (TYPE_PTRMEM_P (type) && TYPE_PTRMEM_P (intype))
    {
      if (comptypes (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (type))),
		     TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (intype))), 1)
	  && (TYPE_READONLY (TREE_TYPE (TREE_TYPE (type)))
	      >= TYPE_READONLY (TREE_TYPE (TREE_TYPE (intype))))
	  && (TYPE_VOLATILE (TREE_TYPE (TREE_TYPE (type)))
	      >= TYPE_VOLATILE (TREE_TYPE (TREE_TYPE (intype))))
	  && (binfo = get_binfo (TYPE_OFFSET_BASETYPE (intype),
				 TYPE_OFFSET_BASETYPE (type), 0))
	  && ! TREE_VIA_VIRTUAL (binfo))
	ok = 1;
    }
  else if (TREE_CODE (intype) != BOOLEAN_TYPE
	   && TREE_CODE (type) != ARRAY_TYPE
	   && TREE_CODE (type) != FUNCTION_TYPE
	   && can_convert (intype, type))
    ok = 1;

  if (ok)
    return build_c_cast (type, expr);

  cp_error ("static_cast from `%T' to `%T'", intype, type);
  return error_mark_node;
}

tree
build_reinterpret_cast (type, expr)
   tree type, expr;
{
  tree intype;

  if (type == error_mark_node || expr == error_mark_node)
    return error_mark_node;

  if (TREE_CODE (expr) == OFFSET_REF)
    expr = resolve_offset_ref (expr);

  if (processing_template_decl)
    {
      tree t = build_min (REINTERPRET_CAST_EXPR, type, expr);
      return t;
    }

  if (TREE_CODE (type) != REFERENCE_TYPE)
    {
      expr = decay_conversion (expr);

      /* build_c_cast puts on a NOP_EXPR to make the result not an lvalue.
	 Strip such NOP_EXPRs if VALUE is being used in non-lvalue context.  */
      if (TREE_CODE (expr) == NOP_EXPR
	  && TREE_TYPE (expr) == TREE_TYPE (TREE_OPERAND (expr, 0)))
	expr = TREE_OPERAND (expr, 0);
    }

  if (type_unknown_p (expr))
    {
      expr = instantiate_type (type, expr, 1);
      if (expr == error_mark_node)
	return error_mark_node;
    }

  intype = TREE_TYPE (expr);

  if (TREE_CODE (type) == REFERENCE_TYPE)
    {
      if (! real_lvalue_p (expr))
	{
	  cp_error ("reinterpret_cast from `%T' rvalue to `%T'", intype, type);
	  return error_mark_node;
	}
      expr = build_unary_op (ADDR_EXPR, expr, 0);
      if (expr != error_mark_node)
	expr = build_reinterpret_cast
	  (build_pointer_type (TREE_TYPE (type)), expr);
      if (expr != error_mark_node)
	expr = build_indirect_ref (expr, 0);
      return expr;
    }
  else if (comptypes (TYPE_MAIN_VARIANT (intype), TYPE_MAIN_VARIANT (type), 1))
    return build_static_cast (type, expr);

  if (TYPE_PTR_P (type) && (TREE_CODE (intype) == INTEGER_TYPE
			    || TREE_CODE (intype) == ENUMERAL_TYPE))
    /* OK */;
  else if (TREE_CODE (type) == INTEGER_TYPE && TYPE_PTR_P (intype))
    {
      if (TYPE_PRECISION (type) < TYPE_PRECISION (intype))
	cp_pedwarn ("reinterpret_cast from `%T' to `%T' loses precision",
		    intype, type);
    }
  else if ((TYPE_PTRFN_P (type) && TYPE_PTRFN_P (intype))
	   || (TYPE_PTRMEMFUNC_P (type) && TYPE_PTRMEMFUNC_P (intype)))
    {
      if (TREE_READONLY_DECL_P (expr))
	expr = decl_constant_value (expr);
      return fold (build1 (NOP_EXPR, type, expr));
    }
  else if ((TYPE_PTRMEM_P (type) && TYPE_PTRMEM_P (intype))
	   || (TYPE_PTROBV_P (type) && TYPE_PTROBV_P (intype)))
    {
      if (! comp_ptr_ttypes_reinterpret (TREE_TYPE (type), TREE_TYPE (intype)))
	cp_pedwarn ("reinterpret_cast from `%T' to `%T' casts away const (or volatile)",
		    intype, type);

      if (TREE_READONLY_DECL_P (expr))
	expr = decl_constant_value (expr);
      return fold (build1 (NOP_EXPR, type, expr));
    }
  else if ((TYPE_PTRFN_P (type) && TYPE_PTROBV_P (intype))
	   || (TYPE_PTRFN_P (type) && TYPE_PTROBV_P (intype)))
    {
      pedwarn ("ANSI C++ forbids casting between pointers to functions and objects");
      if (TREE_READONLY_DECL_P (expr))
	expr = decl_constant_value (expr);
      return fold (build1 (NOP_EXPR, type, expr));
    }
  else
    {
      cp_error ("reinterpret_cast from `%T' to `%T'", intype, type);
      return error_mark_node;
    }
      
  return cp_convert (type, expr);
}

tree
build_const_cast (type, expr)
   tree type, expr;
{
  tree intype;

  if (type == error_mark_node || expr == error_mark_node)
    return error_mark_node;

  if (TREE_CODE (expr) == OFFSET_REF)
    expr = resolve_offset_ref (expr);

  if (processing_template_decl)
    {
      tree t = build_min (CONST_CAST_EXPR, type, expr);
      return t;
    }

  if (TREE_CODE (type) != REFERENCE_TYPE)
    {
      expr = decay_conversion (expr);

      /* build_c_cast puts on a NOP_EXPR to make the result not an lvalue.
	 Strip such NOP_EXPRs if VALUE is being used in non-lvalue context.  */
      if (TREE_CODE (expr) == NOP_EXPR
	  && TREE_TYPE (expr) == TREE_TYPE (TREE_OPERAND (expr, 0)))
	expr = TREE_OPERAND (expr, 0);
    }

  if (type_unknown_p (expr))
    {
      expr = instantiate_type (type, expr, 1);
      if (expr == error_mark_node)
	return error_mark_node;
    }

  intype = TREE_TYPE (expr);

  if (comptypes (TYPE_MAIN_VARIANT (intype), TYPE_MAIN_VARIANT (type), 1))
    return build_static_cast (type, expr);
  else if (TREE_CODE (type) == REFERENCE_TYPE)
    {
      if (! real_lvalue_p (expr))
	{
	  cp_error ("const_cast from `%T' rvalue to `%T'", intype, type);
	  return error_mark_node;
	}

      if (comp_ptr_ttypes_const (TREE_TYPE (type), intype))
	return (convert_from_reference
		(convert_to_reference (type, expr, CONV_CONST|CONV_IMPLICIT,
				       LOOKUP_COMPLAIN, NULL_TREE)));
    }
  else if (TREE_CODE (type) == POINTER_TYPE
	   && TREE_CODE (intype) == POINTER_TYPE
	   && comp_ptr_ttypes_const (TREE_TYPE (type), TREE_TYPE (intype)))
    return cp_convert (type, expr);

  cp_error ("const_cast from `%T' to `%T'", intype, type);
  return error_mark_node;
}

/* Build an expression representing a cast to type TYPE of expression EXPR.

   ALLOW_NONCONVERTING is true if we should allow non-converting constructors
   when doing the cast.  */

tree
build_c_cast (type, expr)
     tree type, expr;
{
  register tree value = expr;

  if (type == error_mark_node || expr == error_mark_node)
    return error_mark_node;

  /* build_c_cast puts on a NOP_EXPR to make the result not an lvalue.
     Strip such NOP_EXPRs if VALUE is being used in non-lvalue context.  */
  if (TREE_CODE (type) != REFERENCE_TYPE
      && TREE_CODE (value) == NOP_EXPR
      && TREE_TYPE (value) == TREE_TYPE (TREE_OPERAND (value, 0)))
    value = TREE_OPERAND (value, 0);

  if (TREE_TYPE (expr)
      && TREE_CODE (TREE_TYPE (expr)) == OFFSET_TYPE
      && TREE_CODE (type) != OFFSET_TYPE)
    value = resolve_offset_ref (value);

  if (TREE_CODE (type) == ARRAY_TYPE)
    {
      /* Allow casting from T1* to T2[] because Cfront allows it.
	 NIHCL uses it. It is not valid ANSI C however, and hence, not
	 valid ANSI C++.  */
      if (TREE_CODE (TREE_TYPE (expr)) == POINTER_TYPE)
	{
	  if (pedantic)
	    pedwarn ("ANSI C++ forbids casting to an array type");
	  type = build_pointer_type (TREE_TYPE (type));
	}
      else
	{
	  error ("ANSI C++ forbids casting to an array type");
	  return error_mark_node;
	}
    }

  if (TREE_CODE (type) == FUNCTION_TYPE
      || TREE_CODE (type) == METHOD_TYPE)
    {
      cp_error ("casting to function type `%T'", type);
      return error_mark_node;
    }

  if (IS_SIGNATURE (type))
    {
      error ("cast specifies signature type");
      return error_mark_node;
    }

  if (processing_template_decl)
    {
      tree t = build_min (CAST_EXPR, type,
			  min_tree_cons (NULL_TREE, value, NULL_TREE));
      return t;
    }

  if (TREE_CODE (type) == VOID_TYPE)
    value = build1 (CONVERT_EXPR, type, value);
  else if (TREE_TYPE (value) == NULL_TREE
      || type_unknown_p (value))
    {
      value = instantiate_type (type, value, 1);
      /* Did we lose?  */
      if (value == error_mark_node)
	return error_mark_node;
    }
  else
    {
      tree otype;

      /* Convert functions and arrays to pointers and
	 convert references to their expanded types,
	 but don't convert any other types.  */
      if (TREE_CODE (TREE_TYPE (value)) == FUNCTION_TYPE
	  || (TREE_CODE (TREE_TYPE (value)) == METHOD_TYPE
	      /* Don't do the default conversion if we want a
		 pointer to a function.  */
	      && ! (TREE_CODE (type) == POINTER_TYPE
		    && TREE_CODE (TREE_TYPE (type)) == FUNCTION_TYPE))
	  || TREE_CODE (TREE_TYPE (value)) == ARRAY_TYPE
	  || TREE_CODE (TREE_TYPE (value)) == REFERENCE_TYPE)
	value = default_conversion (value);
      otype = TREE_TYPE (value);

      /* Optionally warn about potentially worrisome casts.  */

      if (warn_cast_qual
	  && TREE_CODE (type) == POINTER_TYPE
	  && TREE_CODE (otype) == POINTER_TYPE)
	{
	  /* For C++ we make these regular warnings, rather than
	     softening them into pedwarns.  */
	  if (TYPE_VOLATILE (TREE_TYPE (otype))
	      && ! TYPE_VOLATILE (TREE_TYPE (type)))
	    warning ("cast discards `volatile' from pointer target type");
	  if (TYPE_READONLY (TREE_TYPE (otype))
	      && ! TYPE_READONLY (TREE_TYPE (type)))
	    warning ("cast discards `const' from pointer target type");
	}

      /* Warn about possible alignment problems.  */
      if (STRICT_ALIGNMENT && warn_cast_align
	  && TREE_CODE (type) == POINTER_TYPE
	  && TREE_CODE (otype) == POINTER_TYPE
	  && TREE_CODE (TREE_TYPE (otype)) != VOID_TYPE
	  && TREE_CODE (TREE_TYPE (otype)) != FUNCTION_TYPE
	  && TYPE_ALIGN (TREE_TYPE (type)) > TYPE_ALIGN (TREE_TYPE (otype)))
	warning ("cast increases required alignment of target type");

#if 0
      /* We should see about re-enabling these, they seem useful to
         me.  */
      if (TREE_CODE (type) == INTEGER_TYPE
	  && TREE_CODE (otype) == POINTER_TYPE
	  && TYPE_PRECISION (type) != TYPE_PRECISION (otype))
	warning ("cast from pointer to integer of different size");

      if (TREE_CODE (type) == POINTER_TYPE
	  && TREE_CODE (otype) == INTEGER_TYPE
	  && TYPE_PRECISION (type) != TYPE_PRECISION (otype)
	  /* Don't warn about converting 0 to pointer,
	     provided the 0 was explicit--not cast or made by folding.  */
	  && !(TREE_CODE (value) == INTEGER_CST && integer_zerop (value)))
	warning ("cast to pointer from integer of different size");
#endif

      if (TREE_CODE (type) == REFERENCE_TYPE)
	value = (convert_from_reference
		 (convert_to_reference (type, value, CONV_C_CAST,
					LOOKUP_COMPLAIN, NULL_TREE)));
      else
	{
	  tree ovalue;

	  if (TREE_READONLY_DECL_P (value))
	    value = decl_constant_value (value);

	  ovalue = value;
	  value = convert_force (type, value, CONV_C_CAST);

	  /* Ignore any integer overflow caused by the cast.  */
	  if (TREE_CODE (value) == INTEGER_CST)
	    {
	      TREE_OVERFLOW (value) = TREE_OVERFLOW (ovalue);
	      TREE_CONSTANT_OVERFLOW (value) = TREE_CONSTANT_OVERFLOW (ovalue);
	    }
	}
    }

    /* Always produce some operator for an explicit cast,
       so we can tell (for -pedantic) that the cast is no lvalue.  */
  if (TREE_CODE (type) != REFERENCE_TYPE && value == expr
      && real_lvalue_p (value))
    value = non_lvalue (value);

  return value;
}

static tree
expand_target_expr (t)
     tree t;
{
  extern int temp_slot_level;
  extern int target_temp_slot_level;
  int old_temp_level = target_temp_slot_level;

  tree xval = make_node (RTL_EXPR);
  rtx rtxval;

  /* Any TARGET_EXPR temps live only as long as the outer temp level.
     Since they are preserved in this new inner level, we know they
     will make it into the outer level.  */
  push_temp_slots ();
  target_temp_slot_level = temp_slot_level;

  do_pending_stack_adjust ();
  start_sequence_for_rtl_expr (xval);
  emit_note (0, -1);
  rtxval = expand_expr (t, NULL_RTX, VOIDmode, EXPAND_NORMAL);
  do_pending_stack_adjust ();
  TREE_SIDE_EFFECTS (xval) = 1;
  RTL_EXPR_SEQUENCE (xval) = get_insns ();
  end_sequence ();
  RTL_EXPR_RTL (xval) = rtxval;
  TREE_TYPE (xval) = TREE_TYPE (t);

  pop_temp_slots ();
  target_temp_slot_level = old_temp_level;

  return xval;
}

/* Build an assignment expression of lvalue LHS from value RHS.
   MODIFYCODE is the code for a binary operator that we use
   to combine the old value of LHS with RHS to get the new value.
   Or else MODIFYCODE is NOP_EXPR meaning do a simple assignment.

   C++: If MODIFYCODE is INIT_EXPR, then leave references unbashed.  */

tree
build_modify_expr (lhs, modifycode, rhs)
     tree lhs;
     enum tree_code modifycode;
     tree rhs;
{
  register tree result;
  tree newrhs = rhs;
  tree lhstype = TREE_TYPE (lhs);
  tree olhstype = lhstype;
  tree olhs = lhs;

  /* Avoid duplicate error messages from operands that had errors.  */
  if (lhs == error_mark_node || rhs == error_mark_node)
    return error_mark_node;

  /* Types that aren't fully specified cannot be used in assignments.  */
  lhs = require_complete_type (lhs);

  newrhs = rhs;

  /* Handle assignment to signature pointers/refs.  */

  if (TYPE_LANG_SPECIFIC (lhstype)
      && (IS_SIGNATURE_POINTER (lhstype) || IS_SIGNATURE_REFERENCE (lhstype)))
    {
      return build_signature_pointer_constructor (lhs, rhs);
    }

  /* Handle control structure constructs used as "lvalues".  */

  switch (TREE_CODE (lhs))
    {
      /* Handle --foo = 5; as these are valid constructs in C++ */
    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
      if (TREE_SIDE_EFFECTS (TREE_OPERAND (lhs, 0)))
	lhs = build (TREE_CODE (lhs), TREE_TYPE (lhs),
		     stabilize_reference (TREE_OPERAND (lhs, 0)),
		     TREE_OPERAND (lhs, 1));
      return build (COMPOUND_EXPR, lhstype,
		    lhs,
		    build_modify_expr (TREE_OPERAND (lhs, 0),
				       modifycode, rhs));

      /* Handle (a, b) used as an "lvalue".  */
    case COMPOUND_EXPR:
      newrhs = build_modify_expr (TREE_OPERAND (lhs, 1),
				  modifycode, rhs);
      if (newrhs == error_mark_node)
	return error_mark_node;
      return build (COMPOUND_EXPR, lhstype,
		    TREE_OPERAND (lhs, 0), newrhs);

    case MODIFY_EXPR:
      newrhs = build_modify_expr (TREE_OPERAND (lhs, 0), modifycode, rhs);
      if (newrhs == error_mark_node)
	return error_mark_node;
      return build (COMPOUND_EXPR, lhstype, lhs, newrhs);

      /* Handle (a ? b : c) used as an "lvalue".  */
    case COND_EXPR:
      rhs = save_expr (rhs);
      {
	/* Produce (a ? (b = rhs) : (c = rhs))
	   except that the RHS goes through a save-expr
	   so the code to compute it is only emitted once.  */
	tree cond
	  = build_conditional_expr (TREE_OPERAND (lhs, 0),
				    build_modify_expr (cp_convert (TREE_TYPE (lhs), TREE_OPERAND (lhs, 1)),
						       modifycode, rhs),
				    build_modify_expr (cp_convert (TREE_TYPE (lhs), TREE_OPERAND (lhs, 2)),
						       modifycode, rhs));
	if (cond == error_mark_node)
	  return cond;
	/* Make sure the code to compute the rhs comes out
	   before the split.  */
	return build (COMPOUND_EXPR, TREE_TYPE (lhs),
		      /* Case to void to suppress warning
			 from warn_if_unused_value.  */
		      cp_convert (void_type_node, rhs), cond);
      }
    }

  if (TREE_CODE (lhs) == OFFSET_REF)
    {
      if (TREE_OPERAND (lhs, 0) == NULL_TREE)
	{
	  /* Static class member?  */
	  tree member = TREE_OPERAND (lhs, 1);
	  if (TREE_CODE (member) == VAR_DECL)
	    lhs = member;
	  else
	    {
	      compiler_error ("invalid static class member");
	      return error_mark_node;
	    }
	}
      else
	lhs = resolve_offset_ref (lhs);

      olhstype = lhstype = TREE_TYPE (lhs);
    }

  if (TREE_CODE (lhstype) == REFERENCE_TYPE
      && modifycode != INIT_EXPR)
    {
      lhs = convert_from_reference (lhs);
      olhstype = lhstype = TREE_TYPE (lhs);
    }

  /* If a binary op has been requested, combine the old LHS value with the RHS
     producing the value we should actually store into the LHS.  */

  if (modifycode == INIT_EXPR)
    {
      if (! IS_AGGR_TYPE (lhstype))
	/* Do the default thing */;
      else if (! TYPE_HAS_CONSTRUCTOR (lhstype))
	{
	  cp_error ("`%T' has no constructors", lhstype);
	  return error_mark_node;
	}
      else if (TYPE_HAS_TRIVIAL_INIT_REF (lhstype)
	       && TYPE_MAIN_VARIANT (lhstype) == TYPE_MAIN_VARIANT (TREE_TYPE (newrhs)))
	/* Do the default thing */;
      else
	{
	  result = build_method_call (lhs, ctor_identifier,
				      build_expr_list (NULL_TREE, rhs),
				      TYPE_BINFO (lhstype), LOOKUP_NORMAL);
	  if (result == NULL_TREE)
	    return error_mark_node;
	  return result;
	}
    }
  else if (modifycode == NOP_EXPR)
    {
      /* `operator=' is not an inheritable operator.  */
      if (! IS_AGGR_TYPE (lhstype))
	/* Do the default thing */;
      else if (! TYPE_HAS_ASSIGNMENT (lhstype))
	{
	  cp_error ("`%T' does not define operator=", lhstype);
	  return error_mark_node;
	}
      else if (TYPE_HAS_TRIVIAL_ASSIGN_REF (lhstype)
	       && TYPE_MAIN_VARIANT (lhstype) == TYPE_MAIN_VARIANT (TREE_TYPE (newrhs)))
	{
	  build_opfncall (MODIFY_EXPR, LOOKUP_NORMAL,
			  lhs, rhs, make_node (NOP_EXPR));

	  /* Do the default thing */;
	}
      else
	{
	  result = build_opfncall (MODIFY_EXPR, LOOKUP_NORMAL,
				   lhs, rhs, make_node (NOP_EXPR));
	  if (result == NULL_TREE)
	    return error_mark_node;
	  return result;
	}
      lhstype = olhstype;
    }
  else if (PROMOTES_TO_AGGR_TYPE (lhstype, REFERENCE_TYPE))
    {
      /* This case must convert to some sort of lvalue that
	 can participate in an op= operation.  */
      tree lhs_tmp = lhs;
      tree rhs_tmp = rhs;
      if (build_default_binary_type_conversion (modifycode, &lhs_tmp, &rhs_tmp))
	{
	  lhs = stabilize_reference (lhs_tmp);
	  /* Forget it was ever anything else.  */
	  olhstype = lhstype = TREE_TYPE (lhs);
	  newrhs = build_binary_op (modifycode, lhs, rhs_tmp, 1);
	}
      else
	{
	  cp_error ("no match for `%Q(%#T, %#T)'", modifycode,
		    TREE_TYPE (lhs), TREE_TYPE (rhs));
	  return error_mark_node;
	}
    }
  else
    {
      lhs = stabilize_reference (lhs);
      newrhs = build_binary_op (modifycode, lhs, rhs, 1);
      if (newrhs == error_mark_node)
	{
	  cp_error ("  in evaluation of `%Q(%#T, %#T)'", modifycode,
		    TREE_TYPE (lhs), TREE_TYPE (rhs));
	  return error_mark_node;
	}
    }

  /* Handle a cast used as an "lvalue".
     We have already performed any binary operator using the value as cast.
     Now convert the result to the cast type of the lhs,
     and then true type of the lhs and store it there;
     then convert result back to the cast type to be the value
     of the assignment.  */

  switch (TREE_CODE (lhs))
    {
    case NOP_EXPR:
    case CONVERT_EXPR:
    case FLOAT_EXPR:
    case FIX_TRUNC_EXPR:
    case FIX_FLOOR_EXPR:
    case FIX_ROUND_EXPR:
    case FIX_CEIL_EXPR:
      if (TREE_CODE (TREE_TYPE (newrhs)) == ARRAY_TYPE
	  || TREE_CODE (TREE_TYPE (newrhs)) == FUNCTION_TYPE
	  || TREE_CODE (TREE_TYPE (newrhs)) == METHOD_TYPE
	  || TREE_CODE (TREE_TYPE (newrhs)) == OFFSET_TYPE)
	newrhs = default_conversion (newrhs);
      {
	tree inner_lhs = TREE_OPERAND (lhs, 0);
	tree result;

	/* WP 5.4.1:  The result is an lvalue if T is a reference type,
	   otherwise the result is an rvalue.   */
	if (! lvalue_p (lhs))
	  pedwarn ("ANSI C++ forbids cast to non-reference type used as lvalue");

	result = build_modify_expr (inner_lhs, NOP_EXPR,
				    cp_convert (TREE_TYPE (inner_lhs),
						cp_convert (lhstype, newrhs)));
	if (result == error_mark_node)
	  return result;
	return cp_convert (TREE_TYPE (lhs), result);
      }
    }

  /* Now we have handled acceptable kinds of LHS that are not truly lvalues.
     Reject anything strange now.  */

  if (!lvalue_or_else (lhs, "assignment"))
    return error_mark_node;

  GNU_xref_assign (lhs);

  /* Warn about storing in something that is `const'.  */
  /* For C++, don't warn if this is initialization.  */
  if (modifycode != INIT_EXPR
      /* For assignment to `const' signature pointer/reference fields,
	 don't warn either, we already printed a better message before.  */
      && ! (TREE_CODE (lhs) == COMPONENT_REF
	    && (IS_SIGNATURE_POINTER (TREE_TYPE (TREE_OPERAND (lhs, 0)))
		|| IS_SIGNATURE_REFERENCE (TREE_TYPE (TREE_OPERAND (lhs, 0)))))
      && (TREE_READONLY (lhs) || TYPE_READONLY (lhstype)
	  || ((TREE_CODE (lhstype) == RECORD_TYPE
	       || TREE_CODE (lhstype) == UNION_TYPE)
	      && C_TYPE_FIELDS_READONLY (lhstype))
	  || (TREE_CODE (lhstype) == REFERENCE_TYPE
	      && TYPE_READONLY (TREE_TYPE (lhstype)))))
    readonly_error (lhs, "assignment", 0);

  /* If storing into a structure or union member,
     it has probably been given type `int'.
     Compute the type that would go with
     the actual amount of storage the member occupies.  */

  if (TREE_CODE (lhs) == COMPONENT_REF
      && (TREE_CODE (lhstype) == INTEGER_TYPE
	  || TREE_CODE (lhstype) == REAL_TYPE
	  || TREE_CODE (lhstype) == ENUMERAL_TYPE))
    {
      lhstype = TREE_TYPE (get_unwidened (lhs, 0));

      /* If storing in a field that is in actuality a short or narrower
	 than one, we must store in the field in its actual type.  */

      if (lhstype != TREE_TYPE (lhs))
	{
	  lhs = copy_node (lhs);
	  TREE_TYPE (lhs) = lhstype;
	}
    }

  /* check to see if there is an assignment to `this' */
  if (lhs == current_class_ptr)
    {
      if (flag_this_is_variable > 0
	  && DECL_NAME (current_function_decl) != NULL_TREE
	  && (DECL_NAME (current_function_decl)
	      != constructor_name (current_class_type)))
	warning ("assignment to `this' not in constructor or destructor");
      current_function_just_assigned_this = 1;
    }

  /* The TREE_TYPE of RHS may be TYPE_UNKNOWN.  This can happen
     when the type of RHS is not yet known, i.e. its type
     is inherited from LHS.  */
  rhs = require_instantiated_type (lhstype, newrhs, error_mark_node);
  if (rhs == error_mark_node)
    return error_mark_node;
  newrhs = rhs;

  if (modifycode != INIT_EXPR)
    {
      /* Make modifycode now either a NOP_EXPR or an INIT_EXPR.  */
      modifycode = NOP_EXPR;
      /* Reference-bashing */
      if (TREE_CODE (lhstype) == REFERENCE_TYPE)
	{
	  tree tmp = convert_from_reference (lhs);
	  lhstype = TREE_TYPE (tmp);
	  if (TYPE_SIZE (lhstype) == 0)
	    {
	      incomplete_type_error (lhs, lhstype);
	      return error_mark_node;
	    }
	  lhs = tmp;
	  olhstype = lhstype;
	}
      if (TREE_CODE (TREE_TYPE (newrhs)) == REFERENCE_TYPE)
	{
	  tree tmp = convert_from_reference (newrhs);
	  if (TYPE_SIZE (TREE_TYPE (tmp)) == 0)
	    {
	      incomplete_type_error (newrhs, TREE_TYPE (tmp));
	      return error_mark_node;
	    }
	  newrhs = tmp;
	}
    }

  if (TREE_SIDE_EFFECTS (lhs))
    lhs = stabilize_reference (lhs);
  if (TREE_SIDE_EFFECTS (newrhs))
    newrhs = stabilize_reference (newrhs);

  /* Convert new value to destination type.  */

  if (TREE_CODE (lhstype) == ARRAY_TYPE)
    {
      int from_array;
      
      if (! comptypes (lhstype, TREE_TYPE (rhs), 0))
	{
	  cp_error ("incompatible types in assignment of `%T' to `%T'",
		    TREE_TYPE (rhs), lhstype);
	  return error_mark_node;
	}

      /* Allow array assignment in compiler-generated code.  */
      if (pedantic && ! DECL_ARTIFICIAL (current_function_decl))
	pedwarn ("ANSI C++ forbids assignment of arrays");

      /* Have to wrap this in RTL_EXPR for two cases:
	 in base or member initialization and if we
	 are a branch of a ?: operator.  Since we
	 can't easily know the latter, just do it always.  */

      result = make_node (RTL_EXPR);

      TREE_TYPE (result) = void_type_node;
      do_pending_stack_adjust ();
      start_sequence_for_rtl_expr (result);

      /* As a matter of principle, `start_sequence' should do this.  */
      emit_note (0, -1);

      from_array = TREE_CODE (TREE_TYPE (newrhs)) == ARRAY_TYPE
	           ? 1 + (modifycode != INIT_EXPR): 0;
      expand_vec_init (lhs, lhs, array_type_nelts (lhstype), newrhs,
		       from_array);

      do_pending_stack_adjust ();

      TREE_SIDE_EFFECTS (result) = 1;
      RTL_EXPR_SEQUENCE (result) = get_insns ();
      RTL_EXPR_RTL (result) = const0_rtx;
      end_sequence ();
      return result;
    }

  if (modifycode == INIT_EXPR)
    {
      newrhs = convert_for_initialization (lhs, lhstype, newrhs, LOOKUP_NORMAL,
					   "assignment", NULL_TREE, 0);
      if (lhs == DECL_RESULT (current_function_decl))
	{
	  if (DECL_INITIAL (lhs))
	    warning ("return value from function receives multiple initializations");
	  DECL_INITIAL (lhs) = newrhs;
	}
    }
  else
    {
      /* Avoid warnings on enum bit fields.  */
      if (TREE_CODE (olhstype) == ENUMERAL_TYPE
	  && TREE_CODE (lhstype) == INTEGER_TYPE)
	{
	  newrhs = convert_for_assignment (olhstype, newrhs, "assignment",
					   NULL_TREE, 0);
	  newrhs = convert_force (lhstype, newrhs, 0);
	}
      else
	newrhs = convert_for_assignment (lhstype, newrhs, "assignment",
					 NULL_TREE, 0);
      if (TREE_CODE (newrhs) == CALL_EXPR
	  && TYPE_NEEDS_CONSTRUCTING (lhstype))
	newrhs = build_cplus_new (lhstype, newrhs);

      /* Can't initialize directly from a TARGET_EXPR, since that would
	 cause the lhs to be constructed twice, and possibly result in
	 accidental self-initialization.  So we force the TARGET_EXPR to be
	 expanded without a target.  */
      if (TREE_CODE (newrhs) == TARGET_EXPR)
	newrhs = build (COMPOUND_EXPR, TREE_TYPE (newrhs), newrhs,
			TREE_OPERAND (newrhs, 0));
    }

  if (newrhs == error_mark_node)
    return error_mark_node;

  if (TREE_CODE (newrhs) == COND_EXPR)
    {
      tree lhs1;
      tree cond = TREE_OPERAND (newrhs, 0);

      if (TREE_SIDE_EFFECTS (lhs))
	cond = build_compound_expr (tree_cons
				    (NULL_TREE, lhs,
				     build_expr_list (NULL_TREE, cond)));

      /* Cannot have two identical lhs on this one tree (result) as preexpand
	 calls will rip them out and fill in RTL for them, but when the
	 rtl is generated, the calls will only be in the first side of the
	 condition, not on both, or before the conditional jump! (mrs) */
      lhs1 = break_out_calls (lhs);

      if (lhs == lhs1)
	/* If there's no change, the COND_EXPR behaves like any other rhs.  */
	result = build (modifycode == NOP_EXPR ? MODIFY_EXPR : INIT_EXPR,
			lhstype, lhs, newrhs);
      else
	{
	  tree result_type = TREE_TYPE (newrhs);
	  /* We have to convert each arm to the proper type because the
	     types may have been munged by constant folding.  */
	  result
	    = build (COND_EXPR, result_type, cond,
		     build_modify_expr (lhs, modifycode,
					cp_convert (result_type,
						    TREE_OPERAND (newrhs, 1))),
		     build_modify_expr (lhs1, modifycode,
					cp_convert (result_type,
						    TREE_OPERAND (newrhs, 2))));
	}
    }
  else
    result = build (modifycode == NOP_EXPR ? MODIFY_EXPR : INIT_EXPR,
		    lhstype, lhs, newrhs);

  TREE_SIDE_EFFECTS (result) = 1;

  /* If we got the LHS in a different type for storing in,
     convert the result back to the nominal type of LHS
     so that the value we return always has the same type
     as the LHS argument.  */

  if (olhstype == TREE_TYPE (result))
    return result;
  /* Avoid warnings converting integral types back into enums
     for enum bit fields.  */
  if (TREE_CODE (TREE_TYPE (result)) == INTEGER_TYPE
      && TREE_CODE (olhstype) == ENUMERAL_TYPE)
    {
      result = build (COMPOUND_EXPR, olhstype, result, olhs);
      TREE_NO_UNUSED_WARNING (result) = 1;
      return result;
    }
  return convert_for_assignment (olhstype, result, "assignment",
				 NULL_TREE, 0);
}

tree
build_x_modify_expr (lhs, modifycode, rhs)
     tree lhs;
     enum tree_code modifycode;
     tree rhs;
{
  if (processing_template_decl)
    return build_min_nt (MODOP_EXPR, lhs,
			 build_min_nt (modifycode, NULL_TREE, NULL_TREE), rhs);

  if (modifycode != NOP_EXPR)
    {
      tree rval = build_opfncall (MODIFY_EXPR, LOOKUP_NORMAL, lhs, rhs,
				  make_node (modifycode));
      if (rval)
	return rval;
    }
  return build_modify_expr (lhs, modifycode, rhs);
}

/* Return 0 if EXP is not a valid lvalue in this language
   even though `lvalue_or_else' would accept it.  */

int
language_lvalue_valid (exp)
     tree exp;
{
  return 1;
}

/* Get difference in deltas for different pointer to member function
   types.  Return integer_zero_node, if FROM cannot be converted to a
   TO type.  If FORCE is true, then allow reverse conversions as well.  */

static tree
get_delta_difference (from, to, force)
     tree from, to;
     int force;
{
  tree delta = integer_zero_node;
  tree binfo;
  
  if (to == from)
    return delta;

  /* Should get_base_distance here, so we can check if any thing along the
     path is virtual, and we need to make sure we stay
     inside the real binfos when going through virtual bases.
     Maybe we should replace virtual bases with
     binfo_member (...CLASSTYPE_VBASECLASSES...)...  (mrs) */
  binfo = get_binfo (from, to, 1);
  if (binfo == error_mark_node)
    {
      error ("   in pointer to member function conversion");
      return delta;
    }
  if (binfo == 0)
    {
      if (!force)
	{
	  error_not_base_type (from, to);
	  error ("   in pointer to member conversion");
	  return delta;
	}
      binfo = get_binfo (to, from, 1);
      if (binfo == error_mark_node)
	{
	  if (!force)
	    error ("   in pointer to member conversion");
	  return delta;
	}
      if (binfo == 0)
	{
	  if (!force)
	    cp_error ("cannot convert pointer to member of type %T to unrelated pointer to member of type %T", from, to);
	  return delta;
	}
      if (TREE_VIA_VIRTUAL (binfo))
	{
	  binfo = binfo_member (BINFO_TYPE (binfo),
				CLASSTYPE_VBASECLASSES (from));
	  warning ("pointer to member conversion to virtual base class will only work if you are very careful");
	}
      delta = BINFO_OFFSET (binfo);
      delta = cp_convert (ptrdiff_type_node, delta);
      
      return build_binary_op (MINUS_EXPR,
			      integer_zero_node,
			      delta, 1);
    }
  if (TREE_VIA_VIRTUAL (binfo))
    {
      warning ("pointer to member conversion from virtual base class will only work if you are very careful");
    }
  return BINFO_OFFSET (binfo);
}

static tree
build_ptrmemfunc1 (type, delta, idx, pfn, delta2)
     tree type, delta, idx, pfn, delta2;
{
  tree u;

#if 0
  /* This is the old way we did it.  We want to avoid calling
     digest_init, so that it can give an error if we use { } when
     initializing a pointer to member function.  */

  if (pfn)
    {
      u = build_nt (CONSTRUCTOR, NULL_TREE,
		    expr_tree_cons (pfn_identifier, pfn, NULL_TREE));
    }
  else
    {
      u = build_nt (CONSTRUCTOR, NULL_TREE,
		    expr_tree_cons (delta2_identifier, delta2, NULL_TREE));
    }

  u = build_nt (CONSTRUCTOR, NULL_TREE,
		expr_tree_cons (NULL_TREE, delta,
			   expr_tree_cons (NULL_TREE, idx,
				      expr_tree_cons (NULL_TREE, u, NULL_TREE))));

  return digest_init (type, u, (tree*)0);
#else
  tree delta_field, idx_field, pfn_or_delta2_field, pfn_field, delta2_field;
  tree subtype;
  int allconstant, allsimple;

  delta_field = TYPE_FIELDS (type);
  idx_field = TREE_CHAIN (delta_field);
  pfn_or_delta2_field = TREE_CHAIN (idx_field);
  subtype = TREE_TYPE (pfn_or_delta2_field);
  pfn_field = TYPE_FIELDS (subtype);
  delta2_field = TREE_CHAIN (pfn_field);

  if (pfn)
    {
      allconstant = TREE_CONSTANT (pfn);
      allsimple = !! initializer_constant_valid_p (pfn, TREE_TYPE (pfn));
      u = expr_tree_cons (pfn_field, pfn, NULL_TREE);
    }
  else
    {
      delta2 = convert_and_check (delta_type_node, delta2);
      allconstant = TREE_CONSTANT (delta2);
      allsimple = !! initializer_constant_valid_p (delta2, TREE_TYPE (delta2));
      u = expr_tree_cons (delta2_field, delta2, NULL_TREE);
    }

  delta = convert_and_check (delta_type_node, delta);
  idx = convert_and_check (delta_type_node, idx);

  allconstant = allconstant && TREE_CONSTANT (delta) && TREE_CONSTANT (idx);
  allsimple = allsimple
    && initializer_constant_valid_p (delta, TREE_TYPE (delta))
      && initializer_constant_valid_p (idx, TREE_TYPE (idx));

  u = build (CONSTRUCTOR, subtype, NULL_TREE, u);
  u = expr_tree_cons (delta_field, delta,
		 expr_tree_cons (idx_field, idx,
			    expr_tree_cons (pfn_or_delta2_field, u, NULL_TREE)));
  u = build (CONSTRUCTOR, type, NULL_TREE, u);
  TREE_CONSTANT (u) = allconstant;
  TREE_STATIC (u) = allconstant && allsimple;
  return u;
#endif
}

/* Build a constructor for a pointer to member function.  It can be
   used to initialize global variables, local variable, or used
   as a value in expressions.  TYPE is the POINTER to METHOD_TYPE we
   want to be.

   If FORCE is non-zero, then force this conversion, even if
   we would rather not do it.  Usually set when using an explicit
   cast.

   Return error_mark_node, if something goes wrong.  */

tree
build_ptrmemfunc (type, pfn, force)
     tree type, pfn;
     int force;
{
  tree idx = integer_zero_node;
  tree delta = integer_zero_node;
  tree delta2 = integer_zero_node;
  tree vfield_offset;
  tree npfn = NULL_TREE;

  /* Handle multiple conversions of pointer to member functions.  */
  if (TYPE_PTRMEMFUNC_P (TREE_TYPE (pfn)))
    {
      tree ndelta, ndelta2;
      tree e1, e2, e3, n;

      /* Is is already the right type? */
      if (type == TYPE_PTRMEMFUNC_FN_TYPE (TREE_TYPE (pfn)))
	return pfn;

      ndelta = cp_convert (ptrdiff_type_node, build_component_ref (pfn, delta_identifier, NULL_TREE, 0));
      ndelta2 = cp_convert (ptrdiff_type_node, DELTA2_FROM_PTRMEMFUNC (pfn));
      idx = build_component_ref (pfn, index_identifier, NULL_TREE, 0);

      n = get_delta_difference (TYPE_METHOD_BASETYPE (TREE_TYPE (TYPE_PTRMEMFUNC_FN_TYPE (TREE_TYPE (pfn)))),
				TYPE_METHOD_BASETYPE (TREE_TYPE (type)),
				force);

      delta = build_binary_op (PLUS_EXPR, ndelta, n, 1);
      delta2 = build_binary_op (PLUS_EXPR, ndelta2, n, 1);
      e1 = fold (build (GT_EXPR, boolean_type_node, idx, integer_zero_node));
	  
      e2 = build_ptrmemfunc1 (TYPE_GET_PTRMEMFUNC_TYPE (type), delta, idx,
			      NULL_TREE, delta2);

      pfn = PFN_FROM_PTRMEMFUNC (pfn);
      npfn = build1 (NOP_EXPR, type, pfn);
      TREE_CONSTANT (npfn) = TREE_CONSTANT (pfn);

      e3 = build_ptrmemfunc1 (TYPE_GET_PTRMEMFUNC_TYPE (type), delta, idx, npfn,
			      NULL_TREE);
      return build_conditional_expr (e1, e2, e3);
    }

  /* Handle null pointer to member function conversions.  */
  if (integer_zerop (pfn))
    {
      pfn = build_c_cast (type, integer_zero_node);
      return build_ptrmemfunc1 (TYPE_GET_PTRMEMFUNC_TYPE (type),
				integer_zero_node, integer_zero_node,
				pfn, NULL_TREE);
    }

  if (TREE_CODE (pfn) == TREE_LIST
      || (TREE_CODE (pfn) == ADDR_EXPR
	  && TREE_CODE (TREE_OPERAND (pfn, 0)) == TREE_LIST))
    return instantiate_type (type, pfn, 1);

  /* Allow pointer to member conversions here.  */
  delta = get_delta_difference (TYPE_METHOD_BASETYPE (TREE_TYPE (TREE_TYPE (pfn))),
				TYPE_METHOD_BASETYPE (TREE_TYPE (type)),
				force);
  delta2 = build_binary_op (PLUS_EXPR, delta2, delta, 1);

#if 0
  /* We need to check the argument types to see if they are compatible
     (any const or volatile violations.  */
  something like this:
  comptype (TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (type))),
	    TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (TREE_TYPE (pfn)))), ?);
#endif

  if (TREE_CODE (TREE_OPERAND (pfn, 0)) != FUNCTION_DECL)
    warning ("assuming pointer to member function is non-virtual");

  if (TREE_CODE (TREE_OPERAND (pfn, 0)) == FUNCTION_DECL
      && DECL_VINDEX (TREE_OPERAND (pfn, 0)))
    {
      /* Find the offset to the vfield pointer in the object.  */
      vfield_offset = get_binfo (DECL_CONTEXT (TREE_OPERAND (pfn, 0)),
				 DECL_CLASS_CONTEXT (TREE_OPERAND (pfn, 0)),
				 0);
      vfield_offset = get_vfield_offset (vfield_offset);
      delta2 = size_binop (PLUS_EXPR, vfield_offset, delta2);

      /* Map everything down one to make room for the null pointer to member.  */
      idx = size_binop (PLUS_EXPR,
			DECL_VINDEX (TREE_OPERAND (pfn, 0)),
			integer_one_node);
    }
  else
    {
      idx = size_binop (MINUS_EXPR, integer_zero_node, integer_one_node);

      if (type == TREE_TYPE (pfn))
	{
	  npfn = pfn;
	}
      else
	{
	  npfn = build1 (NOP_EXPR, type, pfn);
	  TREE_CONSTANT (npfn) = TREE_CONSTANT (pfn);
	}
    }

  return build_ptrmemfunc1 (TYPE_GET_PTRMEMFUNC_TYPE (type), delta, idx, npfn, delta2);
}

/* Convert value RHS to type TYPE as preparation for an assignment
   to an lvalue of type TYPE.
   The real work of conversion is done by `convert'.
   The purpose of this function is to generate error messages
   for assignments that are not allowed in C.
   ERRTYPE is a string to use in error messages:
   "assignment", "return", etc.

   C++: attempts to allow `convert' to find conversions involving
   implicit type conversion between aggregate and scalar types
   as per 8.5.6 of C++ manual.  Does not randomly dereference
   pointers to aggregates!  */

static tree
convert_for_assignment (type, rhs, errtype, fndecl, parmnum)
     tree type, rhs;
     char *errtype;
     tree fndecl;
     int parmnum;
{
  register enum tree_code codel = TREE_CODE (type);
  register tree rhstype;
  register enum tree_code coder = TREE_CODE (TREE_TYPE (rhs));

  if (coder == UNKNOWN_TYPE)
    rhs = instantiate_type (type, rhs, 1);

  if (coder == ERROR_MARK)
    return error_mark_node;

  if (codel == OFFSET_TYPE)
    {
      type = TREE_TYPE (type);
      codel = TREE_CODE (type);
    }

  /* Strip NON_LVALUE_EXPRs since we aren't using as an lvalue.  */
  if (TREE_CODE (rhs) == NON_LVALUE_EXPR)
    rhs = TREE_OPERAND (rhs, 0);

  if (rhs == error_mark_node)
    return error_mark_node;

  if (TREE_VALUE (rhs) == error_mark_node)
    return error_mark_node;

  if (TREE_CODE (TREE_TYPE (rhs)) == OFFSET_TYPE)
    {
      rhs = resolve_offset_ref (rhs);
      if (rhs == error_mark_node)
	return error_mark_node;
      rhstype = TREE_TYPE (rhs);
      coder = TREE_CODE (rhstype);
    }

  if (TREE_CODE (TREE_TYPE (rhs)) == ARRAY_TYPE
      || TREE_CODE (TREE_TYPE (rhs)) == FUNCTION_TYPE
      || TREE_CODE (TREE_TYPE (rhs)) == METHOD_TYPE)
    rhs = default_conversion (rhs);
  else if (TREE_CODE (TREE_TYPE (rhs)) == REFERENCE_TYPE)
    rhs = convert_from_reference (rhs);

  rhstype = TREE_TYPE (rhs);
  coder = TREE_CODE (rhstype);

  /* This should no longer change types on us.  */
  if (TREE_CODE (rhs) == CONST_DECL)
    rhs = DECL_INITIAL (rhs);
  else if (TREE_READONLY_DECL_P (rhs))
    rhs = decl_constant_value (rhs);

  if (type == rhstype)
    {
      overflow_warning (rhs);
      return rhs;
    }

  if (coder == VOID_TYPE)
    {
      error ("void value not ignored as it ought to be");
      return error_mark_node;
    }
  /* Arithmetic types all interconvert.  */
  if ((codel == INTEGER_TYPE || codel == REAL_TYPE || codel == BOOLEAN_TYPE
       || codel == COMPLEX_TYPE)
       && (coder == INTEGER_TYPE || coder == REAL_TYPE || coder == BOOLEAN_TYPE
	   || coder == COMPLEX_TYPE))
    {
      /* But we should warn if assigning REAL_TYPE to INTEGER_TYPE.  */
      if (coder == REAL_TYPE && codel == INTEGER_TYPE)
	{
	  if (fndecl)
	    cp_warning ("`%T' used for argument %P of `%D'",
			rhstype, parmnum, fndecl);
	  else
	    cp_warning ("%s to `%T' from `%T'", errtype, type, rhstype);
	}
      /* And we should warn if assigning a negative value to
	 an unsigned variable.  */
      else if (TREE_UNSIGNED (type) && codel != BOOLEAN_TYPE)
	{
	  if (TREE_CODE (rhs) == INTEGER_CST
	      && TREE_NEGATED_INT (rhs))
	    {
	      if (fndecl)
		cp_warning ("negative value `%E' passed as argument %P of `%D'",
			    rhs, parmnum, fndecl);
	      else
		cp_warning ("%s of negative value `%E' to `%T'",
			    errtype, rhs, type);
	    }
	  overflow_warning (rhs);
	  if (TREE_CONSTANT (rhs))
	    rhs = fold (rhs);
	}

      return convert_and_check (type, rhs);
    }
  /* Conversions involving enums.  */
  else if ((codel == ENUMERAL_TYPE
	    && (INTEGRAL_CODE_P (coder) || coder == REAL_TYPE))
	   || (coder == ENUMERAL_TYPE
	       && (INTEGRAL_CODE_P (codel) || codel == REAL_TYPE)))
    {
      return ocp_convert (type, rhs, CONV_IMPLICIT, LOOKUP_NORMAL);
    }
  /* Conversions among pointers */
  else if (codel == POINTER_TYPE
	   && (coder == POINTER_TYPE
	       || (coder == RECORD_TYPE
		   && (IS_SIGNATURE_POINTER (rhstype)
		       || IS_SIGNATURE_REFERENCE (rhstype)))))
    {
      register tree ttl = TREE_TYPE (type);
      register tree ttr;
      int ctt = 0;

      if (coder == RECORD_TYPE)
	{
	  rhs = build_optr_ref (rhs);
	  rhstype = TREE_TYPE (rhs);
	}
      ttr = TREE_TYPE (rhstype);

      /* If both pointers are of aggregate type, then we
	 can give better error messages, and save some work
	 as well.  */
      if (TREE_CODE (ttl) == RECORD_TYPE && TREE_CODE (ttr) == RECORD_TYPE)
	{
	  tree binfo;

	  if (TYPE_MAIN_VARIANT (ttl) == TYPE_MAIN_VARIANT (ttr)
	      || type == class_star_type_node
	      || rhstype == class_star_type_node)
	    binfo = TYPE_BINFO (ttl);
	  else
	    binfo = get_binfo (ttl, ttr, 1);

	  if (binfo == error_mark_node)
	    return error_mark_node;
	  if (binfo == 0)
	    return error_not_base_type (ttl, ttr);

	  if (! TYPE_READONLY (ttl) && TYPE_READONLY (ttr))
	    {
	      if (fndecl)
		cp_pedwarn ("passing `%T' as argument %P of `%D' discards const",
			    rhstype, parmnum, fndecl);
	      else
		cp_pedwarn ("%s to `%T' from `%T' discards const",
			    errtype, type, rhstype);
	    }
	  if (! TYPE_VOLATILE (ttl) && TYPE_VOLATILE (ttr))
	    {
	      if (fndecl)
		cp_pedwarn ("passing `%T' as argument %P of `%D' discards volatile",
			    rhstype, parmnum, fndecl);
	      else
		cp_pedwarn ("%s to `%T' from `%T' discards volatile",
			    errtype, type, rhstype);
	    }
	}

      /* Any non-function converts to a [const][volatile] void *
	 and vice versa; otherwise, targets must be the same.
	 Meanwhile, the lhs target must have all the qualifiers of the rhs.  */
      else if (TYPE_MAIN_VARIANT (ttl) == void_type_node
	       || TYPE_MAIN_VARIANT (ttr) == void_type_node
	       || (ctt = comp_target_types (type, rhstype, 1))
	       || (unsigned_type (TYPE_MAIN_VARIANT (ttl))
		   == unsigned_type (TYPE_MAIN_VARIANT (ttr))))
	{
	  /* ARM $4.8, commentary on p39.  */
	  if (TYPE_MAIN_VARIANT (ttl) == void_type_node
	      && TREE_CODE (ttr) == OFFSET_TYPE)
	    {
	      cp_error ("no standard conversion from `%T' to `void *'", ttr);
	      return error_mark_node;
	    }

	  if (ctt < 0 && TYPE_MAIN_VARIANT (ttl) != TYPE_MAIN_VARIANT (ttr))
	    cp_pedwarn ("converting `%T' to `%T' is a contravariance violation",
			rhstype, type);

	  if (TYPE_MAIN_VARIANT (ttl) != void_type_node
	      && TYPE_MAIN_VARIANT (ttr) == void_type_node
	      && ! null_ptr_cst_p (rhs))
	    {
	      if (coder == RECORD_TYPE)
		cp_pedwarn ("implicit conversion of signature pointer to type `%T'",
			    type);
	      else
		pedwarn ("ANSI C++ forbids implicit conversion from `void *' in %s",
			 errtype);
	    }
	  /* Const and volatile mean something different for function types,
	     so the usual warnings are not appropriate.  */
	  else if ((TREE_CODE (ttr) != FUNCTION_TYPE && TREE_CODE (ttr) != METHOD_TYPE)
		   || (TREE_CODE (ttl) != FUNCTION_TYPE && TREE_CODE (ttl) != METHOD_TYPE))
	    {
	      if (TREE_CODE (ttl) == OFFSET_TYPE
		  && binfo_member (TYPE_OFFSET_BASETYPE (ttr),
				   CLASSTYPE_VBASECLASSES (TYPE_OFFSET_BASETYPE (ttl))))
		{
		  sorry ("%s between pointer to members converting across virtual baseclasses", errtype);
		  return error_mark_node;
		}
	      else if (! TYPE_READONLY (ttl) && TYPE_READONLY (ttr))
		{
		  if (fndecl)
		    cp_pedwarn ("passing `%T' as argument %P of `%D' discards const",
				rhstype, parmnum, fndecl);
		  else
		    cp_pedwarn ("%s to `%T' from `%T' discards const",
				errtype, type, rhstype);
		}
	      else if (! TYPE_VOLATILE (ttl) && TYPE_VOLATILE (ttr))
		{
		  if (fndecl)
		    cp_pedwarn ("passing `%T' as argument %P of `%D' discards volatile",
				rhstype, parmnum, fndecl);
		  else
		    cp_pedwarn ("%s to `%T' from `%T' discards volatile",
				errtype, type, rhstype);
		}
	      else if (TREE_CODE (ttl) == TREE_CODE (ttr)
		       && ! comp_target_types (type, rhstype, 1))
		{
		  if (fndecl)
		    cp_pedwarn ("passing `%T' as argument %P of `%D' changes signedness",
				rhstype, parmnum, fndecl);
		  else
		    cp_pedwarn ("%s to `%T' from `%T' changes signedness",
				errtype, type, rhstype);
		}
	    }
	}
      else
	{
	  int add_quals = 0, const_parity = 0, volatile_parity = 0;
	  int left_const = 1;
	  int unsigned_parity;
	  int nptrs = 0;

	  /* This code is basically a duplicate of comp_ptr_ttypes_real.  */
	  for (; ; ttl = TREE_TYPE (ttl), ttr = TREE_TYPE (ttr))
	    {
	      nptrs -= 1;
	      const_parity |= (TYPE_READONLY (ttl) < TYPE_READONLY (ttr));
	      volatile_parity |= (TYPE_VOLATILE (ttl) < TYPE_VOLATILE (ttr));

	      if (! left_const
		  && (TYPE_READONLY (ttl) > TYPE_READONLY (ttr)
		      || TYPE_VOLATILE (ttl) > TYPE_VOLATILE (ttr)))
		add_quals = 1;
	      left_const &= TYPE_READONLY (ttl);

	      if (TREE_CODE (ttl) != POINTER_TYPE
		  || TREE_CODE (ttr) != POINTER_TYPE)
		break;
	    }
	  unsigned_parity = TREE_UNSIGNED (ttl) - TREE_UNSIGNED (ttr);
	  if (unsigned_parity)
	    {
	      if (TREE_UNSIGNED (ttl))
		ttr = unsigned_type (ttr);
	      else
		ttl = unsigned_type (ttl);
	    }

	  if (comp_target_types (ttl, ttr, nptrs) > 0)
	    {
	      if (add_quals)
		{
		  if (fndecl)
		    cp_pedwarn ("passing `%T' as argument %P of `%D' adds cv-quals without intervening `const'",
				rhstype, parmnum, fndecl);
		  else
		    cp_pedwarn ("%s to `%T' from `%T' adds cv-quals without intervening `const'",
				errtype, type, rhstype);
		}
	      if (const_parity)
		{
		  if (fndecl)
		    cp_pedwarn ("passing `%T' as argument %P of `%D' discards const",
				rhstype, parmnum, fndecl);
		  else
		    cp_pedwarn ("%s to `%T' from `%T' discards const",
				errtype, type, rhstype);
		}
	      if (volatile_parity)
		{
		  if (fndecl)
		    cp_pedwarn ("passing `%T' as argument %P of `%D' discards volatile",
				rhstype, parmnum, fndecl);
		  else
		    cp_pedwarn ("%s to `%T' from `%T' discards volatile",
				errtype, type, rhstype);
		}
	      if (unsigned_parity > 0)
		{
		  if (fndecl)
		    cp_pedwarn ("passing `%T' as argument %P of `%D' changes signed to unsigned",
				rhstype, parmnum, fndecl);
		  else
		    cp_pedwarn ("%s to `%T' from `%T' changes signed to unsigned",
				errtype, type, rhstype);
		}
	      else if (unsigned_parity < 0)
		{
		  if (fndecl)
		    cp_pedwarn ("passing `%T' as argument %P of `%D' changes unsigned to signed",
				rhstype, parmnum, fndecl);
		  else
		    cp_pedwarn ("%s to `%T' from `%T' changes unsigned to signed",
				errtype, type, rhstype);
		}

	      /* C++ is not so friendly about converting function and
		 member function pointers as C.  Emit warnings here.  */
	      if (TREE_CODE (ttl) == FUNCTION_TYPE
		  || TREE_CODE (ttl) == METHOD_TYPE)
		if (! comptypes (ttl, ttr, 0))
		  {
		    warning ("conflicting function types in %s:", errtype);
		    cp_warning ("\t`%T' != `%T'", type, rhstype);
		  }
	    }
	  else
	    {
	      if (fndecl)
		cp_error ("passing `%T' as argument %P of `%D'",
			  rhstype, parmnum, fndecl);
	      else
		cp_error ("%s to `%T' from `%T'", errtype, type, rhstype);
	      return error_mark_node;
	    }
	}
      return cp_convert (type, rhs);
    }
  else if (codel == POINTER_TYPE && coder == INTEGER_TYPE)
    {
      /* An explicit constant 0 can convert to a pointer,
         but not a 0 that results from casting or folding.  */
      if (! (TREE_CODE (rhs) == INTEGER_CST && integer_zerop (rhs)))
	{
	  if (fndecl)
	    cp_pedwarn ("passing `%T' to argument %P of `%D' lacks a cast",
			rhstype, parmnum, fndecl);
	  else
	    cp_pedwarn ("%s to `%T' from `%T' lacks a cast",
			errtype, type, rhstype);
	}
      return cp_convert (type, rhs);
    }
  else if (codel == INTEGER_TYPE
	   && (coder == POINTER_TYPE
	       || (coder == RECORD_TYPE
		   && (IS_SIGNATURE_POINTER (rhstype)
		       || TYPE_PTRMEMFUNC_FLAG (rhstype)
		       || IS_SIGNATURE_REFERENCE (rhstype)))))
    {
      if (fndecl)
	cp_pedwarn ("passing `%T' to argument %P of `%D' lacks a cast",
		    rhstype, parmnum, fndecl);
      else
	cp_pedwarn ("%s to `%T' from `%T' lacks a cast",
		    errtype, type, rhstype);
      return cp_convert (type, rhs);
    }
  else if (codel == BOOLEAN_TYPE
	   && (coder == POINTER_TYPE
	       || (coder == RECORD_TYPE
		   && (IS_SIGNATURE_POINTER (rhstype)
		       || TYPE_PTRMEMFUNC_FLAG (rhstype)
		       || IS_SIGNATURE_REFERENCE (rhstype)))))
    return cp_convert (type, rhs);

  /* C++ */
  else if (((coder == POINTER_TYPE
	     && TREE_CODE (TREE_TYPE (rhstype)) == METHOD_TYPE)
	    || integer_zerop (rhs)
	    || TYPE_PTRMEMFUNC_P (rhstype))
	   && TYPE_PTRMEMFUNC_P (type))
    {
      tree ttl = TYPE_PTRMEMFUNC_FN_TYPE (type);
      tree ttr = (TREE_CODE (rhstype) == POINTER_TYPE ? rhstype
		    : TYPE_PTRMEMFUNC_FN_TYPE (type));
      int ctt = comp_target_types (ttl, ttr, 1);

      if (ctt < 0)
	cp_pedwarn ("converting `%T' to `%T' is a contravariance violation",
		    ttr, ttl);
      else if (ctt == 0)
	cp_error ("%s to `%T' from `%T'", errtype, ttl, ttr);

      /* compatible pointer to member functions.  */
      return build_ptrmemfunc (ttl, rhs, 0);
    }
  else if (codel == ERROR_MARK || coder == ERROR_MARK)
    return error_mark_node;

  /* This should no longer happen.  References are initialized via
     `convert_for_initialization'.  They should otherwise be
     bashed before coming here.  */
  else if (codel == REFERENCE_TYPE)
    my_friendly_abort (317);
  else if (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (TREE_TYPE (rhs)))
    {
      tree nrhs = build1 (NOP_EXPR, type, rhs);
      TREE_CONSTANT (nrhs) = TREE_CONSTANT (rhs);
      return nrhs;
    }
  else if (TYPE_HAS_CONSTRUCTOR (type) || IS_AGGR_TYPE (TREE_TYPE (rhs)))
    return cp_convert (type, rhs);
  /* Handle anachronistic conversions from (::*)() to cv void* or (*)().  */
  else if (TREE_CODE (type) == POINTER_TYPE
	   && (TREE_CODE (TREE_TYPE (type)) == FUNCTION_TYPE
	       || TYPE_MAIN_VARIANT (TREE_TYPE (type)) == void_type_node)
	   && TREE_TYPE (rhs)
	   && TYPE_PTRMEMFUNC_P (TREE_TYPE (rhs)))
    return cp_convert (type, rhs);

  cp_error ("%s to `%T' from `%T'", errtype, type, rhstype);
  return error_mark_node;
}

/* Convert RHS to be of type TYPE.  If EXP is non-zero,
   it is the target of the initialization.
   ERRTYPE is a string to use in error messages.

   Two major differences between the behavior of
   `convert_for_assignment' and `convert_for_initialization'
   are that references are bashed in the former, while
   copied in the latter, and aggregates are assigned in
   the former (operator=) while initialized in the
   latter (X(X&)).

   If using constructor make sure no conversion operator exists, if one does
   exist, an ambiguity exists.

   If flags doesn't include LOOKUP_COMPLAIN, don't complain about anything.  */

tree
convert_for_initialization (exp, type, rhs, flags, errtype, fndecl, parmnum)
     tree exp, type, rhs;
     int flags;
     char *errtype;
     tree fndecl;
     int parmnum;
{
  register enum tree_code codel = TREE_CODE (type);
  register tree rhstype;
  register enum tree_code coder;

  /* build_c_cast puts on a NOP_EXPR to make the result not an lvalue.
     Strip such NOP_EXPRs, since RHS is used in non-lvalue context.  */
  if (TREE_CODE (rhs) == NOP_EXPR
      && TREE_TYPE (rhs) == TREE_TYPE (TREE_OPERAND (rhs, 0))
      && codel != REFERENCE_TYPE)
    rhs = TREE_OPERAND (rhs, 0);

  if (rhs == error_mark_node
      || (TREE_CODE (rhs) == TREE_LIST && TREE_VALUE (rhs) == error_mark_node))
    return error_mark_node;

  if (TREE_CODE (TREE_TYPE (rhs)) == OFFSET_TYPE)
    {
      rhs = resolve_offset_ref (rhs);
      if (rhs == error_mark_node)
	return error_mark_node;
    }

  if (TREE_CODE (TREE_TYPE (rhs)) == REFERENCE_TYPE)
    rhs = convert_from_reference (rhs);

  if ((TREE_CODE (TREE_TYPE (rhs)) == ARRAY_TYPE
       && TREE_CODE (type) != ARRAY_TYPE
       && (TREE_CODE (type) != REFERENCE_TYPE
	   || TREE_CODE (TREE_TYPE (type)) != ARRAY_TYPE))
      || TREE_CODE (TREE_TYPE (rhs)) == FUNCTION_TYPE
      || TREE_CODE (TREE_TYPE (rhs)) == METHOD_TYPE)
    rhs = default_conversion (rhs);

  rhstype = TREE_TYPE (rhs);
  coder = TREE_CODE (rhstype);

  if (coder == UNKNOWN_TYPE)
    {
      rhs = instantiate_type (type, rhs, 1);
      rhstype = TREE_TYPE (rhs);
      coder = TREE_CODE (rhstype);
    }

  if (coder == ERROR_MARK)
    return error_mark_node;

  /* We accept references to incomplete types, so we can
     return here before checking if RHS is of complete type.  */
     
  if (codel == REFERENCE_TYPE)
    {
      /* This should eventually happen in convert_arguments.  */
      extern int warningcount, errorcount;
      int savew, savee;

      if (fndecl)
	savew = warningcount, savee = errorcount;
      rhs = convert_to_reference (type, rhs, CONV_IMPLICIT, flags,
				  exp ? exp : error_mark_node);
      if (fndecl)
	{
	  if (warningcount > savew)
	    cp_warning_at ("in passing argument %P of `%+D'", parmnum, fndecl);
	  else if (errorcount > savee)
	    cp_error_at ("in passing argument %P of `%+D'", parmnum, fndecl);
	}
      return rhs;
    }      

  rhs = require_complete_type (rhs);
  if (rhs == error_mark_node)
    return error_mark_node;

  if (exp != 0) exp = require_complete_type (exp);
  if (exp == error_mark_node)
    return error_mark_node;

  if (TREE_CODE (rhstype) == REFERENCE_TYPE)
    rhstype = TREE_TYPE (rhstype);

  type = complete_type (type);

  if (TYPE_LANG_SPECIFIC (type)
      && (IS_SIGNATURE_POINTER (type) || IS_SIGNATURE_REFERENCE (type)))
    return build_signature_pointer_constructor (type, rhs);

  if (IS_AGGR_TYPE (type)
      && (TYPE_NEEDS_CONSTRUCTING (type) || TREE_HAS_CONSTRUCTOR (rhs)))
    {
      if (flag_ansi_overloading)
	return ocp_convert (type, rhs, CONV_IMPLICIT|CONV_FORCE_TEMP, flags);

      if (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (rhstype))
	{
	  /* This is sufficient to perform initialization.  No need,
	     apparently, to go through X(X&) to do first-cut
	     initialization.  Return through a TARGET_EXPR so that we get
	     cleanups if it is used.  */
	  if (TREE_CODE (rhs) == CALL_EXPR)
	    {
	      rhs = build_cplus_new (type, rhs);
	      return rhs;
	    }
	  /* Handle the case of default parameter initialization and
	     initialization of static variables.  */
	  else if (TREE_CODE (rhs) == TARGET_EXPR)
	    return rhs;
	  else if (TREE_CODE (rhs) == INDIRECT_REF && TREE_HAS_CONSTRUCTOR (rhs))
	    {
	      my_friendly_assert (TREE_CODE (TREE_OPERAND (rhs, 0)) == CALL_EXPR, 318);
	      if (exp)
		{
		  my_friendly_assert (TREE_VALUE (TREE_OPERAND (TREE_OPERAND (rhs, 0), 1)) == NULL_TREE, 316);
		  TREE_VALUE (TREE_OPERAND (TREE_OPERAND (rhs, 0), 1))
		    = build_unary_op (ADDR_EXPR, exp, 0);
		}
	      else
		rhs = build_cplus_new (type, TREE_OPERAND (rhs, 0));
	      return rhs;
	    }
	  else if (TYPE_HAS_TRIVIAL_INIT_REF (type))
	    return rhs;
	}
      if (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (rhstype)
	  || (IS_AGGR_TYPE (rhstype) && UNIQUELY_DERIVED_FROM_P (type, rhstype)))
	{
	  if (TYPE_HAS_INIT_REF (type))
	    {
	      tree init = build_method_call (exp, ctor_identifier,
					     build_expr_list (NULL_TREE, rhs),
					     TYPE_BINFO (type), LOOKUP_NORMAL);

	      if (init == error_mark_node)
		return error_mark_node;

	      if (exp == 0)
		{
		  exp = build_cplus_new (type, init);
		  return exp;
		}

	      return build (COMPOUND_EXPR, type, init, exp);
	    }

	  /* ??? The following warnings are turned off because
	     this is another place where the default X(X&) constructor
	     is implemented.  */
	  if (TYPE_HAS_ASSIGNMENT (type))
	    cp_warning ("bitwise copy: `%T' defines operator=", type);

	  if (TREE_CODE (TREE_TYPE (rhs)) == REFERENCE_TYPE)
	    rhs = convert_from_reference (rhs);
	  if (type != rhstype)
	    {
	      tree nrhs = build1 (NOP_EXPR, type, rhs);
	      TREE_CONSTANT (nrhs) = TREE_CONSTANT (rhs);
	      rhs = nrhs;
	    }
	  return rhs;
	}

      return ocp_convert (type, rhs, CONV_OLD_CONVERT,
			  flags | LOOKUP_NO_CONVERSION);
    }

  if (type == TREE_TYPE (rhs))
    {
      if (TREE_READONLY_DECL_P (rhs))
	rhs = decl_constant_value (rhs);
      return rhs;
    }

  return convert_for_assignment (type, rhs, errtype, fndecl, parmnum);
}

/* Expand an ASM statement with operands, handling output operands
   that are not variables or INDIRECT_REFS by transforming such
   cases into cases that expand_asm_operands can handle.

   Arguments are same as for expand_asm_operands.

   We don't do default conversions on all inputs, because it can screw
   up operands that are expected to be in memory.  */

void
c_expand_asm_operands (string, outputs, inputs, clobbers, vol, filename, line)
     tree string, outputs, inputs, clobbers;
     int vol;
     char *filename;
     int line;
{
  int noutputs = list_length (outputs);
  register int i;
  /* o[I] is the place that output number I should be written.  */
  register tree *o = (tree *) alloca (noutputs * sizeof (tree));
  register tree tail;

  /* Record the contents of OUTPUTS before it is modified.  */
  for (i = 0, tail = outputs; tail; tail = TREE_CHAIN (tail), i++)
    o[i] = TREE_VALUE (tail);

  /* Generate the ASM_OPERANDS insn;
     store into the TREE_VALUEs of OUTPUTS some trees for
     where the values were actually stored.  */
  expand_asm_operands (string, outputs, inputs, clobbers, vol, filename, line);

  /* Copy all the intermediate outputs into the specified outputs.  */
  for (i = 0, tail = outputs; tail; tail = TREE_CHAIN (tail), i++)
    {
      if (o[i] != TREE_VALUE (tail))
	{
	  expand_expr (build_modify_expr (o[i], NOP_EXPR, TREE_VALUE (tail)),
		       const0_rtx, VOIDmode, EXPAND_NORMAL);
	  free_temp_slots ();
	}
      /* Detect modification of read-only values.
	 (Otherwise done by build_modify_expr.)  */
      else
	{
	  tree type = TREE_TYPE (o[i]);
	  if (TYPE_READONLY (type)
	      || ((TREE_CODE (type) == RECORD_TYPE
		   || TREE_CODE (type) == UNION_TYPE)
		  && C_TYPE_FIELDS_READONLY (type)))
	    readonly_error (o[i], "modification by `asm'", 1);
	}
    }

  /* Those MODIFY_EXPRs could do autoincrements.  */
  emit_queue ();
}

/* Expand a C `return' statement.
   RETVAL is the expression for what to return,
   or a null pointer for `return;' with no value.

   C++: upon seeing a `return', we must call destructors on all
   variables in scope which had constructors called on them.
   This means that if in a destructor, the base class destructors
   must be called before returning.

   The RETURN statement in C++ has initialization semantics.  */

void
c_expand_return (retval)
     tree retval;
{
  extern struct nesting *cond_stack, *loop_stack, *case_stack;
  extern tree dtor_label, ctor_label;
  tree result = DECL_RESULT (current_function_decl);
  tree valtype = TREE_TYPE (result);

  if (TREE_THIS_VOLATILE (current_function_decl))
    warning ("function declared `noreturn' has a `return' statement");

  if (retval == error_mark_node)
    {
      current_function_returns_null = 1;
      return;
    }

  if (processing_template_decl)
    {
      add_tree (build_min_nt (RETURN_STMT, retval));
      return;
    }

  if (retval == NULL_TREE)
    {
      /* A non-named return value does not count.  */

      /* Can't just return from a destructor.  */
      if (dtor_label)
	{
	  expand_goto (dtor_label);
	  return;
	}

      if (DECL_CONSTRUCTOR_P (current_function_decl))
	retval = current_class_ptr;
      else if (DECL_NAME (result) != NULL_TREE
	       && TREE_CODE (valtype) != VOID_TYPE)
	retval = result;
      else
	{
	  current_function_returns_null = 1;

	  if (valtype != NULL_TREE && TREE_CODE (valtype) != VOID_TYPE)
	    {
	      if (DECL_NAME (DECL_RESULT (current_function_decl)) == NULL_TREE)
		{
		  pedwarn ("`return' with no value, in function returning non-void");
		  /* Clear this, so finish_function won't say that we
		     reach the end of a non-void function (which we don't,
		     we gave a return!).  */
		  current_function_returns_null = 0;
		}
	    }

	  expand_null_return ();
	  return;
	}
    }
  else if (DECL_CONSTRUCTOR_P (current_function_decl)
	   && retval != current_class_ptr)
    {
      if (flag_this_is_variable)
	error ("return from a constructor: use `this = ...' instead");
      else
	error ("return from a constructor");
      retval = current_class_ptr;
    }

  /* Effective C++ rule 15.  See also start_function.  */
  if (warn_ecpp
      && DECL_NAME (current_function_decl) == ansi_opname[(int) MODIFY_EXPR]
      && retval != current_class_ref)
    cp_warning ("`operator=' should return a reference to `*this'");

  if (valtype == NULL_TREE || TREE_CODE (valtype) == VOID_TYPE)
    {
      current_function_returns_null = 1;
      if ((pedantic && ! DECL_ARTIFICIAL (current_function_decl))
	  || TREE_CODE (TREE_TYPE (retval)) != VOID_TYPE)
	pedwarn ("`return' with a value, in function returning void");
      expand_return (retval);
      return;
    }
  
  /* Now deal with possible C++ hair:
     (1) Compute the return value.
     (2) If there are aggregate values with destructors which
     must be cleaned up, clean them (taking care
     not to clobber the return value).
     (3) If an X(X&) constructor is defined, the return
     value must be returned via that.  */

  if (retval == result
      || DECL_CONSTRUCTOR_P (current_function_decl))
    /* It's already done for us.  */;
  else if (TREE_TYPE (retval) == void_type_node)
    {
      pedwarn ("return of void value in function returning non-void");
      expand_expr_stmt (retval);
      retval = 0;
    }
  else
    {
      retval = convert_for_initialization
	(NULL_TREE, valtype, retval, LOOKUP_NORMAL|LOOKUP_ONLYCONVERTING,
	 "return", NULL_TREE, 0);

      if (retval == error_mark_node)
	{
	  /* Avoid warning about control reaching end of function.  */
	  expand_null_return ();
	  return;
	}

      /* We can't initialize a register from a NEW_EXPR.  */
      else if (! current_function_returns_struct
	       && TREE_CODE (retval) == TARGET_EXPR
	       && TREE_CODE (TREE_OPERAND (retval, 1)) == NEW_EXPR)
	retval = build (COMPOUND_EXPR, TREE_TYPE (retval), retval,
			TREE_OPERAND (retval, 0));

      /* Add some useful error checking for C++.  */
      else if (TREE_CODE (valtype) == REFERENCE_TYPE)
	{
	  tree whats_returned;

	  /* Sort through common things to see what it is
	     we are returning.  */
	  whats_returned = retval;
	  if (TREE_CODE (whats_returned) == COMPOUND_EXPR)
	    {
	      whats_returned = TREE_OPERAND (whats_returned, 1);
	      if (TREE_CODE (whats_returned) == ADDR_EXPR)
		whats_returned = TREE_OPERAND (whats_returned, 0);
	    }
	  while (TREE_CODE (whats_returned) == CONVERT_EXPR
		 || TREE_CODE (whats_returned) == NOP_EXPR)
	    whats_returned = TREE_OPERAND (whats_returned, 0);
	  if (TREE_CODE (whats_returned) == ADDR_EXPR)
	    {
	      whats_returned = TREE_OPERAND (whats_returned, 0);
	      while (TREE_CODE (whats_returned) == NEW_EXPR
		     || TREE_CODE (whats_returned) == TARGET_EXPR)
		{
		  /* Get the target.  */
		  whats_returned = TREE_OPERAND (whats_returned, 0);
		  warning ("returning reference to temporary");
		}
	    }

	  if (TREE_CODE (whats_returned) == VAR_DECL && DECL_NAME (whats_returned))
	    {
	      if (TEMP_NAME_P (DECL_NAME (whats_returned)))
		warning ("reference to non-lvalue returned");
	      else if (TREE_CODE (TREE_TYPE (whats_returned)) != REFERENCE_TYPE
		       && ! TREE_STATIC (whats_returned)
		       && IDENTIFIER_LOCAL_VALUE (DECL_NAME (whats_returned))
		       && !TREE_PUBLIC (whats_returned))
		cp_warning_at ("reference to local variable `%D' returned", whats_returned);
	    }
	}
      else if (TREE_CODE (retval) == ADDR_EXPR)
	{
	  tree whats_returned = TREE_OPERAND (retval, 0);

	  if (TREE_CODE (whats_returned) == VAR_DECL
	      && DECL_NAME (whats_returned)
	      && IDENTIFIER_LOCAL_VALUE (DECL_NAME (whats_returned))
	      && !TREE_STATIC (whats_returned)
	      && !TREE_PUBLIC (whats_returned))
	    cp_warning_at ("address of local variable `%D' returned", whats_returned);
	}
    }

  if (retval != NULL_TREE
      && TREE_CODE_CLASS (TREE_CODE (retval)) == 'd'
      && cond_stack == 0 && loop_stack == 0 && case_stack == 0)
    current_function_return_value = retval;

  if (ctor_label && TREE_CODE (ctor_label) != ERROR_MARK)
    {
      /* Here RETVAL is CURRENT_CLASS_PTR, so there's nothing to do.  */
      expand_goto (ctor_label);
    }

  if (retval && retval != result)
    {
      result = build (INIT_EXPR, TREE_TYPE (result), result, retval);
      TREE_SIDE_EFFECTS (result) = 1;
    }

  expand_start_target_temps ();

  expand_return (result);

  expand_end_target_temps ();

  current_function_returns_value = 1;
}

/* Start a C switch statement, testing expression EXP.
   Return EXP if it is valid, an error node otherwise.  */

tree
c_expand_start_case (exp)
     tree exp;
{
  tree type;
  register enum tree_code code;

  /* Convert from references, etc.  */
  exp = default_conversion (exp);
  type = TREE_TYPE (exp);
  code = TREE_CODE (type);

  if (IS_AGGR_TYPE_CODE (code))
    exp = build_type_conversion (CONVERT_EXPR, integer_type_node, exp, 1);

  if (exp == NULL_TREE)
    {
      error ("switch quantity not an integer");
      exp = error_mark_node;
    }
  type = TREE_TYPE (exp);
  code = TREE_CODE (type);

  if (code != INTEGER_TYPE && code != ENUMERAL_TYPE && code != ERROR_MARK)
    {
      error ("switch quantity not an integer");
      exp = error_mark_node;
    }
  else
    {
      tree idx;

      exp = default_conversion (exp);
      type = TREE_TYPE (exp);
      idx = get_unwidened (exp, 0);
      /* We can't strip a conversion from a signed type to an unsigned,
	 because if we did, int_fits_type_p would do the wrong thing
	 when checking case values for being in range,
	 and it's too hard to do the right thing.  */
      if (TREE_UNSIGNED (TREE_TYPE (exp))
	  == TREE_UNSIGNED (TREE_TYPE (idx)))
	exp = idx;
    }

  expand_start_case
    (1, fold (build1 (CLEANUP_POINT_EXPR, TREE_TYPE (exp), exp)),
     type, "switch statement");

  return exp;
}

/* CONSTP remembers whether or not all the intervening pointers in the `to'
   type have been const.  */

static int
comp_ptr_ttypes_real (to, from, constp)
     tree to, from;
     int constp;
{
  for (; ; to = TREE_TYPE (to), from = TREE_TYPE (from))
    {
      if (TREE_CODE (to) != TREE_CODE (from))
	return 0;

      if (TREE_CODE (from) == OFFSET_TYPE
	  && comptypes (TYPE_OFFSET_BASETYPE (from),
			TYPE_OFFSET_BASETYPE (to), 1))
	  continue;

      /* Const and volatile mean something different for function types,
	 so the usual checks are not appropriate.  */
      if (TREE_CODE (to) != FUNCTION_TYPE && TREE_CODE (to) != METHOD_TYPE)
	{
	  if (TYPE_READONLY (from) > TYPE_READONLY (to)
	      || TYPE_VOLATILE (from) > TYPE_VOLATILE (to))
	    return 0;

	  if (! constp
	      && (TYPE_READONLY (to) > TYPE_READONLY (from)
		  || TYPE_VOLATILE (to) > TYPE_READONLY (from)))
	    return 0;
	  constp &= TYPE_READONLY (to);
	}

      if (TREE_CODE (to) != POINTER_TYPE)
	return comptypes (TYPE_MAIN_VARIANT (to), TYPE_MAIN_VARIANT (from), 1);
    }
}

/* When comparing, say, char ** to char const **, this function takes the
   'char *' and 'char const *'.  Do not pass non-pointer types to this
   function.  */

int
comp_ptr_ttypes (to, from)
     tree to, from;
{
  return comp_ptr_ttypes_real (to, from, 1);
}

/* Returns 1 if to and from are (possibly multi-level) pointers to the same
   type or inheritance-related types, regardless of cv-quals.  */

int
ptr_reasonably_similar (to, from)
     tree to, from;
{
  for (; ; to = TREE_TYPE (to), from = TREE_TYPE (from))
    {
      if (TREE_CODE (to) != TREE_CODE (from))
	return 0;

      if (TREE_CODE (from) == OFFSET_TYPE
	  && comptypes (TYPE_OFFSET_BASETYPE (to),
			TYPE_OFFSET_BASETYPE (from), -1))
	continue;

      if (TREE_CODE (to) != POINTER_TYPE)
	return comptypes
	  (TYPE_MAIN_VARIANT (to), TYPE_MAIN_VARIANT (from), -1);
    }
}

/* Like comp_ptr_ttypes, for const_cast.  */

static int
comp_ptr_ttypes_const (to, from)
     tree to, from;
{
  for (; ; to = TREE_TYPE (to), from = TREE_TYPE (from))
    {
      if (TREE_CODE (to) != TREE_CODE (from))
	return 0;

      if (TREE_CODE (from) == OFFSET_TYPE
	  && comptypes (TYPE_OFFSET_BASETYPE (from),
			TYPE_OFFSET_BASETYPE (to), 1))
	  continue;

      if (TREE_CODE (to) != POINTER_TYPE)
	return comptypes (TYPE_MAIN_VARIANT (to), TYPE_MAIN_VARIANT (from), 1);
    }
}

/* Like comp_ptr_ttypes, for reinterpret_cast.  */

static int
comp_ptr_ttypes_reinterpret (to, from)
     tree to, from;
{
  int constp = 1;

  for (; ; to = TREE_TYPE (to), from = TREE_TYPE (from))
    {
      if (TREE_CODE (from) == OFFSET_TYPE)
	from = TREE_TYPE (from);
      if (TREE_CODE (to) == OFFSET_TYPE)
	to = TREE_TYPE (to);

      if (TREE_CODE (to) != TREE_CODE (from))
	return 1;

      /* Const and volatile mean something different for function types,
	 so the usual checks are not appropriate.  */
      if (TREE_CODE (to) != FUNCTION_TYPE && TREE_CODE (to) != METHOD_TYPE)
	{
	  if (TYPE_READONLY (from) > TYPE_READONLY (to)
	      || TYPE_VOLATILE (from) > TYPE_VOLATILE (to))
	    return 0;

	  if (! constp
	      && (TYPE_READONLY (to) > TYPE_READONLY (from)
		  || TYPE_VOLATILE (to) > TYPE_READONLY (from)))
	    return 0;
	  constp &= TYPE_READONLY (to);
	}

      if (TREE_CODE (to) != POINTER_TYPE)
	return 1;
    }
}
