/* Build expressions with type checking for C compiler.
   Copyright (C) 1987, 88, 91-97, 1998 Free Software Foundation, Inc.

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


/* This file is part of the C front end.
   It contains routines to build C expressions given their operands,
   including computing the types of the result, C-specific error checks,
   and some optimization.

   There are also routines to build RETURN_STMT nodes and CASE_STMT nodes,
   and to process initializations in declarations (since they work
   like a strange sort of assignment).  */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "c-tree.h"
#include "tm_p.h"
#include "flags.h"
#include "output.h"
#include "rtl.h"
#include "expr.h"
#include "toplev.h"
#include "intl.h"
#include "defaults.h"

/* Nonzero if we've already printed a "missing braces around initializer"
   message within this initializer.  */
static int missing_braces_mentioned;

static tree qualify_type		PROTO((tree, tree));
static int comp_target_types		PROTO((tree, tree));
static int function_types_compatible_p	PROTO((tree, tree));
static int type_lists_compatible_p	PROTO((tree, tree));
static tree decl_constant_value		PROTO((tree));
static tree lookup_field		PROTO((tree, tree, tree *));
static tree convert_arguments		PROTO((tree, tree, tree, tree));
static tree pointer_int_sum		PROTO((enum tree_code, tree, tree));
static tree pointer_diff		PROTO((tree, tree));
static tree unary_complex_lvalue	PROTO((enum tree_code, tree));
static void pedantic_lvalue_warning	PROTO((enum tree_code));
static tree internal_build_compound_expr PROTO((tree, int));
static tree convert_for_assignment	PROTO((tree, tree, const char *, tree,
					       tree, int));
static void warn_for_assignment		PROTO((const char *, const char *,
					       tree, int));
static tree valid_compound_expr_initializer PROTO((tree, tree));
static void push_string			PROTO((const char *));
static void push_member_name		PROTO((tree));
static void push_array_bounds		PROTO((int));
static int spelling_length		PROTO((void));
static char *print_spelling		PROTO((char *));
static void warning_init		PROTO((const char *));
static tree digest_init			PROTO((tree, tree, int, int));
static void check_init_type_bitfields	PROTO((tree));
static void output_init_element		PROTO((tree, tree, tree, int));
static void output_pending_init_elements PROTO((int));
static void add_pending_init		PROTO((tree, tree));
static int pending_init_member		PROTO((tree));

/* Do `exp = require_complete_type (exp);' to make sure exp
   does not have an incomplete type.  (That includes void types.)  */

tree
require_complete_type (value)
     tree value;
{
  tree type = TREE_TYPE (value);

  if (TREE_CODE (value) == ERROR_MARK)
    return error_mark_node;

  /* First, detect a valid value with a complete type.  */
  if (TYPE_SIZE (type) != 0
      && type != void_type_node)
    return value;

  incomplete_type_error (value, type);
  return error_mark_node;
}

/* Print an error message for invalid use of an incomplete type.
   VALUE is the expression that was used (or 0 if that isn't known)
   and TYPE is the type that was invalid.  */

void
incomplete_type_error (value, type)
     tree value;
     tree type;
{
  const char *type_code_string;

  /* Avoid duplicate error message.  */
  if (TREE_CODE (type) == ERROR_MARK)
    return;

  if (value != 0 && (TREE_CODE (value) == VAR_DECL
		     || TREE_CODE (value) == PARM_DECL))
    error ("`%s' has an incomplete type",
	   IDENTIFIER_POINTER (DECL_NAME (value)));
  else
    {
    retry:
      /* We must print an error message.  Be clever about what it says.  */

      switch (TREE_CODE (type))
	{
	case RECORD_TYPE:
	  type_code_string = "struct";
	  break;

	case UNION_TYPE:
	  type_code_string = "union";
	  break;

	case ENUMERAL_TYPE:
	  type_code_string = "enum";
	  break;

	case VOID_TYPE:
	  error ("invalid use of void expression");
	  return;

	case ARRAY_TYPE:
	  if (TYPE_DOMAIN (type))
	    {
	      type = TREE_TYPE (type);
	      goto retry;
	    }
	  error ("invalid use of array with unspecified bounds");
	  return;

	default:
	  abort ();
	}

      if (TREE_CODE (TYPE_NAME (type)) == IDENTIFIER_NODE)
	error ("invalid use of undefined type `%s %s'",
	       type_code_string, IDENTIFIER_POINTER (TYPE_NAME (type)));
      else
	/* If this type has a typedef-name, the TYPE_NAME is a TYPE_DECL.  */
	error ("invalid use of incomplete typedef `%s'",
	       IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type))));
    }
}

/* Return a variant of TYPE which has all the type qualifiers of LIKE
   as well as those of TYPE.  */

static tree
qualify_type (type, like)
     tree type, like;
{
  return c_build_qualified_type (type, 
				 TYPE_QUALS (type) | TYPE_QUALS (like));
}

/* Return the common type of two types.
   We assume that comptypes has already been done and returned 1;
   if that isn't so, this may crash.  In particular, we assume that qualifiers
   match.

   This is the type for the result of most arithmetic operations
   if the operands have the given two types.  */

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

  /* Merge the attributes.  */
  attributes = merge_machine_type_attributes (t1, t2);

  /* Treat an enum type as the unsigned integer type of the same width.  */

  if (TREE_CODE (t1) == ENUMERAL_TYPE)
    t1 = type_for_size (TYPE_PRECISION (t1), 1);
  if (TREE_CODE (t2) == ENUMERAL_TYPE)
    t2 = type_for_size (TYPE_PRECISION (t2), 1);

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

      /* Likewise, prefer long double to double even if same size.  */
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
      /* For two pointers, do this recursively on the target type,
	 and combine the qualifiers of the two types' targets.  */
      /* This code was turned off; I don't know why.
	 But ANSI C specifies doing this with the qualifiers.
	 So I turned it on again.  */
      {
	tree pointed_to_1 = TREE_TYPE (t1);
	tree pointed_to_2 = TREE_TYPE (t2);
	tree target = common_type (TYPE_MAIN_VARIANT (pointed_to_1),
				   TYPE_MAIN_VARIANT (pointed_to_2));
	t1 = build_pointer_type (c_build_qualified_type 
				 (target, 
				  TYPE_QUALS (pointed_to_1) | 
				  TYPE_QUALS (pointed_to_2)));
	return build_type_attribute_variant (t1, attributes);
      }
#if 0
      t1 = build_pointer_type (common_type (TREE_TYPE (t1), TREE_TYPE (t2)));
      return build_type_attribute_variant (t1, attributes);
#endif

    case ARRAY_TYPE:
      {
	tree elt = common_type (TREE_TYPE (t1), TREE_TYPE (t2));
	/* Save space: see if the result is identical to one of the args.  */
	if (elt == TREE_TYPE (t1) && TYPE_DOMAIN (t1))
	  return build_type_attribute_variant (t1, attributes);
	if (elt == TREE_TYPE (t2) && TYPE_DOMAIN (t2))
	  return build_type_attribute_variant (t2, attributes);
	/* Merge the element types, and have a size if either arg has one.  */
	t1 = build_array_type (elt, TYPE_DOMAIN (TYPE_DOMAIN (t1) ? t1 : t2));
	return build_type_attribute_variant (t1, attributes);
      }

    case FUNCTION_TYPE:
      /* Function types: prefer the one that specified arg types.
	 If both do, merge the arg types.  Also merge the return types.  */
      {
	tree valtype = common_type (TREE_TYPE (t1), TREE_TYPE (t2));
	tree p1 = TYPE_ARG_TYPES (t1);
	tree p2 = TYPE_ARG_TYPES (t2);
	int len;
	tree newargs, n;
	int i;

	/* Save space: see if the result is identical to one of the args.  */
	if (valtype == TREE_TYPE (t1) && ! TYPE_ARG_TYPES (t2))
	  return build_type_attribute_variant (t1, attributes);
	if (valtype == TREE_TYPE (t2) && ! TYPE_ARG_TYPES (t1))
	  return build_type_attribute_variant (t2, attributes);

	/* Simple way if one arg fails to specify argument types.  */
	if (TYPE_ARG_TYPES (t1) == 0)
	 {
	   t1 = build_function_type (valtype, TYPE_ARG_TYPES (t2));
	   return build_type_attribute_variant (t1, attributes);
	 }
	if (TYPE_ARG_TYPES (t2) == 0)
	 {
	   t1 = build_function_type (valtype, TYPE_ARG_TYPES (t1));
	   return build_type_attribute_variant (t1, attributes);
	 }

	/* If both args specify argument types, we must merge the two
	   lists, argument by argument.  */

	len = list_length (p1);
	newargs = 0;

	for (i = 0; i < len; i++)
	  newargs = tree_cons (NULL_TREE, NULL_TREE, newargs);

	n = newargs;

	for (; p1;
	     p1 = TREE_CHAIN (p1), p2 = TREE_CHAIN (p2), n = TREE_CHAIN (n))
	  {
	    /* A null type means arg type is not specified.
	       Take whatever the other function type has.  */
	    if (TREE_VALUE (p1) == 0)
	      {
		TREE_VALUE (n) = TREE_VALUE (p2);
		goto parm_done;
	      }
	    if (TREE_VALUE (p2) == 0)
	      {
		TREE_VALUE (n) = TREE_VALUE (p1);
		goto parm_done;
	      }
	      
	    /* Given  wait (union {union wait *u; int *i} *)
	       and  wait (union wait *),
	       prefer  union wait *  as type of parm.  */
	    if (TREE_CODE (TREE_VALUE (p1)) == UNION_TYPE
		&& TREE_VALUE (p1) != TREE_VALUE (p2))
	      {
		tree memb;
		for (memb = TYPE_FIELDS (TREE_VALUE (p1));
		     memb; memb = TREE_CHAIN (memb))
		  if (comptypes (TREE_TYPE (memb), TREE_VALUE (p2)))
		    {
		      TREE_VALUE (n) = TREE_VALUE (p2);
		      if (pedantic)
			pedwarn ("function types not truly compatible in ANSI C");
		      goto parm_done;
		    }
	      }
	    if (TREE_CODE (TREE_VALUE (p2)) == UNION_TYPE
		&& TREE_VALUE (p2) != TREE_VALUE (p1))
	      {
		tree memb;
		for (memb = TYPE_FIELDS (TREE_VALUE (p2));
		     memb; memb = TREE_CHAIN (memb))
		  if (comptypes (TREE_TYPE (memb), TREE_VALUE (p1)))
		    {
		      TREE_VALUE (n) = TREE_VALUE (p1);
		      if (pedantic)
			pedwarn ("function types not truly compatible in ANSI C");
		      goto parm_done;
		    }
	      }
	    TREE_VALUE (n) = common_type (TREE_VALUE (p1), TREE_VALUE (p2));
	  parm_done: ;
	  }

	t1 = build_function_type (valtype, newargs);
	/* ... falls through ...  */
      }

    default:
      return build_type_attribute_variant (t1, attributes);
    }

}

/* Return 1 if TYPE1 and TYPE2 are compatible types for assignment
   or various other operations.  Return 2 if they are compatible
   but a warning may be needed if you use them together.  */

int
comptypes (type1, type2)
     tree type1, type2;
{
  register tree t1 = type1;
  register tree t2 = type2;
  int attrval, val;

  /* Suppress errors caused by previously reported errors.  */

  if (t1 == t2 || !t1 || !t2
      || TREE_CODE (t1) == ERROR_MARK || TREE_CODE (t2) == ERROR_MARK)
    return 1;

  /* Treat an enum type as the integer type of the same width and 
     signedness.  */

  if (TREE_CODE (t1) == ENUMERAL_TYPE)
    t1 = type_for_size (TYPE_PRECISION (t1), TREE_UNSIGNED (t1));
  if (TREE_CODE (t2) == ENUMERAL_TYPE)
    t2 = type_for_size (TYPE_PRECISION (t2), TREE_UNSIGNED (t2));

  if (t1 == t2)
    return 1;

  /* Different classes of types can't be compatible.  */

  if (TREE_CODE (t1) != TREE_CODE (t2)) return 0;

  /* Qualifiers must match.  */

  if (TYPE_QUALS (t1) != TYPE_QUALS (t2))
    return 0;

  /* Allow for two different type nodes which have essentially the same
     definition.  Note that we already checked for equality of the type
     qualifiers (just above).  */

  if (TYPE_MAIN_VARIANT (t1) == TYPE_MAIN_VARIANT (t2))
    return 1;

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
    case POINTER_TYPE:
      val = (TREE_TYPE (t1) == TREE_TYPE (t2)
	      ? 1 : comptypes (TREE_TYPE (t1), TREE_TYPE (t2)));
      break;

    case FUNCTION_TYPE:
      val = function_types_compatible_p (t1, t2);
      break;

    case ARRAY_TYPE:
      {
	tree d1 = TYPE_DOMAIN (t1);
	tree d2 = TYPE_DOMAIN (t2);
	val = 1;

	/* Target types must match incl. qualifiers.  */
	if (TREE_TYPE (t1) != TREE_TYPE (t2)
	    && 0 == (val = comptypes (TREE_TYPE (t1), TREE_TYPE (t2))))
	  return 0;

	/* Sizes must match unless one is missing or variable.  */
	if (d1 == 0 || d2 == 0 || d1 == d2
	    || TREE_CODE (TYPE_MIN_VALUE (d1)) != INTEGER_CST
	    || TREE_CODE (TYPE_MIN_VALUE (d2)) != INTEGER_CST
	    || TREE_CODE (TYPE_MAX_VALUE (d1)) != INTEGER_CST
	    || TREE_CODE (TYPE_MAX_VALUE (d2)) != INTEGER_CST)
	  break;

	if (! ((TREE_INT_CST_LOW (TYPE_MIN_VALUE (d1))
		  == TREE_INT_CST_LOW (TYPE_MIN_VALUE (d2)))
		 && (TREE_INT_CST_HIGH (TYPE_MIN_VALUE (d1))
		     == TREE_INT_CST_HIGH (TYPE_MIN_VALUE (d2)))
		 && (TREE_INT_CST_LOW (TYPE_MAX_VALUE (d1))
		     == TREE_INT_CST_LOW (TYPE_MAX_VALUE (d2)))
		 && (TREE_INT_CST_HIGH (TYPE_MAX_VALUE (d1))
		     == TREE_INT_CST_HIGH (TYPE_MAX_VALUE (d2)))))
	   val = 0;
        break;
      }

    case RECORD_TYPE:
      if (maybe_objc_comptypes (t1, t2, 0) == 1)
	val = 1;
      break;

    default:
      break;
    }
  return attrval == 2 && val == 1 ? 2 : val;
}

/* Return 1 if TTL and TTR are pointers to types that are equivalent,
   ignoring their qualifiers.  */

static int
comp_target_types (ttl, ttr)
     tree ttl, ttr;
{
  int val;

  /* Give maybe_objc_comptypes a crack at letting these types through.  */
  if ((val = maybe_objc_comptypes (ttl, ttr, 1)) >= 0)
    return val;

  val = comptypes (TYPE_MAIN_VARIANT (TREE_TYPE (ttl)),
		   TYPE_MAIN_VARIANT (TREE_TYPE (ttr)));

  if (val == 2 && pedantic)
    pedwarn ("types are not quite compatible");
  return val;
}

/* Subroutines of `comptypes'.  */

/* Return 1 if two function types F1 and F2 are compatible.
   If either type specifies no argument types,
   the other must specify a fixed number of self-promoting arg types.
   Otherwise, if one type specifies only the number of arguments, 
   the other must specify that number of self-promoting arg types.
   Otherwise, the argument types must match.  */

static int
function_types_compatible_p (f1, f2)
     tree f1, f2;
{
  tree args1, args2;
  /* 1 if no need for warning yet, 2 if warning cause has been seen.  */
  int val = 1;
  int val1;

  if (!(TREE_TYPE (f1) == TREE_TYPE (f2)
	|| (val = comptypes (TREE_TYPE (f1), TREE_TYPE (f2)))))
    return 0;

  args1 = TYPE_ARG_TYPES (f1);
  args2 = TYPE_ARG_TYPES (f2);

  /* An unspecified parmlist matches any specified parmlist
     whose argument types don't need default promotions.  */

  if (args1 == 0)
    {
      if (!self_promoting_args_p (args2))
	return 0;
      /* If one of these types comes from a non-prototype fn definition,
	 compare that with the other type's arglist.
	 If they don't match, ask for a warning (but no error).  */
      if (TYPE_ACTUAL_ARG_TYPES (f1)
	  && 1 != type_lists_compatible_p (args2, TYPE_ACTUAL_ARG_TYPES (f1)))
	val = 2;
      return val;
    }
  if (args2 == 0)
    {
      if (!self_promoting_args_p (args1))
	return 0;
      if (TYPE_ACTUAL_ARG_TYPES (f2)
	  && 1 != type_lists_compatible_p (args1, TYPE_ACTUAL_ARG_TYPES (f2)))
	val = 2;
      return val;
    }

  /* Both types have argument lists: compare them and propagate results.  */
  val1 = type_lists_compatible_p (args1, args2);
  return val1 != 1 ? val1 : val;
}

/* Check two lists of types for compatibility,
   returning 0 for incompatible, 1 for compatible,
   or 2 for compatible with warning.  */

static int
type_lists_compatible_p (args1, args2)
     tree args1, args2;
{
  /* 1 if no need for warning yet, 2 if warning cause has been seen.  */
  int val = 1;
  int newval = 0;

  while (1)
    {
      if (args1 == 0 && args2 == 0)
	return val;
      /* If one list is shorter than the other,
	 they fail to match.  */
      if (args1 == 0 || args2 == 0)
	return 0;
      /* A null pointer instead of a type
	 means there is supposed to be an argument
	 but nothing is specified about what type it has.
	 So match anything that self-promotes.  */
      if (TREE_VALUE (args1) == 0)
	{
	  if (simple_type_promotes_to (TREE_VALUE (args2)) != NULL_TREE)
	    return 0;
	}
      else if (TREE_VALUE (args2) == 0)
	{
	  if (simple_type_promotes_to (TREE_VALUE (args1)) != NULL_TREE)
	    return 0;
	}
      else if (! (newval = comptypes (TREE_VALUE (args1), TREE_VALUE (args2))))
	{
	  /* Allow  wait (union {union wait *u; int *i} *)
	     and  wait (union wait *)  to be compatible.  */
	  if (TREE_CODE (TREE_VALUE (args1)) == UNION_TYPE
	      && (TYPE_NAME (TREE_VALUE (args1)) == 0
		  || TYPE_TRANSPARENT_UNION (TREE_VALUE (args1)))
	      && TREE_CODE (TYPE_SIZE (TREE_VALUE (args1))) == INTEGER_CST
	      && tree_int_cst_equal (TYPE_SIZE (TREE_VALUE (args1)),
				     TYPE_SIZE (TREE_VALUE (args2))))
	    {
	      tree memb;
	      for (memb = TYPE_FIELDS (TREE_VALUE (args1));
		   memb; memb = TREE_CHAIN (memb))
		if (comptypes (TREE_TYPE (memb), TREE_VALUE (args2)))
		  break;
	      if (memb == 0)
		return 0;
	    }
	  else if (TREE_CODE (TREE_VALUE (args2)) == UNION_TYPE
		   && (TYPE_NAME (TREE_VALUE (args2)) == 0
		       || TYPE_TRANSPARENT_UNION (TREE_VALUE (args2)))
		   && TREE_CODE (TYPE_SIZE (TREE_VALUE (args2))) == INTEGER_CST
		   && tree_int_cst_equal (TYPE_SIZE (TREE_VALUE (args2)),
					  TYPE_SIZE (TREE_VALUE (args1))))
	    {
	      tree memb;
	      for (memb = TYPE_FIELDS (TREE_VALUE (args2));
		   memb; memb = TREE_CHAIN (memb))
		if (comptypes (TREE_TYPE (memb), TREE_VALUE (args1)))
		  break;
	      if (memb == 0)
		return 0;
	    }
	  else
	    return 0;
	}

      /* comptypes said ok, but record if it said to warn.  */
      if (newval > val)
	val = newval;

      args1 = TREE_CHAIN (args1);
      args2 = TREE_CHAIN (args2);
    }
}

/* Compute the value of the `sizeof' operator.  */

tree
c_sizeof (type)
     tree type;
{
  enum tree_code code = TREE_CODE (type);
  tree t;

  if (code == FUNCTION_TYPE)
    {
      if (pedantic || warn_pointer_arith)
	pedwarn ("sizeof applied to a function type");
      return size_int (1);
    }
  if (code == VOID_TYPE)
    {
      if (pedantic || warn_pointer_arith)
	pedwarn ("sizeof applied to a void type");
      return size_int (1);
    }
  if (code == ERROR_MARK)
    return size_int (1);
  if (TYPE_SIZE (type) == 0)
    {
      error ("sizeof applied to an incomplete type");
      return size_int (0);
    }

  /* Convert in case a char is more than one unit.  */
  t = size_binop (CEIL_DIV_EXPR, TYPE_SIZE (type), 
		  size_int (TYPE_PRECISION (char_type_node)));
  t = convert (sizetype, t);
  /* size_binop does not put the constant in range, so do it now.  */
  if (TREE_CODE (t) == INTEGER_CST && force_fit_type (t, 0))
    TREE_CONSTANT_OVERFLOW (t) = TREE_OVERFLOW (t) = 1;
  return t;
}

tree
c_sizeof_nowarn (type)
     tree type;
{
  enum tree_code code = TREE_CODE (type);
  tree t;

  if (code == FUNCTION_TYPE
      || code == VOID_TYPE
      || code == ERROR_MARK)
    return size_int (1);
  if (TYPE_SIZE (type) == 0)
    return size_int (0);

  /* Convert in case a char is more than one unit.  */
  t = size_binop (CEIL_DIV_EXPR, TYPE_SIZE (type), 
		  size_int (TYPE_PRECISION (char_type_node)));
  t = convert (sizetype, t);
  force_fit_type (t, 0);
  return t;
}

/* Compute the size to increment a pointer by.  */

tree
c_size_in_bytes (type)
     tree type;
{
  enum tree_code code = TREE_CODE (type);
  tree t;

  if (code == FUNCTION_TYPE)
    return size_int (1);
  if (code == VOID_TYPE)
    return size_int (1);
  if (code == ERROR_MARK)
    return size_int (1);
  if (TYPE_SIZE (type) == 0)
    {
      error ("arithmetic on pointer to an incomplete type");
      return size_int (1);
    }

  /* Convert in case a char is more than one unit.  */
  t = size_binop (CEIL_DIV_EXPR, TYPE_SIZE (type), 
		     size_int (BITS_PER_UNIT));
  t = convert (sizetype, t);
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

  if (code == FUNCTION_TYPE)
    return size_int (FUNCTION_BOUNDARY / BITS_PER_UNIT);

  if (code == VOID_TYPE || code == ERROR_MARK)
    return size_int (1);

  return size_int (TYPE_ALIGN (type) / BITS_PER_UNIT);
}

/* Implement the __alignof keyword: Return the minimum required
   alignment of EXPR, measured in bytes.  For VAR_DECL's and
   FIELD_DECL's return DECL_ALIGN (which can be set from an
   "aligned" __attribute__ specification).  */

tree
c_alignof_expr (expr)
     tree expr;
{
  if (TREE_CODE (expr) == VAR_DECL)
    return size_int (DECL_ALIGN (expr) / BITS_PER_UNIT);
 
  if (TREE_CODE (expr) == COMPONENT_REF
      && DECL_C_BIT_FIELD (TREE_OPERAND (expr, 1)))
    {
      error ("`__alignof' applied to a bit-field");
      return size_int (1);
    }
  else if (TREE_CODE (expr) == COMPONENT_REF
      && TREE_CODE (TREE_OPERAND (expr, 1)) == FIELD_DECL)
    return size_int (DECL_ALIGN (TREE_OPERAND (expr, 1)) / BITS_PER_UNIT);
 
  if (TREE_CODE (expr) == INDIRECT_REF)
    {
      tree t = TREE_OPERAND (expr, 0);
      tree best = t;
      int bestalign = TYPE_ALIGN (TREE_TYPE (TREE_TYPE (t)));
 
      while (TREE_CODE (t) == NOP_EXPR
	      && TREE_CODE (TREE_TYPE (TREE_OPERAND (t, 0))) == POINTER_TYPE)
	{
	  int thisalign;

	  t = TREE_OPERAND (t, 0);
	  thisalign = TYPE_ALIGN (TREE_TYPE (TREE_TYPE (t)));
	  if (thisalign > bestalign)
	    best = t, bestalign = thisalign;
	}
      return c_alignof (TREE_TYPE (TREE_TYPE (best)));
    }
  else
    return c_alignof (TREE_TYPE (expr));
}

/* Return either DECL or its known constant value (if it has one).  */

static tree
decl_constant_value (decl)
     tree decl;
{
  if (/* Don't change a variable array bound or initial value to a constant
	 in a place where a variable is invalid.  */
      current_function_decl != 0
      && ! pedantic
      && ! TREE_THIS_VOLATILE (decl)
      && TREE_READONLY (decl) && ! ITERATOR_P (decl)
      && DECL_INITIAL (decl) != 0
      && TREE_CODE (DECL_INITIAL (decl)) != ERROR_MARK
      /* This is invalid if initial value is not constant.
	 If it has either a function call, a memory reference,
	 or a variable, then re-evaluating it could give different results.  */
      && TREE_CONSTANT (DECL_INITIAL (decl))
      /* Check for cases where this is sub-optimal, even though valid.  */
      && TREE_CODE (DECL_INITIAL (decl)) != CONSTRUCTOR
      && DECL_MODE (decl) != BLKmode)
    return DECL_INITIAL (decl);
  return decl;
}

/* Perform default promotions for C data used in expressions.
   Arrays and functions are converted to pointers;
   enumeral types or short or char, to int.
   In addition, manifest constants symbols are replaced by their values.  */

tree
default_conversion (exp)
     tree exp;
{
  register tree type = TREE_TYPE (exp);
  register enum tree_code code = TREE_CODE (type);

  /* Constants can be used directly unless they're not loadable.  */
  if (TREE_CODE (exp) == CONST_DECL)
    exp = DECL_INITIAL (exp);

  /* Replace a nonvolatile const static variable with its value unless
     it is an array, in which case we must be sure that taking the
     address of the array produces consistent results.  */
  else if (optimize && TREE_CODE (exp) == VAR_DECL && code != ARRAY_TYPE)
    {
      exp = decl_constant_value (exp);
      type = TREE_TYPE (exp);
    }

  /* Strip NON_LVALUE_EXPRs and no-op conversions, since we aren't using as
     an lvalue.  */
  /* Do not use STRIP_NOPS here!  It will remove conversions from pointer
     to integer and cause infinite recursion.  */
  while (TREE_CODE (exp) == NON_LVALUE_EXPR
	 || (TREE_CODE (exp) == NOP_EXPR
	     && TREE_TYPE (TREE_OPERAND (exp, 0)) == TREE_TYPE (exp)))
    exp = TREE_OPERAND (exp, 0);

  /* Normally convert enums to int,
     but convert wide enums to something wider.  */
  if (code == ENUMERAL_TYPE)
    {
      type = type_for_size (MAX (TYPE_PRECISION (type),
				 TYPE_PRECISION (integer_type_node)),
			    ((flag_traditional
			      || (TYPE_PRECISION (type)
				  >= TYPE_PRECISION (integer_type_node)))
			     && TREE_UNSIGNED (type)));
      return convert (type, exp);
    }

  if (TREE_CODE (exp) == COMPONENT_REF
      && DECL_C_BIT_FIELD (TREE_OPERAND (exp, 1)))
    {
      tree width = DECL_SIZE (TREE_OPERAND (exp, 1));
      HOST_WIDE_INT low = TREE_INT_CST_LOW (width);

      /* If it's thinner than an int, promote it like a
	 C_PROMOTING_INTEGER_TYPE_P, otherwise leave it alone.  */

      if (low < TYPE_PRECISION (integer_type_node))
	{
	  if (flag_traditional && TREE_UNSIGNED (type))
	    return convert (unsigned_type_node, exp);
	  else
	    return convert (integer_type_node, exp);
	}
    }

  if (C_PROMOTING_INTEGER_TYPE_P (type))
    {
      /* Traditionally, unsignedness is preserved in default promotions.
         Also preserve unsignedness if not really getting any wider.  */
      if (TREE_UNSIGNED (type)
	  && (flag_traditional
	      || TYPE_PRECISION (type) == TYPE_PRECISION (integer_type_node)))
	return convert (unsigned_type_node, exp);
      return convert (integer_type_node, exp);
    }
  if (flag_traditional && !flag_allow_single_precision
      && TYPE_MAIN_VARIANT (type) == float_type_node)
    return convert (double_type_node, exp);
  if (code == VOID_TYPE)
    {
      error ("void value not ignored as it ought to be");
      return error_mark_node;
    }
  if (code == FUNCTION_TYPE)
    {
      return build_unary_op (ADDR_EXPR, exp, 0);
    }
  if (code == ARRAY_TYPE)
    {
      register tree adr;
      tree restype = TREE_TYPE (type);
      tree ptrtype;
      int constp = 0;
      int volatilep = 0;

      if (TREE_CODE_CLASS (TREE_CODE (exp)) == 'r'
	  || TREE_CODE_CLASS (TREE_CODE (exp)) == 'd')
	{
	  constp = TREE_READONLY (exp);
	  volatilep = TREE_THIS_VOLATILE (exp);
	}

      if (TYPE_QUALS (type) || constp || volatilep)
	restype 
	  = c_build_qualified_type (restype,
				    TYPE_QUALS (type) 
				    | (constp * TYPE_QUAL_CONST)
				    | (volatilep * TYPE_QUAL_VOLATILE));

      if (TREE_CODE (exp) == INDIRECT_REF)
	return convert (TYPE_POINTER_TO (restype),
			TREE_OPERAND (exp, 0));

      if (TREE_CODE (exp) == COMPOUND_EXPR)
	{
	  tree op1 = default_conversion (TREE_OPERAND (exp, 1));
	  return build (COMPOUND_EXPR, TREE_TYPE (op1),
			TREE_OPERAND (exp, 0), op1);
	}

      if (! lvalue_p (exp)
	  && ! (TREE_CODE (exp) == CONSTRUCTOR && TREE_STATIC (exp)))
	{
	  error ("invalid use of non-lvalue array");
	  return error_mark_node;
	}

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
      return convert (ptrtype, adr);
    }
  return exp;
}

/* Look up component name in the structure type definition.

   If this component name is found indirectly within an anonymous union,
   store in *INDIRECT the component which directly contains
   that anonymous union.  Otherwise, set *INDIRECT to 0.  */
     
static tree
lookup_field (type, component, indirect)
     tree type, component;
     tree *indirect;
{
  tree field;

  /* If TYPE_LANG_SPECIFIC is set, then it is a sorted array of pointers
     to the field elements.  Use a binary search on this array to quickly
     find the element.  Otherwise, do a linear search.  TYPE_LANG_SPECIFIC
     will always be set for structures which have many elements.  */

  if (TYPE_LANG_SPECIFIC (type))
    {
      int bot, top, half;
      tree *field_array = &TYPE_LANG_SPECIFIC (type)->elts[0];

      field = TYPE_FIELDS (type);
      bot = 0;
      top = TYPE_LANG_SPECIFIC (type)->len;
      while (top - bot > 1)
	{
	  half = (top - bot + 1) >> 1;
	  field = field_array[bot+half];

	  if (DECL_NAME (field) == NULL_TREE)
	    {
	      /* Step through all anon unions in linear fashion.  */
	      while (DECL_NAME (field_array[bot]) == NULL_TREE)
		{
		  tree anon = 0, junk;

		  field = field_array[bot++];
		  if (TREE_CODE (TREE_TYPE (field)) == RECORD_TYPE
		      || TREE_CODE (TREE_TYPE (field)) == UNION_TYPE)
		    anon = lookup_field (TREE_TYPE (field), component, &junk);

		  if (anon != NULL_TREE)
		    {
		      *indirect = field;
		      return anon;
		    }
		}

	      /* Entire record is only anon unions.  */
	      if (bot > top)
		return NULL_TREE;

	      /* Restart the binary search, with new lower bound.  */
	      continue;
	    }

	  if (DECL_NAME (field) == component)
	    break;
	  if (DECL_NAME (field) < component)
	    bot += half;
	  else
	    top = bot + half;
	}

      if (DECL_NAME (field_array[bot]) == component)
	field = field_array[bot];
      else if (DECL_NAME (field) != component)
	field = 0;
    }
  else
    {
      for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	{
	  if (DECL_NAME (field) == NULL_TREE)
	    {
	      tree junk;
	      tree anon = 0;

	      if (TREE_CODE (TREE_TYPE (field)) == RECORD_TYPE
		  || TREE_CODE (TREE_TYPE (field)) == UNION_TYPE)
		anon = lookup_field (TREE_TYPE (field), component, &junk);

	      if (anon != NULL_TREE)
		{
		  *indirect = field;
		  return anon;
		}
	    }

	  if (DECL_NAME (field) == component)
	    break;
	}
    }

  *indirect = NULL_TREE;
  return field;
}

/* Make an expression to refer to the COMPONENT field of
   structure or union value DATUM.  COMPONENT is an IDENTIFIER_NODE.  */

tree
build_component_ref (datum, component)
     tree datum, component;
{
  register tree type = TREE_TYPE (datum);
  register enum tree_code code = TREE_CODE (type);
  register tree field = NULL;
  register tree ref;

  /* If DATUM is a COMPOUND_EXPR or COND_EXPR, move our reference inside it
     unless we are not to support things not strictly ANSI.  */
  switch (TREE_CODE (datum))
    {
    case COMPOUND_EXPR:
      {
	tree value = build_component_ref (TREE_OPERAND (datum, 1), component);
	return build (COMPOUND_EXPR, TREE_TYPE (value),
		      TREE_OPERAND (datum, 0), value);
      }
    case COND_EXPR:
      return build_conditional_expr
	(TREE_OPERAND (datum, 0),
	 build_component_ref (TREE_OPERAND (datum, 1), component),
	 build_component_ref (TREE_OPERAND (datum, 2), component));

    default:
      break;
    }

  /* See if there is a field or component with name COMPONENT.  */

  if (code == RECORD_TYPE || code == UNION_TYPE)
    {
      tree indirect = 0;

      if (TYPE_SIZE (type) == 0)
	{
	  incomplete_type_error (NULL_TREE, type);
	  return error_mark_node;
	}

      field = lookup_field (type, component, &indirect);

      if (!field)
	{
	  error ("%s has no member named `%s'",
		 code == RECORD_TYPE ? "structure" : "union",
		 IDENTIFIER_POINTER (component));
	  return error_mark_node;
	}
      if (TREE_TYPE (field) == error_mark_node)
	return error_mark_node;

      /* If FIELD was found buried within an anonymous union,
	 make one COMPONENT_REF to get that anonymous union,
	 then fall thru to make a second COMPONENT_REF to get FIELD.  */
      if (indirect != 0)
	{
	  ref = build (COMPONENT_REF, TREE_TYPE (indirect), datum, indirect);
	  if (TREE_READONLY (datum) || TREE_READONLY (indirect))
	    TREE_READONLY (ref) = 1;
	  if (TREE_THIS_VOLATILE (datum) || TREE_THIS_VOLATILE (indirect))
	    TREE_THIS_VOLATILE (ref) = 1;
	  datum = ref;
	}

      ref = build (COMPONENT_REF, TREE_TYPE (field), datum, field);

      if (TREE_READONLY (datum) || TREE_READONLY (field))
	TREE_READONLY (ref) = 1;
      if (TREE_THIS_VOLATILE (datum) || TREE_THIS_VOLATILE (field))
	TREE_THIS_VOLATILE (ref) = 1;

      return ref;
    }
  else if (code != ERROR_MARK)
    error ("request for member `%s' in something not a structure or union",
	    IDENTIFIER_POINTER (component));

  return error_mark_node;
}

/* Given an expression PTR for a pointer, return an expression
   for the value pointed to.
   ERRORSTRING is the name of the operator to appear in error messages.  */

tree
build_indirect_ref (ptr, errorstring)
     tree ptr;
     const char *errorstring;
{
  register tree pointer = default_conversion (ptr);
  register tree type = TREE_TYPE (pointer);

  if (TREE_CODE (type) == POINTER_TYPE)
    {
      if (TREE_CODE (pointer) == ADDR_EXPR
	  && !flag_volatile
	  && (TREE_TYPE (TREE_OPERAND (pointer, 0))
	      == TREE_TYPE (type)))
	return TREE_OPERAND (pointer, 0);
      else
	{
	  tree t = TREE_TYPE (type);
	  register tree ref = build1 (INDIRECT_REF,
				      TYPE_MAIN_VARIANT (t), pointer);

	  if (TYPE_SIZE (t) == 0 && TREE_CODE (t) != ARRAY_TYPE)
	    {
	      error ("dereferencing pointer to incomplete type");
	      return error_mark_node;
	    }
	  if (TREE_CODE (t) == VOID_TYPE && skip_evaluation == 0)
	    warning ("dereferencing `void *' pointer");

	  /* We *must* set TREE_READONLY when dereferencing a pointer to const,
	     so that we get the proper error message if the result is used
	     to assign to.  Also, &* is supposed to be a no-op.
	     And ANSI C seems to specify that the type of the result
	     should be the const type.  */
	  /* A de-reference of a pointer to const is not a const.  It is valid
	     to change it via some other pointer.  */
	  TREE_READONLY (ref) = TYPE_READONLY (t);
	  TREE_SIDE_EFFECTS (ref)
	    = TYPE_VOLATILE (t) || TREE_SIDE_EFFECTS (pointer) || flag_volatile;
	  TREE_THIS_VOLATILE (ref) = TYPE_VOLATILE (t);
	  return ref;
	}
    }
  else if (TREE_CODE (pointer) != ERROR_MARK)
    error ("invalid type argument of `%s'", errorstring);
  return error_mark_node;
}

/* This handles expressions of the form "a[i]", which denotes
   an array reference.

   This is logically equivalent in C to *(a+i), but we may do it differently.
   If A is a variable or a member, we generate a primitive ARRAY_REF.
   This avoids forcing the array out of registers, and can work on
   arrays that are not lvalues (for example, members of structures returned
   by functions).  */

tree
build_array_ref (array, index)
     tree array, index;
{
  if (index == 0)
    {
      error ("subscript missing in array reference");
      return error_mark_node;
    }

  if (TREE_TYPE (array) == error_mark_node
      || TREE_TYPE (index) == error_mark_node)
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
	  && TYPE_MAIN_VARIANT (TREE_TYPE (index)) == char_type_node)
	warning ("array subscript has type `char'");

      /* Apply default promotions *after* noticing character types.  */
      index = default_conversion (index);

      /* Require integer *after* promotion, for sake of enums.  */
      if (TREE_CODE (TREE_TYPE (index)) != INTEGER_TYPE)
	{
	  error ("array subscript is not an integer");
	  return error_mark_node;
	}

      /* An array that is indexed by a non-constant
	 cannot be stored in a register; we must be able to do
	 address arithmetic on its address.
	 Likewise an array of elements of variable size.  */
      if (TREE_CODE (index) != INTEGER_CST
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
      if (TREE_CODE (index) == INTEGER_CST
	  && TYPE_VALUES (TREE_TYPE (array))
	  && ! int_fits_type_p (index, TYPE_VALUES (TREE_TYPE (array))))
	{
	  if (mark_addressable (array) == 0)
	    return error_mark_node;
	}

      if (pedantic && !lvalue_p (array))
	{
	  if (DECL_REGISTER (array))
	    pedwarn ("ANSI C forbids subscripting `register' array");
	  else
	    pedwarn ("ANSI C forbids subscripting non-lvalue array");
	}

      if (pedantic)
	{
	  tree foo = array;
	  while (TREE_CODE (foo) == COMPONENT_REF)
	    foo = TREE_OPERAND (foo, 0);
	  if (TREE_CODE (foo) == VAR_DECL && DECL_REGISTER (foo))
	    pedwarn ("ANSI C forbids subscripting non-lvalue array");
	}

      type = TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (array)));
      rval = build (ARRAY_REF, type, array, index);
      /* Array ref is const/volatile if the array elements are
         or if the array is.  */
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
    tree ind = default_conversion (index);

    /* Do the same warning check as above, but only on the part that's
       syntactically the index and only if it is also semantically
       the index.  */
    if (warn_char_subscripts
	&& TREE_CODE (TREE_TYPE (index)) == INTEGER_TYPE
	&& TYPE_MAIN_VARIANT (TREE_TYPE (index)) == char_type_node)
      warning ("subscript has type `char'");

    /* Put the integer in IND to simplify error checking.  */
    if (TREE_CODE (TREE_TYPE (ar)) == INTEGER_TYPE)
      {
	tree temp = ar;
	ar = ind;
	ind = temp;
      }

    if (ar == error_mark_node)
      return ar;

    if (TREE_CODE (TREE_TYPE (ar)) != POINTER_TYPE
	|| TREE_CODE (TREE_TYPE (TREE_TYPE (ar))) == FUNCTION_TYPE)
      {
	error ("subscripted value is neither array nor pointer");
	return error_mark_node;
      }
    if (TREE_CODE (TREE_TYPE (ind)) != INTEGER_TYPE)
      {
	error ("array subscript is not an integer");
	return error_mark_node;
      }

    return build_indirect_ref (build_binary_op (PLUS_EXPR, ar, ind, 0),
			       "array indexing");
  }
}

/* Build a function call to function FUNCTION with parameters PARAMS.
   PARAMS is a list--a chain of TREE_LIST nodes--in which the
   TREE_VALUE of each node is a parameter-expression.
   FUNCTION's data type may be a function type or a pointer-to-function.  */

tree
build_function_call (function, params)
     tree function, params;
{
  register tree fntype, fundecl = 0;
  register tree coerced_params;
  tree name = NULL_TREE, assembler_name = NULL_TREE;

  /* Strip NON_LVALUE_EXPRs, etc., since we aren't using as an lvalue.  */
  STRIP_TYPE_NOPS (function);

  /* Convert anything with function type to a pointer-to-function.  */
  if (TREE_CODE (function) == FUNCTION_DECL)
    {
      name = DECL_NAME (function);
      assembler_name = DECL_ASSEMBLER_NAME (function);

      /* Differs from default_conversion by not setting TREE_ADDRESSABLE
	 (because calling an inline function does not mean the function
	 needs to be separately compiled).  */
      fntype = build_type_variant (TREE_TYPE (function),
				   TREE_READONLY (function),
				   TREE_THIS_VOLATILE (function));
      fundecl = function;
      function = build1 (ADDR_EXPR, build_pointer_type (fntype), function);
    }
  else
    function = default_conversion (function);

  fntype = TREE_TYPE (function);

  if (TREE_CODE (fntype) == ERROR_MARK)
    return error_mark_node;

  if (!(TREE_CODE (fntype) == POINTER_TYPE
	&& TREE_CODE (TREE_TYPE (fntype)) == FUNCTION_TYPE))
    {
      error ("called object is not a function");
      return error_mark_node;
    }

  /* fntype now gets the type of function pointed to.  */
  fntype = TREE_TYPE (fntype);

  /* Convert the parameters to the types declared in the
     function prototype, or apply default promotions.  */

  coerced_params
    = convert_arguments (TYPE_ARG_TYPES (fntype), params, name, fundecl);

  /* Check for errors in format strings.  */

  if (warn_format && (name || assembler_name))
    check_function_format (name, assembler_name, coerced_params);

  /* Recognize certain built-in functions so we can make tree-codes
     other than CALL_EXPR.  We do this when it enables fold-const.c
     to do something useful.  */

  if (TREE_CODE (function) == ADDR_EXPR
      && TREE_CODE (TREE_OPERAND (function, 0)) == FUNCTION_DECL
      && DECL_BUILT_IN (TREE_OPERAND (function, 0))
      && DECL_BUILT_IN_CLASS (TREE_OPERAND (function, 0)) == BUILT_IN_NORMAL)
    switch (DECL_FUNCTION_CODE (TREE_OPERAND (function, 0)))
      {
      case BUILT_IN_ABS:
      case BUILT_IN_LABS:
      case BUILT_IN_FABS:
	if (coerced_params == 0)
	  return integer_zero_node;
	return build_unary_op (ABS_EXPR, TREE_VALUE (coerced_params), 0);
      default:
	break;
      }

  {
    register tree result
      = build (CALL_EXPR, TREE_TYPE (fntype),
	       function, coerced_params, NULL_TREE);

    TREE_SIDE_EFFECTS (result) = 1;
    if (TREE_TYPE (result) == void_type_node)
      return result;
    return require_complete_type (result);
  }
}

/* Convert the argument expressions in the list VALUES
   to the types in the list TYPELIST.  The result is a list of converted
   argument expressions.

   If TYPELIST is exhausted, or when an element has NULL as its type,
   perform the default conversions.

   PARMLIST is the chain of parm decls for the function being called.
   It may be 0, if that info is not available.
   It is used only for generating error messages.

   NAME is an IDENTIFIER_NODE or 0.  It is used only for error messages.

   This is also where warnings about wrong number of args are generated.

   Both VALUES and the returned value are chains of TREE_LIST nodes
   with the elements of the list in the TREE_VALUE slots of those nodes.  */

static tree
convert_arguments (typelist, values, name, fundecl)
     tree typelist, values, name, fundecl;
{
  register tree typetail, valtail;
  register tree result = NULL;
  int parmnum;

  /* Scan the given expressions and types, producing individual
     converted arguments and pushing them on RESULT in reverse order.  */

  for (valtail = values, typetail = typelist, parmnum = 0;
       valtail;
       valtail = TREE_CHAIN (valtail), parmnum++)
    {
      register tree type = typetail ? TREE_VALUE (typetail) : 0;
      register tree val = TREE_VALUE (valtail);

      if (type == void_type_node)
	{
	  if (name)
	    error ("too many arguments to function `%s'",
		   IDENTIFIER_POINTER (name));
	  else
	    error ("too many arguments to function");
	  break;
	}

      /* Strip NON_LVALUE_EXPRs since we aren't using as an lvalue.  */
      /* Do not use STRIP_NOPS here!  We do not want an enumerator with value 0
	 to convert automatically to a pointer.  */
      if (TREE_CODE (val) == NON_LVALUE_EXPR)
	val = TREE_OPERAND (val, 0);

      if (TREE_CODE (TREE_TYPE (val)) == ARRAY_TYPE
	  || TREE_CODE (TREE_TYPE (val)) == FUNCTION_TYPE)
	val = default_conversion (val);

      val = require_complete_type (val);

      if (type != 0)
	{
	  /* Formal parm type is specified by a function prototype.  */
	  tree parmval;

	  if (TYPE_SIZE (type) == 0)
	    {
	      error ("type of formal parameter %d is incomplete", parmnum + 1);
	      parmval = val;
	    }
	  else
	    {
	      /* Optionally warn about conversions that
		 differ from the default conversions.  */
	      if (warn_conversion)
		{
		  int formal_prec = TYPE_PRECISION (type);

		  if (INTEGRAL_TYPE_P (type)
		      && TREE_CODE (TREE_TYPE (val)) == REAL_TYPE)
		    warn_for_assignment ("%s as integer rather than floating due to prototype", (char *) 0, name, parmnum + 1);
		  else if (TREE_CODE (type) == COMPLEX_TYPE
			   && TREE_CODE (TREE_TYPE (val)) == REAL_TYPE)
		    warn_for_assignment ("%s as complex rather than floating due to prototype", (char *) 0, name, parmnum + 1);
		  else if (TREE_CODE (type) == REAL_TYPE
			   && INTEGRAL_TYPE_P (TREE_TYPE (val)))
		    warn_for_assignment ("%s as floating rather than integer due to prototype", (char *) 0, name, parmnum + 1);
		  else if (TREE_CODE (type) == REAL_TYPE
			   && TREE_CODE (TREE_TYPE (val)) == COMPLEX_TYPE)
		    warn_for_assignment ("%s as floating rather than complex due to prototype", (char *) 0, name, parmnum + 1);
		  /* ??? At some point, messages should be written about
		     conversions between complex types, but that's too messy
		     to do now.  */
		  else if (TREE_CODE (type) == REAL_TYPE
			   && TREE_CODE (TREE_TYPE (val)) == REAL_TYPE)
		    {
		      /* Warn if any argument is passed as `float',
			 since without a prototype it would be `double'.  */
		      if (formal_prec == TYPE_PRECISION (float_type_node))
			warn_for_assignment ("%s as `float' rather than `double' due to prototype", (char *) 0, name, parmnum + 1);
		    }
		  /* Detect integer changing in width or signedness.  */
		  else if (INTEGRAL_TYPE_P (type)
			   && INTEGRAL_TYPE_P (TREE_TYPE (val)))
		    {
		      tree would_have_been = default_conversion (val);
		      tree type1 = TREE_TYPE (would_have_been);

		      if (TREE_CODE (type) == ENUMERAL_TYPE
			  && type == TREE_TYPE (val))
			/* No warning if function asks for enum
			   and the actual arg is that enum type.  */
			;
		      else if (formal_prec != TYPE_PRECISION (type1))
			warn_for_assignment ("%s with different width due to prototype", (char *) 0, name, parmnum + 1);
		      else if (TREE_UNSIGNED (type) == TREE_UNSIGNED (type1))
			;
		      /* Don't complain if the formal parameter type
			 is an enum, because we can't tell now whether
			 the value was an enum--even the same enum.  */
		      else if (TREE_CODE (type) == ENUMERAL_TYPE)
			;
		      else if (TREE_CODE (val) == INTEGER_CST
			       && int_fits_type_p (val, type))
			/* Change in signedness doesn't matter
			   if a constant value is unaffected.  */
			;
		      /* Likewise for a constant in a NOP_EXPR.  */
		      else if (TREE_CODE (val) == NOP_EXPR
			       && TREE_CODE (TREE_OPERAND (val, 0)) == INTEGER_CST
			       && int_fits_type_p (TREE_OPERAND (val, 0), type))
			;
#if 0 /* We never get such tree structure here.  */
		      else if (TREE_CODE (TREE_TYPE (val)) == ENUMERAL_TYPE
			       && int_fits_type_p (TYPE_MIN_VALUE (TREE_TYPE (val)), type)
			       && int_fits_type_p (TYPE_MAX_VALUE (TREE_TYPE (val)), type))
			/* Change in signedness doesn't matter
			   if an enum value is unaffected.  */
			;
#endif
		      /* If the value is extended from a narrower
			 unsigned type, it doesn't matter whether we
			 pass it as signed or unsigned; the value
			 certainly is the same either way.  */
		      else if (TYPE_PRECISION (TREE_TYPE (val)) < TYPE_PRECISION (type)
			       && TREE_UNSIGNED (TREE_TYPE (val)))
			;
		      else if (TREE_UNSIGNED (type))
			warn_for_assignment ("%s as unsigned due to prototype", (char *) 0, name, parmnum + 1);
		      else
			warn_for_assignment ("%s as signed due to prototype", (char *) 0, name, parmnum + 1);
		    }
		}

	      parmval = convert_for_assignment (type, val, 
					        (char *) 0, /* arg passing  */
						fundecl, name, parmnum + 1);
	      
	      if (PROMOTE_PROTOTYPES
		  && (TREE_CODE (type) == INTEGER_TYPE
		      || TREE_CODE (type) == ENUMERAL_TYPE)
		  && (TYPE_PRECISION (type) < TYPE_PRECISION (integer_type_node)))
		parmval = default_conversion (parmval);
	    }
	  result = tree_cons (NULL_TREE, parmval, result);
	}
      else if (TREE_CODE (TREE_TYPE (val)) == REAL_TYPE
               && (TYPE_PRECISION (TREE_TYPE (val))
	           < TYPE_PRECISION (double_type_node)))
	/* Convert `float' to `double'.  */
	result = tree_cons (NULL_TREE, convert (double_type_node, val), result);
      else
	/* Convert `short' and `char' to full-size `int'.  */
	result = tree_cons (NULL_TREE, default_conversion (val), result);

      if (typetail)
	typetail = TREE_CHAIN (typetail);
    }

  if (typetail != 0 && TREE_VALUE (typetail) != void_type_node)
    {
      if (name)
	error ("too few arguments to function `%s'",
	       IDENTIFIER_POINTER (name));
      else
	error ("too few arguments to function");
    }

  return nreverse (result);
}

/* This is the entry point used by the parser
   for binary operators in the input.
   In addition to constructing the expression,
   we check for operands that were written with other binary operators
   in a way that is likely to confuse the user.  */

tree
parser_build_binary_op (code, arg1, arg2)
     enum tree_code code;
     tree arg1, arg2;
{
  tree result = build_binary_op (code, arg1, arg2, 1);

  char class;
  char class1 = TREE_CODE_CLASS (TREE_CODE (arg1));
  char class2 = TREE_CODE_CLASS (TREE_CODE (arg2));
  enum tree_code code1 = ERROR_MARK;
  enum tree_code code2 = ERROR_MARK;

  if (class1 == 'e' || class1 == '1'
      || class1 == '2' || class1 == '<')
    code1 = C_EXP_ORIGINAL_CODE (arg1);
  if (class2 == 'e' || class2 == '1'
      || class2 == '2' || class2 == '<')
    code2 = C_EXP_ORIGINAL_CODE (arg2);

  /* Check for cases such as x+y<<z which users are likely
     to misinterpret.  If parens are used, C_EXP_ORIGINAL_CODE
     is cleared to prevent these warnings.  */
  if (warn_parentheses)
    {
      if (code == LSHIFT_EXPR || code == RSHIFT_EXPR)
	{
	  if (code1 == PLUS_EXPR || code1 == MINUS_EXPR
	      || code2 == PLUS_EXPR || code2 == MINUS_EXPR)
	    warning ("suggest parentheses around + or - inside shift");
	}

      if (code == TRUTH_ORIF_EXPR)
	{
	  if (code1 == TRUTH_ANDIF_EXPR
	      || code2 == TRUTH_ANDIF_EXPR)
	    warning ("suggest parentheses around && within ||");
	}

      if (code == BIT_IOR_EXPR)
	{
	  if (code1 == BIT_AND_EXPR || code1 == BIT_XOR_EXPR
	      || code1 == PLUS_EXPR || code1 == MINUS_EXPR
	      || code2 == BIT_AND_EXPR || code2 == BIT_XOR_EXPR
	      || code2 == PLUS_EXPR || code2 == MINUS_EXPR)
	    warning ("suggest parentheses around arithmetic in operand of |");
	  /* Check cases like x|y==z */
	  if (TREE_CODE_CLASS (code1) == '<' || TREE_CODE_CLASS (code2) == '<')
	    warning ("suggest parentheses around comparison in operand of |");
	}

      if (code == BIT_XOR_EXPR)
	{
	  if (code1 == BIT_AND_EXPR
	      || code1 == PLUS_EXPR || code1 == MINUS_EXPR
	      || code2 == BIT_AND_EXPR
	      || code2 == PLUS_EXPR || code2 == MINUS_EXPR)
	    warning ("suggest parentheses around arithmetic in operand of ^");
	  /* Check cases like x^y==z */
	  if (TREE_CODE_CLASS (code1) == '<' || TREE_CODE_CLASS (code2) == '<')
	    warning ("suggest parentheses around comparison in operand of ^");
	}

      if (code == BIT_AND_EXPR)
	{
	  if (code1 == PLUS_EXPR || code1 == MINUS_EXPR
	      || code2 == PLUS_EXPR || code2 == MINUS_EXPR)
	    warning ("suggest parentheses around + or - in operand of &");
	  /* Check cases like x&y==z */
	  if (TREE_CODE_CLASS (code1) == '<' || TREE_CODE_CLASS (code2) == '<')
	    warning ("suggest parentheses around comparison in operand of &");
	}
    }

  /* Similarly, check for cases like 1<=i<=10 that are probably errors.  */
  if (TREE_CODE_CLASS (code) == '<' && extra_warnings
      && (TREE_CODE_CLASS (code1) == '<' || TREE_CODE_CLASS (code2) == '<'))
    warning ("comparisons like X<=Y<=Z do not have their mathematical meaning");

  unsigned_conversion_warning (result, arg1);
  unsigned_conversion_warning (result, arg2);
  overflow_warning (result);

  class = TREE_CODE_CLASS (TREE_CODE (result));

  /* Record the code that was specified in the source,
     for the sake of warnings about confusing nesting.  */
  if (class == 'e' || class == '1'
      || class == '2' || class == '<')
    C_SET_EXP_ORIGINAL_CODE (result, code);
  else
    {
      int flag = TREE_CONSTANT (result);
      /* We used to use NOP_EXPR rather than NON_LVALUE_EXPR
	 so that convert_for_assignment wouldn't strip it.
	 That way, we got warnings for things like p = (1 - 1).
	 But it turns out we should not get those warnings.  */
      result = build1 (NON_LVALUE_EXPR, TREE_TYPE (result), result);
      C_SET_EXP_ORIGINAL_CODE (result, code);
      TREE_CONSTANT (result) = flag;
    }

  return result;
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

   Note that the operands will never have enumeral types, or function
   or array types, because either they will have the default conversions
   performed or they have both just been converted to some other type in which
   the arithmetic is to be done.  */

tree
build_binary_op (code, orig_op0, orig_op1, convert_p)
     enum tree_code code;
     tree orig_op0, orig_op1;
     int convert_p;
{
  tree type0, type1;
  register enum tree_code code0, code1;
  tree op0, op1;

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

  if (convert_p)
    {
      op0 = default_conversion (orig_op0);
      op1 = default_conversion (orig_op1);
    }
  else
    {
      op0 = orig_op0;
      op1 = orig_op1;
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
	  && comp_target_types (type0, type1))
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
	  if (!(code0 == INTEGER_TYPE && code1 == INTEGER_TYPE))
	    resultcode = RDIV_EXPR;
	  else
	    {
	      /* Although it would be tempting to shorten always here, that
		 loses on some targets, since the modulo instruction is
		 undefined if the quotient can't be represented in the
		 computation mode.  We shorten only if unsigned or if
		 dividing by something we know != -1.  */
	      shorten = (TREE_UNSIGNED (TREE_TYPE (orig_op0))
			 || (TREE_CODE (op1) == INTEGER_CST
			     && (TREE_INT_CST_LOW (op1) != -1
				 || TREE_INT_CST_HIGH (op1) != -1)));
	    }
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
      if (code0 == INTEGER_TYPE && code1 == INTEGER_TYPE)
	{
	  /* Although it would be tempting to shorten always here, that loses
	     on some targets, since the modulo instruction is undefined if the
	     quotient can't be represented in the computation mode.  We shorten
	     only if unsigned or if dividing by something we know != -1.  */
	  shorten = (TREE_UNSIGNED (TREE_TYPE (orig_op0))
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
    case TRUTH_XOR_EXPR:
      if ((code0 == INTEGER_TYPE || code0 == POINTER_TYPE
	   || code0 == REAL_TYPE || code0 == COMPLEX_TYPE)
	  && (code1 == INTEGER_TYPE || code1 == POINTER_TYPE
	      || code1 == REAL_TYPE || code1 == COMPLEX_TYPE))
	{
	  /* Result of these operations is always an int,
	     but that does not mean the operands should be
	     converted to ints!  */
	  result_type = integer_type_node;
	  op0 = truthvalue_conversion (op0);
	  op1 = truthvalue_conversion (op1);
	  converted = 1;
	}
      break;

      /* Shift operations: result has same type as first operand;
	 always convert second operand to int.
	 Also set SHORT_SHIFT if shifting rightward.  */

    case RSHIFT_EXPR:
      if (code0 == INTEGER_TYPE && code1 == INTEGER_TYPE)
	{
	  if (TREE_CODE (op1) == INTEGER_CST && skip_evaluation == 0)
	    {
	      if (tree_int_cst_sgn (op1) < 0)
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
	  /* Use the type of the value to be shifted.
	     This is what most traditional C compilers do.  */
	  result_type = type0;
	  /* Unless traditional, convert the shift-count to an integer,
	     regardless of size of value being shifted.  */
	  if (! flag_traditional)
	    {
	      if (TYPE_MAIN_VARIANT (TREE_TYPE (op1)) != integer_type_node)
		op1 = convert (integer_type_node, op1);
	      /* Avoid converting op1 to result_type later.  */
	      converted = 1;
	    }
	}
      break;

    case LSHIFT_EXPR:
      if (code0 == INTEGER_TYPE && code1 == INTEGER_TYPE)
	{
	  if (TREE_CODE (op1) == INTEGER_CST && skip_evaluation == 0)
	    {
	      if (tree_int_cst_sgn (op1) < 0)
		warning ("left shift count is negative");
	      else if (TREE_INT_CST_HIGH (op1) != 0
		       || ((unsigned HOST_WIDE_INT) TREE_INT_CST_LOW (op1)
			   >= TYPE_PRECISION (type0)))
		warning ("left shift count >= width of type");
	    }
	  /* Use the type of the value to be shifted.
	     This is what most traditional C compilers do.  */
	  result_type = type0;
	  /* Unless traditional, convert the shift-count to an integer,
	     regardless of size of value being shifted.  */
	  if (! flag_traditional)
	    {
	      if (TYPE_MAIN_VARIANT (TREE_TYPE (op1)) != integer_type_node)
		op1 = convert (integer_type_node, op1);
	      /* Avoid converting op1 to result_type later.  */
	      converted = 1;
	    }
	}
      break;

    case RROTATE_EXPR:
    case LROTATE_EXPR:
      if (code0 == INTEGER_TYPE && code1 == INTEGER_TYPE)
	{
	  if (TREE_CODE (op1) == INTEGER_CST && skip_evaluation == 0)
	    {
	      if (tree_int_cst_sgn (op1) < 0)
		warning ("shift count is negative");
	      else if (TREE_INT_CST_HIGH (op1) != 0
		       || ((unsigned HOST_WIDE_INT) TREE_INT_CST_LOW (op1)
			   >= TYPE_PRECISION (type0)))
		warning ("shift count >= width of type");
	    }
	  /* Use the type of the value to be shifted.
	     This is what most traditional C compilers do.  */
	  result_type = type0;
	  /* Unless traditional, convert the shift-count to an integer,
	     regardless of size of value being shifted.  */
	  if (! flag_traditional)
	    {
	      if (TYPE_MAIN_VARIANT (TREE_TYPE (op1)) != integer_type_node)
		op1 = convert (integer_type_node, op1);
	      /* Avoid converting op1 to result_type later.  */
	      converted = 1;
	    }
	}
      break;

    case EQ_EXPR:
    case NE_EXPR:
      if (warn_float_equal && (code0 == REAL_TYPE || code1 == REAL_TYPE))
	warning ("comparing floating point with == or != is unsafe");
      /* Result of comparison is always int,
	 but don't convert the args to int!  */
      build_type = integer_type_node;
      if ((code0 == INTEGER_TYPE || code0 == REAL_TYPE
	   || code0 == COMPLEX_TYPE)
	  && (code1 == INTEGER_TYPE || code1 == REAL_TYPE
	      || code1 == COMPLEX_TYPE))
	short_compare = 1;
      else if (code0 == POINTER_TYPE && code1 == POINTER_TYPE)
	{
	  register tree tt0 = TREE_TYPE (type0);
	  register tree tt1 = TREE_TYPE (type1);
	  /* Anything compares with void *.  void * compares with anything.
	     Otherwise, the targets must be compatible
	     and both must be object or both incomplete.  */
	  if (comp_target_types (type0, type1))
	    result_type = common_type (type0, type1);
	  else if (TYPE_MAIN_VARIANT (tt0) == void_type_node)
	    {
	      /* op0 != orig_op0 detects the case of something
		 whose value is 0 but which isn't a valid null ptr const.  */
	      if (pedantic && (!integer_zerop (op0) || op0 != orig_op0)
		  && TREE_CODE (tt1) == FUNCTION_TYPE)
		pedwarn ("ANSI C forbids comparison of `void *' with function pointer");
	    }
	  else if (TYPE_MAIN_VARIANT (tt1) == void_type_node)
	    {
	      if (pedantic && (!integer_zerop (op1) || op1 != orig_op1)
		  && TREE_CODE (tt0) == FUNCTION_TYPE)
		pedwarn ("ANSI C forbids comparison of `void *' with function pointer");
	    }
	  else
	    pedwarn ("comparison of distinct pointer types lacks a cast");

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
	  if (! flag_traditional)
	    pedwarn ("comparison between pointer and integer");
	}
      else if (code0 == INTEGER_TYPE && code1 == POINTER_TYPE)
	{
	  result_type = type1;
	  if (! flag_traditional)
	    pedwarn ("comparison between pointer and integer");
	}
      break;

    case MAX_EXPR:
    case MIN_EXPR:
      if ((code0 == INTEGER_TYPE || code0 == REAL_TYPE)
	  && (code1 == INTEGER_TYPE || code1 == REAL_TYPE))
	shorten = 1;
      else if (code0 == POINTER_TYPE && code1 == POINTER_TYPE)
	{
	  if (comp_target_types (type0, type1))
	    {
	      result_type = common_type (type0, type1);
	      if (pedantic 
		  && TREE_CODE (TREE_TYPE (type0)) == FUNCTION_TYPE)
		pedwarn ("ANSI C forbids ordered comparisons of pointers to functions");
	    }
	  else
	    {
	      result_type = ptr_type_node;
	      pedwarn ("comparison of distinct pointer types lacks a cast");
	    }
	}
      break;

    case LE_EXPR:
    case GE_EXPR:
    case LT_EXPR:
    case GT_EXPR:
      build_type = integer_type_node;
      if ((code0 == INTEGER_TYPE || code0 == REAL_TYPE)
	  && (code1 == INTEGER_TYPE || code1 == REAL_TYPE))
	short_compare = 1;
      else if (code0 == POINTER_TYPE && code1 == POINTER_TYPE)
	{
	  if (comp_target_types (type0, type1))
	    {
	      result_type = common_type (type0, type1);
	      if ((TYPE_SIZE (TREE_TYPE (type0)) != 0)
		  != (TYPE_SIZE (TREE_TYPE (type1)) != 0))
		pedwarn ("comparison of complete and incomplete pointers");
	      else if (pedantic 
		       && TREE_CODE (TREE_TYPE (type0)) == FUNCTION_TYPE)
		pedwarn ("ANSI C forbids ordered comparisons of pointers to functions");
	    }
	  else
	    {
	      result_type = ptr_type_node;
	      pedwarn ("comparison of distinct pointer types lacks a cast");
	    }
	}
      else if (code0 == POINTER_TYPE && TREE_CODE (op1) == INTEGER_CST
	       && integer_zerop (op1))
	{
	  result_type = type0;
	  if (pedantic || extra_warnings)
	    pedwarn ("ordered comparison of pointer with integer zero");
	}
      else if (code1 == POINTER_TYPE && TREE_CODE (op0) == INTEGER_CST
	       && integer_zerop (op0))
	{
	  result_type = type1;
	  if (pedantic)
	    pedwarn ("ordered comparison of pointer with integer zero");
	}
      else if (code0 == POINTER_TYPE && code1 == INTEGER_TYPE)
	{
	  result_type = type0;
	  if (! flag_traditional)
	    pedwarn ("comparison between pointer and integer");
	}
      else if (code0 == INTEGER_TYPE && code1 == POINTER_TYPE)
	{
	  result_type = type1;
	  if (! flag_traditional)
	    pedwarn ("comparison between pointer and integer");
	}
      break;
      
    default:
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

	  /* Handle the case that OP0 (or OP1) does not *contain* a conversion
	     but it *requires* conversion to FINAL_TYPE.  */

	  if ((TYPE_PRECISION (TREE_TYPE (op0))
	       == TYPE_PRECISION (TREE_TYPE (arg0)))
	      && TREE_TYPE (op0) != final_type)
	    unsigned0 = TREE_UNSIGNED (TREE_TYPE (op0));
	  if ((TYPE_PRECISION (TREE_TYPE (op1))
	       == TYPE_PRECISION (TREE_TYPE (arg1)))
	      && TREE_TYPE (op1) != final_type)
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
		  || 2 * TYPE_PRECISION (TREE_TYPE (arg0)) <= TYPE_PRECISION (result_type)))
	    {
	      /* Do an unsigned shift if the operand was zero-extended.  */
	      result_type
		= signed_or_unsigned_type (unsigned_arg,
					   TREE_TYPE (arg0));
	      /* Convert value-to-be-shifted to that type.  */
	      if (TREE_TYPE (op0) != result_type)
		op0 = convert (result_type, op0);
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
	    return val;
	  op0 = xop0, op1 = xop1;
	  converted = 1;
	  resultcode = xresultcode;

	  if ((warn_sign_compare < 0 ? extra_warnings : warn_sign_compare != 0)
	      && skip_evaluation == 0)
	    {
	      int op0_signed = ! TREE_UNSIGNED (TREE_TYPE (orig_op0));
	      int op1_signed = ! TREE_UNSIGNED (TREE_TYPE (orig_op1));

	      int unsignedp0, unsignedp1;
	      tree primop0 = get_narrower (op0, &unsignedp0);
	      tree primop1 = get_narrower (op1, &unsignedp1);

	      xop0 = orig_op0;
	      xop1 = orig_op1;
	      STRIP_TYPE_NOPS (xop0);
	      STRIP_TYPE_NOPS (xop1);

	      /* Give warnings for comparisons between signed and unsigned
		 quantities that may fail.  */
	      /* Do the checking based on the original operand trees, so that
		 casts will be considered, but default promotions won't be.  */

	      /* Do not warn if the comparison is being done in a signed type,
		 since the signed type will only be chosen if it can represent
		 all the values of the unsigned type.  */
	      if (! TREE_UNSIGNED (result_type))
		/* OK */;
              /* Do not warn if both operands are the same signedness.  */
              else if (op0_signed == op1_signed)
                /* OK */;
	      else
		{
		  tree sop, uop;
		  if (op0_signed)
		    sop = xop0, uop = xop1;
		  else
		    sop = xop1, uop = xop0;

		  /* Do not warn if the signed quantity is an unsuffixed
		     integer literal (or some static constant expression
		     involving such literals) and it is non-negative.  */
		  if (TREE_CODE (sop) == INTEGER_CST
		      && tree_int_cst_sgn (sop) >= 0)
		    /* OK */;
		  /* Do not warn if the comparison is an equality operation,
		     the unsigned quantity is an integral constant, and it
		     would fit in the result if the result were signed.  */
		  else if (TREE_CODE (uop) == INTEGER_CST
			   && (resultcode == EQ_EXPR || resultcode == NE_EXPR)
			   && int_fits_type_p (uop, signed_type (result_type)))
		    /* OK */;
		  /* Do not warn if the unsigned quantity is an enumeration
		     constant and its maximum value would fit in the result
		     if the result were signed.  */
		  else if (TREE_CODE (uop) == INTEGER_CST
			   && TREE_CODE (TREE_TYPE (uop)) == ENUMERAL_TYPE
			   && int_fits_type_p (TYPE_MAX_VALUE (TREE_TYPE(uop)),
					       signed_type (result_type)))
		    /* OK */;
		  else
		    warning ("comparison between signed and unsigned");
		}

	      /* Warn if two unsigned values are being compared in a size
		 larger than their original size, and one (and only one) is the
		 result of a `~' operator.  This comparison will always fail.

		 Also warn if one operand is a constant, and the constant
		 does not have all bits set that are set in the ~ operand
		 when it is extended.  */

	      if ((TREE_CODE (primop0) == BIT_NOT_EXPR)
		  != (TREE_CODE (primop1) == BIT_NOT_EXPR))
		{
		  if (TREE_CODE (primop0) == BIT_NOT_EXPR)
		    primop0 = get_narrower (TREE_OPERAND (primop0, 0),
					    &unsignedp0);
		  else
		    primop1 = get_narrower (TREE_OPERAND (primop1, 0),
					    &unsignedp1);
	      
		  if (TREE_CODE (primop0) == INTEGER_CST
		      || TREE_CODE (primop1) == INTEGER_CST)
		    {
		      tree primop;
		      long constant, mask;
		      int unsignedp, bits;

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
			  mask = (~0L) << bits;
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
    }

  /* At this point, RESULT_TYPE must be nonzero to avoid an error message.
     If CONVERTED is zero, both args will be converted to type RESULT_TYPE.
     Then the expression will be built.
     It will be given type FINAL_TYPE if that is nonzero;
     otherwise, it will be given type RESULT_TYPE.  */

  if (!result_type)
    {
      binary_op_error (code);
      return error_mark_node;
    }

  if (! converted)
    {
      if (TREE_TYPE (op0) != result_type)
	op0 = convert (result_type, op0); 
      if (TREE_TYPE (op1) != result_type)
	op1 = convert (result_type, op1); 
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
      return convert (final_type, folded);
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
  register tree folded;

  /* The result is a pointer of the same type that is being added.  */

  register tree result_type = TREE_TYPE (ptrop);

  if (TREE_CODE (TREE_TYPE (result_type)) == VOID_TYPE)
    {
      if (pedantic || warn_pointer_arith)
	pedwarn ("pointer of type `void *' used in arithmetic");
      size_exp = integer_one_node;
    }
  else if (TREE_CODE (TREE_TYPE (result_type)) == FUNCTION_TYPE)
    {
      if (pedantic || warn_pointer_arith)
	pedwarn ("pointer to a function used in arithmetic");
      size_exp = integer_one_node;
    }
  else
    size_exp = c_size_in_bytes (TREE_TYPE (result_type));

  /* If what we are about to multiply by the size of the elements
     contains a constant term, apply distributive law
     and multiply that constant term separately.
     This helps produce common subexpressions.  */

  if ((TREE_CODE (intop) == PLUS_EXPR || TREE_CODE (intop) == MINUS_EXPR)
      && ! TREE_CONSTANT (intop)
      && TREE_CONSTANT (TREE_OPERAND (intop, 1))
      && TREE_CONSTANT (size_exp)
      /* If the constant comes from pointer subtraction,
	 skip this optimization--it would cause an error.  */
      && TREE_CODE (TREE_TYPE (TREE_OPERAND (intop, 0))) == INTEGER_TYPE
      /* If the constant is unsigned, and smaller than the pointer size,
	 then we must skip this optimization.  This is because it could cause
	 an overflow error if the constant is negative but INTOP is not.  */
      && (! TREE_UNSIGNED (TREE_TYPE (intop))
	  || (TYPE_PRECISION (TREE_TYPE (intop))
	      == TYPE_PRECISION (TREE_TYPE (ptrop)))))
    {
      enum tree_code subcode = resultcode;
      tree int_type = TREE_TYPE (intop);
      if (TREE_CODE (intop) == MINUS_EXPR)
	subcode = (subcode == PLUS_EXPR ? MINUS_EXPR : PLUS_EXPR);
      /* Convert both subexpression types to the type of intop,
	 because weird cases involving pointer arithmetic
	 can result in a sum or difference with different type args.  */
      ptrop = build_binary_op (subcode, ptrop,
			       convert (int_type, TREE_OPERAND (intop, 1)), 1);
      intop = convert (int_type, TREE_OPERAND (intop, 0));
    }

  /* Convert the integer argument to a type the same size as sizetype
     so the multiply won't overflow spuriously.  */

  if (TYPE_PRECISION (TREE_TYPE (intop)) != TYPE_PRECISION (sizetype)
      || TREE_UNSIGNED (TREE_TYPE (intop)) != TREE_UNSIGNED (sizetype))
    intop = convert (type_for_size (TYPE_PRECISION (sizetype), 
				    TREE_UNSIGNED (sizetype)), intop);

  /* Replace the integer argument with a suitable product by the object size.
     Do this multiplication as signed, then convert to the appropriate
     pointer type (actually unsigned integral).  */

  intop = convert (result_type,
		   build_binary_op (MULT_EXPR, intop,
				    convert (TREE_TYPE (intop), size_exp), 1));

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
	pedwarn ("pointer of type `void *' used in subtraction");
      if (TREE_CODE (target_type) == FUNCTION_TYPE)
	pedwarn ("pointer to a function used in subtraction");
    }

  /* First do the subtraction as integers;
     then drop through to build the divide operator.
     Do not do default conversions on the minus operator
     in case restype is a short type.  */

  op0 = build_binary_op (MINUS_EXPR, convert (restype, op0),
			 convert (restype, op1), 0);
  /* This generates an error if op1 is pointer to incomplete type.  */
  if (TYPE_SIZE (TREE_TYPE (TREE_TYPE (op1))) == 0)
    error ("arithmetic on pointer to an incomplete type");

  /* This generates an error if op0 is pointer to incomplete type.  */
  op1 = c_size_in_bytes (target_type);

  /* Divide by the size, in easiest possible way.  */

  result = build (EXACT_DIV_EXPR, restype, op0, convert (restype, op1));

  folded = fold (result);
  if (folded == result)
    TREE_CONSTANT (folded) = TREE_CONSTANT (op0) & TREE_CONSTANT (op1);
  return folded;
}

/* Construct and perhaps optimize a tree representation
   for a unary operation.  CODE, a tree_code, specifies the operation
   and XARG is the operand.  NOCONVERT nonzero suppresses
   the default promotions (such as from short to int).  */

tree
build_unary_op (code, xarg, noconvert)
     enum tree_code code;
     tree xarg;
     int noconvert;
{
  /* No default_conversion here.  It causes trouble for ADDR_EXPR.  */
  register tree arg = xarg;
  register tree argtype = 0;
  register enum tree_code typecode = TREE_CODE (TREE_TYPE (arg));
  tree val;

  if (typecode == ERROR_MARK)
    return error_mark_node;
  if (typecode == ENUMERAL_TYPE)
    typecode = INTEGER_TYPE;

  switch (code)
    {
    case CONVERT_EXPR:
      /* This is used for unary plus, because a CONVERT_EXPR
	 is enough to prevent anybody from looking inside for
	 associativity, but won't generate any code.  */
      if (!(typecode == INTEGER_TYPE || typecode == REAL_TYPE
	    || typecode == COMPLEX_TYPE))
	{
	  error ("wrong type argument to unary plus");
	  return error_mark_node;
	}
      else if (!noconvert)
	arg = default_conversion (arg);
      break;

    case NEGATE_EXPR:
      if (!(typecode == INTEGER_TYPE || typecode == REAL_TYPE
	    || typecode == COMPLEX_TYPE))
	{
	  error ("wrong type argument to unary minus");
	  return error_mark_node;
	}
      else if (!noconvert)
	arg = default_conversion (arg);
      break;

    case BIT_NOT_EXPR:
      if (typecode == COMPLEX_TYPE)
	{
	  code = CONJ_EXPR;
	  if (!noconvert)
	    arg = default_conversion (arg);
	}
      else if (typecode != INTEGER_TYPE)
	{
	  error ("wrong type argument to bit-complement");
	  return error_mark_node;
	}
      else if (!noconvert)
	arg = default_conversion (arg);
      break;

    case ABS_EXPR:
      if (!(typecode == INTEGER_TYPE || typecode == REAL_TYPE
	    || typecode == COMPLEX_TYPE))
	{
	  error ("wrong type argument to abs");
	  return error_mark_node;
	}
      else if (!noconvert)
	arg = default_conversion (arg);
      break;

    case CONJ_EXPR:
      /* Conjugating a real value is a no-op, but allow it anyway.  */
      if (!(typecode == INTEGER_TYPE || typecode == REAL_TYPE
	    || typecode == COMPLEX_TYPE))
	{
	  error ("wrong type argument to conjugation");
	  return error_mark_node;
	}
      else if (!noconvert)
	arg = default_conversion (arg);
      break;

    case TRUTH_NOT_EXPR:
      if (typecode != INTEGER_TYPE
	  && typecode != REAL_TYPE && typecode != POINTER_TYPE
	  && typecode != COMPLEX_TYPE
	  /* These will convert to a pointer.  */
	  && typecode != ARRAY_TYPE && typecode != FUNCTION_TYPE)
	{
	  error ("wrong type argument to unary exclamation mark");
	  return error_mark_node;
	}
      arg = truthvalue_conversion (arg);
      return invert_truthvalue (arg);

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
	return convert (TREE_TYPE (arg), integer_zero_node);
      
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
      if (typecode == COMPLEX_TYPE)
	{
	  tree real, imag;

	  arg = stabilize_reference (arg);
	  real = build_unary_op (REALPART_EXPR, arg, 1);
	  imag = build_unary_op (IMAGPART_EXPR, arg, 1);
	  return build (COMPLEX_EXPR, TREE_TYPE (arg),
			build_unary_op (code, real, 1), imag);
	}

      /* Report invalid types.  */

      if (typecode != POINTER_TYPE
	  && typecode != INTEGER_TYPE && typecode != REAL_TYPE)
	{
	  error ("wrong type argument to %s",
		 code == PREINCREMENT_EXPR || code == POSTINCREMENT_EXPR
		 ? "increment" : "decrement");
	  return error_mark_node;
	}

      {
	register tree inc;
	tree result_type = TREE_TYPE (arg);

	arg = get_unwidened (arg, 0);
	argtype = TREE_TYPE (arg);

	/* Compute the increment.  */

	if (typecode == POINTER_TYPE)
	  {
	    /* If pointer target is an undefined struct,
	       we just cannot know how to do the arithmetic.  */
	    if (TYPE_SIZE (TREE_TYPE (result_type)) == 0)
	      error ("%s of pointer to unknown structure",
		     code == PREINCREMENT_EXPR || code == POSTINCREMENT_EXPR
		     ? "increment" : "decrement");
	    else if ((pedantic || warn_pointer_arith)
		     && (TREE_CODE (TREE_TYPE (result_type)) == FUNCTION_TYPE
			 || TREE_CODE (TREE_TYPE (result_type)) == VOID_TYPE))
	      pedwarn ("wrong type argument to %s",
		       code == PREINCREMENT_EXPR || code == POSTINCREMENT_EXPR
		       ? "increment" : "decrement");
	    inc = c_size_in_bytes (TREE_TYPE (result_type));
	  }
	else
	  inc = integer_one_node;

	inc = convert (argtype, inc);

	/* Handle incrementing a cast-expression.  */

	while (1)
	  switch (TREE_CODE (arg))
	    {
	    case NOP_EXPR:
	    case CONVERT_EXPR:
	    case FLOAT_EXPR:
	    case FIX_TRUNC_EXPR:
	    case FIX_FLOOR_EXPR:
	    case FIX_ROUND_EXPR:
	    case FIX_CEIL_EXPR:
	      pedantic_lvalue_warning (CONVERT_EXPR);
	      /* If the real type has the same machine representation
		 as the type it is cast to, we can make better output
		 by adding directly to the inside of the cast.  */
	      if ((TREE_CODE (TREE_TYPE (arg))
		   == TREE_CODE (TREE_TYPE (TREE_OPERAND (arg, 0))))
		  && (TYPE_MODE (TREE_TYPE (arg))
		      == TYPE_MODE (TREE_TYPE (TREE_OPERAND (arg, 0)))))
		arg = TREE_OPERAND (arg, 0);
	      else
		{
		  tree incremented, modify, value;
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
		  value = build (COMPOUND_EXPR, TREE_TYPE (arg), modify, value);
		  TREE_USED (value) = 1;
		  return value;
		}
	      break;

	    default:
	      goto give_up;
	    }
      give_up:

	/* Complain about anything else that is not a true lvalue.  */
	if (!lvalue_or_else (arg, ((code == PREINCREMENT_EXPR
				    || code == POSTINCREMENT_EXPR)
				   ? "invalid lvalue in increment"
				   : "invalid lvalue in decrement")))
	  return error_mark_node;

	/* Report a read-only lvalue.  */
	if (TREE_READONLY (arg))
	  readonly_warning (arg, 
			    ((code == PREINCREMENT_EXPR
			      || code == POSTINCREMENT_EXPR)
			     ? "increment" : "decrement"));

	val = build (code, TREE_TYPE (arg), arg, inc);
	TREE_SIDE_EFFECTS (val) = 1;
	val = convert (result_type, val);
	if (TREE_CODE (val) != code)
	  TREE_NO_UNUSED_WARNING (val) = 1;
	return val;
      }

    case ADDR_EXPR:
      /* Note that this operation never does default_conversion
	 regardless of NOCONVERT.  */

      /* Let &* cancel out to simplify resulting code.  */
      if (TREE_CODE (arg) == INDIRECT_REF)
	{
	  /* Don't let this be an lvalue.  */
	  if (lvalue_p (TREE_OPERAND (arg, 0)))
	    return non_lvalue (TREE_OPERAND (arg, 0));
	  return TREE_OPERAND (arg, 0);
	}

      /* For &x[y], return x+y */
      if (TREE_CODE (arg) == ARRAY_REF)
	{
	  if (mark_addressable (TREE_OPERAND (arg, 0)) == 0)
	    return error_mark_node;
	  return build_binary_op (PLUS_EXPR, TREE_OPERAND (arg, 0),
				  TREE_OPERAND (arg, 1), 1);
	}

      /* Handle complex lvalues (when permitted)
	 by reduction to simpler cases.  */
      val = unary_complex_lvalue (code, arg);
      if (val != 0)
	return val;

#if 0 /* Turned off because inconsistent;
	 float f; *&(int)f = 3.4 stores in int format
	 whereas (int)f = 3.4 stores in float format.  */
      /* Address of a cast is just a cast of the address
	 of the operand of the cast.  */
      switch (TREE_CODE (arg))
	{
	case NOP_EXPR:
	case CONVERT_EXPR:
	case FLOAT_EXPR:
	case FIX_TRUNC_EXPR:
	case FIX_FLOOR_EXPR:
	case FIX_ROUND_EXPR:
	case FIX_CEIL_EXPR:
	  if (pedantic)
	    pedwarn ("ANSI C forbids the address of a cast expression");
	  return convert (build_pointer_type (TREE_TYPE (arg)),
			  build_unary_op (ADDR_EXPR, TREE_OPERAND (arg, 0),
					  0));
	}
#endif

      /* Allow the address of a constructor if all the elements
	 are constant.  */
      if (TREE_CODE (arg) == CONSTRUCTOR && TREE_CONSTANT (arg))
	;
      /* Anything not already handled and not a true memory reference
	 is an error.  */
      else if (typecode != FUNCTION_TYPE
	       && !lvalue_or_else (arg, "invalid lvalue in unary `&'"))
	return error_mark_node;

      /* Ordinary case; arg is a COMPONENT_REF or a decl.  */
      argtype = TREE_TYPE (arg);
      /* If the lvalue is const or volatile, merge that into the type
         to which the address will point.  Note that you can't get a
	 restricted pointer by taking the address of something, so we
	 only have to deal with `const' and `volatile' here.  */
      if (TREE_CODE_CLASS (TREE_CODE (arg)) == 'd'
	  || TREE_CODE_CLASS (TREE_CODE (arg)) == 'r')
	{
	  if (TREE_READONLY (arg) || TREE_THIS_VOLATILE (arg))
	    argtype = c_build_type_variant (argtype,
					    TREE_READONLY (arg),
					    TREE_THIS_VOLATILE (arg));
	}

      argtype = build_pointer_type (argtype);

      if (mark_addressable (arg) == 0)
	return error_mark_node;

      {
	tree addr;

	if (TREE_CODE (arg) == COMPONENT_REF)
	  {
	    tree field = TREE_OPERAND (arg, 1);

	    addr = build_unary_op (ADDR_EXPR, TREE_OPERAND (arg, 0), 0);

	    if (DECL_C_BIT_FIELD (field))
	      {
		error ("attempt to take address of bit-field structure member `%s'",
		       IDENTIFIER_POINTER (DECL_NAME (field)));
		return error_mark_node;
	      }

	    addr = convert (argtype, addr);

	    if (! integer_zerop (DECL_FIELD_BITPOS (field)))
	      {
		tree offset
		  = size_binop (EASY_DIV_EXPR, DECL_FIELD_BITPOS (field),
				size_int (BITS_PER_UNIT));
		int flag = TREE_CONSTANT (addr);
		addr = fold (build (PLUS_EXPR, argtype,
				    addr, convert (argtype, offset)));
		TREE_CONSTANT (addr) = flag;
	      }
	  }
	else
	  addr = build1 (code, argtype, arg);

	/* Address of a static or external variable or
	   file-scope function counts as a constant.  */
	if (staticp (arg)
	    && ! (TREE_CODE (arg) == FUNCTION_DECL
		  && DECL_CONTEXT (arg) != 0))
	  TREE_CONSTANT (addr) = 1;
	return addr;
      }

    default:
      break;
    }

  if (argtype == 0)
    argtype = TREE_TYPE (arg);
  return fold (build1 (code, argtype, arg));
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
      return convert (TREE_TYPE (conversions),
		      convert_sequence (TREE_OPERAND (conversions, 0),
					arg));

    default:
      return arg;
    }
}
#endif /* 0 */

/* Return nonzero if REF is an lvalue valid for this language.
   Lvalues can be assigned, unless their type has TYPE_READONLY.
   Lvalues can have their address taken, unless they have DECL_REGISTER.  */

int
lvalue_p (ref)
     tree ref;
{
  register enum tree_code code = TREE_CODE (ref);

  switch (code)
    {
    case REALPART_EXPR:
    case IMAGPART_EXPR:
    case COMPONENT_REF:
      return lvalue_p (TREE_OPERAND (ref, 0));

    case STRING_CST:
      return 1;

    case INDIRECT_REF:
    case ARRAY_REF:
    case VAR_DECL:
    case PARM_DECL:
    case RESULT_DECL:
    case ERROR_MARK:
      return (TREE_CODE (TREE_TYPE (ref)) != FUNCTION_TYPE
	      && TREE_CODE (TREE_TYPE (ref)) != METHOD_TYPE);

    case BIND_EXPR:
    case RTL_EXPR:
      return TREE_CODE (TREE_TYPE (ref)) == ARRAY_TYPE;

    default:
      return 0;
    }
}

/* Return nonzero if REF is an lvalue valid for this language;
   otherwise, print an error message and return zero.  */

int
lvalue_or_else (ref, msgid)
     tree ref;
     const char *msgid;
{
  int win = lvalue_p (ref);

  if (! win)
    error ("%s", msgid);

  return win;
}

/* Apply unary lvalue-demanding operator CODE to the expression ARG
   for certain kinds of expressions which are not really lvalues
   but which we can accept as lvalues.

   If ARG is not a kind of expression we can handle, return zero.  */
   
static tree
unary_complex_lvalue (code, arg)
     enum tree_code code;
     tree arg;
{
  /* Handle (a, b) used as an "lvalue".  */
  if (TREE_CODE (arg) == COMPOUND_EXPR)
    {
      tree real_result = build_unary_op (code, TREE_OPERAND (arg, 1), 0);

      /* If this returns a function type, it isn't really being used as
	 an lvalue, so don't issue a warning about it.  */
      if (TREE_CODE (TREE_TYPE (arg)) != FUNCTION_TYPE)
	pedantic_lvalue_warning (COMPOUND_EXPR);

      return build (COMPOUND_EXPR, TREE_TYPE (real_result),
		    TREE_OPERAND (arg, 0), real_result);
    }

  /* Handle (a ? b : c) used as an "lvalue".  */
  if (TREE_CODE (arg) == COND_EXPR)
    {
      pedantic_lvalue_warning (COND_EXPR);
      if (TREE_CODE (TREE_TYPE (arg)) != FUNCTION_TYPE)
	pedantic_lvalue_warning (COMPOUND_EXPR);

      return (build_conditional_expr
	      (TREE_OPERAND (arg, 0),
	       build_unary_op (code, TREE_OPERAND (arg, 1), 0),
	       build_unary_op (code, TREE_OPERAND (arg, 2), 0)));
    }

  return 0;
}

/* If pedantic, warn about improper lvalue.   CODE is either COND_EXPR
   COMPOUND_EXPR, or CONVERT_EXPR (for casts).  */

static void
pedantic_lvalue_warning (code)
     enum tree_code code;
{
  if (pedantic)
    switch (code)
      {
      case COND_EXPR:
	pedwarn ("ANSI C forbids use of conditional expressions as lvalues");
	break;
      case COMPOUND_EXPR:
	pedwarn ("ANSI C forbids use of compound expressions as lvalues");
	break;
      default:
	pedwarn ("ANSI C forbids use of cast expressions as lvalues");
	break;
      }
}

/* Warn about storing in something that is `const'.  */

void
readonly_warning (arg, msgid)
     tree arg;
     const char *msgid;
{
  /* Forbid assignments to iterators.  */
  if (TREE_CODE (arg) == VAR_DECL && ITERATOR_P (arg))
    pedwarn ("%s of iterator `%s'",  _(msgid), 
	     IDENTIFIER_POINTER (DECL_NAME (arg)));

  if (TREE_CODE (arg) == COMPONENT_REF)
    {
      if (TYPE_READONLY (TREE_TYPE (TREE_OPERAND (arg, 0))))
	readonly_warning (TREE_OPERAND (arg, 0), msgid);
      else
	pedwarn ("%s of read-only member `%s'", _(msgid),
		 IDENTIFIER_POINTER (DECL_NAME (TREE_OPERAND (arg, 1))));
    }
  else if (TREE_CODE (arg) == VAR_DECL)
    pedwarn ("%s of read-only variable `%s'", _(msgid),
	     IDENTIFIER_POINTER (DECL_NAME (arg)));
  else
    pedwarn ("%s of read-only location", _(msgid));
}

/* Mark EXP saying that we need to be able to take the
   address of it; it should not be allocated in a register.
   Value is 1 if successful.  */

int
mark_addressable (exp)
     tree exp;
{
  register tree x = exp;
  while (1)
    switch (TREE_CODE (x))
      {
      case COMPONENT_REF:
	if (DECL_C_BIT_FIELD (TREE_OPERAND (x, 1)))
	  {
	    error ("cannot take address of bitfield `%s'",
		   IDENTIFIER_POINTER (DECL_NAME (TREE_OPERAND (x, 1))));
	    return 0;
	  }

	/* ... fall through ...  */

      case ADDR_EXPR:
      case ARRAY_REF:
      case REALPART_EXPR:
      case IMAGPART_EXPR:
	x = TREE_OPERAND (x, 0);
	break;

      case CONSTRUCTOR:
	TREE_ADDRESSABLE (x) = 1;
	return 1;

      case VAR_DECL:
      case CONST_DECL:
      case PARM_DECL:
      case RESULT_DECL:
	if (DECL_REGISTER (x) && !TREE_ADDRESSABLE (x)
	    && DECL_NONLOCAL (x))
	  {
	    if (TREE_PUBLIC (x))
	      {
		error ("global register variable `%s' used in nested function",
		       IDENTIFIER_POINTER (DECL_NAME (x)));
		return 0;
	      }
	    pedwarn ("register variable `%s' used in nested function",
		     IDENTIFIER_POINTER (DECL_NAME (x)));
	  }
	else if (DECL_REGISTER (x) && !TREE_ADDRESSABLE (x))
	  {
	    if (TREE_PUBLIC (x))
	      {
		error ("address of global register variable `%s' requested",
		       IDENTIFIER_POINTER (DECL_NAME (x)));
		return 0;
	      }

	    /* If we are making this addressable due to its having
	       volatile components, give a different error message.  Also
	       handle the case of an unnamed parameter by not trying
	       to give the name.  */

	    else if (C_TYPE_FIELDS_VOLATILE (TREE_TYPE (x)))
	      {
		error ("cannot put object with volatile field into register");
		return 0;
	      }

	    pedwarn ("address of register variable `%s' requested",
		     IDENTIFIER_POINTER (DECL_NAME (x)));
	  }
	put_var_into_stack (x);

	/* drops in */
      case FUNCTION_DECL:
	TREE_ADDRESSABLE (x) = 1;
#if 0  /* poplevel deals with this now.  */
	if (DECL_CONTEXT (x) == 0)
	  TREE_ADDRESSABLE (DECL_ASSEMBLER_NAME (x)) = 1;
#endif

      default:
	return 1;
    }
}

/* Build and return a conditional expression IFEXP ? OP1 : OP2.  */

tree
build_conditional_expr (ifexp, op1, op2)
     tree ifexp, op1, op2;
{
  register tree type1;
  register tree type2;
  register enum tree_code code1;
  register enum tree_code code2;
  register tree result_type = NULL;
  tree orig_op1 = op1, orig_op2 = op2;

  ifexp = truthvalue_conversion (default_conversion (ifexp));

#if 0 /* Produces wrong result if within sizeof.  */
  /* Don't promote the operands separately if they promote
     the same way.  Return the unpromoted type and let the combined
     value get promoted if necessary.  */

  if (TREE_TYPE (op1) == TREE_TYPE (op2)
      && TREE_CODE (TREE_TYPE (op1)) != ARRAY_TYPE
      && TREE_CODE (TREE_TYPE (op1)) != ENUMERAL_TYPE
      && TREE_CODE (TREE_TYPE (op1)) != FUNCTION_TYPE)
    {
      if (TREE_CODE (ifexp) == INTEGER_CST)
	return pedantic_non_lvalue (integer_zerop (ifexp) ? op2 : op1);

      return fold (build (COND_EXPR, TREE_TYPE (op1), ifexp, op1, op2));
    }
#endif

  /* Promote both alternatives.  */

  if (TREE_CODE (TREE_TYPE (op1)) != VOID_TYPE)
    op1 = default_conversion (op1);
  if (TREE_CODE (TREE_TYPE (op2)) != VOID_TYPE)
    op2 = default_conversion (op2);

  if (TREE_CODE (ifexp) == ERROR_MARK
      || TREE_CODE (TREE_TYPE (op1)) == ERROR_MARK
      || TREE_CODE (TREE_TYPE (op2)) == ERROR_MARK)
    return error_mark_node;

  type1 = TREE_TYPE (op1);
  code1 = TREE_CODE (type1);
  type2 = TREE_TYPE (op2);
  code2 = TREE_CODE (type2);
      
  /* Quickly detect the usual case where op1 and op2 have the same type
     after promotion.  */
  if (TYPE_MAIN_VARIANT (type1) == TYPE_MAIN_VARIANT (type2))
    {
      if (type1 == type2)
	result_type = type1;
      else
	result_type = TYPE_MAIN_VARIANT (type1);
    }
  else if ((code1 == INTEGER_TYPE || code1 == REAL_TYPE)
           && (code2 == INTEGER_TYPE || code2 == REAL_TYPE))
    {
      result_type = common_type (type1, type2);

      /* If -Wsign-compare, warn here if type1 and type2 have
	 different signedness.  We'll promote the signed to unsigned
	 and later code won't know it used to be different.
	 Do this check on the original types, so that explicit casts
	 will be considered, but default promotions won't.  */
      if ((warn_sign_compare < 0 ? extra_warnings : warn_sign_compare)
	  && !skip_evaluation)
	{
	  int unsigned_op1 = TREE_UNSIGNED (TREE_TYPE (orig_op1));
	  int unsigned_op2 = TREE_UNSIGNED (TREE_TYPE (orig_op2));

	  if (unsigned_op1 ^ unsigned_op2)
	    {
	      /* Do not warn if the result type is signed, since the
		 signed type will only be chosen if it can represent
		 all the values of the unsigned type.  */
	      if (! TREE_UNSIGNED (result_type))
		/* OK */;
	      /* Do not warn if the signed quantity is an unsuffixed
		 integer literal (or some static constant expression
		 involving such literals) and it is non-negative.  */
	      else if ((unsigned_op2 && TREE_CODE (op1) == INTEGER_CST
			&& tree_int_cst_sgn (op1) >= 0)
		       || (unsigned_op1 && TREE_CODE (op2) == INTEGER_CST
			   && tree_int_cst_sgn (op2) >= 0))
		/* OK */;
	      else
		warning ("signed and unsigned type in conditional expression");
	    }
	}
    }
  else if (code1 == VOID_TYPE || code2 == VOID_TYPE)
    {
      if (pedantic && (code1 != VOID_TYPE || code2 != VOID_TYPE))
	pedwarn ("ANSI C forbids conditional expr with only one void side");
      result_type = void_type_node;
    }
  else if (code1 == POINTER_TYPE && code2 == POINTER_TYPE)
    {
      if (comp_target_types (type1, type2))
	result_type = common_type (type1, type2);
      else if (integer_zerop (op1) && TREE_TYPE (type1) == void_type_node
	       && TREE_CODE (orig_op1) != NOP_EXPR)
	result_type = qualify_type (type2, type1);
      else if (integer_zerop (op2) && TREE_TYPE (type2) == void_type_node
	       && TREE_CODE (orig_op2) != NOP_EXPR)
	result_type = qualify_type (type1, type2);
      else if (TYPE_MAIN_VARIANT (TREE_TYPE (type1)) == void_type_node)
	{
	  if (pedantic && TREE_CODE (TREE_TYPE (type2)) == FUNCTION_TYPE)
	    pedwarn ("ANSI C forbids conditional expr between `void *' and function pointer");
	  result_type = qualify_type (type1, type2);
	}
      else if (TYPE_MAIN_VARIANT (TREE_TYPE (type2)) == void_type_node)
	{
	  if (pedantic && TREE_CODE (TREE_TYPE (type1)) == FUNCTION_TYPE)
	    pedwarn ("ANSI C forbids conditional expr between `void *' and function pointer");
	  result_type = qualify_type (type2, type1);
	}
      else
	{
	  pedwarn ("pointer type mismatch in conditional expression");
	  result_type = build_pointer_type (void_type_node);
	}
    }
  else if (code1 == POINTER_TYPE && code2 == INTEGER_TYPE)
    {
      if (! integer_zerop (op2))
	pedwarn ("pointer/integer type mismatch in conditional expression");
      else
	{
	  op2 = null_pointer_node;
#if 0  /* The spec seems to say this is permitted.  */
	  if (pedantic && TREE_CODE (type1) == FUNCTION_TYPE)
	    pedwarn ("ANSI C forbids conditional expr between 0 and function pointer");
#endif
	}
      result_type = type1;
    }
  else if (code2 == POINTER_TYPE && code1 == INTEGER_TYPE)
    {
      if (!integer_zerop (op1))
	pedwarn ("pointer/integer type mismatch in conditional expression");
      else
	{
	  op1 = null_pointer_node;
#if 0  /* The spec seems to say this is permitted.  */
	  if (pedantic && TREE_CODE (type2) == FUNCTION_TYPE)
	    pedwarn ("ANSI C forbids conditional expr between 0 and function pointer");
#endif
	}
      result_type = type2;
    }

  if (!result_type)
    {
      if (flag_cond_mismatch)
	result_type = void_type_node;
      else
	{
	  error ("type mismatch in conditional expression");
	  return error_mark_node;
	}
    }

  /* Merge const and volatile flags of the incoming types.  */
  result_type
    = build_type_variant (result_type,
			  TREE_READONLY (op1) || TREE_READONLY (op2),
			  TREE_THIS_VOLATILE (op1) || TREE_THIS_VOLATILE (op2));

  if (result_type != TREE_TYPE (op1))
    op1 = convert_and_check (result_type, op1);
  if (result_type != TREE_TYPE (op2))
    op2 = convert_and_check (result_type, op2);
    
#if 0
  if (code1 == RECORD_TYPE || code1 == UNION_TYPE)
    {
      result_type = TREE_TYPE (op1);
      if (TREE_CONSTANT (ifexp))
	return pedantic_non_lvalue (integer_zerop (ifexp) ? op2 : op1);

      if (TYPE_MODE (result_type) == BLKmode)
	{
	  register tree tempvar
	    = build_decl (VAR_DECL, NULL_TREE, result_type);
	  register tree xop1 = build_modify_expr (tempvar, op1);
	  register tree xop2 = build_modify_expr (tempvar, op2);
	  register tree result = fold (build (COND_EXPR, result_type,
					      ifexp, xop1, xop2));

	  layout_decl (tempvar, TYPE_ALIGN (result_type));
	  /* No way to handle variable-sized objects here.
	     I fear that the entire handling of BLKmode conditional exprs
	     needs to be redone.  */
	  if (TREE_CODE (DECL_SIZE (tempvar)) != INTEGER_CST)
	    abort ();
	  DECL_RTL (tempvar)
	    = assign_stack_local (DECL_MODE (tempvar),
				  (TREE_INT_CST_LOW (DECL_SIZE (tempvar))
				   + BITS_PER_UNIT - 1)
				  / BITS_PER_UNIT,
				  0);

	  TREE_SIDE_EFFECTS (result)
	    = TREE_SIDE_EFFECTS (ifexp) | TREE_SIDE_EFFECTS (op1)
	      | TREE_SIDE_EFFECTS (op2);
	  return build (COMPOUND_EXPR, result_type, result, tempvar);
	}
    }
#endif /* 0 */
    
  if (TREE_CODE (ifexp) == INTEGER_CST)
    return pedantic_non_lvalue (integer_zerop (ifexp) ? op2 : op1);

  return fold (build (COND_EXPR, result_type, ifexp, op1, op2));
}

/* Given a list of expressions, return a compound expression
   that performs them all and returns the value of the last of them.  */

tree
build_compound_expr (list)
     tree list;
{
  return internal_build_compound_expr (list, TRUE);
}

static tree
internal_build_compound_expr (list, first_p)
     tree list;
     int first_p;
{
  register tree rest;

  if (TREE_CHAIN (list) == 0)
    {
#if 0 /* If something inside inhibited lvalueness, we should not override.  */
      /* Consider (x, y+0), which is not an lvalue since y+0 is not.  */

      /* Strip NON_LVALUE_EXPRs since we aren't using as an lvalue.  */
      if (TREE_CODE (list) == NON_LVALUE_EXPR)
	list = TREE_OPERAND (list, 0);
#endif

      /* Don't let (0, 0) be null pointer constant.  */
      if (!first_p && integer_zerop (TREE_VALUE (list)))
	return non_lvalue (TREE_VALUE (list));
      return TREE_VALUE (list);
    }

  if (TREE_CHAIN (list) != 0 && TREE_CHAIN (TREE_CHAIN (list)) == 0)
    {
      /* Convert arrays to pointers when there really is a comma operator.  */
      if (TREE_CODE (TREE_TYPE (TREE_VALUE (TREE_CHAIN (list)))) == ARRAY_TYPE)
	TREE_VALUE (TREE_CHAIN (list))
	  = default_conversion (TREE_VALUE (TREE_CHAIN (list)));
    }

  rest = internal_build_compound_expr (TREE_CHAIN (list), FALSE);

  if (! TREE_SIDE_EFFECTS (TREE_VALUE (list)))
    {
      /* The left-hand operand of a comma expression is like an expression
         statement: with -W or -Wunused, we should warn if it doesn't have
	 any side-effects, unless it was explicitly cast to (void).  */
      if ((extra_warnings || warn_unused)
           && ! (TREE_CODE (TREE_VALUE (list)) == CONVERT_EXPR
                && TREE_TYPE (TREE_VALUE (list)) == void_type_node))
        warning ("left-hand operand of comma expression has no effect");

      /* When pedantic, a compound expression can be neither an lvalue
         nor an integer constant expression.  */
      if (! pedantic)
        return rest;
    }

  /* With -Wunused, we should also warn if the left-hand operand does have
     side-effects, but computes a value which is not used.  For example, in
     `foo() + bar(), baz()' the result of the `+' operator is not used,
     so we should issue a warning.  */
  else if (warn_unused)
    warn_if_unused_value (TREE_VALUE (list));

  return build (COMPOUND_EXPR, TREE_TYPE (rest), TREE_VALUE (list), rest);
}

/* Build an expression representing a cast to type TYPE of expression EXPR.  */

tree
build_c_cast (type, expr)
     register tree type;
     tree expr;
{
  register tree value = expr;
  
  if (type == error_mark_node || expr == error_mark_node)
    return error_mark_node;
  type = TYPE_MAIN_VARIANT (type);

#if 0
  /* Strip NON_LVALUE_EXPRs since we aren't using as an lvalue.  */
  if (TREE_CODE (value) == NON_LVALUE_EXPR)
    value = TREE_OPERAND (value, 0);
#endif

  if (TREE_CODE (type) == ARRAY_TYPE)
    {
      error ("cast specifies array type");
      return error_mark_node;
    }

  if (TREE_CODE (type) == FUNCTION_TYPE)
    {
      error ("cast specifies function type");
      return error_mark_node;
    }

  if (type == TREE_TYPE (value))
    {
      if (pedantic)
	{
	  if (TREE_CODE (type) == RECORD_TYPE
	      || TREE_CODE (type) == UNION_TYPE)
	    pedwarn ("ANSI C forbids casting nonscalar to the same type");
	}
    }
  else if (TREE_CODE (type) == UNION_TYPE)
    {
      tree field;
      if (TREE_CODE (TREE_TYPE (value)) == ARRAY_TYPE
	  || TREE_CODE (TREE_TYPE (value)) == FUNCTION_TYPE)
	value = default_conversion (value);

      for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	if (comptypes (TYPE_MAIN_VARIANT (TREE_TYPE (field)),
		       TYPE_MAIN_VARIANT (TREE_TYPE (value))))
	  break;

      if (field)
	{
	  const char *name;
	  tree t;

	  if (pedantic)
	    pedwarn ("ANSI C forbids casts to union type");
	  if (TYPE_NAME (type) != 0)
	    {
	      if (TREE_CODE (TYPE_NAME (type)) == IDENTIFIER_NODE)
		name = IDENTIFIER_POINTER (TYPE_NAME (type));
	      else
		name = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type)));
	    }
	  else
	    name = "";
	  t = digest_init (type, build (CONSTRUCTOR, type, NULL_TREE,
					build_tree_list (field, value)),
			   0, 0);
	  TREE_CONSTANT (t) = TREE_CONSTANT (value);
	  return t;
	}
      error ("cast to union type from type not present in union");
      return error_mark_node;
    }
  else
    {
      tree otype, ovalue;

      /* If casting to void, avoid the error that would come
	 from default_conversion in the case of a non-lvalue array.  */
      if (type == void_type_node)
	return build1 (CONVERT_EXPR, type, value);

      /* Convert functions and arrays to pointers,
	 but don't convert any other types.  */
      if (TREE_CODE (TREE_TYPE (value)) == FUNCTION_TYPE
	  || TREE_CODE (TREE_TYPE (value)) == ARRAY_TYPE)
	value = default_conversion (value);
      otype = TREE_TYPE (value);

      /* Optionally warn about potentially worrisome casts.  */

      if (warn_cast_qual
	  && TREE_CODE (type) == POINTER_TYPE
	  && TREE_CODE (otype) == POINTER_TYPE)
	{
	  tree in_type = type;
	  tree in_otype = otype;
	  int warn = 0;

	  /* Check that the qualifiers on IN_TYPE are a superset of
	     the qualifiers of IN_OTYPE.  The outermost level of
	     POINTER_TYPE nodes is uninteresting and we stop as soon
	     as we hit a non-POINTER_TYPE node on either type.  */
	  do
	    {
	      in_otype = TREE_TYPE (in_otype);
	      in_type = TREE_TYPE (in_type);
	      warn |= (TYPE_QUALS (in_otype) & ~TYPE_QUALS (in_type));
	    }
	  while (TREE_CODE (in_type) == POINTER_TYPE
		 && TREE_CODE (in_otype) == POINTER_TYPE);

	  if (warn)
	    /* There are qualifiers present in IN_OTYPE that are not
	       present in IN_TYPE.  */
	    pedwarn ("cast discards qualifiers from pointer target type");
	}

      /* Warn about possible alignment problems.  */
      if (STRICT_ALIGNMENT && warn_cast_align
	  && TREE_CODE (type) == POINTER_TYPE
	  && TREE_CODE (otype) == POINTER_TYPE
	  && TREE_CODE (TREE_TYPE (otype)) != VOID_TYPE
	  && TREE_CODE (TREE_TYPE (otype)) != FUNCTION_TYPE
	  /* Don't warn about opaque types, where the actual alignment
	     restriction is unknown.  */
	  && !((TREE_CODE (TREE_TYPE (otype)) == UNION_TYPE
		|| TREE_CODE (TREE_TYPE (otype)) == RECORD_TYPE)
	       && TYPE_MODE (TREE_TYPE (otype)) == VOIDmode)
	  && TYPE_ALIGN (TREE_TYPE (type)) > TYPE_ALIGN (TREE_TYPE (otype)))
	warning ("cast increases required alignment of target type");

      if (TREE_CODE (type) == INTEGER_TYPE
	  && TREE_CODE (otype) == POINTER_TYPE
	  && TYPE_PRECISION (type) != TYPE_PRECISION (otype)
	  && !TREE_CONSTANT (value))
	warning ("cast from pointer to integer of different size");

      if (warn_bad_function_cast
	  && TREE_CODE (value) == CALL_EXPR
	  && TREE_CODE (type) != TREE_CODE (otype))
	warning ("cast does not match function type");

      if (TREE_CODE (type) == POINTER_TYPE
	  && TREE_CODE (otype) == INTEGER_TYPE
	  && TYPE_PRECISION (type) != TYPE_PRECISION (otype)
#if 0
	  /* Don't warn about converting 0 to pointer,
	     provided the 0 was explicit--not cast or made by folding.  */
	  && !(TREE_CODE (value) == INTEGER_CST && integer_zerop (value))
#endif
	  /* Don't warn about converting any constant.  */
	  && !TREE_CONSTANT (value))
	warning ("cast to pointer from integer of different size");

      ovalue = value;
      value = convert (type, value);

      /* Ignore any integer overflow caused by the cast.  */
      if (TREE_CODE (value) == INTEGER_CST)
	{
	  TREE_OVERFLOW (value) = TREE_OVERFLOW (ovalue);
	  TREE_CONSTANT_OVERFLOW (value) = TREE_CONSTANT_OVERFLOW (ovalue);
	}
    }

  /* Pedantically, don't ley (void *) (FOO *) 0 be a null pointer constant.  */
  if (pedantic && TREE_CODE (value) == INTEGER_CST
      && TREE_CODE (expr) == INTEGER_CST
      && TREE_CODE (TREE_TYPE (expr)) != INTEGER_TYPE)
    value = non_lvalue (value);

  /* If pedantic, don't let a cast be an lvalue.  */
  if (value == expr && pedantic)
    value = non_lvalue (value);

  return value;
}

/* Build an assignment expression of lvalue LHS from value RHS.
   MODIFYCODE is the code for a binary operator that we use
   to combine the old value of LHS with RHS to get the new value.
   Or else MODIFYCODE is NOP_EXPR meaning do a simple assignment.  */

tree
build_modify_expr (lhs, modifycode, rhs)
     tree lhs, rhs;
     enum tree_code modifycode;
{
  register tree result;
  tree newrhs;
  tree lhstype = TREE_TYPE (lhs);
  tree olhstype = lhstype;

  /* Types that aren't fully specified cannot be used in assignments.  */
  lhs = require_complete_type (lhs);

  /* Avoid duplicate error messages from operands that had errors.  */
  if (TREE_CODE (lhs) == ERROR_MARK || TREE_CODE (rhs) == ERROR_MARK)
    return error_mark_node;

  /* Strip NON_LVALUE_EXPRs since we aren't using as an lvalue.  */
  /* Do not use STRIP_NOPS here.  We do not want an enumerator
     whose value is 0 to count as a null pointer constant.  */
  if (TREE_CODE (rhs) == NON_LVALUE_EXPR)
    rhs = TREE_OPERAND (rhs, 0);

  newrhs = rhs;

  /* Handle control structure constructs used as "lvalues".  */

  switch (TREE_CODE (lhs))
    {
      /* Handle (a, b) used as an "lvalue".  */
    case COMPOUND_EXPR:
      pedantic_lvalue_warning (COMPOUND_EXPR);
      newrhs = build_modify_expr (TREE_OPERAND (lhs, 1), modifycode, rhs);
      if (TREE_CODE (newrhs) == ERROR_MARK)
	return error_mark_node;
      return build (COMPOUND_EXPR, lhstype,
		    TREE_OPERAND (lhs, 0), newrhs);
 
      /* Handle (a ? b : c) used as an "lvalue".  */
    case COND_EXPR:
      pedantic_lvalue_warning (COND_EXPR);
      rhs = save_expr (rhs);
      {
	/* Produce (a ? (b = rhs) : (c = rhs))
	   except that the RHS goes through a save-expr
	   so the code to compute it is only emitted once.  */
	tree cond
	  = build_conditional_expr (TREE_OPERAND (lhs, 0),
				    build_modify_expr (TREE_OPERAND (lhs, 1),
						       modifycode, rhs),
				    build_modify_expr (TREE_OPERAND (lhs, 2),
						       modifycode, rhs));
	if (TREE_CODE (cond) == ERROR_MARK)
	  return cond;
	/* Make sure the code to compute the rhs comes out
	   before the split.  */
	return build (COMPOUND_EXPR, TREE_TYPE (lhs),
		      /* But cast it to void to avoid an "unused" error.  */
		      convert (void_type_node, rhs), cond);
      }
    default:
      break;
    }

  /* If a binary op has been requested, combine the old LHS value with the RHS
     producing the value we should actually store into the LHS.  */

  if (modifycode != NOP_EXPR)
    {
      lhs = stabilize_reference (lhs);
      newrhs = build_binary_op (modifycode, lhs, rhs, 1);
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
	  || TREE_CODE (TREE_TYPE (newrhs)) == FUNCTION_TYPE)
	newrhs = default_conversion (newrhs);
      {
	tree inner_lhs = TREE_OPERAND (lhs, 0);
	tree result;
	result = build_modify_expr (inner_lhs, NOP_EXPR,
				    convert (TREE_TYPE (inner_lhs),
					     convert (lhstype, newrhs)));
	if (TREE_CODE (result) == ERROR_MARK)
	  return result;
	pedantic_lvalue_warning (CONVERT_EXPR);
	return convert (TREE_TYPE (lhs), result);
      }
      
    default:
      break;
    }

  /* Now we have handled acceptable kinds of LHS that are not truly lvalues.
     Reject anything strange now.  */

  if (!lvalue_or_else (lhs, "invalid lvalue in assignment"))
    return error_mark_node;

  /* Warn about storing in something that is `const'.  */

  if (TREE_READONLY (lhs) || TYPE_READONLY (lhstype)
      || ((TREE_CODE (lhstype) == RECORD_TYPE
	   || TREE_CODE (lhstype) == UNION_TYPE)
	  && C_TYPE_FIELDS_READONLY (lhstype)))
    readonly_warning (lhs, "assignment");

  /* If storing into a structure or union member,
     it has probably been given type `int'.
     Compute the type that would go with
     the actual amount of storage the member occupies.  */

  if (TREE_CODE (lhs) == COMPONENT_REF
      && (TREE_CODE (lhstype) == INTEGER_TYPE
	  || TREE_CODE (lhstype) == REAL_TYPE
	  || TREE_CODE (lhstype) == ENUMERAL_TYPE))
    lhstype = TREE_TYPE (get_unwidened (lhs, 0));

  /* If storing in a field that is in actuality a short or narrower than one,
     we must store in the field in its actual type.  */

  if (lhstype != TREE_TYPE (lhs))
    {
      lhs = copy_node (lhs);
      TREE_TYPE (lhs) = lhstype;
    }

  /* Convert new value to destination type.  */

  newrhs = convert_for_assignment (lhstype, newrhs, _("assignment"),
				   NULL_TREE, NULL_TREE, 0);
  if (TREE_CODE (newrhs) == ERROR_MARK)
    return error_mark_node;

  result = build (MODIFY_EXPR, lhstype, lhs, newrhs);
  TREE_SIDE_EFFECTS (result) = 1;

  /* If we got the LHS in a different type for storing in,
     convert the result back to the nominal type of LHS
     so that the value we return always has the same type
     as the LHS argument.  */

  if (olhstype == TREE_TYPE (result))
    return result;
  return convert_for_assignment (olhstype, result, _("assignment"),
				 NULL_TREE, NULL_TREE, 0);
}

/* Convert value RHS to type TYPE as preparation for an assignment
   to an lvalue of type TYPE.
   The real work of conversion is done by `convert'.
   The purpose of this function is to generate error messages
   for assignments that are not allowed in C.
   ERRTYPE is a string to use in error messages:
   "assignment", "return", etc.  If it is null, this is parameter passing
   for a function call (and different error messages are output).

   FUNNAME is the name of the function being called,
   as an IDENTIFIER_NODE, or null.
   PARMNUM is the number of the argument, for printing in error messages.  */

static tree
convert_for_assignment (type, rhs, errtype, fundecl, funname, parmnum)
     tree type, rhs;
     const char *errtype;
     tree fundecl, funname;
     int parmnum;
{
  register enum tree_code codel = TREE_CODE (type);
  register tree rhstype;
  register enum tree_code coder;

  /* Strip NON_LVALUE_EXPRs since we aren't using as an lvalue.  */
  /* Do not use STRIP_NOPS here.  We do not want an enumerator
     whose value is 0 to count as a null pointer constant.  */
  if (TREE_CODE (rhs) == NON_LVALUE_EXPR)
    rhs = TREE_OPERAND (rhs, 0);

  if (TREE_CODE (TREE_TYPE (rhs)) == ARRAY_TYPE
      || TREE_CODE (TREE_TYPE (rhs)) == FUNCTION_TYPE)
    rhs = default_conversion (rhs);
  else if (optimize && TREE_CODE (rhs) == VAR_DECL)
    rhs = decl_constant_value (rhs);

  rhstype = TREE_TYPE (rhs);
  coder = TREE_CODE (rhstype);

  if (coder == ERROR_MARK)
    return error_mark_node;

  if (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (rhstype))
    {
      overflow_warning (rhs);
      /* Check for Objective-C protocols.  This will issue a warning if
	 there are protocol violations.  No need to use the return value.  */
      maybe_objc_comptypes (type, rhstype, 0);
      return rhs;
    }

  if (coder == VOID_TYPE)
    {
      error ("void value not ignored as it ought to be");
      return error_mark_node;
    }
  /* Arithmetic types all interconvert, and enum is treated like int.  */
  if ((codel == INTEGER_TYPE || codel == REAL_TYPE || codel == ENUMERAL_TYPE
       || codel == COMPLEX_TYPE)
      && (coder == INTEGER_TYPE || coder == REAL_TYPE || coder == ENUMERAL_TYPE
	  || coder == COMPLEX_TYPE))
    return convert_and_check (type, rhs);

  /* Conversion to a transparent union from its member types.
     This applies only to function arguments.  */
  else if (codel == UNION_TYPE && TYPE_TRANSPARENT_UNION (type) && ! errtype)
    {
      tree memb_types;
      tree marginal_memb_type = 0;

      for (memb_types = TYPE_FIELDS (type); memb_types;
	   memb_types = TREE_CHAIN (memb_types))
	{
	  tree memb_type = TREE_TYPE (memb_types);

	  if (comptypes (TYPE_MAIN_VARIANT (memb_type),
			 TYPE_MAIN_VARIANT (rhstype)))
	    break;

	  if (TREE_CODE (memb_type) != POINTER_TYPE)
	    continue;

	  if (coder == POINTER_TYPE)
	    {
	      register tree ttl = TREE_TYPE (memb_type);
	      register tree ttr = TREE_TYPE (rhstype);

	      /* Any non-function converts to a [const][volatile] void *
		 and vice versa; otherwise, targets must be the same.
		 Meanwhile, the lhs target must have all the qualifiers of
		 the rhs.  */
	      if (TYPE_MAIN_VARIANT (ttl) == void_type_node
		  || TYPE_MAIN_VARIANT (ttr) == void_type_node
		  || comp_target_types (memb_type, rhstype))
		{
		  /* If this type won't generate any warnings, use it.  */
		  if (TYPE_QUALS (ttl) == TYPE_QUALS (ttr)
		      || ((TREE_CODE (ttr) == FUNCTION_TYPE
			   && TREE_CODE (ttl) == FUNCTION_TYPE)
			  ? ((TYPE_QUALS (ttl) | TYPE_QUALS (ttr))
			     == TYPE_QUALS (ttr))
			  : ((TYPE_QUALS (ttl) | TYPE_QUALS (ttr))
			     == TYPE_QUALS (ttl))))
		    break;

		  /* Keep looking for a better type, but remember this one.  */
		  if (! marginal_memb_type)
		    marginal_memb_type = memb_type;
		}
	    }

	  /* Can convert integer zero to any pointer type.  */
	  if (integer_zerop (rhs)
	      || (TREE_CODE (rhs) == NOP_EXPR
		  && integer_zerop (TREE_OPERAND (rhs, 0))))
	    {
	      rhs = null_pointer_node;
	      break;
	    }
	}

      if (memb_types || marginal_memb_type)
	{
	  if (! memb_types)
	    {
	      /* We have only a marginally acceptable member type;
		 it needs a warning.  */
	      register tree ttl = TREE_TYPE (marginal_memb_type);
	      register tree ttr = TREE_TYPE (rhstype);

	      /* Const and volatile mean something different for function
		 types, so the usual warnings are not appropriate.  */
	      if (TREE_CODE (ttr) == FUNCTION_TYPE
		  && TREE_CODE (ttl) == FUNCTION_TYPE)
		{
		  /* Because const and volatile on functions are
		     restrictions that say the function will not do
		     certain things, it is okay to use a const or volatile
		     function where an ordinary one is wanted, but not
		     vice-versa.  */
		  if (TYPE_QUALS (ttl) & ~TYPE_QUALS (ttr))
		    warn_for_assignment ("%s makes qualified function pointer from unqualified",
					 errtype, funname, parmnum);
		}
	      else if (TYPE_QUALS (ttr) & ~TYPE_QUALS (ttl))
		warn_for_assignment ("%s discards qualifiers from pointer target type",
				     errtype, funname,
				     parmnum);
	    }
	  
	  if (pedantic && ! DECL_IN_SYSTEM_HEADER (fundecl))
	    pedwarn ("ANSI C prohibits argument conversion to union type");

	  return build1 (NOP_EXPR, type, rhs);
	}
    }

  /* Conversions among pointers */
  else if (codel == POINTER_TYPE && coder == POINTER_TYPE)
    {
      register tree ttl = TREE_TYPE (type);
      register tree ttr = TREE_TYPE (rhstype);

      /* Any non-function converts to a [const][volatile] void *
	 and vice versa; otherwise, targets must be the same.
	 Meanwhile, the lhs target must have all the qualifiers of the rhs.  */
      if (TYPE_MAIN_VARIANT (ttl) == void_type_node
	  || TYPE_MAIN_VARIANT (ttr) == void_type_node
	  || comp_target_types (type, rhstype)
	  || (unsigned_type (TYPE_MAIN_VARIANT (ttl))
	      == unsigned_type (TYPE_MAIN_VARIANT (ttr))))
	{
	  if (pedantic
	      && ((TYPE_MAIN_VARIANT (ttl) == void_type_node
		   && TREE_CODE (ttr) == FUNCTION_TYPE)
		  ||
		  (TYPE_MAIN_VARIANT (ttr) == void_type_node
		   /* Check TREE_CODE to catch cases like (void *) (char *) 0
		      which are not ANSI null ptr constants.  */
		   && (!integer_zerop (rhs) || TREE_CODE (rhs) == NOP_EXPR)
		   && TREE_CODE (ttl) == FUNCTION_TYPE)))
	    warn_for_assignment ("ANSI forbids %s between function pointer and `void *'",
				 errtype, funname, parmnum);
	  /* Const and volatile mean something different for function types,
	     so the usual warnings are not appropriate.  */
	  else if (TREE_CODE (ttr) != FUNCTION_TYPE
		   && TREE_CODE (ttl) != FUNCTION_TYPE)
	    {
	      if (TYPE_QUALS (ttr) & ~TYPE_QUALS (ttl))
		warn_for_assignment ("%s discards qualifiers from pointer target type",
				     errtype, funname, parmnum);
	      /* If this is not a case of ignoring a mismatch in signedness,
		 no warning.  */
	      else if (TYPE_MAIN_VARIANT (ttl) == void_type_node
		       || TYPE_MAIN_VARIANT (ttr) == void_type_node
		       || comp_target_types (type, rhstype))
		;
	      /* If there is a mismatch, do warn.  */
	      else if (pedantic)
		warn_for_assignment ("pointer targets in %s differ in signedness",
				     errtype, funname, parmnum);
	    }
	  else if (TREE_CODE (ttl) == FUNCTION_TYPE
		   && TREE_CODE (ttr) == FUNCTION_TYPE)
	    {
	      /* Because const and volatile on functions are restrictions
		 that say the function will not do certain things,
		 it is okay to use a const or volatile function
		 where an ordinary one is wanted, but not vice-versa.  */
	      if (TYPE_QUALS (ttl) & ~TYPE_QUALS (ttr))
		warn_for_assignment ("%s makes qualified function pointer from unqualified",
				     errtype, funname, parmnum);
	    }
	}
      else
	warn_for_assignment ("%s from incompatible pointer type",
			     errtype, funname, parmnum);
      return convert (type, rhs);
    }
  else if (codel == POINTER_TYPE && coder == INTEGER_TYPE)
    {
      /* An explicit constant 0 can convert to a pointer,
	 or one that results from arithmetic, even including
	 a cast to integer type.  */
      if (! (TREE_CODE (rhs) == INTEGER_CST && integer_zerop (rhs))
	  &&
	  ! (TREE_CODE (rhs) == NOP_EXPR
	     && TREE_CODE (TREE_TYPE (rhs)) == INTEGER_TYPE
	     && TREE_CODE (TREE_OPERAND (rhs, 0)) == INTEGER_CST
	     && integer_zerop (TREE_OPERAND (rhs, 0))))
	{
	  warn_for_assignment ("%s makes pointer from integer without a cast",
			       errtype, funname, parmnum);
	  return convert (type, rhs);
	}
      return null_pointer_node;
    }
  else if (codel == INTEGER_TYPE && coder == POINTER_TYPE)
    {
      warn_for_assignment ("%s makes integer from pointer without a cast",
			   errtype, funname, parmnum);
      return convert (type, rhs);
    }

  if (!errtype)
    {
      if (funname)
 	{
 	  tree selector = maybe_building_objc_message_expr ();
 
 	  if (selector && parmnum > 2)
 	    error ("incompatible type for argument %d of `%s'",
		   parmnum - 2, IDENTIFIER_POINTER (selector));
 	  else
	    error ("incompatible type for argument %d of `%s'",
		   parmnum, IDENTIFIER_POINTER (funname));
	}
      else
	error ("incompatible type for argument %d of indirect function call",
	       parmnum);
    }
  else
    error ("incompatible types in %s", errtype);

  return error_mark_node;
}

/* Print a warning using MSGID.
   It gets OPNAME as its one parameter.
   If OPNAME is null, it is replaced by "passing arg ARGNUM of `FUNCTION'".
   FUNCTION and ARGNUM are handled specially if we are building an
   Objective-C selector.  */

static void
warn_for_assignment (msgid, opname, function, argnum)
     const char *msgid;
     const char *opname;
     tree function;
     int argnum;
{
  if (opname == 0)
    {
      tree selector = maybe_building_objc_message_expr ();
      char * new_opname;
      
      if (selector && argnum > 2)
	{
	  function = selector;
	  argnum -= 2;
	}
      if (function)
	{
	  /* Function name is known; supply it.  */
	  const char *argstring = _("passing arg %d of `%s'");
	  new_opname = (char *) alloca (IDENTIFIER_LENGTH (function)
					+ strlen (argstring) + 1 + 25
					/*%d*/ + 1);
	  sprintf (new_opname, argstring, argnum,
		   IDENTIFIER_POINTER (function));
	}
      else
	{
	  /* Function name unknown (call through ptr); just give arg number.*/
	  const char *argnofun = _("passing arg %d of pointer to function");
	  new_opname = (char *) alloca (strlen (argnofun) + 1 + 25 /*%d*/ + 1);
	  sprintf (new_opname, argnofun, argnum);
	}
      opname = new_opname;
    }
  pedwarn (msgid, opname);
}

/* If VALUE is a compound expr all of whose expressions are constant, then
   return its value.  Otherwise, return error_mark_node.

   This is for handling COMPOUND_EXPRs as initializer elements
   which is allowed with a warning when -pedantic is specified.  */

static tree
valid_compound_expr_initializer (value, endtype)
     tree value;
     tree endtype;
{
  if (TREE_CODE (value) == COMPOUND_EXPR)
    {
      if (valid_compound_expr_initializer (TREE_OPERAND (value, 0), endtype)
	  == error_mark_node)
	return error_mark_node;
      return valid_compound_expr_initializer (TREE_OPERAND (value, 1),
					      endtype);
    }
  else if (! TREE_CONSTANT (value)
	   && ! initializer_constant_valid_p (value, endtype))
    return error_mark_node;
  else
    return value;
}

/* Perform appropriate conversions on the initial value of a variable,
   store it in the declaration DECL,
   and print any error messages that are appropriate.
   If the init is invalid, store an ERROR_MARK.  */

void
store_init_value (decl, init)
     tree decl, init;
{
  register tree value, type;

  /* If variable's type was invalidly declared, just ignore it.  */

  type = TREE_TYPE (decl);
  if (TREE_CODE (type) == ERROR_MARK)
    return;

  /* Digest the specified initializer into an expression.  */

  value = digest_init (type, init, TREE_STATIC (decl),
		       TREE_STATIC (decl) || pedantic);

  /* Store the expression if valid; else report error.  */

#if 0
  /* Note that this is the only place we can detect the error
     in a case such as   struct foo bar = (struct foo) { x, y };
     where there is one initial value which is a constructor expression.  */
  if (value == error_mark_node)
    ;
  else if (TREE_STATIC (decl) && ! TREE_CONSTANT (value))
    {
      error ("initializer for static variable is not constant");
      value = error_mark_node;
    }
  else if (TREE_STATIC (decl)
	   && initializer_constant_valid_p (value, TREE_TYPE (value)) == 0)
    {
      error ("initializer for static variable uses complicated arithmetic");
      value = error_mark_node;
    }
  else
    {
      if (pedantic && TREE_CODE (value) == CONSTRUCTOR)
	{
	  if (! TREE_CONSTANT (value))
	    pedwarn ("aggregate initializer is not constant");
	  else if (! TREE_STATIC (value))
	    pedwarn ("aggregate initializer uses complicated arithmetic");
	}
    }
#endif

  DECL_INITIAL (decl) = value;

  /* ANSI wants warnings about out-of-range constant initializers.  */
  STRIP_TYPE_NOPS (value);
  constant_expression_warning (value);
}

/* Methods for storing and printing names for error messages.  */

/* Implement a spelling stack that allows components of a name to be pushed
   and popped.  Each element on the stack is this structure.  */

struct spelling
{
  int kind;
  union
    {
      int i;
      const char *s;
    } u;
};

#define SPELLING_STRING 1
#define SPELLING_MEMBER 2
#define SPELLING_BOUNDS 3

static struct spelling *spelling;	/* Next stack element (unused).  */
static struct spelling *spelling_base;	/* Spelling stack base.  */
static int spelling_size;		/* Size of the spelling stack.  */

/* Macros to save and restore the spelling stack around push_... functions.
   Alternative to SAVE_SPELLING_STACK.  */

#define SPELLING_DEPTH() (spelling - spelling_base)
#define RESTORE_SPELLING_DEPTH(depth) (spelling = spelling_base + depth)

/* Save and restore the spelling stack around arbitrary C code.  */

#define SAVE_SPELLING_DEPTH(code)		\
{						\
  int __depth = SPELLING_DEPTH ();		\
  code;						\
  RESTORE_SPELLING_DEPTH (__depth);		\
}

/* Push an element on the spelling stack with type KIND and assign VALUE
   to MEMBER.  */

#define PUSH_SPELLING(KIND, VALUE, MEMBER)				\
{									\
  int depth = SPELLING_DEPTH ();					\
									\
  if (depth >= spelling_size)						\
    {									\
      spelling_size += 10;						\
      if (spelling_base == 0)						\
	spelling_base							\
	  = (struct spelling *) xmalloc (spelling_size * sizeof (struct spelling));	\
      else								\
        spelling_base							\
	  = (struct spelling *) xrealloc (spelling_base,		\
					  spelling_size * sizeof (struct spelling));	\
      RESTORE_SPELLING_DEPTH (depth);					\
    }									\
									\
  spelling->kind = (KIND);						\
  spelling->MEMBER = (VALUE);						\
  spelling++;								\
}

/* Push STRING on the stack.  Printed literally.  */

static void
push_string (string)
     const char *string;
{
  PUSH_SPELLING (SPELLING_STRING, string, u.s);
}

/* Push a member name on the stack.  Printed as '.' STRING.  */

static void
push_member_name (decl)
     tree decl;
     
{
  const char *string
    = DECL_NAME (decl) ? IDENTIFIER_POINTER (DECL_NAME (decl)) : "<anonymous>";
  PUSH_SPELLING (SPELLING_MEMBER, string, u.s);
}

/* Push an array bounds on the stack.  Printed as [BOUNDS].  */

static void
push_array_bounds (bounds)
     int bounds;
{
  PUSH_SPELLING (SPELLING_BOUNDS, bounds, u.i);
}

/* Compute the maximum size in bytes of the printed spelling.  */

static int
spelling_length ()
{
  register int size = 0;
  register struct spelling *p;

  for (p = spelling_base; p < spelling; p++)
    {
      if (p->kind == SPELLING_BOUNDS)
	size += 25;
      else
	size += strlen (p->u.s) + 1;
    }

  return size;
}

/* Print the spelling to BUFFER and return it.  */

static char *
print_spelling (buffer)
     register char *buffer;
{
  register char *d = buffer;
  register struct spelling *p;

  for (p = spelling_base; p < spelling; p++)
    if (p->kind == SPELLING_BOUNDS)
      {
	sprintf (d, "[%d]", p->u.i);
	d += strlen (d);
      }
    else
      {
	register const char *s;
	if (p->kind == SPELLING_MEMBER)
	  *d++ = '.';
	for (s = p->u.s; (*d = *s++); d++)
	  ;
      }
  *d++ = '\0';
  return buffer;
}

/* Issue an error message for a bad initializer component.
   MSGID identifies the message.
   The component name is taken from the spelling stack.  */

void
error_init (msgid)
     const char *msgid;
{
  char *ofwhat;

  error ("%s", msgid);
  ofwhat = print_spelling ((char *) alloca (spelling_length () + 1));
  if (*ofwhat)
    error ("(near initialization for `%s')", ofwhat);
}

/* Issue a pedantic warning for a bad initializer component.
   MSGID identifies the message.
   The component name is taken from the spelling stack.  */

void
pedwarn_init (msgid)
     const char *msgid;
{
  char *ofwhat;

  pedwarn ("%s", msgid);
  ofwhat = print_spelling ((char *) alloca (spelling_length () + 1));
  if (*ofwhat)
    pedwarn ("(near initialization for `%s')", ofwhat);
}

/* Issue a warning for a bad initializer component.
   MSGID identifies the message.
   The component name is taken from the spelling stack.  */

static void
warning_init (msgid)
     const char *msgid;
{
  char *ofwhat;

  warning ("%s", msgid);
  ofwhat = print_spelling ((char *) alloca (spelling_length () + 1));
  if (*ofwhat)
    warning ("(near initialization for `%s')", ofwhat);
}

/* Digest the parser output INIT as an initializer for type TYPE.
   Return a C expression of type TYPE to represent the initial value.

   The arguments REQUIRE_CONSTANT and CONSTRUCTOR_CONSTANT request errors
   if non-constant initializers or elements are seen.  CONSTRUCTOR_CONSTANT
   applies only to elements of constructors.  */

static tree
digest_init (type, init, require_constant, constructor_constant)
     tree type, init;
     int require_constant, constructor_constant;
{
  enum tree_code code = TREE_CODE (type);
  tree inside_init = init;

  if (type == error_mark_node || init == error_mark_node)
    return error_mark_node;

  /* Strip NON_LVALUE_EXPRs since we aren't using as an lvalue.  */
  /* Do not use STRIP_NOPS here.  We do not want an enumerator
     whose value is 0 to count as a null pointer constant.  */
  if (TREE_CODE (init) == NON_LVALUE_EXPR)
    inside_init = TREE_OPERAND (init, 0);

  /* Initialization of an array of chars from a string constant
     optionally enclosed in braces.  */

  if (code == ARRAY_TYPE)
    {
      tree typ1 = TYPE_MAIN_VARIANT (TREE_TYPE (type));
      if ((typ1 == char_type_node
	   || typ1 == signed_char_type_node
	   || typ1 == unsigned_char_type_node
	   || typ1 == unsigned_wchar_type_node
	   || typ1 == signed_wchar_type_node)
	  && ((inside_init && TREE_CODE (inside_init) == STRING_CST)))
	{
	  if (comptypes (TYPE_MAIN_VARIANT (TREE_TYPE (inside_init)),
			 TYPE_MAIN_VARIANT (type)))
	    return inside_init;

	  if ((TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (inside_init)))
	       != char_type_node)
	      && TYPE_PRECISION (typ1) == TYPE_PRECISION (char_type_node))
	    {
	      error_init ("char-array initialized from wide string");
	      return error_mark_node;
	    }
	  if ((TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (inside_init)))
	       == char_type_node)
	      && TYPE_PRECISION (typ1) != TYPE_PRECISION (char_type_node))
	    {
	      error_init ("int-array initialized from non-wide string");
	      return error_mark_node;
	    }

	  TREE_TYPE (inside_init) = type;
	  if (TYPE_DOMAIN (type) != 0
	      && TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST)
	    {
	      register int size = TREE_INT_CST_LOW (TYPE_SIZE (type));
	      size = (size + BITS_PER_UNIT - 1) / BITS_PER_UNIT;
	      /* Subtract 1 (or sizeof (wchar_t))
		 because it's ok to ignore the terminating null char
		 that is counted in the length of the constant.  */
	      if (size < TREE_STRING_LENGTH (inside_init)
		  - (TYPE_PRECISION (typ1) != TYPE_PRECISION (char_type_node)
		     ? TYPE_PRECISION (wchar_type_node) / BITS_PER_UNIT
		     : 1))
		pedwarn_init ("initializer-string for array of chars is too long");
	    }
	  return inside_init;
	}
    }

  /* Any type can be initialized
     from an expression of the same type, optionally with braces.  */

  if (inside_init && TREE_TYPE (inside_init) != 0
      && (comptypes (TYPE_MAIN_VARIANT (TREE_TYPE (inside_init)),
		     TYPE_MAIN_VARIANT (type))
	  || (code == ARRAY_TYPE
	      && comptypes (TREE_TYPE (inside_init), type))
	  || (code == POINTER_TYPE
	      && (TREE_CODE (TREE_TYPE (inside_init)) == ARRAY_TYPE
		  || TREE_CODE (TREE_TYPE (inside_init)) == FUNCTION_TYPE)
	      && comptypes (TREE_TYPE (TREE_TYPE (inside_init)),
			    TREE_TYPE (type)))))
    {
      if (code == POINTER_TYPE
	  && (TREE_CODE (TREE_TYPE (inside_init)) == ARRAY_TYPE
	      || TREE_CODE (TREE_TYPE (inside_init)) == FUNCTION_TYPE))
	inside_init = default_conversion (inside_init);
      else if (code == ARRAY_TYPE && TREE_CODE (inside_init) != STRING_CST
	       && TREE_CODE (inside_init) != CONSTRUCTOR)
	{
	  error_init ("array initialized from non-constant array expression");
	  return error_mark_node;
	}

      if (optimize && TREE_CODE (inside_init) == VAR_DECL)
	inside_init = decl_constant_value (inside_init);

      /* Compound expressions can only occur here if -pedantic or
	 -pedantic-errors is specified.  In the later case, we always want
	 an error.  In the former case, we simply want a warning.  */
      if (require_constant && pedantic
	  && TREE_CODE (inside_init) == COMPOUND_EXPR)
	{
	  inside_init
	    = valid_compound_expr_initializer (inside_init,
					       TREE_TYPE (inside_init));
	  if (inside_init == error_mark_node)
	    error_init ("initializer element is not constant");
	  else
	    pedwarn_init ("initializer element is not constant");
	  if (flag_pedantic_errors)
	    inside_init = error_mark_node;
	}
      else if (require_constant && ! TREE_CONSTANT (inside_init))
	{
	  error_init ("initializer element is not constant");
	  inside_init = error_mark_node;
	}
      else if (require_constant
	       && initializer_constant_valid_p (inside_init, TREE_TYPE (inside_init)) == 0)
	{
	  error_init ("initializer element is not computable at load time");
	  inside_init = error_mark_node;
	}

      return inside_init;
    }

  /* Handle scalar types, including conversions.  */

  if (code == INTEGER_TYPE || code == REAL_TYPE || code == POINTER_TYPE
      || code == ENUMERAL_TYPE || code == COMPLEX_TYPE)
    {
      /* Note that convert_for_assignment calls default_conversion
	 for arrays and functions.  We must not call it in the
	 case where inside_init is a null pointer constant.  */
      inside_init
	= convert_for_assignment (type, init, _("initialization"),
				  NULL_TREE, NULL_TREE, 0);

      if (require_constant && ! TREE_CONSTANT (inside_init))
	{
	  error_init ("initializer element is not constant");
	  inside_init = error_mark_node;
	}
      else if (require_constant
	       && initializer_constant_valid_p (inside_init, TREE_TYPE (inside_init)) == 0)
	{
	  error_init ("initializer element is not computable at load time");
	  inside_init = error_mark_node;
	}

      return inside_init;
    }

  /* Come here only for records and arrays.  */

  if (TYPE_SIZE (type) && TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
    {
      error_init ("variable-sized object may not be initialized");
      return error_mark_node;
    }

  /* Traditionally, you can write  struct foo x = 0;
     and it initializes the first element of x to 0.  */
  if (flag_traditional)
    {
      tree top = 0, prev = 0, otype = type;
      while (TREE_CODE (type) == RECORD_TYPE
	     || TREE_CODE (type) == ARRAY_TYPE
	     || TREE_CODE (type) == QUAL_UNION_TYPE
	     || TREE_CODE (type) == UNION_TYPE)
	{
	  tree temp = build (CONSTRUCTOR, type, NULL_TREE, NULL_TREE);
	  if (prev == 0)
	    top = temp;
	  else
	    TREE_OPERAND (prev, 1) = build_tree_list (NULL_TREE, temp);
	  prev = temp;
	  if (TREE_CODE (type) == ARRAY_TYPE)
	    type = TREE_TYPE (type);
	  else if (TYPE_FIELDS (type))
	    type = TREE_TYPE (TYPE_FIELDS (type));
	  else
	    {
	      error_init ("invalid initializer");
	      return error_mark_node;
	    }
	}

      if (otype != type)
	{
	  TREE_OPERAND (prev, 1)
	    = build_tree_list (NULL_TREE,
			       digest_init (type, init, require_constant,
					    constructor_constant));
	  return top;
	}
      else
	return error_mark_node;
    }
  error_init ("invalid initializer");
  return error_mark_node;
}

/* Handle initializers that use braces.  */

/* Type of object we are accumulating a constructor for.
   This type is always a RECORD_TYPE, UNION_TYPE or ARRAY_TYPE.  */
static tree constructor_type;

/* For a RECORD_TYPE or UNION_TYPE, this is the chain of fields
   left to fill.  */
static tree constructor_fields;

/* For an ARRAY_TYPE, this is the specified index
   at which to store the next element we get.
   This is a special INTEGER_CST node that we modify in place.  */
static tree constructor_index;

/* For an ARRAY_TYPE, this is the end index of the range
   to initialize with the next element, or NULL in the ordinary case
   where the element is used just once.  */
static tree constructor_range_end;

/* For an ARRAY_TYPE, this is the maximum index.  */
static tree constructor_max_index;

/* For a RECORD_TYPE, this is the first field not yet written out.  */
static tree constructor_unfilled_fields;

/* For an ARRAY_TYPE, this is the index of the first element
   not yet written out.
   This is a special INTEGER_CST node that we modify in place.  */
static tree constructor_unfilled_index;

/* In a RECORD_TYPE, the byte index of the next consecutive field.
   This is so we can generate gaps between fields, when appropriate.
   This is a special INTEGER_CST node that we modify in place.  */
static tree constructor_bit_index;

/* If we are saving up the elements rather than allocating them,
   this is the list of elements so far (in reverse order,
   most recent first).  */
static tree constructor_elements;

/* 1 if so far this constructor's elements are all compile-time constants.  */
static int constructor_constant;

/* 1 if so far this constructor's elements are all valid address constants.  */
static int constructor_simple;

/* 1 if this constructor is erroneous so far.  */
static int constructor_erroneous;

/* 1 if have called defer_addressed_constants.  */
static int constructor_subconstants_deferred;

/* Structure for managing pending initializer elements, organized as an
   AVL tree.  */

struct init_node
{
  struct init_node *left, *right;
  struct init_node *parent;
  int balance;
  tree purpose;
  tree value;
};

/* Tree of pending elements at this constructor level.
   These are elements encountered out of order
   which belong at places we haven't reached yet in actually
   writing the output.  */
static struct init_node *constructor_pending_elts;

/* The SPELLING_DEPTH of this constructor.  */
static int constructor_depth;

/* 0 if implicitly pushing constructor levels is allowed.  */
int constructor_no_implicit = 0; /* 0 for C; 1 for some other languages.  */

static int require_constant_value;
static int require_constant_elements;

/* 1 if it is ok to output this constructor as we read it.
   0 means must accumulate a CONSTRUCTOR expression.  */
static int constructor_incremental;

/* DECL node for which an initializer is being read.
   0 means we are reading a constructor expression
   such as (struct foo) {...}.  */
static tree constructor_decl;

/* start_init saves the ASMSPEC arg here for really_start_incremental_init.  */
static char *constructor_asmspec;

/* Nonzero if this is an initializer for a top-level decl.  */
static int constructor_top_level;


/* This stack has a level for each implicit or explicit level of
   structuring in the initializer, including the outermost one.  It
   saves the values of most of the variables above.  */

struct constructor_stack
{
  struct constructor_stack *next;
  tree type;
  tree fields;
  tree index;
  tree range_end;
  tree max_index;
  tree unfilled_index;
  tree unfilled_fields;
  tree bit_index;
  tree elements;
  int offset;
  struct init_node *pending_elts;
  int depth;
  /* If nonzero, this value should replace the entire
     constructor at this level.  */
  tree replacement_value;
  char constant;
  char simple;
  char implicit;
  char incremental;
  char erroneous;
  char outer;
};

struct constructor_stack *constructor_stack;

/* This stack records separate initializers that are nested.
   Nested initializers can't happen in ANSI C, but GNU C allows them
   in cases like { ... (struct foo) { ... } ... }.  */

struct initializer_stack
{
  struct initializer_stack *next;
  tree decl;
  char *asmspec;
  struct constructor_stack *constructor_stack;
  tree elements;
  struct spelling *spelling;
  struct spelling *spelling_base;
  int spelling_size;
  char top_level;
  char incremental;
  char require_constant_value;
  char require_constant_elements;
  char deferred;
};

struct initializer_stack *initializer_stack;

/* Prepare to parse and output the initializer for variable DECL.  */

void
start_init (decl, asmspec_tree, top_level)
     tree decl;
     tree asmspec_tree;
     int top_level;
{
  const char *locus;
  struct initializer_stack *p
    = (struct initializer_stack *) xmalloc (sizeof (struct initializer_stack));
  char *asmspec = 0;

  if (asmspec_tree)
    asmspec = TREE_STRING_POINTER (asmspec_tree);

  p->decl = constructor_decl;
  p->asmspec = constructor_asmspec;
  p->incremental = constructor_incremental;
  p->require_constant_value = require_constant_value;
  p->require_constant_elements = require_constant_elements;
  p->constructor_stack = constructor_stack;
  p->elements = constructor_elements;
  p->spelling = spelling;
  p->spelling_base = spelling_base;
  p->spelling_size = spelling_size;
  p->deferred = constructor_subconstants_deferred;
  p->top_level = constructor_top_level;
  p->next = initializer_stack;
  initializer_stack = p;

  constructor_decl = decl;
  constructor_incremental = top_level;
  constructor_asmspec = asmspec;
  constructor_subconstants_deferred = 0;
  constructor_top_level = top_level;

  if (decl != 0)
    {
      require_constant_value = TREE_STATIC (decl);
      require_constant_elements
	= ((TREE_STATIC (decl) || pedantic)
	   /* For a scalar, you can always use any value to initialize,
	      even within braces.  */
	   && (TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE
	       || TREE_CODE (TREE_TYPE (decl)) == RECORD_TYPE
	       || TREE_CODE (TREE_TYPE (decl)) == UNION_TYPE
	       || TREE_CODE (TREE_TYPE (decl)) == QUAL_UNION_TYPE));
      locus = IDENTIFIER_POINTER (DECL_NAME (decl));
      constructor_incremental |= TREE_STATIC (decl);
    }
  else
    {
      require_constant_value = 0;
      require_constant_elements = 0;
      locus = "(anonymous)";
    }

  constructor_stack = 0;

  missing_braces_mentioned = 0;

  spelling_base = 0;
  spelling_size = 0;
  RESTORE_SPELLING_DEPTH (0);

  if (locus)
    push_string (locus);
}

void
finish_init ()
{
  struct initializer_stack *p = initializer_stack;

  /* Output subconstants (string constants, usually)
     that were referenced within this initializer and saved up.
     Must do this if and only if we called defer_addressed_constants.  */
  if (constructor_subconstants_deferred)
    output_deferred_addressed_constants ();

  /* Free the whole constructor stack of this initializer.  */
  while (constructor_stack)
    {
      struct constructor_stack *q = constructor_stack;
      constructor_stack = q->next;
      free (q);
    }

  /* Pop back to the data of the outer initializer (if any).  */
  constructor_decl = p->decl;
  constructor_asmspec = p->asmspec;
  constructor_incremental = p->incremental;
  require_constant_value = p->require_constant_value;
  require_constant_elements = p->require_constant_elements;
  constructor_stack = p->constructor_stack;
  constructor_elements = p->elements;
  spelling = p->spelling;
  spelling_base = p->spelling_base;
  spelling_size = p->spelling_size;
  constructor_subconstants_deferred = p->deferred;
  constructor_top_level = p->top_level;
  initializer_stack = p->next;
  free (p);
}

/* Call here when we see the initializer is surrounded by braces.
   This is instead of a call to push_init_level;
   it is matched by a call to pop_init_level.

   TYPE is the type to initialize, for a constructor expression.
   For an initializer for a decl, TYPE is zero.  */

void
really_start_incremental_init (type)
     tree type;
{
  struct constructor_stack *p
    = (struct constructor_stack *) xmalloc (sizeof (struct constructor_stack));

  if (type == 0)
    type = TREE_TYPE (constructor_decl);

  /* Turn off constructor_incremental if type is a struct with bitfields.
     Do this before the first push, so that the corrected value
     is available in finish_init.  */
  check_init_type_bitfields (type);

  p->type = constructor_type;
  p->fields = constructor_fields;
  p->index = constructor_index;
  p->range_end = constructor_range_end;
  p->max_index = constructor_max_index;
  p->unfilled_index = constructor_unfilled_index;
  p->unfilled_fields = constructor_unfilled_fields;
  p->bit_index = constructor_bit_index;
  p->elements = constructor_elements;
  p->constant = constructor_constant;
  p->simple = constructor_simple;
  p->erroneous = constructor_erroneous;
  p->pending_elts = constructor_pending_elts;
  p->depth = constructor_depth;
  p->replacement_value = 0;
  p->implicit = 0;
  p->incremental = constructor_incremental;
  p->outer = 0;
  p->next = 0;
  constructor_stack = p;

  constructor_constant = 1;
  constructor_simple = 1;
  constructor_depth = SPELLING_DEPTH ();
  constructor_elements = 0;
  constructor_pending_elts = 0;
  constructor_type = type;

  if (TREE_CODE (constructor_type) == RECORD_TYPE
      || TREE_CODE (constructor_type) == UNION_TYPE)
    {
      constructor_fields = TYPE_FIELDS (constructor_type);
      /* Skip any nameless bit fields at the beginning.  */
      while (constructor_fields != 0 && DECL_C_BIT_FIELD (constructor_fields)
	     && DECL_NAME (constructor_fields) == 0)
	constructor_fields = TREE_CHAIN (constructor_fields);
      constructor_unfilled_fields = constructor_fields;
      constructor_bit_index = copy_node (integer_zero_node);
      TREE_TYPE (constructor_bit_index) = sbitsizetype;
    }
  else if (TREE_CODE (constructor_type) == ARRAY_TYPE)
    {
      constructor_range_end = 0;
      if (TYPE_DOMAIN (constructor_type))
	{
	  constructor_max_index
	    = TYPE_MAX_VALUE (TYPE_DOMAIN (constructor_type));
	  constructor_index
	    = copy_node (TYPE_MIN_VALUE (TYPE_DOMAIN (constructor_type)));
	}
      else
	constructor_index = copy_node (integer_zero_node);
      constructor_unfilled_index = copy_node (constructor_index);
    }
  else
    {
      /* Handle the case of int x = {5}; */
      constructor_fields = constructor_type;
      constructor_unfilled_fields = constructor_type;
    }

  if (constructor_incremental)
    {
      int momentary = suspend_momentary ();
      push_obstacks_nochange ();
      if (TREE_PERMANENT (constructor_decl))
	end_temporary_allocation ();
      make_decl_rtl (constructor_decl, constructor_asmspec,
		     constructor_top_level);
      assemble_variable (constructor_decl, constructor_top_level, 0, 1);
      pop_obstacks ();
      resume_momentary (momentary);
    }

  if (constructor_incremental)
    {
      defer_addressed_constants ();
      constructor_subconstants_deferred = 1;
    }
}

/* Push down into a subobject, for initialization.
   If this is for an explicit set of braces, IMPLICIT is 0.
   If it is because the next element belongs at a lower level,
   IMPLICIT is 1.  */

void
push_init_level (implicit)
     int implicit;
{
  struct constructor_stack *p;

  /* If we've exhausted any levels that didn't have braces,
     pop them now.  */
  while (constructor_stack->implicit)
    {
      if ((TREE_CODE (constructor_type) == RECORD_TYPE
	   || TREE_CODE (constructor_type) == UNION_TYPE)
	  && constructor_fields == 0)
	process_init_element (pop_init_level (1));
      else if (TREE_CODE (constructor_type) == ARRAY_TYPE
	       && tree_int_cst_lt (constructor_max_index, constructor_index))
	process_init_element (pop_init_level (1));
      else
	break;
    }

  /* Structure elements may require alignment.  Do this now if necessary
     for the subaggregate, and if it comes next in sequence.  Don't do
     this for subaggregates that will go on the pending list.  */
  if (constructor_incremental && constructor_type != 0
      && TREE_CODE (constructor_type) == RECORD_TYPE && constructor_fields
      && constructor_fields == constructor_unfilled_fields)
    {
      /* Advance to offset of this element.  */
      if (! tree_int_cst_equal (constructor_bit_index,
				DECL_FIELD_BITPOS (constructor_fields)))
	{
	  /* By using unsigned arithmetic, the result will be correct even
	     in case of overflows, if BITS_PER_UNIT is a power of two.  */
	  unsigned next = (TREE_INT_CST_LOW
			   (DECL_FIELD_BITPOS (constructor_fields))
			   / (unsigned)BITS_PER_UNIT);
	  unsigned here = (TREE_INT_CST_LOW (constructor_bit_index)
			   / (unsigned)BITS_PER_UNIT);

	  assemble_zeros ((next - here)
			  * (unsigned)BITS_PER_UNIT
			  / (unsigned)BITS_PER_UNIT);
	}
      /* Indicate that we have now filled the structure up to the current
	 field.  */
      constructor_unfilled_fields = constructor_fields;
    }

  p = (struct constructor_stack *) xmalloc (sizeof (struct constructor_stack));
  p->type = constructor_type;
  p->fields = constructor_fields;
  p->index = constructor_index;
  p->range_end = constructor_range_end;
  p->max_index = constructor_max_index;
  p->unfilled_index = constructor_unfilled_index;
  p->unfilled_fields = constructor_unfilled_fields;
  p->bit_index = constructor_bit_index;
  p->elements = constructor_elements;
  p->constant = constructor_constant;
  p->simple = constructor_simple;
  p->erroneous = constructor_erroneous;
  p->pending_elts = constructor_pending_elts;
  p->depth = constructor_depth;
  p->replacement_value = 0;
  p->implicit = implicit;
  p->incremental = constructor_incremental;
  p->outer = 0;
  p->next = constructor_stack;
  constructor_stack = p;

  constructor_constant = 1;
  constructor_simple = 1;
  constructor_depth = SPELLING_DEPTH ();
  constructor_elements = 0;
  constructor_pending_elts = 0;

  /* Don't die if an entire brace-pair level is superfluous
     in the containing level.  */
  if (constructor_type == 0)
    ;
  else if (TREE_CODE (constructor_type) == RECORD_TYPE
	   || TREE_CODE (constructor_type) == UNION_TYPE)
    {
      /* Don't die if there are extra init elts at the end.  */
      if (constructor_fields == 0)
	constructor_type = 0;
      else
	{
	  constructor_type = TREE_TYPE (constructor_fields);
	  push_member_name (constructor_fields);
	  constructor_depth++;
	  if (constructor_fields != constructor_unfilled_fields)
	    constructor_incremental = 0;
	}
    }
  else if (TREE_CODE (constructor_type) == ARRAY_TYPE)
    {
      constructor_type = TREE_TYPE (constructor_type);
      push_array_bounds (TREE_INT_CST_LOW (constructor_index));
      constructor_depth++;
      if (! tree_int_cst_equal (constructor_index, constructor_unfilled_index)
	  || constructor_range_end != 0)
	constructor_incremental = 0;
    }

  if (constructor_type == 0)
    {
      error_init ("extra brace group at end of initializer");
      constructor_fields = 0;
      constructor_unfilled_fields = 0;
      return;
    }

  /* Turn off constructor_incremental if type is a struct with bitfields.  */
  check_init_type_bitfields (constructor_type);

  if (implicit && warn_missing_braces && !missing_braces_mentioned)
    {
      missing_braces_mentioned = 1;
      warning_init ("missing braces around initializer");
    }

  if (TREE_CODE (constructor_type) == RECORD_TYPE
	   || TREE_CODE (constructor_type) == UNION_TYPE)
    {
      constructor_fields = TYPE_FIELDS (constructor_type);
      /* Skip any nameless bit fields at the beginning.  */
      while (constructor_fields != 0 && DECL_C_BIT_FIELD (constructor_fields)
	     && DECL_NAME (constructor_fields) == 0)
	constructor_fields = TREE_CHAIN (constructor_fields);
      constructor_unfilled_fields = constructor_fields;
      constructor_bit_index = copy_node (integer_zero_node);
      TREE_TYPE (constructor_bit_index) = sbitsizetype;
    }
  else if (TREE_CODE (constructor_type) == ARRAY_TYPE)
    {
      constructor_range_end = 0;
      if (TYPE_DOMAIN (constructor_type))
	{
	  constructor_max_index
	    = TYPE_MAX_VALUE (TYPE_DOMAIN (constructor_type));
	  constructor_index
	    = copy_node (TYPE_MIN_VALUE (TYPE_DOMAIN (constructor_type)));
	}
      else
	constructor_index = copy_node (integer_zero_node);
      constructor_unfilled_index = copy_node (constructor_index);
    }
  else
    {
      warning_init ("braces around scalar initializer");
      constructor_fields = constructor_type;
      constructor_unfilled_fields = constructor_type;
    }
}

/* Don't read a struct incrementally if it has any bitfields,
   because the incremental reading code doesn't know how to
   handle bitfields yet.  */

static void
check_init_type_bitfields (type)
     tree type;
{
  if (TREE_CODE (type) == RECORD_TYPE)
    {
      tree tail;
      for (tail = TYPE_FIELDS (type); tail;
	   tail = TREE_CHAIN (tail))
	{
	  if (DECL_C_BIT_FIELD (tail))
	    {
	      constructor_incremental = 0;
	      break;
	    }

	  check_init_type_bitfields (TREE_TYPE (tail));
	}
    }

  else if (TREE_CODE (type) == UNION_TYPE)
    {
      tree tail = TYPE_FIELDS (type);
      if (tail && DECL_C_BIT_FIELD (tail))
	/* We also use the nonincremental algorithm for initiliazation
	   of unions whose first member is a bitfield, becuase the
	   incremental algorithm has no code for dealing with
	   bitfields. */
	constructor_incremental = 0;
    }

  else if (TREE_CODE (type) == ARRAY_TYPE)
    check_init_type_bitfields (TREE_TYPE (type));
}

/* At the end of an implicit or explicit brace level, 
   finish up that level of constructor.
   If we were outputting the elements as they are read, return 0
   from inner levels (process_init_element ignores that),
   but return error_mark_node from the outermost level
   (that's what we want to put in DECL_INITIAL).
   Otherwise, return a CONSTRUCTOR expression.  */

tree
pop_init_level (implicit)
     int implicit;
{
  struct constructor_stack *p;
  int size = 0;
  tree constructor = 0;

  if (implicit == 0)
    {
      /* When we come to an explicit close brace,
	 pop any inner levels that didn't have explicit braces.  */
      while (constructor_stack->implicit)
	process_init_element (pop_init_level (1));
    }

  p = constructor_stack;

  if (constructor_type != 0)
    size = int_size_in_bytes (constructor_type);

  /* Warn when some struct elements are implicitly initialized to zero.  */
  if (extra_warnings
      && constructor_type
      && TREE_CODE (constructor_type) == RECORD_TYPE
      && constructor_unfilled_fields)
    {
      push_member_name (constructor_unfilled_fields);
      warning_init ("missing initializer");
      RESTORE_SPELLING_DEPTH (constructor_depth);
    }

  /* Now output all pending elements.  */
  output_pending_init_elements (1);

#if 0 /* c-parse.in warns about {}.  */
  /* In ANSI, each brace level must have at least one element.  */
  if (! implicit && pedantic
      && (TREE_CODE (constructor_type) == ARRAY_TYPE
	  ? integer_zerop (constructor_unfilled_index)
	  : constructor_unfilled_fields == TYPE_FIELDS (constructor_type)))
    pedwarn_init ("empty braces in initializer");
#endif

  /* Pad out the end of the structure.  */
  
  if (p->replacement_value)
    {
      /* If this closes a superfluous brace pair,
	 just pass out the element between them.  */
      constructor = p->replacement_value;
      /* If this is the top level thing within the initializer,
	 and it's for a variable, then since we already called
	 assemble_variable, we must output the value now.  */
      if (p->next == 0 && constructor_decl != 0
	  && constructor_incremental)
	{
	  constructor = digest_init (constructor_type, constructor,
				     require_constant_value,
				     require_constant_elements);

	  /* If initializing an array of unknown size,
	     determine the size now.  */
	  if (TREE_CODE (constructor_type) == ARRAY_TYPE
	      && TYPE_DOMAIN (constructor_type) == 0)
	    {
	      int failure;
	      int momentary_p;

	      push_obstacks_nochange ();
	      if (TREE_PERMANENT (constructor_type))
		end_temporary_allocation ();

	      momentary_p = suspend_momentary ();

	      /* We shouldn't have an incomplete array type within
		 some other type.  */
	      if (constructor_stack->next)
		abort ();

	      failure
		= complete_array_type (constructor_type,
				       constructor, 0);
	      if (failure)
		abort ();

	      size = int_size_in_bytes (constructor_type);
	      resume_momentary (momentary_p);
	      pop_obstacks ();
	    }

	  output_constant (constructor, size);
	}
    }
  else if (constructor_type == 0)
    ;
  else if (TREE_CODE (constructor_type) != RECORD_TYPE
	   && TREE_CODE (constructor_type) != UNION_TYPE
	   && TREE_CODE (constructor_type) != ARRAY_TYPE
	   && ! constructor_incremental)
    {
      /* A nonincremental scalar initializer--just return
	 the element, after verifying there is just one.  */
      if (constructor_elements == 0)
	{
	  error_init ("empty scalar initializer");
	  constructor = error_mark_node;
	}
      else if (TREE_CHAIN (constructor_elements) != 0)
	{
	  error_init ("extra elements in scalar initializer");
	  constructor = TREE_VALUE (constructor_elements);
	}
      else
	constructor = TREE_VALUE (constructor_elements);
    }
  else if (! constructor_incremental)
    {
      if (constructor_erroneous)
	constructor = error_mark_node;
      else
	{
	  int momentary = suspend_momentary ();

	  constructor = build (CONSTRUCTOR, constructor_type, NULL_TREE,
			       nreverse (constructor_elements));
	  if (constructor_constant)
	    TREE_CONSTANT (constructor) = 1;
	  if (constructor_constant && constructor_simple)
	    TREE_STATIC (constructor) = 1;

	  resume_momentary (momentary);
	}
    }
  else
    {
      tree filled;
      int momentary = suspend_momentary ();

      if (TREE_CODE (constructor_type) == RECORD_TYPE
	  || TREE_CODE (constructor_type) == UNION_TYPE)
	{
	  /* Find the offset of the end of that field.  */
	  filled = size_binop (CEIL_DIV_EXPR,
			       constructor_bit_index,
			       size_int (BITS_PER_UNIT));
	}
      else if (TREE_CODE (constructor_type) == ARRAY_TYPE)
	{
	  /* If initializing an array of unknown size,
	     determine the size now.  */
	  if (TREE_CODE (constructor_type) == ARRAY_TYPE
	      && TYPE_DOMAIN (constructor_type) == 0)
	    {
	      tree maxindex
		= size_binop (MINUS_EXPR,
			      constructor_unfilled_index,
			      integer_one_node);

	      push_obstacks_nochange ();
	      if (TREE_PERMANENT (constructor_type))
		end_temporary_allocation ();
	      maxindex = copy_node (maxindex);
	      TYPE_DOMAIN (constructor_type) = build_index_type (maxindex);
	      TREE_TYPE (maxindex) = TYPE_DOMAIN (constructor_type);

	      /* TYPE_MAX_VALUE is always one less than the number of elements
		 in the array, because we start counting at zero.  Therefore,
		 warn only if the value is less than zero.  */
	      if (pedantic
		  && (tree_int_cst_sgn (TYPE_MAX_VALUE (TYPE_DOMAIN (constructor_type)))
		      < 0))
		error_with_decl (constructor_decl,
				 "zero or negative array size `%s'");
	      layout_type (constructor_type);
	      size = int_size_in_bytes (constructor_type);
	      pop_obstacks ();
	    }

	  filled = size_binop (MULT_EXPR, constructor_unfilled_index,
			       size_in_bytes (TREE_TYPE (constructor_type)));
	}
      else
	filled = 0;

      if (filled != 0)
	assemble_zeros (size - TREE_INT_CST_LOW (filled));

      resume_momentary (momentary);
    }

	  
  constructor_type = p->type;
  constructor_fields = p->fields;
  constructor_index = p->index;
  constructor_range_end = p->range_end;
  constructor_max_index = p->max_index;
  constructor_unfilled_index = p->unfilled_index;
  constructor_unfilled_fields = p->unfilled_fields;
  constructor_bit_index = p->bit_index;
  constructor_elements = p->elements;
  constructor_constant = p->constant;
  constructor_simple = p->simple;
  constructor_erroneous = p->erroneous;
  constructor_pending_elts = p->pending_elts;
  constructor_depth = p->depth;
  constructor_incremental = p->incremental;
  RESTORE_SPELLING_DEPTH (constructor_depth);

  constructor_stack = p->next;
  free (p);

  if (constructor == 0)
    {
      if (constructor_stack == 0)
	return error_mark_node;
      return NULL_TREE;
    }
  return constructor;
}

/* Within an array initializer, specify the next index to be initialized.
   FIRST is that index.  If LAST is nonzero, then initialize a range
   of indices, running from FIRST through LAST.  */

void
set_init_index (first, last)
     tree first, last;
{
  while ((TREE_CODE (first) == NOP_EXPR
	  || TREE_CODE (first) == CONVERT_EXPR
	  || TREE_CODE (first) == NON_LVALUE_EXPR)
	 && (TYPE_MODE (TREE_TYPE (first))
	     == TYPE_MODE (TREE_TYPE (TREE_OPERAND (first, 0)))))
    (first) = TREE_OPERAND (first, 0);
  if (last)
    while ((TREE_CODE (last) == NOP_EXPR
	    || TREE_CODE (last) == CONVERT_EXPR
	    || TREE_CODE (last) == NON_LVALUE_EXPR)
	   && (TYPE_MODE (TREE_TYPE (last))
	       == TYPE_MODE (TREE_TYPE (TREE_OPERAND (last, 0)))))
      (last) = TREE_OPERAND (last, 0);

  if (TREE_CODE (first) != INTEGER_CST)
    error_init ("nonconstant array index in initializer");
  else if (last != 0 && TREE_CODE (last) != INTEGER_CST)
    error_init ("nonconstant array index in initializer");
  else if (! constructor_unfilled_index)
    error_init ("array index in non-array initializer");
  else if (tree_int_cst_lt (first, constructor_unfilled_index))
    error_init ("duplicate array index in initializer");
  else
    {
      TREE_INT_CST_LOW (constructor_index) = TREE_INT_CST_LOW (first);
      TREE_INT_CST_HIGH (constructor_index) = TREE_INT_CST_HIGH (first);

      if (last != 0 && tree_int_cst_lt (last, first))
	error_init ("empty index range in initializer");
      else
	{
	  if (pedantic)
	    pedwarn ("ANSI C forbids specifying element to initialize");
	  constructor_range_end = last;
	}
    }
}

/* Within a struct initializer, specify the next field to be initialized.  */

void
set_init_label (fieldname)
     tree fieldname;
{
  tree tail;
  int passed = 0;

  /* Don't die if an entire brace-pair level is superfluous
     in the containing level.  */
  if (constructor_type == 0)
    return;

  for (tail = TYPE_FIELDS (constructor_type); tail;
       tail = TREE_CHAIN (tail))
    {
      if (tail == constructor_unfilled_fields)
	passed = 1;
      if (DECL_NAME (tail) == fieldname)
	break;
    }

  if (tail == 0)
    error ("unknown field `%s' specified in initializer",
	   IDENTIFIER_POINTER (fieldname));
  else if (!passed)
    error ("field `%s' already initialized",
	   IDENTIFIER_POINTER (fieldname));
  else
    {
      constructor_fields = tail;
      if (pedantic)
	pedwarn ("ANSI C forbids specifying structure member to initialize");
    }
}

/* Add a new initializer to the tree of pending initializers.  PURPOSE
   indentifies the initializer, either array index or field in a structure. 
   VALUE is the value of that index or field.  */

static void
add_pending_init (purpose, value)
     tree purpose, value;
{
  struct init_node *p, **q, *r;

  q = &constructor_pending_elts;
  p = 0;

  if (TREE_CODE (constructor_type) == ARRAY_TYPE)
    {
      while (*q != 0)
	{
	  p = *q;
	  if (tree_int_cst_lt (purpose, p->purpose))
	    q = &p->left;
	  else if (tree_int_cst_lt (p->purpose, purpose))
	    q = &p->right;
	  else
	    abort ();
	}
    }
  else
    {
      while (*q != NULL)
	{
	  p = *q;
	  if (tree_int_cst_lt (DECL_FIELD_BITPOS (purpose),
			       DECL_FIELD_BITPOS (p->purpose)))
	    q = &p->left;
	  else if (tree_int_cst_lt (DECL_FIELD_BITPOS (p->purpose),
				    DECL_FIELD_BITPOS (purpose)))
	    q = &p->right;
	  else
	    abort ();
	}
    }

  r = (struct init_node *) oballoc (sizeof (struct init_node));
  r->purpose = purpose;
  r->value = value;

  *q = r;
  r->parent = p;
  r->left = 0;
  r->right = 0;
  r->balance = 0;

  while (p)
    {
      struct init_node *s;

      if (r == p->left)
	{
	  if (p->balance == 0)
	    p->balance = -1;
	  else if (p->balance < 0)
	    {
	      if (r->balance < 0)
		{
		  /* L rotation. */
		  p->left = r->right;
		  if (p->left)
		    p->left->parent = p;
		  r->right = p;

		  p->balance = 0;
		  r->balance = 0;

		  s = p->parent;
		  p->parent = r;
		  r->parent = s;
		  if (s)
		    {
		      if (s->left == p)
			s->left = r;
		      else
			s->right = r;
		    }
		  else
		    constructor_pending_elts = r;
		}
	      else
		{
		  /* LR rotation. */
		  struct init_node *t = r->right;

		  r->right = t->left;
		  if (r->right)
		    r->right->parent = r;
		  t->left = r;

		  p->left = t->right;
		  if (p->left)
		    p->left->parent = p;
		  t->right = p;

		  p->balance = t->balance < 0;
		  r->balance = -(t->balance > 0);
		  t->balance = 0;

		  s = p->parent;
		  p->parent = t;
		  r->parent = t;
		  t->parent = s;
		  if (s)
		    {
		      if (s->left == p)
			s->left = t;
		      else
			s->right = t;
		    }
		  else
		    constructor_pending_elts = t;
		}
	      break;
	    }
	  else
	    {
	      /* p->balance == +1; growth of left side balances the node.  */
	      p->balance = 0;
	      break;
	    }
	}
      else /* r == p->right */
	{
	  if (p->balance == 0)
	    /* Growth propagation from right side.  */
	    p->balance++;
	  else if (p->balance > 0)
	    {
	      if (r->balance > 0)
		{
		  /* R rotation. */
		  p->right = r->left;
		  if (p->right)
		    p->right->parent = p;
		  r->left = p;

		  p->balance = 0;
		  r->balance = 0;

		  s = p->parent;
		  p->parent = r;
		  r->parent = s;
		  if (s)
		    {
		      if (s->left == p)
			s->left = r;
		      else
			s->right = r;
		    }
		  else
		    constructor_pending_elts = r;
		}
	      else /* r->balance == -1 */
		{
		  /* RL rotation */
		  struct init_node *t = r->left;

		  r->left = t->right;
		  if (r->left)
		    r->left->parent = r;
		  t->right = r;

		  p->right = t->left;
		  if (p->right)
		    p->right->parent = p;
		  t->left = p;

		  r->balance = (t->balance < 0);
		  p->balance = -(t->balance > 0);
		  t->balance = 0;

		  s = p->parent;
		  p->parent = t;
		  r->parent = t;
		  t->parent = s;
		  if (s)
		    {
		      if (s->left == p)
			s->left = t;
		      else
			s->right = t;
		    }
		  else
		    constructor_pending_elts = t;
		}
	      break;
	    }
	  else
	    {
	      /* p->balance == -1; growth of right side balances the node. */
	      p->balance = 0;
	      break;
	    }
	}

      r = p;
      p = p->parent;
    }
}

/* Return nonzero if FIELD is equal to the index of a pending initializer.  */

static int
pending_init_member (field)
     tree field;
{
  struct init_node *p;

  p = constructor_pending_elts;
  if (TREE_CODE (constructor_type) == ARRAY_TYPE)
    {
      while (p)
	{
	  if (tree_int_cst_equal (field, p->purpose))
	    return 1;
	  else if (tree_int_cst_lt (field, p->purpose))
	    p = p->left;
	  else
	    p = p->right;
	}
    }
  else
    {
      while (p)
	{
	  if (field == p->purpose)
	    return 1;
	  else if (tree_int_cst_lt (DECL_FIELD_BITPOS (field),
				    DECL_FIELD_BITPOS (p->purpose)))
	    p = p->left;
	  else
	    p = p->right;
	}
    }

  return 0;
}

/* "Output" the next constructor element.
   At top level, really output it to assembler code now.
   Otherwise, collect it in a list from which we will make a CONSTRUCTOR.
   TYPE is the data type that the containing data type wants here.
   FIELD is the field (a FIELD_DECL) or the index that this element fills.

   PENDING if non-nil means output pending elements that belong
   right after this element.  (PENDING is normally 1;
   it is 0 while outputting pending elements, to avoid recursion.)  */

static void
output_init_element (value, type, field, pending)
     tree value, type, field;
     int pending;
{
  int duplicate = 0;

  if (TREE_CODE (TREE_TYPE (value)) == FUNCTION_TYPE
      || (TREE_CODE (TREE_TYPE (value)) == ARRAY_TYPE
	  && !(TREE_CODE (value) == STRING_CST
	       && TREE_CODE (type) == ARRAY_TYPE
	       && TREE_CODE (TREE_TYPE (type)) == INTEGER_TYPE)
	  && !comptypes (TYPE_MAIN_VARIANT (TREE_TYPE (value)),
			 TYPE_MAIN_VARIANT (type))))
    value = default_conversion (value);

  if (value == error_mark_node)
    constructor_erroneous = 1;
  else if (!TREE_CONSTANT (value))
    constructor_constant = 0;
  else if (initializer_constant_valid_p (value, TREE_TYPE (value)) == 0
	   || ((TREE_CODE (constructor_type) == RECORD_TYPE
		|| TREE_CODE (constructor_type) == UNION_TYPE)
	       && DECL_C_BIT_FIELD (field)
	       && TREE_CODE (value) != INTEGER_CST))
    constructor_simple = 0;

  if (require_constant_value && ! TREE_CONSTANT (value))
    {
      error_init ("initializer element is not constant");
      value = error_mark_node;
    }
  else if (require_constant_elements
	   && initializer_constant_valid_p (value, TREE_TYPE (value)) == 0)
    {
      error_init ("initializer element is not computable at load time");
      value = error_mark_node;
    }

  /* If this element duplicates one on constructor_pending_elts,
     print a message and ignore it.  Don't do this when we're
     processing elements taken off constructor_pending_elts,
     because we'd always get spurious errors.  */
  if (pending)
    {
      if (TREE_CODE (constructor_type) == RECORD_TYPE
	  || TREE_CODE (constructor_type) == UNION_TYPE
	  || TREE_CODE (constructor_type) == ARRAY_TYPE)
	{
	  if (pending_init_member (field))
	    {
	      error_init ("duplicate initializer");
	      duplicate = 1;
	    }
	}
    }

  /* If this element doesn't come next in sequence,
     put it on constructor_pending_elts.  */
  if (TREE_CODE (constructor_type) == ARRAY_TYPE
      && !tree_int_cst_equal (field, constructor_unfilled_index))
    {
      if (! duplicate)
	/* The copy_node is needed in case field is actually
	   constructor_index, which is modified in place.  */
	add_pending_init (copy_node (field),
			  digest_init (type, value, require_constant_value, 
				       require_constant_elements));
    }
  else if (TREE_CODE (constructor_type) == RECORD_TYPE
	   && field != constructor_unfilled_fields)
    {
      /* We do this for records but not for unions.  In a union,
	 no matter which field is specified, it can be initialized
	 right away since it starts at the beginning of the union.  */
      if (!duplicate)
	add_pending_init (field,
			  digest_init (type, value, require_constant_value, 
				       require_constant_elements));
    }
  else
    {
      /* Otherwise, output this element either to
	 constructor_elements or to the assembler file.  */

      if (!duplicate)
	{
	  if (! constructor_incremental)
	    {
	      if (field && TREE_CODE (field) == INTEGER_CST)
		field = copy_node (field);
	      constructor_elements
		= tree_cons (field, digest_init (type, value,
						 require_constant_value, 
						 require_constant_elements),
			     constructor_elements);
	    }
	  else
	    {
	      /* Structure elements may require alignment.
		 Do this, if necessary.  */
	      if (TREE_CODE (constructor_type) == RECORD_TYPE)
		{
		  /* Advance to offset of this element.  */
		  if (! tree_int_cst_equal (constructor_bit_index,
					    DECL_FIELD_BITPOS (field)))
		    {
		      /* By using unsigned arithmetic, the result will be
			 correct even in case of overflows, if BITS_PER_UNIT
			 is a power of two.  */
		      unsigned next = (TREE_INT_CST_LOW
				       (DECL_FIELD_BITPOS (field))
				       / (unsigned)BITS_PER_UNIT);
		      unsigned here = (TREE_INT_CST_LOW
				       (constructor_bit_index)
				       / (unsigned)BITS_PER_UNIT);

		      assemble_zeros ((next - here)
				      * (unsigned)BITS_PER_UNIT
				      / (unsigned)BITS_PER_UNIT);
		    }
		}
	      output_constant (digest_init (type, value,
					    require_constant_value,
					    require_constant_elements),
			       int_size_in_bytes (type));

	      /* For a record or union,
		 keep track of end position of last field.  */
	      if (TREE_CODE (constructor_type) == RECORD_TYPE
		  || TREE_CODE (constructor_type) == UNION_TYPE)
		{
		  tree temp = size_binop (PLUS_EXPR, DECL_FIELD_BITPOS (field),
					  DECL_SIZE (field));
		  TREE_INT_CST_LOW (constructor_bit_index)
		    = TREE_INT_CST_LOW (temp);
		  TREE_INT_CST_HIGH (constructor_bit_index)
		    = TREE_INT_CST_HIGH (temp);
		}
	    }
	}

      /* Advance the variable that indicates sequential elements output.  */
      if (TREE_CODE (constructor_type) == ARRAY_TYPE)
	{
	  tree tem = size_binop (PLUS_EXPR, constructor_unfilled_index,
				 integer_one_node);
	  TREE_INT_CST_LOW (constructor_unfilled_index)
	    = TREE_INT_CST_LOW (tem);
	  TREE_INT_CST_HIGH (constructor_unfilled_index)
	    = TREE_INT_CST_HIGH (tem);
	}
      else if (TREE_CODE (constructor_type) == RECORD_TYPE)
	{
	  constructor_unfilled_fields =
	    TREE_CHAIN (constructor_unfilled_fields);
	  /* Skip any nameless bit fields.  */
	  while (constructor_unfilled_fields != 0
		 && DECL_C_BIT_FIELD (constructor_unfilled_fields)
		 && DECL_NAME (constructor_unfilled_fields) == 0)
	    constructor_unfilled_fields =
	      TREE_CHAIN (constructor_unfilled_fields);
	}
      else if (TREE_CODE (constructor_type) == UNION_TYPE)
	constructor_unfilled_fields = 0;

      /* Now output any pending elements which have become next.  */
      if (pending)
	output_pending_init_elements (0);
    }
}

/* Output any pending elements which have become next.
   As we output elements, constructor_unfilled_{fields,index}
   advances, which may cause other elements to become next;
   if so, they too are output.

   If ALL is 0, we return when there are
   no more pending elements to output now.

   If ALL is 1, we output space as necessary so that
   we can output all the pending elements.  */

static void
output_pending_init_elements (all)
     int all;
{
  struct init_node *elt = constructor_pending_elts;
  tree next;

 retry:

  /* Look thru the whole pending tree.
     If we find an element that should be output now,
     output it.  Otherwise, set NEXT to the element
     that comes first among those still pending.  */
     
  next = 0;
  while (elt)
    {
      if (TREE_CODE (constructor_type) == ARRAY_TYPE)
	{
	  if (tree_int_cst_equal (elt->purpose,
				  constructor_unfilled_index))
	    output_init_element (elt->value,
				 TREE_TYPE (constructor_type),
				 constructor_unfilled_index, 0);
	  else if (tree_int_cst_lt (constructor_unfilled_index,
				    elt->purpose))
	    {
	      /* Advance to the next smaller node.  */
	      if (elt->left)
		elt = elt->left;
	      else
		{
		  /* We have reached the smallest node bigger than the
		     current unfilled index.  Fill the space first.  */
		  next = elt->purpose;
		  break;
		}
	    }
	  else
	    {
	      /* Advance to the next bigger node.  */
	      if (elt->right)
		elt = elt->right;
	      else
		{
		  /* We have reached the biggest node in a subtree.  Find
		     the parent of it, which is the next bigger node.  */
		  while (elt->parent && elt->parent->right == elt)
		    elt = elt->parent;
		  elt = elt->parent;
		  if (elt && tree_int_cst_lt (constructor_unfilled_index,
					      elt->purpose))
		    {
		      next = elt->purpose;
		      break;
		    }
		}
	    }
	}
      else if (TREE_CODE (constructor_type) == RECORD_TYPE
	       || TREE_CODE (constructor_type) == UNION_TYPE)
	{
	  /* If the current record is complete we are done.  */
	  if (constructor_unfilled_fields == 0)
	    break;
	  if (elt->purpose == constructor_unfilled_fields)
	    {
	      output_init_element (elt->value,
				   TREE_TYPE (constructor_unfilled_fields),
				   constructor_unfilled_fields,
				   0);
	    }
	  else if (tree_int_cst_lt (DECL_FIELD_BITPOS (constructor_unfilled_fields),
				    DECL_FIELD_BITPOS (elt->purpose)))
	    {
	      /* Advance to the next smaller node.  */
	      if (elt->left)
		elt = elt->left;
	      else
		{
		  /* We have reached the smallest node bigger than the
		     current unfilled field.  Fill the space first.  */
		  next = elt->purpose;
		  break;
		}
	    }
	  else
	    {
	      /* Advance to the next bigger node.  */
	      if (elt->right)
		elt = elt->right;
	      else
		{
		  /* We have reached the biggest node in a subtree.  Find
		     the parent of it, which is the next bigger node.  */
		  while (elt->parent && elt->parent->right == elt)
		    elt = elt->parent;
		  elt = elt->parent;
		  if (elt
		      && tree_int_cst_lt (DECL_FIELD_BITPOS (constructor_unfilled_fields),
					  DECL_FIELD_BITPOS (elt->purpose)))
		    {
		      next = elt->purpose;
		      break;
		    }
		}
	    }
	}
    }

  /* Ordinarily return, but not if we want to output all
     and there are elements left.  */
  if (! (all && next != 0))
    return;

  /* Generate space up to the position of NEXT.  */
  if (constructor_incremental)
    {
      tree filled;
      tree nextpos_tree = size_int (0);

      if (TREE_CODE (constructor_type) == RECORD_TYPE
	  || TREE_CODE (constructor_type) == UNION_TYPE)
	{
	  tree tail;
	  /* Find the last field written out, if any.  */
	  for (tail = TYPE_FIELDS (constructor_type); tail;
	       tail = TREE_CHAIN (tail))
	    if (TREE_CHAIN (tail) == constructor_unfilled_fields)
	      break;

	  if (tail)
	    /* Find the offset of the end of that field.  */
	    filled = size_binop (CEIL_DIV_EXPR,
				 size_binop (PLUS_EXPR,
					     DECL_FIELD_BITPOS (tail),
					     DECL_SIZE (tail)),
				 size_int (BITS_PER_UNIT));
	  else
	    filled = size_int (0);

	  nextpos_tree = size_binop (CEIL_DIV_EXPR,
				     DECL_FIELD_BITPOS (next),
				     size_int (BITS_PER_UNIT));

	  TREE_INT_CST_HIGH (constructor_bit_index)
	    = TREE_INT_CST_HIGH (DECL_FIELD_BITPOS (next));
	  TREE_INT_CST_LOW (constructor_bit_index)
	    = TREE_INT_CST_LOW (DECL_FIELD_BITPOS (next));
	  constructor_unfilled_fields = next;
	}
      else if (TREE_CODE (constructor_type) == ARRAY_TYPE)
	{
	  filled = size_binop (MULT_EXPR, constructor_unfilled_index,
			       size_in_bytes (TREE_TYPE (constructor_type)));
	  nextpos_tree
	    = size_binop (MULT_EXPR, next,
			  size_in_bytes (TREE_TYPE (constructor_type)));
	  TREE_INT_CST_LOW (constructor_unfilled_index)
	    = TREE_INT_CST_LOW (next);
	  TREE_INT_CST_HIGH (constructor_unfilled_index)
	    = TREE_INT_CST_HIGH (next);
	}
      else
	filled = 0;

      if (filled)
	{
	  int nextpos = TREE_INT_CST_LOW (nextpos_tree);

	  assemble_zeros (nextpos - TREE_INT_CST_LOW (filled));
	}
    }
  else
    {
      /* If it's not incremental, just skip over the gap,
	 so that after jumping to retry we will output the next
	 successive element.  */
      if (TREE_CODE (constructor_type) == RECORD_TYPE
	  || TREE_CODE (constructor_type) == UNION_TYPE)
	constructor_unfilled_fields = next;
      else if (TREE_CODE (constructor_type) == ARRAY_TYPE)
	{
	  TREE_INT_CST_LOW (constructor_unfilled_index)
	    = TREE_INT_CST_LOW (next);
	  TREE_INT_CST_HIGH (constructor_unfilled_index)
	    = TREE_INT_CST_HIGH (next);
	}
    }

  /* ELT now points to the node in the pending tree with the next
     initializer to output.  */
  goto retry;
}

/* Add one non-braced element to the current constructor level.
   This adjusts the current position within the constructor's type.
   This may also start or terminate implicit levels
   to handle a partly-braced initializer.

   Once this has found the correct level for the new element,
   it calls output_init_element.

   Note: if we are incrementally outputting this constructor,
   this function may be called with a null argument
   representing a sub-constructor that was already incrementally output.
   When that happens, we output nothing, but we do the bookkeeping
   to skip past that element of the current constructor.  */

void
process_init_element (value)
     tree value;
{
  tree orig_value = value;
  int string_flag = value != 0 && TREE_CODE (value) == STRING_CST;

  /* Handle superfluous braces around string cst as in
     char x[] = {"foo"}; */
  if (string_flag
      && constructor_type
      && TREE_CODE (constructor_type) == ARRAY_TYPE
      && TREE_CODE (TREE_TYPE (constructor_type)) == INTEGER_TYPE
      && integer_zerop (constructor_unfilled_index))
    {
      if (constructor_stack->replacement_value)
        error_init ("excess elements in char array initializer");
      constructor_stack->replacement_value = value;
      return;
    }

  if (constructor_stack->replacement_value != 0)
    {
      error_init ("excess elements in struct initializer");
      return;
    }

  /* Ignore elements of a brace group if it is entirely superfluous
     and has already been diagnosed.  */
  if (constructor_type == 0)
    return;

  /* If we've exhausted any levels that didn't have braces,
     pop them now.  */
  while (constructor_stack->implicit)
    {
      if ((TREE_CODE (constructor_type) == RECORD_TYPE
	   || TREE_CODE (constructor_type) == UNION_TYPE)
	  && constructor_fields == 0)
	process_init_element (pop_init_level (1));
      else if (TREE_CODE (constructor_type) == ARRAY_TYPE
	       && (constructor_max_index == 0
		   || tree_int_cst_lt (constructor_max_index,
				       constructor_index)))
	process_init_element (pop_init_level (1));
      else
	break;
    }

  while (1)
    {
      if (TREE_CODE (constructor_type) == RECORD_TYPE)
	{
	  tree fieldtype;
	  enum tree_code fieldcode;

	  if (constructor_fields == 0)
	    {
	      pedwarn_init ("excess elements in struct initializer");
	      break;
	    }

	  fieldtype = TREE_TYPE (constructor_fields);
	  if (fieldtype != error_mark_node)
	    fieldtype = TYPE_MAIN_VARIANT (fieldtype);
	  fieldcode = TREE_CODE (fieldtype);

	  /* Accept a string constant to initialize a subarray.  */
	  if (value != 0
	      && fieldcode == ARRAY_TYPE
	      && TREE_CODE (TREE_TYPE (fieldtype)) == INTEGER_TYPE
	      && string_flag)
	    value = orig_value;
	  /* Otherwise, if we have come to a subaggregate,
	     and we don't have an element of its type, push into it.  */
	  else if (value != 0 && !constructor_no_implicit
		   && value != error_mark_node
		   && TYPE_MAIN_VARIANT (TREE_TYPE (value)) != fieldtype
		   && (fieldcode == RECORD_TYPE || fieldcode == ARRAY_TYPE
		       || fieldcode == UNION_TYPE))
	    {
	      push_init_level (1);
	      continue;
	    }

	  if (value)
	    {
	      push_member_name (constructor_fields);
	      output_init_element (value, fieldtype, constructor_fields, 1);
	      RESTORE_SPELLING_DEPTH (constructor_depth);
	    }
	  else
	    /* Do the bookkeeping for an element that was
	       directly output as a constructor.  */
	    {
	      /* For a record, keep track of end position of last field.  */
	      tree temp = size_binop (PLUS_EXPR,
				      DECL_FIELD_BITPOS (constructor_fields),
				      DECL_SIZE (constructor_fields));
	      TREE_INT_CST_LOW (constructor_bit_index)
		= TREE_INT_CST_LOW (temp);
	      TREE_INT_CST_HIGH (constructor_bit_index)
		= TREE_INT_CST_HIGH (temp);

	      constructor_unfilled_fields = TREE_CHAIN (constructor_fields);
	      /* Skip any nameless bit fields.  */
	      while (constructor_unfilled_fields != 0
		     && DECL_C_BIT_FIELD (constructor_unfilled_fields)
		     && DECL_NAME (constructor_unfilled_fields) == 0)
		constructor_unfilled_fields =
		  TREE_CHAIN (constructor_unfilled_fields);
	    }

	  constructor_fields = TREE_CHAIN (constructor_fields);
	  /* Skip any nameless bit fields at the beginning.  */
	  while (constructor_fields != 0
		 && DECL_C_BIT_FIELD (constructor_fields)
		 && DECL_NAME (constructor_fields) == 0)
	    constructor_fields = TREE_CHAIN (constructor_fields);
	  break;
	}
      if (TREE_CODE (constructor_type) == UNION_TYPE)
	{
	  tree fieldtype;
	  enum tree_code fieldcode;

	  if (constructor_fields == 0)
	    {
	      pedwarn_init ("excess elements in union initializer");
	      break;
	    }

	  fieldtype = TREE_TYPE (constructor_fields);
	  if (fieldtype != error_mark_node)
	    fieldtype = TYPE_MAIN_VARIANT (fieldtype);
	  fieldcode = TREE_CODE (fieldtype);

	  /* Accept a string constant to initialize a subarray.  */
	  if (value != 0
	      && fieldcode == ARRAY_TYPE
	      && TREE_CODE (TREE_TYPE (fieldtype)) == INTEGER_TYPE
	      && string_flag)
	    value = orig_value;
	  /* Otherwise, if we have come to a subaggregate,
	     and we don't have an element of its type, push into it.  */
	  else if (value != 0 && !constructor_no_implicit
		   && value != error_mark_node
		   && TYPE_MAIN_VARIANT (TREE_TYPE (value)) != fieldtype
		   && (fieldcode == RECORD_TYPE || fieldcode == ARRAY_TYPE
		       || fieldcode == UNION_TYPE))
	    {
	      push_init_level (1);
	      continue;
	    }

	  if (value)
	    {
	      push_member_name (constructor_fields);
	      output_init_element (value, fieldtype, constructor_fields, 1);
	      RESTORE_SPELLING_DEPTH (constructor_depth);
	    }
	  else
	    /* Do the bookkeeping for an element that was
	       directly output as a constructor.  */
	    {
	      TREE_INT_CST_LOW (constructor_bit_index)
		= TREE_INT_CST_LOW (DECL_SIZE (constructor_fields));
	      TREE_INT_CST_HIGH (constructor_bit_index)
		= TREE_INT_CST_HIGH (DECL_SIZE (constructor_fields));

	      constructor_unfilled_fields = TREE_CHAIN (constructor_fields);
	    }

	  constructor_fields = 0;
	  break;
	}
      if (TREE_CODE (constructor_type) == ARRAY_TYPE)
	{
	  tree elttype = TYPE_MAIN_VARIANT (TREE_TYPE (constructor_type));
	  enum tree_code eltcode = TREE_CODE (elttype);

	  /* Accept a string constant to initialize a subarray.  */
	  if (value != 0
	      && eltcode == ARRAY_TYPE
	      && TREE_CODE (TREE_TYPE (elttype)) == INTEGER_TYPE
	      && string_flag)
	    value = orig_value;
	  /* Otherwise, if we have come to a subaggregate,
	     and we don't have an element of its type, push into it.  */
	  else if (value != 0 && !constructor_no_implicit
		   && value != error_mark_node
		   && TYPE_MAIN_VARIANT (TREE_TYPE (value)) != elttype
		   && (eltcode == RECORD_TYPE || eltcode == ARRAY_TYPE
		       || eltcode == UNION_TYPE))
	    {
	      push_init_level (1);
	      continue;
	    }

	  if (constructor_max_index != 0
	      && tree_int_cst_lt (constructor_max_index, constructor_index))
	    {
	      pedwarn_init ("excess elements in array initializer");
	      break;
	    }

	  /* In the case of [LO .. HI] = VALUE, only evaluate VALUE once.  */
	  if (constructor_range_end)
	    {
	      if (constructor_max_index != 0
		  && tree_int_cst_lt (constructor_max_index, 
				      constructor_range_end))
		{
		  pedwarn_init ("excess elements in array initializer");
		  TREE_INT_CST_HIGH (constructor_range_end)
		    = TREE_INT_CST_HIGH (constructor_max_index);
		  TREE_INT_CST_LOW (constructor_range_end)
		    = TREE_INT_CST_LOW (constructor_max_index);
		}

	      value = save_expr (value);
	    }

	  /* Now output the actual element.
	     Ordinarily, output once.
	     If there is a range, repeat it till we advance past the range.  */
	  do
	    {
	      tree tem;

	      if (value)
		{
		  push_array_bounds (TREE_INT_CST_LOW (constructor_index));
		  output_init_element (value, elttype, constructor_index, 1);
		  RESTORE_SPELLING_DEPTH (constructor_depth);
		}

	      tem = size_binop (PLUS_EXPR, constructor_index,
				integer_one_node);
	      TREE_INT_CST_LOW (constructor_index) = TREE_INT_CST_LOW (tem);
	      TREE_INT_CST_HIGH (constructor_index) = TREE_INT_CST_HIGH (tem);

	      if (!value)
		/* If we are doing the bookkeeping for an element that was
		   directly output as a constructor,
		   we must update constructor_unfilled_index.  */
		{
		  TREE_INT_CST_LOW (constructor_unfilled_index)
		    = TREE_INT_CST_LOW (constructor_index);
		  TREE_INT_CST_HIGH (constructor_unfilled_index)
		    = TREE_INT_CST_HIGH (constructor_index);
		}
	    }
	  while (! (constructor_range_end == 0
		    || tree_int_cst_lt (constructor_range_end,
					constructor_index)));

	  break;
	}

      /* Handle the sole element allowed in a braced initializer
	 for a scalar variable.  */
      if (constructor_fields == 0)
	{
	  pedwarn_init ("excess elements in scalar initializer");
	  break;
	}

      if (value)
	output_init_element (value, constructor_type, NULL_TREE, 1);
      constructor_fields = 0;
      break;
    }

  /* If the (lexically) previous elments are not now saved,
     we can discard the storage for them.  */
  if (constructor_incremental && constructor_pending_elts == 0 && value != 0
      && constructor_stack == 0)
    clear_momentary ();
}

/* Expand an ASM statement with operands, handling output operands
   that are not variables or INDIRECT_REFS by transforming such
   cases into cases that expand_asm_operands can handle.

   Arguments are same as for expand_asm_operands.  */

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

  if (TREE_CODE (string) == ADDR_EXPR)
    string = TREE_OPERAND (string, 0);
  if (TREE_CODE (string) != STRING_CST)
    {
      error ("asm template is not a string constant");
      return;
    }

  /* Record the contents of OUTPUTS before it is modified.  */
  for (i = 0, tail = outputs; tail; tail = TREE_CHAIN (tail), i++)
    {
      tree output = TREE_VALUE (tail);

      /* We can remove conversions that just change the type, not the mode.  */
      STRIP_NOPS (output);
      o[i] = output;

      /* Allow conversions as LHS here.  build_modify_expr as called below
	 will do the right thing with them.  */
      while (TREE_CODE (output) == NOP_EXPR
	     || TREE_CODE (output) == CONVERT_EXPR
	     || TREE_CODE (output) == FLOAT_EXPR
	     || TREE_CODE (output) == FIX_TRUNC_EXPR
	     || TREE_CODE (output) == FIX_FLOOR_EXPR
	     || TREE_CODE (output) == FIX_ROUND_EXPR
	     || TREE_CODE (output) == FIX_CEIL_EXPR)
	output = TREE_OPERAND (output, 0);

      lvalue_or_else (o[i], "invalid lvalue in asm statement");
    }

  /* Perform default conversions on array and function inputs.  */
  /* Don't do this for other types--
     it would screw up operands expected to be in memory.  */
  for (i = 0, tail = inputs; tail; tail = TREE_CHAIN (tail), i++)
    if (TREE_CODE (TREE_TYPE (TREE_VALUE (tail))) == ARRAY_TYPE
	|| TREE_CODE (TREE_TYPE (TREE_VALUE (tail))) == FUNCTION_TYPE)
      TREE_VALUE (tail) = default_conversion (TREE_VALUE (tail));

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
		       NULL_RTX, VOIDmode, EXPAND_NORMAL);
	  free_temp_slots ();
	}
      /* Detect modification of read-only values.
	 (Otherwise done by build_modify_expr.)  */
      else
	{
	  tree type = TREE_TYPE (o[i]);
	  if (TREE_READONLY (o[i])
	      || TYPE_READONLY (type)
	      || ((TREE_CODE (type) == RECORD_TYPE
		   || TREE_CODE (type) == UNION_TYPE)
		  && C_TYPE_FIELDS_READONLY (type)))
	    readonly_warning (o[i], "modification by `asm'");
	}
    }

  /* Those MODIFY_EXPRs could do autoincrements.  */
  emit_queue ();
}

/* Expand a C `return' statement.
   RETVAL is the expression for what to return,
   or a null pointer for `return;' with no value.  */

void
c_expand_return (retval)
     tree retval;
{
  tree valtype = TREE_TYPE (TREE_TYPE (current_function_decl));

  if (TREE_THIS_VOLATILE (current_function_decl))
    warning ("function declared `noreturn' has a `return' statement");

  if (!retval)
    {
      current_function_returns_null = 1;
      if (warn_return_type && valtype != 0 && TREE_CODE (valtype) != VOID_TYPE)
	warning ("`return' with no value, in function returning non-void");
      expand_null_return ();
    }
  else if (valtype == 0 || TREE_CODE (valtype) == VOID_TYPE)
    {
      current_function_returns_null = 1;
      if (pedantic || TREE_CODE (TREE_TYPE (retval)) != VOID_TYPE)
	pedwarn ("`return' with a value, in function returning void");
      expand_return (retval);
    }
  else
    {
      tree t = convert_for_assignment (valtype, retval, _("return"),
				       NULL_TREE, NULL_TREE, 0);
      tree res = DECL_RESULT (current_function_decl);
      tree inner;

      if (t == error_mark_node)
	return;

      inner = t = convert (TREE_TYPE (res), t);

      /* Strip any conversions, additions, and subtractions, and see if
	 we are returning the address of a local variable.  Warn if so.  */
      while (1)
	{
	  switch (TREE_CODE (inner))
	    {
	    case NOP_EXPR:   case NON_LVALUE_EXPR:  case CONVERT_EXPR:
	    case PLUS_EXPR:
	      inner = TREE_OPERAND (inner, 0);
	      continue;

	    case MINUS_EXPR:
	      /* If the second operand of the MINUS_EXPR has a pointer
		 type (or is converted from it), this may be valid, so
		 don't give a warning.  */
	      {
		tree op1 = TREE_OPERAND (inner, 1);

		while (! POINTER_TYPE_P (TREE_TYPE (op1))
		       && (TREE_CODE (op1) == NOP_EXPR
			   || TREE_CODE (op1) == NON_LVALUE_EXPR
			   || TREE_CODE (op1) == CONVERT_EXPR))
		  op1 = TREE_OPERAND (op1, 0);

		if (POINTER_TYPE_P (TREE_TYPE (op1)))
		  break;

		inner = TREE_OPERAND (inner, 0);
		continue;
	      }
	      
	    case ADDR_EXPR:
	      inner = TREE_OPERAND (inner, 0);

	      while (TREE_CODE_CLASS (TREE_CODE (inner)) == 'r')
		inner = TREE_OPERAND (inner, 0);

	      if (TREE_CODE (inner) == VAR_DECL
		  && ! DECL_EXTERNAL (inner)
		  && ! TREE_STATIC (inner)
		  && DECL_CONTEXT (inner) == current_function_decl)
		warning ("function returns address of local variable");
	      break;

	    default:
	      break;
	    }

	  break;
	}

      t = build (MODIFY_EXPR, TREE_TYPE (res), res, t);
      TREE_SIDE_EFFECTS (t) = 1;
      expand_return (t);
      current_function_returns_value = 1;
    }
}

/* Start a C switch statement, testing expression EXP.
   Return EXP if it is valid, an error node otherwise.  */

tree
c_expand_start_case (exp)
     tree exp;
{
  register enum tree_code code;
  tree type;

  if (TREE_CODE (exp) == ERROR_MARK)
    return exp;

  code = TREE_CODE (TREE_TYPE (exp));
  type = TREE_TYPE (exp);

  if (code != INTEGER_TYPE && code != ENUMERAL_TYPE && code != ERROR_MARK)
    {
      error ("switch quantity not an integer");
      exp = error_mark_node;
    }
  else
    {
      tree index;
      type = TYPE_MAIN_VARIANT (TREE_TYPE (exp));

      if (warn_traditional
	  && ! in_system_header
	  && (type == long_integer_type_node
	      || type == long_unsigned_type_node))
	pedwarn ("`long' switch expression not converted to `int' in ANSI C");

      exp = default_conversion (exp);
      type = TREE_TYPE (exp);
      index = get_unwidened (exp, NULL_TREE);
      /* We can't strip a conversion from a signed type to an unsigned,
	 because if we did, int_fits_type_p would do the wrong thing
	 when checking case values for being in range,
	 and it's too hard to do the right thing.  */
      if (TREE_UNSIGNED (TREE_TYPE (exp))
	  == TREE_UNSIGNED (TREE_TYPE (index)))
	exp = index;
    }

  expand_start_case (1, exp, type, "switch statement");

  return exp;
}
