/* Build expressions with type checking for C compiler.
   Copyright (C) 1987, 1988, 1989, 1992 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* This file is part of the C front end.
   It contains routines to build C expressions given their operands,
   including computing the types of the result, C-specific error checks,
   and some optimization.

   There are also routines to build RETURN_STMT nodes and CASE_STMT nodes,
   and to process initializations in declarations (since they work
   like a strange sort of assignment).  */

#include "config.h"
#include <stdio.h>
#include "tree.h"
#include "c-tree.h"
#include "flags.h"

/* Nonzero if we've already printed a "partly bracketed initializer"
   message within this initializer.  */
static int partial_bracket_mentioned = 0;

extern char *index ();
extern char *rindex ();

int mark_addressable ();
static tree convert_for_assignment ();
static void warn_for_assignment ();
static int function_types_compatible_p ();
static int type_lists_compatible_p ();
int self_promoting_args_p ();
static int self_promoting_type_p ();
static int comp_target_types ();
static tree pointer_int_sum ();
static tree pointer_diff ();
static tree convert_sequence ();
static tree unary_complex_lvalue ();
static tree process_init_constructor ();
static tree convert_arguments ();
static char *get_spelling ();
tree digest_init ();
static void pedantic_lvalue_warning ();
tree truthvalue_conversion ();
void incomplete_type_error ();
void readonly_warning ();
static tree internal_build_compound_expr ();


/* Do `exp = require_complete_type (exp);' to make sure exp
   does not have an incomplete type.  (That includes void types.)  */

tree
require_complete_type (value)
     tree value;
{
  tree type = TREE_TYPE (value);

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
  char *errmsg;

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
	  errmsg = "invalid use of undefined type `struct %s'";
	  break;

	case UNION_TYPE:
	  errmsg = "invalid use of undefined type `union %s'";
	  break;

	case ENUMERAL_TYPE:
	  errmsg = "invalid use of undefined type `enum %s'";
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
	error (errmsg, IDENTIFIER_POINTER (TYPE_NAME (type)));
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
  int constflag = TYPE_READONLY (type) || TYPE_READONLY (like);
  int volflag = TYPE_VOLATILE (type) || TYPE_VOLATILE (like);
  return c_build_type_variant (type, constflag, volflag);
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

  /* Save time if the two types are the same.  */

  if (t1 == t2) return t1;

  /* If one type is nonsense, use the other.  */
  if (t1 == error_mark_node)
    return t2;
  if (t2 == error_mark_node)
    return t1;

  /* Treat an enum type as the unsigned integer type of the same width.  */

  if (TREE_CODE (t1) == ENUMERAL_TYPE)
    t1 = type_for_size (TYPE_PRECISION (t1), 1);
  if (TREE_CODE (t2) == ENUMERAL_TYPE)
    t2 = type_for_size (TYPE_PRECISION (t2), 1);

  code1 = TREE_CODE (t1);
  code2 = TREE_CODE (t2);

  /* If one type is complex, form the common type
     of the non-complex components,
     then make that complex.  */
  if (code1 == COMPLEX_TYPE || code2 == COMPLEX_TYPE)
    {
      tree subtype1, subtype2, subtype;
      if (code1 == COMPLEX_TYPE)
	subtype1 = TREE_TYPE (t1);
      else
	subtype1 = t1;
      if (code2 == COMPLEX_TYPE)
	subtype2 = TREE_TYPE (t2);
      else
	subtype2 = t2;
      subtype = common_type (subtype1, subtype2);
      return build_complex_type (subtype);
    }

  switch (code1)
    {
    case INTEGER_TYPE:
    case REAL_TYPE:
      /* If only one is real, use it as the result.  */

      if (code1 == REAL_TYPE && code2 != REAL_TYPE)
	return t1;

      if (code2 == REAL_TYPE && code1 != REAL_TYPE)
	return t2;

      /* Both real or both integers; use the one with greater precision.  */

      if (TYPE_PRECISION (t1) > TYPE_PRECISION (t2))
	return t1;
      else if (TYPE_PRECISION (t2) > TYPE_PRECISION (t1))
	return t2;

      /* Same precision.  Prefer longs to ints even when same size.  */

      if (t1 == long_unsigned_type_node
	  || t2 == long_unsigned_type_node)
	return long_unsigned_type_node;

      if (t1 == long_integer_type_node
	  || t2 == long_integer_type_node)
	{
	  /* But preserve unsignedness from the other type,
	     since long cannot hold all the values of an unsigned int.  */
	  if (TREE_UNSIGNED (t1) || TREE_UNSIGNED (t2))
	    return long_unsigned_type_node;
	  return long_integer_type_node;
	}

      /* Otherwise prefer the unsigned one.  */

      if (TREE_UNSIGNED (t1))
	return t1;
      else return t2;

    case POINTER_TYPE:
      /* For two pointers, do this recursively on the target type,
	 and combine the qualifiers of the two types' targets.  */
      /* This code was turned off; I don't know why.
	 But ANSI C specifies doing this with the qualifiers.
	 So I turned it on again.  */
      {
	tree target = common_type (TYPE_MAIN_VARIANT (TREE_TYPE (t1)),
				   TYPE_MAIN_VARIANT (TREE_TYPE (t2)));
	int constp
	  = TYPE_READONLY (TREE_TYPE (t1)) || TYPE_READONLY (TREE_TYPE (t2));
	int volatilep
	  = TYPE_VOLATILE (TREE_TYPE (t1)) || TYPE_VOLATILE (TREE_TYPE (t2));
	return build_pointer_type (c_build_type_variant (target, constp, volatilep));
      }
#if 0
      return build_pointer_type (common_type (TREE_TYPE (t1), TREE_TYPE (t2)));
#endif

    case ARRAY_TYPE:
      {
	tree elt = common_type (TREE_TYPE (t1), TREE_TYPE (t2));
	/* Save space: see if the result is identical to one of the args.  */
	if (elt == TREE_TYPE (t1) && TYPE_DOMAIN (t1))
	  return t1;
	if (elt == TREE_TYPE (t2) && TYPE_DOMAIN (t2))
	  return t2;
	/* Merge the element types, and have a size if either arg has one.  */
	return build_array_type (elt, TYPE_DOMAIN (TYPE_DOMAIN (t1) ? t1 : t2));
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
	  return t1;
	if (valtype == TREE_TYPE (t2) && ! TYPE_ARG_TYPES (t1))
	  return t2;

	/* Simple way if one arg fails to specify argument types.  */
	if (TYPE_ARG_TYPES (t1) == 0)
	  return build_function_type (valtype, TYPE_ARG_TYPES (t2));
	if (TYPE_ARG_TYPES (t2) == 0)
	  return build_function_type (valtype, TYPE_ARG_TYPES (t1));

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

	return build_function_type (valtype, newargs);
      }

    default:
      return t1;
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

  /* Suppress errors caused by previously reported errors.  */

  if (t1 == t2 || TREE_CODE (t1) == ERROR_MARK || TREE_CODE (t2) == ERROR_MARK)
    return 1;

  /* Treat an enum type as the unsigned integer type of the same width.  */

  if (TREE_CODE (t1) == ENUMERAL_TYPE)
    t1 = type_for_size (TYPE_PRECISION (t1), 1);
  if (TREE_CODE (t2) == ENUMERAL_TYPE)
    t2 = type_for_size (TYPE_PRECISION (t2), 1);

  if (t1 == t2)
    return 1;

  /* Different classes of types can't be compatible.  */

  if (TREE_CODE (t1) != TREE_CODE (t2)) return 0;

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

  switch (TREE_CODE (t1))
    {
    case POINTER_TYPE:
      return (TREE_TYPE (t1) == TREE_TYPE (t2)
	      ? 1 : comptypes (TREE_TYPE (t1), TREE_TYPE (t2)));

    case FUNCTION_TYPE:
      return function_types_compatible_p (t1, t2);

    case ARRAY_TYPE:
      {
	/* 1 if no need for warning yet, 2 if warning cause has been seen.  */
	int val = 1;
	tree d1 = TYPE_DOMAIN (t1);
	tree d2 = TYPE_DOMAIN (t2);

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
	  return val;

	return (((TREE_INT_CST_LOW (TYPE_MIN_VALUE (d1))
		  == TREE_INT_CST_LOW (TYPE_MIN_VALUE (d2)))
		 && (TREE_INT_CST_HIGH (TYPE_MIN_VALUE (d1))
		     == TREE_INT_CST_HIGH (TYPE_MIN_VALUE (d2)))
		 && (TREE_INT_CST_LOW (TYPE_MAX_VALUE (d1))
		     == TREE_INT_CST_LOW (TYPE_MAX_VALUE (d2)))
		 && (TREE_INT_CST_HIGH (TYPE_MAX_VALUE (d1))
		     == TREE_INT_CST_HIGH (TYPE_MAX_VALUE (d2))))
		? val : 0);
      }

    case RECORD_TYPE:
      if (maybe_objc_comptypes (t1, t2, 0) == 1)
	return 1;
    }
  return 0;
}

/* Return 1 if TTL and TTR are pointers to types that are equivalent,
   ignoring their qualifiers.  */

static int
comp_target_types (ttl, ttr)
     tree ttl, ttr;
{
  int val;

  /* Give maybe_objc_comptypes a crack at letting these types through.  */
  if (val = maybe_objc_comptypes (ttl, ttr, 1) >= 0)
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
  int newval;

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
	  if (! self_promoting_type_p (TREE_VALUE (args2)))
	    return 0;
	}
      else if (TREE_VALUE (args2) == 0)
	{
	  if (! self_promoting_type_p (TREE_VALUE (args1)))
	    return 0;
	}
      else if (! (newval = comptypes (TREE_VALUE (args1), TREE_VALUE (args2))))
	{
	  /* Allow  wait (union {union wait *u; int *i} *)
	     and  wait (union wait *)  to be compatible.  */
	  if (TREE_CODE (TREE_VALUE (args1)) == UNION_TYPE
	      && TYPE_NAME (TREE_VALUE (args1)) == 0
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
		   && TYPE_NAME (TREE_VALUE (args2)) == 0
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

/* Return 1 if TYPE is not affected by default promotions.  */

static int
self_promoting_type_p (type)
     tree type;
{
  if (TYPE_MAIN_VARIANT (type) == float_type_node)
    return 0;

  if (C_PROMOTING_INTEGER_TYPE_P (type))
    return 0;

  return 1;
}

/* Return an unsigned type the same as TYPE in other respects.  */

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
  return type;
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
  return type;
}

/* Return a type the same as TYPE except unsigned or
   signed according to UNSIGNEDP.  */

tree
signed_or_unsigned_type (unsignedp, type)
     int unsignedp;
     tree type;
{
  if (TREE_CODE (type) != INTEGER_TYPE)
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
  /* size_binop does not put the constant in range, so do it now.  */
  if (TREE_CODE (t) == INTEGER_CST)
    TREE_CONSTANT_OVERFLOW (t) |= force_fit_type (t, 0);
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
      && DECL_BIT_FIELD (TREE_OPERAND (expr, 1)))
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
  if (! TREE_PUBLIC (decl)
      /* Don't change a variable array bound or initial value to a constant
	 in a place where a variable is invalid.  */
      && current_function_decl != 0
      && ! pedantic
      && ! TREE_THIS_VOLATILE (decl)
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
  /* Replace a nonvolatile const static variable with its value.  */
  else if (optimize
	   && TREE_CODE (exp) == VAR_DECL
	   && TREE_READONLY (exp)
	   /* But not for iterators!  */
	   && !ITERATOR_P (exp)
	   && DECL_MODE (exp) != BLKmode)
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
			    (flag_traditional && TREE_UNSIGNED (type)));
      return convert (type, exp);
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
  if (flag_traditional && TYPE_MAIN_VARIANT (type) == float_type_node)
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

      if (TREE_CODE (exp) == INDIRECT_REF)
	return convert (TYPE_POINTER_TO (restype),
			TREE_OPERAND (exp, 0));

      if (TREE_CODE (exp) == COMPOUND_EXPR)
	{
	  tree op1 = default_conversion (TREE_OPERAND (exp, 1));
	  return build (COMPOUND_EXPR, TREE_TYPE (op1),
			TREE_OPERAND (exp, 0), op1);
	}

      if (!lvalue_p (exp)
	  && ! (TREE_CODE (exp) == CONSTRUCTOR && TREE_STATIC (exp)))
	{
	  error ("invalid use of non-lvalue array");
	  return error_mark_node;
	}

      if (TYPE_READONLY (type) || TYPE_VOLATILE (type))
	restype = c_build_type_variant (restype, TYPE_READONLY (type),
					TYPE_VOLATILE (type));

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
    }

  /* See if there is a field or component with name COMPONENT.  */

  if (code == RECORD_TYPE || code == UNION_TYPE)
    {
      if (TYPE_SIZE (type) == 0)
	{
	  incomplete_type_error (NULL_TREE, type);
	  return error_mark_node;
	}

      /* Look up component name in the structure type definition.

	 If TYPE_LANG_SPECIFIC is set, then it is a sorted array of pointers
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
	      int cmp;

	      half = (top - bot + 1) >> 1;
	      field = field_array[bot+half];
	      cmp = (long)DECL_NAME (field) - (long)component;
	      if (cmp == 0)
		break;
	      if (cmp < 0)
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
	      if (DECL_NAME (field) == component)
		break;
	    }
	}

      if (!field)
	{
	  error (code == RECORD_TYPE
		 ? "structure has no member named `%s'"
		 : "union has no member named `%s'",
		 IDENTIFIER_POINTER (component));
	  return error_mark_node;
	}
      if (TREE_TYPE (field) == error_mark_node)
	return error_mark_node;

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
     char *errorstring;
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
	  if (TREE_CODE (t) == VOID_TYPE)
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
	  TREE_THIS_VOLATILE (ref) = TYPE_VOLATILE (t) || flag_volatile;
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

    return build_indirect_ref (build_binary_op (PLUS_EXPR, ar, ind, 0),
			       "array indexing");
  }
}

/* Check a printf/fprintf/sprintf/scanf/fscanf/sscanf format against PARAMS.  */

#define ISDIGIT(c)	((c) >= '0' && (c) <= '9')

#define T_I	&integer_type_node
#define T_L	&long_integer_type_node
#define T_S	&short_integer_type_node
#define T_UI	&unsigned_type_node
#define T_UL	&long_unsigned_type_node
#define T_US	&short_unsigned_type_node
#define T_F	&float_type_node
#define T_D	&double_type_node
#define T_LD	&long_double_type_node
#define T_C	&char_type_node
#define T_V	&void_type_node
#define T_W	&wchar_type_node

typedef struct
{
  char *format_chars;
  int pointer_count;
  /* Type of argument if no length modifier is used.  */
  tree *nolen;
  /* Type of argument if length modifier for shortening is used.
     If NULL, then this modifier is not allowed.  */
  tree *hlen;
  /* Type of argument if length modifier `l' is used.
     If NULL, then this modifier is not allowed.  */
  tree *llen;
  /* Type of argument if length modifier `L' is used.
     If NULL, then this modifier is not allowed.  */
  tree *bigllen;
  /* List of other modifier characters allowed with these options.  */
  char *flag_chars;
} format_char_info;

static format_char_info print_table[]
  = {
      { "di",		0,	T_I,	T_I,	T_L,	NULL,	"-wp0 +" },
      { "oxX",		0,	T_UI,	T_UI,	T_UL,	NULL,	"-wp0#" },
      { "u",		0,	T_UI,	T_UI,	T_UL,	NULL,	"-wp0" },
      { "feEgG",	0,	T_D,	NULL,	NULL,	T_LD,	"-wp0 +#" },
      { "c",		0,	T_I,	NULL,	T_W,	NULL,	"-w" },
      { "C",		0,	T_W,	NULL,	NULL,	NULL,	"-w" },
      { "s",		1,	T_C,	NULL,	T_W,	NULL,	"-wp" },
      { "S",		1,	T_W,	NULL,	NULL,	NULL,	"-wp" },
      { "p",		1,	T_V,	NULL,	NULL,	NULL,	"-" },
      { "n",		1,	T_I,	T_S,	T_L,	NULL,	"" },
      { NULL }
    };

static format_char_info scan_table[]
  = {
      { "di",		1,	T_I,	T_S,	T_L,	NULL,	"*" },
      { "ouxX",		1,	T_UI,	T_US,	T_UL,	NULL,	"*" },	
      { "efgEG",	1,	T_F,	NULL,	T_D,	T_LD,	"*" },
      { "sc",		1,	T_C,	NULL,	T_W,	NULL,	"*" },
      { "[",		1,	T_C,	NULL,	NULL,	NULL,	"*" },
      { "C",		1,	T_W,	NULL,	NULL,	NULL,	"*" },
      { "S",		1,	T_W,	NULL,	NULL,	NULL,	"*" },
      { "p",		2,	T_V,	NULL,	NULL,	NULL,	"*" },
      { "n",		1,	T_I,	T_S,	T_L,	NULL,	"" },
      { NULL }
    };

typedef struct
{
  tree function_ident;		/* identifier such as "printf" */
  int is_scan;			/* TRUE if *scanf */
  int format_num;		/* number of format argument */
  int first_arg_num;		/* number of first arg (zero for varargs) */
} function_info;

static unsigned int function_info_entries = 0;
static function_info *function_info_table = NULL;

/* Record information for argument format checking.  FUNCTION_IDENT is
   the identifier node for the name of the function to check (its decl
   need not exist yet).  IS_SCAN is true for scanf-type format checking;
   false indicates printf-style format checking.  FORMAT_NUM is the number
   of the argument which is the format control string (starting from 1).
   FIRST_ARG_NUM is the number of the first actual argument to check
   against teh format string, or zero if no checking is not be done
   (e.g. for varargs such as vfprintf).  */

void
record_format_info (function_ident, is_scan, format_num, first_arg_num)
      tree function_ident;
      int is_scan;
      int format_num;
      int first_arg_num;
{
  function_info *info;
  
  function_info_entries++;
  if (function_info_table)
    function_info_table
      = (function_info *) xrealloc (function_info_table,
				    function_info_entries * sizeof (function_info));
  else
    function_info_table = (function_info *) xmalloc (sizeof (function_info));

  info = &function_info_table[function_info_entries - 1];
  
  info->function_ident = function_ident;
  info->is_scan = is_scan;
  info->format_num = format_num;
  info->first_arg_num = first_arg_num;
}

/* Initialize the table of functions to perform format checking on.
   The ANSI functions are always checked (whether <stdio.h> is
   included or not), since it is common to call printf without
   including <stdio.h>.  There shouldn't be a problem with this,
   since ANSI reserves these function names whether you include the
   header file or not.  In any case, the checking is harmless.  */

void
init_format_info_table ()
{
  record_format_info (get_identifier ("printf"), 0, 1, 2);
  record_format_info (get_identifier ("fprintf"), 0, 2, 3);
  record_format_info (get_identifier ("sprintf"), 0, 2, 3);
  record_format_info (get_identifier ("scanf"), 1, 1, 2);
  record_format_info (get_identifier ("fscanf"), 1, 2, 3);
  record_format_info (get_identifier ("sscanf"), 1, 2, 3);
  record_format_info (get_identifier ("vprintf"), 0, 1, 0);
  record_format_info (get_identifier ("vfprintf"), 0, 2, 0);
  record_format_info (get_identifier ("vsprintf"), 0, 2, 0);
}

static char	tfaff[] = "too few arguments for format";

/* Check the argument list of a call to printf, scanf, etc.
   INFO points to the element of function_info_table.
   PARAMS is the list of argument values.  */

static void
check_format (info, params)
     function_info *info;
     tree params;
{
  int i;
  int arg_num;
  int suppressed, wide, precise;
  int length_char;
  int format_char;
  int format_length;
  tree format_tree;
  tree cur_param;
  tree cur_type;
  tree wanted_type;
  char *format_chars;
  format_char_info *fci;
  static char message[132];
  char flag_chars[8];

  /* Skip to format argument.  If the argument isn't available, there's
     no work for us to do; prototype checking will catch the problem.  */
  for (arg_num = 1; ; ++arg_num)
    {
      if (params == 0)
	return;
      if (arg_num == info->format_num)
	break;
      params = TREE_CHAIN (params);
    }
  format_tree = TREE_VALUE (params);
  params = TREE_CHAIN (params);
  if (format_tree == 0)
    return;
  /* We can only check the format if it's a string constant.  */
  while (TREE_CODE (format_tree) == NOP_EXPR)
    format_tree = TREE_OPERAND (format_tree, 0); /* strip coercion */
  if (format_tree == null_pointer_node)
    {
      warning ("null format string");
      return;
    }
  if (TREE_CODE (format_tree) != ADDR_EXPR)
    return;
  format_tree = TREE_OPERAND (format_tree, 0);
  if (TREE_CODE (format_tree) != STRING_CST)
    return;
  format_chars = TREE_STRING_POINTER (format_tree);
  format_length = TREE_STRING_LENGTH (format_tree);
  if (format_length <= 1)
    warning ("zero-length format string");
  if (format_chars[--format_length] != 0)
    {
      warning ("unterminated format string");
      return;
    }
  /* Skip to first argument to check.  */
  while (arg_num + 1 < info->first_arg_num)
    {
      if (params == 0)
	return;
      params = TREE_CHAIN (params);
      ++arg_num;
    }
  while (1)
    {
      if (*format_chars == 0)
	{
	  if (format_chars - TREE_STRING_POINTER (format_tree) != format_length)
	    warning ("embedded `\\0' in format");
	  if (info->first_arg_num != 0 && params != 0)
	    warning ("too many arguments for format");
	  return;
	}
      if (*format_chars++ != '%')
	continue;
      if (*format_chars == 0)
	{
	  warning ("spurious trailing `%%' in format");
	  continue;
	}
      if (*format_chars == '%')
	{
	  ++format_chars;
	  continue;
	}
      flag_chars[0] = 0;
      suppressed = wide = precise = FALSE;
      if (info->is_scan)
	{
	  suppressed = *format_chars == '*';
	  if (suppressed)
	    ++format_chars;
	  while (ISDIGIT (*format_chars))
	    ++format_chars;
	}
      else
	{
	  while (*format_chars != 0 && index (" +#0-", *format_chars) != 0)
	    {
	      if (index (flag_chars, *format_chars) != 0)
		{
		  sprintf (message, "repeated `%c' flag in format",
			   *format_chars);
		  warning (message);
		}
	      i = strlen (flag_chars);
	      flag_chars[i++] = *format_chars++;
	      flag_chars[i] = 0;
	    }
	  /* "If the space and + flags both appear, 
	     the space flag will be ignored."  */
	  if (index (flag_chars, ' ') != 0
	      && index (flag_chars, '+') != 0)
	    warning ("use of both ` ' and `+' flags in format");
	  /* "If the 0 and - flags both appear,
	     the 0 flag will be ignored."  */
	  if (index (flag_chars, '0') != 0
	      && index (flag_chars, '-') != 0)
	    warning ("use of both `0' and `-' flags in format");
	  if (*format_chars == '*')
	    {
	      wide = TRUE;
	      /* "...a field width...may be indicated by an asterisk.
		 In this case, an int argument supplies the field width..."  */
	      ++format_chars;
	      if (params == 0)
		{
		  warning (tfaff);
		  return;
		}
	      if (info->first_arg_num != 0)
		{
		  cur_param = TREE_VALUE (params);
		  params = TREE_CHAIN (params);
		  ++arg_num;
		  /* size_t is generally not valid here.
		     It will work on most machines, because size_t and int
		     have the same mode.  But might as well warn anyway,
		     since it will fail on other machines.  */
		  if (TYPE_MAIN_VARIANT (TREE_TYPE (cur_param))
		      != integer_type_node)
		    {
		      sprintf (message,
			       "field width is not type int (arg %d)",
			       arg_num);
		      warning (message);
		    }
		}
	    }
	  else
	    {
	      while (ISDIGIT (*format_chars))
		{
		  wide = TRUE;
		  ++format_chars;
		}
	    }
	  if (*format_chars == '.')
	    {
	      precise = TRUE;
	      ++format_chars;
	      if (*format_chars != '*' && !ISDIGIT (*format_chars))
		warning ("`.' not followed by `*' or digit in format");
	      /* "...a...precision...may be indicated by an asterisk.
		 In this case, an int argument supplies the...precision."  */
	      if (*format_chars == '*')
		{
		  if (info->first_arg_num != 0)
		    {
		      ++format_chars;
		      if (params == 0)
		        {
			  warning (tfaff);
			  return;
			}
		      cur_param = TREE_VALUE (params);
		      params = TREE_CHAIN (params);
		      ++arg_num;
		      if (TYPE_MAIN_VARIANT (TREE_TYPE (cur_param))
			  != integer_type_node)
		        {
		          sprintf (message,
				   "field width is not type int (arg %d)",
				   arg_num);
		          warning (message);
		        }
		    }
		}
	      else
		{
		  while (ISDIGIT (*format_chars))
		    ++format_chars;
		}
	    }
	}
      if (*format_chars == 'h' || *format_chars == 'l' || *format_chars == 'L')
	length_char = *format_chars++;
      else
	length_char = 0;
      if (suppressed && length_char != 0)
	{
	  sprintf (message,
		   "use of `*' and `%c' together in format",
		   length_char);
	  warning (message);
	}
      format_char = *format_chars;
      if (format_char == 0)
	{
	  warning ("conversion lacks type at end of format");
	  continue;
	}
      format_chars++;
      fci = info->is_scan ? scan_table : print_table;
      while (1)
	{
	  if (fci->format_chars == 0
	      || index (fci->format_chars, format_char) != 0)
	    break;
	  ++fci;
	}
      if (fci->format_chars == 0)
	{
	  if (format_char >= 040 && format_char < 0177)
	    sprintf (message,
		     "unknown conversion type character `%c' in format",
		     format_char);
	  else
	    sprintf (message,
		     "unknown conversion type character 0x%x in format",
		     format_char);
	  warning (message);
	  continue;
	}
      if (wide && index (fci->flag_chars, 'w') == 0)
	{
	  sprintf (message, "width used with `%c' format",
		   format_char);
	  warning (message);
	}
      if (precise && index (fci->flag_chars, 'p') == 0)
	{
	  sprintf (message, "precision used with `%c' format",
		   format_char);
	  warning (message);
	}
      if (suppressed)
	{
	  if (index (fci->flag_chars, '*') == 0)
	    {
	      sprintf (message,
		       "suppression of `%c' conversion in format",
		       format_char);
	      warning (message);
	    }
	  continue;
	}
      for (i = 0; flag_chars[i] != 0; ++i)
	{
	  if (index (fci->flag_chars, flag_chars[i]) == 0)
	    {
	      sprintf (message, "flag `%c' used with type `%c'",
		       flag_chars[i], format_char);
	      warning (message);
	    }
	}
      if (precise && index (flag_chars, '0') != 0
	  && (format_char == 'd' || format_char == 'i'
	      || format_char == 'o' || format_char == 'u'
	      || format_char == 'x' || format_char == 'x'))
	{
	  sprintf (message,
		   "precision and `0' flag not both allowed with `%c' format",
		   format_char);
	  warning (message);
	}
      switch (length_char)
	{
	default: wanted_type = fci->nolen ? *(fci->nolen) : 0; break;
	case 'h': wanted_type = fci->hlen ? *(fci->hlen) : 0; break;
	case 'l': wanted_type = fci->llen ? *(fci->llen) : 0; break;
	case 'L': wanted_type = fci->bigllen ? *(fci->bigllen) : 0; break;
	}
      if (wanted_type == 0)
	{
	  sprintf (message,
		   "use of `%c' length character with `%c' type character",
		   length_char, format_char);
	  warning (message);
	}

      /*
       ** XXX -- should kvetch about stuff such as
       **	{
       **		const int	i;
       **
       **		scanf ("%d", &i);
       **	}
       */

      /* Finally. . .check type of argument against desired type!  */
      if (info->first_arg_num == 0)
	continue;
      if (params == 0)
	{
	  warning (tfaff);
	  return;
	}
      cur_param = TREE_VALUE (params);
      params = TREE_CHAIN (params);
      ++arg_num;
      cur_type = TREE_TYPE (cur_param);

      /* Check the types of any additional pointer arguments
	 that precede the "real" argument.  */
      for (i = 0; i < fci->pointer_count; ++i)
	{
	  if (TREE_CODE (cur_type) == POINTER_TYPE)
	    {
	      cur_type = TREE_TYPE (cur_type);
	      continue;
	    }
	  sprintf (message,
		   "format argument is not a %s (arg %d)",
		   ((fci->pointer_count == 1) ? "pointer" : "pointer to a pointer"),
		   arg_num);
	  warning (message);
	  break;
	}

      /* Check the type of the "real" argument, if there's a type we want.  */
      if (i == fci->pointer_count && wanted_type != 0
	  && wanted_type != TYPE_MAIN_VARIANT (cur_type)
	  /* If we want `void *', allow any pointer type.
	     (Anything else would already have got a warning.)  */
	  && ! (wanted_type == void_type_node
		&& fci->pointer_count > 0)
	  /* Don't warn about differences merely in signedness.  */
	  && !(TREE_CODE (wanted_type) == INTEGER_TYPE
	       && TREE_CODE (cur_type) == INTEGER_TYPE
	       && TYPE_PRECISION (wanted_type) == TYPE_PRECISION (cur_type)))
	{
	  register char *this;
	  register char *that;
  
	  this = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (wanted_type)));
	  that = 0;
	  if (TREE_CODE (cur_type) != ERROR_MARK
	      && TYPE_NAME (cur_type) != 0
	      && TREE_CODE (cur_type) != INTEGER_TYPE
	      && !(TREE_CODE (cur_type) == POINTER_TYPE
		   && TREE_CODE (TREE_TYPE (cur_type)) == INTEGER_TYPE))
	    {
	      if (TREE_CODE (TYPE_NAME (cur_type)) == TYPE_DECL
		  && DECL_NAME (TYPE_NAME (cur_type)) != 0)
		that = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (cur_type)));
	      else
		that = IDENTIFIER_POINTER (TYPE_NAME (cur_type));
	    }

	  /* A nameless type can't possibly match what the format wants.
	     So there will be a warning for it.
	     Make up a string to describe vaguely what it is.  */
	  if (that == 0)
	    {
	      if (TREE_CODE (cur_type) == POINTER_TYPE)
		that = "pointer";
	      else
		that = "different type";
	    }

	  if (strcmp (this, that) != 0)
	    {
	      sprintf (message, "%s format, %s arg (arg %d)",
			this, that, arg_num);
	      warning (message);
	    }
	}
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
  register tree fntype, fundecl;
  register tree coerced_params;
  tree name = NULL_TREE;

  /* Strip NON_LVALUE_EXPRs, etc., since we aren't using as an lvalue.  */
  STRIP_TYPE_NOPS (function);

  /* Convert anything with function type to a pointer-to-function.  */
  if (TREE_CODE (function) == FUNCTION_DECL)
    {
      name = DECL_NAME (function);
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
  if (warn_format && name != 0)
    {
      unsigned int i;

      /* See if this function is a format function.  */
      for (i = 0; i < function_info_entries; i++)
	if (function_info_table[i].function_ident == name)
	  {
	    register char *message;

	    /* If so, check it.  */
	    check_format (&function_info_table[i], coerced_params);
	    break;
	  }
    }

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
	      tree parmname;
#ifdef PROMOTE_PROTOTYPES
	      /* Rather than truncating and then reextending,
		 convert directly to int, if that's the type we will want.  */
	      if (! flag_traditional
		  && TREE_CODE (type) == INTEGER_TYPE
		  && (TYPE_PRECISION (type) < TYPE_PRECISION (integer_type_node)))
		type = integer_type_node;
#endif

#if 0 /* This turns out not to win--there's no way to write a prototype
	 for a function whose arg type is a union with no tag.  */
	      /* Nameless union automatically casts the types it contains.  */
	      if (TREE_CODE (type) == UNION_TYPE && TYPE_NAME (type) == 0)
		{
		  tree field;

		  for (field = TYPE_FIELDS (type); field;
		       field = TREE_CHAIN (field))
		    if (comptypes (TYPE_MAIN_VARIANT (TREE_TYPE (field)),
				   TYPE_MAIN_VARIANT (TREE_TYPE (val))))
		      break;

		  if (field)
		    val = build1 (CONVERT_EXPR, type, val);
		}
#endif

	      /* Optionally warn about conversions that
		 differ from the default conversions.  */
	      if (warn_conversion)
		{
		  int formal_prec = TYPE_PRECISION (type);

		  if (TREE_CODE (type) != REAL_TYPE
		      && TREE_CODE (TREE_TYPE (val)) == REAL_TYPE)
		    warn_for_assignment ("%s as integer rather than floating due to prototype", (char *) 0, name, parmnum + 1);
		  else if (TREE_CODE (type) == REAL_TYPE
		      && TREE_CODE (TREE_TYPE (val)) != REAL_TYPE)
		    warn_for_assignment ("%s as floating rather than integer due to prototype", (char *) 0, name, parmnum + 1);
		  else if (TREE_CODE (type) == REAL_TYPE
			   && TREE_CODE (TREE_TYPE (val)) == REAL_TYPE)
		    {
		      /* Warn if any argument is passed as `float',
			 since without a prototype it would be `double'.  */
		      if (formal_prec == TYPE_PRECISION (float_type_node))
			warn_for_assignment ("%s as `float' rather than `double' due to prototype", (char *) 0, name, parmnum + 1);
		    }
		  /* Detect integer changing in width or signedness.  */
		  else if ((TREE_CODE (type) == INTEGER_TYPE
			    || TREE_CODE (type) == ENUMERAL_TYPE)
			   && (TREE_CODE (TREE_TYPE (val)) == INTEGER_TYPE
			       || TREE_CODE (TREE_TYPE (val)) == ENUMERAL_TYPE))
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
		      else if (TREE_CODE (TREE_TYPE (val)) == ENUMERAL_TYPE
			       && int_fits_type_p (TYPE_MIN_VALUE (TREE_TYPE (val)), type)
			       && int_fits_type_p (TYPE_MAX_VALUE (TREE_TYPE (val)), type))
			/* Change in signedness doesn't matter
			   if an enum value is unaffected.  */
			;
		      else if (TREE_UNSIGNED (type))
			warn_for_assignment ("%s as unsigned due to prototype", (char *) 0, name, parmnum + 1);
		      else
			warn_for_assignment ("%s as signed due to prototype", (char *) 0, name, parmnum + 1);
		    }
		}

	      parmval = convert_for_assignment (type, val, 
					        (char *)0, /* arg passing  */
						fundecl, name, parmnum + 1);
	      
#ifdef PROMOTE_PROTOTYPES
	      if (TREE_CODE (type) == INTEGER_TYPE
		  && (TYPE_PRECISION (type) < TYPE_PRECISION (integer_type_node)))
		parmval = default_conversion (parmval);
#endif
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
	}

      if (code == BIT_XOR_EXPR)
	{
	  if (code1 == BIT_AND_EXPR
	      || code1 == PLUS_EXPR || code1 == MINUS_EXPR
	      || code2 == BIT_AND_EXPR
	      || code2 == PLUS_EXPR || code2 == MINUS_EXPR)
	    warning ("suggest parentheses around arithmetic in operand of ^");
	}

      if (code == BIT_AND_EXPR)
	{
	  if (code1 == PLUS_EXPR || code1 == MINUS_EXPR
	      || code2 == PLUS_EXPR || code2 == MINUS_EXPR)
	    warning ("suggest parentheses around + or - in operand of &");
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
      /* We use NOP_EXPR rather than NON_LVALUE_EXPR
	 so that convert_for_assignment won't strip it.
	 That way, we get warnings for things like p = (1 - 1).  */
      result = build1 (NOP_EXPR, TREE_TYPE (result), result);
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

  /* Nonzero means after finally constructing the expression
     give it this type.  Otherwise, give it type RESULT_TYPE.  */
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
	    /* When dividing two signed integers, you have to promote to int.
	       E.g. (short) -32868 / (short) -1 doesn't fit in a short.  */
	    shorten = TREE_UNSIGNED (op0);
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
	shorten = 1;
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
	  if (TREE_CODE (op1) == INTEGER_CST)
	    {
	      if (tree_int_cst_lt (op1, integer_zero_node))
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
	  if (TREE_CODE (op1) == INTEGER_CST)
	    {
	      if (tree_int_cst_lt (op1, integer_zero_node))
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
      /* Result of comparison is always int,
	 but don't convert the args to int!  */
      result_type = integer_type_node;
      converted = 1;
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
	     Otherwise, the targets must be the same.  */
	  if (comp_target_types (type0, type1))
	    ;
	  else if (TYPE_MAIN_VARIANT (tt0) == void_type_node)
	    {
	      if (pedantic && !integer_zerop (op0)
		  && TREE_CODE (tt1) == FUNCTION_TYPE)
		pedwarn ("ANSI C forbids comparison of `void *' with function pointer");
	    }
	  else if (TYPE_MAIN_VARIANT (tt1) == void_type_node)
	    {
	      if (pedantic && !integer_zerop (op1)
		  && TREE_CODE (tt0) == FUNCTION_TYPE)
		pedwarn ("ANSI C forbids comparison of `void *' with function pointer");
	    }
	  else
	    pedwarn ("comparison of distinct pointer types lacks a cast");
	}
      else if (code0 == POINTER_TYPE && TREE_CODE (op1) == INTEGER_CST
	       && integer_zerop (op1))
	op1 = null_pointer_node;
      else if (code1 == POINTER_TYPE && TREE_CODE (op0) == INTEGER_CST
	       && integer_zerop (op0))
	op0 = null_pointer_node;
      else if (code0 == POINTER_TYPE && code1 == INTEGER_TYPE)
	{
	  if (! flag_traditional)
	    pedwarn ("comparison between pointer and integer");
	  op1 = convert (TREE_TYPE (op0), op1);
	}
      else if (code0 == INTEGER_TYPE && code1 == POINTER_TYPE)
	{
	  if (! flag_traditional)
	    pedwarn ("comparison between pointer and integer");
	  op0 = convert (TREE_TYPE (op1), op0);
	}
      else
	/* If args are not valid, clear out RESULT_TYPE
	   to cause an error message later.  */
	result_type = 0;
      break;

    case MAX_EXPR:
    case MIN_EXPR:
      if ((code0 == INTEGER_TYPE || code0 == REAL_TYPE
	   || code0 == COMPLEX_TYPE)
	  && (code1 == INTEGER_TYPE || code1 == REAL_TYPE
	      || code1 == COMPLEX_TYPE))
	shorten = 1;
      else if (code0 == POINTER_TYPE && code1 == POINTER_TYPE)
	{
	  if (! comp_target_types (type0, type1))
	    pedwarn ("comparison of distinct pointer types lacks a cast");
	  else if (pedantic 
		   && TREE_CODE (TREE_TYPE (type0)) == FUNCTION_TYPE)
	    pedwarn ("ANSI C forbids ordered comparisons of pointers to functions");
	  result_type = common_type (type0, type1);
	}
      break;

    case LE_EXPR:
    case GE_EXPR:
    case LT_EXPR:
    case GT_EXPR:
      if ((code0 == INTEGER_TYPE || code0 == REAL_TYPE
	   || code0 == COMPLEX_TYPE)
	  && (code1 == INTEGER_TYPE || code1 == REAL_TYPE
	      || code1 == COMPLEX_TYPE))
	short_compare = 1;
      else if (code0 == POINTER_TYPE && code1 == POINTER_TYPE)
	{
	  if (! comp_target_types (type0, type1))
	    pedwarn ("comparison of distinct pointer types lacks a cast");
	  else if (pedantic 
		   && TREE_CODE (TREE_TYPE (type0)) == FUNCTION_TYPE)
	    pedwarn ("ANSI C forbids ordered comparisons of pointers to functions");
	  result_type = integer_type_node;
	}
      else if (code0 == POINTER_TYPE && TREE_CODE (op1) == INTEGER_CST
	       && integer_zerop (op1))
	{
	  result_type = integer_type_node;
	  op1 = null_pointer_node;
	  if (! flag_traditional)
	    pedwarn ("ordered comparison of pointer with integer zero");
	}
      else if (code1 == POINTER_TYPE && TREE_CODE (op0) == INTEGER_CST
	       && integer_zerop (op0))
	{
	  result_type = integer_type_node;
	  op0 = null_pointer_node;
	  if (pedantic)
	    pedwarn ("ordered comparison of pointer with integer zero");
	}
      else if (code0 == POINTER_TYPE && code1 == INTEGER_TYPE)
	{
	  result_type = integer_type_node;
	  if (! flag_traditional)
	    pedwarn ("comparison between pointer and integer");
	  op1 = convert (TREE_TYPE (op0), op1);
	}
      else if (code0 == INTEGER_TYPE && code1 == POINTER_TYPE)
	{
	  result_type = integer_type_node;
	  if (! flag_traditional)
	    pedwarn ("comparison between pointer and integer");
	  op0 = convert (TREE_TYPE (op1), op0);
	}
      converted = 1;
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

      if (short_compare && none_complex)
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
	  op0 = xop0, op1 = xop1, result_type = xresult_type;
	  resultcode = xresultcode;

	  if (extra_warnings)
	    {
	      tree op0_type = TREE_TYPE (orig_op0);
	      tree op1_type = TREE_TYPE (orig_op1);
	      int op0_unsigned = TREE_UNSIGNED (op0_type);
	      int op1_unsigned = TREE_UNSIGNED (op1_type);
 
	      /* Give warnings for comparisons between signed and unsigned
		 quantities that will fail.  Do not warn if the signed quantity
		 is an unsuffixed integer literal (or some static constant
		 expression involving such literals) and it is positive.
		 Do not warn if the width of the unsigned quantity is less
		 than that of the signed quantity, since in this case all
		 values of the unsigned quantity fit in the signed quantity.
		 Do not warn if the signed type is the same size as the
		 result_type since sign extension does not cause trouble in
		 this case.  */
	      /* Do the checking based on the original operand trees, so that
		 casts will be considered, but default promotions won't be.  */
	      if (op0_unsigned != op1_unsigned
		  && ((op0_unsigned
		       && TYPE_PRECISION (op0_type) >= TYPE_PRECISION (op1_type)
		       && TYPE_PRECISION (op0_type) < TYPE_PRECISION (result_type)
		       && (TREE_CODE (op1) != INTEGER_CST
			   || (TREE_CODE (op1) == INTEGER_CST
			       && INT_CST_LT (op1, integer_zero_node))))
		      ||
		      (op1_unsigned
		       && TYPE_PRECISION (op1_type) >= TYPE_PRECISION (op0_type)
		       && TYPE_PRECISION (op1_type) < TYPE_PRECISION (result_type)
		       && (TREE_CODE (op0) != INTEGER_CST
			   || (TREE_CODE (op0) == INTEGER_CST
			       && INT_CST_LT (op0, integer_zero_node))))))
		warning ("comparison between signed and unsigned");
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

  {
    register tree result = build (resultcode, result_type, op0, op1);
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
      && TREE_CODE (TREE_TYPE (TREE_OPERAND (intop, 0))) == INTEGER_TYPE)
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

  /* Convert the integer argument to a type the same size as a pointer
     so the multiply won't overflow spuriously.  */

  if (TYPE_PRECISION (TREE_TYPE (intop)) != POINTER_SIZE)
    intop = convert (type_for_size (POINTER_SIZE, 0), intop);

  /* Replace the integer argument
     with a suitable product by the object size.  */

  intop = build_binary_op (MULT_EXPR, intop, size_exp, 1);

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
     then drop through to build the divide operator.  */

  op0 = build_binary_op (MINUS_EXPR, convert (restype, op0),
			 convert (restype, op1), 1);
  /* This generates an error if op1 is pointer to incomplete type.  */
  if (TYPE_SIZE (TREE_TYPE (TREE_TYPE (op1))) == 0)
    error ("arithmetic on pointer to an incomplete type");
  /* This generates an error if op0 is pointer to incomplete type.  */
  op1 = c_size_in_bytes (target_type);

  /* Divide by the size, in easiest possible way.  */

  result = build (EXACT_DIV_EXPR, restype, op0, op1);

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
  char *errstring = NULL;
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
        errstring = "wrong type argument to unary plus";
      else if (!noconvert)
	arg = default_conversion (arg);
      break;

    case NEGATE_EXPR:
      if (!(typecode == INTEGER_TYPE || typecode == REAL_TYPE
	    || typecode == COMPLEX_TYPE))
        errstring = "wrong type argument to unary minus";
      else if (!noconvert)
	arg = default_conversion (arg);
      break;

    case BIT_NOT_EXPR:
      if (typecode != INTEGER_TYPE)
        errstring = "wrong type argument to bit-complement";
      else if (!noconvert)
	arg = default_conversion (arg);
      break;

    case ABS_EXPR:
      if (!(typecode == INTEGER_TYPE || typecode == REAL_TYPE
	    || typecode == COMPLEX_TYPE))
        errstring = "wrong type argument to abs";
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
	  errstring = "wrong type argument to unary exclamation mark";
	  break;
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
	  if (code == PREINCREMENT_EXPR || code == POSTINCREMENT_EXPR)
	    errstring ="wrong type argument to increment";
	  else
	    errstring ="wrong type argument to decrement";
	  break;
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
		       ((code == PREINCREMENT_EXPR
			 || code == POSTINCREMENT_EXPR)
			? "increment" : "decrement"));
	    else if ((pedantic || warn_pointer_arith)
		     && (TREE_CODE (TREE_TYPE (result_type)) == FUNCTION_TYPE
			 || TREE_CODE (TREE_TYPE (result_type)) == VOID_TYPE))
	      pedwarn ("wrong type argument to %s",
		       ((code == PREINCREMENT_EXPR
			 || code == POSTINCREMENT_EXPR)
			? "increment" : "decrement"));
	    inc = c_sizeof_nowarn (TREE_TYPE (result_type));
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
				   ? "increment" : "decrement")))
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
      else if (typecode != FUNCTION_TYPE && !lvalue_or_else (arg, "unary `&'"))
	return error_mark_node;

      /* Ordinary case; arg is a COMPONENT_REF or a decl.  */
      argtype = TREE_TYPE (arg);
      /* If the lvalue is const or volatile,
	 merge that into the type that the address will point to.  */
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

	    if (DECL_BIT_FIELD (field))
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
      if (TREE_CODE (TREE_TYPE (ref)) != FUNCTION_TYPE
	  && TREE_CODE (TREE_TYPE (ref)) != METHOD_TYPE)
	return 1;
      break;
    }
  return 0;
}

/* Return nonzero if REF is an lvalue valid for this language;
   otherwise, print an error message and return zero.  */

int
lvalue_or_else (ref, string)
     tree ref;
     char *string;
{
  int win = lvalue_p (ref);
  if (! win)
    error ("invalid lvalue in %s", string);
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
      pedantic_lvalue_warning (COMPOUND_EXPR);
      return build (COMPOUND_EXPR, TREE_TYPE (real_result),
		    TREE_OPERAND (arg, 0), real_result);
    }

  /* Handle (a ? b : c) used as an "lvalue".  */
  if (TREE_CODE (arg) == COND_EXPR)
    {
      pedantic_lvalue_warning (COND_EXPR);
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
    pedwarn ("ANSI C forbids use of %s expressions as lvalues",
	     code == COND_EXPR ? "conditional"
	     : code == COMPOUND_EXPR ? "compound" : "cast");
}

/* Warn about storing in something that is `const'.  */

void
readonly_warning (arg, string)
     tree arg;
     char *string;
{
  char buf[80];
  strcpy (buf, string);

  /* Forbid assignments to iterators.  */
  if (TREE_CODE (arg) == VAR_DECL && ITERATOR_P (arg))
    {
      strcat (buf, " of iterator `%s'");
      pedwarn (buf, IDENTIFIER_POINTER (DECL_NAME (arg)));
    }

  if (TREE_CODE (arg) == COMPONENT_REF)
    {
      if (TYPE_READONLY (TREE_TYPE (TREE_OPERAND (arg, 0))))
	readonly_warning (TREE_OPERAND (arg, 0), string);
      else
	{
	  strcat (buf, " of read-only member `%s'");
	  pedwarn (buf, IDENTIFIER_POINTER (DECL_NAME (TREE_OPERAND (arg, 1))));
	}
    }
  else if (TREE_CODE (arg) == VAR_DECL)
    {
      strcat (buf, " of read-only variable `%s'");
      pedwarn (buf, IDENTIFIER_POINTER (DECL_NAME (arg)));
    }
  else
    {
      pedwarn ("%s of read-only location", buf);
    }
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
      case ADDR_EXPR:
      case COMPONENT_REF:
      case ARRAY_REF:
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

  /* If second operand is omitted, it is the same as the first one;
     make sure it is calculated only once.  */
  if (op1 == 0)
    {
      if (pedantic)
	pedwarn ("ANSI C forbids omitting the middle term of a ?: expression");
      ifexp = op1 = save_expr (ifexp);
    }

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
	return (integer_zerop (ifexp) ? op2 : op1);

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
      else if (integer_zerop (op1) && TREE_TYPE (type1) == void_type_node)
	result_type = qualify_type (type2, type1);
      else if (integer_zerop (op2) && TREE_TYPE (type2) == void_type_node)
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
	return (integer_zerop (ifexp) ? op2 : op1);

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
    return integer_zerop (ifexp) ? op2 : op1;

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

  /* When pedantic, a compound expression can be neither an lvalue
     nor an integer constant expression.  */
  if (! TREE_SIDE_EFFECTS (TREE_VALUE (list)) && ! pedantic)
    return rest;

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
      for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	if (comptypes (TYPE_MAIN_VARIANT (TREE_TYPE (field)),
		       TYPE_MAIN_VARIANT (TREE_TYPE (value))))
	  break;

      if (field)
	{
	  char *name;
	  tree nvalue;

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
	  return digest_init (type, build_nt (CONSTRUCTOR, NULL_TREE,
					      build_tree_list (field, value)),
			      NULL_PTR, 0, 0, name);
	}
      error ("cast to union type from type not present in union");
      return error_mark_node;
    }
  else
    {
      tree otype;

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
	  if (TYPE_VOLATILE (TREE_TYPE (otype))
	      && ! TYPE_VOLATILE (TREE_TYPE (type)))
	    pedwarn ("cast discards `volatile' from pointer target type");
	  if (TYPE_READONLY (TREE_TYPE (otype))
	      && ! TYPE_READONLY (TREE_TYPE (type)))
	    pedwarn ("cast discards `const' from pointer target type");
	}

      /* Warn about possible alignment problems.  */
      if (STRICT_ALIGNMENT && warn_cast_align
	  && TREE_CODE (type) == POINTER_TYPE
	  && TREE_CODE (otype) == POINTER_TYPE
	  && TREE_CODE (TREE_TYPE (otype)) != VOID_TYPE
	  && TREE_CODE (TREE_TYPE (otype)) != FUNCTION_TYPE
	  && TYPE_ALIGN (TREE_TYPE (type)) > TYPE_ALIGN (TREE_TYPE (otype)))
	warning ("cast increases required alignment of target type");

      if (TREE_CODE (type) == INTEGER_TYPE
	  && TREE_CODE (otype) == POINTER_TYPE
	  && TYPE_PRECISION (type) != TYPE_PRECISION (otype)
	  && !TREE_CONSTANT (value))
	warning ("cast from pointer to integer of different size");

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

      value = convert (type, value);

      /* Ignore any integer overflow caused by the cast.  */
      if (TREE_CODE (value) == INTEGER_CST)
	TREE_CONSTANT_OVERFLOW (value) = 0;
    }

  if (value == expr && pedantic)
    {
      /* If pedantic, don't let a cast be an lvalue.  */
      return non_lvalue (value);
    }
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
      return build (COMPOUND_EXPR, lhstype,
		    TREE_OPERAND (lhs, 0),
		    build_modify_expr (TREE_OPERAND (lhs, 1),
				       modifycode, rhs));

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
	/* Make sure the code to compute the rhs comes out
	   before the split.  */
	return build (COMPOUND_EXPR, TREE_TYPE (lhs),
		      /* But cast it to void to avoid an "unused" error.  */
		      convert (void_type_node, rhs), cond);
      }
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
	pedantic_lvalue_warning (CONVERT_EXPR);
	return convert (TREE_TYPE (lhs), result);
      }
    }

  /* Now we have handled acceptable kinds of LHS that are not truly lvalues.
     Reject anything strange now.  */

  if (!lvalue_or_else (lhs, "assignment"))
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

  newrhs = convert_for_assignment (lhstype, newrhs, "assignment",
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
  return convert_for_assignment (olhstype, result, "assignment",
				 NULL_TREE, NULL_TREE, 0);
}

/* Convert value RHS to type TYPE as preparation for an assignment
   to an lvalue of type TYPE.
   The real work of conversion is done by `convert'.
   The purpose of this function is to generate error messages
   for assignments that are not allowed in C.
   ERRTYPE is a string to use in error messages:
   "assignment", "return", etc.  If it is null, this is parameter passing
   for a function call (and different error messages are output).  Otherwise,
   it may be a name stored in the spelling stack and interpreted by
   get_spelling.

   FUNNAME is the name of the function being called,
   as an IDENTIFIER_NODE, or null.
   PARMNUM is the number of the argument, for printing in error messages.  */

static tree
convert_for_assignment (type, rhs, errtype, fundecl, funname, parmnum)
     tree type, rhs;
     char *errtype;
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
       &&
      (coder == INTEGER_TYPE || coder == REAL_TYPE || coder == ENUMERAL_TYPE
       || codel == COMPLEX_TYPE))
    /* Don't use convert_and_check here.  If the input has type int
       and did not overflow, and we are converting it here to a short,
       we don't want an error.  A warning would be okay, but it's too risky now
       to add an option to convert_and_check to get just warnings.  */
    return convert (type, rhs);
  /* Conversion to a union from its member types.  */
  else if (codel == UNION_TYPE)
    {
      tree memb_types;
      for (memb_types = TYPE_FIELDS (type); memb_types;
	   memb_types = TREE_CHAIN (memb_types))
	{
	  if (comptypes (TREE_TYPE (memb_types), TREE_TYPE (rhs)))
	    {
	      if (pedantic
		  && !(fundecl != 0 && DECL_IN_SYSTEM_HEADER (fundecl)))
		pedwarn ("ANSI C prohibits argument conversion to union type");
	      return build1 (NOP_EXPR, type, rhs);
	    }
	  else if (coder == POINTER_TYPE
		   && TREE_CODE (TREE_TYPE (memb_types)) == POINTER_TYPE)
	    {
	      tree memb_type = TREE_TYPE (memb_types);
	      register tree ttl = TREE_TYPE (memb_type);
	      register tree ttr = TREE_TYPE (rhstype);

	      /* Any non-function converts to a [const][volatile] void *
		 and vice versa; otherwise, targets must be the same.
		 Meanwhile, the lhs target must have all the qualifiers of the rhs.  */
	      if (TYPE_MAIN_VARIANT (ttl) == void_type_node
		  || TYPE_MAIN_VARIANT (ttr) == void_type_node
		  || comp_target_types (memb_type, rhstype))
		{
		  /* Const and volatile mean something different for function types,
		     so the usual warnings are not appropriate.  */
		  if (TREE_CODE (ttr) != FUNCTION_TYPE
		      || TREE_CODE (ttl) != FUNCTION_TYPE)
		    {
		      if (! TYPE_READONLY (ttl) && TYPE_READONLY (ttr))
			warn_for_assignment ("%s discards `const' from pointer target type",
					     get_spelling (errtype), funname, parmnum);
		      if (! TYPE_VOLATILE (ttl) && TYPE_VOLATILE (ttr))
			warn_for_assignment ("%s discards `volatile' from pointer target type",
					     get_spelling (errtype), funname, parmnum);
		    }
		  else
		    {
		      /* Because const and volatile on functions are restrictions
			 that say the function will not do certain things,
			 it is okay to use a const or volatile function
			 where an ordinary one is wanted, but not vice-versa.  */
		      if (TYPE_READONLY (ttl) && ! TYPE_READONLY (ttr))
			warn_for_assignment ("%s makes `const *' function pointer from non-const",
					     get_spelling (errtype), funname, parmnum);
		      if (TYPE_VOLATILE (ttl) && ! TYPE_VOLATILE (ttr))
			warn_for_assignment ("%s makes `volatile *' function pointer from non-volatile",
					     get_spelling (errtype), funname, parmnum);
		    }
		  if (pedantic
		      && !(fundecl != 0 && DECL_IN_SYSTEM_HEADER (fundecl)))
		    pedwarn ("ANSI C prohibits argument conversion to union type");
		  return build1 (NOP_EXPR, type, rhs);
		}
	    }
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
	  || comp_target_types (type, rhstype))
	{
	  if (pedantic
	      && ((TYPE_MAIN_VARIANT (ttl) == void_type_node
		   && TREE_CODE (ttr) == FUNCTION_TYPE)
		  ||
		  (TYPE_MAIN_VARIANT (ttr) == void_type_node
		   && !integer_zerop (rhs)
		   && TREE_CODE (ttl) == FUNCTION_TYPE)))
	    warn_for_assignment ("ANSI forbids %s between function pointer and `void *'",
				 get_spelling (errtype), funname, parmnum);
	  /* Const and volatile mean something different for function types,
	     so the usual warnings are not appropriate.  */
	  else if (TREE_CODE (ttr) != FUNCTION_TYPE
		   || TREE_CODE (ttl) != FUNCTION_TYPE)
	    {
	      if (! TYPE_READONLY (ttl) && TYPE_READONLY (ttr))
		warn_for_assignment ("%s discards `const' from pointer target type",
				     get_spelling (errtype), funname, parmnum);
	      if (! TYPE_VOLATILE (ttl) && TYPE_VOLATILE (ttr))
		warn_for_assignment ("%s discards `volatile' from pointer target type",
				     get_spelling (errtype), funname, parmnum);
	    }
	  else
	    {
	      /* Because const and volatile on functions are restrictions
		 that say the function will not do certain things,
		 it is okay to use a const or volatile function
		 where an ordinary one is wanted, but not vice-versa.  */
	      if (TYPE_READONLY (ttl) && ! TYPE_READONLY (ttr))
		warn_for_assignment ("%s makes `const *' function pointer from non-const",
				     get_spelling (errtype), funname, parmnum);
	      if (TYPE_VOLATILE (ttl) && ! TYPE_VOLATILE (ttr))
		warn_for_assignment ("%s makes `volatile *' function pointer from non-volatile",
				     get_spelling (errtype), funname, parmnum);
	    }
	}
      else if (unsigned_type (TYPE_MAIN_VARIANT (ttl))
	       == unsigned_type (TYPE_MAIN_VARIANT (ttr)))
	{
	  if (pedantic)
	    warn_for_assignment ("pointer targets in %s differ in signedness",
				 get_spelling (errtype), funname, parmnum);
	}
      else
	warn_for_assignment ("%s from incompatible pointer type",
			     get_spelling (errtype), funname, parmnum);
      return convert (type, rhs);
    }
  else if (codel == POINTER_TYPE && coder == INTEGER_TYPE)
    {
      /* An explicit constant 0 can convert to a pointer,
	 but not a 0 that results from casting or folding.  */
      if (! (TREE_CODE (rhs) == INTEGER_CST && integer_zerop (rhs)))
	{
	  warn_for_assignment ("%s makes pointer from integer without a cast",
			       get_spelling (errtype), funname, parmnum);
	  return convert (type, rhs);
	}
      return null_pointer_node;
    }
  else if (codel == INTEGER_TYPE && coder == POINTER_TYPE)
    {
      warn_for_assignment ("%s makes integer from pointer without a cast",
			   get_spelling (errtype), funname, parmnum);
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
    error ("incompatible types in %s", get_spelling (errtype));

  return error_mark_node;
}

/* Print a warning using MSG.
   It gets OPNAME as its one parameter.
   If OPNAME is null, it is replaced by "passing arg ARGNUM of `FUNCTION'".
   FUNCTION and ARGNUM are handled specially if we are building an
   Objective-C selector.  */

static void
warn_for_assignment (msg, opname, function, argnum)
     char *msg;
     char *opname;
     tree function;
     int argnum;
{
  static char argstring[] = "passing arg %d of `%s'";
  static char argnofun[] =  "passing arg %d";

  if (opname == 0)
    {
      tree selector = maybe_building_objc_message_expr ();
      
      if (selector && argnum > 2)
	{
	  function = selector;
	  argnum -= 2;
	}
      if (function)
	{
	  /* Function name is known; supply it.  */
	  opname = (char *) alloca (IDENTIFIER_LENGTH (function)
				    + sizeof (argstring) + 25 /*%d*/ + 1);
	  sprintf (opname, argstring, argnum, IDENTIFIER_POINTER (function));
	}
      else
	{
	  /* Function name unknown (call through ptr); just give arg number.  */
	  opname = (char *) alloca (sizeof (argnofun) + 25 /*%d*/ + 1);
	  sprintf (opname, argnofun, argnum);
	}
    }
  pedwarn (msg, opname);
}

/* Return nonzero if VALUE is a valid constant-valued expression
   for use in initializing a static variable; one that can be an
   element of a "constant" initializer.

   Return null_pointer_node if the value is absolute;
   if it is relocatable, return the variable that determines the relocation.
   We assume that VALUE has been folded as much as possible;
   therefore, we do not need to check for such things as
   arithmetic-combinations of integers.  */

static tree
initializer_constant_valid_p (value, endtype)
     tree value;
     tree endtype;
{
  switch (TREE_CODE (value))
    {
    case CONSTRUCTOR:
      return TREE_STATIC (value) ? null_pointer_node : 0;

    case INTEGER_CST:
    case REAL_CST:
    case STRING_CST:
    case COMPLEX_CST:
      return null_pointer_node;

    case ADDR_EXPR:
      return TREE_OPERAND (value, 0);

    case NON_LVALUE_EXPR:
      return initializer_constant_valid_p (TREE_OPERAND (value, 0), endtype);

    case CONVERT_EXPR:
    case NOP_EXPR:
      /* Allow conversions between pointer types.  */
      if (TREE_CODE (TREE_TYPE (value)) == POINTER_TYPE
	  && TREE_CODE (TREE_TYPE (TREE_OPERAND (value, 0))) == POINTER_TYPE)
	return initializer_constant_valid_p (TREE_OPERAND (value, 0), endtype);
      /* Allow conversions between real types.  */
      if (TREE_CODE (TREE_TYPE (value)) == REAL_TYPE
	  && TREE_CODE (TREE_TYPE (TREE_OPERAND (value, 0))) == REAL_TYPE)
	return initializer_constant_valid_p (TREE_OPERAND (value, 0), endtype);
      /* Allow length-preserving conversions between integer types.  */
      if (TREE_CODE (TREE_TYPE (value)) == INTEGER_TYPE
	  && TREE_CODE (TREE_TYPE (TREE_OPERAND (value, 0))) == INTEGER_TYPE
	  && tree_int_cst_equal (TYPE_SIZE (TREE_TYPE (value)),
				 TYPE_SIZE (TREE_TYPE (TREE_OPERAND (value, 0)))))
	return initializer_constant_valid_p (TREE_OPERAND (value, 0), endtype);
      /* Allow conversions between integer types only if explicit value.  */
      if (TREE_CODE (TREE_TYPE (value)) == INTEGER_TYPE
	  && TREE_CODE (TREE_TYPE (TREE_OPERAND (value, 0))) == INTEGER_TYPE)
	{
	  tree inner = initializer_constant_valid_p (TREE_OPERAND (value, 0),
						     endtype);
	  if (inner == null_pointer_node)
	    return null_pointer_node;
	  return 0;
	}
      /* Allow (int) &foo provided int is as wide as a pointer.  */
      if (TREE_CODE (TREE_TYPE (value)) == INTEGER_TYPE
	  && TREE_CODE (TREE_TYPE (TREE_OPERAND (value, 0))) == POINTER_TYPE
	  && ! tree_int_cst_lt (TYPE_SIZE (TREE_TYPE (value)),
				TYPE_SIZE (TREE_TYPE (TREE_OPERAND (value, 0)))))
	return initializer_constant_valid_p (TREE_OPERAND (value, 0),
					     endtype);
      /* Allow conversions to union types if the value inside is okay.  */
      if (TREE_CODE (TREE_TYPE (value)) == UNION_TYPE)
	return initializer_constant_valid_p (TREE_OPERAND (value, 0),
					     endtype);
      return 0;

    case PLUS_EXPR:
      if (TREE_CODE (endtype) == INTEGER_TYPE
	  && TYPE_PRECISION (endtype) < POINTER_SIZE)
	return 0;
      {
	tree valid0 = initializer_constant_valid_p (TREE_OPERAND (value, 0),
						    endtype);
	tree valid1 = initializer_constant_valid_p (TREE_OPERAND (value, 1),
						    endtype);
	/* If either term is absolute, use the other terms relocation.  */
	if (valid0 == null_pointer_node)
	  return valid1;
	if (valid1 == null_pointer_node)
	  return valid0;
	return 0;
      }

    case MINUS_EXPR:
      if (TREE_CODE (endtype) == INTEGER_TYPE
	  && TYPE_PRECISION (endtype) < POINTER_SIZE)
	return 0;
      {
	tree valid0 = initializer_constant_valid_p (TREE_OPERAND (value, 0),
						    endtype);
	tree valid1 = initializer_constant_valid_p (TREE_OPERAND (value, 1),
						    endtype);
	/* Win if second argument is absolute.  */
	if (valid1 == null_pointer_node)
	  return valid0;
	/* Win if both arguments have the same relocation.
	   Then the value is absolute.  */
	if (valid0 == valid1)
	  return null_pointer_node;
	return 0;
      }
    }

  return 0;
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

  value = digest_init (type, init, NULL_PTR, TREE_STATIC (decl),
		       TREE_STATIC (decl) || pedantic, 
		       IDENTIFIER_POINTER (DECL_NAME (decl)));

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

  /* ANSI wants warnings about out-of-range constant initializers.  */
  constant_expression_warning (value);

  DECL_INITIAL (decl) = value;
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
      char *s;
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
     char *string;
{
  PUSH_SPELLING (SPELLING_STRING, string, u.s);
}

/* Push a member name on the stack.  Printed as '.' STRING.  */

static void
push_member_name (string)
     char *string;
{
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
  register char *s;
  register struct spelling *p;

  for (p = spelling_base; p < spelling; p++)
    if (p->kind == SPELLING_BOUNDS)
      {
	sprintf (d, "[%d]", p->u.i);
	d += strlen (d);
      }
    else
      {
	if (p->kind == SPELLING_MEMBER)
	  *d++ = '.';
	for (s = p->u.s; *d = *s++; d++)
	  ;
      }
  *d++ = '\0';
  return buffer;
}

/* Provide a means to pass component names derived from the spelling stack.  */

char initialization_message;

/* Interpret the spelling of the given ERRTYPE message.  */

static char *
get_spelling (errtype)
     char *errtype;
{
  static char *buffer;
  static int size = -1;

  if (errtype == &initialization_message)
    {
      /* Avoid counting chars */
      static char message[] = "initialization of `%s'";
      register int needed = sizeof (message) + spelling_length () + 1;
      char *temp;

      if (size < 0)
	buffer = (char *) xmalloc (size = needed);
      if (needed > size)
	buffer = (char *) xrealloc (buffer, size = needed);

      temp = (char *) alloca (needed);
      sprintf (buffer, message, print_spelling (temp));
      return buffer;
    }

  return errtype;
}

/* Issue an error message for a bad initializer component.
   FORMAT describes the message.  OFWHAT is the name for the component.
   LOCAL is a format string for formatting the insertion of the name
   into the message.

   If OFWHAT is null, the component name is stored on the spelling stack.
   If the component name is a null string, then LOCAL is omitted entirely.  */

void
error_init (format, local, ofwhat)
     char *format, *local, *ofwhat;
{
  char *buffer;

  if (ofwhat == 0)
    ofwhat = print_spelling ((char *) alloca (spelling_length () + 1));
  buffer = (char *) alloca (strlen (local) + strlen (ofwhat) + 2);

  if (*ofwhat)
    sprintf (buffer, local, ofwhat);
  else
    buffer[0] = 0;

  error (format, buffer);
}

/* Issue a pedantic warning for a bad initializer component.
   FORMAT describes the message.  OFWHAT is the name for the component.
   LOCAL is a format string for formatting the insertion of the name
   into the message.

   If OFWHAT is null, the component name is stored on the spelling stack.
   If the component name is a null string, then LOCAL is omitted entirely.  */

void
pedwarn_init (format, local, ofwhat)
     char *format, *local, *ofwhat;
{
  char *buffer;

  if (ofwhat == 0)
    ofwhat = print_spelling ((char *) alloca (spelling_length () + 1));
  buffer = (char *) alloca (strlen (local) + strlen (ofwhat) + 2);

  if (*ofwhat)
    sprintf (buffer, local, ofwhat);
  else
    buffer[0] = 0;

  pedwarn (format, buffer);
}

/* Keep a pointer to the last free TREE_LIST node as we digest an initializer,
   so that we can reuse it.  This is set in digest_init, and used in
   process_init_constructor.

   We will never keep more than one free TREE_LIST node here.  This is for
   two main reasons.  First, we take elements off the old list and add them
   to the new list one at a time, thus there should never be more than
   one free TREE_LIST at a time, and thus even if there is, we will never
   need more than one.  Secondly, to avoid dangling pointers to freed obstacks,
   we want to always ensure that we have either a pointer to a valid TREE_LIST
   within the current initializer, or else a pointer to null.  */

static tree free_tree_list = NULL_TREE;

/* Digest the parser output INIT as an initializer for type TYPE.
   Return a C expression of type TYPE to represent the initial value.

   If TAIL is nonzero, it points to a variable holding a list of elements
   of which INIT is the first.  We update the list stored there by
   removing from the head all the elements that we use.
   Normally this is only one; we use more than one element only if
   TYPE is an aggregate and INIT is not a constructor.

   The arguments REQUIRE_CONSTANT and CONSTRUCTOR_CONSTANT request errors
   if non-constant initializers or elements are seen.  CONSTRUCTOR_CONSTANT
   applies only to elements of constructors.

   If OFWHAT is nonnull, it specifies what we are initializing, for error
   messages.   Examples: variable name, variable.member, array[44].
   If OFWHAT is null, the component name is stored on the spelling stack.
   (That is true for all nested calls to digest_init.)  */

tree
digest_init (type, init, tail, require_constant, constructor_constant, ofwhat)
     tree type, init, *tail;
     int require_constant, constructor_constant;
     char *ofwhat;
{
  enum tree_code code = TREE_CODE (type);
  tree element = 0;
  tree old_tail_contents;
  /* Nonzero if INIT is a braced grouping, which comes in as a CONSTRUCTOR
     tree node which has no TREE_TYPE.  */
  int raw_constructor
    = TREE_CODE (init) == CONSTRUCTOR && TREE_TYPE (init) == 0;
  tree inside_init = init;

  /* Make sure there is just one "partially bracketed" message
     per top-level initializer or constructor.  */
  if (ofwhat != 0)
    partial_bracket_mentioned = 0;

  /* By default, assume we use one element from a list.
     We correct this later in the cases where it is not true.

     Thus, we update TAIL now to point to the next element, and save the
     old value in OLD_TAIL_CONTENTS.  If we didn't actually use the first
     element, then we will reset TAIL before proceeding.  FREE_TREE_LIST
     is handled similarly.  */

  if (tail)
    {
      old_tail_contents = *tail;
      *tail = TREE_CHAIN (*tail);
      free_tree_list = old_tail_contents;
    }
  else
    free_tree_list = 0;

  if (init == error_mark_node)
    return init;

  /* Strip NON_LVALUE_EXPRs since we aren't using as an lvalue.  */
  /* Do not use STRIP_NOPS here.  We do not want an enumerator
     whose value is 0 to count as a null pointer constant.  */
  if (TREE_CODE (init) == NON_LVALUE_EXPR)
    inside_init = TREE_OPERAND (init, 0);

  if (inside_init && raw_constructor
      && CONSTRUCTOR_ELTS (inside_init) != 0
      && TREE_CHAIN (CONSTRUCTOR_ELTS (inside_init)) == 0)
    {
      element = TREE_VALUE (CONSTRUCTOR_ELTS (inside_init));
      /* Strip NON_LVALUE_EXPRs since we aren't using as an lvalue.  */
      if (element && TREE_CODE (element) == NON_LVALUE_EXPR)
	element = TREE_OPERAND (element, 0);
    }

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
	  && ((inside_init && TREE_CODE (inside_init) == STRING_CST)
	      || (element && TREE_CODE (element) == STRING_CST)))
	{
	  tree string = element ? element : inside_init;

	  if ((TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (string)))
	       != char_type_node)
	      && TYPE_PRECISION (typ1) == TYPE_PRECISION (char_type_node))
	    {
	      error_init ("char-array%s initialized from wide string",
			  " `%s'", ofwhat);
	      return error_mark_node;
	    }
	  if ((TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (string)))
	       == char_type_node)
	      && TYPE_PRECISION (typ1) != TYPE_PRECISION (char_type_node))
	    {
	      error_init ("int-array%s initialized from non-wide string",
			  " `%s'", ofwhat);
	      return error_mark_node;
	    }

	  TREE_TYPE (string) = type;
	  if (TYPE_DOMAIN (type) != 0
	      && TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST)
	    {
	      register int size = TREE_INT_CST_LOW (TYPE_SIZE (type));
	      size = (size + BITS_PER_UNIT - 1) / BITS_PER_UNIT;
	      /* Subtract 1 (or sizeof (wchar_t))
		 because it's ok to ignore the terminating null char
		 that is counted in the length of the constant.  */
	      if (size < TREE_STRING_LENGTH (string)
		  - (TYPE_PRECISION (typ1) != TYPE_PRECISION (char_type_node)
		     ? TYPE_PRECISION (wchar_type_node) / BITS_PER_UNIT
		     : 1))
		pedwarn_init (
		  "initializer-string for array of chars%s is too long",
		  " `%s'", ofwhat);
	    }
	  return string;
	}
    }

  /* Any type except an array can be initialized
     from an expression of the same type, optionally with braces.
     For an array, this is allowed only for a string constant.  */

  if (inside_init && TREE_TYPE (inside_init) != 0
      && ((TYPE_MAIN_VARIANT (TREE_TYPE (inside_init))
	   == TYPE_MAIN_VARIANT (type))
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
      else if (code == ARRAY_TYPE && TREE_CODE (inside_init) != STRING_CST)
	{
	  error_init ("array%s initialized from non-constant array expression",
		      " `%s'", ofwhat);
	  return error_mark_node;
	}

      if (optimize && TREE_READONLY (inside_init)
	  && TREE_CODE (inside_init) == VAR_DECL)
	inside_init = decl_constant_value (inside_init);

      if (require_constant && ! TREE_CONSTANT (inside_init))
	{
	  error_init ("initializer element%s is not constant",
		      " for `%s'", ofwhat);
	  inside_init = error_mark_node;
	}
      else if (require_constant
	       && initializer_constant_valid_p (inside_init, TREE_TYPE (inside_init)) == 0)
	{
	  error_init ("initializer element%s is not computable at load time",
		      " for `%s'", ofwhat);
	  inside_init = error_mark_node;
	}

      return inside_init;
    }

  if (element && (TREE_TYPE (element) == type
		  || (code == ARRAY_TYPE && TREE_TYPE (element)
		      && comptypes (TREE_TYPE (element), type))))
    {
      if (code == ARRAY_TYPE)
	{
	  error_init ("array%s initialized from non-constant array expression",
		      " `%s'", ofwhat);
	  return error_mark_node;
	}
      if (pedantic && (code == RECORD_TYPE || code == UNION_TYPE))
	pedwarn ("single-expression nonscalar initializer has braces");
      if (optimize && TREE_READONLY (element) && TREE_CODE (element) == VAR_DECL)
	element = decl_constant_value (element);

      if (require_constant && ! TREE_CONSTANT (element))
	{
	  error_init ("initializer element%s is not constant",
		      " for `%s'", ofwhat);
	  element = error_mark_node;
	}
      else if (require_constant
	       && initializer_constant_valid_p (element, TREE_TYPE (element)) == 0)
	{
	  error_init ("initializer element%s is not computable at load time",
		      " for `%s'", ofwhat);
	  element = error_mark_node;
	}

      return element;
    }

  /* Check for initializing a union by its first field.
     Such an initializer must use braces.  */

  if (code == UNION_TYPE)
    {
      tree result;
      tree field = TYPE_FIELDS (type);

      /* Find the first named field.  ANSI decided in September 1990
	 that only named fields count here.  */
      while (field && DECL_NAME (field) == 0)
	field = TREE_CHAIN (field);

      if (field == 0)
	{
	  error_init ("union%s with no named members cannot be initialized",
		      " `%s'", ofwhat);
	  return error_mark_node;
	}

      if (raw_constructor)
	result = process_init_constructor (type, inside_init, NULL_PTR,
					   require_constant,
					   constructor_constant, ofwhat);
      else if (tail != 0)
	{
	  *tail = old_tail_contents;
	  free_tree_list = NULL_TREE;
	  result = process_init_constructor (type, NULL_TREE, tail,
					     require_constant,
					     constructor_constant, ofwhat);
	}
      else
	result = 0;

      if (result)
	return result;
    }

  /* Handle scalar types, including conversions.  */

  if (code == INTEGER_TYPE || code == REAL_TYPE || code == POINTER_TYPE
      || code == ENUMERAL_TYPE || code == COMPLEX_TYPE)
    {
      if (raw_constructor)
	{
	  if (element == 0)
	    {
	      error_init (
	 	  "initializer for scalar%s requires one element",
		  " `%s'", ofwhat);
	      return error_mark_node;
	    }
	  else
	    {
	      /* Deal with extra levels of {...}.  */
	      if (TREE_CODE (element) == CONSTRUCTOR
		  && TREE_TYPE (element) == 0)
		{
		  error_init (
			      "initializer for scalar%s requires one element",
			      " `%s'", ofwhat);
		  return error_mark_node;
		}
	      inside_init = element;
	    }
	}

#if 0  /* A non-raw constructor is an actual expression.  */
      if (TREE_CODE (inside_init) == CONSTRUCTOR)
	{
	  error_init ("initializer for scalar%s has extra braces",
		      " `%s'", ofwhat);
	  return error_mark_node;
	}
#endif

      SAVE_SPELLING_DEPTH
	({
	  if (ofwhat)
	    push_string (ofwhat);
	  if (!raw_constructor)
	    inside_init = init;
	  /* Note that convert_for_assignment calls default_conversion
	     for arrays and functions.  We must not call it in the
	     case where inside_init is a null pointer constant.  */
	  inside_init
	    = convert_for_assignment (type, inside_init, 
				      &initialization_message,
				      NULL_TREE, NULL_TREE, 0);
	});

      if (require_constant && ! TREE_CONSTANT (inside_init))
	{
	  error_init ("initializer element%s is not constant",
		      " for `%s'", ofwhat);
	  inside_init = error_mark_node;
	}
      else if (require_constant
	       && initializer_constant_valid_p (inside_init, TREE_TYPE (inside_init)) == 0)
	{
	  error_init ("initializer element%s is not computable at load time",
		      " for `%s'", ofwhat);
	  inside_init = error_mark_node;
	}

      return inside_init;
    }

  /* Come here only for records and arrays.  */

  if (TYPE_SIZE (type) && TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
    {
      error_init ("variable-sized object%s may not be initialized",
		  " `%s'", ofwhat);
      return error_mark_node;
    }

  if (code == ARRAY_TYPE || code == RECORD_TYPE)
    {
      if (raw_constructor)
	return process_init_constructor (type, inside_init,
					 NULL_PTR, constructor_constant,
					 constructor_constant, ofwhat);
      else if (tail != 0)
	{
	  *tail = old_tail_contents;
	  free_tree_list = NULL_TREE;
	  return process_init_constructor (type, NULL_TREE, tail,
					   constructor_constant,
					   constructor_constant, ofwhat);
	}
      else if (flag_traditional)
	/* Traditionally one can say `char x[100] = 0;'.  */
	return process_init_constructor (type,
					 build_nt (CONSTRUCTOR, NULL_TREE,
						   tree_cons (NULL_TREE,
							      inside_init,
							      NULL_TREE)),
					 NULL_PTR, constructor_constant,
					 constructor_constant, ofwhat);
    }

  error_init ("invalid initializer%s", " for `%s'", ofwhat);
  return error_mark_node;
}

/* Process a constructor for a variable of type TYPE.
   The constructor elements may be specified either with INIT or with ELTS,
   only one of which should be non-null.

   If INIT is specified, it is a CONSTRUCTOR node which is specifically
   and solely for initializing this datum.

   If ELTS is specified, it is the address of a variable containing
   a list of expressions.  We take as many elements as we need
   from the head of the list and update the list.

   In the resulting constructor, TREE_CONSTANT is set if all elts are
   constant, and TREE_STATIC is set if, in addition, all elts are simple enough
   constants that the assembler and linker can compute them.

   The argument CONSTANT_VALUE says to print an error if either the
   value or any element is not a constant.

   The argument CONSTANT_ELEMENT says to print an error if an element
   of an aggregate is not constant.  It does not apply to a value
   which is not a constructor.  

   OFWHAT is a character string describing the object being initialized,
   for error messages.  It might be "variable" or "variable.member"
   or "variable[17].member[5]".

   If OFWHAT is null, the description string is stored on the spelling
   stack.  That is always true for recursive calls.  */

static tree
process_init_constructor (type, init, elts, constant_value, constant_element,
			  ofwhat)
     tree type, init, *elts;
     int constant_value, constant_element;
     char *ofwhat;
{
  register tree tail;
  /* List of the elements of the result constructor,
     in reverse order.  */
  register tree members = NULL;
  tree result;
  int allconstant = 1;
  int allsimple = 1;
  int erroneous = 0;
  int depth = SPELLING_DEPTH ();

  if (ofwhat)
    push_string (ofwhat);

  /* Make TAIL be the list of elements to use for the initialization,
     no matter how the data was given to us.  */

  if (elts)
    {
      if (warn_missing_braces)
	{
	  if (! partial_bracket_mentioned)
	    warning ("aggregate has a partly bracketed initializer");
	  partial_bracket_mentioned = 1;
	}
      tail = *elts;
    }
  else
    tail = CONSTRUCTOR_ELTS (init);

  /* Gobble as many elements as needed, and make a constructor or initial value
     for each element of this aggregate.  Chain them together in result.
     If there are too few, use 0 for each scalar ultimate component.  */

  if (TREE_CODE (type) == ARRAY_TYPE)
    {
      tree min_index, max_index;
      /* These are non-zero only within a range initializer.  */
      tree start_index = 0, end_index = 0;
      /* Within a range, this is the value for the elts in the range.  */
      tree range_val = 0;
      /* Do arithmetic using double integers, but don't use fold/build,
	 because these allocate a new tree object everytime they are called,
	 thus resulting in gcc using too much memory for large
	 initializers.  */
      union tree_node current_index_node, members_index_node;
      tree current_index = &current_index_node;
      tree members_index = &members_index_node;
      TREE_TYPE (current_index) = integer_type_node;
      TREE_TYPE (members_index) = integer_type_node;

      /* If we have array bounds, set our bounds from that.  Otherwise,
	 we have a lower bound of zero and an unknown upper bound.  */
      if (TYPE_DOMAIN (type))
	{
	  min_index = TYPE_MIN_VALUE (TYPE_DOMAIN (type));
	  max_index = TYPE_MAX_VALUE (TYPE_DOMAIN (type));
	}
      else
	{
	  min_index = integer_zero_node;
	  max_index = 0;
	}

      TREE_INT_CST_LOW (members_index) = TREE_INT_CST_LOW (min_index);
      TREE_INT_CST_HIGH (members_index) = TREE_INT_CST_HIGH (min_index);

      /* Don't leave the loop based on index if the next item has an explicit
	 index value that will override it. */

      for (TREE_INT_CST_LOW (current_index) = TREE_INT_CST_LOW (min_index),
	   TREE_INT_CST_HIGH (current_index) = TREE_INT_CST_HIGH (min_index);
	   tail != 0 || end_index;
	   add_double (TREE_INT_CST_LOW (current_index),
		       TREE_INT_CST_HIGH (current_index), 1, 0,
		       &TREE_INT_CST_LOW (current_index),
		       &TREE_INT_CST_HIGH (current_index)))
	{
	  register tree next1 = 0;

	  /* Handle the case where we are inside of a range.
	     current_index increments through the range,
	     so just keep reusing the same element of TAIL
	     until the end of the range.  */
	  if (end_index != 0)
	    {
	      next1 = range_val;
	      if (!tree_int_cst_lt (current_index, end_index))
		end_index = 0;
	    }

	  /* If this element specifies an index,
	     move to that index before storing it in the new list.  */
	  else if (TREE_PURPOSE (tail) != 0)
	    {
	      int win = 0;
	      tree index = TREE_PURPOSE (tail);

	      if (index && (TREE_CODE (index) == NON_LVALUE_EXPR
			    || TREE_CODE (index) == NOP_EXPR))
		index = TREE_OPERAND (index, 0);

	      /* Begin a range.  */
	      if (TREE_CODE (index) == TREE_LIST)
		{
		  start_index = TREE_PURPOSE (index);
		  end_index = TREE_PURPOSE (TREE_CHAIN (index));

		  /* Expose constants.  It Doesn't matter if we change
		     the mode.*/
		  if (end_index
		      && (TREE_CODE (end_index) == NON_LVALUE_EXPR
			  || TREE_CODE (end_index) == NOP_EXPR))
		    end_index = TREE_OPERAND (end_index, 0);
		  if (start_index
		      && (TREE_CODE (start_index) == NON_LVALUE_EXPR
			  || TREE_CODE (start_index) == NOP_EXPR))
		    start_index = TREE_OPERAND (start_index, 0);

		  constant_expression_warning (start_index);
		  constant_expression_warning (end_index);

		  if ((TREE_CODE (start_index) == IDENTIFIER_NODE) 
		      || (TREE_CODE (end_index) == IDENTIFIER_NODE))
		    error ("field name used as index in array initializer");
		  else if ((TREE_CODE (start_index) != INTEGER_CST)
			   || (TREE_CODE (end_index) != INTEGER_CST))
		    error ("non-constant or non-integer array index in initializer");
		  else if (tree_int_cst_lt (start_index, min_index)
			   || (max_index && tree_int_cst_lt (max_index, start_index))
			   || tree_int_cst_lt (end_index, min_index)
			   || (max_index && tree_int_cst_lt (max_index, end_index)))
		    error ("array index out of range in initializer");
		  else if (tree_int_cst_lt (end_index, start_index))
		    {
		      /* If the range is empty, don't initialize any elements,
			 but do reset current_index for the next initializer
			 element.  */
		      warning ("empty array initializer range");
		      tail = TREE_CHAIN (tail);
		      TREE_INT_CST_LOW (current_index)
			= TREE_INT_CST_LOW (end_index);
		      TREE_INT_CST_HIGH (current_index)
			= TREE_INT_CST_HIGH (end_index);
		      continue;
		    }
		  else
		    {
		      TREE_INT_CST_LOW (current_index)
			= TREE_INT_CST_LOW (start_index);
		      TREE_INT_CST_HIGH (current_index)
			= TREE_INT_CST_HIGH (start_index);
		      win = 1;
		      /* See if the first element is also the last.  */
		      if (!tree_int_cst_lt (current_index, end_index))
			end_index = 0;
		    }
		}
	      else if (TREE_CODE (index) == IDENTIFIER_NODE)
		error ("field name used as index in array initializer");
	      else if (TREE_CODE (index) != INTEGER_CST)
		error ("non-constant array index in initializer");
	      else if (tree_int_cst_lt (index, min_index)
		       || (max_index && tree_int_cst_lt (max_index, index)))
		error ("array index out of range in initializer");
	      else
		{
		  constant_expression_warning (index);
		  TREE_INT_CST_LOW (current_index) = TREE_INT_CST_LOW (index);
		  TREE_INT_CST_HIGH (current_index)
		    = TREE_INT_CST_HIGH (index);
		  win = 1;
		}

	      if (!win)
		{
		  /* If there was an error, end the current range.  */
		  end_index = 0;
		  TREE_VALUE (tail) = error_mark_node;
		}
	    }

	  if (max_index && tree_int_cst_lt (max_index, current_index))
	    break;  /* Stop if we've indeed run out of elements. */

	  /* Now digest the value specified.  */
	  if (next1 != 0)
	    ;
	  else if (TREE_VALUE (tail) != 0)
	    {
	      tree tail1 = tail;

	      /* Build the element of this array, with "[]" notation.  For
		 error messages, we assume that the index fits within a
		 host int.  */
	      SAVE_SPELLING_DEPTH
		({
		  push_array_bounds (TREE_INT_CST_LOW (current_index));
		  next1 = digest_init (TYPE_MAIN_VARIANT (TREE_TYPE (type)),
				       TREE_VALUE (tail), &tail1,
				       /* Both of these are the same because
					  a value here is an elt overall.  */
				       constant_element, constant_element,
				       NULL_PTR);
		});

	      if (tail1 != 0 && TREE_CODE (tail1) != TREE_LIST)
		abort ();
	      if (tail == tail1 && TYPE_DOMAIN (type) == 0)
		{
		  error_init (
		    "non-empty initializer for array%s of empty elements",
		    " `%s'", NULL_PTR);
		  /* Just ignore what we were supposed to use.  */
		  tail1 = 0;
		}
	      tail = tail1;
	    }
	  else
	    {
	      next1 = error_mark_node;
	      tail = TREE_CHAIN (tail);
	    }

	  if (end_index != 0)
	    range_val = next1;

	  if (next1 == error_mark_node)
	    erroneous = 1;
	  else if (!TREE_CONSTANT (next1))
	    allconstant = 0;
	  else if (initializer_constant_valid_p (next1, TREE_TYPE (next1)) == 0)
	    allsimple = 0;

	  /* Now store NEXT1 in the list, I elements from the *end*.
	     Make the list longer if necessary.  */
	  while (! tree_int_cst_lt (current_index, members_index))
	    {
	      if (free_tree_list)
		{
		  TREE_CHAIN (free_tree_list) = members;
		  TREE_PURPOSE (free_tree_list) = NULL_TREE;
		  TREE_VALUE (free_tree_list) = NULL_TREE;
		  members = free_tree_list;
		  free_tree_list = NULL_TREE;
		}
	      else
		members = tree_cons (NULL_TREE, NULL_TREE, members);
	      add_double (TREE_INT_CST_LOW (members_index),
			  TREE_INT_CST_HIGH (members_index), 1, 0,
			  &TREE_INT_CST_LOW (members_index),
			  &TREE_INT_CST_HIGH (members_index));
	    }

	  {
	    tree temp;
	    union tree_node idx_node;
	    tree idx = &idx_node;
	    TREE_TYPE (idx) = integer_type_node;

	    temp = members;
	    for (add_double (TREE_INT_CST_LOW (members_index),
			     TREE_INT_CST_HIGH (members_index), -1, -1,
			     &TREE_INT_CST_LOW (idx),
			     &TREE_INT_CST_HIGH (idx));
		 tree_int_cst_lt (current_index, idx);
		 add_double (TREE_INT_CST_LOW (idx),
			     TREE_INT_CST_HIGH (idx), -1, -1,
			     &TREE_INT_CST_LOW (idx),
			     &TREE_INT_CST_HIGH (idx)))
	      temp = TREE_CHAIN (temp);
	    TREE_VALUE (temp) = next1;
	  }
	}
    }
  if (TREE_CODE (type) == RECORD_TYPE)
    {
      register tree field;
      int members_length = 0;
      int i;

      /* Don't leave the loop based on field just yet; see if next item
	 overrides the expected field first. */

      for (field = TYPE_FIELDS (type), i = 0; tail;
	   field = TREE_CHAIN (field), i++)
	{
	  register tree next1;

	  /* If this element specifies a field, 
	     move to that field before storing it in the new list.  */
	  if (TREE_PURPOSE (tail) != 0)
	    {
	      int win = 0;

	      if (TREE_CODE (TREE_PURPOSE (tail)) != IDENTIFIER_NODE)
		error ("index value instead of field name in structure initializer");
	      else
		{
		  tree temp;
		  int j;
		  for (temp = TYPE_FIELDS (type), j = 0;
		       temp;
		       temp = TREE_CHAIN (temp), j++)
		    if (DECL_NAME (temp) == TREE_PURPOSE (tail))
		      break;
		  if (temp)
		    field = temp, i = j, win = 1;
		  else
		    error ("no field `%s' in structure being initialized",
			   IDENTIFIER_POINTER (TREE_PURPOSE (tail))); 
		}
	      if (!win)
		TREE_VALUE (tail) = error_mark_node;
	    }

	  if (field == 0)
	    break;  /* No more fields to init. */

	  if (! DECL_NAME (field))
	    {
	      next1 = integer_zero_node;
	    }
	  else if (TREE_VALUE (tail) != 0)
	    {
	      tree tail1 = tail;

	      /* Build the name of this member, with a "." for membership.  */
	      SAVE_SPELLING_DEPTH
		({
		  push_member_name (IDENTIFIER_POINTER (DECL_NAME (field)));
		  next1 = digest_init (TREE_TYPE (field),
				       TREE_VALUE (tail), &tail1,
				       constant_element, constant_element,
				       NULL_PTR);
		});
	      if (tail1 != 0 && TREE_CODE (tail1) != TREE_LIST)
		abort ();
	      tail = tail1;
	    }
	  else
	    {
	      next1 = error_mark_node;
	      tail = TREE_CHAIN (tail);
	    }

	  if (next1 == error_mark_node)
	    erroneous = 1;
	  else if (!TREE_CONSTANT (next1))
	    allconstant = 0;
	  else if (initializer_constant_valid_p (next1, TREE_TYPE (next1)) == 0)
	    allsimple = 0;

	  /* Now store NEXT1 in the list, I elements from the *end*.
	     Make the list longer if necessary.  */
	  while (i >= members_length)
	    {
	      if (free_tree_list)
		{
		  TREE_CHAIN (free_tree_list) = members;
		  TREE_PURPOSE (free_tree_list) = NULL_TREE;
		  TREE_VALUE (free_tree_list) = NULL_TREE;
		  members = free_tree_list;
		  free_tree_list = NULL_TREE;
		}
	      else
		members = tree_cons (NULL_TREE, NULL_TREE, members);
	      members_length++;
	    }
	  {
	    tree temp;
	    int j;

	    temp = members;
	    for (j = members_length - 1; j > i; j--)
	      temp = TREE_CHAIN (temp);
	    TREE_VALUE (temp) = next1;
	    TREE_PURPOSE (temp) = field;
	  }
	}
    }
  if (TREE_CODE (type) == UNION_TYPE)
    {
      register tree field = TYPE_FIELDS (type);
      register tree next1;

      /* Find the first named field.  ANSI decided in September 1990
	 that only named fields count here.  */
      while (field && DECL_NAME (field) == 0)
	field = TREE_CHAIN (field);

      /* For a union, get the initializer for 1 fld.  */

      if (tail == 0)
	{
	  error ("empty initializer for union");
	  tail = build_tree_list (0, 0);
	}

      /* If this element specifies a field, initialize via that field.  */
      if (TREE_PURPOSE (tail) != 0)
	{
	  int win = 0;

	  if (TREE_CODE (TREE_PURPOSE (tail)) == FIELD_DECL)
	    /* Handle the case of a call by build_c_cast.  */
	    field = TREE_PURPOSE (tail), win = 1;
	  else if (TREE_CODE (TREE_PURPOSE (tail)) != IDENTIFIER_NODE)
	    error ("index value instead of field name in union initializer");
	  else
	    {
	      tree temp;
	      for (temp = TYPE_FIELDS (type);
		   temp;
		   temp = TREE_CHAIN (temp))
		if (DECL_NAME (temp) == TREE_PURPOSE (tail))
		  break;
	      if (temp)
		field = temp, win = 1;
	      else
		error ("no field `%s' in union being initialized",
		       IDENTIFIER_POINTER (TREE_PURPOSE (tail)));
	    }
	  if (!win)
	    TREE_VALUE (tail) = error_mark_node;
	}

      if (TREE_VALUE (tail) != 0)
	{
	  tree tail1 = tail;

	  /* Build the name of this member, with a "." for membership.  */
	  SAVE_SPELLING_DEPTH
	    ({
	      push_member_name (IDENTIFIER_POINTER (DECL_NAME (field)));
	      next1 = digest_init (TREE_TYPE (field),
				   TREE_VALUE (tail), &tail1,
				   constant_value, constant_element, NULL_PTR);
	    });
	  if (tail1 != 0 && TREE_CODE (tail1) != TREE_LIST)
	    abort ();
	  tail = tail1;
	}
      else
	{
	  next1 = error_mark_node;
	  tail = TREE_CHAIN (tail);
	}

      if (next1 == error_mark_node)
	erroneous = 1;
      else if (!TREE_CONSTANT (next1))
	allconstant = 0;
      else if (initializer_constant_valid_p (next1, TREE_TYPE (next1)) == 0)
	allsimple = 0; 
     if (free_tree_list)
	{
	  TREE_CHAIN (free_tree_list) = members;
	  TREE_PURPOSE (free_tree_list) = field;
	  TREE_VALUE (free_tree_list) = next1;
	  members = free_tree_list;
	  free_tree_list = NULL_TREE;
	}
      else
	members = tree_cons (field, next1, members);
    }

  /* If arguments were specified as a list, just remove the ones we used.  */
  if (elts)
    *elts = tail;
  /* If arguments were specified as a constructor,
     complain unless we used all the elements of the constructor.  */
  else if (tail)
    {
      if (TREE_CODE (type) == UNION_TYPE)
	{
	  pedwarn_init ("excess elements in union initializer%s",
			" after `%s'", NULL_PTR);
	}
      else
	{
	  pedwarn_init ("excess elements in aggregate initializer%s",
			" after `%s'", NULL_PTR);
	}
    }

  /* It might be possible to use SAVE_SPELLING_DEPTH, but I suspect that
     some preprocessor somewhere won't accept that much text as an argument.
     It's also likely to make debugging difficult.  */

  RESTORE_SPELLING_DEPTH (depth);

  if (erroneous)
    return error_mark_node;

  if (elts)
    result = build (CONSTRUCTOR, type, NULL_TREE, nreverse (members));
  else
    {
      result = init;
      CONSTRUCTOR_ELTS (result) = nreverse (members);
      TREE_TYPE (result) = type;
      TREE_CONSTANT (result) = 0;
      TREE_STATIC (result) = 0;
    }
  if (allconstant) TREE_CONSTANT (result) = 1;
  if (allconstant && allsimple) TREE_STATIC (result) = 1;
  return result;
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
    o[i] = TREE_VALUE (tail);

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
		       0, VOIDmode, 0);
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
    warning ("function declared `volatile' has a `return' statement");

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
      tree t = convert_for_assignment (valtype, retval, "return",
				       NULL_TREE, NULL_TREE, 0);
      tree res = DECL_RESULT (current_function_decl);
      t = build (MODIFY_EXPR, TREE_TYPE (res),
		 res, convert (TREE_TYPE (res), t));
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
  register enum tree_code code = TREE_CODE (TREE_TYPE (exp));
  tree type = TREE_TYPE (exp);

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
