/* Report error messages, build initializers, and perform
   some front-end optimizations for C++ compiler.
   Copyright (C) 1987, 1988, 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002 Free Software Foundation, Inc.
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
#include "system.h"
#include "tree.h"
#include "cp-tree.h"
#include "flags.h"
#include "toplev.h"
#include "output.h"
#include "diagnostic.h"

static tree process_init_constructor PARAMS ((tree, tree, tree *));

/* Print an error message stemming from an attempt to use
   BASETYPE as a base class for TYPE.  */

tree
error_not_base_type (basetype, type)
     tree basetype, type;
{
  if (TREE_CODE (basetype) == FUNCTION_DECL)
    basetype = DECL_CONTEXT (basetype);
  error ("type `%T' is not a base type for type `%T'", basetype, type);
  return error_mark_node;
}

tree
binfo_or_else (base, type)
     tree base, type;
{
  tree binfo = lookup_base (type, base, ba_ignore, NULL);

  if (binfo == error_mark_node)
    return NULL_TREE;
  else if (!binfo)
    error_not_base_type (base, type);
  return binfo;
}

/* According to ARM $7.1.6, "A `const' object may be initialized, but its
   value may not be changed thereafter.  Thus, we emit hard errors for these,
   rather than just pedwarns.  If `SOFT' is 1, then we just pedwarn.  (For
   example, conversions to references.)  */

void
readonly_error (arg, string, soft)
     tree arg;
     const char *string;
     int soft;
{
  const char *fmt;
  void (*fn) PARAMS ((const char *, ...));

  if (soft)
    fn = pedwarn;
  else
    fn = error;

  if (TREE_CODE (arg) == COMPONENT_REF)
    {
      if (TYPE_READONLY (TREE_TYPE (TREE_OPERAND (arg, 0))))
        fmt = "%s of data-member `%D' in read-only structure";
      else
        fmt = "%s of read-only data-member `%D'";
      (*fn) (fmt, string, TREE_OPERAND (arg, 1));
    }
  else if (TREE_CODE (arg) == VAR_DECL)
    {
      if (DECL_LANG_SPECIFIC (arg)
	  && DECL_IN_AGGR_P (arg)
	  && !TREE_STATIC (arg))
	fmt = "%s of constant field `%D'";
      else
	fmt = "%s of read-only variable `%D'";
      (*fn) (fmt, string, arg);
    }
  else if (TREE_CODE (arg) == PARM_DECL)
    (*fn) ("%s of read-only parameter `%D'", string, arg);
  else if (TREE_CODE (arg) == INDIRECT_REF
           && TREE_CODE (TREE_TYPE (TREE_OPERAND (arg, 0))) == REFERENCE_TYPE
           && (TREE_CODE (TREE_OPERAND (arg, 0)) == VAR_DECL
               || TREE_CODE (TREE_OPERAND (arg, 0)) == PARM_DECL))
    (*fn) ("%s of read-only reference `%D'", string, TREE_OPERAND (arg, 0));
  else if (TREE_CODE (arg) == RESULT_DECL)
    (*fn) ("%s of read-only named return value `%D'", string, arg);
  else if (TREE_CODE (arg) == FUNCTION_DECL)
    (*fn) ("%s of function `%D'", string, arg);
  else
    (*fn) ("%s of read-only location", string);
}

/* If TYPE has abstract virtual functions, issue an error about trying
   to create an object of that type.  DECL is the object declared, or
   NULL_TREE if the declaration is unavailable.  Returns 1 if an error
   occurred; zero if all was well.  */

int
abstract_virtuals_error (decl, type)
     tree decl;
     tree type;
{
  tree u;
  tree tu;

  if (processing_template_decl)
    /* If we are processing a template, TYPE may be a template
       class where CLASSTYPE_PURE_VIRTUALS always contains
       inline friends.  */
    return 0;

  if (!CLASS_TYPE_P (type) || !CLASSTYPE_PURE_VIRTUALS (type))
    return 0;

  if (!TYPE_SIZE (type))
    /* TYPE is being defined, and during that time
       CLASSTYPE_PURE_VIRTUALS holds the inline friends.  */
    return 0;

  u = CLASSTYPE_PURE_VIRTUALS (type);
  if (decl)
    {
      if (TREE_CODE (decl) == RESULT_DECL)
	return 0;

      if (TREE_CODE (decl) == VAR_DECL)
	error ("cannot declare variable `%D' to be of type `%T'",
		    decl, type);
      else if (TREE_CODE (decl) == PARM_DECL)
	error ("cannot declare parameter `%D' to be of type `%T'",
		    decl, type);
      else if (TREE_CODE (decl) == FIELD_DECL)
	error ("cannot declare field `%D' to be of type `%T'",
		    decl, type);
      else if (TREE_CODE (decl) == FUNCTION_DECL
	       && TREE_CODE (TREE_TYPE (decl)) == METHOD_TYPE)
	error ("invalid return type for member function `%#D'", decl);
      else if (TREE_CODE (decl) == FUNCTION_DECL)
	error ("invalid return type for function `%#D'", decl);
    }
  else
    error ("cannot allocate an object of type `%T'", type);

  /* Only go through this once.  */
  if (TREE_PURPOSE (u) == NULL_TREE)
    {
      TREE_PURPOSE (u) = error_mark_node;

      error ("  because the following virtual functions are abstract:");
      for (tu = u; tu; tu = TREE_CHAIN (tu))
	cp_error_at ("\t%#D", TREE_VALUE (tu));
    }
  else
    error ("  since type `%T' has abstract virtual functions", type);

  return 1;
}

/* Print an error message for invalid use of an incomplete type.
   VALUE is the expression that was used (or 0 if that isn't known)
   and TYPE is the type that was invalid.  DIAG_TYPE indicates the
   type of diagnostic:  0 for an error, 1 for a warning, 2 for a
   pedwarn.  */

void
cxx_incomplete_type_diagnostic (value, type, diag_type)
     tree value;
     tree type;
     int diag_type;
{
  int decl = 0;
  void (*p_msg) PARAMS ((const char *, ...));
  void (*p_msg_at) PARAMS ((const char *, ...));

  if (diag_type == 1)
    {
      p_msg = warning;
      p_msg_at = cp_warning_at;
    }
  else if (diag_type == 2)
    {
      p_msg = pedwarn;
      p_msg_at = cp_pedwarn_at;
    }
  else
    {
      p_msg = error;
      p_msg_at = cp_error_at;
    }
  
  /* Avoid duplicate error message.  */
  if (TREE_CODE (type) == ERROR_MARK)
    return;

  if (value != 0 && (TREE_CODE (value) == VAR_DECL
		     || TREE_CODE (value) == PARM_DECL
		     || TREE_CODE (value) == FIELD_DECL))
    {
      (*p_msg_at) ("`%D' has incomplete type", value);
      decl = 1;
    }
retry:
  /* We must print an error message.  Be clever about what it says.  */

  switch (TREE_CODE (type))
    {
    case RECORD_TYPE:
    case UNION_TYPE:
    case ENUMERAL_TYPE:
      if (!decl)
        (*p_msg) ("invalid use of undefined type `%#T'", type);
      if (!TYPE_TEMPLATE_INFO (type))
	(*p_msg_at) ("forward declaration of `%#T'", type);
      else
	(*p_msg_at) ("declaration of `%#T'", type);
      break;

    case VOID_TYPE:
      (*p_msg) ("invalid use of `%T'", type);
      break;

    case ARRAY_TYPE:
      if (TYPE_DOMAIN (type))
        {
          type = TREE_TYPE (type);
          goto retry;
        }
      (*p_msg) ("invalid use of array with unspecified bounds");
      break;

    case OFFSET_TYPE:
    bad_member:
      (*p_msg) ("invalid use of member (did you forget the `&' ?)");
      break;

    case TEMPLATE_TYPE_PARM:
      (*p_msg) ("invalid use of template type parameter");
      break;

    case UNKNOWN_TYPE:
      if (value && TREE_CODE (value) == COMPONENT_REF)
        goto bad_member;
      else if (value && TREE_CODE (value) == ADDR_EXPR)
        (*p_msg) ("address of overloaded function with no contextual type information");
      else if (value && TREE_CODE (value) == OVERLOAD)
        (*p_msg) ("overloaded function with no contextual type information");
      else
        (*p_msg) ("insufficient contextual information to determine type");
      break;
    
    default:
      abort ();
    }
}

/* Backward-compatibility interface to incomplete_type_diagnostic;
   required by ../tree.c.  */
#undef cxx_incomplete_type_error
void
cxx_incomplete_type_error (value, type)
     tree value;
     tree type;
{
  cxx_incomplete_type_diagnostic (value, type, 0);
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

   Returns value of initializer if initialization could not be
   performed for static variable.  In that case, caller must do
   the storing.  */

tree
store_init_value (decl, init)
     tree decl, init;
{
  register tree value, type;

  /* If variable's type was invalidly declared, just ignore it.  */

  type = TREE_TYPE (decl);
  if (TREE_CODE (type) == ERROR_MARK)
    return NULL_TREE;

  if (IS_AGGR_TYPE (type))
    {
      if (! TYPE_HAS_TRIVIAL_INIT_REF (type)
	  && TREE_CODE (init) != CONSTRUCTOR)
	abort ();

      if (TREE_CODE (init) == TREE_LIST)
	{
	  error ("constructor syntax used, but no constructor declared for type `%T'", type);
	  init = build_nt (CONSTRUCTOR, NULL_TREE, nreverse (init));
	}
    }
  else if (TREE_CODE (init) == TREE_LIST
	   && TREE_TYPE (init) != unknown_type_node)
    {
      if (TREE_CODE (decl) == RESULT_DECL)
	{
	  if (TREE_CHAIN (init))
	    {
	      warning ("comma expression used to initialize return value");
	      init = build_compound_expr (init);
	    }
	  else
	    init = TREE_VALUE (init);
	}
      else if (TREE_CODE (init) == TREE_LIST
	       && TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE)
	{
	  error ("cannot initialize arrays using this syntax");
	  return NULL_TREE;
	}
      else
	{
	  /* We get here with code like `int a (2);' */
	     
	  if (TREE_CHAIN (init) != NULL_TREE)
	    {
	      pedwarn ("initializer list being treated as compound expression");
	      init = build_compound_expr (init);
	    }
	  else
	    init = TREE_VALUE (init);
	}
    }

  /* End of special C++ code.  */

  /* Digest the specified initializer into an expression.  */
  value = digest_init (type, init, (tree *) 0);

  /* Store the expression if valid; else report error.  */

  if (TREE_CODE (value) == ERROR_MARK)
    ;
  /* Other code expects that initializers for objects of types that need
     constructing never make it into DECL_INITIAL, and passes 'init' to
     build_aggr_init without checking DECL_INITIAL.  So just return.  */
  else if (TYPE_NEEDS_CONSTRUCTING (type))
    return value;
  else if (TREE_STATIC (decl)
	   && (! TREE_CONSTANT (value)
	       || ! initializer_constant_valid_p (value, TREE_TYPE (value))))
    return value;
  
  /* Store the VALUE in DECL_INITIAL.  If we're building a
     statement-tree we will actually expand the initialization later
     when we output this function.  */
  DECL_INITIAL (decl) = value;
  return NULL_TREE;
}


/* Digest the parser output INIT as an initializer for type TYPE.
   Return a C expression of type TYPE to represent the initial value.

   If TAIL is nonzero, it points to a variable holding a list of elements
   of which INIT is the first.  We update the list stored there by
   removing from the head all the elements that we use.
   Normally this is only one; we use more than one element only if
   TYPE is an aggregate and INIT is not a constructor.  */

tree
digest_init (type, init, tail)
     tree type, init, *tail;
{
  enum tree_code code = TREE_CODE (type);
  tree element = NULL_TREE;
  tree old_tail_contents = NULL_TREE;
  /* Nonzero if INIT is a braced grouping.  */
  int raw_constructor;

  /* By default, assume we use one element from a list.
     We correct this later in the sole case where it is not true.  */

  if (tail)
    {
      old_tail_contents = *tail;
      *tail = TREE_CHAIN (*tail);
    }

  if (init == error_mark_node || (TREE_CODE (init) == TREE_LIST
				  && TREE_VALUE (init) == error_mark_node))
    return error_mark_node;

  if (TREE_CODE (init) == ERROR_MARK)
    /* __PRETTY_FUNCTION__'s initializer is a bogus expression inside
       a template function. This gets substituted during instantiation.  */
    return init;

  /* We must strip the outermost array type when completing the type,
     because the its bounds might be incomplete at the moment.  */
  if (!complete_type_or_else (TREE_CODE (type) == ARRAY_TYPE
			      ? TREE_TYPE (type) : type, NULL_TREE))
    return error_mark_node;
  
  /* Strip NON_LVALUE_EXPRs since we aren't using as an lvalue.  */
  if (TREE_CODE (init) == NON_LVALUE_EXPR)
    init = TREE_OPERAND (init, 0);

  raw_constructor = (TREE_CODE (init) == CONSTRUCTOR 
		     && TREE_HAS_CONSTRUCTOR (init));

  if (raw_constructor
      && CONSTRUCTOR_ELTS (init) != 0
      && TREE_CHAIN (CONSTRUCTOR_ELTS (init)) == 0)
    {
      element = TREE_VALUE (CONSTRUCTOR_ELTS (init));
      /* Strip NON_LVALUE_EXPRs since we aren't using as an lvalue.  */
      if (element && TREE_CODE (element) == NON_LVALUE_EXPR)
	element = TREE_OPERAND (element, 0);
      if (element == error_mark_node)
	return element;
    }

  /* Initialization of an array of chars from a string constant
     optionally enclosed in braces.  */

  if (code == ARRAY_TYPE)
    {
      tree typ1;

      if (TREE_CODE (init) == TREE_LIST)
	{
	  error ("initializing array with parameter list");
	  return error_mark_node;
	}

      typ1 = TYPE_MAIN_VARIANT (TREE_TYPE (type));
      if (char_type_p (typ1)
	  && ((init && TREE_CODE (init) == STRING_CST)
	      || (element && TREE_CODE (element) == STRING_CST)))
	{
	  tree string = element ? element : init;

	  if ((TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (string)))
	       != char_type_node)
	      && TYPE_PRECISION (typ1) == BITS_PER_UNIT)
	    {
	      error ("char-array initialized from wide string");
	      return error_mark_node;
	    }
	  if ((TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (string)))
	       == char_type_node)
	      && TYPE_PRECISION (typ1) != BITS_PER_UNIT)
	    {
	      error ("int-array initialized from non-wide string");
	      return error_mark_node;
	    }

	  TREE_TYPE (string) = type;
	  if (TYPE_DOMAIN (type) != 0
	      && TREE_CONSTANT (TYPE_SIZE (type)))
	    {
	      register int size
		= TREE_INT_CST_LOW (TYPE_SIZE (type));
	      size = (size + BITS_PER_UNIT - 1) / BITS_PER_UNIT;
	      /* In C it is ok to subtract 1 from the length of the string
		 because it's ok to ignore the terminating null char that is
		 counted in the length of the constant, but in C++ this would
		 be invalid.  */
	      if (size < TREE_STRING_LENGTH (string))
		pedwarn ("initializer-string for array of chars is too long");
	    }
	  return string;
	}
    }

  /* Handle scalar types, including conversions,
     and signature pointers and references.  */

  if (code == INTEGER_TYPE || code == REAL_TYPE || code == POINTER_TYPE
      || code == ENUMERAL_TYPE || code == REFERENCE_TYPE
      || code == BOOLEAN_TYPE || code == COMPLEX_TYPE
      || TYPE_PTRMEMFUNC_P (type))
    {
      if (raw_constructor)
	{
	  if (element == 0)
	    {
	      error ("initializer for scalar variable requires one element");
	      return error_mark_node;
	    }
	  init = element;
	}
      while (TREE_CODE (init) == CONSTRUCTOR && TREE_HAS_CONSTRUCTOR (init))
	{
	  pedwarn ("braces around scalar initializer for `%T'", type);
	  init = CONSTRUCTOR_ELTS (init);
	  if (TREE_CHAIN (init))
	    pedwarn ("ignoring extra initializers for `%T'", type);
	  init = TREE_VALUE (init);
	}

      return convert_for_initialization (0, type, init, LOOKUP_NORMAL,
					 "initialization", NULL_TREE, 0);
    }

  /* Come here only for records and arrays (and unions with constructors).  */

  if (COMPLETE_TYPE_P (type) && ! TREE_CONSTANT (TYPE_SIZE (type)))
    {
      error ("variable-sized object of type `%T' may not be initialized",
		type);
      return error_mark_node;
    }

  if (code == ARRAY_TYPE || code == VECTOR_TYPE || IS_AGGR_TYPE_CODE (code))
    {
      if (raw_constructor && TYPE_NON_AGGREGATE_CLASS (type)
	  && TREE_HAS_CONSTRUCTOR (init))
	{
	  error ("subobject of type `%T' must be initialized by constructor, not by `%E'",
		    type, init);
	  return error_mark_node;
	}
      else if (raw_constructor)
	return process_init_constructor (type, init, (tree *)0);
      else if (can_convert_arg (type, TREE_TYPE (init), init)
	       || TYPE_NON_AGGREGATE_CLASS (type))
	/* These are never initialized from multiple constructor elements.  */;
      else if (tail != 0)
	{
	  *tail = old_tail_contents;
	  return process_init_constructor (type, 0, tail);
	}

      if (code != ARRAY_TYPE)
	{
	  int flags = LOOKUP_NORMAL;
	  /* Initialization from { } is copy-initialization.  */
	  if (tail)
	    flags |= LOOKUP_ONLYCONVERTING;

	  return convert_for_initialization (NULL_TREE, type, init, flags,
					     "initialization", NULL_TREE, 0);
	}
    }

  error ("invalid initializer");
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
   constants that the assembler and linker can compute them.  */

static tree
process_init_constructor (type, init, elts)
     tree type, init, *elts;
{
  register tree tail;
  /* List of the elements of the result constructor,
     in reverse order.  */
  register tree members = NULL;
  register tree next1;
  tree result;
  int allconstant = 1;
  int allsimple = 1;
  int erroneous = 0;

  /* Make TAIL be the list of elements to use for the initialization,
     no matter how the data was given to us.  */

  if (elts)
    {
      if (warn_missing_braces)
	warning ("aggregate has a partly bracketed initializer");
      tail = *elts;
    }
  else
    tail = CONSTRUCTOR_ELTS (init);

  /* Gobble as many elements as needed, and make a constructor or initial value
     for each element of this aggregate.  Chain them together in result.
     If there are too few, use 0 for each scalar ultimate component.  */

  if (TREE_CODE (type) == ARRAY_TYPE || TREE_CODE (type) == VECTOR_TYPE)
    {
      register long len;
      register int i;

      if (TREE_CODE (type) == ARRAY_TYPE)
	{
	  tree domain = TYPE_DOMAIN (type);
	  if (domain)
	    len = (TREE_INT_CST_LOW (TYPE_MAX_VALUE (domain))
		   - TREE_INT_CST_LOW (TYPE_MIN_VALUE (domain))
		   + 1);
	  else
	    len = -1;  /* Take as many as there are */
	}
      else
	{
	  /* Vectors are like simple fixed-size arrays.  */
	  len = TYPE_VECTOR_SUBPARTS (type);
	}

      for (i = 0; len < 0 || i < len; i++)
	{
	  if (tail)
	    {
	      if (TREE_PURPOSE (tail)
		  && (TREE_CODE (TREE_PURPOSE (tail)) != INTEGER_CST
		      || compare_tree_int (TREE_PURPOSE (tail), i) != 0))
		sorry ("non-trivial labeled initializers");

	      if (TREE_VALUE (tail) != 0)
		{
		  tree tail1 = tail;
		  next1 = digest_init (TREE_TYPE (type),
				       TREE_VALUE (tail), &tail1);
		  if (next1 == error_mark_node)
		    return next1;
		  my_friendly_assert
		    (same_type_ignoring_top_level_qualifiers_p
		     (TREE_TYPE (type), TREE_TYPE (next1)),
		     981123);
		  my_friendly_assert (tail1 == 0
				      || TREE_CODE (tail1) == TREE_LIST, 319);
		  if (tail == tail1 && len < 0)
		    {
		      error ("non-empty initializer for array of empty elements");
		      /* Just ignore what we were supposed to use.  */
		      tail1 = NULL_TREE;
		    }
		  tail = tail1;
		}
	      else
		{
		  next1 = error_mark_node;
		  tail = TREE_CHAIN (tail);
		}
	    }
	  else if (len < 0)
	    /* We're done.  */
	    break;
	  else if (TYPE_NEEDS_CONSTRUCTING (TREE_TYPE (type)))
	    {
	      /* If this type needs constructors run for
		 default-initialization, we can't rely on the backend to do it
		 for us, so build up TARGET_EXPRs.  If the type in question is
		 a class, just build one up; if it's an array, recurse.  */

	      if (IS_AGGR_TYPE (TREE_TYPE (type)))
		next1 = build_functional_cast (TREE_TYPE (type), NULL_TREE);
	      else
		next1 = build (CONSTRUCTOR, NULL_TREE, NULL_TREE, NULL_TREE);
	      next1 = digest_init (TREE_TYPE (type), next1, 0);
	    }
	  else if (! zero_init_p (TREE_TYPE (type)))
	    next1 = build_zero_init (TREE_TYPE (type),
				     /*nelts=*/NULL_TREE,
				     /*static_storage_p=*/false);
	  else
	    /* The default zero-initialization is fine for us; don't
	       add anything to the CONSTRUCTOR.  */
	    break;

	  if (next1 == error_mark_node)
	    erroneous = 1;
	  else if (!TREE_CONSTANT (next1))
	    allconstant = 0;
	  else if (! initializer_constant_valid_p (next1, TREE_TYPE (next1)))
	    allsimple = 0;
	  members = tree_cons (size_int (i), next1, members);
	}
    }
  else if (TREE_CODE (type) == RECORD_TYPE)
    {
      register tree field;

      if (tail)
	{
	  if (TYPE_USES_VIRTUAL_BASECLASSES (type))
	    {
	      sorry ("initializer list for object of class with virtual base classes");
	      return error_mark_node;
	    }

	  if (TYPE_BINFO_BASETYPES (type))
	    {
	      sorry ("initializer list for object of class with base classes");
	      return error_mark_node;
	    }

	  if (TYPE_POLYMORPHIC_P (type))
	    {
	      sorry ("initializer list for object using virtual functions");
	      return error_mark_node;
	    }
	}

      for (field = TYPE_FIELDS (type); field;
	   field = TREE_CHAIN (field))
	{
	  if (! DECL_NAME (field) && DECL_C_BIT_FIELD (field))
	    {
	      members = tree_cons (field, integer_zero_node, members);
	      continue;
	    }

	  if (TREE_CODE (field) != FIELD_DECL || DECL_ARTIFICIAL (field))
	    continue;

	  if (tail)
	    {
	      if (TREE_PURPOSE (tail)
		  && TREE_PURPOSE (tail) != field
		  && TREE_PURPOSE (tail) != DECL_NAME (field))
		sorry ("non-trivial labeled initializers");

	      if (TREE_VALUE (tail) != 0)
		{
		  tree tail1 = tail;

		  next1 = digest_init (TREE_TYPE (field),
				       TREE_VALUE (tail), &tail1);
		  my_friendly_assert (tail1 == 0
				      || TREE_CODE (tail1) == TREE_LIST, 320);
		  tail = tail1;
		}
	      else
		{
		  next1 = error_mark_node;
		  tail = TREE_CHAIN (tail);
		}
	    }
	  else if (TYPE_NEEDS_CONSTRUCTING (TREE_TYPE (field)))
	    {
	      /* If this type needs constructors run for
		 default-initialization, we can't rely on the backend to do it
		 for us, so build up TARGET_EXPRs.  If the type in question is
		 a class, just build one up; if it's an array, recurse.  */

	      if (IS_AGGR_TYPE (TREE_TYPE (field)))
		next1 = build_functional_cast (TREE_TYPE (field),
					       NULL_TREE);
	      else
	        {
		  next1 = build (CONSTRUCTOR, NULL_TREE, NULL_TREE,
			         NULL_TREE);
                  if (init)
                    TREE_HAS_CONSTRUCTOR (next1)
                       = TREE_HAS_CONSTRUCTOR (init);
                }
	      next1 = digest_init (TREE_TYPE (field), next1, 0);

	      /* Warn when some struct elements are implicitly initialized.  */
	      if (extra_warnings
	          && (!init || TREE_HAS_CONSTRUCTOR (init)))
		warning ("missing initializer for member `%D'", field);
	    }
	  else
	    {
	      if (TREE_READONLY (field))
		error ("uninitialized const member `%D'", field);
	      else if (CLASSTYPE_READONLY_FIELDS_NEED_INIT (TREE_TYPE (field)))
		error ("member `%D' with uninitialized const fields",
			  field);
	      else if (TREE_CODE (TREE_TYPE (field)) == REFERENCE_TYPE)
		error ("member `%D' is uninitialized reference", field);

	      /* Warn when some struct elements are implicitly initialized
		 to zero.  */
	      if (extra_warnings
	          && (!init || TREE_HAS_CONSTRUCTOR (init)))
		warning ("missing initializer for member `%D'", field);

	      if (! zero_init_p (TREE_TYPE (field)))
		next1 = build_zero_init (TREE_TYPE (field),
					 /*nelts=*/NULL_TREE,
					 /*static_storage_p=*/false);
	      else
		/* The default zero-initialization is fine for us; don't
		   add anything to the CONSTRUCTOR.  */
		continue;
	    }

	  if (next1 == error_mark_node)
	    erroneous = 1;
	  else if (!TREE_CONSTANT (next1))
	    allconstant = 0;
	  else if (! initializer_constant_valid_p (next1, TREE_TYPE (next1)))
	    allsimple = 0;
	  members = tree_cons (field, next1, members);
	}
    }
  else if (TREE_CODE (type) == UNION_TYPE
	   /* If the initializer was empty, use default zero initialization.  */
	   && tail)
    {
      register tree field = TYPE_FIELDS (type);

      /* Find the first named field.  ANSI decided in September 1990
	 that only named fields count here.  */
      while (field && (!DECL_NAME (field) || TREE_CODE (field) != FIELD_DECL))
	field = TREE_CHAIN (field);

      /* If this element specifies a field, initialize via that field.  */
      if (TREE_PURPOSE (tail) != NULL_TREE)
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
		error ("no field `%D' in union being initialized",
			  TREE_PURPOSE (tail));
	    }
	  if (!win)
	    TREE_VALUE (tail) = error_mark_node;
	}
      else if (field == 0)
	{
	  error ("union `%T' with no named members cannot be initialized",
		    type);
	  TREE_VALUE (tail) = error_mark_node;
	}

      if (TREE_VALUE (tail) != 0)
	{
	  tree tail1 = tail;

	  next1 = digest_init (TREE_TYPE (field),
			       TREE_VALUE (tail), &tail1);
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
      members = tree_cons (field, next1, members);
    }

  /* If arguments were specified as a list, just remove the ones we used.  */
  if (elts)
    *elts = tail;
  /* If arguments were specified as a constructor,
     complain unless we used all the elements of the constructor.  */
  else if (tail)
    pedwarn ("excess elements in aggregate initializer");

  if (erroneous)
    return error_mark_node;

  result = build (CONSTRUCTOR, type, NULL_TREE, nreverse (members));
  if (TREE_CODE (type) == ARRAY_TYPE && TYPE_DOMAIN (type) == NULL_TREE)
    complete_array_type (type, result, /*do_default=*/0);
  if (init)
    TREE_HAS_CONSTRUCTOR (result) = TREE_HAS_CONSTRUCTOR (init);
  if (allconstant) TREE_CONSTANT (result) = 1;
  if (allconstant && allsimple) TREE_STATIC (result) = 1;
  return result;
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
build_scoped_ref (datum, basetype, binfo_p)
     tree datum;
     tree basetype;
     tree *binfo_p;
{
  tree binfo;

  if (datum == error_mark_node)
    return error_mark_node;
  if (*binfo_p)
    binfo = *binfo_p;
  else
    binfo = lookup_base (TREE_TYPE (datum), basetype, ba_check, NULL);

  if (!binfo || binfo == error_mark_node)
    {
      *binfo_p = NULL_TREE;
      if (!binfo)
	error_not_base_type (basetype, TREE_TYPE (datum));
      return error_mark_node;
    }

  *binfo_p = binfo;
  return build_base_path (PLUS_EXPR, datum, binfo, 1);
}

/* Build a reference to an object specified by the C++ `->' operator.
   Usually this just involves dereferencing the object, but if the
   `->' operator is overloaded, then such overloads must be
   performed until an object which does not have the `->' operator
   overloaded is found.  An error is reported when circular pointer
   delegation is detected.  */

tree
build_x_arrow (datum)
     tree datum;
{
  tree types_memoized = NULL_TREE;
  register tree rval = datum;
  tree type = TREE_TYPE (rval);
  tree last_rval = NULL_TREE;

  if (type == error_mark_node)
    return error_mark_node;

  if (processing_template_decl)
    return build_min_nt (ARROW_EXPR, rval);

  if (TREE_CODE (rval) == OFFSET_REF)
    {
      rval = resolve_offset_ref (datum);
      type = TREE_TYPE (rval);
    }

  if (TREE_CODE (type) == REFERENCE_TYPE)
    {
      rval = convert_from_reference (rval);
      type = TREE_TYPE (rval);
    }

  if (IS_AGGR_TYPE (type))
    {
      while ((rval = build_opfncall (COMPONENT_REF, LOOKUP_NORMAL, rval,
				     NULL_TREE, NULL_TREE)))
	{
	  if (rval == error_mark_node)
	    return error_mark_node;

	  if (value_member (TREE_TYPE (rval), types_memoized))
	    {
	      error ("circular pointer delegation detected");
	      return error_mark_node;
	    }
	  else
	    {
	      types_memoized = tree_cons (NULL_TREE, TREE_TYPE (rval),
					  types_memoized);
	    }
	  last_rval = rval;
	}     

      if (last_rval == NULL_TREE)
	{
	  error ("base operand of `->' has non-pointer type `%T'", type);
	  return error_mark_node;
	}

      if (TREE_CODE (TREE_TYPE (last_rval)) == REFERENCE_TYPE)
	last_rval = convert_from_reference (last_rval);
    }
  else
    last_rval = default_conversion (rval);

  if (TREE_CODE (TREE_TYPE (last_rval)) == POINTER_TYPE)
    return build_indirect_ref (last_rval, NULL);

  if (types_memoized)
    error ("result of `operator->()' yields non-pointer result");
  else
    error ("base operand of `->' is not a pointer");
  return error_mark_node;
}

/* Make an expression to refer to the COMPONENT field of
   structure or union value DATUM.  COMPONENT is an arbitrary
   expression.  DATUM has not already been checked out to be of
   aggregate type.

   For C++, COMPONENT may be a TREE_LIST.  This happens when we must
   return an object of member type to a method of the current class,
   but there is not yet enough typing information to know which one.
   As a special case, if there is only one method by that name,
   it is returned.  Otherwise we return an expression which other
   routines will have to know how to deal with later.  */

tree
build_m_component_ref (datum, component)
     tree datum, component;
{
  tree type;
  tree objtype;
  tree field_type;
  int type_quals;
  tree binfo;

  if (processing_template_decl)
    return build_min_nt (DOTSTAR_EXPR, datum, component);

  datum = decay_conversion (datum);

  if (datum == error_mark_node || component == error_mark_node)
    return error_mark_node;

  objtype = TYPE_MAIN_VARIANT (TREE_TYPE (datum));  

  if (TYPE_PTRMEMFUNC_P (TREE_TYPE (component)))
    {
      type = TREE_TYPE (TYPE_PTRMEMFUNC_FN_TYPE (TREE_TYPE (component)));
      field_type = type;
    }
  else if (TYPE_PTRMEM_P (TREE_TYPE (component)))
    {
      type = TREE_TYPE (TREE_TYPE (component));
      field_type = TREE_TYPE (type);
      
      /* Compute the type of the field, as described in [expr.ref].  */
      type_quals = TYPE_UNQUALIFIED;
      if (TREE_CODE (field_type) == REFERENCE_TYPE)
	/* The standard says that the type of the result should be the
       	   type referred to by the reference.  But for now, at least,
       	   we do the conversion from reference type later.  */
	;
      else
	{
	  type_quals = (cp_type_quals (field_type)  
			| cp_type_quals (TREE_TYPE (datum)));

	  /* There's no such thing as a mutable pointer-to-member, so
	     things are not as complex as they are for references to
	     non-static data members.  */
	  field_type = cp_build_qualified_type (field_type, type_quals);
	}
    }
  else
    {
      error ("`%E' cannot be used as a member pointer, since it is of type `%T'", 
		component, TREE_TYPE (component));
      return error_mark_node;
    }

  if (! IS_AGGR_TYPE (objtype))
    {
      error ("cannot apply member pointer `%E' to `%E', which is of non-aggregate type `%T'",
		component, datum, objtype);
      return error_mark_node;
    }

  binfo = lookup_base (objtype, TYPE_METHOD_BASETYPE (type),
		       ba_check, NULL);
  if (!binfo)
    {
      error ("member type `%T::' incompatible with object type `%T'",
		TYPE_METHOD_BASETYPE (type), objtype);
      return error_mark_node;
    }
  else if (binfo == error_mark_node)
    return error_mark_node;

  component = build (OFFSET_REF, field_type, datum, component);
  if (TREE_CODE (type) == OFFSET_TYPE)
    component = resolve_offset_ref (component);
  return component;
}

/* Return a tree node for the expression TYPENAME '(' PARMS ')'.  */

tree
build_functional_cast (exp, parms)
     tree exp;
     tree parms;
{
  /* This is either a call to a constructor,
     or a C cast in C++'s `functional' notation.  */
  tree type;

  if (exp == error_mark_node || parms == error_mark_node)
    return error_mark_node;

  if (TREE_CODE (exp) == IDENTIFIER_NODE)
    {
      if (IDENTIFIER_HAS_TYPE_VALUE (exp))
	/* Either an enum or an aggregate type.  */
	type = IDENTIFIER_TYPE_VALUE (exp);
      else
	{
	  type = lookup_name (exp, 1);
	  if (!type || TREE_CODE (type) != TYPE_DECL)
	    {
	      error ("`%T' fails to be a typedef or built-in type", exp);
	      return error_mark_node;
	    }
	  type = TREE_TYPE (type);
	}
    }
  else if (TREE_CODE (exp) == TYPE_DECL)
    type = TREE_TYPE (exp);
  else
    type = exp;

  if (processing_template_decl)
    return build_min (CAST_EXPR, type, parms);

  if (! IS_AGGR_TYPE (type))
    {
      /* this must build a C cast */
      if (parms == NULL_TREE)
	parms = integer_zero_node;
      else
	{
	  if (TREE_CHAIN (parms) != NULL_TREE)
	    pedwarn ("initializer list being treated as compound expression");
	  parms = build_compound_expr (parms);
	}

      return build_c_cast (type, parms);
    }

  /* Prepare to evaluate as a call to a constructor.  If this expression
     is actually used, for example,
	 
     return X (arg1, arg2, ...);
	 
     then the slot being initialized will be filled in.  */

  if (!complete_type_or_else (type, NULL_TREE))
    return error_mark_node;
  if (abstract_virtuals_error (NULL_TREE, type))
    return error_mark_node;

  if (parms && TREE_CHAIN (parms) == NULL_TREE)
    return build_c_cast (type, TREE_VALUE (parms));

  /* We need to zero-initialize POD types.  Let's do that for everything
     that doesn't need a constructor.  */
  if (parms == NULL_TREE && !TYPE_NEEDS_CONSTRUCTING (type)
      && TYPE_HAS_DEFAULT_CONSTRUCTOR (type))
    {
      exp = build (CONSTRUCTOR, type, NULL_TREE, NULL_TREE);
      return get_target_expr (exp);
    }

  exp = build_special_member_call (NULL_TREE, complete_ctor_identifier, parms,
				   TYPE_BINFO (type), LOOKUP_NORMAL);

  if (exp == error_mark_node)
    return error_mark_node;

  return build_cplus_new (type, exp);
}


/* Complain about defining new types in inappropriate places.  We give an
   exception for C-style casts, to accommodate GNU C stylings.  */

void
check_for_new_type (string, inptree)
     const char *string;
     flagged_type_tree inptree;
{
  if (inptree.new_type_flag
      && (pedantic || strcmp (string, "cast") != 0))
    pedwarn ("ISO C++ forbids defining types within %s", string);
}

/* Add new exception specifier SPEC, to the LIST we currently have.
   If it's already in LIST then do nothing.
   Moan if it's bad and we're allowed to. COMPLAIN < 0 means we
   know what we're doing.  */

tree
add_exception_specifier (list, spec, complain)
     tree list, spec;
     int complain;
{
  int ok;
  tree core = spec;
  int is_ptr;
  int diag_type = -1; /* none */
  
  if (spec == error_mark_node)
    return list;
  
  my_friendly_assert (spec && (!list || TREE_VALUE (list)), 19990317);
  
  /* [except.spec] 1, type in an exception specifier shall not be
     incomplete, or pointer or ref to incomplete other than pointer
     to cv void.  */
  is_ptr = TREE_CODE (core) == POINTER_TYPE;
  if (is_ptr || TREE_CODE (core) == REFERENCE_TYPE)
    core = TREE_TYPE (core);
  if (complain < 0)
    ok = 1;
  else if (VOID_TYPE_P (core))
    ok = is_ptr;
  else if (TREE_CODE (core) == TEMPLATE_TYPE_PARM)
    ok = 1;
  else if (processing_template_decl)
    ok = 1;
  else
    {
      ok = 1;
      /* 15.4/1 says that types in an exception specifier must be complete,
         but it seems more reasonable to only require this on definitions
         and calls.  So just give a pedwarn at this point; we will give an
         error later if we hit one of those two cases.  */
      if (!COMPLETE_TYPE_P (complete_type (core)))
	diag_type = 2; /* pedwarn */
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
    diag_type = 0; /* error */
    
  if (diag_type >= 0 && complain)
    cxx_incomplete_type_diagnostic (NULL_TREE, core, diag_type);

  return list;
}

/* Combine the two exceptions specifier lists LIST and ADD, and return
   their union.  */

tree
merge_exception_specifiers (list, add)
     tree list, add;
{
  if (!list || !add)
    return NULL_TREE;
  else if (!TREE_VALUE (list))
    return add;
  else if (!TREE_VALUE (add))
    return list;
  else
    {
      tree orig_list = list;
      
      for (; add; add = TREE_CHAIN (add))
        {
          tree spec = TREE_VALUE (add);
          tree probe;
          
          for (probe = orig_list; probe; probe = TREE_CHAIN (probe))
            if (same_type_p (TREE_VALUE (probe), spec))
              break;
          if (!probe)
            {
              spec = build_tree_list (NULL_TREE, spec);
              TREE_CHAIN (spec) = list;
              list = spec;
            }
        }
    }
  return list;
}

/* Subroutine of build_call.  Ensure that each of the types in the
   exception specification is complete.  Technically, 15.4/1 says that
   they need to be complete when we see a declaration of the function,
   but we should be able to get away with only requiring this when the
   function is defined or called.  See also add_exception_specifier.  */

void
require_complete_eh_spec_types (fntype, decl)
     tree fntype, decl;
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
	      ("call to function `%D' which throws incomplete type `%#T'",
	       decl, type);
	  else
	    error ("call to function which throws incomplete type `%#T'",
		   decl);
	}
    }
}
