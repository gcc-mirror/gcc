/* Handle types for the GNU compiler for the Java(TM) language.
   Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2003, 2004, 2005
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

/* Written by Per Bothner <bothner@cygnus.com> */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "real.h"
#include "obstack.h"
#include "flags.h"
#include "java-tree.h"
#include "jcf.h"
#include "convert.h"
#include "toplev.h"
#include "ggc.h"

static tree convert_ieee_real_to_integer (tree, tree);
static tree parse_signature_type (const unsigned char **,
				  const unsigned char *);
static tree lookup_do (tree, int, tree, tree, tree (*)(tree));
static tree build_null_signature (tree);

tree * type_map;

/* Set the type of the local variable with index SLOT to TYPE. */

void
set_local_type (int slot, tree type)
{
  int max_locals = DECL_MAX_LOCALS(current_function_decl);
  int nslots = TYPE_IS_WIDE (type) ? 2 : 1;

  if (slot < 0 || slot + nslots - 1 >= max_locals)
    abort ();

  type_map[slot] = type;
  while (--nslots > 0)
    type_map[++slot] = void_type_node;
}

/* Convert an IEEE real to an integer type.  The result of such a
   conversion when the source operand is a NaN isn't defined by
   IEEE754, but by the Java language standard: it must be zero.  Also,
   overflows must be clipped to within range.  This conversion
   produces something like:

      ((expr >= (float)MAX_INT)
       ? MAX_INT 
       : ((expr <= (float)MIN_INT)
	  ? MIN_INT
	  : ((expr != expr)
	     ? 0 
	     : (int)expr))) */

static tree
convert_ieee_real_to_integer (tree type, tree expr)
{
  tree result;
  expr = save_expr (expr);

  result = fold_build3 (COND_EXPR, type,
			fold_build2 (NE_EXPR, boolean_type_node, expr, expr),
			 convert (type, integer_zero_node),
			 convert_to_integer (type, expr));
  
  result = fold_build3 (COND_EXPR, type, 
			fold_build2 (LE_EXPR, boolean_type_node, expr, 
				     convert (TREE_TYPE (expr), 
					      TYPE_MIN_VALUE (type))),
			TYPE_MIN_VALUE (type),
			result);
  
  result = fold_build3 (COND_EXPR, type,
			fold_build2 (GE_EXPR, boolean_type_node, expr, 
				     convert (TREE_TYPE (expr), 
					      TYPE_MAX_VALUE (type))),
			TYPE_MAX_VALUE (type),
			result);

  return result;
}  

/* Create an expression whose value is that of EXPR,
   converted to type TYPE.  The TREE_TYPE of the value
   is always TYPE.  This function implements all reasonable
   conversions; callers should filter out those that are
   not permitted by the language being compiled.  */

tree
convert (tree type, tree expr)
{
  enum tree_code code = TREE_CODE (type);

  if (!expr)
   return error_mark_node;

  if (type == TREE_TYPE (expr)
      || TREE_CODE (expr) == ERROR_MARK)
    return expr;
  if (TREE_CODE (TREE_TYPE (expr)) == ERROR_MARK)
    return error_mark_node;
  if (code == VOID_TYPE)
    return build1 (CONVERT_EXPR, type, expr);
  if (code == BOOLEAN_TYPE || code ==  CHAR_TYPE)
    return fold_convert (type, expr);
  if (code == INTEGER_TYPE)
    {
      if ((really_constant_p (expr)
	   || (! flag_unsafe_math_optimizations
	       && ! flag_emit_class_files))
	  && TREE_CODE (TREE_TYPE (expr)) == REAL_TYPE
	  && TARGET_FLOAT_FORMAT == IEEE_FLOAT_FORMAT)
	return convert_ieee_real_to_integer (type, expr);
      else
	{
	  /* fold very helpfully sets the overflow status if a type
	     overflows in a narrowing integer conversion, but Java
	     doesn't care.  */
	  tree tmp = fold (convert_to_integer (type, expr));
	  if (TREE_CODE (tmp) == INTEGER_CST)
	    TREE_OVERFLOW (tmp) = 0;
	  return tmp;
	}
    }	  
  if (code == REAL_TYPE)
    return fold (convert_to_real (type, expr));
  if (code == POINTER_TYPE)
    return fold (convert_to_pointer (type, expr));
  error ("conversion to non-scalar type requested");
  return error_mark_node;
}


/* Return a data type that has machine mode MODE.
   If the mode is an integer,
   then UNSIGNEDP selects between signed and unsigned types.  */

tree
java_type_for_mode (enum machine_mode mode, int unsignedp)
{
  if (mode == TYPE_MODE (int_type_node))
    return unsignedp ? unsigned_int_type_node : int_type_node;
  if (mode == TYPE_MODE (long_type_node))
    return unsignedp ? unsigned_long_type_node : long_type_node;
  if (mode == TYPE_MODE (short_type_node))
    return unsignedp ? unsigned_short_type_node : short_type_node;
  if (mode == TYPE_MODE (byte_type_node))
    return unsignedp ? unsigned_byte_type_node : byte_type_node;
  if (mode == TYPE_MODE (float_type_node))
    return float_type_node;
  if (mode == TYPE_MODE (double_type_node))
    return double_type_node;

  return 0;
}

/* Return an integer type with BITS bits of precision,
   that is unsigned if UNSIGNEDP is nonzero, otherwise signed.  */

tree
java_type_for_size (unsigned bits, int unsignedp)
{
  if (bits <= TYPE_PRECISION (byte_type_node))
    return unsignedp ? unsigned_byte_type_node : byte_type_node;
  if (bits <= TYPE_PRECISION (short_type_node))
    return unsignedp ? unsigned_short_type_node : short_type_node;
  if (bits <= TYPE_PRECISION (int_type_node))
    return unsignedp ? unsigned_int_type_node : int_type_node;
  if (bits <= TYPE_PRECISION (long_type_node))
    return unsignedp ? unsigned_long_type_node : long_type_node;
  return 0;
}

/* Return a type the same as TYPE except unsigned or
   signed according to UNSIGNEDP.  */

tree
java_signed_or_unsigned_type (int unsignedp, tree type)
{
  if (! INTEGRAL_TYPE_P (type))
    return type;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (int_type_node))
    return unsignedp ? unsigned_int_type_node : int_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (byte_type_node))
    return unsignedp ? unsigned_byte_type_node : byte_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (short_type_node))
    return unsignedp ? unsigned_short_type_node : short_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (long_type_node))
    return unsignedp ? unsigned_long_type_node : long_type_node;
  return type;
}

/* Return a signed type the same as TYPE in other respects.  */

tree
java_signed_type (tree type)
{
  return java_signed_or_unsigned_type (0, type);
}

/* Return an unsigned type the same as TYPE in other respects.  */

tree
java_unsigned_type (tree type)
{
  return java_signed_or_unsigned_type (1, type);
}

/* Mark EXP saying that we need to be able to take the
   address of it; it should not be allocated in a register.
   Value is true if successful.  */

bool
java_mark_addressable (tree exp)
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

      case TRUTH_ANDIF_EXPR:
      case TRUTH_ORIF_EXPR:
      case COMPOUND_EXPR:
	x = TREE_OPERAND (x, 1);
	break;

      case COND_EXPR:
	return java_mark_addressable (TREE_OPERAND (x, 1))
	  && java_mark_addressable (TREE_OPERAND (x, 2));

      case CONSTRUCTOR:
	TREE_ADDRESSABLE (x) = 1;
	return true;

      case INDIRECT_REF:
	/* We sometimes add a cast *(TYPE*)&FOO to handle type and mode
	   incompatibility problems.  Handle this case by marking FOO.  */
	if (TREE_CODE (TREE_OPERAND (x, 0)) == NOP_EXPR
	    && TREE_CODE (TREE_OPERAND (TREE_OPERAND (x, 0), 0)) == ADDR_EXPR)
	  {
	    x = TREE_OPERAND (TREE_OPERAND (x, 0), 0);
	    break;
	  }
	if (TREE_CODE (TREE_OPERAND (x, 0)) == ADDR_EXPR)
	  {
	    x = TREE_OPERAND (x, 0);
	    break;
	  }
	return true;

      case VAR_DECL:
      case CONST_DECL:
      case PARM_DECL:
      case RESULT_DECL:
      case FUNCTION_DECL:
	TREE_ADDRESSABLE (x) = 1;
#if 0  /* poplevel deals with this now.  */
	if (DECL_CONTEXT (x) == 0)
	  TREE_ADDRESSABLE (DECL_ASSEMBLER_NAME (x)) = 1;
#endif
	/* drops through */
      default:
	return true;
    }
}

/* Thorough checking of the arrayness of TYPE.  */

int
is_array_type_p (tree type)
{
  return TREE_CODE (type) == POINTER_TYPE
    && TREE_CODE (TREE_TYPE (type)) == RECORD_TYPE
    && TYPE_ARRAY_P (TREE_TYPE (type));
}

/* Return the length of a Java array type.
   Return -1 if the length is unknown or non-constant. */

HOST_WIDE_INT
java_array_type_length (tree array_type)
{
  tree arfld;
  if (TREE_CODE (array_type) == POINTER_TYPE)
    array_type = TREE_TYPE (array_type);
  arfld = TREE_CHAIN (TREE_CHAIN (TYPE_FIELDS (array_type)));
  if (arfld != NULL_TREE)
    {
      tree index_type = TYPE_DOMAIN (TREE_TYPE (arfld));
      if (index_type != NULL_TREE)
	{
	  tree high = TYPE_MAX_VALUE (index_type);
	  if (TREE_CODE (high) == INTEGER_CST)
	    return TREE_INT_CST_LOW (high) + 1;
	}
    }
  return -1;
}

/* An array of unknown length will be ultimately given a length of
   -2, so that we can still have `length' producing a negative value
   even if found. This was part of an optimization aiming at removing
   `length' from static arrays. We could restore it, FIXME.  */

tree
build_prim_array_type (tree element_type, HOST_WIDE_INT length)
{
  tree index = NULL;

  if (length != -1)
    {
      tree max_index = build_int_cst (sizetype, length - 1);
      index = build_index_type (max_index);
    }
  return build_array_type (element_type, index);
}

/* Return a Java array type with a given ELEMENT_TYPE and LENGTH.
   These are hashed (shared) using IDENTIFIER_SIGNATURE_TYPE.
   The LENGTH is -1 if the length is unknown. */

tree
build_java_array_type (tree element_type, HOST_WIDE_INT length)
{
  tree sig, t, fld, atype, arfld;
  char buf[12];
  tree elsig = build_java_signature (element_type);
  tree el_name = element_type;
  buf[0] = '[';
  if (length >= 0)
    sprintf (buf+1, HOST_WIDE_INT_PRINT_DEC, length);
  else
    buf[1] = '\0';
  sig = ident_subst (IDENTIFIER_POINTER (elsig), IDENTIFIER_LENGTH (elsig),
		     buf, 0, 0, "");
  t = IDENTIFIER_SIGNATURE_TYPE (sig);
  if (t != NULL_TREE)
    return TREE_TYPE (t);
  t = make_class ();
  IDENTIFIER_SIGNATURE_TYPE (sig) = build_pointer_type (t);
  TYPE_ARRAY_P (t) = 1;

  if (TREE_CODE (el_name) == POINTER_TYPE)
    el_name = TREE_TYPE (el_name);
  el_name = TYPE_NAME (el_name);
  if (TREE_CODE (el_name) == TYPE_DECL)
    el_name = DECL_NAME (el_name);
  {
    char suffix[12];
    if (length >= 0)
      sprintf (suffix, "[%d]", (int)length); 
    else
      strcpy (suffix, "[]");
    TYPE_NAME (t) 
      = build_decl (TYPE_DECL,
		    identifier_subst (el_name, "", '.', '.', suffix),
                             t);
  }

  set_java_signature (t, sig);
  set_super_info (0, t, object_type_node, 0);
  if (TREE_CODE (element_type) == RECORD_TYPE)
    element_type = promote_type (element_type);
  TYPE_ARRAY_ELEMENT (t) = element_type;

  /* Add length pseudo-field. */
  fld = build_decl (FIELD_DECL, get_identifier ("length"), int_type_node);
  TYPE_FIELDS (t) = fld;
  DECL_CONTEXT (fld) = t;
  FIELD_PUBLIC (fld) = 1;
  FIELD_FINAL (fld) = 1;
  TREE_READONLY (fld) = 1;

  atype = build_prim_array_type (element_type, length);
  arfld = build_decl (FIELD_DECL, get_identifier ("data"), atype);
  DECL_CONTEXT (arfld) = t;
  TREE_CHAIN (fld) = arfld;
  DECL_ALIGN (arfld) = TYPE_ALIGN (element_type);

  /* We could layout_class, but that loads java.lang.Object prematurely.
   * This is called by the parser, and it is a bad idea to do load_class
   * in the middle of parsing, because of possible circularity problems. */
  push_super_field (t, object_type_node);
  layout_type (t);

  return t;
}

/* Promote TYPE to the type actually used for fields and parameters. */

tree
promote_type (tree type)
{
  switch (TREE_CODE (type))
    {
    case RECORD_TYPE:
      return build_pointer_type (type);
    case BOOLEAN_TYPE:
      if (type == boolean_type_node)
	return promoted_boolean_type_node;
      goto handle_int;
    case CHAR_TYPE:
      if (type == char_type_node)
	return promoted_char_type_node;
      goto handle_int;
    case INTEGER_TYPE:
    handle_int:
      if (TYPE_PRECISION (type) < TYPE_PRECISION (int_type_node))
	{
	  if (type == short_type_node)
	    return promoted_short_type_node;
	  if (type == byte_type_node)
	    return promoted_byte_type_node;
	  return int_type_node;
	}
      /* ... else fall through ... */
    default:
      return type;
    }
}

/* Parse a signature string, starting at *PTR and ending at LIMIT.
   Return the seen TREE_TYPE, updating *PTR. */

static tree
parse_signature_type (const unsigned char **ptr, const unsigned char *limit)
{
  tree type;

  if (*ptr >= limit)
    abort ();

  switch (**ptr)
    {
    case 'B':  (*ptr)++;  return byte_type_node;
    case 'C':  (*ptr)++;  return char_type_node;
    case 'D':  (*ptr)++;  return double_type_node;
    case 'F':  (*ptr)++;  return float_type_node;
    case 'S':  (*ptr)++;  return short_type_node;
    case 'I':  (*ptr)++;  return int_type_node;
    case 'J':  (*ptr)++;  return long_type_node;
    case 'Z':  (*ptr)++;  return boolean_type_node;
    case 'V':  (*ptr)++;  return void_type_node;
    case '[':
      for ((*ptr)++; (*ptr) < limit && ISDIGIT (**ptr); ) (*ptr)++;
      type = parse_signature_type (ptr, limit);
      type = build_java_array_type (type, -1); 
      break;
    case 'L':
      {
	const unsigned char *start = ++(*ptr);
	const unsigned char *str = start;
	for ( ; ; str++)
	  {
	    if (str >= limit)
	      abort ();
	    if (*str == ';')
	      break;
	  }
	*ptr = str+1;
	type = lookup_class (unmangle_classname ((const char *) start, str - start));
	break;
      }
    default:
      abort ();
    }
  return promote_type (type);
}

/* Parse a Java "mangled" signature string, starting at SIG_STRING,
   and SIG_LENGTH bytes long.
   Return a gcc type node. */

tree
parse_signature_string (const unsigned char *sig_string, int sig_length)
{
  tree result_type;
  const unsigned char *str = sig_string;
  const unsigned char *limit = str + sig_length;

  if (str < limit && str[0] == '(')
    {
      tree argtype_list = NULL_TREE;
      str++;
      while (str < limit && str[0] != ')')
	{
	  tree argtype = parse_signature_type (&str, limit);
	  argtype_list = tree_cons (NULL_TREE, argtype, argtype_list);
	}
      if (str++, str >= limit)
	abort ();
      result_type = parse_signature_type (&str, limit);
      argtype_list = chainon (nreverse (argtype_list), end_params_node);
      result_type = build_function_type (result_type, argtype_list);
    }
  else
    result_type = parse_signature_type (&str, limit);
  if (str != limit)
    error ("junk at end of signature string");
  return result_type;
}

/* Convert a signature to its type.
 * Uses IDENTIFIER_SIGNATURE_TYPE as a cache (except for primitive types).
 */

tree
get_type_from_signature (tree signature)
{
  const unsigned char *sig = (const unsigned char *) IDENTIFIER_POINTER (signature);
  int len = IDENTIFIER_LENGTH (signature);
  tree type;
  /* Primitive types aren't cached. */
  if (len <= 1)
    return parse_signature_string (sig, len);
  type = IDENTIFIER_SIGNATURE_TYPE (signature);
  if (type == NULL_TREE)
    {
      type = parse_signature_string (sig, len);
      IDENTIFIER_SIGNATURE_TYPE (signature) = type;
    }
  return type;
}

/* Ignore signature and always return null.  Used by has_method. */

static tree
build_null_signature (tree type ATTRIBUTE_UNUSED)
{
  return NULL_TREE;
}

/* Return the signature string for the arguments of method type TYPE. */

tree
build_java_argument_signature (tree type)
{
  extern struct obstack temporary_obstack;
  tree sig = TYPE_ARGUMENT_SIGNATURE (type);
  if (sig == NULL_TREE)
    {
      tree args = TYPE_ARG_TYPES (type);
      if (TREE_CODE (type) == METHOD_TYPE)
	args = TREE_CHAIN (args);  /* Skip "this" argument. */
      for (; args != end_params_node; args = TREE_CHAIN (args))
	{
	  tree t = build_java_signature (TREE_VALUE (args));
	  obstack_grow (&temporary_obstack,
			IDENTIFIER_POINTER (t), IDENTIFIER_LENGTH (t));
	}
      obstack_1grow (&temporary_obstack, '\0');

      sig = get_identifier (obstack_base (&temporary_obstack));
      TYPE_ARGUMENT_SIGNATURE (type) = sig;
      obstack_free (&temporary_obstack, obstack_base (&temporary_obstack));
    }
  return sig;
}

/* Return the signature of the given TYPE. */

tree
build_java_signature (tree type)
{
  tree sig, t;
  while (TREE_CODE (type) == POINTER_TYPE)
    type = TREE_TYPE (type);
  MAYBE_CREATE_TYPE_TYPE_LANG_SPECIFIC (type);
  sig = TYPE_SIGNATURE (type);
  if (sig == NULL_TREE)
    {
      char sg[2];
      switch (TREE_CODE (type))
	{
	case BOOLEAN_TYPE: sg[0] = 'Z';  goto native;
	case CHAR_TYPE:    sg[0] = 'C';  goto native;
	case VOID_TYPE:    sg[0] = 'V';  goto native;
	case INTEGER_TYPE:
	  switch (TYPE_PRECISION (type))
	    {
	    case  8:       sg[0] = 'B';  goto native;
	    case 16:       sg[0] = 'S';  goto native;
	    case 32:       sg[0] = 'I';  goto native;
	    case 64:       sg[0] = 'J';  goto native;
	    default:  goto bad_type;
	    }
	case REAL_TYPE:
	  switch (TYPE_PRECISION (type))
	    {
	    case 32:       sg[0] = 'F';  goto native;
	    case 64:       sg[0] = 'D';  goto native;
	    default:  goto bad_type;
	    }
	native:
	  sg[1] = 0;
	  sig = get_identifier (sg);
	  break;
	case RECORD_TYPE:
	  if (TYPE_ARRAY_P (type))
	    {
	      t = build_java_signature (TYPE_ARRAY_ELEMENT (type));
	      sig = ident_subst (IDENTIFIER_POINTER (t), IDENTIFIER_LENGTH (t),
				 "[", 0, 0, "");
	    }
	  else
	    {
	      t = DECL_NAME (TYPE_NAME (type));
	      sig = ident_subst (IDENTIFIER_POINTER (t), IDENTIFIER_LENGTH (t),
				 "L", '.', '/', ";");
	    }
	  break;
	case METHOD_TYPE:
	case FUNCTION_TYPE:
	  {
	    extern struct obstack temporary_obstack;
	    sig = build_java_argument_signature (type);
	    obstack_1grow (&temporary_obstack, '(');
	    obstack_grow (&temporary_obstack,
			  IDENTIFIER_POINTER (sig), IDENTIFIER_LENGTH (sig));
	    obstack_1grow (&temporary_obstack, ')');

	    t = build_java_signature (TREE_TYPE (type));
	    obstack_grow0 (&temporary_obstack,
			   IDENTIFIER_POINTER (t), IDENTIFIER_LENGTH (t));

	    sig = get_identifier (obstack_base (&temporary_obstack));
	    obstack_free (&temporary_obstack,
			  obstack_base (&temporary_obstack));
	  }
	  break;
	bad_type:
	default:
	  abort ();
	}
      TYPE_SIGNATURE (type) = sig;
    }
  return sig;
}

/* Save signature string SIG (an IDENTIFIER_NODE) in TYPE for future use. */

void
set_java_signature (tree type, tree sig)
{
  tree old_sig;
  while (TREE_CODE (type) == POINTER_TYPE)
    type = TREE_TYPE (type);
  MAYBE_CREATE_TYPE_TYPE_LANG_SPECIFIC (type);
  old_sig = TYPE_SIGNATURE (type);
  if (old_sig != NULL_TREE && old_sig != sig)
    abort ();
  TYPE_SIGNATURE (type) = sig;
#if 0 /* careful about METHOD_TYPE */
  if (IDENTIFIER_SIGNATURE_TYPE (sig) == NULL_TREE && TREE_PERMANENT (type))
    IDENTIFIER_SIGNATURE_TYPE (sig) = type;
#endif
}

/* Search in SEARCHED_CLASS and its superclasses for a method matching
   METHOD_NAME and signature METHOD_SIGNATURE.  This function will
   only search for methods declared in the class hierarchy; interfaces
   will not be considered.  Returns NULL_TREE if the method is not
   found.  */
tree
lookup_argument_method (tree searched_class, tree method_name,
			tree method_signature)
{
  return lookup_do (searched_class, 0,
		    method_name, method_signature, 
		    build_java_argument_signature);
}

/* Like lookup_argument_method, but lets the caller set any flags
   desired.  */
tree
lookup_argument_method_generic (tree searched_class, tree method_name,
				tree method_signature, int flags)
{
  return lookup_do (searched_class, flags,
		    method_name, method_signature, 
		    build_java_argument_signature);
}


/* Search in class SEARCHED_CLASS (and its superclasses) for a method
   matching METHOD_NAME and signature METHOD_SIGNATURE.  Return a
   FUNCTION_DECL on success, or NULL_TREE if none found.  (Contrast
   lookup_argument_method, which ignores return type.)  If
   SEARCHED_CLASS is an interface, search it too. */
tree
lookup_java_method (tree searched_class, tree method_name,
		    tree method_signature)
{
  return lookup_do (searched_class, SEARCH_INTERFACE, method_name, 
		    method_signature, build_java_signature);
}

/* Return true iff CLASS (or its ancestors) has a method METHOD_NAME.  */
int
has_method (tree class, tree method_name)
{
  return lookup_do (class, SEARCH_INTERFACE,
		    method_name, NULL_TREE,
		    build_null_signature) != NULL_TREE;
}

/* Search in class SEARCHED_CLASS, but not its superclasses, for a
   method matching METHOD_NAME and signature SIGNATURE.  A private
   helper for lookup_do.  */
static tree
shallow_find_method (tree searched_class, int flags, tree method_name, 
	     tree signature, tree (*signature_builder) (tree))
{
  tree method;
  for (method = TYPE_METHODS (searched_class);
       method != NULL_TREE;  method = TREE_CHAIN (method))
    {
      tree method_sig = (*signature_builder) (TREE_TYPE (method));
      if (DECL_NAME (method) == method_name && method_sig == signature)
	{
	  /* If the caller requires a visible method, then we
	     skip invisible methods here.  */
	  if (! (flags & SEARCH_VISIBLE)
	      || ! METHOD_INVISIBLE (method))
	    return method;
	}
    }
  return NULL_TREE;
}

/* Search in the superclasses of SEARCHED_CLASS for a method matching
   METHOD_NAME and signature SIGNATURE.  A private helper for
   lookup_do.  */
static tree
find_method_in_superclasses (tree searched_class, int flags, 
                             tree method_name, tree signature,
                             tree (*signature_builder) (tree))
{
  tree klass;
  for (klass = CLASSTYPE_SUPER (searched_class); klass != NULL_TREE;
       klass = CLASSTYPE_SUPER (klass))
    {
      tree method;
      method = shallow_find_method (klass, flags, method_name, 
				    signature, signature_builder);
      if (method != NULL_TREE)
	return method;
    }

  return NULL_TREE;
}

/* Search in the interfaces of SEARCHED_CLASS and its superinterfaces
   for a method matching METHOD_NAME and signature SIGNATURE.  A
   private helper for lookup_do.  */
static tree
find_method_in_interfaces (tree searched_class, int flags, tree method_name,
                           tree signature, tree (*signature_builder) (tree))
{
  int i;
  tree binfo, base_binfo;

  for (binfo = TYPE_BINFO (searched_class), i = 1;
       BINFO_BASE_ITERATE (binfo, i, base_binfo); i++)
    {
      tree iclass = BINFO_TYPE (base_binfo);
      tree method;
	  
      /* If the superinterface hasn't been loaded yet, do so now.  */
      if (CLASS_FROM_SOURCE_P (iclass))
	safe_layout_class (iclass);
      else if (!CLASS_LOADED_P (iclass))
	load_class (iclass, 1);
	  
      /* First, we look in ICLASS.  If that doesn't work we'll
	 recursively look through all its superinterfaces.  */
      method = shallow_find_method (iclass, flags, method_name, 
				    signature, signature_builder);      
      if (method != NULL_TREE)
	return method;
  
      method = find_method_in_interfaces 
	(iclass, flags, method_name, signature, signature_builder);
      if (method != NULL_TREE)
	return method;
    }
  
  return NULL_TREE;
}


/* Search in class SEARCHED_CLASS (and its superclasses) for a method
   matching METHOD_NAME and signature SIGNATURE.  FLAGS control some
   parameters of the search.
   
   SEARCH_INTERFACE means also search interfaces and superinterfaces
   of SEARCHED_CLASS.
   
   SEARCH_SUPER means skip SEARCHED_CLASS and start with its
   superclass.
   
   SEARCH_VISIBLE means skip methods for which METHOD_INVISIBLE is
   set.

   Return the matched method DECL or NULL_TREE.  SIGNATURE_BUILDER is
   used on method candidates to build their (sometimes partial)
   signature.  */
static tree
lookup_do (tree searched_class, int flags, tree method_name,
	   tree signature, tree (*signature_builder) (tree))
{
  tree method;
  tree orig_class = searched_class;
    
  if (searched_class == NULL_TREE)
    return NULL_TREE;

  if (flags & SEARCH_SUPER)
    {
      searched_class = CLASSTYPE_SUPER (searched_class);
      if (searched_class == NULL_TREE)
	return NULL_TREE;
    }

  /* First look in our own methods.  */
  method = shallow_find_method (searched_class, flags, method_name,
				signature, signature_builder);  
  if (method)
    return method;

  /* Then look in our superclasses.  */
  if (! CLASS_INTERFACE (TYPE_NAME (searched_class)))
    method = find_method_in_superclasses (searched_class, flags, method_name,
					  signature, signature_builder);  
  if (method)
    return method;
  
  /* If that doesn't work, look in our interfaces.  */
  if (flags & SEARCH_INTERFACE)
    method = find_method_in_interfaces (orig_class, flags, method_name, 
					signature, signature_builder);
  
  return method;
}

/* Search in class CLAS for a constructor matching METHOD_SIGNATURE.
   Return a FUNCTION_DECL on success, or NULL_TREE if none found. */

tree
lookup_java_constructor (tree clas, tree method_signature)
{
  tree method = TYPE_METHODS (clas);
  for ( ; method != NULL_TREE;  method = TREE_CHAIN (method))
    {
      tree method_sig = build_java_signature (TREE_TYPE (method));
      if (DECL_CONSTRUCTOR_P (method) && method_sig == method_signature)
	return method;
    }
  return NULL_TREE;
}

/* Return a type which is the Binary Numeric Promotion of the pair T1,
   T2 and convert EXP1 and/or EXP2. See 5.6.2 Binary Numeric
   Promotion. It assumes that both T1 and T2 are eligible to BNP. */

tree
binary_numeric_promotion (tree t1, tree t2, tree *exp1, tree *exp2)
{
  if (t1 == double_type_node || t2 == double_type_node)
    {
      if (t1 != double_type_node)
	*exp1 = convert (double_type_node, *exp1);
      if (t2 != double_type_node)
	*exp2 = convert (double_type_node, *exp2);
      return double_type_node;
    }
  if (t1 == float_type_node || t2 == float_type_node)
    {
      if (t1 != float_type_node)
	*exp1 = convert (float_type_node, *exp1);
      if (t2 != float_type_node)
	*exp2 = convert (float_type_node, *exp2);
      return float_type_node;
    }
  if (t1 == long_type_node || t2 == long_type_node)
    {
      if (t1 != long_type_node)
	*exp1 = convert (long_type_node, *exp1);
      if (t2 != long_type_node)
	*exp2 = convert (long_type_node, *exp2);
      return long_type_node;
    }

  if (t1 != int_type_node)
    *exp1 = convert (int_type_node, *exp1);
  if (t2 != int_type_node)
    *exp2 = convert (int_type_node, *exp2);
  return int_type_node;
}
