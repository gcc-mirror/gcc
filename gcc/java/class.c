/* Functions related to building classes and their related objects.
   Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003
   Free Software Foundation, Inc.

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
Boston, MA 02111-1307, USA.

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

/* Written by Per Bothner <bothner@cygnus.com> */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "rtl.h"
#include "flags.h"
#include "java-tree.h"
#include "jcf.h"
#include "obstack.h"
#include "toplev.h"
#include "output.h"
#include "parse.h"
#include "function.h"
#include "ggc.h"
#include "stdio.h"
#include "target.h"

/* DOS brain-damage */
#ifndef O_BINARY
#define O_BINARY 0 /* MS-DOS brain-damage */
#endif

static tree make_method_value PARAMS ((tree));
static tree build_java_method_type PARAMS ((tree, tree, int));
static int32 hashUtf8String PARAMS ((const char *, int));
static tree make_field_value PARAMS ((tree));
static tree get_dispatch_vector PARAMS ((tree));
static tree get_dispatch_table PARAMS ((tree, tree));
static void add_interface_do PARAMS ((tree, tree, int));
static tree maybe_layout_super_class PARAMS ((tree, tree));
static int assume_compiled PARAMS ((const char *));
static tree build_method_symbols_entry PARAMS ((tree));

static GTY(()) rtx registerClass_libfunc;
static GTY(()) rtx registerResource_libfunc;

struct obstack temporary_obstack;

/* The compiler generates different code depending on whether or not
   it can assume certain classes have been compiled down to native
   code or not.  The compiler options -fassume-compiled= and
   -fno-assume-compiled= are used to create a tree of
   assume_compiled_node objects.  This tree is queried to determine if
   a class is assume to be compiled or not.  Each node in the tree
   represents either a package or a specific class.  */

typedef struct assume_compiled_node_struct
{
  /* The class or package name.  */
  const char *ident;

  /* Nonzero if this represents an exclusion.  */
  int excludep;

  /* Pointers to other nodes in the tree.  */
  struct assume_compiled_node_struct *parent;
  struct assume_compiled_node_struct *sibling;
  struct assume_compiled_node_struct *child;
} assume_compiled_node;

static assume_compiled_node *find_assume_compiled_node
			PARAMS ((assume_compiled_node *, const char *));

/* This is the root of the include/exclude tree.  */

static assume_compiled_node *assume_compiled_tree;

static GTY(()) tree class_roots[5];
#define registered_class class_roots[0]
#define fields_ident class_roots[1]  /* get_identifier ("fields") */
#define info_ident class_roots[2]  /* get_identifier ("info") */
#define class_list class_roots[3]
#define class_dtable_decl class_roots[4]

/* Return the node that most closely represents the class whose name
   is IDENT.  Start the search from NODE.  Return NULL if an
   appropriate node does not exist.  */

static assume_compiled_node *
find_assume_compiled_node (node, ident)
     assume_compiled_node *node;
     const char *ident;
{
  while (node)
    {
      size_t node_ident_length = strlen (node->ident);

      /* node_ident_length is zero at the root of the tree.  If the
	 identifiers are the same length, then we have matching
	 classes.  Otherwise check if we've matched an enclosing
	 package name.  */

      if (node_ident_length == 0
	  || (strncmp (ident, node->ident, node_ident_length) == 0
	      && (strlen (ident) == node_ident_length
		  || ident[node_ident_length] == '.')))
	{
	  /* We've found a match, however, there might be a more
             specific match.  */

	  assume_compiled_node *found = find_assume_compiled_node (node->child,
								   ident);
	  if (found)
	    return found;
	  else
	    return node;
	}

      /* No match yet.  Continue through the sibling list.  */
      node = node->sibling;
    }

  /* No match at all in this tree.  */
  return NULL;
}

/* Add a new IDENT to the include/exclude tree.  It's an exclusion
   if EXCLUDEP is nonzero.  */

void
add_assume_compiled (ident, excludep)
     const char *ident;
     int excludep;
{
  assume_compiled_node *parent;
  assume_compiled_node *node = 
    xmalloc (sizeof (assume_compiled_node));

  node->ident = xstrdup (ident);
  node->excludep = excludep;
  node->child = NULL;

  /* Create the root of the tree if it doesn't exist yet.  */

  if (NULL == assume_compiled_tree)
    {
      assume_compiled_tree = xmalloc (sizeof (assume_compiled_node));
      assume_compiled_tree->ident = "";
      assume_compiled_tree->excludep = 0;
      assume_compiled_tree->sibling = NULL;
      assume_compiled_tree->child = NULL;
      assume_compiled_tree->parent = NULL;
    }

  /* Calling the function with the empty string means we're setting
     excludep for the root of the hierarchy.  */

  if (0 == ident[0])
    {
      assume_compiled_tree->excludep = excludep;
      return;
    }

  /* Find the parent node for this new node.  PARENT will either be a
     class or a package name.  Adjust PARENT accordingly.  */

  parent = find_assume_compiled_node (assume_compiled_tree, ident);
  if (ident[strlen (parent->ident)] != '.')
    parent = parent->parent;

  /* Insert NODE into the tree.  */

  node->parent = parent;
  node->sibling = parent->child;
  parent->child = node;
}

/* Returns nonzero if IDENT is the name of a class that the compiler
   should assume has been compiled to FIXME  */

static int
assume_compiled (ident)
     const char *ident;
{
  assume_compiled_node *i;
  int result;
  
  if (NULL == assume_compiled_tree)
    return 1;

  i = find_assume_compiled_node (assume_compiled_tree,
				 ident);

  result = ! i->excludep;
  
  return (result);
}

/* Return an IDENTIFIER_NODE the same as (OLD_NAME, OLD_LENGTH).
   except that characters matching OLD_CHAR are substituted by NEW_CHAR.
   Also, PREFIX is prepended, and SUFFIX is appended. */

tree
ident_subst (old_name, old_length, prefix, old_char, new_char, suffix)
     const char* old_name;
     int old_length;
     const char *prefix;
     int old_char;
     int new_char;
     const char *suffix;
{
  int prefix_len = strlen (prefix);
  int suffix_len = strlen (suffix);
  int i = prefix_len + old_length + suffix_len + 1;
#ifdef __GNUC__
  char buffer[i];
#else
  char *buffer = alloca (i);
#endif
  strcpy (buffer, prefix);
  for (i = 0; i < old_length; i++)
    {
      char ch = old_name[i];
      if (ch == old_char)
	ch = new_char;
      buffer[prefix_len + i] = ch;
    }
  strcpy (buffer + prefix_len + old_length, suffix);
  return get_identifier (buffer);
}

/* Return an IDENTIFIER_NODE the same as OLD_ID,
   except that characters matching OLD_CHAR are substituted by NEW_CHAR.
   Also, PREFIX is prepended, and SUFFIX is appended. */

tree
identifier_subst (old_id, prefix, old_char, new_char, suffix)
     const tree old_id;
     const char *prefix;
     int old_char;
     int new_char;
     const char *suffix;
{
  return ident_subst (IDENTIFIER_POINTER (old_id), IDENTIFIER_LENGTH (old_id),
		      prefix, old_char, new_char, suffix);
}

/* Generate a valid C identifier from the name of the class TYPE,
   prefixed by PREFIX. */

tree
mangled_classname (prefix, type)
  const char *prefix;
  tree type;
{
  tree ident = TYPE_NAME (type);
  if (TREE_CODE (ident) != IDENTIFIER_NODE)
    ident = DECL_NAME (ident);
  return identifier_subst (ident, prefix, '.', '_', "");
}

tree
make_class ()
{
  tree type;
  type = make_node (RECORD_TYPE);
  TYPE_BINFO (type) = make_tree_vec (6);
  MAYBE_CREATE_TYPE_TYPE_LANG_SPECIFIC (type);

  return type;
}

/* Given a fully-qualified classname in NAME (whose length is NAME_LENGTH),
   and where each of the constituents is separated by '/',
   return a corresponding IDENTIFIER_NODE, except using '.' as separator. */

tree
unmangle_classname (name, name_length)
     const char *name;  int name_length;
{
  tree to_return = ident_subst (name, name_length, "", '/', '.', "");
  /* It's not sufficient to compare to_return and get_identifier
     (name) to determine whether to_return is qualified. There are
     cases in signature analysis where name will be stripped of a
     trailing ';'. */
  name = IDENTIFIER_POINTER (to_return);
  while (*name)
    if (*name++ == '.') 
      {
	QUALIFIED_P (to_return) = 1;
	break;
      }
  
  return to_return;
}

tree
push_class (class_type, class_name)
     tree class_type, class_name;
{
  tree decl, signature;
  const char *save_input_filename = input_filename;
  int save_lineno = lineno;
  tree source_name = identifier_subst (class_name, "", '.', '/', ".java");
  CLASS_P (class_type) = 1;
  input_filename = IDENTIFIER_POINTER (source_name);
  lineno = 0;
  decl = build_decl (TYPE_DECL, class_name, class_type);

  /* dbxout needs a DECL_SIZE if in gstabs mode */
  DECL_SIZE (decl) = integer_zero_node;

  input_filename = save_input_filename;
  lineno = save_lineno;
  signature = identifier_subst (class_name, "L", '.', '/', ";");
  IDENTIFIER_SIGNATURE_TYPE (signature) = build_pointer_type (class_type);

  /* Setting DECL_ARTIFICIAL forces dbxout.c to specific the type is
     both a typedef and in the struct name-space.  We may want to re-visit
     this later, but for now it reduces the changes needed for gdb. */
  DECL_ARTIFICIAL (decl) = 1;

  pushdecl_top_level (decl);

  return decl;
}

/* Finds the (global) class named NAME.  Creates the class if not found.
   Also creates associated TYPE_DECL.
   Does not check if the class actually exists, load the class,
   fill in field or methods, or do layout_type. */

tree
lookup_class (name)
     tree name;
{
  tree decl = IDENTIFIER_CLASS_VALUE (name);
  if (decl == NULL_TREE)
    decl = push_class (make_class (), name);
  return TREE_TYPE (decl);
}

void
set_super_info (access_flags, this_class, super_class, interfaces_count)
     int access_flags;
     tree this_class;
     tree super_class;
     int interfaces_count;
{
  int total_supers = interfaces_count;
  tree class_decl = TYPE_NAME (this_class);
  if (super_class)
    total_supers++;

  TYPE_BINFO_BASETYPES (this_class) = make_tree_vec (total_supers);
  if (super_class)
    {
      tree super_binfo = make_tree_vec (6);
      BINFO_TYPE (super_binfo) = super_class;
      BINFO_OFFSET (super_binfo) = integer_zero_node;
      TREE_VIA_PUBLIC (super_binfo) = 1;
      TREE_VEC_ELT (BINFO_BASETYPES (TYPE_BINFO (this_class)), 0)
	= super_binfo;
      CLASS_HAS_SUPER (this_class) = 1;
    }

  set_class_decl_access_flags (access_flags, class_decl);
}

void
set_class_decl_access_flags (access_flags, class_decl)
     int access_flags;
     tree class_decl;
{
  if (access_flags & ACC_PUBLIC)    CLASS_PUBLIC (class_decl) = 1;
  if (access_flags & ACC_FINAL)     CLASS_FINAL (class_decl) = 1;
  if (access_flags & ACC_SUPER)     CLASS_SUPER (class_decl) = 1;
  if (access_flags & ACC_INTERFACE) CLASS_INTERFACE (class_decl) = 1;
  if (access_flags & ACC_ABSTRACT)  CLASS_ABSTRACT (class_decl) = 1;
  if (access_flags & ACC_STATIC)    CLASS_STATIC (class_decl) = 1;
  if (access_flags & ACC_PRIVATE)   CLASS_PRIVATE (class_decl) = 1;
  if (access_flags & ACC_PROTECTED) CLASS_PROTECTED (class_decl) = 1;
  if (access_flags & ACC_STRICT)    CLASS_STRICTFP (class_decl) = 1;
}

/* Return length of inheritance chain of CLAS, where java.lang.Object is 0,
   direct sub-classes of Object are 1, and so on. */

int
class_depth (clas)
     tree clas;
{
  int depth = 0;
  if (! CLASS_LOADED_P (clas))
    load_class (clas, 1);
  if (TYPE_SIZE (clas) == error_mark_node)
    return -1;
  while (clas != object_type_node)
    {
      depth++;
      clas = TYPE_BINFO_BASETYPE (clas, 0);
    }
  return depth;
}

/* Return true iff TYPE2 is an interface that extends interface TYPE1 */

int
interface_of_p (type1, type2)
     tree type1, type2;
{
  int n, i;
  tree basetype_vec;

  if (!(basetype_vec = TYPE_BINFO_BASETYPES (type2)))
    return 0;
  n = TREE_VEC_LENGTH (basetype_vec);
  for (i = 0; i < n; i++)
    {
      tree vec_elt = TREE_VEC_ELT (basetype_vec, i);
      if (vec_elt && BINFO_TYPE (vec_elt) == type1)
	return 1;
    }
  for (i = 0; i < n; i++)
    {
      tree vec_elt = TREE_VEC_ELT (basetype_vec, i);
      if (vec_elt && BINFO_TYPE (vec_elt) 
	  && interface_of_p (type1, BINFO_TYPE (vec_elt)))
	return 1;
    }
  return 0;
}

/* Return true iff TYPE1 inherits from TYPE2. */

int
inherits_from_p (type1, type2)
     tree type1, type2;
{
  while (type1 != NULL_TREE && TREE_CODE (type1) == RECORD_TYPE)
    {
      if (type1 == type2)
	return 1;
      type1 = CLASSTYPE_SUPER (type1);
    }
  return 0;
}

/* Return a 1 iff TYPE1 is an enclosing context for TYPE2 */

int
enclosing_context_p (type1, type2)
     tree type1, type2;
{
  if (!INNER_CLASS_TYPE_P (type2))
    return 0;

  for (type2 = TREE_TYPE (DECL_CONTEXT (TYPE_NAME (type2)));
       type2; 
       type2 = (INNER_CLASS_TYPE_P (type2) ?
		TREE_TYPE (DECL_CONTEXT (TYPE_NAME (type2))) : NULL_TREE))
    {
      if (type2 == type1)
	return 1;
    }

  return 0;
}

/* Return 1 iff there exists a common enclosing context between TYPE1
   and TYPE2.  */

int common_enclosing_context_p (type1, type2)
     tree type1, type2;
{
  if (!PURE_INNER_CLASS_TYPE_P (type1) || !PURE_INNER_CLASS_TYPE_P (type2))
    return 0;
  
  for (type1 = TREE_TYPE (DECL_CONTEXT (TYPE_NAME (type1))); type1; 
       type1 = (PURE_INNER_CLASS_TYPE_P (type1) ?
		TREE_TYPE (DECL_CONTEXT (TYPE_NAME (type1))) : NULL_TREE))
    {
      tree current;
      for (current = TREE_TYPE (DECL_CONTEXT (TYPE_NAME (type2))); current;
	   current = (PURE_INNER_CLASS_TYPE_P (current) ?
		      TREE_TYPE (DECL_CONTEXT (TYPE_NAME (current))) : 
		      NULL_TREE))
	if (type1 == current)
	  return 1;
    }
  return 0;
}

static void
add_interface_do (basetype_vec, interface_class, i)
     tree basetype_vec, interface_class;
     int i;
{
  tree interface_binfo = make_tree_vec (6);
  BINFO_TYPE (interface_binfo) = interface_class;
  BINFO_OFFSET (interface_binfo) = integer_zero_node;
  BINFO_VPTR_FIELD (interface_binfo) = integer_zero_node;
  TREE_VIA_VIRTUAL (interface_binfo) = 1;
  TREE_VIA_PUBLIC (interface_binfo) = 1;
  TREE_VEC_ELT (basetype_vec, i) = interface_binfo;
}

/* Add INTERFACE_CLASS to THIS_CLASS iff INTERFACE_CLASS can't be
   found in THIS_CLASS. Returns NULL_TREE upon success, INTERFACE_CLASS
   if attempt is made to add it twice. */

tree
maybe_add_interface (this_class, interface_class)
     tree this_class, interface_class;
{
  tree basetype_vec = TYPE_BINFO_BASETYPES (this_class);
  int i;
  int n = TREE_VEC_LENGTH (basetype_vec);
  for (i = 0; ; i++)
    {
      if (i >= n)
	{
	  error ("internal error - too many interface type");
	  return NULL_TREE;
	}
      else if (TREE_VEC_ELT (basetype_vec, i) == NULL_TREE)
	break;
      else if (BINFO_TYPE (TREE_VEC_ELT (basetype_vec, i)) == interface_class)
	return interface_class;
    } 
  add_interface_do (basetype_vec, interface_class, i);
  return NULL_TREE;
}

/* Add the INTERFACE_CLASS as one of the interfaces of THIS_CLASS. */

void
add_interface (this_class, interface_class)
     tree this_class, interface_class;
{
  tree basetype_vec = TYPE_BINFO_BASETYPES (this_class);
  int i;
  int n = TREE_VEC_LENGTH (basetype_vec);
  for (i = 0; ; i++)
    {
      if (i >= n)
	{
	  error ("internal error - too many interface type");
	  return;
	}
      else if (TREE_VEC_ELT (basetype_vec, i) == NULL_TREE)
	break;
    }
  add_interface_do (basetype_vec, interface_class, i);
}

#if 0
/* Return the address of a pointer to the first FUNCTION_DECL
   in the list (*LIST) whose DECL_NAME is NAME. */

static tree *
find_named_method (list, name)
     tree *list;
     tree name;
{
  while (*list && DECL_NAME (*list) != name)
    list = &TREE_CHAIN (*list);
  return list;
}
#endif

static tree
build_java_method_type (fntype, this_class, access_flags)
     tree fntype;
     tree this_class;
     int access_flags;
{
  if (access_flags & ACC_STATIC)
    return fntype;
  return build_method_type (this_class, fntype);
}

tree
add_method_1 (this_class, access_flags, name, function_type)
     tree this_class;
     int access_flags;
     tree name;
     tree function_type;
{
  tree method_type, fndecl;

  method_type = build_java_method_type (function_type,
					this_class, access_flags);

  fndecl = build_decl (FUNCTION_DECL, name, method_type);
  DECL_CONTEXT (fndecl) = this_class;

  DECL_LANG_SPECIFIC (fndecl)
    = ggc_alloc_cleared (sizeof (struct lang_decl));
  DECL_LANG_SPECIFIC (fndecl)->desc = LANG_DECL_FUNC;

  /* Initialize the static initializer test table.  */
  
  DECL_FUNCTION_INIT_TEST_TABLE (fndecl) = 
    java_treetreehash_create (10, 1);

  /* Initialize the initialized (static) class table. */
  if (access_flags & ACC_STATIC)
    DECL_FUNCTION_INITIALIZED_CLASS_TABLE (fndecl) =
      htab_create_ggc (50, htab_hash_pointer, htab_eq_pointer, NULL);

  /* Initialize the static method invocation compound list */
  DECL_FUNCTION_STATIC_METHOD_INVOCATION_COMPOUND (fndecl) = NULL_TREE;

  TREE_CHAIN (fndecl) = TYPE_METHODS (this_class);
  TYPE_METHODS (this_class) = fndecl;

  /* Notice that this is a finalizer and update the class type
     accordingly. This is used to optimize instance allocation. */
  if (name == finalize_identifier_node
      && TREE_TYPE (function_type) == void_type_node
      && TREE_VALUE (TYPE_ARG_TYPES (function_type)) == void_type_node)
    HAS_FINALIZER_P (this_class) = 1;

  if (access_flags & ACC_PUBLIC) METHOD_PUBLIC (fndecl) = 1;
  if (access_flags & ACC_PROTECTED) METHOD_PROTECTED (fndecl) = 1;
  if (access_flags & ACC_PRIVATE)
    METHOD_PRIVATE (fndecl) = DECL_INLINE (fndecl) = 1;
  if (access_flags & ACC_NATIVE)
    {
      METHOD_NATIVE (fndecl) = 1;
      DECL_EXTERNAL (fndecl) = 1;
    }
  if (access_flags & ACC_STATIC) 
    METHOD_STATIC (fndecl) = DECL_INLINE (fndecl) = 1;
  if (access_flags & ACC_FINAL) 
    METHOD_FINAL (fndecl) = DECL_INLINE (fndecl) = 1;
  if (access_flags & ACC_SYNCHRONIZED) METHOD_SYNCHRONIZED (fndecl) = 1;
  if (access_flags & ACC_ABSTRACT) METHOD_ABSTRACT (fndecl) = 1;
  if (access_flags & ACC_TRANSIENT) METHOD_TRANSIENT (fndecl) = 1;
  if (access_flags & ACC_STRICT) METHOD_STRICTFP (fndecl) = 1;
  return fndecl;
}

/* Add a method to THIS_CLASS.
   The method's name is NAME.
   Its signature (mangled type) is METHOD_SIG (an IDENTIFIER_NODE). */

tree
add_method (this_class, access_flags, name, method_sig)
     tree this_class;
     int access_flags;
     tree name;
     tree method_sig;
{
  tree function_type, fndecl;
  const unsigned char *sig
    = (const unsigned char *) IDENTIFIER_POINTER (method_sig);

  if (sig[0] != '(')
    fatal_error ("bad method signature");

  function_type = get_type_from_signature (method_sig);
  fndecl = add_method_1 (this_class, access_flags, name, function_type);
  set_java_signature (TREE_TYPE (fndecl), method_sig);
  return fndecl;
}

tree
add_field (class, name, field_type, flags)
     tree class;
     tree name;
     tree field_type;
     int flags;
{
  int is_static = (flags & ACC_STATIC) != 0;
  tree field;
  field = build_decl (is_static ? VAR_DECL : FIELD_DECL, name, field_type);
  TREE_CHAIN (field) = TYPE_FIELDS (class);
  TYPE_FIELDS (class) = field;
  DECL_CONTEXT (field) = class;

  if (flags & ACC_PUBLIC) FIELD_PUBLIC (field) = 1;
  if (flags & ACC_PROTECTED) FIELD_PROTECTED (field) = 1;
  if (flags & ACC_PRIVATE) FIELD_PRIVATE (field) = 1;
  if (flags & ACC_FINAL) FIELD_FINAL (field) = 1;
  if (flags & ACC_VOLATILE) FIELD_VOLATILE (field) = 1;
  if (flags & ACC_TRANSIENT) FIELD_TRANSIENT (field) = 1;
  if (is_static)
    {
      FIELD_STATIC (field) = 1;
      /* Always make field externally visible.  This is required so
	 that native methods can always access the field.  */
      TREE_PUBLIC (field) = 1;
      /* Considered external until we know what classes are being
	 compiled into this object file.  */
      DECL_EXTERNAL (field) = 1;
    }

  return field;
}

/* Associate a constant value CONSTANT with VAR_DECL FIELD. */

void
set_constant_value (field, constant)
     tree field, constant;
{
  if (field == NULL_TREE)
    warning ("misplaced ConstantValue attribute (not in any field)");
  else if (DECL_INITIAL (field) != NULL_TREE)
    warning ("duplicate ConstantValue attribute for field '%s'",
	     IDENTIFIER_POINTER (DECL_NAME (field)));
  else
    {
      DECL_INITIAL (field) = constant;
      if (TREE_TYPE (constant) != TREE_TYPE (field)
	  && ! (TREE_TYPE (constant) == int_type_node
		&& INTEGRAL_TYPE_P (TREE_TYPE (field))
		&& TYPE_PRECISION (TREE_TYPE (field)) <= 32)
	  && ! (TREE_TYPE (constant) == utf8const_ptr_type
		&& TREE_TYPE (field) == string_ptr_type_node))
	error ("ConstantValue attribute of field '%s' has wrong type",
	       IDENTIFIER_POINTER (DECL_NAME (field)));
      if (FIELD_FINAL (field))
	DECL_FIELD_FINAL_IUD (field) = 1;
    }
}

/* Count the number of Unicode chars encoded in a given Ut8 string. */

#if 0
int
strLengthUtf8 (str, len)
     char *str;
     int len;
{
  register unsigned char* ptr = (unsigned char*) str;
  register unsigned char *limit = ptr + len;
  int str_length = 0;
  for (; ptr < limit; str_length++) {
    if (UTF8_GET (ptr, limit) < 0)
      return -1;
  }
  return str_length;
}
#endif


/* Calculate a hash value for a string encoded in Utf8 format.
 * This returns the same hash value as specified for java.lang.String.hashCode.
 */

static int32
hashUtf8String (str, len)
     const char *str;
     int len;
{
  register const unsigned char* ptr = (const unsigned char*) str;
  register const unsigned char *limit = ptr + len;
  int32 hash = 0;
  for (; ptr < limit;)
    {
      int ch = UTF8_GET (ptr, limit);
      /* Updated specification from
	 http://www.javasoft.com/docs/books/jls/clarify.html. */
      hash = (31 * hash) + ch;
    }
  return hash;
}

/* Generate a byte array representing the contents of FILENAME.  The
   array is assigned a unique local symbol.  The array represents a
   compiled Java resource, which is accessed by the runtime using
   NAME.  */
void
compile_resource_file (name, filename)
     char *name;
     const char *filename;
{
  struct stat stat_buf;
  int fd;
  char *buffer;
  char buf[60];
  tree rtype, field = NULL_TREE, data_type, rinit, data, decl;
  static int Jr_count = 0;

  fd = open (filename, O_RDONLY | O_BINARY);
  if (fd < 0)
    {
      perror ("Failed to read resource file");
      return;
    }
  if (fstat (fd, &stat_buf) != 0
      || ! S_ISREG (stat_buf.st_mode))
    {
      perror ("Could not figure length of resource file");
      return;
    }
  buffer = xmalloc (strlen (name) + stat_buf.st_size);
  strcpy (buffer, name);
  read (fd, buffer + strlen (name), stat_buf.st_size);
  close (fd);
  data_type = build_prim_array_type (unsigned_byte_type_node,
				     strlen (name) + stat_buf.st_size);
  rtype = make_node (RECORD_TYPE);
  PUSH_FIELD (rtype, field, "name_length", unsigned_int_type_node);
  PUSH_FIELD (rtype, field, "resource_length", unsigned_int_type_node);
  PUSH_FIELD (rtype, field, "data", data_type);
  FINISH_RECORD (rtype);
  START_RECORD_CONSTRUCTOR (rinit, rtype);
  PUSH_FIELD_VALUE (rinit, "name_length", 
		    build_int_2 (strlen (name), 0));
  PUSH_FIELD_VALUE (rinit, "resource_length", 
		    build_int_2 (stat_buf.st_size, 0));
  data = build_string (strlen(name) + stat_buf.st_size, buffer);
  TREE_TYPE (data) = data_type;
  PUSH_FIELD_VALUE (rinit, "data", data);
  FINISH_RECORD_CONSTRUCTOR (rinit);
  TREE_CONSTANT (rinit) = 1;

  /* Generate a unique-enough identifier.  */
  sprintf(buf, "_Jr%d", ++Jr_count);

  decl = build_decl (VAR_DECL, get_identifier (buf), rtype);
  TREE_STATIC (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  DECL_IGNORED_P (decl) = 1;
  TREE_READONLY (decl) = 1;
  TREE_THIS_VOLATILE (decl) = 0;
  DECL_INITIAL (decl) = rinit;
  layout_decl (decl, 0);
  pushdecl (decl);
  rest_of_decl_compilation (decl, (char*) 0, global_bindings_p (), 0);
  make_decl_rtl (decl, (char*) 0);
  assemble_variable (decl, 1, 0, 0);

  {
    tree init_name = get_file_function_name ('I');
    tree init_type = build_function_type (void_type_node, end_params_node);
    tree init_decl;
    
    init_decl = build_decl (FUNCTION_DECL, init_name, init_type);
    SET_DECL_ASSEMBLER_NAME (init_decl, init_name);
    TREE_STATIC (init_decl) = 1;
    current_function_decl = init_decl;
    DECL_RESULT (init_decl) = build_decl (RESULT_DECL, 
					  NULL_TREE, void_type_node);

    /* It can be a static function as long as collect2 does not have
       to scan the object file to find its ctor/dtor routine.  */
    TREE_PUBLIC (init_decl) = ! targetm.have_ctors_dtors;

    pushlevel (0);
    make_decl_rtl (init_decl, NULL);
    init_function_start (init_decl, input_filename, 0);
    expand_function_start (init_decl, 0);
    
    emit_library_call (registerResource_libfunc, 0, VOIDmode, 1,
		       gen_rtx (SYMBOL_REF, Pmode, buf), 
		       Pmode);
    
    expand_function_end (input_filename, 0, 0);
    poplevel (1, 0, 1);
    { 
      /* Force generation, even with -O3 or deeper. Gross hack. FIXME */
      int saved_flag = flag_inline_functions;
      flag_inline_functions = 0;	
      rest_of_compilation (init_decl);
      flag_inline_functions = saved_flag;
    }
    current_function_decl = NULL_TREE;
    (* targetm.asm_out.constructor) (XEXP (DECL_RTL (init_decl), 0),
				     DEFAULT_INIT_PRIORITY);
  }     
}

tree utf8_decl_list = NULL_TREE;

tree
build_utf8_ref (name)
     tree name;
{
  const char * name_ptr = IDENTIFIER_POINTER(name);
  int name_len = IDENTIFIER_LENGTH(name);
  char buf[60];
  tree ctype, field = NULL_TREE, str_type, cinit, string;
  static int utf8_count = 0;
  int name_hash;
  tree ref = IDENTIFIER_UTF8_REF (name);
  tree decl;
  if (ref != NULL_TREE)
    return ref;

  ctype = make_node (RECORD_TYPE);
  str_type = build_prim_array_type (unsigned_byte_type_node,
				    name_len + 1); /* Allow for final '\0'. */
  PUSH_FIELD (ctype, field, "hash", unsigned_short_type_node);
  PUSH_FIELD (ctype, field, "length", unsigned_short_type_node);
  PUSH_FIELD (ctype, field, "data", str_type);
  FINISH_RECORD (ctype);
  START_RECORD_CONSTRUCTOR (cinit, ctype);
  name_hash = hashUtf8String (name_ptr, name_len) & 0xFFFF;
  PUSH_FIELD_VALUE (cinit, "hash", build_int_2 (name_hash, 0));
  PUSH_FIELD_VALUE (cinit, "length", build_int_2 (name_len, 0));
  string = build_string (name_len, name_ptr);
  TREE_TYPE (string) = str_type;
  PUSH_FIELD_VALUE (cinit, "data", string);
  FINISH_RECORD_CONSTRUCTOR (cinit);
  TREE_CONSTANT (cinit) = 1;

  /* Generate a unique-enough identifier.  */
  sprintf(buf, "_Utf%d", ++utf8_count);

  decl = build_decl (VAR_DECL, get_identifier (buf), utf8const_type);
  TREE_STATIC (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  DECL_IGNORED_P (decl) = 1;
  TREE_READONLY (decl) = 1;
  TREE_THIS_VOLATILE (decl) = 0;
  DECL_INITIAL (decl) = cinit;
#ifdef HAVE_GAS_SHF_MERGE
  {
    int decl_size;
    /* Ensure decl_size is a multiple of utf8const_type's alignment. */
    decl_size = (name_len + 5 + TYPE_ALIGN_UNIT (utf8const_type) - 1)
	         & ~(TYPE_ALIGN_UNIT (utf8const_type) - 1);
    if (flag_merge_constants && decl_size < 256)
      {
        char buf[32];
        int flags = (SECTION_OVERRIDE
        	   | SECTION_MERGE | (SECTION_ENTSIZE & decl_size));
        sprintf (buf, ".rodata.jutf8.%d", decl_size);
        named_section_flags (buf, flags);
        DECL_SECTION_NAME (decl) = build_string (strlen (buf), buf);
      }
  }
#endif
  TREE_CHAIN (decl) = utf8_decl_list;
  layout_decl (decl, 0);
  pushdecl (decl);
  rest_of_decl_compilation (decl, (char*) 0, global_bindings_p (), 0);
  utf8_decl_list = decl;
  make_decl_rtl (decl, (char*) 0);
  ref = build1 (ADDR_EXPR, utf8const_ptr_type, decl);
  IDENTIFIER_UTF8_REF (name) = ref;
  return ref;
}

/* Build a reference to the class TYPE.
   Also handles primitive types and array types. */

tree
build_class_ref (type)
     tree type;
{
  int is_compiled = is_compiled_class (type);
  if (is_compiled)
    {
      tree ref, decl_name, decl;
      if (TREE_CODE (type) == POINTER_TYPE)
	type = TREE_TYPE (type);
      if (TREE_CODE (type) == RECORD_TYPE)
	{
	  if (TYPE_SIZE (type) == error_mark_node)
	    return null_pointer_node;
	  decl_name = identifier_subst (DECL_NAME (TYPE_NAME (type)),
					"", '/', '/', ".class");
	  decl = IDENTIFIER_GLOBAL_VALUE (decl_name);
	  if (decl == NULL_TREE)
	    {
	      decl = build_decl (VAR_DECL, decl_name, class_type_node);
	      DECL_SIZE (decl) = TYPE_SIZE (class_type_node);
	      DECL_SIZE_UNIT (decl) = TYPE_SIZE_UNIT (class_type_node);
	      TREE_STATIC (decl) = 1;
	      TREE_PUBLIC (decl) = 1;
	      DECL_IGNORED_P (decl) = 1;
	      DECL_ARTIFICIAL (decl) = 1;
	      if (is_compiled == 1)
		DECL_EXTERNAL (decl) = 1;
	      SET_DECL_ASSEMBLER_NAME (decl, 
				       java_mangle_class_field
				       (&temporary_obstack, type));
	      make_decl_rtl (decl, NULL);
	      pushdecl_top_level (decl);
	    }
	}
      else
	{
	  const char *name;
	  char buffer[25];
	  if (flag_emit_class_files)
	    {
	      const char *prim_class_name;
	      tree prim_class;
	      if (type == char_type_node)
		prim_class_name = "java.lang.Character";
	      else if (type == boolean_type_node)
		prim_class_name = "java.lang.Boolean";
	      else if (type == byte_type_node)
		prim_class_name = "java.lang.Byte";
	      else if (type == short_type_node)
		prim_class_name = "java.lang.Short";
	      else if (type == int_type_node)
		prim_class_name = "java.lang.Integer";
	      else if (type == long_type_node)
		prim_class_name = "java.lang.Long";
	      else if (type == float_type_node)
                prim_class_name = "java.lang.Float";
	      else if (type == double_type_node)
                prim_class_name = "java.lang.Double";
	      else if (type == void_type_node)
                prim_class_name = "java.lang.Void";
	      else
		abort ();

	      prim_class = lookup_class (get_identifier (prim_class_name));
	      return build (COMPONENT_REF, NULL_TREE,
			    prim_class, TYPE_identifier_node);
	    }
	  decl_name = TYPE_NAME (type);
	  if (TREE_CODE (decl_name) == TYPE_DECL)
	    decl_name = DECL_NAME (decl_name);
	  name = IDENTIFIER_POINTER (decl_name);
	  if (strncmp (name, "promoted_", 9) == 0)
	    name += 9;
	  sprintf (buffer, "_Jv_%sClass", name);
	  decl_name = get_identifier (buffer);
	  decl = IDENTIFIER_GLOBAL_VALUE (decl_name);
	  if (decl == NULL_TREE)
	    {
	      decl = build_decl (VAR_DECL, decl_name, class_type_node);
	      TREE_STATIC (decl) = 1;
	      TREE_PUBLIC (decl) = 1;
	      DECL_EXTERNAL (decl) = 1;
	      make_decl_rtl (decl, NULL);
	      pushdecl_top_level (decl);
	    }
	}

      ref = build1 (ADDR_EXPR, class_ptr_type, decl);
      return ref;
    }
  else
    {
      int index;
      tree cl;
      index = alloc_class_constant (type);
      cl = build_ref_from_constant_pool (index); 
      TREE_TYPE (cl) = promote_type (class_ptr_type);
      return cl;
    }
}

tree
build_static_field_ref (fdecl)
     tree fdecl;
{
  tree fclass = DECL_CONTEXT (fdecl);
  int is_compiled = is_compiled_class (fclass);
  if (is_compiled)
    {
      if (!DECL_RTL_SET_P (fdecl))
	{
	  if (is_compiled == 1)
	    DECL_EXTERNAL (fdecl) = 1;
	  make_decl_rtl (fdecl, NULL);
	}
      return fdecl;
    }
  else
    {
      /* Compile as:
       * *(FTYPE*)build_class_ref(FCLASS)->fields[INDEX].info.addr */
      tree ref = build_class_ref (fclass);
      tree fld;
      int field_index = 0;
      ref = build1 (INDIRECT_REF, class_type_node, ref);
      ref = build (COMPONENT_REF, field_ptr_type_node, ref,
		   lookup_field (&class_type_node, fields_ident));

      for (fld = TYPE_FIELDS (fclass); ; fld = TREE_CHAIN (fld))
	{
	  if (fld == fdecl)
	    break;
	  if (fld == NULL_TREE)
	    fatal_error ("field '%s' not found in class",
			 IDENTIFIER_POINTER (DECL_NAME (fdecl)));
	  if (FIELD_STATIC (fld))
	    field_index++;
	}
      field_index *= int_size_in_bytes (field_type_node);
      ref = fold (build (PLUS_EXPR, field_ptr_type_node,
			 ref, build_int_2 (field_index, 0)));
      ref = build1 (INDIRECT_REF, field_type_node, ref);
      ref = build (COMPONENT_REF, field_info_union_node,
		   ref, lookup_field (&field_type_node, info_ident));
      ref = build (COMPONENT_REF, ptr_type_node,
		   ref, TREE_CHAIN (TYPE_FIELDS (field_info_union_node)));
      return fold (build1 (INDIRECT_REF, TREE_TYPE(fdecl), ref));
    }
}

int
get_access_flags_from_decl (decl)
     tree decl;
{
  int access_flags = 0;
  if (TREE_CODE (decl) == FIELD_DECL || TREE_CODE (decl) == VAR_DECL)
    {
      if (FIELD_STATIC (decl))
	access_flags |= ACC_STATIC;
      if (FIELD_PUBLIC (decl))
	access_flags |= ACC_PUBLIC;
      if (FIELD_PROTECTED (decl))
	access_flags |= ACC_PROTECTED;
      if (FIELD_PRIVATE (decl))
	access_flags |= ACC_PRIVATE;
      if (FIELD_FINAL (decl))
	access_flags |= ACC_FINAL;
      if (FIELD_VOLATILE (decl))
	access_flags |= ACC_VOLATILE;
      if (FIELD_TRANSIENT (decl))
	access_flags |= ACC_TRANSIENT;
      return access_flags;
    }
  if (TREE_CODE (decl) == TYPE_DECL)
    {
      if (CLASS_PUBLIC (decl))
	access_flags |= ACC_PUBLIC;
      if (CLASS_FINAL (decl))
	access_flags |= ACC_FINAL;
      if (CLASS_SUPER (decl))
	access_flags |= ACC_SUPER;
      if (CLASS_INTERFACE (decl))
	access_flags |= ACC_INTERFACE;
      if (CLASS_ABSTRACT (decl))
	access_flags |= ACC_ABSTRACT;
      if (CLASS_STATIC (decl))
	access_flags |= ACC_STATIC;
      if (CLASS_PRIVATE (decl))
	access_flags |= ACC_PRIVATE;
      if (CLASS_PROTECTED (decl))
	access_flags |= ACC_PROTECTED;
      if (CLASS_STRICTFP (decl))
	access_flags |= ACC_STRICT;
      return access_flags;
    }
  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      if (METHOD_PUBLIC (decl))
	access_flags |= ACC_PUBLIC;
      if (METHOD_PRIVATE (decl))
	access_flags |= ACC_PRIVATE;
      if (METHOD_PROTECTED (decl))
	access_flags |= ACC_PROTECTED;
      if (METHOD_STATIC (decl))
	access_flags |= ACC_STATIC;
      if (METHOD_FINAL (decl))
	access_flags |= ACC_FINAL;
      if (METHOD_SYNCHRONIZED (decl))
	access_flags |= ACC_SYNCHRONIZED;
      if (METHOD_NATIVE (decl))
	access_flags |= ACC_NATIVE;
      if (METHOD_ABSTRACT (decl))
	access_flags |= ACC_ABSTRACT;
      if (METHOD_TRANSIENT (decl))
	access_flags |= ACC_TRANSIENT;
      if (METHOD_STRICTFP (decl))
	access_flags |= ACC_STRICT;
      return access_flags;
    }
  abort ();
}

static tree
make_field_value (fdecl)
  tree fdecl;
{
  tree finit;
  int flags;
  tree type = TREE_TYPE (fdecl);
  int resolved = is_compiled_class (type);

  START_RECORD_CONSTRUCTOR (finit, field_type_node);
  PUSH_FIELD_VALUE (finit, "name", build_utf8_ref (DECL_NAME (fdecl)));
  if (resolved)
    type = build_class_ref (type);
  else
    {
      tree signature = build_java_signature (type);

      type = build_utf8_ref (unmangle_classname 
			     (IDENTIFIER_POINTER (signature),
			      IDENTIFIER_LENGTH (signature)));
    }
  PUSH_FIELD_VALUE (finit, "type", type);

  flags = get_access_flags_from_decl (fdecl);
  if (! resolved)
    flags |= 0x8000 /* FIELD_UNRESOLVED_FLAG */;

  PUSH_FIELD_VALUE (finit, "accflags", build_int_2 (flags, 0));
  PUSH_FIELD_VALUE (finit, "bsize", TYPE_SIZE_UNIT (TREE_TYPE (fdecl)));

  PUSH_FIELD_VALUE
    (finit, "info",
     build (CONSTRUCTOR, field_info_union_node, NULL_TREE,
	    build_tree_list
	    ((FIELD_STATIC (fdecl)
	      ? TREE_CHAIN (TYPE_FIELDS (field_info_union_node))
	      : TYPE_FIELDS (field_info_union_node)),
	     (FIELD_STATIC (fdecl)
	      ? build_address_of (build_static_field_ref (fdecl))
	      : byte_position (fdecl)))));

  FINISH_RECORD_CONSTRUCTOR (finit);
  return finit;
}

static tree
make_method_value (mdecl)
     tree mdecl;
{
  static int method_name_count = 0;
  tree minit;
  tree index;
  tree code;
#define ACC_TRANSLATED          0x4000
  int accflags = get_access_flags_from_decl (mdecl) | ACC_TRANSLATED;

  if (!flag_indirect_dispatch && DECL_VINDEX (mdecl) != NULL_TREE)
    index = DECL_VINDEX (mdecl);
  else
    index = integer_minus_one_node;

  code = null_pointer_node;
  if (DECL_RTL_SET_P (mdecl))
    code = build1 (ADDR_EXPR, nativecode_ptr_type_node, mdecl);
  START_RECORD_CONSTRUCTOR (minit, method_type_node);
  PUSH_FIELD_VALUE (minit, "name",
		    build_utf8_ref (DECL_CONSTRUCTOR_P (mdecl) ?
				    init_identifier_node
				    : DECL_NAME (mdecl)));
  {
    tree signature = build_java_signature (TREE_TYPE (mdecl));
    PUSH_FIELD_VALUE (minit, "signature", 
		      (build_utf8_ref 
		       (unmangle_classname 
			(IDENTIFIER_POINTER(signature),
			 IDENTIFIER_LENGTH(signature)))));
  }
  PUSH_FIELD_VALUE (minit, "accflags", build_int_2 (accflags, 0));
  PUSH_FIELD_VALUE (minit, "index", index);
  PUSH_FIELD_VALUE (minit, "ncode", code);

  {
    /* Compute the `throws' information for the method.  */
    tree table = null_pointer_node;
    if (DECL_FUNCTION_THROWS (mdecl) != NULL_TREE)
      {
	int length = 1 + list_length (DECL_FUNCTION_THROWS (mdecl));
	tree iter, type, array;
	char buf[60];

	table = tree_cons (NULL_TREE, table, NULL_TREE);
	for (iter = DECL_FUNCTION_THROWS (mdecl);
	     iter != NULL_TREE;
	     iter = TREE_CHAIN (iter))
	  {
	    tree sig = DECL_NAME (TYPE_NAME (TREE_VALUE (iter)));
	    tree utf8
	      = build_utf8_ref (unmangle_classname (IDENTIFIER_POINTER (sig),
						    IDENTIFIER_LENGTH (sig)));
	    table = tree_cons (NULL_TREE, utf8, table);
	  }
	type = build_prim_array_type (ptr_type_node, length);
	table = build (CONSTRUCTOR, type, NULL_TREE, table);
	/* Compute something unique enough.  */
	sprintf (buf, "_methods%d", method_name_count++);
	array = build_decl (VAR_DECL, get_identifier (buf), type);
	DECL_INITIAL (array) = table;
	TREE_STATIC (array) = 1;
	DECL_ARTIFICIAL (array) = 1;
	DECL_IGNORED_P (array) = 1;
	rest_of_decl_compilation (array, (char*) 0, 1, 0);

	table = build1 (ADDR_EXPR, ptr_type_node, array);
      }

    PUSH_FIELD_VALUE (minit, "throws", table);
  }

  FINISH_RECORD_CONSTRUCTOR (minit);
  return minit;
}

static tree
get_dispatch_vector (type)
     tree type;
{
  tree vtable = TYPE_VTABLE (type);
  if (vtable == NULL)
    {
      HOST_WIDE_INT i;
      tree method;
      tree super = CLASSTYPE_SUPER (type);
      HOST_WIDE_INT nvirtuals = tree_low_cst (TYPE_NVIRTUALS (type), 0);
      vtable = make_tree_vec (nvirtuals);
      TYPE_VTABLE (type) = vtable;
      if (super != NULL_TREE)
	{
	  tree super_vtable = get_dispatch_vector (super);

	  for (i = tree_low_cst (TYPE_NVIRTUALS (super), 0); --i >= 0; )
	    TREE_VEC_ELT (vtable, i) = TREE_VEC_ELT (super_vtable, i);
	}

      for (method = TYPE_METHODS (type);  method != NULL_TREE;
	   method = TREE_CHAIN (method))
	if (DECL_VINDEX (method) != NULL_TREE
	    && host_integerp (DECL_VINDEX (method), 0))
	  TREE_VEC_ELT (vtable, tree_low_cst (DECL_VINDEX (method), 0))
	    = method;
    }

  return vtable;
}

static tree
get_dispatch_table (type, this_class_addr)
     tree type, this_class_addr;
{
  int abstract_p = CLASS_ABSTRACT (TYPE_NAME (type));
  tree vtable = get_dispatch_vector (type);
  int i, j;
  tree list = NULL_TREE;
  int nvirtuals = TREE_VEC_LENGTH (vtable);
  int arraysize;
  tree gc_descr;

  for (i = nvirtuals;  --i >= 0; )
    {
      tree method = TREE_VEC_ELT (vtable, i);
      if (METHOD_ABSTRACT (method))
	{
	  if (! abstract_p)
	    warning_with_decl (method,
			       "abstract method in non-abstract class");

	  if (TARGET_VTABLE_USES_DESCRIPTORS)
	    for (j = 0; j < TARGET_VTABLE_USES_DESCRIPTORS; ++j)
	      list = tree_cons (NULL_TREE, null_pointer_node, list);
	  else
	    list = tree_cons (NULL_TREE, null_pointer_node, list);
	}
      else
	{
	  if (!DECL_RTL_SET_P (method))
	    make_decl_rtl (method, NULL);

	  if (TARGET_VTABLE_USES_DESCRIPTORS)
	    for (j = 0; j < TARGET_VTABLE_USES_DESCRIPTORS; ++j)
	      {
		tree fdesc = build (FDESC_EXPR, nativecode_ptr_type_node, 
				    method, build_int_2 (j, 0));
		TREE_CONSTANT (fdesc) = 1;
	        list = tree_cons (NULL_TREE, fdesc, list);
	      }
	  else
	    list = tree_cons (NULL_TREE,
			      build1 (ADDR_EXPR, nativecode_ptr_type_node,
				      method),
			      list);
	}
    }

  /* Dummy entry for compatibility with G++ -fvtable-thunks.  When
     using the Boehm GC we sometimes stash a GC type descriptor
     there. We set the PURPOSE to NULL_TREE not to interfere (reset)
     the emitted byte count during the output to the assembly file. */
  /* With TARGET_VTABLE_USES_DESCRIPTORS, we only add one extra
     fake "function descriptor".  It's first word is the is the class
     pointer, and subsequent words (usually one) contain the GC descriptor.
     In all other cases, we reserve two extra vtable slots. */
  gc_descr =  get_boehm_type_descriptor (type);
  list = tree_cons (NULL_TREE, gc_descr, list);
  for (j = 1; j < TARGET_VTABLE_USES_DESCRIPTORS-1; ++j)
    list = tree_cons (NULL_TREE, gc_descr, list);
  list = tree_cons (NULL_TREE, this_class_addr, list);

  /** Pointer to type_info object (to be implemented), according to g++ ABI. */
  list = tree_cons (NULL_TREE, null_pointer_node, list);
  /** Offset to start of whole object.  Always (ptrdiff_t)0 for Java. */
  list = tree_cons (integer_zero_node, null_pointer_node, list);

  arraysize = (TARGET_VTABLE_USES_DESCRIPTORS? nvirtuals + 1 : nvirtuals + 2);
  if (TARGET_VTABLE_USES_DESCRIPTORS)
    arraysize *= TARGET_VTABLE_USES_DESCRIPTORS;
  arraysize += 2;
  return build (CONSTRUCTOR,
		build_prim_array_type (nativecode_ptr_type_node, arraysize),
		NULL_TREE, list);
}

void
make_class_data (type)
     tree type;
{
  tree decl, cons, temp;
  tree field, fields_decl;
  tree static_fields = NULL_TREE;
  tree instance_fields = NULL_TREE;
  HOST_WIDE_INT static_field_count = 0;
  HOST_WIDE_INT instance_field_count = 0;
  HOST_WIDE_INT field_count;
  tree field_array_type;
  tree method;
  tree methods = NULL_TREE;
  tree dtable_decl = NULL_TREE;
  HOST_WIDE_INT method_count = 0;
  tree method_array_type;
  tree methods_decl;
  tree super;
  tree this_class_addr;
  tree constant_pool_constructor;
  tree interfaces = null_pointer_node;
  int interface_len = 0;
  tree type_decl = TYPE_NAME (type);
  /** Offset from start of virtual function table declaration
      to where objects actually point at, following new g++ ABI. */
  tree dtable_start_offset = build_int_2 (2 * POINTER_SIZE / BITS_PER_UNIT, 0);

  this_class_addr = build_class_ref (type);
  decl = TREE_OPERAND (this_class_addr, 0);

  /* Build Field array. */
  field = TYPE_FIELDS (type);
  if (DECL_NAME (field) == NULL_TREE)
    field = TREE_CHAIN (field);  /* Skip dummy field for inherited data. */
  for ( ;  field != NULL_TREE;  field = TREE_CHAIN (field))
    {
      if (! DECL_ARTIFICIAL (field))
	{
	  tree init = make_field_value (field);
	  if (FIELD_STATIC (field))
	    {
	      tree initial = DECL_INITIAL (field);
	      static_field_count++;
	      static_fields = tree_cons (NULL_TREE, init, static_fields);
	      /* If the initial value is a string constant,
		 prevent output_constant from trying to assemble the value. */
	      if (initial != NULL_TREE
		  && TREE_TYPE (initial) == string_ptr_type_node)
		DECL_INITIAL (field) = NULL_TREE;
	      rest_of_decl_compilation (field, (char*) 0, 1, 1);
	      DECL_INITIAL (field) = initial;
	    }
	  else
	    {
	      instance_field_count++;
	      instance_fields = tree_cons (NULL_TREE, init, instance_fields);
	    }
	}
    }
  field_count = static_field_count + instance_field_count;
  if (field_count > 0)
    {
      static_fields = nreverse (static_fields);
      instance_fields = nreverse (instance_fields);
      static_fields = chainon (static_fields, instance_fields);
      field_array_type = build_prim_array_type (field_type_node, field_count);
      fields_decl = build_decl (VAR_DECL, mangled_classname ("_FL_", type),
				field_array_type);
      DECL_INITIAL (fields_decl) = build (CONSTRUCTOR, field_array_type,
					  NULL_TREE, static_fields);
      TREE_STATIC (fields_decl) = 1;
      DECL_ARTIFICIAL (fields_decl) = 1;
      DECL_IGNORED_P (fields_decl) = 1;
      rest_of_decl_compilation (fields_decl, (char*) 0, 1, 0);
    }
  else
    fields_decl = NULL_TREE;

  /* Build Method array. */
  for (method = TYPE_METHODS (type);
       method != NULL_TREE; method = TREE_CHAIN (method))
    {
      tree init;
      if (METHOD_PRIVATE (method)
	  && ! flag_keep_inline_functions
	  && (flag_inline_functions || optimize))
	continue;
      init = make_method_value (method);
      method_count++;
      methods = tree_cons (NULL_TREE, init, methods);
    }
  method_array_type = build_prim_array_type (method_type_node, method_count);
  methods_decl = build_decl (VAR_DECL, mangled_classname ("_MT_", type),
			     method_array_type);
  DECL_INITIAL (methods_decl) = build (CONSTRUCTOR, method_array_type,
				       NULL_TREE, nreverse (methods));
  TREE_STATIC (methods_decl) = 1;
  DECL_ARTIFICIAL (methods_decl) = 1;
  DECL_IGNORED_P (methods_decl) = 1;
  rest_of_decl_compilation (methods_decl, (char*) 0, 1, 0);

  if (assume_compiled (IDENTIFIER_POINTER (DECL_NAME (type_decl)))
      && ! CLASS_INTERFACE (type_decl) && !flag_indirect_dispatch)
    {
      tree dtable = get_dispatch_table (type, this_class_addr);
      dtable_decl = build_dtable_decl (type);
      DECL_INITIAL (dtable_decl) = dtable;
      TREE_STATIC (dtable_decl) = 1;
      DECL_ARTIFICIAL (dtable_decl) = 1;
      DECL_IGNORED_P (dtable_decl) = 1;
      TREE_PUBLIC (dtable_decl) = 1;
      rest_of_decl_compilation (dtable_decl, (char*) 0, 1, 0);
      if (type == class_type_node)
	class_dtable_decl = dtable_decl;
    }

  if (class_dtable_decl == NULL_TREE)
    {
      class_dtable_decl = build_dtable_decl (class_type_node);
      TREE_STATIC (class_dtable_decl) = 1;
      DECL_ARTIFICIAL (class_dtable_decl) = 1;
      DECL_IGNORED_P (class_dtable_decl) = 1;
      if (is_compiled_class (class_type_node) != 2)
	DECL_EXTERNAL (class_dtable_decl) = 1;
      rest_of_decl_compilation (class_dtable_decl, (char*) 0, 1, 0);
    }

  super = CLASSTYPE_SUPER (type);
  if (super == NULL_TREE)
    super = null_pointer_node;
  else if (assume_compiled (IDENTIFIER_POINTER (DECL_NAME (type_decl))))
    super = build_class_ref (super);
  else
    {
      int super_index = alloc_class_constant (super);
      super = build_int_2 (super_index, 0);
      TREE_TYPE (super) = ptr_type_node;
    }

  /* Build and emit the array of implemented interfaces. */
  if (type != object_type_node)
      interface_len = TREE_VEC_LENGTH (TYPE_BINFO_BASETYPES (type)) - 1;
  if (interface_len > 0)
    {
      tree init = NULL_TREE;
      int i;
      tree interface_array_type, idecl;
      interface_array_type
	= build_prim_array_type (class_ptr_type, interface_len);
      idecl = build_decl (VAR_DECL, mangled_classname ("_IF_", type),
			  interface_array_type);
      for (i = interface_len;  i > 0; i--)
	{
	  tree child = TREE_VEC_ELT (TYPE_BINFO_BASETYPES (type), i);
	  tree iclass = BINFO_TYPE (child);
	  tree index;
	  if (assume_compiled (IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (iclass)))))
	    index = build_class_ref (iclass);
	  else
	    {
		int int_index = alloc_class_constant (iclass);
		index = build_int_2 (int_index, 0);
		TREE_TYPE (index) = ptr_type_node;
	    }
	  init = tree_cons (NULL_TREE, index, init); 
	}
      DECL_INITIAL (idecl) = build (CONSTRUCTOR, interface_array_type,
				    NULL_TREE, init);
      TREE_STATIC (idecl) = 1;
      DECL_ARTIFICIAL (idecl) = 1;
      DECL_IGNORED_P (idecl) = 1;
      interfaces = build1 (ADDR_EXPR, ptr_type_node, idecl);
      rest_of_decl_compilation (idecl,  (char*) 0, 1, 0);
    }

  constant_pool_constructor = build_constants_constructor ();

  START_RECORD_CONSTRUCTOR (temp, object_type_node);
  PUSH_FIELD_VALUE (temp, "vtable",
		    build (PLUS_EXPR, dtable_ptr_type,
			   build1 (ADDR_EXPR, dtable_ptr_type, class_dtable_decl),
			   dtable_start_offset));
  if (! flag_hash_synchronization)
    PUSH_FIELD_VALUE (temp, "sync_info", null_pointer_node);
  FINISH_RECORD_CONSTRUCTOR (temp);
  START_RECORD_CONSTRUCTOR (cons, class_type_node);
  PUSH_SUPER_VALUE (cons, temp);
  PUSH_FIELD_VALUE (cons, "next", null_pointer_node);
  PUSH_FIELD_VALUE (cons, "name", build_utf8_ref (DECL_NAME (type_decl)));
  PUSH_FIELD_VALUE (cons, "accflags",
		    build_int_2 (get_access_flags_from_decl (type_decl), 0));

  PUSH_FIELD_VALUE (cons, "superclass", 
		    CLASS_INTERFACE (type_decl) ? null_pointer_node : super);
  PUSH_FIELD_VALUE (cons, "constants", constant_pool_constructor);
  PUSH_FIELD_VALUE (cons, "methods",
		    build1 (ADDR_EXPR, method_ptr_type_node, methods_decl));
  PUSH_FIELD_VALUE (cons, "method_count",  build_int_2 (method_count, 0));

  if (flag_indirect_dispatch)
    PUSH_FIELD_VALUE (cons, "vtable_method_count", integer_minus_one_node)
  else
    PUSH_FIELD_VALUE (cons, "vtable_method_count", TYPE_NVIRTUALS (type));
    
  PUSH_FIELD_VALUE (cons, "fields",
		    fields_decl == NULL_TREE ? null_pointer_node
		    : build1 (ADDR_EXPR, field_ptr_type_node, fields_decl));
  PUSH_FIELD_VALUE (cons, "size_in_bytes", size_in_bytes (type));
  PUSH_FIELD_VALUE (cons, "field_count", build_int_2 (field_count, 0));
  PUSH_FIELD_VALUE (cons, "static_field_count",
		    build_int_2 (static_field_count, 0));

  if (flag_indirect_dispatch)
    PUSH_FIELD_VALUE (cons, "vtable", null_pointer_node)
  else
    PUSH_FIELD_VALUE (cons, "vtable",
		      dtable_decl == NULL_TREE ? null_pointer_node
		      : build (PLUS_EXPR, dtable_ptr_type,
			       build1 (ADDR_EXPR, dtable_ptr_type, dtable_decl),
			       dtable_start_offset));
  
  if (otable_methods == NULL_TREE)
    {
      PUSH_FIELD_VALUE (cons, "otable", null_pointer_node);
      PUSH_FIELD_VALUE (cons, "otable_syms", null_pointer_node);
    }
  else
    {
      PUSH_FIELD_VALUE (cons, "otable",
			build1 (ADDR_EXPR, otable_ptr_type, otable_decl));
      PUSH_FIELD_VALUE (cons, "otable_syms",
			build1 (ADDR_EXPR, method_symbols_array_ptr_type,
				otable_syms_decl));
    }
  PUSH_FIELD_VALUE (cons, "interfaces", interfaces);
  PUSH_FIELD_VALUE (cons, "loader", null_pointer_node);
  PUSH_FIELD_VALUE (cons, "interface_count", build_int_2 (interface_len, 0));
  PUSH_FIELD_VALUE (cons, "state", integer_zero_node);

  PUSH_FIELD_VALUE (cons, "thread", null_pointer_node);
  PUSH_FIELD_VALUE (cons, "depth", integer_zero_node);
  PUSH_FIELD_VALUE (cons, "ancestors", null_pointer_node);
  PUSH_FIELD_VALUE (cons, "idt", null_pointer_node);
  PUSH_FIELD_VALUE (cons, "arrayclass", null_pointer_node);
  PUSH_FIELD_VALUE (cons, "protectionDomain", null_pointer_node);
  PUSH_FIELD_VALUE (cons, "chain", null_pointer_node);

  FINISH_RECORD_CONSTRUCTOR (cons);

  DECL_INITIAL (decl) = cons;
  
  /* Hash synchronization requires at least 64-bit alignment. */
  if (flag_hash_synchronization && POINTER_SIZE < 64)
    DECL_ALIGN (decl) = 64; 
  
  rest_of_decl_compilation (decl, (char*) 0, 1, 0);
}

void
finish_class ()
{
  tree method;
  tree type_methods = TYPE_METHODS (current_class);
  int saw_native_method = 0;

  /* Find out if we have any native methods.  We use this information
     later.  */
  for (method = type_methods;
       method != NULL_TREE;
       method = TREE_CHAIN (method))
    {
      if (METHOD_NATIVE (method))
	{
	  saw_native_method = 1;
	  break;
	}
    }

  /* Emit deferred inline methods. */  
  for (method = type_methods; method != NULL_TREE; )
    {
      if (! TREE_ASM_WRITTEN (method) && DECL_SAVED_INSNS (method) != 0)
	{
	  output_inline_function (method);
	  /* Scan the list again to see if there are any earlier
	     methods to emit. */
	  method = type_methods;
	  continue;
	}
      method = TREE_CHAIN (method);
    }

  current_function_decl = NULL_TREE;
  make_class_data (current_class);
  register_class ();
  rest_of_decl_compilation (TYPE_NAME (current_class), (char*) 0, 1, 0);
}

/* Return 2 if CLASS is compiled by this compilation job;
   return 1 if CLASS can otherwise be assumed to be compiled;
   return 0 if we cannot assume that CLASS is compiled.
   Returns 1 for primitive and 0 for array types.  */
int
is_compiled_class (class)
     tree class;
{
  int seen_in_zip;
  if (TREE_CODE (class) == POINTER_TYPE)
    class = TREE_TYPE (class);
  if (TREE_CODE (class) != RECORD_TYPE)  /* Primitive types are static. */
    return 1;
  if (TYPE_ARRAY_P (class))
    return 0;
  if (class == current_class)
    return 2;

  seen_in_zip = (TYPE_JCF (class) && JCF_SEEN_IN_ZIP (TYPE_JCF (class)));
  if (CLASS_FROM_CURRENTLY_COMPILED_P (class) || seen_in_zip)
    {
      /* The class was seen in the current ZIP file and will be
	 available as a compiled class in the future but may not have
	 been loaded already. Load it if necessary. This prevent
	 build_class_ref () from crashing. */

      if (seen_in_zip && !CLASS_LOADED_P (class))
        load_class (class, 1);

      /* We return 2 for class seen in ZIP and class from files
         belonging to the same compilation unit */
      return 2;
    }

  if (assume_compiled (IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (class)))))
    {
      if (!CLASS_LOADED_P (class))
	{
	  if (CLASS_FROM_SOURCE_P (class))
	    safe_layout_class (class);
	  else
	    load_class (class, 1);
	}
      return 1;
    }

  return 0;
}

/* Build a VAR_DECL for the dispatch table (vtable) for class TYPE. */

tree
build_dtable_decl (type)
     tree type;
{
  tree dtype;

  /* We need to build a new dtable type so that its size is uniquely
     computed when we're dealing with the class for real and not just
     faking it (like java.lang.Class during the initialization of the
     compiler.) We know we're not faking a class when CURRENT_CLASS is
     TYPE. */
  if (current_class == type)
    {
      tree dummy = NULL_TREE;
      int n;

      dtype = make_node (RECORD_TYPE);

      PUSH_FIELD (dtype, dummy, "top_offset", ptr_type_node);
      PUSH_FIELD (dtype, dummy, "type_info", ptr_type_node);

      PUSH_FIELD (dtype, dummy, "class", class_ptr_type);
      for (n = 1; n < TARGET_VTABLE_USES_DESCRIPTORS; ++n)
	{
	  tree tmp_field = build_decl (FIELD_DECL, NULL_TREE, ptr_type_node);
	  TREE_CHAIN (dummy) = tmp_field;
	  DECL_CONTEXT (tmp_field) = dtype;
	  DECL_ARTIFICIAL (tmp_field) = 1;
	  dummy = tmp_field;
	}

      PUSH_FIELD (dtype, dummy, "gc_descr", ptr_type_node);
      for (n = 1; n < TARGET_VTABLE_USES_DESCRIPTORS; ++n)
	{
	  tree tmp_field = build_decl (FIELD_DECL, NULL_TREE, ptr_type_node);
	  TREE_CHAIN (dummy) = tmp_field;
	  DECL_CONTEXT (tmp_field) = dtype;
	  DECL_ARTIFICIAL (tmp_field) = 1;
	  dummy = tmp_field;
	}

      n = TREE_VEC_LENGTH (get_dispatch_vector (type));
      if (TARGET_VTABLE_USES_DESCRIPTORS)
	n *= TARGET_VTABLE_USES_DESCRIPTORS;

      PUSH_FIELD (dtype, dummy, "methods",
		  build_prim_array_type (nativecode_ptr_type_node, n));
      layout_type (dtype);
    }
  else
    dtype = dtable_type;

  return build_decl (VAR_DECL, 
		     java_mangle_vtable (&temporary_obstack, type), dtype);
}

/* Pre-pend the TYPE_FIELDS of THIS_CLASS with a dummy FIELD_DECL for the
   fields inherited from SUPER_CLASS. */

void
push_super_field (this_class, super_class)
     tree this_class, super_class;
{
  tree base_decl;
  /* Don't insert the field if we're just re-laying the class out. */ 
  if (TYPE_FIELDS (this_class) && !DECL_NAME (TYPE_FIELDS (this_class)))
    return;
  base_decl = build_decl (FIELD_DECL, NULL_TREE, super_class);
  DECL_IGNORED_P (base_decl) = 1;
  TREE_CHAIN (base_decl) = TYPE_FIELDS (this_class);
  TYPE_FIELDS (this_class) = base_decl;
  DECL_SIZE (base_decl) = TYPE_SIZE (super_class);
  DECL_SIZE_UNIT (base_decl) = TYPE_SIZE_UNIT (super_class);
}

/* Handle the different manners we may have to lay out a super class.  */

static tree
maybe_layout_super_class (super_class, this_class)
     tree super_class;
     tree this_class;
{
  if (TREE_CODE (super_class) == RECORD_TYPE)
    {
      if (!CLASS_LOADED_P (super_class) && CLASS_FROM_SOURCE_P (super_class))
	safe_layout_class (super_class);
      if (!CLASS_LOADED_P (super_class))
	load_class (super_class, 1);
    }
  /* We might have to layout the class before its dependency on
     the super class gets resolved by java_complete_class  */
  else if (TREE_CODE (super_class) == POINTER_TYPE)
    {
      if (TREE_TYPE (super_class) != NULL_TREE)
	super_class = TREE_TYPE (super_class);
      else
	{
	  /* do_resolve_class expects an EXPR_WITH_FILE_LOCATION, so
	     we give it one.  */
	  tree this_wrap = NULL_TREE;

	  if (this_class)
	    {
	      tree this_decl = TYPE_NAME (this_class);
	      this_wrap = build_expr_wfl (this_class,
					  DECL_SOURCE_FILE (this_decl),
					  DECL_SOURCE_LINE (this_decl), 0);
	    }
	  super_class = do_resolve_class (NULL_TREE, /* FIXME? */
					  super_class, NULL_TREE, this_wrap);
	  if (!super_class)
	    return NULL_TREE;	/* FIXME, NULL_TREE not checked by caller. */
	  super_class = TREE_TYPE (super_class);
	}
    }
  if (!TYPE_SIZE (super_class))
    safe_layout_class (super_class);

  return super_class;
}

void
layout_class (this_class)
     tree this_class;
{
  tree super_class = CLASSTYPE_SUPER (this_class);
  tree field;
  
  class_list = tree_cons (this_class, NULL_TREE, class_list);
  if (CLASS_BEING_LAIDOUT (this_class))
    {
      char buffer [1024];
      char *report;
      tree current;
      
      sprintf (buffer, " with `%s'",
	       IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (this_class))));
      obstack_grow (&temporary_obstack, buffer, strlen (buffer));

      for (current = TREE_CHAIN (class_list); current; 
	   current = TREE_CHAIN (current))
	{
	  tree decl = TYPE_NAME (TREE_PURPOSE (current));
	  sprintf (buffer, "\n  which inherits from `%s' (%s:%d)",
		   IDENTIFIER_POINTER (DECL_NAME (decl)),
		   DECL_SOURCE_FILE (decl),
		   DECL_SOURCE_LINE (decl));
	  obstack_grow (&temporary_obstack, buffer, strlen (buffer));
	}
      obstack_1grow (&temporary_obstack, '\0');
      report = obstack_finish (&temporary_obstack);
      cyclic_inheritance_report = ggc_strdup (report);
      obstack_free (&temporary_obstack, report);
      TYPE_SIZE (this_class) = error_mark_node;
      return;
    }
  CLASS_BEING_LAIDOUT (this_class) = 1;

  if (super_class && !CLASS_BEING_LAIDOUT (super_class))
    {
      tree maybe_super_class 
	= maybe_layout_super_class (super_class, this_class);
      if (maybe_super_class == NULL
	  || TREE_CODE (TYPE_SIZE (maybe_super_class)) == ERROR_MARK)
	{
	  TYPE_SIZE (this_class) = error_mark_node;
	  CLASS_BEING_LAIDOUT (this_class) = 0;
	  class_list = TREE_CHAIN (class_list);
	  return;
	}
      if (TYPE_SIZE (this_class) == NULL_TREE)
	push_super_field (this_class, maybe_super_class);
    }

  for (field = TYPE_FIELDS (this_class);
       field != NULL_TREE;  field = TREE_CHAIN (field))
    {
      if (FIELD_STATIC (field))
	{
	  /* Set DECL_ASSEMBLER_NAME to something suitably mangled. */
	  SET_DECL_ASSEMBLER_NAME (field,
				   java_mangle_decl
				   (&temporary_obstack, field));
	}
    }

  layout_type (this_class);

  /* Also recursively load/layout any superinterfaces, but only if class was
  loaded from bytecode. The source parser will take care of this itself. */
  if (!CLASS_FROM_SOURCE_P (this_class))
    {
      tree basetype_vec = TYPE_BINFO_BASETYPES (this_class);

      if (basetype_vec)
	{
	  int n = TREE_VEC_LENGTH (basetype_vec) - 1;
	  int i;
	  for (i = n; i > 0; i--)
	    {
	      tree vec_elt = TREE_VEC_ELT (basetype_vec, i);
	      tree super_interface = BINFO_TYPE (vec_elt);

	      tree maybe_super_interface 
		= maybe_layout_super_class (super_interface, NULL_TREE);
	      if (maybe_super_interface == NULL
		  || TREE_CODE (TYPE_SIZE (maybe_super_interface)) == ERROR_MARK)
		{
		  TYPE_SIZE (this_class) = error_mark_node;
		  CLASS_BEING_LAIDOUT (this_class) = 0;
		  class_list = TREE_CHAIN (class_list);
		  return;
		}
	    }
	}
    }

  /* Convert the size back to an SI integer value */
  TYPE_SIZE_UNIT (this_class) = 
    fold (convert (int_type_node, TYPE_SIZE_UNIT (this_class)));

  CLASS_BEING_LAIDOUT (this_class) = 0;
  class_list = TREE_CHAIN (class_list);
}

void
layout_class_methods (this_class)
     tree this_class;
{
  tree method_decl, dtable_count;
  tree super_class;

  if (TYPE_NVIRTUALS (this_class))
    return;

  super_class = CLASSTYPE_SUPER (this_class);

  if (super_class)
    {
      super_class = maybe_layout_super_class (super_class, this_class);
      if (!TYPE_NVIRTUALS (super_class))
	layout_class_methods (super_class);
      dtable_count = TYPE_NVIRTUALS (super_class);
    }
  else
    dtable_count = integer_zero_node;

  TYPE_METHODS (this_class) = nreverse (TYPE_METHODS (this_class));

  for (method_decl = TYPE_METHODS (this_class);
       method_decl; method_decl = TREE_CHAIN (method_decl))
    dtable_count = layout_class_method (this_class, super_class, 
					method_decl, dtable_count);

  TYPE_NVIRTUALS (this_class) = dtable_count;
}

/* Return 0 if NAME is equal to STR, -1 if STR is "less" than NAME,
   and 1 if STR is "greater" than NAME.  */

/* Lay METHOD_DECL out, returning a possibly new value of
   DTABLE_COUNT. Also mangle the method's name. */

tree
layout_class_method (this_class, super_class, method_decl, dtable_count)
     tree this_class, super_class, method_decl, dtable_count;
{
  tree method_name = DECL_NAME (method_decl);

  TREE_PUBLIC (method_decl) = 1;

  /* This is a good occasion to mangle the method's name */
  SET_DECL_ASSEMBLER_NAME (method_decl,
			   java_mangle_decl (&temporary_obstack, 
					     method_decl));
  /* We don't generate a RTL for the method if it's abstract, or if
     it's an interface method that isn't clinit. */
  if (! METHOD_ABSTRACT (method_decl) 
      || (CLASS_INTERFACE (TYPE_NAME (this_class)) 
	  && (DECL_CLINIT_P (method_decl))))
    make_decl_rtl (method_decl, NULL);

  if (ID_INIT_P (method_name))
    {
      const char *p = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (this_class)));
      const char *ptr;
      for (ptr = p; *ptr; )
	{
	  if (*ptr++ == '.')
	    p = ptr;
	}
      DECL_CONSTRUCTOR_P (method_decl) = 1;
      build_java_argument_signature (TREE_TYPE (method_decl));
    }
  else if (! METHOD_STATIC (method_decl) && !DECL_ARTIFICIAL (method_decl))
    {
      tree method_sig = 
	build_java_argument_signature (TREE_TYPE (method_decl));
      tree super_method = lookup_argument_method (super_class, method_name,
						  method_sig);
      if (super_method != NULL_TREE && ! METHOD_PRIVATE (super_method))
	{
	  DECL_VINDEX (method_decl) = DECL_VINDEX (super_method);
	  if (DECL_VINDEX (method_decl) == NULL_TREE 
	      && !CLASS_FROM_SOURCE_P (this_class))
	    error_with_decl (method_decl,
			     "non-static method '%s' overrides static method");
	}
      else if (! METHOD_FINAL (method_decl)
	       && ! METHOD_PRIVATE (method_decl)
	       && ! CLASS_FINAL (TYPE_NAME (this_class))
	       && dtable_count)
	{
	  DECL_VINDEX (method_decl) = dtable_count;
	  dtable_count = fold (build (PLUS_EXPR, integer_type_node,
				      dtable_count, integer_one_node));
	}
    }

  return dtable_count;
}

void
register_class ()
{
  /* END does not need to be registered with the garbage collector
     because it always points into the list given by REGISTERED_CLASS,
     and that variable is registered with the collector.  */
  static tree end;
  tree node    = TREE_OPERAND (build_class_ref (current_class), 0);
  tree current = copy_node (node);

  XEXP (DECL_RTL (current), 0) = copy_rtx (XEXP (DECL_RTL(node), 0));
  if (!registered_class)
    registered_class = current;
  else
    TREE_CHAIN (end) = current;

  end = current;
}

/* Emit something to register classes at start-up time.

   The preferred mechanism is through the .jcr section, which contain
   a list of pointers to classes which get registered during
   constructor invoction time.  The fallback mechanism is to generate
   a `constructor' function which calls _Jv_RegisterClass for each
   class in this file.  */

void
emit_register_classes ()
{
  /* ??? This isn't quite the correct test.  We also have to know
     that the target is using gcc's crtbegin/crtend objects rather
     than the ones that come with the operating system.  */
  if (SUPPORTS_WEAK && targetm.have_named_sections)
    {
#ifdef JCR_SECTION_NAME
      tree t;
      named_section_flags (JCR_SECTION_NAME, SECTION_WRITE);
      assemble_align (POINTER_SIZE);
      for (t = registered_class; t; t = TREE_CHAIN (t))
	assemble_integer (XEXP (DECL_RTL (t), 0),
			  POINTER_SIZE / BITS_PER_UNIT, POINTER_SIZE, 1);
#else
      abort ();
#endif
    }
  else
    {
      extern tree get_file_function_name PARAMS ((int));
      tree init_name = get_file_function_name ('I');
      tree init_type = build_function_type (void_type_node, end_params_node);
      tree init_decl;
      tree t;
      
      init_decl = build_decl (FUNCTION_DECL, init_name, init_type);
      SET_DECL_ASSEMBLER_NAME (init_decl, init_name);
      TREE_STATIC (init_decl) = 1;
      current_function_decl = init_decl;
      DECL_RESULT (init_decl) = build_decl (RESULT_DECL, NULL_TREE,
					    void_type_node);

      /* It can be a static function as long as collect2 does not have
         to scan the object file to find its ctor/dtor routine.  */
      TREE_PUBLIC (init_decl) = ! targetm.have_ctors_dtors;

      /* Suppress spurious warnings.  */
      TREE_USED (init_decl) = 1;

      pushlevel (0);
      make_decl_rtl (init_decl, NULL);
      init_function_start (init_decl, input_filename, 0);
      expand_function_start (init_decl, 0);

      /* Do not allow the function to be deferred.  */
      current_function_cannot_inline
	= "static constructors and destructors cannot be inlined";

      for ( t = registered_class; t; t = TREE_CHAIN (t))
	emit_library_call (registerClass_libfunc, 0, VOIDmode, 1,
			   XEXP (DECL_RTL (t), 0), Pmode);
      
      expand_function_end (input_filename, 0, 0);
      poplevel (1, 0, 1);
      rest_of_compilation (init_decl);
      current_function_decl = NULL_TREE;

      if (targetm.have_ctors_dtors)
	(* targetm.asm_out.constructor) (XEXP (DECL_RTL (init_decl), 0),
					 DEFAULT_INIT_PRIORITY);
    }
}

/* Make a method_symbol_type (_Jv_MethodSymbol) node for METHOD. */

tree
build_method_symbols_entry (tree method)
{
  tree clname, name, signature, method_symbol;
  
  clname = build_utf8_ref (DECL_NAME (TYPE_NAME (DECL_CONTEXT (method))));
  name = build_utf8_ref (DECL_NAME (method));
  signature = build_java_signature (TREE_TYPE (method));
  signature = build_utf8_ref (unmangle_classname 
			      (IDENTIFIER_POINTER (signature),
			       IDENTIFIER_LENGTH (signature)));

  START_RECORD_CONSTRUCTOR (method_symbol, method_symbol_type);
  PUSH_FIELD_VALUE (method_symbol, "clname", clname);
  PUSH_FIELD_VALUE (method_symbol, "name", name);
  PUSH_FIELD_VALUE (method_symbol, "signature", signature);
  FINISH_RECORD_CONSTRUCTOR (method_symbol);
  TREE_CONSTANT (method_symbol) = 1;

  return method_symbol;
} 

/* Emit the offset symbols table for indirect virtual dispatch. */

void
emit_offset_symbol_table ()
{
  tree method_list, method, table, list, null_symbol;
  tree otable_bound, otable_array_type;
  int index;
  
  /* Only emit an offset table if this translation unit actually made virtual 
     calls. */
  if (otable_methods == NULL_TREE)
    return;

  /* Build a list of _Jv_MethodSymbols for each entry in otable_methods. */
  index = 0;
  method_list = otable_methods;
  list = NULL_TREE;  
  while (method_list != NULL_TREE)
    {
      method = TREE_VALUE (method_list);
      list = tree_cons (NULL_TREE, build_method_symbols_entry (method), list);
      method_list = TREE_CHAIN (method_list);
      index++;
    }

  /* Terminate the list with a "null" entry. */
  START_RECORD_CONSTRUCTOR (null_symbol, method_symbol_type);
  PUSH_FIELD_VALUE (null_symbol, "clname", null_pointer_node);
  PUSH_FIELD_VALUE (null_symbol, "name", null_pointer_node);
  PUSH_FIELD_VALUE (null_symbol, "signature", null_pointer_node);
  FINISH_RECORD_CONSTRUCTOR (null_symbol);
  TREE_CONSTANT (null_symbol) = 1;  
  list = tree_cons (NULL_TREE, null_symbol, list);

  /* Put the list in the right order and make it a constructor. */
  list = nreverse (list);
  table = build (CONSTRUCTOR, method_symbols_array_type, NULL_TREE, list);  

  /* Make it the initial value for otable_syms and emit the decl. */
  DECL_INITIAL (otable_syms_decl) = table;
  DECL_ARTIFICIAL (otable_syms_decl) = 1;
  DECL_IGNORED_P (otable_syms_decl) = 1;
  rest_of_decl_compilation (otable_syms_decl, NULL, 1, 0);
  
  /* Now that its size is known, redefine otable as an uninitialized static 
     array of INDEX + 1 integers. The extra entry is used by the runtime 
     to track whether the otable has been initialized. */
  otable_bound = build_index_type (build_int_2 (index, 0));
  otable_array_type = build_array_type (integer_type_node, otable_bound);
  otable_decl = build_decl (VAR_DECL, get_identifier ("otable"), 
			    otable_array_type);
  TREE_STATIC (otable_decl) = 1;
  TREE_READONLY (otable_decl) = 1;  
  rest_of_decl_compilation (otable_decl, NULL, 1, 0);
}

void
init_class_processing ()
{
  registerClass_libfunc = gen_rtx_SYMBOL_REF (Pmode, "_Jv_RegisterClass");
  registerResource_libfunc = 
    gen_rtx_SYMBOL_REF (Pmode, "_Jv_RegisterResource");
  fields_ident = get_identifier ("fields");
  info_ident = get_identifier ("info");
  gcc_obstack_init (&temporary_obstack);
}

static hashval_t java_treetreehash_hash PARAMS ((const void *));
static int java_treetreehash_compare PARAMS ((const void *, const void *));

/* A hash table mapping trees to trees.  Used generally.  */

#define JAVA_TREEHASHHASH_H(t) (htab_hash_pointer (t))

static hashval_t
java_treetreehash_hash (k_p)
     const void *k_p;
{
  struct treetreehash_entry *k = (struct treetreehash_entry *) k_p;
  return JAVA_TREEHASHHASH_H (k->key);
}

static int
java_treetreehash_compare (k1_p, k2_p)
     const void * k1_p;
     const void * k2_p;
{
  struct treetreehash_entry * k1 = (struct treetreehash_entry *) k1_p;
  tree k2 = (tree) k2_p;
  return (k1->key == k2);
}

tree 
java_treetreehash_find (ht, t)
     htab_t ht;
     tree t;
{
  struct treetreehash_entry *e;
  hashval_t hv = JAVA_TREEHASHHASH_H (t);
  e = (struct treetreehash_entry *) htab_find_with_hash (ht, t, hv);
  if (e == NULL)
    return NULL;
  else
    return e->value;
}

tree *
java_treetreehash_new (ht, t)
     htab_t ht;
     tree t;
{
  PTR *e;
  struct treetreehash_entry *tthe;
  hashval_t hv = JAVA_TREEHASHHASH_H (t);

  e = htab_find_slot_with_hash (ht, t, hv, INSERT);
  if (*e == NULL)
    {
      tthe = (*ht->alloc_f) (1, sizeof (*tthe));
      tthe->key = t;
      *e = (PTR) tthe;
    }
  else
    tthe = (struct treetreehash_entry *) *e;
  return &tthe->value;
}

htab_t
java_treetreehash_create (size, gc)
     size_t size;
     int gc;
{
  if (gc)
    return htab_create_ggc (size, java_treetreehash_hash,
			    java_treetreehash_compare, NULL);
  else
    return htab_create_alloc (size, java_treetreehash_hash,
			      java_treetreehash_compare, free, xcalloc, free);
}

#include "gt-java-class.h"
