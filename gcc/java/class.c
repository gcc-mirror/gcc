/* Functions related to building classes and their related objects.
   Copyright (C) 1996, 1997, 1998, 1999, 2000 Free Software Foundation, Inc.

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

static tree mangle_class_field PARAMS ((tree class));
static tree make_method_value PARAMS ((tree));
static tree build_java_method_type PARAMS ((tree, tree, int));
static int32 hashUtf8String PARAMS ((const char *, int));
static tree make_field_value PARAMS ((tree));
static tree get_dispatch_vector PARAMS ((tree));
static tree get_dispatch_table PARAMS ((tree, tree));
static void append_gpp_mangled_type PARAMS ((struct obstack *, tree));
static tree mangle_static_field PARAMS ((tree));
static void add_interface_do PARAMS ((tree, tree, int));
static tree maybe_layout_super_class PARAMS ((tree, tree));
static int assume_compiled PARAMS ((const char *));
static struct hash_entry *init_test_hash_newfunc PARAMS ((struct hash_entry *,
							  struct hash_table *,
							  hash_table_key));

static rtx registerClass_libfunc;

extern struct obstack permanent_obstack;
extern struct obstack temporary_obstack;

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

  /* Non-zero if this represents an exclusion.  */
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
   if EXCLUDEP is non-zero.  */

void
add_assume_compiled (ident, excludep)
     const char *ident;
     int excludep;
{
  assume_compiled_node *parent;
  assume_compiled_node *node = 
    (assume_compiled_node *) xmalloc (sizeof (assume_compiled_node));

  node->ident = xstrdup (ident);
  node->excludep = excludep;
  node->child = NULL;

  /* Create the root of the tree if it doesn't exist yet.  */

  if (NULL == assume_compiled_tree)
    {
      assume_compiled_tree = 
	(assume_compiled_node *) xmalloc (sizeof (assume_compiled_node));
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

/* Returns non-zero if IDENT is the name of a class that the compiler
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
  char *buffer = (char *)alloca  (i);
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
  push_obstacks (&permanent_obstack, &permanent_obstack);
  type = make_node (RECORD_TYPE);
#ifdef JAVA_USE_HANDLES
  tree field1 = build_decl (FIELD_DECL, get_identifier ("obj"),
			    build_pointer_type (type));
  tree field2 = build_decl (FIELD_DECL, get_identifier ("methods"),
			    methodtable_ptr_type);
  tree handle_type = make_node (RECORD_TYPE);
  TREE_CHAIN (field1) = field2;
  TYPE_FIELDS (handle_type) = field1;
  TYPE_BINFO (type) = make_tree_vec (7);
  TYPE_BINFO (handle_type) = make_tree_vec (7);
  BINFO_HANDLE (TYPE_BINFO (handle_type)) = type;
  BINFO_HANDLE (TYPE_BINFO (type)) = handle_type;
#else
  TYPE_BINFO (type) = make_tree_vec (6);
#endif
  MAYBE_CREATE_TYPE_TYPE_LANG_SPECIFIC (type);
  pop_obstacks ();

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
  char *save_input_filename = input_filename;
  int save_lineno = lineno;
  tree source_name = identifier_subst (class_name, "", '.', '/', ".java");
  push_obstacks (&permanent_obstack, &permanent_obstack);
  CLASS_P (class_type) = 1;
  input_filename = IDENTIFIER_POINTER (source_name);
  lineno = 0;
  decl = build_decl (TYPE_DECL, class_name, class_type);
  input_filename = save_input_filename;
  lineno = save_lineno;
  signature = identifier_subst (class_name, "L", '.', '/', ";");
  IDENTIFIER_SIGNATURE_TYPE (signature) = build_pointer_type (class_type);

  /* Setting DECL_ARTIFICAL forces dbxout.c to specific the type is
     both a typedef and in the struct name-space.  We may want to re-visit
     this later, but for now it reduces the changes needed for gdb. */
  DECL_ARTIFICIAL (decl) = 1;

  pushdecl_top_level (decl);
#ifdef JAVA_USE_HANDLES
  {
    tree handle_name = identifier_subst (class_name,
					 "Handle$", '.', '.', "");
    tree handle_decl = build_decl (TYPE_DECL, handle_name,
				   CLASS_TO_HANDLE_TYPE (class_type));
    pushdecl (handle_decl);
  }
#endif

  pop_obstacks ();
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

  push_obstacks (&permanent_obstack, &permanent_obstack);
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
  pop_obstacks ();

  if (access_flags & ACC_PUBLIC)    CLASS_PUBLIC (class_decl) = 1;
  if (access_flags & ACC_FINAL)     CLASS_FINAL (class_decl) = 1;
  if (access_flags & ACC_SUPER)     CLASS_SUPER (class_decl) = 1;
  if (access_flags & ACC_INTERFACE) CLASS_INTERFACE (class_decl) = 1;
  if (access_flags & ACC_ABSTRACT)  CLASS_ABSTRACT (class_decl) = 1;
  if (access_flags & ACC_STATIC)    CLASS_STATIC (class_decl) = 1;
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

static void
add_interface_do (basetype_vec, interface_class, i)
     tree basetype_vec, interface_class;
     int i;
{
  tree interface_binfo = make_tree_vec (6);
  BINFO_TYPE (interface_binfo) = interface_class;
  BINFO_OFFSET (interface_binfo) = integer_zero_node;
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
  return build_method_type (CLASS_TO_HANDLE_TYPE (this_class), fntype);
}

static struct hash_entry *
init_test_hash_newfunc (entry, table, string)
     struct hash_entry *entry;
     struct hash_table *table;
     hash_table_key string ATTRIBUTE_UNUSED;
{
  struct init_test_hash_entry *ret = (struct init_test_hash_entry *) entry;
  if (ret == NULL)
    {
      ret = ((struct init_test_hash_entry *)
	     hash_allocate (table, sizeof (struct init_test_hash_entry)));
      if (ret == NULL)
	return NULL;
    }
  ret->init_test_decl = 0;
  return (struct hash_entry *) ret;
}

/* Hash table helpers. Also reused in find_applicable_accessible_methods_list
   (parse.y). The hash of a tree node is it's pointer value,
   comparison is direct. */

unsigned long
java_hash_hash_tree_node (k)
     hash_table_key k;
{
  return (long) k;
}

boolean
java_hash_compare_tree_node (k1, k2)
     hash_table_key k1;
     hash_table_key k2;
{
  return ((char*) k1 == (char*) k2);
}

tree
add_method_1 (handle_class, access_flags, name, function_type)
     tree handle_class;
     int access_flags;
     tree name;
     tree function_type;
{
  tree method_type, fndecl;
  push_obstacks (&permanent_obstack, &permanent_obstack);

  method_type = build_java_method_type (function_type,
					handle_class, access_flags);

  fndecl = build_decl (FUNCTION_DECL, name, method_type);
  DECL_CONTEXT (fndecl) = handle_class;

  DECL_LANG_SPECIFIC (fndecl)
    = (struct lang_decl *) permalloc (sizeof (struct lang_decl));
  bzero ((PTR) DECL_LANG_SPECIFIC (fndecl), sizeof (struct lang_decl));

  /* Initialize the static initializer test table.  */
  hash_table_init (&DECL_FUNCTION_INIT_TEST_TABLE (fndecl),
		   init_test_hash_newfunc, java_hash_hash_tree_node, 
		   java_hash_compare_tree_node);

  TREE_CHAIN (fndecl) = TYPE_METHODS (handle_class);
  TYPE_METHODS (handle_class) = fndecl;
  pop_obstacks ();

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
  tree handle_class = CLASS_TO_HANDLE_TYPE (this_class);
  tree function_type, fndecl;
  const unsigned char *sig = (const unsigned char*)IDENTIFIER_POINTER (method_sig);
  push_obstacks (&permanent_obstack, &permanent_obstack);
  if (sig[0] != '(')
    fatal ("bad method signature");
  function_type = get_type_from_signature (method_sig);
  fndecl = add_method_1 (handle_class, access_flags, name, function_type);
  set_java_signature (TREE_TYPE (fndecl), method_sig);
  pop_obstacks ();
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
  /* Push the obstack of field_type ? FIXME */
  push_obstacks (&permanent_obstack, &permanent_obstack);
  field = build_decl (is_static ? VAR_DECL : FIELD_DECL, name, field_type);
  pop_obstacks ();
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
    warning ("duplicate ConstanValue atribute for field '%s'",
	     IDENTIFIER_POINTER (DECL_NAME (field)));
  else
    DECL_INITIAL (field) = constant;
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

tree utf8_decl_list = NULL_TREE;

tree
build_utf8_ref (name)
     tree name;
{
  const char * name_ptr = IDENTIFIER_POINTER(name);
  int name_len = IDENTIFIER_LENGTH(name);
  char buf[60];
  char *buf_ptr;
  tree ctype, field = NULL_TREE, str_type, cinit, string;
  static int utf8_count = 0;
  int name_hash;
  tree ref = IDENTIFIER_UTF8_REF (name);
  tree decl;
  if (ref != NULL_TREE)
    return ref;

  push_obstacks (&permanent_obstack, &permanent_obstack);
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

  /* Build a unique identifier based on buf. */
  sprintf(buf, "_Utf%d", ++utf8_count);
  buf_ptr = &buf[strlen (buf)];
  if (name_len > 0 && name_ptr[0] >= '0' && name_ptr[0] <= '9')
    *buf_ptr++ = '_';
  while (--name_len >= 0)
    {
      unsigned char c = *name_ptr++;
      if (c & 0x80)
	continue;
      if (!ISALPHA(c) && !ISDIGIT(c))
	c = '_';
      *buf_ptr++ = c;
      if (buf_ptr >= buf + 50)
	break;
    }
  *buf_ptr = '\0';

  decl = build_decl (VAR_DECL, get_identifier (buf), utf8const_type);
  /* FIXME get some way to force this into .text, not .data. */
  TREE_STATIC (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  DECL_IGNORED_P (decl) = 1;
  TREE_READONLY (decl) = 1;
  TREE_THIS_VOLATILE (decl) = 0;
  DECL_INITIAL (decl) = cinit;
  TREE_CHAIN (decl) = utf8_decl_list;
  layout_decl (decl, 0);
  pushdecl (decl);
  rest_of_decl_compilation (decl, (char*) 0, global_bindings_p (), 0);
  utf8_decl_list = decl;
  make_decl_rtl (decl, (char*) 0, 1);
  ref = build1 (ADDR_EXPR, utf8const_ptr_type, decl);
  IDENTIFIER_UTF8_REF (name) = ref;
  pop_obstacks ();
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
	      push_obstacks (&permanent_obstack, &permanent_obstack);
	      decl = build_decl (VAR_DECL, decl_name, class_type_node);
	      DECL_SIZE (decl) = TYPE_SIZE (class_type_node);
	      DECL_SIZE_UNIT (decl) = TYPE_SIZE_UNIT (class_type_node);
	      TREE_STATIC (decl) = 1;
	      TREE_PUBLIC (decl) = 1;
	      DECL_IGNORED_P (decl) = 1;
	      DECL_ARTIFICIAL (decl) = 1;
	      DECL_ASSEMBLER_NAME (decl) = mangle_class_field (type);
	      make_decl_rtl (decl, NULL, 1);
	      pushdecl_top_level (decl);
	      if (is_compiled == 1)
		DECL_EXTERNAL (decl) = 1;
	      pop_obstacks ();
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
		fatal ("internal error - bad type to build_class_ref");
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
	      push_obstacks (&permanent_obstack, &permanent_obstack);
	      decl = build_decl (VAR_DECL, decl_name, class_type_node);
	      TREE_STATIC (decl) = 1;
	      TREE_PUBLIC (decl) = 1;
	      make_decl_rtl (decl, NULL, 1);
	      pushdecl_top_level (decl);
	      if (is_compiled == 1)
		DECL_EXTERNAL (decl) = 1;
	      pop_obstacks ();
	    }
	}

      ref = build1 (ADDR_EXPR, class_ptr_type, decl);
      return ref;
    }
  else
    {
      int index;
      tree cl;
      push_obstacks (&permanent_obstack, &permanent_obstack);
      index = alloc_class_constant (type);
      cl = build_ref_from_constant_pool (index); 
      TREE_TYPE (cl) = promote_type (class_ptr_type);
      pop_obstacks ();
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
      if (DECL_RTL (fdecl) == 0)
	{
	  push_obstacks (&permanent_obstack, &permanent_obstack);
	  make_decl_rtl (fdecl, NULL, 1);
	  pop_obstacks ();
	  if (is_compiled == 1)
	    DECL_EXTERNAL (fdecl) = 1;
	}
      return fdecl;
    }
  else
    {
      /* Compile as:
       * *(FTYPE*)build_class_ref(FCLASS)->fields[INDEX].info.addr */
      static tree fields_ident = NULL_TREE;
      static tree info_ident = NULL_TREE;
      tree ref = build_class_ref (fclass);
      tree fld;
      int field_index = 0;
      ref = build1 (INDIRECT_REF, class_type_node, ref);
      if (fields_ident == NULL_TREE)
	fields_ident = get_identifier ("fields");
      if (info_ident == NULL_TREE)
	info_ident = get_identifier ("info");
      ref = build (COMPONENT_REF, field_ptr_type_node, ref,
		   lookup_field (&class_type_node, fields_ident));

      for (fld = TYPE_FIELDS (fclass); ; fld = TREE_CHAIN (fld))
	{
	  if (fld == fdecl)
	    break;
	  if (fld == NULL_TREE)
	    fatal ("field '%s' not found in class",
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
  tree minit;
  tree code;
#define ACC_TRANSLATED          0x4000
  int accflags = get_access_flags_from_decl (mdecl) | ACC_TRANSLATED;
  code = null_pointer_node;
  if (DECL_RTL (mdecl))
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
  PUSH_FIELD_VALUE (minit, "ncode", code);
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
  tree vtable = get_dispatch_vector (type);
  int i;
  tree list = NULL_TREE;
  int nvirtuals = TREE_VEC_LENGTH (vtable);
  for (i = nvirtuals;  --i >= 0; )
    {
      tree method = TREE_VEC_ELT (vtable, i);
      if (METHOD_ABSTRACT (method))
	warning_with_decl (method, "abstract method in non-abstract class");
      if (DECL_RTL (method) == 0)
	make_decl_rtl (method, NULL, 1);
      list = tree_cons (NULL_TREE /*DECL_VINDEX (method) + 2*/,
			build1 (ADDR_EXPR, nativecode_ptr_type_node, method),
			list);
    }
  /* Dummy entry for compatibility with G++ -fvtable-thunks.  When
     using the Boehm GC we sometimes stash a GC type descriptor
     there.  */
  list = tree_cons (integer_zero_node, get_boehm_type_descriptor (type),
		    list);
  list = tree_cons (integer_zero_node, this_class_addr, list);
  return build (CONSTRUCTOR, build_prim_array_type (nativecode_ptr_type_node,
						    nvirtuals + 2),
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
  for (method = TYPE_METHODS (CLASS_TO_HANDLE_TYPE (type));
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
      && ! CLASS_ABSTRACT (type_decl) && ! CLASS_INTERFACE (type_decl))
    {
      tree dtable = get_dispatch_table (type, this_class_addr);
      dtable_decl = build_dtable_decl (type);
      DECL_INITIAL (dtable_decl) = dtable;
      TREE_STATIC (dtable_decl) = 1;
      DECL_ARTIFICIAL (dtable_decl) = 1;
      DECL_IGNORED_P (dtable_decl) = 1;
      TREE_PUBLIC (dtable_decl) = 1;
      rest_of_decl_compilation (dtable_decl, (char*) 0, 1, 0);
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
		    build1 (ADDR_EXPR, dtable_ptr_type, class_dtable_decl));
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
  PUSH_FIELD_VALUE (cons, "vtable_method_count", TYPE_NVIRTUALS (type));
  PUSH_FIELD_VALUE (cons, "fields",
		    fields_decl == NULL_TREE ? null_pointer_node
		    : build1 (ADDR_EXPR, field_ptr_type_node, fields_decl));
  PUSH_FIELD_VALUE (cons, "size_in_bytes", size_in_bytes (type));
  PUSH_FIELD_VALUE (cons, "field_count", build_int_2 (field_count, 0));
  PUSH_FIELD_VALUE (cons, "static_field_count",
		    build_int_2 (static_field_count, 0));
  PUSH_FIELD_VALUE (cons, "vtable",
		    dtable_decl == NULL_TREE ? null_pointer_node
		    : build1 (ADDR_EXPR, dtable_ptr_type, dtable_decl));
  PUSH_FIELD_VALUE (cons, "interfaces", interfaces);
  PUSH_FIELD_VALUE (cons, "loader", null_pointer_node);
  PUSH_FIELD_VALUE (cons, "interface_count", build_int_2 (interface_len, 0));
  PUSH_FIELD_VALUE (cons, "state", integer_zero_node);

  PUSH_FIELD_VALUE (cons, "thread", null_pointer_node);
  PUSH_FIELD_VALUE (cons, "depth", integer_zero_node);
  PUSH_FIELD_VALUE (cons, "ancestors", null_pointer_node);
  PUSH_FIELD_VALUE (cons, "idt", null_pointer_node);

  FINISH_RECORD_CONSTRUCTOR (cons);

  DECL_INITIAL (decl) = cons;
  rest_of_decl_compilation (decl, (char*) 0, 1, 0);
}

void
finish_class ()
{
  tree method;
  tree type_methods = TYPE_METHODS (CLASS_TO_HANDLE_TYPE (current_class));
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
	  /* It's a deferred inline method.  Decide if we need to emit it. */
	  if (flag_keep_inline_functions
	      || TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (method))
	      || ! METHOD_PRIVATE (method)
	      || saw_native_method)
	    {
	      temporary_allocation ();
	      output_inline_function (method);
	      permanent_allocation (1);
	      /* Scan the list again to see if there are any earlier
                 methods to emit. */
	      method = type_methods;
	      continue;
	    }
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

  seen_in_zip = (TYPE_JCF (class) && TYPE_JCF (class)->seen_in_zip);
  if (CLASS_FROM_CURRENTLY_COMPILED_SOURCE_P (class) || seen_in_zip)
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

/* Append the mangled name of TYPE onto OBSTACK. */

static void
append_gpp_mangled_type (obstack, type)
     struct obstack *obstack;
     tree type;
{
  switch (TREE_CODE (type))
    {
      char code;
    case BOOLEAN_TYPE: code = 'b';  goto primitive;
    case CHAR_TYPE:    code = 'w';  goto primitive;
    case VOID_TYPE:    code = 'v';  goto primitive;
    case INTEGER_TYPE:
      /* Get the original type instead of the arguments promoted type.
	 Avoid symbol name clashes. Should call a function to do that.
	 FIXME.  */
      if (type == promoted_short_type_node)
	type = short_type_node;
      if (type == promoted_byte_type_node)
        type = byte_type_node;
      switch (TYPE_PRECISION (type))
	{
	case  8:       code = 'c';  goto primitive;
	case 16:       code = 's';  goto primitive;
	case 32:       code = 'i';  goto primitive;
	case 64:       code = 'x';  goto primitive;
	default:  goto bad_type;
	}
    primitive:
      obstack_1grow (obstack, code);
      break;
    case REAL_TYPE:
      switch (TYPE_PRECISION (type))
	{
	case 32:       code = 'f';  goto primitive;
	case 64:       code = 'd';  goto primitive;
	default:  goto bad_type;
	}
    case POINTER_TYPE:
      type = TREE_TYPE (type);
      obstack_1grow (obstack, 'P');
    case RECORD_TYPE:
      if (TYPE_ARRAY_P (type))
	{
	  obstack_grow (obstack, "t6JArray1Z", sizeof("t6JArray1Z")-1);
	  append_gpp_mangled_type (obstack, TYPE_ARRAY_ELEMENT (type));
	}
      else
	{
	  const char *class_name = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type)));
	  append_gpp_mangled_classtype (obstack, class_name);
	}
      break;
    bad_type:
    default:
      fatal ("internal error - trying to mangle unknown type");
    }
}

/* Build the mangled name of the `class' field.  */

static tree
mangle_class_field (class)
     tree class;
{
  tree name;
  obstack_grow (&temporary_obstack, "_CL_", 4);
  append_gpp_mangled_type (&temporary_obstack, class);
  obstack_1grow (&temporary_obstack, '\0');
  name = get_identifier (obstack_base (&temporary_obstack));
  obstack_free (&temporary_obstack, obstack_base (&temporary_obstack));
  return name;
}

/* Build the mangled (assembly-level) name of the static field FIELD. */

static tree
mangle_static_field (field)
     tree field;
{
  tree class = DECL_CONTEXT (field);
  tree name = DECL_NAME (field);
  int encoded_len;
#if ! defined (NO_DOLLAR_IN_LABEL) || ! defined (NO_DOT_IN_LABEL)
  obstack_1grow (&temporary_obstack, '_');
#else
  obstack_grow (&temporary_obstack, "__static_", 9);
#endif
  append_gpp_mangled_type (&temporary_obstack, class);
  encoded_len = unicode_mangling_length (IDENTIFIER_POINTER (name),
					 IDENTIFIER_LENGTH (name));
  if (encoded_len > 0)
    {
      obstack_1grow (&temporary_obstack, 'U');
    }
#ifndef NO_DOLLAR_IN_LABEL
  obstack_1grow (&temporary_obstack, '$');
#else /* NO_DOLLAR_IN_LABEL */
#ifndef NO_DOT_IN_LABEL
  obstack_1grow (&temporary_obstack, '.');
#else /* NO_DOT_IN_LABEL */
  obstack_1grow (&temporary_obstack, '_');
#endif  /* NO_DOT_IN_LABEL */
#endif  /* NO_DOLLAR_IN_LABEL */
  if (encoded_len > 0)
    {
      emit_unicode_mangled_name (&temporary_obstack,
				 IDENTIFIER_POINTER (name), 
				 IDENTIFIER_LENGTH (name));
    }
  else
    {
      obstack_grow (&temporary_obstack,
		    IDENTIFIER_POINTER (name),
		    IDENTIFIER_LENGTH (name));
    }
  obstack_1grow (&temporary_obstack, '\0');
  name = get_identifier (obstack_base (&temporary_obstack));
  obstack_free (&temporary_obstack, obstack_base (&temporary_obstack));
  return name;
}

/* Build a VAR_DECL for the dispatch table (vtable) for class TYPE. */

tree
build_dtable_decl (type)
     tree type;
{
  tree name;
  obstack_grow (&temporary_obstack, "__vt_", 5);
  append_gpp_mangled_type (&temporary_obstack, type);
  obstack_1grow (&temporary_obstack, '\0');
  name = get_identifier (obstack_base (&temporary_obstack));
  obstack_free (&temporary_obstack, obstack_base (&temporary_obstack));
  return build_decl (VAR_DECL, name, dtable_type);
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
  push_obstacks (&permanent_obstack, &permanent_obstack);
  base_decl = build_decl (FIELD_DECL, NULL_TREE, super_class);
  pop_obstacks ();
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
      if (!CLASS_LOADED_P (super_class) 
	  && CLASS_FROM_SOURCE_P (super_class))
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
	  super_class = do_resolve_class (NULL_TREE, /* FIXME? */
					  super_class, NULL_TREE, this_class);
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
  static tree list = NULL_TREE;
  tree super_class = CLASSTYPE_SUPER (this_class);
  tree field;
  
  list = tree_cons (this_class, NULL_TREE, list);
  if (CLASS_BEING_LAIDOUT (this_class))
    {
      char buffer [1024];
      tree current;
      
      sprintf (buffer, " with `%s'",
	       IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (this_class))));
      obstack_grow (&temporary_obstack, buffer, strlen (buffer));

      for (current = TREE_CHAIN (list); current; 
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
      cyclic_inheritance_report = obstack_finish (&temporary_obstack);
      TYPE_SIZE (this_class) = error_mark_node;
      return;
    }
  CLASS_BEING_LAIDOUT (this_class) = 1;

  if (super_class)
    {
      super_class = maybe_layout_super_class (super_class, this_class);
      if (TREE_CODE (TYPE_SIZE (super_class)) == ERROR_MARK)
	{
	  TYPE_SIZE (this_class) = error_mark_node;
	  CLASS_BEING_LAIDOUT (this_class) = 0;
	  list = TREE_CHAIN (list);
	  return;
	}
      if (TYPE_SIZE (this_class) == NULL_TREE)
	push_super_field (this_class, super_class);
    }

  for (field = TYPE_FIELDS (this_class);
       field != NULL_TREE;  field = TREE_CHAIN (field))
    {
      if (FIELD_STATIC (field))
	{
	  /* Set DECL_ASSEMBLER_NAME to something suitably mangled. */
	  DECL_ASSEMBLER_NAME (field) = mangle_static_field (field);
	}
    }

  layout_type (this_class);

  /* Convert the size back to an SI integer value */
  TYPE_SIZE_UNIT (this_class) = 
    fold (convert (int_type_node, TYPE_SIZE_UNIT (this_class)));

  CLASS_BEING_LAIDOUT (this_class) = 0;
  list = TREE_CHAIN (list);
}

void
layout_class_methods (this_class)
     tree this_class;
{
  tree method_decl, dtable_count;
  tree super_class, handle_type;

  if (TYPE_NVIRTUALS (this_class))
    return;

  push_obstacks (&permanent_obstack, &permanent_obstack);
  super_class = CLASSTYPE_SUPER (this_class);
  handle_type = CLASS_TO_HANDLE_TYPE (this_class);

  if (super_class)
    {
      super_class = maybe_layout_super_class (super_class, this_class);
      if (!TYPE_NVIRTUALS (super_class))
	layout_class_methods (super_class);
      dtable_count = TYPE_NVIRTUALS (super_class);
    }
  else
    dtable_count = integer_zero_node;
  
  TYPE_METHODS (handle_type) = nreverse (TYPE_METHODS (handle_type));

  for (method_decl = TYPE_METHODS (handle_type);
       method_decl; method_decl = TREE_CHAIN (method_decl))
    dtable_count = layout_class_method (this_class, super_class, 
					method_decl, dtable_count);

  TYPE_NVIRTUALS (this_class) = dtable_count;

#ifdef JAVA_USE_HANDLES
  layout_type (handle_type);
#endif
  pop_obstacks ();
}

/* Lay METHOD_DECL out, returning a possibly new value of
   DTABLE_COUNT.  */

tree
layout_class_method (this_class, super_class, method_decl, dtable_count)
     tree this_class, super_class, method_decl, dtable_count;
{
  const char *ptr;
  char *asm_name;
  tree arg, arglist, t;
  int method_name_needs_escapes = 0;
  tree method_name = DECL_NAME (method_decl);
  int method_name_is_wfl = 
    (TREE_CODE (method_name) == EXPR_WITH_FILE_LOCATION);
  if (method_name_is_wfl)
    method_name = java_get_real_method_name (method_decl);

  if (!ID_INIT_P (method_name) && !ID_FINIT_P (method_name))
    {
      int encoded_len
	= unicode_mangling_length (IDENTIFIER_POINTER (method_name), 
				   IDENTIFIER_LENGTH (method_name));
      if (encoded_len > 0)
	{
	  method_name_needs_escapes = 1;
	  emit_unicode_mangled_name (&temporary_obstack,
				     IDENTIFIER_POINTER (method_name), 
				     IDENTIFIER_LENGTH (method_name));
	}
      else
	{
	  obstack_grow (&temporary_obstack,
			IDENTIFIER_POINTER (method_name),
			IDENTIFIER_LENGTH (method_name));
	}
    }
      
  obstack_grow (&temporary_obstack, "__", 2);
  if (ID_FINIT_P (method_name))
    obstack_grow (&temporary_obstack, "finit", 5);
  append_gpp_mangled_type (&temporary_obstack, this_class);
  TREE_PUBLIC (method_decl) = 1;

  t = TREE_TYPE (method_decl);
  arglist = TYPE_ARG_TYPES (t);
  if (TREE_CODE (t) == METHOD_TYPE)
    arglist = TREE_CHAIN (arglist);
  for (arg = arglist; arg != end_params_node;  )
    {
      tree a = arglist;
      tree argtype = TREE_VALUE (arg);
      int tindex = 1;
      if (TREE_CODE (argtype) == POINTER_TYPE)
	{
	  /* This is O(N**2).  Do we care?  Cfr gcc/cp/method.c. */
	  while (a != arg && argtype != TREE_VALUE (a))
	    a = TREE_CHAIN (a), tindex++;
	}
      else
	a = arg;
      if (a != arg)
	{
	  char buf[12];
	  int nrepeats = 0;
	  do
	    {
	      arg = TREE_CHAIN (arg); nrepeats++;
	    }
	  while (arg != end_params_node && argtype == TREE_VALUE (arg));
	  if (nrepeats > 1)
	    {
	      obstack_1grow (&temporary_obstack, 'N');
	      sprintf (buf, "%d", nrepeats);
	      obstack_grow (&temporary_obstack, buf, strlen (buf));
	      if (nrepeats > 9)
		obstack_1grow (&temporary_obstack, '_');
	    }
	  else
	    obstack_1grow (&temporary_obstack, 'T');
	  sprintf (buf, "%d", tindex);
	  obstack_grow (&temporary_obstack, buf, strlen (buf));
	  if (tindex > 9)
	    obstack_1grow (&temporary_obstack, '_');
	}
      else
	{
	  append_gpp_mangled_type (&temporary_obstack, argtype);
	  arg = TREE_CHAIN (arg);
	}
    }
  if (method_name_needs_escapes)
    obstack_1grow (&temporary_obstack, 'U');

  obstack_1grow (&temporary_obstack, '\0');
  asm_name = obstack_finish (&temporary_obstack);
  DECL_ASSEMBLER_NAME (method_decl) = get_identifier (asm_name);
  /* We don't generate a RTL for the method if it's abstract, or if
     it's an interface method that isn't clinit. */
  if (! METHOD_ABSTRACT (method_decl) 
      || (CLASS_INTERFACE (TYPE_NAME (this_class)) 
	  && (DECL_CLINIT_P (method_decl))))
    make_function_rtl (method_decl);
  obstack_free (&temporary_obstack, asm_name);

  if (ID_INIT_P (method_name))
    {
      const char *p = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (this_class)));
      for (ptr = p; *ptr; )
	{
	  if (*ptr++ == '.')
	    p = ptr;
	}
      if (method_name_is_wfl)
	EXPR_WFL_NODE (DECL_NAME (method_decl)) = get_identifier (p);
      else
	DECL_NAME (method_decl) = get_identifier (p);
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
#if 0
	  else if (TREE_TYPE (TREE_TYPE (method_decl))
		   != TREE_TYPE (TREE_TYPE (super_method)))
	    {
	      error_with_decl (method_decl,
			       "Method `%s' redefined with different return type");  
	      error_with_decl (super_method,
			       "Overridden decl is here");
	    }
#endif
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

static tree registered_class = NULL_TREE;

void
register_class ()
{
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

/* Generate a function that gets called at start-up (static contructor) time,
   which calls registerClass for all the compiled classes. */

void
emit_register_classes ()
{
  extern tree get_file_function_name PARAMS ((int));
  tree init_name = get_file_function_name ('I');
  tree init_type = build_function_type (void_type_node, end_params_node);
  tree init_decl;
  tree t;

  init_decl = build_decl (FUNCTION_DECL, init_name, init_type);
  DECL_ASSEMBLER_NAME (init_decl) = init_name;
  TREE_STATIC (init_decl) = 1;
  current_function_decl = init_decl;
  DECL_RESULT (init_decl) = build_decl(RESULT_DECL, NULL_TREE, void_type_node);
  /*  DECL_EXTERNAL (init_decl) = 1;*/
  TREE_PUBLIC (init_decl) = 1;
  pushlevel (0);
  make_function_rtl (init_decl);
  init_function_start (init_decl, input_filename, 0);
  expand_function_start (init_decl, 0);

  for ( t = registered_class; t; t = TREE_CHAIN (t))
    emit_library_call (registerClass_libfunc, 0, VOIDmode, 1,
		       XEXP (DECL_RTL (t), 0), Pmode);

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
  assemble_constructor (IDENTIFIER_POINTER (init_name));
}

void
init_class_processing ()
{
  registerClass_libfunc = gen_rtx (SYMBOL_REF, Pmode, "_Jv_RegisterClass");
}
