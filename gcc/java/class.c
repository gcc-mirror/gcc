/* Functions related to building classes and their related objects.
   Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005
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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

/* Written by Per Bothner <bothner@cygnus.com> */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
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
#include "except.h"
#include "cgraph.h"
#include "tree-iterator.h"

/* DOS brain-damage */
#ifndef O_BINARY
#define O_BINARY 0 /* MS-DOS brain-damage */
#endif

static tree make_method_value (tree);
static tree build_java_method_type (tree, tree, int);
static int32 hashUtf8String (const char *, int);
static tree make_field_value (tree);
static tree get_dispatch_vector (tree);
static tree get_dispatch_table (tree, tree);
static int supers_all_compiled (tree type);
static tree maybe_layout_super_class (tree, tree);
static void add_miranda_methods (tree, tree);
static int assume_compiled (const char *);
static tree build_symbol_entry (tree);
static tree emit_assertion_table (tree);

struct obstack temporary_obstack;

/* The compiler generates different code depending on whether or not
   it can assume certain classes have been compiled down to native
   code or not.  The compiler options -fassume-compiled= and
   -fno-assume-compiled= are used to create a tree of
   class_flag_node objects.  This tree is queried to determine if
   a class is assume to be compiled or not.  Each node in the tree
   represents either a package or a specific class.  */

typedef struct class_flag_node_struct
{
  /* The class or package name.  */
  const char *ident;

  /* Nonzero if this represents an exclusion.  */
  int value;

  /* Pointers to other nodes in the tree.  */
  struct class_flag_node_struct *parent;
  struct class_flag_node_struct *sibling;
  struct class_flag_node_struct *child;
} class_flag_node;

static class_flag_node *find_class_flag_node (class_flag_node *, const char *);
static void add_class_flag (class_flag_node **, const char *, int);

/* This is the root of the include/exclude tree.  */

static class_flag_node *assume_compiled_tree;

static class_flag_node *enable_assert_tree;

static GTY(()) tree class_roots[5];
#define registered_class class_roots[0]
#define fields_ident class_roots[1]  /* get_identifier ("fields") */
#define info_ident class_roots[2]  /* get_identifier ("info") */
#define class_list class_roots[3]
#define class_dtable_decl class_roots[4]

/* Return the node that most closely represents the class whose name
   is IDENT.  Start the search from NODE (followed by its siblings).
   Return NULL if an appropriate node does not exist.  */

static class_flag_node *
find_class_flag_node (class_flag_node *node, const char *ident)
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
	      && (ident[node_ident_length] == '\0'
		  || ident[node_ident_length] == '.')))
	{
	  /* We've found a match, however, there might be a more
             specific match.  */

	  class_flag_node *found = find_class_flag_node (node->child, ident);
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

void
add_class_flag (class_flag_node **rootp, const char *ident, int value)
{
  class_flag_node *root = *rootp;
  class_flag_node *parent, *node;

  /* Create the root of the tree if it doesn't exist yet.  */

  if (NULL == root)
    {
      root = xmalloc (sizeof (class_flag_node));
      root->ident = "";
      root->value = 0;
      root->sibling = NULL;
      root->child = NULL;
      root->parent = NULL;
      *rootp = root;
    }

  /* Calling the function with the empty string means we're setting
     value for the root of the hierarchy.  */

  if (0 == ident[0])
    {
      root->value = value;
      return;
    }

  /* Find the parent node for this new node.  PARENT will either be a
     class or a package name.  Adjust PARENT accordingly.  */

  parent = find_class_flag_node (root, ident);
  if (strcmp (ident, parent->ident) == 0)
    parent->value = value;
  else
    {
      /* Insert new node into the tree.  */
      node = xmalloc (sizeof (class_flag_node));

      node->ident = xstrdup (ident);
      node->value = value;
      node->child = NULL;

      node->parent = parent;
      node->sibling = parent->child;
      parent->child = node;
    }
}

/* Add a new IDENT to the include/exclude tree.  It's an exclusion
   if EXCLUDEP is nonzero.  */

void
add_assume_compiled (const char *ident, int excludep)
{
  add_class_flag (&assume_compiled_tree, ident, excludep);
}

/* The default value returned by enable_assertions. */

#define DEFAULT_ENABLE_ASSERT (flag_emit_class_files || optimize == 0)

/* Enter IDENT (a class or package name) into the enable-assertions table.
   VALUE is true to enable and false to disable. */

void
add_enable_assert (const char *ident, int value)
{
  if (enable_assert_tree == NULL)
    add_class_flag (&enable_assert_tree, "", DEFAULT_ENABLE_ASSERT);
  add_class_flag (&enable_assert_tree, ident, value);
}

/* Returns nonzero if IDENT is the name of a class that the compiler
   should assume has been compiled to object code.  */

static int
assume_compiled (const char *ident)
{
  class_flag_node *i;
  int result;
  
  if (NULL == assume_compiled_tree)
    return 1;

  i = find_class_flag_node (assume_compiled_tree, ident);

  result = ! i->value;
  
  return (result);
}

/* Return true if we should generate code to check assertions within KLASS. */

bool
enable_assertions (tree klass)
{
  /* Check if command-line specifies whether we should check assertions. */

  if (klass != NULL_TREE && DECL_NAME (klass) && enable_assert_tree != NULL)
    {
      const char *ident = IDENTIFIER_POINTER (DECL_NAME (klass));
      class_flag_node *node
	= find_class_flag_node (enable_assert_tree, ident);
      return node->value;
    }

  /* The default is to enable assertions if generating class files,
     or not optimizing. */
  return DEFAULT_ENABLE_ASSERT;
}

/* Return an IDENTIFIER_NODE the same as (OLD_NAME, OLD_LENGTH).
   except that characters matching OLD_CHAR are substituted by NEW_CHAR.
   Also, PREFIX is prepended, and SUFFIX is appended. */

tree
ident_subst (const char* old_name,
	     int old_length,
	     const char *prefix,
	     int old_char,
	     int new_char,
	     const char *suffix)
{
  int prefix_len = strlen (prefix);
  int suffix_len = strlen (suffix);
  int i = prefix_len + old_length + suffix_len + 1;
  char *buffer = alloca (i);

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
identifier_subst (const tree old_id,
		  const char *prefix,
		  int old_char,
		  int new_char,
		  const char *suffix)
{
  return ident_subst (IDENTIFIER_POINTER (old_id), IDENTIFIER_LENGTH (old_id),
		      prefix, old_char, new_char, suffix);
}

/* Generate a valid C identifier from the name of the class TYPE,
   prefixed by PREFIX. */

tree
mangled_classname (const char *prefix, tree type)
{
  tree ident = TYPE_NAME (type);
  if (TREE_CODE (ident) != IDENTIFIER_NODE)
    ident = DECL_NAME (ident);
  return identifier_subst (ident, prefix, '.', '_', "");
}

tree
make_class (void)
{
  tree type;
  type = make_node (RECORD_TYPE);
  MAYBE_CREATE_TYPE_TYPE_LANG_SPECIFIC (type);

  return type;
}

/* Given a fully-qualified classname in NAME (whose length is NAME_LENGTH),
   and where each of the constituents is separated by '/',
   return a corresponding IDENTIFIER_NODE, except using '.' as separator. */

tree
unmangle_classname (const char *name, int name_length)
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

#define GEN_TABLE(TABLE, NAME, TABLE_TYPE, TYPE)			\
do									\
{									\
  const char *typename = IDENTIFIER_POINTER (mangled_classname ("", TYPE)); \
  char *buf = alloca (strlen (typename) + strlen (#NAME "_syms_") + 1);	\
  tree decl;								\
									\
  sprintf (buf, #NAME "_%s", typename);					\
  TYPE_## TABLE ##_DECL (type) = decl =					\
    build_decl (VAR_DECL, get_identifier (buf), TABLE_TYPE);		\
  DECL_EXTERNAL (decl) = 1;						\
  TREE_STATIC (decl) = 1;						\
  TREE_READONLY (decl) = 1;						\
  TREE_CONSTANT (decl) = 1;						\
  DECL_IGNORED_P (decl) = 1;						\
  /* Mark the table as belonging to this class.  */			\
  pushdecl (decl);							\
  MAYBE_CREATE_VAR_LANG_DECL_SPECIFIC (decl);				\
  DECL_OWNER (decl) = TYPE;						\
  sprintf (buf, #NAME "_syms_%s", typename);				\
  TYPE_## TABLE ##_SYMS_DECL (TYPE) =					\
    build_decl (VAR_DECL, get_identifier (buf), symbols_array_type);	\
  TREE_STATIC (TYPE_## TABLE ##_SYMS_DECL (TYPE)) = 1;			\
  TREE_CONSTANT (TYPE_## TABLE ##_SYMS_DECL (TYPE)) = 1;		\
  DECL_IGNORED_P (TYPE_## TABLE ##_SYMS_DECL (TYPE)) = 1;		\
  pushdecl (TYPE_## TABLE ##_SYMS_DECL (TYPE));				\
}									\
while (0)

/* Given a class, create the DECLs for all its associated indirect
   dispatch tables.  */
void
gen_indirect_dispatch_tables (tree type)
{
  const char *typename = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type)));
  {  
    tree field = NULL;
    char *buf = alloca (strlen (typename) + strlen ("_catch_classes_") + 1);
    tree catch_class_type = make_node (RECORD_TYPE);

    sprintf (buf, "_catch_classes_%s", typename);
    PUSH_FIELD (catch_class_type, field, "address", utf8const_ptr_type);
    PUSH_FIELD (catch_class_type, field, "classname", ptr_type_node);
    FINISH_RECORD (catch_class_type);
    
    TYPE_CTABLE_DECL (type) 
      = build_decl (VAR_DECL, get_identifier (buf),
		    build_array_type (catch_class_type, 0));
    DECL_EXTERNAL (TYPE_CTABLE_DECL (type)) = 1;
    TREE_STATIC (TYPE_CTABLE_DECL (type)) = 1;
    TREE_READONLY (TYPE_CTABLE_DECL (type)) = 1;
    TREE_CONSTANT (TYPE_CTABLE_DECL (type)) = 1;
    DECL_IGNORED_P (TYPE_CTABLE_DECL (type)) = 1;
    pushdecl (TYPE_CTABLE_DECL (type));  
  }

  if (flag_indirect_dispatch)
    {
      GEN_TABLE (ATABLE, _atable, atable_type, type);
      GEN_TABLE (OTABLE, _otable, otable_type, type);
      GEN_TABLE (ITABLE, _itable, itable_type, type);
    }
}

#undef GEN_TABLE

tree
push_class (tree class_type, tree class_name)
{
  tree decl, signature;
  location_t saved_loc = input_location;
#ifndef USE_MAPPED_LOCATION
  tree source_name = identifier_subst (class_name, "", '.', '/', ".java");
  input_filename = IDENTIFIER_POINTER (source_name);
  input_line = 0;
#endif
  CLASS_P (class_type) = 1;
  decl = build_decl (TYPE_DECL, class_name, class_type);

  /* dbxout needs a DECL_SIZE if in gstabs mode */
  DECL_SIZE (decl) = integer_zero_node;

  input_location = saved_loc;
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
lookup_class (tree name)
{
  tree decl = IDENTIFIER_CLASS_VALUE (name);
  if (decl == NULL_TREE)
    decl = push_class (make_class (), name);
  return TREE_TYPE (decl);
}

void
set_super_info (int access_flags, tree this_class,
		tree super_class, int interfaces_count)
{
  int total_supers = interfaces_count;
  tree class_decl = TYPE_NAME (this_class);
  
  if (super_class)
    total_supers++;

  TYPE_BINFO (this_class) = make_tree_binfo (total_supers);
  TYPE_VFIELD (this_class) = TYPE_VFIELD (object_type_node);
  if (super_class)
    {
      tree super_binfo = make_tree_binfo (0);
      BINFO_TYPE (super_binfo) = super_class;
      BINFO_OFFSET (super_binfo) = integer_zero_node;
      BINFO_BASE_APPEND (TYPE_BINFO (this_class), super_binfo);
      CLASS_HAS_SUPER_FLAG (TYPE_BINFO (this_class)) = 1;
    }

  set_class_decl_access_flags (access_flags, class_decl);
}

void
set_class_decl_access_flags (int access_flags, tree class_decl)
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
class_depth (tree clas)
{
  int depth = 0;
  if (! CLASS_LOADED_P (clas))
    load_class (clas, 1);
  if (TYPE_SIZE (clas) == error_mark_node)
    return -1;
  while (clas != object_type_node)
    {
      depth++;
      clas = BINFO_TYPE (BINFO_BASE_BINFO (TYPE_BINFO (clas), 0));
    }
  return depth;
}

/* Return true iff TYPE2 is an interface that extends interface TYPE1 */

int
interface_of_p (tree type1, tree type2)
{
  int i;
  tree binfo, base_binfo;

  if (! TYPE_BINFO (type2))
    return 0;

  for (binfo = TYPE_BINFO (type2), i = 0;
       BINFO_BASE_ITERATE (binfo, i, base_binfo); i++)
    if (BINFO_TYPE (base_binfo) == type1)
      return 1;
  
  for (binfo = TYPE_BINFO (type2), i = 0;
       BINFO_BASE_ITERATE (binfo, i, base_binfo); i++) /*  */
    if (BINFO_TYPE (base_binfo)
	&& interface_of_p (type1, BINFO_TYPE (base_binfo)))
      return 1;
  
  return 0;
}

/* Return true iff TYPE1 inherits from TYPE2. */

int
inherits_from_p (tree type1, tree type2)
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
enclosing_context_p (tree type1, tree type2)
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


/* Return 1 iff TYPE1 and TYPE2 share a common enclosing class, regardless of
   nesting level.  */

int
common_enclosing_context_p (tree type1, tree type2)
{
  while (type1)
    {
      tree current;
      for (current = type2; current;
	   current = (INNER_CLASS_TYPE_P (current) ?
		      TREE_TYPE (DECL_CONTEXT (TYPE_NAME (current))) : 
		      NULL_TREE))
	if (type1 == current)
	  return 1;

      if (INNER_CLASS_TYPE_P (type1))
        type1 = TREE_TYPE (DECL_CONTEXT (TYPE_NAME (type1)));
      else
        break;
    }
  return 0;
}

/* Return 1 iff there exists a common enclosing "this" between TYPE1
   and TYPE2, without crossing any static context.  */

int
common_enclosing_instance_p (tree type1, tree type2)
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

/* Add INTERFACE_CLASS to THIS_CLASS iff INTERFACE_CLASS can't be
   found in THIS_CLASS. Returns NULL_TREE upon success, INTERFACE_CLASS
   if attempt is made to add it twice. */

tree
maybe_add_interface (tree this_class, tree interface_class)
{
  tree binfo, base_binfo;
  int i;

  for (binfo = TYPE_BINFO (this_class), i = 0;
       BINFO_BASE_ITERATE (binfo, i, base_binfo); i++)
    if (BINFO_TYPE (base_binfo) == interface_class)
      return interface_class;
  add_interface (this_class, interface_class);
  return NULL_TREE;
}

/* Add the INTERFACE_CLASS as one of the interfaces of THIS_CLASS. */

void
add_interface (tree this_class, tree interface_class)
{
  tree interface_binfo = make_tree_binfo (0);
  
  BINFO_TYPE (interface_binfo) = interface_class;
  BINFO_OFFSET (interface_binfo) = integer_zero_node;
  BINFO_VPTR_FIELD (interface_binfo) = integer_zero_node;
  BINFO_VIRTUAL_P (interface_binfo) = 1;
  
  BINFO_BASE_APPEND (TYPE_BINFO (this_class), interface_binfo);
}

#if 0
/* Return the address of a pointer to the first FUNCTION_DECL
   in the list (*LIST) whose DECL_NAME is NAME. */

static tree *
find_named_method (tree *list, tree name)
{
  while (*list && DECL_NAME (*list) != name)
    list = &TREE_CHAIN (*list);
  return list;
}
#endif

static tree
build_java_method_type (tree fntype, tree this_class, int access_flags)
{
  if (access_flags & ACC_STATIC)
    return fntype;
  return build_method_type (this_class, fntype);
}

tree
add_method_1 (tree this_class, int access_flags, tree name, tree function_type)
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
  if (access_flags & ACC_STRICT) METHOD_STRICTFP (fndecl) = 1;
  return fndecl;
}

/* Add a method to THIS_CLASS.
   The method's name is NAME.
   Its signature (mangled type) is METHOD_SIG (an IDENTIFIER_NODE). */

tree
add_method (tree this_class, int access_flags, tree name, tree method_sig)
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
add_field (tree class, tree name, tree field_type, int flags)
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
set_constant_value (tree field, tree constant)
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
strLengthUtf8 (char *str, int len)
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
hashUtf8String (const char *str, int len)
{
  const unsigned char* ptr = (const unsigned char*) str;
  const unsigned char *limit = ptr + len;
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

static GTY(()) tree utf8_decl_list = NULL_TREE;

tree
build_utf8_ref (tree name)
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
  PUSH_FIELD_VALUE (cinit, "hash", build_int_cst (NULL_TREE, name_hash));
  PUSH_FIELD_VALUE (cinit, "length", build_int_cst (NULL_TREE, name_len));
  string = build_string (name_len, name_ptr);
  TREE_TYPE (string) = str_type;
  PUSH_FIELD_VALUE (cinit, "data", string);
  FINISH_RECORD_CONSTRUCTOR (cinit);
  TREE_CONSTANT (cinit) = 1;
  TREE_INVARIANT (cinit) = 1;

  /* Generate a unique-enough identifier.  */
  sprintf(buf, "_Utf%d", ++utf8_count);

  decl = build_decl (VAR_DECL, get_identifier (buf), utf8const_type);
  TREE_STATIC (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  DECL_IGNORED_P (decl) = 1;
  TREE_READONLY (decl) = 1;
  TREE_THIS_VOLATILE (decl) = 0;
  DECL_INITIAL (decl) = cinit;

  if (HAVE_GAS_SHF_MERGE)
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

  TREE_CHAIN (decl) = utf8_decl_list;
  layout_decl (decl, 0);
  pushdecl (decl);
  rest_of_decl_compilation (decl, global_bindings_p (), 0);
  utf8_decl_list = decl;
  make_decl_rtl (decl);
  ref = build1 (ADDR_EXPR, utf8const_ptr_type, decl);
  IDENTIFIER_UTF8_REF (name) = ref;
  return ref;
}

/* Like build_class_ref, but instead of a direct reference generate a
   pointer into the constant pool.  */

static tree
build_indirect_class_ref (tree type)
{
  int index;
  tree cl;
  index = alloc_class_constant (type);
  cl = build_ref_from_constant_pool (index); 
  return convert (promote_type (class_ptr_type), cl);
}

/* Build a reference to the class TYPE.
   Also handles primitive types and array types. */

tree
build_class_ref (tree type)
{
  int is_compiled = is_compiled_class (type);
  if (is_compiled)
    {
      tree ref, decl_name, decl;
      if (TREE_CODE (type) == POINTER_TYPE)
	type = TREE_TYPE (type);

      if  (flag_indirect_dispatch
	   && type != output_class
	   && TREE_CODE (type) == RECORD_TYPE)
	return build_indirect_class_ref (type);

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
	      make_decl_rtl (decl);
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
	      return build3 (COMPONENT_REF, NULL_TREE,
			     prim_class, TYPE_identifier_node, NULL_TREE);
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
	      DECL_ARTIFICIAL (decl) = 1;
	      make_decl_rtl (decl);
	      pushdecl_top_level (decl);
	    }
	}

      ref = build1 (ADDR_EXPR, class_ptr_type, decl);
      return ref;
    }
  else
    return build_indirect_class_ref (type);
}

/* Create a local statically allocated variable that will hold a
   pointer to a static field.  */

static tree
build_fieldref_cache_entry (int index, tree fdecl ATTRIBUTE_UNUSED)
{
  tree decl, decl_name;
  const char *name = IDENTIFIER_POINTER (mangled_classname ("_cpool_", output_class));
  char *buf = alloca (strlen (name) + 20);
  sprintf (buf, "%s_%d_ref", name, index);
  decl_name = get_identifier (buf);
  decl = IDENTIFIER_GLOBAL_VALUE (decl_name);
  if (decl == NULL_TREE)
    {
      decl = build_decl (VAR_DECL, decl_name, ptr_type_node);
      TREE_STATIC (decl) = 1;
      TREE_PUBLIC (decl) = 0;
      DECL_EXTERNAL (decl) = 0;
      DECL_ARTIFICIAL (decl) = 1;
      make_decl_rtl (decl);
      pushdecl_top_level (decl);
    }
  return decl;
}

tree
build_static_field_ref (tree fdecl)
{
  tree fclass = DECL_CONTEXT (fdecl);
  int is_compiled = is_compiled_class (fclass);
  int from_class = ! CLASS_FROM_SOURCE_P (current_class);

  /* Allow static final fields to fold to a constant.  When using
     -findirect-dispatch, we simply never do this folding if compiling
     from .class; in the .class file constants will be referred to via
     the constant pool.  */
  if ((!flag_indirect_dispatch || !from_class)
      && (is_compiled
	  || (FIELD_FINAL (fdecl) && DECL_INITIAL (fdecl) != NULL_TREE
	      && (JSTRING_TYPE_P (TREE_TYPE (fdecl))
		  || JNUMERIC_TYPE_P (TREE_TYPE (fdecl)))
	      && TREE_CONSTANT (DECL_INITIAL (fdecl)))))
    {
      if (!DECL_RTL_SET_P (fdecl))
	{
	  if (is_compiled == 1)
	    DECL_EXTERNAL (fdecl) = 1;
	  make_decl_rtl (fdecl);
	}
    }
  else
    {
      /* Generate a CONSTANT_FieldRef for FDECL in the constant pool
	 and a class local static variable CACHE_ENTRY, then
      
      *(fdecl **)((__builtin_expect (cache_entry == null, false)) 
		  ? cache_entry = _Jv_ResolvePoolEntry (output_class, cpool_index)
		  : cache_entry)

      This can mostly be optimized away, so that the usual path is a
      load followed by a test and branch.  _Jv_ResolvePoolEntry is
      only called once for each constant pool entry.

      There is an optimization that we don't do: at the start of a
      method, create a local copy of CACHE_ENTRY and use that instead.

      */

      int cpool_index = alloc_constant_fieldref (output_class, fdecl);
      tree cache_entry = build_fieldref_cache_entry (cpool_index, fdecl);
      tree test 
	= build3 (CALL_EXPR, boolean_type_node, 
		  build_address_of (built_in_decls[BUILT_IN_EXPECT]),
		  tree_cons (NULL_TREE, build2 (EQ_EXPR, boolean_type_node,
						cache_entry, null_pointer_node),
			     build_tree_list (NULL_TREE, boolean_false_node)),
		  NULL_TREE);
      tree cpool_index_cst = build_int_cst (NULL_TREE, cpool_index);
      tree init
	= build3 (CALL_EXPR, ptr_type_node,
		  build_address_of (soft_resolvepoolentry_node),
		  tree_cons (NULL_TREE, build_class_ref (output_class),
			     build_tree_list (NULL_TREE, cpool_index_cst)),
		  NULL_TREE);
      init = build2 (MODIFY_EXPR, ptr_type_node, cache_entry, init);
      init = build3 (COND_EXPR, ptr_type_node, test, init, cache_entry);
      init = fold_convert (build_pointer_type (TREE_TYPE (fdecl)), init);
      fdecl = build1 (INDIRECT_REF, TREE_TYPE (fdecl), init);
    }
  return fdecl;
}

int
get_access_flags_from_decl (tree decl)
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
      if (METHOD_STRICTFP (decl))
	access_flags |= ACC_STRICT;
      if (METHOD_INVISIBLE (decl))
	access_flags |= ACC_INVISIBLE;
      return access_flags;
    }
  abort ();
}

static GTY (()) int alias_labelno = 0;

/* Create a private alias for METHOD. Using this alias instead of the method
   decl ensures that ncode entries in the method table point to the real function 
   at runtime, not a PLT entry.  */

static tree
make_local_function_alias (tree method)
{
#ifdef ASM_OUTPUT_DEF
  tree alias;
  
  const char *method_name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (method));
  char *name = alloca (strlen (method_name) + 2);
  char *buf = alloca (strlen (method_name) + 128);

  /* Only create aliases for local functions.  */
  if (DECL_EXTERNAL (method))
    return method;
    
  /* Prefix method_name with 'L' for the alias label.  */
  *name = 'L';
  strcpy (name + 1, method_name);

  ASM_GENERATE_INTERNAL_LABEL (buf, name, alias_labelno++);  
  alias = build_decl (FUNCTION_DECL, get_identifier (buf),
		      TREE_TYPE (method));
  DECL_CONTEXT (alias) = NULL;
  TREE_READONLY (alias) = TREE_READONLY (method);
  TREE_THIS_VOLATILE (alias) = TREE_THIS_VOLATILE (method);
  TREE_PUBLIC (alias) = 0;
  DECL_EXTERNAL (alias) = 0;
  DECL_ARTIFICIAL (alias) = 1;
  DECL_INLINE (alias) = 0;
  DECL_INITIAL (alias) = error_mark_node;
  TREE_ADDRESSABLE (alias) = 1;
  TREE_USED (alias) = 1;
  SET_DECL_ASSEMBLER_NAME (alias, DECL_NAME (alias));
  TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (alias)) = 1;
  if (!flag_syntax_only)
    assemble_alias (alias, DECL_ASSEMBLER_NAME (method));
  return alias;
#else
  return method;
#endif
}

/** Make reflection data (_Jv_Field) for field FDECL. */

static tree
make_field_value (tree fdecl)
{
  tree finit;
  int flags;
  tree type = TREE_TYPE (fdecl);
  int resolved = is_compiled_class (type) && ! flag_indirect_dispatch;

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

  PUSH_FIELD_VALUE (finit, "accflags", build_int_cst (NULL_TREE, flags));
  PUSH_FIELD_VALUE (finit, "bsize", TYPE_SIZE_UNIT (TREE_TYPE (fdecl)));

  PUSH_FIELD_VALUE
    (finit, "info",
     build_constructor (field_info_union_node,
	    build_tree_list
	    ((FIELD_STATIC (fdecl)
	      ? TREE_CHAIN (TYPE_FIELDS (field_info_union_node))
	      : TYPE_FIELDS (field_info_union_node)),
	     (FIELD_STATIC (fdecl)
	      ? build_address_of (fdecl)
	      : byte_position (fdecl)))));

  FINISH_RECORD_CONSTRUCTOR (finit);
  return finit;
}

/** Make reflection data (_Jv_Method) for method MDECL. */

static tree
make_method_value (tree mdecl)
{
  static int method_name_count = 0;
  tree minit;
  tree index;
  tree code;
  tree class_decl;
#define ACC_TRANSLATED          0x4000
  int accflags = get_access_flags_from_decl (mdecl) | ACC_TRANSLATED;

  class_decl = DECL_CONTEXT (mdecl);
  /* For interfaces, the index field contains the dispatch index. */
  if (CLASS_INTERFACE (TYPE_NAME (class_decl)))
    index = build_int_cst (NULL_TREE,
			   get_interface_method_index (mdecl, class_decl));
  else if (!flag_indirect_dispatch && get_method_index (mdecl) != NULL_TREE)
    index = get_method_index (mdecl);
  else
    index = integer_minus_one_node;

  code = null_pointer_node;
  if (DECL_RTL_SET_P (mdecl))
    code = build1 (ADDR_EXPR, nativecode_ptr_type_node, 
		   make_local_function_alias (mdecl));
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
  PUSH_FIELD_VALUE (minit, "accflags", build_int_cst (NULL_TREE, accflags));
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
	table = build_constructor (type, table);
	/* Compute something unique enough.  */
	sprintf (buf, "_methods%d", method_name_count++);
	array = build_decl (VAR_DECL, get_identifier (buf), type);
	DECL_INITIAL (array) = table;
	TREE_STATIC (array) = 1;
	DECL_ARTIFICIAL (array) = 1;
	DECL_IGNORED_P (array) = 1;
	rest_of_decl_compilation (array, 1, 0);

	table = build1 (ADDR_EXPR, ptr_type_node, array);
      }

    PUSH_FIELD_VALUE (minit, "throws", table);
  }

  FINISH_RECORD_CONSTRUCTOR (minit);
  return minit;
}

static tree
get_dispatch_vector (tree type)
{
  tree vtable = TYPE_VTABLE (type);

  if (vtable == NULL_TREE)
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
	{
	  tree method_index = get_method_index (method);
	  if (method_index != NULL_TREE
	      && host_integerp (method_index, 0))
	    TREE_VEC_ELT (vtable, tree_low_cst (method_index, 0)) = method;
	}
    }

  return vtable;
}

static tree
get_dispatch_table (tree type, tree this_class_addr)
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
	    warning ("%Jabstract method in non-abstract class", method);

	  if (TARGET_VTABLE_USES_DESCRIPTORS)
	    for (j = 0; j < TARGET_VTABLE_USES_DESCRIPTORS; ++j)
	      list = tree_cons (NULL_TREE, null_pointer_node, list);
	  else
	    list = tree_cons (NULL_TREE, null_pointer_node, list);
	}
      else
	{
	  if (!DECL_RTL_SET_P (method))
	    make_decl_rtl (method);

	  if (TARGET_VTABLE_USES_DESCRIPTORS)
	    for (j = 0; j < TARGET_VTABLE_USES_DESCRIPTORS; ++j)
	      {
		tree fdesc = build2 (FDESC_EXPR, nativecode_ptr_type_node, 
				     method, build_int_cst (NULL_TREE, j));
		TREE_CONSTANT (fdesc) = 1;
		TREE_INVARIANT (fdesc) = 1;
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
  return build_constructor (build_prim_array_type (nativecode_ptr_type_node,
						   arraysize), list);
}


/* Set the method_index for a method decl.  */
void
set_method_index (tree decl, tree method_index)
{
  if (method_index != NULL_TREE)
    {
      /* method_index is null if we're using indirect dispatch.  */
      method_index = fold (convert (sizetype, method_index));

      if (TARGET_VTABLE_USES_DESCRIPTORS)
	/* Add one to skip bogus descriptor for class and GC descriptor. */
	method_index = size_binop (PLUS_EXPR, method_index, size_int (1));
      else
	/* Add 1 to skip "class" field of dtable, and 1 to skip GC
	   descriptor.  */
	method_index = size_binop (PLUS_EXPR, method_index, size_int (2));
    }

  DECL_VINDEX (decl) = method_index;
}

/* Get the method_index for a method decl.  */
tree
get_method_index (tree decl)
{
  tree method_index = DECL_VINDEX (decl);

  if (! method_index)
    return NULL;

  if (TARGET_VTABLE_USES_DESCRIPTORS)
    /* Sub one to skip bogus descriptor for class and GC descriptor. */
    method_index = size_binop (MINUS_EXPR, method_index, size_int (1));
  else
    /* Sub 1 to skip "class" field of dtable, and 1 to skip GC descriptor.  */
    method_index = size_binop (MINUS_EXPR, method_index, size_int (2));

  return method_index;
}

static int
supers_all_compiled (tree type)
{
  while (type != NULL_TREE)
    {
      if (!assume_compiled (IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type)))))
	return 0;
      type = CLASSTYPE_SUPER (type);
    }
  return 1;
}

void
make_class_data (tree type)
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
  tree dtable_start_offset = build_int_cst (NULL_TREE,
					    2 * POINTER_SIZE / BITS_PER_UNIT);

  this_class_addr = build_class_ref (type);
  decl = TREE_OPERAND (this_class_addr, 0);

  /* Build Field array. */
  field = TYPE_FIELDS (type);
  while (field && DECL_ARTIFICIAL (field))
    field = TREE_CHAIN (field);  /* Skip dummy fields.  */
  if (field && DECL_NAME (field) == NULL_TREE)
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
	      rest_of_decl_compilation (field, 1, 1);
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
      DECL_INITIAL (fields_decl) = build_constructor (field_array_type,
						      static_fields);
      TREE_STATIC (fields_decl) = 1;
      DECL_ARTIFICIAL (fields_decl) = 1;
      DECL_IGNORED_P (fields_decl) = 1;
      rest_of_decl_compilation (fields_decl, 1, 0);
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
	  && optimize)
	continue;
      /* Even if we have a decl, we don't necessarily have the code.
	 This can happen if we inherit a method from a superclass for
	 which we don't have a .class file.  */
      if (METHOD_DUMMY (method))
	continue;
      init = make_method_value (method);
      method_count++;
      methods = tree_cons (NULL_TREE, init, methods);
    }
  method_array_type = build_prim_array_type (method_type_node, method_count);
  methods_decl = build_decl (VAR_DECL, mangled_classname ("_MT_", type),
			     method_array_type);
  DECL_INITIAL (methods_decl) = build_constructor (method_array_type,
						   nreverse (methods));
  TREE_STATIC (methods_decl) = 1;
  DECL_ARTIFICIAL (methods_decl) = 1;
  DECL_IGNORED_P (methods_decl) = 1;
  rest_of_decl_compilation (methods_decl, 1, 0);

  if (supers_all_compiled (type) && ! CLASS_INTERFACE (type_decl)
      && !flag_indirect_dispatch)
    {
      tree dtable = get_dispatch_table (type, this_class_addr);
      dtable_decl = build_dtable_decl (type);
      DECL_INITIAL (dtable_decl) = dtable;
      TREE_STATIC (dtable_decl) = 1;
      DECL_ARTIFICIAL (dtable_decl) = 1;
      DECL_IGNORED_P (dtable_decl) = 1;
      TREE_PUBLIC (dtable_decl) = 1;
      rest_of_decl_compilation (dtable_decl, 1, 0);
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
      rest_of_decl_compilation (class_dtable_decl, 1, 0);
    }

  super = CLASSTYPE_SUPER (type);
  if (super == NULL_TREE)
    super = null_pointer_node;
  else if (! flag_indirect_dispatch
	   && assume_compiled (IDENTIFIER_POINTER (DECL_NAME (type_decl)))
	   && assume_compiled (IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (super)))))
    super = build_class_ref (super);
  else
    {
      int super_index = alloc_class_constant (super);
      super = build_int_cst (ptr_type_node, super_index);
    }

  /* Build and emit the array of implemented interfaces. */
  if (type != object_type_node)
    interface_len = BINFO_N_BASE_BINFOS (TYPE_BINFO (type)) - 1;
  
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
	  tree child = BINFO_BASE_BINFO (TYPE_BINFO (type), i);
	  tree iclass = BINFO_TYPE (child);
	  tree index;
	  if (! flag_indirect_dispatch
	      && (assume_compiled 
		  (IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (iclass))))))
	    index = build_class_ref (iclass);
	  else
	    {
	      int int_index = alloc_class_constant (iclass);
	      index = build_int_cst (ptr_type_node, int_index);
	    }
	  init = tree_cons (NULL_TREE, index, init); 
	}
      DECL_INITIAL (idecl) = build_constructor (interface_array_type, init);
      TREE_STATIC (idecl) = 1;
      DECL_ARTIFICIAL (idecl) = 1;
      DECL_IGNORED_P (idecl) = 1;
      interfaces = build1 (ADDR_EXPR, ptr_type_node, idecl);
      rest_of_decl_compilation (idecl, 1, 0);
    }

  constant_pool_constructor = build_constants_constructor ();

  if (flag_indirect_dispatch)
    {
      TYPE_OTABLE_DECL (type) 
	= emit_symbol_table 
	(DECL_NAME (TYPE_OTABLE_DECL (type)), 
	 TYPE_OTABLE_DECL (type), TYPE_OTABLE_METHODS (type), 
	 TYPE_OTABLE_SYMS_DECL (type), integer_type_node, 1);
       
      TYPE_ATABLE_DECL (type) 
	= emit_symbol_table 
	(DECL_NAME (TYPE_ATABLE_DECL (type)), 
	 TYPE_ATABLE_DECL (type), TYPE_ATABLE_METHODS (type), 
	 TYPE_ATABLE_SYMS_DECL (type), ptr_type_node, 1);
       
      TYPE_ITABLE_DECL (type) 
	= emit_symbol_table 
	(DECL_NAME (TYPE_ITABLE_DECL (type)), 
	 TYPE_ITABLE_DECL (type), TYPE_ITABLE_METHODS (type), 
	 TYPE_ITABLE_SYMS_DECL (type), ptr_type_node, 2);
    }
  
  TYPE_CTABLE_DECL (type) = emit_catch_table (type);

  START_RECORD_CONSTRUCTOR (temp, object_type_node);
  PUSH_FIELD_VALUE (temp, "vtable",
		    build2 (PLUS_EXPR, dtable_ptr_type,
			    build1 (ADDR_EXPR, dtable_ptr_type,
				    class_dtable_decl),
			    dtable_start_offset));
  if (! flag_hash_synchronization)
    PUSH_FIELD_VALUE (temp, "sync_info", null_pointer_node);
  FINISH_RECORD_CONSTRUCTOR (temp);
  START_RECORD_CONSTRUCTOR (cons, class_type_node);
  PUSH_SUPER_VALUE (cons, temp);
  PUSH_FIELD_VALUE (cons, "next_or_version", gcj_abi_version);
  PUSH_FIELD_VALUE (cons, "name", build_utf8_ref (DECL_NAME (type_decl)));
  PUSH_FIELD_VALUE (cons, "accflags",
		    build_int_cst (NULL_TREE,
				   get_access_flags_from_decl (type_decl)));

  PUSH_FIELD_VALUE (cons, "superclass", 
		    CLASS_INTERFACE (type_decl) ? null_pointer_node : super);
  PUSH_FIELD_VALUE (cons, "constants", constant_pool_constructor);
  PUSH_FIELD_VALUE (cons, "methods",
		    build1 (ADDR_EXPR, method_ptr_type_node, methods_decl));
  PUSH_FIELD_VALUE (cons, "method_count",
		    build_int_cst (NULL_TREE, method_count));

  if (flag_indirect_dispatch)
    PUSH_FIELD_VALUE (cons, "vtable_method_count", integer_minus_one_node);
  else
    PUSH_FIELD_VALUE (cons, "vtable_method_count", TYPE_NVIRTUALS (type));
    
  PUSH_FIELD_VALUE (cons, "fields",
		    fields_decl == NULL_TREE ? null_pointer_node
		    : build1 (ADDR_EXPR, field_ptr_type_node, fields_decl));
  /* If we're using the binary compatibility ABI we don't know the
     size until load time.  */
  PUSH_FIELD_VALUE (cons, "size_in_bytes", 
		    (flag_indirect_dispatch 
		     ? integer_minus_one_node 
		     : size_in_bytes (type)));
  PUSH_FIELD_VALUE (cons, "field_count", 
		    build_int_cst (NULL_TREE, field_count));
  PUSH_FIELD_VALUE (cons, "static_field_count",
		    build_int_cst (NULL_TREE, static_field_count));

  if (flag_indirect_dispatch)
    PUSH_FIELD_VALUE (cons, "vtable", null_pointer_node);
  else
    PUSH_FIELD_VALUE (cons, "vtable",
		      dtable_decl == NULL_TREE ? null_pointer_node
		      : build2 (PLUS_EXPR, dtable_ptr_type,
				build1 (ADDR_EXPR, dtable_ptr_type,
					dtable_decl),
				dtable_start_offset));
  if (TYPE_OTABLE_METHODS (type) == NULL_TREE)
    {
      PUSH_FIELD_VALUE (cons, "otable", null_pointer_node);
      PUSH_FIELD_VALUE (cons, "otable_syms", null_pointer_node);
    }
  else
    {
      PUSH_FIELD_VALUE (cons, "otable",
			build1 (ADDR_EXPR, otable_ptr_type, TYPE_OTABLE_DECL (type)));
      PUSH_FIELD_VALUE (cons, "otable_syms",
			build1 (ADDR_EXPR, symbols_array_ptr_type,
				TYPE_OTABLE_SYMS_DECL (type)));
      TREE_CONSTANT (TYPE_OTABLE_DECL (type)) = 1;
      TREE_INVARIANT (TYPE_OTABLE_DECL (type)) = 1;
    }
  if (TYPE_ATABLE_METHODS(type) == NULL_TREE)
    {
      PUSH_FIELD_VALUE (cons, "atable", null_pointer_node);
      PUSH_FIELD_VALUE (cons, "atable_syms", null_pointer_node);
    }
  else
    {
      PUSH_FIELD_VALUE (cons, "atable",
			build1 (ADDR_EXPR, atable_ptr_type, TYPE_ATABLE_DECL (type)));
      PUSH_FIELD_VALUE (cons, "atable_syms",
			build1 (ADDR_EXPR, symbols_array_ptr_type,
				TYPE_ATABLE_SYMS_DECL (type)));
      TREE_CONSTANT (TYPE_ATABLE_DECL (type)) = 1;
      TREE_INVARIANT (TYPE_ATABLE_DECL (type)) = 1;
    }
   if (TYPE_ITABLE_METHODS(type) == NULL_TREE)
    {
      PUSH_FIELD_VALUE (cons, "itable", null_pointer_node);
      PUSH_FIELD_VALUE (cons, "itable_syms", null_pointer_node);
    }
  else
    {
      PUSH_FIELD_VALUE (cons, "itable",
			build1 (ADDR_EXPR, itable_ptr_type, TYPE_ITABLE_DECL (type)));
      PUSH_FIELD_VALUE (cons, "itable_syms",
			build1 (ADDR_EXPR, symbols_array_ptr_type,
				TYPE_ITABLE_SYMS_DECL (type)));
      TREE_CONSTANT (TYPE_ITABLE_DECL (type)) = 1;
      TREE_INVARIANT (TYPE_ITABLE_DECL (type)) = 1;
    }
 
  PUSH_FIELD_VALUE (cons, "catch_classes",
		    build1 (ADDR_EXPR, ptr_type_node, TYPE_CTABLE_DECL (type))); 
  PUSH_FIELD_VALUE (cons, "interfaces", interfaces);
  PUSH_FIELD_VALUE (cons, "loader", null_pointer_node);
  PUSH_FIELD_VALUE (cons, "interface_count",
		    build_int_cst (NULL_TREE, interface_len));
  PUSH_FIELD_VALUE 
    (cons, "state",
     convert (byte_type_node,
	      build_int_cst (NULL_TREE,
			     flag_indirect_dispatch
			     ? JV_STATE_PRELOADING
			     : JV_STATE_COMPILED)));

  PUSH_FIELD_VALUE (cons, "thread", null_pointer_node);
  PUSH_FIELD_VALUE (cons, "depth", integer_zero_node);
  PUSH_FIELD_VALUE (cons, "ancestors", null_pointer_node);
  PUSH_FIELD_VALUE (cons, "idt", null_pointer_node);
  PUSH_FIELD_VALUE (cons, "arrayclass", null_pointer_node);
  PUSH_FIELD_VALUE (cons, "protectionDomain", null_pointer_node);

  {
    tree assertion_table_ref;
    if (TYPE_ASSERTIONS (type) == NULL)
      assertion_table_ref = null_pointer_node;
    else
      assertion_table_ref = build1 (ADDR_EXPR, 
				    build_pointer_type (assertion_table_type),
				    emit_assertion_table (type));
    
    PUSH_FIELD_VALUE (cons, "assertion_table", assertion_table_ref);
  }

  PUSH_FIELD_VALUE (cons, "hack_signers", null_pointer_node);
  PUSH_FIELD_VALUE (cons, "chain", null_pointer_node);
  PUSH_FIELD_VALUE (cons, "aux_info", null_pointer_node);
  PUSH_FIELD_VALUE (cons, "engine", null_pointer_node);

  FINISH_RECORD_CONSTRUCTOR (cons);

  DECL_INITIAL (decl) = cons;
  
  /* Hash synchronization requires at least 64-bit alignment. */
  if (flag_hash_synchronization && POINTER_SIZE < 64)
    DECL_ALIGN (decl) = 64; 
  
  rest_of_decl_compilation (decl, 1, 0);
  
  TYPE_OTABLE_DECL (type) = NULL_TREE;
  TYPE_ATABLE_DECL (type) = NULL_TREE;
  TYPE_CTABLE_DECL (type) = NULL_TREE;
}

void
finish_class (void)
{
  if (TYPE_VERIFY_METHOD (output_class))
    {
      tree verify_method = TYPE_VERIFY_METHOD (output_class);
      DECL_SAVED_TREE (verify_method) 
	= add_stmt_to_compound (DECL_SAVED_TREE (verify_method), void_type_node,
				build (RETURN_EXPR, void_type_node, NULL));
      java_genericize (verify_method);
      cgraph_finalize_function (verify_method, false);
      TYPE_ASSERTIONS (current_class) = NULL;
    }

  java_expand_catch_classes (current_class);

  current_function_decl = NULL_TREE;
  make_class_data (current_class);
  register_class ();
  rest_of_decl_compilation (TYPE_NAME (current_class), 1, 0);
}

/* Return 2 if CLASS is compiled by this compilation job;
   return 1 if CLASS can otherwise be assumed to be compiled;
   return 0 if we cannot assume that CLASS is compiled.
   Returns 1 for primitive and 0 for array types.  */
int
is_compiled_class (tree class)
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
build_dtable_decl (tree type)
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
push_super_field (tree this_class, tree super_class)
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
maybe_layout_super_class (tree super_class, tree this_class)
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
#ifdef USE_MAPPED_LOCATION
	      this_wrap = build_expr_wfl (this_class,
					  DECL_SOURCE_LOCATION (this_decl));
#else
	      this_wrap = build_expr_wfl (this_class,
					  DECL_SOURCE_FILE (this_decl),
					  DECL_SOURCE_LINE (this_decl), 0);
#endif
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
layout_class (tree this_class)
{
  tree super_class = CLASSTYPE_SUPER (this_class);
  tree field;

  class_list = tree_cons (this_class, NULL_TREE, class_list);
  if (CLASS_BEING_LAIDOUT (this_class))
    {
      char buffer [1024];
      char *report;
      tree current;

      sprintf (buffer, " with '%s'",
	       IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (this_class))));
      obstack_grow (&temporary_obstack, buffer, strlen (buffer));

      for (current = TREE_CHAIN (class_list); current; 
	   current = TREE_CHAIN (current))
	{
	  tree decl = TYPE_NAME (TREE_PURPOSE (current));
	  sprintf (buffer, "\n  which inherits from '%s' (%s:%d)",
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

  /* Also recursively load/layout any superinterfaces, but only if
     class was loaded from bytecode.  The source parser will take care
     of this itself.  */
  if (!CLASS_FROM_SOURCE_P (this_class))
    {
      int i;
            if (TYPE_BINFO (this_class))
	{
	  for (i = BINFO_N_BASE_BINFOS (TYPE_BINFO (this_class)) - 1; i > 0; i--)
	    {
	      tree binfo = BINFO_BASE_BINFO (TYPE_BINFO (this_class), i);
	      tree super_interface = BINFO_TYPE (binfo);
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

  /* Convert the size back to an SI integer value.  */
  TYPE_SIZE_UNIT (this_class) =
    fold (convert (int_type_node, TYPE_SIZE_UNIT (this_class)));

  CLASS_BEING_LAIDOUT (this_class) = 0;
  class_list = TREE_CHAIN (class_list);
}

static void
add_miranda_methods (tree base_class, tree search_class)
{
  int i;
  tree binfo, base_binfo;

  if (!CLASS_PARSED_P (search_class))
    load_class (search_class, 1);
  
  for (binfo = TYPE_BINFO (search_class), i = 1;
       BINFO_BASE_ITERATE (binfo, i, base_binfo); i++)
    {
      tree method_decl;
      tree elt = BINFO_TYPE (base_binfo);

      /* FIXME: This is totally bogus.  We should not be handling
	 Miranda methods at all if we're using the BC ABI.  */
      if (TYPE_DUMMY (elt))
	continue;

      /* Ensure that interface methods are seen in declared order.  */
      if (!CLASS_LOADED_P (elt))
	load_class (elt, 1);
      layout_class_methods (elt);

      /* All base classes will have been laid out at this point, so the order 
         will be correct.  This code must match similar layout code in the 
         runtime.  */
      for (method_decl = TYPE_METHODS (elt);
	   method_decl; method_decl = TREE_CHAIN (method_decl))
	{
	  tree sig, override;

	  /* An interface can have <clinit>.  */
	  if (ID_CLINIT_P (DECL_NAME (method_decl)))
	    continue;

	  sig = build_java_argument_signature (TREE_TYPE (method_decl));
	  override = lookup_argument_method (base_class,
					     DECL_NAME (method_decl), sig);
	  if (override == NULL_TREE)
	    {
	      /* Found a Miranda method.  Add it.  */
	      tree new_method;
	      sig = build_java_signature (TREE_TYPE (method_decl));
	      new_method
		= add_method (base_class,
			      get_access_flags_from_decl (method_decl),
			      DECL_NAME (method_decl), sig);
	      METHOD_INVISIBLE (new_method) = 1;
	    }
	}

      /* Try superinterfaces.  */
      add_miranda_methods (base_class, elt);
    }
}

void
layout_class_methods (tree this_class)
{
  tree method_decl, dtable_count;
  tree super_class, type_name;

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

  type_name = TYPE_NAME (this_class);
  if (!flag_indirect_dispatch
      && (CLASS_ABSTRACT (type_name) || CLASS_INTERFACE (type_name)))
    {
      /* An abstract class can have methods which are declared only in
	 an implemented interface.  These are called "Miranda
	 methods".  We make a dummy method entry for such methods
	 here.  */
      add_miranda_methods (this_class, this_class);
    }

  TYPE_METHODS (this_class) = nreverse (TYPE_METHODS (this_class));

  for (method_decl = TYPE_METHODS (this_class);
       method_decl; method_decl = TREE_CHAIN (method_decl))
    dtable_count = layout_class_method (this_class, super_class,
					method_decl, dtable_count);

  TYPE_NVIRTUALS (this_class) = dtable_count;
}

/* Return the index of METHOD in INTERFACE.  This index begins at 1 and is used as an
   argument for _Jv_LookupInterfaceMethodIdx(). */
int
get_interface_method_index (tree method, tree interface)
{
  tree meth;
  int i = 1;

  for (meth = TYPE_METHODS (interface); ; meth = TREE_CHAIN (meth), i++)
    {
      if (meth == method)
	return i;
      if (meth == NULL_TREE)
	abort ();
    }
}

/* Lay METHOD_DECL out, returning a possibly new value of
   DTABLE_COUNT. Also mangle the method's name. */

tree
layout_class_method (tree this_class, tree super_class,
		     tree method_decl, tree dtable_count)
{
  tree method_name = DECL_NAME (method_decl);

  TREE_PUBLIC (method_decl) = 1;
  /* Considered external until we know what classes are being
     compiled into this object file.  */
  DECL_EXTERNAL (method_decl) = 1;

  /* This is a good occasion to mangle the method's name */
  SET_DECL_ASSEMBLER_NAME (method_decl,
			   java_mangle_decl (&temporary_obstack, 
					     method_decl));
  /* We don't generate a RTL for the method if it's abstract, or if
     it's an interface method that isn't clinit. */
  if (! METHOD_ABSTRACT (method_decl) 
      || (CLASS_INTERFACE (TYPE_NAME (this_class)) 
	  && (DECL_CLINIT_P (method_decl))))
    make_decl_rtl (method_decl);

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
  else if (! METHOD_STATIC (method_decl))
    {
      tree method_sig =
	build_java_argument_signature (TREE_TYPE (method_decl));
      bool method_override = false;
      tree super_method = lookup_argument_method (super_class, method_name,
						  method_sig);
      if (super_method != NULL_TREE
	  && ! METHOD_DUMMY (super_method))
        {
	  method_override = true;
	  if (! METHOD_PUBLIC (super_method) && 
	      ! METHOD_PROTECTED (super_method))
	    {
	      /* Don't override private method, or default-access method in 
		 another package.  */
	      if (METHOD_PRIVATE (super_method) ||
		  ! in_same_package (TYPE_NAME (this_class), 
				     TYPE_NAME (super_class)))
		method_override = false;
	   }
	}
      if (method_override)
	{
	  tree method_index = get_method_index (super_method);
	  set_method_index (method_decl, method_index);
	  if (method_index == NULL_TREE 
	      && ! flag_indirect_dispatch
	      && !CLASS_FROM_SOURCE_P (this_class)
	      && ! DECL_ARTIFICIAL (super_method))
	    error ("%Jnon-static method '%D' overrides static method",
                   method_decl, method_decl);
	}
      else if (this_class == object_type_node
	       && (METHOD_FINAL (method_decl)
		   || METHOD_PRIVATE (method_decl)))
	{
	  /* We don't generate vtable entries for final Object
	     methods.  This is simply to save space, since every
	     object would otherwise have to define them.  */
	}
      else if (! METHOD_PRIVATE (method_decl)
	       && dtable_count)
	{
	  /* We generate vtable entries for final methods because they
	     may one day be changed to non-final.  */
	  set_method_index (method_decl, dtable_count);
	  dtable_count = fold (build2 (PLUS_EXPR, integer_type_node,
				       dtable_count, integer_one_node));
	}
    }

  return dtable_count;
}

void
register_class (void)
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
   a list of pointers to classes which get registered during constructor
   invocation time.

   The fallback mechanism is to add statements to *LIST_P to call
   _Jv_RegisterClass for each class in this file.  These statements will
   be added to a static constructor function for this translation unit.  */

void
emit_register_classes (tree *list_p)
{
  if (registered_class == NULL)
    return;

  /* TARGET_USE_JCR_SECTION defaults to 1 if SUPPORTS_WEAK and
     TARGET_ASM_NAMED_SECTION, else 0.  Some targets meet those conditions
     but lack suitable crtbegin/end objects or linker support.  These
     targets can overide the default in tm.h to use the fallback mechanism.  */
  if (TARGET_USE_JCR_SECTION)
    {
#ifdef JCR_SECTION_NAME
      tree t;
      named_section_flags (JCR_SECTION_NAME, SECTION_WRITE);
      assemble_align (POINTER_SIZE);
      for (t = registered_class; t; t = TREE_CHAIN (t))
	assemble_integer (XEXP (DECL_RTL (t), 0),
			  POINTER_SIZE / BITS_PER_UNIT, POINTER_SIZE, 1);
#else
      /* A target has defined TARGET_USE_JCR_SECTION, but doesn't have a
	 JCR_SECTION_NAME.  */
      abort ();
#endif
    }
  else
    {
      tree klass, t, register_class_fn;

      t = build_function_type_list (void_type_node, class_ptr_type, NULL);
      t = build_decl (FUNCTION_DECL, get_identifier ("_Jv_RegisterClass"), t);
      TREE_PUBLIC (t) = 1;
      DECL_EXTERNAL (t) = 1;
      register_class_fn = t;

      for (klass = registered_class; klass; klass = TREE_CHAIN (klass))
	{
	  t = build_fold_addr_expr (klass);
	  t = tree_cons (NULL, t, NULL);
	  t = build_function_call_expr (register_class_fn, t);
	  append_to_statement_list (t, list_p);
	}
    }
}

/* Make a symbol_type (_Jv_MethodSymbol) node for DECL. */

static tree
build_symbol_entry (tree decl)
{
  tree clname, name, signature, sym;
  clname = build_utf8_ref (DECL_NAME (TYPE_NAME (DECL_CONTEXT (decl))));
  /* ???  Constructors are given the name foo.foo all the way through
     the compiler, but in the method table they're all renamed
     foo.<init>.  So, we have to do the same here unless we want an
     unresolved reference at runtime.  */
  name = build_utf8_ref ((TREE_CODE (decl) == FUNCTION_DECL 
			  && DECL_CONSTRUCTOR_P (decl))
			 ? init_identifier_node
			 : DECL_NAME (decl));
  signature = build_java_signature (TREE_TYPE (decl));
  signature = build_utf8_ref (unmangle_classname 
			      (IDENTIFIER_POINTER (signature),
			       IDENTIFIER_LENGTH (signature)));
      
  START_RECORD_CONSTRUCTOR (sym, symbol_type);
  PUSH_FIELD_VALUE (sym, "clname", clname);
  PUSH_FIELD_VALUE (sym, "name", name);
  PUSH_FIELD_VALUE (sym, "signature", signature);
  FINISH_RECORD_CONSTRUCTOR (sym);
  TREE_CONSTANT (sym) = 1;
  TREE_INVARIANT (sym) = 1;

  return sym;
} 

/* Emit a symbol table: used by -findirect-dispatch.  */

tree
emit_symbol_table (tree name, tree the_table, tree decl_list,
                   tree the_syms_decl, tree the_array_element_type,
		   int element_size)
{
  tree method_list, method, table, list, null_symbol;
  tree table_size, the_array_type;
  int index;
  
  /* Only emit a table if this translation unit actually made any
     references via it. */
  if (decl_list == NULL_TREE)
    return the_table;

  /* Build a list of _Jv_MethodSymbols for each entry in otable_methods. */
  index = 0;
  method_list = decl_list;
  list = NULL_TREE;  
  while (method_list != NULL_TREE)
    {
      method = TREE_VALUE (method_list);
      list = tree_cons (NULL_TREE, build_symbol_entry (method), list);
      method_list = TREE_CHAIN (method_list);
      index++;
    }

  /* Terminate the list with a "null" entry. */
  START_RECORD_CONSTRUCTOR (null_symbol, symbol_type);
  PUSH_FIELD_VALUE (null_symbol, "clname", null_pointer_node);
  PUSH_FIELD_VALUE (null_symbol, "name", null_pointer_node);
  PUSH_FIELD_VALUE (null_symbol, "signature", null_pointer_node);
  FINISH_RECORD_CONSTRUCTOR (null_symbol);
  TREE_CONSTANT (null_symbol) = 1;  
  TREE_INVARIANT (null_symbol) = 1;  
  list = tree_cons (NULL_TREE, null_symbol, list);

  /* Put the list in the right order and make it a constructor. */
  list = nreverse (list);
  table = build_constructor (symbols_array_type, list);  

  /* Make it the initial value for otable_syms and emit the decl. */
  DECL_INITIAL (the_syms_decl) = table;
  DECL_ARTIFICIAL (the_syms_decl) = 1;
  DECL_IGNORED_P (the_syms_decl) = 1;
  rest_of_decl_compilation (the_syms_decl, 1, 0);
  
  /* Now that its size is known, redefine the table as an
     uninitialized static array of INDEX + 1 elements. The extra entry
     is used by the runtime to track whether the table has been
     initialized. */
  table_size 
    = build_index_type (build_int_cst (NULL_TREE, index * element_size + 1));
  the_array_type = build_array_type (the_array_element_type, table_size);
  the_table = build_decl (VAR_DECL, name, the_array_type);
  TREE_STATIC (the_table) = 1;
  TREE_READONLY (the_table) = 1;  
  rest_of_decl_compilation (the_table, 1, 0);

  return the_table;
}

/* Make an entry for the catch_classes list.  */
tree
make_catch_class_record (tree catch_class, tree classname)
{
  tree entry;
  tree type = TREE_TYPE (TREE_TYPE (TYPE_CTABLE_DECL (output_class)));
  START_RECORD_CONSTRUCTOR (entry, type);
  PUSH_FIELD_VALUE (entry, "address", catch_class);
  PUSH_FIELD_VALUE (entry, "classname", classname);
  FINISH_RECORD_CONSTRUCTOR (entry);
  return entry;
}


/* Generate the list of Throwable classes that are caught by exception
   handlers in this class.  */
tree 
emit_catch_table (tree this_class)
{
  tree table, table_size, array_type;
  TYPE_CATCH_CLASSES (this_class) =
    tree_cons (NULL,
	       make_catch_class_record (null_pointer_node, null_pointer_node),
	       TYPE_CATCH_CLASSES (this_class));
  TYPE_CATCH_CLASSES (this_class) = nreverse (TYPE_CATCH_CLASSES (this_class));
  TYPE_CATCH_CLASSES (this_class) = 
    tree_cons (NULL,
	       make_catch_class_record (null_pointer_node, null_pointer_node),
	       TYPE_CATCH_CLASSES (this_class));
  table_size = build_index_type
    (build_int_cst (NULL_TREE,
		    list_length (TYPE_CATCH_CLASSES (this_class))));
  array_type 
    = build_array_type (TREE_TYPE (TREE_TYPE (TYPE_CTABLE_DECL (this_class))),
			table_size);
  table = 
    build_decl (VAR_DECL, DECL_NAME (TYPE_CTABLE_DECL (this_class)), array_type);
  DECL_INITIAL (table) = 
    build_constructor (array_type, TYPE_CATCH_CLASSES (this_class));
  TREE_STATIC (table) = 1;
  TREE_READONLY (table) = 1;  
  DECL_IGNORED_P (table) = 1;
  rest_of_decl_compilation (table, 1, 0);
  return table;
}

/* Given a type, return the signature used by
   _Jv_FindClassFromSignature() in libgcj.  This isn't exactly the
   same as build_java_signature() because we want the canonical array
   type.  */

static tree
build_signature_for_libgcj (tree type)
{
  tree sig, ref;

  sig = build_java_signature (type);
  ref = build_utf8_ref (unmangle_classname (IDENTIFIER_POINTER (sig),
					    IDENTIFIER_LENGTH (sig)));
  return ref;
}

/* Add an entry to the type assertion table. Callback used during hashtable
   traversal.  */

static int
add_assertion_table_entry (void **htab_entry, void *ptr)
{
  tree entry;
  tree code_val, op1_utf8, op2_utf8;
  tree *list = (tree *) ptr;
  type_assertion *as = (type_assertion *) *htab_entry;

  code_val = build_int_cst (NULL_TREE, as->assertion_code);

  if (as->op1 == NULL_TREE)
    op1_utf8 = null_pointer_node;
  else
    op1_utf8 = build_signature_for_libgcj (as->op1);

  if (as->op2 == NULL_TREE)
    op2_utf8 = null_pointer_node;
  else
    op2_utf8 = build_signature_for_libgcj (as->op2);
  
  START_RECORD_CONSTRUCTOR (entry, assertion_entry_type);
  PUSH_FIELD_VALUE (entry, "assertion_code", code_val);
  PUSH_FIELD_VALUE (entry, "op1", op1_utf8);
  PUSH_FIELD_VALUE (entry, "op2", op2_utf8);
  FINISH_RECORD_CONSTRUCTOR (entry);
  
  *list = tree_cons (NULL_TREE, entry, *list);
  return true;
}

/* Generate the type assertion table for CLASS, and return its DECL.  */

static tree
emit_assertion_table (tree class)
{
  tree null_entry, ctor, table_decl;
  tree list = NULL_TREE;
  htab_t assertions_htab = TYPE_ASSERTIONS (class);

  /* Iterate through the hash table.  */
  htab_traverse (assertions_htab, add_assertion_table_entry, &list);

  /* Finish with a null entry.  */
  START_RECORD_CONSTRUCTOR (null_entry, assertion_entry_type);
  PUSH_FIELD_VALUE (null_entry, "assertion_code", integer_zero_node);
  PUSH_FIELD_VALUE (null_entry, "op1", null_pointer_node);
  PUSH_FIELD_VALUE (null_entry, "op2", null_pointer_node);
  FINISH_RECORD_CONSTRUCTOR (null_entry);
  
  list = tree_cons (NULL_TREE, null_entry, list);
  
  /* Put the list in the right order and make it a constructor. */
  list = nreverse (list);
  ctor = build_constructor (assertion_table_type, list);

  table_decl = build_decl (VAR_DECL, mangled_classname ("_type_assert_", class),
			   assertion_table_type);

  TREE_STATIC (table_decl) = 1;
  TREE_READONLY (table_decl) = 1;
  TREE_CONSTANT (table_decl) = 1;
  DECL_IGNORED_P (table_decl) = 1;

  DECL_INITIAL (table_decl) = ctor;
  DECL_ARTIFICIAL (table_decl) = 1;
  rest_of_decl_compilation (table_decl, 1, 0);

  return table_decl;
}

void
init_class_processing (void)
{
  fields_ident = get_identifier ("fields");
  info_ident = get_identifier ("info");

  gcc_obstack_init (&temporary_obstack);
}

static hashval_t java_treetreehash_hash (const void *);
static int java_treetreehash_compare (const void *, const void *);

/* A hash table mapping trees to trees.  Used generally.  */

#define JAVA_TREEHASHHASH_H(t) (htab_hash_pointer (t))

static hashval_t
java_treetreehash_hash (const void *k_p)
{
  struct treetreehash_entry *k = (struct treetreehash_entry *) k_p;
  return JAVA_TREEHASHHASH_H (k->key);
}

static int
java_treetreehash_compare (const void * k1_p, const void * k2_p)
{
  struct treetreehash_entry * k1 = (struct treetreehash_entry *) k1_p;
  tree k2 = (tree) k2_p;
  return (k1->key == k2);
}

tree 
java_treetreehash_find (htab_t ht, tree t)
{
  struct treetreehash_entry *e;
  hashval_t hv = JAVA_TREEHASHHASH_H (t);
  e = htab_find_with_hash (ht, t, hv);
  if (e == NULL)
    return NULL;
  else
    return e->value;
}

tree *
java_treetreehash_new (htab_t ht, tree t)
{
  void **e;
  struct treetreehash_entry *tthe;
  hashval_t hv = JAVA_TREEHASHHASH_H (t);

  e = htab_find_slot_with_hash (ht, t, hv, INSERT);
  if (*e == NULL)
    {
      tthe = (*ht->alloc_f) (1, sizeof (*tthe));
      tthe->key = t;
      *e = tthe;
    }
  else
    tthe = (struct treetreehash_entry *) *e;
  return &tthe->value;
}

htab_t
java_treetreehash_create (size_t size, int gc)
{
  if (gc)
    return htab_create_ggc (size, java_treetreehash_hash,
			    java_treetreehash_compare, NULL);
  else
    return htab_create_alloc (size, java_treetreehash_hash,
			      java_treetreehash_compare, free, xcalloc, free);
}

/* Break down qualified IDENTIFIER into package and class-name components.
   For example, given SOURCE "pkg.foo.Bar", LEFT will be set to
   "pkg.foo", and RIGHT to "Bar". */

int
split_qualified_name (tree *left, tree *right, tree source)
{
  char *p, *base;
  int l = IDENTIFIER_LENGTH (source);

  base = alloca (l + 1);
  memcpy (base, IDENTIFIER_POINTER (source), l + 1);

  /* Breakdown NAME into REMAINDER . IDENTIFIER.  */
  p = base + l - 1;
  while (*p != '.' && p != base)
    p--;

  /* We didn't find a '.'. Return an error.  */
  if (p == base)
    return 1;

  *p = '\0';
  if (right)
    *right = get_identifier (p+1);
  *left = get_identifier (base);

  return 0;
}

/* Given two classes (TYPE_DECL) or class names (IDENTIFIER), return TRUE 
   if the classes are from the same package. */

int
in_same_package (tree name1, tree name2)
{
  tree tmp;
  tree pkg1;
  tree pkg2;

  if (TREE_CODE (name1) == TYPE_DECL)
    name1 = DECL_NAME (name1);
  if (TREE_CODE (name2) == TYPE_DECL)
    name2 = DECL_NAME (name2);

  if (QUALIFIED_P (name1) != QUALIFIED_P (name2))
    /* One in empty package. */
    return 0;

  if (QUALIFIED_P (name1) == 0 && QUALIFIED_P (name2) == 0)
    /* Both in empty package. */
    return 1;

  split_qualified_name (&pkg1, &tmp, name1);
  split_qualified_name (&pkg2, &tmp, name2);

  return (pkg1 == pkg2);
}

#include "gt-java-class.h"
