/* Functions related to building classes and their related objects.
   Copyright (C) 1996-2013 Free Software Foundation, Inc.

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
<http://www.gnu.org/licenses/>.

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

/* Written by Per Bothner <bothner@cygnus.com> */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "flags.h"
#include "java-tree.h"
#include "jcf.h"
#include "obstack.h"
#include "diagnostic-core.h"
#include "toplev.h"
#include "output.h" /* for switch_to_section and get_section */
#include "parse.h"
#include "function.h"
#include "ggc.h"
#include "cgraph.h"
#include "tree-iterator.h"
#include "target.h"

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
static tree build_symbol_entry (tree, tree);
static tree emit_assertion_table (tree);
static void register_class (void);

struct obstack temporary_obstack;

static const char *cyclic_inheritance_report;

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

static GTY(()) tree class_roots[4];
#define fields_ident class_roots[0]  /* get_identifier ("fields") */
#define info_ident class_roots[1]  /* get_identifier ("info") */
#define class_list class_roots[2]
#define class_dtable_decl class_roots[3]

static GTY(()) vec<tree, va_gc> *registered_class;

/* A tree that returns the address of the class$ of the class
   currently being compiled.  */
static GTY(()) tree this_classdollar;

/* A list of static class fields.  This is to emit proper debug
   info for them.  */
vec<tree, va_gc> *pending_static_fields;

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
      root = XNEW (class_flag_node);
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
      node = XNEW (class_flag_node);

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

#define DEFAULT_ENABLE_ASSERT (optimize == 0)

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
  char *buffer = (char *) alloca (i);

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
  tree result;
  tree ident = TYPE_NAME (type);
  if (TREE_CODE (ident) != IDENTIFIER_NODE)
    ident = DECL_NAME (ident);
  result = identifier_subst (ident, prefix, '.', '_', "");

  /* Replace any characters that aren't in the set [0-9a-zA-Z_$] with
     "_0xXX".  Class names containing such chracters are uncommon, but
     they do sometimes occur in class files.  Without this check,
     these names cause assembly errors.

     There is a possibility that a real class name could conflict with
     the identifier we generate, but it is unlikely and will
     immediately be detected as an assembler error.  At some point we
     should do something more elaborate (perhaps using the full
     unicode mangling scheme) in order to prevent such a conflict.  */
  {
    int i;
    const int len = IDENTIFIER_LENGTH (result);
    const char *p = IDENTIFIER_POINTER (result);
    int illegal_chars = 0;

    /* Make two passes over the identifier.  The first pass is merely
       to count illegal characters; we need to do this in order to
       allocate a buffer.  */
    for (i = 0; i < len; i++)
      {
	char c = p[i];
	illegal_chars += (! ISALNUM (c) && c != '_' && c != '$');
      }

    /* And the second pass, which is rarely executed, does the
       rewriting.  */
    if (illegal_chars != 0)
      {
	char *buffer = (char *) alloca (illegal_chars * 4 + len + 1);
	int j;

	for (i = 0, j = 0; i < len; i++)
	  {
	    char c = p[i];
	    if (! ISALNUM (c) && c != '_' && c != '$')
	      {
		buffer[j++] = '_';
		sprintf (&buffer[j], "0x%02x", c);
		j += 4;
	      }
	    else
	      buffer[j++] = c;
	  }

	buffer[j] = 0;
	result = get_identifier (buffer);
      }
  }

  return result;
}

tree
make_class (void)
{
  tree type;
  type = make_node (RECORD_TYPE);
  /* Unfortunately we must create the binfo here, so that class
     loading works.  */
  TYPE_BINFO (type) = make_tree_binfo (0);
  MAYBE_CREATE_TYPE_TYPE_LANG_SPECIFIC (type);
  TYPE_CATCH_CLASSES (type) = NULL;
  /* Push a dummy entry; we can't call make_catch_class_record here
     because other infrastructure may not be set up yet.  We'll come
     back and fill it in later once said infrastructure is
     initialized.  */
  CONSTRUCTOR_APPEND_ELT (TYPE_CATCH_CLASSES (type), NULL_TREE, NULL_TREE);

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
  const char *type_name = IDENTIFIER_POINTER (mangled_classname ("", TYPE)); \
  char *buf = (char *) alloca (strlen (type_name)			\
                               + strlen (#NAME "_syms_") + 1);		\
  tree decl;								\
									\
  sprintf (buf, #NAME "_%s", type_name);				\
  TYPE_## TABLE ##_DECL (type) = decl =					\
    build_decl (input_location, VAR_DECL, get_identifier (buf), TABLE_TYPE); \
  DECL_EXTERNAL (decl) = 1;						\
  TREE_STATIC (decl) = 1;						\
  TREE_READONLY (decl) = 1;						\
  TREE_CONSTANT (decl) = 1;						\
  DECL_IGNORED_P (decl) = 1;						\
  /* Mark the table as belonging to this class.  */			\
  pushdecl (decl);							\
  MAYBE_CREATE_VAR_LANG_DECL_SPECIFIC (decl);				\
  DECL_OWNER (decl) = TYPE;						\
  sprintf (buf, #NAME "_syms_%s", type_name);				\
  TYPE_## TABLE ##_SYMS_DECL (TYPE) =					\
    build_decl (input_location, VAR_DECL, get_identifier (buf), symbols_array_type); \
  TREE_STATIC (TYPE_## TABLE ##_SYMS_DECL (TYPE)) = 1;			\
  TREE_CONSTANT (TYPE_## TABLE ##_SYMS_DECL (TYPE)) = 1;		\
  DECL_IGNORED_P (TYPE_## TABLE ##_SYMS_DECL (TYPE)) = 1;		\
}									\
while (0)

/* Given a class, create the DECLs for all its associated indirect
   dispatch tables.  */
void
gen_indirect_dispatch_tables (tree type)
{
  const char *type_name = IDENTIFIER_POINTER (mangled_classname ("", type));
  {  
    tree field = NULL;
    char *buf = (char *) alloca (strlen (type_name)
				 + strlen ("_catch_classes_") + 1);
    tree catch_class_type = make_node (RECORD_TYPE);

    sprintf (buf, "_catch_classes_%s", type_name);
    PUSH_FIELD (input_location,
		catch_class_type, field, "address", utf8const_ptr_type);
    PUSH_FIELD (input_location,
		catch_class_type, field, "classname", ptr_type_node);
    FINISH_RECORD (catch_class_type);
    
    TYPE_CTABLE_DECL (type) 
      = build_decl (input_location, VAR_DECL, get_identifier (buf),
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
  CLASS_P (class_type) = 1;
  decl = build_decl (input_location, TYPE_DECL, class_name, class_type);
  TYPE_DECL_SUPPRESS_DEBUG (decl) = 1;

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

  if (total_supers)
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
  if (access_flags & ACC_ENUM)      CLASS_ENUM (class_decl) = 1;
  if (access_flags & ACC_SYNTHETIC) CLASS_SYNTHETIC (class_decl) = 1;
  if (access_flags & ACC_ANNOTATION) CLASS_ANNOTATION (class_decl) = 1;
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

      if (! CLASS_LOADED_P (type1))
	load_class (type1, 1);

      type1 = maybe_layout_super_class (CLASSTYPE_SUPER (type1), type1);
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

static tree
build_java_method_type (tree fntype, tree this_class, int access_flags)
{
  if (access_flags & ACC_STATIC)
    return fntype;
  fntype = build_method_type (this_class, fntype);

  /* We know that arg 1 of every nonstatic method is non-null; tell
     the back-end so.  */
  TYPE_ATTRIBUTES (fntype) = (tree_cons 
			      (get_identifier ("nonnull"),
			       tree_cons (NULL_TREE, 
					  build_int_cst (NULL_TREE, 1),
					  NULL_TREE),
			       TYPE_ATTRIBUTES (fntype)));
  return fntype;
}

void
java_hide_decl (tree decl ATTRIBUTE_UNUSED)
{
#ifdef HAVE_GAS_HIDDEN
  DECL_VISIBILITY (decl) = VISIBILITY_HIDDEN;
  DECL_VISIBILITY_SPECIFIED (decl) = 1;
#endif
}

tree
add_method_1 (tree this_class, int access_flags, tree name, tree function_type)
{
  tree method_type, fndecl;

  method_type = build_java_method_type (function_type,
					this_class, access_flags);

  fndecl = build_decl (input_location, FUNCTION_DECL, name, method_type);
  DECL_CONTEXT (fndecl) = this_class;

  DECL_LANG_SPECIFIC (fndecl)
    = ggc_alloc_cleared_lang_decl(sizeof (struct lang_decl));
  DECL_LANG_SPECIFIC (fndecl)->desc = LANG_DECL_FUNC;

  /* Initialize the static initializer test table.  */

  DECL_FUNCTION_INIT_TEST_TABLE (fndecl) = java_treetreehash_create (10);

  /* Initialize the initialized (static) class table. */
  if (access_flags & ACC_STATIC)
    DECL_FUNCTION_INITIALIZED_CLASS_TABLE (fndecl) =
      htab_create_ggc (50, htab_hash_pointer, htab_eq_pointer, NULL);

  DECL_CHAIN (fndecl) = TYPE_METHODS (this_class);
  TYPE_METHODS (this_class) = fndecl;

  /* If pointers to member functions use the least significant bit to
     indicate whether a function is virtual, ensure a pointer
     to this function will have that bit clear.  */
  if (TARGET_PTRMEMFUNC_VBIT_LOCATION == ptrmemfunc_vbit_in_pfn
      && !(access_flags & ACC_STATIC)
      && DECL_ALIGN (fndecl) < 2 * BITS_PER_UNIT)
    DECL_ALIGN (fndecl) = 2 * BITS_PER_UNIT;

  /* Notice that this is a finalizer and update the class type
     accordingly. This is used to optimize instance allocation. */
  if (name == finalize_identifier_node
      && TREE_TYPE (function_type) == void_type_node
      && TREE_VALUE (TYPE_ARG_TYPES (function_type)) == void_type_node)
    HAS_FINALIZER_P (this_class) = 1;

  if (access_flags & ACC_PUBLIC) METHOD_PUBLIC (fndecl) = 1;
  if (access_flags & ACC_PROTECTED) METHOD_PROTECTED (fndecl) = 1;
  if (access_flags & ACC_PRIVATE)
    METHOD_PRIVATE (fndecl) = 1;
  if (access_flags & ACC_NATIVE)
    {
      METHOD_NATIVE (fndecl) = 1;
      DECL_EXTERNAL (fndecl) = 1;
    }
  else
    /* FNDECL is external unless we are compiling it into this object
       file.  */
    DECL_EXTERNAL (fndecl) = CLASS_FROM_CURRENTLY_COMPILED_P (this_class) == 0;
  if (access_flags & ACC_STATIC) 
    METHOD_STATIC (fndecl) = 1;
  if (access_flags & ACC_FINAL) 
    METHOD_FINAL (fndecl) = 1;
  if (access_flags & ACC_SYNCHRONIZED) METHOD_SYNCHRONIZED (fndecl) = 1;
  if (access_flags & ACC_ABSTRACT) METHOD_ABSTRACT (fndecl) = 1;
  if (access_flags & ACC_STRICT) METHOD_STRICTFP (fndecl) = 1;
  if (access_flags & ACC_SYNTHETIC) DECL_ARTIFICIAL (fndecl) = 1;
  if (access_flags & ACC_BRIDGE) METHOD_BRIDGE (fndecl) = 1;
  if (access_flags & ACC_VARARGS) METHOD_VARARGS (fndecl) = 1;
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
add_field (tree klass, tree name, tree field_type, int flags)
{
  int is_static = (flags & ACC_STATIC) != 0;
  tree field;
  field = build_decl (input_location,
		      is_static ? VAR_DECL : FIELD_DECL, name, field_type);
  DECL_CHAIN (field) = TYPE_FIELDS (klass);
  TYPE_FIELDS (klass) = field;
  DECL_CONTEXT (field) = klass;
  MAYBE_CREATE_VAR_LANG_DECL_SPECIFIC (field);

  if (flags & ACC_PUBLIC) FIELD_PUBLIC (field) = 1;
  if (flags & ACC_PROTECTED) FIELD_PROTECTED (field) = 1;
  if (flags & ACC_PRIVATE) FIELD_PRIVATE (field) = 1;
  if (flags & ACC_FINAL) FIELD_FINAL (field) = 1;
  if (flags & ACC_VOLATILE) 
    {
      FIELD_VOLATILE (field) = 1;
      TREE_THIS_VOLATILE (field) = 1;
    }
  if (flags & ACC_TRANSIENT) FIELD_TRANSIENT (field) = 1;
  if (flags & ACC_ENUM) FIELD_ENUM (field) = 1;
  if (flags & ACC_SYNTHETIC) FIELD_SYNTHETIC (field) = 1;
  if (is_static)
    {
      FIELD_STATIC (field) = 1;
      /* Always make field externally visible.  This is required so
	 that native methods can always access the field.  */
      TREE_PUBLIC (field) = 1;
      /* Hide everything that shouldn't be visible outside a DSO.  */
      if (flag_indirect_classes
	  || (FIELD_PRIVATE (field)))
	java_hide_decl (field);
      /* Considered external unless we are compiling it into this
	 object file.  */
      DECL_EXTERNAL (field) = (is_compiled_class (klass) != 2);
      if (!DECL_EXTERNAL (field))
	vec_safe_push (pending_static_fields, field);
    }

  return field;
}

/* Associate a constant value CONSTANT with VAR_DECL FIELD. */

void
set_constant_value (tree field, tree constant)
{
  if (field == NULL_TREE)
    warning (OPT_Wattributes,
	     "misplaced ConstantValue attribute (not in any field)");
  else if (DECL_INITIAL (field) != NULL_TREE)
    warning (OPT_Wattributes,
	     "duplicate ConstantValue attribute for field '%s'",
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
    }
}

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

tree
build_utf8_ref (tree name)
{
  const char * name_ptr = IDENTIFIER_POINTER (name);
  int name_len = IDENTIFIER_LENGTH (name), name_pad;
  char buf[60];
  tree ctype, field = NULL_TREE, str_type, cinit, string;
  static int utf8_count = 0;
  int name_hash;
  tree ref = IDENTIFIER_UTF8_REF (name);
  tree decl;
  vec<constructor_elt, va_gc> *v = NULL;
  if (ref != NULL_TREE)
    return ref;

  ctype = make_node (RECORD_TYPE);
  /* '\0' byte plus padding to utf8const_type's alignment.  */
  name_pad = TYPE_ALIGN_UNIT (utf8const_type)
	     - (name_len & (TYPE_ALIGN_UNIT (utf8const_type) - 1));
  str_type = build_prim_array_type (unsigned_byte_type_node,
				    name_len + name_pad);
  PUSH_FIELD (input_location, ctype, field, "hash", unsigned_short_type_node);
  PUSH_FIELD (input_location,
	      ctype, field, "length", unsigned_short_type_node);
  PUSH_FIELD (input_location, ctype, field, "data", str_type);
  FINISH_RECORD (ctype);
  START_RECORD_CONSTRUCTOR (v, ctype);
  name_hash = hashUtf8String (name_ptr, name_len) & 0xFFFF;
  PUSH_FIELD_VALUE (v, "hash", build_int_cst (NULL_TREE, name_hash));
  PUSH_FIELD_VALUE (v, "length", build_int_cst (NULL_TREE, name_len));
  string = build_string (name_len, name_ptr);
  TREE_TYPE (string) = str_type;
  PUSH_FIELD_VALUE (v, "data", string);
  FINISH_RECORD_CONSTRUCTOR (cinit, v, ctype);
  TREE_CONSTANT (cinit) = 1;

  /* Generate a unique-enough identifier.  */
  sprintf(buf, "_Utf%d", ++utf8_count);

  decl = build_decl (input_location,
		     VAR_DECL, get_identifier (buf), utf8const_type);
  TREE_STATIC (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  DECL_IGNORED_P (decl) = 1;
  TREE_READONLY (decl) = 1;
  TREE_THIS_VOLATILE (decl) = 0;
  DECL_INITIAL (decl) = cinit;
  DECL_USER_ALIGN (decl) = 1;

  if (HAVE_GAS_SHF_MERGE)
    {
      int decl_size;
      /* Ensure decl_size is a multiple of utf8const_type's alignment. */
      decl_size = name_len + 4 + name_pad;
      if (flag_merge_constants && decl_size < 256)
	{
	  char buf[32];
	  int flags = (SECTION_OVERRIDE
		       | SECTION_MERGE | (SECTION_ENTSIZE & decl_size));
	  sprintf (buf, ".rodata.jutf8.%d", decl_size);
	  switch_to_section (get_section (buf, flags, NULL));
	  DECL_SECTION_NAME (decl) = build_string (strlen (buf), buf);
	}
    }

  layout_decl (decl, 0);
  DECL_SIZE (decl) = TYPE_SIZE (ctype);
  DECL_SIZE_UNIT (decl) = TYPE_SIZE_UNIT (ctype);
  pushdecl (decl);
  rest_of_decl_compilation (decl, global_bindings_p (), 0);
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

static tree
build_static_class_ref (tree type)
{
  tree decl_name, decl, ref;

  if (TYPE_SIZE (type) == error_mark_node)
    return null_pointer_node;
  decl_name = identifier_subst (DECL_NAME (TYPE_NAME (type)),
				"", '/', '/', ".class$$");
  decl = IDENTIFIER_GLOBAL_VALUE (decl_name);
  if (decl == NULL_TREE)
    {
      decl = build_decl (input_location, VAR_DECL, decl_name, class_type_node);
      TREE_STATIC (decl) = 1;
      if (! flag_indirect_classes)
	{
	  TREE_PUBLIC (decl) = 1;
	  if (CLASS_PRIVATE (TYPE_NAME (type)))
	    java_hide_decl (decl);
	}
      DECL_IGNORED_P (decl) = 1;
      DECL_ARTIFICIAL (decl) = 1;
      if (is_compiled_class (type) == 1)
	DECL_EXTERNAL (decl) = 1;
      MAYBE_CREATE_VAR_LANG_DECL_SPECIFIC (decl);
      DECL_CLASS_FIELD_P (decl) = 1;
      DECL_CONTEXT (decl) = type;

      /* ??? We want to preserve the DECL_CONTEXT we set just above,
	 that that means not calling pushdecl_top_level.  */
      IDENTIFIER_GLOBAL_VALUE (decl_name) = decl;
    }

  ref = build1 (ADDR_EXPR, class_ptr_type, decl);
  return ref;
}

static tree
build_classdollar_field (tree type)
{
  tree decl_name = identifier_subst (DECL_NAME (TYPE_NAME (type)),
				     "", '/', '/', ".class$");
  tree decl = IDENTIFIER_GLOBAL_VALUE (decl_name);

  if (decl == NULL_TREE)
    {
      decl 
	= build_decl (input_location,
		      VAR_DECL, decl_name, 
		      (build_type_variant 
		       (build_pointer_type 
			(build_type_variant (class_type_node, 
					     /* const */ 1, 0)),
			/* const */ 1, 0)));
      TREE_STATIC (decl) = 1;
      TREE_CONSTANT (decl) = 1;
      TREE_READONLY (decl) = 1;
      TREE_PUBLIC (decl) = 1;
      java_hide_decl (decl);
      DECL_IGNORED_P (decl) = 1;
      DECL_ARTIFICIAL (decl) = 1;
      MAYBE_CREATE_VAR_LANG_DECL_SPECIFIC (decl);
      IDENTIFIER_GLOBAL_VALUE (decl_name) = decl;
      DECL_CLASS_FIELD_P (decl) = 1;
      DECL_CONTEXT (decl) = type;
    }

  return decl;
}

/* Create a local variable that holds the current class$.  */

void
cache_this_class_ref (tree fndecl)
{
  if (optimize)
    {
      tree classdollar_field;
      if (flag_indirect_classes)
	classdollar_field = build_classdollar_field (output_class);
      else
	classdollar_field = build_static_class_ref (output_class);

      this_classdollar = build_decl (input_location,
				     VAR_DECL, NULL_TREE, 
				     TREE_TYPE (classdollar_field));
      
      java_add_local_var (this_classdollar);
      java_add_stmt (build2 (MODIFY_EXPR, TREE_TYPE (this_classdollar), 
			     this_classdollar, classdollar_field));
    }
  else
    this_classdollar = build_classdollar_field (output_class);

  /* Prepend class initialization for static methods reachable from
     other classes.  */
  if (METHOD_STATIC (fndecl)
      && (! METHOD_PRIVATE (fndecl)
          || INNER_CLASS_P (DECL_CONTEXT (fndecl)))
      && ! DECL_CLINIT_P (fndecl)
      && ! CLASS_INTERFACE (TYPE_NAME (DECL_CONTEXT (fndecl))))
    {
      tree init = build_call_expr (soft_initclass_node, 1,
				   this_classdollar);
      java_add_stmt (init);
    }
}

/* Remove the reference to the local variable that holds the current
   class$.  */

void
uncache_this_class_ref (tree fndecl ATTRIBUTE_UNUSED)
{
  this_classdollar = build_classdollar_field (output_class);
}

/* Build a reference to the class TYPE.
   Also handles primitive types and array types. */

tree
build_class_ref (tree type)
{
  int is_compiled = is_compiled_class (type);
  if (is_compiled)
    {
      tree ref, decl;
      if (TREE_CODE (type) == POINTER_TYPE)
	type = TREE_TYPE (type);

      if (flag_indirect_dispatch
	  && type != output_class
	  && TREE_CODE (type) == RECORD_TYPE)
	return build_indirect_class_ref (type);

      if (type == output_class && flag_indirect_classes)
	{
	  /* This can be NULL if we see a JNI stub before we see any
	     other method.  */
	  if (! this_classdollar)
	    this_classdollar = build_classdollar_field (output_class);
	  return this_classdollar;
	}
      
      if (TREE_CODE (type) == RECORD_TYPE)
	return build_static_class_ref (type);
      else
	{
	  const char *name;
	  tree decl_name;
	  char buffer[25];
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
	      decl = build_decl (input_location,
				 VAR_DECL, decl_name, class_type_node);
	      TREE_STATIC (decl) = 1;
	      TREE_PUBLIC (decl) = 1;
	      DECL_EXTERNAL (decl) = 1;
	      DECL_ARTIFICIAL (decl) = 1;
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
  char *buf = (char *) alloca (strlen (name) + 20);
  sprintf (buf, "%s_%d_ref", name, index);
  decl_name = get_identifier (buf);
  decl = IDENTIFIER_GLOBAL_VALUE (decl_name);
  if (decl == NULL_TREE)
    {
      decl = build_decl (input_location,
			 VAR_DECL, decl_name, ptr_type_node);
      TREE_STATIC (decl) = 1;
      TREE_PUBLIC (decl) = 0;
      DECL_EXTERNAL (decl) = 0;
      DECL_ARTIFICIAL (decl) = 1;
      DECL_IGNORED_P (decl) = 1;
      pushdecl_top_level (decl);
    }
  return decl;
}

tree
build_static_field_ref (tree fdecl)
{
  tree fclass = DECL_CONTEXT (fdecl);
  int is_compiled = is_compiled_class (fclass);

  /* Allow static final fields to fold to a constant.  When using
     -findirect-dispatch, we simply never do this folding if compiling
     from .class; in the .class file constants will be referred to via
     the constant pool.  */
  if (!flag_indirect_dispatch
      && (is_compiled
	  || (FIELD_FINAL (fdecl) && DECL_INITIAL (fdecl) != NULL_TREE
	      && (JSTRING_TYPE_P (TREE_TYPE (fdecl))
		  || JNUMERIC_TYPE_P (TREE_TYPE (fdecl)))
	      && TREE_CONSTANT (DECL_INITIAL (fdecl)))))
    {
      if (is_compiled == 1)
	DECL_EXTERNAL (fdecl) = 1;
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
        = build_call_expr (builtin_decl_implicit (BUILT_IN_EXPECT), 2,
			   build2 (EQ_EXPR, boolean_type_node,
				   cache_entry, null_pointer_node),
			   boolean_false_node);
      tree cpool_index_cst = build_int_cst (NULL_TREE, cpool_index);
      tree init
	= build_call_expr (soft_resolvepoolentry_node, 2,
			   build_class_ref (output_class),
			   cpool_index_cst);
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
      if (FIELD_ENUM (decl))
	access_flags |= ACC_ENUM;
      if (FIELD_SYNTHETIC (decl))
	access_flags |= ACC_SYNTHETIC;
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
      if (CLASS_ENUM (decl))
	access_flags |= ACC_ENUM;
      if (CLASS_SYNTHETIC (decl))
	access_flags |= ACC_SYNTHETIC;
      if (CLASS_ANNOTATION (decl))
	access_flags |= ACC_ANNOTATION;
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
      if (DECL_ARTIFICIAL (decl))
	access_flags |= ACC_SYNTHETIC;
      if (METHOD_BRIDGE (decl))
	access_flags |= ACC_BRIDGE;
      if (METHOD_VARARGS (decl))
	access_flags |= ACC_VARARGS;
      return access_flags;
    }
  gcc_unreachable ();
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
  char *name = (char *) alloca (strlen (method_name) + 2);
  char *buf = (char *) alloca (strlen (method_name) + 128);

  /* Only create aliases for local functions.  */
  if (DECL_EXTERNAL (method))
    return method;
    
  /* Prefix method_name with 'L' for the alias label.  */
  *name = 'L';
  strcpy (name + 1, method_name);

  targetm.asm_out.generate_internal_label (buf, name, alias_labelno++);  
  alias = build_decl (input_location,
		      FUNCTION_DECL, get_identifier (buf),
		      TREE_TYPE (method));
  DECL_CONTEXT (alias) = NULL;
  TREE_READONLY (alias) = TREE_READONLY (method);
  TREE_THIS_VOLATILE (alias) = TREE_THIS_VOLATILE (method);
  TREE_PUBLIC (alias) = 0;
  DECL_EXTERNAL (alias) = 0;
  DECL_ARTIFICIAL (alias) = 1;
  DECL_INITIAL (alias) = error_mark_node;
  TREE_ADDRESSABLE (alias) = 1;
  TREE_USED (alias) = 1;
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
  vec<constructor_elt, va_gc> *v = NULL;

  START_RECORD_CONSTRUCTOR (v, field_type_node);
  PUSH_FIELD_VALUE (v, "name", build_utf8_ref (DECL_NAME (fdecl)));
  if (resolved)
    type = build_class_ref (type);
  else
    {
      tree signature = build_java_signature (type);

      type = build_utf8_ref (unmangle_classname 
			     (IDENTIFIER_POINTER (signature),
			      IDENTIFIER_LENGTH (signature)));
    }
  PUSH_FIELD_VALUE (v, "type", type);

  flags = get_access_flags_from_decl (fdecl);
  if (! resolved)
    flags |= 0x8000 /* FIELD_UNRESOLVED_FLAG */;

  PUSH_FIELD_VALUE (v, "accflags", build_int_cst (NULL_TREE, flags));
  PUSH_FIELD_VALUE (v, "bsize", TYPE_SIZE_UNIT (TREE_TYPE (fdecl)));

  {
    tree field_address = integer_zero_node;
    tree index, value;
    if ((DECL_INITIAL (fdecl) || ! flag_indirect_classes) 
	&& FIELD_STATIC (fdecl))
      field_address = build_address_of (fdecl);

    index = (FIELD_STATIC (fdecl)
	     ? DECL_CHAIN (TYPE_FIELDS (field_info_union_node))
	     : TYPE_FIELDS (field_info_union_node));
    value = (FIELD_STATIC (fdecl)
	     ? field_address
	     : byte_position (fdecl));

    PUSH_FIELD_VALUE
      (v, "info",
       build_constructor_single (field_info_union_node, index, value));
  }

  FINISH_RECORD_CONSTRUCTOR (finit, v, field_type_node);
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
  vec<constructor_elt, va_gc> *v = NULL;

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
  if (METHOD_ABSTRACT (mdecl))
    code = build1 (ADDR_EXPR, nativecode_ptr_type_node,
		   soft_abstractmethod_node);
  else
    code = build1 (ADDR_EXPR, nativecode_ptr_type_node, 
		   make_local_function_alias (mdecl));
  START_RECORD_CONSTRUCTOR (v, method_type_node);
  PUSH_FIELD_VALUE (v, "name",
		    build_utf8_ref (DECL_CONSTRUCTOR_P (mdecl) ?
				    init_identifier_node
				    : DECL_NAME (mdecl)));
  {
    tree signature = build_java_signature (TREE_TYPE (mdecl));
    PUSH_FIELD_VALUE (v, "signature", 
		      (build_utf8_ref 
		       (unmangle_classname 
			(IDENTIFIER_POINTER(signature),
			 IDENTIFIER_LENGTH(signature)))));
  }
  PUSH_FIELD_VALUE (v, "accflags", build_int_cst (NULL_TREE, accflags));
  PUSH_FIELD_VALUE (v, "index", index);
  PUSH_FIELD_VALUE (v, "ncode", code);

  {
    /* Compute the `throws' information for the method.  */
    tree table = null_pointer_node;

    if (!vec_safe_is_empty (DECL_FUNCTION_THROWS (mdecl)))
      {
	int length = 1 + DECL_FUNCTION_THROWS (mdecl)->length ();
	tree t, type, array;
	char buf[60];
	vec<constructor_elt, va_gc> *v = NULL;
	int idx = length - 1;
	unsigned ix;
	constructor_elt *e;

	vec_alloc (v, length);
	v->quick_grow_cleared (length);

	e = &(*v)[idx--];
	e->value = null_pointer_node;

	FOR_EACH_VEC_SAFE_ELT (DECL_FUNCTION_THROWS (mdecl), ix, t)
	  {
	    tree sig = DECL_NAME (TYPE_NAME (t));
	    tree utf8
	      = build_utf8_ref (unmangle_classname (IDENTIFIER_POINTER (sig),
						    IDENTIFIER_LENGTH (sig)));
	    e = &(*v)[idx--];
	    e->value = utf8;
	  }
	gcc_assert (idx == -1);
	type = build_prim_array_type (ptr_type_node, length);
	table = build_constructor (type, v);
	/* Compute something unique enough.  */
	sprintf (buf, "_methods%d", method_name_count++);
	array = build_decl (input_location,
			    VAR_DECL, get_identifier (buf), type);
	DECL_INITIAL (array) = table;
	TREE_STATIC (array) = 1;
	DECL_ARTIFICIAL (array) = 1;
	DECL_IGNORED_P (array) = 1;
	rest_of_decl_compilation (array, 1, 0);

	table = build1 (ADDR_EXPR, ptr_type_node, array);
      }

    PUSH_FIELD_VALUE (v, "throws", table);
  }

  FINISH_RECORD_CONSTRUCTOR (minit, v, method_type_node);
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
	   method = DECL_CHAIN (method))
	{
	  tree method_index = get_method_index (method);
	  if (method_index != NULL_TREE
	      && tree_fits_shwi_p (method_index))
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
  int nvirtuals = TREE_VEC_LENGTH (vtable);
  int arraysize;
  tree gc_descr;
  vec<constructor_elt, va_gc> *v = NULL;
  constructor_elt *e;
  tree arraytype;

  arraysize = (TARGET_VTABLE_USES_DESCRIPTORS? nvirtuals + 1 : nvirtuals + 2);
  if (TARGET_VTABLE_USES_DESCRIPTORS)
    arraysize *= TARGET_VTABLE_USES_DESCRIPTORS;
  arraysize += 2;

  vec_safe_grow_cleared (v, arraysize);
  e = &(*v)[arraysize - 1];

#define CONSTRUCTOR_PREPEND_VALUE(E, V) E->value = V, E--
  for (i = nvirtuals;  --i >= 0; )
    {
      tree method = TREE_VEC_ELT (vtable, i);
      if (METHOD_ABSTRACT (method))
	{
	  if (! abstract_p)
	    warning_at (DECL_SOURCE_LOCATION (method), 0,
			"abstract method in non-abstract class");

	  if (TARGET_VTABLE_USES_DESCRIPTORS)
	    for (j = 0; j < TARGET_VTABLE_USES_DESCRIPTORS; ++j)
	      CONSTRUCTOR_PREPEND_VALUE (e, null_pointer_node);
	  else
	    CONSTRUCTOR_PREPEND_VALUE (e, null_pointer_node);
	}
      else
	{
	  if (TARGET_VTABLE_USES_DESCRIPTORS)
	    for (j = 0; j < TARGET_VTABLE_USES_DESCRIPTORS; ++j)
	      {
		tree fdesc = build2 (FDESC_EXPR, nativecode_ptr_type_node, 
				     method, build_int_cst (NULL_TREE, j));
		TREE_CONSTANT (fdesc) = 1;
		CONSTRUCTOR_PREPEND_VALUE (e, fdesc);
	      }
	  else
	    CONSTRUCTOR_PREPEND_VALUE (e,
				       build1 (ADDR_EXPR,
					       nativecode_ptr_type_node,
					       method));
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
  CONSTRUCTOR_PREPEND_VALUE (e, gc_descr);
  for (j = 1; j < TARGET_VTABLE_USES_DESCRIPTORS-1; ++j)
    CONSTRUCTOR_PREPEND_VALUE (e, gc_descr);
  CONSTRUCTOR_PREPEND_VALUE (e, this_class_addr);

  /** Pointer to type_info object (to be implemented), according to g++ ABI. */
  CONSTRUCTOR_PREPEND_VALUE (e, null_pointer_node);
  /** Offset to start of whole object.  Always (ptrdiff_t)0 for Java. */
  gcc_assert (e == v->address ());
  e->index = integer_zero_node;
  e->value = null_pointer_node;
#undef CONSTRUCTOR_PREPEND_VALUE

  arraytype = build_prim_array_type (nativecode_ptr_type_node, arraysize);
  return build_constructor (arraytype, v);
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

static void
add_table_and_syms (vec<constructor_elt, va_gc> **v,
                    vec<method_entry, va_gc> *methods,
                    const char *table_name, tree table_slot, tree table_type,
                    const char *syms_name, tree syms_slot)
{
  if (methods == NULL)
    {
      PUSH_FIELD_VALUE (*v, table_name, null_pointer_node);
      PUSH_FIELD_VALUE (*v, syms_name, null_pointer_node);
    }
  else
    {
      pushdecl_top_level (syms_slot);
      PUSH_FIELD_VALUE (*v, table_name,
                        build1 (ADDR_EXPR, table_type, table_slot));
      PUSH_FIELD_VALUE (*v, syms_name,
                        build1 (ADDR_EXPR, symbols_array_ptr_type,
                                syms_slot));
      TREE_CONSTANT (table_slot) = 1;
    }
}
                    
void
make_class_data (tree type)
{
  tree decl, cons, temp;
  tree field, fields_decl;
  HOST_WIDE_INT static_field_count = 0;
  HOST_WIDE_INT instance_field_count = 0;
  HOST_WIDE_INT field_count;
  tree field_array_type;
  tree method;
  tree dtable_decl = NULL_TREE;
  HOST_WIDE_INT method_count = 0;
  tree method_array_type;
  tree methods_decl;
  tree super;
  tree this_class_addr;
  tree constant_pool_constructor;
  tree interfaces = null_pointer_node;
  int interface_len = 0;
  int uses_jv_markobj = 0;
  tree type_decl = TYPE_NAME (type);
  tree id_main = get_identifier("main");
  tree id_class = get_identifier("java.lang.Class");
  /** Offset from start of virtual function table declaration
      to where objects actually point at, following new g++ ABI. */
  tree dtable_start_offset = size_int (2 * POINTER_SIZE / BITS_PER_UNIT);
  vec<int> field_indexes;
  tree first_real_field;
  vec<constructor_elt, va_gc> *v1 = NULL, *v2 = NULL;
  tree reflection_data;
  vec<constructor_elt, va_gc> *static_fields = NULL;
  vec<constructor_elt, va_gc> *instance_fields = NULL;
  vec<constructor_elt, va_gc> *methods = NULL;

  this_class_addr = build_static_class_ref (type);
  decl = TREE_OPERAND (this_class_addr, 0);

  if (supers_all_compiled (type) && ! CLASS_INTERFACE (type_decl)
      && !flag_indirect_dispatch)
    {
      tree dtable = get_dispatch_table (type, this_class_addr);
      uses_jv_markobj = uses_jv_markobj_p (dtable);
      if (type == class_type_node && class_dtable_decl != NULL_TREE)
	{
	  /* We've already created some other class, and consequently
	     we made class_dtable_decl.  Now we just want to fill it
	     in.  */
	  dtable_decl = class_dtable_decl;
	}
      else
	{
	  dtable_decl = build_dtable_decl (type);
	  TREE_STATIC (dtable_decl) = 1;
	  DECL_ARTIFICIAL (dtable_decl) = 1;
	  DECL_IGNORED_P (dtable_decl) = 1;
	}

      TREE_PUBLIC (dtable_decl) = 1;
      DECL_INITIAL (dtable_decl) = dtable;
      /* The only dispatch table exported from a DSO is the dispatch
	 table for java.lang.Class.  */
      if (DECL_NAME (type_decl) != id_class)
	java_hide_decl (dtable_decl);
      if (! flag_indirect_classes)
	rest_of_decl_compilation (dtable_decl, 1, 0);
      /* Maybe we're compiling Class as the first class.  If so, set
	 class_dtable_decl to the decl we just made.  */
      if (type == class_type_node && class_dtable_decl == NULL_TREE)
	class_dtable_decl = dtable_decl;
    }

  /* Build Field array. */
  field = TYPE_FIELDS (type);
  while (field && DECL_ARTIFICIAL (field))
    field = DECL_CHAIN (field);  /* Skip dummy fields.  */
  if (field && DECL_NAME (field) == NULL_TREE)
    field = DECL_CHAIN (field);  /* Skip dummy field for inherited data. */
  first_real_field = field;

  /* First count static and instance fields.  */
  for ( ; field != NULL_TREE; field = DECL_CHAIN (field))
    {
      if (! DECL_ARTIFICIAL (field))
	{
	  if (FIELD_STATIC (field))
	    static_field_count++;
	  else if (uses_jv_markobj || !flag_reduced_reflection)
	    instance_field_count++;
	}
    }
  field_count = static_field_count + instance_field_count;
  field_indexes.create (field_count);
  
  /* gcj sorts fields so that static fields come first, followed by
     instance fields.  Unfortunately, by the time this takes place we
     have already generated the reflection_data for this class, and
     that data contains indexes into the fields.  So, we generate a
     permutation that maps each original field index to its final
     position.  Then we pass this permutation to
     rewrite_reflection_indexes(), which fixes up the reflection
     data.  */
  {
    int i;
    int static_count = 0;
    int instance_count = static_field_count;
    int field_index;

    for (i = 0, field = first_real_field; 
	 field != NULL_TREE; 
	 field = DECL_CHAIN (field), i++)
    {
      if (! DECL_ARTIFICIAL (field))
	{
	  field_index = 0;
	  if (FIELD_STATIC (field))
	    field_index = static_count++;
	  else if (uses_jv_markobj || !flag_reduced_reflection)
	    field_index = instance_count++;
	  else
	    continue;
	  field_indexes.quick_push (field_index);
	}
    }
  }

  for (field = first_real_field; field != NULL_TREE; 
       field = DECL_CHAIN (field))
    {
      if (! DECL_ARTIFICIAL (field))
	{
	  if (FIELD_STATIC (field))
	    {
              /* We must always create reflection data for static fields
                 as it is used in the creation of the field itself. */
              tree init = make_field_value (field);
	      tree initial = DECL_INITIAL (field);
              CONSTRUCTOR_APPEND_ELT (static_fields, NULL_TREE, init);
	      /* If the initial value is a string constant,
		 prevent output_constant from trying to assemble the value. */
	      if (initial != NULL_TREE
		  && TREE_TYPE (initial) == string_ptr_type_node)
		DECL_INITIAL (field) = NULL_TREE;
	      rest_of_decl_compilation (field, 1, 1);
	      DECL_INITIAL (field) = initial;
	    }
	  else if (uses_jv_markobj || !flag_reduced_reflection)
	    {
              tree init = make_field_value (field);
              CONSTRUCTOR_APPEND_ELT (instance_fields, NULL_TREE, init);
	    }
	}
    }

  gcc_assert (static_field_count == (int) vec_safe_length (static_fields));
  gcc_assert (instance_field_count == (int) vec_safe_length (instance_fields));

  if (field_count > 0)
    {
      vec_safe_splice (static_fields, instance_fields);
      field_array_type = build_prim_array_type (field_type_node, field_count);
      fields_decl = build_decl (input_location,
				VAR_DECL, mangled_classname ("_FL_", type),
				field_array_type);
      DECL_INITIAL (fields_decl)
        = build_constructor (field_array_type, static_fields);
      TREE_STATIC (fields_decl) = 1;
      DECL_ARTIFICIAL (fields_decl) = 1;
      DECL_IGNORED_P (fields_decl) = 1;
      rest_of_decl_compilation (fields_decl, 1, 0);
    }
  else
    fields_decl = NULL_TREE;

  /* Build Method array. */
  for (method = TYPE_METHODS (type);
       method != NULL_TREE; method = DECL_CHAIN (method))
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

      /* Generate method reflection data if:

          - !flag_reduced_reflection.

          - <clinit> -- The runtime uses reflection to initialize the
            class.

          - Any method in class java.lang.Class -- Class.forName() and
            perhaps other things require it.

          - class$ -- It does not work if reflection data missing.

          - main -- Reflection is used to find main(String[]) methods.

          - public not static -- It is potentially part of an
            interface.  The runtime uses reflection data to build
            interface dispatch tables.  */
      if (!flag_reduced_reflection
          || DECL_CLINIT_P (method)
          || DECL_NAME (type_decl) == id_class
          || DECL_NAME (method) == id_main
          || (METHOD_PUBLIC (method) && !METHOD_STATIC (method)))
        {
          init = make_method_value (method);
          method_count++;
          CONSTRUCTOR_APPEND_ELT (methods, NULL_TREE, init);
        }
    }
  method_array_type = build_prim_array_type (method_type_node, method_count);
  methods_decl = build_decl (input_location,
			     VAR_DECL, mangled_classname ("_MT_", type),
			     method_array_type);
  DECL_INITIAL (methods_decl) = build_constructor (method_array_type, methods);
  TREE_STATIC (methods_decl) = 1;
  DECL_ARTIFICIAL (methods_decl) = 1;
  DECL_IGNORED_P (methods_decl) = 1;
  rest_of_decl_compilation (methods_decl, 1, 0);

  if (class_dtable_decl == NULL_TREE)
    {
      class_dtable_decl = build_dtable_decl (class_type_node);
      TREE_STATIC (class_dtable_decl) = 1;
      DECL_ARTIFICIAL (class_dtable_decl) = 1;
      DECL_IGNORED_P (class_dtable_decl) = 1;
      if (is_compiled_class (class_type_node) != 2)
	{
	  DECL_EXTERNAL (class_dtable_decl) = 1;
	  rest_of_decl_compilation (class_dtable_decl, 1, 0);
	}
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
      int i;
      tree interface_array_type, idecl;
      vec<constructor_elt, va_gc> *init;
      vec_alloc (init, interface_len);
      interface_array_type
	= build_prim_array_type (class_ptr_type, interface_len);
      idecl = build_decl (input_location,
			  VAR_DECL, mangled_classname ("_IF_", type),
			  interface_array_type);
      
      for (i = 1; i <= interface_len; i++)
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
	  CONSTRUCTOR_APPEND_ELT (init, NULL_TREE, index);
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

  START_RECORD_CONSTRUCTOR (v1, object_type_node);
  PUSH_FIELD_VALUE (v1, "vtable",
		    (flag_indirect_classes 
		     ? null_pointer_node
		     : fold_build_pointer_plus
			 (build1 (ADDR_EXPR, dtable_ptr_type,
				  class_dtable_decl),
			  dtable_start_offset)));
  if (! flag_hash_synchronization)
    PUSH_FIELD_VALUE (v1, "sync_info", null_pointer_node);
  FINISH_RECORD_CONSTRUCTOR (temp, v1, object_type_node);
  START_RECORD_CONSTRUCTOR (v2, class_type_node);
  PUSH_SUPER_VALUE (v2, temp);
  PUSH_FIELD_VALUE (v2, "next_or_version", gcj_abi_version);
  PUSH_FIELD_VALUE (v2, "name", build_utf8_ref (DECL_NAME (type_decl)));
  PUSH_FIELD_VALUE (v2, "accflags",
		    build_int_cst (NULL_TREE,
				   get_access_flags_from_decl (type_decl)));

  PUSH_FIELD_VALUE (v2, "superclass", 
		    CLASS_INTERFACE (type_decl) ? null_pointer_node : super);
  PUSH_FIELD_VALUE (v2, "constants", constant_pool_constructor);
  PUSH_FIELD_VALUE (v2, "methods",
                    methods_decl == NULL_TREE ? null_pointer_node
		    : build1 (ADDR_EXPR, method_ptr_type_node, methods_decl));
  PUSH_FIELD_VALUE (v2, "method_count",
		    build_int_cst (NULL_TREE, method_count));

  PUSH_FIELD_VALUE (v2, "vtable_method_count",
                    (flag_indirect_dispatch
                     ? integer_minus_one_node
                     : TYPE_NVIRTUALS (type)));
    
  PUSH_FIELD_VALUE (v2, "fields",
		    fields_decl == NULL_TREE ? null_pointer_node
		    : build1 (ADDR_EXPR, field_ptr_type_node, fields_decl));
  /* If we're using the binary compatibility ABI we don't know the
     size until load time.  */
  PUSH_FIELD_VALUE (v2, "size_in_bytes", 
		    (flag_indirect_dispatch 
		     ? integer_minus_one_node 
		     : size_in_bytes (type)));
  PUSH_FIELD_VALUE (v2, "field_count", 
		    build_int_cst (NULL_TREE, field_count));
  PUSH_FIELD_VALUE (v2, "static_field_count",
		    build_int_cst (NULL_TREE, static_field_count));

  PUSH_FIELD_VALUE (v2, "vtable",
                    (flag_indirect_dispatch || dtable_decl == NULL_TREE
                     ? null_pointer_node
                     : fold_build_pointer_plus
			 (build1 (ADDR_EXPR, dtable_ptr_type,
				  dtable_decl),
			  dtable_start_offset)));
  add_table_and_syms (&v2, TYPE_OTABLE_METHODS (type),
                      "otable", TYPE_OTABLE_DECL (type), otable_ptr_type,
                      "otable_syms", TYPE_OTABLE_SYMS_DECL (type));
  add_table_and_syms (&v2, TYPE_ATABLE_METHODS (type),
                      "atable", TYPE_ATABLE_DECL (type), atable_ptr_type,
                      "atable_syms", TYPE_ATABLE_SYMS_DECL (type));
  add_table_and_syms (&v2, TYPE_ITABLE_METHODS (type),
                      "itable", TYPE_ITABLE_DECL (type), itable_ptr_type,
                      "itable_syms", TYPE_ITABLE_SYMS_DECL (type));
 
  PUSH_FIELD_VALUE (v2, "catch_classes",
		    build1 (ADDR_EXPR, ptr_type_node, TYPE_CTABLE_DECL (type)));
  PUSH_FIELD_VALUE (v2, "interfaces", interfaces);
  PUSH_FIELD_VALUE (v2, "loader", null_pointer_node);
  PUSH_FIELD_VALUE (v2, "interface_count",
		    build_int_cst (NULL_TREE, interface_len));
  PUSH_FIELD_VALUE (v2, "state",
		    convert (byte_type_node,
			     build_int_cst (NULL_TREE, JV_STATE_PRELOADING)));

  PUSH_FIELD_VALUE (v2, "thread", null_pointer_node);
  PUSH_FIELD_VALUE (v2, "depth", integer_zero_node);
  PUSH_FIELD_VALUE (v2, "ancestors", null_pointer_node);
  PUSH_FIELD_VALUE (v2, "idt", null_pointer_node);
  PUSH_FIELD_VALUE (v2, "arrayclass", null_pointer_node);
  PUSH_FIELD_VALUE (v2, "protectionDomain", null_pointer_node);

  {
    tree assertion_table_ref;
    if (TYPE_ASSERTIONS (type) == NULL)
      assertion_table_ref = null_pointer_node;
    else
      assertion_table_ref = build1 (ADDR_EXPR, 
				    build_pointer_type (assertion_table_type),
				    emit_assertion_table (type));
    
    PUSH_FIELD_VALUE (v2, "assertion_table", assertion_table_ref);
  }

  PUSH_FIELD_VALUE (v2, "hack_signers", null_pointer_node);
  PUSH_FIELD_VALUE (v2, "chain", null_pointer_node);
  PUSH_FIELD_VALUE (v2, "aux_info", null_pointer_node);
  PUSH_FIELD_VALUE (v2, "engine", null_pointer_node);

  if (TYPE_REFLECTION_DATA (current_class))
    {
      int i;
      int count = TYPE_REFLECTION_DATASIZE (current_class);
      vec<constructor_elt, va_gc> *v;
      vec_alloc (v, count);
      unsigned char *data = TYPE_REFLECTION_DATA (current_class);
      tree max_index = build_int_cst (sizetype, count);
      tree index = build_index_type (max_index);
      tree type = build_array_type (unsigned_byte_type_node, index);
      char buf[64];
      tree array;
      static int reflection_data_count;

      sprintf (buf, "_reflection_data_%d", reflection_data_count++);
      array = build_decl (input_location,
			  VAR_DECL, get_identifier (buf), type);

      rewrite_reflection_indexes (&field_indexes);

      for (i = 0; i < count; i++)
	{
	  constructor_elt elt;
 	  elt.index = build_int_cst (sizetype, i);
	  elt.value = build_int_cstu (byte_type_node, data[i]);
	  v->quick_push (elt);
	}

      DECL_INITIAL (array) = build_constructor (type, v);
      TREE_STATIC (array) = 1;
      DECL_ARTIFICIAL (array) = 1;
      DECL_IGNORED_P (array) = 1;
      TREE_READONLY (array) = 1;
      TREE_CONSTANT (DECL_INITIAL (array)) = 1;
      rest_of_decl_compilation (array, 1, 0);

      reflection_data = build_address_of (array);

      free (data);
      TYPE_REFLECTION_DATA (current_class) = NULL;
    }
  else
    reflection_data = null_pointer_node;

  PUSH_FIELD_VALUE (v2, "reflection_data", reflection_data);
  FINISH_RECORD_CONSTRUCTOR (cons, v2, class_type_node);

  DECL_INITIAL (decl) = cons;
  
  /* Hash synchronization requires at least 64-bit alignment. */
  if (flag_hash_synchronization && POINTER_SIZE < 64)
    DECL_ALIGN (decl) = 64; 
  
  if (flag_indirect_classes)
    {
      TREE_READONLY (decl) = 1;
      TREE_CONSTANT (DECL_INITIAL (decl)) = 1;
    }

  rest_of_decl_compilation (decl, 1, 0);
  
  {
    tree classdollar_field = build_classdollar_field (type);
    if (!flag_indirect_classes)
      DECL_INITIAL (classdollar_field) = build_static_class_ref (type);
    rest_of_decl_compilation (classdollar_field, 1, 0);
  }

  TYPE_OTABLE_DECL (type) = NULL_TREE;
  TYPE_ATABLE_DECL (type) = NULL_TREE;
  TYPE_CTABLE_DECL (type) = NULL_TREE;
}

void
finish_class (void)
{
  java_expand_catch_classes (current_class);

  current_function_decl = NULL_TREE;
  TYPE_DECL_SUPPRESS_DEBUG (TYPE_NAME (current_class)) = 0;
  make_class_data (current_class);
  register_class ();
  rest_of_decl_compilation (TYPE_NAME (current_class), 1, 0);
}

/* Return 2 if KLASS is compiled by this compilation job;
   return 1 if KLASS can otherwise be assumed to be compiled;
   return 0 if we cannot assume that KLASS is compiled.
   Returns 1 for primitive and 0 for array types.  */
int
is_compiled_class (tree klass)
{
  int seen_in_zip;
  if (TREE_CODE (klass) == POINTER_TYPE)
    klass = TREE_TYPE (klass);
  if (TREE_CODE (klass) != RECORD_TYPE)  /* Primitive types are static. */
    return 1;
  if (TYPE_ARRAY_P (klass))
    return 0;

  seen_in_zip = (TYPE_JCF (klass) && JCF_SEEN_IN_ZIP (TYPE_JCF (klass)));
  if (CLASS_FROM_CURRENTLY_COMPILED_P (klass))
    {
      /* The class was seen in the current ZIP file and will be
	 available as a compiled class in the future but may not have
	 been loaded already. Load it if necessary. This prevent
	 build_class_ref () from crashing. */

      if (seen_in_zip && !CLASS_LOADED_P (klass) && (klass != current_class))
        load_class (klass, 1);

      /* We return 2 for class seen in ZIP and class from files
         belonging to the same compilation unit */
      return 2;
    }

  if (assume_compiled (IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (klass)))))
    {
      if (!CLASS_LOADED_P (klass))
	{
	  if (klass != current_class)
	    load_class (klass, 1);
	}
      return 1;
    }

  return 0;
}

/* Build a VAR_DECL for the dispatch table (vtable) for class TYPE. */

tree
build_dtable_decl (tree type)
{
  tree dtype, decl;

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

      PUSH_FIELD (input_location, dtype, dummy, "top_offset", ptr_type_node);
      PUSH_FIELD (input_location, dtype, dummy, "type_info", ptr_type_node);

      PUSH_FIELD (input_location, dtype, dummy, "class", class_ptr_type);
      for (n = 1; n < TARGET_VTABLE_USES_DESCRIPTORS; ++n)
	{
	  tree tmp_field = build_decl (input_location,
				       FIELD_DECL, NULL_TREE, ptr_type_node);
	  TREE_CHAIN (dummy) = tmp_field;
	  DECL_CONTEXT (tmp_field) = dtype;
	  DECL_ARTIFICIAL (tmp_field) = 1;
	  dummy = tmp_field;
	}

      PUSH_FIELD (input_location, dtype, dummy, "gc_descr", ptr_type_node);
      for (n = 1; n < TARGET_VTABLE_USES_DESCRIPTORS; ++n)
	{
	  tree tmp_field = build_decl (input_location,
				       FIELD_DECL, NULL_TREE, ptr_type_node);
	  TREE_CHAIN (dummy) = tmp_field;
	  DECL_CONTEXT (tmp_field) = dtype;
	  DECL_ARTIFICIAL (tmp_field) = 1;
	  dummy = tmp_field;
	}

      n = TREE_VEC_LENGTH (get_dispatch_vector (type));
      if (TARGET_VTABLE_USES_DESCRIPTORS)
	n *= TARGET_VTABLE_USES_DESCRIPTORS;

      PUSH_FIELD (input_location, dtype, dummy, "methods",
		  build_prim_array_type (nativecode_ptr_type_node, n));
      layout_type (dtype);
    }
  else
    dtype = dtable_type;

  decl = build_decl (input_location,
		     VAR_DECL, get_identifier ("vt$"), dtype);
  DECL_CONTEXT (decl) = type;
  MAYBE_CREATE_VAR_LANG_DECL_SPECIFIC (decl);
  DECL_VTABLE_P (decl) = 1;

  return decl;
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
  base_decl = build_decl (input_location,
			  FIELD_DECL, NULL_TREE, super_class);
  DECL_IGNORED_P (base_decl) = 1;
  DECL_CHAIN (base_decl) = TYPE_FIELDS (this_class);
  TYPE_FIELDS (this_class) = base_decl;
  DECL_SIZE (base_decl) = TYPE_SIZE (super_class);
  DECL_SIZE_UNIT (base_decl) = TYPE_SIZE_UNIT (super_class);
}

/* Handle the different manners we may have to lay out a super class.  */

static tree
maybe_layout_super_class (tree super_class, tree this_class ATTRIBUTE_UNUSED)
{
  if (!super_class)
    return NULL_TREE;
  else if (TREE_CODE (super_class) == RECORD_TYPE)
    {
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
	gcc_unreachable ();
    }
  if (!TYPE_SIZE (super_class))
    safe_layout_class (super_class);

  return super_class;
}

/* safe_layout_class just makes sure that we can load a class without
   disrupting the current_class, input_file, input_line, etc, information
   about the class processed currently.  */

void
safe_layout_class (tree klass)
{
  tree save_current_class = current_class;
  location_t save_location = input_location;

  layout_class (klass);

  current_class = save_current_class;
  input_location = save_location;
}

void
layout_class (tree this_class)
{
  int i;
  tree super_class = CLASSTYPE_SUPER (this_class);

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
      report = XOBFINISH (&temporary_obstack, char *);
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

  layout_type (this_class);

  /* Also recursively load/layout any superinterfaces.  */
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
	   method_decl; method_decl = DECL_CHAIN (method_decl))
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
       method_decl; method_decl = DECL_CHAIN (method_decl))
    dtable_count = layout_class_method (this_class, super_class,
					method_decl, dtable_count);

  TYPE_NVIRTUALS (this_class) = dtable_count;
}

/* Return the index of METHOD in INTERFACE.  This index begins at 1
   and is used as an argument for _Jv_LookupInterfaceMethodIdx(). */
int
get_interface_method_index (tree method, tree interface)
{
  tree meth;
  int i = 1;

  for (meth = TYPE_METHODS (interface); ; meth = DECL_CHAIN (meth))
    {
      if (meth == method)
	return i;
      /* We don't want to put <clinit> into the interface table.  */
      if (! ID_CLINIT_P (DECL_NAME (meth)))
	++i;
      gcc_assert (meth != NULL_TREE);
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

  if (flag_indirect_classes
      || (METHOD_PRIVATE (method_decl) && METHOD_STATIC (method_decl)
	  && ! METHOD_NATIVE (method_decl)
	  && ! special_method_p (method_decl)))
    java_hide_decl (method_decl);

  /* Considered external unless it is being compiled into this object
     file, or it was already flagged as external.  */
  if (!DECL_EXTERNAL (method_decl))
    DECL_EXTERNAL (method_decl) = ((is_compiled_class (this_class) != 2)
                                   || METHOD_NATIVE (method_decl));

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
      build_java_signature (TREE_TYPE (method_decl));
    }
  else if (! METHOD_STATIC (method_decl))
    {
      tree method_sig =
	build_java_signature (TREE_TYPE (method_decl));
      bool method_override = false;
      tree super_method = lookup_java_method (super_class, method_name,
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
	      && ! DECL_ARTIFICIAL (super_method))
	    error ("non-static method %q+D overrides static method",
                   method_decl);
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
	  dtable_count = fold_build2 (PLUS_EXPR, integer_type_node,
				      dtable_count, integer_one_node);
	}
    }

  return dtable_count;
}

static void
register_class (void)
{
  tree node;

  if (!registered_class)
    vec_alloc (registered_class, 8);

  if (flag_indirect_classes)
    node = current_class;
  else
    node = TREE_OPERAND (build_class_ref (current_class), 0);
  vec_safe_push (registered_class, node);
}

/* Emit a function that calls _Jv_RegisterNewClasses with a list of
   all the classes we have emitted.  */

static void
emit_indirect_register_classes (tree *list_p)
{
  tree klass, t, register_class_fn;
  int i;

  int size = vec_safe_length (registered_class) * 2 + 1;
  vec<constructor_elt, va_gc> *init;
  vec_alloc (init, size);
  tree class_array_type
    = build_prim_array_type (ptr_type_node, size);
  tree cdecl = build_decl (input_location,
			   VAR_DECL, get_identifier ("_Jv_CLS"),
			   class_array_type);
  tree reg_class_list;
  FOR_EACH_VEC_SAFE_ELT (registered_class, i, klass)
    {
      t = fold_convert (ptr_type_node, build_static_class_ref (klass));
      CONSTRUCTOR_APPEND_ELT (init, NULL_TREE, t);
      t = fold_convert (ptr_type_node,
                        build_address_of (build_classdollar_field (klass)));
      CONSTRUCTOR_APPEND_ELT (init, NULL_TREE, t);
    }
  CONSTRUCTOR_APPEND_ELT (init, NULL_TREE, integer_zero_node);
  DECL_INITIAL (cdecl) = build_constructor (class_array_type, init);
  TREE_CONSTANT (DECL_INITIAL (cdecl)) = 1;
  TREE_STATIC (cdecl) = 1;
  DECL_ARTIFICIAL (cdecl) = 1;
  DECL_IGNORED_P (cdecl) = 1;
  TREE_READONLY (cdecl) = 1;
  TREE_CONSTANT (cdecl) = 1;
  rest_of_decl_compilation (cdecl, 1, 0);
  reg_class_list = fold_convert (ptr_type_node, build_address_of (cdecl));

  t = build_function_type_list (void_type_node, 
				build_pointer_type (ptr_type_node), NULL);
  t = build_decl (input_location,
		  FUNCTION_DECL, 
		  get_identifier ("_Jv_RegisterNewClasses"), t);
  TREE_PUBLIC (t) = 1;
  DECL_EXTERNAL (t) = 1;
  register_class_fn = t;
  t = build_call_expr (register_class_fn, 1, reg_class_list);
  append_to_statement_list (t, list_p);
}

/* Emit a list of pointers to all classes we have emitted to JCR_SECTION.  */

static void
emit_register_classes_in_jcr_section (void)
{
#ifdef JCR_SECTION_NAME
  tree klass, cdecl, class_array_type;
  int i;
  int size = vec_safe_length (registered_class);
  vec<constructor_elt, va_gc> *init;
  vec_alloc (init, size);

  FOR_EACH_VEC_SAFE_ELT (registered_class, i, klass)
    CONSTRUCTOR_APPEND_ELT (init, NULL_TREE, build_fold_addr_expr (klass));

  /* ??? I would like to use tree_output_constant_def() but there is no way
	 to put the data in a named section name, or to set the alignment,
	 via that function.  So do everything manually here.  */
  class_array_type = build_prim_array_type (ptr_type_node, size);
  cdecl = build_decl (UNKNOWN_LOCATION,
		      VAR_DECL, get_identifier ("_Jv_JCR_SECTION_data"),
		      class_array_type);
  DECL_SECTION_NAME (cdecl) = build_string (strlen (JCR_SECTION_NAME),
					    JCR_SECTION_NAME);
  DECL_ALIGN (cdecl) = POINTER_SIZE;
  DECL_USER_ALIGN (cdecl) = 1;
  DECL_INITIAL (cdecl) = build_constructor (class_array_type, init);
  TREE_CONSTANT (DECL_INITIAL (cdecl)) = 1;
  TREE_STATIC (cdecl) = 1;
  TREE_READONLY (cdecl) = 0;
  TREE_CONSTANT (cdecl) = 1;
  DECL_ARTIFICIAL (cdecl) = 1;
  DECL_IGNORED_P (cdecl) = 1;
  DECL_PRESERVE_P (cdecl) = 1;
  pushdecl_top_level (cdecl);
  relayout_decl (cdecl);
  rest_of_decl_compilation (cdecl, 1, 0);
#else
  /* A target has defined TARGET_USE_JCR_SECTION,
     but doesn't have a JCR_SECTION_NAME.  */
  gcc_unreachable ();
#endif
}


/* Emit a series of calls to _Jv_RegisterClass for every class we emitted.
   A series of calls is added to LIST_P.  */

static void
emit_Jv_RegisterClass_calls (tree *list_p)
{
  tree klass, t, register_class_fn;
  int i;

  t = build_function_type_list (void_type_node, class_ptr_type, NULL);
  t = build_decl (input_location,
		  FUNCTION_DECL, get_identifier ("_Jv_RegisterClass"), t);
  TREE_PUBLIC (t) = 1;
  DECL_EXTERNAL (t) = 1;
  register_class_fn = t;

  FOR_EACH_VEC_SAFE_ELT (registered_class, i, klass)
    {
      t = build_fold_addr_expr (klass);
      t = build_call_expr (register_class_fn, 1, t);
      append_to_statement_list (t, list_p);
    }
}

/* Emit something to register classes at start-up time.

   The default mechanism is to generate instances at run-time.

   An alternative mechanism is through the .jcr section, which contain
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

  /* By default, generate instances of Class at runtime.  */
  if (flag_indirect_classes)
    emit_indirect_register_classes (list_p);
  /* TARGET_USE_JCR_SECTION defaults to 1 if SUPPORTS_WEAK and
     TARGET_ASM_NAMED_SECTION, else 0.  Some targets meet those conditions
     but lack suitable crtbegin/end objects or linker support.  These
     targets can override the default in tm.h to use the fallback mechanism.  */
  else if (TARGET_USE_JCR_SECTION)
    emit_register_classes_in_jcr_section ();
  /* Use the fallback mechanism.  */
  else
    emit_Jv_RegisterClass_calls (list_p);
}

/* Build a constructor for an entry in the symbol table.  */

static tree
build_symbol_table_entry (tree clname, tree name, tree signature)
{
  tree symbol;
  vec<constructor_elt, va_gc> *v = NULL;

  START_RECORD_CONSTRUCTOR (v, symbol_type);
  PUSH_FIELD_VALUE (v, "clname", clname);
  PUSH_FIELD_VALUE (v, "name", name);
  PUSH_FIELD_VALUE (v, "signature", signature);
  FINISH_RECORD_CONSTRUCTOR (symbol, v, symbol_type);
  TREE_CONSTANT (symbol) = 1;

  return symbol;
}

/* Make a symbol_type (_Jv_MethodSymbol) node for DECL. */

static tree
build_symbol_entry (tree decl, tree special)
{
  tree clname, name, signature;
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
  /* SPECIAL is either NULL_TREE or integer_one_node.  We emit
     signature addr+1 if SPECIAL, and this indicates to the runtime
     system that this is a "special" symbol, i.e. one that should
     bypass access controls.  */
  if (special != NULL_TREE)
    signature = fold_build_pointer_plus (signature, special);

  return build_symbol_table_entry (clname, name, signature);
} 

/* Emit a symbol table: used by -findirect-dispatch.  */

tree
emit_symbol_table (tree name, tree the_table,
		   vec<method_entry, va_gc> *decl_table,
                   tree the_syms_decl, tree the_array_element_type,
		   int element_size)
{
  tree table, null_symbol, table_size, the_array_type;
  unsigned index;
  method_entry *e;
  vec<constructor_elt, va_gc> *v = NULL;
  
  /* Only emit a table if this translation unit actually made any
     references via it. */
  if (!decl_table)
    return the_table;

  /* Build a list of _Jv_MethodSymbols for each entry in otable_methods. */
  FOR_EACH_VEC_ELT (*decl_table, index, e)
    CONSTRUCTOR_APPEND_ELT (v, NULL_TREE,
			    build_symbol_entry (e->method, e->special));

  /* Terminate the list with a "null" entry. */
  null_symbol = build_symbol_table_entry (null_pointer_node,
                                          null_pointer_node,
                                          null_pointer_node);
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, null_symbol);

  tree symbols_arr_type
    = build_prim_array_type (symbol_type, vec_safe_length (v));

  table = build_constructor (symbols_arr_type, v);

  /* Make it the initial value for otable_syms and emit the decl. */
  TREE_TYPE (the_syms_decl) = symbols_arr_type;
  relayout_decl (the_syms_decl);
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
  the_table = build_decl (input_location,
			  VAR_DECL, name, the_array_type);
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
  vec<constructor_elt, va_gc> *v = NULL;
  START_RECORD_CONSTRUCTOR (v, type);
  PUSH_FIELD_VALUE (v, "address", catch_class);
  PUSH_FIELD_VALUE (v, "classname", classname);
  FINISH_RECORD_CONSTRUCTOR (entry, v, type);
  return entry;
}


/* Generate the list of Throwable classes that are caught by exception
   handlers in this class.  */
tree 
emit_catch_table (tree this_class)
{
  tree table, table_size, array_type;
  int n_catch_classes;
  constructor_elt *e;
  /* Fill in the dummy entry that make_class created.  */
  e = &(*TYPE_CATCH_CLASSES (this_class))[0];
  e->value = make_catch_class_record (null_pointer_node, null_pointer_node);
  CONSTRUCTOR_APPEND_ELT (TYPE_CATCH_CLASSES (this_class), NULL_TREE,
			  make_catch_class_record (null_pointer_node,
						   null_pointer_node));
  n_catch_classes = TYPE_CATCH_CLASSES (this_class)->length ();
  table_size = build_index_type (build_int_cst (NULL_TREE, n_catch_classes));
  array_type 
    = build_array_type (TREE_TYPE (TREE_TYPE (TYPE_CTABLE_DECL (this_class))),
			table_size);
  table = 
    build_decl (input_location,
		VAR_DECL, DECL_NAME (TYPE_CTABLE_DECL (this_class)), array_type);
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

/* Build an entry in the type assertion table.  */

static tree
build_assertion_table_entry (tree code, tree op1, tree op2)
{
  vec<constructor_elt, va_gc> *v = NULL;
  tree entry;

  START_RECORD_CONSTRUCTOR (v, assertion_entry_type);
  PUSH_FIELD_VALUE (v, "assertion_code", code);
  PUSH_FIELD_VALUE (v, "op1", op1);
  PUSH_FIELD_VALUE (v, "op2", op2);
  FINISH_RECORD_CONSTRUCTOR (entry, v, assertion_entry_type);

  return entry;
}

/* Add an entry to the type assertion table. Callback used during hashtable
   traversal.  */

static int
add_assertion_table_entry (void **htab_entry, void *ptr)
{
  tree entry;
  tree code_val, op1_utf8, op2_utf8;
  vec<constructor_elt, va_gc> **v
      = ((vec<constructor_elt, va_gc> **) ptr);
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

  entry = build_assertion_table_entry (code_val, op1_utf8, op2_utf8);
  
  CONSTRUCTOR_APPEND_ELT (*v, NULL_TREE, entry);
  return true;
}

/* Generate the type assertion table for KLASS, and return its DECL.  */

static tree
emit_assertion_table (tree klass)
{
  tree null_entry, ctor, table_decl;
  htab_t assertions_htab = TYPE_ASSERTIONS (klass);
  vec<constructor_elt, va_gc> *v = NULL;

  /* Iterate through the hash table.  */
  htab_traverse (assertions_htab, add_assertion_table_entry, &v);

  /* Finish with a null entry.  */
  null_entry = build_assertion_table_entry (integer_zero_node,
                                            null_pointer_node,
                                            null_pointer_node);
  
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, null_entry);

  tree type
    = build_prim_array_type (assertion_entry_type, vec_safe_length (v));
  
  ctor = build_constructor (type, v);

  table_decl = build_decl (input_location,
			   VAR_DECL, mangled_classname ("_type_assert_", klass),
			   type);

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

#define JAVA_TREEHASHHASH_H(t) ((hashval_t)TYPE_UID (t))

static hashval_t
java_treetreehash_hash (const void *k_p)
{
  const struct treetreehash_entry *const k
    = (const struct treetreehash_entry *) k_p;
  return JAVA_TREEHASHHASH_H (k->key);
}

static int
java_treetreehash_compare (const void * k1_p, const void * k2_p)
{
  const struct treetreehash_entry *const k1
    = (const struct treetreehash_entry *) k1_p;
  const_tree const k2 = (const_tree) k2_p;
  return (k1->key == k2);
}

tree 
java_treetreehash_find (htab_t ht, tree t)
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
java_treetreehash_new (htab_t ht, tree t)
{
  void **e;
  struct treetreehash_entry *tthe;
  hashval_t hv = JAVA_TREEHASHHASH_H (t);

  e = htab_find_slot_with_hash (ht, t, hv, INSERT);
  if (*e == NULL)
    {
      tthe = ggc_alloc_cleared_treetreehash_entry ();
      tthe->key = t;
      *e = tthe;
    }
  else
    tthe = (struct treetreehash_entry *) *e;
  return &tthe->value;
}

htab_t
java_treetreehash_create (size_t size)
{
  return htab_create_ggc (size, java_treetreehash_hash,
			  java_treetreehash_compare, NULL);
}

/* Break down qualified IDENTIFIER into package and class-name components.
   For example, given SOURCE "pkg.foo.Bar", LEFT will be set to
   "pkg.foo", and RIGHT to "Bar". */

int
split_qualified_name (tree *left, tree *right, tree source)
{
  char *p, *base;
  int l = IDENTIFIER_LENGTH (source);

  base = (char *) alloca (l + 1);
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

/* lang_hooks.decls.final_write_globals: perform final processing on
   global variables.  */

void
java_write_globals (void)
{
  tree *vec = vec_safe_address (pending_static_fields);
  int len = vec_safe_length (pending_static_fields);
  write_global_declarations ();
  emit_debug_global_declarations (vec, len);
  vec_free (pending_static_fields);
}

#include "gt-java-class.h"
