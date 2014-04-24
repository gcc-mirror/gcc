/* Implement classes and message passing for Objective C.
   Copyright (C) 1992-2014 Free Software Foundation, Inc.
   Contributed by Steve Naroff.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "stringpool.h"
#include "stor-layout.h"
#include "attribs.h"

#ifdef OBJCPLUS
#include "cp/cp-tree.h"
#else
#include "c/c-tree.h"
#include "c/c-lang.h"
#endif

#include "c-family/c-common.h"
#include "c-family/c-objc.h"
#include "c-family/c-pragma.h"
#include "c-family/c-format.h"
#include "flags.h"
#include "langhooks.h"
#include "objc-act.h"
#include "objc-map.h"
#include "input.h"
#include "function.h"
#include "toplev.h"
#include "debug.h"
#include "c-family/c-target.h"
#include "diagnostic-core.h"
#include "intl.h"
#include "cgraph.h"
#include "tree-iterator.h"
#include "hash-table.h"
#include "wide-int.h"
#include "langhooks-def.h"
/* Different initialization, code gen and meta data generation for each
   runtime.  */
#include "objc-runtime-hooks.h"
/* Routines used mainly by the runtimes.  */
#include "objc-runtime-shared-support.h"
/* For default_tree_printer ().  */
#include "tree-pretty-print.h"

/* For enum gimplify_status */
#include "gimple-expr.h"
#include "gimplify.h"

/* For encode_method_prototype().  */
#include "objc-encoding.h"

static unsigned int should_call_super_dealloc = 0;

/* When building Objective-C++, we are not linking against the C front-end
   and so need to replicate the C tree-construction functions in some way.  */
#ifdef OBJCPLUS
#define OBJCP_REMAP_FUNCTIONS
#include "objcp-decl.h"
#endif  /* OBJCPLUS */

/* This is the default way of generating a method name.  */
/* This has the problem that "test_method:argument:" and
   "test:method_argument:" will generate the same name
   ("_i_Test__test_method_argument_" for an instance method of the
   class "Test"), so you can't have them both in the same class!
   Moreover, the demangling (going from
   "_i_Test__test_method_argument" back to the original name) is
   undefined because there are two correct ways of demangling the
   name.  */
#ifndef OBJC_GEN_METHOD_LABEL
#define OBJC_GEN_METHOD_LABEL(BUF, IS_INST, CLASS_NAME, CAT_NAME, SEL_NAME, NUM) \
  do {					    \
    char *temp;				    \
    sprintf ((BUF), "_%s_%s_%s_%s",	    \
	     ((IS_INST) ? "i" : "c"),	    \
	     (CLASS_NAME),		    \
	     ((CAT_NAME)? (CAT_NAME) : ""), \
	     (SEL_NAME));		    \
    for (temp = (BUF); *temp; temp++)	    \
      if (*temp == ':') *temp = '_';	    \
  } while (0)
#endif

/* These need specifying.  */
#ifndef OBJC_FORWARDING_STACK_OFFSET
#define OBJC_FORWARDING_STACK_OFFSET 0
#endif

#ifndef OBJC_FORWARDING_MIN_OFFSET
#define OBJC_FORWARDING_MIN_OFFSET 0
#endif

/*** Private Interface (procedures) ***/

/* Init stuff.  */
static void synth_module_prologue (void);

/* Code generation.  */

static tree start_class (enum tree_code, tree, tree, tree, tree);
static tree continue_class (tree);
static void finish_class (tree);
static void start_method_def (tree, tree);

static tree start_protocol (enum tree_code, tree, tree, tree);
static tree build_method_decl (enum tree_code, tree, tree, tree, bool);
static tree objc_add_method (tree, tree, int, bool);
static tree add_instance_variable (tree, objc_ivar_visibility_kind, tree);
static tree build_ivar_reference (tree);
static tree is_ivar (tree, tree);

/* We only need the following for ObjC; ObjC++ will use C++'s definition
   of DERIVED_FROM_P.  */
#ifndef OBJCPLUS
static bool objc_derived_from_p (tree, tree);
#define DERIVED_FROM_P(PARENT, CHILD) objc_derived_from_p (PARENT, CHILD)
#endif

/* Property.  */
static void objc_gen_property_data (tree, tree);
static void objc_synthesize_getter (tree, tree, tree);
static void objc_synthesize_setter (tree, tree, tree);
static tree lookup_property (tree, tree);
static tree lookup_property_in_list (tree, tree);
static tree lookup_property_in_protocol_list (tree, tree);
static void build_common_objc_property_accessor_helpers (void);

static void objc_xref_basetypes (tree, tree);

static tree get_class_ivars (tree, bool);

static void build_fast_enumeration_state_template (void);

#ifdef OBJCPLUS
static void objc_generate_cxx_cdtors (void);
#endif

/* objc attribute */
static void objc_decl_method_attributes (tree*, tree, int);
static tree build_keyword_selector (tree);

static void hash_init (void);

/* Hash tables to manage the global pool of method prototypes.  Each
   of these maps map a method name (selector) identifier to either a
   single tree (for methods with a single method prototype) or a
   TREE_VEC (for methods with multiple method prototypes).  */
static GTY(()) objc_map_t instance_method_map = 0;
static GTY(()) objc_map_t class_method_map = 0;

/* Hash tables to manage the global pool of class names.  */

static GTY(()) objc_map_t class_name_map = 0;
static GTY(()) objc_map_t alias_name_map = 0;

static tree lookup_method (tree, tree);
static tree lookup_method_static (tree, tree, int);

static void interface_hash_init (void);
static tree add_interface (tree, tree);
static void add_category (tree, tree);
static inline tree lookup_category (tree, tree);

/* Protocols.  */

static tree lookup_protocol (tree, bool, bool);
static tree lookup_and_install_protocols (tree, bool);

#ifdef OBJCPLUS
static void really_start_method (tree, tree);
#else
static void really_start_method (tree, struct c_arg_info *);
#endif
static int comp_proto_with_proto (tree, tree, int);
static tree objc_decay_parm_type (tree);

/* Utilities for debugging and error diagnostics.  */

static char *gen_type_name (tree);
static char *gen_type_name_0 (tree);
static char *gen_method_decl (tree);
static char *gen_declaration (tree);

/* Everything else.  */

static void generate_struct_by_value_array (void) ATTRIBUTE_NORETURN;

static void mark_referenced_methods (void);
static bool objc_type_valid_for_messaging (tree type, bool allow_classes);
static tree check_duplicates (tree, int, int);

/*** Private Interface (data) ***/
/* Flags for lookup_method_static().  */

/* Look for class methods.  */
#define OBJC_LOOKUP_CLASS	1
/* Do not examine superclasses.  */
#define OBJC_LOOKUP_NO_SUPER	2
/* Disable returning an instance method of a root class when a class
   method can't be found.  */
#define OBJC_LOOKUP_NO_INSTANCE_METHODS_OF_ROOT_CLASS 4

/* The OCTI_... enumeration itself is in objc/objc-act.h.  */
tree objc_global_trees[OCTI_MAX];

struct imp_entry *imp_list = 0;
int imp_count = 0;	/* `@implementation' */
int cat_count = 0;	/* `@category' */

objc_ivar_visibility_kind objc_ivar_visibility, objc_default_ivar_visibility;

/* Use to generate method labels.  */
static int method_slot = 0;

/* Flag to say whether methods in a protocol are optional or
   required.  */
static bool objc_method_optional_flag = false;

static int objc_collecting_ivars = 0;

/* Flag that is set to 'true' while we are processing a class
   extension.  Since a class extension just "reopens" the main
   @interface, this can be used to determine if we are in the main
   @interface, or in a class extension.  */
static bool objc_in_class_extension = false;

static char *errbuf;	/* Buffer for error diagnostics */

/* An array of all the local variables in the current function that
   need to be marked as volatile.  */
vec<tree, va_gc> *local_variables_to_volatilize = NULL;

/* Store all constructed constant strings in a hash table so that
   they get uniqued properly.  */

struct GTY(()) string_descriptor {
  /* The literal argument .  */
  tree literal;

  /* The resulting constant string.  */
  tree constructor;
};

static GTY((param_is (struct string_descriptor))) htab_t string_htab;

FILE *gen_declaration_file;

/* Hooks for stuff that differs between runtimes.  */
objc_runtime_hooks runtime;

/* Create a temporary variable of type 'type'.  If 'name' is set, uses
   the specified name, else use no name.  Returns the declaration of
   the type.  The 'name' is mostly useful for debugging.
*/
tree
objc_create_temporary_var (tree type, const char *name)
{
  tree decl;

  if (name != NULL)
    {
      decl = build_decl (input_location,
			 VAR_DECL, get_identifier (name), type);
    }
  else
    {
      decl = build_decl (input_location,
			 VAR_DECL, NULL_TREE, type);
    }
  TREE_USED (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  DECL_IGNORED_P (decl) = 1;
  DECL_CONTEXT (decl) = current_function_decl;

  return decl;
}

/* Some platforms pass small structures through registers versus
   through an invisible pointer.  Determine at what size structure is
   the transition point between the two possibilities.  */

static void
generate_struct_by_value_array (void)
{
  tree type;
  tree decls;
  int i, j;
  int aggregate_in_mem[32];
  int found = 0;

  /* Presumably no platform passes 32 byte structures in a register.  */
  /* ??? As an example, m64/ppc/Darwin can pass up to 8*long+13*double
     in registers.  */
  for (i = 1; i < 32; i++)
    {
      char buffer[5];
      tree *chain = NULL;

      /* Create an unnamed struct that has `i' character components */
      type = objc_start_struct (NULL_TREE);

      strcpy (buffer, "c1");
      decls = add_field_decl (char_type_node, buffer, &chain);

      for (j = 1; j < i; j++)
	{
	  sprintf (buffer, "c%d", j + 1);
	  add_field_decl (char_type_node, buffer, &chain);
	}
      objc_finish_struct (type, decls);

      aggregate_in_mem[i] = aggregate_value_p (type, 0);
      if (!aggregate_in_mem[i])
	found = 1;
    }

  /* We found some structures that are returned in registers instead of memory
     so output the necessary data.  */
  if (found)
    {
      for (i = 31; i >= 0;  i--)
	if (!aggregate_in_mem[i])
	  break;
      printf ("#define OBJC_MAX_STRUCT_BY_VALUE %d\n", i);
    }

  exit (0);
}

bool
objc_init (void)
{
  bool ok;
#ifdef OBJCPLUS
  if (cxx_init () == false)
#else
  if (c_objc_common_init () == false)
#endif
    return false;

  /* print_struct_values is triggered by -print-runtime-info (used
     when building libobjc, with an empty file as input).  It does not
     require any ObjC setup, and it never returns.

     -fcompare-debug is used to check the compiler output; we are
     executed twice, once with flag_compare_debug set, and once with
     it not set.  If the flag is used together with
     -print-runtime-info, we want to print the runtime info only once,
     else it would be output in duplicate.  So we check
     flag_compare_debug to output it in only one of the invocations.

     As a side effect, this also that means -fcompare-debug
     -print-runtime-info will run the compiler twice, and compare the
     generated assembler file; the first time the compiler exits
     immediately (producing no file), and the second time it compiles
     an empty file.  This checks, as a side effect, that compiling an
     empty file produces no assembler output.  */
  if (print_struct_values && !flag_compare_debug)
    generate_struct_by_value_array ();

  /* Set up stuff used by FE parser and all runtimes.  */
  errbuf = XNEWVEC (char, 1024 * 10);
  interface_hash_init ();
  hash_init ();
  objc_encoding_init ();
  /* ... and then check flags and set-up for the selected runtime ... */
  if (flag_next_runtime && flag_objc_abi >= 2)
    ok = objc_next_runtime_abi_02_init (&runtime);
  else if (flag_next_runtime)
    ok = objc_next_runtime_abi_01_init (&runtime);
  else
    ok = objc_gnu_runtime_abi_01_init (&runtime);

  /* If that part of the setup failed - bail out immediately.  */
  if (!ok)
    return false;

  /* Determine the default visibility for instance variables. */
  switch (default_ivar_visibility)
    {
    case IVAR_VISIBILITY_PRIVATE:
        objc_default_ivar_visibility = OBJC_IVAR_VIS_PRIVATE;
        break;
    case IVAR_VISIBILITY_PUBLIC:
        objc_default_ivar_visibility = OBJC_IVAR_VIS_PUBLIC;
        break;
    case IVAR_VISIBILITY_PACKAGE:
        objc_default_ivar_visibility = OBJC_IVAR_VIS_PACKAGE;
        break;
    default:
        objc_default_ivar_visibility = OBJC_IVAR_VIS_PROTECTED;
    }
      
  /* Generate general types and push runtime-specific decls to file scope.  */
  synth_module_prologue ();

  return true;
}

/* This is called automatically (at the very end of compilation) by
   c_write_global_declarations and cp_write_global_declarations.  */
void
objc_write_global_declarations (void)
{
  mark_referenced_methods ();

  /* A missing @end might not be detected by the parser.  */
  if (objc_implementation_context)
    {
      warning (0, "%<@end%> missing in implementation context");
      finish_class (objc_implementation_context);
      objc_ivar_chain = NULL_TREE;
      objc_implementation_context = NULL_TREE;
    }

  if (warn_selector)
    {
      objc_map_iterator_t i;

      objc_map_iterator_initialize (class_method_map, &i);
      while (objc_map_iterator_move_to_next (class_method_map, &i))
	check_duplicates (objc_map_iterator_current_value (class_method_map, i), 0, 1);

      objc_map_iterator_initialize (instance_method_map, &i);
      while (objc_map_iterator_move_to_next (instance_method_map, &i))
	check_duplicates (objc_map_iterator_current_value (instance_method_map, i), 0, 0);
    }

  /* TODO: consider an early exit here if either errorcount or sorrycount
     is non-zero.  Not only is it wasting time to generate the metadata,
     it needlessly imposes need to re-check for things that are already
     determined to be errors.  */

  /* Finalize Objective-C runtime data.  No need to generate tables
     and code if only checking syntax, or if generating a PCH file.  */
  if (!flag_syntax_only && !pch_file)
    {
      location_t saved_location;

      /* If gen_declaration desired, open the output file.  */
      if (flag_gen_declaration)
	{
	  char * const dumpname = concat (dump_base_name, ".decl", NULL);
	  gen_declaration_file = fopen (dumpname, "w");
	  if (gen_declaration_file == 0)
	    fatal_error ("can%'t open %s: %m", dumpname);
	  free (dumpname);
	}

      /* Set the input location to BUILTINS_LOCATION.  This is good
	 for error messages, in case any is generated while producing
	 the metadata, but it also silences warnings that would be
	 produced when compiling with -Wpadded in case when padding is
	 automatically added to the built-in runtime data structure
	 declarations.  We know about this padding, and it is fine; we
	 don't want users to see any warnings about it if they use
	 -Wpadded.  */
      saved_location = input_location;
      input_location = BUILTINS_LOCATION;

      /* Compute and emit the meta-data tables for this runtime.  */
      (*runtime.generate_metadata) ();

      /* Restore the original location, just in case it mattered.  */
      input_location = saved_location;

      /* ... and then close any declaration file we opened.  */
      if (gen_declaration_file)
	fclose (gen_declaration_file);
    }
}

/* Return the first occurrence of a method declaration corresponding
   to sel_name in rproto_list.  Search rproto_list recursively.
   If is_class is 0, search for instance methods, otherwise for class
   methods.  */
static tree
lookup_method_in_protocol_list (tree rproto_list, tree sel_name,
				int is_class)
{
  tree rproto, p, m;

   for (rproto = rproto_list; rproto; rproto = TREE_CHAIN (rproto))
     {
       p = TREE_VALUE (rproto);
       m = NULL_TREE;

	if (TREE_CODE (p) == PROTOCOL_INTERFACE_TYPE)
	  {
	    /* First, search the @required protocol methods.  */
	    if (is_class)
	      m = lookup_method (PROTOCOL_CLS_METHODS (p),  sel_name);
	    else
	      m = lookup_method (PROTOCOL_NST_METHODS (p), sel_name);

	    if (m)
	      return m;

	    /* If still not found, search the @optional protocol methods.  */
	    if (is_class)
	      m = lookup_method (PROTOCOL_OPTIONAL_CLS_METHODS (p), sel_name);
	    else
	      m = lookup_method (PROTOCOL_OPTIONAL_NST_METHODS (p), sel_name);

	    if (m)
	      return m;

	    /* If still not found, search the attached protocols.  */
	    if (PROTOCOL_LIST (p))
	      m = lookup_method_in_protocol_list (PROTOCOL_LIST (p),
						  sel_name, is_class);
	    if (m)
	      return m;
	  }
	else
          {
	    ; /* An identifier...if we could not find a protocol.  */
          }
     }

   return 0;
}

static tree
lookup_protocol_in_reflist (tree rproto_list, tree lproto)
{
  tree rproto, p;

  /* Make sure the protocol is supported by the object on the rhs.  */
  if (TREE_CODE (lproto) == PROTOCOL_INTERFACE_TYPE)
    {
      tree fnd = 0;
      for (rproto = rproto_list; rproto; rproto = TREE_CHAIN (rproto))
	{
	  p = TREE_VALUE (rproto);

	  if (TREE_CODE (p) == PROTOCOL_INTERFACE_TYPE)
	    {
	      if (lproto == p)
		fnd = lproto;

	      else if (PROTOCOL_LIST (p))
		fnd = lookup_protocol_in_reflist (PROTOCOL_LIST (p), lproto);
	    }

	  if (fnd)
	    return fnd;
	}
    }
  else
    {
      ; /* An identifier...if we could not find a protocol.  */
    }

  return 0;
}

void
objc_start_class_interface (tree klass, tree super_class,
			    tree protos, tree attributes)
{
  if (flag_objc1_only && attributes)
    error_at (input_location, "class attributes are not available in Objective-C 1.0");

  objc_interface_context
    = objc_ivar_context
    = start_class (CLASS_INTERFACE_TYPE, klass, super_class, protos, attributes);
  objc_ivar_visibility = objc_default_ivar_visibility;
}

void
objc_start_category_interface (tree klass, tree categ,
			       tree protos, tree attributes)
{
  if (attributes)
    {
      if (flag_objc1_only)
	error_at (input_location, "category attributes are not available in Objective-C 1.0");
      else
	warning_at (input_location, OPT_Wattributes,
		    "category attributes are not available in this version"
		    " of the compiler, (ignored)");
    }
  if (categ == NULL_TREE)
    {
      if (flag_objc1_only)
	error_at (input_location, "class extensions are not available in Objective-C 1.0");
      else
	{
	  /* Iterate over all the classes and categories implemented
	     up to now in this compilation unit.  */
	  struct imp_entry *t;

	  for (t = imp_list; t; t = t->next)
	    {
	      /* If we find a class @implementation with the same name
		 as the one we are extending, produce an error.  */
	    if (TREE_CODE (t->imp_context) == CLASS_IMPLEMENTATION_TYPE
		&& IDENTIFIER_POINTER (CLASS_NAME (t->imp_context)) == IDENTIFIER_POINTER (klass))
	      error_at (input_location,
			"class extension for class %qE declared after its %<@implementation%>",
			klass);
	    }
	}
    }
  objc_interface_context
    = start_class (CATEGORY_INTERFACE_TYPE, klass, categ, protos, NULL_TREE);
  objc_ivar_chain
    = continue_class (objc_interface_context);
}

void
objc_start_protocol (tree name, tree protos, tree attributes)
{
  if (flag_objc1_only && attributes)
    error_at (input_location, "protocol attributes are not available in Objective-C 1.0");

  objc_interface_context
    = start_protocol (PROTOCOL_INTERFACE_TYPE, name, protos, attributes);
  objc_method_optional_flag = false;
}

void
objc_continue_interface (void)
{
  objc_ivar_chain
    = continue_class (objc_interface_context);
}

void
objc_finish_interface (void)
{
  finish_class (objc_interface_context);
  objc_interface_context = NULL_TREE;
  objc_method_optional_flag = false;
  objc_in_class_extension = false;
}

void
objc_start_class_implementation (tree klass, tree super_class)
{
  objc_implementation_context
    = objc_ivar_context
    = start_class (CLASS_IMPLEMENTATION_TYPE, klass, super_class, NULL_TREE,
		   NULL_TREE);
  objc_ivar_visibility = objc_default_ivar_visibility;
}

void
objc_start_category_implementation (tree klass, tree categ)
{
  objc_implementation_context
    = start_class (CATEGORY_IMPLEMENTATION_TYPE, klass, categ, NULL_TREE,
		   NULL_TREE);
  objc_ivar_chain
    = continue_class (objc_implementation_context);
}

void
objc_continue_implementation (void)
{
  objc_ivar_chain
    = continue_class (objc_implementation_context);
}

void
objc_finish_implementation (void)
{
#ifdef OBJCPLUS
  if (flag_objc_call_cxx_cdtors)
    objc_generate_cxx_cdtors ();
#endif

  if (objc_implementation_context)
    {
      finish_class (objc_implementation_context);
      objc_ivar_chain = NULL_TREE;
      objc_implementation_context = NULL_TREE;
    }
  else
    warning (0, "%<@end%> must appear in an @implementation context");
}

void
objc_set_visibility (objc_ivar_visibility_kind visibility)
{
  if (visibility == OBJC_IVAR_VIS_PACKAGE)
    {
      if (flag_objc1_only)
	error ("%<@package%> is not available in Objective-C 1.0");
      else
	warning (0, "%<@package%> presently has the same effect as %<@public%>");
    }
  objc_ivar_visibility = visibility;
}

void
objc_set_method_opt (bool optional)
{
  if (flag_objc1_only)
    {
      if (optional)
	error_at (input_location, "%<@optional%> is not available in Objective-C 1.0");
      else
	error_at (input_location, "%<@required%> is not available in Objective-C 1.0");
    }

  objc_method_optional_flag = optional;
  if (!objc_interface_context
      || TREE_CODE (objc_interface_context) != PROTOCOL_INTERFACE_TYPE)
    {
      if (optional)
	error ("%<@optional%> is allowed in @protocol context only");
      else
	error ("%<@required%> is allowed in @protocol context only");
      objc_method_optional_flag = false;
    }
}

/* This routine looks for a given PROPERTY in a list of CLASS, CATEGORY, or
   PROTOCOL.  */
static tree
lookup_property_in_list (tree chain, tree property)
{
  tree x;
  for (x = CLASS_PROPERTY_DECL (chain); x; x = TREE_CHAIN (x))
    if (PROPERTY_NAME (x) == property)
      return x;
  return NULL_TREE;
}

/* This routine looks for a given PROPERTY in the tree chain of RPROTO_LIST. */
static tree lookup_property_in_protocol_list (tree rproto_list, tree property)
{
  tree rproto, x;
  for (rproto = rproto_list; rproto; rproto = TREE_CHAIN (rproto))
    {
      tree p = TREE_VALUE (rproto);
      if (TREE_CODE (p) == PROTOCOL_INTERFACE_TYPE)
	{
	  if ((x = lookup_property_in_list (p, property)))
	    return x;
	  if (PROTOCOL_LIST (p))
	    return lookup_property_in_protocol_list (PROTOCOL_LIST (p), property);
	}
      else
	{
	  ; /* An identifier...if we could not find a protocol.  */
	}
    }
  return NULL_TREE;
}

/* This routine looks up the PROPERTY in current INTERFACE, its categories and up the
   chain of interface hierarchy.  */
static tree
lookup_property (tree interface_type, tree property)
{
  tree inter = interface_type;
  while (inter)
    {
      tree x, category;
      if ((x = lookup_property_in_list (inter, property)))
	return x;
      /* Failing that, look for the property in each category of the class.  */
      category = inter;
      while ((category = CLASS_CATEGORY_LIST (category)))
	{
	  if ((x = lookup_property_in_list (category, property)))
	    return x;

	  /* When checking a category, also check the protocols
	     attached with the category itself.  */
	  if (CLASS_PROTOCOL_LIST (category)
	      && (x = lookup_property_in_protocol_list
		  (CLASS_PROTOCOL_LIST (category), property)))
	    return x;
	}

      /*  Failing to find in categories, look for property in protocol list. */
      if (CLASS_PROTOCOL_LIST (inter)
	  && (x = lookup_property_in_protocol_list
	      (CLASS_PROTOCOL_LIST (inter), property)))
	return x;

      /* Failing that, climb up the inheritance hierarchy.  */
      inter = lookup_interface (CLASS_SUPER_NAME (inter));
    }
  return inter;
}

/* This routine is called by the parser when a
   @property... declaration is found.  'decl' is the declaration of
   the property (type/identifier), and the other arguments represent
   property attributes that may have been specified in the Objective-C
   declaration.  'parsed_property_readonly' is 'true' if the attribute
   'readonly' was specified, and 'false' if not; similarly for the
   other bool parameters.  'parsed_property_getter_ident' is NULL_TREE
   if the attribute 'getter' was not specified, and is the identifier
   corresponding to the specified getter if it was; similarly for
   'parsed_property_setter_ident'.  */
void
objc_add_property_declaration (location_t location, tree decl,
			       bool parsed_property_readonly, bool parsed_property_readwrite,
			       bool parsed_property_assign, bool parsed_property_retain,
			       bool parsed_property_copy, bool parsed_property_nonatomic,
			       tree parsed_property_getter_ident, tree parsed_property_setter_ident)
{
  tree property_decl;
  tree x;
  /* 'property_readonly' and 'property_assign_semantics' are the final
     attributes of the property after all parsed attributes have been
     considered (eg, if we parsed no 'readonly' and no 'readwrite', ie
     parsed_property_readonly = false and parsed_property_readwrite =
     false, then property_readonly will be false because the default
     is readwrite).  */
  bool property_readonly = false;
  objc_property_assign_semantics property_assign_semantics = OBJC_PROPERTY_ASSIGN;
  bool property_extension_in_class_extension = false;

  if (flag_objc1_only)
    error_at (input_location, "%<@property%> is not available in Objective-C 1.0");

  if (parsed_property_readonly && parsed_property_readwrite)
    {
      error_at (location, "%<readonly%> attribute conflicts with %<readwrite%> attribute");
      /* In case of conflicting attributes (here and below), after
	 producing an error, we pick one of the attributes and keep
	 going.  */
      property_readonly = false;
    }
  else
    {
      if (parsed_property_readonly)
	property_readonly = true;

      if (parsed_property_readwrite)
	property_readonly = false;
    }

  if (parsed_property_readonly && parsed_property_setter_ident)
    {
      error_at (location, "%<readonly%> attribute conflicts with %<setter%> attribute");
      property_readonly = false;
    }

  if (parsed_property_assign && parsed_property_retain)
    {
      error_at (location, "%<assign%> attribute conflicts with %<retain%> attribute");
      property_assign_semantics = OBJC_PROPERTY_RETAIN;
    }
  else if (parsed_property_assign && parsed_property_copy)
    {
      error_at (location, "%<assign%> attribute conflicts with %<copy%> attribute");
      property_assign_semantics = OBJC_PROPERTY_COPY;
    }
  else if (parsed_property_retain && parsed_property_copy)
    {
      error_at (location, "%<retain%> attribute conflicts with %<copy%> attribute");
      property_assign_semantics = OBJC_PROPERTY_COPY;
    }
  else
    {
      if (parsed_property_assign)
	property_assign_semantics = OBJC_PROPERTY_ASSIGN;

      if (parsed_property_retain)
	property_assign_semantics = OBJC_PROPERTY_RETAIN;

      if (parsed_property_copy)
	property_assign_semantics = OBJC_PROPERTY_COPY;
    }

  if (!objc_interface_context)
    {
      error_at (location, "property declaration not in @interface or @protocol context");
      return;
    }

  /* At this point we know that we are either in an interface, a
     category, or a protocol.  */

  /* We expect a FIELD_DECL from the parser.  Make sure we didn't get
     something else, as that would confuse the checks below.  */
  if (TREE_CODE (decl) != FIELD_DECL)
    {
      error_at (location, "invalid property declaration");
      return;
    }

  /* Do some spot-checks for the most obvious invalid types.  */

  if (TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE)
    {
      error_at (location, "property can not be an array");
      return;
    }

  /* The C++/ObjC++ parser seems to reject the ':' for a bitfield when
     parsing, while the C/ObjC parser accepts it and gives us a
     FIELD_DECL with a DECL_INITIAL set.  So we use the DECL_INITIAL
     to check for a bitfield when doing ObjC.  */
#ifndef OBJCPLUS
  if (DECL_INITIAL (decl))
    {
      /* A @property is not an actual variable, but it is a way to
	 describe a pair of accessor methods, so its type (which is
	 the type of the return value of the getter and the first
	 argument of the setter) can't be a bitfield (as return values
	 and arguments of functions can not be bitfields).  The
	 underlying instance variable could be a bitfield, but that is
	 a different matter.  */
      error_at (location, "property can not be a bit-field");
      return;
    }
#endif

  /* TODO: Check that the property type is an Objective-C object or a
     "POD".  */

  /* Implement -Wproperty-assign-default (which is enabled by default).  */
  if (warn_property_assign_default
      /* If garbage collection is not being used, then 'assign' is
	 valid for objects (and typically used for delegates) but it
	 is wrong in most cases (since most objects need to be
	 retained or copied in setters).  Warn users when 'assign' is
	 used implicitly.  */
      && property_assign_semantics == OBJC_PROPERTY_ASSIGN
      /* Read-only properties are never assigned, so the assignment
	 semantics do not matter in that case.  */
      && !property_readonly
      && !flag_objc_gc)
    {
      /* Please note that it would make sense to default to 'assign'
	 for non-{Objective-C objects}, and to 'retain' for
	 Objective-C objects.  But that would break compatibility with
	 other compilers.  */
      if (!parsed_property_assign && !parsed_property_retain && !parsed_property_copy)
	{
	  /* Use 'false' so we do not warn for Class objects.  */
	  if (objc_type_valid_for_messaging (TREE_TYPE (decl), false))
	    {
	      warning_at (location,
			  0,
			  "object property %qD has no %<assign%>, %<retain%> or %<copy%> attribute; assuming %<assign%>",
			  decl);
	      inform (location,
		      "%<assign%> can be unsafe for Objective-C objects; please state explicitly if you need it");
	    }
	}
    }

  if (property_assign_semantics == OBJC_PROPERTY_RETAIN
      && !objc_type_valid_for_messaging (TREE_TYPE (decl), true))
    error_at (location, "%<retain%> attribute is only valid for Objective-C objects");

  if (property_assign_semantics == OBJC_PROPERTY_COPY
      && !objc_type_valid_for_messaging (TREE_TYPE (decl), true))
    error_at (location, "%<copy%> attribute is only valid for Objective-C objects");

  /* Now determine the final property getter and setter names.  They
     will be stored in the PROPERTY_DECL, from which they'll always be
     extracted and used.  */

  /* Adjust, or fill in, setter and getter names.  We overwrite the
     parsed_property_setter_ident and parsed_property_getter_ident
     with the final setter and getter identifiers that will be
     used.  */
  if (parsed_property_setter_ident)
    {
      /* The setter should be terminated by ':', but the parser only
	 gives us an identifier without ':'.  So, we need to add ':'
	 at the end.  */
      const char *parsed_setter = IDENTIFIER_POINTER (parsed_property_setter_ident);
      size_t length = strlen (parsed_setter);
      char *final_setter = (char *)alloca (length + 2);

      sprintf (final_setter, "%s:", parsed_setter);
      parsed_property_setter_ident = get_identifier (final_setter);
    }
  else
    {
      if (!property_readonly)
	parsed_property_setter_ident = get_identifier (objc_build_property_setter_name
						       (DECL_NAME (decl)));
    }

  if (!parsed_property_getter_ident)
    parsed_property_getter_ident = DECL_NAME (decl);

  /* Check for duplicate property declarations.  We first check the
     immediate context for a property with the same name.  Any such
     declarations are an error, unless this is a class extension and
     we are extending a property from readonly to readwrite.  */
  for (x = CLASS_PROPERTY_DECL (objc_interface_context); x; x = TREE_CHAIN (x))
    {
      if (PROPERTY_NAME (x) == DECL_NAME (decl))
	{
	  if (objc_in_class_extension
	      && property_readonly == 0
	      && PROPERTY_READONLY (x) == 1)
	    {
	      /* This is a class extension, and we are extending an
		 existing readonly property to a readwrite one.
		 That's fine.  :-) */
	      property_extension_in_class_extension = true;
	      break;
	    }
	  else
	    {
	      location_t original_location = DECL_SOURCE_LOCATION (x);

	      error_at (location, "redeclaration of property %qD", decl);

	      if (original_location != UNKNOWN_LOCATION)
		inform (original_location, "originally specified here");
	      return;
	    }
	}
    }

  /* If x is not NULL_TREE, we must be in a class extension and we're
     extending a readonly property.  In that case, no point in
     searching for another declaration.  */
  if (x == NULL_TREE)
    {
      /* We now need to check for existing property declarations (in
	 the superclass, other categories or protocols) and check that
	 the new declaration is not in conflict with existing
	 ones.  */

      /* Search for a previous, existing declaration of a property
	 with the same name in superclasses, protocols etc.  If one is
	 found, it will be in the 'x' variable.  */

      /* Note that, for simplicity, the following may search again the
	 local context.  That's Ok as nothing will be found (else we'd
	 have thrown an error above); it's only a little inefficient,
	 but the code is simpler.  */
      switch (TREE_CODE (objc_interface_context))
	{
	case CLASS_INTERFACE_TYPE:
	  /* Look up the property in the current @interface (which
	     will find nothing), then its protocols and categories and
	     superclasses.  */
	  x = lookup_property (objc_interface_context, DECL_NAME (decl));
	  break;
	case CATEGORY_INTERFACE_TYPE:
	  /* Look up the property in the main @interface, then
	     protocols and categories (one of them is ours, and will
	     find nothing) and superclasses.  */
	  x = lookup_property (lookup_interface (CLASS_NAME (objc_interface_context)),
			       DECL_NAME (decl));
	  break;
	case PROTOCOL_INTERFACE_TYPE:
	  /* Looks up the property in any protocols attached to the
	     current protocol.  */
	  if (PROTOCOL_LIST (objc_interface_context))
	    {
	      x = lookup_property_in_protocol_list (PROTOCOL_LIST (objc_interface_context),
						    DECL_NAME (decl));
	    }
	  break;
	default:
	  gcc_unreachable ();
	}
    }

  if (x != NULL_TREE)
    {
      /* An existing property was found; check that it has the same
	 types, or it is compatible.  */
      location_t original_location = DECL_SOURCE_LOCATION (x);

      if (PROPERTY_NONATOMIC (x) != parsed_property_nonatomic)
	{
	  warning_at (location, 0,
		      "'nonatomic' attribute of property %qD conflicts with previous declaration", decl);

	  if (original_location != UNKNOWN_LOCATION)
	    inform (original_location, "originally specified here");
	  return;
	}

      if (PROPERTY_GETTER_NAME (x) != parsed_property_getter_ident)
	{
	  warning_at (location, 0,
		      "'getter' attribute of property %qD conflicts with previous declaration", decl);

	  if (original_location != UNKNOWN_LOCATION)
	    inform (original_location, "originally specified here");
	  return;
	}

      /* We can only compare the setter names if both the old and new property have a setter.  */
      if (!property_readonly  &&  !PROPERTY_READONLY(x))
	{
	  if (PROPERTY_SETTER_NAME (x) != parsed_property_setter_ident)
	    {
	      warning_at (location, 0,
			  "'setter' attribute of property %qD conflicts with previous declaration", decl);

	      if (original_location != UNKNOWN_LOCATION)
		inform (original_location, "originally specified here");
	      return;
	    }
	}

      if (PROPERTY_ASSIGN_SEMANTICS (x) != property_assign_semantics)
	{
	  warning_at (location, 0,
		      "assign semantics attributes of property %qD conflict with previous declaration", decl);

	  if (original_location != UNKNOWN_LOCATION)
	    inform (original_location, "originally specified here");
	  return;
	}

      /* It's ok to have a readonly property that becomes a readwrite, but not vice versa.  */
      if (PROPERTY_READONLY (x) == 0  &&  property_readonly == 1)
	{
	  warning_at (location, 0,
		      "'readonly' attribute of property %qD conflicts with previous declaration", decl);

	  if (original_location != UNKNOWN_LOCATION)
	    inform (original_location, "originally specified here");
	  return;
	}

      /* We now check that the new and old property declarations have
	 the same types (or compatible one).  In the Objective-C
	 tradition of loose type checking, we do type-checking but
	 only generate warnings (not errors) if they do not match.
	 For non-readonly properties, the types must match exactly;
	 for readonly properties, it is allowed to use a "more
	 specialized" type in the new property declaration.  Eg, the
	 superclass has a getter returning (NSArray *) and the
	 subclass a getter returning (NSMutableArray *).  The object's
	 getter returns an (NSMutableArray *); but if you cast the
	 object to the superclass, which is allowed, you'd still
	 expect the getter to return an (NSArray *), which works since
	 an (NSMutableArray *) is an (NSArray *) too.  So, the set of
	 objects belonging to the type of the new @property should be
	 a subset of the set of objects belonging to the type of the
	 old @property.  This is what "specialization" means.  And the
	 reason it only applies to readonly properties is that for a
	 readwrite property the setter would have the opposite
	 requirement - ie that the superclass type is more specialized
	 then the subclass one; hence the only way to satisfy both
	 constraints is that the types match.  */

      /* If the types are not the same in the C sense, we warn ...  */
      if (!comptypes (TREE_TYPE (x), TREE_TYPE (decl))
	  /* ... unless the property is readonly, in which case we
	     allow a new, more specialized, declaration.  */
	  && (!property_readonly
	      || !objc_compare_types (TREE_TYPE (x),
				      TREE_TYPE (decl), -5, NULL_TREE)))
	{
	  warning_at (location, 0,
		      "type of property %qD conflicts with previous declaration", decl);
	  if (original_location != UNKNOWN_LOCATION)
	    inform (original_location, "originally specified here");
	  return;
	}

      /* If we are in a class extension and we're extending a readonly
	 property in the main @interface, we'll just update the
	 existing property with the readwrite flag and potentially the
	 new setter name.  */
      if (property_extension_in_class_extension)
	{
	  PROPERTY_READONLY (x) = 0;
	  PROPERTY_SETTER_NAME (x) = parsed_property_setter_ident;
	  return;
	}
    }

  /* Create a PROPERTY_DECL node.  */
  property_decl = make_node (PROPERTY_DECL);

  /* Copy the basic information from the original decl.  */
  TREE_TYPE (property_decl) = TREE_TYPE (decl);
  DECL_SOURCE_LOCATION (property_decl) = DECL_SOURCE_LOCATION (decl);
  TREE_DEPRECATED (property_decl) = TREE_DEPRECATED (decl);

  /* Add property-specific information.  */
  PROPERTY_NAME (property_decl) = DECL_NAME (decl);
  PROPERTY_GETTER_NAME (property_decl) = parsed_property_getter_ident;
  PROPERTY_SETTER_NAME (property_decl) = parsed_property_setter_ident;
  PROPERTY_READONLY (property_decl) = property_readonly;
  PROPERTY_NONATOMIC (property_decl) = parsed_property_nonatomic;
  PROPERTY_ASSIGN_SEMANTICS (property_decl) = property_assign_semantics;
  PROPERTY_IVAR_NAME (property_decl) = NULL_TREE;
  PROPERTY_DYNAMIC (property_decl) = 0;

  /* Remember the fact that the property was found in the @optional
     section in a @protocol, or not.  */
  if (objc_method_optional_flag)
    PROPERTY_OPTIONAL (property_decl) = 1;
  else
    PROPERTY_OPTIONAL (property_decl) = 0;

  /* Note that PROPERTY_GETTER_NAME is always set for all
     PROPERTY_DECLs, and PROPERTY_SETTER_NAME is always set for all
     PROPERTY_DECLs where PROPERTY_READONLY == 0.  Any time we deal
     with a getter or setter, we should get the PROPERTY_DECL and use
     PROPERTY_GETTER_NAME and PROPERTY_SETTER_NAME to know the correct
     names.  */

  /* Add the PROPERTY_DECL to the list of properties for the class.  */
  TREE_CHAIN (property_decl) = CLASS_PROPERTY_DECL (objc_interface_context);
  CLASS_PROPERTY_DECL (objc_interface_context) = property_decl;
}

/* This is a subroutine of objc_maybe_build_component_ref.  Search the
   list of methods in the interface (and, failing that, the local list
   in the implementation, and failing that, the protocol list)
   provided for a 'setter' or 'getter' for 'component' with default
   names (ie, if 'component' is "name", then search for "name" and
   "setName:").  It is also possible to specify a different
   'getter_name' (this is used for @optional readonly properties).  If
   any is found, then create an artificial property that uses them.
   Return NULL_TREE if 'getter' or 'setter' could not be found.  */
static tree
maybe_make_artificial_property_decl (tree interface, tree implementation,
				     tree protocol_list, tree component, bool is_class,
				     tree getter_name)
{
  tree setter_name = get_identifier (objc_build_property_setter_name (component));
  tree getter = NULL_TREE;
  tree setter = NULL_TREE;

  if (getter_name == NULL_TREE)
    getter_name = component;

  /* First, check the @interface and all superclasses.  */
  if (interface)
    {
      int flags = 0;

      /* Using instance methods of the root class as accessors is most
	 likely unwanted and can be extremely confusing (and, most
	 importantly, other Objective-C 2.0 compilers do not do it).
	 Turn it off.  */
      if (is_class)
	flags = OBJC_LOOKUP_CLASS | OBJC_LOOKUP_NO_INSTANCE_METHODS_OF_ROOT_CLASS;

      getter = lookup_method_static (interface, getter_name, flags);
      setter = lookup_method_static (interface, setter_name, flags);
    }

  /* Second, check the local @implementation context.  */
  if (!getter && !setter)
    {
      if (implementation)
	{
	  if (is_class)
	    {
	      getter = lookup_method (CLASS_CLS_METHODS (implementation), getter_name);
	      setter = lookup_method (CLASS_CLS_METHODS (implementation), setter_name);
	    }
	  else
	    {
	      getter = lookup_method (CLASS_NST_METHODS (implementation), getter_name);
	      setter = lookup_method (CLASS_NST_METHODS (implementation), setter_name);
	    }
	}
    }

  /* Try the protocol_list if we didn't find anything in the
     @interface and in the @implementation.  */
  if (!getter && !setter)
    {
      getter = lookup_method_in_protocol_list (protocol_list, getter_name, is_class);
      setter = lookup_method_in_protocol_list (protocol_list, setter_name, is_class);
    }

  /* There needs to be at least a getter or setter for this to be a
     valid 'object.component' syntax.  */
  if (getter || setter)
    {
      /* Yes ... determine the type of the expression.  */
      tree property_decl;
      tree type;

      if (getter)
	type = TREE_VALUE (TREE_TYPE (getter));
      else
	type = TREE_VALUE (TREE_TYPE (METHOD_SEL_ARGS (setter)));

      /* Create an artificial property declaration with the
	 information we collected on the type and getter/setter
	 names.  */
      property_decl = make_node (PROPERTY_DECL);

      TREE_TYPE (property_decl) = type;
      DECL_SOURCE_LOCATION (property_decl) = input_location;
      TREE_DEPRECATED (property_decl) = 0;
      DECL_ARTIFICIAL (property_decl) = 1;

      /* Add property-specific information.  Note that one of
	 PROPERTY_GETTER_NAME or PROPERTY_SETTER_NAME may refer to a
	 non-existing method; this will generate an error when the
	 expression is later compiled.  At this stage we don't know if
	 the getter or setter will be used, so we can't generate an
	 error.  */
      PROPERTY_NAME (property_decl) = component;
      PROPERTY_GETTER_NAME (property_decl) = getter_name;
      PROPERTY_SETTER_NAME (property_decl) = setter_name;
      PROPERTY_READONLY (property_decl) = 0;
      PROPERTY_NONATOMIC (property_decl) = 0;
      PROPERTY_ASSIGN_SEMANTICS (property_decl) = 0;
      PROPERTY_IVAR_NAME (property_decl) = NULL_TREE;
      PROPERTY_DYNAMIC (property_decl) = 0;
      PROPERTY_OPTIONAL (property_decl) = 0;

      if (!getter)
	PROPERTY_HAS_NO_GETTER (property_decl) = 1;

      /* The following is currently unused, but it's nice to have
	 there.  We may use it if we need in the future.  */
      if (!setter)
	PROPERTY_HAS_NO_SETTER (property_decl) = 1;

      return property_decl;
    }

  return NULL_TREE;
}

/* This hook routine is invoked by the parser when an expression such
   as 'xxx.yyy' is parsed.  We get a chance to process these
   expressions in a way that is specified to Objective-C (to implement
   the Objective-C 2.0 dot-syntax, properties, or non-fragile ivars).
   If the expression is not an Objective-C specified expression, we
   should return NULL_TREE; else we return the expression.

   At the moment this only implements dot-syntax and properties (not
   non-fragile ivars yet), ie 'object.property' or 'object.component'
   where 'component' is not a declared property, but a valid getter or
   setter for it could be found.  */
tree
objc_maybe_build_component_ref (tree object, tree property_ident)
{
  tree x = NULL_TREE;
  tree rtype;

  /* If we are in Objective-C 1.0 mode, dot-syntax and properties are
     not available.  */
  if (flag_objc1_only)
    return NULL_TREE;

  /* Try to determine if 'object' is an Objective-C object or not.  If
     not, return.  */
  if (object == NULL_TREE || object == error_mark_node
      || (rtype = TREE_TYPE (object)) == NULL_TREE)
    return NULL_TREE;

  if (property_ident == NULL_TREE || property_ident == error_mark_node
      || TREE_CODE (property_ident) != IDENTIFIER_NODE)
    return NULL_TREE;

  /* The following analysis of 'object' is similar to the one used for
     the 'receiver' of a method invocation.  We need to determine what
     'object' is and find the appropriate property (either declared,
     or artificial) for it (in the same way as we need to find the
     appropriate method prototype for a method invocation).  There are
     some simplifications here though: "object.property" is invalid if
     "object" has a type of "id" or "Class"; it must at least have a
     protocol attached to it, and "object" is never a class name as
     that is done by objc_build_class_component_ref.  Finally, we
     don't know if this really is a dot-syntax expression, so we want
     to make a quick exit if it is not; for this reason, we try to
     postpone checks after determining that 'object' looks like an
     Objective-C object.  */

  if (objc_is_id (rtype))
    {
      /* This is the case that the 'object' is of type 'id' or
	 'Class'.  */

      /* Check if at least it is of type 'id <Protocol>' or 'Class
	 <Protocol>'; if so, look the property up in the
	 protocols.  */
      if (TYPE_HAS_OBJC_INFO (TREE_TYPE (rtype)))
	{
	  tree rprotos = TYPE_OBJC_PROTOCOL_LIST (TREE_TYPE (rtype));

	  if (rprotos)
	    {
	      /* No point looking up declared @properties if we are
		 dealing with a class.  Classes have no declared
		 properties.  */
	      if (!IS_CLASS (rtype))
		x = lookup_property_in_protocol_list (rprotos, property_ident);

	      if (x == NULL_TREE)
		{
		  /* Ok, no property.  Maybe it was an
		     object.component dot-syntax without a declared
		     property (this is valid for classes too).  Look
		     for getter/setter methods and internally declare
		     an artificial property based on them if found.  */
		  x = maybe_make_artificial_property_decl (NULL_TREE,
							   NULL_TREE,
							   rprotos,
							   property_ident,
							   IS_CLASS (rtype),
							   NULL_TREE);
		}
	      else if (PROPERTY_OPTIONAL (x) && PROPERTY_READONLY (x))
		{
		  /* This is a special, complicated case.  If the
		     property is optional, and is read-only, then the
		     property is always used for reading, but an
		     eventual existing non-property setter can be used
		     for writing.  We create an artificial property
		     decl copying the getter from the optional
		     property, and looking up the setter in the
		     interface.  */
		  x = maybe_make_artificial_property_decl (NULL_TREE,
							   NULL_TREE,
							   rprotos,
							   property_ident,
							   false,
							   PROPERTY_GETTER_NAME (x));
		}
	    }
	}
      else if (objc_method_context)
	{
	  /* Else, if we are inside a method it could be the case of
	     'super' or 'self'.  */
	  tree interface_type = NULL_TREE;
	  tree t = object;
	  while (TREE_CODE (t) == COMPOUND_EXPR
		 || TREE_CODE (t) == MODIFY_EXPR
		 || CONVERT_EXPR_P (t)
		 || TREE_CODE (t) == COMPONENT_REF)
	    t = TREE_OPERAND (t, 0);

	  if (t == UOBJC_SUPER_decl)
	    interface_type = lookup_interface (CLASS_SUPER_NAME (implementation_template));
	  else if (t == self_decl)
	    interface_type = lookup_interface (CLASS_NAME (implementation_template));

	  if (interface_type)
	    {
	      if (TREE_CODE (objc_method_context) != CLASS_METHOD_DECL)
		x = lookup_property (interface_type, property_ident);

	      if (x == NULL_TREE)
		{
		  /* Try the dot-syntax without a declared property.
		     If this is an access to 'self', it is possible
		     that they may refer to a setter/getter that is
		     not declared in the interface, but exists locally
		     in the implementation.  In that case, get the
		     implementation context and use it.  */
		  tree implementation = NULL_TREE;

		  if (t == self_decl)
		    implementation = objc_implementation_context;

		  x = maybe_make_artificial_property_decl
		    (interface_type, implementation, NULL_TREE,
		     property_ident,
		     (TREE_CODE (objc_method_context) == CLASS_METHOD_DECL),
		     NULL_TREE);
		}
	      else if (PROPERTY_OPTIONAL (x) && PROPERTY_READONLY (x))
		{
		  tree implementation = NULL_TREE;

		  if (t == self_decl)
		    implementation = objc_implementation_context;

		  x = maybe_make_artificial_property_decl (interface_type,
							   implementation,
							   NULL_TREE,
							   property_ident,
							   false,
							   PROPERTY_GETTER_NAME (x));
		}
	    }
	}
    }
  else
    {
      /* This is the case where we have more information on 'rtype'.  */
      tree basetype = TYPE_MAIN_VARIANT (rtype);

      /* Skip the pointer - if none, it's not an Objective-C object or
	 class.  */
      if (basetype != NULL_TREE && TREE_CODE (basetype) == POINTER_TYPE)
	basetype = TREE_TYPE (basetype);
      else
	return NULL_TREE;

      /* Traverse typedefs.  */
      while (basetype != NULL_TREE
	     && TREE_CODE (basetype) == RECORD_TYPE
	     && OBJC_TYPE_NAME (basetype)
	     && TREE_CODE (OBJC_TYPE_NAME (basetype)) == TYPE_DECL
	     && DECL_ORIGINAL_TYPE (OBJC_TYPE_NAME (basetype)))
	basetype = DECL_ORIGINAL_TYPE (OBJC_TYPE_NAME (basetype));

      if (basetype != NULL_TREE && TYPED_OBJECT (basetype))
	{
	  tree interface_type = TYPE_OBJC_INTERFACE (basetype);
	  tree protocol_list = TYPE_OBJC_PROTOCOL_LIST (basetype);

	  if (interface_type
	      && (TREE_CODE (interface_type) == CLASS_INTERFACE_TYPE
		  || TREE_CODE (interface_type) == CATEGORY_INTERFACE_TYPE
		  || TREE_CODE (interface_type) == PROTOCOL_INTERFACE_TYPE))
	    {
	      /* Not sure 'rtype' could ever be a class here!  Just
		 for safety we keep the checks.  */
	      if (!IS_CLASS (rtype))
		{
		  x = lookup_property (interface_type, property_ident);

		  if (x == NULL_TREE)
		    x = lookup_property_in_protocol_list (protocol_list,
							  property_ident);
		}

	      if (x == NULL_TREE)
		{
		  /* Try the dot-syntax without a declared property.
		     If we are inside a method implementation, it is
		     possible that they may refer to a setter/getter
		     that is not declared in the interface, but exists
		     locally in the implementation.  In that case, get
		     the implementation context and use it.  */
		  tree implementation = NULL_TREE;

		  if (objc_implementation_context
		      && CLASS_NAME (objc_implementation_context)
		      == OBJC_TYPE_NAME (interface_type))
		    implementation = objc_implementation_context;

		  x = maybe_make_artificial_property_decl (interface_type,
							   implementation,
							   protocol_list,
							   property_ident,
							   IS_CLASS (rtype),
							   NULL_TREE);
		}
	      else if (PROPERTY_OPTIONAL (x) && PROPERTY_READONLY (x))
		{
		  tree implementation = NULL_TREE;

		  if (objc_implementation_context
		      && CLASS_NAME (objc_implementation_context)
		      == OBJC_TYPE_NAME (interface_type))
		    implementation = objc_implementation_context;

		  x = maybe_make_artificial_property_decl (interface_type,
							   implementation,
							   protocol_list,
							   property_ident,
							   false,
							   PROPERTY_GETTER_NAME (x));
		}
	    }
	}
    }

  if (x)
    {
      tree expression;
      tree getter_call;
      tree deprecated_method_prototype = NULL_TREE;

      /* We have an additional nasty problem here; if this
	 PROPERTY_REF needs to become a 'getter', then the conversion
	 from PROPERTY_REF into a getter call happens in gimplify,
	 after the selector table has already been generated and when
	 it is too late to add another selector to it.  To work around
	 the problem, we always create the getter call at this stage,
	 which puts the selector in the table.  Note that if the
	 PROPERTY_REF becomes a 'setter' instead of a 'getter', then
	 we have added a selector too many to the selector table.
	 This is a little inefficient.

	 Also note that method calls to 'self' and 'super' require the
	 context (self_decl, UOBJS_SUPER_decl,
	 objc_implementation_context etc) to be built correctly; this
	 is yet another reason why building the call at the gimplify
	 stage (when this context has been lost) is not very
	 practical.  If we build it at this stage, we know it will
	 always be built correctly.

	 If the PROPERTY_HAS_NO_GETTER() (ie, it is an artificial
	 property decl created to deal with a dotsyntax not really
	 referring to an existing property) then do not try to build a
	 call to the getter as there is no getter.  */
      if (PROPERTY_HAS_NO_GETTER (x))
	getter_call = NULL_TREE;
      else
	getter_call = objc_finish_message_expr
	  (object, PROPERTY_GETTER_NAME (x), NULL_TREE,
	   /* Disable the immediate deprecation warning if the getter
	      is deprecated, but record the fact that the getter is
	      deprecated by setting PROPERTY_REF_DEPRECATED_GETTER to
	      the method prototype.  */
	   &deprecated_method_prototype);

      expression = build4 (PROPERTY_REF, TREE_TYPE(x), object, x, getter_call,
			   deprecated_method_prototype);
      SET_EXPR_LOCATION (expression, input_location);
      TREE_SIDE_EFFECTS (expression) = 1;

      return expression;
    }

  return NULL_TREE;
}

/* This hook routine is invoked by the parser when an expression such
   as 'xxx.yyy' is parsed, and 'xxx' is a class name.  This is the
   Objective-C 2.0 dot-syntax applied to classes, so we need to
   convert it into a setter/getter call on the class.  */
tree
objc_build_class_component_ref (tree class_name, tree property_ident)
{
  tree x = NULL_TREE;
  tree object, rtype;

  if (flag_objc1_only)
    error_at (input_location, "the dot syntax is not available in Objective-C 1.0");

  if (class_name == NULL_TREE || class_name == error_mark_node
      || TREE_CODE (class_name) != IDENTIFIER_NODE)
    return error_mark_node;

  if (property_ident == NULL_TREE || property_ident == error_mark_node
      || TREE_CODE (property_ident) != IDENTIFIER_NODE)
    return NULL_TREE;

  object = objc_get_class_reference (class_name);
  if (!object)
    {
      /* We know that 'class_name' is an Objective-C class name as the
	 parser won't call this function if it is not.  This is only a
	 double-check for safety.  */
      error_at (input_location, "could not find class %qE", class_name);
      return error_mark_node;
    }

  rtype = lookup_interface (class_name);
  if (!rtype)
    {
      /* Again, this should never happen, but we do check.  */
      error_at (input_location, "could not find interface for class %qE", class_name);
      return error_mark_node;
    }
  else
    {
      if (TREE_DEPRECATED (rtype))
	warning (OPT_Wdeprecated_declarations, "class %qE is deprecated", class_name);
    }

  x = maybe_make_artificial_property_decl (rtype, NULL_TREE, NULL_TREE,
					   property_ident,
					   true, NULL_TREE);

  if (x)
    {
      tree expression;
      tree getter_call;
      tree deprecated_method_prototype = NULL_TREE;

      if (PROPERTY_HAS_NO_GETTER (x))
	getter_call = NULL_TREE;
      else
	getter_call = objc_finish_message_expr
	  (object, PROPERTY_GETTER_NAME (x), NULL_TREE,
	   &deprecated_method_prototype);

      expression = build4 (PROPERTY_REF, TREE_TYPE(x), object, x, getter_call,
			   deprecated_method_prototype);
      SET_EXPR_LOCATION (expression, input_location);
      TREE_SIDE_EFFECTS (expression) = 1;

      return expression;
    }
  else
    {
      error_at (input_location, "could not find setter/getter for %qE in class %qE",
		property_ident,	class_name);
      return error_mark_node;
    }

  return NULL_TREE;
}



/* This is used because we don't want to expose PROPERTY_REF to the
   C/C++ frontends.  Maybe we should!  */
bool
objc_is_property_ref (tree node)
{
  if (node  &&  TREE_CODE (node) == PROPERTY_REF)
    return true;
  else
    return false;
}

/* This function builds a setter call for a PROPERTY_REF (real, for a
   declared property, or artificial, for a dot-syntax accessor which
   is not corresponding to a property).  'lhs' must be a PROPERTY_REF
   (the caller must check this beforehand).  'rhs' is the value to
   assign to the property.  A plain setter call is returned, or
   error_mark_node if the property is readonly.  */

static tree
objc_build_setter_call (tree lhs, tree rhs)
{
  tree object_expr = PROPERTY_REF_OBJECT (lhs);
  tree property_decl = PROPERTY_REF_PROPERTY_DECL (lhs);

  if (PROPERTY_READONLY (property_decl))
    {
      error ("readonly property can not be set");
      return error_mark_node;
    }
  else
    {
      tree setter_argument = build_tree_list (NULL_TREE, rhs);
      tree setter;

      /* TODO: Check that the setter return type is 'void'.  */

      /* TODO: Decay arguments in C.  */
      setter = objc_finish_message_expr (object_expr,
					 PROPERTY_SETTER_NAME (property_decl),
					 setter_argument, NULL);
      return setter;
    }

  /* Unreachable, but the compiler may not realize.  */
  return error_mark_node;
}

/* This hook routine is called when a MODIFY_EXPR is being built.  We
   check what is being modified; if it is a PROPERTY_REF, we need to
   generate a 'setter' function call for the property.  If this is not
   a PROPERTY_REF, we return NULL_TREE and the C/C++ frontend will go
   on creating their MODIFY_EXPR.

   This is used for example if you write

   object.count = 1;

   where 'count' is a property.  The left-hand side creates a
   PROPERTY_REF, and then the compiler tries to generate a MODIFY_EXPR
   to assign something to it.  We intercept that here, and generate a
   call to the 'setter' method instead.  */
tree
objc_maybe_build_modify_expr (tree lhs, tree rhs)
{
  if (lhs && TREE_CODE (lhs) == PROPERTY_REF)
    {
      /* Building a simple call to the setter method would work for cases such as

      object.count = 1;

      but wouldn't work for cases such as

      count = object2.count = 1;

      to get these to work with very little effort, we build a
      compound statement which does the setter call (to set the
      property to 'rhs'), but which can also be evaluated returning
      the 'rhs'.  If the 'rhs' has no side effects, we can simply
      evaluate it twice, building

      ([object setProperty: rhs]; rhs)

      If it has side effects, we put it in a temporary variable first,
      so we create the following:

      (temp = rhs; [object setProperty: temp]; temp)

      setter_argument is rhs in the first case, and temp in the second
      case.
      */
      tree setter_argument;

      /* s1, s2 and s3 are the tree statements that we need in the
	 compound expression.  */
      tree s1, s2, s3, compound_expr;

      if (TREE_SIDE_EFFECTS (rhs))
	{
	  tree bind;

	  /* Declare __objc_property_temp in a local bind.  */
	  setter_argument = objc_create_temporary_var (TREE_TYPE (rhs), "__objc_property_temp");
	  DECL_SOURCE_LOCATION (setter_argument) = input_location;
	  bind = build3 (BIND_EXPR, void_type_node, setter_argument, NULL, NULL);
	  SET_EXPR_LOCATION (bind, input_location);
	  TREE_SIDE_EFFECTS (bind) = 1;
	  add_stmt (bind);

	  /* s1: x = rhs */
	  s1 = build_modify_expr (input_location, setter_argument, NULL_TREE,
				  NOP_EXPR,
				  input_location, rhs, NULL_TREE);
	  SET_EXPR_LOCATION (s1, input_location);
	}
      else
	{
	  /* No s1.  */
	  setter_argument = rhs;
	  s1 = NULL_TREE;
	}

      /* Now build the compound statement.  */

      /* s2: [object setProperty: x] */
      s2 = objc_build_setter_call (lhs, setter_argument);

      /* This happens if building the setter failed because the
	 property is readonly.  */
      if (s2 == error_mark_node)
	return error_mark_node;

      SET_EXPR_LOCATION (s2, input_location);

      /* s3: x */
      s3 = convert (TREE_TYPE (lhs), setter_argument);

      /* Now build the compound statement (s1, s2, s3) or (s2, s3) as
	 appropriate.  */
      if (s1)
	compound_expr = build_compound_expr (input_location, build_compound_expr (input_location, s1, s2), s3);
      else
	compound_expr = build_compound_expr (input_location, s2, s3);

      /* Without this, with -Wall you get a 'valued computed is not
	 used' every time there is a "object.property = x" where the
	 value of the resulting MODIFY_EXPR is not used.  That is
	 correct (maybe a more sophisticated implementation could
	 avoid generating the compound expression if not needed), but
	 we need to turn it off.  */
      TREE_NO_WARNING (compound_expr) = 1;
      return compound_expr;
    }
  else
    return NULL_TREE;
}

/* This hook is called by the frontend when one of the four unary
   expressions PREINCREMENT_EXPR, POSTINCREMENT_EXPR,
   PREDECREMENT_EXPR and POSTDECREMENT_EXPR is being built with an
   argument which is a PROPERTY_REF.  For example, this happens if you have

   object.count++;

   where 'count' is a property.  We need to use the 'getter' and
   'setter' for the property in an appropriate way to build the
   appropriate expression.  'code' is the code for the expression (one
   of the four mentioned above); 'argument' is the PROPERTY_REF, and
   'increment' is how much we need to add or subtract.  */
tree
objc_build_incr_expr_for_property_ref (location_t location,
				       enum tree_code code,
				       tree argument, tree increment)
{
  /* Here are the expressions that we want to build:

     For PREINCREMENT_EXPR / PREDECREMENT_EXPR:
    (temp = [object property] +/- increment, [object setProperty: temp], temp)

    For POSTINCREMENT_EXPR / POSTECREMENT_EXPR:
    (temp = [object property], [object setProperty: temp +/- increment], temp) */

  tree temp_variable_decl, bind;
  /* s1, s2 and s3 are the tree statements that we need in the
     compound expression.  */
  tree s1, s2, s3, compound_expr;

  /* Safety check.  */
  if (!argument || TREE_CODE (argument) != PROPERTY_REF)
    return error_mark_node;

  /* Declare __objc_property_temp in a local bind.  */
  temp_variable_decl = objc_create_temporary_var (TREE_TYPE (argument), "__objc_property_temp");
  DECL_SOURCE_LOCATION (temp_variable_decl) = location;
  bind = build3 (BIND_EXPR, void_type_node, temp_variable_decl, NULL, NULL);
  SET_EXPR_LOCATION (bind, location);
  TREE_SIDE_EFFECTS (bind) = 1;
  add_stmt (bind);

  /* Now build the compound statement.  */

  /* Note that the 'getter' is generated at gimplify time; at this
     time, we can simply put the property_ref (ie, argument) wherever
     we want the getter ultimately to be.  */

  /* s1: __objc_property_temp = [object property] <+/- increment> */
  switch (code)
    {
    case PREINCREMENT_EXPR:
      /* __objc_property_temp = [object property] + increment */
      s1 = build_modify_expr (location, temp_variable_decl, NULL_TREE,
			      NOP_EXPR,
			      location, build2 (PLUS_EXPR, TREE_TYPE (argument),
						argument, increment), NULL_TREE);
      break;
    case PREDECREMENT_EXPR:
      /* __objc_property_temp = [object property] - increment */
      s1 = build_modify_expr (location, temp_variable_decl, NULL_TREE,
			      NOP_EXPR,
			      location, build2 (MINUS_EXPR, TREE_TYPE (argument),
						argument, increment), NULL_TREE);
      break;
    case POSTINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
      /* __objc_property_temp = [object property] */
      s1 = build_modify_expr (location, temp_variable_decl, NULL_TREE,
			      NOP_EXPR,
			      location, argument, NULL_TREE);
      break;
    default:
      gcc_unreachable ();
    }

  /* s2: [object setProperty: __objc_property_temp <+/- increment>] */
  switch (code)
    {
    case PREINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
      /* [object setProperty: __objc_property_temp] */
      s2 = objc_build_setter_call (argument, temp_variable_decl);
      break;
    case POSTINCREMENT_EXPR:
      /* [object setProperty: __objc_property_temp + increment] */
      s2 = objc_build_setter_call (argument,
				   build2 (PLUS_EXPR, TREE_TYPE (argument),
					   temp_variable_decl, increment));
      break;
    case POSTDECREMENT_EXPR:
      /* [object setProperty: __objc_property_temp - increment] */
      s2 = objc_build_setter_call (argument,
				   build2 (MINUS_EXPR, TREE_TYPE (argument),
					   temp_variable_decl, increment));
      break;
    default:
      gcc_unreachable ();
    }

  /* This happens if building the setter failed because the property
     is readonly.  */
  if (s2 == error_mark_node)
    return error_mark_node;

  SET_EXPR_LOCATION (s2, location);

  /* s3: __objc_property_temp */
  s3 = convert (TREE_TYPE (argument), temp_variable_decl);

  /* Now build the compound statement (s1, s2, s3) */
  compound_expr = build_compound_expr (location, build_compound_expr (location, s1, s2), s3);

  /* Prevent C++ from warning with -Wall that "right operand of comma
     operator has no effect".  */
  TREE_NO_WARNING (compound_expr) = 1;
  return compound_expr;
}

tree
objc_build_method_signature (bool is_class_method, tree rettype, tree selector,
			     tree optparms, bool ellipsis)
{
  if (is_class_method)
    return build_method_decl (CLASS_METHOD_DECL, rettype, selector,
			      optparms, ellipsis);
  else
    return build_method_decl (INSTANCE_METHOD_DECL, rettype, selector,
			      optparms, ellipsis);
}

void
objc_add_method_declaration (bool is_class_method, tree decl, tree attributes)
{
  if (!objc_interface_context)
    {
      /* PS: At the moment, due to how the parser works, it should be
	 impossible to get here.  But it's good to have the check in
	 case the parser changes.
      */
      fatal_error ("method declaration not in @interface context");
    }

  if (flag_objc1_only && attributes)
    error_at (input_location, "method attributes are not available in Objective-C 1.0");

  objc_decl_method_attributes (&decl, attributes, 0);
  objc_add_method (objc_interface_context,
		   decl,
		   is_class_method,
		   objc_method_optional_flag);
}

/* Return 'true' if the method definition could be started, and
   'false' if not (because we are outside an @implementation context).
   EXPR is NULL or an expression that needs to be evaluated for the
   side effects of array size expressions in the parameters.
*/
bool
objc_start_method_definition (bool is_class_method, tree decl, tree attributes,
			      tree expr)
{
  if (!objc_implementation_context)
    {
      error ("method definition not in @implementation context");
      return false;
    }

  if (decl != NULL_TREE  && METHOD_SEL_NAME (decl) == error_mark_node)
    return false;

#ifndef OBJCPLUS
  /* Indicate no valid break/continue context by setting these variables
     to some non-null, non-label value.  We'll notice and emit the proper
     error message in c_finish_bc_stmt.  */
  c_break_label = c_cont_label = size_zero_node;
#endif

  if (attributes)
    warning_at (input_location, 0, "method attributes can not be specified in @implementation context");
  else
    objc_decl_method_attributes (&decl, attributes, 0);

  objc_add_method (objc_implementation_context,
		   decl,
		   is_class_method,
		   /* is optional */ false);
  start_method_def (decl, expr);
  return true;
}

void
objc_add_instance_variable (tree decl)
{
  (void) add_instance_variable (objc_ivar_context,
				objc_ivar_visibility,
				decl);
}

/* Construct a C struct with same name as KLASS, a base struct with tag
   SUPER_NAME (if any), and FIELDS indicated.  */

static tree
objc_build_struct (tree klass, tree fields, tree super_name)
{
  tree name = CLASS_NAME (klass);
  tree s = objc_start_struct (name);
  tree super = (super_name ? xref_tag (RECORD_TYPE, super_name) : NULL_TREE);
  tree t;
  vec<tree> objc_info = vNULL;
  int i;

  if (super)
    {
      /* Prepend a packed variant of the base class into the layout.  This
	 is necessary to preserve ObjC ABI compatibility.  */
      tree base = build_decl (input_location,
			      FIELD_DECL, NULL_TREE, super);
      tree field = TYPE_FIELDS (super);

      while (field && DECL_CHAIN (field)
	     && TREE_CODE (DECL_CHAIN (field)) == FIELD_DECL)
	field = DECL_CHAIN (field);

      /* For ObjC ABI purposes, the "packed" size of a base class is
	 the sum of the offset and the size (in bits) of the last field
	 in the class.  */
      DECL_SIZE (base)
	= (field && TREE_CODE (field) == FIELD_DECL
	   ? size_binop (PLUS_EXPR,
			 size_binop (PLUS_EXPR,
				     size_binop
				     (MULT_EXPR,
				      convert (bitsizetype,
					       DECL_FIELD_OFFSET (field)),
				      bitsize_int (BITS_PER_UNIT)),
				     DECL_FIELD_BIT_OFFSET (field)),
			 DECL_SIZE (field))
	   : bitsize_zero_node);
      DECL_SIZE_UNIT (base)
	= size_binop (FLOOR_DIV_EXPR, convert (sizetype, DECL_SIZE (base)),
		      size_int (BITS_PER_UNIT));
      DECL_ARTIFICIAL (base) = 1;
      DECL_ALIGN (base) = 1;
      DECL_FIELD_CONTEXT (base) = s;
#ifdef OBJCPLUS
      DECL_FIELD_IS_BASE (base) = 1;

      if (fields)
	TREE_NO_WARNING (fields) = 1;	/* Suppress C++ ABI warnings -- we   */
#endif					/* are following the ObjC ABI here.  */
      DECL_CHAIN (base) = fields;
      fields = base;
    }

  /* NB: Calling finish_struct() may cause type TYPE_OBJC_INFO
     information in all variants of this RECORD_TYPE to be destroyed
     (this is because the C frontend manipulates TYPE_LANG_SPECIFIC
     for something else and then will change all variants to use the
     same resulting TYPE_LANG_SPECIFIC, ignoring the fact that we use
     it for ObjC protocols and that such propagation will make all
     variants use the same objc_info), but it is therein that we store
     protocol conformance info (e.g., 'NSObject <MyProtocol>').
     Hence, we must save the ObjC-specific information before calling
     finish_struct(), and then reinstate it afterwards.  */

  for (t = TYPE_MAIN_VARIANT (s); t; t = TYPE_NEXT_VARIANT (t))
    {
      INIT_TYPE_OBJC_INFO (t);
      objc_info.safe_push (TYPE_OBJC_INFO (t));
    }

  s = objc_finish_struct (s, fields);

  for (i = 0, t = TYPE_MAIN_VARIANT (s); t; t = TYPE_NEXT_VARIANT (t), i++)
    {
      /* We now want to restore the different TYPE_OBJC_INFO, but we
	 have the additional problem that the C frontend doesn't just
	 copy TYPE_LANG_SPECIFIC from one variant to the other; it
	 actually makes all of them the *same* TYPE_LANG_SPECIFIC.  As
	 we need a different TYPE_OBJC_INFO for each (and
	 TYPE_OBJC_INFO is a field in TYPE_LANG_SPECIFIC), we need to
	 make a copy of each TYPE_LANG_SPECIFIC before we modify
	 TYPE_OBJC_INFO.  */
      if (TYPE_LANG_SPECIFIC (t))
	{
	  /* Create a copy of TYPE_LANG_SPECIFIC.  */
	  struct lang_type *old_lang_type = TYPE_LANG_SPECIFIC (t);
	  ALLOC_OBJC_TYPE_LANG_SPECIFIC (t);
	  memcpy (TYPE_LANG_SPECIFIC (t), old_lang_type,
		  SIZEOF_OBJC_TYPE_LANG_SPECIFIC);
	}
      else
	{
	  /* Just create a new one.  */
	  ALLOC_OBJC_TYPE_LANG_SPECIFIC (t);
	}
      /* Replace TYPE_OBJC_INFO with the saved one.  This restores any
	 protocol information that may have been associated with the
	 type.  */
      TYPE_OBJC_INFO (t) = objc_info[i];
      /* Replace the IDENTIFIER_NODE with an actual @interface now
	 that we have it.  */
      TYPE_OBJC_INTERFACE (t) = klass;
    }
  objc_info.release ();

  /* Use TYPE_BINFO structures to point at the super class, if any.  */
  objc_xref_basetypes (s, super);

  /* Mark this struct as a class template.  */
  CLASS_STATIC_TEMPLATE (klass) = s;

  return s;
}

/* Mark DECL as being 'volatile' for purposes of Darwin
   _setjmp()/_longjmp() exception handling.  Called from
   objc_mark_locals_volatile().  */
void
objc_volatilize_decl (tree decl)
{
  /* Do not mess with variables that are 'static' or (already)
     'volatile'.  */
  if (!TREE_THIS_VOLATILE (decl) && !TREE_STATIC (decl)
      && (TREE_CODE (decl) == VAR_DECL
	  || TREE_CODE (decl) == PARM_DECL))
    {
      if (local_variables_to_volatilize == NULL)
	vec_alloc (local_variables_to_volatilize, 8);

      vec_safe_push (local_variables_to_volatilize, decl);
    }
}

/* Called when parsing of a function completes; if any local variables
   in the function were marked as variables to volatilize, change them
   to volatile.  We do this at the end of the function when the
   warnings about discarding 'volatile' have already been produced.
   We are making the variables as volatile just to force the compiler
   to preserve them between setjmp/longjmp, but we don't want warnings
   for them as they aren't really volatile.  */
void
objc_finish_function (void)
{
  /* If there are any local variables to volatilize, volatilize them.  */
  if (local_variables_to_volatilize)
    {
      int i;
      tree decl;
      FOR_EACH_VEC_ELT (*local_variables_to_volatilize, i, decl)
	{
	  tree t = TREE_TYPE (decl);

	  t = build_qualified_type (t, TYPE_QUALS (t) | TYPE_QUAL_VOLATILE);
	  TREE_TYPE (decl) = t;
	  TREE_THIS_VOLATILE (decl) = 1;
	  TREE_SIDE_EFFECTS (decl) = 1;
	  DECL_REGISTER (decl) = 0;
#ifndef OBJCPLUS
	  C_DECL_REGISTER (decl) = 0;
#endif
	}

      /* Now we delete the vector.  This sets it to NULL as well.  */
      vec_free (local_variables_to_volatilize);
    }
}

/* Check if protocol PROTO is adopted (directly or indirectly) by class CLS
   (including its categories and superclasses) or by object type TYP.
   Issue a warning if PROTO is not adopted anywhere and WARN is set.  */

static bool
objc_lookup_protocol (tree proto, tree cls, tree typ, bool warn)
{
  bool class_type = (cls != NULL_TREE);

  while (cls)
    {
      tree c;

      /* Check protocols adopted by the class and its categories.  */
      for (c = cls; c; c = CLASS_CATEGORY_LIST (c))
	{
	  if (lookup_protocol_in_reflist (CLASS_PROTOCOL_LIST (c), proto))
	    return true;
	}

      /* Repeat for superclasses.  */
      cls = lookup_interface (CLASS_SUPER_NAME (cls));
    }

  /* Check for any protocols attached directly to the object type.  */
  if (TYPE_HAS_OBJC_INFO (typ))
    {
      if (lookup_protocol_in_reflist (TYPE_OBJC_PROTOCOL_LIST (typ), proto))
	return true;
    }

  if (warn)
    {
      *errbuf = 0;
      gen_type_name_0 (class_type ? typ : TYPE_POINTER_TO (typ));
      /* NB: Types 'id' and 'Class' cannot reasonably be described as
	 "implementing" a given protocol, since they do not have an
	 implementation.  */
      if (class_type)
	warning (0, "class %qs does not implement the %qE protocol",
		 identifier_to_locale (errbuf), PROTOCOL_NAME (proto));
      else
	warning (0, "type %qs does not conform to the %qE protocol",
		 identifier_to_locale (errbuf), PROTOCOL_NAME (proto));
    }

  return false;
}

/* Check if class RCLS and instance struct type RTYP conform to at least the
   same protocols that LCLS and LTYP conform to.  */

static bool
objc_compare_protocols (tree lcls, tree ltyp, tree rcls, tree rtyp, bool warn)
{
  tree p;
  bool have_lproto = false;

  while (lcls)
    {
      /* NB: We do _not_ look at categories defined for LCLS; these may or
	 may not get loaded in, and therefore it is unreasonable to require
	 that RCLS/RTYP must implement any of their protocols.  */
      for (p = CLASS_PROTOCOL_LIST (lcls); p; p = TREE_CHAIN (p))
	{
	  have_lproto = true;

	  if (!objc_lookup_protocol (TREE_VALUE (p), rcls, rtyp, warn))
	    return warn;
	}

      /* Repeat for superclasses.  */
      lcls = lookup_interface (CLASS_SUPER_NAME (lcls));
    }

  /* Check for any protocols attached directly to the object type.  */
  if (TYPE_HAS_OBJC_INFO (ltyp))
    {
      for (p = TYPE_OBJC_PROTOCOL_LIST (ltyp); p; p = TREE_CHAIN (p))
	{
	  have_lproto = true;

	  if (!objc_lookup_protocol (TREE_VALUE (p), rcls, rtyp, warn))
	    return warn;
	}
    }

  /* NB: If LTYP and LCLS have no protocols to search for, return 'true'
     vacuously, _unless_ RTYP is a protocol-qualified 'id'.  We can get
     away with simply checking for 'id' or 'Class' (!RCLS), since this
     routine will not get called in other cases.  */
  return have_lproto || (rcls != NULL_TREE);
}

/* Given two types TYPE1 and TYPE2, return their least common ancestor.
   Both TYPE1 and TYPE2 must be pointers, and already determined to be
   compatible by objc_compare_types() below.  */

tree
objc_common_type (tree type1, tree type2)
{
  tree inner1 = TREE_TYPE (type1), inner2 = TREE_TYPE (type2);

  while (POINTER_TYPE_P (inner1))
    {
      inner1 = TREE_TYPE (inner1);
      inner2 = TREE_TYPE (inner2);
    }

  /* If one type is derived from another, return the base type.  */
  if (DERIVED_FROM_P (inner1, inner2))
    return type1;
  else if (DERIVED_FROM_P (inner2, inner1))
    return type2;

  /* If both types are 'Class', return 'Class'.  */
  if (objc_is_class_id (inner1) && objc_is_class_id (inner2))
    return objc_class_type;

  /* Otherwise, return 'id'.  */
  return objc_object_type;
}

/* Determine if it is permissible to assign (if ARGNO is greater than -3)
   an instance of RTYP to an instance of LTYP or to compare the two
   (if ARGNO is equal to -3), per ObjC type system rules.  Before
   returning 'true', this routine may issue warnings related to, e.g.,
   protocol conformance.  When returning 'false', the routine must
   produce absolutely no warnings; the C or C++ front-end will do so
   instead, if needed.  If either LTYP or RTYP is not an Objective-C
   type, the routine must return 'false'.

   The ARGNO parameter is encoded as follows:
     >= 1	Parameter number (CALLEE contains function being called);
     0		Return value;
     -1		Assignment;
     -2		Initialization;
     -3		Comparison (LTYP and RTYP may match in either direction);
     -4		Silent comparison (for C++ overload resolution);
     -5		Silent "specialization" comparison for RTYP to be a "specialization"
                of LTYP (a specialization means that RTYP is LTYP plus some constraints,
                so that each object of type RTYP is also of type LTYP).  This is used
                when comparing property types.  */

bool
objc_compare_types (tree ltyp, tree rtyp, int argno, tree callee)
{
  tree lcls, rcls, lproto, rproto;
  bool pointers_compatible;

  /* We must be dealing with pointer types */
  if (!POINTER_TYPE_P (ltyp) || !POINTER_TYPE_P (rtyp))
    return false;

  do
    {
      ltyp = TREE_TYPE (ltyp);  /* Remove indirections.  */
      rtyp = TREE_TYPE (rtyp);
    }
  while (POINTER_TYPE_P (ltyp) && POINTER_TYPE_P (rtyp));

  /* We must also handle function pointers, since ObjC is a bit more
     lenient than C or C++ on this.  */
  if (TREE_CODE (ltyp) == FUNCTION_TYPE && TREE_CODE (rtyp) == FUNCTION_TYPE)
    {
      function_args_iterator liter, riter;

      /* Return types must be covariant.  */
      if (!comptypes (TREE_TYPE (ltyp), TREE_TYPE (rtyp))
	  && !objc_compare_types (TREE_TYPE (ltyp), TREE_TYPE (rtyp),
				  argno, callee))
      return false;

      /* Argument types must be contravariant.  */
      function_args_iter_init (&liter, ltyp);
      function_args_iter_init (&riter, rtyp);

      while (1)
	{
	  ltyp = function_args_iter_cond (&liter);
	  rtyp = function_args_iter_cond (&riter);

	  /* If we've exhaused both lists simulateously, we're done.  */
	  if (ltyp == NULL_TREE && rtyp == NULL_TREE)
	    break;

	  /* If one list is shorter than the other, they fail to match.  */
	  if (ltyp == NULL_TREE || rtyp == NULL_TREE)
	    return false;

	  if (!comptypes (rtyp, ltyp)
	      && !objc_compare_types (rtyp, ltyp, argno, callee))
	    return false;

	  function_args_iter_next (&liter);
	  function_args_iter_next (&riter);
	}

      return true;
    }

  /* Past this point, we are only interested in ObjC class instances,
     or 'id' or 'Class'.  */
  if (TREE_CODE (ltyp) != RECORD_TYPE || TREE_CODE (rtyp) != RECORD_TYPE)
    return false;

  if (!objc_is_object_id (ltyp) && !objc_is_class_id (ltyp)
      && !TYPE_HAS_OBJC_INFO (ltyp))
    return false;

  if (!objc_is_object_id (rtyp) && !objc_is_class_id (rtyp)
      && !TYPE_HAS_OBJC_INFO (rtyp))
    return false;

  /* Past this point, we are committed to returning 'true' to the caller
     (unless performing a silent comparison; see below).  However, we can
     still warn about type and/or protocol mismatches.  */

  if (TYPE_HAS_OBJC_INFO (ltyp))
    {
      lcls = TYPE_OBJC_INTERFACE (ltyp);
      lproto = TYPE_OBJC_PROTOCOL_LIST (ltyp);
    }
  else
    lcls = lproto = NULL_TREE;

  if (TYPE_HAS_OBJC_INFO (rtyp))
    {
      rcls = TYPE_OBJC_INTERFACE (rtyp);
      rproto = TYPE_OBJC_PROTOCOL_LIST (rtyp);
    }
  else
    rcls = rproto = NULL_TREE;

  /* If we could not find an @interface declaration, we must have
     only seen a @class declaration; for purposes of type comparison,
     treat it as a stand-alone (root) class.  */

  if (lcls && TREE_CODE (lcls) == IDENTIFIER_NODE)
    lcls = NULL_TREE;

  if (rcls && TREE_CODE (rcls) == IDENTIFIER_NODE)
    rcls = NULL_TREE;

  /* If either type is an unqualified 'id', we're done.  This is because
     an 'id' can be assigned to or from any type with no warnings.  */
  if (argno != -5)
    {
      if ((!lproto && objc_is_object_id (ltyp))
	  || (!rproto && objc_is_object_id (rtyp)))
	return true;
    }
  else
    {
      /* For property checks, though, an 'id' is considered the most
	 general type of object, hence if you try to specialize an
	 'NSArray *' (ltyp) property with an 'id' (rtyp) one, we need
	 to warn.  */
      if (!lproto && objc_is_object_id (ltyp))
	return true;
    }

  pointers_compatible = (TYPE_MAIN_VARIANT (ltyp) == TYPE_MAIN_VARIANT (rtyp));

  /* If the underlying types are the same, and at most one of them has
     a protocol list, we do not need to issue any diagnostics.  */
  if (pointers_compatible && (!lproto || !rproto))
    return true;

  /* If exactly one of the types is 'Class', issue a diagnostic; any
     exceptions of this rule have already been handled.  */
  if (objc_is_class_id (ltyp) ^ objc_is_class_id (rtyp))
    pointers_compatible = false;
  /* Otherwise, check for inheritance relations.  */
  else
    {
      if (!pointers_compatible)
	{
	  /* Again, if any of the two is an 'id', we're satisfied,
	     unless we're comparing properties, in which case only an
	     'id' on the left-hand side (old property) is good
	     enough.  */
	  if (argno != -5)
	    pointers_compatible
	      = (objc_is_object_id (ltyp) || objc_is_object_id (rtyp));
	  else
	    pointers_compatible = objc_is_object_id (ltyp);
	}

      if (!pointers_compatible)
	pointers_compatible = DERIVED_FROM_P (ltyp, rtyp);

      if (!pointers_compatible && (argno == -3 || argno == -4))
	pointers_compatible = DERIVED_FROM_P (rtyp, ltyp);
    }

  /* If the pointers match modulo protocols, check for protocol conformance
     mismatches.  */
  if (pointers_compatible)
    {
      pointers_compatible = objc_compare_protocols (lcls, ltyp, rcls, rtyp,
						    argno != -3);

      if (!pointers_compatible && argno == -3)
	pointers_compatible = objc_compare_protocols (rcls, rtyp, lcls, ltyp,
						      argno != -3);
    }

  if (!pointers_compatible)
    {
      /* The two pointers are not exactly compatible.  Issue a warning, unless
	 we are performing a silent comparison, in which case return 'false'
	 instead.  */
      /* NB: For the time being, we shall make our warnings look like their
	 C counterparts.  In the future, we may wish to make them more
	 ObjC-specific.  */
      switch (argno)
	{
	case -5:
	case -4:
	  return false;

	case -3:
	  warning (0, "comparison of distinct Objective-C types lacks a cast");
	  break;

	case -2:
	  warning (0, "initialization from distinct Objective-C type");
	  break;

	case -1:
	  warning (0, "assignment from distinct Objective-C type");
	  break;

	case 0:
	  warning (0, "distinct Objective-C type in return");
	  break;

	default:
	  warning (0, "passing argument %d of %qE from distinct "
		   "Objective-C type", argno, callee);
	  break;
	}
    }

  return true;
}

/* This routine is similar to objc_compare_types except that function-pointers are
   excluded. This is because, caller assumes that common types are of (id, Object*)
   variety and calls objc_common_type to obtain a common type. There is no commonolty
   between two function-pointers in this regard. */

bool
objc_have_common_type (tree ltyp, tree rtyp, int argno, tree callee)
{
  if (objc_compare_types (ltyp, rtyp, argno, callee))
    {
      /* exclude function-pointer types. */
      do
        {
          ltyp = TREE_TYPE (ltyp);  /* Remove indirections.  */
          rtyp = TREE_TYPE (rtyp);
        }
      while (POINTER_TYPE_P (ltyp) && POINTER_TYPE_P (rtyp));
      return !(TREE_CODE (ltyp) == FUNCTION_TYPE && TREE_CODE (rtyp) == FUNCTION_TYPE);
    }
  return false;
}

#ifndef OBJCPLUS
/* Determine if CHILD is derived from PARENT.  The routine assumes that
   both parameters are RECORD_TYPEs, and is non-reflexive.  */

static bool
objc_derived_from_p (tree parent, tree child)
{
  parent = TYPE_MAIN_VARIANT (parent);

  for (child = TYPE_MAIN_VARIANT (child);
       TYPE_BINFO (child) && BINFO_N_BASE_BINFOS (TYPE_BINFO (child));)
    {
      child = TYPE_MAIN_VARIANT (BINFO_TYPE (BINFO_BASE_BINFO
					     (TYPE_BINFO (child),
					      0)));

      if (child == parent)
	return true;
    }

  return false;
}
#endif

tree
objc_build_component_ref (tree datum, tree component)
{
  /* If COMPONENT is NULL, the caller is referring to the anonymous
     base class field.  */
  if (!component)
    {
      tree base = TYPE_FIELDS (TREE_TYPE (datum));

      return build3 (COMPONENT_REF, TREE_TYPE (base), datum, base, NULL_TREE);
    }

  /* The 'build_component_ref' routine has been removed from the C++
     front-end, but 'finish_class_member_access_expr' seems to be
     a worthy substitute.  */
#ifdef OBJCPLUS
  return finish_class_member_access_expr (datum, component, false,
                                          tf_warning_or_error);
#else
  return build_component_ref (input_location, datum, component);
#endif
}

/* Recursively copy inheritance information rooted at BINFO.  To do this,
   we emulate the song and dance performed by cp/tree.c:copy_binfo().  */

static tree
objc_copy_binfo (tree binfo)
{
  tree btype = BINFO_TYPE (binfo);
  tree binfo2 = make_tree_binfo (BINFO_N_BASE_BINFOS (binfo));
  tree base_binfo;
  int ix;

  BINFO_TYPE (binfo2) = btype;
  BINFO_OFFSET (binfo2) = BINFO_OFFSET (binfo);
  BINFO_BASE_ACCESSES (binfo2) = BINFO_BASE_ACCESSES (binfo);

  /* Recursively copy base binfos of BINFO.  */
  for (ix = 0; BINFO_BASE_ITERATE (binfo, ix, base_binfo); ix++)
    {
      tree base_binfo2 = objc_copy_binfo (base_binfo);

      BINFO_INHERITANCE_CHAIN (base_binfo2) = binfo2;
      BINFO_BASE_APPEND (binfo2, base_binfo2);
    }

  return binfo2;
}

/* Record superclass information provided in BASETYPE for ObjC class REF.
   This is loosely based on cp/decl.c:xref_basetypes().  */

static void
objc_xref_basetypes (tree ref, tree basetype)
{
  tree binfo = make_tree_binfo (basetype ? 1 : 0);

  TYPE_BINFO (ref) = binfo;
  BINFO_OFFSET (binfo) = size_zero_node;
  BINFO_TYPE (binfo) = ref;

  if (basetype)
    {
      tree base_binfo = objc_copy_binfo (TYPE_BINFO (basetype));

      BINFO_INHERITANCE_CHAIN (base_binfo) = binfo;
      vec_alloc (BINFO_BASE_ACCESSES (binfo), 1);
      BINFO_BASE_APPEND (binfo, base_binfo);
      BINFO_BASE_ACCESS_APPEND (binfo, access_public_node);
    }
}

/* Called from finish_decl.  */

void
objc_check_decl (tree decl)
{
  tree type = TREE_TYPE (decl);

  if (TREE_CODE (type) != RECORD_TYPE)
    return;
  if (OBJC_TYPE_NAME (type) && (type = objc_is_class_name (OBJC_TYPE_NAME (type))))
    error ("statically allocated instance of Objective-C class %qE",
	   type);
}

void
objc_check_global_decl (tree decl)
{
  tree id = DECL_NAME (decl);
  if (objc_is_class_name (id) && global_bindings_p())
    error ("redeclaration of Objective-C class %qs", IDENTIFIER_POINTER (id));
}

/* Construct a PROTOCOLS-qualified variant of INTERFACE, where
   INTERFACE may either name an Objective-C class, or refer to the
   special 'id' or 'Class' types.  If INTERFACE is not a valid ObjC
   type, just return it unchanged.  This function is often called when
   PROTOCOLS is NULL_TREE, in which case we simply look up the
   appropriate INTERFACE.  */

tree
objc_get_protocol_qualified_type (tree interface, tree protocols)
{
  /* If INTERFACE is not provided, default to 'id'.  */
  tree type = (interface ? objc_is_id (interface) : objc_object_type);
  bool is_ptr = (type != NULL_TREE);

  if (!is_ptr)
    {
      type = objc_is_class_name (interface);

      if (type)
	{
	  /* If looking at a typedef, retrieve the precise type it
	     describes.  */
	  if (TREE_CODE (interface) == IDENTIFIER_NODE)
	    interface = identifier_global_value (interface);

	  type = ((interface && TREE_CODE (interface) == TYPE_DECL
		   && DECL_ORIGINAL_TYPE (interface))
		  ? DECL_ORIGINAL_TYPE (interface)
		  : xref_tag (RECORD_TYPE, type));
	}
      else
	{
	  /* This case happens when we are given an 'interface' which
	     is not a valid class name.  For example if a typedef was
	     used, and 'interface' really is the identifier of the
	     typedef, but when you resolve it you don't get an
	     Objective-C class, but something else, such as 'int'.
	     This is an error; protocols make no sense unless you use
	     them with Objective-C objects.  */
	  error_at (input_location, "only Objective-C object types can be qualified with a protocol");

	  /* Try to recover.  Ignore the invalid class name, and treat
	     the object as an 'id' to silence further warnings about
	     the class.  */
	  type = objc_object_type;
	  is_ptr = true;
	}
    }

  if (protocols)
    {
      type = build_variant_type_copy (type);

      /* For pointers (i.e., 'id' or 'Class'), attach the protocol(s)
	 to the pointee.  */
      if (is_ptr)
	{
	  tree orig_pointee_type = TREE_TYPE (type);
	  TREE_TYPE (type) = build_variant_type_copy (orig_pointee_type);

	  /* Set up the canonical type information. */
	  TYPE_CANONICAL (type)
	    = TYPE_CANONICAL (TYPE_POINTER_TO (orig_pointee_type));

	  TYPE_POINTER_TO (TREE_TYPE (type)) = type;
	  type = TREE_TYPE (type);
	}

      /* Look up protocols and install in lang specific list.  */
      DUP_TYPE_OBJC_INFO (type, TYPE_MAIN_VARIANT (type));
      TYPE_OBJC_PROTOCOL_LIST (type) = lookup_and_install_protocols
	(protocols, /* definition_required */ false);

      /* For RECORD_TYPEs, point to the @interface; for 'id' and 'Class',
	 return the pointer to the new pointee variant.  */
      if (is_ptr)
	type = TYPE_POINTER_TO (type);
      else
	TYPE_OBJC_INTERFACE (type)
	  = TYPE_OBJC_INTERFACE (TYPE_MAIN_VARIANT (type));
    }

  return type;
}

/* Check for circular dependencies in protocols.  The arguments are
   PROTO, the protocol to check, and LIST, a list of protocol it
   conforms to.  */

static void
check_protocol_recursively (tree proto, tree list)
{
  tree p;

  for (p = list; p; p = TREE_CHAIN (p))
    {
      tree pp = TREE_VALUE (p);

      if (TREE_CODE (pp) == IDENTIFIER_NODE)
	pp = lookup_protocol (pp, /* warn if deprecated */ false,
			      /* definition_required */ false);

      if (pp == proto)
	fatal_error ("protocol %qE has circular dependency",
		     PROTOCOL_NAME (pp));
      if (pp)
	check_protocol_recursively (proto, PROTOCOL_LIST (pp));
    }
}

/* Look up PROTOCOLS, and return a list of those that are found.  If
   none are found, return NULL.  Note that this function will emit a
   warning if a protocol is found and is deprecated.  If
   'definition_required', then warn if the protocol is found but is
   not defined (ie, if we only saw a forward-declaration of the
   protocol (as in "@protocol NSObject;") not a real definition with
   the list of methods).  */
static tree
lookup_and_install_protocols (tree protocols, bool definition_required)
{
  tree proto;
  tree return_value = NULL_TREE;

  if (protocols == error_mark_node)
    return NULL;

  for (proto = protocols; proto; proto = TREE_CHAIN (proto))
    {
      tree ident = TREE_VALUE (proto);
      tree p = lookup_protocol (ident, /* warn_if_deprecated */ true,
				definition_required);

      if (p)
	return_value = chainon (return_value,
				build_tree_list (NULL_TREE, p));
      else if (ident != error_mark_node)
	error ("cannot find protocol declaration for %qE",
	       ident);
    }

  return return_value;
}

static void
build_common_objc_exception_stuff (void)
{
  tree noreturn_list, nothrow_list, temp_type;

  noreturn_list = tree_cons (get_identifier ("noreturn"), NULL, NULL);
  nothrow_list = tree_cons (get_identifier ("nothrow"), NULL, NULL);

  /* void objc_exception_throw(id) __attribute__((noreturn)); */
  /* void objc_sync_enter(id); */
  /* void objc_sync_exit(id); */
  temp_type = build_function_type_list (void_type_node,
                                        objc_object_type,
                                        NULL_TREE);
  objc_exception_throw_decl
    = add_builtin_function (TAG_EXCEPTIONTHROW, temp_type, 0, NOT_BUILT_IN, NULL,
			    noreturn_list);
  /* Make sure that objc_exception_throw (id) claims that it may throw an
     exception. */
  TREE_NOTHROW (objc_exception_throw_decl) = 0;

  objc_sync_enter_decl
    = add_builtin_function (TAG_SYNCENTER, temp_type, 0, NOT_BUILT_IN,
			    NULL, nothrow_list);

  objc_sync_exit_decl
    = add_builtin_function (TAG_SYNCEXIT, temp_type, 0, NOT_BUILT_IN,
			    NULL, nothrow_list);
}

/* Purpose: "play" parser, creating/installing representations
   of the declarations that are required by Objective-C.

   Model:

	type_spec--------->sc_spec
	(tree_list)        (tree_list)
	    |                  |
	    |                  |
	identifier_node    identifier_node  */

static void
synth_module_prologue (void)
{
  tree type;
  enum debug_info_type save_write_symbols = write_symbols;
  const struct gcc_debug_hooks *const save_hooks = debug_hooks;

  /* Suppress outputting debug symbols, because
     dbxout_init hasn't been called yet.  */
  write_symbols = NO_DEBUG;
  debug_hooks = &do_nothing_debug_hooks;

#ifdef OBJCPLUS
  push_lang_context (lang_name_c); /* extern "C" */
#endif

  /* The following are also defined in <objc/objc.h> and friends.  */

  objc_object_id = get_identifier (TAG_OBJECT);
  objc_class_id = get_identifier (TAG_CLASS);

  objc_object_reference = xref_tag (RECORD_TYPE, objc_object_id);
  objc_class_reference = xref_tag (RECORD_TYPE, objc_class_id);

  objc_object_type = build_pointer_type (objc_object_reference);
  objc_class_type = build_pointer_type (objc_class_reference);

  objc_object_name = get_identifier (OBJECT_TYPEDEF_NAME);
  objc_class_name = get_identifier (CLASS_TYPEDEF_NAME);

  /* Declare the 'id' and 'Class' typedefs.  */
  type = lang_hooks.decls.pushdecl (build_decl (input_location,
						TYPE_DECL,
						objc_object_name,
						objc_object_type));
  TREE_NO_WARNING (type) = 1;

  type = lang_hooks.decls.pushdecl (build_decl (input_location,
						TYPE_DECL,
						objc_class_name,
						objc_class_type));
  TREE_NO_WARNING (type) = 1;

  /* Forward-declare '@interface Protocol'.  */
  type = get_identifier (PROTOCOL_OBJECT_CLASS_NAME);
  objc_declare_class (type);
  objc_protocol_type = build_pointer_type (xref_tag (RECORD_TYPE, type));

  /* Declare receiver type used for dispatching messages to 'super'.  */
  /* `struct objc_super *' */
  objc_super_type = build_pointer_type (xref_tag (RECORD_TYPE,
						  get_identifier (TAG_SUPER)));

  /* Declare pointers to method and ivar lists.  */
  objc_method_list_ptr = build_pointer_type
			 (xref_tag (RECORD_TYPE,
				    get_identifier (UTAG_METHOD_LIST)));
  objc_method_proto_list_ptr
    = build_pointer_type (xref_tag (RECORD_TYPE,
				    get_identifier (UTAG_METHOD_PROTOTYPE_LIST)));
  objc_ivar_list_ptr = build_pointer_type
		       (xref_tag (RECORD_TYPE,
				  get_identifier (UTAG_IVAR_LIST)));

  build_common_objc_exception_stuff ();

  /* Set-up runtime-specific templates, message and exception stuff.  */
  (*runtime.initialize) ();

  /* Declare objc_getProperty, object_setProperty and other property
     accessor helpers.  */
  build_common_objc_property_accessor_helpers ();

  /* Forward declare constant_string_id and constant_string_type.  */
  if (!constant_string_class_name)
    constant_string_class_name = runtime.default_constant_string_class_name;
  constant_string_id = get_identifier (constant_string_class_name);
  objc_declare_class (constant_string_id);

  /* Pre-build the following entities - for speed/convenience.  */
  self_id = get_identifier ("self");
  ucmd_id = get_identifier ("_cmd");

  /* Declare struct _objc_fast_enumeration_state { ... };  */
  build_fast_enumeration_state_template ();

  /* void objc_enumeration_mutation (id) */
  type = build_function_type_list (void_type_node,
				   objc_object_type, NULL_TREE);
  objc_enumeration_mutation_decl
    = add_builtin_function (TAG_ENUMERATION_MUTATION, type, 0, NOT_BUILT_IN,
			    NULL, NULL_TREE);
  TREE_NOTHROW (objc_enumeration_mutation_decl) = 0;

#ifdef OBJCPLUS
  pop_lang_context ();
#endif

  write_symbols = save_write_symbols;
  debug_hooks = save_hooks;
}

/* --- const strings --- */

/* Ensure that the ivar list for NSConstantString/NXConstantString
   (or whatever was specified via `-fconstant-string-class')
   contains fields at least as large as the following three, so that
   the runtime can stomp on them with confidence:

   struct STRING_OBJECT_CLASS_NAME
   {
     Object isa;
     char *cString;
     unsigned int length;
   }; */

static int
check_string_class_template (void)
{
  tree field_decl = objc_get_class_ivars (constant_string_id);

#define AT_LEAST_AS_LARGE_AS(F, T) \
  (F && TREE_CODE (F) == FIELD_DECL \
     && (TREE_INT_CST_LOW (TYPE_SIZE (TREE_TYPE (F))) \
	 >= TREE_INT_CST_LOW (TYPE_SIZE (T))))

  if (!AT_LEAST_AS_LARGE_AS (field_decl, ptr_type_node))
    return 0;

  field_decl = DECL_CHAIN (field_decl);
  if (!AT_LEAST_AS_LARGE_AS (field_decl, ptr_type_node))
    return 0;

  field_decl = DECL_CHAIN (field_decl);
  return AT_LEAST_AS_LARGE_AS (field_decl, unsigned_type_node);

#undef AT_LEAST_AS_LARGE_AS
}

/* Avoid calling `check_string_class_template ()' more than once.  */
static GTY(()) int string_layout_checked;

/* Construct an internal string layout to be used as a template for
   creating NSConstantString/NXConstantString instances.  */

static tree
objc_build_internal_const_str_type (void)
{
  tree type = (*lang_hooks.types.make_type) (RECORD_TYPE);
  tree fields = build_decl (input_location,
			    FIELD_DECL, NULL_TREE, ptr_type_node);
  tree field = build_decl (input_location,
			   FIELD_DECL, NULL_TREE, ptr_type_node);

  DECL_CHAIN (field) = fields; fields = field;
  field = build_decl (input_location,
		      FIELD_DECL, NULL_TREE, unsigned_type_node);
  DECL_CHAIN (field) = fields; fields = field;
  /* NB: The finish_builtin_struct() routine expects FIELD_DECLs in
     reverse order!  */
  finish_builtin_struct (type, "__builtin_ObjCString",
			 fields, NULL_TREE);

  return type;
}

/* Custom build_string which sets TREE_TYPE!  */

tree
my_build_string (int len, const char *str)
{
  return fix_string_type (build_string (len, str));
}

/* Build a string with contents STR and length LEN and convert it to a
   pointer.  */

tree
my_build_string_pointer (int len, const char *str)
{
  tree string = my_build_string (len, str);
  tree ptrtype = build_pointer_type (TREE_TYPE (TREE_TYPE (string)));
  return build1 (ADDR_EXPR, ptrtype, string);
}

static hashval_t
string_hash (const void *ptr)
{
  const_tree const str = ((const struct string_descriptor *)ptr)->literal;
  const unsigned char *p = (const unsigned char *) TREE_STRING_POINTER (str);
  int i, len = TREE_STRING_LENGTH (str);
  hashval_t h = len;

  for (i = 0; i < len; i++)
    h = ((h * 613) + p[i]);

  return h;
}

static int
string_eq (const void *ptr1, const void *ptr2)
{
  const_tree const str1 = ((const struct string_descriptor *)ptr1)->literal;
  const_tree const str2 = ((const struct string_descriptor *)ptr2)->literal;
  int len1 = TREE_STRING_LENGTH (str1);

  return (len1 == TREE_STRING_LENGTH (str2)
	  && !memcmp (TREE_STRING_POINTER (str1), TREE_STRING_POINTER (str2),
		      len1));
}

/* Given a chain of STRING_CST's, build a static instance of
   NXConstantString which points at the concatenation of those
   strings.  We place the string object in the __string_objects
   section of the __OBJC segment.  The Objective-C runtime will
   initialize the isa pointers of the string objects to point at the
   NXConstantString class object.  */

tree
objc_build_string_object (tree string)
{
  tree constant_string_class;
  int length;
  tree addr;
  struct string_descriptor *desc, key;
  void **loc;

  /* We should be passed a STRING_CST.  */
  gcc_checking_assert (TREE_CODE (string) == STRING_CST);
  length = TREE_STRING_LENGTH (string) - 1;

  /* The target may have different ideas on how to construct an ObjC string
     literal.  On Darwin (Mac OS X), for example, we may wish to obtain a
     constant CFString reference instead.
     At present, this is only supported for the NeXT runtime.  */
  if (flag_next_runtime
      && targetcm.objc_construct_string_object)
    {
      tree constructor = (*targetcm.objc_construct_string_object) (string);
      if (constructor)
	return build1 (NOP_EXPR, objc_object_type, constructor);
    }

  /* Check whether the string class being used actually exists and has the
     correct ivar layout.  */
  if (!string_layout_checked)
    {
      string_layout_checked = -1;
      constant_string_class = lookup_interface (constant_string_id);
      internal_const_str_type = objc_build_internal_const_str_type ();

      if (!constant_string_class
	  || !(constant_string_type
	       = CLASS_STATIC_TEMPLATE (constant_string_class)))
	error ("cannot find interface declaration for %qE",
	       constant_string_id);
      /* The NSConstantString/NXConstantString ivar layout is now known.  */
      else if (!check_string_class_template ())
	error ("interface %qE does not have valid constant string layout",
	       constant_string_id);
      /* If the runtime can generate a literal reference to the string class,
	 don't need to run a constructor.  */
      else if (!(*runtime.setup_const_string_class_decl)())
	error ("cannot find reference tag for class %qE", constant_string_id);
      else
	{
	  string_layout_checked = 1;  /* Success!  */
	  add_class_reference (constant_string_id);
	}
    }

  if (string_layout_checked == -1)
    return error_mark_node;

  /* Perhaps we already constructed a constant string just like this one? */
  key.literal = string;
  loc = htab_find_slot (string_htab, &key, INSERT);
  desc = (struct string_descriptor *) *loc;

  if (!desc)
    {
      *loc = desc = ggc_alloc_string_descriptor ();
      desc->literal = string;
      desc->constructor =
	(*runtime.build_const_string_constructor) (input_location, string, length);
    }

  addr = convert (build_pointer_type (constant_string_type),
		  build_unary_op (input_location,
				  ADDR_EXPR, desc->constructor, 1));

  return addr;
}

/* Build a static constant CONSTRUCTOR
   with type TYPE and elements ELTS.  */

tree
objc_build_constructor (tree type, vec<constructor_elt, va_gc> *elts)
{
  tree constructor = build_constructor (type, elts);

  TREE_CONSTANT (constructor) = 1;
  TREE_STATIC (constructor) = 1;
  TREE_READONLY (constructor) = 1;

#ifdef OBJCPLUS
  /* Adjust for impedance mismatch.  We should figure out how to build
     CONSTRUCTORs that consistently please both the C and C++ gods.  */
  if (!(*elts)[0].index)
    TREE_TYPE (constructor) = init_list_type_node;
#endif

  return constructor;
}

/* Return the DECL of the string IDENT in the SECTION.  */

tree
get_objc_string_decl (tree ident, enum string_section section)
{
  tree chain;

  switch (section)
    {
    case class_names:
      chain = class_names_chain;
      break;
    case meth_var_names:
      chain = meth_var_names_chain;
      break;
    case meth_var_types:
      chain = meth_var_types_chain;
      break;
    case prop_names_attr:
      chain = prop_names_attr_chain;
      break;
    default:
      gcc_unreachable ();
    }

  for (; chain != 0; chain = TREE_CHAIN (chain))
    if (TREE_VALUE (chain) == ident)
      return (TREE_PURPOSE (chain));

  /* We didn't find the entry.  */
  return NULL_TREE;
}

/* Create a class reference, but don't create a variable to reference
   it.  */

void
add_class_reference (tree ident)
{
  tree chain;

  if ((chain = cls_ref_chain))
    {
      tree tail;
      do
        {
	  if (ident == TREE_VALUE (chain))
	    return;

	  tail = chain;
	  chain = TREE_CHAIN (chain);
        }
      while (chain);

      /* Append to the end of the list */
      TREE_CHAIN (tail) = tree_cons (NULL_TREE, ident, NULL_TREE);
    }
  else
    cls_ref_chain = tree_cons (NULL_TREE, ident, NULL_TREE);
}

/* Get a class reference, creating it if necessary.  Also create the
   reference variable.  */
tree
objc_get_class_reference (tree ident)
{
  tree orig_ident = (DECL_P (ident)
		     ? DECL_NAME (ident)
		     : TYPE_P (ident)
		     ? OBJC_TYPE_NAME (ident)
		     : ident);
  bool local_scope = false;

#ifdef OBJCPLUS
  if (processing_template_decl)
    /* Must wait until template instantiation time.  */
    return build_min_nt_loc (UNKNOWN_LOCATION, CLASS_REFERENCE_EXPR, ident);
#endif

  if (TREE_CODE (ident) == TYPE_DECL)
    ident = (DECL_ORIGINAL_TYPE (ident)
	     ? DECL_ORIGINAL_TYPE (ident)
	     : TREE_TYPE (ident));

#ifdef OBJCPLUS
  if (TYPE_P (ident)
      && CP_TYPE_CONTEXT (ident) != global_namespace)
    local_scope = true;
#endif

  if (local_scope || !(ident = objc_is_class_name (ident)))
    {
      error ("%qE is not an Objective-C class name or alias",
	     orig_ident);
      return error_mark_node;
    }

  return (*runtime.get_class_reference) (ident);
}

void
objc_declare_alias (tree alias_ident, tree class_ident)
{
  tree underlying_class;

#ifdef OBJCPLUS
  if (current_namespace != global_namespace) {
    error ("Objective-C declarations may only appear in global scope");
  }
#endif /* OBJCPLUS */

  if (!(underlying_class = objc_is_class_name (class_ident)))
    warning (0, "cannot find class %qE", class_ident);
  else if (objc_is_class_name (alias_ident))
    warning (0, "class %qE already exists", alias_ident);
  else
    {
      /* Implement @compatibility_alias as a typedef.  */
#ifdef OBJCPLUS
      push_lang_context (lang_name_c); /* extern "C" */
#endif
      lang_hooks.decls.pushdecl (build_decl
				 (input_location,
				  TYPE_DECL,
				  alias_ident,
				  xref_tag (RECORD_TYPE, underlying_class)));
#ifdef OBJCPLUS
      pop_lang_context ();
#endif
      objc_map_put (alias_name_map, alias_ident, underlying_class);
    }
}

void
objc_declare_class (tree identifier)
{
#ifdef OBJCPLUS
  if (current_namespace != global_namespace) {
    error ("Objective-C declarations may only appear in global scope");
  }
#endif /* OBJCPLUS */

  if (! objc_is_class_name (identifier))
    {
      tree record = lookup_name (identifier), type = record;

      if (record)
	{
	  if (TREE_CODE (record) == TYPE_DECL)
	    type = DECL_ORIGINAL_TYPE (record)
	      ? DECL_ORIGINAL_TYPE (record)
	      : TREE_TYPE (record);

	  if (!TYPE_HAS_OBJC_INFO (type)
	      || !TYPE_OBJC_INTERFACE (type))
	    {
	      error ("%qE redeclared as different kind of symbol",
		     identifier);
	      error ("previous declaration of %q+D",
		     record);
	    }
	}

      record = xref_tag (RECORD_TYPE, identifier);
      INIT_TYPE_OBJC_INFO (record);
      /* In the case of a @class declaration, we store the ident in
	 the TYPE_OBJC_INTERFACE.  If later an @interface is found,
	 we'll replace the ident with the interface.  */
      TYPE_OBJC_INTERFACE (record) = identifier;
      objc_map_put (class_name_map, identifier, NULL_TREE);
    }
}

tree
objc_is_class_name (tree ident)
{
  if (ident && TREE_CODE (ident) == IDENTIFIER_NODE)
    {
      tree t = identifier_global_value (ident);
      if (t)
	ident = t;
    }

  while (ident && TREE_CODE (ident) == TYPE_DECL && DECL_ORIGINAL_TYPE (ident))
    ident = OBJC_TYPE_NAME (DECL_ORIGINAL_TYPE (ident));

  if (ident && TREE_CODE (ident) == RECORD_TYPE)
    ident = OBJC_TYPE_NAME (ident);
#ifdef OBJCPLUS
  if (ident && TREE_CODE (ident) == TYPE_DECL)
    {
      tree type = TREE_TYPE (ident);
      if (type && TREE_CODE (type) == TEMPLATE_TYPE_PARM)
        return NULL_TREE;
      ident = DECL_NAME (ident);
    }
#endif
  if (!ident || TREE_CODE (ident) != IDENTIFIER_NODE)
    return NULL_TREE;

  if (lookup_interface (ident))
    return ident;

  {
    tree target;

    target = objc_map_get (class_name_map, ident);
    if (target != OBJC_MAP_NOT_FOUND)
      return ident;

    target = objc_map_get (alias_name_map, ident);
    if (target != OBJC_MAP_NOT_FOUND)
      return target;
  }

  return 0;
}

/* Check whether TYPE is either 'id' or 'Class'.  */

tree
objc_is_id (tree type)
{
  if (type && TREE_CODE (type) == IDENTIFIER_NODE)
    {
      tree t = identifier_global_value (type);
      if (t)
	type = t;
    }

  if (type && TREE_CODE (type) == TYPE_DECL)
    type = TREE_TYPE (type);

  /* NB: This function may be called before the ObjC front-end has
     been initialized, in which case OBJC_OBJECT_TYPE will (still) be NULL.  */
  return (objc_object_type && type
	  && (IS_ID (type) || IS_CLASS (type) || IS_SUPER (type))
	  ? type
	  : NULL_TREE);
}

/* Check whether TYPE is either 'id', 'Class', or a pointer to an ObjC
   class instance.  This is needed by other parts of the compiler to
   handle ObjC types gracefully.  */

tree
objc_is_object_ptr (tree type)
{
  tree ret;

  type = TYPE_MAIN_VARIANT (type);
  if (!POINTER_TYPE_P (type))
    return 0;

  ret = objc_is_id (type);
  if (!ret)
    ret = objc_is_class_name (TREE_TYPE (type));

  return ret;
}

static int
objc_is_gcable_type (tree type, int or_strong_p)
{
  tree name;

  if (!TYPE_P (type))
    return 0;
  if (objc_is_id (TYPE_MAIN_VARIANT (type)))
    return 1;
  if (or_strong_p && lookup_attribute ("objc_gc", TYPE_ATTRIBUTES (type)))
    return 1;
  if (TREE_CODE (type) != POINTER_TYPE && TREE_CODE (type) != INDIRECT_REF)
    return 0;
  type = TREE_TYPE (type);
  if (TREE_CODE (type) != RECORD_TYPE)
    return 0;
  name = TYPE_NAME (type);
  return (objc_is_class_name (name) != NULL_TREE);
}

static tree
objc_substitute_decl (tree expr, tree oldexpr, tree newexpr)
{
  if (expr == oldexpr)
    return newexpr;

  switch (TREE_CODE (expr))
    {
    case COMPONENT_REF:
      return objc_build_component_ref
	     (objc_substitute_decl (TREE_OPERAND (expr, 0),
				    oldexpr,
				    newexpr),
	      DECL_NAME (TREE_OPERAND (expr, 1)));
    case ARRAY_REF:
      return build_array_ref (input_location,
			      objc_substitute_decl (TREE_OPERAND (expr, 0),
						    oldexpr,
						    newexpr),
			      TREE_OPERAND (expr, 1));
    case INDIRECT_REF:
      return build_indirect_ref (input_location,
				 objc_substitute_decl (TREE_OPERAND (expr, 0),
						       oldexpr,
						       newexpr), RO_ARROW);
    default:
      return expr;
    }
}

static tree
objc_build_ivar_assignment (tree outervar, tree lhs, tree rhs)
{
  tree func_params;
  /* The LHS parameter contains the expression 'outervar->memberspec';
     we need to transform it into '&((typeof(outervar) *) 0)->memberspec',
     where memberspec may be arbitrarily complex (e.g., 'g->f.d[2].g[3]').
  */
  tree offs
    = objc_substitute_decl
      (lhs, outervar, convert (TREE_TYPE (outervar), integer_zero_node));
  tree func
    = (flag_objc_direct_dispatch
       ? objc_assign_ivar_fast_decl
       : objc_assign_ivar_decl);

  offs = convert (integer_type_node, build_unary_op (input_location,
						     ADDR_EXPR, offs, 0));
  offs = fold (offs);
  func_params = tree_cons (NULL_TREE,
	convert (objc_object_type, rhs),
	    tree_cons (NULL_TREE, convert (objc_object_type, outervar),
		tree_cons (NULL_TREE, offs,
		    NULL_TREE)));

  return build_function_call (input_location, func, func_params);
}

static tree
objc_build_global_assignment (tree lhs, tree rhs)
{
  tree func_params = tree_cons (NULL_TREE,
	convert (objc_object_type, rhs),
	    tree_cons (NULL_TREE, convert (build_pointer_type (objc_object_type),
		      build_unary_op (input_location, ADDR_EXPR, lhs, 0)),
		    NULL_TREE));

  return build_function_call (input_location,
			      objc_assign_global_decl, func_params);
}

static tree
objc_build_strong_cast_assignment (tree lhs, tree rhs)
{
  tree func_params = tree_cons (NULL_TREE,
	convert (objc_object_type, rhs),
	    tree_cons (NULL_TREE, convert (build_pointer_type (objc_object_type),
		      build_unary_op (input_location, ADDR_EXPR, lhs, 0)),
		    NULL_TREE));

  return build_function_call (input_location,
			      objc_assign_strong_cast_decl, func_params);
}

static int
objc_is_gcable_p (tree expr)
{
  return (TREE_CODE (expr) == COMPONENT_REF
	  ? objc_is_gcable_p (TREE_OPERAND (expr, 1))
	  : TREE_CODE (expr) == ARRAY_REF
	  ? (objc_is_gcable_p (TREE_TYPE (expr))
	     || objc_is_gcable_p (TREE_OPERAND (expr, 0)))
	  : TREE_CODE (expr) == ARRAY_TYPE
	  ? objc_is_gcable_p (TREE_TYPE (expr))
	  : TYPE_P (expr)
	  ? objc_is_gcable_type (expr, 1)
	  : (objc_is_gcable_p (TREE_TYPE (expr))
	     || (DECL_P (expr)
		 && lookup_attribute ("objc_gc", DECL_ATTRIBUTES (expr)))));
}

static int
objc_is_ivar_reference_p (tree expr)
{
  return (TREE_CODE (expr) == ARRAY_REF
	  ? objc_is_ivar_reference_p (TREE_OPERAND (expr, 0))
	  : TREE_CODE (expr) == COMPONENT_REF
	  ? TREE_CODE (TREE_OPERAND (expr, 1)) == FIELD_DECL
	  : 0);
}

static int
objc_is_global_reference_p (tree expr)
{
  return (TREE_CODE (expr) == INDIRECT_REF || TREE_CODE (expr) == PLUS_EXPR
	  ? objc_is_global_reference_p (TREE_OPERAND (expr, 0))
	  : DECL_P (expr)
	  ? (DECL_FILE_SCOPE_P (expr) || TREE_STATIC (expr))
	  : 0);
}

tree
objc_generate_write_barrier (tree lhs, enum tree_code modifycode, tree rhs)
{
  tree result = NULL_TREE, outer;
  int strong_cast_p = 0, outer_gc_p = 0, indirect_p = 0;

  /* This function is currently only used with the next runtime with
     garbage collection enabled (-fobjc-gc).  */
  gcc_assert (flag_next_runtime);

  /* See if we have any lhs casts, and strip them out.  NB: The lvalue casts
     will have been transformed to the form '*(type *)&expr'.  */
  if (TREE_CODE (lhs) == INDIRECT_REF)
    {
      outer = TREE_OPERAND (lhs, 0);

      while (!strong_cast_p
	     && (CONVERT_EXPR_P (outer)
		 || TREE_CODE (outer) == NON_LVALUE_EXPR))
	{
	  tree lhstype = TREE_TYPE (outer);

	  /* Descend down the cast chain, and record the first objc_gc
	     attribute found.  */
	  if (POINTER_TYPE_P (lhstype))
	    {
	      tree attr
		= lookup_attribute ("objc_gc",
				    TYPE_ATTRIBUTES (TREE_TYPE (lhstype)));

	      if (attr)
		strong_cast_p = 1;
	    }

	  outer = TREE_OPERAND (outer, 0);
	}
    }

  /* If we have a __strong cast, it trumps all else.  */
  if (strong_cast_p)
    {
      if (modifycode != NOP_EXPR)
        goto invalid_pointer_arithmetic;

      if (warn_assign_intercept)
	warning (0, "strong-cast assignment has been intercepted");

      result = objc_build_strong_cast_assignment (lhs, rhs);

      goto exit_point;
    }

  /* the lhs must be of a suitable type, regardless of its underlying
     structure.  */
  if (!objc_is_gcable_p (lhs))
    goto exit_point;

  outer = lhs;

  while (outer
	 && (TREE_CODE (outer) == COMPONENT_REF
	     || TREE_CODE (outer) == ARRAY_REF))
    outer = TREE_OPERAND (outer, 0);

  if (TREE_CODE (outer) == INDIRECT_REF)
    {
      outer = TREE_OPERAND (outer, 0);
      indirect_p = 1;
    }

  outer_gc_p = objc_is_gcable_p (outer);

  /* Handle ivar assignments. */
  if (objc_is_ivar_reference_p (lhs))
    {
      /* if the struct to the left of the ivar is not an Objective-C object (__strong
	 doesn't cut it here), the best we can do here is suggest a cast.  */
      if (!objc_is_gcable_type (TREE_TYPE (outer), 0))
	{
	  /* We may still be able to use the global write barrier... */
	  if (!indirect_p && objc_is_global_reference_p (outer))
	    goto global_reference;

	 suggest_cast:
	  if (modifycode == NOP_EXPR)
	    {
	      if (warn_assign_intercept)
		warning (0, "strong-cast may possibly be needed");
	    }

	  goto exit_point;
	}

      if (modifycode != NOP_EXPR)
        goto invalid_pointer_arithmetic;

      if (warn_assign_intercept)
	warning (0, "instance variable assignment has been intercepted");

      result = objc_build_ivar_assignment (outer, lhs, rhs);

      goto exit_point;
    }

  /* Likewise, intercept assignment to global/static variables if their type is
     GC-marked.  */
  if (objc_is_global_reference_p (outer))
    {
      if (indirect_p)
	goto suggest_cast;

     global_reference:
      if (modifycode != NOP_EXPR)
	{
	 invalid_pointer_arithmetic:
	  if (outer_gc_p)
	    warning (0, "pointer arithmetic for garbage-collected objects not allowed");

	  goto exit_point;
	}

      if (warn_assign_intercept)
	warning (0, "global/static variable assignment has been intercepted");

      result = objc_build_global_assignment (lhs, rhs);
    }

  /* In all other cases, fall back to the normal mechanism.  */
 exit_point:
  return result;
}

/* Implementation of the table mapping a class name (as an identifier)
   to a class node.  The two public functions for it are
   lookup_interface() and add_interface().  add_interface() is only
   used in this file, so we can make it static.  */

static GTY(()) objc_map_t interface_map;

static void
interface_hash_init (void)
{
  interface_map = objc_map_alloc_ggc (200);  
}

static tree
add_interface (tree class_name, tree name)
{
  /* Put interfaces on list in reverse order.  */
  TREE_CHAIN (class_name) = interface_chain;
  interface_chain = class_name;

  /* Add it to the map.  */
  objc_map_put (interface_map, name, class_name);

  return interface_chain;
}

tree
lookup_interface (tree ident)
{
#ifdef OBJCPLUS
  if (ident && TREE_CODE (ident) == TYPE_DECL)
    ident = DECL_NAME (ident);
#endif

  if (ident == NULL_TREE || TREE_CODE (ident) != IDENTIFIER_NODE)
    return NULL_TREE;

  {
    tree interface = objc_map_get (interface_map, ident);

    if (interface == OBJC_MAP_NOT_FOUND)
      return NULL_TREE;
    else
      return interface;
  }
}



/* Implement @defs (<classname>) within struct bodies.  */

tree
objc_get_class_ivars (tree class_name)
{
  tree interface = lookup_interface (class_name);

  if (interface)
    return get_class_ivars (interface, true);

  error ("cannot find interface declaration for %qE",
	 class_name);

  return error_mark_node;
}


/* Functions used by the hashtable for field duplicates in
   objc_detect_field_duplicates().  Ideally, we'd use a standard
   key-value dictionary hashtable , and store as keys the field names,
   and as values the actual declarations (used to print nice error
   messages with the locations).  But, the hashtable we are using only
   allows us to store keys in the hashtable, without values (it looks
   more like a set).  So, we store the DECLs, but define equality as
   DECLs having the same name, and hash as the hash of the name.  */

struct decl_name_hash : typed_noop_remove <tree_node>
{
  typedef tree_node value_type;
  typedef tree_node compare_type;
  static inline hashval_t hash (const value_type *);
  static inline bool equal (const value_type *, const compare_type *);
};

inline hashval_t
decl_name_hash::hash (const value_type *q)
{
  return (hashval_t) ((intptr_t)(DECL_NAME (q)) >> 3);
}

inline bool
decl_name_hash::equal (const value_type *a, const compare_type *b)
{
  return DECL_NAME (a) == DECL_NAME (b);
}

/* Called when checking the variables in a struct.  If we are not
   doing the ivars list inside an @interface context, then return
   false.  Else, perform the check for duplicate ivars, then return
   true.  The check for duplicates checks if an instance variable with
   the same name exists in the class or in a superclass.  If
   'check_superclasses_only' is set to true, then it is assumed that
   checks for instance variables in the same class has already been
   performed (this is the case for ObjC++) and only the instance
   variables of superclasses are checked.  */
bool
objc_detect_field_duplicates (bool check_superclasses_only)
{
  if (!objc_collecting_ivars || !objc_interface_context
      || TREE_CODE (objc_interface_context) != CLASS_INTERFACE_TYPE)
    return false;

  /* We have two ways of doing this check:

  "direct comparison": we iterate over the instance variables and
  compare them directly.  This works great for small numbers of
  instance variables (such as 10 or 20), which are extremely common.
  But it will potentially take forever for the pathological case with
  a huge number (eg, 10k) of instance variables.

  "hashtable": we use a hashtable, which requires a single sweep
  through the list of instances variables.  This is much slower for a
  small number of variables, and we only use it for large numbers.

  To decide which one to use, we need to get an idea of how many
  instance variables we have to compare.  */
  {
    unsigned int number_of_ivars_to_check = 0;
    {
      tree ivar;
      for (ivar = CLASS_RAW_IVARS (objc_interface_context);
	   ivar; ivar = DECL_CHAIN (ivar))
	{
	  /* Ignore anonymous ivars.  */
	  if (DECL_NAME (ivar))
	    number_of_ivars_to_check++;
	}
    }

    /* Exit if there is nothing to do.  */
    if (number_of_ivars_to_check == 0)
      return true;

    /* In case that there are only 1 or 2 instance variables to check,
       we always use direct comparison.  If there are more, it is
       worth iterating over the instance variables in the superclass
       to count how many there are (note that this has the same cost
       as checking 1 instance variable by direct comparison, which is
       why we skip this check in the case of 1 or 2 ivars and just do
       the direct comparison) and then decide if it worth using a
       hashtable.  */
    if (number_of_ivars_to_check > 2)
      {
	unsigned int number_of_superclass_ivars = 0;
	{
	  tree interface;
	  for (interface = lookup_interface (CLASS_SUPER_NAME (objc_interface_context));
	       interface; interface = lookup_interface (CLASS_SUPER_NAME (interface)))
	    {
	      tree ivar;
	      for (ivar = CLASS_RAW_IVARS (interface);
		   ivar; ivar = DECL_CHAIN (ivar))
		number_of_superclass_ivars++;
	    }
	}

	/* We use a hashtable if we have over 10k comparisons.  */
	if (number_of_ivars_to_check * (number_of_superclass_ivars
					+ (number_of_ivars_to_check / 2))
	    > 10000)
	  {
	    /* First, build the hashtable by putting all the instance
	       variables of superclasses in it.  */
	    hash_table <decl_name_hash> htab;
	    htab.create (37);
	    tree interface;
	    for (interface = lookup_interface (CLASS_SUPER_NAME
					       (objc_interface_context));
		 interface; interface = lookup_interface
		   (CLASS_SUPER_NAME (interface)))
	      {
		tree ivar;
		for (ivar = CLASS_RAW_IVARS (interface); ivar;
		     ivar = DECL_CHAIN (ivar))
		  {
		    if (DECL_NAME (ivar) != NULL_TREE)
		      {
			tree_node **slot = htab.find_slot (ivar, INSERT);
			/* Do not check for duplicate instance
			   variables in superclasses.  Errors have
			   already been generated.  */
			*slot = ivar;
		      }
		  }
	      }

	    /* Now, we go through all the instance variables in the
	       class, and check that they are not in the
	       hashtable.  */
	    if (check_superclasses_only)
	      {
		tree ivar;
		for (ivar = CLASS_RAW_IVARS (objc_interface_context); ivar;
		     ivar = DECL_CHAIN (ivar))
		  {
		    if (DECL_NAME (ivar) != NULL_TREE)
		      {
			tree duplicate_ivar = htab.find (ivar);
			if (duplicate_ivar != HTAB_EMPTY_ENTRY)
			  {
			    error_at (DECL_SOURCE_LOCATION (ivar),
				      "duplicate instance variable %q+D",
				      ivar);
			    inform (DECL_SOURCE_LOCATION (duplicate_ivar),
				    "previous declaration of %q+D",
				    duplicate_ivar);
			    /* FIXME: Do we need the following ?  */
			    /* DECL_NAME (ivar) = NULL_TREE; */
			  }
		      }
		  }
	      }
	    else
	      {
		/* If we're checking for duplicates in the class as
		   well, we insert variables in the hashtable as we
		   check them, so if a duplicate follows, it will be
		   caught.  */
		tree ivar;
		for (ivar = CLASS_RAW_IVARS (objc_interface_context); ivar;
		     ivar = DECL_CHAIN (ivar))
		  {
		    if (DECL_NAME (ivar) != NULL_TREE)
		      {
			tree_node **slot = htab.find_slot (ivar, INSERT);
			if (*slot)
			  {
			    tree duplicate_ivar = (tree)(*slot);
			    error_at (DECL_SOURCE_LOCATION (ivar),
				      "duplicate instance variable %q+D",
				      ivar);
			    inform (DECL_SOURCE_LOCATION (duplicate_ivar),
				    "previous declaration of %q+D",
				    duplicate_ivar);
			    /* FIXME: Do we need the following ?  */
			    /* DECL_NAME (ivar) = NULL_TREE; */
			  }
			*slot = ivar;
		      }
		  }
	      }
	    htab.dispose ();
	    return true;
	  }
      }
  }

  /* This is the "direct comparison" approach, which is used in most
     non-pathological cases.  */
  {
    /* Walk up to class hierarchy, starting with this class (this is
       the external loop, because lookup_interface() is expensive, and
       we want to do it few times).  */
    tree interface = objc_interface_context;

    if (check_superclasses_only)
      interface = lookup_interface (CLASS_SUPER_NAME (interface));

    for ( ; interface; interface = lookup_interface
	    (CLASS_SUPER_NAME (interface)))
      {
	tree ivar_being_checked;

	for (ivar_being_checked = CLASS_RAW_IVARS (objc_interface_context);
	     ivar_being_checked;
	     ivar_being_checked = DECL_CHAIN (ivar_being_checked))
	  {
	    tree decl;

	    /* Ignore anonymous ivars.  */
	    if (DECL_NAME (ivar_being_checked) == NULL_TREE)
	      continue;

	    /* Note how we stop when we find the ivar we are checking
	       (this can only happen in the main class, not
	       superclasses), to avoid comparing things twice
	       (otherwise, for each ivar, you'd compare A to B then B
	       to A, and get duplicated error messages).  */
	    for (decl = CLASS_RAW_IVARS (interface);
		 decl && decl != ivar_being_checked;
		 decl = DECL_CHAIN (decl))
	      {
		if (DECL_NAME (ivar_being_checked) == DECL_NAME (decl))
		  {
		    error_at (DECL_SOURCE_LOCATION (ivar_being_checked),
			      "duplicate instance variable %q+D",
			      ivar_being_checked);
		    inform (DECL_SOURCE_LOCATION (decl),
			    "previous declaration of %q+D",
			    decl);
		    /* FIXME: Do we need the following ?  */
		    /* DECL_NAME (ivar_being_checked) = NULL_TREE; */
		  }
	      }
	  }
      }
  }
  return true;
}

/* Used by: build_private_template, continue_class,
   and for @defs constructs.  */

static tree
get_class_ivars (tree interface, bool inherited)
{
  tree ivar_chain = copy_list (CLASS_RAW_IVARS (interface));

  /* Both CLASS_RAW_IVARS and CLASS_IVARS contain a list of ivars declared
     by the current class (i.e., they do not include super-class ivars).
     However, the CLASS_IVARS list will be side-effected by a call to
     finish_struct(), which will fill in field offsets.  */
  if (!CLASS_IVARS (interface))
    CLASS_IVARS (interface) = ivar_chain;

  if (!inherited)
    return ivar_chain;

  while (CLASS_SUPER_NAME (interface))
    {
      /* Prepend super-class ivars.  */
      interface = lookup_interface (CLASS_SUPER_NAME (interface));
      ivar_chain = chainon (copy_list (CLASS_RAW_IVARS (interface)),
			    ivar_chain);
    }

  return ivar_chain;
}

void
objc_maybe_warn_exceptions (location_t loc)
{
  /* -fobjc-exceptions is required to enable Objective-C exceptions.
     For example, on Darwin, ObjC exceptions require a sufficiently
     recent version of the runtime, so the user must ask for them
     explicitly.  On other platforms, at the moment -fobjc-exceptions
     triggers -fexceptions which again is required for exceptions to
     work.  */
  if (!flag_objc_exceptions)
    {
      /* Warn only once per compilation unit.  */
      static bool warned = false;

      if (!warned)
	{
	  error_at (loc, "%<-fobjc-exceptions%> is required to enable Objective-C exception syntax");
	  warned = true;
	}
    }
}

static struct objc_try_context *cur_try_context;

/* Called just after parsing the @try and its associated BODY.  We now
   must prepare for the tricky bits -- handling the catches and finally.  */

void
objc_begin_try_stmt (location_t try_locus, tree body)
{
  struct objc_try_context *c = XCNEW (struct objc_try_context);
  c->outer = cur_try_context;
  c->try_body = body;
  c->try_locus = try_locus;
  c->end_try_locus = input_location;
  cur_try_context = c;

  /* Collect the list of local variables.  We'll mark them as volatile
     at the end of compilation of this function to prevent them being
     clobbered by setjmp/longjmp.  */
  if (flag_objc_sjlj_exceptions)
    objc_mark_locals_volatile (NULL);
}

/* Called just after parsing "@catch (parm)".  Open a binding level,
   enter DECL into the binding level, and initialize it.  Leave the
   binding level open while the body of the compound statement is
   parsed.  If DECL is NULL_TREE, then we are compiling "@catch(...)"
   which we compile as "@catch(id tmp_variable)".  */

void
objc_begin_catch_clause (tree decl)
{
  tree compound, type, t;
  bool ellipsis = false;

  /* Begin a new scope that the entire catch clause will live in.  */
  compound = c_begin_compound_stmt (true);

  /* Create the appropriate declaration for the argument.  */
 if (decl == error_mark_node)
   type = error_mark_node;
 else
   {
     if (decl == NULL_TREE)
       {
	 /* If @catch(...) was specified, create a temporary variable of
	    type 'id' and use it.  */
	 decl = objc_create_temporary_var (objc_object_type, "__objc_generic_catch_var");
	 DECL_SOURCE_LOCATION (decl) = input_location;
	 /* ... but allow the runtime to differentiate between ellipsis and the
	    case of @catch (id xyz).  */
	 ellipsis = true;
       }
     else
       {
	 /* The parser passed in a PARM_DECL, but what we really want is a VAR_DECL.  */
	 decl = build_decl (input_location,
			    VAR_DECL, DECL_NAME (decl), TREE_TYPE (decl));
       }
     lang_hooks.decls.pushdecl (decl);

     /* Mark the declaration as used so you never any warnings whether
	you use the exception argument or not.  TODO: Implement a
	-Wunused-exception-parameter flag, which would cause warnings
	if exception parameter is not used.  */
     TREE_USED (decl) = 1;
     DECL_READ_P (decl) = 1;

     type = TREE_TYPE (decl);
   }

  /* Verify that the type of the catch is valid.  It must be a pointer
     to an Objective-C class, or "id" (which is catch-all).  */
  if (type == error_mark_node)
    {
      ;/* Just keep going.  */
    }
  else if (!objc_type_valid_for_messaging (type, false))
    {
      error ("@catch parameter is not a known Objective-C class type");
      type = error_mark_node;
    }
  else if (TYPE_HAS_OBJC_INFO (TREE_TYPE (type))
	   && TYPE_OBJC_PROTOCOL_LIST (TREE_TYPE (type)))
    {
      error ("@catch parameter can not be protocol-qualified");
      type = error_mark_node;
    }
  else if (POINTER_TYPE_P (type) && objc_is_object_id (TREE_TYPE (type)))
    /* @catch (id xyz) or @catch (...) but we note this for runtimes that
       identify 'id'.  */
    ;
  else
    {
      /* If 'type' was built using typedefs, we need to get rid of
	 them and get a simple pointer to the class.  */
      bool is_typedef = false;
      tree x = TYPE_MAIN_VARIANT (type);

      /* Skip from the pointer to the pointee.  */
      if (TREE_CODE (x) == POINTER_TYPE)
	x = TREE_TYPE (x);

      /* Traverse typedef aliases */
      while (TREE_CODE (x) == RECORD_TYPE && OBJC_TYPE_NAME (x)
	     && TREE_CODE (OBJC_TYPE_NAME (x)) == TYPE_DECL
	     && DECL_ORIGINAL_TYPE (OBJC_TYPE_NAME (x)))
	{
	  is_typedef = true;
	  x = DECL_ORIGINAL_TYPE (OBJC_TYPE_NAME (x));
	}

      /* If it was a typedef, build a pointer to the final, original
	 class.  */
      if (is_typedef)
	type = build_pointer_type (x);

      if (cur_try_context->catch_list)
	{
	  /* Examine previous @catch clauses and see if we've already
	     caught the type in question.  */
	  tree_stmt_iterator i = tsi_start (cur_try_context->catch_list);
	  for (; !tsi_end_p (i); tsi_next (&i))
	    {
	      tree stmt = tsi_stmt (i);
	      t = CATCH_TYPES (stmt);
	      if (t == error_mark_node)
		continue;
	      if (!t || DERIVED_FROM_P (TREE_TYPE (t), TREE_TYPE (type)))
		{
		  warning (0, "exception of type %<%T%> will be caught",
			   TREE_TYPE (type));
		  warning_at  (EXPR_LOCATION (stmt), 0, "   by earlier handler for %<%T%>",
			       TREE_TYPE (t ? t : objc_object_type));
		  break;
		}
	    }
	}
    }

  t = (*runtime.begin_catch) (&cur_try_context, type, decl, compound, ellipsis);
  add_stmt (t);
}

/* Called just after parsing the closing brace of a @catch clause.  Close
   the open binding level, and record a CATCH_EXPR for it.  */

void
objc_finish_catch_clause (void)
{
  tree c = cur_try_context->current_catch;
  cur_try_context->current_catch = NULL;
  cur_try_context->end_catch_locus = input_location;

  CATCH_BODY (c) = c_end_compound_stmt (input_location, CATCH_BODY (c), 1);

  (*runtime.finish_catch) (&cur_try_context, c);
}

/* Called after parsing a @finally clause and its associated BODY.
   Record the body for later placement.  */

void
objc_build_finally_clause (location_t finally_locus, tree body)
{
  cur_try_context->finally_body = body;
  cur_try_context->finally_locus = finally_locus;
  cur_try_context->end_finally_locus = input_location;
}

/* Called to finalize a @try construct.  */

tree
objc_finish_try_stmt (void)
{
  struct objc_try_context *c = cur_try_context;
  tree stmt;

  if (c->catch_list == NULL && c->finally_body == NULL)
    error ("%<@try%> without %<@catch%> or %<@finally%>");

  stmt = (*runtime.finish_try_stmt) (&cur_try_context);
  add_stmt (stmt);

  cur_try_context = c->outer;
  free (c);
  return stmt;
}

tree
objc_build_throw_stmt (location_t loc, tree throw_expr)
{
  bool rethrown = false;

  objc_maybe_warn_exceptions (loc);

  /* Don't waste time trying to build something if we're already dead.  */
  if (throw_expr == error_mark_node)
    return error_mark_node;

  if (throw_expr == NULL)
    {
      /* If we're not inside a @catch block, there is no "current
	 exception" to be rethrown.  */
      if (cur_try_context == NULL
          || cur_try_context->current_catch == NULL)
	{
	  error_at (loc, "%<@throw%> (rethrow) used outside of a @catch block");
	  return error_mark_node;
	}

      /* Otherwise the object is still sitting in the EXC_PTR_EXPR
	 value that we get from the runtime.  */
      throw_expr = (*runtime.build_exc_ptr) (&cur_try_context);
      rethrown = true;
    }
  else
    {
      if (!objc_type_valid_for_messaging (TREE_TYPE (throw_expr), true))
	{
	  error_at (loc, "%<@throw%> argument is not an object");
	  return error_mark_node;
	}
    }

  return (*runtime.build_throw_stmt) (loc, throw_expr, rethrown);
}

tree
objc_build_synchronized (location_t start_locus, tree object_expr, tree body)
{
  /* object_expr should never be NULL; but in case it is, convert it to
     error_mark_node.  */
  if (object_expr == NULL)
    object_expr = error_mark_node;

  /* Validate object_expr.  If not valid, set it to error_mark_node.  */
  if (object_expr != error_mark_node)
    {
      if (!objc_type_valid_for_messaging (TREE_TYPE (object_expr), true))
	{
	  error_at (start_locus, "%<@synchronized%> argument is not an object");
	  object_expr = error_mark_node;
	}
    }

  if (object_expr == error_mark_node)
    {
      /* If we found an error, we simply ignore the '@synchronized'.
	 Compile the body so we can keep going with minimal
	 casualties.  */
      return add_stmt (body);
    }
  else
    {
      tree call;
      tree args;

      /* objc_sync_enter (object_expr); */
      object_expr = save_expr (object_expr);
      args = tree_cons (NULL, object_expr, NULL);
      call = build_function_call (input_location,
				  objc_sync_enter_decl, args);
      SET_EXPR_LOCATION (call, start_locus);
      add_stmt (call);

      /* Build "objc_sync_exit (object_expr);" but do not add it yet;
	 it goes inside the @finalize() clause.  */
      args = tree_cons (NULL, object_expr, NULL);
      call = build_function_call (input_location,
				  objc_sync_exit_decl, args);
      SET_EXPR_LOCATION (call, input_location);

      /* @try { body; } */
      objc_begin_try_stmt (start_locus, body);

      /* @finally { objc_sync_exit (object_expr); } */
      objc_build_finally_clause (input_location, call);

      /* End of try statement.  */
      return objc_finish_try_stmt ();
    }
}

/* Construct a C struct corresponding to ObjC class CLASS, with the same
   name as the class:

   struct <classname> {
     struct _objc_class *isa;
     ...
   };  */

static void
build_private_template (tree klass)
{
  if (!CLASS_STATIC_TEMPLATE (klass))
    {
      tree record = objc_build_struct (klass,
				       get_class_ivars (klass, false),
				       CLASS_SUPER_NAME (klass));

      /* Set the TREE_USED bit for this struct, so that stab generator
	 can emit stabs for this struct type.  */
      if (flag_debug_only_used_symbols && TYPE_STUB_DECL (record))
	TREE_USED (TYPE_STUB_DECL (record)) = 1;

      /* Copy the attributes from the class to the type.  */
      if (TREE_DEPRECATED (klass))
	TREE_DEPRECATED (record) = 1;
    }
}

/* Generate either '- .cxx_construct' or '- .cxx_destruct' for the
   current class.  */
#ifdef OBJCPLUS
static void
objc_generate_cxx_ctor_or_dtor (bool dtor)
{
  tree fn, body, compound_stmt, ivar;

  /* - (id) .cxx_construct { ... return self; } */
  /* - (void) .cxx_construct { ... }            */

  objc_start_method_definition
    (false /* is_class_method */,
     objc_build_method_signature (false /* is_class_method */,
				  build_tree_list (NULL_TREE,
						   dtor
						   ? void_type_node
						   : objc_object_type),
				  get_identifier (dtor
						  ? TAG_CXX_DESTRUCT
						  : TAG_CXX_CONSTRUCT),
				  make_node (TREE_LIST),
				  false), NULL, NULL_TREE);
  body = begin_function_body ();
  compound_stmt = begin_compound_stmt (0);

  ivar = CLASS_IVARS (implementation_template);
  /* Destroy ivars in reverse order.  */
  if (dtor)
    ivar = nreverse (copy_list (ivar));

  for (; ivar; ivar = TREE_CHAIN (ivar))
    {
      if (TREE_CODE (ivar) == FIELD_DECL)
	{
	  tree type = TREE_TYPE (ivar);

	  /* Call the ivar's default constructor or destructor.  Do not
	     call the destructor unless a corresponding constructor call
	     has also been made (or is not needed).  */
	  if (MAYBE_CLASS_TYPE_P (type)
	      && (dtor
		  ? (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (type)
		     && (!TYPE_NEEDS_CONSTRUCTING (type)
			 || TYPE_HAS_DEFAULT_CONSTRUCTOR (type)))
		  : (TYPE_NEEDS_CONSTRUCTING (type)
		     && TYPE_HAS_DEFAULT_CONSTRUCTOR (type))))
	    finish_expr_stmt
	     (build_special_member_call
	      (build_ivar_reference (DECL_NAME (ivar)),
	       dtor ? complete_dtor_identifier : complete_ctor_identifier,
	       NULL, type, LOOKUP_NORMAL, tf_warning_or_error));
	}
    }

  /* The constructor returns 'self'.  */
  if (!dtor)
    finish_return_stmt (self_decl);

  finish_compound_stmt (compound_stmt);
  finish_function_body (body);
  fn = current_function_decl;
  finish_function ();
  objc_finish_method_definition (fn);
}

/* The following routine will examine the current @interface for any
   non-POD C++ ivars requiring non-trivial construction and/or
   destruction, and then synthesize special '- .cxx_construct' and/or
   '- .cxx_destruct' methods which will run the appropriate
   construction or destruction code.  Note that ivars inherited from
   super-classes are _not_ considered.  */
static void
objc_generate_cxx_cdtors (void)
{
  bool need_ctor = false, need_dtor = false;
  tree ivar;

  /* Error case, due to possibly an extra @end. */
  if (!objc_implementation_context)
    return;

  /* We do not want to do this for categories, since they do not have
     their own ivars.  */

  if (TREE_CODE (objc_implementation_context) != CLASS_IMPLEMENTATION_TYPE)
    return;

  /* First, determine if we even need a constructor and/or destructor.  */

  for (ivar = CLASS_IVARS (implementation_template); ivar;
       ivar = TREE_CHAIN (ivar))
    {
      if (TREE_CODE (ivar) == FIELD_DECL)
	{
	  tree type = TREE_TYPE (ivar);

	  if (MAYBE_CLASS_TYPE_P (type))
	    {
	      if (TYPE_NEEDS_CONSTRUCTING (type)
		  && TYPE_HAS_DEFAULT_CONSTRUCTOR (type))
		/* NB: If a default constructor is not available, we will not
		   be able to initialize this ivar; the add_instance_variable()
		   routine will already have warned about this.  */
		need_ctor = true;

	      if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (type)
		  && (!TYPE_NEEDS_CONSTRUCTING (type)
		      || TYPE_HAS_DEFAULT_CONSTRUCTOR (type)))
		/* NB: If a default constructor is not available, we will not
		   call the destructor either, for symmetry.  */
		need_dtor = true;
	    }
	}
    }

  /* Generate '- .cxx_construct' if needed.  */

  if (need_ctor)
    objc_generate_cxx_ctor_or_dtor (false);

  /* Generate '- .cxx_destruct' if needed.  */

  if (need_dtor)
    objc_generate_cxx_ctor_or_dtor (true);

  /* The 'imp_list' variable points at an imp_entry record for the current
     @implementation.  Record the existence of '- .cxx_construct' and/or
     '- .cxx_destruct' methods therein; it will be included in the
     metadata for the class if the runtime needs it.  */
  imp_list->has_cxx_cdtors = (need_ctor || need_dtor);
}
#endif

static void
error_with_ivar (const char *message, tree decl)
{
  error_at (DECL_SOURCE_LOCATION (decl), "%s %qs",
	    message, identifier_to_locale (gen_declaration (decl)));

}

static void
check_ivars (tree inter, tree imp)
{
  tree intdecls = CLASS_RAW_IVARS (inter);
  tree impdecls = CLASS_RAW_IVARS (imp);

  while (1)
    {
      tree t1, t2;

#ifdef OBJCPLUS
      if (intdecls && TREE_CODE (intdecls) == TYPE_DECL)
	intdecls = TREE_CHAIN (intdecls);
#endif
      if (intdecls == 0 && impdecls == 0)
	break;
      if (intdecls == 0 || impdecls == 0)
	{
	  error ("inconsistent instance variable specification");
	  break;
	}

      t1 = TREE_TYPE (intdecls); t2 = TREE_TYPE (impdecls);

      if (!comptypes (t1, t2)
	  || !tree_int_cst_equal (DECL_INITIAL (intdecls),
				  DECL_INITIAL (impdecls)))
	{
	  if (DECL_NAME (intdecls) == DECL_NAME (impdecls))
	    {
	      error_with_ivar ("conflicting instance variable type",
			       impdecls);
	      error_with_ivar ("previous declaration of",
			       intdecls);
	    }
	  else			/* both the type and the name don't match */
	    {
	      error ("inconsistent instance variable specification");
	      break;
	    }
	}

      else if (DECL_NAME (intdecls) != DECL_NAME (impdecls))
	{
	  error_with_ivar ("conflicting instance variable name",
			   impdecls);
	  error_with_ivar ("previous declaration of",
			   intdecls);
	}

      intdecls = DECL_CHAIN (intdecls);
      impdecls = DECL_CHAIN (impdecls);
    }
}


static void
mark_referenced_methods (void)
{
  struct imp_entry *impent;
  tree chain;

  for (impent = imp_list; impent; impent = impent->next)
    {
      chain = CLASS_CLS_METHODS (impent->imp_context);
      while (chain)
	{
	  cgraph_mark_force_output_node (
			   cgraph_get_create_node (METHOD_DEFINITION (chain)));
	  chain = DECL_CHAIN (chain);
	}

      chain = CLASS_NST_METHODS (impent->imp_context);
      while (chain)
	{
	  cgraph_mark_force_output_node (
			   cgraph_get_create_node (METHOD_DEFINITION (chain)));
	  chain = DECL_CHAIN (chain);
	}
    }
}

/* If type is empty or only type qualifiers are present, add default
   type of id (otherwise grokdeclarator will default to int).  */
static inline tree
adjust_type_for_id_default (tree type)
{
  if (!type)
    type = make_node (TREE_LIST);

  if (!TREE_VALUE (type))
    TREE_VALUE (type) = objc_object_type;
  else if (TREE_CODE (TREE_VALUE (type)) == RECORD_TYPE
	   && TYPED_OBJECT (TREE_VALUE (type)))
    error ("can not use an object as parameter to a method");

  return type;
}

/* Return a KEYWORD_DECL built using the specified key_name, arg_type,
   arg_name and attributes. (TODO: Rename KEYWORD_DECL to
   OBJC_METHOD_PARM_DECL ?)

   A KEYWORD_DECL is a tree representing the declaration of a
   parameter of an Objective-C method.  It is produced when parsing a
   fragment of Objective-C method declaration of the form

   keyworddecl:
     selector ':' '(' typename ')' identifier

   For example, take the Objective-C method

   -(NSString *)pathForResource:(NSString *)resource ofType:(NSString *)type;

   the two fragments "pathForResource:(NSString *)resource" and
   "ofType:(NSString *)type" will generate a KEYWORD_DECL each.  The
   KEYWORD_DECL stores the 'key_name' (eg, identifier for
   "pathForResource"), the 'arg_type' (eg, tree representing a
   NSString *), the 'arg_name' (eg identifier for "resource") and
   potentially some attributes (for example, a tree representing
   __attribute__ ((unused)) if such an attribute was attached to a
   certain parameter).  You can access this information using the
   TREE_TYPE (for arg_type), KEYWORD_ARG_NAME (for arg_name),
   KEYWORD_KEY_NAME (for key_name), DECL_ATTRIBUTES (for attributes).

   'key_name' is an identifier node (and is optional as you can omit
   it in Objective-C methods).
   'arg_type' is a tree list (and is optional too if no parameter type
   was specified).
   'arg_name' is an identifier node and is required.
   'attributes' is an optional tree containing parameter attributes.  */
tree
objc_build_keyword_decl (tree key_name, tree arg_type,
			 tree arg_name, tree attributes)
{
  tree keyword_decl;

  if (flag_objc1_only && attributes)
    error_at (input_location, "method argument attributes are not available in Objective-C 1.0");

  /* If no type is specified, default to "id".  */
  arg_type = adjust_type_for_id_default (arg_type);

  keyword_decl = make_node (KEYWORD_DECL);

  TREE_TYPE (keyword_decl) = arg_type;
  KEYWORD_ARG_NAME (keyword_decl) = arg_name;
  KEYWORD_KEY_NAME (keyword_decl) = key_name;
  DECL_ATTRIBUTES (keyword_decl) = attributes;

  return keyword_decl;
}

/* Given a chain of keyword_decl's, synthesize the full keyword selector.  */
static tree
build_keyword_selector (tree selector)
{
  int len = 0;
  tree key_chain, key_name;
  char *buf;

  /* Scan the selector to see how much space we'll need.  */
  for (key_chain = selector; key_chain; key_chain = TREE_CHAIN (key_chain))
    {
      switch (TREE_CODE (selector))
	{
	case KEYWORD_DECL:
	  key_name = KEYWORD_KEY_NAME (key_chain);
	  break;
	case TREE_LIST:
	  key_name = TREE_PURPOSE (key_chain);
	  break;
	default:
	  gcc_unreachable ();
	}

      if (key_name)
	len += IDENTIFIER_LENGTH (key_name) + 1;
      else
	/* Just a ':' arg.  */
	len++;
    }

  buf = (char *) alloca (len + 1);
  /* Start the buffer out as an empty string.  */
  buf[0] = '\0';

  for (key_chain = selector; key_chain; key_chain = TREE_CHAIN (key_chain))
    {
      switch (TREE_CODE (selector))
	{
	case KEYWORD_DECL:
	  key_name = KEYWORD_KEY_NAME (key_chain);
	  break;
	case TREE_LIST:
	  key_name = TREE_PURPOSE (key_chain);
	  /* The keyword decl chain will later be used as a function
	     argument chain.  Unhook the selector itself so as to not
	     confuse other parts of the compiler.  */
	  TREE_PURPOSE (key_chain) = NULL_TREE;
	  break;
	default:
	  gcc_unreachable ();
	}

      if (key_name)
	strcat (buf, IDENTIFIER_POINTER (key_name));
      strcat (buf, ":");
    }

  return get_identifier_with_length (buf, len);
}

/* Used for declarations and definitions.  */

static tree
build_method_decl (enum tree_code code, tree ret_type, tree selector,
		   tree add_args, bool ellipsis)
{
  tree method_decl;

  /* If no type is specified, default to "id".  */
  ret_type = adjust_type_for_id_default (ret_type);

  /* Note how a method_decl has a TREE_TYPE which is not the function
     type of the function implementing the method, but only the return
     type of the method.  We may want to change this, and store the
     entire function type in there (eg, it may be used to simplify
     dealing with attributes below).  */
  method_decl = make_node (code);
  TREE_TYPE (method_decl) = ret_type;

  /* If we have a keyword selector, create an identifier_node that
     represents the full selector name (`:' included)...  */
  if (TREE_CODE (selector) == KEYWORD_DECL)
    {
      METHOD_SEL_NAME (method_decl) = build_keyword_selector (selector);
      METHOD_SEL_ARGS (method_decl) = selector;
      METHOD_ADD_ARGS (method_decl) = add_args;
      METHOD_ADD_ARGS_ELLIPSIS_P (method_decl) = ellipsis;
    }
  else
    {
      METHOD_SEL_NAME (method_decl) = selector;
      METHOD_SEL_ARGS (method_decl) = NULL_TREE;
      METHOD_ADD_ARGS (method_decl) = NULL_TREE;
    }

  return method_decl;
}

/* This routine processes objective-c method attributes. */

static void
objc_decl_method_attributes (tree *node, tree attributes, int flags)
{
  /* TODO: Replace the hackery below.  An idea would be to store the
     full function type in the method declaration (for example in
     TREE_TYPE) and then expose ObjC method declarations to c-family
     and they could deal with them by simply treating them as
     functions.  */

  /* Because of the dangers in the hackery below, we filter out any
     attribute that we do not know about.  For the ones we know about,
     we know that they work with the hackery.  For the other ones,
     there is no guarantee, so we have to filter them out.  */
  tree filtered_attributes = NULL_TREE;

  if (attributes)
    {
      tree attribute;
      for (attribute = attributes; attribute; attribute = TREE_CHAIN (attribute))
	{
	  tree name = TREE_PURPOSE (attribute);

	  if (is_attribute_p  ("deprecated", name)
	      || is_attribute_p ("sentinel", name)
	      || is_attribute_p ("noreturn", name))
	    {
	      /* An attribute that we support; add it to the filtered
		 attributes.  */
	      filtered_attributes = chainon (filtered_attributes,
					     copy_node (attribute));
	    }
	  else if (is_attribute_p ("format", name))
	    {
	      /* "format" is special because before adding it to the
		 filtered attributes we need to adjust the specified
		 format by adding the hidden function parameters for
		 an Objective-C method (self, _cmd).  */
	      tree new_attribute = copy_node (attribute);

	      /* Check the arguments specified with the attribute, and
		 modify them adding 2 for the two hidden arguments.
		 Note how this differs from C++; according to the
		 specs, C++ does not do it so you have to add the +1
		 yourself.  For Objective-C, instead, the compiler
		 adds the +2 for you.  */

	      /* The attribute arguments have not been checked yet, so
		 we need to be careful as they could be missing or
		 invalid.  If anything looks wrong, we skip the
		 process and the compiler will complain about it later
		 when it validates the attribute.  */
	      /* Check that we have at least three arguments.  */
	      if (TREE_VALUE (new_attribute)
		  && TREE_CHAIN (TREE_VALUE (new_attribute))
		  && TREE_CHAIN (TREE_CHAIN (TREE_VALUE (new_attribute))))
		{
		  tree second_argument = TREE_CHAIN (TREE_VALUE (new_attribute));
		  tree third_argument = TREE_CHAIN (second_argument);
		  tree number;

		  /* This is the second argument, the "string-index",
		     which specifies the index of the format string
		     argument.  Add 2.  */
		  number = TREE_VALUE (second_argument);
		  if (number
		      && TREE_CODE (number) == INTEGER_CST
		      && !wi::eq_p (number, 0))
		    TREE_VALUE (second_argument)
		      = wide_int_to_tree (TREE_TYPE (number),
					  wi::add (number, 2));

		  /* This is the third argument, the "first-to-check",
		     which specifies the index of the first argument to
		     check.  This could be 0, meaning it is not available,
		     in which case we don't need to add 2.  Add 2 if not
		     0.  */
		  number = TREE_VALUE (third_argument);
		  if (number
		      && TREE_CODE (number) == INTEGER_CST
		      && !wi::eq_p (number, 0))
		    TREE_VALUE (third_argument)
		      = wide_int_to_tree (TREE_TYPE (number),
					  wi::add (number, 2));
		}
	      filtered_attributes = chainon (filtered_attributes,
					     new_attribute);
	    }
	  else if (is_attribute_p ("nonnull", name))
	    {
	      /* We need to fixup all the argument indexes by adding 2
		 for the two hidden arguments of an Objective-C method
		 invocation, similat to what we do above for the
		 "format" attribute.  */
	      /* FIXME: This works great in terms of implementing the
		 functionality, but the warnings that are produced by
		 nonnull do mention the argument index (while the
		 format ones don't).  For example, you could get
		 "warning: null argument where non-null required
		 (argument 3)".  Now in that message, "argument 3"
		 includes the 2 hidden arguments; it would be much
		 more friendly to call it "argument 1", as that would
		 be consistent with __attribute__ ((nonnnull (1))).
		 To do this, we'd need to have the C family code that
		 checks the arguments know about adding/removing 2 to
		 the argument index ... or alternatively we could
		 maybe store the "printable" argument index in
		 addition to the actual argument index ?  Some
		 refactoring is needed to do this elegantly.  */
	      tree new_attribute = copy_node (attribute);
	      tree argument = TREE_VALUE (attribute);
	      while (argument != NULL_TREE)
		{
		  /* Get the value of the argument and add 2.  */
		  tree number = TREE_VALUE (argument);
		  if (number && TREE_CODE (number) == INTEGER_CST
		      && !wi::eq_p (number, 0))
		    TREE_VALUE (argument)
		      = wide_int_to_tree (TREE_TYPE (number),
					  wi::add (number, 2));
		  argument = TREE_CHAIN (argument);
		}

	      filtered_attributes = chainon (filtered_attributes,
					     new_attribute);
	    }
	  else
	    warning (OPT_Wattributes, "%qE attribute directive ignored", name);
	}
    }

  if (filtered_attributes)
    {
      /* This hackery changes the TREE_TYPE of the ObjC method
	 declaration to be a function type, so that decl_attributes
	 will treat the ObjC method as if it was a function.  Some
	 attributes (sentinel, format) will be applied to the function
	 type, changing it in place; so after calling decl_attributes,
	 we extract the function type attributes and store them in
	 METHOD_TYPE_ATTRIBUTES.  Some other attributes (noreturn,
	 deprecated) are applied directly to the method declaration
	 (by setting TREE_DEPRECATED and TREE_THIS_VOLATILE) so there
	 is nothing to do.  */
      tree saved_type = TREE_TYPE (*node);
      TREE_TYPE (*node)
	= build_function_type_for_method (TREE_VALUE (saved_type), *node,
					  METHOD_REF, 0);
      decl_attributes (node, filtered_attributes, flags);
      METHOD_TYPE_ATTRIBUTES (*node) = TYPE_ATTRIBUTES (TREE_TYPE (*node));
      TREE_TYPE (*node) = saved_type;
    }
}

bool
objc_method_decl (enum tree_code opcode)
{
  return opcode == INSTANCE_METHOD_DECL || opcode == CLASS_METHOD_DECL;
}

/* Return a function type for METHOD with RETURN_TYPE.  CONTEXT is
   either METHOD_DEF or METHOD_REF, indicating whether we are defining a
   method or calling one.  SUPER_FLAG indicates whether this is a send
   to super; this makes a difference for the NeXT calling sequence in
   which the lookup and the method call are done together.  If METHOD is
   NULL, user-defined arguments (i.e., beyond self and _cmd) shall be
   represented as varargs.  */

tree
build_function_type_for_method (tree return_type, tree method,
				int context, bool super_flag)
{
  vec<tree, va_gc> *argtypes = make_tree_vector ();
  tree t, ftype;
  bool is_varargs = false;

  (*runtime.get_arg_type_list_base) (&argtypes, method, context, super_flag);

  /* No actual method prototype given; remaining args passed as varargs.  */
  if (method == NULL_TREE)
    {
      is_varargs = true;
      goto build_ftype;
    }

  for (t = METHOD_SEL_ARGS (method); t; t = DECL_CHAIN (t))
    {
      tree arg_type = TREE_VALUE (TREE_TYPE (t));

      /* Decay argument types for the underlying C function as
         appropriate.  */
      arg_type = objc_decay_parm_type (arg_type);

      vec_safe_push (argtypes, arg_type);
    }

  if (METHOD_ADD_ARGS (method))
    {
      for (t = TREE_CHAIN (METHOD_ADD_ARGS (method));
	   t; t = TREE_CHAIN (t))
	{
	  tree arg_type = TREE_TYPE (TREE_VALUE (t));

	  arg_type = objc_decay_parm_type (arg_type);

	  vec_safe_push (argtypes, arg_type);
	}

      if (METHOD_ADD_ARGS_ELLIPSIS_P (method))
	is_varargs = true;
    }

 build_ftype:
  if (is_varargs)
    ftype = build_varargs_function_type_vec (return_type, argtypes);
  else
    ftype = build_function_type_vec (return_type, argtypes);

  release_tree_vector (argtypes);
  return ftype;
}

/* The 'method' argument is a tree; this tree could either be a single
   method, which is returned, or could be a TREE_VEC containing a list
   of methods.  In that case, the first one is returned, and warnings
   are issued as appropriate.  */
static tree
check_duplicates (tree method, int methods, int is_class)
{
  tree first_method;
  size_t i;

  if (method == NULL_TREE)
    return NULL_TREE;

  if (TREE_CODE (method) != TREE_VEC)
    return method;

  /* We have two or more methods with the same name but different
     types.  */
  first_method = TREE_VEC_ELT (method, 0);
  
  /* But just how different are those types?  If
     -Wno-strict-selector-match is specified, we shall not complain if
     the differences are solely among types with identical size and
     alignment.  */
  if (!warn_strict_selector_match)
    {
      for (i = 0; i < (size_t) TREE_VEC_LENGTH (method); i++)
	if (!comp_proto_with_proto (first_method, TREE_VEC_ELT (method, i), 0))
	  goto issue_warning;
      
      return first_method;
    }
    
 issue_warning:
  if (methods)
    {
      bool type = TREE_CODE (first_method) == INSTANCE_METHOD_DECL;
      
      warning_at (input_location, 0,
		  "multiple methods named %<%c%E%> found",
		  (is_class ? '+' : '-'),
		  METHOD_SEL_NAME (first_method));
      inform (DECL_SOURCE_LOCATION (first_method), "using %<%c%s%>",
	      (type ? '-' : '+'),
	      identifier_to_locale (gen_method_decl (first_method)));
    }
  else
    {
      bool type = TREE_CODE (first_method) == INSTANCE_METHOD_DECL;
      
      warning_at (input_location, 0,
		  "multiple selectors named %<%c%E%> found",
		  (is_class ? '+' : '-'),
		  METHOD_SEL_NAME (first_method));
      inform (DECL_SOURCE_LOCATION (first_method), "found %<%c%s%>",
	      (type ? '-' : '+'),
	      identifier_to_locale (gen_method_decl (first_method)));
    }
  
  for (i = 0; i < (size_t) TREE_VEC_LENGTH (method); i++)
    {
      bool type = TREE_CODE (TREE_VEC_ELT (method, i)) == INSTANCE_METHOD_DECL;
      
      inform (DECL_SOURCE_LOCATION (TREE_VEC_ELT (method, i)), "also found %<%c%s%>",
	      (type ? '-' : '+'),
	      identifier_to_locale (gen_method_decl (TREE_VEC_ELT (method, i))));
    }

  return first_method;
}

/* If RECEIVER is a class reference, return the identifier node for
   the referenced class.  RECEIVER is created by objc_get_class_reference,
   so we check the exact form created depending on which runtimes are
   used.  */

static tree
receiver_is_class_object (tree receiver, int self, int super)
{
  tree exp, arg;

  /* The receiver is 'self' or 'super' in the context of a class method.  */
  if (objc_method_context
      && TREE_CODE (objc_method_context) == CLASS_METHOD_DECL
      && (self || super))
    return (super
	    ? CLASS_SUPER_NAME (implementation_template)
	    : CLASS_NAME (implementation_template));

  /* The runtime might encapsulate things its own way.  */
  exp = (*runtime.receiver_is_class_object) (receiver);
  if (exp)
    return exp;

  /* The receiver is a function call that returns an id.  Check if
     it is a call to objc_getClass, if so, pick up the class name.

     This is required by the GNU runtime, which compiles

       [NSObject alloc]

     into

       [objc_get_class ("NSObject") alloc];

     and then, to check that the receiver responds to the +alloc
     method, needs to be able to determine that the objc_get_class()
     call returns the NSObject class and not just a generic Class
     pointer.

     But, traditionally this is enabled for all runtimes, not just the
     GNU one, which means that the compiler is smarter than you'd
     expect when dealing with objc_getClass().  For example, with the
     Apple runtime, in the code

       [objc_getClass ("NSObject")  alloc];

     the compiler will recognize the objc_getClass() call as special
     (due to the code below) and so will know that +alloc is called on
     the 'NSObject' class, and can perform the corresponding checks.

     Programmers can disable this behaviour by casting the results of
     objc_getClass() to 'Class' (this may seem weird because
     objc_getClass() is already declared to return 'Class', but the
     compiler treats it as a special function).  This may be useful if
     the class is never declared, and the compiler would complain
     about a missing @interface for it.  Then, you can do

       [(Class)objc_getClass ("MyClassNeverDeclared")  alloc];

     to silence the warnings.  */
  if (TREE_CODE (receiver) == CALL_EXPR
      && (exp = CALL_EXPR_FN (receiver))
      && TREE_CODE (exp) == ADDR_EXPR
      && (exp = TREE_OPERAND (exp, 0))
      && TREE_CODE (exp) == FUNCTION_DECL
      /* For some reason, we sometimes wind up with multiple FUNCTION_DECL
	 prototypes for objc_get_class().  Thankfully, they seem to share the
	 same function type.  */
      && TREE_TYPE (exp) == TREE_TYPE (objc_get_class_decl)
      && !strcmp (IDENTIFIER_POINTER (DECL_NAME (exp)), runtime.tag_getclass)
      /* We have a call to objc_get_class/objc_getClass!  */
      && (arg = CALL_EXPR_ARG (receiver, 0)))
    {
      STRIP_NOPS (arg);
      if (TREE_CODE (arg) == ADDR_EXPR
	  && (arg = TREE_OPERAND (arg, 0))
	  && TREE_CODE (arg) == STRING_CST)
	/* Finally, we have the class name.  */
	return get_identifier (TREE_STRING_POINTER (arg));
    }
  return 0;
}

/* If we are currently building a message expr, this holds
   the identifier of the selector of the message.  This is
   used when printing warnings about argument mismatches.  */

static tree current_objc_message_selector = 0;

tree
objc_message_selector (void)
{
  return current_objc_message_selector;
}

/* Construct an expression for sending a message.
   MESS has the object to send to in TREE_PURPOSE
   and the argument list (including selector) in TREE_VALUE.

   (*(<abstract_decl>(*)())_msg)(receiver, selTransTbl[n], ...);
   (*(<abstract_decl>(*)())_msgSuper)(receiver, selTransTbl[n], ...);  */

tree
objc_build_message_expr (tree receiver, tree message_args)
{
  tree sel_name;
#ifdef OBJCPLUS
  tree args = TREE_PURPOSE (message_args);
#else
  tree args = message_args;
#endif
  tree method_params = NULL_TREE;

  if (TREE_CODE (receiver) == ERROR_MARK || TREE_CODE (args) == ERROR_MARK)
    return error_mark_node;

  /* Obtain the full selector name.  */
  switch (TREE_CODE (args))
    {
    case IDENTIFIER_NODE:
      /* A unary selector.  */
      sel_name = args;
      break;
    case TREE_LIST:
      sel_name = build_keyword_selector (args);
      break;
    default:
      gcc_unreachable ();
    }

  /* Build the parameter list to give to the method.  */
  if (TREE_CODE (args) == TREE_LIST)
#ifdef OBJCPLUS
    method_params = chainon (args, TREE_VALUE (message_args));
#else
    {
      tree chain = args, prev = NULL_TREE;

      /* We have a keyword selector--check for comma expressions.  */
      while (chain)
	{
	  tree element = TREE_VALUE (chain);

	  /* We have a comma expression, must collapse...  */
	  if (TREE_CODE (element) == TREE_LIST)
	    {
	      if (prev)
		TREE_CHAIN (prev) = element;
	      else
		args = element;
	    }
	  prev = chain;
	  chain = TREE_CHAIN (chain);
        }
      method_params = args;
    }
#endif

#ifdef OBJCPLUS
  if (processing_template_decl)
    /* Must wait until template instantiation time.  */
    return build_min_nt_loc (UNKNOWN_LOCATION, MESSAGE_SEND_EXPR, receiver,
			     sel_name, method_params);
#endif

  return objc_finish_message_expr (receiver, sel_name, method_params, NULL);
}

/* Look up method SEL_NAME that would be suitable for receiver
   of type 'id' (if IS_CLASS is zero) or 'Class' (if IS_CLASS is
   nonzero), and report on any duplicates.  */

static tree
lookup_method_in_hash_lists (tree sel_name, int is_class)
{
  tree method_prototype = OBJC_MAP_NOT_FOUND;

  if (!is_class)
    method_prototype = objc_map_get (instance_method_map, sel_name);
  
  if (method_prototype == OBJC_MAP_NOT_FOUND)
    {
      method_prototype = objc_map_get (class_method_map, sel_name);
      is_class = 1;

      if (method_prototype == OBJC_MAP_NOT_FOUND)
	return NULL_TREE;
    }

  return check_duplicates (method_prototype, 1, is_class);
}

/* The 'objc_finish_message_expr' routine is called from within
   'objc_build_message_expr' for non-template functions.  In the case of
   C++ template functions, it is called from 'build_expr_from_tree'
   (in decl2.c) after RECEIVER and METHOD_PARAMS have been expanded.

   If the DEPRECATED_METHOD_PROTOTYPE argument is NULL, then we warn
   if the method being used is deprecated.  If it is not NULL, instead
   of deprecating, we set *DEPRECATED_METHOD_PROTOTYPE to the method
   prototype that was used and is deprecated.  This is useful for
   getter calls that are always generated when compiling dot-syntax
   expressions, even if they may not be used.  In that case, we don't
   want the warning immediately; we produce it (if needed) at gimplify
   stage when we are sure that the deprecated getter is being
   used.  */
tree
objc_finish_message_expr (tree receiver, tree sel_name, tree method_params,
			  tree *deprecated_method_prototype)
{
  tree method_prototype = NULL_TREE, rprotos = NULL_TREE, rtype;
  tree retval, class_tree;
  int self, super, have_cast;

  /* We have used the receiver, so mark it as read.  */
  mark_exp_read (receiver);

  /* Extract the receiver of the message, as well as its type
     (where the latter may take the form of a cast or be inferred
     from the implementation context).  */
  rtype = receiver;
  while (TREE_CODE (rtype) == COMPOUND_EXPR
	 || TREE_CODE (rtype) == MODIFY_EXPR
	 || CONVERT_EXPR_P (rtype)
	 || TREE_CODE (rtype) == COMPONENT_REF)
    rtype = TREE_OPERAND (rtype, 0);

  /* self is 1 if this is a message to self, 0 otherwise  */
  self = (rtype == self_decl);

  /* super is 1 if this is a message to super, 0 otherwise.  */
  super = (rtype == UOBJC_SUPER_decl);

  /* rtype is the type of the receiver.  */
  rtype = TREE_TYPE (receiver);

  /* have_cast is 1 if the receiver is casted.  */
  have_cast = (TREE_CODE (receiver) == NOP_EXPR
	       || (TREE_CODE (receiver) == COMPOUND_EXPR
		   && !IS_SUPER (rtype)));

  /* If we are calling [super dealloc], reset our warning flag.  */
  if (super && !strcmp ("dealloc", IDENTIFIER_POINTER (sel_name)))
    should_call_super_dealloc = 0;

  /* If the receiver is a class object, retrieve the corresponding
     @interface, if one exists.  class_tree is the class name
     identifier, or NULL_TREE if this is not a class method or the
     class name could not be determined (as in the case "Class c; [c
     method];").  */
  class_tree = receiver_is_class_object (receiver, self, super);

  /* Now determine the receiver type (if an explicit cast has not been
     provided).  */
  if (!have_cast)
    {
      if (class_tree)
	{
	  /* We are here when we have no cast, and we have a class
	     name.  So, this is a plain method to a class object, as
	     in [NSObject alloc].  Find the interface corresponding to
	     the class name.  */
	  rtype = lookup_interface (class_tree);

	  if (rtype == NULL_TREE)
	    {
	      /* If 'rtype' is NULL_TREE at this point it means that
		 we have seen no @interface corresponding to that
		 class name, only a @class declaration (alternatively,
		 this was a call such as [objc_getClass("SomeClass")
		 alloc], where we've never seen the @interface of
		 SomeClass).  So, we have a class name (class_tree)
		 but no actual details of the class methods.  We won't
		 be able to check that the class responds to the
		 method, and we will have to guess the method
		 prototype.  Emit a warning, then keep going (this
		 will use any method with a matching name, as if the
		 receiver was of type 'Class').  */
	      warning (0, "@interface of class %qE not found", class_tree);
	    }
	}
      /* Handle `self' and `super'.  */
      else if (super)
	{
	  if (!CLASS_SUPER_NAME (implementation_template))
	    {
	      error ("no super class declared in @interface for %qE",
		     CLASS_NAME (implementation_template));
	      return error_mark_node;
	    }
	  rtype = lookup_interface (CLASS_SUPER_NAME (implementation_template));
	}
      else if (self)
	rtype = lookup_interface (CLASS_NAME (implementation_template));
    }

  if (objc_is_id (rtype))
    {
      /* The receiver is of type 'id' or 'Class' (with or without some
	 protocols attached to it).  */

      /* We set class_tree to the identifier for 'Class' if this is a
	 class method, and to NULL_TREE if not.  */
      class_tree = (IS_CLASS (rtype) ? objc_class_name : NULL_TREE);
      
      /* 'rprotos' is the list of protocols that the receiver
	 supports.  */
      rprotos = (TYPE_HAS_OBJC_INFO (TREE_TYPE (rtype))
		 ? TYPE_OBJC_PROTOCOL_LIST (TREE_TYPE (rtype))
		 : NULL_TREE);

      /* We have no information on the type, and we set it to
	 NULL_TREE.  */
      rtype = NULL_TREE;

      /* If there are any protocols, check that the method we are
	 calling appears in the protocol list.  If there are no
	 protocols, this is a message to 'id' or 'Class' and we accept
	 any method that exists.  */
      if (rprotos)
	{
	  /* If messaging 'id <Protos>' or 'Class <Proto>', first
	     search in protocols themselves for the method
	     prototype.  */
	  method_prototype
	    = lookup_method_in_protocol_list (rprotos, sel_name,
					      class_tree != NULL_TREE);

	  /* If messaging 'Class <Proto>' but did not find a class
	     method prototype, search for an instance method instead,
	     and warn about having done so.  */
	  if (!method_prototype && !rtype && class_tree != NULL_TREE)
	    {
	      method_prototype
		= lookup_method_in_protocol_list (rprotos, sel_name, 0);

	      if (method_prototype)
		warning (0, "found %<-%E%> instead of %<+%E%> in protocol(s)",
			 sel_name, sel_name);
	    }
	}
    }
  else if (rtype)
    {
      /* We have a receiver type which is more specific than 'id' or
	 'Class'.  */
      tree orig_rtype = rtype;

      if (TREE_CODE (rtype) == POINTER_TYPE)
	rtype = TREE_TYPE (rtype);
      /* Traverse typedef aliases */
      while (TREE_CODE (rtype) == RECORD_TYPE && OBJC_TYPE_NAME (rtype)
	     && TREE_CODE (OBJC_TYPE_NAME (rtype)) == TYPE_DECL
	     && DECL_ORIGINAL_TYPE (OBJC_TYPE_NAME (rtype)))
	rtype = DECL_ORIGINAL_TYPE (OBJC_TYPE_NAME (rtype));
      if (TYPED_OBJECT (rtype))
	{
	  rprotos = TYPE_OBJC_PROTOCOL_LIST (rtype);
	  rtype = TYPE_OBJC_INTERFACE (rtype);
	}
      if (!rtype || TREE_CODE (rtype) == IDENTIFIER_NODE)
	{
	  /* If we could not find an @interface declaration, we must
	     have only seen a @class declaration; so, we cannot say
	     anything more intelligent about which methods the
	     receiver will understand.  Note that this only happens
	     for instance methods; for class methods to a class where
	     we have only seen a @class declaration,
	     lookup_interface() above would have set rtype to
	     NULL_TREE.  */
	  if (rprotos)
	    {
	      /* We could not find an @interface declaration, yet, if
		 there are protocols attached to the type, we can
		 still look up the method in the protocols.  Ie, we
		 are in the following case:
	     
		 @class MyClass;
		 MyClass<MyProtocol> *x;
		 [x method];
		 
		 If 'MyProtocol' has the method 'method', we can check
		 and retrieve the method prototype.  */
	      method_prototype
		= lookup_method_in_protocol_list (rprotos, sel_name, 0);

	      /* At this point, if we have found the method_prototype,
		 we are quite happy.  The details of the class are
		 irrelevant.  If we haven't found it, a warning will
		 have been produced that the method could not be found
		 in the protocol, and we won't produce further
		 warnings (please note that this means that "@class
		 MyClass; MyClass <MyProtocol> *x;" is exactly
		 equivalent to "id <MyProtocol> x", which isn't too
		 satisfactory but it's not easy to see how to do
		 better).  */
	    }
	  else
	    {
	      if (rtype)
		{
		  /* We could not find an @interface declaration, and
		     there are no protocols attached to the receiver,
		     so we can't complete the check that the receiver
		     responds to the method, and we can't retrieve the
		     method prototype.  But, because the receiver has
		     a well-specified class, the programmer did want
		     this check to be performed.  Emit a warning, then
		     keep going as if it was an 'id'.  To remove the
		     warning, either include an @interface for the
		     class, or cast the receiver to 'id'.  Note that
		     rtype is an IDENTIFIER_NODE at this point.  */
		  warning (0, "@interface of class %qE not found", rtype);
		}
	    }

	  rtype = NULL_TREE;
	}
      else if (TREE_CODE (rtype) == CLASS_INTERFACE_TYPE
	  || TREE_CODE (rtype) == CLASS_IMPLEMENTATION_TYPE)
	{
	  /* We have a valid ObjC class name with an associated
	     @interface.  Look up the method name in the published
	     @interface for the class (and its superclasses).  */
	  method_prototype
	    = lookup_method_static (rtype, sel_name, class_tree != NULL_TREE);

	  /* If the method was not found in the @interface, it may still
	     exist locally as part of the @implementation.  */
	  if (!method_prototype && objc_implementation_context
	     && CLASS_NAME (objc_implementation_context)
		== OBJC_TYPE_NAME (rtype))
	    method_prototype
	      = lookup_method
		((class_tree
		  ? CLASS_CLS_METHODS (objc_implementation_context)
		  : CLASS_NST_METHODS (objc_implementation_context)),
		  sel_name);

	  /* If we haven't found a candidate method by now, try looking for
	     it in the protocol list.  */
	  if (!method_prototype && rprotos)
	    method_prototype
	      = lookup_method_in_protocol_list (rprotos, sel_name,
						class_tree != NULL_TREE);
	}
      else
	{
	  /* We have a type, but it's not an Objective-C type (!).  */
	  warning (0, "invalid receiver type %qs",
		   identifier_to_locale (gen_type_name (orig_rtype)));
	  /* After issuing the "invalid receiver" warning, perform method
	     lookup as if we were messaging 'id'.  */
	  rtype = rprotos = NULL_TREE;
	}
    }
  /* Note that rtype could also be NULL_TREE.  This happens if we are
     messaging a class by name, but the class was only
     forward-declared using @class.  */

  /* For 'id' or 'Class' receivers, search in the global hash table as
     a last resort.  For all receivers, warn if protocol searches have
     failed.  */
  if (!method_prototype)
    {
      if (rprotos)
	warning (0, "%<%c%E%> not found in protocol(s)",
		 (class_tree ? '+' : '-'),
		 sel_name);

      if (!rtype)
	method_prototype
	  = lookup_method_in_hash_lists (sel_name, class_tree != NULL_TREE);
    }

  if (!method_prototype)
    {
      static bool warn_missing_methods = false;

      if (rtype)
	warning (0, "%qE may not respond to %<%c%E%>",
		 OBJC_TYPE_NAME (rtype),
		 (class_tree ? '+' : '-'),
		 sel_name);
      /* If we are messaging an 'id' or 'Class' object and made it here,
	 then we have failed to find _any_ instance or class method,
	 respectively.  */
      else
	warning (0, "no %<%c%E%> method found",
		 (class_tree ? '+' : '-'),
		 sel_name);

      if (!warn_missing_methods)
	{
	  warning_at (input_location,
		      0, "(Messages without a matching method signature");
	  warning_at (input_location,
		      0, "will be assumed to return %<id%> and accept");
	  warning_at (input_location,
		      0, "%<...%> as arguments.)");
	  warn_missing_methods = true;
	}
    }
  else
    {
      /* Warn if the method is deprecated, but not if the receiver is
	 a generic 'id'.  'id' is used to cast an object to a generic
	 object of an unspecified class; in that case, we'll use
	 whatever method prototype we can find to get the method
	 argument and return types, but it is not appropriate to
	 produce deprecation warnings since we don't know the class
	 that the object will be of at runtime.  The @interface(s) for
	 that class may not even be available to the compiler right
	 now, and it is perfectly possible that the method is marked
	 as non-deprecated in such @interface(s).

	 In practice this makes sense since casting an object to 'id'
	 is often used precisely to turn off warnings associated with
	 the object being of a particular class.  */
      if (TREE_DEPRECATED (method_prototype) && rtype != NULL_TREE)
	{
	  if (deprecated_method_prototype)
	    *deprecated_method_prototype = method_prototype;
	  else
	    warn_deprecated_use (method_prototype, NULL_TREE);
	}
    }

  /* Save the selector name for printing error messages.  */
  current_objc_message_selector = sel_name;

  /* Build the method call.
     TODO: Get the location from somewhere that will work for delayed
	   expansion.  */

  retval = (*runtime.build_objc_method_call) (input_location, method_prototype,
					      receiver, rtype, sel_name,
					      method_params, super);

  current_objc_message_selector = 0;

  return retval;
}


/* This routine creates a static variable used to implement @protocol(MyProtocol)
   expression. This variable will be initialized to global protocol_t meta-data
   pointer. */

/* This function is called by the parser when (and only when) a
   @protocol() expression is found, in order to compile it.  */
tree
objc_build_protocol_expr (tree protoname)
{
  tree p = lookup_protocol (protoname, /* warn if deprecated */ true,
			    /* definition_required */ false);

  if (!p)
    {
      error ("cannot find protocol declaration for %qE", protoname);
      return error_mark_node;
    }

  return (*runtime.get_protocol_reference) (input_location, p);
}

/* This function is called by the parser when a @selector() expression
   is found, in order to compile it.  It is only called by the parser
   and only to compile a @selector().  LOC is the location of the
   @selector.  */
tree
objc_build_selector_expr (location_t loc, tree selnamelist)
{
  tree selname;

  /* Obtain the full selector name.  */
  switch (TREE_CODE (selnamelist))
    {
    case IDENTIFIER_NODE:
      /* A unary selector.  */
      selname = selnamelist;
      break;
    case TREE_LIST:
      selname = build_keyword_selector (selnamelist);
      break;
    default:
      gcc_unreachable ();
    }

  /* If we are required to check @selector() expressions as they
     are found, check that the selector has been declared.  */
  if (warn_undeclared_selector)
    {
      /* Look the selector up in the list of all known class and
         instance methods (up to this line) to check that the selector
         exists.  */
      tree method;

      /* First try with instance methods.  */
      method = objc_map_get (instance_method_map, selname);

      /* If not found, try with class methods.  */
      if (method == OBJC_MAP_NOT_FOUND)
	{
	  method = objc_map_get (class_method_map, selname);

	  /* If still not found, print out a warning.  */
	  if (method == OBJC_MAP_NOT_FOUND)
	    warning (0, "undeclared selector %qE", selname);
	}
    }

  /* The runtimes do this differently, most particularly, GNU has typed
     selectors, whilst NeXT does not.  */
  return (*runtime.build_selector_reference) (loc, selname, NULL_TREE);
}

static tree
build_ivar_reference (tree id)
{
  tree base;
  if (TREE_CODE (objc_method_context) == CLASS_METHOD_DECL)
    {
      /* Historically, a class method that produced objects (factory
	 method) would assign `self' to the instance that it
	 allocated.  This would effectively turn the class method into
	 an instance method.  Following this assignment, the instance
	 variables could be accessed.  That practice, while safe,
	 violates the simple rule that a class method should not refer
	 to an instance variable.  It's better to catch the cases
	 where this is done unknowingly than to support the above
	 paradigm.  */
      warning (0, "instance variable %qE accessed in class method",
	       id);
      self_decl = convert (objc_instance_type, self_decl); /* cast */
    }

  base = build_indirect_ref (input_location, self_decl, RO_ARROW);
  return (*runtime.build_ivar_reference) (input_location, base, id);
}

static void
hash_init (void)
{
  instance_method_map = objc_map_alloc_ggc (1000);
  class_method_map = objc_map_alloc_ggc (1000);

  class_name_map = objc_map_alloc_ggc (200);
  alias_name_map = objc_map_alloc_ggc (200);

  /* Initialize the hash table used to hold the constant string objects.  */
  string_htab = htab_create_ggc (31, string_hash,
				   string_eq, NULL);
}

/* Use the following to add a method to class_method_map or
   instance_method_map.  It will add the method, keyed by the
   METHOD_SEL_NAME.  If the method already exists, but with one or
   more different prototypes, it will store a TREE_VEC in the map,
   with the method prototypes in the vector.  */
static void
insert_method_into_method_map (bool class_method, tree method)
{
  tree method_name = METHOD_SEL_NAME (method);
  tree existing_entry;
  objc_map_t map;

  if (class_method)
    map = class_method_map;
  else
    map = instance_method_map;

  /* Check if the method already exists in the map.  */
  existing_entry = objc_map_get (map, method_name);

  /* If not, we simply add it to the map.  */
  if (existing_entry == OBJC_MAP_NOT_FOUND)
    objc_map_put (map, method_name, method);
  else
    {
      tree new_entry;
      
      /* If an entry already exists, it's more complicated.  We'll
	 have to check whether the method prototype is the same or
	 not.  */
      if (TREE_CODE (existing_entry) != TREE_VEC)
	{
	  /* If the method prototypes are the same, there is nothing
	     to do.  */
	  if (comp_proto_with_proto (method, existing_entry, 1))
	    return;

	  /* If not, create a vector to store both the method already
	     in the map, and the new one that we are adding.  */
	  new_entry = make_tree_vec (2);
	  
	  TREE_VEC_ELT (new_entry, 0) = existing_entry;
	  TREE_VEC_ELT (new_entry, 1) = method;
	}
      else
	{
	  /* An entry already exists, and it's already a vector.  This
	     means that at least 2 different method prototypes were
	     already found, and we're considering registering yet
	     another one.  */
	  size_t i;

	  /* Check all the existing prototypes.  If any matches the
	     one we need to add, there is nothing to do because it's
	     already there.  */
	  for (i = 0; i < (size_t) TREE_VEC_LENGTH (existing_entry); i++)
	    if (comp_proto_with_proto (method, TREE_VEC_ELT (existing_entry, i), 1))
	      return;

	  /* Else, create a new, bigger vector and add the new method
	     at the end of it.  This is inefficient but extremely
	     rare; in any sane program most methods have a single
	     prototype, and very few, if any, will have more than
	     2!  */
	  new_entry = make_tree_vec (TREE_VEC_LENGTH (existing_entry) + 1);
	  
	  /* Copy the methods from the existing vector.  */
	  for (i = 0; i < (size_t) TREE_VEC_LENGTH (existing_entry); i++)
	    TREE_VEC_ELT (new_entry, i) = TREE_VEC_ELT (existing_entry, i);
	  
	  /* Add the new method at the end.  */
	  TREE_VEC_ELT (new_entry, i) = method;
	}

      /* Store the new vector in the map.  */
      objc_map_put (map, method_name, new_entry);
    }
}


static tree
lookup_method (tree mchain, tree method)
{
  tree key;

  if (TREE_CODE (method) == IDENTIFIER_NODE)
    key = method;
  else
    key = METHOD_SEL_NAME (method);

  while (mchain)
    {
      if (METHOD_SEL_NAME (mchain) == key)
	return mchain;

      mchain = DECL_CHAIN (mchain);
    }
  return NULL_TREE;
}

/* Look up a class (if OBJC_LOOKUP_CLASS is set in FLAGS) or instance
   method in INTERFACE, along with any categories and protocols
   attached thereto.  If method is not found, and the
   OBJC_LOOKUP_NO_SUPER is _not_ set in FLAGS, recursively examine the
   INTERFACE's superclass.  If OBJC_LOOKUP_CLASS is set,
   OBJC_LOOKUP_NO_SUPER is clear, and no suitable class method could
   be found in INTERFACE or any of its superclasses, look for an
   _instance_ method of the same name in the root class as a last
   resort.  This behaviour can be turned off by using
   OBJC_LOOKUP_NO_INSTANCE_METHODS_OF_ROOT_CLASS.

   If a suitable method cannot be found, return NULL_TREE.  */

static tree
lookup_method_static (tree interface, tree ident, int flags)
{
  tree meth = NULL_TREE, root_inter = NULL_TREE;
  tree inter = interface;
  int is_class = (flags & OBJC_LOOKUP_CLASS);
  int no_superclasses = (flags & OBJC_LOOKUP_NO_SUPER);
  int no_instance_methods_of_root_class = (flags & OBJC_LOOKUP_NO_INSTANCE_METHODS_OF_ROOT_CLASS);

  while (inter)
    {
      tree chain = is_class ? CLASS_CLS_METHODS (inter) : CLASS_NST_METHODS (inter);
      tree category = inter;

      /* First, look up the method in the class itself.  */
      if ((meth = lookup_method (chain, ident)))
	return meth;

      /* Failing that, look for the method in each category of the class.  */
      while ((category = CLASS_CATEGORY_LIST (category)))
	{
	  chain = is_class ? CLASS_CLS_METHODS (category) : CLASS_NST_METHODS (category);

	  /* Check directly in each category.  */
	  if ((meth = lookup_method (chain, ident)))
	    return meth;

	  /* Failing that, check in each category's protocols.  */
	  if (CLASS_PROTOCOL_LIST (category))
	    {
	      if ((meth = (lookup_method_in_protocol_list
			   (CLASS_PROTOCOL_LIST (category), ident, is_class))))
		return meth;
	    }
	}

      /* If not found in categories, check in protocols of the main class.  */
      if (CLASS_PROTOCOL_LIST (inter))
	{
	  if ((meth = (lookup_method_in_protocol_list
		       (CLASS_PROTOCOL_LIST (inter), ident, is_class))))
	    return meth;
	}

      /* If we were instructed not to look in superclasses, don't.  */
      if (no_superclasses)
	return NULL_TREE;

      /* Failing that, climb up the inheritance hierarchy.  */
      root_inter = inter;
      inter = lookup_interface (CLASS_SUPER_NAME (inter));
    }
  while (inter);

  if (is_class && !no_instance_methods_of_root_class)
    {
      /* If no class (factory) method was found, check if an _instance_
	 method of the same name exists in the root class.  This is what
	 the Objective-C runtime will do.  */
      return lookup_method_static (root_inter, ident, 0);
    }
  else
    {
      /* If an instance method was not found, return 0.  */
      return NULL_TREE;
    }
}

static tree
objc_add_method (tree klass, tree method, int is_class, bool is_optional)
{
  tree existing_method = NULL_TREE;

  /* The first thing we do is look up the method in the list of
     methods already defined in the interface (or implementation).  */
  if (is_class)
    existing_method = lookup_method (CLASS_CLS_METHODS (klass), method);
  else
    existing_method = lookup_method (CLASS_NST_METHODS (klass), method);

  /* In the case of protocols, we have a second list of methods to
     consider, the list of optional ones.  */
  if (TREE_CODE (klass) == PROTOCOL_INTERFACE_TYPE)
    {
      /* @required methods are added to the protocol's normal list.
	 @optional methods are added to the protocol's OPTIONAL lists.
	 Note that adding the methods to the optional lists disables
	 checking that the methods are implemented by classes
	 implementing the protocol, since these checks only use the
	 CLASS_CLS_METHODS and CLASS_NST_METHODS.  */

      /* First of all, if the method to add is @optional, and we found
	 it already existing as @required, emit an error.  */
      if (is_optional && existing_method)
	{
	  error ("method %<%c%E%> declared %<@optional%> and %<@required%> at the same time",
		 (is_class ? '+' : '-'),
		 METHOD_SEL_NAME (existing_method));
	  inform (DECL_SOURCE_LOCATION (existing_method),
		  "previous declaration of %<%c%E%> as %<@required%>",
		  (is_class ? '+' : '-'),
		  METHOD_SEL_NAME (existing_method));
	}

      /* Now check the list of @optional methods if we didn't find the
	 method in the @required list.  */
      if (!existing_method)
	{
	  if (is_class)
	    existing_method = lookup_method (PROTOCOL_OPTIONAL_CLS_METHODS (klass), method);
	  else
	    existing_method = lookup_method (PROTOCOL_OPTIONAL_NST_METHODS (klass), method);

	  if (!is_optional && existing_method)
	    {
	      error ("method %<%c%E%> declared %<@optional%> and %<@required%> at the same time",
		     (is_class ? '+' : '-'),
		     METHOD_SEL_NAME (existing_method));
	      inform (DECL_SOURCE_LOCATION (existing_method),
		      "previous declaration of %<%c%E%> as %<@optional%>",
		      (is_class ? '+' : '-'),
		      METHOD_SEL_NAME (existing_method));
	    }
	}
    }

  /* If the method didn't exist already, add it.  */
  if (!existing_method)
    {
      if (is_optional)
	{
	  if (is_class)
	    {
	      /* Put the method on the list in reverse order.  */
	      TREE_CHAIN (method) = PROTOCOL_OPTIONAL_CLS_METHODS (klass);
	      PROTOCOL_OPTIONAL_CLS_METHODS (klass) = method;
	    }
	  else
	    {
	      TREE_CHAIN (method) = PROTOCOL_OPTIONAL_NST_METHODS (klass);
	      PROTOCOL_OPTIONAL_NST_METHODS (klass) = method;
	    }
	}
      else
	{
	  if (is_class)
	    {
	      DECL_CHAIN (method) = CLASS_CLS_METHODS (klass);
	      CLASS_CLS_METHODS (klass) = method;
	    }
	  else
	    {
	      DECL_CHAIN (method) = CLASS_NST_METHODS (klass);
	      CLASS_NST_METHODS (klass) = method;
	    }
	}
    }
  else
    {
      /* The method was already defined.  Check that the types match
	 for an @interface for a class or category, or for a
	 @protocol.  Give hard errors on methods with identical
	 selectors but differing argument and/or return types.  We do
	 not do this for @implementations, because C/C++ will do it
	 for us (i.e., there will be duplicate function definition
	 errors).  */
      if ((TREE_CODE (klass) == CLASS_INTERFACE_TYPE
	   || TREE_CODE (klass) == CATEGORY_INTERFACE_TYPE
	   /* Starting with GCC 4.6, we emit the same error for
	      protocols too.  The situation is identical to
	      @interfaces as there is no possible meaningful reason
	      for defining the same method with different signatures
	      in the very same @protocol.  If that was allowed,
	      whenever the protocol is used (both at compile and run
	      time) there wouldn't be any meaningful way to decide
	      which of the two method signatures should be used.  */
	   || TREE_CODE (klass) == PROTOCOL_INTERFACE_TYPE)
	  && !comp_proto_with_proto (method, existing_method, 1))
	{
	  error ("duplicate declaration of method %<%c%E%> with conflicting types",
		 (is_class ? '+' : '-'),
		 METHOD_SEL_NAME (existing_method));
	  inform (DECL_SOURCE_LOCATION (existing_method),
		  "previous declaration of %<%c%E%>",
		  (is_class ? '+' : '-'),
		  METHOD_SEL_NAME (existing_method));
	}
    }

  if (is_class)
    insert_method_into_method_map (true, method);
  else
    {
      insert_method_into_method_map (false, method);

      /* Instance methods in root classes (and categories thereof)
	 may act as class methods as a last resort.  We also add
	 instance methods listed in @protocol declarations to
	 the class hash table, on the assumption that @protocols
	 may be adopted by root classes or categories.  */
      if (TREE_CODE (klass) == CATEGORY_INTERFACE_TYPE
	  || TREE_CODE (klass) == CATEGORY_IMPLEMENTATION_TYPE)
	klass = lookup_interface (CLASS_NAME (klass));

      if (TREE_CODE (klass) == PROTOCOL_INTERFACE_TYPE
	  || !CLASS_SUPER_NAME (klass))
	insert_method_into_method_map (true, method);
    }

  return method;
}

static void
add_category (tree klass, tree category)
{
  /* Put categories on list in reverse order.  */
  tree cat = lookup_category (klass, CLASS_SUPER_NAME (category));

  if (cat)
    {
      warning (0, "duplicate interface declaration for category %<%E(%E)%>",
	       CLASS_NAME (klass),
	       CLASS_SUPER_NAME (category));
    }
  else
    {
      CLASS_CATEGORY_LIST (category) = CLASS_CATEGORY_LIST (klass);
      CLASS_CATEGORY_LIST (klass) = category;
    }
}

#ifndef OBJCPLUS
/* A flexible array member is a C99 extension where you can use
   "type[]" at the end of a struct to mean a variable-length array.

   In Objective-C, instance variables are fundamentally members of a
   struct, but the struct can always be extended by subclassing; hence
   we need to detect and forbid all instance variables declared using
   flexible array members.

   No check for this is needed in Objective-C++, since C++ does not
   have flexible array members.  */

/* Determine whether TYPE is a structure with a flexible array member,
   a union containing such a structure (possibly recursively) or an
   array of such structures or unions.  These are all invalid as
   instance variable.  */
static bool
flexible_array_type_p (tree type)
{
  tree x;
  switch (TREE_CODE (type))
    {
    case RECORD_TYPE:
      x = TYPE_FIELDS (type);
      if (x == NULL_TREE)
	return false;
      while (DECL_CHAIN (x) != NULL_TREE)
	x = DECL_CHAIN (x);
      if (TREE_CODE (TREE_TYPE (x)) == ARRAY_TYPE
	  && TYPE_SIZE (TREE_TYPE (x)) == NULL_TREE
	  && TYPE_DOMAIN (TREE_TYPE (x)) != NULL_TREE
	  && TYPE_MAX_VALUE (TYPE_DOMAIN (TREE_TYPE (x))) == NULL_TREE)
	return true;
      return false;
    case UNION_TYPE:
      for (x = TYPE_FIELDS (type); x != NULL_TREE; x = DECL_CHAIN (x))
	{
	  if (flexible_array_type_p (TREE_TYPE (x)))
	    return true;
	}
      return false;
    /* Note that we also check for arrays of something that uses a flexible array member.  */
    case ARRAY_TYPE:
      if (flexible_array_type_p (TREE_TYPE (type)))
	return true;
      return false;
    default:
    return false;
  }
}
#endif

/* Produce a printable version of an ivar name.  This is only used
   inside add_instance_variable.  */
static const char *
printable_ivar_name (tree field_decl)
{
  if (DECL_NAME (field_decl))
    return identifier_to_locale (IDENTIFIER_POINTER (DECL_NAME (field_decl)));
  else
    return _("<unnamed>");
}

/* Called after parsing each instance variable declaration. Necessary to
   preserve typedefs and implement public/private...

   VISIBILITY is 1 for public, 0 for protected, and 2 for private.  */

static tree
add_instance_variable (tree klass, objc_ivar_visibility_kind visibility,
		       tree field_decl)
{
  tree field_type = TREE_TYPE (field_decl);

#ifdef OBJCPLUS
  if (TREE_CODE (field_type) == REFERENCE_TYPE)
    {
      error ("illegal reference type specified for instance variable %qs",
	     printable_ivar_name (field_decl));
      /* Return class as is without adding this ivar.  */
      return klass;
    }
#endif

  if (field_type == error_mark_node || !TYPE_SIZE (field_type)
      || TYPE_SIZE (field_type) == error_mark_node)
      /* 'type[0]' is allowed, but 'type[]' is not! */
    {
      error ("instance variable %qs has unknown size",
	     printable_ivar_name (field_decl));
      /* Return class as is without adding this ivar.  */
      return klass;
    }

#ifndef OBJCPLUS
  /* Also, in C reject a struct with a flexible array member.  Ie,

       struct A { int x; int[] y; };

       @interface X
       {
         struct A instance_variable;
       }
       @end

       is not valid because if the class is subclassed, we wouldn't be able
       to calculate the offset of the next instance variable.  */
  if (flexible_array_type_p (field_type))
    {
      error ("instance variable %qs uses flexible array member",
	     printable_ivar_name (field_decl));
      /* Return class as is without adding this ivar.  */
      return klass;
    }
#endif

#ifdef OBJCPLUS
  /* Check if the ivar being added has a non-POD C++ type.   If so, we will
     need to either (1) warn the user about it or (2) generate suitable
     constructor/destructor call from '- .cxx_construct' or '- .cxx_destruct'
     methods (if '-fobjc-call-cxx-cdtors' was specified).  */
  if (MAYBE_CLASS_TYPE_P (field_type)
      && (TYPE_NEEDS_CONSTRUCTING (field_type)
	  || TYPE_HAS_NONTRIVIAL_DESTRUCTOR (field_type)
	  || TYPE_POLYMORPHIC_P (field_type)))
    {
      tree type_name = OBJC_TYPE_NAME (field_type);

      if (flag_objc_call_cxx_cdtors)
        {
	  /* Since the ObjC runtime will be calling the constructors and
	     destructors for us, the only thing we can't handle is the lack
	     of a default constructor.  */
	  if (TYPE_NEEDS_CONSTRUCTING (field_type)
	      && !TYPE_HAS_DEFAULT_CONSTRUCTOR (field_type))
	    {
	      warning (0, "type %qE has no default constructor to call",
		       type_name);

	      /* If we cannot call a constructor, we should also avoid
		 calling the destructor, for symmetry.  */
	      if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (field_type))
		warning (0, "destructor for %qE shall not be run either",
			 type_name);
	    }
        }
      else
	{
	  static bool warn_cxx_ivars = false;

	  if (TYPE_POLYMORPHIC_P (field_type))
	    {
	      /* Vtable pointers are Real Bad(tm), since Obj-C cannot
		 initialize them.  */
	      error ("type %qE has virtual member functions", type_name);
	      error ("illegal aggregate type %qE specified "
		     "for instance variable %qs",
		     type_name, printable_ivar_name (field_decl));
	      /* Return class as is without adding this ivar.  */
	      return klass;
	    }

	  /* User-defined constructors and destructors are not known to Obj-C
	     and hence will not be called.  This may or may not be a problem. */
	  if (TYPE_NEEDS_CONSTRUCTING (field_type))
	    warning (0, "type %qE has a user-defined constructor", type_name);
	  if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (field_type))
	    warning (0, "type %qE has a user-defined destructor", type_name);

	  if (!warn_cxx_ivars)
	    {
	      warning (0, "C++ constructors and destructors will not "
		       "be invoked for Objective-C fields");
	      warn_cxx_ivars = true;
	    }
	}
    }
#endif

  /* Overload the public attribute, it is not used for FIELD_DECLs.  */
  switch (visibility)
    {
    case OBJC_IVAR_VIS_PROTECTED:
      TREE_PUBLIC (field_decl) = 0;
      TREE_PRIVATE (field_decl) = 0;
      TREE_PROTECTED (field_decl) = 1;
      break;

    case OBJC_IVAR_VIS_PACKAGE:
    /* TODO: Implement the package variant.  */
    case OBJC_IVAR_VIS_PUBLIC:
      TREE_PUBLIC (field_decl) = 1;
      TREE_PRIVATE (field_decl) = 0;
      TREE_PROTECTED (field_decl) = 0;
      break;

    case OBJC_IVAR_VIS_PRIVATE:
      TREE_PUBLIC (field_decl) = 0;
      TREE_PRIVATE (field_decl) = 1;
      TREE_PROTECTED (field_decl) = 0;
      break;

    }

  CLASS_RAW_IVARS (klass) = chainon (CLASS_RAW_IVARS (klass), field_decl);

  return klass;
}

/* True if the ivar is private and we are not in its implementation.  */

static int
is_private (tree decl)
{
  return (TREE_PRIVATE (decl)
	  && ! is_ivar (CLASS_IVARS (implementation_template),
			DECL_NAME (decl)));
}

/* Searches all the instance variables of 'klass' and of its
   superclasses for an instance variable whose name (identifier) is
   'ivar_name_ident'.  Return the declaration (DECL) of the instance
   variable, if found, or NULL_TREE, if not found.  */
static inline tree
ivar_of_class (tree klass, tree ivar_name_ident)
{
  /* First, look up the ivar in CLASS_RAW_IVARS.  */
  tree decl_chain = CLASS_RAW_IVARS (klass);

  for ( ; decl_chain; decl_chain = DECL_CHAIN (decl_chain))
    if (DECL_NAME (decl_chain) == ivar_name_ident)
      return decl_chain;

  /* If not found, search up the class hierarchy.  */
  while (CLASS_SUPER_NAME (klass))
    {
      klass = lookup_interface (CLASS_SUPER_NAME (klass));

      decl_chain = CLASS_RAW_IVARS (klass);

      for ( ; decl_chain; decl_chain = DECL_CHAIN (decl_chain))
	if (DECL_NAME (decl_chain) == ivar_name_ident)
	  return decl_chain;
    }

  return NULL_TREE;
}

/* We have an instance variable reference;, check to see if it is public.  */

int
objc_is_public (tree expr, tree identifier)
{
  tree basetype, decl;

#ifdef OBJCPLUS
  if (processing_template_decl)
    return 1;
#endif

  if (TREE_TYPE (expr) == error_mark_node)
    return 1;

  basetype = TYPE_MAIN_VARIANT (TREE_TYPE (expr));

  if (basetype && TREE_CODE (basetype) == RECORD_TYPE)
    {
      if (TYPE_HAS_OBJC_INFO (basetype) && TYPE_OBJC_INTERFACE (basetype))
	{
	  tree klass = lookup_interface (OBJC_TYPE_NAME (basetype));

	  if (!klass)
	    {
	      error ("cannot find interface declaration for %qE",
		     OBJC_TYPE_NAME (basetype));
	      return 0;
	    }

	  if ((decl = ivar_of_class (klass, identifier)))
	    {
	      if (TREE_PUBLIC (decl))
		return 1;

	      /* Important difference between the Stepstone translator:
		 all instance variables should be public within the context
		 of the implementation.  */
	      if (objc_implementation_context
		 && ((TREE_CODE (objc_implementation_context)
		      == CLASS_IMPLEMENTATION_TYPE)
		     || (TREE_CODE (objc_implementation_context)
			 == CATEGORY_IMPLEMENTATION_TYPE)))
		{
		  tree curtype = TYPE_MAIN_VARIANT
				 (CLASS_STATIC_TEMPLATE
				  (implementation_template));

		  if (basetype == curtype
		      || DERIVED_FROM_P (basetype, curtype))
		    {
		      int priv = is_private (decl);

		      if (priv)
			error ("instance variable %qE is declared private",
			       DECL_NAME (decl));

		      return !priv;
		    }
		}

	      /* The 2.95.2 compiler sometimes allowed C functions to access
		 non-@public ivars.  We will let this slide for now...  */
	      if (!objc_method_context)
	      {
		warning (0, "instance variable %qE is %s; "
			 "this will be a hard error in the future",
			 identifier,
			 TREE_PRIVATE (decl) ? "@private" : "@protected");
		return 1;
	      }

	      error ("instance variable %qE is declared %s",
		     identifier,
		     TREE_PRIVATE (decl) ? "private" : "protected");
	      return 0;
	    }
	}
    }

  return 1;
}

/* Make sure all methods in CHAIN (a list of method declarations from
   an @interface or a @protocol) are in IMPLEMENTATION (the
   implementation context).  This is used to check for example that
   all methods declared in an @interface were implemented in an
   @implementation.

   Some special methods (property setters/getters) are special and if
   they are not found in IMPLEMENTATION, we look them up in its
   superclasses.  */

static int
check_methods (tree chain, tree implementation, int mtype)
{
  int first = 1;
  tree list;

  if (mtype == (int)'+')
    list = CLASS_CLS_METHODS (implementation);
  else
    list = CLASS_NST_METHODS (implementation);

  while (chain)
    {
      /* If the method is associated with a dynamic property, then it
	 is Ok not to have the method implementation, as it will be
	 generated dynamically at runtime.  To decide if the method is
	 associated with a @dynamic property, we search the list of
	 @synthesize and @dynamic for this implementation, and look
	 for any @dynamic property with the same setter or getter name
	 as this method.  */
      tree x;
      for (x = IMPL_PROPERTY_DECL (implementation); x; x = TREE_CHAIN (x))
	if (PROPERTY_DYNAMIC (x)
	    && (PROPERTY_GETTER_NAME (x) == METHOD_SEL_NAME (chain)
		|| PROPERTY_SETTER_NAME (x) == METHOD_SEL_NAME (chain)))
	  break;

      if (x != NULL_TREE)
	{
	  chain = TREE_CHAIN (chain); /* next method...  */
	  continue;
	}

      if (!lookup_method (list, chain))
	{
	  /* If the method is a property setter/getter, we'll still
	     allow it to be missing if it is implemented by
	     'interface' or any of its superclasses.  */
	  tree property = METHOD_PROPERTY_CONTEXT (chain);
	  if (property)
	    {
	      /* Note that since this is a property getter/setter, it
		 is obviously an instance method.  */
	      tree interface = NULL_TREE;

	      /* For a category, first check the main class
		 @interface.  */
	      if (TREE_CODE (implementation) == CATEGORY_IMPLEMENTATION_TYPE)
		{
		  interface = lookup_interface (CLASS_NAME (implementation));

		  /* If the method is found in the main class, it's Ok.  */
		  if (lookup_method (CLASS_NST_METHODS (interface), chain))
		    {
		      chain = DECL_CHAIN (chain);
		      continue;
		    }

		  /* Else, get the superclass.  */
		  if (CLASS_SUPER_NAME (interface))
		    interface = lookup_interface (CLASS_SUPER_NAME (interface));
		  else
		    interface = NULL_TREE;
		}

	      /* Get the superclass for classes.  */
	      if (TREE_CODE (implementation) == CLASS_IMPLEMENTATION_TYPE)
		{
		  if (CLASS_SUPER_NAME (implementation))
		    interface = lookup_interface (CLASS_SUPER_NAME (implementation));
		  else
		    interface = NULL_TREE;
		}

	      /* Now, interface is the superclass, if any; go check it.  */
	      if (interface)
		{
		  if (lookup_method_static (interface, chain, 0))
		    {
		      chain = DECL_CHAIN (chain);
		      continue;
		    }
		}
	      /* Else, fall through - warn.  */
	    }
	  if (first)
	    {
	      switch (TREE_CODE (implementation))
		{
		case CLASS_IMPLEMENTATION_TYPE:
		  warning (0, "incomplete implementation of class %qE",
			   CLASS_NAME (implementation));
		  break;
		case CATEGORY_IMPLEMENTATION_TYPE:
		  warning (0, "incomplete implementation of category %qE",
			   CLASS_SUPER_NAME (implementation));
		  break;
		default:
		  gcc_unreachable ();
		}
	      first = 0;
	    }

	  warning (0, "method definition for %<%c%E%> not found",
		   mtype, METHOD_SEL_NAME (chain));
	}

      chain = DECL_CHAIN (chain);
    }

    return first;
}

/* Check if KLASS, or its superclasses, explicitly conforms to PROTOCOL.  */

static int
conforms_to_protocol (tree klass, tree protocol)
{
   if (TREE_CODE (protocol) == PROTOCOL_INTERFACE_TYPE)
     {
       tree p = CLASS_PROTOCOL_LIST (klass);
       while (p && TREE_VALUE (p) != protocol)
	 p = TREE_CHAIN (p);

       if (!p)
	 {
	   tree super = (CLASS_SUPER_NAME (klass)
			 ? lookup_interface (CLASS_SUPER_NAME (klass))
			 : NULL_TREE);
	   int tmp = super ? conforms_to_protocol (super, protocol) : 0;
	   if (!tmp)
	     return 0;
	 }
     }

   return 1;
}

/* Make sure all methods in CHAIN are accessible as MTYPE methods in
   CONTEXT.  This is one of two mechanisms to check protocol integrity.  */

static int
check_methods_accessible (tree chain, tree context, int mtype)
{
  int first = 1;
  tree list;
  tree base_context = context;

  while (chain)
    {
      /* If the method is associated with a dynamic property, then it
	 is Ok not to have the method implementation, as it will be
	 generated dynamically at runtime.  Search for any @dynamic
	 property with the same setter or getter name as this
	 method.  TODO: Use a hashtable lookup.  */
      tree x;
      for (x = IMPL_PROPERTY_DECL (base_context); x; x = TREE_CHAIN (x))
	if (PROPERTY_DYNAMIC (x)
	    && (PROPERTY_GETTER_NAME (x) == METHOD_SEL_NAME (chain)
		|| PROPERTY_SETTER_NAME (x) == METHOD_SEL_NAME (chain)))
	  break;

      if (x != NULL_TREE)
	{
	  chain = TREE_CHAIN (chain); /* next method...  */
	  continue;
	}

      context = base_context;
      while (context)
	{
	  if (mtype == '+')
	    list = CLASS_CLS_METHODS (context);
	  else
	    list = CLASS_NST_METHODS (context);

	  if (lookup_method (list, chain))
	      break;

	  switch (TREE_CODE (context))
	    {
	    case CLASS_IMPLEMENTATION_TYPE:
	    case CLASS_INTERFACE_TYPE:
	      context = (CLASS_SUPER_NAME (context)
			 ? lookup_interface (CLASS_SUPER_NAME (context))
			 : NULL_TREE);
	      break;
	    case CATEGORY_IMPLEMENTATION_TYPE:
	    case CATEGORY_INTERFACE_TYPE:
	      context = (CLASS_NAME (context)
			 ? lookup_interface (CLASS_NAME (context))
			 : NULL_TREE);
	      break;
	    default:
	      gcc_unreachable ();
	    }
	}

      if (context == NULL_TREE)
	{
	  if (first)
	    {
	      switch (TREE_CODE (objc_implementation_context))
		{
		case CLASS_IMPLEMENTATION_TYPE:
		  warning (0, "incomplete implementation of class %qE",
			   CLASS_NAME (objc_implementation_context));
		  break;
		case CATEGORY_IMPLEMENTATION_TYPE:
		  warning (0, "incomplete implementation of category %qE",
			   CLASS_SUPER_NAME (objc_implementation_context));
		  break;
		default:
		  gcc_unreachable ();
		}
	      first = 0;
	    }
	  warning (0, "method definition for %<%c%E%> not found",
		   mtype, METHOD_SEL_NAME (chain));
	}

      chain = TREE_CHAIN (chain); /* next method...  */
    }
  return first;
}

/* Check whether the current interface (accessible via
   'objc_implementation_context') actually implements protocol P, along
   with any protocols that P inherits.  */

static void
check_protocol (tree p, const char *type, tree name)
{
  if (TREE_CODE (p) == PROTOCOL_INTERFACE_TYPE)
    {
      int f1, f2;

      /* Ensure that all protocols have bodies!  */
      if (warn_protocol)
	{
	  f1 = check_methods (PROTOCOL_CLS_METHODS (p),
			      objc_implementation_context,
			      '+');
	  f2 = check_methods (PROTOCOL_NST_METHODS (p),
			      objc_implementation_context,
			      '-');
	}
      else
	{
	  f1 = check_methods_accessible (PROTOCOL_CLS_METHODS (p),
					 objc_implementation_context,
					 '+');
	  f2 = check_methods_accessible (PROTOCOL_NST_METHODS (p),
					 objc_implementation_context,
					 '-');
	}

      if (!f1 || !f2)
	warning (0, "%s %qE does not fully implement the %qE protocol",
		 type, name, PROTOCOL_NAME (p));
    }

  /* Check protocols recursively.  */
  if (PROTOCOL_LIST (p))
    {
      tree subs = PROTOCOL_LIST (p);
      tree super_class =
	lookup_interface (CLASS_SUPER_NAME (implementation_template));

      while (subs)
	{
	  tree sub = TREE_VALUE (subs);

	  /* If the superclass does not conform to the protocols
	     inherited by P, then we must!  */
	  if (!super_class || !conforms_to_protocol (super_class, sub))
	    check_protocol (sub, type, name);
	  subs = TREE_CHAIN (subs);
	}
    }
}

/* Check whether the current interface (accessible via
   'objc_implementation_context') actually implements the protocols listed
   in PROTO_LIST.  */

static void
check_protocols (tree proto_list, const char *type, tree name)
{
  for ( ; proto_list; proto_list = TREE_CHAIN (proto_list))
    {
      tree p = TREE_VALUE (proto_list);

      check_protocol (p, type, name);
    }
}

/* Make sure that the class CLASS_NAME is defined CODE says which kind
   of thing CLASS_NAME ought to be.  It can be CLASS_INTERFACE_TYPE,
   CLASS_IMPLEMENTATION_TYPE, CATEGORY_INTERFACE_TYPE, or
   CATEGORY_IMPLEMENTATION_TYPE.  For a CATEGORY_INTERFACE_TYPE,
   SUPER_NAME is the name of the category.  For a class extension,
   CODE is CATEGORY_INTERFACE_TYPE and SUPER_NAME is NULL_TREE.  */
static tree
start_class (enum tree_code code, tree class_name, tree super_name,
	     tree protocol_list, tree attributes)
{
  tree klass = NULL_TREE;
  tree decl;

#ifdef OBJCPLUS
  if (current_namespace != global_namespace)
    {
      error ("Objective-C declarations may only appear in global scope");
    }
#endif /* OBJCPLUS */

  if (objc_implementation_context)
    {
      warning (0, "%<@end%> missing in implementation context");
      finish_class (objc_implementation_context);
      objc_ivar_chain = NULL_TREE;
      objc_implementation_context = NULL_TREE;
    }

  /* If this is a class extension, we'll be "reopening" the existing
     CLASS_INTERFACE_TYPE, so in that case there is no need to create
     a new node.  */
  if (code != CATEGORY_INTERFACE_TYPE || super_name != NULL_TREE)
    {
      klass = make_node (code);
      TYPE_LANG_SLOT_1 (klass) = make_tree_vec (CLASS_LANG_SLOT_ELTS);
    }

  /* Check for existence of the super class, if one was specified.  Note
     that we must have seen an @interface, not just a @class.  If we
     are looking at a @compatibility_alias, traverse it first.  */
  if ((code == CLASS_INTERFACE_TYPE || code == CLASS_IMPLEMENTATION_TYPE)
      && super_name)
    {
      tree super = objc_is_class_name (super_name);
      tree super_interface = NULL_TREE;

      if (super)
	super_interface = lookup_interface (super);

      if (!super_interface)
	{
	  error ("cannot find interface declaration for %qE, superclass of %qE",
		 super ? super : super_name,
		 class_name);
	  super_name = NULL_TREE;
	}
      else
	{
	  if (TREE_DEPRECATED (super_interface))
	    warning (OPT_Wdeprecated_declarations, "class %qE is deprecated",
		     super);
	  super_name = super;
	}
    }

  if (code != CATEGORY_INTERFACE_TYPE || super_name != NULL_TREE)
    {
      CLASS_NAME (klass) = class_name;
      CLASS_SUPER_NAME (klass) = super_name;
      CLASS_CLS_METHODS (klass) = NULL_TREE;
    }

  if (! objc_is_class_name (class_name)
      && (decl = lookup_name (class_name)))
    {
      error ("%qE redeclared as different kind of symbol",
	     class_name);
      error ("previous declaration of %q+D",
	     decl);
    }

  switch (code)
    {
    case CLASS_IMPLEMENTATION_TYPE:
      {
	tree chain;

	for (chain = implemented_classes; chain; chain = TREE_CHAIN (chain))
	  if (TREE_VALUE (chain) == class_name)
	    {
	      error ("reimplementation of class %qE",
		     class_name);
	      /* TODO: error message saying where it was previously
		 implemented.  */
	      break;
	    }
	if (chain == NULL_TREE)
	  implemented_classes = tree_cons (NULL_TREE, class_name,
					   implemented_classes);
      }

      /* Reset for multiple classes per file.  */
      method_slot = 0;

      objc_implementation_context = klass;

      /* Lookup the interface for this implementation.  */

      if (!(implementation_template = lookup_interface (class_name)))
        {
	  warning (0, "cannot find interface declaration for %qE",
		   class_name);
	  add_interface (implementation_template = objc_implementation_context,
			 class_name);
        }

      /* If a super class has been specified in the implementation,
	 insure it conforms to the one specified in the interface.  */

      if (super_name
	  && (super_name != CLASS_SUPER_NAME (implementation_template)))
	{
	  tree previous_name = CLASS_SUPER_NAME (implementation_template);
	  error ("conflicting super class name %qE",
		 super_name);
	  if (previous_name)
	    error ("previous declaration of %qE", previous_name);
	  else
	    error ("previous declaration");
	}

      else if (! super_name)
	{
	  CLASS_SUPER_NAME (objc_implementation_context)
	    = CLASS_SUPER_NAME (implementation_template);
	}
      break;

    case CLASS_INTERFACE_TYPE:
      if (lookup_interface (class_name))
#ifdef OBJCPLUS
	error ("duplicate interface declaration for class %qE", class_name);
#else
        warning (0, "duplicate interface declaration for class %qE", class_name);
#endif
      else
	add_interface (klass, class_name);

      if (protocol_list)
	CLASS_PROTOCOL_LIST (klass)
	  = lookup_and_install_protocols (protocol_list, /* definition_required */ true);

      if (attributes)
	{
	  tree attribute;
	  for (attribute = attributes; attribute; attribute = TREE_CHAIN (attribute))
	    {
	      tree name = TREE_PURPOSE (attribute);

	      /* TODO: Document what the objc_exception attribute is/does.  */
	      /* We handle the 'deprecated' and (undocumented) 'objc_exception'
		 attributes.  */
	      if (is_attribute_p  ("deprecated", name))
		TREE_DEPRECATED (klass) = 1;
	      else if (is_attribute_p  ("objc_exception", name))
		CLASS_HAS_EXCEPTION_ATTR (klass) = 1;
	      else
		/* Warn about and ignore all others for now, but store them.  */
		warning (OPT_Wattributes, "%qE attribute directive ignored", name);
	    }
	  TYPE_ATTRIBUTES (klass) = attributes;
	}
      break;

    case CATEGORY_INTERFACE_TYPE:
      {
	tree class_category_is_assoc_with;

	/* For a category, class_name is really the name of the class that
	   the following set of methods will be associated with. We must
	   find the interface so that can derive the objects template.  */
	if (!(class_category_is_assoc_with = lookup_interface (class_name)))
	  {
	    error ("cannot find interface declaration for %qE",
		   class_name);
	    exit (FATAL_EXIT_CODE);
	  }
	else
	  {
	    if (TREE_DEPRECATED (class_category_is_assoc_with))
	      warning (OPT_Wdeprecated_declarations, "class %qE is deprecated",
		       class_name);

	    if (super_name == NULL_TREE)
	      {
		/* This is a class extension.  Get the original
		   interface, and continue working on it.  */
		objc_in_class_extension = true;
		klass = class_category_is_assoc_with;

		if (protocol_list)
		  {
		    /* Append protocols to the original protocol
		       list.  */
		    CLASS_PROTOCOL_LIST (klass)
		      = chainon (CLASS_PROTOCOL_LIST (klass),
				 lookup_and_install_protocols
				 (protocol_list,
				  /* definition_required */ true));
		  }
	      }
	    else
	      {
		add_category (class_category_is_assoc_with, klass);

		if (protocol_list)
		  CLASS_PROTOCOL_LIST (klass)
		    = lookup_and_install_protocols
		    (protocol_list, /* definition_required */ true);
	      }
	  }
      }
      break;

    case CATEGORY_IMPLEMENTATION_TYPE:
      /* Reset for multiple classes per file.  */
      method_slot = 0;

      objc_implementation_context = klass;

      /* For a category, class_name is really the name of the class that
	 the following set of methods will be associated with.  We must
	 find the interface so that can derive the objects template.  */

      if (!(implementation_template = lookup_interface (class_name)))
        {
	  error ("cannot find interface declaration for %qE",
		 class_name);
	  exit (FATAL_EXIT_CODE);
        }
      break;
    default:
      gcc_unreachable ();
    }
  return klass;
}

static tree
continue_class (tree klass)
{
  switch (TREE_CODE (klass))
    {
    case CLASS_IMPLEMENTATION_TYPE:
    case CATEGORY_IMPLEMENTATION_TYPE:
      {
	struct imp_entry *imp_entry;

        /* Check consistency of the instance variables.  */

	if (CLASS_RAW_IVARS (klass))
	  check_ivars (implementation_template, klass);

	/* code generation */
#ifdef OBJCPLUS
	push_lang_context (lang_name_c);
#endif
	build_private_template (implementation_template);
	uprivate_record = CLASS_STATIC_TEMPLATE (implementation_template);
	objc_instance_type = build_pointer_type (uprivate_record);

	imp_entry = ggc_alloc_imp_entry ();

	imp_entry->next = imp_list;
	imp_entry->imp_context = klass;
	imp_entry->imp_template = implementation_template;
	ucls_super_ref = uucls_super_ref = NULL;
	if (TREE_CODE (klass) == CLASS_IMPLEMENTATION_TYPE)
	  {
	    imp_entry->class_decl = (*runtime.class_decl) (klass);
	    imp_entry->meta_decl = (*runtime.metaclass_decl) (klass);
	  }
	else
	  {
	    imp_entry->class_decl = (*runtime.category_decl) (klass);
	    imp_entry->meta_decl = NULL;
	  }
	imp_entry->has_cxx_cdtors = 0;

	/* Append to front and increment count.  */
	imp_list = imp_entry;
	if (TREE_CODE (klass) == CLASS_IMPLEMENTATION_TYPE)
	  imp_count++;
	else
	  cat_count++;
#ifdef OBJCPLUS
	pop_lang_context ();
#endif /* OBJCPLUS */

	return get_class_ivars (implementation_template, true);
	break;
      }
    case CLASS_INTERFACE_TYPE:
      {
	if (objc_in_class_extension)
	  return NULL_TREE;
#ifdef OBJCPLUS
	push_lang_context (lang_name_c);
#endif /* OBJCPLUS */
	objc_collecting_ivars = 1;
	build_private_template (klass);
	objc_collecting_ivars = 0;
#ifdef OBJCPLUS
	pop_lang_context ();
#endif /* OBJCPLUS */
	return NULL_TREE;
	break;
      }
    default:
      return error_mark_node;
    }
}

/* This routine builds name of the setter synthesized function. */
char *
objc_build_property_setter_name (tree ident)
{
  /* TODO: Use alloca to allocate buffer of appropriate size.  */
  static char string[BUFSIZE];
  sprintf (string, "set%s:", IDENTIFIER_POINTER (ident));
  string[3] = TOUPPER (string[3]);
  return string;
}

/* This routine prepares the declarations of the property accessor
   helper functions (objc_getProperty(), etc) that are used when
   @synthesize is used.

   runtime-specific routines are built in the respective runtime
   initialize functions.  */
static void
build_common_objc_property_accessor_helpers (void)
{
  tree type;

  /* Declare the following function:
     id
     objc_getProperty (id self, SEL _cmd,
                       ptrdiff_t offset, BOOL is_atomic);  */
  type = build_function_type_list (objc_object_type,
				   objc_object_type,
				   objc_selector_type,
				   ptrdiff_type_node,
				   boolean_type_node,
				   NULL_TREE);
  objc_getProperty_decl = add_builtin_function ("objc_getProperty",
						type, 0, NOT_BUILT_IN,
						NULL, NULL_TREE);
  TREE_NOTHROW (objc_getProperty_decl) = 0;

  /* Declare the following function:
     void
     objc_setProperty (id self, SEL _cmd,
                       ptrdiff_t offset, id new_value,
                       BOOL is_atomic, BOOL should_copy);  */
  type = build_function_type_list (void_type_node,
				   objc_object_type,
				   objc_selector_type,
				   ptrdiff_type_node,
				   objc_object_type,
				   boolean_type_node,
				   boolean_type_node,
				   NULL_TREE);
  objc_setProperty_decl = add_builtin_function ("objc_setProperty",
						type, 0, NOT_BUILT_IN,
						NULL, NULL_TREE);
  TREE_NOTHROW (objc_setProperty_decl) = 0;
}

/* This looks up an ivar in a class (including superclasses).  */
static tree
lookup_ivar (tree interface, tree instance_variable_name)
{
  while (interface)
    {
      tree decl_chain;

      for (decl_chain = CLASS_IVARS (interface); decl_chain; decl_chain = DECL_CHAIN (decl_chain))
	if (DECL_NAME (decl_chain) == instance_variable_name)
	  return decl_chain;

      /* Not found.  Search superclass if any.  */
      if (CLASS_SUPER_NAME (interface))
	interface = lookup_interface (CLASS_SUPER_NAME (interface));
    }

  return NULL_TREE;
}

/* This routine synthesizes a 'getter' method.  This is only called
   for @synthesize properties.  */
static void
objc_synthesize_getter (tree klass, tree class_methods ATTRIBUTE_UNUSED, tree property)
{
  location_t location = DECL_SOURCE_LOCATION (property);
  tree fn, decl;
  tree body;
  tree ret_val;

  /* If user has implemented a getter with same name then do nothing.  */
  if (lookup_method (CLASS_NST_METHODS (objc_implementation_context),
		     PROPERTY_GETTER_NAME (property)))
    return;

  /* Find declaration of the property getter in the interface (or
     superclass, or protocol). There must be one.  */
  decl = lookup_method_static (klass, PROPERTY_GETTER_NAME (property), 0);

  /* If one not declared in the interface, this condition has already
     been reported as user error (because property was not declared in
     the interface).  */
  if (!decl)
    return;

  /* Adapt the 'decl'.  Use the source location of the @synthesize
     statement for error messages.  */
  decl = copy_node (decl);
  DECL_SOURCE_LOCATION (decl) = location;

  objc_start_method_definition (false /* is_class_method */, decl, NULL_TREE,
				NULL_TREE);
  body = c_begin_compound_stmt (true);

  /* Now we need to decide how we build the getter.  There are three
     cases:

     for 'copy' or 'retain' properties we need to use the
     objc_getProperty() accessor helper which knows about retain and
     copy.  It supports both 'nonatomic' and 'atomic' access.

     for 'nonatomic, assign' properties we can access the instance
     variable directly.  'nonatomic' means we don't have to use locks,
     and 'assign' means we don't have to worry about retain or copy.
     If you combine the two, it means we can just access the instance
     variable directly.

     for 'atomic, assign' properties we use objc_copyStruct() (for the
     next runtime) or objc_getPropertyStruct() (for the GNU runtime).  */
  switch (PROPERTY_ASSIGN_SEMANTICS (property))
    {
    case OBJC_PROPERTY_RETAIN:
    case OBJC_PROPERTY_COPY:
      {
	/* We build "return objc_getProperty (self, _cmd, offset, is_atomic);"  */
	tree cmd, ivar, offset, is_atomic;
	cmd = TREE_CHAIN (DECL_ARGUMENTS (current_function_decl));

	/* Find the ivar to compute the offset.  */
	ivar = lookup_ivar (klass, PROPERTY_IVAR_NAME (property));
	if (!ivar || is_private (ivar))
	  {
	    /* This should never happen.  */
	    error_at (location,
		      "can not find instance variable associated with property");
	    ret_val = error_mark_node;
	    break;
	  }
	offset = byte_position (ivar);

	if (PROPERTY_NONATOMIC (property))
	  is_atomic = boolean_false_node;
	else
	  is_atomic = boolean_true_node;

	ret_val = build_function_call
	  (location,
	   /* Function prototype.  */
	   objc_getProperty_decl,
	   /* Parameters.  */
	   tree_cons    /* self */
	   (NULL_TREE, self_decl,
	    tree_cons   /* _cmd */
	    (NULL_TREE, cmd,
	     tree_cons  /* offset */
	     (NULL_TREE, offset,
	      tree_cons /* is_atomic */
	      (NULL_TREE, is_atomic, NULL_TREE)))));
      }
      break;
    case OBJC_PROPERTY_ASSIGN:
      if (PROPERTY_NONATOMIC (property))
	{
	  /* We build "return self->PROPERTY_IVAR_NAME;"  */
	  ret_val = objc_lookup_ivar (NULL_TREE, PROPERTY_IVAR_NAME (property));
	  break;
	}
      else
	{
	  /* We build
	       <property type> __objc_property_temp;
	       objc_getPropertyStruct (&__objc_property_temp,
	                               &(self->PROPERTY_IVAR_NAME),
	                               sizeof (type of self->PROPERTY_IVAR_NAME),
				       is_atomic,
				       false)
	       return __objc_property_temp;

	     For the NeXT runtime, we need to use objc_copyStruct
	     instead of objc_getPropertyStruct.  */
	  tree objc_property_temp_decl, function_decl, function_call;
	  tree size_of, is_atomic;

	  objc_property_temp_decl = objc_create_temporary_var (TREE_TYPE (property), "__objc_property_temp");
	  DECL_SOURCE_LOCATION (objc_property_temp_decl) = location;
	  objc_property_temp_decl = lang_hooks.decls.pushdecl (objc_property_temp_decl);

	  /* sizeof (ivar type).  Since the ivar and the property have
	     the same type, there is no need to lookup the ivar.  */
	  size_of = c_sizeof_or_alignof_type (location, TREE_TYPE (property),
					      true /* is_sizeof */,
					      false /* min_alignof */,
					      false /* complain */);

	  if (PROPERTY_NONATOMIC (property))
	    is_atomic = boolean_false_node;
	  else
	    is_atomic = boolean_true_node;

	  if (objc_copyStruct_decl)
	    function_decl = objc_copyStruct_decl;
	  else
	    function_decl = objc_getPropertyStruct_decl;

	  function_call = build_function_call
	    (location,
	     /* Function prototype.  */
	     function_decl,
	     /* Parameters.  */
	     tree_cons /* &__objc_property_temp_decl */
	     /* Warning: note that using build_fold_addr_expr_loc()
		here causes invalid code to be generated.  */
	     (NULL_TREE, build_unary_op (location, ADDR_EXPR, objc_property_temp_decl, 0),
	      tree_cons /* &(self->PROPERTY_IVAR_NAME); */
	      (NULL_TREE, build_fold_addr_expr_loc (location,
						    objc_lookup_ivar
						    (NULL_TREE, PROPERTY_IVAR_NAME (property))),
	       tree_cons /* sizeof (PROPERTY_IVAR) */
	       (NULL_TREE, size_of,
		tree_cons /* is_atomic */
		(NULL_TREE, is_atomic,
		 /* TODO: This is currently ignored by the GNU
		    runtime, but what about the next one ? */
		 tree_cons /* has_strong */
		 (NULL_TREE, boolean_true_node, NULL_TREE))))));

	  add_stmt (function_call);

	  ret_val = objc_property_temp_decl;
	}
      break;
    default:
      gcc_unreachable ();
    }

  gcc_assert (ret_val);

#ifdef OBJCPLUS
  finish_return_stmt (ret_val);
#else
  c_finish_return (location, ret_val, NULL_TREE);
#endif

  add_stmt (c_end_compound_stmt (location, body, true));
  fn = current_function_decl;
#ifdef OBJCPLUS
  finish_function ();
#endif
  objc_finish_method_definition (fn);
}

/* This routine synthesizes a 'setter' method.  */

static void
objc_synthesize_setter (tree klass, tree class_methods ATTRIBUTE_UNUSED, tree property)
{
  location_t location = DECL_SOURCE_LOCATION (property);
  tree fn, decl;
  tree body;
  tree new_value, statement;

  /* If user has implemented a setter with same name then do nothing.  */
  if (lookup_method (CLASS_NST_METHODS (objc_implementation_context),
		     PROPERTY_SETTER_NAME (property)))
    return;

  /* Find declaration of the property setter in the interface (or
     superclass, or protocol). There must be one.  */
  decl = lookup_method_static (klass, PROPERTY_SETTER_NAME (property), 0);

  /* If one not declared in the interface, this condition has already
     been reported as user error (because property was not declared in
     the interface).  */
  if (!decl)
    return;

  /* Adapt the 'decl'.  Use the source location of the @synthesize
     statement for error messages.  */
  decl = copy_node (decl);
  DECL_SOURCE_LOCATION (decl) = DECL_SOURCE_LOCATION (property);

  objc_start_method_definition (false /* is_class_method */, decl, NULL_TREE,
				NULL_TREE);

  body = c_begin_compound_stmt (true);

  /* The 'new_value' is the only argument to the method, which is the
     3rd argument of the function, after self and _cmd.  We use twice
     TREE_CHAIN to move forward two arguments.  */
  new_value = TREE_CHAIN (TREE_CHAIN (DECL_ARGUMENTS (current_function_decl)));

  /* This would presumably happen if the user has specified a
     prototype for the setter that does not have an argument!  */
  if (new_value == NULL_TREE)
    {
      /* TODO: This should be caught much earlier than this.  */
      error_at (DECL_SOURCE_LOCATION (decl), "invalid setter, it must have one argument");
      /* Try to recover somehow.  */
      new_value = error_mark_node;
    }

  /* Now we need to decide how we build the setter.  There are three
     cases:

     for 'copy' or 'retain' properties we need to use the
     objc_setProperty() accessor helper which knows about retain and
     copy.  It supports both 'nonatomic' and 'atomic' access.

     for 'nonatomic, assign' properties we can access the instance
     variable directly.  'nonatomic' means we don't have to use locks,
     and 'assign' means we don't have to worry about retain or copy.
     If you combine the two, it means we can just access the instance
     variable directly.

     for 'atomic, assign' properties we use objc_copyStruct() (for the
     next runtime) or objc_setPropertyStruct() (for the GNU runtime).  */
  switch (PROPERTY_ASSIGN_SEMANTICS (property))
    {
    case OBJC_PROPERTY_RETAIN:
    case OBJC_PROPERTY_COPY:
      {
	/* We build "objc_setProperty (self, _cmd, new_value, offset, is_atomic, should_copy);"  */
	tree cmd, ivar, offset, is_atomic, should_copy;
	cmd = TREE_CHAIN (DECL_ARGUMENTS (current_function_decl));

	/* Find the ivar to compute the offset.  */
	ivar = lookup_ivar (klass, PROPERTY_IVAR_NAME (property));
	if (!ivar || is_private (ivar))
	  {
	    error_at (location,
		      "can not find instance variable associated with property");
	    statement = error_mark_node;
	    break;
	  }
	offset = byte_position (ivar);

	if (PROPERTY_NONATOMIC (property))
	  is_atomic = boolean_false_node;
	else
	  is_atomic = boolean_true_node;

	if (PROPERTY_ASSIGN_SEMANTICS (property) == OBJC_PROPERTY_COPY)
	  should_copy = boolean_true_node;
	else
	  should_copy = boolean_false_node;

	statement = build_function_call
	  (location,
	   /* Function prototype.  */
	   objc_setProperty_decl,
	   /* Parameters.  */
	   tree_cons    /* self */
	   (NULL_TREE, self_decl,
	    tree_cons   /* _cmd */
	    (NULL_TREE, cmd,
	     tree_cons  /* offset */
	     (NULL_TREE, offset,
	      tree_cons /* new_value */
	      (NULL_TREE, new_value,
	       tree_cons /* is_atomic */
	       (NULL_TREE, is_atomic,
		tree_cons /* should_copy */
		(NULL_TREE, should_copy, NULL_TREE)))))));
      }
      break;
    case OBJC_PROPERTY_ASSIGN:
      if (PROPERTY_NONATOMIC (property))
	{
	  /* We build "self->PROPERTY_IVAR_NAME = new_value;"  */
	  statement = build_modify_expr
	    (location,
	     objc_lookup_ivar (NULL_TREE, PROPERTY_IVAR_NAME (property)),
	     NULL_TREE, NOP_EXPR,
	     location, new_value, NULL_TREE);
	  break;
	}
      else
	{
	  /* We build
	       objc_setPropertyStruct (&(self->PROPERTY_IVAR_NAME),
	                               &new_value,
	                               sizeof (type of self->PROPERTY_IVAR_NAME),
				       is_atomic,
				       false)

	     For the NeXT runtime, we need to use objc_copyStruct
	     instead of objc_getPropertyStruct.  */
	  tree function_decl, size_of, is_atomic;

	  /* sizeof (ivar type).  Since the ivar and the property have
	     the same type, there is no need to lookup the ivar.  */
	  size_of = c_sizeof_or_alignof_type (location, TREE_TYPE (property),
					      true /* is_sizeof */,
					      false /* min_alignof */,
					      false /* complain */);

	  if (PROPERTY_NONATOMIC (property))
	    is_atomic = boolean_false_node;
	  else
	    is_atomic = boolean_true_node;

	  if (objc_copyStruct_decl)
	    function_decl = objc_copyStruct_decl;
	  else
	    function_decl = objc_setPropertyStruct_decl;

	  statement = build_function_call
	    (location,
	     /* Function prototype.  */
	     function_decl,
	     /* Parameters.  */
	     tree_cons /* &(self->PROPERTY_IVAR_NAME); */
	     (NULL_TREE, build_fold_addr_expr_loc (location,
						   objc_lookup_ivar
						   (NULL_TREE, PROPERTY_IVAR_NAME (property))),
	      tree_cons /* &new_value */
	      (NULL_TREE, build_fold_addr_expr_loc (location, new_value),
	       tree_cons /* sizeof (PROPERTY_IVAR) */
	       (NULL_TREE, size_of,
		tree_cons /* is_atomic */
		(NULL_TREE, is_atomic,
		 /* TODO: This is currently ignored by the GNU
		    runtime, but what about the next one ? */
		 tree_cons /* has_strong */
		 (NULL_TREE, boolean_true_node, NULL_TREE))))));
	}
      break;
    default:
      gcc_unreachable ();
    }
  gcc_assert (statement);

  add_stmt (statement);
  add_stmt (c_end_compound_stmt (location, body, true));
  fn = current_function_decl;
#ifdef OBJCPLUS
  finish_function ();
#endif
  objc_finish_method_definition (fn);
}

/* This function is a sub-routine of objc_add_synthesize_declaration.
   It is called for each property to synthesize once we have
   determined that the context is Ok.  */
static void
objc_add_synthesize_declaration_for_property (location_t location, tree interface,
					      tree property_name, tree ivar_name)
{
  /* Find the @property declaration.  */
  tree property;
  tree x;

  /* Check that synthesize or dynamic has not already been used for
     the same property.  */
  for (property = IMPL_PROPERTY_DECL (objc_implementation_context); property; property = TREE_CHAIN (property))
    if (PROPERTY_NAME (property) == property_name)
      {
	location_t original_location = DECL_SOURCE_LOCATION (property);

	if (PROPERTY_DYNAMIC (property))
	  error_at (location, "property %qs already specified in %<@dynamic%>",
		    IDENTIFIER_POINTER (property_name));
	else
	  error_at (location, "property %qs already specified in %<@synthesize%>",
		    IDENTIFIER_POINTER (property_name));

	if (original_location != UNKNOWN_LOCATION)
	  inform (original_location, "originally specified here");
	return;
      }

  /* Check that the property is declared in the interface.  It could
     also be declared in a superclass or protocol.  */
  property = lookup_property (interface, property_name);

  if (!property)
    {
      error_at (location, "no declaration of property %qs found in the interface",
		IDENTIFIER_POINTER (property_name));
      return;
    }
  else
    {
      /* We have to copy the property, because we want to chain it to
	 the implementation context, and we want to store the source
	 location of the @synthesize, not of the original
	 @property.  */
      property = copy_node (property);
      DECL_SOURCE_LOCATION (property) = location;
    }

  /* Determine PROPERTY_IVAR_NAME.  */
  if (ivar_name == NULL_TREE)
    ivar_name = property_name;

  /* Check that the instance variable exists.  You can only use an
     instance variable from the same class, not one from the
     superclass (this makes sense as it allows us to check that an
     instance variable is only used in one synthesized property).  */
  {
    tree ivar = is_ivar (CLASS_IVARS (interface), ivar_name);
    tree type_of_ivar;
    if (!ivar)
      {
	error_at (location, "ivar %qs used by %<@synthesize%> declaration must be an existing ivar",
		  IDENTIFIER_POINTER (property_name));
	return;
      }

    if (DECL_BIT_FIELD_TYPE (ivar))
      type_of_ivar = DECL_BIT_FIELD_TYPE (ivar);
    else
      type_of_ivar = TREE_TYPE (ivar);

    /* If the instance variable has a different C type, we throw an error ...  */
    if (!comptypes (TREE_TYPE (property), type_of_ivar)
	/* ... unless the property is readonly, in which case we allow
	   the instance variable to be more specialized (this means we
	   can generate the getter all right and it works).  */
	&& (!PROPERTY_READONLY (property)
	    || !objc_compare_types (TREE_TYPE (property),
				    type_of_ivar, -5, NULL_TREE)))
      {
	location_t original_location = DECL_SOURCE_LOCATION (ivar);

	error_at (location, "property %qs is using instance variable %qs of incompatible type",
		  IDENTIFIER_POINTER (property_name),
		  IDENTIFIER_POINTER (ivar_name));

	if (original_location != UNKNOWN_LOCATION)
	  inform (original_location, "originally specified here");
      }

    /* If the instance variable is a bitfield, the property must be
       'assign', 'nonatomic' because the runtime getter/setter helper
       do not work with bitfield instance variables.  */
    if (DECL_BIT_FIELD_TYPE (ivar))
      {
	/* If there is an error, we return and not generate any
	   getter/setter because trying to set up the runtime
	   getter/setter helper calls with bitfields is at high risk
	   of ICE.  */

	if (PROPERTY_ASSIGN_SEMANTICS (property) != OBJC_PROPERTY_ASSIGN)
	  {
	    location_t original_location = DECL_SOURCE_LOCATION (ivar);

	    error_at (location, "'assign' property %qs is using bit-field instance variable %qs",
		      IDENTIFIER_POINTER (property_name),
		      IDENTIFIER_POINTER (ivar_name));

	    if (original_location != UNKNOWN_LOCATION)
	      inform (original_location, "originally specified here");
	    return;
	  }

	if (!PROPERTY_NONATOMIC (property))
	  {
	    location_t original_location = DECL_SOURCE_LOCATION (ivar);

	    error_at (location, "'atomic' property %qs is using bit-field instance variable %qs",
		      IDENTIFIER_POINTER (property_name),
		      IDENTIFIER_POINTER (ivar_name));

	    if (original_location != UNKNOWN_LOCATION)
	      inform (original_location, "originally specified here");
	    return;
	  }
      }
  }

  /* Check that no other property is using the same instance
     variable.  */
  for (x = IMPL_PROPERTY_DECL (objc_implementation_context); x; x = TREE_CHAIN (x))
    if (PROPERTY_IVAR_NAME (x) == ivar_name)
      {
	location_t original_location = DECL_SOURCE_LOCATION (x);

	error_at (location, "property %qs is using the same instance variable as property %qs",
		  IDENTIFIER_POINTER (property_name),
		  IDENTIFIER_POINTER (PROPERTY_NAME (x)));

	if (original_location != UNKNOWN_LOCATION)
	  inform (original_location, "originally specified here");

	/* We keep going on.  This won't cause the compiler to fail;
	   the failure would most likely be at runtime.  */
      }

  /* Note that a @synthesize (and only a @synthesize) always sets
     PROPERTY_IVAR_NAME to a non-NULL_TREE.  You can recognize a
     @synthesize by that.  */
  PROPERTY_IVAR_NAME (property) = ivar_name;

  /* PROPERTY_SETTER_NAME and PROPERTY_GETTER_NAME are copied from the
     original declaration; they are always set (with the exception of
     PROPERTY_SETTER_NAME not being set if PROPERTY_READONLY == 1).  */

  /* Add the property to the list of properties for current implementation. */
  TREE_CHAIN (property) = IMPL_PROPERTY_DECL (objc_implementation_context);
  IMPL_PROPERTY_DECL (objc_implementation_context) = property;

  /* Note how we don't actually synthesize the getter/setter here; it
     would be very natural, but we may miss the fact that the user has
     implemented his own getter/setter later on in the @implementation
     (in which case we shouldn't generate getter/setter).  We wait
     until we have parsed it all before generating the code.  */
}

/* This function is called by the parser after a @synthesize
   expression is parsed.  'location' is the location of the
   @synthesize expression, and 'property_and_ivar_list' is a chained
   list of the property and ivar names.  */
void
objc_add_synthesize_declaration (location_t location, tree property_and_ivar_list)
{
  tree interface, chain;

  if (flag_objc1_only)
    error_at (input_location, "%<@synthesize%> is not available in Objective-C 1.0");

  if (property_and_ivar_list == error_mark_node)
    return;

  if (!objc_implementation_context)
    {
      /* We can get here only in Objective-C; the Objective-C++ parser
	 detects the problem while parsing, outputs the error
	 "misplaced '@synthesize' Objective-C++ construct" and skips
	 the declaration.  */
      error_at (location, "%<@synthesize%> not in @implementation context");
      return;
    }

  if (TREE_CODE (objc_implementation_context) == CATEGORY_IMPLEMENTATION_TYPE)
    {
      error_at (location, "%<@synthesize%> can not be used in categories");
      return;
    }

  interface = lookup_interface (CLASS_NAME (objc_implementation_context));
  if (!interface)
    {
      /* I can't see how this could happen, but it is good as a safety check.  */
      error_at (location,
		"%<@synthesize%> requires the @interface of the class to be available");
      return;
    }

  /* Now, iterate over the properties and do each of them.  */
  for (chain = property_and_ivar_list; chain; chain = TREE_CHAIN (chain))
    {
      objc_add_synthesize_declaration_for_property (location, interface, TREE_VALUE (chain),
						    TREE_PURPOSE (chain));
    }
}

/* This function is a sub-routine of objc_add_dynamic_declaration.  It
   is called for each property to mark as dynamic once we have
   determined that the context is Ok.  */
static void
objc_add_dynamic_declaration_for_property (location_t location, tree interface,
					   tree property_name)
{
  /* Find the @property declaration.  */
  tree property;

  /* Check that synthesize or dynamic has not already been used for
     the same property.  */
  for (property = IMPL_PROPERTY_DECL (objc_implementation_context); property; property = TREE_CHAIN (property))
    if (PROPERTY_NAME (property) == property_name)
      {
	location_t original_location = DECL_SOURCE_LOCATION (property);

	if (PROPERTY_DYNAMIC (property))
	  error_at (location, "property %qs already specified in %<@dynamic%>",
		    IDENTIFIER_POINTER (property_name));
	else
	  error_at (location, "property %qs already specified in %<@synthesize%>",
		    IDENTIFIER_POINTER (property_name));

	if (original_location != UNKNOWN_LOCATION)
	  inform (original_location, "originally specified here");
	return;
      }

  /* Check that the property is declared in the interface.  It could
     also be declared in a superclass or protocol.  */
  property = lookup_property (interface, property_name);

  if (!property)
    {
      error_at (location, "no declaration of property %qs found in the interface",
		IDENTIFIER_POINTER (property_name));
      return;
    }
  else
    {
      /* We have to copy the property, because we want to chain it to
	 the implementation context, and we want to store the source
	 location of the @synthesize, not of the original
	 @property.  */
      property = copy_node (property);
      DECL_SOURCE_LOCATION (property) = location;
    }

  /* Note that a @dynamic (and only a @dynamic) always sets
     PROPERTY_DYNAMIC to 1.  You can recognize a @dynamic by that.
     (actually, as explained above, PROPERTY_DECL generated by
     @property and associated with a @dynamic property are also marked
     as PROPERTY_DYNAMIC).  */
  PROPERTY_DYNAMIC (property) = 1;

  /* Add the property to the list of properties for current implementation. */
  TREE_CHAIN (property) = IMPL_PROPERTY_DECL (objc_implementation_context);
  IMPL_PROPERTY_DECL (objc_implementation_context) = property;
}

/* This function is called by the parser after a @dynamic expression
   is parsed.  'location' is the location of the @dynamic expression,
   and 'property_list' is a chained list of all the property
   names.  */
void
objc_add_dynamic_declaration (location_t location, tree property_list)
{
  tree interface, chain;

  if (flag_objc1_only)
    error_at (input_location, "%<@dynamic%> is not available in Objective-C 1.0");

  if (property_list == error_mark_node)
    return;

  if (!objc_implementation_context)
    {
      /* We can get here only in Objective-C; the Objective-C++ parser
	 detects the problem while parsing, outputs the error
	 "misplaced '@dynamic' Objective-C++ construct" and skips the
	 declaration.  */
      error_at (location, "%<@dynamic%> not in @implementation context");
      return;
    }

  /* @dynamic is allowed in categories.  */
  switch (TREE_CODE (objc_implementation_context))
    {
    case CLASS_IMPLEMENTATION_TYPE:
      interface = lookup_interface (CLASS_NAME (objc_implementation_context));
      break;
    case CATEGORY_IMPLEMENTATION_TYPE:
      interface = lookup_category (implementation_template,
				   CLASS_SUPER_NAME (objc_implementation_context));
      break;
    default:
      gcc_unreachable ();
    }

  if (!interface)
    {
      /* I can't see how this could happen, but it is good as a safety check.  */
      error_at (location,
		"%<@dynamic%> requires the @interface of the class to be available");
      return;
    }

  /* Now, iterate over the properties and do each of them.  */
  for (chain = property_list; chain; chain = TREE_CHAIN (chain))
    {
      objc_add_dynamic_declaration_for_property (location, interface, TREE_VALUE (chain));
    }
}

/* Main routine to generate code/data for all the property information for
   current implementation (class or category). CLASS is the interface where
   ivars are declared.  CLASS_METHODS is where methods are found which
   could be a class or a category depending on whether we are implementing
   property of a class or a category.  */

static void
objc_gen_property_data (tree klass, tree class_methods)
{
  tree x;

  for (x = IMPL_PROPERTY_DECL (objc_implementation_context); x; x = TREE_CHAIN (x))
    {
      /* @dynamic property - nothing to check or synthesize.  */
      if (PROPERTY_DYNAMIC (x))
	continue;

      /* @synthesize property - need to synthesize the accessors.  */
      if (PROPERTY_IVAR_NAME (x))
	{
	  objc_synthesize_getter (klass, class_methods, x);

	  if (PROPERTY_READONLY (x) == 0)
	    objc_synthesize_setter (klass, class_methods, x);

	  continue;
	}

      gcc_unreachable ();
    }
}

/* This is called once we see the "@end" in an interface/implementation.  */

static void
finish_class (tree klass)
{
  switch (TREE_CODE (klass))
    {
    case CLASS_IMPLEMENTATION_TYPE:
      {
	/* All metadata generation is done in runtime.generate_metadata().  */

	/* Generate what needed for property; setters, getters, etc. */
	objc_gen_property_data (implementation_template, implementation_template);

	if (implementation_template != objc_implementation_context)
	  {
	    /* Ensure that all method listed in the interface contain bodies.  */
	    check_methods (CLASS_CLS_METHODS (implementation_template),
			   objc_implementation_context, '+');
	    check_methods (CLASS_NST_METHODS (implementation_template),
			   objc_implementation_context, '-');

	    if (CLASS_PROTOCOL_LIST (implementation_template))
	      check_protocols (CLASS_PROTOCOL_LIST (implementation_template),
			       "class",
			       CLASS_NAME (objc_implementation_context));
	  }
	break;
      }
    case CATEGORY_IMPLEMENTATION_TYPE:
      {
	tree category = lookup_category (implementation_template, CLASS_SUPER_NAME (klass));

	if (category)
	  {
	    /* Generate what needed for property; setters, getters, etc. */
	    objc_gen_property_data (implementation_template, category);

	    /* Ensure all method listed in the interface contain bodies.  */
	    check_methods (CLASS_CLS_METHODS (category),
			   objc_implementation_context, '+');
	    check_methods (CLASS_NST_METHODS (category),
			   objc_implementation_context, '-');

	    if (CLASS_PROTOCOL_LIST (category))
	      check_protocols (CLASS_PROTOCOL_LIST (category),
			       "category",
			       CLASS_SUPER_NAME (objc_implementation_context));
	  }
	break;
      }
    case CLASS_INTERFACE_TYPE:
    case CATEGORY_INTERFACE_TYPE:
    case PROTOCOL_INTERFACE_TYPE:
      {
	/* Process properties of the class. */
	tree x;
	for (x = CLASS_PROPERTY_DECL (objc_interface_context); x; x = TREE_CHAIN (x))
	  {
	    /* Now we check that the appropriate getter is declared,
	       and if not, we declare one ourselves.  */
	    tree getter_decl = lookup_method (CLASS_NST_METHODS (klass),
					      PROPERTY_GETTER_NAME (x));

	    if (getter_decl)
	      {
		/* TODO: Check that the declaration is consistent with the property.  */
		;
	      }
	    else
	      {
		/* Generate an instance method declaration for the
		   getter; for example "- (id) name;".  In general it
		   will be of the form
		   -(type)property_getter_name;  */
		tree rettype = build_tree_list (NULL_TREE, TREE_TYPE (x));
		getter_decl = build_method_decl (INSTANCE_METHOD_DECL,
						 rettype, PROPERTY_GETTER_NAME (x),
						 NULL_TREE, false);
		if (PROPERTY_OPTIONAL (x))
		  objc_add_method (objc_interface_context, getter_decl, false, true);
		else
		  objc_add_method (objc_interface_context, getter_decl, false, false);
		TREE_DEPRECATED (getter_decl) = TREE_DEPRECATED (x);
		METHOD_PROPERTY_CONTEXT (getter_decl) = x;
	      }

	    if (PROPERTY_READONLY (x) == 0)
	      {
		/* Now we check that the appropriate setter is declared,
		   and if not, we declare on ourselves.  */
		tree setter_decl = lookup_method (CLASS_NST_METHODS (klass),
						  PROPERTY_SETTER_NAME (x));

		if (setter_decl)
		  {
		    /* TODO: Check that the declaration is consistent with the property.  */
		    ;
		  }
		else
		  {
		    /* The setter name is something like 'setName:'.
		       We need the substring 'setName' to build the
		       method declaration due to how the declaration
		       works.  TODO: build_method_decl() will then
		       generate back 'setName:' from 'setName'; it
		       would be more efficient to hook into there.  */
		    const char *full_setter_name = IDENTIFIER_POINTER (PROPERTY_SETTER_NAME (x));
		    size_t length = strlen (full_setter_name);
		    char *setter_name = (char *) alloca (length);
		    tree ret_type, selector, arg_type, arg_name;

		    strcpy (setter_name, full_setter_name);
		    setter_name[length - 1] = '\0';
		    ret_type = build_tree_list (NULL_TREE, void_type_node);
		    arg_type = build_tree_list (NULL_TREE, TREE_TYPE (x));
		    arg_name = get_identifier ("_value");
		    selector = objc_build_keyword_decl (get_identifier (setter_name),
							arg_type, arg_name, NULL);
		    setter_decl = build_method_decl (INSTANCE_METHOD_DECL,
						     ret_type, selector,
						     build_tree_list (NULL_TREE, NULL_TREE),
						     false);
		    if (PROPERTY_OPTIONAL (x))
		      objc_add_method (objc_interface_context, setter_decl, false, true);
		    else
		      objc_add_method (objc_interface_context, setter_decl, false, false);
		    TREE_DEPRECATED (setter_decl) = TREE_DEPRECATED (x);
		    METHOD_PROPERTY_CONTEXT (setter_decl) = x;
		  }
	      }
	  }
	break;
      }
    default:
      gcc_unreachable ();
      break;
    }
}

static tree
add_protocol (tree protocol)
{
  /* Put protocol on list in reverse order.  */
  TREE_CHAIN (protocol) = protocol_chain;
  protocol_chain = protocol;
  return protocol_chain;
}

/* Check that a protocol is defined, and, recursively, that all
   protocols that this protocol conforms to are defined too.  */
static void
check_that_protocol_is_defined (tree protocol)
{
  if (!PROTOCOL_DEFINED (protocol))
    warning (0, "definition of protocol %qE not found",
	     PROTOCOL_NAME (protocol));

  /* If the protocol itself conforms to other protocols, check them
     too, recursively.  */
  if (PROTOCOL_LIST (protocol))
    {
      tree p;

      for (p = PROTOCOL_LIST (protocol); p; p = TREE_CHAIN (p))
	check_that_protocol_is_defined (TREE_VALUE (p));
    }
}

/* Looks up a protocol.  If 'warn_if_deprecated' is true, a warning is
   emitted if the protocol is deprecated.  If 'definition_required' is
   true, a warning is emitted if a full @protocol definition has not
   been seen.  */
static tree
lookup_protocol (tree ident, bool warn_if_deprecated, bool definition_required)
{
  tree chain;

  for (chain = protocol_chain; chain; chain = TREE_CHAIN (chain))
    if (ident == PROTOCOL_NAME (chain))
      {
	if (warn_if_deprecated && TREE_DEPRECATED (chain))
	  {
	    /* It would be nice to use warn_deprecated_use() here, but
	       we are using TREE_CHAIN (which is supposed to be the
	       TYPE_STUB_DECL for a TYPE) for something different.  */
	    warning (OPT_Wdeprecated_declarations, "protocol %qE is deprecated",
		     PROTOCOL_NAME (chain));
	  }

	if (definition_required)
	  check_that_protocol_is_defined (chain);

	return chain;
      }

  return NULL_TREE;
}

/* This function forward declares the protocols named by NAMES.  If
   they are already declared or defined, the function has no effect.  */

void
objc_declare_protocol (tree name, tree attributes)
{
  bool deprecated = false;

#ifdef OBJCPLUS
  if (current_namespace != global_namespace) {
    error ("Objective-C declarations may only appear in global scope");
  }
#endif /* OBJCPLUS */

  /* Determine if 'deprecated', the only attribute we recognize for
     protocols, was used.  Ignore all other attributes.  */
  if (attributes)
    {
      tree attribute;
      for (attribute = attributes; attribute; attribute = TREE_CHAIN (attribute))
	{
	  tree name = TREE_PURPOSE (attribute);

	  if (is_attribute_p  ("deprecated", name))
	    deprecated = true;
	  else
	    warning (OPT_Wattributes, "%qE attribute directive ignored", name);
	}
    }

  if (lookup_protocol (name, /* warn if deprecated */ false,
		       /* definition_required */ false) == NULL_TREE)
    {
      tree protocol = make_node (PROTOCOL_INTERFACE_TYPE);

      TYPE_LANG_SLOT_1 (protocol)
	= make_tree_vec (PROTOCOL_LANG_SLOT_ELTS);
      PROTOCOL_NAME (protocol) = name;
      PROTOCOL_LIST (protocol) = NULL_TREE;
      add_protocol (protocol);
      PROTOCOL_DEFINED (protocol) = 0;
      PROTOCOL_FORWARD_DECL (protocol) = NULL_TREE;

      if (attributes)
	{
	  /* TODO: Do we need to store the attributes here ? */
	  TYPE_ATTRIBUTES (protocol) = attributes;
	  if (deprecated)
	    TREE_DEPRECATED (protocol) = 1;
	}
    }
}

static tree
start_protocol (enum tree_code code, tree name, tree list, tree attributes)
{
  tree protocol;
  bool deprecated = false;

#ifdef OBJCPLUS
  if (current_namespace != global_namespace) {
    error ("Objective-C declarations may only appear in global scope");
  }
#endif /* OBJCPLUS */

  /* Determine if 'deprecated', the only attribute we recognize for
     protocols, was used.  Ignore all other attributes.  */
  if (attributes)
    {
      tree attribute;
      for (attribute = attributes; attribute; attribute = TREE_CHAIN (attribute))
	{
	  tree name = TREE_PURPOSE (attribute);

	  if (is_attribute_p  ("deprecated", name))
	    deprecated = true;
	  else
	    warning (OPT_Wattributes, "%qE attribute directive ignored", name);
	}
    }

  protocol = lookup_protocol (name, /* warn_if_deprecated */ false,
			      /* definition_required */ false);

  if (!protocol)
    {
      protocol = make_node (code);
      TYPE_LANG_SLOT_1 (protocol) = make_tree_vec (PROTOCOL_LANG_SLOT_ELTS);

      PROTOCOL_NAME (protocol) = name;
      PROTOCOL_LIST (protocol) = lookup_and_install_protocols (list, /* definition_required */ false);
      add_protocol (protocol);
      PROTOCOL_DEFINED (protocol) = 1;
      PROTOCOL_FORWARD_DECL (protocol) = NULL_TREE;

      check_protocol_recursively (protocol, list);
    }
  else if (! PROTOCOL_DEFINED (protocol))
    {
      PROTOCOL_DEFINED (protocol) = 1;
      PROTOCOL_LIST (protocol) = lookup_and_install_protocols (list, /* definition_required */ false);

      check_protocol_recursively (protocol, list);
    }
  else
    {
      warning (0, "duplicate declaration for protocol %qE",
	       name);
    }

  if (attributes)
    {
      TYPE_ATTRIBUTES (protocol) = attributes;
      if (deprecated)
	TREE_DEPRECATED (protocol) = 1;
    }

  return protocol;
}

/* Decay array and function parameters into pointers.  */

static tree
objc_decay_parm_type (tree type)
{
  if (TREE_CODE (type) == ARRAY_TYPE || TREE_CODE (type) == FUNCTION_TYPE)
    type = build_pointer_type (TREE_CODE (type) == ARRAY_TYPE
			       ? TREE_TYPE (type)
			       : type);

  return type;
}

static GTY(()) tree objc_parmlist = NULL_TREE;

/* Append PARM to a list of formal parameters of a method, making a necessary
   array-to-pointer adjustment along the way.  */

void
objc_push_parm (tree parm)
{
  tree type;

  if (TREE_TYPE (parm) == error_mark_node)
    {
      objc_parmlist = chainon (objc_parmlist, parm);
      return;
    }

  /* Decay arrays and functions into pointers.  */
  type = objc_decay_parm_type (TREE_TYPE (parm));

  /* If the parameter type has been decayed, a new PARM_DECL needs to be
     built as well.  */
  if (type != TREE_TYPE (parm))
    parm = build_decl (input_location, PARM_DECL, DECL_NAME (parm), type);

  DECL_ARG_TYPE (parm)
    = lang_hooks.types.type_promotes_to (TREE_TYPE (parm));

  /* Record constancy and volatility.  */
  c_apply_type_quals_to_decl
  ((TYPE_READONLY (TREE_TYPE (parm)) ? TYPE_QUAL_CONST : 0)
   | (TYPE_RESTRICT (TREE_TYPE (parm)) ? TYPE_QUAL_RESTRICT : 0)
   | (TYPE_ATOMIC (TREE_TYPE (parm)) ? TYPE_QUAL_ATOMIC : 0)
   | (TYPE_VOLATILE (TREE_TYPE (parm)) ? TYPE_QUAL_VOLATILE : 0), parm);

  objc_parmlist = chainon (objc_parmlist, parm);
}

/* Retrieve the formal parameter list constructed via preceding calls to
   objc_push_parm().  */

#ifdef OBJCPLUS
tree
objc_get_parm_info (int have_ellipsis ATTRIBUTE_UNUSED,
		    tree expr ATTRIBUTE_UNUSED)
{
  tree parm_info = objc_parmlist;
  objc_parmlist = NULL_TREE;

  return parm_info;
}
#else
struct c_arg_info *
objc_get_parm_info (int have_ellipsis, tree expr)
{
  tree parm_info = objc_parmlist;
  struct c_arg_info *arg_info;
  /* The C front-end requires an elaborate song and dance at
     this point.  */
  push_scope ();
  declare_parm_level ();
  while (parm_info)
    {
      tree next = DECL_CHAIN (parm_info);

      DECL_CHAIN (parm_info) = NULL_TREE;
      parm_info = pushdecl (parm_info);
      finish_decl (parm_info, input_location, NULL_TREE, NULL_TREE, NULL_TREE);
      parm_info = next;
    }
  arg_info = get_parm_info (have_ellipsis, expr);
  pop_scope ();
  objc_parmlist = NULL_TREE;
  return arg_info;
}
#endif

/* Synthesize the formal parameters 'id self' and 'SEL _cmd' needed for ObjC
   method definitions.  In the case of instance methods, we can be more
   specific as to the type of 'self'.  */

static void
synth_self_and_ucmd_args (void)
{
  tree self_type;

  if (objc_method_context
      && TREE_CODE (objc_method_context) == INSTANCE_METHOD_DECL)
    self_type = objc_instance_type;
  else
    /* Really a `struct objc_class *'. However, we allow people to
       assign to self, which changes its type midstream.  */
    self_type = objc_object_type;

  /* id self; */
  objc_push_parm (build_decl (input_location,
			      PARM_DECL, self_id, self_type));

  /* SEL _cmd; */
  objc_push_parm (build_decl (input_location,
			      PARM_DECL, ucmd_id, objc_selector_type));
}

/* Transform an Objective-C method definition into a static C function
   definition, synthesizing the first two arguments, "self" and "_cmd",
   in the process.  EXPR is NULL or an expression that needs to be
   evaluated for the side effects of array size expressions in the
   parameters.  */

static void
start_method_def (tree method, tree expr)
{
  tree parmlist;
#ifdef OBJCPLUS
  tree parm_info;
#else
  struct c_arg_info *parm_info;
#endif
  int have_ellipsis = 0;

  /* If we are defining a "dealloc" method in a non-root class, we
     will need to check if a [super dealloc] is missing, and warn if
     it is.  */
  if(CLASS_SUPER_NAME (objc_implementation_context)
     && !strcmp ("dealloc", IDENTIFIER_POINTER (METHOD_SEL_NAME (method))))
    should_call_super_dealloc = 1;
  else
    should_call_super_dealloc = 0;

  /* Required to implement _msgSuper.  */
  objc_method_context = method;
  UOBJC_SUPER_decl = NULL_TREE;

  /* Generate prototype declarations for arguments..."new-style".  */
  synth_self_and_ucmd_args ();

  /* Generate argument declarations if a keyword_decl.  */
  parmlist = METHOD_SEL_ARGS (method);
  while (parmlist)
    {
      /* parmlist is a KEYWORD_DECL.  */
      tree type = TREE_VALUE (TREE_TYPE (parmlist));
      tree parm;

      parm = build_decl (input_location,
			 PARM_DECL, KEYWORD_ARG_NAME (parmlist), type);
      decl_attributes (&parm, DECL_ATTRIBUTES (parmlist), 0);
      objc_push_parm (parm);
      parmlist = DECL_CHAIN (parmlist);
    }

  if (METHOD_ADD_ARGS (method))
    {
      tree akey;

      for (akey = TREE_CHAIN (METHOD_ADD_ARGS (method));
	   akey; akey = TREE_CHAIN (akey))
	{
	  objc_push_parm (TREE_VALUE (akey));
	}

      if (METHOD_ADD_ARGS_ELLIPSIS_P (method))
	have_ellipsis = 1;
    }

  parm_info = objc_get_parm_info (have_ellipsis, expr);

  really_start_method (objc_method_context, parm_info);
}

/* Return 1 if TYPE1 is equivalent to TYPE2 for purposes of method
   overloading.  */
static int
objc_types_are_equivalent (tree type1, tree type2)
{
  if (type1 == type2)
    return 1;

  /* Strip away indirections.  */
  while ((TREE_CODE (type1) == ARRAY_TYPE || TREE_CODE (type1) == POINTER_TYPE)
	 && (TREE_CODE (type1) == TREE_CODE (type2)))
    type1 = TREE_TYPE (type1), type2 = TREE_TYPE (type2);
  if (TYPE_MAIN_VARIANT (type1) != TYPE_MAIN_VARIANT (type2))
    return 0;

  /* Compare the protocol lists.  */
  type1 = (TYPE_HAS_OBJC_INFO (type1)
	   ? TYPE_OBJC_PROTOCOL_LIST (type1)
	   : NULL_TREE);
  type2 = (TYPE_HAS_OBJC_INFO (type2)
	   ? TYPE_OBJC_PROTOCOL_LIST (type2)
	   : NULL_TREE);

  /* If there are no protocols (most common case), the types are
     identical.  */
  if (type1 == NULL_TREE && type2 == NULL_TREE)
    return 1;

  /* If one has protocols, and the other one hasn't, they are not
     identical.  */
  if ((type1 == NULL_TREE && type2 != NULL_TREE)
      || (type1 != NULL_TREE && type2 == NULL_TREE))
    return 0;
  else
    {
      /* Else, both have protocols, and we need to do the full
	 comparison.  It is possible that either type1 or type2
	 contain some duplicate protocols in the list, so we can't
	 even just compare list_length as a first check.  */
      tree t;

      for (t = type2; t; t = TREE_CHAIN (t))
	if (!lookup_protocol_in_reflist (type1, TREE_VALUE (t)))
	  return 0;

      for (t = type1; t; t = TREE_CHAIN (t))
	if (!lookup_protocol_in_reflist (type2, TREE_VALUE (t)))
	  return 0;

      return 1;
    }
}

/* Return 1 if TYPE1 has the same size and alignment as TYPE2.  */

static int
objc_types_share_size_and_alignment (tree type1, tree type2)
{
  return (simple_cst_equal (TYPE_SIZE (type1), TYPE_SIZE (type2))
	  && TYPE_ALIGN (type1) == TYPE_ALIGN (type2));
}

/* Return 1 if PROTO1 is equivalent to PROTO2
   for purposes of method overloading.  Ordinarily, the type signatures
   should match up exactly, unless STRICT is zero, in which case we
   shall allow differences in which the size and alignment of a type
   is the same.  */

static int
comp_proto_with_proto (tree proto1, tree proto2, int strict)
{
  tree type1, type2;

  /* The following test is needed in case there are hashing
     collisions.  */
  if (METHOD_SEL_NAME (proto1) != METHOD_SEL_NAME (proto2))
    return 0;

  /* Compare return types.  */
  type1 = TREE_VALUE (TREE_TYPE (proto1));
  type2 = TREE_VALUE (TREE_TYPE (proto2));

  if (!objc_types_are_equivalent (type1, type2)
      && (strict || !objc_types_share_size_and_alignment (type1, type2)))
    return 0;

  /* Compare argument types.  */

  /* The first argument (objc_object_type) is always the same, no need
     to compare.  */

  /* The second argument (objc_selector_type) is always the same, no
     need to compare.  */

  /* Compare the other arguments.  */
  {
    tree arg1, arg2;

    /* Compare METHOD_SEL_ARGS.  */
    for (arg1 = METHOD_SEL_ARGS (proto1), arg2 = METHOD_SEL_ARGS (proto2);
	 arg1 && arg2;
	 arg1 = DECL_CHAIN (arg1), arg2 = DECL_CHAIN (arg2))
      {
	type1 = TREE_VALUE (TREE_TYPE (arg1));
	type2 = TREE_VALUE (TREE_TYPE (arg2));

	/* FIXME: Do we need to decay argument types to compare them ?  */
	type1 = objc_decay_parm_type (type1);
	type2 = objc_decay_parm_type (type2);

	if (!objc_types_are_equivalent (type1, type2)
	    && (strict || !objc_types_share_size_and_alignment (type1, type2)))
	  return 0;
      }

    /* The loop ends when arg1 or arg2 are NULL.  Make sure they are
       both NULL.  */
    if (arg1 != arg2)
      return 0;

    /* Compare METHOD_ADD_ARGS.  */
    if ((METHOD_ADD_ARGS (proto1) && !METHOD_ADD_ARGS (proto2))
	|| (METHOD_ADD_ARGS (proto2) && !METHOD_ADD_ARGS (proto1)))
      return 0;

    if (METHOD_ADD_ARGS (proto1))
      {
	for (arg1 = TREE_CHAIN (METHOD_ADD_ARGS (proto1)), arg2 = TREE_CHAIN (METHOD_ADD_ARGS (proto2));
	     arg1 && arg2;
	     arg1 = TREE_CHAIN (arg1), arg2 = TREE_CHAIN (arg2))
	  {
	    type1 = TREE_TYPE (TREE_VALUE (arg1));
	    type2 = TREE_TYPE (TREE_VALUE (arg2));

	    /* FIXME: Do we need to decay argument types to compare them ?  */
	    type1 = objc_decay_parm_type (type1);
	    type2 = objc_decay_parm_type (type2);

	    if (!objc_types_are_equivalent (type1, type2)
		&& (strict || !objc_types_share_size_and_alignment (type1, type2)))
	      return 0;
	  }
      }

    /* The loop ends when arg1 or arg2 are NULL.  Make sure they are
       both NULL.  */
    if (arg1 != arg2)
      return 0;

    /* Compare METHOD_ADD_ARGS_ELLIPSIS_P.  */
    if (METHOD_ADD_ARGS_ELLIPSIS_P (proto1) != METHOD_ADD_ARGS_ELLIPSIS_P (proto2))
      return 0;
  }

  /* Success.  */
  return 1;
}

/* This routine returns true if TYPE is a valid objc object type,
   suitable for messaging; false otherwise.  If 'accept_class' is
   'true', then a Class object is considered valid for messaging and
   'true' is returned if 'type' refers to a Class.  If 'accept_class'
   is 'false', then a Class object is not considered valid for
   messaging and 'false' is returned in that case.  */

static bool
objc_type_valid_for_messaging (tree type, bool accept_classes)
{
  if (!POINTER_TYPE_P (type))
    return false;

  /* Remove the pointer indirection; don't remove more than one
     otherwise we'd consider "NSObject **" a valid type for messaging,
     which it isn't.  */
  type = TREE_TYPE (type);

  if (TREE_CODE (type) != RECORD_TYPE)
    return false;

  if (objc_is_object_id (type))
    return true;

  if (objc_is_class_id (type))
    return accept_classes;

  if (TYPE_HAS_OBJC_INFO (type))
    return true;

  return false;
}

void
objc_start_function (tree name, tree type, tree attrs,
#ifdef OBJCPLUS
		     tree params
#else
		     struct c_arg_info *params
#endif
		     )
{
  tree fndecl = build_decl (input_location,
			    FUNCTION_DECL, name, type);

#ifdef OBJCPLUS
  DECL_ARGUMENTS (fndecl) = params;
  DECL_INITIAL (fndecl) = error_mark_node;
  DECL_EXTERNAL (fndecl) = 0;
  TREE_STATIC (fndecl) = 1;
  retrofit_lang_decl (fndecl);
  cplus_decl_attributes (&fndecl, attrs, 0);
  start_preparsed_function (fndecl, attrs, /*flags=*/SF_DEFAULT);
#else
  current_function_returns_value = 0;  /* Assume, until we see it does.  */
  current_function_returns_null = 0;
  decl_attributes (&fndecl, attrs, 0);
  announce_function (fndecl);
  DECL_INITIAL (fndecl) = error_mark_node;
  DECL_EXTERNAL (fndecl) = 0;
  TREE_STATIC (fndecl) = 1;
  current_function_decl = pushdecl (fndecl);
  push_scope ();
  declare_parm_level ();
  DECL_RESULT (current_function_decl)
    = build_decl (input_location,
		  RESULT_DECL, NULL_TREE,
		  TREE_TYPE (TREE_TYPE (current_function_decl)));
  DECL_ARTIFICIAL (DECL_RESULT (current_function_decl)) = 1;
  DECL_IGNORED_P (DECL_RESULT (current_function_decl)) = 1;
  start_fname_decls ();
  store_parm_decls_from (params);
#endif

  TREE_USED (current_function_decl) = 1;
}

/* - Generate an identifier for the function. the format is "_n_cls",
     where 1 <= n <= nMethods, and cls is the name the implementation we
     are processing.
   - Install the return type from the method declaration.
   - If we have a prototype, check for type consistency.  */

static void
really_start_method (tree method,
#ifdef OBJCPLUS
		     tree parmlist
#else
		     struct c_arg_info *parmlist
#endif
		     )
{
  tree ret_type, meth_type;
  tree method_id;
  const char *sel_name, *class_name, *cat_name;
  char *buf;

  /* Synth the storage class & assemble the return type.  */
  ret_type = TREE_VALUE (TREE_TYPE (method));

  sel_name = IDENTIFIER_POINTER (METHOD_SEL_NAME (method));
  class_name = IDENTIFIER_POINTER (CLASS_NAME (objc_implementation_context));
  cat_name = ((TREE_CODE (objc_implementation_context)
	       == CLASS_IMPLEMENTATION_TYPE)
	      ? NULL
	      : IDENTIFIER_POINTER (CLASS_SUPER_NAME (objc_implementation_context)));
  method_slot++;

  /* Make sure this is big enough for any plausible method label.  */
  buf = (char *) alloca (50 + strlen (sel_name) + strlen (class_name)
			 + (cat_name ? strlen (cat_name) : 0));

  OBJC_GEN_METHOD_LABEL (buf, TREE_CODE (method) == INSTANCE_METHOD_DECL,
			 class_name, cat_name, sel_name, method_slot);

  method_id = get_identifier (buf);

#ifdef OBJCPLUS
  /* Objective-C methods cannot be overloaded, so we don't need
     the type encoding appended.  It looks bad anyway... */
  push_lang_context (lang_name_c);
#endif

  meth_type = build_function_type_for_method (ret_type, method, METHOD_DEF, 0);
  objc_start_function (method_id, meth_type, NULL_TREE, parmlist);

  /* Set self_decl from the first argument.  */
  self_decl = DECL_ARGUMENTS (current_function_decl);

  /* Suppress unused warnings.  */
  TREE_USED (self_decl) = 1;
  DECL_READ_P (self_decl) = 1;
  TREE_USED (DECL_CHAIN (self_decl)) = 1;
  DECL_READ_P (DECL_CHAIN (self_decl)) = 1;
#ifdef OBJCPLUS
  pop_lang_context ();
#endif

  METHOD_DEFINITION (method) = current_function_decl;

  /* Check consistency...start_function, pushdecl, duplicate_decls.  */

  if (implementation_template != objc_implementation_context)
    {
      tree proto
	= lookup_method_static (implementation_template,
				METHOD_SEL_NAME (method),
				((TREE_CODE (method) == CLASS_METHOD_DECL)
				 | OBJC_LOOKUP_NO_SUPER));

      if (proto)
	{
	  if (!comp_proto_with_proto (method, proto, 1))
	    {
	      bool type = TREE_CODE (method) == INSTANCE_METHOD_DECL;

	      warning_at (DECL_SOURCE_LOCATION (method), 0,
			  "conflicting types for %<%c%s%>",
			  (type ? '-' : '+'),
			  identifier_to_locale (gen_method_decl (method)));
	      inform (DECL_SOURCE_LOCATION (proto),
		      "previous declaration of %<%c%s%>",
		      (type ? '-' : '+'),
		      identifier_to_locale (gen_method_decl (proto)));
	    }
	  else
	    {
	      /* If the method in the @interface was deprecated, mark
		 the implemented method as deprecated too.  It should
		 never be used for messaging (when the deprecation
		 warnings are produced), but just in case.  */
	      if (TREE_DEPRECATED (proto))
		TREE_DEPRECATED (method) = 1;

	      /* If the method in the @interface was marked as
		 'noreturn', mark the function implementing the method
		 as 'noreturn' too.  */
	      TREE_THIS_VOLATILE (current_function_decl) = TREE_THIS_VOLATILE (proto);
	    }
	}
      else
	{
	  /* We have a method @implementation even though we did not
	     see a corresponding @interface declaration (which is allowed
	     by Objective-C rules).  Go ahead and place the method in
	     the @interface anyway, so that message dispatch lookups
	     will see it.  */
	  tree interface = implementation_template;

	  if (TREE_CODE (objc_implementation_context)
	      == CATEGORY_IMPLEMENTATION_TYPE)
	    interface = lookup_category
			(interface,
			 CLASS_SUPER_NAME (objc_implementation_context));

	  if (interface)
	    objc_add_method (interface, copy_node (method),
			     TREE_CODE (method) == CLASS_METHOD_DECL,
			     /* is_optional= */ false);
	}
    }
}

static void *UOBJC_SUPER_scope = 0;

/* _n_Method (id self, SEL sel, ...)
     {
       struct objc_super _S;
       _msgSuper ((_S.self = self, _S.class = _cls, &_S), ...);
     }  */

static tree
get_super_receiver (void)
{
  if (objc_method_context)
    {
      tree super_expr, super_expr_list, class_expr;
      bool inst_meth;
      if (!UOBJC_SUPER_decl)
      {
	UOBJC_SUPER_decl = build_decl (input_location,
				       VAR_DECL, get_identifier (TAG_SUPER),
				       objc_super_template);
	/* This prevents `unused variable' warnings when compiling with -Wall.  */
	TREE_USED (UOBJC_SUPER_decl) = 1;
	DECL_READ_P (UOBJC_SUPER_decl) = 1;
	lang_hooks.decls.pushdecl (UOBJC_SUPER_decl);
        finish_decl (UOBJC_SUPER_decl, input_location, NULL_TREE, NULL_TREE,
		     NULL_TREE);
	UOBJC_SUPER_scope = objc_get_current_scope ();
      }

      /* Set receiver to self.  */
      super_expr = objc_build_component_ref (UOBJC_SUPER_decl, self_id);
      super_expr = build_modify_expr (input_location, super_expr, NULL_TREE,
				      NOP_EXPR, input_location, self_decl,
				      NULL_TREE);
      super_expr_list = super_expr;

      /* Set class to begin searching.  */
      /* Get the ident for the superclass class field & build a ref to it.
         ??? maybe we should just name the field the same for all runtimes.  */
      super_expr = (*runtime.super_superclassfield_ident) ();
      super_expr = objc_build_component_ref (UOBJC_SUPER_decl, super_expr);

      gcc_assert (imp_list->imp_context == objc_implementation_context
		  && imp_list->imp_template == implementation_template);
      inst_meth = (TREE_CODE (objc_method_context) == INSTANCE_METHOD_DECL);

      if (TREE_CODE (objc_implementation_context) == CLASS_IMPLEMENTATION_TYPE)
	class_expr =  (*runtime.get_class_super_ref) (input_location,
						      imp_list, inst_meth);
      else
	/* We have a category.  */
	{
	  tree super_name = CLASS_SUPER_NAME (imp_list->imp_template);
	  tree super_class;

	  /* Barf if super used in a category of a root object.  */
	  if (!super_name)
	    {
	      error ("no super class declared in interface for %qE",
		     CLASS_NAME (imp_list->imp_template));
	      return error_mark_node;
	    }

	  super_class = (*runtime.get_category_super_ref) (input_location,
							   imp_list, inst_meth);
	  class_expr = build_c_cast (input_location,
				     TREE_TYPE (super_expr), super_class);
	}

      super_expr = build_modify_expr (input_location, super_expr, NULL_TREE,
				      NOP_EXPR,
				      input_location, class_expr, NULL_TREE);

      super_expr_list = build_compound_expr (input_location,
					     super_expr_list, super_expr);

      super_expr = build_unary_op (input_location,
				   ADDR_EXPR, UOBJC_SUPER_decl, 0);
      super_expr_list = build_compound_expr (input_location,
					     super_expr_list, super_expr);

      return super_expr_list;
    }
  else
    {
      error ("[super ...] must appear in a method context");
      return error_mark_node;
    }
}

/* When exiting a scope, sever links to a 'super' declaration (if any)
   therein contained.  */

void
objc_clear_super_receiver (void)
{
  if (objc_method_context
      && UOBJC_SUPER_scope == objc_get_current_scope ())
    {
      UOBJC_SUPER_decl = 0;
      UOBJC_SUPER_scope = 0;
    }
}

void
objc_finish_method_definition (tree fndecl)
{
  /* We cannot validly inline ObjC methods, at least not without a language
     extension to declare that a method need not be dynamically
     dispatched, so suppress all thoughts of doing so.  */
  DECL_UNINLINABLE (fndecl) = 1;

#ifndef OBJCPLUS
  /* The C++ front-end will have called finish_function() for us.  */
  finish_function ();
#endif

  METHOD_ENCODING (objc_method_context)
    = encode_method_prototype (objc_method_context);

  /* Required to implement _msgSuper. This must be done AFTER finish_function,
     since the optimizer may find "may be used before set" errors.  */
  objc_method_context = NULL_TREE;

  if (should_call_super_dealloc)
    warning (0, "method possibly missing a [super dealloc] call");
}

/* Given a tree DECL node, produce a printable description of it in the given
   buffer, overwriting the buffer.  */

static char *
gen_declaration (tree decl)
{
  errbuf[0] = '\0';

  if (DECL_P (decl))
    {
      gen_type_name_0 (TREE_TYPE (decl));

      if (DECL_NAME (decl))
	{
	  if (!POINTER_TYPE_P (TREE_TYPE (decl)))
	    strcat (errbuf, " ");

	  strcat (errbuf, IDENTIFIER_POINTER (DECL_NAME (decl)));
	}

      if (DECL_INITIAL (decl)
	  && TREE_CODE (DECL_INITIAL (decl)) == INTEGER_CST)
	sprintf (errbuf + strlen (errbuf), ": " HOST_WIDE_INT_PRINT_DEC,
		 TREE_INT_CST_LOW (DECL_INITIAL (decl)));
    }

  return errbuf;
}

/* Given a tree TYPE node, produce a printable description of it in the given
   buffer, overwriting the buffer.  */

static char *
gen_type_name_0 (tree type)
{
  tree orig = type, proto;

  if (TYPE_P (type) && TYPE_NAME (type))
    type = TYPE_NAME (type);
  else if (POINTER_TYPE_P (type) || TREE_CODE (type) == ARRAY_TYPE)
    {
      tree inner = TREE_TYPE (type);

      while (TREE_CODE (inner) == ARRAY_TYPE)
	inner = TREE_TYPE (inner);

      gen_type_name_0 (inner);

      if (!POINTER_TYPE_P (inner))
	strcat (errbuf, " ");

      if (POINTER_TYPE_P (type))
	strcat (errbuf, "*");
      else
	while (type != inner)
	  {
	    strcat (errbuf, "[");

	    if (TYPE_DOMAIN (type))
	      {
		char sz[20];

		sprintf (sz, HOST_WIDE_INT_PRINT_DEC,
			 (TREE_INT_CST_LOW
			  (TYPE_MAX_VALUE (TYPE_DOMAIN (type))) + 1));
		strcat (errbuf, sz);
	      }

	    strcat (errbuf, "]");
	    type = TREE_TYPE (type);
	  }

      goto exit_function;
    }

  if (TREE_CODE (type) == TYPE_DECL && DECL_NAME (type))
    type = DECL_NAME (type);

  strcat (errbuf, TREE_CODE (type) == IDENTIFIER_NODE
		  ? IDENTIFIER_POINTER (type)
		  : "");

  /* For 'id' and 'Class', adopted protocols are stored in the pointee.  */
  if (objc_is_id (orig))
    orig = TREE_TYPE (orig);

  proto = TYPE_HAS_OBJC_INFO (orig) ? TYPE_OBJC_PROTOCOL_LIST (orig) : NULL_TREE;

  if (proto)
    {
      strcat (errbuf, " <");

      while (proto) {
	strcat (errbuf,
		IDENTIFIER_POINTER (PROTOCOL_NAME (TREE_VALUE (proto))));
	proto = TREE_CHAIN (proto);
	strcat (errbuf, proto ? ", " : ">");
      }
    }

 exit_function:
  return errbuf;
}

static char *
gen_type_name (tree type)
{
  errbuf[0] = '\0';

  return gen_type_name_0 (type);
}

/* Given a method tree, put a printable description into the given
   buffer (overwriting) and return a pointer to the buffer.  */

static char *
gen_method_decl (tree method)
{
  tree chain;

  strcpy (errbuf, "(");  /* NB: Do _not_ call strcat() here.  */
  gen_type_name_0 (TREE_VALUE (TREE_TYPE (method)));
  strcat (errbuf, ")");
  chain = METHOD_SEL_ARGS (method);

  if (chain)
    {
      /* We have a chain of keyword_decls.  */
      do
        {
	  if (KEYWORD_KEY_NAME (chain))
	    strcat (errbuf, IDENTIFIER_POINTER (KEYWORD_KEY_NAME (chain)));

	  strcat (errbuf, ":(");
	  gen_type_name_0 (TREE_VALUE (TREE_TYPE (chain)));
	  strcat (errbuf, ")");

	  strcat (errbuf, IDENTIFIER_POINTER (KEYWORD_ARG_NAME (chain)));
	  if ((chain = DECL_CHAIN (chain)))
	    strcat (errbuf, " ");
        }
      while (chain);

      if (METHOD_ADD_ARGS (method))
	{
	  chain = TREE_CHAIN (METHOD_ADD_ARGS (method));

	  /* Know we have a chain of parm_decls.  */
	  while (chain)
	    {
	      strcat (errbuf, ", ");
	      gen_type_name_0 (TREE_TYPE (TREE_VALUE (chain)));
	      chain = TREE_CHAIN (chain);
	    }

	  if (METHOD_ADD_ARGS_ELLIPSIS_P (method))
	    strcat (errbuf, ", ...");
	}
    }

  else
    /* We have a unary selector.  */
    strcat (errbuf, IDENTIFIER_POINTER (METHOD_SEL_NAME (method)));

  return errbuf;
}

/* Debug info.  */


/* Dump an @interface declaration of the supplied class CHAIN to the
   supplied file FP.  Used to implement the -gen-decls option (which
   prints out an @interface declaration of all classes compiled in
   this run); potentially useful for debugging the compiler too.  */
void
dump_interface (FILE *fp, tree chain)
{
  /* FIXME: A heap overflow here whenever a method (or ivar)
     declaration is so long that it doesn't fit in the buffer.  The
     code and all the related functions should be rewritten to avoid
     using fixed size buffers.  */
  const char *my_name = IDENTIFIER_POINTER (CLASS_NAME (chain));
  tree ivar_decls = CLASS_RAW_IVARS (chain);
  tree nst_methods = CLASS_NST_METHODS (chain);
  tree cls_methods = CLASS_CLS_METHODS (chain);

  fprintf (fp, "\n@interface %s", my_name);

  /* CLASS_SUPER_NAME is used to store the superclass name for
     classes, and the category name for categories.  */
  if (CLASS_SUPER_NAME (chain))
    {
      const char *name = IDENTIFIER_POINTER (CLASS_SUPER_NAME (chain));

      switch (TREE_CODE (chain))
	{
	case CATEGORY_IMPLEMENTATION_TYPE:
	case CATEGORY_INTERFACE_TYPE:
	  fprintf (fp, " (%s)\n", name);
	  break;
	default:
	  fprintf (fp, " : %s\n", name);
	  break;
	}
    }
  else
    fprintf (fp, "\n");

  /* FIXME - the following doesn't seem to work at the moment.  */
  if (ivar_decls)
    {
      fprintf (fp, "{\n");
      do
	{
	  fprintf (fp, "\t%s;\n", gen_declaration (ivar_decls));
	  ivar_decls = TREE_CHAIN (ivar_decls);
	}
      while (ivar_decls);
      fprintf (fp, "}\n");
    }

  while (nst_methods)
    {
      fprintf (fp, "- %s;\n", gen_method_decl (nst_methods));
      nst_methods = TREE_CHAIN (nst_methods);
    }

  while (cls_methods)
    {
      fprintf (fp, "+ %s;\n", gen_method_decl (cls_methods));
      cls_methods = TREE_CHAIN (cls_methods);
    }

  fprintf (fp, "@end\n");
}

#if 0
/* Produce the pretty printing for an Objective-C method.  This is
   currently unused, but could be handy while reorganizing the pretty
   printing to be more robust.  */
static const char *
objc_pretty_print_method (bool is_class_method,
			  const char *class_name,
			  const char *category_name,
			  const char *selector)
{
  if (category_name)
    {
      char *result = XNEWVEC (char, strlen (class_name) + strlen (category_name)
			      + strlen (selector) + 7);

      if (is_class_method)
	sprintf (result, "+[%s(%s) %s]", class_name, category_name, selector);
      else
	sprintf (result, "-[%s(%s) %s]", class_name, category_name, selector);

      return result;
    }
  else
    {
      char *result = XNEWVEC (char, strlen (class_name)
			      + strlen (selector) + 5);

      if (is_class_method)
	sprintf (result, "+[%s %s]", class_name, selector);
      else
	sprintf (result, "-[%s %s]", class_name, selector);

      return result;
    }
}
#endif

/* Demangle function for Objective-C.  Attempt to demangle the
   function name associated with a method (eg, going from
   "_i_NSObject__class" to "-[NSObject class]"); usually for the
   purpose of pretty printing or error messages.  Return the demangled
   name, or NULL if the string is not an Objective-C mangled method
   name.

   Because of how the mangling is done, any method that has a '_' in
   its original name is at risk of being demangled incorrectly.  In
   some cases there are multiple valid ways to demangle a method name
   and there is no way we can decide.

   TODO: objc_demangle() can't always get it right; the right way to
   get this correct for all method names would be to store the
   Objective-C method name somewhere in the function decl.  Then,
   there is no demangling to do; we'd just pull the method name out of
   the decl.  As an additional bonus, when printing error messages we
   could check for such a method name, and if we find it, we know the
   function is actually an Objective-C method and we could print error
   messages saying "In method '+[NSObject class]" instead of "In
   function '+[NSObject class]" as we do now.  */
static const char *
objc_demangle (const char *mangled)
{
  char *demangled, *cp;

  /* First of all, if the name is too short it can't be an Objective-C
     mangled method name.  */
  if (mangled[0] == '\0' || mangled[1] == '\0' || mangled[2] == '\0')
    return NULL;

  /* If the name looks like an already demangled one, return it
     unchanged.  This should only happen on Darwin, where method names
     are mangled differently into a pretty-print form (such as
     '+[NSObject class]', see darwin.h).  In that case, demangling is
     a no-op, but we need to return the demangled name if it was an
     ObjC one, and return NULL if not.  We should be safe as no C/C++
     function can start with "-[" or "+[".  */
  if ((mangled[0] == '-' || mangled[0] == '+')
      && (mangled[1] == '['))
    return mangled;

  if (mangled[0] == '_' &&
      (mangled[1] == 'i' || mangled[1] == 'c') &&
      mangled[2] == '_')
    {
      cp = demangled = XNEWVEC (char, strlen(mangled) + 2);
      if (mangled[1] == 'i')
	*cp++ = '-';            /* for instance method */
      else
	*cp++ = '+';            /* for class method */
      *cp++ = '[';              /* opening left brace */
      strcpy(cp, mangled+3);    /* tack on the rest of the mangled name */
      while (*cp && *cp == '_')
	cp++;                   /* skip any initial underbars in class name */
      cp = strchr(cp, '_');     /* find first non-initial underbar */
      if (cp == NULL)
	{
	  free(demangled);      /* not mangled name */
	  return NULL;
	}
      if (cp[1] == '_')  /* easy case: no category name */
	{
	  *cp++ = ' ';            /* replace two '_' with one ' ' */
	  strcpy(cp, mangled + (cp - demangled) + 2);
	}
      else
	{
	  *cp++ = '(';            /* less easy case: category name */
	  cp = strchr(cp, '_');
	  if (cp == 0)
	    {
	      free(demangled);    /* not mangled name */
	      return NULL;
	    }
	  *cp++ = ')';
	  *cp++ = ' ';            /* overwriting 1st char of method name... */
	  strcpy(cp, mangled + (cp - demangled)); /* get it back */
	}
      /* Now we have the method name.  We need to generally replace
	 '_' with ':' but trying to preserve '_' if it could only have
	 been in the mangled string because it was already in the
	 original name.  In cases where it's ambiguous, we assume that
	 any '_' originated from a ':'.  */

      /* Initial '_'s in method name can't have been generating by
	 converting ':'s.  Skip them.  */
      while (*cp && *cp == '_')
	cp++;

      /* If the method name does not end with '_', then it has no
	 arguments and there was no replacement of ':'s with '_'s
	 during mangling.  Check for that case, and skip any
	 replacement if so.  This at least guarantees that methods
	 with no arguments are always demangled correctly (unless the
	 original name ends with '_').  */
      if (*(mangled + strlen (mangled) - 1) != '_')
	{
	  /* Skip to the end.  */
	  for (; *cp; cp++)
	    ;
	}
      else
	{
	  /* Replace remaining '_' with ':'.  This may get it wrong if
	     there were '_'s in the original name.  In most cases it
	     is impossible to disambiguate.  */
	  for (; *cp; cp++)
	    if (*cp == '_')
	      *cp = ':';
	}
      *cp++ = ']';              /* closing right brace */
      *cp++ = 0;                /* string terminator */
      return demangled;
    }
  else
    return NULL;             /* not an objc mangled name */
}

/* Try to pretty-print a decl.  If the 'decl' is an Objective-C
   specific decl, return the printable name for it.  If not, return
   NULL.  */
const char *
objc_maybe_printable_name (tree decl, int v ATTRIBUTE_UNUSED)
{
  switch (TREE_CODE (decl))
    {
    case FUNCTION_DECL:
      return objc_demangle (IDENTIFIER_POINTER (DECL_NAME (decl)));
      break;

      /* The following happens when we are printing a deprecation
	 warning for a method.  The warn_deprecation() will end up
	 trying to print the decl for INSTANCE_METHOD_DECL or
	 CLASS_METHOD_DECL.  It would be nice to be able to print
	 "-[NSObject autorelease] is deprecated", but to do that, we'd
	 need to store the class and method name in the method decl,
	 which we currently don't do.  For now, just return the name
	 of the method.  We don't return NULL, because that may
	 trigger further attempts to pretty-print the decl in C/C++,
	 but they wouldn't know how to pretty-print it.  */
    case INSTANCE_METHOD_DECL:
    case CLASS_METHOD_DECL:
      return IDENTIFIER_POINTER (DECL_NAME (decl));
      break;
      /* This happens when printing a deprecation warning for a
	 property.  We may want to consider some sort of pretty
	 printing (eg, include the class name where it was declared
	 ?).  */
    case PROPERTY_DECL:
      return IDENTIFIER_POINTER (PROPERTY_NAME (decl));
      break;
    default:
      return NULL;
      break;
    }
}

/* Return a printable name for 'decl'.  This first tries
   objc_maybe_printable_name(), and if that fails, it returns the name
   in the decl.  This is used as LANG_HOOKS_DECL_PRINTABLE_NAME for
   Objective-C; in Objective-C++, setting the hook is not enough
   because lots of C++ Front-End code calls cxx_printable_name,
   dump_decl and other C++ functions directly.  So instead we have
   modified dump_decl to call objc_maybe_printable_name directly.  */
const char *
objc_printable_name (tree decl, int v)
{
  const char *demangled_name = objc_maybe_printable_name (decl, v);

  if (demangled_name != NULL)
    return demangled_name;
  else
    return IDENTIFIER_POINTER (DECL_NAME (decl));
}

/* Routine is called to issue diagnostic when reference to a private
   ivar is made and no other variable with same name is found in
   current scope.  */
bool
objc_diagnose_private_ivar (tree id)
{
  tree ivar;
  if (!objc_method_context)
    return false;
  ivar = is_ivar (objc_ivar_chain, id);
  if (ivar && is_private (ivar))
    {
      error ("instance variable %qs is declared private",
	     IDENTIFIER_POINTER (id));
      return true;
    }
  return false;
}

/* Look up ID as an instance variable.  OTHER contains the result of
   the C or C++ lookup, which we may want to use instead.  */
/* To use properties inside an instance method, use self.property.  */
tree
objc_lookup_ivar (tree other, tree id)
{
  tree ivar;

  /* If we are not inside of an ObjC method, ivar lookup makes no sense.  */
  if (!objc_method_context)
    return other;

  if (!strcmp (IDENTIFIER_POINTER (id), "super"))
    /* We have a message to super.  */
    return get_super_receiver ();

  /* In a class method, look up an instance variable only as a last
     resort.  */
  if (TREE_CODE (objc_method_context) == CLASS_METHOD_DECL
      && other && other != error_mark_node)
    return other;

  /* Don't look up the ivar if the user has explicitly advised against
     it with -fno-local-ivars.  */

  if (!flag_local_ivars)
    return other;

  /* Look up the ivar, but do not use it if it is not accessible.  */
  ivar = is_ivar (objc_ivar_chain, id);

  if (!ivar || is_private (ivar))
    return other;

  /* In an instance method, a local variable (or parameter) may hide the
     instance variable.  */
  if (TREE_CODE (objc_method_context) == INSTANCE_METHOD_DECL
      && other && other != error_mark_node
#ifdef OBJCPLUS
      && CP_DECL_CONTEXT (other) != global_namespace)
#else
      && !DECL_FILE_SCOPE_P (other))
#endif
    {
      if (warn_shadow_ivar == 1 || (warn_shadow && warn_shadow_ivar != 0)) {
          warning (warn_shadow_ivar ? OPT_Wshadow_ivar : OPT_Wshadow,
                   "local declaration of %qE hides instance variable", id);
      }
        
      return other;
    }

  /* At this point, we are either in an instance method with no obscuring
     local definitions, or in a class method with no alternate definitions
     at all.  */
  return build_ivar_reference (id);
}

/* Possibly rewrite a function CALL into an OBJ_TYPE_REF expression.  This
   needs to be done if we are calling a function through a cast.  */

tree
objc_rewrite_function_call (tree function, tree first_param)
{
  if (TREE_CODE (function) == NOP_EXPR
      && TREE_CODE (TREE_OPERAND (function, 0)) == ADDR_EXPR
      && TREE_CODE (TREE_OPERAND (TREE_OPERAND (function, 0), 0))
	 == FUNCTION_DECL)
    {
      function = build3 (OBJ_TYPE_REF, TREE_TYPE (function),
			 TREE_OPERAND (function, 0),
			 first_param, size_zero_node);
    }

  return function;
}

/* This is called to "gimplify" a PROPERTY_REF node.  It builds the
   corresponding 'getter' function call.  Note that we assume the
   PROPERTY_REF to be valid since we generated it while parsing.  */
static void
objc_gimplify_property_ref (tree *expr_p)
{
  tree getter = PROPERTY_REF_GETTER_CALL (*expr_p);
  tree call_exp;

  if (getter == NULL_TREE)
    {
      tree property_decl = PROPERTY_REF_PROPERTY_DECL (*expr_p);
      /* This can happen if DECL_ARTIFICIAL (*expr_p), but
	 should be impossible for real properties, which always
	 have a getter.  */
      error_at (EXPR_LOCATION (*expr_p), "no %qs getter found",
		IDENTIFIER_POINTER (PROPERTY_NAME (property_decl)));
      /* Try to recover from the error to prevent an ICE.  We take
	 zero and cast it to the type of the property.  */
      *expr_p = convert (TREE_TYPE (property_decl),
			 integer_zero_node);
      return;
    }

  if (PROPERTY_REF_DEPRECATED_GETTER (*expr_p))
    {
      /* PROPERTY_REF_DEPRECATED_GETTER contains the method prototype
	 that is deprecated.  */
      warn_deprecated_use (PROPERTY_REF_DEPRECATED_GETTER (*expr_p),
			   NULL_TREE);
    }

  call_exp = getter;
#ifdef OBJCPLUS
  /* In C++, a getter which returns an aggregate value results in a
     target_expr which initializes a temporary to the call
     expression.  */
  if (TREE_CODE (getter) == TARGET_EXPR)
    {
      gcc_assert (MAYBE_CLASS_TYPE_P (TREE_TYPE (getter)));
      gcc_assert (TREE_CODE (TREE_OPERAND (getter, 0)) == VAR_DECL);
      call_exp = TREE_OPERAND (getter, 1);
    }
#endif
  gcc_assert (TREE_CODE (call_exp) == CALL_EXPR);

  *expr_p = call_exp;
}

/* This is called when "gimplifying" the trees.  We need to gimplify
   the Objective-C/Objective-C++ specific trees, then hand over the
   process to C/C++.  */
int
objc_gimplify_expr (tree *expr_p, gimple_seq *pre_p, gimple_seq *post_p)
{
  enum tree_code code = TREE_CODE (*expr_p);
  switch (code)
    {
      /* Look for the special case of OBJC_TYPE_REF with the address
	 of a function in OBJ_TYPE_REF_EXPR (presumably objc_msgSend
	 or one of its cousins).  */
    case OBJ_TYPE_REF:
      if (TREE_CODE (OBJ_TYPE_REF_EXPR (*expr_p)) == ADDR_EXPR
	  && TREE_CODE (TREE_OPERAND (OBJ_TYPE_REF_EXPR (*expr_p), 0))
	  == FUNCTION_DECL)
	{
	  enum gimplify_status r0, r1;

	  /* Postincrements in OBJ_TYPE_REF_OBJECT don't affect the
	     value of the OBJ_TYPE_REF, so force them to be emitted
	     during subexpression evaluation rather than after the
	     OBJ_TYPE_REF. This permits objc_msgSend calls in
	     Objective C to use direct rather than indirect calls when
	     the object expression has a postincrement.  */
	  r0 = gimplify_expr (&OBJ_TYPE_REF_OBJECT (*expr_p), pre_p, NULL,
			      is_gimple_val, fb_rvalue);
	  r1 = gimplify_expr (&OBJ_TYPE_REF_EXPR (*expr_p), pre_p, post_p,
			      is_gimple_val, fb_rvalue);

	  return MIN (r0, r1);
	}
      break;
    case PROPERTY_REF:
      objc_gimplify_property_ref (expr_p);
      /* Do not return yet; let C/C++ gimplify the resulting expression.  */
      break;
    default:
      break;
    }

#ifdef OBJCPLUS
  return (enum gimplify_status) cp_gimplify_expr (expr_p, pre_p, post_p);
#else
  return (enum gimplify_status) c_gimplify_expr (expr_p, pre_p, post_p);
#endif
}

/* --- FAST ENUMERATION --- */
/* Begin code generation for fast enumeration (foreach) ... */

/* Defines

  struct __objcFastEnumerationState
   {
     unsigned long state;
     id            *itemsPtr;
     unsigned long *mutationsPtr;
     unsigned long extra[5];
   };

   Confusingly enough, NSFastEnumeration is then defined by libraries
   to be the same structure.
*/

static void
build_fast_enumeration_state_template (void)
{
  tree decls, *chain = NULL;

  /* { */
  objc_fast_enumeration_state_template = objc_start_struct (get_identifier
							    (TAG_FAST_ENUMERATION_STATE));

  /* unsigned long state; */
  decls = add_field_decl (long_unsigned_type_node, "state", &chain);

  /* id            *itemsPtr; */
  add_field_decl (build_pointer_type (objc_object_type),
		  "itemsPtr", &chain);

  /* unsigned long *mutationsPtr; */
  add_field_decl (build_pointer_type (long_unsigned_type_node),
		  "mutationsPtr", &chain);

  /* unsigned long extra[5]; */
  add_field_decl (build_sized_array_type (long_unsigned_type_node, 5),
		  "extra", &chain);

  /* } */
  objc_finish_struct (objc_fast_enumeration_state_template, decls);
}

/*
  'objc_finish_foreach_loop()' generates the code for an Objective-C
  foreach loop.  The 'location' argument is the location of the 'for'
  that starts the loop.  The 'object_expression' is the expression of
  the 'object' that iterates; the 'collection_expression' is the
  expression of the collection that we iterate over (we need to make
  sure we evaluate this only once); the 'for_body' is the set of
  statements to be executed in each iteration; 'break_label' and
  'continue_label' are the break and continue labels which we need to
  emit since the <statements> may be jumping to 'break_label' (if they
  contain 'break') or to 'continue_label' (if they contain
  'continue').

  The syntax is

  for (<object expression> in <collection expression>)
    <statements>

  which is compiled into the following blurb:

  {
    id __objc_foreach_collection;
    __objc_fast_enumeration_state __objc_foreach_enum_state;
    unsigned long __objc_foreach_batchsize;
    id __objc_foreach_items[16];
    __objc_foreach_collection = <collection expression>;
    __objc_foreach_enum_state = { 0 };
    __objc_foreach_batchsize = [__objc_foreach_collection countByEnumeratingWithState: &__objc_foreach_enum_state  objects: __objc_foreach_items  count: 16];

    if (__objc_foreach_batchsize == 0)
      <object expression> = nil;
    else
      {
	unsigned long __objc_foreach_mutations_pointer = *__objc_foreach_enum_state.mutationsPtr;
        next_batch:
	  {
	    unsigned long __objc_foreach_index;
            __objc_foreach_index = 0;

            next_object:
	    if (__objc_foreach_mutation_pointer != *__objc_foreach_enum_state.mutationsPtr) objc_enumeration_mutation (<collection expression>);
	    <object expression> = enumState.itemsPtr[__objc_foreach_index];
	    <statements> [PS: inside <statments>, 'break' jumps to break_label and 'continue' jumps to continue_label]

            continue_label:
            __objc_foreach_index++;
            if (__objc_foreach_index < __objc_foreach_batchsize) goto next_object;
	    __objc_foreach_batchsize = [__objc_foreach_collection countByEnumeratingWithState: &__objc_foreach_enum_state  objects: __objc_foreach_items  count: 16];
         }
       if (__objc_foreach_batchsize != 0) goto next_batch;
       <object expression> = nil;
       break_label:
      }
  }

  'statements' may contain a 'continue' or 'break' instruction, which
  the user expects to 'continue' or 'break' the entire foreach loop.
  We are provided the labels that 'break' and 'continue' jump to, so
  we place them where we want them to jump to when they pick them.

  Optimization TODO: we could cache the IMP of
  countByEnumeratingWithState:objects:count:.
*/

/* If you need to debug objc_finish_foreach_loop(), uncomment the following line.  */
/* #define DEBUG_OBJC_FINISH_FOREACH_LOOP 1 */

#ifdef DEBUG_OBJC_FINISH_FOREACH_LOOP
#include "tree-pretty-print.h"
#endif

void
objc_finish_foreach_loop (location_t location, tree object_expression, tree collection_expression, tree for_body,
			  tree break_label, tree continue_label)
{
  /* A tree representing the __objcFastEnumerationState struct type,
     or NSFastEnumerationState struct, whatever we are using.  */
  tree objc_fast_enumeration_state_type;

  /* The trees representing the declarations of each of the local variables.  */
  tree objc_foreach_collection_decl;
  tree objc_foreach_enum_state_decl;
  tree objc_foreach_items_decl;
  tree objc_foreach_batchsize_decl;
  tree objc_foreach_mutations_pointer_decl;
  tree objc_foreach_index_decl;

  /* A tree representing the selector countByEnumeratingWithState:objects:count:.  */
  tree selector_name;

  /* A tree representing the local bind.  */
  tree bind;

  /* A tree representing the external 'if (__objc_foreach_batchsize)' */
  tree first_if;

  /* A tree representing the 'else' part of 'first_if'  */
  tree first_else;

  /* A tree representing the 'next_batch' label.  */
  tree next_batch_label_decl;

  /* A tree representing the binding after the 'next_batch' label.  */
  tree next_batch_bind;

  /* A tree representing the 'next_object' label.  */
  tree next_object_label_decl;

  /* Temporary variables.  */
  tree t;
  int i;

  if (flag_objc1_only)
    error_at (location, "fast enumeration is not available in Objective-C 1.0");

  if (object_expression == error_mark_node)
    return;

  if (collection_expression == error_mark_node)
    return;

  if (!objc_type_valid_for_messaging (TREE_TYPE (object_expression), true))
    {
      error_at (location, "iterating variable in fast enumeration is not an object");
      return;
    }

  if (!objc_type_valid_for_messaging (TREE_TYPE (collection_expression), true))
    {
      error_at (location, "collection in fast enumeration is not an object");
      return;
    }

  /* TODO: Check that object_expression is either a variable
     declaration, or an lvalue.  */

  /* This kludge is an idea from apple.  We use the
     __objcFastEnumerationState struct implicitly defined by the
     compiler, unless a NSFastEnumerationState struct has been defined
     (by a Foundation library such as GNUstep Base) in which case, we
     use that one.
  */
  objc_fast_enumeration_state_type = objc_fast_enumeration_state_template;
  {
    tree objc_NSFastEnumeration_type = lookup_name (get_identifier ("NSFastEnumerationState"));

    if (objc_NSFastEnumeration_type)
      {
	/* TODO: We really need to check that
	   objc_NSFastEnumeration_type is the same as ours!  */
	if (TREE_CODE (objc_NSFastEnumeration_type) == TYPE_DECL)
	  {
	    /* If it's a typedef, use the original type.  */
	    if (DECL_ORIGINAL_TYPE (objc_NSFastEnumeration_type))
	      objc_fast_enumeration_state_type = DECL_ORIGINAL_TYPE (objc_NSFastEnumeration_type);
	    else
	      objc_fast_enumeration_state_type = TREE_TYPE (objc_NSFastEnumeration_type);
	  }
      }
  }

  /* { */
  /* Done by c-parser.c.  */

  /* type object; */
  /* Done by c-parser.c.  */

  /* Disable warnings that 'object' is unused.  For example the code

     for (id object in collection)
       i++;

     which can be used to count how many objects there are in the
     collection is fine and should generate no warnings even if
     'object' is technically unused.  */
  TREE_USED (object_expression) = 1;
  if (DECL_P (object_expression))
    DECL_READ_P (object_expression) = 1;

  /*  id __objc_foreach_collection */
  objc_foreach_collection_decl = objc_create_temporary_var (objc_object_type, "__objc_foreach_collection");

  /*  __objcFastEnumerationState __objc_foreach_enum_state; */
  objc_foreach_enum_state_decl = objc_create_temporary_var (objc_fast_enumeration_state_type, "__objc_foreach_enum_state");
  TREE_CHAIN (objc_foreach_enum_state_decl) = objc_foreach_collection_decl;

  /* id __objc_foreach_items[16]; */
  objc_foreach_items_decl = objc_create_temporary_var (build_sized_array_type (objc_object_type, 16), "__objc_foreach_items");
  TREE_CHAIN (objc_foreach_items_decl) = objc_foreach_enum_state_decl;

  /* unsigned long __objc_foreach_batchsize; */
  objc_foreach_batchsize_decl = objc_create_temporary_var (long_unsigned_type_node, "__objc_foreach_batchsize");
  TREE_CHAIN (objc_foreach_batchsize_decl) = objc_foreach_items_decl;

  /* Generate the local variable binding.  */
  bind = build3 (BIND_EXPR, void_type_node, objc_foreach_batchsize_decl, NULL, NULL);
  SET_EXPR_LOCATION (bind, location);
  TREE_SIDE_EFFECTS (bind) = 1;

  /*  __objc_foreach_collection = <collection expression>; */
  t = build2 (MODIFY_EXPR, void_type_node, objc_foreach_collection_decl, collection_expression);
  SET_EXPR_LOCATION (t, location);
  append_to_statement_list (t, &BIND_EXPR_BODY (bind));
  /* We have used 'collection_expression'.  */
  mark_exp_read (collection_expression);

  /*  __objc_foreach_enum_state.state = 0; */
  t = build2 (MODIFY_EXPR, void_type_node, objc_build_component_ref (objc_foreach_enum_state_decl,
								     get_identifier ("state")),
	      build_int_cst (long_unsigned_type_node, 0));
  SET_EXPR_LOCATION (t, location);
  append_to_statement_list (t, &BIND_EXPR_BODY (bind));

  /*  __objc_foreach_enum_state.itemsPtr = NULL; */
  t = build2 (MODIFY_EXPR, void_type_node, objc_build_component_ref (objc_foreach_enum_state_decl,
								     get_identifier ("itemsPtr")),
	      null_pointer_node);
  SET_EXPR_LOCATION (t, location);
  append_to_statement_list (t, &BIND_EXPR_BODY (bind));

  /*  __objc_foreach_enum_state.mutationsPtr = NULL; */
  t = build2 (MODIFY_EXPR, void_type_node, objc_build_component_ref (objc_foreach_enum_state_decl,
								     get_identifier ("mutationsPtr")),
	      null_pointer_node);
  SET_EXPR_LOCATION (t, location);
  append_to_statement_list (t, &BIND_EXPR_BODY (bind));

  /*  __objc_foreach_enum_state.extra[0] = 0; */
  /*  __objc_foreach_enum_state.extra[1] = 0; */
  /*  __objc_foreach_enum_state.extra[2] = 0; */
  /*  __objc_foreach_enum_state.extra[3] = 0; */
  /*  __objc_foreach_enum_state.extra[4] = 0; */
  for (i = 0; i < 5 ; i++)
    {
      t = build2 (MODIFY_EXPR, void_type_node,
		  build_array_ref (location, objc_build_component_ref (objc_foreach_enum_state_decl,
								       get_identifier ("extra")),
				   build_int_cst (NULL_TREE, i)),
		  build_int_cst (long_unsigned_type_node, 0));
      SET_EXPR_LOCATION (t, location);
      append_to_statement_list (t, &BIND_EXPR_BODY (bind));
    }

  /* __objc_foreach_batchsize = [__objc_foreach_collection countByEnumeratingWithState: &__objc_foreach_enum_state  objects: __objc_foreach_items  count: 16]; */
  selector_name = get_identifier ("countByEnumeratingWithState:objects:count:");
#ifdef OBJCPLUS
  t = objc_finish_message_expr (objc_foreach_collection_decl, selector_name,
				/* Parameters.  */
				tree_cons    /* &__objc_foreach_enum_state */
				(NULL_TREE, build_fold_addr_expr_loc (location, objc_foreach_enum_state_decl),
				 tree_cons   /* __objc_foreach_items  */
				 (NULL_TREE, objc_foreach_items_decl,
				  tree_cons  /* 16 */
				  (NULL_TREE, build_int_cst (NULL_TREE, 16), NULL_TREE))), NULL);
#else
  /* In C, we need to decay the __objc_foreach_items array that we are passing.  */
  {
    struct c_expr array;
    array.value = objc_foreach_items_decl;
    t = objc_finish_message_expr (objc_foreach_collection_decl, selector_name,
				  /* Parameters.  */
				  tree_cons    /* &__objc_foreach_enum_state */
				  (NULL_TREE, build_fold_addr_expr_loc (location, objc_foreach_enum_state_decl),
				   tree_cons   /* __objc_foreach_items  */
				   (NULL_TREE, default_function_array_conversion (location, array).value,
				    tree_cons  /* 16 */
				    (NULL_TREE, build_int_cst (NULL_TREE, 16), NULL_TREE))), NULL);
  }
#endif
  t = build2 (MODIFY_EXPR, void_type_node, objc_foreach_batchsize_decl,
	      convert (long_unsigned_type_node, t));
  SET_EXPR_LOCATION (t, location);
  append_to_statement_list (t, &BIND_EXPR_BODY (bind));

  /* if (__objc_foreach_batchsize == 0) */
  first_if = build3 (COND_EXPR, void_type_node,
		     /* Condition.  */
		     c_fully_fold
		     (c_common_truthvalue_conversion
		      (location,
		       build_binary_op (location,
					EQ_EXPR,
					objc_foreach_batchsize_decl,
					build_int_cst (long_unsigned_type_node, 0), 1)),
		      false, NULL),
		     /* Then block (we fill it in later).  */
		     NULL_TREE,
		     /* Else block (we fill it in later).  */
		     NULL_TREE);
  SET_EXPR_LOCATION (first_if, location);
  append_to_statement_list (first_if, &BIND_EXPR_BODY (bind));

  /* then <object expression> = nil; */
  t = build2 (MODIFY_EXPR, void_type_node, object_expression, convert (objc_object_type, null_pointer_node));
  SET_EXPR_LOCATION (t, location);
  COND_EXPR_THEN (first_if) = t;

  /* Now we build the 'else' part of the if; once we finish building
     it, we attach it to first_if as the 'else' part.  */

  /* else */
  /* { */

  /* unsigned long __objc_foreach_mutations_pointer; */
  objc_foreach_mutations_pointer_decl = objc_create_temporary_var (long_unsigned_type_node, "__objc_foreach_mutations_pointer");

  /* Generate the local variable binding.  */
  first_else = build3 (BIND_EXPR, void_type_node, objc_foreach_mutations_pointer_decl, NULL, NULL);
  SET_EXPR_LOCATION (first_else, location);
  TREE_SIDE_EFFECTS (first_else) = 1;

  /* __objc_foreach_mutations_pointer = *__objc_foreach_enum_state.mutationsPtr; */
  t = build2 (MODIFY_EXPR, void_type_node, objc_foreach_mutations_pointer_decl,
	      build_indirect_ref (location, objc_build_component_ref (objc_foreach_enum_state_decl,
								      get_identifier ("mutationsPtr")),
				  RO_UNARY_STAR));
  SET_EXPR_LOCATION (t, location);
  append_to_statement_list (t, &BIND_EXPR_BODY (first_else));

  /* next_batch: */
  next_batch_label_decl = create_artificial_label (location);
  t = build1 (LABEL_EXPR, void_type_node, next_batch_label_decl);
  SET_EXPR_LOCATION (t, location);
  append_to_statement_list (t, &BIND_EXPR_BODY (first_else));

  /* { */

  /* unsigned long __objc_foreach_index; */
  objc_foreach_index_decl = objc_create_temporary_var (long_unsigned_type_node, "__objc_foreach_index");

  /* Generate the local variable binding.  */
  next_batch_bind = build3 (BIND_EXPR, void_type_node, objc_foreach_index_decl, NULL, NULL);
  SET_EXPR_LOCATION (next_batch_bind, location);
  TREE_SIDE_EFFECTS (next_batch_bind) = 1;
  append_to_statement_list (next_batch_bind, &BIND_EXPR_BODY (first_else));

  /* __objc_foreach_index = 0; */
  t = build2 (MODIFY_EXPR, void_type_node, objc_foreach_index_decl,
	      build_int_cst (long_unsigned_type_node, 0));
  SET_EXPR_LOCATION (t, location);
  append_to_statement_list (t, &BIND_EXPR_BODY (next_batch_bind));

  /* next_object: */
  next_object_label_decl = create_artificial_label (location);
  t = build1 (LABEL_EXPR, void_type_node, next_object_label_decl);
  SET_EXPR_LOCATION (t, location);
  append_to_statement_list (t, &BIND_EXPR_BODY (next_batch_bind));

  /* if (__objc_foreach_mutation_pointer != *__objc_foreach_enum_state.mutationsPtr) objc_enumeration_mutation (<collection expression>); */
  t = build3 (COND_EXPR, void_type_node,
	      /* Condition.  */
	      c_fully_fold
	      (c_common_truthvalue_conversion
	       (location,
		build_binary_op
		(location,
		 NE_EXPR,
		 objc_foreach_mutations_pointer_decl,
		 build_indirect_ref (location,
				     objc_build_component_ref (objc_foreach_enum_state_decl,
							       get_identifier ("mutationsPtr")),
				     RO_UNARY_STAR), 1)),
	       false, NULL),
	      /* Then block.  */
	      build_function_call (input_location,
				   objc_enumeration_mutation_decl,
				   tree_cons (NULL, collection_expression, NULL)),
	      /* Else block.  */
	      NULL_TREE);
  SET_EXPR_LOCATION (t, location);
  append_to_statement_list (t, &BIND_EXPR_BODY (next_batch_bind));

  /* <object expression> = enumState.itemsPtr[__objc_foreach_index]; */
  t = build2 (MODIFY_EXPR, void_type_node, object_expression,
	      build_array_ref (location, objc_build_component_ref (objc_foreach_enum_state_decl,
								   get_identifier ("itemsPtr")),
			       objc_foreach_index_decl));
  SET_EXPR_LOCATION (t, location);
  append_to_statement_list (t, &BIND_EXPR_BODY (next_batch_bind));

  /* <statements> [PS: in <statments>, 'break' jumps to break_label and 'continue' jumps to continue_label] */
  append_to_statement_list (for_body, &BIND_EXPR_BODY (next_batch_bind));

  /* continue_label: */
  if (continue_label)
    {
      t = build1 (LABEL_EXPR, void_type_node, continue_label);
      SET_EXPR_LOCATION (t, location);
      append_to_statement_list (t, &BIND_EXPR_BODY (next_batch_bind));
    }

  /* __objc_foreach_index++; */
  t = build2 (MODIFY_EXPR, void_type_node, objc_foreach_index_decl,
	      build_binary_op (location,
			       PLUS_EXPR,
			       objc_foreach_index_decl,
			       build_int_cst (long_unsigned_type_node, 1), 1));
  SET_EXPR_LOCATION (t, location);
  append_to_statement_list (t, &BIND_EXPR_BODY (next_batch_bind));

  /* if (__objc_foreach_index < __objc_foreach_batchsize) goto next_object; */
  t = build3 (COND_EXPR, void_type_node,
	      /* Condition.  */
	      c_fully_fold
	      (c_common_truthvalue_conversion
	       (location,
		build_binary_op (location,
				 LT_EXPR,
				 objc_foreach_index_decl,
				 objc_foreach_batchsize_decl, 1)),
	       false, NULL),
	      /* Then block.  */
	      build1 (GOTO_EXPR, void_type_node, next_object_label_decl),
	      /* Else block.  */
	      NULL_TREE);
  SET_EXPR_LOCATION (t, location);
  append_to_statement_list (t, &BIND_EXPR_BODY (next_batch_bind));

  /* __objc_foreach_batchsize = [__objc_foreach_collection countByEnumeratingWithState: &__objc_foreach_enum_state  objects: __objc_foreach_items  count: 16]; */
#ifdef OBJCPLUS
  t = objc_finish_message_expr (objc_foreach_collection_decl, selector_name,
				/* Parameters.  */
				tree_cons    /* &__objc_foreach_enum_state */
				(NULL_TREE, build_fold_addr_expr_loc (location, objc_foreach_enum_state_decl),
				 tree_cons   /* __objc_foreach_items  */
				 (NULL_TREE, objc_foreach_items_decl,
				  tree_cons  /* 16 */
				  (NULL_TREE, build_int_cst (NULL_TREE, 16), NULL_TREE))), NULL);
#else
  /* In C, we need to decay the __objc_foreach_items array that we are passing.  */
  {
    struct c_expr array;
    array.value = objc_foreach_items_decl;
    t = objc_finish_message_expr (objc_foreach_collection_decl, selector_name,
				  /* Parameters.  */
				  tree_cons    /* &__objc_foreach_enum_state */
				  (NULL_TREE, build_fold_addr_expr_loc (location, objc_foreach_enum_state_decl),
				   tree_cons   /* __objc_foreach_items  */
				   (NULL_TREE, default_function_array_conversion (location, array).value,
				    tree_cons  /* 16 */
				    (NULL_TREE, build_int_cst (NULL_TREE, 16), NULL_TREE))), NULL);
  }
#endif
  t = build2 (MODIFY_EXPR, void_type_node, objc_foreach_batchsize_decl,
	      convert (long_unsigned_type_node, t));
  SET_EXPR_LOCATION (t, location);
  append_to_statement_list (t, &BIND_EXPR_BODY (next_batch_bind));

  /* } */

  /* if (__objc_foreach_batchsize != 0) goto next_batch; */
  t = build3 (COND_EXPR, void_type_node,
	      /* Condition.  */
	      c_fully_fold
	      (c_common_truthvalue_conversion
	       (location,
		build_binary_op (location,
				 NE_EXPR,
				 objc_foreach_batchsize_decl,
				 build_int_cst (long_unsigned_type_node, 0), 1)),
	       false, NULL),
	      /* Then block.  */
	      build1 (GOTO_EXPR, void_type_node, next_batch_label_decl),
	      /* Else block.  */
	      NULL_TREE);
  SET_EXPR_LOCATION (t, location);
  append_to_statement_list (t, &BIND_EXPR_BODY (first_else));

  /* <object expression> = nil; */
  t = build2 (MODIFY_EXPR, void_type_node, object_expression, convert (objc_object_type, null_pointer_node));
  SET_EXPR_LOCATION (t, location);
  append_to_statement_list (t, &BIND_EXPR_BODY (first_else));

  /* break_label: */
  if (break_label)
    {
      t = build1 (LABEL_EXPR, void_type_node, break_label);
      SET_EXPR_LOCATION (t, location);
      append_to_statement_list (t, &BIND_EXPR_BODY (first_else));
    }

  /* } */
  COND_EXPR_ELSE (first_if) = first_else;

  /* Do the whole thing.  */
  add_stmt (bind);

#ifdef DEBUG_OBJC_FINISH_FOREACH_LOOP
  /* This will print to stderr the whole blurb generated by the
     compiler while compiling (assuming the compiler doesn't crash
     before getting here).
   */
  debug_generic_stmt (bind);
#endif

  /* } */
  /* Done by c-parser.c  */
}

/* --- SUPPORT FOR FORMAT ARG CHECKING --- */
/* Return true if we have an NxString object pointer.  */

bool
objc_string_ref_type_p (tree strp)
{
  tree tmv;
  if (!strp || TREE_CODE (strp) != POINTER_TYPE)
    return false;

  tmv = TYPE_MAIN_VARIANT (TREE_TYPE (strp));
  tmv = OBJC_TYPE_NAME (tmv);
  return (tmv
	  && TREE_CODE (tmv) == IDENTIFIER_NODE
	  && IDENTIFIER_POINTER (tmv)
	  && !strncmp (IDENTIFIER_POINTER (tmv), "NSString", 8));
}

/* At present the behavior of this is undefined and it does nothing.  */
void
objc_check_format_arg (tree ARG_UNUSED (format_arg),
		       tree ARG_UNUSED (args_list))
{
}

void
objc_common_init_ts (void)
{
  c_common_init_ts ();

  MARK_TS_DECL_NON_COMMON (CLASS_METHOD_DECL);
  MARK_TS_DECL_NON_COMMON (INSTANCE_METHOD_DECL);
  MARK_TS_DECL_NON_COMMON (KEYWORD_DECL);
  MARK_TS_DECL_NON_COMMON (PROPERTY_DECL);

  MARK_TS_COMMON (CLASS_INTERFACE_TYPE);
  MARK_TS_COMMON (PROTOCOL_INTERFACE_TYPE);
  MARK_TS_COMMON (CLASS_IMPLEMENTATION_TYPE);

  MARK_TS_TYPED (MESSAGE_SEND_EXPR);
  MARK_TS_TYPED (PROPERTY_REF);
}

#include "gt-objc-objc-act.h"
