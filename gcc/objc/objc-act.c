/* Implement classes and message passing for Objective C.
   Copyright (C) 1992, 1993, 1994, 1995, 1997, 1998, 1999, 2000,
   2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.
   Contributed by Steve Naroff.

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
Boston, MA 02111-1307, USA.  */

/* Purpose: This module implements the Objective-C 4.0 language.

   compatibility issues (with the Stepstone translator):

   - does not recognize the following 3.3 constructs.
     @requires, @classes, @messages, = (...)
   - methods with variable arguments must conform to ANSI standard.
   - tagged structure definitions that appear in BOTH the interface
     and implementation are not allowed.
   - public/private: all instance variables are public within the
     context of the implementation...I consider this to be a bug in
     the translator.
   - statically allocated objects are not supported. the user will
     receive an error if this service is requested.

   code generation `options':

   */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "tm_p.h"
#include "expr.h"

#ifdef OBJCPLUS
#include "cp-tree.h"
#else
#include "c-tree.h"
#endif

#include "c-common.h"
#include "c-pragma.h"
#include "flags.h"
#include "langhooks.h"
#include "objc-act.h"
#include "input.h"
#include "except.h"
#include "function.h"
#include "output.h"
#include "toplev.h"
#include "ggc.h"
#include "varray.h"
#include "debug.h"
#include "target.h"
#include "diagnostic.h"
#include "cgraph.h"
#include "tree-iterator.h"
#include "libfuncs.h"
#include "hashtab.h"
#include "langhooks-def.h"

#define OBJC_VOID_AT_END	void_list_node

/* When building Objective-C++, we are not linking against the C front-end
   and so need to replicate the C tree-construction functions in some way.  */
#ifdef OBJCPLUS
#define OBJCP_REMAP_FUNCTIONS
#include "objcp-decl.h"
#endif  /* OBJCPLUS */

/* This is the default way of generating a method name.  */
/* I am not sure it is really correct.
   Perhaps there's a danger that it will make name conflicts
   if method names contain underscores. -- rms.  */
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

/* Set up for use of obstacks.  */

#include "obstack.h"

/* This obstack is used to accumulate the encoding of a data type.  */
static struct obstack util_obstack;

/* This points to the beginning of obstack contents, so we can free
   the whole contents.  */
char *util_firstobj;

/* The version identifies which language generation and runtime
   the module (file) was compiled for, and is recorded in the
   module descriptor.  */

#define OBJC_VERSION	(flag_next_runtime ? 5 : 8)
#define PROTOCOL_VERSION 2

/* (Decide if these can ever be validly changed.) */
#define OBJC_ENCODE_INLINE_DEFS 	0
#define OBJC_ENCODE_DONT_INLINE_DEFS	1

/*** Private Interface (procedures) ***/

/* Used by compile_file.  */

static void init_objc (void);
static void finish_objc (void);

/* Code generation.  */

static void synth_module_prologue (void);
static tree objc_build_constructor (tree, tree);
static void build_module_descriptor (void);
static void build_module_initializer_routine (void);
static tree init_module_descriptor (tree);
static tree build_objc_method_call (int, tree, tree, tree, tree);
static void generate_strings (void);
static tree get_proto_encoding (tree);
static void build_selector_translation_table (void);
static tree lookup_interface (tree);
static tree objc_add_static_instance (tree, tree);

static tree start_class (enum tree_code, tree, tree, tree);
static tree continue_class (tree);
static void finish_class (tree);
static void start_method_def (tree);
#ifdef OBJCPLUS
static void objc_start_function (tree, tree, tree, tree);
#else
static void objc_start_function (tree, tree, tree, struct c_arg_info *);
#endif
static tree start_protocol (enum tree_code, tree, tree);
static tree build_method_decl (enum tree_code, tree, tree, tree);
static tree objc_add_method (tree, tree, int);
static tree add_instance_variable (tree, int, tree);
static tree build_ivar_reference (tree);
static tree is_ivar (tree, tree);
static int is_private (tree);
static tree get_super_receiver (void);

static void build_objc_exception_stuff (void);
static void build_next_objc_exception_stuff (void);

static tree build_ivar_template (void);
static tree build_method_template (void);
static void build_private_template (tree);
static void build_class_template (void);
static void build_selector_template (void);
static void build_category_template (void);
static tree lookup_method_in_hash_lists (tree, int);
static void build_super_template (void);
static tree build_category_initializer (tree, tree, tree, tree, tree, tree);
static tree build_protocol_initializer (tree, tree, tree, tree, tree);
static void synth_forward_declarations (void);
static int ivar_list_length (tree);
static tree get_class_ivars (tree);
static void generate_ivar_lists (void);
static void generate_dispatch_tables (void);
static void generate_shared_structures (void);
static tree generate_protocol_list (tree);
static void build_protocol_reference (tree);

static tree build_keyword_selector (tree);
static const char *synth_id_with_class_suffix (const char *, tree);

static void generate_static_references (void);
static int check_methods_accessible (tree, tree, int);
static void encode_aggregate_within (tree, int, int, int, int);
static const char *objc_demangle (const char *);

/* Hash tables to manage the global pool of method prototypes.  */

hash *nst_method_hash_list = 0;
hash *cls_method_hash_list = 0;

static size_t hash_func (tree);
static void hash_init (void);
static void hash_enter (hash *, tree);
static hash hash_lookup (hash *, tree);
static void hash_add_attr (hash, tree);
static tree lookup_method (tree, tree);
static tree lookup_method_static (tree, tree, int);
static void add_method_to_hash_list (hash *, tree);
static tree add_class (tree);
static void add_category (tree, tree);
static inline tree lookup_category (tree, tree);

enum string_section
{
  class_names,		/* class, category, protocol, module names */
  meth_var_names,	/* method and variable names */
  meth_var_types	/* method and variable type descriptors */
};

static tree add_objc_string (tree, enum string_section);
static tree get_objc_string_decl (tree, enum string_section);
static tree build_objc_string_decl (enum string_section);
static tree build_selector_reference_decl (void);
static void build_selector_table_decl (void);

/* Protocol additions.  */

static tree add_protocol (tree);
static tree lookup_protocol (tree);
static void check_protocol_recursively (tree, tree);
static tree lookup_and_install_protocols (tree);

/* Type encoding.  */

static void encode_type_qualifiers (tree);
static void encode_pointer (tree, int, int);
static void encode_array (tree, int, int);
static void encode_aggregate (tree, int, int);
static void encode_next_bitfield (int);
static void encode_gnu_bitfield (int, tree, int);
static void encode_type (tree, int, int);
static void encode_field_decl (tree, int, int);

#ifdef OBJCPLUS
static void really_start_method (tree, tree);
#else
static void really_start_method (tree, struct c_arg_info *);
#endif
static int objc_types_are_equivalent (tree, tree);
static int comp_proto_with_proto (tree, tree);
static tree get_arg_type_list (tree, int, int);
static void objc_push_parm (tree);
#ifdef OBJCPLUS
static tree objc_get_parm_info (int);
#else
static struct c_arg_info *objc_get_parm_info (int);
#endif
static void synth_self_and_ucmd_args (void);

/* Utilities for debugging and error diagnostics.  */

static void warn_with_method (const char *, int, tree);
static void error_with_ivar (const char *, tree);
static char *gen_type_name (tree);
static char *gen_type_name_0 (tree);
static char *gen_method_decl (tree);
static char *gen_declaration (tree);
static void dump_interface (FILE *, tree);

/* Everything else.  */

static tree lookup_method_in_protocol_list (tree, tree, int);
static tree lookup_protocol_in_reflist (tree, tree);
static tree start_var_decl (tree, const char *);
static void finish_var_decl (tree, tree);
static tree create_field_decl (tree, const char *);
static tree setup_string_decl (void);
static int check_string_class_template (void);
static tree my_build_string (int, const char *);
static void build_objc_symtab_template (void);
static tree init_def_list (tree);
static tree init_objc_symtab (tree);
static tree build_metadata_decl (const char *, tree);
static void forward_declare_categories (void);
static void generate_objc_symtab_decl (void);
static tree build_selector (tree);
static tree build_typed_selector_reference (tree, tree);
static tree build_selector_reference (tree);
static tree build_class_reference_decl (void);
static void add_class_reference (tree);
static void build_protocol_template (void);
static tree build_descriptor_table_initializer (tree, tree);
static tree build_method_prototype_list_template (tree, int);
static tree build_method_prototype_template (void);
static tree objc_method_parm_type (tree);
static int objc_encoded_type_size (tree);
static tree encode_method_prototype (tree);
static tree generate_descriptor_table (tree, const char *, int, tree, tree);
static void generate_method_descriptors (tree);
static void generate_protocol_references (tree);
static void generate_protocols (void);
static void check_ivars (tree, tree);
static tree build_ivar_list_template (tree, int);
static tree build_method_list_template (tree, int);
static tree build_ivar_list_initializer (tree, tree);
static tree generate_ivars_list (tree, const char *, int, tree);
static tree build_dispatch_table_initializer (tree, tree);
static tree generate_dispatch_table (tree, const char *, int, tree);
static tree build_shared_structure_initializer (tree, tree, tree, tree,
						tree, int, tree, tree, tree);
static void generate_category (tree);
static tree adjust_type_for_id_default (tree);
static tree check_duplicates (hash, int, int);
static tree receiver_is_class_object (tree, int, int);
static int check_methods (tree, tree, int);
static int conforms_to_protocol (tree, tree);
static void check_protocol (tree, const char *, const char *);
static void check_protocols (tree, const char *, const char *);
static void generate_classref_translation_entry (tree);
static void handle_class_ref (tree);
static void generate_struct_by_value_array (void)
     ATTRIBUTE_NORETURN;
static void mark_referenced_methods (void);
static void generate_objc_image_info (void);

/*** Private Interface (data) ***/

/* Reserved tag definitions.  */

#define OBJECT_TYPEDEF_NAME		"id"
#define CLASS_TYPEDEF_NAME		"Class"

#define TAG_OBJECT			"objc_object"
#define TAG_CLASS			"objc_class"
#define TAG_SUPER			"objc_super"
#define TAG_SELECTOR			"objc_selector"

#define UTAG_CLASS			"_objc_class"
#define UTAG_IVAR			"_objc_ivar"
#define UTAG_IVAR_LIST			"_objc_ivar_list"
#define UTAG_METHOD			"_objc_method"
#define UTAG_METHOD_LIST		"_objc_method_list"
#define UTAG_CATEGORY			"_objc_category"
#define UTAG_MODULE			"_objc_module"
#define UTAG_SYMTAB			"_objc_symtab"
#define UTAG_SUPER			"_objc_super"
#define UTAG_SELECTOR			"_objc_selector"

#define UTAG_PROTOCOL			"_objc_protocol"
#define UTAG_METHOD_PROTOTYPE		"_objc_method_prototype"
#define UTAG_METHOD_PROTOTYPE_LIST	"_objc__method_prototype_list"

/* Note that the string object global name is only needed for the
   NeXT runtime.  */
#define STRING_OBJECT_GLOBAL_FORMAT	"_%sClassReference"

#define PROTOCOL_OBJECT_CLASS_NAME	"Protocol"

static const char *TAG_GETCLASS;
static const char *TAG_GETMETACLASS;
static const char *TAG_MSGSEND;
static const char *TAG_MSGSENDSUPER;
/* The NeXT Objective-C messenger may have two extra entry points, for use
   when returning a structure. */
static const char *TAG_MSGSEND_STRET;
static const char *TAG_MSGSENDSUPER_STRET;
static const char *default_constant_string_class_name;

/* Runtime metadata flags.  */
#define CLS_FACTORY			0x0001L
#define CLS_META			0x0002L

#define OBJC_MODIFIER_STATIC		0x00000001
#define OBJC_MODIFIER_FINAL		0x00000002
#define OBJC_MODIFIER_PUBLIC		0x00000004
#define OBJC_MODIFIER_PRIVATE		0x00000008
#define OBJC_MODIFIER_PROTECTED		0x00000010
#define OBJC_MODIFIER_NATIVE		0x00000020
#define OBJC_MODIFIER_SYNCHRONIZED	0x00000040
#define OBJC_MODIFIER_ABSTRACT		0x00000080
#define OBJC_MODIFIER_VOLATILE		0x00000100
#define OBJC_MODIFIER_TRANSIENT		0x00000200
#define OBJC_MODIFIER_NONE_SPECIFIED	0x80000000

/* NeXT-specific tags.  */

#define TAG_MSGSEND_NONNIL		"objc_msgSendNonNil"
#define TAG_MSGSEND_NONNIL_STRET	"objc_msgSendNonNil_stret"
#define TAG_EXCEPTIONEXTRACT		"objc_exception_extract"
#define TAG_EXCEPTIONTRYENTER		"objc_exception_try_enter"
#define TAG_EXCEPTIONTRYEXIT		"objc_exception_try_exit"
#define TAG_EXCEPTIONMATCH		"objc_exception_match"
#define TAG_EXCEPTIONTHROW		"objc_exception_throw"
#define TAG_SYNCENTER			"objc_sync_enter"
#define TAG_SYNCEXIT			"objc_sync_exit"
#define TAG_SETJMP			"_setjmp"
#define UTAG_EXCDATA			"_objc_exception_data"

/* GNU-specific tags.  */

#define TAG_EXECCLASS			"__objc_exec_class"
#define TAG_GNUINIT			"__objc_gnu_init"

/* The OCTI_... enumeration itself is in objc/objc-act.h.  */
tree objc_global_trees[OCTI_MAX];

static void handle_impent (struct imp_entry *);

struct imp_entry *imp_list = 0;
int imp_count = 0;	/* `@implementation' */
int cat_count = 0;	/* `@category' */

enum tree_code objc_inherit_code;
int objc_public_flag;

/* Use to generate method labels.  */
static int method_slot = 0;

#define BUFSIZE		1024

static char *errbuf;	/* Buffer for error diagnostics */

/* Data imported from tree.c.  */

extern enum debug_info_type write_symbols;

/* Data imported from toplev.c.  */

extern const char *dump_base_name;

static int flag_typed_selectors;

/* Store all constructed constant strings in a hash table so that
   they get uniqued properly.  */

struct string_descriptor GTY(())
{
  /* The literal argument .  */
  tree literal;

  /* The resulting constant string.  */
  tree constructor;
};

static GTY((param_is (struct string_descriptor))) htab_t string_htab;

static hashval_t string_hash (const void *);
static int string_eq (const void *, const void *);

FILE *gen_declaration_file;

/* Tells "encode_pointer/encode_aggregate" whether we are generating
   type descriptors for instance variables (as opposed to methods).
   Type descriptors for instance variables contain more information
   than methods (for static typing and embedded structures).  */

static int generating_instance_variables = 0;

/* Some platforms pass small structures through registers versus
   through an invisible pointer.  Determine at what size structure is
   the transition point between the two possibilities.  */

static void
generate_struct_by_value_array (void)
{
  tree type;
  tree field_decl, field_decl_chain;
  int i, j;
  int aggregate_in_mem[32];
  int found = 0;

  /* Presumably no platform passes 32 byte structures in a register.  */
  for (i = 1; i < 32; i++)
    {
      char buffer[5];

      /* Create an unnamed struct that has `i' character components */
      type = start_struct (RECORD_TYPE, NULL_TREE);

      strcpy (buffer, "c1");
      field_decl = create_field_decl (char_type_node,
				      buffer);
      field_decl_chain = field_decl;

      for (j = 1; j < i; j++)
	{
	  sprintf (buffer, "c%d", j + 1);
	  field_decl = create_field_decl (char_type_node,
					  buffer);
	  chainon (field_decl_chain, field_decl);
	}
      finish_struct (type, field_decl_chain, NULL_TREE);

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
      printf ("#define OBJC_MAX_STRUCT_BY_VALUE %d\n\n", i);

      /* The first member of the structure is always 0 because we don't handle
	 structures with 0 members */
      printf ("static int struct_forward_array[] = {\n  0");

      for (j = 1; j <= i; j++)
	printf (", %d", aggregate_in_mem[j]);
      printf ("\n};\n");
    }

  exit (0);
}

bool
objc_init (void)
{
#ifdef OBJCPLUS
  if (cxx_init () == false)
#else
  if (c_objc_common_init () == false)
#endif
    return false;

#ifndef USE_MAPPED_LOCATION
  /* Force the line number back to 0; check_newline will have
     raised it to 1, which will make the builtin functions appear
     not to be built in.  */
  input_line = 0;
#endif

  /* If gen_declaration desired, open the output file.  */
  if (flag_gen_declaration)
    {
      register char * const dumpname = concat (dump_base_name, ".decl", NULL);
      gen_declaration_file = fopen (dumpname, "w");
      if (gen_declaration_file == 0)
	fatal_error ("can't open %s: %m", dumpname);
      free (dumpname);
    }

  if (flag_next_runtime)
    {
      TAG_GETCLASS = "objc_getClass";
      TAG_GETMETACLASS = "objc_getMetaClass";
      TAG_MSGSEND = "objc_msgSend";
      TAG_MSGSENDSUPER = "objc_msgSendSuper";
      TAG_MSGSEND_STRET = "objc_msgSend_stret";
      TAG_MSGSENDSUPER_STRET = "objc_msgSendSuper_stret";
      default_constant_string_class_name = "NSConstantString";
    }
  else
    {
      TAG_GETCLASS = "objc_get_class";
      TAG_GETMETACLASS = "objc_get_meta_class";
      TAG_MSGSEND = "objc_msg_lookup";
      TAG_MSGSENDSUPER = "objc_msg_lookup_super";
      /* GNU runtime does not provide special functions to support
	 structure-returning methods.  */
      default_constant_string_class_name = "NXConstantString";
      flag_typed_selectors = 1;
    }

  init_objc ();

  if (print_struct_values)
    generate_struct_by_value_array ();

  return true;
}

void
objc_finish_file (void)
{
  mark_referenced_methods ();

#ifdef OBJCPLUS
  /* We need to instantiate templates _before_ we emit ObjC metadata;
     if we do not, some metadata (such as selectors) may go missing.  */
  at_eof = 1;
  instantiate_pending_templates (0);
#endif

  /* Finalize Objective-C runtime data.  No need to generate tables
     and code if only checking syntax.  */
  if (!flag_syntax_only)
    finish_objc ();

  if (gen_declaration_file)
    fclose (gen_declaration_file);

#ifdef OBJCPLUS
  cp_finish_file ();
#endif
}

/* Return the first occurrence of a method declaration corresponding
   to sel_name in rproto_list.  Search rproto_list recursively.
   If is_class is 0, search for instance methods, otherwise for class
   methods.  */
static tree
lookup_method_in_protocol_list (tree rproto_list, tree sel_name,
				int is_class)
{
   tree rproto, p;
   tree fnd = 0;

   for (rproto = rproto_list; rproto; rproto = TREE_CHAIN (rproto))
     {
        p = TREE_VALUE (rproto);

	if (TREE_CODE (p) == PROTOCOL_INTERFACE_TYPE)
	  {
	    if ((fnd = lookup_method (is_class
				      ? PROTOCOL_CLS_METHODS (p)
				      : PROTOCOL_NST_METHODS (p), sel_name)))
	      ;
	    else if (PROTOCOL_LIST (p))
	      fnd = lookup_method_in_protocol_list (PROTOCOL_LIST (p),
						    sel_name, is_class);
	  }
	else
          {
	    ; /* An identifier...if we could not find a protocol.  */
          }

	if (fnd)
	  return fnd;
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
objc_start_class_interface (tree class, tree super_class, tree protos)
{
  objc_interface_context
    = objc_ivar_context
    = start_class (CLASS_INTERFACE_TYPE, class, super_class, protos);
  objc_public_flag = 0;
}

void
objc_start_category_interface (tree class, tree categ, tree protos)
{
  objc_interface_context
    = start_class (CATEGORY_INTERFACE_TYPE, class, categ, protos);
  objc_ivar_chain
    = continue_class (objc_interface_context);
}

void
objc_start_protocol (tree name, tree protos)
{
  objc_interface_context
    = start_protocol (PROTOCOL_INTERFACE_TYPE, name, protos);
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
}

void
objc_start_class_implementation (tree class, tree super_class)
{
  objc_implementation_context
    = objc_ivar_context
    = start_class (CLASS_IMPLEMENTATION_TYPE, class, super_class, NULL_TREE);
  objc_public_flag = 0;
}

void
objc_start_category_implementation (tree class, tree categ)
{
  objc_implementation_context
    = start_class (CATEGORY_IMPLEMENTATION_TYPE, class, categ, NULL_TREE);
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
  if (objc_implementation_context)
    {
      finish_class (objc_implementation_context);
      objc_ivar_chain = NULL_TREE;
      objc_implementation_context = NULL_TREE;
    }
  else
    warning ("%<@end%> must appear in an @implementation context");
}

void
objc_set_visibility (int visibility)
{
  objc_public_flag = visibility;
}

void
objc_set_method_type (enum tree_code type)
{
  objc_inherit_code = (type == PLUS_EXPR
		       ? CLASS_METHOD_DECL
		       : INSTANCE_METHOD_DECL);
}

tree
objc_build_method_signature (tree rettype, tree selector, tree optparms)
{
  return build_method_decl (objc_inherit_code, rettype, selector, optparms);
}

void
objc_add_method_declaration (tree decl)
{
  if (!objc_interface_context)
    fatal_error ("method declaration not in @interface context");

  objc_add_method (objc_interface_context,
		   decl,
		   objc_inherit_code == CLASS_METHOD_DECL);
}

void
objc_start_method_definition (tree decl)
{
  if (!objc_implementation_context)
    fatal_error ("method definition not in @implementation context");

  objc_add_method (objc_implementation_context,
		   decl,
		   objc_inherit_code == CLASS_METHOD_DECL);
  start_method_def (decl);
}

void
objc_add_instance_variable (tree decl)
{
  (void) add_instance_variable (objc_ivar_context,
				objc_public_flag,
				decl);
}

/* Return 1 if IDENT is an ObjC/ObjC++ reserved keyword in the context of
   an '@'.  */

int
objc_is_reserved_word (tree ident)
{
  unsigned char code = C_RID_CODE (ident);

  return (OBJC_IS_AT_KEYWORD (code)
#ifdef OBJCPLUS
	  || code == RID_CLASS || code == RID_PUBLIC
	  || code == RID_PROTECTED || code == RID_PRIVATE
	  || code == RID_TRY || code == RID_THROW || code == RID_CATCH
#endif
	    );
}

/* Return true if TYPE is 'id'.  */

static bool
objc_is_object_id (tree type)
{
  return OBJC_TYPE_NAME (type) == objc_object_id;
}

static bool
objc_is_class_id (tree type)
{
  return OBJC_TYPE_NAME (type) == objc_class_id;
}


int
objc_types_compatible_p (tree type1, tree type2)
{

  if (objc_is_object_ptr (type1) || objc_is_object_ptr (type2)
      || objc_is_class_name (type1) || objc_is_class_name (type2))
    {
      return lhd_types_compatible_p (type1, type2);
    }
  else
    {
#ifdef OBJCPLUS
      return cxx_types_compatible_p (type1, type2);
#else
      return c_types_compatible_p (type1, type2);
#endif
    }
}


/* Return 1 if LHS and RHS are compatible types for assignment or
   various other operations.  Return 0 if they are incompatible, and
   return -1 if we choose to not decide (because the types are really
   just C types, not ObjC specific ones).  When the operation is
   REFLEXIVE (typically comparisons), check for compatibility in
   either direction; when it's not (typically assignments), don't.

   This function is called in two cases: when both lhs and rhs are
   pointers to records (in which case we check protocols too), and
   when both lhs and rhs are records (in which case we check class
   inheritance only).

   Warnings about classes/protocols not implementing a protocol are
   emitted here (multiple of those warnings might be emitted for a
   single line!); generic warnings about incompatible assignments and
   lacks of casts in comparisons are/must be emitted by the caller if
   we return 0.
*/

int
objc_comptypes (tree lhs, tree rhs, int reflexive)
{
  /* New clause for protocols.  */

  /* Here we manage the case of a POINTER_TYPE = POINTER_TYPE.  We only
     manage the ObjC ones, and leave the rest to the C code.  */
  if (TREE_CODE (lhs) == POINTER_TYPE
      && TREE_CODE (TREE_TYPE (lhs)) == RECORD_TYPE
      && TREE_CODE (rhs) == POINTER_TYPE
      && TREE_CODE (TREE_TYPE (rhs)) == RECORD_TYPE)
    {
      int lhs_is_proto = IS_PROTOCOL_QUALIFIED_UNTYPED (lhs);
      int rhs_is_proto = IS_PROTOCOL_QUALIFIED_UNTYPED (rhs);

      if (lhs_is_proto)
        {
	  tree lproto, lproto_list = TYPE_OBJC_PROTOCOL_LIST (TREE_TYPE (lhs));
	  tree rproto, rproto_list;
	  tree p;

	  /* <Protocol> = <Protocol>  */
	  if (rhs_is_proto)
	    {
	      /* Class <Protocol> != id <Protocol>;
		 id <Protocol> != Class <Protocol>  */
	      if (IS_ID (lhs) != IS_ID (rhs))
		return 0;

	      rproto_list = TYPE_OBJC_PROTOCOL_LIST (TREE_TYPE (rhs));

	      if (!reflexive)
		{
		  /* An assignment between objects of type 'id
		     <Protocol>'; make sure the protocol on the lhs is
		     supported by the object on the rhs.  */
		  for (lproto = lproto_list; lproto;
		       lproto = TREE_CHAIN (lproto))
		    {
		      p = TREE_VALUE (lproto);
		      rproto = lookup_protocol_in_reflist (rproto_list, p);

		      if (!rproto)
			warning
			  ("object does not conform to the %qs protocol",
			   IDENTIFIER_POINTER (PROTOCOL_NAME (p)));
		    }
		  return 1;
		}
	      else
		{
		  /* Obscure case - a comparison between two objects
		     of type 'id <Protocol>'.  Check that either the
		     protocol on the lhs is supported by the object on
		     the rhs, or viceversa.  */

		  /* Check if the protocol on the lhs is supported by the
		     object on the rhs.  */
		  for (lproto = lproto_list; lproto;
		       lproto = TREE_CHAIN (lproto))
		    {
		      p = TREE_VALUE (lproto);
		      rproto = lookup_protocol_in_reflist (rproto_list, p);

		      if (!rproto)
			{
			  /* Check failed - check if the protocol on the rhs
			     is supported by the object on the lhs.  */
			  for (rproto = rproto_list; rproto;
			       rproto = TREE_CHAIN (rproto))
			    {
			      p = TREE_VALUE (rproto);
			      lproto = lookup_protocol_in_reflist (lproto_list,
								   p);

			      if (!lproto)
				{
				  /* This check failed too: incompatible  */
				  return 0;
				}
			    }
			  return 1;
			}
		    }
		  return 1;
		}
	    }
	  /* <Protocol> = <class> *  */
	  else if (TYPED_OBJECT (TREE_TYPE (rhs)))
	    {
	      tree rname = OBJC_TYPE_NAME (TREE_TYPE (rhs));
	      tree rinter;

	      /* Class <Protocol> != <class> *  */
	      if (IS_CLASS (lhs))
		return 0;

	      /* Make sure the protocol is supported by the object on
		 the rhs.  */
	      for (lproto = lproto_list; lproto; lproto = TREE_CHAIN (lproto))
		{
		  p = TREE_VALUE (lproto);
		  rproto = 0;
		  rinter = lookup_interface (rname);

		  while (rinter && !rproto)
		    {
		      tree cat;

		      rproto_list = CLASS_PROTOCOL_LIST (rinter);
		      rproto = lookup_protocol_in_reflist (rproto_list, p);
		      /* If the underlying ObjC class does not have
			 the protocol we're looking for, check for "one-off"
			 protocols (e.g., `NSObject<MyProt> *foo;') attached
			 to the rhs.  */
		      if (!rproto && TYPE_HAS_OBJC_INFO (TREE_TYPE (rhs)))
			{
			  rproto_list = TYPE_OBJC_PROTOCOL_LIST (TREE_TYPE (rhs));
			  rproto = lookup_protocol_in_reflist (rproto_list, p);
			}

		      /* Check for protocols adopted by categories.  */
		      cat = CLASS_CATEGORY_LIST (rinter);
		      while (cat && !rproto)
			{
			  rproto_list = CLASS_PROTOCOL_LIST (cat);
			  rproto = lookup_protocol_in_reflist (rproto_list, p);
			  cat = CLASS_CATEGORY_LIST (cat);
			}

		      rinter = lookup_interface (CLASS_SUPER_NAME (rinter));
		    }

		  if (!rproto)
		    warning ("class %qs does not implement the %qs protocol",
			     IDENTIFIER_POINTER (OBJC_TYPE_NAME (TREE_TYPE (rhs))),
			     IDENTIFIER_POINTER (PROTOCOL_NAME (p)));
		}
	      return 1;
	    }
	  /* id <Protocol> = id; Class <Protocol> = id */
	  else if (objc_is_object_id (TREE_TYPE (rhs)))
	    {
	      return 1;
	    }
	  /* id <Protocol> != Class; Class <Protocol> = Class */
	  else if (objc_is_class_id (TREE_TYPE (rhs)))
	    {
	      return IS_CLASS (lhs);
	    }
	  /* <Protocol> = ?? : let comptypes decide.  */
          return -1;
        }
      else if (rhs_is_proto)
	{
	  /* <class> * = <Protocol> */
	  if (TYPED_OBJECT (TREE_TYPE (lhs)))
	    {
	      /* <class> * != Class <Protocol> */
	      if (IS_CLASS (rhs))
		return 0;

	      if (reflexive)
		{
		  tree rname = OBJC_TYPE_NAME (TREE_TYPE (lhs));
		  tree rinter;
		  tree rproto, rproto_list = TYPE_OBJC_PROTOCOL_LIST (TREE_TYPE (rhs));

		  /* Make sure the protocol is supported by the object on
		     the lhs.  */
		  for (rproto = rproto_list; rproto;
		       rproto = TREE_CHAIN (rproto))
		    {
		      tree p = TREE_VALUE (rproto);
		      tree lproto = 0;
		      rinter = lookup_interface (rname);

		      while (rinter && !lproto)
			{
			  tree cat;

			  tree lproto_list = CLASS_PROTOCOL_LIST (rinter);
			  lproto = lookup_protocol_in_reflist (lproto_list, p);
			  /* If the underlying ObjC class does not
			     have the protocol we're looking for,
			     check for "one-off" protocols (e.g.,
			     `NSObject<MyProt> *foo;') attached to the
			     lhs.  */
			  if (!lproto && TYPE_HAS_OBJC_INFO (TREE_TYPE (lhs)))
			    {
			      lproto_list = TYPE_OBJC_PROTOCOL_LIST
				(TREE_TYPE (lhs));
			      lproto = lookup_protocol_in_reflist
				(lproto_list, p);
			    }

			  /* Check for protocols adopted by categories.  */
			  cat = CLASS_CATEGORY_LIST (rinter);
			  while (cat && !lproto)
			    {
			      lproto_list = CLASS_PROTOCOL_LIST (cat);
			      lproto = lookup_protocol_in_reflist (lproto_list,
								   p);
			      cat = CLASS_CATEGORY_LIST (cat);
			    }
			
			  rinter = lookup_interface (CLASS_SUPER_NAME
						     (rinter));
			}
		
		      if (!lproto)
			warning ("class %qs does not implement the %qs protocol",
				 IDENTIFIER_POINTER (OBJC_TYPE_NAME
						     (TREE_TYPE (lhs))),
				 IDENTIFIER_POINTER (PROTOCOL_NAME (p)));
		    }
		  return 1;
		}
	      else
		return 0;
	    }
	  /* id = id <Protocol>; id = Class <Protocol> */
	  else if (objc_is_object_id (TREE_TYPE (lhs)))
	    {
	      return 1;
	    }
	  /* Class != id <Protocol>; Class = Class <Protocol> */
	  else if (objc_is_class_id (TREE_TYPE (lhs)))
	    {
	      return IS_CLASS (rhs);
	    }
	  /* ??? = <Protocol> : let comptypes decide */
	  else
	    {
	      return -1;
	    }
	}
      else
	{
	  /* Attention: we shouldn't defer to comptypes here.  One bad
	     side effect would be that we might loose the REFLEXIVE
	     information.
	  */
	  lhs = TREE_TYPE (lhs);
	  rhs = TREE_TYPE (rhs);
	}
    }

  if (TREE_CODE (lhs) != RECORD_TYPE || TREE_CODE (rhs) != RECORD_TYPE)
    {
      /* Nothing to do with ObjC - let immediately comptypes take
	 responsibility for checking.  */
      return -1;
    }

  /* `id' = `<class> *' `<class> *' = `id': always allow it.
     Please note that
     'Object *o = [[Object alloc] init]; falls
     in the case <class> * = `id'.
  */
  if ((objc_is_object_id (lhs) && TYPED_OBJECT (rhs))
      || (objc_is_object_id (rhs) && TYPED_OBJECT (lhs)))
    return 1;

  /* `id' = `Class', `Class' = `id' */

  else if ((objc_is_object_id (lhs) && objc_is_class_id (rhs))
	   || (objc_is_class_id (lhs) && objc_is_object_id (rhs)))
    return 1;

  /* `Class' != `<class> *' && `<class> *' != `Class'!  */
  else if ((OBJC_TYPE_NAME (lhs) == objc_class_id && TYPED_OBJECT (rhs))
	   || (OBJC_TYPE_NAME (rhs) == objc_class_id && TYPED_OBJECT (lhs)))
    return 0;

  /* `<class> *' = `<class> *' */

  else if (TYPED_OBJECT (lhs) && TYPED_OBJECT (rhs))
    {
      tree lname = OBJC_TYPE_NAME (lhs);
      tree rname = OBJC_TYPE_NAME (rhs);
      tree inter;

      if (lname == rname)
	return 1;

      /* If the left hand side is a super class of the right hand side,
	 allow it.  */
      for (inter = lookup_interface (rname); inter;
	   inter = lookup_interface (CLASS_SUPER_NAME (inter)))
	if (lname == CLASS_SUPER_NAME (inter))
	  return 1;

      /* Allow the reverse when reflexive.  */
      if (reflexive)
	for (inter = lookup_interface (lname); inter;
	     inter = lookup_interface (CLASS_SUPER_NAME (inter)))
	  if (rname == CLASS_SUPER_NAME (inter))
	    return 1;

      return 0;
    }
  else
    /* Not an ObjC type - let comptypes do the check.  */
    return -1;
}

/* Called from finish_decl.  */

void
objc_check_decl (tree decl)
{
  tree type = TREE_TYPE (decl);

  if (TREE_CODE (type) != RECORD_TYPE)
    return;
  if (OBJC_TYPE_NAME (type) && (type = objc_is_class_name (OBJC_TYPE_NAME (type))))
    error ("statically allocated instance of Objective-C class %qs",
	   IDENTIFIER_POINTER (type));
}

/* Construct a PROTOCOLS-qualified variant of INTERFACE, where INTERFACE may
   either name an Objective-C class, or refer to the special 'id' or 'Class'
   types.  If INTERFACE is not a valid ObjC type, just return it unchanged.  */

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
	type = xref_tag (RECORD_TYPE, type);
      else
        return interface;
    }

  if (protocols)
    {
      type = build_variant_type_copy (type);

      /* For pointers (i.e., 'id' or 'Class'), attach the protocol(s)
	 to the pointee.  */
      if (is_ptr)
	{
	  TREE_TYPE (type) = build_variant_type_copy (TREE_TYPE (type));
	  TYPE_POINTER_TO (TREE_TYPE (type)) = type;
	  type = TREE_TYPE (type);
	}

      /* Look up protocols and install in lang specific list.  */
      DUP_TYPE_OBJC_INFO (type, TYPE_MAIN_VARIANT (type));
      TYPE_OBJC_PROTOCOL_LIST (type) = lookup_and_install_protocols (protocols);

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
	pp = lookup_protocol (pp);

      if (pp == proto)
	fatal_error ("protocol %qs has circular dependency",
		     IDENTIFIER_POINTER (PROTOCOL_NAME (pp)));
      if (pp)
	check_protocol_recursively (proto, PROTOCOL_LIST (pp));
    }
}

/* Look up PROTOCOLS, and return a list of those that are found.
   If none are found, return NULL.  */

static tree
lookup_and_install_protocols (tree protocols)
{
  tree proto;
  tree return_value = NULL_TREE;

  for (proto = protocols; proto; proto = TREE_CHAIN (proto))
    {
      tree ident = TREE_VALUE (proto);
      tree p = lookup_protocol (ident);

      if (!p)
	error ("cannot find protocol declaration for %qs",
	       IDENTIFIER_POINTER (ident));
      else
	return_value = chainon (return_value,
				build_tree_list (NULL_TREE, p));
    }

  return return_value;
}

/* Create a declaration for field NAME of a given TYPE.  */

static tree
create_field_decl (tree type, const char *name)
{
  return build_decl (FIELD_DECL, get_identifier (name), type);
}

/* Create a global, static declaration for variable NAME of a given TYPE.  The
   finish_var_decl() routine will need to be called on it afterwards.  */

static tree
start_var_decl (tree type, const char *name)
{
  tree var = build_decl (VAR_DECL, get_identifier (name), type);

  TREE_STATIC (var) = 1;
  DECL_INITIAL (var) = error_mark_node;  /* A real initializer is coming... */
  DECL_IGNORED_P (var) = 1;
  DECL_ARTIFICIAL (var) = 1;
  DECL_CONTEXT (var) = NULL_TREE;
#ifdef OBJCPLUS
  DECL_THIS_STATIC (var) = 1; /* squash redeclaration errors */
#endif

  return var;
}

/* Finish off the variable declaration created by start_var_decl().  */

static void
finish_var_decl (tree var, tree initializer)
{
  finish_decl (var, initializer, NULL_TREE);
  /* Ensure that the variable actually gets output.  */
  mark_decl_referenced (var);
  /* Mark the decl to avoid "defined but not used" warning.  */
  TREE_USED (var) = 1;
}

/* Find the decl for the constant string class reference.  This is only
   used for the NeXT runtime.  */

static tree
setup_string_decl (void)
{
  char *name;
  size_t length;

  /* %s in format will provide room for terminating null */
  length = strlen (STRING_OBJECT_GLOBAL_FORMAT)
	   + strlen (constant_string_class_name);
  name = xmalloc (length);
  sprintf (name, STRING_OBJECT_GLOBAL_FORMAT,
	   constant_string_class_name);
  constant_string_global_id = get_identifier (name);
  string_class_decl = lookup_name (constant_string_global_id);

  return string_class_decl;
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
     dbxout_init hasn'r been called yet.  */
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

  type = lang_hooks.decls.pushdecl (build_decl (TYPE_DECL,
						objc_object_name,
						objc_object_type));
  DECL_IN_SYSTEM_HEADER (type) = 1;
  type = lang_hooks.decls.pushdecl (build_decl (TYPE_DECL,
						objc_class_name,
						objc_class_type));
  DECL_IN_SYSTEM_HEADER (type) = 1;

  /* Forward-declare '@interface Protocol'.  */

  type = get_identifier (PROTOCOL_OBJECT_CLASS_NAME);
  objc_declare_class (tree_cons (NULL_TREE, type, NULL_TREE));
  objc_protocol_type = build_pointer_type (xref_tag (RECORD_TYPE,
                                type));

  /* Declare type of selector-objects that represent an operation name.  */

  if (flag_next_runtime)
    /* `struct objc_selector *' */
    objc_selector_type
      = build_pointer_type (xref_tag (RECORD_TYPE,
				      get_identifier (TAG_SELECTOR)));
  else
    /* `const struct objc_selector *' */
    objc_selector_type
      = build_pointer_type
	(build_qualified_type (xref_tag (RECORD_TYPE,
					 get_identifier (TAG_SELECTOR)),
			       TYPE_QUAL_CONST));

  /* Declare receiver type used for dispatching messages to 'super'.  */

  /* `struct objc_super *' */
  objc_super_type = build_pointer_type (xref_tag (RECORD_TYPE,
						  get_identifier (TAG_SUPER)));

  if (flag_next_runtime)
    {
      /* NB: In order to call one of the ..._stret (struct-returning)
      functions, the function *MUST* first be cast to a signature that
      corresponds to the actual ObjC method being invoked.  This is
      what is done by the build_objc_method_call() routine below.  */

      /* id objc_msgSend (id, SEL, ...); */
      /* id objc_msgSendNonNil (id, SEL, ...); */
      /* id objc_msgSend_stret (id, SEL, ...); */
      /* id objc_msgSendNonNil_stret (id, SEL, ...); */
      type
	= build_function_type (objc_object_type,
			       tree_cons (NULL_TREE, objc_object_type,
					  tree_cons (NULL_TREE, objc_selector_type,
						     NULL_TREE)));
      umsg_decl = builtin_function (TAG_MSGSEND,
				    type, 0, NOT_BUILT_IN,
				    NULL, NULL_TREE);
      umsg_nonnil_decl = builtin_function (TAG_MSGSEND_NONNIL,
					   type, 0, NOT_BUILT_IN,
					   NULL, NULL_TREE);
      umsg_stret_decl = builtin_function (TAG_MSGSEND_STRET,
					  type, 0, NOT_BUILT_IN,
					  NULL, NULL_TREE);
      umsg_nonnil_stret_decl = builtin_function (TAG_MSGSEND_NONNIL_STRET,
						 type, 0, NOT_BUILT_IN,
						 NULL, NULL_TREE);

      /* id objc_msgSendSuper (struct objc_super *, SEL, ...); */
      /* id objc_msgSendSuper_stret (struct objc_super *, SEL, ...); */
      type
	= build_function_type (objc_object_type,
			       tree_cons (NULL_TREE, objc_super_type,
					  tree_cons (NULL_TREE, objc_selector_type,
						     NULL_TREE)));
      umsg_super_decl = builtin_function (TAG_MSGSENDSUPER,
					  type, 0, NOT_BUILT_IN,
					  NULL, NULL_TREE);
      umsg_super_stret_decl = builtin_function (TAG_MSGSENDSUPER_STRET,
						type, 0, NOT_BUILT_IN, 0,
						NULL_TREE);
    }
  else
    {
      /* GNU runtime messenger entry points.  */

      /* typedef id (*IMP)(id, SEL, ...); */
      tree IMP_type
	= build_pointer_type
	  (build_function_type (objc_object_type,      
				tree_cons (NULL_TREE, objc_object_type,      
					   tree_cons (NULL_TREE, objc_selector_type,      
						      NULL_TREE))));      

      /* IMP objc_msg_lookup (id, SEL); */
      type
        = build_function_type (IMP_type,
			       tree_cons (NULL_TREE, objc_object_type,
					  tree_cons (NULL_TREE, objc_selector_type,
						     OBJC_VOID_AT_END)));
      umsg_decl = builtin_function (TAG_MSGSEND,
				    type, 0, NOT_BUILT_IN,
				    NULL, NULL_TREE);

      /* IMP objc_msg_lookup_super (struct objc_super *, SEL); */
      type
        = build_function_type (IMP_type,
			       tree_cons (NULL_TREE, objc_super_type,
					  tree_cons (NULL_TREE, objc_selector_type,
						     OBJC_VOID_AT_END)));
      umsg_super_decl = builtin_function (TAG_MSGSENDSUPER,
					  type, 0, NOT_BUILT_IN,
					  NULL, NULL_TREE);

      /* The following GNU runtime entry point is called to initialize
	 each module:

	 __objc_exec_class (void *); */
      type
	= build_function_type (void_type_node,
			       tree_cons (NULL_TREE, ptr_type_node,
					  OBJC_VOID_AT_END));
      execclass_decl = builtin_function (TAG_EXECCLASS,
					 type, 0, NOT_BUILT_IN,
					 NULL, NULL_TREE);
    }

  /* id objc_getClass (const char *); */

  type = build_function_type (objc_object_type,
				   tree_cons (NULL_TREE,
					      const_string_type_node,
					      OBJC_VOID_AT_END));

  objc_get_class_decl
    = builtin_function (TAG_GETCLASS, type, 0, NOT_BUILT_IN,
			NULL, NULL_TREE);

  /* id objc_getMetaClass (const char *); */

  objc_get_meta_class_decl
    = builtin_function (TAG_GETMETACLASS, type, 0, NOT_BUILT_IN, NULL, NULL_TREE);

  build_class_template ();
  build_super_template ();
  build_protocol_template ();
  build_category_template ();
  build_objc_exception_stuff ();

  if (flag_next_runtime)
    build_next_objc_exception_stuff ();

  /* static SEL _OBJC_SELECTOR_TABLE[]; */

  if (! flag_next_runtime)
    build_selector_table_decl ();

  /* Forward declare constant_string_id and constant_string_type.  */
  if (!constant_string_class_name)
    constant_string_class_name = default_constant_string_class_name;

  constant_string_id = get_identifier (constant_string_class_name);
  objc_declare_class (tree_cons (NULL_TREE, constant_string_id, NULL_TREE));

  /* Pre-build the following entities - for speed/convenience.  */
  self_id = get_identifier ("self");
  ucmd_id = get_identifier ("_cmd");
#ifndef OBJCPLUS
  /* The C++ front-end does not appear to grok __attribute__((__unused__)).  */
  unused_list = build_tree_list (get_identifier ("__unused__"), NULL_TREE);
#endif	  

#ifdef OBJCPLUS
  pop_lang_context ();
#endif

  write_symbols = save_write_symbols;
  debug_hooks = save_hooks;
}

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
  tree field_decl = TYPE_FIELDS (constant_string_type);

#define AT_LEAST_AS_LARGE_AS(F, T) \
  (F && TREE_CODE (F) == FIELD_DECL \
     && (TREE_INT_CST_LOW (DECL_SIZE (F)) \
	 >= TREE_INT_CST_LOW (TYPE_SIZE (T))))

  if (!AT_LEAST_AS_LARGE_AS (field_decl, ptr_type_node))
    return 0;

  field_decl = TREE_CHAIN (field_decl);
  if (!AT_LEAST_AS_LARGE_AS (field_decl, ptr_type_node))
    return 0;

  field_decl = TREE_CHAIN (field_decl);
  return AT_LEAST_AS_LARGE_AS (field_decl, unsigned_type_node);

#undef AT_LEAST_AS_LARGE_AS
}

/* Avoid calling `check_string_class_template ()' more than once.  */
static GTY(()) int string_layout_checked;

/* Custom build_string which sets TREE_TYPE!  */

static tree
my_build_string (int len, const char *str)
{
  return fix_string_type (build_string (len, str));
}


static hashval_t
string_hash (const void *ptr)
{
  tree str = ((struct string_descriptor *)ptr)->literal;
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
  tree str1 = ((struct string_descriptor *)ptr1)->literal;
  tree str2 = ((struct string_descriptor *)ptr2)->literal;
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
  tree initlist, constructor, constant_string_class;
  int length;
  tree fields, addr;
  struct string_descriptor *desc, key;
  void **loc;

  /* Prep the string argument.  */
  string = fix_string_type (string);
  TREE_SET_CODE (string, STRING_CST);
  length = TREE_STRING_LENGTH (string) - 1;

  /* Check whether the string class being used actually exists and has the
     correct ivar layout.  */
  if (!string_layout_checked)
    {
      string_layout_checked = -1;
      constant_string_class = lookup_interface (constant_string_id);

      if (!constant_string_class
	  || !(constant_string_type
	       = CLASS_STATIC_TEMPLATE (constant_string_class)))
	error ("cannot find interface declaration for %qs",
	       IDENTIFIER_POINTER (constant_string_id));
      /* The NSConstantString/NXConstantString ivar layout is now known.  */
      else if (!check_string_class_template ())
	error ("interface %qs does not have valid constant string layout",
	       IDENTIFIER_POINTER (constant_string_id));
      /* For the NeXT runtime, we can generate a literal reference
	 to the string class, don't need to run a constructor.  */
      else if (flag_next_runtime && !setup_string_decl ())
	error ("cannot find reference tag for class %qs",
	       IDENTIFIER_POINTER (constant_string_id));
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
  desc = *loc;

  if (!desc)
    {
      tree var;
      *loc = desc = ggc_alloc (sizeof (*desc));
      desc->literal = string;

      /* GNU:    & ((NXConstantString) { NULL, string, length })  */
      /* NeXT:   & ((NSConstantString) { isa, string, length })   */
      fields = TYPE_FIELDS (constant_string_type);
      initlist
	= build_tree_list (fields,
			   flag_next_runtime
			   ? build_unary_op (ADDR_EXPR, string_class_decl, 0)
			   : build_int_cst (NULL_TREE, 0));
      fields = TREE_CHAIN (fields);
      initlist = tree_cons (fields, build_unary_op (ADDR_EXPR, string, 1),
			    initlist);
      fields = TREE_CHAIN (fields);
      initlist = tree_cons (fields, build_int_cst (NULL_TREE, length),
 			    initlist);
      constructor = objc_build_constructor (constant_string_type,
					    nreverse (initlist));
      TREE_INVARIANT (constructor) = true;

      if (!flag_next_runtime)
	constructor
	  = objc_add_static_instance (constructor, constant_string_type);
      else
        {
	  var = build_decl (CONST_DECL, NULL, TREE_TYPE (constructor));
	  DECL_INITIAL (var) = constructor;
	  TREE_STATIC (var) = 1;
	  pushdecl_top_level (var);
	  constructor = var;
	}
      desc->constructor = constructor;
    }

  addr = build_unary_op (ADDR_EXPR, desc->constructor, 1);

  return addr;
}

/* Declare a static instance of CLASS_DECL initialized by CONSTRUCTOR.  */

static GTY(()) int num_static_inst;

static tree
objc_add_static_instance (tree constructor, tree class_decl)
{
  tree *chain, decl;
  char buf[256];

  /* Find the list of static instances for the CLASS_DECL.  Create one if
     not found.  */
  for (chain = &objc_static_instances;
       *chain && TREE_VALUE (*chain) != class_decl;
       chain = &TREE_CHAIN (*chain));
  if (!*chain)
    {
      *chain = tree_cons (NULL_TREE, class_decl, NULL_TREE);
      add_objc_string (OBJC_TYPE_NAME (class_decl), class_names);
    }

  sprintf (buf, "_OBJC_INSTANCE_%d", num_static_inst++);
  decl = build_decl (VAR_DECL, get_identifier (buf), class_decl);
  DECL_COMMON (decl) = 1;
  TREE_STATIC (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  DECL_INITIAL (decl) = constructor;

  /* We may be writing something else just now.
     Postpone till end of input.  */
  DECL_DEFER_OUTPUT (decl) = 1;
  pushdecl_top_level (decl);
  rest_of_decl_compilation (decl, 1, 0);

  /* Add the DECL to the head of this CLASS' list.  */
  TREE_PURPOSE (*chain) = tree_cons (NULL_TREE, decl, TREE_PURPOSE (*chain));

  return decl;
}

/* Build a static constant CONSTRUCTOR
   with type TYPE and elements ELTS.  */

static tree
objc_build_constructor (tree type, tree elts)
{
  tree constructor = build_constructor (type, elts);

  TREE_CONSTANT (constructor) = 1;
  TREE_STATIC (constructor) = 1;
  TREE_READONLY (constructor) = 1;

#ifdef OBJCPLUS
  /* Adjust for impedance mismatch.  We should figure out how to build
     CONSTRUCTORs that consistently please both the C and C++ gods.  */
  if (!TREE_PURPOSE (elts))
    TREE_TYPE (constructor) = NULL_TREE;
  TREE_HAS_CONSTRUCTOR (constructor) = 1;
#endif

  return constructor;
}

/* Take care of defining and initializing _OBJC_SYMBOLS.  */

/* Predefine the following data type:

   struct _objc_symtab
   {
     long sel_ref_cnt;
     SEL *refs;
     short cls_def_cnt;
     short cat_def_cnt;
     void *defs[cls_def_cnt + cat_def_cnt];
   }; */

static void
build_objc_symtab_template (void)
{
  tree field_decl, field_decl_chain;

  objc_symtab_template
    = start_struct (RECORD_TYPE, get_identifier (UTAG_SYMTAB));

  /* long sel_ref_cnt; */
  field_decl = create_field_decl (long_integer_type_node, "sel_ref_cnt");
  field_decl_chain = field_decl;

  /* SEL *refs; */
  field_decl = create_field_decl (build_pointer_type (objc_selector_type),
				  "refs");
  chainon (field_decl_chain, field_decl);

  /* short cls_def_cnt; */
  field_decl = create_field_decl (short_integer_type_node, "cls_def_cnt");
  chainon (field_decl_chain, field_decl);

  /* short cat_def_cnt; */
  field_decl = create_field_decl (short_integer_type_node,
				  "cat_def_cnt");
  chainon (field_decl_chain, field_decl);

  if (imp_count || cat_count || !flag_next_runtime)
    {
      /* void *defs[imp_count + cat_count (+ 1)]; */
      /* NB: The index is one less than the size of the array.  */
      int index = imp_count + cat_count
		+ (flag_next_runtime? -1: 0);
      field_decl = create_field_decl
		   (build_array_type
		    (ptr_type_node,
		     build_index_type (build_int_cst (NULL_TREE, index))),
		    "defs");
      chainon (field_decl_chain, field_decl);
    }

  finish_struct (objc_symtab_template, field_decl_chain, NULL_TREE);
}

/* Create the initial value for the `defs' field of _objc_symtab.
   This is a CONSTRUCTOR.  */

static tree
init_def_list (tree type)
{
  tree expr, initlist = NULL_TREE;
  struct imp_entry *impent;

  if (imp_count)
    for (impent = imp_list; impent; impent = impent->next)
      {
	if (TREE_CODE (impent->imp_context) == CLASS_IMPLEMENTATION_TYPE)
	  {
	    expr = build_unary_op (ADDR_EXPR, impent->class_decl, 0);
	    initlist = tree_cons (NULL_TREE, expr, initlist);
	  }
      }

  if (cat_count)
    for (impent = imp_list; impent; impent = impent->next)
      {
	if (TREE_CODE (impent->imp_context) == CATEGORY_IMPLEMENTATION_TYPE)
	  {
	    expr = build_unary_op (ADDR_EXPR, impent->class_decl, 0);
	    initlist = tree_cons (NULL_TREE, expr, initlist);
	  }
      }

  if (!flag_next_runtime)
    {
      /* statics = { ..., _OBJC_STATIC_INSTANCES, ... }  */
      tree expr;

      if (static_instances_decl)
	expr = build_unary_op (ADDR_EXPR, static_instances_decl, 0);
      else
	expr = build_int_cst (NULL_TREE, 0);

      initlist = tree_cons (NULL_TREE, expr, initlist);
    }

  return objc_build_constructor (type, nreverse (initlist));
}

/* Construct the initial value for all of _objc_symtab.  */

static tree
init_objc_symtab (tree type)
{
  tree initlist;

  /* sel_ref_cnt = { ..., 5, ... } */

  initlist = build_tree_list (NULL_TREE,
			      build_int_cst (long_integer_type_node, 0));

  /* refs = { ..., _OBJC_SELECTOR_TABLE, ... } */

  if (flag_next_runtime || ! sel_ref_chain)
    initlist = tree_cons (NULL_TREE, build_int_cst (NULL_TREE, 0), initlist);
  else
    initlist
      = tree_cons (NULL_TREE,
		   convert (build_pointer_type (objc_selector_type),
			    build_unary_op (ADDR_EXPR,
					    UOBJC_SELECTOR_TABLE_decl, 1)),
		   initlist);

  /* cls_def_cnt = { ..., 5, ... } */

  initlist = tree_cons (NULL_TREE, build_int_cst (NULL_TREE, imp_count), initlist);

  /* cat_def_cnt = { ..., 5, ... } */

  initlist = tree_cons (NULL_TREE, build_int_cst (NULL_TREE, cat_count), initlist);

  /* cls_def = { ..., { &Foo, &Bar, ...}, ... } */

  if (imp_count || cat_count || !flag_next_runtime)
    {

      tree field = TYPE_FIELDS (type);
      field = TREE_CHAIN (TREE_CHAIN (TREE_CHAIN (TREE_CHAIN (field))));

      initlist = tree_cons (NULL_TREE, init_def_list (TREE_TYPE (field)),
			    initlist);
    }

  return objc_build_constructor (type, nreverse (initlist));
}

/* Generate forward declarations for metadata such as
  'OBJC_CLASS_...'.  */

static tree
build_metadata_decl (const char *name, tree type)
{
  tree decl;

  /* struct TYPE NAME_<name>; */
  decl = start_var_decl (type, synth_id_with_class_suffix
			       (name,
				objc_implementation_context));

  return decl;
}

/* Push forward-declarations of all the categories so that
   init_def_list can use them in a CONSTRUCTOR.  */

static void
forward_declare_categories (void)
{
  struct imp_entry *impent;
  tree sav = objc_implementation_context;

  for (impent = imp_list; impent; impent = impent->next)
    {
      if (TREE_CODE (impent->imp_context) == CATEGORY_IMPLEMENTATION_TYPE)
	{
	  /* Set an invisible arg to synth_id_with_class_suffix.  */
	  objc_implementation_context = impent->imp_context;
	  /* extern struct objc_category _OBJC_CATEGORY_<name>; */
	  impent->class_decl = build_metadata_decl ("_OBJC_CATEGORY",
						    objc_category_template);
	}
    }
  objc_implementation_context = sav;
}

/* Create the declaration of _OBJC_SYMBOLS, with type `struct _objc_symtab'
   and initialized appropriately.  */

static void
generate_objc_symtab_decl (void)
{
  /* forward declare categories */
  if (cat_count)
    forward_declare_categories ();

  build_objc_symtab_template ();
  UOBJC_SYMBOLS_decl = start_var_decl (objc_symtab_template, "_OBJC_SYMBOLS");
  finish_var_decl (UOBJC_SYMBOLS_decl,
		   init_objc_symtab (TREE_TYPE (UOBJC_SYMBOLS_decl)));
}

static tree
init_module_descriptor (tree type)
{
  tree initlist, expr;

  /* version = { 1, ... } */

  expr = build_int_cst (long_integer_type_node, OBJC_VERSION);
  initlist = build_tree_list (NULL_TREE, expr);

  /* size = { ..., sizeof (struct _objc_module), ... } */

  expr = convert (long_integer_type_node,
		  size_in_bytes (objc_module_template));
  initlist = tree_cons (NULL_TREE, expr, initlist);

  /* name = { ..., "foo.m", ... } */

  expr = add_objc_string (get_identifier (input_filename), class_names);
  initlist = tree_cons (NULL_TREE, expr, initlist);

  /* symtab = { ..., _OBJC_SYMBOLS, ... } */

  if (UOBJC_SYMBOLS_decl)
    expr = build_unary_op (ADDR_EXPR, UOBJC_SYMBOLS_decl, 0);
  else
    expr = build_int_cst (NULL_TREE, 0);
  initlist = tree_cons (NULL_TREE, expr, initlist);

  return objc_build_constructor (type, nreverse (initlist));
}

/* Write out the data structures to describe Objective C classes defined.

   struct _objc_module { ... } _OBJC_MODULE = { ... };   */

static void
build_module_descriptor (void)
{
  tree field_decl, field_decl_chain;

#ifdef OBJCPLUS
  push_lang_context (lang_name_c); /* extern "C" */
#endif

  objc_module_template
    = start_struct (RECORD_TYPE, get_identifier (UTAG_MODULE));

  /* long version; */
  field_decl = create_field_decl (long_integer_type_node, "version");
  field_decl_chain = field_decl;

  /* long size; */
  field_decl = create_field_decl (long_integer_type_node, "size");
  chainon (field_decl_chain, field_decl);

  /* char *name; */
  field_decl = create_field_decl (string_type_node, "name");
  chainon (field_decl_chain, field_decl);

  /* struct _objc_symtab *symtab; */
  field_decl
    = create_field_decl (build_pointer_type
			 (xref_tag (RECORD_TYPE,
				    get_identifier (UTAG_SYMTAB))),
			 "symtab");
  chainon (field_decl_chain, field_decl);

  finish_struct (objc_module_template, field_decl_chain, NULL_TREE);

  /* Create an instance of "_objc_module".  */
  UOBJC_MODULES_decl = start_var_decl (objc_module_template, "_OBJC_MODULES");
  finish_var_decl (UOBJC_MODULES_decl,
		   init_module_descriptor (TREE_TYPE (UOBJC_MODULES_decl)));

#ifdef OBJCPLUS
  pop_lang_context ();
#endif
}

/* The GNU runtime requires us to provide a static initializer function
   for each module:

   static void __objc_gnu_init (void) {
     __objc_exec_class (&L_OBJC_MODULES);
   }  */ 

static void
build_module_initializer_routine (void)
{
  tree body;

#ifdef OBJCPLUS
  push_lang_context (lang_name_c); /* extern "C" */
#endif                                     

  objc_push_parm (build_decl (PARM_DECL, NULL_TREE, void_type_node));
  objc_start_function (get_identifier (TAG_GNUINIT),
		       build_function_type (void_type_node,
					    OBJC_VOID_AT_END),
		       NULL_TREE, objc_get_parm_info (0));

  body = c_begin_compound_stmt (true);
  add_stmt (build_function_call
	    (execclass_decl,
	     build_tree_list
	     (NULL_TREE,
	      build_unary_op (ADDR_EXPR,
			      UOBJC_MODULES_decl, 0))));
  add_stmt (c_end_compound_stmt (body, true));

  TREE_PUBLIC (current_function_decl) = 0;

#ifndef OBJCPLUS
  /* For Objective-C++, we will need to call __objc_gnu_init
     from objc_generate_static_init_call() below.  */
  DECL_STATIC_CONSTRUCTOR (current_function_decl) = 1;
#endif

  GNU_INIT_decl = current_function_decl;
  finish_function ();

#ifdef OBJCPLUS
    pop_lang_context ();
#endif
}

#ifdef OBJCPLUS
/* Return 1 if the __objc_gnu_init function has been synthesized and needs
   to be called by the module initializer routine.  */

int
objc_static_init_needed_p (void)
{
  return (GNU_INIT_decl != NULL_TREE);
}

/* Generate a call to the __objc_gnu_init initializer function.  */

tree
objc_generate_static_init_call (tree ctors ATTRIBUTE_UNUSED)
{
  add_stmt (build_stmt (EXPR_STMT,
			build_function_call (GNU_INIT_decl, NULL_TREE)));

  return ctors;
}
#endif /* OBJCPLUS */

/* Return the DECL of the string IDENT in the SECTION.  */

static tree
get_objc_string_decl (tree ident, enum string_section section)
{
  tree chain;

  if (section == class_names)
    chain = class_names_chain;
  else if (section == meth_var_names)
    chain = meth_var_names_chain;
  else if (section == meth_var_types)
    chain = meth_var_types_chain;
  else
    abort ();

  for (; chain != 0; chain = TREE_CHAIN (chain))
    if (TREE_VALUE (chain) == ident)
      return (TREE_PURPOSE (chain));

  abort ();
  return NULL_TREE;
}

/* Output references to all statically allocated objects.  Return the DECL
   for the array built.  */

static void
generate_static_references (void)
{
  tree decls = NULL_TREE, expr = NULL_TREE;
  tree class_name, class, decl, initlist;
  tree cl_chain, in_chain, type
    = build_array_type (build_pointer_type (void_type_node), NULL_TREE);
  int num_inst, num_class;
  char buf[256];

  if (flag_next_runtime)
    abort ();

  for (cl_chain = objc_static_instances, num_class = 0;
       cl_chain; cl_chain = TREE_CHAIN (cl_chain), num_class++)
    {
      for (num_inst = 0, in_chain = TREE_PURPOSE (cl_chain);
	   in_chain; num_inst++, in_chain = TREE_CHAIN (in_chain));

      sprintf (buf, "_OBJC_STATIC_INSTANCES_%d", num_class);
      decl = start_var_decl (type, buf);

      /* Output {class_name, ...}.  */
      class = TREE_VALUE (cl_chain);
      class_name = get_objc_string_decl (OBJC_TYPE_NAME (class), class_names);
      initlist = build_tree_list (NULL_TREE,
				  build_unary_op (ADDR_EXPR, class_name, 1));

      /* Output {..., instance, ...}.  */
      for (in_chain = TREE_PURPOSE (cl_chain);
	   in_chain; in_chain = TREE_CHAIN (in_chain))
	{
	  expr = build_unary_op (ADDR_EXPR, TREE_VALUE (in_chain), 1);
	  initlist = tree_cons (NULL_TREE, expr, initlist);
	}

      /* Output {..., NULL}.  */
      initlist = tree_cons (NULL_TREE, build_int_cst (NULL_TREE, 0), initlist);

      expr = objc_build_constructor (TREE_TYPE (decl), nreverse (initlist));
      finish_var_decl (decl, expr);
      decls
	= tree_cons (NULL_TREE, build_unary_op (ADDR_EXPR, decl, 1), decls);
    }

  decls = tree_cons (NULL_TREE, build_int_cst (NULL_TREE, 0), decls);
  expr = objc_build_constructor (type, nreverse (decls));
  static_instances_decl = start_var_decl (type, "_OBJC_STATIC_INSTANCES");
  finish_var_decl (static_instances_decl, expr);
}

/* Output all strings.  */

static void
generate_strings (void)
{
  tree chain, string_expr;
  tree string, decl, type;

  for (chain = class_names_chain; chain; chain = TREE_CHAIN (chain))
    {
      string = TREE_VALUE (chain);
      decl = TREE_PURPOSE (chain);
      type = build_array_type
	     (char_type_node,
	      build_index_type
	      (build_int_cst (NULL_TREE, 
			      IDENTIFIER_LENGTH (string))));
      decl = start_var_decl (type, IDENTIFIER_POINTER (DECL_NAME (decl)));
      string_expr = my_build_string (IDENTIFIER_LENGTH (string) + 1,
				     IDENTIFIER_POINTER (string));
      finish_var_decl (decl, string_expr);
    }

  for (chain = meth_var_names_chain; chain; chain = TREE_CHAIN (chain))
    {
      string = TREE_VALUE (chain);
      decl = TREE_PURPOSE (chain);
      type = build_array_type
	     (char_type_node,
	      build_index_type
	      (build_int_cst (NULL_TREE,
			      IDENTIFIER_LENGTH (string))));
      decl = start_var_decl (type, IDENTIFIER_POINTER (DECL_NAME (decl)));
      string_expr = my_build_string (IDENTIFIER_LENGTH (string) + 1,
				     IDENTIFIER_POINTER (string));
      finish_var_decl (decl, string_expr);
    }

  for (chain = meth_var_types_chain; chain; chain = TREE_CHAIN (chain))
    {
      string = TREE_VALUE (chain);
      decl = TREE_PURPOSE (chain);
      type = build_array_type
	     (char_type_node,
	      build_index_type
	      (build_int_cst (NULL_TREE,
			      IDENTIFIER_LENGTH (string))));
      decl = start_var_decl (type, IDENTIFIER_POINTER (DECL_NAME (decl)));
      string_expr = my_build_string (IDENTIFIER_LENGTH (string) + 1,
				     IDENTIFIER_POINTER (string));
      finish_var_decl (decl, string_expr);
    }
}

static GTY(()) int selector_reference_idx;

static tree
build_selector_reference_decl (void)
{
  tree decl;
  char buf[256];

  sprintf (buf, "_OBJC_SELECTOR_REFERENCES_%d", selector_reference_idx++);
  decl = start_var_decl (objc_selector_type, buf);

  return decl;
}

static void
build_selector_table_decl (void)
{
  tree temp;

  if (flag_typed_selectors)
    {
      build_selector_template ();
      temp = build_array_type (objc_selector_template, NULL_TREE);
    }
  else
    temp = build_array_type (objc_selector_type, NULL_TREE);

  UOBJC_SELECTOR_TABLE_decl = start_var_decl (temp, "_OBJC_SELECTOR_TABLE");
}

/* Just a handy wrapper for add_objc_string.  */

static tree
build_selector (tree ident)
{
  return convert (objc_selector_type,
		  add_objc_string (ident, meth_var_names));
}

static void
build_selector_translation_table (void)
{
  tree chain, initlist = NULL_TREE;
  int offset = 0;
  tree decl = NULL_TREE;

  for (chain = sel_ref_chain; chain; chain = TREE_CHAIN (chain))
    {
      tree expr;

      if (warn_selector && objc_implementation_context)
      {
        tree method_chain;
        bool found = false;
        for (method_chain = meth_var_names_chain;
             method_chain;
             method_chain = TREE_CHAIN (method_chain))
          {
            if (TREE_VALUE (method_chain) == TREE_VALUE (chain))
              {
                found = true;
                break;
              }
          }
        if (!found)
	  {
	    location_t *loc;
	    if (flag_next_runtime && TREE_PURPOSE (chain))
	      loc = &DECL_SOURCE_LOCATION (TREE_PURPOSE (chain));
	    else
	      loc = &input_location;
	    warning ("%Hcreating selector for nonexistent method %qE",
		     loc, TREE_VALUE (chain));
	  }
      }

      expr = build_selector (TREE_VALUE (chain));
      /* add one for the '\0' character */
      offset += IDENTIFIER_LENGTH (TREE_VALUE (chain)) + 1;

      if (flag_next_runtime)
	{
	  decl = TREE_PURPOSE (chain);
	  finish_var_decl (decl, expr);
	}
      else
	{
	  if (flag_typed_selectors)
	    {
	      tree eltlist = NULL_TREE;
	      tree encoding = get_proto_encoding (TREE_PURPOSE (chain));
	      eltlist = tree_cons (NULL_TREE, expr, NULL_TREE);
	      eltlist = tree_cons (NULL_TREE, encoding, eltlist);
	      expr = objc_build_constructor (objc_selector_template,
					     nreverse (eltlist));
	    }

	  initlist = tree_cons (NULL_TREE, expr, initlist);
	}
    }

  if (! flag_next_runtime)
    {
      /* Cause the selector table (previously forward-declared)
	 to be actually output.  */
      initlist = tree_cons (NULL_TREE,
			    flag_typed_selectors
			    ? objc_build_constructor
			      (objc_selector_template,
			       tree_cons (NULL_TREE,
					  build_int_cst (NULL_TREE, 0),
					  tree_cons (NULL_TREE,
						     build_int_cst (NULL_TREE, 0), 
						     NULL_TREE)))
			    : build_int_cst (NULL_TREE, 0), initlist);
      initlist = objc_build_constructor (TREE_TYPE (UOBJC_SELECTOR_TABLE_decl),
					 nreverse (initlist));
      finish_var_decl (UOBJC_SELECTOR_TABLE_decl, initlist);
    }
}

static tree
get_proto_encoding (tree proto)
{
  tree encoding;
  if (proto)
    {
      if (! METHOD_ENCODING (proto))
	{
	  encoding = encode_method_prototype (proto);
	  METHOD_ENCODING (proto) = encoding;
	}
      else
	encoding = METHOD_ENCODING (proto);

      return add_objc_string (encoding, meth_var_types);
    }
  else
    return build_int_cst (NULL_TREE, 0);
}

/* sel_ref_chain is a list whose "value" fields will be instances of
   identifier_node that represent the selector.  */

static tree
build_typed_selector_reference (tree ident, tree prototype)
{
  tree *chain = &sel_ref_chain;
  tree expr;
  int index = 0;

  while (*chain)
    {
      if (TREE_PURPOSE (*chain) == prototype && TREE_VALUE (*chain) == ident)
	goto return_at_index;

      index++;
      chain = &TREE_CHAIN (*chain);
    }

  *chain = tree_cons (prototype, ident, NULL_TREE);

 return_at_index:
  expr = build_unary_op (ADDR_EXPR,
			 build_array_ref (UOBJC_SELECTOR_TABLE_decl,
					  build_int_cst (NULL_TREE, index)),
			 1);
  return convert (objc_selector_type, expr);
}

static tree
build_selector_reference (tree ident)
{
  tree *chain = &sel_ref_chain;
  tree expr;
  int index = 0;

  while (*chain)
    {
      if (TREE_VALUE (*chain) == ident)
	return (flag_next_runtime
		? TREE_PURPOSE (*chain)
		: build_array_ref (UOBJC_SELECTOR_TABLE_decl,
				   build_int_cst (NULL_TREE, index)));

      index++;
      chain = &TREE_CHAIN (*chain);
    }

  expr = (flag_next_runtime ? build_selector_reference_decl (): NULL_TREE);

  *chain = tree_cons (expr, ident, NULL_TREE);

  return (flag_next_runtime
	  ? expr
	  : build_array_ref (UOBJC_SELECTOR_TABLE_decl,
			     build_int_cst (NULL_TREE, index)));
}

static GTY(()) int class_reference_idx;

static tree
build_class_reference_decl (void)
{
  tree decl;
  char buf[256];

  sprintf (buf, "_OBJC_CLASS_REFERENCES_%d", class_reference_idx++);
  decl = start_var_decl (objc_class_type, buf);

  return decl;
}

/* Create a class reference, but don't create a variable to reference
   it.  */

static void
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
  tree orig_ident;

#ifdef OBJCPLUS
  if (processing_template_decl)
    /* Must wait until template instantiation time.  */
    return build_min_nt (CLASS_REFERENCE_EXPR, ident);
  if (TREE_CODE (ident) == TYPE_DECL)
    ident = DECL_NAME (ident);
#endif
  orig_ident = ident;

  if (!(ident = objc_is_class_name (ident)))
    {
      error ("%qs is not an Objective-C class name or alias",
	     IDENTIFIER_POINTER (orig_ident));
      return error_mark_node;
    }

  if (flag_next_runtime && !flag_zero_link)
    {
      tree *chain;
      tree decl;

      for (chain = &cls_ref_chain; *chain; chain = &TREE_CHAIN (*chain))
	if (TREE_VALUE (*chain) == ident)
	  {
	    if (! TREE_PURPOSE (*chain))
	      TREE_PURPOSE (*chain) = build_class_reference_decl ();

	    return TREE_PURPOSE (*chain);
	  }

      decl = build_class_reference_decl ();
      *chain = tree_cons (decl, ident, NULL_TREE);
      return decl;
    }
  else
    {
      tree params;

      add_class_reference (ident);

      params = build_tree_list (NULL_TREE,
				my_build_string (IDENTIFIER_LENGTH (ident) + 1,
						 IDENTIFIER_POINTER (ident)));

      assemble_external (objc_get_class_decl);
      return build_function_call (objc_get_class_decl, params);
    }
}

/* For each string section we have a chain which maps identifier nodes
   to decls for the strings.  */

static tree
add_objc_string (tree ident, enum string_section section)
{
  tree *chain, decl;

  if (section == class_names)
    chain = &class_names_chain;
  else if (section == meth_var_names)
    chain = &meth_var_names_chain;
  else if (section == meth_var_types)
    chain = &meth_var_types_chain;
  else
    abort ();

  while (*chain)
    {
      if (TREE_VALUE (*chain) == ident)
	return convert (string_type_node,
			build_unary_op (ADDR_EXPR, TREE_PURPOSE (*chain), 1));

      chain = &TREE_CHAIN (*chain);
    }

  decl = build_objc_string_decl (section);

  *chain = tree_cons (decl, ident, NULL_TREE);

  return convert (string_type_node, build_unary_op (ADDR_EXPR, decl, 1));
}

static GTY(()) int class_names_idx;
static GTY(()) int meth_var_names_idx;
static GTY(()) int meth_var_types_idx;

static tree
build_objc_string_decl (enum string_section section)
{
  tree decl, ident;
  char buf[256];

  if (section == class_names)
    sprintf (buf, "_OBJC_CLASS_NAME_%d", class_names_idx++);
  else if (section == meth_var_names)
    sprintf (buf, "_OBJC_METH_VAR_NAME_%d", meth_var_names_idx++);
  else if (section == meth_var_types)
    sprintf (buf, "_OBJC_METH_VAR_TYPE_%d", meth_var_types_idx++);

  ident = get_identifier (buf);

  decl = build_decl (VAR_DECL, ident, build_array_type (char_type_node, 0));
  DECL_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 0;
  TREE_USED (decl) = 1;
  TREE_CONSTANT (decl) = 1;
  DECL_CONTEXT (decl) = 0;
  DECL_ARTIFICIAL (decl) = 1;
#ifdef OBJCPLUS
  DECL_THIS_STATIC (decl) = 1; /* squash redeclaration errors */
#endif  

  make_decl_rtl (decl);
  pushdecl_top_level (decl);

  return decl;
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
    warning ("cannot find class %qs", IDENTIFIER_POINTER (class_ident));
  else if (objc_is_class_name (alias_ident))
    warning ("class %qs already exists", IDENTIFIER_POINTER (alias_ident));
  else
    alias_chain = tree_cons (underlying_class, alias_ident, alias_chain);
}

void
objc_declare_class (tree ident_list)
{
  tree list;
#ifdef OBJCPLUS
  if (current_namespace != global_namespace) {
    error ("Objective-C declarations may only appear in global scope");
  }
#endif /* OBJCPLUS */

  for (list = ident_list; list; list = TREE_CHAIN (list))
    {
      tree ident = TREE_VALUE (list);

      if (! objc_is_class_name (ident))
	{
	  tree record = lookup_name (ident), type = record;

	  if (record)
	    {
	      if (TREE_CODE (record) == TYPE_DECL)
		type = DECL_ORIGINAL_TYPE (record);

	      if (!TYPE_HAS_OBJC_INFO (type)
		  || !TYPE_OBJC_INTERFACE (type))
		{
		  error ("%qs redeclared as different kind of symbol",
			 IDENTIFIER_POINTER (ident));
		  error ("%Jprevious declaration of '%D'",
			 record, record);
		}
	    }

	  record = xref_tag (RECORD_TYPE, ident);
	  INIT_TYPE_OBJC_INFO (record);
	  TYPE_OBJC_INTERFACE (record) = ident;
	  class_chain = tree_cons (NULL_TREE, ident, class_chain);
	}
    }
}

tree
objc_is_class_name (tree ident)
{
  tree chain;

  if (ident && TREE_CODE (ident) == IDENTIFIER_NODE
      && identifier_global_value (ident))
    ident = identifier_global_value (ident);
  while (ident && TREE_CODE (ident) == TYPE_DECL && DECL_ORIGINAL_TYPE (ident))
    ident = OBJC_TYPE_NAME (DECL_ORIGINAL_TYPE (ident));

  if (ident && TREE_CODE (ident) == RECORD_TYPE)
    ident = OBJC_TYPE_NAME (ident);
#ifdef OBJCPLUS
  if (ident && TREE_CODE (ident) == TYPE_DECL)
    ident = DECL_NAME (ident);
#endif
  if (!ident || TREE_CODE (ident) != IDENTIFIER_NODE)
    return NULL_TREE;

  if (lookup_interface (ident))
    return ident;

  for (chain = class_chain; chain; chain = TREE_CHAIN (chain))
    {
      if (ident == TREE_VALUE (chain))
	return ident;
    }

  for (chain = alias_chain; chain; chain = TREE_CHAIN (chain))
    {
      if (ident == TREE_VALUE (chain))
	return TREE_PURPOSE (chain);
    }

  return 0;
}

/* Check whether TYPE is either 'id' or 'Class'.  */

tree
objc_is_id (tree type)
{
  if (type && TREE_CODE (type) == IDENTIFIER_NODE
      && identifier_global_value (type))
    type = identifier_global_value (type);

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

static tree
lookup_interface (tree ident)
{
  tree chain;

#ifdef OBJCPLUS
  if (ident && TREE_CODE (ident) == TYPE_DECL)
    ident = DECL_NAME (ident);
#endif
  for (chain = interface_chain; chain; chain = TREE_CHAIN (chain))
    {
      if (ident == CLASS_NAME (chain))
      return chain;
    }
  return NULL_TREE;
}

/* Implement @defs (<classname>) within struct bodies.  */

tree
objc_get_class_ivars (tree class_name)
{
  tree interface = lookup_interface (class_name);

  if (interface)
    return get_class_ivars (interface);

  error ("cannot find interface declaration for %qs",
	 IDENTIFIER_POINTER (class_name));

  return error_mark_node;
}

/* Used by: build_private_template, continue_class,
   and for @defs constructs.  */

static tree
get_class_ivars (tree interface)
{
  tree ivar_chain = copy_list (CLASS_RAW_IVARS (interface));

  /* Both CLASS_RAW_IVARS and CLASS_IVARS contain a list of ivars declared
     by the current class (i.e., they do not include super-class ivars).
     However, the CLASS_IVARS list will be side-effected by a call to
     finish_struct(), which will fill in field offsets.  */
  if (!CLASS_IVARS (interface))
    CLASS_IVARS (interface) = ivar_chain;

  while (CLASS_SUPER_NAME (interface))
    {
      /* Prepend super-class ivars.  */
      interface = lookup_interface (CLASS_SUPER_NAME (interface));
      ivar_chain = chainon (copy_list (CLASS_RAW_IVARS (interface)),
			    ivar_chain);
    }

  return ivar_chain;
}

static tree
objc_create_temporary_var (tree type)
{
  tree decl;
 
  decl = build_decl (VAR_DECL, NULL_TREE, type);
  TREE_USED (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  DECL_IGNORED_P (decl) = 1;
  DECL_CONTEXT (decl) = current_function_decl;

  return decl;
}

/* Exception handling constructs.  We begin by having the parser do most
   of the work and passing us blocks.  What we do next depends on whether
   we're doing "native" exception handling or legacy Darwin setjmp exceptions.
   We abstract all of this in a handful of appropriately named routines.  */

/* Stack of open try blocks.  */

struct objc_try_context
{
  struct objc_try_context *outer;

  /* Statements (or statement lists) as processed by the parser.  */
  tree try_body;
  tree finally_body;

  /* Some file position locations.  */
  location_t try_locus;
  location_t end_try_locus;
  location_t end_catch_locus;
  location_t finally_locus;
  location_t end_finally_locus;

  /* A STATEMENT_LIST of CATCH_EXPRs, appropriate for sticking into op1
     of a TRY_CATCH_EXPR.  Even when doing Darwin setjmp.  */
  tree catch_list;

  /* The CATCH_EXPR of an open @catch clause.  */
  tree current_catch;

  /* The VAR_DECL holding the Darwin equivalent of EXC_PTR_EXPR.  */
  tree caught_decl;
  tree stack_decl;
  tree rethrow_decl;
};

static struct objc_try_context *cur_try_context;

/* This hook, called via lang_eh_runtime_type, generates a runtime object
   that represents TYPE.  For Objective-C, this is just the class name.  */
/* ??? Isn't there a class object or some such?  Is it easy to get?  */

#ifndef OBJCPLUS
static tree
objc_eh_runtime_type (tree type)
{
  return add_objc_string (OBJC_TYPE_NAME (TREE_TYPE (type)), class_names);
}
#endif

/* Initialize exception handling.  */

static void
objc_init_exceptions (void)
{
  static bool done = false;
  if (done)
    return;
  done = true;

  if (flag_objc_sjlj_exceptions)
    {
      /* On Darwin, ObjC exceptions require a sufficiently recent
	 version of the runtime, so the user must ask for them explicitly.  */
      if (!flag_objc_exceptions)
	warning ("use %<-fobjc-exceptions%> to enable Objective-C "
		 "exception syntax");
    }
#ifndef OBJCPLUS
  else
    {
      c_eh_initialized_p = true;
      eh_personality_libfunc
	= init_one_libfunc (USING_SJLJ_EXCEPTIONS
			    ? "__gnu_objc_personality_sj0"
			    : "__gnu_objc_personality_v0");
      using_eh_for_cleanups ();
      lang_eh_runtime_type = objc_eh_runtime_type;
    }
#endif
}

/* Build an EXC_PTR_EXPR, or the moral equivalent.  In the case of Darwin,
   we'll arrange for it to be initialized (and associated with a binding)
   later.  */

static tree
objc_build_exc_ptr (void)
{
  if (flag_objc_sjlj_exceptions)
    {
      tree var = cur_try_context->caught_decl;
      if (!var)
	{
	  var = objc_create_temporary_var (objc_object_type);
	  cur_try_context->caught_decl = var;
	}
      return var;
    }
  else
    return build (EXC_PTR_EXPR, objc_object_type);
}

/* Build "objc_exception_try_exit(&_stack)".  */

static tree
next_sjlj_build_try_exit (void)
{
  tree t;
  t = build_fold_addr_expr (cur_try_context->stack_decl);
  t = tree_cons (NULL, t, NULL);
  t = build_function_call (objc_exception_try_exit_decl, t);
  return t;
}

/* Build
	objc_exception_try_enter (&_stack);
	if (_setjmp(&_stack.buf))
	  ;
	else
	  ;
   Return the COND_EXPR.  Note that the THEN and ELSE fields are left
   empty, ready for the caller to fill them in.  */

static tree
next_sjlj_build_enter_and_setjmp (void)
{
  tree t, enter, sj, cond;

  t = build_fold_addr_expr (cur_try_context->stack_decl);
  t = tree_cons (NULL, t, NULL);
  enter = build_function_call (objc_exception_try_enter_decl, t);

  t = build_component_ref (cur_try_context->stack_decl,
			   get_identifier ("buf"));
  t = build_fold_addr_expr (t);
  t = convert (ptr_type_node, t);
  t = tree_cons (NULL, t, NULL);
  sj = build_function_call (objc_setjmp_decl, t);

  cond = build (COMPOUND_EXPR, TREE_TYPE (sj), enter, sj);
  cond = lang_hooks.truthvalue_conversion (cond);

  return build (COND_EXPR, void_type_node, cond, NULL, NULL);
}

/* Build
	DECL = objc_exception_extract(&_stack);
*/
   
static tree
next_sjlj_build_exc_extract (tree decl)
{
  tree t;

  t = build_fold_addr_expr (cur_try_context->stack_decl);
  t = tree_cons (NULL, t, NULL);
  t = build_function_call (objc_exception_extract_decl, t);
  t = convert (TREE_TYPE (decl), t);
  t = build (MODIFY_EXPR, void_type_node, decl, t);

  return t;
}

/* Build
	if (objc_exception_match(obj_get_class(TYPE), _caught)
	  BODY
	else if (...)
	  ...
	else
	  {
	    _rethrow = _caught;
	    objc_exception_try_exit(&_stack);
	  }
   from the sequence of CATCH_EXPRs in the current try context.  */

static tree
next_sjlj_build_catch_list (void)
{
  tree_stmt_iterator i = tsi_start (cur_try_context->catch_list);
  tree catch_seq, t;
  tree *last = &catch_seq;
  bool saw_id = false;

  for (; !tsi_end_p (i); tsi_next (&i))
    {
      tree stmt = tsi_stmt (i);
      tree type = CATCH_TYPES (stmt);
      tree body = CATCH_BODY (stmt);

      if (type == NULL)
	{
	  *last = body;
	  saw_id = true;
	  break;
	}
      else
	{
	  tree args, cond;

	  if (type == error_mark_node)
	    cond = error_mark_node;
	  else
	    {
	      args = tree_cons (NULL, cur_try_context->caught_decl, NULL);
	      t = objc_get_class_reference (OBJC_TYPE_NAME (TREE_TYPE (type)));
	      args = tree_cons (NULL, t, args);
	      t = build_function_call (objc_exception_match_decl, args);
	      cond = lang_hooks.truthvalue_conversion (t);
	    }
	  t = build (COND_EXPR, void_type_node, cond, body, NULL);
	  SET_EXPR_LOCUS (t, EXPR_LOCUS (stmt));

	  *last = t;
	  last = &COND_EXPR_ELSE (t);
	}
    }

  if (!saw_id)
    {
      t = build (MODIFY_EXPR, void_type_node, cur_try_context->rethrow_decl,
		 cur_try_context->caught_decl);
      SET_EXPR_LOCATION (t, cur_try_context->end_catch_locus);
      append_to_statement_list (t, last);

      t = next_sjlj_build_try_exit ();
      SET_EXPR_LOCATION (t, cur_try_context->end_catch_locus);
      append_to_statement_list (t, last);
    }

  return catch_seq;
}

/* Build a complete @try-@catch-@finally block for legacy Darwin setjmp
   exception handling.  We aim to build:

	{
	  struct _objc_exception_data _stack;
	  id volatile _rethrow = 0;
	  try
	    {
	      objc_exception_try_enter (&_stack);
	      if (_setjmp(&_stack.buf))
	        {
		  id _caught = objc_exception_extract(&_stack);
		  objc_exception_try_enter (&_stack);
		  if (_setjmp(&_stack.buf))
		    _rethrow = objc_exception_extract(&_stack);
		  else
		    CATCH-LIST
	        }
	      else
		TRY-BLOCK
	    }
	  finally
	    {
	      if (!_rethrow)
		objc_exception_try_exit(&_stack);
	      FINALLY-BLOCK
	      if (_rethrow)
		objc_exception_throw(_rethrow);
	    }
	}

   If CATCH-LIST is empty, we can omit all of the block containing
   "_caught" except for the setting of _rethrow.  Note the use of
   a real TRY_FINALLY_EXPR here, which is not involved in EH per-se,
   but handles goto and other exits from the block.  */

static tree
next_sjlj_build_try_catch_finally (void)
{
  tree rethrow_decl, stack_decl, t;
  tree catch_seq, try_fin, bind;

  /* Create the declarations involved.  */
  t = xref_tag (RECORD_TYPE, get_identifier (UTAG_EXCDATA));
  stack_decl = objc_create_temporary_var (t);
  cur_try_context->stack_decl = stack_decl;

  rethrow_decl = objc_create_temporary_var (objc_object_type);
  cur_try_context->rethrow_decl = rethrow_decl;
  TREE_THIS_VOLATILE (rethrow_decl) = 1;
  TREE_CHAIN (rethrow_decl) = stack_decl;

  /* Build the outermost variable binding level.  */
  bind = build (BIND_EXPR, void_type_node, rethrow_decl, NULL, NULL);
  SET_EXPR_LOCATION (bind, cur_try_context->try_locus);
  TREE_SIDE_EFFECTS (bind) = 1;

  /* Initialize rethrow_decl.  */
  t = build (MODIFY_EXPR, void_type_node, rethrow_decl,
	     convert (objc_object_type, null_pointer_node));
  SET_EXPR_LOCATION (t, cur_try_context->try_locus);
  append_to_statement_list (t, &BIND_EXPR_BODY (bind));

  /* Build the outermost TRY_FINALLY_EXPR.  */
  try_fin = build (TRY_FINALLY_EXPR, void_type_node, NULL, NULL);
  SET_EXPR_LOCATION (try_fin, cur_try_context->try_locus);
  TREE_SIDE_EFFECTS (try_fin) = 1;
  append_to_statement_list (try_fin, &BIND_EXPR_BODY (bind));

  /* Create the complete catch sequence.  */
  if (cur_try_context->catch_list)
    {
      tree caught_decl = objc_build_exc_ptr ();
      catch_seq = build_stmt (BIND_EXPR, caught_decl, NULL, NULL);

      t = next_sjlj_build_exc_extract (caught_decl);
      append_to_statement_list (t, &BIND_EXPR_BODY (catch_seq));

      t = next_sjlj_build_enter_and_setjmp ();
      COND_EXPR_THEN (t) = next_sjlj_build_exc_extract (rethrow_decl);
      COND_EXPR_ELSE (t) = next_sjlj_build_catch_list ();
      append_to_statement_list (t, &BIND_EXPR_BODY (catch_seq));
    }
  else
    catch_seq = next_sjlj_build_exc_extract (rethrow_decl);
  SET_EXPR_LOCATION (catch_seq, cur_try_context->end_try_locus);

  /* Build the main register-and-try if statement.  */
  t = next_sjlj_build_enter_and_setjmp ();
  SET_EXPR_LOCATION (t, cur_try_context->try_locus);
  COND_EXPR_THEN (t) = catch_seq;
  COND_EXPR_ELSE (t) = cur_try_context->try_body;
  TREE_OPERAND (try_fin, 0) = t;

  /* Build the complete FINALLY statement list.  */
  t = next_sjlj_build_try_exit ();
  t = build_stmt (COND_EXPR,
		  lang_hooks.truthvalue_conversion (rethrow_decl),
		  NULL, t);
  SET_EXPR_LOCATION (t, cur_try_context->finally_locus);
  append_to_statement_list (t, &TREE_OPERAND (try_fin, 1));

  append_to_statement_list (cur_try_context->finally_body,
			    &TREE_OPERAND (try_fin, 1));

  t = tree_cons (NULL, rethrow_decl, NULL);
  t = build_function_call (objc_exception_throw_decl, t);
  t = build_stmt (COND_EXPR,
		  lang_hooks.truthvalue_conversion (rethrow_decl),
		  t, NULL);
  SET_EXPR_LOCATION (t, cur_try_context->end_finally_locus);
  append_to_statement_list (t, &TREE_OPERAND (try_fin, 1));

  return bind;
}

/* Called just after parsing the @try and its associated BODY.  We now
   must prepare for the tricky bits -- handling the catches and finally.  */

void
objc_begin_try_stmt (location_t try_locus, tree body)
{
  struct objc_try_context *c = xcalloc (1, sizeof (*c));
  c->outer = cur_try_context;
  c->try_body = body;
  c->try_locus = try_locus;
  c->end_try_locus = input_location;
  cur_try_context = c;

  objc_init_exceptions ();
}

/* Called just after parsing "@catch (parm)".  Open a binding level, 
   enter DECL into the binding level, and initialize it.  Leave the
   binding level open while the body of the compound statement is parsed.  */
   
void
objc_begin_catch_clause (tree decl)
{
  tree compound, type, t;

  /* Begin a new scope that the entire catch clause will live in.  */
  compound = c_begin_compound_stmt (true);

  /* The parser passed in a PARM_DECL, but what we really want is a VAR_DECL.  */
  decl = build_decl (VAR_DECL, DECL_NAME (decl), TREE_TYPE (decl));
  lang_hooks.decls.pushdecl (decl);

  /* Since a decl is required here by syntax, don't warn if its unused.  */
  /* ??? As opposed to __attribute__((unused))?  Anyway, this appears to
     be what the previous objc implementation did.  */
  TREE_USED (decl) = 1;

  /* Verify that the type of the catch is valid.  It must be a pointer
     to an Objective-C class, or "id" (which is catch-all).  */
  type = TREE_TYPE (decl);

  if (POINTER_TYPE_P (type) && objc_is_object_id (TREE_TYPE (type)))
    type = NULL;
  else if (!POINTER_TYPE_P (type) || !TYPED_OBJECT (TREE_TYPE (type)))
    {
      error ("@catch parameter is not a known Objective-C class type");
      type = error_mark_node;
    }
  else if (cur_try_context->catch_list)
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
	  if (!t || objc_comptypes (TREE_TYPE (t), TREE_TYPE (type), 0) == 1)
	    {
	      warning ("exception of type %<%T%> will be caught",
		       TREE_TYPE (type));
	      warning ("%H   by earlier handler for %<%T%>",
		       EXPR_LOCUS (stmt), TREE_TYPE (t ? t : objc_object_type));
	      break;
	    }
	}
    }

  /* Record the data for the catch in the try context so that we can
     finalize it later.  */
  t = build_stmt (CATCH_EXPR, type, compound);
  cur_try_context->current_catch = t;

  /* Initialize the decl from the EXC_PTR_EXPR we get from the runtime.  */
  t = objc_build_exc_ptr ();
  t = convert (TREE_TYPE (decl), t);
  t = build (MODIFY_EXPR, void_type_node, decl, t);
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

  CATCH_BODY (c) = c_end_compound_stmt (CATCH_BODY (c), 1);
  append_to_statement_list (c, &cur_try_context->catch_list);
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

void
objc_finish_try_stmt (void)
{
  struct objc_try_context *c = cur_try_context;
  tree stmt;

  if (c->catch_list == NULL && c->finally_body == NULL)
    error ("%<@try%> without %<@catch%> or %<@finally%>");

  /* If we're doing Darwin setjmp exceptions, build the big nasty.  */
  if (flag_objc_sjlj_exceptions)
    {
      if (!cur_try_context->finally_body)
	{
	  cur_try_context->finally_locus = input_location;
	  cur_try_context->end_finally_locus = input_location;
	}
      stmt = next_sjlj_build_try_catch_finally ();
    }
  else
    {
      /* Otherwise, nest the CATCH inside a FINALLY.  */
      stmt = c->try_body;
      if (c->catch_list)
	{
          stmt = build_stmt (TRY_CATCH_EXPR, stmt, c->catch_list);
	  SET_EXPR_LOCATION (stmt, cur_try_context->try_locus);
	}
      if (c->finally_body)
	{
	  stmt = build_stmt (TRY_FINALLY_EXPR, stmt, c->finally_body);
	  SET_EXPR_LOCATION (stmt, cur_try_context->try_locus);
	}
    }
  add_stmt (stmt);

  cur_try_context = c->outer;
  free (c);
}

tree
objc_build_throw_stmt (tree throw_expr)
{
  tree args;

  objc_init_exceptions ();

  if (throw_expr == NULL)
    {
      /* If we're not inside a @catch block, there is no "current
	 exception" to be rethrown.  */
      if (cur_try_context == NULL
          || cur_try_context->current_catch == NULL)
	{
	  error ("%<@throw%> (rethrow) used outside of a @catch block");
	  return NULL_TREE;
	}

      /* Otherwise the object is still sitting in the EXC_PTR_EXPR
	 value that we get from the runtime.  */
      throw_expr = objc_build_exc_ptr ();
    }

  /* A throw is just a call to the runtime throw function with the
     object as a parameter.  */
  args = tree_cons (NULL, throw_expr, NULL);
  return add_stmt (build_function_call (objc_exception_throw_decl, args));
}

void
objc_build_synchronized (location_t start_locus, tree mutex, tree body)
{
  tree args, call;

  /* First lock the mutex.  */
  mutex = save_expr (mutex);
  args = tree_cons (NULL, mutex, NULL);
  call = build_function_call (objc_sync_enter_decl, args);
  SET_EXPR_LOCATION (call, start_locus);
  add_stmt (call);

  /* Build the mutex unlock.  */
  args = tree_cons (NULL, mutex, NULL);
  call = build_function_call (objc_sync_exit_decl, args);
  SET_EXPR_LOCATION (call, input_location);

  /* Put the that and the body in a TRY_FINALLY.  */
  objc_begin_try_stmt (start_locus, body);
  objc_build_finally_clause (input_location, call);
  objc_finish_try_stmt ();
}


/* Predefine the following data type:

   struct _objc_exception_data
   {
     int buf[_JBLEN];
     void *pointers[4];
   }; */

/* The following yuckiness should prevent users from having to #include
   <setjmp.h> in their code... */

#ifdef TARGET_POWERPC
/* snarfed from /usr/include/ppc/setjmp.h */
#define _JBLEN (26 + 36 + 129 + 1)
#else
/* snarfed from /usr/include/i386/{setjmp,signal}.h */
#define _JBLEN 18
#endif

static void
build_next_objc_exception_stuff (void)
{
  tree field_decl, field_decl_chain, index, temp_type;

  objc_exception_data_template
    = start_struct (RECORD_TYPE, get_identifier (UTAG_EXCDATA));

  /* int buf[_JBLEN]; */

  index = build_index_type (build_int_cst (NULL_TREE, _JBLEN - 1));
  field_decl = create_field_decl (build_array_type (integer_type_node, index),
				  "buf");
  field_decl_chain = field_decl;

  /* void *pointers[4]; */

  index = build_index_type (build_int_cst (NULL_TREE, 4 - 1));
  field_decl = create_field_decl (build_array_type (ptr_type_node, index),
				  "pointers");
  chainon (field_decl_chain, field_decl);

  finish_struct (objc_exception_data_template, field_decl_chain, NULL_TREE);

  /* int _setjmp(...); */
  /* If the user includes <setjmp.h>, this shall be superseded by
     'int _setjmp(jmp_buf);' */
  temp_type = build_function_type (integer_type_node, NULL_TREE);
  objc_setjmp_decl
    = builtin_function (TAG_SETJMP, temp_type, 0, NOT_BUILT_IN, NULL, NULL_TREE);

  /* id objc_exception_extract(struct _objc_exception_data *); */
  temp_type
    = build_function_type (objc_object_type,
			   tree_cons (NULL_TREE,
				      build_pointer_type (objc_exception_data_template),
				      OBJC_VOID_AT_END));
  objc_exception_extract_decl
    = builtin_function (TAG_EXCEPTIONEXTRACT, temp_type, 0, NOT_BUILT_IN, NULL, NULL_TREE);
  /* void objc_exception_try_enter(struct _objc_exception_data *); */
  /* void objc_exception_try_exit(struct _objc_exception_data *); */
  temp_type
    = build_function_type (void_type_node,
			   tree_cons (NULL_TREE,
				      build_pointer_type (objc_exception_data_template),
				      OBJC_VOID_AT_END));
  objc_exception_try_enter_decl
    = builtin_function (TAG_EXCEPTIONTRYENTER, temp_type, 0, NOT_BUILT_IN, NULL, NULL_TREE);
  objc_exception_try_exit_decl
    = builtin_function (TAG_EXCEPTIONTRYEXIT, temp_type, 0, NOT_BUILT_IN, NULL, NULL_TREE);

  /* int objc_exception_match(id, id); */
  temp_type 
    = build_function_type (integer_type_node,
			   tree_cons (NULL_TREE, objc_object_type,
				      tree_cons (NULL_TREE, objc_object_type,
						 OBJC_VOID_AT_END)));
  objc_exception_match_decl
    = builtin_function (TAG_EXCEPTIONMATCH, temp_type, 0, NOT_BUILT_IN, NULL, NULL_TREE);
}

static void
build_objc_exception_stuff (void)
{
  tree noreturn_list, nothrow_list, temp_type;

  noreturn_list = tree_cons (get_identifier ("noreturn"), NULL, NULL);
  nothrow_list = tree_cons (get_identifier ("nothrow"), NULL, NULL);

  /* void objc_exception_throw(id) __attribute__((noreturn)); */
  /* void objc_sync_enter(id); */
  /* void objc_sync_exit(id); */
  temp_type = build_function_type (void_type_node,
				   tree_cons (NULL_TREE, objc_object_type,
					      OBJC_VOID_AT_END));
  objc_exception_throw_decl
    = builtin_function (TAG_EXCEPTIONTHROW, temp_type, 0, NOT_BUILT_IN, NULL,
			noreturn_list);
  objc_sync_enter_decl
    = builtin_function (TAG_SYNCENTER, temp_type, 0, NOT_BUILT_IN,
			NULL, nothrow_list);
  objc_sync_exit_decl
    = builtin_function (TAG_SYNCEXIT, temp_type, 0, NOT_BUILT_IN,
			NULL, nothrow_list);
}

/* Construct a C struct corresponding to ObjC class CLASS, with the same
   name as the class:

   struct <classname> {
     struct _objc_class *isa;
     ...
   };  */

static void
build_private_template (tree class)
{
  if (!CLASS_STATIC_TEMPLATE (class))
    {
      tree record = start_struct (RECORD_TYPE, CLASS_NAME (class));

      finish_struct (record, get_class_ivars (class), NULL_TREE);
      /* mark this record as class template - for class type checking */
      INIT_TYPE_OBJC_INFO (record);
      TYPE_OBJC_INTERFACE (record) = class;
      CLASS_STATIC_TEMPLATE (class) = record;
    }
}

/* Begin code generation for protocols...  */

/* struct _objc_protocol {
     struct _objc_class *isa;
     char *protocol_name;
     struct _objc_protocol **protocol_list;
     struct _objc__method_prototype_list *instance_methods;
     struct _objc__method_prototype_list *class_methods;
   };  */

static void
build_protocol_template (void)
{
  tree field_decl, field_decl_chain;

  objc_protocol_template = start_struct (RECORD_TYPE,
					 get_identifier (UTAG_PROTOCOL));

  /* struct _objc_class *isa; */
  field_decl = create_field_decl (build_pointer_type
				  (xref_tag (RECORD_TYPE,
					     get_identifier (UTAG_CLASS))),
				  "isa");
  field_decl_chain = field_decl;

  /* char *protocol_name; */
  field_decl = create_field_decl (string_type_node, "protocol_name");
  chainon (field_decl_chain, field_decl);

  /* struct _objc_protocol **protocol_list; */
  field_decl = create_field_decl (build_pointer_type
				  (build_pointer_type
				   (objc_protocol_template)),
				  "protocol_list");
  chainon (field_decl_chain, field_decl);

  /* struct objc_method_list *instance_methods; */
  field_decl = create_field_decl (build_pointer_type
				  (xref_tag (RECORD_TYPE,
					     get_identifier
					     (UTAG_METHOD_PROTOTYPE_LIST))),
				  "instance_methods");
  chainon (field_decl_chain, field_decl);

  /* struct objc_method_list *class_methods; */
  field_decl = create_field_decl (build_pointer_type
				  (xref_tag (RECORD_TYPE,
					     get_identifier
					     (UTAG_METHOD_PROTOTYPE_LIST))),
				  "class_methods");
  chainon (field_decl_chain, field_decl);

  finish_struct (objc_protocol_template, field_decl_chain, NULL_TREE);
}

static tree
build_descriptor_table_initializer (tree type, tree entries)
{
  tree initlist = NULL_TREE;

  do
    {
      tree eltlist = NULL_TREE;

      eltlist
	= tree_cons (NULL_TREE,
		     build_selector (METHOD_SEL_NAME (entries)), NULL_TREE);
      eltlist
	= tree_cons (NULL_TREE,
		     add_objc_string (METHOD_ENCODING (entries),
				      meth_var_types),
		     eltlist);

      initlist
	= tree_cons (NULL_TREE,
		     objc_build_constructor (type, nreverse (eltlist)),
		     initlist);

      entries = TREE_CHAIN (entries);
    }
  while (entries);

  return objc_build_constructor (build_array_type (type, 0),
				 nreverse (initlist));
}

/* struct objc_method_prototype_list {
     int count;
     struct objc_method_prototype {
	SEL name;
	char *types;
     } list[1];
   };  */

static tree
build_method_prototype_list_template (tree list_type, int size)
{
  tree objc_ivar_list_record;
  tree field_decl, field_decl_chain;

  /* Generate an unnamed struct definition.  */

  objc_ivar_list_record = start_struct (RECORD_TYPE, NULL_TREE);

  /* int method_count; */
  field_decl = create_field_decl (integer_type_node, "method_count");
  field_decl_chain = field_decl;

  /* struct objc_method method_list[]; */
  field_decl = create_field_decl (build_array_type
				  (list_type,
				   build_index_type
				   (build_int_cst (NULL_TREE, size - 1))),
				  "method_list");
  chainon (field_decl_chain, field_decl);

  finish_struct (objc_ivar_list_record, field_decl_chain, NULL_TREE);

  return objc_ivar_list_record;
}

static tree
build_method_prototype_template (void)
{
  tree proto_record;
  tree field_decl, field_decl_chain;

  proto_record
    = start_struct (RECORD_TYPE, get_identifier (UTAG_METHOD_PROTOTYPE));

  /* SEL _cmd; */
  field_decl = create_field_decl (objc_selector_type, "_cmd");
  field_decl_chain = field_decl;

  /* char *method_types; */
  field_decl = create_field_decl (string_type_node, "method_types");
  chainon (field_decl_chain, field_decl);

  finish_struct (proto_record, field_decl_chain, NULL_TREE);

  return proto_record;
}

static tree
objc_method_parm_type (tree type)
{
  type = TREE_VALUE (TREE_TYPE (type));
  if (TREE_CODE (type) == TYPE_DECL)
    type = TREE_TYPE (type);
  return TYPE_MAIN_VARIANT (type);
}

static int
objc_encoded_type_size (tree type)
{
  int sz = int_size_in_bytes (type);

  /* Make all integer and enum types at least as large
     as an int.  */
  if (sz > 0 && INTEGRAL_TYPE_P (type))
    sz = MAX (sz, int_size_in_bytes (integer_type_node));
  /* Treat arrays as pointers, since that's how they're
     passed in.  */
  else if (TREE_CODE (type) == ARRAY_TYPE)
    sz = int_size_in_bytes (ptr_type_node);
  return sz;
}

static tree
encode_method_prototype (tree method_decl)
{
  tree parms;
  int parm_offset, i;
  char buf[40];
  tree result;

  /* ONEWAY and BYCOPY, for remote object are the only method qualifiers.  */
  encode_type_qualifiers (TREE_PURPOSE (TREE_TYPE (method_decl)));

  /* Encode return type.  */
  encode_type (objc_method_parm_type (method_decl),
	       obstack_object_size (&util_obstack),
	       OBJC_ENCODE_INLINE_DEFS);

  /* Stack size.  */
  /* The first two arguments (self and _cmd) are pointers; account for
     their size.  */
  i = int_size_in_bytes (ptr_type_node);
  parm_offset = 2 * i;
  for (parms = METHOD_SEL_ARGS (method_decl); parms;
       parms = TREE_CHAIN (parms))
    {
      tree type = objc_method_parm_type (parms);
      int sz = objc_encoded_type_size (type);

      /* If a type size is not known, bail out.  */
      if (sz < 0)
	{
	  error ("%Jtype '%D' does not have a known size",
		 type, type);
	  /* Pretend that the encoding succeeded; the compilation will
	     fail nevertheless.  */
	  goto finish_encoding;
	}
      parm_offset += sz;
    }

  sprintf (buf, "%d@0:%d", parm_offset, i);
  obstack_grow (&util_obstack, buf, strlen (buf));

  /* Argument types.  */
  parm_offset = 2 * i;
  for (parms = METHOD_SEL_ARGS (method_decl); parms;
       parms = TREE_CHAIN (parms))
    {
      tree type = objc_method_parm_type (parms);

      /* Process argument qualifiers for user supplied arguments.  */
      encode_type_qualifiers (TREE_PURPOSE (TREE_TYPE (parms)));

      /* Type.  */
      encode_type (type, obstack_object_size (&util_obstack),
		   OBJC_ENCODE_INLINE_DEFS);

      /* Compute offset.  */
      sprintf (buf, "%d", parm_offset);
      parm_offset += objc_encoded_type_size (type);

      obstack_grow (&util_obstack, buf, strlen (buf));
    }

  finish_encoding:
  obstack_1grow (&util_obstack, '\0');
  result = get_identifier (obstack_finish (&util_obstack));
  obstack_free (&util_obstack, util_firstobj);
  return result;
}

static tree
generate_descriptor_table (tree type, const char *name, int size, tree list,
			   tree proto)
{
  tree decl, initlist;

  decl = start_var_decl (type, synth_id_with_class_suffix (name, proto));

  initlist = build_tree_list (NULL_TREE, build_int_cst (NULL_TREE, size));
  initlist = tree_cons (NULL_TREE, list, initlist);

  finish_var_decl (decl, objc_build_constructor (type, nreverse (initlist)));

  return decl;
}

static void
generate_method_descriptors (tree protocol)
{
  tree initlist, chain, method_list_template;
  int size;

  if (!objc_method_prototype_template)
    objc_method_prototype_template = build_method_prototype_template ();

  chain = PROTOCOL_CLS_METHODS (protocol);
  if (chain)
    {
      size = list_length (chain);

      method_list_template
	= build_method_prototype_list_template (objc_method_prototype_template,
						size);

      initlist
	= build_descriptor_table_initializer (objc_method_prototype_template,
					      chain);

      UOBJC_CLASS_METHODS_decl
	= generate_descriptor_table (method_list_template,
				     "_OBJC_PROTOCOL_CLASS_METHODS",
				     size, initlist, protocol);
    }
  else
    UOBJC_CLASS_METHODS_decl = 0;

  chain = PROTOCOL_NST_METHODS (protocol);
  if (chain)
    {
      size = list_length (chain);

      method_list_template
	= build_method_prototype_list_template (objc_method_prototype_template,
						size);
      initlist
	= build_descriptor_table_initializer (objc_method_prototype_template,
					      chain);

      UOBJC_INSTANCE_METHODS_decl
	= generate_descriptor_table (method_list_template,
				     "_OBJC_PROTOCOL_INSTANCE_METHODS",
				     size, initlist, protocol);
    }
  else
    UOBJC_INSTANCE_METHODS_decl = 0;
}

static void
generate_protocol_references (tree plist)
{
  tree lproto;

  /* Forward declare protocols referenced.  */
  for (lproto = plist; lproto; lproto = TREE_CHAIN (lproto))
    {
      tree proto = TREE_VALUE (lproto);

      if (TREE_CODE (proto) == PROTOCOL_INTERFACE_TYPE
	  && PROTOCOL_NAME (proto))
	{
          if (! PROTOCOL_FORWARD_DECL (proto))
            build_protocol_reference (proto);

          if (PROTOCOL_LIST (proto))
            generate_protocol_references (PROTOCOL_LIST (proto));
        }
    }
}

/* For each protocol which was referenced either from a @protocol()
   expression, or because a class/category implements it (then a
   pointer to the protocol is stored in the struct describing the
   class/category), we create a statically allocated instance of the
   Protocol class.  The code is written in such a way as to generate
   as few Protocol objects as possible; we generate a unique Protocol
   instance for each protocol, and we don't generate a Protocol
   instance if the protocol is never referenced (either from a
   @protocol() or from a class/category implementation).  These
   statically allocated objects can be referred to via the static
   (that is, private to this module) symbols _OBJC_PROTOCOL_n.

   The statically allocated Protocol objects that we generate here
   need to be fixed up at runtime in order to be used: the 'isa'
   pointer of the objects need to be set up to point to the 'Protocol'
   class, as known at runtime.

   The NeXT runtime fixes up all protocols at program startup time,
   before main() is entered.  It uses a low-level trick to look up all
   those symbols, then loops on them and fixes them up.

   The GNU runtime as well fixes up all protocols before user code
   from the module is executed; it requires pointers to those symbols
   to be put in the objc_symtab (which is then passed as argument to
   the function __objc_exec_class() which the compiler sets up to be
   executed automatically when the module is loaded); setup of those
   Protocol objects happen in two ways in the GNU runtime: all
   Protocol objects referred to by a class or category implementation
   are fixed up when the class/category is loaded; all Protocol
   objects referred to by a @protocol() expression are added by the
   compiler to the list of statically allocated instances to fixup
   (the same list holding the statically allocated constant string
   objects).  Because, as explained above, the compiler generates as
   few Protocol objects as possible, some Protocol object might end up
   being referenced multiple times when compiled with the GNU runtime,
   and end up being fixed up multiple times at runtime initialization.
   But that doesn't hurt, it's just a little inefficient.  */

static void
generate_protocols (void)
{
  tree p, encoding;
  tree decl;
  tree initlist, protocol_name_expr, refs_decl, refs_expr;

  /* If a protocol was directly referenced, pull in indirect references.  */
  for (p = protocol_chain; p; p = TREE_CHAIN (p))
    if (PROTOCOL_FORWARD_DECL (p) && PROTOCOL_LIST (p))
      generate_protocol_references (PROTOCOL_LIST (p));

  for (p = protocol_chain; p; p = TREE_CHAIN (p))
    {
      tree nst_methods = PROTOCOL_NST_METHODS (p);
      tree cls_methods = PROTOCOL_CLS_METHODS (p);

      /* If protocol wasn't referenced, don't generate any code.  */
      decl = PROTOCOL_FORWARD_DECL (p);

      if (!decl)
	continue;

      /* Make sure we link in the Protocol class.  */
      add_class_reference (get_identifier (PROTOCOL_OBJECT_CLASS_NAME));

      while (nst_methods)
	{
	  if (! METHOD_ENCODING (nst_methods))
	    {
	      encoding = encode_method_prototype (nst_methods);
	      METHOD_ENCODING (nst_methods) = encoding;
	    }
	  nst_methods = TREE_CHAIN (nst_methods);
	}

      while (cls_methods)
	{
	  if (! METHOD_ENCODING (cls_methods))
	    {
	      encoding = encode_method_prototype (cls_methods);
	      METHOD_ENCODING (cls_methods) = encoding;
	    }

	  cls_methods = TREE_CHAIN (cls_methods);
	}
      generate_method_descriptors (p);

      if (PROTOCOL_LIST (p))
	refs_decl = generate_protocol_list (p);
      else
	refs_decl = 0;

      /* static struct objc_protocol _OBJC_PROTOCOL_<mumble>; */
      protocol_name_expr = add_objc_string (PROTOCOL_NAME (p), class_names);

      if (refs_decl)
	refs_expr = convert (build_pointer_type (build_pointer_type
						 (objc_protocol_template)),
			     build_unary_op (ADDR_EXPR, refs_decl, 0));
      else
	refs_expr = build_int_cst (NULL_TREE, 0);

      /* UOBJC_INSTANCE_METHODS_decl/UOBJC_CLASS_METHODS_decl are set
	 by generate_method_descriptors, which is called above.  */
      initlist = build_protocol_initializer (TREE_TYPE (decl),
					     protocol_name_expr, refs_expr,
					     UOBJC_INSTANCE_METHODS_decl,
					     UOBJC_CLASS_METHODS_decl);
      finish_var_decl (decl, initlist);
    }
}

static tree
build_protocol_initializer (tree type, tree protocol_name,
			    tree protocol_list, tree instance_methods,
			    tree class_methods)
{
  tree initlist = NULL_TREE, expr;
  tree cast_type = build_pointer_type
		   (xref_tag (RECORD_TYPE,
			      get_identifier (UTAG_CLASS)));

  /* Filling the "isa" in with one allows the runtime system to
     detect that the version change...should remove before final release.  */

  expr = build_int_cst (cast_type, PROTOCOL_VERSION);
  initlist = tree_cons (NULL_TREE, expr, initlist);
  initlist = tree_cons (NULL_TREE, protocol_name, initlist);
  initlist = tree_cons (NULL_TREE, protocol_list, initlist);

  if (!instance_methods)
    initlist = tree_cons (NULL_TREE, build_int_cst (NULL_TREE, 0), initlist);
  else
    {
      expr = build_unary_op (ADDR_EXPR, instance_methods, 0);
      initlist = tree_cons (NULL_TREE, expr, initlist);
    }

  if (!class_methods)
    initlist = tree_cons (NULL_TREE, build_int_cst (NULL_TREE, 0), initlist);
  else
    {
      expr = build_unary_op (ADDR_EXPR, class_methods, 0);
      initlist = tree_cons (NULL_TREE, expr, initlist);
    }

  return objc_build_constructor (type, nreverse (initlist));
}

/* struct _objc_category {
     char *category_name;
     char *class_name;
     struct _objc_method_list *instance_methods;
     struct _objc_method_list *class_methods;
     struct _objc_protocol_list *protocols;
   };   */

static void
build_category_template (void)
{
  tree field_decl, field_decl_chain;

  objc_category_template = start_struct (RECORD_TYPE,
					 get_identifier (UTAG_CATEGORY));

  /* char *category_name; */
  field_decl = create_field_decl (string_type_node, "category_name");
  field_decl_chain = field_decl;

  /* char *class_name; */
  field_decl = create_field_decl (string_type_node, "class_name");
  chainon (field_decl_chain, field_decl);

  /* struct _objc_method_list *instance_methods; */
  field_decl = create_field_decl (build_pointer_type
				  (xref_tag (RECORD_TYPE,
					     get_identifier
					     (UTAG_METHOD_LIST))),
				  "instance_methods");
  chainon (field_decl_chain, field_decl);

  /* struct _objc_method_list *class_methods; */
  field_decl = create_field_decl (build_pointer_type
				  (xref_tag (RECORD_TYPE,
					     get_identifier
					     (UTAG_METHOD_LIST))),
				  "class_methods");
  chainon (field_decl_chain, field_decl);

  /* struct _objc_protocol **protocol_list; */
  field_decl = create_field_decl (build_pointer_type
				  (build_pointer_type
				   (objc_protocol_template)),
				  "protocol_list");
  chainon (field_decl_chain, field_decl);

  finish_struct (objc_category_template, field_decl_chain, NULL_TREE);
}

/* struct _objc_selector {
     SEL sel_id;
     char *sel_type;
   }; */

static void
build_selector_template (void)
{

  tree field_decl, field_decl_chain;

  objc_selector_template
    = start_struct (RECORD_TYPE, get_identifier (UTAG_SELECTOR));

  /* SEL sel_id; */
  field_decl = create_field_decl (objc_selector_type, "sel_id");
  field_decl_chain = field_decl;

  /* char *sel_type; */
  field_decl = create_field_decl (string_type_node, "sel_type");
  chainon (field_decl_chain, field_decl);

  finish_struct (objc_selector_template, field_decl_chain, NULL_TREE);
}

/* struct _objc_class {
     struct _objc_class *isa;
     struct _objc_class *super_class;
     char *name;
     long version;
     long info;
     long instance_size;
     struct _objc_ivar_list *ivars;
     struct _objc_method_list *methods;
     #ifdef __NEXT_RUNTIME__
       struct objc_cache *cache;
     #else
       struct sarray *dtable;
       struct _objc_class *subclass_list;
       struct _objc_class *sibling_class;
     #endif
     struct _objc_protocol_list *protocols;
     #ifdef __NEXT_RUNTIME__
       void *sel_id;
     #endif
     void *gc_object_type;
   };  */

/* NB: The 'sel_id' and 'gc_object_type' fields are not being used by
   the NeXT/Apple runtime; still, the compiler must generate them to
   maintain backward binary compatibility (and to allow for future
   expansion).  */

static void
build_class_template (void)
{
  tree field_decl, field_decl_chain;

  objc_class_template
    = start_struct (RECORD_TYPE, get_identifier (UTAG_CLASS));

  /* struct _objc_class *isa; */
  field_decl = create_field_decl (build_pointer_type (objc_class_template),
				  "isa");
  field_decl_chain = field_decl;

  /* struct _objc_class *super_class; */
  field_decl = create_field_decl (build_pointer_type (objc_class_template),
				  "super_class");
  chainon (field_decl_chain, field_decl);

  /* char *name; */
  field_decl = create_field_decl (string_type_node, "name");
  chainon (field_decl_chain, field_decl);

  /* long version; */
  field_decl = create_field_decl (long_integer_type_node, "version");
  chainon (field_decl_chain, field_decl);

  /* long info; */
  field_decl = create_field_decl (long_integer_type_node, "info");
  chainon (field_decl_chain, field_decl);

  /* long instance_size; */
  field_decl = create_field_decl (long_integer_type_node, "instance_size");
  chainon (field_decl_chain, field_decl);

  /* struct _objc_ivar_list *ivars; */
  field_decl = create_field_decl (build_pointer_type
				  (xref_tag (RECORD_TYPE,
					     get_identifier
					     (UTAG_IVAR_LIST))),
				  "ivars");
  chainon (field_decl_chain, field_decl);

  /* struct _objc_method_list *methods; */
  field_decl = create_field_decl (build_pointer_type
				  (xref_tag (RECORD_TYPE,
					     get_identifier
					     (UTAG_METHOD_LIST))),
				  "methods");
  chainon (field_decl_chain, field_decl);

  if (flag_next_runtime)
    {
      /* struct objc_cache *cache; */
      field_decl = create_field_decl (build_pointer_type
				      (xref_tag (RECORD_TYPE,
						 get_identifier
						 ("objc_cache"))),
				      "cache");
      chainon (field_decl_chain, field_decl);
    }
  else
    {
      /* struct sarray *dtable; */
      field_decl = create_field_decl (build_pointer_type
				      (xref_tag (RECORD_TYPE,
						 get_identifier
						 ("sarray"))),
				      "dtable");
      chainon (field_decl_chain, field_decl);

      /* struct objc_class *subclass_list; */
      field_decl = create_field_decl (build_pointer_type
				      (objc_class_template),
				      "subclass_list");
      chainon (field_decl_chain, field_decl);

      /* struct objc_class *sibling_class; */
      field_decl = create_field_decl (build_pointer_type
				      (objc_class_template),
				      "sibling_class");
      chainon (field_decl_chain, field_decl);
    }

  /* struct _objc_protocol **protocol_list; */
  field_decl = create_field_decl (build_pointer_type
				  (build_pointer_type
				   (xref_tag (RECORD_TYPE,
					     get_identifier
					     (UTAG_PROTOCOL)))),
				  "protocol_list");
  chainon (field_decl_chain, field_decl);

  if (flag_next_runtime)
    {
      /* void *sel_id; */
      field_decl = create_field_decl (build_pointer_type (void_type_node),
				      "sel_id");
      chainon (field_decl_chain, field_decl);
    }

  /* void *gc_object_type; */
  field_decl = create_field_decl (build_pointer_type (void_type_node),
				  "gc_object_type");
  chainon (field_decl_chain, field_decl);

  finish_struct (objc_class_template, field_decl_chain, NULL_TREE);
}

/* Generate appropriate forward declarations for an implementation.  */

static void
synth_forward_declarations (void)
{
  tree an_id;

  /* static struct objc_class _OBJC_CLASS_<my_name>; */
  UOBJC_CLASS_decl = build_metadata_decl ("_OBJC_CLASS",
					  objc_class_template);

  /* static struct objc_class _OBJC_METACLASS_<my_name>; */
  UOBJC_METACLASS_decl = build_metadata_decl ("_OBJC_METACLASS",
						  objc_class_template);

  /* Pre-build the following entities - for speed/convenience.  */

  an_id = get_identifier ("super_class");
  ucls_super_ref = build_component_ref (UOBJC_CLASS_decl, an_id);
  uucls_super_ref = build_component_ref (UOBJC_METACLASS_decl, an_id);
}

static void
error_with_ivar (const char *message, tree decl)
{
  error ("%J%s %qs", decl,
         message, gen_declaration (decl));

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

      intdecls = TREE_CHAIN (intdecls);
      impdecls = TREE_CHAIN (impdecls);
    }
}

/* Set 'objc_super_template' to the data type node for 'struct _objc_super'.
   This needs to be done just once per compilation.  */

/* struct _objc_super {
     struct _objc_object *self;
     struct _objc_class *super_class;
   };  */

static void
build_super_template (void)
{
  tree field_decl, field_decl_chain;

  objc_super_template = start_struct (RECORD_TYPE, get_identifier (UTAG_SUPER));

  /* struct _objc_object *self; */
  field_decl = create_field_decl (objc_object_type, "self");
  field_decl_chain = field_decl;

  /* struct _objc_class *super_class; */
  field_decl = create_field_decl (build_pointer_type (objc_class_template),
				  "super_class");
  chainon (field_decl_chain, field_decl);

  finish_struct (objc_super_template, field_decl_chain, NULL_TREE);
}

/* struct _objc_ivar {
     char *ivar_name;
     char *ivar_type;
     int ivar_offset;
   };  */

static tree
build_ivar_template (void)
{
  tree objc_ivar_id, objc_ivar_record;
  tree field_decl, field_decl_chain;

  objc_ivar_id = get_identifier (UTAG_IVAR);
  objc_ivar_record = start_struct (RECORD_TYPE, objc_ivar_id);

  /* char *ivar_name; */
  field_decl = create_field_decl (string_type_node, "ivar_name");
  field_decl_chain = field_decl;

  /* char *ivar_type; */
  field_decl = create_field_decl (string_type_node, "ivar_type");
  chainon (field_decl_chain, field_decl);

  /* int ivar_offset; */
  field_decl = create_field_decl (integer_type_node, "ivar_offset");
  chainon (field_decl_chain, field_decl);

  finish_struct (objc_ivar_record, field_decl_chain, NULL_TREE);

  return objc_ivar_record;
}

/* struct {
     int ivar_count;
     struct objc_ivar ivar_list[ivar_count];
   };  */

static tree
build_ivar_list_template (tree list_type, int size)
{
  tree objc_ivar_list_record;
  tree field_decl, field_decl_chain;

  objc_ivar_list_record = start_struct (RECORD_TYPE, NULL_TREE);

  /* int ivar_count; */
  field_decl = create_field_decl (integer_type_node, "ivar_count");
  field_decl_chain = field_decl;

  /* struct objc_ivar ivar_list[]; */
  field_decl = create_field_decl (build_array_type
				  (list_type,
				   build_index_type
				   (build_int_cst (NULL_TREE, size - 1))),
				  "ivar_list");
  chainon (field_decl_chain, field_decl);

  finish_struct (objc_ivar_list_record, field_decl_chain, NULL_TREE);

  return objc_ivar_list_record;
}

/* struct {
     struct _objc__method_prototype_list *method_next;
     int method_count;
     struct objc_method method_list[method_count];
   };  */

static tree
build_method_list_template (tree list_type, int size)
{
  tree objc_ivar_list_record;
  tree field_decl, field_decl_chain;

  objc_ivar_list_record = start_struct (RECORD_TYPE, NULL_TREE);

  /* struct _objc__method_prototype_list *method_next; */
  field_decl = create_field_decl (build_pointer_type
				  (xref_tag (RECORD_TYPE,
					     get_identifier
					     (UTAG_METHOD_PROTOTYPE_LIST))),
				  "method_next");
  field_decl_chain = field_decl;

  /* int method_count; */
  field_decl = create_field_decl (integer_type_node, "method_count");
  chainon (field_decl_chain, field_decl);

  /* struct objc_method method_list[]; */
  field_decl = create_field_decl (build_array_type
				  (list_type,
				   build_index_type
				   (build_int_cst (NULL_TREE, size - 1))),
				  "method_list");
  chainon (field_decl_chain, field_decl);

  finish_struct (objc_ivar_list_record, field_decl_chain, NULL_TREE);

  return objc_ivar_list_record;
}

static tree
build_ivar_list_initializer (tree type, tree field_decl)
{
  tree initlist = NULL_TREE;

  do
    {
      tree ivar = NULL_TREE;

      /* Set name.  */
      if (DECL_NAME (field_decl))
	ivar = tree_cons (NULL_TREE,
			  add_objc_string (DECL_NAME (field_decl),
					   meth_var_names),
			  ivar);
      else
	/* Unnamed bit-field ivar (yuck).  */
	ivar = tree_cons (NULL_TREE, build_int_cst (NULL_TREE, 0), ivar);

      /* Set type.  */
      encode_field_decl (field_decl,
			 obstack_object_size (&util_obstack),
			 OBJC_ENCODE_DONT_INLINE_DEFS);

      /* Null terminate string.  */
      obstack_1grow (&util_obstack, 0);
      ivar
	= tree_cons
	  (NULL_TREE,
	   add_objc_string (get_identifier (obstack_finish (&util_obstack)),
			    meth_var_types),
	   ivar);
      obstack_free (&util_obstack, util_firstobj);

      /* Set offset.  */
      ivar = tree_cons (NULL_TREE, byte_position (field_decl), ivar);
      initlist = tree_cons (NULL_TREE,
			    objc_build_constructor (type, nreverse (ivar)),
			    initlist);
      do
	field_decl = TREE_CHAIN (field_decl);
      while (field_decl && TREE_CODE (field_decl) != FIELD_DECL);
    }
  while (field_decl);

  return objc_build_constructor (build_array_type (type, 0),
				 nreverse (initlist));
}

static tree
generate_ivars_list (tree type, const char *name, int size, tree list)
{
  tree decl, initlist;

  decl = start_var_decl (type, synth_id_with_class_suffix
			       (name, objc_implementation_context));

  initlist = build_tree_list (NULL_TREE, build_int_cst (NULL_TREE, size));
  initlist = tree_cons (NULL_TREE, list, initlist);

  finish_var_decl (decl,
		   objc_build_constructor (TREE_TYPE (decl),
					   nreverse (initlist)));

  return decl;
}

/* Count only the fields occurring in T.  */
static int
ivar_list_length (tree t)
{
  int count = 0;

  for (; t; t = TREE_CHAIN (t))
    if (TREE_CODE (t) == FIELD_DECL)
      ++count;

  return count;
}

static void
generate_ivar_lists (void)
{
  tree initlist, ivar_list_template, chain;
  int size;

  generating_instance_variables = 1;

  if (!objc_ivar_template)
    objc_ivar_template = build_ivar_template ();

  /* Only generate class variables for the root of the inheritance
     hierarchy since these will be the same for every class.  */

  if (CLASS_SUPER_NAME (implementation_template) == NULL_TREE
      && (chain = TYPE_FIELDS (objc_class_template)))
    {
      size = ivar_list_length (chain);

      ivar_list_template = build_ivar_list_template (objc_ivar_template, size);
      initlist = build_ivar_list_initializer (objc_ivar_template, chain);

      UOBJC_CLASS_VARIABLES_decl
	= generate_ivars_list (ivar_list_template, "_OBJC_CLASS_VARIABLES",
			       size, initlist);
    }
  else
    UOBJC_CLASS_VARIABLES_decl = 0;

  chain = CLASS_IVARS (implementation_template);
  if (chain)
    {
      size = ivar_list_length (chain);
      ivar_list_template = build_ivar_list_template (objc_ivar_template, size);
      initlist = build_ivar_list_initializer (objc_ivar_template, chain);

      UOBJC_INSTANCE_VARIABLES_decl
	= generate_ivars_list (ivar_list_template, "_OBJC_INSTANCE_VARIABLES",
			       size, initlist);
    }
  else
    UOBJC_INSTANCE_VARIABLES_decl = 0;

  generating_instance_variables = 0;
}

static tree
build_dispatch_table_initializer (tree type, tree entries)
{
  tree initlist = NULL_TREE;

  do
    {
      tree elemlist = NULL_TREE;

      elemlist = tree_cons (NULL_TREE,
			    build_selector (METHOD_SEL_NAME (entries)),
			    NULL_TREE);

      /* Generate the method encoding if we don't have one already.  */
      if (! METHOD_ENCODING (entries))
	METHOD_ENCODING (entries) =
	  encode_method_prototype (entries);

      elemlist = tree_cons (NULL_TREE,
			    add_objc_string (METHOD_ENCODING (entries),
					     meth_var_types),
			    elemlist);

      elemlist
	= tree_cons (NULL_TREE,
		     convert (ptr_type_node, 
			      build_unary_op (ADDR_EXPR,
					      METHOD_DEFINITION (entries), 1)),
		     elemlist);

      initlist = tree_cons (NULL_TREE,
			    objc_build_constructor (type, nreverse (elemlist)),
			    initlist);

      entries = TREE_CHAIN (entries);
    }
  while (entries);

  return objc_build_constructor (build_array_type (type, 0),
				 nreverse (initlist));
}

/* To accomplish method prototyping without generating all kinds of
   inane warnings, the definition of the dispatch table entries were
   changed from:

	struct objc_method { SEL _cmd; ...; id (*_imp)(); };
   to:
	struct objc_method { SEL _cmd; ...; void *_imp; };  */

static tree
build_method_template (void)
{
  tree _SLT_record;
  tree field_decl, field_decl_chain;

  _SLT_record = start_struct (RECORD_TYPE, get_identifier (UTAG_METHOD));

  /* SEL _cmd; */
  field_decl = create_field_decl (objc_selector_type, "_cmd");
  field_decl_chain = field_decl;

  /* char *method_types; */
  field_decl = create_field_decl (string_type_node, "method_types");
  chainon (field_decl_chain, field_decl);

  /* void *_imp; */
  field_decl = create_field_decl (build_pointer_type (void_type_node),
				  "_imp");
  chainon (field_decl_chain, field_decl);

  finish_struct (_SLT_record, field_decl_chain, NULL_TREE);

  return _SLT_record;
}


static tree
generate_dispatch_table (tree type, const char *name, int size, tree list)
{
  tree decl, initlist;

  decl = start_var_decl (type, synth_id_with_class_suffix
			       (name, objc_implementation_context));

  initlist = build_tree_list (NULL_TREE, build_int_cst (NULL_TREE, 0));
  initlist = tree_cons (NULL_TREE, build_int_cst (NULL_TREE, size), initlist);
  initlist = tree_cons (NULL_TREE, list, initlist);

  finish_var_decl (decl,
		   objc_build_constructor (TREE_TYPE (decl),
					   nreverse (initlist)));

  return decl;
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
	  cgraph_mark_needed_node (cgraph_node (METHOD_DEFINITION (chain)));
	  chain = TREE_CHAIN (chain);
	}

      chain = CLASS_NST_METHODS (impent->imp_context);
      while (chain)
	{
	  cgraph_mark_needed_node (cgraph_node (METHOD_DEFINITION (chain)));
	  chain = TREE_CHAIN (chain);
	}
    }
}

static void
generate_dispatch_tables (void)
{
  tree initlist, chain, method_list_template;
  int size;

  if (!objc_method_template)
    objc_method_template = build_method_template ();

  chain = CLASS_CLS_METHODS (objc_implementation_context);
  if (chain)
    {
      size = list_length (chain);

      method_list_template
	= build_method_list_template (objc_method_template, size);
      initlist
	= build_dispatch_table_initializer (objc_method_template, chain);

      UOBJC_CLASS_METHODS_decl
	= generate_dispatch_table (method_list_template,
				   ((TREE_CODE (objc_implementation_context)
				     == CLASS_IMPLEMENTATION_TYPE)
				    ? "_OBJC_CLASS_METHODS"
				    : "_OBJC_CATEGORY_CLASS_METHODS"),
				   size, initlist);
    }
  else
    UOBJC_CLASS_METHODS_decl = 0;

  chain = CLASS_NST_METHODS (objc_implementation_context);
  if (chain)
    {
      size = list_length (chain);

      method_list_template
	= build_method_list_template (objc_method_template, size);
      initlist
	= build_dispatch_table_initializer (objc_method_template, chain);

      if (TREE_CODE (objc_implementation_context) == CLASS_IMPLEMENTATION_TYPE)
	UOBJC_INSTANCE_METHODS_decl
	  = generate_dispatch_table (method_list_template,
				     "_OBJC_INSTANCE_METHODS",
				     size, initlist);
      else
	/* We have a category.  */
	UOBJC_INSTANCE_METHODS_decl
	  = generate_dispatch_table (method_list_template,
				     "_OBJC_CATEGORY_INSTANCE_METHODS",
				     size, initlist);
    }
  else
    UOBJC_INSTANCE_METHODS_decl = 0;
}

static tree
generate_protocol_list (tree i_or_p)
{
  tree initlist;
  tree refs_decl, lproto, e, plist;
  int size = 0;
  const char *ref_name;

  if (TREE_CODE (i_or_p) == CLASS_INTERFACE_TYPE
      || TREE_CODE (i_or_p) == CATEGORY_INTERFACE_TYPE)
    plist = CLASS_PROTOCOL_LIST (i_or_p);
  else if (TREE_CODE (i_or_p) == PROTOCOL_INTERFACE_TYPE)
    plist = PROTOCOL_LIST (i_or_p);
  else
    abort ();

  /* Compute size.  */
  for (lproto = plist; lproto; lproto = TREE_CHAIN (lproto))
    if (TREE_CODE (TREE_VALUE (lproto)) == PROTOCOL_INTERFACE_TYPE
	&& PROTOCOL_FORWARD_DECL (TREE_VALUE (lproto)))
      size++;

  /* Build initializer.  */
  initlist = tree_cons (NULL_TREE, build_int_cst (NULL_TREE, 0), NULL_TREE);
  e = build_int_cst (build_pointer_type (objc_protocol_template), size);
  initlist = tree_cons (NULL_TREE, e, initlist);

  for (lproto = plist; lproto; lproto = TREE_CHAIN (lproto))
    {
      tree pval = TREE_VALUE (lproto);

      if (TREE_CODE (pval) == PROTOCOL_INTERFACE_TYPE
	  && PROTOCOL_FORWARD_DECL (pval))
	{
	  e = build_unary_op (ADDR_EXPR, PROTOCOL_FORWARD_DECL (pval), 0);
	  initlist = tree_cons (NULL_TREE, e, initlist);
	}
    }

  /* static struct objc_protocol *refs[n]; */

  if (TREE_CODE (i_or_p) == PROTOCOL_INTERFACE_TYPE)
    ref_name = synth_id_with_class_suffix ("_OBJC_PROTOCOL_REFS", i_or_p);
  else if (TREE_CODE (i_or_p) == CLASS_INTERFACE_TYPE)
    ref_name = synth_id_with_class_suffix ("_OBJC_CLASS_PROTOCOLS", i_or_p);
  else if (TREE_CODE (i_or_p) == CATEGORY_INTERFACE_TYPE)
    ref_name = synth_id_with_class_suffix ("_OBJC_CATEGORY_PROTOCOLS", i_or_p);
  else
    abort ();

  refs_decl = start_var_decl
	      (build_array_type
	       (build_pointer_type (objc_protocol_template),
		build_index_type (build_int_cst (NULL_TREE, size + 2))),
	       ref_name);

  finish_var_decl (refs_decl, objc_build_constructor (TREE_TYPE (refs_decl),
  						      nreverse (initlist)));

  return refs_decl;
}

static tree
build_category_initializer (tree type, tree cat_name, tree class_name,
			    tree instance_methods, tree class_methods,
			    tree protocol_list)
{
  tree initlist = NULL_TREE, expr;

  initlist = tree_cons (NULL_TREE, cat_name, initlist);
  initlist = tree_cons (NULL_TREE, class_name, initlist);

  if (!instance_methods)
    initlist = tree_cons (NULL_TREE, build_int_cst (NULL_TREE, 0), initlist);
  else
    {
      expr = build_unary_op (ADDR_EXPR, instance_methods, 0);
      initlist = tree_cons (NULL_TREE, expr, initlist);
    }
  if (!class_methods)
    initlist = tree_cons (NULL_TREE, build_int_cst (NULL_TREE, 0), initlist);
  else
    {
      expr = build_unary_op (ADDR_EXPR, class_methods, 0);
      initlist = tree_cons (NULL_TREE, expr, initlist);
    }

  /* protocol_list = */
  if (!protocol_list)
     initlist = tree_cons (NULL_TREE, build_int_cst (NULL_TREE, 0), initlist);
  else
    {
      expr = convert (build_pointer_type
		      (build_pointer_type 
		       (objc_protocol_template)),
		      build_unary_op (ADDR_EXPR, protocol_list, 0));
      initlist = tree_cons (NULL_TREE, expr, initlist);
    }

  return objc_build_constructor (type, nreverse (initlist));
}

/* struct _objc_class {
     struct objc_class *isa;
     struct objc_class *super_class;
     char *name;
     long version;
     long info;
     long instance_size;
     struct objc_ivar_list *ivars;
     struct objc_method_list *methods;
     if (flag_next_runtime)
       struct objc_cache *cache;
     else {
       struct sarray *dtable;
       struct objc_class *subclass_list;
       struct objc_class *sibling_class;
     }
     struct objc_protocol_list *protocols;
     if (flag_next_runtime)
       void *sel_id;
     void *gc_object_type;
   };  */

static tree
build_shared_structure_initializer (tree type, tree isa, tree super,
				    tree name, tree size, int status,
				    tree dispatch_table, tree ivar_list,
				    tree protocol_list)
{
  tree initlist = NULL_TREE, expr;

  /* isa = */
  initlist = tree_cons (NULL_TREE, isa, initlist);

  /* super_class = */
  initlist = tree_cons (NULL_TREE, super, initlist);

  /* name = */
  initlist = tree_cons (NULL_TREE, default_conversion (name), initlist);

  /* version = */
  initlist = tree_cons (NULL_TREE, build_int_cst (long_integer_type_node, 0),
			initlist);

  /* info = */
  initlist = tree_cons (NULL_TREE,
			build_int_cst (long_integer_type_node, status),
			initlist);

  /* instance_size = */
  initlist = tree_cons (NULL_TREE, convert (long_integer_type_node, size),
			initlist);

  /* objc_ivar_list = */
  if (!ivar_list)
    initlist = tree_cons (NULL_TREE, build_int_cst (NULL_TREE, 0), initlist);
  else
    {
      expr = build_unary_op (ADDR_EXPR, ivar_list, 0);
      initlist = tree_cons (NULL_TREE, expr, initlist);
    }

  /* objc_method_list = */
  if (!dispatch_table)
    initlist = tree_cons (NULL_TREE, build_int_cst (NULL_TREE, 0), initlist);
  else
    {
      expr = build_unary_op (ADDR_EXPR, dispatch_table, 0);
      initlist = tree_cons (NULL_TREE, expr, initlist);
    }

  if (flag_next_runtime)
    /* method_cache = */
    initlist = tree_cons (NULL_TREE, build_int_cst (NULL_TREE, 0), initlist);
  else
    {
      /* dtable = */
      initlist = tree_cons (NULL_TREE, build_int_cst (NULL_TREE, 0), initlist);

      /* subclass_list = */
      initlist = tree_cons (NULL_TREE, build_int_cst (NULL_TREE, 0), initlist);

      /* sibling_class = */
      initlist = tree_cons (NULL_TREE, build_int_cst (NULL_TREE, 0), initlist);
    }

  /* protocol_list = */
  if (! protocol_list)
    initlist = tree_cons (NULL_TREE, build_int_cst (NULL_TREE, 0), initlist);
  else
    {
      expr = convert (build_pointer_type
		      (build_pointer_type 
		       (objc_protocol_template)),
		      build_unary_op (ADDR_EXPR, protocol_list, 0));
      initlist = tree_cons (NULL_TREE, expr, initlist);
    }

  if (flag_next_runtime)
    /* sel_id = NULL */
    initlist = tree_cons (NULL_TREE, build_int_cst (NULL_TREE, 0), initlist);

  /* gc_object_type = NULL */
  initlist = tree_cons (NULL_TREE, build_int_cst (NULL_TREE, 0), initlist);

  return objc_build_constructor (type, nreverse (initlist));
}

/* Retrieve category interface CAT_NAME (if any) associated with CLASS.  */

static inline tree
lookup_category (tree class, tree cat_name)
{
  tree category = CLASS_CATEGORY_LIST (class);

  while (category && CLASS_SUPER_NAME (category) != cat_name)
    category = CLASS_CATEGORY_LIST (category);
  return category;
}

/* static struct objc_category _OBJC_CATEGORY_<name> = { ... };  */

static void
generate_category (tree cat)
{
  tree decl;
  tree initlist, cat_name_expr, class_name_expr;
  tree protocol_decl, category;

  add_class_reference (CLASS_NAME (cat));
  cat_name_expr = add_objc_string (CLASS_SUPER_NAME (cat), class_names);

  class_name_expr = add_objc_string (CLASS_NAME (cat), class_names);

  category = lookup_category (implementation_template,
				CLASS_SUPER_NAME (cat));

  if (category && CLASS_PROTOCOL_LIST (category))
    {
      generate_protocol_references (CLASS_PROTOCOL_LIST (category));
      protocol_decl = generate_protocol_list (category);
    }
  else
    protocol_decl = 0;

  decl = start_var_decl (objc_category_template,
			 synth_id_with_class_suffix
			 ("_OBJC_CATEGORY", objc_implementation_context));

  initlist = build_category_initializer (TREE_TYPE (decl),
					 cat_name_expr, class_name_expr,
					 UOBJC_INSTANCE_METHODS_decl,
					 UOBJC_CLASS_METHODS_decl,
					 protocol_decl);

  finish_var_decl (decl, initlist);
}

/* static struct objc_class _OBJC_METACLASS_Foo={ ... };
   static struct objc_class _OBJC_CLASS_Foo={ ... };  */

static void
generate_shared_structures (void)
{
  tree sc_spec, decl_specs, decl;
  tree name_expr, super_expr, root_expr;
  tree my_root_id = NULL_TREE, my_super_id = NULL_TREE;
  tree cast_type, initlist, protocol_decl;

  my_super_id = CLASS_SUPER_NAME (implementation_template);
  if (my_super_id)
    {
      add_class_reference (my_super_id);

      /* Compute "my_root_id" - this is required for code generation.
         the "isa" for all meta class structures points to the root of
         the inheritance hierarchy (e.g. "__Object")...  */
      my_root_id = my_super_id;
      do
	{
	  tree my_root_int = lookup_interface (my_root_id);

	  if (my_root_int && CLASS_SUPER_NAME (my_root_int))
	    my_root_id = CLASS_SUPER_NAME (my_root_int);
	  else
	    break;
	}
      while (1);
    }
  else
    /* No super class.  */
    my_root_id = CLASS_NAME (implementation_template);

  cast_type = build_pointer_type (objc_class_template);
  name_expr = add_objc_string (CLASS_NAME (implementation_template),
			       class_names);

  /* Install class `isa' and `super' pointers at runtime.  */
  if (my_super_id)
    {
      super_expr = add_objc_string (my_super_id, class_names);
      super_expr = build_c_cast (cast_type, super_expr); /* cast! */
    }
  else
    super_expr = build_int_cst (NULL_TREE, 0);

  root_expr = add_objc_string (my_root_id, class_names);
  root_expr = build_c_cast (cast_type, root_expr); /* cast! */

  if (CLASS_PROTOCOL_LIST (implementation_template))
    {
      generate_protocol_references
	(CLASS_PROTOCOL_LIST (implementation_template));
      protocol_decl = generate_protocol_list (implementation_template);
    }
  else
    protocol_decl = 0;

  /* static struct objc_class _OBJC_METACLASS_Foo = { ... }; */

  sc_spec = build_tree_list (NULL_TREE, ridpointers[(int) RID_STATIC]);
  decl_specs = tree_cons (NULL_TREE, objc_class_template, sc_spec);

  decl = start_var_decl (objc_class_template,
			 IDENTIFIER_POINTER
			 (DECL_NAME (UOBJC_METACLASS_decl)));

  initlist
    = build_shared_structure_initializer
      (TREE_TYPE (decl),
       root_expr, super_expr, name_expr,
       convert (integer_type_node, TYPE_SIZE_UNIT (objc_class_template)),
       2 /*CLS_META*/,
       UOBJC_CLASS_METHODS_decl,
       UOBJC_CLASS_VARIABLES_decl,
       protocol_decl);

  finish_var_decl (decl, initlist);

  /* static struct objc_class _OBJC_CLASS_Foo={ ... }; */

  decl = start_var_decl (objc_class_template,
			 IDENTIFIER_POINTER
			 (DECL_NAME (UOBJC_CLASS_decl)));

  initlist
    = build_shared_structure_initializer
      (TREE_TYPE (decl),
       build_unary_op (ADDR_EXPR, UOBJC_METACLASS_decl, 0),
       super_expr, name_expr,
       convert (integer_type_node,
		TYPE_SIZE_UNIT (CLASS_STATIC_TEMPLATE
				(implementation_template))),
       1 /*CLS_FACTORY*/,
       UOBJC_INSTANCE_METHODS_decl,
       UOBJC_INSTANCE_VARIABLES_decl,
       protocol_decl);

  finish_var_decl (decl, initlist);
}


static const char *
synth_id_with_class_suffix (const char *preamble, tree ctxt)
{
  static char string[BUFSIZE];

  if (TREE_CODE (ctxt) == CLASS_IMPLEMENTATION_TYPE
      || TREE_CODE (ctxt) == CLASS_INTERFACE_TYPE)
    {
      sprintf (string, "%s_%s", preamble,
	       IDENTIFIER_POINTER (CLASS_NAME (ctxt)));
    }
  else if (TREE_CODE (ctxt) == CATEGORY_IMPLEMENTATION_TYPE
	   || TREE_CODE (ctxt) == CATEGORY_INTERFACE_TYPE)
    {
      /* We have a category.  */
      const char *const class_name
	= IDENTIFIER_POINTER (CLASS_NAME (objc_implementation_context));
      const char *const class_super_name
	= IDENTIFIER_POINTER (CLASS_SUPER_NAME (objc_implementation_context));
      sprintf (string, "%s_%s_%s", preamble, class_name, class_super_name);
    }
  else if (TREE_CODE (ctxt) == PROTOCOL_INTERFACE_TYPE)
    {
      const char *protocol_name = IDENTIFIER_POINTER (PROTOCOL_NAME (ctxt));
      sprintf (string, "%s_%s", preamble, protocol_name);
    }
  else
    abort ();

  return string;
}

/* If type is empty or only type qualifiers are present, add default
   type of id (otherwise grokdeclarator will default to int).  */

static tree
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

/*   Usage:
		keyworddecl:
			selector ':' '(' typename ')' identifier

     Purpose:
		Transform an Objective-C keyword argument into
		the C equivalent parameter declarator.

     In:	key_name, an "identifier_node" (optional).
		arg_type, a  "tree_list" (optional).
		arg_name, an "identifier_node".

     Note:	It would be really nice to strongly type the preceding
		arguments in the function prototype; however, then I
		could not use the "accessor" macros defined in "tree.h".

     Out:	an instance of "keyword_decl".  */

tree
objc_build_keyword_decl (tree key_name, tree arg_type, tree arg_name)
{
  tree keyword_decl;

  /* If no type is specified, default to "id".  */
  arg_type = adjust_type_for_id_default (arg_type);

  keyword_decl = make_node (KEYWORD_DECL);

  TREE_TYPE (keyword_decl) = arg_type;
  KEYWORD_ARG_NAME (keyword_decl) = arg_name;
  KEYWORD_KEY_NAME (keyword_decl) = key_name;

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
      if (TREE_CODE (selector) == KEYWORD_DECL)
	key_name = KEYWORD_KEY_NAME (key_chain);
      else if (TREE_CODE (selector) == TREE_LIST)
	key_name = TREE_PURPOSE (key_chain);
      else
	abort ();

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
      if (TREE_CODE (selector) == KEYWORD_DECL)
	key_name = KEYWORD_KEY_NAME (key_chain);
      else if (TREE_CODE (selector) == TREE_LIST)
	{
	  key_name = TREE_PURPOSE (key_chain);
	  /* The keyword decl chain will later be used as a function argument
	     chain.  Unhook the selector itself so as to not confuse other
	     parts of the compiler.  */
	  TREE_PURPOSE (key_chain) = NULL_TREE;
	}
      else
	abort ();

      if (key_name)
	strcat (buf, IDENTIFIER_POINTER (key_name));
      strcat (buf, ":");
    }

  return get_identifier (buf);
}

/* Used for declarations and definitions.  */

static tree
build_method_decl (enum tree_code code, tree ret_type, tree selector,
		   tree add_args)
{
  tree method_decl;

  /* If no type is specified, default to "id".  */
  ret_type = adjust_type_for_id_default (ret_type);

  method_decl = make_node (code);
  TREE_TYPE (method_decl) = ret_type;

  /* If we have a keyword selector, create an identifier_node that
     represents the full selector name (`:' included)...  */
  if (TREE_CODE (selector) == KEYWORD_DECL)
    {
      METHOD_SEL_NAME (method_decl) = build_keyword_selector (selector);
      METHOD_SEL_ARGS (method_decl) = selector;
      METHOD_ADD_ARGS (method_decl) = add_args;
    }
  else
    {
      METHOD_SEL_NAME (method_decl) = selector;
      METHOD_SEL_ARGS (method_decl) = NULL_TREE;
      METHOD_ADD_ARGS (method_decl) = NULL_TREE;
    }

  return method_decl;
}

#define METHOD_DEF 0
#define METHOD_REF 1

/* Used by `build_objc_method_call' and `comp_proto_with_proto'.  Return
   an argument list for method METH.  CONTEXT is either METHOD_DEF or
   METHOD_REF, saying whether we are trying to define a method or call
   one.  SUPERFLAG says this is for a send to super; this makes a
   difference for the NeXT calling sequence in which the lookup and
   the method call are done together.  If METH is null, user-defined
   arguments (i.e., beyond self and _cmd) shall be represented by `...'.  */

static tree
get_arg_type_list (tree meth, int context, int superflag)
{
  tree arglist, akey;

  /* Receiver type.  */
  if (flag_next_runtime && superflag)
    arglist = build_tree_list (NULL_TREE, objc_super_type);
  else if (context == METHOD_DEF && TREE_CODE (meth) == INSTANCE_METHOD_DECL)
    arglist = build_tree_list (NULL_TREE, objc_instance_type);
  else
    arglist = build_tree_list (NULL_TREE, objc_object_type);

  /* Selector type - will eventually change to `int'.  */
  chainon (arglist, build_tree_list (NULL_TREE, objc_selector_type));

  /* No actual method prototype given -- assume that remaining arguments
     are `...'.  */
  if (!meth)
    return arglist;

  /* Build a list of argument types.  */
  for (akey = METHOD_SEL_ARGS (meth); akey; akey = TREE_CHAIN (akey))
    {
      tree arg_type = TREE_VALUE (TREE_TYPE (akey));

      /* Decay arrays and functions into pointers.  */
      if (TREE_CODE (arg_type) == ARRAY_TYPE)
	arg_type = build_pointer_type (TREE_TYPE (arg_type));
      else if (TREE_CODE (arg_type) == FUNCTION_TYPE)
	arg_type = build_pointer_type (arg_type);

      chainon (arglist, build_tree_list (NULL_TREE, arg_type));
    }

  if (METHOD_ADD_ARGS (meth))
    {
      for (akey = TREE_CHAIN (METHOD_ADD_ARGS (meth));
	   akey; akey = TREE_CHAIN (akey))
	{
	  tree arg_type = TREE_TYPE (TREE_VALUE (akey));

	  chainon (arglist, build_tree_list (NULL_TREE, arg_type));
	}

      if (!TREE_OVERFLOW (METHOD_ADD_ARGS (meth)))
	goto lack_of_ellipsis;
    }
  else
    {
     lack_of_ellipsis:
      chainon (arglist, OBJC_VOID_AT_END);
    }

  return arglist;
}

static tree
check_duplicates (hash hsh, int methods, int is_class)
{
  tree meth = NULL_TREE;

  if (hsh)
    {
      meth = hsh->key;

      if (hsh->list)
        {
	  /* We have two or more methods with the same name but
	     different types.  */
	  attr loop;

	  warning ("multiple %s named %<%c%s%> found",
		   methods ? "methods" : "selectors",
		   (is_class ? '+' : '-'),
		   IDENTIFIER_POINTER (METHOD_SEL_NAME (meth)));

	  warn_with_method (methods ? "using" : "found",
			    ((TREE_CODE (meth) == INSTANCE_METHOD_DECL)
			     ? '-'
			     : '+'), 
			    meth);
	  for (loop = hsh->list; loop; loop = loop->next)
	    warn_with_method ("also found",
			      ((TREE_CODE (loop->value) == INSTANCE_METHOD_DECL)
			       ? '-' 
			       : '+'),
			      loop->value);
        }
    }
  return meth;
}

/* If RECEIVER is a class reference, return the identifier node for
   the referenced class.  RECEIVER is created by objc_get_class_reference,
   so we check the exact form created depending on which runtimes are
   used.  */

static tree
receiver_is_class_object (tree receiver, int self, int super)
{
  tree chain, exp, arg;

  /* The receiver is 'self' or 'super' in the context of a class method.  */
  if (objc_method_context
      && TREE_CODE (objc_method_context) == CLASS_METHOD_DECL
      && (self || super))
    return (super
	    ? CLASS_SUPER_NAME (implementation_template)
	    : CLASS_NAME (implementation_template));

  if (flag_next_runtime)
    {
      /* The receiver is a variable created by
         build_class_reference_decl.  */
      if (TREE_CODE (receiver) == VAR_DECL && IS_CLASS (TREE_TYPE (receiver)))
        /* Look up the identifier.  */
	for (chain = cls_ref_chain; chain; chain = TREE_CHAIN (chain))
	  if (TREE_PURPOSE (chain) == receiver)
            return TREE_VALUE (chain);
    }

  /* The receiver is a function call that returns an id.  Check if
     it is a call to objc_getClass, if so, pick up the class name.  */
  if (TREE_CODE (receiver) == CALL_EXPR
      && (exp = TREE_OPERAND (receiver, 0))
      && TREE_CODE (exp) == ADDR_EXPR
      && (exp = TREE_OPERAND (exp, 0))
      && TREE_CODE (exp) == FUNCTION_DECL
      /* For some reason, we sometimes wind up with multiple FUNCTION_DECL
	 prototypes for objc_get_class().  Thankfully, they seem to share the
	 same function type.  */
      && TREE_TYPE (exp) == TREE_TYPE (objc_get_class_decl)
      && !strcmp (IDENTIFIER_POINTER (DECL_NAME (exp)), TAG_GETCLASS)
      /* We have a call to objc_get_class/objc_getClass!  */
      && (arg = TREE_OPERAND (receiver, 1))
      && TREE_CODE (arg) == TREE_LIST
      && (arg = TREE_VALUE (arg)))
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
objc_build_message_expr (tree mess)
{
  tree receiver = TREE_PURPOSE (mess);
  tree sel_name;
#ifdef OBJCPLUS
  tree args = TREE_PURPOSE (TREE_VALUE (mess));
#else
  tree args = TREE_VALUE (mess);
#endif
  tree method_params = NULL_TREE;

  if (TREE_CODE (receiver) == ERROR_MARK)
    return error_mark_node;

  /* Obtain the full selector name.  */
  if (TREE_CODE (args) == IDENTIFIER_NODE)
    /* A unary selector.  */
    sel_name = args;
  else if (TREE_CODE (args) == TREE_LIST)
    sel_name = build_keyword_selector (args);
  else
    abort ();

  /* Build the parameter list to give to the method.  */
  if (TREE_CODE (args) == TREE_LIST)
#ifdef OBJCPLUS
    method_params = chainon (args, TREE_VALUE (TREE_VALUE (mess)));
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
    return build_min_nt (MESSAGE_SEND_EXPR, receiver, sel_name,
			 method_params);
#endif

  return objc_finish_message_expr (receiver, sel_name, method_params);
}

/* Look up method SEL_NAME that would be suitable for receiver
   of type 'id' (if IS_CLASS is zero) or 'Class' (if IS_CLASS is
   nonzero), and report on any duplicates.  */

static tree
lookup_method_in_hash_lists (tree sel_name, int is_class)
{
  hash method_prototype = NULL;

  if (!is_class)
    method_prototype = hash_lookup (nst_method_hash_list,
				    sel_name);
					
  if (!method_prototype)
    {
      method_prototype = hash_lookup (cls_method_hash_list,
				      sel_name);
      is_class = 1;
    }

  return check_duplicates (method_prototype, 1, is_class);
}

/* The 'objc_finish_message_expr' routine is called from within
   'objc_build_message_expr' for non-template functions.  In the case of
   C++ template functions, it is called from 'build_expr_from_tree'
   (in decl2.c) after RECEIVER and METHOD_PARAMS have been expanded.  */

tree
objc_finish_message_expr (tree receiver, tree sel_name, tree method_params)
{
  tree method_prototype = NULL_TREE, rprotos = NULL_TREE, rtype;
  tree selector, retval, class_tree;
  int self, super, have_cast;

  /* Extract the receiver of the message, as well as its type
     (where the latter may take the form of a cast or be inferred
     from the implementation context).  */
  rtype = receiver;
  while (TREE_CODE (rtype) == COMPOUND_EXPR
	      || TREE_CODE (rtype) == MODIFY_EXPR
	      || TREE_CODE (rtype) == NOP_EXPR
	      || TREE_CODE (rtype) == CONVERT_EXPR
	      || TREE_CODE (rtype) == COMPONENT_REF)
    rtype = TREE_OPERAND (rtype, 0);
  self = (rtype == self_decl);
  super = (rtype == UOBJC_SUPER_decl);
  rtype = TREE_TYPE (receiver);
  have_cast = (TREE_CODE (receiver) == NOP_EXPR
	       || (TREE_CODE (receiver) == COMPOUND_EXPR
		   && !IS_SUPER (rtype)));

  /* If the receiver is a class object, retrieve the corresponding
     @interface, if one exists. */
  class_tree = receiver_is_class_object (receiver, self, super);

  /* Now determine the receiver type (if an explicit cast has not been
     provided).  */
  if (!have_cast)
    {
      if (class_tree)
	rtype = lookup_interface (class_tree);
      /* Handle `self' and `super'.  */
      else if (super)
	{
	  if (!CLASS_SUPER_NAME (implementation_template))
	    {
	      error ("no super class declared in @interface for %qs",
		     IDENTIFIER_POINTER (CLASS_NAME (implementation_template)));
	      return error_mark_node;
	    }
	  rtype = lookup_interface (CLASS_SUPER_NAME (implementation_template));
	}
      else if (self)
	rtype = lookup_interface (CLASS_NAME (implementation_template));
    }

  /* If receiver is of type `id' or `Class' (or if the @interface for a
     class is not visible), we shall be satisfied with the existence of
     any instance or class method. */
  if (!rtype || objc_is_id (rtype))
    {
      if (!rtype)
	rtype = xref_tag (RECORD_TYPE, class_tree);
      else
	{
	  class_tree = (IS_CLASS (rtype) ? objc_class_name : NULL_TREE);
	  rprotos = (TYPE_HAS_OBJC_INFO (TREE_TYPE (rtype))
		     ? TYPE_OBJC_PROTOCOL_LIST (TREE_TYPE (rtype))
		     : NULL_TREE);
	  rtype = NULL_TREE;
	}

      if (rprotos)
	{
	  /* If messaging 'id <Protos>' or 'Class <Proto>', first search
	     in protocols themselves for the method prototype.  */
	  method_prototype
	    = lookup_method_in_protocol_list (rprotos, sel_name,
					      class_tree != NULL_TREE);

	  /* If messaging 'Class <Proto>' but did not find a class method
	     prototype, search for an instance method instead, and warn
	     about having done so.  */
	  if (!method_prototype && !rtype && class_tree != NULL_TREE)
	    {
	      method_prototype
		= lookup_method_in_protocol_list (rprotos, sel_name, 0);

	      if (method_prototype)
		warning ("found %<-%s%> instead of %<+%s%> in protocol(s)",
			 IDENTIFIER_POINTER (sel_name),
			 IDENTIFIER_POINTER (sel_name));
	    }
	}
    }
  else
    {
      tree orig_rtype = rtype, saved_rtype;

      if (TREE_CODE (rtype) == POINTER_TYPE)
	rtype = TREE_TYPE (rtype);
      /* Traverse typedef aliases */
      while (TREE_CODE (rtype) == RECORD_TYPE && OBJC_TYPE_NAME (rtype)
	     && TREE_CODE (OBJC_TYPE_NAME (rtype)) == TYPE_DECL
	     && DECL_ORIGINAL_TYPE (OBJC_TYPE_NAME (rtype)))
	rtype = DECL_ORIGINAL_TYPE (OBJC_TYPE_NAME (rtype));
      saved_rtype = rtype;
      if (TYPED_OBJECT (rtype))
	{
	  rprotos = TYPE_OBJC_PROTOCOL_LIST (rtype);
	  rtype = TYPE_OBJC_INTERFACE (rtype);
	}
      /* If we could not find an @interface declaration, we must have
	 only seen a @class declaration; so, we cannot say anything
	 more intelligent about which methods the receiver will
	 understand. */
      if (!rtype || TREE_CODE (rtype) == IDENTIFIER_NODE)
	rtype = saved_rtype;
      else if (TREE_CODE (rtype) == CLASS_INTERFACE_TYPE
	  || TREE_CODE (rtype) == CLASS_IMPLEMENTATION_TYPE)
	{
	  /* We have a valid ObjC class name.  Look up the method name
	     in the published @interface for the class (and its
	     superclasses). */
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
	  warning ("invalid receiver type %qs",
		   gen_type_name (orig_rtype));
	  /* After issuing the "invalid receiver" warning, perform method
	     lookup as if we were messaging 'id'.  */
	  rtype = rprotos = NULL_TREE;
	}
    }	


  /* For 'id' or 'Class' receivers, search in the global hash table
     as a last resort.  For all receivers, warn if protocol searches
     have failed.  */
  if (!method_prototype)
    {
      if (rprotos)
	warning ("%<%c%s%> not found in protocol(s)",
		 (class_tree ? '+' : '-'),
		 IDENTIFIER_POINTER (sel_name));

      if (!rtype)
	method_prototype
	  = lookup_method_in_hash_lists (sel_name, class_tree != NULL_TREE);
    }

  if (!method_prototype)
    {
      static bool warn_missing_methods = false;

      if (rtype)
	warning ("%qs may not respond to %<%c%s%>",
		 IDENTIFIER_POINTER (OBJC_TYPE_NAME (rtype)),
		 (class_tree ? '+' : '-'),
		 IDENTIFIER_POINTER (sel_name));
      /* If we are messaging an 'id' or 'Class' object and made it here,
	 then we have failed to find _any_ instance or class method,
	 respectively.  */
      else
	warning ("no %<%c%s%> method found",
		 (class_tree ? '+' : '-'),
		 IDENTIFIER_POINTER (sel_name));

      if (!warn_missing_methods)
	{
	  warning ("(Messages without a matching method signature");
	  warning ("will be assumed to return %<id%> and accept");
	  warning ("%<...%> as arguments.)");
	  warn_missing_methods = true;
	}
    }

  /* Save the selector name for printing error messages.  */
  current_objc_message_selector = sel_name;

  /* Build the parameters list for looking up the method.
     These are the object itself and the selector.  */

  if (flag_typed_selectors)
    selector = build_typed_selector_reference (sel_name, method_prototype);
  else
    selector = build_selector_reference (sel_name);

  retval = build_objc_method_call (super, method_prototype,
				   receiver,
				   selector, method_params);

  current_objc_message_selector = 0;

  return retval;
}

/* Build a tree expression to send OBJECT the operation SELECTOR,
   looking up the method on object LOOKUP_OBJECT (often same as OBJECT),
   assuming the method has prototype METHOD_PROTOTYPE.
   (That is an INSTANCE_METHOD_DECL or CLASS_METHOD_DECL.)
   Use METHOD_PARAMS as list of args to pass to the method.
   If SUPER_FLAG is nonzero, we look up the superclass's method.  */

static tree
build_objc_method_call (int super_flag, tree method_prototype,
			tree lookup_object, tree selector,
			tree method_params)
{
  tree sender = (super_flag ? umsg_super_decl :
		 (!flag_next_runtime || flag_nil_receivers
		  ? umsg_decl
		  : umsg_nonnil_decl));
  tree rcv_p = (super_flag ? objc_super_type : objc_object_type);

  /* If a prototype for the method to be called exists, then cast
     the sender's return type and arguments to match that of the method.
     Otherwise, leave sender as is.  */
  tree ret_type
    = (method_prototype
       ? TREE_VALUE (TREE_TYPE (method_prototype))
       : objc_object_type);
  tree sender_cast
    = build_pointer_type
      (build_function_type
       (ret_type,
	get_arg_type_list
	(method_prototype, METHOD_REF, super_flag)));
  tree method, t;

  lookup_object = build_c_cast (rcv_p, lookup_object);
    
  /* Use SAVE_EXPR to avoid evaluating the receiver twice.  */
  lookup_object = save_expr (lookup_object);

  if (flag_next_runtime)
    {
      /* If we are returning a struct in memory, and the address
	 of that memory location is passed as a hidden first
	 argument, then change which messenger entry point this
	 expr will call.  NB: Note that sender_cast remains
	 unchanged (it already has a struct return type).  */
      if (!targetm.calls.struct_value_rtx (0, 0)
	  && (TREE_CODE (ret_type) == RECORD_TYPE
	      || TREE_CODE (ret_type) == UNION_TYPE)
	  && targetm.calls.return_in_memory (ret_type, 0))
	sender = (super_flag ? umsg_super_stret_decl :
		flag_nil_receivers ? umsg_stret_decl : umsg_nonnil_stret_decl);

      method_params = tree_cons (NULL_TREE, lookup_object,
				 tree_cons (NULL_TREE, selector,
					    method_params));
      method = build_fold_addr_expr (sender);
    }
  else
    {
      /* This is the portable (GNU) way.  */
      tree object;

      /* First, call the lookup function to get a pointer to the method,
	 then cast the pointer, then call it with the method arguments.  */
      
      object = (super_flag ? self_decl : lookup_object);

      t = tree_cons (NULL_TREE, selector, NULL_TREE);
      t = tree_cons (NULL_TREE, lookup_object, t);
      method = build_function_call (sender, t);

      /* Pass the object to the method.  */
      method_params = tree_cons (NULL_TREE, object,
				 tree_cons (NULL_TREE, selector,
					    method_params));
    }

  /* ??? Selector is not at this point something we can use inside
     the compiler itself.  Set it to garbage for the nonce.  */
  t = build (OBJ_TYPE_REF, sender_cast, method, lookup_object, size_zero_node);
  return build_function_call (t, method_params);
}

static void
build_protocol_reference (tree p)
{
  tree decl;
  const char *proto_name;

  /* static struct _objc_protocol _OBJC_PROTOCOL_<mumble>; */

  proto_name = synth_id_with_class_suffix ("_OBJC_PROTOCOL", p);
  decl = start_var_decl (objc_protocol_template, proto_name);

  PROTOCOL_FORWARD_DECL (p) = decl;
}

/* This function is called by the parser when (and only when) a
   @protocol() expression is found, in order to compile it.  */
tree
objc_build_protocol_expr (tree protoname)
{
  tree expr;
  tree p = lookup_protocol (protoname);

  if (!p)
    {
      error ("cannot find protocol declaration for %qs",
	     IDENTIFIER_POINTER (protoname));
      return error_mark_node;
    }

  if (!PROTOCOL_FORWARD_DECL (p))
    build_protocol_reference (p);

  expr = build_unary_op (ADDR_EXPR, PROTOCOL_FORWARD_DECL (p), 0);

  /* ??? Ideally we'd build the reference with objc_protocol_type directly,
     if we have it, rather than converting it here.  */
  expr = convert (objc_protocol_type, expr);

  /* The @protocol() expression is being compiled into a pointer to a
     statically allocated instance of the Protocol class.  To become
     usable at runtime, the 'isa' pointer of the instance need to be
     fixed up at runtime by the runtime library, to point to the
     actual 'Protocol' class.  */

  /* For the GNU runtime, put the static Protocol instance in the list
     of statically allocated instances, so that we make sure that its
     'isa' pointer is fixed up at runtime by the GNU runtime library
     to point to the Protocol class (at runtime, when loading the
     module, the GNU runtime library loops on the statically allocated
     instances (as found in the defs field in objc_symtab) and fixups
     all the 'isa' pointers of those objects).  */
  if (! flag_next_runtime)
    {
      /* This type is a struct containing the fields of a Protocol
        object.  (Cfr. objc_protocol_type instead is the type of a pointer
        to such a struct).  */
      tree protocol_struct_type = xref_tag
       (RECORD_TYPE, get_identifier (PROTOCOL_OBJECT_CLASS_NAME));
      tree *chain;

      /* Look for the list of Protocol statically allocated instances
        to fixup at runtime.  Create a new list to hold Protocol
        statically allocated instances, if the list is not found.  At
        present there is only another list, holding NSConstantString
        static instances to be fixed up at runtime.  */
      for (chain = &objc_static_instances;
	   *chain && TREE_VALUE (*chain) != protocol_struct_type;
	   chain = &TREE_CHAIN (*chain));
      if (!*chain)
	{
         *chain = tree_cons (NULL_TREE, protocol_struct_type, NULL_TREE);
         add_objc_string (OBJC_TYPE_NAME (protocol_struct_type),
                          class_names);
       }

      /* Add this statically allocated instance to the Protocol list.  */
      TREE_PURPOSE (*chain) = tree_cons (NULL_TREE,
					 PROTOCOL_FORWARD_DECL (p),
					 TREE_PURPOSE (*chain));
    }


  return expr;
}

/* This function is called by the parser when a @selector() expression
   is found, in order to compile it.  It is only called by the parser
   and only to compile a @selector().  */
tree
objc_build_selector_expr (tree selnamelist)
{
  tree selname;

  /* Obtain the full selector name.  */
  if (TREE_CODE (selnamelist) == IDENTIFIER_NODE)
    /* A unary selector.  */
    selname = selnamelist;
  else if (TREE_CODE (selnamelist) == TREE_LIST)
    selname = build_keyword_selector (selnamelist);
  else
    abort ();

  /* If we are required to check @selector() expressions as they
     are found, check that the selector has been declared.  */
  if (warn_undeclared_selector)
    {
      /* Look the selector up in the list of all known class and
         instance methods (up to this line) to check that the selector
         exists.  */
      hash hsh;

      /* First try with instance methods.  */
      hsh = hash_lookup (nst_method_hash_list, selname);

      /* If not found, try with class methods.  */
      if (!hsh)
	{
	  hsh = hash_lookup (cls_method_hash_list, selname);
	}

      /* If still not found, print out a warning.  */
      if (!hsh)
	{
	  warning ("undeclared selector %qs", IDENTIFIER_POINTER (selname));
	}
    }


  if (flag_typed_selectors)
    return build_typed_selector_reference (selname, 0);
  else
    return build_selector_reference (selname);
}

tree
objc_build_encode_expr (tree type)
{
  tree result;
  const char *string;

  encode_type (type, obstack_object_size (&util_obstack),
	       OBJC_ENCODE_INLINE_DEFS);
  obstack_1grow (&util_obstack, 0);    /* null terminate string */
  string = obstack_finish (&util_obstack);

  /* Synthesize a string that represents the encoded struct/union.  */
  result = my_build_string (strlen (string) + 1, string);
  obstack_free (&util_obstack, util_firstobj);
  return result;
}

static tree
build_ivar_reference (tree id)
{
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
      warning ("instance variable %qs accessed in class method",
	       IDENTIFIER_POINTER (id));
      self_decl = convert (objc_instance_type, self_decl); /* cast */
    }

  return build_component_ref (build_indirect_ref (self_decl, "->"), id);
}

/* Compute a hash value for a given method SEL_NAME.  */

static size_t
hash_func (tree sel_name)
{
  const unsigned char *s
    = (const unsigned char *)IDENTIFIER_POINTER (sel_name);
  size_t h = 0;

  while (*s)
    h = h * 67 + *s++ - 113;
  return h;
}

static void
hash_init (void)
{
  nst_method_hash_list
    = (hash *) ggc_alloc_cleared (SIZEHASHTABLE * sizeof (hash));
  cls_method_hash_list
    = (hash *) ggc_alloc_cleared (SIZEHASHTABLE * sizeof (hash));

  /* Initialize the hash table used to hold the constant string objects.  */
  string_htab = htab_create_ggc (31, string_hash,
				   string_eq, NULL);
}

/* WARNING!!!!  hash_enter is called with a method, and will peek
   inside to find its selector!  But hash_lookup is given a selector
   directly, and looks for the selector that's inside the found
   entry's key (method) for comparison.  */

static void
hash_enter (hash *hashlist, tree method)
{
  hash obj;
  int slot = hash_func (METHOD_SEL_NAME (method)) % SIZEHASHTABLE;

  obj = (hash) ggc_alloc (sizeof (struct hashed_entry));
  obj->list = 0;
  obj->next = hashlist[slot];
  obj->key = method;

  hashlist[slot] = obj;		/* append to front */
}

static hash
hash_lookup (hash *hashlist, tree sel_name)
{
  hash target;

  target = hashlist[hash_func (sel_name) % SIZEHASHTABLE];

  while (target)
    {
      if (sel_name == METHOD_SEL_NAME (target->key))
	return target;

      target = target->next;
    }
  return 0;
}

static void
hash_add_attr (hash entry, tree value)
{
  attr obj;

  obj = (attr) ggc_alloc (sizeof (struct hashed_attribute));
  obj->next = entry->list;
  obj->value = value;

  entry->list = obj;		/* append to front */
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

      mchain = TREE_CHAIN (mchain);
    }
  return NULL_TREE;
}

static tree
lookup_method_static (tree interface, tree ident, int is_class)
{
  tree meth = NULL_TREE, root_inter = NULL_TREE;
  tree inter = interface;

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

      /* Failing that, climb up the inheritance hierarchy.  */
      root_inter = inter;
      inter = lookup_interface (CLASS_SUPER_NAME (inter));
    }
  while (inter);

  /* If no class (factory) method was found, check if an _instance_
     method of the same name exists in the root class.  This is what
     the Objective-C runtime will do.  If an instance method was not
     found, return 0.  */
  return is_class ? lookup_method_static (root_inter, ident, 0): NULL_TREE;
}

/* Add the method to the hash list if it doesn't contain an identical
   method already. */
static void
add_method_to_hash_list (hash *hash_list, tree method)
{
  hash hsh;

  if (!(hsh = hash_lookup (hash_list, METHOD_SEL_NAME (method))))
    {
      /* Install on a global chain.  */
      hash_enter (hash_list, method);
    }
  else
    {
      /* Check types against those; if different, add to a list.  */
      attr loop;
      int already_there = comp_proto_with_proto (method, hsh->key);
      for (loop = hsh->list; !already_there && loop; loop = loop->next)
	already_there |= comp_proto_with_proto (method, loop->value);
      if (!already_there)
	hash_add_attr (hsh, method);
    }
}

static tree
objc_add_method (tree class, tree method, int is_class)
{
  tree mth;

  if (!(mth = lookup_method (is_class
			     ? CLASS_CLS_METHODS (class)
			     : CLASS_NST_METHODS (class), method)))
    {
      /* put method on list in reverse order */
      if (is_class)
	{
	  TREE_CHAIN (method) = CLASS_CLS_METHODS (class);
	  CLASS_CLS_METHODS (class) = method;
	}
      else
	{
	  TREE_CHAIN (method) = CLASS_NST_METHODS (class);
	  CLASS_NST_METHODS (class) = method;
	}
    }
  else
    {
      /* When processing an @interface for a class or category, give hard
	 errors on methods with identical selectors but differing argument
	 and/or return types. We do not do this for @implementations, because
	 C/C++ will do it for us (i.e., there will be duplicate function
	 definition errors).  */
      if ((TREE_CODE (class) == CLASS_INTERFACE_TYPE
	   || TREE_CODE (class) == CATEGORY_INTERFACE_TYPE)
	  && !comp_proto_with_proto (method, mth))
	error ("duplicate declaration of method %<%c%s%>",
		is_class ? '+' : '-', 
		IDENTIFIER_POINTER (METHOD_SEL_NAME (mth)));
    }

  if (is_class)
    add_method_to_hash_list (cls_method_hash_list, method);
  else
    {
      add_method_to_hash_list (nst_method_hash_list, method);

      /* Instance methods in root classes (and categories thereof)
	 may act as class methods as a last resort.  We also add
	 instance methods listed in @protocol declarations to
	 the class hash table, on the assumption that @protocols
	 may be adopted by root classes or categories.  */
      if (TREE_CODE (class) == CATEGORY_INTERFACE_TYPE
	  || TREE_CODE (class) == CATEGORY_IMPLEMENTATION_TYPE)
	class = lookup_interface (CLASS_NAME (class));

      if (TREE_CODE (class) == PROTOCOL_INTERFACE_TYPE
	  || !CLASS_SUPER_NAME (class))
	add_method_to_hash_list (cls_method_hash_list, method);
    }

  return method;
}

static tree
add_class (tree class)
{
  /* Put interfaces on list in reverse order.  */
  TREE_CHAIN (class) = interface_chain;
  interface_chain = class;
  return interface_chain;
}

static void
add_category (tree class, tree category)
{
  /* Put categories on list in reverse order.  */
  tree cat = lookup_category (class, CLASS_SUPER_NAME (category));

  if (cat)
    {
      warning ("duplicate interface declaration for category %<%s(%s)%>",
	       IDENTIFIER_POINTER (CLASS_NAME (class)),
	       IDENTIFIER_POINTER (CLASS_SUPER_NAME (category)));
    }
  else
    {
      CLASS_CATEGORY_LIST (category) = CLASS_CATEGORY_LIST (class);
      CLASS_CATEGORY_LIST (class) = category;
    }
}

/* Called after parsing each instance variable declaration. Necessary to
   preserve typedefs and implement public/private...

   PUBLIC is 1 for public, 0 for protected, and 2 for private.  */

static tree
add_instance_variable (tree class, int public, tree field_decl)
{
  tree field_type = TREE_TYPE (field_decl);
  const char *ivar_name = DECL_NAME (field_decl)
			  ? IDENTIFIER_POINTER (DECL_NAME (field_decl))
			  : "<unnamed>";

#ifdef OBJCPLUS
  if (TREE_CODE (field_type) == REFERENCE_TYPE)
    {
      error ("illegal reference type specified for instance variable %qs",
	     ivar_name);
      /* Return class as is without adding this ivar.  */
      return class;
    }
#endif

  if (field_type == error_mark_node || !TYPE_SIZE (field_type)
      || TYPE_SIZE (field_type) == error_mark_node)
      /* 'type[0]' is allowed, but 'type[]' is not! */
    {
      error ("instance variable %qs has unknown size", ivar_name);
      /* Return class as is without adding this ivar.  */
      return class;
    }

#ifdef OBJCPLUS
  /* zlaski 2001-Apr-24: C++ classes with non-trivial constructors and/or destructors
     cannot be ivars; ditto for classes with vtables. */
  if(IS_AGGR_TYPE (field_type) && (TYPE_NEEDS_CONSTRUCTING (field_type)
      || TYPE_HAS_NONTRIVIAL_DESTRUCTOR (field_type) || TYPE_POLYMORPHIC_P (field_type)))
    {
      const char *type_name = IDENTIFIER_POINTER (OBJC_TYPE_NAME (field_type));
      if(TYPE_POLYMORPHIC_P (field_type)) {
        /* vtable pointers are Real Bad(tm), since Obj-C cannot initialize them */
        error ("type %qs has virtual member functions", type_name);
        error ("illegal aggregate type %qs specified for instance variable %qs",
  	       type_name, ivar_name);
        /* Return class as is without adding this ivar.  */
        return class;
      }
      /* user-defined constructors and destructors are not known to Obj-C and
         hence will not be called.  This may or may not be a problem. */
      if (TYPE_NEEDS_CONSTRUCTING (field_type))
        warning ("type %qs has a user-defined constructor", type_name);
      if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (field_type))
        warning ("type %qs has a user-defined destructor", type_name);
      warning ("C++ constructors and destructors will not be invoked for Objective-C fields");
    }
#endif

  /* Overload the public attribute, it is not used for FIELD_DECLs.  */
  switch (public)
    {
    case 0:
      TREE_PUBLIC (field_decl) = 0;
      TREE_PRIVATE (field_decl) = 0;
      TREE_PROTECTED (field_decl) = 1;
      break;

    case 1:
      TREE_PUBLIC (field_decl) = 1;
      TREE_PRIVATE (field_decl) = 0;
      TREE_PROTECTED (field_decl) = 0;
      break;

    case 2:
      TREE_PUBLIC (field_decl) = 0;
      TREE_PRIVATE (field_decl) = 1;
      TREE_PROTECTED (field_decl) = 0;
      break;

    }

  CLASS_RAW_IVARS (class) = chainon (CLASS_RAW_IVARS (class), field_decl);

  return class;
}

static tree
is_ivar (tree decl_chain, tree ident)
{
  for ( ; decl_chain; decl_chain = TREE_CHAIN (decl_chain))
    if (DECL_NAME (decl_chain) == ident)
      return decl_chain;
  return NULL_TREE;
}

/* True if the ivar is private and we are not in its implementation.  */

static int
is_private (tree decl)
{
  return (TREE_PRIVATE (decl)
	  && ! is_ivar (CLASS_IVARS (implementation_template),
			DECL_NAME (decl)));
}

/* We have an instance variable reference;, check to see if it is public.  */

int
objc_is_public (tree expr, tree identifier)
{
  tree basetype = TREE_TYPE (expr);
  enum tree_code code = TREE_CODE (basetype);
  tree decl;

  if (code == RECORD_TYPE)
    {
      if (TYPE_HAS_OBJC_INFO (basetype) && TYPE_OBJC_INTERFACE (basetype))
	{
	  if (TREE_CODE (TYPE_OBJC_INTERFACE (basetype)) == IDENTIFIER_NODE)
	    {
	      error ("cannot find interface declaration for %qs",
		     IDENTIFIER_POINTER (OBJC_TYPE_NAME (basetype)));
	      return 0;
	    }

	  if ((decl = is_ivar (TYPE_FIELDS (basetype), identifier)))
	    {
	      if (TREE_PUBLIC (decl))
		return 1;

	      /* Important difference between the Stepstone translator:
		 all instance variables should be public within the context
		 of the implementation.  */
	      if (objc_implementation_context
		  && (((TREE_CODE (objc_implementation_context)
			== CLASS_IMPLEMENTATION_TYPE)
		       || (TREE_CODE (objc_implementation_context)
			   == CATEGORY_IMPLEMENTATION_TYPE))
		      && (CLASS_NAME (objc_implementation_context)
			  == OBJC_TYPE_NAME (basetype))))
		{
		  int private = is_private (decl);

		  if (private)
		    error ("instance variable %qs is declared private",
			   IDENTIFIER_POINTER (DECL_NAME (decl)));
		  return !private;
		}

	      /* The 2.95.2 compiler sometimes allowed C functions to access
		 non-@public ivars.  We will let this slide for now...  */
	      if (!objc_method_context)
	      {
		warning ("instance variable %qs is %s; "
			 "this will be a hard error in the future",
			 IDENTIFIER_POINTER (identifier),
			 TREE_PRIVATE (decl) ? "@private" : "@protected");
		return 1;
	      }
		
	      error ("instance variable %qs is declared %s",
		     IDENTIFIER_POINTER (identifier),
		     TREE_PRIVATE (decl) ? "private" : "protected");
	      return 0;
	    }
	}

      else if (objc_implementation_context && (basetype == objc_object_reference))
	{
	  expr = convert (uprivate_record, expr);
	  warning ("static access to object of type %<id%>");
	}
    }

  return 1;
}

/* Make sure all entries in CHAIN are also in LIST.  */

static int
check_methods (tree chain, tree list, int mtype)
{
  int first = 1;

  while (chain)
    {
      if (!lookup_method (list, chain))
	{
	  if (first)
	    {
	      if (TREE_CODE (objc_implementation_context)
		  == CLASS_IMPLEMENTATION_TYPE)
		warning ("incomplete implementation of class %qs",
			 IDENTIFIER_POINTER (CLASS_NAME (objc_implementation_context)));
	      else if (TREE_CODE (objc_implementation_context)
		       == CATEGORY_IMPLEMENTATION_TYPE)
		warning ("incomplete implementation of category %qs",
			 IDENTIFIER_POINTER (CLASS_SUPER_NAME (objc_implementation_context)));
	      first = 0;
	    }

	  warning ("method definition for %<%c%s%> not found",
		   mtype, IDENTIFIER_POINTER (METHOD_SEL_NAME (chain)));
	}

      chain = TREE_CHAIN (chain);
    }

    return first;
}

/* Check if CLASS, or its superclasses, explicitly conforms to PROTOCOL.  */

static int
conforms_to_protocol (tree class, tree protocol)
{
   if (TREE_CODE (protocol) == PROTOCOL_INTERFACE_TYPE)
     {
       tree p = CLASS_PROTOCOL_LIST (class);
       while (p && TREE_VALUE (p) != protocol)
	 p = TREE_CHAIN (p);

       if (!p)
	 {
	   tree super = (CLASS_SUPER_NAME (class)
			 ? lookup_interface (CLASS_SUPER_NAME (class))
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
      context = base_context;
      while (context)
	{
	  if (mtype == '+')
	    list = CLASS_CLS_METHODS (context);
	  else
	    list = CLASS_NST_METHODS (context);

	  if (lookup_method (list, chain))
	      break;

	  else if (TREE_CODE (context) == CLASS_IMPLEMENTATION_TYPE
		   || TREE_CODE (context) == CLASS_INTERFACE_TYPE)
	    context = (CLASS_SUPER_NAME (context)
		       ? lookup_interface (CLASS_SUPER_NAME (context))
		       : NULL_TREE);

	  else if (TREE_CODE (context) == CATEGORY_IMPLEMENTATION_TYPE
		   || TREE_CODE (context) == CATEGORY_INTERFACE_TYPE)
	    context = (CLASS_NAME (context)
		       ? lookup_interface (CLASS_NAME (context))
		       : NULL_TREE);
	  else
	    abort ();
	}

      if (context == NULL_TREE)
	{
	  if (first)
	    {
	      if (TREE_CODE (objc_implementation_context)
		  == CLASS_IMPLEMENTATION_TYPE)
		warning ("incomplete implementation of class %qs",
			 IDENTIFIER_POINTER
			   (CLASS_NAME (objc_implementation_context)));
	      else if (TREE_CODE (objc_implementation_context)
		       == CATEGORY_IMPLEMENTATION_TYPE)
		warning ("incomplete implementation of category %qs",
			 IDENTIFIER_POINTER
			   (CLASS_SUPER_NAME (objc_implementation_context)));
	      first = 0;
	    }
	  warning ("method definition for %<%c%s%> not found",
		   mtype, IDENTIFIER_POINTER (METHOD_SEL_NAME (chain)));
	}

      chain = TREE_CHAIN (chain); /* next method...  */
    }
  return first;
}

/* Check whether the current interface (accessible via
   'objc_implementation_context') actually implements protocol P, along
   with any protocols that P inherits.  */

static void
check_protocol (tree p, const char *type, const char *name)
{
  if (TREE_CODE (p) == PROTOCOL_INTERFACE_TYPE)
    {
      int f1, f2;

      /* Ensure that all protocols have bodies!  */
      if (warn_protocol)
	{
	  f1 = check_methods (PROTOCOL_CLS_METHODS (p),
			      CLASS_CLS_METHODS (objc_implementation_context),
			      '+');
	  f2 = check_methods (PROTOCOL_NST_METHODS (p),
			      CLASS_NST_METHODS (objc_implementation_context),
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
	warning ("%s %qs does not fully implement the %qs protocol",
		 type, name, IDENTIFIER_POINTER (PROTOCOL_NAME (p)));
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
check_protocols (tree proto_list, const char *type, const char *name)
{
  for ( ; proto_list; proto_list = TREE_CHAIN (proto_list))
    {
      tree p = TREE_VALUE (proto_list);

      check_protocol (p, type, name);
    }
}

/* Make sure that the class CLASS_NAME is defined
   CODE says which kind of thing CLASS_NAME ought to be.
   It can be CLASS_INTERFACE_TYPE, CLASS_IMPLEMENTATION_TYPE,
   CATEGORY_INTERFACE_TYPE, or CATEGORY_IMPLEMENTATION_TYPE.  */

static tree
start_class (enum tree_code code, tree class_name, tree super_name,
	     tree protocol_list)
{
  tree class, decl;

#ifdef OBJCPLUS
  if (current_namespace != global_namespace) {
    error ("Objective-C declarations may only appear in global scope");
  }
#endif /* OBJCPLUS */

  if (objc_implementation_context)
    {
      warning ("%<@end%> missing in implementation context");
      finish_class (objc_implementation_context);
      objc_ivar_chain = NULL_TREE;
      objc_implementation_context = NULL_TREE;
    }

  class = make_node (code);
  TYPE_LANG_SLOT_1 (class) = make_tree_vec (CLASS_LANG_SLOT_ELTS);

  /* Check for existence of the super class, if one was specified.  */
  if ((code == CLASS_INTERFACE_TYPE || code == CLASS_IMPLEMENTATION_TYPE)
      && super_name && !objc_is_class_name (super_name))
    {
      error ("cannot find interface declaration for %qs, superclass of %qs",
	     IDENTIFIER_POINTER (super_name),
	     IDENTIFIER_POINTER (class_name));
      super_name = NULL_TREE;
    }

  CLASS_NAME (class) = class_name;
  CLASS_SUPER_NAME (class) = super_name;
  CLASS_CLS_METHODS (class) = NULL_TREE;

  if (! objc_is_class_name (class_name)
      && (decl = lookup_name (class_name)))
    {
      error ("%qs redeclared as different kind of symbol",
	     IDENTIFIER_POINTER (class_name));
      error ("%Jprevious declaration of '%D'",
	     decl, decl);
    }

  if (code == CLASS_IMPLEMENTATION_TYPE)
    {
      {
        tree chain;

        for (chain = implemented_classes; chain; chain = TREE_CHAIN (chain))
           if (TREE_VALUE (chain) == class_name)
	     {
	       error ("reimplementation of class %qs",
		      IDENTIFIER_POINTER (class_name));
	       return error_mark_node;
	     }
        implemented_classes = tree_cons (NULL_TREE, class_name,
					 implemented_classes);
      }

      /* Reset for multiple classes per file.  */
      method_slot = 0;

      objc_implementation_context = class;

      /* Lookup the interface for this implementation.  */

      if (!(implementation_template = lookup_interface (class_name)))
        {
	  warning ("cannot find interface declaration for %qs",
		   IDENTIFIER_POINTER (class_name));
	  add_class (implementation_template = objc_implementation_context);
        }

      /* If a super class has been specified in the implementation,
	 insure it conforms to the one specified in the interface.  */

      if (super_name
	  && (super_name != CLASS_SUPER_NAME (implementation_template)))
        {
	  tree previous_name = CLASS_SUPER_NAME (implementation_template);
          const char *const name =
	    previous_name ? IDENTIFIER_POINTER (previous_name) : "";
	  error ("conflicting super class name %qs",
		 IDENTIFIER_POINTER (super_name));
	  error ("previous declaration of %qs", name);
        }

      else if (! super_name)
	{
	  CLASS_SUPER_NAME (objc_implementation_context)
	    = CLASS_SUPER_NAME (implementation_template);
	}
    }

  else if (code == CLASS_INTERFACE_TYPE)
    {
      if (lookup_interface (class_name))
#ifdef OBJCPLUS
	error ("duplicate interface declaration for class %qs",
#else
	warning ("duplicate interface declaration for class %qs",
#endif	
        IDENTIFIER_POINTER (class_name));
      else
        add_class (class);

      if (protocol_list)
	CLASS_PROTOCOL_LIST (class)
	  = lookup_and_install_protocols (protocol_list);
    }

  else if (code == CATEGORY_INTERFACE_TYPE)
    {
      tree class_category_is_assoc_with;

      /* For a category, class_name is really the name of the class that
	 the following set of methods will be associated with. We must
	 find the interface so that can derive the objects template.  */

      if (!(class_category_is_assoc_with = lookup_interface (class_name)))
	{
	  error ("cannot find interface declaration for %qs",
		 IDENTIFIER_POINTER (class_name));
	  exit (FATAL_EXIT_CODE);
	}
      else
        add_category (class_category_is_assoc_with, class);

      if (protocol_list)
	CLASS_PROTOCOL_LIST (class)
	  = lookup_and_install_protocols (protocol_list);
    }

  else if (code == CATEGORY_IMPLEMENTATION_TYPE)
    {
      /* Reset for multiple classes per file.  */
      method_slot = 0;

      objc_implementation_context = class;

      /* For a category, class_name is really the name of the class that
	 the following set of methods will be associated with.  We must
	 find the interface so that can derive the objects template.  */

      if (!(implementation_template = lookup_interface (class_name)))
        {
	  error ("cannot find interface declaration for %qs",
		 IDENTIFIER_POINTER (class_name));
	  exit (FATAL_EXIT_CODE);
        }
    }
  return class;
}

static tree
continue_class (tree class)
{
  if (TREE_CODE (class) == CLASS_IMPLEMENTATION_TYPE
      || TREE_CODE (class) == CATEGORY_IMPLEMENTATION_TYPE)
    {
      struct imp_entry *imp_entry;
      tree ivar_context;

      /* Check consistency of the instance variables.  */

      if (CLASS_RAW_IVARS (class))
	check_ivars (implementation_template, class);

      /* code generation */

#ifdef OBJCPLUS
      push_lang_context (lang_name_c);
#endif

      build_private_template (implementation_template);
      uprivate_record = CLASS_STATIC_TEMPLATE (implementation_template);
      ivar_context = TYPE_FIELDS (uprivate_record);
      objc_instance_type = build_pointer_type (uprivate_record);

      imp_entry = (struct imp_entry *) ggc_alloc (sizeof (struct imp_entry));

      imp_entry->next = imp_list;
      imp_entry->imp_context = class;
      imp_entry->imp_template = implementation_template;

      synth_forward_declarations ();
      imp_entry->class_decl = UOBJC_CLASS_decl;
      imp_entry->meta_decl = UOBJC_METACLASS_decl;

      /* Append to front and increment count.  */
      imp_list = imp_entry;
      if (TREE_CODE (class) == CLASS_IMPLEMENTATION_TYPE)
	imp_count++;
      else
	cat_count++;

#ifdef OBJCPLUS
      pop_lang_context ();
#endif /* OBJCPLUS */

      return ivar_context;
    }

  else if (TREE_CODE (class) == CLASS_INTERFACE_TYPE)
    {
#ifdef OBJCPLUS
      push_lang_context (lang_name_c);
#endif /* OBJCPLUS */

      build_private_template (class);

#ifdef OBJCPLUS
      pop_lang_context ();
#endif /* OBJCPLUS */

      return NULL_TREE;
    }

  else
    return error_mark_node;
}

/* This is called once we see the "@end" in an interface/implementation.  */

static void
finish_class (tree class)
{
  if (TREE_CODE (class) == CLASS_IMPLEMENTATION_TYPE)
    {
      /* All code generation is done in finish_objc.  */

      if (implementation_template != objc_implementation_context)
	{
	  /* Ensure that all method listed in the interface contain bodies.  */
	  check_methods (CLASS_CLS_METHODS (implementation_template),
			 CLASS_CLS_METHODS (objc_implementation_context), '+');
	  check_methods (CLASS_NST_METHODS (implementation_template),
			 CLASS_NST_METHODS (objc_implementation_context), '-');

	  if (CLASS_PROTOCOL_LIST (implementation_template))
	    check_protocols (CLASS_PROTOCOL_LIST (implementation_template),
			     "class",
			     IDENTIFIER_POINTER (CLASS_NAME (objc_implementation_context)));
	}
    }

  else if (TREE_CODE (class) == CATEGORY_IMPLEMENTATION_TYPE)
    {
      tree category = lookup_category (implementation_template, CLASS_SUPER_NAME (class));

      if (category)
	{
	  /* Ensure all method listed in the interface contain bodies.  */
	  check_methods (CLASS_CLS_METHODS (category),
			 CLASS_CLS_METHODS (objc_implementation_context), '+');
	  check_methods (CLASS_NST_METHODS (category),
			 CLASS_NST_METHODS (objc_implementation_context), '-');

	  if (CLASS_PROTOCOL_LIST (category))
	    check_protocols (CLASS_PROTOCOL_LIST (category),
			     "category",
			     IDENTIFIER_POINTER (CLASS_SUPER_NAME (objc_implementation_context)));
	}
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

static tree
lookup_protocol (tree ident)
{
  tree chain;

  for (chain = protocol_chain; chain; chain = TREE_CHAIN (chain))
    if (ident == PROTOCOL_NAME (chain))
      return chain;

  return NULL_TREE;
}

/* This function forward declares the protocols named by NAMES.  If
   they are already declared or defined, the function has no effect.  */

void
objc_declare_protocols (tree names)
{
  tree list;

#ifdef OBJCPLUS
  if (current_namespace != global_namespace) {
    error ("Objective-C declarations may only appear in global scope");
  }
#endif /* OBJCPLUS */

  for (list = names; list; list = TREE_CHAIN (list))
    {
      tree name = TREE_VALUE (list);

      if (lookup_protocol (name) == NULL_TREE)
	{
	  tree protocol = make_node (PROTOCOL_INTERFACE_TYPE);

	  TYPE_LANG_SLOT_1 (protocol)
	    = make_tree_vec (PROTOCOL_LANG_SLOT_ELTS);
	  PROTOCOL_NAME (protocol) = name;
	  PROTOCOL_LIST (protocol) = NULL_TREE;
	  add_protocol (protocol);
	  PROTOCOL_DEFINED (protocol) = 0;
	  PROTOCOL_FORWARD_DECL (protocol) = NULL_TREE;
	}
    }
}

static tree
start_protocol (enum tree_code code, tree name, tree list)
{
  tree protocol;

#ifdef OBJCPLUS
  if (current_namespace != global_namespace) {
    error ("Objective-C declarations may only appear in global scope");
  }
#endif /* OBJCPLUS */

  protocol = lookup_protocol (name);

  if (!protocol)
    {
      protocol = make_node (code);
      TYPE_LANG_SLOT_1 (protocol) = make_tree_vec (PROTOCOL_LANG_SLOT_ELTS);

      PROTOCOL_NAME (protocol) = name;
      PROTOCOL_LIST (protocol) = lookup_and_install_protocols (list);
      add_protocol (protocol);
      PROTOCOL_DEFINED (protocol) = 1;
      PROTOCOL_FORWARD_DECL (protocol) = NULL_TREE;

      check_protocol_recursively (protocol, list);
    }
  else if (! PROTOCOL_DEFINED (protocol))
    {
      PROTOCOL_DEFINED (protocol) = 1;
      PROTOCOL_LIST (protocol) = lookup_and_install_protocols (list);

      check_protocol_recursively (protocol, list);
    }
  else
    {
      warning ("duplicate declaration for protocol %qs",
	       IDENTIFIER_POINTER (name));
    }
  return protocol;
}


/* "Encode" a data type into a string, which grows in util_obstack.
   ??? What is the FORMAT?  Someone please document this!  */

static void
encode_type_qualifiers (tree declspecs)
{
  tree spec;

  for (spec = declspecs; spec; spec = TREE_CHAIN (spec))
    {
      if (ridpointers[(int) RID_IN] == TREE_VALUE (spec))
	obstack_1grow (&util_obstack, 'n');
      else if (ridpointers[(int) RID_INOUT] == TREE_VALUE (spec))
	obstack_1grow (&util_obstack, 'N');
      else if (ridpointers[(int) RID_OUT] == TREE_VALUE (spec))
	obstack_1grow (&util_obstack, 'o');
      else if (ridpointers[(int) RID_BYCOPY] == TREE_VALUE (spec))
	obstack_1grow (&util_obstack, 'O');
      else if (ridpointers[(int) RID_BYREF] == TREE_VALUE (spec))
        obstack_1grow (&util_obstack, 'R');
      else if (ridpointers[(int) RID_ONEWAY] == TREE_VALUE (spec))
	obstack_1grow (&util_obstack, 'V');
    }
}

/* Encode a pointer type.  */

static void
encode_pointer (tree type, int curtype, int format)
{
  tree pointer_to = TREE_TYPE (type);

  if (TREE_CODE (pointer_to) == RECORD_TYPE)
    {
      if (OBJC_TYPE_NAME (pointer_to)
	  && TREE_CODE (OBJC_TYPE_NAME (pointer_to)) == IDENTIFIER_NODE)
	{
	  const char *name = IDENTIFIER_POINTER (OBJC_TYPE_NAME (pointer_to));

	  if (strcmp (name, TAG_OBJECT) == 0) /* '@' */
	    {
	      obstack_1grow (&util_obstack, '@');
	      return;
	    }
	  else if (TYPE_HAS_OBJC_INFO (pointer_to)
		   && TYPE_OBJC_INTERFACE (pointer_to))
	    {
              if (generating_instance_variables)
	        {
	          obstack_1grow (&util_obstack, '@');
	          obstack_1grow (&util_obstack, '"');
	          obstack_grow (&util_obstack, name, strlen (name));
	          obstack_1grow (&util_obstack, '"');
	          return;
		}
              else
	        {
	          obstack_1grow (&util_obstack, '@');
	          return;
		}
	    }
	  else if (strcmp (name, TAG_CLASS) == 0) /* '#' */
	    {
	      obstack_1grow (&util_obstack, '#');
	      return;
	    }
	  else if (strcmp (name, TAG_SELECTOR) == 0) /* ':' */
	    {
	      obstack_1grow (&util_obstack, ':');
	      return;
	    }
	}
    }
  else if (TREE_CODE (pointer_to) == INTEGER_TYPE
	   && TYPE_MODE (pointer_to) == QImode)
    {
      tree pname = TREE_CODE (OBJC_TYPE_NAME (pointer_to)) == IDENTIFIER_NODE
	          ? OBJC_TYPE_NAME (pointer_to)
	          : DECL_NAME (OBJC_TYPE_NAME (pointer_to));

      if (!flag_next_runtime || strcmp (IDENTIFIER_POINTER (pname), "BOOL"))
	{
	  /* It appears that "r*" means "const char *" rather than
	     "char *const".  */
	  if (TYPE_READONLY (pointer_to))
	    obstack_1grow (&util_obstack, 'r');

	  obstack_1grow (&util_obstack, '*');
	  return;
	}
    }

  /* We have a type that does not get special treatment.  */

  /* NeXT extension */
  obstack_1grow (&util_obstack, '^');
  encode_type (pointer_to, curtype, format);
}

static void
encode_array (tree type, int curtype, int format)
{
  tree an_int_cst = TYPE_SIZE (type);
  tree array_of = TREE_TYPE (type);
  char buffer[40];

  /* An incomplete array is treated like a pointer.  */
  if (an_int_cst == NULL)
    {
      encode_pointer (type, curtype, format);
      return;
    }

  sprintf (buffer, "[" HOST_WIDE_INT_PRINT_DEC,
		   (TREE_INT_CST_LOW (an_int_cst)
		    / TREE_INT_CST_LOW (TYPE_SIZE (array_of))));

  obstack_grow (&util_obstack, buffer, strlen (buffer));
  encode_type (array_of, curtype, format);
  obstack_1grow (&util_obstack, ']');
  return;
}

static void
encode_aggregate_within (tree type, int curtype, int format, int left,
			 int right)
{
  tree name;
  /* NB: aggregates that are pointed to have slightly different encoding
     rules in that you never encode the names of instance variables.  */
  int pointed_to
   = (obstack_object_size (&util_obstack) > 0
      && *(obstack_next_free (&util_obstack) - 1) == '^');
  int inline_contents
   = ((format == OBJC_ENCODE_INLINE_DEFS || generating_instance_variables)
      && (!pointed_to || obstack_object_size (&util_obstack) - curtype == 1));

  /* Traverse struct aliases; it is important to get the
     original struct and its tag name (if any).  */
  type = TYPE_MAIN_VARIANT (type);
  name = OBJC_TYPE_NAME (type);
  /* Open parenth/bracket.  */  			
  obstack_1grow (&util_obstack, left);

  /* Encode the struct/union tag name, or '?' if a tag was
     not provided.  Typedef aliases do not qualify.  */
  if (name && TREE_CODE (name) == IDENTIFIER_NODE
#ifdef OBJCPLUS
      /* Did this struct have a tag?  */
      && !TYPE_WAS_ANONYMOUS (type)
#endif
      )
    obstack_grow (&util_obstack,
		  IDENTIFIER_POINTER (name),
		  strlen (IDENTIFIER_POINTER (name)));
  else
    obstack_1grow (&util_obstack, '?');

  /* Encode the types (and possibly names) of the inner fields,
     if required.  */
  if (inline_contents)
    {
      tree fields = TYPE_FIELDS (type);

      obstack_1grow (&util_obstack, '=');
      for (; fields; fields = TREE_CHAIN (fields))
	{
#ifdef OBJCPLUS
	  /* C++ static members, and things that are not fields at all,
	     should not appear in the encoding.  */
	  if (TREE_CODE (fields) != FIELD_DECL || TREE_STATIC (fields))
	    continue;
#endif
	  if (generating_instance_variables && !pointed_to)
	    {
	      tree fname = DECL_NAME (fields);

	      obstack_1grow (&util_obstack, '"');
	      if (fname && TREE_CODE (fname) == IDENTIFIER_NODE)
		obstack_grow (&util_obstack,
			      IDENTIFIER_POINTER (fname),
			      strlen (IDENTIFIER_POINTER (fname)));
	      obstack_1grow (&util_obstack, '"');
	    }
	  encode_field_decl (fields, curtype, format);
	}
    }
  /* Close parenth/bracket.  */  			
  obstack_1grow (&util_obstack, right);
}

static void
encode_aggregate (tree type, int curtype, int format)
{
  enum tree_code code = TREE_CODE (type);

  switch (code)
    {
    case RECORD_TYPE:
      {
	encode_aggregate_within (type, curtype, format, '{', '}');
	break;
      }
    case UNION_TYPE:
      {
	encode_aggregate_within (type, curtype, format, '(', ')');
	break;
      }

    case ENUMERAL_TYPE:
      obstack_1grow (&util_obstack, 'i');
      break;

    default:
      break;
    }
}

/* Encode a bitfield NeXT-style (i.e., without a bit offset or the underlying
   field type.  */

static void
encode_next_bitfield (int width)
{
  char buffer[40];
  sprintf (buffer, "b%d", width);
  obstack_grow (&util_obstack, buffer, strlen (buffer));
}

/* FORMAT will be OBJC_ENCODE_INLINE_DEFS or OBJC_ENCODE_DONT_INLINE_DEFS.  */
static void
encode_type (tree type, int curtype, int format)
{
  enum tree_code code = TREE_CODE (type);
  char c;

  if (TYPE_READONLY (type))
    obstack_1grow (&util_obstack, 'r');

  if (code == INTEGER_TYPE)
    {
      switch (GET_MODE_BITSIZE (TYPE_MODE (type)))
	{
	case 8:  c = TYPE_UNSIGNED (type) ? 'C' : 'c'; break;
	case 16: c = TYPE_UNSIGNED (type) ? 'S' : 's'; break;
	case 32: 
	  if (type == long_unsigned_type_node
	      || type == long_integer_type_node)
	         c = TYPE_UNSIGNED (type) ? 'L' : 'l';
	  else
	         c = TYPE_UNSIGNED (type) ? 'I' : 'i';
	  break;
	case 64: c = TYPE_UNSIGNED (type) ? 'Q' : 'q'; break;
	default: abort ();
	}
      obstack_1grow (&util_obstack, c);
    }

  else if (code == REAL_TYPE)
    {
      /* Floating point types.  */
      switch (GET_MODE_BITSIZE (TYPE_MODE (type)))
	{
	case 32:  c = 'f'; break;
	case 64:
	case 96:
	case 128: c = 'd'; break;
	default: abort ();
	}
      obstack_1grow (&util_obstack, c);
    }

  else if (code == VOID_TYPE)
    obstack_1grow (&util_obstack, 'v');

  else if (code == BOOLEAN_TYPE)
    obstack_1grow (&util_obstack, 'B');

  else if (code == ARRAY_TYPE)
    encode_array (type, curtype, format);

  else if (code == POINTER_TYPE)
    encode_pointer (type, curtype, format);

  else if (code == RECORD_TYPE || code == UNION_TYPE || code == ENUMERAL_TYPE)
    encode_aggregate (type, curtype, format);

  else if (code == FUNCTION_TYPE) /* '?' */
    obstack_1grow (&util_obstack, '?');
}

static void
encode_gnu_bitfield (int position, tree type, int size)
{
  enum tree_code code = TREE_CODE (type);
  char buffer[40];
  char charType = '?';

  if (code == INTEGER_TYPE)
    {
      if (integer_zerop (TYPE_MIN_VALUE (type)))
	{
	  /* Unsigned integer types.  */

	  if (TYPE_MODE (type) == QImode)
	    charType = 'C';
	  else if (TYPE_MODE (type) == HImode)
	    charType = 'S';
	  else if (TYPE_MODE (type) == SImode)
	    {
	      if (type == long_unsigned_type_node)
		charType = 'L';
	      else
		charType = 'I';
	    }
	  else if (TYPE_MODE (type) == DImode)
	    charType = 'Q';
	}

      else
	/* Signed integer types.  */
	{
	  if (TYPE_MODE (type) == QImode)
	    charType = 'c';
	  else if (TYPE_MODE (type) == HImode)
	    charType = 's';
	  else if (TYPE_MODE (type) == SImode)
	    {
	      if (type == long_integer_type_node)
		charType = 'l';
	      else
		charType = 'i';
	    }

	  else if (TYPE_MODE (type) == DImode)
	    charType = 'q';
	}
    }
  else if (code == ENUMERAL_TYPE)
    charType = 'i';
  else
    abort ();

  sprintf (buffer, "b%d%c%d", position, charType, size);
  obstack_grow (&util_obstack, buffer, strlen (buffer));
}

static void
encode_field_decl (tree field_decl, int curtype, int format)
{
  tree type;

#ifdef OBJCPLUS
  /* C++ static members, and things that are not fields at all,
     should not appear in the encoding.  */
  if (TREE_CODE (field_decl) != FIELD_DECL || TREE_STATIC (field_decl))
    return;
#endif

  type = TREE_TYPE (field_decl);

  /* Generate the bitfield typing information, if needed.  Note the difference
     between GNU and NeXT runtimes.  */
  if (DECL_BIT_FIELD_TYPE (field_decl))
    {
      int size = tree_low_cst (DECL_SIZE (field_decl), 1);

      if (flag_next_runtime)
	encode_next_bitfield (size);
      else
	encode_gnu_bitfield (int_bit_position (field_decl),
				  DECL_BIT_FIELD_TYPE (field_decl), size);
    }
  else
    encode_type (TREE_TYPE (field_decl), curtype, format);
}

static GTY(()) tree objc_parmlist = NULL_TREE;

/* Append PARM to a list of formal parameters of a method, making a necessary
   array-to-pointer adjustment along the way.  */

static void
objc_push_parm (tree parm)
{
  /* Decay arrays and functions into pointers.  */
  if (TREE_CODE (TREE_TYPE (parm)) == ARRAY_TYPE)
    TREE_TYPE (parm) = build_pointer_type (TREE_TYPE (TREE_TYPE (parm)));
  else if (TREE_CODE (TREE_TYPE (parm)) == FUNCTION_TYPE)
    TREE_TYPE (parm) = build_pointer_type (TREE_TYPE (parm));

  DECL_ARG_TYPE_AS_WRITTEN (parm) = TREE_TYPE (parm);
  DECL_ARG_TYPE (parm)
    = lang_hooks.types.type_promotes_to (TREE_TYPE (parm));

  /* Record constancy and volatility.  */
  c_apply_type_quals_to_decl
  ((TYPE_READONLY (TREE_TYPE (parm)) ? TYPE_QUAL_CONST : 0)
   | (TYPE_RESTRICT (TREE_TYPE (parm)) ? TYPE_QUAL_RESTRICT : 0)
   | (TYPE_VOLATILE (TREE_TYPE (parm)) ? TYPE_QUAL_VOLATILE : 0), parm);
  
  objc_parmlist = chainon (objc_parmlist, parm);
}

/* Retrieve the formal parameter list constructed via preceding calls to
   objc_push_parm().  */

#ifdef OBJCPLUS
static tree
objc_get_parm_info (int have_ellipsis ATTRIBUTE_UNUSED)
#else
static struct c_arg_info *
objc_get_parm_info (int have_ellipsis)
#endif
{
#ifdef OBJCPLUS
  tree parm_info = objc_parmlist;
  objc_parmlist = NULL_TREE;

  return parm_info;
#else
  tree parm_info = objc_parmlist;
  struct c_arg_info *arg_info;
  /* The C front-end requires an elaborate song and dance at
     this point.  */
  push_scope ();
  declare_parm_level ();
  while (parm_info)
    {
      tree next = TREE_CHAIN (parm_info);

      TREE_CHAIN (parm_info) = NULL_TREE; 
      parm_info = pushdecl (parm_info);
      finish_decl (parm_info, NULL_TREE, NULL_TREE);
      parm_info = next;
    }
  arg_info = get_parm_info (have_ellipsis);
  pop_scope ();
  objc_parmlist = NULL_TREE;
  return arg_info;
#endif
}

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
  objc_push_parm (build_decl (PARM_DECL, self_id, self_type));

  /* SEL _cmd; */
  objc_push_parm (build_decl (PARM_DECL, ucmd_id, objc_selector_type));
}

/* Transform an Objective-C method definition into a static C function
   definition, synthesizing the first two arguments, "self" and "_cmd",
   in the process.  */

static void
start_method_def (tree method)
{
  tree parmlist;
#ifdef OBJCPLUS
  tree parm_info;
#else
  struct c_arg_info *parm_info;
#endif
  int have_ellipsis = 0;

  /* Required to implement _msgSuper.  */
  objc_method_context = method;
  UOBJC_SUPER_decl = NULL_TREE;

  /* Generate prototype declarations for arguments..."new-style".  */
  synth_self_and_ucmd_args ();

  /* Generate argument declarations if a keyword_decl.  */
  parmlist = METHOD_SEL_ARGS (method);
  while (parmlist)
    {
      tree type = TREE_VALUE (TREE_TYPE (parmlist)), parm;

      parm = build_decl (PARM_DECL, KEYWORD_ARG_NAME (parmlist), type);
      objc_push_parm (parm);
      parmlist = TREE_CHAIN (parmlist);
    }

  if (METHOD_ADD_ARGS (method))
    {
      tree akey;

      for (akey = TREE_CHAIN (METHOD_ADD_ARGS (method)); 
	   akey; akey = TREE_CHAIN (akey))
	{
	  objc_push_parm (TREE_VALUE (akey));
	}

      if (TREE_OVERFLOW (METHOD_ADD_ARGS (method)))
	have_ellipsis = 1;
    }

  parm_info = objc_get_parm_info (have_ellipsis);

  really_start_method (objc_method_context, parm_info);
}

static void
warn_with_method (const char *message, int mtype, tree method)
{
  /* Add a readable method name to the warning.  */
  warning ("%J%s %<%c%s%>", method,
           message, mtype, gen_method_decl (method));
}

/* Return 1 if TYPE1 is equivalent to TYPE2
   for purposes of method overloading.  */

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

  type1 = (TYPE_HAS_OBJC_INFO (type1)
	   ? TYPE_OBJC_PROTOCOL_LIST (type1)
	   : NULL_TREE);
  type2 = (TYPE_HAS_OBJC_INFO (type2)
	   ? TYPE_OBJC_PROTOCOL_LIST (type2)
	   : NULL_TREE);

  if (list_length (type1) == list_length (type2))
    {
      for (; type2; type2 = TREE_CHAIN (type2))
	if (!lookup_protocol_in_reflist (type1, TREE_VALUE (type2)))
	  return 0;
      return 1;
    }
  return 0;
}

/* Return 1 if PROTO1 is equivalent to PROTO2
   for purposes of method overloading.  */

static int
comp_proto_with_proto (tree proto1, tree proto2)
{
  tree type1, type2;

  /* The following test is needed in case there are hashing
     collisions.  */
  if (METHOD_SEL_NAME (proto1) != METHOD_SEL_NAME (proto2))
    return 0;

  /* Compare return types.  */
  type1 = TREE_VALUE (TREE_TYPE (proto1));
  type2 = TREE_VALUE (TREE_TYPE (proto2));

  if (!objc_types_are_equivalent (type1, type2))
    return 0;

  /* Compare argument types.  */
  for (type1 = get_arg_type_list (proto1, METHOD_REF, 0),
       type2 = get_arg_type_list (proto2, METHOD_REF, 0);
       type1 && type2;
       type1 = TREE_CHAIN (type1), type2 = TREE_CHAIN (type2))
    {
      if (!objc_types_are_equivalent (TREE_VALUE (type1), TREE_VALUE (type2)))
	return 0;
    }

  return (!type1 && !type2);
}

/* Fold an OBJ_TYPE_REF expression for ObjC method dispatches, where
   this occurs.  ObjC method dispatches are _not_ like C++ virtual
   member function dispatches, and we account for the difference here.  */
tree
#ifdef OBJCPLUS
objc_fold_obj_type_ref (tree ref, tree known_type)
#else
objc_fold_obj_type_ref (tree ref ATTRIBUTE_UNUSED,
			tree known_type ATTRIBUTE_UNUSED)
#endif
{
#ifdef OBJCPLUS
  tree v = BINFO_VIRTUALS (TYPE_BINFO (known_type));

  /* If the receiver does not have virtual member functions, there
     is nothing we can (or need to) do here.  */
  if (!v)
    return NULL_TREE;

  /* Let C++ handle C++ virtual functions.  */
  return cp_fold_obj_type_ref (ref, known_type);
#else
  /* For plain ObjC, we currently do not need to do anything.  */
  return NULL_TREE;
#endif
}

static void
objc_start_function (tree name, tree type, tree attrs,
#ifdef OBJCPLUS
		     tree params
#else
		     struct c_arg_info *params
#endif
		     )
{
  tree fndecl = build_decl (FUNCTION_DECL, name, type);

#ifdef OBJCPLUS
  DECL_ARGUMENTS (fndecl) = params;
  DECL_INITIAL (fndecl) = error_mark_node;
  DECL_EXTERNAL (fndecl) = 0;
  TREE_STATIC (fndecl) = 1;
  retrofit_lang_decl (fndecl);
  cplus_decl_attributes (&fndecl, attrs, 0);
  start_preparsed_function (fndecl, attrs, /*flags=*/SF_DEFAULT);
#else
  struct c_label_context_se *nstack_se;
  struct c_label_context_vm *nstack_vm;
  nstack_se = XOBNEW (&parser_obstack, struct c_label_context_se);
  nstack_se->labels_def = NULL;
  nstack_se->labels_used = NULL;
  nstack_se->next = label_context_stack_se;
  label_context_stack_se = nstack_se;
  nstack_vm = XOBNEW (&parser_obstack, struct c_label_context_vm);
  nstack_vm->labels_def = NULL;
  nstack_vm->labels_used = NULL;
  nstack_vm->scope = 0;
  nstack_vm->next = label_context_stack_vm;
  label_context_stack_vm = nstack_vm;
  decl_attributes (&fndecl, attrs, 0);
  announce_function (fndecl);
  DECL_INITIAL (fndecl) = error_mark_node;
  DECL_EXTERNAL (fndecl) = 0;
  TREE_STATIC (fndecl) = 1;
  current_function_decl = pushdecl (fndecl);
  push_scope ();
  declare_parm_level ();
  DECL_RESULT (current_function_decl)
    = build_decl (RESULT_DECL, NULL_TREE,
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

  meth_type
    = build_function_type (ret_type,
			   get_arg_type_list (method, METHOD_DEF, 0));
  objc_start_function (method_id, meth_type, NULL_TREE, parmlist);

  /* Set self_decl from the first argument.  */
  self_decl = DECL_ARGUMENTS (current_function_decl);

  /* Suppress unused warnings.  */
  TREE_USED (self_decl) = 1;
  TREE_USED (TREE_CHAIN (self_decl)) = 1;
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
				TREE_CODE (method) == CLASS_METHOD_DECL);

      if (proto)
	{
	  if (!comp_proto_with_proto (method, proto))
	    {
	      char type = (TREE_CODE (method) == INSTANCE_METHOD_DECL ? '-' : '+');

	      warn_with_method ("conflicting types for", type, method);
	      warn_with_method ("previous declaration of", type, proto);
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
			     TREE_CODE (method) == CLASS_METHOD_DECL);
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
      tree super_expr, super_expr_list;

      if (!UOBJC_SUPER_decl)
      {
	UOBJC_SUPER_decl = build_decl (VAR_DECL, get_identifier (TAG_SUPER),
				       objc_super_template);
	/* This prevents `unused variable' warnings when compiling with -Wall.  */
	TREE_USED (UOBJC_SUPER_decl) = 1;
	lang_hooks.decls.pushdecl (UOBJC_SUPER_decl);
        finish_decl (UOBJC_SUPER_decl, NULL_TREE, NULL_TREE);
	UOBJC_SUPER_scope = objc_get_current_scope ();
      }

      /* Set receiver to self.  */
      super_expr = build_component_ref (UOBJC_SUPER_decl, self_id);
      super_expr = build_modify_expr (super_expr, NOP_EXPR, self_decl);
      super_expr_list = super_expr;

      /* Set class to begin searching.  */
      super_expr = build_component_ref (UOBJC_SUPER_decl,
					get_identifier ("super_class"));

      if (TREE_CODE (objc_implementation_context) == CLASS_IMPLEMENTATION_TYPE)
	{
	  /* [_cls, __cls]Super are "pre-built" in
	     synth_forward_declarations.  */

	  super_expr = build_modify_expr (super_expr, NOP_EXPR,
					  ((TREE_CODE (objc_method_context)
					    == INSTANCE_METHOD_DECL)
					   ? ucls_super_ref
					   : uucls_super_ref));
	}

      else
	/* We have a category.  */
	{
	  tree super_name = CLASS_SUPER_NAME (implementation_template);
	  tree super_class;

	  /* Barf if super used in a category of Object.  */
	  if (!super_name)
	    {
	      error ("no super class declared in interface for %qs",
		    IDENTIFIER_POINTER (CLASS_NAME (implementation_template)));
	      return error_mark_node;
	    }

	  if (flag_next_runtime && !flag_zero_link)
	    {
	      super_class = objc_get_class_reference (super_name);
	      if (TREE_CODE (objc_method_context) == CLASS_METHOD_DECL)
		/* If we are in a class method, we must retrieve the
		   _metaclass_ for the current class, pointed at by
		   the class's "isa" pointer.  The following assumes that
		   "isa" is the first ivar in a class (which it must be).  */
		super_class
		  = build_indirect_ref
		    (build_c_cast (build_pointer_type (objc_class_type),
				   super_class), "unary *");
	    }
	  else
	    {
	      add_class_reference (super_name);
	      super_class = (TREE_CODE (objc_method_context) == INSTANCE_METHOD_DECL
			     ? objc_get_class_decl : objc_get_meta_class_decl);
	      assemble_external (super_class);
	      super_class
		= build_function_call
		  (super_class,
		   build_tree_list
		   (NULL_TREE,
		    my_build_string (IDENTIFIER_LENGTH (super_name) + 1,
				     IDENTIFIER_POINTER (super_name))));
	    }

	  super_expr
	    = build_modify_expr (super_expr, NOP_EXPR,
				 build_c_cast (TREE_TYPE (super_expr),
					       super_class));
	}

      super_expr_list = build_compound_expr (super_expr_list, super_expr);

      super_expr = build_unary_op (ADDR_EXPR, UOBJC_SUPER_decl, 0);
      super_expr_list = build_compound_expr (super_expr_list, super_expr);

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
      && UOBJC_SUPER_scope == objc_get_current_scope ()) {
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
  DECL_INLINE (fndecl) = 0;
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
}

#if 0
int
lang_report_error_function (tree decl)
{
  if (objc_method_context)
    {
      fprintf (stderr, "In method %qs\n",
	       IDENTIFIER_POINTER (METHOD_SEL_NAME (objc_method_context)));
      return 1;
    }

  else
    return 0;
}
#endif

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

  strcat (errbuf, IDENTIFIER_POINTER (type));

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
	  if ((chain = TREE_CHAIN (chain)))
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

	  if (TREE_OVERFLOW (METHOD_ADD_ARGS (method)))
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
static void
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

      if (TREE_CODE (chain) == CATEGORY_IMPLEMENTATION_TYPE
	  || TREE_CODE (chain) == CATEGORY_INTERFACE_TYPE)
	{
	  fprintf (fp, " (%s)\n", name);
	}
      else
	{
	  fprintf (fp, " : %s\n", name);
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

/* Demangle function for Objective-C */
static const char *
objc_demangle (const char *mangled)
{
  char *demangled, *cp;

  if (mangled[0] == '_' &&
      (mangled[1] == 'i' || mangled[1] == 'c') &&
      mangled[2] == '_')
    {
      cp = demangled = xmalloc(strlen(mangled) + 2);
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
	  return mangled;
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
	      return mangled;
	    }
	  *cp++ = ')';
	  *cp++ = ' ';            /* overwriting 1st char of method name... */
	  strcpy(cp, mangled + (cp - demangled)); /* get it back */
	}
      while (*cp && *cp == '_')
	cp++;                   /* skip any initial underbars in method name */
      for (; *cp; cp++)
	if (*cp == '_')
	  *cp = ':';            /* replace remaining '_' with ':' */
      *cp++ = ']';              /* closing right brace */
      *cp++ = 0;                /* string terminator */
      return demangled;
    }
  else
    return mangled;             /* not an objc mangled name */
}

const char *
objc_printable_name (tree decl, int kind ATTRIBUTE_UNUSED)
{
  return objc_demangle (IDENTIFIER_POINTER (DECL_NAME (decl)));
}

static void
init_objc (void)
{
  gcc_obstack_init (&util_obstack);
  util_firstobj = (char *) obstack_finish (&util_obstack);

  errbuf = (char *) xmalloc (1024 * 10);
  hash_init ();
  synth_module_prologue ();
}

static void
finish_objc (void)
{
  struct imp_entry *impent;
  tree chain;
  /* The internally generated initializers appear to have missing braces.
     Don't warn about this.  */
  int save_warn_missing_braces = warn_missing_braces;
  warn_missing_braces = 0;

  /* A missing @end may not be detected by the parser.  */
  if (objc_implementation_context)
    {
      warning ("%<@end%> missing in implementation context");
      finish_class (objc_implementation_context);
      objc_ivar_chain = NULL_TREE;
      objc_implementation_context = NULL_TREE;
    }

  /* Process the static instances here because initialization of objc_symtab
     depends on them.  */
  if (objc_static_instances)
    generate_static_references ();

  if (imp_list || class_names_chain
      || meth_var_names_chain || meth_var_types_chain || sel_ref_chain)
    generate_objc_symtab_decl ();

  for (impent = imp_list; impent; impent = impent->next)
    {
      objc_implementation_context = impent->imp_context;
      implementation_template = impent->imp_template;

      UOBJC_CLASS_decl = impent->class_decl;
      UOBJC_METACLASS_decl = impent->meta_decl;

      /* Dump the @interface of each class as we compile it, if the
	 -gen-decls option is in use.  TODO: Dump the classes in the
         order they were found, rather than in reverse order as we
         are doing now.  */
      if (flag_gen_declaration)
	{
	  dump_interface (gen_declaration_file, objc_implementation_context);
	}

      if (TREE_CODE (objc_implementation_context) == CLASS_IMPLEMENTATION_TYPE)
	{
	  /* all of the following reference the string pool...  */
	  generate_ivar_lists ();
	  generate_dispatch_tables ();
	  generate_shared_structures ();
	}
      else
	{
	  generate_dispatch_tables ();
	  generate_category (objc_implementation_context);
	}
    }

  /* If we are using an array of selectors, we must always
     finish up the array decl even if no selectors were used.  */
  if (! flag_next_runtime || sel_ref_chain)
    build_selector_translation_table ();

  if (protocol_chain)
    generate_protocols ();

  if (flag_replace_objc_classes && imp_list)
    generate_objc_image_info ();

  /* Arrange for ObjC data structures to be initialized at run time.  */
  if (objc_implementation_context || class_names_chain || objc_static_instances
      || meth_var_names_chain || meth_var_types_chain || sel_ref_chain)
    {
      build_module_descriptor ();

      if (!flag_next_runtime)
	build_module_initializer_routine ();
    }

  /* Dump the class references.  This forces the appropriate classes
     to be linked into the executable image, preserving unix archive
     semantics.  This can be removed when we move to a more dynamically
     linked environment.  */

  for (chain = cls_ref_chain; chain; chain = TREE_CHAIN (chain))
    {
      handle_class_ref (chain);
      if (TREE_PURPOSE (chain))
	generate_classref_translation_entry (chain);
    }

  for (impent = imp_list; impent; impent = impent->next)
    handle_impent (impent);

  /* Dump the string table last.  */

  generate_strings ();

  if (warn_selector)
    {
      int slot;
      hash hsh;

      /* Run through the selector hash tables and print a warning for any
         selector which has multiple methods.  */

      for (slot = 0; slot < SIZEHASHTABLE; slot++)
	{
	  for (hsh = cls_method_hash_list[slot]; hsh; hsh = hsh->next)
	    check_duplicates (hsh, 0, 1);
	  for (hsh = nst_method_hash_list[slot]; hsh; hsh = hsh->next)
	    check_duplicates (hsh, 0, 1);
	}
    }

  warn_missing_braces = save_warn_missing_braces;
}

/* Subroutines of finish_objc.  */

static void
generate_classref_translation_entry (tree chain)
{
  tree expr, decl, type;

  decl = TREE_PURPOSE (chain);
  type = TREE_TYPE (decl);

  expr = add_objc_string (TREE_VALUE (chain), class_names);
  expr = convert (type, expr); /* cast! */

  /* The decl that is the one that we
     forward declared in build_class_reference.  */
  finish_var_decl (decl, expr);
  return;
}

static void
handle_class_ref (tree chain)
{
  const char *name = IDENTIFIER_POINTER (TREE_VALUE (chain));
  char *string = (char *) alloca (strlen (name) + 30);
  tree decl;
  tree exp;

  sprintf (string, "%sobjc_class_name_%s",
	   (flag_next_runtime ? "." : "__"), name);

#ifdef ASM_DECLARE_UNRESOLVED_REFERENCE
  if (flag_next_runtime)
    {
      ASM_DECLARE_UNRESOLVED_REFERENCE (asm_out_file, string);
      return;
    }
#endif

  /* Make a decl for this name, so we can use its address in a tree.  */
  decl = build_decl (VAR_DECL, get_identifier (string), char_type_node);
  DECL_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;

  pushdecl (decl);
  rest_of_decl_compilation (decl, 0, 0);

  /* Make a decl for the address.  */
  sprintf (string, "%sobjc_class_ref_%s",
	   (flag_next_runtime ? "." : "__"), name);
  exp = build1 (ADDR_EXPR, string_type_node, decl);
  decl = build_decl (VAR_DECL, get_identifier (string), string_type_node);
  DECL_INITIAL (decl) = exp;
  TREE_STATIC (decl) = 1;
  TREE_USED (decl) = 1;

  pushdecl (decl);
  rest_of_decl_compilation (decl, 0, 0);
}

static void
handle_impent (struct imp_entry *impent)
{
  char *string;

  objc_implementation_context = impent->imp_context;
  implementation_template = impent->imp_template;

  if (TREE_CODE (impent->imp_context) == CLASS_IMPLEMENTATION_TYPE)
    {
      const char *const class_name =
	IDENTIFIER_POINTER (CLASS_NAME (impent->imp_context));

      string = (char *) alloca (strlen (class_name) + 30);

      sprintf (string, "%sobjc_class_name_%s",
               (flag_next_runtime ? "." : "__"), class_name);
    }
  else if (TREE_CODE (impent->imp_context) == CATEGORY_IMPLEMENTATION_TYPE)
    {
      const char *const class_name =
	IDENTIFIER_POINTER (CLASS_NAME (impent->imp_context));
      const char *const class_super_name =
        IDENTIFIER_POINTER (CLASS_SUPER_NAME (impent->imp_context));

      string = (char *) alloca (strlen (class_name)
				+ strlen (class_super_name) + 30);

      /* Do the same for categories.  Even though no references to
         these symbols are generated automatically by the compiler, it
         gives you a handle to pull them into an archive by hand.  */
      sprintf (string, "*%sobjc_category_name_%s_%s",
               (flag_next_runtime ? "." : "__"), class_name, class_super_name);
    }
  else
    return;

#ifdef ASM_DECLARE_CLASS_REFERENCE
  if (flag_next_runtime)
    {
      ASM_DECLARE_CLASS_REFERENCE (asm_out_file, string);
      return;
    }
  else
#endif
    {
      tree decl, init;

      init = build_int_cst (c_common_type_for_size (BITS_PER_WORD, 1), 0);
      decl = build_decl (VAR_DECL, get_identifier (string), TREE_TYPE (init));
      TREE_PUBLIC (decl) = 1;
      TREE_READONLY (decl) = 1;
      TREE_USED (decl) = 1;
      TREE_CONSTANT (decl) = 1;
      DECL_CONTEXT (decl) = 0;
      DECL_ARTIFICIAL (decl) = 1;
      DECL_INITIAL (decl) = init;
      assemble_variable (decl, 1, 0, 0);
    }
}

/* The Fix-and-Continue functionality available in Mac OS X 10.3 and
   later requires that ObjC translation units participating in F&C be
   specially marked.  The following routine accomplishes this.  */

/* static int _OBJC_IMAGE_INFO[2] = { 0, 1 }; */

static void
generate_objc_image_info (void)
{
  tree decl, initlist;

  decl = start_var_decl (build_array_type
			 (integer_type_node,
			  build_index_type (build_int_cst (NULL_TREE, 2 - 1))),
			 "_OBJC_IMAGE_INFO");

  initlist = build_tree_list (NULL_TREE, build_int_cst (NULL_TREE, 0));
  initlist = tree_cons (NULL_TREE, build_int_cst (NULL_TREE, 1), initlist);
  initlist = objc_build_constructor (TREE_TYPE (decl), nreverse (initlist));

  finish_var_decl (decl, initlist);
}

/* Look up ID as an instance variable.  OTHER contains the result of
   the C or C++ lookup, which we may want to use instead.  */

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

  /* Look up the ivar, but do not use it if it is not accessible.  */
  ivar = is_ivar (objc_ivar_chain, id);

  if (!ivar || is_private (ivar))
    return other;

  /* In an instance method, a local variable (or parameter) may hide the
     instance variable.  */
  if (TREE_CODE (objc_method_context) == INSTANCE_METHOD_DECL
      && other && other != error_mark_node && !DECL_FILE_SCOPE_P (other))
    {
      warning ("local declaration of %qs hides instance variable",
	       IDENTIFIER_POINTER (id));

      return other;
    }

  /* At this point, we are either in an instance method with no obscuring
     local definitions, or in a class method with no alternate definitions
     at all.  */
  return build_ivar_reference (id);
}

#include "gt-objc-objc-act.h"
