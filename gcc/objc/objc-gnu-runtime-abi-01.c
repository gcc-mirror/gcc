/* GNU Runtime ABI version 8
   Copyright (C) 2011-2021 Free Software Foundation, Inc.
   Contributed by Iain Sandoe (split from objc-act.c)

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
#include "options.h"
#include "tree.h"
#include "stringpool.h"
#include "attribs.h"

#ifdef OBJCPLUS
#include "cp/cp-tree.h"
#else
#include "c/c-tree.h"
#include "c/c-lang.h"
#endif

#include "langhooks.h"
#include "c-family/c-objc.h"
#include "objc-act.h"

/* When building Objective-C++, we are not linking against the C front-end
   and so need to replicate the C tree-construction functions in some way.  */
#ifdef OBJCPLUS
#define OBJCP_REMAP_FUNCTIONS
#include "objcp-decl.h"
#endif  /* OBJCPLUS */

#include "toplev.h"
#include "tree-iterator.h"

#include "objc-runtime-hooks.h"
#include "objc-runtime-shared-support.h"
#include "objc-encoding.h"

/* GNU runtime private definitions.  */
#define DEF_CONSTANT_STRING_CLASS_NAME "NXConstantString"

#define TAG_GETCLASS		"objc_get_class"
#define TAG_GETMETACLASS	"objc_get_meta_class"

#define TAG_MSGSEND		"objc_msg_lookup"
#define TAG_MSGSENDSUPER	"objc_msg_lookup_super"

/* GNU-specific tags.  */

#define TAG_EXECCLASS		"__objc_exec_class"
#define TAG_GNUINIT		"__objc_gnu_init"

/* The version identifies which language generation and runtime
   the module (file) was compiled for, and is recorded in the
   module descriptor.  */
#define OBJC_VERSION		8

#define PROTOCOL_VERSION	2

/* This macro provides a method of removing ambiguity between runtimes
   when LTO is in use on targets supporting multiple runtimes.

   For example, at present, any target that includes an implementation of
   the NeXT runtime needs to place Objective-C meta-data into specific
   named sections.  This should _not_ be done for the GNU runtime, and the
   following macro is used to attach Objective-C private attributes that may
   be used to identify the runtime for which the meta-data are intended.  */

#define OBJCMETA(DECL,VERS,KIND)					\
  if (VERS)								\
    DECL_ATTRIBUTES (DECL) = build_tree_list ((VERS), (KIND));

static void gnu_runtime_01_initialize (void);

static void build_selector_template (void);

static tree gnu_runtime_abi_01_super_superclassfield_id (void);

static tree gnu_runtime_abi_01_class_decl (tree);
static tree gnu_runtime_abi_01_metaclass_decl (tree);
static tree gnu_runtime_abi_01_category_decl (tree);
static tree gnu_runtime_abi_01_protocol_decl (tree);
static tree gnu_runtime_abi_01_string_decl (tree, const char *, string_section);

static tree gnu_runtime_abi_01_get_class_reference (tree);
static tree gnu_runtime_abi_01_build_typed_selector_reference (location_t, tree,
								tree);
static tree gnu_runtime_abi_01_get_protocol_reference (location_t, tree);
static tree gnu_runtime_abi_01_build_ivar_ref (location_t, tree, tree);
static tree gnu_runtime_abi_01_get_class_super_ref (location_t, struct imp_entry *, bool);
static tree gnu_runtime_abi_01_get_category_super_ref (location_t, struct imp_entry *, bool);

static tree gnu_runtime_abi_01_receiver_is_class_object (tree);
static void gnu_runtime_abi_01_get_arg_type_list_base (vec<tree, va_gc> **,
						       tree, int, int);
static tree gnu_runtime_abi_01_build_objc_method_call (location_t, tree, tree,
							tree, tree, tree, int);

static bool gnu_runtime_abi_01_setup_const_string_class_decl (void);
static tree gnu_runtime_abi_01_build_const_string_constructor (location_t, tree,int);

static void objc_generate_v1_gnu_metadata (void);

static tree objc_eh_runtime_type (tree type);
static tree objc_eh_personality (void);
static tree objc_build_exc_ptr (struct objc_try_context **);
static tree build_throw_stmt (location_t, tree, bool);
static tree begin_catch (struct objc_try_context **, tree, tree, tree, bool);
static void finish_catch (struct objc_try_context **, tree);
static tree finish_try_stmt (struct objc_try_context **);

bool
objc_gnu_runtime_abi_01_init (objc_runtime_hooks *rthooks)
{
  /* GNU runtime does not need the compiler to change code in order to do GC. */
  if (flag_objc_gc)
    {
      warning_at (UNKNOWN_LOCATION, 0,
		  "%<-fobjc-gc%> is ignored for %<-fgnu-runtime%>");
      flag_objc_gc = 0;
    }

  /* Although I guess we could, we don't currently support SJLJ exceptions for the
     GNU runtime.  */
  if (flag_objc_sjlj_exceptions)
    {
      inform (UNKNOWN_LOCATION, "%<-fobjc-sjlj-exceptions%> is ignored for %<-fgnu-runtime%>");
      flag_objc_sjlj_exceptions = 0;
    }

  /* TODO: Complain if -fobjc-abi-version=N was used.  */

  /* TODO: Complain if -fobj-nilcheck was used.  */

  rthooks->initialize = gnu_runtime_01_initialize;
  rthooks->default_constant_string_class_name = DEF_CONSTANT_STRING_CLASS_NAME;
  rthooks->tag_getclass = TAG_GETCLASS;
  rthooks->super_superclassfield_ident = gnu_runtime_abi_01_super_superclassfield_id;

  rthooks->class_decl = gnu_runtime_abi_01_class_decl;
  rthooks->metaclass_decl = gnu_runtime_abi_01_metaclass_decl;
  rthooks->category_decl = gnu_runtime_abi_01_category_decl;
  rthooks->protocol_decl = gnu_runtime_abi_01_protocol_decl;
  rthooks->string_decl = gnu_runtime_abi_01_string_decl;

  rthooks->get_class_reference = gnu_runtime_abi_01_get_class_reference;
  rthooks->build_selector_reference = gnu_runtime_abi_01_build_typed_selector_reference;
  rthooks->get_protocol_reference = gnu_runtime_abi_01_get_protocol_reference;
  rthooks->build_ivar_reference = gnu_runtime_abi_01_build_ivar_ref;
  rthooks->get_class_super_ref = gnu_runtime_abi_01_get_class_super_ref;
  rthooks->get_category_super_ref = gnu_runtime_abi_01_get_category_super_ref;

  rthooks->receiver_is_class_object = gnu_runtime_abi_01_receiver_is_class_object;
  rthooks->get_arg_type_list_base = gnu_runtime_abi_01_get_arg_type_list_base;
  rthooks->build_objc_method_call = gnu_runtime_abi_01_build_objc_method_call;

  rthooks->setup_const_string_class_decl =
				gnu_runtime_abi_01_setup_const_string_class_decl;
  rthooks->build_const_string_constructor =
				gnu_runtime_abi_01_build_const_string_constructor;

  rthooks->build_throw_stmt = build_throw_stmt;
  rthooks->build_exc_ptr = objc_build_exc_ptr;
  rthooks->begin_catch = begin_catch;
  rthooks->finish_catch = finish_catch;
  rthooks->finish_try_stmt = finish_try_stmt;

  rthooks->generate_metadata = objc_generate_v1_gnu_metadata;
  return true;
}

static void build_selector_table_decl (void);
static void build_class_template (void);
static void build_category_template (void);
static void build_protocol_template (void);

static GTY(()) tree objc_meta;
static GTY(()) tree meta_base;

static void gnu_runtime_01_initialize (void)
{
  tree type, ftype, IMP_type;

  /* We do not need to mark GNU ObjC metadata for different sections,
     however, we do need to make sure that it is not mistaken for NeXT
     metadata.  */
  objc_meta = get_identifier ("OBJC1METG");
  meta_base = get_identifier ("NONE");

  /* Declare type of selector-objects that represent an operation name.  */
  /* `const struct objc_selector *' */
  type = xref_tag (RECORD_TYPE, get_identifier (TAG_SELECTOR));
  type = build_qualified_type (type, TYPE_QUAL_CONST);
  objc_selector_type = build_pointer_type (type);

  /* SEL typedef.  */
  type = lang_hooks.decls.pushdecl (build_decl (input_location,
						TYPE_DECL,
						objc_selector_name,
						objc_selector_type));
  suppress_warning (type);

  /* typedef id (*IMP)(id, SEL, ...); */
  ftype = build_varargs_function_type_list (objc_object_type,
					    objc_object_type,
					    objc_selector_type,
					    NULL_TREE);

  IMP_type = build_pointer_type (ftype);

  build_class_template ();
  build_super_template ();
  build_protocol_template ();
  build_category_template ();

  /* GNU runtime messenger entry points.  */
  /* TREE_NOTHROW is cleared for the message-sending functions,
     because the function that gets called can throw in Obj-C++, or
     could itself call something that can throw even in Obj-C.  */

  /* IMP objc_msg_lookup (id, SEL); */
  type = build_function_type_list (IMP_type,
				   objc_object_type,
				   objc_selector_type,
				   NULL_TREE);

  umsg_decl = add_builtin_function (TAG_MSGSEND,
				    type, 0, NOT_BUILT_IN,
				    NULL, NULL_TREE);
  TREE_NOTHROW (umsg_decl) = 0;

  /* IMP objc_msg_lookup_super (struct objc_super *, SEL); */
  type = build_function_type_list (IMP_type,
				   objc_super_type,
				   objc_selector_type,
				   NULL_TREE);

  umsg_super_decl = add_builtin_function (TAG_MSGSENDSUPER,
					  type, 0, NOT_BUILT_IN,
					  NULL, NULL_TREE);
  TREE_NOTHROW (umsg_super_decl) = 0;

  /* The following GNU runtime entry point is called to initialize
	 each module:

	 __objc_exec_class (void *); */
  type = build_function_type_list (void_type_node,
				   ptr_type_node,
				   NULL_TREE);

  execclass_decl = add_builtin_function (TAG_EXECCLASS,
					 type, 0, NOT_BUILT_IN,
					 NULL, NULL_TREE);

  type = build_function_type_list (objc_object_type,
				   const_string_type_node,
				   NULL_TREE);

  /* id objc_getClass (const char *); */
  objc_get_class_decl
    = add_builtin_function (TAG_GETCLASS, type, 0, NOT_BUILT_IN,
			    NULL, NULL_TREE);

  /* id objc_getMetaClass (const char *); */
  objc_get_meta_class_decl = add_builtin_function (TAG_GETMETACLASS, type,
						   0, NOT_BUILT_IN, NULL,
						   NULL_TREE);

  /* static SEL _OBJC_SELECTOR_TABLE[]; */
  build_selector_table_decl ();

  /* Stuff for properties.
     The codegen relies on this being NULL for GNU.  */
  objc_copyStruct_decl = NULL_TREE;

  /* This is the type of all of the following functions
     bjc_getPropertyStruct() and objc_setPropertyStruct().  */
  type = build_function_type_list (void_type_node,
				   ptr_type_node,
				   const_ptr_type_node,
				   ptrdiff_type_node,
				   boolean_type_node,
				   boolean_type_node,
				   NULL_TREE);

  /* Declare the following function:
	 void
	 objc_getPropertyStruct (void *destination, const void *source,
                                 ptrdiff_t size, BOOL is_atomic, BOOL has_strong);  */
  objc_getPropertyStruct_decl = add_builtin_function ("objc_getPropertyStruct",
							  type, 0, NOT_BUILT_IN,
							  NULL, NULL_TREE);
  TREE_NOTHROW (objc_getPropertyStruct_decl) = 0;
  /* Declare the following function:
	 void
	 objc_setPropertyStruct (void *destination, const void *source,
	                         ptrdiff_t size, BOOL is_atomic, BOOL has_strong);  */
  objc_setPropertyStruct_decl = add_builtin_function ("objc_setPropertyStruct",
							  type, 0, NOT_BUILT_IN,
							  NULL, NULL_TREE);
  TREE_NOTHROW (objc_setPropertyStruct_decl) = 0;

  using_eh_for_cleanups ();
  lang_hooks.eh_runtime_type = objc_eh_runtime_type;
  lang_hooks.eh_personality = objc_eh_personality;
}

/* --- templates --- */
/* struct _objc_selector {
     SEL sel_id;
     char *sel_type;
   }; */

static void
build_selector_template (void)
{
  tree decls, *chain = NULL;

  objc_selector_template = objc_start_struct (get_identifier (UTAG_SELECTOR));

  /* SEL sel_id; */
  decls = add_field_decl (objc_selector_type, "sel_id", &chain);

  /* char *sel_type; */
  add_field_decl (string_type_node, "sel_type", &chain);

  objc_finish_struct (objc_selector_template, decls);
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
     struct sarray *dtable;
     struct _objc_class *subclass_list;
     struct _objc_class *sibling_class;
     struct _objc_protocol_list *protocols;
     void *gc_object_type;
   };  */

static void
build_class_template (void)
{
  tree ptype, decls, *chain = NULL;

  objc_class_template = objc_start_struct (get_identifier (UTAG_CLASS));

  /* struct _objc_class *isa; */
  decls = add_field_decl (build_pointer_type (objc_class_template),
			  "isa", &chain);

  /* struct _objc_class *super_class; */
  add_field_decl (build_pointer_type (objc_class_template),
		  "super_class", &chain);

  /* char *name; */
  add_field_decl (string_type_node, "name", &chain);

  /* long version; */
  add_field_decl (long_integer_type_node, "version", &chain);

  /* long info; */
  add_field_decl (long_integer_type_node, "info", &chain);

  /* long instance_size; */
  add_field_decl (long_integer_type_node, "instance_size", &chain);

  /* struct _objc_ivar_list *ivars; */
  add_field_decl (objc_ivar_list_ptr,"ivars", &chain);

  /* struct _objc_method_list *methods; */
  add_field_decl (objc_method_list_ptr, "methods", &chain);

  /* struct sarray *dtable; */
  ptype = build_pointer_type(xref_tag (RECORD_TYPE,
					   get_identifier ("sarray")));
  add_field_decl (ptype, "dtable", &chain);

  /* struct objc_class *subclass_list; */
  ptype = build_pointer_type (objc_class_template);
  add_field_decl (ptype, "subclass_list", &chain);

  /* struct objc_class *sibling_class; */
  ptype = build_pointer_type (objc_class_template);
  add_field_decl (ptype, "sibling_class", &chain);

  /* struct _objc_protocol **protocol_list; */
  ptype = build_pointer_type (build_pointer_type
			      (xref_tag (RECORD_TYPE,
					 get_identifier (UTAG_PROTOCOL))));
  add_field_decl (ptype, "protocol_list", &chain);

  /* void *gc_object_type; */
  add_field_decl (build_pointer_type (void_type_node),
		    "gc_object_type", &chain);

  objc_finish_struct (objc_class_template, decls);
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
  tree ptype, decls, *chain = NULL;

  objc_category_template = objc_start_struct (get_identifier (UTAG_CATEGORY));

  /* char *category_name; */
  decls = add_field_decl (string_type_node, "category_name", &chain);

  /* char *class_name; */
  add_field_decl (string_type_node, "class_name", &chain);

  /* struct _objc_method_list *instance_methods; */
  add_field_decl (objc_method_list_ptr, "instance_methods", &chain);

  /* struct _objc_method_list *class_methods; */
  add_field_decl (objc_method_list_ptr, "class_methods", &chain);

  /* struct _objc_protocol **protocol_list; */
  ptype = build_pointer_type (build_pointer_type (objc_protocol_template));
  add_field_decl (ptype, "protocol_list", &chain);

  objc_finish_struct (objc_category_template, decls);
}

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
  tree ptype, decls, *chain = NULL;

  objc_protocol_template = objc_start_struct (get_identifier (UTAG_PROTOCOL));

  /* struct _objc_class *isa; */
  ptype = build_pointer_type (xref_tag (RECORD_TYPE,
					get_identifier (UTAG_CLASS)));
  decls = add_field_decl (ptype, "isa", &chain);

  /* char *protocol_name; */
  add_field_decl (string_type_node, "protocol_name", &chain);

  /* struct _objc_protocol **protocol_list; */
  ptype = build_pointer_type (build_pointer_type (objc_protocol_template));
  add_field_decl (ptype, "protocol_list", &chain);

  /* struct _objc__method_prototype_list *instance_methods; */
  add_field_decl (objc_method_proto_list_ptr, "instance_methods", &chain);

  /* struct _objc__method_prototype_list *class_methods; */
  add_field_decl (objc_method_proto_list_ptr, "class_methods", &chain);

  objc_finish_struct (objc_protocol_template, decls);
}

/* --- names, decls + identifiers --- */

static void
build_selector_table_decl (void)
{
  tree temp;

  build_selector_template ();
  temp = build_array_type (objc_selector_template, NULL_TREE);

  UOBJC_SELECTOR_TABLE_decl = start_var_decl (temp, "_OBJC_SELECTOR_TABLE");
  /* Squash `defined but not used' warning check_global_declaration.  */
  TREE_USED (UOBJC_SELECTOR_TABLE_decl) = 1;
  OBJCMETA (UOBJC_SELECTOR_TABLE_decl, objc_meta, meta_base);
}


static tree
gnu_runtime_abi_01_super_superclassfield_id (void)
{
  if (!super_superclassfield_id)
    super_superclassfield_id = get_identifier ("super_class");
  return super_superclassfield_id;
}


static tree
gnu_runtime_abi_01_class_decl (tree klass)
{
  tree decl;
  char buf[BUFSIZE];
  snprintf (buf, BUFSIZE, "_OBJC_Class_%s",
	    IDENTIFIER_POINTER (CLASS_NAME (klass)));
  decl = start_var_decl (objc_class_template, buf);
  OBJCMETA (decl, objc_meta, meta_base);
  return decl;
}

static tree
gnu_runtime_abi_01_metaclass_decl (tree klass)
{
  tree decl;
  char buf[BUFSIZE];
  snprintf (buf, BUFSIZE, "_OBJC_MetaClass_%s",
	    IDENTIFIER_POINTER (CLASS_NAME (klass)));
  decl = start_var_decl (objc_class_template, buf);
  OBJCMETA (decl, objc_meta, meta_base);
  return decl;
}

static tree
gnu_runtime_abi_01_category_decl (tree klass)
{
  tree decl;
  char buf[BUFSIZE];
  snprintf (buf, BUFSIZE, "_OBJC_Category_%s_on_%s",
	    IDENTIFIER_POINTER (CLASS_SUPER_NAME (klass)),
	    IDENTIFIER_POINTER (CLASS_NAME (klass)));
  decl = start_var_decl (objc_category_template, buf);
  OBJCMETA (decl, objc_meta, meta_base);
  return decl;
}

static tree
gnu_runtime_abi_01_protocol_decl (tree p)
{
  tree decl;
  char buf[BUFSIZE];

  /* static struct _objc_protocol _OBJC_Protocol_<mumble>; */
  snprintf (buf, BUFSIZE, "_OBJC_Protocol_%s",
	    IDENTIFIER_POINTER (PROTOCOL_NAME (p)));
  decl = start_var_decl (objc_protocol_template, buf);
  OBJCMETA (decl, objc_meta, meta_base);
  return decl;
}

static tree
gnu_runtime_abi_01_string_decl (tree type, const char *name,
				string_section where ATTRIBUTE_UNUSED)
{
  tree decl = start_var_decl (type, name);
  OBJCMETA (decl, objc_meta, meta_base);
  return decl;
}

/* --- entry --- */

static tree
gnu_runtime_abi_01_get_class_reference (tree ident)
{
  tree params;

  add_class_reference (ident);

  params = build_tree_list (NULL_TREE, my_build_string_pointer
						(IDENTIFIER_LENGTH (ident) + 1,
						 IDENTIFIER_POINTER (ident)));

  return build_function_call (input_location, objc_get_class_decl, params);
}

/* Used by build_function_type_for_method.  Append the types for
   receiver & _cmd at the start of a method argument list to ARGTYPES.
   CONTEXT is either METHOD_DEF or METHOD_REF, saying whether we are
   trying to define a method or call one.  SUPERFLAG says this is for a
   send to super.  METH may be NULL, in the case that there is no
   prototype.  */

static void
gnu_runtime_abi_01_get_arg_type_list_base (vec<tree, va_gc> **argtypes,
					   tree meth, int context,
					   int superflag ATTRIBUTE_UNUSED)
{
  tree receiver_type;

  if (context == METHOD_DEF && TREE_CODE (meth) == INSTANCE_METHOD_DECL)
    receiver_type = objc_instance_type;
  else
    receiver_type = objc_object_type;

  vec_safe_push (*argtypes, receiver_type);
  /* Selector type - will eventually change to `int'.  */
  vec_safe_push (*argtypes, objc_selector_type);
}

/* Unused for GNU runtime.  */
static tree
gnu_runtime_abi_01_receiver_is_class_object (tree a ATTRIBUTE_UNUSED)
{
  return NULL_TREE;
}

/* sel_ref_chain is a list whose "value" fields will be instances of
   identifier_node that represent the selector.  LOC is the location of
   the @selector.  */

static tree
gnu_runtime_abi_01_build_typed_selector_reference (location_t loc, tree ident,
						   tree prototype)
{
  tree *chain = &sel_ref_chain;
  tree expr;
  int index = 0;

  while (*chain)
    {
      /* When we do a lookup for @selector () we have no idea of the
         prototype - so match the first we find.  */
      if (TREE_VALUE (*chain) == ident
          && (!prototype || TREE_PURPOSE (*chain) == prototype))
	goto return_at_index;

      index++;
      chain = &TREE_CHAIN (*chain);
    }

  *chain = tree_cons (prototype, ident, NULL_TREE);

  /* TODO: Use a vec and keep this in it to (a) avoid re-creating and
     (b) provide better diagnostics for the first time an undefined
     selector is used.  */
 return_at_index:
  expr = build_unary_op (loc, ADDR_EXPR,
			 build_array_ref (loc, UOBJC_SELECTOR_TABLE_decl,
					  build_int_cst (NULL_TREE, index)),
			 1);
  return convert (objc_selector_type, expr);
}

/* Build a tree expression to send OBJECT the operation SELECTOR,
   looking up the method on object LOOKUP_OBJECT (often same as OBJECT),
   assuming the method has prototype METHOD_PROTOTYPE.
   (That is an INSTANCE_METHOD_DECL or CLASS_METHOD_DECL.)
   LOC is the location of the expression to build.
   Use METHOD_PARAMS as list of args to pass to the method.
   If SUPER_FLAG is nonzero, we look up the superclass's method.  */

static tree
build_objc_method_call (location_t loc, int super_flag, tree method_prototype,
			tree lookup_object, tree selector,
			tree method_params)
{
  tree sender = (super_flag ? umsg_super_decl
			    : (flag_objc_direct_dispatch ? umsg_fast_decl
							 : umsg_decl));
  tree rcv_p = (super_flag ? objc_super_type : objc_object_type);
  vec<tree, va_gc> *parms;
  vec<tree, va_gc> *tv;
  unsigned nparm = (method_params ? list_length (method_params) : 0);

  /* If a prototype for the method to be called exists, then cast
     the sender's return type and arguments to match that of the method.
     Otherwise, leave sender as is.  */
  tree ret_type
    = (method_prototype
       ? TREE_VALUE (TREE_TYPE (method_prototype))
       : objc_object_type);
  tree ftype
    = build_function_type_for_method (ret_type, method_prototype,
				      METHOD_REF, super_flag);
  tree sender_cast;
  tree method, t;

  if (method_prototype && METHOD_TYPE_ATTRIBUTES (method_prototype))
    ftype = build_type_attribute_variant (ftype,
					  METHOD_TYPE_ATTRIBUTES
					  (method_prototype));

  sender_cast = build_pointer_type (ftype);

  lookup_object = build_c_cast (loc, rcv_p, lookup_object);

  /* Use SAVE_EXPR to avoid evaluating the receiver twice.  */
  lookup_object = save_expr (lookup_object);

  /* Param list + 2 slots for object and selector.  */
  vec_alloc (parms, nparm + 2);
  vec_alloc (tv, 2);

  /* First, call the lookup function to get a pointer to the method,
     then cast the pointer, then call it with the method arguments.  */
  tv->quick_push (lookup_object);
  tv->quick_push (selector);
  method = build_function_call_vec (loc, vNULL, sender, tv, NULL);
  vec_free (tv);

  /* Pass the appropriate object to the method.  */
  parms->quick_push ((super_flag ? self_decl : lookup_object));

  /* Pass the selector to the method.  */
  parms->quick_push (selector);
  /* Now append the remainder of the parms.  */
  if (nparm)
    for (; method_params; method_params = TREE_CHAIN (method_params))
      parms->quick_push (TREE_VALUE (method_params));

  /* Build an obj_type_ref, with the correct cast for the method call.  */
  t = build3 (OBJ_TYPE_REF, sender_cast, method, lookup_object, size_zero_node);
  t = build_function_call_vec (loc, vNULL, t, parms, NULL);
  vec_free (parms);
  return t;
}

static tree
gnu_runtime_abi_01_build_objc_method_call (location_t loc,
					   tree method_prototype,
					   tree receiver,
					   tree rtype ATTRIBUTE_UNUSED,
					   tree sel_name,
					   tree method_params,
					   int super ATTRIBUTE_UNUSED)
{
  tree selector =
	gnu_runtime_abi_01_build_typed_selector_reference (loc,
							  sel_name,
							  method_prototype);

  return build_objc_method_call (loc, super, method_prototype, receiver,
				 selector, method_params);
}

static tree
gnu_runtime_abi_01_get_protocol_reference (location_t loc, tree p)
{
  tree expr, protocol_struct_type, *chain;
  if (!PROTOCOL_FORWARD_DECL (p))
    PROTOCOL_FORWARD_DECL (p) = gnu_runtime_abi_01_protocol_decl (p);

  expr = build_unary_op (loc, ADDR_EXPR, PROTOCOL_FORWARD_DECL (p), 0);

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

  /* This type is a struct containing the fields of a Protocol
     object.  (Cfr. objc_protocol_type instead is the type of a pointer
     to such a struct).  */
  protocol_struct_type = xref_tag (RECORD_TYPE,
				   get_identifier (PROTOCOL_OBJECT_CLASS_NAME));

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
  return expr;
}

/* For ABI 8 an IVAR is just a fixed offset in the class struct.  */

static tree
gnu_runtime_abi_01_build_ivar_ref (location_t loc ATTRIBUTE_UNUSED,
				   tree base, tree id)
{
  return objc_build_component_ref (base, id);
}

/* We build super class references as we need them (but keep them once
   built for the sake of efficiency).  */

static tree
gnu_runtime_abi_01_get_class_super_ref (location_t loc ATTRIBUTE_UNUSED,
					struct imp_entry *imp, bool inst_meth)
{
  if (inst_meth)
    {
      if (!ucls_super_ref)
	ucls_super_ref =
		objc_build_component_ref (imp->class_decl,
					  get_identifier ("super_class"));
      return ucls_super_ref;
    }
  else
    {
      if (!uucls_super_ref)
	uucls_super_ref =
		objc_build_component_ref (imp->meta_decl,
					  get_identifier ("super_class"));
      return uucls_super_ref;
    }
}

static tree
gnu_runtime_abi_01_get_category_super_ref (location_t loc ATTRIBUTE_UNUSED,
					   struct imp_entry *imp, bool inst_meth)
{
  tree super_name = CLASS_SUPER_NAME (imp->imp_template);
  tree super_class;

  add_class_reference (super_name);
  super_class = (inst_meth ? objc_get_class_decl : objc_get_meta_class_decl);
  super_name = my_build_string_pointer (IDENTIFIER_LENGTH (super_name) + 1,
					IDENTIFIER_POINTER (super_name));
  /* super_class = get_{meta_}class("CLASS_SUPER_NAME");  */
  return build_function_call (input_location,
			      super_class,
			      build_tree_list (NULL_TREE, super_name));
}

static bool
gnu_runtime_abi_01_setup_const_string_class_decl (void)
{
  /* Do nothing, and create no error.  */
  return true;
}

/* Declare a static instance of CLASS_DECL initialized by CONSTRUCTOR.  */

static GTY(()) int num_static_inst;

static tree
objc_add_static_instance (tree constructor, tree class_decl)
{
  tree *chain, decl;
  char buf[BUFSIZE];

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

  snprintf (buf, BUFSIZE, "_OBJC_INSTANCE_%d", num_static_inst++);
  decl = build_decl (input_location,
		     VAR_DECL, get_identifier (buf), class_decl);
  TREE_STATIC (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  TREE_USED (decl) = 1;
  DECL_INITIAL (decl) = constructor;
  DECL_CONTEXT (decl) = NULL;
  OBJCMETA (decl, objc_meta, meta_base);

  /* We may be writing something else just now.
     Postpone till end of input. */
  DECL_DEFER_OUTPUT (decl) = 1;
  lang_hooks.decls.pushdecl (decl);
  rest_of_decl_compilation (decl, 1, 0);

  /* Add the DECL to the head of this CLASS' list.  */
  TREE_PURPOSE (*chain) = tree_cons (NULL_TREE, decl, TREE_PURPOSE (*chain));

  return decl;
}

static tree
gnu_runtime_abi_01_build_const_string_constructor (location_t loc, tree string,
						   int length)
{
  tree constructor, fields;
  vec<constructor_elt, va_gc> *v = NULL;

  /* GNU:    (NXConstantString *) & ((__builtin_ObjCString) { NULL, string, length })  */
  fields = TYPE_FIELDS (internal_const_str_type);
  CONSTRUCTOR_APPEND_ELT (v, fields, build_int_cst (NULL_TREE, 0));

  fields = DECL_CHAIN (fields);
  CONSTRUCTOR_APPEND_ELT (v, fields, build_unary_op (loc,
						     ADDR_EXPR, string, 1));

  fields = DECL_CHAIN (fields);
  CONSTRUCTOR_APPEND_ELT (v, fields, build_int_cst (NULL_TREE, length));
  constructor = objc_build_constructor (internal_const_str_type, v);

  constructor = objc_add_static_instance (constructor, constant_string_type);
  return constructor;
}

/* --- metadata - module initializer --- */

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

  objc_push_parm (build_decl (input_location,
			      PARM_DECL, NULL_TREE, void_type_node));
#ifdef OBJCPLUS
  objc_start_function (get_identifier (TAG_GNUINIT),
		       build_function_type_list (void_type_node, NULL_TREE),
		       NULL_TREE, NULL_TREE);
#else
  objc_start_function (get_identifier (TAG_GNUINIT),
		       build_function_type_list (void_type_node, NULL_TREE),
		       NULL_TREE, objc_get_parm_info (0, NULL_TREE));
#endif
  body = c_begin_compound_stmt (true);
  add_stmt (build_function_call
	    (input_location,
	     execclass_decl,
	     build_tree_list
	     (NULL_TREE,
	      build_unary_op (input_location, ADDR_EXPR,
			      UOBJC_MODULES_decl, 0))));
  add_stmt (c_end_compound_stmt (input_location, body, true));

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
  add_stmt (build_stmt (input_location, EXPR_STMT,
			build_function_call (input_location,
					     GNU_INIT_decl, NULL_TREE)));

  return ctors;
}
#endif /* OBJCPLUS */

/* --- Output GNU Meta-data --- */

static void
generate_classref_translation_entry (tree chain)
{
  tree expr, decl, type;

  decl = TREE_PURPOSE (chain);
  type = TREE_TYPE (decl);

  expr = add_objc_string (TREE_VALUE (chain), class_names);
  expr = convert (type, expr); /* cast! */

  /* This is a class reference.  It is re-written by the runtime,
     but will be optimized away unless we force it.  */
  DECL_PRESERVE_P (decl) = 1;
  OBJCMETA (decl, objc_meta, meta_base);
  finish_var_decl (decl, expr);
  return;
}


static void
handle_impent (struct imp_entry *impent)
{
  char *string;

/*  objc_implementation_context = impent->imp_context;
  implementation_template = impent->imp_template;*/

  switch (TREE_CODE (impent->imp_context))
    {
    case CLASS_IMPLEMENTATION_TYPE:
      {
	const char *const class_name =
	  IDENTIFIER_POINTER (CLASS_NAME (impent->imp_context));

	string = (char *) alloca (strlen (class_name) + 30);

	sprintf (string, "__objc_class_name_%s", class_name);
	break;
      }
    case CATEGORY_IMPLEMENTATION_TYPE:
      {
	const char *const class_name =
	  IDENTIFIER_POINTER (CLASS_NAME (impent->imp_context));
	const char *const class_super_name =
	  IDENTIFIER_POINTER (CLASS_SUPER_NAME (impent->imp_context));

	string = (char *) alloca (strlen (class_name)
				  + strlen (class_super_name) + 30);

	/* Do the same for categories.  Even though no references to
	   these symbols are generated automatically by the compiler,
	   it gives you a handle to pull them into an archive by
	   hand.  */
	sprintf (string, "*__objc_category_name_%s_%s", class_name, class_super_name);
	break;
      }
    default:
      return;
    }

    {
      tree decl, init;

      init = integer_zero_node;
      decl = build_decl (input_location,
			 VAR_DECL, get_identifier (string), TREE_TYPE (init));
      TREE_PUBLIC (decl) = 1;
      TREE_READONLY (decl) = 1;
      TREE_USED (decl) = 1;
      TREE_CONSTANT (decl) = 1;
      DECL_CONTEXT (decl) = NULL_TREE;
      DECL_ARTIFICIAL (decl) = 1;
      TREE_STATIC (decl) = 1;
      DECL_INITIAL (decl) = error_mark_node; /* A real initializer is coming... */
      /* We must force the reference.  */
      DECL_PRESERVE_P (decl) = 1;

      finish_var_decl(decl, init) ;
    }
}

tree
build_protocol_initializer (tree type, tree protocol_name, tree protocol_list,
			    tree inst_methods, tree class_methods)
{
  tree expr, ttyp;
  location_t loc;
  vec<constructor_elt, va_gc> *inits = NULL;

  /* TODO: pass the loc in or find it from args.  */
  loc = input_location;
  ttyp = build_pointer_type (xref_tag (RECORD_TYPE,
				       get_identifier (UTAG_CLASS)));
  /* Filling the "isa" in with a version allows the runtime system to
     detect this ...   */
  expr = build_int_cst (ttyp, PROTOCOL_VERSION);

  CONSTRUCTOR_APPEND_ELT (inits, NULL_TREE, expr);

  CONSTRUCTOR_APPEND_ELT (inits, NULL_TREE, protocol_name);
  CONSTRUCTOR_APPEND_ELT (inits, NULL_TREE, protocol_list);

  ttyp = objc_method_proto_list_ptr;
  if (inst_methods)
    expr = convert (ttyp, build_unary_op (loc, ADDR_EXPR, inst_methods, 0));
  else
    expr = convert (ttyp, null_pointer_node);
  CONSTRUCTOR_APPEND_ELT (inits, NULL_TREE, expr);

  if (class_methods)
    expr = convert (ttyp, build_unary_op (loc, ADDR_EXPR, class_methods, 0));
  else
    expr = convert (ttyp, null_pointer_node);
  CONSTRUCTOR_APPEND_ELT (inits, NULL_TREE, expr);

  return objc_build_constructor (type, inits);
}

static tree
generate_protocol_list (tree i_or_p, tree klass_ctxt)
{
  tree array_type, ptype, refs_decl, lproto, e, plist;
  vec<constructor_elt, va_gc> *v = NULL;
  char buf[BUFSIZE];
  int size = 0;

  switch (TREE_CODE (i_or_p))
    {
    case CLASS_INTERFACE_TYPE:
    case CATEGORY_INTERFACE_TYPE:
      plist = CLASS_PROTOCOL_LIST (i_or_p);
      break;
    case PROTOCOL_INTERFACE_TYPE:
      plist = PROTOCOL_LIST (i_or_p);
      break;
    default:
      gcc_unreachable ();
    }

  /* Compute size.  */
  for (lproto = plist; lproto; lproto = TREE_CHAIN (lproto))
    if (TREE_CODE (TREE_VALUE (lproto)) == PROTOCOL_INTERFACE_TYPE
	&& PROTOCOL_FORWARD_DECL (TREE_VALUE (lproto)))
      size++;

  /* Build initializer.  */
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, build_int_cst (NULL_TREE, 0));
  e = build_int_cst (build_pointer_type (objc_protocol_template), size);
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, e);

  for (lproto = plist; lproto; lproto = TREE_CHAIN (lproto))
    {
      tree pval = TREE_VALUE (lproto);

      if (TREE_CODE (pval) == PROTOCOL_INTERFACE_TYPE
	  && PROTOCOL_FORWARD_DECL (pval))
	{
	  tree fwref = PROTOCOL_FORWARD_DECL (pval);
	  location_t loc = DECL_SOURCE_LOCATION (fwref) ;
	  e = build_unary_op (loc, ADDR_EXPR, fwref, 0);
          CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, e);
	}
    }

  /* static struct objc_protocol *refs[n]; */

  switch (TREE_CODE (i_or_p))
    {
    case PROTOCOL_INTERFACE_TYPE:
      snprintf (buf, BUFSIZE, "_OBJC_ProtocolRefs_%s",
		IDENTIFIER_POINTER (PROTOCOL_NAME (i_or_p)));
      break;
    case CLASS_INTERFACE_TYPE:
      snprintf (buf, BUFSIZE, "_OBJC_ClassProtocols_%s",
		IDENTIFIER_POINTER (CLASS_NAME (i_or_p)));
      break;
    case CATEGORY_INTERFACE_TYPE:
      snprintf (buf, BUFSIZE, "_OBJC_CategoryProtocols_%s_%s",
		IDENTIFIER_POINTER (CLASS_NAME (klass_ctxt)),
		IDENTIFIER_POINTER (CLASS_SUPER_NAME (klass_ctxt)));
      break;
    default:
      gcc_unreachable ();
    }

  ptype = build_pointer_type (objc_protocol_template);
  array_type = build_sized_array_type (ptype, size + 3);
  refs_decl = start_var_decl (array_type, buf);
  OBJCMETA (refs_decl, objc_meta, meta_base);
  finish_var_decl (refs_decl,
                   objc_build_constructor (TREE_TYPE (refs_decl), v));

  return refs_decl;
}

static tree
generate_v1_meth_descriptor_table (tree chain, tree protocol, const char *prefix)
{
  tree method_list_template, initlist, decl;
  int size;
  vec<constructor_elt, va_gc> *v = NULL;
  char buf[BUFSIZE];

  if (!chain || !prefix)
    return NULL_TREE;

  if (!objc_method_prototype_template)
    objc_method_prototype_template = build_method_prototype_template ();

  size = list_length (chain);
  method_list_template =
	build_method_prototype_list_template (objc_method_prototype_template,
					      size);
  snprintf (buf, BUFSIZE, "%s_%s", prefix,
	    IDENTIFIER_POINTER (PROTOCOL_NAME (protocol)));

  decl = start_var_decl (method_list_template, buf);

  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, build_int_cst (NULL_TREE, size));
  initlist =
	build_descriptor_table_initializer (objc_method_prototype_template,
					    chain);
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, initlist);
  OBJCMETA (decl, objc_meta, meta_base);
  finish_var_decl (decl, objc_build_constructor (method_list_template, v));
  return decl;
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

   The GNU runtime fixes up all protocols before user code from the module
   is executed; it requires pointers to those symbols
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
	  nst_methods = DECL_CHAIN (nst_methods);
	}

      UOBJC_INSTANCE_METHODS_decl =
	generate_v1_meth_descriptor_table (PROTOCOL_NST_METHODS (p), p,
					   "_OBJC_PROTOCOL_INSTANCE_METHODS");

      while (cls_methods)
	{
	  if (! METHOD_ENCODING (cls_methods))
	    {
	      encoding = encode_method_prototype (cls_methods);
	      METHOD_ENCODING (cls_methods) = encoding;
	    }

	  cls_methods = DECL_CHAIN (cls_methods);
	}

      UOBJC_CLASS_METHODS_decl =
	generate_v1_meth_descriptor_table (PROTOCOL_CLS_METHODS (p), p,
					   "_OBJC_PROTOCOL_CLASS_METHODS");
/*      generate_method_descriptors (p);*/

      if (PROTOCOL_LIST (p))
	refs_decl = generate_protocol_list (p, NULL_TREE);
      else
	refs_decl = 0;

      /* static struct objc_protocol _OBJC_PROTOCOL_<mumble>; */
      protocol_name_expr = add_objc_string (PROTOCOL_NAME (p), class_names);

      if (refs_decl)
	refs_expr = convert (build_pointer_type (build_pointer_type
						 (objc_protocol_template)),
			     build_unary_op (input_location,
					     ADDR_EXPR, refs_decl, 0));
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
generate_dispatch_table (tree chain, const char *name)
{
  tree decl, method_list_template, initlist;
  vec<constructor_elt, va_gc> *v = NULL;
  int size = list_length (chain);

  if (!objc_method_template)
    objc_method_template = build_method_template ();

  method_list_template = build_method_list_template (objc_method_template,
						     size);
  initlist = build_dispatch_table_initializer (objc_method_template, chain);

  decl = start_var_decl (method_list_template, name);

  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, integer_zero_node);
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE,
			  build_int_cst (integer_type_node, size));
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, initlist);

  OBJCMETA (decl, objc_meta, meta_base);
  finish_var_decl (decl,
		   objc_build_constructor (TREE_TYPE (decl), v));

  return decl;
}

/* Init a category.  */
static tree
build_category_initializer (tree type, tree cat_name, tree class_name,
			    tree inst_methods, tree class_methods,
			    tree protocol_list)
{
  tree expr, ltyp;
  location_t loc;
  vec<constructor_elt, va_gc> *v = NULL;

  /* TODO: pass the loc in or find it from args.  */
  /* TODO: pass the loc in or find it from args.  */
  loc = UNKNOWN_LOCATION;
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, cat_name);
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, class_name);

  ltyp = objc_method_list_ptr;
  if (inst_methods)
    expr = convert (ltyp, build_unary_op (loc, ADDR_EXPR, inst_methods, 0));
  else
    expr = convert (ltyp, null_pointer_node);
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, expr);

  if (class_methods)
    expr = convert (ltyp, build_unary_op (loc, ADDR_EXPR, class_methods, 0));
  else
    expr = convert (ltyp, null_pointer_node);
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, expr);

  /* protocol_list = */
  ltyp = build_pointer_type (build_pointer_type (objc_protocol_template));
  if (protocol_list)
    expr = convert (ltyp, build_unary_op (loc, ADDR_EXPR, protocol_list, 0));
  else
    expr = convert (ltyp, null_pointer_node);
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, expr);

  return objc_build_constructor (type, v);
}

/* static struct objc_category _OBJC_CATEGORY_<name> = { ... };  */

static void
generate_category (struct imp_entry *impent)
{
  tree initlist, cat_name_expr, class_name_expr;
  tree protocol_decl, category, cat_decl;
  tree inst_methods = NULL_TREE, class_methods = NULL_TREE;
  tree cat = impent->imp_context;
  char buf[BUFSIZE];

  cat_decl = impent->class_decl;

  add_class_reference (CLASS_NAME (cat));
  cat_name_expr = add_objc_string (CLASS_SUPER_NAME (cat), class_names);

  class_name_expr = add_objc_string (CLASS_NAME (cat), class_names);

  category = lookup_category (impent->imp_template, CLASS_SUPER_NAME (cat));

  if (category && CLASS_PROTOCOL_LIST (category))
    {
      generate_protocol_references (CLASS_PROTOCOL_LIST (category));
      protocol_decl = generate_protocol_list (category, cat);
    }
  else
    protocol_decl = 0;

  if (CLASS_NST_METHODS (cat))
    {
      snprintf (buf, BUFSIZE, "_OBJC_CategoryInstanceMethods_%s_%s",
		IDENTIFIER_POINTER (CLASS_NAME (cat)),
		IDENTIFIER_POINTER (CLASS_SUPER_NAME (cat)));
      inst_methods = generate_dispatch_table (CLASS_NST_METHODS (cat), buf);
    }

  if (CLASS_CLS_METHODS (cat))
    {
      snprintf (buf, BUFSIZE, "_OBJC_CategoryClassMethods_%s_%s",
		IDENTIFIER_POINTER (CLASS_NAME (cat)),
		IDENTIFIER_POINTER (CLASS_SUPER_NAME (cat)));
      class_methods = generate_dispatch_table (CLASS_CLS_METHODS (cat), buf);
    }

  initlist = build_category_initializer (TREE_TYPE (cat_decl),
					 cat_name_expr, class_name_expr,
					 inst_methods, class_methods,
					 protocol_decl);
  /* Finish and initialize the forward decl.  */
  finish_var_decl (cat_decl, initlist);
  impent->class_decl = cat_decl;
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
     struct sarray *dtable;
     struct objc_class *subclass_list;
     struct objc_class *sibling_class;
     struct objc_protocol_list *protocols;
     void *gc_object_type;
   };  */

static tree
build_shared_structure_initializer (tree type, tree isa, tree super,
				    tree name, tree size, int status,
				    tree dispatch_table, tree ivar_list,
				    tree protocol_list)
{
  tree expr, ltyp;
  vec<constructor_elt, va_gc> *v = NULL;

  /* isa = */
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, isa);

  /* super_class = */
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, super);

  /* name = */
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, default_conversion (name));

  /* version = */
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE,
                          build_int_cst (long_integer_type_node, 0));

  /* info = */
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE,
                          build_int_cst (long_integer_type_node, status));

  /* instance_size = */
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE,
                          convert (long_integer_type_node, size));

  /* objc_ivar_list = */
  if (!ivar_list)
    CONSTRUCTOR_APPEND_ELT (v, NULL_TREE,
			    build_int_cst (objc_ivar_list_ptr, 0));
  else
    {
      expr = convert (objc_ivar_list_ptr,
		      build_unary_op (input_location, ADDR_EXPR,
				      ivar_list, 0));
      CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, expr);
    }

  /* objc_method_list = */
  if (!dispatch_table)
    CONSTRUCTOR_APPEND_ELT (v, NULL_TREE,
			   convert (objc_method_list_ptr, null_pointer_node));
  else
    {
      expr = convert (objc_method_list_ptr,
		      build_unary_op (input_location, ADDR_EXPR,
				      dispatch_table, 0));
      CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, expr);
    }

  /* dtable = */
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, build_int_cst (NULL_TREE, 0));

  /* subclass_list = */
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, build_int_cst (NULL_TREE, 0));

  /* sibling_class = */
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, build_int_cst (NULL_TREE, 0));

  /* protocol_list = */
  ltyp = build_pointer_type (build_pointer_type (objc_protocol_template));
  if (! protocol_list)
    CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, build_int_cst (ltyp, 0));
  else
    {
      expr = convert (ltyp,
		      build_unary_op (input_location, ADDR_EXPR,
				      protocol_list, 0));
      CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, expr);
    }

  /* gc_object_type = NULL */
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, build_int_cst (NULL_TREE, 0));

  return objc_build_constructor (type, v);
}


static tree
generate_ivars_list (tree chain, const char *name)
{
  tree initlist, ivar_list_template, decl;
  int size;
  vec<constructor_elt, va_gc> *inits = NULL;

  if (!chain)
    return NULL_TREE;

  if (!objc_ivar_template)
    objc_ivar_template = build_ivar_template ();

  size = ivar_list_length (chain);

  generating_instance_variables = 1;
  ivar_list_template = build_ivar_list_template (objc_ivar_template, size);
  initlist = build_ivar_list_initializer (objc_ivar_template, chain);
  generating_instance_variables = 0;

  decl = start_var_decl (ivar_list_template, name);

  CONSTRUCTOR_APPEND_ELT (inits, NULL_TREE, build_int_cst (NULL_TREE, size));
  CONSTRUCTOR_APPEND_ELT (inits, NULL_TREE, initlist);

  OBJCMETA (decl, objc_meta, meta_base);
  finish_var_decl (decl,
		   objc_build_constructor (TREE_TYPE (decl), inits));

  return decl;
}

/* static struct objc_class _OBJC_METACLASS_Foo={ ... };
   static struct objc_class _OBJC_CLASS_Foo={ ... };  */

static void
generate_class_structures (struct imp_entry *impent)
{
  tree name_expr, super_expr, root_expr, class_decl, meta_decl;
  tree my_root_id, my_super_id;
  tree cast_type, initlist, protocol_decl;
  tree inst_methods = NULL_TREE, class_methods = NULL_TREE;
  tree chain, inst_ivars = NULL_TREE, class_ivars = NULL_TREE;
  location_t loc;
  char buf[BUFSIZE];
  int cls_flags = 0 ;

/*  objc_implementation_context = impent->imp_context;
  implementation_template = impent->imp_template;*/
  class_decl = impent->class_decl;
  meta_decl = impent->meta_decl;
/*  UOBJC_CLASS_decl = impent->class_decl;
  UOBJC_METACLASS_decl = impent->meta_decl;*/

  loc = DECL_SOURCE_LOCATION (impent->class_decl);

  my_super_id = CLASS_SUPER_NAME (impent->imp_template);
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
    my_root_id = CLASS_NAME (impent->imp_template);

  cast_type = build_pointer_type (objc_class_template);
  name_expr = add_objc_string (CLASS_NAME (impent->imp_template),
			       class_names);

  /* Install class `isa' and `super' pointers at runtime.  */
  if (my_super_id)
    super_expr = add_objc_string (my_super_id, class_names);
  else
    super_expr = null_pointer_node;

  super_expr = build_c_cast (loc, cast_type, super_expr);

  root_expr = add_objc_string (my_root_id, class_names);
  root_expr = build_c_cast (loc, cast_type, root_expr);

  if (CLASS_PROTOCOL_LIST (impent->imp_template))
    {
      generate_protocol_references (CLASS_PROTOCOL_LIST (impent->imp_template));
      protocol_decl = generate_protocol_list (impent->imp_template,
					      impent->imp_context);
    }
  else
    protocol_decl = NULL_TREE;

  if (CLASS_CLS_METHODS (impent->imp_context))
    {
      snprintf (buf, BUFSIZE, "_OBJC_ClassMethods_%s",
		IDENTIFIER_POINTER (CLASS_NAME (impent->imp_context)));
      class_methods = generate_dispatch_table (CLASS_CLS_METHODS (impent->imp_context),
					       buf);
    }

  if (CLASS_SUPER_NAME (impent->imp_template) == NULL_TREE
      && (chain = TYPE_FIELDS (objc_class_template)))
    {
      snprintf (buf, BUFSIZE, "_OBJC_ClassIvars_%s",
		IDENTIFIER_POINTER (CLASS_NAME (impent->imp_context)));
      class_ivars = generate_ivars_list (chain, buf);
    }

  /* static struct objc_class _OBJC_METACLASS_Foo = { ... }; */

  initlist =
	build_shared_structure_initializer
			(TREE_TYPE (meta_decl),
			root_expr, super_expr, name_expr,
			convert (integer_type_node,
				TYPE_SIZE_UNIT (objc_class_template)),
			CLS_META, class_methods, class_ivars,
			protocol_decl);

  finish_var_decl (meta_decl, initlist);
  impent->meta_decl = meta_decl;

  /* static struct objc_class _OBJC_CLASS_Foo={ ... }; */
  if (CLASS_NST_METHODS (impent->imp_context))
    {
      snprintf (buf, BUFSIZE, "_OBJC_InstanceMethods_%s",
		IDENTIFIER_POINTER (CLASS_NAME (impent->imp_context)));
      inst_methods = generate_dispatch_table (CLASS_NST_METHODS (impent->imp_context),
					      buf);
    }

  if ((chain = CLASS_IVARS (impent->imp_template)))
    {
      snprintf (buf, BUFSIZE, "_OBJC_InstanceIvars_%s",
		IDENTIFIER_POINTER (CLASS_NAME (impent->imp_context)));
      inst_ivars = generate_ivars_list (chain, buf);
    }

  initlist =
	build_shared_structure_initializer
		(TREE_TYPE (class_decl),
		build_unary_op (loc, ADDR_EXPR, meta_decl, 0),
		super_expr, name_expr,
		convert (integer_type_node,
			 TYPE_SIZE_UNIT (CLASS_STATIC_TEMPLATE
					(impent->imp_template))),
		CLS_FACTORY | cls_flags, inst_methods, inst_ivars,
		protocol_decl);

  finish_var_decl (class_decl, initlist);
  impent->class_decl = class_decl;
}

/* --- Output GNU Metadata --- */

/* TODO: Make this into an array of refs.  */
static void
handle_class_ref (tree chain)
{
  const char *name = IDENTIFIER_POINTER (TREE_VALUE (chain));
  char *string = (char *) alloca (strlen (name) + 30);
  tree decl;
  tree exp;

  sprintf (string, "__objc_class_name_%s", name);

  /* Make a decl for this name, so we can use its address in a tree.  */
  decl = build_decl (input_location,
		     VAR_DECL, get_identifier (string), TREE_TYPE (integer_zero_node));
  DECL_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;
  DECL_CONTEXT (decl) = NULL_TREE;
  finish_var_decl (decl, 0);

  /* Make a decl for the address.  */
  sprintf (string, "__objc_class_ref_%s", name);
  exp = build1 (ADDR_EXPR, string_type_node, decl);
  decl = build_decl (input_location,
		     VAR_DECL, get_identifier (string), string_type_node);
  TREE_STATIC (decl) = 1;
  TREE_USED (decl) = 1;
  DECL_READ_P (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  DECL_INITIAL (decl) = error_mark_node;

  /* We must force the reference.  */
  DECL_PRESERVE_P (decl) = 1;

  DECL_CONTEXT (decl) = NULL_TREE;
  finish_var_decl (decl, exp);
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

static void
build_gnu_selector_translation_table (void)
{
  tree chain, expr;
  vec<constructor_elt, va_gc> *inits = NULL;
  vec<constructor_elt, va_gc> *v ;

  /* Cause the selector table (previously forward-declared)
     to be actually output.  */

  for (chain = sel_ref_chain; chain; chain = TREE_CHAIN (chain))
    {
      tree encoding;
      if (warn_selector)
	{
	  /* TODO: improve on the location for the diagnostic.  */
	  location_t loc = input_location;
	  diagnose_missing_method (TREE_VALUE (chain), loc);
	}

      v = NULL;
      expr = build_selector (TREE_VALUE (chain));
      encoding = get_proto_encoding (TREE_PURPOSE (chain));
      CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, expr);
      CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, encoding);
      expr = objc_build_constructor (objc_selector_template, v);

      CONSTRUCTOR_APPEND_ELT (inits, NULL_TREE, expr);
    } /* each element in the chain */

  /* List terminator.  */
  v = NULL;
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, integer_zero_node);
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, integer_zero_node);
  expr = objc_build_constructor (objc_selector_template, v);

  CONSTRUCTOR_APPEND_ELT (inits, NULL_TREE, expr);
  expr = objc_build_constructor (TREE_TYPE (UOBJC_SELECTOR_TABLE_decl),
				     inits);
  finish_var_decl (UOBJC_SELECTOR_TABLE_decl, expr);
}

/* Output references to all statically allocated objects.  Return the DECL
   for the array built.  */

static void
generate_static_references (void)
{
  tree expr = NULL_TREE;
  tree class_name, klass, decl;
  tree cl_chain, in_chain, type
    = build_array_type (build_pointer_type (void_type_node), NULL_TREE);
  int num_inst, num_class;
  char buf[BUFSIZE];
  vec<constructor_elt, va_gc> *decls = NULL;

  for (cl_chain = objc_static_instances, num_class = 0;
       cl_chain; cl_chain = TREE_CHAIN (cl_chain), num_class++)
    {
      vec<constructor_elt, va_gc> *v = NULL;

      for (num_inst = 0, in_chain = TREE_PURPOSE (cl_chain);
	   in_chain; num_inst++, in_chain = TREE_CHAIN (in_chain));

      snprintf (buf, BUFSIZE, "_OBJC_STATIC_INSTANCES_%d", num_class);
      decl = start_var_decl (type, buf);

      /* Output {class_name, ...}.  */
      klass = TREE_VALUE (cl_chain);
      class_name = get_objc_string_decl (OBJC_TYPE_NAME (klass), class_names);
      CONSTRUCTOR_APPEND_ELT (v, NULL_TREE,
			      build_unary_op (input_location,
					      ADDR_EXPR, class_name, 1));

      /* Output {..., instance, ...}.  */
      for (in_chain = TREE_PURPOSE (cl_chain);
	   in_chain; in_chain = TREE_CHAIN (in_chain))
	{
	  expr = build_unary_op (input_location,
				 ADDR_EXPR, TREE_VALUE (in_chain), 1);
	  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, expr);
	}

      /* Output {..., NULL}.  */
      CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, build_int_cst (NULL_TREE, 0));

      expr = objc_build_constructor (TREE_TYPE (decl), v);
      OBJCMETA (decl, objc_meta, meta_base);
      finish_var_decl (decl, expr);
      CONSTRUCTOR_APPEND_ELT (decls, NULL_TREE,
			      build_unary_op (input_location,
					      ADDR_EXPR, decl, 1));
    }

  CONSTRUCTOR_APPEND_ELT (decls, NULL_TREE, build_int_cst (NULL_TREE, 0));
  expr = objc_build_constructor (type, decls);
  static_instances_decl = start_var_decl (type, "_OBJC_STATIC_INSTANCES");
  OBJCMETA (static_instances_decl, objc_meta, meta_base);
  finish_var_decl (static_instances_decl, expr);
}

/* Create the initial value for the `defs' field of _objc_symtab.
   This is a CONSTRUCTOR.  */

static tree
init_def_list (tree type)
{
  tree expr;
  struct imp_entry *impent;
  location_t loc;
  vec<constructor_elt, va_gc> *v = NULL;

  if (imp_count)
    for (impent = imp_list; impent; impent = impent->next)
      {
	if (TREE_CODE (impent->imp_context) == CLASS_IMPLEMENTATION_TYPE)
	  {
	    loc = DECL_SOURCE_LOCATION (impent->class_decl);
	    expr = build_unary_op (loc,
				   ADDR_EXPR, impent->class_decl, 0);
	    CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, expr);
	  }
      }

  if (cat_count)
    for (impent = imp_list; impent; impent = impent->next)
      {
	if (TREE_CODE (impent->imp_context) == CATEGORY_IMPLEMENTATION_TYPE)
	  {
	    loc = DECL_SOURCE_LOCATION (impent->class_decl);
	    expr = build_unary_op (loc,
				   ADDR_EXPR, impent->class_decl, 0);
	    CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, expr);
	  }
      }

  loc = UNKNOWN_LOCATION;
  /* statics = { ..., _OBJC_STATIC_INSTANCES, ... }  */
  if (static_instances_decl)
    expr = build_unary_op (loc, ADDR_EXPR, static_instances_decl, 0);
  else
    expr = integer_zero_node;
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, expr);

  return objc_build_constructor (type, v);
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
  tree fields, array_type, *chain = NULL;
  int index;

  objc_symtab_template = objc_start_struct (get_identifier (UTAG_SYMTAB));

  /* long sel_ref_cnt; */
  fields = add_field_decl (long_integer_type_node, "sel_ref_cnt", &chain);

  /* SEL *refs; */
  add_field_decl (build_pointer_type (objc_selector_type), "refs", &chain);

  /* short cls_def_cnt; */
  add_field_decl (short_integer_type_node, "cls_def_cnt", &chain);

  /* short cat_def_cnt; */
  add_field_decl (short_integer_type_node, "cat_def_cnt", &chain);

  /* Note that padding will be added here on LP64.  */

  /* void *defs[imp_count + cat_count (+ 1)]; */
  /* NB: The index is one less than the size of the array.  */
  index = imp_count + cat_count;
  array_type = build_sized_array_type (ptr_type_node, index + 1);
  add_field_decl (array_type, "defs", &chain);

  objc_finish_struct (objc_symtab_template, fields);
}
/* Construct the initial value for all of _objc_symtab.  */

static tree
init_objc_symtab (tree type)
{
  tree field, expr, ltyp;
  location_t loc;
  vec<constructor_elt, va_gc> *v = NULL;

  loc = UNKNOWN_LOCATION;

  /* sel_ref_cnt = { ..., 5, ... } */

  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE,
			  build_int_cst (long_integer_type_node, 0));

  /* refs = { ..., _OBJC_SELECTOR_TABLE, ... } */

  ltyp = build_pointer_type (objc_selector_type);
  if (sel_ref_chain)
    expr = convert (ltyp, build_unary_op (loc, ADDR_EXPR,
					  UOBJC_SELECTOR_TABLE_decl, 1));
  else
    expr = convert (ltyp, null_pointer_node);
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, expr);

  /* cls_def_cnt = { ..., 5, ... } */

  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE,
			  build_int_cst (short_integer_type_node, imp_count));

  /* cat_def_cnt = { ..., 5, ... } */

  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE,
			  build_int_cst (short_integer_type_node, cat_count));

  /* cls_def = { ..., { &Foo, &Bar, ...}, ... } */

  field = TYPE_FIELDS (type);
  field = DECL_CHAIN (DECL_CHAIN (DECL_CHAIN (DECL_CHAIN (field))));

  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, init_def_list (TREE_TYPE (field)));

  return objc_build_constructor (type, v);
}

/* Create the declaration of _OBJC_SYMBOLS, with type `struct _objc_symtab'
   and initialized appropriately.  */

static void
generate_objc_symtab_decl (void)
{
  build_objc_symtab_template ();
  UOBJC_SYMBOLS_decl = start_var_decl (objc_symtab_template, "_OBJC_SYMBOLS");
  OBJCMETA (UOBJC_SYMBOLS_decl, objc_meta, meta_base);
  finish_var_decl (UOBJC_SYMBOLS_decl,
		   init_objc_symtab (TREE_TYPE (UOBJC_SYMBOLS_decl)));
}

static void
objc_generate_v1_gnu_metadata (void)
{
  struct imp_entry *impent;
  tree chain;

  /* Process the static instances here because initialization of objc_symtab
     depends on them.  */
  if (objc_static_instances)
    generate_static_references ();

  objc_implementation_context =
  implementation_template =
  UOBJC_CLASS_decl =
  UOBJC_METACLASS_decl = NULL_TREE;

  for (impent = imp_list; impent; impent = impent->next)
    {
      /* If -gen-decls is present, Dump the @interface of each class.
	 TODO: Dump the classes in the  order they were found, rather than in
	 reverse order as we are doing now.  */
      if (flag_gen_declaration)
	dump_interface (gen_declaration_file, impent->imp_context);

      /* all of the following reference the string pool...  */
      if (TREE_CODE (impent->imp_context) == CLASS_IMPLEMENTATION_TYPE)
	generate_class_structures (impent);
      else
	generate_category (impent);
    }

  /* If we are using an array of selectors, we must always
     finish up the array decl even if no selectors were used.  */
  build_gnu_selector_translation_table ();

  if (protocol_chain)
    generate_protocols ();

  /* Arrange for ObjC data structures to be initialized at run time.  */
  /* FIXME: Have some more elegant way to determine if we need to
     generate objc_symtab_decl or not, instead of checking these
     global symbols.  */
  if (imp_list || class_names_chain
      || meth_var_names_chain || meth_var_types_chain || sel_ref_chain
      || prop_names_attr_chain)
    generate_objc_symtab_decl ();

  if (imp_list || class_names_chain || objc_static_instances
      || meth_var_names_chain || meth_var_types_chain || sel_ref_chain)
    {
      /* Make sure that the meta-data are identified as being
	 GNU-runtime.  */
      build_module_descriptor (OBJC_VERSION,
			       build_tree_list (objc_meta, meta_base));
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

  generate_strings ();
}

/* --- exceptions --- */

static GTY(()) tree objc_eh_personality_decl;

static tree
objc_eh_runtime_type (tree type)
{
  tree ident, eh_id, decl, str;

  if (type == error_mark_node
      || errorcount || sorrycount)
    {
      /* Use 'ErrorMarkNode' as class name when error_mark_node is found
	 to prevent an ICE.  Note that we know that the compiler will
	 terminate with an error and this 'ErrorMarkNode' class name will
	 never be actually used.  */
      ident = get_identifier ("ErrorMarkNode");
      goto make_err_class;
    }

  if (POINTER_TYPE_P (type) && objc_is_object_id (TREE_TYPE (type)))
    /* We don't want to identify 'id' for GNU. Instead, build a 0
       entry in the exceptions table.  */
    return null_pointer_node;

  if (!POINTER_TYPE_P (type) || !TYPED_OBJECT (TREE_TYPE (type)))
    {
#ifdef OBJCPLUS
      /* This routine is also called for c++ catch clauses; in which case,
	 we use the c++ typeinfo decl. */
      return build_eh_type_type (type);
#else
      error ("non-objective-c type %qT cannot be caught", type);
      ident = get_identifier ("ErrorMarkNode");
      goto make_err_class;
#endif
    }
  else
    ident = OBJC_TYPE_NAME (TREE_TYPE (type));

make_err_class:
  /* If this class was already referenced, then it will be output during
     meta-data emission, so we don't need to do it here.  */
  decl = get_objc_string_decl (ident, class_names);
  eh_id = add_objc_string (ident, class_names);
  if (!decl)
    {
      /* Not found ... so we need to build it - from the freshly-entered id.  */
      decl = get_objc_string_decl (ident, class_names);
      str = my_build_string (IDENTIFIER_LENGTH (ident) + 1,
			     IDENTIFIER_POINTER (ident));
      /* We have to finalize this var here, because this might be called after
	 all the other metadata strings have been emitted.  */
      finish_var_decl (decl, str);
    }
  return eh_id;
}

static tree
objc_eh_personality (void)
{
  if (!objc_eh_personality_decl)
#ifndef OBJCPLUS
    objc_eh_personality_decl = build_personality_function  ("gnu_objc");
#else
    objc_eh_personality_decl = build_personality_function  ("gxx");
#endif
  return objc_eh_personality_decl;
}

/* -- interfaces --- */

static tree
build_throw_stmt (location_t loc, tree throw_expr, bool rethrown ATTRIBUTE_UNUSED)
{
  tree t;
  vec<tree, va_gc> *parms;
  vec_alloc (parms, 1);
  /* A throw is just a call to the runtime throw function with the
     object as a parameter.  */
  parms->quick_push (throw_expr);
  t = build_function_call_vec (loc, vNULL, objc_exception_throw_decl, parms,
			       NULL);
  vec_free (parms);
  return add_stmt (t);
}

/* Build __builtin_eh_pointer.  */

static tree
objc_build_exc_ptr (struct objc_try_context **x ATTRIBUTE_UNUSED)
{
  tree t;
  t = builtin_decl_explicit (BUILT_IN_EH_POINTER);
  t = build_call_expr (t, 1, integer_zero_node);
  return fold_convert (objc_object_type, t);
}

static tree
begin_catch (struct objc_try_context **cur_try_context, tree type,
	     tree decl, tree compound, bool ellipsis ATTRIBUTE_UNUSED)
{
  tree t;
  /* Record the data for the catch in the try context so that we can
     finalize it later.  */
  if (ellipsis)
    t = build_stmt (input_location, CATCH_EXPR, NULL, compound);
  else
    t = build_stmt (input_location, CATCH_EXPR, type, compound);
  (*cur_try_context)->current_catch = t;

  /* Initialize the decl from the EXC_PTR_EXPR we get from the runtime.  */
  t = objc_build_exc_ptr (cur_try_context);
  t = convert (TREE_TYPE (decl), t);
  return build2 (MODIFY_EXPR, void_type_node, decl, t);
}

static void
finish_catch (struct objc_try_context **cur_try_context, tree current_catch)
{
  append_to_statement_list (current_catch, &((*cur_try_context)->catch_list));
}

static tree
finish_try_stmt (struct objc_try_context **cur_try_context)
{
  struct objc_try_context *c = *cur_try_context;
  tree stmt = c->try_body;
  if (c->catch_list)
    stmt = build_stmt (c->try_locus, TRY_CATCH_EXPR, stmt, c->catch_list);
  if (c->finally_body)
    stmt = build_stmt (c->try_locus, TRY_FINALLY_EXPR, stmt, c->finally_body);
  return stmt;
}

#include "gt-objc-objc-gnu-runtime-abi-01.h"
