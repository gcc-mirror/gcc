/* Next Runtime (ABI-0/1) private.
   Copyright (C) 2011-2024 Free Software Foundation, Inc.
   Contributed by Iain Sandoe (split from objc-act.cc)

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

/* This implements the original NeXT ABI (0) used for m32 code and
   indicated by module version 6.  It also implements the small number
   of additions made for properties and optional protocol methods as
   ABI=1 (module version 7).  */

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"
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
#include "opts.h"

/* When building Objective-C++, we are not linking against the C
   front-end and so need to replicate the C tree-construction
   functions in some way.  */
#ifdef OBJCPLUS
#define OBJCP_REMAP_FUNCTIONS
#include "objcp-decl.h"
#endif  /* OBJCPLUS */

#include "target.h"
#include "c-family/c-target.h"
#include "tree-iterator.h"

#include "objc-runtime-hooks.h"
#include "objc-runtime-shared-support.h"
#include "objc-next-metadata-tags.h"
#include "objc-encoding.h"

/* NeXT ABI 0 and 1 private definitions.  */
#define DEF_CONSTANT_STRING_CLASS_NAME "NSConstantString"

#define TAG_GETCLASS			"objc_getClass"
#define TAG_GETMETACLASS		"objc_getMetaClass"

#define TAG_MSGSEND			"objc_msgSend"
#define TAG_MSGSENDSUPER		"objc_msgSendSuper"
#define TAG_MSGSEND_STRET		"objc_msgSend_stret"
#define TAG_MSGSENDSUPER_STRET		"objc_msgSendSuper_stret"

/* NeXT-specific tags.  */

#define TAG_MSGSEND_NONNIL		"objc_msgSendNonNil"
#define TAG_MSGSEND_NONNIL_STRET	"objc_msgSendNonNil_stret"
#define TAG_EXCEPTIONEXTRACT		"objc_exception_extract"
#define TAG_EXCEPTIONTRYENTER		"objc_exception_try_enter"
#define TAG_EXCEPTIONTRYEXIT		"objc_exception_try_exit"
#define TAG_EXCEPTIONMATCH		"objc_exception_match"
#define TAG_SETJMP			"_setjmp"

#define TAG_ASSIGNIVAR			"objc_assign_ivar"
#define TAG_ASSIGNGLOBAL		"objc_assign_global"
#define TAG_ASSIGNSTRONGCAST		"objc_assign_strongCast"

/* Branch entry points.  All that matters here are the addresses;
   functions with these names do not really exist in libobjc.  */

#define TAG_MSGSEND_FAST		"objc_msgSend_Fast"
#define TAG_ASSIGNIVAR_FAST		"objc_assign_ivar_Fast"

/* The version identifies which language generation and runtime the
   module (file) was compiled for, and is recorded in the module
   descriptor.  */
#define OBJC_VERSION			(flag_objc_abi >= 1 ? 7 : 6)

#define UTAG_CLASS_EXT			"_objc_class_ext"
#define UTAG_PROPERTY_LIST		"_prop_list_t"
#define UTAG_PROTOCOL_EXT		"_objc_protocol_extension"

#define CLS_HAS_CXX_STRUCTORS		0x2000L

static void next_runtime_01_initialize (void);

static tree next_runtime_abi_01_super_superclassfield_id (void);

static tree next_runtime_abi_01_class_decl (tree);
static tree next_runtime_abi_01_metaclass_decl (tree);
static tree next_runtime_abi_01_category_decl (tree);
static tree next_runtime_abi_01_protocol_decl (tree);
static tree next_runtime_abi_01_string_decl (tree, const char *, string_section);

static tree next_runtime_abi_01_get_class_reference (tree);
static tree next_runtime_abi_01_build_selector_reference (location_t, tree, tree);
static tree next_runtime_abi_01_get_protocol_reference (location_t, tree);
static tree next_runtime_abi_01_build_ivar_ref (location_t, tree, tree);
static tree next_runtime_abi_01_get_class_super_ref (location_t, struct imp_entry *, bool);
static tree next_runtime_abi_01_get_category_super_ref (location_t, struct imp_entry *, bool);

static tree next_runtime_abi_01_receiver_is_class_object (tree);
static void next_runtime_abi_01_get_arg_type_list_base (vec<tree, va_gc> **,
							tree, int, int);
static tree next_runtime_abi_01_build_objc_method_call (location_t, tree, tree,
							tree, tree, tree, int);
static bool next_runtime_abi_01_setup_const_string_class_decl (void);
static tree next_runtime_abi_01_build_const_string_constructor (location_t, tree, int);

static void objc_generate_v1_next_metadata (void);

static void build_next_objc_exception_stuff (void);
static tree objc_eh_runtime_type (tree type);
static tree objc_eh_personality (void);
static tree build_throw_stmt (location_t, tree, bool);
static tree objc_build_exc_ptr (struct objc_try_context **);
static tree begin_catch (struct objc_try_context **, tree, tree, tree, bool);
static void finish_catch (struct objc_try_context **, tree);
static tree finish_try_stmt (struct objc_try_context **);

bool
objc_next_runtime_abi_01_init (objc_runtime_hooks *rthooks)
{
  if (flag_objc_exceptions
      && !flag_objc_sjlj_exceptions)
    {
      warning_at (UNKNOWN_LOCATION, OPT_Wall,
		"%<-fobjc-sjlj-exceptions%> is the only supported exceptions "
		"system for %<-fnext-runtime%> with %<-fobjc-abi-version%> "
		"argument less than 2");
    }

  rthooks->initialize = next_runtime_01_initialize;
  rthooks->default_constant_string_class_name = DEF_CONSTANT_STRING_CLASS_NAME;
  rthooks->tag_getclass = TAG_GETCLASS;
  rthooks->super_superclassfield_ident = next_runtime_abi_01_super_superclassfield_id;

  rthooks->class_decl = next_runtime_abi_01_class_decl;
  rthooks->metaclass_decl = next_runtime_abi_01_metaclass_decl;
  rthooks->category_decl = next_runtime_abi_01_category_decl;
  rthooks->protocol_decl = next_runtime_abi_01_protocol_decl;
  rthooks->string_decl = next_runtime_abi_01_string_decl;

  rthooks->get_class_reference = next_runtime_abi_01_get_class_reference;
  rthooks->build_selector_reference = next_runtime_abi_01_build_selector_reference;
  rthooks->get_protocol_reference = next_runtime_abi_01_get_protocol_reference;
  rthooks->build_ivar_reference = next_runtime_abi_01_build_ivar_ref;
  rthooks->get_class_super_ref = next_runtime_abi_01_get_class_super_ref;
  rthooks->get_category_super_ref = next_runtime_abi_01_get_category_super_ref;

  rthooks->receiver_is_class_object = next_runtime_abi_01_receiver_is_class_object;
  rthooks->get_arg_type_list_base = next_runtime_abi_01_get_arg_type_list_base;
  rthooks->build_objc_method_call = next_runtime_abi_01_build_objc_method_call;

  rthooks->setup_const_string_class_decl =
				next_runtime_abi_01_setup_const_string_class_decl;
  rthooks->build_const_string_constructor =
				next_runtime_abi_01_build_const_string_constructor;

  rthooks->build_throw_stmt = build_throw_stmt;
  rthooks->build_exc_ptr = objc_build_exc_ptr;
  rthooks->begin_catch = begin_catch;
  rthooks->finish_catch = finish_catch;
  rthooks->finish_try_stmt = finish_try_stmt;

  rthooks->generate_metadata = objc_generate_v1_next_metadata;
  return true;
}

/* We need a way to convey what kind of meta-data are represented by a
   given variable, since each type is expected (by the runtime) to be
   found in a specific named section.  The solution must be usable
   with LTO.

   The scheme used for NeXT ABI 0/1 (partial matching of variable
   names) is not satisfactory for LTO & ABI-2.  We now tag ObjC
   meta-data with identification attributes in the front end.  The
   back-end may choose to act on these as it requires.  */

static void
next_runtime_abi_01_init_metadata_attributes (void)
{
  if (!objc_meta)
    objc_meta = get_identifier ("OBJC1META");

  if (!meta_base)
    meta_base = get_identifier ("V1_BASE");

  meta_class = get_identifier ("V1_CLAS");
  meta_metaclass = get_identifier ("V1_META");
  meta_category = get_identifier ("V1_CATG");
  meta_protocol = get_identifier ("V1_PROT");

  meta_clac_vars = get_identifier ("V1_CLCV");
  meta_clai_vars = get_identifier ("V1_CLIV");

  meta_clac_meth = get_identifier ("V1_CLCM");
  meta_clai_meth = get_identifier ("V1_CLIM");
  meta_catc_meth = get_identifier ("V1_CACM");
  meta_cati_meth = get_identifier ("V1_CAIM");
  meta_proto_cls_meth = get_identifier ("V1_PCLM");
  meta_proto_nst_meth = get_identifier ("V1_PNSM");

  meta_clas_prot = get_identifier ("V1_CLPR");
  meta_catg_prot = get_identifier ("V1_CAPR");

  meta_class_reference = get_identifier ("V1_CLRF");
  meta_proto_ref = get_identifier ("V1_PRFS");
  meta_sel_refs = get_identifier ("V1_SRFS");

  meta_class_name = get_identifier ("V1_CLSN");
  meta_meth_name = get_identifier ("V1_METN");
  meta_meth_type = get_identifier ("V1_METT");
  meta_prop_name_attr = get_identifier ("V1_STRG");

  meta_modules = get_identifier ("V1_MODU");
  meta_symtab = get_identifier ("V1_SYMT");
  meta_info = get_identifier ("V1_INFO");

  meta_proplist = get_identifier ("V1_PLST");
  meta_protocol_extension = get_identifier ("V1_PEXT");
  meta_class_extension = get_identifier ("V1_CEXT");

  meta_const_str = get_identifier ("V1_CSTR");
}

static void build_v1_class_template (void);
static void build_v1_category_template (void);
static void build_v1_protocol_template (void);

static void next_runtime_01_initialize (void)
{
  tree type;

#ifdef OBJCPLUS
  /* For all NeXT objc ABIs -fobjc-call-cxx-cdtors is on by
     default.  */
  if (!OPTION_SET_P (flag_objc_call_cxx_cdtors))
    global_options.x_flag_objc_call_cxx_cdtors = 1;
#endif

  /* Set up attributes to be attached to the meta-data so that they
     will be placed in the correct sections.  */
  next_runtime_abi_01_init_metadata_attributes ();

  if (flag_objc_abi >= 1)
    objc_prop_list_ptr = build_pointer_type (xref_tag (RECORD_TYPE,
					     get_identifier ("_prop_list_t")));

 /* Declare type of selector-objects that represent an operation
    name.  */
  /* `struct objc_selector *' */
  objc_selector_type = build_pointer_type (xref_tag (RECORD_TYPE,
					   get_identifier (TAG_SELECTOR)));

  /* SEL typedef.  */
  type = lang_hooks.decls.pushdecl (build_decl (input_location,
						TYPE_DECL,
						objc_selector_name,
						objc_selector_type));
  suppress_warning (type);

  build_v1_class_template ();
  build_super_template ();
  build_v1_protocol_template ();
  build_v1_category_template ();

  /* NB: In order to call one of the ..._stret (struct-returning)
     functions, the function *MUST* first be cast to a signature that
     corresponds to the actual ObjC method being invoked.  This is
     what is done by the build_objc_method_call() routine below.  */

  /* id objc_msgSend (id, SEL, ...); */
  /* id objc_msgSendNonNil (id, SEL, ...); */
  /* id objc_msgSend_stret (id, SEL, ...); */
  /* id objc_msgSendNonNil_stret (id, SEL, ...); */
  type = build_varargs_function_type_list (objc_object_type,
					   objc_object_type,
					   objc_selector_type,
					   NULL_TREE);

  umsg_decl = add_builtin_function (TAG_MSGSEND,
				    type, 0, NOT_BUILT_IN,
				    NULL, NULL_TREE);

  umsg_nonnil_decl = add_builtin_function (TAG_MSGSEND_NONNIL,
					   type, 0, NOT_BUILT_IN,
					    NULL, NULL_TREE);

  umsg_stret_decl = add_builtin_function (TAG_MSGSEND_STRET,
					  type, 0, NOT_BUILT_IN,
					  NULL, NULL_TREE);

  umsg_nonnil_stret_decl = add_builtin_function (TAG_MSGSEND_NONNIL_STRET,
						 type, 0, NOT_BUILT_IN,
						 NULL, NULL_TREE);

  /* These can throw, because the function that gets called can throw
     in Obj-C++, or could itself call something that can throw even in
     Obj-C.  */
  TREE_NOTHROW (umsg_decl) = 0;
  TREE_NOTHROW (umsg_nonnil_decl) = 0;
  TREE_NOTHROW (umsg_stret_decl) = 0;
  TREE_NOTHROW (umsg_nonnil_stret_decl) = 0;

 /* id objc_msgSend_Fast (id, SEL, ...)
	   __attribute__ ((hard_coded_address (OFFS_MSGSEND_FAST))); */
#ifdef OFFS_MSGSEND_FAST
  umsg_fast_decl = add_builtin_function (TAG_MSGSEND_FAST,
					     type, 0, NOT_BUILT_IN,
					     NULL, NULL_TREE);
  TREE_NOTHROW (umsg_fast_decl) = 0;
  DECL_ATTRIBUTES (umsg_fast_decl)
	= tree_cons (get_identifier ("hard_coded_address"),
		     build_int_cst (NULL_TREE, OFFS_MSGSEND_FAST),
		     NULL_TREE);
#else
  /* No direct dispatch available.  */
  umsg_fast_decl = umsg_decl;
#endif

  /* id objc_msgSendSuper (struct objc_super *, SEL, ...); */
  /* id objc_msgSendSuper_stret (struct objc_super *, SEL, ...); */
  type = build_varargs_function_type_list (objc_object_type,
                                            objc_super_type,
                                            objc_selector_type,
                                            NULL_TREE);
  umsg_super_decl = add_builtin_function (TAG_MSGSENDSUPER,
					      type, 0, NOT_BUILT_IN,
					      NULL, NULL_TREE);
  umsg_super_stret_decl = add_builtin_function (TAG_MSGSENDSUPER_STRET,
						    type, 0, NOT_BUILT_IN, 0,
						    NULL_TREE);
  TREE_NOTHROW (umsg_super_decl) = 0;
  TREE_NOTHROW (umsg_super_stret_decl) = 0;

  type = build_function_type_list (objc_object_type,
                                   const_string_type_node,
                                   NULL_TREE);

  /* id objc_getClass (const char *); */
  objc_get_class_decl
    = add_builtin_function (TAG_GETCLASS, type, 0, NOT_BUILT_IN,
			    NULL, NULL_TREE);

  /* id objc_getMetaClass (const char *); */
  objc_get_meta_class_decl
    = add_builtin_function (TAG_GETMETACLASS, type, 0, NOT_BUILT_IN, NULL, NULL_TREE);

  /* This is the type of all of the following functions
     objc_copyStruct().  */
  type = build_function_type_list (void_type_node,
				   ptr_type_node,
				   const_ptr_type_node,
				   ptrdiff_type_node,
				   boolean_type_node,
				   boolean_type_node,
				   NULL_TREE);
  /* Declare the following function:
	 void
         objc_copyStruct (void *destination, const void *source,
	                  ptrdiff_t size, BOOL is_atomic, BOOL has_strong); */
  objc_copyStruct_decl = add_builtin_function ("objc_copyStruct",
						   type, 0, NOT_BUILT_IN,
						   NULL, NULL_TREE);
  TREE_NOTHROW (objc_copyStruct_decl) = 0;
  objc_getPropertyStruct_decl = NULL_TREE;
  objc_setPropertyStruct_decl = NULL_TREE;

  build_next_objc_exception_stuff ();
  if (flag_objc_exceptions && !flag_objc_sjlj_exceptions)
    using_eh_for_cleanups ();
  lang_hooks.eh_runtime_type = objc_eh_runtime_type;
  lang_hooks.eh_personality = objc_eh_personality;
}

/* --- templates --- */

/* struct _objc_class
   {
     struct _objc_class *isa;
     struct _objc_class *super_class;
     char *name;
     long version;
     long info;
     long instance_size;
     struct _objc_ivar_list *ivars;
     struct _objc_method_list *methods;
     struct objc_cache *cache;
     struct _objc_protocol_list *protocols;
   #if ABI=1
     const char *ivar_layout;
     struct _objc_class_ext *ext;
   #else
     void *sel_id;
     void *gc_object_type;
    #endif
   }; */

/* The 'sel_id' & 'gc_object_type' fields are not used by the NeXT
   runtime.  We generate them for ABI==0 to maintain backward binary
   compatibility.  */

static void
build_v1_class_template (void)
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

  /* struct objc_cache *cache; */
  ptype = build_pointer_type (xref_tag (RECORD_TYPE,
					    get_identifier ("objc_cache")));
  add_field_decl (ptype, "cache", &chain);

  /* struct _objc_protocol **protocol_list; */
  ptype = build_pointer_type (build_pointer_type
			      (xref_tag (RECORD_TYPE,
					 get_identifier (UTAG_PROTOCOL))));
  add_field_decl (ptype, "protocol_list", &chain);

  if (flag_objc_abi >= 1)
    {
      /* const char *ivar_layout; */
      add_field_decl (const_string_type_node, "ivar_layout", &chain);

      /* struct _objc_class_ext *ext; */
      ptype = build_pointer_type (xref_tag (RECORD_TYPE,
					    get_identifier (UTAG_CLASS_EXT)));
      add_field_decl (ptype, "ext", &chain);
    }
  else
    {
      /* void *sel_id; */
      add_field_decl (build_pointer_type (void_type_node), "sel_id", &chain);
      /* void *gc_object_type; */
      add_field_decl (build_pointer_type (void_type_node), "gc_object_type",
		      &chain);
    }

  objc_finish_struct (objc_class_template, decls);
}

/* struct _objc_category
   {
     char *category_name;
     char *class_name;
     struct _objc_method_list *instance_methods;
     struct _objc_method_list *class_methods;
     struct _objc_protocol_list *protocols;
   #if ABI=1
     uint32_t size;	// sizeof (struct _objc_category)
     struct _objc_property_list *instance_properties;  // category's own @property decl.
   #endif
   };   */

static void
build_v1_category_template (void)
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

  if (flag_objc_abi >= 1)
    {
      add_field_decl (integer_type_node, "size", &chain);

      /* struct _objc_property_list *instance_properties;
         This field describes a category's @property declarations.
         Properties from inherited protocols are not included.  */
      ptype = build_pointer_type (xref_tag (RECORD_TYPE,
					    get_identifier (UTAG_PROPERTY_LIST)));
      add_field_decl (ptype, "instance_properties", &chain);
    }
  objc_finish_struct (objc_category_template, decls);
}

/* Begin code generation for protocols...
   Modified for ObjC #1 extensions.  */

/* struct _objc_protocol
   {
   #if ABI=1
     struct _objc_protocol_extension *isa;
   #else
     struct _objc_class *isa;
   #endif

     char *protocol_name;
     struct _objc_protocol **protocol_list;
     struct _objc__method_prototype_list *instance_methods;
     struct _objc__method_prototype_list *class_methods;
   }; */

static void
build_v1_protocol_template (void)
{
  tree ptype, decls, *chain = NULL;

  objc_protocol_template = objc_start_struct (get_identifier (UTAG_PROTOCOL));

  if (flag_objc_abi >= 1)
    /* struct _objc_protocol_extension *isa; */
    ptype = build_pointer_type (xref_tag (RECORD_TYPE,
					  get_identifier (UTAG_PROTOCOL_EXT)));
  else
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

/* --- names, decls identifiers --- */

static tree
next_runtime_abi_01_super_superclassfield_id (void)
{
  if (!super_superclassfield_id)
    super_superclassfield_id = get_identifier ("super_class");
  return super_superclassfield_id;
}

static tree
next_runtime_abi_01_class_decl (tree klass)
{
  tree decl;
  char buf[BUFSIZE];
  snprintf (buf, BUFSIZE, "_OBJC_Class_%s",
	    IDENTIFIER_POINTER (CLASS_NAME (klass)));
  decl = start_var_decl (objc_class_template, buf);
  OBJCMETA (decl, objc_meta, meta_class);
  return decl;
}

static tree
next_runtime_abi_01_metaclass_decl (tree klass)
{
  tree decl;
  char buf[BUFSIZE];
  snprintf (buf, BUFSIZE, "_OBJC_MetaClass_%s",
	    IDENTIFIER_POINTER (CLASS_NAME (klass)));
  decl = start_var_decl (objc_class_template, buf);
  OBJCMETA (decl, objc_meta, meta_metaclass);
  return decl;
}

static tree
next_runtime_abi_01_category_decl (tree klass)
{
  tree decl;
  char buf[BUFSIZE];
  snprintf (buf, BUFSIZE, "_OBJC_Category_%s_on_%s",
	    IDENTIFIER_POINTER (CLASS_SUPER_NAME (klass)),
	    IDENTIFIER_POINTER (CLASS_NAME (klass)));
  decl = start_var_decl (objc_category_template, buf);
  OBJCMETA (decl, objc_meta, meta_category);
  return decl;
}

static tree
next_runtime_abi_01_protocol_decl (tree p)
{
  tree decl;
  char buf[BUFSIZE];

  /* static struct _objc_protocol _OBJC_Protocol_<mumble>; */

  snprintf (buf, BUFSIZE, "_OBJC_Protocol_%s",
	    IDENTIFIER_POINTER (PROTOCOL_NAME (p)));
  decl = start_var_decl (objc_protocol_template, buf);
  OBJCMETA (decl, objc_meta, meta_protocol);
  return decl;
}

static tree
next_runtime_abi_01_string_decl (tree type, const char *name,  string_section where)
{
  tree var = start_var_decl (type, name);
  switch (where)
    {
      case class_names:
	OBJCMETA (var, objc_meta, meta_class_name);
	break;
      case meth_var_names:
	OBJCMETA (var, objc_meta, meta_meth_name);
	break;
      case meth_var_types:
	OBJCMETA (var, objc_meta, meta_meth_type);
	break;
      case prop_names_attr:
	OBJCMETA (var, objc_meta, meta_prop_name_attr);
	break;
      default:
	OBJCMETA (var, objc_meta, meta_base);
	break;
    }
  return var;
}

/* --- entry --- */

static GTY(()) int class_reference_idx;

static tree
build_class_reference_decl (void)
{
  tree decl;
  char buf[BUFSIZE];

  sprintf (buf, "_OBJC_ClassRefs_%d", class_reference_idx++);
  decl = start_var_decl (objc_class_type, buf);

  return decl;
}

static tree
next_runtime_abi_01_get_class_reference (tree ident)
{
  if (!flag_zero_link)
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
				my_build_string_pointer
				(IDENTIFIER_LENGTH (ident) + 1,
				 IDENTIFIER_POINTER (ident)));

      return build_function_call (input_location, objc_get_class_decl, params);
    }
}

/* Used by build_function_type_for_method.  Append the types for
   receiver & _cmd at the start of a method argument list to ARGTYPES.
   CONTEXT is either METHOD_DEF or METHOD_REF, saying whether we are
   trying to define a method or call one.  SUPERFLAG says this is for a
   send to super.  METH may be NULL, in the case that there is no
   prototype.  */

static void
next_runtime_abi_01_get_arg_type_list_base (vec<tree, va_gc> **argtypes,
					    tree meth, int context,
					    int superflag)
{
  tree receiver_type;

  if (superflag)
    receiver_type = objc_super_type;
  else if (context == METHOD_DEF && TREE_CODE (meth) == INSTANCE_METHOD_DECL)
    receiver_type = objc_instance_type;
  else
    receiver_type = objc_object_type;

  vec_safe_push (*argtypes, receiver_type);
  /* Selector type - will eventually change to `int'.  */
  vec_safe_push (*argtypes, objc_selector_type);
}

static tree
next_runtime_abi_01_receiver_is_class_object (tree receiver)
{
  if (VAR_P (receiver)
      && IS_CLASS (TREE_TYPE (receiver)))
    {
      /* The receiver is a variable created by build_class_reference_decl.  */
      tree chain = cls_ref_chain ;
      /* Look up the identifier in the relevant chain.  */
      for (; chain; chain = TREE_CHAIN (chain))
	if (TREE_PURPOSE (chain) == receiver)
	  return TREE_VALUE (chain);
    }
  return NULL_TREE;
}

static tree
build_selector_reference_decl (tree ident)
{
  tree decl;
  char *t, buf[BUFSIZE];

  snprintf (buf, BUFSIZE, "_OBJC_SelRef_%s", IDENTIFIER_POINTER (ident));
  t = buf;
  while (*t)
    {
      if (*t==':')
        *t = '$'; /* Underscore would clash between foo:bar and foo_bar.  */
      t++;
    }
  decl = start_var_decl (objc_selector_type, buf);
  OBJCMETA (decl, objc_meta, meta_sel_refs);
  return decl;
}

static tree
next_runtime_abi_01_build_selector_reference (location_t loc ATTRIBUTE_UNUSED,
					      tree ident,
					      tree proto ATTRIBUTE_UNUSED)
{
  tree *chain = &sel_ref_chain;
  tree expr;

  while (*chain)
    {
      if (TREE_VALUE (*chain) == ident)
	return TREE_PURPOSE (*chain);

      chain = &TREE_CHAIN (*chain);
    }

  expr = build_selector_reference_decl (ident);

  *chain = tree_cons (expr, ident, NULL_TREE);

  return expr;
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
  tree sender, sender_cast, method, t;
  tree rcv_p = (super_flag ? objc_super_type : objc_object_type);
  vec<tree, va_gc> *parms;
  unsigned nparm = (method_params ? list_length (method_params) : 0);

  /* If a prototype for the method to be called exists, then cast
     the sender's return type and arguments to match that of the method.
     Otherwise, leave sender as is.  */
  tree ret_type
    = (method_prototype
       ? TREE_VALUE (TREE_TYPE (method_prototype))
       : objc_object_type);
  tree ftype = build_function_type_for_method (ret_type, method_prototype,
					       METHOD_REF, super_flag);

  if (method_prototype && METHOD_TYPE_ATTRIBUTES (method_prototype))
    ftype = build_type_attribute_variant (ftype,
					  METHOD_TYPE_ATTRIBUTES
					  (method_prototype));

  sender_cast = build_pointer_type (ftype);

  lookup_object = build_c_cast (loc, rcv_p, lookup_object);

  if (error_operand_p (lookup_object))
    return error_mark_node;

  /* Use SAVE_EXPR to avoid evaluating the receiver twice.  */
  lookup_object = save_expr (lookup_object);

  /* Param list + 2 slots for object and selector.  */
  vec_alloc (parms, nparm + 2);

  /* If we are returning a struct in memory, and the address
     of that memory location is passed as a hidden first
     argument, then change which messenger entry point this
     expr will call.  NB: Note that sender_cast remains
     unchanged (it already has a struct return type).  */
  if (!targetm.calls.struct_value_rtx (0, 0)
      && (TREE_CODE (ret_type) == RECORD_TYPE
	  || TREE_CODE (ret_type) == UNION_TYPE)
      && targetm.calls.return_in_memory (ret_type, 0))
    sender = (super_flag ? umsg_super_stret_decl
			 : flag_nil_receivers ? umsg_stret_decl
					      : umsg_nonnil_stret_decl);
  else
    sender = (super_flag ? umsg_super_decl
			 : (flag_nil_receivers  ? (flag_objc_direct_dispatch
							? umsg_fast_decl
							: umsg_decl)
						: umsg_nonnil_decl));
  method = build_fold_addr_expr_loc (loc, sender);

  /* Pass the object to the method.  */
  parms->quick_push (lookup_object);
  /* Pass the selector to the method.  */
  parms->quick_push (selector);
  /* Now append the remainder of the parms.  */
  if (nparm)
    for (; method_params; method_params = TREE_CHAIN (method_params))
      parms->quick_push (TREE_VALUE (method_params));

  /* Build an obj_type_ref, with the correct cast for the method call.  */
  t = build3 (OBJ_TYPE_REF, sender_cast, method,
	      lookup_object, build_int_cst (TREE_TYPE (lookup_object), 0));
  t = build_function_call_vec (loc, vNULL, t, parms, NULL);
  vec_free (parms);
  return t;
}

static tree
next_runtime_abi_01_build_objc_method_call (location_t loc,
					    tree method_prototype,
					    tree receiver,
					    tree rtype ATTRIBUTE_UNUSED,
					    tree sel_name,
					    tree method_params,
					    int super)
{
  tree selector = next_runtime_abi_01_build_selector_reference (loc, sel_name,
								NULL_TREE);

  return build_objc_method_call (loc, super, method_prototype,
				 receiver, selector, method_params);
}

static tree
next_runtime_abi_01_get_protocol_reference (location_t loc, tree p)
{
  tree expr;

  if (!PROTOCOL_FORWARD_DECL (p))
    PROTOCOL_FORWARD_DECL (p) = next_runtime_abi_01_protocol_decl (p);

  expr = build_unary_op (loc, ADDR_EXPR, PROTOCOL_FORWARD_DECL (p), 0);
  return convert (objc_protocol_type, expr);
}

/* For ABI 0/1 and IVAR is just a fixed offset in the class struct.  */

static tree
next_runtime_abi_01_build_ivar_ref (location_t loc ATTRIBUTE_UNUSED,
				   tree base, tree id)
{
  return objc_build_component_ref (base, id);
}

/* We build super class references as we need them (but keep them once
   built for the sake of efficiency).  */

static tree
next_runtime_abi_01_get_class_super_ref (location_t loc ATTRIBUTE_UNUSED,
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
next_runtime_abi_01_get_category_super_ref (location_t loc ATTRIBUTE_UNUSED,
					   struct imp_entry *imp, bool inst_meth)
{
  tree super_name = CLASS_SUPER_NAME (imp->imp_template);
  tree super_class;

  if (!flag_zero_link)
    {
      super_class = objc_get_class_reference (super_name);

      if (!inst_meth)

	/* If we are in a class method, we must retrieve the
	   _metaclass_ for the current class, pointed at by
	   the class's "isa" pointer.  The following assumes that
	   "isa" is the first ivar in a class (which it must be).  */
	   super_class =
		build_indirect_ref (input_location,
				    build_c_cast (input_location,
					build_pointer_type (objc_class_type),
					super_class),
				    RO_UNARY_STAR);
      return super_class;
    }

  /* else do it the slow way.  */
  add_class_reference (super_name);
  super_class = (inst_meth ? objc_get_class_decl : objc_get_meta_class_decl);
  super_name = my_build_string_pointer (IDENTIFIER_LENGTH (super_name) + 1,
					IDENTIFIER_POINTER (super_name));
  /* super_class = objc_get{Meta}Class("CLASS_SUPER_NAME"); */
  return build_function_call (input_location,
			      super_class,
			      build_tree_list (NULL_TREE, super_name));
}

static bool
next_runtime_abi_01_setup_const_string_class_decl (void)
{
  if (!constant_string_global_id)
    {
      /* Hopefully, this should not represent a serious limitation.  */
      char buf[BUFSIZE];
      snprintf (buf, BUFSIZE, "_%sClassReference", constant_string_class_name);
      constant_string_global_id = get_identifier (buf);
    }

  string_class_decl = lookup_name (constant_string_global_id);

  return (string_class_decl != NULL_TREE);
}

static tree
next_runtime_abi_01_build_const_string_constructor (location_t loc, tree string,
						   int length)
{
  tree constructor, fields, var;
  vec<constructor_elt, va_gc> *v = NULL;

  /* NeXT:   (NSConstantString *) & ((__builtin_ObjCString) { isa, string, length })   */
  fields = TYPE_FIELDS (internal_const_str_type);
  CONSTRUCTOR_APPEND_ELT (v, fields,
			  build_unary_op (loc, ADDR_EXPR, string_class_decl, 0));

  fields = DECL_CHAIN (fields);
  CONSTRUCTOR_APPEND_ELT (v, fields,
			  build_unary_op (loc, ADDR_EXPR, string, 1));

  /* ??? check if this should be long.  */
  fields = DECL_CHAIN (fields);
  CONSTRUCTOR_APPEND_ELT (v, fields, build_int_cst (NULL_TREE, length));
  constructor = objc_build_constructor (internal_const_str_type, v);

  var = build_decl (input_location, CONST_DECL, NULL, TREE_TYPE (constructor));
  DECL_INITIAL (var) = constructor;
  TREE_STATIC (var) = 1;
  DECL_CONTEXT (var) = NULL;
  OBJCMETA (var, objc_meta, meta_const_str);
  return var;
}

/* ---  metadata templates --- */

/* This routine builds the following type:
   struct _prop_t {
     const char * const name;			// property name
     const char * const attributes;		// comma-delimited, encoded,
						// property attributes
   };
*/

static GTY(()) tree objc_v1_property_template;

static tree
build_v1_property_template (void)
{
  tree prop_record;
  tree decls, *chain = NULL;

  prop_record = objc_start_struct (get_identifier ("_prop_t"));
  /* const char * name */
  decls = add_field_decl (string_type_node, "name", &chain);

  /* const char * attribute */
  add_field_decl (string_type_node, "attribute", &chain);

  objc_finish_struct (prop_record, decls);
  return prop_record;
}

/* Build the following type:

  struct _objc_protocol_extension
    {
      uint32_t size;	// sizeof (struct _objc_protocol_extension)
      struct objc_method_list	*optional_instance_methods;
      struct objc_method_list   *optional_class_methods;
      struct objc_prop_list	*instance_properties;
    }
*/

static GTY(()) tree objc_protocol_extension_template;

static void
build_v1_objc_protocol_extension_template (void)
{
  tree decls, *chain = NULL;

  objc_protocol_extension_template =
	objc_start_struct (get_identifier (UTAG_PROTOCOL_EXT));

  /*  uint32_t size; */
  decls = add_field_decl (integer_type_node, "size", &chain);

  /* struct objc_method_list   *optional_instance_methods; */
  add_field_decl (objc_method_list_ptr, "optional_instance_methods", &chain);

  /* struct objc_method_list   *optional_class_methods; */
  add_field_decl (objc_method_list_ptr, "optional_class_methods", &chain);

  /* struct objc_prop_list     *instance_properties; */
  add_field_decl (objc_prop_list_ptr, "instance_properties", &chain);

  objc_finish_struct (objc_protocol_extension_template, decls);
}

/* This routine build following struct type:
   struct _objc_class_ext
     {
       uint32_t size;	// sizeof(struct _objc_class_ext)
       const char *weak_ivar_layout;
       struct _prop_list_t *properties;
     }
*/

static GTY(()) tree objc_class_ext_template;

static void
build_objc_class_ext_template (void)
{
  tree ptrt, decls, *chain = NULL;

  objc_class_ext_template = objc_start_struct (get_identifier (UTAG_CLASS_EXT));

  /* uint32_t size; */
  decls = add_field_decl (integer_type_node, "size", &chain);

  /* const char *weak_ivar_layout; */
  add_field_decl (const_string_type_node, "weak_ivar_layout", &chain);

  /* struct _prop_list_t *properties; */
  ptrt = build_pointer_type (xref_tag (RECORD_TYPE,
			     get_identifier(UTAG_PROPERTY_LIST)));
  add_field_decl (ptrt, "properties", &chain);

  objc_finish_struct (objc_class_ext_template, decls);
}

static void
build_metadata_templates (void)
{

  if (!objc_method_template)
    objc_method_template = build_method_template ();



}

/* --- emit metadata --- */

static tree
generate_v1_meth_descriptor_table (tree chain, tree protocol,
				   const char *prefix, tree attr)
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
  /* Get into the right section.  */
  OBJCMETA (decl, objc_meta, attr);
  finish_var_decl (decl, objc_build_constructor (method_list_template, v));
  return decl;
}

/* Build protocol ext =
   {size, opt_instance_meth, opt_class_meth, instance_props};
   or NULL_TREE if none are present.  */

static tree
generate_v1_objc_protocol_extension (tree proto_interface,
				     tree opt_instance_meth,
				     tree opt_class_meth,
				     tree instance_props)
{
  int size;
  location_t loc;
  vec<constructor_elt, va_gc> *v = NULL;
  tree decl, expr;
  char buf[BUFSIZE];

  /* If there are no extensions, then don't bother... */
  if (!opt_instance_meth && !opt_class_meth && !instance_props)
    return NULL_TREE;

  if (!objc_protocol_extension_template)
    build_v1_objc_protocol_extension_template ();

  /* uint32_t size */
  size = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (objc_protocol_extension_template));
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, build_int_cst (NULL_TREE, size));

  /* Try for meaningful diagnostics.  */
  loc = DECL_SOURCE_LOCATION (PROTOCOL_FORWARD_DECL (proto_interface));

  /* struct objc_method_list *optional_instance_methods; */
  if (opt_instance_meth)
    expr = convert (objc_method_list_ptr,
		    build_unary_op (loc, ADDR_EXPR, opt_instance_meth, 0));
  else
    expr = convert (objc_method_list_ptr, null_pointer_node);

  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, expr);

  /* struct objc_method_list *optional_class_methods; */
  if (opt_class_meth)
    expr = convert (objc_method_list_ptr,
		    build_unary_op (loc, ADDR_EXPR, opt_class_meth, 0));
  else
    expr = convert (objc_method_list_ptr, null_pointer_node);

  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, expr);
  /* struct objc_prop_list *instance_properties; */
  if (instance_props)
      expr = convert (objc_prop_list_ptr,
		      build_unary_op (loc, ADDR_EXPR, instance_props, 0));
  else
    expr = convert (objc_prop_list_ptr, null_pointer_node);

  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, expr);
  snprintf (buf, BUFSIZE, "_OBJC_ProtocolExt_%s",
	    IDENTIFIER_POINTER (PROTOCOL_NAME (proto_interface)));

  decl = start_var_decl (objc_protocol_extension_template, buf);
  expr = objc_build_constructor (TREE_TYPE (decl), v);
  OBJCMETA (decl, objc_meta, meta_protocol_extension);
  finish_var_decl (decl, expr);
  return decl;
}

/* This routine builds the following type:
   struct _prop_list_t {
     uint32_t entsize;			// sizeof (struct _prop_t)
     uint32_t prop_count;
     struct _prop_t prop_list [prop_count];
   }
*/

static tree
build_v1_property_list_template (tree list_type, int size)
{
  tree property_list_t_record;
  tree array_type, decls, *chain = NULL;

  /* anonymous.  */
  property_list_t_record = objc_start_struct (NULL_TREE);

  /* uint32_t const entsize */
  decls = add_field_decl (integer_type_node, "entsize", &chain);

  /* int prop_count */
  add_field_decl (integer_type_node, "prop_count", &chain);

  /* struct _prop_t prop_list[]; */
  array_type = build_sized_array_type (list_type, size);
  add_field_decl (array_type, "prop_list", &chain);

  objc_finish_struct (property_list_t_record, decls);
  return property_list_t_record;
}

/* This routine builds the initializer list to initialize the
   'struct _prop_t prop_list[]'  field of 'struct _prop_list_t' meta-data. */

static tree
build_v1_property_table_initializer (tree type, tree context)
{
  tree x;
  vec<constructor_elt, va_gc> *inits = NULL;

  if (TREE_CODE (context) == PROTOCOL_INTERFACE_TYPE)
    x = CLASS_PROPERTY_DECL (context);
  else
    x = IMPL_PROPERTY_DECL (context);

  for (; x; x = TREE_CHAIN (x))
    {
      vec<constructor_elt, va_gc> *elemlist = NULL;
      tree attribute, name_ident = PROPERTY_NAME (x);

      CONSTRUCTOR_APPEND_ELT (elemlist, NULL_TREE,
			      add_objc_string (name_ident, prop_names_attr));

      attribute = objc_v2_encode_prop_attr (x);
      CONSTRUCTOR_APPEND_ELT (elemlist, NULL_TREE,
			      add_objc_string (attribute, prop_names_attr));

      CONSTRUCTOR_APPEND_ELT (inits, NULL_TREE,
			      objc_build_constructor (type, elemlist));
    }

  return objc_build_constructor (build_array_type (type, 0),inits);
}

/* This routine builds the 'struct _prop_list_t' variable declaration and
   initializes it with its initializer list. TYPE is 'struct _prop_list_t',
   NAME is the internal name of this variable, SIZE is number of properties
   for this class and LIST is the initializer list for its 'prop_list' field. */

static tree
generate_v1_property_table (tree context, tree klass_ctxt)
{
  tree x, decl, initlist, property_list_template;
  bool is_proto = false;
  vec<constructor_elt, va_gc> *inits = NULL;
  int init_val, size = 0;
  char buf[BUFSIZE];

  if (context)
    {
      gcc_assert (TREE_CODE (context) == PROTOCOL_INTERFACE_TYPE);
      x = CLASS_PROPERTY_DECL (context);
      is_proto = true;
    }
  else
    x = IMPL_PROPERTY_DECL (klass_ctxt);

  for (; x; x = TREE_CHAIN (x))
    size++;

  if (size == 0)
    return NULL_TREE;

  if (!objc_v1_property_template)
    objc_v1_property_template = build_v1_property_template ();

  property_list_template =
	build_v1_property_list_template (objc_v1_property_template,
					 size);
  initlist = build_v1_property_table_initializer (objc_v1_property_template,
						  is_proto ? context
							   : klass_ctxt);

  init_val = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (objc_v1_property_template));
  if (is_proto)
    snprintf (buf, BUFSIZE, "_OBJC_ProtocolPropList_%s",
	      IDENTIFIER_POINTER (PROTOCOL_NAME (context)));
  else
    snprintf (buf, BUFSIZE, "_OBJC_ClassPropList_%s",
	      IDENTIFIER_POINTER (CLASS_NAME (klass_ctxt)));

  decl = start_var_decl (property_list_template, buf);
  CONSTRUCTOR_APPEND_ELT (inits, NULL_TREE, build_int_cst (NULL_TREE, init_val));
  CONSTRUCTOR_APPEND_ELT (inits, NULL_TREE, build_int_cst (NULL_TREE, size));
  CONSTRUCTOR_APPEND_ELT (inits, NULL_TREE, initlist);
  x = objc_build_constructor (TREE_TYPE (decl), inits);
  OBJCMETA (decl, objc_meta, meta_proplist);
  finish_var_decl (decl, x);
  return decl;
}

static tree
generate_v1_protocol_list (tree i_or_p, tree klass_ctxt)
{
  tree array_type, ptype, refs_decl, lproto, e, plist, attr;
  int size = 0;
  vec<constructor_elt, va_gc> *v = NULL;
  char buf[BUFSIZE];

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
      attr = meta_proto_ref;
      break;
    case CLASS_INTERFACE_TYPE:
      snprintf (buf, BUFSIZE, "_OBJC_ClassProtocols_%s",
		IDENTIFIER_POINTER (CLASS_NAME (i_or_p)));
      attr = meta_clas_prot;
      break;
    case CATEGORY_INTERFACE_TYPE:
      snprintf (buf, BUFSIZE, "_OBJC_CategoryProtocols_%s_%s",
		IDENTIFIER_POINTER (CLASS_NAME (klass_ctxt)),
		IDENTIFIER_POINTER (CLASS_SUPER_NAME (klass_ctxt)));
      attr = meta_catg_prot;
      break;
    default:
      gcc_unreachable ();
    }

  ptype = build_pointer_type (objc_protocol_template);
  array_type = build_sized_array_type (ptype, size + 3);
  refs_decl = start_var_decl (array_type, buf);

  OBJCMETA (refs_decl, objc_meta, attr);
  finish_var_decl (refs_decl,
                   objc_build_constructor (TREE_TYPE (refs_decl), v));

  return refs_decl;
}

static tree
build_v1_protocol_initializer (tree type, tree protocol_name, tree protocol_list,
			       tree inst_methods, tree class_methods,
			       tree protocol_ext)
{
  tree expr, ttyp;
  location_t loc;
  vec<constructor_elt, va_gc> *inits = NULL;

  if (!objc_protocol_extension_template)
    build_v1_objc_protocol_extension_template ();

  /* TODO: find a better representation of location from the inputs.  */
  loc = UNKNOWN_LOCATION;
  ttyp = build_pointer_type (objc_protocol_extension_template);
  /* Instead of jamming the protocol version number into the isa, we pass
     either a pointer to the protocol extension - or NULL.  */
  if (protocol_ext)
    expr = convert (ttyp, build_unary_op (loc, ADDR_EXPR, protocol_ext, 0));
  else
    expr = convert (ttyp, null_pointer_node);

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

/* An updated version of generate_protocols () that emit the protocol
   extension for ABI=1.  */

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
   those symbols, then loops on them and fixes them up.  */

/* TODO: finish getting rid of passing stuff around in globals.  */

static GTY(()) tree V1_Protocol_OPT_NST_METHODS_decl;
static GTY(()) tree V1_Protocol_OPT_CLS_METHODS_decl;
static GTY(()) tree V1_ProtocolExt_decl;
static GTY(()) tree V1_Property_decl;

static void
generate_v1_protocols (void)
{
  tree p;

  /* If a protocol was directly referenced, pull in indirect references.  */
  for (p = protocol_chain; p; p = TREE_CHAIN (p))
    if (PROTOCOL_FORWARD_DECL (p) && PROTOCOL_LIST (p))
      generate_protocol_references (PROTOCOL_LIST (p));

  for (p = protocol_chain; p; p = TREE_CHAIN (p))
    {
      tree decl, encoding, initlist, protocol_name_expr;
      tree refs_type, refs_decl, refs_expr;
      location_t loc;
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

      UOBJC_INSTANCE_METHODS_decl =
	generate_v1_meth_descriptor_table (PROTOCOL_NST_METHODS (p), p,
					   "_OBJC_ProtocolInstanceMethods",
					   meta_proto_nst_meth);

      while (cls_methods)
	{
	  if (! METHOD_ENCODING (cls_methods))
	    {
	      encoding = encode_method_prototype (cls_methods);
	      METHOD_ENCODING (cls_methods) = encoding;
	    }

	  cls_methods = TREE_CHAIN (cls_methods);
	}

      UOBJC_CLASS_METHODS_decl =
	generate_v1_meth_descriptor_table (PROTOCOL_CLS_METHODS (p), p,
					   "_OBJC_ProtocolClassMethods",
					   meta_proto_cls_meth);

      /* There should be no optional methods for ABI-0 - but we need to
         check all this here before the lists are made.  */
      nst_methods = PROTOCOL_OPTIONAL_NST_METHODS (p);
      while (nst_methods)
        {
          if (! METHOD_ENCODING (nst_methods))
            {
              encoding = encode_method_prototype (nst_methods);
              METHOD_ENCODING (nst_methods) = encoding;
            }
          nst_methods = TREE_CHAIN (nst_methods);
        }

      V1_Protocol_OPT_NST_METHODS_decl =
	generate_v1_meth_descriptor_table (PROTOCOL_OPTIONAL_NST_METHODS (p), p,
					   "_OBJC_OptionalProtocolInstanceMethods",
					   meta_proto_nst_meth);

      cls_methods = PROTOCOL_OPTIONAL_CLS_METHODS (p);
      while (cls_methods)
        {
          if (! METHOD_ENCODING (cls_methods))
            {
              encoding = encode_method_prototype (cls_methods);
              METHOD_ENCODING (cls_methods) = encoding;
            }

          cls_methods = TREE_CHAIN (cls_methods);
        }

      V1_Protocol_OPT_CLS_METHODS_decl =
	generate_v1_meth_descriptor_table (PROTOCOL_OPTIONAL_CLS_METHODS (p), p,
					   "_OBJC_OptionalProtocolClassMethods",
					   meta_proto_cls_meth);

      if (PROTOCOL_LIST (p))
	refs_decl = generate_v1_protocol_list (p, objc_implementation_context);
      else
	refs_decl = 0;

      /* static struct objc_protocol _OBJC_PROTOCOL_<mumble>; */
      protocol_name_expr = add_objc_string (PROTOCOL_NAME (p), class_names);
      /* TODO: more locations to be fixed up... */
      loc = UNKNOWN_LOCATION;
      refs_type =
	build_pointer_type (build_pointer_type (objc_protocol_template));
      if (refs_decl)
	refs_expr = convert (refs_type,
			     build_unary_op (loc, ADDR_EXPR, refs_decl, 0));
      else
        refs_expr = convert (refs_type, null_pointer_node);

      if (flag_objc_abi < 1)
	{
	  /* Original ABI.  */
	  initlist =
		build_protocol_initializer (TREE_TYPE (decl),
					    protocol_name_expr, refs_expr,
					    UOBJC_INSTANCE_METHODS_decl,
					    UOBJC_CLASS_METHODS_decl);
	  finish_var_decl (decl, initlist);
          continue;
	}

      /* else - V1 extensions.  */

      V1_Property_decl =
		generate_v1_property_table (p, NULL_TREE);

      V1_ProtocolExt_decl =
	generate_v1_objc_protocol_extension (p,
					     V1_Protocol_OPT_NST_METHODS_decl,
					     V1_Protocol_OPT_CLS_METHODS_decl,
					     V1_Property_decl);

      initlist = build_v1_protocol_initializer (TREE_TYPE (decl),
						protocol_name_expr, refs_expr,
						UOBJC_INSTANCE_METHODS_decl,
						UOBJC_CLASS_METHODS_decl,
						V1_ProtocolExt_decl);
      finish_var_decl (decl, initlist);
    }
}

static tree
generate_dispatch_table (tree chain, const char *name, tree attr)
{
  tree decl, method_list_template, initlist;
  vec<constructor_elt, va_gc> *v = NULL;
  int size;

  if (!chain || !name || !(size = list_length (chain)))
    return NULL_TREE;

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

  OBJCMETA (decl, objc_meta, attr);
  finish_var_decl (decl,
		   objc_build_constructor (TREE_TYPE (decl), v));

  return decl;
}

/* Init a category.  */
static tree
build_v1_category_initializer (tree type, tree cat_name, tree class_name,
				tree inst_methods, tree class_methods,
				tree protocol_list, tree property_list,
				location_t loc)
{
  tree expr, ltyp;
  vec<constructor_elt, va_gc> *v = NULL;

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

  if (flag_objc_abi >= 1)
    {
      int val = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (objc_category_template));
      expr = build_int_cst (NULL_TREE, val);
      CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, expr);
      ltyp = objc_prop_list_ptr;
      if (property_list)
	expr = convert (ltyp, build_unary_op (loc, ADDR_EXPR, property_list, 0));
      else
	expr = convert (ltyp, null_pointer_node);
      CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, expr);
    }

  return objc_build_constructor (type, v);
}

/* static struct objc_category _OBJC_CATEGORY_<name> = { ... }; */
/* TODO: get rid of passing stuff around in globals.  */
static void
generate_v1_category (struct imp_entry *impent)
{
  tree initlist, cat_name_expr, class_name_expr;
  tree protocol_decl, category, cat_decl;
  tree inst_methods = NULL_TREE, class_methods = NULL_TREE;
  tree cat = impent->imp_context;
  location_t loc;
  char buf[BUFSIZE];

  cat_decl = impent->class_decl;
  loc = DECL_SOURCE_LOCATION (cat_decl);

  add_class_reference (CLASS_NAME (cat));
  cat_name_expr = add_objc_string (CLASS_SUPER_NAME (cat), class_names);
  class_name_expr = add_objc_string (CLASS_NAME (cat), class_names);

  category = lookup_category (impent->imp_template, CLASS_SUPER_NAME (cat));

  if (category && CLASS_PROTOCOL_LIST (category))
    {
      generate_protocol_references (CLASS_PROTOCOL_LIST (category));
      protocol_decl = generate_v1_protocol_list (category, cat);
    }
  else
    protocol_decl = 0;

  if (flag_objc_abi >= 1)
    V1_Property_decl = generate_v1_property_table (NULL_TREE, cat);
  else
    V1_Property_decl = NULL_TREE;

  if (CLASS_NST_METHODS (cat))
    {
      snprintf (buf, BUFSIZE, "_OBJC_CategoryInstanceMethods_%s_%s",
		IDENTIFIER_POINTER (CLASS_NAME (cat)),
		IDENTIFIER_POINTER (CLASS_SUPER_NAME (cat)));
      inst_methods = generate_dispatch_table (CLASS_NST_METHODS (cat), buf,
					      meta_cati_meth);
    }

  if (CLASS_CLS_METHODS (cat))
    {
      snprintf (buf, BUFSIZE, "_OBJC_CategoryClassMethods_%s_%s",
		IDENTIFIER_POINTER (CLASS_NAME (cat)),
		IDENTIFIER_POINTER (CLASS_SUPER_NAME (cat)));
      class_methods = generate_dispatch_table (CLASS_CLS_METHODS (cat), buf,
					       meta_catc_meth);
    }

  initlist = build_v1_category_initializer (TREE_TYPE (cat_decl),
					    cat_name_expr, class_name_expr,
					    inst_methods, class_methods,
					    protocol_decl, V1_Property_decl,
					    loc);

  finish_var_decl (cat_decl, initlist);
  impent->class_decl = cat_decl;
}

/* This routine builds the class extension used by v1 NeXT.  */

static tree
generate_objc_class_ext (tree property_list, tree context)
{
  tree decl, expr, ltyp;
  tree weak_ivar_layout_tree;
  int size;
  location_t loc;
  vec<constructor_elt, va_gc> *v = NULL;
  char buf[BUFSIZE];

  /* TODO: pass the loc in or find it from args.  */
  loc = UNKNOWN_LOCATION;

  /* const char *weak_ivar_layout
     TODO: Figure the ivar layouts out... */
  weak_ivar_layout_tree = NULL_TREE;

  if (!property_list && !weak_ivar_layout_tree)
    return NULL_TREE;

  if (!objc_class_ext_template)
    build_objc_class_ext_template ();

  /* uint32_t size */
  size = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (objc_class_ext_template));
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, build_int_cst (NULL_TREE, size));

  ltyp = const_string_type_node;
  if (weak_ivar_layout_tree)
    expr = convert (ltyp, weak_ivar_layout_tree);
  else
    expr = convert (ltyp, null_pointer_node);
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, expr);

  /* struct _prop_list_t *properties; */
  ltyp = objc_prop_list_ptr;
  if (property_list)
     expr = convert (ltyp, build_unary_op (loc, ADDR_EXPR, property_list, 0));
  else
    expr = convert (ltyp, null_pointer_node);
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, expr);

  snprintf (buf, BUFSIZE, "_OBJC_ClassExt_%s",
	    IDENTIFIER_POINTER (CLASS_NAME (context)));
  decl = start_var_decl (objc_class_ext_template, buf);
  expr = objc_build_constructor (TREE_TYPE (decl), v);
  OBJCMETA (decl, objc_meta, meta_class_extension);
  finish_var_decl (decl, expr);
  return decl;
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
     struct objc_cache *cache;
     struct objc_protocol_list *protocols;
  #if ABI >= 1
     const char *ivar_layout;
     struct _objc_class_ext *ext;
  #else
     void *sel_id;
     void *gc_object_type;
  #endif
   }; */

static tree
build_v1_shared_structure_initializer (tree type, tree isa, tree super,
				    tree name, tree size, int status,
				    tree dispatch_table, tree ivar_list,
				    tree protocol_list, tree class_ext)
{
  tree expr, ltyp;
  location_t loc;
  vec<constructor_elt, va_gc> *v = NULL;

  /* TODO: fish the location out of the input data.  */
  loc = UNKNOWN_LOCATION;

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
  ltyp = objc_ivar_list_ptr;
  if (ivar_list)
    expr = convert (ltyp, build_unary_op (loc, ADDR_EXPR, ivar_list, 0));
  else
    expr = convert (ltyp, null_pointer_node);
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, expr);

  /* objc_method_list = */
  ltyp = objc_method_list_ptr;
  if (dispatch_table)
    expr = convert (ltyp, build_unary_op (loc, ADDR_EXPR, dispatch_table, 0));
  else
    expr = convert (ltyp, null_pointer_node);
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, expr);

  ltyp = build_pointer_type (xref_tag (RECORD_TYPE,
					get_identifier ("objc_cache")));
  /* method_cache = */
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, convert (ltyp, null_pointer_node));

  /* protocol_list = */
  ltyp = build_pointer_type (build_pointer_type (objc_protocol_template));
  if (protocol_list)
    expr = convert (ltyp, build_unary_op (loc, ADDR_EXPR,  protocol_list, 0));
  else
    expr = convert (ltyp, null_pointer_node);
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, expr);

  if (flag_objc_abi >= 1)
    {
      /* TODO: figure out the ivar_layout stuff.  */
      expr = convert (const_string_type_node, null_pointer_node);
      CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, expr);
      if (!objc_class_ext_template)
	build_objc_class_ext_template ();
      ltyp = build_pointer_type (objc_class_ext_template);
      if (class_ext)
	expr = convert (ltyp, build_unary_op (loc, ADDR_EXPR, class_ext, 0));
      else
	expr = convert (ltyp, null_pointer_node);
      CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, expr);
    }
  else
    {
      /* sel_id = NULL */
      CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, null_pointer_node);

      /* gc_object_type = NULL */
      CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, null_pointer_node);
    }
  return objc_build_constructor (type, v);
}

static tree
generate_ivars_list (tree chain, const char *name, tree attr)
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

  OBJCMETA (decl, objc_meta, attr);
  finish_var_decl (decl,
		   objc_build_constructor (TREE_TYPE (decl), inits));

  return decl;
}

/* static struct objc_class _OBJC_METACLASS_Foo={ ... };
   static struct objc_class _OBJC_CLASS_Foo={ ... }; */

static void
generate_v1_class_structs (struct imp_entry *impent)
{
  tree name_expr, super_expr, root_expr, class_decl, meta_decl;
  tree my_root_id, my_super_id;
  tree cast_type, initlist, protocol_decl;
  tree class_ext_decl = NULL_TREE, props = NULL_TREE;
  tree inst_methods = NULL_TREE, class_methods = NULL_TREE;
  tree chain, inst_ivars = NULL_TREE, class_ivars = NULL_TREE;
  int cls_flags;
  location_t loc;
  char buf[BUFSIZE];

/*  objc_implementation_context = impent->imp_context;
  implementation_template = impent->imp_template;*/
  class_decl = impent->class_decl;
  meta_decl = impent->meta_decl;
  cls_flags = impent->has_cxx_cdtors ? CLS_HAS_CXX_STRUCTORS : 0 ;

  loc = DECL_SOURCE_LOCATION (impent->class_decl);

  if (flag_objc_abi >= 1)
    {
      /* ABI=1 additions.  */
      props = generate_v1_property_table (NULL_TREE, impent->imp_context);
      class_ext_decl = generate_objc_class_ext (props, impent->imp_context);
    }

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
      super_expr = add_objc_string (my_super_id, class_names);
    }
  else
    {
      /* No super class.  */
      my_root_id = CLASS_NAME (impent->imp_template);
      super_expr = null_pointer_node;
    }

  /* Install class `isa' and `super' pointers at runtime.  */
  cast_type = build_pointer_type (objc_class_template);
  super_expr = build_c_cast (loc, cast_type, super_expr);

  root_expr = add_objc_string (my_root_id, class_names);
  root_expr = build_c_cast (loc, cast_type, root_expr);

  if (CLASS_PROTOCOL_LIST (impent->imp_template))
    {
      generate_protocol_references (CLASS_PROTOCOL_LIST (impent->imp_template));
      protocol_decl = generate_v1_protocol_list (impent->imp_template,
						 impent->imp_context);
    }
  else
    protocol_decl = NULL_TREE;

  if (CLASS_CLS_METHODS (impent->imp_context))
    {
      snprintf (buf, BUFSIZE, "_OBJC_ClassMethods_%s",
		IDENTIFIER_POINTER (CLASS_NAME (impent->imp_context)));
      class_methods = generate_dispatch_table (CLASS_CLS_METHODS (impent->imp_context),
					       buf, meta_clac_meth);
    }

  if (CLASS_SUPER_NAME (impent->imp_template) == NULL_TREE
      && (chain = TYPE_FIELDS (objc_class_template)))
    {
      snprintf (buf, BUFSIZE, "_OBJC_ClassIvars_%s",
		IDENTIFIER_POINTER (CLASS_NAME (impent->imp_context)));
      class_ivars = generate_ivars_list (chain, buf, meta_clac_vars);
    }
  /* TODO: get rid of hidden passing of stuff in globals.  */
  /* UOBJC_INSTANCE/CLASS_Variables_decl made in generate_ivarlists().  */

  name_expr = add_objc_string (CLASS_NAME (impent->imp_template), class_names);

  /* static struct objc_class _OBJC_METACLASS_Foo = { ... }; */

  initlist = build_v1_shared_structure_initializer
		(TREE_TYPE (meta_decl),
		root_expr, super_expr, name_expr,
		convert (integer_type_node, TYPE_SIZE_UNIT (objc_class_template)),
		CLS_META, class_methods, class_ivars,
		protocol_decl, NULL_TREE);

  finish_var_decl (meta_decl, initlist);
  impent->meta_decl = meta_decl;

  /* static struct objc_class _OBJC_CLASS_Foo={ ... }; */
  if (CLASS_NST_METHODS (impent->imp_context))
    {
      snprintf (buf, BUFSIZE, "_OBJC_InstanceMethods_%s",
		IDENTIFIER_POINTER (CLASS_NAME (impent->imp_context)));
      inst_methods = generate_dispatch_table (CLASS_NST_METHODS (impent->imp_context),
					      buf, meta_clai_meth);
    }

  if ((chain = CLASS_IVARS (impent->imp_template)))
    {
      snprintf (buf, BUFSIZE, "_OBJC_InstanceIvars_%s",
		IDENTIFIER_POINTER (CLASS_NAME (impent->imp_context)));
      inst_ivars = generate_ivars_list (chain, buf, meta_clai_vars);
    }

  initlist = build_v1_shared_structure_initializer
		(TREE_TYPE (class_decl),
		build_unary_op (loc, ADDR_EXPR, meta_decl, 0),
		super_expr, name_expr,
		convert (integer_type_node,
			 TYPE_SIZE_UNIT (CLASS_STATIC_TEMPLATE (impent->imp_template))),
		CLS_FACTORY | cls_flags, inst_methods, inst_ivars,
		protocol_decl, class_ext_decl);

  finish_var_decl (class_decl, initlist);
  impent->class_decl = class_decl;
}

/* --- Output NeXT V1 Metadata --- */

/* Create the initial value for the `defs' field of _objc_symtab.
   This is a CONSTRUCTOR.  */

static tree
init_def_list (tree type)
{
  tree expr;
  location_t loc;
  struct imp_entry *impent;
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
  tree fields, *chain = NULL;

  objc_symtab_template = objc_start_struct (get_identifier (UTAG_SYMTAB));

  /* long sel_ref_cnt; */
  fields = add_field_decl (long_integer_type_node, "sel_ref_cnt", &chain);

  /* SEL *refs; */
  add_field_decl (build_pointer_type (objc_selector_type), "refs", &chain);

  /* short cls_def_cnt; */
  add_field_decl (short_integer_type_node, "cls_def_cnt", &chain);

  /* short cat_def_cnt; */
  add_field_decl (short_integer_type_node, "cat_def_cnt", &chain);

  if (imp_count || cat_count)
    {
      /* void *defs[imp_count + cat_count (+ 1)]; */
      /* NB: The index is one less than the size of the array.  */
      int index = imp_count + cat_count;
      tree array_type = build_sized_array_type (ptr_type_node, index);
      add_field_decl (array_type, "defs", &chain);
    }

  objc_finish_struct (objc_symtab_template, fields);
}
/* Construct the initial value for all of _objc_symtab.  */

static tree
init_objc_symtab (tree type)
{
  vec<constructor_elt, va_gc> *v = NULL;

  /* sel_ref_cnt = { ..., 5, ... } */

  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE,
			  build_int_cst (long_integer_type_node, 0));

  /* refs = { ..., _OBJC_SELECTOR_TABLE, ... } */

  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE,
			  convert (build_pointer_type (objc_selector_type),
							integer_zero_node));

  /* cls_def_cnt = { ..., 5, ... } */

  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE,
			  build_int_cst (short_integer_type_node, imp_count));

  /* cat_def_cnt = { ..., 5, ... } */

  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE,
			  build_int_cst (short_integer_type_node, cat_count));

  /* cls_def = { ..., { &Foo, &Bar, ...}, ... } */

  if (imp_count || cat_count)
    {
      tree field = TYPE_FIELDS (type);
      field = DECL_CHAIN (DECL_CHAIN (DECL_CHAIN (DECL_CHAIN (field))));

      CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, init_def_list (TREE_TYPE (field)));
    }

  return objc_build_constructor (type, v);
}

/* Create the declaration of _OBJC_SYMBOLS, with type `struct _objc_symtab'
   and initialized appropriately.  */

static void
generate_objc_symtab_decl (void)
{
  build_objc_symtab_template ();
  UOBJC_SYMBOLS_decl = start_var_decl (objc_symtab_template, "_OBJC_Symbols");
  /* Allow the runtime to mark meta-data such that it can be assigned to target
     specific sections by the back-end.  */
  OBJCMETA (UOBJC_SYMBOLS_decl, objc_meta, meta_symtab);
  finish_var_decl (UOBJC_SYMBOLS_decl,
		   init_objc_symtab (TREE_TYPE (UOBJC_SYMBOLS_decl)));
}

/* Any target implementing NeXT ObjC m32 ABI has to ensure that objects
   refer to, and define, symbols that enforce linkage of classes into the
   executable image, preserving unix archive semantics.

   At present (4.8), the only targets implementing this are Darwin; these
   use top level asms to implement a scheme (see config/darwin-c.cc).  The
   latter method is a hack, but compatible with LTO see also PR48109 for
   further discussion and other possible methods.  */

static void
handle_next_class_ref (tree chain ATTRIBUTE_UNUSED)
{
  if (targetcm.objc_declare_unresolved_class_reference)
    {
      const char *name = IDENTIFIER_POINTER (TREE_VALUE (chain));
      char *string = (char *) alloca (strlen (name) + 30);
      sprintf (string, ".objc_class_name_%s", name);
      targetcm.objc_declare_unresolved_class_reference (string);
    }
}

static void
handle_next_impent (struct imp_entry *impent ATTRIBUTE_UNUSED)
{
  if (targetcm.objc_declare_class_definition)
    {
      char buf[BUFSIZE];

      switch (TREE_CODE (impent->imp_context))
	{
	  case CLASS_IMPLEMENTATION_TYPE:
	    snprintf (buf, BUFSIZE, ".objc_class_name_%s",
		      IDENTIFIER_POINTER (CLASS_NAME (impent->imp_context)));
	    break;
	  case CATEGORY_IMPLEMENTATION_TYPE:
	    snprintf (buf, BUFSIZE, "*.objc_category_name_%s_%s",
		      IDENTIFIER_POINTER (CLASS_NAME (impent->imp_context)),
		      IDENTIFIER_POINTER (CLASS_SUPER_NAME (impent->imp_context)));
	    break;
	  default:
	    return;
	}
      targetcm.objc_declare_class_definition (buf);
    }
}

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
  OBJCMETA (decl, objc_meta, meta_class_reference);
  finish_var_decl (decl, expr);
  return;
}

static void
objc_generate_v1_next_metadata (void)
{
  struct imp_entry *impent;
  tree chain, attr;
  long vers;

  /* FIXME: Make sure that we generate no metadata if there is nothing
     to put into it.  */

  if (objc_static_instances)
    gcc_unreachable (); /* Not for NeXT */

  build_metadata_templates ();
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
	generate_v1_class_structs (impent);
      else
	generate_v1_category (impent);
    }

  /* If we are using an array of selectors, we must always
     finish up the array decl even if no selectors were used.  */
  build_next_selector_translation_table ();

  if (protocol_chain)
    generate_v1_protocols ();

  /* Pass summary information to the runtime.  */
  if (imp_count || cat_count)
    generate_objc_symtab_decl ();

  vers = OBJC_VERSION;
  attr = build_tree_list (objc_meta, meta_modules);
  build_module_descriptor (vers, attr);

  /* Dump the class references.  This forces the appropriate classes
     to be linked into the executable image, preserving unix archive
     semantics.  */
  for (chain = cls_ref_chain; chain; chain = TREE_CHAIN (chain))
    {
      handle_next_class_ref (chain);
      if (TREE_PURPOSE (chain))
	generate_classref_translation_entry (chain);
    }

  for (impent = imp_list; impent; impent = impent->next)
    handle_next_impent (impent);

  /* Emit the strings tables.  */
  generate_strings ();
}

/* --- exceptions stuff --- */

/* Predefine the following data type:

   struct _objc_exception_data
   {
     int buf[OBJC_JBLEN];
     void *pointers[4];
   }; */

/* The following yuckiness should prevent users from having to #include
   <setjmp.h> in their code... */

/* Define to a harmless positive value so the below code doesn't die.  */
#ifndef OBJC_JBLEN
#define OBJC_JBLEN 18
#endif

static void
build_next_objc_exception_stuff (void)
{
  tree decls, temp_type, *chain = NULL;

  objc_exception_data_template
    = objc_start_struct (get_identifier (UTAG_EXCDATA));

  /* int buf[OBJC_JBLEN]; */

  temp_type = build_sized_array_type (integer_type_node, OBJC_JBLEN);
  decls = add_field_decl (temp_type, "buf", &chain);

  /* void *pointers[4]; */

  temp_type = build_sized_array_type (ptr_type_node, 4);
  add_field_decl (temp_type, "pointers", &chain);

  objc_finish_struct (objc_exception_data_template, decls);

  /* int _setjmp(...); */
  /* If the user includes <setjmp.h>, this shall be superseded by
     'int _setjmp(jmp_buf);' */
  temp_type = build_function_type (integer_type_node, NULL_TREE);
  objc_setjmp_decl
    = add_builtin_function (TAG_SETJMP, temp_type, 0, NOT_BUILT_IN, NULL, NULL_TREE);

  /* id objc_exception_extract(struct _objc_exception_data *); */
  temp_type
    = build_function_type_list (objc_object_type,
                                build_pointer_type (objc_exception_data_template),
                                NULL_TREE);
  objc_exception_extract_decl
    = add_builtin_function (TAG_EXCEPTIONEXTRACT, temp_type, 0, NOT_BUILT_IN, NULL,
			    NULL_TREE);
  /* void objc_exception_try_enter(struct _objc_exception_data *); */
  /* void objc_exception_try_exit(struct _objc_exception_data *); */
  temp_type
    = build_function_type_list (void_type_node,
                                build_pointer_type (objc_exception_data_template),
                                NULL_TREE);
  objc_exception_try_enter_decl
    = add_builtin_function (TAG_EXCEPTIONTRYENTER, temp_type, 0, NOT_BUILT_IN, NULL,
			    NULL_TREE);
  objc_exception_try_exit_decl
    = add_builtin_function (TAG_EXCEPTIONTRYEXIT, temp_type, 0, NOT_BUILT_IN, NULL,
			    NULL_TREE);

  /* int objc_exception_match(id, id); */
  temp_type
    = build_function_type_list (integer_type_node,
                                objc_object_type, objc_object_type, NULL_TREE);
  objc_exception_match_decl
    = add_builtin_function (TAG_EXCEPTIONMATCH, temp_type, 0, NOT_BUILT_IN, NULL,
			    NULL_TREE);

  /* id objc_assign_ivar (id, id, unsigned int); */
  /* id objc_assign_ivar_Fast (id, id, unsigned int)
       __attribute__ ((hard_coded_address (OFFS_ASSIGNIVAR_FAST))); */
  temp_type
    = build_function_type_list (objc_object_type,
                                objc_object_type,
                                objc_object_type,
                                unsigned_type_node,
                                NULL_TREE);
  objc_assign_ivar_decl
    = add_builtin_function (TAG_ASSIGNIVAR, temp_type, 0, NOT_BUILT_IN,
			    NULL, NULL_TREE);
#ifdef OFFS_ASSIGNIVAR_FAST
  objc_assign_ivar_fast_decl
    = add_builtin_function (TAG_ASSIGNIVAR_FAST, temp_type, 0,
			    NOT_BUILT_IN, NULL, NULL_TREE);
  DECL_ATTRIBUTES (objc_assign_ivar_fast_decl)
    = tree_cons (get_identifier ("hard_coded_address"),
		 build_int_cst (NULL_TREE, OFFS_ASSIGNIVAR_FAST),
		 NULL_TREE);
#else
  /* Default to slower ivar method.  */
  objc_assign_ivar_fast_decl = objc_assign_ivar_decl;
#endif

  /* id objc_assign_global (id, id *); */
  /* id objc_assign_strongCast (id, id *); */
  temp_type = build_function_type_list (objc_object_type,
                                        objc_object_type,
                                        build_pointer_type (objc_object_type),
                                        NULL_TREE);
  objc_assign_global_decl
	= add_builtin_function (TAG_ASSIGNGLOBAL, temp_type, 0, NOT_BUILT_IN, NULL,
				NULL_TREE);
  objc_assign_strong_cast_decl
	= add_builtin_function (TAG_ASSIGNSTRONGCAST, temp_type, 0, NOT_BUILT_IN, NULL,
				NULL_TREE);
}

/* --- NeXT V1 SJLJ Exceptions --- */

/* Build "objc_exception_try_exit(&_stack)".  */

static tree
next_sjlj_build_try_exit (struct objc_try_context **ctcp)
{
  tree t;
  t = build_fold_addr_expr_loc (input_location, (*ctcp)->stack_decl);
  t = tree_cons (NULL, t, NULL);
  t = build_function_call (input_location,
			   objc_exception_try_exit_decl, t);
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
next_sjlj_build_enter_and_setjmp (struct objc_try_context **ctcp)
{
  tree t, enter, sj, cond;

  t = build_fold_addr_expr_loc (input_location, (*ctcp)->stack_decl);
  t = tree_cons (NULL, t, NULL);
  enter = build_function_call (input_location,
			       objc_exception_try_enter_decl, t);

  t = objc_build_component_ref ((*ctcp)->stack_decl,
				get_identifier ("buf"));
  t = build_fold_addr_expr_loc (input_location, t);
#ifdef OBJCPLUS
  /* Convert _setjmp argument to type that is expected.  */
  if (prototype_p (TREE_TYPE (objc_setjmp_decl)))
    t = convert (TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (objc_setjmp_decl))), t);
  else
    t = convert (ptr_type_node, t);
#else
  t = convert (ptr_type_node, t);
#endif
  t = tree_cons (NULL, t, NULL);
  sj = build_function_call (input_location,
			    objc_setjmp_decl, t);

  cond = build2 (COMPOUND_EXPR, TREE_TYPE (sj), enter, sj);
  cond = c_common_truthvalue_conversion (input_location, cond);

  return build3 (COND_EXPR, void_type_node, cond, NULL, NULL);
}

/* Build:

   DECL = objc_exception_extract(&_stack); */

static tree
next_sjlj_build_exc_extract (struct objc_try_context **ctcp, tree decl)
{
  tree t;

  t = build_fold_addr_expr_loc (input_location, (*ctcp)->stack_decl);
  t = tree_cons (NULL, t, NULL);
  t = build_function_call (input_location,
			   objc_exception_extract_decl, t);
  t = convert (TREE_TYPE (decl), t);
  t = build2 (MODIFY_EXPR, void_type_node, decl, t);

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
next_sjlj_build_catch_list (struct objc_try_context **ctcp)
{
  tree_stmt_iterator i = tsi_start ((*ctcp)->catch_list);
  tree catch_seq, t;
  tree *last = &catch_seq;
  bool saw_id = false;

  for (; !tsi_end_p (i); tsi_next (&i))
    {
      tree stmt = tsi_stmt (i);
      tree type = CATCH_TYPES (stmt);
      tree body = CATCH_BODY (stmt);

      if (type != error_mark_node
	  && objc_is_object_id (TREE_TYPE (type)))
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
	      args = tree_cons (NULL, (*ctcp)->caught_decl, NULL);
	      t = objc_get_class_reference (OBJC_TYPE_NAME (TREE_TYPE (type)));
	      args = tree_cons (NULL, t, args);
	      t = build_function_call (input_location,
				       objc_exception_match_decl, args);
	      cond = c_common_truthvalue_conversion (input_location, t);
	    }
	  t = build3 (COND_EXPR, void_type_node, cond, body, NULL);
	  SET_EXPR_LOCATION (t, EXPR_LOCATION (stmt));

	  *last = t;
	  last = &COND_EXPR_ELSE (t);
	}
    }

  if (!saw_id)
    {
      t = build2 (MODIFY_EXPR, void_type_node, (*ctcp)->rethrow_decl,
		  (*ctcp)->caught_decl);
      SET_EXPR_LOCATION (t, (*ctcp)->end_catch_locus);
      append_to_statement_list (t, last);

      t = next_sjlj_build_try_exit (ctcp);
      SET_EXPR_LOCATION (t, (*ctcp)->end_catch_locus);
      append_to_statement_list (t, last);
    }

  return catch_seq;
}

/* Build a complete @try-@catch-@finally block for legacy Darwin setjmp
   exception handling.  We aim to build:

	{
	  struct _objc_exception_data _stack;
	  id _rethrow = 0;
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
next_sjlj_build_try_catch_finally (struct objc_try_context **ctcp)
{
  tree rethrow_decl, stack_decl, t;
  tree catch_seq, try_fin, bind;
  struct objc_try_context *cur_try_context = *ctcp;

  /* Create the declarations involved.  */
  t = xref_tag (RECORD_TYPE, get_identifier (UTAG_EXCDATA));
  stack_decl = objc_create_temporary_var (t, NULL);
  cur_try_context->stack_decl = stack_decl;

  rethrow_decl = objc_create_temporary_var (objc_object_type, NULL);
  cur_try_context->rethrow_decl = rethrow_decl;
  TREE_CHAIN (rethrow_decl) = stack_decl;

  /* Build the outermost variable binding level.  */
  bind = build3 (BIND_EXPR, void_type_node, rethrow_decl, NULL, NULL);
  SET_EXPR_LOCATION (bind, cur_try_context->try_locus);
  TREE_SIDE_EFFECTS (bind) = 1;

  /* Initialize rethrow_decl.  */
  t = build2 (MODIFY_EXPR, void_type_node, rethrow_decl,
	      convert (objc_object_type, null_pointer_node));
  SET_EXPR_LOCATION (t, cur_try_context->try_locus);
  append_to_statement_list (t, &BIND_EXPR_BODY (bind));

  /* Build the outermost TRY_FINALLY_EXPR.  */
  try_fin = build2 (TRY_FINALLY_EXPR, void_type_node, NULL, NULL);
  SET_EXPR_LOCATION (try_fin, cur_try_context->try_locus);
  TREE_SIDE_EFFECTS (try_fin) = 1;
  append_to_statement_list (try_fin, &BIND_EXPR_BODY (bind));

  /* Create the complete catch sequence.  */
  if (cur_try_context->catch_list)
    {
      tree caught_decl = objc_build_exc_ptr (ctcp);
      catch_seq = build_stmt (input_location, BIND_EXPR, caught_decl, NULL, NULL);
      TREE_SIDE_EFFECTS (catch_seq) = 1;

      t = next_sjlj_build_exc_extract (ctcp, caught_decl);
      append_to_statement_list (t, &BIND_EXPR_BODY (catch_seq));

      t = next_sjlj_build_enter_and_setjmp (ctcp);
      COND_EXPR_THEN (t) = next_sjlj_build_exc_extract (ctcp, rethrow_decl);
      COND_EXPR_ELSE (t) = next_sjlj_build_catch_list (ctcp);
      append_to_statement_list (t, &BIND_EXPR_BODY (catch_seq));
    }
  else
    catch_seq = next_sjlj_build_exc_extract (ctcp, rethrow_decl);
  SET_EXPR_LOCATION (catch_seq, cur_try_context->end_try_locus);

  /* Build the main register-and-try if statement.  */
  t = next_sjlj_build_enter_and_setjmp (ctcp);
  SET_EXPR_LOCATION (t, cur_try_context->try_locus);
  COND_EXPR_THEN (t) = catch_seq;
  COND_EXPR_ELSE (t) = cur_try_context->try_body;
  TREE_OPERAND (try_fin, 0) = t;

  /* Build the complete FINALLY statement list.  */
  t = next_sjlj_build_try_exit (ctcp);
  t = build_stmt (input_location, COND_EXPR,
		  c_common_truthvalue_conversion
		    (input_location, rethrow_decl),
		  NULL, t);
  SET_EXPR_LOCATION (t, cur_try_context->finally_locus);
  append_to_statement_list (t, &TREE_OPERAND (try_fin, 1));

  append_to_statement_list (cur_try_context->finally_body,
			    &TREE_OPERAND (try_fin, 1));

  t = tree_cons (NULL, rethrow_decl, NULL);
  t = build_function_call (input_location,
			   objc_exception_throw_decl, t);
  t = build_stmt (input_location, COND_EXPR,
		  c_common_truthvalue_conversion (input_location,
						  rethrow_decl),
		  t, NULL);
  SET_EXPR_LOCATION (t, cur_try_context->end_finally_locus);
  append_to_statement_list (t, &TREE_OPERAND (try_fin, 1));

  return bind;
}

/* We do not expect this to be used at the moment.
   If (a) it is possible to implement unwinder exceptions.
      (b) we do it... then it might be possibly useful.
*/
static GTY(()) tree objc_eh_personality_decl;

static tree
objc_eh_runtime_type (tree type)
{
  tree ident, eh_id, decl, str;

  gcc_unreachable ();
  if (type == error_mark_node)
    {
      /* Use 'ErrorMarkNode' as class name when error_mark_node is found
	 to prevent an ICE.  Note that we know that the compiler will
	 terminate with an error and this 'ErrorMarkNode' class name will
	 never be actually used.  */
      ident = get_identifier ("ErrorMarkNode");
      goto make_err_class;
    }

  if (POINTER_TYPE_P (type) && objc_is_object_id (TREE_TYPE (type)))
    {
      ident = get_identifier ("id");
      goto make_err_class;
    }

  if (!POINTER_TYPE_P (type) || !TYPED_OBJECT (TREE_TYPE (type)))
    {
#ifdef OBJCPLUS
      /* This routine is also called for c++'s catch clause; in which case,
	 we use c++'s typeinfo decl. */
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

/* For NeXT ABI 0 and 1, the personality routines are just those of the
   underlying language.  */

static tree
objc_eh_personality (void)
{
  if (!objc_eh_personality_decl)
#ifndef OBJCPLUS
    objc_eh_personality_decl = build_personality_function ("gcc");
#else
    objc_eh_personality_decl = build_personality_function ("gxx");
#endif
  return objc_eh_personality_decl;
}

/* --- interfaces --- */

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

/* Build __builtin_eh_pointer, or the moral equivalent.  In the case
   of Darwin, we'll arrange for it to be initialized (and associated
   with a binding) later.  */

static tree
objc_build_exc_ptr (struct objc_try_context **cur_try_context)
{
  if (flag_objc_sjlj_exceptions)
    {
      tree var = (*cur_try_context)->caught_decl;
      if (!var)
	{
	  var = objc_create_temporary_var (objc_object_type, NULL);
	  (*cur_try_context)->caught_decl = var;
	}
      return var;
    }
  else
    {
      tree t;
      t = builtin_decl_explicit (BUILT_IN_EH_POINTER);
      t = build_call_expr (t, 1, integer_zero_node);
      return fold_convert (objc_object_type, t);
    }
}

static tree
begin_catch (struct objc_try_context **cur_try_context, tree type,
	     tree decl, tree compound, bool ellipsis ATTRIBUTE_UNUSED)
{
  tree t;
  /* Record the data for the catch in the try context so that we can
     finalize it later.  We treat ellipsis the same way as catching
     with 'id xyz'.  */
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
  tree stmt;
  struct objc_try_context *c = *cur_try_context;
  /* If we're doing Darwin setjmp exceptions, build the big nasty.  */
  if (flag_objc_sjlj_exceptions)
    {
      bool save = in_late_binary_op;
      in_late_binary_op = true;
      if (!c->finally_body)
	{
	  c->finally_locus = input_location;
	  c->end_finally_locus = input_location;
	}
      stmt = next_sjlj_build_try_catch_finally (cur_try_context);
      in_late_binary_op = save;
    }
  else
    /* This doesn't happen at the moment... but maybe one day... */
    {
      /* Otherwise, nest the CATCH inside a FINALLY.  */
      stmt = c->try_body;
      if (c->catch_list)
	stmt = build_stmt (c->try_locus, TRY_CATCH_EXPR, stmt, c->catch_list);
      if (c->finally_body)
	stmt = build_stmt (c->try_locus, TRY_FINALLY_EXPR, stmt, c->finally_body);
    }
  return stmt;
}

#include "gt-objc-objc-next-runtime-abi-01.h"
