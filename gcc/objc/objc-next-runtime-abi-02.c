/* Next Runtime (ABI-2) private.
   Copyright (C) 2011-2013 Free Software Foundation, Inc.

   Contributed by Iain Sandoe and based, in part, on an implementation in
   'branches/apple/trunk' contributed by Apple Computer Inc.

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

/* The NeXT ABI2 is used for m64 implementations on Darwin/OSX machines.

   This version is intended to match (logically) the output of Apple's
   4.2.1 compiler.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "stringpool.h"

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

#include "ggc.h"
#include "target.h"
#include "tree-iterator.h"

#include "objc-runtime-hooks.h"
#include "objc-runtime-shared-support.h"
#include "objc-encoding.h"

/* ABI 2 Private definitions. */
#define DEF_CONSTANT_STRING_CLASS_NAME "NSConstantString"

#define TAG_GETCLASS		"objc_getClass"
#define TAG_GETMETACLASS	"objc_getMetaClass"

#define TAG_MSGSEND		"objc_msgSend"
#define TAG_MSGSENDSUPER	"objc_msgSendSuper"
#define TAG_MSGSEND_STRET	"objc_msgSend_stret"
#define TAG_MSGSENDSUPER_STRET	"objc_msgSendSuper_stret"

#define TAG_NEXT_EHVTABLE_NAME	"objc_ehtype_vtable"
#define TAG_V2_EH_TYPE		"objc_ehtype_t"

#define UTAG_V2_CLASS		"_class_t"
#define UTAG_V2_CLASS_RO	"_class_ro_t"
#define UTAG_V2_PROTOCOL	"_protocol_t"
#define UTAG_V2_PROTOCOL_LIST	"_protocol_list_t"

#define UTAG_V2_EH_TYPE		"_objc_ehtype_t"

#define OBJC2_CLS_HAS_CXX_STRUCTORS	0x0004L

enum objc_v2_tree_index
{
  /* Templates.  */
  OCTI_V2_CLS_TEMPL,
  OCTI_V2_CAT_TEMPL,
  OCTI_V2_CLS_RO_TEMPL,
  OCTI_V2_PROTO_TEMPL,
  OCTI_V2_IVAR_TEMPL,
  OCTI_V2_IVAR_LIST_TEMPL,
  OCTI_V2_MESSAGE_REF_TEMPL,
  OCTI_V2_SUPER_MESSAGE_REF_TEMPL,

  OCTI_V2_MESSAGE_SELECTOR_TYPE,
  OCTI_V2_SUPER_MESSAGE_SELECTOR_TYPE,
  OCTI_V2_IMP_TYPE,
  OCTI_V2_SUPER_IMP_TYPE,

  OCTI_V2_CACHE_DECL,
  OCTI_V2_VTABLE_DECL,

  OCTI_V2_PROPERTY_TEMPL,

  /* V2 messaging.  */
  OCTI_V2_UMSG_FIXUP_DECL,
  OCTI_V2_UMSG_STRET_FIXUP_DECL,
  OCTI_V2_UMSG_ID_FIXUP_DECL,
  OCTI_V2_UMSG_ID_STRET_FIXUP_DECL,
  OCTI_V2_UMSG_SUPER2_FIXUP_DECL,
  OCTI_V2_UMSG_SUPER2_STRET_FIXUP_DECL,

  /* Exceptions - related.  */
  OCTI_V2_BEGIN_CATCH_DECL,
  OCTI_V2_END_CATCH_DECL,
  OCTI_V2_RETHROW_DECL,

  OCTI_V2_MAX
};

#define objc_v2_class_template	objc_v2_global_trees[OCTI_V2_CLS_TEMPL]
#define objc_v2_class_ro_template \
				objc_v2_global_trees[OCTI_V2_CLS_RO_TEMPL]
#define objc_v2_category_template \
				objc_v2_global_trees[OCTI_V2_CAT_TEMPL]
#define objc_v2_protocol_template \
				objc_v2_global_trees[OCTI_V2_PROTO_TEMPL]

/* struct message_ref_t */
#define objc_v2_message_ref_template \
				objc_v2_global_trees[OCTI_V2_MESSAGE_REF_TEMPL]

#define objc_v2_ivar_list_ptr	objc_v2_global_trees[OCTI_V2_IVAR_LIST_TEMPL]

/* struct super_message_ref_t */
#define objc_v2_super_message_ref_template \
				objc_v2_global_trees[OCTI_V2_SUPER_MESSAGE_REF_TEMPL]

/* struct message_ref_t* */
#define objc_v2_selector_type	objc_v2_global_trees[OCTI_V2_MESSAGE_SELECTOR_TYPE]
/* struct super_super_message_ref_t */
#define objc_v2_super_selector_type \
				objc_v2_global_trees[OCTI_V2_SUPER_MESSAGE_SELECTOR_TYPE]
#define objc_v2_imp_type	objc_v2_global_trees[OCTI_V2_IMP_TYPE]
#define objc_v2_super_imp_type	objc_v2_global_trees[OCTI_V2_SUPER_IMP_TYPE]

#define UOBJC_V2_CACHE_decl	objc_v2_global_trees[OCTI_V2_CACHE_DECL]
#define UOBJC_V2_VTABLE_decl	objc_v2_global_trees[OCTI_V2_VTABLE_DECL]

#define objc_v2_ivar_template	objc_v2_global_trees[OCTI_V2_IVAR_TEMPL]
#define objc_v2_property_template \
				objc_v2_global_trees[OCTI_V2_PROPERTY_TEMPL]

/* V2 Messaging */

/* objc_msgSend_fixup_rtp */
#define umsg_fixup_decl		objc_v2_global_trees[OCTI_V2_UMSG_FIXUP_DECL]
/* objc_msgSend_stret_fixup_rtp */
#define umsg_stret_fixup_decl	objc_v2_global_trees[OCTI_V2_UMSG_STRET_FIXUP_DECL]
/* objc_msgSendId_fixup_rtp */
#define umsg_id_fixup_decl	objc_v2_global_trees[OCTI_V2_UMSG_ID_FIXUP_DECL]
/* objc_msgSendId_stret_fixup_rtp */
#define umsg_id_stret_fixup_decl \
				objc_v2_global_trees[OCTI_V2_UMSG_ID_STRET_FIXUP_DECL]
/* objc_msgSendSuper2_fixup_rtp */
#define umsg_id_super2_fixup_decl \
				objc_v2_global_trees[OCTI_V2_UMSG_SUPER2_FIXUP_DECL]
/* objc_msgSendSuper2_stret_fixup_rtp */
#define umsg_id_super2_stret_fixup_decl \
				objc_v2_global_trees[OCTI_V2_UMSG_SUPER2_STRET_FIXUP_DECL]

#define objc2_begin_catch_decl	objc_v2_global_trees[OCTI_V2_BEGIN_CATCH_DECL]
#define objc2_end_catch_decl	objc_v2_global_trees[OCTI_V2_END_CATCH_DECL]
#define objc_rethrow_exception_decl \
				objc_v2_global_trees[OCTI_V2_RETHROW_DECL]

/* rt_trees identifiers - shared between NeXT implementations.  These allow
   the FE to tag meta-data in a manner that survives LTO and can be used when
   the  runtime requires that certain meta-data items appear in particular
   named sections.  */

#include "objc-next-metadata-tags.h"
extern GTY(()) tree objc_rt_trees[OCTI_RT_META_MAX];

/* The OCTI_V2_... enumeration itself is in above.  */
static GTY(()) tree objc_v2_global_trees[OCTI_V2_MAX];

static void next_runtime_02_initialize (void);

static void build_v2_message_ref_templates (void);
static void build_v2_class_templates (void);
static void build_v2_super_template (void);
static void build_v2_category_template (void);
static void build_v2_protocol_template (void);

static tree next_runtime_abi_02_super_superclassfield_id (void);

static tree next_runtime_abi_02_class_decl (tree);
static tree next_runtime_abi_02_metaclass_decl (tree);
static tree next_runtime_abi_02_category_decl (tree);
static tree next_runtime_abi_02_protocol_decl (tree);
static tree next_runtime_abi_02_string_decl (tree, const char *, string_section);

static tree next_runtime_abi_02_get_class_reference (tree);
static tree next_runtime_abi_02_build_selector_reference (location_t, tree, tree);
static tree next_runtime_abi_02_get_protocol_reference (location_t, tree);
static tree next_runtime_abi_02_build_ivar_ref (location_t, tree, tree);
static tree next_runtime_abi_02_get_class_super_ref (location_t, struct imp_entry *, bool);
static tree next_runtime_abi_02_get_category_super_ref (location_t, struct imp_entry *, bool);

static tree next_runtime_abi_02_receiver_is_class_object (tree);
static void next_runtime_abi_02_get_arg_type_list_base (vec<tree, va_gc> **,
							tree, int, int);
static tree next_runtime_abi_02_build_objc_method_call (location_t, tree, tree,
							tree, tree, tree, int);
static bool next_runtime_abi_02_setup_const_string_class_decl (void);
static tree next_runtime_abi_02_build_const_string_constructor (location_t, tree, int);

static tree create_extern_decl (tree, const char *);

static void objc_generate_v2_next_metadata (void);
static bool objc2_objc_exception_attr (tree);

/* void build_v2_protocol_reference (tree);*/
static void build_v2_ehtype_template (void);
static void build_v2_eh_catch_objects (void);
static tree next_runtime_02_eh_type (tree);
static tree objc_eh_personality (void);
static tree build_throw_stmt (location_t, tree, bool);
static tree objc_build_exc_ptr (struct objc_try_context **);
static tree begin_catch (struct objc_try_context **, tree, tree, tree, bool);
static void finish_catch (struct objc_try_context **, tree);
static tree finish_try_stmt (struct objc_try_context **);

/* TODO: Use an objc-map.  */
static GTY ((length ("SIZEHASHTABLE"))) hash *extern_names;

bool
objc_next_runtime_abi_02_init (objc_runtime_hooks *rthooks)
{
  extern_names = ggc_alloc_cleared_vec_hash (SIZEHASHTABLE);

  if (flag_objc_exceptions && flag_objc_sjlj_exceptions)
    {
      inform (UNKNOWN_LOCATION, "%<-fobjc-sjlj-exceptions%> is ignored for "
				"%<-fnext-runtime%> when %<-fobjc-abi-version%> >= 2");
      flag_objc_sjlj_exceptions = 0;
    }

  rthooks->initialize = next_runtime_02_initialize;
  rthooks->default_constant_string_class_name = DEF_CONSTANT_STRING_CLASS_NAME;
  rthooks->tag_getclass = TAG_GETCLASS;
  rthooks->super_superclassfield_ident = next_runtime_abi_02_super_superclassfield_id;

  rthooks->class_decl = next_runtime_abi_02_class_decl;
  rthooks->metaclass_decl = next_runtime_abi_02_metaclass_decl;
  rthooks->category_decl = next_runtime_abi_02_category_decl;
  rthooks->protocol_decl = next_runtime_abi_02_protocol_decl;
  rthooks->string_decl = next_runtime_abi_02_string_decl;

  rthooks->get_class_reference = next_runtime_abi_02_get_class_reference;
  rthooks->build_selector_reference = next_runtime_abi_02_build_selector_reference;
  rthooks->get_protocol_reference = next_runtime_abi_02_get_protocol_reference;
  rthooks->build_ivar_reference = next_runtime_abi_02_build_ivar_ref;
  rthooks->get_class_super_ref = next_runtime_abi_02_get_class_super_ref;
  rthooks->get_category_super_ref = next_runtime_abi_02_get_category_super_ref;

  rthooks->receiver_is_class_object = next_runtime_abi_02_receiver_is_class_object;
  rthooks->get_arg_type_list_base = next_runtime_abi_02_get_arg_type_list_base;
  rthooks->build_objc_method_call = next_runtime_abi_02_build_objc_method_call;

  rthooks->setup_const_string_class_decl =
				next_runtime_abi_02_setup_const_string_class_decl;
  rthooks->build_const_string_constructor =
				next_runtime_abi_02_build_const_string_constructor;

  rthooks->build_throw_stmt = build_throw_stmt;
  rthooks->build_exc_ptr = objc_build_exc_ptr;
  rthooks->begin_catch = begin_catch;
  rthooks->finish_catch = finish_catch;
  rthooks->finish_try_stmt = finish_try_stmt;

  rthooks->generate_metadata = objc_generate_v2_next_metadata;
  return true;
}

/* We need a way to convey what kind of meta-data are represented by a given
   variable, since each type is expected (by the runtime) to be found in a
   specific named section.  The solution must be usable with LTO.

   The scheme used for NeXT ABI 0/1 (partial matching of variable names) is not
   satisfactory when LTO is used with ABI-2.  We now tag ObjC meta-data with
   identification attributes in the front end.  The back-end may choose to act
   on these as it requires.  */

static void
next_runtime_abi_02_init_metadata_attributes (void)
{
  if (!objc_meta)
    objc_meta = get_identifier ("OBJC2META");

  if (!meta_base)
    meta_base = get_identifier ("V2_BASE");

  meta_class = get_identifier ("G2_CLAS");
  meta_metaclass = get_identifier ("G2_META");
  meta_category =
  meta_protocol = meta_base;

  meta_clac_vars =
  meta_clai_vars = meta_base;

  meta_clac_meth =
  meta_clai_meth =
  meta_catc_meth =
  meta_cati_meth =
  meta_proto_cls_meth =
  meta_proto_nst_meth = meta_base;

  meta_clas_prot =
  meta_catg_prot = meta_base;

  meta_sel_refs = get_identifier ("V2_SRFS");

  meta_class_name =
  meta_meth_name =
  meta_meth_type =
  meta_prop_name_attr = get_identifier ("V2_STRG");

  meta_mref = get_identifier ("V2_MREF");
  meta_class_ref = get_identifier ("V2_CLRF");
  meta_superclass_ref = get_identifier ("V2_SURF");

  meta_label_classlist = get_identifier ("V2_CLAB");
  meta_label_nonlazy_classlist = get_identifier ("V2_NLCL");
  meta_label_categorylist = get_identifier ("V2_CALA");
  meta_label_nonlazy_categorylist = get_identifier ("V2_NLCA");

  meta_label_protocollist = get_identifier ("V2_PLST");
  meta_proto_ref = get_identifier ("V2_PRFS");

  meta_info = get_identifier ("V2_INFO");

  meta_ehtype = get_identifier ("V2_EHTY");

  meta_const_str = get_identifier ("V2_CSTR");
}

static void next_runtime_02_initialize (void)
{
  tree type;
#ifdef OBJCPLUS
  /* For all NeXT objc ABIs -fobjc-call-cxx-cdtors is on by
     default.  */
  if (!global_options_set.x_flag_objc_call_cxx_cdtors)
    global_options.x_flag_objc_call_cxx_cdtors = 1;
#endif

  /* Set up attributes to be attached to the meta-data so that they
     will be placed in the correct sections.  */
  next_runtime_abi_02_init_metadata_attributes ();

  /* `struct objc_selector *' */
  objc_selector_type = build_pointer_type (xref_tag (RECORD_TYPE,
					   get_identifier (TAG_SELECTOR)));

  /* IMP : id (*) (id, _message_ref_t*, ...)
     SUPER_IMP : id (*) ( super_t*, _super_message_ref_t*, ...)
     objc_v2_selector_type.  */
  build_v2_message_ref_templates ();

  objc_v2_ivar_list_ptr =
		build_pointer_type (xref_tag (RECORD_TYPE,
				    get_identifier ("_ivar_list_t")));

  objc_prop_list_ptr =
		build_pointer_type (xref_tag (RECORD_TYPE,
				    get_identifier ("_prop_list_t")));

  build_v2_class_templates ();
  build_v2_super_template ();
  build_v2_protocol_template ();
  build_v2_category_template ();

  /* id objc_msgSend_fixup_rtp (id, struct message_ref_t*, ...); */
  type = build_varargs_function_type_list (objc_object_type,
						   objc_object_type,
						   objc_v2_selector_type,
						   NULL_TREE);
  umsg_fixup_decl =  add_builtin_function ("objc_msgSend_fixup",
					   type, 0, NOT_BUILT_IN,
					   NULL, NULL_TREE);
  TREE_NOTHROW (umsg_fixup_decl) = 0;

  /* id objc_msgSend_stret_fixup_rtp (id, struct message_ref_t*, ...); */
  umsg_stret_fixup_decl = add_builtin_function ("objc_msgSend_stret_fixup",
						type, 0, NOT_BUILT_IN,
						NULL, NULL_TREE);
  TREE_NOTHROW (umsg_stret_fixup_decl) = 0;

  /* id objc_msgSendId_fixup_rtp (id, struct message_ref_t*, ...); */
  umsg_id_fixup_decl = add_builtin_function ("objc_msgSendId_fixup",
					     type, 0, NOT_BUILT_IN,
					     NULL, NULL_TREE);
  TREE_NOTHROW (umsg_id_fixup_decl) = 0;

  /* id objc_msgSendId_stret_fixup_rtp
			(id, struct message_ref_t*, ...); */
  umsg_id_stret_fixup_decl = add_builtin_function ("objc_msgSendId_stret_fixup",
						   type, 0, NOT_BUILT_IN,
						   NULL, NULL_TREE);
  TREE_NOTHROW (umsg_id_stret_fixup_decl) = 0;

 /* id objc_msgSendSuper2_fixup_rtp
			(struct objc_super *, struct message_ref_t*, ...); */
  type = build_varargs_function_type_list (objc_object_type,
					   objc_super_type,
					   objc_v2_super_selector_type,
					   NULL_TREE);
  umsg_id_super2_fixup_decl = add_builtin_function ("objc_msgSendSuper2_fixup",
						    type, 0, NOT_BUILT_IN,
						    NULL, NULL_TREE);
  TREE_NOTHROW (umsg_id_super2_fixup_decl) = 0;

  /* id objc_msgSendSuper2_stret_fixup_rtp
			(struct objc_super *, struct message_ref_t*, ...); */
  umsg_id_super2_stret_fixup_decl =
			add_builtin_function ("objc_msgSendSuper2_stret_fixup",
					      type, 0, NOT_BUILT_IN,
					      NULL, NULL_TREE);
  TREE_NOTHROW (umsg_id_super2_stret_fixup_decl) = 0;

  /* Present in the library, but unused by the FE.  */
  /* Protocol *objc_getProtocol (const char *)
  type = build_function_type_list (objc_protocol_type,
				   const_string_type_node,
				   NULL_TREE);
  objc_v2_getprotocol_decl = add_builtin_function ("objc_getProtocol",
						    type, 0, NOT_BUILT_IN,
						    NULL, NULL_TREE);
  TREE_NOTHROW (objc_v2_getprotocol_decl) = 0;*/

  UOBJC_V2_CACHE_decl = create_extern_decl (ptr_type_node,
					    "_objc_empty_cache");

  UOBJC_V2_VTABLE_decl = create_extern_decl (objc_v2_imp_type,
					     "_objc_empty_vtable");

  /* id objc_getClass (const char *); */
  type = build_function_type_list (objc_object_type,
                                   const_string_type_node,
                                   NULL_TREE);
  objc_get_class_decl = add_builtin_function (TAG_GETCLASS,
					      type, 0, NOT_BUILT_IN,
					      NULL, NULL_TREE);

  /* id objc_getMetaClass (const char *); */
  objc_get_meta_class_decl = add_builtin_function (TAG_GETMETACLASS,
						   type, 0, NOT_BUILT_IN,
						   NULL, NULL_TREE);

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

  gcc_assert (!flag_objc_sjlj_exceptions);

  /* Although we warn that fobjc-exceptions is required for exceptions
     code, we carry on and create it anyway.  */

  /* This can be required, even when exceptions code is not present,
     when an __attribute__((objc_exception)) is applied to a
     class.  */
  build_v2_ehtype_template ();

  /* void * objc_begin_catch (void *) */
  type = build_function_type_list (ptr_type_node,
				   ptr_type_node, NULL_TREE);

  objc2_begin_catch_decl = add_builtin_function ("objc_begin_catch",
						 type, 0, NOT_BUILT_IN,
						 NULL, NULL_TREE);
  TREE_NOTHROW (objc2_begin_catch_decl) = 0;

  /* void objc_end_catch () */
  type = build_function_type_list (void_type_node, NULL_TREE);
  objc2_end_catch_decl = add_builtin_function ("objc_end_catch",
						type, 0, NOT_BUILT_IN,
						NULL, NULL_TREE);
  TREE_NOTHROW (objc2_end_catch_decl) = 0;

  /* void objc_exception_rethrow (void) */
  objc_rethrow_exception_decl =
			add_builtin_function ("objc_exception_rethrow",
					      type, 0, NOT_BUILT_IN,
					      NULL, NULL_TREE);
  TREE_NOTHROW (objc_rethrow_exception_decl) = 0;
  using_eh_for_cleanups ();
  lang_hooks.eh_runtime_type = next_runtime_02_eh_type;
  lang_hooks.eh_personality = objc_eh_personality;
}

/* NOTE --- templates --- */

/* Set 'objc_v2_message_ref_template' to the data type node for
   'struct _message_ref_t'.  This needs to be done just once per
   compilation.  Also Set 'objc_v2_super_message_ref_template' to data
   type node for 'struct _super_message_ref_t'.  */

/* struct _message_ref_t
   {
     IMP messenger;
     SEL name;
   };
   where IMP is: id (*) (id, _message_ref_t*, ...)
*/

/* struct _super_message_ref_t
   {
     SUPER_IMP messenger;
     SEL name;
   };
   where SUPER_IMP is: id (*) ( super_t*, _super_message_ref_t*, ...)
*/

static void
build_v2_message_ref_templates (void)
{
  tree ptr_message_ref_t;
  tree decls, *chain = NULL;

  /* struct _message_ref_t {...} */
  objc_v2_message_ref_template =
		objc_start_struct (get_identifier ("_message_ref_t"));

  /* IMP messenger; */
  ptr_message_ref_t =
		build_pointer_type (xref_tag (RECORD_TYPE,
				    get_identifier ("_message_ref_t")));

  objc_v2_imp_type =
		build_pointer_type (build_function_type_list
					(objc_object_type,
					 objc_object_type,
					 ptr_message_ref_t,
					 NULL_TREE));

  decls = add_field_decl (objc_v2_imp_type, "messenger", &chain);

  /* SEL name; */
  add_field_decl (objc_selector_type, "name", &chain);

  objc_finish_struct (objc_v2_message_ref_template, decls);

  objc_v2_selector_type = build_pointer_type (objc_v2_message_ref_template);

  chain = NULL;
  /* struct _super_message_ref_t {...} */
  objc_v2_super_message_ref_template =
		objc_start_struct (get_identifier ("_super_message_ref_t"));

  /* SUPER_IMP messenger; */
  ptr_message_ref_t = build_pointer_type
			(xref_tag (RECORD_TYPE,
				   get_identifier ("_super_message_ref_t")));

  objc_v2_super_imp_type =
		build_pointer_type (build_function_type_list
					(objc_object_type,
					 objc_super_type,
					 ptr_message_ref_t,
					 NULL_TREE));

  add_field_decl (objc_v2_super_imp_type, "messenger", &chain);

  /* SEL name; */
  add_field_decl (objc_selector_type, "name", &chain);

  objc_finish_struct (objc_v2_super_message_ref_template, decls);
  objc_v2_super_selector_type =
		build_pointer_type (objc_v2_super_message_ref_template);
}

/* Build following types which represent each class implementation.

struct class_ro_t
{
    uint32_t const flags;
    uint32_t const instanceStart;
    uint32_t const instanceSize;
#ifdef __LP64__
    uint32_t const reserved;
#endif
    const uint8_t * const ivarLayout;
    const char *const name;
    const struct method_list_t * const baseMethods;
    const struct objc_protocol_list *const baseProtocols;
    const struct ivar_list_t *const ivars;
    const uint8_t * const weakIvarLayout;
    const struct _prop_list_t * const properties;
};

struct class_t
{
    struct class_t *isa;
    struct class_t *superclass;
    void *cache;
    IMP *vtable;

    ...When this is active - it will point to a rw version, but
       when we build the meta-data we point it to the ro...
    struct class_ro_t *data;
};

*/

static void
build_v2_class_templates (void)
{
  tree cnst_strg_type;
  tree decls, *chain = NULL;

  /* struct class_ro_t {...} */
  objc_v2_class_ro_template =
	objc_start_struct (get_identifier (UTAG_V2_CLASS_RO));

  /* uint32_t const flags; */
  decls = add_field_decl (integer_type_node, "flags", &chain);

  /* uint32_t const instanceStart; */
  add_field_decl (integer_type_node, "instanceStart", &chain);

  /* uint32_t const instanceSize; */
  add_field_decl (integer_type_node, "instanceSize", &chain);

  /* This ABI is currently only used on m64 NeXT.  We always
     explicitly declare the alignment padding.  */
  /* uint32_t const reserved; */
  add_field_decl (integer_type_node, "reserved", &chain);

  /* const uint8_t * const ivarLayout; */
  cnst_strg_type = build_pointer_type (unsigned_char_type_node);
  add_field_decl (cnst_strg_type, "ivarLayout", &chain);

  /* const char *const name; */
  add_field_decl (string_type_node, "name", &chain);

  /* const struct method_list_t * const baseMethods; */
  add_field_decl (objc_method_list_ptr, "baseMethods", &chain);

  /* const struct objc_protocol_list *const baseProtocols; */
  add_field_decl (build_pointer_type
			(xref_tag (RECORD_TYPE,
				   get_identifier (UTAG_V2_PROTOCOL_LIST))),
				  "baseProtocols", &chain);

  /* const struct ivar_list_t *const ivars; */
  add_field_decl (objc_v2_ivar_list_ptr, "ivars", &chain);

  /* const uint8_t * const weakIvarLayout; */
  add_field_decl (cnst_strg_type, "weakIvarLayout",  &chain);

  /* struct _prop_list_t * baseProperties; */
  add_field_decl (objc_prop_list_ptr, "baseProperties", &chain);

  objc_finish_struct (objc_v2_class_ro_template, decls);

  chain = NULL;
  /* struct class_t {...} */
  objc_v2_class_template =
	objc_start_struct (get_identifier (UTAG_V2_CLASS));

  /* struct class_t *isa; */
  decls = add_field_decl (build_pointer_type (objc_v2_class_template),
			  "isa", &chain);

  /* struct class_t * const superclass; */
  add_field_decl (build_pointer_type (objc_v2_class_template),
				      "superclass", &chain);

  /* void *cache; */
  add_field_decl (build_pointer_type (void_type_node), "cache", &chain);

  /* IMP *vtable; */
  add_field_decl (build_pointer_type (objc_v2_imp_type), "vtable", &chain);

  /* struct class_ro_t *ro; */
  add_field_decl (build_pointer_type (objc_v2_class_ro_template), "ro", &chain);

  objc_finish_struct (objc_v2_class_template, decls);
}

/* struct _objc_super
   {
     struct _objc_object *self;
     Class cls;
   }; */
void
build_v2_super_template (void)
{
  tree decls, *chain = NULL;

  objc_super_template = objc_start_struct (get_identifier (UTAG_SUPER));

  /* struct _objc_object *self; */
  decls = add_field_decl (objc_object_type, "self", &chain);

  /* Class cls; */
  add_field_decl (objc_class_type, "cls", &chain);

  objc_finish_struct (objc_super_template, decls);
}

/* struct protocol_t
  {
     Class isa;
     const char * const protocol_name;
     const struct protocol_list_t * const protocol_list;
     const struct method_list_t * const instance_methods;
     const struct method_list_t * const class_methods;
     const struct method_list_t * optionalInstanceMethods;
     const struct method_list_t * optionalClassMethod
     const struct _prop_list_t * const properties;
     const uint32_t size;
     const uint32_t flags;
   }
*/
static void
build_v2_protocol_template (void)
{
  tree decls, *chain = NULL;

  objc_v2_protocol_template =
	objc_start_struct (get_identifier (UTAG_V2_PROTOCOL));

  /* Class isa; */
  decls = add_field_decl (objc_object_type, "isa", &chain);

  /* char *protocol_name; */
  add_field_decl (string_type_node, "protocol_name", &chain);

  /* const struct protocol_list_t * const protocol_list; */
  add_field_decl (build_pointer_type (objc_v2_protocol_template),
		  "protocol_list", &chain);

  /* const struct method_list_t * const instance_methods; */
  add_field_decl (objc_method_proto_list_ptr,  "instance_methods", &chain);

  /* const struct method_list_t * const class_methods; */
  add_field_decl (objc_method_proto_list_ptr, "class_methods", &chain);

  /* const struct method_list_t * optionalInstanceMethods; */
  add_field_decl (objc_method_proto_list_ptr, "optionalInstanceMethods", &chain);

  /* const struct method_list_t * optionalClassMethods; */
  add_field_decl (objc_method_proto_list_ptr, "optionalClassMethods", &chain);

  /* struct _prop_list_t * properties; */
  add_field_decl (objc_prop_list_ptr, "properties", &chain);

  /* const uint32_t size; */
  add_field_decl (integer_type_node, "size", &chain);

  /* const uint32_t flags; */
  add_field_decl (integer_type_node, "flags", &chain);

  objc_finish_struct (objc_v2_protocol_template, decls);
}

/* Build type for a category:
   struct category_t
   {
     const char * const name;
     struct class_t *const cls;
     const struct method_list_t * const instance_methods;
     const struct method_list_t * const class_methods;
     const struct protocol_list_t * const protocols;
     const struct _prop_list_t * const properties;
   }
*/

static void
build_v2_category_template (void)
{
  tree decls, *chain = NULL;

  objc_v2_category_template =
	objc_start_struct (get_identifier ("_category_t"));

  /* char *name; */
  decls = add_field_decl (string_type_node, "name", &chain);

  /* struct class_t *const cls; */
  add_field_decl (build_pointer_type (objc_v2_class_template), "cls", &chain);

  /* struct method_list_t *instance_methods; */
  add_field_decl (objc_method_list_ptr, "instance_methods", &chain);

  /* struct method_list_t *class_methods; */
  add_field_decl (objc_method_list_ptr, "class_methods", &chain);

  /* struct protocol_list_t *protocol_list; */
  add_field_decl (build_pointer_type (objc_v2_protocol_template),
                  "protocol_list", &chain );

  /* struct _prop_list_t * properties; */
  add_field_decl (objc_prop_list_ptr, "properties", &chain);

  objc_finish_struct (objc_v2_category_template, decls);
}

/* NOTE --- Decls, Identifiers, Names etc. --- */

/* This routine is given a name and returns a matching extern variable
   if one is found.  */

static tree
hash_name_lookup (hash *hashlist, tree name)
{
  hash target;

  target = hashlist[IDENTIFIER_HASH_VALUE (name) % SIZEHASHTABLE];

  while (target)
    {
      if (name == DECL_NAME (target->key))
	return target->key;

      target = target->next;
    }
  return 0;
}

/* This routine is given an extern variable and enters it in its hash
   table.  Note that hashing is done on its inner IDENTIFIER_NODE
   node.  */

static void
hash_name_enter (hash *hashlist, tree id)
{
  hash obj;
  int slot = IDENTIFIER_HASH_VALUE (DECL_NAME (id)) % SIZEHASHTABLE;

  obj = ggc_alloc_hashed_entry ();
  obj->list = 0;
  obj->next = hashlist[slot];
  obj->key = id;

  hashlist[slot] = obj;		/* append to front */
}

/* Create a declaration "extern <type> <name>;"
   The var will need to be finalized (e.g. by calling finish_var_decl()).  */

static tree
create_extern_decl (tree type, const char *name)
{
  tree id = get_identifier (name);
  tree var = hash_name_lookup (extern_names, id);
  if (var)
      return var;
  /* New name. */
  var = start_var_decl (type, name);
  TREE_STATIC (var) = 0;
  DECL_EXTERNAL (var) = 1;
  TREE_PUBLIC (var) = 1;
  hash_name_enter (extern_names, var);
  return var;
}

/* Create a globally visible definition for variable NAME of a given TYPE. The
   finish_var_decl() routine will need to be called on it afterwards.  */

static tree
create_global_decl (tree type, const char *name)
{
  tree id = get_identifier (name);
  tree var = hash_name_lookup (extern_names, id);
  if (var)
    {
      DECL_EXTERNAL (var) = 0;
      TREE_STATIC (var) = 1;
    }
  else
    {
      var = start_var_decl (type, name);
      hash_name_enter (extern_names, var);
    }
  TREE_PUBLIC (var) = 1;
  return var;
}

/* Create a symbol with __attribute__ ((visibility ("hidden")))
   attribute (private extern).  */

static tree
create_hidden_decl (tree type, const char *name)
{
    tree decl = create_global_decl (type, name);
    DECL_VISIBILITY (decl) = VISIBILITY_HIDDEN;
    DECL_VISIBILITY_SPECIFIED (decl) = 1;
    return decl;
}

/* Irritatingly, we have a different superclass field name for ABI=2.  */
/* PS/TODO: The field name does not matter, it is only used internally
   by the compiler.  We can rename it to whatever we want. ;-) */

static tree
next_runtime_abi_02_super_superclassfield_id (void)
{
  /* TODO: Simplify.  Just always return get_identifier ("cls"), or at
     most look it once at startup then always return it.  */
  if (!super_superclassfield_id)
    super_superclassfield_id = get_identifier ("cls");
  return super_superclassfield_id;
}

static tree
next_runtime_abi_02_class_decl (tree klass)
{
  tree decl;
  char buf[BUFSIZE];
  snprintf (buf, BUFSIZE, "OBJC_CLASS_$_%s",
	    IDENTIFIER_POINTER (CLASS_NAME (klass)));
  /* ObjC2 classes are extern visible.  */
  decl = create_global_decl (objc_v2_class_template, buf);
  OBJCMETA (decl, objc_meta, meta_class);
  return decl;
}

static tree
next_runtime_abi_02_metaclass_decl (tree klass)
{
  tree decl;
  char buf[BUFSIZE];
  snprintf (buf, BUFSIZE, "OBJC_METACLASS_$_%s",
	    IDENTIFIER_POINTER (CLASS_NAME (klass)));
  /* ObjC2 classes are extern visible.  */
  decl = create_global_decl (objc_v2_class_template, buf);
  OBJCMETA (decl, objc_meta, meta_metaclass);
  return decl;
}

static tree
next_runtime_abi_02_category_decl (tree klass)
{
  tree decl;
  char buf[BUFSIZE];
  snprintf (buf, BUFSIZE, "_OBJC_Category_%s_on_%s",
	    IDENTIFIER_POINTER (CLASS_SUPER_NAME (klass)),
	    IDENTIFIER_POINTER (CLASS_NAME (klass)));
  decl = start_var_decl (objc_v2_category_template, buf);
  OBJCMETA (decl, objc_meta, meta_category);
  return decl;
}

static tree
next_runtime_abi_02_protocol_decl (tree p)
{
  tree decl;
  char buf[BUFSIZE];

  /* static struct _objc_protocol _OBJC_Protocol_<mumble>; */
  snprintf (buf, BUFSIZE, "_OBJC_Protocol_%s",
	    IDENTIFIER_POINTER (PROTOCOL_NAME (p)));
  decl = start_var_decl (objc_v2_protocol_template, buf);
  OBJCMETA (decl, objc_meta, meta_protocol);
  return decl;
}

static tree
next_runtime_abi_02_string_decl (tree type, const char *name,  string_section where)
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

/* NOTE --- entry --- */

typedef struct GTY(()) ident_data_tuple {
  tree ident;
  tree data;
} ident_data_tuple ;

/* This routine creates a file scope static variable of type 'Class'
   to hold the address of a class.  */

static tree
build_v2_class_reference_decl (tree ident)
{
  tree decl;
  char buf[BUFSIZE];

  snprintf (buf, BUFSIZE, "_OBJC_ClassRef_%s", IDENTIFIER_POINTER (ident));
  decl = start_var_decl (objc_class_type, buf);
  OBJCMETA (decl, objc_meta, meta_class_ref);
  return decl;
}

/* This routine builds a class refs entry for each class name used.
   Initially, a (static-ref, IDENT) tuple is added to the list.  The
   ident is replaced with address of the class metadata (of type
   'Class') in the output routine.  */

static GTY (()) vec<ident_data_tuple, va_gc> *classrefs;

static tree
objc_v2_get_class_reference (tree ident)
{
  tree decl;
  ident_data_tuple e;
  if (classrefs)
    {
      int count;
      ident_data_tuple *ref;
      FOR_EACH_VEC_ELT (*classrefs, count, ref)
	{
	  if (ref->ident == ident)
	    {
	      if (!ref->data)
		ref->data = build_v2_class_reference_decl (ident);
	      return ref->data;
	    }
	}
    }
  else
    /* Somewhat arbitrary initial provision.  */
    vec_alloc (classrefs, 16);

  /* We come here if we don't find the entry - or if the table was yet
     to be created.  */
  decl = build_v2_class_reference_decl (ident);
  e.ident = ident;
  e.data = decl;
  vec_safe_push (classrefs, e);
  return decl;
}

static tree
next_runtime_abi_02_get_class_reference (tree ident)
{
  if (!flag_zero_link)
    return objc_v2_get_class_reference (ident);
  else
    {
      /* We fall back to using objc_getClass ().  */
      vec<tree, va_gc> *v;
      vec_alloc (v, 1);
      tree t;
      /* ??? add_class_reference (ident); - is pointless, since the
         system lib does not export the equivalent symbols.  Maybe we
         need to build a class ref anyway.  */
      t = my_build_string_pointer (IDENTIFIER_LENGTH (ident) + 1,
				   IDENTIFIER_POINTER (ident));
      v->quick_push (t);
      t = build_function_call_vec (input_location, objc_get_class_decl, v, 0);
      vec_free (v);
      return t;
    }
}

/* Used by build_function_type_for_method.  Append the types for
   receiver & _cmd at the start of a method argument list to ARGTYPES.
   CONTEXT is either METHOD_DEF or METHOD_REF, saying whether we are
   trying to define a method or call one.  SUPERFLAG says this is for a
   send to super.  METH may be NULL, in the case that there is no
   prototype.  */

static void
next_runtime_abi_02_get_arg_type_list_base (vec<tree, va_gc> **argtypes,
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
  vec_safe_push (*argtypes,
		 superflag ? objc_v2_super_selector_type
		           : objc_v2_selector_type);
}

/* TODO: Merge this with the message refs.  */
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
next_runtime_abi_02_build_selector_reference (location_t loc ATTRIBUTE_UNUSED,
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

/* Declare a variable of type 'struct message_ref_t'. */
/* This will be finished in build_v2_message_ref_translation_table ().
   We take an idea from LLVM in making the names a bit more connected
   and thus the asm more readable.  */

static tree
build_v2_message_reference_decl (tree sel_name, tree message_func_ident)
{
  tree decl;
  char buf[BUFSIZE], *t;
  int offset = 12;

  /* Skip past the objc_msgSend it's the same for all...  */
  if (IDENTIFIER_POINTER (message_func_ident)[offset] == '_')
    offset++;

  snprintf (buf, BUFSIZE, "_OBJC_MsgRef_%s_%s",
	    &(IDENTIFIER_POINTER (message_func_ident)[offset]),
	    IDENTIFIER_POINTER (sel_name));
  t = buf;
  while (*t)
    {
      if (*t==':')
        *t = '$'; /* Underscore would clash between foo:bar and foo_bar.  */
      t++;
    }
  decl = start_var_decl (objc_v2_message_ref_template, buf);
  OBJCMETA (decl, objc_meta, meta_mref);
  return decl;
}

typedef struct GTY(()) msgref_entry {
  tree func;
  tree selname;
  tree refdecl;
} msgref_entry;

static GTY (()) vec<msgref_entry, va_gc> *msgrefs;

/* Build the list of (objc_msgSend_fixup_xxx, selector name), used
   later on to initialize the table of 'struct message_ref_t'
   elements.  */

static tree
build_v2_selector_messenger_reference (tree sel_name, tree message_func_decl)
{
  tree decl;
  msgref_entry e;
  if (msgrefs)
    {
      int count;
      msgref_entry *ref;
      FOR_EACH_VEC_ELT (*msgrefs, count, ref)
	if (ref->func == message_func_decl && ref->selname == sel_name)
	  return ref->refdecl;
    }
  else
    /* Somewhat arbitrary initial provision.  */
    vec_alloc (msgrefs, 32);

  /* We come here if we don't find a match or at the start.  */
  decl = build_v2_message_reference_decl (sel_name,
					  DECL_NAME (message_func_decl));
  e.func = message_func_decl;
  e.selname = sel_name;
  e.refdecl = decl;
  vec_safe_push (msgrefs, e);
  return decl;
}

static tree
build_v2_protocollist_ref_decl (tree protocol)
{
  tree decl;
  tree protocol_ident = PROTOCOL_NAME (protocol);
  char buf[BUFSIZE];

  snprintf (buf, BUFSIZE, "_OBJC_ProtocolRef_%s",
	    IDENTIFIER_POINTER (protocol_ident));
  /* TODO: other compiler versions make these hidden & weak.  */
  decl = create_global_decl (objc_protocol_type, buf);
  /* Let optimizer know that this decl is not removable.  */
  DECL_PRESERVE_P (decl) = 1;
  OBJCMETA (decl, objc_meta, meta_proto_ref);
  return decl;
}

typedef struct GTY(()) prot_list_entry {
  tree id;
  tree refdecl;
} prot_list_entry;
static GTY (()) vec<prot_list_entry, va_gc> *protrefs;

static tree
objc_v2_get_protocol_reference (tree ident)
{
  tree decl;
  prot_list_entry e;
  if (protrefs)
    {
      int count;
      prot_list_entry *ref;
      FOR_EACH_VEC_ELT (*protrefs, count, ref)
	{
	  if (ref->id == ident)
	    {
	      if (!ref->refdecl)
		ref->refdecl = build_v2_protocollist_ref_decl (ident);
	      return ref->refdecl;
	    }
	}
    }
  else
    /* Somewhat arbitrary initial provision.  */
    vec_alloc (protrefs, 32);

  /* We come here if we don't find the entry - or if the table was yet
     to be created.  */
  decl = build_v2_protocollist_ref_decl (ident);
  e.id = ident;
  e.refdecl = decl;
  vec_safe_push (protrefs, e);
  return decl;
}

static tree
next_runtime_abi_02_get_protocol_reference (location_t loc ATTRIBUTE_UNUSED,
					    tree p)
{
  if (!PROTOCOL_FORWARD_DECL (p))
    PROTOCOL_FORWARD_DECL (p) = next_runtime_abi_02_protocol_decl (p);

  return objc_v2_get_protocol_reference (p);
}

/* This routine returns the ivar declaration, if component is a valid
   ivar field; NULL_TREE otherwise. On finding an ivar, it also
   returns the class name in CLASS.  */

static tree
objc_is_ivar (tree expr, tree component, tree *klass)
{
  tree field = NULL_TREE;
  tree basetype = TYPE_MAIN_VARIANT (TREE_TYPE (expr));

  if (TREE_CODE (basetype) == RECORD_TYPE
      && TYPE_HAS_OBJC_INFO (basetype) && TYPE_OBJC_INTERFACE (basetype))
    {
      *klass = lookup_interface (OBJC_TYPE_NAME (basetype));
      if (*klass)
	{
	  do
	    {
	      tree ivar_chain = CLASS_RAW_IVARS (*klass);
	      if (ivar_chain)
		{
		  field = is_ivar (ivar_chain, component);
		  if (field != NULL_TREE)
		    break;
	        }
	      *klass = lookup_interface (CLASS_SUPER_NAME (*klass));
	    }
	  while (*klass);
	}
    }
  return field;
}

static void
create_ivar_offset_name (char *buf, tree class_name, tree field_decl)
{
  tree fname = DECL_NAME (field_decl);

  sprintf (buf, "OBJC_IVAR_$_%s.%s", IDENTIFIER_POINTER (class_name),
	   IDENTIFIER_POINTER (fname));
  return;
}

/* This routine generates new abi's ivar reference tree.  It amounts
   to generating *(TYPE*)((char*)pObj + OFFSET_IVAR) when we normally
   generate pObj->IVAR.  OFFSET_IVAR is an 'extern' variable holding
   the offset for 'IVAR' field.  TYPE is type of IVAR field.  */

static tree
objc_v2_build_ivar_ref (tree datum, tree component)
{
  tree field, ref, class_name, offset, ftype, expr;
  char var_offset_name[512];

  field = objc_is_ivar (datum, component, &class_name);
  if (!field)
    return NULL_TREE;

  /* This routine only handles non-bitfield fields */
  /* DECL_INITIAL macro is set to width of bitfield and can be relied
     on to check for bitfield ivars.  Note that I cannot rely on
     DECL_BIT_FIELD macro because it is only set when the whole struct
     is seen (at finish_struct) and not when the ivar chain is
     built.  */
  if (DECL_INITIAL (field))
    return NULL_TREE;

  create_ivar_offset_name (var_offset_name, CLASS_NAME (class_name),  field);

  offset = create_extern_decl (TREE_TYPE (size_zero_node), var_offset_name);

  ftype = TREE_TYPE (field);

  /* (char*)datum */
  expr = build_c_cast (input_location,
		       string_type_node, build_fold_addr_expr (datum));

  /* (char*)datum + offset */
  expr = fold_build_pointer_plus_loc (input_location, expr, offset);

  /* (ftype*)((char*)datum + offset) */
  expr = build_c_cast (input_location, build_pointer_type (ftype), expr);

  /* Finally: *(ftype*)((char*)datum + offset) */
  ref = build_indirect_ref (input_location, expr, RO_UNARY_STAR);

  /* We must set type of the resulting expression to be the same as
     the field type. This is because, build_indirect_ref (...)
     rebuilds the type which may result in lost information; as in the
     case of protocol-qualified types (id <protocol> ).  */
  TREE_TYPE (ref) = ftype;

  if (TREE_READONLY (datum) || TREE_READONLY (field))
    TREE_READONLY (ref) = 1;

  if (TREE_THIS_VOLATILE (datum) || TREE_THIS_VOLATILE (field))
    TREE_THIS_VOLATILE (ref) = 1;

  if (TREE_DEPRECATED (field))
    warn_deprecated_use (field, NULL_TREE);

  return ref;
}

/* IVAR refs are made via an externally referenceable offset and built
   on the fly.  That is, unless they refer to (private) fields in  the
   class structure.  */
static tree
next_runtime_abi_02_build_ivar_ref (location_t loc ATTRIBUTE_UNUSED,
				   tree base, tree id)
{
  tree ivar;
  if ((ivar = objc_v2_build_ivar_ref (base, id)))
    return ivar;
  return objc_build_component_ref (base, id);
}

/* [super ...] references are listed here (and built into a table at
   meta -data emit time).  */
static tree
build_v2_superclass_ref_decl (tree ident, bool inst)
{
  tree decl;
  char buf[BUFSIZE];

  snprintf (buf, BUFSIZE, "_OBJC_%sSuperRef_%s", (inst?"":"Meta"),
	    IDENTIFIER_POINTER (ident));
  decl = start_var_decl (objc_class_type, buf);
  OBJCMETA (decl, objc_meta, meta_superclass_ref);
  return decl;
}

static GTY (()) vec<ident_data_tuple, va_gc> *class_super_refs;
static GTY (()) vec<ident_data_tuple, va_gc> *metaclass_super_refs;

static tree
next_runtime_abi_02_get_class_super_ref (location_t loc ATTRIBUTE_UNUSED,
					 struct imp_entry *imp, bool inst_meth)
{
  tree decl;
  ident_data_tuple e;
  tree id = CLASS_NAME (imp->imp_context);
  vec<ident_data_tuple, va_gc> *list = inst_meth  ? class_super_refs
						: metaclass_super_refs;

  if (list)
    {
      int count;
      ident_data_tuple *ref;
      FOR_EACH_VEC_ELT (*list, count, ref)
	{
	  if (ref->ident == id)
	    {
	      if (!ref->data)
		ref->data = build_v2_superclass_ref_decl (id, inst_meth);
	      return ref->data;
	    }
	}
    }
  else
    {
      /* Somewhat arbitrary initial provision.  */
      if (inst_meth)
	{
	  vec_alloc (class_super_refs, 16);
	  list = class_super_refs;
	}
      else
	{
	  vec_alloc (metaclass_super_refs, 16);
	  list = metaclass_super_refs;
	}
    }
  /* We come here if we don't find the entry - or if the table was yet
     to be created.  */
  decl = build_v2_superclass_ref_decl (id, inst_meth);
  e.ident = id;
  e.data = decl;
  vec_safe_push (list, e);
  return decl;
}

static tree
next_runtime_abi_02_get_category_super_ref (location_t loc ATTRIBUTE_UNUSED,
					   struct imp_entry *imp, bool inst_meth)
{
  /* ??? is this OK when zero-link = true?  */
  tree super_name = CLASS_SUPER_NAME (imp->imp_template);
  tree super_class;

  if (!flag_zero_link)
    {
      super_class = objc_get_class_reference (CLASS_NAME (imp->imp_template));

      if (!inst_meth)

	/* If we are in a class method, we must retrieve the
	   _metaclass_ for the current class, pointed at by the
	   class's "isa" pointer.  The following assumes that "isa" is
	   the first ivar in a class (which it must be).  */
	   super_class =
		build_indirect_ref (input_location,
				    build_c_cast (input_location,
					build_pointer_type (objc_class_type),
					super_class),
				    RO_UNARY_STAR);
      return super_class;
    }
  /* ??? Do we need to add the class ref anway for zero-link?  */
  /* else do it the slow way.  */
  super_class = (inst_meth ? objc_get_class_decl : objc_get_meta_class_decl);
  super_name = my_build_string_pointer (IDENTIFIER_LENGTH (super_name) + 1,
					IDENTIFIER_POINTER (super_name));
  /* super_class = objc_get{Meta}Class("CLASS_SUPER_NAME"); */
  return build_function_call (input_location,
			      super_class,
			      build_tree_list (NULL_TREE, super_name));
}

static tree
next_runtime_abi_02_receiver_is_class_object (tree receiver)
{
  if (TREE_CODE (receiver) == VAR_DECL
      && IS_CLASS (TREE_TYPE (receiver))
      && vec_safe_length (classrefs))
    {
      int count;
      ident_data_tuple *ref;
      /* The receiver is a variable created by build_class_reference_decl.  */
      FOR_EACH_VEC_ELT (*classrefs, count, ref)
	if (ref->data == receiver)
	  return ref->ident;
    }
  return NULL_TREE;
}

/* Assign all arguments in VALUES which have side-effect to a temporary
   and replaced that argument in VALUES list with the temporary. The
   arguments will be passed to a function with FNTYPE.  */

static tree
objc_copy_to_temp_side_effect_params (tree fntype, tree values)
{
  tree valtail;
  function_args_iterator iter;

  /* Skip over receiver and the &_msf_ref types.  */
  function_args_iter_init (&iter, fntype);
  function_args_iter_next (&iter);
  function_args_iter_next (&iter);

  for (valtail = values; valtail;
       valtail = TREE_CHAIN (valtail), function_args_iter_next (&iter))
    {
      tree value = TREE_VALUE (valtail);
      tree type = function_args_iter_cond (&iter);
      if (type == NULL_TREE)
	break;
      if (!TREE_SIDE_EFFECTS (value))
	continue;
      /* To prevent re-evaluation.  */
      value = save_expr (value);
      add_stmt (value);
      TREE_VALUE (valtail) = value;
    }
  return values;
}

/* Build the new abi's messaging library call. It looks like:
   (*_msg.messenger) (receiver, &_msg, ...) */

static tree
build_v2_build_objc_method_call (int super_flag, tree method_prototype,
				 tree lookup_object, tree selector,
				 tree method_params,
				 bool check_for_nil)
{
  tree ret_val;
  tree sender, rcv_p, t;
  tree ret_type
    = (method_prototype
       ? TREE_VALUE (TREE_TYPE (method_prototype))
       : objc_object_type);
  tree ftype = build_function_type_for_method (ret_type, method_prototype,
					       METHOD_REF, super_flag);
  tree sender_cast;

  if (method_prototype && METHOD_TYPE_ATTRIBUTES (method_prototype))
    ftype = build_type_attribute_variant (
	      ftype, METHOD_TYPE_ATTRIBUTES (method_prototype));

  sender_cast = build_pointer_type (ftype);

  if (check_for_nil)
    method_params = objc_copy_to_temp_side_effect_params (ftype,
							  method_params);

  /* Get &message_ref_t.messenger.  */
  sender = build_c_cast (input_location,
			 build_pointer_type (super_flag
					     ? objc_v2_super_imp_type
					     : objc_v2_imp_type),
			 selector);

  sender = build_indirect_ref (input_location, sender, RO_UNARY_STAR);

  rcv_p = (super_flag ? objc_super_type : objc_object_type);

  lookup_object = build_c_cast (input_location, rcv_p, lookup_object);

  /* Use SAVE_EXPR to avoid evaluating the receiver twice.  */
  lookup_object = save_expr (lookup_object);

  method_params = tree_cons (NULL_TREE, lookup_object,
                             tree_cons (NULL_TREE, selector,
                                        method_params));
  t = build3 (OBJ_TYPE_REF, sender_cast, sender, lookup_object, size_zero_node);
  ret_val =  build_function_call (input_location, t, method_params);
  if (check_for_nil)
    {
      /* receiver != nil ? ret_val : 0 */
      tree ftree;
      tree ifexp;

      if (TREE_CODE (ret_type) == RECORD_TYPE
	  || TREE_CODE (ret_type) == UNION_TYPE)
	{
	  vec<constructor_elt, va_gc> *rtt = NULL;
	  /* ??? CHECKME. hmmm..... think we need something more
	     here.  */
	  CONSTRUCTOR_APPEND_ELT (rtt, NULL_TREE, NULL_TREE);
	  ftree = objc_build_constructor (ret_type, rtt);
	}
      else
	ftree = fold_convert (ret_type, integer_zero_node);

      ifexp = build_binary_op (input_location, NE_EXPR,
			       lookup_object,
			       fold_convert (rcv_p, integer_zero_node), 1);

#ifdef OBJCPLUS
      ret_val = build_conditional_expr (input_location,
					ifexp, ret_val, ftree,
					tf_warning_or_error);
#else
     /* ??? CHECKME.   */
      ret_val = build_conditional_expr (input_location,
					ifexp, 1,
					ret_val, NULL_TREE,
					ftree, NULL_TREE);
#endif
    }
  return ret_val;
}

static tree
next_runtime_abi_02_build_objc_method_call (location_t loc,
					    tree method_prototype,
					    tree receiver,
					    tree rtype,
					    tree sel_name,
					    tree method_params,
					    int super)
{
  tree ret_type, selector;
  tree message_func_decl;
  bool check_for_nil = flag_objc_nilcheck;

  ret_type = method_prototype
	     ? TREE_VALUE (TREE_TYPE (method_prototype))
	     : objc_object_type;

  /* Do we need to check for nil receivers ? */
  /* For now, message sent to classes need no nil check.  In the
      future, class declaration marked as weak_import must be nil
      checked.  */
  if (super
      || (TREE_CODE (receiver) == VAR_DECL
	  && TREE_TYPE (receiver) == objc_class_type))
    check_for_nil = false;

  if (!targetm.calls.struct_value_rtx (0, 0)
          && (TREE_CODE (ret_type) == RECORD_TYPE
	      || TREE_CODE (ret_type) == UNION_TYPE)
          && targetm.calls.return_in_memory (ret_type, 0))
    {
      if (super)
	message_func_decl = umsg_id_super2_stret_fixup_decl;
      else
	message_func_decl = objc_is_id (rtype)
			    ? umsg_id_stret_fixup_decl
			    : umsg_stret_fixup_decl;
    }
  else
    {
      if (super)
	message_func_decl = umsg_id_super2_fixup_decl;
      else
	message_func_decl = objc_is_id (rtype)
			    ? umsg_id_fixup_decl
			    : umsg_fixup_decl;
    }

  selector = build_v2_selector_messenger_reference (sel_name,
						      message_func_decl);

  /* selector = &_msg; */
  selector = build_unary_op (loc, ADDR_EXPR, selector, 0);

  selector = build_c_cast (loc, (super ? objc_v2_super_selector_type
				       : objc_v2_selector_type),
			   selector);

  /* (*_msg.messenger) (receiver, &_msg, ...); */
  return build_v2_build_objc_method_call (super, method_prototype,
					  receiver, selector,
					  method_params, check_for_nil);
}

/* NOTE --- Constant String Class Stuff --- */

static bool
next_runtime_abi_02_setup_const_string_class_decl (void)
{
  if (!constant_string_global_id)
    {
      /* Hopefully, this should not represent a serious limitation.  */
      char buf[BUFSIZE];
      snprintf (buf, BUFSIZE, "OBJC_CLASS_$_%s", constant_string_class_name);
      constant_string_global_id = get_identifier (buf);
    }

  string_class_decl = lookup_name (constant_string_global_id);

  /* In OBJC2 abi, constant string class reference refers to class
     name for NSConstantString class.  This declaration may not be
     available yet (in fact it is not in most cases).  So, declare an
     extern OBJC_CLASS_$_NSConstantString in its place. */
  if (!string_class_decl)
    string_class_decl =
	create_extern_decl (objc_v2_class_template,
			    IDENTIFIER_POINTER (constant_string_global_id));

  return (string_class_decl != NULL_TREE);
}

static tree
next_runtime_abi_02_build_const_string_constructor (location_t loc, tree string,
						   int length)
{
  tree constructor, fields, var;
  vec<constructor_elt, va_gc> *v = NULL;

  /* NeXT: (NSConstantString *) & ((__builtin_ObjCString) { isa, string, length }) */
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

/* NOTE --- NeXT V2 Metadata templates --- */

/* This routine builds the following type:
   struct _prop_t
   {
     const char * const name;			// property name
     const char * const attributes;		// comma-delimited, encoded,
						// property attributes
   };
*/

static tree
build_v2_property_template (void)
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

/* struct ivar_t
   {
     unsigned long int *offset;
     char *name;
     char *type;
     uint32_t alignment;
     uint32_t size;
   };
*/

static tree
build_v2_ivar_t_template (void)
{
  tree objc_ivar_id, objc_ivar_record;
  tree decls, *chain = NULL;

  objc_ivar_id = get_identifier ("_ivar_t");
  objc_ivar_record = objc_start_struct (objc_ivar_id);

  /* unsigned long int *offset; */
  decls = add_field_decl (build_pointer_type
			   (TREE_TYPE (size_zero_node)), "offset", &chain);

  /* char *name; */
  add_field_decl (string_type_node, "name", &chain);

  /* char *type; */
  add_field_decl (string_type_node, "type", &chain);

  /* uint32_t alignment; */
  add_field_decl (integer_type_node, "alignment", &chain);

  /* uint32_t size; */
  add_field_decl (integer_type_node, "size", &chain);

  objc_finish_struct (objc_ivar_record, decls);
  return objc_ivar_record;
}

static void
build_metadata_templates (void)
{

  if (!objc_method_template)
    objc_method_template = build_method_template ();

  if (!objc_v2_property_template)
    objc_v2_property_template = build_v2_property_template ();

  if (!objc_v2_ivar_template)
    objc_v2_ivar_template = build_v2_ivar_t_template ();

}

/* NOTE --- Output NeXT V2 Metadata --- */

/* Routine builds name of Interface's main meta-data of type class_t. */

static char *
objc_build_internal_classname (tree ident, bool metaclass)
{
  static char string[512];
  snprintf (string, 512, "%s_%s", metaclass ? "OBJC_METACLASS_$"
					    : "OBJC_CLASS_$",
	    IDENTIFIER_POINTER (ident));
  return string;
}

/* Build the name for object of type struct class_ro_t */

static const char *
newabi_append_ro (const char *name)
{
  const char *dollar;
  char *p;
  static char string[BUFSIZE];
  dollar = strchr (name, '$');
  gcc_assert (dollar);
  p = string;
  *p = '_'; p++;
  strncpy (p, name, (int)(dollar - name));
  p += (int)(dollar - name);
  sprintf (p, "RO_%s", dollar);
  return string;
}

/* Build the struct message_ref_t msg =
	       {objc_msgSend_fixup_xxx, @selector(func)}
   table.  */

static
void build_v2_message_ref_translation_table (void)
{
  int count;
  msgref_entry *ref;

  if (!vec_safe_length (msgrefs))
    return;

  FOR_EACH_VEC_ELT (*msgrefs, count, ref)
    {
      vec<constructor_elt, va_gc> *initializer;
      tree expr, constructor;
      tree struct_type = TREE_TYPE (ref->refdecl);
      location_t loc = DECL_SOURCE_LOCATION (ref->refdecl);

      initializer = NULL;
      /* First 'IMP messenger' field...  */
      expr = build_unary_op (loc, ADDR_EXPR, ref->func, 0);
      expr = convert (objc_v2_imp_type, expr);
      CONSTRUCTOR_APPEND_ELT (initializer, NULL_TREE, expr);

      /* ... then 'SEL name' field.  */
      expr = build_selector (ref->selname);
      CONSTRUCTOR_APPEND_ELT (initializer, NULL_TREE, expr);
      constructor = objc_build_constructor (struct_type, initializer);
      finish_var_decl (ref->refdecl, constructor);
    }
}

/* Build decl = initializer; for each externally visible class
   reference.  */

static void
build_v2_classrefs_table (void)
{
  int count;
  ident_data_tuple *ref;

  if (!vec_safe_length (classrefs))
    return;

  FOR_EACH_VEC_ELT (*classrefs, count, ref)
    {
      tree expr = ref->ident;
      tree decl = ref->data;
      /* Interface with no implementation and yet one of its messages
	 has been used. Need to generate a full address-of tree for it
	 here.  */
      if (TREE_CODE (expr) == IDENTIFIER_NODE)
        {
          const char *name = objc_build_internal_classname (expr, false);
          expr = create_extern_decl (objc_v2_class_template, name);
	  expr = convert (objc_class_type, build_fold_addr_expr (expr));
	}
      /* The runtime wants this, even if it appears unused, so we must force the
	 output.
      DECL_PRESERVE_P (decl) = 1; */
      finish_var_decl (decl, expr);
    }
}

/* Build decl = initializer; for each externally visible super class
   reference.  */

static void
build_v2_super_classrefs_table (bool metaclass)
{
  int count;
  ident_data_tuple *ref;
  vec<ident_data_tuple, va_gc> *list = metaclass  ? metaclass_super_refs
						: class_super_refs;

  if (!vec_safe_length (list))
    return;

  FOR_EACH_VEC_ELT (*list, count, ref)
    {
      tree expr = ref->ident;
      tree decl = ref->data;
      /* Interface with no implementation and yet one of its messages
	 has been used. Need to generate a full address-of tree for it
	 here.  */
      if (TREE_CODE (expr) == IDENTIFIER_NODE)
	{
	  const char * name = objc_build_internal_classname (expr, metaclass);
          expr = create_extern_decl (objc_v2_class_template, name);
	  expr = convert (objc_class_type, build_fold_addr_expr (expr));
	}
      finish_var_decl (decl, expr);
    }
}

/* Add the global class meta-data declaration to the list which later
   on ends up in the __class_list section.  */

static GTY(()) vec<tree, va_gc> *class_list;

static void
objc_v2_add_to_class_list (tree global_class_decl)
{
  vec_safe_push (class_list, global_class_decl);
}

static GTY(()) vec<tree, va_gc> *nonlazy_class_list;

/* Add the global class meta-data declaration to the list which later
   on ends up in the __nonlazy_class section.  */

static void
objc_v2_add_to_nonlazy_class_list (tree global_class_decl)
{
  vec_safe_push (nonlazy_class_list, global_class_decl);
}

static GTY(()) vec<tree, va_gc> *category_list;

/* Add the category meta-data declaration to the list which later on
   ends up in the __nonlazy_category section.  */

static void
objc_v2_add_to_category_list (tree decl)
{
  vec_safe_push (category_list, decl);
}

static GTY(()) vec<tree, va_gc> *nonlazy_category_list;

/* Add the category meta-data declaration to the list which later on
   ends up in the __category_list section.  */

static void
objc_v2_add_to_nonlazy_category_list (tree decl)
{
  vec_safe_push (nonlazy_category_list, decl);
}

static bool
has_load_impl (tree clsmeth)
{
  while (clsmeth)
    {
      tree id = METHOD_SEL_NAME (clsmeth);
      if (IDENTIFIER_LENGTH (id) == 4
	  && strncmp (IDENTIFIER_POINTER (id), "load", 4) == 0)
        return true;
      clsmeth = DECL_CHAIN (clsmeth);
    }

  return false;
}

/* Build a __{class,category}_list section table containing address of
   all @implemented {class,category} meta-data.  */

static void
build_v2_address_table (vec<tree, va_gc> *src, const char *nam, tree attr)
{
  int count=0;
  tree type, decl, expr;
  vec<constructor_elt, va_gc> *initlist = NULL;

  if (!vec_safe_length (src))
    return;

  FOR_EACH_VEC_ELT (*src, count, decl)
    {
#ifndef OBJCPLUS
      tree purpose = build_int_cst (NULL_TREE, count);
#else
      tree purpose = NULL_TREE;
#endif
      expr = convert (objc_class_type, build_fold_addr_expr (decl));
      CONSTRUCTOR_APPEND_ELT (initlist, purpose, expr);
    }
  gcc_assert (count > 0);
  type = build_array_type (objc_class_type,
			   build_index_type (build_int_cst (NULL_TREE, count - 1)));
  decl = start_var_decl (type, nam);
  /* The runtime wants this, even if it appears unused, so we must
     force the output.  */
  DECL_PRESERVE_P (decl) = 1;
  expr = objc_build_constructor (type, initlist);
  OBJCMETA (decl, objc_meta, attr);
  finish_var_decl (decl, expr);
}

/* Build decl = initializer; for each protocol referenced in
   @protocol(MyProt) expression.  Refs as built in the entry section
   above.  */

static void
build_v2_protocol_list_translation_table (void)
{
  int count;
  prot_list_entry *ref;

  if (!protrefs)
    return;

  FOR_EACH_VEC_ELT (*protrefs, count, ref)
    {
      char buf[BUFSIZE];
      tree expr;
      gcc_assert (TREE_CODE (ref->id) == PROTOCOL_INTERFACE_TYPE);
      snprintf (buf, BUFSIZE, "_OBJC_Protocol_%s",
		IDENTIFIER_POINTER (PROTOCOL_NAME (ref->id)));
      expr = start_var_decl (objc_v2_protocol_template, buf);
      expr = convert (objc_protocol_type, build_fold_addr_expr (expr));
      finish_var_decl (ref->refdecl, expr);
    }
  /* TODO: Maybe we could explicitly delete the vec. now?  */
}

static GTY (()) vec<prot_list_entry, va_gc> *protlist;

/* Add the local protocol meta-data declaration to the list which
   later on ends up in the __protocol_list section.  */

static void
objc_add_to_protocol_list (tree protocol_interface_decl, tree protocol_decl)
{
  prot_list_entry e;
  if (!protlist)
    /* Arbitrary init count.  */
    vec_alloc (protlist, 32);
  e.id = protocol_interface_decl;
  e.refdecl = protocol_decl;
  vec_safe_push (protlist, e);
}

/* Build the __protocol_list section table containing address of all
   generate protocol_t meta-data.  */

static void
build_v2_protocol_list_address_table (void)
{
  int count;
  prot_list_entry *ref;
  if (!vec_safe_length (protlist))
    return;

  FOR_EACH_VEC_ELT (*protlist, count, ref)
    {
      tree decl, expr;
      char buf[BUFSIZE];
      gcc_assert (ref->id && TREE_CODE (ref->id) == PROTOCOL_INTERFACE_TYPE);
      snprintf (buf, BUFSIZE, "_OBJC_LabelProtocol_%s",
		IDENTIFIER_POINTER (PROTOCOL_NAME (ref->id)));
      decl = create_global_decl (objc_protocol_type, buf);
      expr = convert (objc_protocol_type, build_fold_addr_expr (ref->refdecl));
      OBJCMETA (decl, objc_meta, meta_label_protocollist);
      finish_var_decl (decl, expr);
    }

    /* TODO: delete the vec.  */
    /* TODO: upgrade to the clang/llvm hidden version.  */
}

/* This routine declares a variable to hold meta data for 'struct
   protocol_list_t'.  */

static tree
generate_v2_protocol_list (tree i_or_p, tree klass_ctxt)
{
  tree refs_decl, lproto, e, plist, ptempl_p_t;
  int size = 0;
  vec<constructor_elt, va_gc> *initlist = NULL;
  char buf[BUFSIZE];

  if (TREE_CODE (i_or_p) == CLASS_INTERFACE_TYPE
      || TREE_CODE (i_or_p) == CATEGORY_INTERFACE_TYPE)
    plist = CLASS_PROTOCOL_LIST (i_or_p);
  else if (TREE_CODE (i_or_p) == PROTOCOL_INTERFACE_TYPE)
    plist = PROTOCOL_LIST (i_or_p);
  else
    gcc_unreachable ();

  /* Compute size.  */
  for (lproto = plist; lproto; lproto = TREE_CHAIN (lproto))
    if (TREE_CODE (TREE_VALUE (lproto)) == PROTOCOL_INTERFACE_TYPE
	&& PROTOCOL_FORWARD_DECL (TREE_VALUE (lproto)))
      size++;

  /* Build initializer.  */

  ptempl_p_t = build_pointer_type (objc_v2_protocol_template);
  e = build_int_cst (ptempl_p_t, size);
  CONSTRUCTOR_APPEND_ELT (initlist, NULL_TREE, e);

  for (lproto = plist; lproto; lproto = TREE_CHAIN (lproto))
    {
      tree pval = TREE_VALUE (lproto);

      if (TREE_CODE (pval) == PROTOCOL_INTERFACE_TYPE
	  && PROTOCOL_FORWARD_DECL (pval))
	{
	  tree fwref = PROTOCOL_FORWARD_DECL (pval);
	  location_t loc = DECL_SOURCE_LOCATION (fwref) ;
	  e = build_unary_op (loc, ADDR_EXPR, fwref, 0);
	  CONSTRUCTOR_APPEND_ELT (initlist, NULL_TREE, e);
	}
    }

  /* static struct protocol_list_t *list[size]; */

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

  refs_decl = start_var_decl (build_sized_array_type (ptempl_p_t, size+1),
			      buf);
  /* ObjC2 puts all these in the base section.  */
  OBJCMETA (refs_decl, objc_meta, meta_base);
  DECL_PRESERVE_P (refs_decl) = 1;
  finish_var_decl (refs_decl,
		   objc_build_constructor (TREE_TYPE (refs_decl),initlist));
  return refs_decl;
}

/* This routine builds one 'struct method_t' initializer list. Note
   that the old ABI is supposed to build 'struct objc_method' which
   has 3 fields, but it does not build the initialization expression
   for 'method_imp' which for protocols is NULL any way.  To be
   consistent with declaration of 'struct method_t', in the new ABI we
   set the method_t.imp to NULL.  */

static tree
build_v2_descriptor_table_initializer (tree type, tree entries)
{
  vec<constructor_elt, va_gc> *initlist = NULL;
  do
    {
      vec<constructor_elt, va_gc> *eltlist = NULL;
      CONSTRUCTOR_APPEND_ELT (eltlist, NULL_TREE,
			      build_selector (METHOD_SEL_NAME (entries)));
      CONSTRUCTOR_APPEND_ELT (eltlist, NULL_TREE,
			      add_objc_string (METHOD_ENCODING (entries),
						meth_var_types));
      CONSTRUCTOR_APPEND_ELT (eltlist, NULL_TREE, null_pointer_node);

      CONSTRUCTOR_APPEND_ELT (initlist, NULL_TREE,
			      objc_build_constructor (type, eltlist));
      entries = TREE_CHAIN (entries);
    }
  while (entries);

  return objc_build_constructor (build_array_type (type, 0), initlist);
}

/* struct method_list_t
  {
     uint32_t entsize;
     uint32_t method_count;
     struct objc_method method_list[method_count];
   }; */

static tree
build_v2_method_list_template (tree list_type, int size)
{
  tree method_list_t_record;
  tree array_type, decls, *chain = NULL;

  method_list_t_record = objc_start_struct (NULL_TREE);

  /* uint32_t const entsize; */
  decls = add_field_decl (integer_type_node, "entsize", &chain);

  /* int method_count; */
  add_field_decl (integer_type_node, "method_count", &chain);

  /* struct objc_method method_list[]; */
  array_type = build_sized_array_type (list_type, size);
  add_field_decl (array_type, "method_list", &chain);

  objc_finish_struct (method_list_t_record, decls);
  return method_list_t_record;
}

/* Note, as above that we are building to the objc_method_template
   which has the *imp field.  ABI0/1 build with
   objc_method_prototype_template which is missing this field.  */
static tree
generate_v2_meth_descriptor_table (tree chain, tree protocol,
				   const char *prefix, tree attr)
{
  tree method_list_template, initlist, decl, methods;
  int size, entsize;
  vec<constructor_elt, va_gc> *v = NULL;
  char buf[BUFSIZE];

  if (!chain || !prefix)
    return NULL_TREE;

  methods = chain;
  size = 0;
  while (methods)
    {
      if (! METHOD_ENCODING (methods))
	METHOD_ENCODING (methods) = encode_method_prototype (methods);
      methods = TREE_CHAIN (methods);
      size++;
    }

  gcc_assert (size);
  method_list_template = build_v2_method_list_template (objc_method_template,
							size);
  snprintf (buf, BUFSIZE, "%s_%s", prefix,
	    IDENTIFIER_POINTER (PROTOCOL_NAME (protocol)));

  decl = start_var_decl (method_list_template, buf);

  entsize = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (objc_method_template));
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, build_int_cst (NULL_TREE, entsize));
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, build_int_cst (NULL_TREE, size));
  initlist =
	build_v2_descriptor_table_initializer (objc_method_template,
					    chain);
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, initlist);
  /* Get into the right section.  */
  OBJCMETA (decl, objc_meta, attr);
  finish_var_decl (decl, objc_build_constructor (method_list_template, v));
  return decl;
}

/* This routine builds the initializer list to initialize the 'struct
   _prop_t prop_list[]' field of 'struct _prop_list_t' meta-data.  */

static tree
build_v2_property_table_initializer (tree type, tree context)
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
      /* NOTE! sections where property name/attribute go MUST change
	 later.  */
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

/* This routine builds the following type:
   struct _prop_list_t
   {
     uint32_t entsize;			// sizeof (struct _prop_t)
     uint32_t prop_count;
     struct _prop_t prop_list [prop_count];
   }
*/

static tree
build_v2_property_list_template (tree list_type, int size)
{
  tree property_list_t_record;
  tree array_type, decls, *chain = NULL;

  /* anonymous.  */
  property_list_t_record = objc_start_struct (NULL_TREE);

  /* uint32_t const entsize; */
  decls = add_field_decl (integer_type_node, "entsize", &chain);

  /* int prop_count; */
  add_field_decl (integer_type_node, "prop_count", &chain);

  /* struct _prop_t prop_list[]; */
  array_type = build_sized_array_type (list_type, size);
  add_field_decl (array_type, "prop_list", &chain);

  objc_finish_struct (property_list_t_record, decls);
  return property_list_t_record;
}

/* Top-level routine to generate property tables for each
   implementation.  */

static tree
generate_v2_property_table (tree context, tree klass_ctxt)
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

  property_list_template =
	build_v2_property_list_template (objc_v2_property_template,
					 size);

  initlist = build_v2_property_table_initializer (objc_v2_property_template,
						  is_proto ? context
							   : klass_ctxt);

  init_val = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (objc_v2_property_template));
  if (is_proto)
    snprintf (buf, BUFSIZE, "_OBJC_ProtocolPropList_%s",
	      IDENTIFIER_POINTER (PROTOCOL_NAME (context)));
  else
    snprintf (buf, BUFSIZE, "_OBJC_ClassPropList_%s",
	      IDENTIFIER_POINTER (CLASS_NAME (klass_ctxt)));

  decl = start_var_decl (property_list_template, buf);

  CONSTRUCTOR_APPEND_ELT (inits, NULL_TREE,
			  build_int_cst (NULL_TREE, init_val));
  CONSTRUCTOR_APPEND_ELT (inits, NULL_TREE,
			  build_int_cst (NULL_TREE, size));
  CONSTRUCTOR_APPEND_ELT (inits, NULL_TREE, initlist);

  OBJCMETA (decl, objc_meta, meta_base);
  finish_var_decl (decl, objc_build_constructor (TREE_TYPE (decl), inits));
  return decl;
}

static tree
build_v2_protocol_initializer (tree type, tree protocol_name, tree protocol_list,
			      tree inst_methods, tree class_methods,
			      tree opt_ins_meth, tree opt_cls_meth,
			      tree property_list)
{
  tree expr, ttyp;
  location_t loc;
  vec<constructor_elt, va_gc> *inits = NULL;

  /* TODO: find a better representation of location from the inputs.  */
  loc = UNKNOWN_LOCATION;

  /*  This is NULL for the new ABI.  */
  CONSTRUCTOR_APPEND_ELT (inits, NULL_TREE,
			      convert (objc_object_type, null_pointer_node));

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

  if (opt_ins_meth)
    expr = convert (ttyp, build_unary_op (loc, ADDR_EXPR, opt_ins_meth, 0));
  else
    expr = convert (ttyp, null_pointer_node);
  CONSTRUCTOR_APPEND_ELT (inits, NULL_TREE, expr);

  if (opt_cls_meth)
    expr = convert (ttyp, build_unary_op (loc, ADDR_EXPR, opt_cls_meth, 0));
  else
    expr = convert (ttyp, null_pointer_node);
  CONSTRUCTOR_APPEND_ELT (inits, NULL_TREE, expr);

  ttyp = objc_prop_list_ptr;
  if (property_list)
    expr = convert (ttyp, build_unary_op (loc, ADDR_EXPR, property_list, 0));
  else
    expr = convert (ttyp, null_pointer_node);
  CONSTRUCTOR_APPEND_ELT (inits, NULL_TREE, expr);

  /* const uint32_t size;  = sizeof(struct protocol_t) */
  expr = build_int_cst (integer_type_node,
	      TREE_INT_CST_LOW (TYPE_SIZE_UNIT (objc_v2_protocol_template)));
  CONSTRUCTOR_APPEND_ELT (inits, NULL_TREE, expr);
  /* const uint32_t flags; = 0 */
  CONSTRUCTOR_APPEND_ELT (inits, NULL_TREE, integer_zero_node);

  return objc_build_constructor (type, inits);
}

/* Main routine to build all meta data for all protocols used in a
   translation unit.  */

static void
generate_v2_protocols (void)
{
  tree p ;
  bool some = false;

  if (!protocol_chain)
    return ;

  /* If a protocol was directly referenced, pull in indirect
     references.  */
  for (p = protocol_chain; p; p = TREE_CHAIN (p))
    if (PROTOCOL_FORWARD_DECL (p) && PROTOCOL_LIST (p))
      generate_protocol_references (PROTOCOL_LIST (p));

  for (p = protocol_chain; p; p = TREE_CHAIN (p))
    {
      location_t loc;
      tree inst_meth, class_meth, opt_inst_meth, opt_class_meth, props;
      tree decl, initlist, protocol_name_expr, refs_decl, refs_expr;

      /* If protocol wasn't referenced, don't generate any code.  */
      decl = PROTOCOL_FORWARD_DECL (p);

      if (!decl)
	continue;

      loc = DECL_SOURCE_LOCATION (decl);
      some = true;

      inst_meth =
	generate_v2_meth_descriptor_table (PROTOCOL_NST_METHODS (p), p,
					   "_OBJC_ProtocolInstanceMethods",
					   meta_proto_nst_meth);

      class_meth =
	generate_v2_meth_descriptor_table (PROTOCOL_CLS_METHODS (p), p,
					   "_OBJC_ProtocolClassMethods",
					   meta_proto_cls_meth);

      opt_inst_meth =
	generate_v2_meth_descriptor_table (PROTOCOL_OPTIONAL_NST_METHODS (p), p,
					   "_OBJC_OptProtocolInstMethods",
					   meta_proto_nst_meth);

      opt_class_meth =
	generate_v2_meth_descriptor_table (PROTOCOL_OPTIONAL_CLS_METHODS (p), p,
					   "_OBJC_OptProtocolClassMethods",
					   meta_proto_cls_meth);

      if (PROTOCOL_LIST (p))
	refs_decl = generate_v2_protocol_list (p, NULL_TREE);
      else
	refs_decl = 0;

      /* static struct objc_protocol _OBJC_Protocol_<mumble>; */
      protocol_name_expr = add_objc_string (PROTOCOL_NAME (p), class_names);

      if (refs_decl)
	refs_expr = convert (build_pointer_type (objc_v2_protocol_template),
			     build_unary_op (loc, ADDR_EXPR, refs_decl, 0));
      else
	refs_expr = build_int_cst (NULL_TREE, 0);

      props = generate_v2_property_table (p, NULL_TREE);

      initlist = build_v2_protocol_initializer (TREE_TYPE (decl),
						protocol_name_expr, refs_expr,
						inst_meth, class_meth,
						opt_inst_meth, opt_class_meth,
						props);
      finish_var_decl (decl, initlist);
      objc_add_to_protocol_list (p, decl);
    }

  if (some)
    {
      /* Make sure we get the Protocol class linked in - reference
	 it...  */
      p = objc_v2_get_class_reference (get_identifier (PROTOCOL_OBJECT_CLASS_NAME));
      /* ... but since we don't specifically use the reference...  we
         need to force it.  */
      DECL_PRESERVE_P (p) = 1;
    }
}

static tree
generate_v2_dispatch_table (tree chain, const char *name, tree attr)
{
  tree decl, method_list_template, initlist;
  vec<constructor_elt, va_gc> *v = NULL;
  int size, init_val;

  if (!chain || !name || !(size = list_length (chain)))
    return NULL_TREE;

  method_list_template
	= build_v2_method_list_template (objc_method_template, size);
  initlist
	= build_dispatch_table_initializer (objc_method_template, chain);

  decl = start_var_decl  (method_list_template, name);

  init_val = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (objc_method_template));
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE,
			  build_int_cst (integer_type_node, init_val));
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
build_v2_category_initializer (tree type, tree cat_name, tree class_name,
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
  ltyp = build_pointer_type (objc_v2_protocol_template);
  if (protocol_list)
    expr = convert (ltyp, build_unary_op (loc, ADDR_EXPR, protocol_list, 0));
  else
    expr = convert (ltyp, null_pointer_node);
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, expr);

  ltyp = objc_prop_list_ptr;
  if (property_list)
    expr = convert (ltyp, build_unary_op (loc, ADDR_EXPR, property_list, 0));
  else
    expr = convert (ltyp, null_pointer_node);
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, expr);

  return objc_build_constructor (type, v);
}

/* static struct category_t _OBJC_CATEGORY_$_<name> = { ... }; */

static void
generate_v2_category (struct imp_entry *impent)
{
  tree initlist, cat_name_expr, class_name_expr;
  tree protocol_decl, category, props, t;
  tree inst_methods = NULL_TREE, class_methods = NULL_TREE;
  tree cat = impent->imp_context;
  tree cat_decl = impent->class_decl;
  location_t loc;
  char buf[BUFSIZE];

  loc = DECL_SOURCE_LOCATION (cat_decl);

  /* ??? not sure this is really necessary, the following references should
     force appropriate linkage linkage...
     -- but ... ensure a reference to the class...  */
  t = objc_v2_get_class_reference (CLASS_NAME (cat));
  /* ... which we ignore so force it out.. */
  DECL_PRESERVE_P (t) = 1;

  snprintf (buf, BUFSIZE, "OBJC_CLASS_$_%s", IDENTIFIER_POINTER (CLASS_NAME (cat)));
  class_name_expr = create_extern_decl (objc_v2_class_template, buf);
  class_name_expr = build_fold_addr_expr (class_name_expr);

  cat_name_expr = add_objc_string (CLASS_SUPER_NAME (cat), class_names);
  category = lookup_category (impent->imp_template, CLASS_SUPER_NAME (cat));

  if (category && CLASS_PROTOCOL_LIST (category))
    {
      generate_protocol_references (CLASS_PROTOCOL_LIST (category));
      protocol_decl = generate_v2_protocol_list (category, cat);
    }
  else
    protocol_decl = NULL_TREE;

/* decl = update_var_decl(impent->class_decl); */

  props = generate_v2_property_table (NULL_TREE, cat);

  if (CLASS_NST_METHODS (cat))
    {
      snprintf (buf, BUFSIZE, "_OBJC_CategoryInstanceMethods_%s_%s",
		IDENTIFIER_POINTER (CLASS_NAME (cat)),
		IDENTIFIER_POINTER (CLASS_SUPER_NAME (cat)));
      inst_methods = generate_v2_dispatch_table (CLASS_NST_METHODS (cat), buf,
						 meta_cati_meth);
    }

  if (CLASS_CLS_METHODS (cat))
    {
      snprintf (buf, BUFSIZE, "_OBJC_CategoryClassMethods_%s_%s",
		IDENTIFIER_POINTER (CLASS_NAME (cat)),
		IDENTIFIER_POINTER (CLASS_SUPER_NAME (cat)));
      class_methods = generate_v2_dispatch_table (CLASS_CLS_METHODS (cat), buf,
						  meta_catc_meth);
    }

  initlist = build_v2_category_initializer (TREE_TYPE (cat_decl),
					    cat_name_expr, class_name_expr,
					    inst_methods, class_methods,
					    protocol_decl, props, loc);

  finish_var_decl (cat_decl, initlist);
  impent->class_decl = cat_decl;

  /* Add to list of pointers in __category_list section.  */
  objc_v2_add_to_category_list (cat_decl);
  if (has_load_impl (CLASS_CLS_METHODS (impent->imp_context)))
    objc_v2_add_to_nonlazy_category_list (cat_decl);
}

/* This routine declares a variable to hold the offset for ivar
   FIELD_DECL.  Variable name is .objc_ivar.ClassName.IvarName.  */

typedef struct GTY(()) ivarref_entry
{
  tree decl;
  tree offset;
} ivarref_entry;

static GTY (()) vec<ivarref_entry, va_gc> *ivar_offset_refs;

static tree
ivar_offset_ref (tree class_name, tree field_decl)
{
  tree decl, field_decl_id;
  ivarref_entry e;
  bool global_var;
  char buf[512];

  create_ivar_offset_name (buf, class_name, field_decl);
  field_decl_id = get_identifier (buf);

  if (ivar_offset_refs)
    {
      int count;
      ivarref_entry *ref;
      FOR_EACH_VEC_ELT (*ivar_offset_refs, count, ref)
	if (DECL_NAME (ref->decl) == field_decl_id)
	  return ref->decl;
    }
  else
    /* Somewhat arbitrary initial provision.  */
    vec_alloc (ivar_offset_refs, 32);

  /* We come here if we don't find a match or at the start.  */
  global_var = (TREE_PUBLIC (field_decl) || TREE_PROTECTED (field_decl));
  if (global_var)
    decl = create_global_decl (TREE_TYPE (size_zero_node), buf);
  else
    decl = create_hidden_decl (TREE_TYPE (size_zero_node), buf);

  /* Make sure it ends up in an ObjC section.  */
  OBJCMETA (decl, objc_meta, meta_base);

  e.decl = decl;
  e.offset = byte_position (field_decl);
  vec_safe_push (ivar_offset_refs, e);
  return decl;
}

/* This routine builds initializer-list needed to initialize 'struct
   ivar_t list[count] of 'struct ivar_list_t' meta data. TYPE is
   'struct ivar_t' and FIELD_DECL is list of ivars for the target
   class.  */

static tree
build_v2_ivar_list_initializer (tree class_name, tree type, tree field_decl)
{
  vec<constructor_elt, va_gc> *inits = NULL;

  do
    {
      vec<constructor_elt, va_gc> *ivar = NULL;
      int val;
      tree id;

      /* Unnamed bitfields are ignored.  */
      if (!DECL_NAME (field_decl))
	{
	  field_decl = DECL_CHAIN (field_decl);
	  continue;
	}

      /* Set offset.  */
      CONSTRUCTOR_APPEND_ELT (ivar, NULL_TREE,
			      build_unary_op (input_location,
					      ADDR_EXPR,
					      ivar_offset_ref (class_name,
							       field_decl), 0));

      /* Set name.  */
      CONSTRUCTOR_APPEND_ELT (ivar, NULL_TREE,
			      add_objc_string (DECL_NAME (field_decl),
						meth_var_names));

      /* Set type.  */
      id = add_objc_string (encode_field_decl (field_decl),
                            meth_var_types);
      CONSTRUCTOR_APPEND_ELT (ivar, NULL_TREE, id);

      /* Set alignment.  */
      val = DECL_ALIGN_UNIT (field_decl);
      val = exact_log2 (val);
      CONSTRUCTOR_APPEND_ELT (ivar, NULL_TREE,
			      build_int_cst (integer_type_node, val));

      /* Set size.  */
      val = TREE_INT_CST_LOW (DECL_SIZE_UNIT (field_decl));
      CONSTRUCTOR_APPEND_ELT (ivar, NULL_TREE,
			      build_int_cst (integer_type_node, val));

      CONSTRUCTOR_APPEND_ELT (inits, NULL_TREE,
			      objc_build_constructor (type, ivar));

      do
	field_decl = DECL_CHAIN (field_decl);
      while (field_decl && TREE_CODE (field_decl) != FIELD_DECL);
    }
  while (field_decl);

  return objc_build_constructor (build_array_type (type, 0), inits);
}

/*
  struct ivar_list_t
  {
    uint32 entsize;
    uint32 count;
    struct iver_t list[count];
  };
*/

static tree
build_v2_ivar_list_t_template (tree list_type, int size)
{
  tree objc_ivar_list_record;
  tree decls, *chain = NULL;

  /* Anonymous.  */
  objc_ivar_list_record = objc_start_struct (NULL_TREE);

  /* uint32 entsize; */
  decls = add_field_decl (integer_type_node, "entsize", &chain);

  /* uint32 count; */
  add_field_decl (integer_type_node, "count", &chain);

  /* struct objc_ivar ivar_list[]; */
  add_field_decl (build_sized_array_type (list_type, size),
			  "list", &chain);

  objc_finish_struct (objc_ivar_list_record, decls);
  return objc_ivar_list_record;
}

/* This routine declares a static variable of type 'struct
   ivar_list_t' and initializes it.  chain is the source of the data,
   name is the name for the var.  attr is the meta-data section tag
   attribute.  templ is the implementation template for the class.  */

static tree
generate_v2_ivars_list (tree chain, const char *name, tree attr, tree templ)
{
  tree decl, initlist, ivar_list_template;
  vec<constructor_elt, va_gc> *inits = NULL;
  int size, ivar_t_size;

  if (!chain || !name || !(size = ivar_list_length (chain)))
    return NULL_TREE;

  generating_instance_variables = 1;
  ivar_list_template = build_v2_ivar_list_t_template (objc_v2_ivar_template,
						      size);

  initlist = build_v2_ivar_list_initializer (CLASS_NAME (templ),
					     objc_v2_ivar_template, chain);
  ivar_t_size = TREE_INT_CST_LOW  (TYPE_SIZE_UNIT (objc_v2_ivar_template));

  decl = start_var_decl (ivar_list_template, name);
  CONSTRUCTOR_APPEND_ELT (inits, NULL_TREE,
			  build_int_cst (integer_type_node, ivar_t_size));
  CONSTRUCTOR_APPEND_ELT (inits, NULL_TREE,
			  build_int_cst (integer_type_node, size));
  CONSTRUCTOR_APPEND_ELT (inits, NULL_TREE, initlist);
  OBJCMETA (decl, objc_meta, attr);
  finish_var_decl (decl, objc_build_constructor (TREE_TYPE (decl), inits));
  generating_instance_variables = 0;
  return decl;
}

/* Routine to build initializer list to initialize objects of type
   struct class_t; */

static tree
build_v2_class_t_initializer (tree type, tree isa, tree superclass,
			      tree ro, tree cache, tree vtable)
{
  vec<constructor_elt, va_gc> *initlist = NULL;

  /* isa */
  CONSTRUCTOR_APPEND_ELT (initlist, NULL_TREE, isa);

  /* superclass */
  CONSTRUCTOR_APPEND_ELT (initlist, NULL_TREE, superclass);

  /* cache */
  if (cache)
    CONSTRUCTOR_APPEND_ELT (initlist, NULL_TREE, cache);
  else
    CONSTRUCTOR_APPEND_ELT (initlist, NULL_TREE, null_pointer_node);

  /* vtable */
  if (vtable)
    CONSTRUCTOR_APPEND_ELT (initlist, NULL_TREE, vtable);
  else
    CONSTRUCTOR_APPEND_ELT (initlist, NULL_TREE, null_pointer_node);

  /* ro */
  CONSTRUCTOR_APPEND_ELT (initlist, NULL_TREE, ro);

  return objc_build_constructor (type, initlist);
}

/* Routine to build object of struct class_ro_t { ... }; */

static tree
build_v2_class_ro_t_initializer (tree type, tree name,
			       unsigned int flags, unsigned int instanceStart,
			       unsigned int instanceSize,
			       tree ivarLayout,
			       tree baseMethods, tree baseProtocols,
			       tree ivars, tree property_list)
{
  tree expr, unsigned_char_star, ltyp;
  location_t loc;
  vec<constructor_elt, va_gc> *initlist = NULL;

  /* TODO: fish out the real location from somewhere.  */
  loc = UNKNOWN_LOCATION;

  /* flags */
  CONSTRUCTOR_APPEND_ELT (initlist, NULL_TREE,
			  build_int_cst (integer_type_node, flags));

  /* instanceStart */
  CONSTRUCTOR_APPEND_ELT (initlist, NULL_TREE,
			  build_int_cst (integer_type_node, instanceStart));

  /* instanceSize */
  CONSTRUCTOR_APPEND_ELT (initlist, NULL_TREE,
			  build_int_cst (integer_type_node, instanceSize));

  /* This ABI is currently only used on m64 NeXT.  We always
     explicitly declare the alignment padding.  */
  /* reserved, pads alignment.  */
  CONSTRUCTOR_APPEND_ELT (initlist, NULL_TREE,
			    build_int_cst (integer_type_node, 0));

  /* ivarLayout */
  unsigned_char_star = build_pointer_type (unsigned_char_type_node);
  if (ivarLayout)
    expr = ivarLayout;
  else
    expr = convert (unsigned_char_star, null_pointer_node);
  CONSTRUCTOR_APPEND_ELT (initlist, NULL_TREE, expr);

  /* name */
  CONSTRUCTOR_APPEND_ELT (initlist, NULL_TREE, default_conversion (name));

  /* baseMethods */
  ltyp = objc_method_list_ptr;
  if (baseMethods)
    expr = convert (ltyp, build_unary_op (loc, ADDR_EXPR, baseMethods, 0));
  else
    expr = convert (ltyp, null_pointer_node);
  CONSTRUCTOR_APPEND_ELT (initlist, NULL_TREE, expr);

  /* baseProtocols */
  ltyp = build_pointer_type (xref_tag (RECORD_TYPE,
			               get_identifier (UTAG_V2_PROTOCOL_LIST)));
  if (baseProtocols)
    expr = convert (ltyp, build_unary_op (loc, ADDR_EXPR, baseProtocols, 0));
  else
    expr = convert (ltyp, null_pointer_node);
  CONSTRUCTOR_APPEND_ELT (initlist, NULL_TREE, expr);

  /* ivars */
  ltyp = objc_v2_ivar_list_ptr;
  if (ivars)
    expr = convert (ltyp, build_unary_op (loc, ADDR_EXPR, ivars, 0));
  else
    expr = convert (ltyp, null_pointer_node);
  CONSTRUCTOR_APPEND_ELT (initlist, NULL_TREE, expr);

  /* TODO: We don't yet have the weak/strong stuff...  */
  /* weakIvarLayout */
  CONSTRUCTOR_APPEND_ELT (initlist, NULL_TREE,
			  convert (unsigned_char_star, null_pointer_node));

  /* property list */
  ltyp = objc_prop_list_ptr;
  if (property_list)
    expr = convert (ltyp, build_unary_op (loc, ADDR_EXPR, property_list, 0));
  else
    expr = convert (ltyp, null_pointer_node);
  CONSTRUCTOR_APPEND_ELT (initlist, NULL_TREE, expr);
  return objc_build_constructor (type, initlist);
}

static GTY (()) vec<ident_data_tuple, va_gc> *ehtype_list;

/* Record a name as needing a catcher.  */
static void
objc_v2_add_to_ehtype_list (tree name)
{
  ident_data_tuple e;
  if (ehtype_list)
    {
      int count = 0;
      ident_data_tuple *ref;

      FOR_EACH_VEC_ELT (*ehtype_list, count, ref)
	if (ref->ident == name)
	  return; /* Already entered.  */
     }
  else
    /* Arbitrary initial count.  */
    vec_alloc (ehtype_list, 8);

  /* Not found, or new list.  */
  e.ident = name;
  e.data = NULL_TREE;
  vec_safe_push (ehtype_list, e);
}

static void
generate_v2_class_structs (struct imp_entry *impent)
{
  tree decl, name_expr, initlist, protocol_decl, metaclass_decl, class_decl;
  tree field, firstIvar, chain;
  tree class_superclass_expr, metaclass_superclass_expr, props;
  /* TODO: figure out how to compute this.  */
  tree ivarLayout = NULL_TREE;
  tree my_super_id = NULL_TREE, root_expr = NULL_TREE;
  tree inst_methods = NULL_TREE, class_methods = NULL_TREE;
  tree inst_ivars = NULL_TREE, class_ivars = NULL_TREE;
  location_t loc;
  char buf[BUFSIZE];
  unsigned int instanceStart, instanceSize;
  unsigned int flags = 0x01; /* RO_META */
  int cls_flags = impent->has_cxx_cdtors ? OBJC2_CLS_HAS_CXX_STRUCTORS
					 : 0 ;

  class_decl = impent->class_decl;
  metaclass_decl = impent->meta_decl;
  loc = DECL_SOURCE_LOCATION (class_decl);

  DECL_EXTERNAL (class_decl) = DECL_EXTERNAL (metaclass_decl) = 0;
  TREE_PUBLIC (class_decl) = TREE_PUBLIC (metaclass_decl) = 1;
#ifdef OBJCPLUS
  gcc_assert (!CP_DECL_CONTEXT (class_decl) || CP_DECL_CONTEXT (class_decl) == global_namespace);
  gcc_assert (!CP_DECL_CONTEXT (metaclass_decl) || CP_DECL_CONTEXT (metaclass_decl) == global_namespace);
#endif

  /* Generation of data for meta class.  */
  my_super_id = CLASS_SUPER_NAME (impent->imp_template);
  if (my_super_id)
    {
      /* Compute reference to root's name.  For a meta class, "isa" is
	 a reference to the root class name. */
      tree my_root_id = my_super_id;
      tree my_root_int, interface;
      do
        {
          my_root_int = lookup_interface (my_root_id);

          if (my_root_int && CLASS_SUPER_NAME (my_root_int))
            my_root_id = CLASS_SUPER_NAME (my_root_int);
          else
            break;
        }
      while (1);

      /* {extern} struct class_t OBJC_METACLASS_$_<my_root_int>
         create extern if not already declared.  */
      snprintf (buf, BUFSIZE, "OBJC_METACLASS_$_%s",
		IDENTIFIER_POINTER (CLASS_NAME (my_root_int)));
      root_expr = create_extern_decl (objc_v2_class_template, buf);
      root_expr = build_fold_addr_expr (root_expr);

      /* Install class `isa' and `super' pointers at runtime.  */
      interface = lookup_interface (my_super_id);
      gcc_assert (interface);
      /* Similarly, for OBJC_CLASS_$_<interface>...  */
      snprintf (buf, BUFSIZE, "OBJC_CLASS_$_%s",
		IDENTIFIER_POINTER (CLASS_NAME (interface)));
      class_superclass_expr = create_extern_decl (objc_v2_class_template, buf);
      class_superclass_expr = build_fold_addr_expr (class_superclass_expr);
      /* ... and for OBJC_METACLASS_$_<interface>.  */
      snprintf (buf, BUFSIZE, "OBJC_METACLASS_$_%s",
		IDENTIFIER_POINTER (CLASS_NAME (interface)));
      metaclass_superclass_expr = create_extern_decl (objc_v2_class_template, buf);
      metaclass_superclass_expr = build_fold_addr_expr (metaclass_superclass_expr);
    }
  else
    {
      /* Root class.  */
      root_expr = build_unary_op (loc, ADDR_EXPR, metaclass_decl, 0);
      metaclass_superclass_expr = build_unary_op (loc, ADDR_EXPR, class_decl, 0);
      class_superclass_expr = build_int_cst (NULL_TREE, 0);
      flags |= 0x02; /* RO_ROOT: it is also a root meta class.  */
    }

  if (CLASS_PROTOCOL_LIST (impent->imp_template))
    {
      generate_protocol_references (CLASS_PROTOCOL_LIST (impent->imp_template));
      protocol_decl = generate_v2_protocol_list (impent->imp_template,
						 impent->imp_context);
    }
  else
    protocol_decl = 0;

  name_expr = add_objc_string (CLASS_NAME (impent->imp_template),
                               class_names);

  if (CLASS_CLS_METHODS (impent->imp_context))
    {
      snprintf (buf, BUFSIZE, "_OBJC_ClassMethods_%s",
		IDENTIFIER_POINTER (CLASS_NAME (impent->imp_context)));
      class_methods =
	generate_v2_dispatch_table (CLASS_CLS_METHODS (impent->imp_context),
				    buf, meta_clac_meth);
    }

  instanceStart = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (objc_v2_class_template));

  /* Currently there are no class ivars and generation of class
     variables for the root of the inheritance has been removed.  It
     causes multiple defines if there are two root classes in the
     link, because each will define its own identically-named offset
     variable.  */

  class_ivars = NULL_TREE;
  /* TODO: Add total size of class variables when implemented. */
  instanceSize = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (objc_v2_class_template));

  /* So now build the META CLASS structs.  */
  /* static struct class_ro_t  _OBJC_METACLASS_Foo = { ... }; */

  decl = start_var_decl (objc_v2_class_ro_template,
			 newabi_append_ro (IDENTIFIER_POINTER
						(DECL_NAME (metaclass_decl))));

  /* TODO: ivarLayout needs t be built.  */
  initlist =
	build_v2_class_ro_t_initializer (TREE_TYPE (decl), name_expr,
					(flags | cls_flags), instanceStart,
					instanceSize, ivarLayout,
					class_methods, protocol_decl,
					class_ivars, NULL_TREE);
  /* The ROs sit in the default const section.  */
  OBJCMETA (decl, objc_meta, meta_base);
  finish_var_decl (decl, initlist);

  /* static struct class_t _OBJC_METACLASS_Foo = { ... }; */
  initlist =
	build_v2_class_t_initializer (TREE_TYPE (metaclass_decl),
				      root_expr,
				      metaclass_superclass_expr,
				      build_fold_addr_expr (decl),
				      build_fold_addr_expr (UOBJC_V2_CACHE_decl),
				      build_fold_addr_expr (UOBJC_V2_VTABLE_decl));
  /* The class section attributes are set when they are created.  */
  finish_var_decl (metaclass_decl, initlist);
  impent->meta_decl = metaclass_decl;

  /* So now build the CLASS structs.  */

  flags = 0x0;		/* ... */
  if (!my_super_id)
    flags |= 0x02;	/* RO_ROOT: this is a root class */

  if (DECL_VISIBILITY (class_decl) == VISIBILITY_HIDDEN)
    flags |= 0x10;	/* RO_HIDDEN, OBJC2_CLS_HIDDEN; */

  if (objc2_objc_exception_attr (impent->imp_template))
    flags |= 0x20;	/* RO_EXCEPTION */

  if (CLASS_NST_METHODS (impent->imp_context))
    {
      snprintf (buf, BUFSIZE, "_OBJC_InstanceMethods_%s",
		IDENTIFIER_POINTER (CLASS_NAME (impent->imp_context)));
      inst_methods =
	generate_v2_dispatch_table (CLASS_NST_METHODS (impent->imp_context),
				    buf, meta_clai_meth);
    }

  /* Sort out the ivars before we try to compute the class sizes.  */
  if ((chain = CLASS_IVARS (impent->imp_template)))
    {
      snprintf (buf, BUFSIZE, "_OBJC_InstanceIvars_%s",
		IDENTIFIER_POINTER (CLASS_NAME (impent->imp_context)));
      inst_ivars = generate_v2_ivars_list (chain, buf, meta_clai_vars,
					   impent->imp_template);
    }

  /* Compute instanceStart.  */
  gcc_assert (CLASS_STATIC_TEMPLATE (impent->imp_template));
  field = TYPE_FIELDS (CLASS_STATIC_TEMPLATE (impent->imp_template));
  if (my_super_id && field && TREE_CHAIN (field))
    field = TREE_CHAIN (field);

  firstIvar = field;

  while (firstIvar && TREE_CODE (firstIvar) != FIELD_DECL)
    firstIvar = TREE_CHAIN (firstIvar);

  gcc_assert (inst_ivars? (firstIvar != NULL_TREE): true);

  /* Compute instanceSize.  */
  while (field && TREE_CHAIN (field)
         && TREE_CODE (TREE_CHAIN (field)) == FIELD_DECL)
    field = TREE_CHAIN (field);

  if (field && TREE_CODE (field) == FIELD_DECL)
    instanceSize = int_byte_position (field) * BITS_PER_UNIT
		   + tree_to_shwi (DECL_SIZE (field));
  else
    instanceSize = 0;
  instanceSize /= BITS_PER_UNIT;

  props = generate_v2_property_table (NULL_TREE, impent->imp_context);

  /* If the class has no ivars, instanceStart should be set to the
     superclass's instanceSize.  */
  instanceStart =
	(inst_ivars != NULL_TREE) ? (unsigned) int_byte_position (firstIvar)
				  : instanceSize;

  /* static struct class_ro_t  _OBJC_CLASS_Foo = { ... }; */
  decl = start_var_decl (objc_v2_class_ro_template,
			 newabi_append_ro (IDENTIFIER_POINTER
						(DECL_NAME (class_decl))));

  initlist =
	build_v2_class_ro_t_initializer (TREE_TYPE (decl), name_expr,
					 (flags | cls_flags), instanceStart,
					 instanceSize, ivarLayout,
					 inst_methods, protocol_decl,
					 inst_ivars, props);
  /* The ROs sit in the default const section.  */
  OBJCMETA (decl, objc_meta, meta_base);
  finish_var_decl (decl, initlist);

  /* static struct class_t _OBJC_CLASS_Foo = { ... }; */
  initlist = build_v2_class_t_initializer (TREE_TYPE (class_decl),
					build_fold_addr_expr (metaclass_decl),
					class_superclass_expr,
					build_fold_addr_expr (decl),
					build_fold_addr_expr (UOBJC_V2_CACHE_decl),
					build_fold_addr_expr (UOBJC_V2_VTABLE_decl));

  /* The class section attributes are set when they are created.  */
  finish_var_decl (class_decl, initlist);
  impent->class_decl = class_decl;

  objc_v2_add_to_class_list (class_decl);
  if (has_load_impl (CLASS_CLS_METHODS (impent->imp_context)))
    objc_v2_add_to_nonlazy_class_list (class_decl);

  if (flags & 0x20) /* RO_EXCEPTION */
    objc_v2_add_to_ehtype_list (CLASS_NAME (impent->imp_template));
}

/* This routine outputs the (ivar_reference_offset, offset)
   tuples.  */

static void
build_v2_ivar_offset_ref_table (void)
{
  int count;
  ivarref_entry *ref;

  if (!vec_safe_length (ivar_offset_refs))
    return;

  FOR_EACH_VEC_ELT (*ivar_offset_refs, count, ref)
    finish_var_decl (ref->decl, ref->offset);
}

static void
objc_generate_v2_next_metadata (void)
{
  struct imp_entry *impent;

  /* FIXME: Make sure that we generate no metadata if there is nothing
     to put into it.  */

  gcc_assert (!objc_static_instances); /* Not for NeXT */

  build_metadata_templates ();

  for (impent = imp_list; impent; impent = impent->next)
    {
      /* If -gen-decls is present, Dump the @interface of each class.
	 TODO: Dump the classes in the order they were found, rather
	 than in reverse order as we are doing now.  */
      if (flag_gen_declaration)
	dump_interface (gen_declaration_file, impent->imp_context);

      /* all of the following reference the string pool...  */
      if (TREE_CODE (impent->imp_context) == CLASS_IMPLEMENTATION_TYPE)
	generate_v2_class_structs (impent);
      else
	generate_v2_category (impent);
    }

  build_next_selector_translation_table ();
  build_v2_message_ref_translation_table ();

  /* This will add "Protocol" to the class refs.  */
  generate_v2_protocols ();

  build_v2_classrefs_table ();
  build_v2_super_classrefs_table (/*metaclass= */false);
  build_v2_super_classrefs_table (/*metaclass= */true);

  build_v2_ivar_offset_ref_table ();

  build_v2_protocol_list_translation_table ();
  build_v2_protocol_list_address_table ();

  build_v2_address_table (class_list, "_OBJC_ClassList$",
			  meta_label_classlist);
  build_v2_address_table (category_list, "_OBJC_CategoryList$",
			  meta_label_categorylist);
  build_v2_address_table (nonlazy_class_list, "_OBJC_NonLazyClassList$",
			  meta_label_nonlazy_classlist);
  build_v2_address_table (nonlazy_category_list, "_OBJC_NonLazyCategoryList$",
			  meta_label_nonlazy_categorylist);

  /* Generate catch objects for eh, if any are needed.  */
  build_v2_eh_catch_objects ();

  /* Emit the string table last.  */
  generate_strings ();
}

/* NOTE --- Output NeXT V2 Exceptions --- */

static GTY(()) tree objc_v2_ehtype_template;
static GTY(()) tree next_v2_ehvtable_decl;
static GTY(()) tree next_v2_EHTYPE_id_decl;

static void
build_v2_ehtype_template (void)
{
  tree decls, *chain = NULL;
  objc_v2_ehtype_template = objc_start_struct (get_identifier (UTAG_V2_EH_TYPE));

  /* void *_objc_ehtype_vtable; */
  decls = add_field_decl (ptr_type_node, "_objc_ehtype_vtable_ptr", &chain);

  /* const char *className; */
  add_field_decl (string_type_node, "className", &chain);

  /* struct class_t *const cls; */
  add_field_decl (build_pointer_type (objc_v2_class_template), "cls", &chain);

  objc_finish_struct (objc_v2_ehtype_template, decls);
}

/* Template for the Objective-C family typeinfo type for ABI=2.  This
   starts off the same as the gxx/cxx eh typeinfo.

   struct _objc_ehtype_t
   {
     void *_objc_ehtype_vtable_ptr;	- as per c++
     const char *className;		- as per c++
     struct class_t *const cls;
   }
*/

/* This routine builds initializer list for object of type struct _objc_ehtype_t.
*/

static tree
objc2_build_ehtype_initializer (tree name, tree cls)
{
  vec<constructor_elt, va_gc> *initlist = NULL;
  tree addr, offs;

  /* This is done the same way as c++, missing the two first entries
     in the parent vtable.  NOTE: there is a fix-me in the Apple/NeXT
     runtime source about this so, perhaps, this will change at some
     point.  */
  /* _objc_ehtype_vtable + 2*sizeof(void*)  */
  if (!next_v2_ehvtable_decl)
    {
      next_v2_ehvtable_decl =
			start_var_decl (ptr_type_node, TAG_NEXT_EHVTABLE_NAME);
      TREE_STATIC (next_v2_ehvtable_decl) = 0;
      DECL_EXTERNAL (next_v2_ehvtable_decl) = 1;
      TREE_PUBLIC (next_v2_ehvtable_decl) = 1;
    }
  addr = build_fold_addr_expr_with_type (next_v2_ehvtable_decl, ptr_type_node);
  offs = size_int (2 * int_cst_value (TYPE_SIZE_UNIT (ptr_type_node)));
  addr = fold_build_pointer_plus (addr, offs);

  CONSTRUCTOR_APPEND_ELT (initlist, NULL_TREE, addr);

  /* className */
  CONSTRUCTOR_APPEND_ELT (initlist, NULL_TREE, name);

  /* cls */
  CONSTRUCTOR_APPEND_ELT (initlist, NULL_TREE, cls);

  return objc_build_constructor (objc_v2_ehtype_template, initlist);
}

static tree
build_ehtype (tree name, const char *eh_name, bool weak)
{
  tree name_expr, class_name_expr, ehtype_decl, inits;

  name_expr = add_objc_string (name, class_names);
  /* Extern ref. for the class. ???  Maybe we can look this up
     somewhere.  */
  class_name_expr =
	create_extern_decl (objc_v2_class_template,
			    objc_build_internal_classname (name, false));
  class_name_expr = build_fold_addr_expr (class_name_expr);
  ehtype_decl = create_global_decl (objc_v2_ehtype_template, eh_name);
  if (weak)
    DECL_WEAK (ehtype_decl) = 1;
  inits = objc2_build_ehtype_initializer (name_expr, class_name_expr);
  OBJCMETA (ehtype_decl, objc_meta, meta_ehtype);
  finish_var_decl (ehtype_decl, inits);
  return ehtype_decl;
}

/* This routine returns TRUE if CLS or any of its super classes has
   __attribute__ ((objc_exception)).  */

static bool
objc2_objc_exception_attr (tree cls)
{
  while (cls)
    {
      if (CLASS_HAS_EXCEPTION_ATTR (cls))
	return true;
      cls = lookup_interface (CLASS_SUPER_NAME (cls));
    }

  return false;
}

static bool
is_implemented (tree name)
{
  struct imp_entry *t;
  for (t = imp_list; t; t = t->next)
    if (TREE_CODE (t->imp_context) == CLASS_IMPLEMENTATION_TYPE
	&& CLASS_NAME (t->imp_template) == name)
      return true;

  return false;
}

/* We will build catch objects:
     for any type  implemented here.
     for any type used in a catch that has no exception attribute.  */
static void build_v2_eh_catch_objects (void)
{
  int count=0;
  ident_data_tuple *ref;

  if (!vec_safe_length (ehtype_list))
    return;

  FOR_EACH_VEC_ELT (*ehtype_list, count, ref)
    {
      char buf[BUFSIZE];
      bool impl = is_implemented (ref->ident);
      bool excpt = objc2_objc_exception_attr (lookup_interface (ref->ident));
      snprintf (buf, BUFSIZE, "OBJC_EHTYPE_$_%s", IDENTIFIER_POINTER (ref->ident));
      if (!impl && excpt)
	/* The User says this class has a catcher already.  */
	ref->data = create_extern_decl (objc_v2_ehtype_template, buf);
      else
	/* Create a catcher, weak if it wasn't marked.  */
	ref->data = build_ehtype (ref->ident, buf, !excpt);
    }
}

static tree
lookup_ehtype_ref (tree id)
{
  int count=0;
  ident_data_tuple *ref;

  if (!vec_safe_length (ehtype_list))
    return NULL_TREE;

  FOR_EACH_VEC_ELT (*ehtype_list, count, ref)
    if (ref->ident == id)
      return ref->data;
  return NULL_TREE;
}

/* This hook, called via lang_eh_runtime_type, generates a runtime
   object which is either the address of the 'OBJC_EHTYPE_$_class'
   object or address of external OBJC_EHTYPE_id object.  */
static tree
next_runtime_02_eh_type (tree type)
{
  tree t;

  if (type == error_mark_node
      /*|| errorcount || sorrycount*/)
    goto err_mark_in;

  if (POINTER_TYPE_P (type) && objc_is_object_id (TREE_TYPE (type)))
    {
      if (!next_v2_EHTYPE_id_decl)
	{
	  /* This is provided by the Apple/NeXT libobjc.dylib so we
	     need only to reference it.  */
	  next_v2_EHTYPE_id_decl =
		start_var_decl (objc_v2_ehtype_template, "OBJC_EHTYPE_id");
	  DECL_EXTERNAL (next_v2_EHTYPE_id_decl) = 1;
	  TREE_PUBLIC (next_v2_EHTYPE_id_decl) = 1;
	  TREE_STATIC (next_v2_EHTYPE_id_decl) = 0;
	}
      return build_fold_addr_expr (next_v2_EHTYPE_id_decl);
    }

  if (!POINTER_TYPE_P (type) || !TYPED_OBJECT (TREE_TYPE (type)))
    {
#ifdef OBJCPLUS
      /* This routine is also called for c++'s catch clause; in which
	 case, we use c++'s typeinfo decl.  */
      return build_eh_type_type (type);
#else
      error ("non-objective-c type '%T' cannot be caught", type);
      goto err_mark_in;
#endif
    }
  else
    t = OBJC_TYPE_NAME (TREE_TYPE (type));

  /* We have to build a reference to the OBJC_EHTYPE_<Class>.  */
  t = lookup_ehtype_ref (t);
  if (!t)
    goto err_mark_in;

  return build_fold_addr_expr (t);

err_mark_in:
  return error_mark_node;
}

static GTY(()) tree objc_eh_personality_decl;

static tree
objc_eh_personality (void)
{
  if (!objc_eh_personality_decl)
    objc_eh_personality_decl = build_personality_function  ("objc");
  return objc_eh_personality_decl;
}

/* NOTE --- interfaces --- */

static tree
build_throw_stmt (location_t loc, tree throw_expr, bool rethrown)
{
  tree t;
  if (rethrown)
    /* We have a separate re-throw entry.  */
    t = build_function_call_vec (loc, objc_rethrow_exception_decl, NULL, NULL);
  else
    {
      /* Throw like the others...  */
      vec<tree, va_gc> *parms;
      vec_alloc (parms, 1);
      parms->quick_push (throw_expr);
      t = build_function_call_vec (loc, objc_exception_throw_decl, parms, 0);
      vec_free (parms);
    }
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

static tree begin_catch (struct objc_try_context **cur_try_context, tree type,
			 tree decl, tree compound, bool ellipsis ATTRIBUTE_UNUSED)
{
  tree t;

  /* Record the data for the catch in the try context so that we can
     finalize it later.  Ellipsis is signalled by a NULL entry.  */
  if (ellipsis)
    t = build_stmt (input_location, CATCH_EXPR, NULL_TREE, compound);
  else
    t = build_stmt (input_location, CATCH_EXPR, type, compound);
  (*cur_try_context)->current_catch = t;

  /* Initialize the decl from the EXC_PTR_EXPR we get from the runtime.  */
  t = objc_build_exc_ptr (cur_try_context);
  t = convert (TREE_TYPE (decl), t);
  /* FIXME: location.  */
  if (type && type != error_mark_node)
    {
      t = build1(NOP_EXPR, ptr_type_node, t);
      t = build_function_call (input_location, objc2_begin_catch_decl,
			      tree_cons (NULL_TREE, t, NULL_TREE));

      /* We might want to build a catch object for this (if it's not
	 id).  */
      if (POINTER_TYPE_P (type)
	  && !objc_is_object_id (TREE_TYPE (type))
	  && TYPED_OBJECT (TREE_TYPE (type)))
	objc_v2_add_to_ehtype_list (OBJC_TYPE_NAME (TREE_TYPE (type)));
    }
  return build2 (MODIFY_EXPR, void_type_node, decl, t);
}

/* try { catch-body } finally { objc_end_catch (); } */
static void
finish_catch (struct objc_try_context **cur_try_context, tree curr_catch)
{
  struct objc_try_context *ct;
  tree try_exp, func, *l, t ;
  location_t loc = (*cur_try_context)->try_locus;

  if (!curr_catch || curr_catch == error_mark_node)
    return;

  t = CATCH_BODY (curr_catch);
  if (TREE_CODE (t) == BIND_EXPR)
    {
      /* Usual case of @catch (objc-expr).  */
      objc_begin_try_stmt (loc, BIND_EXPR_BODY (t));
      BIND_EXPR_BODY (t) = NULL_TREE;
      l = &BIND_EXPR_BODY (t);
    }
  else
    {
      /* NULL entry, meaning @catch (...).  */
      objc_begin_try_stmt (loc, t);
      CATCH_BODY (curr_catch) = NULL_TREE;
      l = &CATCH_BODY (curr_catch);
    }

  /* Pick up the new context we made in begin_try above...  */
  ct = *cur_try_context;
  func = build_function_call_vec (loc, objc2_end_catch_decl, NULL, NULL);
  append_to_statement_list (func, &ct->finally_body);
  try_exp = build_stmt (loc, TRY_FINALLY_EXPR, ct->try_body, ct->finally_body);
  *cur_try_context = ct->outer;
  free (ct);
  append_to_statement_list (try_exp, l);
  append_to_statement_list (curr_catch, &((*cur_try_context)->catch_list));
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

#include "gt-objc-objc-next-runtime-abi-02.h"
