/* Declarations for objc-act.c.
   Copyright (C) 1990-2020 Free Software Foundation, Inc.

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


#ifndef GCC_OBJC_ACT_H
#define GCC_OBJC_ACT_H

/*** Language hooks ***/

bool objc_init (void);
const char *objc_printable_name (tree, int);
int objc_gimplify_expr (tree *, gimple_seq *, gimple_seq *);
void objc_common_init_ts (void);

/* NB: The remaining public functions are prototyped in c-common.h, for the
   benefit of stub-objc.c and objc-act.c.  */

/* Objective-C structures */

#define CLASS_LANG_SLOT_ELTS		7
#define PROTOCOL_LANG_SLOT_ELTS		7
#define OBJC_INFO_SLOT_ELTS		2

/* KEYWORD_DECL */
#define KEYWORD_KEY_NAME(DECL) (KEYWORD_DECL_CHECK (DECL)->decl_minimal.name)
#define KEYWORD_ARG_NAME(DECL) (KEYWORD_DECL_CHECK (DECL)->decl_common.size)

#define INSTANCE_METHOD_OR_CLASS_METHOD_DECL_CHECK(NODE) \
  TREE_CHECK2(NODE,INSTANCE_METHOD_DECL,CLASS_METHOD_DECL)

/* INSTANCE_METHOD_DECL, CLASS_METHOD_DECL */
#define METHOD_SEL_NAME(DECL) \
  (INSTANCE_METHOD_OR_CLASS_METHOD_DECL_CHECK (DECL)->decl_minimal.name)
#define METHOD_SEL_ARGS(DECL) \
  (INSTANCE_METHOD_OR_CLASS_METHOD_DECL_CHECK (DECL)->decl_common.size)
#define METHOD_ADD_ARGS(DECL) \
  (INSTANCE_METHOD_OR_CLASS_METHOD_DECL_CHECK (DECL)->decl_non_common.result)
#define METHOD_ADD_ARGS_ELLIPSIS_P(DECL) \
  (INSTANCE_METHOD_OR_CLASS_METHOD_DECL_CHECK (DECL)->decl_common.lang_flag_0)
#define METHOD_DEFINITION(DECL) \
  (INSTANCE_METHOD_OR_CLASS_METHOD_DECL_CHECK (DECL)->decl_common.initial)
#define METHOD_ENCODING(DECL) \
  (INSTANCE_METHOD_OR_CLASS_METHOD_DECL_CHECK (DECL)->decl_minimal.context)
#define METHOD_TYPE_ATTRIBUTES(DECL) \
  (INSTANCE_METHOD_OR_CLASS_METHOD_DECL_CHECK (DECL)->decl_common.abstract_origin)
#define METHOD_PROPERTY_CONTEXT(DECL) \
  (INSTANCE_METHOD_OR_CLASS_METHOD_DECL_CHECK (DECL)->decl_common.size_unit)


/* PROPERTY_DECL.  A PROPERTY_DECL repesents a @property declaration
   (when attached to the list of properties of an interface) or a
   @synthesize or @dynamic declaration (when attached to the list of
   properties of an implementation).  */

/* TREE_TYPE is the type (int, float, etc) of the property.  */

/* DECL_ARTIFICIAL is set to 1 if the PROPERTY_DECL is an artificial
   property declaration created when the dot-syntax object.component
   is used with no actual @property matching the component, but a
   valid getter/setter.  */

/* PROPERTY_NAME is the name of the property.  */
#define PROPERTY_NAME(DECL) \
   DECL_NAME(PROPERTY_DECL_CHECK (DECL))

/* PROPERTY_GETTER_NAME is the identifier of the getter method.  */
#define PROPERTY_GETTER_NAME(DECL)\
   (PROPERTY_DECL_CHECK (DECL)->decl_common.size)

/* PROPERTY_SETTER_NAME is the identifier of the setter method.  */
#define PROPERTY_SETTER_NAME(DECL) \
   (PROPERTY_DECL_CHECK (DECL)->decl_non_common.result)

/* PROPERTY_READONLY can be 0 or 1.  */
#define PROPERTY_READONLY(DECL) \
   DECL_LANG_FLAG_0 (PROPERTY_DECL_CHECK (DECL))

/* PROPERTY_NONATOMIC can be 0 or 1.  */
#define PROPERTY_NONATOMIC(DECL) \
   DECL_LANG_FLAG_1 (PROPERTY_DECL_CHECK (DECL))

enum objc_property_assign_semantics {
  OBJC_PROPERTY_ASSIGN = 1,
  OBJC_PROPERTY_RETAIN = 2,
  OBJC_PROPERTY_COPY = 3
};

/* PROPERTY_ASSIGN_SEMANTICS can be OBJC_PROPERTY_ASSIGN,
   OBJC_PROPERTY_RETAIN or OBJC_PROPERTY_COPY.  We need an integer to
   store it, so we hijack the alignment, that properties don't
   have.  */
#define PROPERTY_ASSIGN_SEMANTICS(DECL) \
   (PROPERTY_DECL_CHECK (DECL)->decl_common.align)

/* PROPERTY_IVAR_NAME is the identifier of the instance variable.
   This is set only if the PROPERTY_DECL represents a @synthesize;
   otherwise, it is set to TREE_NULL.  */
#define PROPERTY_IVAR_NAME(DECL) \
  (PROPERTY_DECL_CHECK (DECL)->decl_common.initial)

/* PROPERTY_DYNAMIC can be 0 or 1.  This is 1 if the PROPERTY_DECL
   represents a @dynamic; otherwise, it is set to 0.  */
#define PROPERTY_DYNAMIC(DECL) \
  DECL_LANG_FLAG_2 (PROPERTY_DECL_CHECK (DECL))

/* PROPERTY_HAS_NO_GETTER can be 0 or 1.  Normally it is 0, but if
   this is an artificial PROPERTY_DECL that we generate even without a
   getter, it is set to 1.  */
#define PROPERTY_HAS_NO_GETTER(DECL) \
  DECL_LANG_FLAG_3 (PROPERTY_DECL_CHECK (DECL))

/* PROPERTY_HAS_NO_SETTER can be 0 or 1.  Normally it is 0, but if
   this is an artificial PROPERTY_DECL that we generate even without a
   setter, it is set to 1.  */
#define PROPERTY_HAS_NO_SETTER(DECL) \
  DECL_LANG_FLAG_4 (PROPERTY_DECL_CHECK (DECL))

/* PROPERTY_OPTIONAL can be 0 or 1.  Normally it is 0, but if this is
   a property declared as @optional in a @protocol, then it is set to
   1.  */
#define PROPERTY_OPTIONAL(DECL) \
  DECL_LANG_FLAG_5 (PROPERTY_DECL_CHECK (DECL))

/* PROPERTY_REF.  A PROPERTY_REF represents an 'object.property'
   expression.  It is normally used for property access, but when
   the Objective-C 2.0 "dot-syntax" (object.component) is used
   with no matching property, a PROPERTY_REF is still created to
   represent it, with an artificial PROPERTY_DECL.  */

/* PROPERTY_REF_OBJECT is the object whose property we are
   accessing.  */
#define PROPERTY_REF_OBJECT(NODE) TREE_OPERAND (PROPERTY_REF_CHECK (NODE), 0)

/* PROPERTY_REF_PROPERTY_DECL is the PROPERTY_DECL for the property
   used in the expression.  From it, you can get the property type,
   and the getter/setter names.  This PROPERTY_DECL could be artificial
   if we are processing an 'object.component' syntax with no matching
   declared property.  */
#define PROPERTY_REF_PROPERTY_DECL(NODE) TREE_OPERAND (PROPERTY_REF_CHECK (NODE), 1)

/* PROPERTY_REF_GETTER_CALL is the getter call expression, ready to
   use at gimplify time if needed.  Generating the getter call
   requires modifying the selector table, and, in the case of
   self/super, requires the context to be generated correctly.  The
   gimplify stage is too late to do these things, so we generate the
   getter call earlier instead, and keep it here in case we need to
   use it.  */
#define PROPERTY_REF_GETTER_CALL(NODE) TREE_OPERAND (PROPERTY_REF_CHECK (NODE), 2)

/* PROPERTY_REF_DEPRECATED_GETTER is normally set to NULL_TREE.  If
   the property getter is deprecated, it is set to the method
   prototype for it, which is used to generate the deprecation warning
   when the getter is used.  */
#define PROPERTY_REF_DEPRECATED_GETTER(NODE) TREE_OPERAND (PROPERTY_REF_CHECK (NODE), 3)

/* CLASS_INTERFACE_TYPE, CLASS_IMPLEMENTATION_TYPE,
   CATEGORY_INTERFACE_TYPE, CATEGORY_IMPLEMENTATION_TYPE,
   PROTOCOL_INTERFACE_TYPE */
/* CLASS_NAME is the name of the class.  */
#define CLASS_NAME(CLASS) (TYPE_NAME (CLASS))
/* CLASS_SUPER_NAME is the name of the superclass, or, in the case of
   categories, it is the name of the category itself.  */
#define CLASS_SUPER_NAME(CLASS) (TYPE_CONTEXT (CLASS))
#define CLASS_IVARS(CLASS) TREE_VEC_ELT (TYPE_LANG_SLOT_1 (CLASS), 0)
#define CLASS_RAW_IVARS(CLASS) TREE_VEC_ELT (TYPE_LANG_SLOT_1 (CLASS), 1)
#define CLASS_NST_METHODS(CLASS) (TYPE_MIN_VALUE_RAW (CLASS))
#define CLASS_CLS_METHODS(CLASS) (TYPE_MAX_VALUE_RAW (CLASS))
#define CLASS_STATIC_TEMPLATE(CLASS) TREE_VEC_ELT (TYPE_LANG_SLOT_1 (CLASS), 2)
#define CLASS_CATEGORY_LIST(CLASS) TREE_VEC_ELT (TYPE_LANG_SLOT_1 (CLASS), 3)
#define CLASS_PROTOCOL_LIST(CLASS) TREE_VEC_ELT (TYPE_LANG_SLOT_1 (CLASS), 4)
#define TOTAL_CLASS_RAW_IVARS(CLASS) TREE_VEC_ELT (TYPE_LANG_SLOT_1 (CLASS), 5)
#define CLASS_HAS_EXCEPTION_ATTR(CLASS) (TYPE_LANG_FLAG_0 (CLASS))

#define PROTOCOL_NAME(CLASS) (TYPE_NAME (CLASS))
#define PROTOCOL_LIST(CLASS) TREE_VEC_ELT (TYPE_LANG_SLOT_1 (CLASS), 0)
#define PROTOCOL_NST_METHODS(CLASS) (TYPE_MIN_VALUE_RAW (CLASS))
#define PROTOCOL_CLS_METHODS(CLASS) (TYPE_MAX_VALUE_RAW (CLASS))
#define PROTOCOL_FORWARD_DECL(CLASS) TREE_VEC_ELT (TYPE_LANG_SLOT_1 (CLASS), 1)
#define PROTOCOL_DEFINED(CLASS) TREE_USED (CLASS)
#define PROTOCOL_OPTIONAL_CLS_METHODS(CLASS) TREE_VEC_ELT (TYPE_LANG_SLOT_1 (CLASS), 2)
#define PROTOCOL_OPTIONAL_NST_METHODS(CLASS) TREE_VEC_ELT (TYPE_LANG_SLOT_1 (CLASS), 3)

/* For CATEGORY_INTERFACE_TYPE, CLASS_INTERFACE_TYPE or PROTOCOL_INTERFACE_TYPE */
#define CLASS_PROPERTY_DECL(CLASS) TREE_VEC_ELT (TYPE_LANG_SLOT_1 (CLASS), 6)
/* For CLASS_IMPLEMENTATION_TYPE or CATEGORY_IMPLEMENTATION_TYPE. */
#define IMPL_PROPERTY_DECL(CLASS) TREE_VEC_ELT (TYPE_LANG_SLOT_1 (CLASS), 6)

/* TREE_DEPRECATED is used for a CLASS_INTERFACE_TYPE or PROTOCOL_INTERFACE_TYPE.  */

/* TYPE_ATTRIBUTES is used for a CLASS_INTERFACE_TYPE or PROTOCOL_INTERFACE_TYPE.  */

/* ObjC-specific information pertaining to RECORD_TYPEs are stored in
   the LANG_SPECIFIC structures, which may itself need allocating first.  */

/* The following three macros must be overridden (in objcp/objcp-decl.h)
   for Objective-C++.  */
#define TYPE_OBJC_INFO(TYPE) TYPE_LANG_SPECIFIC (TYPE)->objc_info
#define SIZEOF_OBJC_TYPE_LANG_SPECIFIC sizeof (struct lang_type)
#define ALLOC_OBJC_TYPE_LANG_SPECIFIC(NODE)				\
  do {									\
      TYPE_LANG_SPECIFIC (NODE) = ggc_cleared_alloc<struct lang_type> (); \
  } while (0)

#define TYPE_HAS_OBJC_INFO(TYPE)				\
	(TYPE_LANG_SPECIFIC (TYPE) && TYPE_OBJC_INFO (TYPE))
#define TYPE_OBJC_INTERFACE(TYPE) TREE_VEC_ELT (TYPE_OBJC_INFO (TYPE), 0)
#define TYPE_OBJC_PROTOCOL_LIST(TYPE) TREE_VEC_ELT (TYPE_OBJC_INFO (TYPE), 1)


#define INIT_TYPE_OBJC_INFO(TYPE)				\
	do							\
	  {							\
	    if (!TYPE_LANG_SPECIFIC (TYPE))			\
	      ALLOC_OBJC_TYPE_LANG_SPECIFIC(TYPE);		\
	    if (!TYPE_OBJC_INFO (TYPE))				\
	      TYPE_OBJC_INFO (TYPE)				\
		= make_tree_vec (OBJC_INFO_SLOT_ELTS);		\
	  }							\
	while (0)

#define DUP_TYPE_OBJC_INFO(DST, SRC)				\
	do							\
	  {							\
	    ALLOC_OBJC_TYPE_LANG_SPECIFIC(DST);			\
	    if (TYPE_LANG_SPECIFIC (SRC))			\
	      memcpy (TYPE_LANG_SPECIFIC (DST),			\
		      TYPE_LANG_SPECIFIC (SRC),			\
		      SIZEOF_OBJC_TYPE_LANG_SPECIFIC);		\
	    TYPE_OBJC_INFO (DST)				\
	      = make_tree_vec (OBJC_INFO_SLOT_ELTS);		\
	  }							\
	while (0)

#define TYPED_OBJECT(TYPE)					\
	(TREE_CODE (TYPE) == RECORD_TYPE			\
	 && TYPE_HAS_OBJC_INFO (TYPE)				\
	 && TYPE_OBJC_INTERFACE (TYPE))
#define OBJC_TYPE_NAME(TYPE) TYPE_NAME(TYPE)
#define OBJC_SET_TYPE_NAME(TYPE, NAME) (TYPE_NAME (TYPE) = NAME)

/* Hash tables to manage the global pool of method prototypes.  */

typedef struct hashed_entry	*hash;
typedef struct hashed_attribute	*attr;

struct GTY(()) hashed_attribute {
  attr next;
  tree value;
};

struct GTY(()) hashed_entry {
  attr list;
  hash next;
  tree key;
};

#define SIZEHASHTABLE		257

/* An array of all the local variables in the current function that
   need to be marked as volatile.  */
extern GTY(()) vec<tree, va_gc> *local_variables_to_volatilize;

/* Objective-C/Objective-C++ @implementation list.  */

struct GTY(()) imp_entry {
  struct imp_entry *next;
  tree imp_context;
  tree imp_template;
  tree class_decl;		/* _OBJC[_v2]_CLASS/CATEGORY_<my_name>; */
  tree meta_decl;		/* _OBJC[_v2]_METACLASS_<my_name>; */
  BOOL_BITFIELD has_cxx_cdtors : 1;
};

extern GTY(()) struct imp_entry *imp_list;
extern GTY(()) int imp_count;	/* `@implementation' */
extern GTY(()) int cat_count;	/* `@category' */

extern GTY(()) enum objc_ivar_visibility_kind objc_ivar_visibility;

/* Objective-C/Objective-C++ global tree enumeration.  */

enum objc_tree_index
{
    OCTI_STATIC_NST,
    OCTI_STATIC_NST_DECL,
    OCTI_SELF_ID,
    OCTI_UCMD_ID,

    OCTI_SELF_DECL,
    OCTI_UMSG_DECL,
    OCTI_UMSG_FAST_DECL,
    OCTI_UMSG_SUPER_DECL,
    OCTI_UMSG_STRET_DECL,
    OCTI_UMSG_SUPER_STRET_DECL,
    OCTI_GET_CLASS_DECL,
    OCTI_GET_MCLASS_DECL,
    OCTI_SUPER_TYPE,
    OCTI_SEL_TYPE,
    OCTI_ID_TYPE,
    OCTI_INSTANCE_TYPE,
    OCTI_CLS_TYPE,
    OCTI_NST_TYPE,
    OCTI_PROTO_TYPE,

    OCTI_INTF_CHAIN,
    OCTI_PROTO_CHAIN,
    OCTI_IMPL_CHAIN,
    OCTI_CLS_REF_CHAIN,
    OCTI_SEL_REF_CHAIN,
    OCTI_IVAR_CHAIN,
    OCTI_CLS_NAMES_CHAIN,
    OCTI_METH_VAR_NAMES_CHAIN,
    OCTI_METH_VAR_TYPES_CHAIN,

    OCTI_SYMBOLS_DECL,
    OCTI_NST_VAR_DECL,
    OCTI_CLS_VAR_DECL,
    OCTI_NST_METH_DECL,
    OCTI_CLS_METH_DECL,
    OCTI_CLS_DECL,
    OCTI_MCLS_DECL,
    OCTI_SEL_TABLE_DECL,
    OCTI_MODULES_DECL,
    OCTI_GNU_INIT_DECL,

    OCTI_INTF_CTX,
    OCTI_IMPL_CTX,
    OCTI_METH_CTX,
    OCTI_IVAR_CTX,

    OCTI_IMPL_TEMPL,
    OCTI_CLS_TEMPL,
    OCTI_CAT_TEMPL,
    OCTI_UPRIV_REC,
    OCTI_PROTO_TEMPL,
    OCTI_SEL_TEMPL,
    OCTI_UCLS_SUPER_REF,
    OCTI_UUCLS_SUPER_REF,
    OCTI_METH_TEMPL,
    OCTI_IVAR_TEMPL,
    OCTI_METH_LIST_TEMPL,
    OCTI_METH_PROTO_LIST_TEMPL,
    OCTI_IVAR_LIST_TEMPL,
    OCTI_SYMTAB_TEMPL,
    OCTI_MODULE_TEMPL,
    OCTI_SUPER_TEMPL,
    OCTI_OBJ_REF,
    OCTI_CLS_REF,
    OCTI_METH_PROTO_TEMPL,
    OCTI_FUNCTION1_TEMPL,
    OCTI_FUNCTION2_TEMPL,

    OCTI_OBJ_ID,
    OCTI_CLS_ID,
    OCTI_ID_NAME,
    OCTI_INSTANCETYPE_NAME,
    OCTI_CLASS_NAME,
    OCTI_CNST_STR_ID,
    OCTI_CNST_STR_TYPE,
    OCTI_CNST_STR_GLOB_ID,
    OCTI_STRING_CLASS_DECL,
    OCTI_INTERNAL_CNST_STR_TYPE,
    OCTI_SUPER_DECL,
    OCTI_SUPER_SUPERFIELD_ID,
    OCTI_UMSG_NONNIL_DECL,
    OCTI_UMSG_NONNIL_STRET_DECL,
    OCTI_STORAGE_CLS,
    OCTI_EXCEPTION_EXTRACT_DECL,
    OCTI_EXCEPTION_TRY_ENTER_DECL,
    OCTI_EXCEPTION_TRY_EXIT_DECL,
    OCTI_EXCEPTION_MATCH_DECL,
    OCTI_EXCEPTION_THROW_DECL,
    OCTI_SYNC_ENTER_DECL,
    OCTI_SYNC_EXIT_DECL,
    OCTI_SETJMP_DECL,
    OCTI_EXCDATA_TEMPL,
    OCTI_STACK_EXCEPTION_DATA_DECL,
    OCTI_LOCAL_EXCEPTION_DECL,
    OCTI_RETHROW_EXCEPTION_DECL,
    OCTI_EVAL_ONCE_DECL,
    OCTI_CATCH_TYPE,
    OCTI_EXECCLASS_DECL,

    OCTI_ASSIGN_IVAR_DECL,
    OCTI_ASSIGN_IVAR_FAST_DECL,
    OCTI_ASSIGN_GLOBAL_DECL,
    OCTI_ASSIGN_STRONGCAST_DECL,

    OCTI_FAST_ENUM_STATE_TEMP,
    OCTI_ENUM_MUTATION_DECL,

    OCTI_GET_PROPERTY_DECL,
    OCTI_SET_PROPERTY_DECL,
    OCTI_COPY_STRUCT_DECL,
    OCTI_GET_PROPERTY_STRUCT_DECL,
    OCTI_SET_PROPERTY_STRUCT_DECL,

    /* TODO: Add comment.  */
    /* "V1" stuff.  */
    OCTI_V1_PROP_LIST_TEMPL,
    OCTI_V1_PROP_NAME_ATTR_CHAIN,

    OCTI_MAX
};

extern GTY(()) tree objc_global_trees[OCTI_MAX];

/* List of classes with list of their static instances.  */
#define objc_static_instances	objc_global_trees[OCTI_STATIC_NST]

/* The declaration of the array administrating the static instances.  */
#define static_instances_decl	objc_global_trees[OCTI_STATIC_NST_DECL]

/* Some commonly used instances of "identifier_node".  */

#define self_id			objc_global_trees[OCTI_SELF_ID]
#define ucmd_id			objc_global_trees[OCTI_UCMD_ID]

#define self_decl		objc_global_trees[OCTI_SELF_DECL]
#define umsg_decl		objc_global_trees[OCTI_UMSG_DECL]
#define umsg_fast_decl		objc_global_trees[OCTI_UMSG_FAST_DECL]
#define umsg_super_decl		objc_global_trees[OCTI_UMSG_SUPER_DECL]
#define umsg_stret_decl		objc_global_trees[OCTI_UMSG_STRET_DECL]
#define umsg_super_stret_decl	objc_global_trees[OCTI_UMSG_SUPER_STRET_DECL]
#define objc_get_class_decl	objc_global_trees[OCTI_GET_CLASS_DECL]
#define objc_get_meta_class_decl			\
				objc_global_trees[OCTI_GET_MCLASS_DECL]

#define objc_super_type		objc_global_trees[OCTI_SUPER_TYPE]
#define objc_selector_type		objc_global_trees[OCTI_SEL_TYPE]
#define objc_object_type	objc_global_trees[OCTI_ID_TYPE]
#define objc_instancetype_type	objc_global_trees[OCTI_INSTANCE_TYPE]
#define objc_class_type		objc_global_trees[OCTI_CLS_TYPE]
#define objc_instance_type	objc_global_trees[OCTI_NST_TYPE]
#define objc_protocol_type	objc_global_trees[OCTI_PROTO_TYPE]

/* Type checking macros.  */

#define IS_ID(TYPE)							\
	(TREE_CODE (TYPE) == POINTER_TYPE				\
	 && (TYPE_MAIN_VARIANT (TREE_TYPE (TYPE))			\
	     == TREE_TYPE (objc_object_type)))

#define IS_CLASS(TYPE)							\
	(TREE_CODE (TYPE) == POINTER_TYPE				\
	 && (TYPE_MAIN_VARIANT (TREE_TYPE (TYPE))			\
	     == TREE_TYPE (objc_class_type)))

#define IS_PROTOCOL_QUALIFIED_UNTYPED(TYPE)				\
	((IS_ID (TYPE) || IS_CLASS (TYPE))				\
	 && TYPE_HAS_OBJC_INFO (TREE_TYPE (TYPE))			\
	 && TYPE_OBJC_PROTOCOL_LIST (TREE_TYPE (TYPE)))

#define IS_SUPER(TYPE)							\
	(TREE_CODE (TYPE) == POINTER_TYPE				\
	 && TREE_TYPE (TYPE) == objc_super_template)

#define interface_chain		objc_global_trees[OCTI_INTF_CHAIN]
#define protocol_chain		objc_global_trees[OCTI_PROTO_CHAIN]
#define implemented_classes	objc_global_trees[OCTI_IMPL_CHAIN]

/* Chains to manage selectors that are referenced and defined in the
   module.  */

#define cls_ref_chain		objc_global_trees[OCTI_CLS_REF_CHAIN]	/* Classes referenced.  */
#define sel_ref_chain		objc_global_trees[OCTI_SEL_REF_CHAIN]	/* Selectors referenced.  */
#define objc_ivar_chain		objc_global_trees[OCTI_IVAR_CHAIN]

/* Chains to manage uniquing of strings.  */

#define class_names_chain	objc_global_trees[OCTI_CLS_NAMES_CHAIN]
#define meth_var_names_chain	objc_global_trees[OCTI_METH_VAR_NAMES_CHAIN]
#define meth_var_types_chain	objc_global_trees[OCTI_METH_VAR_TYPES_CHAIN]


/* Backend data declarations.  */

#define UOBJC_SYMBOLS_decl		objc_global_trees[OCTI_SYMBOLS_DECL]
#define UOBJC_INSTANCE_VARIABLES_decl	objc_global_trees[OCTI_NST_VAR_DECL]
#define UOBJC_CLASS_VARIABLES_decl	objc_global_trees[OCTI_CLS_VAR_DECL]
#define UOBJC_INSTANCE_METHODS_decl	objc_global_trees[OCTI_NST_METH_DECL]
#define UOBJC_CLASS_METHODS_decl	objc_global_trees[OCTI_CLS_METH_DECL]
#define UOBJC_CLASS_decl		objc_global_trees[OCTI_CLS_DECL]
#define UOBJC_METACLASS_decl		objc_global_trees[OCTI_MCLS_DECL]
#define UOBJC_SELECTOR_TABLE_decl	objc_global_trees[OCTI_SEL_TABLE_DECL]
#define UOBJC_MODULES_decl		objc_global_trees[OCTI_MODULES_DECL]
#define GNU_INIT_decl			objc_global_trees[OCTI_GNU_INIT_DECL]

/* The following are used when compiling a class implementation.
   implementation_template will normally be an interface, however if
   none exists this will be equal to objc_implementation_context...it is
   set in start_class.  */

#define objc_interface_context		objc_global_trees[OCTI_INTF_CTX]
#define objc_implementation_context	objc_global_trees[OCTI_IMPL_CTX]
#define objc_method_context		objc_global_trees[OCTI_METH_CTX]
#define objc_ivar_context		objc_global_trees[OCTI_IVAR_CTX]

#define implementation_template	objc_global_trees[OCTI_IMPL_TEMPL]
#define objc_class_template	objc_global_trees[OCTI_CLS_TEMPL]
#define objc_category_template	objc_global_trees[OCTI_CAT_TEMPL]
#define uprivate_record		objc_global_trees[OCTI_UPRIV_REC]
#define objc_protocol_template	objc_global_trees[OCTI_PROTO_TEMPL]
#define objc_selector_template	objc_global_trees[OCTI_SEL_TEMPL]
#define ucls_super_ref		objc_global_trees[OCTI_UCLS_SUPER_REF]
#define uucls_super_ref		objc_global_trees[OCTI_UUCLS_SUPER_REF]

#define umsg_nonnil_decl	objc_global_trees[OCTI_UMSG_NONNIL_DECL]
#define umsg_nonnil_stret_decl	objc_global_trees[OCTI_UMSG_NONNIL_STRET_DECL]
#define objc_storage_class	objc_global_trees[OCTI_STORAGE_CLS]
#define objc_exception_extract_decl		\
				objc_global_trees[OCTI_EXCEPTION_EXTRACT_DECL]
#define objc_exception_try_enter_decl		\
				objc_global_trees[OCTI_EXCEPTION_TRY_ENTER_DECL]
#define objc_exception_try_exit_decl		\
				objc_global_trees[OCTI_EXCEPTION_TRY_EXIT_DECL]
#define objc_exception_match_decl		\
				objc_global_trees[OCTI_EXCEPTION_MATCH_DECL]
#define objc_exception_throw_decl		\
				objc_global_trees[OCTI_EXCEPTION_THROW_DECL]
#define objc_sync_enter_decl	objc_global_trees[OCTI_SYNC_ENTER_DECL]
#define objc_sync_exit_decl	objc_global_trees[OCTI_SYNC_EXIT_DECL]
#define objc_exception_data_template		\
				objc_global_trees[OCTI_EXCDATA_TEMPL]
#define objc_setjmp_decl	objc_global_trees[OCTI_SETJMP_DECL]
#define objc_stack_exception_data		\
				objc_global_trees[OCTI_STACK_EXCEPTION_DATA_DECL]
#define objc_caught_exception	objc_global_trees[OCTI_LOCAL_EXCEPTION_DECL]
#define objc_rethrow_exception	objc_global_trees[OCTI_RETHROW_EXCEPTION_DECL]
#define objc_eval_once		objc_global_trees[OCTI_EVAL_ONCE_DECL]
#define objc_catch_type		objc_global_trees[OCTI_CATCH_TYPE]

#define execclass_decl		objc_global_trees[OCTI_EXECCLASS_DECL]

#define objc_assign_ivar_decl	objc_global_trees[OCTI_ASSIGN_IVAR_DECL]
#define objc_assign_ivar_fast_decl		\
				objc_global_trees[OCTI_ASSIGN_IVAR_FAST_DECL]
#define objc_assign_global_decl	objc_global_trees[OCTI_ASSIGN_GLOBAL_DECL]
#define objc_assign_strong_cast_decl		\
				objc_global_trees[OCTI_ASSIGN_STRONGCAST_DECL]

#define objc_method_template	objc_global_trees[OCTI_METH_TEMPL]
#define objc_ivar_template	objc_global_trees[OCTI_IVAR_TEMPL]
#define objc_method_list_ptr	objc_global_trees[OCTI_METH_LIST_TEMPL]
#define objc_method_proto_list_ptr		\
				objc_global_trees[OCTI_METH_PROTO_LIST_TEMPL]
#define objc_ivar_list_ptr	objc_global_trees[OCTI_IVAR_LIST_TEMPL]
#define objc_symtab_template	objc_global_trees[OCTI_SYMTAB_TEMPL]
#define objc_module_template	objc_global_trees[OCTI_MODULE_TEMPL]
#define objc_super_template	objc_global_trees[OCTI_SUPER_TEMPL]
#define objc_object_reference	objc_global_trees[OCTI_OBJ_REF]
#define objc_class_reference	objc_global_trees[OCTI_CLS_REF]
#define objc_method_prototype_template		\
				objc_global_trees[OCTI_METH_PROTO_TEMPL]
#define function1_template	objc_global_trees[OCTI_FUNCTION1_TEMPL]
#define function2_template	objc_global_trees[OCTI_FUNCTION2_TEMPL]

#define objc_object_id		objc_global_trees[OCTI_OBJ_ID]
#define objc_class_id		objc_global_trees[OCTI_CLS_ID]
#define objc_object_name        objc_global_trees[OCTI_ID_NAME]
#define objc_instancetype_name	objc_global_trees[OCTI_INSTANCETYPE_NAME]
#define objc_class_name		objc_global_trees[OCTI_CLASS_NAME]

/* Constant string classes.  */
#define constant_string_id	objc_global_trees[OCTI_CNST_STR_ID]
#define constant_string_type	objc_global_trees[OCTI_CNST_STR_TYPE]
#define constant_string_global_id		\
				objc_global_trees[OCTI_CNST_STR_GLOB_ID]
#define string_class_decl	objc_global_trees[OCTI_STRING_CLASS_DECL]
#define internal_const_str_type	objc_global_trees[OCTI_INTERNAL_CNST_STR_TYPE]

#define UOBJC_SUPER_decl	objc_global_trees[OCTI_SUPER_DECL]
#define super_superclassfield_id \
				objc_global_trees[OCTI_SUPER_SUPERFIELD_ID]

#define objc_fast_enumeration_state_template	\
                                objc_global_trees[OCTI_FAST_ENUM_STATE_TEMP]
#define objc_enumeration_mutation_decl		\
                                objc_global_trees[OCTI_ENUM_MUTATION_DECL]

/* Declarations of functions used when synthesizing property
   accessors.  */
#define objc_getProperty_decl	objc_global_trees[OCTI_GET_PROPERTY_DECL]
#define objc_setProperty_decl	objc_global_trees[OCTI_SET_PROPERTY_DECL]
#define objc_copyStruct_decl	objc_global_trees[OCTI_COPY_STRUCT_DECL]
#define objc_getPropertyStruct_decl \
				objc_global_trees[OCTI_GET_PROPERTY_STRUCT_DECL]
#define objc_setPropertyStruct_decl \
				objc_global_trees[OCTI_SET_PROPERTY_STRUCT_DECL]

/* TODO: Add comment.  */
/* V1 stuff.  */
#define objc_prop_list_ptr	objc_global_trees[OCTI_V1_PROP_LIST_TEMPL]
#define prop_names_attr_chain	objc_global_trees[OCTI_V1_PROP_NAME_ATTR_CHAIN]

/* Reserved tag definitions.  */

#define OBJECT_TYPEDEF_NAME		"id"
#define INSTANCE_TYPEDEF_NAME		"instancetype"
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

#define PROTOCOL_OBJECT_CLASS_NAME	"Protocol"

#define TAG_EXCEPTIONTHROW		"objc_exception_throw"
#define TAG_SYNCENTER			"objc_sync_enter"
#define TAG_SYNCEXIT			"objc_sync_exit"

/* Really should be NeXT private.  */
#define UTAG_EXCDATA			"_objc_exception_data"

#define TAG_CXX_CONSTRUCT		".cxx_construct"
#define TAG_CXX_DESTRUCT		".cxx_destruct"

#define TAG_ENUMERATION_MUTATION        "objc_enumerationMutation"
#define TAG_FAST_ENUMERATION_STATE      "__objcFastEnumerationState"

enum string_section
{
  class_names,		/* class, category, protocol, module names */
  meth_var_names,	/* method and variable names */
  meth_var_types,	/* method and variable type descriptors */
  prop_names_attr	/* property names and their attributes. */
};

#define METHOD_DEF			0
#define METHOD_REF			1

#define BUFSIZE				1024

#define CLS_FACTORY			0x0001L
#define CLS_META			0x0002L

/* Runtime metadata flags - ??? apparently unused.  */

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

/* Exception handling constructs.  We begin by having the parser do most
   of the work and passing us blocks.
   This allows us to handle different exceptions implementations.  */

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

  /* The VAR_DECL holding  __builtin_eh_pointer (or equivalent).  */
  tree caught_decl;
  tree stack_decl;
  tree rethrow_decl;
};

/*  A small number of routines used by the FE parser and the runtime code
   generators.  Put here as inlines for efficiency in non-lto builds rather
   than making them externs.  */

extern tree objc_create_temporary_var (tree, const char *);

size_t objc_common_tree_size (enum tree_code code);


#define objc_is_object_id(TYPE) (OBJC_TYPE_NAME (TYPE) == objc_object_id)
#define objc_is_class_id(TYPE) (OBJC_TYPE_NAME (TYPE) == objc_class_id)

/* Retrieve category interface CAT_NAME (if any) associated with CLASS.  */
static inline tree
lookup_category (tree klass, tree cat_name)
{
  tree category = CLASS_CATEGORY_LIST (klass);

  while (category && CLASS_SUPER_NAME (category) != cat_name)
    category = CLASS_CATEGORY_LIST (category);
  return category;
}

/* Count only the fields occurring in T.  */
static inline int
ivar_list_length (tree t)
{
  int count = 0;

  for (; t; t = DECL_CHAIN (t))
    if (TREE_CODE (t) == FIELD_DECL)
      ++count;

  return count;
}

static inline tree
is_ivar (tree decl_chain, tree ident)
{
  for ( ; decl_chain; decl_chain = DECL_CHAIN (decl_chain))
    if (DECL_NAME (decl_chain) == ident)
      return decl_chain;
  return NULL_TREE;
}

#endif /* GCC_OBJC_ACT_H */
