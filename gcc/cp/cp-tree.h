/* Definitions for C++ parsing and type checking.
   Copyright (C) 1987, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
   2000, 2001, 2002, 2003 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com)

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
Boston, MA 02111-1307, USA.  */

#include "function.h"
#include "hashtab.h"
#include "splay-tree.h"
#include "varray.h"

#ifndef GCC_CP_TREE_H
#define GCC_CP_TREE_H

#ifndef __GNUC__
#error "You should be using 'make bootstrap' -- see installation instructions"
#endif

#include "c-common.h"

/* Usage of TREE_LANG_FLAG_?:
   0: BINFO_MARKED (BINFO nodes).
      IDENTIFIER_MARKED (IDENTIFIER_NODEs)
      NEW_EXPR_USE_GLOBAL (in NEW_EXPR).
      DELETE_EXPR_USE_GLOBAL (in DELETE_EXPR).
      LOOKUP_EXPR_GLOBAL (in LOOKUP_EXPR).
      TREE_INDIRECT_USING (in NAMESPACE_DECL).
      LOCAL_BINDING_P (in CPLUS_BINDING)
      ICS_USER_FLAG (in _CONV)
      CLEANUP_P (in TRY_BLOCK)
      AGGR_INIT_VIA_CTOR_P (in AGGR_INIT_EXPR)
      CTOR_BEGIN_P (in CTOR_STMT)
      BV_USE_VCALL_INDEX_P (in the BINFO_VIRTUALS TREE_LIST)
      PTRMEM_OK_P (in ADDR_EXPR, OFFSET_REF)
      PARMLIST_ELLIPSIS_P (in PARMLIST)
   1: IDENTIFIER_VIRTUAL_P.
      TI_PENDING_TEMPLATE_FLAG.
      TEMPLATE_PARMS_FOR_INLINE.
      DELETE_EXPR_USE_VEC (in DELETE_EXPR).
      (TREE_CALLS_NEW) (in _EXPR or _REF) (commented-out).
      TYPE_BASE_CONVS_MAY_REQUIRE_CODE_P (in _TYPE).
      INHERITED_VALUE_BINDING_P (in CPLUS_BINDING)
      BASELINK_P (in TREE_LIST)
      ICS_ELLIPSIS_FLAG (in _CONV)
      BINFO_ACCESS (in BINFO)
   2: IDENTIFIER_OPNAME_P.
      TYPE_POLYMORHPIC_P (in _TYPE)
      ICS_THIS_FLAG (in _CONV)
      BINDING_HAS_LEVEL_P (in CPLUS_BINDING)
      BINFO_LOST_PRIMARY_P (in BINFO)
      TREE_PARMLIST (in TREE_LIST)
   3: TYPE_USES_VIRTUAL_BASECLASSES (in a class TYPE).
      BINFO_VTABLE_PATH_MARKED.
      BINFO_PUSHDECLS_MARKED.
      (TREE_REFERENCE_EXPR) (in NON_LVALUE_EXPR) (commented-out).
      ICS_BAD_FLAG (in _CONV)
      FN_TRY_BLOCK_P (in TRY_BLOCK)
      IDENTIFIER_CTOR_OR_DTOR_P (in IDENTIFIER_NODE)
   4: BINFO_NEW_VTABLE_MARKED.
      TREE_HAS_CONSTRUCTOR (in INDIRECT_REF, SAVE_EXPR, CONSTRUCTOR,
          or FIELD_DECL).
      NEED_TEMPORARY_P (in REF_BIND, BASE_CONV)
      IDENTIFIER_TYPENAME_P (in IDENTIFIER_NODE)
   5: C_IS_RESERVED_WORD (in IDENTIFIER_NODE)
   6: BINFO_ACCESS (in BINFO)

   Usage of TYPE_LANG_FLAG_?:
   0: C_TYPE_FIELDS_READONLY (in RECORD_TYPE or UNION_TYPE).
   1: TYPE_HAS_CONSTRUCTOR.
   2: TYPE_HAS_DESTRUCTOR.
   3: TYPE_FOR_JAVA.
   4: TYPE_HAS_NONTRIVIAL_DESTRUCTOR
   5: IS_AGGR_TYPE.
   6: TYPE_BUILT_IN.

   Usage of DECL_LANG_FLAG_?:
   0: DECL_ERROR_REPORTED (in VAR_DECL).
      DECL_TEMPLATE_PARM_P (in PARM_DECL, CONST_DECL, TYPE_DECL, or TEMPLATE_DECL)
      DECL_LOCAL_FUNCTION_P (in FUNCTION_DECL)
      DECL_MUTABLE_P (in FIELD_DECL)
   1: C_TYPEDEF_EXPLICITLY_SIGNED (in TYPE_DECL).
      DECL_TEMPLATE_INSTANTIATED (in a VAR_DECL or a FUNCTION_DECL)
      DECL_C_BITFIELD (in FIELD_DECL)
   2: DECL_THIS_EXTERN (in VAR_DECL or FUNCTION_DECL).
      DECL_IMPLICIT_TYPEDEF_P (in a TYPE_DECL)
   3: DECL_IN_AGGR_P.
   4: DECL_C_BIT_FIELD
   5: DECL_INTERFACE_KNOWN.
   6: DECL_THIS_STATIC (in VAR_DECL or FUNCTION_DECL).
   7: DECL_DEAD_FOR_LOCAL (in VAR_DECL).
      DECL_THUNK_P (in a member FUNCTION_DECL)

   Usage of language-independent fields in a language-dependent manner:

   TREE_USED
     This field is BINFO_INDIRECT_PRIMARY_P in a BINFO.

   TYPE_ALIAS_SET
     This field is used by TYPENAME_TYPEs, TEMPLATE_TYPE_PARMs, and so
     forth as a substitute for the mark bits provided in `lang_type'.
     At present, only the six low-order bits are used.

   TYPE_BINFO
     For an ENUMERAL_TYPE, this is ENUM_TEMPLATE_INFO.
     For a FUNCTION_TYPE or METHOD_TYPE, this is TYPE_RAISES_EXCEPTIONS

  BINFO_VIRTUALS
     For a binfo, this is a TREE_LIST.  The BV_DELTA of each node
     gives the amount by which to adjust the `this' pointer when
     calling the function.  If the method is an overriden version of a
     base class method, then it is assumed that, prior to adjustment,
     the this pointer points to an object of the base class.

     The BV_VCALL_INDEX of each node, if non-NULL, gives the vtable
     index of the vcall offset for this entry.  If
     BV_USE_VCALL_INDEX_P then the corresponding vtable entry should
     use a virtual thunk, as opposed to an ordinary thunk.

     The BV_FN is the declaration for the virtual function itself.

   BINFO_VTABLE
     This is an expression with POINTER_TYPE that gives the value
     to which the vptr should be initialized.  Use get_vtbl_decl_for_binfo
     to extract the VAR_DECL for the complete vtable.

   DECL_ARGUMENTS
     For a VAR_DECL this is DECL_ANON_UNION_ELEMS.

   DECL_VINDEX
     This field is NULL for a non-virtual function.  For a virtual
     function, it is eventually set to an INTEGER_CST indicating the
     index in the vtable at which this function can be found.  When
     a virtual function is declared, but before it is known what
     function is overriden, this field is the error_mark_node.

     Temporarily, it may be set to a TREE_LIST whose TREE_VALUE is
     the virtual function this one overrides, and whose TREE_CHAIN is
     the old DECL_VINDEX.  */

/* Language-specific tree checkers. */

#if defined ENABLE_TREE_CHECKING && (GCC_VERSION >= 2007)

#define VAR_OR_FUNCTION_DECL_CHECK(NODE)			\
({  const tree __t = (NODE);					\
    enum tree_code const __c = TREE_CODE(__t);			\
    if (__c != VAR_DECL && __c != FUNCTION_DECL)		\
      tree_check_failed (__t, VAR_DECL, __FILE__, __LINE__,	\
			 __FUNCTION__);				\
    __t; })

#define VAR_FUNCTION_OR_PARM_DECL_CHECK(NODE)			\
({  const tree __t = (NODE);					\
    enum tree_code const __c = TREE_CODE(__t);			\
    if (__c != VAR_DECL						\
	&& __c != FUNCTION_DECL					\
        && __c != PARM_DECL)					\
      tree_check_failed (__t, VAR_DECL, __FILE__, __LINE__,	\
			 __FUNCTION__);				\
    __t; })

#define VAR_TEMPL_TYPE_OR_FUNCTION_DECL_CHECK(NODE)		\
({  const tree __t = (NODE);					\
    enum tree_code const __c = TREE_CODE(__t);			\
    if (__c != VAR_DECL						\
	&& __c != FUNCTION_DECL					\
	&& __c != TYPE_DECL					\
	&& __c != TEMPLATE_DECL)				\
      tree_check_failed (__t, VAR_DECL, __FILE__, __LINE__,	\
			 __FUNCTION__);				\
    __t; })

#define RECORD_OR_UNION_TYPE_CHECK(NODE)			\
({  const tree __t = (NODE);					\
    enum tree_code const __c = TREE_CODE(__t);			\
    if (__c != RECORD_TYPE && __c != UNION_TYPE)		\
      tree_check_failed (__t, RECORD_TYPE, __FILE__, __LINE__,	\
			 __FUNCTION__);				\
    __t; })

#define BOUND_TEMPLATE_TEMPLATE_PARM_TYPE_CHECK(NODE)		\
({  const tree __t = (NODE);					\
    enum tree_code const __c = TREE_CODE(__t);			\
    if (__c != BOUND_TEMPLATE_TEMPLATE_PARM)			\
      tree_check_failed (__t, BOUND_TEMPLATE_TEMPLATE_PARM,	\
			 __FILE__, __LINE__, __FUNCTION__);	\
    __t; })

#else /* not ENABLE_TREE_CHECKING, or not gcc */

#define VAR_OR_FUNCTION_DECL_CHECK(NODE)		(NODE)
#define VAR_FUNCTION_OR_PARM_DECL_CHECK(NODE)   	(NODE)
#define VAR_TEMPL_TYPE_OR_FUNCTION_DECL_CHECK(NODE)	(NODE)
#define RECORD_OR_UNION_TYPE_CHECK(NODE)		(NODE)
#define BOUND_TEMPLATE_TEMPLATE_PARM_TYPE_CHECK(NODE)	(NODE)

#endif


/* ABI control.  */

/* Nonzero to use __cxa_atexit, rather than atexit, to register
   destructors for local statics and global objects.  */

extern int flag_use_cxa_atexit;

/* Nonzero means generate 'rtti' that give run-time type information.  */

extern int flag_rtti;

/* Nonzero if we want to support huge (> 2^(sizeof(short)*8-1) bytes)
   objects.  */

extern int flag_huge_objects;


/* Language-dependent contents of an identifier.  */

struct lang_identifier
{
  struct c_common_identifier ignore;
  tree namespace_bindings;
  tree bindings;
  tree class_value;
  tree class_template_info;
  struct lang_id2 *x;
};

/* In an IDENTIFIER_NODE, nonzero if this identifier is actually a
   keyword.  C_RID_CODE (node) is then the RID_* value of the keyword,
   and C_RID_YYCODE is the token number wanted by Yacc.  */

#define C_IS_RESERVED_WORD(ID) TREE_LANG_FLAG_5 (ID)

extern const short rid_to_yy[RID_MAX];
#define C_RID_YYCODE(ID) rid_to_yy[C_RID_CODE (ID)]

#define LANG_IDENTIFIER_CAST(NODE) \
	((struct lang_identifier*)IDENTIFIER_NODE_CHECK (NODE))

struct lang_id2
{
  tree label_value, implicit_decl;
  tree error_locus;
};

typedef struct
{
  tree t;
  int new_type_flag;
  tree lookups;
} flagged_type_tree;

typedef struct
{
  struct tree_common common;
  HOST_WIDE_INT index;
  HOST_WIDE_INT level;
  HOST_WIDE_INT orig_level;
  tree decl;
} template_parm_index;

typedef struct ptrmem_cst
{
  struct tree_common common;
  /* This isn't used, but the middle-end expects all constants to have
     this field.  */
  rtx rtl;
  tree member;
}* ptrmem_cst_t;

/* Nonzero if this binding is for a local scope, as opposed to a class
   or namespace scope.  */
#define LOCAL_BINDING_P(NODE) TREE_LANG_FLAG_0 (NODE)

/* Nonzero if BINDING_VALUE is from a base class of the class which is
   currently being defined.  */
#define INHERITED_VALUE_BINDING_P(NODE) TREE_LANG_FLAG_1 (NODE)

/* For a binding between a name and an entity at a non-local scope,
   defines the scope where the binding is declared.  (Either a class
   _TYPE node, or a NAMESPACE_DECL.)  This macro should be used only
   for namespace-level bindings; on the IDENTIFIER_BINDING list
   BINDING_LEVEL is used instead.  */
#define BINDING_SCOPE(NODE) \
  (((struct tree_binding*)CPLUS_BINDING_CHECK (NODE))->scope.scope)

/* Nonzero if NODE has BINDING_LEVEL, rather than BINDING_SCOPE.  */
#define BINDING_HAS_LEVEL_P(NODE) TREE_LANG_FLAG_2 (NODE)

/* This is the declaration bound to the name. Possible values:
   variable, overloaded function, namespace, template, enumerator.  */
#define BINDING_VALUE(NODE) \
  (((struct tree_binding*)CPLUS_BINDING_CHECK (NODE))->value)

/* If name is bound to a type, this is the type (struct, union, enum).  */
#define BINDING_TYPE(NODE)     TREE_TYPE (NODE)

#define IDENTIFIER_GLOBAL_VALUE(NODE) \
  namespace_binding ((NODE), global_namespace)
#define SET_IDENTIFIER_GLOBAL_VALUE(NODE, VAL) \
  set_namespace_binding ((NODE), global_namespace, (VAL))
#define IDENTIFIER_NAMESPACE_VALUE(NODE) \
  namespace_binding ((NODE), current_namespace)
#define SET_IDENTIFIER_NAMESPACE_VALUE(NODE, VAL) \
  set_namespace_binding ((NODE), current_namespace, (VAL))

#define CLEANUP_P(NODE)         TREE_LANG_FLAG_0 (TRY_BLOCK_CHECK (NODE))

/* Returns nonzero iff TYPE1 and TYPE2 are the same type, in the usual
   sense of `same'.  */
#define same_type_p(TYPE1, TYPE2) \
  comptypes ((TYPE1), (TYPE2), COMPARE_STRICT)

/* Returns nonzero iff TYPE1 and TYPE2 are the same type, ignoring
   top-level qualifiers.  */
#define same_type_ignoring_top_level_qualifiers_p(TYPE1, TYPE2) \
  same_type_p (TYPE_MAIN_VARIANT (TYPE1), TYPE_MAIN_VARIANT (TYPE2))

/* Non-zero if we are presently building a statement tree, rather
   than expanding each statement as we encounter it.  */
#define building_stmt_tree() (last_tree != NULL_TREE)

/* Returns non-zero iff NODE is a declaration for the global function
   `main'.  */
#define DECL_MAIN_P(NODE)				\
   (DECL_EXTERN_C_FUNCTION_P (NODE)                     \
    && DECL_NAME (NODE) != NULL_TREE			\
    && MAIN_NAME_P (DECL_NAME (NODE)))


struct tree_binding
{
  struct tree_common common;
  union {
    tree scope;
    struct binding_level *level;
  } scope;
  tree value;
};

/* The overloaded FUNCTION_DECL. */
#define OVL_FUNCTION(NODE) \
  (((struct tree_overload*)OVERLOAD_CHECK (NODE))->function)
#define OVL_CHAIN(NODE)      TREE_CHAIN (NODE)
/* Polymorphic access to FUNCTION and CHAIN. */
#define OVL_CURRENT(NODE)     \
  ((TREE_CODE (NODE) == OVERLOAD) ? OVL_FUNCTION (NODE) : (NODE))
#define OVL_NEXT(NODE)        \
  ((TREE_CODE (NODE) == OVERLOAD) ? TREE_CHAIN (NODE) : NULL_TREE)
/* If set, this was imported in a using declaration.
   This is not to confuse with being used somewhere, which
   is not important for this node. */
#define OVL_USED(NODE)        TREE_USED (NODE)

struct tree_overload
{
  struct tree_common common;
  tree function;
};

/* A `baselink' is a TREE_LIST whose TREE_PURPOSE is a BINFO
   indicating a particular base class, and whose TREE_VALUE is a
   (possibly overloaded) function from that base class.  */
#define BASELINK_P(NODE) \
  (TREE_CODE (NODE) == TREE_LIST && TREE_LANG_FLAG_1 (NODE))
#define SET_BASELINK_P(NODE) \
  (TREE_LANG_FLAG_1 (NODE) = 1)

#define WRAPPER_PTR(NODE) (((struct tree_wrapper*)WRAPPER_CHECK (NODE))->u.ptr)
#define WRAPPER_INT(NODE) (((struct tree_wrapper*)WRAPPER_CHECK (NODE))->u.i)

struct tree_wrapper
{
  struct tree_common common;
  union {
    void *ptr;
    int i;
  } u;
};

#define SRCLOC_FILE(NODE) (((struct tree_srcloc*)SRCLOC_CHECK (NODE))->filename)
#define SRCLOC_LINE(NODE) (((struct tree_srcloc*)SRCLOC_CHECK (NODE))->linenum)
struct tree_srcloc
{
  struct tree_common common;
  const char *filename;
  int linenum;
};

/* Macros for access to language-specific slots in an identifier.  */

#define IDENTIFIER_NAMESPACE_BINDINGS(NODE)	\
  (LANG_IDENTIFIER_CAST (NODE)->namespace_bindings)
#define IDENTIFIER_TEMPLATE(NODE)	\
  (LANG_IDENTIFIER_CAST (NODE)->class_template_info)

/* The IDENTIFIER_BINDING is the innermost CPLUS_BINDING for the
    identifier.  It's TREE_CHAIN is the next outermost binding.  Each
    BINDING_VALUE is a DECL for the associated declaration.  Thus,
    name lookup consists simply of pulling off the node at the front
    of the list (modulo oddities for looking up the names of types,
    and such.)  You can use BINDING_SCOPE or BINDING_LEVEL to
    determine the scope that bound the name.  */
#define IDENTIFIER_BINDING(NODE) \
  (LANG_IDENTIFIER_CAST (NODE)->bindings)

/* The IDENTIFIER_VALUE is the value of the IDENTIFIER_BINDING, or
   NULL_TREE if there is no binding.  */
#define IDENTIFIER_VALUE(NODE)			\
  (IDENTIFIER_BINDING (NODE)			\
   ? BINDING_VALUE (IDENTIFIER_BINDING (NODE))	\
   : NULL_TREE)

/* If IDENTIFIER_CLASS_VALUE is set, then NODE is bound in the current
   class, and IDENTIFIER_CLASS_VALUE is the value binding.  This is
   just a pointer to the BINDING_VALUE of one of the bindings in the
   IDENTIFIER_BINDINGs list, so any time that this is non-NULL so is
   IDENTIFIER_BINDING.  */
#define IDENTIFIER_CLASS_VALUE(NODE) \
  (LANG_IDENTIFIER_CAST (NODE)->class_value)

/* TREE_TYPE only indicates on local and class scope the current
   type. For namespace scope, the presence of a type in any namespace
   is indicated with global_type_node, and the real type behind must
   be found through lookup. */
#define IDENTIFIER_TYPE_VALUE(NODE) identifier_type_value (NODE)
#define REAL_IDENTIFIER_TYPE_VALUE(NODE) TREE_TYPE (NODE)
#define SET_IDENTIFIER_TYPE_VALUE(NODE,TYPE) (TREE_TYPE (NODE) = (TYPE))
#define IDENTIFIER_HAS_TYPE_VALUE(NODE) (IDENTIFIER_TYPE_VALUE (NODE) ? 1 : 0)

#define LANG_ID_FIELD(NAME, NODE)			\
  (LANG_IDENTIFIER_CAST (NODE)->x			\
   ? LANG_IDENTIFIER_CAST (NODE)->x->NAME : 0)

#define SET_LANG_ID(NODE, VALUE, NAME)					  \
  (LANG_IDENTIFIER_CAST (NODE)->x == 0				  	  \
   ? LANG_IDENTIFIER_CAST (NODE)->x					  \
      = (struct lang_id2 *)perm_calloc (1, sizeof (struct lang_id2)) : 0, \
   LANG_IDENTIFIER_CAST (NODE)->x->NAME = (VALUE))

#define IDENTIFIER_LABEL_VALUE(NODE) \
  LANG_ID_FIELD (label_value, NODE)
#define SET_IDENTIFIER_LABEL_VALUE(NODE, VALUE)   \
  SET_LANG_ID (NODE, VALUE, label_value)

#define IDENTIFIER_IMPLICIT_DECL(NODE) \
  LANG_ID_FIELD (implicit_decl, NODE)
#define SET_IDENTIFIER_IMPLICIT_DECL(NODE, VALUE) \
  SET_LANG_ID (NODE, VALUE, implicit_decl)

#define IDENTIFIER_ERROR_LOCUS(NODE) \
  LANG_ID_FIELD (error_locus, NODE)
#define SET_IDENTIFIER_ERROR_LOCUS(NODE, VALUE)	\
  SET_LANG_ID (NODE, VALUE, error_locus)

/* Nonzero if this identifier is used as a virtual function name somewhere
   (optimizes searches).  */
#define IDENTIFIER_VIRTUAL_P(NODE) TREE_LANG_FLAG_1 (NODE)

/* Nonzero if this identifier is the prefix for a mangled C++ operator
   name.  */
#define IDENTIFIER_OPNAME_P(NODE) TREE_LANG_FLAG_2 (NODE)

/* Nonzero if this identifier is the name of a type-conversion
   operator.  */
#define IDENTIFIER_TYPENAME_P(NODE) \
  TREE_LANG_FLAG_4 (NODE)

/* Nonzero if this identifier is the name of a constructor or
   destructor.  */
#define IDENTIFIER_CTOR_OR_DTOR_P(NODE) \
  TREE_LANG_FLAG_3 (NODE)

/* In a RECORD_TYPE or UNION_TYPE, nonzero if any component is read-only.  */
#define C_TYPE_FIELDS_READONLY(TYPE) TYPE_LANG_FLAG_0 (TYPE)

/* Store a value in that field.  */
#define C_SET_EXP_ORIGINAL_CODE(EXP, CODE) \
  (TREE_COMPLEXITY (EXP) = (int)(CODE))

/* If non-zero, a VAR_DECL whose cleanup will cause a throw to the
   next exception handler.  */
extern tree exception_throw_decl;

enum cp_tree_index
{
    CPTI_JAVA_BYTE_TYPE,
    CPTI_JAVA_SHORT_TYPE,
    CPTI_JAVA_INT_TYPE,
    CPTI_JAVA_LONG_TYPE,
    CPTI_JAVA_FLOAT_TYPE,
    CPTI_JAVA_DOUBLE_TYPE,
    CPTI_JAVA_CHAR_TYPE,
    CPTI_JAVA_BOOLEAN_TYPE,

    CPTI_WCHAR_DECL,
    CPTI_VTABLE_ENTRY_TYPE,
    CPTI_DELTA_TYPE,
    CPTI_VTABLE_INDEX_TYPE,
    CPTI_CLEANUP_TYPE,
    CPTI_VTT_PARM_TYPE,

    CPTI_TI_DESC_TYPE,
    CPTI_BLTN_DESC_TYPE,
    CPTI_PTR_DESC_TYPE,
    CPTI_ARY_DESC_TYPE,
    CPTI_FUNC_DESC_TYPE,
    CPTI_ENUM_DESC_TYPE,
    CPTI_CLASS_DESC_TYPE,
    CPTI_SI_CLASS_DESC_TYPE,
    CPTI_VMI_CLASS_DESC_TYPE,
    CPTI_PTM_DESC_TYPE,
    CPTI_BASE_DESC_TYPE,

    CPTI_CLASS_TYPE,
    CPTI_RECORD_TYPE,
    CPTI_UNION_TYPE,
    CPTI_ENUM_TYPE,
    CPTI_UNKNOWN_TYPE,
    CPTI_VTBL_TYPE,
    CPTI_VTBL_PTR_TYPE,
    CPTI_STD,
    CPTI_ABI,
    CPTI_TYPE_INFO_TYPE,
    CPTI_TYPE_INFO_PTR_TYPE,
    CPTI_ABORT_FNDECL,
    CPTI_GLOBAL_DELETE_FNDECL,
    CPTI_AGGR_TAG,

    CPTI_ACCESS_DEFAULT,
    CPTI_ACCESS_PUBLIC,
    CPTI_ACCESS_PROTECTED,
    CPTI_ACCESS_PRIVATE,
    CPTI_ACCESS_DEFAULT_VIRTUAL,
    CPTI_ACCESS_PUBLIC_VIRTUAL,
    CPTI_ACCESS_PROTECTED_VIRTUAL,
    CPTI_ACCESS_PRIVATE_VIRTUAL,

    CPTI_CTOR_IDENTIFIER,
    CPTI_COMPLETE_CTOR_IDENTIFIER,
    CPTI_BASE_CTOR_IDENTIFIER,
    CPTI_DTOR_IDENTIFIER,
    CPTI_COMPLETE_DTOR_IDENTIFIER,
    CPTI_BASE_DTOR_IDENTIFIER,
    CPTI_DELETING_DTOR_IDENTIFIER,
    CPTI_DELTA_IDENTIFIER,
    CPTI_IN_CHARGE_IDENTIFIER,
    CPTI_VTT_PARM_IDENTIFIER,
    CPTI_NELTS_IDENTIFIER,
    CPTI_THIS_IDENTIFIER,
    CPTI_PFN_IDENTIFIER,
    CPTI_VPTR_IDENTIFIER,
    CPTI_STD_IDENTIFIER,

    CPTI_LANG_NAME_C,
    CPTI_LANG_NAME_CPLUSPLUS,
    CPTI_LANG_NAME_JAVA,

    CPTI_EMPTY_EXCEPT_SPEC,
    CPTI_NULL,
    CPTI_JCLASS,
    CPTI_TERMINATE,
    CPTI_CALL_UNEXPECTED,
    CPTI_ATEXIT,
    CPTI_DSO_HANDLE,
    CPTI_DCAST,

    CPTI_MAX
};

extern tree cp_global_trees[CPTI_MAX];

#define java_byte_type_node		cp_global_trees[CPTI_JAVA_BYTE_TYPE]
#define java_short_type_node		cp_global_trees[CPTI_JAVA_SHORT_TYPE]
#define java_int_type_node		cp_global_trees[CPTI_JAVA_INT_TYPE]
#define java_long_type_node		cp_global_trees[CPTI_JAVA_LONG_TYPE]
#define java_float_type_node		cp_global_trees[CPTI_JAVA_FLOAT_TYPE]
#define java_double_type_node		cp_global_trees[CPTI_JAVA_DOUBLE_TYPE]
#define java_char_type_node		cp_global_trees[CPTI_JAVA_CHAR_TYPE]
#define java_boolean_type_node		cp_global_trees[CPTI_JAVA_BOOLEAN_TYPE]

#define wchar_decl_node			cp_global_trees[CPTI_WCHAR_DECL]
#define vtable_entry_type		cp_global_trees[CPTI_VTABLE_ENTRY_TYPE]
/* The type used to represent an offset by which to adjust the `this'
   pointer in pointer-to-member types.  */
#define delta_type_node			cp_global_trees[CPTI_DELTA_TYPE]
/* The type used to represent an index into the vtable.  */
#define vtable_index_type               cp_global_trees[CPTI_VTABLE_INDEX_TYPE]

#define ti_desc_type_node		cp_global_trees[CPTI_TI_DESC_TYPE]
#define bltn_desc_type_node		cp_global_trees[CPTI_BLTN_DESC_TYPE]
#define ptr_desc_type_node		cp_global_trees[CPTI_PTR_DESC_TYPE]
#define ary_desc_type_node		cp_global_trees[CPTI_ARY_DESC_TYPE]
#define func_desc_type_node		cp_global_trees[CPTI_FUNC_DESC_TYPE]
#define enum_desc_type_node		cp_global_trees[CPTI_ENUM_DESC_TYPE]
#define class_desc_type_node		cp_global_trees[CPTI_CLASS_DESC_TYPE]
#define si_class_desc_type_node		cp_global_trees[CPTI_SI_CLASS_DESC_TYPE]
#define vmi_class_desc_type_node	cp_global_trees[CPTI_VMI_CLASS_DESC_TYPE]
#define ptm_desc_type_node		cp_global_trees[CPTI_PTM_DESC_TYPE]
#define base_desc_type_node		cp_global_trees[CPTI_BASE_DESC_TYPE]

#define class_type_node			cp_global_trees[CPTI_CLASS_TYPE]
#define record_type_node		cp_global_trees[CPTI_RECORD_TYPE]
#define union_type_node			cp_global_trees[CPTI_UNION_TYPE]
#define enum_type_node			cp_global_trees[CPTI_ENUM_TYPE]
#define unknown_type_node		cp_global_trees[CPTI_UNKNOWN_TYPE]
#define vtbl_type_node			cp_global_trees[CPTI_VTBL_TYPE]
#define vtbl_ptr_type_node		cp_global_trees[CPTI_VTBL_PTR_TYPE]
#define std_node			cp_global_trees[CPTI_STD]
#define abi_node                        cp_global_trees[CPTI_ABI]
#define type_info_type_node		cp_global_trees[CPTI_TYPE_INFO_TYPE]
#define type_info_ptr_type		cp_global_trees[CPTI_TYPE_INFO_PTR_TYPE]
#define abort_fndecl			cp_global_trees[CPTI_ABORT_FNDECL]
#define global_delete_fndecl		cp_global_trees[CPTI_GLOBAL_DELETE_FNDECL]
#define current_aggr			cp_global_trees[CPTI_AGGR_TAG]

/* Define the sets of attributes that member functions and baseclasses
   can have.  These are sensible combinations of {public,private,protected}
   cross {virtual,non-virtual}.  */

#define access_default_node             cp_global_trees[CPTI_ACCESS_DEFAULT]
#define access_public_node              cp_global_trees[CPTI_ACCESS_PUBLIC]
#define access_protected_node           cp_global_trees[CPTI_ACCESS_PROTECTED]
#define access_private_node             cp_global_trees[CPTI_ACCESS_PRIVATE]
#define access_default_virtual_node     cp_global_trees[CPTI_ACCESS_DEFAULT_VIRTUAL]
#define access_public_virtual_node      cp_global_trees[CPTI_ACCESS_PUBLIC_VIRTUAL]
#define access_protected_virtual_node   cp_global_trees[CPTI_ACCESS_PROTECTED_VIRTUAL]
#define access_private_virtual_node     cp_global_trees[CPTI_ACCESS_PRIVATE_VIRTUAL]

/* We cache these tree nodes so as to call get_identifier less
   frequently.  */

/* The name of a constructor that takes an in-charge parameter to
   decide whether or not to construct virtual base classes.  */
#define ctor_identifier                 cp_global_trees[CPTI_CTOR_IDENTIFIER]
/* The name of a constructor that constructs virtual base classes.  */
#define complete_ctor_identifier        cp_global_trees[CPTI_COMPLETE_CTOR_IDENTIFIER]
/* The name of a constructor that does not construct virtual base classes.  */
#define base_ctor_identifier            cp_global_trees[CPTI_BASE_CTOR_IDENTIFIER]
/* The name of a destructor that takes an in-charge parameter to
   decide whether or not to destroy virtual base classes and whether
   or not to delete the object.  */
#define dtor_identifier                 cp_global_trees[CPTI_DTOR_IDENTIFIER]
/* The name of a destructor that destroys virtual base classes.  */
#define complete_dtor_identifier        cp_global_trees[CPTI_COMPLETE_DTOR_IDENTIFIER]
/* The name of a destructor that does not destroy virtual base
   classes.  */
#define base_dtor_identifier            cp_global_trees[CPTI_BASE_DTOR_IDENTIFIER]
/* The name of a destructor that destroys virtual base classes, and
   then deletes the entire object.  */
#define deleting_dtor_identifier        cp_global_trees[CPTI_DELETING_DTOR_IDENTIFIER]
#define delta_identifier                cp_global_trees[CPTI_DELTA_IDENTIFIER]
#define in_charge_identifier            cp_global_trees[CPTI_IN_CHARGE_IDENTIFIER]
/* The name of the parameter that contains a pointer to the VTT to use
   for this subobject constructor or destructor.  */
#define vtt_parm_identifier             cp_global_trees[CPTI_VTT_PARM_IDENTIFIER]
#define nelts_identifier                cp_global_trees[CPTI_NELTS_IDENTIFIER]
#define this_identifier                 cp_global_trees[CPTI_THIS_IDENTIFIER]
#define pfn_identifier                  cp_global_trees[CPTI_PFN_IDENTIFIER]
#define vptr_identifier                 cp_global_trees[CPTI_VPTR_IDENTIFIER]
/* The name of the std namespace.  */
#define std_identifier                  cp_global_trees[CPTI_STD_IDENTIFIER]
#define lang_name_c                     cp_global_trees[CPTI_LANG_NAME_C]
#define lang_name_cplusplus             cp_global_trees[CPTI_LANG_NAME_CPLUSPLUS]
#define lang_name_java                  cp_global_trees[CPTI_LANG_NAME_JAVA]

/* Exception specifier used for throw().  */
#define empty_except_spec               cp_global_trees[CPTI_EMPTY_EXCEPT_SPEC]

/* The node for `__null'.  */
#define null_node                       cp_global_trees[CPTI_NULL]

/* If non-NULL, a POINTER_TYPE equivalent to (java::lang::Class*). */
#define jclass_node                     cp_global_trees[CPTI_JCLASS]

/* The declaration for `std::terminate'.  */
#define terminate_node                  cp_global_trees[CPTI_TERMINATE]

/* The declaration for "__cxa_call_unexpected".  */
#define call_unexpected_node            cp_global_trees[CPTI_CALL_UNEXPECTED]

/* A pointer to `std::atexit'.  */
#define atexit_node                     cp_global_trees[CPTI_ATEXIT]

/* A pointer to `__dso_handle'.  */
#define dso_handle_node                 cp_global_trees[CPTI_DSO_HANDLE]

/* The declaration of the dynamic_cast runtime.  */
#define dynamic_cast_node               cp_global_trees[CPTI_DCAST]

/* The type of a destructor.  */
#define cleanup_type                    cp_global_trees[CPTI_CLEANUP_TYPE]

/* The type of the vtt parameter passed to subobject constructors and
   destructors.  */
#define vtt_parm_type                   cp_global_trees[CPTI_VTT_PARM_TYPE]

/* Global state.  */

struct saved_scope
{
  tree old_bindings;
  tree old_namespace;
  tree decl_ns_list;
  tree class_name;
  tree class_type;
  tree access_specifier;
  tree function_decl;
  varray_type lang_base;
  tree lang_name;
  tree template_parms;
  tree x_previous_class_type;
  tree x_previous_class_values;
  tree x_saved_tree;
  tree lookups;
  tree last_parms;

  HOST_WIDE_INT x_processing_template_decl;
  int x_processing_specialization;
  int x_processing_explicit_instantiation;
  int need_pop_function_context;

  struct stmt_tree_s x_stmt_tree;

  struct binding_level *class_bindings;
  struct binding_level *bindings;

  struct saved_scope *prev;
};

/* The current open namespace.  */

#define current_namespace scope_chain->old_namespace

/* The stack for namespaces of current declarations. */

#define decl_namespace_list scope_chain->decl_ns_list

/* IDENTIFIER_NODE: name of current class */

#define current_class_name scope_chain->class_name

/* _TYPE: the type of the current class */

#define current_class_type scope_chain->class_type

/* When parsing a class definition, the access specifier most recently
   given by the user, or, if no access specifier was given, the
   default value appropriate for the kind of class (i.e., struct,
   class, or union).  */

#define current_access_specifier scope_chain->access_specifier

/* Pointer to the top of the language name stack.  */

#define current_lang_base scope_chain->lang_base
#define current_lang_name scope_chain->lang_name

/* Parsing a function declarator leaves a list of parameter names
   or a chain or parameter decls here.  */

#define current_template_parms scope_chain->template_parms

#define processing_template_decl scope_chain->x_processing_template_decl
#define processing_specialization scope_chain->x_processing_specialization
#define processing_explicit_instantiation scope_chain->x_processing_explicit_instantiation

/* _TYPE: the previous type that was a class */

#define previous_class_type scope_chain->x_previous_class_type

/* This is a copy of the class_shadowed list of the previous class
   binding contour when at global scope.  It's used to reset
   IDENTIFIER_CLASS_VALUEs when entering another class scope (i.e. a
   cache miss).  */

#define previous_class_values scope_chain->x_previous_class_values

/* A list of private types mentioned, for deferred access checking.  */

#define type_lookups scope_chain->lookups

extern struct saved_scope *scope_chain;

struct unparsed_text;

/* Global state pertinent to the current function.  */

struct cp_language_function
{
  struct language_function base;

  tree x_dtor_label;
  tree x_current_class_ptr;
  tree x_current_class_ref;
  tree x_eh_spec_block;
  tree x_in_charge_parm;
  tree x_vtt_parm;
  tree x_return_value;

  tree *x_vcalls_possible_p;

  int returns_value;
  int returns_null;
  int returns_abnormally;
  int in_function_try_handler;
  int x_expanding_p;

  struct named_label_use_list *x_named_label_uses;
  struct named_label_list *x_named_labels;
  struct binding_level *bindings;
  varray_type x_local_names;

  const char *cannot_inline;
  struct unparsed_text *unparsed_inlines;
};

/* The current C++-specific per-function global variables.  */

#define cp_function_chain \
  ((struct cp_language_function *) (cfun->language))

/* In a destructor, the point at which all derived class destroying
   has been done, just before any base class destroying will be done.  */

#define dtor_label cp_function_chain->x_dtor_label

/* When we're processing a member function, current_class_ptr is the
   PARM_DECL for the `this' pointer.  The current_class_ref is an
   expression for `*this'.  */

#define current_class_ptr \
  (cfun ? cp_function_chain->x_current_class_ptr : NULL_TREE)
#define current_class_ref \
  (cfun ? cp_function_chain->x_current_class_ref : NULL_TREE)

/* The EH_SPEC_BLOCK for the exception-specifiers for the current
   function, if any.  */

#define current_eh_spec_block cp_function_chain->x_eh_spec_block

/* The `__in_chrg' parameter for the current function.  Only used for
   constructors and destructors.  */

#define current_in_charge_parm cp_function_chain->x_in_charge_parm

/* The `__vtt_parm' parameter for the current function.  Only used for
   constructors and destructors.  */

#define current_vtt_parm cp_function_chain->x_vtt_parm

/* In destructors, this is a pointer to a condition in an
   if-statement.  If the pointed-to value is boolean_true_node, then
   there may be virtual function calls in this destructor.  */

#define current_vcalls_possible_p cp_function_chain->x_vcalls_possible_p

/* Set to 0 at beginning of a function definition, set to 1 if
   a return statement that specifies a return value is seen.  */

#define current_function_returns_value cp_function_chain->returns_value

/* Set to 0 at beginning of a function definition, set to 1 if
   a return statement with no argument is seen.  */

#define current_function_returns_null cp_function_chain->returns_null

/* Set to 0 at beginning of a function definition, set to 1 if
   a call to a noreturn function is seen.  */

#define current_function_returns_abnormally \
  cp_function_chain->returns_abnormally

/* Non-zero if we should generate RTL for functions that we process.
   When this is zero, we just accumulate tree structure, without
   interacting with the back end.  */

#define expanding_p cp_function_chain->x_expanding_p

/* Non-zero if we are in the semantic analysis phase for the current
   function.  */

#define doing_semantic_analysis_p() (!expanding_p)

#define in_function_try_handler cp_function_chain->in_function_try_handler

/* Expression always returned from function, or error_mark_node
   otherwise, for use by the automatic named return value optimization.  */

#define current_function_return_value \
  (cp_function_chain->x_return_value)

extern tree global_namespace;

#define ansi_opname(CODE) \
  (operator_name_info[(int) (CODE)].identifier)
#define ansi_assopname(CODE) \
  (assignment_operator_name_info[(int) (CODE)].identifier)

/* Nonzero means `$' can be in an identifier.  */

extern int dollars_in_ident;

/* Nonzero means don't recognize the keyword `asm'.  */

extern int flag_no_asm;

/* Nonzero means don't recognize any extended keywords.  */

extern int flag_no_gnu_keywords;

/* Nonzero means recognize the named operators from C++98.  */

extern int flag_operator_names;

/* For environments where you can use GNU binutils (as, ld in particular).  */

extern int flag_gnu_binutils;

/* Nonzero means warn about things that will change when compiling
   with an ABI-compliant compiler.  */

extern int warn_abi;

/* Nonzero means warn about implicit declarations.  */

extern int warn_implicit;

/* Nonzero means warn when all ctors or dtors are private, and the class
   has no friends.  */

extern int warn_ctor_dtor_privacy;

/* Nonzero means warn about deprecated conversion from string constant to
   `char *'.  */

extern int warn_write_strings;

/* Nonzero means warn about multiple (redundant) decls for the same single
   variable or function.  */

extern int warn_redundant_decls;

/* Warn if initializer is not completely bracketed.  */

extern int warn_missing_braces;

/* Warn about comparison of signed and unsigned values.  */

extern int warn_sign_compare;

/* Warn about testing equality of floating point numbers. */

extern int warn_float_equal;

/* Warn about a subscript that has type char.  */

extern int warn_char_subscripts;

/* Nonzero means warn about pointer casts that can drop a type qualifier
   from the pointer target type.  */

extern int warn_cast_qual;

/* Nonzero means warn about non virtual destructors in classes that have
   virtual functions.  */

extern int warn_nonvdtor;

/* Non-zero means warn when we convert a pointer to member function
   into a pointer to (void or function).  */

extern int warn_pmf2ptr;

/* Nonzero means warn about violation of some Effective C++ style rules.  */

extern int warn_ecpp;

/* Nonzero means warn where overload resolution chooses a promotion from
   unsigned to signed over a conversion to an unsigned of the same size.  */

extern int warn_sign_promo;

/* Non-zero means warn when an old-style cast is used.  */

extern int warn_old_style_cast;

/* Non-zero means warn when the compiler will reorder code.  */

extern int warn_reorder;

/* Non-zero means warn about deprecated features.  */

extern int warn_deprecated;

/* Nonzero means to treat bitfields as unsigned unless they say `signed'.  */

extern int flag_signed_bitfields;

/* INTERFACE_ONLY nonzero means that we are in an "interface"
   section of the compiler.  INTERFACE_UNKNOWN nonzero means
   we cannot trust the value of INTERFACE_ONLY.  If INTERFACE_UNKNOWN
   is zero and INTERFACE_ONLY is zero, it means that we are responsible
   for exporting definitions that others might need.  */
extern int interface_only, interface_unknown;

/* Nonzero means we should attempt to elide constructors when possible.  */

extern int flag_elide_constructors;

/* Nonzero means enable obscure standard features and disable GNU
   extensions that might cause standard-compliant code to be
   miscompiled.  */

extern int flag_ansi;

/* Nonzero means that member functions defined in class scope are
   inline by default.  */

extern int flag_default_inline;

/* Nonzero means generate separate instantiation control files and juggle
   them at link time.  */
extern int flag_use_repository;

/* Nonzero if we want to issue diagnostics that the standard says are not
   required.  */
extern int flag_optional_diags;

/* Nonzero means output .vtable_{entry,inherit} for use in doing vtable gc.  */
extern int flag_vtable_gc;

/* Nonzero means make the default pedwarns warnings instead of errors.
   The value of this flag is ignored if -pedantic is specified.  */
extern int flag_permissive;

/* Nonzero means to implement standard semantics for exception
   specifications, calling unexpected if an exception is thrown that
   doesn't match the specification.  Zero means to treat them as
   assertions and optimize accordingly, but not check them.  */
extern int flag_enforce_eh_specs;

/* Nonzero if we want to obey access control semantics.  */

extern int flag_access_control;

/* Nonzero if we want to check the return value of new and avoid calling
   constructors if it is a null pointer.  */

extern int flag_check_new;


/* C++ language-specific tree codes.  */
#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) SYM,
enum cplus_tree_code {
  CP_DUMMY_TREE_CODE = LAST_C_TREE_CODE,
#include "cp-tree.def"
  LAST_CPLUS_TREE_CODE
};
#undef DEFTREECODE

enum languages { lang_c, lang_cplusplus, lang_java };

/* Macros to make error reporting functions' lives easier.  */
#define TYPE_IDENTIFIER(NODE) (DECL_NAME (TYPE_NAME (NODE)))
#define TYPE_LINKAGE_IDENTIFIER(NODE) \
  (TYPE_IDENTIFIER (TYPE_MAIN_VARIANT (NODE)))
#define TYPE_NAME_STRING(NODE) (IDENTIFIER_POINTER (TYPE_IDENTIFIER (NODE)))
#define TYPE_NAME_LENGTH(NODE) (IDENTIFIER_LENGTH (TYPE_IDENTIFIER (NODE)))

#define TYPE_ASSEMBLER_NAME_STRING(NODE) \
  (IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (TYPE_NAME  (NODE))))
#define TYPE_ASSEMBLER_NAME_LENGTH(NODE) \
  (IDENTIFIER_LENGTH (DECL_ASSEMBLER_NAME (TYPE_NAME (NODE))))

/* Nonzero if NODE has no name for linkage purposes.  */
#define TYPE_ANONYMOUS_P(NODE) \
  (TAGGED_TYPE_P (NODE) && ANON_AGGRNAME_P (TYPE_LINKAGE_IDENTIFIER (NODE)))

/* The _DECL for this _TYPE.  */
#define TYPE_MAIN_DECL(NODE) (TYPE_STUB_DECL (TYPE_MAIN_VARIANT (NODE)))

/* Nonzero if T is a class (or struct or union) type.  Also nonzero
   for template type parameters, typename types, and instantiated
   template template parameters.  Despite its name,
   this macro has nothing to do with the definition of aggregate given
   in the standard.  Think of this macro as MAYBE_CLASS_TYPE_P.  */
#define IS_AGGR_TYPE(T)					\
  (TREE_CODE (T) == TEMPLATE_TYPE_PARM			\
   || TREE_CODE (T) == TYPENAME_TYPE			\
   || TREE_CODE (T) == TYPEOF_TYPE			\
   || TREE_CODE (T) == BOUND_TEMPLATE_TEMPLATE_PARM	\
   || TYPE_LANG_FLAG_5 (T))

/* Set IS_AGGR_TYPE for T to VAL.  T must be a class, struct, or
   union type.  */
#define SET_IS_AGGR_TYPE(T, VAL) \
  (TYPE_LANG_FLAG_5 (T) = (VAL))

/* Nonzero if T is a class type.  Zero for template type parameters,
   typename types, and so forth.  */
#define CLASS_TYPE_P(T) \
  (IS_AGGR_TYPE_CODE (TREE_CODE (T)) && IS_AGGR_TYPE (T))

#define IS_AGGR_TYPE_CODE(T)	((T) == RECORD_TYPE || (T) == UNION_TYPE)
#define IS_AGGR_TYPE_2(TYPE1, TYPE2) \
  (TREE_CODE (TYPE1) == TREE_CODE (TYPE2)	\
   && IS_AGGR_TYPE (TYPE1) && IS_AGGR_TYPE (TYPE2))
#define TAGGED_TYPE_P(T) \
  (CLASS_TYPE_P (T) || TREE_CODE (T) == ENUMERAL_TYPE)
#define IS_OVERLOAD_TYPE(T) TAGGED_TYPE_P (T)

/* In a *_TYPE, nonzero means a built-in type.  */
#define TYPE_BUILT_IN(NODE) TYPE_LANG_FLAG_6 (NODE)

/* True if this a "Java" type, defined in 'extern "Java"'. */
#define TYPE_FOR_JAVA(NODE) TYPE_LANG_FLAG_3 (NODE)

/* Nonzero if this type is const-qualified.  */
#define CP_TYPE_CONST_P(NODE)				\
  ((cp_type_quals (NODE) & TYPE_QUAL_CONST) != 0)

/* Nonzero if this type is volatile-qualified.  */
#define CP_TYPE_VOLATILE_P(NODE)			\
  ((cp_type_quals (NODE) & TYPE_QUAL_VOLATILE) != 0)

/* Nonzero if this type is restrict-qualified.  */
#define CP_TYPE_RESTRICT_P(NODE)			\
  ((cp_type_quals (NODE) & TYPE_QUAL_RESTRICT) != 0)

/* Nonzero if this type is const-qualified, but not
   volatile-qualified.  Other qualifiers are ignored.  This macro is
   used to test whether or not it is OK to bind an rvalue to a
   reference.  */
#define CP_TYPE_CONST_NON_VOLATILE_P(NODE)				\
  ((cp_type_quals (NODE) & (TYPE_QUAL_CONST | TYPE_QUAL_VOLATILE))	\
   == TYPE_QUAL_CONST)

#define FUNCTION_ARG_CHAIN(NODE) \
  TREE_CHAIN (TYPE_ARG_TYPES (TREE_TYPE (NODE)))

/* Given a FUNCTION_DECL, returns the first TREE_LIST out of TYPE_ARG_TYPES
   which refers to a user-written parameter.  */
#define FUNCTION_FIRST_USER_PARMTYPE(NODE) \
  skip_artificial_parms_for ((NODE), TYPE_ARG_TYPES (TREE_TYPE (NODE)))

/* Similarly, but for DECL_ARGUMENTS.  */
#define FUNCTION_FIRST_USER_PARM(NODE) \
  skip_artificial_parms_for ((NODE), DECL_ARGUMENTS (NODE))

#define PROMOTES_TO_AGGR_TYPE(NODE, CODE)	\
  (((CODE) == TREE_CODE (NODE)			\
    && IS_AGGR_TYPE (TREE_TYPE (NODE)))		\
   || IS_AGGR_TYPE (NODE))

/* Nonzero iff TYPE is derived from PARENT. Ignores accessibility and
   ambiguity issues.  */
#define DERIVED_FROM_P(PARENT, TYPE) \
  lookup_base ((TYPE), PARENT, ba_any, NULL)
/* Nonzero iff TYPE is uniquely derived from PARENT. Ignores
   accessibility.  */
#define UNIQUELY_DERIVED_FROM_P(PARENT, TYPE) \
  lookup_base ((TYPE), (PARENT), ba_ignore | ba_quiet, NULL)
/* Nonzero iff TYPE is accessible in the current scope and uniquely
   derived from PARENT.  */
#define ACCESSIBLY_UNIQUELY_DERIVED_P(PARENT, TYPE) \
  lookup_base ((TYPE), (PARENT), ba_check | ba_quiet, NULL)
/* Nonzero iff TYPE is publicly & uniquely derived from PARENT.  */
#define PUBLICLY_UNIQUELY_DERIVED_P(PARENT, TYPE) \
  lookup_base ((TYPE), (PARENT),  ba_not_special | ba_quiet, NULL)

/* This structure provides additional information above and beyond
   what is provide in the ordinary tree_type.  In the past, we used it
   for the types of class types, template parameters types, typename
   types, and so forth.  However, there can be many (tens to hundreds
   of thousands) of template parameter types in a compilation, and
   there's no need for this additional information in that case.
   Therefore, we now use this data structure only for class types.

   In the past, it was thought that there would be relatively few
   class types.  However, in the presence of heavy use of templates,
   many (i.e., thousands) of classes can easily be generated.
   Therefore, we should endeavor to keep the size of this structure to
   a minimum.  */
struct lang_type
{
  unsigned char align;

  unsigned has_type_conversion : 1;
  unsigned has_init_ref : 1;
  unsigned has_default_ctor : 1;
  unsigned uses_multiple_inheritance : 1;
  unsigned const_needs_init : 1;
  unsigned ref_needs_init : 1;
  unsigned has_const_assign_ref : 1;
  unsigned anon_aggr : 1;

  unsigned has_mutable : 1;
  unsigned com_interface : 1;
  unsigned non_pod_class : 1;
  unsigned nearly_empty_p : 1;
  unsigned user_align : 1;
  unsigned has_assign_ref : 1;
  unsigned has_new : 1;
  unsigned has_array_new : 1;

  unsigned gets_delete : 2;
  unsigned has_call_overloaded : 1;
  unsigned has_array_ref_overloaded : 1;
  unsigned has_arrow_overloaded : 1;
  unsigned interface_only : 1;
  unsigned interface_unknown : 1;
  unsigned needs_virtual_reinit : 1;

  unsigned marks: 6;
  unsigned vec_new_uses_cookie : 1;
  unsigned declared_class : 1;

  unsigned being_defined : 1;
  unsigned redefined : 1;
  unsigned debug_requested : 1;
  unsigned use_template : 2;
  unsigned got_semicolon : 1;
  unsigned ptrmemfunc_flag : 1;
  unsigned was_anonymous : 1;

  unsigned has_real_assign_ref : 1;
  unsigned has_const_init_ref : 1;
  unsigned has_complex_init_ref : 1;
  unsigned has_complex_assign_ref : 1;
  unsigned has_abstract_assign_ref : 1;
  unsigned non_aggregate : 1;
  unsigned is_partial_instantiation : 1;
  unsigned java_interface : 1;

  unsigned non_zero_init : 1;
  unsigned contains_empty_class_p : 1;

  /* When adding a flag here, consider whether or not it ought to
     apply to a template instance if it applies to the template.  If
     so, make sure to copy it in instantiate_class_template!  */

  /* There are some bits left to fill out a 32-bit word.  Keep track
     of this by updating the size of this bitfield whenever you add or
     remove a flag.  */
  unsigned dummy : 6;

  int vsize;

  tree primary_base;
  tree vfields;
  tree typeinfo_var;
  tree vbases;
  tree tags;
  tree size;
  tree size_unit;
  tree pure_virtuals;
  tree friend_classes;
  tree rtti;
  tree methods;
  tree template_info;
  tree befriending_classes;
};

/* Indicates whether or not (and how) a template was expanded for this class.
     0=no information yet/non-template class
     1=implicit template instantiation
     2=explicit template specialization
     3=explicit template instantiation  */
#define CLASSTYPE_USE_TEMPLATE(NODE) (TYPE_LANG_SPECIFIC (NODE)->use_template)

/* Fields used for storing information before the class is defined.
   After the class is defined, these fields hold other information.  */

/* List of friends which were defined inline in this class definition.  */
#define CLASSTYPE_INLINE_FRIENDS(NODE) CLASSTYPE_PURE_VIRTUALS (NODE)

/* Nonzero for _CLASSTYPE means that operator delete is defined.  */
#define TYPE_GETS_DELETE(NODE) (TYPE_LANG_SPECIFIC (NODE)->gets_delete)
#define TYPE_GETS_REG_DELETE(NODE) (TYPE_GETS_DELETE (NODE) & 1)

/* Nonzero if `new NODE[x]' should cause the allocation of extra
   storage to indicate how many array elements are in use.  */
#define TYPE_VEC_NEW_USES_COOKIE(NODE)			\
  (CLASS_TYPE_P (NODE)					\
   && TYPE_LANG_SPECIFIC (NODE)->vec_new_uses_cookie)

/* Nonzero means that this _CLASSTYPE node defines ways of converting
   itself to other types.  */
#define TYPE_HAS_CONVERSION(NODE) \
  (TYPE_LANG_SPECIFIC (NODE)->has_type_conversion)

/* Nonzero means that this _CLASSTYPE node overloads operator=(X&).  */
#define TYPE_HAS_ASSIGN_REF(NODE) (TYPE_LANG_SPECIFIC (NODE)->has_assign_ref)
#define TYPE_HAS_CONST_ASSIGN_REF(NODE) \
  (TYPE_LANG_SPECIFIC (NODE)->has_const_assign_ref)

/* Nonzero means that this _CLASSTYPE node has an X(X&) constructor.  */
#define TYPE_HAS_INIT_REF(NODE) (TYPE_LANG_SPECIFIC (NODE)->has_init_ref)
#define TYPE_HAS_CONST_INIT_REF(NODE) \
  (TYPE_LANG_SPECIFIC (NODE)->has_const_init_ref)

/* Nonzero if this class defines an overloaded operator new.  (An
   operator new [] doesn't count.)  */
#define TYPE_HAS_NEW_OPERATOR(NODE) \
  (TYPE_LANG_SPECIFIC (NODE)->has_new)

/* Nonzero if this class defines an overloaded operator new[].  */
#define TYPE_HAS_ARRAY_NEW_OPERATOR(NODE) \
  (TYPE_LANG_SPECIFIC (NODE)->has_array_new)

/* Nonzero means that this type is being defined.  I.e., the left brace
   starting the definition of this type has been seen.  */
#define TYPE_BEING_DEFINED(NODE) (TYPE_LANG_SPECIFIC (NODE)->being_defined)
/* Nonzero means that this type has been redefined.  In this case, if
   convenient, don't reprocess any methods that appear in its redefinition.  */
#define TYPE_REDEFINED(NODE) (TYPE_LANG_SPECIFIC (NODE)->redefined)

/* The is the basetype that contains NODE's rtti.  */
#define CLASSTYPE_RTTI(NODE) (TYPE_LANG_SPECIFIC (NODE)->rtti)

/* Nonzero means that this _CLASSTYPE node overloads operator().  */
#define TYPE_OVERLOADS_CALL_EXPR(NODE) \
  (TYPE_LANG_SPECIFIC (NODE)->has_call_overloaded)

/* Nonzero means that this _CLASSTYPE node overloads operator[].  */
#define TYPE_OVERLOADS_ARRAY_REF(NODE) \
  (TYPE_LANG_SPECIFIC (NODE)->has_array_ref_overloaded)

/* Nonzero means that this _CLASSTYPE node overloads operator->.  */
#define TYPE_OVERLOADS_ARROW(NODE) \
  (TYPE_LANG_SPECIFIC (NODE)->has_arrow_overloaded)

/* Nonzero means that this _CLASSTYPE (or one of its ancestors) uses
   multiple inheritance.  If this is 0 for the root of a type
   hierarchy, then we can use more efficient search techniques.  */
#define TYPE_USES_MULTIPLE_INHERITANCE(NODE) \
  (TYPE_LANG_SPECIFIC (NODE)->uses_multiple_inheritance)

/* Nonzero means that this _CLASSTYPE (or one of its ancestors) uses
   virtual base classes.  If this is 0 for the root of a type
   hierarchy, then we can use more efficient search techniques.  */
#define TYPE_USES_VIRTUAL_BASECLASSES(NODE) (TREE_LANG_FLAG_3 (NODE))

/* Vector member functions defined in this class.  Each element is
   either a FUNCTION_DECL, a TEMPLATE_DECL, or an OVERLOAD.  All
   functions with the same name end up in the same slot.  The first
   two elements are for constructors, and destructors, respectively.
   Any conversion operators are next, followed by ordinary member
   functions.  There may be empty entries at the end of the vector.  */
#define CLASSTYPE_METHOD_VEC(NODE) (TYPE_LANG_SPECIFIC (NODE)->methods)

/* The slot in the CLASSTYPE_METHOD_VEC where constructors go.  */
#define CLASSTYPE_CONSTRUCTOR_SLOT 0

/* The slot in the CLASSTYPE_METHOD_VEC where destructors go.  */
#define CLASSTYPE_DESTRUCTOR_SLOT 1

/* The first slot in the CLASSTYPE_METHOD_VEC where conversion
   operators can appear.  */
#define CLASSTYPE_FIRST_CONVERSION_SLOT 2

/* A FUNCTION_DECL or OVERLOAD for the constructors for NODE.  These
   are the constructors that take an in-charge parameter.  */
#define CLASSTYPE_CONSTRUCTORS(NODE) \
  (TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (NODE), CLASSTYPE_CONSTRUCTOR_SLOT))

/* A FUNCTION_DECL for the destructor for NODE.  These are the
   destructors that take an in-charge parameter.  */
#define CLASSTYPE_DESTRUCTORS(NODE) \
  (TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (NODE), CLASSTYPE_DESTRUCTOR_SLOT))

/* Mark bits for depth-first and breath-first searches.  */

/* Get the value of the Nth mark bit.  */
#define CLASSTYPE_MARKED_N(NODE, N)				\
  (((CLASS_TYPE_P (NODE) ? TYPE_LANG_SPECIFIC (NODE)->marks	\
     : ((unsigned) TYPE_ALIAS_SET (NODE))) & (1 << (N))) != 0)

/* Set the Nth mark bit.  */
#define SET_CLASSTYPE_MARKED_N(NODE, N)				\
  (CLASS_TYPE_P (NODE)						\
   ? (void) (TYPE_LANG_SPECIFIC (NODE)->marks |= (1 << (N)))	\
   : (void) (TYPE_ALIAS_SET (NODE) |= (1 << (N))))

/* Clear the Nth mark bit.  */
#define CLEAR_CLASSTYPE_MARKED_N(NODE, N)			\
  (CLASS_TYPE_P (NODE)						\
   ? (void) (TYPE_LANG_SPECIFIC (NODE)->marks &= ~(1 << (N)))	\
   : (void) (TYPE_ALIAS_SET (NODE) &= ~(1 << (N))))

/* Get the value of the mark bits.  */
#define CLASSTYPE_MARKED(NODE) CLASSTYPE_MARKED_N (NODE, 0)
#define CLASSTYPE_MARKED2(NODE) CLASSTYPE_MARKED_N (NODE, 1)
#define CLASSTYPE_MARKED3(NODE) CLASSTYPE_MARKED_N (NODE, 2)
#define CLASSTYPE_MARKED4(NODE) CLASSTYPE_MARKED_N (NODE, 3)
#define CLASSTYPE_MARKED5(NODE) CLASSTYPE_MARKED_N (NODE, 4)
#define CLASSTYPE_MARKED6(NODE) CLASSTYPE_MARKED_N (NODE, 5)

/* Macros to modify the above flags */
#define SET_CLASSTYPE_MARKED(NODE)    SET_CLASSTYPE_MARKED_N (NODE, 0)
#define CLEAR_CLASSTYPE_MARKED(NODE)  CLEAR_CLASSTYPE_MARKED_N (NODE, 0)
#define SET_CLASSTYPE_MARKED2(NODE)   SET_CLASSTYPE_MARKED_N (NODE, 1)
#define CLEAR_CLASSTYPE_MARKED2(NODE) CLEAR_CLASSTYPE_MARKED_N (NODE, 1)
#define SET_CLASSTYPE_MARKED3(NODE)   SET_CLASSTYPE_MARKED_N (NODE, 2)
#define CLEAR_CLASSTYPE_MARKED3(NODE) CLEAR_CLASSTYPE_MARKED_N (NODE, 2)
#define SET_CLASSTYPE_MARKED4(NODE)   SET_CLASSTYPE_MARKED_N (NODE, 3)
#define CLEAR_CLASSTYPE_MARKED4(NODE) CLEAR_CLASSTYPE_MARKED_N (NODE, 3)
#define SET_CLASSTYPE_MARKED5(NODE)   SET_CLASSTYPE_MARKED_N (NODE, 4)
#define CLEAR_CLASSTYPE_MARKED5(NODE) CLEAR_CLASSTYPE_MARKED_N (NODE, 4)
#define SET_CLASSTYPE_MARKED6(NODE)   SET_CLASSTYPE_MARKED_N (NODE, 5)
#define CLEAR_CLASSTYPE_MARKED6(NODE) CLEAR_CLASSTYPE_MARKED_N (NODE, 5)

/* A list of the nested tag-types (class, struct, union, or enum)
   found within this class.  The TREE_PURPOSE of each node is the name
   of the type; the TREE_VALUE is the type itself.  This list includes
   nested member class templates.  */
#define CLASSTYPE_TAGS(NODE)		(TYPE_LANG_SPECIFIC (NODE)->tags)

/* Nonzero if NODE has a primary base class, i.e., a base class with
   which it shares the virtual function table pointer.  */
#define CLASSTYPE_HAS_PRIMARY_BASE_P(NODE) \
  (CLASSTYPE_PRIMARY_BINFO (NODE) != NULL_TREE)

/* If non-NULL, this is the binfo for the primary base class, i.e.,
   the base class which contains the virtual function table pointer
   for this class.  */
#define CLASSTYPE_PRIMARY_BINFO(NODE) \
  (TYPE_LANG_SPECIFIC (NODE)->primary_base)

/* The number of virtual functions present in this class' virtual
   function table.  */
#define CLASSTYPE_VSIZE(NODE) (TYPE_LANG_SPECIFIC (NODE)->vsize)

/* A chain of BINFOs for the direct and indirect virtual base classes
   that this type uses in a post-order depth-first left-to-right
   order.  (In other words, these bases appear in the order that they
   should be initialized.)  If a virtual base is primary, then the
   primary copy will appear on this list.  Thus, the BINFOs on this
   list are all "real"; they are the same BINFOs that will be
   encountered when using dfs_unmarked_real_bases_queue_p and related
   functions.  */
#define CLASSTYPE_VBASECLASSES(NODE) (TYPE_LANG_SPECIFIC (NODE)->vbases)

/* For a non-virtual BINFO, the BINFO itself; for a virtual BINFO, the
   binfo_for_vbase.  C is the most derived class for the hierarchy
   containing BINFO.  */
#define CANONICAL_BINFO(BINFO, C)		\
  (TREE_VIA_VIRTUAL (BINFO)			\
   ? binfo_for_vbase (BINFO_TYPE (BINFO), C)	\
   : (BINFO))

/* Number of direct baseclasses of NODE.  */
#define CLASSTYPE_N_BASECLASSES(NODE) \
  (BINFO_N_BASETYPES (TYPE_BINFO (NODE)))

/* These are the size and alignment of the type without its virtual
   base classes, for when we use this type as a base itself.  */
#define CLASSTYPE_SIZE(NODE) (TYPE_LANG_SPECIFIC (NODE)->size)
#define CLASSTYPE_SIZE_UNIT(NODE) (TYPE_LANG_SPECIFIC (NODE)->size_unit)
#define CLASSTYPE_ALIGN(NODE) (TYPE_LANG_SPECIFIC (NODE)->align)
#define CLASSTYPE_USER_ALIGN(NODE) (TYPE_LANG_SPECIFIC (NODE)->user_align)

/* The alignment of NODE, without its virtual bases, in bytes.  */
#define CLASSTYPE_ALIGN_UNIT(NODE) \
  (CLASSTYPE_ALIGN (NODE) / BITS_PER_UNIT)

/* True if this a Java interface type, declared with 
   '__attribute__ ((java_interface))'. */
#define TYPE_JAVA_INTERFACE(NODE) (TYPE_LANG_SPECIFIC (NODE)->java_interface)

/* A cons list of virtual functions which cannot be inherited by
   derived classes.  When deriving from this type, the derived
   class must provide its own definition for each of these functions.  */
#define CLASSTYPE_PURE_VIRTUALS(NODE) (TYPE_LANG_SPECIFIC (NODE)->pure_virtuals)

/* Nonzero means that this aggr type has been `closed' by a semicolon.  */
#define CLASSTYPE_GOT_SEMICOLON(NODE) (TYPE_LANG_SPECIFIC (NODE)->got_semicolon)

/* Nonzero means that the main virtual function table pointer needs to be
   set because base constructors have placed the wrong value there.
   If this is zero, it means that they placed the right value there,
   and there is no need to change it.  */
#define CLASSTYPE_NEEDS_VIRTUAL_REINIT(NODE) \
  (TYPE_LANG_SPECIFIC (NODE)->needs_virtual_reinit)

/* Nonzero means that this type has an X() constructor.  */
#define TYPE_HAS_DEFAULT_CONSTRUCTOR(NODE) \
  (TYPE_LANG_SPECIFIC (NODE)->has_default_ctor)

/* Nonzero means that this type contains a mutable member */
#define CLASSTYPE_HAS_MUTABLE(NODE) (TYPE_LANG_SPECIFIC (NODE)->has_mutable)
#define TYPE_HAS_MUTABLE_P(NODE) (cp_has_mutable_p (NODE))

/* Nonzero means that this class type is a non-POD class.  */
#define CLASSTYPE_NON_POD_P(NODE) (TYPE_LANG_SPECIFIC (NODE)->non_pod_class)

/* Nonzero means that this class contains pod types whose default
   initialization is not a zero initialization (namely, pointers to
   data members).  */
#define CLASSTYPE_NON_ZERO_INIT_P(NODE) (TYPE_LANG_SPECIFIC (NODE)->non_zero_init)

/* Nonzero if this class is "nearly empty", i.e., contains only a
   virtual function table pointer.  */
#define CLASSTYPE_NEARLY_EMPTY_P(NODE) \
  (TYPE_LANG_SPECIFIC (NODE)->nearly_empty_p)

/* Nonzero if this class contains an empty subobject.  */
#define CLASSTYPE_CONTAINS_EMPTY_CLASS_P(NODE) \
  (TYPE_LANG_SPECIFIC (NODE)->contains_empty_class_p)

/* A list of class types of which this type is a friend.  The
   TREE_VALUE is normally a TYPE, but will be a TEMPLATE_DECL in the
   case of a template friend.  */
#define CLASSTYPE_FRIEND_CLASSES(NODE) \
  (TYPE_LANG_SPECIFIC (NODE)->friend_classes)

/* A list of the classes which grant friendship to this class.  */
#define CLASSTYPE_BEFRIENDING_CLASSES(NODE) \
  (TYPE_LANG_SPECIFIC (NODE)->befriending_classes)

/* Say whether this node was declared as a "class" or a "struct".  */
#define CLASSTYPE_DECLARED_CLASS(NODE) \
  (TYPE_LANG_SPECIFIC (NODE)->declared_class)

/* Nonzero if this class has const members
   which have no specified initialization.  */
#define CLASSTYPE_READONLY_FIELDS_NEED_INIT(NODE)	\
  (TYPE_LANG_SPECIFIC (NODE)				\
   ? TYPE_LANG_SPECIFIC (NODE)->const_needs_init : 0)
#define SET_CLASSTYPE_READONLY_FIELDS_NEED_INIT(NODE, VALUE) \
  (TYPE_LANG_SPECIFIC (NODE)->const_needs_init = (VALUE))

/* Nonzero if this class has ref members
   which have no specified initialization.  */
#define CLASSTYPE_REF_FIELDS_NEED_INIT(NODE)		\
  (TYPE_LANG_SPECIFIC (NODE)				\
   ? TYPE_LANG_SPECIFIC (NODE)->ref_needs_init : 0)
#define SET_CLASSTYPE_REF_FIELDS_NEED_INIT(NODE, VALUE) \
  (TYPE_LANG_SPECIFIC (NODE)->ref_needs_init = (VALUE))

/* Nonzero if this class is included from a header file which employs
   `#pragma interface', and it is not included in its implementation file.  */
#define CLASSTYPE_INTERFACE_ONLY(NODE) \
  (TYPE_LANG_SPECIFIC (NODE)->interface_only)

/* True if we have already determined whether or not vtables, VTTs,
   typeinfo, and other similar per-class data should be emitted in
   this translation unit.  This flag does not indicate whether or not
   these items should be emitted; it only indicates that we know one
   way or the other.  */
#define CLASSTYPE_INTERFACE_KNOWN(NODE) \
  (TYPE_LANG_SPECIFIC (NODE)->interface_unknown == 0)
/* The opposite of CLASSTYPE_INTERFANCE_KNOWN.  */
#define CLASSTYPE_INTERFACE_UNKNOWN(NODE) \
  (TYPE_LANG_SPECIFIC (NODE)->interface_unknown)

#define SET_CLASSTYPE_INTERFACE_UNKNOWN_X(NODE,X) \
  (TYPE_LANG_SPECIFIC (NODE)->interface_unknown = !!(X))
#define SET_CLASSTYPE_INTERFACE_UNKNOWN(NODE) \
  (TYPE_LANG_SPECIFIC (NODE)->interface_unknown = 1)
#define SET_CLASSTYPE_INTERFACE_KNOWN(NODE) \
  (TYPE_LANG_SPECIFIC (NODE)->interface_unknown = 0)

/* Nonzero if a _DECL node requires us to output debug info for this class.  */
#define CLASSTYPE_DEBUG_REQUESTED(NODE) \
  (TYPE_LANG_SPECIFIC (NODE)->debug_requested)

/* Additional macros for inheritance information.  */

/* The BINFO_INHERITANCE_CHAIN is used opposite to the description in
   gcc/tree.h.  In particular if D is derived from B then the BINFO
   for B (in D) will have a BINFO_INHERITANCE_CHAIN pointing to
   D.  In tree.h, this pointer is described as pointing in other
   direction.  There is a different BINFO for each path to a virtual
   base; BINFOs for virtual bases are not shared.

   We use TREE_VIA_PROTECTED and TREE_VIA_PUBLIC, but private
   inheritance is indicated by the absence of the other two flags, not
   by TREE_VIA_PRIVATE, which is unused.  */

/* Mark the binfo, whether shared or not. Each instance of a virtual
   base can be separately marked.  */
#define BINFO_UNSHARED_MARKED(NODE) TREE_LANG_FLAG_0 (NODE)

/* Nonzero means marked by DFS or BFS search.  */
#define BINFO_MARKED(NODE)			\
  (TREE_VIA_VIRTUAL (NODE)			\
   ? CLASSTYPE_MARKED (BINFO_TYPE (NODE))	\
   : TREE_LANG_FLAG_0 (NODE))
/* Macros needed because of C compilers that don't allow conditional
   expressions to be lvalues.  Grr!  */
#define SET_BINFO_MARKED(NODE)			\
  (TREE_VIA_VIRTUAL(NODE)			\
   ? SET_CLASSTYPE_MARKED (BINFO_TYPE (NODE))	\
   : (void)(TREE_LANG_FLAG_0 (NODE) = 1))
#define CLEAR_BINFO_MARKED(NODE)		\
  (TREE_VIA_VIRTUAL (NODE)			\
   ? CLEAR_CLASSTYPE_MARKED (BINFO_TYPE (NODE))	\
   : (void)(TREE_LANG_FLAG_0 (NODE) = 0))

/* Nonzero means that this class is on a path leading to a new vtable.  */
#define BINFO_VTABLE_PATH_MARKED(NODE)		\
  (TREE_VIA_VIRTUAL (NODE)			\
   ? CLASSTYPE_MARKED3 (BINFO_TYPE (NODE))	\
   : TREE_LANG_FLAG_3 (NODE))
#define SET_BINFO_VTABLE_PATH_MARKED(NODE)	\
  (TREE_VIA_VIRTUAL(NODE)			\
   ? SET_CLASSTYPE_MARKED3 (BINFO_TYPE (NODE))	\
   : (TREE_LANG_FLAG_3 (NODE) = 1))
#define CLEAR_BINFO_VTABLE_PATH_MARKED(NODE)	\
  (TREE_VIA_VIRTUAL (NODE)			\
   ? CLEAR_CLASSTYPE_MARKED3 (BINFO_TYPE (NODE))\
   : (TREE_LANG_FLAG_3 (NODE) = 0))

/* Nonzero means B (a BINFO) has its own vtable.  Under the old ABI,
   secondary vtables are sometimes shared.  Any copies will not have
   this flag set.

   B is part of the hierarchy dominated by C.  */
#define BINFO_NEW_VTABLE_MARKED(B, C) \
  (TREE_LANG_FLAG_4 (CANONICAL_BINFO (B, C)))

/* Any subobject that needs a new vtable must have a vptr and must not
   be a non-virtual primary base (since it would then use the vtable from a
   derived class and never become non-primary.)  */
#define SET_BINFO_NEW_VTABLE_MARKED(B, C)				 \
  (BINFO_NEW_VTABLE_MARKED (B, C) = 1,					 \
   my_friendly_assert (!BINFO_PRIMARY_P (B)				 \
		       || TREE_VIA_VIRTUAL (B), 20000517),		 \
   my_friendly_assert (CLASSTYPE_VFIELDS (BINFO_TYPE (B)) != NULL_TREE,  \
		       20000517))

/* Nonzero means this class has done dfs_pushdecls.  */
#define BINFO_PUSHDECLS_MARKED(NODE) BINFO_VTABLE_PATH_MARKED (NODE)
#define SET_BINFO_PUSHDECLS_MARKED(NODE) SET_BINFO_VTABLE_PATH_MARKED (NODE)
#define CLEAR_BINFO_PUSHDECLS_MARKED(NODE) CLEAR_BINFO_VTABLE_PATH_MARKED (NODE)

/* Nonzero if this BINFO is a primary base class.  Note, this can be
   set for non-canononical virtual bases. For a virtual primary base
   you might also need to check whether it is canonical.  */

#define BINFO_PRIMARY_P(NODE) \
  (BINFO_PRIMARY_BASE_OF (NODE) != NULL_TREE)

/* The index in the VTT where this subobject's sub-VTT can be found.
   NULL_TREE if there is no sub-VTT.  */
#define BINFO_SUBVTT_INDEX(NODE) TREE_VEC_ELT (NODE, 8)

/* The index in the VTT where the vptr for this subobject can be
   found.  NULL_TREE if there is no secondary vptr in the VTT.  */
#define BINFO_VPTR_INDEX(NODE) TREE_VEC_ELT (NODE, 9)

/* The binfo of which NODE is a primary base.  (This is different from
   BINFO_INHERITANCE_CHAIN for virtual base because a virtual base is
   sometimes a primary base for a class for which it is not an
   immediate base.)  */
#define BINFO_PRIMARY_BASE_OF(NODE) TREE_VEC_ELT (NODE, 10)

/* Nonzero if this binfo has lost its primary base binfo (because that
   is a nearly-empty virtual base that has been taken by some other
   base in the complete hierarchy.  */
#define BINFO_LOST_PRIMARY_P(NODE) TREE_LANG_FLAG_2 (NODE)

/* Nonzero if this binfo is an indirect primary base, i.e. a virtual
   base that is a primary base of some of other class in the
   hierarchy.  */
#define BINFO_INDIRECT_PRIMARY_P(NODE) TREE_USED (NODE)

/* Used by various search routines.  */
#define IDENTIFIER_MARKED(NODE) TREE_LANG_FLAG_0 (NODE)

/* The std::type_info variable representing this class, or NULL if no
   such variable has been created.  This field is only set for the
   TYPE_MAIN_VARIANT of the class.  */
#define CLASSTYPE_TYPEINFO_VAR(NODE) \
  (TYPE_LANG_SPECIFIC (NODE)->typeinfo_var)

/* Accessor macros for the vfield slots in structures.  */

/* The virtual function pointer fields that this type contains.  For a
   vfield defined just for this class, or from a primary base, the
   TREE_PURPOSE is NULL.  Otherwise, the TREE_PURPOSE is the BINFO for
   the class containing the vfield.  The TREE_VALUE is the class where
   the vfield was first defined.  */
#define CLASSTYPE_VFIELDS(NODE) (TYPE_LANG_SPECIFIC (NODE)->vfields)

/* Get the assoc info that caused this vfield to exist.  */
#define VF_BINFO_VALUE(NODE) TREE_PURPOSE (NODE)

/* Get that same information as a _TYPE.  */
#define VF_BASETYPE_VALUE(NODE) TREE_VALUE (NODE)

/* Get the value of the top-most type dominating the non-`normal' vfields.  */
#define VF_DERIVED_VALUE(NODE) \
  (VF_BINFO_VALUE (NODE) ? BINFO_TYPE (VF_BINFO_VALUE (NODE)) : NULL_TREE)

/* The number of bytes by which to adjust the `this' pointer when
   calling this virtual function.  */
#define BV_DELTA(NODE) (TREE_PURPOSE (NODE))

/* If non-NULL, the vtable index at which to find the vcall offset
   when calling this virtual function.  */
#define BV_VCALL_INDEX(NODE) (TREE_TYPE (NODE))

/* The function to call.  */
#define BV_FN(NODE) (TREE_VALUE (NODE))

/* Nonzero if we should use a virtual thunk for this entry.  */
#define BV_USE_VCALL_INDEX_P(NODE) \
   (TREE_LANG_FLAG_0 (NODE))

/* Nonzero for TREE_LIST node means that this list of things
   is a list of parameters, as opposed to a list of expressions.  */
#define TREE_PARMLIST(NODE) (TREE_LANG_FLAG_2 (NODE))

/* Nonzero for a parmlist means that this parmlist ended in ...  */
#define PARMLIST_ELLIPSIS_P(NODE) TREE_LANG_FLAG_0 (NODE)

/* For FUNCTION_TYPE or METHOD_TYPE, a list of the exceptions that
   this type can raise.  Each TREE_VALUE is a _TYPE.  The TREE_VALUE
   will be NULL_TREE to indicate a throw specification of `()', or
   no exceptions allowed.  */
#define TYPE_RAISES_EXCEPTIONS(NODE) TYPE_BINFO (NODE)

/* For FUNCTION_TYPE or METHOD_TYPE, return 1 iff it is declared `throw()'.  */
#define TYPE_NOTHROW_P(NODE) \
  (TYPE_RAISES_EXCEPTIONS (NODE) \
   && TREE_VALUE (TYPE_RAISES_EXCEPTIONS (NODE)) == NULL_TREE)

/* The binding level associated with the namespace.  */
#define NAMESPACE_LEVEL(NODE) \
  (DECL_LANG_SPECIFIC (NODE)->decl_flags.u.level)


/* If a DECL has DECL_LANG_SPECIFIC, it is either a lang_decl_flags or
   a lang_decl (which has lang_decl_flags as its initial prefix).
   This macro is nonzero for tree nodes whose DECL_LANG_SPECIFIC is
   the full lang_decl, and not just lang_decl_flags.  */
#define CAN_HAVE_FULL_LANG_DECL_P(NODE)		\
  (!(TREE_CODE (NODE) == VAR_DECL		\
     || TREE_CODE (NODE) == CONST_DECL		\
     || TREE_CODE (NODE) == FIELD_DECL		\
     || TREE_CODE (NODE) == USING_DECL))

struct lang_decl_flags
{
  struct c_lang_decl base;

  ENUM_BITFIELD(languages) language : 8;

  unsigned operator_attr : 1;
  unsigned constructor_attr : 1;
  unsigned destructor_attr : 1;
  unsigned friend_attr : 1;
  unsigned static_function : 1;
  unsigned pure_virtual : 1;
  unsigned has_in_charge_parm_p : 1;
  unsigned has_vtt_parm_p : 1;

  unsigned deferred : 1;
  unsigned use_template : 2;
  unsigned nonconverting : 1;
  unsigned not_really_extern : 1;
  unsigned needs_final_overrider : 1;
  unsigned initialized_in_class : 1;
  unsigned pending_inline_p : 1;

  unsigned global_ctor_p : 1;
  unsigned global_dtor_p : 1;
  unsigned assignment_operator_p : 1;
  unsigned anticipated_p : 1;
  /* Four unused bits.  */

  union {
    /* In a FUNCTION_DECL, VAR_DECL, TYPE_DECL, or TEMPLATE_DECL, this
       is DECL_TEMPLATE_INFO.  */
    tree template_info;

    /* In a NAMESPACE_DECL, this is NAMESPACE_LEVEL.  */
    struct binding_level *level;
  } u;

  union {
    /* This is DECL_ACCESS.  */
    tree access;

    /* For VAR_DECL in function, this is DECL_DISCRIMINATOR.  */
    int discriminator;

    /* In a namespace-scope FUNCTION_DECL, this is
       GLOBAL_INIT_PRIORITY.  */
    int init_priority;

    /* In a FUNCTION_DECL for which DECL_THUNK_P holds, this is
       THUNK_VCALL_OFFSET.  */
    tree vcall_offset;
  } u2;
};

struct lang_decl
{
  struct lang_decl_flags decl_flags;

  tree befriending_classes;

  /* For a virtual FUNCTION_DECL, this is DECL_VIRTUAL_CONTEXT.  For a
     non-virtual FUNCTION_DECL, this is DECL_FRIEND_CONTEXT.  */
  tree context;

  /* In a FUNCTION_DECL, this is DECL_CLONED_FUNCTION.  */
  tree cloned_function;

  union
  {
    tree sorted_fields;
    struct unparsed_text *pending_inline_info;
    struct cp_language_function *saved_language_function;
  } u;

  union {
    /* In an overloaded operator, this is the value of
       DECL_OVERLOADED_OPERATOR_P.  */
    enum tree_code operator_code;
  } u2;
};

#define DEFARG_POINTER(NODE) (DEFAULT_ARG_CHECK (NODE)->identifier.id.str)

/* DECL_NEEDED_P holds of a declaration when we need to emit its
   definition.  This is true when the back-end tells us that
   the symbol has been referenced in the generated code.  If, however,
   we are not generating code, then it is also true when a symbol has
   just been used somewhere, even if it's not really needed.  We need
   anything that isn't comdat, but we don't know for sure whether or
   not something is comdat until end-of-file.  */
#define DECL_NEEDED_P(DECL)					\
  ((at_eof && TREE_PUBLIC (DECL) && !DECL_COMDAT (DECL))	\
   || (DECL_ASSEMBLER_NAME_SET_P (DECL)				\
       && TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (DECL)))	\
   || (flag_syntax_only && TREE_USED (DECL)))

/* Non-zero iff DECL is memory-based.  The DECL_RTL of
   certain const variables might be a CONST_INT, or a REG
   in some cases.  We cannot use `memory_operand' as a test
   here because on most RISC machines, a variable's address
   is not, by itself, a legitimate address.  */
#define DECL_IN_MEMORY_P(NODE) \
  (DECL_RTL_SET_P (NODE) && GET_CODE (DECL_RTL (NODE)) == MEM)

/* For a FUNCTION_DECL or a VAR_DECL, the language linkage for the
   declaration.  Some entities (like a member function in a local
   class, or a local variable) do not have linkage at all, and this
   macro should not be used in those cases.
   
   Implementation note: A FUNCTION_DECL without DECL_LANG_SPECIFIC was
   created by language-independent code, and has C linkage.  Most
   VAR_DECLs have C++ linkage, and do not have DECL_LANG_SPECIFIC, but
   we do create DECL_LANG_SPECIFIC for variables with non-C++ linkage.  */
#define DECL_LANGUAGE(NODE) 				\
  (DECL_LANG_SPECIFIC (NODE) 				\
   ? DECL_LANG_SPECIFIC (NODE)->decl_flags.language	\
   : (TREE_CODE (NODE) == FUNCTION_DECL			\
      ? lang_c : lang_cplusplus))

/* Set the language linkage for NODE to LANGUAGE.  */
#define SET_DECL_LANGUAGE(NODE, LANGUAGE) \
  (DECL_LANG_SPECIFIC (NODE)->decl_flags.language = (LANGUAGE))

/* For FUNCTION_DECLs: nonzero means that this function is a constructor.  */
#define DECL_CONSTRUCTOR_P(NODE) \
  (DECL_LANG_SPECIFIC (NODE)->decl_flags.constructor_attr)

/* Nonzero if NODE (a FUNCTION_DECL) is a constructor for a complete
   object.  */
#define DECL_COMPLETE_CONSTRUCTOR_P(NODE)		\
  (DECL_CONSTRUCTOR_P (NODE)				\
   && DECL_NAME (NODE) == complete_ctor_identifier)

/* Nonzero if NODE (a FUNCTION_DECL) is a constructor for a base
   object.  */
#define DECL_BASE_CONSTRUCTOR_P(NODE)		\
  (DECL_CONSTRUCTOR_P (NODE)			\
   && DECL_NAME (NODE) == base_ctor_identifier)

/* Nonzero if NODE (a FUNCTION_DECL) is a constructor, but not either the
   specialized in-charge constructor or the specialized not-in-charge
   constructor.  */
#define DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P(NODE)		\
  (DECL_CONSTRUCTOR_P (NODE) && !DECL_CLONED_FUNCTION_P (NODE))

/* Nonzero if NODE (a FUNCTION_DECL) is a copy constructor.  */
#define DECL_COPY_CONSTRUCTOR_P(NODE) \
  (DECL_CONSTRUCTOR_P (NODE) && copy_fn_p (NODE) > 0)

/* Nonzero if NODE is a destructor.  */
#define DECL_DESTRUCTOR_P(NODE)				\
  (DECL_LANG_SPECIFIC (NODE)->decl_flags.destructor_attr)

/* Nonzero if NODE (a FUNCTION_DECL) is a destructor, but not the
   specialized in-charge constructor, in-charge deleting constructor,
   or the the base destructor.  */
#define DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P(NODE)			\
  (DECL_DESTRUCTOR_P (NODE) && !DECL_CLONED_FUNCTION_P (NODE))

/* Nonzero if NODE (a FUNCTION_DECL) is a destructor for a complete
   object.  */
#define DECL_COMPLETE_DESTRUCTOR_P(NODE)		\
  (DECL_DESTRUCTOR_P (NODE)				\
   && DECL_NAME (NODE) == complete_dtor_identifier)

/* Nonzero if NODE (a FUNCTION_DECL) is a destructor for a base
   object.  */
#define DECL_BASE_DESTRUCTOR_P(NODE)		\
  (DECL_DESTRUCTOR_P (NODE)			\
   && DECL_NAME (NODE) == base_dtor_identifier)

/* Nonzero if NODE (a FUNCTION_DECL) is a destructor for a complete
   object that deletes the object after it has been destroyed.  */
#define DECL_DELETING_DESTRUCTOR_P(NODE)		\
  (DECL_DESTRUCTOR_P (NODE)				\
   && DECL_NAME (NODE) == deleting_dtor_identifier)

/* Nonzero if NODE (a FUNCTION_DECL) is a cloned constructor or
   destructor.  */
#define DECL_CLONED_FUNCTION_P(NODE)		\
  ((TREE_CODE (NODE) == FUNCTION_DECL 		\
    || TREE_CODE (NODE) == TEMPLATE_DECL)	\
   && DECL_LANG_SPECIFIC (NODE)			\
   && DECL_CLONED_FUNCTION (NODE) != NULL_TREE)

/* If DECL_CLONED_FUNCTION_P holds, this is the function that was
   cloned.  */
#define DECL_CLONED_FUNCTION(NODE) \
  (DECL_LANG_SPECIFIC (NODE)->cloned_function)

/* Nonzero if NODE has DECL_DISCRIMINATOR and not DECL_ACCESS.  */
#define DECL_DISCRIMINATOR_P(NODE)	\
  (TREE_CODE (NODE) == VAR_DECL		\
   && DECL_FUNCTION_SCOPE_P (NODE))

/* Discriminator for name mangling.  */
#define DECL_DISCRIMINATOR(NODE) \
  (DECL_LANG_SPECIFIC (NODE)->decl_flags.u2.discriminator)

/* Non-zero if the VTT parm has been added to NODE.  */
#define DECL_HAS_VTT_PARM_P(NODE) \
  (DECL_LANG_SPECIFIC (NODE)->decl_flags.has_vtt_parm_p)

/* Non-zero if NODE is a FUNCTION_DECL for which a VTT parameter is
   required.  */
#define DECL_NEEDS_VTT_PARM_P(NODE)			\
  (TYPE_USES_VIRTUAL_BASECLASSES (DECL_CONTEXT (NODE))	\
   && (DECL_BASE_CONSTRUCTOR_P (NODE)			\
       || DECL_BASE_DESTRUCTOR_P (NODE)))

/* Non-zero if NODE is a user-defined conversion operator.  */
#define DECL_CONV_FN_P(NODE) \
  (IDENTIFIER_TYPENAME_P (DECL_NAME (NODE)))

/* Set the overloaded operator code for NODE to CODE.  */
#define SET_OVERLOADED_OPERATOR_CODE(NODE, CODE) \
  (DECL_LANG_SPECIFIC (NODE)->u2.operator_code = (CODE))

/* If NODE is an overloaded operator, then this returns the TREE_CODE
   associcated with the overloaded operator.
   DECL_ASSIGNMENT_OPERATOR_P must also be checked to determine
   whether or not NODE is an assignment operator.  If NODE is not an
   overloaded operator, ERROR_MARK is returned.  Since the numerical
   value of ERROR_MARK is zero, this macro can be used as a predicate
   to test whether or not NODE is an overloaded operator.  */
#define DECL_OVERLOADED_OPERATOR_P(NODE)		\
  (IDENTIFIER_OPNAME_P (DECL_NAME (NODE))		\
   ? DECL_LANG_SPECIFIC (NODE)->u2.operator_code : ERROR_MARK)

/* Non-zero if NODE is an assignment operator.  */
#define DECL_ASSIGNMENT_OPERATOR_P(NODE) \
  (DECL_LANG_SPECIFIC (NODE)->decl_flags.assignment_operator_p)

/* For FUNCTION_DECLs: nonzero means that this function is a
   constructor or a destructor with an extra in-charge parameter to
   control whether or not virtual bases are constructed.  */
#define DECL_HAS_IN_CHARGE_PARM_P(NODE) \
  (DECL_LANG_SPECIFIC (NODE)->decl_flags.has_in_charge_parm_p)

/* Nonzero if NODE is an overloaded `operator delete[]' function.  */
#define DECL_ARRAY_DELETE_OPERATOR_P(NODE) \
  (DECL_OVERLOADED_OPERATOR_P (NODE) == VEC_DELETE_EXPR)

/* Nonzero for _DECL means that this decl appears in (or will appear
   in) as a member in a RECORD_TYPE or UNION_TYPE node.  It is also for
   detecting circularity in case members are multiply defined.  In the
   case of a VAR_DECL, it is also used to determine how program storage
   should be allocated.  */
#define DECL_IN_AGGR_P(NODE) (DECL_LANG_FLAG_3 (NODE))

/* Nonzero if the DECL was initialized in the class definition itself,
   rather than outside the class.  */
#define DECL_INITIALIZED_IN_CLASS_P(DECL) \
 (DECL_LANG_SPECIFIC (DECL)->decl_flags.initialized_in_class)

/* Nonzero for FUNCTION_DECL means that this decl is just a
   friend declaration, and should not be added to the list of
   member functions for this class.  */
#define DECL_FRIEND_P(NODE) (DECL_LANG_SPECIFIC (NODE)->decl_flags.friend_attr)

/* A TREE_LIST of the types which have befriended this FUNCTION_DECL.  */
#define DECL_BEFRIENDING_CLASSES(NODE) \
  (DECL_LANG_SPECIFIC (NODE)->befriending_classes)

/* Nonzero for FUNCTION_DECL means that this decl is a static
   member function.  */
#define DECL_STATIC_FUNCTION_P(NODE) \
  (DECL_LANG_SPECIFIC (NODE)->decl_flags.static_function)

/* Nonzero for FUNCTION_DECL means that this decl is a non-static
   member function.  */
#define DECL_NONSTATIC_MEMBER_FUNCTION_P(NODE) \
  (TREE_CODE (TREE_TYPE (NODE)) == METHOD_TYPE)

/* Nonzero for FUNCTION_DECL means that this decl is a member function
   (static or non-static).  */
#define DECL_FUNCTION_MEMBER_P(NODE) \
 (DECL_NONSTATIC_MEMBER_FUNCTION_P (NODE) || DECL_STATIC_FUNCTION_P (NODE))

/* Nonzero for FUNCTION_DECL means that this member function
   has `this' as const X *const.  */
#define DECL_CONST_MEMFUNC_P(NODE)					 \
  (DECL_NONSTATIC_MEMBER_FUNCTION_P (NODE)				 \
   && CP_TYPE_CONST_P (TREE_TYPE (TREE_VALUE				 \
				  (TYPE_ARG_TYPES (TREE_TYPE (NODE))))))

/* Nonzero for FUNCTION_DECL means that this member function
   has `this' as volatile X *const.  */
#define DECL_VOLATILE_MEMFUNC_P(NODE)					 \
  (DECL_NONSTATIC_MEMBER_FUNCTION_P (NODE)				 \
   && CP_TYPE_VOLATILE_P (TREE_TYPE (TREE_VALUE				 \
				  (TYPE_ARG_TYPES (TREE_TYPE (NODE))))))

/* Nonzero for a DECL means that this member is a non-static member.  */
#define DECL_NONSTATIC_MEMBER_P(NODE)		\
  ((TREE_CODE (NODE) == FUNCTION_DECL		\
    && DECL_NONSTATIC_MEMBER_FUNCTION_P (NODE))	\
   || TREE_CODE (NODE) == FIELD_DECL)

/* Nonzero for _DECL means that this member object type
   is mutable.  */
#define DECL_MUTABLE_P(NODE) (DECL_LANG_FLAG_0 (NODE))

/* Nonzero for _DECL means that this constructor is a non-converting
   constructor.  */
#define DECL_NONCONVERTING_P(NODE) \
  (DECL_LANG_SPECIFIC (NODE)->decl_flags.nonconverting)

/* Nonzero for FUNCTION_DECL means that this member function is a pure
   virtual function.  */
#define DECL_PURE_VIRTUAL_P(NODE) \
  (DECL_LANG_SPECIFIC (NODE)->decl_flags.pure_virtual)

/* Nonzero for FUNCTION_DECL means that this member function
   must be overridden by derived classes.  */
#define DECL_NEEDS_FINAL_OVERRIDER_P(NODE) \
  (DECL_LANG_SPECIFIC (NODE)->decl_flags.needs_final_overrider)

/* Nonzero if NODE is a thunk, rather than an ordinary function.  */
#define DECL_THUNK_P(NODE)			\
  (TREE_CODE (NODE) == FUNCTION_DECL		\
   && DECL_LANG_FLAG_7 (NODE))

/* Nonzero if NODE is a FUNCTION_DECL, but not a thunk.  */
#define DECL_NON_THUNK_FUNCTION_P(NODE)				\
  (TREE_CODE (NODE) == FUNCTION_DECL && !DECL_THUNK_P (NODE))

/* Nonzero if NODE is `extern "C"'.  */
#define DECL_EXTERN_C_P(NODE) \
  (DECL_LANGUAGE (NODE) == lang_c)

/* Nonzero if NODE is an `extern "C"' function.  */
#define DECL_EXTERN_C_FUNCTION_P(NODE) \
  (DECL_NON_THUNK_FUNCTION_P (NODE) && DECL_EXTERN_C_P (NODE))

/* Set DECL_THUNK_P for node.  */
#define SET_DECL_THUNK_P(NODE) \
  (DECL_LANG_FLAG_7 (NODE) = 1)

/* Nonzero if this DECL is the __PRETTY_FUNCTION__ variable in a
   template function.  */
#define DECL_PRETTY_FUNCTION_P(NODE) \
  (TREE_LANG_FLAG_0 (NODE))

/* The _TYPE context in which this _DECL appears.  This field holds the
   class where a virtual function instance is actually defined. */
#define DECL_CLASS_CONTEXT(NODE) \
  (DECL_CLASS_SCOPE_P (NODE) ? DECL_CONTEXT (NODE) : NULL_TREE)

/* For a non-member friend function, the class (if any) in which this
   friend was defined.  For example, given:

     struct S { friend void f (); };

   the DECL_FRIEND_CONTEXT for `f' will be `S'.  */
#define DECL_FRIEND_CONTEXT(NODE)				\
  ((DECL_FRIEND_P (NODE) && !DECL_FUNCTION_MEMBER_P (NODE))	\
   ? DECL_LANG_SPECIFIC (NODE)->context                         \
   : NULL_TREE)

/* Set the DECL_FRIEND_CONTEXT for NODE to CONTEXT.  */
#define SET_DECL_FRIEND_CONTEXT(NODE, CONTEXT) \
  (DECL_LANG_SPECIFIC (NODE)->context = (CONTEXT))

/* NULL_TREE in DECL_CONTEXT represents the global namespace. */
#define CP_DECL_CONTEXT(NODE) \
  (DECL_CONTEXT (NODE) ? DECL_CONTEXT (NODE) : global_namespace)
#define FROB_CONTEXT(NODE)   ((NODE) == global_namespace ? NULL_TREE : (NODE))

/* For a virtual function, the base where we find its vtable entry.
   For a non-virtual function, the base where it is defined.  */
#define DECL_VIRTUAL_CONTEXT(NODE) \
  (DECL_LANG_SPECIFIC (NODE)->context)

/* 1 iff NODE has namespace scope, including the global namespace.  */
#define DECL_NAMESPACE_SCOPE_P(NODE)				\
  (!DECL_TEMPLATE_PARM_P (NODE)					\
   && TREE_CODE (CP_DECL_CONTEXT (NODE)) == NAMESPACE_DECL)

/* 1 iff NODE is a class member.  */
#define DECL_CLASS_SCOPE_P(NODE) \
  (DECL_CONTEXT (NODE) && TYPE_P (DECL_CONTEXT (NODE)))

/* 1 iff NODE is function-local.  */
#define DECL_FUNCTION_SCOPE_P(NODE) \
  (DECL_CONTEXT (NODE) \
   && TREE_CODE (DECL_CONTEXT (NODE)) == FUNCTION_DECL)

/* 1 iff NODE is function-local, but for types.  */
#define LOCAL_CLASS_P(NODE)				\
  (decl_function_context (TYPE_MAIN_DECL (NODE)) != NULL_TREE)

/* For a NAMESPACE_DECL: the list of using namespace directives
   The PURPOSE is the used namespace, the value is the namespace
   that is the common ancestor. */
#define DECL_NAMESPACE_USING(NODE) DECL_VINDEX (NAMESPACE_DECL_CHECK (NODE))

/* In a NAMESPACE_DECL, the DECL_INITIAL is used to record all users
   of a namespace, to record the transitive closure of using namespace. */
#define DECL_NAMESPACE_USERS(NODE) DECL_INITIAL (NAMESPACE_DECL_CHECK (NODE))

/* In a NAMESPACE_DECL, points to the original namespace if this is
   a namespace alias.  */
#define DECL_NAMESPACE_ALIAS(NODE) \
	DECL_ABSTRACT_ORIGIN (NAMESPACE_DECL_CHECK (NODE))
#define ORIGINAL_NAMESPACE(NODE)  \
  (DECL_NAMESPACE_ALIAS (NODE) ? DECL_NAMESPACE_ALIAS (NODE) : (NODE))

/* Non-zero if NODE is the std namespace.  */
#define DECL_NAMESPACE_STD_P(NODE)			\
  (TREE_CODE (NODE) == NAMESPACE_DECL			\
   && CP_DECL_CONTEXT (NODE) == global_namespace	\
   && DECL_NAME (NODE) == std_identifier)

/* In a non-local VAR_DECL with static storage duration, this is the
   initialization priority.  If this value is zero, the NODE will be
   initialized at the DEFAULT_INIT_PRIORITY.  */
#define DECL_INIT_PRIORITY(NODE) (VAR_DECL_CHECK (NODE)->decl.u2.i)

/* In a TREE_LIST concatenating using directives, indicate indirect
   directives  */
#define TREE_INDIRECT_USING(NODE) (TREE_LIST_CHECK (NODE)->common.lang_flag_0)

/* In a VAR_DECL for a variable declared in a for statement,
   this is the shadowed (local) variable.  */
#define DECL_SHADOWED_FOR_VAR(NODE) DECL_RESULT_FLD(VAR_DECL_CHECK (NODE))

/* In a FUNCTION_DECL, this is nonzero if this function was defined in
   the class definition.  We have saved away the text of the function,
   but have not yet processed it.  */
#define DECL_PENDING_INLINE_P(NODE) \
  (DECL_LANG_SPECIFIC (NODE)->decl_flags.pending_inline_p)

/* If DECL_PENDING_INLINE_P holds, this is the saved text of the
   function.  */
#define DECL_PENDING_INLINE_INFO(NODE) \
  (DECL_LANG_SPECIFIC (NODE)->u.pending_inline_info)

/* For a TYPE_DECL: if this function has many fields, we'll sort them
   and put them into a TREE_VEC. */
#define DECL_SORTED_FIELDS(NODE) \
  (DECL_LANG_SPECIFIC (TYPE_DECL_CHECK (NODE))->u.sorted_fields)

/* True if on the deferred_fns (see decl2.c) list.  */
#define DECL_DEFERRED_FN(DECL) \
  (DECL_LANG_SPECIFIC (DECL)->decl_flags.deferred)

/* For a VAR_DECL, FUNCTION_DECL, TYPE_DECL or TEMPLATE_DECL:
   template-specific information.  */
#define DECL_TEMPLATE_INFO(NODE) \
  (DECL_LANG_SPECIFIC (VAR_TEMPL_TYPE_OR_FUNCTION_DECL_CHECK (NODE)) \
   ->decl_flags.u.template_info)

/* Template information for a RECORD_TYPE or UNION_TYPE.  */
#define CLASSTYPE_TEMPLATE_INFO(NODE) \
  (TYPE_LANG_SPECIFIC (RECORD_OR_UNION_TYPE_CHECK (NODE))->template_info)

/* Template information for an ENUMERAL_TYPE.  Although an enumeration may
   not be a primary template, it may be declared within the scope of a
   primary template and the enumeration constants may depend on
   non-type template parameters.  */
#define ENUM_TEMPLATE_INFO(NODE) (TYPE_BINFO (ENUMERAL_TYPE_CHECK (NODE)))

/* Template information for a template template parameter.  */
#define TEMPLATE_TEMPLATE_PARM_TEMPLATE_INFO(NODE) \
  (TYPE_LANG_SPECIFIC (BOUND_TEMPLATE_TEMPLATE_PARM_TYPE_CHECK (NODE)) \
   ->template_info)

/* Template information for an ENUMERAL_, RECORD_, or UNION_TYPE.  */
#define TYPE_TEMPLATE_INFO(NODE)			\
  (TREE_CODE (NODE) == ENUMERAL_TYPE			\
   ? ENUM_TEMPLATE_INFO (NODE) :			\
   (TREE_CODE (NODE) == BOUND_TEMPLATE_TEMPLATE_PARM	\
    ? TEMPLATE_TEMPLATE_PARM_TEMPLATE_INFO (NODE) :	\
    (TYPE_LANG_SPECIFIC (NODE)				\
     ? CLASSTYPE_TEMPLATE_INFO (NODE)			\
     : NULL_TREE)))

/* Set the template information for an ENUMERAL_, RECORD_, or
   UNION_TYPE to VAL.  */
#define SET_TYPE_TEMPLATE_INFO(NODE, VAL)	\
  (TREE_CODE (NODE) == ENUMERAL_TYPE		\
   ? (ENUM_TEMPLATE_INFO (NODE) = (VAL))	\
   : (CLASSTYPE_TEMPLATE_INFO (NODE) = (VAL)))

#define TI_TEMPLATE(NODE) (TREE_PURPOSE (NODE))
#define TI_ARGS(NODE) (TREE_VALUE (NODE))
#define TI_PENDING_TEMPLATE_FLAG(NODE) TREE_LANG_FLAG_1 (NODE)

/* We use TREE_VECs to hold template arguments.  If there is only one
   level of template arguments, then the TREE_VEC contains the
   arguments directly.  If there is more than one level of template
   arguments, then each entry in the TREE_VEC is itself a TREE_VEC,
   containing the template arguments for a single level.  The first
   entry in the outer TREE_VEC is the outermost level of template
   parameters; the last is the innermost.

   It is incorrect to ever form a template argument vector containing
   only one level of arguments, but which is a TREE_VEC containing as
   its only entry the TREE_VEC for that level.  */

/* Non-zero if the template arguments is actually a vector of vectors,
   rather than just a vector.  */
#define TMPL_ARGS_HAVE_MULTIPLE_LEVELS(NODE) \
  ((NODE) != NULL_TREE						\
   && TREE_CODE (NODE) == TREE_VEC				\
   && TREE_VEC_LENGTH (NODE) > 0				\
   && TREE_VEC_ELT (NODE, 0) != NULL_TREE			\
   && TREE_CODE (TREE_VEC_ELT (NODE, 0)) == TREE_VEC)

/* The depth of a template argument vector.  When called directly by
   the parser, we use a TREE_LIST rather than a TREE_VEC to represent
   template arguments.  In fact, we may even see NULL_TREE if there
   are no template arguments.  In both of those cases, there is only
   one level of template arguments.  */
#define TMPL_ARGS_DEPTH(NODE)					\
  (TMPL_ARGS_HAVE_MULTIPLE_LEVELS (NODE) ? TREE_VEC_LENGTH (NODE) : 1)

/* The LEVELth level of the template ARGS.  The outermost level of of
   args is level 1, not level 0.  */
#define TMPL_ARGS_LEVEL(ARGS, LEVEL)		\
  (TMPL_ARGS_HAVE_MULTIPLE_LEVELS (ARGS)	\
   ? TREE_VEC_ELT (ARGS, (LEVEL) - 1) : (ARGS))

/* Set the LEVELth level of the template ARGS to VAL.  This macro does
   not work with single-level argument vectors.  */
#define SET_TMPL_ARGS_LEVEL(ARGS, LEVEL, VAL)	\
  (TREE_VEC_ELT (ARGS, (LEVEL) - 1) = (VAL))

/* Accesses the IDXth parameter in the LEVELth level of the ARGS.  */
#define TMPL_ARG(ARGS, LEVEL, IDX)				\
  (TREE_VEC_ELT (TMPL_ARGS_LEVEL (ARGS, LEVEL), IDX))

/* Set the IDXth element in the LEVELth level of ARGS to VAL.  This
   macro does not work with single-level argument vectors.  */
#define SET_TMPL_ARG(ARGS, LEVEL, IDX, VAL)			\
  (TREE_VEC_ELT (TREE_VEC_ELT ((ARGS), (LEVEL) - 1), (IDX)) = (VAL))

/* Given a single level of template arguments in NODE, return the
   number of arguments.  */
#define NUM_TMPL_ARGS(NODE)				\
  ((NODE) == NULL_TREE ? 0				\
   : (TREE_CODE (NODE) == TREE_VEC			\
      ? TREE_VEC_LENGTH (NODE) : list_length (NODE)))

/* Returns the innermost level of template arguments in ARGS.  */
#define INNERMOST_TEMPLATE_ARGS(NODE) \
  (get_innermost_template_args ((NODE), 1))

/* The number of levels of template parameters given by NODE.  */
#define TMPL_PARMS_DEPTH(NODE) \
  ((HOST_WIDE_INT) TREE_INT_CST_LOW (TREE_PURPOSE (NODE)))

/* The TEMPLATE_DECL instantiated or specialized by NODE.  This
   TEMPLATE_DECL will be the immediate parent, not the most general
   template.  For example, in:

      template <class T> struct S { template <class U> void f(U); }

   the FUNCTION_DECL for S<int>::f<double> will have, as its
   DECL_TI_TEMPLATE, `template <class U> S<int>::f<U>'.

   As a special case, for a member friend template of a template
   class, this value will not be a TEMPLATE_DECL, but rather a
   LOOKUP_EXPR, IDENTIFIER_NODE or OVERLOAD indicating the name of
   the template and any explicit template arguments provided.  For
   example, in:

     template <class T> struct S { friend void f<int>(int, double); }

   the DECL_TI_TEMPLATE will be a LOOKUP_EXPR for `f' and the
   DECL_TI_ARGS will be {int}.  */
#define DECL_TI_TEMPLATE(NODE)      TI_TEMPLATE (DECL_TEMPLATE_INFO (NODE))

/* The template arguments used to obtain this decl from the most
   general form of DECL_TI_TEMPLATE.  For the example given for
   DECL_TI_TEMPLATE, the DECL_TI_ARGS will be {int, double}.  These
   are always the full set of arguments required to instantiate this
   declaration from the most general template specialized here.  */
#define DECL_TI_ARGS(NODE)          TI_ARGS (DECL_TEMPLATE_INFO (NODE))
#define CLASSTYPE_TI_TEMPLATE(NODE) TI_TEMPLATE (CLASSTYPE_TEMPLATE_INFO (NODE))
#define CLASSTYPE_TI_ARGS(NODE)     TI_ARGS (CLASSTYPE_TEMPLATE_INFO (NODE))
#define ENUM_TI_TEMPLATE(NODE)			\
  TI_TEMPLATE (ENUM_TEMPLATE_INFO (NODE))
#define ENUM_TI_ARGS(NODE)			\
  TI_ARGS (ENUM_TEMPLATE_INFO (NODE))

/* Like DECL_TI_TEMPLATE, but for an ENUMERAL_, RECORD_, or UNION_TYPE.  */
#define TYPE_TI_TEMPLATE(NODE)			\
  (TI_TEMPLATE (TYPE_TEMPLATE_INFO (NODE)))

/* Like DECL_TI_ARGS, but for an ENUMERAL_, RECORD_, or UNION_TYPE.  */
#define TYPE_TI_ARGS(NODE)			\
  (TI_ARGS (TYPE_TEMPLATE_INFO (NODE)))

#define INNERMOST_TEMPLATE_PARMS(NODE)  TREE_VALUE (NODE)

/* Nonzero if the NODE corresponds to the template parameters for a
   member template, whose inline definition is being processed after
   the class definition is complete.  */
#define TEMPLATE_PARMS_FOR_INLINE(NODE) TREE_LANG_FLAG_1 (NODE)

/* In a FUNCTION_DECL, the saved language-specific per-function data.  */
#define DECL_SAVED_FUNCTION_DATA(NODE) \
  (DECL_LANG_SPECIFIC (FUNCTION_DECL_CHECK (NODE))->u.saved_language_function)

#define NEW_EXPR_USE_GLOBAL(NODE)	TREE_LANG_FLAG_0 (NODE)
#define DELETE_EXPR_USE_GLOBAL(NODE)	TREE_LANG_FLAG_0 (NODE)
#define DELETE_EXPR_USE_VEC(NODE)	TREE_LANG_FLAG_1 (NODE)
#define LOOKUP_EXPR_GLOBAL(NODE)	TREE_LANG_FLAG_0 (NODE)

/* Nonzero if this AGGR_INIT_EXPR provides for initialization via a
   constructor call, rather than an ordinary function call.  */
#define AGGR_INIT_VIA_CTOR_P(NODE) \
  TREE_LANG_FLAG_0 (AGGR_INIT_EXPR_CHECK (NODE))

/* The TYPE_MAIN_DECL for a class template type is a TYPE_DECL, not a
   TEMPLATE_DECL.  This macro determines whether or not a given class
   type is really a template type, as opposed to an instantiation or
   specialization of one.  */
#define CLASSTYPE_IS_TEMPLATE(NODE)  \
  (CLASSTYPE_TEMPLATE_INFO (NODE)    \
   && !CLASSTYPE_USE_TEMPLATE (NODE) \
   && PRIMARY_TEMPLATE_P (CLASSTYPE_TI_TEMPLATE (NODE)))

/* The name used by the user to name the typename type.  Typically,
   this is an IDENTIFIER_NODE, and the same as the DECL_NAME on the
   corresponding TYPE_DECL.  However, this may also be a
   TEMPLATE_ID_EXPR if we had something like `typename X::Y<T>'.  */
#define TYPENAME_TYPE_FULLNAME(NODE) (TYPE_FIELDS (NODE))

/* Nonzero if NODE is an implicit typename.  */
#define IMPLICIT_TYPENAME_P(NODE) \
  (TREE_CODE (NODE) == TYPENAME_TYPE && TREE_TYPE (NODE))

/* Nonzero if NODE is a TYPE_DECL that should not be visible because
   it is from a dependent base class.  */
#define IMPLICIT_TYPENAME_TYPE_DECL_P(NODE)	\
  (TREE_CODE (NODE) == TYPE_DECL		\
   && DECL_ARTIFICIAL (NODE)			\
   && IMPLICIT_TYPENAME_P (TREE_TYPE (NODE)))

/* Nonzero in INTEGER_CST means that this int is negative by dint of
   using a twos-complement negated operand.  */
#define TREE_NEGATED_INT(NODE) TREE_LANG_FLAG_0 (INTEGER_CST_CHECK (NODE))

/* Nonzero in any kind of _TYPE where conversions to base-classes may
   involve pointer arithmetic.  If this is zero, then converting to
   a base-class never requires changing the value of the pointer.  */
#define TYPE_BASE_CONVS_MAY_REQUIRE_CODE_P(NODE) (TREE_LANG_FLAG_1 (NODE))

/* [class.virtual]

   A class that declares or inherits a virtual function is called a
   polymorphic class.  */
#define TYPE_POLYMORPHIC_P(NODE) (TREE_LANG_FLAG_2 (NODE))

/* Nonzero if this class has a virtual function table pointer.  */
#define TYPE_CONTAINS_VPTR_P(NODE)		\
  (TYPE_POLYMORPHIC_P (NODE)			\
   || TYPE_USES_VIRTUAL_BASECLASSES (NODE))

extern int flag_new_for_scope;

/* This flag is true of a local VAR_DECL if it was declared in a for
   statement, but we are no longer in the scope of the for.  */
#define DECL_DEAD_FOR_LOCAL(NODE) DECL_LANG_FLAG_7 (VAR_DECL_CHECK (NODE))

/* This flag is set on a VAR_DECL that is a DECL_DEAD_FOR_LOCAL
   if we already emitted a warning about using it.  */
#define DECL_ERROR_REPORTED(NODE) DECL_LANG_FLAG_0 (VAR_DECL_CHECK (NODE))

/* Nonzero if NODE is a FUNCTION_DECL (for a function with global
   scope) declared in a local scope.  */
#define DECL_LOCAL_FUNCTION_P(NODE) \
  DECL_LANG_FLAG_0 (FUNCTION_DECL_CHECK (NODE))

/* Nonzero if NODE is a FUNCTION_DECL for a built-in function, and we have
   not yet seen a prototype for that function.  */
#define DECL_ANTICIPATED(NODE) \
  (DECL_LANG_SPECIFIC (DECL_CHECK (NODE))->decl_flags.anticipated_p)

/* Record whether a typedef for type `int' was actually `signed int'.  */
#define C_TYPEDEF_EXPLICITLY_SIGNED(EXP) DECL_LANG_FLAG_1 (EXP)

/* Returns non-zero if DECL has external linkage, as specified by the
   language standard.  (This predicate may hold even when the
   corresponding entity is not actually given external linkage in the
   object file; see decl_linkage for details.)  */
#define DECL_EXTERNAL_LINKAGE_P(DECL) \
  (decl_linkage (DECL) == lk_external)

#define INTEGRAL_CODE_P(CODE) \
  ((CODE) == INTEGER_TYPE || (CODE) == ENUMERAL_TYPE || (CODE) == BOOLEAN_TYPE)

/* [basic.fundamental]

   Types  bool, char, wchar_t, and the signed and unsigned integer types
   are collectively called integral types.

   Note that INTEGRAL_TYPE_P, as defined in tree.h, allows enumeration
   types as well, which is incorrect in C++.  */
#define CP_INTEGRAL_TYPE_P(TYPE)		\
  (TREE_CODE (TYPE) == BOOLEAN_TYPE		\
   || TREE_CODE (TYPE) == INTEGER_TYPE)

/* [basic.fundamental]

   Integral and floating types are collectively called arithmetic
   types.  */
#define ARITHMETIC_TYPE_P(TYPE) \
  (CP_INTEGRAL_TYPE_P (TYPE) || TREE_CODE (TYPE) == REAL_TYPE)

/* Nonzero for _TYPE means that the _TYPE defines
   at least one constructor.  */
#define TYPE_HAS_CONSTRUCTOR(NODE) (TYPE_LANG_FLAG_1 (NODE))

/* When appearing in an INDIRECT_REF, it means that the tree structure
   underneath is actually a call to a constructor.  This is needed
   when the constructor must initialize local storage (which can
   be automatically destroyed), rather than allowing it to allocate
   space from the heap.

   When appearing in a SAVE_EXPR, it means that underneath
   is a call to a constructor.

   When appearing in a CONSTRUCTOR, it means that it was
   a GNU C constructor expression.

   When appearing in a FIELD_DECL, it means that this field
   has been duly initialized in its constructor.  */
#define TREE_HAS_CONSTRUCTOR(NODE) (TREE_LANG_FLAG_4 (NODE))

#define EMPTY_CONSTRUCTOR_P(NODE) (TREE_CODE (NODE) == CONSTRUCTOR	   \
				   && CONSTRUCTOR_ELTS (NODE) == NULL_TREE \
				   && ! TREE_HAS_CONSTRUCTOR (NODE))

/* Nonzero for _TYPE means that the _TYPE defines a destructor.  */
#define TYPE_HAS_DESTRUCTOR(NODE) (TYPE_LANG_FLAG_2 (NODE))

/* Nonzero means that an object of this type can not be initialized using
   an initializer list.  */
#define CLASSTYPE_NON_AGGREGATE(NODE) \
  (TYPE_LANG_SPECIFIC (NODE)->non_aggregate)
#define TYPE_NON_AGGREGATE_CLASS(NODE) \
  (IS_AGGR_TYPE (NODE) && CLASSTYPE_NON_AGGREGATE (NODE))

/* Nonzero if there is a user-defined X::op=(x&) for this class.  */
#define TYPE_HAS_REAL_ASSIGN_REF(NODE) (TYPE_LANG_SPECIFIC (NODE)->has_real_assign_ref)
#define TYPE_HAS_COMPLEX_ASSIGN_REF(NODE) (TYPE_LANG_SPECIFIC (NODE)->has_complex_assign_ref)
#define TYPE_HAS_ABSTRACT_ASSIGN_REF(NODE) (TYPE_LANG_SPECIFIC (NODE)->has_abstract_assign_ref)
#define TYPE_HAS_COMPLEX_INIT_REF(NODE) (TYPE_LANG_SPECIFIC (NODE)->has_complex_init_ref)

/* Nonzero if TYPE has a trivial destructor.  From [class.dtor]:

     A destructor is trivial if it is an implicitly declared
     destructor and if:

       - all of the direct base classes of its class have trivial
         destructors,

       - for all of the non-static data members of its class that are
         of class type (or array thereof), each such class has a
	 trivial destructor.  */
#define TYPE_HAS_TRIVIAL_DESTRUCTOR(NODE) \
  (!TYPE_HAS_NONTRIVIAL_DESTRUCTOR (NODE))

/* Nonzero for _TYPE node means that this type does not have a trivial
   destructor.  Therefore, destroying an object of this type will
   involve a call to a destructor.  This can apply to objects of
   ARRAY_TYPE is the type of the elements needs a destructor.  */
#define TYPE_HAS_NONTRIVIAL_DESTRUCTOR(NODE) \
  (TYPE_LANG_FLAG_4 (NODE))

/* Nonzero for class type means that copy initialization of this type can use
   a bitwise copy.  */
#define TYPE_HAS_TRIVIAL_INIT_REF(NODE) \
  (TYPE_HAS_INIT_REF (NODE) && ! TYPE_HAS_COMPLEX_INIT_REF (NODE))

/* Nonzero for class type means that assignment of this type can use
   a bitwise copy.  */
#define TYPE_HAS_TRIVIAL_ASSIGN_REF(NODE) \
  (TYPE_HAS_ASSIGN_REF (NODE) && ! TYPE_HAS_COMPLEX_ASSIGN_REF (NODE))

#define TYPE_PTRMEM_P(NODE)					\
  (TREE_CODE (NODE) == POINTER_TYPE				\
   && TREE_CODE (TREE_TYPE (NODE)) == OFFSET_TYPE)
#define TYPE_PTR_P(NODE)				\
  (TREE_CODE (NODE) == POINTER_TYPE			\
   && TREE_CODE (TREE_TYPE (NODE)) != OFFSET_TYPE)
#define TYPE_PTROB_P(NODE)						\
  (TYPE_PTR_P (NODE) && TREE_CODE (TREE_TYPE (NODE)) != FUNCTION_TYPE	\
   && TREE_CODE (TREE_TYPE (NODE)) != VOID_TYPE)
#define TYPE_PTROBV_P(NODE)						\
  (TYPE_PTR_P (NODE) && TREE_CODE (TREE_TYPE (NODE)) != FUNCTION_TYPE)
#define TYPE_PTRFN_P(NODE)				\
  (TREE_CODE (NODE) == POINTER_TYPE			\
   && TREE_CODE (TREE_TYPE (NODE)) == FUNCTION_TYPE)
#define TYPE_REFFN_P(NODE)				\
  (TREE_CODE (NODE) == REFERENCE_TYPE			\
   && TREE_CODE (TREE_TYPE (NODE)) == FUNCTION_TYPE)

/* Nonzero for _TYPE node means that this type is a pointer to member
   function type.  */
#define TYPE_PTRMEMFUNC_P(NODE)		\
  (TREE_CODE (NODE) == RECORD_TYPE	\
   && TYPE_LANG_SPECIFIC (NODE)		\
   && TYPE_PTRMEMFUNC_FLAG (NODE))

#define TYPE_PTRMEMFUNC_FLAG(NODE) \
  (TYPE_LANG_SPECIFIC (NODE)->ptrmemfunc_flag)

/* Indicates when overload resolution may resolve to a pointer to
   member function. [expr.unary.op]/3 */
#define PTRMEM_OK_P(NODE) TREE_LANG_FLAG_0 (NODE)

/* Get the POINTER_TYPE to the METHOD_TYPE associated with this
   pointer to member function.  TYPE_PTRMEMFUNC_P _must_ be true,
   before using this macro.  */
#define TYPE_PTRMEMFUNC_FN_TYPE(NODE) \
  (TREE_TYPE (TYPE_FIELDS (NODE)))

/* Returns `A' for a type like `int (A::*)(double)' */
#define TYPE_PTRMEMFUNC_OBJECT_TYPE(NODE) \
  TYPE_METHOD_BASETYPE (TREE_TYPE (TYPE_PTRMEMFUNC_FN_TYPE (NODE)))

/* These are use to manipulate the canonical RECORD_TYPE from the
   hashed POINTER_TYPE, and can only be used on the POINTER_TYPE.  */
#define TYPE_GET_PTRMEMFUNC_TYPE(NODE) \
  ((tree)TYPE_LANG_SPECIFIC (NODE))
#define TYPE_SET_PTRMEMFUNC_TYPE(NODE, VALUE) \
  (TYPE_LANG_SPECIFIC (NODE) = ((struct lang_type *)(void*)(VALUE)))
/* Returns the pfn field from a TYPE_PTRMEMFUNC_P.  */
#define PFN_FROM_PTRMEMFUNC(NODE) pfn_from_ptrmemfunc ((NODE))

/* For a pointer-to-member type of the form `T X::*', this is `X'.
   For a type like `void (X::*)() const', this type is `X', not `const
   X'.  To get at the `const X' you have to look at the
   TYPE_PTRMEM_POINTED_TO_TYPE; there, the first parameter will have
   type `const X*'.  */
#define TYPE_PTRMEM_CLASS_TYPE(NODE)			\
  (TYPE_PTRMEM_P (NODE)					\
   ? TYPE_OFFSET_BASETYPE (TREE_TYPE (NODE))		\
   : TYPE_PTRMEMFUNC_OBJECT_TYPE (NODE))

/* For a pointer-to-member type of the form `T X::*', this is `T'.  */
#define TYPE_PTRMEM_POINTED_TO_TYPE(NODE)		\
   (TYPE_PTRMEM_P (NODE)				\
    ? TREE_TYPE (TREE_TYPE (NODE))			\
    : TREE_TYPE (TYPE_PTRMEMFUNC_FN_TYPE (NODE)))

/* For a pointer-to-member constant `X::Y' this is the RECORD_TYPE for
   `X'.  */
#define PTRMEM_CST_CLASS(NODE) \
  TYPE_PTRMEM_CLASS_TYPE (TREE_TYPE (PTRMEM_CST_CHECK (NODE)))

/* For a pointer-to-member constant `X::Y' this is the _DECL for
   `Y'.  */
#define PTRMEM_CST_MEMBER(NODE) (((ptrmem_cst_t)PTRMEM_CST_CHECK (NODE))->member)

/* Nonzero for VAR_DECL and FUNCTION_DECL node means that `extern' was
   specified in its declaration.  This can also be set for an
   erroneously declared PARM_DECL.  */
#define DECL_THIS_EXTERN(NODE) \
  DECL_LANG_FLAG_2 (VAR_FUNCTION_OR_PARM_DECL_CHECK (NODE))

/* Nonzero for VAR_DECL and FUNCTION_DECL node means that `static' was
   specified in its declaration.  This can also be set for an
   erroneously declared PARM_DECL.  */
#define DECL_THIS_STATIC(NODE) \
  DECL_LANG_FLAG_6 (VAR_FUNCTION_OR_PARM_DECL_CHECK (NODE))

/* Nonzero if TYPE is an anonymous union or struct type.  We have to use a
   flag for this because "A union for which objects or pointers are
   declared is not an anonymous union" [class.union].  */
#define ANON_AGGR_TYPE_P(NODE)				\
  (CLASS_TYPE_P (NODE) && TYPE_LANG_SPECIFIC (NODE)->anon_aggr)
#define SET_ANON_AGGR_TYPE_P(NODE)			\
  (TYPE_LANG_SPECIFIC (NODE)->anon_aggr = 1)

/* Nonzero if TYPE is an anonymous union type.  */
#define ANON_UNION_TYPE_P(NODE) \
  (TREE_CODE (NODE) == UNION_TYPE && ANON_AGGR_TYPE_P (NODE))

#define UNKNOWN_TYPE LANG_TYPE

/* Define fields and accessors for nodes representing declared names.  */

#define TYPE_WAS_ANONYMOUS(NODE) (TYPE_LANG_SPECIFIC (NODE)->was_anonymous)

/* C++: all of these are overloaded!  These apply only to TYPE_DECLs.  */

/* The format of each node in the DECL_FRIENDLIST is as follows:

   The TREE_PURPOSE will be the name of a function, i.e., an
   IDENTIFIER_NODE.  The TREE_VALUE will be itself a TREE_LIST, the
   list of functions with that name which are friends.  The
   TREE_PURPOSE of each node in this sublist will be error_mark_node,
   if the function was declared a friend individually, in which case
   the TREE_VALUE will be the function_decl.  If, however, all
   functions with a given name in a class were declared to be friends,
   the TREE_PUROSE will be the class type, and the TREE_VALUE will be
   NULL_TREE.  */
#define DECL_FRIENDLIST(NODE)		(DECL_INITIAL (NODE))
#define FRIEND_NAME(LIST) (TREE_PURPOSE (LIST))
#define FRIEND_DECLS(LIST) (TREE_VALUE (LIST))

/* The DECL_ACCESS, if non-NULL, is a TREE_LIST.  The TREE_PURPOSE of
   each node is a type; the TREE_VALUE is the access granted for this
   DECL in that type.  The DECL_ACCESS is set by access declarations.
   For example, if a member that would normally be public in a
   derived class is made protected, then the derived class and the
   protected_access_node will appear in the DECL_ACCESS for the node.  */
#define DECL_ACCESS(NODE) (DECL_LANG_SPECIFIC (NODE)->decl_flags.u2.access)

/* Nonzero if the FUNCTION_DECL is a global constructor.  */
#define DECL_GLOBAL_CTOR_P(NODE) \
  (DECL_LANG_SPECIFIC (NODE)->decl_flags.global_ctor_p)

/* Nonzero if the FUNCTION_DECL is a global destructor.  */
#define DECL_GLOBAL_DTOR_P(NODE) \
  (DECL_LANG_SPECIFIC (NODE)->decl_flags.global_dtor_p)

/* If DECL_GLOBAL_CTOR_P or DECL_GLOBAL_DTOR_P holds, this macro
   returns the initialization priority for the function.  Constructors
   with lower numbers should be run first.  Destructors should be run
   in the reverse order of constructors.  */
#define GLOBAL_INIT_PRIORITY(NODE) \
  (DECL_LANG_SPECIFIC (NODE)->decl_flags.u2.init_priority)

/* Accessor macros for C++ template decl nodes.  */

/* The DECL_TEMPLATE_PARMS are a list.  The TREE_PURPOSE of each node
   is a INT_CST whose TREE_INT_CST_LOW indicates the level of the
   template parameters, with 1 being the outermost set of template
   parameters.  The TREE_VALUE is a vector, whose elements are the
   template parameters at each level.  Each element in the vector is a
   TREE_LIST, whose TREE_VALUE is a PARM_DECL (if the parameter is a
   non-type parameter), or a TYPE_DECL (if the parameter is a type
   parameter).  The TREE_PURPOSE is the default value, if any.  The
   TEMPLATE_PARM_INDEX for the parameter is avilable as the
   DECL_INITIAL (for a PARM_DECL) or as the TREE_TYPE (for a
   TYPE_DECL).  */
#define DECL_TEMPLATE_PARMS(NODE)       DECL_ARGUMENTS (NODE)
#define DECL_INNERMOST_TEMPLATE_PARMS(NODE) \
   INNERMOST_TEMPLATE_PARMS (DECL_TEMPLATE_PARMS (NODE))
#define DECL_NTPARMS(NODE) \
   TREE_VEC_LENGTH (DECL_INNERMOST_TEMPLATE_PARMS (NODE))
/* For function, method, class-data templates.  */
#define DECL_TEMPLATE_RESULT(NODE)      DECL_RESULT_FLD (NODE)
/* For a static member variable template, the
   DECL_TEMPLATE_INSTANTIATIONS list contains the explicitly and
   implicitly generated instantiations of the variable.  There are no
   partial instantiations of static member variables, so all of these
   will be full instantiations.

   For a class template the DECL_TEMPLATE_INSTANTIATIONS lists holds
   all instantiations and specializations of the class type, including
   partial instantiations and partial specializations.

   In both cases, the TREE_PURPOSE of each node contains the arguments
   used; the TREE_VALUE contains the generated variable.  The template
   arguments are always complete.  For example, given:

      template <class T> struct S1 {
        template <class U> struct S2 {};
	template <class U> struct S2<U*> {};
      };

   the record for the partial specialization will contain, as its
   argument list, { {T}, {U*} }, and will be on the
   DECL_TEMPLATE_INSTANTIATIONS list for `template <class T> template
   <class U> struct S1<T>::S2'.

   This list is not used for function templates.  */
#define DECL_TEMPLATE_INSTANTIATIONS(NODE) DECL_VINDEX (NODE)
/* For a function template, the DECL_TEMPLATE_SPECIALIZATIONS lists
   contains all instantiations and specializations of the function,
   including partial instantiations.  For a partial instantiation
   which is a specialization, this list holds only full
   specializations of the template that are instantiations of the
   partial instantiation.  For example, given:

      template <class T> struct S {
        template <class U> void f(U);
	template <> void f(T);
      };

   the `S<int>::f<int>(int)' function will appear on the
   DECL_TEMPLATE_SPECIALIZATIONS list for both `template <class T>
   template <class U> void S<T>::f(U)' and `template <class T> void
   S<int>::f(T)'.  In the latter case, however, it will have only the
   innermost set of arguments (T, in this case).  The DECL_TI_TEMPLATE
   for the function declaration will point at the specialization, not
   the fully general template.

   For a class template, this list contains the partial
   specializations of this template.  (Full specializations are not
   recorded on this list.)  The TREE_PURPOSE holds the innermost
   arguments used in the partial specialization (e.g., for `template
   <class T> struct S<T*, int>' this will be `T*'.)  The TREE_VALUE
   holds the innermost template parameters for the specialization
   (e.g., `T' in the example above.)  The TREE_TYPE is the _TYPE node
   for the partial specialization.

   This list is not used for static variable templates.  */
#define DECL_TEMPLATE_SPECIALIZATIONS(NODE)     DECL_SIZE (NODE)

/* Nonzero for a DECL which is actually a template parameter.  */
#define DECL_TEMPLATE_PARM_P(NODE)		\
  (DECL_LANG_FLAG_0 (NODE)			\
   && (TREE_CODE (NODE) == CONST_DECL		\
       || TREE_CODE (NODE) == PARM_DECL		\
       || TREE_CODE (NODE) == TYPE_DECL		\
       || TREE_CODE (NODE) == TEMPLATE_DECL))

/* Mark NODE as a template parameter.  */
#define SET_DECL_TEMPLATE_PARM_P(NODE) \
  (DECL_LANG_FLAG_0 (NODE) = 1)

/* Nonzero if NODE is a template template parameter.  */
#define DECL_TEMPLATE_TEMPLATE_PARM_P(NODE) \
  (TREE_CODE (NODE) == TEMPLATE_DECL && DECL_TEMPLATE_PARM_P (NODE))

#define DECL_FUNCTION_TEMPLATE_P(NODE)  \
  (TREE_CODE (NODE) == TEMPLATE_DECL \
   && TREE_CODE (DECL_TEMPLATE_RESULT (NODE)) == FUNCTION_DECL)

/* Nonzero for a DECL that represents a template class.  */
#define DECL_CLASS_TEMPLATE_P(NODE) \
  (TREE_CODE (NODE) == TEMPLATE_DECL \
   && TREE_CODE (DECL_TEMPLATE_RESULT (NODE)) == TYPE_DECL \
   && !DECL_TEMPLATE_TEMPLATE_PARM_P (NODE))

/* Nonzero if NODE which declares a type.  */
#define DECL_DECLARES_TYPE_P(NODE) \
  (TREE_CODE (NODE) == TYPE_DECL || DECL_CLASS_TEMPLATE_P (NODE))

/* Nonzero if NODE is the typedef implicitly generated for a type when
   the type is declared.  (In C++, `struct S {};' is roughly equivalent
   to `struct S {}; typedef struct S S;' in C.  This macro will hold
   for the typedef indicated in this example.  Note that in C++, there
   is a second implicit typedef for each class, in the scope of `S'
   itself, so that you can say `S::S'.  This macro does *not* hold for
   those typedefs.  */
#define DECL_IMPLICIT_TYPEDEF_P(NODE) \
  (TREE_CODE (NODE) == TYPE_DECL && DECL_LANG_FLAG_2 (NODE))
#define SET_DECL_IMPLICIT_TYPEDEF_P(NODE) \
  (DECL_LANG_FLAG_2 (NODE) = 1)

/* A `primary' template is one that has its own template header.  A
   member function of a class template is a template, but not primary.
   A member template is primary.  Friend templates are primary, too.  */

/* Returns the primary template corresponding to these parameters.  */
#define DECL_PRIMARY_TEMPLATE(NODE) \
  (TREE_TYPE (DECL_INNERMOST_TEMPLATE_PARMS (NODE)))

/* Returns non-zero if NODE is a primary template.  */
#define PRIMARY_TEMPLATE_P(NODE) (DECL_PRIMARY_TEMPLATE (NODE) == (NODE))

#define CLASSTYPE_TEMPLATE_LEVEL(NODE) \
  (TREE_INT_CST_LOW (TREE_PURPOSE (CLASSTYPE_TI_TEMPLATE (NODE))))

/* Indicates whether or not (and how) a template was expanded for this
   FUNCTION_DECL or VAR_DECL.
     0=normal declaration, e.g. int min (int, int);
     1=implicit template instantiation
     2=explicit template specialization, e.g. int min<int> (int, int);
     3=explicit template instantiation, e.g. template int min<int> (int, int);  */
#define DECL_USE_TEMPLATE(NODE) (DECL_LANG_SPECIFIC (NODE)->decl_flags.use_template)

#define DECL_TEMPLATE_INSTANTIATION(NODE) (DECL_USE_TEMPLATE (NODE) & 1)
#define CLASSTYPE_TEMPLATE_INSTANTIATION(NODE) \
  (CLASSTYPE_USE_TEMPLATE (NODE) & 1)

#define DECL_TEMPLATE_SPECIALIZATION(NODE) (DECL_USE_TEMPLATE (NODE) == 2)
#define SET_DECL_TEMPLATE_SPECIALIZATION(NODE) (DECL_USE_TEMPLATE (NODE) = 2)
#define CLASSTYPE_TEMPLATE_SPECIALIZATION(NODE) \
  (CLASSTYPE_USE_TEMPLATE (NODE) == 2)
#define SET_CLASSTYPE_TEMPLATE_SPECIALIZATION(NODE) \
  (CLASSTYPE_USE_TEMPLATE (NODE) = 2)

#define DECL_IMPLICIT_INSTANTIATION(NODE) (DECL_USE_TEMPLATE (NODE) == 1)
#define SET_DECL_IMPLICIT_INSTANTIATION(NODE) (DECL_USE_TEMPLATE (NODE) = 1)
#define CLASSTYPE_IMPLICIT_INSTANTIATION(NODE) \
  (CLASSTYPE_USE_TEMPLATE (NODE) == 1)
#define SET_CLASSTYPE_IMPLICIT_INSTANTIATION(NODE) \
  (CLASSTYPE_USE_TEMPLATE (NODE) = 1)

#define DECL_EXPLICIT_INSTANTIATION(NODE) (DECL_USE_TEMPLATE (NODE) == 3)
#define SET_DECL_EXPLICIT_INSTANTIATION(NODE) (DECL_USE_TEMPLATE (NODE) = 3)
#define CLASSTYPE_EXPLICIT_INSTANTIATION(NODE) \
  (CLASSTYPE_USE_TEMPLATE (NODE) == 3)
#define SET_CLASSTYPE_EXPLICIT_INSTANTIATION(NODE) \
  (CLASSTYPE_USE_TEMPLATE (NODE) = 3)

/* Non-zero if DECL is a friend function which is an instantiation
   from the point of view of the compiler, but not from the point of
   view of the language.  For example given:
      template <class T> struct S { friend void f(T) {}; };
   the declaration of `void f(int)' generated when S<int> is
   instantiated will not be a DECL_TEMPLATE_INSTANTIATION, but will be
   a DECL_FRIEND_PSUEDO_TEMPLATE_INSTANTIATION.  */
#define DECL_FRIEND_PSEUDO_TEMPLATE_INSTANTIATION(DECL) \
  (DECL_TEMPLATE_INFO (DECL) && !DECL_USE_TEMPLATE (DECL))

/* Non-zero if TYPE is a partial instantiation of a template class,
   i.e., an instantiation whose instantiation arguments involve
   template types.  */
#define PARTIAL_INSTANTIATION_P(TYPE) \
  (TYPE_LANG_SPECIFIC (TYPE)->is_partial_instantiation)

/* Non-zero iff we are currently processing a declaration for an
   entity with its own template parameter list, and which is not a
   full specialization.  */
#define PROCESSING_REAL_TEMPLATE_DECL_P() \
  (processing_template_decl > template_class_depth (current_class_type))

/* This function may be a guiding decl for a template.  */
#define DECL_MAYBE_TEMPLATE(NODE) DECL_LANG_FLAG_4 (NODE)

/* Nonzero if this VAR_DECL or FUNCTION_DECL has already been
   instantiated, i.e. its definition has been generated from the
   pattern given in the the template.  */
#define DECL_TEMPLATE_INSTANTIATED(NODE) \
  DECL_LANG_FLAG_1 (VAR_OR_FUNCTION_DECL_CHECK (NODE))

/* We know what we're doing with this decl now.  */
#define DECL_INTERFACE_KNOWN(NODE) DECL_LANG_FLAG_5 (NODE)

/* This function was declared inline.  This flag controls the linkage
   semantics of 'inline'; whether or not the function is inlined is
   controlled by DECL_INLINE.  */
#define DECL_DECLARED_INLINE_P(NODE) \
  (DECL_LANG_SPECIFIC (NODE)->decl_flags.base.declared_inline)

/* DECL_EXTERNAL must be set on a decl until the decl is actually emitted,
   so that assemble_external will work properly.  So we have this flag to
   tell us whether the decl is really not external.  */
#define DECL_NOT_REALLY_EXTERN(NODE) \
  (DECL_LANG_SPECIFIC (NODE)->decl_flags.not_really_extern)

#define DECL_REALLY_EXTERN(NODE) \
  (DECL_EXTERNAL (NODE) && ! DECL_NOT_REALLY_EXTERN (NODE))

/* A thunk is a stub function.

   A thunk is an alternate entry point for an ordinary FUNCTION_DECL.
   The address of the ordinary FUNCTION_DECL is given by the
   DECL_INITIAL, which is always an ADDR_EXPR whose operand is a
   FUNCTION_DECL.  The job of the thunk is to adjust the `this'
   pointer before transferring control to the FUNCTION_DECL.

   A thunk may perform either, or both, of the following operations:

   o Adjust the `this' pointer by a constant offset.
   o Adjust the `this' pointer by looking up a vcall-offset
     in the vtable.

   If both operations are performed, then the constant adjument to
   `this' is performed first.

   The constant adjustment is given by THUNK_DELTA.  If the
   vcall-offset is required, the index into the vtable is given by
   THUNK_VCALL_OFFSET.  */

/* An integer indicating how many bytes should be subtracted from the
   `this' pointer when this function is called.  */
#define THUNK_DELTA(DECL) (DECL_CHECK (DECL)->decl.u1.i)

/* A tree indicating how many bytes should be subtracted from the
   vtable for the `this' pointer to find the vcall offset.  (The vptr
   is always located at offset zero from the f `this' pointer.)  If
   NULL, then there is no vcall offset.  */
#define THUNK_VCALL_OFFSET(DECL) \
  (DECL_LANG_SPECIFIC (DECL)->decl_flags.u2.vcall_offset)

/* These macros provide convenient access to the various _STMT nodes
   created when parsing template declarations.  */
#define TRY_STMTS(NODE)         TREE_OPERAND (TRY_BLOCK_CHECK (NODE), 0)
#define TRY_HANDLERS(NODE)      TREE_OPERAND (TRY_BLOCK_CHECK (NODE), 1)

#define EH_SPEC_STMTS(NODE)     TREE_OPERAND (EH_SPEC_BLOCK_CHECK (NODE), 0)
#define EH_SPEC_RAISES(NODE)    TREE_OPERAND (EH_SPEC_BLOCK_CHECK (NODE), 1)

#define USING_STMT_NAMESPACE(NODE) TREE_OPERAND (USING_STMT_CHECK (NODE), 0)

/* Nonzero if this try block is a function try block.  */
#define FN_TRY_BLOCK_P(NODE)    TREE_LANG_FLAG_3 (TRY_BLOCK_CHECK (NODE))
#define HANDLER_PARMS(NODE)     TREE_OPERAND (HANDLER_CHECK (NODE), 0)
#define HANDLER_BODY(NODE)      TREE_OPERAND (HANDLER_CHECK (NODE), 1)
#define HANDLER_TYPE(NODE)	TREE_TYPE (HANDLER_CHECK (NODE))
#define SUBOBJECT_CLEANUP(NODE) TREE_OPERAND (SUBOBJECT_CHECK (NODE), 0)

/* Nonzero if this CTOR_STMT is for the beginning of a constructor.  */
#define CTOR_BEGIN_P(NODE) \
  (TREE_LANG_FLAG_0 (CTOR_STMT_CHECK (NODE)))

/* Nonzero if this CTOR_STMT is for the end of a constructor.  */
#define CTOR_END_P(NODE) \
  (!CTOR_BEGIN_P (NODE))

/* The parameters for a call-declarator.  */
#define CALL_DECLARATOR_PARMS(NODE) \
  (TREE_PURPOSE (TREE_OPERAND (NODE, 1)))

/* The cv-qualifiers for a call-declarator.  */
#define CALL_DECLARATOR_QUALS(NODE) \
  (TREE_VALUE (TREE_OPERAND (NODE, 1)))

/* The exception-specification for a call-declarator.  */
#define CALL_DECLARATOR_EXCEPTION_SPEC(NODE) \
  (TREE_TYPE (NODE))

/* An enumeration of the kind of tags that C++ accepts.  */
enum tag_types { record_type, class_type, union_type, enum_type };

/* The various kinds of lvalues we distinguish.  */
typedef enum cp_lvalue_kind {
  clk_none = 0,     /* Things that are not an lvalue.  */
  clk_ordinary = 1, /* An ordinary lvalue.  */
  clk_class = 2,    /* An rvalue of class-type.  */
  clk_bitfield = 4, /* An lvalue for a bit-field.  */
} cp_lvalue_kind;

/* The kinds of scopes we recognize.  */
typedef enum scope_kind {
  sk_template_parms, /* A scope for template parameters.  */
  sk_template_spec   /* A scope corresponding to a template
			specialization.  There is never anything in
			this scope.  */
} scope_kind;

/* Various kinds of template specialization, instantiation, etc.  */
typedef enum tmpl_spec_kind {
  tsk_none,                /* Not a template at all.  */
  tsk_invalid_member_spec, /* An explicit member template
			      specialization, but the enclosing
			      classes have not all been explicitly
			      specialized.  */
  tsk_invalid_expl_inst,   /* An explicit instantiation containing
			      template parameter lists.  */
  tsk_excessive_parms,     /* A template declaration with too many
			      template parameter lists.  */
  tsk_insufficient_parms,  /* A template declaration with too few
			      parameter lists.  */
  tsk_template,            /* A template declaration.  */
  tsk_expl_spec,           /* An explicit specialization.  */
  tsk_expl_inst            /* An explicit instantiation.  */
} tmpl_spec_kind;

/* The various kinds of access.  BINFO_ACCESS depends on these being
   two bit quantities.  The numerical values are important; they are
   used to initialize RTTI data structures, so changing them changes
   the ABI.  */
typedef enum access_kind {
  ak_none = 0,             /* Inaccessible.  */
  ak_public = 1,           /* Accessible, as a `public' thing.  */
  ak_protected = 2,        /* Accessible, as a `protected' thing.  */
  ak_private = 3           /* Accessible, as a `private' thing.  */
} access_kind;

/* The various kinds of special functions.  If you add to this list,
   you should update special_function_p as well.  */
typedef enum special_function_kind {
  sfk_none = 0,            /* Not a special function.  This enumeral
			      must have value zero; see
			      special_function_p.  */
  sfk_constructor,         /* A constructor.  */
  sfk_copy_constructor,    /* A copy constructor.  */
  sfk_assignment_operator, /* An assignment operator.  */
  sfk_destructor,          /* A destructor.  */
  sfk_complete_destructor, /* A destructor for complete objects.  */
  sfk_base_destructor,     /* A destructor for base subobjects.  */
  sfk_deleting_destructor, /* A destructor for complete objects that
			      deletes the object after it has been
			      destroyed.  */
  sfk_conversion           /* A conversion operator.  */
} special_function_kind;

/* The various kinds of linkage.  From [basic.link], 
   
      A name is said to have linkage when it might denote the same
      object, reference, function, type, template, namespace or value
      as a name introduced in another scope:

      -- When a name has external linkage, the entity it denotes can
         be referred to from scopes of other translation units or from
	 other scopes of the same translation unit.

      -- When a name has internal linkage, the entity it denotes can
         be referred to by names from other scopes in the same
	 translation unit.

      -- When a name has no linkage, the entity it denotes cannot be
         referred to by names from other scopes.  */

typedef enum linkage_kind {
  lk_none,                 /* No linkage.  */
  lk_internal,             /* Internal linkage.  */
  lk_external              /* External linkage.  */
} linkage_kind;

/* Bitmask flags to control type substitution.  */
typedef enum tsubst_flags_t {
  tf_none = 0,               /* nothing special */
  tf_error = 1 << 0,         /* give error messages  */
  tf_warning = 1 << 1,       /* give warnings too  */
  tf_no_attributes = 1 << 2, /* ignore attributes on comparisons
				(instantiate_type use) */
  tf_ignore_bad_quals = 1 << 3, /* ignore bad cvr qualifiers */
  tf_keep_type_decl = 1 << 4,	/* retain typedef type decls
				   (make_typename_type use) */
  tf_ptrmem_ok = 1 << 5      /* pointers to member ok (internal
				instantiate_type use) */
} tsubst_flags_t;

/* The kind of checking we can do looking in a class hierarchy. */
typedef enum base_access {
  ba_any = 0,      /* Do not check access, allow an ambiguous base,
		      prefer a non-virtual base */
  ba_ignore = 1,   /* Do not check access */
  ba_check = 2,    /* Check access */
  ba_not_special = 3, /* Do not consider special privilege
		         current_class_type might give. */
  ba_quiet = 4,    /* Do not issue error messages (bit mask).  */
} base_access;

/* The kind of base we can find, looking in a class hierarchy.
   Values <0 indicate we failed. */
typedef enum base_kind {
  bk_inaccessible = -3,   /* The base is inaccessible */
  bk_ambig = -2,          /* The base is ambiguous */
  bk_not_base = -1,       /* It is not a base */
  bk_same_type = 0,       /* It is the same type */
  bk_proper_base = 1,     /* It is a proper base */
  bk_via_virtual = 2      /* It is a proper base, but via a virtual
			     path. This might not be the canonical
			     binfo. */
} base_kind;

/* Nonzero means allow Microsoft extensions without a pedwarn.  */
extern int flag_ms_extensions;

/* Non-zero means warn in function declared in derived class has the
   same name as a virtual in the base class, but fails to match the
   type signature of any virtual function in the base class.  */
extern int warn_overloaded_virtual;

/* Nonzero means warn about use of multicharacter literals.  */
extern int warn_multichar;

/* Set by add_implicitly_declared_members() to keep those members from
   being flagged as deprecated or reported as using deprecated
   types.  */
extern int adding_implicit_members;

/* Non-zero means warn if a non-templatized friend function is
   declared in a templatized class. This behavior is warned about with
   flag_guiding_decls in do_friend. */
extern int warn_nontemplate_friend;

/* in decl{2}.c */
/* A node that is a list (length 1) of error_mark_nodes.  */
extern tree error_mark_list;

/* Node for "pointer to (virtual) function".
   This may be distinct from ptr_type_node so gdb can distinguish them.  */
#define vfunc_ptr_type_node  vtable_entry_type


/* For building calls to `delete'.  */
extern tree integer_two_node, integer_three_node;

extern tree anonymous_namespace_name;

/* The number of function bodies which we are currently processing.
   (Zero if we are at namespace scope, one inside the body of a
   function, two inside the body of a function in a local class, etc.)  */
extern int function_depth;

/* in pt.c  */

/* These values are used for the `STRICT' parameter to type_unification and
   fn_type_unification.  Their meanings are described with the
   documentation for fn_type_unification.  */

typedef enum unification_kind_t {
  DEDUCE_CALL,
  DEDUCE_CONV,
  DEDUCE_EXACT,
  DEDUCE_ORDER
} unification_kind_t;

/* Macros for operating on a template instantiation level node, represented
   by an EXPR_WITH_FILE_LOCATION.  */

#define TINST_DECL(NODE) EXPR_WFL_NODE (NODE)
#define TINST_LINE(NODE) EXPR_WFL_LINENO (NODE)
#define TINST_FILE(NODE) EXPR_WFL_FILENAME (NODE)

/* in class.c */

extern int current_class_depth;

/* Points to the name of that function. May not be the DECL_NAME
   of CURRENT_FUNCTION_DECL due to overloading */
extern tree original_function_name;

/* An array of all local classes present in this translation unit, in
   declaration order.  */
extern varray_type local_classes;

/* Here's where we control how name mangling takes place.  */

/* Cannot use '$' up front, because this confuses gdb
   (names beginning with '$' are gdb-local identifiers).

   Note that all forms in which the '$' is significant are long enough
   for direct indexing (meaning that if we know there is a '$'
   at a particular location, we can index into the string at
   any other location that provides distinguishing characters).  */

/* Define NO_DOLLAR_IN_LABEL in your favorite tm file if your assembler
   doesn't allow '$' in symbol names.  */
#ifndef NO_DOLLAR_IN_LABEL

#define JOINER '$'

#define VPTR_NAME "$v"
#define THROW_NAME "$eh_throw"
#define AUTO_VTABLE_NAME "__vtbl$me__"
#define AUTO_TEMP_NAME "_$tmp_"
#define AUTO_TEMP_FORMAT "_$tmp_%d"
#define VTABLE_BASE "$vb"
#define VTABLE_NAME_PREFIX "__vt_"
#define VFIELD_BASE "$vf"
#define VFIELD_NAME "_vptr$"
#define VFIELD_NAME_FORMAT "_vptr$%s"
#define STATIC_NAME_FORMAT "_%s$%s"
#define ANON_AGGRNAME_FORMAT "$_%d"

#else /* NO_DOLLAR_IN_LABEL */

#ifndef NO_DOT_IN_LABEL

#define JOINER '.'

#define VPTR_NAME ".v"
#define THROW_NAME ".eh_throw"
#define AUTO_VTABLE_NAME "__vtbl.me__"
#define AUTO_TEMP_NAME "_.tmp_"
#define AUTO_TEMP_FORMAT "_.tmp_%d"
#define VTABLE_BASE ".vb"
#define VTABLE_NAME_PREFIX "__vt_"
#define VFIELD_BASE ".vf"
#define VFIELD_NAME "_vptr."
#define VFIELD_NAME_FORMAT "_vptr.%s"
#define STATIC_NAME_FORMAT "_%s.%s"

#define ANON_AGGRNAME_FORMAT "._%d"

#else /* NO_DOT_IN_LABEL */

#define VPTR_NAME "__vptr"
#define VPTR_NAME_P(ID_NODE) \
  (!strncmp (IDENTIFIER_POINTER (ID_NODE), VPTR_NAME, sizeof (VPTR_NAME) - 1))
#define THROW_NAME "__eh_throw"
#define IN_CHARGE_NAME "__in_chrg"
#define AUTO_VTABLE_NAME "__vtbl_me__"
#define AUTO_TEMP_NAME "__tmp_"
#define TEMP_NAME_P(ID_NODE) \
  (!strncmp (IDENTIFIER_POINTER (ID_NODE), AUTO_TEMP_NAME, \
	     sizeof (AUTO_TEMP_NAME) - 1))
#define AUTO_TEMP_FORMAT "__tmp_%d"
#define VTABLE_BASE "__vtb"
#define VTABLE_NAME "__vt_"
#define VTABLE_NAME_PREFIX "__vt_"
#define VTABLE_NAME_P(ID_NODE) \
  (!strncmp (IDENTIFIER_POINTER (ID_NODE), VTABLE_NAME, \
	     sizeof (VTABLE_NAME) - 1))
#define VFIELD_BASE "__vfb"
#define VFIELD_NAME "__vptr_"
#define VFIELD_NAME_P(ID_NODE) \
  (!strncmp (IDENTIFIER_POINTER (ID_NODE), VFIELD_NAME, \
	    sizeof (VFIELD_NAME) - 1))
#define VFIELD_NAME_FORMAT "__vptr_%s"
#define STATIC_NAME_FORMAT "__static_%s_%s"

#define ANON_AGGRNAME_PREFIX "__anon_"
#define ANON_AGGRNAME_P(ID_NODE) \
  (!strncmp (IDENTIFIER_POINTER (ID_NODE), ANON_AGGRNAME_PREFIX, \
	     sizeof (ANON_AGGRNAME_PREFIX) - 1))
#define ANON_AGGRNAME_FORMAT "__anon_%d"

#endif	/* NO_DOT_IN_LABEL */
#endif	/* NO_DOLLAR_IN_LABEL */

#define THIS_NAME "this"
#define FILE_FUNCTION_PREFIX_LEN 9
#define CTOR_NAME "__ct"
#define DTOR_NAME "__dt"

#define IN_CHARGE_NAME "__in_chrg"

#define VTBL_PTR_TYPE		"__vtbl_ptr_type"
#define VTABLE_DELTA_NAME	"__delta"
#define VTABLE_PFN_NAME		"__pfn"

#define EXCEPTION_CLEANUP_NAME	"exception cleanup"

#if !defined(NO_DOLLAR_IN_LABEL) || !defined(NO_DOT_IN_LABEL)

#define VPTR_NAME_P(ID_NODE) (IDENTIFIER_POINTER (ID_NODE)[0] == JOINER \
			      && IDENTIFIER_POINTER (ID_NODE)[1] == 'v')

#define VTABLE_NAME_P(ID_NODE) (IDENTIFIER_POINTER (ID_NODE)[1] == 'v' \
  && IDENTIFIER_POINTER (ID_NODE)[2] == 't' \
  && IDENTIFIER_POINTER (ID_NODE)[3] == JOINER)

#define TEMP_NAME_P(ID_NODE) \
  (!strncmp (IDENTIFIER_POINTER (ID_NODE), AUTO_TEMP_NAME, sizeof (AUTO_TEMP_NAME)-1))
#define VFIELD_NAME_P(ID_NODE) \
  (!strncmp (IDENTIFIER_POINTER (ID_NODE), VFIELD_NAME, sizeof(VFIELD_NAME)-1))

/* For anonymous aggregate types, we need some sort of name to
   hold on to.  In practice, this should not appear, but it should
   not be harmful if it does.  */
#define ANON_AGGRNAME_P(ID_NODE) (IDENTIFIER_POINTER (ID_NODE)[0] == JOINER \
				  && IDENTIFIER_POINTER (ID_NODE)[1] == '_')
#endif /* !defined(NO_DOLLAR_IN_LABEL) || !defined(NO_DOT_IN_LABEL) */

/* Returns non-zero iff NODE is a declaration for the global function
   `main'.  */
#define DECL_MAIN_P(NODE)				\
   (DECL_EXTERN_C_FUNCTION_P (NODE)                     \
    && DECL_NAME (NODE) != NULL_TREE			\
    && MAIN_NAME_P (DECL_NAME (NODE)))


/* Things for handling inline functions.  */

/* Nonzero means do emit exported implementations of functions even if
   they can be inlined.  */

extern int flag_implement_inlines;

/* Nonzero means templates obey #pragma interface and implementation.  */

extern int flag_external_templates;

/* Nonzero means templates are emitted where they are instantiated.  */

extern int flag_alt_external_templates;

/* Nonzero means implicit template instantiations are emitted.  */

extern int flag_implicit_templates;

/* Nonzero if we want to emit defined symbols with common-like linkage as
   weak symbols where possible, in order to conform to C++ semantics.
   Otherwise, emit them as local symbols.  */

extern int flag_weak;

/* Nonzero if we're done parsing and into end-of-file activities.  */

extern int at_eof;

/* Functions called along with real static constructors and destructors.  */

extern tree static_ctors;
extern tree static_dtors;

enum overload_flags { NO_SPECIAL = 0, DTOR_FLAG, OP_FLAG, TYPENAME_FLAG };

/* Some macros for char-based bitfields.  */
#define B_SET(A,X) ((A)[(X)>>3] |=  (1 << ((X)&7)))
#define B_CLR(A,X) ((A)[(X)>>3] &= ~(1 << ((X)&7)))
#define B_TST(A,X) ((A)[(X)>>3] &   (1 << ((X)&7)))

/* These are uses as bits in flags passed to build_method_call
   to control its error reporting behavior.

   LOOKUP_PROTECT means flag access violations.
   LOOKUP_COMPLAIN mean complain if no suitable member function
     matching the arguments is found.
   LOOKUP_NORMAL is just a combination of these two.
   LOOKUP_NONVIRTUAL means make a direct call to the member function found
   LOOKUP_GLOBAL means search through the space of overloaded functions,
     as well as the space of member functions.
   LOOKUP_ONLYCONVERTING means that non-conversion constructors are not tried.
   DIRECT_BIND means that if a temporary is created, it should be created so
     that it lives as long as the current variable bindings; otherwise it
     only lives until the end of the complete-expression.  It also forces
     direct-initialization in cases where other parts of the compiler have
     already generated a temporary, such as reference initialization and the
     catch parameter.
   LOOKUP_SPECULATIVELY means return NULL_TREE if we cannot find what we are
     after.  Note, LOOKUP_COMPLAIN is checked and error messages printed
     before LOOKUP_SPECULATIVELY is checked.
   LOOKUP_NO_CONVERSION means that user-defined conversions are not
     permitted.  Built-in conversions are permitted.
   LOOKUP_DESTRUCTOR means explicit call to destructor.
   LOOKUP_NO_TEMP_BIND means temporaries will not be bound to references.

   These are used in global lookup to support elaborated types and
   qualifiers.

   LOOKUP_PREFER_TYPES means not to accept objects, and possibly namespaces.
   LOOKUP_PREFER_NAMESPACES means not to accept objects, and possibly types.
   LOOKUP_PREFER_BOTH means class-or-namespace-name.
   LOOKUP_TEMPLATES_EXPECTED means that class templates also count
     as types.  */

#define LOOKUP_PROTECT (1)
#define LOOKUP_COMPLAIN (2)
#define LOOKUP_NORMAL (3)
#define LOOKUP_NONVIRTUAL (8)
#define LOOKUP_GLOBAL (16)
#define LOOKUP_SPECULATIVELY (64)
#define LOOKUP_ONLYCONVERTING (128)
#define DIRECT_BIND (256)
#define LOOKUP_NO_CONVERSION (512)
#define LOOKUP_DESTRUCTOR (512)
#define LOOKUP_NO_TEMP_BIND (1024)
#define LOOKUP_PREFER_TYPES (2048)
#define LOOKUP_PREFER_NAMESPACES (4096)
#define LOOKUP_PREFER_BOTH (6144)
#define LOOKUP_TEMPLATES_EXPECTED (8192)

#define LOOKUP_NAMESPACES_ONLY(F)  \
  (((F) & LOOKUP_PREFER_NAMESPACES) && !((F) & LOOKUP_PREFER_TYPES))
#define LOOKUP_TYPES_ONLY(F)  \
  (!((F) & LOOKUP_PREFER_NAMESPACES) && ((F) & LOOKUP_PREFER_TYPES))
#define LOOKUP_QUALIFIERS_ONLY(F)     ((F) & LOOKUP_PREFER_BOTH)


/* These flags are used by the conversion code.
   CONV_IMPLICIT   :  Perform implicit conversions (standard and user-defined).
   CONV_STATIC     :  Perform the explicit conversions for static_cast.
   CONV_CONST      :  Perform the explicit conversions for const_cast.
   CONV_REINTERPRET:  Perform the explicit conversions for reinterpret_cast.
   CONV_PRIVATE    :  Perform upcasts to private bases.
   CONV_FORCE_TEMP :  Require a new temporary when converting to the same
		      aggregate type.  */

#define CONV_IMPLICIT    1
#define CONV_STATIC      2
#define CONV_CONST       4
#define CONV_REINTERPRET 8
#define CONV_PRIVATE	 16
/* #define CONV_NONCONVERTING 32 */
#define CONV_FORCE_TEMP  64
#define CONV_STATIC_CAST (CONV_IMPLICIT | CONV_STATIC | CONV_FORCE_TEMP)
#define CONV_OLD_CONVERT (CONV_IMPLICIT | CONV_STATIC | CONV_CONST \
			  | CONV_REINTERPRET)
#define CONV_C_CAST      (CONV_IMPLICIT | CONV_STATIC | CONV_CONST \
			  | CONV_REINTERPRET | CONV_PRIVATE | CONV_FORCE_TEMP)

/* Used by build_expr_type_conversion to indicate which types are
   acceptable as arguments to the expression under consideration.  */

#define WANT_INT	1 /* integer types, including bool */
#define WANT_FLOAT	2 /* floating point types */
#define WANT_ENUM	4 /* enumerated types */
#define WANT_POINTER	8 /* pointer types */
#define WANT_NULL      16 /* null pointer constant */
#define WANT_ARITH	(WANT_INT | WANT_FLOAT)

/* Used with comptypes, and related functions, to guide type
   comparison.  */

#define COMPARE_STRICT        0 /* Just check if the types are the
				   same.  */
#define COMPARE_BASE          1 /* Check to see if the second type is
				   derived from the first, or if both
				   are pointers (or references) and
				   the types pointed to by the second
				   type is derived from the pointed to
				   by the first.  */
#define COMPARE_RELAXED       2 /* Like COMPARE_DERIVED, but in
				   reverse.  Also treat enmeration
				   types as the same as integer types
				   of the same width.  */
#define COMPARE_REDECLARATION 4 /* The comparsion is being done when
				   another declaration of an existing
				   entity is seen.  */
#define COMPARE_NO_ATTRIBUTES 8 /* The comparison should ignore
				   extra-linguistic type attributes.  */

/* Used with push_overloaded_decl.  */
#define PUSH_GLOBAL          0  /* Push the DECL into namespace scope,
				   regardless of the current scope.  */
#define PUSH_LOCAL           1  /* Push the DECL into the current
				   scope.  */
#define PUSH_USING           2  /* We are pushing this DECL as the
				   result of a using declaration.  */

/* Used with start function.  */
#define SF_DEFAULT           0  /* No flags.  */
#define SF_PRE_PARSED        1  /* The function declaration has
				   already been parsed.  */
#define SF_INCLASS_INLINE    2  /* The function is an inline, defined
				   in the class body.  */

/* Returns nonzero iff TYPE1 and TYPE2 are the same type, or if TYPE2
   is derived from TYPE1, or if TYPE2 is a pointer (reference) to a
   class derived from the type pointed to (referred to) by TYPE1.  */
#define same_or_base_type_p(TYPE1, TYPE2) \
  comptypes ((TYPE1), (TYPE2), COMPARE_BASE)

/* These macros are used to access a TEMPLATE_PARM_INDEX.  */
#define TEMPLATE_PARM_INDEX_CAST(NODE) \
	((template_parm_index*)TEMPLATE_PARM_INDEX_CHECK (NODE))
#define TEMPLATE_PARM_IDX(NODE) (TEMPLATE_PARM_INDEX_CAST (NODE)->index)
#define TEMPLATE_PARM_LEVEL(NODE) (TEMPLATE_PARM_INDEX_CAST (NODE)->level)
#define TEMPLATE_PARM_DESCENDANTS(NODE) (TREE_CHAIN (NODE))
#define TEMPLATE_PARM_ORIG_LEVEL(NODE) (TEMPLATE_PARM_INDEX_CAST (NODE)->orig_level)
#define TEMPLATE_PARM_DECL(NODE) (TEMPLATE_PARM_INDEX_CAST (NODE)->decl)

/* These macros are for accessing the fields of TEMPLATE_TYPE_PARM,
   TEMPLATE_TEMPLATE_PARM and BOUND_TEMPLATE_TEMPLATE_PARM nodes.  */
#define TEMPLATE_TYPE_PARM_INDEX(NODE) (TYPE_FIELDS (NODE))
#define TEMPLATE_TYPE_IDX(NODE) \
  (TEMPLATE_PARM_IDX (TEMPLATE_TYPE_PARM_INDEX (NODE)))
#define TEMPLATE_TYPE_LEVEL(NODE) \
  (TEMPLATE_PARM_LEVEL (TEMPLATE_TYPE_PARM_INDEX (NODE)))
#define TEMPLATE_TYPE_ORIG_LEVEL(NODE) \
  (TEMPLATE_PARM_ORIG_LEVEL (TEMPLATE_TYPE_PARM_INDEX (NODE)))
#define TEMPLATE_TYPE_DECL(NODE) \
  (TEMPLATE_PARM_DECL (TEMPLATE_TYPE_PARM_INDEX (NODE)))

/* These constants can used as bit flags in the process of tree formatting.

   TFF_PLAIN_IDENTIFIER: unqualified part of a name.
   TFF_SCOPE: include the class and namespace scope of the name.
   TFF_CHASE_TYPEDEF: print the original type-id instead of the typedef-name.
   TFF_DECL_SPECIFIERS: print decl-specifiers.
   TFF_CLASS_KEY_OR_ENUM: precede a class-type name (resp. enum name) with
       a class-key (resp. `enum').
   TFF_RETURN_TYPE: include function return type.
   TFF_FUNCTION_DEFAULT_ARGUMENTS: include function default parameter values.
   TFF_EXCEPTION_SPECIFICATION: show function exception specification.
   TFF_TEMPLATE_HEADER: show the template<...> header in a
       template-declaration.
   TFF_TEMPLATE_NAME: show only template-name.
   TFF_EXPR_IN_PARENS: Parenthesize expressions.  */

#define TFF_PLAIN_IDENTIFIER               (0)
#define TFF_SCOPE                	   (1)
#define TFF_CHASE_TYPEDEF                  (1 << 1)
#define TFF_DECL_SPECIFIERS                (1 << 2)
#define TFF_CLASS_KEY_OR_ENUM              (1 << 3)
#define TFF_RETURN_TYPE                    (1 << 4)
#define TFF_FUNCTION_DEFAULT_ARGUMENTS     (1 << 5)
#define TFF_EXCEPTION_SPECIFICATION        (1 << 6)
#define TFF_TEMPLATE_HEADER                (1 << 7)
#define TFF_TEMPLATE_NAME                  (1 << 8)
#define TFF_EXPR_IN_PARENS                 (1 << 9)

/* Returns the TEMPLATE_DECL associated to a TEMPLATE_TEMPLATE_PARM
   node.  */
#define TEMPLATE_TEMPLATE_PARM_TEMPLATE_DECL(NODE)	\
  ((TREE_CODE (NODE) == BOUND_TEMPLATE_TEMPLATE_PARM)	\
   ? TYPE_TI_TEMPLATE (NODE)				\
   : TYPE_NAME (NODE))

/* in lex.c  */

extern void init_reswords PARAMS ((void));

/* Indexed by TREE_CODE, these tables give C-looking names to
   operators represented by TREE_CODES.  For example,
   opname_tab[(int) MINUS_EXPR] == "-".  */
extern const char **opname_tab, **assignop_tab;

typedef struct operator_name_info_t
{
  /* The IDENTIFIER_NODE for the operator.  */
  tree identifier;
  /* The name of the operator.  */
  const char *name;
  /* The mangled name of the operator.  */
  const char *mangled_name;
} operator_name_info_t;

/* A mapping from tree codes to operator name information.  */
extern operator_name_info_t operator_name_info[];
/* Similar, but for assignment operators.  */
extern operator_name_info_t assignment_operator_name_info[];

/* in call.c */
extern int check_dtor_name			PARAMS ((tree, tree));
extern int get_arglist_len_in_bytes		PARAMS ((tree));

extern tree build_vfield_ref			PARAMS ((tree, tree));
extern tree build_scoped_method_call		PARAMS ((tree, tree, tree, tree));
extern tree build_conditional_expr		PARAMS ((tree, tree, tree));
extern tree build_addr_func			PARAMS ((tree));
extern tree build_call				PARAMS ((tree, tree));
extern tree build_method_call			PARAMS ((tree, tree, tree, tree, int));
extern int null_ptr_cst_p			PARAMS ((tree));
extern int sufficient_parms_p                   PARAMS ((tree));
extern tree type_decays_to			PARAMS ((tree));
extern tree build_user_type_conversion		PARAMS ((tree, tree, int));
extern tree build_new_function_call		PARAMS ((tree, tree));
extern tree build_new_op			PARAMS ((enum tree_code, int, tree, tree, tree));
extern tree build_op_delete_call		PARAMS ((enum tree_code, tree, tree, int, tree));
extern int can_convert				PARAMS ((tree, tree));
extern int can_convert_arg			PARAMS ((tree, tree, tree));
extern int can_convert_arg_bad			PARAMS ((tree, tree, tree));
extern int enforce_access                       PARAMS ((tree, tree));
extern tree convert_default_arg                 PARAMS ((tree, tree, tree, int));
extern tree convert_arg_to_ellipsis             PARAMS ((tree));
extern tree build_x_va_arg                      PARAMS ((tree, tree));
extern tree convert_type_from_ellipsis          PARAMS ((tree));
extern int is_properly_derived_from             PARAMS ((tree, tree));
extern tree initialize_reference                PARAMS ((tree, tree));
extern tree strip_top_quals                     PARAMS ((tree));
extern tree perform_implicit_conversion         PARAMS ((tree, tree));

/* in class.c */
extern tree build_base_path			PARAMS ((enum tree_code, tree, tree, int));
extern tree build_vbase_path			PARAMS ((enum tree_code, tree, tree, tree, int));
extern tree build_vtbl_ref			PARAMS ((tree, tree));
extern tree build_vfn_ref			PARAMS ((tree, tree));
extern tree get_vtable_decl                     PARAMS ((tree, int));
extern void add_method				PARAMS ((tree, tree, int));
extern int currently_open_class			PARAMS ((tree));
extern tree currently_open_derived_class	PARAMS ((tree));
extern void duplicate_tag_error			PARAMS ((tree));
extern tree finish_struct			PARAMS ((tree, tree));
extern void finish_struct_1			PARAMS ((tree));
extern int resolves_to_fixed_type_p		PARAMS ((tree, int *));
extern void init_class_processing		PARAMS ((void));
extern int is_empty_class			PARAMS ((tree));
extern void pushclass				PARAMS ((tree, int));
extern void popclass				PARAMS ((void));
extern void push_nested_class			PARAMS ((tree, int));
extern void pop_nested_class			PARAMS ((void));
extern int current_lang_depth			PARAMS ((void));
extern void push_lang_context			PARAMS ((tree));
extern void pop_lang_context			PARAMS ((void));
extern tree instantiate_type			PARAMS ((tree, tree, tsubst_flags_t));
extern void print_class_statistics              PARAMS ((void));
extern void cxx_print_statistics		PARAMS ((void));
extern void cxx_print_xnode			PARAMS ((FILE *, tree, int));
extern void cxx_print_decl			PARAMS ((FILE *, tree, int));
extern void cxx_print_type			PARAMS ((FILE *, tree, int));
extern void cxx_print_identifier		PARAMS ((FILE *, tree, int));
extern void cxx_set_yydebug			PARAMS ((int));
extern void build_self_reference		PARAMS ((void));
extern int same_signature_p			PARAMS ((tree, tree));
extern void warn_hidden				PARAMS ((tree));
extern tree get_enclosing_class			PARAMS ((tree));
int is_base_of_enclosing_class			PARAMS ((tree, tree));
extern void unreverse_member_declarations       PARAMS ((tree));
extern void invalidate_class_lookup_cache       PARAMS ((void));
extern void maybe_note_name_used_in_class       PARAMS ((tree, tree));
extern void note_name_declared_in_class         PARAMS ((tree, tree));
extern tree get_vtbl_decl_for_binfo             PARAMS ((tree));
extern tree in_charge_arg_for_name              PARAMS ((tree));
extern tree get_vtt_name                        PARAMS ((tree));
extern tree get_primary_binfo                   PARAMS ((tree));

/* in cvt.c */
extern tree convert_to_reference		PARAMS ((tree, tree, int, int, tree));
extern tree convert_from_reference		PARAMS ((tree));
extern tree convert_lvalue			PARAMS ((tree, tree));
extern tree ocp_convert				PARAMS ((tree, tree, int, int));
extern tree cp_convert				PARAMS ((tree, tree));
extern tree convert_to_void			PARAMS ((tree, const char */*implicit context*/));
extern tree convert_force			PARAMS ((tree, tree, int));
extern tree build_type_conversion		PARAMS ((tree, tree, int));
extern tree build_expr_type_conversion		PARAMS ((int, tree, int));
extern tree type_promotes_to			PARAMS ((tree));
extern tree perform_qualification_conversions   PARAMS ((tree, tree));
extern void clone_function_decl                 PARAMS ((tree, int));
extern void adjust_clone_args			PARAMS ((tree));

/* decl.c */
/* resume_binding_level */
extern void cxx_init_decl_processing		PARAMS ((void));
extern int toplevel_bindings_p			PARAMS ((void));
extern int namespace_bindings_p			PARAMS ((void));
extern void keep_next_level			PARAMS ((int));
extern int kept_level_p				PARAMS ((void));
extern int template_parm_scope_p		PARAMS ((void));
extern void set_class_shadows			PARAMS ((tree));
extern void maybe_push_cleanup_level		PARAMS ((tree));
extern void begin_scope                         PARAMS ((scope_kind));
extern void finish_scope                        PARAMS ((void));
extern void note_level_for_for			PARAMS ((void));
extern void note_level_for_try			PARAMS ((void));
extern void note_level_for_catch		PARAMS ((void));
extern void resume_level			PARAMS ((struct binding_level *));
extern void delete_block			PARAMS ((tree));
extern void add_block_current_level		PARAMS ((tree));
extern void pushlevel_class			PARAMS ((void));
extern void poplevel_class                      PARAMS ((void));
extern void print_binding_stack			PARAMS ((void));
extern void print_binding_level			PARAMS ((struct binding_level *));
extern void push_namespace			PARAMS ((tree));
extern void pop_namespace			PARAMS ((void));
extern void push_nested_namespace		PARAMS ((tree));
extern void pop_nested_namespace		PARAMS ((tree));
extern void maybe_push_to_top_level		PARAMS ((int));
extern void push_to_top_level			PARAMS ((void));
extern void pop_from_top_level			PARAMS ((void));
extern void push_switch				PARAMS ((tree));
extern void pop_switch				PARAMS ((void));
extern tree identifier_type_value		PARAMS ((tree));
extern void set_identifier_type_value		PARAMS ((tree, tree));
extern void pop_everything			PARAMS ((void));
extern void pushtag				PARAMS ((tree, tree, int));
extern tree make_anon_name			PARAMS ((void));
extern void clear_anon_tags			PARAMS ((void));
extern int decls_match				PARAMS ((tree, tree));
extern int duplicate_decls			PARAMS ((tree, tree));
extern tree pushdecl_top_level			PARAMS ((tree));
extern void pushdecl_class_level		PARAMS ((tree));
extern tree pushdecl_namespace_level            PARAMS ((tree));
extern tree push_using_decl                     PARAMS ((tree, tree));
extern tree push_using_directive                PARAMS ((tree));
extern void push_class_level_binding		PARAMS ((tree, tree));
extern tree implicitly_declare			PARAMS ((tree));
extern tree declare_local_label                 PARAMS ((tree));
extern tree define_label			PARAMS ((const char *, int, tree));
extern void check_goto				PARAMS ((tree));
extern void define_case_label			PARAMS ((void));
extern tree binding_for_name                    PARAMS ((tree, tree));
extern tree namespace_binding                   PARAMS ((tree, tree));
extern void set_namespace_binding               PARAMS ((tree, tree, tree));
extern tree lookup_namespace_name		PARAMS ((tree, tree));
extern tree build_typename_type                 PARAMS ((tree, tree, tree, tree));
extern tree make_typename_type			PARAMS ((tree, tree, tsubst_flags_t));
extern tree make_unbound_class_template		PARAMS ((tree, tree, int));
extern tree lookup_name_nonclass		PARAMS ((tree));
extern tree lookup_function_nonclass            PARAMS ((tree, tree));
extern tree lookup_name				PARAMS ((tree, int));
extern tree lookup_name_current_level		PARAMS ((tree));
extern tree lookup_type_current_level		PARAMS ((tree));
extern tree lookup_name_namespace_only          PARAMS ((tree));
extern void begin_only_namespace_names          PARAMS ((void));
extern void end_only_namespace_names            PARAMS ((void));
extern tree namespace_ancestor			PARAMS ((tree, tree));
extern tree unqualified_namespace_lookup	PARAMS ((tree, int, tree *));
extern int  lookup_using_namespace              PARAMS ((tree, tree, tree, tree, int, tree *));
extern int  qualified_lookup_using_namespace    PARAMS ((tree, tree, tree, int));
extern tree build_library_fn			PARAMS ((tree, tree));
extern tree build_library_fn_ptr		PARAMS ((const char *, tree));
extern tree build_cp_library_fn_ptr		PARAMS ((const char *, tree));
extern tree push_library_fn			PARAMS ((tree, tree));
extern tree push_void_library_fn		PARAMS ((tree, tree));
extern tree push_throw_library_fn		PARAMS ((tree, tree));
extern int init_type_desc			PARAMS ((void));
extern tree check_tag_decl			PARAMS ((tree));
extern void shadow_tag				PARAMS ((tree));
extern tree groktypename			PARAMS ((tree));
extern tree start_decl				PARAMS ((tree, tree, int, tree, tree));
extern void start_decl_1			PARAMS ((tree));
extern void cp_finish_decl			PARAMS ((tree, tree, tree, int));
extern void finish_decl				PARAMS ((tree, tree, tree));
extern void maybe_inject_for_scope_var          PARAMS ((tree));
extern void initialize_local_var                PARAMS ((tree, tree, int));
extern void expand_static_init			PARAMS ((tree, tree));
extern tree start_handler_parms                 PARAMS ((tree, tree));
extern int complete_array_type			PARAMS ((tree, tree, int));
extern tree build_ptrmemfunc_type		PARAMS ((tree));
/* the grokdeclarator prototype is in decl.h */
extern int parmlist_is_exprlist			PARAMS ((tree));
extern int copy_fn_p				PARAMS ((tree));
extern void grok_special_member_properties	PARAMS ((tree));
extern int grok_ctor_properties			PARAMS ((tree, tree));
extern void grok_op_properties			PARAMS ((tree, int));
extern tree xref_tag				PARAMS ((tree, tree, int));
extern tree xref_tag_from_type			PARAMS ((tree, tree, int));
extern void xref_basetypes			PARAMS ((tree, tree, tree, tree));
extern tree start_enum				PARAMS ((tree));
extern void finish_enum				PARAMS ((tree));
extern void build_enumerator			PARAMS ((tree, tree, tree));
extern int start_function			PARAMS ((tree, tree, tree, int));
extern tree begin_function_body			PARAMS ((void));
extern void finish_function_body		PARAMS ((tree));
extern tree finish_function			PARAMS ((int));
extern tree start_method			PARAMS ((tree, tree, tree));
extern tree finish_method			PARAMS ((tree));
extern void maybe_register_incomplete_var       PARAMS ((tree));
extern void complete_vars			PARAMS ((tree));
extern void finish_stmt				PARAMS ((void));
extern void print_other_binding_stack		PARAMS ((struct binding_level *));
extern void revert_static_member_fn             PARAMS ((tree));
extern void fixup_anonymous_aggr                PARAMS ((tree));
extern int check_static_variable_definition     PARAMS ((tree, tree));
extern tree compute_array_index_type		PARAMS ((tree, tree));
extern void push_local_binding                  PARAMS ((tree, tree, int));
extern int push_class_binding                   PARAMS ((tree, tree));
extern tree check_default_argument              PARAMS ((tree, tree));
extern tree push_overloaded_decl		PARAMS ((tree, int));
extern void clear_identifier_class_values       PARAMS ((void));
extern void storetags                           PARAMS ((tree));
extern int vtable_decl_p                        PARAMS ((tree, void *));
extern int vtype_decl_p                         PARAMS ((tree, void *));
extern int sigtable_decl_p                      PARAMS ((tree, void *));
typedef int (*walk_globals_pred)                PARAMS ((tree, void *));
typedef int (*walk_globals_fn)                  PARAMS ((tree *, void *));
extern int walk_globals                         PARAMS ((walk_globals_pred,
						       walk_globals_fn,
						       void *));
typedef int (*walk_namespaces_fn)               PARAMS ((tree, void *));
extern int walk_namespaces                      PARAMS ((walk_namespaces_fn,
						       void *));
extern int wrapup_globals_for_namespace         PARAMS ((tree, void *));
extern tree cp_namespace_decls                  PARAMS ((tree));
extern tree create_implicit_typedef             PARAMS ((tree, tree));
extern tree maybe_push_decl                     PARAMS ((tree));
extern tree build_target_expr_with_type         PARAMS ((tree, tree));
extern int local_variable_p                     PARAMS ((tree));
extern int nonstatic_local_decl_p               PARAMS ((tree));
extern tree declare_global_var                  PARAMS ((tree, tree));
extern void register_dtor_fn                    PARAMS ((tree));
extern tmpl_spec_kind current_tmpl_spec_kind    PARAMS ((int));
extern tree cp_fname_init			PARAMS ((const char *));

/* in decl2.c */
extern void init_decl2				PARAMS ((void));
extern int check_java_method			PARAMS ((tree));
extern int cxx_decode_option			PARAMS ((int, char **));
extern int grok_method_quals			PARAMS ((tree, tree, tree));
extern void warn_if_unknown_interface		PARAMS ((tree));
extern void grok_x_components			PARAMS ((tree));
extern void maybe_retrofit_in_chrg		PARAMS ((tree));
extern void maybe_make_one_only			PARAMS ((tree));
extern void grokclassfn				PARAMS ((tree, tree, enum overload_flags, tree));
extern tree grok_array_decl			PARAMS ((tree, tree));
extern tree delete_sanity			PARAMS ((tree, tree, int, int));
extern tree check_classfn			PARAMS ((tree, tree));
extern void check_member_template               PARAMS ((tree));
extern tree grokfield				PARAMS ((tree, tree, tree, tree, tree));
extern tree grokbitfield			PARAMS ((tree, tree, tree));
extern tree groktypefield			PARAMS ((tree, tree));
extern tree grokoptypename			PARAMS ((tree, tree));
extern void cplus_decl_attributes		PARAMS ((tree *, tree, int));
extern tree constructor_name_full		PARAMS ((tree));
extern tree constructor_name			PARAMS ((tree));
extern void defer_fn            		PARAMS ((tree));
extern void finish_anon_union			PARAMS ((tree));
extern tree finish_table			PARAMS ((tree, tree, tree, int));
extern void finish_builtin_type			PARAMS ((tree, const char *,
						       tree *, int, tree));
extern tree coerce_new_type			PARAMS ((tree));
extern tree coerce_delete_type			PARAMS ((tree));
extern void comdat_linkage			PARAMS ((tree));
extern void import_export_vtable		PARAMS ((tree, tree, int));
extern void import_export_decl			PARAMS ((tree));
extern void import_export_tinfo			PARAMS ((tree, tree, int));
extern tree build_cleanup			PARAMS ((tree));
extern void finish_file				PARAMS ((void));
extern tree reparse_absdcl_as_expr		PARAMS ((tree, tree));
extern tree reparse_absdcl_as_casts		PARAMS ((tree, tree));
extern tree build_expr_from_tree		PARAMS ((tree));
extern tree reparse_decl_as_expr		PARAMS ((tree, tree));
extern tree finish_decl_parsing			PARAMS ((tree));
extern void set_decl_namespace                  PARAMS ((tree, tree, int));
extern tree current_decl_namespace              PARAMS ((void));
extern void push_decl_namespace                 PARAMS ((tree));
extern void pop_decl_namespace                  PARAMS ((void));
extern void push_scope				PARAMS ((tree));
extern void pop_scope				PARAMS ((tree));
extern void do_namespace_alias			PARAMS ((tree, tree));
extern void do_toplevel_using_decl		PARAMS ((tree));
extern void do_local_using_decl                 PARAMS ((tree));
extern tree do_class_using_decl			PARAMS ((tree));
extern void do_using_directive			PARAMS ((tree));
extern void check_default_args			PARAMS ((tree));
extern void mark_used				PARAMS ((tree));
extern tree handle_class_head			PARAMS ((tree, tree, tree, int, int *));
extern tree lookup_arg_dependent                PARAMS ((tree, tree, tree));
extern void finish_static_data_member_decl      PARAMS ((tree, tree, tree, int));
extern tree build_artificial_parm               PARAMS ((tree, tree));
extern tree get_guard                           PARAMS ((tree));
extern tree get_guard_cond                      PARAMS ((tree));
extern tree set_guard                           PARAMS ((tree));

/* in parse.y */
extern void cp_parse_init			PARAMS ((void));

extern void cp_error_at		PARAMS ((const char *msgid, ...));
extern void cp_warning_at	PARAMS ((const char *msgid, ...));
extern void cp_pedwarn_at	PARAMS ((const char *msgid, ...));

/* XXX Not i18n clean.  */
#define cp_deprecated(STR)							\
  do {										\
    if (warn_deprecated)							\
      warning ("%s is deprecated, please see the documentation for details",	\
	       (STR));								\
  } while (0)

/* in error.c */
extern void init_error				PARAMS ((void));
extern const char *type_as_string		PARAMS ((tree, int));
extern const char *decl_as_string		PARAMS ((tree, int));
extern const char *expr_as_string		PARAMS ((tree, int));
extern const char *context_as_string            PARAMS ((tree, int));
extern const char *lang_decl_name		PARAMS ((tree, int));
extern const char *cp_file_of			PARAMS ((tree));
extern int cp_line_of				PARAMS ((tree));
extern const char *language_to_string           PARAMS ((enum languages, int));
extern void print_instantiation_context         PARAMS ((void));

/* in except.c */
extern void init_exception_processing		PARAMS ((void));
extern tree expand_start_catch_block		PARAMS ((tree));
extern void expand_end_catch_block		PARAMS ((void));
extern void expand_builtin_throw		PARAMS ((void));
extern void expand_eh_spec_block	        PARAMS ((tree));
extern void expand_exception_blocks		PARAMS ((void));
extern tree build_exc_ptr			PARAMS ((void));
extern tree build_throw				PARAMS ((tree));
extern void mark_all_runtime_matches            PARAMS ((void));
extern int nothrow_libfn_p			PARAMS ((tree));
extern void check_handlers			PARAMS ((tree));
extern void choose_personality_routine		PARAMS ((enum languages));

/* in expr.c */
extern void init_cplus_expand			PARAMS ((void));
extern int extract_init				PARAMS ((tree, tree));
extern tree cplus_expand_constant               PARAMS ((tree));

/* friend.c */
extern int is_friend				PARAMS ((tree, tree));
extern void make_friend_class			PARAMS ((tree, tree));
extern void add_friend                          PARAMS ((tree, tree));
extern tree do_friend				PARAMS ((tree, tree, tree, tree, tree, enum overload_flags, tree, int));

/* in init.c */
extern void init_init_processing		PARAMS ((void));
extern void emit_base_init			PARAMS ((tree, tree));
extern tree expand_member_init			PARAMS ((tree, tree, tree));
extern tree build_aggr_init			PARAMS ((tree, tree, int));
extern int is_aggr_type				PARAMS ((tree, int));
extern tree get_aggr_from_typedef		PARAMS ((tree, int));
extern tree get_type_value			PARAMS ((tree));
extern tree build_forced_zero_init		PARAMS ((tree));
extern tree build_member_call			PARAMS ((tree, tree, tree));
extern tree build_offset_ref			PARAMS ((tree, tree));
extern tree resolve_offset_ref			PARAMS ((tree));
extern tree build_new				PARAMS ((tree, tree, tree, int));
extern tree build_vec_init			PARAMS ((tree, tree, int));
extern tree build_x_delete			PARAMS ((tree, int, tree));
extern tree build_delete			PARAMS ((tree, tree, special_function_kind, int, int));
extern void perform_base_cleanups		PARAMS ((void));
extern tree build_vbase_delete			PARAMS ((tree, tree));
extern tree build_vec_delete			PARAMS ((tree, tree, special_function_kind, int));
extern tree create_temporary_var                PARAMS ((tree));
extern void begin_init_stmts                    PARAMS ((tree *, tree *));
extern tree finish_init_stmts                   PARAMS ((tree, tree));
extern void initialize_vtbl_ptrs                PARAMS ((tree));
extern tree build_java_class_ref                PARAMS ((tree));

/* in input.c */

/* in lex.c */
extern tree make_pointer_declarator		PARAMS ((tree, tree));
extern tree make_reference_declarator		PARAMS ((tree, tree));
extern tree make_call_declarator		PARAMS ((tree, tree, tree, tree));
extern void set_quals_and_spec			PARAMS ((tree, tree, tree));
extern void print_parse_statistics		PARAMS ((void));
extern void do_pending_inlines			PARAMS ((void));
extern void process_next_inline			PARAMS ((struct unparsed_text *));

extern void yyungetc				PARAMS ((int, int));
extern void snarf_method			PARAMS ((tree));

extern void check_for_missing_semicolon		PARAMS ((tree));
extern void note_got_semicolon			PARAMS ((tree));
extern void note_list_got_semicolon		PARAMS ((tree));
extern void do_pending_lang_change		PARAMS ((void));
extern void see_typename			PARAMS ((void));
extern tree do_identifier			PARAMS ((tree, int, tree));
extern tree do_scoped_id			PARAMS ((tree, int));
extern tree identifier_typedecl_value		PARAMS ((tree));
extern tree build_lang_decl			PARAMS ((enum tree_code, tree, tree));
extern void retrofit_lang_decl			PARAMS ((tree));
extern tree copy_decl                           PARAMS ((tree));
extern tree copy_type                           PARAMS ((tree));
extern tree cp_make_lang_type			PARAMS ((enum tree_code));
extern tree make_aggr_type			PARAMS ((enum tree_code));
extern void compiler_error			PARAMS ((const char *, ...))
  ATTRIBUTE_PRINTF_1;
extern void yyerror				PARAMS ((const char *));
extern void clear_inline_text_obstack		PARAMS ((void));
extern void yyhook				PARAMS ((int));
extern int cp_type_qual_from_rid                PARAMS ((tree));
extern const char *cxx_init			PARAMS ((const char *));
extern void cxx_finish PARAMS ((void));
extern void cxx_init_options PARAMS ((void));

/* in method.c */
extern void init_method				PARAMS ((void));
extern void set_mangled_name_for_decl           PARAMS ((tree));
extern tree build_opfncall			PARAMS ((enum tree_code, int, tree, tree, tree));
extern tree hack_identifier			PARAMS ((tree, tree));
extern tree make_thunk				PARAMS ((tree, tree, tree));
extern void use_thunk				PARAMS ((tree, int));
extern void synthesize_method			PARAMS ((tree));
extern tree implicitly_declare_fn               PARAMS ((special_function_kind, tree, int));
extern tree skip_artificial_parms_for		PARAMS ((tree, tree));

/* In optimize.c */
extern void optimize_function                   PARAMS ((tree));
extern int calls_setjmp_p                       PARAMS ((tree));
extern int maybe_clone_body                     PARAMS ((tree));

/* in pt.c */
extern void init_pt                             PARAMS ((void));
extern void check_template_shadow		PARAMS ((tree));
extern tree get_innermost_template_args         PARAMS ((tree, int));
extern tree tsubst				PARAMS ((tree, tree, tsubst_flags_t, tree));
extern tree tsubst_expr				PARAMS ((tree, tree, tsubst_flags_t, tree));
extern tree tsubst_copy				PARAMS ((tree, tree, tsubst_flags_t, tree));
extern void maybe_begin_member_template_processing PARAMS ((tree));
extern void maybe_end_member_template_processing PARAMS ((void));
extern tree finish_member_template_decl         PARAMS ((tree));
extern void begin_template_parm_list		PARAMS ((void));
extern void begin_specialization                PARAMS ((void));
extern void reset_specialization                PARAMS ((void));
extern void end_specialization                  PARAMS ((void));
extern void begin_explicit_instantiation        PARAMS ((void));
extern void end_explicit_instantiation          PARAMS ((void));
extern tree check_explicit_specialization       PARAMS ((tree, tree, int, int));
extern tree process_template_parm		PARAMS ((tree, tree));
extern tree end_template_parm_list		PARAMS ((tree));
extern void end_template_decl			PARAMS ((void));
extern tree current_template_args		PARAMS ((void));
extern tree push_template_decl			PARAMS ((tree));
extern tree push_template_decl_real             PARAMS ((tree, int));
extern void redeclare_class_template            PARAMS ((tree, tree));
extern tree lookup_template_class		PARAMS ((tree, tree, tree, tree, int, tsubst_flags_t));
extern tree lookup_template_function            PARAMS ((tree, tree));
extern int uses_template_parms			PARAMS ((tree));
extern tree instantiate_class_template		PARAMS ((tree));
extern tree instantiate_template		PARAMS ((tree, tree));
extern int fn_type_unification                  PARAMS ((tree, tree, tree, tree, tree, unification_kind_t, int));
extern tree tinst_for_decl			PARAMS ((void));
extern void mark_decl_instantiated		PARAMS ((tree, int));
extern int more_specialized			PARAMS ((tree, tree, int, int));
extern void mark_class_instantiated		PARAMS ((tree, int));
extern void do_decl_instantiation		PARAMS ((tree, tree, tree));
extern void do_type_instantiation		PARAMS ((tree, tree, tsubst_flags_t));
extern tree instantiate_decl			PARAMS ((tree, int));
extern tree get_bindings			PARAMS ((tree, tree, tree));
extern int push_tinst_level			PARAMS ((tree));
extern void pop_tinst_level			PARAMS ((void));
extern int more_specialized_class		PARAMS ((tree, tree));
extern int is_member_template                   PARAMS ((tree));
extern int template_parms_equal                 PARAMS ((tree, tree));
extern int comp_template_parms                  PARAMS ((tree, tree));
extern int template_class_depth                 PARAMS ((tree));
extern int is_specialization_of                 PARAMS ((tree, tree));
extern int comp_template_args                   PARAMS ((tree, tree));
extern void maybe_process_partial_specialization PARAMS ((tree));
extern void maybe_check_template_type           PARAMS ((tree));
extern tree most_specialized_instantiation      PARAMS ((tree));
extern void print_candidates                    PARAMS ((tree));
extern int instantiate_pending_templates        PARAMS ((void));
extern tree tsubst_default_argument             PARAMS ((tree, tree, tree));
extern tree most_general_template		PARAMS ((tree));
extern tree get_mostly_instantiated_function_type PARAMS ((tree, tree *, tree *));
extern int problematic_instantiation_changed    PARAMS ((void));
extern void record_last_problematic_instantiation PARAMS ((void));
extern tree current_instantiation               PARAMS ((void));
extern int processing_template_parmlist;

/* in repo.c */
extern void repo_template_used			PARAMS ((tree));
extern void repo_template_instantiated		PARAMS ((tree, int));
extern void init_repo				PARAMS ((const char *));
extern void finish_repo				PARAMS ((void));

/* in rtti.c */
extern void init_rtti_processing		PARAMS((void));
extern tree build_typeid			PARAMS((tree));
extern tree get_tinfo_decl                      PARAMS((tree));
extern tree get_typeid				PARAMS((tree));
extern tree build_dynamic_cast			PARAMS((tree, tree));
extern void emit_support_tinfos                 PARAMS((void));
extern int unemitted_tinfo_decl_p    	        PARAMS((tree, void *));
extern int emit_tinfo_decl                      PARAMS((tree *, void *));

/* in search.c */
extern tree lookup_base PARAMS ((tree, tree, base_access, base_kind *));
extern int types_overlap_p			PARAMS ((tree, tree));
extern tree get_vbase				PARAMS ((tree, tree));
extern tree get_dynamic_cast_base_type          PARAMS ((tree, tree));
extern void type_access_control			PARAMS ((tree, tree));
extern int accessible_p                         PARAMS ((tree, tree));
extern tree lookup_field			PARAMS ((tree, tree, int, int));
extern tree lookup_nested_field			PARAMS ((tree, int));
extern int lookup_fnfields_1                    PARAMS ((tree, tree));
extern tree lookup_fnfields			PARAMS ((tree, tree, int));
extern tree lookup_member			PARAMS ((tree, tree, int, int));
extern int look_for_overrides			PARAMS ((tree, tree));
extern void get_pure_virtuals		        PARAMS ((tree));
extern void get_vbase_types			PARAMS ((tree));
extern void maybe_suppress_debug_info		PARAMS ((tree));
extern void note_debug_info_needed		PARAMS ((tree));
extern void push_class_decls			PARAMS ((tree));
extern void pop_class_decls			PARAMS ((void));
extern void unuse_fields			PARAMS ((tree));
extern void print_search_statistics		PARAMS ((void));
extern void init_search_processing		PARAMS ((void));
extern void reinit_search_statistics		PARAMS ((void));
extern tree current_scope			PARAMS ((void));
extern int at_function_scope_p                  PARAMS ((void));
extern tree context_for_name_lookup		PARAMS ((tree));
extern tree lookup_conversions			PARAMS ((tree));
extern tree binfo_for_vtable			PARAMS ((tree));
extern tree binfo_from_vbase			PARAMS ((tree));
extern tree look_for_overrides_here		PARAMS ((tree, tree));
extern int check_final_overrider		PARAMS ((tree, tree));
extern tree dfs_walk                            PARAMS ((tree,
						       tree (*) (tree, void *),
						       tree (*) (tree, void *),
						       void *));
extern tree dfs_walk_real                      PARAMS ((tree,
						       tree (*) (tree, void *),
						       tree (*) (tree, void *),
						       tree (*) (tree, void *),
						       void *));
extern tree dfs_unmark                          PARAMS ((tree, void *));
extern tree markedp                             PARAMS ((tree, void *));
extern tree unmarkedp                           PARAMS ((tree, void *));
extern tree dfs_unmarked_real_bases_queue_p     PARAMS ((tree, void *));
extern tree dfs_marked_real_bases_queue_p       PARAMS ((tree, void *));
extern tree dfs_skip_vbases                     PARAMS ((tree, void *));
extern tree marked_vtable_pathp                 PARAMS ((tree, void *));
extern tree unmarked_vtable_pathp               PARAMS ((tree, void *));
extern tree find_vbase_instance                 PARAMS ((tree, tree));
extern tree binfo_for_vbase                     PARAMS ((tree, tree));
extern tree binfo_via_virtual                   PARAMS ((tree, tree));

/* in semantics.c */
extern void init_cp_semantics                   PARAMS ((void));
extern tree finish_expr_stmt                    PARAMS ((tree));
extern tree begin_if_stmt                       PARAMS ((void));
extern void finish_if_stmt_cond                 PARAMS ((tree, tree));
extern tree finish_then_clause                  PARAMS ((tree));
extern void begin_else_clause                   PARAMS ((void));
extern void finish_else_clause                  PARAMS ((tree));
extern void finish_if_stmt                      PARAMS ((void));
extern tree begin_while_stmt                    PARAMS ((void));
extern void finish_while_stmt_cond              PARAMS ((tree, tree));
extern void finish_while_stmt                   PARAMS ((tree));
extern tree begin_do_stmt                       PARAMS ((void));
extern void finish_do_body                      PARAMS ((tree));
extern void finish_do_stmt                      PARAMS ((tree, tree));
extern tree finish_return_stmt                  PARAMS ((tree));
extern tree begin_for_stmt                      PARAMS ((void));
extern void finish_for_init_stmt                PARAMS ((tree));
extern void finish_for_cond                     PARAMS ((tree, tree));
extern void finish_for_expr                     PARAMS ((tree, tree));
extern void finish_for_stmt                     PARAMS ((tree));
extern tree finish_break_stmt                   PARAMS ((void));
extern tree finish_continue_stmt                PARAMS ((void));
extern tree begin_switch_stmt                   PARAMS ((void));
extern void finish_switch_cond                  PARAMS ((tree, tree));
extern void finish_switch_stmt                  PARAMS ((tree));
extern tree finish_case_label                   PARAMS ((tree, tree));
extern tree finish_goto_stmt                    PARAMS ((tree));
extern tree begin_try_block                     PARAMS ((void));
extern void finish_try_block                    PARAMS ((tree));
extern tree begin_eh_spec_block			PARAMS ((void));
extern void finish_eh_spec_block		PARAMS ((tree, tree));
extern void finish_handler_sequence             PARAMS ((tree));
extern tree begin_function_try_block            PARAMS ((void));
extern void finish_function_try_block           PARAMS ((tree));
extern void finish_function_handler_sequence    PARAMS ((tree));
extern void finish_cleanup_try_block            PARAMS ((tree));
extern tree begin_handler                       PARAMS ((void));
extern void finish_handler_parms                PARAMS ((tree, tree));
extern void begin_catch_block                   PARAMS ((tree));
extern void finish_handler                      PARAMS ((tree));
extern void finish_cleanup                      PARAMS ((tree, tree));
extern tree begin_compound_stmt                 PARAMS ((int));
extern tree finish_compound_stmt                PARAMS ((int, tree));
extern tree finish_asm_stmt                     PARAMS ((tree, tree, tree, tree, tree));
extern void finish_label_stmt                   PARAMS ((tree));
extern void finish_label_decl                   PARAMS ((tree));
extern void finish_subobject                    PARAMS ((tree));
extern tree finish_parenthesized_expr           PARAMS ((tree));
extern tree begin_stmt_expr                     PARAMS ((void));
extern tree finish_stmt_expr                    PARAMS ((tree));
extern tree finish_call_expr                    PARAMS ((tree, tree, int));
extern tree finish_increment_expr               PARAMS ((tree, enum tree_code));
extern tree finish_this_expr                    PARAMS ((void));
extern tree finish_object_call_expr             PARAMS ((tree, tree, tree));
extern tree finish_qualified_object_call_expr   PARAMS ((tree, tree, tree));
extern tree finish_pseudo_destructor_call_expr  PARAMS ((tree, tree, tree));
extern tree finish_qualified_call_expr          PARAMS ((tree, tree));
extern tree finish_unary_op_expr                PARAMS ((enum tree_code, tree));
extern tree finish_id_expr                      PARAMS ((tree));
extern void save_type_access_control		PARAMS ((tree));
extern void reset_type_access_control           PARAMS ((void));
extern void decl_type_access_control		PARAMS ((tree));
extern int begin_function_definition            PARAMS ((tree, tree));
extern tree begin_constructor_declarator        PARAMS ((tree, tree));
extern tree finish_declarator                   PARAMS ((tree, tree, tree, tree, int));
extern void finish_translation_unit             PARAMS ((void));
extern tree finish_template_type_parm           PARAMS ((tree, tree));
extern tree finish_template_template_parm       PARAMS ((tree, tree));
extern tree finish_parmlist                     PARAMS ((tree, int));
extern tree begin_class_definition              PARAMS ((tree));
extern tree finish_class_definition             PARAMS ((tree, tree, int, int));
extern void finish_default_args                 PARAMS ((void));
extern void begin_inline_definitions            PARAMS ((void));
extern void finish_inline_definitions           PARAMS ((void));
extern tree finish_member_class_template        PARAMS ((tree));
extern void finish_template_decl                PARAMS ((tree));
extern tree finish_template_type                PARAMS ((tree, tree, int));
extern void enter_scope_of                      PARAMS ((tree));
extern tree finish_base_specifier               PARAMS ((tree, tree));
extern void finish_member_declaration           PARAMS ((tree));
extern void check_multiple_declarators          PARAMS ((void));
extern tree finish_typeof			PARAMS ((tree));
extern tree finish_sizeof			PARAMS ((tree));
extern tree finish_alignof			PARAMS ((tree));
extern void finish_decl_cleanup                 PARAMS ((tree, tree));
extern void finish_named_return_value           PARAMS ((tree, tree));
extern void expand_body                         PARAMS ((tree));
extern tree nullify_returns_r		      PARAMS ((tree *, int *, void *));
extern void do_pushlevel                        PARAMS ((void));
extern tree do_poplevel                         PARAMS ((void));
extern void finish_mem_initializers             PARAMS ((tree));
extern void setup_vtbl_ptr			PARAMS ((tree, tree));
extern void clear_out_block                     PARAMS ((void));
extern tree begin_global_stmt_expr              PARAMS ((void));
extern tree finish_global_stmt_expr             PARAMS ((tree));


/* in spew.c */
extern void init_spew				PARAMS ((void));
extern void mark_pending_inlines		PARAMS ((PTR));
extern int peekyylex				PARAMS ((void));
extern tree arbitrate_lookup			PARAMS ((tree, tree, tree));
extern tree frob_opname                         PARAMS ((tree));
extern void maybe_snarf_defarg			PARAMS ((void));
extern void add_defarg_fn			PARAMS ((tree));
extern void do_pending_defargs			PARAMS ((void));
extern void done_pending_defargs		PARAMS ((void));
extern void unprocessed_defarg_fn               PARAMS ((tree));
extern void replace_defarg			PARAMS ((tree, tree));
extern void end_input				PARAMS ((void));

/* in tree.c */
extern void init_tree			        PARAMS ((void));
extern int pod_type_p				PARAMS ((tree));
extern int zero_init_p				PARAMS ((tree));
extern tree canonical_type_variant              PARAMS ((tree));
extern void unshare_base_binfos			PARAMS ((tree));
extern int member_p				PARAMS ((tree));
extern cp_lvalue_kind real_lvalue_p		PARAMS ((tree));
extern tree build_min				PARAMS ((enum tree_code, tree, ...));
extern tree build_min_nt			PARAMS ((enum tree_code, ...));
extern tree build_cplus_new			PARAMS ((tree, tree));
extern tree get_target_expr			PARAMS ((tree));
extern tree break_out_calls			PARAMS ((tree));
extern tree build_cplus_method_type		PARAMS ((tree, tree, tree));
extern tree build_cplus_staticfn_type		PARAMS ((tree, tree, tree));
extern tree build_cplus_array_type		PARAMS ((tree, tree));
extern tree hash_tree_cons			PARAMS ((tree, tree, tree));
extern tree hash_tree_chain			PARAMS ((tree, tree));
extern tree hash_chainon			PARAMS ((tree, tree));
extern tree make_binfo				PARAMS ((tree, tree, tree, tree));
extern tree reverse_path			PARAMS ((tree));
extern int count_functions			PARAMS ((tree));
extern int is_overloaded_fn			PARAMS ((tree));
extern tree get_first_fn			PARAMS ((tree));
extern int bound_pmf_p				PARAMS ((tree));
extern tree ovl_cons                            PARAMS ((tree, tree));
extern tree build_overload                      PARAMS ((tree, tree));
extern tree function_arg_chain			PARAMS ((tree));
extern int promotes_to_aggr_type		PARAMS ((tree, enum tree_code));
extern int is_aggr_type_2			PARAMS ((tree, tree));
extern const char *lang_printable_name		PARAMS ((tree, int));
extern tree build_exception_variant		PARAMS ((tree, tree));
extern tree bind_template_template_parm		PARAMS ((tree, tree));
extern tree array_type_nelts_total		PARAMS ((tree));
extern tree array_type_nelts_top		PARAMS ((tree));
extern tree break_out_target_exprs		PARAMS ((tree));
extern tree get_type_decl			PARAMS ((tree));
extern tree vec_binfo_member			PARAMS ((tree, tree));
extern tree decl_namespace_context		PARAMS ((tree));
extern tree lvalue_type				PARAMS ((tree));
extern tree error_type				PARAMS ((tree));
extern tree build_ptr_wrapper			PARAMS ((void *));
extern tree build_int_wrapper			PARAMS ((int));
extern tree build_srcloc_here			PARAMS ((void));
extern int varargs_function_p			PARAMS ((tree));
extern int really_overloaded_fn			PARAMS ((tree));
extern int cp_tree_equal			PARAMS ((tree, tree));
extern tree no_linkage_check			PARAMS ((tree));
extern void debug_binfo				PARAMS ((tree));
extern tree build_dummy_object			PARAMS ((tree));
extern tree maybe_dummy_object			PARAMS ((tree, tree *));
extern int is_dummy_object			PARAMS ((tree));
extern const struct attribute_spec cp_attribute_table[];
extern tree make_ptrmem_cst                     PARAMS ((tree, tree));
extern tree cp_build_qualified_type_real        PARAMS ((tree, int, tsubst_flags_t));
#define cp_build_qualified_type(TYPE, QUALS) \
  cp_build_qualified_type_real ((TYPE), (QUALS), tf_error | tf_warning)
extern tree build_shared_int_cst                PARAMS ((int));
extern special_function_kind special_function_p PARAMS ((tree));
extern int count_trees                          PARAMS ((tree));
extern int char_type_p                          PARAMS ((tree));
extern void verify_stmt_tree                    PARAMS ((tree));
extern tree find_tree                           PARAMS ((tree, tree));
extern linkage_kind decl_linkage                PARAMS ((tree));
extern tree cp_walk_subtrees PARAMS ((tree*, int*, walk_tree_fn,
				      void*, void*));
extern int cp_cannot_inline_tree_fn PARAMS ((tree*));
extern tree cp_add_pending_fn_decls PARAMS ((void*,tree));
extern int cp_is_overload_p PARAMS ((tree));
extern int cp_auto_var_in_fn_p PARAMS ((tree,tree));
extern tree cp_copy_res_decl_for_inlining PARAMS ((tree, tree, tree, void*,
						   int*, void*));
extern int cp_start_inlining			PARAMS ((tree));
extern void cp_end_inlining			PARAMS ((tree));

/* in typeck.c */
extern int string_conv_p			PARAMS ((tree, tree, int));
extern tree cp_truthvalue_conversion		PARAMS ((tree));
extern tree condition_conversion		PARAMS ((tree));
extern tree target_type				PARAMS ((tree));
extern tree require_complete_type		PARAMS ((tree));
extern tree complete_type			PARAMS ((tree));
extern tree complete_type_or_else               PARAMS ((tree, tree));
extern int type_unknown_p			PARAMS ((tree));
extern tree commonparms				PARAMS ((tree, tree));
extern tree original_type			PARAMS ((tree));
extern int comp_except_specs			PARAMS ((tree, tree, int));
extern int comptypes				PARAMS ((tree, tree, int));
extern int comp_target_types			PARAMS ((tree, tree, int));
extern int compparms				PARAMS ((tree, tree));
extern int comp_cv_qualification                PARAMS ((tree, tree));
extern int comp_cv_qual_signature               PARAMS ((tree, tree));
extern tree expr_sizeof				PARAMS ((tree));
extern tree c_sizeof_nowarn			PARAMS ((tree));
extern tree inline_conversion			PARAMS ((tree));
extern tree decay_conversion			PARAMS ((tree));
extern tree build_object_ref			PARAMS ((tree, tree, tree));
extern tree build_component_ref_1		PARAMS ((tree, tree, int));
extern tree build_component_ref			PARAMS ((tree, tree, tree, int));
extern tree build_x_component_ref		PARAMS ((tree, tree, tree, int));
extern tree build_x_indirect_ref		PARAMS ((tree, const char *));
extern tree build_indirect_ref			PARAMS ((tree, const char *));
extern tree build_array_ref			PARAMS ((tree, tree));
extern tree build_x_function_call		PARAMS ((tree, tree, tree));
extern tree get_member_function_from_ptrfunc	PARAMS ((tree *, tree));
extern tree build_function_call_real		PARAMS ((tree, tree, int, int));
extern tree build_function_call_maybe		PARAMS ((tree, tree));
extern tree convert_arguments			PARAMS ((tree, tree, tree, int));
extern tree build_x_binary_op			PARAMS ((enum tree_code, tree, tree));
extern tree build_x_unary_op			PARAMS ((enum tree_code, tree));
extern tree unary_complex_lvalue		PARAMS ((enum tree_code, tree));
extern tree build_x_conditional_expr		PARAMS ((tree, tree, tree));
extern tree build_x_compound_expr		PARAMS ((tree));
extern tree build_compound_expr			PARAMS ((tree));
extern tree build_static_cast			PARAMS ((tree, tree));
extern tree build_reinterpret_cast		PARAMS ((tree, tree));
extern tree build_const_cast			PARAMS ((tree, tree));
extern tree build_c_cast			PARAMS ((tree, tree));
extern tree build_x_modify_expr			PARAMS ((tree, enum tree_code, tree));
extern tree build_modify_expr			PARAMS ((tree, enum tree_code, tree));
extern tree dubious_conversion_warnings         PARAMS ((tree, tree, const char *, tree, int));
extern tree convert_for_initialization		PARAMS ((tree, tree, tree, int, const char *, tree, int));
extern int comp_ptr_ttypes			PARAMS ((tree, tree));
extern int ptr_reasonably_similar		PARAMS ((tree, tree));
extern tree build_ptrmemfunc			PARAMS ((tree, tree, int));
extern int cp_type_quals                        PARAMS ((tree));
extern int cp_has_mutable_p                     PARAMS ((tree));
extern int at_least_as_qualified_p              PARAMS ((tree, tree));
extern int more_qualified_p                     PARAMS ((tree, tree));
extern tree build_ptrmemfunc1                   PARAMS ((tree, tree, tree));
extern void expand_ptrmemfunc_cst               PARAMS ((tree, tree *, tree *));
extern tree pfn_from_ptrmemfunc                 PARAMS ((tree));
extern tree type_after_usual_arithmetic_conversions PARAMS ((tree, tree));
extern tree composite_pointer_type              PARAMS ((tree, tree, tree, tree,
						       const char*));
extern tree merge_types				PARAMS ((tree, tree));
extern tree check_return_expr                   PARAMS ((tree));
#define cp_build_binary_op(code, arg1, arg2) \
  build_binary_op(code, arg1, arg2, 1)

/* in typeck2.c */
extern tree error_not_base_type			PARAMS ((tree, tree));
extern tree binfo_or_else			PARAMS ((tree, tree));
extern void readonly_error			PARAMS ((tree, const char *, int));
extern int abstract_virtuals_error		PARAMS ((tree, tree));

#define my_friendly_assert(EXP, N) (void) \
 (((EXP) == 0) ? (fancy_abort (__FILE__, __LINE__, __FUNCTION__), 0) : 0)

extern tree force_store_init_value		PARAMS ((tree, tree));
extern tree store_init_value			PARAMS ((tree, tree));
extern tree digest_init				PARAMS ((tree, tree, tree *));
extern tree build_scoped_ref			PARAMS ((tree, tree));
extern tree build_x_arrow			PARAMS ((tree));
extern tree build_m_component_ref		PARAMS ((tree, tree));
extern tree build_functional_cast		PARAMS ((tree, tree));
extern void check_for_new_type			PARAMS ((const char *, flagged_type_tree));
extern tree add_exception_specifier             PARAMS ((tree, tree, int));
extern tree merge_exception_specifiers          PARAMS ((tree, tree));

/* in mangle.c */
extern void init_mangle                         PARAMS ((void));
extern void mangle_decl                         PARAMS ((tree));
extern const char *mangle_type_string           PARAMS ((tree));
extern tree mangle_type                         PARAMS ((tree));
extern tree mangle_typeinfo_for_type            PARAMS ((tree));
extern tree mangle_typeinfo_string_for_type     PARAMS ((tree));
extern tree mangle_vtbl_for_type                PARAMS ((tree));
extern tree mangle_vtt_for_type                 PARAMS ((tree));
extern tree mangle_ctor_vtbl_for_type           PARAMS ((tree, tree));
extern tree mangle_thunk                        PARAMS ((tree, tree, tree));
extern tree mangle_conv_op_name_for_type        PARAMS ((tree));
extern tree mangle_guard_variable               PARAMS ((tree));
extern tree mangle_ref_init_variable            PARAMS ((tree));

/* in dump.c */
extern int cp_dump_tree                         PARAMS ((void *, tree));

/* -- end of C++ */

#endif /* ! GCC_CP_TREE_H */
