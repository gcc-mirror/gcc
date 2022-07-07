// Copyright (C) 2020-2022 Free Software Foundation, Inc.

// This file is part of GCC.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#ifndef RUST_TREE
#define RUST_TREE

#include "rust-system.h"
#include "coretypes.h"
#include "tree.h"
#include "cpplib.h"

/* Returns true if NODE is a pointer.  */
#define TYPE_PTR_P(NODE) (TREE_CODE (NODE) == POINTER_TYPE)

/* Returns true if NODE is a reference.  */
#define TYPE_REF_P(NODE) (TREE_CODE (NODE) == REFERENCE_TYPE)

/* Returns true if NODE is a pointer or a reference.  */
#define INDIRECT_TYPE_P(NODE) (TYPE_PTR_P (NODE) || TYPE_REF_P (NODE))

/* [basic.fundamental]

   Types  bool, char, wchar_t, and the signed and unsigned integer types
   are collectively called integral types.

   Note that INTEGRAL_TYPE_P, as defined in tree.h, allows enumeration
   types as well, which is incorrect in C++.  Keep these checks in
   ascending code order.  */
#define RS_INTEGRAL_TYPE_P(TYPE)                                               \
  (TREE_CODE (TYPE) == BOOLEAN_TYPE || TREE_CODE (TYPE) == INTEGER_TYPE)

/* [basic.fundamental]

   Integral and floating types are collectively called arithmetic
   types.

   As a GNU extension, we also accept complex types.

   Keep these checks in ascending code order.  */
#define ARITHMETIC_TYPE_P(TYPE)                                                \
  (RS_INTEGRAL_TYPE_P (TYPE) || TREE_CODE (TYPE) == REAL_TYPE                  \
   || TREE_CODE (TYPE) == COMPLEX_TYPE)

/* True iff TYPE is cv decltype(nullptr).  */
#define NULLPTR_TYPE_P(TYPE) (TREE_CODE (TYPE) == NULLPTR_TYPE)

/* [basic.types]

   Arithmetic types, enumeration types, pointer types,
   pointer-to-member types, and std::nullptr_t are collectively called
   scalar types.

   Keep these checks in ascending code order.  */
#define SCALAR_TYPE_P(TYPE)                                                    \
  (TREE_CODE (TYPE) == ENUMERAL_TYPE || ARITHMETIC_TYPE_P (TYPE)               \
   || TYPE_PTR_P (TYPE) || NULLPTR_TYPE_P (TYPE))

/* True if NODE is an implicit INDIRECT_REF from convert_from_reference.  */
#define REFERENCE_REF_P(NODE)                                                  \
  (INDIRECT_REF_P (NODE) && TREE_TYPE (TREE_OPERAND (NODE, 0))                 \
   && TYPE_REF_P (TREE_TYPE (TREE_OPERAND ((NODE), 0))))

// this is a helper to differentiate RECORD types between actual records and
// slices
#define SLICE_FLAG TREE_LANG_FLAG_0
#define SLICE_TYPE_P(TYPE)                                                     \
  (TREE_CODE (TYPE) == RECORD_TYPE && TREE_LANG_FLAG_0 (TYPE))

/* Returns true if NODE is a pointer to member function type.  */
#define TYPE_PTRMEMFUNC_P(NODE)                                                \
  (TREE_CODE (NODE) == RECORD_TYPE && TYPE_PTRMEMFUNC_FLAG (NODE))

#define TYPE_PTRMEMFUNC_FLAG(NODE) (TYPE_LANG_FLAG_2 (RECORD_TYPE_CHECK (NODE)))

#define TYPE_PTRMEMFUNC_FN_TYPE_RAW(NODE) (TREE_TYPE (TYPE_FIELDS (NODE)))

/* True if NODE is a compound-literal, i.e., a brace-enclosed
   initializer cast to a particular type.  This is mostly only set during
   template parsing; once the initializer has been digested into an actual
   value of the type, the expression is represented by a TARGET_EXPR.  */
#define COMPOUND_LITERAL_P(NODE)                                               \
  (TREE_CODE (NODE) == CONSTRUCTOR && TREE_HAS_CONSTRUCTOR (NODE))

/* When appearing in an INDIRECT_REF, it means that the tree structure
   underneath is actually a call to a constructor.  This is needed
   when the constructor must initialize local storage (which can
   be automatically destroyed), rather than allowing it to allocate
   space from the heap.

   When appearing in a SAVE_EXPR, it means that underneath
   is a call to a constructor.

   When appearing in a CONSTRUCTOR, the expression is an unconverted
   compound literal.

   When appearing in a FIELD_DECL, it means that this field
   has been duly initialized in its constructor.  */
#define TREE_HAS_CONSTRUCTOR(NODE) (TREE_LANG_FLAG_4 (NODE))

/* Nonzero if T is a class type.  Zero for template type parameters,
   typename types, and so forth.  */
#define CLASS_TYPE_P(T)                                                        \
  (RECORD_OR_UNION_CODE_P (TREE_CODE (T)) && TYPE_LANG_FLAG_5 (T))

/* [class.virtual]

   A class that declares or inherits a virtual function is called a
   polymorphic class.  */
#define TYPE_POLYMORPHIC_P(NODE) (TREE_LANG_FLAG_2 (NODE))

/* Nonzero if this class has a virtual function table pointer.  */
#define TYPE_CONTAINS_VPTR_P(NODE)                                             \
  (TYPE_POLYMORPHIC_P (NODE) || CLASSTYPE_VBASECLASSES (NODE))

/* A vector of BINFOs for the direct and indirect virtual base classes
   that this type uses in a post-order depth-first left-to-right
   order.  (In other words, these bases appear in the order that they
   should be initialized.)  */
#define CLASSTYPE_VBASECLASSES(NODE) (LANG_TYPE_CLASS_CHECK (NODE)->vbases)

/* A vector of BINFOs for the direct and indirect virtual base classes
   that this type uses in a post-order depth-first left-to-right
   order.  (In other words, these bases appear in the order that they
   should be initialized.)  */
#define CLASSTYPE_VBASECLASSES(NODE) (LANG_TYPE_CLASS_CHECK (NODE)->vbases)

/* We used to have a variant type for lang_type.  Keep the name of the
   checking accessor for the sole survivor.  */
#define LANG_TYPE_CLASS_CHECK(NODE) (TYPE_LANG_SPECIFIC (NODE))

/* Keep these checks in ascending code order.  */
#define RECORD_OR_UNION_CODE_P(T) ((T) == RECORD_TYPE || (T) == UNION_TYPE)
#define OVERLOAD_TYPE_P(T) (CLASS_TYPE_P (T) || TREE_CODE (T) == ENUMERAL_TYPE)

/* Nonzero if this class is "empty" in the sense of the C++ ABI.  */
#define CLASSTYPE_EMPTY_P(NODE) (LANG_TYPE_CLASS_CHECK (NODE)->empty_p)

/* True if DECL is declared 'constexpr'.  */
#define DECL_DECLARED_CONSTEXPR_P(DECL)                                        \
  DECL_LANG_FLAG_8 (VAR_OR_FUNCTION_DECL_CHECK (DECL))

#define VAR_OR_FUNCTION_DECL_CHECK(NODE)                                       \
  TREE_CHECK2 (NODE, VAR_DECL, FUNCTION_DECL)

// forked from gcc/cp/cp-tree.h cp_tree_index

enum cp_tree_index
{
  CPTI_WCHAR_DECL,
  CPTI_VTABLE_ENTRY_TYPE,
  CPTI_DELTA_TYPE,
  CPTI_VTABLE_INDEX_TYPE,
  CPTI_CLEANUP_TYPE,
  CPTI_VTT_PARM_TYPE,

  CPTI_CLASS_TYPE,
  CPTI_UNKNOWN_TYPE,
  CPTI_INIT_LIST_TYPE,
  CPTI_EXPLICIT_VOID_LIST,
  CPTI_VTBL_TYPE,
  CPTI_VTBL_PTR_TYPE,
  CPTI_GLOBAL,
  CPTI_ABORT_FNDECL,
  CPTI_AGGR_TAG,
  CPTI_CONV_OP_MARKER,

  CPTI_CTOR_IDENTIFIER,
  CPTI_COMPLETE_CTOR_IDENTIFIER,
  CPTI_BASE_CTOR_IDENTIFIER,
  CPTI_DTOR_IDENTIFIER,
  CPTI_COMPLETE_DTOR_IDENTIFIER,
  CPTI_BASE_DTOR_IDENTIFIER,
  CPTI_DELETING_DTOR_IDENTIFIER,
  CPTI_CONV_OP_IDENTIFIER,
  CPTI_DELTA_IDENTIFIER,
  CPTI_IN_CHARGE_IDENTIFIER,
  CPTI_VTT_PARM_IDENTIFIER,
  CPTI_AS_BASE_IDENTIFIER,
  CPTI_THIS_IDENTIFIER,
  CPTI_PFN_IDENTIFIER,
  CPTI_VPTR_IDENTIFIER,
  CPTI_GLOBAL_IDENTIFIER,
  CPTI_ANON_IDENTIFIER,
  CPTI_AUTO_IDENTIFIER,
  CPTI_DECLTYPE_AUTO_IDENTIFIER,
  CPTI_INIT_LIST_IDENTIFIER,
  CPTI_FOR_RANGE__IDENTIFIER,
  CPTI_FOR_BEGIN__IDENTIFIER,
  CPTI_FOR_END__IDENTIFIER,
  CPTI_FOR_RANGE_IDENTIFIER,
  CPTI_FOR_BEGIN_IDENTIFIER,
  CPTI_FOR_END_IDENTIFIER,
  CPTI_ABI_TAG_IDENTIFIER,
  CPTI_ALIGNED_IDENTIFIER,
  CPTI_BEGIN_IDENTIFIER,
  CPTI_END_IDENTIFIER,
  CPTI_GET_IDENTIFIER,
  CPTI_GNU_IDENTIFIER,
  CPTI_TUPLE_ELEMENT_IDENTIFIER,
  CPTI_TUPLE_SIZE_IDENTIFIER,
  CPTI_TYPE_IDENTIFIER,
  CPTI_VALUE_IDENTIFIER,
  CPTI_FUN_IDENTIFIER,
  CPTI_CLOSURE_IDENTIFIER,
  CPTI_HEAP_UNINIT_IDENTIFIER,
  CPTI_HEAP_IDENTIFIER,
  CPTI_HEAP_DELETED_IDENTIFIER,
  CPTI_HEAP_VEC_UNINIT_IDENTIFIER,
  CPTI_HEAP_VEC_IDENTIFIER,
  CPTI_OMP_IDENTIFIER,

  CPTI_LANG_NAME_C,
  CPTI_LANG_NAME_CPLUSPLUS,

  CPTI_EMPTY_EXCEPT_SPEC,
  CPTI_NOEXCEPT_TRUE_SPEC,
  CPTI_NOEXCEPT_FALSE_SPEC,
  CPTI_NOEXCEPT_DEFERRED_SPEC,

  CPTI_NULLPTR,
  CPTI_NULLPTR_TYPE,

  CPTI_ANY_TARG,

  CPTI_MODULE_HWM,
  /* Nodes after here change during compilation, or should not be in
     the module's global tree table.  Such nodes must be locatable
     via name lookup or type-construction, as those are the only
     cross-TU matching capabilities remaining.  */

  /* We must find these via the global namespace.  */
  CPTI_STD,
  CPTI_ABI,

  /* These are created at init time, but the library/headers provide
     definitions.  */
  CPTI_ALIGN_TYPE,
  CPTI_TERMINATE_FN,
  CPTI_CALL_UNEXPECTED_FN,

  /* These are lazily inited.  */
  CPTI_CONST_TYPE_INFO_TYPE,
  CPTI_GET_EXCEPTION_PTR_FN,
  CPTI_BEGIN_CATCH_FN,
  CPTI_END_CATCH_FN,
  CPTI_ALLOCATE_EXCEPTION_FN,
  CPTI_FREE_EXCEPTION_FN,
  CPTI_THROW_FN,
  CPTI_RETHROW_FN,
  CPTI_ATEXIT_FN_PTR_TYPE,
  CPTI_ATEXIT,
  CPTI_DSO_HANDLE,
  CPTI_DCAST,

  CPTI_SOURCE_LOCATION_IMPL,

  CPTI_FALLBACK_DFLOAT32_TYPE,
  CPTI_FALLBACK_DFLOAT64_TYPE,
  CPTI_FALLBACK_DFLOAT128_TYPE,

  CPTI_MAX
};

// forked from gcc/cp/cp-tree.h cp_global_trees

extern GTY (()) tree cp_global_trees[CPTI_MAX];

#define wchar_decl_node cp_global_trees[CPTI_WCHAR_DECL]
#define vtable_entry_type cp_global_trees[CPTI_VTABLE_ENTRY_TYPE]
/* The type used to represent an offset by which to adjust the `this'
   pointer in pointer-to-member types.  */
#define delta_type_node cp_global_trees[CPTI_DELTA_TYPE]
/* The type used to represent an index into the vtable.  */
#define vtable_index_type cp_global_trees[CPTI_VTABLE_INDEX_TYPE]

#define class_type_node cp_global_trees[CPTI_CLASS_TYPE]
#define unknown_type_node cp_global_trees[CPTI_UNKNOWN_TYPE]
#define init_list_type_node cp_global_trees[CPTI_INIT_LIST_TYPE]
#define explicit_void_list_node cp_global_trees[CPTI_EXPLICIT_VOID_LIST]
#define vtbl_type_node cp_global_trees[CPTI_VTBL_TYPE]
#define vtbl_ptr_type_node cp_global_trees[CPTI_VTBL_PTR_TYPE]
#define std_node cp_global_trees[CPTI_STD]
#define abi_node cp_global_trees[CPTI_ABI]
#define global_namespace cp_global_trees[CPTI_GLOBAL]
#define const_type_info_type_node cp_global_trees[CPTI_CONST_TYPE_INFO_TYPE]
#define conv_op_marker cp_global_trees[CPTI_CONV_OP_MARKER]
#define abort_fndecl cp_global_trees[CPTI_ABORT_FNDECL]
#define current_aggr cp_global_trees[CPTI_AGGR_TAG]
#define nullptr_node cp_global_trees[CPTI_NULLPTR]
#define nullptr_type_node cp_global_trees[CPTI_NULLPTR_TYPE]
/* std::align_val_t */
#define align_type_node cp_global_trees[CPTI_ALIGN_TYPE]

/* We cache these tree nodes so as to call get_identifier less frequently.
   For identifiers for functions, including special member functions such
   as ctors and assignment operators, the nodes can be used (among other
   things) to iterate over their overloads defined by/for a type.  For
   example:

     tree ovlid = assign_op_identifier;
     tree overloads = get_class_binding (type, ovlid);
     for (ovl_iterator it (overloads); it; ++it) { ... }

   iterates over the set of implicitly and explicitly defined overloads
   of the assignment operator for type (including the copy and move
   assignment operators, whether deleted or not).  */

/* The name of a constructor that takes an in-charge parameter to
   decide whether or not to construct virtual base classes.  */
#define ctor_identifier cp_global_trees[CPTI_CTOR_IDENTIFIER]
/* The name of a constructor that constructs virtual base classes.  */
#define complete_ctor_identifier cp_global_trees[CPTI_COMPLETE_CTOR_IDENTIFIER]
/* The name of a constructor that does not construct virtual base classes.  */
#define base_ctor_identifier cp_global_trees[CPTI_BASE_CTOR_IDENTIFIER]
/* The name of a destructor that takes an in-charge parameter to
   decide whether or not to destroy virtual base classes and whether
   or not to delete the object.  */
#define dtor_identifier cp_global_trees[CPTI_DTOR_IDENTIFIER]
/* The name of a destructor that destroys virtual base classes.  */
#define complete_dtor_identifier cp_global_trees[CPTI_COMPLETE_DTOR_IDENTIFIER]
/* The name of a destructor that does not destroy virtual base
   classes.  */
#define base_dtor_identifier cp_global_trees[CPTI_BASE_DTOR_IDENTIFIER]
/* The name of a destructor that destroys virtual base classes, and
   then deletes the entire object.  */
#define deleting_dtor_identifier cp_global_trees[CPTI_DELETING_DTOR_IDENTIFIER]

/* The name used for conversion operators -- but note that actual
   conversion functions use special identifiers outside the identifier
   table.  */
#define conv_op_identifier cp_global_trees[CPTI_CONV_OP_IDENTIFIER]

#define delta_identifier cp_global_trees[CPTI_DELTA_IDENTIFIER]
#define in_charge_identifier cp_global_trees[CPTI_IN_CHARGE_IDENTIFIER]
/* The name of the parameter that contains a pointer to the VTT to use
   for this subobject constructor or destructor.  */
#define vtt_parm_identifier cp_global_trees[CPTI_VTT_PARM_IDENTIFIER]
#define as_base_identifier cp_global_trees[CPTI_AS_BASE_IDENTIFIER]
#define this_identifier cp_global_trees[CPTI_THIS_IDENTIFIER]
#define pfn_identifier cp_global_trees[CPTI_PFN_IDENTIFIER]
#define vptr_identifier cp_global_trees[CPTI_VPTR_IDENTIFIER]
/* The name of the ::, std & anon namespaces.  */
#define global_identifier cp_global_trees[CPTI_GLOBAL_IDENTIFIER]
#define anon_identifier cp_global_trees[CPTI_ANON_IDENTIFIER]
/* auto and declspec(auto) identifiers.  */
#define auto_identifier cp_global_trees[CPTI_AUTO_IDENTIFIER]
#define decltype_auto_identifier cp_global_trees[CPTI_DECLTYPE_AUTO_IDENTIFIER]
#define init_list_identifier cp_global_trees[CPTI_INIT_LIST_IDENTIFIER]
#define for_range__identifier cp_global_trees[CPTI_FOR_RANGE__IDENTIFIER]
#define for_begin__identifier cp_global_trees[CPTI_FOR_BEGIN__IDENTIFIER]
#define for_end__identifier cp_global_trees[CPTI_FOR_END__IDENTIFIER]
#define for_range_identifier cp_global_trees[CPTI_FOR_RANGE_IDENTIFIER]
#define for_begin_identifier cp_global_trees[CPTI_FOR_BEGIN_IDENTIFIER]
#define for_end_identifier cp_global_trees[CPTI_FOR_END_IDENTIFIER]
#define abi_tag_identifier cp_global_trees[CPTI_ABI_TAG_IDENTIFIER]
#define aligned_identifier cp_global_trees[CPTI_ALIGNED_IDENTIFIER]
#define begin_identifier cp_global_trees[CPTI_BEGIN_IDENTIFIER]
#define end_identifier cp_global_trees[CPTI_END_IDENTIFIER]
#define get__identifier cp_global_trees[CPTI_GET_IDENTIFIER]
#define gnu_identifier cp_global_trees[CPTI_GNU_IDENTIFIER]
#define tuple_element_identifier cp_global_trees[CPTI_TUPLE_ELEMENT_IDENTIFIER]
#define tuple_size_identifier cp_global_trees[CPTI_TUPLE_SIZE_IDENTIFIER]
#define type_identifier cp_global_trees[CPTI_TYPE_IDENTIFIER]
#define value_identifier cp_global_trees[CPTI_VALUE_IDENTIFIER]
#define fun_identifier cp_global_trees[CPTI_FUN_IDENTIFIER]
#define closure_identifier cp_global_trees[CPTI_CLOSURE_IDENTIFIER]
#define heap_uninit_identifier cp_global_trees[CPTI_HEAP_UNINIT_IDENTIFIER]
#define heap_identifier cp_global_trees[CPTI_HEAP_IDENTIFIER]
#define heap_deleted_identifier cp_global_trees[CPTI_HEAP_DELETED_IDENTIFIER]
#define heap_vec_uninit_identifier                                             \
  cp_global_trees[CPTI_HEAP_VEC_UNINIT_IDENTIFIER]
#define heap_vec_identifier cp_global_trees[CPTI_HEAP_VEC_IDENTIFIER]
#define omp_identifier cp_global_trees[CPTI_OMP_IDENTIFIER]
#define lang_name_c cp_global_trees[CPTI_LANG_NAME_C]
#define lang_name_cplusplus cp_global_trees[CPTI_LANG_NAME_CPLUSPLUS]

/* Exception specifiers used for throw(), noexcept(true),
   noexcept(false) and deferred noexcept.  We rely on these being
   uncloned.  */
#define empty_except_spec cp_global_trees[CPTI_EMPTY_EXCEPT_SPEC]
#define noexcept_true_spec cp_global_trees[CPTI_NOEXCEPT_TRUE_SPEC]
#define noexcept_false_spec cp_global_trees[CPTI_NOEXCEPT_FALSE_SPEC]
#define noexcept_deferred_spec cp_global_trees[CPTI_NOEXCEPT_DEFERRED_SPEC]

/* Exception handling function declarations.  */
#define terminate_fn cp_global_trees[CPTI_TERMINATE_FN]
#define call_unexpected_fn cp_global_trees[CPTI_CALL_UNEXPECTED_FN]
#define get_exception_ptr_fn cp_global_trees[CPTI_GET_EXCEPTION_PTR_FN]
#define begin_catch_fn cp_global_trees[CPTI_BEGIN_CATCH_FN]
#define end_catch_fn cp_global_trees[CPTI_END_CATCH_FN]
#define allocate_exception_fn cp_global_trees[CPTI_ALLOCATE_EXCEPTION_FN]
#define free_exception_fn cp_global_trees[CPTI_FREE_EXCEPTION_FN]
#define throw_fn cp_global_trees[CPTI_THROW_FN]
#define rethrow_fn cp_global_trees[CPTI_RETHROW_FN]

/* The type of the function-pointer argument to "__cxa_atexit" (or
   "std::atexit", if "__cxa_atexit" is not being used).  */
#define atexit_fn_ptr_type_node cp_global_trees[CPTI_ATEXIT_FN_PTR_TYPE]

/* A pointer to `std::atexit'.  */
#define atexit_node cp_global_trees[CPTI_ATEXIT]

/* A pointer to `__dso_handle'.  */
#define dso_handle_node cp_global_trees[CPTI_DSO_HANDLE]

/* The declaration of the dynamic_cast runtime.  */
#define dynamic_cast_node cp_global_trees[CPTI_DCAST]

/* The type of a destructor.  */
#define cleanup_type cp_global_trees[CPTI_CLEANUP_TYPE]

/* The type of the vtt parameter passed to subobject constructors and
   destructors.  */
#define vtt_parm_type cp_global_trees[CPTI_VTT_PARM_TYPE]

/* A node which matches any template argument.  */
#define any_targ_node cp_global_trees[CPTI_ANY_TARG]

/* std::source_location::__impl class.  */
#define source_location_impl cp_global_trees[CPTI_SOURCE_LOCATION_IMPL]

/* These two accessors should only be used by OVL manipulators.
   Other users should use iterators and convenience functions.  */
#define OVL_FUNCTION(NODE)                                                     \
  (((struct tree_overload *) OVERLOAD_CHECK (NODE))->function)
#define OVL_CHAIN(NODE)                                                        \
  (((struct tree_overload *) OVERLOAD_CHECK (NODE))->common.chain)

/* If set, this or a subsequent overload contains decls that need deduping.  */
#define OVL_DEDUP_P(NODE) TREE_LANG_FLAG_0 (OVERLOAD_CHECK (NODE))
/* If set, this was imported in a using declaration.   */
#define OVL_USING_P(NODE) TREE_LANG_FLAG_1 (OVERLOAD_CHECK (NODE))
/* If set, this overload is a hidden decl.  */
#define OVL_HIDDEN_P(NODE) TREE_LANG_FLAG_2 (OVERLOAD_CHECK (NODE))
/* If set, this overload contains a nested overload.  */
#define OVL_NESTED_P(NODE) TREE_LANG_FLAG_3 (OVERLOAD_CHECK (NODE))
/* If set, this overload was constructed during lookup.  */
#define OVL_LOOKUP_P(NODE) TREE_LANG_FLAG_4 (OVERLOAD_CHECK (NODE))
/* If set, this OVL_USING_P overload is exported.  */
#define OVL_EXPORT_P(NODE) TREE_LANG_FLAG_5 (OVERLOAD_CHECK (NODE))

/* The first decl of an overload.  */
#define OVL_FIRST(NODE) ovl_first (NODE)
/* The name of the overload set.  */
#define OVL_NAME(NODE) DECL_NAME (OVL_FIRST (NODE))

/* Whether this is a set of overloaded functions.  TEMPLATE_DECLS are
   always wrapped in an OVERLOAD, so we don't need to check them
   here.  */
#define OVL_P(NODE)                                                            \
  (TREE_CODE (NODE) == FUNCTION_DECL || TREE_CODE (NODE) == OVERLOAD)
/* Whether this is a single member overload.  */
#define OVL_SINGLE_P(NODE) (TREE_CODE (NODE) != OVERLOAD || !OVL_CHAIN (NODE))

/* Nonzero means that this type has an X() constructor.  */
#define TYPE_HAS_DEFAULT_CONSTRUCTOR(NODE)                                     \
  (LANG_TYPE_CLASS_CHECK (NODE)->has_default_ctor)

/* Nonzero means that NODE (a class type) has a default constructor --
   but that it has not yet been declared.  */
#define CLASSTYPE_LAZY_DEFAULT_CTOR(NODE)                                      \
  (LANG_TYPE_CLASS_CHECK (NODE)->lazy_default_ctor)

/* A FUNCTION_DECL or OVERLOAD for the constructors for NODE.  These
   are the constructors that take an in-charge parameter.  */
#define CLASSTYPE_CONSTRUCTORS(NODE)                                           \
  (get_class_binding_direct (NODE, ctor_identifier))

/* In a TREE_LIST in an attribute list, indicates that the attribute
   must be applied at instantiation time.  */
#define ATTR_IS_DEPENDENT(NODE) TREE_LANG_FLAG_0 (TREE_LIST_CHECK (NODE))

/* In a TREE_LIST in the argument of attribute abi_tag, indicates that the tag
   was inherited from a template parameter, not explicitly indicated.  */
#define ABI_TAG_IMPLICIT(NODE) TREE_LANG_FLAG_0 (TREE_LIST_CHECK (NODE))

/* In a TREE_LIST for a parameter-declaration-list, indicates that all the
   parameters in the list have declarators enclosed in ().  */
#define PARENTHESIZED_LIST_P(NODE) TREE_LANG_FLAG_0 (TREE_LIST_CHECK (NODE))

/* Non zero if this is a using decl for a dependent scope. */
#define DECL_DEPENDENT_P(NODE) DECL_LANG_FLAG_0 (USING_DECL_CHECK (NODE))

/* The scope named in a using decl.  */
#define USING_DECL_SCOPE(NODE) DECL_RESULT_FLD (USING_DECL_CHECK (NODE))

/* The decls named by a using decl.  */
#define USING_DECL_DECLS(NODE) DECL_INITIAL (USING_DECL_CHECK (NODE))

/* Non zero if the using decl refers to a dependent type.  */
#define USING_DECL_TYPENAME_P(NODE) DECL_LANG_FLAG_1 (USING_DECL_CHECK (NODE))

/* True if member using decl NODE refers to a non-inherited NODE.  */
#define USING_DECL_UNRELATED_P(NODE) DECL_LANG_FLAG_2 (USING_DECL_CHECK (NODE))

/* Nonzero if NODE declares a function.  */
#define DECL_DECLARES_FUNCTION_P(NODE) (TREE_CODE (NODE) == FUNCTION_DECL)

/* Nonzero for a NODE which declares a type.  */
#define DECL_DECLARES_TYPE_P(NODE) (TREE_CODE (NODE) == TYPE_DECL)

/* Kind bits.  */
#define IDENTIFIER_KIND_BIT_0(NODE)                                            \
  TREE_LANG_FLAG_0 (IDENTIFIER_NODE_CHECK (NODE))
#define IDENTIFIER_KIND_BIT_1(NODE)                                            \
  TREE_LANG_FLAG_1 (IDENTIFIER_NODE_CHECK (NODE))
#define IDENTIFIER_KIND_BIT_2(NODE)                                            \
  TREE_LANG_FLAG_2 (IDENTIFIER_NODE_CHECK (NODE))

/* Used by various search routines.  */
#define IDENTIFIER_MARKED(NODE) TREE_LANG_FLAG_4 (IDENTIFIER_NODE_CHECK (NODE))

/* Nonzero if this identifier is used as a virtual function name somewhere
   (optimizes searches).  */
#define IDENTIFIER_VIRTUAL_P(NODE)                                             \
  TREE_LANG_FLAG_5 (IDENTIFIER_NODE_CHECK (NODE))

/* True if this identifier is a reserved word.  C_RID_CODE (node) is
   then the RID_* value of the keyword.  Value 1.  */
#define IDENTIFIER_KEYWORD_P(NODE)                                             \
  ((!IDENTIFIER_KIND_BIT_2 (NODE)) & (!IDENTIFIER_KIND_BIT_1 (NODE))           \
   & IDENTIFIER_KIND_BIT_0 (NODE))

/* True if this identifier is the name of a constructor or
   destructor.  Value 2 or 3.  */
#define IDENTIFIER_CDTOR_P(NODE)                                               \
  ((!IDENTIFIER_KIND_BIT_2 (NODE)) & IDENTIFIER_KIND_BIT_1 (NODE))

/* True if this identifier is the name of a constructor.  Value 2.  */
#define IDENTIFIER_CTOR_P(NODE)                                                \
  (IDENTIFIER_CDTOR_P (NODE) & (!IDENTIFIER_KIND_BIT_0 (NODE)))

/* True if this identifier is the name of a destructor.  Value 3.  */
#define IDENTIFIER_DTOR_P(NODE)                                                \
  (IDENTIFIER_CDTOR_P (NODE) & IDENTIFIER_KIND_BIT_0 (NODE))

/* True if this identifier is for any operator name (including
   conversions).  Value 4, 5, 6 or 7.  */
#define IDENTIFIER_ANY_OP_P(NODE) (IDENTIFIER_KIND_BIT_2 (NODE))

/* True if this identifier is for an overloaded operator. Values 4, 5.  */
#define IDENTIFIER_OVL_OP_P(NODE)                                              \
  (IDENTIFIER_ANY_OP_P (NODE) & (!IDENTIFIER_KIND_BIT_1 (NODE)))

/* True if this identifier is for any assignment. Values 5.  */
#define IDENTIFIER_ASSIGN_OP_P(NODE)                                           \
  (IDENTIFIER_OVL_OP_P (NODE) & IDENTIFIER_KIND_BIT_0 (NODE))

/* True if this identifier is the name of a type-conversion
   operator.  Value 7.  */
#define IDENTIFIER_CONV_OP_P(NODE)                                             \
  (IDENTIFIER_ANY_OP_P (NODE) & IDENTIFIER_KIND_BIT_1 (NODE)                   \
   & (!IDENTIFIER_KIND_BIT_0 (NODE)))

/* True if this identifier is a new or delete operator.  */
#define IDENTIFIER_NEWDEL_OP_P(NODE)                                           \
  (IDENTIFIER_OVL_OP_P (NODE)                                                  \
   && IDENTIFIER_OVL_OP_FLAGS (NODE) & OVL_OP_FLAG_ALLOC)

/* True if this identifier is a new operator.  */
#define IDENTIFIER_NEW_OP_P(NODE)                                              \
  (IDENTIFIER_OVL_OP_P (NODE)                                                  \
   && (IDENTIFIER_OVL_OP_FLAGS (NODE)                                          \
       & (OVL_OP_FLAG_ALLOC | OVL_OP_FLAG_DELETE))                             \
	== OVL_OP_FLAG_ALLOC)

/* Nonzero if the class NODE has multiple paths to the same (virtual)
   base object.  */
#define CLASSTYPE_DIAMOND_SHAPED_P(NODE)                                       \
  (LANG_TYPE_CLASS_CHECK (NODE)->diamond_shaped)

/* Nonzero if the class NODE has multiple instances of the same base
   type.  */
#define CLASSTYPE_REPEATED_BASE_P(NODE)                                        \
  (LANG_TYPE_CLASS_CHECK (NODE)->repeated_base)

/* The member function with which the vtable will be emitted:
   the first noninline non-pure-virtual member function.  NULL_TREE
   if there is no key function or if this is a class template */
#define CLASSTYPE_KEY_METHOD(NODE) (LANG_TYPE_CLASS_CHECK (NODE)->key_method)

/* Vector of members.  During definition, it is unordered and only
   member functions are present.  After completion it is sorted and
   contains both member functions and non-functions.  STAT_HACK is
   involved to preserve oneslot per name invariant.  */
#define CLASSTYPE_MEMBER_VEC(NODE) (LANG_TYPE_CLASS_CHECK (NODE)->members)

/* For class templates, this is a TREE_LIST of all member data,
   functions, types, and friends in the order of declaration.
   The TREE_PURPOSE of each TREE_LIST is NULL_TREE for a friend,
   and the RECORD_TYPE for the class template otherwise.  */
#define CLASSTYPE_DECL_LIST(NODE) (LANG_TYPE_CLASS_CHECK (NODE)->decl_list)

/* A FUNCTION_DECL or OVERLOAD for the constructors for NODE.  These
   are the constructors that take an in-charge parameter.  */
#define CLASSTYPE_CONSTRUCTORS(NODE)                                           \
  (get_class_binding_direct (NODE, ctor_identifier))

/* A FUNCTION_DECL for the destructor for NODE.  This is the
   destructors that take an in-charge parameter.  If
   CLASSTYPE_LAZY_DESTRUCTOR is true, then this entry will be NULL
   until the destructor is created with lazily_declare_fn.  */
#define CLASSTYPE_DESTRUCTOR(NODE)                                             \
  (get_class_binding_direct (NODE, dtor_identifier))

/* Nonzero if NODE has a primary base class, i.e., a base class with
   which it shares the virtual function table pointer.  */
#define CLASSTYPE_HAS_PRIMARY_BASE_P(NODE)                                     \
  (CLASSTYPE_PRIMARY_BINFO (NODE) != NULL_TREE)

/* If non-NULL, this is the binfo for the primary base class, i.e.,
   the base class which contains the virtual function table pointer
   for this class.  */
#define CLASSTYPE_PRIMARY_BINFO(NODE)                                          \
  (LANG_TYPE_CLASS_CHECK (NODE)->primary_base)

/* A vector of BINFOs for the direct and indirect virtual base classes
   that this type uses in a post-order depth-first left-to-right
   order.  (In other words, these bases appear in the order that they
   should be initialized.)  */
#define CLASSTYPE_VBASECLASSES(NODE) (LANG_TYPE_CLASS_CHECK (NODE)->vbases)

/* The type corresponding to NODE when NODE is used as a base class,
   i.e., NODE without virtual base classes or tail padding.  */
#define CLASSTYPE_AS_BASE(NODE) (LANG_TYPE_CLASS_CHECK (NODE)->as_base)

/* Nonzero if NODE is a user-defined conversion operator.  */
#define DECL_CONV_FN_P(NODE) IDENTIFIER_CONV_OP_P (DECL_NAME (NODE))

/* The type to which conversion operator FN converts to.   */
#define DECL_CONV_FN_TYPE(FN)                                                  \
  TREE_TYPE ((gcc_checking_assert (DECL_CONV_FN_P (FN)), DECL_NAME (FN)))

/* Returns nonzero iff TYPE1 and TYPE2 are the same type, in the usual
   sense of `same'.  */
#define same_type_p(TYPE1, TYPE2) comptypes ((TYPE1), (TYPE2), COMPARE_STRICT)

/* Nonzero if T is a type that could resolve to any kind of concrete type
   at instantiation time.  */
#define WILDCARD_TYPE_P(T)                                                     \
  (TREE_CODE (T) == TEMPLATE_TYPE_PARM || TREE_CODE (T) == TYPENAME_TYPE       \
   || TREE_CODE (T) == TYPEOF_TYPE                                             \
   || TREE_CODE (T) == BOUND_TEMPLATE_TEMPLATE_PARM                            \
   || TREE_CODE (T) == DECLTYPE_TYPE                                           \
   || TREE_CODE (T) == DEPENDENT_OPERATOR_TYPE)

/* Nonzero if T is a class (or struct or union) type.  Also nonzero
   for template type parameters, typename types, and instantiated
   template template parameters.  Keep these checks in ascending code
   order.  */
#define MAYBE_CLASS_TYPE_P(T) (WILDCARD_TYPE_P (T) || CLASS_TYPE_P (T))

/* 1 iff FUNCTION_TYPE or METHOD_TYPE has a ref-qualifier (either & or &&). */
#define FUNCTION_REF_QUALIFIED(NODE)                                           \
  TREE_LANG_FLAG_4 (FUNC_OR_METHOD_CHECK (NODE))

/* 1 iff FUNCTION_TYPE or METHOD_TYPE has &&-ref-qualifier.  */
#define FUNCTION_RVALUE_QUALIFIED(NODE)                                        \
  TREE_LANG_FLAG_5 (FUNC_OR_METHOD_CHECK (NODE))

/* Get the POINTER_TYPE to the METHOD_TYPE associated with this
   pointer to member function.  TYPE_PTRMEMFUNC_P _must_ be true,
   before using this macro.  */
#define TYPE_PTRMEMFUNC_FN_TYPE(NODE)                                          \
  (cp_build_qualified_type (TREE_TYPE (TYPE_FIELDS (NODE)),                    \
			    rs_type_quals (NODE)))

/* As above, but can be used in places that want an lvalue at the expense
   of not necessarily having the correct cv-qualifiers.  */
#define TYPE_PTRMEMFUNC_FN_TYPE_RAW(NODE) (TREE_TYPE (TYPE_FIELDS (NODE)))

/* True if this type is dependent.  This predicate is only valid if
   TYPE_DEPENDENT_P_VALID is true.  */
#define TYPE_DEPENDENT_P(NODE) TYPE_LANG_FLAG_0 (NODE)

/* True if dependent_type_p has been called for this type, with the
   result that TYPE_DEPENDENT_P is valid.  */
#define TYPE_DEPENDENT_P_VALID(NODE) TYPE_LANG_FLAG_6 (NODE)

/* Nonzero for _TYPE node means that this type does not have a trivial
   destructor.  Therefore, destroying an object of this type will
   involve a call to a destructor.  This can apply to objects of
   ARRAY_TYPE if the type of the elements needs a destructor.  */
#define TYPE_HAS_NONTRIVIAL_DESTRUCTOR(NODE) (TYPE_LANG_FLAG_4 (NODE))

/* For FUNCTION_TYPE or METHOD_TYPE, a list of the exceptions that
   this type can raise.  Each TREE_VALUE is a _TYPE.  The TREE_VALUE
   will be NULL_TREE to indicate a throw specification of `()', or
   no exceptions allowed.  For a noexcept specification, TREE_VALUE
   is NULL_TREE and TREE_PURPOSE is the constant-expression.  For
   a deferred noexcept-specification, TREE_PURPOSE is a DEFERRED_NOEXCEPT
   (for templates) or an OVERLOAD list of functions (for implicitly
   declared functions).  */
#define TYPE_RAISES_EXCEPTIONS(NODE)                                           \
  TYPE_LANG_SLOT_1 (FUNC_OR_METHOD_CHECK (NODE))

/* Identifiers map directly to block or class-scope bindings.
   Namespace-scope bindings are held in hash tables on the respective
   namespaces.  The identifier bindings are the innermost active
   binding, from whence you can get the decl and/or implicit-typedef
   of an elaborated type.   When not bound to a local entity the
   values are NULL.  */
#define IDENTIFIER_BINDING(NODE) (LANG_IDENTIFIER_CAST (NODE)->bindings)

#define LANG_IDENTIFIER_CAST(NODE)                                             \
  ((struct lang_identifier *) IDENTIFIER_NODE_CHECK (NODE))

/* IF_STMT accessors. These give access to the condition of the if
   statement, the then block of the if statement, and the else block
   of the if statement if it exists.  */
#define IF_COND(NODE) TREE_OPERAND (IF_STMT_CHECK (NODE), 0)
#define THEN_CLAUSE(NODE) TREE_OPERAND (IF_STMT_CHECK (NODE), 1)
#define ELSE_CLAUSE(NODE) TREE_OPERAND (IF_STMT_CHECK (NODE), 2)
#define IF_SCOPE(NODE) TREE_OPERAND (IF_STMT_CHECK (NODE), 3)
#define IF_STMT_CONSTEXPR_P(NODE) TREE_LANG_FLAG_0 (IF_STMT_CHECK (NODE))
#define IF_STMT_CONSTEVAL_P(NODE) TREE_LANG_FLAG_2 (IF_STMT_CHECK (NODE))

/* The expression in question for a DECLTYPE_TYPE.  */
#define DECLTYPE_TYPE_EXPR(NODE) (TYPE_VALUES_RAW (DECLTYPE_TYPE_CHECK (NODE)))

// Below macros are copied from gcc/c-family/c-common.h

/* In a FIELD_DECL, nonzero if the decl was originally a bitfield.  */
#define DECL_C_BIT_FIELD(NODE) (DECL_LANG_FLAG_4 (FIELD_DECL_CHECK (NODE)) == 1)
#define SET_DECL_C_BIT_FIELD(NODE)                                             \
  (DECL_LANG_FLAG_4 (FIELD_DECL_CHECK (NODE)) = 1)
#define CLEAR_DECL_C_BIT_FIELD(NODE)                                           \
  (DECL_LANG_FLAG_4 (FIELD_DECL_CHECK (NODE)) = 0)

/* True if the decl was an unnamed bitfield.  */
#define DECL_UNNAMED_BIT_FIELD(NODE)                                           \
  (DECL_C_BIT_FIELD (NODE) && !DECL_NAME (NODE))

/* 1 iff NODE is function-local.  */
#define DECL_FUNCTION_SCOPE_P(NODE)                                            \
  (DECL_CONTEXT (NODE) && TREE_CODE (DECL_CONTEXT (NODE)) == FUNCTION_DECL)

/* Nonzero if this type is const-qualified, but not
   volatile-qualified.  Other qualifiers are ignored.  This macro is
   used to test whether or not it is OK to bind an rvalue to a
   reference.  */
#define RS_TYPE_CONST_NON_VOLATILE_P(NODE)                                     \
  ((rs_type_quals (NODE) & (TYPE_QUAL_CONST | TYPE_QUAL_VOLATILE))             \
   == TYPE_QUAL_CONST)

/* Returns true if TYPE is an integral or enumeration name.  Keep
   these checks in ascending code order.  */
#define INTEGRAL_OR_ENUMERATION_TYPE_P(TYPE)                                   \
  (TREE_CODE (TYPE) == ENUMERAL_TYPE || RS_INTEGRAL_TYPE_P (TYPE))

/* Nonzero for a VAR_DECL that was initialized with a
   constant-expression.  */
#define DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P(NODE)                        \
  (TREE_LANG_FLAG_2 (VAR_DECL_CHECK (NODE)))

// Above macros are copied from gcc/c-family/c-common.h

// Below macros are copied from gcc/cp/name-lookup.cc

/* Create an overload suitable for recording an artificial TYPE_DECL
   and another decl.  We use this machanism to implement the struct
   stat hack.  */

#define STAT_HACK_P(N) ((N) && TREE_CODE (N) == OVERLOAD && OVL_LOOKUP_P (N))
#define STAT_TYPE_VISIBLE_P(N) TREE_USED (OVERLOAD_CHECK (N))
#define STAT_TYPE(N) TREE_TYPE (N)
#define STAT_DECL(N) OVL_FUNCTION (N)
#define STAT_VISIBLE(N) OVL_CHAIN (N)
#define MAYBE_STAT_DECL(N) (STAT_HACK_P (N) ? STAT_DECL (N) : N)
#define MAYBE_STAT_TYPE(N) (STAT_HACK_P (N) ? STAT_TYPE (N) : NULL_TREE)

/* When a STAT_HACK_P is true, OVL_USING_P and OVL_EXPORT_P are valid
   and apply to the hacked type.  */

/* For regular (maybe) overloaded functions, we have OVL_HIDDEN_P.
   But we also need to indicate hiddenness on implicit type decls
   (injected friend classes), and (coming soon) decls injected from
   block-scope externs.  It is too awkward to press the existing
   overload marking for that.  If we have a hidden non-function, we
   always create a STAT_HACK, and use these two markers as needed.  */
#define STAT_TYPE_HIDDEN_P(N) OVL_HIDDEN_P (N)
#define STAT_DECL_HIDDEN_P(N) OVL_DEDUP_P (N)

// Above macros are copied from gcc/cp/name-lookup.cc

// forked from gcc/cp/name-lookup.h

/* Datatype that represents binding established by a declaration between
   a name and a C++ entity.  */
struct GTY (()) cxx_binding
{
  /* Link to chain together various bindings for this name.  */
  cxx_binding *previous;
  /* The non-type entity this name is bound to.  */
  tree value;
  /* The type entity this name is bound to.  */
  tree type;

  bool value_is_inherited : 1;
  bool is_local : 1;
  bool type_is_hidden : 1;
};

// forked from gcc/c-family/c-common.h c_common_identifier

/* Identifier part common to the C front ends.  Inherits from
   tree_identifier, despite appearances.  */
struct GTY (()) c_common_identifier
{
  struct tree_common common;
  struct cpp_hashnode node; // from cpplib.h
};

// forked from gcc/cp/cp-tree.h lang_identifier

/* Language-dependent contents of an identifier.  */

struct GTY (()) lang_identifier
{
  struct c_common_identifier c_common;
  cxx_binding *bindings;
};

// forked from gcc/cp/cp-tree.h tree_overload

/* OVL_HIDDEN_P nodes come before other nodes.  */

struct GTY (()) tree_overload
{
  struct tree_common common;
  tree function;
};

// forked from gcc/cp/cp-tree.h ovl_iterator

class ovl_iterator
{
  tree ovl;
  const bool allow_inner; /* Only used when checking.  */

public:
  explicit ovl_iterator (tree o, bool allow = false)
    : ovl (o), allow_inner (allow)
  {}

public:
  operator bool () const { return ovl; }
  ovl_iterator &operator++ ()
  {
    ovl = TREE_CODE (ovl) != OVERLOAD ? NULL_TREE : OVL_CHAIN (ovl);
    return *this;
  }
  tree operator* () const
  {
    tree fn = TREE_CODE (ovl) != OVERLOAD ? ovl : OVL_FUNCTION (ovl);

    /* Check this is not an unexpected 2-dimensional overload.  */
    gcc_checking_assert (allow_inner || TREE_CODE (fn) != OVERLOAD);

    return fn;
  }
  bool operator== (const ovl_iterator &o) const { return ovl == o.ovl; }
  tree get_using () const
  {
    gcc_checking_assert (using_p ());
    return ovl;
  }

public:
  /* Whether this overload was introduced by a using decl.  */
  bool using_p () const
  {
    return (TREE_CODE (ovl) == USING_DECL
	    || (TREE_CODE (ovl) == OVERLOAD && OVL_USING_P (ovl)));
  }
  /* Whether this using is being exported.  */
  bool exporting_p () const { return OVL_EXPORT_P (get_using ()); }

  bool hidden_p () const
  {
    return TREE_CODE (ovl) == OVERLOAD && OVL_HIDDEN_P (ovl);
  }

public:
  tree remove_node (tree head) { return remove_node (head, ovl); }
  tree reveal_node (tree head) { return reveal_node (head, ovl); }

protected:
  /* If we have a nested overload, point at the inner overload and
     return the next link on the outer one.  */
  tree maybe_push ()
  {
    tree r = NULL_TREE;

    if (ovl && TREE_CODE (ovl) == OVERLOAD && OVL_NESTED_P (ovl))
      {
	r = OVL_CHAIN (ovl);
	ovl = OVL_FUNCTION (ovl);
      }
    return r;
  }
  /* Restore an outer nested overload.  */
  void pop (tree outer)
  {
    gcc_checking_assert (!ovl);
    ovl = outer;
  }

private:
  /* We make these static functions to avoid the address of the
     iterator escaping the local context.  */
  static tree remove_node (tree head, tree node);
  static tree reveal_node (tree ovl, tree node);
};

// forked from gcc/cp/cp-tree.h treee_pair_s

struct GTY (()) tree_pair_s
{
  tree purpose;
  tree value;
};

// forked from gcc/cp/cp-tree.h tree_pair_p

typedef tree_pair_s *tree_pair_p;

// forked from gcc/cp/cp-tree.h lang_type

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
struct GTY (()) lang_type
{
  unsigned char align;

  unsigned has_type_conversion : 1;
  unsigned has_copy_ctor : 1;
  unsigned has_default_ctor : 1;
  unsigned const_needs_init : 1;
  unsigned ref_needs_init : 1;
  unsigned has_const_copy_assign : 1;
  unsigned use_template : 2;

  unsigned has_mutable : 1;
  unsigned com_interface : 1;
  unsigned non_pod_class : 1;
  unsigned nearly_empty_p : 1;
  unsigned user_align : 1;
  unsigned has_copy_assign : 1;
  unsigned has_new : 1;
  unsigned has_array_new : 1;

  unsigned gets_delete : 2;
  unsigned interface_only : 1;
  unsigned interface_unknown : 1;
  unsigned contains_empty_class_p : 1;
  unsigned anon_aggr : 1;
  unsigned non_zero_init : 1;
  unsigned empty_p : 1;
  /* 32 bits allocated.  */

  unsigned vec_new_uses_cookie : 1;
  unsigned declared_class : 1;
  unsigned diamond_shaped : 1;
  unsigned repeated_base : 1;
  unsigned being_defined : 1;
  unsigned debug_requested : 1;
  unsigned fields_readonly : 1;
  unsigned ptrmemfunc_flag : 1;

  unsigned lazy_default_ctor : 1;
  unsigned lazy_copy_ctor : 1;
  unsigned lazy_copy_assign : 1;
  unsigned lazy_destructor : 1;
  unsigned has_const_copy_ctor : 1;
  unsigned has_complex_copy_ctor : 1;
  unsigned has_complex_copy_assign : 1;
  unsigned non_aggregate : 1;

  unsigned has_complex_dflt : 1;
  unsigned has_list_ctor : 1;
  unsigned non_std_layout : 1;
  unsigned is_literal : 1;
  unsigned lazy_move_ctor : 1;
  unsigned lazy_move_assign : 1;
  unsigned has_complex_move_ctor : 1;
  unsigned has_complex_move_assign : 1;

  unsigned has_constexpr_ctor : 1;
  unsigned unique_obj_representations : 1;
  unsigned unique_obj_representations_set : 1;
  bool erroneous : 1;
  bool non_pod_aggregate : 1;

  /* When adding a flag here, consider whether or not it ought to
     apply to a template instance if it applies to the template.  If
     so, make sure to copy it in instantiate_class_template!  */

  /* There are some bits left to fill out a 32-bit word.  Keep track
     of this by updating the size of this bitfield whenever you add or
     remove a flag.  */
  unsigned dummy : 3;

  tree primary_base;
  vec<tree_pair_s, va_gc> *vcall_indices;
  tree vtables;
  tree typeinfo_var;
  vec<tree, va_gc> *vbases;
  tree as_base;
  vec<tree, va_gc> *pure_virtuals;
  tree friend_classes;
  vec<tree, va_gc> *GTY ((reorder ("resort_type_member_vec"))) members;
  tree key_method;
  tree decl_list;
  tree befriending_classes;
  /* In a RECORD_TYPE, information specific to Objective-C++, such
     as a list of adopted protocols or a pointer to a corresponding
     @interface.  See objc/objc-act.h for details.  */
  tree objc_info;
  /* FIXME reuse another field?  */
  tree lambda_expr;
};

namespace Rust {

// forked from gcc/cp/cp-tree.h cp_ref_qualifier

enum rs_ref_qualifier
{
  REF_QUAL_NONE = 0,
  REF_QUAL_LVALUE = 1,
  REF_QUAL_RVALUE = 2
};

// forked from gcc/cp/cp-tree.h tsubst_flags

/* Bitmask flags to control type substitution.  */
enum tsubst_flags
{
  tf_none = 0,			/* nothing special */
  tf_error = 1 << 0,		/* give error messages  */
  tf_warning = 1 << 1,		/* give warnings too  */
  tf_ignore_bad_quals = 1 << 2, /* ignore bad cvr qualifiers */
  tf_keep_type_decl = 1 << 3,	/* retain typedef type decls
				   (make_typename_type use) */
  tf_ptrmem_ok = 1 << 4,	/* pointers to member ok (internal
				   instantiate_type use) */
  tf_user = 1 << 5,		/* found template must be a user template
				   (lookup_template_class use) */
  tf_conv = 1 << 6,		/* We are determining what kind of
				   conversion might be permissible,
				   not actually performing the
				   conversion.  */
  tf_decltype = 1 << 7,		/* We are the operand of decltype.
				   Used to implement the special rules
				   for calls in decltype (5.2.2/11).  */
  tf_partial = 1 << 8,		/* Doing initial explicit argument
				   substitution in fn_type_unification.  */
  tf_fndecl_type = 1 << 9,	/* Substituting the type of a function
				   declaration.  */
  tf_no_cleanup = 1 << 10,	/* Do not build a cleanup
				   (build_target_expr and friends) */
  tf_norm = 1 << 11,		/* Build diagnostic information during
				   constraint normalization.  */
  /* Convenient substitution flags combinations.  */
  tf_warning_or_error = tf_warning | tf_error
};

// forked from gcc/cp/cp-tree.h cp_identifier_kind

/* Kinds of identifiers.  Values are carefully chosen.  */
enum cp_identifier_kind
{
  cik_normal = 0,	      /* Not a special identifier.  */
  cik_keyword = 1,	      /* A keyword.  */
  cik_ctor = 2,		      /* Constructor (in-chg, complete or base).  */
  cik_dtor = 3,		      /* Destructor (in-chg, deleting, complete or
				 base).  */
  cik_simple_op = 4,	      /* Non-assignment operator name.  */
  cik_assign_op = 5,	      /* An assignment operator name.  */
  cik_conv_op = 6,	      /* Conversion operator name.  */
  cik_reserved_for_udlit = 7, /* Not yet in use  */
  cik_max
};

// forked from gcc/cp/cp-tree.h tag_types

/* An enumeration of the kind of tags that C++ accepts.  */
enum tag_types
{
  none_type = 0, /* Not a tag type.  */
  record_type,	 /* "struct" types.  */
  class_type,	 /* "class" types.  */
  union_type,	 /* "union" types.  */
  enum_type,	 /* "enum" types.  */
  typename_type, /* "typename" types.  */
  scope_type	 /* namespace or tagged type name followed by :: */
};

// forked from gcc/cp/cp-tree.h tsubst_flags_t

/* This type is used for parameters and variables which hold
   combinations of the flags in enum tsubst_flags.  */
typedef int tsubst_flags_t;

// forked from gcc/cp/cvt.cc convert_to_void
//
// When an expression is used in a void context, its value is discarded and
// no lvalue-rvalue and similar conversions happen [expr.static.cast/4,
// stmt.expr/1, expr.comma/1].  This permits dereferencing an incomplete type
// in a void context. The C++ standard does not define what an `access' to an
// object is, but there is reason to believe that it is the lvalue to rvalue
// conversion -- if it were not, `*&*p = 1' would violate [expr]/4 in that it
// accesses `*p' not to calculate the value to be stored. But, dcl.type.cv/8
// indicates that volatile semantics should be the same between C and C++
// where ever possible. C leaves it implementation defined as to what
// constitutes an access to a volatile. So, we interpret `*vp' as a read of
// the volatile object `vp' points to, unless that is an incomplete type. For
// volatile references we do not do this interpretation, because that would
// make it impossible to ignore the reference return value from functions. We
// issue warnings in the confusing cases.
//
// The IMPLICIT is ICV_CAST when the user is explicitly converting an
// expression to void via a cast. If an expression is being implicitly
// converted, IMPLICIT indicates the context of the implicit conversion.

/* Possible cases of implicit or explicit bad conversions to void. */
enum impl_conv_void
{
  ICV_CAST,	      /* (explicit) conversion to void */
  ICV_SECOND_OF_COND, /* second operand of conditional expression */
  ICV_THIRD_OF_COND,  /* third operand of conditional expression */
  ICV_RIGHT_OF_COMMA, /* right operand of comma operator */
  ICV_LEFT_OF_COMMA,  /* left operand of comma operator */
  ICV_STATEMENT,      /* statement */
  ICV_THIRD_IN_FOR    /* for increment expression */
};

/* BUILT_IN_FRONTEND function codes.  */
enum rs_built_in_function
{
  RS_BUILT_IN_IS_CONSTANT_EVALUATED,
  RS_BUILT_IN_INTEGER_PACK,
  RS_BUILT_IN_IS_CORRESPONDING_MEMBER,
  RS_BUILT_IN_IS_POINTER_INTERCONVERTIBLE_WITH_CLASS,
  RS_BUILT_IN_SOURCE_LOCATION,
  RS_BUILT_IN_LAST
};

extern tree
convert_to_void (tree expr, impl_conv_void implicit);

// The lvalue-to-rvalue conversion (7.1) is applied if and only if the
// expression is a glvalue of volatile-qualified type and it is one of the
// following:
// * ( expression ), where expression is one of these expressions,
// * id-expression (8.1.4),
// * subscripting (8.2.1),
// * class member access (8.2.5),
// * indirection (8.3.1),
// * pointer-to-member operation (8.5),
// * conditional expression (8.16) where both the second and the third
//   operands are one of these expressions, or
// * comma expression (8.19) where the right operand is one of these
//   expressions.
extern tree
mark_discarded_use (tree expr);

// Mark EXP as read, not just set, for set but not used -Wunused warning
// purposes.
extern void
mark_exp_read (tree exp);

// We've seen an actual use of EXPR.  Possibly replace an outer variable
// reference inside with its constant value or a lambda capture.
extern tree
mark_use (tree expr, bool rvalue_p, bool read_p, location_t loc,
	  bool reject_builtin);

// Called whenever the expression EXPR is used in an rvalue context.
// When REJECT_BUILTIN is true the expression is checked to make sure
// it doesn't make it possible to obtain the address of a GCC built-in
// function with no library fallback (or any of its bits, such as in
// a conversion to bool).
extern tree
mark_rvalue_use (tree e, location_t loc /* = UNKNOWN_LOCATION */,
		 bool reject_builtin /* = true */);

// Called whenever an expression is used in an lvalue context.
extern tree
mark_lvalue_use (tree expr);

// As above, but don't consider this use a read.
extern tree
mark_lvalue_use_nonread (tree expr);

// We are using a reference VAL for its value. Bash that reference all the way
// down to its lowest form.
extern tree
convert_from_reference (tree val);

// Subroutine of convert_to_void.  Warn if we're discarding something with
// attribute [[nodiscard]].
extern void
maybe_warn_nodiscard (tree expr, impl_conv_void implicit);

extern location_t
expr_loc_or_loc (const_tree t, location_t or_loc);

extern location_t
expr_loc_or_input_loc (const_tree t);

// FN is the callee of a CALL_EXPR or AGGR_INIT_EXPR; return the FUNCTION_DECL
// if we can.
extern tree
get_fndecl_from_callee (tree fn);

// FIXME some helpers from HIRCompileBase could probably be moved here over time

// Return an expression for the address of BASE[INDEX], used in offset intrinsic
extern tree
pointer_offset_expression (tree base_tree, tree index_tree, location_t locus);

/* A tree node, together with a location, so that we can track locations
   (and ranges) during parsing.

   The location is redundant for node kinds that have locations,
   but not all node kinds do (e.g. constants, and references to
   params, locals, etc), so we stash a copy here.  */

extern location_t rs_expr_location (const_tree);

extern int
is_empty_class (tree type);

extern tree array_type_nelts_top (tree);

extern bool
is_really_empty_class (tree, bool);

extern bool builtin_valid_in_constant_expr_p (const_tree);

extern bool maybe_constexpr_fn (tree);

extern bool var_in_maybe_constexpr_fn (tree);

extern int
rs_type_quals (const_tree type);

inline bool type_unknown_p (const_tree);

extern bool decl_maybe_constant_var_p (tree);

extern void
init_modules ();

extern bool var_in_constexpr_fn (tree);

inline tree ovl_first (tree) ATTRIBUTE_PURE;

inline bool type_unknown_p (const_tree);

extern tree
rs_walk_subtrees (tree *, int *, walk_tree_fn, void *, hash_set<tree> *);
#define rs_walk_tree(tp, func, data, pset)                                     \
  walk_tree_1 (tp, func, data, pset, rs_walk_subtrees)
#define rs_walk_tree_without_duplicates(tp, func, data)                        \
  walk_tree_without_duplicates_1 (tp, func, data, rs_walk_subtrees)

// forked from gcc/cp/cp-tree.h cp_expr_loc_or_loc

inline location_t
rs_expr_loc_or_loc (const_tree t, location_t or_loc)
{
  location_t loc = rs_expr_location (t);
  if (loc == UNKNOWN_LOCATION)
    loc = or_loc;
  return loc;
}

// forked from gcc/cp/cp-tree.h cp_expr_loc_or_input_loc

inline location_t
rs_expr_loc_or_input_loc (const_tree t)
{
  return rs_expr_loc_or_loc (t, input_location);
}

// forked from gcc/cp/cp-tree.h type_unknown_p

inline bool
type_unknown_p (const_tree expr)
{
  return TREE_TYPE (expr) == unknown_type_node;
}

// forked from gcc/cp/cp-tree.h ovl_first

/* Inline bodies.  */

inline tree
ovl_first (tree node)
{
  while (TREE_CODE (node) == OVERLOAD)
    node = OVL_FUNCTION (node);
  return node;
}

// forked from gcc/cp/cp-tree.h type_of_this_parm

/* Return the type of the `this' parameter of FNTYPE.  */

inline tree
type_of_this_parm (const_tree fntype)
{
  function_args_iterator iter;
  gcc_assert (TREE_CODE (fntype) == METHOD_TYPE);
  function_args_iter_init (&iter, fntype);
  return function_args_iter_cond (&iter);
}

// forked from gcc/cp/cp-tree.h class_of_this_parm

/* Return the class of the `this' parameter of FNTYPE.  */

inline tree
class_of_this_parm (const_tree fntype)
{
  return TREE_TYPE (type_of_this_parm (fntype));
}
} // namespace Rust

#endif // RUST_TREE
