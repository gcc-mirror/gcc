// Copyright (C) 2020-2023 Free Software Foundation, Inc.

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
#include "splay-tree.h"

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

// lambda?
#define RS_CLOSURE_FLAG TREE_LANG_FLAG_1
#define RS_CLOSURE_TYPE_P(TYPE)                                                \
  (TREE_CODE (TYPE) == RECORD_TYPE && TREE_LANG_FLAG_1 (TYPE))

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

// forked from gcc/cp/c-common.h c_tree_index

/* Standard named or nameless data types of the C compiler.  */

enum c_tree_index
{
  CTI_CHAR8_TYPE,
  CTI_CHAR16_TYPE,
  CTI_CHAR32_TYPE,
  CTI_WCHAR_TYPE,
  CTI_UNDERLYING_WCHAR_TYPE,
  CTI_WINT_TYPE,
  CTI_SIGNED_SIZE_TYPE,	     /* For format checking only.  */
  CTI_UNSIGNED_PTRDIFF_TYPE, /* For format checking only.  */
  CTI_INTMAX_TYPE,
  CTI_UINTMAX_TYPE,
  CTI_WIDEST_INT_LIT_TYPE,
  CTI_WIDEST_UINT_LIT_TYPE,

  /* Types for <stdint.h>, that may not be defined on all
     targets.  */
  CTI_SIG_ATOMIC_TYPE,
  CTI_INT8_TYPE,
  CTI_INT16_TYPE,
  CTI_INT32_TYPE,
  CTI_INT64_TYPE,
  CTI_UINT8_TYPE,
  CTI_UINT16_TYPE,
  CTI_UINT32_TYPE,
  CTI_UINT64_TYPE,
  CTI_INT_LEAST8_TYPE,
  CTI_INT_LEAST16_TYPE,
  CTI_INT_LEAST32_TYPE,
  CTI_INT_LEAST64_TYPE,
  CTI_UINT_LEAST8_TYPE,
  CTI_UINT_LEAST16_TYPE,
  CTI_UINT_LEAST32_TYPE,
  CTI_UINT_LEAST64_TYPE,
  CTI_INT_FAST8_TYPE,
  CTI_INT_FAST16_TYPE,
  CTI_INT_FAST32_TYPE,
  CTI_INT_FAST64_TYPE,
  CTI_UINT_FAST8_TYPE,
  CTI_UINT_FAST16_TYPE,
  CTI_UINT_FAST32_TYPE,
  CTI_UINT_FAST64_TYPE,
  CTI_INTPTR_TYPE,
  CTI_UINTPTR_TYPE,

  CTI_CHAR_ARRAY_TYPE,
  CTI_CHAR8_ARRAY_TYPE,
  CTI_CHAR16_ARRAY_TYPE,
  CTI_CHAR32_ARRAY_TYPE,
  CTI_WCHAR_ARRAY_TYPE,
  CTI_STRING_TYPE,
  CTI_CONST_STRING_TYPE,

  /* Type for boolean expressions (bool in C++, int in C).  */
  CTI_TRUTHVALUE_TYPE,
  CTI_TRUTHVALUE_TRUE,
  CTI_TRUTHVALUE_FALSE,

  CTI_DEFAULT_FUNCTION_TYPE,

  CTI_NULL,

  /* These are not types, but we have to look them up all the time.  */
  CTI_FUNCTION_NAME_DECL,
  CTI_PRETTY_FUNCTION_NAME_DECL,
  CTI_C99_FUNCTION_NAME_DECL,

  CTI_MODULE_HWM,
  /* Below here entities change during compilation.  */

  CTI_SAVED_FUNCTION_NAME_DECLS,

  CTI_MAX
};

// forked from gcc/c-family/c-common.h c_tree_index

extern GTY (()) tree c_global_trees[CTI_MAX];

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

#define char8_type_node c_global_trees[CTI_CHAR8_TYPE]
#define char16_type_node c_global_trees[CTI_CHAR16_TYPE]
#define char32_type_node c_global_trees[CTI_CHAR32_TYPE]
#define wchar_type_node c_global_trees[CTI_WCHAR_TYPE]
#define underlying_wchar_type_node c_global_trees[CTI_UNDERLYING_WCHAR_TYPE]
#define wint_type_node c_global_trees[CTI_WINT_TYPE]
#define signed_size_type_node c_global_trees[CTI_SIGNED_SIZE_TYPE]
#define unsigned_ptrdiff_type_node c_global_trees[CTI_UNSIGNED_PTRDIFF_TYPE]
#define intmax_type_node c_global_trees[CTI_INTMAX_TYPE]
#define uintmax_type_node c_global_trees[CTI_UINTMAX_TYPE]
#define widest_integer_literal_type_node c_global_trees[CTI_WIDEST_INT_LIT_TYPE]
#define widest_unsigned_literal_type_node                                      \
  c_global_trees[CTI_WIDEST_UINT_LIT_TYPE]

#define sig_atomic_type_node c_global_trees[CTI_SIG_ATOMIC_TYPE]
#define int8_type_node c_global_trees[CTI_INT8_TYPE]
#define int16_type_node c_global_trees[CTI_INT16_TYPE]
#define int32_type_node c_global_trees[CTI_INT32_TYPE]
#define int64_type_node c_global_trees[CTI_INT64_TYPE]
#define uint8_type_node c_global_trees[CTI_UINT8_TYPE]
#define c_uint16_type_node c_global_trees[CTI_UINT16_TYPE]
#define c_uint32_type_node c_global_trees[CTI_UINT32_TYPE]
#define c_uint64_type_node c_global_trees[CTI_UINT64_TYPE]
#define int_least8_type_node c_global_trees[CTI_INT_LEAST8_TYPE]
#define int_least16_type_node c_global_trees[CTI_INT_LEAST16_TYPE]
#define int_least32_type_node c_global_trees[CTI_INT_LEAST32_TYPE]
#define int_least64_type_node c_global_trees[CTI_INT_LEAST64_TYPE]
#define uint_least8_type_node c_global_trees[CTI_UINT_LEAST8_TYPE]
#define uint_least16_type_node c_global_trees[CTI_UINT_LEAST16_TYPE]
#define uint_least32_type_node c_global_trees[CTI_UINT_LEAST32_TYPE]
#define uint_least64_type_node c_global_trees[CTI_UINT_LEAST64_TYPE]
#define int_fast8_type_node c_global_trees[CTI_INT_FAST8_TYPE]
#define int_fast16_type_node c_global_trees[CTI_INT_FAST16_TYPE]
#define int_fast32_type_node c_global_trees[CTI_INT_FAST32_TYPE]
#define int_fast64_type_node c_global_trees[CTI_INT_FAST64_TYPE]
#define uint_fast8_type_node c_global_trees[CTI_UINT_FAST8_TYPE]
#define uint_fast16_type_node c_global_trees[CTI_UINT_FAST16_TYPE]
#define uint_fast32_type_node c_global_trees[CTI_UINT_FAST32_TYPE]
#define uint_fast64_type_node c_global_trees[CTI_UINT_FAST64_TYPE]
#define intptr_type_node c_global_trees[CTI_INTPTR_TYPE]
#define uintptr_type_node c_global_trees[CTI_UINTPTR_TYPE]

#define truthvalue_type_node c_global_trees[CTI_TRUTHVALUE_TYPE]
#define truthvalue_true_node c_global_trees[CTI_TRUTHVALUE_TRUE]
#define truthvalue_false_node c_global_trees[CTI_TRUTHVALUE_FALSE]

#define char_array_type_node c_global_trees[CTI_CHAR_ARRAY_TYPE]
#define char8_array_type_node c_global_trees[CTI_CHAR8_ARRAY_TYPE]
#define char16_array_type_node c_global_trees[CTI_CHAR16_ARRAY_TYPE]
#define char32_array_type_node c_global_trees[CTI_CHAR32_ARRAY_TYPE]
#define wchar_array_type_node c_global_trees[CTI_WCHAR_ARRAY_TYPE]
#define string_type_node c_global_trees[CTI_STRING_TYPE]
#define const_string_type_node c_global_trees[CTI_CONST_STRING_TYPE]

#define default_function_type c_global_trees[CTI_DEFAULT_FUNCTION_TYPE]

#define function_name_decl_node c_global_trees[CTI_FUNCTION_NAME_DECL]
#define pretty_function_name_decl_node                                         \
  c_global_trees[CTI_PRETTY_FUNCTION_NAME_DECL]
#define c99_function_name_decl_node c_global_trees[CTI_C99_FUNCTION_NAME_DECL]
#define saved_function_name_decls c_global_trees[CTI_SAVED_FUNCTION_NAME_DECLS]

/* The node for C++ `__null'.  */
#define null_node c_global_trees[CTI_NULL]

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
  (rs_build_qualified_type (TREE_TYPE (TYPE_FIELDS (NODE)),                    \
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

#define SET_CLASSTYPE_INTERFACE_UNKNOWN_X(NODE, X)                             \
  (LANG_TYPE_CLASS_CHECK (NODE)->interface_unknown = !!(X))

/* Nonzero if this class is included from a header file which employs
   `#pragma interface', and it is not included in its implementation file.  */
#define CLASSTYPE_INTERFACE_ONLY(NODE)                                         \
  (LANG_TYPE_CLASS_CHECK (NODE)->interface_only)

#define TYPE_NAME_STRING(NODE) (IDENTIFIER_POINTER (TYPE_IDENTIFIER (NODE)))
#define TYPE_NAME_LENGTH(NODE) (IDENTIFIER_LENGTH (TYPE_IDENTIFIER (NODE)))

/* Whether a PARM_DECL represents a local parameter in a
   requires-expression.  */
#define CONSTRAINT_VAR_P(NODE) DECL_LANG_FLAG_2 (TREE_CHECK (NODE, PARM_DECL))

/* In a CALL_EXPR appearing in a template, true if Koenig lookup
   should be performed at instantiation time.  */
#define KOENIG_LOOKUP_P(NODE) TREE_LANG_FLAG_0 (CALL_EXPR_CHECK (NODE))

/* The index of a user-declared parameter in its function, starting at 1.
   All artificial parameters will have index 0.  */
#define DECL_PARM_INDEX(NODE) (LANG_DECL_PARM_CHECK (NODE)->index)

/* The level of a user-declared parameter in its function, starting at 1.
   A parameter of the function will have level 1; a parameter of the first
   nested function declarator (i.e. t in void f (void (*p)(T t))) will have
   level 2.  */
#define DECL_PARM_LEVEL(NODE) (LANG_DECL_PARM_CHECK (NODE)->level)

/* These flags are used by the conversion code.
   CONV_IMPLICIT   :  Perform implicit conversions (standard and user-defined).
   CONV_STATIC     :  Perform the explicit conversions for static_cast.
   CONV_CONST      :  Perform the explicit conversions for const_cast.
   CONV_REINTERPRET:  Perform the explicit conversions for reinterpret_cast.
   CONV_PRIVATE    :  Perform upcasts to private bases.
   CONV_FORCE_TEMP :  Require a new temporary when converting to the same
		      aggregate type.  */

#define CONV_IMPLICIT 1
#define CONV_STATIC 2
#define CONV_CONST 4
#define CONV_REINTERPRET 8
#define CONV_PRIVATE 16
#define CONV_FORCE_TEMP 32
#define CONV_FOLD 64
#define CONV_OLD_CONVERT                                                       \
  (CONV_IMPLICIT | CONV_STATIC | CONV_CONST | CONV_REINTERPRET)
#define CONV_C_CAST                                                            \
  (CONV_IMPLICIT | CONV_STATIC | CONV_CONST | CONV_REINTERPRET | CONV_PRIVATE  \
   | CONV_FORCE_TEMP)
#define CONV_BACKEND_CONVERT (CONV_OLD_CONVERT | CONV_FOLD)

/* Used by build_expr_type_conversion to indicate which types are
   acceptable as arguments to the expression under consideration.  */

#define WANT_INT 1		  /* integer types, including bool */
#define WANT_FLOAT 2		  /* floating point types */
#define WANT_ENUM 4		  /* enumerated types */
#define WANT_POINTER 8		  /* pointer types */
#define WANT_NULL 16		  /* null pointer constant */
#define WANT_VECTOR_OR_COMPLEX 32 /* vector or complex types */
#define WANT_ARITH (WANT_INT | WANT_FLOAT | WANT_VECTOR_OR_COMPLEX)

/* Used with comptypes, and related functions, to guide type
   comparison.  */

#define COMPARE_STRICT                                                         \
  0 /* Just check if the types are the                                         \
       same.  */
#define COMPARE_BASE                                                           \
  1 /* Check to see if the second type is                                      \
       derived from the first.  */
#define COMPARE_DERIVED                                                        \
  2 /* Like COMPARE_BASE, but in                                               \
       reverse.  */
#define COMPARE_REDECLARATION                                                  \
  4 /* The comparison is being done when                                       \
       another declaration of an existing                                      \
       entity is seen.  */
#define COMPARE_STRUCTURAL                                                     \
  8 /* The comparison is intended to be                                        \
       structural. The actual comparison                                       \
       will be identical to                                                    \
       COMPARE_STRICT.  */

/* Used with start function.  */
#define SF_DEFAULT 0 /* No flags.  */
#define SF_PRE_PARSED                                                          \
  1 /* The function declaration has                                            \
       already been parsed.  */
#define SF_INCLASS_INLINE                                                      \
  2 /* The function is an inline, defined                                      \
       in the class body.  */

/* Used with start_decl's initialized parameter.  */
#define SD_UNINITIALIZED 0
#define SD_INITIALIZED 1
/* Like SD_INITIALIZED, but also mark the new decl as DECL_DECOMPOSITION_P.  */
#define SD_DECOMPOSITION 2
#define SD_DEFAULTED 3
#define SD_DELETED 4

/* Returns nonzero iff TYPE1 and TYPE2 are the same type, in the usual
   sense of `same'.  */
#define same_type_p(TYPE1, TYPE2) comptypes ((TYPE1), (TYPE2), COMPARE_STRICT)

/* Returns true if NODE is a pointer-to-data-member.  */
#define TYPE_PTRDATAMEM_P(NODE) (TREE_CODE (NODE) == OFFSET_TYPE)

/* Nonzero if this type is const-qualified.  */
#define RS_TYPE_CONST_P(NODE) ((rs_type_quals (NODE) & TYPE_QUAL_CONST) != 0)

/* The _DECL for this _TYPE.  */
#define TYPE_MAIN_DECL(NODE) (TYPE_STUB_DECL (TYPE_MAIN_VARIANT (NODE)))

/* Nonzero for a VAR_DECL iff an explicit initializer was provided
   or a non-trivial constructor is called.  */
#define DECL_NONTRIVIALLY_INITIALIZED_P(NODE)                                  \
  (TREE_LANG_FLAG_6 (VAR_DECL_CHECK (NODE)))

/* Nonzero if DECL was declared with '= default' (maybe implicitly).  */
#define DECL_DEFAULTED_FN(DECL) (LANG_DECL_FN_CHECK (DECL)->defaulted_p)

/* Nonzero for a class type means that the class type has a
   user-declared constructor.  */
#define TYPE_HAS_USER_CONSTRUCTOR(NODE) (TYPE_LANG_FLAG_1 (NODE))

/* A FUNCTION_DECL or OVERLOAD for the constructors for NODE.  These
   are the constructors that take an in-charge parameter.  */
#define CLASSTYPE_CONSTRUCTORS(NODE)                                           \
  (get_class_binding_direct (NODE, ctor_identifier))

/* Nonzero if the DECL was initialized in the class definition itself,
   rather than outside the class.  This is used for both static member
   VAR_DECLS, and FUNCTION_DECLS that are defined in the class.  */
#define DECL_INITIALIZED_IN_CLASS_P(DECL)                                      \
  (DECL_LANG_SPECIFIC (VAR_OR_FUNCTION_DECL_CHECK (DECL))                      \
     ->u.base.initialized_in_class)

/* Nonzero if DECL is explicitly defaulted in the class body.  */
#define DECL_DEFAULTED_IN_CLASS_P(DECL)                                        \
  (DECL_DEFAULTED_FN (DECL) && DECL_INITIALIZED_IN_CLASS_P (DECL))

/* Nonzero for FUNCTION_DECL means that this decl is a non-static
   member function.  */
#define DECL_NONSTATIC_MEMBER_FUNCTION_P(NODE)                                 \
  (TREE_CODE (TREE_TYPE (NODE)) == METHOD_TYPE)

/* For FUNCTION_DECLs: nonzero means that this function is a
   constructor or a destructor with an extra in-charge parameter to
   control whether or not virtual bases are constructed.  */
#define DECL_HAS_IN_CHARGE_PARM_P(NODE)                                        \
  (LANG_DECL_FN_CHECK (NODE)->has_in_charge_parm_p)

/* Nonzero if the VTT parm has been added to NODE.  */
#define DECL_HAS_VTT_PARM_P(NODE) (LANG_DECL_FN_CHECK (NODE)->has_vtt_parm_p)

/* Given a FUNCTION_DECL, returns the first TREE_LIST out of TYPE_ARG_TYPES
   which refers to a user-written parameter.  */
#define FUNCTION_FIRST_USER_PARMTYPE(NODE)                                     \
  skip_artificial_parms_for ((NODE), TYPE_ARG_TYPES (TREE_TYPE (NODE)))

/* Similarly, but for DECL_ARGUMENTS.  */
#define FUNCTION_FIRST_USER_PARM(NODE)                                         \
  skip_artificial_parms_for ((NODE), DECL_ARGUMENTS (NODE))

/* For FUNCTION_DECLs and TEMPLATE_DECLs: nonzero means that this function
   is a constructor.  */
#define DECL_CONSTRUCTOR_P(NODE) DECL_CXX_CONSTRUCTOR_P (NODE)

/* Nonzero if DECL was declared with '= delete'.  */
#define DECL_DELETED_FN(DECL)                                                  \
  (LANG_DECL_FN_CHECK (DECL)->min.base.threadprivate_or_deleted_p)

/* Nonzero if DECL was declared with '= default' (maybe implicitly).  */
#define DECL_DEFAULTED_FN(DECL) (LANG_DECL_FN_CHECK (DECL)->defaulted_p)

/* True if NODE is a brace-enclosed initializer.  */
#define BRACE_ENCLOSED_INITIALIZER_P(NODE)                                     \
  (TREE_CODE (NODE) == CONSTRUCTOR && TREE_TYPE (NODE) == init_list_type_node)

/* True if FNDECL is an immediate function.  */
#define DECL_IMMEDIATE_FUNCTION_P(NODE)                                        \
  (DECL_LANG_SPECIFIC (FUNCTION_DECL_CHECK (NODE))                             \
     ? LANG_DECL_FN_CHECK (NODE)->immediate_fn_p                               \
     : false)
#define SET_DECL_IMMEDIATE_FUNCTION_P(NODE)                                    \
  (retrofit_lang_decl (FUNCTION_DECL_CHECK (NODE)),                            \
   LANG_DECL_FN_CHECK (NODE)->immediate_fn_p = true)

/* True if this CONSTRUCTOR should not be used as a variable initializer
   because it was loaded from a constexpr variable with mutable fields.  */
#define CONSTRUCTOR_MUTABLE_POISON(NODE)                                       \
  (TREE_LANG_FLAG_2 (CONSTRUCTOR_CHECK (NODE)))

/* For a pointer-to-member constant `X::Y' this is the _DECL for
   `Y'.  */
#define PTRMEM_CST_MEMBER(NODE)                                                \
  (((ptrmem_cst_t) PTRMEM_CST_CHECK (NODE))->member)

/* Indicates whether a COMPONENT_REF or a SCOPE_REF has been parenthesized, an
   INDIRECT_REF comes from parenthesizing a _DECL, or a PAREN_EXPR identifies a
   parenthesized initializer relevant for decltype(auto).  Currently only set
   some of the time in C++14 mode.  */

#define REF_PARENTHESIZED_P(NODE)                                              \
  TREE_LANG_FLAG_2 (TREE_CHECK5 ((NODE), COMPONENT_REF, INDIRECT_REF,          \
				 SCOPE_REF, VIEW_CONVERT_EXPR, PAREN_EXPR))

/* Returns true if NODE is a pointer-to-member.  */
#define TYPE_PTRMEM_P(NODE)                                                    \
  (TYPE_PTRDATAMEM_P (NODE) || TYPE_PTRMEMFUNC_P (NODE))

/* Returns true if NODE is a pointer or a pointer-to-member.  */
#define TYPE_PTR_OR_PTRMEM_P(NODE) (TYPE_PTR_P (NODE) || TYPE_PTRMEM_P (NODE))

/* Nonzero if NODE is an artificial VAR_DECL for a C++17 structured binding
   declaration or one of VAR_DECLs for the user identifiers in it.  */
#define DECL_DECOMPOSITION_P(NODE)                                             \
  (VAR_P (NODE) && DECL_LANG_SPECIFIC (NODE)                                   \
     ? DECL_LANG_SPECIFIC (NODE)->u.base.selector == lds_decomp                \
     : false)

/* The underlying artificial VAR_DECL for structured binding.  */
#define DECL_DECOMP_BASE(NODE) (LANG_DECL_DECOMP_CHECK (NODE)->base)

/* Nonzero if either DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P or
   DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P is true of NODE.  */
#define DECL_MAYBE_IN_CHARGE_CDTOR_P(NODE)                                     \
  (DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P (NODE)                                   \
   || DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P (NODE))

/* Nonzero if NODE (a FUNCTION_DECL) is a destructor, but not the
   specialized in-charge constructor, in-charge deleting constructor,
   or the base destructor.  */
#define DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P(NODE)                                \
  (DECL_NAME (NODE) == dtor_identifier)

/* Nonzero if NODE (a _DECL) is a cloned constructor or
   destructor.  */
#define DECL_CLONED_FUNCTION_P(NODE)                                           \
  (DECL_NAME (NODE) && IDENTIFIER_CDTOR_P (DECL_NAME (NODE))                   \
   && !DECL_MAYBE_IN_CHARGE_CDTOR_P (NODE))

/* If DECL_CLONED_FUNCTION_P holds, this is the function that was
   cloned.  */
#define DECL_CLONED_FUNCTION(NODE)                                             \
  (DECL_LANG_SPECIFIC (FUNCTION_DECL_CHECK (NODE))->u.fn.u5.cloned_function)

/* Nonzero if NODE (a _DECL) is a cloned constructor or
   destructor.  */
#define DECL_CLONED_FUNCTION_P(NODE)                                           \
  (DECL_NAME (NODE) && IDENTIFIER_CDTOR_P (DECL_NAME (NODE))                   \
   && !DECL_MAYBE_IN_CHARGE_CDTOR_P (NODE))

/* Nonzero if NODE (a FUNCTION_DECL) is a constructor, but not either the
   specialized in-charge constructor or the specialized not-in-charge
   constructor.  */
#define DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P(NODE)                               \
  (DECL_NAME (NODE) == ctor_identifier)

/* The current C++-specific per-function global variables.  */

#define cp_function_chain (cfun->language)

/* In a constructor destructor, the point at which all derived class
   destroying/construction has been done.  I.e., just before a
   constructor returns, or before any base class destroying will be done
   in a destructor.  */

#define cdtor_label cp_function_chain->x_cdtor_label

/* When we're processing a member function, current_class_ptr is the
   PARM_DECL for the `this' pointer.  The current_class_ref is an
   expression for `*this'.  */

#define current_class_ptr                                                      \
  (*(cfun && cp_function_chain ? &cp_function_chain->x_current_class_ptr       \
			       : &scope_chain->x_current_class_ptr))
#define current_class_ref                                                      \
  (*(cfun && cp_function_chain ? &cp_function_chain->x_current_class_ref       \
			       : &scope_chain->x_current_class_ref))

/* The EH_SPEC_BLOCK for the exception-specifiers for the current
   function, if any.  */

#define current_eh_spec_block cp_function_chain->x_eh_spec_block

/* The `__in_chrg' parameter for the current function.  Only used for
   constructors and destructors.  */

#define current_in_charge_parm cp_function_chain->x_in_charge_parm

/* The `__vtt_parm' parameter for the current function.  Only used for
   constructors and destructors.  */

#define current_vtt_parm cp_function_chain->x_vtt_parm

/* A boolean flag to control whether we need to clean up the return value if a
   local destructor throws.  Only used in functions that return by value a
   class with a destructor.  Which 'tors don't, so we can use the same
   field as current_vtt_parm.  */

#define current_retval_sentinel current_vtt_parm

/* Set to 0 at beginning of a function definition, set to 1 if
   a return statement that specifies a return value is seen.  */

#define current_function_returns_value cp_function_chain->returns_value

/* Set to 0 at beginning of a function definition, set to 1 if
   a return statement with no argument is seen.  */

#define current_function_returns_null cp_function_chain->returns_null

/* Set to 0 at beginning of a function definition, set to 1 if
   a call to a noreturn function is seen.  */

#define current_function_returns_abnormally                                    \
  cp_function_chain->returns_abnormally

/* Set to 0 at beginning of a function definition, set to 1 if we see an
   obvious infinite loop.  This can have false positives and false
   negatives, so it should only be used as a heuristic.  */

#define current_function_infinite_loop cp_function_chain->infinite_loop

/* Nonzero if we are processing a base initializer.  Zero elsewhere.  */
#define in_base_initializer cp_function_chain->x_in_base_initializer

#define in_function_try_handler cp_function_chain->x_in_function_try_handler

/* Expression always returned from function, or error_mark_node
   otherwise, for use by the automatic named return value optimization.  */

#define current_function_return_value (cp_function_chain->x_return_value)

#define current_class_type scope_chain->class_type

#define in_discarded_stmt scope_chain->discarded_stmt
#define in_consteval_if_p scope_chain->consteval_if_p

/* Nonzero means that this type is being defined.  I.e., the left brace
   starting the definition of this type has been seen.  */
#define TYPE_BEING_DEFINED(NODE) (LANG_TYPE_CLASS_CHECK (NODE)->being_defined)

/* Nonzero for FUNCTION_DECL means that this decl is a static
   member function.  */
#define DECL_STATIC_FUNCTION_P(NODE)                                           \
  (LANG_DECL_FN_CHECK (NODE)->static_function)

/* Nonzero for FUNCTION_DECL means that this decl is a non-static
   member function.  */
#define DECL_NONSTATIC_MEMBER_FUNCTION_P(NODE)                                 \
  (TREE_CODE (TREE_TYPE (NODE)) == METHOD_TYPE)

/* Nonzero for FUNCTION_DECL means that this decl is a member function
   (static or non-static).  */
#define DECL_FUNCTION_MEMBER_P(NODE)                                           \
  (DECL_NONSTATIC_MEMBER_FUNCTION_P (NODE) || DECL_STATIC_FUNCTION_P (NODE))

/* Nonzero if NODE is the target for genericization of 'return' stmts
   in constructors/destructors of targetm.cxx.cdtor_returns_this targets.  */
#define LABEL_DECL_CDTOR(NODE) DECL_LANG_FLAG_2 (LABEL_DECL_CHECK (NODE))

/* Nonzero if this NOP_EXPR is a reinterpret_cast.  Such conversions
   are not constexprs.  Other NOP_EXPRs are.  */
#define REINTERPRET_CAST_P(NODE) TREE_LANG_FLAG_0 (NOP_EXPR_CHECK (NODE))

/* Returns true if NODE is an object type:

     [basic.types]

     An object type is a (possibly cv-qualified) type that is not a
     function type, not a reference type, and not a void type.

   Keep these checks in ascending order, for speed.  */
#define TYPE_OBJ_P(NODE)                                                       \
  (!TYPE_REF_P (NODE) && !VOID_TYPE_P (NODE) && !FUNC_OR_METHOD_TYPE_P (NODE))

/* Returns true if NODE is a pointer to an object.  Keep these checks
   in ascending tree code order.  */
#define TYPE_PTROB_P(NODE) (TYPE_PTR_P (NODE) && TYPE_OBJ_P (TREE_TYPE (NODE)))

/* True if this CONSTRUCTOR contains PLACEHOLDER_EXPRs referencing the
   CONSTRUCTOR's type not nested inside another CONSTRUCTOR marked with
   CONSTRUCTOR_PLACEHOLDER_BOUNDARY.  */
#define CONSTRUCTOR_PLACEHOLDER_BOUNDARY(NODE)                                 \
  (TREE_LANG_FLAG_5 (CONSTRUCTOR_CHECK (NODE)))

#define AGGR_INIT_EXPR_SLOT(NODE) TREE_OPERAND (AGGR_INIT_EXPR_CHECK (NODE), 2)

/* True if this TARGET_EXPR expresses direct-initialization of an object
   to be named later.  */
#define TARGET_EXPR_DIRECT_INIT_P(NODE)                                        \
  TREE_LANG_FLAG_2 (TARGET_EXPR_CHECK (NODE))

/* Nonzero if DECL is a declaration of __builtin_constant_p.  */
#define DECL_IS_BUILTIN_CONSTANT_P(NODE)                                       \
  (TREE_CODE (NODE) == FUNCTION_DECL                                           \
   && DECL_BUILT_IN_CLASS (NODE) == BUILT_IN_NORMAL                            \
   && DECL_FUNCTION_CODE (NODE) == BUILT_IN_CONSTANT_P)

/* True iff this represents an lvalue being treated as an rvalue during return
   or throw as per [class.copy.elision].  */
#define IMPLICIT_RVALUE_P(NODE)                                                \
  TREE_LANG_FLAG_3 (TREE_CHECK2 ((NODE), NON_LVALUE_EXPR, STATIC_CAST_EXPR))

/* Nonzero for _DECL means that this decl appears in (or will appear
   in) as a member in a RECORD_TYPE or UNION_TYPE node.  It is also for
   detecting circularity in case members are multiply defined.  In the
   case of a VAR_DECL, it means that no definition has been seen, even
   if an initializer has been.  */
#define DECL_IN_AGGR_P(NODE) (DECL_LANG_FLAG_3 (NODE))

/* Nonzero means that this class type is a non-standard-layout class.  */
#define CLASSTYPE_NON_STD_LAYOUT(NODE)                                         \
  (LANG_TYPE_CLASS_CHECK (NODE)->non_std_layout)

/* Nonzero for FIELD_DECL node means that this field is a base class
   of the parent object, as opposed to a member field.  */
#define DECL_FIELD_IS_BASE(NODE) DECL_LANG_FLAG_6 (FIELD_DECL_CHECK (NODE))

/* Nonzero if TYPE is an anonymous union type.  */
#define ANON_UNION_TYPE_P(NODE)                                                \
  (TREE_CODE (NODE) == UNION_TYPE && ANON_AGGR_TYPE_P (NODE))

/* For an ANON_AGGR_TYPE_P the single FIELD_DECL it is used with.  */
#define ANON_AGGR_TYPE_FIELD(NODE) (LANG_TYPE_CLASS_CHECK (NODE)->typeinfo_var)

/* Nonzero if TYPE is an anonymous union or struct type.  We have to use a
   flag for this because "A union for which objects or pointers are
   declared is not an anonymous union" [class.union].  */
#define ANON_AGGR_TYPE_P(NODE)                                                 \
  (CLASS_TYPE_P (NODE) && LANG_TYPE_CLASS_CHECK (NODE)->anon_aggr)
#define SET_ANON_AGGR_TYPE_P(NODE) (LANG_TYPE_CLASS_CHECK (NODE)->anon_aggr = 1)

/* Nonzero if T is a class type but not a union.  */
#define NON_UNION_CLASS_TYPE_P(T)                                              \
  (TREE_CODE (T) == RECORD_TYPE && TYPE_LANG_FLAG_5 (T))

/* Determines whether an ENUMERAL_TYPE has an explicit
   underlying type.  */
#define ENUM_FIXED_UNDERLYING_TYPE_P(NODE) (TYPE_LANG_FLAG_5 (NODE))

/* Returns the underlying type of the given enumeration type. The
   underlying type is determined in different ways, depending on the
   properties of the enum:

     - In C++0x, the underlying type can be explicitly specified, e.g.,

	 enum E1 : char { ... } // underlying type is char

     - In a C++0x scoped enumeration, the underlying type is int
       unless otherwises specified:

	 enum class E2 { ... } // underlying type is int

     - Otherwise, the underlying type is determined based on the
       values of the enumerators. In this case, the
       ENUM_UNDERLYING_TYPE will not be set until after the definition
       of the enumeration is completed by finish_enum.  */
#define ENUM_UNDERLYING_TYPE(TYPE) TREE_TYPE (ENUMERAL_TYPE_CHECK (TYPE))

/* Nonzero if this type is volatile-qualified.  */
#define RS_TYPE_VOLATILE_P(NODE)                                               \
  ((rs_type_quals (NODE) & TYPE_QUAL_VOLATILE) != 0)

/* Nonzero means that this type is either complete or being defined, so we
   can do lookup in it.  */
#define COMPLETE_OR_OPEN_TYPE_P(NODE)                                          \
  (COMPLETE_TYPE_P (NODE) || (CLASS_TYPE_P (NODE) && TYPE_BEING_DEFINED (NODE)))

/* Indicates when overload resolution may resolve to a pointer to
   member function. [expr.unary.op]/3 */
#define PTRMEM_OK_P(NODE)                                                      \
  TREE_LANG_FLAG_0 (TREE_CHECK3 ((NODE), ADDR_EXPR, OFFSET_REF, SCOPE_REF))

/* Returns nonzero iff NODE is a declaration for the global function
   `main'.  */
#define DECL_MAIN_P(NODE)                                                      \
  (DECL_NAME (NODE) != NULL_TREE && MAIN_NAME_P (DECL_NAME (NODE))             \
   && flag_hosted)

/* Nonzero if the variable was declared to be thread-local.
   We need a special C++ version of this test because the middle-end
   DECL_THREAD_LOCAL_P uses the symtab, so we can't use it for
   templates.  */
#define RS_DECL_THREAD_LOCAL_P(NODE) (TREE_LANG_FLAG_0 (VAR_DECL_CHECK (NODE)))

#define COND_EXPR_IS_VEC_DELETE(NODE) TREE_LANG_FLAG_0 (COND_EXPR_CHECK (NODE))

/* RANGE_FOR_STMT accessors. These give access to the declarator,
   expression, body, and scope of the statement, respectively.  */
#define RANGE_FOR_DECL(NODE) TREE_OPERAND (RANGE_FOR_STMT_CHECK (NODE), 0)
#define RANGE_FOR_EXPR(NODE) TREE_OPERAND (RANGE_FOR_STMT_CHECK (NODE), 1)
#define RANGE_FOR_BODY(NODE) TREE_OPERAND (RANGE_FOR_STMT_CHECK (NODE), 2)
#define RANGE_FOR_SCOPE(NODE) TREE_OPERAND (RANGE_FOR_STMT_CHECK (NODE), 3)
#define RANGE_FOR_UNROLL(NODE) TREE_OPERAND (RANGE_FOR_STMT_CHECK (NODE), 4)
#define RANGE_FOR_INIT_STMT(NODE) TREE_OPERAND (RANGE_FOR_STMT_CHECK (NODE), 5)
#define RANGE_FOR_IVDEP(NODE) TREE_LANG_FLAG_6 (RANGE_FOR_STMT_CHECK (NODE))

#define CP_DECL_CONTEXT(NODE)                                                  \
  (!DECL_FILE_SCOPE_P (NODE) ? DECL_CONTEXT (NODE) : global_namespace)
#define CP_TYPE_CONTEXT(NODE)                                                  \
  (!TYPE_FILE_SCOPE_P (NODE) ? TYPE_CONTEXT (NODE) : global_namespace)
#define FROB_CONTEXT(NODE)                                                     \
  ((NODE) == global_namespace ? DECL_CONTEXT (NODE) : (NODE))

/* Nonzero if NODE is the std namespace.  */
#define DECL_NAMESPACE_STD_P(NODE) ((NODE) == std_node)

/* Whether the namepace is an inline namespace.  */
#define DECL_NAMESPACE_INLINE_P(NODE)                                          \
  TREE_LANG_FLAG_0 (NAMESPACE_DECL_CHECK (NODE))

#define CP_DECL_CONTEXT(NODE)                                                  \
  (!DECL_FILE_SCOPE_P (NODE) ? DECL_CONTEXT (NODE) : global_namespace)

/* Based off of TYPE_UNNAMED_P.  */
#define LAMBDA_TYPE_P(NODE)                                                    \
  (TREE_CODE (NODE) == RECORD_TYPE && TYPE_LINKAGE_IDENTIFIER (NODE)           \
   && IDENTIFIER_LAMBDA_P (TYPE_LINKAGE_IDENTIFIER (NODE)))

/* Macros to make error reporting functions' lives easier.  */
#define TYPE_LINKAGE_IDENTIFIER(NODE)                                          \
  (TYPE_IDENTIFIER (TYPE_MAIN_VARIANT (NODE)))

/* Identifiers used for lambda types are almost anonymous.  Use this
   spare flag to distinguish them (they also have the anonymous flag).  */
#define IDENTIFIER_LAMBDA_P(NODE)                                              \
  (IDENTIFIER_NODE_CHECK (NODE)->base.protected_flag)

/* If NODE, a FUNCTION_DECL, is a C++11 inheriting constructor, then this
   is the constructor it inherits from.  */
#define DECL_INHERITED_CTOR(NODE)                                              \
  (DECL_DECLARES_FUNCTION_P (NODE) && DECL_CONSTRUCTOR_P (NODE)                \
     ? LANG_DECL_FN_CHECK (NODE)->context                                      \
     : NULL_TREE)

/* True if the class type TYPE is a literal type.  */
#define CLASSTYPE_LITERAL_P(TYPE) (LANG_TYPE_CLASS_CHECK (TYPE)->is_literal)

/* Nonzero if NODE (a FUNCTION_DECL or TEMPLATE_DECL)
   is a destructor.  */
#define DECL_DESTRUCTOR_P(NODE) DECL_CXX_DESTRUCTOR_P (NODE)

/* Nonzero if TYPE has a trivial destructor.  From [class.dtor]:

     A destructor is trivial if it is an implicitly declared
     destructor and if:

       - all of the direct base classes of its class have trivial
	 destructors,

       - for all of the non-static data members of its class that are
	 of class type (or array thereof), each such class has a
	 trivial destructor.  */
#define TYPE_HAS_TRIVIAL_DESTRUCTOR(NODE)                                      \
  (!TYPE_HAS_NONTRIVIAL_DESTRUCTOR (NODE))

/* Nonzero means that NODE (a class type) has a destructor -- but that
   it has not yet been declared.  */
#define CLASSTYPE_LAZY_DESTRUCTOR(NODE)                                        \
  (LANG_TYPE_CLASS_CHECK (NODE)->lazy_destructor)

/* Nonzero if NODE (a FUNCTION_DECL) is a constructor for a complete
   object.  */
#define DECL_COMPLETE_CONSTRUCTOR_P(NODE)                                      \
  (DECL_NAME (NODE) == complete_ctor_identifier)

/* Nonzero if NODE (a FUNCTION_DECL) is a constructor for a base
   object.  */
#define DECL_BASE_CONSTRUCTOR_P(NODE) (DECL_NAME (NODE) == base_ctor_identifier)

/* Nonzero if NODE (a FUNCTION_DECL) is a constructor, but not either the
   specialized in-charge constructor or the specialized not-in-charge
   constructor.  */
#define DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P(NODE)                               \
  (DECL_NAME (NODE) == ctor_identifier)

/* Nonzero if NODE (a FUNCTION_DECL) is a copy constructor.  */
#define DECL_COPY_CONSTRUCTOR_P(NODE)                                          \
  (DECL_CONSTRUCTOR_P (NODE) && copy_fn_p (NODE) > 0)

/* Nonzero if NODE (a FUNCTION_DECL) is a move constructor.  */
#define DECL_MOVE_CONSTRUCTOR_P(NODE)                                          \
  (DECL_CONSTRUCTOR_P (NODE) && move_fn_p (NODE))

/* Nonzero if NODE (a FUNCTION_DECL) is a destructor, but not the
   specialized in-charge constructor, in-charge deleting constructor,
   or the base destructor.  */
#define DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P(NODE)                                \
  (DECL_NAME (NODE) == dtor_identifier)

/* Nonzero if NODE (a FUNCTION_DECL) is a destructor for a complete
   object.  */
#define DECL_COMPLETE_DESTRUCTOR_P(NODE)                                       \
  (DECL_NAME (NODE) == complete_dtor_identifier)

/* Nonzero if NODE (a FUNCTION_DECL) is a destructor for a base
   object.  */
#define DECL_BASE_DESTRUCTOR_P(NODE) (DECL_NAME (NODE) == base_dtor_identifier)

/* Nonzero if NODE (a FUNCTION_DECL) is a destructor for a complete
   object that deletes the object after it has been destroyed.  */
#define DECL_DELETING_DESTRUCTOR_P(NODE)                                       \
  (DECL_NAME (NODE) == deleting_dtor_identifier)

/* Nonzero if either DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P or
   DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P is true of NODE.  */
#define DECL_MAYBE_IN_CHARGE_CDTOR_P(NODE)                                     \
  (DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P (NODE)                                   \
   || DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P (NODE))

/* Nonzero if NODE (a _DECL) is a cloned constructor or
   destructor.  */
#define DECL_CLONED_FUNCTION_P(NODE)                                           \
  (DECL_NAME (NODE) && IDENTIFIER_CDTOR_P (DECL_NAME (NODE))                   \
   && !DECL_MAYBE_IN_CHARGE_CDTOR_P (NODE))

/* If DECL_CLONED_FUNCTION_P holds, this is the function that was
   cloned.  */
#define DECL_CLONED_FUNCTION(NODE)                                             \
  (DECL_LANG_SPECIFIC (FUNCTION_DECL_CHECK (NODE))->u.fn.u5.cloned_function)

/* Nonzero means that an object of this type cannot be initialized using
   an initializer list.  */
#define CLASSTYPE_NON_AGGREGATE(NODE)                                          \
  (LANG_TYPE_CLASS_CHECK (NODE)->non_aggregate)
#define TYPE_NON_AGGREGATE_CLASS(NODE)                                         \
  (CLASS_TYPE_P (NODE) && CLASSTYPE_NON_AGGREGATE (NODE))

/* Nonzero for class type means that the default constructor is trivial.  */
#define TYPE_HAS_TRIVIAL_DFLT(NODE)                                            \
  (TYPE_HAS_DEFAULT_CONSTRUCTOR (NODE) && !TYPE_HAS_COMPLEX_DFLT (NODE))

/* Nonzero if this class has a constexpr constructor other than a copy/move
   constructor.  Note that a class can have constexpr constructors for
   static initialization even if it isn't a literal class.  */
#define TYPE_HAS_CONSTEXPR_CTOR(NODE)                                          \
  (LANG_TYPE_CLASS_CHECK (NODE)->has_constexpr_ctor)

/* Nonzero if there is no trivial default constructor for this class.  */
#define TYPE_HAS_COMPLEX_DFLT(NODE)                                            \
  (LANG_TYPE_CLASS_CHECK (NODE)->has_complex_dflt)

/* [dcl.init.aggr]

   An aggregate is an array or a class with no user-provided
   constructors, no brace-or-equal-initializers for non-static data
   members, no private or protected non-static data members, no
   base classes, and no virtual functions.

   As an extension, we also treat vectors as aggregates.  Keep these
   checks in ascending code order.  */
#define CP_AGGREGATE_TYPE_P(TYPE)                                              \
  (gnu_vector_type_p (TYPE) || TREE_CODE (TYPE) == ARRAY_TYPE                  \
   || (CLASS_TYPE_P (TYPE) && COMPLETE_TYPE_P (TYPE)                           \
       && !CLASSTYPE_NON_AGGREGATE (TYPE)))

/* Nonzero for a FIELD_DECL means that this member object type
   is mutable.  */
#define DECL_MUTABLE_P(NODE) (DECL_LANG_FLAG_0 (FIELD_DECL_CHECK (NODE)))

#if defined ENABLE_TREE_CHECKING

#define LANG_DECL_MIN_CHECK(NODE)                                              \
  __extension__({                                                              \
    struct lang_decl *lt = DECL_LANG_SPECIFIC (NODE);                          \
    if (!LANG_DECL_HAS_MIN (NODE))                                             \
      lang_check_failed (__FILE__, __LINE__, __FUNCTION__);                    \
    &lt->u.min;                                                                \
  })

/* We want to be able to check DECL_CONSTRUCTOR_P and such on a function
   template, not just on a FUNCTION_DECL.  So when looking for things in
   lang_decl_fn, look down through a TEMPLATE_DECL into its result.  */
#define LANG_DECL_FN_CHECK(NODE)                                               \
  __extension__({                                                              \
    struct lang_decl *lt = DECL_LANG_SPECIFIC (NODE);                          \
    if (!DECL_DECLARES_FUNCTION_P (NODE) || lt->u.base.selector != lds_fn)     \
      lang_check_failed (__FILE__, __LINE__, __FUNCTION__);                    \
    &lt->u.fn;                                                                 \
  })

#define LANG_DECL_NS_CHECK(NODE)                                               \
  __extension__({                                                              \
    struct lang_decl *lt = DECL_LANG_SPECIFIC (NODE);                          \
    if (TREE_CODE (NODE) != NAMESPACE_DECL || lt->u.base.selector != lds_ns)   \
      lang_check_failed (__FILE__, __LINE__, __FUNCTION__);                    \
    &lt->u.ns;                                                                 \
  })

#define LANG_DECL_PARM_CHECK(NODE)                                             \
  __extension__({                                                              \
    struct lang_decl *lt = DECL_LANG_SPECIFIC (NODE);                          \
    if (TREE_CODE (NODE) != PARM_DECL || lt->u.base.selector != lds_parm)      \
      lang_check_failed (__FILE__, __LINE__, __FUNCTION__);                    \
    &lt->u.parm;                                                               \
  })

#define LANG_DECL_DECOMP_CHECK(NODE)                                           \
  __extension__({                                                              \
    struct lang_decl *lt = DECL_LANG_SPECIFIC (NODE);                          \
    if (!VAR_P (NODE) || lt->u.base.selector != lds_decomp)                    \
      lang_check_failed (__FILE__, __LINE__, __FUNCTION__);                    \
    &lt->u.decomp;                                                             \
  })

#else

#define LANG_DECL_MIN_CHECK(NODE) (&DECL_LANG_SPECIFIC (NODE)->u.min)

#define LANG_DECL_FN_CHECK(NODE) (&DECL_LANG_SPECIFIC (NODE)->u.fn)

#define LANG_DECL_NS_CHECK(NODE) (&DECL_LANG_SPECIFIC (NODE)->u.ns)

#define LANG_DECL_PARM_CHECK(NODE) (&DECL_LANG_SPECIFIC (NODE)->u.parm)

#define LANG_DECL_DECOMP_CHECK(NODE) (&DECL_LANG_SPECIFIC (NODE)->u.decomp)

#endif /* ENABLE_TREE_CHECKING */

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

/* WHILE_STMT accessors. These give access to the condition of the
   while statement and the body of the while statement, respectively.  */
#define WHILE_COND(NODE) TREE_OPERAND (WHILE_STMT_CHECK (NODE), 0)
#define WHILE_BODY(NODE) TREE_OPERAND (WHILE_STMT_CHECK (NODE), 1)

/* FOR_STMT accessors. These give access to the init statement,
   condition, update expression, and body of the for statement,
   respectively.  */
#define FOR_INIT_STMT(NODE) TREE_OPERAND (FOR_STMT_CHECK (NODE), 0)
#define FOR_COND(NODE) TREE_OPERAND (FOR_STMT_CHECK (NODE), 1)
#define FOR_EXPR(NODE) TREE_OPERAND (FOR_STMT_CHECK (NODE), 2)
#define FOR_BODY(NODE) TREE_OPERAND (FOR_STMT_CHECK (NODE), 3)
#define FOR_SCOPE(NODE) TREE_OPERAND (FOR_STMT_CHECK (NODE), 4)

#define SWITCH_STMT_COND(NODE) TREE_OPERAND (SWITCH_STMT_CHECK (NODE), 0)
#define SWITCH_STMT_BODY(NODE) TREE_OPERAND (SWITCH_STMT_CHECK (NODE), 1)
#define SWITCH_STMT_TYPE(NODE) TREE_OPERAND (SWITCH_STMT_CHECK (NODE), 2)
#define SWITCH_STMT_SCOPE(NODE) TREE_OPERAND (SWITCH_STMT_CHECK (NODE), 3)

/* Nonzero if NODE is the target for genericization of 'break' stmts.  */
#define LABEL_DECL_BREAK(NODE) DECL_LANG_FLAG_0 (LABEL_DECL_CHECK (NODE))

/* Nonzero if NODE is the target for genericization of 'continue' stmts.  */
#define LABEL_DECL_CONTINUE(NODE) DECL_LANG_FLAG_1 (LABEL_DECL_CHECK (NODE))

// Above macros are copied from gcc/c-family/c-common.h

// Below macros are copied from gcc/cp/name-lookup.h

/* Lookup walker marking.  */
#define LOOKUP_SEEN_P(NODE) TREE_VISITED (NODE)
#define LOOKUP_FOUND_P(NODE)                                                   \
  TREE_LANG_FLAG_4 (TREE_CHECK4 (NODE, RECORD_TYPE, UNION_TYPE, ENUMERAL_TYPE, \
				 NAMESPACE_DECL))

// Above macros are copied from gcc/cp/name-lookup.h

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

/* The binding level currently in effect.  */

#define current_binding_level                                                  \
  (*(cfun && cp_function_chain && cp_function_chain->bindings                  \
       ? &cp_function_chain->bindings                                          \
       : &scope_chain->bindings))

// Above macros are copied from gcc/cp/name-lookup.cc

/* The various kinds of special functions.  If you add to this list,
   you should update special_function_p as well.  */
enum special_function_kind
{
  sfk_none = 0, /* Not a special function.  This enumeral
		   must have value zero; see
		   special_function_p.  */
  /* The following are ordered, for use by member synthesis fns.  */
  sfk_destructor,	      /* A destructor.  */
  sfk_constructor,	      /* A constructor.  */
  sfk_inheriting_constructor, /* An inheriting constructor */
  sfk_copy_constructor,	      /* A copy constructor.  */
  sfk_move_constructor,	      /* A move constructor.  */
  sfk_copy_assignment,	      /* A copy assignment operator.  */
  sfk_move_assignment,	      /* A move assignment operator.  */
  /* The following are unordered.  */
  sfk_complete_destructor, /* A destructor for complete objects.  */
  sfk_base_destructor,	   /* A destructor for base subobjects.  */
  sfk_deleting_destructor, /* A destructor for complete objects that
			      deletes the object after it has been
			      destroyed.  */
  sfk_conversion,	   /* A conversion operator.  */
  sfk_deduction_guide,	   /* A class template deduction guide.  */
  sfk_comparison,	   /* A comparison operator (e.g. ==, <, <=>).  */
  sfk_virtual_destructor   /* Used by member synthesis fns.  */
};

/* Places where an lvalue, or modifiable lvalue, may be required.
   Used to select diagnostic messages in lvalue_error and
   readonly_error.  */
enum lvalue_use
{
  lv_assign,
  lv_increment,
  lv_decrement,
  lv_addressof,
  lv_asm
};

/* A class for recording information about access failures (e.g. private
   fields), so that we can potentially supply a fix-it hint about
   an accessor (from a context in which the constness of the object
   is known).  */

class access_failure_info
{
public:
  access_failure_info ()
    : m_was_inaccessible (false), m_basetype_path (NULL_TREE),
      m_decl (NULL_TREE), m_diag_decl (NULL_TREE)
  {}

  void record_access_failure (tree basetype_path, tree decl, tree diag_decl);

  bool was_inaccessible_p () const { return m_was_inaccessible; }
  tree get_decl () const { return m_decl; }
  tree get_diag_decl () const { return m_diag_decl; }
  tree get_any_accessor (bool const_p) const;
  void maybe_suggest_accessor (bool const_p) const;
  static void add_fixit_hint (rich_location *richloc, tree accessor);

private:
  bool m_was_inaccessible;
  tree m_basetype_path;
  tree m_decl;
  tree m_diag_decl;
};

/* The various kinds of access check during parsing.  */
enum deferring_kind
{
  dk_no_deferred = 0, /* Check access immediately */
  dk_deferred = 1,    /* Deferred check */
  dk_no_check = 2     /* No access check */
};

/* The representation of a deferred access check.  */

struct GTY (()) deferred_access_check
{
  /* The base class in which the declaration is referenced. */
  tree binfo;
  /* The declaration whose access must be checked.  */
  tree decl;
  /* The declaration that should be used in the error message.  */
  tree diag_decl;
  /* The location of this access.  */
  location_t loc;
};

struct GTY (()) tree_template_info
{
  struct tree_base base;
  tree tmpl;
  tree args;
  vec<deferred_access_check, va_gc> *deferred_access_checks;
};

/* The various kinds of lvalues we distinguish.  */
enum cp_lvalue_kind_flags
{
  clk_none = 0,	     /* Things that are not an lvalue.  */
  clk_ordinary = 1,  /* An ordinary lvalue.  */
  clk_rvalueref = 2, /* An xvalue (rvalue formed using an rvalue reference) */
  clk_class = 4,     /* A prvalue of class or array type.  */
  clk_bitfield = 8,  /* An lvalue for a bit-field.  */
  clk_packed = 16,   /* An lvalue for a packed field.  */
  clk_implicit_rval = 1 << 5 /* An lvalue being treated as an xvalue.  */
};

/* This type is used for parameters and variables which hold
   combinations of the flags in enum cp_lvalue_kind_flags.  */
typedef int cp_lvalue_kind;

// forked from gcc/cp/name_lookup.h scope_kind

/* The kinds of scopes we recognize.  */
enum scope_kind
{
  sk_block = 0,	     /* An ordinary block scope.  This enumerator must
			have the value zero because "cp_binding_level"
			is initialized by using "memset" to set the
			contents to zero, and the default scope kind
			is "sk_block".  */
  sk_cleanup,	     /* A scope for (pseudo-)scope for cleanup.  It is
			pseudo in that it is transparent to name lookup
			activities.  */
  sk_try,	     /* A try-block.  */
  sk_catch,	     /* A catch-block.  */
  sk_for,	     /* The scope of the variable declared in a
			init-statement.  */
  sk_cond,	     /* The scope of the variable declared in the condition
			of an if or switch statement.  */
  sk_function_parms, /* The scope containing function parameters.  */
  sk_class,	     /* The scope containing the members of a class.  */
  sk_scoped_enum,    /* The scope containing the enumerators of a C++11
			scoped enumeration.  */
  sk_namespace,	     /* The scope containing the members of a
			namespace, including the global scope.  */
  sk_template_parms, /* A scope for template parameters.  */
  sk_template_spec,  /* Like sk_template_parms, but for an explicit
			specialization.  Since, by definition, an
			explicit specialization is introduced by
			"template <>", this scope is always empty.  */
  sk_transaction,    /* A synchronized or atomic statement.  */
  sk_omp	     /* An OpenMP structured block.  */
};

// forked from gcc/cp/cp-tree.h cp_built_in_function

/* BUILT_IN_FRONTEND function codes.  */
enum cp_built_in_function
{
  CP_BUILT_IN_IS_CONSTANT_EVALUATED,
  CP_BUILT_IN_INTEGER_PACK,
  CP_BUILT_IN_IS_CORRESPONDING_MEMBER,
  CP_BUILT_IN_IS_POINTER_INTERCONVERTIBLE_WITH_CLASS,
  CP_BUILT_IN_SOURCE_LOCATION,
  CP_BUILT_IN_LAST
};

// forked from gcc/cp/cp-tree.h warning_sentinel

/* RAII sentinel to disable certain warnings during template substitution
   and elsewhere.  */

class warning_sentinel
{
public:
  int &flag;
  int val;
  warning_sentinel (int &flag, bool suppress = true) : flag (flag), val (flag)
  {
    if (suppress)
      flag = 0;
  }
  ~warning_sentinel () { flag = val; }
};

// forked from gcc/cp/cp-tree.h uid_sensitive_constexpr_evaluation_checker

/* Used to determine whether uid_sensitive_constexpr_evaluation_p was
   called and returned true, indicating that we've restricted constexpr
   evaluation in order to avoid UID generation.  We use this to control
   updates to the fold_cache and cv_cache.  */

struct uid_sensitive_constexpr_evaluation_checker
{
  const unsigned saved_counter;
  uid_sensitive_constexpr_evaluation_checker ();
  bool evaluation_restricted_p () const;
};

// forked from gcc/cp/cp-tree.h iloc_sentinel

/* RAII sentinel to temporarily override input_location.  This will not set
   input_location to UNKNOWN_LOCATION or BUILTINS_LOCATION.  */

class iloc_sentinel
{
  location_t saved_loc;

public:
  iloc_sentinel (location_t loc) : saved_loc (input_location)
  {
    if (loc >= RESERVED_LOCATION_COUNT)
      input_location = loc;
  }
  ~iloc_sentinel () { input_location = saved_loc; }
};

// forked from gcc/cp/cp-tree.h ptrmem_cst

struct GTY (()) ptrmem_cst
{
  struct tree_common common;
  tree member;
  location_t locus;
};
typedef struct ptrmem_cst *ptrmem_cst_t;

// forked from gcc/cp/cp-tree.h named_decl_hash

/* hash traits for declarations.  Hashes potential overload sets via
   DECL_NAME.  */

struct named_decl_hash : ggc_remove<tree>
{
  typedef tree value_type;   /* A DECL or OVERLOAD  */
  typedef tree compare_type; /* An identifier.  */

  inline static hashval_t hash (const value_type decl);
  inline static bool equal (const value_type existing, compare_type candidate);

  static const bool empty_zero_p = true;
  static inline void mark_empty (value_type &p) { p = NULL_TREE; }
  static inline bool is_empty (value_type p) { return !p; }

  /* Nothing is deletable.  Everything is insertable.  */
  static bool is_deleted (value_type) { return false; }
  static void mark_deleted (value_type) { gcc_unreachable (); }
};

// forked from gcc/cp/cp-tree.h lang_decl_selector

/* Discriminator values for lang_decl.  */

enum lang_decl_selector
{
  lds_min,
  lds_fn,
  lds_ns,
  lds_parm,
  lds_decomp
};

// forked from gcc/cp/cp-tree.h lang_decl_base

/* Flags shared by all forms of DECL_LANG_SPECIFIC.

   Some of the flags live here only to make lang_decl_min/fn smaller.  Do
   not make this struct larger than 32 bits.  */

struct GTY (()) lang_decl_base
{
  ENUM_BITFIELD (lang_decl_selector) selector : 3;
  unsigned use_template : 2;
  unsigned not_really_extern : 1;    /* var or fn */
  unsigned initialized_in_class : 1; /* var or fn */

  unsigned threadprivate_or_deleted_p : 1; /* var or fn */
  /* anticipated_p is no longer used for anticipated_decls (fn, type
     or template).  It is used as DECL_OMP_PRIVATIZED_MEMBER in
     var.  */
  unsigned anticipated_p : 1;
  unsigned friend_or_tls : 1;	      /* var, fn, type or template */
  unsigned unknown_bound_p : 1;	      /* var */
  unsigned odr_used : 1;	      /* var or fn */
  unsigned concept_p : 1;	      /* applies to vars and functions */
  unsigned var_declared_inline_p : 1; /* var */
  unsigned dependent_init_p : 1;      /* var */

  /* The following apply to VAR, FUNCTION, TYPE, CONCEPT, & NAMESPACE
     decls.  */
  unsigned module_purview_p : 1; /* in module purview (not GMF) */
  unsigned module_import_p : 1;	 /* from an import */
  unsigned module_entity_p : 1;	 /* is in the entitity ary &
				    hash.  */
  /* VAR_DECL or FUNCTION_DECL has attached decls.     */
  unsigned module_attached_p : 1;

  /* 12 spare bits.  */
};

/* True for DECL codes which have template info and access.  */
#define LANG_DECL_HAS_MIN(NODE)                                                \
  (VAR_OR_FUNCTION_DECL_P (NODE) || TREE_CODE (NODE) == FIELD_DECL             \
   || TREE_CODE (NODE) == CONST_DECL || TREE_CODE (NODE) == TYPE_DECL          \
   || TREE_CODE (NODE) == TEMPLATE_DECL || TREE_CODE (NODE) == USING_DECL      \
   || TREE_CODE (NODE) == CONCEPT_DECL)

// forked from gcc/c-family-common.h stmt_tree_s

/* Information about a statement tree.  */

struct GTY (()) stmt_tree_s
{
  /* A stack of statement lists being collected.  */
  vec<tree, va_gc> *x_cur_stmt_list;

  /* In C++, Nonzero if we should treat statements as full
     expressions.  In particular, this variable is non-zero if at the
     end of a statement we should destroy any temporaries created
     during that statement.  Similarly, if, at the end of a block, we
     should destroy any local variables in this block.  Normally, this
     variable is nonzero, since those are the normal semantics of
     C++.

     This flag has no effect in C.  */
  int stmts_are_full_exprs_p;
};

// forked from gcc/c-family-common.h stmt_tree_s

typedef struct stmt_tree_s *stmt_tree;

// forked from gcc/c-family-common.h c_language_function

/* Global state pertinent to the current function.  Some C dialects
   extend this structure with additional fields.  */

struct GTY (()) c_language_function
{
  /* While we are parsing the function, this contains information
     about the statement-tree that we are building.  */
  struct stmt_tree_s x_stmt_tree;

  /* Vector of locally defined typedefs, for
     -Wunused-local-typedefs.  */
  vec<tree, va_gc> *local_typedefs;
};

// forked from gcc/cp/cp-tree.h omp_declare_target_attr

struct GTY (()) omp_declare_target_attr
{
  bool attr_syntax;
};

// forked from gcc/cp/name-lookup.h cxx_binding

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

// forked from gcc/cp/name-lookup.h cxx_saved_binding

/* Datatype used to temporarily save C++ bindings (for implicit
   instantiations purposes and like).  Implemented in decl.cc.  */
struct GTY (()) cxx_saved_binding
{
  /* The name of the current binding.  */
  tree identifier;
  /* The binding we're saving.  */
  cxx_binding *binding;
  tree real_type_value;
};

// forked from gcc/cp/cp-tree.h saved_scope

/* Global state.  */

struct GTY (()) saved_scope
{
  vec<cxx_saved_binding, va_gc> *old_bindings;
  tree old_namespace;
  vec<tree, va_gc> *decl_ns_list;
  tree class_name;
  tree class_type;
  tree access_specifier;
  tree function_decl;
  vec<tree, va_gc> *lang_base;
  tree lang_name;
  tree template_parms;
  tree x_saved_tree;

  /* Only used for uses of this in trailing return type.  */
  tree x_current_class_ptr;
  tree x_current_class_ref;

  int x_processing_template_decl;
  int x_processing_specialization;
  int x_processing_constraint;
  int suppress_location_wrappers;
  BOOL_BITFIELD x_processing_explicit_instantiation : 1;
  BOOL_BITFIELD need_pop_function_context : 1;

  /* Nonzero if we are parsing the discarded statement of a constexpr
     if-statement.  */
  BOOL_BITFIELD discarded_stmt : 1;
  /* Nonzero if we are parsing or instantiating the compound-statement
     of consteval if statement.  Also set while processing an immediate
     invocation.  */
  BOOL_BITFIELD consteval_if_p : 1;

  int unevaluated_operand;
  int inhibit_evaluation_warnings;
  int noexcept_operand;
  int ref_temp_count;

  struct stmt_tree_s x_stmt_tree;

  hash_map<tree, tree> *GTY ((skip)) x_local_specializations;
  vec<omp_declare_target_attr, va_gc> *omp_declare_target_attribute;

  struct saved_scope *prev;
};

extern GTY (()) struct saved_scope *scope_chain;

// forked from gcc/cp/cp-tree.h named_label_hash

struct named_label_entry; /* Defined in decl.cc.  */

struct named_label_hash : ggc_remove<named_label_entry *>
{
  typedef named_label_entry *value_type;
  typedef tree compare_type; /* An identifier.  */

  inline static hashval_t hash (value_type);
  inline static bool equal (const value_type, compare_type);

  static const bool empty_zero_p = true;
  inline static void mark_empty (value_type &p) { p = NULL; }
  inline static bool is_empty (value_type p) { return !p; }

  /* Nothing is deletable.  Everything is insertable.  */
  inline static bool is_deleted (value_type) { return false; }
  inline static void mark_deleted (value_type) { gcc_unreachable (); }
};

// forked from gcc/cp/cp-tree.h

/* Global state pertinent to the current function.  */

struct GTY (()) language_function
{
  struct c_language_function base;

  tree x_cdtor_label;
  tree x_current_class_ptr;
  tree x_current_class_ref;
  tree x_eh_spec_block;
  tree x_in_charge_parm;
  tree x_vtt_parm;
  tree x_return_value;

  BOOL_BITFIELD returns_value : 1;
  BOOL_BITFIELD returns_null : 1;
  BOOL_BITFIELD returns_abnormally : 1;
  BOOL_BITFIELD infinite_loop : 1;
  BOOL_BITFIELD x_in_function_try_handler : 1;
  BOOL_BITFIELD x_in_base_initializer : 1;

  /* True if this function can throw an exception.  */
  BOOL_BITFIELD can_throw : 1;

  BOOL_BITFIELD invalid_constexpr : 1;
  BOOL_BITFIELD throwing_cleanup : 1;

  hash_table<named_label_hash> *x_named_labels;

  /* Tracking possibly infinite loops.  This is a vec<tree> only because
     vec<bool> doesn't work with gtype.  */
  vec<tree, va_gc> *infinite_loops;
};

// forked from gcc/c-family/c-common.h ref_operator

/* The various name of operator that appears in error messages. */
enum ref_operator
{
  /* NULL */
  RO_NULL,
  /* array indexing */
  RO_ARRAY_INDEXING,
  /* unary * */
  RO_UNARY_STAR,
  /* -> */
  RO_ARROW,
  /* implicit conversion */
  RO_IMPLICIT_CONVERSION,
  /* ->* */
  RO_ARROW_STAR
};

// forked from gcc/cp/cp-tree.h lang_decl_min

/* DECL_LANG_SPECIFIC for the above codes.  */

struct GTY (()) lang_decl_min
{
  struct lang_decl_base base; /* 32-bits.  */

  /* In a FUNCTION_DECL for which DECL_THUNK_P holds, this is
     THUNK_ALIAS.
     In a FUNCTION_DECL for which DECL_THUNK_P does not hold,
     VAR_DECL, TYPE_DECL, or TEMPLATE_DECL, this is
     DECL_TEMPLATE_INFO.  */
  tree template_info;

  /* In a DECL_THUNK_P FUNCTION_DECL, this is THUNK_VIRTUAL_OFFSET.
     In a lambda-capture proxy VAR_DECL, this is DECL_CAPTURED_VARIABLE.
     In a function-scope TREE_STATIC VAR_DECL or IMPLICIT_TYPEDEF_P TYPE_DECL,
     this is DECL_DISCRIMINATOR.
     In a DECL_LOCAL_DECL_P decl, this is the namespace decl it aliases.
     Otherwise, in a class-scope DECL, this is DECL_ACCESS.   */
  tree access;
};

// forked from gcc/cp/cp-tree.h lang_decl_fn

/* Additional DECL_LANG_SPECIFIC information for functions.  */

struct GTY (()) lang_decl_fn
{
  struct lang_decl_min min;

  /* In a overloaded operator, this is the compressed operator code.  */
  unsigned ovl_op_code : 6;
  unsigned global_ctor_p : 1;
  unsigned global_dtor_p : 1;

  unsigned static_function : 1;
  unsigned pure_virtual : 1;
  unsigned defaulted_p : 1;
  unsigned has_in_charge_parm_p : 1;
  unsigned has_vtt_parm_p : 1;
  unsigned pending_inline_p : 1;
  unsigned nonconverting : 1;
  unsigned thunk_p : 1;

  unsigned this_thunk_p : 1;
  unsigned omp_declare_reduction_p : 1;
  unsigned has_dependent_explicit_spec_p : 1;
  unsigned immediate_fn_p : 1;
  unsigned maybe_deleted : 1;
  unsigned coroutine_p : 1;
  unsigned implicit_constexpr : 1;

  unsigned spare : 9;

  /* 32-bits padding on 64-bit host.  */

  /* For a non-thunk function decl, this is a tree list of
     friendly classes. For a thunk function decl, it is the
     thunked to function decl.  */
  tree befriending_classes;

  /* For a virtual FUNCTION_DECL for which
     DECL_THIS_THUNK_P does not hold, this is DECL_THUNKS. Both
     this pointer and result pointer adjusting thunks are
     chained here.  This pointer thunks to return pointer thunks
     will be chained on the return pointer thunk.
     For a DECL_CONSTUCTOR_P FUNCTION_DECL, this is the base from
     whence we inherit.  Otherwise, it is the class in which a
     (namespace-scope) friend is defined (if any).   */
  tree context;

  union lang_decl_u5
  {
    /* In a non-thunk FUNCTION_DECL, this is DECL_CLONED_FUNCTION.  */
    tree GTY ((tag ("0"))) cloned_function;

    /* In a FUNCTION_DECL for which THUNK_P holds this is the
       THUNK_FIXED_OFFSET.  */
    HOST_WIDE_INT GTY ((tag ("1"))) fixed_offset;
  } GTY ((desc ("%1.thunk_p"))) u5;

  union lang_decl_u3
  {
    struct cp_token_cache *GTY ((tag ("1"))) pending_inline_info;
    tree GTY ((tag ("0"))) saved_auto_return_type;
  } GTY ((desc ("%1.pending_inline_p"))) u;
};

// forked from gcc/cp/cp-tree.h lang_decl_ns

/* DECL_LANG_SPECIFIC for namespaces.  */

struct GTY (()) lang_decl_ns
{
  struct lang_decl_base base; /* 32 bits.  */

  /* Inline children.  Needs to be va_gc, because of PCH.  */
  vec<tree, va_gc> *inlinees;

  /* Hash table of bound decls. It'd be nice to have this inline, but
     as the hash_map has a dtor, we can't then put this struct into a
     union (until moving to c++11).  */
  hash_table<named_decl_hash> *bindings;
};

// forked from gcc/cp/cp-tree.h lang_decl_parm

/* DECL_LANG_SPECIFIC for parameters.  */

struct GTY (()) lang_decl_parm
{
  struct lang_decl_base base; /* 32 bits.  */
  int level;
  int index;
};

// forked from gcc/cp/cp-tree.h lang_decl_decomp

/* Additional DECL_LANG_SPECIFIC information for structured bindings.  */

struct GTY (()) lang_decl_decomp
{
  struct lang_decl_min min;
  /* The artificial underlying "e" variable of the structured binding
     variable.  */
  tree base;
};

// forked from gcc/cp/cp-tree.h lang_decl

/* DECL_LANG_SPECIFIC for all types.  It would be nice to just make this a
   union rather than a struct containing a union as its only field, but
   tree.h declares it as a struct.  */

struct GTY (()) lang_decl
{
  union GTY ((desc ("%h.base.selector"))) lang_decl_u
  {
    /* Nothing of only the base type exists.  */
    struct lang_decl_base GTY ((default)) base;
    struct lang_decl_min GTY ((tag ("lds_min"))) min;
    struct lang_decl_fn GTY ((tag ("lds_fn"))) fn;
    struct lang_decl_ns GTY ((tag ("lds_ns"))) ns;
    struct lang_decl_parm GTY ((tag ("lds_parm"))) parm;
    struct lang_decl_decomp GTY ((tag ("lds_decomp"))) decomp;
  } u;
};

// forked from gcc/c-family/c-common.h c_fileinfo

/* Information recorded about each file examined during compilation.  */

struct c_fileinfo
{
  int time; /* Time spent in the file.  */

  /* Flags used only by C++.
     INTERFACE_ONLY nonzero means that we are in an "interface" section
     of the compiler.  INTERFACE_UNKNOWN nonzero means we cannot trust
     the value of INTERFACE_ONLY.  If INTERFACE_UNKNOWN is zero and
     INTERFACE_ONLY is zero, it means that we are responsible for
     exporting definitions that others might need.  */
  short interface_only;
  short interface_unknown;
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

// forked from gcc/cp/cp-tree.h lkp_iterator

/* Iterator over a (potentially) 2 dimensional overload, which is
   produced by name lookup.  */

class lkp_iterator : public ovl_iterator
{
  typedef ovl_iterator parent;

  tree outer;

public:
  explicit lkp_iterator (tree o) : parent (o, true), outer (maybe_push ()) {}

public:
  lkp_iterator &operator++ ()
  {
    bool repush = !outer;

    if (!parent::operator++ () && !repush)
      {
	pop (outer);
	repush = true;
      }

    if (repush)
      outer = maybe_push ();

    return *this;
  }
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

// forked from gcc/cp/cp-tree.h compare_bounds_t

/* in typeck.cc */
/* Says how we should behave when comparing two arrays one of which
   has unknown bounds.  */
enum compare_bounds_t
{
  bounds_none,
  bounds_either,
  bounds_first
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
mark_rvalue_use (tree, location_t = UNKNOWN_LOCATION,
		 bool reject_builtin = true);

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
lookup_add (tree fns, tree lookup);

extern tree
ovl_make (tree fn, tree next = NULL_TREE);

extern int is_overloaded_fn (tree) ATTRIBUTE_PURE;

extern bool maybe_add_lang_type_raw (tree);

extern rs_ref_qualifier type_memfn_rqual (const_tree);

extern bool builtin_pack_fn_p (tree);

extern tree make_conv_op_name (tree);

extern int type_memfn_quals (const_tree);

struct c_fileinfo *
get_fileinfo (const char *);

extern tree
cxx_make_type (enum tree_code CXX_MEM_STAT_INFO);

extern tree
build_cplus_array_type (tree, tree, int is_dep = -1);

extern bool is_byte_access_type (tree);

extern bool
comptypes (tree, tree, int);

extern tree canonical_eh_spec (tree);

extern int cp_tree_operand_length (const_tree);

extern bool rs_tree_equal (tree, tree);

extern bool compparms (const_tree, const_tree);

extern tree
rs_build_qualified_type_real (tree, int, tsubst_flags_t);
#define rs_build_qualified_type(TYPE, QUALS)                                   \
  rs_build_qualified_type_real ((TYPE), (QUALS), tf_warning_or_error)
extern bool cv_qualified_p (const_tree);

extern bool similar_type_p (tree, tree);

extern bool rs_tree_equal (tree, tree);

extern bool
vector_targets_convertible_p (const_tree t1, const_tree t2);

extern bool same_type_ignoring_top_level_qualifiers_p (tree, tree);

extern bool comp_ptr_ttypes_const (tree, tree, compare_bounds_t);

extern tree
get_class_binding_direct (tree, tree, bool want_type = false);

extern tree skip_artificial_parms_for (const_tree, tree);

extern void
lang_check_failed (const char *, int,
		   const char *) ATTRIBUTE_NORETURN ATTRIBUTE_COLD;

extern tree default_init_uninitialized_part (tree);

extern bool type_has_non_user_provided_default_constructor (tree);

extern bool default_ctor_p (const_tree);

extern bool user_provided_p (tree);

extern bool sufficient_parms_p (const_tree);

extern tree next_initializable_field (tree);

extern tree in_class_defaulted_default_constructor (tree);

extern bool is_instantiation_of_constexpr (tree);

extern bool
check_for_uninitialized_const_var (tree, bool, tsubst_flags_t);

extern bool reduced_constant_expression_p (tree);

extern tree cv_unqualified (tree);

extern tree cp_get_callee (tree);
extern tree rs_get_callee_fndecl_nofold (tree);

extern bool is_nondependent_static_init_expression (tree);

extern tree build_nop (tree, tree);

extern bool scalarish_type_p (const_tree);

extern tree is_bitfield_expr_with_lowered_type (const_tree);

extern tree convert_bitfield_to_declared_type (tree);

extern tree
cp_fold_maybe_rvalue (tree, bool);

extern tree maybe_undo_parenthesized_ref (tree);

extern tree
fold_offsetof (tree, tree = size_type_node, tree_code ctx = ERROR_MARK);

extern tree cp_truthvalue_conversion (tree, tsubst_flags_t);

extern tree
fold_non_dependent_expr (tree, tsubst_flags_t = tf_warning_or_error,
			 bool = false, tree = NULL_TREE);

extern int char_type_p (tree);

extern bool instantiation_dependent_expression_p (tree);

extern bool type_has_nontrivial_copy_init (const_tree);

extern tree build_local_temp (tree);

extern bool is_normal_capture_proxy (tree);

extern bool reject_gcc_builtin (const_tree, location_t = UNKNOWN_LOCATION);

extern tree resolve_nondeduced_context (tree, tsubst_flags_t);

extern void cxx_incomplete_type_diagnostic (location_t, const_tree, const_tree,
					    diagnostic_t);

extern void cxx_incomplete_type_error (location_t, const_tree, const_tree);

extern bool invalid_nonstatic_memfn_p (location_t, tree, tsubst_flags_t);

extern bool really_overloaded_fn (tree) ATTRIBUTE_PURE;

extern tree resolve_nondeduced_context_or_error (tree, tsubst_flags_t);

extern tree instantiate_non_dependent_or_null (tree);

extern void cxx_incomplete_type_inform (const_tree);

extern tree strip_top_quals (tree);

extern bool undeduced_auto_decl (tree);

extern bool require_deduced_type (tree, tsubst_flags_t = tf_warning_or_error);

extern bool decl_constant_var_p (tree);

extern tree build_new_constexpr_heap_type (tree, tree, tree);

extern bool is_empty_field (tree);

extern bool
in_immediate_context ();

extern tree cp_get_callee_fndecl_nofold (tree);

extern bool
cxx_mark_addressable (tree, bool = false);

extern tree fold_builtin_source_location (location_t);

extern tree build_address (tree);

extern bool bitfield_p (const_tree);

extern tree rvalue (tree);

extern bool glvalue_p (const_tree);

extern cp_lvalue_kind lvalue_kind (const_tree);

extern tree
decl_constant_value (tree, bool);

extern tree lookup_enumerator (tree, tree);

extern int
is_class_type (tree, int);

extern tree braced_lists_to_strings (tree, tree);

extern tree
fold_builtin_is_pointer_inverconvertible_with_class (location_t, int, tree *);

extern bool layout_compatible_type_p (tree, tree);

extern tree finish_underlying_type (tree);

extern tree
c_common_type_for_mode (machine_mode, int);

extern bool std_layout_type_p (const_tree);

extern tree complete_type (tree);

extern tree complete_type_or_else (tree, tree);

extern void note_failed_type_completion_for_satisfaction (tree);

extern tree complete_type_or_maybe_complain (tree, tree, tsubst_flags_t);

extern bool
next_common_initial_seqence (tree &, tree &);

extern bool null_member_pointer_value_p (tree);

extern tree
fold_builtin_is_corresponding_member (location_t, int, tree *);

extern tree cp_fold_rvalue (tree);

extern tree
maybe_constant_value (tree, tree = NULL_TREE, bool = false);

extern tree lvalue_type (tree);

extern void lvalue_error (location_t, enum lvalue_use);

extern tree
cp_fold_maybe_rvalue (tree, bool);

extern tree get_first_fn (tree) ATTRIBUTE_PURE;

extern void explain_non_literal_class (tree);

extern bool reference_related_p (tree, tree);

extern bool ordinary_char_type_p (tree);

extern bool array_string_literal_compatible_p (tree, tree);

// forked from gcc/cp/cp-tree.h

enum
{
  ce_derived,
  ce_type,
  ce_normal,
  ce_exact
};

extern tree
rs_build_qualified_type_real (tree, int, tsubst_flags_t);
#define rs_build_qualified_type(TYPE, QUALS)                                   \
  rs_build_qualified_type_real ((TYPE), (QUALS), tf_warning_or_error)

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

// forked from gcc/cp/cp-tree.h identifier_p

/* Return a typed pointer version of T if it designates a
   C++ front-end identifier.  */
inline lang_identifier *
identifier_p (tree t)
{
  if (TREE_CODE (t) == IDENTIFIER_NODE)
    return (lang_identifier *) t;
  return NULL;
}

// forked from gcc/c-family/c-common.h gnu_vector_type_p

/* Return true if TYPE is a vector type that should be subject to the GNU
   vector extensions (as opposed to a vector type that is used only for
   the purposes of defining target-specific built-in functions).  */

inline bool
gnu_vector_type_p (const_tree type)
{
  return TREE_CODE (type) == VECTOR_TYPE && !TYPE_INDIVISIBLE_P (type);
}

extern vec<tree, va_gc> *
make_tree_vector (void);

extern void
release_tree_vector (vec<tree, va_gc> *);

/* Simplified unique_ptr clone to release a tree vec on exit.  */

class releasing_vec
{
public:
  typedef vec<tree, va_gc> vec_t;

  releasing_vec (vec_t *v) : v (v) {}
  releasing_vec () : v (make_tree_vector ()) {}

  /* Copy ops are deliberately declared but not defined,
     copies must always be elided.  */
  releasing_vec (const releasing_vec &);
  releasing_vec &operator= (const releasing_vec &);

  vec_t &operator* () const { return *v; }
  vec_t *operator-> () const { return v; }
  vec_t *get () const { return v; }
  operator vec_t * () const { return v; }
  vec_t **operator& () { return &v; }

  /* Breaks pointer/value consistency for convenience.  This takes ptrdiff_t
     rather than unsigned to avoid ambiguity with the built-in operator[]
     (bootstrap/91828).  */
  tree &operator[] (ptrdiff_t i) const { return (*v)[i]; }

  tree *begin () { return ::begin (v); }
  tree *end () { return ::end (v); }

  void release ()
  {
    release_tree_vector (v);
    v = NULL;
  }

  ~releasing_vec () { release_tree_vector (v); }

private:
  vec_t *v;
};

inline tree *
vec_safe_push (releasing_vec &r, const tree &t CXX_MEM_STAT_INFO)
{
  return vec_safe_push (*&r, t PASS_MEM_STAT);
}

inline bool
vec_safe_reserve (releasing_vec &r, unsigned n,
		  bool e = false CXX_MEM_STAT_INFO)
{
  return vec_safe_reserve (*&r, n, e PASS_MEM_STAT);
}
inline unsigned
vec_safe_length (releasing_vec &r)
{
  return r->length ();
}
inline void
vec_safe_splice (releasing_vec &r, vec<tree, va_gc> *p CXX_MEM_STAT_INFO)
{
  vec_safe_splice (*&r, p PASS_MEM_STAT);
}

inline bool
null_node_p (const_tree expr)
{
  STRIP_ANY_LOCATION_WRAPPER (expr);
  return expr == null_node;
}

inline void
cxx_incomplete_type_diagnostic (const_tree value, const_tree type,
				diagnostic_t diag_kind)
{
  cxx_incomplete_type_diagnostic (rs_expr_loc_or_input_loc (value), value, type,
				  diag_kind);
}

inline void
cxx_incomplete_type_error (const_tree value, const_tree type)
{
  cxx_incomplete_type_diagnostic (value, type, DK_ERROR);
}

extern location_t
location_of (tree t);

/* Helpers for IMPLICIT_RVALUE_P to look through automatic dereference.  */

inline bool
implicit_rvalue_p (const_tree t)
{
  if (REFERENCE_REF_P (t))
    t = TREE_OPERAND (t, 0);
  return ((TREE_CODE (t) == NON_LVALUE_EXPR) && IMPLICIT_RVALUE_P (t));
}
inline tree
set_implicit_rvalue_p (tree ot)
{
  tree t = ot;
  if (REFERENCE_REF_P (t))
    t = TREE_OPERAND (t, 0);
  IMPLICIT_RVALUE_P (t) = 1;
  return ot;
}

namespace Compile {
extern tree
maybe_constant_init (tree, tree = NULL_TREE, bool = false);

extern void
explain_invalid_constexpr_fn (tree fun);

extern bool potential_constant_expression (tree);

extern bool
literal_type_p (tree t);

extern bool
maybe_constexpr_fn (tree t);

extern tree
fold_non_dependent_init (tree, tsubst_flags_t = tf_warning_or_error,
			 bool = false, tree = NULL_TREE);
} // namespace Compile

} // namespace Rust

#endif // RUST_TREE
