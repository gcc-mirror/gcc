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

/* [basic.fundamental]

   Types  bool, char, wchar_t, and the signed and unsigned integer types
   are collectively called integral types.

   Note that INTEGRAL_TYPE_P, as defined in tree.h, allows enumeration
   types as well, which is incorrect in C++.  Keep these checks in
   ascending code order.  */
#define RS_INTEGRAL_TYPE_P(TYPE)                                               \
  (TREE_CODE (TYPE) == BOOLEAN_TYPE || TREE_CODE (TYPE) == INTEGER_TYPE)

/* Returns true if TYPE is an integral or enumeration name.  Keep
   these checks in ascending code order.  */
#define INTEGRAL_OR_ENUMERATION_TYPE_P(TYPE)                                   \
  (TREE_CODE (TYPE) == ENUMERAL_TYPE || RS_INTEGRAL_TYPE_P (TYPE))

/* Nonzero for a VAR_DECL that was initialized with a
   constant-expression.  */
#define DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P(NODE)                        \
  (TREE_LANG_FLAG_2 (VAR_DECL_CHECK (NODE)))

// Above macros are copied from gcc/c-family/c-common.h

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

extern bool decl_maybe_constant_var_p (tree);

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

} // namespace Rust

#endif // RUST_TREE
