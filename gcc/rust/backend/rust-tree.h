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

namespace Rust {

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

} // namespace Rust

#endif // RUST_TREE
