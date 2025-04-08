// Copyright (C) 2020-2025 Free Software Foundation, Inc.

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

#include "rust-system.h"
#include "rust-operators.h"
#include "optional.h"
#include "bi-map.h"

namespace Rust {

class LangItem
{
public:
  // FIXME: We should clean up that enum to make it more inline with the list of
  // lang-items in Rust 1.49
  // https://github.com/rust-lang/rust/blob/1.49.0/compiler/rustc_hir/src/lang_items.rs
  enum class Kind
  {
    // https://github.com/rust-lang/rust/blob/master/library/core/src/ops/arith.rs
    ADD,
    SUBTRACT,
    MULTIPLY,
    DIVIDE,
    REMAINDER,
    BITAND,
    BITOR,
    BITXOR,
    SHL,
    SHR,

    NEGATION,
    NOT,
    EQ,
    PARTIAL_ORD,

    ADD_ASSIGN,
    SUB_ASSIGN,
    MUL_ASSIGN,
    DIV_ASSIGN,
    REM_ASSIGN,
    BITAND_ASSIGN,
    BITOR_ASSIGN,
    BITXOR_ASSIGN,
    SHL_ASSIGN,
    SHR_ASSIGN,

    DEREF,
    DEREF_MUT,
    RECEIVER,

    // https://github.com/rust-lang/rust/blob/master/library/core/src/ops/index.rs
    INDEX,
    INDEX_MUT,

    // https://github.com/rust-lang/rust/blob/master/library/core/src/ops/range.rs
    RANGE_FULL,
    RANGE,
    RANGE_FROM,
    RANGE_TO,
    RANGE_INCLUSIVE,
    RANGE_TO_INCLUSIVE,

    // https://github.com/rust-lang/rust/blob/master/library/core/src/marker.rs
    PHANTOM_DATA,

    // functions
    FN,
    FN_MUT,
    FN_ONCE,
    FN_ONCE_OUTPUT,

    // markers
    COPY,
    CLONE,
    SIZED,
    SYNC,

    // https://github.com/Rust-GCC/gccrs/issues/1896
    // https://github.com/rust-lang/rust/commit/afbecc0f68c4dcfc4878ba5bcb1ac942544a1bdc
    // https://github.com/Rust-GCC/gccrs/issues/1494
    // https://github.com/rust-lang/rust/blob/master/library/core/src/ptr/const_ptr.rs
    SLICE_ALLOC,
    SLICE_U8_ALLOC,
    STR_ALLOC,
    ARRAY,
    BOOL,
    CHAR,
    F32,
    F64,
    I8,
    I16,
    I32,
    I64,
    I128,
    ISIZE,
    U8,
    U16,
    U32,
    U64,
    U128,
    USIZE,
    CONST_PTR,
    CONST_SLICE_PTR,
    MUT_PTR,
    MUT_SLICE_PTR,
    SLICE_U8,
    SLICE,
    STR,
    F32_RUNTIME,
    F64_RUNTIME,

    OPTION_SOME,
    OPTION_NONE,

    RESULT_OK,
    RESULT_ERR,

    INTOITER_INTOITER,
    ITERATOR_NEXT,

    // NOTE: These lang items are *not* necessarily present in later versions of
    // Rust (I am unsure at which point they have been removed as the `Try`
    // trait is unstable). They will need to be changed when updating the
    // targeted Rust version of gccrs
    TRY,
    TRY_INTO_RESULT,
    TRY_FROM_ERROR,
    TRY_FROM_OK,

    // NOTE: This is not a lang item in later versions of Rust
    FROM_FROM,

    STRUCTURAL_PEQ,
    STRUCTURAL_TEQ,

    DISCRIMINANT_TYPE,
    DISCRIMINANT_KIND,

    MANUALLY_DROP,
  };

  static const BiMap<std::string, Kind> lang_items;

  static tl::optional<Kind> Parse (const std::string &item);

  static std::string ToString (Kind type);
  static std::string PrettyString (Kind type);

  static Kind OperatorToLangItem (ArithmeticOrLogicalOperator op);
  static Kind
  CompoundAssignmentOperatorToLangItem (ArithmeticOrLogicalOperator op);
  static Kind NegationOperatorToLangItem (NegationOperator op);
  static Kind ComparisonToLangItem (ComparisonOperator op);
  static std::string ComparisonToSegment (ComparisonOperator op);

  static bool IsEnumVariant (Kind type);
};

} // namespace Rust

// GCC 4.8 needs us to manually implement hashing for enum classes
namespace std {
template <> struct hash<Rust::LangItem::Kind>
{
  size_t operator() (const Rust::LangItem::Kind &lang_item) const noexcept
  {
    return hash<std::underlying_type<Rust::LangItem::Kind>::type> () (
      static_cast<std::underlying_type<Rust::LangItem::Kind>::type> (
	lang_item));
  }
};
} // namespace std
