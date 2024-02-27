// Copyright (C) 2020-2024 Free Software Foundation, Inc.

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
namespace Analysis {

// https://github.com/rust-lang/rust/blob/master/library/core/src/ops/arith.rs
class RustLangItem
{
public:
  enum ItemType
  {
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
  };

  static const BiMap<std::string, ItemType> lang_items;

  static tl::optional<ItemType> Parse (const std::string &item);
  static std::string ToString (ItemType type);
  static ItemType OperatorToLangItem (ArithmeticOrLogicalOperator op);
  static ItemType
  CompoundAssignmentOperatorToLangItem (ArithmeticOrLogicalOperator op);
  static ItemType NegationOperatorToLangItem (NegationOperator op);
};

} // namespace Analysis
} // namespace Rust
