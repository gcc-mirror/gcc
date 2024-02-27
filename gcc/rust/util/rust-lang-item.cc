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

#include "rust-lang-item.h"
#include "rust-system.h"

namespace Rust {
namespace Analysis {

const BiMap<std::string, RustLangItem::ItemType>
  Rust::Analysis::RustLangItem::lang_items = {{
    {"add", ADD},
    {"sub", SUBTRACT},
    {"mul", MULTIPLY},
    {"div", DIVIDE},
    {"rem", REMAINDER},
    {"bitand", BITAND},
    {"bitor", BITOR},
    {"bitxor", BITXOR},
    {"shl", SHL},
    {"shr", SHR},
    {"neg", NEGATION},
    {"not", NOT},
    {"add_assign", ADD_ASSIGN},
    {"sub_assign", SUB_ASSIGN},
    {"mul_assign", MUL_ASSIGN},
    {"div_assign", DIV_ASSIGN},
    {"rem_assign", REM_ASSIGN},
    {"bitand_assign", BITAND_ASSIGN},
    {"bitor_assign", BITOR_ASSIGN},
    {"bitxor_assign", BITXOR_ASSIGN},
    {"shl_assign", SHL_ASSIGN},
    {"shr_assign", SHR_ASSIGN},
    {"deref", DEREF},
    {"deref_mut", DEREF_MUT},
    {"index", INDEX},
    {"index_mut", INDEX_MUT},
    {"RangeFull", RANGE_FULL},
    {"Range", RANGE},
    {"RangeFrom", RANGE_FROM},
    {"RangeTo", RANGE_TO},
    {"RangeInclusive", RANGE_INCLUSIVE},
    {"RangeToInclusive", RANGE_TO_INCLUSIVE},
    {"phantom_data", PHANTOM_DATA},
    {"fn", FN},
    {"fn_mut", FN_MUT},
    {"fn_once", FN_ONCE},
    {"fn_once_output", FN_ONCE_OUTPUT},
    {"copy", COPY},
    {"clone", CLONE},
    {"sized", SIZED},
    {"slice_alloc", SLICE_ALLOC},
    {"slice_u8_alloc", SLICE_U8_ALLOC},
    {"str_alloc", STR_ALLOC},
    {"array", ARRAY},
    {"bool", BOOL},
    {"char", CHAR},
    {"f32", F32},
    {"f64", F64},
    {"i8", I8},
    {"i16", I16},
    {"i32", I32},
    {"i64", I64},
    {"i128", I128},
    {"isize", ISIZE},
    {"u8", U8},
    {"u16", U16},
    {"u32", U32},
    {"u64", U64},
    {"u128", U128},
    {"usize", USIZE},
    {"const_ptr", CONST_PTR},
    {"const_slice_ptr", CONST_SLICE_PTR},
    {"mut_ptr", MUT_PTR},
    {"mut_slice_ptr", MUT_SLICE_PTR},
    {"slice_u8", SLICE_U8},
    {"slice", SLICE},
    {"str", STR},
    {"f32_runtime", F32_RUNTIME},
    {"f64_runtime", F64_RUNTIME},
  }};

tl::optional<RustLangItem::ItemType>
RustLangItem::Parse (const std::string &item)
{
  auto lang_item = RustLangItem::lang_items.lookup (item);
  if (!RustLangItem::lang_items.is_iter_ok (lang_item))
    return tl::nullopt;

  return lang_item->second;
}

std::string
RustLangItem::ToString (RustLangItem::ItemType type)
{
  auto str = RustLangItem::lang_items.lookup (type);
  return str->second;
}

RustLangItem::ItemType
RustLangItem::OperatorToLangItem (ArithmeticOrLogicalOperator op)
{
  switch (op)
    {
    case ArithmeticOrLogicalOperator::ADD:
      return RustLangItem::ItemType::ADD;
    case ArithmeticOrLogicalOperator::SUBTRACT:
      return RustLangItem::ItemType::SUBTRACT;
    case ArithmeticOrLogicalOperator::MULTIPLY:
      return RustLangItem::ItemType::MULTIPLY;
    case ArithmeticOrLogicalOperator::DIVIDE:
      return RustLangItem::ItemType::DIVIDE;
    case ArithmeticOrLogicalOperator::MODULUS:
      return RustLangItem::ItemType::REMAINDER;
    case ArithmeticOrLogicalOperator::BITWISE_AND:
      return RustLangItem::ItemType::BITAND;
    case ArithmeticOrLogicalOperator::BITWISE_OR:
      return RustLangItem::ItemType::BITOR;
    case ArithmeticOrLogicalOperator::BITWISE_XOR:
      return RustLangItem::ItemType::BITXOR;
    case ArithmeticOrLogicalOperator::LEFT_SHIFT:
      return RustLangItem::ItemType::SHL;
    case ArithmeticOrLogicalOperator::RIGHT_SHIFT:
      return RustLangItem::ItemType::SHR;
    }

  rust_unreachable ();
}

RustLangItem::ItemType
RustLangItem::CompoundAssignmentOperatorToLangItem (
  ArithmeticOrLogicalOperator op)
{
  switch (op)
    {
    case ArithmeticOrLogicalOperator::ADD:
      return RustLangItem::ItemType::ADD_ASSIGN;
    case ArithmeticOrLogicalOperator::SUBTRACT:
      return RustLangItem::ItemType::SUB_ASSIGN;
    case ArithmeticOrLogicalOperator::MULTIPLY:
      return RustLangItem::ItemType::MUL_ASSIGN;
    case ArithmeticOrLogicalOperator::DIVIDE:
      return RustLangItem::ItemType::DIV_ASSIGN;
    case ArithmeticOrLogicalOperator::MODULUS:
      return RustLangItem::ItemType::REM_ASSIGN;
    case ArithmeticOrLogicalOperator::BITWISE_AND:
      return RustLangItem::ItemType::BITAND_ASSIGN;
    case ArithmeticOrLogicalOperator::BITWISE_OR:
      return RustLangItem::ItemType::BITOR_ASSIGN;
    case ArithmeticOrLogicalOperator::BITWISE_XOR:
      return RustLangItem::ItemType::BITXOR_ASSIGN;
    case ArithmeticOrLogicalOperator::LEFT_SHIFT:
      return RustLangItem::ItemType::SHL_ASSIGN;
    case ArithmeticOrLogicalOperator::RIGHT_SHIFT:
      return RustLangItem::ItemType::SHR_ASSIGN;
    }

  rust_unreachable ();
}

RustLangItem::ItemType
RustLangItem::NegationOperatorToLangItem (NegationOperator op)
{
  switch (op)
    {
    case NegationOperator::NEGATE:
      return RustLangItem::ItemType::NEGATION;
    case NegationOperator::NOT:
      return RustLangItem::ItemType::NOT;
    }

  rust_unreachable ();
}

} // namespace Analysis
} // namespace Rust
