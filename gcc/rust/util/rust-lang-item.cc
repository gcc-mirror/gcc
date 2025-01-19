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

#include "rust-lang-item.h"
#include "rust-system.h"

namespace Rust {

const BiMap<std::string, LangItem::Kind> Rust::LangItem::lang_items = {{
  {"add", Kind::ADD},
  {"sub", Kind::SUBTRACT},
  {"mul", Kind::MULTIPLY},
  {"div", Kind::DIVIDE},
  {"rem", Kind::REMAINDER},
  {"bitand", Kind::BITAND},
  {"bitor", Kind::BITOR},
  {"bitxor", Kind::BITXOR},
  {"shl", Kind::SHL},
  {"shr", Kind::SHR},
  {"neg", Kind::NEGATION},
  {"not", Kind::NOT},
  {"add_assign", Kind::ADD_ASSIGN},
  {"sub_assign", Kind::SUB_ASSIGN},
  {"mul_assign", Kind::MUL_ASSIGN},
  {"div_assign", Kind::DIV_ASSIGN},
  {"rem_assign", Kind::REM_ASSIGN},
  {"bitand_assign", Kind::BITAND_ASSIGN},
  {"bitor_assign", Kind::BITOR_ASSIGN},
  {"bitxor_assign", Kind::BITXOR_ASSIGN},
  {"shl_assign", Kind::SHL_ASSIGN},
  {"shr_assign", Kind::SHR_ASSIGN},
  {"deref", Kind::DEREF},
  {"deref_mut", Kind::DEREF_MUT},
  {"index", Kind::INDEX},
  {"index_mut", Kind::INDEX_MUT},
  {"RangeFull", Kind::RANGE_FULL},
  {"Range", Kind::RANGE},
  {"RangeFrom", Kind::RANGE_FROM},
  {"RangeTo", Kind::RANGE_TO},
  {"RangeInclusive", Kind::RANGE_INCLUSIVE},
  {"RangeToInclusive", Kind::RANGE_TO_INCLUSIVE},
  {"phantom_data", Kind::PHANTOM_DATA},
  {"fn", Kind::FN},
  {"fn_mut", Kind::FN_MUT},
  {"fn_once", Kind::FN_ONCE},
  {"fn_once_output", Kind::FN_ONCE_OUTPUT},
  {"copy", Kind::COPY},
  {"clone", Kind::CLONE},
  {"sized", Kind::SIZED},
  {"slice_alloc", Kind::SLICE_ALLOC},
  {"slice_u8_alloc", Kind::SLICE_U8_ALLOC},
  {"str_alloc", Kind::STR_ALLOC},
  {"array", Kind::ARRAY},
  {"bool", Kind::BOOL},
  {"char", Kind::CHAR},
  {"f32", Kind::F32},
  {"f64", Kind::F64},
  {"i8", Kind::I8},
  {"i16", Kind::I16},
  {"i32", Kind::I32},
  {"i64", Kind::I64},
  {"i128", Kind::I128},
  {"isize", Kind::ISIZE},
  {"u8", Kind::U8},
  {"u16", Kind::U16},
  {"u32", Kind::U32},
  {"u64", Kind::U64},
  {"u128", Kind::U128},
  {"usize", Kind::USIZE},
  {"const_ptr", Kind::CONST_PTR},
  {"const_slice_ptr", Kind::CONST_SLICE_PTR},
  {"mut_ptr", Kind::MUT_PTR},
  {"mut_slice_ptr", Kind::MUT_SLICE_PTR},
  {"slice_u8", Kind::SLICE_U8},
  {"slice", Kind::SLICE},
  {"str", Kind::STR},
  {"f32_runtime", Kind::F32_RUNTIME},
  {"f64_runtime", Kind::F64_RUNTIME},
}};

tl::optional<LangItem::Kind>
LangItem::Parse (const std::string &item)
{
  auto lang_item = LangItem::lang_items.lookup (item);

  return lang_item;
}

std::string
LangItem::ToString (LangItem::Kind type)
{
  auto str = LangItem::lang_items.lookup (type);
  return str.value ();
}

LangItem::Kind
LangItem::OperatorToLangItem (ArithmeticOrLogicalOperator op)
{
  switch (op)
    {
    case ArithmeticOrLogicalOperator::ADD:
      return LangItem::Kind::ADD;
    case ArithmeticOrLogicalOperator::SUBTRACT:
      return LangItem::Kind::SUBTRACT;
    case ArithmeticOrLogicalOperator::MULTIPLY:
      return LangItem::Kind::MULTIPLY;
    case ArithmeticOrLogicalOperator::DIVIDE:
      return LangItem::Kind::DIVIDE;
    case ArithmeticOrLogicalOperator::MODULUS:
      return LangItem::Kind::REMAINDER;
    case ArithmeticOrLogicalOperator::BITWISE_AND:
      return LangItem::Kind::BITAND;
    case ArithmeticOrLogicalOperator::BITWISE_OR:
      return LangItem::Kind::BITOR;
    case ArithmeticOrLogicalOperator::BITWISE_XOR:
      return LangItem::Kind::BITXOR;
    case ArithmeticOrLogicalOperator::LEFT_SHIFT:
      return LangItem::Kind::SHL;
    case ArithmeticOrLogicalOperator::RIGHT_SHIFT:
      return LangItem::Kind::SHR;
    }

  rust_unreachable ();
}

LangItem::Kind
LangItem::CompoundAssignmentOperatorToLangItem (ArithmeticOrLogicalOperator op)
{
  switch (op)
    {
    case ArithmeticOrLogicalOperator::ADD:
      return LangItem::Kind::ADD_ASSIGN;
    case ArithmeticOrLogicalOperator::SUBTRACT:
      return LangItem::Kind::SUB_ASSIGN;
    case ArithmeticOrLogicalOperator::MULTIPLY:
      return LangItem::Kind::MUL_ASSIGN;
    case ArithmeticOrLogicalOperator::DIVIDE:
      return LangItem::Kind::DIV_ASSIGN;
    case ArithmeticOrLogicalOperator::MODULUS:
      return LangItem::Kind::REM_ASSIGN;
    case ArithmeticOrLogicalOperator::BITWISE_AND:
      return LangItem::Kind::BITAND_ASSIGN;
    case ArithmeticOrLogicalOperator::BITWISE_OR:
      return LangItem::Kind::BITOR_ASSIGN;
    case ArithmeticOrLogicalOperator::BITWISE_XOR:
      return LangItem::Kind::BITXOR_ASSIGN;
    case ArithmeticOrLogicalOperator::LEFT_SHIFT:
      return LangItem::Kind::SHL_ASSIGN;
    case ArithmeticOrLogicalOperator::RIGHT_SHIFT:
      return LangItem::Kind::SHR_ASSIGN;
    }

  rust_unreachable ();
}

LangItem::Kind
LangItem::NegationOperatorToLangItem (NegationOperator op)
{
  switch (op)
    {
    case NegationOperator::NEGATE:
      return LangItem::Kind::NEGATION;
    case NegationOperator::NOT:
      return LangItem::Kind::NOT;
    }

  rust_unreachable ();
}

} // namespace Rust
