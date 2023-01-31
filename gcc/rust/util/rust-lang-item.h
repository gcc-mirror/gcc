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

#include "rust-system.h"
#include "rust-operators.h"

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

    // https://github.com/rust-lang/rust/blob/master/library/core/src/ptr/const_ptr.rs
    CONST_PTR,
    MUT_PTR,
    CONST_SLICE_PTR,

    // https://github.com/rust-lang/rust/blob/master/library/core/src/marker.rs
    PHANTOM_DATA,

    // functions
    FN_ONCE,
    FN_ONCE_OUTPUT,

    // markers
    COPY,
    CLONE,
    SIZED,

    // delimiter
    UNKNOWN,
  };

  static ItemType Parse (const std::string &item)
  {
    if (item.compare ("add") == 0)
      {
	return ItemType::ADD;
      }
    else if (item.compare ("sub") == 0)
      {
	return ItemType::SUBTRACT;
      }
    else if (item.compare ("mul") == 0)
      {
	return ItemType::MULTIPLY;
      }
    else if (item.compare ("div") == 0)
      {
	return ItemType::DIVIDE;
      }
    else if (item.compare ("rem") == 0)
      {
	return ItemType::REMAINDER;
      }
    else if (item.compare ("bitand") == 0)
      {
	return ItemType::BITAND;
      }
    else if (item.compare ("bitor") == 0)
      {
	return ItemType::BITOR;
      }
    else if (item.compare ("bitxor") == 0)
      {
	return ItemType::BITXOR;
      }
    else if (item.compare ("shl") == 0)
      {
	return ItemType::SHL;
      }
    else if (item.compare ("shr") == 0)
      {
	return ItemType::SHR;
      }
    else if (item.compare ("neg") == 0)
      {
	return ItemType::NEGATION;
      }
    else if (item.compare ("not") == 0)
      {
	return ItemType::NOT;
      }
    else if (item.compare ("add_assign") == 0)
      {
	return ItemType::ADD_ASSIGN;
      }
    else if (item.compare ("sub_assign") == 0)
      {
	return ItemType::SUB_ASSIGN;
      }
    else if (item.compare ("mul_assign") == 0)
      {
	return ItemType::MUL_ASSIGN;
      }
    else if (item.compare ("div_assign") == 0)
      {
	return ItemType::DIV_ASSIGN;
      }
    else if (item.compare ("rem_assign") == 0)
      {
	return ItemType::REM_ASSIGN;
      }
    else if (item.compare ("bitand_assign") == 0)
      {
	return ItemType::BITAND_ASSIGN;
      }
    else if (item.compare ("bitor_assign") == 0)
      {
	return ItemType::BITOR_ASSIGN;
      }
    else if (item.compare ("bitxor_assign") == 0)
      {
	return ItemType::BITXOR_ASSIGN;
      }
    else if (item.compare ("shl_assign") == 0)
      {
	return ItemType::SHL_ASSIGN;
      }
    else if (item.compare ("shr_assign") == 0)
      {
	return ItemType::SHR_ASSIGN;
      }
    else if (item.compare ("deref") == 0)
      {
	return ItemType::DEREF;
      }
    else if (item.compare ("deref_mut") == 0)
      {
	return ItemType::DEREF_MUT;
      }
    else if (item.compare ("index") == 0)
      {
	return ItemType::INDEX;
      }
    else if (item.compare ("index_mut") == 0)
      {
	return ItemType::INDEX_MUT;
      }
    else if (item.compare ("RangeFull") == 0)
      {
	return ItemType::RANGE_FULL;
      }
    else if (item.compare ("Range") == 0)
      {
	return ItemType::RANGE;
      }
    else if (item.compare ("RangeFrom") == 0)
      {
	return ItemType::RANGE_FROM;
      }
    else if (item.compare ("RangeTo") == 0)
      {
	return ItemType::RANGE_TO;
      }
    else if (item.compare ("RangeInclusive") == 0)
      {
	return ItemType::RANGE_INCLUSIVE;
      }
    else if (item.compare ("RangeToInclusive") == 0)
      {
	return ItemType::RANGE_TO_INCLUSIVE;
      }
    else if (item.compare ("const_ptr") == 0)
      {
	return ItemType::CONST_PTR;
      }
    else if (item.compare ("mut_ptr") == 0)
      {
	return ItemType::MUT_PTR;
      }
    else if (item.compare ("const_slice_ptr") == 0)
      {
	return ItemType::CONST_SLICE_PTR;
      }
    else if (item.compare ("phantom_data") == 0)
      {
	return ItemType::PHANTOM_DATA;
      }
    else if (item.compare ("fn_once") == 0)
      {
	return ItemType::FN_ONCE;
      }
    else if (item.compare ("fn_once_output") == 0)
      {
	return ItemType::FN_ONCE_OUTPUT;
      }
    else if (item.compare ("copy") == 0)
      {
	return ItemType::COPY;
      }
    else if (item.compare ("clone") == 0)
      {
	return ItemType::CLONE;
      }
    else if (item.compare ("sized") == 0)
      {
	return ItemType::SIZED;
      }

    return ItemType::UNKNOWN;
  }

  static std::string ToString (ItemType type)
  {
    switch (type)
      {
      case ADD:
	return "add";
      case SUBTRACT:
	return "sub";
      case MULTIPLY:
	return "mul";
      case DIVIDE:
	return "div";
      case REMAINDER:
	return "rem";
      case BITAND:
	return "bitand";
      case BITOR:
	return "bitor";
      case BITXOR:
	return "bitxor";
      case SHL:
	return "shl";
      case SHR:
	return "shr";
      case NEGATION:
	return "neg";
      case NOT:
	return "not";
      case ADD_ASSIGN:
	return "add_assign";
      case SUB_ASSIGN:
	return "sub_assign";
      case MUL_ASSIGN:
	return "mul_assign";
      case DIV_ASSIGN:
	return "div_assign";
      case REM_ASSIGN:
	return "rem_assign";
      case BITAND_ASSIGN:
	return "bitand_assign";
      case BITOR_ASSIGN:
	return "bitor_assign";
      case BITXOR_ASSIGN:
	return "bitxor_assign";
      case SHL_ASSIGN:
	return "shl_assign";
      case SHR_ASSIGN:
	return "shr_assign";
      case DEREF:
	return "deref";
      case DEREF_MUT:
	return "deref_mut";
      case INDEX:
	return "index";
      case INDEX_MUT:
	return "index_mut";
      case RANGE_FULL:
	return "RangeFull";
      case RANGE:
	return "Range";
      case RANGE_FROM:
	return "RangeFrom";
      case RANGE_TO:
	return "RangeTo";
      case RANGE_INCLUSIVE:
	return "RangeInclusive";
      case RANGE_TO_INCLUSIVE:
	return "RangeToInclusive";
      case CONST_PTR:
	return "const_ptr";
      case MUT_PTR:
	return "mut_ptr";
      case CONST_SLICE_PTR:
	return "const_slice_ptr";
      case PHANTOM_DATA:
	return "phantom_data";
      case FN_ONCE:
	return "fn_once";
      case FN_ONCE_OUTPUT:
	return "fn_once_output";
      case COPY:
	return "copy";
      case CLONE:
	return "clone";
      case SIZED:
	return "sized";

      case UNKNOWN:
	return "<UNKNOWN>";
      }
    return "<UNKNOWN>";
  }

  static ItemType OperatorToLangItem (ArithmeticOrLogicalOperator op)
  {
    switch (op)
      {
      case ArithmeticOrLogicalOperator::ADD:
	return ItemType::ADD;
      case ArithmeticOrLogicalOperator::SUBTRACT:
	return ItemType::SUBTRACT;
      case ArithmeticOrLogicalOperator::MULTIPLY:
	return ItemType::MULTIPLY;
      case ArithmeticOrLogicalOperator::DIVIDE:
	return ItemType::DIVIDE;
      case ArithmeticOrLogicalOperator::MODULUS:
	return ItemType::REMAINDER;
      case ArithmeticOrLogicalOperator::BITWISE_AND:
	return ItemType::BITAND;
      case ArithmeticOrLogicalOperator::BITWISE_OR:
	return ItemType::BITOR;
      case ArithmeticOrLogicalOperator::BITWISE_XOR:
	return ItemType::BITXOR;
      case ArithmeticOrLogicalOperator::LEFT_SHIFT:
	return ItemType::SHL;
      case ArithmeticOrLogicalOperator::RIGHT_SHIFT:
	return ItemType::SHR;
      }
    return ItemType::UNKNOWN;
  }

  static ItemType
  CompoundAssignmentOperatorToLangItem (ArithmeticOrLogicalOperator op)
  {
    switch (op)
      {
      case ArithmeticOrLogicalOperator::ADD:
	return ItemType::ADD_ASSIGN;
      case ArithmeticOrLogicalOperator::SUBTRACT:
	return ItemType::SUB_ASSIGN;
      case ArithmeticOrLogicalOperator::MULTIPLY:
	return ItemType::MUL_ASSIGN;
      case ArithmeticOrLogicalOperator::DIVIDE:
	return ItemType::DIV_ASSIGN;
      case ArithmeticOrLogicalOperator::MODULUS:
	return ItemType::REM_ASSIGN;
      case ArithmeticOrLogicalOperator::BITWISE_AND:
	return ItemType::BITAND_ASSIGN;
      case ArithmeticOrLogicalOperator::BITWISE_OR:
	return ItemType::BITOR_ASSIGN;
      case ArithmeticOrLogicalOperator::BITWISE_XOR:
	return ItemType::BITXOR_ASSIGN;
      case ArithmeticOrLogicalOperator::LEFT_SHIFT:
	return ItemType::SHL_ASSIGN;
      case ArithmeticOrLogicalOperator::RIGHT_SHIFT:
	return ItemType::SHR_ASSIGN;
      }
    return ItemType::UNKNOWN;
  }

  static ItemType NegationOperatorToLangItem (NegationOperator op)
  {
    switch (op)
      {
      case NegationOperator::NEGATE:
	return ItemType::NEGATION;
      case NegationOperator::NOT:
	return ItemType::NOT;
      }
    return ItemType::UNKNOWN;
  }
};

} // namespace Analysis
} // namespace Rust
