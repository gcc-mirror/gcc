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

#ifndef RUST_OPERATOR_H
#define RUST_OPERATOR_H

enum class NegationOperator
{
  NEGATE,
  NOT
};

enum class ArithmeticOrLogicalOperator
{
  ADD,	       // std::ops::Add
  SUBTRACT,    // std::ops::Sub
  MULTIPLY,    // std::ops::Mul
  DIVIDE,      // std::ops::Div
  MODULUS,     // std::ops::Rem
  BITWISE_AND, // std::ops::BitAnd
  BITWISE_OR,  // std::ops::BitOr
  BITWISE_XOR, // std::ops::BitXor
  LEFT_SHIFT,  // std::ops::Shl
  RIGHT_SHIFT  // std::ops::Shr
};

enum class ComparisonOperator
{
  EQUAL,	    // std::cmp::PartialEq::eq
  NOT_EQUAL,	    // std::cmp::PartialEq::ne
  GREATER_THAN,	    // std::cmp::PartialOrd::gt
  LESS_THAN,	    // std::cmp::PartialOrd::lt
  GREATER_OR_EQUAL, // std::cmp::PartialOrd::ge
  LESS_OR_EQUAL	    // std::cmp::PartialOrd::le
};

enum class LazyBooleanOperator
{
  LOGICAL_OR,
  LOGICAL_AND
};

enum class CompoundAssignmentOperator
{
  ADD,	       // std::ops::AddAssign
  SUBTRACT,    // std::ops::SubAssign
  MULTIPLY,    // std::ops::MulAssign
  DIVIDE,      // std::ops::DivAssign
  MODULUS,     // std::ops::RemAssign
  BITWISE_AND, // std::ops::BitAndAssign
  BITWISE_OR,  // std::ops::BitOrAssign
  BITWISE_XOR, // std::ops::BitXorAssign
  LEFT_SHIFT,  // std::ops::ShlAssign
  RIGHT_SHIFT  // std::ops::ShrAssign
};

#endif // RUST_OPERATOR_H
