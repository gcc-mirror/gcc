#ifndef OPERATOR_H
#define OPERATOR_H

enum Operator {};

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
  GREATER_THAN,	    // std::cmp::PartialEq::gt
  LESS_THAN,	    // std::cmp::PartialEq::lt
  GREATER_OR_EQUAL, // std::cmp::PartialEq::ge
  LESS_OR_EQUAL	    // std::cmp::PartialEq::le
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

#endif
