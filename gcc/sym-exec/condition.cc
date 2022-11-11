#include "condition.h"


bit_condition::bit_condition (value* left, value* right, condition_type type)
{
  this->left = left;
  this->right = right;
  this->type = type;
}


bit_condition::bit_condition (const bit_condition &expr)
{
  bit_expression::copy (&expr);
  type = expr.get_cond_type ();
}


condition_type
bit_condition::get_cond_type () const
{
  return type;
}


value *
bit_condition::copy () const
{
  return new bit_condition (*this);
}


value_type
bit_condition::get_type () const
{
  return value_type::BIT_CONDITION;
}