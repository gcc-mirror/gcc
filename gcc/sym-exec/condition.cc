#include "condition.h"

bit_condition::bit_condition (value_bit *left, value_bit *right, tree_code type)
{
  this->left = left;
  this->right = right;
  this->type = type;

  switch (this->type)
    {
      case GT_EXPR:
	op_sign[0] = '>';
	op_sign[1] = '\0';
	break;
      case LT_EXPR:
	op_sign[0] = '<';
	op_sign[1] = '\0';
	break;
      case EQ_EXPR:
	op_sign[0] = '=';
	op_sign[1] = '=';
	break;
      case NE_EXPR:
	op_sign[0] = '!';
	op_sign[1] = '=';
	break;
      default:
	op_sign[0] = '\0';
	op_sign[1] = '\0';
    }
}


bit_condition::bit_condition (const bit_condition &expr)
{
  bit_expression::copy (&expr);
  type = expr.get_code ();
}


tree_code
bit_condition::get_code () const
{
  return type;
}


value_bit *
bit_condition::copy () const
{
  return new bit_condition (*this);
}


value_type
bit_condition::get_type () const
{
  return value_type::BIT_CONDITION;
}