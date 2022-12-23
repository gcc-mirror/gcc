#include "condition.h"


bit_condition::bit_condition (value_bit* left, value_bit* right,
			      condition_type type)
{
  this->left = left;
  this->right = right;
  this->type = type;

  switch (this->type)
    {
      case GREAT_THAN:
	op_sign[0] = '>';
	op_sign[1] = '\0';
	break;
      case LESS_THAN:
	op_sign[0] = '<';
	op_sign[1] = '\0';
	break;
      case EQUAL:
	op_sign[0] = '=';
	op_sign[1] = '=';
	break;
      case NOT_EQUAL:
	op_sign[0] = '!';
	op_sign[1] = '=';
	break;
      case IS_FALSE:
	op_sign[0] = '0';
	op_sign[1] = '\0';
	break;
      case IS_TRUE:
	op_sign[0] = '1';
	op_sign[1] = '\0';
	break;
      default:
	op_sign[0] = '\0';
	op_sign[1] = '\0';
    }
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