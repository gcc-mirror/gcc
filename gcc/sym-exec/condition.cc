#include "condition.h"

bit_condition::bit_condition (value_bit *left, value_bit *right, tree_code code)
{
  this->left = left;
  this->right = right;
  this->code = code;
  type = BIT_CONDITION;
}


bit_condition::bit_condition (const bit_condition &expr)
{
  bit_expression::copy (&expr);
  code = expr.get_code ();
}


tree_code
bit_condition::get_code () const
{
  return code;
}


value_bit *
bit_condition::copy () const
{
  return new bit_condition (*this);
}


void
bit_condition::print_expr_sign ()
{
  switch (code)
    {
      case GT_EXPR:
	fprintf (dump_file, " > ");
	break;
      case LT_EXPR:
	fprintf (dump_file, " < ");
	break;
      case EQ_EXPR:
	fprintf (dump_file, " == ");
	break;
      case NE_EXPR:
	fprintf (dump_file, " != ");
	break;
      default:
	fprintf (dump_file, " ? ");
    }
}