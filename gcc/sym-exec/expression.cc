/* Every class defined here represents a single bit value of a variable.
   Every variable will be represented as a vector of these classes which later
   will be used for bit-level symbolic execution.  */

#include "expression-is-a-helper.h"


value *
bit_expression::get_left ()
{
  return left;
}


value *
bit_expression::get_right ()
{
  return right;
}


void
bit_expression::set_left (value *expr)
{
  left = expr;
}


void
bit_expression::set_right (value *expr)
{
  right = expr;
}


size_t
value::get_index () const
{
  return index;
}


unsigned char
bit::get_val () const
{
  return val;
}


void
bit::set_val (unsigned char new_val)
{
  val = new_val;
}


bit_complement_expression::bit_complement_expression (value *right)
{
  this->left = nullptr;
  this->right = right;
  op_sign[0] = '!';
  op_sign[1] = '\0';
}


bit_complement_expression::bit_complement_expression (
	const bit_complement_expression& expr)
{
  bit_expression::copy (&expr);
}


bit_expression::~bit_expression ()
{
  delete left;
  left = nullptr;
  delete right;
  right = nullptr;
}


value*
symbolic_bit::copy () const
{
  return new symbolic_bit (*this);
}


value *
bit::copy () const
{
  return new bit (*this);
}


void
bit_expression::copy (const bit_expression *expr)
{
  if (expr->left)
    left = expr->left->copy ();

  if (expr->right)
    right = expr->right->copy ();

  op_sign[0] = (expr->op_sign)[0];
  op_sign[1] = (expr->op_sign)[1];
}


value *
bit_xor_expression::copy () const
{
  return new bit_xor_expression (*this);
}


value *
bit_and_expression::copy () const
{
  return new bit_and_expression (*this);
}


value *
bit_or_expression::copy () const
{
  return new bit_or_expression (*this);
}


value *
shift_right_expression::copy () const
{
  return new shift_right_expression (*this);
}


value *
shift_left_expression::copy () const
{
  return new shift_left_expression (*this);
}


value *
add_expression::copy () const
{
  return new add_expression (*this);
}


value *
sub_expression::copy () const
{
  return new sub_expression (*this);
}


value *
bit_complement_expression::copy () const
{
  return new bit_complement_expression (*this);
}


bit_xor_expression::bit_xor_expression (value *left, value *right)
{
  this->left = left;
  this->right = right;
  op_sign[0] = '^';
  op_sign[1] = '\0';
}


bit_xor_expression::bit_xor_expression (const bit_xor_expression &expr)
{
  bit_expression::copy (&expr);
}


bit_and_expression::bit_and_expression (value *left, value *right)
{
  this->left = left;
  this->right = right;
  op_sign[0] = '&';
  op_sign[1] = '\0';
}


bit_and_expression::bit_and_expression (const bit_and_expression& expr)
{
  bit_expression::copy (&expr);
}


bit_or_expression::bit_or_expression (value *left, value *right)
{
  this->left = left;
  this->right = right;
  op_sign[0] = '|';
  op_sign[1] = '\0';
}


bit_or_expression::bit_or_expression (const bit_or_expression& expr)
{
  bit_expression::copy (&expr);
}


shift_right_expression::shift_right_expression (value *left, value *right)
{
  this->left = left;
  this->right = right;
  op_sign[0] = '>';
  op_sign[1] = '>';
}


shift_right_expression::shift_right_expression (
	const shift_right_expression& expr)
{
  bit_expression::copy (&expr);
}


shift_left_expression::shift_left_expression (value *left, value *right)
{
  this->left = left;
  this->right = right;
  op_sign[0] = '<';
  op_sign[1] = '<';
}


shift_left_expression::shift_left_expression (const shift_left_expression& expr)
{
  bit_expression::copy (&expr);
}


add_expression::add_expression (value *left, value *right)
{
  this->left = left;
  this->right = right;
  op_sign[0] = '+';
  op_sign[1] = '\0';
}


add_expression::add_expression (const add_expression& expr)
{
  bit_expression::copy (&expr);
}


sub_expression::sub_expression (value *left, value *right)
{
  this->left = left;
  this->right = right;
  op_sign[0] = '-';
  op_sign[1] = '\0';
}


sub_expression::sub_expression (const sub_expression& expr)
{
  bit_expression::copy (&expr);
}


value_type
symbolic_bit::get_type () const
{
  return value_type::SYMBOLIC_BIT;
}


value_type
bit::get_type () const
{
  return value_type::BIT;
}


value_type
bit_and_expression::get_type () const
{
  return value_type::BIT_AND_EXPRESSION;
}


value_type
bit_or_expression::get_type () const
{
  return value_type::BIT_OR_EXPRESSION;
}


value_type
bit_xor_expression::get_type () const
{
  return value_type::BIT_XOR_EXPRESSION;
}


value_type
bit_complement_expression::get_type () const
{
  return value_type::BIT_COMPLEMENT_EXPRESSION;
}


value_type
shift_left_expression::get_type () const
{
  return value_type::SHIFT_LEFT_EXPRESSION;
}


value_type
shift_right_expression::get_type () const
{
  return value_type::SHIFT_RIGHT_EXPRESSION;
}


value_type
add_expression::get_type () const
{
  return value_type::ADD_EXPRESSION;
}


value_type
sub_expression::get_type () const
{
  return value_type::SUB_EXPRESSION;
}


tree
symbolic_bit::get_origin ()
{
  return origin;
}


void
symbolic_bit::print ()
{
  if (dump_file)
    {
      print_generic_expr (dump_file, origin, dump_flags);
      fprintf (dump_file, "[%lu]", index);
    }
}


void
bit::print ()
{
  if (dump_file)
    fprintf (dump_file, "%u", val);
}


void
bit_expression::print ()
{
  if (dump_file)
    {
      fprintf (dump_file, "(");
      if (left)
	left->print ();
      else
	fprintf (dump_file, "null");

      fprintf (dump_file, " %.2s ", op_sign);

      if (right)
	right->print ();
      else
	fprintf (dump_file, "null");

      fprintf (dump_file, ")");
    }
}


void
bit_complement_expression::print ()
{
  if (dump_file)
    {
      fprintf (dump_file, "%.2s", op_sign);
      if (right)
	right->print ();
      else
	fprintf (dump_file, "null");
    }
}