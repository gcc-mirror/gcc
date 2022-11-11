/* Every class defined here represents a single bit value of a variable.
   Every variable will be represented as a vector of these classes which later
   will be used for bit-level symbolic execution.  */

#include "stddef.h"
#include "expression.h"


bit_expression::bit_expression (value* left, value* right)
{
  this->left = left;
  this->right = right;
}


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


bit_complement_expression::bit_complement_expression (value *right) :
  bit_expression (nullptr, right)
{}


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


bit_expression::bit_expression (const bit_expression &expr)
{
  if (expr.left)
    {
      left = expr.left->copy ();
    }

  if (expr.right)
    {
      right = expr.right->copy ();
    }
}


value *
bit_expression::copy () const
{
  return new bit_expression (*this);
}


value *
bit_xor_expression::copy () const
{
  return bit_expression::copy ();
}


value *
bit_and_expression::copy () const
{
  return bit_expression::copy ();
}


value *
bit_or_expression::copy () const
{
  return bit_expression::copy ();
}


value *
shift_right_expression::copy () const
{
  return bit_expression::copy ();
}


value *
shift_left_expression::copy () const
{
  return bit_expression::copy ();
}


value *
add_expression::copy () const
{
  return bit_expression::copy ();
}


value *
sub_expression::copy () const
{
  return bit_expression::copy ();
}


value *
bit_complement_expression::copy () const
{
  return bit_expression::copy ();
}


bit_expression::~bit_expression ()
{
  delete left;
  left = nullptr;

  delete right;
  right = nullptr;
}
