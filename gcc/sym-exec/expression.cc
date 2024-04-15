/* Every class defined here represents a single bit value of a variable.
   Every variable will be represented as a vector of these classes which later
   will be used for bit-level symbolic execution.  */

#include "expression-is-a-helper.h"

value_type
value_bit::get_type () const
{
  return m_type;
}

symbolic_bit::symbolic_bit (size_t i, tree orig)
    : value_bit (i), m_origin (orig)
{
  m_type = SYMBOLIC_BIT;
}


bit::bit (unsigned char i) : m_val (i)
{
  m_type = BIT;
}


value_bit *
bit_expression::get_left ()
{
  return m_left;
}


value_bit *
bit_expression::get_right ()
{
  return m_right;
}


void
bit_expression::set_left (value_bit *expr)
{
  m_left = expr;
}


void
bit_expression::set_right (value_bit *expr)
{
  m_right = expr;
}


size_t
value_bit::get_index () const
{
  return m_index;
}


unsigned char
bit::get_val () const
{
  return m_val;
}


void
bit::set_val (unsigned char new_val)
{
  m_val = new_val;
}


bit_complement_expression::bit_complement_expression (value_bit *right)
{
  this->m_left = nullptr;
  this->m_right = right;
  m_type = BIT_COMPLEMENT_EXPRESSION;
}


bit_complement_expression::bit_complement_expression (
  const bit_complement_expression &expr)
{
  bit_expression::copy (&expr);
}


bit_expression::~bit_expression ()
{
  delete m_left;
  m_left = nullptr;
  delete m_right;
  m_right = nullptr;
}


value_bit *
symbolic_bit::copy () const
{
  return new symbolic_bit (*this);
}


value_bit *
bit::copy () const
{
  return new bit (*this);
}


void
bit_expression::copy (const bit_expression *expr)
{
  if (expr->m_left)
    m_left = expr->m_left->copy ();

  if (expr->m_right)
    m_right = expr->m_right->copy ();

  m_type = expr->m_type;
}


value_bit *
bit_xor_expression::copy () const
{
  return new bit_xor_expression (*this);
}


value_bit *
bit_and_expression::copy () const
{
  return new bit_and_expression (*this);
}


value_bit *
bit_or_expression::copy () const
{
  return new bit_or_expression (*this);
}


value_bit *
shift_right_expression::copy () const
{
  return new shift_right_expression (*this);
}


value_bit *
shift_left_expression::copy () const
{
  return new shift_left_expression (*this);
}


value_bit *
add_expression::copy () const
{
  return new add_expression (*this);
}


value_bit *
sub_expression::copy () const
{
  return new sub_expression (*this);
}


value_bit *
bit_complement_expression::copy () const
{
  return new bit_complement_expression (*this);
}


bit_xor_expression::bit_xor_expression (value_bit *left, value_bit *right)
{
  this->m_left = left;
  this->m_right = right;
  m_type = BIT_XOR_EXPRESSION;
}


bit_xor_expression::bit_xor_expression (const bit_xor_expression &expr)
{
  bit_expression::copy (&expr);
}


bit_and_expression::bit_and_expression (value_bit *left, value_bit *right)
{
  this->m_left = left;
  this->m_right = right;
  m_type = BIT_AND_EXPRESSION;
}


bit_and_expression::bit_and_expression (const bit_and_expression &expr)
{
  bit_expression::copy (&expr);
}


bit_or_expression::bit_or_expression (value_bit *left, value_bit *right)
{
  this->m_left = left;
  this->m_right = right;
  m_type = BIT_OR_EXPRESSION;
}


bit_or_expression::bit_or_expression (const bit_or_expression &expr)
{
  bit_expression::copy (&expr);
}


shift_right_expression::shift_right_expression (value_bit *left,
						value_bit *right)
{
  this->m_left = left;
  this->m_right = right;
  m_type = SHIFT_RIGHT_EXPRESSION;
}


shift_right_expression::shift_right_expression (
  const shift_right_expression &expr)
{
  bit_expression::copy (&expr);
}


shift_left_expression::shift_left_expression (value_bit *left, value_bit *right)
{
  this->m_left = left;
  this->m_right = right;
  m_type = SHIFT_LEFT_EXPRESSION;
}


shift_left_expression::shift_left_expression (const shift_left_expression &expr)
{
  bit_expression::copy (&expr);
}


add_expression::add_expression (value_bit *left, value_bit *right)
{
  this->m_left = left;
  this->m_right = right;
  m_type = ADD_EXPRESSION;
}


add_expression::add_expression (const add_expression &expr)
{
  bit_expression::copy (&expr);
}


sub_expression::sub_expression (value_bit *left, value_bit *right)
{
  this->m_left = left;
  this->m_right = right;
  m_type = SUB_EXPRESSION;
}


sub_expression::sub_expression (const sub_expression &expr)
{
  bit_expression::copy (&expr);
}


tree
symbolic_bit::get_origin ()
{
  return m_origin;
}


void
symbolic_bit::print ()
{
  if (dump_file)
    {
      print_generic_expr (dump_file, m_origin, dump_flags);
      fprintf (dump_file, "[%zu]", m_index);
    }
}


void
bit::print ()
{
  if (dump_file)
    fprintf (dump_file, "%u", m_val);
}


void
bit_expression::print_expr_sign ()
{
  switch (m_type)
    {
      case BIT_XOR_EXPRESSION:
	fprintf (dump_file, " ^ ");
	break;
      case BIT_AND_EXPRESSION:
	fprintf (dump_file, " & ");
	break;
      case BIT_OR_EXPRESSION:
	fprintf (dump_file, " | ");
	break;
      case SHIFT_RIGHT_EXPRESSION:
	fprintf (dump_file, " >> ");
	break;
      case SHIFT_LEFT_EXPRESSION:
	fprintf (dump_file, " << ");
	break;
      case ADD_EXPRESSION:
	fprintf (dump_file, " + ");
	break;
      case SUB_EXPRESSION:
	fprintf (dump_file, " - ");
	break;
      default:
	fprintf (dump_file, " ?? ");
    }
}


void
bit_expression::print ()
{
  if (dump_file)
    {
      fprintf (dump_file, "(");
      if (m_left)
	m_left->print ();
      else
	fprintf (dump_file, "null");

      print_expr_sign ();

      if (m_right)
	m_right->print ();
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
      fprintf (dump_file, "!");
      if (m_right)
	m_right->print ();
      else
	fprintf (dump_file, "null");
    }
}