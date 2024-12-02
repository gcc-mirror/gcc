/* Every class defined here represents a single bit value of a variable.
   Every variable will be represented as a vector of these classes which later
   will be used for bit-level symbolic execution.
   Copyright (C) 2022-2024 Free Software Foundation, Inc.
   Contributed by Matevos Mehrabyan <matevosmehrabyan@gmail.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "sym-exec-expr-is-a-helper.h"

/* Returns type of the bit.  */

value_type
value_bit::get_type () const
{
  return m_type;
}


/* Constructor that sets the bit's initial position and its origin.  */

symbolic_bit::symbolic_bit (size_t i, tree orig)
    : value_bit (i), m_origin (orig)
{
  m_type = SYMBOLIC_BIT;
}


/* Constructor that sets m_val to the specified value.  */

bit::bit (unsigned char i) : m_val (i)
{
  m_type = BIT;
}


/* Returns left operand of the expression.  */

value_bit *
bit_expression::get_left ()
{
  return m_left;
}


/* Returns right operand of the expression.  */

value_bit *
bit_expression::get_right ()
{
  return m_right;
}


/* Sets left operand of the expression.  */

void
bit_expression::set_left (value_bit *expr)
{
  m_left = expr;
}


/* Sets right operand of the expression.  */

void
bit_expression::set_right (value_bit *expr)
{
  m_right = expr;
}


/* Returns the bit's initial index in bit-vector.  */

size_t
value_bit::get_index () const
{
  return m_index;
}


/* Returns the value of the bit.  */

unsigned char
bit::get_val () const
{
  return m_val;
}


/* Sets the value of the bit.  */

void
bit::set_val (unsigned char new_val)
{
  m_val = new_val;
}


/* Constructor that sets the left and right side bits
   of the bit_complement_expression sign.  */

bit_complement_expression::bit_complement_expression (value_bit *right)
{
  /* As complement has only one argument, we use only the m_right.  */
  this->m_left = nullptr;
  this->m_right = right;
  m_type = BIT_COMPLEMENT_EXPRESSION;
}


/* Copy constructor for bit_complement_expression.  */

bit_complement_expression::bit_complement_expression (
  const bit_complement_expression &expr)
{
  bit_expression::copy (&expr);
}


/* Destructor for bit_expression.  */

bit_expression::~bit_expression ()
{
  delete m_left;
  m_left = nullptr;
  delete m_right;
  m_right = nullptr;
}


/* Returns a copy of the bit.  */

value_bit *
symbolic_bit::copy () const
{
  return new symbolic_bit (*this);
}


/* Return a copy of the bit.  */

value_bit *
bit::copy () const
{
  return new bit (*this);
}


/* Copies the given expression to it by copying the left and right operands.  */

void
bit_expression::copy (const bit_expression *expr)
{
  if (expr->m_left)
    m_left = expr->m_left->copy ();

  if (expr->m_right)
    m_right = expr->m_right->copy ();

  m_type = expr->m_type;
}


/* Returns a copy of the expression.  */

value_bit *
bit_xor_expression::copy () const
{
  return new bit_xor_expression (*this);
}


/* Returns a copy of the expression.  */

value_bit *
bit_and_expression::copy () const
{
  return new bit_and_expression (*this);
}


/* Returns a copy of the expression.  */

value_bit *
bit_or_expression::copy () const
{
  return new bit_or_expression (*this);
}


/* Returns a copy of the expression.  */

value_bit *
shift_right_expression::copy () const
{
  return new shift_right_expression (*this);
}


/* Returns a copy of the expression.  */

value_bit *
shift_left_expression::copy () const
{
  return new shift_left_expression (*this);
}


/* Returns a copy of the expression.  */

value_bit *
add_expression::copy () const
{
  return new add_expression (*this);
}


/* Returns a copy of the expression.  */

value_bit *
sub_expression::copy () const
{
  return new sub_expression (*this);
}


/* Returns a copy of the expression.  */

value_bit *
bit_complement_expression::copy () const
{
  return new bit_complement_expression (*this);
}


/* Constructor that sets the left and right side bits
   of the bit_xor_expression sign.  */

bit_xor_expression::bit_xor_expression (value_bit *left, value_bit *right)
{
  this->m_left = left;
  this->m_right = right;
  m_type = BIT_XOR_EXPRESSION;
}


/* Copy constructor for bit_xor_expression.  */

bit_xor_expression::bit_xor_expression (const bit_xor_expression &expr)
{
  bit_expression::copy (&expr);
}


/* Constructor that sets the left and right side bits
   of the bit_and_expression sign.  */

bit_and_expression::bit_and_expression (value_bit *left, value_bit *right)
{
  this->m_left = left;
  this->m_right = right;
  m_type = BIT_AND_EXPRESSION;
}


/* Copy constructor for bit_and_expression.  */

bit_and_expression::bit_and_expression (const bit_and_expression &expr)
{
  bit_expression::copy (&expr);
}


/* Constructor that sets the left and right side bits
   of the bit_or_expression sign.  */

bit_or_expression::bit_or_expression (value_bit *left, value_bit *right)
{
  this->m_left = left;
  this->m_right = right;
  m_type = BIT_OR_EXPRESSION;
}


/* Copy constructor for bit_or_expression.  */

bit_or_expression::bit_or_expression (const bit_or_expression &expr)
{
  bit_expression::copy (&expr);
}


/* Constructor that sets the left and right side bits
   of the shift_right_expression sign.  */

shift_right_expression::shift_right_expression (value_bit *left,
						value_bit *right)
{
  this->m_left = left;
  this->m_right = right;
  m_type = SHIFT_RIGHT_EXPRESSION;
}


/* Copy constructor for shift_right_expression.  */

shift_right_expression::shift_right_expression (
  const shift_right_expression &expr)
{
  bit_expression::copy (&expr);
}


/* Constructor that sets the left and right side bits
   of the shift_left_expression sign.  */

shift_left_expression::shift_left_expression (value_bit *left, value_bit *right)
{
  this->m_left = left;
  this->m_right = right;
  m_type = SHIFT_LEFT_EXPRESSION;
}


/* Copy constructor for shift_left_expression.  */

shift_left_expression::shift_left_expression (const shift_left_expression &expr)
{
  bit_expression::copy (&expr);
}


/* Constructor that sets the left and right side bits
   of the add_expression sign.  */

add_expression::add_expression (value_bit *left, value_bit *right)
{
  this->m_left = left;
  this->m_right = right;
  m_type = ADD_EXPRESSION;
}


/* Copy constructor for add_expression.  */

add_expression::add_expression (const add_expression &expr)
{
  bit_expression::copy (&expr);
}


/* Constructor that sets the left and right side bits
   of the sub_expression sign.  */

sub_expression::sub_expression (value_bit *left, value_bit *right)
{
  this->m_left = left;
  this->m_right = right;
  m_type = SUB_EXPRESSION;
}


/* Copy constructor for sub_expression.  */

sub_expression::sub_expression (const sub_expression &expr)
{
  bit_expression::copy (&expr);
}


/* Returns the origin of the bit, to whom it belongs.  */

tree
symbolic_bit::get_origin ()
{
  return m_origin;
}


/* Prints the bit.  */

void
symbolic_bit::print ()
{
  if (dump_file)
    {
      print_generic_expr (dump_file, m_origin, dump_flags);
      fprintf (dump_file, "[%zu]", m_index);
    }
}


/* Prints the bit.  */

void
bit::print ()
{
  if (dump_file)
    fprintf (dump_file, "%u", m_val);
}


/* Depending on the expression, prints its sign.  */

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


/* Prints the expression.  */

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


/* Prints the expression.  */

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
