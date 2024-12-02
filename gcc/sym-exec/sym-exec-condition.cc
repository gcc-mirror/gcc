/* Everything defined here is used for representing conditions for bits
   and their status.
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

#include "sym-exec-condition.h"

/* Constructor where the first argument is the bit left to the condition sign,
   the second argument is the bit right to the condition sign and the third
   argument is the code of the condition.  */

bit_condition::bit_condition (value_bit *left, value_bit *right, tree_code code)
{
  this->m_left = left;
  this->m_right = right;
  this->m_code = code;
  m_type = BIT_CONDITION;
}


/* Copy constructor.  */

bit_condition::bit_condition (const bit_condition &expr)
{
  bit_expression::copy (&expr);
  m_code = expr.get_code ();
}


/* Returns the condition's code.  */

tree_code
bit_condition::get_code () const
{
  return m_code;
}


/* Returns a copy of the condition.  */

value_bit *
bit_condition::copy () const
{
  return new bit_condition (*this);
}


/* Prints the condition's sign.  */

void
bit_condition::print_expr_sign ()
{
  switch (m_code)
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
