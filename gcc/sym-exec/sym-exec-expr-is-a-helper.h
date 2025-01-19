/* Defining test functions for value conversion via dyn_cast.
   Copyright (C) 2022-2025 Free Software Foundation, Inc.
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


#ifndef SYM_EXEC_EXPRESSION_IS_A_HELPER_H
#define SYM_EXEC_EXPRESSION_IS_A_HELPER_H

#include "sym-exec-condition.h"

/* Test function used by dyn_cast checks if the value_bit is of
   the value_type::SYMBOLIC_BIT type.  */

template<>
template<>
inline bool
is_a_helper<symbolic_bit *>::test (value_bit *ptr)
{
  return ptr->get_type () == value_type::SYMBOLIC_BIT;
}


/* Test function used by dyn_cast checks if the value_bit is of
   the value_type::BIT type.  */

template<>
template<>
inline bool
is_a_helper<bit *>::test (value_bit *ptr)
{
  return ptr->get_type () == value_type::BIT;
}


/* Test function used by dyn_cast checks if the value_bit
   is a bit_expression.  */

template<>
template<>
inline bool
is_a_helper<bit_expression *>::test (value_bit *ptr)
{
  value_type type = ptr->get_type ();
  return type == value_type::BIT_AND_EXPRESSION
	 || type == value_type::BIT_OR_EXPRESSION
	 || type == value_type::BIT_XOR_EXPRESSION
	 || type == value_type::BIT_COMPLEMENT_EXPRESSION
	 || type == value_type::SHIFT_RIGHT_EXPRESSION
	 || type == value_type::SHIFT_LEFT_EXPRESSION
	 || type == value_type::ADD_EXPRESSION
	 || type == value_type::SUB_EXPRESSION
	 || type == value_type::BIT_CONDITION;
}


/* Test function used by dyn_cast checks if the value_bit
   is a bit_and_expression.  */

template<>
template<>
inline bool
is_a_helper<bit_and_expression *>::test (value_bit *ptr)
{
  return ptr->get_type () == value_type::BIT_AND_EXPRESSION;
}


/* Test function used by dyn_cast checks if the value_bit
   is a bit_or_expression.  */

template<>
template<>
inline bool
is_a_helper<bit_or_expression *>::test (value_bit *ptr)
{
  return ptr->get_type () == value_type::BIT_OR_EXPRESSION;
}


/* Test function used by dyn_cast checks if the value_bit
   is a bit_xor_expression.  */

template<>
template<>
inline bool
is_a_helper<bit_xor_expression *>::test (value_bit *ptr)
{
  return ptr->get_type () == value_type::BIT_XOR_EXPRESSION;
}


/* Test function used by dyn_cast checks if the value_bit
   is a bit_complement_expression.  */

template<>
template<>
inline bool
is_a_helper<bit_complement_expression *>::test (value_bit *ptr)
{
  return ptr->get_type () == value_type::BIT_COMPLEMENT_EXPRESSION;
}


/* Test function used by dyn_cast checks if the value_bit
   is a shift_left_expression.  */

template<>
template<>
inline bool
is_a_helper<shift_left_expression *>::test (value_bit *ptr)
{
  return ptr->get_type () == value_type::SHIFT_LEFT_EXPRESSION;
}


/* Test function used by dyn_cast checks if the value_bit
   is a shift_right_expression.  */

template<>
template<>
inline bool
is_a_helper<shift_right_expression *>::test (value_bit *ptr)
{
  return ptr->get_type () == value_type::SHIFT_RIGHT_EXPRESSION;
}


/* Test function used by dyn_cast checks if the value_bit
   is an add_expression.  */

template<>
template<>
inline bool
is_a_helper<add_expression *>::test (value_bit *ptr)
{
  return ptr->get_type () == value_type::ADD_EXPRESSION;
}


/* Test function used by dyn_cast checks if the value_bit
   is a sub_expression.  */

template<>
template<>
inline bool
is_a_helper<sub_expression *>::test (value_bit *ptr)
{
  return ptr->get_type () == value_type::SUB_EXPRESSION;
}


/* Test function used by dyn_cast checks if the value_bit
   is a bit_condition.  */

template<>
template<>
inline bool
is_a_helper<bit_condition *>::test (value_bit *ptr)
{
  return ptr->get_type () == value_type::BIT_CONDITION;
}


/* Test function used by dyn_cast checks if the bit_expression
   is a bit_and_expression.  */

template<>
template<>
inline bool
is_a_helper<bit_and_expression *>::test (bit_expression *ptr)
{
  return ptr->get_type () == value_type::BIT_AND_EXPRESSION;
}


/* Test function used by dyn_cast checks if the bit_expression
   is a bit_or_expression.  */

template<>
template<>
inline bool
is_a_helper<bit_or_expression *>::test (bit_expression *ptr)
{
  return ptr->get_type () == value_type::BIT_OR_EXPRESSION;
}


/* Test function used by dyn_cast checks if the bit_expression
   is a bit_xor_expression.  */

template<>
template<>
inline bool
is_a_helper<bit_xor_expression *>::test (bit_expression *ptr)
{
  return ptr->get_type () == value_type::BIT_XOR_EXPRESSION;
}


/* Test function used by dyn_cast checks if the bit_expression
   is a bit_complement_expression.  */

template<>
template<>
inline bool
is_a_helper<bit_complement_expression *>::test (bit_expression *ptr)
{
  return ptr->get_type () == value_type::BIT_COMPLEMENT_EXPRESSION;
}


/* Test function used by dyn_cast checks if the bit_expression
   is a shift_left_expression.  */

template<>
template<>
inline bool
is_a_helper<shift_left_expression *>::test (bit_expression *ptr)
{
  return ptr->get_type () == value_type::SHIFT_LEFT_EXPRESSION;
}


/* Test function used by dyn_cast checks if the bit_expression
   is a shift_right_expression.  */

template<>
template<>
inline bool
is_a_helper<shift_right_expression *>::test (bit_expression *ptr)
{
  return ptr->get_type () == value_type::SHIFT_RIGHT_EXPRESSION;
}


/* Test function used by dyn_cast checks if the bit_expression
   is a add_expression.  */

template<>
template<>
inline bool
is_a_helper<add_expression *>::test (bit_expression *ptr)
{
  return ptr->get_type () == value_type::ADD_EXPRESSION;
}


/* Test function used by dyn_cast checks if the bit_expression
   is a sub_expression.  */

template<>
template<>
inline bool
is_a_helper<sub_expression *>::test (bit_expression *ptr)
{
  return ptr->get_type () == value_type::SUB_EXPRESSION;
}


/* Test function used by dyn_cast checks if the bit_expression
   is a bit_condition_expression.  */

template<>
template<>
inline bool
is_a_helper<bit_condition *>::test (bit_expression *ptr)
{
  return ptr->get_type () == value_type::BIT_CONDITION;
}

#endif /* SYM_EXEC_EXPRESSION_IS_A_HELPER_H.  */
