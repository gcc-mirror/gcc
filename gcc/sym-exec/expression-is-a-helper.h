#ifndef SYM_EXEC_EXPRESSION_IS_A_HELPER_H
#define SYM_EXEC_EXPRESSION_IS_A_HELPER_H

#include "condition.h"

/* Defining test functions for value conversion via dyn_cast.  */

template<>
template<>
inline bool
is_a_helper<symbolic_bit *>::test (value_bit *ptr)
{
  return ptr->get_type () == value_type::SYMBOLIC_BIT;
}


template<>
template<>
inline bool
is_a_helper<bit *>::test (value_bit *ptr)
{
  return ptr->get_type () == value_type::BIT;
}


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


template<>
template<>
inline bool
is_a_helper<bit_and_expression *>::test (value_bit *ptr)
{
  return ptr->get_type () == value_type::BIT_AND_EXPRESSION;
}


template<>
template<>
inline bool
is_a_helper<bit_or_expression *>::test (value_bit *ptr)
{
  return ptr->get_type () == value_type::BIT_OR_EXPRESSION;
}


template<>
template<>
inline bool
is_a_helper<bit_xor_expression *>::test (value_bit *ptr)
{
  return ptr->get_type () == value_type::BIT_XOR_EXPRESSION;
}


template<>
template<>
inline bool
is_a_helper<bit_complement_expression *>::test (value_bit *ptr)
{
  return ptr->get_type () == value_type::BIT_COMPLEMENT_EXPRESSION;
}


template<>
template<>
inline bool
is_a_helper<shift_left_expression *>::test (value_bit *ptr)
{
  return ptr->get_type () == value_type::SHIFT_LEFT_EXPRESSION;
}


template<>
template<>
inline bool
is_a_helper<shift_right_expression *>::test (value_bit *ptr)
{
  return ptr->get_type () == value_type::SHIFT_RIGHT_EXPRESSION;
}


template<>
template<>
inline bool
is_a_helper<add_expression *>::test (value_bit *ptr)
{
  return ptr->get_type () == value_type::ADD_EXPRESSION;
}


template<>
template<>
inline bool
is_a_helper<sub_expression *>::test (value_bit *ptr)
{
  return ptr->get_type () == value_type::SUB_EXPRESSION;
}


template<>
template<>
inline bool
is_a_helper<bit_condition *>::test (value_bit *ptr)
{
  return ptr->get_type () == value_type::BIT_CONDITION;
}


template<>
template<>
inline bool
is_a_helper<bit_and_expression *>::test (bit_expression *ptr)
{
  return ptr->get_type () == value_type::BIT_AND_EXPRESSION;
}


template<>
template<>
inline bool
is_a_helper<bit_or_expression *>::test (bit_expression *ptr)
{
  return ptr->get_type () == value_type::BIT_OR_EXPRESSION;
}


template<>
template<>
inline bool
is_a_helper<bit_xor_expression *>::test (bit_expression *ptr)
{
  return ptr->get_type () == value_type::BIT_XOR_EXPRESSION;
}


template<>
template<>
inline bool
is_a_helper<bit_complement_expression *>::test (bit_expression *ptr)
{
  return ptr->get_type () == value_type::BIT_COMPLEMENT_EXPRESSION;
}


template<>
template<>
inline bool
is_a_helper<shift_left_expression *>::test (bit_expression *ptr)
{
  return ptr->get_type () == value_type::SHIFT_LEFT_EXPRESSION;
}

template<>
template<>
inline bool
is_a_helper<shift_right_expression *>::test (bit_expression *ptr)
{
  return ptr->get_type () == value_type::SHIFT_RIGHT_EXPRESSION;
}


template<>
template<>
inline bool
is_a_helper<add_expression *>::test (bit_expression *ptr)
{
  return ptr->get_type () == value_type::ADD_EXPRESSION;
}


template<>
template<>
inline bool
is_a_helper<sub_expression *>::test (bit_expression *ptr)
{
  return ptr->get_type () == value_type::SUB_EXPRESSION;
}


template<>
template<>
inline bool
is_a_helper<bit_condition *>::test (bit_expression *ptr)
{
  return ptr->get_type () == value_type::BIT_CONDITION;
}

#endif /* SYM_EXEC_EXPRESSION_IS_A_HELPER_H.  */