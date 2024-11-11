/* State will store states of variables for a function's single execution path.
   It will be used for bit-level symbolic execution to determine values of bits
   of function's return value and symbolic marked arguments.
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


#ifndef SYM_EXEC_STATE_H
#define SYM_EXEC_STATE_H

#define MAX_VALUE_SIZE 64

#include "sym-exec-expr-is-a-helper.h"

/* Struct used for representing values.  */

struct value {
 private:
  /* bit-vector that represents the value.  */
  vec<value_bit *> number;

 public:
  /* Used for denoting whether the number is unsigned.  */
  const bool is_unsigned;

  /* Constructor for value.  The first argument is the size of the bit-vector
     and the second argument is the sign of the number.  */
  value (unsigned size, bool is_unsigned);

  /* Copy constructor for value.  */
  value (const value &other);

  /* Pushes the given bit to the end of the bit-vector.  */
  value_bit **push (value_bit *elem);

  /* Returns pushed bits count.  */
  unsigned length () const;

  /* Returns a reference the last bit.  */
  value_bit *&last ();

  /* Returns the size in bits.  */
  unsigned allocated () const;

  /* Wrapper of vec<..>.exists for the bit-vector.  */
  bool exists () const;

  /* Wrapper of vec<..>::operator[] for the bit-vector.  */
  value_bit *&operator[] (unsigned i);

  /* Assignment operator.  If the specified value's size is smaller,
     then 0 constant bit will be assigned to the remaining upper bits.  */
  value &operator= (const value &other);

  /* Wrapper of vec<..>::operator[] const for the bit-vector.  */
  value_bit *operator[] (unsigned i) const;

  /* Destructor for value.  */
  ~value ();

  /* Removes given sequence of bits.  */
  void free_bits ();
};


/* Stores states of variables' values on bit-level.  */

class state {
  typedef void (state::*binary_func) (value *arg1, value *arg2, tree dest);
  typedef value_bit *(*bit_func) (value_bit *bit1, value_bit *bit2);
  typedef value_bit *(*bit_func3) (value_bit *var1, value_bit *var2,
				   value_bit **var3);
  typedef void (state::*binary_cond_func) (value *arg1, value *arg2);

 private:

  /* Here is stored values by bits of each variable.  */
  hash_map<tree, value> var_states;

  /* Here is stored conditions of symbolic bits.  */
  hash_set<bit_expression *> conditions;

  /* The result of last added condition.  */
  condition_status last_cond_status = condition_status::CS_NO_COND;

  /* Creates value for given constant tree.  */
  static value create_val_for_const (tree var, size_t size);

  /* Checks if sizes of arguments and destination are compatible.  */
  bool check_args_compatibility (tree arg1, tree arg2, tree dest);

  /* Adds equality condition for two values.  */
  void add_equal_cond (value *arg1, value *arg2);

  /* Adds not equal condition for two values.  */
  void add_not_equal_cond (value *arg1, value *arg2);

  /* Adds greater than condition for two values.  */
  void add_greater_than_cond (value *arg1, value *arg2);

  /* Adds less than condition for two values.  */
  void add_less_than_cond (value *arg1, value *arg2);

  /* Adds greater or equal condition for two values.  */
  void add_greater_or_equal_cond (value *arg1, value *arg2);

  /* Adds less or equal condition for two values.  */
  void add_less_or_equal_cond (value *arg1, value *arg2);

  /* Does preprocessing and postprocessing for condition adding.
     Handles value creation for constants and their removement in the end.  */
  bool add_binary_cond (tree arg1, tree arg2, binary_cond_func cond_func);

  /* Constructs expression trees of greater than condition for given values.  */
  bit_expression *construct_great_than_cond (value *arg1, value *arg2);

  /* Constructs expression trees of less than condition for given values.  */
  bit_expression *construct_less_than_cond (value *arg1, value *arg2);

  /* Constructs expression trees of equal condition for given values.  */
  bit_expression *construct_equal_cond (value *arg1, value *arg2);

  /* A wrapper for operations on two bits.
     Operation and operands are passed as arguments.  */
  static value_bit *operate_bits (bit_func bit_op, value_bit *bit1,
				  value_bit *bit2, value_bit **bit3);

  /* A wrapper for operations on three bits.
     Operation and operands are passed as arguments.  */
  static value_bit *operate_bits (bit_func3 bit_op, value_bit *bit1,
				  value_bit *bit2, value_bit **bit3);

  /* Performs the given operation on passed arguments.
     The result is stored in dest.  */
  template<class func>
  void operate (value *arg1, value *arg2, value_bit **bit_arg, tree dest,
		func bit_op);

  /* Does preprocessing and postprocessing for expressions with tree operands.
     Handles value creation for constant and their removement in the end.  */
  bool do_binary_operation (tree arg1, tree arg2, tree dest,
			    binary_func bin_func);

  /* Performs AND operation on given values.  The result is stored in dest.  */
  void do_and (value *arg1, value *arg2, tree dest);

  /* Performs OR operation on given values.  The result is stored in dest.  */
  void do_or (value *arg1, value *arg2, tree dest);

  /* Performs XOR operation on given values.  The result is stored in dest.  */
  void do_xor (value *arg1, value *arg2, tree dest);

  /* Performs shift right operation on given values.
     The result is stored in dest.  */
  void do_shift_right (value *arg1, value *arg2, tree dest);

  /* Performs shift left operation on given values.
     The result is stored in dest.  */
  void do_shift_left (value *arg1, value *arg2, tree dest);

  /* Adds given values.  The result is stored in dest.  */
  void do_add (value *arg1, value *arg2, tree dest);

  /* Subtracks second value from the first.  The result is stored in dest.  */
  void do_sub (value *arg1, value *arg2, tree dest);

  /* Performs AND operation on two bits.  */
  static value_bit *and_two_bits (value_bit *arg1, value_bit *arg2);

  /* ANDs every bit of the value with var_bit, stroes the result in var1.  */
  void and_number_bit (value *var1, value_bit *var_bit);

  /* Multiplies given values.  The result is stored in dest.  */
  void do_mul (value *arg1, value *arg2, tree dest);

  /* Performs AND operation for 2 symbolic_bit operands.  */
  static value_bit *and_sym_bits (const value_bit *var1,
				  const value_bit *var2);

  /* Performs AND operation for a symbolic_bit and const_bit operands.  */
  static value_bit *and_var_const (const value_bit *var1,
				   const bit *const_bit);

  /* Performs AND operation for 2 constant bit operands.  */
  static bit *and_const_bits (const bit *const_bit1, const bit *const_bit2);

  /* Performs OR operation on two bits.  */
  static value_bit *or_two_bits (value_bit *arg1_bit, value_bit *arg2_bit);

  /* Performs OR operation for 2 symbolic_bit operands.  */
  static value_bit *or_sym_bits (const value_bit *var1,
				 const value_bit *var2);

  /* Performs OR operation for a symbolic_bit and a constant bit operands.  */
  static value_bit *or_var_const (const value_bit *var1,
				  const bit *const_bit);

  /* Performs OR operation for 2 constant bit operands.  */
  static bit *or_const_bits (const bit *const_bit1, const bit *const_bit2);

  /* Performs complement operation on a bit.  */
  static value_bit *complement_a_bit (value_bit *var);

  /* Performs NOT operation for constant bit.  */
  static bit *complement_const_bit (const bit *const_bit);

  /* Performs NOT operation for symbolic_bit.  */
  static value_bit *complement_sym_bit (const value_bit *var);

  /* Performs XOR operation on two bits.  */
  static value_bit *xor_two_bits (value_bit *var1, value_bit *var2);

  /* Performs XOR operation for 2 symbolic_bit operands.  */
  static value_bit *xor_sym_bits (const value_bit *var1,
				  const value_bit *var2);

  /* Performs XOR operation for 2 constant bit operands.  */
  static bit *xor_const_bits (const bit *const_bit1, const bit *const_bit2);

  /* Performs XOR operation for a symbolic_bit and const_bit operands.  */
  static value_bit *xor_var_const (const value_bit *var,
				   const bit *const_bit);

  /* Shift_right operation.  Case: var2 is a symbolic value.  */
  static value_bit *shift_right_sym_bits (value_bit *var1, value_bit *var2);

  /* Shift_left operation.  Case: var2 is a symbolic value.  */
  static value_bit *shift_left_sym_bits (value_bit *var1, value_bit *var2);

  /* Shifts var right by size of shift_value.  */
  value *shift_right_by_const (value *var, size_t shift_value);

  /* Return node which has a const bit child.  Traversal is done based
     on safe branching.  */
  static void get_parent_with_const_child (value_bit *root,
					   bit_expression *&parent,
					   bit_expression *&parent_of_parent);

  /* Checks whether state for variable with specified name already
     exists or not.  */
  bool is_declared (tree var);

  /* Declares given variable if it has not been declared yet.  */
  void declare_if_needed (tree var, size_t size);

  /* Shifts number left by size of shift_value.  */
  value *shift_left_by_const (const value *number, size_t shift_value);

  /* Adds two bits and carry value.
     Resturn result and stores new carry bit in "carry".  */
  static value_bit *full_adder (value_bit *var1, value_bit *var2,
				value_bit **carry);

  /* Returns the additive inverse of the given number.  */
  value *additive_inverse (const value *number);

  /* Adds two values, stores the result in the first one.  */
  void add_numbers (value *var1, const value *var2);

  /* Make a copy of given bits.  */
  static vec<value_bit *> *make_copy (vec<value_bit *> *bits);

  /* Create LFSR value for the reversed CRC.  */
  static void create_reversed_lfsr (value &lfsr, const value &crc,
				    const value &polynomial);

  /* Create LFSR value for the forward CRC.  */
  static void create_forward_lfsr (value &lfsr, const value &crc,
				   const value &polynomial);

 public:
  /* Default constructor for state.  */
  state () = default;

  /* Destructor for state.  */
  ~state ();

  /* Adds an empty state for the given variable.  */
  bool decl_var (tree name, unsigned size);

  /* Copy constructor for state.  It copies all variables and conditions
     of the given state.  */
  state (const state &s);

  /* Adds the given variable to state.  */
  bool add_var_state (tree var, value *state);

  /* Remove all states from the states' vector.  */
  static void remove_states (vec<state *> *states);

  /* Remove all states from the states' vector and release the vector.  */
  static void clear_states (vec<state *> *states);

  /* Removes all variables added to the state.  */
  void clear_var_states ();

  /* Removes all conditions added to the state.  */
  void clear_conditions ();

  /* Adds the given condition to the state.  */
  bool add_condition (bit_expression *cond);

  /* Bulk add the given conditions to the state.  */
  bool bulk_add_conditions (const hash_set<bit_expression *> &conds);

  /* Get value of the given variable.  */
  value *get_value (tree var);

  /* Get the value of the tree, which is in the beginning of the var_states.  */
  value *get_first_value ();

  /* Returns the list of conditions in the state.  */
  const hash_set<bit_expression *> &get_conditions ();

  /* Adds a variable with unknown value to state.  Such variables are
     represented as sequence of symbolic bits.  */
  bool make_symbolic (tree var, unsigned size);

  /* Returns size of the given variable.  */
  unsigned get_var_size (tree var);

  /* Prints the given value.  */
  static void print_value (value *var);

  /* Prints added conditions.  */
  void print_conditions ();

  /* Returns the number represented by the value.  */
  static unsigned HOST_WIDE_INT
  make_number (const value *var);

  /* Checks if all bits of the given value have constant bit type.  */
  static bool is_bit_vector (const value *var);

  /* Performs the specified operation on passed variables.  */
  bool do_operation (tree_code op_code, tree arg1, tree arg2, tree dest);

  /* Does Assignment.  */
  bool do_assign (tree arg, tree dest);

  /* Assigns pow 2 value.  */
  bool do_assign_pow2 (tree dest, unsigned pow);

  /* Negates given variable.  */
  bool do_complement (tree arg, tree dest);

  /* Adds EQUAL condition of given variables to state.  */
  bool add_equal_cond (tree arg1, tree arg2);

  /* Adds NOT EQUAL condition of given variables to state.  */
  bool add_not_equal_cond (tree arg1, tree arg2);

  /* Adds GREATER THAN condition of given variables to state.  */
  bool add_greater_than_cond (tree arg1, tree arg2);

  /* Adds LESS THAN condition of given variables to state.  */
  bool add_less_than_cond (tree arg1, tree arg2);

  /* Adds GREATER OR EQUAL condition of given variables to state.  */
  bool add_greater_or_equal_cond (tree arg1, tree arg2);

  /* Adds LESS OR EQUAL condition of given variables to state.  */
  bool add_less_or_equal_cond (tree arg1, tree arg2);

  /* Adds a bool condition to state.  */
  bool add_bool_cond (tree arg);

  /* Checks whether the given two constant values are equal.  */
  static bool check_const_value_equality (value *arg1, value *arg2);

  /* Checks whether the given two constant values are not equal.  */
  static bool check_const_value_are_not_equal (value *arg1, value *arg2);

  /* Checks whether the first given constant value
     is greater than the second one.  */
  static bool check_const_value_is_greater_than (value *arg1, value *arg2);

  /* Checks whether the first given constant value
     is less than the second one.  */
  static bool check_const_value_is_less_than (value *arg1, value *arg2);

  /* For the given value_bit, iterates over its expression tree, complements
     those bit which came from the given origin.  */
  static value_bit *complement_bits_with_origin (value_bit *root, tree origin);

  /* Iterates over every bit of the given value and complements their
     expression trees' those bits, that came from the given origin.  */
  static void complement_val_bits_with_origin (value *val, tree origin);

  /* Complements all bits of all values that came from the given origin.  */
  void complement_all_vars_bits_with_origin (tree origin);

  /* Complements all bits with the given origin of all added conditions.  */
  void complement_conditions_with_origin (tree origin);

  /* Complements all bits with the given origin of all values
     and added conditions.  */
  void complement_state_with_origin (tree origin);

  /* Returns status of last added condition.  */
  condition_status get_last_cond_status ();

  /* Create LFSR value.  */
  static value *create_lfsr (tree crc, value *polynomial, bool is_bit_forward);
};


/* Returns the minimum of A, B, C.  */

size_t min (size_t a, size_t b, size_t c);


/* Performs the given operation on passed arguments.
   The result is stored in dest.  */

template<class func>
void
state::operate (value *arg1, value *arg2, value_bit **bit_arg, tree dest,
		func bit_op)
{
  value *dest_var = var_states.get (dest);
  size_t min_iter = min (arg1->length (), arg2->length (), dest_var->length ());

  size_t i = 0;
  for (; i < min_iter; i++)
    {
      value_bit *temp = (*var_states.get (dest))[i];
      (*var_states.get (dest))[i] = operate_bits (bit_op, (*arg1)[i],
						  (*arg2)[i], bit_arg);
      delete temp;
    }

  if (i >= dest_var->length ())
    return;

  value *biggest = arg1;
  value_bit *sign_bit = (*arg2)[i - 1];
  if (arg2->length () > arg1->length ())
    {
      biggest = arg2;
      sign_bit = (*arg1)[i - 1];
    }

  min_iter = min (biggest->length (), dest_var->length (), dest_var->length ());
  for (; i < min_iter; i++)
    {
      value_bit *temp = (*var_states.get (dest))[i];
      (*var_states.get (dest))[i] = operate_bits (bit_op, (*biggest)[i],
						  sign_bit, bit_arg);
      delete temp;
    }

  if (i >= dest_var->length ())
    return;

  sign_bit = (*biggest)[i - 1];
  for (; i < dest_var->length (); i++)
    {
      value_bit *temp = (*var_states.get (dest))[i];
      (*var_states.get (dest))[i] = operate_bits (bit_op, sign_bit, sign_bit,
						  bit_arg);
      delete temp;
    }
}

#endif /* SYM_EXEC_STATE_H.  */
