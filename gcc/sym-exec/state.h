/* State will store states of variables for a function's single execution path.
   It will be used for bit-level symbolic execution to determine values of bits
   of function's return value and symbolic marked arguments.  */


#ifndef SYM_EXEC_STATE_H
#define SYM_EXEC_STATE_H

#define MAX_VALUE_SIZE 64

#include "expression-is-a-helper.h"

struct value {
 private:
  vec<value_bit *> number;

 public:
  const bool is_unsigned;

  value (unsigned size, bool is_unsigned);
  value (const value &other);
  value_bit **push (value_bit *elem);
  size_t length () const;
  value_bit *&last ();
  unsigned allocated () const;
  bool exists () const;
  value_bit *&operator[] (unsigned i);
  value &operator= (const value &other);
  value_bit *operator[] (unsigned i) const;
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

  /* Casts arg to cast_size size, stores value in dest.  */
  bool do_cast (tree arg, tree dest, size_t cast_size);

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
  state () = default;

  ~state ();

  /* Adds an empty state for the given variable.  */
  bool decl_var (tree name, unsigned size);

  state (const state &s);

  /* Adds the given variable to state.  */
  bool add_var_state (tree var, value *state);

  /* Remove all states from the states' vector.  */
  static void remove_states (vec<state *> *states);

  /* Remove all states from the states' vector and release the vector.  */
  static void clear_states (vec<state *> *states);

  void clear_var_states ();

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

  /* Checks if all bits of the given value have constant bit type.  */
  static bool is_bit_vector (const value *var);

  /* Returns the number represented by the value.  */
  static unsigned HOST_WIDE_INT
  make_number (const value *var);

  /* Does bit-level XOR operation for given variables.  */
  bool do_xor (tree arg1, tree arg2, tree dest);

  /* Does bit-level AND operation for given variables.  */
  bool do_and (tree arg1, tree arg2, tree dest);

  /* Does bit-level OR operation for given variables.  */
  bool do_or (tree arg1, tree arg2, tree dest);

  /* Does Assignment.  */
  bool do_assign (tree arg, tree dest);

  /* Assigns pow 2 value.  */
  bool do_assign_pow2 (tree dest, unsigned pow);

  /* Does shift_left operation for given variables.  */
  bool do_shift_left (tree arg1, tree arg2, tree dest);

  /* Does shift_right operation for given variables.  */
  bool do_shift_right (tree arg1, tree arg2, tree dest);

  /* Adds two variables.  */
  bool do_add (tree arg1, tree arg2, tree dest);

  /* Does subtraction.  */
  bool do_sub (tree arg1, tree arg2, tree dest);

  /* Multiplies two variables, stores result in dest.  */
  bool do_mul (tree arg1, tree arg2, tree dest);

  /* Negates given variable.  */
  bool do_complement (tree arg, tree dest);

  /* Adds EQUAL condition of given variables to state.  */
  bool add_equal_cond (tree arg1, tree arg2);

  /* Gets the value of *arg1 and stores it in dest.  */
  bool do_mem_ref (tree arg1, tree dest);

  /* Performs addition on arg1 pointer.  */
  bool do_pointer_plus (tree arg1, tree arg2, tree dest);

  /* Perform subtractions on arg1 pointer.  */
  bool do_pointer_diff (tree arg1, tree arg2, tree dest);

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

  static value_bit *complement_bits_with_origin (value_bit *root, tree origin);

  static void complement_val_bits_with_origin (value *val, tree origin);

  void complement_all_vars_bits_with_origin (tree origin);

  void complement_conditions_with_origin (tree origin);

  void complement_state_with_origin (tree origin);

  /* Returns status of last added condition.  */
  condition_status get_last_cond_status ();

  /* Create LFSR value.  */
  static value *create_lfsr (tree crc, value *polynomial, bool is_bit_forward);
};


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
