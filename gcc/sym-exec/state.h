/* State will store states of variables for a function's single execution path.
   It will be used for bit-level symbolic execution to determine values of bits
   of function's return value and symbolic marked arguments.  */


#ifndef SYM_EXEC_STATE_H
#define SYM_EXEC_STATE_H

#include "expression-is-a-helper.h"


/* Stores states of variables' values on bit-level.  */

class state {
 typedef void (state::*binary_func) (vec<value*> * arg1_bits,
				     vec<value*> * arg2_bits, tree dest);

 typedef void (state::*binary_cond_func) (vec<value*> * arg1_bits,
					  vec<value*> * arg2_bits);

 private:
  /* Here is stored values of bit of each variable.  */
  hash_map<tree, vec < value * >> var_states;

  /* Here is stored conditions of symbolic bits.  */
  hash_set<bit_expression *> conditions;

  /* The result of last added condition.  */
  condition_status last_cond_status = condition_status::CS_NO_COND;

  /* Creates bit sequence of given constant tree.  */
  vec<value*> create_bits_for_const (tree var, size_t size);

  /* Removes given sequence of bits.  */
  static void free_bits (vec<value*> * bits);

  /* Checks if sizes of arguments and destination are compatible.  */
  bool check_args_compatibility (tree arg1, tree arg2, tree dest);

  /* Adds equality condition for two sequences of bits.  */
  void add_equal_cond (vec<value*> * arg1_bits, vec<value*> * arg2_bits);

  /* Adds not equal condition for two sequences of bits.  */
  void add_not_equal_cond (vec<value*> * arg1_bits, vec<value*> * arg2_bits);

  /* Adds greater than condition for two sequences of bits.  */
  void add_greater_than_cond (vec<value*> * arg1_bits, vec<value*> * arg2_bits);

  /* Adds less than condition for two sequences of bits.  */
  void add_less_than_cond (vec<value*> * arg1_bits, vec<value*> * arg2_bits);

  /* Adds greater or equal condition for two sequences of bits.  */
  void add_greater_or_equal_cond (vec<value*> * arg1_bits,
				  vec<value*> * arg2_bits);

  /* Adds less or equal condition for two sequences of bits.  */
  void add_less_or_equal_cond (vec<value*> * arg1_bits,
			       vec<value*> * arg2_bits);

  /* Does preprocessing and postprocessing for condition adding.
     Handles bit sequence creation for constant values
     and their removement in the end.  */
  bool add_binary_cond (tree arg1, tree arg2, binary_cond_func cond_func);

  /* Constructs expression trees of greater than condition
     for given sequences of bits.  */
  bit_expression* construct_great_than_cond (vec<value*> * arg1_bits,
					     vec<value*> * arg2_bits);

  /* Constructs expression trees of less than condition
     for given sequences of bits.  */
  bit_expression* construct_less_than_cond (vec<value*> * arg1_bits,
					    vec<value*> * arg2_bits);

  /* Constructs expression trees of equal condition
     for given sequences of bits.  */
  bit_expression* construct_equal_cond (vec<value*> * arg1_bits,
					vec<value*> * arg2_bits);

  /* Does preprocessing and postprocessing for expressions with tree operands.
     Handles bit sequence creation for constant values
     and their removement in the end.  */
  bool do_binary_operation (tree arg1, tree arg2, tree dest,
			    binary_func bin_func);

  void do_and (vec<value*> * arg1_bits, vec<value*> * arg2_bits, tree dest);

  void do_or (vec<value*> * arg1_bits, vec<value*> * arg2_bits, tree dest);

  void do_xor (vec<value*> * arg1_bits, vec<value*> * arg2_bits, tree dest);

  void do_shift_right (vec<value*> * arg1_bits, vec<value*> * arg2_bits,
		       tree dest);

  void do_shift_left (vec<value*> * arg1_bits, vec<value*> * arg2_bits,
		      tree dest);

  void do_add (vec<value*> * arg1_bits, vec<value*> * arg2_bits, tree dest);

  void do_sub (vec<value*> * arg1_bits, vec<value*> * arg2_bits, tree dest);

  /* Casts arg_bits to cast_size size, stores value in dest.  */
  bool do_cast (tree arg, tree dest, size_t cast_size);

  /* Performs AND operation on two bits.  */
  static value *and_two_bits (value *arg1_bit, value* arg2_bit);

  /* ANDs every bit of the vector with var_bit, stroes the result in var1.  */
  void and_number_bit (vec<value *> *var1, value *var_bit);

  void do_mul (vec<value*> * arg1_bits, vec<value*> * arg2_bits, tree dest);

  /* Performs AND operation for 2 symbolic_bit operands.  */
  static value *and_sym_bits (const value * var1, const value * var2);

  /* Performs AND operation for a symbolic_bit and const_bit operands.  */
  static value *and_var_const (const value * var1, const bit * const_bit);

  /* Performs AND operation for 2 constant bit operands.  */
  static bit *and_const_bits (const bit * const_bit1, const bit * const_bit2);

  /* Performs OR operation on two bits.  */
  static value *or_two_bits (value *arg1_bit, value* arg2_bit);

  /* Performs OR operation for 2 symbolic_bit operands.  */
  static value *or_sym_bits (const value * var1, const value * var2);

  /* Performs OR operation for a symbolic_bit and a constant bit operands.  */
  static value *or_var_const (const value * var1, const bit * const_bit);

  /* Performs OR operation for 2 constant bit operands.  */
  static bit *or_const_bits (const bit * const_bit1, const bit * const_bit2);

  /* Performs complement operation on a bit.  */
  static value * complement_a_bit (value *var);

  /* Performs NOT operation for constant bit.  */
  static bit *complement_const_bit (const bit * const_bit);

  /* Performs NOT operation for symbolic_bit.  */
  static value *complement_sym_bit (const value * var);

  /* Performs XOR operation on two bits.  */
  static value * xor_two_bits (value *var1, value* var2);

  /* Performs XOR operation for 2 symbolic_bit operands.  */
  static value *xor_sym_bits (const value * var1, const value * var2);

  /* Performs XOR operation for 2 constant bit operands.  */
  static bit *xor_const_bits (const bit * const_bit1, const bit * const_bit2);

  /* Performs XOR operation for a symbolic_bit and const_bit operands.  */
  static value *xor_var_const (const value * var, const bit * const_bit);

  /* shift_right operation.  Case: var2 is a sym_bit.  */
  void shift_right_sym_bits (vec<value*> * arg1_bits, vec<value*> * arg2_bits,
			     tree dest);

  /* shift_left operation.  Case: var2 is a sym_bit.  */
  void shift_left_sym_bits (vec<value*> * arg1_bits, vec<value*> * arg2_bits,
			    tree dest);

  /* Shifts value_vector right by shift_value bits.  */
  vec <value *> shift_right_by_const (const vec <value *> * value_vector,
				      size_t shift_value);

  /* Return node which has a const bit child.  Traversal is done based
     on safe branching.  */
  static bit_expression* get_parent_with_const_child (value* root);

  /* Checks if node is AND, OR or XOR expression.  */
  static bool is_safe_branching (value* node);

  /* Checks whether state for variable with specified name already
     exists or not.  */
  bool is_declared (tree var);

  void declare_if_needed (tree var, size_t size);

 /* Shifts value_vector left by shift_value bits.  */
  vec <value *> *shift_left_by_const (const vec <value *> * number,
				      size_t shift_value);

  /* Checks if all vector elements are const_bit_expressions.  */
  bool is_bit_vector (const vec <value *> *bits);

  /* Adds two bits and carry value.
     Resturn result and stores new carry bit in "carry".  */
  value* full_adder (value* var1, value* var2, value*& carry);

  /* Returns the additive inverse of the number stored in number verctor.  */
  vec <value *> * additive_inverse (const vec <value *> *number);

  /* Adds two vectors, stores the result in the first one.  */
  void add_numbers (vec<value *> *var1, const vec<value *> *var2);

  static vec<value *> * make_copy (vec<value *> *bits);


  /* Create LFSR value for the reversed CRC.  */
  static void
  create_reversed_lfsr (vec<value *> &lfsr, const vec<value *> &crc,
			const vec<value *> &polynomial);

  /* Create LFSR value for the forward CRC.  */
  static void
  create_forward_lfsr (vec<value *> &lfsr, const vec<value *> &crc,
		       const vec<value *> &polynomial);

 public:
   state ();

  ~state ();

  /* Adds an empty state for the given variable.  */
  bool decl_var (tree name, unsigned size);

  state (const state& s);

  bool add_var_state (tree var, vec<value*> * state);

  void clear_states ();

  void clear_conditions ();

  bool add_condition (bit_expression* cond);

  bool bulk_add_conditions (const hash_set<bit_expression*>& conds);

  vec<value*> * get_bits (tree var);

  const hash_set<bit_expression *>& get_conditions ();

  /* Adds a variable with unknown value to state.  Such variables are
     represented as sequence of symbolic bits.  */
  bool make_symbolic (tree var, unsigned size);

  /* Returns size of the given variable.  */
  unsigned get_var_size (tree var);

  static void print_bits (vec<value *> * bits);

  /* returns the value of the number represented as a bit vector.  */
  static unsigned HOST_WIDE_INT get_value (const vec <value *> *bit_vector);

  /* Does bit-level XOR operation for given variables.  */
  bool do_xor (tree arg1, tree arg2, tree dest);

  /* Does bit-level AND operation for given variables.  */
  bool do_and (tree arg1, tree arg2, tree dest);

  /* Does bit-level OR operation for given variables.  */
  bool do_or (tree arg1, tree arg2, tree dest);

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

  bool add_equal_cond (tree arg1, tree arg2);

  /* Gets the value of *arg1 and stores it in dest.  */
  bool do_mem_ref (tree arg1, tree dest);

  /* Performs addition on arg1 pointer.  */
  bool do_pointer_plus (tree arg1, tree arg2, tree dest);

  /* Perform subtractions on arg1 pointer.  */
  bool do_pointer_diff (tree arg1, tree arg2, tree dest);

  bool add_not_equal_cond (tree arg1, tree arg2);

  bool add_greater_than_cond (tree arg1, tree arg2);

  bool add_less_than_cond (tree arg1, tree arg2);

  bool add_greater_or_equal_cond (tree arg1, tree arg2);

  bool add_less_or_equal_cond (tree arg1, tree arg2);

  bool add_bool_cond (tree arg);

  static bool check_const_bit_equality (vec<value *> * arg1_bits,
					vec<value *> * arg2_bits);

  static bool check_const_bit_are_not_equal (vec<value *> * arg1_bits,
					     vec<value *> * arg2_bits);

  static bool check_const_bit_is_greater_than (vec<value *> * arg1_bits,
					       vec<value *> * arg2_bits);

  static bool check_const_bit_is_less_than (vec<value *> * arg1_bits,
					    vec<value *> * arg2_bits);

  /* Returns status of last added condition.  */
  condition_status get_last_cond_status ();

  /* Create LFSR value.  */
  static vec<value *> * create_lfsr (tree crc, vec<value *> *polynomial,
				     bool is_bit_forward);
};

#endif /* SYM_EXEC_STATE_H.  */
