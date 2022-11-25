/* State will store states of variables for a function's single execution path.
   It will be used for bit-level symbolic execution to determine values of bits
   of function's return value and symbolic marked arguments.  */


#ifndef SYM_EXEC_STATE_H
#define SYM_EXEC_STATE_H

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "tree-ssa-loop-niter.h"
#include "cfgloop.h"
#include "gimple-range.h"
#include "tree-scalar-evolution.h"
#include "hwint.h"
#include "gimple-pretty-print.h"
#include "is-a.h"
#include "vec.h"
#include "hash-map.h"
#include "hash-set.h"
#include "expression.h"
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

  hash_set<bit_expression *> conditions;

  /* Performs AND operation on two bits.  */
  value * and_two_bits (value *var1, value* var2) const;

  vec<value*> create_bits_for_const (tree var, size_t size) const;

  void free_bits (vec<value*> * bits) const;

  bool check_args_compatibility (tree arg1, tree arg2, tree dest);

  void add_equal_cond (vec<value*> * arg1_bits, vec<value*> * arg2_bits);

  void add_not_equal_cond (vec<value*> * arg1_bits, vec<value*> * arg2_bits);

  void add_greater_than_cond (vec<value*> * arg1_bits, vec<value*> * arg2_bits);

  void add_less_than_cond (vec<value*> * arg1_bits, vec<value*> * arg2_bits);

  void add_greater_or_equal_cond (vec<value*> * arg1_bits,
				  vec<value*> * arg2_bits);

  void add_less_or_equal_cond (vec<value*> * arg1_bits,
			       vec<value*> * arg2_bits);

  bool add_binary_cond (tree arg1, tree arg2, binary_cond_func cond_func);

  bit_expression* construct_great_than_cond (vec<value*> * arg1_bits,
					     vec<value*> * arg2_bits);

  bit_expression* construct_less_than_cond (vec<value*> * arg1_bits,
					    vec<value*> * arg2_bits);

  bit_expression* construct_equal_cond (vec<value*> * arg1_bits,
					vec<value*> * arg2_bits);

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

  void do_mul (vec<value*> * arg1_bits, vec<value*> * arg2_bits, tree dest);

  /* Performs AND operation for 2 symbolic_bit operands.  */
  value *and_sym_bits (const value * var1, const value * var2) const;

  /* Performs AND operation for a symbolic_bit and const_bit operands.  */
  value *and_var_const (const value * var1, const bit * const_bit) const;

  /* Performs AND operation for 2 constant bit operands.  */
  bit *and_const_bits (const bit * const_bit1, const bit * const_bit2) const;

  /* Performs OR operation on two bits.  */
  value * or_two_bits (value *var1, value* var2) const;

  /* Performs OR operation for 2 symbolic_bit operands.  */
  value *or_sym_bits (const value * var1, const value * var2) const;

  /* Performs OR operation for a symbolic_bit and a constant bit operands.  */
  value *or_var_const (const value * var1, const bit * const_bit) const;

  /* Performs OR operation for 2 constant bit operands.  */
  bit *or_const_bits (const bit * const_bit1, const bit * const_bit2) const;

  /* Performs complement operation on a bit.  */
  value * complement_a_bit (value *var) const;

  /* Performs NOT operation for constant bit.  */
  bit *complement_const_bit (const bit * const_bit) const;

  /* Performs NOT operation for symbolic_bit.  */
  value *complement_sym_bit (const value * var) const;

  /* Performs XOR operation on two bits.  */
  value * xor_two_bits (value *var1, value* var2) const;

  /* Performs XOR operation for 2 symbolic_bit operands.  */
  value *xor_sym_bits (const value * var1, const value * var2) const;

  /* Performs XOR operation for 2 constant bit operands.  */
  bit *xor_const_bits (const bit * const_bit1, const bit * const_bit2) const;

  /* Performs XOR operation for a symbolic_bit and const_bit operands.  */
  value *xor_var_const (const value * var, const bit * const_bit) const;

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
  bit_expression* get_parent_with_const_child (value* root) const;

  /* Checks if node is AND, OR or XOR expression.  */
  bool is_safe_branching (value* node) const;

  /* Checks whether state for variable with specified name already
     exists or not.  */
  bool is_declared (tree var);

  void declare_if_needed (tree var, size_t size);

 /* Shifts value_vector left by shift_value bits.  */
  vec <value *> *shift_left_by_const (const vec <value *> *arg_vector,
				      size_t shift_value);

  /* Checks if all vector elements are const_bit_expressions.  */
  bool is_bit_vector (const vec <value *> *value_vector);

  /* returns the value of the number represented as a bit vector.  */
  size_t get_value (const vec <value *> *bit_vector);

  /* Adds two bits and carry value.
     Resturn result and stores new carry bit in "carry".  */
  value* full_adder (value* var1, value* var2, value*& carry);

  /* Returns the additive inverse of the number stored in number verctor.  */
  vec <value *> * additive_inverse (const vec <value *> *number);

  /* ANDs every bit of the vector with var_bit, stroes the result in var1.  */
  void and_number_bit (vec<value *> *var1, value *var_bit);

  /* Adds two vectors, stores the result in the first one.  */
  void add_numbers (vec< value * > *var1, const vec< value * > *var2);

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

  /* Does bit-level XOR operation for given variables.  */
  bool do_xor (tree arg1, tree arg2, tree dest);

  /* Does bit-level AND operation for given variables.  */
  bool do_and (tree arg1, tree arg2, tree dest);

  /* Does bit-level OR operation for given variables.  */
  bool do_or (tree arg1, tree arg2, tree dest);

  bool do_assign (tree arg, tree dest);

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

  bool check_const_bit_equality (vec<value *> * arg1_bits,
				 vec<value *> * arg2_bits) const;

  bool check_const_bit_are_not_equal (vec<value *> * arg1_bits,
				      vec<value *> * arg2_bits) const;

  bool check_const_bit_is_greater_than (vec<value *> * arg1_bits,
					vec<value *> * arg2_bits) const;

  bool check_const_bit_is_less_than (vec<value *> * arg1_bits,
				     vec<value *> * arg2_bits) const;
};

#endif /* SYM_EXEC_STATE_H.  */
