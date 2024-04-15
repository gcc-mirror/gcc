/* State will store states of variables for a function's single execution path.
   It will be used for bit-level symbolic execution to determine values of bits
   of function's return value and symbolic marked arguments.  */

#include "state.h"

size_t min (size_t a, size_t b, size_t c)
{
  size_t min = (a < b ? a : b);
  return min < c ? min : c;
}


state::state (const state &s)
{
  for (auto iter = s.var_states.begin (); iter != s.var_states.end (); ++iter)
    {
      value val ((*iter).second.length (), (*iter).second.is_unsigned);
      for (size_t i = 0; i < (*iter).second.length (); i++)
	val.push ((*iter).second[i]->copy ());

      var_states.put ((*iter).first, val);
    }

  for (auto iter = s.conditions.begin (); iter != s.conditions.end (); ++iter)
    conditions.add (as_a<bit_expression *> ((*iter)->copy ()));
}


state::~state ()
{
  clear_conditions ();
}


/* Checks whether state for variable with specified name already
   exists or not.  */

bool
state::is_declared (tree var)
{
  return var_states.get (var) != NULL;
}


/* Declares given variable if it has not been declared yet.  */

void
state::declare_if_needed (tree var, size_t size)
{
  if (TREE_CODE (var) != INTEGER_CST && !is_declared (var))
    {
      make_symbolic (var, size);
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file,
		   "Declaring var ");
	  print_generic_expr (dump_file, var, dump_flags);
	  fprintf (dump_file,
		   " with size %zd\n", size);
	}
    }
}


/* Get value of the given variable.  */

value *
state::get_value (tree var)
{
  return var_states.get (var);
}


/* Get the value of the tree, which is in the beginning of the var_states.  */

value *
state::get_first_value ()
{
  return &(*(var_states.begin ())).second;
}


/* Returns the list of conditions in the state.  */

const hash_set<bit_expression *> &
state::get_conditions ()
{
  return conditions;
}


/* Checks if sizes of arguments and destination are compatible.  */

bool
state::check_args_compatibility (tree arg1, tree arg2, tree dest)
{
  if (!(get_var_size (arg1) == get_var_size (dest)
	|| TREE_CODE (arg1) == INTEGER_CST)
      || !(get_var_size (arg2) == get_var_size (dest)
	   || TREE_CODE (arg2) == INTEGER_CST))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Sym-Exec: Incompatible destination"
			    "and argument sizes.\n");

      return false;
    }

  return true;
}


/* Creates value for given constant tree.  */

value
state::create_val_for_const (tree var, size_t size)
{
  unsigned HOST_WIDE_INT val = TYPE_UNSIGNED (TREE_TYPE (var))
			       ? tree_to_uhwi (var) : tree_to_shwi (var);
  value result (size, TYPE_UNSIGNED (TREE_TYPE (var)));

  for (size_t i = 0; i < size; i++)
    {
      result.push (new bit (val & 1));
      val >>= 1;
    }

  return result;
}


/* Adds the given variable to state.  */

bool
state::add_var_state (tree var, value *vstate)
{
  size_t size = vstate->length ();
  value val (size, vstate->is_unsigned);
  for (size_t i = 0; i < size; i++)
    val.push ((*vstate)[i]->copy ());

  return var_states.put (var, val);
}


/* Adds the given condition to the state.  */

bool
state::add_condition (bit_expression *cond)
{
  return conditions.add (as_a<bit_expression *> (cond->copy ()));
}


/* Bulk add the given conditions to the state.  */

bool
state::bulk_add_conditions (const hash_set<bit_expression *> &conds)
{
  bool status = true;
  for (auto iter = conds.begin (); iter != conds.end (); ++iter)
    status &= add_condition (*iter);

  return status;
}


/* Remove all states from the states' vector.  */

void
state::remove_states (vec<state *> *states)
{
  while (!states->is_empty ())
    {
      delete states->last ();
      states->pop ();
    }
}


/* Remove all states from the states' vector and release the vector.  */

void
state::clear_states (vec<state *> *states)
{
  remove_states (states);
  states->release ();
}


void
state::clear_var_states ()
{
  var_states.empty ();
}


void
state::clear_conditions ()
{
  for (auto iter = conditions.begin (); iter != conditions.end (); ++iter)
    delete (*iter);
  conditions.empty ();
}


/* Performs AND operation for 2 symbolic_bit operands.  */

value_bit *
state::and_sym_bits (const value_bit *var1, const value_bit *var2)
{
  return new bit_and_expression (var1->copy (), var2->copy ());
}


/* Performs AND operation for a symbolic_bit and const_bit operands.  */

value_bit *
state::and_var_const (const value_bit *var1, const bit *const_bit)
{
  if (const_bit->get_val () == 1)
    return var1->copy ();

  return new bit (0);
}


/* Performs AND operation for 2 constant bit operands.  */

bit *
state::and_const_bits (const bit *const_bit1, const bit *const_bit2)
{
  if (const_bit1->get_val () == const_bit2->get_val ())
    return new bit (*const_bit1);

  return new bit (0);
}


/* Performs OR operation for 2 symbolic_bit operands.  */

value_bit *
state::or_sym_bits (const value_bit *var1, const value_bit *var2)
{
  return new bit_or_expression (var1->copy (), var2->copy ());
}


/* Performs OR operation for a symbolic_bit and a constant bit operands.  */

value_bit *
state::or_var_const (const value_bit *var1, const bit *const_bit)
{
  if (const_bit->get_val () == 0)
    return var1->copy ();

  return new bit (1);
}


/* Performs OR operation for 2 constant bit operands.  */

bit *
state::or_const_bits (const bit *const_bit1, const bit *const_bit2)
{
  if (const_bit1->get_val () == const_bit2->get_val ())
    return new bit (*const_bit1);

  return new bit (1);
}


/* Adds an empty state for the given variable.  */

bool
state::decl_var (tree var, unsigned size)
{
  if (is_declared (var))
    return false;

  value val (size, TYPE_UNSIGNED (TREE_TYPE (var)));
  for (unsigned i = 0; i < size; i++)
    val.push (nullptr);

  return var_states.put (var, val);
}


/* Returns size of the given variable.  */

unsigned
state::get_var_size (tree var)
{
  value *content = var_states.get (var);
  if (content == NULL)
    return 0;

  return content->allocated ();
}


/* Adds a variable with unknown value to state.  Such variables are
   represented as sequence of symbolic bits.  */

bool
state::make_symbolic (tree var, unsigned size)
{
  if (is_declared (var))
    return false;

  value val (size, TYPE_UNSIGNED (TREE_TYPE (var)));
  /* Initialize each bit of a variable with unknown value.  */
  for (size_t i = 0; i < size; i++)
    val.push (new symbolic_bit (i, var));

  return var_states.put (var, val);
}


/* Performs AND operation on two bits.  */

value_bit *
state::and_two_bits (value_bit *arg1, value_bit *arg2)
{
  value_bit *result = nullptr;

  if (is_a<bit *> (arg1) && is_a<bit *> (arg2))
    result = and_const_bits (as_a<bit *> (arg1), as_a<bit *> (arg2));

  else if (is_a<bit *> (arg1) && (is_a<symbolic_bit *> (arg2)
				  || (is_a<bit_expression *> (arg2))))
    result = and_var_const (arg2, as_a<bit *> (arg1));

  else if ((is_a<symbolic_bit *> (arg1)
	    || (is_a<bit_expression *> (arg1))) && is_a<bit *> (arg2))
    result = and_var_const (arg1, as_a<bit *> (arg2));

  else
    result = and_sym_bits (arg1, arg2);

  return result;
}


/* A wrapper for operations on two bits.
   Operation and operands are passed as arguments.  */

value_bit *
state::operate_bits (bit_func bit_op, value_bit *bit1, value_bit *bit2,
		     value_bit **)
{
  return (bit_op) (bit1, bit2);
}


/* A wrapper for operations on three bits.
   Operation and operands are passed as arguments.  */

value_bit *
state::operate_bits (bit_func3 bit_op, value_bit *bit1, value_bit *bit2,
		     value_bit **bit3)
{
  return (bit_op) (bit1, bit2, bit3);
}


/* Does preprocessing and postprocessing for expressions with tree operands.
   Handles value creation for constant and their removement in the end.  */

bool
state::do_binary_operation (tree arg1, tree arg2, tree dest,
			    binary_func bin_func)
{
  declare_if_needed (dest, tree_to_uhwi (TYPE_SIZE (TREE_TYPE (dest))));
  declare_if_needed (arg1, var_states.get (dest)->allocated ());
  declare_if_needed (arg2, var_states.get (dest)->allocated ());

  if (!check_args_compatibility (arg1, arg2, dest))
    return false;

  size_t dest_size = var_states.get (dest)->length ();

  value *arg1_val = var_states.get (arg1);
  value arg1_const_val (dest_size, false);
  if (arg1_val == NULL && TREE_CODE (arg1) == INTEGER_CST)
    {
      arg1_const_val = create_val_for_const (arg1, dest_size);
      arg1_val = &arg1_const_val;
    }

  value *arg2_val = var_states.get (arg2);
  value arg2_const_val (dest_size, false);
  if (arg2_val == NULL && TREE_CODE (arg2) == INTEGER_CST)
    {
      arg2_const_val = create_val_for_const (arg2, dest_size);
      arg2_val = &arg2_const_val;
    }

  (this->*bin_func) (arg1_val, arg2_val, dest);
  print_value (var_states.get (dest));
  return true;
}


/* Does bit-level AND operation for given variables.  */

bool
state::do_and (tree arg1, tree arg2, tree dest)
{
  return do_binary_operation (arg1, arg2, dest, &state::do_and);
}


/* Performs AND operation on given values.  The result is stored in dest.  */

void
state::do_and (value *arg1, value *arg2, tree dest)
{
  /* Creating AND expressions for every bit pair of given arguments
     and store them as a new state for given destination.  */

  operate (arg1, arg2, nullptr, dest, &state::and_two_bits);
}


/* Performs OR operation on two bits.  */

value_bit *
state::or_two_bits (value_bit *arg1_bit, value_bit *arg2_bit)
{
  value_bit *result = nullptr;

  if (is_a<bit *> (arg1_bit) && is_a<bit *> (arg2_bit))
    result = or_const_bits (as_a<bit *> (arg1_bit), as_a<bit *> (arg2_bit));

  else if (is_a<bit *> (arg1_bit) && (is_a<symbolic_bit *> (arg2_bit)
				      || is_a<bit_expression *> (arg2_bit)))
    result = or_var_const (arg2_bit, as_a<bit *> (arg1_bit));

  else if ((is_a<symbolic_bit *> (arg1_bit)
	    || is_a<bit_expression *> (arg1_bit))
	   && is_a<bit *> (arg2_bit))
    result = or_var_const (arg1_bit, as_a<bit *> (arg2_bit));

  else
    result = or_sym_bits (arg1_bit, arg2_bit);

  return result;
}


/* Does bit-level OR operation for given variables.  */

bool
state::do_or (tree arg1, tree arg2, tree dest)
{
  return do_binary_operation (arg1, arg2, dest, &state::do_or);
}


/* Performs OR operation on given values.  The result is stored in dest.  */

void
state::do_or (value *arg1, value *arg2, tree dest)
{
  /* Creating OR expressions for every bit pair of given arguments
     and store them as a new state for given destination.  */
  operate (arg1, arg2, nullptr, dest, &state::or_two_bits);
}


/* Performs XOR operation on two bits.  */

value_bit *
state::xor_two_bits (value_bit *arg1_bit, value_bit *arg2_bit)
{
  value_bit *result = nullptr;

  if (is_a<bit *> (arg1_bit) && is_a<bit *> (arg2_bit))
    result = xor_const_bits (as_a<bit *> (arg1_bit), as_a<bit *> (arg2_bit));

  else if (is_a<bit *> (arg1_bit) && (is_a<symbolic_bit *> (arg2_bit)
				      || is_a<bit_expression *> (arg2_bit)))
    result = xor_var_const (arg2_bit, as_a<bit *> (arg1_bit));

  else if ((is_a<symbolic_bit *> (arg1_bit)
	    || is_a<bit_expression *> (arg1_bit))
	   && is_a<bit *> (arg2_bit))
    result = xor_var_const (arg1_bit, as_a<bit *> (arg2_bit));

  else
    result = xor_sym_bits (arg1_bit, arg2_bit);

  return result;
}


/* Does bit-level XOR operation for given variables.  */

bool
state::do_xor (tree arg1, tree arg2, tree dest)
{
  return do_binary_operation (arg1, arg2, dest, &state::do_xor);
}


/* Performs XOR operation on given values.  The result is stored in dest.  */

void
state::do_xor (value *arg1, value *arg2, tree dest)
{
  operate (arg1, arg2, nullptr, dest, &state::xor_two_bits);
}


/* Shifts value right by size of shift_value.  */

value *
state::shift_right_by_const (value *var, size_t shift_value)
{
  value *shift_result = new value (var->length (), var->is_unsigned);
  if (var->length () <= shift_value)
    for (size_t i = 0; i < var->length (); i++)
      shift_result->push (new bit (0));
  else
    {
      size_t i = 0;
      for (; i < var->length () - shift_value; ++i)
	shift_result->push (((*var)[shift_value + i])->copy ());

      for (; i < var->length (); ++i)
	shift_result->push (var->is_unsigned ? new bit (0)
					     : var->last ()->copy ());
    }
  return shift_result;
}


/* Checks if all bits of the given value have constant bit type.  */

bool
state::is_bit_vector (const value *var)
{
  if (var == nullptr || !var->exists ())
    return false;

  for (size_t i = 0; i < var->length (); i++)
    if (!(is_a<bit *> ((*var)[i])))
      return false;
  return true;
}


/* Returns the number represented by the value.  */

unsigned HOST_WIDE_INT
state::make_number (const value *var)
{
  unsigned HOST_WIDE_INT
  number = 0;
  int value_size = var->length ();
  for (int i = value_size - 1; i >= 0; i--)
    {
      if (is_a<bit *> ((*var)[i]))
	number = (number << 1) | as_a<bit *> ((*var)[i])->get_val ();
      else
	return 0;
    }
  return number;
}


/* Shift_left operation.  Case: var2 is a symbolic value.  */

value_bit *
state::shift_left_sym_bits (value_bit *var1, value_bit *var2)
{
  return new shift_left_expression (var1->copy (), var2->copy ());
}


/* Does shift_left operation for given variables.  */

bool
state::do_shift_left (tree arg1, tree arg2, tree dest)
{
  return do_binary_operation (arg1, arg2, dest, &state::do_shift_left);
}


/* Performs shift left operation on given values.
   The result is stored in dest.  */

void
state::do_shift_left (value *arg1, value *arg2, tree dest)
{
  if (is_bit_vector (arg2))
    {
      size_t shift_value = make_number (arg2);
      value *result = shift_left_by_const (arg1, shift_value);
      for (size_t i = 0; i < get_var_size (dest); i++)
	{
	  delete (*var_states.get (dest))[i];
	  (*var_states.get (dest))[i] = (*result)[i]->copy ();
	}
      delete result;
    }
  else
    operate (arg1, arg2, nullptr, dest, &state::shift_left_sym_bits);
}


/* Does shift_right operation for given variables.  */

bool
state::do_shift_right (tree arg1, tree arg2, tree dest)
{
  return do_binary_operation (arg1, arg2, dest, &state::do_shift_right);
}


/* Performs shift right operation on given values.
   The result is stored in dest.  */

void
state::do_shift_right (value *arg1, value *arg2, tree dest)
{
  if (is_bit_vector (arg2))
    {
      size_t shift_value = make_number (arg2);
      value *result = shift_right_by_const (arg1, shift_value);
      for (size_t i = 0; i < get_var_size (dest); i++)
	{
	  delete (*var_states.get (dest))[i];
	  (*var_states.get (dest))[i] = (*result)[i]->copy ();
	}

      delete result;
    }
  else
    operate (arg1, arg2, nullptr, dest, &state::shift_right_sym_bits);
}


/* Adds two bits and carry value.
   Resturn result and stores new carry bit in "carry".  */

value_bit *
state::full_adder (value_bit *var1, value_bit *var2, value_bit **carry)
{
  value_bit *new_carry = and_two_bits (var1, var2);
  value_bit *sum = xor_two_bits (var1, var2);

  value_bit *result = xor_two_bits (sum, *carry);
  value_bit *sum_and_carry = and_two_bits (sum, *carry);

  delete *carry;
  delete sum;

  *carry = or_two_bits (sum_and_carry, new_carry);

  delete sum_and_carry;
  delete new_carry;

  return result;
}


/* Adds two variables.  */

bool
state::do_add (tree arg1, tree arg2, tree dest)
{
  return do_binary_operation (arg1, arg2, dest, &state::do_add);
}


/* Adds given values.  The result is stored in dest.  */

void
state::do_add (value *arg1, value *arg2, tree dest)
{
  value_bit *carry = new bit (0);
  operate (arg1, arg2, &carry, dest, &state::full_adder);
  delete carry;
}


/* Returns the additive inverse of the given number.  */

value *
state::additive_inverse (const value *number)
{
  value *result = new value (number->length (), number->is_unsigned);
  value one (number->length (), number->is_unsigned);

  size_t size = number->length ();
  one.push (new bit (1));
  result->push (complement_a_bit ((*number)[0]));

  for (size_t i = 1; i < size; i++)
    {
      one.push (new bit (0));
      result->push (complement_a_bit ((*number)[i]));
    }

  value_bit *carry = new bit (0);
  for (size_t i = 0; i < size; ++i)
    {
      value_bit *cur_bit = (*result)[i];
      (*result)[i] = full_adder (cur_bit, one[i], &carry);
      delete cur_bit;
    }

  delete carry;
  return result;
}


/* Does subtraction.  */

bool
state::do_sub (tree arg1, tree arg2, tree dest)
{
  return do_binary_operation (arg1, arg2, dest, &state::do_sub);
}


/* Subtracks second value from the first.  The result is stored in dest.  */

void
state::do_sub (value *arg1, value *arg2, tree dest)
{
  value *neg_arg2 = additive_inverse (arg2);
  do_add (arg1, neg_arg2, dest);
  delete neg_arg2;
}


/* Performs complement operation on a bit.  */

value_bit *
state::complement_a_bit (value_bit *var)
{
  value_bit *result = nullptr;
  if (is_a<bit *> (var))
    result = complement_const_bit (as_a<bit *> (var));
  else
    result = complement_sym_bit (var);

  return result;
}


/* Negates given variable.  */

bool
state::do_complement (tree arg, tree dest)
{
  declare_if_needed (dest, tree_to_uhwi (TYPE_SIZE (TREE_TYPE (dest))));
  declare_if_needed (arg, var_states.get (dest)->allocated ());

  /* Creating complement expressions for every bit the given argument
     and store it as a new state for given destination.  */
  size_t iter_count = min (get_var_size (dest), get_var_size (arg),
			   get_var_size (arg));

  size_t i = 0;
  for (; i < iter_count; i++)
    {
      value_bit *result = complement_a_bit ((*var_states.get (arg))[i]);
      delete (*var_states.get (dest))[i];
      (*var_states.get (dest))[i] = result;
    }

  if (i >= get_var_size (dest))
    {
      print_value (var_states.get (dest));
      return true;
    }

  for (; i < get_var_size (dest); i++)
    {
      delete (*var_states.get (dest))[i];
      bit tmp (0);
      (*var_states.get (dest))[i] = complement_a_bit (&tmp);
    }

  print_value (var_states.get (dest));
  return true;
}


/* Does Assignment.  */

bool
state::do_assign (tree arg, tree dest)
{
  declare_if_needed (dest, tree_to_uhwi (TYPE_SIZE (TREE_TYPE (dest))));
  if (TREE_CODE (arg) != INTEGER_CST)
    declare_if_needed (arg, tree_to_uhwi (TYPE_SIZE (TREE_TYPE (arg))));
  else
    declare_if_needed (arg, var_states.get (dest)->allocated ());

  value *dest_val = var_states.get (dest);

  /* If the argument is already defined, then we must just copy its bits.  */
  if (auto arg_val = var_states.get (arg))
    {
      for (size_t i = 0; i < dest_val->length (); i++)
	{
	  value_bit *new_val = nullptr;
	  if (i < arg_val->length ())
	    new_val = (*arg_val)[i]->copy ();
	  else
	    new_val = new bit (0);

	  delete (*dest_val)[i];
	  (*dest_val)[i] = new_val;
	}
    }
    /* If the argument is a constant, we must save it as sequence of bits.  */
  else if (TREE_CODE (arg) == INTEGER_CST)
    {
      value arg_val
	= create_val_for_const (arg, dest_val->length ());
      for (size_t i = 0; i < dest_val->length (); i++)
	{
	  delete (*dest_val)[i];
	  (*dest_val)[i] = arg_val[i]->copy ();
	}
    }
  else
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Sym-Exec: Unsupported assignment"
			    " for given argument.\n");

      return false;
    }

  print_value (var_states.get (dest));
  return true;
}


/* Assigns pow 2 value.  */

bool
state::do_assign_pow2 (tree dest, unsigned pow)
{
  value *dest_val = var_states.get (dest);
  unsigned dest_size = dest_val ? dest_val->allocated ()
				: tree_to_uhwi (TYPE_SIZE (TREE_TYPE (dest)));
  if (pow > dest_size)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Sym-Exec: pow %u of 2 won't fit in"
			    " specified destination\n", pow);
      return false;
    }

  if (!dest_val)
    {
      decl_var (dest, tree_to_uhwi (TYPE_SIZE (TREE_TYPE (dest))));
      dest_val = var_states.get (dest);
    }
  else
    dest_val->free_bits ();

  for (unsigned i = 0; i < dest_val->length (); i++)
    {
      if (i == pow)
	(*dest_val)[i] = new bit (1);
      else
	(*dest_val)[i] = new bit (0);
    }

  print_value (dest_val);
  return true;
}


/* Performs NOT operation for constant bit.  */

bit *
state::complement_const_bit (const bit *const_bit)
{
  return new bit (1u ^ const_bit->get_val ());
}


/* Performs NOT operation for symbolic_bit.  */

value_bit *
state::complement_sym_bit (const value_bit *var)
{
  return new bit_complement_expression (var->copy ());
}


/* Performs XOR operation for 2 symbolic_bit operands.  */

value_bit *
state::xor_sym_bits (const value_bit *var1, const value_bit *var2)
{
  value_bit *var2_copy = var2->copy ();
  bit_expression *node2_with_const_child = nullptr;
  bit_expression *parent_of_node2_with_child = nullptr;
  get_parent_with_const_child (var2_copy, node2_with_const_child,
			       parent_of_node2_with_child);

  if (node2_with_const_child != nullptr)
    {
      value_bit *var1_copy = var1->copy ();
      bit_expression *node1_with_const_child = nullptr;
      bit_expression *parent_of_node1_with_child = nullptr;
      get_parent_with_const_child (var1_copy, node1_with_const_child,
				   parent_of_node1_with_child);

      /* If both subtrees have constant bit nodes,
	 we can merge them together.  */
      if (node1_with_const_child != nullptr)
	{
	  value_bit *var1_reformed = nullptr;
	  value_bit *var2_reformed = nullptr;

	  /* If var1's const bit is in its left subtree.  */
	  value_bit *var1_left = node1_with_const_child->get_left ();
	  if (var1_left != nullptr && is_a<bit *> (var1_left))
	    {
	      var1_reformed = node1_with_const_child->get_right ()->copy ();
	      value_bit *var2_left = node2_with_const_child->get_left ();

	      /* If var2's const bit is in its left subtree.  */
	      if (var2_left != nullptr && is_a<bit *> (var2_left))
		var2_reformed = node2_with_const_child->get_right ()->copy ();
	      else /* Var2's const bit is in its right subtree.  */
		var2_reformed = node2_with_const_child->get_left ()->copy ();
	    }
	  else /* Var1's const bit is in its right subtree.  */
	    {
	      var1_reformed = node1_with_const_child->get_left ()->copy ();
	      value_bit *var2_left = node2_with_const_child->get_left ();

	      /* If var2's const bit is in its left subtree.  */
	      if (var2_left != nullptr && is_a<bit *> (var2_left))
		var2_reformed = node2_with_const_child->get_right ()->copy ();
	      else /* Var2's const bit is in its right subtree.  */
		var2_reformed = node2_with_const_child->get_left ()->copy ();
	    }

	  if (parent_of_node1_with_child)
	    {
	      parent_of_node1_with_child->get_left ()
	      == node1_with_const_child
	      ? parent_of_node1_with_child->set_left (var1_reformed)
	      : parent_of_node1_with_child->set_right (var1_reformed);
	      delete node1_with_const_child;
	    }
	  else
	    {
	      delete var1_copy;
	      var1_copy = var1_reformed;
	    }

	  if (parent_of_node2_with_child)
	    {
	      parent_of_node2_with_child->get_left ()
	      == node2_with_const_child
	      ? parent_of_node2_with_child->set_left (var2_reformed)
	      : parent_of_node2_with_child->set_right (var2_reformed);
	      delete node2_with_const_child;
	    }
	  else
	    {
	      delete var2_copy;
	      var2_copy = var2_reformed;
	    }

	  return new bit_xor_expression (var1_copy, var2_copy);
	}
      delete var1_copy;
    }

  delete var2_copy;
  return new bit_xor_expression (var1->copy (), var2->copy ());
}


/* Performs XOR operation for 2 constant bit operands.  */

bit *
state::xor_const_bits (const bit *const_bit1, const bit *const_bit2)
{
  return new bit (const_bit1->get_val () ^ const_bit2->get_val ());
}


/* Performs XOR operation for a symbolic_bit and const_bit operands.  */

value_bit *
state::xor_var_const (const value_bit *var, const bit *const_bit)
{
  if (const_bit->get_val () == 0)
    return var->copy ();

  value_bit *var_copy = var->copy ();
  bit_expression *node_with_const_child = nullptr;
  bit_expression *tmp = nullptr;
  get_parent_with_const_child (var_copy, node_with_const_child, tmp);

  if (node_with_const_child != nullptr)
    {
      value_bit *left = node_with_const_child->get_left ();
      if (left != nullptr && is_a<bit *> (left))
	{
	  bit *new_left = xor_const_bits (as_a<bit *> (left), const_bit);
	  delete left;
	  node_with_const_child->set_left (new_left);
	}
      else
	{
	  value_bit *right = node_with_const_child->get_right ();
	  bit *new_right = xor_const_bits (as_a<bit *> (right), const_bit);
	  delete right;
	  node_with_const_child->set_right (new_right);
	}
      return var_copy;
    }

  delete var_copy;
  return new bit_xor_expression (var->copy (), const_bit->copy ());
}


/* Return node which has a const bit child.  Traversal is done based
   on safe branching.  */

void
state::get_parent_with_const_child (value_bit *root, bit_expression *&parent,
				    bit_expression *&parent_of_parent)
{
  parent_of_parent = nullptr;
  parent = nullptr;

  if (!is_a<bit_expression *> (root))
    return;

  bit_expression *expr_root = as_a<bit_expression *> (root);
  hash_set < bit_expression * > nodes_to_consider;
  nodes_to_consider.add (expr_root);

  hash_map < bit_expression * , bit_expression * > node_to_parent;
  node_to_parent.put (expr_root, nullptr);

  /* Traversing expression tree:
     considering only comutative expression nodes.  */
  while (!nodes_to_consider.is_empty ())
    {
      bit_expression *cur_element = *nodes_to_consider.begin ();
      nodes_to_consider.remove (cur_element);

      value_bit *left = cur_element->get_left ();
      value_bit *right = cur_element->get_right ();

      if ((left != nullptr && is_a<bit *> (left))
	  || (right != nullptr && is_a<bit *> (right)))
	{
	  parent = cur_element;
	  parent_of_parent = *node_to_parent.get (cur_element);
	}

      if (left != nullptr && is_a<bit_xor_expression *> (left))
	{
	  nodes_to_consider.add (as_a<bit_expression *> (left));
	  node_to_parent.put (as_a<bit_expression *> (left), cur_element);
	}

      if (right != nullptr && is_a<bit_xor_expression *> (right))
	{
	  nodes_to_consider.add (as_a<bit_expression *> (right));
	  node_to_parent.put (as_a<bit_expression *> (right), cur_element);
	}
    }
}


/* Shifts number left by size of shift_value.  */

value *
state::shift_left_by_const (const value *number, size_t shift_value)
{
  value *shift_result = new value (number->length (), number->is_unsigned);
  if (number->length () <= shift_value)
    for (size_t i = 0; i < number->length (); i++)
      shift_result->push (new bit (0));

  else
    {
      size_t i = 0;
      for (; i < shift_value; ++i)
	shift_result->push (new bit (0));
      for (size_t j = 0; i < number->length (); ++i, j++)
	shift_result->push (((*number)[j])->copy ());
    }
  return shift_result;
}


/* Shift_right operation.  Case: var2 is a symbolic value.  */

value_bit *
state::shift_right_sym_bits (value_bit *var1, value_bit *var2)
{
  return new shift_right_expression (var1->copy (), var2->copy ());
}


/* Adds two values, stores the result in the first one.  */

void
state::add_numbers (value *var1, const value *var2)
{
  value_bit *carry = new bit (0);
  for (unsigned i = 0; i < var1->length (); i++)
    {
      value_bit *temp = (*var1)[i];
      (*var1)[i] = full_adder ((*var1)[i], (*var2)[i], &carry);
      delete temp;
    }
  delete carry;
}


/* ANDs every bit of the vector with var_bit, stroes the result in var1.  */

void
state::and_number_bit (value *var1, value_bit *var_bit)
{
  for (unsigned i = 0; i < var1->length (); i++)
    {
      value_bit *tmp = (*var1)[i];
      (*var1)[i] = and_two_bits ((*var1)[i], var_bit);
      delete tmp;
    }

}


/* Multiplies two variables, stores result in dest.  */

bool
state::do_mul (tree arg1, tree arg2, tree dest)
{
  return do_binary_operation (arg1, arg2, dest, &state::do_mul);
}


void
state::do_mul (value *arg1, value *arg2, tree dest)
{
  value *shifted = new value (*arg1);
  value *dest_val = var_states.get (dest);

  for (unsigned i = 0; i < dest_val->length (); i++)
    {
      delete (*dest_val)[i];
      (*dest_val)[i] = new bit (0);
    }

  for (unsigned i = arg2->length (); i != 0; --i)
    {
      if (is_a<bit *> ((*arg2)[i - 1])
	  && as_a<bit *> ((*arg2)[i - 1])->get_val () != 0)
	add_numbers (dest_val, shifted);
      else if (is_a<symbolic_bit *> ((*arg2)[i - 1]))
	{
	  and_number_bit (shifted, as_a<symbolic_bit *> ((*arg2)[i - 1]));
	  add_numbers (dest_val, shifted);
	}

      value *temp = shifted;
      shifted = shift_left_by_const (shifted, 1);
      delete temp;
    }
  delete shifted;
}


/* Checks whether the given two constant values are equal.  */

bool
state::check_const_value_equality (value *arg1, value *arg2)
{
  for (size_t i = 0; i < arg1->length (); i++)
    if (as_a<bit *> ((*arg1)[i])->get_val ()
	!= as_a<bit *> ((*arg2)[i])->get_val ())
      return false;
  return true;
}


/* Adds EQUAL condition of given variables to state.  */

bool
state::add_equal_cond (tree arg1, tree arg2)
{
  return add_binary_cond (arg1, arg2, &state::add_equal_cond);
}


/* Adds equality condition for two values.  */

void
state::add_equal_cond (value *arg1, value *arg2)
{

  /* If both arguments are constants then we can evaluate it.  */
  if (is_bit_vector (arg1) && is_bit_vector (arg2))
    {
      bool result = check_const_value_equality (arg1, arg2);
      last_cond_status = result ? condition_status::CS_TRUE
				: condition_status::CS_FALSE;
      return;
    }

  /* When some of bits are constants and they differ by value,
     then we can evalate it to be false.  */
  for (size_t i = 0; i < arg1->length (); i++)
    {
      if (is_a<bit *> ((*arg1)[i]) && is_a<bit *> ((*arg2)[i])
	  && as_a<bit *> ((*arg1)[i])->get_val ()
	     != as_a<bit *> ((*arg2)[i])->get_val ())
	{
	  last_cond_status = condition_status::CS_FALSE;
	  return;
	}
    }

  for (size_t i = 0; i < arg1->length (); i++)
    {
      /* If there is a constant bit pair, then they are equal
	 as we checked not equality above.  */
      if (is_a<bit *> ((*arg1)[i]) && is_a<bit *> ((*arg2)[i]))
	continue;

      conditions.add (new bit_condition ((*arg1)[i]->copy (),
					 (*arg2)[i]->copy (),
					 EQ_EXPR));
    }
  last_cond_status = condition_status::CS_SYM;
}


/* Checks whether the given two constant values are not equal.  */

bool
state::check_const_value_are_not_equal (value *arg1, value *arg2)
{
  for (size_t i = 0; i < arg1->length (); i++)
    if (as_a<bit *> ((*arg1)[i])->get_val ()
	!= as_a<bit *> ((*arg2)[i])->get_val ())
      return true;
  return false;
}


/* Adds NOT EQUAL condition of given variables to state.  */

bool
state::add_not_equal_cond (tree arg1, tree arg2)
{
  return add_binary_cond (arg1, arg2, &state::add_not_equal_cond);
}


/* Adds not equal condition for two values.  */

void
state::add_not_equal_cond (value *arg1, value *arg2)
{
  if (is_bit_vector (arg1) && is_bit_vector (arg2))
    {
      bool result = check_const_value_are_not_equal (arg1, arg2);
      last_cond_status = result ? condition_status::CS_TRUE
				: condition_status::CS_FALSE;
      return;
    }

  /* When some of bits are constants and they differ by value,
     then we can evalate it to be true.  */
  for (size_t i = 0; i < arg1->length (); i++)
    {
      if (is_a<bit *> ((*arg1)[i]) && is_a<bit *> ((*arg2)[i])
	  && as_a<bit *> ((*arg1)[i])->get_val ()
	     != as_a<bit *> ((*arg2)[i])->get_val ())
	{
	  last_cond_status = condition_status::CS_TRUE;
	  return;
	}
    }

  bit_expression *prev = nullptr;
  for (size_t i = 0; i < arg1->length (); i++)
    {
      /* If there is a constant bit pair, then they are equal
	 as we checked not equality above.  */
      if (is_a<bit *> ((*arg1)[i]) && is_a<bit *> ((*arg2)[i]))
	continue;

      bit_condition *new_cond = new bit_condition ((*arg1)[i]->copy (),
						   (*arg2)[i]->copy (),
						   NE_EXPR);
      if (prev)
	prev = new bit_or_expression (prev, new_cond);
      else
	prev = new_cond;
    }

  last_cond_status = condition_status::CS_SYM;
  conditions.add (prev);
}


/* Checks whether the first given constant value
   is greater than the second one.  */

bool
state::check_const_value_is_greater_than (value *arg1, value *arg2)
{
  for (int i = arg1->length () - 1; i >= 0; i--)
    {
      if (as_a<bit *> ((*arg1)[i])->get_val ()
	  > as_a<bit *> ((*arg2)[i])->get_val ())
	return true;
      else if (as_a<bit *> ((*arg1)[i])->get_val ()
	       < as_a<bit *> ((*arg2)[i])->get_val ())
	return false;
    }
  return false;
}


/* Adds GREATER THAN condition of given variables to state.  */

bool
state::add_greater_than_cond (tree arg1, tree arg2)
{
  return add_binary_cond (arg1, arg2, &state::add_greater_than_cond);
}


/* Adds greater than condition for two values.  */

void
state::add_greater_than_cond (value *arg1, value *arg2)
{
  if (is_bit_vector (arg1) && is_bit_vector (arg2))
    {
      bool result = check_const_value_is_greater_than (arg1, arg2);
      last_cond_status = result ? condition_status::CS_TRUE
				: condition_status::CS_FALSE;
      return;
    }

  if (is_bit_vector (arg2) && is_a<bit *> (arg1->last ())
      && make_number (arg2) == 0 && !arg1->is_unsigned)
    {
      if (as_a<bit *> (arg1->last ())->get_val () == 1)
	last_cond_status = condition_status::CS_FALSE;
      else
	{
	  for (size_t i = 0; i < arg1->length (); i++)
	    if (is_a<bit *> ((*arg1)[i])
		&& as_a<bit *> ((*arg1)[i])->get_val ())
	      {
		last_cond_status = condition_status::CS_TRUE;
		return;
	      }
	}
    }

  bit_expression *gt_cond = construct_great_than_cond (arg1, arg2);
  if (gt_cond)
    {
      /* Otherwise its status is already set.  */
      last_cond_status = condition_status::CS_SYM;
      conditions.add (gt_cond);
    }
}


/* Constructs expression trees of greater than condition for given values.  */

bit_expression *
state::construct_great_than_cond (value *arg1, value *arg2)
{
  bit_expression *prev = nullptr;
  int i = arg1->length () - 1;
  for (; i >= 0; i--)
    {
      if (is_a<bit *> ((*arg1)[i]) && is_a<bit *> ((*arg2)[i]))
	{
	  if (as_a<bit *> ((*arg1)[i])->get_val ()
	      > as_a<bit *> ((*arg2)[i])->get_val ())
	    {
	      if (!prev)
		last_cond_status = condition_status::CS_TRUE;
	      return prev;
	    }
	  else if (as_a<bit *> ((*arg1)[i])->get_val ()
		   < as_a<bit *> ((*arg2)[i])->get_val ())
	    {
	      if (prev)
		{
		  bit_expression *ret_val
		    = as_a<bit_expression *> (prev->get_left ()->copy ());
		  delete prev;
		  return ret_val;
		}
	      else
		{
		  last_cond_status = condition_status::CS_FALSE;
		  return nullptr;
		}
	    }
	}
      else
	{
	  bit_condition *gt_cond
	    = new bit_condition ((*arg1)[i]->copy (), (*arg2)[i]->copy (),
				 GT_EXPR);
	  bit_expression *expr = nullptr;
	  if (i)
	    {
	      bit_condition *eq_cond
		= new bit_condition ((*arg1)[i]->copy (), (*arg2)[i]->copy (),
				     EQ_EXPR);
	      expr = new bit_or_expression (gt_cond, eq_cond);
	    }
	  else
	    expr = gt_cond;

	  if (prev)
	    prev = new bit_and_expression (expr, prev);
	  else
	    prev = expr;
	}
    }

  return prev;
}


/* Checks whether the first given constant value
   is less than the second one.  */

bool
state::check_const_value_is_less_than (value *arg1, value *arg2)
{
  for (int i = arg1->length () - 1; i >= 0; i--)
    {
      if (as_a<bit *> ((*arg1)[i])->get_val ()
	  < as_a<bit *> ((*arg2)[i])->get_val ())
	return true;
      else if (as_a<bit *> ((*arg1)[i])->get_val ()
	       > as_a<bit *> ((*arg2)[i])->get_val ())
	return false;
    }
  return false;
}


/* Adds LESS THAN condition of given variables to state.  */

bool
state::add_less_than_cond (tree arg1, tree arg2)
{
  return add_binary_cond (arg1, arg2, &state::add_less_than_cond);
}


/* Adds less than condition for two values.  */

void
state::add_less_than_cond (value *arg1, value *arg2)
{
  if (is_bit_vector (arg1) && is_bit_vector (arg2)
      && (make_number (arg2) != 0 || arg1->is_unsigned))
    {
      bool result = check_const_value_is_less_than (arg1, arg2);
      last_cond_status = result ? condition_status::CS_TRUE
				: condition_status::CS_FALSE;
      return;
    }

  last_cond_status = condition_status::CS_SYM;
  if (is_bit_vector (arg2) && make_number (arg2) == 0 && !arg1->is_unsigned)
    {
      if (is_a<bit *> (arg1->last ()))
	{
	  if (as_a<bit *> (arg1->last ())->get_val () == 1)
	    last_cond_status = condition_status::CS_TRUE;
	  else
	    last_cond_status = condition_status::CS_FALSE;
	}
      else
	conditions.add (new bit_condition (arg1->last ()->copy (), new bit (1),
					   EQ_EXPR));

      return;
    }

  bit_expression *lt_cond = construct_less_than_cond (arg1, arg2);
  if (lt_cond)
    /* Otherwise its status is already set.  */
    conditions.add (lt_cond);
}


/* Constructs expression trees of less than condition for given values.  */

bit_expression *
state::construct_less_than_cond (value *arg1, value *arg2)
{
  bit_expression *prev = nullptr;
  int i = arg1->length () - 1;
  for (; i >= 0; i--)
    {
      if (is_a<bit *> ((*arg1)[i]) && is_a<bit *> ((*arg2)[i]))
	{
	  if (as_a<bit *> ((*arg1)[i])->get_val ()
	      < as_a<bit *> ((*arg2)[i])->get_val ())
	    {
	      if (!prev)
		last_cond_status = condition_status::CS_TRUE;
	      return prev;
	    }
	  else if (as_a<bit *> ((*arg1)[i])->get_val ()
		   > as_a<bit *> ((*arg2)[i])->get_val ())
	    {
	      if (prev)
		{
		  bit_expression *ret_val
		    = as_a<bit_expression *> (prev->get_left ()->copy ());
		  delete prev;
		  return ret_val;
		}
	      else
		{
		  last_cond_status = condition_status::CS_FALSE;
		  return nullptr;
		}
	    }
	}
      else
	{
	  bit_condition *lt_cond
	    = new bit_condition ((*arg1)[i]->copy (), (*arg2)[i]->copy (),
				 LT_EXPR);
	  bit_expression *expr = nullptr;
	  if (i)
	    {
	      bit_condition *eq_cond
		= new bit_condition ((*arg1)[i]->copy (), (*arg2)[i]->copy (),
				     EQ_EXPR);
	      expr = new bit_or_expression (lt_cond, eq_cond);
	    }
	  else
	    expr = lt_cond;

	  if (prev)
	    prev = new bit_and_expression (expr, prev);
	  else
	    prev = expr;
	}
    }

  return prev;
}


/* Adds GREATER OR EQUAL condition of given variables to state.  */

bool
state::add_greater_or_equal_cond (tree arg1, tree arg2)
{
  return add_binary_cond (arg1, arg2, &state::add_greater_or_equal_cond);
}


/* Adds greater or equal condition for two values.  */

void
state::add_greater_or_equal_cond (value *arg1, value *arg2)
{
  if (is_bit_vector (arg1) && is_bit_vector (arg2)
      && (make_number (arg2) != 0 || arg1->is_unsigned))
    {
      bool is_greater_than = check_const_value_is_greater_than (arg1,
								arg2);
      bool is_equal = check_const_value_equality (arg1, arg2);
      last_cond_status = (is_greater_than | is_equal)
			 ? condition_status::CS_TRUE
			 : condition_status::CS_FALSE;
      return;
    }

  last_cond_status = condition_status::CS_SYM;
  if (is_bit_vector (arg2) && make_number (arg2) == 0 && !arg1->is_unsigned)
    {
      if (is_a<bit *> (arg1->last ()))
	{
	  if (as_a<bit *> (arg1->last ())->get_val () == 1)
	    last_cond_status = condition_status::CS_FALSE;
	  else
	    last_cond_status = condition_status::CS_TRUE;
	}
      else
	conditions.add (new bit_condition (arg1->last ()->copy (), new bit (0),
					   EQ_EXPR));

      return;
    }

  bit_expression *eq_cond = construct_equal_cond (arg1, arg2);
  if (!eq_cond)
    return;

  bit_expression *gt_cond = construct_great_than_cond (arg1, arg2);
  if (gt_cond)
    /* Otherwise its status is already set.  */
    conditions.add (new bit_or_expression (eq_cond, gt_cond));
}


/* Adds LESS OR EQUAL condition of given variables to state.  */

bool
state::add_less_or_equal_cond (tree arg1, tree arg2)
{
  return add_binary_cond (arg1, arg2, &state::add_less_or_equal_cond);
}


/* Adds less or equal condition for two values.  */

void
state::add_less_or_equal_cond (value *arg1, value *arg2)
{
  if (is_bit_vector (arg1) && is_bit_vector (arg2))
    {
      bool is_less_than = check_const_value_is_less_than (arg1, arg2);
      bool is_equal = check_const_value_equality (arg1, arg2);
      last_cond_status = (is_less_than | is_equal)
			 ? condition_status::CS_TRUE
			 : condition_status::CS_FALSE;
      return;
    }

  last_cond_status = condition_status::CS_SYM;
  bit_expression *eq_cond = construct_equal_cond (arg1, arg2);
  if (!eq_cond)
    return;

  bit_expression *lt_cond = construct_less_than_cond (arg1, arg2);
  if (lt_cond)
    /* Otherwise its status is already set.  */
    conditions.add (new bit_or_expression (eq_cond, lt_cond));
}


/* Adds a bool condition to state.  */

bool
state::add_bool_cond (tree arg)
{
  if (!is_declared (arg))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Sym-Exec: Argument must be declared "
			    "for bool condition.\n");

      return false;
    }

  value *arg_bits = var_states.get (arg);
  for (size_t i = 0; i < arg_bits->length (); i++)
    if (is_a<bit *> ((*arg_bits)[i])
	&& as_a<bit *> ((*arg_bits)[i])->get_val () != 0)
      {
	last_cond_status = condition_status::CS_TRUE;
	print_conditions ();
	return true;
      }

  if (is_bit_vector (arg_bits))
    {
      last_cond_status = condition_status::CS_FALSE;
      print_conditions ();
      return true;
    }

  bit_expression *prev = nullptr;
  for (size_t i = 0; i < arg_bits->length (); i++)
    {
      if (is_a<bit *> ((*arg_bits)[i]))
	continue;

      bit_condition *not_eq_cond
	= new bit_condition ((*arg_bits)[i], new bit (0), NE_EXPR);
      if (prev)
	prev = new bit_or_expression (not_eq_cond, prev);
      else
	prev = not_eq_cond;
    }

  last_cond_status = condition_status::CS_SYM;
  conditions.add (prev);
  print_conditions ();
  return true;
}


/* Does preprocessing and postprocessing for condition adding.
   Handles value creation for constants and their removement in the end.  */

bool
state::add_binary_cond (tree arg1, tree arg2, binary_cond_func cond_func)
{
  bool arg1_is_declared = is_declared (arg1);
  bool arg2_is_declared = is_declared (arg2);

  if (!arg1_is_declared && !arg2_is_declared)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Sym-Exec: At least one of arguments must be"
			    " declared for adding the condition.\n");

      return false;
    }

  if (arg1_is_declared)
    declare_if_needed (arg2, var_states.get (arg1)->length ());

  if (arg2_is_declared)
    declare_if_needed (arg1, var_states.get (arg2)->length ());

  value *arg1_val = var_states.get (arg1);
  value arg1_const_val (MAX_VALUE_SIZE, false);

  if (arg1_val == NULL && TREE_CODE (arg1) == INTEGER_CST)
    {
      arg1_const_val = create_val_for_const (arg1,
					     var_states.get (arg2)->length ());
      arg1_val = &arg1_const_val;
    }

  value *arg2_val = var_states.get (arg2);
  value arg2_const_val (MAX_VALUE_SIZE, false);
  if (arg2_val == NULL && TREE_CODE (arg2) == INTEGER_CST)
    {
      arg2_const_val = create_val_for_const (arg2,
					     var_states.get (arg1)->length ());
      arg2_val = &arg2_const_val;
    }

  (this->*cond_func) (arg1_val, arg2_val);
  print_conditions ();
  return true;
}


/* Constructs expression trees of equal condition for given values.  */

bit_expression *
state::construct_equal_cond (value *arg1, value *arg2)
{
  /* If both arguments are constants then we can evaluate it.  */
  if (is_bit_vector (arg1) && is_bit_vector (arg2))
    {
      bool result = check_const_value_equality (arg1, arg2);
      last_cond_status = result ? condition_status::CS_TRUE
				: condition_status::CS_FALSE;
      return nullptr;
    }

  /* When some bits are constants, and they differ by value,
     then we can evaluate it to be false.  */
  for (size_t i = 0; i < arg1->length (); i++)
    {
      if (is_a<bit *> ((*arg1)[i]) && is_a<bit *> ((*arg2)[i])
	  && as_a<bit *> ((*arg1)[i])->get_val ()
	     != as_a<bit *> ((*arg2)[i])->get_val ())
	{
	  last_cond_status = condition_status::CS_FALSE;
	  return nullptr;
	}
    }

  bit_expression *prev = nullptr;
  for (size_t i = 0; i < arg1->length (); i++)
    {
      bit_condition *eq_expr = new bit_condition ((*arg1)[i]->copy (),
						  (*arg2)[i]->copy (), EQ_EXPR);
      if (prev)
	prev = new bit_and_expression (eq_expr, prev);
      else
	prev = eq_expr;
    }

  return prev;
}


/* Gets the value of *arg1 and stores it in dest.  */

bool
state::do_mem_ref (tree arg1, tree dest)
{
  declare_if_needed (dest, tree_to_uhwi (TYPE_SIZE (TREE_TYPE (dest))));
  if (!is_declared (arg1))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Sym-Exec: For memory reference"
			    " argument must be declared\n");

      return false;
    }

  for (unsigned i = 0; i < get_var_size (dest); i++)
    {
      delete (*var_states.get (dest))[i];
      // TODO: Find a better way.
      (*var_states.get (dest))[i] = new symbolic_bit (i, arg1);
    }

  print_value (var_states.get (dest));
  return true;
}


/* Performs addition on arg1 pointer.  */

bool
state::do_pointer_plus (tree arg1, tree arg2, tree dest)
{
  //TODO: For pointer addition we need to consider size of argument
  return do_add (arg1, arg2, dest);
}


/* Perform subtractions on arg1 pointer.  */

bool
state::do_pointer_diff (tree arg1, tree arg2, tree dest)
{
  //TODO: For pointer subtraction we need to consider size of argument
  return do_sub (arg1, arg2, dest);
}


value::value (unsigned size, bool is_unsigned) : is_unsigned (is_unsigned)
{
  number.create (size);
}


value::value (const value &other) : is_unsigned (other.is_unsigned)
{
  number.create (other.length ());
  for (size_t i = 0; i < other.length (); ++i)
    {
      value_bit *temp = other[i] ? other[i]->copy () : other[i];
      number.quick_push (temp);
    }
}


size_t
value::length () const
{
  return number.length ();
}


value_bit *&
value::operator[] (unsigned i)
{
  return number[i];
}


value &
value::operator= (const value &other)
{
  unsigned smallest = number.allocated () < other.length ()
		      ? number.allocated () : other.length ();

  for (size_t i = 0; i < smallest; i++)
    if (i < number.length ())
      {
	delete number[i];
	number[i] = other[i]->copy ();
      }
    else
      number.quick_push (other[i]->copy ());

  for (size_t i = smallest; i < number.allocated (); i++)
    if (i < number.length ())
      {
	delete number[i];
	number[i] = other.is_unsigned ? new bit (0)
				      : other[other.length () - 1]->copy ();
      }
    else
      number.quick_push (other.is_unsigned
			 ? new bit (0) : other[other.length () - 1]->copy ());

  return (*this);
}


value_bit *
value::operator[] (unsigned i) const
{
  return number[i];
}


bool
value::exists () const
{
  return number.exists ();
}


unsigned
value::allocated () const
{
  return number.allocated ();
}


value_bit *&
value::last ()
{
  return number.last ();
}


/* Make a copy of given bits.  */

vec<value_bit *> *
state::make_copy (vec<value_bit *> *bits)
{
  vec < value_bit * > *copied_bits = new vec<value_bit *> ();
  copied_bits->create (bits->length ());
  for (size_t i = 0; i < bits->length (); i++)
    copied_bits->quick_push ((*bits)[i]->copy ());

  return copied_bits;
}


/* Returns status of last added condition.  */

condition_status
state::get_last_cond_status ()
{
  return last_cond_status;
}


/* Prints the given value.  */

void
state::print_value (value *var)
{
  if (!dump_file || !(dump_flags & TDF_DETAILS))
    return;

  fprintf (dump_file, "{");
  for (int i = var->length () - 1; i >= 0; i--)
    {
      (*var)[i]->print ();

      if (i)
	fprintf (dump_file, ", ");
    }
  fprintf (dump_file, "}\n");
}


/* Casts arg to cast_size size, stores value in dest.  */

bool
state::do_cast (tree var, tree dest, size_t cast_size)
{
  declare_if_needed (dest, tree_to_uhwi (TYPE_SIZE (TREE_TYPE (dest))));
  if (!is_declared (var))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Sym-Exec: For cast operantion "
			    "argument must be declared\n");

      return false;
    }

  value *arg = var_states.get (var);
  value *dest_val = var_states.get (dest);

  size_t arg_size = min (arg->length (), dest_val->length (), cast_size);

  for (size_t i = 0; i < arg_size; i++)
    {
      value_bit *temp = (*dest_val)[i];
      (*dest_val)[i] = (*arg)[i]->copy ();
      delete temp;
    }

  value_bit *sign_bit = arg->is_unsigned
			? new bit (0) : arg->last ()->copy ();
  for (size_t i = arg_size; i < dest_val->length (); i++)
    {
      value_bit *temp = (*dest_val)[i];
      (*dest_val)[i] = sign_bit->copy ();
      delete temp;
    }
  delete sign_bit;
  return true;
}

/* Create LFSR value for the reversed CRC.  */

void
state::create_reversed_lfsr (value &lfsr, const value &crc,
			     const value &polynomial)
{
  size_t size = polynomial.length ();

  /* Determine values of all bits, except MSB.  */
  for (size_t i = 0; i < size - 1; i++)
    {
      if (as_a<bit *> (polynomial[i])->get_val ())
	lfsr.push (state::xor_two_bits (crc[i + 1], crc[0]));
      else
	lfsr.push (crc[i + 1]->copy ());
    }

  /* Determine value of MSB.  */
  if (as_a<bit *> (polynomial[size - 1])->get_val ())
    lfsr.push (crc[0]->copy ());
  else
    lfsr.push (new bit (0));
}


/* Create LFSR value for the forward CRC.  */

void
state::create_forward_lfsr (value &lfsr, const value &crc,
			    const value &polynomial)
{
  size_t size = polynomial.length ();
  /* Determine value of LSB.  */
  if (as_a<bit *> (polynomial[0])->get_val ())
    lfsr.push (crc[size - 1]->copy ());
  else
    lfsr.push (new bit (0));

  /* Determine values of remaining bits.  */
  for (size_t i = 1; i < size; i++)
    {
      if (as_a<bit *> (polynomial[i])->get_val ())
	lfsr.push (state::xor_two_bits (crc[i - 1], crc[size - 1]));
      else
	lfsr.push (crc[i - 1]->copy ());
    }
}


/* Get the last 1 bit index.  */

size_t
last_set_bit (const value &polynomial)
{
  for (size_t i = 0; i < polynomial.length (); ++i)
    {
      if (as_a<bit *> (polynomial[polynomial.length () - i - 1])->get_val ())
	return polynomial.length () - i - 1;
    }
  return 0;
}


/* Create LFSR value.  */

value *
state::create_lfsr (tree crc, value *polynomial, bool is_bit_forward)
{
  /* Check size compatibility․  */
  unsigned HOST_WIDE_INT polynomial_length = polynomial->length ();
  unsigned HOST_WIDE_INT crc_size = tree_to_uhwi (TYPE_SIZE (TREE_TYPE (crc)));
  if (crc_size < polynomial_length)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "LFSR state creation: "
			    "Polynomial doesn't fit into the crc.\n");

      return nullptr;
    }

  /* Get the minimal byte size to keep the polynomial.
 Ie, if the last 1 bit of the polynomial is at 6 index, size will be 8.  */
  size_t required_polynomial_size = ((last_set_bit (*polynomial)/8) + 1) * 8;

  /* Polynomial's length actually equals to the CRC variable's size.
 Now we detect only those CRC calculation algorithms, where leading 1 of the
 polynomial isn't kept.  */
  if (required_polynomial_size == 0
      || required_polynomial_size != polynomial_length)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Polynomial's all bits are zeros "
			    "or the size of the polynomial is uncertain.\n");
      return nullptr;
    }

  /* Create vector of symbolic bits for crc.  */
  value crc_value (polynomial_length, TYPE_UNSIGNED (TREE_TYPE (crc)));

  for (unsigned HOST_WIDE_INT i = 0; i < polynomial_length; i++)
  crc_value.push (new symbolic_bit (i, crc));

  /* create LFSR vector.  */
  value *lfsr = new value (polynomial_length, TYPE_UNSIGNED (TREE_TYPE (crc)));

  /* Calculate values for LFSR.  */
  if (is_bit_forward)
    create_forward_lfsr (*lfsr, crc_value, *polynomial);
  else
    create_reversed_lfsr (*lfsr, crc_value, *polynomial);

  return lfsr;
}


/* Prints added conditions.  */

void
state::print_conditions ()
{
  if (!dump_file || !(dump_flags & TDF_DETAILS))
    return;

  fprintf (dump_file, "Conditions {");
  auto iter = conditions.begin ();
  while (true)
    {
      if (iter != conditions.end ())
	{
	  (*iter)->print ();
	  ++iter;
	}

      if (iter != conditions.end ())
	fprintf (dump_file, ", ");
      else
	break;
    }
  fprintf (dump_file, "}\n");
}


value_bit **
value::push (value_bit *elem)
{
  return number.quick_push (elem);
}


value::~value ()
{
  free_bits ();
  number.release ();
}


/* Removes given sequence of bits.  */

void
value::free_bits ()
{
  if (!number.exists ())
    return;

  for (size_t i = 0; i < number.length (); i++)
    {
      delete number[i];
      number[i] = nullptr;
    }
}


value_bit *
state::complement_bits_with_origin (value_bit *root, tree origin)
{
  /* Be careful.  This function doesn't make a full copy of the bit.  */
  if (!is_a<bit_expression *> (root))
    {
      if (is_a<symbolic_bit *> (root)
	  && as_a<symbolic_bit *> (root)->get_origin () == origin)
	root = new bit_complement_expression (root);

      return root;
    }

  bit_expression *expr_root = as_a<bit_expression *> (root);
  hash_set <value_bit *> nodes_to_consider;
  nodes_to_consider.add (expr_root);
  hash_map <value_bit *, value_bit *> node_to_parent;
  node_to_parent.put (expr_root, nullptr);

  /* Traversing expression tree.  */
  while (!nodes_to_consider.is_empty ())
    {
      value_bit *cur_element = *nodes_to_consider.begin ();
      nodes_to_consider.remove (cur_element);

      if (is_a<symbolic_bit *> (cur_element))
	{
	  if (as_a<symbolic_bit *> (cur_element)->get_origin () != origin)
	    continue;

	  bit_expression *parent
	  = as_a<bit_expression *> (*node_to_parent.get (cur_element));
	  if (is_a<bit_complement_expression *> (parent))
	    {
	      value_bit *parent_of_parent = *node_to_parent.get (parent);
	      if (parent_of_parent)
		{
		  bit_expression *parent_of_parent_expr
		  = as_a<bit_expression *> (parent_of_parent);
		  parent->set_right (nullptr);
		  delete parent;
		  parent_of_parent_expr->get_left () == parent
		    ? parent_of_parent_expr->set_left (cur_element)
		    : parent_of_parent_expr->set_right (cur_element);
		}
	      else
		{
		  /* Parent is our root.  */
		  as_a<bit_expression *> (root)->set_right (nullptr);
		  delete root;
		  root = cur_element;
		}
	    }
	  else
	    {
	      value_bit* new_bit = new bit_complement_expression (cur_element);
	      parent->get_left () == cur_element ? parent->set_left (new_bit)
						 : parent->set_right (new_bit);
	    }
	  continue;
	}

      bit_expression* cur_elem_expr = as_a<bit_expression *> (cur_element);
      value_bit *left = cur_elem_expr->get_left ();
      value_bit *right = cur_elem_expr->get_right ();
      if (left != nullptr && !is_a<bit *> (left))
	{
	  nodes_to_consider.add (left);
	  node_to_parent.put (left, cur_element);
	}

      if (right != nullptr && !is_a<bit *> (right))
	{
	  nodes_to_consider.add (right);
	  node_to_parent.put (right, cur_element);
	}
    }

  return root;
}


void
state::complement_val_bits_with_origin (value *val, tree origin)
{
  for (size_t i = 0; i < val->length (); i++)
    {
      (*val)[i] = complement_bits_with_origin ((*val)[i], origin);
    }
}


void
state::complement_all_vars_bits_with_origin (tree origin)
{
  for (auto iter = var_states.begin (); iter != var_states.end (); ++iter)
    {
      complement_val_bits_with_origin (&(*iter).second, origin);
    }
}


void
state::complement_conditions_with_origin (tree origin)
{
  hash_set<bit_expression *> updated_conditions;
  for (auto iter = conditions.begin (); iter != conditions.end (); ++iter)
    updated_conditions.add (as_a<bit_expression *> (
      complement_bits_with_origin (*iter, origin)));

  conditions.empty ();
  for (auto iter = updated_conditions.begin ();
       iter != updated_conditions.end (); ++iter)
    conditions.add (*iter);
}


void
state::complement_state_with_origin (tree origin)
{
  complement_all_vars_bits_with_origin (origin);
  complement_conditions_with_origin (origin);
}