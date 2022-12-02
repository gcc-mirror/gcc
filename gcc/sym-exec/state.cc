/* State will store states of variables for a function's single execution path.
   It will be used for bit-level symbolic execution to determine values of bits
   of function's return value and symbolic marked arguments.  */

#include "state.h"


state::state ()
{
}


state::state (const state& s)
{
  for (auto iter = s.var_states.begin (); iter != s.var_states.end (); ++iter)
    {
      vec < value * > bits;
      bits.create ((*iter).second.length ());
      for (size_t i = 0; i < (*iter).second.length (); i++)
	bits.quick_push ((*iter).second[i]->copy ());

      var_states.put ((*iter).first, bits);
    }

  for (auto iter = s.conditions.begin (); iter != s.conditions.end (); ++iter)
    conditions.add (as_a<bit_expression *> ((*iter)->copy ()));
}


state::~state ()
{
  clear_states ();
  clear_conditions ();
}


/* Checks whether state for variable with specified name already
   exists or not.  */

bool
state::is_declared (tree var)
{
  return var_states.get (var) != NULL;
}


void
state::declare_if_needed (tree var, size_t size)
{
  if (TREE_CODE (var) != INTEGER_CST && !is_declared (var))
    make_symbolic (var, size);
}


vec<value*> *
state::get_bits (tree var)
{
  return var_states.get (var);
}


const hash_set<bit_expression *>&
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


/* Creates bit sequence of given constant tree.  */

vec<value*>
state::create_bits_for_const (tree var, size_t size) const
{
  HOST_WIDE_INT val = tree_to_shwi (var);
  vec<value *> bits;
  bits.create (size);

  for (size_t i = 0; i < size; i++)
    {
      bits.quick_push (new bit (val & 1));
      val >>= 1;
    }

  return bits;
}


/* Removes given sequence of bits.  */

void
state::free_bits (vec<value*> * bits) const
{
  if (bits == nullptr || !bits->exists ())
    return;

  for (size_t i = 0; i < bits->length (); i++)
    {
      delete (*bits)[i];
      (*bits)[i] = nullptr;
    }
}


bool
state::add_var_state (tree var, vec<value*> * vstate)
{
  vec<value* > bits;
  bits.create (vstate->length ());
  for (size_t i = 0; i < vstate->length (); i++)
    bits.quick_push ((*vstate)[i]->copy ());

  free_bits (var_states.get (var));
  return var_states.put (var, bits);
}


bool
state::add_condition (bit_expression* cond)
{
  return conditions.add (as_a<bit_expression*> (cond->copy ()));
}


bool
state::bulk_add_conditions (const hash_set<bit_expression*>& conds)
{
  bool status = true;
  for (auto iter = conds.begin (); iter != conds.end (); ++iter)
    status &= add_condition (*iter);

  return status;
}


void
state::clear_states ()
{
  for (auto iter = var_states.begin (); iter != var_states.end (); ++iter)
    {
      vec < value * > *bits = &(*iter).second;
      for (size_t i = 0; i < bits->length (); i++)
	delete (*bits)[i];
    }

  var_states.empty ();
}


void
state::clear_conditions ()
{
  for (auto iter = conditions.begin (); iter != conditions.end (); ++iter)
    delete (*iter);
  conditions.empty ();
}


/* performs AND operation for 2 symbolic_bit operands.  */

value *
state::and_sym_bits (const value * var1, const value * var2) const
{
  return new bit_and_expression (var1->copy (), var2->copy ());
}


/* performs AND operation for a symbolic_bit and const_bit operands.  */

value *
state::and_var_const (const value * var1, const bit * const_bit) const
{
  if (const_bit->get_val () == 1)
    return var1->copy ();

  return new bit (0);
}


/* performs AND operation for 2 constant bit operands.  */

bit *
state::and_const_bits (const bit * const_bit1, const bit * const_bit2) const
{
  if (const_bit1->get_val () == const_bit2->get_val ())
    return new bit (*const_bit1);

  return new bit (0);
}


/* performs OR operation for 2 symbolic_bit operands.  */

value *
state::or_sym_bits (const value * var1, const value * var2) const
{
  return new bit_or_expression (var1->copy (), var2->copy ());
}


/* performs OR operation for a symbolic_bit and a constant bit operands.  */

value *
state::or_var_const (const value * var1, const bit * const_bit) const
{
  if (const_bit->get_val () == 0)
    return var1->copy ();

  return new bit (1);
}


/* performs OR operation for 2 constant bit operands.  */

bit *
state::or_const_bits (const bit * const_bit1, const bit * const_bit2) const
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

  vec < value * > content;
  content.create (size);
  for (unsigned i = 0; i < size; i++)
    content.quick_push (nullptr);

  return var_states.put (var, content);
}


/* Returns size of the given variable.  */

unsigned
state::get_var_size (tree var)
{
  vec < value * > *content = var_states.get (var);
  if (content == NULL)
    return 0;

  return content->length ();
}


/* Adds a variable with unknown value to state.  Such variables are
   represented as sequence of symbolic bits.  */

bool
state::make_symbolic (tree var, unsigned size)
{
  if (is_declared (var))
    return false;

  vec < value * > bits;
  bits.create (size);

  /* Initialize each bit of a variable with unknown value.  */
  for (size_t i = 0; i < size; i++)
    bits.quick_push (new symbolic_bit (i, var));

  return var_states.put (var, bits);
}


/* Performs AND operation on two bits.  */

value *
state::and_two_bits (value *arg1_bit, value* arg2_bit) const
{
  value *result = nullptr;

  if (is_a<bit *> (arg1_bit) && is_a<bit *> (arg2_bit))
    result = and_const_bits (as_a<bit *> (arg1_bit), as_a<bit *> (arg2_bit));

  else if (is_a<bit *> (arg1_bit) && (is_a<symbolic_bit *> (arg2_bit)
			|| (is_a<bit_expression *> (arg2_bit))))
    result = and_var_const (arg2_bit, as_a<bit *> (arg1_bit));

  else if ((is_a<symbolic_bit *> (arg1_bit)
	   || (is_a<bit_expression *> (arg1_bit))) && is_a<bit *> (arg2_bit))
    result = and_var_const (arg1_bit, as_a<bit *> (arg2_bit));

  else
    result = and_sym_bits (arg1_bit, arg2_bit);

  return result;
}


/* Does preprocessing and postprocessing for expressions with tree operands.
   Handles bit sequence creation for constant values
   and their removement in the end.  */

bool
state::do_binary_operation (tree arg1, tree arg2, tree dest,
			    binary_func bin_func)
{
  declare_if_needed (dest, tree_to_uhwi (TYPE_SIZE (TREE_TYPE (dest))));
  declare_if_needed (arg1, var_states.get (dest)->allocated ());
  declare_if_needed (arg2, var_states.get (dest)->allocated ());

  if (!check_args_compatibility (arg1, arg2, dest))
    return false;

  vec<value*> * arg1_bits = var_states.get (arg1);
  vec<value*> arg1_const_bits (vNULL);
  if (arg1_bits == NULL && TREE_CODE (arg1) == INTEGER_CST)
    {
      arg1_const_bits = create_bits_for_const (arg1,
				var_states.get (dest)->length ());
      arg1_bits = &arg1_const_bits;
    }

  vec<value*> * arg2_bits = var_states.get (arg2);
  vec<value*> arg2_const_bits (vNULL);
  if (arg2_bits == NULL && TREE_CODE (arg2) == INTEGER_CST)
    {
      arg2_const_bits = create_bits_for_const (arg2,
				var_states.get (dest)->length ());
      arg2_bits = &arg2_const_bits;
    }

  (this->*bin_func)(arg1_bits, arg2_bits, dest);
  free_bits (&arg1_const_bits);
  free_bits (&arg2_const_bits);

  print_bits (var_states.get (dest));
  return true;
}


/* Does bit-level AND operation for given variables.  */

bool
state::do_and (tree arg1, tree arg2, tree dest)
{
  return do_binary_operation (arg1, arg2, dest, &state::do_and);
}


void
state::do_and (vec<value*> * arg1_bits, vec<value*> * arg2_bits, tree dest)
{
  /* Creating AND expressions for every bit pair of given arguments
     and store them as a new state for given destination.  */

  for (size_t i = 0; i < get_var_size (dest); i++)
    {
      value* temp = (*var_states.get (dest))[i];
      (*var_states.get (dest))[i] = and_two_bits ((*arg1_bits)[i],
						  (*arg2_bits)[i]);
      delete temp;
    }
}


/* Performs OR operation on two bits.  */

value *
state::or_two_bits (value *arg1_bit, value* arg2_bit) const
{
  value *result = nullptr;

  if (is_a<bit *> (arg1_bit) && is_a<bit *> (arg2_bit))
    result = or_const_bits (as_a<bit *> (arg1_bit), as_a<bit *> (arg2_bit));

  else if (is_a<bit *> (arg1_bit) && (is_a<symbolic_bit *> (arg2_bit)
  				|| (is_a<bit_expression *> (arg2_bit))))
    result = or_var_const (arg2_bit, as_a<bit *> (arg1_bit));

  else if ((is_a<symbolic_bit *> (arg1_bit)
	     || (is_a<bit_expression *> (arg1_bit))) && is_a<bit *> (arg2_bit))
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


void
state::do_or (vec<value*> * arg1_bits, vec<value*> * arg2_bits, tree dest)
{
  /* Creating OR expressions for every bit pair of given arguments
     and store them as a new state for given destination.  */
  for (size_t i = 0; i < get_var_size (dest); i++)
    {
      value * temp = (*var_states.get (dest))[i];
      (*var_states.get (dest))[i] = or_two_bits ((*arg1_bits)[i],
						 (*arg2_bits)[i]);
      delete temp;
    }
}


/* Performs XOR operation on two bits.  */

value *
state::xor_two_bits (value *arg1_bit, value* arg2_bit) const
{
  value *result = nullptr;

  if (is_a<bit *> (arg1_bit) && is_a<bit *> (arg2_bit))
    result = xor_const_bits (as_a<bit *> (arg1_bit), as_a<bit *> (arg2_bit));

   else if (is_a<bit *> (arg1_bit) && (is_a<symbolic_bit *> (arg2_bit)
			  	|| (is_a<bit_expression *> (arg2_bit))))
    result = xor_var_const (arg2_bit, as_a<bit *> (arg1_bit));

   else if ((is_a<symbolic_bit *> (arg1_bit)
	     || (is_a<bit_expression *> (arg1_bit))) && is_a<bit *> (arg2_bit))
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


void
state::do_xor (vec<value*> * arg1_bits, vec<value *> * arg2_bits, tree dest)
{
  for (size_t i = 0; i < get_var_size (dest); i++)
    {
      value * temp = (*var_states.get (dest))[i];
      (*var_states.get (dest))[i] = xor_two_bits ((*arg1_bits)[i],
						  (*arg2_bits)[i]);
      delete temp;
    }
}


/* Shifts value_vector right by shift_value bits.  */

vec <value *>
state::shift_right_by_const (const vec <value *> * number,
			     size_t shift_value)
{
  vec <value *> shift_result;
  shift_result.create (number->length ());
  if (number->length () <= shift_value)
    for (size_t i = 0; i < number->length (); i++)
      shift_result.quick_push (new bit (0));
  else
    {
      size_t i = 0;
      for (; i < number->length () - shift_value; ++i)
	shift_result.quick_push (((*number)[shift_value + i])->copy ());

      for (; i < number->length (); ++i)
	shift_result.quick_push (new bit (0));
    }
  return shift_result;
}


/* Checks if all vector elements are const_bit_expressions.  */

bool
state::is_bit_vector (const vec <value *>* bits)
{
  if (bits == nullptr)
    return false;

  for (size_t i = 0; i < bits->length (); i++)
    if (!(is_a <bit *> ((*bits)[i])))
	return false;
  return true;
}


/* Returns the value of the number represented as a bit vector.  */

unsigned HOST_WIDE_INT
state::get_value (const vec <value *> * bits_value)
{
  unsigned HOST_WIDE_INT number = 0;
  if (bits_value->length () <= sizeof (size_t))
    for (unsigned i = 0; i < bits_value->length (); i++)
      {
	if (is_a<bit *> ((*bits_value)[i]))
	  number = (number | as_a<bit*> ((*bits_value)[i])->get_val ()) << 1;
	else
	  return 0;
      }
  return number;
}


/* shift_left operation.  Case: var2 is a sym_bit.  */

void
state::shift_left_sym_bits (vec<value*> * arg1_bits, vec<value*> * arg2_bits,
			    tree dest)
{
  for (size_t i = 0; i < get_var_size (dest); i++)
    {
      value *var1_elem = (*arg1_bits)[i];
      value *var2_elem = (*arg2_bits)[i];
      value *new_elem = new shift_left_expression (var1_elem->copy (),
						   var2_elem->copy ());
      delete (*var_states.get (dest))[i];
      (*var_states.get (dest))[i] = new_elem;
    }
}


/* Does shift_left operation for given variables.  */

bool
state::do_shift_left (tree arg1, tree arg2, tree dest)
{
  return do_binary_operation (arg1, arg2, dest, &state::do_shift_left);
}


void
state::do_shift_left (vec<value*> * arg1_bits, vec<value*> * arg2_bits,
		      tree dest)
{
  if (is_bit_vector (arg2_bits))
    {
      size_t shift_value = get_value (arg2_bits);
      vec <value *> * result = shift_left_by_const (arg1_bits, shift_value);
      for (size_t i = 0; i < get_var_size (dest); i++)
	{
	  delete (*var_states.get (dest))[i];
	  (*var_states.get (dest))[i] = (*result)[i];
	}
      delete result;
    }
  else
    shift_left_sym_bits (arg1_bits, arg2_bits, dest);
}



/* Does shift_right operation for given variables.  */

bool
state::do_shift_right (tree arg1, tree arg2, tree dest)
{
  return do_binary_operation (arg1, arg2, dest, &state::do_shift_right);
}


void
state::do_shift_right (vec<value*> * arg1_bits, vec<value*> * arg2_bits,
		       tree dest)
{
  /* TODO: Add support for signed var shift.  */
  if (is_bit_vector (arg2_bits))
    {
      size_t shift_value = get_value (arg2_bits);
      vec < value * > result = shift_right_by_const (arg1_bits, shift_value);
      for (size_t i = 0; i < get_var_size (dest); i++)
	{
	  delete (*var_states.get (dest))[i];
	  (*var_states.get (dest))[i] = result[i];
	}
    }
  else
    shift_right_sym_bits (arg1_bits, arg2_bits, dest);
}


/* Adds two bits and carry value.
Resturn result and stores new carry bit in "carry".  */

value*
state::full_adder (value* var1, value* var2, value*& carry)
{
  value * new_carry = and_two_bits (var1, var2);
  value * sum = xor_two_bits (var1, var2);

  value* result = xor_two_bits (sum, carry);
  value * sum_and_carry = and_two_bits (sum, carry);

  delete carry;
  delete sum;

  carry = or_two_bits (sum_and_carry, new_carry);

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


void
state::do_add (vec<value*> * arg1_bits, vec<value*> * arg2_bits, tree dest)
{
  value * carry = new bit (0);
  for (size_t i = 0; i < get_var_size (dest); ++i)
    {
      value * temp = (*var_states.get (dest))[i];
      (*var_states.get (dest))[i] = full_adder ((*arg1_bits)[i],
						(*arg2_bits)[i],
						carry);
      delete temp;
    }
  delete carry;
}


/* Returns the additive inverse of the number stored in number verctor.  */

vec < value * > *
state::additive_inverse (const vec <value *> *number)
{
  vec <value *> * result = new vec <value *> ();
  vec <value *> * one = new vec <value *> ();

  result->create (number->length ());
  one->create (number -> length ());

  size_t vec_len = number->length ();
  one->quick_push (new bit (1));
  result->quick_push (complement_a_bit ((*number)[0]->copy ()));

  for (size_t i = 1; i < vec_len; i++)
    {
      one->quick_push (new bit (0));
      result->quick_push (complement_a_bit ((*number)[i]->copy ()));
    }

  value * carry = new bit (0);
  for (size_t i = 0; i < vec_len; ++i)
    (*result)[i] = full_adder ((*result)[i], (*one)[i], carry);

  delete carry;
  return result;
}


/* Does subtraction.  */

bool
state::do_sub (tree arg1, tree arg2, tree dest)
{
  return do_binary_operation (arg1, arg2, dest, &state::do_sub);
}


void
state::do_sub (vec<value*> * arg1_bits, vec<value*> * arg2_bits, tree dest)
{
  vec < value * > * neg_arg2 = additive_inverse (arg2_bits);
  do_add (arg1_bits, neg_arg2, dest);

  free_bits (neg_arg2);
  delete neg_arg2;
}


/* Performs complement operation on a bit.  */

value *
state::complement_a_bit (value *var) const
{
  value *result = nullptr;
  bit* const_bit = dyn_cast<bit *> (var);
  if (const_bit)
    result = complement_const_bit (const_bit);
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

  if (get_var_size (arg) != get_var_size (dest))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Sym-Exec: Incompatible destination"
			    " and argument sizes.\n");

      return false;
    }

  /* Creating complement expressions for every bit the given argument
     and store it as a new state for given destination.  */
  for (size_t i = 0; i < get_var_size (dest); i++)
    {
      value *result = complement_a_bit ((*var_states.get (arg))[i]);

      delete (*var_states.get (dest))[i];
      (*var_states.get (dest))[i] = result;
    }

  print_bits (var_states.get (dest));
  return true;
}


bool
state::do_assign (tree arg, tree dest)
{
  declare_if_needed (dest, tree_to_uhwi (TYPE_SIZE (TREE_TYPE (dest))));
  declare_if_needed (arg, var_states.get (dest)->allocated ());
  vec<value*> * dest_bits = var_states.get (dest);

  /* If the argument is already defined, then we must just copy its bits.  */
  if (auto bits = var_states.get (arg))
    {
      for (size_t i = 0; i < dest_bits->length (); i++)
	{
	  value *new_val = nullptr;
	  if (i < bits->length ())
	    new_val = (*bits)[i]->copy ();
	  else
	    new_val = new bit (0);

	  delete (*dest_bits)[i];
	  (*dest_bits)[i] = new_val;
	}
    }
  /* If the argument is a constant, we must save it as sequence of bits.  */
  else if (TREE_CODE (arg) == INTEGER_CST)
    {
      vec <value *> arg_bits
      = create_bits_for_const (arg, dest_bits->length ());
      for (size_t i = 0; i < dest_bits->length (); i++)
	{
	  delete (*dest_bits)[i];
	  (*dest_bits)[i] = arg_bits[i];
	}
    }
  else
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Sym-Exec: Unsupported assignment"
			    " for given argument.\n");

      return false;
    }

  print_bits (var_states.get (dest));
  return true;
}


/* Assigns pow 2 value.  */

bool
state::do_assign_pow2 (tree dest, unsigned pow)
{
  vec<value *> * dest_bits = var_states.get (dest);
  unsigned dest_size = dest_bits ? dest_bits->allocated ()
				 : tree_to_uhwi (TYPE_SIZE (TREE_TYPE (dest)));
  if (pow > dest_size)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Sym-Exec: pow %u of 2 won't fit in"
			    " specified destination\n", pow);
      return false;
    }

  if (!dest_bits)
    {
      decl_var (dest, tree_to_uhwi (TYPE_SIZE (TREE_TYPE (dest))));
      dest_bits = var_states.get (dest);
    }
  else
    free_bits (dest_bits);

  for (unsigned i = 0; i < dest_bits->length (); i++)
    {
      if (i == pow)
	(*dest_bits)[i] = new bit (1);
      else
	(*dest_bits)[i] = new bit (0);
    }

  return true;
}


/* Performs NOT operation for constant bit.  */

bit *
state::complement_const_bit (const bit * const_bit) const
{
  return new bit (1u ^ const_bit->get_val ());
}


/* Performs NOT operation for symbolic_bit.  */

value *
state::complement_sym_bit (const value * var) const
{
  return new bit_complement_expression (var->copy ());
}


/* Performs XOR operation for 2 symbolic_bit operands.  */

value *
state::xor_sym_bits (const value * var1, const value * var2) const
{
  value * var2_copy = var2->copy ();
  bit_expression * var2_node_with_const_child
      = get_parent_with_const_child (var2_copy);

  if (var2_node_with_const_child != nullptr)
    {
      value *var1_copy = var1->copy ();
      bit_expression *var1_node_with_const_child
	= get_parent_with_const_child (var1_copy);

      /* If both subtrees have constant bit nodes,
	 we can merge them together.  */
      if (var1_node_with_const_child != nullptr)
	{
	  /* If var1's const bit is in its left subtree.  */
	  value *var1_left = var1_node_with_const_child->get_left ();
	  if (var1_left != nullptr && is_a<bit *> (var1_left))
	    {
	      /* If var2's const bit is in its left subtree.  */
	      value *var2_left = var2_node_with_const_child->get_left ();
	      if (var2_left != nullptr && is_a<bit *> (var2_left))
		{
		  bit *new_left = xor_const_bits (as_a<bit *> (var1_left),
						  as_a<bit *> (var2_left));
		  delete var2_left;
		  var2_node_with_const_child->set_left (nullptr);

		  delete var1_left;
		  var1_node_with_const_child->set_left (new_left);
		}
	      else /* Var2's const bit is in its right subtree.  */
		{
		  value *var2_right = var2_node_with_const_child->get_right ();
		  bit *new_left = xor_const_bits (as_a<bit *> (var1_left),
						  as_a<bit *> (var2_right));
		  delete var2_right;
		  var2_node_with_const_child->set_right (nullptr);

		  delete var1_left;
		  var1_node_with_const_child->set_left (new_left);
		}
	    }
	  else /* Var1's const bit is in its right subtree.  */
	    {
	      value *var1_right = var1_node_with_const_child->get_right ();
	      value *var2_left = var2_node_with_const_child->get_left ();
	      /* If var2's const bit is in its left subtree.  */
	      if (var2_left != nullptr && is_a<bit *> (var2_left))
		{
		  bit *new_right = xor_const_bits (as_a<bit *> (var1_left),
						   as_a<bit *> (var2_left));
		  delete var2_left;
		  var2_node_with_const_child->set_left (nullptr);

		  delete var1_right;
		  var1_node_with_const_child->set_right (new_right);
		}
	      else /* Var2's const bit is in its right subtree.  */
		{
		  value *var2_right = var2_node_with_const_child->get_right ();
		  bit *new_right = xor_const_bits (as_a<bit *> (var1_right),
						   as_a<bit *> (var2_right));
		  delete var2_right;
		  var2_node_with_const_child->set_right (nullptr);

		  delete var1_right;
		  var1_node_with_const_child->set_right (new_right);
		}
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
state::xor_const_bits (const bit * const_bit1, const bit * const_bit2) const
{
  return new bit (const_bit1->get_val () ^ const_bit2->get_val ());
}


/* Performs XOR operation for a symbolic_bit and const_bit operands.  */

value *
state::xor_var_const (const value * var, const bit * const_bit) const
{
  value *var_copy = var->copy ();
  bit_expression *node_with_const_child
    = get_parent_with_const_child (var_copy);
  if (node_with_const_child != nullptr)
    {
      value *left = node_with_const_child->get_left ();
      if (left != nullptr && is_a<bit *> (left))
	{
	  bit *new_left = xor_const_bits (as_a<bit *> (left), const_bit);
	  delete left;
	  node_with_const_child->set_left (new_left);
	}
      else
	{
	  value *right = node_with_const_child->get_right ();
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

bit_expression *
state::get_parent_with_const_child (value* root) const
{
  if (! is_a<bit_expression *> (root))
    return nullptr;

  bit_expression* expr_root = as_a<bit_expression *> (root->copy ());
  hash_set<bit_expression *> nodes_to_consider;
  nodes_to_consider.add (expr_root);

  /* Traversing expression tree:
     considering only comutative expression nodes.  */
  while (!nodes_to_consider.is_empty ())
    {
      bit_expression *cur_element = *nodes_to_consider.begin ();
      nodes_to_consider.remove (cur_element);

      value *left = cur_element->get_left ();
      value *right = cur_element->get_right ();

      if ((left != nullptr && is_a<bit *> (left))
	  || (right != nullptr && is_a<bit *> (right)))
	return cur_element;

      if (left != nullptr && is_safe_branching (left))
	nodes_to_consider.add (as_a<bit_expression *> (left));

      if (right != nullptr && is_safe_branching (right))
	nodes_to_consider.add (as_a<bit_expression *> (right));
    }
  return nullptr;
}


/* Checks if node is AND, OR or XOR expression as they are comutative.  */

bool
state::is_safe_branching (value* node) const
{
  return is_a<bit_and_expression *> (node) || is_a<bit_or_expression *> (node)
	 || is_a<bit_xor_expression *> (node);
}


/* Shifts number left by shift_value bits.  */

vec <value *> *
state::shift_left_by_const (const vec <value *> * number,
			    size_t shift_value)
{
  vec <value *> *shift_result = new vec< value * > ();
  shift_result->create (number->length ());

  if (number->length () <= shift_value)
    for (size_t i = 0; i < number->length (); i++)
      shift_result->quick_push (new bit (0));
  else
    {
      size_t i = 0;
      for ( ; i < number->length () - shift_value; ++i)
	shift_result->quick_push (new bit (0));
      for (size_t j = 0; i < number->length (); ++i, ++j)
	shift_result->quick_push (((*number)[j])->copy ());
    }
  return shift_result;
}


/* shift_right operation.  Case: var2 is a sym_bit.  */

void
state::shift_right_sym_bits (vec<value*> * arg1_bits, vec<value*> * arg2_bits,
			     tree dest)
{
  for (size_t i = 0; i < get_var_size (dest); i++)
    {
      value *var1_elem = (*arg1_bits)[i];
      value *var2_elem = (*arg2_bits)[i];
      value *new_elem = new shift_right_expression (var1_elem->copy (),
						    var2_elem->copy ());
      delete (*var_states.get (dest))[i];
      (*var_states.get (dest))[i] = new_elem;
    }
}


/* Adds two variables, stores the result in the first one.  */

void
state::add_numbers (vec<value *> *var1, const vec<value *> *var2)
{
  value * carry = new bit (0);
  for (unsigned i = 0; i < var1->length (); i++)
    {
      value *temp = (*var1)[i];
      (*var1)[i] = full_adder ((*var1)[i], (*var2)[i], carry);
      delete temp;
    }
  delete carry;
}


/* ANDs every bit of the vector with var_bit, stroes the result in var1.  */

void
state::and_number_bit (vec< value * > *var1, value *var_bit)
{
  for (unsigned i = 0; i < var1->length (); i++)
    {
      value *tmp = (*var1)[i];
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
state::do_mul (vec<value*> * arg1_bits, vec<value*> * arg2_bits, tree dest)
{
  vec <value *> * dest_bits = var_states.get (dest);
  vec <value *> * shifted = make_copy (arg1_bits);

  for (unsigned i = 0; i < dest_bits->length (); i++)
    {
      delete (*dest_bits)[i];
      (*dest_bits)[i] = new bit (0);
    }

  for (unsigned i = arg2_bits->length (); i != 0; --i)
    {
      if (is_a<bit *> ((*arg2_bits)[i - 1])
	  && as_a<bit *> ((*arg2_bits)[i - 1])->get_val () != 0)
	add_numbers (dest_bits, shifted);
      else if (is_a<symbolic_bit *> ((*arg2_bits)[i - 1]))
	{
	  and_number_bit (shifted, as_a<symbolic_bit *> ((*arg2_bits)[i - 1]));
	  add_numbers (dest_bits, shifted);
	}
      vec <value *> * temp = shifted;
      shifted = shift_left_by_const (shifted, 1);
      free_bits (temp);
      delete temp;
    }
  free_bits (shifted);
  delete shifted;
}


bool
state::check_const_bit_equality (vec<value *> * arg1_bits,
				 vec<value *> * arg2_bits) const
{
  for (size_t i = 1; i < arg1_bits->length (); i++)
    if (as_a<bit *>((*arg1_bits)[i])->get_val ()
	!= as_a<bit *>((*arg2_bits)[i])->get_val ())
      return false;
  return true;
}


bool
state::add_equal_cond (tree arg1, tree arg2)
{
  return add_binary_cond (arg1, arg2, &state::add_equal_cond);
}


/* Adds equality condition for two sequences of bits.  */

void
state::add_equal_cond (vec<value*> * arg1_bits, vec<value*> * arg2_bits)
{
  if (is_bit_vector (arg1_bits) && is_bit_vector (arg2_bits))
    {
      bool result = check_const_bit_equality (arg1_bits, arg2_bits);
      last_cond_status = result ? condition_status::CS_TRUE
				: condition_status::CS_FALSE;
      return;
    }
  for (size_t i = 0; i < arg1_bits->length (); i++)
    {
      conditions.add (new bit_condition ((*arg1_bits)[i]->copy (),
					 (*arg2_bits)[i]->copy (),
					 condition_type::EQUAL));
    }
  last_cond_status = condition_status::CS_SYM;
}


bool
state::check_const_bit_are_not_equal (vec<value *> * arg1_bits,
				      vec<value *> * arg2_bits) const
{
  for (size_t i = 0; i < arg1_bits->length (); i++)
    if (as_a<bit *>((*arg1_bits)[i])->get_val ()
	!= as_a<bit*>((*arg2_bits)[i])->get_val ())
      return true;
  return false;
}


bool
state::add_not_equal_cond (tree arg1, tree arg2)
{
  return add_binary_cond (arg1, arg2, &state::add_not_equal_cond);
}


/* Adds not equal condition for two sequences of bits.  */

void
state::add_not_equal_cond (vec<value*> * arg1_bits, vec<value*> * arg2_bits)
{
  if (is_bit_vector (arg1_bits) && is_bit_vector (arg2_bits))
    {
      bool result = check_const_bit_are_not_equal (arg1_bits, arg2_bits);
      last_cond_status = result ? condition_status::CS_TRUE
				: condition_status::CS_FALSE;
      return;
    }

  bit_expression * prev = nullptr;
  for (size_t i = 0; i < arg1_bits->length (); i++)
    {
      bit_condition* new_cond = new bit_condition ((*arg1_bits)[i]->copy (),
						   (*arg2_bits)[i]->copy (),
						   condition_type::NOT_EQUAL);
      if (prev)
	prev = new bit_or_expression (prev, new_cond);
      else
	prev = new_cond;
    }

  last_cond_status = condition_status::CS_SYM;
  conditions.add (prev);
}


bool
state::check_const_bit_is_greater_than (vec<value *> * arg1_bits,
					vec<value *> * arg2_bits) const
{
  for (int i = arg1_bits->length () - 1; i >= 0; i--)
    {
      if (as_a<bit *>((*arg1_bits)[i])->get_val ()
	  > as_a<bit *>((*arg2_bits)[i])->get_val ())
	return true;
      else if (as_a<bit *>((*arg1_bits)[i])->get_val ()
	       < as_a<bit *>((*arg2_bits)[i])->get_val ())
	return false;
    }
  return false;
}


bool
state::add_greater_than_cond (tree arg1, tree arg2)
{
  return add_binary_cond (arg1, arg2, &state::add_greater_than_cond);
}


/* Adds greater than condition for two sequences of bits.  */

void
state::add_greater_than_cond (vec<value*> * arg1_bits,
			      vec<value*> * arg2_bits)
{
  if (is_bit_vector (arg1_bits) && is_bit_vector (arg2_bits))
    {
      bool result = check_const_bit_is_greater_than (arg1_bits, arg2_bits);
      last_cond_status = result ? condition_status::CS_TRUE
				: condition_status::CS_FALSE;
      return;
    }

  last_cond_status = condition_status::CS_SYM;
  conditions.add (construct_great_than_cond (arg1_bits, arg2_bits));
}


/* Constructs expression trees of greater than condition
   for given sequences of bits.  */

bit_expression*
state::construct_great_than_cond (vec<value*> * arg1_bits,
				  vec<value*> * arg2_bits)
{
  bit_expression* prev = nullptr;
  size_t i = 0;
  for ( ; i < arg1_bits->length () - 1; i++)
    {
      bit_condition* greater_cond
      = new bit_condition ((*arg1_bits)[i]->copy (), (*arg2_bits)[i]->copy (),
			   condition_type::GREAT_THAN);
      bit_condition* eq_cond = new bit_condition ((*arg1_bits)[i + 1]->copy (),
						  (*arg2_bits)[i + 1]->copy (),
						  condition_type::EQUAL);
      bit_expression* and_expr = new bit_and_expression (eq_cond, greater_cond);
      if (prev)
	prev = new bit_or_expression (and_expr, prev);
      else
	prev = and_expr;
    }

  bit_condition* greater_cond = new bit_condition ((*arg1_bits)[i]->copy (),
						   (*arg2_bits)[i]->copy (),
						   condition_type::GREAT_THAN);
  return new bit_or_expression (greater_cond, prev);
}


bool
state::check_const_bit_is_less_than (vec<value *> * arg1_bits,
				     vec<value *> * arg2_bits) const
{
  for (int i = arg1_bits->length () - 1; i >= 0; i--)
    {
      if (as_a<bit *>((*arg1_bits)[i])->get_val ()
	  < as_a<bit *>((*arg2_bits)[i])->get_val ())
	return true;
      else if (as_a<bit *>((*arg1_bits)[i])->get_val ()
	       > as_a<bit *>((*arg2_bits)[i])->get_val ())
	return false;
    }
  return false;
}


bool
state::add_less_than_cond (tree arg1, tree arg2)
{
  return add_binary_cond (arg1, arg2, &state::add_less_than_cond);
}


/* Adds less than condition for two sequences of bits.  */

void
state::add_less_than_cond (vec<value*> * arg1_bits, vec<value*> * arg2_bits)
{
  if (is_bit_vector (arg1_bits) && is_bit_vector (arg2_bits))
    {
      bool result = check_const_bit_is_less_than (arg1_bits, arg2_bits);
      last_cond_status = result ? condition_status::CS_TRUE
				: condition_status::CS_FALSE;
      return;
    }

  last_cond_status = condition_status::CS_SYM;
  conditions.add (construct_less_than_cond (arg1_bits, arg2_bits));
}


/* Constructs expression trees of less than condition
   for given sequences of bits.  */

bit_expression*
state::construct_less_than_cond (vec<value*> * arg1_bits,
				 vec<value*> * arg2_bits)
{
  bit_expression* prev = nullptr;
  size_t i = 0;
  for ( ; i < arg1_bits->length () - 1; i++)
    {
      bit_condition* less_cond = new bit_condition ((*arg1_bits)[i]->copy (),
						    (*arg2_bits)[i]->copy (),
						    condition_type::LESS_THAN);
      bit_condition* eq_cond = new bit_condition ((*arg1_bits)[i + 1]->copy (),
						  (*arg2_bits)[i + 1]->copy (),
						  condition_type::EQUAL);
      bit_expression* and_expr = new bit_and_expression (eq_cond, less_cond);
      if (prev)
	prev = new bit_or_expression (and_expr, prev);
      else
	prev = and_expr;
    }

  bit_condition* less_cond = new bit_condition ((*arg1_bits)[i]->copy (),
						(*arg2_bits)[i]->copy (),
						condition_type::LESS_THAN);
  return new bit_or_expression (less_cond, prev);
}


bool
state::add_greater_or_equal_cond (tree arg1, tree arg2)
{
  return add_binary_cond (arg1, arg2, &state::add_greater_or_equal_cond);
}


/* Adds greater or equal condition for two sequences of bits.  */

void
state::add_greater_or_equal_cond (vec<value*> * arg1_bits,
				  vec<value*> * arg2_bits)
{
  if (is_bit_vector (arg1_bits) && is_bit_vector (arg2_bits))
    {
      bool is_greater_than = check_const_bit_is_greater_than (arg1_bits,
							      arg2_bits);
      bool is_equal = check_const_bit_equality (arg1_bits, arg2_bits);
      last_cond_status = (is_greater_than | is_equal)
			 ? condition_status::CS_TRUE
			 : condition_status::CS_FALSE;
      return;
    }

  last_cond_status = condition_status::CS_SYM;
  conditions.add (
	new bit_or_expression (construct_great_than_cond (arg1_bits, arg2_bits),
			       construct_equal_cond (arg1_bits, arg2_bits)));
}


/* Adds less or equal condition for two sequences of bits.  */

bool
state::add_less_or_equal_cond (tree arg1, tree arg2)
{
  return add_binary_cond (arg1, arg2, &state::add_less_or_equal_cond);
}


void
state::add_less_or_equal_cond (vec<value*> * arg1_bits,
			       vec<value*> * arg2_bits)
{
  if (is_bit_vector (arg1_bits) && is_bit_vector (arg2_bits))
    {
      bool is_less_than = check_const_bit_is_less_than (arg1_bits, arg2_bits);
      bool is_equal = check_const_bit_equality (arg1_bits, arg2_bits);
      last_cond_status = (is_less_than | is_equal)
			 ? condition_status::CS_TRUE
			 : condition_status::CS_FALSE;
      return;
    }

  last_cond_status = condition_status::CS_SYM;
  conditions.add (
	new bit_or_expression (construct_less_than_cond (arg1_bits, arg2_bits),
			       construct_equal_cond (arg1_bits, arg2_bits)));
}


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
  vec<value *> * arg_bits = var_states.get (arg);
  if (is_bit_vector (arg_bits))
    {
      last_cond_status = condition_status::CS_FALSE;
      for (size_t i = 0; i < arg_bits->length (); i++)
	if (as_a<bit *>((*arg_bits)[i])->get_val () != 0)
	  {
	    last_cond_status = condition_status::CS_TRUE;
	    break;
	  }
      return true;
    }

  bit_expression* prev = nullptr;
  for (size_t i = 0; i < arg_bits->length (); i++)
    {
      bit_condition* not_eq_cond
      = new bit_condition ((*arg_bits)[i], new bit (0),
			   condition_type::NOT_EQUAL);
      if (prev)
	prev = new bit_or_expression (not_eq_cond, prev);
      else
	prev = not_eq_cond;
    }

  last_cond_status = condition_status::CS_SYM;
  conditions.add (prev);
  return true;
}


/* Does preprocessing and postprocessing for condition adding.
   Handles bit sequence creation for constant values
   and their removement in the end.  */

bool
state::add_binary_cond (tree arg1, tree arg2, binary_cond_func cond_func)
{
  bool arg1_is_declared = is_declared (arg1);
  bool arg2_is_declared = is_declared (arg2);

  if (!arg1_is_declared && !arg2_is_declared)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Sym-Exec: At least one of arguments must be"
			    " declared for adding condition.\n");

      return false;
    }

  if (arg1_is_declared)
    declare_if_needed (arg2, var_states.get (arg1)->length ());

  if (arg2_is_declared)
    declare_if_needed (arg1, var_states.get (arg2)->length ());

  vec<value*> * arg1_bits = var_states.get (arg1);
  vec<value*> arg1_const_bits (vNULL);
  if (arg1_bits == NULL && TREE_CODE (arg1) == INTEGER_CST)
    {
      arg1_const_bits = create_bits_for_const (arg1,
				var_states.get (arg2)->length ());
      arg1_bits = &arg1_const_bits;
    }

  vec<value*> * arg2_bits = var_states.get (arg2);
  vec<value*> arg2_const_bits (vNULL);
  if (arg2_bits == NULL && TREE_CODE (arg2) == INTEGER_CST)
    {
      arg2_const_bits = create_bits_for_const (arg2,
				var_states.get (arg1)->length ());
      arg2_bits = &arg2_const_bits;
    }

  (this->*cond_func)(arg1_bits, arg2_bits);
  free_bits (&arg1_const_bits);
  free_bits (&arg2_const_bits);
  return true;
}


/* Constructs expression trees of equal condition
   for given sequences of bits.  */

bit_expression*
state::construct_equal_cond (vec<value*> * arg1_bits,
			     vec<value*> * arg2_bits)
{
  bit_expression* prev = nullptr;
  for (size_t i = 0; i < arg1_bits->length (); i++)
    {
      bit_condition* eq_expr = new bit_condition ((*arg1_bits)[i]->copy (),
						  (*arg2_bits)[i]->copy (),
						  condition_type::EQUAL);
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

  print_bits (var_states.get (dest));
  return true;
}


/* Performs addition on arg1 pointer.  */

bool
state::do_pointer_plus (tree arg1, tree arg2, tree dest)
{
  declare_if_needed (dest, tree_to_uhwi (TYPE_SIZE (TREE_TYPE (dest))));
  if (!is_declared (arg1) || !is_declared (arg2))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Sym-Exec: For pointer addition "
			    "arguments must be declared\n");

      return false;
    }

  if (is_bit_vector (var_states.get (arg2))
	&& get_value (var_states.get (arg2)) == 0)
    return do_mem_ref (arg1, dest);

  return do_add (arg1, arg2, dest);
}


/* Perform subtractions on arg1 pointer.  */

bool
state::do_pointer_diff (tree arg1, tree arg2, tree dest)
{
  declare_if_needed (dest, tree_to_uhwi (TYPE_SIZE (TREE_TYPE (dest))));
  if (!is_declared (arg1) || !is_declared (arg2))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Sym-Exec: For pointer subtraction "
			    "arguments must be declared\n");

      return false;
    }

  if (is_bit_vector (var_states.get (arg2))
  	&& get_value (var_states.get (arg2)) == 0)
    return do_mem_ref (arg1, dest);

  return do_sub (arg1, arg2, dest);
}


vec<value *> *
state::make_copy (vec<value *> *bits) const
{
  vec<value *> * copied_bits = new vec<value*> ();
  copied_bits->create (bits->length ());
  for (size_t i = 0; i < bits->length (); i++)
    copied_bits->quick_push ((*bits)[i]->copy ());

  return copied_bits;
}


condition_status
state::get_last_cond_status ()
{
  return last_cond_status;
}


void
state::print_bits (vec<value *> * bits)
{
  if (!dump_file || !(dump_flags & TDF_DETAILS))
    return;

  fprintf (dump_file, "{");
  for (int i = bits->length () - 1; i >= 0; i--)
    {
      (*bits)[i]->print ();

      if (i)
	fprintf (dump_file, ", ");
    }
  fprintf (dump_file, "}\n");
}



size_t min (size_t a, size_t b, size_t c)
{
  size_t min = (a < b ? a : b);
  return min < c ? min : c;
}


/* Casts arg_bits to cast_size size, stores value in dest.  */

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

  //TODO: add case for signed numbers

  vec<value *> *arg = var_states.get (var);
  vec<value *> *dest_bits = var_states.get (dest);

  size_t arg_size = min (arg->length (), dest_bits->length (), cast_size);

  for (size_t i = 0; i < arg_size; i++)
    {
      value *temp = (*dest_bits)[i];
      (*dest_bits)[i] = (*arg)[i]->copy ();
      delete temp;
    }

  value * sign_bit = arg->last ();
  for (size_t i = arg_size; i < dest_bits->length (); i++)
    {
      value *temp = (*dest_bits)[i];
      (*dest_bits)[i] = sign_bit->copy ();
      delete temp;
    }
  return true;
}
