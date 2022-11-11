/* State will store states of variables for a function's single execution path.
   It will be used for bit-level symbolic execution to determine values of bits
   of function's return value and symbolic marked arguments.  */

#include "stddef.h"
#include "state.h"
#include "vec.h"
#include "hash-set.h"
#include "expression.h"


/* Checks whether state for variable with specified name already
   exists or not.  */

bool
State::is_declared (tree *var)
{
  return var_states.get (var) != NULL;
}


/* performs AND operation for 2 symbolic_bit operands.  */

value *
State::and_sym_bits (const value * var1, const value * var2) const
{
  return new bit_and_expression (var1->copy (), var2->copy ());
}

/* performs AND operation for a symbolic_bit and const_bit operands.  */

value *
State::and_var_const (const value * var1, const bit * const_bit) const
{
  if (const_bit->get_val () == 1)
    return var1->copy ();

  return new bit (0);
}


/* performs AND operation for 2 constant bit operands.  */

bit *
State::and_const_bits (const bit * const_bit1, const bit * const_bit2) const
{
  if (const_bit1->get_val () == const_bit2->get_val ())
    return new bit (*const_bit1);

  return new bit (0);
}


/* performs OR operation for 2 symbolic_bit operands.  */

value *
State::or_sym_bits (const value * var1, const value * var2) const
{
  return new bit_or_expression (var1->copy (), var2->copy ());
}

/* performs OR operation for a symbolic_bit and a constant bit operands.  */

value *
State::or_var_const (const value * var1, const bit * const_bit) const
{
  if (const_bit->get_val () == 0)
    return var1->copy ();

  return new bit (1);
}

/* performs OR operation for 2 constant bit operands.  */

bit *
State::or_const_bits (const bit * const_bit1, const bit * const_bit2) const
{
  if (const_bit1->get_val () == const_bit2->get_val ())
    return new bit (*const_bit1);

  return new bit (1);
}


/* Adds an empty state for the given variable.  */

bool
State::decl_var (tree *var, unsigned size)
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
State::get_var_size (tree *var)
{
  vec < value * > *content = var_states.get (var);
  if (content == NULL)
    return 0;

  return content->length ();
}


/* Adds a variable with unknown value to state.  Such variables are
   represented as sequence of symbolic bits.  */

bool
State::make_symbolic (tree *var, unsigned size)
{
  if (is_declared (var))
    return false;

  vec < value * > bits;
  bits.create (size);

  /* Initialize each bit of a variable with unknown value.  */
  for (size_t i = 0; i < size; i++)
    bits.quick_push (new symbolic_bit (i));

  return var_states.put (var, bits);
}


/* Does bit-level AND operation for given variables.  */

void
State::do_and (tree* arg1, tree* arg2, tree* dest)
{
  gcc_assert (!(is_declared (arg1) || is_declared (arg2)
		|| is_declared (dest)));
  gcc_assert (get_var_size (arg1) == get_var_size (dest)
	      && get_var_size (arg2) == get_var_size (dest));

  /* Creating AND expressions for every bit pair of given arguments
     and store them as a new state for given destination.  */

  for (size_t i = 0; i < get_var_size (dest); i++)
  {
    value *result = nullptr;
    value *sym_bit1 = dyn_cast<symbolic_bit *> ((*var_states.get (arg1))[i]);
    if (sym_bit1 == nullptr)
      sym_bit1 = dyn_cast<bit_expression *> ((*var_states.get (arg1))[i]);

    value *sym_bit2 = dyn_cast<symbolic_bit *> ((*var_states.get (arg2))[i]);
    if (sym_bit2 == nullptr)
      sym_bit2 = dyn_cast<bit_expression *> ((*var_states.get (arg2))[i]);

    bit *const_bit1 = dyn_cast<bit *> ((*var_states.get (arg1))[i]);
    bit *const_bit2 = dyn_cast<bit *> ((*var_states.get (arg2))[i]);

    if (sym_bit1 && sym_bit2)
      result = and_sym_bits (sym_bit1, sym_bit2);
    else if (sym_bit1 && const_bit2)
      result = and_var_const (sym_bit1, const_bit2);
    else if (const_bit1 && sym_bit2)
      result = and_var_const (sym_bit2, const_bit1);
    else if (const_bit1 && const_bit2)
      result = and_const_bits (const_bit1, const_bit2);
    else
      gcc_assert (true);

    delete (*var_states.get (dest))[i];
    (*var_states.get (dest))[i] = result;
  }
}


/* Does bit-level OR operation for given variables.  */

void
State::do_or (tree* arg1, tree* arg2, tree* dest)
{
  gcc_assert (!(is_declared (arg1) || is_declared (arg2)
		|| is_declared (dest)));
  gcc_assert (get_var_size (arg1) == get_var_size (dest)
	      && get_var_size (arg2) == get_var_size (dest));

  /* Creating OR expressions for every bit pair of given arguments
   and store them as a new state for given destination.  */
  for (size_t i = 0; i < get_var_size (dest); i++)
  {
    value *result = nullptr;
    value *sym_bit1 = dyn_cast<symbolic_bit *> ((*var_states.get (arg1))[i]);
    if (sym_bit1 == nullptr)
      sym_bit1 = dyn_cast<bit_expression *> ((*var_states.get (arg1))[i]);

    value *sym_bit2 = dyn_cast<symbolic_bit *> ((*var_states.get (arg2))[i]);
    if (sym_bit2 == nullptr)
      sym_bit2 = dyn_cast<bit_expression *> ((*var_states.get (arg2))[i]);

    bit *const_bit1 = dyn_cast<bit *> ((*var_states.get (arg1))[i]);
    bit *const_bit2 = dyn_cast<bit *> ((*var_states.get (arg2))[i]);

    if (sym_bit1 && sym_bit2)
      result = or_sym_bits (sym_bit1, sym_bit2);
    else if (sym_bit1 && const_bit2)
      result = or_var_const (sym_bit1, const_bit2);
    else if (const_bit1 && sym_bit2)
      result = or_var_const (sym_bit2, const_bit1);
    else if (const_bit1 && const_bit2)
      result = or_const_bits (const_bit1, const_bit2);
    else
      gcc_assert (true);

    delete (*var_states.get (dest))[i];
    (*var_states.get (dest))[i] = result;
  }
}


/* Does bit-level XOR operation for given variables.  */

void
State::do_xor (tree *arg1, tree *arg2, tree *dest)
{
  gcc_assert (!(is_declared (arg1) || is_declared (arg2)
		|| is_declared (dest)));
  gcc_assert (get_var_size (arg1) == get_var_size (dest)
	      && get_var_size (arg2) == get_var_size (dest));

  for (size_t i = 0; i < get_var_size (dest); i++)
  {
    value *result = nullptr;
    value *sym_bit1 = dyn_cast<symbolic_bit *> ((*var_states.get (arg1))[i]);
    if (sym_bit1 == nullptr)
      sym_bit1 = dyn_cast<bit_expression *> ((*var_states.get (arg1))[i]);

    value *sym_bit2 = dyn_cast<symbolic_bit *> ((*var_states.get (arg2))[i]);
    if (sym_bit2 == nullptr)
      sym_bit2 = dyn_cast<bit_expression *> ((*var_states.get (arg2))[i]);

    bit *const_bit1 = dyn_cast<bit *> ((*var_states.get (arg1))[i]);
    bit *const_bit2 = dyn_cast<bit *> ((*var_states.get (arg2))[i]);

    if (sym_bit1 && sym_bit2)
      result = xor_sym_bits (sym_bit1, sym_bit2);
    else if (sym_bit1 && const_bit2)
      result = xor_var_const (sym_bit1, const_bit2);
    else if (const_bit1 && sym_bit2)
      result = xor_var_const (sym_bit2, const_bit1);
    else if (const_bit1 && const_bit2)
      result = xor_const_bits (const_bit1, const_bit2);
    else
      gcc_assert (true);

    delete (*var_states.get (dest))[i];
    (*var_states.get (dest))[i] = result;
  }
}


/* Shifts value_vector left by shift_value bits.  */

vec <value *>
State::shift_left_by_const (const vec < value * > * value_vector,
			    size_t shift_value)
{
  vec <value *> shift_result;
  shift_result.create (value_vector->length ());
  if (shift_result.length () <= shift_value)
    for (size_t i = 0; i < shift_result.length (); i++)
      shift_result[i] = new bit (0);
  else
  {
    size_t i = 0;
    for ( ; i < shift_result.length () - shift_value; ++i)
      shift_result[i] = ((*value_vector)[shift_value + i])->copy ();

    for ( ; i < shift_result.length (); ++i)
      shift_result[i] = new bit (0);
  }
  return shift_result;
}


/* Checks if all vector elements are const_bit_expressions.  */

bool
State::is_bit_vector (vec <value *>* value_vector)
{
  for (size_t i = 0; i < value_vector->length (); i++)
    if (!(is_a <bit *> ((*value_vector)[i])))
	return false;
  return true;
}


/* returns the value of the number represented as a bit vector.  */

size_t
State::get_value (vec <value *> * bit_vector)
{
  size_t number = 0;
  for (int i = bit_vector->length () - 1; i >= 0; --i)
  {
    bit * cur_elem = dyn_cast<bit *> ((*bit_vector)[i]);
    number = (number | cur_elem->get_val ()) << 1;
  }

  return number;
}


/* shift_left operation.  Case: var2 is a sym_bit.  */

void
State::shift_left_sym_bits (tree * var1, tree * var2, tree * dest)
{
  for (size_t i = 0; i < get_var_size (dest); i++)
  {
    value * var1_elem = (*var_states.get (var1))[i];
    value * var2_elem = (*var_states.get (var2))[i];
    value * new_elem = new shift_left_expression (var1_elem->copy (),
						  var2_elem->copy ());
    delete (*var_states.get (dest))[i];
    (*var_states.get (dest))[i] = new_elem;
  }
}


/* Does SHIFT_LEFT operation for given variables.  */

void
State::do_shift_left (tree * arg1, tree * arg2, tree * dest)
{
  gcc_assert (!(is_declared (arg1) || is_declared (arg2)
		|| is_declared (dest)));
  gcc_assert (get_var_size (arg1) == get_var_size (dest)
  	      && get_var_size (arg2) == get_var_size (dest));

  if (is_bit_vector (var_states.get (arg2)))
  {
    size_t shift_value = get_value (var_states.get (arg2));
    vec <value *> result = shift_left_by_const (var_states.get (arg1),
						shift_value);
    for (size_t i = 0; i < get_var_size (dest); i++)
    {
      delete (*var_states.get (dest))[i];
      (*var_states.get (dest))[i] = result[i];
    }
  }
  else
    shift_left_sym_bits (arg1, arg2, dest);
}


/* Does SHIFT_RIGHT operation for given variables.  */

void
State::do_shift_right (tree * arg1, tree * arg2, tree * dest)
{
  gcc_assert (!(is_declared (arg1) || is_declared (arg2)
		|| is_declared (dest)));
  gcc_assert (get_var_size (arg1) == get_var_size (dest)
	      && get_var_size (arg2) == get_var_size (dest));

  if (is_bit_vector (var_states.get (arg2)))
  {
    size_t shift_value = get_value (var_states.get (arg2));
    vec <value *> result = shift_right_by_const (var_states.get (arg1),
						 shift_value);
    for (size_t i = 0; i < get_var_size (dest); i++)
    {
      delete (*var_states.get (dest))[i];
      (*var_states.get (dest))[i] = result[i];
    }
  }
  else
    shift_right_sym_bits (arg1, arg2, dest);
}


/* Adds two variables.  */

void
State::do_add (tree *arg1, tree *arg2, tree *dest)
{
  gcc_assert (!(is_declared (arg1) || is_declared (arg2)
		|| is_declared (dest)));
  gcc_assert (get_var_size (dest)
	      == (get_var_size (arg1) > get_var_size (arg2)
		  ? get_var_size (arg1) : get_var_size (arg2)));

  for (size_t i = 0; i < get_var_size (dest); i++)
  {
    value * new_val = new add_expression ((*var_states.get (arg1))[i],
					  (*var_states.get (arg2))[i]);
    delete (*var_states.get (dest))[i];
    (*var_states.get (dest))[i] = new_val;
  }
}


/* Does subtraction.  */

void
State::do_sub (tree *arg1, tree *arg2, tree *dest)
{
  gcc_assert (!(is_declared (arg1) || is_declared (arg2)
		|| is_declared (dest)));
  gcc_assert (get_var_size (arg1) == get_var_size (dest)
	      && get_var_size (arg2) == get_var_size (dest));

  for (size_t i = 0; i < get_var_size (dest); i++)
  {
    value * new_val = new sub_expression ((*var_states.get (arg1))[i],
					  (*var_states.get (arg2))[i]);
    delete (*var_states.get (dest))[i];
    (*var_states.get (dest))[i] = new_val;
  }
}


/* Negates given variable.  */

void
State::do_complement (tree *arg, tree *dest)
{
  gcc_assert (!(is_declared (arg) || is_declared (dest)));
  gcc_assert (get_var_size (arg) == get_var_size (dest));

  /* Creating complement expressions for every bit the given argument
     and store it as a new state for given destination.  */
  for (size_t i = 0; i < get_var_size (dest); i++)
  {
    value *result = nullptr;
    bit* const_bit = dyn_cast<bit *> ((*var_states.get (arg))[i]);
    if (const_bit)
      result = complement_const_bit (const_bit);
    else
      result = complement_sym_bit ((*var_states.get (arg))[i]);

    delete (*var_states.get (dest))[i];
    (*var_states.get (dest))[i] = result;
  }
}


/* performs NOT operation for constant bit.  */

bit *
State::complement_const_bit (const bit * const_bit) const
{
  return new bit (1u - const_bit->get_val ());
}


/* performs NOT operation for symbolic_bit.  */

value *
State::complement_sym_bit (const value * var) const
{
  return new bit_complement_expression (var->copy ());
}


/* performs XOR operation for 2 symbolic_bit operands.  */

value *
State::xor_sym_bits (const value * var1, const value * var2) const
{
  value * var2_copy = var2->copy ();
  bit_expression * var2_node_with_const_child
      = get_parent_with_const_child (var2_copy);

  if (var2_node_with_const_child != nullptr)
  {
    value * var1_copy = var1->copy ();
    bit_expression * var1_node_with_const_child
	= get_parent_with_const_child (var1_copy);

    if (var1_node_with_const_child != nullptr)
    {
      value * var1_left = var1_node_with_const_child->get_left ();
      if (var1_left != nullptr && dyn_cast<bit *> (var1_left) != nullptr)
      {
	value * var2_left = var2_node_with_const_child->get_left ();
	if (var2_left != nullptr && dyn_cast<bit *> (var2_left) != nullptr)
	{
	  bit * new_left = xor_const_bits (dyn_cast<bit *> (var1_left),
					   dyn_cast<bit *> (var2_left));
	  delete var2_left;
	  var2_node_with_const_child->set_left (nullptr);

	  delete var1_left;
	  var1_node_with_const_child->set_left (new_left);
	}
	else
	{
	  value * var2_right = var2_node_with_const_child->get_right ();
	  bit * new_left = xor_const_bits (dyn_cast<bit *> (var1_left),
					   dyn_cast<bit *> (var2_right));
	  delete var2_right;
	  var2_node_with_const_child->set_right (nullptr);

	  delete var1_left;
	  var1_node_with_const_child->set_left (new_left);
	}
      }

      else
      {
	value * var1_right = var1_node_with_const_child->get_right ();
	value * var2_left = var2_node_with_const_child->get_left ();
	if (var2_left != nullptr && dyn_cast<bit *> (var2_left) != nullptr)
	{
	  bit * new_right = xor_const_bits (dyn_cast<bit *> (var1_left),
					    dyn_cast<bit *> (var2_left));
	  delete var2_left;
	  var2_node_with_const_child->set_left (nullptr);

	  delete var1_right;
	  var1_node_with_const_child->set_right (new_right);
	}
	else
	{
	  value * var2_right = var2_node_with_const_child->get_right ();
	  bit * new_right = xor_const_bits (dyn_cast<bit *> (var1_right),
					    dyn_cast<bit *> (var2_right));
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


/* performs XOR operation for 2 constant bit operands.  */

bit *
State::xor_const_bits (const bit * const_bit1, const bit * const_bit2) const
{
  return new bit (const_bit1->get_val () ^ const_bit2->get_val ());
}


/* performs XOR operation for a symbolic_bit and const_bit operands.  */

value *
State::xor_var_const (const value * var, const bit * const_bit) const
{
  value * var_copy = var->copy ();
  bit_expression * node_with_const_child
      = get_parent_with_const_child (var_copy);
  if (node_with_const_child != nullptr)
  {
    value * left = node_with_const_child->get_left ();
    if (left != nullptr && dyn_cast<bit *> (left) != nullptr)
    {
      bit * new_left = xor_const_bits (dyn_cast<bit *> (left), const_bit);
      delete left;
      node_with_const_child->set_left (new_left);
    }
    else
    {
      value * right = node_with_const_child->get_right ();
      bit * new_right = xor_const_bits (dyn_cast<bit *> (right), const_bit);
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
State::get_parent_with_const_child (value* root) const
{
  const bit_expression * expr = dyn_cast<const bit_expression *> (root);
  if (expr == nullptr)
    return nullptr;

  bit_expression* expr_root = dyn_cast<bit_expression *> (expr->copy ());
  hash_set<bit_expression *> nodes_to_consider;
  nodes_to_consider.add (expr_root);

  while (!nodes_to_consider.is_empty ())
  {
    bit_expression* cur_element = *nodes_to_consider.begin ();
    nodes_to_consider.remove (cur_element);

    value* left = cur_element->get_left ();
    value* right = cur_element->get_right ();

    if ((left != nullptr && dyn_cast<bit *> (left) != nullptr)
	|| (right != nullptr && dyn_cast<bit *> (right) != nullptr))
      return cur_element;

    if (left != nullptr && is_safe_branching (left))
      nodes_to_consider.add (dyn_cast<bit_expression *> (left));

    if (right != nullptr && is_safe_branching (right))
      nodes_to_consider.add (dyn_cast<bit_expression *> (right));
  }
}


/* Checks if node is AND, OR or XOR expression.  */

bool
State::is_safe_branching (value* node) const
{
  return dyn_cast<bit_and_expression *> (node) != nullptr
	 || dyn_cast<bit_or_expression *> (node) != nullptr
	 || dyn_cast<bit_xor_expression *> (node) != nullptr;
}


/* Shifts value_vector right by shift_value bits.  */

vec <value *>
State::shift_right_by_const (const vec < value * > * value_vector,
			     size_t shift_value)
{
  vec <value *> shift_result;
  shift_result.create (value_vector->length ());
  if (shift_result.length () <= shift_value)
    for (size_t i = 0; i < shift_result.length (); i++)
      {
	shift_result[i] = new bit (0);
      }
  else
    {
      size_t i = 0;
      for ( ; i < shift_result.length () - shift_value; ++i)
	shift_result[i] = new bit (0);

      for (size_t j = 0; i < shift_result.length (); ++i, ++j)
	shift_result[i] = ((*value_vector)[j])->copy ();
    }
  return shift_result;
}


/* shift_right operation.  Case: var2 is a sym_bit.  */

void
State::shift_right_sym_bits (tree * var1, tree * var2, tree * dest)
{
  for (size_t i = 0; i < get_var_size (dest); i++)
    {
      value * var1_elem = (*var_states.get (var1))[i];
      value * var2_elem = (*var_states.get (var2))[i];
      value * new_elem = new shift_right_expression (var1_elem->copy (),
						     var2_elem->copy ());
      delete (*var_states.get (dest))[i];
      (*var_states.get (dest))[i] = new_elem;
    }
}
