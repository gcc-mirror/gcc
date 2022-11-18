/* State will store states of variables for a function's single execution path.
   It will be used for bit-level symbolic execution to determine values of bits
   of function's return value and symbolic marked arguments.  */

#include "stddef.h"
#include "state.h"
#include "vec.h"
#include "hash-set.h"
#include "condition.h"


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


void
state::check_args_compatibility (tree arg1, tree arg2, tree dest)
{
  gcc_assert ((is_declared (arg1) || TREE_CODE (arg1) == INTEGER_CST)
	      && (is_declared (arg2) || TREE_CODE (arg2) == INTEGER_CST)
	      && is_declared (dest));
  gcc_assert ((get_var_size (arg1) == get_var_size (dest)
	       || TREE_CODE (arg1) == INTEGER_CST)
	      && (get_var_size (arg2) == get_var_size (dest)
		  || TREE_CODE (arg2) == INTEGER_CST));
}


vec<value*>
state::create_bits_for_const (tree var, size_t size) const
{
  HOST_WIDE_INT val = tree_to_shwi (var);
  unsigned HOST_WIDE_INT pos_bit = 1;

  vec<value *> bits;
  bits.create (size);
  for (size_t i = 0; i < size; i++)
    {
      bits.quick_push (new bit (val & pos_bit));
      pos_bit >>= 1;
    }

  return bits;
}


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
    bits.quick_push (new symbolic_bit (i));

  return var_states.put (var, bits);
}


void
state::do_binary_operation (tree arg1, tree arg2, tree dest,
			    binary_func bin_func)
{
  check_args_compatibility (arg1, arg2, dest);
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
}


/* Does bit-level AND operation for given variables.  */

void
state::do_and (tree arg1, tree arg2, tree dest)
{
  do_binary_operation (arg1, arg2, dest, &state::do_and);
}


void
state::do_and (vec<value*> * arg1_bits, vec<value*> * arg2_bits, tree dest)
{
  /* Creating AND expressions for every bit pair of given arguments
     and store them as a new state for given destination.  */
  for (size_t i = 0; i < get_var_size (dest); i++)
    {
      value *result = nullptr;
      value *arg1_bit = (*arg1_bits)[i];
      value *arg2_bit = (*arg2_bits)[i];

      if (is_a<bit *> (arg1_bit) && is_a<bit *> (arg2_bit))
	result = and_const_bits (as_a<bit *> (arg1_bit),
				 as_a<bit *> (arg2_bit));

      else if (is_a<bit *> (arg1_bit)
	       && (is_a<symbolic_bit *> (arg2_bit)
		   || (is_a<bit_expression *> (arg2_bit))))
	result = and_var_const (arg2_bit, as_a<bit *> (arg1_bit));

      else if ((is_a<symbolic_bit *> (arg1_bit)
		|| (is_a<bit_expression *> (arg1_bit)))
	       && is_a<bit *> (arg2_bit))
	result = and_var_const (arg1_bit, as_a<bit *> (arg2_bit));

      else
	result = and_sym_bits (arg1_bit, arg2_bit);

      delete (*var_states.get (dest))[i];
      (*var_states.get (dest))[i] = result;
    }
}


/* Does bit-level OR operation for given variables.  */

void
state::do_or (tree arg1, tree arg2, tree dest)
{
  do_binary_operation (arg1, arg2, dest, &state::do_or);
}


void
state::do_or (vec<value*> * arg1_bits, vec<value*> * arg2_bits, tree dest)
{
  /* Creating OR expressions for every bit pair of given arguments
     and store them as a new state for given destination.  */
  for (size_t i = 0; i < get_var_size (dest); i++)
    {
      value *result = nullptr;
      value *arg1_bit = (*arg1_bits)[i];
      value *arg2_bit = (*arg2_bits)[i];

      if (is_a<bit *> (arg1_bit) && is_a<bit *> (arg2_bit))
	result = or_const_bits (as_a<bit *> (arg1_bit), as_a<bit *> (arg2_bit));

      else if (is_a<bit *> (arg1_bit)
	       && (is_a<symbolic_bit *> (arg2_bit)
		   || (is_a<bit_expression *> (arg2_bit))))
	result = or_var_const (arg2_bit, as_a<bit *> (arg1_bit));

      else if ((is_a<symbolic_bit *> (arg1_bit)
		|| (is_a<bit_expression *> (arg1_bit)))
	       && is_a<bit *> (arg2_bit))
	result = or_var_const (arg1_bit, as_a<bit *> (arg2_bit));

      else
	result = or_sym_bits (arg1_bit, arg2_bit);

      delete (*var_states.get (dest))[i];
      (*var_states.get (dest))[i] = result;
    }
}


/* Does bit-level XOR operation for given variables.  */

void
state::do_xor (tree arg1, tree arg2, tree dest)
{
  do_binary_operation (arg1, arg2, dest, &state::do_xor);
}


void
state::do_xor (vec<value*> * arg1_bits, vec<value *> * arg2_bits, tree dest)
{
  for (size_t i = 0; i < get_var_size (dest); i++)
    {
      value *result = nullptr;
      value *arg1_bit = (*arg1_bits)[i];
      value *arg2_bit = (*arg2_bits)[i];

      if (is_a<bit *> (arg1_bit) && is_a<bit *> (arg2_bit))
	result = xor_const_bits (as_a<bit *> (arg1_bit),
				 as_a<bit *> (arg2_bit));

      else if (is_a<bit *> (arg1_bit)
	       && (is_a<symbolic_bit *> (arg2_bit)
		   || (is_a<bit_expression *> (arg2_bit))))
	result = xor_var_const (arg2_bit, as_a<bit *> (arg1_bit));

      else if ((is_a<symbolic_bit *> (arg1_bit)
		|| (is_a<bit_expression *> (arg1_bit)))
	       && is_a<bit *> (arg2_bit))
	result = xor_var_const (arg1_bit, as_a<bit *> (arg2_bit));

      else
	result = xor_sym_bits (arg1_bit, arg2_bit);

      delete (*var_states.get (dest))[i];
      (*var_states.get (dest))[i] = result;
    }
}


/* Shifts value_vector right by shift_value bits.  */

vec <value *>
state::shift_right_by_const (const vec <value *> * value_vector,
			     size_t shift_value)
{
  vec <value *> shift_result;
  shift_result.create (value_vector->length ());
  if (value_vector->length () <= shift_value)
    for (size_t i = 0; i < value_vector->length (); i++)
      shift_result.quick_push (new bit (0));
  else
    {
      size_t i = 0;
      for (; i < value_vector->length () - shift_value; ++i)
	shift_result.quick_push (((*value_vector)[shift_value + i])->copy ());

      for (; i < value_vector->length (); ++i)
	shift_result.quick_push (new bit (0));
    }
  return shift_result;
}


/* Checks if all vector elements are const_bit_expressions.  */

bool
state::is_bit_vector (vec <value *>* value_vector)
{
  for (size_t i = 0; i < value_vector->length (); i++)
    if (!(is_a <bit *> ((*value_vector)[i])))
	return false;
  return true;
}


/* Returns the value of the number represented as a bit vector.  */

size_t
state::get_value (vec <value *> * bit_vector)
{
  size_t number = 0;
  for (int i = bit_vector->length () - 1; i >= 0; --i)
    {
      bit *cur_elem = dyn_cast<bit *> ((*bit_vector)[i]);
      number = (number | cur_elem->get_val ()) << 1;
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

void
state::do_shift_left (tree arg1, tree arg2, tree dest)
{
  do_binary_operation (arg1, arg2, dest, &state::do_shift_left);
}


void
state::do_shift_left (vec<value*> * arg1_bits, vec<value*> * arg2_bits,
		      tree dest)
{
  if (is_bit_vector (arg2_bits))
    {
      size_t shift_value = get_value (arg2_bits);
      vec < value * > result = shift_left_by_const (arg1_bits, shift_value);
      for (size_t i = 0; i < get_var_size (dest); i++)
	{
	  delete (*var_states.get (dest))[i];
	  (*var_states.get (dest))[i] = result[i];
	}
    }
  else
    shift_left_sym_bits (arg1_bits, arg2_bits, dest);
}



/* Does shift_right operation for given variables.  */

void
state::do_shift_right (tree arg1, tree arg2, tree dest)
{
  do_binary_operation (arg1, arg2, dest, &state::do_shift_right);
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


/* Adds two variables.  */

void
state::do_add (tree arg1, tree arg2, tree dest)
{
  do_binary_operation (arg1, arg2, dest, &state::do_add);
}


void
state::do_add (vec<value*> * arg1_bits, vec<value*> * arg2_bits, tree dest)
{
  for (size_t i = 0; i < get_var_size (dest); i++)
    {
      value *new_val = new add_expression ((*arg1_bits)[i]->copy (),
					   (*arg2_bits)[i]->copy ());
      delete (*var_states.get (dest))[i];
      (*var_states.get (dest))[i] = new_val;
    }
}


/* Does subtraction.  */

void
state::do_sub (tree arg1, tree arg2, tree dest)
{
  do_binary_operation (arg1, arg2, dest, &state::do_sub);
}


void
state::do_sub (vec<value*> * arg1_bits, vec<value*> * arg2_bits, tree dest)
{
  for (size_t i = 0; i < get_var_size (dest); i++)
    {
      value *new_val = new sub_expression ((*arg1_bits)[i]->copy (),
					   (*arg2_bits)[i]->copy ());
      delete (*var_states.get (dest))[i];
      (*var_states.get (dest))[i] = new_val;
    }
}


/* Negates given variable.  */

void
state::do_complement (tree arg, tree dest)
{
  gcc_assert (is_declared (arg) && is_declared (dest));
  gcc_assert (get_var_size (arg) == get_var_size (dest));

  /* Creating complement expressions for every bit the given argument
     and store it as a new state for given destination.  */
  for (size_t i = 0; i < get_var_size (dest); i++)
    {
      value *result = nullptr;
      bit *const_bit = dyn_cast<bit *> ((*var_states.get (arg))[i]);
      if (const_bit)
	result = complement_const_bit (const_bit);
      else
	result = complement_sym_bit ((*var_states.get (arg))[i]);

      delete (*var_states.get (dest))[i];
      (*var_states.get (dest))[i] = result;
    }
}


void
state::do_assign (tree arg, tree dest)
{
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
      vec < value * > arg_bits
      = create_bits_for_const (arg, dest_bits->length ());
      for (size_t i = 0; i < dest_bits->length (); i++)
	{
	  delete (*dest_bits)[i];
	  (*dest_bits)[i] = arg_bits[i];
	}
    }
  else
    gcc_assert (false);
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


/* Shifts value_vector left by shift_value bits.  */

vec <value *>
state::shift_left_by_const (const vec < value * > * value_vector,
			    size_t shift_value)
{
  vec <value *> shift_result;
  shift_result.create (value_vector->length ());
  if (value_vector->length () <= shift_value)
    for (size_t i = 0; i < value_vector->length (); i++)
      shift_result.quick_push (new bit (0));
  else
    {
      size_t i = 0;
      for (; i < value_vector->length () - shift_value; ++i)
	shift_result.quick_push (new bit (0));

      for (size_t j = 0; i < value_vector->length (); ++i, ++j)
	shift_result.quick_push (((*value_vector)[j])->copy ());
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


bool
state::check_const_bit_equality (vec<value *> * arg1_bits,
				 vec<value *> * arg2_bits) const
{
  for (size_t i = 1; i < arg1_bits->length (); i++)
    if ((*arg1_bits)[i] != (*arg2_bits)[i])
      return false;
  return true;
}


void
state::add_equal_cond (tree arg1, tree arg2)
{
  vec<value *> * arg1_bits = var_states.get (arg1);
  vec<value *> * arg2_bits = var_states.get (arg2);

  if (is_bit_vector (arg1_bits) && is_bit_vector (arg2_bits))
    {
      bool result = check_const_bit_equality (arg1_bits, arg2_bits);
      conditions.add (new bit_condition (nullptr, nullptr,
					 result ? condition_type::IS_TRUE
						: condition_type::IS_FALSE));
      return;
    }
  else if (is_bit_vector (arg1_bits) && TREE_CODE (arg2) == INTEGER_CST
	   && (integer_onep (arg2) || integer_zerop (arg2)))
    {
      size_t i = 0;
      if (integer_onep (arg2))
	{
	  conditions.add (new bit_condition ((*arg1_bits)[0]->copy (),
					     new bit (1),
					     condition_type::EQUAL));
	  i++;
	}

      for (; i < arg1_bits->length (); i++)
	conditions.add (new bit_condition ((*arg1_bits)[i]->copy (),
					   new bit (0),
					   condition_type::EQUAL));
    }

  for (size_t i = 0; i < arg1_bits->length (); i++)
    {
      conditions.add (new bit_condition ((*arg1_bits)[i]->copy (),
					 (*arg2_bits)[i]->copy (),
					 condition_type::EQUAL));
    }
}


bool
state::check_const_bit_are_not_equal (vec<value *> * arg1_bits,
				      vec<value *> * arg2_bits) const
{
  for (size_t i = 0; i < arg1_bits->length (); i++)
    if ((*arg1_bits)[i] != (*arg2_bits)[i])
      return true;
  return false;
}


void
state::add_not_equal_cond (tree arg1, tree arg2)
{
  vec<value *> * arg1_bits = var_states.get (arg1);
  vec<value *> * arg2_bits = var_states.get (arg2);

  if (is_bit_vector (arg1_bits) && is_bit_vector (arg2_bits))
    {
      bool result = check_const_bit_are_not_equal (arg1_bits, arg2_bits);
      conditions.add (new bit_condition (nullptr, nullptr,
					 result ? condition_type::IS_TRUE
						: condition_type::IS_FALSE));
      return;
    }

  /* TODO: add condition when one of arguments is symbolic.  */
}


bool
state::check_const_bit_is_greater_than (vec<value *> * arg1_bits,
					vec<value *> * arg2_bits) const
{
  for (int i = arg1_bits->length () - 1; i >= 0; i--)
    {
      if ((*arg1_bits)[i] > (*arg2_bits)[i])
	return true;
      else if ((*arg1_bits)[i] < (*arg2_bits)[i])
	return false;
    }
  return false;
}


void
state::add_greater_than_cond (tree arg1, tree arg2)
{
  vec<value *> * arg1_bits = var_states.get (arg1);
  vec<value *> * arg2_bits = var_states.get (arg2);

  if (is_bit_vector (arg1_bits) && is_bit_vector (arg2_bits))
    {
      bool result = check_const_bit_is_greater_than (arg1_bits, arg2_bits);
      conditions.add (new bit_condition (nullptr, nullptr,
					 result ? condition_type::IS_TRUE
						: condition_type::IS_FALSE));
      return;
    }

  /* TODO: add condition when one of arguments is symbolic.  */
}


bool
state::check_const_bit_is_less_than (vec<value *> * arg1_bits,
				     vec<value *> * arg2_bits) const
{
  for (int i = arg1_bits->length () - 1; i >= 0; i--)
    {
      if ((*arg1_bits)[i] < (*arg2_bits)[i])
	return true;
      else if ((*arg1_bits)[i] > (*arg2_bits)[i])
	return false;
    }
  return false;
}


void
state::add_less_than_cond (tree arg1, tree arg2)
{
  vec<value *> * arg1_bits = var_states.get (arg1);
  vec<value *> * arg2_bits = var_states.get (arg2);

  if (is_bit_vector (arg1_bits) && is_bit_vector (arg2_bits))
    {
      bool result = check_const_bit_is_less_than (arg1_bits, arg2_bits);
      conditions.add (new bit_condition (nullptr, nullptr,
					 result ? condition_type::IS_TRUE
						: condition_type::IS_FALSE));
      return;
    }

  /* TODO: add condition when one of arguments is symbolic.  */
}


void
state::add_greater_or_equal_cond (tree arg1, tree arg2)
{
  vec<value *> * arg1_bits = var_states.get (arg1);
  vec<value *> * arg2_bits = var_states.get (arg2);

  if (is_bit_vector (arg1_bits) && is_bit_vector (arg2_bits))
    {
      bool is_greater_than = check_const_bit_is_greater_than (arg1_bits,
							      arg2_bits);
      bool is_equal = check_const_bit_equality (arg1_bits, arg2_bits);
      conditions.add (new bit_condition (nullptr, nullptr,
					 (is_greater_than | is_equal)
					 ? condition_type::IS_TRUE
					 : condition_type::IS_FALSE));
      return;
    }

  /* TODO: add condition when one of arguments is symbolic.  */
}


void
state::add_less_or_equal_cond (tree arg1, tree arg2)
{
  vec<value *> * arg1_bits = var_states.get (arg1);
  vec<value *> * arg2_bits = var_states.get (arg2);

  if (is_bit_vector (arg1_bits) && is_bit_vector (arg2_bits))
    {
      bool is_less_than = check_const_bit_is_less_than (arg1_bits, arg2_bits);
      bool is_equal = check_const_bit_equality (arg1_bits, arg2_bits);
      conditions.add (new bit_condition (nullptr, nullptr,
					 (is_less_than | is_equal)
					 ? condition_type::IS_TRUE
					 : condition_type::IS_FALSE));
      return;
    }

  /* TODO: add condition when one of arguments is symbolic.  */
}


void
state::add_bool_cond (tree arg)
{
  vec<value *> * arg_bits = var_states.get (arg);
  condition_type result = condition_type::IS_FALSE;
  if (is_bit_vector (arg_bits))
    {
      for (size_t i = 0; i < arg_bits->length (); i++)
	if ((*arg_bits)[i] != 0)
	  {
	    result = condition_type::IS_TRUE;
	    break;
	  }
    }
  conditions.add (new bit_condition (nullptr, nullptr, result));

  /* TODO: add condition when one of arguments is symbolic.  */
}
