/* Build expressions with type checking for CHILL compiler.
   Copyright (C) 1992, 1993, 1994, 1998, 1999, 2000
   Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


/* This file is part of the CHILL front end.
   It contains routines to build C expressions given their operands,
   including computing the modes of the result, C-specific error checks,
   and some optimization.

   There are also routines to build RETURN_STMT nodes and CASE_STMT nodes,
   and to process initializations in declarations (since they work
   like a strange sort of assignment).  */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "ch-tree.h"
#include "flags.h"
#include "rtl.h"
#include "expr.h"
#include "lex.h"
#include "toplev.h"
#include "output.h"

/* forward declarations */
static int chill_l_equivalent PARAMS ((tree, tree, struct mode_chain*));
static tree extract_constant_from_buffer PARAMS ((tree, const unsigned char *, int));
static int expand_constant_to_buffer PARAMS ((tree, unsigned char *, int));
static tree build_empty_string PARAMS ((tree));
static tree make_chill_pointer_type PARAMS ((tree, enum tree_code));
static tree make_chill_range_type PARAMS ((tree, tree, tree));
static void apply_chill_array_layout PARAMS ((tree));
static int field_decl_cmp PARAMS ((tree *, tree*));
static tree make_chill_struct_type PARAMS ((tree));
static int apply_chill_field_layout PARAMS ((tree, int *));

/*
 * This function checks an array access.
 * It calls error (ERROR_MESSAGE) if the condition (index <= domain max value
 *                                     index >= domain min value)
 *                   is not met at compile time,
 *         If a runtime test is required and permitted,
 *         check_expression is used to do so.
 * the global RANGE_CHECKING flags controls the
 * generation of runtime checking code.
 */
tree
valid_array_index_p (array, idx, error_message, is_varying_lhs)
     tree array, idx;
     const char *error_message;
     int is_varying_lhs;
{
  tree cond, low_limit, high_cond, atype, domain;
  tree orig_index = idx;
  enum chill_tree_code condition;

  if (array == NULL_TREE || TREE_CODE (array) == ERROR_MARK
      || idx == NULL_TREE || TREE_CODE (idx) == ERROR_MARK)
    return error_mark_node;
  
  if (TREE_CODE (idx) == TYPE_DECL
      || TREE_CODE_CLASS (TREE_CODE (idx)) == 't')
    {
      error ("array or string index is a mode (instead of a value)");
      return error_mark_node;
    }

  atype = TREE_TYPE (array);

  if (chill_varying_type_p (atype))
    {
      domain = TYPE_DOMAIN (CH_VARYING_ARRAY_TYPE (atype));
      high_cond = build_component_ref (array, var_length_id);
      if (chill_varying_string_type_p (atype))
	{
	  if (is_varying_lhs)
	    condition = GT_EXPR;
	  else
	    condition = GE_EXPR;
	}
      else
	condition = GT_EXPR;
    }
  else
    {
      domain = TYPE_DOMAIN (atype);
      high_cond = TYPE_MAX_VALUE (domain);
      condition = GT_EXPR;
    }

  if (CH_STRING_TYPE_P (atype))
    {
      if (! CH_SIMILAR (TREE_TYPE (orig_index), integer_type_node))
	{
	  error ("index is not an integer expression");
	  return error_mark_node;
	}
    }
  else
    {
      if (! CH_COMPATIBLE (orig_index, domain))
	{
	  error ("index not compatible with index mode");
	  return error_mark_node;
	}
    }

  /* Convert BOOLS(1) to BOOL and CHARS(1) to CHAR. */
  if (flag_old_strings)
    {
      idx = convert_to_discrete (idx);
      if (idx == NULL) /* should never happen */
	error ("index is not discrete");
    }

  /* we know we'll refer to this value twice */
  if (range_checking)
    idx = save_expr (idx);

  low_limit = TYPE_MIN_VALUE (domain);
  high_cond = build_compare_discrete_expr (condition, idx, high_cond);

  /* an invalid index expression meets this condition */
  cond = fold (build (TRUTH_ORIF_EXPR, boolean_type_node,
	   build_compare_discrete_expr (LT_EXPR, idx, low_limit),
	     high_cond));

  /* strip a redundant NOP_EXPR */
  if (TREE_CODE (cond) == NOP_EXPR
      && TREE_TYPE (cond) == boolean_type_node
      && TREE_CODE (TREE_OPERAND (cond, 0)) == INTEGER_CST)
    cond = TREE_OPERAND (cond, 0);
      
  idx = convert (CH_STRING_TYPE_P (atype) ? integer_type_node : domain,
		 idx);

  if (TREE_CODE (cond) == INTEGER_CST)
    {
      if (tree_int_cst_equal (cond, boolean_false_node))
	return idx;       /* condition met at compile time */
      error ("%s", error_message); /* condition failed at compile time */
      return error_mark_node;
    }
  else if (range_checking)
    {
      /* FIXME: often, several of these conditions will
	 be generated for the same source file and line number.
	 A great optimization would be to share the
	 cause_exception function call among them rather
	 than generating a cause_exception call for each. */
      return check_expression (idx, cond,
			       ridpointers[(int) RID_RANGEFAIL]);
    }
  else
    return idx;           /* don't know at compile time */
}

/*
 * Extract a slice from an array, which could look like a
 * SET_TYPE if it's a bitstring.  The array could also be VARYING
 * if the element type is CHAR.  The min_value and length values 
 * must have already been checked with valid_array_index_p.  No 
 * checking is done here.
 */
tree
build_chill_slice (array, min_value, length)
     tree array, min_value, length;
{
  tree result;
  tree array_type = TREE_TYPE (array);

  if (!CH_REFERABLE (array) && TREE_CODE (array) != SAVE_EXPR
      && (TREE_CODE (array) != COMPONENT_REF
	   || TREE_CODE (TREE_OPERAND (array, 0)) != SAVE_EXPR))
    {
      if (!TREE_CONSTANT (array))
	warning ("possible internal error - slice argument is neither referable nor constant");
      else
	{
	  /* Force to storage.
	     NOTE:  This could mean multiple identical copies of
	     the same constant.  FIXME. */
	  tree mydecl = decl_temp1 (get_unique_identifier("SLICEE"),
				    array_type, 1, array, 0, 0);
	  TREE_READONLY (mydecl) = 1;
	  /* mark_addressable (mydecl); FIXME: necessary? */
	  array = mydecl;
	}
    }

  /*
     The code-generation which uses a slice tree needs not only to
     know the dynamic upper and lower limits of that slice, but the
     original static allocation, to use to build temps where one or both
     of the dynamic limits must be calculated at runtime..  We pass the
     dynamic size by building a new array_type whose limits are the
     min_value and min_value + length values passed to us.  
     
     The static allocation info is passed by using the parent array's
     limits to compute a temp_size, which is passed in the lang_specific
     field of the slice_type. */
     
  if (TREE_CODE (array_type) == ARRAY_TYPE)
    {
      tree domain_type = TYPE_DOMAIN (array_type);
      tree domain_min = TYPE_MIN_VALUE (domain_type);
      tree domain_max
	= fold (build (PLUS_EXPR, domain_type,
		       domain_min,
		       fold (build (MINUS_EXPR, integer_type_node,
				    length, integer_one_node))));
      tree index_type = build_chill_range_type (TYPE_DOMAIN (array_type),
						domain_min,
						domain_max);

      tree element_type = TREE_TYPE (array_type);
      tree slice_type = build_simple_array_type (element_type, index_type, NULL_TREE);
      tree slice_pointer_type;
      tree max_size;

      if (CH_CHARS_TYPE_P (array_type))
	MARK_AS_STRING_TYPE (slice_type);
      else
	TYPE_PACKED (slice_type) = TYPE_PACKED (array_type);

      SET_CH_NOVELTY (slice_type, CH_NOVELTY (array_type));

      if (TREE_CONSTANT (array) && TREE_CODE (min_value) == INTEGER_CST
	  && TREE_CODE (length) == INTEGER_CST)
	{
	  int type_size = int_size_in_bytes (array_type);
	  unsigned char *buffer = (unsigned char*) alloca (type_size);
	  int delta = int_size_in_bytes (element_type)
	    * (TREE_INT_CST_LOW (min_value) - TREE_INT_CST_LOW (domain_min));
	  bzero (buffer, type_size);
	  if (expand_constant_to_buffer (array, buffer, type_size))
	    {
	      result = extract_constant_from_buffer (slice_type,
						     buffer + delta,
						     type_size - delta);
	      if (result)
		return result;
	    }
	}

      /* Kludge used by case CONCAT_EXPR in chill_expand_expr.
	 Set TYPE_ARRAY_MAX_SIZE to a constant upper bound on the
	 bytes needed. */
      max_size = size_in_bytes (slice_type);
      if (TREE_CODE (max_size) != INTEGER_CST)
	{
	  max_size = TYPE_ARRAY_MAX_SIZE (array_type);
	  if (max_size == NULL_TREE)
	    max_size = size_in_bytes (array_type);
	}
      TYPE_ARRAY_MAX_SIZE (slice_type) = max_size;

      mark_addressable (array);
      /* Contruct a SLICE_EXPR to represent a slice of a packed array of bits. */
      if (TYPE_PACKED (array_type))
	{
	  if (pass == 2 && TREE_CODE (length) != INTEGER_CST)
	    {
	      sorry ("bit array slice with non-constant length");
	      return error_mark_node;
	    }
	  if (domain_min && ! integer_zerop (domain_min))
	    min_value = size_binop (MINUS_EXPR, min_value,
				    convert (sizetype, domain_min));
	  result = build (SLICE_EXPR, slice_type, array, min_value, length);
	  TREE_READONLY (result)
	    = TREE_READONLY (array) | TYPE_READONLY (TREE_TYPE (array_type));
	  return result;
	}

      slice_pointer_type = build_chill_pointer_type (slice_type);
      if (TREE_CODE (min_value) == INTEGER_CST
	  && domain_min && TREE_CODE (domain_min) == INTEGER_CST
	  && compare_int_csts (EQ_EXPR, min_value, domain_min))
	result = fold (build1 (ADDR_EXPR, slice_pointer_type, array));
      else
	{
	  min_value = convert (sizetype, min_value);
	  if (domain_min && ! integer_zerop (domain_min))
	    min_value = size_binop (MINUS_EXPR, min_value,
				    convert (sizetype, domain_min));
	  min_value = size_binop (MULT_EXPR, min_value,
				  size_in_bytes (element_type));
	  result = fold (build (PLUS_EXPR, slice_pointer_type,
				build1 (ADDR_EXPR, slice_pointer_type,
					array),
				convert (slice_pointer_type, min_value)));
	}
      /* Return the final array value. */
      result = fold (build1 (INDIRECT_REF, slice_type, result));
      TREE_READONLY (result)
	= TREE_READONLY (array) | TYPE_READONLY (element_type);
      return result;
    }
  else if (TREE_CODE (array_type) == SET_TYPE)  /* actually a bitstring */
    {
      if (pass == 2 && TREE_CODE (length) != INTEGER_CST)
	{
	  sorry ("bitstring slice with non-constant length");
	  return error_mark_node;
	}
      result = build (SLICE_EXPR, build_bitstring_type (length),
		      array, min_value, length);
      TREE_READONLY (result)
	= TREE_READONLY (array) | TYPE_READONLY (TREE_TYPE (array_type));
      return result;
    }
  else if (chill_varying_type_p (array_type))
      return build_chill_slice (varying_to_slice (array), min_value, length);
  else
    {
      error ("slice operation on non-array, non-bitstring value not supported");
      return error_mark_node;
    }
}

static tree
build_empty_string (type)
     tree type;
{
  int orig_pass = pass;
  tree range, result;

  range = build_chill_range_type (type, integer_zero_node,
				  integer_minus_one_node);
  result = build_chill_array_type (type,
	     tree_cons (NULL_TREE, range, NULL_TREE), 0, NULL_TREE);
  pass = 2;
  range = build_chill_range_type (type, integer_zero_node,
				  integer_minus_one_node);
  result = build_chill_array_type (type,
	     tree_cons (NULL_TREE, range, NULL_TREE), 0, NULL_TREE);
  pass = orig_pass;

  return decl_temp1 (get_unique_identifier ("EMPTY_STRING"),
		     result, 0, NULL_TREE, 0, 0);
}

/* We build the runtime range-checking as a separate list
 * rather than making a compound_expr with min_value
 * (for example), to control when that comparison gets 
 * generated.  We cannot allow it in a TYPE_MAX_VALUE or
 * TYPE_MIN_VALUE expression, for instance, because that code 
 * will get generated when the slice is laid out, which would 
 * put it outside the scope of an exception handler for the 
 * statement we're generating.  I.e. we would be generating
 * cause_exception calls which might execute before the
 * necessary ch_link_handler call.
 */
tree
build_chill_slice_with_range (array, min_value, max_value)
     tree array, min_value, max_value;
{
  if (array == NULL_TREE || TREE_CODE (array) == ERROR_MARK
      || min_value == NULL_TREE || TREE_CODE(min_value) == ERROR_MARK
      || max_value == NULL_TREE || TREE_CODE(max_value) == ERROR_MARK)
    return error_mark_node;

  if (TREE_TYPE (array) == NULL_TREE
      || (TREE_CODE (TREE_TYPE (array)) != ARRAY_TYPE
	  && TREE_CODE (TREE_TYPE (array)) != SET_TYPE
	  && !chill_varying_type_p (TREE_TYPE (array))))
    {
      error ("can only take slice of array or string");
      return error_mark_node;
    }

  array = save_if_needed (array);

  /* FIXME: test here for max_value >= min_value, except
     for max_value == -1, min_value == 0 (empty string) */
  min_value = valid_array_index_p (array, min_value,
				   "slice lower limit out-of-range", 0);
  if (TREE_CODE (min_value) == ERROR_MARK)
    return min_value;

  /* FIXME: suppress this test if max_value is the LENGTH of a 
     varying array, which has presumably already been checked. */
  max_value = valid_array_index_p (array, max_value,
				   "slice upper limit out-of-range", 0);
  if (TREE_CODE (max_value) == ERROR_MARK)
    return error_mark_node;

  if (TREE_CODE (min_value) == INTEGER_CST
      && TREE_CODE (max_value) == INTEGER_CST
      && tree_int_cst_lt (max_value, min_value))
    return build_empty_string (TREE_TYPE (TREE_TYPE (array)));

  return
    build_chill_slice
      (array, min_value,
       save_expr (fold (build (PLUS_EXPR, integer_type_node,
			       fold (build (MINUS_EXPR, integer_type_node,
					    max_value, min_value)),
			       integer_one_node))));
}

tree
build_chill_slice_with_length (array, min_value, length)
     tree array, min_value, length;
{
  tree max_index;
  tree cond, high_cond, atype;

  if (array == NULL_TREE || TREE_CODE (array) == ERROR_MARK
      || min_value == NULL_TREE || TREE_CODE(min_value) == ERROR_MARK
      || length == NULL_TREE || TREE_CODE(length) == ERROR_MARK)
    return error_mark_node;

  if (TREE_TYPE (array) == NULL_TREE
      || (TREE_CODE (TREE_TYPE (array)) != ARRAY_TYPE
	  && TREE_CODE (TREE_TYPE (array)) != SET_TYPE
	  && !chill_varying_type_p (TREE_TYPE (array))))
    {
      error ("can only take slice of array or string");
      return error_mark_node;
    }

  if (TREE_CONSTANT (length) 
      && tree_int_cst_lt (length, integer_zero_node))
    return build_empty_string (TREE_TYPE (TREE_TYPE (array)));

  array = save_if_needed (array);
  min_value = save_expr (min_value);
  length = save_expr (length);

  if (! CH_SIMILAR (TREE_TYPE (length), integer_type_node))
    {
      error ("slice length is not an integer");
      length = integer_one_node;
    }

  max_index = fold (build (MINUS_EXPR, integer_type_node,
			   fold (build (PLUS_EXPR, integer_type_node,
					length, min_value)),
			   integer_one_node));
  max_index = convert_to_class (chill_expr_class (min_value), max_index);

  min_value = valid_array_index_p (array, min_value,
				   "slice start index out-of-range", 0);
  if (TREE_CODE (min_value) == ERROR_MARK)
    return error_mark_node;

  atype = TREE_TYPE (array);

  if (chill_varying_type_p (atype))
    high_cond = build_component_ref (array, var_length_id);
  else
    high_cond = TYPE_MAX_VALUE (TYPE_DOMAIN (atype));

  /* an invalid index expression meets this condition */
  cond = fold (build (TRUTH_ORIF_EXPR, boolean_type_node,
		      build_compare_discrete_expr (LT_EXPR,
						   length, integer_zero_node),
		      build_compare_discrete_expr (GT_EXPR,
						   max_index, high_cond)));

  if (TREE_CODE (cond) == INTEGER_CST)
    {
      if (! tree_int_cst_equal (cond, boolean_false_node))
	{
	  error ("slice length out-of-range");
	  return error_mark_node;
	}
	  
    }
  else if (range_checking)
    {
      min_value = check_expression (min_value, cond,
				    ridpointers[(int) RID_RANGEFAIL]);
    }

  return build_chill_slice (array, min_value, length);
}

tree
build_chill_array_ref (array, indexlist)
     tree array, indexlist;
{
  tree idx;

  if (array == NULL_TREE || TREE_CODE (array) == ERROR_MARK)
    return error_mark_node;
  if (indexlist == NULL_TREE || TREE_CODE (indexlist) == ERROR_MARK)
    return error_mark_node;

  idx = TREE_VALUE (indexlist);   /* handle first index */

  idx = valid_array_index_p (array, idx,
			     "array index out-of-range", 0);
  if (TREE_CODE (idx) == ERROR_MARK)
    return error_mark_node;

  array = build_chill_array_ref_1 (array, idx);

  if (array && TREE_CODE (array) != ERROR_MARK 
      && TREE_CHAIN (indexlist))
    {
      /* Z.200 (1988) section 4.2.8 says that:
	 <array> '(' <expression {',' <expression> }* ')'
	 is derived syntax (i.e. syntactic sugar) for:
	 <array> '(' <expression ')' { '(' <expression> ')' }*
	 The intent is clear if <array> has mode: ARRAY (...) ARRAY (...) XXX.
	 But what if <array> has mode: ARRAY (...) CHARS (N)
	 or: ARRAY (...) BOOLS (N).
	 Z.200 doesn't explicitly prohibit it, but the intent is unclear.
	 We'll allow it, since it seems reasonable and useful.
	 However, we won't allow it if <array> is:
	 ARRAY (...) PROC (...).
	 (The latter would make sense if we allowed general
	 Currying, which Chill doesn't.)  */
      if (TREE_CODE (TREE_TYPE (array)) == ARRAY_TYPE
	  || chill_varying_type_p (TREE_TYPE (array))
	  || CH_BOOLS_TYPE_P (TREE_TYPE (array)))
	array = build_generalized_call (array, TREE_CHAIN (indexlist));
      else
	error ("too many index expressions");
    }
  return array;
}

/*
 * Don't error check the index in here.  It's supposed to be 
 * checked by the caller.
 */
tree
build_chill_array_ref_1 (array, idx)
     tree array, idx;
{
  tree type;
  tree domain;
  tree rval;

  if (array == NULL_TREE || TREE_CODE (array) == ERROR_MARK
      || idx == NULL_TREE || TREE_CODE (idx) == ERROR_MARK)
    return error_mark_node;

  if (chill_varying_type_p (TREE_TYPE (array)))
    array = varying_to_slice (array);

  domain = TYPE_DOMAIN (TREE_TYPE (array));

#if 0
  if (! integer_zerop (TYPE_MIN_VALUE (domain)))
    {
      /* The C part of the compiler doesn't understand how to do
	 arithmetic with dissimilar enum types.  So we check compatability
	 here, and perform the math in INTEGER_TYPE.  */
      if (TREE_CODE (TREE_TYPE (idx)) == ENUMERAL_TYPE
	  && chill_comptypes (TREE_TYPE (idx), domain, 0))
	idx = convert (TREE_TYPE (TYPE_MIN_VALUE (domain)), idx);
      idx = build_binary_op (MINUS_EXPR, idx, TYPE_MIN_VALUE (domain), 0);
    }
#endif

  if (CH_STRING_TYPE_P (TREE_TYPE (array)))
    {
      /* Could be bitstring or char string.  */
      if (TREE_TYPE (TREE_TYPE (array)) == boolean_type_node)
	{
	  rval = build (SET_IN_EXPR, boolean_type_node, idx, array);
	  TREE_READONLY (rval) = TREE_READONLY (array);
	  return rval;
	}
    }

  if (!discrete_type_p (TREE_TYPE (idx)))
    {
      error ("array index is not discrete");
      return error_mark_node;
    }

  /* An array that is indexed by a non-constant
     cannot be stored in a register; we must be able to do
     address arithmetic on its address.
     Likewise an array of elements of variable size.  */
  if (TREE_CODE (idx) != INTEGER_CST
      || (TYPE_SIZE (TREE_TYPE (TREE_TYPE (array))) != 0
	  && TREE_CODE (TYPE_SIZE (TREE_TYPE (TREE_TYPE (array)))) != INTEGER_CST))
    {
      if (mark_addressable (array) == 0)
	return error_mark_node;
    }

  type = TREE_TYPE (TREE_TYPE (array));

  /* Do constant folding */
  if (TREE_CODE (idx) == INTEGER_CST && TREE_CONSTANT (array))
    {
      struct ch_class class;
      class.kind = CH_VALUE_CLASS;
      class.mode = type;

      if (TREE_CODE (array) == CONSTRUCTOR)
	{
	  tree list = CONSTRUCTOR_ELTS (array);
	  for ( ; list != NULL_TREE; list = TREE_CHAIN (list))
	    {
	      if (tree_int_cst_equal (TREE_PURPOSE (list), idx))
		return convert_to_class (class, TREE_VALUE (list));
	    }
	}
      else if (TREE_CODE (array) == STRING_CST
	       && CH_CHARS_TYPE_P (TREE_TYPE (array)))
	{
	  HOST_WIDE_INT i = TREE_INT_CST_LOW (idx);
	  if (i >= 0 && i < TREE_STRING_LENGTH (array))
	    {
	      char ch = TREE_STRING_POINTER (array) [i];
	      return convert_to_class (class,
				       build_int_2 ((unsigned char)ch, 0));
	    }
	}
    }

  if (TYPE_PACKED (TREE_TYPE (array)))
    rval = build (PACKED_ARRAY_REF, type, array, idx);
  else
    rval = build (ARRAY_REF, type, array, idx);

  /* Array ref is const/volatile if the array elements are
     or if the array is.  */
  TREE_READONLY (rval) = TREE_READONLY (array) | TYPE_READONLY (type);
  TREE_SIDE_EFFECTS (rval)
    |= (TYPE_VOLATILE (TREE_TYPE (TREE_TYPE (array)))
	| TREE_SIDE_EFFECTS (array));
  TREE_THIS_VOLATILE (rval)
    |= (TYPE_VOLATILE (TREE_TYPE (TREE_TYPE (array)))
	/* This was added by rms on 16 Nov 91.
	   It fixes  vol struct foo *a;  a->elts[1] 
	   in an inline function.
	   Hope it doesn't break something else.  */
	| TREE_THIS_VOLATILE (array));
  return fold (rval);
}

tree
build_chill_bitref (bitstring, indexlist)
     tree bitstring, indexlist;
{
  if (TREE_CODE (bitstring) == ERROR_MARK)
    return bitstring;
  if (TREE_CODE (indexlist) == ERROR_MARK)
    return indexlist;

  if (TREE_CHAIN (indexlist) != NULL_TREE)
    {
      error ("invalid compound index for bitstring mode");
      return error_mark_node;
    }

  if (TREE_CODE (indexlist) == TREE_LIST)
    {
      tree result = build (SET_IN_EXPR, boolean_type_node,
			   TREE_VALUE (indexlist), bitstring);
      TREE_READONLY (result) = TREE_READONLY (bitstring);
      return result;
    }
  else abort ();
}


int
discrete_type_p (type)
     tree type;
{
  return INTEGRAL_TYPE_P (type);
}

/* Checks that EXP has discrete type, or can be converted to discrete.
   Otherwise, returns NULL_TREE.
   Normally returns the (possibly-converted) EXP. */

tree
convert_to_discrete (exp)
     tree exp;
{
  if (! discrete_type_p (TREE_TYPE (exp)))
    {
      if (flag_old_strings)
	{
	  if (CH_CHARS_ONE_P (TREE_TYPE (exp)))
	    return convert (char_type_node, exp);
	  if (CH_BOOLS_ONE_P (TREE_TYPE (exp)))
	    return convert (boolean_type_node, exp);
	}
      return NULL_TREE;
    }
  return exp;
}

/* Write into BUFFER the target-machine representation of VALUE.
   Returns 1 on success, or 0 on failure. (Either the VALUE was
   not constant, or we don't know how to do the conversion.) */

static int
expand_constant_to_buffer (value, buffer, buf_size)
     tree value;
     unsigned char *buffer; 
     int buf_size;
{
  tree type = TREE_TYPE (value);
  int size = int_size_in_bytes (type);
  int i;
  if (size < 0 || size > buf_size)
    return 0;
  switch (TREE_CODE (value))
    {
    case INTEGER_CST:
      {
	HOST_WIDE_INT lo = TREE_INT_CST_LOW (value);
	HOST_WIDE_INT hi = TREE_INT_CST_HIGH (value);
	for (i = 0; i < size; i++)
	  {
	    /* Doesn't work if host and target BITS_PER_UNIT differ. */
	    unsigned char byte = lo & ((1 << BITS_PER_UNIT) - 1);
	    if (BYTES_BIG_ENDIAN)
	      buffer[size - i - 1] = byte;
	    else
	      buffer[i] = byte;
	    rshift_double (lo, hi, BITS_PER_UNIT, BITS_PER_UNIT * size,
			   &lo, &hi, 0);
	  }
      }
      break;
    case STRING_CST:
      {
	size = TREE_STRING_LENGTH (value);
	if (size > buf_size)
	  return 0;
	bcopy (TREE_STRING_POINTER (value), buffer, size);
	break;
      }
    case CONSTRUCTOR:
      if (TREE_CODE (type) == ARRAY_TYPE)
	{
	  tree element_type = TREE_TYPE (type);
	  int element_size = int_size_in_bytes (element_type);
	  tree list = CONSTRUCTOR_ELTS (value);
	  HOST_WIDE_INT next_index;
	  HOST_WIDE_INT min_index = 0;
	  if (element_size < 0)
	    return 0;

	  if (TYPE_DOMAIN (type) != 0)
	    {
	      tree min_val = TYPE_MIN_VALUE (TYPE_DOMAIN (type));
	      if (min_val)
		{
		  if (TREE_CODE (min_val) != INTEGER_CST)
		    return 0;
		  else
		    min_index = TREE_INT_CST_LOW (min_val);
		}
	    }

	  next_index = min_index;

	  for (; list != NULL_TREE; list = TREE_CHAIN (list))
	    {
	      HOST_WIDE_INT offset;
	      HOST_WIDE_INT last_index;
	      tree purpose = TREE_PURPOSE (list);
	      if (purpose)
		{
		  if (TREE_CODE (purpose) == INTEGER_CST)
		    last_index = next_index = TREE_INT_CST_LOW (purpose);
		  else if (TREE_CODE (purpose) == RANGE_EXPR)
		    {
		      next_index = TREE_INT_CST_LOW (TREE_OPERAND(purpose, 0));
		      last_index = TREE_INT_CST_LOW (TREE_OPERAND(purpose, 1));
		    }
		  else
		    return 0;
		}
	      else
		last_index = next_index;
	      for ( ; next_index <= last_index; next_index++)
		{
		  offset = (next_index - min_index) * element_size;
		  if (!expand_constant_to_buffer (TREE_VALUE (list),
						  buffer + offset,
						  buf_size - offset))
		    return 0;
		}
	    }
	  break;
	}
      else if (TREE_CODE (type) == RECORD_TYPE)
	{
	  tree list = CONSTRUCTOR_ELTS (value);
	  for (; list != NULL_TREE; list = TREE_CHAIN (list))
	    {
	      tree field = TREE_PURPOSE (list);
	      HOST_WIDE_INT offset;
	      if (field == NULL_TREE || TREE_CODE (field) != FIELD_DECL)
		return 0;
	      if (DECL_BIT_FIELD (field))
		return 0;
	      offset = TREE_INT_CST_LOW (DECL_FIELD_BITPOS (field))
		/ BITS_PER_UNIT;
	      if (!expand_constant_to_buffer (TREE_VALUE (list),
					      buffer + offset,
					      buf_size - offset))
		return 0;
	    }
	  break;
	}
      else if (TREE_CODE (type) == SET_TYPE)
	{
	  if (get_set_constructor_bytes (value, buffer, buf_size)
	      != NULL_TREE)
	    return 0;
	}
      break;
    default:
      return 0;
    }
  return 1;
}

/* Given that BUFFER contains a target-machine representation of
   a value of type TYPE, return that value as a tree.
   Returns NULL_TREE on failure. (E.g. the TYPE might be variable size,
   or perhaps we don't know how to do the conversion.) */

static tree
extract_constant_from_buffer (type, buffer, buf_size)
     tree type;
     const unsigned char *buffer;
     int buf_size;
{
  tree value;
  int size = int_size_in_bytes (type);
  int i;
  if (size < 0 || size > buf_size)
    return 0;
  switch (TREE_CODE (type))
    {
    case INTEGER_TYPE:
    case CHAR_TYPE:
    case BOOLEAN_TYPE:
    case ENUMERAL_TYPE:
    case POINTER_TYPE:
      {
	HOST_WIDE_INT lo = 0, hi = 0;
	/* Accumulate (into (lo,hi) the bytes (from buffer). */
	for (i = size; --i >= 0; )
	  {
	    unsigned char byte;
	    /* Get next byte (in big-endian order). */
	    if (BYTES_BIG_ENDIAN)
	      byte = buffer[size - i - 1];
	    else
	      byte = buffer[i];
	    lshift_double (lo, hi, BITS_PER_UNIT, TYPE_PRECISION (type),
			   &lo, &hi, 0);
	    add_double (lo, hi, byte, 0, &lo, &hi);
	  }
	value = build_int_2 (lo, hi);
	TREE_TYPE (value) = type;
	return value;
      }
    case ARRAY_TYPE:
      {
	tree element_type = TREE_TYPE (type);
	int element_size = int_size_in_bytes (element_type);
	tree list = NULL_TREE;
	HOST_WIDE_INT min_index = 0, max_index, cur_index;
	if (element_size == 1 && CH_CHARS_TYPE_P (type))
	  {
	    value = build_string (size, buffer);
	    CH_DERIVED_FLAG (value) = 1;
	    TREE_TYPE (value) = type;
	    return value;
	  }
	if (TYPE_DOMAIN (type) == 0)
	  return 0;
	value = TYPE_MIN_VALUE (TYPE_DOMAIN (type));
	if (value)
	  {
	    if (TREE_CODE (value) != INTEGER_CST)
	      return 0;
	    else
	      min_index = TREE_INT_CST_LOW (value);
	  }
	value = TYPE_MAX_VALUE (TYPE_DOMAIN (type));
	if (value == NULL_TREE || TREE_CODE (value) != INTEGER_CST)
	  return 0;
	else
	  max_index = TREE_INT_CST_LOW (value);
	for (cur_index = max_index; cur_index >= min_index; cur_index--)
	  {
	    HOST_WIDE_INT offset = (cur_index - min_index) * element_size;
	    value = extract_constant_from_buffer (element_type,
						  buffer + offset,
						  buf_size - offset);
	    if (value == NULL_TREE)
	      return NULL_TREE;
	    list = tree_cons (build_int_2 (cur_index, 0), value, list);
	  }
	value = build (CONSTRUCTOR, type, NULL_TREE, list);
	TREE_CONSTANT (value) = 1;
	TREE_STATIC (value) = 1;
	return value;
      }
    case RECORD_TYPE:
      {
	tree list = NULL_TREE;
	tree field = TYPE_FIELDS (type);
	for (; field != NULL_TREE; field = TREE_CHAIN (field))
	  {
	    HOST_WIDE_INT offset
	      = TREE_INT_CST_LOW (DECL_FIELD_BITPOS (field)) / BITS_PER_UNIT;
	    if (DECL_BIT_FIELD (field))
	      return 0;
	    value = extract_constant_from_buffer (TREE_TYPE (field),
						  buffer + offset,
						  buf_size - offset);
	    if (value == NULL_TREE)
	      return NULL_TREE;
	    list = tree_cons (field, value, list);
	  }
	value = build (CONSTRUCTOR, type, NULL_TREE, nreverse (list));
	TREE_CONSTANT (value) = 1;
	TREE_STATIC (value) = 1;
	return value;
      }

    case UNION_TYPE:
      {
	tree longest_variant = NULL_TREE;
	int longest_size = 0;
	tree field = TYPE_FIELDS (type);
	
	/* This is a kludge.  We assume that converting the data to te
	   longest variant will provide valid data for the "correct"
	   variant.  This is usually the case, but is not guaranteed.
	   For example, the longest variant may include holes.
	   Also incorrect interpreting the given value as the longest
	   variant may confuse the compiler if that should happen
	   to yield invalid values.  ??? */

	for (; field != NULL_TREE; field = TREE_CHAIN (field))
	  {
	    int size = TREE_INT_CST_LOW (size_in_bytes (TREE_TYPE (field)));
	    
	    if (size > longest_size)
	      {
		longest_size = size;
		longest_variant = field;
	      }
	  }
	if (longest_variant == NULL_TREE)
	  return NULL_TREE;
	return extract_constant_from_buffer (TREE_TYPE (longest_variant), buffer, buf_size);
      }

    case SET_TYPE:
      {
	tree list = NULL_TREE;
	int i;
	HOST_WIDE_INT min_index, max_index;
	if (TYPE_DOMAIN (type) == 0)
	  return 0;
	value = TYPE_MIN_VALUE (TYPE_DOMAIN (type));
	if (value == NULL_TREE)
	  min_index = 0;
	else if (TREE_CODE (value) != INTEGER_CST)
	  return 0;
	else
	  min_index = TREE_INT_CST_LOW (value);
	value = TYPE_MAX_VALUE (TYPE_DOMAIN (type));
	if (value == NULL_TREE)
	  max_index = 0;
	else if (TREE_CODE (value) != INTEGER_CST)
	  return 0;
	else
	  max_index = TREE_INT_CST_LOW (value);
	for (i = max_index + 1 - min_index; --i >= 0; )
	  {
	    unsigned char byte = (unsigned char)buffer[i / BITS_PER_UNIT];
	    unsigned bit_pos = (unsigned)i % (unsigned)BITS_PER_UNIT;
	    if (BYTES_BIG_ENDIAN
		? (byte & (1 << (BITS_PER_UNIT - 1 - bit_pos)))
		: (byte & (1 << bit_pos)))
	      list = tree_cons (NULL_TREE,
				build_int_2 (i + min_index, 0), list);
	  }
	value = build (CONSTRUCTOR, type, NULL_TREE, list);
	TREE_CONSTANT (value) = 1;
	TREE_STATIC (value) = 1;
	return value;
      }

    default:
      return NULL_TREE;
    }
}

tree
build_chill_cast (type, expr)
     tree type, expr;
{
  tree expr_type;
  int  expr_type_size;
  int  type_size;
  int  type_is_discrete;
  int  expr_type_is_discrete;

  if (type == NULL_TREE || TREE_CODE (type) == ERROR_MARK)
    return error_mark_node;
  if (expr == NULL_TREE || TREE_CODE (expr) == ERROR_MARK)
    return error_mark_node;

  /* if expression was untyped because of its context (an
     if_expr or case_expr in a tuple, perhaps) just apply
     the type */
  expr_type = TREE_TYPE (expr);
  if (expr_type == NULL_TREE
      || TREE_CODE (expr_type) == ERROR_MARK)
    return convert (type, expr);

  if (expr_type == type)
    return expr;

  expr_type_size = int_size_in_bytes (expr_type);
  type_size      = int_size_in_bytes (type);

  if (expr_type_size == -1)
    {
      error ("conversions from variable_size value");
      return error_mark_node;
    }
  if (type_size == -1)
    {
      error ("conversions to variable_size mode");
      return error_mark_node;
    }

  /* FIXME: process REAL ==> INT && INT ==> REAL && REAL ==> REAL. I hope this is correct. */
  if ((TREE_CODE (expr_type) == INTEGER_TYPE && TREE_CODE (type) == REAL_TYPE) ||
      (TREE_CODE (expr_type) == REAL_TYPE && TREE_CODE (type) == INTEGER_TYPE) ||
      (TREE_CODE (expr_type) == REAL_TYPE && TREE_CODE (type) == REAL_TYPE))
    return convert (type, expr);

  /* FIXME: Don't know if this is correct */
  /* Don't allow conversions to or from REAL with others then integer */
  if (TREE_CODE (type) == REAL_TYPE)
    {
      error ("cannot convert to float");
      return error_mark_node;
    }
  else if (TREE_CODE (expr_type) == REAL_TYPE)
    {
      error ("cannot convert float to this mode");
      return error_mark_node;
    }

  if (expr_type_size == type_size && CH_REFERABLE (expr))
    goto do_location_conversion;

  type_is_discrete
    = discrete_type_p (type) || TREE_CODE (type) == POINTER_TYPE;
  expr_type_is_discrete
    = discrete_type_p (expr_type) || TREE_CODE (expr_type) == POINTER_TYPE;
  if (expr_type_is_discrete && type_is_discrete)
    {
      /* do an overflow check
	 FIXME: is this always neccessary ??? */
      /* FIXME: don't do range chacking when target type is PTR.
	 PTR doesn't have MIN and MAXVALUE. result is sigsegv. */
      if (range_checking && type != ptr_type_node)
	{
	  tree tmp = expr;

	  STRIP_NOPS (tmp);
	  if (TREE_CONSTANT (tmp) && TREE_CODE (tmp) != ADDR_EXPR)
	    {
	      if (compare_int_csts (LT_EXPR, tmp, TYPE_MIN_VALUE (type)) ||
		  compare_int_csts (GT_EXPR, tmp, TYPE_MAX_VALUE (type)))
		{
		  error ("OVERFLOW in expression conversion");
		  return error_mark_node;
		}
	    }
	  else
	    {
	      int cond1 = tree_int_cst_lt (TYPE_SIZE (type),
					   TYPE_SIZE (expr_type));
	      int cond2 = TREE_UNSIGNED (type) && (! TREE_UNSIGNED (expr_type));
	      int cond3 = (! TREE_UNSIGNED (type))
		&& TREE_UNSIGNED (expr_type)
		&& tree_int_cst_equal (TYPE_SIZE (type),
				       TYPE_SIZE (expr_type));
	      int cond4 = TREE_TYPE (type) && type_is_discrete;

	      if (cond1 || cond2 || cond3 || cond4)
		{
		  tree type_min = TYPE_MIN_VALUE (type);
		  tree type_max = TYPE_MAX_VALUE (type);
  
		  expr = save_if_needed (expr);
		  if (expr && type_min && type_max)
		    {
		      tree check = test_range (expr, type_min, type_max);
		      if (!integer_zerop (check))
			{
			  if (current_function_decl == NULL_TREE)
			    {
			      if (TREE_CODE (check) == INTEGER_CST)
				error ("overflow (not inside function)");
			      else
				warning ("possible overflow (not inside function)");
			    }
			  else
			    {
			      if (TREE_CODE (check) == INTEGER_CST)
				warning ("expression will always cause OVERFLOW");
			      expr = check_expression (expr, check,
						       ridpointers[(int) RID_OVERFLOW]);
			    }
			}
		    }
		}
	    }
	}
      return convert (type, expr);
    }

  if (TREE_CODE (expr) == INTEGER_CST && expr_type_size != type_size)
    {
      /* There should probably be a pedwarn here ... */
      tree itype = type_for_size (type_size * BITS_PER_UNIT, 1);
      if (itype)
	{
	  expr = convert (itype, expr);
	  expr_type = TREE_TYPE (expr);
	  expr_type_size= type_size;
	}
    }

  /* If expr is a constant of the right size, use it to to
     initialize a static variable. */
  if (expr_type_size == type_size && TREE_CONSTANT (expr) && !pedantic)
    {
      unsigned char *buffer = (unsigned char*) alloca (type_size);
      tree value;
      bzero (buffer, type_size);
      if (!expand_constant_to_buffer (expr, buffer, type_size))
	{
	  error ("not implemented: constant conversion from that kind of expression");
	  return error_mark_node;
	}
      value = extract_constant_from_buffer (type, buffer, type_size);
      if (value == NULL_TREE)
	{
	  error ("not implemented: constant conversion to that kind of mode");
	  return error_mark_node;
	}
      return value;
    }

  if (!CH_REFERABLE (expr) && expr_type_size == type_size)
    {
      tree temp = decl_temp1 (get_unique_identifier ("CAST"),
			      TREE_TYPE (expr), 0, 0, 0, 0);
      tree convert1 = build_chill_modify_expr (temp, expr);
      pedwarn ("non-standard, non-portable value conversion");
      return build (COMPOUND_EXPR, type, convert1,
		    build_chill_cast (type, temp));
    }

  if (CH_REFERABLE (expr) && expr_type_size != type_size)
    error ("location conversion between differently-sized modes");
  else
    error ("unsupported value conversion");
  return error_mark_node;

 do_location_conversion:
  /* To avoid confusing other parts of gcc,
     represent this as the C expression: *(TYPE*)EXPR. */
  mark_addressable (expr);
  expr = build1 (INDIRECT_REF, type,
		 build1 (NOP_EXPR, build_pointer_type (type),
			 build1 (ADDR_EXPR, build_pointer_type (expr_type),
				 expr)));
  TREE_READONLY (expr) = TYPE_READONLY (type);
  return expr;
}

/* Given a set_type, build an integer array from it that C will grok. */

tree
build_array_from_set (type)
     tree type;
{
  tree bytespint, bit_array_size, int_array_count;
 
  if (type == NULL_TREE || type == error_mark_node
      || TREE_CODE (type) != SET_TYPE)
    return error_mark_node;

  /* ??? Should this really be *HOST*??  */
  bytespint = size_int (HOST_BITS_PER_INT / HOST_BITS_PER_CHAR);
  bit_array_size = size_in_bytes (type);
  int_array_count = size_binop (TRUNC_DIV_EXPR, bit_array_size, bytespint);
  if (integer_zerop (int_array_count))
    int_array_count = size_one_node;
  type = build_array_type (integer_type_node, 
			   build_index_type (int_array_count));
  return type;
}


tree
build_chill_bin_type (size)
     tree size;
{
#if 0
  int isize;

  if (TREE_CODE (size) != INTEGER_CST
      || (isize = TREE_INT_CST_LOW (size), isize <= 0))
    {
      error ("operand to bin must be a non-negative integer literal");
      return error_mark_node;
    }
  if (isize <= TYPE_PRECISION (unsigned_char_type_node))
    return unsigned_char_type_node;
  if (isize <= TYPE_PRECISION (short_unsigned_type_node))
    return short_unsigned_type_node;
  if (isize <= TYPE_PRECISION (unsigned_type_node))
    return unsigned_type_node;
  if (isize <= TYPE_PRECISION (long_unsigned_type_node))
    return long_unsigned_type_node;
  if (isize <= TYPE_PRECISION (long_long_unsigned_type_node))
    return long_long_unsigned_type_node;
  error ("size %d of BIN too big - no such integer mode", isize);
  return error_mark_node;
#endif
  tree bintype;
 
  if (pass == 1)
    {
      bintype = make_node (INTEGER_TYPE);
      TREE_TYPE (bintype) = ridpointers[(int) RID_BIN];
      TYPE_MIN_VALUE (bintype) = size;
      TYPE_MAX_VALUE (bintype) = size;
    }
  else
    {
      error ("BIN in pass 2");
      return error_mark_node;
    }
  return bintype;
}

tree
chill_expand_tuple (type, constructor)
     tree type, constructor;
{
  const char *name;
  tree nonreft = type;

  if (TYPE_NAME (type) != NULL_TREE)
    {
      if (TREE_CODE (TYPE_NAME (type)) == IDENTIFIER_NODE)
	name = IDENTIFIER_POINTER (TYPE_NAME (type));
      else
	name = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type)));
    }
  else
    name = "";

  /* get to actual underlying type for digest_init */
  while (nonreft && TREE_CODE (nonreft) == REFERENCE_TYPE)
    nonreft = TREE_TYPE (nonreft);

  if (TREE_CODE (nonreft) == ARRAY_TYPE
      || TREE_CODE (nonreft) == RECORD_TYPE
      || TREE_CODE (nonreft) == SET_TYPE)
    return convert (nonreft, constructor);
  else
    {
      error ("mode of tuple is neither ARRAY, STRUCT, nor POWERSET");
      return error_mark_node;
    }
}

/* This function classifies an expr into the Null class,
   the All class, the M-Value, the M-derived, or the M-reference class.
   It probably has some inaccuracies. */

struct ch_class
chill_expr_class (expr)
     tree expr;
{
  struct ch_class class;
  /* The Null class contains the NULL pointer constant (only). */
  if (expr == null_pointer_node)
    {
      class.kind = CH_NULL_CLASS;
      class.mode = NULL_TREE;
      return class;
    }

  /* The All class contains the <undefined value> "*". */
  if (TREE_CODE (expr) == UNDEFINED_EXPR)
    {
      class.kind = CH_ALL_CLASS;
      class.mode = NULL_TREE;
      return class;
    }

  if (CH_DERIVED_FLAG (expr))
    {
      class.kind = CH_DERIVED_CLASS;
      class.mode = TREE_TYPE (expr);
      return class;
    }

  /* The M-Reference contains <references location> (address-of) expressions.
     Note that something that's been converted to a reference doesn't count. */
  if (TREE_CODE (expr) == ADDR_EXPR
      && TREE_CODE (TREE_TYPE (expr)) != REFERENCE_TYPE)
    {
      class.kind = CH_REFERENCE_CLASS;
      class.mode = TREE_TYPE (TREE_TYPE (expr));
      return class;
    }

  /* The M-Value class contains expressions with a known, specific mode M. */
  class.kind = CH_VALUE_CLASS;
  class.mode = TREE_TYPE (expr);
  return class;
}

/* Returns >= 1 iff REF is a location. Return 2 if it is referable. */

int chill_location (ref)
     tree ref;
{
  register enum tree_code code = TREE_CODE (ref);

  switch (code)
    {
    case REALPART_EXPR:
    case IMAGPART_EXPR:
    case ARRAY_REF:
    case PACKED_ARRAY_REF:
    case COMPONENT_REF:
    case NOP_EXPR: /* RETYPE_EXPR */
      return chill_location (TREE_OPERAND (ref, 0));
    case COMPOUND_EXPR:
      return chill_location (TREE_OPERAND (ref, 1));

    case BIT_FIELD_REF:
    case SLICE_EXPR:
      /* A bit-string slice is nor referable. */
      return chill_location (TREE_OPERAND (ref, 0)) == 0 ? 0 : 1;

    case CONSTRUCTOR:
    case STRING_CST:
      return 0;

    case INDIRECT_REF:
    case VAR_DECL:
    case PARM_DECL:
    case RESULT_DECL:
    case ERROR_MARK:
      if (TREE_CODE (TREE_TYPE (ref)) != FUNCTION_TYPE
	  && TREE_CODE (TREE_TYPE (ref)) != METHOD_TYPE)
	return 2;
      break;

    default:
      break;
    }
  return 0;
}

int
chill_referable (val)
     tree val;
{
  return chill_location (val) > 1;
}

/* Make a copy of MODE, but with the given NOVELTY. */

tree
copy_novelty (novelty, mode)
     tree novelty, mode;
{
  if (CH_NOVELTY (mode) != novelty)
    {
      mode = copy_node (mode);
      TYPE_MAIN_VARIANT (mode) = mode;
      TYPE_NEXT_VARIANT (mode) = 0;
      TYPE_POINTER_TO (mode) = 0;
      TYPE_REFERENCE_TO (mode) = 0;
      SET_CH_NOVELTY (mode, novelty);
    }
  return mode;
}


struct mode_chain
{
  struct mode_chain *prev;
  tree mode1, mode2;
};

/* Tests if MODE1 and MODE2 are SIMILAR.
   This is more or less as defined in the Blue Book, though
   see FIXME for parts that are unfinished.
   CHAIN is used to catch infinite recursion:  It is a list of pairs
   of mode arguments to calls to chill_similar "outer" to this call. */   

int
chill_similar (mode1, mode2, chain)
     tree mode1, mode2;
     struct mode_chain *chain;
{
  int varying1, varying2;
  tree t1, t2;
  struct mode_chain *link, node;
  if (mode1 == NULL_TREE || mode2 == NULL_TREE)
    return 0;

  while (TREE_CODE (mode1) == REFERENCE_TYPE)
    mode1 = TREE_TYPE (mode1);
  while (TREE_CODE (mode2) == REFERENCE_TYPE)
    mode2 = TREE_TYPE (mode2);

  /* Range modes are similar to their parent types. */
  while (TREE_CODE (mode1) == INTEGER_TYPE && TREE_TYPE (mode1) != NULL_TREE)
    mode1 = TREE_TYPE (mode1);
  while (TREE_CODE (mode2) == INTEGER_TYPE && TREE_TYPE (mode2) != NULL_TREE)
    mode2 = TREE_TYPE (mode2);

   
  /* see Z.200 sections 12.1.2.2 and 13.2 - all integer precisions 
     are similar to INT and to each other */
  if (mode1 == mode2 ||
      (TREE_CODE (mode1) == INTEGER_TYPE && TREE_CODE (mode2) == INTEGER_TYPE))
    return 1;

  /* This guards against certain kinds of recursion.
     For example:
     SYNMODE a = STRUCT ( next REF a );
     SYNMODE b = STRUCT ( next REF b );
     These moes are similar, but will get an infite recursion trying
     to prove that.  So, if we are recursing, assume the moes are similar.
     If they are not, we'll find some other discrepancy.  */
  for (link = chain; link != NULL; link = link->prev)
    {
      if (link->mode1 == mode1 && link->mode2 == mode2)
	return 1;
    }

  node.mode1 = mode1;
  node.mode2 = mode2;
  node.prev = chain;

  varying1 = chill_varying_type_p (mode1);
  varying2 = chill_varying_type_p (mode2);
  /* FIXME:  This isn't quite strict enough. */
  if ((varying1 && varying2)
      || (varying1 && TREE_CODE (mode2) == ARRAY_TYPE)
      || (varying2 && TREE_CODE (mode1) == ARRAY_TYPE))
    return 1;

  if (TREE_CODE(mode1) != TREE_CODE(mode2))
    {
      if (flag_old_strings)
	{
	  /* The recursion is to handle varying strings. */
	  if ((TREE_CODE (mode1) == CHAR_TYPE
	       && CH_SIMILAR (mode2, string_one_type_node))
	      || (TREE_CODE (mode2) == CHAR_TYPE
	       && CH_SIMILAR (mode1, string_one_type_node)))
	    return 1;
	  if ((TREE_CODE (mode1) == BOOLEAN_TYPE
	       && CH_SIMILAR (mode2, bitstring_one_type_node))
	      || (TREE_CODE (mode2) == BOOLEAN_TYPE
	       && CH_SIMILAR (mode1, bitstring_one_type_node)))
	    return 1;
	}
      if (TREE_CODE (mode1) == FUNCTION_TYPE
	  && TREE_CODE (mode2) == POINTER_TYPE
	  && TREE_CODE (TREE_TYPE (mode2)) == FUNCTION_TYPE)
	mode2 = TREE_TYPE (mode2);
      else if (TREE_CODE (mode2) == FUNCTION_TYPE
	  && TREE_CODE (mode1) == POINTER_TYPE
	  && TREE_CODE (TREE_TYPE (mode1)) == FUNCTION_TYPE)
	mode1 = TREE_TYPE (mode1);
      else
	return 0;
    }

  if (CH_IS_BUFFER_MODE (mode1) && CH_IS_BUFFER_MODE (mode2))
    {
      tree len1 = max_queue_size (mode1);
      tree len2 = max_queue_size (mode2);
      return tree_int_cst_equal (len1, len2);
    }
  else if (CH_IS_EVENT_MODE (mode1) && CH_IS_EVENT_MODE (mode2))
    {
      tree len1 = max_queue_size (mode1);
      tree len2 = max_queue_size (mode2);
      return tree_int_cst_equal (len1, len2);
    }
  else if (CH_IS_ACCESS_MODE (mode1) && CH_IS_ACCESS_MODE (mode2))
    {
      tree index1 = access_indexmode (mode1);
      tree index2 = access_indexmode (mode2);
      tree record1 = access_recordmode (mode1);
      tree record2 = access_recordmode (mode2);
      if (! chill_read_compatible (index1, index2))
	return 0;
      return chill_read_compatible (record1, record2);
    }
  switch ((enum chill_tree_code)TREE_CODE (mode1))
    {
    case INTEGER_TYPE:
    case BOOLEAN_TYPE:
    case CHAR_TYPE:
      return 1;
    case ENUMERAL_TYPE:
      if (TYPE_VALUES (mode1) == TYPE_VALUES (mode2))
	return 1;
      else
	{
	  /* FIXME: This is more strict than z.200, which seems to
	     allow the elements to be reordered, as long as they
	     have the same values. */

	  tree field1 = TYPE_VALUES (mode1);
	  tree field2 = TYPE_VALUES (mode2);

	  while (field1 != NULL_TREE && field2 != NULL_TREE)
	    {
	      tree value1, value2;
	      /* Check that the names are equal.  */
	      if (TREE_PURPOSE (field1) != TREE_PURPOSE (field2))
		break;

	      value1 = TREE_VALUE (field1);
	      value2 = TREE_VALUE (field2);
	      /* This isn't quite sufficient in general, but will do ... */
	      /* Note that proclaim_decl can cause the SET modes to be
		 compared BEFORE they are satisfied, but otherwise
		 chill_similar is mostly called after satisfaction. */
	      if (TREE_CODE (value1) == CONST_DECL)
		value1 = DECL_INITIAL (value1);
	      if (TREE_CODE (value2) == CONST_DECL)
		value2 = DECL_INITIAL (value2);
	      /* Check that the values are equal or both NULL.  */
	      if (!(value1 == NULL_TREE && value2 == NULL_TREE)
		  && (value1 == NULL_TREE || value2 == NULL_TREE
		      || ! tree_int_cst_equal (value1, value2)))
		break;
	      field1 = TREE_CHAIN (field1);
	      field2 = TREE_CHAIN (field2);
	    }
	  return field1 == NULL_TREE && field2 == NULL_TREE;
	}
    case SET_TYPE:
      /* check for bit strings */
      if (CH_BOOLS_TYPE_P (mode1))
	return CH_BOOLS_TYPE_P (mode2);
      if (CH_BOOLS_TYPE_P (mode2))
	return CH_BOOLS_TYPE_P (mode1);
      /* both are powerset modes */
      return CH_EQUIVALENT (TYPE_DOMAIN (mode1), TYPE_DOMAIN (mode2));

    case POINTER_TYPE:
      /* Are the referenced modes equivalent? */
      return !integer_zerop (chill_equivalent (TREE_TYPE (mode1),
					       TREE_TYPE (mode2),
					       &node));

    case ARRAY_TYPE:
      /* char for char strings */
      if (CH_CHARS_TYPE_P (mode1))
	return CH_CHARS_TYPE_P (mode2);
      if (CH_CHARS_TYPE_P (mode2))
	return CH_CHARS_TYPE_P (mode1);
      /* array modes */
      if (CH_V_EQUIVALENT (TYPE_DOMAIN (mode1), TYPE_DOMAIN (mode2))
	  /* Are the elements modes equivalent? */
	  && !integer_zerop (chill_equivalent (TREE_TYPE (mode1),
					       TREE_TYPE (mode2),
					       &node)))
	{
	  /* FIXME:  Check that element layouts are equivalent */

	  tree count1 = fold (build (MINUS_EXPR, sizetype,
				     TYPE_MAX_VALUE (TYPE_DOMAIN (mode1)),
				     TYPE_MIN_VALUE (TYPE_DOMAIN (mode1))));
	  tree count2 = fold (build (MINUS_EXPR, sizetype,
				     TYPE_MAX_VALUE (TYPE_DOMAIN (mode2)),
				     TYPE_MIN_VALUE (TYPE_DOMAIN (mode2))));
	  tree cond = build_compare_discrete_expr (EQ_EXPR, count1, count2);
	  if (TREE_CODE (cond) == INTEGER_CST)
	    return !integer_zerop (cond);
	  else
	    {
#if 0
	      extern int ignoring;
	      if (!ignoring 
		  && range_checking
		  && current_function_decl)
		return cond;
#endif
	      return 1;
	    }
	}
      return 0;

    case RECORD_TYPE:
    case UNION_TYPE:
      for (t1 = TYPE_FIELDS (mode1), t2 = TYPE_FIELDS (mode2);
	   t1 && t2;  t1 = TREE_CHAIN (t1), t2 = TREE_CHAIN (t2))
	   {
	     if (TREE_CODE (t1) != TREE_CODE (t2))
	       return 0;
	     /* Are the field modes equivalent? */
	     if (integer_zerop (chill_equivalent (TREE_TYPE (t1),
						   TREE_TYPE (t2),
						   &node)))
	       return 0;
	   }
      return t1 == t2;

    case FUNCTION_TYPE:
      if (!chill_l_equivalent (TREE_TYPE (mode1), TREE_TYPE (mode2), &node))
	return 0;
      for (t1 = TYPE_ARG_TYPES (mode1), t2 = TYPE_ARG_TYPES (mode2);
	   t1 != NULL_TREE && t2 != NULL_TREE;
	   t1 = TREE_CHAIN (t1), t2 = TREE_CHAIN (t2))
	{
	  tree attr1 = TREE_PURPOSE (t1)
	    ? TREE_PURPOSE (t1) : ridpointers[(int) RID_IN];
	  tree attr2 = TREE_PURPOSE (t2)
	    ? TREE_PURPOSE (t2) : ridpointers[(int) RID_IN];
	  if (attr1 != attr2)
	    return 0;
	  if (!chill_l_equivalent (TREE_VALUE (t1), TREE_VALUE (t2), &node))
	    return 0;
	}
      if (t1 != t2) /* Both NULL_TREE */
	return 0;
      /* check list of exception names */
      t1 = TYPE_RAISES_EXCEPTIONS (mode1);
      t2 = TYPE_RAISES_EXCEPTIONS (mode2);
      if (t1 == NULL_TREE && t2 != NULL_TREE)
	return 0;
      if (t1 != NULL_TREE && t2 == NULL_TREE)
	return 0;
      if (list_length (t1) != list_length (t2))
	return 0;
      while (t1 != NULL_TREE)
        {
	  if (value_member (TREE_VALUE (t1), t2) == NULL_TREE)
	    return 0;
	  t1 = TREE_CHAIN (t1);
        }
      /* FIXME:  Should also check they have the same RECURSIVITY */
      return 1;

    default:
      ;
#if 0
      /* Need to handle row modes, instance modes,
	 association modes, access modes, text modes,
	 duration modes, absolute time modes, structure modes,
	 parameterized structure modes */
#endif
    }
  return 1;
}

/* Return a node that is true iff MODE1 and MODE2 are equivalent.
   This is normally boolean_true_node or boolean_false_node,
   but can be dynamic for dynamic types.
   CHAIN is as for chill_similar.  */

tree
chill_equivalent (mode1, mode2, chain)
     tree mode1, mode2;
     struct mode_chain *chain;
{
  int varying1, varying2;
  int is_string1, is_string2;
  tree base_mode1, base_mode2;

  /* Are the modes v-equivalent? */
#if 0
  if (!chill_similar (mode1, mode2, chain)
      || CH_NOVELTY(mode1) != CH_NOVELTY(mode2))
    return boolean_false_node;
#endif
  if (!chill_similar (mode1, mode2, chain))
    return boolean_false_node;
  else if (TREE_CODE (mode2) == FUNCTION_TYPE
	   && TREE_CODE (mode1) == POINTER_TYPE
	   && TREE_CODE (TREE_TYPE (mode1)) == FUNCTION_TYPE)
    /* don't check novelty in this case to avoid error in case of
       NEWMODE'd proceduremode gets assigned a function */
    return boolean_true_node;
  else if (CH_NOVELTY(mode1) != CH_NOVELTY(mode2))
    return boolean_false_node;

  varying1 = chill_varying_type_p (mode1);
  varying2 = chill_varying_type_p (mode2);

  if (varying1 != varying2)
    return boolean_false_node;
  base_mode1 = varying1 ? CH_VARYING_ARRAY_TYPE (mode1) : mode1;
  base_mode2 = varying2 ? CH_VARYING_ARRAY_TYPE (mode2) : mode2;
  is_string1 = CH_STRING_TYPE_P (base_mode1);
  is_string2 = CH_STRING_TYPE_P (base_mode2);
  if (is_string1 || is_string2)
    {
      if (is_string1 != is_string2)
	return boolean_false_node;
      return fold (build (EQ_EXPR, boolean_type_node,
			  TYPE_SIZE (base_mode1),
			  TYPE_SIZE (base_mode2)));
    }

  /* && some more stuff FIXME! */
  if (TREE_CODE(mode1) == INTEGER_TYPE || TREE_CODE(mode2) == INTEGER_TYPE)
    {
      if (TREE_CODE(mode1) != INTEGER_TYPE || TREE_CODE(mode2) != INTEGER_TYPE)
	return boolean_false_node;
      /* If one is a range, the other has to be a range. */
      if ((TREE_TYPE (mode1) != NULL_TREE) != (TREE_TYPE (mode2) != NULL_TREE))
	return boolean_false_node;
      if (TYPE_PRECISION (mode1) != TYPE_PRECISION (mode2))
	return boolean_false_node;
      if (!tree_int_cst_equal (TYPE_MIN_VALUE (mode1), TYPE_MIN_VALUE (mode2)))
	return boolean_false_node;
      if (!tree_int_cst_equal (TYPE_MAX_VALUE (mode1), TYPE_MAX_VALUE (mode2)))
	return boolean_false_node;
    }
  return boolean_true_node;
}

static int
chill_l_equivalent (mode1, mode2, chain)
     tree mode1, mode2;
     struct mode_chain *chain;
{
  /* Are the modes equivalent? */
  if (integer_zerop (chill_equivalent (mode1, mode2, chain)))
    return 0;
  if (TYPE_READONLY (mode1) != TYPE_READONLY (mode2))
    return 0;
#if 0
  ... other conditions ...;
#endif
  return 1;
}

/* See Z200 12.1.2.12 */

int
chill_read_compatible (modeM, modeN)
     tree modeM, modeN;
{
  while (TREE_CODE (modeM) == REFERENCE_TYPE)
    modeM = TREE_TYPE (modeM);
  while (TREE_CODE (modeN) == REFERENCE_TYPE)
    modeN = TREE_TYPE (modeN);

  if (!CH_EQUIVALENT (modeM, modeN))
    return 0;
  if (TYPE_READONLY (modeN))
    {
      if (!TYPE_READONLY (modeM))
	return 0;
      if (CH_IS_BOUND_REFERENCE_MODE (modeM)
	  && CH_IS_BOUND_REFERENCE_MODE (modeN))
	{
	  return chill_l_equivalent (TREE_TYPE (modeM), TREE_TYPE (modeN), 0);
	}
#if 0
      ...;
#endif
    }
  return 1;
}

/* Tests if MODE is compatible with the class of EXPR.
   Cfr. Chill Blue Book 12.1.2.15. */

int
chill_compatible (expr, mode)
     tree expr, mode;
{
  struct ch_class class;

  if (expr == NULL_TREE || TREE_CODE (expr) == ERROR_MARK)
    return 0;
  if (mode == NULL_TREE || TREE_CODE (mode) == ERROR_MARK)
    return 0;

  while (TREE_CODE (mode) == REFERENCE_TYPE)
    mode = TREE_TYPE (mode);

  if (TREE_TYPE (expr) == NULL_TREE)
    {
      if (TREE_CODE (expr) == CONSTRUCTOR)
	return TREE_CODE (mode) == RECORD_TYPE
	  || ((TREE_CODE (mode) == SET_TYPE || TREE_CODE (mode) == ARRAY_TYPE)
	      && ! TYPE_STRING_FLAG (mode));
      else
	return TREE_CODE (expr) == CASE_EXPR || TREE_CODE (expr) == COND_EXPR;
    }

  class = chill_expr_class (expr);
  switch (class.kind)
    {
    case CH_ALL_CLASS:
      return 1;
    case CH_NULL_CLASS:
      return CH_IS_REFERENCE_MODE (mode) || CH_IS_PROCEDURE_MODE (mode)
	|| CH_IS_INSTANCE_MODE (mode);
    case CH_VALUE_CLASS:
      if (CH_HAS_REFERENCING_PROPERTY (mode))
	return CH_RESTRICTABLE_TO(mode, class.mode);
      else
	return CH_V_EQUIVALENT(mode, class.mode);
    case CH_DERIVED_CLASS:
      return CH_SIMILAR (class.mode, mode);
    case CH_REFERENCE_CLASS:
      if (!CH_IS_REFERENCE_MODE (mode))
	return 0;
#if 0
      /* FIXME! */
      if (class.mode is a row mode)
	...;
      else if (class.mode is not a static mode)
	return 0; /* is this possible? FIXME */
#endif
      return !CH_IS_BOUND_REFERENCE_MODE(mode)
	|| CH_READ_COMPATIBLE (TREE_TYPE (mode), class.mode);
    }
  return 0; /* ERROR! */
}

/* Tests if the class of of EXPR1 and EXPR2 are compatible.
   Cfr. Chill Blue Book 12.1.2.16. */

int
chill_compatible_classes (expr1, expr2)
     tree expr1, expr2;
{
  struct ch_class temp;
  struct ch_class class1, class2;
  class1 = chill_expr_class (expr1);
  class2 = chill_expr_class (expr2);

  switch (class1.kind)
    {
    case CH_ALL_CLASS:
      return 1;
    case CH_NULL_CLASS:
      switch (class2.kind)
	{
	case CH_ALL_CLASS:
	case CH_NULL_CLASS:
	case CH_REFERENCE_CLASS:
	  return 1;
	case CH_VALUE_CLASS:
	case CH_DERIVED_CLASS:
	  goto rule4;
	}
    case CH_REFERENCE_CLASS:
      switch (class2.kind)
	{
	case CH_ALL_CLASS:
	case CH_NULL_CLASS:
	  return 1;
	case CH_REFERENCE_CLASS:
	  return CH_EQUIVALENT (class1.mode, class2.mode);
	case CH_VALUE_CLASS:
	  goto rule6;
	case CH_DERIVED_CLASS:
	  return 0;
	}
    case CH_DERIVED_CLASS:
      switch (class2.kind)
	{
	case CH_ALL_CLASS:
	  return 1;
	case CH_VALUE_CLASS:
	case CH_DERIVED_CLASS:
	  return CH_SIMILAR (class1.mode, class2.mode);
	case CH_NULL_CLASS:
	  class2 = class1;
	  goto rule4;
	case CH_REFERENCE_CLASS:
	  return 0;
	}
    case CH_VALUE_CLASS:
      switch (class2.kind)
	{
	case CH_ALL_CLASS:
	  return 1;
	case CH_DERIVED_CLASS:
	  return CH_SIMILAR (class1.mode, class2.mode);
	case CH_VALUE_CLASS:
	  return CH_V_EQUIVALENT (class1.mode, class2.mode);
	case CH_NULL_CLASS:
	  class2 = class1;
	  goto rule4;
	case CH_REFERENCE_CLASS:
	  temp = class1;  class1 = class2;  class2 = temp;
	  goto rule6;
	}
    }
 rule4:
  /* The Null class is Compatible with the M-derived class or M-value class
     if and only if M is a reference mdoe, procedure mode or instance mode.*/
  return CH_IS_REFERENCE_MODE (class2.mode)
    || CH_IS_PROCEDURE_MODE (class2.mode)
    || CH_IS_INSTANCE_MODE (class2.mode);

 rule6:
  /* The M-reference class is compatible with the N-value class if and
     only if N is a reference mode and ... */
  if (!CH_IS_REFERENCE_MODE (class2.mode))
    return 0;
  if (1) /* If M is a static mode - FIXME */
    {
      if (!CH_IS_BOUND_REFERENCE_MODE (class2.mode))
	return 1;
      if (CH_EQUIVALENT (TREE_TYPE (class2.mode), class1.mode))
	return 1;
    }
  /* If N is a row mode whose .... FIXME */
  return 0;
}

/* Cfr.  Blue Book 12.1.1.6, with some "extensions." */

tree
chill_root_mode (mode)
     tree mode;
{
  /* Reference types are not user-visible types.
     This seems like a good place to get rid of them. */
  if (TREE_CODE (mode) == REFERENCE_TYPE)
    mode = TREE_TYPE (mode);

  while (TREE_CODE (mode) == INTEGER_TYPE && TREE_TYPE (mode) != NULL_TREE)
    mode = TREE_TYPE (mode);  /* a sub-range */

  /* This extension in not in the Blue Book - which only has a
     single Integer type.
     We should probably use chill_integer_type_node rather
     than integer_type_node, but that is likely to bomb.
     At some point, these will become the same, I hope. FIXME */
  if (TREE_CODE (mode) == INTEGER_TYPE
      && TYPE_PRECISION (mode) < TYPE_PRECISION (integer_type_node)
      && CH_NOVELTY (mode) == NULL_TREE)
    mode = integer_type_node;
 
  if (TREE_CODE (mode) == FUNCTION_TYPE)
    return build_pointer_type (mode);

  return mode;
}

/* Cfr.  Blue Book 12.1.1.7. */

tree
chill_resulting_mode (mode1, mode2)
     tree mode1, mode2;
{
  mode1 = CH_ROOT_MODE (mode1);
  mode2 = CH_ROOT_MODE (mode2);
  if (chill_varying_type_p (mode1))
    return mode1;
  if (chill_varying_type_p (mode2))
    return mode2;
  return mode1;
}

/* Cfr.  Blue Book (z200, 1988) 12.1.1.7 Resulting class. */

struct ch_class
chill_resulting_class (class1, class2)
     struct ch_class class1, class2;
{
  struct ch_class class;
  switch (class1.kind)
    {
    case CH_VALUE_CLASS:
      switch (class2.kind)
	{
	case CH_DERIVED_CLASS:
	case CH_ALL_CLASS:
	  class.kind = CH_VALUE_CLASS;
	  class.mode = CH_ROOT_MODE (class1.mode);
	  return class;
	case CH_VALUE_CLASS:
	  class.kind = CH_VALUE_CLASS;
	  class.mode
	    = CH_ROOT_MODE (CH_RESULTING_MODE (class1.mode, class2.mode));
	  return class;
	default:
	  break;
	}
      break;
    case CH_DERIVED_CLASS:
      switch (class2.kind)
	{
	case CH_VALUE_CLASS:
	  class.kind = CH_VALUE_CLASS;
	  class.mode = CH_ROOT_MODE (class2.mode);
	  return class;
	case CH_DERIVED_CLASS:
	  class.kind = CH_DERIVED_CLASS;
	  class.mode = CH_RESULTING_MODE (class1.mode, class2.mode);
	  return class;
	case CH_ALL_CLASS:
	  class.kind = CH_DERIVED_CLASS;
	  class.mode = CH_ROOT_MODE (class1.mode);
	  return class;
	default:
	  break;
	}
      break;
    case CH_ALL_CLASS:
      switch (class2.kind)
	{
	case CH_VALUE_CLASS:
	  class.kind = CH_VALUE_CLASS;
	  class.mode = CH_ROOT_MODE (class2.mode);
	  return class;
	case CH_ALL_CLASS:
	  class.kind = CH_ALL_CLASS;
	  class.mode = NULL_TREE;
	  return class;
	case CH_DERIVED_CLASS:
	  class.kind = CH_DERIVED_CLASS;
	  class.mode = CH_ROOT_MODE (class2.mode);
	  return class;
	default:
	  break;
	}
      break;
    default:
      break;
    }
  error ("internal error in chill_root_resulting_mode");
  class.kind = CH_VALUE_CLASS;
  class.mode = CH_ROOT_MODE (class1.mode);
  return class;
}


/*
 * See Z.200, section 6.3, static conditions. This function
 * returns bool_false_node if the condition is not met at compile time,
 *         bool_true_node if the condition is detectably met at compile time
 *         an expression if a runtime check would be required or was generated.
 * It should only be called with string modes and values.
 */
tree
string_assignment_condition (lhs_mode, rhs_value)
     tree lhs_mode, rhs_value;
{
  tree lhs_size, rhs_size, cond;
  tree rhs_mode = TREE_TYPE (rhs_value);
  int lhs_varying = chill_varying_type_p (lhs_mode);

  if (lhs_varying)
    lhs_size = size_in_bytes (CH_VARYING_ARRAY_TYPE (lhs_mode));
  else if (CH_BOOLS_TYPE_P (lhs_mode))
    lhs_size = TYPE_MAX_VALUE (TYPE_DOMAIN (lhs_mode));
  else
    lhs_size = size_in_bytes (lhs_mode);
  lhs_size = convert (chill_unsigned_type_node, lhs_size);

  if (rhs_mode && TREE_CODE (rhs_mode) == REFERENCE_TYPE)
    rhs_mode = TREE_TYPE (rhs_mode);
  if (rhs_mode == NULL_TREE)
    {
      /* actually, count constructor's length */
      abort ();
    }
  else if (chill_varying_type_p (rhs_mode))
    rhs_size = build_component_ref (rhs_value, var_length_id);
  else if (CH_BOOLS_TYPE_P (rhs_mode))
    rhs_size = TYPE_MAX_VALUE (TYPE_DOMAIN (rhs_mode));
  else
    rhs_size = size_in_bytes (rhs_mode);
  rhs_size = convert (chill_unsigned_type_node, rhs_size);

  /* validity condition */
  cond = fold (build (lhs_varying ? GE_EXPR : EQ_EXPR, 
	   boolean_type_node, lhs_size, rhs_size));
  return cond;
}

/*
 * take a basic CHILL type and wrap it in a VARYING structure.
 * Be sure the length field is initialized.  Return the wrapper.
 */
tree
build_varying_struct (type)
     tree type;
{  
  tree decl1, decl2, result;

  if (type == NULL_TREE || TREE_CODE (type) == ERROR_MARK)
    return error_mark_node;

  decl1 = build_decl (FIELD_DECL, var_length_id, chill_integer_type_node);
  decl2 = build_decl (FIELD_DECL, var_data_id, type);
  TREE_CHAIN (decl1) = decl2;      
  TREE_CHAIN (decl2) = NULL_TREE;
  result = build_chill_struct_type (decl1);

  /* mark this so we don't complain about missing initializers.
     It's fine for a VARYING array to be partially initialized.. */
  C_TYPE_VARIABLE_SIZE(type) = 1;
  return result;
}


/*
 * This is the struct type that forms the runtime initializer
 * list.  There's at least one of these generated per module.
 * It's attached to the global initializer list by the module's
 * 'constructor' code.  Should only be called in pass 2.
 */
tree
build_init_struct ()
{
  tree decl1, decl2, result;
  /* We temporarily reset the maximum_field_alignment to zero so the
     compiler's init data structures can be compatible with the
     run-time system, even when we're compiling with -fpack. */
  unsigned int save_maximum_field_alignment = maximum_field_alignment;
  maximum_field_alignment = 0;

  decl1 = build_decl (FIELD_DECL, get_identifier ("__INIT_ENTRY"),
	    build_chill_pointer_type (
              build_function_type (void_type_node, NULL_TREE)));

  decl2 = build_decl (FIELD_DECL, get_identifier ("__INIT_NEXT"),
		      build_chill_pointer_type (void_type_node));

  TREE_CHAIN (decl1) = decl2;      
  TREE_CHAIN (decl2) = NULL_TREE;
  result = build_chill_struct_type (decl1);
  maximum_field_alignment = save_maximum_field_alignment;
  return result;
}


/*
 * Return 1 if the given type is a single-bit boolean set,
 *          in which the domain's min and max values 
 *          are both zero,
 *        0 if not.  This can become a macro later..
 */
int
ch_singleton_set (type)
     tree type;
{
  if (type == NULL_TREE || TREE_CODE (type) == ERROR_MARK)
    return 0;
  if (TREE_CODE (type) != SET_TYPE)
    return 0;
  if (TREE_TYPE (type) == NULL_TREE 
      || TREE_CODE (TREE_TYPE (type)) != BOOLEAN_TYPE)
    return 0;
  if (TYPE_DOMAIN (type) == NULL_TREE)
    return 0;
  if (! tree_int_cst_equal (TYPE_MIN_VALUE (TYPE_DOMAIN (type)),
			    integer_zero_node))
    return 0;
  if (! tree_int_cst_equal (TYPE_MAX_VALUE (TYPE_DOMAIN (type)),
			    integer_zero_node))
    return 0;
  return 1;
}

/* return non-zero if TYPE is a compiler-generated VARYING
   array of some base type */
int
chill_varying_type_p (type)
     tree type;
{
  if (type == NULL_TREE)
    return 0;
  if (TREE_CODE (type) != RECORD_TYPE)
    return 0;
  if (TYPE_FIELDS (type) == NULL_TREE 
      || TREE_CHAIN (TYPE_FIELDS (type)) == NULL_TREE)
    return 0;
  if (DECL_NAME (TYPE_FIELDS (type)) != var_length_id)
    return 0;
  if (DECL_NAME (TREE_CHAIN (TYPE_FIELDS (type))) != var_data_id)
    return 0;
  if (TREE_CHAIN (TREE_CHAIN (TYPE_FIELDS (type))) != NULL_TREE)
    return 0;
  return 1;
}

/* return non-zero if TYPE is a compiler-generated VARYING
   string record */
int
chill_varying_string_type_p (type)
     tree type;
{
  tree var_data_type;
    
  if (!chill_varying_type_p (type))
      return 0;
  
  var_data_type = CH_VARYING_ARRAY_TYPE (type);
  return CH_CHARS_TYPE_P (var_data_type);
}

/* swiped from c-typeck.c */
/* Build an assignment expression of lvalue LHS from value RHS. */

tree
build_chill_modify_expr (lhs, rhs)
     tree lhs, rhs;
{
  register tree result;


  tree lhstype = TREE_TYPE (lhs);

  /* Avoid duplicate error messages from operands that had errors.  */
  if (lhs == NULL_TREE || TREE_CODE (lhs) == ERROR_MARK || rhs == NULL_TREE || TREE_CODE (rhs) == ERROR_MARK)
    return error_mark_node;

  /* Strip NON_LVALUE_EXPRs since we aren't using as an lvalue.  */
  /* Do not use STRIP_NOPS here.  We do not want an enumerator
     whose value is 0 to count as a null pointer constant.  */
  if (TREE_CODE (rhs) == NON_LVALUE_EXPR)
    rhs = TREE_OPERAND (rhs, 0);

#if 0
  /* Handle a cast used as an "lvalue".
     We have already performed any binary operator using the value as cast.
     Now convert the result to the cast type of the lhs,
     and then true type of the lhs and store it there;
     then convert result back to the cast type to be the value
     of the assignment.  */

  switch (TREE_CODE (lhs))
    {
    case NOP_EXPR:
    case CONVERT_EXPR:
    case FLOAT_EXPR:
    case FIX_TRUNC_EXPR:
    case FIX_FLOOR_EXPR:
    case FIX_ROUND_EXPR:
    case FIX_CEIL_EXPR:
      {
	tree inner_lhs = TREE_OPERAND (lhs, 0);
	tree result;
	result = build_chill_modify_expr (inner_lhs,
		   convert (TREE_TYPE (inner_lhs),
		     convert (lhstype, rhs)));
	pedantic_lvalue_warning (CONVERT_EXPR);
	return convert (TREE_TYPE (lhs), result);
      }
    }

  /* Now we have handled acceptable kinds of LHS that are not truly lvalues.
     Reject anything strange now.  */

  if (!lvalue_or_else (lhs, "assignment"))
    return error_mark_node;
#endif
  /* FIXME: need to generate a RANGEFAIL if the RHS won't
     fit into the LHS. */

  if (TREE_CODE (lhs) != VAR_DECL
      && ((TREE_CODE (TREE_TYPE (lhs)) == ARRAY_TYPE &&
	   (TREE_TYPE (rhs) && TREE_CODE (TREE_TYPE (rhs)) == ARRAY_TYPE)) ||
	  chill_varying_type_p (TREE_TYPE (lhs)) || 
	  chill_varying_type_p (TREE_TYPE (rhs))))
    {
      int lhs_varying = chill_varying_type_p (TREE_TYPE (lhs));
      int rhs_varying = chill_varying_type_p (TREE_TYPE (rhs));

      /* point at actual RHS data's type */
      tree rhs_data_type = rhs_varying ? 
	CH_VARYING_ARRAY_TYPE (TREE_TYPE (rhs)) :
	  TREE_TYPE (rhs);
      {
	/* point at actual LHS data's type */
	tree lhs_data_type = lhs_varying ? 
	  CH_VARYING_ARRAY_TYPE (TREE_TYPE (lhs)) :
	    TREE_TYPE (lhs);

	int lhs_bytes = int_size_in_bytes (lhs_data_type);
	int rhs_bytes = int_size_in_bytes (rhs_data_type);

	/* if both sides not varying, and sizes not dynamically 
	   computed, sizes must *match* */
	if (! lhs_varying && ! rhs_varying && lhs_bytes != rhs_bytes
	    && lhs_bytes > 0 && rhs_bytes > 0)
	  {
	    error ("string lengths not equal");
	    return error_mark_node;
	  }
	/* Must have enough space on LHS for static size of RHS */
    
	if (lhs_bytes > 0 && rhs_bytes > 0 
	    && lhs_bytes < rhs_bytes)
	  {
	    if (rhs_varying)
	      {
		/* FIXME: generate runtime test for room */
		;
	      }
	    else
	      {
		error ("can't do ARRAY assignment - too large");
		return error_mark_node;
	      }
	  }
      }

      /* now we know the RHS will fit in LHS, build trees for the
	 emit_block_move parameters */

      if (lhs_varying)
	rhs = convert (TREE_TYPE (lhs), rhs);
      else
	{
	  if (rhs_varying)
	    rhs = build_component_ref (rhs, var_data_id);

	  if (! mark_addressable (rhs))
	    {
	      error ("rhs of array assignment is not addressable");
	      return error_mark_node;
	    }

	  lhs = force_addr_of (lhs);
	  rhs = build1 (ADDR_EXPR, const_ptr_type_node, rhs);
	  return
	  build_chill_function_call (lookup_name (get_identifier ("memmove")),
	    tree_cons (NULL_TREE, lhs,
              tree_cons (NULL_TREE, rhs,
                tree_cons (NULL_TREE, size_in_bytes (rhs_data_type), 
		   NULL_TREE))));
	}
    }

  result = build (MODIFY_EXPR, lhstype, lhs, rhs);
  TREE_SIDE_EFFECTS (result) = 1;

  return result;
}

/* Constructors for pointer, array and function types.
   (RECORD_TYPE, UNION_TYPE and ENUMERAL_TYPE nodes are
   constructed by language-dependent code, not here.)  */

/* Construct, lay out and return the type of pointers to TO_TYPE.
   If such a type has already been constructed, reuse it.  */

static tree
make_chill_pointer_type (to_type, code)
     tree to_type;
     enum tree_code code;  /* POINTER_TYPE or REFERENCE_TYPE */
{
  extern struct obstack *current_obstack;
  extern struct obstack *saveable_obstack;
  extern struct obstack  permanent_obstack;
  tree t;
  register struct obstack *ambient_obstack = current_obstack;
  register struct obstack *ambient_saveable_obstack = saveable_obstack;

  /* If TO_TYPE is permanent, make this permanent too.  */
  if (TREE_PERMANENT (to_type))
    {
      current_obstack = &permanent_obstack;
      saveable_obstack = &permanent_obstack;
    }

  t = make_node (code);
  TREE_TYPE (t) = to_type;

  current_obstack = ambient_obstack;
  saveable_obstack = ambient_saveable_obstack;
  return t;
}


tree
build_chill_pointer_type (to_type)
     tree to_type;
{
  int is_type_node = TREE_CODE_CLASS (TREE_CODE (to_type)) == 't';
  register tree t = is_type_node ? TYPE_POINTER_TO (to_type) : NULL_TREE;

  /* First, if we already have a type for pointers to TO_TYPE, use it.  */

  if (t)
    return t;

  /* We need a new one. */
  t = make_chill_pointer_type (to_type, POINTER_TYPE);

  /* Lay out the type.  This function has many callers that are concerned
     with expression-construction, and this simplifies them all.
     Also, it guarantees the TYPE_SIZE is permanent if the type is.  */
  if ((is_type_node && (TYPE_SIZE (to_type) != NULL_TREE))
      || pass == 2)
    {
      /* Record this type as the pointer to TO_TYPE.  */
      TYPE_POINTER_TO (to_type) = t;
      layout_type (t);
    }

  return t;
}

tree
build_chill_reference_type (to_type)
     tree to_type;
{
  int is_type_node = TREE_CODE_CLASS (TREE_CODE (to_type)) == 't';
  register tree t = is_type_node ? TYPE_REFERENCE_TO (to_type) : NULL_TREE;

  /* First, if we already have a type for references to TO_TYPE, use it.  */

  if (t)
    return t;

  /* We need a new one. */
  t = make_chill_pointer_type (to_type, REFERENCE_TYPE);

  /* Lay out the type.  This function has many callers that are concerned
     with expression-construction, and this simplifies them all.
     Also, it guarantees the TYPE_SIZE is permanent if the type is.  */
  if ((is_type_node && (TYPE_SIZE (to_type) != NULL_TREE))
      || pass == 2)
    {
      /* Record this type as the reference to TO_TYPE.  */
      TYPE_REFERENCE_TO (to_type) = t;
      layout_type (t);
      CH_NOVELTY (t) = CH_NOVELTY (to_type);
    }

  return t;
}

static tree
make_chill_range_type (type, lowval, highval)
     tree type, lowval, highval;
{
  register tree itype = make_node (INTEGER_TYPE);
  TREE_TYPE (itype) = type;
  TYPE_MIN_VALUE (itype) = lowval;
  TYPE_MAX_VALUE (itype) = highval;
  return itype;
}

tree
layout_chill_range_type (rangetype, must_be_const)
     tree rangetype;
     int must_be_const;
{
  tree type = TREE_TYPE (rangetype);
  tree lowval = TYPE_MIN_VALUE (rangetype);
  tree highval = TYPE_MAX_VALUE (rangetype);
  int bad_limits = 0;

  if (TYPE_SIZE (rangetype) != NULL_TREE)
    return rangetype;

  /* process BIN */
  if (type == ridpointers[(int) RID_BIN])
    {
      int binsize;
      
      /* make a range out of it */
      if (TREE_CODE (highval) != INTEGER_CST)
	{
	  error ("non-constant expression for BIN");
	  return error_mark_node;
	}
      binsize = TREE_INT_CST_LOW (highval);
      if (binsize < 0)
	{
	  error ("expression for BIN must not be negative");
	  return error_mark_node;
	}
      if (binsize > 32)
	{
	  error ("cannot process BIN (>32)");
	  return error_mark_node;
	}
      type = ridpointers [(int) RID_RANGE];
      lowval = integer_zero_node;
      highval = build_int_2 ((1 << binsize) - 1, 0);
    }
 
  if (TREE_CODE (lowval) == ERROR_MARK ||
      TREE_CODE (highval) == ERROR_MARK)
    return error_mark_node;

  if (!CH_COMPATIBLE_CLASSES (lowval, highval))
    {
      error ("bounds of range are not compatible");
      return error_mark_node;
    }

  if (type == string_index_type_dummy)
    {
      if (TREE_CODE (highval) == INTEGER_CST
	  && compare_int_csts (LT_EXPR, highval, integer_minus_one_node))
	{
	  error ("negative string length");
	  highval = integer_minus_one_node;
	}
      if (compare_int_csts (EQ_EXPR, highval, integer_minus_one_node))
	type = integer_type_node;
      else
	type = sizetype;
      TREE_TYPE (rangetype) = type;
    }
  else if (type == ridpointers[(int) RID_RANGE])
    {
      /* This isn't 100% right, since the Blue Book definition
	 uses Resulting Class, rather than Resulting Mode,
	 but it's close enough. */
      type = CH_ROOT_RESULTING_CLASS (lowval, highval).mode;

      /* The default TYPE is the type of the constants -
	 except if the constants are integers, we choose an
	 integer type that fits. */
      if (TREE_CODE (type) == INTEGER_TYPE
	  && TREE_CODE (lowval) == INTEGER_CST
	  && TREE_CODE (highval) == INTEGER_CST)
	{
	  /* The logic of this code has been copied from finish_enum
	     in c-decl.c.  FIXME duplication! */
	  int precision = 0;
	  HOST_WIDE_INT maxvalue = TREE_INT_CST_LOW (highval);
	  HOST_WIDE_INT minvalue = TREE_INT_CST_LOW (lowval);
	  if (TREE_INT_CST_HIGH (lowval) >= 0
	      ? tree_int_cst_lt (TYPE_MAX_VALUE (unsigned_type_node), highval)
	      : (tree_int_cst_lt (lowval, TYPE_MIN_VALUE (integer_type_node))
		 || tree_int_cst_lt (TYPE_MAX_VALUE (integer_type_node), highval)))
	    precision = TYPE_PRECISION (long_long_integer_type_node);
	  else
	    {
	      if (maxvalue > 0)
		precision = floor_log2 (maxvalue) + 1;
	      if (minvalue < 0)
		{
		  /* Compute number of bits to represent magnitude of a
		     negative value.  Add one to MINVALUE since range of
		     negative numbers includes the power of two.  */
		  int negprecision = floor_log2 (-minvalue - 1) + 1;
		  if (negprecision > precision)
		    precision = negprecision;
		  precision += 1;	/* room for sign bit */
		}

	      if (!precision)
		precision = 1;
	    }
	  type = type_for_size (precision, minvalue >= 0);

	}
      TREE_TYPE (rangetype) = type;
    }
  else
    {
      if (!CH_COMPATIBLE (lowval, type))
	{
	  error ("range's lower bound and parent mode don't match");
	  return integer_type_node;    /* an innocuous fake */
	}
      if (!CH_COMPATIBLE (highval, type))
	{
	  error ("range's upper bound and parent mode don't match");
	  return integer_type_node;    /* an innocuous fake */
	}
    }

  if (TREE_CODE (type) == ERROR_MARK)
    return type;
  else if (TREE_CODE_CLASS (TREE_CODE (type)) != 't')
    {
      error ("making range from non-mode");
      return error_mark_node;
    }

  if (TREE_CODE (lowval) == REAL_CST || TREE_CODE (highval) == REAL_CST)
    {
      sorry ("floating point ranges");
      return integer_type_node; /* another fake */
    }

  if (TREE_CODE (lowval) != INTEGER_CST || TREE_CODE (highval) != INTEGER_CST)
    {
      if (must_be_const)
	{
	  error ("range mode has non-constant limits");
	  bad_limits = 1;
	}
    }
  else if (tree_int_cst_equal (lowval, integer_zero_node)
	   && tree_int_cst_equal (highval, integer_minus_one_node))
    ; /* do nothing - this is the index type for an empty string */
  else if (compare_int_csts (LT_EXPR, highval, TYPE_MIN_VALUE (type)))
    {
      error ("range's high bound < mode's low bound");
      bad_limits = 1;
    }
  else if (compare_int_csts (GT_EXPR, highval, TYPE_MAX_VALUE (type)))
    {
      error ("range's high bound > mode's high bound");
      bad_limits = 1;
    }
  else if (compare_int_csts (LT_EXPR, highval, lowval))
    {
      error ("range mode high bound < range mode low bound");
      bad_limits = 1;
    }
  else if (compare_int_csts (LT_EXPR, lowval, TYPE_MIN_VALUE (type)))
    {
      error ("range's low bound < mode's low bound");
      bad_limits = 1;
    }
  else if (compare_int_csts (GT_EXPR, lowval, TYPE_MAX_VALUE (type)))
    {
      error ("range's low bound > mode's high bound");
      bad_limits = 1;
    }

  if (bad_limits)
    {
      lowval = TYPE_MIN_VALUE (type);
      highval = lowval;
    }

  highval = convert (type, highval);
  lowval =  convert (type, lowval);
  TYPE_MIN_VALUE (rangetype) = lowval;
  TYPE_MAX_VALUE (rangetype) = highval;
  TYPE_PRECISION (rangetype) = TYPE_PRECISION (type);
  TYPE_MODE (rangetype) = TYPE_MODE (type);
  TYPE_SIZE (rangetype) = TYPE_SIZE (type);
  TYPE_SIZE_UNIT (rangetype) = TYPE_SIZE_UNIT (type);
  TYPE_ALIGN (rangetype) = TYPE_ALIGN (type);
  TREE_UNSIGNED (rangetype) = TREE_UNSIGNED (type);
  CH_NOVELTY (rangetype) = CH_NOVELTY (type);
  return rangetype;
}

/* Build a _TYPE node that has range bounds associated with its values.
   TYPE is the base type for the range type. */
tree
build_chill_range_type (type, lowval, highval)
     tree type, lowval, highval;
{
  tree rangetype;

  if (type == NULL_TREE)
    type = ridpointers[(int) RID_RANGE];
  else if (TREE_CODE (type) == ERROR_MARK)
    return error_mark_node;

  rangetype = make_chill_range_type (type, lowval, highval);
  if (pass != 1)
    rangetype = layout_chill_range_type (rangetype, 0);

  return rangetype;
}

/* Build a CHILL array type, but with minimal checking etc. */

tree
build_simple_array_type (type, idx, layout)
     tree type, idx, layout;
{
  tree array_type = make_node (ARRAY_TYPE);
  TREE_TYPE (array_type) = type;
  TYPE_DOMAIN (array_type) = idx;
  TYPE_ATTRIBUTES (array_type) = layout;
  if (pass != 1)
    array_type = layout_chill_array_type (array_type);
  return array_type;
}

static void
apply_chill_array_layout (array_type)
     tree array_type;
{
  tree layout, temp, what, element_type;
  int stepsize=0, word, start_bit=0, length, natural_length;
  int stepsize_specified;
  int start_bit_error = 0;
  int length_error = 0;

  layout = TYPE_ATTRIBUTES (array_type);
  if (layout == NULL_TREE)
    return;

  if (layout == integer_zero_node) /* NOPACK */
    {
      TYPE_PACKED (array_type) = 0;
      return;
    }

  /* Allow for the packing of 1 bit discrete modes at the bit level. */
  element_type = TREE_TYPE (array_type);
  if (discrete_type_p (element_type)
      && get_type_precision (TYPE_MIN_VALUE (element_type),
			     TYPE_MAX_VALUE (element_type)) == 1)
    natural_length = 1;
  else
    natural_length = TREE_INT_CST_LOW (TYPE_SIZE (element_type));

  if (layout == integer_one_node) /* PACK */
    {
      if (natural_length == 1)
	TYPE_PACKED (array_type) = 1;
      return;
    }

  /* The layout is a STEP (...).
     The current implementation restricts STEP specifications to be of the form
     STEP(POS(0,0,n),n) where n is the natural size of the element mode. */
  stepsize_specified = 0;
  temp = TREE_VALUE (layout);
  if (TREE_VALUE (temp) != NULL_TREE)
    {
      if (TREE_CODE (TREE_VALUE (temp)) != INTEGER_CST)
	error ("Stepsize in STEP must be an integer constant");
      else
	{
	  stepsize = TREE_INT_CST_LOW (TREE_VALUE (temp));
	  if (stepsize <= 0)
	    error ("Stepsize in STEP must be > 0");
	  else
	    stepsize_specified = 1;

	  if (stepsize != natural_length)
	    sorry ("Stepsize in STEP must be the natural width of the array element mode");
	}
    }

  temp = TREE_PURPOSE (temp);
  if (TREE_CODE (TREE_PURPOSE (temp)) != INTEGER_CST)
    error ("Starting word in POS must be an integer constant");
  else
    {
      word = TREE_INT_CST_LOW (TREE_PURPOSE (temp));
      if (word < 0)
	error ("Starting word in POS must be >= 0");
      if (word != 0)
	sorry ("Starting word in POS within STEP must be 0");
    }

  length = natural_length;
  temp = TREE_VALUE (temp);
  if (temp != NULL_TREE)
    {
      int wordsize = TYPE_PRECISION (chill_integer_type_node);
      if (TREE_CODE (TREE_PURPOSE (temp)) != INTEGER_CST)
	{
	  error ("Starting bit in POS must be an integer constant");
	  start_bit_error = 1;
	}
      else
	{
	  start_bit = TREE_INT_CST_LOW (TREE_PURPOSE (temp));
	  if (start_bit != 0)
	    sorry ("Starting bit in POS within STEP must be 0");
	  if (start_bit < 0)
	    {
	      error ("Starting bit in POS must be >= 0");
	      start_bit = 0;
	      start_bit_error = 1;
	    }
	  else if (start_bit >= wordsize)
	    {
	      error ("Starting bit in POS must be < the width of a word");
	      start_bit = 0;
	      start_bit_error = 1;
	    }
	}

      temp = TREE_VALUE (temp);
      if (temp != NULL_TREE)
	{
	  what = TREE_PURPOSE (temp);
	  if (what == integer_zero_node)
	    {
	      if (TREE_CODE (TREE_VALUE (temp)) != INTEGER_CST)
		{
		  error ("Length in POS must be an integer constant");
		  length_error = 1;
		}
	      else
		{
		  length = TREE_INT_CST_LOW (TREE_VALUE (temp));
		  if (length <= 0)
		    error ("Length in POS must be > 0");
		}
	    }
	  else
	    {
	      if (TREE_CODE (TREE_VALUE (temp)) != INTEGER_CST)
		{
		  error ("End bit in POS must be an integer constant");
		  length_error = 1;
		}
	      else
		{
		  int end_bit = TREE_INT_CST_LOW (TREE_VALUE (temp));
		  if (end_bit < start_bit)
		    {
		      error ("End bit in POS must be >= the start bit");
		      end_bit = wordsize - 1;
		      length_error = 1;
		    }
		  else if (end_bit >= wordsize)
		    {
		      error ("End bit in POS must be < the width of a word");
		      end_bit = wordsize - 1;
		      length_error = 1;
		    }
		  else if (start_bit_error)
		    length_error = 1;
		  else
		    length = end_bit - start_bit + 1;
		}
	    }
	  if (! length_error && length != natural_length)
	    {
	      sorry ("The length specified on POS within STEP must be the natural length of the array element type");
	    }
	}
    }

  if (! length_error && stepsize_specified && stepsize < length)
    error ("Step size in STEP must be >= the length in POS");

  if (length == 1)
    TYPE_PACKED (array_type) = 1;
}

tree
layout_chill_array_type (array_type)
     tree array_type;
{
  tree itype;
  tree element_type = TREE_TYPE (array_type);

  if (TREE_CODE (element_type) == ARRAY_TYPE
      && TYPE_SIZE (element_type) == 0)
    layout_chill_array_type (element_type);

  itype = TYPE_DOMAIN (array_type);

  if (TREE_CODE (itype) == ERROR_MARK
      || TREE_CODE (element_type) == ERROR_MARK)
    return error_mark_node;

  /* do a lower/upper bound check. */
  if (TREE_CODE (itype) == INTEGER_CST)
    {
      error ("array index must be a range, not a single integer");
      return error_mark_node;
    }
  if (TREE_CODE_CLASS (TREE_CODE (itype)) != 't'
      || !discrete_type_p (itype))
    {
      error ("array index is not a discrete mode");
      return error_mark_node;
    }

  /* apply the array layout, if specified. */
  apply_chill_array_layout (array_type);
  TYPE_ATTRIBUTES (array_type) = NULL_TREE;

  /* Make sure TYPE_POINTER_TO (element_type) is filled in.  */
  build_pointer_type (element_type);

  if (TYPE_SIZE (array_type) == 0)
    layout_type (array_type);

  if (TYPE_READONLY_PROPERTY (element_type))
    TYPE_FIELDS_READONLY (array_type) = 1;

  TYPE_ARRAY_MAX_SIZE (array_type) = size_in_bytes (array_type);
  return array_type;
}

/* Build a CHILL array type.

   TYPE is the element type of the array.
   IDXLIST is the list of dimensions of the array.
   VARYING_P is non-zero if the array is a varying array.
   LAYOUT is (NULL_TREE, integer_one_node, integer_zero_node, tree_list),
   meaning (default, pack, nopack, STEP (...) ).  */
tree
build_chill_array_type (type, idxlist, varying_p, layouts)
     tree type, idxlist;
     int varying_p;
     tree layouts;
{
  tree array_type = type;

  if (type == NULL_TREE || TREE_CODE (type) == ERROR_MARK)
    return error_mark_node;
  if (idxlist == NULL_TREE || TREE_CODE (idxlist) == ERROR_MARK)
    return error_mark_node;

  /* We have to walk down the list of index decls, building inner
     array types as we go. We need to reverse the list of layouts so that the
     first layout applies to the last index etc. */
  layouts = nreverse (layouts);
  for ( ; idxlist; idxlist = TREE_CHAIN (idxlist))
    {
      if (layouts != NULL_TREE)
	{
	  type = build_simple_array_type (
		   type, TREE_VALUE (idxlist), TREE_VALUE (layouts));
	  layouts = TREE_CHAIN (layouts);
	}
      else
        type = build_simple_array_type (type, TREE_VALUE (idxlist), NULL_TREE);
    }
  array_type = type;
  if (varying_p)
    array_type = build_varying_struct (array_type);
  return array_type;
}

/* Function to help qsort sort FIELD_DECLs by name order.  */

static int
field_decl_cmp (x, y)
     tree *x, *y;
{
  return (long)DECL_NAME (*x) - (long)DECL_NAME (*y);
}

static tree
make_chill_struct_type (fieldlist)
     tree fieldlist;
{
  tree t, x;

  t = make_node (TREE_UNION_ELEM (fieldlist) ? UNION_TYPE : RECORD_TYPE);

  /* Install struct as DECL_CONTEXT of each field decl. */
  for (x = fieldlist; x; x = TREE_CHAIN (x))
    DECL_CONTEXT (x) = t;

  /* Delete all duplicate fields from the fieldlist */
  for (x = fieldlist; x && TREE_CHAIN (x);)
    /* Anonymous fields aren't duplicates.  */
    if (DECL_NAME (TREE_CHAIN (x)) == 0)
      x = TREE_CHAIN (x);
    else
      {
	register tree y = fieldlist;
	  
	while (1)
	  {
	    if (DECL_NAME (y) == DECL_NAME (TREE_CHAIN (x)))
	      break;
	    if (y == x)
	      break;
	    y = TREE_CHAIN (y);
	  }
	if (DECL_NAME (y) == DECL_NAME (TREE_CHAIN (x)))
	  {
	    error_with_decl (TREE_CHAIN (x), "duplicate member `%s'");
	    TREE_CHAIN (x) = TREE_CHAIN (TREE_CHAIN (x));
	  }
	else x = TREE_CHAIN (x);
      }

  TYPE_FIELDS (t) = fieldlist;

  return t;
}

/* DECL is a FIELD_DECL.
   DECL_INIT (decl) is
       (NULL_TREE, integer_one_node, integer_zero_node, tree_list)
    meaning
        (default, pack, nopack, POS (...) ).

   The return value is a boolean: 1 if POS specified, 0 if not */

static int
apply_chill_field_layout (decl, next_struct_offset)
     tree decl;
     int *next_struct_offset;
{
  tree layout = DECL_INITIAL (decl);
  tree type = TREE_TYPE (decl);
  tree temp, what;
  HOST_WIDE_INT word = 0;
  HOST_WIDE_INT wordsize, start_bit, offset, length, natural_length;
  int pos_error = 0;
  int is_discrete = discrete_type_p (type);

  if (is_discrete)
    natural_length
      = get_type_precision (TYPE_MIN_VALUE (type), TYPE_MAX_VALUE (type));
  else
    natural_length = TREE_INT_CST_LOW (TYPE_SIZE (type));

  if (layout == integer_zero_node) /* NOPACK */
    {
      *next_struct_offset += natural_length;
      return 0; /* not POS */
    }

  if (layout == integer_one_node) /* PACK */
    {
      if (is_discrete)
	{
	  DECL_BIT_FIELD (decl) = 1;
	  DECL_SIZE (decl) = bitsize_int (natural_length);
	}
      else
	DECL_ALIGN (decl) = BITS_PER_UNIT;

      DECL_PACKED (decl) = 1;
      *next_struct_offset += natural_length;
      return 0; /* not POS */
    }

  /* The layout is a POS (...). The current implementation restricts the use
     of POS to monotonically increasing fields whose width must be the
     natural width of the underlying type. */
  temp = TREE_PURPOSE (layout);

  if (TREE_CODE (TREE_PURPOSE (temp)) != INTEGER_CST)
    {
      error ("Starting word in POS must be an integer constant");
      pos_error = 1;
    }
  else
    {
      word = TREE_INT_CST_LOW (TREE_PURPOSE (temp));
      if (tree_int_cst_sgn (TREE_PURPOSE (temp)) < 0)
	{
	  error ("Starting word in POS must be >= 0");
	  word = 0;
	  pos_error = 1;
	}
    }

  wordsize = TYPE_PRECISION (chill_integer_type_node);
  offset = word * wordsize;
  length = natural_length;

  temp = TREE_VALUE (temp);
  if (temp != NULL_TREE)
    {
      if (TREE_CODE (TREE_PURPOSE (temp)) != INTEGER_CST)
	{
	  error ("Starting bit in POS must be an integer constant");
	  start_bit = *next_struct_offset - offset;
	  pos_error = 1;
	}
      else
	{
	  start_bit = TREE_INT_CST_LOW (TREE_PURPOSE (temp));
	  if (tree_int_cst_sgn (TREE_PURPOSE (temp)) < 0)
	    {
	      error ("Starting bit in POS must be >= 0");
	      start_bit = *next_struct_offset - offset;
	      pos_error = 1;
	    }
	  else if (start_bit >= wordsize)
	    {
	      error ("Starting bit in POS must be < the width of a word");
	      start_bit = *next_struct_offset - offset;
	      pos_error = 1;
	    }
	}

      temp = TREE_VALUE (temp);
      if (temp != NULL_TREE)
	{
	  what = TREE_PURPOSE (temp);
	  if (what == integer_zero_node)
	    {
	      if (TREE_CODE (TREE_VALUE (temp)) != INTEGER_CST)
		{
		  error ("Length in POS must be an integer constant");
		  pos_error = 1;
		}
	      else
		{
		  length = TREE_INT_CST_LOW (TREE_VALUE (temp));
		  if (tree_int_cst_sgn (TREE_VALUE (temp)) < 0)
		    {
		      error ("Length in POS must be > 0");
		      length = natural_length;
		      pos_error = 1;
		    }
		}
	    }
	  else
	    {
	      if (TREE_CODE (TREE_VALUE (temp)) != INTEGER_CST)
		{
		  error ("End bit in POS must be an integer constant");
		  pos_error = 1;
		}
	      else
		{
		  HOST_WIDE_INT end_bit = TREE_INT_CST_LOW (TREE_VALUE (temp));

		  if (end_bit < start_bit)
		    {
		      error ("End bit in POS must be >= the start bit");
		      pos_error = 1;
		    }
		  else if (end_bit >= wordsize)
		    {
		      error ("End bit in POS must be < the width of a word");
		      pos_error = 1;
		    }
		  else
		    length = end_bit - start_bit + 1;
		}
	    }

	  if (length != natural_length && ! pos_error)
	    {
	      sorry ("The length specified on POS must be the natural length of the field type");
	      length = natural_length;
	    }
	}

      offset += start_bit;
    }

  if (offset != *next_struct_offset && ! pos_error)
    sorry ("STRUCT fields must be layed out in monotonically increasing order");

  DECL_PACKED (decl) = 1;
  DECL_BIT_FIELD (decl) = is_discrete;

  if (is_discrete)
    DECL_SIZE (decl) = bitsize_int (length);

  *next_struct_offset += natural_length;

  return 1; /* was POS */
}

tree
layout_chill_struct_type (t)
     tree t;
{
  tree fieldlist = TYPE_FIELDS (t);
  tree x;
  int old_momentary;
  int was_pos;
  int pos_seen = 0;
  int pos_error = 0;
  int next_struct_offset;

  old_momentary = suspend_momentary ();

  /* Process specified field sizes.  */
  next_struct_offset = 0;
  for (x = fieldlist; x; x = TREE_CHAIN (x))
    {
      /* An EVENT or BUFFER mode is implemented as a RECORD_TYPE
	 which may contain a CONST_DECL for the maximum queue size. */
      if (TREE_CODE (x) == CONST_DECL)
	continue;

      /* If any field is const, the structure type is pseudo-const.  */
      /* A field that is pseudo-const makes the structure likewise.  */
      if (TREE_READONLY (x) || TYPE_READONLY_PROPERTY (TREE_TYPE (x)))
	TYPE_FIELDS_READONLY (t) = 1;

      /* Any field that is volatile means variables of this type must be
	 treated in some ways as volatile.  */
      if (TREE_THIS_VOLATILE (x))
	C_TYPE_FIELDS_VOLATILE (t) = 1;

      if (DECL_INITIAL (x) != NULL_TREE)
	{
	  was_pos = apply_chill_field_layout (x, &next_struct_offset);
	  DECL_INITIAL (x) = NULL_TREE;
	}
      else
	{
	  unsigned int min_align = TYPE_ALIGN (TREE_TYPE (x));
	  DECL_ALIGN (x) = MAX (DECL_ALIGN (x), min_align);
	  was_pos = 0;
	}
      if ((! was_pos && pos_seen) || (was_pos && ! pos_seen && x != fieldlist))
	pos_error = 1;
      pos_seen |= was_pos;
    }

  if (pos_error)
    error ("If one field has a POS layout, then all fields must have a POS layout");

  /* Now DECL_INITIAL is null on all fields.  */

  layout_type (t);

  /*  Now we have the truly final field list.
      Store it in this type and in the variants.  */

  TYPE_FIELDS (t) = fieldlist;

  /* If there are lots of fields, sort so we can look through them fast.
     We arbitrarily consider 16 or more elts to be "a lot".  */
  {
    int len = 0;

    for (x = fieldlist; x; x = TREE_CHAIN (x))
      {
	if (len > 15)
	  break;
	len += 1;
      }
    if (len > 15)
      {
	tree *field_array;
	char *space;

	len += list_length (x);
	/* Use the same allocation policy here that make_node uses, to
	   ensure that this lives as long as the rest of the struct decl.
	   All decls in an inline function need to be saved.  */
	if (allocation_temporary_p ())
	  space = savealloc (sizeof (struct lang_type) + len * sizeof (tree));
	else
	  space = oballoc (sizeof (struct lang_type) + len * sizeof (tree));

	TYPE_LANG_SPECIFIC (t) = (struct lang_type *) space;
	TYPE_LANG_SPECIFIC (t)->foo.rec.len = len;

	field_array = &TYPE_LANG_SPECIFIC (t)->foo.rec.elts[0];
	len = 0;
	for (x = fieldlist; x; x = TREE_CHAIN (x))
	  field_array[len++] = x;

	qsort (field_array, len, sizeof (tree),
	       (int (*) PARAMS ((const void *, const void *))) field_decl_cmp);
      }
  }

  for (x = TYPE_MAIN_VARIANT (t); x; x = TYPE_NEXT_VARIANT (x))
    {
      TYPE_FIELDS (x) = TYPE_FIELDS (t);
      TYPE_LANG_SPECIFIC (x) = TYPE_LANG_SPECIFIC (t);
      TYPE_ALIGN (x) = TYPE_ALIGN (t);
    }

  resume_momentary (old_momentary);

  return t;
}

/* Given a list of fields, FIELDLIST, return a structure 
   type that contains these fields.  The returned type is 
   always a new type.  */
tree
build_chill_struct_type (fieldlist)
     tree fieldlist;
{
  register tree t;

  if (fieldlist == NULL_TREE || TREE_CODE (fieldlist) == ERROR_MARK)
    return error_mark_node;

  t = make_chill_struct_type (fieldlist);
  if (pass != 1)
    t = layout_chill_struct_type (t);

/*   pushtag (NULL_TREE, t); */

  return t;
}

/* Fix a LANG_TYPE.  These are used for three different uses:
   - representing a 'READ M' (in which case TYPE_READONLY is set);
   - for a  NEWMODE or SYNMODE (CH_NOVELTY is set for a NEWMODE); and
   - for a parameterised type (TREE_TYPE points to base type,
     while TYPE_DOMAIN is the parameter or parameter list).
   Called from satisfy. */
tree
smash_dummy_type (type)
     tree type;
{
  /* Save fields that we don't want to copy from ORIGIN. */ 
  tree origin = TREE_TYPE (type);
  tree main_tree = TYPE_MAIN_VARIANT (origin);
  int  save_uid = TYPE_UID (type);
  struct obstack *save_obstack = TYPE_OBSTACK (type);
  tree save_name = TYPE_NAME (type);
  int  save_permanent = TREE_PERMANENT (type);
  int  save_readonly = TYPE_READONLY (type);
  tree  save_novelty = CH_NOVELTY (type);
  tree save_domain = TYPE_DOMAIN (type);

  if (origin == NULL_TREE)
    abort ();

  if (save_domain)
    {
      if (TREE_CODE (save_domain) == ERROR_MARK)
	return error_mark_node;
      if (origin == char_type_node)
	{ /* Old-fashioned CHAR(N) declaration. */
	  origin = build_string_type (origin, save_domain);
	}
      else
	{ /* Handle parameterised modes. */
	  int is_varying = chill_varying_type_p (origin);
	  tree new_max = save_domain;
	  tree origin_novelty = CH_NOVELTY (origin);
	  if (is_varying)
	    origin = CH_VARYING_ARRAY_TYPE (origin);
	  if (CH_STRING_TYPE_P (origin))
	    {
	      tree oldindex = TYPE_DOMAIN (origin);
	      new_max = check_range (new_max, new_max, NULL_TREE,
				     fold (build (PLUS_EXPR, integer_type_node,
						  TYPE_MAX_VALUE (oldindex),
						  integer_one_node)));
	      origin = build_string_type (TREE_TYPE (origin), new_max);
	    }
	  else if (TREE_CODE (origin) == ARRAY_TYPE)
	    {
	      tree oldindex = TYPE_DOMAIN (origin);
	      tree upper = check_range (new_max, new_max, NULL_TREE,
					TYPE_MAX_VALUE (oldindex));
	      tree newindex
		= build_chill_range_type (TREE_TYPE (oldindex),
					  TYPE_MIN_VALUE (oldindex), upper);
	      origin = build_simple_array_type (TREE_TYPE (origin), newindex, NULL_TREE);
	    }
	  else if (TREE_CODE (origin) == RECORD_TYPE)
	    {
	      error ("parameterised structures not implemented");
	      return error_mark_node;
	    }
	  else
	    {
	      error ("invalid parameterised type");
	      return error_mark_node;
	    }
	    
	  SET_CH_NOVELTY (origin, origin_novelty);
	  if (is_varying)
	    {
	      origin = build_varying_struct (origin);
	      SET_CH_NOVELTY (origin, origin_novelty);
	    }
	}
      save_domain = NULL_TREE;
    }

  if (TREE_CODE (origin) == ERROR_MARK)
    return error_mark_node;

  *(struct tree_type*)type = *(struct tree_type*)origin;
  /* The following is so that the debug code for
     the copy is different from the original type.
     The two statements usually duplicate each other
     (because they clear fields of the same union),
     but the optimizer should catch that. */
  TYPE_SYMTAB_POINTER (type) = 0;
  TYPE_SYMTAB_ADDRESS (type) = 0;

  /* Restore fields that we didn't want copied from ORIGIN. */
  TYPE_UID (type) = save_uid;
  TYPE_OBSTACK (type) = save_obstack;
  TREE_PERMANENT (type) = save_permanent;
  TYPE_NAME (type) = save_name;

  TREE_CHAIN (type) = NULL_TREE;
  TYPE_VOLATILE (type) = 0;
  TYPE_POINTER_TO (type) = 0;
  TYPE_REFERENCE_TO (type) = 0;

  if (save_readonly)
    { /* TYPE is READ ORIGIN.
	 Add this type to the chain of variants of TYPE.  */
      TYPE_NEXT_VARIANT (type) = TYPE_NEXT_VARIANT (main_tree);
      TYPE_NEXT_VARIANT (main_tree) = type;
      TYPE_READONLY (type) = save_readonly;
    }
  else
    {
      /* TYPE is the copy of the RHS in a NEWMODE or SYNMODE.
       We also get here after old-fashioned CHAR(N) declaration (see above). */
      TYPE_MAIN_VARIANT (type) = type;
      TYPE_NEXT_VARIANT (type) = NULL_TREE;
      if (save_name)
	DECL_ORIGINAL_TYPE (save_name) = origin;

      if (save_novelty != NULL_TREE)  /* A NEWMODE declaration. */
	{
	  CH_NOVELTY (type) = save_novelty;

	  /* Z.200: "If the DEFINING mode of the NEWMODE name is a range mode,
	     then the virtual mode &name is introduced as the PARENT mode
	     of the NEWMODE name. The DEFINING mode of &name is the PARENT
	     mode of the range mode, and the NOVELTY of &name is that of
	     the NEWMODE name." */

	  if (TREE_CODE (type) == INTEGER_TYPE && TREE_TYPE (type))
	    {
	      tree parent;
	      /* PARENT is the virtual mode &name mentioned above. */
	      push_obstacks_nochange ();
	      end_temporary_allocation ();
	      parent = copy_novelty (save_novelty,TREE_TYPE (type));
	      pop_obstacks ();
	      
	      TREE_TYPE (type) = parent;
	      TYPE_MIN_VALUE (type) = convert (parent, TYPE_MIN_VALUE (type));
	      TYPE_MAX_VALUE (type) = convert (parent, TYPE_MAX_VALUE (type));
	    }
	}
    }
  return type;
}

/* This generates a LANG_TYPE node that represents 'READ TYPE'. */

tree
build_readonly_type (type)
     tree type;
{
  tree node = make_node (LANG_TYPE);
  TREE_TYPE (node) = type;
  TYPE_READONLY (node) = 1;
  if (pass != 1)
    node = smash_dummy_type (node);
  return node;
}


/* Return an unsigned type the same as TYPE in other respects.  */

tree
unsigned_type (type)
     tree type;
{
  tree type1 = TYPE_MAIN_VARIANT (type);
  if (type1 == signed_char_type_node || type1 == char_type_node)
    return unsigned_char_type_node;
  if (type1 == integer_type_node)
    return unsigned_type_node;
  if (type1 == short_integer_type_node)
    return short_unsigned_type_node;
  if (type1 == long_integer_type_node)
    return long_unsigned_type_node;
  if (type1 == long_long_integer_type_node)
    return long_long_unsigned_type_node;

  return signed_or_unsigned_type (1, type);
}

/* Return a signed type the same as TYPE in other respects.  */

tree
signed_type (type)
     tree type;
{
  tree type1 = TYPE_MAIN_VARIANT (type);
  while (TREE_CODE (type1) == INTEGER_TYPE && TREE_TYPE (type1) != NULL_TREE)
    type1 = TREE_TYPE (type1);
  if (type1 == unsigned_char_type_node || type1 == char_type_node)
    return signed_char_type_node;
  if (type1 == unsigned_type_node)
    return integer_type_node;
  if (type1 == short_unsigned_type_node)
    return short_integer_type_node;
  if (type1 == long_unsigned_type_node)
    return long_integer_type_node;
  if (type1 == long_long_unsigned_type_node)
    return long_long_integer_type_node;
  if (TYPE_PRECISION (type1) == 1)
    return signed_boolean_type_node;

  return signed_or_unsigned_type (0, type);
}

/* Return a type the same as TYPE except unsigned or
   signed according to UNSIGNEDP.  */

tree
signed_or_unsigned_type (unsignedp, type)
     int unsignedp;
     tree type;
{
  if (! INTEGRAL_TYPE_P (type)
      || TREE_UNSIGNED (type) == unsignedp)
    return type;

  if (TYPE_PRECISION (type) == TYPE_PRECISION (signed_char_type_node))
    return unsignedp ? unsigned_char_type_node : signed_char_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (integer_type_node)) 
    return unsignedp ? unsigned_type_node : integer_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (short_integer_type_node)) 
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (long_integer_type_node)) 
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (long_long_integer_type_node)) 
    return (unsignedp ? long_long_unsigned_type_node
	    : long_long_integer_type_node);
  return type;
}

/* Mark EXP saying that we need to be able to take the
   address of it; it should not be allocated in a register.
   Value is 1 if successful.  */

int
mark_addressable (exp)
     tree exp;
{
  register tree x = exp;
  while (1)
    switch (TREE_CODE (x))
      {
      case ADDR_EXPR:
      case COMPONENT_REF:
      case ARRAY_REF:
      case REALPART_EXPR:
      case IMAGPART_EXPR:
	x = TREE_OPERAND (x, 0);
	break;

      case TRUTH_ANDIF_EXPR:
      case TRUTH_ORIF_EXPR:
      case COMPOUND_EXPR:
	x = TREE_OPERAND (x, 1);
	break;

      case COND_EXPR:
	return mark_addressable (TREE_OPERAND (x, 1))
	  & mark_addressable (TREE_OPERAND (x, 2));

      case CONSTRUCTOR:
	TREE_ADDRESSABLE (x) = 1;
	return 1;

      case INDIRECT_REF:
	/* We sometimes add a cast *(TYPE*)&FOO to handle type and mode
	   incompatibility problems.  Handle this case by marking FOO.  */
	if (TREE_CODE (TREE_OPERAND (x, 0)) == NOP_EXPR
	    && TREE_CODE (TREE_OPERAND (TREE_OPERAND (x, 0), 0)) == ADDR_EXPR)
	  {
	    x = TREE_OPERAND (TREE_OPERAND (x, 0), 0);
	    break;
	  }
	if (TREE_CODE (TREE_OPERAND (x, 0)) == ADDR_EXPR)
	  {
	    x = TREE_OPERAND (x, 0);
	    break;
	  }
	return 1;

      case VAR_DECL:
      case CONST_DECL:
      case PARM_DECL:
      case RESULT_DECL:
	if (DECL_REGISTER (x) && !TREE_ADDRESSABLE (x)
	    && DECL_NONLOCAL (x))
	  {
	    if (TREE_PUBLIC (x))
	      {
		error ("global register variable `%s' used in nested function",
		       IDENTIFIER_POINTER (DECL_NAME (x)));
		return 0;
	      }
	    pedwarn ("register variable `%s' used in nested function",
		     IDENTIFIER_POINTER (DECL_NAME (x)));
	  }
	else if (DECL_REGISTER (x) && !TREE_ADDRESSABLE (x))
	  {
	    if (TREE_PUBLIC (x))
	      {
		error ("address of global register variable `%s' requested",
		       IDENTIFIER_POINTER (DECL_NAME (x)));
		return 0;
	      }

	    /* If we are making this addressable due to its having
	       volatile components, give a different error message.  Also
	       handle the case of an unnamed parameter by not trying
	       to give the name.  */

	    else if (C_TYPE_FIELDS_VOLATILE (TREE_TYPE (x)))
	      {
		error ("cannot put object with volatile field into register");
		return 0;
	      }

	    pedwarn ("address of register variable `%s' requested",
		     IDENTIFIER_POINTER (DECL_NAME (x)));
	  }
	put_var_into_stack (x);

	/* drops through */
      case FUNCTION_DECL:
	TREE_ADDRESSABLE (x) = 1;
#if 0  /* poplevel deals with this now.  */
	if (DECL_CONTEXT (x) == 0)
	  TREE_ADDRESSABLE (DECL_ASSEMBLER_NAME (x)) = 1;
#endif
	/* drops through */
      default:
	return 1;
    }
}

/* Return an integer type with BITS bits of precision,
   that is unsigned if UNSIGNEDP is nonzero, otherwise signed.  */

tree
type_for_size (bits, unsignedp)
     unsigned bits;
     int unsignedp;
{
  if (bits == TYPE_PRECISION (integer_type_node))
    return unsignedp ? unsigned_type_node : integer_type_node;

  if (bits == TYPE_PRECISION (signed_char_type_node))
    return unsignedp ? unsigned_char_type_node : signed_char_type_node;

  if (bits == TYPE_PRECISION (short_integer_type_node))
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;

  if (bits == TYPE_PRECISION (long_integer_type_node))
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;

  if (bits == TYPE_PRECISION (long_long_integer_type_node))
    return (unsignedp ? long_long_unsigned_type_node
	    : long_long_integer_type_node);

  if (bits <= TYPE_PRECISION (intQI_type_node))
    return unsignedp ? unsigned_intQI_type_node : intQI_type_node;

  if (bits <= TYPE_PRECISION (intHI_type_node))
    return unsignedp ? unsigned_intHI_type_node : intHI_type_node;

  if (bits <= TYPE_PRECISION (intSI_type_node))
    return unsignedp ? unsigned_intSI_type_node : intSI_type_node;

  if (bits <= TYPE_PRECISION (intDI_type_node))
    return unsignedp ? unsigned_intDI_type_node : intDI_type_node;

#if HOST_BITS_PER_WIDE_INT >= 64
  if (bits <= TYPE_PRECISION (intTI_type_node))
    return unsignedp ? unsigned_intTI_type_node : intTI_type_node;
#endif

  return 0;
}

/* Return a data type that has machine mode MODE.
   If the mode is an integer,
   then UNSIGNEDP selects between signed and unsigned types.  */

tree
type_for_mode (mode, unsignedp)
     enum machine_mode mode;
     int unsignedp;
{
  if ((int)mode == (int)TYPE_MODE (integer_type_node))
    return unsignedp ? unsigned_type_node : integer_type_node;

  if ((int)mode == (int)TYPE_MODE (signed_char_type_node))
    return unsignedp ? unsigned_char_type_node : signed_char_type_node;

  if ((int)mode == (int)TYPE_MODE (short_integer_type_node))
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;

  if ((int)mode == (int)TYPE_MODE (long_integer_type_node))
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;

  if ((int)mode == (int)TYPE_MODE (long_long_integer_type_node))
    return unsignedp ? long_long_unsigned_type_node : long_long_integer_type_node;

  if ((int)mode == (int)TYPE_MODE (intQI_type_node))
    return unsignedp ? unsigned_intQI_type_node : intQI_type_node;

  if ((int)mode == (int)TYPE_MODE (intHI_type_node))
    return unsignedp ? unsigned_intHI_type_node : intHI_type_node;

  if ((int)mode == (int)TYPE_MODE (intSI_type_node))
    return unsignedp ? unsigned_intSI_type_node : intSI_type_node;

  if ((int)mode == (int)TYPE_MODE (intDI_type_node))
    return unsignedp ? unsigned_intDI_type_node : intDI_type_node;

#if HOST_BITS_PER_WIDE_INT >= 64
  if ((int)mode == (int)TYPE_MODE (intTI_type_node))
    return unsignedp ? unsigned_intTI_type_node : intTI_type_node;
#endif

  if ((int)mode == (int)TYPE_MODE (float_type_node))
    return float_type_node;

  if ((int)mode == (int)TYPE_MODE (double_type_node))
    return double_type_node;

  if ((int)mode == (int)TYPE_MODE (long_double_type_node))
    return long_double_type_node;

  if ((int)mode == (int)TYPE_MODE (build_pointer_type (char_type_node)))
    return build_pointer_type (char_type_node);

  if ((int)mode == (int)TYPE_MODE (build_pointer_type (integer_type_node)))
    return build_pointer_type (integer_type_node);

  return 0;
}
