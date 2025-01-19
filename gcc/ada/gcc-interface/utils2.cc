/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                               U T I L S 2                                *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *          Copyright (C) 1992-2025, Free Software Foundation, Inc.         *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have received a copy of the GNU General   *
 * Public License along with GCC; see the file COPYING3.  If not see        *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "memmodel.h"
#include "tm.h"
#include "vec.h"
#include "alias.h"
#include "tree.h"
#include "inchash.h"
#include "builtins.h"
#include "expmed.h"
#include "fold-const.h"
#include "optabs-query.h"
#include "stor-layout.h"
#include "stringpool.h"
#include "varasm.h"
#include "flags.h"
#include "toplev.h"
#include "ggc.h"
#include "tree-inline.h"

#include "ada.h"
#include "types.h"
#include "atree.h"
#include "elists.h"
#include "namet.h"
#include "nlists.h"
#include "snames.h"
#include "stringt.h"
#include "uintp.h"
#include "fe.h"
#include "sinfo.h"
#include "einfo.h"
#include "ada-tree.h"
#include "gigi.h"

/* Return the base type of TYPE.  */

tree
get_base_type (tree type)
{
  if (TREE_CODE (type) == RECORD_TYPE
      && TYPE_JUSTIFIED_MODULAR_P (type))
    type = TREE_TYPE (TYPE_FIELDS (type));

  while (TREE_TYPE (type)
	 && (TREE_CODE (type) == INTEGER_TYPE
	     || SCALAR_FLOAT_TYPE_P (type)))
    type = TREE_TYPE (type);

  return type;
}

/* EXP is a GCC tree representing an address.  See if we can find how strictly
   the object at this address is aligned and, if so, return the alignment of
   the object in bits.  Otherwise return 0.  */

unsigned int
known_alignment (tree exp)
{
  unsigned int this_alignment;
  unsigned int lhs, rhs;

  switch (TREE_CODE (exp))
    {
    CASE_CONVERT:
    case VIEW_CONVERT_EXPR:
    case NON_LVALUE_EXPR:
      /* Conversions between pointers and integers don't change the alignment
	 of the underlying object.  */
      this_alignment = known_alignment (TREE_OPERAND (exp, 0));
      break;

    case COMPOUND_EXPR:
      /* The value of a COMPOUND_EXPR is that of its second operand.  */
      this_alignment = known_alignment (TREE_OPERAND (exp, 1));
      break;

    case PLUS_EXPR:
    case MINUS_EXPR:
      /* If two addresses are added, the alignment of the result is the
	 minimum of the two alignments.  */
      lhs = known_alignment (TREE_OPERAND (exp, 0));
      rhs = known_alignment (TREE_OPERAND (exp, 1));
      this_alignment = MIN (lhs, rhs);
      break;

    case POINTER_PLUS_EXPR:
      /* If this is the pattern built for aligning types, decode it.  */
      if (TREE_CODE (TREE_OPERAND (exp, 1)) == BIT_AND_EXPR
	  && TREE_CODE (TREE_OPERAND (TREE_OPERAND (exp, 1), 0)) == NEGATE_EXPR)
	{
	  tree op = TREE_OPERAND (TREE_OPERAND (exp, 1), 1);
	  return
	    known_alignment (fold_build1 (BIT_NOT_EXPR, TREE_TYPE (op), op));
	}

      /* If we don't know the alignment of the offset, we assume that
	 of the base.  */
      lhs = known_alignment (TREE_OPERAND (exp, 0));
      rhs = known_alignment (TREE_OPERAND (exp, 1));

      if (rhs == 0)
	this_alignment = lhs;
      else
	this_alignment = MIN (lhs, rhs);
      break;

    case COND_EXPR:
      /* If there is a choice between two values, use the smaller one.  */
      lhs = known_alignment (TREE_OPERAND (exp, 1));
      rhs = known_alignment (TREE_OPERAND (exp, 2));
      this_alignment = MIN (lhs, rhs);
      break;

    case INTEGER_CST:
      {
	unsigned HOST_WIDE_INT c = TREE_INT_CST_LOW (exp);
	/* The first part of this represents the lowest bit in the constant,
	   but it is originally in bytes, not bits.  */
	this_alignment = (c & -c) * BITS_PER_UNIT;
      }
      break;

    case MULT_EXPR:
      /* If we know the alignment of just one side, use it.  Otherwise,
	 use the product of the alignments.  */
      lhs = known_alignment (TREE_OPERAND (exp, 0));
      rhs = known_alignment (TREE_OPERAND (exp, 1));

      if (lhs == 0)
	this_alignment = rhs;
      else if (rhs == 0)
	this_alignment = lhs;
      else
	this_alignment = MIN (lhs * rhs, BIGGEST_ALIGNMENT);
      break;

    case BIT_AND_EXPR:
      /* A bit-and expression is as aligned as the maximum alignment of the
	 operands.  We typically get here for a complex lhs and a constant
	 negative power of two on the rhs to force an explicit alignment, so
	 don't bother looking at the lhs.  */
      this_alignment = known_alignment (TREE_OPERAND (exp, 1));
      break;

    case ADDR_EXPR:
      if (DECL_P (TREE_OPERAND (exp, 0)))
	this_alignment = DECL_ALIGN (TREE_OPERAND (exp, 0));
      else
	this_alignment = get_object_alignment (TREE_OPERAND (exp, 0));
      break;

    case CALL_EXPR:
      {
	tree fndecl = get_callee_fndecl (exp);
	if (fndecl == malloc_decl || fndecl == realloc_decl)
	  return get_target_system_allocator_alignment () * BITS_PER_UNIT;

	tree t = maybe_inline_call_in_expr (exp);
	if (t)
	  return known_alignment (t);
      }

      /* ... fall through ... */

    default:
      /* For other pointer expressions, we assume that the pointed-to object
	 is at least as aligned as the pointed-to type.  Beware that we can
	 have a dummy type here (e.g. a Taft Amendment type), for which the
	 alignment is meaningless and should be ignored.  */
      if (POINTER_TYPE_P (TREE_TYPE (exp))
	  && !TYPE_IS_DUMMY_P (TREE_TYPE (TREE_TYPE (exp)))
	  && !VOID_TYPE_P (TREE_TYPE (TREE_TYPE (exp))))
	this_alignment = TYPE_ALIGN (TREE_TYPE (TREE_TYPE (exp)));
      else
	this_alignment = 0;
      break;
    }

  return this_alignment;
}

/* We have a comparison or assignment operation on two types, T1 and T2, which
   are either both array types or both record types.  T1 is assumed to be for
   the left hand side operand, and T2 for the right hand side.  Return the
   type that both operands should be converted to for the operation, if any.
   Otherwise return zero.  */

static tree
find_common_type (tree t1, tree t2)
{
  /* ??? As of today, various constructs lead to here with types of different
     sizes even when both constants (e.g. tagged types, packable vs regular
     component types, padded vs unpadded types, ...).  While some of these
     would better be handled upstream (types should be made consistent before
     calling into build_binary_op), some others are really expected and we
     have to be careful.  */

  const bool variable_record_on_lhs
    = (TREE_CODE (t1) == RECORD_TYPE
       && TREE_CODE (t2) == RECORD_TYPE
       && get_variant_part (t1)
       && !get_variant_part (t2));

  const bool variable_array_on_lhs
    = (TREE_CODE (t1) == ARRAY_TYPE
       && TREE_CODE (t2) == ARRAY_TYPE
       && !TREE_CONSTANT (TYPE_MIN_VALUE (TYPE_DOMAIN (t1)))
       && TREE_CONSTANT (TYPE_MIN_VALUE (TYPE_DOMAIN (t2))));

  /* We must avoid writing more than what the target can hold if this is for
     an assignment and the case of tagged types is handled in build_binary_op
     so we use the lhs type if it is known to be smaller or of constant size
     and the rhs type is not, whatever the modes.  We also force t1 in case of
     constant size equality to minimize occurrences of view conversions on the
     lhs of an assignment, except for the case of types with a variable part
     on the lhs but not on the rhs to make the conversion simpler.  */
  if (TREE_CONSTANT (TYPE_SIZE (t1))
      && (!TREE_CONSTANT (TYPE_SIZE (t2))
	  || tree_int_cst_lt (TYPE_SIZE (t1), TYPE_SIZE (t2))
	  || (TYPE_SIZE (t1) == TYPE_SIZE (t2)
	      && !variable_record_on_lhs
	      && !variable_array_on_lhs)))
    return t1;

  /* Otherwise, if the lhs type is non-BLKmode, use it, except for the case of
     a non-BLKmode rhs and array types with a variable part on the lhs but not
     on the rhs to make sure the conversion is preserved during gimplification.
     Note that we know that we will not have any alignment problems since, if
     we did, the non-BLKmode type could not have been used.  */
  if (TYPE_MODE (t1) != BLKmode
      && (TYPE_MODE (t2) == BLKmode || !variable_array_on_lhs))
    return t1;

  /* If the rhs type is of constant size, use it whatever the modes.  At
     this point it is known to be smaller, or of constant size and the
     lhs type is not.  */
  if (TREE_CONSTANT (TYPE_SIZE (t2)))
    return t2;

  /* Otherwise, if the rhs type is non-BLKmode, use it.  */
  if (TYPE_MODE (t2) != BLKmode)
    return t2;

  /* In this case, both types have variable size and BLKmode.  It's
     probably best to leave the "type mismatch" because changing it
     could cause a bad self-referential reference.  */
  return NULL_TREE;
}

/* Return an expression tree representing an equality comparison of A1 and A2,
   two objects of type ARRAY_TYPE.  The result should be of type RESULT_TYPE.

   Two arrays are equal in one of two ways: (1) if both have zero length in
   some dimension (not necessarily the same dimension) or (2) if the lengths
   in each dimension are equal and the data is equal.  We perform the length
   tests in as efficient a manner as possible.  */

static tree
compare_arrays_for_equality (location_t loc, tree result_type, tree a1, tree a2)
{
  tree result = convert (result_type, boolean_true_node);
  tree a1_is_null = convert (result_type, boolean_false_node);
  tree a2_is_null = convert (result_type, boolean_false_node);
  tree t1 = TREE_TYPE (a1);
  tree t2 = TREE_TYPE (a2);
  bool a1_side_effects_p = TREE_SIDE_EFFECTS (a1);
  bool a2_side_effects_p = TREE_SIDE_EFFECTS (a2);
  bool length_zero_p = false;

  /* If the operands have side-effects, they need to be evaluated only once
     in spite of the multiple references in the comparison.  */
  if (a1_side_effects_p)
    a1 = gnat_protect_expr (a1);

  if (a2_side_effects_p)
    a2 = gnat_protect_expr (a2);

  /* Process each dimension separately and compare the lengths.  If any
     dimension has a length known to be zero, set LENGTH_ZERO_P to true
     in order to suppress the comparison of the data at the end.  */
  while (TREE_CODE (t1) == ARRAY_TYPE && TREE_CODE (t2) == ARRAY_TYPE)
    {
      tree dom1 = TYPE_DOMAIN (t1);
      tree dom2 = TYPE_DOMAIN (t2);
      tree length1 = size_binop (PLUS_EXPR,
				 size_binop (MINUS_EXPR,
					     TYPE_MAX_VALUE (dom1),
					     TYPE_MIN_VALUE (dom1)),
				 size_one_node);
      tree length2 = size_binop (PLUS_EXPR,
				 size_binop (MINUS_EXPR,
					     TYPE_MAX_VALUE (dom2),
					     TYPE_MIN_VALUE (dom2)),
				 size_one_node);
      tree ind1 = TYPE_INDEX_TYPE (dom1);
      tree ind2 = TYPE_INDEX_TYPE (dom2);
      tree base_type = maybe_character_type (get_base_type (ind1));
      tree lb1 = convert (base_type, TYPE_MIN_VALUE (ind1));
      tree ub1 = convert (base_type, TYPE_MAX_VALUE (ind1));
      tree lb2 = convert (base_type, TYPE_MIN_VALUE (ind2));
      tree ub2 = convert (base_type, TYPE_MAX_VALUE (ind2));
      tree comparison, this_a1_is_null, this_a2_is_null;

      /* If the length of the first array is a constant and that of the second
	 array is not, swap our operands to have the constant second.  */
      if (TREE_CODE (length1) == INTEGER_CST
	  && TREE_CODE (length2) != INTEGER_CST)
	{
	  tree tem;
	  bool btem;

	  tem = a1, a1 = a2, a2 = tem;
	  tem = t1, t1 = t2, t2 = tem;
	  tem = lb1, lb1 = lb2, lb2 = tem;
	  tem = ub1, ub1 = ub2, ub2 = tem;
	  tem = length1, length1 = length2, length2 = tem;
	  tem = a1_is_null, a1_is_null = a2_is_null, a2_is_null = tem;
	  btem = a1_side_effects_p, a1_side_effects_p = a2_side_effects_p,
	  a2_side_effects_p = btem;
	}

      /* If the length of the second array is the constant zero, we can just
	 use the original stored bounds for the first array and see whether
	 last < first holds.  */
      if (integer_zerop (length2))
	{
	  length_zero_p = true;

	  lb1 = SUBSTITUTE_PLACEHOLDER_IN_EXPR (lb1, a1);
	  ub1 = SUBSTITUTE_PLACEHOLDER_IN_EXPR (ub1, a1);

	  comparison = fold_build2_loc (loc, LT_EXPR, result_type, ub1, lb1);

	  this_a1_is_null = comparison;
	  this_a2_is_null = convert (result_type, boolean_true_node);
	}

      /* Otherwise, if the length is some other constant value, we know that
	 this dimension in the second array cannot be superflat, so we can
	 just use its length computed from the actual stored bounds.  */
      else if (TREE_CODE (length2) == INTEGER_CST)
	{
	  /* Note that we know that LB2 and UB2 are constant and hence
	     cannot contain a PLACEHOLDER_EXPR.  */
	  lb1 = SUBSTITUTE_PLACEHOLDER_IN_EXPR (lb1, a1);
	  ub1 = SUBSTITUTE_PLACEHOLDER_IN_EXPR (ub1, a1);

	  comparison
	    = fold_build2_loc (loc, EQ_EXPR, result_type,
			       build_binary_op (MINUS_EXPR, base_type,
						ub1, lb1),
			       build_binary_op (MINUS_EXPR, base_type,
						ub2, lb2));
	  this_a1_is_null
	    = fold_build2_loc (loc, LT_EXPR, result_type, ub1, lb1);

	  this_a2_is_null = convert (result_type, boolean_false_node);
	}

      /* Otherwise, compare the computed lengths.  */
      else
	{
	  length1 = SUBSTITUTE_PLACEHOLDER_IN_EXPR (length1, a1);
	  length2 = SUBSTITUTE_PLACEHOLDER_IN_EXPR (length2, a2);

	  comparison
	    = fold_build2_loc (loc, EQ_EXPR, result_type, length1, length2);

	  lb1 = SUBSTITUTE_PLACEHOLDER_IN_EXPR (lb1, a1);
	  ub1 = SUBSTITUTE_PLACEHOLDER_IN_EXPR (ub1, a1);

	  this_a1_is_null
	    = fold_build2_loc (loc, LT_EXPR, result_type, ub1, lb1);

	  lb2 = SUBSTITUTE_PLACEHOLDER_IN_EXPR (lb2, a2);
	  ub2 = SUBSTITUTE_PLACEHOLDER_IN_EXPR (ub2, a2);

	  this_a2_is_null
	    = fold_build2_loc (loc, LT_EXPR, result_type, ub2, lb2);
	}

      /* Append expressions for this dimension to the final expressions.  */
      result = build_binary_op (TRUTH_ANDIF_EXPR, result_type,
				result, comparison);

      a1_is_null = build_binary_op (TRUTH_ORIF_EXPR, result_type,
				    this_a1_is_null, a1_is_null);

      a2_is_null = build_binary_op (TRUTH_ORIF_EXPR, result_type,
				    this_a2_is_null, a2_is_null);

      t1 = TREE_TYPE (t1);
      t2 = TREE_TYPE (t2);
    }

  /* Unless the length of some dimension is known to be zero, compare the
     data in the array.  */
  if (!length_zero_p)
    {
      tree type = find_common_type (TREE_TYPE (a1), TREE_TYPE (a2));
      tree comparison;

      if (type)
	{
	  a1 = convert (type, a1),
	  a2 = convert (type, a2);
	}

      comparison = fold_build2_loc (loc, EQ_EXPR, result_type, a1, a2);

      result
	= build_binary_op (TRUTH_ANDIF_EXPR, result_type, result, comparison);
    }

  /* The result is also true if both sizes are zero.  */
  result = build_binary_op (TRUTH_ORIF_EXPR, result_type,
			    build_binary_op (TRUTH_ANDIF_EXPR, result_type,
					     a1_is_null, a2_is_null),
			    result);

  /* If the operands have side-effects, they need to be evaluated before
     doing the tests above since the place they otherwise would end up
     being evaluated at run time could be wrong.  */
  if (a1_side_effects_p)
    result = build2 (COMPOUND_EXPR, result_type, a1, result);

  if (a2_side_effects_p)
    result = build2 (COMPOUND_EXPR, result_type, a2, result);

  return result;
}

/* Return an expression tree representing an ordering comparison of A1 and A2,
   two objects of type ARRAY_TYPE.  The result should be of type RESULT_TYPE.

   A1 is less than A2 according to the following alternative:
     - when A1's length is less than A2'length: if every element of A1 is equal
       to its counterpart in A2 or the first differing is lesser in A1 than A2,
     - otherwise: if not every element of A2 is equal to its counterpart in A1
       and the first differing is lesser in A1 than A2.

   The other 3 ordering comparisons can be easily deduced from this one.  */

static tree
compare_arrays_for_ordering (location_t loc, tree result_type, tree a1, tree a2)
{
  const bool a1_side_effects_p = TREE_SIDE_EFFECTS (a1);
  const bool a2_side_effects_p = TREE_SIDE_EFFECTS (a2);
  tree t1 = TREE_TYPE (a1);
  tree t2 = TREE_TYPE (a2);
  tree dom1 = TYPE_DOMAIN (t1);
  tree dom2 = TYPE_DOMAIN (t2);
  tree length1 = size_binop (PLUS_EXPR,
			     size_binop (MINUS_EXPR,
					 TYPE_MAX_VALUE (dom1),
					 TYPE_MIN_VALUE (dom1)),
			     size_one_node);
  tree length2 = size_binop (PLUS_EXPR,
			     size_binop (MINUS_EXPR,
					 TYPE_MAX_VALUE (dom2),
					 TYPE_MIN_VALUE (dom2)),
			     size_one_node);
  tree addr1, addr2, fndecl, result;

  /* If the lengths are known at compile time, fold the alternative and let the
     gimplifier optimize the case of power-of-two lengths.  */
  if (TREE_CODE (length1) == INTEGER_CST && TREE_CODE (length2) == INTEGER_CST)
    return tree_int_cst_compare (length1, length2) < 0
	   ? fold_build2_loc (loc, LE_EXPR, result_type, a1, convert (t1, a2))
	   : fold_build2_loc (loc, LT_EXPR, result_type, convert (t2, a1), a2);

  /* If the operands have side-effects, they need to be evaluated only once
     in spite of the multiple references in the comparison.  */
  if (a1_side_effects_p)
    a1 = gnat_protect_expr (a1);

  if (a2_side_effects_p)
    a2 = gnat_protect_expr (a2);

  length1 = SUBSTITUTE_PLACEHOLDER_IN_EXPR (length1, a1);
  length2 = SUBSTITUTE_PLACEHOLDER_IN_EXPR (length2, a2);

  /* If the lengths are not known at compile time, call memcmp directly with
     the actual lengths since a1 and a2 may have the same nominal subtype.  */
  addr1 = build_fold_addr_expr_loc (loc, a1);
  addr2 = build_fold_addr_expr_loc (loc, a2);
  fndecl = builtin_decl_implicit (BUILT_IN_MEMCMP);

  result
    = fold_build3_loc (loc, COND_EXPR, result_type,
		       fold_build2_loc (loc, LT_EXPR, boolean_type_node,
				        length1, length2),
		       fold_build2_loc (loc, LE_EXPR, result_type,
				        build_call_expr_loc (loc, fndecl, 3,
							     addr1, addr2,
							     length1),
					integer_zero_node),
		       fold_build2_loc (loc, LT_EXPR, result_type,
					build_call_expr_loc (loc, fndecl, 3,
							     addr1, addr2,
							     length2),
					integer_zero_node));

  /* If the operands have side-effects, they need to be evaluated before
     doing the tests above since the place they otherwise would end up
     being evaluated at run time could be wrong.  */
  if (a1_side_effects_p)
    result = build2 (COMPOUND_EXPR, result_type, a1, result);

  if (a2_side_effects_p)
    result = build2 (COMPOUND_EXPR, result_type, a2, result);

  return result;
}

/* Return an expression tree representing an equality comparison of P1 and P2,
   two objects of fat pointer type.  The result should be of type RESULT_TYPE.

   Two fat pointers are equal in one of two ways: (1) if both have a null
   pointer to the array or (2) if they contain the same couple of pointers.
   We perform the comparison in as efficient a manner as possible.  */

static tree
compare_fat_pointers (location_t loc, tree result_type, tree p1, tree p2)
{
  tree p1_array, p2_array, p1_bounds, p2_bounds, same_array, same_bounds;
  tree p1_array_is_null, p2_array_is_null;

  /* If either operand has side-effects, they have to be evaluated only once
     in spite of the multiple references to the operand in the comparison.  */
  p1 = gnat_protect_expr (p1);
  p2 = gnat_protect_expr (p2);

  /* The constant folder doesn't fold fat pointer types so we do it here.  */
  if (TREE_CODE (p1) == CONSTRUCTOR)
    p1_array = CONSTRUCTOR_ELT (p1, 0)->value;
  else
    p1_array = build_component_ref (p1, TYPE_FIELDS (TREE_TYPE (p1)), true);

  p1_array_is_null
    = fold_build2_loc (loc, EQ_EXPR, result_type, p1_array,
		       fold_convert_loc (loc, TREE_TYPE (p1_array),
					 null_pointer_node));

  if (TREE_CODE (p2) == CONSTRUCTOR)
    p2_array = CONSTRUCTOR_ELT (p2, 0)->value;
  else
    p2_array = build_component_ref (p2, TYPE_FIELDS (TREE_TYPE (p2)), true);

  p2_array_is_null
    = fold_build2_loc (loc, EQ_EXPR, result_type, p2_array,
		       fold_convert_loc (loc, TREE_TYPE (p2_array),
					 null_pointer_node));

  /* If one of the pointers to the array is null, just compare the other.  */
  if (integer_zerop (p1_array))
    return p2_array_is_null;
  else if (integer_zerop (p2_array))
    return p1_array_is_null;

  /* Otherwise, do the fully-fledged comparison.  */
  same_array
    = fold_build2_loc (loc, EQ_EXPR, result_type, p1_array, p2_array);

  if (TREE_CODE (p1) == CONSTRUCTOR)
    p1_bounds = CONSTRUCTOR_ELT (p1, 1)->value;
  else
    p1_bounds
      = build_component_ref (p1, DECL_CHAIN (TYPE_FIELDS (TREE_TYPE (p1))),
			     true);

  if (TREE_CODE (p2) == CONSTRUCTOR)
    p2_bounds = CONSTRUCTOR_ELT (p2, 1)->value;
  else
    p2_bounds
      = build_component_ref (p2, DECL_CHAIN (TYPE_FIELDS (TREE_TYPE (p2))),
			     true);

  same_bounds
    = fold_build2_loc (loc, EQ_EXPR, result_type, p1_bounds, p2_bounds);

  /* P1_ARRAY == P2_ARRAY && (P1_ARRAY == NULL || P1_BOUNDS == P2_BOUNDS).  */
  return build_binary_op (TRUTH_ANDIF_EXPR, result_type, same_array,
			  build_binary_op (TRUTH_ORIF_EXPR, result_type,
					   p1_array_is_null, same_bounds));
}

/* Try to compute the reduction of OP modulo MODULUS in PRECISION bits with a
   division-free algorithm.  Return NULL_TREE if this is not easily doable.  */

tree
fast_modulo_reduction (tree op, tree modulus, unsigned int precision)
{
  const tree type = TREE_TYPE (op);
  const unsigned int type_precision = TYPE_PRECISION (type);

  /* The implementation is host-dependent for the time being.  */
  if (type_precision <= HOST_BITS_PER_WIDE_INT)
    {
      const unsigned HOST_WIDE_INT d = tree_to_uhwi (modulus);
      unsigned HOST_WIDE_INT ml, mh;
      int pre_shift, post_shift;
      tree t;

      /* The trick is to replace the division by d with a multiply-and-shift
	 sequence parameterized by a (multiplier, shifter) pair computed from
	 d, the precision of the type and the needed precision:

	   op / d = (op * multiplier) >> shifter

	 But choose_multiplier provides a slightly different interface:

	  op / d = (op h* multiplier) >> reduced_shifter

	 that makes things easier by using a high-part multiplication.  */
      mh = choose_multiplier (d, type_precision, precision, &ml, &post_shift);

      /* If the suggested multiplier is more than TYPE_PRECISION bits, we can
	 do better for even divisors, using an initial right shift.  */
      if (mh != 0 && (d & 1) == 0)
	{
	  pre_shift = ctz_or_zero (d);
	  mh = choose_multiplier (d >> pre_shift, type_precision,
				  precision - pre_shift, &ml, &post_shift);
	}
      else
	pre_shift = 0;

      /* If the suggested multiplier is still more than TYPE_PRECISION bits,
	 or the TYPE_MODE does not have a high-part multiply, try again with
	 a larger type up to the word size.  */
      if (mh != 0 || !can_mult_highpart_p (TYPE_MODE (type), true))
	{
	  if (type_precision < BITS_PER_WORD)
	    {
	      const scalar_int_mode m
		= smallest_int_mode_for_size (type_precision + 1).require ();
	      tree new_type = gnat_type_for_mode (m, 1);
	      op = fold_convert (new_type, op);
	      modulus = fold_convert (new_type, modulus);
	      t = fast_modulo_reduction (op, modulus, precision);
	      if (t)
		return fold_convert (type, t);
	    }

	  return NULL_TREE;
	}

      /* This computes op - (op / modulus) * modulus with PRECISION bits.  */
      op = gnat_protect_expr (op);

      /* t = op >> pre_shift
	 t = t h* ml
	 t = t >> post_shift
	 t = t * modulus  */
      if (pre_shift)
	t = fold_build2 (RSHIFT_EXPR, type, op,
			 build_int_cst (type, pre_shift));
      else
	t = op;
      t = fold_build2 (MULT_HIGHPART_EXPR, type, t, build_int_cst (type, ml));
      if (post_shift)
	t = fold_build2 (RSHIFT_EXPR, type, t,
			 build_int_cst (type, post_shift));
      t = fold_build2 (MULT_EXPR, type, t, modulus);

      return fold_build2 (MINUS_EXPR, type, op, t);
    }

  else
    return NULL_TREE;
}

/* Compute the result of applying OP_CODE to LHS and RHS, where both are of
   TYPE.  We know that TYPE is a modular type with a nonbinary modulus.  */

static tree
nonbinary_modular_operation (enum tree_code op_code, tree type, tree lhs,
                             tree rhs)
{
  tree modulus = TYPE_MODULUS (type);
  unsigned precision = tree_floor_log2 (modulus) + 1;
  tree op_type, result, fmr;

  /* For the logical operations, we only need PRECISION bits.  For addition and
     subtraction, we need one more, and for multiplication twice as many.  */
  if (op_code == PLUS_EXPR || op_code == MINUS_EXPR)
    precision += 1;
  else if (op_code == MULT_EXPR)
    precision *= 2;

  /* If the type is not wide enough, make a new type of the needed precision
     and convert modulus and operands to it.  Use a type with full precision
     for its mode since operations are ultimately performed in the mode.  */
  if (TYPE_PRECISION (type) < precision)
    {
      const scalar_int_mode m
	= smallest_int_mode_for_size (precision).require ();
      op_type = gnat_type_for_mode (m, 1);
      modulus = fold_convert (op_type, modulus);
      lhs = fold_convert (op_type, lhs);
      rhs = fold_convert (op_type, rhs);
    }
  else
    op_type = type;

  /* Do the operation, then we'll fix it up.  */
  result = fold_build2 (op_code, op_type, lhs, rhs);

  /* Unconditionally add the modulus to the result for a subtraction, this gets
     rid of all its peculiarities by cancelling out the addition of the binary
     modulus in the case where the subtraction wraps around in OP_TYPE, and may
     even generate better code on architectures with conditional moves.  */
  if (op_code == MINUS_EXPR)
    result = fold_build2 (PLUS_EXPR, op_type, result, modulus);

  /* For a multiplication, we first try to do a modulo reduction by means of a
     (multiplier, shifter) pair in the needed precision up to the word size, or
     else we fall back to a standard modulo operation.  But not when optimizing
     for size, because it will be longer than a div+mul+sub sequence.  */
  if (op_code == MULT_EXPR)
    {
      if (!optimize_size
	  && precision <= BITS_PER_WORD
	  && (fmr = fast_modulo_reduction (result, modulus, precision)))
	result = fmr;
      else
	result = fold_build2 (TRUNC_MOD_EXPR, op_type, result, modulus);
    }

  /* For the other operations, subtract the modulus if we are >= it.  */
  else
    {
      result = gnat_protect_expr (result);
      result = fold_build3 (COND_EXPR, op_type,
			    fold_build2 (GE_EXPR, boolean_type_node,
					 result, modulus),
			    fold_build2 (MINUS_EXPR, op_type,
					 result, modulus),
			    result);
    }

  return fold_convert (type, result);
}

/* This page contains routines that implement the Ada semantics with regard
   to atomic objects.  They are fully piggybacked on the middle-end support
   for atomic loads and stores.

   *** Memory barriers and volatile objects ***

   We implement the weakened form of the C.6(16) clause that was introduced
   in Ada 2012 (AI05-117).  Earlier forms of this clause wouldn't have been
   implementable without significant performance hits on modern platforms.

   We also take advantage of the requirements imposed on shared variables by
   9.10 (conditions for sequential actions) to have non-erroneous execution
   and consider that C.6(16) and C.6(17) only prescribe an uniform order of
   volatile updates with regard to sequential actions, i.e. with regard to
   reads or updates of atomic objects.

   As such, an update of an atomic object by a task requires that all earlier
   accesses to volatile objects have completed.  Similarly, later accesses to
   volatile objects cannot be reordered before the update of the atomic object.
   So, memory barriers both before and after the atomic update are needed.

   For a read of an atomic object, to avoid seeing writes of volatile objects
   by a task earlier than by the other tasks, a memory barrier is needed before
   the atomic read.  Finally, to avoid reordering later reads or updates of
   volatile objects to before the atomic read, a barrier is needed after the
   atomic read.

   So, memory barriers are needed before and after atomic reads and updates.
   And, in order to simplify the implementation, we use full memory barriers
   in all cases, i.e. we enforce sequential consistency for atomic accesses.  */

/* Return the size of TYPE, which must be a positive power of 2.  */

unsigned int
resolve_atomic_size (tree type)
{
  unsigned HOST_WIDE_INT size = tree_to_uhwi (TYPE_SIZE_UNIT (type));

  if (size == 1 || size == 2 || size == 4 || size == 8 || size == 16)
    return size;

  /* We shouldn't reach here without having already detected that the size
     isn't compatible with an atomic access.  */
  gcc_assert (Serious_Errors_Detected);

  return 0;
}

/* Build an atomic load for the underlying atomic object in SRC.  SYNC is
   true if the load requires synchronization.  */

tree
build_atomic_load (tree src, bool sync)
{
  tree ptr_type
    = build_pointer_type
      (build_qualified_type (void_type_node,
			     TYPE_QUAL_ATOMIC | TYPE_QUAL_VOLATILE));
  tree mem_model
    = build_int_cst (integer_type_node,
		     sync ? MEMMODEL_SEQ_CST : MEMMODEL_RELAXED);
  tree orig_src = src;
  tree type, t, addr, val;
  unsigned int size;
  int fncode;

  /* Remove conversions to get the address of the underlying object.  */
  src = remove_conversions (src, false);
  type = TREE_TYPE (src);
  size = resolve_atomic_size (type);
  if (size == 0)
    return orig_src;

  fncode = (int) BUILT_IN_ATOMIC_LOAD_N + exact_log2 (size) + 1;
  t = builtin_decl_implicit ((enum built_in_function) fncode);

  addr = build_unary_op (ADDR_EXPR, ptr_type, src);
  val = build_call_expr (t, 2, addr, mem_model);

  /* First reinterpret the loaded bits in the original type of the load,
     then convert to the expected result type.  */
  t = fold_build1 (VIEW_CONVERT_EXPR, type, val);
  return convert (TREE_TYPE (orig_src), t);
}

/* Build an atomic store from SRC to the underlying atomic object in DEST.
   SYNC is true if the store requires synchronization.  */

tree
build_atomic_store (tree dest, tree src, bool sync)
{
  tree ptr_type
    = build_pointer_type
      (build_qualified_type (void_type_node,
			     TYPE_QUAL_ATOMIC | TYPE_QUAL_VOLATILE));
  tree mem_model
    = build_int_cst (integer_type_node,
		     sync ? MEMMODEL_SEQ_CST : MEMMODEL_RELAXED);
  tree orig_dest = dest;
  tree type, t, int_type, addr;
  unsigned int size;
  int fncode;

  /* Remove conversions to get the address of the underlying object.  */
  dest = remove_conversions (dest, false);
  type = TREE_TYPE (dest);
  size = resolve_atomic_size (type);
  if (size == 0)
    return build_binary_op (MODIFY_EXPR, NULL_TREE, orig_dest, src);

  fncode = (int) BUILT_IN_ATOMIC_STORE_N + exact_log2 (size) + 1;
  t = builtin_decl_implicit ((enum built_in_function) fncode);
  int_type = gnat_type_for_size (BITS_PER_UNIT * size, 1);

  /* First convert the bits to be stored to the original type of the store,
     then reinterpret them in the effective type.  But if the original type
     is a padded type with the same size, convert to the inner type instead,
     as we don't want to artificially introduce a CONSTRUCTOR here.  */
  if (TYPE_IS_PADDING_P (type)
      && TYPE_SIZE (type) == TYPE_SIZE (TREE_TYPE (TYPE_FIELDS (type))))
    src = convert (TREE_TYPE (TYPE_FIELDS (type)), src);
  else
    src = convert (type, src);
  src = fold_build1 (VIEW_CONVERT_EXPR, int_type, src);
  addr = build_unary_op (ADDR_EXPR, ptr_type, dest);

  return build_call_expr (t, 3, addr, src, mem_model);
}

/* Build a load-modify-store sequence from SRC to DEST.  GNAT_NODE is used for
   the location of the sequence.  Note that, even though the load and the store
   are both atomic, the sequence itself is not atomic.  */

tree
build_load_modify_store (tree dest, tree src, Node_Id gnat_node)
{
  /* We will be modifying DEST below so we build a copy.  */
  dest = copy_node (dest);
  tree ref = dest;

  while (handled_component_p (ref))
    {
      /* The load should already have been generated during the translation
	 of the GNAT destination tree; find it out in the GNU tree.  */
      if (TREE_CODE (TREE_OPERAND (ref, 0)) == VIEW_CONVERT_EXPR)
	{
	  tree op = TREE_OPERAND (TREE_OPERAND (ref, 0), 0);
	  if (TREE_CODE (op) == CALL_EXPR && call_is_atomic_load (op))
	    {
	      tree type = TREE_TYPE (TREE_OPERAND (ref, 0));
	      tree t = CALL_EXPR_ARG (op, 0);
	      tree obj, temp, stmt;

	      /* Find out the loaded object.  */
	      if (TREE_CODE (t) == NOP_EXPR)
		t = TREE_OPERAND (t, 0);
	      if (TREE_CODE (t) == ADDR_EXPR)
		obj = TREE_OPERAND (t, 0);
	      else
		obj = build1 (INDIRECT_REF, type, t);

	      /* Drop atomic and volatile qualifiers for the temporary.  */
	      type = TYPE_MAIN_VARIANT (type);

	      /* And drop BLKmode, if need be, to put it into a register.  */
	      if (TYPE_MODE (type) == BLKmode)
		{
		  unsigned int size = tree_to_uhwi (TYPE_SIZE (type));
		  type = copy_type (type);
		  machine_mode mode = int_mode_for_size (size, 0).else_blk ();
		  SET_TYPE_MODE (type, mode);
		}

	      /* Create the temporary by inserting a SAVE_EXPR.  */
	      temp = build1 (SAVE_EXPR, type,
			     build1 (VIEW_CONVERT_EXPR, type, op));
	      TREE_OPERAND (ref, 0) = temp;

	      start_stmt_group ();

	      /* Build the modify of the temporary.  */
	      stmt = build_binary_op (MODIFY_EXPR, NULL_TREE, dest, src);
	      add_stmt_with_node (stmt, gnat_node);

	      /* Build the store to the object.  */
	      stmt = build_atomic_store (obj, temp, false);
	      add_stmt_with_node (stmt, gnat_node);

	      return end_stmt_group ();
	    }
	}

      TREE_OPERAND (ref, 0) = copy_node (TREE_OPERAND (ref, 0));
      ref = TREE_OPERAND (ref, 0);
    }

  /* Something went wrong earlier if we have not found the atomic load.  */
  gcc_unreachable ();
}

/* Make a binary operation of kind OP_CODE.  RESULT_TYPE is the type
   desired for the result.  Usually the operation is to be performed
   in that type.  For INIT_EXPR and MODIFY_EXPR, RESULT_TYPE must be
   NULL_TREE.  For ARRAY_REF, RESULT_TYPE may be NULL_TREE, in which
   case the type to be used will be derived from the operands.
   Don't fold the result if NO_FOLD is true.

   This function is very much unlike the ones for C and C++ since we
   have already done any type conversion and matching required.  All we
   have to do here is validate the work done by SEM and handle subtypes.  */

tree
build_binary_op (enum tree_code op_code, tree result_type,
		 tree left_operand, tree right_operand,
		 bool no_fold)
{
  tree left_type = TREE_TYPE (left_operand);
  tree right_type = TREE_TYPE (right_operand);
  tree left_base_type = get_base_type (left_type);
  tree right_base_type = get_base_type (right_type);
  tree operation_type = result_type;
  tree best_type = NULL_TREE;
  tree modulus, result;
  bool has_side_effects = false;

  if (operation_type
      && TREE_CODE (operation_type) == RECORD_TYPE
      && TYPE_JUSTIFIED_MODULAR_P (operation_type))
    operation_type = TREE_TYPE (TYPE_FIELDS (operation_type));

  if (operation_type && TYPE_IS_EXTRA_SUBTYPE_P (operation_type))
    operation_type = get_base_type (operation_type);

  modulus = (operation_type
	     && TREE_CODE (operation_type) == INTEGER_TYPE
	     && TYPE_MODULAR_P (operation_type)
	     ? TYPE_MODULUS (operation_type) : NULL_TREE);

  switch (op_code)
    {
    case INIT_EXPR:
    case MODIFY_EXPR:
      gcc_checking_assert (!result_type);

      /* If there were integral or pointer conversions on the LHS, remove
	 them; we'll be putting them back below if needed.  Likewise for
	 conversions between record types, except for justified modular types.
	 But don't do this if the right operand is not BLKmode (for packed
	 arrays) unless we are not changing the mode, or if both ooperands
	 are view conversions to the same type.  */
      while ((CONVERT_EXPR_P (left_operand)
	      || TREE_CODE (left_operand) == VIEW_CONVERT_EXPR)
	     && (((INTEGRAL_TYPE_P (left_type)
		   || POINTER_TYPE_P (left_type))
		  && (INTEGRAL_TYPE_P (operand_type (left_operand))
		      || POINTER_TYPE_P (operand_type (left_operand))))
		 || (TREE_CODE (left_type) == RECORD_TYPE
		     && !TYPE_JUSTIFIED_MODULAR_P (left_type)
		     && TREE_CODE (operand_type (left_operand)) == RECORD_TYPE
		     && (TYPE_MODE (right_type) == BLKmode
			 || TYPE_MODE (left_type)
			    == TYPE_MODE (operand_type (left_operand)))
		     && !(TREE_CODE (left_operand) == VIEW_CONVERT_EXPR
			  && TREE_CODE (right_operand) == VIEW_CONVERT_EXPR
			  && left_type == right_type))))
	{
	  left_operand = TREE_OPERAND (left_operand, 0);
	  left_type = TREE_TYPE (left_operand);
	}

      /* If a class-wide type may be involved, force use of the RHS type.  */
      if ((TREE_CODE (right_type) == RECORD_TYPE
	   || TREE_CODE (right_type) == UNION_TYPE)
	  && TYPE_ALIGN_OK (right_type))
	operation_type = right_type;

      /* If we are copying between padded objects with compatible types, use
	 the padded view of the objects, this is very likely more efficient.
	 Likewise for a padded object that is assigned a constructor, if we
	 can convert the constructor to the inner type, to avoid putting a
	 VIEW_CONVERT_EXPR on the LHS.  But don't do so if we wouldn't have
	 actually copied anything.  */
      else if (TYPE_IS_PADDING_P (left_type)
	       && TREE_CONSTANT (TYPE_SIZE (left_type))
	       && ((TREE_CODE (right_operand) == COMPONENT_REF
		    && TYPE_MAIN_VARIANT (left_type)
		       == TYPE_MAIN_VARIANT (operand_type (right_operand)))
		   || (TREE_CODE (right_operand) == CONSTRUCTOR
		       && !CONTAINS_PLACEHOLDER_P
			   (DECL_SIZE (TYPE_FIELDS (left_type)))))
	       && !integer_zerop (TYPE_SIZE (right_type)))
	{
	  /* We make an exception for a BLKmode type padding a non-BLKmode
	     inner type and do the conversion of the LHS right away, since
	     unchecked_convert wouldn't do it properly.  */
	  if (TYPE_MODE (left_type) == BLKmode
	      && TYPE_MODE (right_type) != BLKmode
	      && TREE_CODE (right_operand) != CONSTRUCTOR)
	    {
	      operation_type = right_type;
	      left_operand = convert (operation_type, left_operand);
	      left_type = operation_type;
	    }
	  else
	    operation_type = left_type;
	}

      /* If we have a call to a function that returns with variable size, use
	 the RHS type in case we want to use the return slot optimization.  */
      else if (TREE_CODE (right_operand) == CALL_EXPR
	       && return_type_with_variable_size_p (right_type))
	operation_type = right_type;

      /* Find the best type to use for copying between aggregate types.  */
      else if (((TREE_CODE (left_type) == ARRAY_TYPE
		 && TREE_CODE (right_type) == ARRAY_TYPE)
		|| (TREE_CODE (left_type) == RECORD_TYPE
		    && TREE_CODE (right_type) == RECORD_TYPE))
	       && (best_type = find_common_type (left_type, right_type)))
	operation_type = best_type;

      /* Otherwise use the LHS type.  */
      else
	operation_type = left_type;

      /* Ensure everything on the LHS is valid.  If we have a field reference,
	 strip anything that get_inner_reference can handle.  Then remove any
	 conversions between types having the same code and mode.  And mark
	 VIEW_CONVERT_EXPRs with TREE_ADDRESSABLE.  When done, we must have
	 either an INDIRECT_REF, a NULL_EXPR, a SAVE_EXPR or a DECL node.  */
      result = left_operand;
      while (true)
	{
	  tree restype = TREE_TYPE (result);

	  if (TREE_CODE (result) == COMPONENT_REF
	      || TREE_CODE (result) == ARRAY_REF
	      || TREE_CODE (result) == ARRAY_RANGE_REF)
	    while (handled_component_p (result))
	      result = TREE_OPERAND (result, 0);

	  else if (TREE_CODE (result) == REALPART_EXPR
		   || TREE_CODE (result) == IMAGPART_EXPR
		   || (CONVERT_EXPR_P (result)
		       && (((TREE_CODE (restype)
			     == TREE_CODE (operand_type (result))
			     && TYPE_MODE (restype)
				== TYPE_MODE (operand_type (result))))
			   || TYPE_ALIGN_OK (restype))))
	    result = TREE_OPERAND (result, 0);

	  else if (TREE_CODE (result) == VIEW_CONVERT_EXPR)
	    {
	      TREE_ADDRESSABLE (result) = 1;
	      result = TREE_OPERAND (result, 0);
	    }

	  else
	    break;
	}

      gcc_assert (INDIRECT_REF_P (result)
		  || TREE_CODE (result) == NULL_EXPR
		  || TREE_CODE (result) == SAVE_EXPR
		  || DECL_P (result));

      /* Convert the right operand to the operation type unless it is
	 either already of the correct type or if the type involves a
	 placeholder, since the RHS may not have the same record type.  */
      if (operation_type != right_type
	  && !CONTAINS_PLACEHOLDER_P (TYPE_SIZE (operation_type)))
	{
	  right_operand = convert (operation_type, right_operand);
	  right_type = operation_type;
	}

      /* If the left operand is not of the same type as the operation
	 type, wrap it up in a VIEW_CONVERT_EXPR.  */
      if (left_type != operation_type)
	left_operand = unchecked_convert (operation_type, left_operand, false);

      has_side_effects = true;
      modulus = NULL_TREE;
      break;

    case ARRAY_REF:
      if (!operation_type)
	operation_type = TREE_TYPE (left_type);

      /* ... fall through ... */

    case ARRAY_RANGE_REF:
      /* First look through conversion between type variants.  Note that
	 this changes neither the operation type nor the type domain.  */
      if (TREE_CODE (left_operand) == VIEW_CONVERT_EXPR
	  && TYPE_MAIN_VARIANT (TREE_TYPE (TREE_OPERAND (left_operand, 0)))
	     == TYPE_MAIN_VARIANT (left_type))
	{
	  left_operand = TREE_OPERAND (left_operand, 0);
	  left_type = TREE_TYPE (left_operand);
	}

      /* For a range, make sure the element type is consistent.  */
      if (op_code == ARRAY_RANGE_REF
	  && TREE_TYPE (operation_type) != TREE_TYPE (left_type))
	{
          operation_type = copy_type (operation_type);
          TREE_TYPE (operation_type) = TREE_TYPE (left_type);

	  /* Declare it now since it will never be declared otherwise.  This
	     is necessary to ensure that its subtrees are properly marked.  */
	  create_type_decl (TYPE_NAME (operation_type), operation_type, true,
			    false, Empty);
	}

      /* Then convert the right operand to its base type.  This will prevent
	 unneeded sign conversions when sizetype is wider than integer.  */
      right_operand = convert (right_base_type, right_operand);
      right_operand = convert_to_index_type (right_operand);
      modulus = NULL_TREE;
      break;

    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
      gcc_checking_assert
	(TREE_CODE (get_base_type (result_type)) == BOOLEAN_TYPE);
      operation_type = left_base_type;
      left_operand = convert (operation_type, left_operand);
      right_operand = convert (operation_type, right_operand);
      break;

    case GE_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case LT_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
      gcc_checking_assert
	(TREE_CODE (get_base_type (result_type)) == BOOLEAN_TYPE);
      /* If either operand is a NULL_EXPR, just return a new one.  */
      if (TREE_CODE (left_operand) == NULL_EXPR)
	return build2 (op_code, result_type,
		       build1 (NULL_EXPR, integer_type_node,
			       TREE_OPERAND (left_operand, 0)),
		       integer_zero_node);

      else if (TREE_CODE (right_operand) == NULL_EXPR)
	return build2 (op_code, result_type,
		       build1 (NULL_EXPR, integer_type_node,
			       TREE_OPERAND (right_operand, 0)),
		       integer_zero_node);

      /* If either object is a justified modular types, get the
	 fields from within.  */
      if (TREE_CODE (left_type) == RECORD_TYPE
	  && TYPE_JUSTIFIED_MODULAR_P (left_type))
	{
	  left_operand = convert (TREE_TYPE (TYPE_FIELDS (left_type)),
				  left_operand);
	  left_type = TREE_TYPE (left_operand);
	  left_base_type = get_base_type (left_type);
	}

      if (TREE_CODE (right_type) == RECORD_TYPE
	  && TYPE_JUSTIFIED_MODULAR_P (right_type))
	{
	  right_operand = convert (TREE_TYPE (TYPE_FIELDS (right_type)),
				  right_operand);
	  right_type = TREE_TYPE (right_operand);
	  right_base_type = get_base_type (right_type);
	}

      /* If both objects are arrays, compare them specially.  */
      if ((TREE_CODE (left_type) == ARRAY_TYPE
	   || (TREE_CODE (left_type) == INTEGER_TYPE
	       && TYPE_HAS_ACTUAL_BOUNDS_P (left_type)))
	  && (TREE_CODE (right_type) == ARRAY_TYPE
	      || (TREE_CODE (right_type) == INTEGER_TYPE
		  && TYPE_HAS_ACTUAL_BOUNDS_P (right_type))))
	{
          if (op_code == EQ_EXPR || op_code == NE_EXPR)
	    {
	      result
		= compare_arrays_for_equality (input_location, result_type,
					       left_operand, right_operand);
	      if (op_code == NE_EXPR)
		result = invert_truthvalue_loc (input_location, result);
	    }

	  else
	    {
	      /* Swap the operands to canonicalize to LT_EXPR or GE_EXPR.  */
	      if (op_code == GT_EXPR || op_code == LE_EXPR)
		result
		  = compare_arrays_for_ordering (input_location, result_type,
						 right_operand, left_operand);

	      else
		result
		  = compare_arrays_for_ordering (input_location, result_type,
						 left_operand, right_operand);

	      /* GE_EXPR is (not LT_EXPR) for discrete array types.  */
	      if (op_code == GE_EXPR || op_code == LE_EXPR)
		result = invert_truthvalue_loc (input_location, result);
	    }

	  return result;
	}

      /* Otherwise, the base types must be the same, unless they are both (fat)
	 pointer types or record types.  In the latter case, use the best type
	 and convert both operands to that type.  */
      if (left_base_type != right_base_type)
	{
	  if (TYPE_IS_FAT_POINTER_P (left_base_type)
	      && TYPE_IS_FAT_POINTER_P (right_base_type))
	    {
	      gcc_assert (TYPE_MAIN_VARIANT (left_base_type)
			  == TYPE_MAIN_VARIANT (right_base_type));
	      best_type = left_base_type;
	    }

	  else if (POINTER_TYPE_P (left_base_type)
		   && POINTER_TYPE_P (right_base_type))
	    {
	      tree left_ref_type = TREE_TYPE (left_base_type);
	      tree right_ref_type = TREE_TYPE (right_base_type);

	      /* Anonymous access types in Ada 2005 may point to compatible
		 object subtypes or function types in the language sense.  */
	      gcc_assert (FUNCTION_POINTER_TYPE_P (left_ref_type)
			  == FUNCTION_POINTER_TYPE_P (right_ref_type));
	      best_type = left_base_type;
	    }

	  else if (TREE_CODE (left_base_type) == RECORD_TYPE
		   && TREE_CODE (right_base_type) == RECORD_TYPE)
	    {
	      /* The only way this is permitted is if both types have the same
		 name.  In that case, one of them must not be self-referential.
		 Use it as the best type.  Even better with a fixed size.  */
	      gcc_assert (TYPE_NAME (left_base_type)
			  && TYPE_NAME (left_base_type)
			     == TYPE_NAME (right_base_type));

	      if (TREE_CONSTANT (TYPE_SIZE (left_base_type)))
		best_type = left_base_type;
	      else if (TREE_CONSTANT (TYPE_SIZE (right_base_type)))
		best_type = right_base_type;
	      else if (!CONTAINS_PLACEHOLDER_P (TYPE_SIZE (left_base_type)))
		best_type = left_base_type;
	      else if (!CONTAINS_PLACEHOLDER_P (TYPE_SIZE (right_base_type)))
		best_type = right_base_type;
	      else
		gcc_unreachable ();
	    }

	  else
	    gcc_unreachable ();

	  left_operand = convert (best_type, left_operand);
	  right_operand = convert (best_type, right_operand);
	}
      else
	{
	  left_operand = convert (left_base_type, left_operand);
	  right_operand = convert (right_base_type, right_operand);
	}

      /* If both objects are fat pointers, compare them specially.  */
      if (TYPE_IS_FAT_POINTER_P (left_base_type))
	{
	  result
	    = compare_fat_pointers (input_location,
				    result_type, left_operand, right_operand);
	  if (op_code == NE_EXPR)
	    result = invert_truthvalue_loc (EXPR_LOCATION (result), result);
	  else
	    gcc_assert (op_code == EQ_EXPR);

	  return result;
	}

      modulus = NULL_TREE;
      break;

    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
       /* The RHS of a shift can be any type.  Also, ignore any modulus
	 (we used to abort, but this is needed for unchecked conversion
	 to modular types).  Otherwise, processing is the same as normal.  */
      gcc_assert (operation_type == left_base_type);
      modulus = NULL_TREE;
      left_operand = convert (operation_type, left_operand);
      break;

    case BIT_AND_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
      /* For binary modulus, if the inputs are in range, so are the
	 outputs.  */
      if (modulus && integer_pow2p (modulus))
	modulus = NULL_TREE;
      goto common;

    case COMPLEX_EXPR:
      gcc_assert (TREE_TYPE (result_type) == left_base_type
		  && TREE_TYPE (result_type) == right_base_type);
      left_operand = convert (left_base_type, left_operand);
      right_operand = convert (right_base_type, right_operand);
      break;

    case TRUNC_DIV_EXPR:   case TRUNC_MOD_EXPR:
    case CEIL_DIV_EXPR:    case CEIL_MOD_EXPR:
    case FLOOR_DIV_EXPR:   case FLOOR_MOD_EXPR:
    case ROUND_DIV_EXPR:   case ROUND_MOD_EXPR:
      /* These always produce results lower than either operand.  */
      modulus = NULL_TREE;
      goto common;

    case POINTER_PLUS_EXPR:
      gcc_assert (operation_type == left_base_type
		  && sizetype == right_base_type);
      left_operand = convert (operation_type, left_operand);
      right_operand = convert (sizetype, right_operand);
      break;

    case PLUS_NOMOD_EXPR:
    case MINUS_NOMOD_EXPR:
      if (op_code == PLUS_NOMOD_EXPR)
	op_code = PLUS_EXPR;
      else
	op_code = MINUS_EXPR;
      modulus = NULL_TREE;

      /* ... fall through ... */

    case PLUS_EXPR:
    case MINUS_EXPR:
      /* Avoid doing arithmetics in ENUMERAL_TYPE or BOOLEAN_TYPE like the
	 other compilers.  Contrary to C, Ada doesn't allow arithmetics in
	 these types but can generate addition/subtraction for Succ/Pred.  */
      if (operation_type
	  && (TREE_CODE (operation_type) == ENUMERAL_TYPE
	      || TREE_CODE (operation_type) == BOOLEAN_TYPE))
	operation_type = left_base_type = right_base_type
	  = gnat_type_for_mode (TYPE_MODE (operation_type),
				TYPE_UNSIGNED (operation_type));

      /* ... fall through ... */

    default:
    common:
      /* The result type should be the same as the base types of the
	 both operands (and they should be the same).  Convert
	 everything to the result type.  */

      gcc_assert (operation_type == left_base_type
		  && left_base_type == right_base_type);
      left_operand = convert (operation_type, left_operand);
      right_operand = convert (operation_type, right_operand);
    }

  if (modulus && !integer_pow2p (modulus))
    {
      result = nonbinary_modular_operation (op_code, operation_type,
					    left_operand, right_operand);
      modulus = NULL_TREE;
    }
  /* If either operand is a NULL_EXPR, just return a new one.  */
  else if (TREE_CODE (left_operand) == NULL_EXPR)
    return build1 (NULL_EXPR, operation_type, TREE_OPERAND (left_operand, 0));
  else if (TREE_CODE (right_operand) == NULL_EXPR)
    return build1 (NULL_EXPR, operation_type, TREE_OPERAND (right_operand, 0));
  else if (op_code == ARRAY_REF || op_code == ARRAY_RANGE_REF)
    {
      result = build4 (op_code, operation_type, left_operand, right_operand,
		       NULL_TREE, NULL_TREE);
      if (!no_fold)
	result = fold (result);
    }
  else if (op_code == INIT_EXPR || op_code == MODIFY_EXPR)
    result = build2 (op_code, void_type_node, left_operand, right_operand);
  else if (no_fold)
    result = build2 (op_code, operation_type, left_operand, right_operand);
  else
    result
      = fold_build2 (op_code, operation_type, left_operand, right_operand);

  if (TREE_CONSTANT (result))
    ;
  else if (op_code == ARRAY_REF || op_code == ARRAY_RANGE_REF)
    {
      if (TYPE_VOLATILE (operation_type))
	TREE_THIS_VOLATILE (result) = 1;
    }
  else if (TREE_CONSTANT (left_operand) && TREE_CONSTANT (right_operand))
    TREE_CONSTANT (result) = 1;

  if (has_side_effects)
    TREE_SIDE_EFFECTS (result) = 1;

  /* If we are working with modular types, perform the MOD operation
     if something above hasn't eliminated the need for it.  */
  if (modulus)
    {
      modulus = convert (operation_type, modulus);
      if (no_fold)
	result = build2 (FLOOR_MOD_EXPR, operation_type, result, modulus);
      else
	result = fold_build2 (FLOOR_MOD_EXPR, operation_type, result, modulus);
    }

  if (result_type && result_type != operation_type)
    result = convert (result_type, result);

  return result;
}

/* Similar, but for unary operations.  */

tree
build_unary_op (enum tree_code op_code, tree result_type, tree operand)
{
  tree type = TREE_TYPE (operand);
  tree base_type = get_base_type (type);
  tree operation_type = result_type;
  tree result;

  if (operation_type
      && TREE_CODE (operation_type) == RECORD_TYPE
      && TYPE_JUSTIFIED_MODULAR_P (operation_type))
    operation_type = TREE_TYPE (TYPE_FIELDS (operation_type));

  if (operation_type
      && TREE_CODE (operation_type) == INTEGER_TYPE
      && TYPE_EXTRA_SUBTYPE_P (operation_type))
    operation_type = get_base_type (operation_type);

  switch (op_code)
    {
    case REALPART_EXPR:
    case IMAGPART_EXPR:
      if (!operation_type)
	result_type = operation_type = TREE_TYPE (type);
      else
	gcc_assert (result_type == TREE_TYPE (type));

      result = fold_build1 (op_code, operation_type, operand);
      break;

    case TRUTH_NOT_EXPR:
      gcc_checking_assert
	(TREE_CODE (get_base_type (result_type)) == BOOLEAN_TYPE);
      result = invert_truthvalue_loc (EXPR_LOCATION (operand), operand);
      /* When not optimizing, fold the result as invert_truthvalue_loc
	 doesn't fold the result of comparisons.  This is intended to undo
	 the trick used for boolean rvalues in gnat_to_gnu.  */
      if (!optimize)
	result = fold (result);
      break;

    case ATTR_ADDR_EXPR:
    case ADDR_EXPR:
      switch (TREE_CODE (operand))
	{
	case INDIRECT_REF:
	case UNCONSTRAINED_ARRAY_REF:
	  result = TREE_OPERAND (operand, 0);

	  /* Make sure the type here is a pointer, not a reference.
	     GCC wants pointer types for function addresses.  */
	  if (!result_type)
	    result_type = build_pointer_type (type);

	  /* If the underlying object can alias everything, propagate the
	     property since we are effectively retrieving the object.  */
	  if (POINTER_TYPE_P (TREE_TYPE (result))
	      && TYPE_REF_CAN_ALIAS_ALL (TREE_TYPE (result)))
	    {
	      if (TREE_CODE (result_type) == POINTER_TYPE
		  && !TYPE_REF_CAN_ALIAS_ALL (result_type))
		result_type
		  = build_pointer_type_for_mode (TREE_TYPE (result_type),
						 TYPE_MODE (result_type),
						 true);
	      else if (TREE_CODE (result_type) == REFERENCE_TYPE
		       && !TYPE_REF_CAN_ALIAS_ALL (result_type))
	        result_type
		  = build_reference_type_for_mode (TREE_TYPE (result_type),
						   TYPE_MODE (result_type),
						   true);
	    }
	  break;

	case NULL_EXPR:
	  result = operand;
	  TREE_TYPE (result) = type = build_pointer_type (type);
	  break;

	case COMPOUND_EXPR:
	  /* Fold a compound expression if it has unconstrained array type
	     since the middle-end cannot handle it.  But we don't it in the
	     general case because it may introduce aliasing issues if the
	     first operand is an indirect assignment and the second operand
	     the corresponding address, e.g. for an allocator.  However do
	     it for a return value to expose it for later recognition.  */
	  if (TREE_CODE (type) == UNCONSTRAINED_ARRAY_TYPE
	      || (VAR_P (TREE_OPERAND (operand, 1))
		  && DECL_RETURN_VALUE_P (TREE_OPERAND (operand, 1))))
	    {
	      result = build_unary_op (ADDR_EXPR, result_type,
				       TREE_OPERAND (operand, 1));
	      result = build2 (COMPOUND_EXPR, TREE_TYPE (result),
			       TREE_OPERAND (operand, 0), result);
	      break;
	    }
	  goto common;

	case ARRAY_REF:
	case ARRAY_RANGE_REF:
	case COMPONENT_REF:
	case BIT_FIELD_REF:
	    /* If this is for 'Address, find the address of the prefix and add
	       the offset to the field.  Otherwise, do this the normal way.  */
	  if (op_code == ATTR_ADDR_EXPR)
	    {
	      poly_int64 bitsize;
	      poly_int64 bitpos;
	      tree offset, inner;
	      machine_mode mode;
	      int unsignedp, reversep, volatilep;

	      inner = get_inner_reference (operand, &bitsize, &bitpos, &offset,
					   &mode, &unsignedp, &reversep,
					   &volatilep);

	      /* If INNER is a padding type whose field has a self-referential
		 size, convert to that inner type.  We know the offset is zero
		 and we need to have that type visible.  */
	      if (type_is_padding_self_referential (TREE_TYPE (inner)))
		inner = convert (TREE_TYPE (TYPE_FIELDS (TREE_TYPE (inner))),
				 inner);

	      /* Compute the offset as a byte offset from INNER.  */
	      if (!offset)
		offset = size_zero_node;

	      offset
		= size_binop (PLUS_EXPR, offset,
			      size_int (bits_to_bytes_round_down (bitpos)));

	      /* Take the address of INNER, convert it to a pointer to our type
		 and add the offset.  */
	      inner = build_unary_op (ADDR_EXPR,
				      build_pointer_type (TREE_TYPE (operand)),
				      inner);
	      result = build_binary_op (POINTER_PLUS_EXPR, TREE_TYPE (inner),
					inner, offset);
	      break;
	    }
	  goto common;

	case CONSTRUCTOR:
	  /* If this is just a constructor for a padded record, we can
	     just take the address of the single field and convert it to
	     a pointer to our type.  */
	  if (TYPE_IS_PADDING_P (type))
	    {
	      result
		= build_unary_op (ADDR_EXPR,
				  build_pointer_type (TREE_TYPE (operand)),
				  CONSTRUCTOR_ELT (operand, 0)->value);
	      break;
	    }
	  goto common;

	case NOP_EXPR:
	  if (AGGREGATE_TYPE_P (type)
	      && AGGREGATE_TYPE_P (TREE_TYPE (TREE_OPERAND (operand, 0))))
	    return build_unary_op (ADDR_EXPR, result_type,
				   TREE_OPERAND (operand, 0));

	  /* ... fallthru ... */

	case VIEW_CONVERT_EXPR:
	  /* If this just a variant conversion or if the conversion doesn't
	     change the mode, get the result type from this type and go down.
	     This is needed for conversions of CONST_DECLs, to eventually get
	     to the address of their CORRESPONDING_VARs.  */
	  if ((TYPE_MAIN_VARIANT (type)
	       == TYPE_MAIN_VARIANT (TREE_TYPE (TREE_OPERAND (operand, 0))))
	      || (TYPE_MODE (type) != BLKmode
		  && (TYPE_MODE (type)
		      == TYPE_MODE (TREE_TYPE (TREE_OPERAND (operand, 0))))))
	    return build_unary_op (ADDR_EXPR,
				   (result_type ? result_type
				    : build_pointer_type (type)),
				   TREE_OPERAND (operand, 0));
	  goto common;

	case CONST_DECL:
	  operand = DECL_CONST_CORRESPONDING_VAR (operand);

	  /* ... fall through ... */

	default:
	common:

	  /* If we are taking the address of a padded record whose field
	     contains a template, take the address of the field.  */
	  if (TYPE_IS_PADDING_P (type)
	      && TREE_CODE (TREE_TYPE (TYPE_FIELDS (type))) == RECORD_TYPE
	      && TYPE_CONTAINS_TEMPLATE_P (TREE_TYPE (TYPE_FIELDS (type))))
	    {
	      type = TREE_TYPE (TYPE_FIELDS (type));
	      operand = convert (type, operand);
	    }

	  gnat_mark_addressable (operand);
	  result = build_fold_addr_expr (operand);
	}

      if (TREE_CONSTANT (operand) || staticp (operand))
	TREE_CONSTANT (result) = 1;

      break;

    case INDIRECT_REF:
      {
	tree t = remove_conversions (operand, false);
	bool can_never_be_null = DECL_P (t) && DECL_CAN_NEVER_BE_NULL_P (t);

	/* If TYPE is a thin pointer, either first retrieve the base if this
	   is an expression with an offset built for the initialization of an
	   object with an unconstrained nominal subtype, or else convert to
	   the fat pointer.  */
	if (TYPE_IS_THIN_POINTER_P (type))
	  {
	    tree rec_type = TREE_TYPE (type);

	    if (TREE_CODE (operand) == POINTER_PLUS_EXPR
		&& TREE_OPERAND (operand, 1)
		   == byte_position (DECL_CHAIN (TYPE_FIELDS (rec_type)))
		&& TREE_CODE (TREE_OPERAND (operand, 0)) == NOP_EXPR)
	      {
		operand = TREE_OPERAND (TREE_OPERAND (operand, 0), 0);
		type = TREE_TYPE (operand);
	      }
	    else if (TYPE_UNCONSTRAINED_ARRAY (rec_type))
	      {
		operand
		  = convert (TREE_TYPE (TYPE_UNCONSTRAINED_ARRAY (rec_type)),
			     operand);
		type = TREE_TYPE (operand);
	      }
	  }

	/* If we want to refer to an unconstrained array, use the appropriate
	   expression.  But this will never survive down to the back-end.  */
	if (TYPE_IS_FAT_POINTER_P (type))
	  {
	    result = build1 (UNCONSTRAINED_ARRAY_REF,
			     TYPE_UNCONSTRAINED_ARRAY (type), operand);
	    TREE_READONLY (result)
	      = TYPE_READONLY (TYPE_UNCONSTRAINED_ARRAY (type));
	  }

	/* If we are dereferencing an ADDR_EXPR, return its operand.  */
	else if (TREE_CODE (operand) == ADDR_EXPR)
	  result = TREE_OPERAND (operand, 0);

	/* Otherwise, build and fold the indirect reference.  */
	else
	  {
	    result = build_fold_indirect_ref (operand);
	    TREE_READONLY (result) = TYPE_READONLY (TREE_TYPE (type));
	  }

	if (!TYPE_IS_FAT_POINTER_P (type) && TYPE_VOLATILE (TREE_TYPE (type)))
	  {
	    TREE_SIDE_EFFECTS (result) = 1;
	    if (INDIRECT_REF_P (result))
	      TREE_THIS_VOLATILE (result) = TYPE_VOLATILE (TREE_TYPE (result));
	  }

	if ((INDIRECT_REF_P (result)
	     || TREE_CODE (result) == UNCONSTRAINED_ARRAY_REF)
	    && can_never_be_null)
	  TREE_THIS_NOTRAP (result) = 1;

	break;
      }

    case NEGATE_EXPR:
    case BIT_NOT_EXPR:
      {
	tree modulus = ((operation_type
			 && TREE_CODE (operation_type) == INTEGER_TYPE
			 && TYPE_MODULAR_P (operation_type))
			? TYPE_MODULUS (operation_type) : NULL_TREE);
	int mod_pow2 = modulus && integer_pow2p (modulus);

	/* If this is a modular type, there are various possibilities
	   depending on the operation and whether the modulus is a
	   power of two or not.  */

	if (modulus)
	  {
	    gcc_assert (operation_type == base_type);
	    operand = convert (operation_type, operand);

	    /* The fastest in the negate case for binary modulus is
	       the straightforward code; the TRUNC_MOD_EXPR below
	       is an AND operation.  */
	    if (op_code == NEGATE_EXPR && mod_pow2)
	      result = fold_build2 (TRUNC_MOD_EXPR, operation_type,
				    fold_build1 (NEGATE_EXPR, operation_type,
						 operand),
				    modulus);

	    /* For nonbinary negate case, return zero for zero operand,
	       else return the modulus minus the operand.  If the modulus
	       is a power of two minus one, we can do the subtraction
	       as an XOR since it is equivalent and faster on most machines. */
	    else if (op_code == NEGATE_EXPR && !mod_pow2)
	      {
		if (integer_pow2p (fold_build2 (PLUS_EXPR, operation_type,
						modulus,
						build_int_cst (operation_type,
							       1))))
		  result = fold_build2 (BIT_XOR_EXPR, operation_type,
					operand, modulus);
		else
		  result = fold_build2 (MINUS_EXPR, operation_type,
					modulus, operand);

		result = fold_build3 (COND_EXPR, operation_type,
				      fold_build2 (NE_EXPR,
						   boolean_type_node,
						   operand,
						   build_int_cst
						   (operation_type, 0)),
				      result, operand);
	      }
	    else
	      {
		/* For the NOT cases, we need a constant equal to
		   the modulus minus one.  For a binary modulus, we
		   XOR against the constant and subtract the operand from
		   that constant for nonbinary modulus.  */

		tree cnst = fold_build2 (MINUS_EXPR, operation_type, modulus,
					 build_int_cst (operation_type, 1));

		if (mod_pow2)
		  result = fold_build2 (BIT_XOR_EXPR, operation_type,
					operand, cnst);
		else
		  result = fold_build2 (MINUS_EXPR, operation_type,
					cnst, operand);
	      }

	    break;
	  }
      }

      /* ... fall through ... */

    default:
      gcc_assert (operation_type == base_type);
      result = fold_build1 (op_code, operation_type,
			    convert (operation_type, operand));
    }

  if (result_type && TREE_TYPE (result) != result_type)
    result = convert (result_type, result);

  return result;
}

/* Similar, but for COND_EXPR.  */

tree
build_cond_expr (tree result_type, tree condition_operand,
                 tree true_operand, tree false_operand)
{
  bool addr_p = false;
  tree result;

  /* The front-end verified that result, true and false operands have
     same base type.  Convert everything to the result type.  */
  true_operand = convert (result_type, true_operand);
  false_operand = convert (result_type, false_operand);

  /* If the result type is unconstrained or variable-sized, take the address
     of the operands and then dereference the result.  Likewise if the result
     type is passed by reference, because creating a temporary of this type is
     not allowed.  */
  if (TREE_CODE (result_type) == UNCONSTRAINED_ARRAY_TYPE
      || type_contains_placeholder_p (result_type)
      || !TREE_CONSTANT (TYPE_SIZE (result_type))
      || TYPE_IS_BY_REFERENCE_P (result_type))
    {
      result_type = build_pointer_type (result_type);
      true_operand = build_unary_op (ADDR_EXPR, result_type, true_operand);
      false_operand = build_unary_op (ADDR_EXPR, result_type, false_operand);
      addr_p = true;
    }

  result = fold_build3 (COND_EXPR, result_type, condition_operand,
			true_operand, false_operand);

  /* If we have a common SAVE_EXPR (possibly surrounded by arithmetics)
     in both arms, make sure it gets evaluated by moving it ahead of the
     conditional expression.  This is necessary because it is evaluated
     in only one place at run time and would otherwise be uninitialized
     in one of the arms.  */
  true_operand = skip_simple_arithmetic (true_operand);
  false_operand = skip_simple_arithmetic (false_operand);

  if (true_operand == false_operand && TREE_CODE (true_operand) == SAVE_EXPR)
    result = build2 (COMPOUND_EXPR, result_type, true_operand, result);

  if (addr_p)
    result = build_unary_op (INDIRECT_REF, NULL_TREE, result);

  return result;
}

/* Similar, but for COMPOUND_EXPR.  */

tree
build_compound_expr (tree result_type, tree stmt_operand, tree expr_operand)
{
  bool addr_p = false;
  tree result;

  /* If the result type is unconstrained, take the address of the operand and
     then dereference the result.  Likewise if the result type is passed by
     reference, but this is natively handled in the gimplifier.  */
  if (TREE_CODE (result_type) == UNCONSTRAINED_ARRAY_TYPE
      || CONTAINS_PLACEHOLDER_P (TYPE_SIZE (result_type)))
    {
      result_type = build_pointer_type (result_type);
      expr_operand = build_unary_op (ADDR_EXPR, result_type, expr_operand);
      addr_p = true;
    }

  result = fold_build2 (COMPOUND_EXPR, result_type, stmt_operand,
			expr_operand);

  if (addr_p)
    result = build_unary_op (INDIRECT_REF, NULL_TREE, result);

  return result;
}

/* Conveniently construct a function call expression.  FNDECL names the
   function to be called, N is the number of arguments, and the "..."
   parameters are the argument expressions.  Unlike build_call_expr
   this doesn't fold the call, hence it will always return a CALL_EXPR.  */

tree
build_call_n_expr (tree fndecl, int n, ...)
{
  va_list ap;
  tree fntype = TREE_TYPE (fndecl);
  tree fn = build1 (ADDR_EXPR, build_pointer_type (fntype), fndecl);

  va_start (ap, n);
  fn = build_call_valist (TREE_TYPE (fntype), fn, n, ap);
  va_end (ap);
  return fn;
}

/* Build a goto to LABEL for a raise, with an optional call to Local_Raise.
   MSG gives the exception's identity for the call to Local_Raise, if any.  */

static tree
build_goto_raise (Entity_Id gnat_label, int msg)
{
  tree gnu_label = gnat_to_gnu_entity (gnat_label, NULL_TREE, false);
  tree gnu_result = build1 (GOTO_EXPR, void_type_node, gnu_label);
  Entity_Id local_raise = Get_Local_Raise_Call_Entity ();

  /* If Local_Raise is present, build Local_Raise (Exception'Identity).  */
  if (Present (local_raise))
    {
      tree gnu_local_raise
	= gnat_to_gnu_entity (local_raise, NULL_TREE, false);
      tree gnu_exception_entity
	= gnat_to_gnu_entity (Get_RT_Exception_Entity (msg), NULL_TREE, false);
      tree gnu_call
	= build_call_n_expr (gnu_local_raise, 1,
			     build_unary_op (ADDR_EXPR, NULL_TREE,
					     gnu_exception_entity));
      gnu_result
	= build2 (COMPOUND_EXPR, void_type_node, gnu_call, gnu_result);
    }

  TREE_USED (gnu_label) = 1;
  return gnu_result;
}

/* Expand the SLOC of GNAT_NODE, if present, into tree location information
   pointed to by FILENAME, LINE and COL.  Fall back to the current location
   if GNAT_NODE is absent or has no SLOC.  */

static void
expand_sloc (Node_Id gnat_node, tree *filename, tree *line, tree *col)
{
  const char *str;
  int line_number, column_number;

  if (Debug_Flag_NN || Exception_Locations_Suppressed)
    {
      str = "";
      line_number = 0;
      column_number = 0;
    }
  else if (Present (gnat_node) && Sloc (gnat_node) != No_Location)
    {
      str = Get_Name_String
	    (Debug_Source_Name (Get_Source_File_Index (Sloc (gnat_node))));
      line_number = Get_Logical_Line_Number (Sloc (gnat_node));
      column_number = Get_Column_Number (Sloc (gnat_node));
    }
  else
    {
      str = lbasename (LOCATION_FILE (input_location));
      line_number = LOCATION_LINE (input_location);
      column_number = LOCATION_COLUMN (input_location);
    }

  const int len = strlen (str);
  *filename = build_string (len, str);
  TREE_TYPE (*filename) = build_array_type (char_type_node,
					    build_index_type (size_int (len)));
  *line = build_int_cst (NULL_TREE, line_number);
  if (col)
    *col = build_int_cst (NULL_TREE, column_number);
}

/* Build a call to a function that raises an exception and passes file name
   and line number, if requested.  MSG says which exception function to call.
   GNAT_NODE is the node conveying the source location for which the error
   should be signaled, or Empty in which case the error is signaled for the
   current location.  KIND says which kind of exception node this is for,
   among N_Raise_{Constraint,Storage,Program}_Error.  */

tree
build_call_raise (int msg, Node_Id gnat_node, char kind)
{
  Entity_Id gnat_label = get_exception_label (kind);
  tree fndecl = gnat_raise_decls[msg];
  tree filename, line;

  /* If this is to be done as a goto, handle that case.  */
  if (Present (gnat_label))
    return build_goto_raise (gnat_label, msg);

  expand_sloc (gnat_node, &filename, &line, NULL);

  return
    build_call_n_expr (fndecl, 2,
		       build1 (ADDR_EXPR,
			       build_pointer_type (char_type_node),
			       filename),
		       line);
}

/* Similar to build_call_raise, with extra information about the column
   where the check failed.  */

tree
build_call_raise_column (int msg, Node_Id gnat_node, char kind)
{
  Entity_Id gnat_label = get_exception_label (kind);
  tree fndecl = gnat_raise_decls_ext[msg];
  tree filename, line, col;

  /* If this is to be done as a goto, handle that case.  */
  if (Present (gnat_label))
    return build_goto_raise (gnat_label, msg);

  expand_sloc (gnat_node, &filename, &line, &col);

  return
    build_call_n_expr (fndecl, 3,
		       build1 (ADDR_EXPR,
			       build_pointer_type (char_type_node),
			       filename),
		       line, col);
}

/* Similar to build_call_raise_column, for an index or range check exception ,
   with extra information of the form "INDEX out of range FIRST..LAST".  */

tree
build_call_raise_range (int msg, Node_Id gnat_node, char kind,
			tree index, tree first, tree last)
{
  Entity_Id gnat_label = get_exception_label (kind);
  tree fndecl = gnat_raise_decls_ext[msg];
  tree filename, line, col;

  /* If this is to be done as a goto, handle that case.  */
  if (Present (gnat_label))
    return build_goto_raise (gnat_label, msg);

  expand_sloc (gnat_node, &filename, &line, &col);

  return
    build_call_n_expr (fndecl, 6,
		       build1 (ADDR_EXPR,
			       build_pointer_type (char_type_node),
			       filename),
		       line, col,
		       convert (integer_type_node, index),
		       convert (integer_type_node, first),
		       convert (integer_type_node, last));
}

/* qsort comparer for the bit positions of two constructor elements
   for record components.  */

static int
compare_elmt_bitpos (const void *rt1, const void *rt2)
{
  const constructor_elt * const elmt1 = (const constructor_elt *) rt1;
  const constructor_elt * const elmt2 = (const constructor_elt *) rt2;
  const_tree const field1 = elmt1->index;
  const_tree const field2 = elmt2->index;
  const int ret
    = tree_int_cst_compare (bit_position (field1), bit_position (field2));

  if (ret)
    return ret;

  /* The bit position can be the same if one of the fields has zero size.
     In this case, if the other has nonzero size, put the former first to
     match the layout done by components_to_record.  Otherwise, preserve
     the order of the source code.  */

  const bool field1_zero_size = integer_zerop (DECL_SIZE (field1));
  const bool field2_zero_size = integer_zerop (DECL_SIZE (field2));

  if (field1_zero_size && !field2_zero_size)
    return -1;
  else if (!field1_zero_size && field2_zero_size)
    return 1;
  else
    return (int) (DECL_UID (field1) - DECL_UID (field2));
}

/* Return a CONSTRUCTOR of TYPE whose elements are V.  */

tree
gnat_build_constructor (tree type, vec<constructor_elt, va_gc> *v)
{
  bool allconstant = (TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST);
  bool read_only = true;
  bool side_effects = false;
  tree result, obj, val;
  unsigned int n_elmts;

  /* Scan the elements to see if they are all constant or if any has side
     effects, to let us set global flags on the resulting constructor.  Count
     the elements along the way for possible sorting purposes below.  */
  FOR_EACH_CONSTRUCTOR_ELT (v, n_elmts, obj, val)
    {
      /* The predicate must be in keeping with output_constructor and, unlike
	 initializer_constant_valid_p, we accept "&{...}" because we'll put
	 the CONSTRUCTOR into the constant pool during gimplification.  */
      if ((!TREE_CONSTANT (val) && !TREE_STATIC (val))
	  || (TREE_CODE (type) == RECORD_TYPE
	      && CONSTRUCTOR_BITFIELD_P (obj)
	      && !initializer_constant_valid_for_bitfield_p (val))
	  || (!initializer_constant_valid_p (val,
					     TREE_TYPE (val),
					     TYPE_REVERSE_STORAGE_ORDER (type))
	      && !(TREE_CODE (val) == ADDR_EXPR
		   && TREE_CODE (TREE_OPERAND (val, 0)) == CONSTRUCTOR
		   && TREE_CONSTANT (TREE_OPERAND (val, 0)))))
	allconstant = false;

      if (!TREE_READONLY (val))
	read_only = false;

      if (TREE_SIDE_EFFECTS (val))
	side_effects = true;
    }

  /* For record types with constant components only, sort field list
     by increasing bit position.  This is necessary to ensure the
     constructor can be output as static data.  */
  if (allconstant && TREE_CODE (type) == RECORD_TYPE && n_elmts > 1)
    v->qsort (compare_elmt_bitpos);

  result = build_constructor (type, v);
  CONSTRUCTOR_NO_CLEARING (result) = 1;
  TREE_CONSTANT (result) = TREE_STATIC (result) = allconstant;
  TREE_SIDE_EFFECTS (result) = side_effects;
  TREE_READONLY (result) = TYPE_READONLY (type) || read_only || allconstant;
  return result;
}

/* Return a COMPONENT_REF to access FIELD in RECORD, or NULL_TREE if the field
   is not found in the record.  Don't fold the result if NO_FOLD is true.  */

static tree
build_simple_component_ref (tree record, tree field, bool no_fold)
{
  tree type = TYPE_MAIN_VARIANT (TREE_TYPE (record));
  tree ref;

  /* The failure of this assertion will very likely come from a missing
     insertion of an explicit dereference.  */
  gcc_assert (RECORD_OR_UNION_TYPE_P (type));

  /* The type must be frozen at this point.  */
  gcc_assert (COMPLETE_TYPE_P (type));

  /* Try to fold a conversion from another record or union type unless the type
     contains a placeholder as it might be needed for a later substitution.  */
  if (TREE_CODE (record) == VIEW_CONVERT_EXPR
      && RECORD_OR_UNION_TYPE_P (TREE_TYPE (TREE_OPERAND (record, 0)))
      && !type_contains_placeholder_p (type))
    {
      tree op = TREE_OPERAND (record, 0);

      /* If this is an unpadding operation, convert the underlying object to
	 the unpadded type directly.  */
      if (TYPE_IS_PADDING_P (type) && field == TYPE_FIELDS (type))
	return convert (TREE_TYPE (field), op);

      /* Otherwise try to access FIELD directly in the underlying type, but
	 make sure that the form of the reference doesn't change too much;
	 this can happen for an unconstrained bit-packed array type whose
	 constrained form can be an integer type.  */
      ref = build_simple_component_ref (op, field, no_fold);
      if (ref && TREE_CODE (TREE_TYPE (ref)) == TREE_CODE (TREE_TYPE (field)))
	return ref;
    }

  /* If this field is not in the specified record, see if we can find a field
     in the specified record whose original field is the same as this one.  */
  if (DECL_CONTEXT (field) != type)
    {
      tree new_field;

      /* First loop through normal components.  */
      for (new_field = TYPE_FIELDS (type);
	   new_field;
	   new_field = DECL_CHAIN (new_field))
	if (SAME_FIELD_P (field, new_field))
	  break;

      /* Next, loop through DECL_INTERNAL_P components if we haven't found the
	 component in the first search.  Doing this search in two steps is
	 required to avoid hidden homonymous fields in the _Parent field.  */
      if (!new_field)
	for (new_field = TYPE_FIELDS (type);
	     new_field;
	     new_field = DECL_CHAIN (new_field))
	  if (DECL_INTERNAL_P (new_field)
	      && RECORD_OR_UNION_TYPE_P (TREE_TYPE (new_field)))
	    {
	      tree field_ref
		= build_simple_component_ref (record, new_field, no_fold);
	      ref = build_simple_component_ref (field_ref, field, no_fold);
	      if (ref)
		return ref;
	    }

      field = new_field;
    }

  if (!field)
    return NULL_TREE;

  /* If the field's offset has overflowed, do not try to access it, as doing
     so may trigger sanity checks deeper in the back-end.  Note that we don't
     need to warn since this will be done on trying to declare the object.  */
  if (TREE_CODE (DECL_FIELD_OFFSET (field)) == INTEGER_CST
      && TREE_OVERFLOW (DECL_FIELD_OFFSET (field)))
    return build1 (NULL_EXPR, TREE_TYPE (field),
		   build_call_raise (SE_Object_Too_Large, Empty,
				     N_Raise_Storage_Error));

  ref = build3 (COMPONENT_REF, TREE_TYPE (field), record, field, NULL_TREE);

  if (TREE_READONLY (record)
      || TREE_READONLY (field)
      || TYPE_READONLY (type))
    TREE_READONLY (ref) = 1;

  if (TREE_THIS_VOLATILE (record)
      || TREE_THIS_VOLATILE (field)
      || TYPE_VOLATILE (type))
    TREE_THIS_VOLATILE (ref) = 1;

  if (no_fold)
    return ref;

  /* The generic folder may punt in this case because the inner array type
     can be self-referential, but folding is in fact not problematic.  */
  if (TREE_CODE (record) == CONSTRUCTOR
      && TYPE_CONTAINS_TEMPLATE_P (TREE_TYPE (record)))
    {
      vec<constructor_elt, va_gc> *elts = CONSTRUCTOR_ELTS (record);
      unsigned HOST_WIDE_INT idx;
      tree index, value;
      FOR_EACH_CONSTRUCTOR_ELT (elts, idx, index, value)
       if (index == field)
	return value;
      return ref;
    }

  return fold (ref);
}

/* Likewise, but return NULL_EXPR and generate a Program_Error if the
   field is not found in the record.  */

tree
build_component_ref (tree record, tree field, bool no_fold)
{
  tree ref = build_simple_component_ref (record, field, no_fold);
  if (ref)
    return ref;

  /* The missing field should have been detected in the front-end.  */
  gigi_checking_assert (false);

  /* Assume this is an invalid user field so raise Program_Error.  */
  return build1 (NULL_EXPR, TREE_TYPE (field),
		 build_call_raise (PE_Explicit_Raise, Empty,
				   N_Raise_Program_Error));
}

/* Helper for build_call_alloc_dealloc, with arguments to be interpreted
   identically.  Process the case where a GNAT_PROC to call is provided.  */

static inline tree
build_call_alloc_dealloc_proc (tree gnu_obj, tree gnu_size, tree gnu_type,
			       Entity_Id gnat_proc, Entity_Id gnat_pool)
{
  tree gnu_proc = gnat_to_gnu (gnat_proc);
  tree gnu_align = size_int (TYPE_ALIGN (gnu_type) / BITS_PER_UNIT);

  tree gnu_call;

  /* A storage pool's underlying type is a record type for both predefined
     storage pools and GNAT simple storage pools.  The return and secondary
     stacks use the same mechanism, but their pool object is an integer.  */
  if (Is_Record_Type (Underlying_Type (Etype (gnat_pool))))
    {
      /* The size is the third parameter; the alignment is the
	 same type.  */
      Entity_Id gnat_size_type
	= Etype (Next_Formal (Next_Formal (First_Formal (gnat_proc))));
      tree gnu_size_type = gnat_to_gnu_type (gnat_size_type);

      tree gnu_pool = gnat_to_gnu (gnat_pool);
      tree gnu_pool_addr = build_unary_op (ADDR_EXPR, NULL_TREE, gnu_pool);

      gnu_size = convert (gnu_size_type, gnu_size);
      gnu_align = convert (gnu_size_type, gnu_align);

      /* The first arg is always the address of the storage pool; next
	 comes the address of the object, for a deallocator, then the
	 size and alignment.  */
      if (gnu_obj)
	gnu_call = build_call_n_expr (gnu_proc, 4, gnu_pool_addr, gnu_obj,
				      gnu_size, gnu_align);
      else
	gnu_call = build_call_n_expr (gnu_proc, 3, gnu_pool_addr,
				      gnu_size, gnu_align);
    }

  else
    {
      /* The size is the second parameter.  */
      Entity_Id gnat_size_type
	= Etype (Next_Formal (First_Formal (gnat_proc)));
      tree gnu_size_type = gnat_to_gnu_type (gnat_size_type);

      /* Deallocation is not supported for return and secondary stacks.  */
      gcc_assert (!gnu_obj);

      gnu_size = convert (gnu_size_type, gnu_size);
      gnu_align = convert (gnu_size_type, gnu_align);

      if (DECL_BUILT_IN_CLASS (gnu_proc) == BUILT_IN_FRONTEND
	  && DECL_FE_FUNCTION_CODE (gnu_proc) == BUILT_IN_RETURN_SLOT)
	{
	  /* This must be a function that returns by invisible reference.  */
	  gcc_assert (current_function_decl
		      && TREE_ADDRESSABLE (TREE_TYPE (current_function_decl)));
	  tree gnu_ret_size;

	  gnu_call = DECL_RESULT (current_function_decl);

	  /* The allocation has already been done by the caller so we check that
	     we are not going to overflow the return slot.  */
	  if (TYPE_CI_CO_LIST (TREE_TYPE (current_function_decl)))
	    gnu_ret_size
	      = TYPE_SIZE_UNIT
                (TREE_TYPE (TYPE_FIELDS (TREE_TYPE (TREE_TYPE (gnu_call)))));
	  else
	    gnu_ret_size = TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (gnu_call)));

	  gnu_call
	    = fold_build3 (COND_EXPR, TREE_TYPE (gnu_call),
			   fold_build2 (LE_EXPR, boolean_type_node,
				        fold_convert (sizetype, gnu_size),
					gnu_ret_size),
			   gnu_call,
			   build_call_raise (PE_Explicit_Raise, Empty,
					     N_Raise_Program_Error));
	}

      else
	gnu_call = build_call_n_expr (gnu_proc, 2, gnu_size, gnu_align);
    }

  return gnu_call;
}

/* Helper for build_call_alloc_dealloc, to build and return an allocator for
   DATA_SIZE bytes aimed at containing a DATA_TYPE object, using the default
   __gnat_malloc allocator.  Honor DATA_TYPE alignments greater than what the
   latter offers.  */

static inline tree
maybe_wrap_malloc (tree data_size, tree data_type, Node_Id gnat_node)
{
  /* When the DATA_TYPE alignment is stricter than what malloc offers
     (super-aligned case), we allocate an "aligning" wrapper type and return
     the address of its single data field with the malloc's return value
     stored just in front.  */

  unsigned int data_align = TYPE_ALIGN (data_type);
  unsigned int system_allocator_alignment
      = get_target_system_allocator_alignment () * BITS_PER_UNIT;

  tree aligning_type
    = ((data_align > system_allocator_alignment)
       ? make_aligning_type (data_type, data_align, data_size,
			     system_allocator_alignment,
			     POINTER_SIZE / BITS_PER_UNIT,
			     gnat_node)
       : NULL_TREE);

  tree size_to_malloc
    = aligning_type ? TYPE_SIZE_UNIT (aligning_type) : data_size;

  tree malloc_ptr = build_call_n_expr (malloc_decl, 1, size_to_malloc);

  Check_Restriction_No_Dependence_On_System (Name_Memory, gnat_node);

  if (aligning_type)
    {
      /* Latch malloc's return value and get a pointer to the aligning field
	 first.  */
      tree storage_ptr = gnat_protect_expr (malloc_ptr);

      tree aligning_record_addr
	= convert (build_pointer_type (aligning_type), storage_ptr);

      tree aligning_record
	= build_unary_op (INDIRECT_REF, NULL_TREE, aligning_record_addr);

      tree aligning_field
	= build_component_ref (aligning_record, TYPE_FIELDS (aligning_type),
			       false);

      tree aligning_field_addr
        = build_unary_op (ADDR_EXPR, NULL_TREE, aligning_field);

      /* Then arrange to store the allocator's return value ahead
	 and return.  */
      tree storage_ptr_slot_addr
	= build_binary_op (POINTER_PLUS_EXPR, ptr_type_node,
			   convert (ptr_type_node, aligning_field_addr),
			   size_int (-(HOST_WIDE_INT) POINTER_SIZE
				     / BITS_PER_UNIT));

      tree storage_ptr_slot
	= build_unary_op (INDIRECT_REF, NULL_TREE,
			  convert (build_pointer_type (ptr_type_node),
				   storage_ptr_slot_addr));

      return
	build2 (COMPOUND_EXPR, TREE_TYPE (aligning_field_addr),
		build_binary_op (INIT_EXPR, NULL_TREE,
				 storage_ptr_slot, storage_ptr),
		aligning_field_addr);
    }
  else
    return malloc_ptr;
}

/* Helper for build_call_alloc_dealloc, to release a DATA_TYPE object
   designated by DATA_PTR using the __gnat_free entry point.  */

static inline tree
maybe_wrap_free (tree data_ptr, tree data_type, Node_Id gnat_node)
{
  /* In the regular alignment case, we pass the data pointer straight to free.
     In the superaligned case, we need to retrieve the initial allocator
     return value, stored in front of the data block at allocation time.  */

  unsigned int data_align = TYPE_ALIGN (data_type);
  unsigned int system_allocator_alignment
      = get_target_system_allocator_alignment () * BITS_PER_UNIT;

  tree free_ptr;

  Check_Restriction_No_Dependence_On_System (Name_Memory, gnat_node);

  if (data_align > system_allocator_alignment)
    {
      /* DATA_FRONT_PTR (void *)
	 = (void *)DATA_PTR - (void *)sizeof (void *))  */
      tree data_front_ptr
	= build_binary_op
	  (POINTER_PLUS_EXPR, ptr_type_node,
	   convert (ptr_type_node, data_ptr),
	   size_int (-(HOST_WIDE_INT) POINTER_SIZE / BITS_PER_UNIT));

      /* FREE_PTR (void *) = *(void **)DATA_FRONT_PTR  */
      free_ptr
	= build_unary_op
	  (INDIRECT_REF, NULL_TREE,
	   convert (build_pointer_type (ptr_type_node), data_front_ptr));
    }
  else
    free_ptr = data_ptr;

  return build_call_n_expr (free_decl, 1, free_ptr);
}

/* Build a GCC tree to call an allocation or deallocation function.
   If GNU_OBJ is nonzero, it is an object to deallocate.  Otherwise,
   generate an allocation.

   GNU_SIZE is the number of bytes to allocate and GNU_TYPE is the contained
   object type, used to determine the to-be-honored address alignment.
   GNAT_PROC, if present, is a procedure to call and GNAT_POOL is the storage
   pool to use.  If not present, malloc and free are used.  GNAT_NODE is used
   to provide an error location for restriction violation messages.  */

tree
build_call_alloc_dealloc (tree gnu_obj, tree gnu_size, tree gnu_type,
                          Entity_Id gnat_proc, Entity_Id gnat_pool,
                          Node_Id gnat_node)
{
  /* Explicit proc to call ?  This one is assumed to deal with the type
     alignment constraints.  */
  if (Present (gnat_proc))
    return build_call_alloc_dealloc_proc (gnu_obj, gnu_size, gnu_type,
					  gnat_proc, gnat_pool);

  /* Otherwise, object to "free" or "malloc" with possible special processing
     for alignments stricter than what the default allocator honors.  */
  else if (gnu_obj)
    return maybe_wrap_free (gnu_obj, gnu_type, gnat_node);
  else
    {
      /* Assert that we no longer can be called with this special pool.  */
      gcc_assert (gnat_pool != -1);

      /* Check that we aren't violating the associated restriction.  */
      if (!(Nkind (gnat_node) == N_Allocator && Comes_From_Source (gnat_node)))
	{
	  Check_No_Implicit_Heap_Alloc (gnat_node);
	  if (Has_Task (Etype (gnat_node)))
	    Check_No_Implicit_Task_Alloc (gnat_node);
	  if (Has_Protected (Etype (gnat_node)))
	    Check_No_Implicit_Protected_Alloc (gnat_node);
	}
      return maybe_wrap_malloc (gnu_size, gnu_type, gnat_node);
    }
}

/* Build a GCC tree that corresponds to allocating an object of TYPE whose
   initial value is INIT, if INIT is nonzero.  Convert the expression to
   RESULT_TYPE, which must be some pointer type, and return the result.

   GNAT_PROC and GNAT_POOL optionally give the procedure to call and
   the storage pool to use.  GNAT_NODE is used to provide an error
   location for restriction violation messages.  If IGNORE_INIT_TYPE is
   true, ignore the type of INIT for the purpose of determining the size;
   this will cause the maximum size to be allocated if TYPE is of
   self-referential size.  */

tree
build_allocator (tree type, tree init, tree result_type, Entity_Id gnat_proc,
                 Entity_Id gnat_pool, Node_Id gnat_node, bool ignore_init_type)
{
  const bool pool_is_storage_model
    = Present (gnat_pool)
      && Has_Storage_Model_Type_Aspect (Etype (gnat_pool))
      && Present (Storage_Model_Copy_To (gnat_pool));
  tree size, storage, storage_deref, storage_init;

  /* If the initializer, if present, is a NULL_EXPR, just return a new one.  */
  if (init && TREE_CODE (init) == NULL_EXPR)
    return build1 (NULL_EXPR, result_type, TREE_OPERAND (init, 0));

  /* If we are just annotating types, also return a NULL_EXPR.  */
  else if (type_annotate_only)
    return build1 (NULL_EXPR, result_type,
		   build_call_raise (CE_Range_Check_Failed, gnat_node,
				     N_Raise_Constraint_Error));

  /* If the initializer, if present, is a COND_EXPR, deal with each branch.  */
  else if (init && TREE_CODE (init) == COND_EXPR)
    return build3 (COND_EXPR, result_type, TREE_OPERAND (init, 0),
		   build_allocator (type, TREE_OPERAND (init, 1), result_type,
				    gnat_proc, gnat_pool, gnat_node,
				    ignore_init_type),
		   build_allocator (type, TREE_OPERAND (init, 2), result_type,
				    gnat_proc, gnat_pool, gnat_node,
				    ignore_init_type));

  /* If RESULT_TYPE is a fat or thin pointer, set SIZE to be the sum of the
     sizes of the object and its template.  Allocate the whole thing and
     fill in the parts that are known.  */
  else if (TYPE_IS_FAT_OR_THIN_POINTER_P (result_type))
    {
      tree storage_type
	= build_unc_object_type_from_ptr (result_type, type,
					  get_identifier ("ALLOC"), false);
      tree template_type = TREE_TYPE (TYPE_FIELDS (storage_type));
      tree storage_ptr_type = build_pointer_type (storage_type);
      tree lhs, rhs;

      size = TYPE_SIZE_UNIT (storage_type);
      size = SUBSTITUTE_PLACEHOLDER_IN_EXPR (size, init);

      /* If the size overflows, pass -1 so Storage_Error will be raised.  */
      if (TREE_CODE (size) == INTEGER_CST && !valid_constant_size_p (size))
	size = size_int (-1);

      storage = build_call_alloc_dealloc (NULL_TREE, size, storage_type,
					  gnat_proc, gnat_pool, gnat_node);
      storage = convert (storage_ptr_type, gnat_protect_expr (storage));
      storage_deref = build_unary_op (INDIRECT_REF, NULL_TREE, storage);
      TREE_THIS_NOTRAP (storage_deref) = 1;

      /* If there is an initializing expression, then make a constructor for
	 the entire object including the bounds and copy it into the object.
	 If there is no initializing expression, just set the bounds.  Note
	 that, if we have a storage model, we need to copy the initializing
	 expression separately from the bounds.  */
      if (init && !pool_is_storage_model)
	{
	  vec<constructor_elt, va_gc> *v;
	  vec_alloc (v, 2);

	  CONSTRUCTOR_APPEND_ELT (v, TYPE_FIELDS (storage_type),
				  build_template (template_type, type, init));
	  CONSTRUCTOR_APPEND_ELT (v, DECL_CHAIN (TYPE_FIELDS (storage_type)),
				  init);

	  lhs = storage_deref;
	  rhs = gnat_build_constructor (storage_type, v);
	}
      else
	{
	  lhs = build_component_ref (storage_deref, TYPE_FIELDS (storage_type),
				     false);
	  rhs = build_template (template_type, type, init);
	}

      if (pool_is_storage_model)
	{
	  storage_init = build_storage_model_store (gnat_pool, lhs, rhs);
	  if (init)
	    {
	      start_stmt_group ();
	      add_stmt (storage_init);
	      lhs
		= build_component_ref (storage_deref,
				       DECL_CHAIN (TYPE_FIELDS (storage_type)),
				       false);
	      rhs = init;
	      size = TYPE_SIZE_UNIT (TREE_TYPE (lhs));
	      size = SUBSTITUTE_PLACEHOLDER_IN_EXPR (size, init);
	      tree t = build_storage_model_store (gnat_pool, lhs, rhs, size);
	      add_stmt (t);
	      storage_init = end_stmt_group ();
	    }
	}
      else
	storage_init = build_binary_op (INIT_EXPR, NULL_TREE, lhs, rhs);

      return build2 (COMPOUND_EXPR, result_type,
		     storage_init, convert (result_type, storage));
    }

  size = TYPE_SIZE_UNIT (type);

  /* If we have an initializing expression, see if its size is simpler
     than the size from the type.  */
  if (!ignore_init_type && init && TYPE_SIZE_UNIT (TREE_TYPE (init))
      && (TREE_CODE (TYPE_SIZE_UNIT (TREE_TYPE (init))) == INTEGER_CST
	  || CONTAINS_PLACEHOLDER_P (size)))
    size = TYPE_SIZE_UNIT (TREE_TYPE (init));

  /* If the size is still self-referential, reference the initializing
     expression, if it is present.  If not, this must have been a call
     to allocate a library-level object, in which case we just use the
     maximum size.  */
  if (!ignore_init_type && init)
    size = SUBSTITUTE_PLACEHOLDER_IN_EXPR (size, init);
  else if (CONTAINS_PLACEHOLDER_P (size))
    size = max_size (size, true);

  /* If the size overflows, pass -1 so Storage_Error will be raised.  */
  if (TREE_CODE (size) == INTEGER_CST && !valid_constant_size_p (size))
    size = size_int (-1);

  storage = convert (result_type,
		     build_call_alloc_dealloc (NULL_TREE, size, type,
					       gnat_proc, gnat_pool,
					       gnat_node));

  /* If we have an initial value, protect the new address, assign the value
     and return the address with a COMPOUND_EXPR.  */
  if (init)
    {
      storage = gnat_protect_expr (storage);
      storage_deref = build_unary_op (INDIRECT_REF, NULL_TREE, storage);
      TREE_THIS_NOTRAP (storage_deref) = 1;
      if (pool_is_storage_model)
	storage_init
	  = build_storage_model_store (gnat_pool, storage_deref, init, size);
      else
	storage_init
	  = build_binary_op (INIT_EXPR, NULL_TREE, storage_deref, init);
      return build2 (COMPOUND_EXPR, result_type, storage_init, storage);
    }

  return storage;
}

/* Build a call to a copy procedure of a storage model given by an object.
   DEST, SRC and SIZE are as for a call to memcpy.  GNAT_SMO is the entity
   for the storage model object and COPY_TO says which procedure to use.  */

static tree
build_storage_model_copy (Entity_Id gnat_smo, tree dest, tree src, tree size,
			  bool copy_to)
{
  const Entity_Id gnat_copy_proc
    = copy_to
      ? Storage_Model_Copy_To (gnat_smo)
      : Storage_Model_Copy_From (gnat_smo);
  tree gnu_copy_proc = gnat_to_gnu (gnat_copy_proc);
  tree gnu_param_type_list = TYPE_ARG_TYPES (TREE_TYPE (gnu_copy_proc));
  tree t1 = TREE_VALUE (gnu_param_type_list);
  tree t2 = TREE_VALUE (TREE_CHAIN (gnu_param_type_list));
  tree t3 = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (gnu_param_type_list)));
  tree t4
    = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (TREE_CHAIN (gnu_param_type_list))));

  return
    build_call_n_expr (gnu_copy_proc,
		       4,
		       build_unary_op (ADDR_EXPR, t1, gnat_to_gnu (gnat_smo)),
		       build_unary_op (ADDR_EXPR, t2, dest),
		       build_unary_op (ADDR_EXPR, t3, src),
		       convert (t4, size));
}

/* Build a load of SRC using the storage model of GNAT_SMO.  */

tree
build_storage_model_load (Entity_Id gnat_smo, tree src)
{
  tree ret = build2 (LOAD_EXPR, TREE_TYPE (src), src, NULL_TREE);

  /* Unconstrained array references have no size so we need to store the
     storage object model for future processing by the machinery.  */
  if (TREE_CODE (src) == UNCONSTRAINED_ARRAY_REF)
    TREE_OPERAND (ret, 1) = build_int_cst (integer_type_node, gnat_smo);
  else
    TREE_OPERAND (ret, 1) = build_storage_model_load (gnat_smo, src, src);

  return ret;
}

/* Build a load of SRC into DEST using the storage model of GNAT_SMO.
   If SIZE is specified, use it, otherwise use the size of SRC.  */

tree
build_storage_model_load (Entity_Id gnat_smo, tree dest, tree src, tree size)
{
  gcc_assert (TREE_CODE (src) != LOAD_EXPR);

  if (!size)
    {
      size = TYPE_SIZE_UNIT (TREE_TYPE (src));
      size = SUBSTITUTE_PLACEHOLDER_IN_EXPR (size, src);
      size = INSTANTIATE_LOAD_IN_EXPR (size, gnat_smo);
    }

  return build_storage_model_copy (gnat_smo, dest, src, size, false);
}

/* Build a store of SRC into DEST using the storage model of GNAT_SMO.
   If SIZE is specified, use it, otherwise use the size of DEST.  */

tree
build_storage_model_store (Entity_Id gnat_smo, tree dest, tree src, tree size)
{
  gcc_assert (TREE_CODE (src) != LOAD_EXPR);

  if (!size)
    {
      size = TYPE_SIZE_UNIT (TREE_TYPE (dest));
      size = SUBSTITUTE_PLACEHOLDER_IN_EXPR (size, dest);
      size = INSTANTIATE_LOAD_IN_EXPR (size, gnat_smo);
    }

  return build_storage_model_copy (gnat_smo, dest, src, size, true);
}

/* Given a tree EXP, instantiate occurrences of LOAD_EXPR in it and associate
   them with the storage model of GNAT_SMO.  */

tree
instantiate_load_in_expr (tree exp, Entity_Id gnat_smo)
{
  const enum tree_code code = TREE_CODE (exp);
  tree type = TREE_TYPE (exp);
  tree op0, op1, op2, op3;
  tree new_tree;

  /* We handle TREE_LIST and COMPONENT_REF separately.  */
  if (code == TREE_LIST)
    {
      op0 = INSTANTIATE_LOAD_IN_EXPR (TREE_CHAIN (exp), gnat_smo);
      op1 = INSTANTIATE_LOAD_IN_EXPR (TREE_VALUE (exp), gnat_smo);
      if (op0 == TREE_CHAIN (exp) && op1 == TREE_VALUE (exp))
	return exp;

      return tree_cons (TREE_PURPOSE (exp), op1, op0);
    }
  else if (code == COMPONENT_REF)
    {
      /* The field.  */
      op1 = TREE_OPERAND (exp, 1);

      /* If it is a discriminant or equivalent, a LOAD_EXPR is needed.  */
      if (DECL_DISCRIMINANT_NUMBER (op1))
	return build_storage_model_load (gnat_smo, exp);

      op0 = INSTANTIATE_LOAD_IN_EXPR (TREE_OPERAND (exp, 0), gnat_smo);
      if (op0 == TREE_OPERAND (exp, 0))
	return exp;

      new_tree = fold_build3 (COMPONENT_REF, type, op0, op1, NULL_TREE);
   }
  else
    switch (TREE_CODE_CLASS (code))
      {
      case tcc_constant:
      case tcc_declaration:
	  return exp;

      case tcc_expression:
	if (code == LOAD_EXPR)
	  return exp;

	/* Fall through.  */

      case tcc_exceptional:
      case tcc_unary:
      case tcc_binary:
      case tcc_comparison:
      case tcc_reference:
	switch (TREE_CODE_LENGTH (code))
	  {
	  case 0:
	    return exp;

	  case 1:
	    op0 = INSTANTIATE_LOAD_IN_EXPR (TREE_OPERAND (exp, 0), gnat_smo);
	    if (op0 == TREE_OPERAND (exp, 0))
	      return exp;

	    new_tree = fold_build1 (code, type, op0);
	    break;

	  case 2:
	    op0 = INSTANTIATE_LOAD_IN_EXPR (TREE_OPERAND (exp, 0), gnat_smo);
	    op1 = INSTANTIATE_LOAD_IN_EXPR (TREE_OPERAND (exp, 1), gnat_smo);

	    if (op0 == TREE_OPERAND (exp, 0) && op1 == TREE_OPERAND (exp, 1))
	      return exp;

	    new_tree = fold_build2 (code, type, op0, op1);
	    break;

	  case 3:
	    op0 = INSTANTIATE_LOAD_IN_EXPR (TREE_OPERAND (exp, 0), gnat_smo);
	    op1 = INSTANTIATE_LOAD_IN_EXPR (TREE_OPERAND (exp, 1), gnat_smo);
	    op2 = INSTANTIATE_LOAD_IN_EXPR (TREE_OPERAND (exp, 2), gnat_smo);

	    if (op0 == TREE_OPERAND (exp, 0)
		&& op1 == TREE_OPERAND (exp, 1)
		&& op2 == TREE_OPERAND (exp, 2))
	      return exp;

	    new_tree = fold_build3 (code, type, op0, op1, op2);
	    break;

	  case 4:
	    op0 = INSTANTIATE_LOAD_IN_EXPR (TREE_OPERAND (exp, 0), gnat_smo);
	    op1 = INSTANTIATE_LOAD_IN_EXPR (TREE_OPERAND (exp, 1), gnat_smo);
	    op2 = INSTANTIATE_LOAD_IN_EXPR (TREE_OPERAND (exp, 2), gnat_smo);
	    op3 = INSTANTIATE_LOAD_IN_EXPR (TREE_OPERAND (exp, 3), gnat_smo);

	    if (op0 == TREE_OPERAND (exp, 0)
		&& op1 == TREE_OPERAND (exp, 1)
		&& op2 == TREE_OPERAND (exp, 2)
		&& op3 == TREE_OPERAND (exp, 3))
	      return exp;

	    new_tree = fold (build4 (code, type, op0, op1, op2, op3));
	    break;

	  default:
	    gcc_unreachable ();
	  }
	break;

      case tcc_vl_exp:
	{
	  gcc_assert (code == CALL_EXPR);

	  const int n = call_expr_nargs (exp);
	  gcc_assert (n > 0);
	  tree *argarray = XALLOCAVEC (tree, n);
	  for (int i = 0; i < n; i++)
	    argarray[i]
	      = INSTANTIATE_LOAD_IN_EXPR (CALL_EXPR_ARG (exp, i), gnat_smo);

	  for (int i = 0; i < n; i++)
	    if (argarray[i] != CALL_EXPR_ARG (exp, i))
	      return build_call_array (type, CALL_EXPR_FN (exp), n, argarray);

	  return exp;
	}

      default:
	gcc_unreachable ();
      }

  TREE_READONLY (new_tree) |= TREE_READONLY (exp);

  if (code == INDIRECT_REF || code == ARRAY_REF || code == ARRAY_RANGE_REF)
    TREE_THIS_NOTRAP (new_tree) |= TREE_THIS_NOTRAP (exp);

  return new_tree;
}

/* Given an array or slice reference, instantiate occurrences of LOAD_EXPR in
   it and associate them with the storage model of GNAT_SMO.  */

void
instantiate_load_in_array_ref (tree ref, Entity_Id gnat_smo)
{
  tree domain_type = TYPE_DOMAIN (TREE_TYPE (TREE_OPERAND (ref, 0)));
  tree elem_type = TREE_TYPE (TREE_TYPE (TREE_OPERAND (ref, 0)));

  TREE_OPERAND (ref, 2)
    = SUBSTITUTE_PLACEHOLDER_IN_EXPR (TYPE_MIN_VALUE (domain_type), ref);
  TREE_OPERAND (ref, 2)
    = INSTANTIATE_LOAD_IN_EXPR (TREE_OPERAND (ref, 2), gnat_smo);

  TREE_OPERAND (ref, 3)
    = size_binop (EXACT_DIV_EXPR,
		  SUBSTITUTE_PLACEHOLDER_IN_EXPR (TYPE_SIZE_UNIT (elem_type),
						  ref),
		  size_int (TYPE_ALIGN_UNIT (elem_type)));
  TREE_OPERAND (ref, 3)
    = INSTANTIATE_LOAD_IN_EXPR (TREE_OPERAND (ref, 3), gnat_smo);
}

/* Indicate that we need to take the address of T and that it therefore
   should not be allocated in a register.  Return true if successful.  */

bool
gnat_mark_addressable (tree t)
{
  while (true)
    switch (TREE_CODE (t))
      {
      case ADDR_EXPR:
      case COMPONENT_REF:
      case ARRAY_REF:
      case ARRAY_RANGE_REF:
      case REALPART_EXPR:
      case IMAGPART_EXPR:
      case VIEW_CONVERT_EXPR:
      case NON_LVALUE_EXPR:
      CASE_CONVERT:
	t = TREE_OPERAND (t, 0);
	break;

      case COMPOUND_EXPR:
	t = TREE_OPERAND (t, 1);
	break;

      case CONSTRUCTOR:
	TREE_ADDRESSABLE (t) = 1;
	return true;

      case VAR_DECL:
      case PARM_DECL:
      case RESULT_DECL:
	TREE_ADDRESSABLE (t) = 1;
	return true;

      case FUNCTION_DECL:
	TREE_ADDRESSABLE (t) = 1;
	return true;

      case CONST_DECL:
	return DECL_CONST_CORRESPONDING_VAR (t)
	       && gnat_mark_addressable (DECL_CONST_CORRESPONDING_VAR (t));

      default:
	return true;
    }
}

/* Return true if EXP is a stable expression for the purpose of the functions
   below and, therefore, can be returned unmodified by them.  We accept things
   that are actual constants or that have already been handled.  */

static bool
gnat_stable_expr_p (tree exp)
{
  enum tree_code code = TREE_CODE (exp);
  return TREE_CONSTANT (exp) || code == NULL_EXPR || code == SAVE_EXPR;
}

/* Save EXP for later use or reuse.  This is equivalent to save_expr in tree.cc
   but we know how to handle our own nodes.  */

tree
gnat_save_expr (tree exp)
{
  tree type = TREE_TYPE (exp);
  enum tree_code code = TREE_CODE (exp);

  if (gnat_stable_expr_p (exp))
    return exp;

  if (code == UNCONSTRAINED_ARRAY_REF)
    {
      tree t = build1 (code, type, gnat_save_expr (TREE_OPERAND (exp, 0)));
      TREE_READONLY (t) = TYPE_READONLY (type);
      return t;
    }

  /* If this is a COMPONENT_REF of a fat pointer, save the entire fat pointer.
     This may be more efficient, but will also allow us to more easily find
     the match for the PLACEHOLDER_EXPR.  */
  if (code == COMPONENT_REF
      && TYPE_IS_FAT_POINTER_P (TREE_TYPE (TREE_OPERAND (exp, 0))))
    return build3 (code, type, gnat_save_expr (TREE_OPERAND (exp, 0)),
		   TREE_OPERAND (exp, 1), NULL_TREE);

  return save_expr (exp);
}

/* Protect EXP for immediate reuse.  This is a variant of gnat_save_expr that
   is optimized under the assumption that EXP's value doesn't change before
   its subsequent reuse(s) except potentially through its reevaluation.

   gnat_protect_expr guarantees that multiple evaluations of the expression
   will not generate multiple side effects, whereas gnat_save_expr further
   guarantees that all evaluations will yield the same result.  */

tree
gnat_protect_expr (tree exp)
{
  tree type = TREE_TYPE (exp);
  enum tree_code code = TREE_CODE (exp);

  if (gnat_stable_expr_p (exp))
    return exp;

  /* If EXP has no side effects, we theoretically don't need to do anything.
     However, we may be recursively passed more and more complex expressions
     involving checks which will be reused multiple times and eventually be
     unshared for gimplification; in order to avoid a complexity explosion
     at that point, we protect any expressions more complex than a simple
     arithmetic expression.  */
  if (!TREE_SIDE_EFFECTS (exp))
    {
      tree inner = skip_simple_arithmetic (exp);
      if (!EXPR_P (inner) || REFERENCE_CLASS_P (inner))
	return exp;
    }

  /* If this is a conversion, protect what's inside the conversion.  */
  if (code == NON_LVALUE_EXPR
      || CONVERT_EXPR_CODE_P (code)
      || code == VIEW_CONVERT_EXPR)
    return build1 (code, type, gnat_protect_expr (TREE_OPERAND (exp, 0)));

  /* If we're indirectly referencing something, we only need to protect the
     address since the data itself can't change in these situations.  */
  if (code == INDIRECT_REF || code == UNCONSTRAINED_ARRAY_REF)
    {
      tree t = build1 (code, type, gnat_protect_expr (TREE_OPERAND (exp, 0)));
      TREE_READONLY (t) = TYPE_READONLY (type);
      return t;
    }

  /* Likewise if we're indirectly referencing part of something.  */
  if (code == COMPONENT_REF
      && INDIRECT_REF_P (TREE_OPERAND (exp, 0)))
    return build3 (code, type, gnat_protect_expr (TREE_OPERAND (exp, 0)),
		   TREE_OPERAND (exp, 1), NULL_TREE);

  /* An atomic load is an INDIRECT_REF of its first argument, so apply the
     same transformation as in the INDIRECT_REF case above.  */
  if (code == CALL_EXPR && call_is_atomic_load (exp))
    return build_call_expr (TREE_OPERAND (CALL_EXPR_FN (exp), 0), 2,
			    gnat_protect_expr (CALL_EXPR_ARG (exp, 0)),
			    CALL_EXPR_ARG (exp, 1));

  /* If this is a COMPONENT_REF of a fat pointer, save the entire fat pointer.
     This may be more efficient, but will also allow us to more easily find
     the match for the PLACEHOLDER_EXPR.  */
  if (code == COMPONENT_REF
      && TYPE_IS_FAT_POINTER_P (TREE_TYPE (TREE_OPERAND (exp, 0))))
    return build3 (code, type, gnat_protect_expr (TREE_OPERAND (exp, 0)),
		   TREE_OPERAND (exp, 1), NULL_TREE);

  /* If this is a fat pointer or a scalar, just make a SAVE_EXPR.  Likewise
     for a CALL_EXPR as large objects are returned via invisible reference
     in most ABIs so the temporary will directly be filled by the callee.  */
  if (TYPE_IS_FAT_POINTER_P (type)
      || !AGGREGATE_TYPE_P (type)
      || code == CALL_EXPR)
    return save_expr (exp);

  /* Otherwise reference, protect the address and dereference.  */
  return
    build_unary_op (INDIRECT_REF, type,
		    save_expr (build_unary_op (ADDR_EXPR, NULL_TREE, exp)));
}

/* This is equivalent to stabilize_reference_1 in tree.cc but we take an extra
   argument to force evaluation of everything.  */

static tree
gnat_stabilize_reference_1 (tree e, void *data)
{
  const bool force = *(bool *)data;
  enum tree_code code = TREE_CODE (e);
  tree type = TREE_TYPE (e);
  tree result;

  if (gnat_stable_expr_p (e))
    return e;

  switch (TREE_CODE_CLASS (code))
    {
    case tcc_exceptional:
    case tcc_declaration:
    case tcc_comparison:
    case tcc_expression:
    case tcc_reference:
    case tcc_vl_exp:
      /* If this is a COMPONENT_REF of a fat pointer, save the entire
	 fat pointer.  This may be more efficient, but will also allow
	 us to more easily find the match for the PLACEHOLDER_EXPR.  */
      if (code == COMPONENT_REF
	  && TYPE_IS_FAT_POINTER_P (TREE_TYPE (TREE_OPERAND (e, 0))))
	result
	  = build3 (code, type,
		    gnat_stabilize_reference_1 (TREE_OPERAND (e, 0), data),
		    TREE_OPERAND (e, 1), NULL_TREE);
      /* If the expression has side-effects, then encase it in a SAVE_EXPR
	 so that it will only be evaluated once.  */
      /* The tcc_reference and tcc_comparison classes could be handled as
	 below, but it is generally faster to only evaluate them once.  */
      else if (TREE_SIDE_EFFECTS (e) || force)
	return save_expr (e);
      else
	return e;
      break;

    case tcc_binary:
      /* Recursively stabilize each operand.  */
      result
	= build2 (code, type,
		  gnat_stabilize_reference_1 (TREE_OPERAND (e, 0), data),
		  gnat_stabilize_reference_1 (TREE_OPERAND (e, 1), data));
      break;

    case tcc_unary:
      /* Recursively stabilize each operand.  */
      result
	= build1 (code, type,
		  gnat_stabilize_reference_1 (TREE_OPERAND (e, 0), data));
      break;

    default:
      gcc_unreachable ();
    }

  /* See gnat_rewrite_reference below for the rationale.  */
  TREE_READONLY (result) = TREE_READONLY (e);
  TREE_THIS_VOLATILE (result) = TREE_THIS_VOLATILE (e);

  if (TREE_SIDE_EFFECTS (e))
    TREE_SIDE_EFFECTS (result) = 1;

  return result;
}

/* This is equivalent to stabilize_reference in tree.cc but we know how to
   handle our own nodes and we take extra arguments.  FORCE says whether to
   force evaluation of everything in REF.  INIT is set to the first arm of
   a COMPOUND_EXPR present in REF, if any.  */

tree
gnat_stabilize_reference (tree ref, bool force, tree *init)
{
  return
    gnat_rewrite_reference (ref, gnat_stabilize_reference_1, &force, init);
}

/* Rewrite reference REF and call FUNC on each expression within REF in the
   process.  DATA is passed unmodified to FUNC.  INIT is set to the first
   arm of a COMPOUND_EXPR present in REF, if any.  */

tree
gnat_rewrite_reference (tree ref, rewrite_fn func, void *data, tree *init)
{
  tree type = TREE_TYPE (ref);
  enum tree_code code = TREE_CODE (ref);
  tree result;

  switch (code)
    {
    case CONST_DECL:
    case VAR_DECL:
    case PARM_DECL:
    case RESULT_DECL:
      /* No action is needed in this case.  */
      return ref;

    CASE_CONVERT:
    case FLOAT_EXPR:
    case FIX_TRUNC_EXPR:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
    case VIEW_CONVERT_EXPR:
      result
	= build1 (code, type,
		  gnat_rewrite_reference (TREE_OPERAND (ref, 0), func, data,
					  init));
      break;

    case INDIRECT_REF:
    case UNCONSTRAINED_ARRAY_REF:
      result = build1 (code, type, func (TREE_OPERAND (ref, 0), data));
      break;

    case COMPONENT_REF:
      result = build3 (COMPONENT_REF, type,
		       gnat_rewrite_reference (TREE_OPERAND (ref, 0), func,
					       data, init),
		       TREE_OPERAND (ref, 1), NULL_TREE);
      break;

    case BIT_FIELD_REF:
      result = build3 (BIT_FIELD_REF, type,
		       gnat_rewrite_reference (TREE_OPERAND (ref, 0), func,
					       data, init),
		       TREE_OPERAND (ref, 1), TREE_OPERAND (ref, 2));
      REF_REVERSE_STORAGE_ORDER (result) = REF_REVERSE_STORAGE_ORDER (ref);
      break;

    case ARRAY_REF:
    case ARRAY_RANGE_REF:
      result
	= build4 (code, type,
		  gnat_rewrite_reference (TREE_OPERAND (ref, 0), func, data,
					  init),
		  func (TREE_OPERAND (ref, 1), data),
		  TREE_OPERAND (ref, 2), TREE_OPERAND (ref, 3));
      break;

    case COMPOUND_EXPR:
      gcc_assert (!*init);
      *init = TREE_OPERAND (ref, 0);
      /* We expect only the pattern built in Call_to_gnu.  */
      gcc_assert (DECL_P (TREE_OPERAND (ref, 1))
		  || (TREE_CODE (TREE_OPERAND (ref, 1)) == COMPONENT_REF
		      && DECL_P (TREE_OPERAND (TREE_OPERAND (ref, 1), 0))));
      return TREE_OPERAND (ref, 1);

    case CALL_EXPR:
      {
	/* This can only be an atomic load.  */
	gcc_assert (call_is_atomic_load (ref));

	/* An atomic load is an INDIRECT_REF of its first argument.  */
	tree t = CALL_EXPR_ARG (ref, 0);
	if (TREE_CODE (t) == NOP_EXPR)
	  t = TREE_OPERAND (t, 0);
	if (TREE_CODE (t) == ADDR_EXPR)
	  t = build1 (ADDR_EXPR, TREE_TYPE (t),
		      gnat_rewrite_reference (TREE_OPERAND (t, 0), func, data,
					      init));
	else
	  t = func (t, data);
	t = fold_convert (TREE_TYPE (CALL_EXPR_ARG (ref, 0)), t);

	result = build_call_expr (TREE_OPERAND (CALL_EXPR_FN (ref), 0), 2,
				  t, CALL_EXPR_ARG (ref, 1));
      }
      break;

    case ERROR_MARK:
    case NULL_EXPR:
      return ref;

    default:
      gcc_unreachable ();
    }

  /* TREE_READONLY and TREE_THIS_VOLATILE set on the initial expression may
     not be sustained across some paths, such as the one for INDIRECT_REF.

     Special care should be taken regarding TREE_SIDE_EFFECTS, because some
     paths introduce side-effects where there was none initially (e.g. if a
     SAVE_EXPR is built) and we also want to keep track of that.  */
  TREE_READONLY (result) = TREE_READONLY (ref);
  TREE_THIS_VOLATILE (result) = TREE_THIS_VOLATILE (ref);

  if (TREE_SIDE_EFFECTS (ref))
    TREE_SIDE_EFFECTS (result) = 1;

  if (code == INDIRECT_REF
      || code == UNCONSTRAINED_ARRAY_REF
      || code == ARRAY_REF
      || code == ARRAY_RANGE_REF)
    TREE_THIS_NOTRAP (result) = TREE_THIS_NOTRAP (ref);

  return result;
}

/* This is equivalent to get_inner_reference in expr.cc but it returns the
   ultimate containing object only if the reference (lvalue) is constant,
   i.e. if it doesn't depend on the context in which it is evaluated.  */

tree
get_inner_constant_reference (tree exp)
{
  while (true)
    {
      switch (TREE_CODE (exp))
	{
	case BIT_FIELD_REF:
	  break;

	case COMPONENT_REF:
	  if (!TREE_CONSTANT (DECL_FIELD_OFFSET (TREE_OPERAND (exp, 1))))
	    return NULL_TREE;
	  break;

	case ARRAY_REF:
	case ARRAY_RANGE_REF:
	  {
	    tree array_type = TREE_TYPE (TREE_OPERAND (exp, 0));
	    if (!TREE_CONSTANT (TREE_OPERAND (exp, 1))
	        || !TREE_CONSTANT (TYPE_MIN_VALUE (TYPE_DOMAIN (array_type)))
	        || !TREE_CONSTANT (TYPE_SIZE_UNIT (TREE_TYPE (array_type))))
	      return NULL_TREE;
	  }
	  break;

	case REALPART_EXPR:
	case IMAGPART_EXPR:
	case VIEW_CONVERT_EXPR:
	  break;

	default:
	  goto done;
	}

      exp = TREE_OPERAND (exp, 0);
    }

done:
  return exp;
}

/* Return true if EXPR is the addition or the subtraction of a constant and,
   if so, set *ADD to the addend, *CST to the constant and *MINUS_P to true
   if this is a subtraction.  */

bool
is_simple_additive_expression (tree expr, tree *add, tree *cst, bool *minus_p)
{
  /* Skip overflow checks.  */
  if (TREE_CODE (expr) == COND_EXPR
      && TREE_CODE (COND_EXPR_THEN (expr)) == COMPOUND_EXPR
      && TREE_CODE (TREE_OPERAND (COND_EXPR_THEN (expr), 0)) == CALL_EXPR
      && get_callee_fndecl (TREE_OPERAND (COND_EXPR_THEN (expr), 0))
         == gnat_raise_decls[CE_Overflow_Check_Failed])
    expr = COND_EXPR_ELSE (expr);

  if (TREE_CODE (expr) == PLUS_EXPR)
    {
      if (TREE_CONSTANT (TREE_OPERAND (expr, 0)))
	{
	  *add = TREE_OPERAND (expr, 1);
	  *cst = TREE_OPERAND (expr, 0);
	  *minus_p = false;
	  return true;
	}
      else if (TREE_CONSTANT (TREE_OPERAND (expr, 1)))
	{
	  *add = TREE_OPERAND (expr, 0);
	  *cst = TREE_OPERAND (expr, 1);
	  *minus_p = false;
	  return true;
	}
    }
  else if (TREE_CODE (expr) == MINUS_EXPR)
    {
      if (TREE_CONSTANT (TREE_OPERAND (expr, 1)))
	{
	  *add = TREE_OPERAND (expr, 0);
	  *cst = TREE_OPERAND (expr, 1);
	  *minus_p = true;
	  return true;
	}
    }

  return false;
}

/* If EXPR is an expression that is invariant in the current function, in the
   sense that it can be evaluated anywhere in the function and any number of
   times, return EXPR or an equivalent expression.  Otherwise return NULL.  */

tree
gnat_invariant_expr (tree expr)
{
  tree type = TREE_TYPE (expr);
  tree add, cst;
  bool minus_p;

  expr = remove_conversions (expr, false);

  /* Look through temporaries created to capture values.  */
  while ((TREE_CODE (expr) == CONST_DECL
	  || (VAR_P (expr) && TREE_READONLY (expr)))
	 && decl_function_context (expr) == current_function_decl
	 && DECL_INITIAL (expr))
    {
      expr = DECL_INITIAL (expr);
      /* Look into CONSTRUCTORs built to initialize padded types.  */
      expr = maybe_padded_object (expr);
      expr = remove_conversions (expr, false);
    }

  /* We are only interested in scalar types at the moment and, even if we may
     have gone through padding types in the above loop, we must be back to a
     scalar value at this point.  */
  if (AGGREGATE_TYPE_P (TREE_TYPE (expr)))
    return NULL_TREE;

  if (TREE_CONSTANT (expr))
    return fold_convert (type, expr);

  /* Deal with aligning patterns.  */
  if (TREE_CODE (expr) == BIT_AND_EXPR
      && TREE_CONSTANT (TREE_OPERAND (expr, 1)))
    {
      tree op0 = gnat_invariant_expr (TREE_OPERAND (expr, 0));
      if (op0)
	return fold_build2 (BIT_AND_EXPR, type, op0, TREE_OPERAND (expr, 1));
      else
	return NULL_TREE;
    }

  /* Deal with addition or subtraction of constants.  */
  if (is_simple_additive_expression (expr, &add, &cst, &minus_p))
    {
      add = gnat_invariant_expr (add);
      if (add)
	return
	  fold_build2 (minus_p ? MINUS_EXPR : PLUS_EXPR, type,
		       fold_convert (type, add), fold_convert (type, cst));
      else
	return NULL_TREE;
    }

  bool invariant_p = false;
  tree t = expr;

  while (true)
    {
      switch (TREE_CODE (t))
	{
	case COMPONENT_REF:
	  invariant_p |= DECL_INVARIANT_P (TREE_OPERAND (t, 1));
	  break;

	case ARRAY_REF:
	case ARRAY_RANGE_REF:
	  {
	    tree array_type = TREE_TYPE (TREE_OPERAND (t, 0));
	    if (!TREE_CONSTANT (TREE_OPERAND (t, 1))
	        || !TREE_CONSTANT (TYPE_MIN_VALUE (TYPE_DOMAIN (array_type)))
	        || !TREE_CONSTANT (TYPE_SIZE_UNIT (TREE_TYPE (array_type))))
	      return NULL_TREE;
	  }
	  break;

	case BIT_FIELD_REF:
	case REALPART_EXPR:
	case IMAGPART_EXPR:
	case VIEW_CONVERT_EXPR:
	CASE_CONVERT:
	  break;

	case INDIRECT_REF:
	  if ((!invariant_p && !TREE_READONLY (t)) || TREE_SIDE_EFFECTS (t))
	    return NULL_TREE;
	  invariant_p = false;
	  break;

	default:
	  goto object;
	}

      t = TREE_OPERAND (t, 0);
    }

object:
  if (TREE_SIDE_EFFECTS (t))
    return NULL_TREE;

  if (TREE_CODE (t) == CONST_DECL
      && (DECL_EXTERNAL (t)
	  || decl_function_context (t) != current_function_decl))
    return fold_convert (type, expr);

  if (!invariant_p && !TREE_READONLY (t))
    return NULL_TREE;

  if (TREE_CODE (t) == PARM_DECL)
    return fold_convert (type, expr);

  if (VAR_P (t)
      && (DECL_EXTERNAL (t)
	  || decl_function_context (t) != current_function_decl))
    return fold_convert (type, expr);

  return NULL_TREE;
}
