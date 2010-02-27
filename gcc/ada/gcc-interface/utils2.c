/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                               U T I L S 2                                *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *          Copyright (C) 1992-2009, Free Software Foundation, Inc.         *
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
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "ggc.h"
#include "flags.h"
#include "output.h"
#include "ada.h"
#include "types.h"
#include "atree.h"
#include "stringt.h"
#include "namet.h"
#include "uintp.h"
#include "fe.h"
#include "elists.h"
#include "nlists.h"
#include "sinfo.h"
#include "einfo.h"
#include "ada-tree.h"
#include "gigi.h"
#include "snames.h"

static tree find_common_type (tree, tree);
static bool contains_save_expr_p (tree);
static tree contains_null_expr (tree);
static tree compare_arrays (tree, tree, tree);
static tree nonbinary_modular_operation (enum tree_code, tree, tree, tree);
static tree build_simple_component_ref (tree, tree, tree, bool);

/* Prepare expr to be an argument of a TRUTH_NOT_EXPR or other logical
   operation.

   This preparation consists of taking the ordinary representation of
   an expression expr and producing a valid tree boolean expression
   describing whether expr is nonzero. We could simply always do

      build_binary_op (NE_EXPR, expr, integer_zero_node, 1),

   but we optimize comparisons, &&, ||, and !.

   The resulting type should always be the same as the input type.
   This function is simpler than the corresponding C version since
   the only possible operands will be things of Boolean type.  */

tree
gnat_truthvalue_conversion (tree expr)
{
  tree type = TREE_TYPE (expr);

  switch (TREE_CODE (expr))
    {
    case EQ_EXPR:  case NE_EXPR: case LE_EXPR: case GE_EXPR:
    case LT_EXPR:  case GT_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
    case ERROR_MARK:
      return expr;

    case INTEGER_CST:
      return (integer_zerop (expr)
	      ? build_int_cst (type, 0)
	      : build_int_cst (type, 1));

    case REAL_CST:
      return (real_zerop (expr)
	      ? fold_convert (type, integer_zero_node)
	      : fold_convert (type, integer_one_node));

    case COND_EXPR:
      /* Distribute the conversion into the arms of a COND_EXPR.  */
      {
	tree arg1 = gnat_truthvalue_conversion (TREE_OPERAND (expr, 1));
	tree arg2 = gnat_truthvalue_conversion (TREE_OPERAND (expr, 2));
	return fold_build3 (COND_EXPR, type, TREE_OPERAND (expr, 0),
			    arg1, arg2);
      }

    default:
      return build_binary_op (NE_EXPR, type, expr,
			      fold_convert (type, integer_zero_node));
    }
}

/* Return the base type of TYPE.  */

tree
get_base_type (tree type)
{
  if (TREE_CODE (type) == RECORD_TYPE
      && TYPE_JUSTIFIED_MODULAR_P (type))
    type = TREE_TYPE (TYPE_FIELDS (type));

  while (TREE_TYPE (type)
	 && (TREE_CODE (type) == INTEGER_TYPE
	     || TREE_CODE (type) == REAL_TYPE))
    type = TREE_TYPE (type);

  return type;
}

/* EXP is a GCC tree representing an address.  See if we can find how
   strictly the object at that address is aligned.   Return that alignment
   in bits.  If we don't know anything about the alignment, return 0.  */

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
      /* The value of a COMPOUND_EXPR is that of it's second operand.  */
      this_alignment = known_alignment (TREE_OPERAND (exp, 1));
      break;

    case PLUS_EXPR:
    case MINUS_EXPR:
      /* If two address are added, the alignment of the result is the
	 minimum of the two alignments.  */
      lhs = known_alignment (TREE_OPERAND (exp, 0));
      rhs = known_alignment (TREE_OPERAND (exp, 1));
      this_alignment = MIN (lhs, rhs);
      break;

    case POINTER_PLUS_EXPR:
      lhs = known_alignment (TREE_OPERAND (exp, 0));
      rhs = known_alignment (TREE_OPERAND (exp, 1));
      /* If we don't know the alignment of the offset, we assume that
	 of the base.  */
      if (rhs == 0)
	this_alignment = lhs;
      else
	this_alignment = MIN (lhs, rhs);
      break;

    case COND_EXPR:
      /* If there is a choice between two values, use the smallest one.  */
      lhs = known_alignment (TREE_OPERAND (exp, 1));
      rhs = known_alignment (TREE_OPERAND (exp, 2));
      this_alignment = MIN (lhs, rhs);
      break;

    case INTEGER_CST:
      {
	unsigned HOST_WIDE_INT c = TREE_INT_CST_LOW (exp);
	/* The first part of this represents the lowest bit in the constant,
	   but it is originally in bytes, not bits.  */
	this_alignment = MIN (BITS_PER_UNIT * (c & -c), BIGGEST_ALIGNMENT);
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
      this_alignment = expr_align (TREE_OPERAND (exp, 0));
      break;

    default:
      /* For other pointer expressions, we assume that the pointed-to object
	 is at least as aligned as the pointed-to type.  Beware that we can
	 have a dummy type here (e.g. a Taft Amendment type), for which the
	 alignment is meaningless and should be ignored.  */
      if (POINTER_TYPE_P (TREE_TYPE (exp))
	  && !TYPE_IS_DUMMY_P (TREE_TYPE (TREE_TYPE (exp))))
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
  /* ??? As of today, various constructs lead here with types of different
     sizes even when both constants (e.g. tagged types, packable vs regular
     component types, padded vs unpadded types, ...).  While some of these
     would better be handled upstream (types should be made consistent before
     calling into build_binary_op), some others are really expected and we
     have to be careful.  */

  /* We must prevent writing more than what the target may hold if this is for
     an assignment and the case of tagged types is handled in build_binary_op
     so use the lhs type if it is known to be smaller, or of constant size and
     the rhs type is not, whatever the modes.  We also force t1 in case of
     constant size equality to minimize occurrences of view conversions on the
     lhs of assignments.  */
  if (TREE_CONSTANT (TYPE_SIZE (t1))
      && (!TREE_CONSTANT (TYPE_SIZE (t2))
          || !tree_int_cst_lt (TYPE_SIZE (t2), TYPE_SIZE (t1))))
    return t1;

  /* Otherwise, if the lhs type is non-BLKmode, use it.  Note that we know
     that we will not have any alignment problems since, if we did, the
     non-BLKmode type could not have been used.  */
  if (TYPE_MODE (t1) != BLKmode)
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

/* See if EXP contains a SAVE_EXPR in a position where we would
   normally put it.

   ??? This is a real kludge, but is probably the best approach short
   of some very general solution.  */

static bool
contains_save_expr_p (tree exp)
{
  switch (TREE_CODE (exp))
    {
    case SAVE_EXPR:
      return true;

    case ADDR_EXPR:  case INDIRECT_REF:
    case COMPONENT_REF:
    CASE_CONVERT: case VIEW_CONVERT_EXPR:
      return contains_save_expr_p (TREE_OPERAND (exp, 0));

    case CONSTRUCTOR:
      {
	tree value;
	unsigned HOST_WIDE_INT ix;

	FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (exp), ix, value)
	  if (contains_save_expr_p (value))
	    return true;
	return false;
      }

    default:
      return false;
    }
}

/* See if EXP contains a NULL_EXPR in an expression we use for sizes. Return
   it if so.  This is used to detect types whose sizes involve computations
   that are known to raise Constraint_Error.  */

static tree
contains_null_expr (tree exp)
{
  tree tem;

  if (TREE_CODE (exp) == NULL_EXPR)
    return exp;

  switch (TREE_CODE_CLASS (TREE_CODE (exp)))
    {
    case tcc_unary:
      return contains_null_expr (TREE_OPERAND (exp, 0));

    case tcc_comparison:
    case tcc_binary:
      tem = contains_null_expr (TREE_OPERAND (exp, 0));
      if (tem)
	return tem;

      return contains_null_expr (TREE_OPERAND (exp, 1));

    case tcc_expression:
      switch (TREE_CODE (exp))
	{
	case SAVE_EXPR:
	  return contains_null_expr (TREE_OPERAND (exp, 0));

	case COND_EXPR:
	  tem = contains_null_expr (TREE_OPERAND (exp, 0));
	  if (tem)
	    return tem;

	  tem = contains_null_expr (TREE_OPERAND (exp, 1));
	  if (tem)
	    return tem;

	  return contains_null_expr (TREE_OPERAND (exp, 2));

	default:
	  return 0;
	}

    default:
      return 0;
    }
}

/* Return an expression tree representing an equality comparison of
   A1 and A2, two objects of ARRAY_TYPE.  The returned expression should
   be of type RESULT_TYPE

   Two arrays are equal in one of two ways: (1) if both have zero length
   in some dimension (not necessarily the same dimension) or (2) if the
   lengths in each dimension are equal and the data is equal.  We perform the
   length tests in as efficient a manner as possible.  */

static tree
compare_arrays (tree result_type, tree a1, tree a2)
{
  tree t1 = TREE_TYPE (a1);
  tree t2 = TREE_TYPE (a2);
  tree result = convert (result_type, integer_one_node);
  tree a1_is_null = convert (result_type, integer_zero_node);
  tree a2_is_null = convert (result_type, integer_zero_node);
  bool length_zero_p = false;

  /* Process each dimension separately and compare the lengths.  If any
     dimension has a size known to be zero, set SIZE_ZERO_P to 1 to
     suppress the comparison of the data.  */
  while (TREE_CODE (t1) == ARRAY_TYPE && TREE_CODE (t2) == ARRAY_TYPE)
    {
      tree lb1 = TYPE_MIN_VALUE (TYPE_DOMAIN (t1));
      tree ub1 = TYPE_MAX_VALUE (TYPE_DOMAIN (t1));
      tree lb2 = TYPE_MIN_VALUE (TYPE_DOMAIN (t2));
      tree ub2 = TYPE_MAX_VALUE (TYPE_DOMAIN (t2));
      tree bt = get_base_type (TREE_TYPE (lb1));
      tree length1 = fold_build2 (MINUS_EXPR, bt, ub1, lb1);
      tree length2 = fold_build2 (MINUS_EXPR, bt, ub2, lb2);
      tree nbt;
      tree tem;
      tree comparison, this_a1_is_null, this_a2_is_null;

      /* If the length of the first array is a constant, swap our operands
	 unless the length of the second array is the constant zero.
	 Note that we have set the `length' values to the length - 1.  */
      if (TREE_CODE (length1) == INTEGER_CST
	  && !integer_zerop (fold_build2 (PLUS_EXPR, bt, length2,
					  convert (bt, integer_one_node))))
	{
	  tem = a1, a1 = a2, a2 = tem;
	  tem = t1, t1 = t2, t2 = tem;
	  tem = lb1, lb1 = lb2, lb2 = tem;
	  tem = ub1, ub1 = ub2, ub2 = tem;
	  tem = length1, length1 = length2, length2 = tem;
	  tem = a1_is_null, a1_is_null = a2_is_null, a2_is_null = tem;
	}

      /* If the length of this dimension in the second array is the constant
	 zero, we can just go inside the original bounds for the first
	 array and see if last < first.  */
      if (integer_zerop (fold_build2 (PLUS_EXPR, bt, length2,
				      convert (bt, integer_one_node))))
	{
	  tree ub = TYPE_MAX_VALUE (TYPE_INDEX_TYPE (TYPE_DOMAIN (t1)));
	  tree lb = TYPE_MIN_VALUE (TYPE_INDEX_TYPE (TYPE_DOMAIN (t1)));

	  comparison = build_binary_op (LT_EXPR, result_type, ub, lb);
	  comparison = SUBSTITUTE_PLACEHOLDER_IN_EXPR (comparison, a1);
	  length1 = SUBSTITUTE_PLACEHOLDER_IN_EXPR (length1, a1);

	  length_zero_p = true;
	  this_a1_is_null = comparison;
	  this_a2_is_null = convert (result_type, integer_one_node);
	}

      /* If the length is some other constant value, we know that the
	 this dimension in the first array cannot be superflat, so we
	 can just use its length from the actual stored bounds.  */
      else if (TREE_CODE (length2) == INTEGER_CST)
	{
	  ub1 = TYPE_MAX_VALUE (TYPE_INDEX_TYPE (TYPE_DOMAIN (t1)));
	  lb1 = TYPE_MIN_VALUE (TYPE_INDEX_TYPE (TYPE_DOMAIN (t1)));
	  ub2 = TYPE_MAX_VALUE (TYPE_INDEX_TYPE (TYPE_DOMAIN (t2)));
	  lb2 = TYPE_MIN_VALUE (TYPE_INDEX_TYPE (TYPE_DOMAIN (t2)));
	  nbt = get_base_type (TREE_TYPE (ub1));

	  comparison
	    = build_binary_op (EQ_EXPR, result_type,
			       build_binary_op (MINUS_EXPR, nbt, ub1, lb1),
			       build_binary_op (MINUS_EXPR, nbt, ub2, lb2));

	  /* Note that we know that UB2 and LB2 are constant and hence
	     cannot contain a PLACEHOLDER_EXPR.  */

	  comparison = SUBSTITUTE_PLACEHOLDER_IN_EXPR (comparison, a1);
	  length1 = SUBSTITUTE_PLACEHOLDER_IN_EXPR (length1, a1);

	  this_a1_is_null = build_binary_op (LT_EXPR, result_type, ub1, lb1);
	  this_a2_is_null = convert (result_type, integer_zero_node);
	}

      /* Otherwise compare the computed lengths.  */
      else
	{
	  length1 = SUBSTITUTE_PLACEHOLDER_IN_EXPR (length1, a1);
	  length2 = SUBSTITUTE_PLACEHOLDER_IN_EXPR (length2, a2);

	  comparison
	    = build_binary_op (EQ_EXPR, result_type, length1, length2);

	  this_a1_is_null
	    = build_binary_op (LT_EXPR, result_type, length1,
			       convert (bt, integer_zero_node));
	  this_a2_is_null
	    = build_binary_op (LT_EXPR, result_type, length2,
			       convert (bt, integer_zero_node));
	}

      result = build_binary_op (TRUTH_ANDIF_EXPR, result_type,
				result, comparison);

      a1_is_null = build_binary_op (TRUTH_ORIF_EXPR, result_type,
				    this_a1_is_null, a1_is_null);
      a2_is_null = build_binary_op (TRUTH_ORIF_EXPR, result_type,
				    this_a2_is_null, a2_is_null);

      t1 = TREE_TYPE (t1);
      t2 = TREE_TYPE (t2);
    }

  /* Unless the size of some bound is known to be zero, compare the
     data in the array.  */
  if (!length_zero_p)
    {
      tree type = find_common_type (TREE_TYPE (a1), TREE_TYPE (a2));

      if (type)
	a1 = convert (type, a1), a2 = convert (type, a2);

      result = build_binary_op (TRUTH_ANDIF_EXPR, result_type, result,
				fold_build2 (EQ_EXPR, result_type, a1, a2));

    }

  /* The result is also true if both sizes are zero.  */
  result = build_binary_op (TRUTH_ORIF_EXPR, result_type,
			    build_binary_op (TRUTH_ANDIF_EXPR, result_type,
					     a1_is_null, a2_is_null),
			    result);

  /* If either operand contains SAVE_EXPRs, they have to be evaluated before
     starting the comparison above since the place it would be otherwise
     evaluated would be wrong.  */

  if (contains_save_expr_p (a1))
    result = build2 (COMPOUND_EXPR, result_type, a1, result);

  if (contains_save_expr_p (a2))
    result = build2 (COMPOUND_EXPR, result_type, a2, result);

  return result;
}

/* Compute the result of applying OP_CODE to LHS and RHS, where both are of
   type TYPE.  We know that TYPE is a modular type with a nonbinary
   modulus.  */

static tree
nonbinary_modular_operation (enum tree_code op_code, tree type, tree lhs,
                             tree rhs)
{
  tree modulus = TYPE_MODULUS (type);
  unsigned int needed_precision = tree_floor_log2 (modulus) + 1;
  unsigned int precision;
  bool unsignedp = true;
  tree op_type = type;
  tree result;

  /* If this is an addition of a constant, convert it to a subtraction
     of a constant since we can do that faster.  */
  if (op_code == PLUS_EXPR && TREE_CODE (rhs) == INTEGER_CST)
    {
      rhs = fold_build2 (MINUS_EXPR, type, modulus, rhs);
      op_code = MINUS_EXPR;
    }

  /* For the logical operations, we only need PRECISION bits.  For
     addition and subtraction, we need one more and for multiplication we
     need twice as many.  But we never want to make a size smaller than
     our size. */
  if (op_code == PLUS_EXPR || op_code == MINUS_EXPR)
    needed_precision += 1;
  else if (op_code == MULT_EXPR)
    needed_precision *= 2;

  precision = MAX (needed_precision, TYPE_PRECISION (op_type));

  /* Unsigned will do for everything but subtraction.  */
  if (op_code == MINUS_EXPR)
    unsignedp = false;

  /* If our type is the wrong signedness or isn't wide enough, make a new
     type and convert both our operands to it.  */
  if (TYPE_PRECISION (op_type) < precision
      || TYPE_UNSIGNED (op_type) != unsignedp)
    {
      /* Copy the node so we ensure it can be modified to make it modular.  */
      op_type = copy_node (gnat_type_for_size (precision, unsignedp));
      modulus = convert (op_type, modulus);
      SET_TYPE_MODULUS (op_type, modulus);
      TYPE_MODULAR_P (op_type) = 1;
      lhs = convert (op_type, lhs);
      rhs = convert (op_type, rhs);
    }

  /* Do the operation, then we'll fix it up.  */
  result = fold_build2 (op_code, op_type, lhs, rhs);

  /* For multiplication, we have no choice but to do a full modulus
     operation.  However, we want to do this in the narrowest
     possible size.  */
  if (op_code == MULT_EXPR)
    {
      tree div_type = copy_node (gnat_type_for_size (needed_precision, 1));
      modulus = convert (div_type, modulus);
      SET_TYPE_MODULUS (div_type, modulus);
      TYPE_MODULAR_P (div_type) = 1;
      result = convert (op_type,
			fold_build2 (TRUNC_MOD_EXPR, div_type,
				     convert (div_type, result), modulus));
    }

  /* For subtraction, add the modulus back if we are negative.  */
  else if (op_code == MINUS_EXPR)
    {
      result = save_expr (result);
      result = fold_build3 (COND_EXPR, op_type,
			    fold_build2 (LT_EXPR, integer_type_node, result,
					 convert (op_type, integer_zero_node)),
			    fold_build2 (PLUS_EXPR, op_type, result, modulus),
			    result);
    }

  /* For the other operations, subtract the modulus if we are >= it.  */
  else
    {
      result = save_expr (result);
      result = fold_build3 (COND_EXPR, op_type,
			    fold_build2 (GE_EXPR, integer_type_node,
					 result, modulus),
			    fold_build2 (MINUS_EXPR, op_type,
					 result, modulus),
			    result);
    }

  return convert (type, result);
}

/* Make a binary operation of kind OP_CODE.  RESULT_TYPE is the type
   desired for the result.  Usually the operation is to be performed
   in that type.  For MODIFY_EXPR and ARRAY_REF, RESULT_TYPE may be 0
   in which case the type to be used will be derived from the operands.

   This function is very much unlike the ones for C and C++ since we
   have already done any type conversion and matching required.  All we
   have to do here is validate the work done by SEM and handle subtypes.  */

tree
build_binary_op (enum tree_code op_code, tree result_type,
                 tree left_operand, tree right_operand)
{
  tree left_type  = TREE_TYPE (left_operand);
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

  if (operation_type
      && !AGGREGATE_TYPE_P (operation_type)
      && TYPE_EXTRA_SUBTYPE_P (operation_type))
    operation_type = get_base_type (operation_type);

  modulus = (operation_type
	     && TREE_CODE (operation_type) == INTEGER_TYPE
	     && TYPE_MODULAR_P (operation_type)
	     ? TYPE_MODULUS (operation_type) : NULL_TREE);

  switch (op_code)
    {
    case MODIFY_EXPR:
      /* If there were integral or pointer conversions on the LHS, remove
	 them; we'll be putting them back below if needed.  Likewise for
	 conversions between array and record types, except for justified
	 modular types.  But don't do this if the right operand is not
	 BLKmode (for packed arrays) unless we are not changing the mode.  */
      while ((CONVERT_EXPR_P (left_operand)
	      || TREE_CODE (left_operand) == VIEW_CONVERT_EXPR)
	     && (((INTEGRAL_TYPE_P (left_type)
		   || POINTER_TYPE_P (left_type))
		  && (INTEGRAL_TYPE_P (TREE_TYPE
				       (TREE_OPERAND (left_operand, 0)))
		      || POINTER_TYPE_P (TREE_TYPE
					 (TREE_OPERAND (left_operand, 0)))))
		 || (((TREE_CODE (left_type) == RECORD_TYPE
		       && !TYPE_JUSTIFIED_MODULAR_P (left_type))
		      || TREE_CODE (left_type) == ARRAY_TYPE)
		     && ((TREE_CODE (TREE_TYPE
				     (TREE_OPERAND (left_operand, 0)))
			  == RECORD_TYPE)
			 || (TREE_CODE (TREE_TYPE
					(TREE_OPERAND (left_operand, 0)))
			     == ARRAY_TYPE))
		     && (TYPE_MODE (right_type) == BLKmode
			 || (TYPE_MODE (left_type)
			     == TYPE_MODE (TREE_TYPE
					   (TREE_OPERAND
					    (left_operand, 0))))))))
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
      else if (TREE_CODE (left_type) == RECORD_TYPE
	       && TYPE_IS_PADDING_P (left_type)
	       && TREE_CONSTANT (TYPE_SIZE (left_type))
	       && ((TREE_CODE (right_operand) == COMPONENT_REF
		    && TREE_CODE (TREE_TYPE (TREE_OPERAND (right_operand, 0)))
		       == RECORD_TYPE
		    && TYPE_IS_PADDING_P
		       (TREE_TYPE (TREE_OPERAND (right_operand, 0)))
		    && gnat_types_compatible_p
		       (left_type,
			TREE_TYPE (TREE_OPERAND (right_operand, 0))))
		   || (TREE_CODE (right_operand) == CONSTRUCTOR
		       && !CONTAINS_PLACEHOLDER_P
			   (DECL_SIZE (TYPE_FIELDS (left_type)))))
	       && !integer_zerop (TYPE_SIZE (right_type)))
	operation_type = left_type;

      /* Find the best type to use for copying between aggregate types.  */
      else if (((TREE_CODE (left_type) == ARRAY_TYPE
		 && TREE_CODE (right_type) == ARRAY_TYPE)
		|| (TREE_CODE (left_type) == RECORD_TYPE
		    && TREE_CODE (right_type) == RECORD_TYPE))
	       && (best_type = find_common_type (left_type, right_type)))
	operation_type = best_type;

      /* Otherwise use the LHS type.  */
      else if (!operation_type)
	operation_type = left_type;

      /* Ensure everything on the LHS is valid.  If we have a field reference,
	 strip anything that get_inner_reference can handle.  Then remove any
	 conversions between types having the same code and mode.  And mark
	 VIEW_CONVERT_EXPRs with TREE_ADDRESSABLE.  When done, we must have
	 either an INDIRECT_REF, a NULL_EXPR or a DECL node.  */
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
			     == TREE_CODE (TREE_TYPE
					   (TREE_OPERAND (result, 0))))
			     && (TYPE_MODE (TREE_TYPE
					    (TREE_OPERAND (result, 0)))
				 == TYPE_MODE (restype)))
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

      gcc_assert (TREE_CODE (result) == INDIRECT_REF
		  || TREE_CODE (result) == NULL_EXPR
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

      /* Then convert the right operand to its base type.  This will
	 prevent unneeded signedness conversions when sizetype is wider than
	 integer.  */
      right_operand = convert (right_base_type, right_operand);
      right_operand = convert (TYPE_DOMAIN (left_type), right_operand);

      if (!TREE_CONSTANT (right_operand)
	  || !TREE_CONSTANT (TYPE_MIN_VALUE (right_type)))
	gnat_mark_addressable (left_operand);

      modulus = NULL_TREE;
      break;

    case GE_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case LT_EXPR:
      gcc_assert (!POINTER_TYPE_P (left_type));

      /* ... fall through ... */

    case EQ_EXPR:
    case NE_EXPR:
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
	  result = compare_arrays (result_type, left_operand, right_operand);

	  if (op_code == NE_EXPR)
	    result = invert_truthvalue (result);
	  else
	    gcc_assert (op_code == EQ_EXPR);

	  return result;
	}

      /* Otherwise, the base types must be the same, unless they are both fat
	 pointer types or record types.  In the latter case, use the best type
	 and convert both operands to that type.  */
      if (left_base_type != right_base_type)
	{
	  if (TYPE_FAT_POINTER_P (left_base_type)
	      && TYPE_FAT_POINTER_P (right_base_type))
	    {
	      gcc_assert (TYPE_MAIN_VARIANT (left_base_type)
			  == TYPE_MAIN_VARIANT (right_base_type));
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

      /* If we are comparing a fat pointer against zero, we just need to
	 compare the data pointer.  */
      if (TYPE_FAT_POINTER_P (left_base_type)
	  && TREE_CODE (right_operand) == CONSTRUCTOR
	  && integer_zerop (VEC_index (constructor_elt,
				       CONSTRUCTOR_ELTS (right_operand),
				       0)->value))
	{
	  left_operand
	    = build_component_ref (left_operand, NULL_TREE,
				   TYPE_FIELDS (left_base_type), false);
	  right_operand
	    = convert (TREE_TYPE (left_operand), integer_zero_node);
	}

      modulus = NULL_TREE;
      break;

    case PREINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
      /* These operations are not used anymore.  */
      gcc_unreachable ();

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

    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
      left_operand = gnat_truthvalue_conversion (left_operand);
      right_operand = gnat_truthvalue_conversion (right_operand);
      goto common;

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
    result = fold (build4 (op_code, operation_type, left_operand,
			   right_operand, NULL_TREE, NULL_TREE));
  else
    result
      = fold_build2 (op_code, operation_type, left_operand, right_operand);

  TREE_SIDE_EFFECTS (result) |= has_side_effects;
  TREE_CONSTANT (result)
    |= (TREE_CONSTANT (left_operand) & TREE_CONSTANT (right_operand)
	&& op_code != ARRAY_REF && op_code != ARRAY_RANGE_REF);

  if ((op_code == ARRAY_REF || op_code == ARRAY_RANGE_REF)
      && TYPE_VOLATILE (operation_type))
    TREE_THIS_VOLATILE (result) = 1;

  /* If we are working with modular types, perform the MOD operation
     if something above hasn't eliminated the need for it.  */
  if (modulus)
    result = fold_build2 (FLOOR_MOD_EXPR, operation_type, result,
			  convert (operation_type, modulus));

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
  bool side_effects = false;

  if (operation_type
      && TREE_CODE (operation_type) == RECORD_TYPE
      && TYPE_JUSTIFIED_MODULAR_P (operation_type))
    operation_type = TREE_TYPE (TYPE_FIELDS (operation_type));

  if (operation_type
      && !AGGREGATE_TYPE_P (operation_type)
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
      gcc_assert (result_type == base_type);
      result = invert_truthvalue (gnat_truthvalue_conversion (operand));
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

	case ARRAY_REF:
	case ARRAY_RANGE_REF:
	case COMPONENT_REF:
	case BIT_FIELD_REF:
	    /* If this is for 'Address, find the address of the prefix and
	       add the offset to the field.  Otherwise, do this the normal
	       way.  */
	  if (op_code == ATTR_ADDR_EXPR)
	    {
	      HOST_WIDE_INT bitsize;
	      HOST_WIDE_INT bitpos;
	      tree offset, inner;
	      enum machine_mode mode;
	      int unsignedp, volatilep;

	      inner = get_inner_reference (operand, &bitsize, &bitpos, &offset,
					   &mode, &unsignedp, &volatilep,
					   false);

	      /* If INNER is a padding type whose field has a self-referential
		 size, convert to that inner type.  We know the offset is zero
		 and we need to have that type visible.  */
	      if (TREE_CODE (TREE_TYPE (inner)) == RECORD_TYPE
		  && TYPE_IS_PADDING_P (TREE_TYPE (inner))
		  && (CONTAINS_PLACEHOLDER_P
		      (TYPE_SIZE (TREE_TYPE (TYPE_FIELDS
					     (TREE_TYPE (inner)))))))
		inner = convert (TREE_TYPE (TYPE_FIELDS (TREE_TYPE (inner))),
				 inner);

	      /* Compute the offset as a byte offset from INNER.  */
	      if (!offset)
		offset = size_zero_node;

	      if (bitpos % BITS_PER_UNIT != 0)
		post_error
		  ("taking address of object not aligned on storage unit?",
		   error_gnat_node);

	      offset = size_binop (PLUS_EXPR, offset,
				   size_int (bitpos / BITS_PER_UNIT));

	      /* Take the address of INNER, convert the offset to void *, and
		 add then.  It will later be converted to the desired result
		 type, if any.  */
	      inner = build_unary_op (ADDR_EXPR, NULL_TREE, inner);
	      inner = convert (ptr_void_type_node, inner);
	      result = build_binary_op (POINTER_PLUS_EXPR, ptr_void_type_node,
					inner, offset);
	      result = convert (build_pointer_type (TREE_TYPE (operand)),
				result);
	      break;
	    }
	  goto common;

	case CONSTRUCTOR:
	  /* If this is just a constructor for a padded record, we can
	     just take the address of the single field and convert it to
	     a pointer to our type.  */
	  if (TREE_CODE (type) == RECORD_TYPE && TYPE_IS_PADDING_P (type))
	    {
	      result = (VEC_index (constructor_elt,
				   CONSTRUCTOR_ELTS (operand),
				   0)
			->value);

	      result = convert (build_pointer_type (TREE_TYPE (operand)),
				build_unary_op (ADDR_EXPR, NULL_TREE, result));
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

	  /* If we are taking the address of a padded record whose field is
	     contains a template, take the address of the template.  */
	  if (TREE_CODE (type) == RECORD_TYPE
	      && TYPE_IS_PADDING_P (type)
	      && TREE_CODE (TREE_TYPE (TYPE_FIELDS (type))) == RECORD_TYPE
	      && TYPE_CONTAINS_TEMPLATE_P (TREE_TYPE (TYPE_FIELDS (type))))
	    {
	      type = TREE_TYPE (TYPE_FIELDS (type));
	      operand = convert (type, operand);
	    }

	  if (type != error_mark_node)
	    operation_type = build_pointer_type (type);

	  gnat_mark_addressable (operand);
	  result = fold_build1 (ADDR_EXPR, operation_type, operand);
	}

      TREE_CONSTANT (result) = staticp (operand) || TREE_CONSTANT (operand);
      break;

    case INDIRECT_REF:
      /* If we want to refer to an entire unconstrained array,
	 make up an expression to do so.  This will never survive to
	 the backend.  If TYPE is a thin pointer, first convert the
	 operand to a fat pointer.  */
      if (TYPE_THIN_POINTER_P (type)
	  && TYPE_UNCONSTRAINED_ARRAY (TREE_TYPE (type)))
	{
	  operand
	    = convert (TREE_TYPE (TYPE_UNCONSTRAINED_ARRAY (TREE_TYPE (type))),
		       operand);
	  type = TREE_TYPE (operand);
	}

      if (TYPE_FAT_POINTER_P (type))
	{
	  result = build1 (UNCONSTRAINED_ARRAY_REF,
			   TYPE_UNCONSTRAINED_ARRAY (type), operand);
	  TREE_READONLY (result) = TREE_STATIC (result)
	    = TYPE_READONLY (TYPE_UNCONSTRAINED_ARRAY (type));
	}
      else if (TREE_CODE (operand) == ADDR_EXPR)
	result = TREE_OPERAND (operand, 0);

      else
	{
	  result = fold_build1 (op_code, TREE_TYPE (type), operand);
	  TREE_READONLY (result) = TYPE_READONLY (TREE_TYPE (type));
	}

      side_effects
	=  (!TYPE_FAT_POINTER_P (type) && TYPE_VOLATILE (TREE_TYPE (type)));
      break;

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
						convert (operation_type,
							 integer_one_node))))
		  result = fold_build2 (BIT_XOR_EXPR, operation_type,
					operand, modulus);
		else
		  result = fold_build2 (MINUS_EXPR, operation_type,
					modulus, operand);

		result = fold_build3 (COND_EXPR, operation_type,
				      fold_build2 (NE_EXPR,
						   integer_type_node,
						   operand,
						   convert
						     (operation_type,
						      integer_zero_node)),
				      result, operand);
	      }
	    else
	      {
		/* For the NOT cases, we need a constant equal to
		   the modulus minus one.  For a binary modulus, we
		   XOR against the constant and subtract the operand from
		   that constant for nonbinary modulus.  */

		tree cnst = fold_build2 (MINUS_EXPR, operation_type, modulus,
					 convert (operation_type,
						  integer_one_node));

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

  if (side_effects)
    {
      TREE_SIDE_EFFECTS (result) = 1;
      if (TREE_CODE (result) == INDIRECT_REF)
	TREE_THIS_VOLATILE (result) = TYPE_VOLATILE (TREE_TYPE (result));
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
  tree result;
  bool addr_p = false;

  /* The front-end verifies that result, true and false operands have same base
     type.  Convert everything to the result type.  */

  true_operand  = convert (result_type, true_operand);
  false_operand = convert (result_type, false_operand);

  /* If the result type is unconstrained, take the address of
     the operands and then dereference our result.  */
  if (TREE_CODE (result_type) == UNCONSTRAINED_ARRAY_TYPE
      || CONTAINS_PLACEHOLDER_P (TYPE_SIZE (result_type)))
    {
      addr_p = true;
      result_type = build_pointer_type (result_type);
      true_operand = build_unary_op (ADDR_EXPR, result_type, true_operand);
      false_operand = build_unary_op (ADDR_EXPR, result_type, false_operand);
    }

  result = fold_build3 (COND_EXPR, result_type, condition_operand,
			true_operand, false_operand);

  /* If either operand is a SAVE_EXPR (possibly surrounded by
     arithmetic, make sure it gets done.  */
  true_operand  = skip_simple_arithmetic (true_operand);
  false_operand = skip_simple_arithmetic (false_operand);

  if (TREE_CODE (true_operand) == SAVE_EXPR)
    result = build2 (COMPOUND_EXPR, result_type, true_operand, result);

  if (TREE_CODE (false_operand) == SAVE_EXPR)
    result = build2 (COMPOUND_EXPR, result_type, false_operand, result);

  /* ??? Seems the code above is wrong, as it may move ahead of the COND
     SAVE_EXPRs with side effects and not shared by both arms.  */

 if (addr_p)
    result = build_unary_op (INDIRECT_REF, NULL_TREE, result);

  return result;
}

/* Similar, but for RETURN_EXPR.  If RESULT_DECL is non-zero, build
   a RETURN_EXPR around the assignment of RET_VAL to RESULT_DECL.
   If RESULT_DECL is zero, build a bare RETURN_EXPR.  */

tree
build_return_expr (tree result_decl, tree ret_val)
{
  tree result_expr;

  if (result_decl)
    {
      /* The gimplifier explicitly enforces the following invariant:

           RETURN_EXPR
               |
           MODIFY_EXPR
           /        \
          /          \
      RESULT_DECL    ...

      As a consequence, type-homogeneity dictates that we use the type
      of the RESULT_DECL as the operation type.  */

      tree operation_type = TREE_TYPE (result_decl);

      /* Convert the right operand to the operation type.  Note that
         it's the same transformation as in the MODIFY_EXPR case of
         build_binary_op with the additional guarantee that the type
         cannot involve a placeholder, since otherwise the function
         would use the "target pointer" return mechanism.  */

      if (operation_type != TREE_TYPE (ret_val))
	ret_val = convert (operation_type, ret_val);

      result_expr
	= build2 (MODIFY_EXPR, operation_type, result_decl, ret_val);
    }
  else
    result_expr = NULL_TREE;

  return build1 (RETURN_EXPR, void_type_node, result_expr);
}

/* Build a CALL_EXPR to call FUNDECL with one argument, ARG.  Return
   the CALL_EXPR.  */

tree
build_call_1_expr (tree fundecl, tree arg)
{
  tree call = build_call_nary (TREE_TYPE (TREE_TYPE (fundecl)),
			       build_unary_op (ADDR_EXPR, NULL_TREE, fundecl),
			       1, arg);
  TREE_SIDE_EFFECTS (call) = 1;
  return call;
}

/* Build a CALL_EXPR to call FUNDECL with two arguments, ARG1 & ARG2.  Return
   the CALL_EXPR.  */

tree
build_call_2_expr (tree fundecl, tree arg1, tree arg2)
{
  tree call = build_call_nary (TREE_TYPE (TREE_TYPE (fundecl)),
			       build_unary_op (ADDR_EXPR, NULL_TREE, fundecl),
			       2, arg1, arg2);
  TREE_SIDE_EFFECTS (call) = 1;
  return call;
}

/* Likewise to call FUNDECL with no arguments.  */

tree
build_call_0_expr (tree fundecl)
{
  /* We rely on build_call_nary to compute TREE_SIDE_EFFECTS.  This makes
     it possible to propagate DECL_IS_PURE on parameterless functions.  */
  tree call = build_call_nary (TREE_TYPE (TREE_TYPE (fundecl)),
			       build_unary_op (ADDR_EXPR, NULL_TREE, fundecl),
			       0);
  return call;
}

/* Call a function that raises an exception and pass the line number and file
   name, if requested.  MSG says which exception function to call.

   GNAT_NODE is the gnat node conveying the source location for which the
   error should be signaled, or Empty in which case the error is signaled on
   the current ref_file_name/input_line.

   KIND says which kind of exception this is for
   (N_Raise_{Constraint,Storage,Program}_Error).  */

tree
build_call_raise (int msg, Node_Id gnat_node, char kind)
{
  tree fndecl = gnat_raise_decls[msg];
  tree label = get_exception_label (kind);
  tree filename;
  int line_number;
  const char *str;
  int len;

  /* If this is to be done as a goto, handle that case.  */
  if (label)
    {
      Entity_Id local_raise = Get_Local_Raise_Call_Entity ();
      tree gnu_result = build1 (GOTO_EXPR, void_type_node, label);

      /* If Local_Raise is present, generate
	 Local_Raise (exception'Identity);  */
      if (Present (local_raise))
	{
	  tree gnu_local_raise
	    = gnat_to_gnu_entity (local_raise, NULL_TREE, 0);
	  tree gnu_exception_entity
	    = gnat_to_gnu_entity (Get_RT_Exception_Entity (msg), NULL_TREE, 0);
	  tree gnu_call
	    = build_call_1_expr (gnu_local_raise,
				 build_unary_op (ADDR_EXPR, NULL_TREE,
						 gnu_exception_entity));

	  gnu_result = build2 (COMPOUND_EXPR, void_type_node,
			       gnu_call, gnu_result);}

      return gnu_result;
    }

  str
    = (Debug_Flag_NN || Exception_Locations_Suppressed)
      ? ""
      : (gnat_node != Empty && Sloc (gnat_node) != No_Location)
        ? IDENTIFIER_POINTER
          (get_identifier (Get_Name_String
			   (Debug_Source_Name
			    (Get_Source_File_Index (Sloc (gnat_node))))))
        : ref_filename;

  len = strlen (str) + 1;
  filename = build_string (len, str);
  line_number
    = (gnat_node != Empty && Sloc (gnat_node) != No_Location)
      ? Get_Logical_Line_Number (Sloc(gnat_node)) : input_line;

  TREE_TYPE (filename)
    = build_array_type (char_type_node,
			build_index_type (build_int_cst (NULL_TREE, len)));

  return
    build_call_2_expr (fndecl,
		       build1 (ADDR_EXPR, build_pointer_type (char_type_node),
			       filename),
		       build_int_cst (NULL_TREE, line_number));
}

/* qsort comparer for the bit positions of two constructor elements
   for record components.  */

static int
compare_elmt_bitpos (const PTR rt1, const PTR rt2)
{
  const_tree const elmt1 = * (const_tree const *) rt1;
  const_tree const elmt2 = * (const_tree const *) rt2;
  const_tree const field1 = TREE_PURPOSE (elmt1);
  const_tree const field2 = TREE_PURPOSE (elmt2);
  const int ret
    = tree_int_cst_compare (bit_position (field1), bit_position (field2));

  return ret ? ret : (int) (DECL_UID (field1) - DECL_UID (field2));
}

/* Return a CONSTRUCTOR of TYPE whose list is LIST.  */

tree
gnat_build_constructor (tree type, tree list)
{
  tree elmt;
  int n_elmts;
  bool allconstant = (TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST);
  bool side_effects = false;
  tree result;

  /* Scan the elements to see if they are all constant or if any has side
     effects, to let us set global flags on the resulting constructor.  Count
     the elements along the way for possible sorting purposes below.  */
  for (n_elmts = 0, elmt = list; elmt; elmt = TREE_CHAIN (elmt), n_elmts ++)
    {
      if (!TREE_CONSTANT (TREE_VALUE (elmt))
	  || (TREE_CODE (type) == RECORD_TYPE
	      && DECL_BIT_FIELD (TREE_PURPOSE (elmt))
	      && TREE_CODE (TREE_VALUE (elmt)) != INTEGER_CST)
	  || !initializer_constant_valid_p (TREE_VALUE (elmt),
					    TREE_TYPE (TREE_VALUE (elmt))))
	allconstant = false;

      if (TREE_SIDE_EFFECTS (TREE_VALUE (elmt)))
	side_effects = true;

      /* Propagate an NULL_EXPR from the size of the type.  We won't ever
	 be executing the code we generate here in that case, but handle it
	 specially to avoid the compiler blowing up.  */
      if (TREE_CODE (type) == RECORD_TYPE
	  && (0 != (result
		    = contains_null_expr (DECL_SIZE (TREE_PURPOSE (elmt))))))
	return build1 (NULL_EXPR, type, TREE_OPERAND (result, 0));
    }

  /* For record types with constant components only, sort field list
     by increasing bit position.  This is necessary to ensure the
     constructor can be output as static data.  */
  if (allconstant && TREE_CODE (type) == RECORD_TYPE && n_elmts > 1)
    {
      /* Fill an array with an element tree per index, and ask qsort to order
	 them according to what a bitpos comparison function says.  */
      tree *gnu_arr = (tree *) alloca (sizeof (tree) * n_elmts);
      int i;

      for (i = 0, elmt = list; elmt; elmt = TREE_CHAIN (elmt), i++)
	gnu_arr[i] = elmt;

      qsort (gnu_arr, n_elmts, sizeof (tree), compare_elmt_bitpos);

      /* Then reconstruct the list from the sorted array contents.  */
      list = NULL_TREE;
      for (i = n_elmts - 1; i >= 0; i--)
	{
	  TREE_CHAIN (gnu_arr[i]) = list;
	  list = gnu_arr[i];
	}
    }

  result = build_constructor_from_list (type, list);
  TREE_CONSTANT (result) = TREE_STATIC (result) = allconstant;
  TREE_SIDE_EFFECTS (result) = side_effects;
  TREE_READONLY (result) = TYPE_READONLY (type) || allconstant;
  return result;
}

/* Return a COMPONENT_REF to access a field that is given by COMPONENT,
   an IDENTIFIER_NODE giving the name of the field, or FIELD, a FIELD_DECL,
   for the field.  Don't fold the result if NO_FOLD_P is true.

   We also handle the fact that we might have been passed a pointer to the
   actual record and know how to look for fields in variant parts.  */

static tree
build_simple_component_ref (tree record_variable, tree component,
                            tree field, bool no_fold_p)
{
  tree record_type = TYPE_MAIN_VARIANT (TREE_TYPE (record_variable));
  tree ref, inner_variable;

  gcc_assert ((TREE_CODE (record_type) == RECORD_TYPE
	       || TREE_CODE (record_type) == UNION_TYPE
	       || TREE_CODE (record_type) == QUAL_UNION_TYPE)
	      && TYPE_SIZE (record_type)
	      && (component != 0) != (field != 0));

  /* If no field was specified, look for a field with the specified name
     in the current record only.  */
  if (!field)
    for (field = TYPE_FIELDS (record_type); field;
	 field = TREE_CHAIN (field))
      if (DECL_NAME (field) == component)
	break;

  if (!field)
    return NULL_TREE;

  /* If this field is not in the specified record, see if we can find
     something in the record whose original field is the same as this one. */
  if (DECL_CONTEXT (field) != record_type)
    /* Check if there is a field with name COMPONENT in the record.  */
    {
      tree new_field;

      /* First loop thru normal components.  */

      for (new_field = TYPE_FIELDS (record_type); new_field;
	   new_field = TREE_CHAIN (new_field))
	if (field == new_field
	    || DECL_ORIGINAL_FIELD (new_field) == field
	    || new_field == DECL_ORIGINAL_FIELD (field)
	    || (DECL_ORIGINAL_FIELD (field)
		&& (DECL_ORIGINAL_FIELD (field)
		    == DECL_ORIGINAL_FIELD (new_field))))
	  break;

      /* Next, loop thru DECL_INTERNAL_P components if we haven't found
         the component in the first search. Doing this search in 2 steps
         is required to avoiding hidden homonymous fields in the
         _Parent field.  */

      if (!new_field)
	for (new_field = TYPE_FIELDS (record_type); new_field;
	     new_field = TREE_CHAIN (new_field))
	  if (DECL_INTERNAL_P (new_field))
	    {
	      tree field_ref
		= build_simple_component_ref (record_variable,
					      NULL_TREE, new_field, no_fold_p);
	      ref = build_simple_component_ref (field_ref, NULL_TREE, field,
						no_fold_p);

	      if (ref)
		return ref;
	    }

      field = new_field;
    }

  if (!field)
    return NULL_TREE;

  /* If the field's offset has overflowed, do not attempt to access it
     as doing so may trigger sanity checks deeper in the back-end.
     Note that we don't need to warn since this will be done on trying
     to declare the object.  */
  if (TREE_CODE (DECL_FIELD_OFFSET (field)) == INTEGER_CST
      && TREE_OVERFLOW (DECL_FIELD_OFFSET (field)))
    return NULL_TREE;

  /* Look through conversion between type variants.  Note that this
     is transparent as far as the field is concerned.  */
  if (TREE_CODE (record_variable) == VIEW_CONVERT_EXPR
      && TYPE_MAIN_VARIANT (TREE_TYPE (TREE_OPERAND (record_variable, 0)))
	 == record_type)
    inner_variable = TREE_OPERAND (record_variable, 0);
  else
    inner_variable = record_variable;

  ref = build3 (COMPONENT_REF, TREE_TYPE (field), inner_variable, field,
		NULL_TREE);

  if (TREE_READONLY (record_variable) || TREE_READONLY (field))
    TREE_READONLY (ref) = 1;
  if (TREE_THIS_VOLATILE (record_variable) || TREE_THIS_VOLATILE (field)
      || TYPE_VOLATILE (record_type))
    TREE_THIS_VOLATILE (ref) = 1;

  if (no_fold_p)
    return ref;

  /* The generic folder may punt in this case because the inner array type
     can be self-referential, but folding is in fact not problematic.  */
  else if (TREE_CODE (record_variable) == CONSTRUCTOR
	   && TYPE_CONTAINS_TEMPLATE_P (TREE_TYPE (record_variable)))
    {
      VEC(constructor_elt,gc) *elts = CONSTRUCTOR_ELTS (record_variable);
      unsigned HOST_WIDE_INT idx;
      tree index, value;
      FOR_EACH_CONSTRUCTOR_ELT (elts, idx, index, value)
	if (index == field)
	  return value;
      return ref;
    }

  else
    return fold (ref);
}

/* Like build_simple_component_ref, except that we give an error if the
   reference could not be found.  */

tree
build_component_ref (tree record_variable, tree component,
                     tree field, bool no_fold_p)
{
  tree ref = build_simple_component_ref (record_variable, component, field,
					 no_fold_p);

  if (ref)
    return ref;

  /* If FIELD was specified, assume this is an invalid user field so
     raise constraint error.  Otherwise, we can't find the type to return, so
     abort.  */
  gcc_assert (field);
  return build1 (NULL_EXPR, TREE_TYPE (field),
		 build_call_raise (CE_Discriminant_Check_Failed, Empty,
				   N_Raise_Constraint_Error));
}

/* Build a GCC tree to call an allocation or deallocation function.
   If GNU_OBJ is nonzero, it is an object to deallocate.  Otherwise,
   generate an allocator.

   GNU_SIZE is the size of the object in bytes and ALIGN is the alignment in
   bits.  GNAT_PROC, if present, is a procedure to call and GNAT_POOL is the
   storage pool to use.  If not preset, malloc and free will be used except
   if GNAT_PROC is the "fake" value of -1, in which case we allocate the
   object dynamically on the stack frame.  */

tree
build_call_alloc_dealloc (tree gnu_obj, tree gnu_size, unsigned align,
                          Entity_Id gnat_proc, Entity_Id gnat_pool,
                          Node_Id gnat_node)
{
  tree gnu_align = size_int (align / BITS_PER_UNIT);

  gnu_size = SUBSTITUTE_PLACEHOLDER_IN_EXPR (gnu_size, gnu_obj);

  if (Present (gnat_proc))
    {
      /* The storage pools are obviously always tagged types, but the
	 secondary stack uses the same mechanism and is not tagged */
      if (Is_Tagged_Type (Etype (gnat_pool)))
	{
	  /* The size is the third parameter; the alignment is the
             same type.  */
	  Entity_Id gnat_size_type
	    = Etype (Next_Formal (Next_Formal (First_Formal (gnat_proc))));
	  tree gnu_size_type = gnat_to_gnu_type (gnat_size_type);
	  tree gnu_proc = gnat_to_gnu (gnat_proc);
	  tree gnu_proc_addr = build_unary_op (ADDR_EXPR, NULL_TREE, gnu_proc);
	  tree gnu_pool = gnat_to_gnu (gnat_pool);
	  tree gnu_pool_addr = build_unary_op (ADDR_EXPR, NULL_TREE, gnu_pool);
	  tree gnu_call;

	  gnu_size = convert (gnu_size_type, gnu_size);
	  gnu_align = convert (gnu_size_type, gnu_align);

	  /* The first arg is always the address of the storage pool; next
	     comes the address of the object, for a deallocator, then the
	     size and alignment.  */
	  if (gnu_obj)
	    gnu_call = build_call_nary (TREE_TYPE (TREE_TYPE (gnu_proc)),
					gnu_proc_addr, 4, gnu_pool_addr,
					gnu_obj, gnu_size, gnu_align);
	  else
	    gnu_call = build_call_nary (TREE_TYPE (TREE_TYPE (gnu_proc)),
					gnu_proc_addr, 3, gnu_pool_addr,
					gnu_size, gnu_align);
	  TREE_SIDE_EFFECTS (gnu_call) = 1;
	  return gnu_call;
	}

      /* Secondary stack case.  */
      else
	{
	  /* The size is the second parameter */
	  Entity_Id gnat_size_type
	    = Etype (Next_Formal (First_Formal (gnat_proc)));
	  tree gnu_size_type = gnat_to_gnu_type (gnat_size_type);
	  tree gnu_proc = gnat_to_gnu (gnat_proc);
	  tree gnu_proc_addr = build_unary_op (ADDR_EXPR, NULL_TREE, gnu_proc);
	  tree gnu_call;

	  gnu_size = convert (gnu_size_type, gnu_size);

	  /* The first arg is the address of the object, for a
	     deallocator, then the size */
	  if (gnu_obj)
	    gnu_call = build_call_nary (TREE_TYPE (TREE_TYPE (gnu_proc)),
					gnu_proc_addr, 2, gnu_obj, gnu_size);
	  else
	    gnu_call = build_call_nary (TREE_TYPE (TREE_TYPE (gnu_proc)),
					gnu_proc_addr, 1, gnu_size);
	  TREE_SIDE_EFFECTS (gnu_call) = 1;
	  return gnu_call;
	}
    }

  else if (gnu_obj)
    return build_call_1_expr (free_decl, gnu_obj);

  /* ??? For now, disable variable-sized allocators in the stack since
     we can't yet gimplify an ALLOCATE_EXPR.  */
  else if (gnat_pool == -1
	   && TREE_CODE (gnu_size) == INTEGER_CST
	   && flag_stack_check != GENERIC_STACK_CHECK)
    {
      /* If the size is a constant, we can put it in the fixed portion of
	 the stack frame to avoid the need to adjust the stack pointer.  */
	{
	  tree gnu_range
	    = build_range_type (NULL_TREE, size_one_node, gnu_size);
	  tree gnu_array_type = build_array_type (char_type_node, gnu_range);
	  tree gnu_decl
	    = create_var_decl (get_identifier ("RETVAL"), NULL_TREE,
			       gnu_array_type, NULL_TREE, false, false, false,
			       false, NULL, gnat_node);

	  return convert (ptr_void_type_node,
			  build_unary_op (ADDR_EXPR, NULL_TREE, gnu_decl));
	}
#if 0
      else
	return build2 (ALLOCATE_EXPR, ptr_void_type_node, gnu_size, gnu_align);
#endif
    }
  else
    {
      if (Nkind (gnat_node) != N_Allocator || !Comes_From_Source (gnat_node))
        Check_No_Implicit_Heap_Alloc (gnat_node);

      /* If the allocator size is 32bits but the pointer size is 64bits then
	 allocate 32bit memory (sometimes necessary on 64bit VMS). Otherwise
	 default to standard malloc. */
      if (TARGET_ABI_OPEN_VMS &&
          (!TARGET_MALLOC64 ||
           (POINTER_SIZE == 64
	    && (UI_To_Int (Esize (Etype (gnat_node))) == 32
	        || Convention (Etype (gnat_node)) == Convention_C))))
        return build_call_1_expr (malloc32_decl, gnu_size);
      else
        return build_call_1_expr (malloc_decl, gnu_size);
    }
}

/* Build a GCC tree to correspond to allocating an object of TYPE whose
   initial value is INIT, if INIT is nonzero.  Convert the expression to
   RESULT_TYPE, which must be some type of pointer.  Return the tree.
   GNAT_PROC and GNAT_POOL optionally give the procedure to call and
   the storage pool to use.  GNAT_NODE is used to provide an error
   location for restriction violations messages.  If IGNORE_INIT_TYPE is
   true, ignore the type of INIT for the purpose of determining the size;
   this will cause the maximum size to be allocated if TYPE is of
   self-referential size.  */

tree
build_allocator (tree type, tree init, tree result_type, Entity_Id gnat_proc,
                 Entity_Id gnat_pool, Node_Id gnat_node, bool ignore_init_type)
{
  tree size = TYPE_SIZE_UNIT (type);
  tree result;
  unsigned int default_allocator_alignment
    = get_target_default_allocator_alignment () * BITS_PER_UNIT;

  /* If the initializer, if present, is a NULL_EXPR, just return a new one.  */
  if (init && TREE_CODE (init) == NULL_EXPR)
    return build1 (NULL_EXPR, result_type, TREE_OPERAND (init, 0));

  /* If RESULT_TYPE is a fat or thin pointer, set SIZE to be the sum of the
     sizes of the object and its template.  Allocate the whole thing and
     fill in the parts that are known.  */
  else if (TYPE_FAT_OR_THIN_POINTER_P (result_type))
    {
      tree storage_type
	= build_unc_object_type_from_ptr (result_type, type,
					  get_identifier ("ALLOC"));
      tree template_type = TREE_TYPE (TYPE_FIELDS (storage_type));
      tree storage_ptr_type = build_pointer_type (storage_type);
      tree storage;
      tree template_cons = NULL_TREE;

      size = SUBSTITUTE_PLACEHOLDER_IN_EXPR (TYPE_SIZE_UNIT (storage_type),
					     init);

      /* If the size overflows, pass -1 so the allocator will raise
	 storage error.  */
      if (TREE_CODE (size) == INTEGER_CST && TREE_OVERFLOW (size))
	size = ssize_int (-1);

      storage = build_call_alloc_dealloc (NULL_TREE, size,
					  TYPE_ALIGN (storage_type),
					  gnat_proc, gnat_pool, gnat_node);
      storage = convert (storage_ptr_type, protect_multiple_eval (storage));

      if (TREE_CODE (type) == RECORD_TYPE && TYPE_IS_PADDING_P (type))
	{
	  type = TREE_TYPE (TYPE_FIELDS (type));

	  if (init)
	    init = convert (type, init);
	}

      /* If there is an initializing expression, make a constructor for
	 the entire object including the bounds and copy it into the
	 object.  If there is no initializing expression, just set the
	 bounds.  */
      if (init)
	{
	  template_cons = tree_cons (TREE_CHAIN (TYPE_FIELDS (storage_type)),
				     init, NULL_TREE);
	  template_cons = tree_cons (TYPE_FIELDS (storage_type),
				     build_template (template_type, type,
						     init),
				     template_cons);

	  return convert
	    (result_type,
	     build2 (COMPOUND_EXPR, storage_ptr_type,
		     build_binary_op
		     (MODIFY_EXPR, storage_type,
		      build_unary_op (INDIRECT_REF, NULL_TREE,
				      convert (storage_ptr_type, storage)),
		      gnat_build_constructor (storage_type, template_cons)),
		     convert (storage_ptr_type, storage)));
	}
      else
	return build2
	  (COMPOUND_EXPR, result_type,
	   build_binary_op
	   (MODIFY_EXPR, template_type,
	    build_component_ref
	    (build_unary_op (INDIRECT_REF, NULL_TREE,
			     convert (storage_ptr_type, storage)),
	     NULL_TREE, TYPE_FIELDS (storage_type), 0),
	    build_template (template_type, type, NULL_TREE)),
	   convert (result_type, convert (storage_ptr_type, storage)));
    }

  /* If we have an initializing expression, see if its size is simpler
     than the size from the type.  */
  if (!ignore_init_type && init && TYPE_SIZE_UNIT (TREE_TYPE (init))
      && (TREE_CODE (TYPE_SIZE_UNIT (TREE_TYPE (init))) == INTEGER_CST
	  || CONTAINS_PLACEHOLDER_P (size)))
    size = TYPE_SIZE_UNIT (TREE_TYPE (init));

  /* If the size is still self-referential, reference the initializing
     expression, if it is present.  If not, this must have been a
     call to allocate a library-level object, in which case we use
     the maximum size.  */
  if (CONTAINS_PLACEHOLDER_P (size))
    {
      if (!ignore_init_type && init)
	size = substitute_placeholder_in_expr (size, init);
      else
	size = max_size (size, true);
    }

  /* If the size overflows, pass -1 so the allocator will raise
     storage error.  */
  if (TREE_CODE (size) == INTEGER_CST && TREE_OVERFLOW (size))
    size = ssize_int (-1);

  /* If this is in the default storage pool and the type alignment is larger
     than what the default allocator supports, make an "aligning" record type
     with room to store a pointer before the field, allocate an object of that
     type, store the system's allocator return value just in front of the
     field and return the field's address.  */

  if (No (gnat_proc) && TYPE_ALIGN (type) > default_allocator_alignment)
    {
      /* Construct the aligning type with enough room for a pointer ahead
	 of the field, then allocate.  */
      tree record_type
	= make_aligning_type (type, TYPE_ALIGN (type), size,
			      default_allocator_alignment,
			      POINTER_SIZE / BITS_PER_UNIT);

      tree record, record_addr;

      record_addr
	= build_call_alloc_dealloc (NULL_TREE, TYPE_SIZE_UNIT (record_type),
				    default_allocator_alignment, Empty, Empty,
				    gnat_node);

      record_addr
	= convert (build_pointer_type (record_type),
		   save_expr (record_addr));

      record = build_unary_op (INDIRECT_REF, NULL_TREE, record_addr);

      /* Our RESULT (the Ada allocator's value) is the super-aligned address
	 of the internal record field ... */
      result
	= build_unary_op (ADDR_EXPR, NULL_TREE,
			  build_component_ref
			  (record, NULL_TREE, TYPE_FIELDS (record_type), 0));
      result = convert (result_type, result);

      /* ... with the system allocator's return value stored just in
	 front.  */
      {
	tree ptr_addr
	  = build_binary_op (POINTER_PLUS_EXPR, ptr_void_type_node,
			     convert (ptr_void_type_node, result),
			     size_int (-POINTER_SIZE/BITS_PER_UNIT));

	tree ptr_ref
	  = convert (build_pointer_type (ptr_void_type_node), ptr_addr);

	result
	  = build2 (COMPOUND_EXPR, TREE_TYPE (result),
		    build_binary_op (MODIFY_EXPR, NULL_TREE,
				     build_unary_op (INDIRECT_REF, NULL_TREE,
						     ptr_ref),
				     convert (ptr_void_type_node,
					      record_addr)),
		    result);
      }
    }
  else
    result = convert (result_type,
		      build_call_alloc_dealloc (NULL_TREE, size,
						TYPE_ALIGN (type),
						gnat_proc,
						gnat_pool,
						gnat_node));

  /* If we have an initial value, put the new address into a SAVE_EXPR, assign
     the value, and return the address.  Do this with a COMPOUND_EXPR.  */

  if (init)
    {
      result = save_expr (result);
      result
	= build2 (COMPOUND_EXPR, TREE_TYPE (result),
		  build_binary_op
		  (MODIFY_EXPR, NULL_TREE,
		   build_unary_op (INDIRECT_REF,
				   TREE_TYPE (TREE_TYPE (result)), result),
		   init),
		  result);
    }

  return convert (result_type, result);
}

/* Fill in a VMS descriptor for EXPR and return a constructor for it.
   GNAT_FORMAL is how we find the descriptor record.  GNAT_ACTUAL is
   how we derive the source location to raise C_E on an out of range
   pointer. */

tree
fill_vms_descriptor (tree expr, Entity_Id gnat_formal, Node_Id gnat_actual)
{
  tree field;
  tree parm_decl = get_gnu_tree (gnat_formal);
  tree const_list = NULL_TREE;
  tree record_type = TREE_TYPE (TREE_TYPE (parm_decl));
  int do_range_check =
      strcmp ("MBO",
	      IDENTIFIER_POINTER (DECL_NAME (TYPE_FIELDS (record_type))));

  expr = maybe_unconstrained_array (expr);
  gnat_mark_addressable (expr);

  for (field = TYPE_FIELDS (record_type); field; field = TREE_CHAIN (field))
    {
      tree conexpr = convert (TREE_TYPE (field),
			      SUBSTITUTE_PLACEHOLDER_IN_EXPR
			      (DECL_INITIAL (field), expr));

      /* Check to ensure that only 32bit pointers are passed in
	 32bit descriptors */
      if (do_range_check &&
          strcmp (IDENTIFIER_POINTER (DECL_NAME (field)), "POINTER") == 0)
        {
	  tree pointer64type =
	     build_pointer_type_for_mode (void_type_node, DImode, false);
	  tree addr64expr = build_unary_op (ADDR_EXPR, pointer64type, expr);
	  tree malloc64low =
	     build_int_cstu (long_integer_type_node, 0x80000000);

	  add_stmt (build3 (COND_EXPR, void_type_node,
			    build_binary_op (GE_EXPR, long_integer_type_node,
					     convert (long_integer_type_node,
						      addr64expr),
					     malloc64low),
			    build_call_raise (CE_Range_Check_Failed, gnat_actual,
					      N_Raise_Constraint_Error),
			    NULL_TREE));
        }
      const_list = tree_cons (field, conexpr, const_list);
    }

  return gnat_build_constructor (record_type, nreverse (const_list));
}

/* Indicate that we need to make the address of EXPR_NODE and it therefore
   should not be allocated in a register.  Returns true if successful.  */

bool
gnat_mark_addressable (tree expr_node)
{
  while (1)
    switch (TREE_CODE (expr_node))
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
	expr_node = TREE_OPERAND (expr_node, 0);
	break;

      case CONSTRUCTOR:
	TREE_ADDRESSABLE (expr_node) = 1;
	return true;

      case VAR_DECL:
      case PARM_DECL:
      case RESULT_DECL:
	TREE_ADDRESSABLE (expr_node) = 1;
	return true;

      case FUNCTION_DECL:
	TREE_ADDRESSABLE (expr_node) = 1;
	return true;

      case CONST_DECL:
	return (DECL_CONST_CORRESPONDING_VAR (expr_node)
		&& (gnat_mark_addressable
		    (DECL_CONST_CORRESPONDING_VAR (expr_node))));
      default:
	return true;
    }
}
