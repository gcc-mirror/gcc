/* Utility routines for data type conversion for GNU C.
   Copyright (C) 1987, 1988, 1991, 1992, 1993, 1994, 1995, 1997,
   1998 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */


/* These routines are somewhat language-independent utility function
   intended to be called by the language-specific convert () functions.  */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "flags.h"
#include "convert.h"
#include "toplev.h"
#include "langhooks.h"

/* Convert EXPR to some pointer or reference type TYPE.

   EXPR must be pointer, reference, integer, enumeral, or literal zero;
   in other cases error is called.  */

tree
convert_to_pointer (type, expr)
     tree type, expr;
{
  if (integer_zerop (expr))
    {
      expr = build_int_2 (0, 0);
      TREE_TYPE (expr) = type;
      return expr;
    }

  switch (TREE_CODE (TREE_TYPE (expr)))
    {
    case POINTER_TYPE:
    case REFERENCE_TYPE:
      return build1 (NOP_EXPR, type, expr);

    case INTEGER_TYPE:
    case ENUMERAL_TYPE:
    case BOOLEAN_TYPE:
    case CHAR_TYPE:
      if (TYPE_PRECISION (TREE_TYPE (expr)) == POINTER_SIZE)
	return build1 (CONVERT_EXPR, type, expr);

      return
	convert_to_pointer (type,
			    convert ((*lang_hooks.types.type_for_size)
				     (POINTER_SIZE, 0), expr));

    default:
      error ("cannot convert to a pointer type");
      return convert_to_pointer (type, integer_zero_node);
    }
}

/* Convert EXPR to some floating-point type TYPE.

   EXPR must be float, integer, or enumeral;
   in other cases error is called.  */

tree
convert_to_real (type, expr)
     tree type, expr;
{
  switch (TREE_CODE (TREE_TYPE (expr)))
    {
    case REAL_TYPE:
      return build1 (flag_float_store ? CONVERT_EXPR : NOP_EXPR,
		     type, expr);

    case INTEGER_TYPE:
    case ENUMERAL_TYPE:
    case BOOLEAN_TYPE:
    case CHAR_TYPE:
      return build1 (FLOAT_EXPR, type, expr);

    case COMPLEX_TYPE:
      return convert (type,
		      fold (build1 (REALPART_EXPR,
				    TREE_TYPE (TREE_TYPE (expr)), expr)));

    case POINTER_TYPE:
    case REFERENCE_TYPE:
      error ("pointer value used where a floating point value was expected");
      return convert_to_real (type, integer_zero_node);

    default:
      error ("aggregate value used where a float was expected");
      return convert_to_real (type, integer_zero_node);
    }
}

/* Convert EXPR to some integer (or enum) type TYPE.

   EXPR must be pointer, integer, discrete (enum, char, or bool), float, or
   vector; in other cases error is called.

   The result of this is always supposed to be a newly created tree node
   not in use in any existing structure.  */

tree
convert_to_integer (type, expr)
     tree type, expr;
{
  enum tree_code ex_form = TREE_CODE (expr);
  tree intype = TREE_TYPE (expr);
  unsigned int inprec = TYPE_PRECISION (intype);
  unsigned int outprec = TYPE_PRECISION (type);

  /* An INTEGER_TYPE cannot be incomplete, but an ENUMERAL_TYPE can
     be.  Consider `enum E = { a, b = (enum E) 3 };'.  */
  if (!COMPLETE_TYPE_P (type))
    {
      error ("conversion to incomplete type");
      return error_mark_node;
    }

  switch (TREE_CODE (intype))
    {
    case POINTER_TYPE:
    case REFERENCE_TYPE:
      if (integer_zerop (expr))
	expr = integer_zero_node;
      else
	expr = fold (build1 (CONVERT_EXPR, (*lang_hooks.types.type_for_size)
			     (POINTER_SIZE, 0), expr));

      return convert_to_integer (type, expr);

    case INTEGER_TYPE:
    case ENUMERAL_TYPE:
    case BOOLEAN_TYPE:
    case CHAR_TYPE:
      /* If this is a logical operation, which just returns 0 or 1, we can
	 change the type of the expression.  For some logical operations,
	 we must also change the types of the operands to maintain type
	 correctness.  */

      if (TREE_CODE_CLASS (ex_form) == '<')
	{
	  TREE_TYPE (expr) = type;
	  return expr;
	}

      else if (ex_form == TRUTH_AND_EXPR || ex_form == TRUTH_ANDIF_EXPR
	       || ex_form == TRUTH_OR_EXPR || ex_form == TRUTH_ORIF_EXPR
	       || ex_form == TRUTH_XOR_EXPR)
	{
	  TREE_OPERAND (expr, 0) = convert (type, TREE_OPERAND (expr, 0));
	  TREE_OPERAND (expr, 1) = convert (type, TREE_OPERAND (expr, 1));
	  TREE_TYPE (expr) = type;
	  return expr;
	}

      else if (ex_form == TRUTH_NOT_EXPR)
	{
	  TREE_OPERAND (expr, 0) = convert (type, TREE_OPERAND (expr, 0));
	  TREE_TYPE (expr) = type;
	  return expr;
	}

      /* If we are widening the type, put in an explicit conversion.
	 Similarly if we are not changing the width.  After this, we know
	 we are truncating EXPR.  */

      else if (outprec >= inprec)
	{
	  enum tree_code code;

	  /* If the precision of the EXPR's type is K bits and the
	     destination mode has more bits, and the sign is changing,
	     it is not safe to use a NOP_EXPR.  For example, suppose
	     that EXPR's type is a 3-bit unsigned integer type, the
	     TYPE is a 3-bit signed integer type, and the machine mode
	     for the types is 8-bit QImode.  In that case, the
	     conversion necessitates an explicit sign-extension.  In
	     the signed-to-unsigned case the high-order bits have to
	     be cleared.  */
	  if (TREE_UNSIGNED (type) != TREE_UNSIGNED (TREE_TYPE (expr))
	      && (TYPE_PRECISION (TREE_TYPE (expr))
		  != GET_MODE_BITSIZE (TYPE_MODE (TREE_TYPE (expr)))))
	    code = CONVERT_EXPR;
	  else
	    code = NOP_EXPR;

	  return build1 (code, type, expr);
	}

      /* If TYPE is an enumeral type or a type with a precision less
	 than the number of bits in its mode, do the conversion to the
	 type corresponding to its mode, then do a nop conversion
	 to TYPE.  */
      else if (TREE_CODE (type) == ENUMERAL_TYPE
	       || outprec != GET_MODE_BITSIZE (TYPE_MODE (type)))
	return build1 (NOP_EXPR, type,
		       convert ((*lang_hooks.types.type_for_mode)
				(TYPE_MODE (type), TREE_UNSIGNED (type)),
				expr));

      /* Here detect when we can distribute the truncation down past some
	 arithmetic.  For example, if adding two longs and converting to an
	 int, we can equally well convert both to ints and then add.
	 For the operations handled here, such truncation distribution
	 is always safe.
	 It is desirable in these cases:
	 1) when truncating down to full-word from a larger size
	 2) when truncating takes no work.
	 3) when at least one operand of the arithmetic has been extended
	 (as by C's default conversions).  In this case we need two conversions
	 if we do the arithmetic as already requested, so we might as well
	 truncate both and then combine.  Perhaps that way we need only one.

	 Note that in general we cannot do the arithmetic in a type
	 shorter than the desired result of conversion, even if the operands
	 are both extended from a shorter type, because they might overflow
	 if combined in that type.  The exceptions to this--the times when
	 two narrow values can be combined in their narrow type even to
	 make a wider result--are handled by "shorten" in build_binary_op.  */

      switch (ex_form)
	{
	case RSHIFT_EXPR:
	  /* We can pass truncation down through right shifting
	     when the shift count is a nonpositive constant.  */
	  if (TREE_CODE (TREE_OPERAND (expr, 1)) == INTEGER_CST
	      && tree_int_cst_lt (TREE_OPERAND (expr, 1),
				  convert (TREE_TYPE (TREE_OPERAND (expr, 1)),
					   integer_one_node)))
	    goto trunc1;
	  break;

	case LSHIFT_EXPR:
	  /* We can pass truncation down through left shifting
	     when the shift count is a nonnegative constant and
	     the target type is unsigned.  */
	  if (TREE_CODE (TREE_OPERAND (expr, 1)) == INTEGER_CST
	      && tree_int_cst_sgn (TREE_OPERAND (expr, 1)) >= 0
	      && TREE_UNSIGNED (type)
	      && TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST)
	    {
	      /* If shift count is less than the width of the truncated type,
		 really shift.  */
	      if (tree_int_cst_lt (TREE_OPERAND (expr, 1), TYPE_SIZE (type)))
		/* In this case, shifting is like multiplication.  */
		goto trunc1;
	      else
		{
		  /* If it is >= that width, result is zero.
		     Handling this with trunc1 would give the wrong result:
		     (int) ((long long) a << 32) is well defined (as 0)
		     but (int) a << 32 is undefined and would get a
		     warning.  */

		  tree t = convert_to_integer (type, integer_zero_node);

		  /* If the original expression had side-effects, we must
		     preserve it.  */
		  if (TREE_SIDE_EFFECTS (expr))
		    return build (COMPOUND_EXPR, type, expr, t);
		  else
		    return t;
		}
	    }
	  break;

	case MAX_EXPR:
	case MIN_EXPR:
	case MULT_EXPR:
	  {
	    tree arg0 = get_unwidened (TREE_OPERAND (expr, 0), type);
	    tree arg1 = get_unwidened (TREE_OPERAND (expr, 1), type);

	    /* Don't distribute unless the output precision is at least as big
	       as the actual inputs.  Otherwise, the comparison of the
	       truncated values will be wrong.  */
	    if (outprec >= TYPE_PRECISION (TREE_TYPE (arg0))
		&& outprec >= TYPE_PRECISION (TREE_TYPE (arg1))
		/* If signedness of arg0 and arg1 don't match,
		   we can't necessarily find a type to compare them in.  */
		&& (TREE_UNSIGNED (TREE_TYPE (arg0))
		    == TREE_UNSIGNED (TREE_TYPE (arg1))))
	      goto trunc1;
	    break;
	  }

	case PLUS_EXPR:
	case MINUS_EXPR:
	case BIT_AND_EXPR:
	case BIT_IOR_EXPR:
	case BIT_XOR_EXPR:
	case BIT_ANDTC_EXPR:
	trunc1:
	  {
	    tree arg0 = get_unwidened (TREE_OPERAND (expr, 0), type);
	    tree arg1 = get_unwidened (TREE_OPERAND (expr, 1), type);

	    if (outprec >= BITS_PER_WORD
		|| TRULY_NOOP_TRUNCATION (outprec, inprec)
		|| inprec > TYPE_PRECISION (TREE_TYPE (arg0))
		|| inprec > TYPE_PRECISION (TREE_TYPE (arg1)))
	      {
		/* Do the arithmetic in type TYPEX,
		   then convert result to TYPE.  */
		tree typex = type;

		/* Can't do arithmetic in enumeral types
		   so use an integer type that will hold the values.  */
		if (TREE_CODE (typex) == ENUMERAL_TYPE)
		  typex = (*lang_hooks.types.type_for_size)
		    (TYPE_PRECISION (typex), TREE_UNSIGNED (typex));

		/* But now perhaps TYPEX is as wide as INPREC.
		   In that case, do nothing special here.
		   (Otherwise would recurse infinitely in convert.  */
		if (TYPE_PRECISION (typex) != inprec)
		  {
		    /* Don't do unsigned arithmetic where signed was wanted,
		       or vice versa.
		       Exception: if both of the original operands were
 		       unsigned then we can safely do the work as unsigned.
		       Exception: shift operations take their type solely
		       from the first argument.
		       Exception: the LSHIFT_EXPR case above requires that
		       we perform this operation unsigned lest we produce
		       signed-overflow undefinedness.
		       And we may need to do it as unsigned
		       if we truncate to the original size.  */
		    if (TREE_UNSIGNED (TREE_TYPE (expr))
			|| (TREE_UNSIGNED (TREE_TYPE (arg0))
			    && (TREE_UNSIGNED (TREE_TYPE (arg1))
				|| ex_form == LSHIFT_EXPR
				|| ex_form == RSHIFT_EXPR
				|| ex_form == LROTATE_EXPR
				|| ex_form == RROTATE_EXPR))
			|| ex_form == LSHIFT_EXPR)
		      typex = (*lang_hooks.types.unsigned_type) (typex);
		    else
		      typex = (*lang_hooks.types.signed_type) (typex);
		    return convert (type,
				    fold (build (ex_form, typex,
						 convert (typex, arg0),
						 convert (typex, arg1),
						 0)));
		  }
	      }
	  }
	  break;

	case NEGATE_EXPR:
	case BIT_NOT_EXPR:
	  /* This is not correct for ABS_EXPR,
	     since we must test the sign before truncation.  */
	  {
	    tree typex = type;

	    /* Can't do arithmetic in enumeral types
	       so use an integer type that will hold the values.  */
	    if (TREE_CODE (typex) == ENUMERAL_TYPE)
	      typex = (*lang_hooks.types.type_for_size)
		(TYPE_PRECISION (typex), TREE_UNSIGNED (typex));

	    /* But now perhaps TYPEX is as wide as INPREC.
	       In that case, do nothing special here.
	       (Otherwise would recurse infinitely in convert.  */
	    if (TYPE_PRECISION (typex) != inprec)
	      {
		/* Don't do unsigned arithmetic where signed was wanted,
		   or vice versa.  */
		if (TREE_UNSIGNED (TREE_TYPE (expr)))
		  typex = (*lang_hooks.types.unsigned_type) (typex);
		else
		  typex = (*lang_hooks.types.signed_type) (typex);
		return convert (type,
				fold (build1 (ex_form, typex,
					      convert (typex,
						       TREE_OPERAND (expr, 0)))));
	      }
	  }

	case NOP_EXPR:
	  /* Don't introduce a
	     "can't convert between vector values of different size" error.  */
	  if (TREE_CODE (TREE_TYPE (TREE_OPERAND (expr, 0))) == VECTOR_TYPE
	      && (GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (TREE_OPERAND (expr, 0))))
		  != GET_MODE_SIZE (TYPE_MODE (type))))
	    break;
	  /* If truncating after truncating, might as well do all at once.
	     If truncating after extending, we may get rid of wasted work.  */
	  return convert (type, get_unwidened (TREE_OPERAND (expr, 0), type));

	case COND_EXPR:
	  /* It is sometimes worthwhile to push the narrowing down through
	     the conditional and never loses.  */
	  return fold (build (COND_EXPR, type, TREE_OPERAND (expr, 0),
			      convert (type, TREE_OPERAND (expr, 1)), 
			      convert (type, TREE_OPERAND (expr, 2))));

	default:
	  break;
	}

      return build1 (NOP_EXPR, type, expr);

    case REAL_TYPE:
      return build1 (FIX_TRUNC_EXPR, type, expr);

    case COMPLEX_TYPE:
      return convert (type,
		      fold (build1 (REALPART_EXPR,
				    TREE_TYPE (TREE_TYPE (expr)), expr)));

    case VECTOR_TYPE:
      if (GET_MODE_SIZE (TYPE_MODE (type))
	  != GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (expr))))
	{
	  error ("can't convert between vector values of different size");
	  return error_mark_node;
	}
      return build1 (NOP_EXPR, type, expr);

    default:
      error ("aggregate value used where an integer was expected");
      return convert (type, integer_zero_node);
    }
}

/* Convert EXPR to the complex type TYPE in the usual ways.  */

tree
convert_to_complex (type, expr)
     tree type, expr;
{
  tree subtype = TREE_TYPE (type);
  
  switch (TREE_CODE (TREE_TYPE (expr)))
    {
    case REAL_TYPE:
    case INTEGER_TYPE:
    case ENUMERAL_TYPE:
    case BOOLEAN_TYPE:
    case CHAR_TYPE:
      return build (COMPLEX_EXPR, type, convert (subtype, expr),
		    convert (subtype, integer_zero_node));

    case COMPLEX_TYPE:
      {
	tree elt_type = TREE_TYPE (TREE_TYPE (expr));

	if (TYPE_MAIN_VARIANT (elt_type) == TYPE_MAIN_VARIANT (subtype))
	  return expr;
	else if (TREE_CODE (expr) == COMPLEX_EXPR)
	  return fold (build (COMPLEX_EXPR,
			      type,
			      convert (subtype, TREE_OPERAND (expr, 0)),
			      convert (subtype, TREE_OPERAND (expr, 1))));
	else
	  {
	    expr = save_expr (expr);
	    return
	      fold (build (COMPLEX_EXPR,
			   type, convert (subtype,
					  fold (build1 (REALPART_EXPR,
							TREE_TYPE (TREE_TYPE (expr)),
							expr))),
			   convert (subtype,
				    fold (build1 (IMAGPART_EXPR,
						  TREE_TYPE (TREE_TYPE (expr)),
						  expr)))));
	  }
      }

    case POINTER_TYPE:
    case REFERENCE_TYPE:
      error ("pointer value used where a complex was expected");
      return convert_to_complex (type, integer_zero_node);

    default:
      error ("aggregate value used where a complex was expected");
      return convert_to_complex (type, integer_zero_node);
    }
}

/* Convert EXPR to the vector type TYPE in the usual ways.  */

tree
convert_to_vector (type, expr)
     tree type, expr;
{
  switch (TREE_CODE (TREE_TYPE (expr)))
    {
    case INTEGER_TYPE:
    case VECTOR_TYPE:
      if (GET_MODE_SIZE (TYPE_MODE (type))
	  != GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (expr))))
	{
	  error ("can't convert between vector values of different size");
	  return error_mark_node;
	}
      return build1 (NOP_EXPR, type, expr);

    default:
      error ("can't convert value to a vector");
      return convert_to_vector (type, integer_zero_node);
    }
}
