/* Utility routines for data type conversion for GNU C.
   Copyright (C) 1987, 88, 91, 92, 94, 1995 Free Software Foundation, Inc.

This file is part of GNU C.

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


/* These routines are somewhat language-independent utility function
   intended to be called by the language-specific convert () functions. */

#include "config.h"
#include "tree.h"
#include "flags.h"
#include "convert.h"

/* Convert EXPR to some pointer or reference type TYPE.

   EXPR must be pointer, reference, integer, enumeral, or literal zero;
   in other cases error is called. */

tree
convert_to_pointer (type, expr)
     tree type, expr;
{
  register tree intype = TREE_TYPE (expr);
  register enum tree_code form = TREE_CODE (intype);
  
  if (integer_zerop (expr))
    {
      expr = build_int_2 (0, 0);
      TREE_TYPE (expr) = type;
      return expr;
    }

  if (form == POINTER_TYPE || form == REFERENCE_TYPE)
    return build1 (NOP_EXPR, type, expr);


  if (form == INTEGER_TYPE || form == ENUMERAL_TYPE)
    {
      if (type_precision (intype) == POINTER_SIZE)
	return build1 (CONVERT_EXPR, type, expr);
      expr = convert (type_for_size (POINTER_SIZE, 0), expr);
      /* Modes may be different but sizes should be the same.  */
      if (GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (expr)))
	  != GET_MODE_SIZE (TYPE_MODE (type)))
	/* There is supposed to be some integral type
	   that is the same width as a pointer.  */
	abort ();
      return convert_to_pointer (type, expr);
    }

  error ("cannot convert to a pointer type");

  expr = build_int_2 (0, 0);
  TREE_TYPE (expr) = type;
  return expr;
}

/* Convert EXPR to some floating-point type TYPE.

   EXPR must be float, integer, or enumeral;
   in other cases error is called. */

tree
convert_to_real (type, expr)
     tree type, expr;
{
  register enum tree_code form = TREE_CODE (TREE_TYPE (expr));

  if (form == REAL_TYPE)
    return build1 (flag_float_store ? CONVERT_EXPR : NOP_EXPR,
		   type, expr);

  if (INTEGRAL_TYPE_P (TREE_TYPE (expr)))
    return build1 (FLOAT_EXPR, type, expr);

  if (form == COMPLEX_TYPE)
    return convert (type, fold (build1 (REALPART_EXPR,
					TREE_TYPE (TREE_TYPE (expr)), expr)));

  if (form == POINTER_TYPE || form == REFERENCE_TYPE)
    error ("pointer value used where a floating point value was expected");
  else
    error ("aggregate value used where a float was expected");

  {
    register tree tem = make_node (REAL_CST);
    TREE_TYPE (tem) = type;
    TREE_REAL_CST (tem) = REAL_VALUE_ATOF ("0.0", TYPE_MODE (type));
    return tem;
  }
}

/* Convert EXPR to some integer (or enum) type TYPE.

   EXPR must be pointer, integer, discrete (enum, char, or bool), or float;
   in other cases error is called.

   The result of this is always supposed to be a newly created tree node
   not in use in any existing structure.  */

tree
convert_to_integer (type, expr)
     tree type, expr;
{
  register tree intype = TREE_TYPE (expr);
  register enum tree_code form = TREE_CODE (intype);

  if (form == POINTER_TYPE || form == REFERENCE_TYPE)
    {
      if (integer_zerop (expr))
	expr = integer_zero_node;
      else
	expr = fold (build1 (CONVERT_EXPR,
			     type_for_size (POINTER_SIZE, 0), expr));
      intype = TREE_TYPE (expr);
      form = TREE_CODE (intype);
      if (intype == type)
	return expr;
    }

  if (form == INTEGER_TYPE || form == ENUMERAL_TYPE
      || form == BOOLEAN_TYPE || form == CHAR_TYPE)
    {
      register unsigned outprec = TYPE_PRECISION (type);
      register unsigned inprec = TYPE_PRECISION (intype);
      register enum tree_code ex_form = TREE_CODE (expr);

      /* If we are widening the type, put in an explicit conversion.
	 Similarly if we are not changing the width.  However, if this is
	 a logical operation that just returns 0 or 1, we can change the
	 type of the expression.  For logical operations, we must
	 also change the types of the operands to maintain type
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
      else if (outprec >= inprec)
	return build1 (NOP_EXPR, type, expr);

      /* If TYPE is an enumeral type or a type with a precision less
	 than the number of bits in its mode, do the conversion to the
	 type corresponding to its mode, then do a nop conversion
	 to TYPE.  */
      else if (TREE_CODE (type) == ENUMERAL_TYPE
	       || outprec != GET_MODE_BITSIZE (TYPE_MODE (type)))
	return build1 (NOP_EXPR, type,
		       convert (type_for_mode (TYPE_MODE (type),
					       TREE_UNSIGNED (type)),
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
	     when the shift count is a nonnegative constant.  */
	  if (TREE_CODE (TREE_OPERAND (expr, 1)) == INTEGER_CST
	      && tree_int_cst_sgn (TREE_OPERAND (expr, 1)) >= 0
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
		register tree typex = type;

		/* Can't do arithmetic in enumeral types
		   so use an integer type that will hold the values.  */
		if (TREE_CODE (typex) == ENUMERAL_TYPE)
		  typex = type_for_size (TYPE_PRECISION (typex),
					 TREE_UNSIGNED (typex));

		/* But now perhaps TYPEX is as wide as INPREC.
		   In that case, do nothing special here.
		   (Otherwise would recurse infinitely in convert.  */
		if (TYPE_PRECISION (typex) != inprec)
		  {
		    /* Don't do unsigned arithmetic where signed was wanted,
		       or vice versa.
		       Exception: if either of the original operands were
		       unsigned then can safely do the work as unsigned.
		       And we may need to do it as unsigned
		       if we truncate to the original size.  */
		    typex = ((TREE_UNSIGNED (TREE_TYPE (expr))
			      || TREE_UNSIGNED (TREE_TYPE (arg0))
			      || TREE_UNSIGNED (TREE_TYPE (arg1)))
			     ? unsigned_type (typex) : signed_type (typex));
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
	    register tree typex = type;

	    /* Can't do arithmetic in enumeral types
	       so use an integer type that will hold the values.  */
	    if (TREE_CODE (typex) == ENUMERAL_TYPE)
	      typex = type_for_size (TYPE_PRECISION (typex),
				     TREE_UNSIGNED (typex));

	    /* But now perhaps TYPEX is as wide as INPREC.
	       In that case, do nothing special here.
	       (Otherwise would recurse infinitely in convert.  */
	    if (TYPE_PRECISION (typex) != inprec)
	      {
		/* Don't do unsigned arithmetic where signed was wanted,
		   or vice versa.  */
		typex = (TREE_UNSIGNED (TREE_TYPE (expr))
			 ? unsigned_type (typex) : signed_type (typex));
		return convert (type,
				fold (build1 (ex_form, typex,
					      convert (typex,
						       TREE_OPERAND (expr, 0)))));
	      }
	  }

	case NOP_EXPR:
	  /* If truncating after truncating, might as well do all at once.
	     If truncating after extending, we may get rid of wasted work.  */
	  return convert (type, get_unwidened (TREE_OPERAND (expr, 0), type));

	case COND_EXPR:
	  /* Can treat the two alternative values like the operands
	     of an arithmetic expression.  */
	  {
	    tree arg1 = get_unwidened (TREE_OPERAND (expr, 1), type);
	    tree arg2 = get_unwidened (TREE_OPERAND (expr, 2), type);

	    if (outprec >= BITS_PER_WORD
		|| TRULY_NOOP_TRUNCATION (outprec, inprec)
		|| inprec > TYPE_PRECISION (TREE_TYPE (arg1))
		|| inprec > TYPE_PRECISION (TREE_TYPE (arg2)))
	      {
		/* Do the arithmetic in type TYPEX,
		   then convert result to TYPE.  */
		register tree typex = type;

		/* Can't do arithmetic in enumeral types
		   so use an integer type that will hold the values.  */
		if (TREE_CODE (typex) == ENUMERAL_TYPE)
		  typex = type_for_size (TYPE_PRECISION (typex),
					 TREE_UNSIGNED (typex));

		/* But now perhaps TYPEX is as wide as INPREC.
		   In that case, do nothing special here.
		   (Otherwise would recurse infinitely in convert.  */
		if (TYPE_PRECISION (typex) != inprec)
		  {
		    /* Don't do unsigned arithmetic where signed was wanted,
		       or vice versa.  */
		    typex = (TREE_UNSIGNED (TREE_TYPE (expr))
			     ? unsigned_type (typex) : signed_type (typex));
		    return convert (type,
				    fold (build (COND_EXPR, typex,
						 TREE_OPERAND (expr, 0),
						 convert (typex, arg1),
						 convert (typex, arg2))));
		  }
		else
		  /* It is sometimes worthwhile
		     to push the narrowing down through the conditional.  */
		  return fold (build (COND_EXPR, type,
				      TREE_OPERAND (expr, 0),
				      convert (type, TREE_OPERAND (expr, 1)), 
				      convert (type, TREE_OPERAND (expr, 2))));
	      }
	  }

	}

      return build1 (NOP_EXPR, type, expr);
    }

  if (form == REAL_TYPE)
    return build1 (FIX_TRUNC_EXPR, type, expr);

  if (form == COMPLEX_TYPE)
    return convert (type, fold (build1 (REALPART_EXPR,
					TREE_TYPE (TREE_TYPE (expr)), expr)));

  error ("aggregate value used where an integer was expected");

  {
    register tree tem = build_int_2 (0, 0);
    TREE_TYPE (tem) = type;
    return tem;
  }
}

/* Convert EXPR to the complex type TYPE in the usual ways.  */

tree
convert_to_complex (type, expr)
     tree type, expr;
{
  register enum tree_code form = TREE_CODE (TREE_TYPE (expr));
  tree subtype = TREE_TYPE (type);
  
  if (form == REAL_TYPE || form == INTEGER_TYPE || form == ENUMERAL_TYPE)
    {
      expr = convert (subtype, expr);
      return build (COMPLEX_EXPR, type, expr,
		    convert (subtype, integer_zero_node));
    }

  if (form == COMPLEX_TYPE)
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
	  return fold (build (COMPLEX_EXPR,
			      type,
			      convert (subtype,
				       fold (build1 (REALPART_EXPR,
						     TREE_TYPE (TREE_TYPE (expr)),
						     expr))),
			      convert (subtype,
				       fold (build1 (IMAGPART_EXPR,
						     TREE_TYPE (TREE_TYPE (expr)),
						     expr)))));
	}
    }

  if (form == POINTER_TYPE || form == REFERENCE_TYPE)
    error ("pointer value used where a complex was expected");
  else
    error ("aggregate value used where a complex was expected");
  
  return build (COMPLEX_EXPR, type,
		convert (subtype, integer_zero_node),
		convert (subtype, integer_zero_node));
}
