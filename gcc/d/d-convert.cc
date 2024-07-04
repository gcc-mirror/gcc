/* d-convert.cc -- Data type conversion routines.
   Copyright (C) 2006-2024 Free Software Foundation, Inc.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"

#include "dmd/aggregate.h"
#include "dmd/declaration.h"
#include "dmd/expression.h"
#include "dmd/mtype.h"

#include "tree.h"
#include "fold-const.h"
#include "diagnostic.h"
#include "langhooks.h"
#include "target.h"
#include "convert.h"
#include "stor-layout.h"

#include "d-tree.h"


/* Build CODE expression with operands OP0 and OP1.
   Helper function for d_truthvalue_conversion, so assumes bool result.  */

static tree
d_build_truthvalue_op (tree_code code, tree op0, tree op1)
{
  tree type0, type1;

  tree result_type = NULL_TREE;

  type0 = TREE_TYPE (op0);
  type1 = TREE_TYPE (op1);

  /* Strip NON_LVALUE_EXPRs, etc., since we aren't using as an lvalue.  */
  STRIP_TYPE_NOPS (op0);
  STRIP_TYPE_NOPS (op1);

  /* Also need to convert pointer/int comparison.  */
  if (POINTER_TYPE_P (type0) && TREE_CODE (op1) == INTEGER_CST
      && integer_zerop (op1))
    {
      result_type = type0;
    }
  else if (POINTER_TYPE_P (type1) && TREE_CODE (op0) == INTEGER_CST
	   && integer_zerop (op0))
    {
      result_type = type1;
    }
  /* If integral, need to convert unsigned/signed comparison.
     Will also need to convert if type precisions differ.  */
  else if (INTEGRAL_TYPE_P (type0) && INTEGRAL_TYPE_P (type1))
    {
      if (TYPE_PRECISION (type0) > TYPE_PRECISION (type1))
	result_type = type0;
      else if (TYPE_PRECISION (type0) < TYPE_PRECISION (type1))
	result_type = type1;
      else if (TYPE_UNSIGNED (type0) != TYPE_UNSIGNED (type1))
	result_type = TYPE_UNSIGNED (type0) ? type0 : type1;
    }

  if (result_type)
    {
      if (TREE_TYPE (op0) != result_type)
	op0 = convert (result_type, op0);
      if (TREE_TYPE (op1) != result_type)
	op1 = convert (result_type, op1);
    }

  return fold_build2 (code, d_bool_type, op0, op1);
}

/* Return whether EXPR is a declaration whose address can never be NULL.  */

bool
decl_with_nonnull_addr_p (const_tree expr)
{
  return (DECL_P (expr)
	  && (TREE_CODE (expr) == PARM_DECL
	      || TREE_CODE (expr) == LABEL_DECL
	      || !DECL_WEAK (expr)));
}

/* Convert EXPR to be a truth-value, validating its type for this purpose.  */

tree
d_truthvalue_conversion (tree expr)
{
  switch (TREE_CODE (expr))
    {
    case EQ_EXPR:   case NE_EXPR:   case LE_EXPR:
    case GE_EXPR:   case LT_EXPR:   case GT_EXPR:
      if (TREE_TYPE (expr) == d_bool_type)
	return expr;
      return build2 (TREE_CODE (expr), d_bool_type,
		     TREE_OPERAND (expr, 0), TREE_OPERAND (expr, 1));

    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
      if (TREE_TYPE (expr) == d_bool_type)
	return expr;
      return build2 (TREE_CODE (expr), d_bool_type,
		     d_truthvalue_conversion (TREE_OPERAND (expr, 0)),
		     d_truthvalue_conversion (TREE_OPERAND (expr, 1)));

    case TRUTH_NOT_EXPR:
      if (TREE_TYPE (expr) == d_bool_type)
	return expr;
      return build1 (TREE_CODE (expr), d_bool_type,
		     d_truthvalue_conversion (TREE_OPERAND (expr, 0)));

    case ERROR_MARK:
      return expr;

    case INTEGER_CST:
      return integer_zerop (expr) ? d_bool_false_node
				  : d_bool_true_node;

    case REAL_CST:
      return real_compare (NE_EXPR, &TREE_REAL_CST (expr), &dconst0)
	     ? d_bool_true_node
	     : d_bool_false_node;

    case ADDR_EXPR:
      /* If we are taking the address of a decl that can never be null,
	 then the return result is always true.  */
      if (decl_with_nonnull_addr_p (TREE_OPERAND (expr, 0)))
	{
	  warning (OPT_Waddress,
		   "the address of %qD will always evaluate as %<true%>",
		   TREE_OPERAND (expr, 0));
	  return d_bool_true_node;
	}
      break;

    case COMPLEX_EXPR:
      return d_build_truthvalue_op ((TREE_SIDE_EFFECTS (TREE_OPERAND (expr, 1))
				     ? TRUTH_OR_EXPR : TRUTH_ORIF_EXPR),
			d_truthvalue_conversion (TREE_OPERAND (expr, 0)),
			d_truthvalue_conversion (TREE_OPERAND (expr, 1)));

    case NEGATE_EXPR:
    case ABS_EXPR:
    case FLOAT_EXPR:
      /* These don't change whether an object is nonzero or zero.  */
      return d_truthvalue_conversion (TREE_OPERAND (expr, 0));

    case LROTATE_EXPR:
    case RROTATE_EXPR:
      /* These don't change whether an object is zero or nonzero, but
	 we can't ignore them if their second arg has side-effects.  */
      if (TREE_SIDE_EFFECTS (TREE_OPERAND (expr, 1)))
	{
	  return build2 (COMPOUND_EXPR, d_bool_type, TREE_OPERAND (expr, 1),
			 d_truthvalue_conversion (TREE_OPERAND (expr, 0)));
	}
      else
	return d_truthvalue_conversion (TREE_OPERAND (expr, 0));

    case COND_EXPR:
      /* Distribute the conversion into the arms of a COND_EXPR.  */
      return fold_build3 (COND_EXPR, d_bool_type, TREE_OPERAND (expr, 0),
			  d_truthvalue_conversion (TREE_OPERAND (expr, 1)),
			  d_truthvalue_conversion (TREE_OPERAND (expr, 2)));

    case CONVERT_EXPR:
      /* Don't cancel the effect of a CONVERT_EXPR from a REFERENCE_TYPE,
	 since that affects how `default_conversion' will behave.  */
      if (TREE_CODE (TREE_TYPE (expr)) == REFERENCE_TYPE
	  || TREE_CODE (TREE_TYPE (TREE_OPERAND (expr, 0))) == REFERENCE_TYPE)
	break;
      /* Fall through.  */

    case NOP_EXPR:
      /* If this isn't narrowing the argument, we can ignore it.  */
      if (TYPE_PRECISION (TREE_TYPE (expr))
	  >= TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (expr, 0))))
	return d_truthvalue_conversion (TREE_OPERAND (expr, 0));
      break;

    default:
      break;
    }

  if (TREE_CODE (TREE_TYPE (expr)) == COMPLEX_TYPE)
    {
      tree t = save_expr (expr);
      return d_build_truthvalue_op ((TREE_SIDE_EFFECTS (expr)
				     ? TRUTH_OR_EXPR : TRUTH_ORIF_EXPR),
			d_truthvalue_conversion (real_part (t)),
			d_truthvalue_conversion (imaginary_part (t)));
    }
  else
    return d_build_truthvalue_op (NE_EXPR, expr,
				  build_zero_cst (TREE_TYPE (expr)));
}


/* Creates an expression whose value is that of EXPR, converted to type TYPE.
   This function implements all reasonable scalar conversions.  */

tree
convert (tree type, tree expr)
{
  tree e = expr;
  tree_code code = TREE_CODE (type);

  if (type == error_mark_node
      || expr == error_mark_node
      || TREE_TYPE (expr) == error_mark_node)
    return error_mark_node;

  const char *invalid_conv_diag
    = targetm.invalid_conversion (TREE_TYPE (expr), type);

  if (invalid_conv_diag)
    {
      error ("%s", invalid_conv_diag);
      return error_mark_node;
    }

  if (type == TREE_TYPE (expr))
    return expr;

  if (TREE_CODE (type) == ARRAY_TYPE
      && TREE_CODE (TREE_TYPE (expr)) == ARRAY_TYPE
      && TYPE_DOMAIN (type) == TYPE_DOMAIN (TREE_TYPE (expr)))
    return expr;

  tree ret = targetm.convert_to_type (type, expr);
  if (ret)
    return ret;

  STRIP_TYPE_NOPS (e);
  tree etype = TREE_TYPE (e);

  if (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (TREE_TYPE (expr)))
    return fold_convert (type, expr);
  if (TREE_CODE (TREE_TYPE (expr)) == ERROR_MARK)
    return error_mark_node;
  if (VOID_TYPE_P (TREE_TYPE (expr)))
    {
      error ("void value not ignored as it ought to be");
      return error_mark_node;
    }

  switch (code)
    {
    case VOID_TYPE:
      return fold_convert (type, e);

    case INTEGER_TYPE:
    case ENUMERAL_TYPE:
      if (POINTER_TYPE_P (etype))
	{
	  if (integer_zerop (e))
	    return build_int_cst (type, 0);

	  /* Convert to an unsigned integer of the correct width first, and
	     from there widen/truncate to the required type.  */
	  tree utype = lang_hooks.types.type_for_size (TYPE_PRECISION (etype),
						       1);
	  ret = fold_build1 (CONVERT_EXPR, utype, e);
	  return fold_convert (type, ret);
	}

      return fold (convert_to_integer (type, e));

    case BOOLEAN_TYPE:
      return fold_convert (type, d_truthvalue_conversion (expr));

    case POINTER_TYPE:
    case REFERENCE_TYPE:
      return fold (convert_to_pointer (type, e));

    case REAL_TYPE:
      if (TREE_CODE (etype) == COMPLEX_TYPE && TYPE_IMAGINARY_FLOAT (type))
	e = build1 (IMAGPART_EXPR, TREE_TYPE (etype), e);

      return fold (convert_to_real (type, e));

    case COMPLEX_TYPE:
      if (SCALAR_FLOAT_TYPE_P (etype) && TYPE_IMAGINARY_FLOAT (etype))
	return fold_build2 (COMPLEX_EXPR, type,
			    build_zero_cst (TREE_TYPE (type)),
			    convert (TREE_TYPE (type), expr));

      return fold (convert_to_complex (type, e));

    case VECTOR_TYPE:
      return fold (convert_to_vector (type, e));

    case RECORD_TYPE:
    case UNION_TYPE:
      if (lang_hooks.types_compatible_p (type, TREE_TYPE (expr)))
	return fold_build1 (VIEW_CONVERT_EXPR, type, expr);
      break;

    default:
      break;
    }

  error ("conversion to non-scalar type requested");
  return error_mark_node;
}

/* Return expression EXP, whose type has been converted to TYPE.  */

tree
d_convert (tree type, tree exp)
{
  /* Check this first before retrieving frontend type.  */
  if (error_operand_p (type) || error_operand_p (exp))
    return error_mark_node;

  Type *totype = TYPE_LANG_FRONTEND (type);
  Type *etype = TYPE_LANG_FRONTEND (TREE_TYPE (exp));

  if (totype && etype)
    return convert_expr (exp, etype, totype);

  return convert (type, exp);
}

/* Return expression EXP, whose type has been convert from ETYPE to TOTYPE.  */

tree
convert_expr (tree exp, Type *etype, Type *totype)
{
  tree result = NULL_TREE;

  gcc_assert (etype && totype);
  Type *ebtype = etype->toBasetype ();
  Type *tbtype = totype->toBasetype ();

  if (same_type_p (etype, totype))
    return exp;

  if (error_operand_p (exp))
    return exp;

  switch (ebtype->ty)
    {
    case TY::Tdelegate:
      if (tbtype->ty == TY::Tdelegate)
	{
	  exp = d_save_expr (exp);
	  return build_delegate_cst (delegate_method (exp),
				     delegate_object (exp), totype);
	}
      else if (tbtype->ty == TY::Tpointer)
	{
	  /* The front-end converts <delegate>.ptr to cast (void *)<delegate>.
	     Maybe should only allow void* ?  */
	  exp = delegate_object (exp);
	}
      else
	{
	  error ("cannot convert a delegate expression to %qs",
		 totype->toChars ());
	  return error_mark_node;
	}
      break;

    case TY::Tstruct:
      if (tbtype->ty == TY::Tstruct)
	{
	  if (totype->size () == etype->size ())
	    {
	      /* Allowed to cast to structs with same type size.  */
	      result = build_vconvert (build_ctype (totype), exp);
	    }
	  else
	    {
	      error ("cannot convert struct %qs to %qs",
		     etype->toChars (), totype->toChars ());
	      return error_mark_node;
	    }
	}
      /* else, default conversion, which should produce an error.  */
      break;

    case TY::Tclass:
      if (tbtype->ty == TY::Tclass)
	{
	  ClassDeclaration *cdfrom = ebtype->isClassHandle ();
	  ClassDeclaration *cdto = tbtype->isClassHandle ();
	  int offset;

	  if (cdto->isBaseOf (cdfrom, &offset) && offset != OFFSET_RUNTIME)
	    {
	      /* Casting up the inheritance tree: Don't do anything special.
		 Cast to an implemented interface: Handle at compile-time.  */
	      if (offset)
		{
		  /* Forward references should not leak from the frontend.  */
		  gcc_assert (offset != OFFSET_FWDREF);

		  tree type = build_ctype (totype);
		  exp = d_save_expr (exp);

		  tree cond = build_boolop (NE_EXPR, exp, null_pointer_node);
		  tree object = build_offset (exp, size_int (offset));

		  return build_condition (build_ctype (totype), cond,
					  build_nop (type, object),
					  build_nop (type, null_pointer_node));
		}

	      /* d_convert will make a no-op cast.  */
	      break;
	    }
	  else if (cdfrom->isCPPclass () || cdto->isCPPclass ())
	    {
	      /* Downcasting in C++ is a no-op.  */
	      if (cdfrom->isCPPclass () && cdto->isCPPclass ())
		break;

	      /* Casting from a C++ interface to a class/non-C++ interface
		 always results in null as there is no run-time information,
		 and no way one can derive from the other.  */
	      warning (OPT_Wcast_result, "cast to %qs will produce null result",
		       totype->toChars ());
	      result = d_convert (build_ctype (totype), null_pointer_node);

	      /* Make sure the expression is still evaluated if necessary.  */
	      if (TREE_SIDE_EFFECTS (exp))
		result = compound_expr (exp, result);

	      break;
	    }

	  /* The offset can only be determined at run-time, do dynamic cast.  */
	  libcall_fn libcall = cdfrom->isInterfaceDeclaration ()
	    ? LIBCALL_INTERFACE_CAST : LIBCALL_DYNAMIC_CAST;

	  return build_libcall (libcall, totype, 2, exp,
				build_address (get_classinfo_decl (cdto)));
	}
      /* else default conversion.  */
      break;

    case TY::Tsarray:
      if (tbtype->ty == TY::Tpointer)
	{
	  result = build_nop (build_ctype (totype), build_address (exp));
	}
      else if (tbtype->ty == TY::Tarray)
	{
	  dinteger_t dim = ebtype->isTypeSArray ()->dim->toInteger ();
	  dinteger_t esize = ebtype->nextOf ()->size ();
	  dinteger_t tsize = tbtype->nextOf ()->size ();

	  tree ptrtype = build_ctype (dmd::pointerTo (tbtype->nextOf ()));

	  if (esize != tsize)
	    {
	      /* Array element sizes do not match, so we must adjust the
		 dimensions.  */
	      if (tsize == 0 || (dim * esize) % tsize != 0)
		{
		  error ("cannot cast %qs to %qs since sizes do not line up",
			 etype->toChars (), totype->toChars ());
		  return error_mark_node;
		}
	      dim = (dim * esize) / tsize;
	    }

	  /* Assumes casting to dynamic array of same type or void.  */
	  return d_array_value (build_ctype (totype), size_int (dim),
				build_nop (ptrtype, build_address (exp)));
	}
      else if (tbtype->ty == TY::Tsarray)
	{
	  /* D allows casting a static array to any static array type.  */
	  return build_nop (build_ctype (totype), exp);
	}
      else if (tbtype->ty == TY::Tstruct)
	{
	  /* And allows casting a static array to any struct type too.
	     Type sizes should have already been checked by the frontend.  */
	  gcc_assert (totype->size () == etype->size ());
	  result = build_vconvert (build_ctype (totype), exp);
	}
      else if (tbtype->ty == TY::Tvector && tbtype->size () == ebtype->size ())
	{
	  /* Allow casting from array to vector as if its an unaligned load.  */
	  tree type = build_ctype (totype);
	  tree unaligned_type = build_variant_type_copy (type);
	  SET_TYPE_ALIGN (unaligned_type, 1 * BITS_PER_UNIT);
	  TYPE_USER_ALIGN (unaligned_type) = 1;
	  result = convert (type, build_vconvert (unaligned_type, exp));
	}
      else
	{
	  error ("cannot cast expression of type %qs to type %qs",
		 etype->toChars (), totype->toChars ());
	  return error_mark_node;
	}
      break;

    case TY::Tarray:
      if (tbtype->ty == TY::Tpointer)
	{
	  return d_convert (build_ctype (totype), d_array_ptr (exp));
	}
      else if (tbtype->ty == TY::Tarray)
	{
	  /* Assume tvoid->size() == 1.  */
	  dinteger_t fsize = ebtype->nextOf ()->toBasetype ()->size ();
	  dinteger_t tsize = tbtype->nextOf ()->toBasetype ()->size ();

	  if (fsize != tsize)
	    {
	      /* Conversion requires a reinterpret cast of array.
		 This case should have been lowered in the semantic pass.  */
	      if (tsize != 0 && fsize % tsize == 0)
		{
		  /* Set array dimension to (length * (fsize / tsize)).  */
		  tree newlength = size_mult_expr (d_array_length (exp),
						   size_int (fsize / tsize));
		  return d_array_value (build_ctype (totype), newlength,
					d_array_ptr (exp));
		}
	      else
		gcc_unreachable ();
	    }
	  else
	    {
	      /* Convert from void[] or elements are the same size
		 -- don't change length.  */
	      return build_vconvert (build_ctype (totype), exp);
	    }
	}
      else if (tbtype->ty == TY::Tsarray)
	{
	  /* Strings are treated as dynamic arrays in D2.  */
	  if (ebtype->isString () && tbtype->isString ())
	    return indirect_ref (build_ctype (totype), d_array_ptr (exp));
	}
      else
	{
	  error ("cannot cast expression of type %qs to %qs",
		 etype->toChars (), totype->toChars ());
	  return error_mark_node;
	}
      break;

    case TY::Taarray:
      if (tbtype->ty == TY::Taarray)
	return build_vconvert (build_ctype (totype), exp);
      /* Can convert associative arrays to void pointers.  */
      else if (tbtype->ty == TY::Tpointer && tbtype->nextOf ()->ty == TY::Tvoid)
	return build_vconvert (build_ctype (totype), exp);
      /* Else, default conversion, which should product an error.  */
      break;

    case TY::Tpointer:
      /* Can convert void pointers to associative arrays too.  */
      if (tbtype->ty == TY::Taarray && ebtype->nextOf ()->ty == TY::Tvoid)
	return build_vconvert (build_ctype (totype), exp);
      break;

    case TY::Tnull:
    case TY::Tnoreturn:
      /* Casting from `typeof(null)' for `null' expressions, or `typeof(*null)'
	 for `noreturn' expressions is represented as all zeros.  */
      result = build_typeof_null_value (totype);

      /* Make sure the expression is still evaluated if necessary.  */
      if (TREE_SIDE_EFFECTS (exp))
	result = compound_expr (exp, result);
      break;

    case TY::Tvector:
      if (tbtype->ty == TY::Tsarray)
	{
	  if (tbtype->size () == ebtype->size ())
	    return build_vconvert (build_ctype (totype), exp);
	}
      break;

    default:
      /* All casts between imaginary and non-imaginary result in 0.0,
	 except for casts between complex and imaginary types.  */
      if (!ebtype->iscomplex () && !tbtype->iscomplex ()
	  && (ebtype->isimaginary () != tbtype->isimaginary ()))
	{
	  warning (OPT_Wcast_result,
		   "cast from %qs to %qs will produce zero result",
		   ebtype->toChars (), tbtype->toChars ());

	  return compound_expr (exp, build_zero_cst (build_ctype (tbtype)));
	}

      gcc_assert (TREE_CODE (exp) != STRING_CST);
      break;
    }

  return result ? result : convert (build_ctype (totype), exp);
}

/* Return a TREE representation of EXPR, whose type has been converted from
 * ETYPE to TOTYPE, and is being used in an rvalue context.  */

tree
convert_for_rvalue (tree expr, Type *etype, Type *totype)
{
  tree result = NULL_TREE;

  Type *ebtype = etype->toBasetype ();
  Type *tbtype = totype->toBasetype ();

  if (ebtype->ty == TY::Tbool)
    {
      /* If casting from bool, the result is either 0 or 1, any other value
	 violates @safe code, so enforce that it is never invalid.  */
      for (tree ref = expr; TREE_CODE (ref) == COMPONENT_REF;
	   ref = TREE_OPERAND (ref, 0))
	{
	  /* If the expression is a field that's part of a union, reinterpret
	     the boolean as an integer and test the first bit.  The generated
	     code should end up being equivalent to:
		*cast(ubyte *)&expr & 1;  */
	  if (TREE_CODE (TREE_TYPE (TREE_OPERAND (ref, 0))) == UNION_TYPE)
	    {
	      machine_mode bool_mode = TYPE_MODE (TREE_TYPE (expr));
	      tree mtype = lang_hooks.types.type_for_mode (bool_mode, 1);
	      result = fold_build2 (BIT_AND_EXPR, mtype,
				    build_vconvert (mtype, expr),
				    build_one_cst (mtype));
	      break;
	    }
	}

      if (result == NULL_TREE)
	result = d_truthvalue_conversion (expr);

      result = convert (build_ctype (tbtype), result);
    }

  if (tbtype->ty == TY::Tsarray
      && ebtype->ty == TY::Tsarray
      && tbtype->nextOf ()->ty == ebtype->nextOf ()->ty
      && INDIRECT_REF_P (expr)
      && CONVERT_EXPR_P (TREE_OPERAND (expr, 0))
      && TREE_CODE (TREE_OPERAND (TREE_OPERAND (expr, 0), 0)) == ADDR_EXPR)
    {
      /* If expression is a vector that was casted to an array either by
	 explicit type cast or by taking the vector's `.array' value, strip the
	 reinterpret cast and build a constructor instead.  */
      tree ptr = TREE_OPERAND (TREE_OPERAND (expr, 0), 0);

      if (VECTOR_TYPE_P (TREE_TYPE (TREE_TYPE (ptr))))
	{
	  /* Rewrite: `*(Array *)&vector'
		into: `{ vector[0], vector[1], ... }'  */
	  tree array = d_save_expr (TREE_OPERAND (ptr, 0));
	  array = build1 (VIEW_CONVERT_EXPR, TREE_TYPE (expr), array);

	  uinteger_t dim = tbtype->isTypeSArray ()->dim->toUInteger ();
	  vec <constructor_elt, va_gc> *elms = NULL;
	  for (uinteger_t i = 0; i < dim; i++)
	    {
	      tree index = size_int (i);
	      tree value = build4 (ARRAY_REF, TREE_TYPE (TREE_TYPE (array)),
				   array, index, NULL_TREE, NULL_TREE);
	      CONSTRUCTOR_APPEND_ELT (elms, index, value);
	    }

	  return build_constructor (build_ctype (totype), elms);
	}
    }

  return result ? result : convert_expr (expr, etype, totype);
}

/* Helper for convert_for_assigment and convert_for_argument.
   Returns true if EXPR is a va_list static array parameter.  */

static bool
is_valist_parameter_type (Expression *expr)
{
  Declaration *decl = NULL;

  if (VarExp *ve = expr->isVarExp ())
    decl = ve->var;
  else if (SymOffExp *se = expr->isSymOffExp ())
    decl = se->var;

  if (decl != NULL && decl->isParameter () && valist_array_p (decl->type))
    return true;

  return false;
}

/* Helper for convert_for_assigment and convert_for_argument.
   Report erroneous uses of assigning or passing a va_list parameter.  */

static void
check_valist_conversion (Expression *expr, Type *totype, bool in_assignment)
{
  /* Parameter symbol and its converted type.  */
  Declaration *decl = NULL;
  /* Type of parameter when evaluated in the expression.  */
  Type *type = NULL;

  if (VarExp *ve = expr->isVarExp ())
    {
      decl = ve->var;
      type = dmd::pointerTo (ve->var->type->nextOf ());
    }
  else if (SymOffExp *se = expr->isSymOffExp ())
    {
      decl = se->var;
      type = dmd::pointerTo (dmd::pointerTo (se->var->type->nextOf ()));
    }

  /* Should not be called unless is_valist_parameter_type also matched.  */
  gcc_assert (decl != NULL && decl->isParameter ()
	      && valist_array_p (decl->type));

  /* OK if conversion between types is allowed.  */
  if (type->implicitConvTo (totype) != MATCH::nomatch)
    return;

  if (in_assignment)
    {
      error_at (make_location_t (expr->loc), "cannot convert parameter %qs "
		"from type %qs to type %qs in assignment",
		expr->toChars(), type->toChars (), totype->toChars ());
    }
  else
    {
      error_at (make_location_t (expr->loc), "cannot convert parameter %qs "
		"from type %qs to type %qs in argument passing",
		expr->toChars(), type->toChars (), totype->toChars ());
    }

  inform (make_location_t (decl->loc), "parameters of type %<va_list%> "
	  "{aka %qs} are decayed to pointer types, and require %<va_copy%> "
	  "to be converted back into a static array type",
	  decl->type->toChars ());
}

/* Apply semantics of assignment to a value of type TOTYPE to EXPR
   For example: `pointer = array' gets lowered to `pointer = &array[0]'.
   If LITERALP is true, then EXPR is a value used in the initialization
   of another literal.

   Return a TREE representation of EXPR implicitly converted to TOTYPE
   for use in assignment expressions MODIFY_EXPR, INIT_EXPR.  */

tree
convert_for_assignment (Expression *expr, Type *totype, bool literalp)
{
  Type *ebtype = expr->type->toBasetype ();
  Type *tbtype = totype->toBasetype ();

  /* Assuming this only has to handle converting a non Tsarray type to
     arbitrarily dimensioned Tsarrays.  */
  if (tbtype->ty == TY::Tsarray)
    {
      Type *telem = tbtype->nextOf ()->baseElemOf ();

      if (same_type_p (telem, ebtype))
	{
	  TypeSArray *sa_type = tbtype->isTypeSArray ();
	  uinteger_t count = sa_type->dim->toUInteger ();

	  tree ctor = build_constructor (build_ctype (totype), NULL);
	  if (count)
	    {
	      vec <constructor_elt, va_gc> *ce = NULL;
	      tree index = build2 (RANGE_EXPR, build_ctype (Type::tsize_t),
				   size_zero_node, size_int (count - 1));
	      tree value = convert_for_assignment (expr, sa_type->next,
						   literalp);
	      /* Can't use VAR_DECLs in CONSTRUCTORS.  */
	      if (VAR_P (value))
		{
		  value = DECL_INITIAL (value);
		  gcc_assert (value);
		}

	      CONSTRUCTOR_APPEND_ELT (ce, index, value);
	      CONSTRUCTOR_ELTS (ctor) = ce;
	    }
	  TREE_READONLY (ctor) = 1;
	  TREE_CONSTANT (ctor) = 1;
	  return ctor;
	}
    }

  /* D Front end uses IntegerExp(0) to mean zero-init an array or structure.  */
  if ((tbtype->ty == TY::Tsarray || tbtype->ty == TY::Tstruct)
      && ebtype->isintegral ())
    {
      tree ret = build_expr (expr, false, literalp);
      gcc_assert (integer_zerop (ret));
      return ret;
    }

  /* Assigning a va_list by value or reference, check whether RHS is a parameter
     that has has been lowered by declaration_type or parameter_type.  */
  if (is_valist_parameter_type (expr))
    check_valist_conversion (expr, totype, true);

  return convert_for_rvalue (build_expr (expr, false, literalp),
			     expr->type, totype);
}

/* Return a TREE representation of EXPR converted to represent
   the parameter type ARG.  */

tree
convert_for_argument (Expression *expr, Parameter *arg)
{
  tree targ = build_expr (expr);

  /* Lazy arguments: expr should already be a delegate.  */
  if (arg->storageClass & STClazy)
    return targ;

  /* Passing a va_list by value, check whether the target requires it to
     be decayed to a pointer type.  */
  if (valist_array_p (arg->type))
    {
      if (!POINTER_TYPE_P (TREE_TYPE (targ)))
	return build_address (targ);

      /* Do nothing if the va_list has already been converted.  */
      return targ;
    }

  /* Passing a va_list by reference, check if types are really compatible
     after conversion from static array to pointer type.  */
  if (is_valist_parameter_type (expr))
    check_valist_conversion (expr, arg->type, false);

  /* Front-end shouldn't automatically take the address of `ref' parameters.  */
  if (parameter_reference_p (arg))
    return convert (parameter_type (arg), build_address (targ));

  return targ;
}

/* Perform default promotions for data used in expressions.
   Arrays and functions are converted to pointers;
   enumeral types or short or char, to int.
   In addition, manifest constants symbols are replaced by their values.

   Return truth-value conversion of expression EXPR from value type TYPE.  */

tree
convert_for_condition (tree expr, Type *type)
{
  tree result = NULL_TREE;

  switch (type->toBasetype ()->ty)
    {
    case TY::Taarray:
      /* Checks that aa.ptr !is null.  */
      result = component_ref (expr, TYPE_FIELDS (TREE_TYPE (expr)));
      break;

    case TY::Tarray:
      {
	/* Checks (arr.length || arr.ptr) (i.e arr !is null).  */
	expr = d_save_expr (expr);
	tree len = d_array_length (expr);
	tree ptr = d_array_ptr (expr);
	if (TYPE_MODE (TREE_TYPE (len)) == TYPE_MODE (TREE_TYPE (ptr)))
	  {
	    result = build2 (BIT_IOR_EXPR, TREE_TYPE (len), len,
			     d_convert (TREE_TYPE (len), ptr));
	  }
	else
	  {
	    len = d_truthvalue_conversion (len);
	    ptr = d_truthvalue_conversion (ptr);
	    /* Probably not worth using TRUTH_OROR here.  */
	    result = build2 (TRUTH_OR_EXPR, TREE_TYPE (len), len, ptr);
	  }
	break;
      }

    case TY::Tdelegate:
      {
	/* Checks (function || object), but what good is it if there is
	   a null function pointer?  */
	tree obj, func;
	if (METHOD_CALL_EXPR (expr))
	  extract_from_method_call (expr, obj, func);
	else
	  {
	    expr = d_save_expr (expr);
	    obj = delegate_object (expr);
	    func = delegate_method (expr);
	  }

	obj = d_truthvalue_conversion (obj);
	func = d_truthvalue_conversion (func);
	/* Probably not worth using TRUTH_ORIF here.  */
	result = build2 (BIT_IOR_EXPR, TREE_TYPE (obj), obj, func);
	break;
      }

    case TY::Tnoreturn:
      /* Front-end allows conditionals that never return, represent the
	 conditional result value as all zeros.  */
      result = build_zero_cst (d_bool_type);

      /* Make sure the expression is still evaluated if necessary.  */
      if (TREE_SIDE_EFFECTS (expr))
	result = compound_expr (expr, result);
      break;

    default:
      result = convert_for_rvalue (expr, type, type);
      break;
    }

  return d_truthvalue_conversion (result);
}


/* Convert EXP to a dynamic array.
   EXP must be a static array or dynamic array.  */

tree
d_array_convert (Expression *exp)
{
  Type *tb = exp->type->toBasetype ();

  if (tb->ty == TY::Tarray)
    return build_expr (exp);

  if (tb->ty == TY::Tsarray)
    {
      Type *totype = dmd::arrayOf (tb->nextOf ());
      return convert_expr (build_expr (exp), exp->type, totype);
    }

  /* Invalid type passed.  */
  gcc_unreachable ();
}

/* Convert EXP to a dynamic array, where ETYPE is the element type.
   Similar to above, except that EXP is allowed to be an element of an array.
   Temporary variables are created inline if EXP is not an lvalue.  */

tree
d_array_convert (Type *etype, Expression *exp)
{
  Type *tb = exp->type->toBasetype ();

  if ((tb->ty != TY::Tarray && tb->ty != TY::Tsarray)
      || same_type_p (tb, etype))
    {
      /* Convert single element to an array.  */
      tree expr = build_expr (exp);

      if (!exp->isLvalue ())
	{
	  tree var = build_local_temp (TREE_TYPE (expr));
	  expr = compound_expr (modify_expr (var, expr), var);
	}

      return d_array_value (build_ctype (dmd::arrayOf (exp->type)),
			    size_int (1), build_address (expr));
    }
  else
    return d_array_convert (exp);
}
