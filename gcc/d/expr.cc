/* expr.cc -- Lower D frontend expressions to GCC trees.
   Copyright (C) 2015-2020 Free Software Foundation, Inc.

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
#include "dmd/ctfe.h"
#include "dmd/declaration.h"
#include "dmd/expression.h"
#include "dmd/identifier.h"
#include "dmd/init.h"
#include "dmd/module.h"
#include "dmd/mtype.h"
#include "dmd/template.h"

#include "tree.h"
#include "fold-const.h"
#include "diagnostic.h"
#include "langhooks.h"
#include "tm.h"
#include "function.h"
#include "toplev.h"
#include "varasm.h"
#include "predict.h"
#include "stor-layout.h"

#include "d-tree.h"


/* Implements the visitor interface to build the GCC trees of all Expression
   AST classes emitted from the D Front-end.
   All visit methods accept one parameter E, which holds the frontend AST
   of the expression to compile.  They also don't return any value, instead
   generated code is cached in RESULT_ and returned from the caller.  */

class ExprVisitor : public Visitor
{
  using Visitor::visit;

  tree result_;
  bool constp_;

  /* Determine if type is a struct that has a postblit.  */

  bool needs_postblit (Type *t)
  {
    t = t->baseElemOf ();

    if (t->ty == Tstruct)
      {
	StructDeclaration *sd = ((TypeStruct *) t)->sym;
	if (sd->postblit)
	  return true;
      }

    return false;
  }

  /* Determine if type is a struct that has a destructor.  */

  bool needs_dtor (Type *t)
  {
    t = t->baseElemOf ();

    if (t->ty == Tstruct)
      {
	StructDeclaration *sd = ((TypeStruct *) t)->sym;
	if (sd->dtor)
	  return true;
      }

    return false;
  }

  /* Determine if expression is suitable lvalue.  */

  bool lvalue_p (Expression *e)
  {
    return ((e->op != TOKslice && e->isLvalue ())
	    || (e->op == TOKslice && ((UnaExp *) e)->e1->isLvalue ())
	    || (e->op == TOKcast && ((UnaExp *) e)->e1->isLvalue ()));
  }

  /* Build an expression of code CODE, data type TYPE, and operands ARG0 and
     ARG1.  Perform relevant conversions needed for correct code operations.  */

  tree binary_op (tree_code code, tree type, tree arg0, tree arg1)
  {
    tree t0 = TREE_TYPE (arg0);
    tree t1 = TREE_TYPE (arg1);
    tree ret = NULL_TREE;

    bool unsignedp = TYPE_UNSIGNED (t0) || TYPE_UNSIGNED (t1);

    /* Deal with float mod expressions immediately.  */
    if (code == FLOAT_MOD_EXPR)
      return build_float_modulus (type, arg0, arg1);

    if (POINTER_TYPE_P (t0) && INTEGRAL_TYPE_P (t1))
      return build_nop (type, build_offset_op (code, arg0, arg1));

    if (INTEGRAL_TYPE_P (t0) && POINTER_TYPE_P (t1))
      return build_nop (type, build_offset_op (code, arg1, arg0));

    if (POINTER_TYPE_P (t0) && POINTER_TYPE_P (t1))
      {
	gcc_assert (code == MINUS_EXPR);
	tree ptrtype = lang_hooks.types.type_for_mode (ptr_mode, 0);

	/* POINTER_DIFF_EXPR requires a signed integer type of the same size as
	   pointers.  If some platform cannot provide that, or has a larger
	   ptrdiff_type to support differences larger than half the address
	   space, cast the pointers to some larger integer type and do the
	   computations in that type.  */
	if (TYPE_PRECISION (ptrtype) > TYPE_PRECISION (t0))
	  ret = fold_build2 (MINUS_EXPR, ptrtype,
			     d_convert (ptrtype, arg0),
			     d_convert (ptrtype, arg1));
	else
	  ret = fold_build2 (POINTER_DIFF_EXPR, ptrtype, arg0, arg1);
      }
    else if (INTEGRAL_TYPE_P (type) && (TYPE_UNSIGNED (type) != unsignedp))
      {
	tree inttype = (unsignedp)
	  ? d_unsigned_type (type) : d_signed_type (type);
	ret = fold_build2 (code, inttype, arg0, arg1);
      }
    else
      {
	/* If the operation needs excess precision.  */
	tree eptype = excess_precision_type (type);
	if (eptype != NULL_TREE)
	  {
	    arg0 = d_convert (eptype, arg0);
	    arg1 = d_convert (eptype, arg1);
	  }
	else
	  {
	    /* Front-end does not do this conversion and GCC does not
	       always do it right.  */
	    if (COMPLEX_FLOAT_TYPE_P (t0) && !COMPLEX_FLOAT_TYPE_P (t1))
	      arg1 = d_convert (t0, arg1);
	    else if (COMPLEX_FLOAT_TYPE_P (t1) && !COMPLEX_FLOAT_TYPE_P (t0))
	      arg0 = d_convert (t1, arg0);

	    eptype = type;
	  }

	ret = fold_build2 (code, eptype, arg0, arg1);
      }

    return d_convert (type, ret);
  }

  /* Build a binary expression of code CODE, assigning the result into E1.  */

  tree binop_assignment (tree_code code, Expression *e1, Expression *e2)
  {
    /* Skip casts for lhs assignment.  */
    Expression *e1b = e1;
    while (e1b->op == TOKcast)
      {
	CastExp *ce = (CastExp *) e1b;
	gcc_assert (same_type_p (ce->type, ce->to));
	e1b = ce->e1;
      }

    /* Stabilize LHS for assignment.  */
    tree lhs = build_expr (e1b);
    tree lexpr = stabilize_expr (&lhs);

    /* The LHS expression could be an assignment, to which its operation gets
       lost during gimplification.  */
    if (TREE_CODE (lhs) == MODIFY_EXPR)
      {
	/* If LHS has side effects, call stabilize_reference on it, so it can
	   be evaluated multiple times.  */
	if (TREE_SIDE_EFFECTS (TREE_OPERAND (lhs, 0)))
	  lhs = build_assign (MODIFY_EXPR,
			      stabilize_reference (TREE_OPERAND (lhs, 0)),
			      TREE_OPERAND (lhs, 1));

	lexpr = compound_expr (lexpr, lhs);
	lhs = TREE_OPERAND (lhs, 0);
      }

    lhs = stabilize_reference (lhs);

    /* Save RHS, to ensure that the expression is evaluated before LHS.  */
    tree rhs = build_expr (e2);
    tree rexpr = d_save_expr (rhs);

    rhs = this->binary_op (code, build_ctype (e1->type),
			   convert_expr (lhs, e1b->type, e1->type), rexpr);
    if (TREE_SIDE_EFFECTS (rhs))
      rhs = compound_expr (rexpr, rhs);

    tree expr = modify_expr (lhs, convert_expr (rhs, e1->type, e1b->type));
    return compound_expr (lexpr, expr);
  }

public:
  ExprVisitor (bool constp)
  {
    this->result_ = NULL_TREE;
    this->constp_ = constp;
  }

  tree result (void)
  {
    return this->result_;
  }

  /* Visitor interfaces, each Expression class should have
     overridden the default.  */

  void visit (Expression *)
  {
    gcc_unreachable ();
  }

  /* Build a conditional expression.  If either the second or third
     expression is void, then the resulting type is void.  Otherwise
     they are implicitly converted to a common type.  */

  void visit (CondExp *e)
  {
    tree cond = convert_for_condition (build_expr (e->econd),
				       e->econd->type);
    tree t1 = build_expr (e->e1);
    tree t2 = build_expr (e->e2);

    if (e->type->ty != Tvoid)
      {
	t1 = convert_expr (t1, e->e1->type, e->type);
	t2 = convert_expr (t2, e->e2->type, e->type);
      }

    this->result_ = build_condition (build_ctype (e->type), cond, t1, t2);
  }

  /* Build an identity comparison expression.  Operands go through the
     usual conversions to bring them to a common type before comparison.
     The result type is bool.  */

  void visit (IdentityExp *e)
  {
    tree_code code = (e->op == TOKidentity) ? EQ_EXPR : NE_EXPR;
    Type *tb1 = e->e1->type->toBasetype ();
    Type *tb2 = e->e2->type->toBasetype ();

    if ((tb1->ty == Tsarray || tb1->ty == Tarray)
	&& (tb2->ty == Tsarray || tb2->ty == Tarray))
      {
	/* For static and dynamic arrays, identity is defined as referring to
	   the same array elements and the same number of elements.  */
	tree t1 = d_array_convert (e->e1);
	tree t2 = d_array_convert (e->e2);
	this->result_ = d_convert (build_ctype (e->type),
				   build_boolop (code, t1, t2));
      }
    else if (tb1->isfloating () && tb1->ty != Tvector)
      {
	/* For floating-point values, identity is defined as the bits in the
	   operands being identical.  */
	tree t1 = d_save_expr (build_expr (e->e1));
	tree t2 = d_save_expr (build_expr (e->e2));

	if (!tb1->iscomplex ())
	  this->result_ = build_float_identity (code, t1, t2);
	else
	  {
	    /* Compare the real and imaginary parts separately.  */
	    tree req = build_float_identity (code, real_part (t1),
					     real_part (t2));
	    tree ieq = build_float_identity (code, imaginary_part (t1),
					     imaginary_part (t2));

	    if (code == EQ_EXPR)
	      this->result_ = build_boolop (TRUTH_ANDIF_EXPR, req, ieq);
	    else
	      this->result_ = build_boolop (TRUTH_ORIF_EXPR, req, ieq);
	  }
      }
    else if (tb1->ty == Tstruct)
      {
	/* For struct objects, identity is defined as bits in operands being
	   identical also.  Alignment holes in structs are ignored.  */
	StructDeclaration *sd = ((TypeStruct *) tb1)->sym;
	tree t1 = build_expr (e->e1);
	tree t2 = build_expr (e->e2);

	gcc_assert (same_type_p (tb1, tb2));

	this->result_ = build_struct_comparison (code, sd, t1, t2);
      }
    else
      {
	/* For operands of other types, identity is defined as being the
	   same as equality expressions.  */
	tree t1 = build_expr (e->e1);
	tree t2 = build_expr (e->e2);
	this->result_ = d_convert (build_ctype (e->type),
				   build_boolop (code, t1, t2));
      }
  }

  /* Build an equality expression, which compare the two operands for either
     equality or inequality.  Operands go through the usual conversions to bring
     them to a common type before comparison.  The result type is bool.  */

  void visit (EqualExp *e)
  {
    Type *tb1 = e->e1->type->toBasetype ();
    Type *tb2 = e->e2->type->toBasetype ();
    tree_code code = (e->op == TOKequal) ? EQ_EXPR : NE_EXPR;

    if ((tb1->ty == Tsarray || tb1->ty == Tarray)
	&& (tb2->ty == Tsarray || tb2->ty == Tarray))
      {
	/* For static and dynamic arrays, equality is defined as the lengths of
	   the arrays matching, and all the elements are equal.  */
	Type *t1elem = tb1->nextOf ()->toBasetype ();
	Type *t2elem = tb1->nextOf ()->toBasetype ();

	/* Check if comparisons of arrays can be optimized using memcmp.
	   This will inline EQ expressions as:
		e1.length == e2.length && memcmp(e1.ptr, e2.ptr, size) == 0;
	    Or when generating a NE expression:
		e1.length != e2.length || memcmp(e1.ptr, e2.ptr, size) != 0;  */
	if ((t1elem->isintegral () || t1elem->ty == Tvoid
	     || (t1elem->ty == Tstruct && !((TypeStruct *)t1elem)->sym->xeq))
	    && t1elem->ty == t2elem->ty)
	  {
	    tree t1 = d_array_convert (e->e1);
	    tree t2 = d_array_convert (e->e2);
	    tree result;

	    /* Make temporaries to prevent multiple evaluations.  */
	    tree t1saved = d_save_expr (t1);
	    tree t2saved = d_save_expr (t2);

	    /* Length of arrays, for comparisons done before calling memcmp.  */
	    tree t1len = d_array_length (t1saved);
	    tree t2len = d_array_length (t2saved);

	    /* Reference to array data.  */
	    tree t1ptr = d_array_ptr (t1saved);
	    tree t2ptr = d_array_ptr (t2saved);

	    /* Compare arrays using memcmp if possible, otherwise for structs,
	       each field is compared inline.  */
	    if (t1elem->ty != Tstruct
		|| identity_compare_p (((TypeStruct *) t1elem)->sym))
	      {
		tree size = size_mult_expr (t1len, size_int (t1elem->size ()));
		tree tmemcmp = builtin_decl_explicit (BUILT_IN_MEMCMP);

		result = build_call_expr (tmemcmp, 3, t1ptr, t2ptr, size);
		result = build_boolop (code, result, integer_zero_node);
	      }
	    else
	      {
		StructDeclaration *sd = ((TypeStruct *) t1elem)->sym;

		result = build_array_struct_comparison (code, sd, t1len,
							t1ptr, t2ptr);
	      }

	    /* Check array length first before passing to memcmp.
	       For equality expressions, this becomes:
		    (e1.length == 0 || memcmp);
	       Otherwise for inequality:
		    (e1.length != 0 && memcmp);  */
	    tree tsizecmp = build_boolop (code, t1len, size_zero_node);
	    if (e->op == TOKequal)
	      result = build_boolop (TRUTH_ORIF_EXPR, tsizecmp, result);
	    else
	      result = build_boolop (TRUTH_ANDIF_EXPR, tsizecmp, result);

	    /* Finally, check if lengths of both arrays match if dynamic.
	       The frontend should have already guaranteed that static arrays
	       have same size.  */
	    if (tb1->ty == Tsarray && tb2->ty == Tsarray)
	      gcc_assert (tb1->size () == tb2->size ());
	    else
	      {
		tree tlencmp = build_boolop (code, t1len, t2len);
		if (e->op == TOKequal)
		  result = build_boolop (TRUTH_ANDIF_EXPR, tlencmp, result);
		else
		  result = build_boolop (TRUTH_ORIF_EXPR, tlencmp, result);
	      }

	    /* Ensure left-to-right order of evaluation.  */
	    if (TREE_SIDE_EFFECTS (t2))
	      result = compound_expr (t2saved, result);

	    if (TREE_SIDE_EFFECTS (t1))
	      result = compound_expr (t1saved, result);

	    this->result_ = result;
	  }
	else
	  {
	    /* Use _adEq2() to compare each element.  */
	    Type *t1array = t1elem->arrayOf ();
	    tree result = build_libcall (LIBCALL_ADEQ2, e->type, 3,
					 d_array_convert (e->e1),
					 d_array_convert (e->e2),
					 build_typeinfo (e->loc, t1array));

	    if (e->op == TOKnotequal)
	      result = build1 (TRUTH_NOT_EXPR, build_ctype (e->type), result);

	    this->result_ = result;
	  }
      }
    else if (tb1->ty == Tstruct)
      {
	/* Equality for struct objects means the logical product of all
	   equality results of the corresponding object fields.  */
	StructDeclaration *sd = ((TypeStruct *) tb1)->sym;
	tree t1 = build_expr (e->e1);
	tree t2 = build_expr (e->e2);

	gcc_assert (same_type_p (tb1, tb2));

	this->result_ = build_struct_comparison (code, sd, t1, t2);
      }
    else if (tb1->ty == Taarray && tb2->ty == Taarray)
      {
	/* Use _aaEqual() for associative arrays.  */
	TypeAArray *taa1 = (TypeAArray *) tb1;
	tree result = build_libcall (LIBCALL_AAEQUAL, e->type, 3,
				     build_typeinfo (e->loc, taa1),
				     build_expr (e->e1),
				     build_expr (e->e2));

	if (e->op == TOKnotequal)
	  result = build1 (TRUTH_NOT_EXPR, build_ctype (e->type), result);

	this->result_ = result;
      }
    else
      {
	/* For operands of other types, equality is defined as the bit pattern
	   of the type matches exactly.  */
	tree t1 = build_expr (e->e1);
	tree t2 = build_expr (e->e2);

	this->result_ = d_convert (build_ctype (e->type),
				   build_boolop (code, t1, t2));
      }
  }

  /* Build an `in' expression.  This is a condition to see if an element
     exists in an associative array.  The result is a pointer to the
     element, or null if false.  */

  void visit (InExp *e)
  {
    Type *tb2 = e->e2->type->toBasetype ();
    gcc_assert (tb2->ty == Taarray);

    Type *tkey = ((TypeAArray *) tb2)->index->toBasetype ();
    tree key = convert_expr (build_expr (e->e1), e->e1->type, tkey);

    /* Build a call to _aaInX().  */
    this->result_ = build_libcall (LIBCALL_AAINX, e->type, 3,
				   build_expr (e->e2),
				   build_typeinfo (e->loc, tkey),
				   build_address (key));
  }

  /* Build a relational expression.  The result type is bool.  */

  void visit (CmpExp *e)
  {
    Type *tb1 = e->e1->type->toBasetype ();
    Type *tb2 = e->e2->type->toBasetype ();

    tree result;
    tree_code code;

    switch (e->op)
      {
      case TOKle:
	code = LE_EXPR;
	break;

      case TOKlt:
	code = LT_EXPR;
	break;

      case TOKge:
	code = GE_EXPR;
	break;

      case TOKgt:
	code = GT_EXPR;
	break;

      default:
	gcc_unreachable ();
      }

    if ((tb1->ty == Tsarray || tb1->ty == Tarray)
	&& (tb2->ty == Tsarray || tb2->ty == Tarray))
      {
	/* For static and dynamic arrays, the result of the relational op is
	   the result of the operator applied to the first non-equal element
	   of the array.  If two arrays compare equal, but are of different
	   lengths, the shorter array compares as less than the longer.  */
	Type *telem = tb1->nextOf ()->toBasetype ();

	tree call = build_libcall (LIBCALL_ADCMP2, Type::tint32, 3,
				   d_array_convert (e->e1),
				   d_array_convert (e->e2),
				   build_typeinfo (e->loc, telem->arrayOf ()));
	result = build_boolop (code, call, integer_zero_node);

	this->result_ = d_convert (build_ctype (e->type), result);
	return;
      }

    /* Simple comparison.  */
    result = build_boolop (code, build_expr (e->e1), build_expr (e->e2));
    this->result_ = d_convert (build_ctype (e->type), result);
  }

  /* Build an `and if' expression.  If the right operand expression is void,
     then the resulting type is void.  Otherwise the result is bool.  */

  void visit (AndAndExp *e)
  {
    if (e->e2->type->toBasetype ()->ty != Tvoid)
      {
	tree t1 = build_expr (e->e1);
	tree t2 = build_expr (e->e2);

	t1 = convert_for_condition (t1, e->e1->type);
	t2 = convert_for_condition (t2, e->e2->type);

	this->result_ = d_convert (build_ctype (e->type),
				   build_boolop (TRUTH_ANDIF_EXPR, t1, t2));
      }
    else
      {
	tree t1 = convert_for_condition (build_expr (e->e1), e->e1->type);
	tree t2 = build_expr_dtor (e->e2);

	this->result_ = build_condition (build_ctype (e->type),
					 t1, t2, void_node);
      }
  }

  /* Build an `or if' expression.  If the right operand expression is void,
     then the resulting type is void.  Otherwise the result is bool.  */

  void visit (OrOrExp *e)
  {
    if (e->e2->type->toBasetype ()->ty != Tvoid)
      {
	tree t1 = convert_for_condition (build_expr (e->e1), e->e1->type);
	tree t2 = convert_for_condition (build_expr (e->e2), e->e2->type);

	this->result_ = d_convert (build_ctype (e->type),
				   build_boolop (TRUTH_ORIF_EXPR, t1, t2));
      }
    else
      {
	tree t1 = convert_for_condition (build_expr (e->e1), e->e1->type);
	tree t2 = build_expr_dtor (e->e2);
	tree cond = build1 (TRUTH_NOT_EXPR, d_bool_type, t1);

	this->result_ = build_condition (build_ctype (e->type),
					 cond, t2, void_node);
      }
  }

  /* Build a binary operand expression.  Operands go through usual arithmetic
     conversions to bring them to a common type before evaluating.  */

  void visit (BinExp *e)
  {
    tree_code code;

    switch (e->op)
      {
      case TOKadd:
      case TOKmin:
	if ((e->e1->type->isreal () && e->e2->type->isimaginary ())
	    || (e->e1->type->isimaginary () && e->e2->type->isreal ()))
	  {
	    /* If the result is complex, then we can shortcut binary_op.
	       Frontend should have already validated types and sizes.  */
	    tree t1 = build_expr (e->e1);
	    tree t2 = build_expr (e->e2);

	    if (e->op == TOKmin)
	      t2 = build1 (NEGATE_EXPR, TREE_TYPE (t2), t2);

	    if (e->e1->type->isreal ())
	      this->result_ = complex_expr (build_ctype (e->type), t1, t2);
	    else
	      this->result_ = complex_expr (build_ctype (e->type), t2, t1);

	    return;
	  }
	else
	  code = (e->op == TOKadd)
	    ? PLUS_EXPR : MINUS_EXPR;
	break;

      case TOKmul:
	code = MULT_EXPR;
	break;

      case TOKdiv:
	code = e->e1->type->isintegral ()
	  ? TRUNC_DIV_EXPR : RDIV_EXPR;
	break;

      case TOKmod:
	code = e->e1->type->isfloating ()
	  ? FLOAT_MOD_EXPR : TRUNC_MOD_EXPR;
	break;

      case TOKand:
	code = BIT_AND_EXPR;
	break;

      case TOKor:
	code = BIT_IOR_EXPR;
	break;

      case TOKxor:
	code = BIT_XOR_EXPR;
	break;

      case TOKshl:
	code = LSHIFT_EXPR;
	  break;

      case TOKshr:
	code = RSHIFT_EXPR;
	break;

      case TOKushr:
	code = UNSIGNED_RSHIFT_EXPR;
	break;

      default:
	gcc_unreachable ();
      }

    this->result_ = this->binary_op (code, build_ctype (e->type),
				     build_expr (e->e1), build_expr (e->e2));
  }


  /* Build a concat expression, which concatenates two or more arrays of the
     same type, producing a dynamic array with the result.  If one operand
     is an element type, that element is converted to an array of length 1.  */

  void visit (CatExp *e)
  {
    Type *tb1 = e->e1->type->toBasetype ();
    Type *tb2 = e->e2->type->toBasetype ();
    Type *etype;

    if (tb1->ty == Tarray || tb1->ty == Tsarray)
      etype = tb1->nextOf ();
    else
      etype = tb2->nextOf ();

    tree result;

    if (e->e1->op == TOKcat)
      {
	/* Flatten multiple concatenations to an array.
	   So the expression ((a ~ b) ~ c) becomes [a, b, c]  */
	int ndims = 2;

	for (Expression *ex = e->e1; ex->op == TOKcat;)
	  {
	    if (ex->op == TOKcat)
	      {
		ex = ((CatExp *) ex)->e1;
		ndims++;
	      }
	  }

	/* Store all concatenation args to a temporary byte[][ndims] array.  */
	Type *targselem = Type::tint8->arrayOf ();
	tree var = build_local_temp (make_array_type (targselem, ndims));

	/* Loop through each concatenation from right to left.  */
	vec<constructor_elt, va_gc> *elms = NULL;
	CatExp *ce = e;
	int dim = ndims - 1;

	for (Expression *oe = ce->e2; oe != NULL;
	     (ce->e1->op != TOKcat
	      ? (oe = ce->e1)
	      : (ce = (CatExp *)ce->e1, oe = ce->e2)))
	  {
	    tree arg = d_array_convert (etype, oe);
	    tree index = size_int (dim);
	    CONSTRUCTOR_APPEND_ELT (elms, index, d_save_expr (arg));

	    /* Finished pushing all arrays.  */
	    if (oe == ce->e1)
	      break;

	    dim -= 1;
	  }

	/* Check there is no logic bug in constructing byte[][] of arrays.  */
	gcc_assert (dim == 0);
	tree init = build_constructor (TREE_TYPE (var), elms);
	var = compound_expr (modify_expr (var, init), var);

	tree arrs = d_array_value (build_ctype (targselem->arrayOf ()),
				   size_int (ndims), build_address (var));

	result = build_libcall (LIBCALL_ARRAYCATNTX, e->type, 2,
				build_typeinfo (e->loc, e->type), arrs);
      }
    else
      {
	/* Handle single concatenation (a ~ b).  */
	result = build_libcall (LIBCALL_ARRAYCATT, e->type, 3,
				build_typeinfo (e->loc, e->type),
				d_array_convert (etype, e->e1),
				d_array_convert (etype, e->e2));
      }

    this->result_ = result;
  }

  /* Build an assignment operator expression.  The right operand is implicitly
     converted to the type of the left operand, and assigned to it.  */

  void visit (BinAssignExp *e)
  {
    tree_code code;
    Expression *e1b = e->e1;

    switch (e->op)
      {
      case TOKaddass:
	code = PLUS_EXPR;
	break;

      case TOKminass:
	code = MINUS_EXPR;
	break;

      case TOKmulass:
	code = MULT_EXPR;
	break;

      case TOKdivass:
	code = e->e1->type->isintegral ()
	  ? TRUNC_DIV_EXPR : RDIV_EXPR;
	break;

      case TOKmodass:
	code = e->e1->type->isfloating ()
	  ? FLOAT_MOD_EXPR : TRUNC_MOD_EXPR;
	break;

      case TOKandass:
	code = BIT_AND_EXPR;
	break;

      case TOKorass:
	code = BIT_IOR_EXPR;
	break;

      case TOKxorass:
	code = BIT_XOR_EXPR;
	break;

      case TOKpowass:
	gcc_unreachable ();

      case TOKshlass:
	code = LSHIFT_EXPR;
	break;

      case TOKshrass:
      case TOKushrass:
	/* Use the original lhs type before it was promoted.  The left operand
	   of `>>>=' does not undergo integral promotions before shifting.
	   Strip off casts just incase anyway.  */
	while (e1b->op == TOKcast)
	  {
	    CastExp *ce = (CastExp *) e1b;
	    gcc_assert (same_type_p (ce->type, ce->to));
	    e1b = ce->e1;
	  }
	code = (e->op == TOKshrass) ? RSHIFT_EXPR : UNSIGNED_RSHIFT_EXPR;
	break;

      default:
	gcc_unreachable ();
      }

    tree exp = this->binop_assignment (code, e1b, e->e2);
    this->result_ = convert_expr (exp, e1b->type, e->type);
  }

  /* Build a concat assignment expression.  The right operand is appended
     to the left operand.  */

  void visit (CatAssignExp *e)
  {
    Type *tb1 = e->e1->type->toBasetype ();
    Type *tb2 = e->e2->type->toBasetype ();
    Type *etype = tb1->nextOf ()->toBasetype ();

    /* Save the address of `e1', so it can be evaluated first.
       As all D run-time library functions for concat assignments update `e1'
       in-place and then return its value, the saved address can also be used as
       the result of this expression as well.  */
    tree lhs = build_expr (e->e1);
    tree lexpr = stabilize_expr (&lhs);
    tree ptr = d_save_expr (build_address (lhs));
    tree result = NULL_TREE;

    if (tb1->ty == Tarray && tb2->ty == Tdchar
	&& (etype->ty == Tchar || etype->ty == Twchar))
      {
	/* Append a dchar to a char[] or wchar[]:
	   The assignment is handled by the D run-time library, so only
	   need to call `_d_arrayappend[cw]d(&e1, e2)'  */
	libcall_fn libcall = (etype->ty == Tchar)
	  ? LIBCALL_ARRAYAPPENDCD : LIBCALL_ARRAYAPPENDWD;

	result = build_libcall (libcall, e->type, 2,
				ptr, build_expr (e->e2));
      }
    else
      {
	gcc_assert (tb1->ty == Tarray || tb2->ty == Tsarray);

	if ((tb2->ty == Tarray || tb2->ty == Tsarray)
	    && same_type_p (etype, tb2->nextOf ()->toBasetype ()))
	  {
	    /* Append an array to another array:
	       The assignment is handled by the D run-time library, so only
	       need to call `_d_arrayappendT(ti, &e1, e2)'  */
	    result = build_libcall (LIBCALL_ARRAYAPPENDT, e->type, 3,
				    build_typeinfo (e->loc, e->type),
				    ptr, d_array_convert (e->e2));
	  }
	else if (same_type_p (etype, tb2))
	  {
	    /* Append an element to an array:
	       The assignment is generated inline, so need to handle temporaries
	       here, and ensure that they are evaluated in the correct order.

	       The generated code should end up being equivalent to:
		    _d_arrayappendcTX(ti, &e1, 1)[e1.length - 1] = e2
	     */
	    tree callexp = build_libcall (LIBCALL_ARRAYAPPENDCTX, e->type, 3,
					  build_typeinfo (e->loc, e->type),
					  ptr, size_one_node);
	    callexp = d_save_expr (callexp);

	    /* Assign e2 to last element.  */
	    tree offexp = d_array_length (callexp);
	    offexp = build2 (MINUS_EXPR, TREE_TYPE (offexp),
			     offexp, size_one_node);

	    tree ptrexp = d_array_ptr (callexp);
	    ptrexp = void_okay_p (ptrexp);
	    ptrexp = build_array_index (ptrexp, offexp);

	    /* Evaluate expression before appending.  */
	    tree rhs = build_expr (e->e2);
	    tree rexpr = stabilize_expr (&rhs);

	    if (TREE_CODE (rhs) == CALL_EXPR)
	      rhs = force_target_expr (rhs);

	    result = modify_expr (build_deref (ptrexp), rhs);
	    result = compound_expr (rexpr, result);
	  }
	else
	  gcc_unreachable ();
      }

    /* Construct in order: ptr = &e1, _d_arrayappend(ptr, e2), *ptr;  */
    result = compound_expr (compound_expr (lexpr, ptr), result);
    this->result_ = compound_expr (result, build_deref (ptr));
  }

  /* Build an assignment expression.  The right operand is implicitly
     converted to the type of the left operand, and assigned to it.  */

  void visit (AssignExp *e)
  {
    /* First, handle special assignment semantics.  */

    /* Look for array.length = n;  */
    if (e->e1->op == TOKarraylength)
      {
	/* Assignment to an array's length property; resize the array.  */
	ArrayLengthExp *ale = (ArrayLengthExp *) e->e1;
	tree newlength = convert_expr (build_expr (e->e2), e->e2->type,
				       Type::tsize_t);
	tree ptr = build_address (build_expr (ale->e1));

	/* Don't want the basetype for the element type.  */
	Type *etype = ale->e1->type->toBasetype ()->nextOf ();
	libcall_fn libcall = etype->isZeroInit ()
	  ? LIBCALL_ARRAYSETLENGTHT : LIBCALL_ARRAYSETLENGTHIT;

	tree result = build_libcall (libcall, ale->e1->type, 3,
				     build_typeinfo (ale->loc, ale->e1->type),
				     newlength, ptr);

	this->result_ = d_array_length (result);
	return;
      }

    /* Look for array[] = n;  */
    if (e->e1->op == TOKslice)
      {
	SliceExp *se = (SliceExp *) e->e1;
	Type *stype = se->e1->type->toBasetype ();
	Type *etype = stype->nextOf ()->toBasetype ();

	/* Determine if we need to run postblit or dtor.  */
	bool postblit = this->needs_postblit (etype) && this->lvalue_p (e->e2);
	bool destructor = this->needs_dtor (etype);

	if (e->memset & blockAssign)
	  {
	    /* Set a range of elements to one value.  */
	    tree t1 = d_save_expr (build_expr (e->e1));
	    tree t2 = build_expr (e->e2);
	    tree result;

	    if ((postblit || destructor) && e->op != TOKblit)
	      {
		libcall_fn libcall = (e->op == TOKconstruct)
		  ? LIBCALL_ARRAYSETCTOR : LIBCALL_ARRAYSETASSIGN;
		/* So we can call postblits on const/immutable objects.  */
		Type *tm = etype->unSharedOf ()->mutableOf ();
		tree ti = build_typeinfo (e->loc, tm);

		tree result = build_libcall (libcall, Type::tvoid, 4,
					     d_array_ptr (t1),
					     build_address (t2),
					     d_array_length (t1), ti);
		this->result_ = compound_expr (result, t1);
		return;
	      }

	    if (integer_zerop (t2))
	      {
		tree tmemset = builtin_decl_explicit (BUILT_IN_MEMSET);
		tree size = size_mult_expr (d_array_length (t1),
					    size_int (etype->size ()));

		result = build_call_expr (tmemset, 3, d_array_ptr (t1),
					  integer_zero_node, size);
	      }
	    else
	      result = build_array_set (d_array_ptr (t1),
					d_array_length (t1), t2);

	    this->result_ = compound_expr (result, t1);
	  }
	else
	  {
	    /* Perform a memcpy operation.  */
	    gcc_assert (e->e2->type->ty != Tpointer);

	    if (!postblit && !destructor && !array_bounds_check ())
	      {
		tree t1 = d_save_expr (d_array_convert (e->e1));
		tree t2 = d_array_convert (e->e2);
		tree tmemcpy = builtin_decl_explicit (BUILT_IN_MEMCPY);
		tree size = size_mult_expr (d_array_length (t1),
					    size_int (etype->size ()));

		tree result = build_call_expr (tmemcpy, 3, d_array_ptr (t1),
					       d_array_ptr (t2), size);
		this->result_ = compound_expr (result, t1);
	      }
	    else if ((postblit || destructor) && e->op != TOKblit)
	      {
		/* Generate: _d_arrayassign(ti, from, to)
			 or: _d_arrayctor(ti, from, to)  */
		libcall_fn libcall = (e->op == TOKconstruct)
		  ? LIBCALL_ARRAYCTOR : LIBCALL_ARRAYASSIGN;

		this->result_ = build_libcall (libcall, e->type, 3,
					       build_typeinfo (e->loc, etype),
					       d_array_convert (e->e2),
					       d_array_convert (e->e1));
	      }
	    else
	      {
		/* Generate: _d_arraycopy()  */
		this->result_ = build_libcall (LIBCALL_ARRAYCOPY, e->type, 3,
					       size_int (etype->size ()),
					       d_array_convert (e->e2),
					       d_array_convert (e->e1));
	      }
	  }

	return;
      }

    /* Look for reference initializations.  */
    if (e->memset & referenceInit)
      {
	gcc_assert (e->op == TOKconstruct || e->op == TOKblit);
	gcc_assert (e->e1->op == TOKvar);

	Declaration *decl = ((VarExp *) e->e1)->var;
	if (decl->storage_class & (STCout | STCref))
	  {
	    tree t2 = convert_for_assignment (build_expr (e->e2),
					      e->e2->type, e->e1->type);
	    tree t1 = build_expr (e->e1);
	    /* Want reference to lhs, not indirect ref.  */
	    t1 = TREE_OPERAND (t1, 0);
	    t2 = build_address (t2);

	    this->result_ = indirect_ref (build_ctype (e->type),
					  build_assign (INIT_EXPR, t1, t2));
	    return;
	  }
      }

    /* Other types of assignments that may require post construction.  */
    Type *tb1 = e->e1->type->toBasetype ();
    tree_code modifycode = (e->op == TOKconstruct) ? INIT_EXPR : MODIFY_EXPR;

    /* Look for struct assignment.  */
    if (tb1->ty == Tstruct)
      {
	tree t1 = build_expr (e->e1);
	tree t2 = convert_for_assignment (build_expr (e->e2),
					  e->e2->type, e->e1->type);
	StructDeclaration *sd = ((TypeStruct *) tb1)->sym;

	/* Look for struct = 0.  */
	if (e->e2->op == TOKint64)
	  {
	    /* Use memset to fill struct.  */
	    gcc_assert (e->op == TOKblit);
	    tree tmemset = builtin_decl_explicit (BUILT_IN_MEMSET);
	    tree result = build_call_expr (tmemset, 3, build_address (t1),
					   t2, size_int (sd->structsize));

	    /* Maybe set-up hidden pointer to outer scope context.  */
	    if (sd->isNested ())
	      {
		tree field = get_symbol_decl (sd->vthis);
		tree value = build_vthis (sd);

		tree vthis_exp = modify_expr (component_ref (t1, field), value);
		result = compound_expr (result, vthis_exp);
	      }

	    this->result_ = compound_expr (result, t1);
	  }
	else
	  {
	    /* Simple struct literal assignment.  */
	    tree init = NULL_TREE;

	    /* Fill any alignment holes in the struct using memset.  */
	    if (e->op == TOKconstruct && !identity_compare_p (sd))
	      {
		tree tmemset = builtin_decl_explicit (BUILT_IN_MEMSET);
		init = build_call_expr (tmemset, 3, build_address (t1),
					integer_zero_node,
					size_int (sd->structsize));
	      }

	    tree result = build_assign (modifycode, t1, t2);
	    this->result_ = compound_expr (init, result);
	  }

	return;
      }

    /* Look for static array assignment.  */
    if (tb1->ty == Tsarray)
      {
	/* Look for array = 0.  */
	if (e->e2->op == TOKint64)
	  {
	    /* Use memset to fill the array.  */
	    gcc_assert (e->op == TOKblit);

	    tree t1 = build_expr (e->e1);
	    tree t2 = convert_for_assignment (build_expr (e->e2),
					      e->e2->type, e->e1->type);
	    tree size = size_int (e->e1->type->size ());

	    tree tmemset = builtin_decl_explicit (BUILT_IN_MEMSET);
	    this->result_ = build_call_expr (tmemset, 3, build_address (t1),
					     t2, size);
	    return;
	  }

	Type *etype = tb1->nextOf ();
	gcc_assert (e->e2->type->toBasetype ()->ty == Tsarray);

	/* Determine if we need to run postblit.  */
	bool postblit = this->needs_postblit (etype);
	bool destructor = this->needs_dtor (etype);
	bool lvalue_p = this->lvalue_p (e->e2);

	/* Even if the elements in rhs are all rvalues and don't have
	   to call postblits, this assignment should call dtors on old
	   assigned elements.  */
	if ((!postblit && !destructor)
	    || (e->op == TOKconstruct && !lvalue_p && postblit)
	    || (e->op == TOKblit || e->e1->type->size () == 0))
	  {
	    tree t1 = build_expr (e->e1);
	    tree t2 = convert_for_assignment (build_expr (e->e2),
					      e->e2->type, e->e1->type);

	    this->result_ = build_assign (modifycode, t1, t2);
	    return;
	  }

	Type *arrtype = (e->type->ty == Tsarray) ? etype->arrayOf () : e->type;
	tree result;

	if (e->op == TOKconstruct)
	  {
	    /* Generate: _d_arrayctor(ti, from, to)  */
	    result = build_libcall (LIBCALL_ARRAYCTOR, arrtype, 3,
				    build_typeinfo (e->loc, etype),
				    d_array_convert (e->e2),
				    d_array_convert (e->e1));
	  }
	else
	  {
	    /* Generate: _d_arrayassign_l()
		     or: _d_arrayassign_r()  */
	    libcall_fn libcall = (lvalue_p)
	      ? LIBCALL_ARRAYASSIGN_L : LIBCALL_ARRAYASSIGN_R;
	    tree elembuf = build_local_temp (build_ctype (etype));

	    result = build_libcall (libcall, arrtype, 4,
				    build_typeinfo (e->loc, etype),
				    d_array_convert (e->e2),
				    d_array_convert (e->e1),
				    build_address (elembuf));
	  }

	/* Cast the libcall result back to a static array.  */
	if (e->type->ty == Tsarray)
	  result = indirect_ref (build_ctype (e->type),
				 d_array_ptr (result));

	this->result_ = result;
	return;
      }

    /* Simple assignment.  */
    tree t1 = build_expr (e->e1);
    tree t2 = convert_for_assignment (build_expr (e->e2),
				      e->e2->type, e->e1->type);

    this->result_ = build_assign (modifycode, t1, t2);
  }

  /* Build a postfix expression.  */

  void visit (PostExp *e)
  {
    tree result;

    if (e->op == TOKplusplus)
      {
	result = build2 (POSTINCREMENT_EXPR, build_ctype (e->type),
			 build_expr (e->e1), build_expr (e->e2));
      }
    else if (e->op == TOKminusminus)
      {
	result = build2 (POSTDECREMENT_EXPR, build_ctype (e->type),
			 build_expr (e->e1), build_expr (e->e2));
      }
    else
      gcc_unreachable ();

    TREE_SIDE_EFFECTS (result) = 1;
    this->result_ = result;
  }

  /* Build an index expression.  */

  void visit (IndexExp *e)
  {
    Type *tb1 = e->e1->type->toBasetype ();

    if (tb1->ty == Taarray)
      {
	/* Get the key for the associative array.  */
	Type *tkey = ((TypeAArray *) tb1)->index->toBasetype ();
	tree key = convert_expr (build_expr (e->e2), e->e2->type, tkey);
	libcall_fn libcall;
	tree tinfo, ptr;

	if (e->modifiable)
	  {
	    libcall = LIBCALL_AAGETY;
	    ptr = build_address (build_expr (e->e1));
	    tinfo = build_typeinfo (e->loc, tb1->unSharedOf ()->mutableOf ());
	  }
	else
	  {
	    libcall = LIBCALL_AAGETRVALUEX;
	    ptr = build_expr (e->e1);
	    tinfo = build_typeinfo (e->loc, tkey);
	  }

	/* Index the associative array.  */
	tree result = build_libcall (libcall, e->type->pointerTo (), 4,
				     ptr, tinfo,
				     size_int (tb1->nextOf ()->size ()),
				     build_address (key));

	if (!e->indexIsInBounds && array_bounds_check ())
	  {
	    tree tassert = (global.params.checkAction == CHECKACTION_D)
	      ? d_assert_call (e->loc, LIBCALL_ARRAY_BOUNDS)
	      : build_call_expr (builtin_decl_explicit (BUILT_IN_TRAP), 0);

	    result = d_save_expr (result);
	    result = build_condition (TREE_TYPE (result),
				      d_truthvalue_conversion (result),
				      result, tassert);
	  }

	this->result_ = indirect_ref (build_ctype (e->type), result);
      }
    else
      {
	/* Get the data pointer and length for static and dynamic arrays.  */
	tree array = d_save_expr (build_expr (e->e1));
	tree ptr = convert_expr (array, tb1, tb1->nextOf ()->pointerTo ());

	tree length = NULL_TREE;
	if (tb1->ty != Tpointer)
	  length = get_array_length (array, tb1);
	else
	  gcc_assert (e->lengthVar == NULL);

	/* The __dollar variable just becomes a placeholder for the
	   actual length.  */
	if (e->lengthVar)
	  e->lengthVar->csym = length;

	/* Generate the index.  */
	tree index = build_expr (e->e2);

	/* If it's a static array and the index is constant, the front end has
	   already checked the bounds.  */
	if (tb1->ty != Tpointer && !e->indexIsInBounds)
	  index = build_bounds_condition (e->e2->loc, index, length, false);

	/* Index the .ptr.  */
	ptr = void_okay_p (ptr);
	this->result_ = indirect_ref (TREE_TYPE (TREE_TYPE (ptr)),
				      build_array_index (ptr, index));
      }
  }

  /* Build a comma expression.  The type is the type of the right operand.  */

  void visit (CommaExp *e)
  {
    tree t1 = build_expr (e->e1);
    tree t2 = build_expr (e->e2);
    tree type = e->type ? build_ctype (e->type) : void_type_node;

    this->result_ = build2 (COMPOUND_EXPR, type, t1, t2);
  }

  /* Build an array length expression.  Returns the number of elements
     in the array.  The result is of type size_t.  */

  void visit (ArrayLengthExp *e)
  {
    if (e->e1->type->toBasetype ()->ty == Tarray)
      this->result_ = d_array_length (build_expr (e->e1));
    else
      {
	/* Static arrays have already been handled by the front-end.  */
	error ("unexpected type for array length: %qs", e->type->toChars ());
	this->result_ = error_mark_node;
      }
  }

  /* Build a delegate pointer expression.  This will return the frame
     pointer value as a type void*.  */

  void visit (DelegatePtrExp *e)
  {
    tree t1 = build_expr (e->e1);
    this->result_ = delegate_object (t1);
  }

  /* Build a delegate function pointer expression.  This will return the
     function pointer value as a function type.  */

  void visit (DelegateFuncptrExp *e)
  {
    tree t1 = build_expr (e->e1);
    this->result_ = delegate_method (t1);
  }

  /* Build a slice expression.  */

  void visit (SliceExp *e)
  {
    Type *tb = e->type->toBasetype ();
    Type *tb1 = e->e1->type->toBasetype ();
    gcc_assert (tb->ty == Tarray || tb->ty == Tsarray);

    /* Use convert-to-dynamic-array code if possible.  */
    if (!e->lwr)
      {
	tree result = build_expr (e->e1);
	if (e->e1->type->toBasetype ()->ty == Tsarray)
	  result = convert_expr (result, e->e1->type, e->type);

	this->result_ = result;
	return;
      }
    else
      gcc_assert (e->upr != NULL);

    /* Get the data pointer and length for static and dynamic arrays.  */
    tree array = d_save_expr (build_expr (e->e1));
    tree ptr = convert_expr (array, tb1, tb1->nextOf ()->pointerTo ());
    tree length = NULL_TREE;

    /* Our array is already a SAVE_EXPR if necessary, so we don't make length
       a SAVE_EXPR which is, at most, a COMPONENT_REF on top of array.  */
    if (tb1->ty != Tpointer)
      length = get_array_length (array, tb1);
    else
      gcc_assert (e->lengthVar == NULL);

    /* The __dollar variable just becomes a placeholder for the
       actual length.  */
    if (e->lengthVar)
      e->lengthVar->csym = length;

    /* Generate upper and lower bounds.  */
    tree lwr_tree = d_save_expr (build_expr (e->lwr));
    tree upr_tree = d_save_expr (build_expr (e->upr));

    /* If the upper bound has any side effects, then the lower bound should be
       copied to a temporary always.  */
    if (TREE_CODE (upr_tree) == SAVE_EXPR && TREE_CODE (lwr_tree) != SAVE_EXPR)
      lwr_tree = save_expr (lwr_tree);

    /* Adjust the .ptr offset.  */
    if (!integer_zerop (lwr_tree))
      {
	tree ptrtype = TREE_TYPE (ptr);
	ptr = build_array_index (void_okay_p (ptr), lwr_tree);
	ptr = build_nop (ptrtype, ptr);
      }
    else
      lwr_tree = NULL_TREE;

    /* Nothing more to do for static arrays, their bounds checking has been
       done at compile-time.  */
    if (tb->ty == Tsarray)
      {
	this->result_ = indirect_ref (build_ctype (e->type), ptr);
	return;
      }
    else
      gcc_assert (tb->ty == Tarray);

    /* Generate bounds checking code.  */
    tree newlength;

    if (!e->upperIsInBounds)
      {
	if (length)
	  {
	    newlength = build_bounds_condition (e->upr->loc, upr_tree,
						length, true);
	  }
	else
	  {
	    /* Still need to check bounds lwr <= upr for pointers.  */
	    gcc_assert (tb1->ty == Tpointer);
	    newlength = upr_tree;
	  }
      }
    else
      newlength = upr_tree;

    if (lwr_tree)
      {
	/* Enforces lwr <= upr.  No need to check lwr <= length as
	   we've already ensured that upr <= length.  */
	if (!e->lowerIsLessThanUpper)
	  {
	    tree cond = build_bounds_condition (e->lwr->loc, lwr_tree,
						upr_tree, true);

	    /* When bounds checking is off, the index value is
	       returned directly.  */
	    if (cond != lwr_tree)
	      newlength = compound_expr (cond, newlength);
	  }

	/* Need to ensure lwr always gets evaluated first, as it may be a
	   function call.  Generates (lwr, upr) - lwr.  */
	newlength = fold_build2 (MINUS_EXPR, TREE_TYPE (newlength),
				 compound_expr (lwr_tree, newlength), lwr_tree);
      }

    tree result = d_array_value (build_ctype (e->type), newlength, ptr);
    this->result_ = compound_expr (array, result);
  }

  /* Build a cast expression, which converts the given unary expression to the
     type of result.  */

  void visit (CastExp *e)
  {
    Type *ebtype = e->e1->type->toBasetype ();
    Type *tbtype = e->to->toBasetype ();
    tree result = build_expr (e->e1, this->constp_);

    /* Just evaluate e1 if it has any side effects.  */
    if (tbtype->ty == Tvoid)
      this->result_ = build_nop (build_ctype (tbtype), result);
    else
      this->result_ = convert_expr (result, ebtype, tbtype);
  }

  /* Build a delete expression.  */

  void visit (DeleteExp *e)
  {
    tree t1 = build_expr (e->e1);
    Type *tb1 = e->e1->type->toBasetype ();

    if (tb1->ty == Tclass)
      {
	/* For class object references, if there is a destructor for that class,
	   the destructor is called for the object instance.  */
	libcall_fn libcall;

	if (e->e1->op == TOKvar)
	  {
	    VarDeclaration *v = ((VarExp *) e->e1)->var->isVarDeclaration ();
	    if (v && v->onstack)
	      {
		libcall = tb1->isClassHandle ()->isInterfaceDeclaration ()
		  ? LIBCALL_CALLINTERFACEFINALIZER : LIBCALL_CALLFINALIZER;

		this->result_ = build_libcall (libcall, Type::tvoid, 1, t1);
		return;
	      }
	  }

	/* Otherwise, the garbage collector is called to immediately free the
	   memory allocated for the class instance.  */
	libcall = tb1->isClassHandle ()->isInterfaceDeclaration ()
	  ? LIBCALL_DELINTERFACE : LIBCALL_DELCLASS;

	t1 = build_address (t1);
	this->result_ = build_libcall (libcall, Type::tvoid, 1, t1);
      }
    else if (tb1->ty == Tarray)
      {
	/* For dynamic arrays, the garbage collector is called to immediately
	   release the memory.  */
	Type *telem = tb1->nextOf ()->baseElemOf ();
	tree ti = null_pointer_node;

	if (telem->ty == Tstruct)
	  {
	    /* Might need to run destructor on array contents.  */
	    TypeStruct *ts = (TypeStruct *) telem;
	    if (ts->sym->dtor)
	      ti = build_typeinfo (e->loc, tb1->nextOf ());
	  }

	/* Generate: _delarray_t (&t1, ti);  */
	this->result_ = build_libcall (LIBCALL_DELARRAYT, Type::tvoid, 2,
				       build_address (t1), ti);
      }
    else if (tb1->ty == Tpointer)
      {
	/* For pointers to a struct instance, if the struct has overloaded
	   operator delete, then that operator is called.  */
	t1 = build_address (t1);
	Type *tnext = ((TypePointer *)tb1)->next->toBasetype ();

	if (tnext->ty == Tstruct)
	  {
	    TypeStruct *ts = (TypeStruct *)tnext;
	    if (ts->sym->dtor)
	      {
		tree ti = build_typeinfo (e->loc, tnext);
		this->result_ = build_libcall (LIBCALL_DELSTRUCT, Type::tvoid,
					       2, t1, ti);
		return;
	      }
	  }

	/* Otherwise, the garbage collector is called to immediately free the
	   memory allocated for the pointer.  */
	this->result_ = build_libcall (LIBCALL_DELMEMORY, Type::tvoid, 1, t1);
      }
    else
      {
	error ("don%'t know how to delete %qs", e->e1->toChars ());
	this->result_ = error_mark_node;
      }
  }

  /* Build a remove expression, which removes a particular key from an
     associative array.  */

  void visit (RemoveExp *e)
  {
    /* Check that the array is actually an associative array.  */
    if (e->e1->type->toBasetype ()->ty == Taarray)
      {
	Type *tb = e->e1->type->toBasetype ();
	Type *tkey = ((TypeAArray *) tb)->index->toBasetype ();
	tree index = convert_expr (build_expr (e->e2), e->e2->type, tkey);

	this->result_ = build_libcall (LIBCALL_AADELX, Type::tbool, 3,
				       build_expr (e->e1),
				       build_typeinfo (e->loc, tkey),
				       build_address (index));
      }
    else
      {
	error ("%qs is not an associative array", e->e1->toChars ());
	this->result_ = error_mark_node;
      }
  }

  /* Build an unary not expression.  */

  void visit (NotExp *e)
  {
    tree result = convert_for_condition (build_expr (e->e1), e->e1->type);
    /* Need to convert to boolean type or this will fail.  */
    result = fold_build1 (TRUTH_NOT_EXPR, d_bool_type, result);

    this->result_ = d_convert (build_ctype (e->type), result);
  }

  /* Build a compliment expression, where all the bits in the value are
     complemented.  Note: unlike in C, the usual integral promotions
     are not performed prior to the complement operation.  */

  void visit (ComExp *e)
  {
    TY ty1 = e->e1->type->toBasetype ()->ty;
    gcc_assert (ty1 != Tarray && ty1 != Tsarray);

    this->result_ = fold_build1 (BIT_NOT_EXPR, build_ctype (e->type),
				 build_expr (e->e1));
  }

  /* Build an unary negation expression.  */

  void visit (NegExp *e)
  {
    TY ty1 = e->e1->type->toBasetype ()->ty;
    gcc_assert (ty1 != Tarray && ty1 != Tsarray);

    tree type = build_ctype (e->type);
    tree expr = build_expr (e->e1);

    /* If the operation needs excess precision.  */
    tree eptype = excess_precision_type (type);
    if (eptype != NULL_TREE)
      expr = d_convert (eptype, expr);
    else
      eptype = type;

    tree ret = fold_build1 (NEGATE_EXPR, eptype, expr);
    this->result_ = d_convert (type, ret);
  }

  /* Build a pointer index expression.  */

  void visit (PtrExp *e)
  {
    Type *tnext = NULL;
    size_t offset;
    tree result;

    if (e->e1->op == TOKadd)
      {
	BinExp *be = (BinExp *) e->e1;
	if (be->e1->op == TOKaddress
	    && be->e2->isConst () && be->e2->type->isintegral ())
	  {
	    Expression *ae = ((AddrExp *) be->e1)->e1;
	    tnext = ae->type->toBasetype ();
	    result = build_expr (ae);
	    offset = be->e2->toUInteger ();
	  }
      }
    else if (e->e1->op == TOKsymoff)
      {
	SymOffExp *se = (SymOffExp *) e->e1;
	if (!declaration_reference_p (se->var))
	  {
	    tnext = se->var->type->toBasetype ();
	    result = get_decl_tree (se->var);
	    offset = se->offset;
	  }
      }

    /* Produce better code by converting *(#record + n) to
       COMPONENT_REFERENCE.  Otherwise, the variable will always be
       allocated in memory because its address is taken.  */
    if (tnext && tnext->ty == Tstruct)
      {
	StructDeclaration *sd = ((TypeStruct *) tnext)->sym;

	for (size_t i = 0; i < sd->fields.dim; i++)
	  {
	    VarDeclaration *field = sd->fields[i];

	    if (field->offset == offset
		&& same_type_p (field->type, e->type))
	      {
		/* Catch errors, backend will ICE otherwise.  */
		if (error_operand_p (result))
		  this->result_ = result;
		else
		  {
		    result  = component_ref (result, get_symbol_decl (field));
		    this->result_ = result;
		  }
		return;
	      }
	    else if (field->offset > offset)
	      break;
	  }
      }

    this->result_ = indirect_ref (build_ctype (e->type), build_expr (e->e1));
  }

  /* Build an unary address expression.  */

  void visit (AddrExp *e)
  {
    tree type = build_ctype (e->type);
    tree exp;

    /* The frontend optimizer can convert const symbol into a struct literal.
       Taking the address of a struct literal is otherwise illegal.  */
    if (e->e1->op == TOKstructliteral)
      {
	StructLiteralExp *sle = ((StructLiteralExp *) e->e1)->origin;
	gcc_assert (sle != NULL);

	/* Build the reference symbol, the decl is built first as the
	   initializer may have recursive references.  */
	if (!sle->sym)
	  {
	    sle->sym = build_artificial_decl (build_ctype (sle->type),
					      NULL_TREE, "S");
	    DECL_INITIAL (sle->sym) = build_expr (sle, true);
	    d_pushdecl (sle->sym);
	    rest_of_decl_compilation (sle->sym, 1, 0);
	  }

	exp = sle->sym;
      }
    else
      exp = build_expr (e->e1, this->constp_);

    TREE_CONSTANT (exp) = 0;
    this->result_ = d_convert (type, build_address (exp));
  }

  /* Build a function call expression.  */

  void visit (CallExp *e)
  {
    Type *tb = e->e1->type->toBasetype ();
    Expression *e1b = e->e1;

    tree callee = NULL_TREE;
    tree object = NULL_TREE;
    tree cleanup = NULL_TREE;
    TypeFunction *tf = NULL;

    /* Calls to delegates can sometimes look like this.  */
    if (e1b->op == TOKcomma)
      {
	e1b = ((CommaExp *) e1b)->e2;
	gcc_assert (e1b->op == TOKvar);

	Declaration *var = ((VarExp *) e1b)->var;
	gcc_assert (var->isFuncDeclaration () && !var->needThis ());
      }

    if (e1b->op == TOKdotvar && tb->ty != Tdelegate)
      {
	DotVarExp *dve = (DotVarExp *) e1b;

	/* Don't modify the static initializer for struct literals.  */
	if (dve->e1->op == TOKstructliteral)
	  {
	    StructLiteralExp *sle = (StructLiteralExp *) dve->e1;
	    sle->useStaticInit = false;
	  }

	FuncDeclaration *fd = dve->var->isFuncDeclaration ();
	if (fd != NULL)
	  {
	    /* Get the correct callee from the DotVarExp object.  */
	    tree fndecl = get_symbol_decl (fd);
	    AggregateDeclaration *ad = fd->isThis ();

	    /* Static method; ignore the object instance.  */
	    if (!ad)
	      callee = build_address (fndecl);
	    else
	      {
		tree thisexp = build_expr (dve->e1);

		/* When constructing temporaries, if the constructor throws,
		   then the object is destructed even though it is not a fully
		   constructed object yet.  And so this call will need to be
		   moved inside the TARGET_EXPR_INITIAL slot.  */
		if (fd->isCtorDeclaration ()
		    && TREE_CODE (thisexp) == COMPOUND_EXPR
		    && TREE_CODE (TREE_OPERAND (thisexp, 0)) == TARGET_EXPR
		    && TARGET_EXPR_CLEANUP (TREE_OPERAND (thisexp, 0)))
		  {
		    cleanup = TREE_OPERAND (thisexp, 0);
		    thisexp = TREE_OPERAND (thisexp, 1);
		  }

		/* Want reference to 'this' object.  */
		if (!POINTER_TYPE_P (TREE_TYPE (thisexp)))
		  thisexp = build_address (thisexp);

		/* Make the callee a virtual call.  */
		if (fd->isVirtual () && !fd->isFinalFunc () && !e->directcall)
		  {
		    tree fntype = build_pointer_type (TREE_TYPE (fndecl));
		    tree thistype = build_ctype (ad->handleType ());
		    thisexp = build_nop (thistype, d_save_expr (thisexp));
		    fndecl = build_vindex_ref (thisexp, fntype, fd->vtblIndex);
		  }
		else
		  fndecl = build_address (fndecl);

		callee = build_method_call (fndecl, thisexp, fd->type);
	      }
	  }
      }

    if (callee == NULL_TREE)
      callee = build_expr (e1b);

    if (METHOD_CALL_EXPR (callee))
      {
	/* This could be a delegate expression (TY == Tdelegate), but not
	   actually a delegate variable.  */
	if (e1b->op == TOKdotvar)
	  {
	    /* This gets the true function type, getting the function type
	       from e1->type can sometimes be incorrect, such as when calling
	       a 'ref' return function.  */
	    tf = get_function_type (((DotVarExp *) e1b)->var->type);
	  }
	else
	  tf = get_function_type (tb);

	extract_from_method_call (callee, callee, object);
      }
    else if (tb->ty == Tdelegate)
      {
	/* Delegate call, extract .object and .funcptr from var.  */
	callee = d_save_expr (callee);
	tf = get_function_type (tb);
	object = delegate_object (callee);
	callee = delegate_method (callee);
      }
    else if (e1b->op == TOKvar)
      {
	FuncDeclaration *fd = ((VarExp *) e1b)->var->isFuncDeclaration ();
	gcc_assert (fd != NULL);
	tf = get_function_type (fd->type);

	if (fd->isNested ())
	  {
	    /* Maybe re-evaluate symbol storage treating 'fd' as public.  */
	    if (call_by_alias_p (d_function_chain->function, fd))
	      TREE_PUBLIC (callee) = 1;

	    object = get_frame_for_symbol (fd);
	  }
	else if (fd->needThis ())
	  {
	    error_at (make_location_t (e1b->loc),
		      "need %<this%> to access member %qs", fd->toChars ());
	    /* Continue compiling...  */
	    object = null_pointer_node;
	  }
      }
    else
      {
	/* Normal direct function call.  */
	tf = get_function_type (tb);
      }

    gcc_assert (tf != NULL);

    /* Now we have the type, callee and maybe object reference,
       build the call expression.  */
    tree exp = d_build_call (tf, callee, object, e->arguments);

    if (tf->isref)
      exp = build_deref (exp);

    /* Some library calls are defined to return a generic type.
       this->type is the real type we want to return.  */
    if (e->type->isTypeBasic ())
      exp = d_convert (build_ctype (e->type), exp);

    /* If this call was found to be a constructor for a temporary with a
       cleanup, then move the call inside the TARGET_EXPR.  */
    if (cleanup != NULL_TREE)
      {
	tree init = TARGET_EXPR_INITIAL (cleanup);
	TARGET_EXPR_INITIAL (cleanup) = compound_expr (init, exp);
	exp = cleanup;
      }

    this->result_ = exp;
  }

  /* Build a delegate expression.  */

  void visit (DelegateExp *e)
  {
    if (e->func->semanticRun == PASSsemantic3done)
      {
	/* Add the function as nested function if it belongs to this module.
	   ie: it is a member of this module, or it is a template instance.  */
	Dsymbol *owner = e->func->toParent ();
	while (!owner->isTemplateInstance () && owner->toParent ())
	  owner = owner->toParent ();
	if (owner->isTemplateInstance () || owner == d_function_chain->module)
	  build_decl_tree (e->func);
      }

    tree fndecl;
    tree object;

    if (e->func->isNested ())
      {
	if (e->e1->op == TOKnull)
	  object = build_expr (e->e1);
	else
	  object = get_frame_for_symbol (e->func);

	fndecl = build_address (get_symbol_decl (e->func));
      }
    else
      {
	if (!e->func->isThis ())
	  {
	    error ("delegates are only for non-static functions");
	    this->result_ = error_mark_node;
	    return;
	  }

	object = build_expr (e->e1);

	/* Want reference to `this' object.  */
	if (e->e1->type->ty != Tclass && e->e1->type->ty != Tpointer)
	  object = build_address (object);

	/* Object reference could be the outer `this' field of a class or
	   closure of type `void*'.  Cast it to the right type.  */
	if (e->e1->type->ty == Tclass)
	  object = d_convert (build_ctype (e->e1->type), object);

	fndecl = get_symbol_decl (e->func);

	/* Get pointer to function out of the virtual table.  */
	if (e->func->isVirtual () && !e->func->isFinalFunc ()
	    && e->e1->op != TOKsuper && e->e1->op != TOKdottype)
	  {
	    tree fntype = build_pointer_type (TREE_TYPE (fndecl));
	    object = d_save_expr (object);
	    fndecl = build_vindex_ref (object, fntype, e->func->vtblIndex);
	  }
	else
	  fndecl = build_address (fndecl);
      }

    this->result_ = build_method_call (fndecl, object, e->type);
  }

  /* Build a type component expression.  */

  void visit (DotTypeExp *e)
  {
    /* Just a pass through to underlying expression.  */
    this->result_ = build_expr (e->e1);
  }

  /* Build a component reference expression.  */

  void visit (DotVarExp *e)
  {
    VarDeclaration *vd = e->var->isVarDeclaration ();

    /* This could also be a function, but relying on that being taken
       care of by the visitor interface for CallExp.  */
    if (vd != NULL)
      {
	if (!vd->isField ())
	  this->result_ = get_decl_tree (vd);
	else
	  {
	    tree object = build_expr (e->e1);

	    if (e->e1->type->toBasetype ()->ty != Tstruct)
	      object = build_deref (object);

	    this->result_ = component_ref (object, get_symbol_decl (vd));
	  }
      }
    else
      {
	error ("%qs is not a field, but a %qs",
	       e->var->toChars (), e->var->kind ());
	this->result_ = error_mark_node;
      }
  }

  /* Build an assert expression, used to declare conditions that must hold at
     that a given point in the program.  */

  void visit (AssertExp *e)
  {
    Type *tb1 = e->e1->type->toBasetype ();
    tree arg = build_expr (e->e1);
    tree tmsg = NULL_TREE;
    tree assert_pass = void_node;
    tree assert_fail;

    if (global.params.useAssert
	&& global.params.checkAction == CHECKACTION_D)
      {
	/* Generate: ((bool) e1  ? (void)0 : _d_assert (...))
		 or: (e1 != null ? e1._invariant() : _d_assert (...))  */
	bool unittest_p = d_function_chain->function->isUnitTestDeclaration ();
	libcall_fn libcall;

	if (e->msg)
	  {
	    tmsg = build_expr_dtor (e->msg);
	    libcall = unittest_p ? LIBCALL_UNITTEST_MSG : LIBCALL_ASSERT_MSG;
	  }
	else
	  libcall = unittest_p ? LIBCALL_UNITTEST : LIBCALL_ASSERT;

	/* Build a call to _d_assert().  */
	assert_fail = d_assert_call (e->loc, libcall, tmsg);

	if (global.params.useInvariants)
	  {
	    /* If the condition is a D class or struct object with an invariant,
	       call it if the condition result is true.  */
	    if (tb1->ty == Tclass)
	      {
		ClassDeclaration *cd = tb1->isClassHandle ();
		if (!cd->isInterfaceDeclaration () && !cd->isCPPclass ())
		  {
		    arg = d_save_expr (arg);
		    assert_pass = build_libcall (LIBCALL_INVARIANT,
						 Type::tvoid, 1, arg);
		  }
	      }
	    else if (tb1->ty == Tpointer && tb1->nextOf ()->ty == Tstruct)
	      {
		StructDeclaration *sd = ((TypeStruct *) tb1->nextOf ())->sym;
		if (sd->inv != NULL)
		  {
		    Expressions args;
		    arg = d_save_expr (arg);
		    assert_pass = d_build_call_expr (sd->inv, arg, &args);
		  }
	      }
	  }
      }
    else if (global.params.useAssert
	     && global.params.checkAction == CHECKACTION_C)
      {
	/* Generate: __builtin_trap()  */
	tree fn = builtin_decl_explicit (BUILT_IN_TRAP);
	assert_fail = build_call_expr (fn, 0);
      }
    else
      {
	/* Assert contracts are turned off, if the contract condition has no
	   side effects can still use it as a predicate for the optimizer.  */
	if (TREE_SIDE_EFFECTS (arg))
	  {
	    this->result_ = void_node;
	    return;
	  }

	assert_fail = build_predict_expr (PRED_NORETURN, NOT_TAKEN);
      }

    /* Build condition that we are asserting in this contract.  */
    tree condition = convert_for_condition (arg, e->e1->type);

    /* We expect the condition to always be true, as what happens if an assert
       contract is false is undefined behavior.  */
    tree fn = builtin_decl_explicit (BUILT_IN_EXPECT);
    tree arg_types = TYPE_ARG_TYPES (TREE_TYPE (fn));
    tree pred_type = TREE_VALUE (arg_types);
    tree expected_type = TREE_VALUE (TREE_CHAIN (arg_types));

    condition = build_call_expr (fn, 2, d_convert (pred_type, condition),
				 build_int_cst (expected_type, 1));
    condition = d_truthvalue_conversion (condition);

    this->result_ = build_vcondition (condition, assert_pass, assert_fail);
  }

  /* Build a declaration expression.  */

  void visit (DeclarationExp *e)
  {
    /* Compile the declaration.  */
    push_stmt_list ();
    build_decl_tree (e->declaration);
    tree result = pop_stmt_list ();

    /* Construction of an array for typesafe-variadic function arguments
       can cause an empty STMT_LIST here.  This can causes problems
       during gimplification.  */
    if (TREE_CODE (result) == STATEMENT_LIST && !STATEMENT_LIST_HEAD (result))
      result = build_empty_stmt (input_location);

    this->result_ = result;
  }

  /* Build a typeid expression.  Returns an instance of class TypeInfo
     corresponding to.  */

  void visit (TypeidExp *e)
  {
    if (Type *tid = isType (e->obj))
      {
	tree ti = build_typeinfo (e->loc, tid);

	/* If the typeinfo is at an offset.  */
	if (tid->vtinfo->offset)
	  ti = build_offset (ti, size_int (tid->vtinfo->offset));

	this->result_ = build_nop (build_ctype (e->type), ti);
      }
    else if (Expression *tid = isExpression (e->obj))
      {
	Type *type = tid->type->toBasetype ();
	assert (type->ty == Tclass);

	/* Generate **classptr to get the classinfo.  */
	tree ci = build_expr (tid);
	ci = indirect_ref (ptr_type_node, ci);
	ci = indirect_ref (ptr_type_node, ci);

	/* Add extra indirection for interfaces.  */
	if (((TypeClass *) type)->sym->isInterfaceDeclaration ())
	  ci = indirect_ref (ptr_type_node, ci);

	this->result_ = build_nop (build_ctype (e->type), ci);
      }
    else
      gcc_unreachable ();
  }

  /* Build a function/lambda expression.  */

  void visit (FuncExp *e)
  {
    Type *ftype = e->type->toBasetype ();

    /* This check is for lambda's, remove 'vthis' as function isn't nested.  */
    if (e->fd->tok == TOKreserved && ftype->ty == Tpointer)
      {
	e->fd->tok = TOKfunction;
	e->fd->vthis = NULL;
      }

    /* Compile the function literal body.  */
    build_decl_tree (e->fd);

    /* If nested, this will be a trampoline.  */
    if (e->fd->isNested ())
      {
	tree func = build_address (get_symbol_decl (e->fd));
	tree object;

	if (this->constp_)
	  {
	    /* Static delegate variables have no context pointer.  */
	    object = null_pointer_node;
	    this->result_ = build_method_call (func, object, e->fd->type);
	    TREE_CONSTANT (this->result_) = 1;
	  }
	else
	  {
	    object = get_frame_for_symbol (e->fd);
	    this->result_ = build_method_call (func, object, e->fd->type);
	  }
      }
    else
      {
	this->result_ = build_nop (build_ctype (e->type),
				   build_address (get_symbol_decl (e->fd)));
      }
  }

  /* Build a halt expression.  */

  void visit (HaltExp *)
  {
    /* Should we use trap() or abort()?  */
    tree ttrap = builtin_decl_explicit (BUILT_IN_TRAP);
    this->result_ = build_call_expr (ttrap, 0);
  }

  /* Build a symbol pointer offset expression.  */

  void visit (SymOffExp *e)
  {
    /* Build the address and offset of the symbol.  */
    size_t soffset = ((SymOffExp *) e)->offset;
    tree result = get_decl_tree (e->var);
    TREE_USED (result) = 1;

    if (declaration_reference_p (e->var))
      gcc_assert (POINTER_TYPE_P (TREE_TYPE (result)));
    else
      result = build_address (result);

    if (!soffset)
      result = d_convert (build_ctype (e->type), result);
    else
      {
	tree offset = size_int (soffset);
	result = build_nop (build_ctype (e->type),
			    build_offset (result, offset));
      }

    this->result_ = result;
  }

  /* Build a variable expression.  */

  void visit (VarExp *e)
  {
    if (e->var->needThis ())
      {
	error ("need %<this%> to access member %qs", e->var->ident->toChars ());
	this->result_ = error_mark_node;
	return;
      }
    else if (e->var->ident == Identifier::idPool ("__ctfe"))
      {
	/* __ctfe is always false at run-time.  */
	this->result_ = integer_zero_node;
	return;
      }

    /* This check is same as is done in FuncExp for lambdas.  */
    FuncLiteralDeclaration *fld = e->var->isFuncLiteralDeclaration ();
    if (fld != NULL)
      {
	if (fld->tok == TOKreserved)
	  {
	    fld->tok = TOKfunction;
	    fld->vthis = NULL;
	  }

	/* Compiler the function literal body.  */
	build_decl_tree (fld);
      }

    if (this->constp_)
      {
	/* Want the initializer, not the expression.  */
	VarDeclaration *var = e->var->isVarDeclaration ();
	SymbolDeclaration *sd = e->var->isSymbolDeclaration ();
	tree init = NULL_TREE;

	if (var && (var->isConst () || var->isImmutable ())
	    && e->type->toBasetype ()->ty != Tsarray && var->_init)
	  {
	    if (var->inuse)
	      error_at (make_location_t (e->loc), "recursive reference %qs",
			e->toChars ());
	    else
	      {
		var->inuse++;
		init = build_expr (initializerToExpression (var->_init), true);
		var->inuse--;
	      }
	  }
	else if (sd && sd->dsym)
	  init = layout_struct_initializer (sd->dsym);
	else
	  error_at (make_location_t (e->loc), "non-constant expression %qs",
		    e->toChars ());

	if (init != NULL_TREE)
	  this->result_ = init;
	else
	  this->result_ = error_mark_node;
      }
    else
      {
	tree result = get_decl_tree (e->var);
	TREE_USED (result) = 1;

	/* For variables that are references - currently only out/inout
	   arguments; objects don't count - evaluating the variable means
	   we want what it refers to.  */
	if (declaration_reference_p (e->var))
	  result = indirect_ref (build_ctype (e->var->type), result);

	this->result_ = result;
      }
  }

  /* Build a this variable expression.  */

  void visit (ThisExp *e)
  {
    FuncDeclaration *fd = d_function_chain ? d_function_chain->function : NULL;
    tree result = NULL_TREE;

    if (e->var)
      result = get_decl_tree (e->var);
    else
      {
	gcc_assert (fd && fd->vthis);
	result = get_decl_tree (fd->vthis);
      }

    if (e->type->ty == Tstruct)
      result = build_deref (result);

    this->result_ = result;
  }

  /* Build a new expression, which allocates memory either on the garbage
     collected heap or by using a class or struct specific allocator.  */

  void visit (NewExp *e)
  {
    Type *tb = e->type->toBasetype ();
    tree result;

    if (e->allocator)
      gcc_assert (e->newargs);

    if (tb->ty == Tclass)
      {
	/* Allocating a new class.  */
	tb = e->newtype->toBasetype ();
	gcc_assert (tb->ty == Tclass);

	ClassDeclaration *cd = ((TypeClass *) tb)->sym;
	tree type = build_ctype (tb);
	tree setup_exp = NULL_TREE;
	tree new_call;

	if (e->onstack)
	  {
	    /* If being used as an initializer for a local variable with scope
	       storage class, then the instance is allocated on the stack
	       rather than the heap or using the class specific allocator.  */
	    tree var = build_local_temp (TREE_TYPE (type));
	    new_call = build_nop (type, build_address (var));
	    setup_exp = modify_expr (var, aggregate_initializer_decl (cd));
	  }
	else if (e->allocator)
	  {
	    /* Call class allocator, and copy the initializer into memory.  */
	    new_call = d_build_call_expr (e->allocator, NULL_TREE, e->newargs);
	    new_call = d_save_expr (new_call);
	    new_call = build_nop (type, new_call);
	    setup_exp = modify_expr (build_deref (new_call),
				     aggregate_initializer_decl (cd));
	  }
	else
	  {
	    /* Generate: _d_newclass()  */
	    tree arg = build_address (get_classinfo_decl (cd));
	    new_call = build_libcall (LIBCALL_NEWCLASS, tb, 1, arg);
	  }

	/* Set the context pointer for nested classes.  */
	if (cd->isNested ())
	  {
	    tree field = get_symbol_decl (cd->vthis);
	    tree value = NULL_TREE;

	    if (e->thisexp)
	      {
		ClassDeclaration *tcd = e->thisexp->type->isClassHandle ();
		Dsymbol *outer = cd->toParent2 ();
		int offset = 0;

		value = build_expr (e->thisexp);
		if (outer != tcd)
		  {
		    ClassDeclaration *ocd = outer->isClassDeclaration ();
		    gcc_assert (ocd->isBaseOf (tcd, &offset));
		    /* Could just add offset...  */
		    value = convert_expr (value, e->thisexp->type, ocd->type);
		  }
	      }
	    else
	      value = build_vthis (cd);

	    if (value != NULL_TREE)
	      {
		/* Generate: (new())->vthis = this;  */
		new_call = d_save_expr (new_call);
		field = component_ref (build_deref (new_call), field);
		setup_exp = compound_expr (setup_exp,
					   modify_expr (field, value));
	      }
	  }
	new_call = compound_expr (setup_exp, new_call);

	/* Call the class constructor.  */
	if (e->member)
	  result = d_build_call_expr (e->member, new_call, e->arguments);
	else
	  result = new_call;

	if (e->argprefix)
	  result = compound_expr (build_expr (e->argprefix), result);
      }
    else if (tb->ty == Tpointer && tb->nextOf ()->toBasetype ()->ty == Tstruct)
      {
	/* Allocating memory for a new struct.  */
	Type *htype = e->newtype->toBasetype ();
	gcc_assert (htype->ty == Tstruct);
	gcc_assert (!e->onstack);

	TypeStruct *stype = (TypeStruct *) htype;
	StructDeclaration *sd = stype->sym;
	tree new_call;

	/* Cannot new an opaque struct.  */
	if (sd->size (e->loc) == 0)
	  {
	    this->result_ = d_convert (build_ctype (e->type),
				       integer_zero_node);
	    return;
	  }

	if (e->allocator)
	  {
	    /* Call struct allocator.  */
	    new_call = d_build_call_expr (e->allocator, NULL_TREE, e->newargs);
	    new_call = build_nop (build_ctype (tb), new_call);
	  }
	else
	  {
	    /* Generate: _d_newitemT()  */
	    libcall_fn libcall = htype->isZeroInit ()
	      ? LIBCALL_NEWITEMT : LIBCALL_NEWITEMIT;
	    tree arg = build_typeinfo (e->loc, e->newtype);
	    new_call = build_libcall (libcall, tb, 1, arg);
	  }

	if (e->member || !e->arguments)
	  {
	    /* Set the context pointer for nested structs.  */
	    if (sd->isNested ())
	      {
		tree value = build_vthis (sd);
		tree field = get_symbol_decl (sd->vthis);
		tree type = build_ctype (stype);

		new_call = d_save_expr (new_call);
		field = component_ref (indirect_ref (type, new_call), field);
		new_call = compound_expr (modify_expr (field, value), new_call);
	      }

	    /* Call the struct constructor.  */
	    if (e->member)
	      result = d_build_call_expr (e->member, new_call, e->arguments);
	    else
	      result = new_call;
	  }
	else
	  {
	    /* If we have a user supplied initializer, then set-up with a
	       struct literal.  */
	    if (e->arguments != NULL && sd->fields.dim != 0)
	      {
		StructLiteralExp *se = StructLiteralExp::create (e->loc, sd,
								 e->arguments,
								 htype);
		new_call = d_save_expr (new_call);
		se->type = sd->type;
		se->sym = new_call;
		result = compound_expr (build_expr (se), new_call);
	      }
	    else
	      result = new_call;
	  }

	if (e->argprefix)
	  result = compound_expr (build_expr (e->argprefix), result);
      }
    else if (tb->ty == Tarray)
      {
	/* Allocating memory for a new D array.  */
	tb = e->newtype->toBasetype ();
	gcc_assert (tb->ty == Tarray);
	TypeDArray *tarray = (TypeDArray *) tb;

	gcc_assert (!e->allocator);
	gcc_assert (e->arguments && e->arguments->dim >= 1);

	if (e->arguments->dim == 1)
	  {
	    /* Single dimension array allocations.  */
	    Expression *arg = (*e->arguments)[0];

	    if (tarray->next->size () == 0)
	      {
		/* Array element size is unknown.  */
		this->result_ = d_array_value (build_ctype (e->type),
					       size_int (0), null_pointer_node);
		return;
	      }

	    libcall_fn libcall = tarray->next->isZeroInit ()
	      ? LIBCALL_NEWARRAYT : LIBCALL_NEWARRAYIT;
	    result = build_libcall (libcall, tb, 2,
				    build_typeinfo (e->loc, e->type),
				    build_expr (arg));
	  }
	else
	  {
	    /* Multidimensional array allocations.  */
	    tree tarray = make_array_type (Type::tsize_t, e->arguments->dim);
	    tree var = build_local_temp (tarray);
	    vec<constructor_elt, va_gc> *elms = NULL;

	    /* Get the base element type for the array, generating the
	       initializer for the dims parameter along the way.  */
	    Type *telem = e->newtype->toBasetype ();
	    for (size_t i = 0; i < e->arguments->dim; i++)
	      {
		Expression *arg = (*e->arguments)[i];
		CONSTRUCTOR_APPEND_ELT (elms, size_int (i), build_expr (arg));

		gcc_assert (telem->ty == Tarray);
		telem = telem->toBasetype ()->nextOf ();
		gcc_assert (telem);
	      }

	    /* Initialize the temporary.  */
	    tree init = modify_expr (var, build_constructor (tarray, elms));
	    var = compound_expr (init, var);

	    /* Generate: _d_newarraymTX(ti, dims)
		     or: _d_newarraymiTX(ti, dims)  */
	    libcall_fn libcall = telem->isZeroInit ()
	      ? LIBCALL_NEWARRAYMTX : LIBCALL_NEWARRAYMITX;

	    tree tinfo = build_typeinfo (e->loc, e->type);
	    tree dims = d_array_value (build_ctype (Type::tsize_t->arrayOf ()),
				       size_int (e->arguments->dim),
				       build_address (var));

	    result = build_libcall (libcall, tb, 2, tinfo, dims);
	  }

	if (e->argprefix)
	  result = compound_expr (build_expr (e->argprefix), result);
      }
    else if (tb->ty == Tpointer)
      {
	/* Allocating memory for a new pointer.  */
	TypePointer *tpointer = (TypePointer *) tb;

	if (tpointer->next->size () == 0)
	  {
	    /* Pointer element size is unknown.  */
	    this->result_ = d_convert (build_ctype (e->type),
				       integer_zero_node);
	    return;
	  }

	libcall_fn libcall = tpointer->next->isZeroInit (e->loc)
	  ? LIBCALL_NEWITEMT : LIBCALL_NEWITEMIT;

	tree arg = build_typeinfo (e->loc, e->newtype);
	result = build_libcall (libcall, tb, 1, arg);

	if (e->arguments && e->arguments->dim == 1)
	  {
	    result = d_save_expr (result);
	    tree init = modify_expr (build_deref (result),
				     build_expr ((*e->arguments)[0]));
	    result = compound_expr (init, result);
	  }

	if (e->argprefix)
	  result = compound_expr (build_expr (e->argprefix), result);
      }
    else
      gcc_unreachable ();

    this->result_ = convert_expr (result, tb, e->type);
  }

  /* Build an integer literal.  */

  void visit (IntegerExp *e)
  {
    tree ctype = build_ctype (e->type->toBasetype ());
    this->result_ = build_integer_cst (e->value, ctype);
  }

  /* Build a floating-point literal.  */

  void visit (RealExp *e)
  {
    this->result_ = build_float_cst (e->value, e->type->toBasetype ());
  }

  /* Build a complex literal.  */

  void visit (ComplexExp *e)
  {
    Type *tnext;

    switch (e->type->toBasetype ()->ty)
      {
      case Tcomplex32:
	tnext = (TypeBasic *) Type::tfloat32;
	break;

      case Tcomplex64:
	tnext = (TypeBasic *) Type::tfloat64;
	break;

      case Tcomplex80:
	tnext = (TypeBasic *) Type::tfloat80;
	break;

      default:
	gcc_unreachable ();
      }

    this->result_ = build_complex (build_ctype (e->type),
				   build_float_cst (creall (e->value), tnext),
				   build_float_cst (cimagl (e->value), tnext));
  }

  /* Build a string literal, all strings are null terminated except for
     static arrays.  */

  void visit (StringExp *e)
  {
    Type *tb = e->type->toBasetype ();
    tree type = build_ctype (e->type);

    if (tb->ty == Tsarray)
      {
	/* Turn the string into a constructor for the static array.  */
	vec<constructor_elt, va_gc> *elms = NULL;
	vec_safe_reserve (elms, e->len);
	tree etype = TREE_TYPE (type);

	for (size_t i = 0; i < e->len; i++)
	  {
	    tree value = build_integer_cst (e->charAt (i), etype);
	    CONSTRUCTOR_APPEND_ELT (elms, size_int (i), value);
	  }

	tree ctor = build_constructor (type, elms);
	TREE_CONSTANT (ctor) = 1;
	this->result_ = ctor;
      }
    else
      {
	/* Copy the string contents to a null terminated string.  */
	dinteger_t length = (e->len * e->sz);
	char *string = XALLOCAVEC (char, length + 1);
	memcpy (string, e->string, length);
	string[length] = '\0';

	/* String value and type includes the null terminator.  */
	tree value = build_string (length, string);
	TREE_TYPE (value) = make_array_type (tb->nextOf (), length + 1);
	value = build_address (value);

	if (tb->ty == Tarray)
	  value = d_array_value (type, size_int (e->len), value);

	TREE_CONSTANT (value) = 1;
	this->result_ = d_convert (type, value);
      }
  }

  /* Build a tuple literal.  Just an argument list that may have
     side effects that need evaluation.  */

  void visit (TupleExp *e)
  {
    tree result = NULL_TREE;

    if (e->e0)
      result = build_expr (e->e0);

    for (size_t i = 0; i < e->exps->dim; ++i)
      {
	Expression *exp = (*e->exps)[i];
	result = compound_expr (result, build_expr (exp));
      }

    if (result == NULL_TREE)
      result = void_node;

    this->result_ = result;
  }

  /* Build an array literal.  The common type of the all elements is taken to
     be the type of the array element, and all elements are implicitly
     converted to that type.  */

  void visit (ArrayLiteralExp *e)
  {
    Type *tb = e->type->toBasetype ();

    /* Implicitly convert void[n] to ubyte[n].  */
    if (tb->ty == Tsarray && tb->nextOf ()->toBasetype ()->ty == Tvoid)
      tb = Type::tuns8->sarrayOf (((TypeSArray *) tb)->dim->toUInteger ());

    gcc_assert (tb->ty == Tarray || tb->ty == Tsarray || tb->ty == Tpointer);

    /* Handle empty array literals.  */
    if (e->elements->dim == 0)
      {
	if (tb->ty == Tarray)
	  this->result_ = d_array_value (build_ctype (e->type),
					 size_int (0), null_pointer_node);
	else
	  this->result_ = build_constructor (make_array_type (tb->nextOf (), 0),
					     NULL);

	return;
      }

    /* Build an expression that assigns the expressions in ELEMENTS to
       a constructor.  */
    vec<constructor_elt, va_gc> *elms = NULL;
    vec_safe_reserve (elms, e->elements->dim);
    bool constant_p = true;
    tree saved_elems = NULL_TREE;

    Type *etype = tb->nextOf ();
    tree satype = make_array_type (etype, e->elements->dim);

    for (size_t i = 0; i < e->elements->dim; i++)
      {
	Expression *expr = e->getElement (i);
	tree value = build_expr (expr, this->constp_);

	/* Only append nonzero values, the backend will zero out the rest
	   of the constructor as we don't set CONSTRUCTOR_NO_CLEARING.  */
	if (!initializer_zerop (value))
	  {
	    if (!TREE_CONSTANT (value))
	      constant_p = false;

	    /* Split construction of values out of the constructor if there
	       may be side effects.  */
	    tree init = stabilize_expr (&value);
	    if (init != NULL_TREE)
	      saved_elems = compound_expr (saved_elems, init);

	    CONSTRUCTOR_APPEND_ELT (elms, size_int (i),
				    convert_expr (value, expr->type, etype));
	  }
      }

    /* Now return the constructor as the correct type.  For static arrays there
       is nothing else to do.  For dynamic arrays, return a two field struct.
       For pointers, return the address.  */
    tree ctor = build_constructor (satype, elms);
    tree type = build_ctype (e->type);

    /* Nothing else to do for static arrays.  */
    if (tb->ty == Tsarray || this->constp_)
      {
	/* Can't take the address of the constructor, so create an anonymous
	   static symbol, and then refer to it.  */
	if (tb->ty != Tsarray)
	  {
	    tree decl = build_artificial_decl (TREE_TYPE (ctor), ctor, "A");
	    ctor = build_address (decl);
	    if (tb->ty == Tarray)
	      ctor = d_array_value (type, size_int (e->elements->dim), ctor);

	    d_pushdecl (decl);
	    rest_of_decl_compilation (decl, 1, 0);
	  }

	/* If the array literal is readonly or static.  */
	if (constant_p)
	  TREE_CONSTANT (ctor) = 1;
	if (constant_p && initializer_constant_valid_p (ctor, TREE_TYPE (ctor)))
	  TREE_STATIC (ctor) = 1;

	this->result_ = compound_expr (saved_elems, d_convert (type, ctor));
      }
    else
      {
	/* Allocate space on the memory managed heap.  */
	tree mem = build_libcall (LIBCALL_ARRAYLITERALTX,
				  etype->pointerTo (), 2,
				  build_typeinfo (e->loc, etype->arrayOf ()),
				  size_int (e->elements->dim));
	mem = d_save_expr (mem);

	/* Now copy the constructor into memory.  */
	tree tmemcpy = builtin_decl_explicit (BUILT_IN_MEMCPY);
	tree size = size_mult_expr (size_int (e->elements->dim),
				    size_int (tb->nextOf ()->size ()));

	tree result = build_call_expr (tmemcpy, 3, mem,
				       build_address (ctor), size);

	/* Return the array pointed to by MEM.  */
	result = compound_expr (result, mem);

	if (tb->ty == Tarray)
	  result = d_array_value (type, size_int (e->elements->dim), result);

	this->result_ = compound_expr (saved_elems, result);
      }
  }

  /* Build an associative array literal.  The common type of the all keys is
     taken to be the key type, and common type of all values the value type.
     All keys and values are then implicitly converted as needed.  */

  void visit (AssocArrayLiteralExp *e)
  {
    /* Want the mutable type for typeinfo reference.  */
    Type *tb = e->type->toBasetype ()->mutableOf ();
    gcc_assert (tb->ty == Taarray);

    /* Handle empty assoc array literals.  */
    TypeAArray *ta = (TypeAArray *) tb;
    if (e->keys->dim == 0)
      {
	this->result_ = build_constructor (build_ctype (ta), NULL);
	return;
      }

    /* Build an expression that assigns all expressions in KEYS
       to a constructor.  */
    vec<constructor_elt, va_gc> *kelts = NULL;
    vec_safe_reserve (kelts, e->keys->dim);
    for (size_t i = 0; i < e->keys->dim; i++)
      {
	Expression *key = (*e->keys)[i];
	tree t = build_expr (key);
	CONSTRUCTOR_APPEND_ELT (kelts, size_int (i),
				convert_expr (t, key->type, ta->index));
      }
    tree tkeys = make_array_type (ta->index, e->keys->dim);
    tree akeys = build_constructor (tkeys, kelts);

    /* Do the same with all expressions in VALUES.  */
    vec<constructor_elt, va_gc> *velts = NULL;
    vec_safe_reserve (velts, e->values->dim);
    for (size_t i = 0; i < e->values->dim; i++)
      {
	Expression *value = (*e->values)[i];
	tree t = build_expr (value);
	CONSTRUCTOR_APPEND_ELT (velts, size_int (i),
				convert_expr (t, value->type, ta->next));
      }
    tree tvals = make_array_type (ta->next, e->values->dim);
    tree avals = build_constructor (tvals, velts);

    /* Generate: _d_assocarrayliteralTX (ti, keys, vals);  */
    tree keys = d_array_value (build_ctype (ta->index->arrayOf ()),
			       size_int (e->keys->dim), build_address (akeys));
    tree vals = d_array_value (build_ctype (ta->next->arrayOf ()),
			       size_int (e->values->dim),
			       build_address (avals));

    tree mem = build_libcall (LIBCALL_ASSOCARRAYLITERALTX, Type::tvoidptr, 3,
			      build_typeinfo (e->loc, ta), keys, vals);

    /* Return an associative array pointed to by MEM.  */
    tree aatype = build_ctype (ta);
    vec<constructor_elt, va_gc> *ce = NULL;
    CONSTRUCTOR_APPEND_ELT (ce, TYPE_FIELDS (aatype), mem);

    this->result_ = build_nop (build_ctype (e->type),
			       build_constructor (aatype, ce));
  }

  /* Build a struct literal.  */

  void visit (StructLiteralExp *e)
  {
    /* Handle empty struct literals.  */
    if (e->elements == NULL || e->sd->fields.dim == 0)
      {
	this->result_ = build_constructor (build_ctype (e->type), NULL);
	return;
      }

    /* Building sinit trees are delayed until after frontend semantic
       processing has complete.  Build the static initializer now.  */
    if (e->useStaticInit && !this->constp_)
      {
	this->result_ = aggregate_initializer_decl (e->sd);
	return;
      }

    /* Build a constructor that assigns the expressions in ELEMENTS
       at each field index that has been filled in.  */
    vec<constructor_elt, va_gc> *ve = NULL;
    tree saved_elems = NULL_TREE;

    /* CTFE may fill the hidden pointer by NullExp.  */
    gcc_assert (e->elements->dim <= e->sd->fields.dim);

    Type *tb = e->type->toBasetype ();
    gcc_assert (tb->ty == Tstruct);

    for (size_t i = 0; i < e->elements->dim; i++)
      {
	Expression *exp = (*e->elements)[i];
	if (!exp)
	  continue;

	VarDeclaration *field = e->sd->fields[i];
	Type *type = exp->type->toBasetype ();
	Type *ftype = field->type->toBasetype ();
	tree value = NULL_TREE;

	if (ftype->ty == Tsarray && !same_type_p (type, ftype))
	  {
	    /* Initialize a static array with a single element.  */
	    tree elem = build_expr (exp, this->constp_);
	    elem = d_save_expr (elem);

	    if (initializer_zerop (elem))
	      value = build_constructor (build_ctype (ftype), NULL);
	    else
	      value = build_array_from_val (ftype, elem);
	  }
	else
	  {
	    value = convert_expr (build_expr (exp, this->constp_),
				  exp->type, field->type);
	  }

	/* Split construction of values out of the constructor.  */
	tree init = stabilize_expr (&value);
	if (init != NULL_TREE)
	  saved_elems = compound_expr (saved_elems, init);

	CONSTRUCTOR_APPEND_ELT (ve, get_symbol_decl (field), value);
      }

    /* Maybe setup hidden pointer to outer scope context.  */
    if (e->sd->isNested () && e->elements->dim != e->sd->fields.dim
	&& this->constp_ == false)
      {
	tree field = get_symbol_decl (e->sd->vthis);
	tree value = build_vthis (e->sd);
	CONSTRUCTOR_APPEND_ELT (ve, field, value);
	gcc_assert (e->useStaticInit == false);
      }

    /* Build a constructor in the correct shape of the aggregate type.  */
    tree ctor = build_struct_literal (build_ctype (e->type), ve);

    /* Nothing more to do for constant literals.  */
    if (this->constp_)
      {
	/* If the struct literal is a valid for static data.  */
	if (TREE_CONSTANT (ctor)
	    && initializer_constant_valid_p (ctor, TREE_TYPE (ctor)))
	  TREE_STATIC (ctor) = 1;

	this->result_ = compound_expr (saved_elems, ctor);
	return;
      }

    if (e->sym != NULL)
      {
	tree var = build_deref (e->sym);
	ctor = compound_expr (modify_expr (var, ctor), var);
	this->result_ = compound_expr (saved_elems, ctor);
      }
    else if (e->sd->isUnionDeclaration ())
      {
	/* For unions, use memset to fill holes in the object.  */
	tree var = build_local_temp (TREE_TYPE (ctor));
	tree tmemset = builtin_decl_explicit (BUILT_IN_MEMSET);
	tree init = build_call_expr (tmemset, 3, build_address (var),
				     size_zero_node,
				     size_int (e->sd->structsize));

	init = compound_expr (init, saved_elems);
	init = compound_expr (init, modify_expr (var, ctor));
	this->result_  = compound_expr (init, var);
      }
    else
      this->result_ = compound_expr (saved_elems, ctor);
  }

  /* Build a null literal.  */

  void visit (NullExp *e)
  {
    this->result_ = build_typeof_null_value (e->type);
  }

  /* Build a vector literal.  */

  void visit (VectorExp *e)
  {
    tree type = build_ctype (e->type);
    tree etype = TREE_TYPE (type);

    /* First handle array literal expressions.  */
    if (e->e1->op == TOKarrayliteral)
      {
	ArrayLiteralExp *ale = ((ArrayLiteralExp *) e->e1);
	vec<constructor_elt, va_gc> *elms = NULL;
	bool constant_p = true;

	vec_safe_reserve (elms, ale->elements->dim);
	for (size_t i = 0; i < ale->elements->dim; i++)
	  {
	    Expression *expr = ale->getElement (i);
	    tree value = d_convert (etype, build_expr (expr, this->constp_));
	    if (!CONSTANT_CLASS_P (value))
	      constant_p = false;

	    CONSTRUCTOR_APPEND_ELT (elms, size_int (i), value);
	  }

	/* Build a VECTOR_CST from a constant vector constructor.  */
	if (constant_p)
	  this->result_ = build_vector_from_ctor (type, elms);
	else
	  this->result_ = build_constructor (type, elms);
      }
    else
      {
	/* Build constructor from single value.  */
	tree val = d_convert (etype, build_expr (e->e1, this->constp_));
	this->result_ = build_vector_from_val (type, val);
      }
  }

  /* Build a static array representation of a vector expression.  */

  void visit (VectorArrayExp *e)
  {
    this->result_ = convert_expr (build_expr (e->e1, this->constp_),
				  e->e1->type, e->type);
  }

  /* Build a static class literal, return its reference.  */

  void visit (ClassReferenceExp *e)
  {
    /* The result of build_new_class_expr is a RECORD_TYPE, we want
       the reference.  */
    tree var = build_address (build_new_class_expr (e));

    /* If the type of this literal is an interface, the we must add the
       interface offset to symbol.  */
    if (this->constp_)
      {
	TypeClass *tc = (TypeClass *) e->type;
	InterfaceDeclaration *to = tc->sym->isInterfaceDeclaration ();

	if (to != NULL)
	  {
	    ClassDeclaration *from = e->originalClass ();
	    int offset = 0;

	    gcc_assert (to->isBaseOf (from, &offset) != 0);

	    if (offset != 0)
	      var = build_offset (var, size_int (offset));
	  }
      }

    this->result_ = var;
  }

  /* These expressions are mainly just a placeholders in the frontend.
     We shouldn't see them here.  */

  void visit (ScopeExp *e)
  {
    error_at (make_location_t (e->loc), "%qs is not an expression",
	      e->toChars ());
    this->result_ = error_mark_node;
  }

  void visit (TypeExp *e)
  {
    error_at (make_location_t (e->loc), "type %qs is not an expression",
	      e->toChars ());
    this->result_ = error_mark_node;
  }
};


/* Main entry point for ExprVisitor interface to generate code for
   the Expression AST class E.  If CONST_P is true, then E is a
   constant expression.  */

tree
build_expr (Expression *e, bool const_p)
{
  ExprVisitor v = ExprVisitor (const_p);
  location_t saved_location = input_location;

  input_location = make_location_t (e->loc);
  e->accept (&v);
  tree expr = v.result ();
  input_location = saved_location;

  /* Check if initializer expression is valid constant.  */
  if (const_p && !initializer_constant_valid_p (expr, TREE_TYPE (expr)))
    {
      error_at (make_location_t (e->loc), "non-constant expression %qs",
		e->toChars ());
      return error_mark_node;
    }

  return expr;
}

/* Same as build_expr, but also calls destructors on any temporaries.  */

tree
build_expr_dtor (Expression *e)
{
  /* Codegen can be improved by determining if no exceptions can be thrown
     between the ctor and dtor, and eliminating the ctor and dtor.  */
  size_t saved_vars = vec_safe_length (d_function_chain->vars_in_scope);
  tree result = build_expr (e);

  if (saved_vars != vec_safe_length (d_function_chain->vars_in_scope))
    {
      result = fold_build_cleanup_point_expr (TREE_TYPE (result), result);
      vec_safe_truncate (d_function_chain->vars_in_scope, saved_vars);
    }

  return result;
}

/* Same as build_expr_dtor, but handles the result of E as a return value.  */

tree
build_return_dtor (Expression *e, Type *type, TypeFunction *tf)
{
  size_t saved_vars = vec_safe_length (d_function_chain->vars_in_scope);
  tree result = build_expr (e);

  /* Convert for initializing the DECL_RESULT.  */
  result = convert_expr (result, e->type, type);

  /* If we are returning a reference, take the address.  */
  if (tf->isref)
    result = build_address (result);

  /* The decl to store the return expression.  */
  tree decl = DECL_RESULT (cfun->decl);

  /* Split comma expressions, so that the result is returned directly.  */
  tree expr = stabilize_expr (&result);
  result = build_assign (INIT_EXPR, decl, result);
  result = compound_expr (expr, return_expr (result));

  /* May nest the return expression inside the try/finally expression.  */
  if (saved_vars != vec_safe_length (d_function_chain->vars_in_scope))
    {
      result = fold_build_cleanup_point_expr (TREE_TYPE (result), result);
      vec_safe_truncate (d_function_chain->vars_in_scope, saved_vars);
    }

  return result;
}

