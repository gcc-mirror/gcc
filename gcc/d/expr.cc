/* expr.cc -- Lower D frontend expressions to GCC trees.
   Copyright (C) 2015-2025 Free Software Foundation, Inc.

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
#include "dmd/enum.h"
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


/* Determine if type T is a struct that has a postblit.  */

static bool
needs_postblit (Type *t)
{
  t = t->baseElemOf ();

  if (TypeStruct *ts = t->isTypeStruct ())
    {
      if (ts->sym->postblit)
	return true;
    }

  return false;
}

/* Determine if type T is a struct that has a destructor.  */

static bool
needs_dtor (Type *t)
{
  t = t->baseElemOf ();

  if (TypeStruct *ts = t->isTypeStruct ())
    {
      if (ts->sym->dtor)
	return true;
    }

  return false;
}

/* Determine if expression E is a suitable lvalue.  */

static bool
lvalue_p (Expression *e)
{
  SliceExp *se = e->isSliceExp ();
  if (se != NULL && se->e1->isLvalue ())
    return true;

  CastExp *ce = e->isCastExp ();
  if (ce != NULL && ce->e1->isLvalue ())
    return true;

  return (e->op != EXP::slice && e->isLvalue ());
}

/* Build an expression of code CODE, data type TYPE, and operands ARG0 and
   ARG1.  Perform relevant conversions needed for correct code operations.  */

static tree
binary_op (tree_code code, tree type, tree arg0, tree arg1)
{
  tree t0 = TREE_TYPE (arg0);
  tree t1 = TREE_TYPE (arg1);
  tree ret = NULL_TREE;

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

      ret = build2 (code, eptype, arg0, arg1);
    }

  return d_convert (type, ret);
}

/* Build a binary expression of code CODE, assigning the result into E1.  */

static tree
binop_assignment (tree_code code, Expression *e1, Expression *e2)
{
  /* Skip casts for lhs assignment.  */
  Expression *e1b = e1;
  while (e1b->op == EXP::cast_)
    {
      CastExp *ce = e1b->isCastExp ();
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

  rhs = binary_op (code, build_ctype (e1->type),
		   convert_expr (lhs, e1b->type, e1->type), rexpr);
  if (TREE_SIDE_EFFECTS (rhs))
    rhs = compound_expr (rexpr, rhs);

  tree expr = modify_expr (lhs, convert_expr (rhs, e1->type, e1b->type));
  return compound_expr (lexpr, expr);
}

/* Compile the function literal body.  */

static void
build_lambda_tree (FuncLiteralDeclaration *fld, Type *type = NULL)
{
  /* This check is for lambda's, remove `vthis' as function isn't nested.  */
  if (fld->tok == TOK::reserved && (type == NULL || type->ty == TY::Tpointer))
    {
      fld->tok = TOK::function_;
      fld->vthis = NULL;
    }

  /* Compile the function literal body.  */
  build_decl_tree (fld);
}

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
  bool literalp_;

public:
  ExprVisitor (bool constp, bool literalp)
  {
    this->result_ = NULL_TREE;
    this->constp_ = constp;
    this->literalp_ = literalp;
  }

  tree result (void)
  {
    return this->result_;
  }

  /* Visitor interfaces, each Expression class should have
     overridden the default.  */

  void visit (Expression *) final override
  {
    gcc_unreachable ();
  }

  /* Build a conditional expression.  If either the second or third
     expression is void, then the resulting type is void.  Otherwise
     they are implicitly converted to a common type.  */

  void visit (CondExp *e) final override
  {
    tree cond = convert_for_condition (build_expr (e->econd),
				       e->econd->type);
    tree t1 = build_expr (e->e1);
    tree t2 = build_expr (e->e2);

    if (e->type->ty != TY::Tvoid)
      {
	t1 = convert_expr (t1, e->e1->type, e->type);
	t2 = convert_expr (t2, e->e2->type, e->type);
      }

    this->result_ = build_condition (build_ctype (e->type), cond, t1, t2);
  }

  /* Build an identity comparison expression.  Operands go through the
     usual conversions to bring them to a common type before comparison.
     The result type is bool.  */

  void visit (IdentityExp *e) final override
  {
    tree_code code = (e->op == EXP::identity) ? EQ_EXPR : NE_EXPR;
    Type *tb1 = e->e1->type->toBasetype ();
    Type *tb2 = e->e2->type->toBasetype ();

    if ((tb1->ty == TY::Tsarray || tb1->ty == TY::Tarray)
	&& (tb2->ty == TY::Tsarray || tb2->ty == TY::Tarray))
      {
	/* For static and dynamic arrays, identity is defined as referring to
	   the same array elements and the same number of elements.  */
	tree t1 = d_array_convert (e->e1);
	tree t2 = d_array_convert (e->e2);
	this->result_ = d_convert (build_ctype (e->type),
				   build_boolop (code, t1, t2));
      }
    else if (tb1->isFloating () && tb1->ty != TY::Tvector)
      {
	/* For floating-point values, identity is defined as the bits in the
	   operands being identical.  */
	tree t1 = d_save_expr (build_expr (e->e1));
	tree t2 = d_save_expr (build_expr (e->e2));

	if (!tb1->isComplex ())
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
    else if (TypeStruct *ts = tb1->isTypeStruct ())
      {
	/* For struct objects, identity is defined as bits in operands being
	   identical also.  Alignment holes in structs are ignored.  */
	tree t1 = build_expr (e->e1);
	tree t2 = build_expr (e->e2);

	gcc_assert (same_type_p (tb1, tb2));

	this->result_ = build_struct_comparison (code, ts->sym, t1, t2);
      }
    else if (tb1->ty == TY::Tvector && tb2->ty == TY::Tvector)
      {
	/* For vectors, identity is defined as all values being equal.  */
	tree t1 = build_expr (e->e1);
	tree t2 = build_expr (e->e2);
	tree mask = build_boolop (code, t1, t2);

	/* To reinterpret the vector comparison as a boolean expression, bitcast
	   the bitmask result and generate an additional integer comparison.  */
	opt_scalar_int_mode mode =
	  int_mode_for_mode (TYPE_MODE (TREE_TYPE (mask)));
	gcc_assert (mode.exists ());

	tree type = lang_hooks.types.type_for_mode (mode.require (), 1);
	if (type == NULL_TREE)
	  type = make_unsigned_type (GET_MODE_BITSIZE (mode.require ()));

	/* In `t1 is t2', all mask bits must be set for vectors to be equal.
	   Otherwise any bit set is enough for vectors to be not-equal.  */
	tree mask_eq = (code == EQ_EXPR)
	  ? build_all_ones_cst (type) : build_zero_cst (type);

	this->result_ = build_boolop (code, mask_eq,
				      build_vconvert (type, mask));
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

  void visit (EqualExp *e) final override
  {
    Type *tb1 = e->e1->type->toBasetype ();
    Type *tb2 = e->e2->type->toBasetype ();
    tree_code code = (e->op == EXP::equal) ? EQ_EXPR : NE_EXPR;

    if ((tb1->ty == TY::Tsarray || tb1->ty == TY::Tarray)
	&& (tb2->ty == TY::Tsarray || tb2->ty == TY::Tarray))
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
	if ((t1elem->isIntegral () || t1elem->ty == TY::Tvoid
	     || (t1elem->ty == TY::Tstruct
		 && !t1elem->isTypeStruct ()->sym->xeq))
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
	    if (t1elem->ty != TY::Tstruct
		|| identity_compare_p (t1elem->isTypeStruct ()->sym))
	      {
		tree size =
		  size_mult_expr (t1len, size_int (dmd::size (t1elem)));

		result = build_memcmp_call (t1ptr, t2ptr, size);
		result = build_boolop (code, result, integer_zero_node);
	      }
	    else
	      {
		StructDeclaration *sd = t1elem->isTypeStruct ()->sym;

		result = build_array_struct_comparison (code, sd, t1len,
							t1ptr, t2ptr);
	      }

	    /* Check array length first before passing to memcmp.
	       For equality expressions, this becomes:
		    (e1.length == 0 || memcmp);
	       Otherwise for inequality:
		    (e1.length != 0 && memcmp);  */
	    tree tsizecmp = build_boolop (code, t1len, size_zero_node);
	    if (e->op == EXP::equal)
	      result = build_boolop (TRUTH_ORIF_EXPR, tsizecmp, result);
	    else
	      result = build_boolop (TRUTH_ANDIF_EXPR, tsizecmp, result);

	    /* Finally, check if lengths of both arrays match if dynamic.
	       The frontend should have already guaranteed that static arrays
	       have same size.  */
	    if (tb1->ty == TY::Tsarray && tb2->ty == TY::Tsarray)
	      gcc_assert (dmd::size (tb1) == dmd::size (tb2));
	    else
	      {
		tree tlencmp = build_boolop (code, t1len, t2len);
		if (e->op == EXP::equal)
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
	    Type *t1array = dmd::arrayOf (t1elem);
	    tree result = build_libcall (LIBCALL_ADEQ2, e->type, 3,
					 d_array_convert (e->e1),
					 d_array_convert (e->e2),
					 build_typeinfo (e, t1array));

	    if (e->op == EXP::notEqual)
	      result = build1 (TRUTH_NOT_EXPR, build_ctype (e->type), result);

	    this->result_ = result;
	  }
      }
    else if (TypeStruct *ts = tb1->isTypeStruct ())
      {
	/* Equality for struct objects means the logical product of all
	   equality results of the corresponding object fields.  */
	tree t1 = build_expr (e->e1);
	tree t2 = build_expr (e->e2);

	gcc_assert (same_type_p (tb1, tb2));

	this->result_ = build_struct_comparison (code, ts->sym, t1, t2);
      }
    else if (tb1->ty == TY::Taarray && tb2->ty == TY::Taarray)
      {
	/* Use _aaEqual() for associative arrays.  */
	tree result = build_libcall (LIBCALL_AAEQUAL, e->type, 3,
				     build_typeinfo (e, tb1),
				     build_expr (e->e1),
				     build_expr (e->e2));

	if (e->op == EXP::notEqual)
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

  void visit (InExp *e) final override
  {
    Type *tb2 = e->e2->type->toBasetype ();
    Type *tkey = tb2->isTypeAArray ()->index->toBasetype ();
    tree key = convert_expr (build_expr (e->e1), e->e1->type, tkey);

    /* Build a call to _aaInX().  */
    this->result_ = build_libcall (LIBCALL_AAINX, e->type, 3,
				   build_expr (e->e2),
				   build_typeinfo (e, tkey),
				   build_address (key));
  }

  /* Build a relational expression.  The result type is bool.  */

  void visit (CmpExp *e) final override
  {
    Type *tb1 = e->e1->type->toBasetype ();
    Type *tb2 = e->e2->type->toBasetype ();

    tree result;
    tree_code code;

    switch (e->op)
      {
      case EXP::lessOrEqual:
	code = LE_EXPR;
	break;

      case EXP::lessThan:
	code = LT_EXPR;
	break;

      case EXP::greaterOrEqual:
	code = GE_EXPR;
	break;

      case EXP::greaterThan:
	code = GT_EXPR;
	break;

      default:
	gcc_unreachable ();
      }

    /* For static and dynamic arrays, the relational op is turned into a
       library call.  It is not lowered during codegen.  */
    if ((tb1->ty == TY::Tsarray || tb1->ty == TY::Tarray)
	&& (tb2->ty == TY::Tsarray || tb2->ty == TY::Tarray))
      {
	error ("cannot handle comparison of type %<%s == %s%>",
	       tb1->toChars (), tb2->toChars ());
	gcc_unreachable ();
      }

    /* Simple comparison.  */
    result = build_boolop (code, build_expr (e->e1), build_expr (e->e2));
    this->result_ = d_convert (build_ctype (e->type), result);
  }

  /* Build a logical `and if' or `or if' expression.  If the right operand
     expression is void, then the resulting type is void.  Otherwise the
     result is bool.  */

  void visit (LogicalExp *e) final override
  {
    tree_code code = (e->op == EXP::andAnd) ? TRUTH_ANDIF_EXPR : TRUTH_ORIF_EXPR;

    if (e->e2->type->toBasetype ()->ty != TY::Tvoid)
      {
	tree t1 = build_expr (e->e1);
	tree t2 = build_expr (e->e2);

	t1 = convert_for_condition (t1, e->e1->type);
	t2 = convert_for_condition (t2, e->e2->type);

	this->result_ = d_convert (build_ctype (e->type),
				   build_boolop (code, t1, t2));
      }
    else
      {
	tree t1 = convert_for_condition (build_expr (e->e1), e->e1->type);
	tree t2 = build_expr_dtor (e->e2);

	/* Invert condition for logical or if expression.  */
	if (e->op == EXP::orOr)
	  t1 = build1 (TRUTH_NOT_EXPR, d_bool_type, t1);

	this->result_ = build_condition (build_ctype (e->type),
					 t1, t2, void_node);
      }
  }

  /* Build a binary operand expression.  Operands go through usual arithmetic
     conversions to bring them to a common type before evaluating.  */

  void visit (BinExp *e) final override
  {
    tree_code code;

    switch (e->op)
      {
      case EXP::add:
      case EXP::min:
	if ((e->e1->type->isReal () && e->e2->type->isImaginary ())
	    || (e->e1->type->isImaginary () && e->e2->type->isReal ()))
	  {
	    /* If the result is complex, then we can shortcut binary_op.
	       Frontend should have already validated types and sizes.  */
	    tree t1 = build_expr (e->e1);
	    tree t2 = build_expr (e->e2);

	    if (e->op == EXP::min)
	      t2 = build1 (NEGATE_EXPR, TREE_TYPE (t2), t2);

	    if (e->e1->type->isReal ())
	      this->result_ = complex_expr (build_ctype (e->type), t1, t2);
	    else
	      this->result_ = complex_expr (build_ctype (e->type), t2, t1);

	    return;
	  }
	else
	  code = (e->op == EXP::add)
	    ? PLUS_EXPR : MINUS_EXPR;
	break;

      case EXP::mul:
	code = MULT_EXPR;
	break;

      case EXP::div:
	/* Determine if the div expression is a lowered pointer diff operation.
	   The front-end rewrites `(p1 - p2)' into `(p1 - p2) / stride'.  */
	if (MinExp *me = e->e1->isMinExp ())
	  {
	    if (me->e1->type->ty == TY::Tpointer
		&& me->e2->type->ty == TY::Tpointer
		&& e->e2->op == EXP::int64)
	      {
		code = EXACT_DIV_EXPR;
		break;
	      }
	  }

	code = e->e1->type->isIntegral ()
	  ? TRUNC_DIV_EXPR : RDIV_EXPR;
	break;

      case EXP::mod:
	code = e->e1->type->isFloating ()
	  ? FLOAT_MOD_EXPR : TRUNC_MOD_EXPR;
	break;

      case EXP::and_:
	code = BIT_AND_EXPR;
	break;

      case EXP::or_:
	code = BIT_IOR_EXPR;
	break;

      case EXP::xor_:
	code = BIT_XOR_EXPR;
	break;

      case EXP::leftShift:
	code = LSHIFT_EXPR;
	  break;

      case EXP::rightShift:
	code = RSHIFT_EXPR;
	break;

      case EXP::unsignedRightShift:
	code = UNSIGNED_RSHIFT_EXPR;
	break;

      default:
	gcc_unreachable ();
      }

    this->result_ = binary_op (code, build_ctype (e->type),
			       build_expr (e->e1), build_expr (e->e2));
  }


  /* Build a concat expression, which concatenates two or more arrays of the
     same type, producing a dynamic array with the result.  If one operand
     is an element type, that element is converted to an array of length 1.  */

  void visit (CatExp *e) final override
  {
    /* This error is only emitted during the code generation pass because
       concatentation is allowed in CTFE.  */
    if (!global.params.useGC)
      {
	error_at (make_location_t (e->loc),
		  "array concatenation of expression %qs requires the GC and "
		  "cannot be used with %<-fno-druntime%>", e->toChars ());
	this->result_ = error_mark_node;
	return;
      }

    /* All concat expressions should have been rewritten to `_d_arraycatnTX` in
       the semantic phase.  */
    gcc_assert (e->lowering);

    this->result_ = build_expr (e->lowering);
  }

  /* Build an assignment operator expression.  The right operand is implicitly
     converted to the type of the left operand, and assigned to it.  */

  void visit (BinAssignExp *e) final override
  {
    tree_code code;
    Expression *e1b = e->e1;

    switch (e->op)
      {
      case EXP::addAssign:
	code = PLUS_EXPR;
	break;

      case EXP::minAssign:
	code = MINUS_EXPR;
	break;

      case EXP::mulAssign:
	code = MULT_EXPR;
	break;

      case EXP::divAssign:
	code = e->e1->type->isIntegral ()
	  ? TRUNC_DIV_EXPR : RDIV_EXPR;
	break;

      case EXP::modAssign:
	code = e->e1->type->isFloating ()
	  ? FLOAT_MOD_EXPR : TRUNC_MOD_EXPR;
	break;

      case EXP::andAssign:
	code = BIT_AND_EXPR;
	break;

      case EXP::orAssign:
	code = BIT_IOR_EXPR;
	break;

      case EXP::xorAssign:
	code = BIT_XOR_EXPR;
	break;

      case EXP::powAssign:
	gcc_unreachable ();

      case EXP::leftShiftAssign:
	code = LSHIFT_EXPR;
	break;

      case EXP::rightShiftAssign:
      case EXP::unsignedRightShiftAssign:
	/* Use the original lhs type before it was promoted.  The left operand
	   of `>>>=' does not undergo integral promotions before shifting.
	   Strip off casts just incase anyway.  */
	while (e1b->op == EXP::cast_)
	  {
	    CastExp *ce = e1b->isCastExp ();
	    gcc_assert (same_type_p (ce->type, ce->to));
	    e1b = ce->e1;
	  }
	code = (e->op == EXP::rightShiftAssign) ? RSHIFT_EXPR : UNSIGNED_RSHIFT_EXPR;
	break;

      default:
	gcc_unreachable ();
      }

    tree exp = binop_assignment (code, e1b, e->e2);
    this->result_ = convert_expr (exp, e1b->type, e->type);
  }

  /* Build a concat assignment expression.  The right operand is appended
     to the left operand.  */

  void visit (CatAssignExp *e) final override
  {
    if (!global.params.useGC)
      {
	error_at (make_location_t (e->loc),
		  "appending to array in %qs requires the GC and cannot be "
		  "used with %<-fno-druntime%>", e->toChars ());
	this->result_ = error_mark_node;
	return;
      }

    Type *tb1 = e->e1->type->toBasetype ();
    Type *tb2 = e->e2->type->toBasetype ();

    if (e->op == EXP::concatenateDcharAssign)
      {
	/* Append a dchar to a char[] or wchar[]:
	   The assignment is handled by the D run-time library, so only
	   need to call `_d_arrayappend[cw]d(&e1, e2)'  */
	Type *etype = tb1->nextOf ()->toBasetype ();

	/* Save the address of `e1', so it can be evaluated first.
	   As all D run-time library functions for concat assignments update
	   `e1' in-place and then return its value, the saved address can also
	   be used as the result of this expression as well.  */
	tree lhs = build_expr (e->e1);
	tree lexpr = stabilize_expr (&lhs);
	tree ptr = d_save_expr (build_address (lhs));
	tree result = NULL_TREE;

	gcc_assert (tb1->ty == TY::Tarray && tb2->ty == TY::Tdchar
		    && (etype->ty == TY::Tchar || etype->ty == TY::Twchar));

	libcall_fn libcall = (etype->ty == TY::Tchar)
	  ? LIBCALL_ARRAYAPPENDCD : LIBCALL_ARRAYAPPENDWD;

	result = build_libcall (libcall, e->type, 2,
				ptr, build_expr (e->e2));

	/* Construct in order: ptr = &e1, _d_arrayappend(ptr, e2), *ptr;  */
	result = compound_expr (compound_expr (lexpr, ptr), result);
	this->result_ = compound_expr (result, build_deref (ptr));
      }
    else
      {
	gcc_assert (e->op == EXP::concatenateAssign
		    || e->op == EXP::concatenateElemAssign);
	gcc_assert (tb1->ty == TY::Tarray || tb2->ty == TY::Tsarray);
	/* Appending an element or array to another array has already been
	   handled by the front-end.  */
	gcc_assert (e->lowering);

	this->result_ = build_expr (e->lowering);
      }
  }

  /* Build an assignment expression.  The right operand is implicitly
     converted to the type of the left operand, and assigned to it.  */

  void visit (AssignExp *e) final override
  {
    /* First, handle special assignment semantics.  */

    /* Look for array.length = n;  */
    if (e->e1->op == EXP::arrayLength)
      {
	/* This case should have been rewritten to `_d_arraysetlengthT` in the
	   semantic phase.  */
	gcc_unreachable ();
      }

    /* Look for exp = noreturn;  */
    if (e->e2->type->isTypeNoreturn ())
      {
	/* If the RHS is a `noreturn' expression, there is no point generating
	   any code for the assignment, just evaluate side effects.  */
	tree t1 = build_expr (e->e1);
	tree t2 = build_expr (e->e2);
	this->result_ = compound_expr (t1, t2);
	return;
      }

    /* Look for array[] = n;  */
    if (e->e1->op == EXP::slice)
      {
	SliceExp *se = e->e1->isSliceExp ();
	Type *stype = se->e1->type->toBasetype ();
	Type *etype = stype->nextOf ()->toBasetype ();

	/* Determine if we need to run postblit or dtor.  */
	bool postblit = needs_postblit (etype) && lvalue_p (e->e2);
	bool destructor = needs_dtor (etype);

	if (e->memset == MemorySet::blockAssign)
	  {
	    /* Set a range of elements to one value.  */
	    tree t1 = build_expr (e->e1);
	    tree t2 = build_expr (e->e2);
	    tree result;

	    /* Extract any array bounds checks from the slice expression.  */
	    tree init = stabilize_expr (&t1);
	    t1 = d_save_expr (t1);

	    if ((postblit || destructor) && e->op != EXP::blit)
	      {
		/* This case should have been rewritten to `_d_arraysetassign`
		   in the semantic phase.  */
		gcc_unreachable ();
	      }

	    if (integer_zerop (t2))
	      {
		tree size = size_mult_expr (d_array_length (t1),
					    size_int (dmd::size (etype)));
		result = build_memset_call (d_array_ptr (t1), size);
	      }
	    else
	      result = build_array_set (d_array_ptr (t1),
					d_array_length (t1), t2);

	    result = compound_expr (init, result);
	    this->result_ = compound_expr (result, t1);
	  }
	else
	  {
	    /* Perform a memcpy operation.  */
	    gcc_assert (e->e2->type->ty != TY::Tpointer);

	    if (!postblit && !destructor)
	      {
		tree t1 = d_save_expr (d_array_convert (e->e1));
		tree t2 = d_save_expr (d_array_convert (e->e2));

		/* References to array data.  */
		tree t1ptr = d_array_ptr (t1);
		tree t1len = d_array_length (t1);
		tree t2ptr = d_array_ptr (t2);

		/* Generate: memcpy(to, from, size)  */
		tree size =
		  size_mult_expr (t1len, size_int (dmd::size (etype)));
		tree result = build_memcpy_call (t1ptr, t2ptr, size);

		/* Insert check that array lengths match and do not overlap.  */
		if (array_bounds_check ())
		  {
		    /* tlencmp = (t1len == t2len)  */
		    tree t2len = d_array_length (t2);
		    tree tlencmp = build_boolop (EQ_EXPR, t1len, t2len);

		    /* toverlap = (t1ptr + size <= t2ptr
				   || t2ptr + size <= t1ptr)  */
		    tree t1ptrcmp = build_boolop (LE_EXPR,
						  build_offset (t1ptr, size),
						  t2ptr);
		    tree t2ptrcmp = build_boolop (LE_EXPR,
						  build_offset (t2ptr, size),
						  t1ptr);
		    tree toverlap = build_boolop (TRUTH_ORIF_EXPR, t1ptrcmp,
						  t2ptrcmp);

		    /* (tlencmp && toverlap) ? memcpy() : _d_arraybounds()  */
		    tree tassert = build_array_bounds_call (e->loc);
		    tree tboundscheck = build_boolop (TRUTH_ANDIF_EXPR,
						      tlencmp, toverlap);

		    result = build_condition (void_type_node, tboundscheck,
					      result, tassert);
		  }

		this->result_ = compound_expr (result, t1);
	      }
	    else if ((postblit || destructor)
		     && e->op != EXP::blit && e->op != EXP::construct)
	      {
		/* Assigning to a non-trivially copyable array has already been
		   handled by the front-end.  */
		gcc_unreachable ();
	      }
	    else
	      {
		/* Generate: _d_arraycopy()  */
		this->result_ = build_libcall (LIBCALL_ARRAYCOPY, e->type, 3,
					       size_int (dmd::size (etype)),
					       d_array_convert (e->e2),
					       d_array_convert (e->e1));
	      }
	  }

	return;
      }

    /* Look for reference initializations.  */
    if (e->memset == MemorySet::referenceInit)
      {
	gcc_assert (e->op == EXP::construct || e->op == EXP::blit);
	gcc_assert (e->e1->op == EXP::variable);

	Declaration *decl = e->e1->isVarExp ()->var;
	if (decl->storage_class & (STCout | STCref))
	  {
	    tree t2 = convert_for_assignment (e->e2, e->e1->type);
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
    tree_code modifycode = (e->op == EXP::construct) ? INIT_EXPR : MODIFY_EXPR;

    /* Look for struct assignment.  */
    if (tb1->ty == TY::Tstruct)
      {
	tree t1 = build_expr (e->e1);
	tree t2 = convert_for_assignment (e->e2, e->e1->type, true);
	StructDeclaration *sd = tb1->isTypeStruct ()->sym;

	/* Look for struct = 0.  */
	if (e->e2->op == EXP::int64)
	  {
	    /* Use memset to fill struct.  */
	    gcc_assert (e->op == EXP::blit);
	    tree result = build_memset_call (t1);

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
	    if ((e->op == EXP::construct
		 || (e->e2->op == EXP::structLiteral && e->op == EXP::blit))
		&& (sd->isUnionDeclaration () || !identity_compare_p (sd)))
	      {
		t1 = stabilize_reference (t1);
		init = build_memset_call (t1);
	      }

	    /* Elide generating assignment if init is all zeroes.  */
	    if (init != NULL_TREE && initializer_zerop (t2))
	      this->result_ = compound_expr (init, t1);
	    else
	      {
		tree result = build_assign (modifycode, t1, t2);
		this->result_ = compound_expr (init, result);
	      }
	  }

	return;
      }

    /* Look for static array assignment.  */
    if (tb1->ty == TY::Tsarray)
      {
	/* Look for array = 0.  */
	if (e->e2->op == EXP::int64)
	  {
	    /* Use memset to fill the array.  */
	    gcc_assert (e->op == EXP::blit);
	    this->result_ = build_memset_call (build_expr (e->e1));
	    return;
	  }

	Type *etype = tb1->nextOf ();
	gcc_assert (e->e2->type->toBasetype ()->ty == TY::Tsarray);

	/* Determine if we need to run postblit.  */
	const bool postblit = needs_postblit (etype);
	const bool destructor = needs_dtor (etype);
	const bool lvalue = lvalue_p (e->e2);

	/* Optimize static array assignment with array literal.  Even if the
	   elements in rhs are all rvalues and don't have to call postblits,
	   this assignment should call dtors on old assigned elements.  */
	if ((!postblit && !destructor)
	    || (e->op == EXP::construct && e->e2->op == EXP::arrayLiteral)
	    || (e->op == EXP::construct && e->e2->op == EXP::call)
	    || (e->op == EXP::construct && !lvalue && postblit)
	    || (e->op == EXP::blit || dmd::size (e->e1->type) == 0))
	  {
	    tree t1 = build_expr (e->e1);
	    tree t2 = convert_for_assignment (e->e2, e->e1->type);

	    this->result_ = build_assign (modifycode, t1, t2);
	    return;
	  }

	/* All other kinds of lvalue or rvalue static array assignment.
	   Array construction has already been handled by the front-end.  */
	gcc_assert (e->op != EXP::construct);
	gcc_unreachable ();
      }

    /* Simple assignment.  */
    tree t1 = build_expr (e->e1);
    tree t2 = convert_for_assignment (e->e2, e->e1->type);

    this->result_ = build_assign (modifycode, t1, t2);
  }

  /* Build an assignment expression that has been lowered in the front-end.  */

  void visit (LoweredAssignExp *e) final override
  {
    this->result_ = build_expr (e->lowering);
  }

  /* Build a throw expression.  */

  void visit (ThrowExp *e) final override
  {
    tree arg = build_expr_dtor (e->e1);
    this->result_ = build_libcall (LIBCALL_THROW, Type::tvoid, 1, arg);
  }

  /* Build a postfix expression.  */

  void visit (PostExp *e) final override
  {
    tree result;

    if (e->op == EXP::plusPlus)
      {
	result = build2 (POSTINCREMENT_EXPR, build_ctype (e->type),
			 build_expr (e->e1), build_expr (e->e2));
      }
    else if (e->op == EXP::minusMinus)
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

  void visit (IndexExp *e) final override
  {
    Type *tb1 = e->e1->type->toBasetype ();

    if (tb1->ty == TY::Taarray)
      {
	/* Get the key for the associative array.  */
	Type *tkey = tb1->isTypeAArray ()->index->toBasetype ();
	tree key = convert_expr (build_expr (e->e2), e->e2->type, tkey);
	libcall_fn libcall;
	tree tinfo, ptr;

	if (e->modifiable)
	  {
	    libcall = LIBCALL_AAGETY;
	    ptr = build_address (build_expr (e->e1));
	    tinfo = build_typeinfo (e, dmd::mutableOf (dmd::unSharedOf (tb1)));
	  }
	else
	  {
	    libcall = LIBCALL_AAGETRVALUEX;
	    ptr = build_expr (e->e1);
	    tinfo = build_typeinfo (e, tkey);
	  }

	/* Index the associative array.  */
	tree result = build_libcall (libcall, dmd::pointerTo (e->type), 4,
				     ptr, tinfo,
				     size_int (dmd::size (tb1->nextOf ())),
				     build_address (key));

	if (!e->indexIsInBounds && array_bounds_check ())
	  {
	    tree tassert = build_array_bounds_call (e->loc);

	    result = d_save_expr (result);
	    result = build_condition (TREE_TYPE (result),
				      d_truthvalue_conversion (result),
				      result, tassert);
	  }

	this->result_ = indirect_ref (build_ctype (e->type), result);
      }
    else
      {
	/* Get the array and length for static and dynamic arrays.  */
	tree array = d_save_expr (build_expr (e->e1));

	tree length = NULL_TREE;
	if (tb1->ty != TY::Tpointer)
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
	if (tb1->ty != TY::Tpointer)
	  index = build_bounds_index_condition (e, index, length);

	/* Convert vectors to their underlying array type.  */
	if (VECTOR_TYPE_P (TREE_TYPE (array)))
	  {
	    tree array_type =
	      build_array_type_nelts (TREE_TYPE (TREE_TYPE (array)),
				      TYPE_VECTOR_SUBPARTS (TREE_TYPE (array)));
	    d_mark_addressable (array, false);
	    array = build1 (VIEW_CONVERT_EXPR, array_type, array);
	  }

	if (TREE_CODE (TREE_TYPE (array)) == ARRAY_TYPE)
	  {
	    /* Generate `array[index]'.  When the index is non-constant, we must
	       mark the array as addressable because we'll need to do pointer
	       arithmetic on its address.  */
	    if (TREE_CODE (index) != INTEGER_CST)
	      d_mark_addressable (array);

	    this->result_ = build4 (ARRAY_REF, TREE_TYPE (TREE_TYPE (array)),
				    array, index, NULL_TREE, NULL_TREE);
	  }
	else
	  {
	    /* Generate `array.ptr[index]'.  */
	    tree ptr = convert_expr (array, tb1,
				     dmd::pointerTo (tb1->nextOf ()));
	    ptr = void_okay_p (ptr);
	    this->result_ = indirect_ref (TREE_TYPE (TREE_TYPE (ptr)),
					  build_pointer_index (ptr, index));
	  }
      }
  }

  /* Build a comma expression.  The type is the type of the right operand.  */

  void visit (CommaExp *e) final override
  {
    tree t1 = build_expr (e->e1);
    tree t2 = build_expr (e->e2);
    tree type = e->type ? build_ctype (e->type) : void_type_node;

    this->result_ = build2 (COMPOUND_EXPR, type, t1, t2);
  }

  /* Build an array length expression.  Returns the number of elements
     in the array.  The result is of type size_t.  */

  void visit (ArrayLengthExp *e) final override
  {
    if (e->e1->type->toBasetype ()->ty == TY::Tarray)
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

  void visit (DelegatePtrExp *e) final override
  {
    tree t1 = build_expr (e->e1);
    this->result_ = delegate_object (t1);
  }

  /* Build a delegate function pointer expression.  This will return the
     function pointer value as a function type.  */

  void visit (DelegateFuncptrExp *e) final override
  {
    tree t1 = build_expr (e->e1);
    this->result_ = delegate_method (t1);
  }

  /* Build a slice expression.  */

  void visit (SliceExp *e) final override
  {
    Type *tb = e->type->toBasetype ();
    Type *tb1 = e->e1->type->toBasetype ();
    gcc_assert (tb->ty == TY::Tarray || tb->ty == TY::Tsarray);

    /* Use convert-to-dynamic-array code if possible.  */
    if (!e->lwr)
      {
	tree result = build_expr (e->e1);
	if (e->e1->type->toBasetype ()->ty == TY::Tsarray)
	  result = convert_expr (result, e->e1->type, e->type);

	this->result_ = result;
	return;
      }
    else
      gcc_assert (e->upr != NULL);

    /* Get the data pointer and length for static and dynamic arrays.  */
    tree array = d_save_expr (build_expr (e->e1));
    tree ptr = convert_expr (array, tb1, dmd::pointerTo (tb1->nextOf ()));
    tree length = NULL_TREE;

    /* Our array is already a SAVE_EXPR if necessary, so we don't make length
       a SAVE_EXPR which is, at most, a COMPONENT_REF on top of array.  */
    if (tb1->ty != TY::Tpointer)
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
	ptr = build_pointer_index (void_okay_p (ptr), lwr_tree);
	ptr = build_nop (ptrtype, ptr);
      }

    /* Nothing more to do for static arrays, their bounds checking has been
       done at compile-time.  */
    if (tb->ty == TY::Tsarray)
      {
	this->result_ = indirect_ref (build_ctype (e->type), ptr);
	return;
      }
    else
      gcc_assert (tb->ty == TY::Tarray);

    /* Generate bounds checking code.  */
    tree newlength = build_bounds_slice_condition (e, lwr_tree, upr_tree,
						   length);
    tree result = d_array_value (build_ctype (e->type), newlength, ptr);
    this->result_ = compound_expr (array, result);
  }

  /* Build a cast expression, which converts the given unary expression to the
     type of result.  */

  void visit (CastExp *e) final override
  {
    Type *ebtype = e->e1->type->toBasetype ();
    Type *tbtype = e->to->toBasetype ();
    tree result = build_expr (e->e1, this->constp_, this->literalp_);

    /* Just evaluate e1 if it has any side effects.  */
    if (tbtype->ty == TY::Tvoid)
      this->result_ = build_nop (build_ctype (tbtype), result);
    else
      this->result_ = convert_for_rvalue (result, ebtype, tbtype);
  }

  /* Build a delete expression.  */

  void visit (DeleteExp *e) final override
  {
    tree t1 = build_expr (e->e1);
    Type *tb1 = e->e1->type->toBasetype ();

    if (tb1->ty == TY::Tclass)
      {
	/* For class object references, if there is a destructor for that class,
	   the destructor is called for the object instance.  */
	gcc_assert (e->e1->op == EXP::variable);

	VarDeclaration *v = e->e1->isVarExp ()->var->isVarDeclaration ();
	gcc_assert (v && v->onstack ());

	libcall_fn libcall = tb1->isClassHandle ()->isInterfaceDeclaration ()
	  ? LIBCALL_CALLINTERFACEFINALIZER : LIBCALL_CALLFINALIZER;

	this->result_ = build_libcall (libcall, Type::tvoid, 1, t1);
	return;
      }
    else
      {
	error ("don%'t know how to delete %qs", e->e1->toChars ());
	this->result_ = error_mark_node;
      }
  }

  /* Build a remove expression, which removes a particular key from an
     associative array.  */

  void visit (RemoveExp *e) final override
  {
    /* Check that the array is actually an associative array.  */
    if (e->e1->type->toBasetype ()->ty == TY::Taarray)
      {
	Type *tb = e->e1->type->toBasetype ();
	Type *tkey = tb->isTypeAArray ()->index->toBasetype ();
	tree index = convert_expr (build_expr (e->e2), e->e2->type, tkey);

	this->result_ = build_libcall (LIBCALL_AADELX, Type::tbool, 3,
				       build_expr (e->e1),
				       build_typeinfo (e, tkey),
				       build_address (index));
      }
    else
      {
	error ("%qs is not an associative array", e->e1->toChars ());
	this->result_ = error_mark_node;
      }
  }

  /* Build an unary not expression.  */

  void visit (NotExp *e) final override
  {
    tree result = convert_for_condition (build_expr (e->e1), e->e1->type);
    /* Need to convert to boolean type or this will fail.  */
    result = fold_build1 (TRUTH_NOT_EXPR, d_bool_type, result);

    this->result_ = d_convert (build_ctype (e->type), result);
  }

  /* Build a compliment expression, where all the bits in the value are
     complemented.  Note: unlike in C, the usual integral promotions
     are not performed prior to the complement operation.  */

  void visit (ComExp *e) final override
  {
    TY ty1 = e->e1->type->toBasetype ()->ty;
    gcc_assert (ty1 != TY::Tarray && ty1 != TY::Tsarray);

    this->result_ = fold_build1 (BIT_NOT_EXPR, build_ctype (e->type),
				 build_expr (e->e1));
  }

  /* Build an unary negation expression.  */

  void visit (NegExp *e) final override
  {
    TY ty1 = e->e1->type->toBasetype ()->ty;
    gcc_assert (ty1 != TY::Tarray && ty1 != TY::Tsarray);

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

  void visit (PtrExp *e) final override
  {
    Type *tnext = NULL;
    dinteger_t offset;
    tree result;

    if (e->e1->op == EXP::add)
      {
	AddExp *ae = e->e1->isAddExp ();
	if (ae->e1->op == EXP::address
	    && ae->e2->isConst () && ae->e2->type->isIntegral ())
	  {
	    Expression *ex = ae->e1->isAddrExp ()->e1;
	    tnext = ex->type->toBasetype ();
	    result = build_expr (ex);
	    offset = ae->e2->toUInteger ();
	  }
      }
    else if (e->e1->op == EXP::symbolOffset)
      {
	SymOffExp *se = e->e1->isSymOffExp ();
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
    if (tnext && tnext->ty == TY::Tstruct)
      {
	StructDeclaration *sd = tnext->isTypeStruct ()->sym;

	for (size_t i = 0; i < sd->fields.length; i++)
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

  void visit (AddrExp *e) final override
  {
    tree type = build_ctype (e->type);
    tree exp;

    /* The frontend optimizer can convert const symbol into a struct literal.
       Taking the address of a struct literal is otherwise illegal.  */
    if (e->e1->op == EXP::structLiteral)
      {
	StructLiteralExp *sle = e->e1->isStructLiteralExp ()->origin;
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
      exp = build_expr (e->e1, this->constp_, this->literalp_);

    TREE_CONSTANT (exp) = 0;
    this->result_ = d_convert (type, build_address (exp));
  }

  /* Build a function call expression.  */

  void visit (CallExp *e) final override
  {
    Type *tb = e->e1->type->toBasetype ();
    Expression *e1b = e->e1;

    tree callee = NULL_TREE;
    tree object = NULL_TREE;
    tree cleanup = NULL_TREE;
    tree returnvalue = NULL_TREE;
    TypeFunction *tf = NULL;

    /* Calls to delegates can sometimes look like this.  */
    if (e1b->op == EXP::comma)
      {
	e1b = e1b->isCommaExp ()->e2;
	gcc_assert (e1b->op == EXP::variable);

	Declaration *var = e1b->isVarExp ()->var;
	gcc_assert (var->isFuncDeclaration () && !var->needThis ());
      }

    if (e1b->op == EXP::dotVariable && tb->ty != TY::Tdelegate)
      {
	DotVarExp *dve = e1b->isDotVarExp ();

	/* Don't modify the static initializer for struct literals.  */
	if (dve->e1->op == EXP::structLiteral)
	  {
	    StructLiteralExp *sle = dve->e1->isStructLiteralExp ();
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

		if (TREE_CODE (thisexp) == CONSTRUCTOR)
		  thisexp = force_target_expr (thisexp);

		/* Want reference to `this' object.  */
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

		/* C++ constructors return void, even though front-end semantic
		   treats them as implicitly returning `this'.  Set returnvalue
		   to override the result of this expression.  */
		if (fd->isCtorDeclaration ())
		  {
		    thisexp = d_save_expr (thisexp);
		    returnvalue = thisexp;
		  }

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
	if (e1b->op == EXP::dotVariable)
	  {
	    /* This gets the true function type, getting the function type
	       from e1->type can sometimes be incorrect, such as when calling
	       a `ref' return function.  */
	    tf = get_function_type (e1b->isDotVarExp ()->var->type);
	  }
	else
	  tf = get_function_type (tb);

	extract_from_method_call (callee, callee, object);
      }
    else if (tb->ty == TY::Tdelegate)
      {
	/* Delegate call, extract .object and .funcptr from var.  */
	callee = d_save_expr (callee);
	tf = get_function_type (tb);
	object = delegate_object (callee);
	callee = delegate_method (callee);
      }
    else if (e1b->op == EXP::variable)
      {
	FuncDeclaration *fd = e1b->isVarExp ()->var->isFuncDeclaration ();
	gcc_assert (fd != NULL);
	tf = get_function_type (fd->type);

	if (fd->isNested ())
	  {
	    /* Maybe re-evaluate symbol storage treating `fd' as public.  */
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

    /* Record whether the call expression has no side effects, so we can check
       for an unused return value later.  */
    if (TREE_CODE (exp) == CALL_EXPR && CALL_EXPR_FN (exp) != NULL_TREE
	&& call_side_effect_free_p (e->f, e->e1->type))
      CALL_EXPR_WARN_IF_UNUSED (exp) = 1;

    if (returnvalue != NULL_TREE)
      exp = compound_expr (exp, returnvalue);

    if (tf->isRef ())
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

	/* Keep the return value outside the TARGET_EXPR.  */
	if (returnvalue != NULL_TREE)
	  cleanup = compound_expr (cleanup, TREE_OPERAND (exp, 1));

	exp = cleanup;
      }

    this->result_ = exp;
  }

  /* Build a delegate expression.  */

  void visit (DelegateExp *e) final override
  {
    if (e->func->semanticRun == PASS::semantic3done)
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

    if (e->func->isNested () && !e->func->isThis ())
      {
	if (e->e1->op == EXP::null_)
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
	if (e->e1->type->ty != TY::Tclass && e->e1->type->ty != TY::Tpointer)
	  object = build_address (object);

	/* Object reference could be the outer `this' field of a class or
	   closure of type `void*'.  Cast it to the right type.  */
	if (e->e1->type->ty == TY::Tclass)
	  object = d_convert (build_ctype (e->e1->type), object);

	fndecl = get_symbol_decl (e->func);

	/* Get pointer to function out of the virtual table.  */
	if (e->func->isVirtual () && !e->func->isFinalFunc ()
	    && e->e1->op != EXP::super_ && e->e1->op != EXP::dotType)
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

  void visit (DotTypeExp *e) final override
  {
    /* Just a pass through to underlying expression.  */
    this->result_ = build_expr (e->e1);
  }

  /* Build a component reference expression.  */

  void visit (DotVarExp *e) final override
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
	    Type *tb = e->e1->type->toBasetype ();

	    if (tb->ty != TY::Tstruct)
	      object = build_deref (object);

	    /* __complex is represented as a struct in the front-end, but
	       underlying is really a complex type.  */
	    if (e->e1->type->ty == TY::Tenum
		&& e->e1->type->isTypeEnum ()->sym->isSpecial ())
	      object = underlying_complex_expr (build_ctype (tb), object);

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

  void visit (AssertExp *e) final override
  {
    Type *tb1 = e->e1->type->toBasetype ();
    tree arg = build_expr (e->e1);
    tree tmsg = NULL_TREE;
    tree assert_pass = void_node;
    tree assert_fail;

    if (global.params.useAssert == CHECKENABLEon && !checkaction_trap_p ())
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
	  libcall = unittest_p ? LIBCALL_UNITTESTP : LIBCALL_ASSERTP;

	/* Build a call to _d_assert().  */
	assert_fail = build_assert_call (e->loc, libcall, tmsg);

	if (global.params.useInvariants == CHECKENABLEon)
	  {
	    /* If the condition is a D class or struct object with an invariant,
	       call it if the condition result is true.  */
	    if (tb1->ty == TY::Tclass)
	      {
		ClassDeclaration *cd = tb1->isClassHandle ();
		if (!cd->isInterfaceDeclaration () && !cd->isCPPclass ())
		  {
		    arg = d_save_expr (arg);
		    assert_pass = build_libcall (LIBCALL_INVARIANT,
						 Type::tvoid, 1, arg);
		  }
	      }
	    else if (tb1->ty == TY::Tpointer
		     && tb1->nextOf ()->ty == TY::Tstruct)
	      {
		StructDeclaration *sd = tb1->nextOf ()->isTypeStruct ()->sym;
		if (sd->inv != NULL)
		  {
		    Expressions args;
		    arg = d_save_expr (arg);
		    assert_pass = d_build_call_expr (sd->inv, arg, &args);
		  }
	      }
	  }
      }
    else if (global.params.useAssert == CHECKENABLEon && checkaction_trap_p ())
      {
	/* Generate: __builtin_trap()  */
	tree fn = builtin_decl_explicit (BUILT_IN_TRAP);
	assert_fail = build_call_expr (fn, 0);
      }
    else
      {
	/* Assert contracts are turned off.  */
	this->result_ = void_node;
	return;
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

  void visit (DeclarationExp *e) final override
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

  void visit (TypeidExp *e) final override
  {
    if (Type *tid = dmd::isType (e->obj))
      {
	tree ti = build_typeinfo (e, tid);

	/* If the typeinfo is at an offset.  */
	if (tid->vtinfo->offset)
	  ti = build_offset (ti, size_int (tid->vtinfo->offset));

	this->result_ = build_nop (build_ctype (e->type), ti);
      }
    else if (Expression *tid = dmd::isExpression (e->obj))
      {
	Type *type = tid->type->toBasetype ();
	assert (type->ty == TY::Tclass);

	/* Generate **classptr to get the classinfo.  */
	tree ci = build_expr (tid);
	ci = indirect_ref (ptr_type_node, ci);
	ci = indirect_ref (ptr_type_node, ci);

	/* Add extra indirection for interfaces.  */
	if (type->isTypeClass ()->sym->isInterfaceDeclaration ())
	  ci = indirect_ref (ptr_type_node, ci);

	this->result_ = build_nop (build_ctype (e->type), ci);
      }
    else
      gcc_unreachable ();
  }

  /* Build a function/lambda expression.  */

  void visit (FuncExp *e) final override
  {
    /* Compile the declaration.  */
    build_lambda_tree (e->fd, e->type->toBasetype ());

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

  void visit (HaltExp *) final override
  {
    /* Should we use trap() or abort()?  */
    tree ttrap = builtin_decl_explicit (BUILT_IN_TRAP);
    this->result_ = build_call_expr (ttrap, 0);
  }

  /* Build a symbol pointer offset expression.  */

  void visit (SymOffExp *e) final override
  {
    /* Build the address and offset of the symbol.  */
    dinteger_t soffset = e->isSymOffExp ()->offset;
    tree result = get_decl_tree (e->var);
    TREE_USED (result) = 1;

    if (e->var->isFuncDeclaration ())
      result = maybe_reject_intrinsic (result);

    /* Emit lambdas, same as is done in FuncExp.  */
    if (FuncLiteralDeclaration *fld = e->var->isFuncLiteralDeclaration ())
      build_lambda_tree (fld);

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

  void visit (VarExp *e) final override
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

    /* Emit lambdas, same as is done in FuncExp.  */
    if (FuncLiteralDeclaration *fld = e->var->isFuncLiteralDeclaration ())
      build_lambda_tree (fld);

    if (this->constp_)
      {
	/* Want the initializer, not the expression.  */
	VarDeclaration *var = e->var->isVarDeclaration ();
	SymbolDeclaration *sdecl = e->var->isSymbolDeclaration ();
	tree init = NULL_TREE;

	if (var && (var->isConst () || var->isImmutable ())
	    && e->type->toBasetype ()->ty != TY::Tsarray && var->_init)
	  {
	    if (var->inuse)
	      error_at (make_location_t (e->loc), "recursive reference %qs",
			e->toChars ());
	    else
	      {
		var->inuse++;
		Expression *vinit = dmd::initializerToExpression (var->_init);
		init = build_expr (vinit, true);
		var->inuse--;
	      }
	  }
	else if (sdecl && sdecl->dsym)
	  {
	    if (StructDeclaration *sd = sdecl->dsym->isStructDeclaration ())
	      init = layout_struct_initializer (sd);
	    else if (ClassDeclaration *cd = sdecl->dsym->isClassDeclaration ())
	      init = layout_class_initializer (cd);
	    else
	      gcc_unreachable ();
	  }
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

	/* The variable expression generated for `__traits(initSymbol)'.  */
	if (SymbolDeclaration *sd = e->var->isSymbolDeclaration ())
	  {
	    if (e->type->isTypeDArray ())
	      {
		/* Generate a slice for non-zero initialized aggregates,
		   otherwise create an empty array.  */
		gcc_assert (e->type->isConst ()
			    && e->type->nextOf ()->ty == TY::Tvoid);

		tree type = build_ctype (e->type);
		tree length = size_int (sd->dsym->structsize);
		tree ptr = (sd->dsym->isStructDeclaration ()
			    && dmd::isZeroInit (sd->dsym->type, e->loc))
		  ? null_pointer_node : build_address (result);

		this->result_ = d_array_value (type, length, ptr);
		return;
	      }
	  }

	/* For variables that are references - currently only out/inout
	   arguments; objects don't count - evaluating the variable means
	   we want what it refers to.  */
	if (declaration_reference_p (e->var))
	  result = indirect_ref (build_ctype (e->var->type), result);

	this->result_ = result;
      }
  }

  /* Build a this variable expression.  */

  void visit (ThisExp *e) final override
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

    if (e->type->ty == TY::Tstruct)
      result = build_deref (result);

    this->result_ = result;
  }

  /* Build a new expression, which allocates memory either on the garbage
     collected heap or by using a class or struct specific allocator.  */

  void visit (NewExp *e) final override
  {
    Type *tb = e->type->toBasetype ();
    tree result;

    if (tb->ty == TY::Tclass)
      {
	/* Allocating a new class.  */
	tb = e->newtype->toBasetype ();

	ClassDeclaration *cd = tb->isTypeClass ()->sym;
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
	else
	  {
	    /* Generate: _d_newclass()
		     or: _d_newThrowable()  */
	    new_call = build_expr (e->lowering);
	  }

	/* Set the context pointer for nested classes.  */
	if (cd->isNested ())
	  {
	    tree field = get_symbol_decl (cd->vthis);
	    tree value = NULL_TREE;

	    if (e->thisexp)
	      {
		ClassDeclaration *tcd = e->thisexp->type->isClassHandle ();
		/* The class or function we're nested in.  */
		Dsymbol *outer = cd->toParentLocal ();

		value = build_expr (e->thisexp);

		if (outer != tcd)
		  {
		    ClassDeclaration *ocd = outer->isClassDeclaration ();
		    int offset = 0;
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
    else if (tb->ty == TY::Tpointer
	     && tb->nextOf ()->toBasetype ()->ty == TY::Tstruct)
      {
	/* Allocating memory for a new struct.  */
	Type *htype = e->newtype->toBasetype ();
	gcc_assert (!e->onstack);

	TypeStruct *stype = htype->isTypeStruct ();
	StructDeclaration *sd = stype->sym;
	tree new_call;

	/* Cannot new an opaque struct.  */
	if (sd->size (e->loc) == 0)
	  {
	    this->result_ = d_convert (build_ctype (e->type),
				       integer_zero_node);
	    return;
	  }

	/* This case should have been rewritten to `_d_newitemT' during the
	   semantic phase.  */
	gcc_assert (e->lowering);

	/* Generate: _d_newitemT()  */
	new_call = build_expr (e->lowering);

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
	    if (e->arguments != NULL && sd->fields.length != 0)
	      {
		StructLiteralExp *se = StructLiteralExp::create (e->loc, sd,
								 e->arguments,
								 htype);
		new_call = d_save_expr (new_call);
		se->type = sd->type;
		se->sym = new_call;

		/* Setting `se->sym' would mean that the result of the
		   constructed struct literal expression is `*(new_call)'.
		   Strip off the indirect reference, as we don't mean to
		   compute the value yet.  */
		result = build_address (build_expr (se));
	      }
	    else
	      result = new_call;
	  }

	if (e->argprefix)
	  result = compound_expr (build_expr (e->argprefix), result);
      }
    else if (tb->ty == TY::Tarray)
      {
	/* Allocating memory for a new D array.  */
	gcc_assert (e->arguments && e->arguments->length >= 1);

	/* Array allocations have already been handled by the front-end.  */
	gcc_assert (e->lowering != NULL);
	result = build_expr (e->lowering);

	if (e->argprefix)
	  result = compound_expr (build_expr (e->argprefix), result);
      }
    else if (tb->ty == TY::Tpointer)
      {
	/* Allocating memory for a new pointer.  */
	TypePointer *tpointer = tb->isTypePointer ();

	if (dmd::size (tpointer->next) == 0)
	  {
	    /* Pointer element size is unknown.  */
	    this->result_ = d_convert (build_ctype (e->type),
				       integer_zero_node);
	    return;
	  }

	/* This case should have been rewritten to `_d_newitemT' during the
	   semantic phase.  */
	gcc_assert (e->lowering);

	/* Generate: _d_newitemT()  */
	result = build_expr (e->lowering);

	if (e->arguments && e->arguments->length == 1)
	  {
	    result = d_save_expr (result);
	    tree init = modify_expr (build_deref (result),
				     build_expr ((*e->arguments)[0]));
	    result = compound_expr (init, result);
	  }

	if (e->argprefix)
	  result = compound_expr (build_expr (e->argprefix), result);
      }
    else if (tb->ty == TY::Taarray)
      {
	/* Allocating memory for a new associative array.  */
	tree arg = build_typeinfo (e, e->newtype);
	tree mem = build_libcall (LIBCALL_AANEW, Type::tvoidptr, 1, arg);

	/* Return an associative array pointed to by MEM.  */
	tree aatype = build_ctype (tb);
	vec <constructor_elt, va_gc> *ce = NULL;
	CONSTRUCTOR_APPEND_ELT (ce, TYPE_FIELDS (aatype), mem);

	result = build_nop (build_ctype (e->type),
			    build_constructor (aatype, ce));
      }
    else
      gcc_unreachable ();

    this->result_ = convert_expr (result, tb, e->type);
  }

  /* Build an integer literal.  */

  void visit (IntegerExp *e) final override
  {
    tree ctype = build_ctype (e->type->toBasetype ());
    this->result_ = build_integer_cst (e->value, ctype);
  }

  /* Build a floating-point literal.  */

  void visit (RealExp *e) final override
  {
    this->result_ = build_float_cst (e->value, e->type->toBasetype ());
  }

  /* Build a complex literal.  */

  void visit (ComplexExp *e) final override
  {
    Type *tnext;

    switch (e->type->toBasetype ()->ty)
      {
      case TY::Tcomplex32:
	tnext = (TypeBasic *) Type::tfloat32;
	break;

      case TY::Tcomplex64:
	tnext = (TypeBasic *) Type::tfloat64;
	break;

      case TY::Tcomplex80:
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

  void visit (StringExp *e) final override
  {
    Type *tb = e->type->toBasetype ();
    tree type = build_ctype (e->type);

    if (tb->ty == TY::Tsarray)
      {
	/* Turn the string into a constructor for the static array.  */
	vec <constructor_elt, va_gc> *elms = NULL;
	vec_safe_reserve (elms, e->len);
	tree etype = TREE_TYPE (type);

	for (size_t i = 0; i < e->len; i++)
	  {
	    tree value = build_integer_cst (e->getIndex (i), etype);
	    CONSTRUCTOR_APPEND_ELT (elms, size_int (i), value);
	  }

	tree ctor = build_constructor (type, elms);
	TREE_CONSTANT (ctor) = 1;
	this->result_ = ctor;
	return;
      }
    else
      {
	/* Copy the string contents to a null terminated STRING_CST.  */
	dinteger_t length = (e->len * e->sz);
	char *string = XALLOCAVEC (char, length + e->sz);
	memset (string, 0, length + e->sz);
	if (length > 0)
	  memcpy (string, e->string, length);

	/* String value and type includes the null terminator.  */
	tree value = build_string (length + e->sz, string);
	if (e->sz <= 4)
	  TREE_TYPE (value) = make_array_type (tb->nextOf (), length + 1);
	else
	  {
	    /* Hexadecimal literal strings with an 8-byte character type are
	       just an alternative way to store an array of `ulong'.
	       Treat it as if it were a `uint[]' array instead.  */
	    dinteger_t resize = e->sz / 4;
	    TREE_TYPE (value) = make_array_type (Type::tuns32,
						 (length * resize) + resize);
	  }

	value = build_address (value);

	if (tb->ty == TY::Tarray)
	  value = d_array_value (type, size_int (e->len), value);

	TREE_CONSTANT (value) = 1;
	this->result_ = d_convert (type, value);
      }
  }

  /* Build a tuple literal.  Just an argument list that may have
     side effects that need evaluation.  */

  void visit (TupleExp *e) final override
  {
    tree result = NULL_TREE;

    if (e->e0)
      result = build_expr (e->e0, this->constp_, true);

    for (size_t i = 0; i < e->exps->length; ++i)
      {
	Expression *exp = (*e->exps)[i];
	result = compound_expr (result, build_expr (exp, this->constp_, true));
      }

    if (result == NULL_TREE)
      result = void_node;

    this->result_ = result;
  }

  /* Build an array literal.  The common type of the all elements is taken to
     be the type of the array element, and all elements are implicitly
     converted to that type.  */

  void visit (ArrayLiteralExp *e) final override
  {
    Type *tb = e->type->toBasetype ();

    /* Implicitly convert void[n] to ubyte[n].  */
    if (tb->ty == TY::Tsarray && tb->nextOf ()->toBasetype ()->ty == TY::Tvoid)
      tb = dmd::sarrayOf (Type::tuns8, tb->isTypeSArray ()->dim->toUInteger ());

    gcc_assert (tb->ty == TY::Tarray || tb->ty == TY::Tsarray
		|| tb->ty == TY::Tpointer);

    /* Handle empty array literals.  */
    if (e->elements->length == 0)
      {
	if (tb->ty == TY::Tarray)
	  this->result_ = d_array_value (build_ctype (e->type),
					 size_int (0), null_pointer_node);
	else
	  this->result_ = build_constructor (make_array_type (tb->nextOf (), 0),
					     NULL);

	return;
      }

    /* Build an expression that assigns the expressions in ELEMENTS to
       a constructor.  */
    vec <constructor_elt, va_gc> *elms = NULL;
    vec_safe_reserve (elms, e->elements->length);
    bool constant_p = true;
    tree saved_elems = NULL_TREE;

    Type *etype = tb->nextOf ();
    tree satype = make_array_type (etype, e->elements->length);

    for (size_t i = 0; i < e->elements->length; i++)
      {
	Expression *expr = e->getElement (i);
	tree value = build_expr (expr, this->constp_, true);

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
    if (tb->ty == TY::Tsarray || this->constp_)
      {
	/* Can't take the address of the constructor, so create an anonymous
	   static symbol, and then refer to it.  */
	if (tb->ty != TY::Tsarray)
	  {
	    tree decl = build_artificial_decl (TREE_TYPE (ctor), ctor, "A");
	    ctor = build_address (decl);
	    if (tb->ty == TY::Tarray)
	      ctor = d_array_value (type, size_int (e->elements->length), ctor);

	    /* Immutable literals can be placed in rodata.  */
	    if (tb->isImmutable ())
	      TREE_READONLY (decl) = 1;

	    d_pushdecl (decl);
	    rest_of_decl_compilation (decl, 1, 0);
	  }

	/* If the array literal is readonly or static.  */
	if (constant_p)
	  TREE_CONSTANT (ctor) = 1;
	if (constant_p && initializer_constant_valid_p (ctor, TREE_TYPE (ctor)))
	  TREE_STATIC (ctor) = 1;

	/* Use memset to fill any alignment holes in the array.  */
	if (!this->constp_ && !this->literalp_)
	  {
	    TypeStruct *ts = etype->baseElemOf ()->isTypeStruct ();

	    if (ts != NULL && (!identity_compare_p (ts->sym)
			       || ts->sym->isUnionDeclaration ()))
	      {
		tree var = build_local_temp (TREE_TYPE (ctor));
		tree init = build_memset_call (var);
		/* Evaluate memset() first, then any saved elements.  */
		saved_elems = compound_expr (init, saved_elems);
		ctor = compound_expr (modify_expr (var, ctor), var);
	      }
	  }

	this->result_ = compound_expr (saved_elems, d_convert (type, ctor));
      }
    else if (e->onstack)
      {
	/* Array literal for a `scope' dynamic array.  */
	gcc_assert (tb->ty == TY::Tarray);
	ctor = force_target_expr (ctor);
	this->result_ = d_array_value (type, size_int (e->elements->length),
				       build_address (ctor));
      }
    else
      {
	/* Allocate space on the memory managed heap.  */
	tree mem = build_libcall (LIBCALL_ARRAYLITERALTX,
				  dmd::pointerTo (etype), 2,
				  build_typeinfo (e, dmd::arrayOf (etype)),
				  size_int (e->elements->length));
	mem = d_save_expr (mem);

	/* Now copy the constructor into memory.  */
	tree size = size_mult_expr (size_int (e->elements->length),
				    size_int (dmd::size (tb->nextOf ())));

	tree result = build_memcpy_call (mem, build_address (ctor), size);

	/* Return the array pointed to by MEM.  */
	result = compound_expr (result, mem);

	if (tb->ty == TY::Tarray)
	  result = d_array_value (type, size_int (e->elements->length), result);

	this->result_ = compound_expr (saved_elems, result);
      }
  }

  /* Build an associative array literal.  The common type of the all keys is
     taken to be the key type, and common type of all values the value type.
     All keys and values are then implicitly converted as needed.  */

  void visit (AssocArrayLiteralExp *e) final override
  {
    if (this->constp_ && e->lowering != NULL)
      {
	/* When an associative array literal gets lowered, it's converted into a
	   struct literal suitable for static initialization.  */
	this->result_ = build_expr (e->lowering, this->constp_, true);
	return ;
      }

    /* Want the mutable type for typeinfo reference.  */
    Type *tb = dmd::mutableOf (e->type->toBasetype ());

    /* Handle empty assoc array literals.  */
    TypeAArray *ta = tb->isTypeAArray ();
    if (e->keys->length == 0)
      {
	this->result_ = build_constructor (build_ctype (ta), NULL);
	return;
      }

    /* Build an expression that assigns all expressions in KEYS
       to a constructor.  */
    Type *tkarray = dmd::sarrayOf (ta->index, e->keys->length);
    tree akeys = build_array_from_exprs (tkarray, e->keys, this->constp_);
    tree init = stabilize_expr (&akeys);

    /* Do the same with all expressions in VALUES.  */
    Type *tvarray = dmd::sarrayOf (ta->next, e->values->length);
    tree avals = build_array_from_exprs (tvarray, e->values, this->constp_);
    init = compound_expr (init, stabilize_expr (&avals));

    /* Generate: _d_assocarrayliteralTX (ti, keys, vals);  */
    tree keys = d_array_value (build_ctype (dmd::arrayOf (ta->index)),
			       size_int (e->keys->length),
			       build_address (akeys));
    tree vals = d_array_value (build_ctype (dmd::arrayOf (ta->next)),
			       size_int (e->values->length),
			       build_address (avals));

    tree mem = build_libcall (LIBCALL_ASSOCARRAYLITERALTX, Type::tvoidptr, 3,
			      build_typeinfo (e, ta), keys, vals);

    /* Return an associative array pointed to by MEM.  */
    tree aatype = build_ctype (ta);
    vec <constructor_elt, va_gc> *ce = NULL;
    CONSTRUCTOR_APPEND_ELT (ce, TYPE_FIELDS (aatype), mem);

    tree result = build_nop (build_ctype (e->type),
			     build_constructor (aatype, ce));
    this->result_ = compound_expr (init, result);
  }

  /* Build a struct literal.  */

  void visit (StructLiteralExp *e) final override
  {
    /* Handle empty struct literals.  */
    if (e->elements == NULL || e->sd->fields.length == 0)
      {
	this->result_ = build_constructor (build_ctype (e->type), NULL);
	return;
      }

    /* Building sinit trees are delayed until after frontend semantic
       processing has complete.  Build the static initializer now.  */
    if (e->useStaticInit && !this->constp_ && !e->sd->isCsymbol ())
      {
	tree init = aggregate_initializer_decl (e->sd);

	/* If initializing a symbol, don't forget to set it.  */
	if (e->sym != NULL)
	  {
	    tree var = build_deref (e->sym);
	    init = compound_expr (modify_expr (var, init), var);
	  }

	this->result_ = init;
	return;
      }

    /* Build a constructor that assigns the expressions in ELEMENTS
       at each field index that has been filled in.  */
    vec <constructor_elt, va_gc> *ve = NULL;
    tree saved_elems = NULL_TREE;

    /* CTFE may fill the hidden pointer by NullExp.  */
    gcc_assert (e->elements->length <= e->sd->fields.length);

    Type *tb = e->type->toBasetype ();
    gcc_assert (tb->ty == TY::Tstruct);

    for (size_t i = 0; i < e->elements->length; i++)
      {
	Expression *exp = (*e->elements)[i];
	if (!exp)
	  continue;

	VarDeclaration *field = e->sd->fields[i];
	Type *type = exp->type->toBasetype ();
	Type *ftype = field->type->toBasetype ();
	tree value = NULL_TREE;

	if (ftype->ty == TY::Tsarray && !same_type_p (type, ftype))
	  {
	    /* Initialize a static array with a single element.  */
	    tree elem = build_expr (exp, this->constp_, true);
	    saved_elems = compound_expr (saved_elems, stabilize_expr (&elem));
	    elem = d_save_expr (elem);

	    if (initializer_zerop (elem))
	      value = build_constructor (build_ctype (ftype), NULL);
	    else
	      value = build_array_from_val (ftype, elem);
	  }
	else
	  {
	    value = convert_expr (build_expr (exp, this->constp_, true),
				  exp->type, field->type);
	  }

	/* Split construction of values out of the constructor.  */
	saved_elems = compound_expr (saved_elems, stabilize_expr (&value));

	CONSTRUCTOR_APPEND_ELT (ve, get_symbol_decl (field), value);
      }

    /* Maybe setup hidden pointer to outer scope context.  */
    if (e->sd->isNested () && e->elements->length != e->sd->fields.length
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

    /* Construct the struct literal for run-time.  */
    if (e->sym != NULL)
      {
	/* Store the result in a symbol to initialize the literal.  */
	tree var = build_deref (e->sym);
	ctor = compound_expr (modify_expr (var, ctor), var);
      }
    else if (!this->literalp_)
      {
	/* Use memset to fill any alignment holes in the object.  */
	if (!identity_compare_p (e->sd) || e->sd->isUnionDeclaration ())
	  {
	    tree var = build_local_temp (TREE_TYPE (ctor));
	    tree init = build_memset_call (var);
	    /* Evaluate memset() first, then any saved element constructors.  */
	    saved_elems = compound_expr (init, saved_elems);
	    ctor = compound_expr (modify_expr (var, ctor), var);
	  }
      }

    this->result_ = compound_expr (saved_elems, ctor);
  }

  /* Build a null literal.  */

  void visit (NullExp *e) final override
  {
    this->result_ = build_typeof_null_value (e->type);
  }

  /* Build a vector literal.  */

  void visit (VectorExp *e) final override
  {
    /* First handle array literal expressions.  */
    if (e->e1->op == EXP::arrayLiteral)
      {
	ArrayLiteralExp *ale = e->e1->isArrayLiteralExp ();
	vec <constructor_elt, va_gc> *elms = NULL;
	bool constant_p = true;
	tree type = build_ctype (e->type);

	vec_safe_reserve (elms, ale->elements->length);
	for (size_t i = 0; i < ale->elements->length; i++)
	  {
	    Expression *expr = ale->getElement (i);
	    tree value = d_convert (TREE_TYPE (type),
				    build_expr (expr, this->constp_, true));
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
    else if (e->e1->type->toBasetype ()->ty == TY::Tsarray)
      {
	/* Build a vector representation from a static array.  */
	this->result_ = convert_expr (build_expr (e->e1, this->constp_),
				      e->e1->type, e->type);
      }
    else
      {
	/* Build constructor from single value.  */
	tree type = build_ctype (e->type);
	tree value = d_convert (TREE_TYPE (type),
				build_expr (e->e1, this->constp_, true));
	this->result_ = build_vector_from_val (type, value);
      }
  }

  /* Build a static array representation of a vector expression.  */

  void visit (VectorArrayExp *e) final override
  {
    this->result_ = convert_expr (build_expr (e->e1, this->constp_, true),
				  e->e1->type, e->type);
  }

  /* Build a static class literal, return its reference.  */

  void visit (ClassReferenceExp *e) final override
  {
    /* The result of build_new_class_expr is a RECORD_TYPE, we want
       the reference.  */
    tree var = build_address (build_new_class_expr (e));

    /* If the type of this literal is an interface, the we must add the
       interface offset to symbol.  */
    if (this->constp_)
      {
	TypeClass *tc = e->type->toBasetype ()->isTypeClass ();
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

  /* Build an uninitialized value, generated from void initializers.  */

  void visit (VoidInitExp *e) final override
  {
    /* The front-end only generates these for the initializer of globals.
       Represent `void' as zeroes, regardless of the type's default value.  */
    gcc_assert (this->constp_);
    this->result_ = build_zero_cst (build_ctype (e->type));
  }

  /* These expressions are mainly just a placeholders in the frontend.
     We shouldn't see them here.  */

  void visit (ScopeExp *e) final override
  {
    error_at (make_location_t (e->loc), "%qs is not an expression",
	      e->toChars ());
    this->result_ = error_mark_node;
  }

  void visit (TypeExp *e) final override
  {
    error_at (make_location_t (e->loc), "type %qs is not an expression",
	      e->toChars ());
    this->result_ = error_mark_node;
  }
};


/* Main entry point for ExprVisitor interface to generate code for
   the Expression AST class E.  If CONST_P is true, then E is a
   constant expression.  If LITERAL_P is true, then E is a value used
   in the initialization of another literal.  */

tree
build_expr (Expression *e, bool const_p, bool literal_p)
{
  ExprVisitor v = ExprVisitor (const_p, literal_p);
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
  if (tf->isRef ())
    {
      /* If we are returning a reference, take the address.  */
      result = convert_expr (result, e->type, type);
      result = build_address (result);
    }
  else
    result = convert_for_rvalue (result, e->type, type);

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

